# Headless shots of the live spacecraft app (or a local instance).
# Usage: Rscript scripts/live_shots.R [url] [out_dir] [log_dir] [dl_dir]
# Writes <out_dir>/live_rc_*.png. Never prints secrets.
suppressPackageStartupMessages({ library(chromote); library(jsonlite) })
args <- commandArgs(TRUE)
root <- normalizePath(file.path(if (file.exists("scripts/live_shots.R")) "." else ".."))
url <- if (length(args) >= 1L && nzchar(args[[1]])) args[[1]] else
  "https://2o6mmo-eddie-benjamin.shinyapps.io/spacecraft-line-simulation/"
out_dir <- if (length(args) >= 2L && nzchar(args[[2]])) args[[2]] else file.path(root, "shots")
log_dir <- if (length(args) >= 3L && nzchar(args[[3]])) args[[3]] else file.path(root, "shots")
dl_dir <- if (length(args) >= 4L && nzchar(args[[4]])) args[[4]] else file.path(root, "shots", "downloads")
dir.create(out_dir, FALSE, TRUE)
dir.create(dl_dir, FALSE, TRUE)
dir.create(log_dir, FALSE, TRUE)
unlink(list.files(dl_dir, full.names = TRUE))
results_path <- file.path(log_dir, "live_rc_tab_check.txt")
dl_log_path <- file.path(log_dir, "live_rc_downloads.txt")
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

b <- ChromoteSession$new()
b$default_timeout <- 300
b$Page$enable(); b$Runtime$enable(); b$Log$enable(); b$Network$enable()
b$Emulation$setDeviceMetricsOverride(width = 1440L, height = 1600L,
                                     deviceScaleFactor = 1, mobile = FALSE)
cdp_downloads <- FALSE
try({
  b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(dl_dir),
                                eventsEnabled = TRUE)
  cdp_downloads <- TRUE
}, silent = TRUE)

ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value,
                              error = function(e) NULL)
wait_for <- function(js, secs = 180) {
  for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }
  FALSE
}
shot <- function(name, full = FALSE) {
  path <- file.path(out_dir, paste0("live_rc_", name, ".png"))
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png", captureBeyondViewport = full)$data), path)
  cat("shot", path, "\n")
  path
}
click_nav <- function(value) {
  ev(sprintf("(() => {
    const a = document.querySelector(\"a.nav-link[data-value='%s'], a[data-value='%s']\");
    if (!a) return false;
    const dd = a.closest('.dropdown-menu');
    if (dd) {
      const tog = dd.parentElement && dd.parentElement.querySelector('.dropdown-toggle, [data-bs-toggle=\"dropdown\"]');
      if (tog) tog.click();
    }
    a.click();
    return true;
  })()", value, value))
}
click_tab <- function(container, value) {
  ev(sprintf("(() => { const a = document.querySelector(\"#%s a[data-value='%s']\"); if (!a) return false; a.click(); return true; })()", container, value))
}
error_report <- function() {
  list(
    shiny_err = ev("(() => { const n = document.querySelectorAll('.shiny-output-error, .shiny-output-error-validation'); return Array.from(n).filter(e => e.offsetParent !== null).map(e => ((e.id||'') + ': ' + (e.innerText||'')).slice(0,300)).filter(t => t && t !== ': '); })()"),
    disconnected = ev("!!(document.querySelector('.shiny-disconnected, #shiny-disconnected-overlay') || (document.body && /disconnected from the server/i.test(document.body.innerText)))"),
    visible_error = ev("(() => { const t = document.body ? document.body.innerText : ''; const m = t.match(/Error in [^\n]{0,200}|Error: [^\n]{0,200}/); return m ? m[0] : ''; })()"),
    title = ev("document.title || ''")
  )
}
lines <- character()
note <- function(...) {
  line <- paste(..., collapse = "")
  lines <<- c(lines, line)
  cat(line, "\n", sep = "")
}
record_status <- function(label) {
  r <- error_report()
  visible_errs <- unlist(r$shiny_err)
  if (is.null(visible_errs)) visible_errs <- character()
  status <- if (isTRUE(r$disconnected)) "DISCONNECTED" else if (length(visible_errs) || nzchar(r$visible_error %||% "")) "ERROR" else "OK"
  note(sprintf("check=%s status=%s", label, status))
  if (length(visible_errs)) note("  visible_shiny_err: ", paste(visible_errs, collapse = " | "))
  if (nzchar(r$visible_error %||% "")) note("  visible_error: ", r$visible_error)
  status
}

cookie_header <- function() {
  cookies <- tryCatch(b$Network$getAllCookies()$cookies, error = function(e) NULL)
  if (is.null(cookies) || !length(cookies)) return("")
  parts <- vapply(cookies, function(ck) {
    nm <- ck$name %||% ck[["name"]]
    vl <- ck$value %||% ck[["value"]]
    if (is.null(nm) || is.null(vl)) return(NA_character_)
    paste0(nm, "=", vl)
  }, character(1))
  parts <- parts[!is.na(parts)]
  paste(parts, collapse = "; ")
}
download_via_href <- function(button_id, dest) {
  href <- ev(sprintf("(document.getElementById('%s')||{}).href || ''", button_id))
  if (!nzchar(href %||% "")) return(list(ok = FALSE, reason = "no href", bytes = 0, href = ""))
  if (startsWith(href, "/")) href <- paste0("https://2o6mmo-eddie-benjamin.shinyapps.io", href)
  hdr <- cookie_header()
  args <- c("-sS", "-L", "--max-time", "90",
            "-A", "Mozilla/5.0 pr17-deploy-download-check",
            "-o", dest, "-w", "%{http_code}",
            "-H", paste0("Referer: ", url))
  if (nzchar(hdr)) args <- c(args, "-H", paste0("Cookie: ", hdr))
  args <- c(args, href)
  code <- tryCatch(system2("curl", args, stdout = TRUE, stderr = FALSE),
                   error = function(e) e$message)
  bytes <- if (file.exists(dest)) file.info(dest)$size else 0
  list(ok = identical(as.character(code)[1], "200") && isTRUE(bytes > 0),
       reason = paste0("http=", as.character(code)[1], " bytes=", bytes),
       bytes = bytes)
}
wait_for_file <- function(pattern, secs = 20, start_files = character(), since = Sys.time() - 0.2) {
  for (i in seq_len(secs * 4)) {
    files <- list.files(dl_dir, full.names = TRUE)
    files <- files[!grepl("\\.crdownload$|\\.tmp$", files)]
    info <- file.info(files)
    newf <- setdiff(files, start_files)
    overwritten <- character()
    if (!is.null(info) && nrow(info)) {
      overwritten <- rownames(info)[!is.na(info$mtime) & info$mtime >= since & info$size > 0]
    }
    hit <- unique(c(newf, overwritten))
    hit <- hit[grepl(pattern, basename(hit), ignore.case = TRUE)]
    if (length(hit) && isTRUE(file.info(hit[[1]])$size > 0)) return(hit[[1]])
    Sys.sleep(0.25)
  }
  NULL
}
peek_file <- function(path) {
  if (is.null(path) || !file.exists(path)) return("missing")
  info <- file.info(path)
  ext <- tools::file_ext(path)
  if (identical(tolower(ext), "zip")) return(sprintf("zip bytes=%d", info$size))
  con <- file(path, "r")
  on.exit(close(con), add = TRUE)
  hdr <- tryCatch(readLines(con, n = 1, warn = FALSE), error = function(e) "")
  sprintf("bytes=%d header=%s", info$size, substr(paste(hdr, collapse = ""), 1, 120))
}
click_and_collect <- function(button_id, expect_pattern, dest_name) {
  start_files <- list.files(dl_dir, full.names = TRUE)
  dest <- file.path(dl_dir, dest_name)
  clicked <- ev(sprintf("(() => { const a = document.getElementById('%s'); if (!a) return false; a.click(); return true; })()", button_id))
  if (!isTRUE(clicked)) return(list(ok = FALSE, reason = "button not found", peek = "missing"))
  found <- wait_for_file(expect_pattern, secs = 15, start_files = start_files, since = Sys.time() - 0.2)
  if (!is.null(found)) {
    if (!identical(normalizePath(found), normalizePath(dest, mustWork = FALSE))) file.copy(found, dest, overwrite = TRUE)
    return(list(ok = isTRUE(file.info(dest)$size > 0), reason = "cdp_or_browser", peek = peek_file(dest)))
  }
  res <- download_via_href(button_id, dest)
  list(ok = isTRUE(res$ok), reason = res$reason, peek = peek_file(dest))
}

note(sprintf("url=%s", url))
note(sprintf("cdp_downloads=%s", cdp_downloads))
invisible(b$Page$navigate(url))
ready <- wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 240)
note(sprintf("simulate_ready=%s", ready))
if (!isTRUE(ready)) {
  record_status("simulate_not_ready")
  writeLines(lines, results_path)
  try(b$close(), silent = TRUE)
  stop("Live Simulate did not become ready (kpi_cycle never showed 'h').")
}
Sys.sleep(1.5)

kpis <- ev("['kpi_throughput','kpi_cycle','kpi_hpu','kpi_wip'].map(i => (document.getElementById(i)||{}).innerText).join(' | ')")
note("kpis: ", if (is.null(kpis)) "" else as.character(kpis))
nav <- ev("Array.from(document.querySelectorAll('.navbar a[data-value], a.nav-link[data-value]')).map(a => a.getAttribute('data-value'))")
note("nav_values: ", paste(unique(unlist(nav)), collapse = ", "))

ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("01_simulate_kpis")
record_status("simulate_kpis")
ev("document.querySelector('.sc-flow-wrap, #sc-flow-svg, .sc-flow-svg')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
shot("02_simulate_flow")
record_status("simulate_flow")

# Settings
ok <- isTRUE(click_nav("Settings"))
note(sprintf("nav Settings ok=%s", ok))
Sys.sleep(1.4)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("03_settings_variants")
record_status("settings_variants")
ev("(() => { const a = document.querySelector(\"a[data-value='Line settings']\"); if (a) a.click(); return !!a; })()")
Sys.sleep(1.0)
shot("04_settings_line", TRUE)
record_status("settings_line")

# Results dropdown pages
results_pages <- list(
  list("results_capacity", "05_results_capacity"),
  list("results_cycle", "09_results_cycle"),
  list("results_variant", "09v_results_variant"),
  list("results_quality", "12q_results_quality"),
  list("results_reliability", "13r_results_reliability"),
  list("results_summary", "16_results_summary")
)
for (pg in results_pages) {
  ok <- isTRUE(click_nav(pg[[1]]))
  note(sprintf("nav %s ok=%s", pg[[1]], ok))
  Sys.sleep(1.8)
  ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
  shot(pg[[2]], TRUE)
  record_status(pg[[1]])
}

# Inner Results tabs
stopifnot(isTRUE(click_nav("results_capacity")))
Sys.sleep(1.0)
for (spec in list(
  list("results_capacity_tabs", "Utilization", "05b_results_util"),
  list("results_capacity_tabs", "Downtime", "05c_results_down"),
  list("results_capacity_tabs", "Queue length", "07_results_queues"),
  list("results_capacity_tabs", "Why stations wait", "06_results_waits")
)) {
  ok <- isTRUE(click_tab(spec[[1]], spec[[2]]))
  note(sprintf("tab %s/%s ok=%s", spec[[1]], spec[[2]], ok))
  Sys.sleep(1.3)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
  record_status(paste0("capacity_", spec[[2]]))
}

stopifnot(isTRUE(click_nav("results_cycle")))
Sys.sleep(1.0)
for (spec in list(
  list("results_cycle_tabs", "All shipped units", "09b_results_cycle_hist"),
  list("results_cycle_tabs", "By variant", "09c_results_cycle_dist"),
  list("results_cycle_tabs", "Per-step breakdown", "10_results_cycle_steps"),
  list("results_cycle_tabs", "Over time", "11_results_cycle_time")
)) {
  ok <- isTRUE(click_tab(spec[[1]], spec[[2]]))
  note(sprintf("tab %s/%s ok=%s", spec[[1]], spec[[2]], ok))
  Sys.sleep(1.3)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
  record_status(paste0("cycle_", spec[[2]]))
}

# Results > By variant (cycle time and touch labor overlapping histograms)
stopifnot(isTRUE(click_nav("results_variant")))
Sys.sleep(1.0)
for (spec in list(
  list("results_variant_tabs", "Cycle time", "09v_results_variant_cycle"),
  list("results_variant_tabs", "Touch labor", "09w_results_variant_hpu")
)) {
  ok <- isTRUE(click_tab(spec[[1]], spec[[2]]))
  note(sprintf("tab %s/%s ok=%s", spec[[1]], spec[[2]], ok))
  Sys.sleep(1.3)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
  record_status(paste0("variant_", spec[[2]]))
}

stopifnot(isTRUE(click_nav("results_quality")))
Sys.sleep(1.0)
for (spec in list(
  list("results_quality_tabs", "Rework", "12_results_quality_rework"),
  list("results_quality_tabs", "Scrap", "12b_results_quality_scrap"),
  list("results_quality_tabs", "Fails", "12c_results_quality_fail")
)) {
  ok <- isTRUE(click_tab(spec[[1]], spec[[2]]))
  note(sprintf("tab %s/%s ok=%s", spec[[1]], spec[[2]], ok))
  Sys.sleep(1.2)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
  record_status(paste0("quality_", spec[[2]]))
}

stopifnot(isTRUE(click_nav("results_reliability")))
Sys.sleep(1.0)
for (spec in list(
  list("results_reliability_tabs", "MTBF", "13_results_reliability_mtbf"),
  list("results_reliability_tabs", "MTTR", "13b_results_reliability_mttr")
)) {
  ok <- isTRUE(click_tab(spec[[1]], spec[[2]]))
  note(sprintf("tab %s/%s ok=%s", spec[[1]], spec[[2]], ok))
  Sys.sleep(1.2)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
  record_status(paste0("reliability_", spec[[2]]))
}

stopifnot(isTRUE(click_nav("results_summary")))
Sys.sleep(1.0)
for (spec in list(
  list("results_summary_tabs", "Throughput", "13t_results_throughput"),
  list("results_summary_tabs", "WIP & flow", "14_results_wip"),
  list("results_summary_tabs", "Labor", "15_results_labor"),
  list("results_summary_tabs", "Downloads", "16_results_downloads")
)) {
  ok <- isTRUE(click_tab(spec[[1]], spec[[2]]))
  note(sprintf("tab %s/%s ok=%s", spec[[1]], spec[[2]], ok))
  Sys.sleep(1.4)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]], full = TRUE)
  record_status(paste0("summary_", spec[[2]]))
}

# Simulate / Results CSV downloads
ok <- wait_for("!!document.getElementById('sim_dl_units')", 45)
note(sprintf("simulate_downloads_buttons=%s", ok))
dl_lines <- c("source=Results/Summary/Downloads")
for (item in list(
  list("sim_dl_units", "units\\.csv", "sim_units.csv"),
  list("sim_dl_kpis", "kpis_by_rep\\.csv", "sim_kpis_by_rep.csv"),
  list("sim_dl_all", "spacecraft_simulate\\.zip", "spacecraft_simulate.zip")
)) {
  res <- click_and_collect(item[[1]], item[[2]], item[[3]])
  msg <- sprintf("download=%s ok=%s reason=%s peek=%s", item[[1]], res$ok, res$reason, res$peek)
  note(msg)
  dl_lines <- c(dl_lines, msg)
  if (!isTRUE(res$ok)) note("  DOWNLOAD_FAILED ", item[[1]])
}

# Compare
ok <- isTRUE(click_nav("Compare"))
note(sprintf("nav Compare ok=%s", ok))
ok <- wait_for("!!document.querySelector('.sc-compare-pick')", 90)
note(sprintf("compare_ready=%s", ok))
Sys.sleep(2.0)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("17_compare_table", TRUE)
record_status("compare_table")
picked <- ev("Array.from(document.querySelectorAll('.sc-compare-picked')).map(e => e.getAttribute('data-id')).filter(Boolean)")
note("picked_before=", paste(unlist(picked), collapse = ","))

ok <- isTRUE(click_tab("compare_spread_tabs", "Cycle time"))
note(sprintf("compare_cycle_tab ok=%s", ok))
wait_for("!!document.querySelector('#compare_plot_cycle .js-plotly-plot, #compare_plot_cycle .plotly')", 30)
Sys.sleep(1.2)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
shot("18_compare_cycle_default")
record_status("compare_cycle_default")

tog <- ev("(() => { const el = document.querySelector('.sc-compare-pick[data-id=\"faster_qa\"]'); if (!el) return 'missing'; el.click(); return el.getAttribute('data-id'); })()")
note("clicked_card=", as.character(tog))
Sys.sleep(5)
picked2 <- ev("Array.from(document.querySelectorAll('.sc-compare-picked')).map(e => e.getAttribute('data-id')).filter(Boolean)")
note("picked_after=", paste(unlist(picked2), collapse = ","))
shot("19_compare_cycle_toggled")
record_status("compare_cycle_toggled")

for (spec in list(
  list("Throughput", "21_compare_throughput"),
  list("Cycle time", "14b_compare_cycle"),
  list("Touch labor", "22_compare_hpu"),
  list("Downtime", "26_compare_downtime"),
  list("Flow", "25_compare_flow"),
  list("Downloads", "27_compare_downloads")
)) {
  ok <- isTRUE(click_tab("compare_spread_tabs", spec[[1]]))
  note(sprintf("compare_tab %s ok=%s", spec[[1]], ok))
  Sys.sleep(if (identical(spec[[1]], "Flow")) 3.0 else 1.3)
  ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
  if (identical(spec[[1]], "Flow")) ev("document.querySelector('#compare_plot_wip_flow')?.scrollIntoView({block:'start'})")
  Sys.sleep(0.5)
  shot(spec[[2]], full = identical(spec[[1]], "Flow") || identical(spec[[1]], "Downloads"))
  record_status(paste0("compare_", spec[[1]]))
}

ok <- wait_for("!!document.getElementById('cmp_dl_units')", 30)
note(sprintf("compare_downloads_buttons=%s", ok))
dl_lines <- c(dl_lines, "source=Compare")
for (item in list(
  list("cmp_dl_units", "compare_units\\.csv", "cmp_units.csv"),
  list("cmp_dl_kpis", "compare_kpis_by_rep\\.csv", "cmp_kpis_by_rep.csv"),
  list("cmp_dl_all", "spacecraft_compare\\.zip", "spacecraft_compare.zip")
)) {
  res <- click_and_collect(item[[1]], item[[2]], item[[3]])
  msg <- sprintf("download=%s ok=%s reason=%s peek=%s", item[[1]], res$ok, res$reason, res$peek)
  note(msg)
  dl_lines <- c(dl_lines, msg)
  if (!isTRUE(res$ok)) note("  DOWNLOAD_FAILED ", item[[1]])
}

# Analyze page was retired into Results.

for (pg in list(
  list("Tornado", "tornado_story", "29_tornado"),
  list("Breakpoint", "break_story", "30_breakpoint"),
  list("Optimize", "opt_story", "31_optimize"),
  list("Warmup", "wu_story", "32_warmup"),
  list("Ramp", "ramp_story", "33_ramp")
)) {
  ok <- isTRUE(click_nav(pg[[1]]))
  note(sprintf("nav %s ok=%s", pg[[1]], ok))
  ready <- wait_for(sprintf("!!((document.getElementById('%s')||{}).innerText)", pg[[2]]), 90)
  note(sprintf("%s_ready=%s", tolower(pg[[1]]), ready))
  Sys.sleep(1.6)
  ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
  shot(pg[[3]], TRUE)
  record_status(pg[[1]])
}

ok <- isTRUE(click_nav("About"))
note(sprintf("nav About ok=%s", ok))
Sys.sleep(1.2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("34_about")
record_status("about")

writeLines(lines, results_path)
writeLines(dl_lines, dl_log_path)
try(b$close(), silent = TRUE)
cat("LIVE_RC_SHOTS_DONE\n")
