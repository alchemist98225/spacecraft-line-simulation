# screenshots_compare_polish.R
# Headless shots of every chart changed in the compare-polish round:
# Compare (throughput density, cycle/hpu histograms, downtime, flow, downloads),
# a 5 s sidebar-toggle wait, Analyze (same chart types), Tornado, and the
# Settings line flow chart. Usage: Rscript scripts/screenshots_compare_polish.R <port>
# Writes shots/polish_*.png
suppressPackageStartupMessages({ library(chromote); library(jsonlite) })
a <- commandArgs(TRUE); port <- a[[1]]
out_dir <- if (length(a) > 1) a[[2]] else "shots"
width <- 1440L
dir.create(out_dir, FALSE, TRUE)
b <- ChromoteSession$new(); b$Page$enable(); b$Runtime$enable()
b$default_timeout <- 300
b$Emulation$setDeviceMetricsOverride(width = width, height = 1600L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value, error = function(e) NULL)
wait_for <- function(js, secs = 180) {
  for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }
  FALSE
}
shot <- function(name, full = FALSE) {
  path <- file.path(out_dir, paste0("polish_", name, ".png"))
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
picked_ids <- function() {
  ev("Array.from(document.querySelectorAll('.sc-compare-picked')).map(e => e.getAttribute('data-id')).filter(Boolean)")
}

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
ok <- wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 240)
cat("simulate_ready=", ok, "\n", sep = "")
if (!isTRUE(ok)) stop("Simulate did not become ready")
Sys.sleep(1.2)

# --- Line flow chart (Settings > Variants) --------------------------------
stopifnot(isTRUE(click_nav("Settings")))
ok <- wait_for("!!document.querySelector('.sc-flow-svg')", 30)
cat("flow_ready=", ok, " qa_int=", ev("!!document.getElementById('sc-flow-qa-int')"), "\n", sep = "")
Sys.sleep(1.0)
ev("document.querySelector('.sc-flow-wrap, .sc-flow-svg')?.scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("01_settings_flow")
ev("document.querySelector('.card .bslib-full-screen-enter')?.click()")
Sys.sleep(1.5)
shot("01b_settings_flow_full")
ev("document.querySelector('.bslib-full-screen-exit, .card[data-full-screen=\"true\"] .bslib-full-screen-enter')?.click()")
Sys.sleep(0.8)

# --- Compare --------------------------------------------------------------
stopifnot(isTRUE(click_nav("Compare")))
ok <- wait_for("!!document.querySelector('.sc-compare-pick') && !!((document.getElementById('compare_status')||{}).innerText)", 60)
cat("compare_ready=", ok, "\n", sep = "")
Sys.sleep(2.0)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("02_compare_table", TRUE)

stopifnot(isTRUE(click_tab("compare_spread_tabs", "Throughput")))
ok <- wait_for("!!document.querySelector('#compare_plot_throughput .js-plotly-plot, #compare_plot_throughput img, #compare_plot_throughput svg')", 30)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(1.2)
shot("03_compare_throughput_default")
before_ids <- picked_ids()
cat("picked_before=", paste(unlist(before_ids), collapse = ","), "\n", sep = "")

# Sidebar toggle, wait 5 s so plotly can remount.
tog <- ev("(() => {
  const el = document.querySelector('.sc-compare-pick[data-id=\"faster_qa\"], .sc-compare-pick[data-id=\"faster\"]');
  if (!el) return 'missing';
  el.click();
  return el.getAttribute('data-id');
})()")
cat("clicked_card=", tog, "\n", sep = "")
Sys.sleep(5)
after_ids <- picked_ids()
cat("picked_after=", paste(unlist(after_ids), collapse = ","), "\n", sep = "")
wait_for("!!document.querySelector('#compare_plot_throughput .js-plotly-plot, #compare_plot_throughput img, #compare_plot_throughput svg')", 20)
Sys.sleep(0.8)
shot("04_compare_throughput_toggled")
cat("sidebar_changed=", !identical(paste(unlist(before_ids), collapse = ","), paste(unlist(after_ids), collapse = ",")), "\n", sep = "")

for (spec in list(
  list("Cycle time", "05_compare_cycle"),
  list("Touch labor", "06_compare_hpu"),
  list("Downtime", "07_compare_downtime"),
  list("Flow", "08_compare_flow"),
  list("Downloads", "09_compare_downloads")
)) {
  stopifnot(isTRUE(click_tab("compare_spread_tabs", spec[[1]])))
  Sys.sleep(if (identical(spec[[1]], "Flow")) 2.5 else 1.5)
  ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
  if (identical(spec[[1]], "Downtime")) ev("document.querySelector('#compare_plot_down')?.scrollIntoView({block:'start'})")
  if (identical(spec[[1]], "Flow")) ev("document.querySelector('#compare_plot_wip_flow')?.scrollIntoView({block:'start'})")
  Sys.sleep(0.5)
  shot(spec[[2]])
}

# --- Analyze --------------------------------------------------------------
stopifnot(isTRUE(click_nav("Analyze")))
ok <- wait_for("!!((document.getElementById('analyze_story')||{}).innerText)", 60)
cat("analyze_ready=", ok, "\n", sep = "")
Sys.sleep(1.4)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("10_analyze_story", TRUE)
for (spec in list(
  list("Throughput", "11_analyze_throughput"),
  list("Cycle time", "12_analyze_cycle"),
  list("Touch labor", "13_analyze_hpu"),
  list("Downtime", "14_analyze_downtime")
)) {
  stopifnot(isTRUE(click_tab("analyze_result_tabs", spec[[1]])))
  Sys.sleep(1.4)
  ev("document.querySelector('#analyze_result_tabs')?.scrollIntoView({block:'start'})")
  Sys.sleep(0.4)
  shot(spec[[2]])
}

# --- Tornado --------------------------------------------------------------
stopifnot(isTRUE(click_nav("Tornado")))
ok <- wait_for("!!((document.getElementById('tornado_story')||{}).innerText)", 60)
cat("tornado_ready=", ok, "\n", sep = "")
Sys.sleep(2.0)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("15_tornado_story", TRUE)
ev("document.querySelector('.card') && Array.from(document.querySelectorAll('.card')).find(c => (c.innerText||'').includes('Sensitivity') || (c.querySelector('img,canvas,svg')))?.scrollIntoView({block:'start'})")
# The plot card
ev("document.querySelector('#tornado_plot, #tornado_chart')?.scrollIntoView({block:'start'})")
Sys.sleep(0.6)
shot("16_tornado_chart")
ev("document.querySelector('#tornado_table, .gt_table')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
shot("17_tornado_table")

cat("done\n")
try(b$close(), silent = TRUE)
