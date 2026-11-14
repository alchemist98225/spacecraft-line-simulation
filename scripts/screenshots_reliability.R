# screenshots_reliability.R
# Headless shots of every tab after the reliability + audit-port work.
# Usage: Rscript scripts/screenshots_reliability.R <port>
# Writes shots/rel_*.png
suppressPackageStartupMessages({ library(chromote) })
a <- commandArgs(TRUE); port <- a[[1]]
width <- 1440L
out_dir <- "shots"
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
  path <- file.path(out_dir, paste0("rel_", name, ".png"))
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png", captureBeyondViewport = full)$data), path)
  cat("shot", path, "\n")
  path
}
click_nav <- function(value) {
  ev(sprintf("(() => { const a = document.querySelector(\"a[data-value='%s']\"); if (!a) return false; a.click(); return true; })()", value))
}
click_tab <- function(container, value) {
  ev(sprintf("(() => { const a = document.querySelector(\"#%s a[data-value='%s']\"); if (!a) return false; a.click(); return true; })()", container, value))
}

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
stopifnot(wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 180))
Sys.sleep(1)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("01_simulate_kpis")

for (spec in list(
  list("simulate_result_tabs", "WIP", "02_simulate_wip"),
  list("simulate_result_tabs", "Demand vs completions", "03_simulate_demand"),
  list("simulate_result_tabs", "Downloads", "04_simulate_downloads"),
  list("simulate_result_tabs", "Cycle Times", "05_simulate_cycle"),
  list("simulate_result_tabs", "Touch Labor", "06_simulate_hpu"),
  list("simulate_result_tabs", "Summary", "07_simulate_summary")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.0)
  ev("document.querySelector('#simulate_result_tabs').scrollIntoView({block:'start'})")
  Sys.sleep(0.3)
  shot(spec[[3]])
}

# Settings: Equipment reliability card
stopifnot(isTRUE(click_nav("Settings")))
Sys.sleep(1.2)
# Line settings tab
ev("(() => { const a = document.querySelector(\"#settings_tabs a[data-value='Line settings'], a[data-value='Line settings']\"); if (a) a.click(); return true; })()")
Sys.sleep(0.8)
# Try to scroll the reliability card into view
ev("(() => { const el = document.getElementById('sc-line-reliability'); if (el) el.scrollIntoView({block:'start'}); return !!el; })()")
Sys.sleep(0.5)
shot("08_settings_reliability", TRUE)

stopifnot(isTRUE(click_nav("Compare")))
ok <- wait_for("!!((document.getElementById('compare_status')||{}).innerText)", 60)
cat("compare_ready=", ok, "\n", sep = "")
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("09_compare_table", TRUE)
for (spec in list(
  list("compare_spread_tabs", "Flow", "10_compare_flow"),
  list("compare_spread_tabs", "Downtime", "11_compare_downtime"),
  list("compare_spread_tabs", "Downloads", "12_compare_downloads"),
  list("compare_spread_tabs", "Throughput", "13_compare_throughput"),
  list("compare_spread_tabs", "Cycle time", "14_compare_cycle")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(if (identical(spec[[2]], "Flow")) 3.0 else 1.2)
  ev("document.querySelector('#compare_spread_tabs').scrollIntoView({block:'start'})")
  if (identical(spec[[2]], "Flow")) ev("document.querySelector('#compare_plot_wip_flow')?.scrollIntoView({block:'start'})")
  Sys.sleep(0.5)
  shot(spec[[3]], full = identical(spec[[2]], "Flow"))
}

stopifnot(isTRUE(click_nav("Analyze")))
ok <- wait_for("!!((document.getElementById('analyze_story')||{}).innerText)", 60)
cat("analyze_ready=", ok, "\n", sep = "")
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("15_analyze_story", TRUE)
for (spec in list(
  list("analyze_result_tabs", "Time Breakdown", "16_analyze_breakdown"),
  list("analyze_result_tabs", "Bottleneck", "17_analyze_bottleneck"),
  list("analyze_result_tabs", "Why stations wait", "18_analyze_waits"),
  list("analyze_result_tabs", "Downtime", "19_analyze_downtime"),
  list("analyze_result_tabs", "Queue Length", "20_analyze_queues")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.2)
  ev("document.querySelector('#analyze_result_tabs').scrollIntoView({block:'start'})")
  Sys.sleep(0.3)
  shot(spec[[3]])
}

stopifnot(isTRUE(click_nav("Tornado")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("21_tornado", TRUE)

stopifnot(isTRUE(click_nav("Breakpoint")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("22_breakpoint", TRUE)

stopifnot(isTRUE(click_nav("Optimize")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("23_optimize", TRUE)

stopifnot(isTRUE(click_nav("Warmup")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("24_warmup", TRUE)

stopifnot(isTRUE(click_nav("Ramp")))
Sys.sleep(2)
ev("document.querySelector('#ramp_result_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
shot("25_ramp", TRUE)
stopifnot(isTRUE(click_tab("ramp_result_tabs", "Throughput")))
Sys.sleep(1.0)
shot("26_ramp_throughput")

stopifnot(isTRUE(click_nav("About")))
Sys.sleep(1.2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("27_about")
ev("(() => { const el = Array.from(document.querySelectorAll('li, p, span')).find(e => (e.innerText||'').includes('Equipment reliability')); if (el) el.scrollIntoView({block:'start'}); return !!el; })()")
Sys.sleep(0.4)
shot("28_about_reliability")

try(b$close(), silent = TRUE)
cat("REL_SHOTS_DONE\n")
