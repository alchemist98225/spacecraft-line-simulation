# screenshots_ui_polish.R
# Headless shots of every page/tab touched by the UI polish.
# Usage: Rscript scripts/screenshots_ui_polish.R <port>
# Writes shots/ui_polish_*.png
suppressPackageStartupMessages({ library(chromote) })
a <- commandArgs(TRUE); port <- a[[1]]
width <- 1440L
out_dir <- "shots"
dir.create(out_dir, FALSE, TRUE)
b <- ChromoteSession$new(); b$Page$enable(); b$Runtime$enable()
b$Emulation$setDeviceMetricsOverride(width = width, height = 1600L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value, error = function(e) NULL)
wait_for <- function(js, secs = 180) {
  for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }
  FALSE
}
shot <- function(name, full = FALSE) {
  path <- file.path(out_dir, paste0("ui_polish_", name, ".png"))
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
stopifnot(wait_for("!!document.querySelector('#table_cycle .gt_table, #table_cycle table')", 60))
Sys.sleep(2)

# Accordion: Run simulation should start collapsed.
run_open <- ev("(() => { const p = document.querySelector('#sc_controls .accordion-collapse[id], #sc_controls .accordion-item [data-value=\"run\"], #sc_controls .accordion-item'); const items = Array.from(document.querySelectorAll('#sc_controls .accordion-collapse')); const run = items[0]; return run ? run.classList.contains('show') : null; })()")
cat("run_accordion_open=", run_open, "\n", sep = "")
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("01_simulate_collapsed", FALSE)

for (spec in list(
  list("Cycle Times", "02_simulate_cycle", "!!document.querySelector('#table_cycle .gt_table, #table_cycle table')"),
  list("Schedule Slip", "03_simulate_slip", "!!document.querySelector('#table_lateness .gt_table, #table_lateness table')"),
  list("WIP", "04_simulate_wip", "!!document.querySelector('#plot_wip img, #plot_wip canvas, #plot_wip.shiny-plot-output')"),
  list("Summary", "05_simulate_summary", "!!document.querySelector('#summary_table table')")
)) {
  stopifnot(isTRUE(click_tab("simulate_result_tabs", spec[[1]])))
  stopifnot(wait_for(spec[[3]], 30))
  Sys.sleep(1)
  ev("document.querySelector('#simulate_result_tabs').scrollIntoView({block:'start'})")
  Sys.sleep(0.4)
  shot(spec[[2]], FALSE)
}

# Settings: flow chart and variant table
stopifnot(isTRUE(click_nav("Settings")))
Sys.sleep(2)
wait_for("!!document.querySelector('.sc-flow-svg')", 30)
ev("window.scrollTo(0,0)"); Sys.sleep(0.5)
shot("06_settings_flow", FALSE)
ev("document.querySelector('#sc-variant-settings, #variants_table').scrollIntoView({block:'start'})")
Sys.sleep(1)
shot("07_settings_variant_table", FALSE)

# Compare
stopifnot(isTRUE(click_nav("Compare")))
stopifnot(wait_for("!!document.querySelector('#compare_table .gt_table, #compare_table table')", 60))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("08_compare_top", FALSE)
ev("document.querySelector('#compare_table').scrollIntoView({block:'start'})"); Sys.sleep(0.8)
shot("09_compare_table", FALSE)
for (spec in list(
  list("Throughput", "10_compare_throughput"),
  list("Cycle time", "11_compare_cycle"),
  list("On time", "12_compare_ontime"),
  list("Schedule slip", "13_compare_slip"),
  list("WIP", "14_compare_wip")
)) {
  stopifnot(isTRUE(click_tab("compare_spread_tabs", spec[[1]])))
  Sys.sleep(1.2)
  ev("document.querySelector('#compare_spread_tabs').scrollIntoView({block:'start'})")
  Sys.sleep(0.3)
  shot(spec[[2]], FALSE)
}

# Analyze
stopifnot(isTRUE(click_nav("Analyze")))
stopifnot(wait_for("!!((document.getElementById('analyze_story')||{}).innerText)", 60))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("15_analyze_top", FALSE)
stopifnot(isTRUE(click_tab("analyze_station_tabs", "Bottleneck")))
Sys.sleep(1.2)
ev("document.querySelector('#analyze_station_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("16_analyze_bottleneck", FALSE)
stopifnot(isTRUE(click_tab("analyze_station_tabs", "Queue length")))
Sys.sleep(1.2)
ev("document.querySelector('#analyze_station_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("17_analyze_queue", FALSE)

# Tornado table
stopifnot(isTRUE(click_nav("Tornado")))
stopifnot(wait_for("!!document.querySelector('#torn_table .gt_table, #torn_table table, [id*=torn] .gt_table')", 60))
Sys.sleep(1.5)
# find the results table
ev("(() => { const t = document.querySelector('#torn_table, .gt_table'); if (t) t.scrollIntoView({block:'start'}); })()")
# Prefer the table under the chart: last gt table on the page
ev("(() => { const ts = document.querySelectorAll('.gt_table'); if (ts.length) ts[ts.length-1].scrollIntoView({block:'center'}); })()")
Sys.sleep(0.6)
shot("18_tornado_table", FALSE)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("18b_tornado_top", FALSE)

# Optimize
stopifnot(isTRUE(click_nav("Optimize")))
stopifnot(wait_for("!!((document.getElementById('opt_story')||{}).innerText)", 60))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("19_optimize_top", FALSE)
ev("(() => { const t = document.querySelector('#opt_table, #opt_story'); if (t) t.scrollIntoView({block:'start'}); })()")
Sys.sleep(0.6)
shot("20_optimize_result", FALSE)
ost <- ev("(document.getElementById('opt_story')||{}).innerText || ''")
cat("optimize_story=", ost, "\n", sep = "")

# About
stopifnot(isTRUE(click_nav("About")))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("21_about", FALSE)
shot("21b_about_full", TRUE)

try(b$close(), silent = TRUE)
cat("UI_POLISH_SHOTS_DONE\n")
