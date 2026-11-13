# screenshots_labor.R
# Headless shots of every tab after the labor + headline-metrics work.
# Usage: Rscript scripts/screenshots_labor.R <port>
# Writes shots/labor_*.png
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
  path <- file.path(out_dir, paste0("labor_", name, ".png"))
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
Sys.sleep(2)
kpis <- ev("['kpi_throughput','kpi_cycle','kpi_hpu','kpi_labor','kpi_wip'].map(i => (document.getElementById(i)||{}).innerText).join(' | ')")
cat("kpis:", kpis, "\n")
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("01_simulate_kpis")
stopifnot(isTRUE(click_tab("simulate_result_tabs", "Cycle Times")))
Sys.sleep(1.5)
ev("document.querySelector('#simulate_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("02_simulate_cycle")
stopifnot(isTRUE(click_tab("simulate_result_tabs", "Touch Labor")))
Sys.sleep(1.5)
ev("document.querySelector('#simulate_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("03_simulate_hpu")
stopifnot(isTRUE(click_tab("simulate_result_tabs", "WIP")))
Sys.sleep(1.2)
ev("document.querySelector('#simulate_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("04_simulate_wip")
stopifnot(isTRUE(click_tab("simulate_result_tabs", "Summary")))
Sys.sleep(1)
ev("document.querySelector('#simulate_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("05_simulate_summary")

stopifnot(isTRUE(click_nav("Settings")))
stopifnot(wait_for("!!document.querySelector('.sc-flow-svg')", 30))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.5)
shot("06_settings_flow")
# Click a star node to Line settings
clicked <- ev("(() => { const g = document.querySelector('.sc-flow-hit[aria-label*=\"Kitting\"]'); if (!g) return false; g.dispatchEvent(new MouseEvent('click', {bubbles:true})); return true; })()")
cat("clicked_kitting=", clicked, "\n", sep = "")
Sys.sleep(1.5)
ev("(() => { const el = document.getElementById('sc-line-kitting'); if (el) el.scrollIntoView({block:'center'}); return !!el; })()")
Sys.sleep(0.6)
shot("07_settings_after_kitting_click")
# Labor table
ev("(() => { const el = document.getElementById('sc-line-labor') || document.getElementById('labor_step_techs') || document.querySelector('[id*=labor]'); if (el) el.scrollIntoView({block:'start'}); return el ? el.id : null; })()") %>%
  { cat("labor_el=", ev("(() => { const el = document.getElementById('sc-line-labor') || document.querySelector('#labor_table, #step_techs_table, [id*=labor]'); if (el) { el.scrollIntoView({block:'start'}); return el.id || el.className; } return 'none'; })()"), "\n", sep = "") }
Sys.sleep(0.8)
shot("08_settings_labor")

# Compare
stopifnot(isTRUE(click_nav("Compare")))
stopifnot(wait_for("!!document.querySelector('#compare_table table, #compare_table .gt_table')", 90))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("09_compare_table")
for (tab in c("Throughput", "Cycle time", "Touch labor", "Labor utilization", "WIP")) {
  stopifnot(isTRUE(click_tab("compare_spread_tabs", tab)))
  Sys.sleep(1.0)
  ev("document.querySelector('#compare_spread_tabs').scrollIntoView({block:'start'})")
  Sys.sleep(0.3)
  slug <- gsub(" ", "_", tolower(tab))
  shot(paste0("10_compare_", slug))
}

# Analyze
stopifnot(isTRUE(click_nav("Analyze")))
stopifnot(wait_for("!!((document.getElementById('analyze_story')||{}).innerText)", 90))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("11_analyze_story")
ev("document.querySelector('#analyze_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("12_analyze_time_breakdown")
stopifnot(isTRUE(click_tab("analyze_result_tabs", "Bottleneck")))
Sys.sleep(1.2)
ev("document.querySelector('#analyze_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("13_analyze_bottleneck")
stopifnot(isTRUE(click_tab("analyze_result_tabs", "Queue Length")))
Sys.sleep(1.2)
ev("document.querySelector('#analyze_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("14_analyze_queue")

# Tornado
stopifnot(isTRUE(click_nav("Tornado")))
stopifnot(wait_for("!!((document.getElementById('torn_story')||{}).innerText) || !!document.querySelector('#torn_plot, #tornado_plot')", 90))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("15_tornado")

# Breakpoint
stopifnot(isTRUE(click_nav("Breakpoint")))
stopifnot(wait_for("!!((document.getElementById('break_story')||{}).innerText)", 90))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("16_breakpoint_story")
ev("document.querySelector('#break_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("17_breakpoint_throughput")
stopifnot(isTRUE(click_tab("break_result_tabs", "Cycle Time")))
Sys.sleep(1.2)
ev("document.querySelector('#break_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("18_breakpoint_cycle")
stopifnot(isTRUE(click_tab("break_result_tabs", "Saturation")))
Sys.sleep(1.2)
ev("document.querySelector('#break_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("19_breakpoint_saturation")

# Optimize
stopifnot(isTRUE(click_nav("Optimize")))
stopifnot(wait_for("!!((document.getElementById('opt_story')||{}).innerText)", 90))
Sys.sleep(1.5)
algo_open <- ev("(() => { const acc = document.querySelector('#opt_algo_settings .accordion-collapse'); return acc ? acc.classList.contains('show') : null; })()")
cat("opt_algo_open=", algo_open, "\n", sep = "")
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("20_optimize_sidebar")
ev("(() => { const el = document.getElementById('opt_plot') || document.getElementById('opt_story'); if (el) el.scrollIntoView({block:'start'}); })()")
Sys.sleep(0.5)
shot("21_optimize_chart")
ost <- ev("(document.getElementById('opt_story')||{}).innerText || ''")
cat("optimize_story=", ost, "\n", sep = "")

# Warmup
stopifnot(isTRUE(click_nav("Warmup")))
stopifnot(wait_for("!!((document.getElementById('wu_story')||{}).innerText)", 90))
Sys.sleep(1.5)
ev("document.querySelector('#wu_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("22_warmup_cycle")
stopifnot(isTRUE(click_tab("wu_result_tabs", "WIP")))
Sys.sleep(1.2)
ev("document.querySelector('#wu_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("23_warmup_wip")
stopifnot(isTRUE(click_tab("wu_result_tabs", "KPIs")))
Sys.sleep(1.2)
ev("document.querySelector('#wu_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("24_warmup_kpis")

# About
stopifnot(isTRUE(click_nav("About")))
Sys.sleep(1)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("25_about")

try(b$close(), silent = TRUE)
cat("LABOR_SHOTS_DONE\n")
