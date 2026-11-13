# screenshots_ui_polish2.R
# Headless shots of every page/tab touched by the second UI polish batch.
# Usage: Rscript scripts/screenshots_ui_polish2.R <port>
# Writes shots/ui_polish2_*.png
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
  path <- file.path(out_dir, paste0("ui_polish2_", name, ".png"))
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

# Simulate summary caption
stopifnot(isTRUE(click_tab("simulate_result_tabs", "Summary")))
stopifnot(wait_for("!!document.querySelector('#summary_table table, #summary_table')", 45))
Sys.sleep(1)
ev("document.querySelector('#simulate_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("01_simulate_summary")

# Settings: flow (Scrap label) and variant table
stopifnot(isTRUE(click_nav("Settings")))
stopifnot(wait_for("!!document.querySelector('.sc-flow-svg')", 30))
Sys.sleep(1.5)
ev("window.scrollTo(0,0)"); Sys.sleep(0.5)
shot("02_settings_flow")
ev("document.querySelector('#sc-variant-settings, #variants_table').scrollIntoView({block:'start'})")
Sys.sleep(1)
shot("03_settings_variant_table")

# Click Kitting node -> Line settings, kitting card
clicked <- ev("(() => { const g = document.querySelector('.sc-flow-hit[aria-label*=\"Kitting\"]'); if (!g) return false; g.dispatchEvent(new MouseEvent('click', {bubbles:true})); return true; })()")
cat("clicked_kitting=", clicked, "\n", sep = "")
Sys.sleep(1.5)
ok <- ev("(() => { const tab = document.querySelector('#settings_tabs a[data-value=\"Line settings\"]'); const card = document.getElementById('sc-line-kitting'); return !!(tab && tab.classList.contains('active') || (card && card.getBoundingClientRect().top < 900)); })()")
# also check the Line settings tab is selected
tab_sel <- ev("(() => { const a = document.querySelector('#settings_tabs a[data-value=\"Line settings\"]'); if (!a) return null; return a.classList.contains('active') || a.getAttribute('aria-selected') === 'true' || (a.closest('.nav-link')||a).classList.contains('active'); })()")
cat("line_settings_selected=", tab_sel, "\n", sep = "")
ev("window.scrollTo(0,0)")
# if the kitting card exists, bring it into view
ev("(() => { const el = document.getElementById('sc-line-kitting'); if (el) el.scrollIntoView({block:'center'}); return !!el; })()")
Sys.sleep(0.6)
shot("04_settings_after_kitting_click")

# Analyze tabs
stopifnot(isTRUE(click_nav("Analyze")))
stopifnot(wait_for("!!((document.getElementById('analyze_story')||{}).innerText)", 90))
Sys.sleep(1.5)
ev("document.querySelector('#analyze_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("05_analyze_time_breakdown")
stopifnot(isTRUE(click_tab("analyze_result_tabs", "Bottleneck")))
Sys.sleep(1.2)
ev("document.querySelector('#analyze_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("06_analyze_bottleneck")
stopifnot(isTRUE(click_tab("analyze_result_tabs", "Queue Length")))
Sys.sleep(1.2)
ev("document.querySelector('#analyze_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("07_analyze_queue")

# Breakpoint tabs
stopifnot(isTRUE(click_nav("Breakpoint")))
stopifnot(wait_for("!!((document.getElementById('break_story')||{}).innerText)", 90))
Sys.sleep(1.5)
ev("document.querySelector('#break_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("08_breakpoint_delivery")
stopifnot(isTRUE(click_tab("break_result_tabs", "Cycle Time")))
Sys.sleep(1.2)
ev("document.querySelector('#break_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("09_breakpoint_cycle")
stopifnot(isTRUE(click_tab("break_result_tabs", "Saturation")))
Sys.sleep(1.2)
ev("document.querySelector('#break_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("10_breakpoint_saturation")

# Tornado: full chart and a close-up of the markers
stopifnot(isTRUE(click_nav("Tornado")))
stopifnot(wait_for("!!document.querySelector('#torn_plot img, #torn_plot canvas, #torn_plot, [id*=torn]')", 90))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("11_tornado_top")
# close-up: clip around the tornado plot
clip <- ev("(() => { const el = document.querySelector('#torn_plot, .sc-tornado-plot, #tornado_plot'); const card = el && (el.closest('.card')||el); if (!card) return null; const r = card.getBoundingClientRect(); return {x: Math.max(0,r.x), y: Math.max(0,r.y), width: Math.max(1,r.width), height: Math.max(1, Math.min(r.height, 700))}; })()")
if (!is.null(clip) && isTRUE(clip$width > 0)) {
  path <- file.path(out_dir, "ui_polish2_12_tornado_markers.png")
  raw <- b$Page$captureScreenshot(format = "png", captureBeyondViewport = FALSE,
                                  clip = list(x = clip$x, y = clip$y, width = clip$width, height = clip$height, scale = 1.5))$data
  writeBin(jsonlite::base64_dec(raw), path)
  cat("shot", path, "\n")
} else {
  ev("(() => { const el = document.querySelector('#torn_plot'); if (el) el.scrollIntoView({block:'center'}); })()")
  Sys.sleep(0.4)
  shot("12_tornado_markers")
}

# Optimize: collapsed algo panel + improving best line
stopifnot(isTRUE(click_nav("Optimize")))
stopifnot(wait_for("!!((document.getElementById('opt_story')||{}).innerText)", 90))
Sys.sleep(1.5)
# confirm accordion collapsed
algo_open <- ev("(() => { const acc = document.querySelector('#opt_algo_settings .accordion-collapse'); return acc ? acc.classList.contains('show') : null; })()")
cat("opt_algo_open=", algo_open, "\n", sep = "")
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("13_optimize_sidebar_collapsed")
ev("(() => { const el = document.getElementById('opt_plot') || document.getElementById('opt_story'); if (el) el.scrollIntoView({block:'start'}); })()")
Sys.sleep(0.5)
shot("14_optimize_best_line")
ost <- ev("(document.getElementById('opt_story')||{}).innerText || ''")
cat("optimize_story=", ost, "\n", sep = "")

# Warmup tabs
stopifnot(isTRUE(click_nav("Warmup")))
stopifnot(wait_for("!!((document.getElementById('wu_story')||{}).innerText)", 90))
Sys.sleep(1.5)
ev("document.querySelector('#wu_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("15_warmup_cycle")
stopifnot(isTRUE(click_tab("wu_result_tabs", "WIP")))
Sys.sleep(1.2)
ev("document.querySelector('#wu_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("16_warmup_wip")
stopifnot(isTRUE(click_tab("wu_result_tabs", "KPIs")))
Sys.sleep(1.2)
ev("document.querySelector('#wu_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.3)
shot("17_warmup_kpis")

try(b$close(), silent = TRUE)
cat("UI_POLISH2_SHOTS_DONE\n")
