# screenshots.R
# Headless screenshots of the main pages from a locally running app.
# Usage: Rscript scripts/screenshots.R <port> [width] [prefix]
# Writes shots/<prefix>NN_page.png. Width defaults to 1400 (1440 ~ a laptop).
suppressPackageStartupMessages({ library(chromote) })
a <- commandArgs(TRUE); port <- a[[1]]
width <- if (length(a) > 1) as.integer(a[[2]]) else 1400L
prefix <- if (length(a) > 2) a[[3]] else ""
b <- ChromoteSession$new(); b$Page$enable()
b$Emulation$setDeviceMetricsOverride(width = width, height = 1000L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value, error = function(e) NULL)
shot <- function(n, full = TRUE) writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png", captureBeyondViewport = full)$data), file.path("shots", paste0(prefix, n, ".png")))
wait_for <- function(js, secs = 180) { for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }; FALSE }
invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')")
Sys.sleep(4); cat("kpis:", ev("['kpi_cycle','kpi_ontime','kpi_wip','kpi_chamber','kpi_mrb','kpi_scrap'].map(i=>document.getElementById(i).innerText).join(' | ')"), "\n")
shot("00_banner", FALSE)
shot("01_simulate")
ev("document.getElementById('plot_cycle').scrollIntoView({block:'start'})"); Sys.sleep(2); shot("01b_simulate_histograms", FALSE)
ev("window.scrollTo(0,0)")
setnum <- function(id, v) ev(sprintf("(()=>{const e=document.getElementById('%s'); e.value=%s; e.dispatchEvent(new Event('change',{bubbles:true}));})()", id, v))
val <- function(id) ev(sprintf("document.getElementById('%s').value", id))
ev("document.querySelector(\"a[data-value='Settings']\").click()"); Sys.sleep(3); shot("02_settings_variants")
# Expand the flow-picture card to full screen, capture, then close it.
ev("document.querySelector('.card .bslib-full-screen-enter').click()"); Sys.sleep(2); shot("02a_flow_full_screen", FALSE)
ev("document.querySelector('.bslib-full-screen-exit').click()"); Sys.sleep(1)
ev("(()=>{const s=document.getElementById('variant_pick'); s.selectize ? s.selectize.setValue('C') : (s.value='C', s.dispatchEvent(new Event('change',{bubbles:true})));})()"); Sys.sleep(3)
ev("document.getElementById('variants_table').scrollIntoView({block:'start'})"); Sys.sleep(1); shot("02b_settings_variant_C_table", FALSE)
ev("window.scrollTo(0,0)"); ev("document.querySelector(\"a[data-value='Line settings']\").click()"); Sys.sleep(2)
# Two-way sync in a real browser: sidebar -> Settings, then Settings + Save -> sidebar.
setnum("sim_hours", 5000); Sys.sleep(2); cat("sidebar->settings set_sim_hours:", val("set_sim_hours"), "\n")
setnum("set_p_fail_func", 0.2); Sys.sleep(1.5); cat("status before save:", ev("document.getElementById('settings_save_status').innerText"), "\n")
ev("window.scrollTo(0, document.body.scrollHeight)"); Sys.sleep(1.5); shot("02c_settings_line_unsaved_scrolled", FALSE)
ev("document.getElementById('settings_save').click()"); Sys.sleep(2.5)
cat("settings->sidebar p_fail_func:", val("p_fail_func"), "| status:", ev("document.getElementById('settings_save_status').innerText"), "\n")
shot("02d_settings_line_saved_scrolled", FALSE)
setnum("sim_hours", 8736); setnum("p_fail_func", 0.15); Sys.sleep(2)
cat("restored:", val("set_sim_hours"), val("set_p_fail_func"), ev("document.getElementById('settings_save_status').innerText"), "\n")
ev("window.scrollTo(0,0)")
ev("document.querySelector(\"a[data-value='Optimize']\").click()"); Sys.sleep(1)
t0 <- Sys.time(); ev("document.getElementById('ga_run').click()")
wait_for("(document.getElementById('ga_status')||{}).innerText?.includes('Done')", 700)
cat("ga_sec:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "\n", ev("document.getElementById('ga_status').innerText"), "\n", ev("document.getElementById('ga_table').innerText"), "\n")
Sys.sleep(3); shot("03_optimize")
ev("document.querySelector(\"a[data-value='About']\").click()"); Sys.sleep(1); shot("04_about", FALSE)
