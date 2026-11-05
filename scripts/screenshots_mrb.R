# screenshots_mrb.R
# Headless shots of the Material review board inputs (sidebar and Line settings).
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
setnum <- function(id, v) ev(sprintf("(()=>{const e=document.getElementById('%s'); e.value=%s; e.dispatchEvent(new Event('change',{bubbles:true}));})()", id, v))
txt <- function(id) ev(sprintf("document.getElementById('%s').innerText", id))
ev("document.querySelector('[data-value=mrb] .accordion-button').click()"); Sys.sleep(1.5)
ev("document.querySelector('[data-value=mrb] .accordion-button').scrollIntoView({block:'start'})"); Sys.sleep(1)
shot("10_sidebar_mrb", FALSE); cat("sidebar rework:", txt("mrb_rework"), "\n")
setnum("mrb_p_use_as_is", 0.8); setnum("mrb_p_scrap", 0.2); Sys.sleep(1.5)
cat("0.8/0.2 rework:", txt("mrb_rework"), "\n"); shot("10b_sidebar_mrb_0.8_0.2", FALSE)
setnum("mrb_p_use_as_is", 0.9); Sys.sleep(1.5); cat("over 1:", txt("mrb_rework"), "\n"); shot("10c_sidebar_mrb_over_1", FALSE)
setnum("mrb_p_use_as_is", 0.5); setnum("mrb_p_scrap", 0.15); Sys.sleep(1.5)
ev("document.querySelector(\"a[data-value='Settings']\").click()"); Sys.sleep(2)
ev("document.querySelector(\"a[data-value='Line settings']\").click()"); Sys.sleep(2)
ev("document.getElementById('set_mrb_p_use_as_is').closest('.card').scrollIntoView({block:'start'})"); Sys.sleep(1)
cat("settings rework:", txt("set_mrb_rework"), "\n"); shot("11_settings_line_mrb", FALSE)
