# screenshots.R
# Headless screenshots of the main pages from a locally running app.
# Usage: Rscript scripts/screenshots.R <port>
suppressPackageStartupMessages({ library(chromote) })
port <- commandArgs(TRUE)[[1]]
b <- ChromoteSession$new(); b$Page$enable()
b$Emulation$setDeviceMetricsOverride(width = 1400L, height = 1100L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value, error = function(e) NULL)
shot <- function(n, full = TRUE) writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png", captureBeyondViewport = full)$data), file.path("shots", paste0(n, ".png")))
wait_for <- function(js, secs = 180) { for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }; FALSE }
invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')")
Sys.sleep(4); cat("kpis:", ev("['kpi_cycle','kpi_ontime','kpi_wip','kpi_chamber','kpi_mrb','kpi_scrap'].map(i=>document.getElementById(i).innerText).join(' | ')"), "\n")
shot("01_simulate")
ev("document.querySelector(\"a[data-value='Variants']\").click()"); Sys.sleep(3); shot("02_variants", FALSE)
ev("document.querySelector(\"a[data-value='Optimize']\").click()"); Sys.sleep(1)
t0 <- Sys.time(); ev("document.getElementById('ga_run').click()")
wait_for("(document.getElementById('ga_status')||{}).innerText?.includes('Done')", 700)
cat("ga_sec:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "\n", ev("document.getElementById('ga_status').innerText"), "\n", ev("document.getElementById('ga_table').innerText"), "\n")
Sys.sleep(3); shot("03_optimize")
ev("document.querySelector(\"a[data-value='About']\").click()"); Sys.sleep(1); shot("04_about", FALSE)
