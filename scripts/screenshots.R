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
ev("document.querySelector(\"a[data-value='Parameters']\").click()"); Sys.sleep(3); shot("02_parameters")
ev("(()=>{const s=document.getElementById('variant_pick'); if (s.selectize) s.selectize.setValue('C'); else {s.value='C'; s.dispatchEvent(new Event('change',{bubbles:true}));}})()"); Sys.sleep(3)
ev("document.getElementById('variants_table').scrollIntoView({block:'start'})"); Sys.sleep(1); shot("02b_parameters_variant_C_table", FALSE)
ev("window.scrollTo(0,0)")
ev("document.querySelector(\"a[data-value='Optimize']\").click()"); Sys.sleep(1)
t0 <- Sys.time(); ev("document.getElementById('ga_run').click()")
wait_for("(document.getElementById('ga_status')||{}).innerText?.includes('Done')", 700)
cat("ga_sec:", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "\n", ev("document.getElementById('ga_status').innerText"), "\n", ev("document.getElementById('ga_table').innerText"), "\n")
Sys.sleep(3); shot("03_optimize")
ev("document.querySelector(\"a[data-value='About']\").click()"); Sys.sleep(1); shot("04_about", FALSE)
