# screenshots_flow.R
# Headless shots of the Settings > Variants star-map flow card, including full-screen and a mobile width.
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
# The flow card lives on Settings > Variants. The default landing page is Simulate.
ev("document.querySelector(\"a[data-value='Settings']\").click()"); Sys.sleep(2)
wait_for("!!document.querySelector('.sc-flow-svg')")
Sys.sleep(1)
ev("window.scrollTo(0,0)"); Sys.sleep(0.5)
shot("40_settings_flow", FALSE)
# Full-screen the flow card, then close it.
ev("document.querySelector('.card .bslib-full-screen-enter').click()"); Sys.sleep(2)
shot("41_settings_flow_full_screen", FALSE)
ev("document.querySelector('.bslib-full-screen-exit, .card[data-full-screen=\"true\"] .bslib-full-screen-enter') && document.querySelector('.bslib-full-screen-exit, .card[data-full-screen=\"true\"] .bslib-full-screen-enter').click()"); Sys.sleep(1)
cat("flow svg present:", ev("!!document.querySelector('.sc-flow-svg')"),
    "scrap marker:", ev("document.getElementById('sc-flow-scrap') && document.getElementById('sc-flow-scrap').getAttribute('marker-end')"),
    "\n")
