# screenshots_compare.R
# Headless shots of the Compare page at the requested width.
# Usage: Rscript scripts/screenshots.R <port> [width] [prefix]
# Writes shots/<prefix>NN_page.png. Width defaults to 1400 (1440 ~ a laptop).
suppressPackageStartupMessages({ library(chromote) })
a <- commandArgs(TRUE); port <- a[[1]]
width <- if (length(a) > 1) as.integer(a[[2]]) else 1400L
prefix <- if (length(a) > 2) a[[3]] else ""
b <- ChromoteSession$new(); b$Page$enable()
b$Emulation$setDeviceMetricsOverride(width = width, height = 1200L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value, error = function(e) NULL)
shot <- function(n, full = TRUE) writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png", captureBeyondViewport = full)$data), file.path("shots", paste0(prefix, n, ".png")))
wait_for <- function(js, secs = 180) { for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }; FALSE }
invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')")
Sys.sleep(4); cat("kpis:", ev("['kpi_cycle','kpi_ontime','kpi_wip','kpi_chamber','kpi_mrb','kpi_scrap'].map(i=>document.getElementById(i).innerText).join(' | ')"), "\n")
ev("document.querySelector(\"a[data-value='Compare']\").click()"); Sys.sleep(3)
wait_for("!!document.querySelector('#compare_table table, #compare_table .gt_table')")
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.5)
shot("50_compare_top", FALSE)
ev("(document.getElementById('compare_table')||{scrollIntoView:()=>{}}).scrollIntoView({block:'start'})"); Sys.sleep(1)
shot("51_compare_table", FALSE)
ev("(document.getElementById('compare_plot')||{scrollIntoView:()=>{}}).scrollIntoView({block:'start'})"); Sys.sleep(1.5)
shot("52_compare_plot", FALSE)
st <- ev("(document.getElementById('compare_status')||{}).innerText")
cat("compare status:", if (is.null(st) || !nzchar(as.character(st))) "" else st, "
")
