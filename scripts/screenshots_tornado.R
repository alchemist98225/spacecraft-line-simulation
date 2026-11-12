# screenshots_tornado.R
# Headless shots of the Tornado page at the requested width.
# Usage: Rscript scripts/screenshots_tornado.R <port> [width] [prefix]
# Writes shots/<prefix>NN_page.png. Width defaults to 1400 (1440 ~ a laptop).
suppressPackageStartupMessages({ library(chromote) })
a <- commandArgs(TRUE); port <- a[[1]]
width <- if (length(a) > 1) as.integer(a[[2]]) else 1400L
prefix <- if (length(a) > 2) a[[3]] else ""
b <- ChromoteSession$new(); b$Page$enable()
b$Emulation$setDeviceMetricsOverride(width = width, height = 1600L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value, error = function(e) NULL)
shot <- function(n, full = TRUE) writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png", captureBeyondViewport = full)$data), file.path("shots", paste0(prefix, n, ".png")))
wait_for <- function(js, secs = 180) { for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }; FALSE }
invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')")
Sys.sleep(4)
ev("document.querySelector(\"a[data-value='Tornado']\").click()"); Sys.sleep(3)
wait_for("!!document.querySelector('#tornado_table table, #tornado_table .gt_table')")
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.5)
shot("70_tornado_top", FALSE)
ev("(document.getElementById('tornado_plot')||{scrollIntoView:()=>{}}).scrollIntoView({block:'start'})"); Sys.sleep(1)
shot("71_tornado_chart", FALSE)
ev("(document.getElementById('tornado_table')||{scrollIntoView:()=>{}}).scrollIntoView({block:'start'})"); Sys.sleep(1)
shot("72_tornado_table", FALSE)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("73_tornado_full", TRUE)
st <- ev("(document.getElementById('tornado_status')||{}).innerText")
story <- ev("(document.getElementById('tornado_story')||{}).innerText")
cat("tornado status:", if (is.null(st) || !nzchar(as.character(st))) "" else st, "\n")
cat("tornado story:", if (is.null(story) || !nzchar(as.character(story))) "" else story, "\n")
