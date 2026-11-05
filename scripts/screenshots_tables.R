# screenshots_tables.R
# Headless shots of the Simulate KPI cards and the three tables with pictures.
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
ev("window.scrollTo(0,0)"); Sys.sleep(1); shot("30_simulate_kpi_cards", FALSE)
for (id in c("table_cycle", "table_lateness", "table_reps")) {
  ev(sprintf("document.getElementById('%s').closest('.card').scrollIntoView({block:'start'})", id)); Sys.sleep(1.5)
  shot(paste0("3", match(id, c("table_cycle", "table_lateness", "table_reps")), "_", id), FALSE)
}
