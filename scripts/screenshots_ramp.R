# screenshots_ramp.R
# Headless shots of the Ramp tab (and the About mention) after the ramp work.
# Usage: Rscript scripts/screenshots_ramp.R <port>
# Writes shots/ramp_*.png
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
  path <- file.path(out_dir, paste0("ramp_", name, ".png"))
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
Sys.sleep(1)

stopifnot(isTRUE(click_nav("Ramp")))
ok <- wait_for("!!((document.getElementById('ramp_story')||{}).innerText)", 60)
cat("ramp_story_ready=", ok, "\n", sep = "")
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("01_story")
ev("document.querySelector('#ramp_result_tabs').scrollIntoView({block:'start'})")
Sys.sleep(0.5)
shot("02_plan")
for (spec in list(
  list("Timeline", "03_timeline"),
  list("Throughput", "04_throughput"),
  list("Cycle time", "05_cycle"),
  list("Touch labor", "06_hpu"),
  list("Utilization", "07_util"),
  list("WIP", "08_wip"),
  list("If a move is delayed", "09_delay")
)) {
  stopifnot(isTRUE(click_tab("ramp_result_tabs", spec[[1]])))
  Sys.sleep(1.2)
  ev("document.querySelector('#ramp_result_tabs').scrollIntoView({block:'start'})")
  Sys.sleep(0.3)
  shot(spec[[2]])
}
# About should mention Ramp
stopifnot(isTRUE(click_nav("About")))
Sys.sleep(1.2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("10_about")
try(b$close(), silent = TRUE)
cat("RAMP_SHOTS_DONE\n")
