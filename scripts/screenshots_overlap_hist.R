# Headless shots of the four overlapping histograms: Compare cycle time and
# touch labor (plotly), Analyze cycle time and touch labor (ggplot).
# Usage: Rscript scripts/screenshots_overlap_hist.R <port> [out_dir]
# Writes <out_dir>/overlap_*.png
suppressPackageStartupMessages({ library(chromote); library(jsonlite) })
a <- commandArgs(TRUE)
port <- a[[1]]
out_dir <- if (length(a) > 1) a[[2]] else "shots"
dir.create(out_dir, FALSE, TRUE)
b <- ChromoteSession$new(); b$Page$enable(); b$Runtime$enable()
b$default_timeout <- 300
b$Emulation$setDeviceMetricsOverride(width = 1440L, height = 1600L,
                                     deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value,
                              error = function(e) NULL)
wait_for <- function(js, secs = 180) {
  for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }
  FALSE
}
shot <- function(name, full = FALSE) {
  path <- file.path(out_dir, paste0("overlap_", name, ".png"))
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png",
                                                          captureBeyondViewport = full)$data),
           path)
  cat("shot", path, "\n")
  path
}
click_nav <- function(value) {
  ev(sprintf("(() => {
    const a = document.querySelector(\"a.nav-link[data-value='%s'], a[data-value='%s']\");
    if (!a) return false;
    const dd = a.closest('.dropdown-menu');
    if (dd) {
      const tog = dd.parentElement && dd.parentElement.querySelector('.dropdown-toggle, [data-bs-toggle=\"dropdown\"]');
      if (tog) tog.click();
    }
    a.click();
    return true;
  })()", value, value))
}
click_tab <- function(container, value) {
  ev(sprintf("(() => { const a = document.querySelector(\"#%s a[data-value='%s']\"); if (!a) return false; a.click(); return true; })()", container, value))
}

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
ok <- wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 240)
cat("simulate_ready=", ok, "\n", sep = "")
if (!isTRUE(ok)) stop("Simulate did not become ready")
Sys.sleep(1.0)

stopifnot(isTRUE(click_nav("Compare")))
ok <- wait_for("!!document.querySelector('.sc-compare-pick') && !!((document.getElementById('compare_status')||{}).innerText)", 60)
cat("compare_ready=", ok, "\n", sep = "")
Sys.sleep(1.5)

for (spec in list(
  list("Cycle time", "#compare_plot_cycle .js-plotly-plot, #compare_plot_cycle .plotly", "01_compare_cycle"),
  list("Touch labor", "#compare_plot_hpu .js-plotly-plot, #compare_plot_hpu .plotly", "02_compare_hpu")
)) {
  stopifnot(isTRUE(click_tab("compare_spread_tabs", spec[[1]])))
  ok <- wait_for(sprintf("!!document.querySelector('%s')", spec[[2]]), 30)
  cat(sprintf("compare_%s ready=%s\n", spec[[1]], ok))
  ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
  Sys.sleep(1.3)
  shot(spec[[3]])
}

stopifnot(isTRUE(click_nav("Analyze")))
ok <- wait_for("!!((document.getElementById('analyze_story')||{}).innerText)", 60)
cat("analyze_ready=", ok, "\n", sep = "")
Sys.sleep(1.2)

for (spec in list(
  list("Cycle time", "03_analyze_cycle"),
  list("Touch labor", "04_analyze_hpu")
)) {
  stopifnot(isTRUE(click_tab("analyze_result_tabs", spec[[1]])))
  ev("document.querySelector('#analyze_result_tabs')?.scrollIntoView({block:'start'})")
  Sys.sleep(1.3)
  shot(spec[[2]])
}

try(b$close(), silent = TRUE)
cat("OVERLAP_SHOTS_DONE\n")
