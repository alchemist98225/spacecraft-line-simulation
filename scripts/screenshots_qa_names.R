# Headless shots for the qa-names round: Simulate flow, Settings flow, and
# the Settings hold-time inputs (Records, Test data, Buy-off).
# Usage: Rscript scripts/screenshots_qa_names.R <port> [out_dir]
# Writes <out_dir>/qa_names_*.png
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
  path <- file.path(out_dir, paste0("qa_names_", name, ".png"))
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

# Simulate flow chart
ok <- wait_for("!!document.querySelector('.sc-flow-svg')", 30)
cat("simulate_flow_ready=", ok,
    " records=", ev("!!document.getElementById('sc-flow-qa-int')"),
    " scrap_node=", ev("!!document.getElementById('sc-flow-scrap-node')"), "\n", sep = "")
ev("document.querySelector('.sc-flow-wrap, .sc-flow-svg')?.scrollIntoView({block:'start'})")
Sys.sleep(0.6)
shot("01_simulate_flow")

# Settings flow (Variants card)
stopifnot(isTRUE(click_nav("Settings")))
ok <- wait_for("!!document.querySelector('#settings_tabs .sc-flow-svg, .sc-flow-svg')", 30)
cat("settings_flow_ready=", ok, "\n", sep = "")
Sys.sleep(1.0)
ev("document.querySelector('.sc-flow-wrap, .sc-flow-svg')?.scrollIntoView({block:'start'})")
Sys.sleep(0.6)
shot("02_settings_flow")
# Full-screen if the expand control is there
ev("document.querySelector('.card .bslib-full-screen-enter')?.click()")
Sys.sleep(1.2)
shot("02b_settings_flow_full")
ev("document.querySelector('.bslib-full-screen-exit, .card[data-full-screen=\"true\"] .bslib-full-screen-enter')?.click()")
Sys.sleep(0.6)

# Settings > Line settings hold inputs
ok <- isTRUE(click_tab("settings_tabs", "Line settings"))
cat("line_settings_tab=", ok, "\n", sep = "")
ok <- wait_for("!!document.getElementById('sc-line-qa')", 30)
cat("qa_panel_ready=", ok, "\n", sep = "")
ev("document.getElementById('sc-line-qa')?.scrollIntoView({block:'start'})")
Sys.sleep(0.8)
shot("03_settings_holds")
# A slightly taller crop of the whole line-settings column so the six fields are visible
ev("document.getElementById('sc-line-qa')?.scrollIntoView({block:'center'})")
Sys.sleep(0.4)
shot("03b_settings_holds_centered", full = TRUE)

# Navbar (Results dropdown open so By variant is visible)
ev("window.scrollTo(0,0)")
Sys.sleep(0.3)
ok <- ev("(() => {
  const tog = Array.from(document.querySelectorAll('.navbar .dropdown-toggle, .navbar [data-bs-toggle=\"dropdown\"]'))
    .find(el => (el.innerText || el.textContent || '').includes('Results'));
  if (tog) { tog.click(); return true; }
  return false;
})()")
cat("results_dropdown_open=", ok, "\n", sep = "")
Sys.sleep(0.6)
shot("04_navbar_results")

# Results > By variant needs a finished Simulate run (default page already ran on load)
ok <- isTRUE(click_nav("results_variant"))
cat("nav_results_variant=", ok, "\n", sep = "")
ok <- wait_for("!!document.getElementById('results_variant_tabs')", 60)
cat("variant_ready=", ok, "\n", sep = "")
Sys.sleep(1.2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("05_results_variant", TRUE)
invisible(click_tab("results_variant_tabs", "Cycle time"))
Sys.sleep(1.0)
ev("document.querySelector('#results_variant_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("05b_results_variant_cycle")
invisible(click_tab("results_variant_tabs", "Touch labor"))
Sys.sleep(1.0)
ev("document.querySelector('#results_variant_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("05c_results_variant_hpu")

# Results > Summary > Throughput bars
ok <- isTRUE(click_nav("results_summary"))
cat("nav_results_summary=", ok, "\n", sep = "")
Sys.sleep(1.0)
ok <- isTRUE(click_tab("results_summary_tabs", "Throughput"))
cat("tab_throughput=", ok, "\n", sep = "")
Sys.sleep(1.2)
ev("document.querySelector('#results_summary_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("06_results_throughput")

# Compare throughput bars (preload). Toggle the sidebar once so the remount is visible.
ok <- isTRUE(click_nav("Compare"))
cat("nav_compare=", ok, "\n", sep = "")
ok <- wait_for("!!document.getElementById('compare_plot_throughput') || !!document.querySelector('#compare_spread_tabs')", 90)
cat("compare_ready=", ok, "\n", sep = "")
Sys.sleep(1.5)
invisible(click_tab("compare_spread_tabs", "Throughput"))
Sys.sleep(1.5)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("07_compare_throughput")
ok <- ev("(() => {
  const boxes = Array.from(document.querySelectorAll('#compare_scenario_list input[type=checkbox], .sc-compare-scenario input[type=checkbox]'));
  const off = boxes.find(b => !b.checked);
  if (off) { off.click(); return off.id || off.value || true; }
  const labels = Array.from(document.querySelectorAll('#compare_scenario_list .form-check-label, #compare_scenario_list label'));
  const hit = labels.find(el => (el.innerText || '').includes('QA inspector') || (el.innerText || '').includes('Faster'));
  if (hit) { hit.click(); return hit.innerText; }
  return false;
})()")
cat("compare_toggle=", ok, "\n", sep = "")
Sys.sleep(5.0)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.4)
shot("07b_compare_throughput_toggled")

try(b$close(), silent = TRUE)
cat("QA_NAMES_SHOTS_DONE\n")
