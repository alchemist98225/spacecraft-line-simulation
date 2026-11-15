# Headless shots of Simulate, Results and Compare at 1024px and 400px, plus a
# 1440px check that sidebars stay open and tab strips stay tabs.
# Usage: Rscript scripts/screenshots_responsive.R <port> [out_dir]
# Writes <out_dir>/resp_*.png
suppressPackageStartupMessages({ library(chromote); library(jsonlite) })
a <- commandArgs(TRUE)
port <- a[[1]]
out_dir <- if (length(a) > 1) a[[2]] else "shots"
dir.create(out_dir, FALSE, TRUE)
b <- ChromoteSession$new(); b$Page$enable(); b$Runtime$enable()
b$default_timeout <- 300

ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value,
                              error = function(e) NULL)
wait_for <- function(js, secs = 180) {
  for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }
  FALSE
}
set_width <- function(w, h = 1100L) {
  b$Emulation$setDeviceMetricsOverride(width = as.integer(w), height = as.integer(h),
                                       deviceScaleFactor = 1, mobile = FALSE)
  ev("window.dispatchEvent(new Event('resize')); if (window.scApplyResponsiveLayout) scApplyResponsiveLayout(true);")
  Sys.sleep(0.8)
}
shot <- function(name, full = FALSE) {
  path <- file.path(out_dir, paste0("resp_", name, ".png"))
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

report_layout <- function(label) {
  info <- ev("(() => {
    const w = window.innerWidth;
    const sb = document.querySelector('.bslib-sidebar-layout');
    const collapsed = !!(sb && sb.classList.contains('sidebar-collapsed'));
    const toggle = document.querySelector('.bslib-sidebar-layout > .collapse-toggle');
    const toggleVis = !!(toggle && toggle.offsetParent !== null);
    const sel = document.querySelector('.sc-tab-select-wrap');
    const selVis = !!(sel && sel.offsetParent !== null && getComputedStyle(sel).display !== 'none');
    const tabs = document.querySelector('.card .nav-tabs, .card .nav-underline');
    let tabsVis = false;
    if (tabs) {
      const cs = getComputedStyle(tabs);
      tabsVis = cs.display !== 'none' && tabs.offsetParent !== null && tabs.offsetHeight > 2;
    }
    const size = sb ? getComputedStyle(sb).getPropertyValue('--bslib-sidebar-js-window-size').trim() : '';
    return {w, collapsed, toggleVis, selVis, tabsVis, size};
  })()")
  cat(sprintf("layout %s w=%s collapsed=%s toggle=%s select=%s tabs=%s size=%s\n",
              label,
              info$w %||% "?",
              info$collapsed %||% "?",
              info$toggleVis %||% "?",
              info$selVis %||% "?",
              info$tabsVis %||% "?",
              info$size %||% "?"), sep = "")
  invisible(info)
}
`%||%` <- function(a, b) if (is.null(a) || (is.atomic(a) && length(a) == 1 && is.na(a))) b else a

set_width(1440L, 1600L)
invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
ok <- wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 240)
cat("simulate_ready=", ok, "\n", sep = "")
if (!isTRUE(ok)) stop("Simulate did not become ready")
Sys.sleep(1.0)

# --- 1440: sidebars open, tabs stay tabs ---
info <- report_layout("1440_simulate")
# On a wide screen the sidebar should be open and tab selects hidden.
# (Simulate itself has no card tabs; check after Compare / Results.)
shot("1440_simulate")

ok <- isTRUE(click_nav("results_variant"))
cat("nav_results_variant_1440=", ok, "\n", sep = "")
ok <- wait_for("!!document.getElementById('results_variant_tabs')", 60)
Sys.sleep(1.0)
info <- report_layout("1440_results")
if (isTRUE(info$selVis)) warning("tab select unexpectedly visible at 1440")
if (isTRUE(info$collapsed)) warning("sidebar unexpectedly collapsed at 1440 (Results has no sidebar)")
shot("1440_results")

ok <- isTRUE(click_nav("Compare"))
cat("nav_compare_1440=", ok, "\n", sep = "")
ok <- wait_for("!!document.querySelector('#compare_spread_tabs')", 90)
Sys.sleep(1.5)
info <- report_layout("1440_compare")
if (isTRUE(info$selVis)) warning("tab select unexpectedly visible at 1440 Compare")
if (isTRUE(info$collapsed)) warning("Compare sidebar unexpectedly collapsed at 1440")
shot("1440_compare")

visit <- function(width, page, nav, ready_js, wait_s = 1.2, scroll_js = NULL) {
  set_width(width, if (width <= 500) 900L else 1100L)
  ok <- isTRUE(click_nav(nav))
  cat(sprintf("nav_%s_%s=%s\n", page, width, ok))
  ok <- wait_for(ready_js, 90)
  cat(sprintf("ready_%s_%s=%s\n", page, width, ok))
  Sys.sleep(wait_s)
  if (!is.null(scroll_js)) ev(scroll_js) else ev("window.scrollTo(0,0)")
  Sys.sleep(0.4)
  ev("if (window.scApplyResponsiveLayout) scApplyResponsiveLayout(true);")
  Sys.sleep(0.4)
  if (!is.null(scroll_js)) ev(scroll_js)
  Sys.sleep(0.3)
  info <- report_layout(sprintf("%s_%s", width, page))
  shot(sprintf("%s_%s", width, page), full = identical(page, "compare"))
  info
}

for (w in c(1024L, 400L)) {
  visit(w, "simulate", "Simulate",
        "(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 1.0)
  visit(w, "results", "results_variant",
        "!!document.getElementById('results_variant_tabs')", 1.2)
  visit(w, "compare", "Compare",
        "!!document.querySelector('#compare_spread_tabs')", 1.6,
        "document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
}

# At 1024, open the Compare controls so the toggle and an open sidebar are both documented.
set_width(1024L, 1100L)
invisible(click_nav("Compare"))
Sys.sleep(1.0)
ev("(() => {
  const el = Array.from(document.querySelectorAll('.bslib-sidebar-layout')).find(x => x.offsetParent);
  const tog = el && el.querySelector(':scope > .collapse-toggle');
  if (tog) { tog.click(); return true; }
  return false;
})()")
Sys.sleep(0.8)
report_layout("1024_compare_open")
shot("1024_compare_controls_open")

try(b$close(), silent = TRUE)
cat("RESPONSIVE_SHOTS_DONE\n")
