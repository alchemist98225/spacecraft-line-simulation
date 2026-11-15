# screenshots_rc_reshoot.R
# Re-shoot Results cycle/labor and Compare after plot fixes.
# Usage: Rscript scripts/screenshots_rc_reshoot.R <port>
suppressPackageStartupMessages({ library(chromote); library(jsonlite) })
a <- commandArgs(TRUE); port <- a[[1]]
out_dir <- "shots"; dir.create(out_dir, FALSE, TRUE)
b <- ChromoteSession$new(); b$Page$enable(); b$Runtime$enable()
b$default_timeout <- 300
b$Emulation$setDeviceMetricsOverride(width = 1440L, height = 1600L, deviceScaleFactor = 1, mobile = FALSE)
ev <- function(code) tryCatch(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value, error = function(e) e$message)
wait_for <- function(js, secs = 180) {
  for (i in seq_len(secs * 2)) { if (isTRUE(ev(js))) return(TRUE); Sys.sleep(0.5) }
  FALSE
}
shot <- function(name, full = FALSE) {
  path <- file.path(out_dir, paste0("rc_", name, ".png"))
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png", captureBeyondViewport = full)$data), path)
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
picked_ids <- function() ev("Array.from(document.querySelectorAll('.sc-compare-picked')).map(e => e.getAttribute('data-id')).filter(Boolean)")
axis_or_legend <- function(plot_id) {
  ev(sprintf("(() => {
    const root = document.getElementById('%s');
    if (!root) return 'no-root';
    const err = (root.innerText || '').match(/Error:.*/);
    const ticks = Array.from(root.querySelectorAll('.xtick text, g.xtick text')).map(t => (t.textContent||'').trim()).filter(Boolean);
    const legend = Array.from(root.querySelectorAll('.legendtext, .legend .traces text')).map(t => (t.textContent||'').trim()).filter(Boolean);
    return JSON.stringify({ticks: ticks, legend: legend, err: err ? err[0] : null, hasPlot: !!root.querySelector('.js-plotly-plot, .plotly')});
  })()", plot_id))
}

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
stopifnot(wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 180))
Sys.sleep(1.0)

# Results: capacity banner + cycle / labor plots
stopifnot(isTRUE(click_nav("results_capacity")))
Sys.sleep(1.8)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("05_results_capacity", TRUE)
for (spec in list(
  list("results_capacity", "results_capacity_tabs", "Utilization", "05b_results_util"),
  list("results_capacity", "results_capacity_tabs", "Downtime", "05c_results_down"),
  list("results_capacity", "results_capacity_tabs", "Why stations wait", "06_results_waits"),
  list("results_cycle", "results_cycle_tabs", "All shipped units", "09b_results_cycle_hist"),
  list("results_cycle", "results_cycle_tabs", "By variant", "09c_results_cycle_dist"),
  list("results_cycle", "results_cycle_tabs", "Per-step breakdown", "10_results_cycle_steps"),
  list("results_cycle", "results_cycle_tabs", "Over time", "11_results_cycle_time"),
  list("results_quality", "results_quality_tabs", "Rework", "12_results_quality_rework"),
  list("results_reliability", "results_reliability_tabs", "MTBF", "13_results_reliability_mtbf"),
  list("results_summary", "results_summary_tabs", "Labor", "15_results_labor")
)) {
  if (!is.null(spec[[1]])) {
    stopifnot(isTRUE(click_nav(spec[[1]])))
    Sys.sleep(1.0)
  }
  stopifnot(isTRUE(click_tab(spec[[2]], spec[[3]])))
  Sys.sleep(1.3)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[2]]))
  Sys.sleep(0.3)
  shot(spec[[4]])
}

# Compare
stopifnot(isTRUE(click_nav("Compare")))
stopifnot(wait_for("!!document.querySelector('.sc-compare-pick')", 60))
Sys.sleep(2.0)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("17_compare_table", TRUE)
before_ids <- picked_ids()
cat("picked_before=", paste(unlist(before_ids), collapse = ","), "\n", sep = "")

stopifnot(isTRUE(click_tab("compare_spread_tabs", "Cycle time")))
stopifnot(wait_for("!!document.querySelector('#compare_plot_cycle .js-plotly-plot, #compare_plot_cycle .plotly')", 30))
Sys.sleep(1.2)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
cat("labels_before=", axis_or_legend("compare_plot_cycle"), "\n", sep = "")
shot("18_compare_cycle_default")

tog <- ev("(() => { const el = document.querySelector('.sc-compare-pick[data-id=\"faster_qa\"]'); if (!el) return 'missing'; el.click(); return el.getAttribute('data-id'); })()")
cat("clicked_card=", tog, "\n", sep = "")
Sys.sleep(5)
ok <- wait_for("(() => {
  const root = document.getElementById('compare_plot_cycle');
  if (!root) return false;
  if ((root.innerText || '').includes('subscript')) return false;
  const ticks = Array.from(root.querySelectorAll('.xtick text, g.xtick text')).map(t => (t.textContent||'').trim());
  return ticks.some(t => t.includes('Faster') || t.includes('holds') || t.includes('QA') || t.includes('qa'));
})()", 20)
cat("toggled_plot_ready=", ok, " after=", paste(unlist(picked_ids()), collapse = ","), "\n", sep = "")
Sys.sleep(0.8)
cat("labels_after=", axis_or_legend("compare_plot_cycle"), "\n", sep = "")
shot("19_compare_cycle_toggled")

# Crosstalk: click a jitter marker via CDP mouse events at its bounding box.
click_res <- ev("(() => {
  const plot = document.querySelector('#compare_plot_cycle .js-plotly-plot') || document.querySelector('#compare_plot_cycle .plotly');
  if (!plot) return {ok:false, reason:'no plot', html: (document.getElementById('compare_plot_cycle')||{}).innerHTML?.slice(0,200)};
  const paths = Array.from(plot.querySelectorAll('.scatterlayer .trace .points path'));
  if (!paths.length) return {ok:false, reason:'no paths', nTrace:(plot.data||[]).length, modes:(plot.data||[]).map(d => (d.mode||'')+'/'+((d.x||[]).length))};
  const path = paths[Math.floor(paths.length/3)] || paths[0];
  const r = path.getBoundingClientRect();
  return {ok:true, n: paths.length, x: r.x + r.width/2, y: r.y + r.height/2, w:r.width, h:r.height};
})()")
cat("crosstalk_target=", jsonlite::toJSON(click_res, auto_unbox = TRUE), "\n", sep = "")
if (is.list(click_res) && isTRUE(click_res$ok)) {
  x <- as.numeric(click_res$x); y <- as.numeric(click_res$y)
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
  Sys.sleep(0.1)
  b$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "left", clickCount = 1)
  Sys.sleep(0.05)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = "left", clickCount = 1)
  Sys.sleep(1.0)
}
hl <- ev("(() => {
  const plot = document.querySelector('#compare_plot_cycle .js-plotly-plot') || document.querySelector('#compare_plot_cycle .plotly');
  if (!plot) return {ok:false};
  const fills = [], opacs = [];
  plot.querySelectorAll('.scatterlayer .trace .points path').forEach(p => {
    const st = window.getComputedStyle(p);
    fills.push(st.fill || p.getAttribute('fill') || '');
    opacs.push(parseFloat(st.opacity || '1'));
  });
  const selected = (plot._fullData || plot.data || []).map(d => d.selectedpoints || null);
  return {traces:(plot.data||[]).length, nPoints:fills.length, dimmed:opacs.filter(o => o < 0.5).length,
          cyan: fills.filter(f => /17becf|23\\s*,?\\s*190/i.test(f)).length, selected:selected, opacs:opacs.slice(0,8)};
})()")
cat("crosstalk_cycle=", jsonlite::toJSON(hl, auto_unbox = TRUE), "\n", sep = "")
shot("20_compare_crosstalk_cycle")

stopifnot(isTRUE(click_tab("compare_spread_tabs", "Throughput")))
Sys.sleep(1.8)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
hl2 <- ev("(() => {
  const plot = document.querySelector('#compare_plot_throughput .js-plotly-plot') || document.querySelector('#compare_plot_throughput .plotly');
  if (!plot) return {ok:false};
  const fills = [], opacs = [];
  plot.querySelectorAll('.scatterlayer .trace .points path').forEach(p => {
    const st = window.getComputedStyle(p);
    fills.push(st.fill || p.getAttribute('fill') || '');
    opacs.push(parseFloat(st.opacity || '1'));
  });
  const selected = (plot._fullData || plot.data || []).map(d => d.selectedpoints || null);
  return {traces:(plot.data||[]).length, nPoints:fills.length, dimmed:opacs.filter(o => o < 0.5).length,
          cyan: fills.filter(f => /17becf|23\\s*,?\\s*190/i.test(f)).length, selected:selected};
})()")
cat("crosstalk_throughput=", jsonlite::toJSON(hl2, auto_unbox = TRUE), "\n", sep = "")
shot("21_compare_crosstalk_throughput")

for (spec in list(
  list("Cycle time", "14b_compare_cycle"),
  list("Touch labor", "22_compare_hpu"),
  list("Downtime", "26_compare_downtime"),
  list("Flow", "25_compare_flow"),
  list("Downloads", "27_compare_downloads")
)) {
  stopifnot(isTRUE(click_tab("compare_spread_tabs", spec[[1]])))
  Sys.sleep(if (identical(spec[[1]], "Flow")) 3.0 else 1.3)
  ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
  if (identical(spec[[1]], "Flow")) ev("document.querySelector('#compare_plot_wip_flow')?.scrollIntoView({block:'start'})")
  Sys.sleep(0.5)
  shot(spec[[2]], full = identical(spec[[1]], "Flow"))
}

try(b$close(), silent = TRUE)
cat("RC_RESHOOT_DONE\n")
