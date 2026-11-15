# screenshots_results_compare.R
# Headless shots of every tab after the Results menu + Compare picker work.
# Usage: Rscript scripts/screenshots_results_compare.R <port>
# Writes shots/rc_*.png
suppressPackageStartupMessages({ library(chromote); library(jsonlite) })
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
picked_ids <- function() {
  ev("Array.from(document.querySelectorAll('.sc-compare-picked')).map(e => e.getAttribute('data-id')).filter(Boolean)")
}
axis_or_legend <- function(plot_id) {
  ev(sprintf("(() => {
    const root = document.getElementById('%s');
    if (!root) return '';
    const ticks = Array.from(root.querySelectorAll('.xtick text, g.xtick text')).map(t => (t.textContent||'').trim()).filter(Boolean);
    const legend = Array.from(root.querySelectorAll('.legendtext, .legend .traces text')).map(t => (t.textContent||'').trim()).filter(Boolean);
    return JSON.stringify({ticks: ticks, legend: legend});
  })()", plot_id))
}

invisible(b$Page$navigate(sprintf("http://127.0.0.1:%s/", port)))
stopifnot(wait_for("(document.getElementById('kpi_cycle')||{}).innerText?.includes('h')", 180))
Sys.sleep(1.2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("01_simulate_kpis")
ev("document.querySelector('.sc-flow-wrap, #sc-flow-svg, .sc-flow-svg')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
shot("02_simulate_flow")

stopifnot(isTRUE(click_nav("Settings")))
Sys.sleep(1.4)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("03_settings_variants")
ev("(() => { const a = document.querySelector(\"a[data-value='Line settings']\"); if (a) a.click(); return !!a; })()")
Sys.sleep(1.0)
shot("04_settings_line", TRUE)

# Results dropdown pages
results_pages <- list(
  list("results_capacity", "05_results_capacity"),
  list("results_cycle", "09_results_cycle"),
  list("results_quality", "12q_results_quality"),
  list("results_reliability", "13r_results_reliability"),
  list("results_summary", "16_results_summary")
)
for (pg in results_pages) {
  ok <- isTRUE(click_nav(pg[[1]]))
  cat("nav ", pg[[1]], " ok=", ok, "\n", sep = "")
  Sys.sleep(1.6)
  ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
  shot(pg[[2]], TRUE)
}

# Inner Results tabs
stopifnot(isTRUE(click_nav("results_capacity")))
Sys.sleep(1.0)
for (spec in list(
  list("results_capacity_tabs", "Utilization", "05b_results_util"),
  list("results_capacity_tabs", "Why stations wait", "06_results_waits"),
  list("results_capacity_tabs", "Queue length", "07_results_queues"),
  list("results_capacity_tabs", "Downtime", "08_results_downtime")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.2)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
}

stopifnot(isTRUE(click_nav("results_cycle")))
Sys.sleep(1.0)
for (spec in list(
  list("results_cycle_tabs", "All shipped units", "09b_results_cycle_hist"),
  list("results_cycle_tabs", "By variant", "09c_results_cycle_dist"),
  list("results_cycle_tabs", "Per-step breakdown", "10_results_cycle_steps"),
  list("results_cycle_tabs", "Over time", "11_results_cycle_time")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.2)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
}

stopifnot(isTRUE(click_nav("results_quality")))
Sys.sleep(1.0)
for (spec in list(
  list("results_quality_tabs", "Rework", "12_results_quality_rework"),
  list("results_quality_tabs", "Scrap", "12b_results_quality_scrap"),
  list("results_quality_tabs", "Fails", "12c_results_quality_fail")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.2)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
}

stopifnot(isTRUE(click_nav("results_reliability")))
Sys.sleep(1.0)
for (spec in list(
  list("results_reliability_tabs", "MTBF", "13_results_reliability_mtbf"),
  list("results_reliability_tabs", "MTTR", "13b_results_reliability_mttr")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.2)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
}

stopifnot(isTRUE(click_nav("results_summary")))
Sys.sleep(1.0)
for (spec in list(
  list("results_summary_tabs", "WIP & flow", "14_results_wip"),
  list("results_summary_tabs", "Labor", "15_results_labor"),
  list("results_summary_tabs", "Downloads", "16_results_downloads")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.4)
  ev(sprintf("document.querySelector('#%s')?.scrollIntoView({block:'start'})", spec[[1]]))
  Sys.sleep(0.3)
  shot(spec[[3]])
}

# Compare
stopifnot(isTRUE(click_nav("Compare")))
ok <- wait_for("!!document.querySelector('.sc-compare-pick') && !!((document.getElementById('compare_status')||{}).innerText)", 60)
cat("compare_ready=", ok, "\n", sep = "")
Sys.sleep(2.0)
ev("window.scrollTo(0,0)"); Sys.sleep(0.4)
shot("17_compare_table", TRUE)
before_ids <- picked_ids()
cat("picked_before=", paste(unlist(before_ids), collapse = ","), "\n", sep = "")

stopifnot(isTRUE(click_tab("compare_spread_tabs", "Cycle time")))
Sys.sleep(2.0)
wait_for("!!document.querySelector('#compare_plot_cycle .js-plotly-plot')", 30)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.6)
labels_before <- axis_or_legend("compare_plot_cycle")
cat("labels_before=", labels_before, "\n", sep = "")
shot("18_compare_cycle_default")

# Toggle a third precomputed scenario into the charts (drops the oldest of the two).
tog <- ev("(() => {
  const el = document.querySelector('.sc-compare-pick[data-id=\"faster\"], .sc-compare-pick[data-id=\"faster_qa\"]');
  if (!el) {
    const all = Array.from(document.querySelectorAll('.sc-compare-pick')).map(e => e.getAttribute('data-id'));
    return 'missing cards: ' + all.join(',');
  }
  el.click();
  return el.getAttribute('data-id');
})()")
cat("clicked_card=", tog, "\n", sep = "")
Sys.sleep(5)
after_ids <- picked_ids()
cat("picked_after=", paste(unlist(after_ids), collapse = ","), "\n", sep = "")
wait_for("!!document.querySelector('#compare_plot_cycle .js-plotly-plot')", 20)
Sys.sleep(0.8)
labels_after <- axis_or_legend("compare_plot_cycle")
cat("labels_after=", labels_after, "\n", sep = "")
shot("19_compare_cycle_toggled")
changed <- !identical(paste(unlist(before_ids), collapse = ","), paste(unlist(after_ids), collapse = ","))
cat("sidebar_changed=", changed, "\n", sep = "")

# Crosstalk: click a jitter point, then screenshot and inspect highlight.
# Stay on Cycle time (already open), click, then also open Throughput to see linkage.
click_pt <- ev("(() => {
  const plot = document.querySelector('#compare_plot_cycle .js-plotly-plot');
  if (!plot || !plot.data) return {ok: false, reason: 'no plot'};
  let curve = -1;
  const modes = plot.data.map(d => (d.mode || '') + '/' + ((d.x || []).length));
  for (let i = 0; i < plot.data.length; i++) {
    const d = plot.data[i];
    const n = (d.x || []).length;
    if (n >= 8 && String(d.mode || '').includes('markers')) { curve = i; break; }
  }
  if (curve < 0) return {ok: false, reason: 'no jitter', modes: modes};
  const key = (plot.data[curve].key || plot.data[curve].ids || plot.data[curve].customdata || [])[0];
  const evnt = {
    event: { button: 0 },
    points: [{
      curveNumber: curve,
      pointNumber: 0,
      data: plot.data[curve],
      x: plot.data[curve].x[0],
      y: plot.data[curve].y[0],
      key: key
    }]
  };
  if (typeof plot.emit === 'function') plot.emit('plotly_click', evnt);
  // Also try a DOM click on a marker path.
  const path = plot.querySelector('.scatterlayer .trace .points path, .scatterlayer path.point');
  if (path) {
    const r = path.getBoundingClientRect();
    const opts = { bubbles: true, cancelable: true, view: window, clientX: r.x + r.width / 2, clientY: r.y + r.height / 2 };
    path.dispatchEvent(new MouseEvent('mousedown', opts));
    path.dispatchEvent(new MouseEvent('mouseup', opts));
    path.dispatchEvent(new MouseEvent('click', opts));
  }
  return {ok: true, curve: curve, modes: modes, key: key, hadPath: !!path};
})()")
cat("crosstalk_click=", jsonlite::toJSON(click_pt, auto_unbox = TRUE), "\n", sep = "")
Sys.sleep(1.2)
hl <- ev("(() => {
  const plot = document.querySelector('#compare_plot_cycle .js-plotly-plot');
  if (!plot) return {ok: false};
  const fills = [], opacs = [];
  plot.querySelectorAll('.scatterlayer .trace .points path').forEach(p => {
    const st = window.getComputedStyle(p);
    fills.push(st.fill || p.getAttribute('fill') || '');
    opacs.push(parseFloat(st.opacity || '1'));
  });
  const selected = (plot._fullData || plot.data || []).map(d => d.selectedpoints || null);
  return {
    traces: (plot.data || []).length,
    nPoints: fills.length,
    dimmed: opacs.filter(o => o < 0.5).length,
    cyan: fills.filter(f => /17becf|23\\s*,?\\s*190\\s*,?\\s*207/i.test(f)).length,
    fills: fills.slice(0, 6),
    opacs: opacs.slice(0, 6),
    selected: selected
  };
})()")
cat("crosstalk_cycle=", jsonlite::toJSON(hl, auto_unbox = TRUE), "\n", sep = "")
shot("20_compare_crosstalk_cycle")

stopifnot(isTRUE(click_tab("compare_spread_tabs", "Throughput")))
Sys.sleep(1.8)
ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
hl2 <- ev("(() => {
  const plot = document.querySelector('#compare_plot_throughput .js-plotly-plot');
  if (!plot) return {ok: false};
  const fills = [], opacs = [];
  plot.querySelectorAll('.scatterlayer .trace .points path').forEach(p => {
    const st = window.getComputedStyle(p);
    fills.push(st.fill || p.getAttribute('fill') || '');
    opacs.push(parseFloat(st.opacity || '1'));
  });
  const selected = (plot._fullData || plot.data || []).map(d => d.selectedpoints || null);
  return {
    traces: (plot.data || []).length,
    nPoints: fills.length,
    dimmed: opacs.filter(o => o < 0.5).length,
    cyan: fills.filter(f => /17becf|23\\s*,?\\s*190\\s*,?\\s*207/i.test(f)).length,
    selected: selected
  };
})()")
cat("crosstalk_throughput=", jsonlite::toJSON(hl2, auto_unbox = TRUE), "\n", sep = "")
shot("21_compare_crosstalk_throughput")

for (spec in list(
  list("compare_spread_tabs", "Cycle time", "14b_compare_cycle"),
  list("compare_spread_tabs", "Touch labor", "22_compare_hpu"),
  list("compare_spread_tabs", "Downtime", "26_compare_downtime"),
  list("compare_spread_tabs", "Flow", "25_compare_flow"),
  list("compare_spread_tabs", "Downloads", "27_compare_downloads")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(if (identical(spec[[2]], "Flow")) 3.0 else 1.3)
  ev("document.querySelector('#compare_spread_tabs')?.scrollIntoView({block:'start'})")
  if (identical(spec[[2]], "Flow")) ev("document.querySelector('#compare_plot_wip_flow')?.scrollIntoView({block:'start'})")
  Sys.sleep(0.5)
  shot(spec[[3]], full = identical(spec[[2]], "Flow"))
}

# Remaining pages
stopifnot(isTRUE(click_nav("results_variant")))
ok <- wait_for("!!((document.getElementById('analyze_story')||{}).innerText)", 60)
cat("analyze_ready=", ok, "\n", sep = "")
Sys.sleep(1.4)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("28_analyze", TRUE)
for (spec in list(
  list("analyze_result_tabs", "Throughput", "28g_analyze_throughput"),
  list("analyze_result_tabs", "Cycle time", "28h_analyze_cycle"),
  list("analyze_result_tabs", "Touch labor", "28i_analyze_hpu"),
  list("analyze_result_tabs", "Downtime", "28e_analyze_downtime"),
  list("analyze_result_tabs", "Time Breakdown", "28b_analyze_breakdown"),
  list("analyze_result_tabs", "Bottleneck", "28c_analyze_bottleneck"),
  list("analyze_result_tabs", "Queue Length", "28f_analyze_queues"),
  list("analyze_result_tabs", "Why stations wait", "28d_analyze_waits")
)) {
  stopifnot(isTRUE(click_tab(spec[[1]], spec[[2]])))
  Sys.sleep(1.1)
  ev("document.querySelector('#analyze_result_tabs')?.scrollIntoView({block:'start'})")
  Sys.sleep(0.3)
  shot(spec[[3]])
}

stopifnot(isTRUE(click_nav("Tornado")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("29_tornado", TRUE)

stopifnot(isTRUE(click_nav("Breakpoint")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("30_breakpoint", TRUE)

stopifnot(isTRUE(click_nav("Optimize")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("31_optimize", TRUE)

stopifnot(isTRUE(click_nav("Warmup")))
Sys.sleep(2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("32_warmup", TRUE)

stopifnot(isTRUE(click_nav("Ramp")))
Sys.sleep(2)
ev("document.querySelector('#ramp_result_tabs')?.scrollIntoView({block:'start'})")
Sys.sleep(0.5)
shot("33_ramp", TRUE)

stopifnot(isTRUE(click_nav("About")))
Sys.sleep(1.2)
ev("window.scrollTo(0,0)"); Sys.sleep(0.3)
shot("34_about")

try(b$close(), silent = TRUE)
cat("RC_SHOTS_DONE\n")
