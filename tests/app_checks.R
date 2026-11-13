# app_checks.R
# Shiny smoke checks: every file parses, the input spec round-trips into a
# params list, and the server runs one simulation through testServer().
# Run from repo root: Rscript tests/app_checks.R
suppressPackageStartupMessages({ library(shiny); library(bslib); library(bsicons); library(tidyverse); library(simmer); library(DT) })
purrr::walk(list.files("app/R", full.names = TRUE), parse)
purrr::walk(list.files("app/R", full.names = TRUE), source)

# Every spec path points at a real number in the defaults.
spec <- sc_sync_fields()
stopifnot(!anyDuplicated(spec$id), all(purrr::map_lgl(spec$default, ~ length(.x) == 1)))
p <- sc_params_from_inputs(list(p_fail_func = 0.3, cap_chamber = 2, kit_p_late = 0))
stopifnot(p$p_fail_func == 0.3, p$capacity$chamber == 2, p$kit$p_late == 0)

testServer(sc_server, {
  session$setInputs(n_reps = 2, seed = 3, run = 1)
  r <- result()
  stopifnot(nrow(r$per_rep) == 2, is.finite(r$summary$cycle_mean_h))
})
message("app_checks OK")

# Settings page. Mock sessions ignore update*Input(), so the test passes an
# update_fn that records each copy; apply() plays them back the way a browser
# would, and repeats until nothing new is sent (the settle count).
make_bus <- function() {
  sent <- list()
  list(update_fn = function(session, type, id, value) sent[[length(sent) + 1]] <<- list(id = id, value = value),
       take = function() { out <- sent; sent <<- list(); out })
}
bus <- make_bus()
app <- NULL
srv <- function(input, output, session) app <<- sc_server(input, output, session, update_fn = bus$update_fn)
testServer(srv, {
  fields <- sc_sync_fields()
  start <- purrr::set_names(fields$default, fields$id)
  do.call(session$setInputs, c(start, purrr::set_names(fields$default, paste0("set_", fields$id))))
  apply <- function() {
    rounds <- 0L
    repeat {
      msgs <- bus$take()
      if (!length(msgs) || rounds > 10) break
      do.call(session$setInputs, purrr::set_names(purrr::map(msgs, "value"), purrr::map_chr(msgs, "id")))
      rounds <- rounds + 1L
    }
    rounds
  }
  stopifnot(nrow(app$line_tab$unsaved()) == 0)

  # Settings edit + Save -> Simulate sidebar.
  session$setInputs(set_p_fail_func = 0.25, set_cap_chamber = 2, set_chamber_restart = TRUE)
  stopifnot(nrow(app$line_tab$unsaved()) == 3, input$p_fail_func == sc_default_params()$p_fail_func)
  session$setInputs(settings_save = 1)
  r1 <- apply()
  stopifnot(input$p_fail_func == 0.25, input$cap_chamber == 2, isTRUE(input$chamber_restart),
            nrow(app$line_tab$unsaved()) == 0, app$current_params()$p_fail_func == 0.25,
            app$current_params()$capacity$chamber == 2, r1 <= 2)

  # Sidebar change -> Settings.
  session$setInputs(horizon_weeks = 2, n_reps = 3)
  r2 <- apply()
  stopifnot(input$set_horizon_weeks == 2, input$set_n_reps == 3, r2 <= 2, nrow(app$line_tab$unsaved()) == 0,
            app$current_params()$sim_hours == sc_horizon_hours(4, 2, 0),
            grepl(format(sc_horizon_hours(4, 2, 0), big.mark = ","), output$horizon_hours_text, fixed = TRUE),
            identical(output$horizon_hours_text, output$set_horizon_hours_text))

  # Horizon steppers through Save: Settings days -> sidebar -> params.
  session$setInputs(set_horizon_days = 3)
  session$setInputs(settings_save = 2)
  apply()
  stopifnot(input$horizon_days == 3, app$current_params()$sim_hours == sc_horizon_hours(4, 2, 3))

  # No loop: copies sent = 3 saved + 2 synced + 1 saved, nothing echoed back.
  stopifnot(app$line_tab$copies() == 6L)
  copies_before <- app$line_tab$copies()

  # Review board: Accepted + Scrap sync both ways; Rework is derived on each side.
  stopifnot(output$mrb_rework == "0.35", output$set_mrb_rework == "0.35",
            isTRUE(all.equal(app$current_params()$mrb$p_rework, 0.35)))
  session$setInputs(set_mrb_p_use_as_is = 0.8, set_mrb_p_scrap = 0.2)
  stopifnot(output$set_mrb_rework == "0.00", output$mrb_rework == "0.35",
            nrow(app$line_tab$unsaved()) == 2, !"mrb_rework" %in% app$line_tab$unsaved()$id)
  session$setInputs(settings_save = 3)
  apply()
  stopifnot(input$mrb_p_use_as_is == 0.8, input$mrb_p_scrap == 0.2, output$mrb_rework == "0.00",
            app$current_params()$mrb$p_rework == 0)
  session$setInputs(mrb_p_use_as_is = 0.6)
  apply()
  stopifnot(input$set_mrb_p_use_as_is == 0.6, output$set_mrb_rework == "0.20", output$mrb_rework == "0.20")
  # Over 1 is blocked with a message on both sides.
  session$setInputs(mrb_p_use_as_is = 0.9)
  apply()
  stopifnot(grepl("over 1", output$mrb_rework), grepl("over 1", output$set_mrb_rework))
  session$setInputs(run = 6)
  over <- tryCatch({ result(); FALSE }, shiny.silent.error = function(e) TRUE, error = function(e) TRUE)
  stopifnot(over)
  session$setInputs(mrb_p_use_as_is = 0.5, mrb_p_scrap = 0.15)
  apply()
  stopifnot(output$mrb_rework == "0.35", output$set_mrb_rework == "0.35")

  # A zero horizon blocks the run with a message.
  session$setInputs(horizon_quarters = 0, horizon_weeks = 0, horizon_days = 0)
  session$setInputs(run = 5)
  blocked <- tryCatch({ result(); FALSE }, shiny.silent.error = function(e) TRUE, error = function(e) TRUE)
  stopifnot(blocked, grepl("0 hours", output$horizon_hours_text))
  session$setInputs(horizon_quarters = 4, horizon_weeks = 2, horizon_days = 3)

  # Variant edits persist across the dropdown and reach the params.
  session$setInputs(variant_pick = "B")
  stopifnot(all(app$variants_tab$shown()$variant == "B"))
  session$setInputs(variants_table_cell_edit = list(row = 1, col = 0, value = "0.9"))
  session$setInputs(variant_pick = "A")
  stopifnot(all(app$variants_tab$shown()$variant == "A"))
  session$setInputs(variants_table_cell_edit = list(row = 2, col = 0, value = "21"))
  session$setInputs(variant_pick = "B")
  v <- app$current_params()$variants
  stopifnot(v$mix[v$variant == "B"] == 0.9, v$route[[1]]$mean_h[[1]] == 21,
            v$mix[v$variant == "A"] == sc_variants_default()$mix[[1]])
})
lv <- sc_variants_long(sc_variants_default())
labels <- sc_item_label(lv$item, lv$group)
stopifnot(all(labels != lv$item), !any(grepl("hours|coefficient|bays", labels, ignore.case = TRUE)),
          all(!is.na(sc_item_tip(lv$item, lv$group))),
          identical(unname(sc_group_labels[["env"]]), "Environmental test"),
          identical(sc_param_columns$name, c("Value", "Setting", "Applies to")))
a_labels <- labels[lv$variant == "A"]
stopifnot(identical(a_labels, c("Share of releases", "Structure", "Structure variation", "Harness", "Harness variation",
                                "Functional test", "Functional test variation", "Vibration", "Vibration variation",
                                "Thermal vacuum", "Thermal vacuum variation")))
# Horizon steppers: defaults give exactly one 52-week year; weeks add hours.
d <- sc_line_fields() %>% dplyr::filter(grepl("^horizon_", id), type == "numeric")
stopifnot(sc_horizon_hours(d$default[[1]], d$default[[2]], d$default[[3]]) == 8736,
          sc_horizon_hours(4, 0, 0) == sc_default_params()$sim_hours,
          sc_horizon_hours(4, 1, 0) - sc_horizon_hours(4, 0, 0) == 168,
          sc_horizon_hours(0, 0, 0) == 0, sc_horizon_label(8736) == "Horizon: 8,736 hours",
          sc_params_from_inputs(list(horizon_quarters = 1, horizon_weeks = 2, horizon_days = 1))$sim_hours == 2184 + 336 + 24)
# Kitting and Test failures labels, and the params each one sets.
lab <- sc_line_fields() %>% dplyr::filter(panel %in% c("kitting", "quality"))
stopifnot(identical(lab$label, c("Lead time (hours)", "Coefficient of variation", "Delay time (hours)", "Delay probability",
                                 "Functional failure rate", "Environmental failure rate", "Failures before review")),
          identical(purrr::map_chr(lab$path, ~ paste(.x, collapse = "$")),
                    c("kit$delay_mean_h", "kit$delay_cv", "kit$late_extra_mean_h", "kit$p_late",
                      "p_fail_func", "p_fail_env", "max_fails")),
          !any(grepl("0 to 1", sc_line_fields()$label)),
          all(lab$max[lab$id %in% c("kit_p_late", "p_fail_func", "p_fail_env")] == 1))
# Review board labels, order, and the derived rework share.
mrb <- sc_line_fields() %>% dplyr::filter(panel == "mrb")
stopifnot(identical(mrb$label, c("Review hold (hours)", "Coefficient of variation", "Accepted", "Scrap", "Rework")),
          identical(mrb$type[[5]], "derived"), !"mrb_p_rework" %in% sc_line_fields()$id,
          sc_rework_share(0.8, 0.2) == 0, isTRUE(all.equal(sc_rework_share(0.5, 0.15), 0.35)),
          isTRUE(all.equal(sc_params_from_inputs(list())$mrb$p_rework, sc_default_params()$mrb$p_rework)),
          sc_rework_label(0.8, 0.2) == "0.00", sc_rework_label(0.5, 0.15) == "0.35",
          grepl("over 1", sc_mrb_problem(0.7, 0.4)), !is.null(sc_mrb_problem(1.2, 0)),
          is.null(sc_mrb_problem(0.8, 0.2)), all(mrb$max[mrb$id %in% c("mrb_p_use_as_is", "mrb_p_scrap")] == 1))
# Final acceptance labels.
acc <- sc_line_fields() %>% dplyr::filter(panel == "acceptance")
stopifnot(identical(acc$label, c("Final acceptance (hours)", "Coefficient of variation")))

# Tables with pictures: default params and a 2-week horizon both build and render.
for (hrs in c(sc_default_params()$sim_hours, 14 * 24)) {
  p <- sc_default_params(); p$sim_hours <- hrs
  sm <- sc_summarise(sc_run(p, n_reps = 3, base_seed = 7), p)
  tabs <- list(sc_table_cycle(sm$units, p), sc_table_hpu(sm$units, p))
  html <- purrr::map_chr(tabs, ~ as.character(gt::as_raw_html(.x, inline_css = FALSE)))
  stopifnot(nrow(tabs[[1]][["_data"]]) == 3, nrow(tabs[[2]][["_data"]]) == 3,
            all(grepl("<img|<svg", html)), all(grepl("Variant", html)),
            grepl("Touch labor", html[[2]], ignore.case = TRUE),
            !grepl("vs_due", html[[2]]), !exists("sc_table_kpi_reps"))
  summ <- sc_format_summary_table(sm$summary)
  stopifnot(all(c("Average WIP", "Scrap Rate", "Units Shipped",
                  "Variant A Cycle Time", "Mean Touch Labor (HPU)",
                  "Labor Utilization", "Variant A Touch Labor (HPU)") %in% summ$Metric),
            !("On Time Percentage" %in% summ$Metric),
            !("Mean Schedule Slip" %in% summ$Metric),
            !any(grepl("_", summ$Metric)))
  cap <- sc_summary_caption(p)
  stopifnot(grepl("run horizon", cap), grepl("Arrivals", cap),
            grepl("Units Shipped", cap), grepl("Units Scrapped", cap),
            grepl("WIP at End", cap), grepl("Touch Labor", cap),
            !grepl("Schedule Slip", cap), !grepl("On Time", cap))
  # Counts on a default-horizon run should add up: released = shipped + scrapped + WIP at end.
  if (isTRUE(all.equal(p$sim_hours, 24 * 7 * 52))) {
    s <- sm$summary
    stopifnot(abs(s$arrivals - (s$shipped + s$scrapped + s$wip_end)) < 1e-6)
  }
  cat(sprintf("tables OK for %g h\n", hrs))
}
ui_sim <- as.character(sc_simulate_ui())
stopifnot(grepl("Cycle Times", ui_sim), grepl("Touch Labor", ui_sim),
          grepl("simulate_result_tabs", ui_sim), grepl("Summary", ui_sim),
          grepl("Throughput", ui_sim), grepl("Labor utilization", ui_sim),
          !grepl("Schedule Slip", ui_sim), !grepl("On time", ui_sim),
          !grepl("table_reps", ui_sim), !grepl("KPIs by replication", ui_sim))
# Run simulation starts collapsed (bslib accordion open = FALSE).
acc <- as.character(sc_sidebar_accordion())
stopifnot(grepl("sc_controls", acc), grepl("Run simulation", acc),
          !grepl("accordion-collapse collapse show", acc))
# Inline star-map flow diagram: routes, arrowheads, labels and colours.
flow <- as.character(sc_flow_svg())
stopifnot(grepl("<svg", flow), grepl("viewBox", flow))
for (lab in c("Kitting", "Integration", "View settings", "Functional test",
              "Environmental test", "Vibration", "Thermal vacuum",
              "Final acceptance", "+ ship", "Review", "Scrap",
              "Accepted", "Rework", "Fail")) {
  stopifnot(grepl(lab, flow, fixed = TRUE))
}
stopifnot(!grepl(">Forward<", flow), grepl("viewBox=\"0 0 1100 420\"", flow) || grepl("viewBox='0 0 1100 420'", flow) || grepl("0 0 1100 420", flow),
          grepl("sc-flow-hit", flow), grepl("scFlowGoToLine", as.character(sc_flow_ui())),
          grepl("kitting", flow), grepl("Open Line settings", flow))
path_tag <- function(html, id) {
  m <- regmatches(html, regexpr(sprintf('<path[^>]*id="%s"[^>]*>', id), html))
  stopifnot(length(m) == 1)
  m
}
# Scrap is a hollow circle on a dashed red line, and has no arrowhead anywhere.
stopifnot(!grepl("marker-end", path_tag(flow, "sc-flow-scrap")),
          grepl("#ab3134", path_tag(flow, "sc-flow-scrap")),
          grepl("id=\"sc-flow-scrap-node\"", flow))
# Functional-test fail and Rework have no arrowhead of their own; they share
# the one arrow into Integration. Review is not an arrow destination for rework.
for (id in c("sc-flow-fail-func", "sc-flow-rework")) {
  stopifnot(!grepl("marker-end", path_tag(flow, id)), grepl("#d2a44c", path_tag(flow, id)))
}
for (id in c("sc-flow-into-integration", "sc-flow-fail-env")) {
  stopifnot(grepl("sc-flow-arrow-fail", path_tag(flow, id)))
}
stopifnot(grepl("sc-flow-arrow-accepted", path_tag(flow, "sc-flow-accepted")),
          grepl("#30894b", path_tag(flow, "sc-flow-accepted")))
stopifnot(grepl("#2c7fb8", flow), grepl("#e0a032", flow),
          grepl('href="#sc-variant-settings"', flow))
ui <- as.character(sc_flow_ui())
stopifnot(grepl("sc-flow-svg", ui), grepl("failed a test twice", ui))
cat("flow diagram checks OK\n")

stopifnot(!anyDuplicated(sc_line_fields()$id), all(sc_line_fields()$panel %in% sc_line_panels$panel))
message("settings page checks OK")

# Compare page: preload loads, the table and plot build, and the UI exists.
preload <- sc_load_preload()
stopifnot(!is.null(preload), nrow(preload$table) == 20)
html <- as.character(gt::as_raw_html(sc_table_compare(preload$table), inline_css = FALSE))
stopifnot(grepl("Baseline", html), grepl("Rework surge", html) || grepl("Rework", html),
          grepl("Mean cycle time (h)", html, fixed = TRUE),
          grepl("Mean touch labor (HPU)", html, fixed = TRUE),
          grepl("Throughput (units)", html, fixed = TRUE),
          grepl("Labor utilization", html, fixed = TRUE),
          grepl("Average WIP (units)", html, fixed = TRUE),
          !grepl("On time", html, fixed = TRUE),
          !grepl("schedule slip", html, ignore.case = TRUE),
          grepl("quarter of the baseline", html),
          !grepl("\\([0-9.]+ to [0-9.]+\\)", html))
# Metric labels are not truncated.
stopifnot(!grepl("Average WIF", html), !grepl("Mean cycle t<", html))
marked <- sc_compare_mark_deltas(preload$table)
cyc <- marked %>% dplyr::filter(id == "lose_shift", col == "cycle_mean_h")
hpu <- marked %>% dplyr::filter(id == "rework", col == "labor_hpu")
stopifnot(isTRUE(cyc$notable[[1]]), isTRUE(cyc$improved[[1]] == FALSE),
          isTRUE(hpu$notable[[1]]), isTRUE(hpu$improved[[1]] == FALSE))
invisible(ggplot2::ggplot_build(sc_plot_compare(preload$table)))
invisible(ggplot2::ggplot_build(sc_plot_compare(preload$table, metric_id = "cycle")))
ui <- as.character(sc_compare_ui())
stopifnot(grepl("Compare", ui), grepl("Add from current Settings", ui), grepl("compare_cancel", ui),
          grepl("compare_spread_tabs", ui), grepl("Touch labor", ui),
          grepl("Labor utilization", ui), !grepl("Schedule slip", ui),
          grepl("compare_plot_cycle", ui))
changes <- sc_params_changes(sc_default_params(), { p <- sc_default_params(); p$capacity$chamber <- 2; p })
stopifnot(any(grepl("Environmental test chambers", changes)))
cat("compare page checks OK\n")

# Analyze page: preload loads, parts sum to cycle time, plots and table build.
ap <- sc_load_preload_analyze()
stopifnot(!is.null(ap), identical(ap$params, sc_default_params()),
          ap$meta$n_reps == 20L, is.character(ap$story), nchar(ap$story) > 40)
ov <- ap$breakdown %>% dplyr::filter(variant == "Overall")
stopifnot(abs(sum(ov$share) - 1) < 1e-8, abs(sum(ov$mean_h) - ov$cycle_h[[1]]) < 1e-6)
stopifnot(all(ap$resources$util >= -1e-9), all(ap$resources$util <= 1 + 1e-9),
          isTRUE(ap$bottleneck$resource == "chamber"))
html <- as.character(gt::as_raw_html(sc_table_analyze_breakdown(ap$breakdown), inline_css = FALSE))
stopifnot(grepl("Working", html), grepl("Waiting for a station", html),
          grepl("Waiting for technicians", html),
          grepl("Failed / rework", html), grepl("MRB hold", html),
          grepl("Kitting delay", html), grepl("Cycle time", html),
          grepl("Overall", html))
invisible(ggplot2::ggplot_build(sc_plot_analyze_breakdown(ap$breakdown)))
invisible(ggplot2::ggplot_build(sc_plot_analyze_util(ap$resources)))
invisible(ggplot2::ggplot_build(sc_plot_analyze_queues(ap$queues)))
ui <- as.character(sc_analyze_ui())
stopifnot(grepl("Analyze", ui), grepl("Analyze current Settings", ui),
          grepl("analyze_cancel", ui), grepl("What this says", ui),
          grepl("analyze_result_tabs", ui), grepl("Time Breakdown", ui),
          grepl("Queue Length", ui), grepl("Bottleneck", ui))
cat("analyze page checks OK\n")

# Tornado table: no repeated baseline column; the value is in the caption.
tp <- sc_load_preload_tornado()
th <- as.character(gt::as_raw_html(sc_table_tornado(tp, "cycle", narrow = FALSE), inline_css = FALSE))
stopifnot(grepl("Factor", th), grepl("Swing", th), grepl("The baseline on the unperturbed line is", th),
          !grepl(">Baseline<", th))

# Breakpoint page: preload loads, plots and tables build at both widths.
bp <- sc_load_preload_breakpoint()
stopifnot(!is.null(bp), bp$meta$n_reps == 20L, bp$meta$base_seed == 42L,
          any(bp$points$is_baseline),
          is.finite(bp$points$cycle_mean[bp$points$is_baseline]))
invisible(ggplot2::ggplot_build(sc_plot_break_throughput(bp, 1.5, 0.90, narrow = FALSE)))
invisible(ggplot2::ggplot_build(sc_plot_break_cycle(bp, 1.5, 0.90, narrow = TRUE)))
invisible(ggplot2::ggplot_build(sc_plot_break_util(bp, 1.5, 0.90, narrow = TRUE)))
tw <- sc_table_break(bp, cycle_mult = 1.5, sat_threshold = 0.90, narrow = FALSE)
tn <- sc_table_break(bp, cycle_mult = 1.5, sat_threshold = 0.90, narrow = TRUE)
hw <- as.character(gt::as_raw_html(tw, inline_css = FALSE))
hn <- as.character(gt::as_raw_html(tn, inline_css = FALSE))
stopifnot(grepl("Cycle time", hw, fixed = TRUE), grepl("Station", hw, fixed = TRUE),
          grepl("Throughput", hn, fixed = TRUE), grepl(">Cycle<", hn) || grepl("Cycle</th>", hn),
          !grepl("Station", hn, fixed = TRUE), !grepl("On time", hw, fixed = TRUE))
ui <- as.character(sc_breakpoint_ui())
stopifnot(grepl("Breakpoint", ui), grepl("break_cancel", ui), grepl("What this says", ui),
          grepl("Run sweep", ui), grepl("break_result_tabs", ui),
          grepl("Throughput", ui), grepl("Cycle Time", ui), grepl("Saturation", ui),
          !grepl("Delivery", ui), !grepl("On-time target", ui))
cat("breakpoint page checks OK\n")

# Optimize page: preload loads, plot and table build at both widths, help text
# names the exact recheck counts, Cancel is present.
op <- sc_load_preload_optimize()
stopifnot(!is.null(op), op$meta$base_seed == 42L, op$meta$elite_reps == 20L,
          is.finite(op$baseline$cycle_mean_h),
          op$best$fitness <= op$baseline$fitness + 1e-9)
invisible(ggplot2::ggplot_build(sc_plot_opt_history(op, narrow = FALSE)))
invisible(ggplot2::ggplot_build(sc_plot_opt_history(op, narrow = TRUE)))
tw <- sc_table_opt(op, narrow = FALSE)
tn <- sc_table_opt(op, narrow = TRUE)
hw <- as.character(gt::as_raw_html(tw, inline_css = FALSE))
hn <- as.character(gt::as_raw_html(tn, inline_css = FALSE))
stopifnot(grepl("Cycle time", hw, fixed = TRUE) || grepl("Throughput", hw, fixed = TRUE),
          grepl("Net cycle time", hw, fixed = TRUE) || grepl("Net throughput", hw, fixed = TRUE),
          grepl("vs current", hw, fixed = TRUE),
          grepl("Net cycle", hn, fixed = TRUE) || grepl("Net throughput", hn, fixed = TRUE),
          grepl("vs current", hn, fixed = TRUE),
          !grepl("On time", hw, fixed = TRUE),
          !grepl("Fitness", hw, fixed = TRUE),
          !grepl("Fitness", hn, fixed = TRUE))
cap <- sc_opt_caption(op)
stopifnot(grepl("net cycle time", cap, fixed = TRUE) || grepl("net throughput", cap, fixed = TRUE),
          grepl("Lower is better", cap, fixed = TRUE) || grepl("Higher is better", cap, fixed = TRUE))
if (isTRUE(sc_opt_best_in_gen1(op$history))) {
  stopifnot(grepl("first generation", cap, fixed = TRUE))
}
story <- sc_opt_story(op)
if (isTRUE(op$best$clearly_better)) {
  stopifnot(grepl("clearly beats the current line", story, fixed = TRUE),
            !grepl("No change clearly beats", story, fixed = TRUE))
} else {
  stopifnot(grepl("No change clearly beats the current line", story, fixed = TRUE))
}
ui <- as.character(sc_optimize_ui())
stopifnot(grepl("Optimize", ui), grepl("opt_cancel", ui), grepl("What this says", ui),
          grepl("Run search", ui), grepl("Quick", ui), grepl("opt_caption", ui),
          grepl("Algorithm settings", ui), grepl("opt_algo_settings", ui),
          grepl("opt_p_mut", ui), grepl("opt_p_cx", ui), grepl("opt_n_elite", ui),
          !grepl("accordion-collapse collapse show", ui))
stopifnot(isTRUE(all.equal(sc_opt_default_cost(), 1)),
          isTRUE(all.equal(sc_opt_default_tech_cost(), 0.25)))
help <- sc_opt_help_text(3, 15)
stopifnot(grepl("top 3 candidates", help, fixed = TRUE),
          grepl("one-step change", help, fixed = TRUE),
          grepl("15 replications", help, fixed = TRUE),
          grepl("net cycle time", help, fixed = TRUE),
          grepl("within noise", help, fixed = TRUE),
          !grepl("top few", help, fixed = TRUE))
cat("optimize page checks OK\n")

# Warmup page: preload loads, plots and table build at both widths, Cancel
# is present, and the default warm-up field is 0.
wu <- sc_load_preload_warmup()
stopifnot(!is.null(wu), wu$meta$base_seed == 42L, wu$meta$n_reps == 20L,
          is.finite(wu$kpis_all$cycle_mean_h))
invisible(ggplot2::ggplot_build(sc_plot_wu_cycle(wu, narrow = FALSE)))
invisible(ggplot2::ggplot_build(sc_plot_wu_cycle(wu, narrow = TRUE)))
invisible(ggplot2::ggplot_build(sc_plot_wu_wip(wu, narrow = FALSE)))
invisible(ggplot2::ggplot_build(sc_plot_wu_wip(wu, narrow = TRUE)))
tw <- sc_table_wu(wu, narrow = FALSE)
tn <- sc_table_wu(wu, narrow = TRUE)
hw <- as.character(gt::as_raw_html(tw, inline_css = FALSE))
hn <- as.character(gt::as_raw_html(tn, inline_css = FALSE))
stopifnot(grepl("Cycle time", hw, fixed = TRUE), grepl("Throughput", hw, fixed = TRUE),
          grepl("vs default", hw, fixed = TRUE),
          grepl("Throughput", hn, fixed = TRUE), grepl("vs default", hn, fixed = TRUE),
          grepl("Discard", hn, fixed = TRUE),
          !grepl("On time", hw, fixed = TRUE))
story <- sc_wu_story(wu)
stopifnot(nchar(story) > 40, grepl("0", story) || grepl("default", story, ignore.case = TRUE))
ui <- as.character(sc_warmup_ui())
stopifnot(grepl("wu_result_tabs", ui), grepl("Cycle Time", ui),
          grepl(">WIP<", ui) || grepl("WIP</", ui), grepl("KPIs", ui))
stopifnot(grepl("Warmup", ui), grepl("wu_cancel", ui), grepl("What this says", ui),
          grepl("Run check", ui))
fields <- sc_line_fields()
wu_f <- fields[fields$id == "warmup_weeks", ]
stopifnot(nrow(wu_f) == 1L, isTRUE(all.equal(wu_f$default[[1]], 0)))
p_def <- sc_params_from_inputs(list())
stopifnot(is.null(p_def$warmup_h) || isTRUE(all.equal(p_def$warmup_h, 0)))
p_on <- sc_params_from_inputs(list(warmup_weeks = 3))
stopifnot(isTRUE(all.equal(p_on$warmup_h, 3 * 7 * 24)))
cat("warmup page checks OK\n")
