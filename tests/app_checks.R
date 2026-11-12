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
  stopifnot(nrow(app$line_tab$unsaved()) == 3, input$p_fail_func == 0.15)
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
                                "Thermal vacuum", "Thermal vacuum variation", "Due date allowance", "Due date spread")))
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
  tabs <- list(sc_table_cycle(sm$units, p), sc_table_lateness(sm$units, p), sc_table_kpi_reps(sm$per_rep))
  html <- purrr::map_chr(tabs, ~ as.character(gt::as_raw_html(.x, inline_css = FALSE)))
  stopifnot(nrow(tabs[[1]][["_data"]]) == 3, nrow(tabs[[2]][["_data"]]) == 3, nrow(tabs[[3]][["_data"]]) == 6,
            all(grepl("<img|<svg", html)), all(grepl("Variant", html[1:2])), grepl("Chamber utilization", html[[3]]))
  cat(sprintf("tables OK for %g h\n", hrs))
}
# Inline star-map flow diagram: routes, arrowheads, labels and colours.
flow <- as.character(sc_flow_svg())
stopifnot(grepl("<svg", flow), grepl("viewBox", flow))
for (lab in c("Kitting", "Integration", "View settings", "Functional test",
              "Environmental test", "Vibration", "Thermal vacuum",
              "Final acceptance", "+ ship", "Review", "Scrap",
              "Forward", "Accepted", "Rework", "Fail")) {
  stopifnot(grepl(lab, flow, fixed = TRUE))
}
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
stopifnot(grepl("Baseline", html), grepl("Second chamber", html),
          grepl("Mean cycle time (h)", html, fixed = TRUE),
          grepl("Mean lateness (h)", html, fixed = TRUE),
          grepl("Throughput (units)", html, fixed = TRUE),
          grepl("On time (%)", html, fixed = TRUE),
          grepl("Average WIP (units)", html, fixed = TRUE),
          grepl("Negative means", html) || grepl("negative means", html),
          grepl("quarter of the baseline", html))
# Metric labels are not truncated, and a tiny throughput delta is not treated as notable.
stopifnot(!grepl("Mean latene<", html), !grepl("Average WIF", html), !grepl("Mean cycle t<", html))
marked <- sc_compare_mark_deltas(preload$table)
thr <- marked %>% dplyr::filter(id == "chamber2", col == "shipped")
cyc <- marked %>% dplyr::filter(id == "chamber2", col == "cycle_mean_h")
lat <- marked %>% dplyr::filter(id == "chamber2", col == "lateness_mean_h")
stopifnot(isTRUE(cyc$notable[[1]]), isTRUE(cyc$improved[[1]]),
          isTRUE(lat$delta[[1]] < 0), isTRUE(lat$improved[[1]]),
          abs(thr$delta[[1]]) < thr$noise[[1]], !isTRUE(thr$notable[[1]]))
invisible(ggplot2::ggplot_build(sc_plot_compare(preload$table)))
ui <- as.character(sc_compare_ui())
stopifnot(grepl("Compare", ui), grepl("Add from current Settings", ui), grepl("compare_cancel", ui))
changes <- sc_params_changes(sc_default_params(), { p <- sc_default_params(); p$capacity$chamber <- 2; p })
stopifnot(any(grepl("Environmental test chambers", changes)))
cat("compare page checks OK\n")
