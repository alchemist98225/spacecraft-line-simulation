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
# Sidebar and Settings are built from the same field list.
stopifnot(!anyDuplicated(sc_line_fields()$id), all(sc_line_fields()$panel %in% sc_line_panels$panel))
message("settings page checks OK")
