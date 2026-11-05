# app_checks.R
# Shiny smoke checks: every file parses, the input spec round-trips into a
# params list, and the server runs one simulation through testServer().
# Run from repo root: Rscript tests/app_checks.R
suppressPackageStartupMessages({ library(shiny); library(bslib); library(bsicons); library(tidyverse); library(simmer); library(DT) })
purrr::walk(list.files("app/R", full.names = TRUE), parse)
purrr::walk(list.files("app/R", full.names = TRUE), source)

# Every spec path points at a real number in the defaults.
spec <- sc_line_fields()
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
  fields <- sc_line_fields()
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
  session$setInputs(sim_hours = 4000, n_reps = 3)
  r2 <- apply()
  stopifnot(input$set_sim_hours == 4000, input$set_n_reps == 3, r2 <= 2, nrow(app$line_tab$unsaved()) == 0)

  # No loop: copies sent so far = 3 saved + 2 synced, nothing echoed back.
  stopifnot(app$line_tab$copies() == 5L)

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
stopifnot(all(sc_item_label(lv$item) != lv$item))
# Sidebar and Settings are built from the same field list.
stopifnot(!anyDuplicated(sc_line_fields()$id), all(sc_line_fields()$panel %in% sc_line_panels$panel))
message("settings page checks OK")
