# app_checks.R
# Shiny smoke checks: every file parses, the input spec round-trips into a
# params list, and the server runs one simulation through testServer().
# Run from repo root: Rscript tests/app_checks.R
suppressPackageStartupMessages({ library(shiny); library(bslib); library(bsicons); library(tidyverse); library(simmer); library(DT) })
purrr::walk(list.files("app/R", full.names = TRUE), parse)
purrr::walk(list.files("app/R", full.names = TRUE), source)

# Every spec path points at a real number in the defaults.
spec <- sc_input_spec()
stopifnot(!anyDuplicated(spec$id), all(is.finite(spec$default)))
p <- sc_params_from_inputs(list(p_fail_func = 0.3, cap_chamber = 2, kit_p_late = 0))
stopifnot(p$p_fail_func == 0.3, p$capacity$chamber == 2, p$kit$p_late == 0)

testServer(sc_server, {
  session$setInputs(n_reps = 2, seed = 3, run = 1)
  r <- result()
  stopifnot(nrow(r$per_rep) == 2, is.finite(r$summary$cycle_mean_h))
})
message("app_checks OK")

# Parameters page: the variant dropdown shows one variant at a time, edits land
# on the right row of the full table, and they survive switching variants.
testServer(sc_server, {
  session$setInputs(variant_pick = "B")
  stopifnot(all(parameters_page$shown()$variant == "B"))
  session$setInputs(variants_table_cell_edit = list(row = 1, col = 0, value = "0.9"))
  session$setInputs(variant_pick = "A")
  stopifnot(all(parameters_page$shown()$variant == "A"))
  session$setInputs(variants_table_cell_edit = list(row = 2, col = 0, value = "21"))
  session$setInputs(variant_pick = "B")
  v <- current_params()$variants
  stopifnot(v$mix[v$variant == "B"] == 0.9, v$route[[1]]$mean_h[[1]] == 21,
            v$mix[v$variant == "A"] == sc_variants_default()$mix[[1]])
})
stopifnot(all(sc_item_label(sc_variants_long(sc_variants_default())$item) != sc_variants_long(sc_variants_default())$item))
message("parameters page checks OK")
