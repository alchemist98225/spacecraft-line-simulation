#!/usr/bin/env Rscript
# preload_tornado.R
# Rebuild app/data/preload_tornado.rds: one-at-a-time tornado around the
# default line, 20 replications, seed 42, common random numbers.

suppressPackageStartupMessages({ library(tidyverse); library(simmer) })

root <- {
  cand <- c(".", "..")
  hit <- cand[file.exists(file.path(cand, "app", "R", "spacecraft_tornado.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
purrr::walk(
  c("spacecraft_params", "spacecraft_model", "spacecraft_metrics", "spacecraft_tornado"),
  ~ source(file.path("app", "R", paste0(.x, ".R")), local = FALSE)
)

dir.create(file.path("app", "data"), showWarnings = FALSE, recursive = TRUE)

n_reps <- 20L
base_seed <- 42L
message("Running tornado: ", nrow(sc_tornado_factors()), " factors x low/high + baseline, ",
        n_reps, " replications, seed ", base_seed, " …")
bundle <- sc_tornado_run(
  sc_default_params(), n_reps = n_reps, base_seed = base_seed,
  on_progress = function(done, total, label) {
    if (done == total || done %% n_reps == 0) message(sprintf("  %d / %d (%s)", done, total, label))
  }
)
stopifnot(!isTRUE(bundle$meta$cancelled), nrow(bundle$effects) > 0)

# Direction checks so a bad preload never ships: longer times and higher
# fail rates should raise cycle time; an extra chamber or integration
# station should lower it.
cyc <- bundle$effects %>% dplyr::filter(metric == "cycle")
up <- function(id) {
  row <- cyc[cyc$id == id, ]
  stopifnot(nrow(row) == 1L, row$high > row$low)
}
down <- function(id) {
  row <- cyc[cyc$id == id, ]
  stopifnot(nrow(row) == 1L, row$high < row$low)
}
for (id in c("integ_time", "func_time", "env_time", "p_fail_func", "p_fail_env",
             "mrb_time", "kit_lead", "kit_p_late")) up(id)
for (id in c("cap_integration", "cap_chamber")) down(id)

# Drop bulky per-replication rows from the shipped file; tests that need
# CRN reproducibility run a short live tornado themselves.
bundle$per_rep <- NULL
bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
bundle$meta$source <- "preload"
out <- file.path("app", "data", "preload_tornado.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")
message("Story (cycle): ", sc_tornado_story(bundle, "cycle"))
