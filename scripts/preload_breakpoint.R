#!/usr/bin/env Rscript
# preload_breakpoint.R
# Rebuild app/data/preload_breakpoint.rds: demand sweep around the default
# line, 20 replications, seed 42, common random numbers. The baseline point
# must match the Compare / Analyze / Tornado baseline (the same default line as Compare).

suppressPackageStartupMessages({ library(tidyverse); library(simmer) })

root <- {
  cand <- c(".", "..")
  hit <- cand[file.exists(file.path(cand, "app", "R", "spacecraft_breakpoint.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
purrr::walk(
  c("spacecraft_params", "spacecraft_labor", "spacecraft_model", "spacecraft_metrics", "spacecraft_analyze", "spacecraft_breakpoint"),
  ~ source(file.path("app", "R", paste0(.x, ".R")), local = FALSE)
)

dir.create(file.path("app", "data"), showWarnings = FALSE, recursive = TRUE)

n_reps <- 20L
base_seed <- 42L
params <- sc_default_params()
demand <- sc_break_default_grid(params)
message("Running breakpoint sweep: ", length(demand), " demand points x ",
        n_reps, " replications, seed ", base_seed, " …")
message("  demand (units/year): ", paste(sprintf("%.1f", demand), collapse = ", "))
bundle <- sc_break_run(
  params, demand = demand, n_reps = n_reps, base_seed = base_seed,
  on_progress = function(done, total, label) {
    if (done == total || done %% n_reps == 0) message(sprintf("  %d / %d (%s)", done, total, label))
  }
)
stopifnot(!isTRUE(bundle$meta$cancelled), nrow(bundle$points) == length(demand))

base_row <- bundle$points[bundle$points$is_baseline, ]
stopifnot(nrow(base_row) == 1L)
message(sprintf("Baseline: cycle %.1f h, throughput %.1f, demand %.1f units/year",
                base_row$cycle_mean, base_row$shipped_mean, base_row$demand))
stopifnot(is.finite(base_row$cycle_mean))

# Signs so a bad preload never ships: higher demand, higher cycle.
last <- bundle$points[bundle$points$demand == max(bundle$points$demand), ]
stopifnot(last$cycle_mean > base_row$cycle_mean)
br <- sc_break_detect(bundle, cycle_mult = 1.5, sat_threshold = 0.90)
stopifnot(isTRUE(br$found), br$demand > base_row$demand)
message(sprintf("Binding limit: %.0f units/year (reason %s, cycle cap %.0f h)",
                br$demand, br$reason, br$cycle_limit))
if (isTRUE(br$sat_found)) {
  message(sprintf("First saturation: %s at %.0f units/year (%.0f%% busy)",
                  br$sat_station, br$sat_demand, 100 * br$sat_util))
}

# Drop bulky per-replication rows from the shipped file.
bundle$per_rep <- NULL
bundle$util_rep <- NULL
bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
bundle$meta$source <- "preload"
out <- file.path("app", "data", "preload_breakpoint.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")
message("Story: ", sc_break_story(bundle, 1.5, 0.90))
