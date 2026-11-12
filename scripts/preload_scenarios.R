#!/usr/bin/env Rscript
# preload_scenarios.R
# Rebuild app/data/preload_scenarios.rds: the default baseline plus the three
# illustrative what-if scenarios, 20 replications, common random numbers.

suppressPackageStartupMessages({ library(tidyverse); library(simmer) })

root <- {
  cand <- c(".", "..")
  hit <- cand[file.exists(file.path(cand, "app", "R", "spacecraft_compare.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
purrr::walk(
  c("spacecraft_params", "spacecraft_model", "spacecraft_metrics", "spacecraft_compare"),
  ~ source(file.path("app", "R", paste0(.x, ".R")), local = FALSE)
)

dir.create(file.path("app", "data"), showWarnings = FALSE, recursive = TRUE)

n_reps <- 20L
base_seed <- 42L
scenarios <- sc_compare_catalog()
message("Running ", nrow(scenarios), " scenarios × ", n_reps, " replications, seed ", base_seed, " …")
bundle <- sc_compare_run(
  scenarios, n_reps = n_reps, base_seed = base_seed,
  on_progress = function(done, total, label) {
    if (done == total || done %% n_reps == 0) message(sprintf("  %d / %d (%s)", done, total, label))
  }
)

# Drop the large per-replication raw tables already discarded; keep per_rep KPIs.
stopifnot(!isTRUE(bundle$meta$cancelled), nrow(bundle$results) == nrow(scenarios))
# Direction check so a bad preload never ships.
tbl <- bundle$table
base_cycle <- tbl$mean[tbl$id == "baseline" & tbl$col == "cycle_mean_h"]
for (id in c("chamber2", "fail_env", "integ3")) {
  cycle <- tbl$mean[tbl$id == id & tbl$col == "cycle_mean_h"]
  ontime <- tbl$mean[tbl$id == id & tbl$col == "on_time_pct"]
  base_on <- tbl$mean[tbl$id == "baseline" & tbl$col == "on_time_pct"]
  message(sprintf("%s: cycle %.1f h (baseline %.1f), on-time %.1f%% (baseline %.1f)",
                  id, cycle, base_cycle, ontime, base_on))
  stopifnot(cycle < base_cycle, ontime > base_on)
}

bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
out <- file.path("app", "data", "preload_scenarios.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")
