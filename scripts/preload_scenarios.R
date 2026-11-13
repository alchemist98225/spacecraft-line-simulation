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
  c("spacecraft_params", "spacecraft_labor", "spacecraft_model", "spacecraft_metrics", "spacecraft_compare", "spacecraft_analyze"),
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
base_hpu <- tbl$mean[tbl$id == "baseline" & tbl$col == "labor_hpu"]
rework_cycle <- tbl$mean[tbl$id == "rework" & tbl$col == "cycle_mean_h"]
rework_hpu <- tbl$mean[tbl$id == "rework" & tbl$col == "labor_hpu"]
shift_cycle <- tbl$mean[tbl$id == "lose_shift" & tbl$col == "cycle_mean_h"]
fast_cycle <- tbl$mean[tbl$id == "faster" & tbl$col == "cycle_mean_h"]
message(sprintf("baseline cycle %.1f h, HPU %.1f", base_cycle, base_hpu))
message(sprintf("rework: cycle %.1f h, HPU %.1f", rework_cycle, rework_hpu))
message(sprintf("lose_shift: cycle %.1f h", shift_cycle))
message(sprintf("faster: cycle %.1f h", fast_cycle))
stopifnot(rework_cycle > base_cycle, rework_hpu > base_hpu,
          shift_cycle > base_cycle, fast_cycle > base_cycle)

bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
out <- file.path("app", "data", "preload_scenarios.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")

# Analyze preload: the same baseline, replications and seed, with the
# diagnostics the Analyze page draws (time breakdown, bottleneck, queues).
message("Running Analyze preload: baseline × ", n_reps, " replications, seed ", base_seed, " …")
runs <- sc_run(sc_default_params(), n_reps = n_reps, base_seed = base_seed)
analyze <- sc_analyze(runs, sc_default_params())
analyze$meta$n_reps <- n_reps
analyze$meta$base_seed <- base_seed
analyze$meta$source <- "preload"
analyze$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
# Sanity: parts add up, utilization is a fraction, chamber is busiest at defaults.
ov <- analyze$breakdown[analyze$breakdown$variant == "Overall", ]
stopifnot(abs(sum(ov$share) - 1) < 1e-8, all(analyze$resources$util >= 0),
          all(analyze$resources$util <= 1 + 1e-9),
          identical(analyze$bottleneck$resource, "chamber"))
out_a <- file.path("app", "data", "preload_analyze.rds")
saveRDS(analyze, out_a)
message("Wrote ", out_a, " (", round(file.size(out_a) / 1024), " KB)")
message("Analyze story: ", analyze$story)
