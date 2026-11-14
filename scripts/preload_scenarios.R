#!/usr/bin/env Rscript
# preload_scenarios.R
# Rebuild app/data/preload_scenarios.rds: the default baseline plus five
# capacity-move what-ifs, 20 replications, common random numbers.

suppressPackageStartupMessages({ library(tidyverse); library(simmer) })

root <- {
  cand <- c(".", "..")
  hit <- cand[file.exists(file.path(cand, "app", "R", "spacecraft_compare.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
purrr::walk(
  c("spacecraft_params", "spacecraft_labor", "spacecraft_reliability", "spacecraft_model", "spacecraft_metrics", "spacecraft_compare", "spacecraft_analyze"),
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
base_ship <- tbl$mean[tbl$id == "baseline" & tbl$col == "shipped"]
base_hpu <- tbl$mean[tbl$id == "baseline" & tbl$col == "labor_hpu"]
message(sprintf("baseline: throughput %.1f, cycle %.1f h, HPU %.1f", base_ship, base_cycle, base_hpu))
for (id in setdiff(unique(tbl$id), "baseline")) {
  ship <- tbl$mean[tbl$id == id & tbl$col == "shipped"]
  cyc <- tbl$mean[tbl$id == id & tbl$col == "cycle_mean_h"]
  hpu <- tbl$mean[tbl$id == id & tbl$col == "labor_hpu"]
  wip <- tbl$mean[tbl$id == id & tbl$col == "wip_avg"]
  message(sprintf("%s: throughput %.1f (d%+.1f), cycle %.1f h (d%+.1f), HPU %.1f, WIP %.1f",
                  id, ship, ship - base_ship, cyc, cyc - base_cycle, hpu, wip))
}
qa_cycle <- tbl$mean[tbl$id == "faster_qa" & tbl$col == "cycle_mean_h"]
stopifnot(length(qa_cycle) == 1, qa_cycle < base_cycle)
for (id in c("add_subassembly", "add_integration", "add_tech", "add_weekends", "faster_qa")) {
  ship <- tbl$mean[tbl$id == id & tbl$col == "shipped"]
  stopifnot(length(ship) == 1, is.finite(ship), ship >= base_ship - 2)
}

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
# Sanity: parts add up, utilization is a fraction, and the bottleneck is a
# known station. Subassembly is typically busiest on the retuned baseline.
ov <- analyze$breakdown[analyze$breakdown$variant == "Overall", ]
known <- sc_resource_catalog()$resource
stopifnot(abs(sum(ov$share) - 1) < 1e-8, all(analyze$resources$util >= 0),
          all(analyze$resources$util <= 1 + 1e-9),
          analyze$bottleneck$resource %in% known,
          is.finite(analyze$bottleneck$util), analyze$bottleneck$util > 0)
message("Analyze bottleneck: ", analyze$bottleneck$resource, " at ",
        sprintf("%.0f%%", 100 * analyze$bottleneck$util))
out_a <- file.path("app", "data", "preload_analyze.rds")
saveRDS(analyze, out_a)
message("Wrote ", out_a, " (", round(file.size(out_a) / 1024), " KB)")
message("Analyze story: ", analyze$story)
