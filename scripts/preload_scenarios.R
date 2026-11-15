#!/usr/bin/env Rscript
# preload_scenarios.R
# Rebuild app/data/preload_scenarios.rds: the default baseline plus the
# catalog capacity-move what-ifs (including +1 QA inspector), 20
# replications, common random numbers. The Analyze preload is no longer
# built; that page was retired into Results.

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
for (id in setdiff(unique(tbl$id), "baseline")) {
  ship <- tbl$mean[tbl$id == id & tbl$col == "shipped"]
  stopifnot(length(ship) == 1, is.finite(ship), ship >= base_ship - 2)
}
# Baseline inspector utilization and the +1 inspector what-if, for the handoff report.
if ("per_rep" %in% names(bundle$results)) {
  pr_all <- bundle$results
  if (is.data.frame(pr_all) && "id" %in% names(pr_all) && "per_rep" %in% names(pr_all)) {
    get_pr <- function(id) pr_all$per_rep[[match(id, pr_all$id)]]
    bpr <- get_pr("baseline")
    ipr <- get_pr("add_qa_inspector")
    if (is.data.frame(bpr) && "qa_inspector_util" %in% names(bpr)) {
      message(sprintf("baseline QA inspector util %.1f%%, WIP %.2f",
                      100 * mean(bpr$qa_inspector_util, na.rm = TRUE),
                      mean(bpr$wip_avg, na.rm = TRUE)))
    }
    if (is.data.frame(ipr) && is.data.frame(bpr)) {
      message(sprintf("+1 QA inspector: throughput %.1f (d%+.1f), cycle %.1f h, inspector util %.1f%%",
                      mean(ipr$shipped, na.rm = TRUE),
                      mean(ipr$shipped, na.rm = TRUE) - mean(bpr$shipped, na.rm = TRUE),
                      mean(ipr$cycle_mean_h, na.rm = TRUE),
                      100 * mean(ipr$qa_inspector_util, na.rm = TRUE)))
    }
  }
}

bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
out <- file.path("app", "data", "preload_scenarios.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")

# Analyze preload is retired with the Analyze page. Tornado preload is left
# as-is (not refreshed in this script).
message("Skipped Analyze preload (page retired into Results).")
if (file.exists(file.path("app", "data", "preload_analyze.rds"))) {
  message("Note: leftover app/data/preload_analyze.rds is no longer used.")
}
