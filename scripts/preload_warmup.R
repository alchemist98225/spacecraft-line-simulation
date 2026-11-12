#!/usr/bin/env Rscript
# preload_warmup.R
# Rebuild app/data/preload_warmup.rds: warm-up check on the default line,
# 20 replications, seed 42, common random numbers. The "keep every unit"
# cycle time must match Compare / Analyze (179.2 h).
# Run from the repo root: source /workspace/sim/activate_r.sh && Rscript scripts/preload_warmup.R

suppressPackageStartupMessages({ library(tidyverse); library(simmer) })

root <- {
  cand <- c(".", "..")
  hit <- cand[file.exists(file.path(cand, "app", "R", "spacecraft_warmup.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
purrr::walk(
  c("spacecraft_params", "spacecraft_model", "spacecraft_metrics", "spacecraft_warmup"),
  ~ source(file.path("app", "R", paste0(.x, ".R")), local = FALSE)
)

dir.create(file.path("app", "data"), showWarnings = FALSE, recursive = TRUE)

n_reps <- 20L
base_seed <- 42L
params <- sc_default_params()
message("Running warm-up check: ", n_reps, " replications, seed ", base_seed, " …")
bundle <- sc_wu_run(
  params, n_reps = n_reps, base_seed = base_seed,
  on_progress = function(done, total) {
    if (done == total || done %% 5L == 0L) message(sprintf("  %d / %d", done, total))
  }
)
stopifnot(!isTRUE(bundle$meta$cancelled), nrow(bundle$wip_ens) > 10,
          abs(bundle$kpis_all$cycle_mean_h - 179.2) < 0.15)
message(sprintf("All units: cycle %.1f h, on-time %.1f%%",
                bundle$kpis_all$cycle_mean_h, bundle$kpis_all$on_time_pct))
message(sprintf("Recommend: %.0f weeks (order cut %.0f)",
                bundle$recommend$warmup_weeks, bundle$recommend$order_cut %||% NA))
if (is.finite(bundle$recommend$warmup_weeks) && bundle$recommend$warmup_weeks > 0) {
  message(sprintf("After discard: cycle %.1f h, on-time %.1f%%",
                  bundle$kpis_drop$cycle_mean_h, bundle$kpis_drop$on_time_pct))
}
bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
bundle$meta$source <- "preload"
out <- file.path("app", "data", "preload_warmup.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB) in ",
        sprintf("%.1f", bundle$meta$secs), " s")
message("Story: ", sc_wu_story(bundle))
