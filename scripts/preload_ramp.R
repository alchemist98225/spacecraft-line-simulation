#!/usr/bin/env Rscript
# preload_ramp.R
# Rebuild app/data/preload_ramp.rds: capacity-ramp plan for a lean early line
# (day shift, two technicians, one of each station) from 12 to 48 units/year
# over 24 months, 10 replications, seed 42, 85% learning curve.
# Run from the repo root: source /workspace/sim/activate_r.sh && Rscript scripts/preload_ramp.R

suppressPackageStartupMessages({ library(tidyverse); library(simmer) })

root <- {
  cand <- c(".", "..")
  hit <- cand[file.exists(file.path(cand, "app", "R", "spacecraft_ramp.R"))]
  normalizePath(hit[[1]], mustWork = TRUE)
}
setwd(root)
purrr::walk(
  c("spacecraft_params", "spacecraft_labor", "spacecraft_reliability", "spacecraft_model", "spacecraft_metrics",
    "spacecraft_theme", "spacecraft_analyze", "spacecraft_tornado", "spacecraft_breakpoint",
    "spacecraft_ramp"),
  ~ source(file.path("app", "R", paste0(.x, ".R")), local = FALSE)
)

dir.create(file.path("app", "data"), showWarnings = FALSE, recursive = TRUE)

n_reps <- 10L
base_seed <- 42L
cfg <- sc_ramp_defaults()
params <- sc_ramp_lean_params(sc_default_params())
schedule <- sc_ramp_default_schedule()
message("Running ramp plan: lean start, ", n_reps, " replications, seed ", base_seed, " …")
bundle <- sc_ramp_run(
  params, schedule, cfg, n_reps = n_reps, base_seed = base_seed,
  on_progress = function(done, total, label) {
    if (done == total || done %% 10L == 0L) message(sprintf("  %d / %d (%s)", done, total, label))
  }
)
stopifnot(!isTRUE(bundle$meta$cancelled), nrow(bundle$points) == 8L,
          nrow(bundle$moves) >= 1L,
          any(bundle$points$meets_target),
          !all(bundle$points$meets_target) || TRUE,
          nchar(bundle$story) > 40)
# The lean start should need at least one capacity move, and delaying a
# shift or station add should be able to make a later quarter miss.
stopifnot(any(bundle$moves$kind %in% c("shift", "tech", "station")))
if (nrow(bundle$delays)) {
  stopifnot(any(!bundle$delays$meets_target) || any(bundle$points$meets_target))
}
bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
bundle$meta$source <- "preload"
bundle$meta$start <- "lean"
out <- file.path("app", "data", "preload_ramp.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB) in ",
        sprintf("%.1f", bundle$meta$secs), " s")
message("Moves: ", paste(bundle$moves$label, collapse = "; "))
message("Meets: ", paste(sprintf("Q%d=%s (%.1f/%.0f)", bundle$points$quarter,
                                 ifelse(bundle$points$meets_target, "yes", "no"),
                                 bundle$points$shipped_mean, bundle$points$target_rate),
                         collapse = ", "))
message("Story: ", bundle$story)
