# preload_optimize.R
# Rebuild app/data/preload_optimize.rds: a Quick search (pop 6, 4 generations,
# 2 search replications) on the default line, then re-check the current line
# and the top 3 candidates with 20 replications, seed 42, so the baseline
# cycle time matches Compare / Analyze / Tornado / Breakpoint (the same default line as Compare).
# Run from the repo root: source /workspace/sim/activate_r.sh && Rscript scripts/preload_optimize.R
suppressPackageStartupMessages({ library(tidyverse); library(simmer) })
for (f in c("spacecraft_params", "spacecraft_labor", "spacecraft_reliability", "spacecraft_model", "spacecraft_metrics",
            "spacecraft_ga", "spacecraft_optimize")) {
  source(file.path("app/R", paste0(f, ".R")))
}

pre <- sc_opt_presets()$preload
t0 <- Sys.time()
bundle <- sc_opt_run(
  sc_default_params(),
  pop_size = pre$pop_size, n_gen = pre$n_gen, n_reps = pre$n_reps,
  elite_n = pre$elite_n, elite_reps = pre$elite_reps,
  base_seed = 42L, capacity_cost = sc_opt_default_cost(), objective = "cycle",
  tech_cost = sc_opt_default_tech_cost()
)
secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
stopifnot(!isTRUE(bundle$meta$cancelled), nrow(bundle$history) == pre$n_gen,
          nrow(bundle$baseline) == 1L, nrow(bundle$best) == 1L,
          is.finite(bundle$baseline$cycle_mean_h),
          bundle$best$fitness <= bundle$baseline$fitness + 1e-9)
# Keep compact per-replication rows so the page can build paired changes.
if ("per_rep" %in% names(bundle$recheck)) {
  bundle$recheck$per_rep <- lapply(bundle$recheck$per_rep, function(d) {
    if (is.null(d) || !nrow(d)) return(d)
    dplyr::select(d, dplyr::any_of(c("rep", "shipped", "cycle_mean_h", "labor_hpu", "labor_util")))
  })
}
bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
bundle$meta$source <- "preload"
bundle$meta$secs <- secs
out <- file.path("app", "data", "preload_optimize.rds")
saveRDS(bundle, out)
message(sprintf("Wrote %s (%.0f KB) in %.1f s", out, file.size(out) / 1024, secs))
message(sprintf("Baseline: cycle %.1f h, throughput %.1f, net cycle %.1f",
                bundle$baseline$cycle_mean_h, bundle$baseline$shipped,
                bundle$baseline$score_mean))
message(sprintf("Best: net score %.1f, paired %+0.2f (%.2f to %.2f), within_noise=%s, clearly_better=%s, throughput %.1f, cycle %.1f h, extra st %s tech %s, change: %s",
                bundle$best$score_mean, bundle$best$delta_mean,
                bundle$best$delta_lo, bundle$best$delta_hi,
                bundle$best$within_noise, bundle$best$clearly_better,
                bundle$best$shipped, bundle$best$cycle_mean_h,
                bundle$best$extra_capacity, bundle$best$extra_techs, bundle$best$change))
message("Story: ", sc_opt_story(bundle))
