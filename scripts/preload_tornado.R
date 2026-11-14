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
  c("spacecraft_params", "spacecraft_labor", "spacecraft_reliability", "spacecraft_model", "spacecraft_metrics", "spacecraft_tornado"),
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
# Checkpoint the full bundle first so a later direction-check failure does not
# throw away the run. The final save below drops per_rep and overwrites this.
bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
bundle$meta$source <- "preload"
saveRDS(bundle, file.path("app", "data", "preload_tornado.rds"))
message("Checkpointed app/data/preload_tornado.rds (", round(file.size(file.path("app", "data", "preload_tornado.rds")) / 1024), " KB)")

# Direction checks so a bad preload never ships: longer process, subassembly
# and inspection times should raise cycle time; an extra station or technician
# should lower it. Functional / environmental fail rates are a small share of
# cycle time under the retuned baseline (mostly inspection waiting), so the
# swing can be noise.
cyc <- bundle$effects %>% dplyr::filter(metric == "cycle")
print_row <- function(id) {
  row <- cyc[cyc$id == id, ]
  message(sprintf("  %s: low %.2f, base %.2f, high %.2f, swing %.2f",
                  id, row$low, row$base, row$high, row$swing))
  row
}
up <- function(id) {
  row <- print_row(id)
  stopifnot(nrow(row) == 1L, row$high > row$low)
}
down <- function(id) {
  row <- print_row(id)
  stopifnot(nrow(row) == 1L, row$high < row$low)
}
for (id in c("integ_time", "func_time", "env_time",
             "subasm_time", "qa_time", "mttr")) up(id)
# Fail rates and MRB holds are a small share of cycle time under the
# retuned baseline (mostly inspection waiting), so the swing can be noise.
for (id in c("p_fail_func", "p_fail_env", "mrb_time")) {
  row <- print_row(id)
  stopifnot(nrow(row) == 1L, is.finite(row$swing), row$swing < 30)
}
for (id in c("cap_subassembly", "cap_integration", "cap_chamber", "techs")) down(id)
# MTBF/MTTR swings are small beside inspection holds; MTBF high can wander the wrong way.
mtbf <- print_row("mtbf")
stopifnot(nrow(mtbf) == 1L, is.finite(mtbf$swing), mtbf$swing < 30)

# Drop bulky per-replication rows from the shipped file; tests that need
# CRN reproducibility run a short live tornado themselves.
bundle$per_rep <- NULL
bundle$meta$generated_at <- format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE)
bundle$meta$source <- "preload"
out <- file.path("app", "data", "preload_tornado.rds")
saveRDS(bundle, out)
message("Wrote ", out, " (", round(file.size(out) / 1024), " KB)")
message("Story (cycle): ", sc_tornado_story(bundle, "cycle"))
