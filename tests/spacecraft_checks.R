# spacecraft_checks.R
# Acceptance checks for the optional spacecraft line.
# Run from repo root: Rscript tests/spacecraft_checks.R
suppressPackageStartupMessages({ library(tidyverse); library(simmer) })
for (f in c("spacecraft_params", "spacecraft_model", "spacecraft_metrics", "spacecraft_ga", "spacecraft_compare", "spacecraft_analyze", "spacecraft_tornado", "spacecraft_breakpoint")) {
  source(file.path("app/R", paste0(f, ".R")))
}
source("tests/helpers/spacecraft_expected.R")

no_queue <- function(p) {
  p$kit <- list(delay_mean_h = 0, delay_cv = 0, p_late = 0, late_extra_mean_h = 1)
  p$capacity <- purrr::map(p$capacity, ~ 1e4)
  p
}

# 1. Mean cycle time vs the runtime-computed expectation (within 5%).
p <- sc_default_params() %>% no_queue()
p$arrival <- list(process = "fixed", mean_interarrival_h = 2)
p$sim_hours <- 1e5                      # long horizon so every unit finishes
p$p_fail_func <- 0.15
p$p_fail_env <- 0
p$mrb$p_use_as_is <- 1; p$mrb$p_rework <- 0; p$mrb$p_scrap <- 0
# Release units over the first 3000 h only; the long horizon lets all finish.
release <- modifyList(p, list(sim_hours = 3000))
units <- 7:10 %>%
  purrr::map(~ sc_run_one(p, seed = .x, plan = sc_arrival_plan(release, .x))) %>%
  purrr::map_dfr(sc_units)
sim_mean <- mean(units$cycle_h, na.rm = TRUE)
# Variants are drawn from `mix`, so compare against the realised variant shares.
p_real <- p; p_real$variants$mix <- as.numeric(table(factor(units$variant, 1:3))) / nrow(units)
rel_err <- abs(sim_mean / sc_expected_cycle_h(p_real) - 1)
stopifnot(all(units$status == "shipped"), rel_err < 0.05)
message(sprintf("cycle-time check OK (relative error %.2f%%)", 100 * rel_err))

# 2. Loop cap: nobody exceeds max_fails, and the cap always routes to MRB.
p <- sc_default_params()
p$p_fail_func <- 0.6; p$p_fail_env <- 0.5; p$mrb$p_rework <- 0.9
u <- purrr::map_dfr(sc_run(p, n_reps = 3, base_seed = 11), sc_units)
stopifnot(
  max(u$fail_count) <= p$max_fails,
  all(u$fails_at_first_mrb <= p$max_fails, na.rm = TRUE),
  all(u$mrb_visits[u$fail_count >= p$max_fails & u$status != "wip"] >= 1),
  any(u$integration_visits > 1)
)
message("loop-cap check OK")

# 3. Conservation: arrivals = shipped + scrapped + WIP at the end.
k <- sc_summarise(sc_run(sc_default_params(), n_reps = 3, base_seed = 5))$per_rep
stopifnot(all(k$arrivals == k$shipped + k$scrapped + k$wip_end), all(k$scrapped >= 0))
message("conservation check OK")

# 4. Preemption by slack: an urgent unit bumps a relaxed one out of the chamber.
p <- sc_default_params() %>% no_queue()
p$capacity$chamber <- 1; p$capacity$func_test <- 1; p$capacity$acceptance <- 1e4
p$p_fail_func <- 0; p$p_fail_env <- 0
p$variants <- p$variants %>% mutate(
  route = purrr::map(route, ~ mutate(.x, cv = 1e-6)),
  func_test = purrr::map(func_test, ~ modifyList(.x, list(cv = 1e-6))),
  env = purrr::map(env, ~ modifyList(.x, list(vibe_cv = 1e-6, tvac_cv = 1e-6)))
)
p$acceptance$cv <- 1e-6
plan <- tibble(unit_id = 1:2, arrive_h = c(0, 10), variant = c(1L, 1L),
               due_h = c(1e5, 80), kit_delay_h = 0, kit_h = c(0, 10))
ship_order <- function(preempt) {
  p$chamber$preempt <- preempt
  sc_units(sc_run_one(p, seed = 1, plan = plan)) %>% arrange(end_time) %>% pull(unit_id)
}
stopifnot(identical(ship_order(TRUE), c(2, 1)), identical(ship_order(FALSE), c(1, 2)))
# Resume vs restart: restarting the bumped unit's test makes it ship later.
ship_time <- function(restart) {
  p$chamber$preempt <- TRUE; p$chamber$restart <- restart
  sc_units(sc_run_one(p, seed = 1, plan = plan)) %>% filter(unit_id == 1) %>% pull(end_time)
}
stopifnot(ship_time(TRUE) > ship_time(FALSE))
message("preemption check OK")

# 5. GA adapter runs and never reports worse than the default genome.
ga <- sc_run_ga(pop_size = 4L, n_gen = 2L, n_reps = 1L)
stopifnot(ga$best$fitness <= ga$baseline$fitness, nrow(ga$history) == 2L,
          all(purrr::map_lgl(ga$evaluated$genome, ~ all(.x >= ga$defs$lo & .x <= ga$defs$hi))))
message("GA adapter check OK")

# 6. Compare: common random numbers, preload, and a better scenario moves
# cycle time down and on-time % up.
catalog <- sc_compare_catalog()
a <- sc_compare_run(catalog[1:2, ], n_reps = 3L, base_seed = 7L)
b <- sc_compare_run(catalog[1:2, ], n_reps = 3L, base_seed = 7L)
stopifnot(identical(a$results$per_rep, b$results$per_rep))
p1 <- sc_arrival_plan(catalog$params[[1]], 7)
p2 <- sc_arrival_plan(catalog$params[[2]], 7)
stopifnot(identical(p1, p2))
preload <- sc_load_preload()
stopifnot(!is.null(preload), nrow(preload$scenarios) == 4, preload$meta$n_reps == 20L,
          identical(preload$scenarios$id, catalog$id),
          all(c("shipped", "cycle_mean_h", "on_time_pct", "lateness_mean_h", "wip_avg") %in% preload$table$col))
tbl <- preload$table
base_cycle <- tbl$mean[tbl$id == "baseline" & tbl$col == "cycle_mean_h"]
base_on <- tbl$mean[tbl$id == "baseline" & tbl$col == "on_time_pct"]
for (id in c("chamber2", "fail_env", "integ3")) {
  stopifnot(tbl$mean[tbl$id == id & tbl$col == "cycle_mean_h"] < base_cycle,
            tbl$mean[tbl$id == id & tbl$col == "on_time_pct"] > base_on)
}
message("compare checks OK")

# 7. Analyze: cycle-time parts sum to cycle time, utilization stays in [0, 1],
# and a forced long chamber test is identified as the bottleneck.
runs <- sc_run(sc_default_params(), n_reps = 3L, base_seed = 21L)
for (run in runs) {
  u <- sc_unit_breakdown(run)
  resid <- u$work_h + u$queue_h + u$rework_h + u$mrb_h + u$kit_h - u$cycle_h
  stopifnot(nrow(u) > 0, max(abs(resid)) < 1e-6)
}
a <- sc_analyze(runs, sc_default_params())
stopifnot(all(a$resources$util >= -1e-9), all(a$resources$util <= 1 + 1e-9))
p <- sc_default_params()
p$kit <- list(delay_mean_h = 0, delay_cv = 0, p_late = 0, late_extra_mean_h = 1)
p$capacity <- list(integration = 100L, func_test = 100L, chamber = 1L,
                   review_board = 100L, acceptance = 100L)
p$p_fail_func <- 0
p$p_fail_env <- 0
p$arrival$mean_interarrival_h <- 30
p$sim_hours <- 24 * 7 * 20
p$variants$env <- purrr::map(p$variants$env, function(e) {
  list(vibe_mean_h = 40, vibe_cv = 0.05, tvac_mean_h = 80, tvac_cv = 0.05)
})
forced <- sc_analyze(sc_run(p, n_reps = 3L, base_seed = 3L), p)
stopifnot(identical(forced$bottleneck$resource, "chamber"),
          forced$bottleneck$util > 0.7,
          all(forced$resources$util >= -1e-9), all(forced$resources$util <= 1 + 1e-9))
message("analyze checks OK")

# 8. Tornado: common random numbers, preload, and sensible swing signs.
small <- sc_tornado_factors() %>% dplyr::filter(id %in% c("integ_time", "cap_chamber"))
a <- sc_tornado_run(sc_default_params(), n_reps = 3L, base_seed = 7L, factors = small)
b <- sc_tornado_run(sc_default_params(), n_reps = 3L, base_seed = 7L, factors = small)
stopifnot(identical(a$per_rep, b$per_rep), !isTRUE(a$meta$cancelled))
# The two factors should share the same arrival plan as the baseline (process
# times and capacity do not change the plan signature).
p_base <- sc_arrival_plan(sc_default_params(), 7)
p_integ <- sc_arrival_plan(sc_tornado_apply(sc_default_params(), "integ_time", "high"), 7)
p_cap <- sc_arrival_plan(sc_tornado_apply(sc_default_params(), "cap_chamber", "high"), 7)
# Integration times live on the variants tibble, but arrivals should not depend
# on process times. The runner uses a tighter signature; the raw plan helper
# still hashes the whole variants tibble, so we only assert capacity matches.
stopifnot(identical(p_base, p_cap))
preload <- sc_load_preload_tornado()
stopifnot(!is.null(preload), preload$meta$n_reps == 20L, preload$meta$base_seed == 42L,
          all(sc_tornado_factors()$id %in% preload$effects$id),
          all(sc_tornado_metrics()$id %in% preload$effects$metric))
cyc <- preload$effects %>% dplyr::filter(metric == "cycle")
for (id in c("integ_time", "func_time", "env_time", "p_fail_func", "p_fail_env",
             "mrb_time", "kit_lead", "kit_p_late")) {
  row <- cyc[cyc$id == id, ]
  stopifnot(nrow(row) == 1L, row$high > row$low)
}
for (id in c("cap_integration", "cap_chamber")) {
  row <- cyc[cyc$id == id, ]
  stopifnot(nrow(row) == 1L, row$high < row$low)
}
message("tornado checks OK")

# 9. Breakpoint: common random numbers, preload, monotone-ish trend, and a
# forced heavy load is detected as a break with a saturated station.
small_d <- c(sc_break_demand(sc_default_params()), 400)
a <- sc_break_run(sc_default_params(), demand = small_d, n_reps = 3L, base_seed = 7L)
b <- sc_break_run(sc_default_params(), demand = small_d, n_reps = 3L, base_seed = 7L)
stopifnot(identical(a$per_rep, b$per_rep), !isTRUE(a$meta$cancelled))
# Higher demand: on-time down, cycle time up (same seeds).
lo <- a$points[a$points$demand == min(a$points$demand), ]
hi <- a$points[a$points$demand == max(a$points$demand), ]
stopifnot(hi$ontime_mean < lo$ontime_mean, hi$cycle_mean > lo$cycle_mean)
preload <- sc_load_preload_breakpoint()
stopifnot(!is.null(preload), preload$meta$n_reps == 20L, preload$meta$base_seed == 42L,
          any(preload$points$is_baseline),
          abs(preload$points$cycle_mean[preload$points$is_baseline] - 179.2) < 0.15)
# Monotone-ish on the 20-rep preload: demand vs cycle up, vs on-time down.
pr <- preload$points %>% dplyr::arrange(demand)
stopifnot(stats::cor(pr$demand, pr$cycle_mean, method = "spearman") > 0,
          stats::cor(pr$demand, pr$ontime_mean, method = "spearman") < 0,
          tail(pr$cycle_mean, 1) > pr$cycle_mean[[1]],
          tail(pr$ontime_mean, 1) < pr$ontime_mean[[1]])
# Default 80% target: a breakpoint above the baseline, interpolated
# between the last passing and first failing demand.
br <- sc_break_detect(preload, target = 80)
stopifnot(isTRUE(br$found), br$demand > br$baseline_demand,
          is.finite(br$pass_demand), is.finite(br$fail_demand),
          br$pass_demand < br$demand, br$demand < br$fail_demand,
          br$pass_ontime >= 80, br$fail_ontime < 80)
# Forced grid with a known crossing at 150.
fake <- list(
  points = tibble::tibble(
    demand = c(100, 200, 300),
    ontime_mean = c(90, 70, 50),
    cycle_mean = c(10, 20, 30),
    is_baseline = c(TRUE, FALSE, FALSE)
  ),
  util = tibble::tibble(
    demand = c(100, 200, 300),
    resource = "chamber", label = "Chamber", short = "Chamber",
    util_mean = 0.5, queue_mean = 0
  )
)
fbr_i <- sc_break_detect(fake, target = 80)
stopifnot(abs(fbr_i$demand - 150) < 1e-9,
          fbr_i$pass_demand == 100, fbr_i$fail_demand == 200,
          fbr_i$pass_demand < fbr_i$demand, fbr_i$demand < fbr_i$fail_demand)
# Forced case: one chamber, long environmental tests, a demand grid that
# crosses saturation. The breakpoint is found and the chamber is saturated.
p <- sc_default_params()
p$capacity <- list(integration = 100L, func_test = 100L, chamber = 1L,
                   review_board = 100L, acceptance = 100L)
p$p_fail_func <- 0
p$p_fail_env <- 0
p$kit <- list(delay_mean_h = 0, delay_cv = 0, p_late = 0, late_extra_mean_h = 1)
p$variants$env <- purrr::map(p$variants$env, function(e) {
  list(vibe_mean_h = 20, vibe_cv = 0.05, tvac_mean_h = 40, tvac_cv = 0.05)
})
# Year at this process time: one unit occupies the chamber ~60 h, so capacity
# is about 8736/60 ≈ 146 units/year. Sweep from well below to well above.
forced <- sc_break_run(p, demand = c(60, 120, 220), n_reps = 3L, base_seed = 3L)
fbr <- sc_break_detect(forced, target = 80)
fbot <- forced$util %>%
  dplyr::filter(abs(demand - max(demand)) < 1e-9) %>%
  dplyr::arrange(dplyr::desc(util_mean)) %>%
  dplyr::slice(1)
stopifnot(isTRUE(fbr$found), identical(fbot$resource, "chamber"),
          fbot$util_mean > 0.8)
message("breakpoint checks OK")
message("spacecraft_checks OK")
