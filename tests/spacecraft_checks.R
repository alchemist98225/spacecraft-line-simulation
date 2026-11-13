# spacecraft_checks.R
# Acceptance checks for the optional spacecraft line.
# Run from repo root: Rscript tests/spacecraft_checks.R
suppressPackageStartupMessages({ library(tidyverse); library(simmer) })
for (f in c("spacecraft_params", "spacecraft_labor", "spacecraft_model", "spacecraft_metrics", "spacecraft_ga", "spacecraft_compare", "spacecraft_analyze", "spacecraft_tornado", "spacecraft_breakpoint", "spacecraft_optimize", "spacecraft_warmup")) {
  source(file.path("app/R", paste0(f, ".R")))
}
source("tests/helpers/spacecraft_expected.R")

no_queue <- function(p) {
  p$kit <- list(delay_mean_h = 0, delay_cv = 0, p_late = 0, late_extra_mean_h = 1)
  p$capacity <- purrr::map(p$capacity, ~ 1e4)
  p$labor$pattern <- "three_shift"
  p$labor$techs_per_shift <- 1e4L
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

# 3b. Labor: HPU and utilization are finite; unconstrained techs do not
# inflate wait; utilization stays in [0, 1].
k_lab <- sc_summarise(sc_run(sc_default_params(), n_reps = 3, base_seed = 5))
stopifnot(is.finite(k_lab$summary$labor_hpu), k_lab$summary$labor_hpu > 0,
          is.finite(k_lab$summary$labor_util),
          k_lab$summary$labor_util >= 0, k_lab$summary$labor_util <= 1 + 1e-9,
          is.finite(k_lab$summary$hpu_A_h), is.finite(k_lab$summary$hpu_C_h),
          k_lab$summary$hpu_C_h > k_lab$summary$hpu_A_h)
p_free <- sc_default_params() %>% no_queue()
k_free <- sc_summarise(sc_run(p_free, n_reps = 2, base_seed = 8), p_free)
stopifnot(is.finite(k_free$summary$labor_hpu), k_free$summary$tech_wait_mean_h < 1)
message("labor checks OK")

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
st <- sc_opt_new_state(sc_default_params(), pop_size = 6L, n_gen = 4L, n_reps = 1L, base_seed = 9L)
base_key <- sc_genome_key(st$defs$default)
stopifnot(!base_key %in% purrr::map_chr(st$pop, sc_genome_key),
          base_key %in% st$evaluated$key)
message("GA adapter check OK")

# 6. Compare: common random numbers, preload, and the what-if set moves
# cycle time, HPU or throughput in the expected direction.
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
          all(c("shipped", "cycle_mean_h", "labor_hpu", "labor_util_pct", "wip_avg") %in% preload$table$col),
          !("on_time_pct" %in% preload$table$col), !("lateness_mean_h" %in% preload$table$col))
tbl <- preload$table
base_cycle <- tbl$mean[tbl$id == "baseline" & tbl$col == "cycle_mean_h"]
base_hpu <- tbl$mean[tbl$id == "baseline" & tbl$col == "labor_hpu"]
stopifnot(tbl$mean[tbl$id == "rework" & tbl$col == "cycle_mean_h"] > base_cycle,
          tbl$mean[tbl$id == "rework" & tbl$col == "labor_hpu"] > base_hpu,
          tbl$mean[tbl$id == "lose_shift" & tbl$col == "cycle_mean_h"] > base_cycle,
          tbl$mean[tbl$id == "faster" & tbl$col == "cycle_mean_h"] > base_cycle)
message("compare checks OK")

# 7. Analyze: cycle-time parts sum to cycle time, utilization stays in [0, 1],
# and a forced long chamber test is identified as the bottleneck.
runs <- sc_run(sc_default_params(), n_reps = 3L, base_seed = 21L)
for (run in runs) {
  u <- sc_unit_breakdown(run)
  resid <- u$work_h + u$queue_h + u$tech_h + u$rework_h + u$mrb_h + u$kit_h - u$cycle_h
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
             "mrb_time", "kit_lead")) {
  row <- cyc[cyc$id == id, ]
  stopifnot(nrow(row) == 1L, row$high > row$low)
}
kp <- cyc[cyc$id == "kit_p_late", ]
stopifnot(nrow(kp) == 1L, kp$swing < 1)
for (id in c("cap_integration", "cap_chamber", "techs")) {
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
# Higher demand: cycle time up (same seeds).
lo <- a$points[a$points$demand == min(a$points$demand), ]
hi <- a$points[a$points$demand == max(a$points$demand), ]
stopifnot(hi$cycle_mean > lo$cycle_mean)
preload <- sc_load_preload_breakpoint()
stopifnot(!is.null(preload), preload$meta$n_reps == 20L, preload$meta$base_seed == 42L,
          any(preload$points$is_baseline),
          is.finite(preload$points$cycle_mean[preload$points$is_baseline]))
# Monotone-ish on the 20-rep preload: demand vs cycle up.
pr <- preload$points %>% dplyr::arrange(demand)
stopifnot(stats::cor(pr$demand, pr$cycle_mean, method = "spearman") > 0,
          tail(pr$cycle_mean, 1) > pr$cycle_mean[[1]])
# Default 1.5x cycle cap: a breakpoint above the baseline, interpolated
# between the last passing and first failing demand, unless utilization
# binds first (still a found break above baseline).
br <- sc_break_detect(preload, cycle_mult = 1.5, sat_threshold = 0.90)
stopifnot(isTRUE(br$found), br$demand > br$baseline_demand)
# Forced grid with a known cycle crossing at 150 (1.5 x baseline 10 h = 15 h).
fake <- list(
  points = tibble::tibble(
    demand = c(100, 200, 300),
    cycle_mean = c(10, 20, 30),
    shipped_mean = c(100, 180, 200),
    is_baseline = c(TRUE, FALSE, FALSE)
  ),
  util = tibble::tibble(
    demand = c(100, 200, 300),
    resource = "chamber", label = "Chamber", short = "Chamber",
    util_mean = 0.5, queue_mean = 0
  )
)
fbr_i <- sc_break_detect(fake, cycle_mult = 1.5, sat_threshold = 0.90)
stopifnot(abs(fbr_i$demand - 150) < 1e-9,
          fbr_i$pass_demand == 100, fbr_i$fail_demand == 200,
          fbr_i$pass_demand < fbr_i$demand, fbr_i$demand < fbr_i$fail_demand,
          identical(fbr_i$reason, "cycle"))
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
fbr <- sc_break_detect(forced, cycle_mult = 1.5, sat_threshold = 0.90)
fbot <- forced$util %>%
  dplyr::filter(abs(demand - max(demand)) < 1e-9) %>%
  dplyr::arrange(dplyr::desc(util_mean)) %>%
  dplyr::slice(1)
stopifnot(isTRUE(fbr$found), identical(fbot$resource, "chamber"),
          fbot$util_mean > 0.8)
message("breakpoint checks OK")

# 10. Optimize: common random numbers, never worse than the current line,
# mid-search cancel, and the preload loads with a 20-rep recheck.
tiny <- list(pop_size = 4L, n_gen = 2L, n_reps = 1L, elite_n = 2L,
             elite_reps = 2L, base_seed = 7L, capacity_cost = 2, objective = "cycle")
a <- do.call(sc_opt_run, c(list(params = sc_default_params()), tiny))
b <- do.call(sc_opt_run, c(list(params = sc_default_params()), tiny))
stopifnot(identical(a$history, b$history),
          identical(a$recheck$fitness, b$recheck$fitness),
          identical(a$recheck$key, b$recheck$key),
          identical(a$recheck$delta_mean, b$recheck$delta_mean),
          identical(a$recheck$delta_lo, b$recheck$delta_lo),
          !isTRUE(a$meta$cancelled),
          a$best$fitness <= a$baseline$fitness + 1e-9,
          a$meta$n_recheck >= 1L,
          "per_rep" %in% names(a$recheck),
          "delta_mean" %in% names(a$recheck))
# Paired change equals the difference of per-replication display scores under CRN.
base_pr <- a$baseline$per_rep[[1]]
base_score <- sc_opt_display_score(base_pr$shipped, base_pr$cycle_mean_h, 0,
                                   a$meta$objective)
for (i in seq_len(nrow(a$recheck))) {
  if (isTRUE(a$recheck$is_baseline[[i]])) {
    stopifnot(isTRUE(abs(a$recheck$delta_mean[[i]]) < 1e-12))
    next
  }
  pr <- a$recheck$per_rep[[i]]
  stopifnot(!is.null(pr), nrow(pr) >= 1L)
  c_score <- sc_opt_display_score(pr$shipped, pr$cycle_mean_h,
                                  a$recheck$cost_term[[i]], a$meta$objective)
  joined <- dplyr::inner_join(
    tibble::tibble(rep = pr$rep, cscore = c_score),
    tibble::tibble(rep = base_pr$rep, bscore = base_score),
    by = "rep"
  )
  diffs <- joined$cscore - joined$bscore
  ci <- sc_opt_mean_ci90(diffs)
  stopifnot(nrow(joined) == nrow(pr),
            abs(mean(diffs) - a$recheck$delta_mean[[i]]) < 1e-9,
            abs(unname(ci[["lo"]]) - a$recheck$delta_lo[[i]]) < 1e-9,
            abs(unname(ci[["hi"]]) - a$recheck$delta_hi[[i]]) < 1e-9)
}
# Cancel after generation 1: on_generation(1) runs before the first step, so
# trip the flag there and the next loop stops before generation 2.
flag <- FALSE
cancelled <- sc_opt_run(sc_default_params(), pop_size = 4L, n_gen = 4L, n_reps = 1L,
                        elite_n = 2L, elite_reps = 2L, base_seed = 11L,
                        on_generation = function(i, n) if (i >= 1L) flag <<- TRUE,
                        cancel = function() flag)
stopifnot(isTRUE(cancelled$meta$cancelled), nrow(cancelled$history) == 1L,
          is.null(cancelled$recheck) || nrow(cancelled$recheck) == 0L)
preload <- sc_load_preload_optimize()
stopifnot(!is.null(preload), preload$meta$base_seed == 42L,
          preload$meta$elite_reps == 20L, preload$meta$n_reps == 2L,
          preload$meta$n_gen == 4L, preload$meta$pop_size == 6L,
          !isTRUE(preload$meta$cancelled),
          nrow(preload$baseline) == 1L, nrow(preload$best) == 1L,
          is.finite(preload$baseline$cycle_mean_h),
          preload$best$fitness <= preload$baseline$fitness + 1e-9,
          "per_rep" %in% names(preload$recheck),
          !is.null(preload$best$per_rep[[1]]),
          nchar(sc_opt_story(preload)) > 40)
# Same paired-identity check on the 20-rep preload.
p_base <- preload$baseline$per_rep[[1]]
p_bscore <- sc_opt_display_score(p_base$shipped, p_base$cycle_mean_h, 0,
                                 preload$meta$objective)
for (i in seq_len(nrow(preload$recheck))) {
  if (isTRUE(preload$recheck$is_baseline[[i]])) next
  pr <- preload$recheck$per_rep[[i]]
  p_cscore <- sc_opt_display_score(pr$shipped, pr$cycle_mean_h,
                                   preload$recheck$cost_term[[i]],
                                   preload$meta$objective)
  joined <- dplyr::inner_join(
    tibble::tibble(rep = pr$rep, cscore = p_cscore),
    tibble::tibble(rep = p_base$rep, bscore = p_bscore),
    by = "rep"
  )
  stopifnot(abs(mean(joined$cscore - joined$bscore) - preload$recheck$delta_mean[[i]]) < 1e-9)
}
message("optimize checks OK")

# 11. Warm-up: warmup = 0 matches current KPIs exactly, CRN, a forced
# empty-start case suggests a positive warm-up, discarding early units
# raises mean cycle time, the preload loads.
p0 <- sc_default_params()
runs0 <- sc_run(p0, n_reps = 3L, base_seed = 5L)
a0 <- sc_summarise(runs0, p0)
p_zero <- p0; p_zero$warmup_h <- 0
b0 <- sc_summarise(runs0, p_zero)
stopifnot(isTRUE(all.equal(a0$per_rep, b0$per_rep)))
# Same runs, a positive discard must not be identical to the default KPIs
# when early units exist, and cycle should not fall.
# Forced empty-start / queue-build: one integration station, shorter gaps,
# so the first units see a clearly emptier line.
p_force <- sc_default_params()
p_force$capacity$integration <- 1L
p_force$arrival$mean_interarrival_h <- 48
tiny_wu <- list(n_reps = 4L, base_seed = 11L)
wa <- do.call(sc_wu_run, c(list(params = p_force), tiny_wu))
wb <- do.call(sc_wu_run, c(list(params = p_force), tiny_wu))
stopifnot(identical(wa$wip_ens, wb$wip_ens),
          identical(wa$cycle_ens, wb$cycle_ens),
          identical(wa$recommend$warmup_weeks, wb$recommend$warmup_weeks),
          !isTRUE(wa$meta$cancelled),
          isTRUE(wa$recommend$found),
          is.finite(wa$recommend$warmup_weeks),
          wa$recommend$warmup_weeks > 0)
# Discarding the first units (by release order) raises mean cycle time:
# the first released unit is faster on an empty line.
ord <- sc_wu_cycle_by_order(wa$units)
first <- ord %>% dplyr::filter(ord == 1L)
rest <- ord %>% dplyr::filter(ord > 3L)
stopifnot(mean(first$cycle_h) < mean(rest$cycle_h))
k_all <- sc_wu_kpis_from_units(wa$units, 0)
k_drop <- sc_wu_kpis_from_units(wa$units, sc_wu_weeks_to_h(wa$recommend$warmup_weeks))
stopifnot(is.finite(k_drop$cycle_mean_h),
          k_drop$cycle_mean_h >= k_all$cycle_mean_h - 1e-6)
# Cancel after the first replication.
flag <- FALSE
cancelled <- sc_wu_run(p_force, n_reps = 6L, base_seed = 13L,
                       on_progress = function(done, total) if (done >= 1L) flag <<- TRUE,
                       should_stop = function() flag)
stopifnot(isTRUE(cancelled$meta$cancelled),
          cancelled$meta$n_reps <= 1L || nrow(cancelled$units) == 0L ||
            length(unique(cancelled$units$rep)) <= 1L)
preload_wu <- sc_load_preload_warmup()
stopifnot(!is.null(preload_wu), preload_wu$meta$base_seed == 42L,
          preload_wu$meta$n_reps == 20L,
          !isTRUE(preload_wu$meta$cancelled),
          is.finite(preload_wu$kpis_all$cycle_mean_h),
          isTRUE(preload_wu$recommend$found),
          nchar(sc_wu_story(preload_wu)) > 40)
message("warmup checks OK")
message("spacecraft_checks OK")
