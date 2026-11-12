# spacecraft_checks.R
# Acceptance checks for the optional spacecraft line.
# Run from repo root: Rscript tests/spacecraft_checks.R
suppressPackageStartupMessages({ library(tidyverse); library(simmer) })
for (f in c("spacecraft_params", "spacecraft_model", "spacecraft_metrics", "spacecraft_ga", "spacecraft_compare")) {
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
message("spacecraft_checks OK")
