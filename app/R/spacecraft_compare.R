# spacecraft_compare.R
# Compare a baseline with up to three what-if scenarios, using the same seeds
# (common random numbers) so a change in the result comes from the settings,
# not from a luckier arrival stream. The Compare page, the preload script and
# the tests all go through these helpers.

# Metrics shown on the Compare page, in display order. `better` says which
# way a change is an improvement, used for the delta colouring.
sc_compare_metrics <- function() {
  tibble::tribble(
    ~id,          ~label,                   ~unit,          ~col,              ~better,  ~tip,
    "throughput", "Throughput (units)",     "units",        "shipped",         "higher", "Units that finished and shipped during the run. The default horizon is one year.",
    "cycle",      "Mean cycle time (h)",    "hours",        "cycle_mean_h",    "lower",  "Average hours from release into the line to shipping, over shipped units.",
    "ontime",     "On time (%)",            "%",            "on_time_pct",     "higher", "Share of shipped units that left on or before their due date.",
    "lateness",   "Mean lateness (h)",      "hours late",   "lateness_mean_h", "lower",  "Average hours late at ship. Negative means early, so a more negative value is an improvement.",
    "wip",        "Average WIP (units)",    "units",        "wip_avg",         "lower",  "Time-average number of units in the line (released but not yet shipped or scrapped)."
  )
}

# The four illustrative scenarios that ship in the preload. Baseline is the
# default line; the other three are modest, model-supported changes.
sc_compare_catalog <- function() {
  base <- sc_default_params()
  chamber <- sc_default_params(); chamber$capacity$chamber <- 2L
  fail <- sc_default_params(); fail$p_fail_env <- 0.04
  integ <- sc_default_params(); integ$capacity$integration <- 3L
  dplyr::bind_rows(
    sc_scenario("baseline", "Baseline",
                "The default line: one environmental test chamber, two integration stations, and a 10% environmental failure rate.",
                base, is_baseline = TRUE, short = "Baseline"),
    sc_scenario("chamber2", "Second environmental test chamber",
                "Adds a second vibe and thermal-vacuum chamber so two units can test at once.",
                chamber, short = "Second chamber"),
    sc_scenario("fail_env", "Fewer environmental test failures",
                "Lowers the environmental failure rate from 10% to 4%.",
                fail, short = "Fewer env. failures"),
    sc_scenario("integ3", "Extra integration station",
                "Adds a third integration station so one more unit can be built at once.",
                integ, short = "Extra integration")
  )
}

sc_scenario <- function(id, label, blurb, params, is_baseline = FALSE, short = NULL) {
  tibble::tibble(
    id = id, label = label, short = short %||% label, blurb = blurb,
    params = list(params), is_baseline = is_baseline
  )
}

# Arrival, kit and mix settings that the pre-drawn plan depends on.
sc_plan_signature <- function(params) {
  list(params$sim_hours, params$arrival, params$variants, params$kit)
}

# One comparison: every scenario is run with the same base_seed, base_seed+1, …
# Arrival plans are reused when those settings match, so the random numbers
# stay locked together. on_progress(done, total, label) is optional.
# should_stop() is optional; if it returns TRUE the remaining work is skipped
# and the result has meta$cancelled = TRUE.
sc_compare_run <- function(scenarios, n_reps = 10L, base_seed = 42L,
                           on_progress = NULL, should_stop = NULL) {
  stopifnot(nrow(scenarios) >= 1, n_reps >= 1)
  t0 <- Sys.time()
  n_scen <- nrow(scenarios)
  total <- n_scen * n_reps
  plan_cache <- list()
  raw <- vector("list", n_scen)
  cancelled <- FALSE
  done <- 0L
  for (s in seq_len(n_scen)) {
    runs <- vector("list", n_reps)
    for (r in seq_len(n_reps)) {
      if (is.function(should_stop) && isTRUE(should_stop())) {
        cancelled <- TRUE
        break
      }
      seed <- as.integer(base_seed + r - 1L)
      params <- scenarios$params[[s]]
      key <- paste(I(rlang::hash(sc_plan_signature(params))), seed, sep = ".")
      if (!key %in% names(plan_cache)) plan_cache[[key]] <- sc_arrival_plan(params, seed)
      out <- sc_run_one(params, seed, plan = plan_cache[[key]])
      out$rep <- r
      runs[[r]] <- out
      done <- done + 1L
      if (is.function(on_progress)) on_progress(done, total, scenarios$label[[s]])
    }
    if (cancelled) break
    raw[[s]] <- sc_summarise(runs, scenarios$params[[s]])
    raw[[s]]$units <- NULL
  }
  kept <- !purrr::map_lgl(raw, is.null)
  results <- if (any(kept)) {
    tibble::tibble(
      id = scenarios$id[kept],
      per_rep = purrr::map(raw[kept], "per_rep"),
      summary = purrr::map(raw[kept], "summary")
    )
  } else {
    tibble::tibble(id = character(), per_rep = list(), summary = list())
  }
  list(
    scenarios = scenarios,
    results = results,
    table = sc_compare_table(scenarios[seq_len(nrow(results)), ], results),
    meta = list(
      n_reps = as.integer(n_reps), base_seed = as.integer(base_seed),
      secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
      cancelled = cancelled
    )
  )
}

# Mean and P10–P90 of each metric, plus a delta against the baseline mean.
sc_compare_table <- function(scenarios, results) {
  metrics <- sc_compare_metrics()
  baseline_id <- scenarios$id[scenarios$is_baseline][[1]] %||% scenarios$id[[1]]
  spread <- results %>%
    dplyr::mutate(spread = purrr::map(per_rep, function(pr) {
      metrics %>%
        dplyr::rename(metric = id) %>%
        dplyr::mutate(
          mean = purrr::map_dbl(.data$col, ~ mean(pr[[.x]], na.rm = TRUE)),
          p10  = purrr::map_dbl(.data$col, ~ unname(stats::quantile(pr[[.x]], 0.10, na.rm = TRUE, names = FALSE))),
          p90  = purrr::map_dbl(.data$col, ~ unname(stats::quantile(pr[[.x]], 0.90, na.rm = TRUE, names = FALSE)))
        )
    })) %>%
    dplyr::select(id, spread) %>%
    tidyr::unnest(spread) %>%
    dplyr::left_join(scenarios %>% dplyr::select(id, scenario = label, short, is_baseline), by = "id")
  base <- spread %>%
    dplyr::filter(id == baseline_id) %>%
    dplyr::select(id, "col", baseline_mean = mean)
  spread %>%
    dplyr::left_join(base %>% dplyr::select("col", baseline_mean), by = "col") %>%
    dplyr::mutate(delta = mean - baseline_mean) %>%
    sc_compare_mark_deltas()
}

# Colour a delta only when it is large compared with the baseline spread.
# The cutoff is a quarter of the baseline P10–P90 half-width, i.e. (P90-P10)/8.
# Smaller changes are left uncoloured (notable = FALSE): they sit inside the
# usual run-to-run noise. For lateness, lower (more negative / earlier) is better.
sc_compare_mark_deltas <- function(tbl) {
  base <- tbl %>%
    dplyr::filter(is_baseline) %>%
    dplyr::select("col", baseline_p10 = p10, baseline_p90 = p90)
  tbl %>%
    dplyr::select(-dplyr::any_of(c("baseline_p10", "baseline_p90", "noise", "notable"))) %>%
    dplyr::left_join(base, by = "col") %>%
    dplyr::mutate(
      noise = (baseline_p90 - baseline_p10) / 8,
      notable = !is_baseline & is.finite(delta) & abs(delta) >= noise,
      improved = dplyr::case_when(
        is_baseline | !notable ~ NA,
        better == "lower"  ~ delta < 0,
        better == "higher" ~ delta > 0,
        TRUE ~ NA
      )
    )
}

sc_preload_path <- function() {
  cand <- c(
    file.path("app", "data", "preload_scenarios.rds"),
    file.path("data", "preload_scenarios.rds"),
    file.path("..", "data", "preload_scenarios.rds")
  )
  hit <- cand[file.exists(cand)]
  if (length(hit)) normalizePath(hit[[1]]) else cand[[1]]
}

sc_load_preload <- function(path = sc_preload_path()) {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

# Plain-English list of how `new` differs from `base` (line-wide knobs only).
sc_params_changes <- function(base, new) {
  rows <- tibble::tribble(
    ~label, ~get,
    "Environmental test chambers", function(p) p$capacity$chamber,
    "Integration stations",        function(p) p$capacity$integration,
    "Functional test stands",      function(p) p$capacity$func_test,
    "Material review boards",      function(p) p$capacity$review_board,
    "Final acceptance stands",     function(p) p$capacity$acceptance,
    "Environmental failure rate",  function(p) p$p_fail_env,
    "Functional failure rate",     function(p) p$p_fail_func,
    "Failures before review",      function(p) p$max_fails,
    "Horizon (hours)",             function(p) p$sim_hours,
    "Mean time between releases",  function(p) p$arrival$mean_interarrival_h
  )
  rows %>%
    dplyr::mutate(from = purrr::map(get, ~ .x(base)), to = purrr::map(get, ~ .x(new))) %>%
    dplyr::filter(!purrr::map2_lgl(from, to, identical)) %>%
    dplyr::mutate(text = sprintf("%s: %s \u2192 %s", label, from, to)) %>%
    dplyr::pull(text)
}
