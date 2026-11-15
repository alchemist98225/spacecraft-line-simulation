# spacecraft_compare.R
# Compare a baseline with a set of capacity-move what-ifs, using the same seeds
# (common random numbers) so a change in the result comes from the settings,
# not from a luckier arrival stream. The Compare page, the preload script and
# the tests all go through these helpers.

# Metrics shown on the Compare page, in display order. Throughput is first:
# each capacity move is judged by the throughput gain vs the baseline, then
# by HPU, WIP and cycle time. `better` says which way a change is an improvement.
sc_compare_metrics <- function() {
  tibble::tribble(
    ~id,          ~label,                   ~unit,          ~col,              ~better,  ~tip,
    "throughput", "Throughput (units)",     "units",        "shipped",         "higher", "Units that finished and shipped during the run. The default horizon is one year.",
    "hpu",        "Mean touch labor (HPU)", "hours",        "labor_hpu",       "lower",  "Average technicians times touch time, summed over every step including rework, in hours per shipped unit.",
    "wip",        "Average WIP (units)",    "units",        "wip_avg",         "lower",  "Time-average number of units in the line (released but not yet shipped or scrapped).",
    "cycle",      "Mean cycle time (h)",    "hours",        "cycle_mean_h",    "lower",  "Average hours from release into the line to shipping, over shipped units."
  )
}

# Precomputed what-ifs: modest capacity moves against the default line.
# The line already runs three shifts Monday–Friday, so "add a shift" is
# staffed weekends (a 7-day week) rather than a fourth daily shift.
sc_compare_catalog <- function() {
  base <- sc_default_params()
  add_sub <- sc_default_params()
  add_sub$capacity$subassembly <- add_sub$capacity$subassembly + 1L
  add_int <- sc_default_params()
  add_int$capacity$integration <- add_int$capacity$integration + 1L
  add_tech <- sc_default_params()
  add_tech$labor$techs_per_shift <- as.integer(add_tech$labor$techs_per_shift + 1L)
  add_weekends <- sc_default_params()
  add_weekends$labor$work_week <- "seven_day"
  faster_qa <- sc_default_params()
  faster_qa$qa$after_integration$mean_h <- 0.8 * faster_qa$qa$after_integration$mean_h
  faster_qa$qa$after_chamber$mean_h     <- 0.8 * faster_qa$qa$after_chamber$mean_h
  faster_qa$qa$before_acceptance$mean_h <- 0.8 * faster_qa$qa$before_acceptance$mean_h
  dplyr::bind_rows(
    sc_scenario("baseline", "Baseline",
                "The default line: eight technicians on three shifts, Monday–Friday, a 5% functional and 3% environmental failure rate, and a release every 72 hours.",
                base, is_baseline = TRUE, short = "Baseline"),
    sc_scenario("add_subassembly", "Add a subassembly station",
                "Adds one subassembly bay (3 to 4) so more units can be in build-up at once.",
                add_sub, short = "+1 subassembly"),
    sc_scenario("add_integration", "Add an integration station",
                "Adds one integration station (2 to 3) so more units can be in integration at once.",
                add_int, short = "+1 integration"),
    sc_scenario("add_tech", "Add a technician per shift",
                "Raises the shared pool from eight to nine technicians on each shift.",
                add_tech, short = "+1 tech/shift"),
    sc_scenario("add_weekends", "Staff weekends (7-day week)",
                "Keeps three shifts and staffs the technician pool seven days a week instead of Monday–Friday. The default line has no fourth daily shift to add.",
                add_weekends, short = "Staff weekends"),
    sc_scenario("faster_qa", "Faster inspection holds",
                "Shortens the three inspection and paperwork holds (after Integration, after the chamber, and before acceptance) by 20%.",
                faster_qa, short = "Faster holds")
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
  list(params$sim_hours, params$arrival, params$commit, params$variants, params$kit)
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
    raw[[s]]$flow <- sc_compare_flow(runs, scenarios$params[[s]], scenarios$label[[s]])
  }
  kept <- !purrr::map_lgl(raw, is.null)
  results <- if (any(kept)) {
    tibble::tibble(
      id = scenarios$id[kept],
      per_rep = purrr::map(raw[kept], "per_rep"),
      summary = purrr::map(raw[kept], "summary"),
      flow = purrr::map(raw[kept], "flow")
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
# usual run-to-run noise.
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
      notable = .data$notable & .data$better != "none",
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
    "Subassembly stations",        function(p) p$capacity$subassembly,
    "Integration stations",        function(p) p$capacity$integration,
    "Functional test stands",      function(p) p$capacity$func_test,
    "Material review boards",      function(p) p$capacity$review_board,
    "Final acceptance stands",     function(p) p$capacity$acceptance,
    "Environmental failure rate",  function(p) p$p_fail_env,
    "Functional failure rate",     function(p) p$p_fail_func,
    "Failures before review",      function(p) p$max_fails,
    "Horizon (hours)",             function(p) p$sim_hours,
    "Mean time between releases",  function(p) p$arrival$mean_interarrival_h,
    "Committed lead time (first unit)", function(p) p$commit$lead_h,
    "Technicians per shift",       function(p) p$labor$techs_per_shift,
    "Shift pattern",               function(p) p$labor$pattern,
    "Work week",                   function(p) p$labor$work_week,
    "Late parts delay",            function(p) if (isTRUE(p$kit$late_parts)) "on" else "off",
    "Equipment breakdowns",        function(p) if (isTRUE(p$reliability$enabled)) "on" else "off",
    "Chamber MTBF (hours)",        function(p) p$reliability$chamber$mtbf_h,
    "Chamber MTTR (hours)",        function(p) p$reliability$chamber$mttr_h,
    "Integration MTBF (hours)",    function(p) p$reliability$integration$mtbf_h,
    "Integration MTTR (hours)",    function(p) p$reliability$integration$mttr_h
  )
  rows %>%
    dplyr::mutate(from = purrr::map(get, ~ .x(base)), to = purrr::map(get, ~ .x(new))) %>%
    dplyr::filter(!purrr::map2_lgl(from, to, identical)) %>%
    dplyr::mutate(text = sprintf("%s: %s \u2192 %s", label, from, to)) %>%
    dplyr::pull(text)
}

# Click-to-toggle selection: at most two scenarios in the charts, at least one.
# The newest click is kept when a third card is added.
sc_compare_toggle_selection <- function(selected, id, catalog_ids, max_n = 2L) {
  catalog_ids <- as.character(catalog_ids)
  id <- as.character(id)
  selected <- as.character(selected)
  selected <- selected[selected %in% catalog_ids]
  if (!id %in% catalog_ids) return(selected)
  if (id %in% selected) {
    out <- setdiff(selected, id)
    if (!length(out)) out <- id
    return(unname(out))
  }
  out <- c(selected, id)
  if (length(out) > max_n) out <- tail(out, max_n)
  unname(out)
}

# Default charts: the baseline plus the capacity move with the largest
# throughput gain, when a results table is on hand; otherwise the first what-if.
sc_compare_default_selection <- function(scenarios, max_n = 2L, tbl = NULL) {
  ids <- as.character(scenarios$id)
  is_base <- scenarios$is_baseline %in% TRUE
  base <- ids[is_base]
  rest <- ids[!is_base]
  if (!is.null(tbl) && nrow(tbl) && all(c("col", "delta", "id") %in% names(tbl))) {
    gains <- tbl %>%
      dplyr::filter(.data$id %in% rest, .data$col == "shipped") %>%
      dplyr::arrange(dplyr::desc(.data$delta), .data$id)
    if (nrow(gains)) rest <- unique(c(as.character(gains$id), rest))
  }
  unname(c(base, rest)[seq_len(min(max_n, length(c(base, rest))))])
}

sc_compare_filter_table <- function(tbl, selected_ids) {
  if (is.null(tbl) || !nrow(tbl) || !length(selected_ids)) return(tbl)
  tbl %>% dplyr::filter(.data$id %in% selected_ids)
}

# Long per-replication points for the two selected scenarios (crosstalk).
# Keyed by rep so a click highlights the same common-random-number run
# in every metric chart.
sc_compare_rep_points <- function(result, selected_ids) {
  metrics <- sc_compare_metrics()
  if (is.null(result) || is.null(result$results) || is.null(result$scenarios)) {
    return(tibble::tibble())
  }
  sc <- result$scenarios %>% dplyr::filter(.data$id %in% selected_ids)
  if (!nrow(sc)) return(tibble::tibble())
  purrr::map_dfr(seq_len(nrow(sc)), function(i) {
    row <- result$results %>% dplyr::filter(.data$id == sc$id[[i]])
    if (!nrow(row) || is.null(row$per_rep[[1]])) return(NULL)
    pr <- row$per_rep[[1]]
    keep <- c("rep", metrics$col)
    keep <- keep[keep %in% names(pr)]
    pr %>%
      dplyr::select(dplyr::all_of(keep)) %>%
      dplyr::mutate(id = sc$id[[i]], short = sc$short[[i]], .before = 1)
  })
}

# Filter bound flow tables to the selected scenario labels (flow$set).
sc_compare_filter_flow <- function(flow, scenarios, selected_ids) {
  if (is.null(flow)) return(NULL)
  labs <- scenarios$label[scenarios$id %in% selected_ids]
  shorts <- scenarios$short[scenarios$id %in% selected_ids]
  want <- unique(c(as.character(labs), as.character(shorts)))
  out <- flow
  for (nm in c("wip", "demand", "queues", "downtime")) {
    if (!is.null(out[[nm]]) && "set" %in% names(out[[nm]])) {
      out[[nm]] <- out[[nm]] %>% dplyr::filter(as.character(.data$set) %in% want)
    }
  }
  if (!is.null(out$exports) && length(want)) {
    out$exports <- Filter(function(b) {
      lab <- b$set %||% if (!is.null(b$wip) && nrow(b$wip) && "set" %in% names(b$wip)) {
        as.character(b$wip$set[[1]])
      } else {
        NA_character_
      }
      lab %in% want
    }, out$exports)
  }
  out
}
