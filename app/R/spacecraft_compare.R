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
# "+1 QA inspector" raises the day-shift inspector pool from two to three.
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
  add_qa_inspector <- sc_default_params()
  add_qa_inspector$qa$inspectors$n <- as.integer(add_qa_inspector$qa$inspectors$n + 1L)
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
                "Shortens the three calendar holds (Build records review, Test data review, and Customer buy-off) by 20%.",
                faster_qa, short = "Faster holds"),
    sc_scenario("add_qa_inspector", "Add a QA inspector",
                "Raises the day-shift QA inspector pool from two to three. Each hold still seizes one inspector for the touch, then the paperwork wait runs on the calendar.",
                add_qa_inspector, short = "+1 QA inspector")
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

# Plain-English list of how `new` differs from `base`. Walks nested params
# (qa$, reliability$, kit$, and the rest), not just a fixed list of
# line-wide knobs. Labels come from sc_line_fields() paths, with a few
# short overrides so existing Compare wording stays the same. The three
# inspection-hold means collapse to one line when they share a scale
# factor, e.g. "holds 232/232/136 h → -20%".
sc_params_leaf_key <- function(path) paste(path, collapse = "$")

sc_params_flatten <- function(x, prefix = character()) {
  if (is.data.frame(x)) {
    return(tibble::tibble(key = sc_params_leaf_key(prefix), value = list(x)))
  }
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms) || any(!nzchar(nms))) {
      return(tibble::tibble(key = sc_params_leaf_key(prefix), value = list(x)))
    }
    dplyr::bind_rows(purrr::imap(x, function(v, nm) sc_params_flatten(v, c(prefix, nm))))
  } else if (is.atomic(x) && !is.null(names(x)) && length(x) > 1) {
    tibble::tibble(
      key = paste(sc_params_leaf_key(prefix), names(x), sep = "$"),
      value = as.list(unname(x))
    )
  } else {
    tibble::tibble(key = sc_params_leaf_key(prefix), value = list(x))
  }
}

sc_params_format_value <- function(x) {
  if (is.null(x)) return("none")
  if (is.data.frame(x)) return("changed")
  if (is.logical(x) && length(x) == 1) return(if (isTRUE(x)) "on" else "off")
  if (is.numeric(x) && length(x) == 1 && is.finite(x)) {
    if (abs(x - round(x)) < 1e-8) {
      return(format(as.integer(round(x)), scientific = FALSE, trim = TRUE))
    }
    return(format(round(x, 4), scientific = FALSE, trim = TRUE))
  }
  if (length(x) != 1) {
    return(paste(vapply(as.list(x), sc_params_format_value, character(1)), collapse = ", "))
  }
  as.character(x)
}

# Historical short names used in the Compare differ table, plus labels for
# params that are not in sc_line_fields() (horizon total, inspector shift).
sc_params_label_overrides <- function() {
  c(
    "capacity$chamber" = "Environmental test chambers",
    "capacity$subassembly" = "Subassembly stations",
    "capacity$integration" = "Integration stations",
    "capacity$func_test" = "Functional test stands",
    "capacity$review_board" = "Material review boards",
    "capacity$acceptance" = "Final acceptance stands",
    "p_fail_env" = "Environmental failure rate",
    "p_fail_func" = "Functional failure rate",
    "max_fails" = "Failures before review",
    "sim_hours" = "Horizon (hours)",
    "arrival$mean_interarrival_h" = "Mean time between releases",
    "commit$lead_h" = "Committed lead time (first unit)",
    "labor$techs_per_shift" = "Technicians per shift",
    "labor$pattern" = "Shift pattern",
    "labor$work_week" = "Work week",
    "kit$late_parts" = "Late parts delay",
    "reliability$enabled" = "Equipment breakdowns",
    "reliability$chamber$mtbf_h" = "Chamber MTBF (hours)",
    "reliability$chamber$mttr_h" = "Chamber MTTR (hours)",
    "reliability$integration$mtbf_h" = "Integration MTBF (hours)",
    "reliability$integration$mttr_h" = "Integration MTTR (hours)",
    "qa$inspectors$shift" = "QA inspector shift",
    "variants" = "Variants"
  )
}

sc_params_label_for <- function(key) {
  ov <- sc_params_label_overrides()
  if (key %in% names(ov)) return(unname(ov[[key]]))
  fields <- sc_line_fields()
  keys <- purrr::map_chr(fields$path, function(p) if (is.null(p)) NA_character_ else sc_params_leaf_key(p))
  hit <- which(keys == key)
  if (length(hit)) return(fields$label[[hit[[1]]]])
  if (startsWith(key, "labor$step_techs$")) {
    return(sprintf("Technicians at %s", sub("^labor\\$step_techs\\$", "", key)))
  }
  key
}

sc_params_changes <- function(base, new) {
  hold_keys <- c(
    "qa$after_integration$mean_h",
    "qa$after_chamber$mean_h",
    "qa$before_acceptance$mean_h"
  )
  leaves <- dplyr::full_join(
    sc_params_flatten(base) %>% dplyr::rename(from = value),
    sc_params_flatten(new) %>% dplyr::rename(to = value),
    by = "key"
  ) %>%
    dplyr::filter(!purrr::map2_lgl(from, to, identical))

  texts <- character()
  hold <- leaves %>% dplyr::filter(key %in% hold_keys)
  if (nrow(hold) == length(hold_keys)) {
    from_n <- purrr::map_dbl(hold$from[match(hold_keys, hold$key)], as.numeric)
    to_n <- purrr::map_dbl(hold$to[match(hold_keys, hold$key)], as.numeric)
    if (all(from_n > 0) && all(is.finite(from_n)) && all(is.finite(to_n))) {
      ratios <- to_n / from_n
      if (diff(range(ratios)) < 1e-8) {
        pct <- as.integer(round((ratios[[1]] - 1) * 100))
        texts <- c(texts, sprintf(
          "holds %s h \u2192 %s",
          paste(vapply(from_n, sc_params_format_value, character(1)), collapse = "/"),
          sprintf("%+d%%", pct)
        ))
        leaves <- leaves %>% dplyr::filter(!key %in% hold_keys)
      }
    }
  }

  if (nrow(leaves)) {
    # Keep a stable order: historical knobs, then sc_line_fields, then leftovers.
    hist <- names(sc_params_label_overrides())
    field_keys <- sc_line_fields() %>%
      dplyr::filter(!purrr::map_lgl(path, is.null)) %>%
      dplyr::mutate(key = purrr::map_chr(path, sc_params_leaf_key)) %>%
      dplyr::pull(key)
    pref <- unique(c(hist, field_keys, hold_keys, leaves$key))
    leaves <- leaves %>%
      dplyr::mutate(ord = match(key, pref)) %>%
      dplyr::arrange(ord, key)
    row_text <- purrr::pmap_chr(leaves, function(key, from, to, ...) {
      lab <- sc_params_label_for(key)
      if (identical(key, "qa$inspectors$n")) {
        sprintf("%s %s \u2192 %s", lab, sc_params_format_value(from), sc_params_format_value(to))
      } else {
        sprintf("%s: %s \u2192 %s", lab, sc_params_format_value(from), sc_params_format_value(to))
      }
    })
    texts <- c(texts, row_text)
  }
  texts
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
