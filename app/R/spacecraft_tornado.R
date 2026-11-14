# spacecraft_tornado.R
# One-at-a-time (OAT) sensitivity around the current (or default) line.
# Each key input is moved low and high while the others stay at the baseline,
# using common random numbers so a change in the result comes from the input,
# not from a luckier arrival stream. The Tornado page, the preload script and
# the tests all go through these helpers.

sc_params_copy <- function(params) unserialize(serialize(params, NULL))

# Outputs the tornado can be drawn for. `better` is which way an increase
# is an improvement, used in the plain-English summary.
sc_tornado_metrics <- function() {
  tibble::tribble(
    ~id,          ~label,                   ~unit,   ~col,              ~better,  ~tip,
    "cycle",      "Mean cycle time (h)",    "h",     "cycle_mean_h",    "lower",  "Average hours from release into the line to shipping, over shipped units.",
    "throughput", "Throughput (units)",     "units", "shipped",         "higher", "Units that finished and shipped during the run. The default horizon is one year.",
    "hpu",        "Mean touch labor (HPU)", "h",     "labor_hpu",       "lower",  "Average technicians times touch time, summed over every step including rework, in hours per shipped unit.",
    "labor",      "Labor utilization (%)",  "%",     "labor_util_pct",  "none",   "Busy technician-hours divided by scheduled technician-hours over the run, as a percentage."
  )
}

# Key inputs to swing. Times and rates move by +/- 20% (probabilities are
# clamped to [0, 1]); station counts move by one, and never below one.
sc_tornado_factors <- function() {
  tibble::tribble(
    ~id,              ~label,                         ~short,                  ~kind,      ~target,         ~tip,
    "integ_time",     "Integration times",            "Integ. times",     "scale",    "integ",         "All Integration step times, scaled together by 20%.",
    "func_time",      "Functional test times",        "Func. times",      "scale",    "func",          "Functional test touch times, scaled together by 20%.",
    "env_time",       "Environmental test times",     "Env. times",       "scale",    "env",           "Vibration and thermal-vacuum test times, scaled together by 20%.",
    "p_fail_func",    "Functional failure rate",      "Func. fail",       "rate",     "p_fail_func",   "Share of units that fail functional test and go back to Integration.",
    "p_fail_env",     "Environmental failure rate",   "Env. fail",        "rate",     "p_fail_env",    "Share of units that fail environmental test and go to the material review board.",
    "mrb_time",       "MRB hold time",                "MRB time",         "scale",    "mrb",           "Time a unit spends with the material review board, scaled by 20%.",
    "kit_lead",       "Kitting lead time",            "Kit lead",         "kit",      "delay_mean_h",  "Average wait for a unit's long-lead parts kit after release, scaled by 20%.",
    "kit_p_late",     "Kit delay probability",        "Kit delay",      "kit",      "p_late",        "Chance of an extra kit slip, scaled by 20%.",
    "cap_integration","Integration stations",         "Integ. stations",  "capacity", "integration",   "How many units Integration can work on at once. Moved by one station.",
    "cap_chamber",    "Environmental test chambers",  "Chambers",              "capacity", "chamber",       "How many vibe and thermal-vacuum chambers there are. Moved by one station.",
    "cap_func_test",  "Functional test stands",       "Func. stands",     "capacity", "func_test",     "How many units can sit in functional test at once. Moved by one stand.",
    "cap_review",     "Material review boards",       "Review boards",         "capacity", "review_board",  "How many review boards there are. Moved by one board.",
    "techs",          "Technicians per shift",        "Techs per shift",  "labor",    "techs_per_shift","How many technicians are on the line during each working shift. Moved by one technician.",
    "mtbf",           "MTBF (all equipment)",         "MTBF",             "reliability", "mtbf_h",       "Mean operating hours between failures, scaled together by 20% for every station and the chamber.",
    "mttr",           "MTTR (all equipment)",         "MTTR",             "reliability", "mttr_h",       "Mean hours to repair a breakdown, scaled together by 20% for every station and the chamber."
  )
}

sc_tornado_scale <- function(level) {
  if (identical(level, "low")) 0.8 else if (identical(level, "high")) 1.2 else 1
}

# Numeric value of a factor at a level, for captions and the table.
sc_tornado_value <- function(params, id, level = "base") {
  fac <- sc_tornado_factors()
  fac <- fac[fac$id == id, ]
  stopifnot(nrow(fac) == 1L)
  kind <- fac$kind[[1]]
  target <- fac$target[[1]]
  if (kind == "scale") {
    return(sc_tornado_scale(level))
  }
  if (kind == "rate") {
    base <- as.numeric(params[[target]])
    if (identical(level, "base")) return(base)
    val <- base * sc_tornado_scale(level)
    return(min(1, max(0, val)))
  }
  if (kind == "kit") {
    base <- as.numeric(params$kit[[target]])
    if (identical(level, "base")) return(base)
    val <- base * sc_tornado_scale(level)
    if (identical(target, "p_late")) val <- min(1, max(0, val))
    return(val)
  }
  if (kind == "capacity") {
    base <- as.integer(params$capacity[[target]])
    if (identical(level, "base")) return(base)
    if (identical(level, "low")) return(as.integer(max(1L, base - 1L)))
    return(as.integer(base + 1L))
  }
  if (kind == "labor") {
    base <- as.integer(params$labor[[target]])
    lo <- sc_labor_max_step(params)
    if (identical(level, "base")) return(base)
    if (identical(level, "low")) return(as.integer(max(lo, base - 1L)))
    return(as.integer(base + 1L))
  }
  if (kind == "reliability") {
    return(sc_tornado_scale(level))
  }
  stop("unknown factor kind: ", kind)
}

sc_tornado_level_label <- function(params, id, level) {
  fac <- sc_tornado_factors()
  fac <- fac[fac$id == id, ]
  kind <- fac$kind[[1]]
  val <- sc_tornado_value(params, id, level)
  if (kind == "scale") {
    if (identical(level, "base")) return("baseline")
    pct <- round(abs(val - 1) * 100)
    return(sprintf("%s%d%%", if (val < 1) "\u2212" else "+", pct))
  }
  if (kind == "rate" || (kind == "kit" && identical(fac$target[[1]], "p_late"))) {
    return(sprintf("%.0f%%", 100 * val))
  }
  if (kind == "kit") {
    return(sprintf("%.0f h", val))
  }
  sprintf("%d", as.integer(val))
}

sc_tornado_apply <- function(params, id, level = c("low", "high", "base")) {
  level <- match.arg(level)
  p <- sc_params_copy(params)
  if (identical(level, "base")) return(p)
  fac <- sc_tornado_factors()
  fac <- fac[fac$id == id, ]
  stopifnot(nrow(fac) == 1L)
  kind <- fac$kind[[1]]
  target <- fac$target[[1]]
  s <- sc_tornado_scale(level)
  if (kind == "scale" && identical(target, "integ")) {
    p$variants$route <- purrr::map(p$variants$route, ~ dplyr::mutate(.x, mean_h = .data$mean_h * s))
  } else if (kind == "scale" && identical(target, "func")) {
    p$variants$func_test <- purrr::map(p$variants$func_test, function(ft) {
      ft$mean_h <- ft$mean_h * s
      ft
    })
  } else if (kind == "scale" && identical(target, "env")) {
    p$variants$env <- purrr::map(p$variants$env, function(e) {
      e$vibe_mean_h <- e$vibe_mean_h * s
      e$tvac_mean_h <- e$tvac_mean_h * s
      e
    })
  } else if (kind == "scale" && identical(target, "mrb")) {
    p$mrb$mean_h <- p$mrb$mean_h * s
  } else if (kind == "rate") {
    p[[target]] <- sc_tornado_value(params, id, level)
  } else if (kind == "kit") {
    p$kit[[target]] <- sc_tornado_value(params, id, level)
  } else if (kind == "capacity") {
    p$capacity[[target]] <- sc_tornado_value(params, id, level)
  } else if (kind == "labor") {
    p$labor[[target]] <- sc_tornado_value(params, id, level)
  } else if (kind == "reliability") {
    p$reliability$enabled <- TRUE
    for (res in sc_reliability_catalog()$resource) {
      p$reliability[[res]][[target]] <- (p$reliability[[res]][[target]] %||% 0) * s
    }
  } else {
    stop("unknown factor: ", id)
  }
  p
}

# Kit lead time and delay probability change the pre-drawn arrival plan.
# Process times, fail rates and station counts do not, so they reuse the
# baseline plan (common random numbers on arrivals).
sc_tornado_affects_plan <- function(id) {
  fac <- sc_tornado_factors()
  identical(fac$kind[fac$id == id][[1]], "kit")
}

sc_tornado_jobs <- function(params, factors = sc_tornado_factors()) {
  base <- tibble::tibble(
    id = "baseline", label = "Baseline", short = "Baseline",
    kind = "base", target = NA_character_, tip = "The unperturbed line.",
    level = "base",
    params = list(sc_params_copy(params)),
    affects_plan = FALSE
  )
  rest <- factors %>%
    tidyr::crossing(level = c("low", "high")) %>%
    dplyr::mutate(
      params = purrr::map2(.data$id, .data$level, ~ sc_tornado_apply(params, .x, .y)),
      affects_plan = purrr::map_lgl(.data$id, sc_tornado_affects_plan)
    )
  dplyr::bind_rows(base, rest)
}

# Run the baseline plus each factor at low and high, with the same seeds
# (base_seed, base_seed+1, …) and a shared arrival plan when the plan is
# unchanged. on_progress(done, total, label) and should_stop() are optional.
sc_tornado_run <- function(params, n_reps = 20L, base_seed = 42L,
                           factors = sc_tornado_factors(),
                           on_progress = NULL, should_stop = NULL) {
  stopifnot(n_reps >= 1)
  t0 <- Sys.time()
  jobs <- sc_tornado_jobs(params, factors)
  n_jobs <- nrow(jobs)
  total <- n_jobs * n_reps
  plan_cache <- list()
  points <- vector("list", n_jobs)
  cancelled <- FALSE
  done <- 0L
  for (s in seq_len(n_jobs)) {
    runs <- vector("list", n_reps)
    for (r in seq_len(n_reps)) {
      if (is.function(should_stop) && isTRUE(should_stop())) {
        cancelled <- TRUE
        break
      }
      seed <- as.integer(base_seed + r - 1L)
      p <- jobs$params[[s]]
      # Always key the cache by the settings the plan actually depends on,
      # excluding process times, so time and fail-rate swings share arrivals.
      sig <- list(p$sim_hours, p$arrival, p$commit, p$variants[, c("variant", "mix")], p$kit)
      key <- paste(I(rlang::hash(sig)), seed, sep = ".")
      if (!key %in% names(plan_cache)) plan_cache[[key]] <- sc_arrival_plan(p, seed)
      out <- sc_run_one(p, seed, plan = plan_cache[[key]])
      out$rep <- r
      runs[[r]] <- out
      done <- done + 1L
      if (is.function(on_progress)) {
        on_progress(done, total, paste(jobs$label[[s]], jobs$level[[s]]))
      }
    }
    if (cancelled) break
    sumr <- sc_summarise(runs, jobs$params[[s]])
    points[[s]] <- sumr$per_rep %>%
      dplyr::mutate(id = jobs$id[[s]], level = jobs$level[[s]], .before = 1)
  }
  kept <- !purrr::map_lgl(points, is.null)
  per_rep <- if (any(kept)) dplyr::bind_rows(points[kept]) else
    tibble::tibble(id = character(), level = character())
  bundle <- sc_tornado_bundle(params, factors, per_rep, jobs[kept, , drop = FALSE])
  bundle$meta <- list(
    n_reps = as.integer(n_reps), base_seed = as.integer(base_seed),
    secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    cancelled = cancelled, source = "live"
  )
  bundle
}

# Compact result: baseline means, per-factor effects for every metric, and
# the factor level labels used. per_rep is kept for tests; the preload may
# drop it to stay small.
sc_tornado_bundle <- function(params, factors, per_rep, jobs = NULL) {
  metrics <- sc_tornado_metrics()
  if (nrow(per_rep) == 0 || !"id" %in% names(per_rep)) {
    return(list(params = params, factors = factors, per_rep = per_rep,
                baseline = tibble::tibble(), effects = tibble::tibble()))
  }
  means <- per_rep %>%
    dplyr::group_by(.data$id, .data$level) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(metrics$col), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  base_row <- means %>% dplyr::filter(.data$id == "baseline")
  stopifnot(nrow(base_row) == 1L)
  baseline <- metrics %>%
    dplyr::mutate(mean = purrr::map_dbl(.data$col, ~ base_row[[.x]][[1]]))
  metric_info <- metrics %>% dplyr::rename(metric = id) %>% dplyr::select(metric, col, unit, better)
  effects <- means %>%
    dplyr::filter(.data$id != "baseline") %>%
    tidyr::pivot_longer(dplyr::all_of(metrics$col), names_to = "col", values_to = "value") %>%
    tidyr::pivot_wider(names_from = "level", values_from = "value") %>%
    dplyr::left_join(metric_info, by = "col") %>%
    dplyr::left_join(factors %>% dplyr::select(id, label, short, kind, tip), by = "id") %>%
    dplyr::mutate(
      base = purrr::map_dbl(.data$col, ~ baseline$mean[baseline$col == .x][[1]]),
      swing = abs(.data$high - .data$low)
    ) %>%
    dplyr::arrange(.data$metric, dplyr::desc(.data$swing), .data$label)
  levels_tbl <- factors %>%
    dplyr::mutate(
      low_label = purrr::map_chr(.data$id, ~ sc_tornado_level_label(params, .x, "low")),
      high_label = purrr::map_chr(.data$id, ~ sc_tornado_level_label(params, .x, "high")),
      base_label = purrr::map_chr(.data$id, ~ sc_tornado_level_label(params, .x, "base"))
    )
  list(
    params = params,
    factors = factors,
    levels = levels_tbl,
    per_rep = per_rep,
    baseline = baseline,
    effects = effects
  )
}

# Join the current factor and metric labels so copy tweaks do not require
# a preload rebuild.
sc_tornado_effects_now <- function(bundle) {
  fac <- sc_tornado_factors() %>% dplyr::select(id, label, short, kind, tip)
  met <- sc_tornado_metrics() %>% dplyr::rename(metric = id) %>%
    dplyr::select(metric, col, unit, better)
  bundle$effects %>%
    dplyr::select(-dplyr::any_of(c("label", "short", "kind", "tip", "unit", "better"))) %>%
    dplyr::left_join(fac, by = "id") %>%
    dplyr::left_join(met, by = c("metric", "col"))
}

sc_tornado_format_value <- function(x, metric_id) {
  if (identical(metric_id, "labor")) sprintf("%.1f%%", x)
  else if (identical(metric_id, "throughput")) sprintf("%.1f", x)
  else sprintf("%.1f h", x)
}

# Plain-English summary of the top drivers for one output.
sc_tornado_story <- function(bundle, metric_id = "cycle") {
  metrics <- sc_tornado_metrics()
  met <- metrics[metrics$id == metric_id, ]
  stopifnot(nrow(met) == 1L)
  if (is.null(bundle$effects) || !nrow(bundle$effects)) {
    return("Run a tornado to fill this summary.")
  }
  ef <- sc_tornado_effects_now(bundle) %>%
    dplyr::filter(.data$metric == metric_id) %>%
    dplyr::arrange(dplyr::desc(.data$swing))
  if (!nrow(ef)) return("Run a tornado to fill this summary.")
  levels_tbl <- bundle$levels
  level_lab <- function(id, which) levels_tbl[[paste0(which, "_label")]][levels_tbl$id == id][[1]]
  phrase <- function(row, which) {
    d <- row[[which]] - row$base
    if (!is.finite(d) || abs(d) < 1e-9) return("leaves it essentially unchanged")
    if (metric_id %in% c("cycle", "hpu")) {
      if (d > 0) sprintf("adds about %s", sc_tornado_format_value(d, metric_id))
      else sprintf("saves about %s", sc_tornado_format_value(-d, metric_id))
    } else if (identical(metric_id, "labor")) {
      if (d > 0) sprintf("raises labor utilization by about %.1f points", d)
      else sprintf("cuts labor utilization by about %.1f points", -d)
    } else {
      if (d > 0) sprintf("ships about %.1f more units", d)
      else sprintf("ships about %.1f fewer units", -d)
    }
  }
  move <- function(row, which) {
    lab <- level_lab(row$id, which)
    base_lab <- level_lab(row$id, "base")
    name <- tolower(row$label)
    if (identical(row$kind, "scale")) {
      shown <- sub("^[−+-]", "", lab)
      if (which == "high") sprintf("Raising %s by %s", name, shown)
      else sprintf("Lowering %s by %s", name, shown)
    } else if (identical(lab, base_lab)) {
      sprintf("Keeping %s at %s", name, lab)
    } else if (which == "high") {
      sprintf("Raising %s to %s", name, lab)
    } else {
      sprintf("Lowering %s to %s", name, lab)
    }
  }
  story_name <- switch(
    metric_id,
    cycle = "mean cycle time",
    throughput = "throughput",
    hpu = "mean touch labor",
    labor = "labor utilization",
    tolower(met$label[[1]])
  )
  lcfirst <- function(s) paste0(tolower(substring(s, 1, 1)), substring(s, 2))
  top <- ef[1, ]
  # Throughput barely moves on this line; don't over-claim a 1-unit swing.
  tiny <- !is.finite(top$swing) ||
    top$swing < 0.02 * abs(top$base) ||
    (identical(metric_id, "throughput") && top$swing < 2)
  if (isTRUE(tiny)) {
    return(paste0(
      "In this study, none of the inputs moved ", story_name, " much. The largest swing was about ",
      if (identical(metric_id, "throughput")) sprintf("%.1f units", top$swing) else sc_tornado_format_value(top$swing, metric_id),
      ", for ", tolower(top$label),
      ". The dashed line is the baseline (",
      sc_tornado_format_value(top$base, metric_id), ")."
    ))
  }
  first <- paste0(
    "The biggest driver of ", story_name, " is ",
    tolower(top$label), ". ",
    move(top, "high"), " ", phrase(top, "high"), "; ",
    lcfirst(move(top, "low")), " ", phrase(top, "low"), "."
  )
  next_bits <- tolower(ef$label[seq_len(min(3L, nrow(ef)))[-1]])
  next_s <- if (length(next_bits) == 0) {
    ""
  } else if (length(next_bits) == 1) {
    sprintf(" Next is %s.", next_bits)
  } else {
    sprintf(" Next are %s and %s.", next_bits[[1]], next_bits[[2]])
  }
  base_s <- sprintf(
    " The dashed line is the baseline (%s).",
    sc_tornado_format_value(ef$base[[1]], metric_id)
  )
  paste0(first, next_s, base_s)
}

sc_tornado_preload_path <- function() {
  cand <- c(
    file.path("app", "data", "preload_tornado.rds"),
    file.path("data", "preload_tornado.rds"),
    file.path("..", "data", "preload_tornado.rds")
  )
  hit <- cand[file.exists(cand)]
  if (length(hit)) normalizePath(hit[[1]]) else cand[[1]]
}

sc_load_preload_tornado <- function(path = sc_tornado_preload_path()) {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}
