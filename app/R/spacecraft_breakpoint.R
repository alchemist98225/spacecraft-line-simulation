# spacecraft_breakpoint.R
# Demand sweep: raise the release rate from the current (or default) line and
# find where it breaks — where mean on-time % crosses a target, interpolated
# between the last passing and first failing demand points. Common random
# numbers (the same seeds at every demand). The
# Breakpoint page, the preload script and the tests all go through these helpers.

sc_break_year_hours <- function() 24 * 7 * 52

# Expected releases per 52-week year from the mean time between releases.
sc_break_demand <- function(params) {
  sc_break_year_hours() / params$arrival$mean_interarrival_h
}

sc_break_apply_demand <- function(params, demand_per_year) {
  p <- unserialize(serialize(params, NULL))
  p$arrival$mean_interarrival_h <- sc_break_year_hours() / as.numeric(demand_per_year)
  p
}

# Demand points for a sweep: the current baseline, then a climb to about
# 3.5 times that rate (integers, plus the exact baseline so the default
# line keeps its 179.2 h cycle-time match).
sc_break_grid <- function(params = sc_default_params(), n_up = 11L, max_mult = 3.5) {
  base <- sc_break_demand(params)
  hi <- base * max_mult
  up <- pretty(c(base, hi), n = n_up + 2L)
  up <- up[up > base * 1.02 & up <= hi * 1.01]
  # Keep a manageable count; prefer mid-range density by just taking unique
  # pretty values and the exact baseline.
  if (length(up) > n_up) {
    idx <- round(seq(1, length(up), length.out = n_up))
    up <- up[idx]
  }
  sort(unique(c(base, up)))
}

# Default preload / first-look grid: exact baseline plus a fixed climb that
# covers the on-time drop and chamber saturation on the default line.
sc_break_default_grid <- function(params = sc_default_params()) {
  base <- sc_break_demand(params)
  extra <- c(140, 160, 180, 200, 220, 240, 270, 300, 340, 380, 420)
  sort(unique(c(base, extra[extra > base + 1])))
}

sc_break_jobs <- function(params, demand = sc_break_grid(params)) {
  tibble::tibble(
    demand = as.numeric(demand),
    is_baseline = abs(demand - sc_break_demand(params)) < 1e-9,
    params = purrr::map(demand, ~ sc_break_apply_demand(params, .x))
  )
}

# Sweep every demand with seeds base_seed, base_seed+1, … . Arrival plans
# change with the release rate, but the seeds stay locked together.
# on_progress(done, total, label) and should_stop() are optional.
sc_break_run <- function(params, demand = sc_break_grid(params),
                         n_reps = 20L, base_seed = 42L,
                         on_progress = NULL, should_stop = NULL) {
  stopifnot(n_reps >= 1, length(demand) >= 2)
  t0 <- Sys.time()
  jobs <- sc_break_jobs(params, demand)
  n_jobs <- nrow(jobs)
  total <- n_jobs * n_reps
  points <- vector("list", n_jobs)
  utils <- vector("list", n_jobs)
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
      out <- sc_run_one(jobs$params[[s]], seed)
      out$rep <- r
      runs[[r]] <- out
      done <- done + 1L
      if (is.function(on_progress)) {
        on_progress(done, total, sprintf("%.0f units/year", jobs$demand[[s]]))
      }
    }
    if (cancelled) break
    sumr <- sc_summarise(runs, jobs$params[[s]])
    points[[s]] <- sumr$per_rep %>%
      dplyr::mutate(demand = jobs$demand[[s]], is_baseline = jobs$is_baseline[[s]],
                    .before = 1)
    utils[[s]] <- purrr::map_dfr(runs, function(run) {
      sc_resource_use(run) %>% dplyr::mutate(rep = run$rep, demand = jobs$demand[[s]])
    })
  }
  kept <- !purrr::map_lgl(points, is.null)
  per_rep <- if (any(kept)) dplyr::bind_rows(points[kept]) else
    tibble::tibble(demand = numeric(), is_baseline = logical())
  util_rep <- if (any(kept)) dplyr::bind_rows(utils[kept]) else
    tibble::tibble(demand = numeric(), resource = character())
  bundle <- sc_break_bundle(params, jobs$demand[kept], per_rep, util_rep)
  bundle$meta <- list(
    n_reps = as.integer(n_reps), base_seed = as.integer(base_seed),
    secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    cancelled = cancelled, source = "live"
  )
  bundle
}

sc_break_bundle <- function(params, demand, per_rep, util_rep) {
  if (nrow(per_rep) == 0) {
    return(list(params = params, demand = demand, per_rep = per_rep,
                util_rep = util_rep, points = tibble::tibble(),
                util = tibble::tibble()))
  }
  qfun <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
  points <- per_rep %>%
    dplyr::group_by(.data$demand, .data$is_baseline) %>%
    dplyr::summarise(
      n_reps = dplyr::n(),
      cycle_mean = mean(.data$cycle_mean_h, na.rm = TRUE),
      cycle_p10 = qfun(.data$cycle_mean_h, 0.10),
      cycle_p90 = qfun(.data$cycle_mean_h, 0.90),
      ontime_mean = mean(.data$on_time_pct, na.rm = TRUE),
      ontime_p10 = qfun(.data$on_time_pct, 0.10),
      ontime_p90 = qfun(.data$on_time_pct, 0.90),
      shipped_mean = mean(.data$shipped, na.rm = TRUE),
      arrivals_mean = mean(.data$arrivals, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$demand)
  util <- util_rep %>%
    dplyr::group_by(.data$demand, .data$resource, .data$label, .data$short) %>%
    dplyr::summarise(
      util_mean = mean(.data$util, na.rm = TRUE),
      util_p10 = qfun(.data$util, 0.10),
      util_p90 = qfun(.data$util, 0.90),
      queue_mean = mean(.data$queue_avg, na.rm = TRUE),
      .groups = "drop"
    )
  list(params = params, demand = demand, per_rep = per_rep, util_rep = util_rep,
       points = points, util = util)
}

# Where mean on-time crosses the target: linearly interpolate demand
# between the last passing point and the first failing point. Saturation
# is the first demand where any station's mean utilization is at least 95%.
# Both can be missing if the sweep never crosses the line.
sc_break_detect <- function(bundle, target = 80, sat_threshold = 0.95) {
  pts <- bundle$points %>% dplyr::arrange(.data$demand)
  below <- which(pts$ontime_mean < target)
  found <- length(below) > 0
  i_fail <- if (found) below[[1]] else NA_integer_
  fail_demand <- if (found) pts$demand[[i_fail]] else NA_real_
  fail_ontime <- if (found) pts$ontime_mean[[i_fail]] else NA_real_
  fail_cycle <- if (found) pts$cycle_mean[[i_fail]] else NA_real_
  i_pass <- if (found && i_fail > 1L) i_fail - 1L else NA_integer_
  can_interp <- isTRUE(found) && is.finite(i_pass) &&
    is.finite(pts$ontime_mean[[i_pass]]) && pts$ontime_mean[[i_pass]] >= target
  pass_demand <- if (can_interp) pts$demand[[i_pass]] else NA_real_
  pass_ontime <- if (can_interp) pts$ontime_mean[[i_pass]] else NA_real_
  pass_cycle <- if (can_interp) pts$cycle_mean[[i_pass]] else NA_real_
  if (can_interp) {
    d0 <- pass_demand; o0 <- pass_ontime
    d1 <- fail_demand; o1 <- fail_ontime
    cross <- if (abs(o1 - o0) < 1e-12) d1 else d0 + (target - o0) / (o1 - o0) * (d1 - d0)
    # Keep the mark inside the two grid points even if rounding drifts.
    cross <- max(min(cross, max(d0, d1)), min(d0, d1))
  } else {
    cross <- fail_demand
  }
  already <- isTRUE(found) && !isTRUE(can_interp)
  sat <- bundle$util %>%
    dplyr::arrange(.data$demand, dplyr::desc(.data$util_mean)) %>%
    dplyr::filter(.data$util_mean >= sat_threshold)
  sat_found <- nrow(sat) > 0
  sat_row <- if (sat_found) sat[1, ] else NULL
  # Busiest station at the first failing demand (or at the last point if none).
  at <- if (found) fail_demand else max(pts$demand)
  busiest <- bundle$util %>%
    dplyr::filter(abs(.data$demand - at) < 1e-9) %>%
    dplyr::arrange(dplyr::desc(.data$util_mean)) %>%
    dplyr::slice(1)
  list(
    target = as.numeric(target),
    found = found,
    already_below = already,
    demand = cross,
    pass_demand = pass_demand,
    pass_ontime = pass_ontime,
    pass_cycle = pass_cycle,
    fail_demand = fail_demand,
    fail_ontime = fail_ontime,
    fail_cycle = fail_cycle,
    ontime = fail_ontime,
    cycle = fail_cycle,
    baseline_demand = {
      i <- which(is.finite(pts$demand) & pts$is_baseline)
      pts$demand[[if (length(i)) i[[1]] else 1L]]
    },
    baseline_ontime = {
      i <- which(is.finite(pts$demand) & pts$is_baseline)
      pts$ontime_mean[[if (length(i)) i[[1]] else 1L]]
    },
    baseline_cycle = {
      i <- which(is.finite(pts$demand) & pts$is_baseline)
      pts$cycle_mean[[if (length(i)) i[[1]] else 1L]]
    },
    last_demand = max(pts$demand),
    last_ontime = pts$ontime_mean[pts$demand == max(pts$demand)][[1]],
    last_cycle = pts$cycle_mean[pts$demand == max(pts$demand)][[1]],
    busiest = busiest,
    sat_found = sat_found,
    sat_demand = if (sat_found) sat_row$demand[[1]] else NA_real_,
    sat_station = if (sat_found) sat_row$label[[1]] else NA_character_,
    sat_util = if (sat_found) sat_row$util_mean[[1]] else NA_real_
  )
}

sc_break_story <- function(bundle, target = 80) {
  if (is.null(bundle$points) || !nrow(bundle$points)) {
    return("Run a demand sweep to fill this summary.")
  }
  br <- sc_break_detect(bundle, target)
  demand_txt <- function(x) sprintf("%.0f units/year", x)
  bot <- br$busiest
  bot_name <- if (nrow(bot)) tolower(bot$label[[1]]) else "the busiest station"
  bot_util <- if (nrow(bot)) sprintf("%.0f%%", 100 * bot$util_mean[[1]]) else "unknown"
  bot_q <- if (nrow(bot)) bot$queue_mean[[1]] else 0
  q_txt <- if (isTRUE(bot_q < 0.05)) "almost no queue" else
    sprintf("an average queue of %.1f units", bot_q)
  sat_txt <- if (isTRUE(br$sat_found)) {
    sprintf(
      " The %s is the first station to saturate, reaching %.0f%% busy at about %s.",
      tolower(br$sat_station), 100 * br$sat_util, demand_txt(br$sat_demand)
    )
  } else {
    sprintf(" In this sweep no station reaches 95%% busy; at the top of the range the busiest is the %s (%s busy, %s).",
            bot_name, bot_util, q_txt)
  }
  target_txt <- sprintf("%.0f%%", target)
  if (!isTRUE(br$found)) {
    return(paste0(
      "The line stays at or above the ", target_txt, " on-time target across this sweep, ",
      "up to about ", demand_txt(br$last_demand), " (on-time ",
      sprintf("%.0f%%", br$last_ontime), ", mean cycle time ",
      sprintf("%.0f h", br$last_cycle), ").", sat_txt,
      " The dashed horizontal line is the on-time target."
    ))
  }
  if (isTRUE(br$already_below)) {
    start_demand <- if (is.finite(br$fail_demand)) br$fail_demand else br$baseline_demand
    start_ontime <- if (is.finite(br$fail_ontime)) br$fail_ontime else br$baseline_ontime
    start_cycle <- if (is.finite(br$fail_cycle)) br$fail_cycle else br$baseline_cycle
    at_base <- is.finite(br$baseline_demand) && abs(start_demand - br$baseline_demand) < 1e-9
    where <- if (isTRUE(at_base)) {
      paste0("at the baseline rate of ", demand_txt(start_demand))
    } else {
      paste0("at the start of this sweep (", demand_txt(start_demand), ")")
    }
    return(paste0(
      "The line already sits below the ", target_txt, " on-time target ",
      where, " (it is about ",
      sprintf("%.0f%%", start_ontime), " on-time, mean cycle time ",
      sprintf("%.0f h", start_cycle), "). On-time falls further as demand rises, ",
      "to about ", sprintf("%.0f%%", br$last_ontime), " at ",
      demand_txt(br$last_demand), ".", sat_txt,
      " The dashed horizontal line is the on-time target; the dashed vertical line is the start of the sweep, where the target is already missed."
    ))
  }
  paste0(
    "The line holds the ", target_txt, " on-time target up to about ",
    demand_txt(br$demand), " (",
    sprintf("%.1f%% at %.0f, %.1f%% at %.0f",
            br$pass_ontime, br$pass_demand, br$fail_ontime, br$fail_demand),
    "). Beyond that, on-time keeps falling toward ",
    sprintf("%.0f%%", br$last_ontime), " at ", demand_txt(br$last_demand), ".",
    sat_txt,
    " The dashed horizontal line is the on-time target; the dashed vertical line is where mean on-time crosses the target."
  )
}

sc_break_preload_path <- function() {
  cand <- c(
    file.path("app", "data", "preload_breakpoint.rds"),
    file.path("data", "preload_breakpoint.rds"),
    file.path("..", "data", "preload_breakpoint.rds")
  )
  hit <- cand[file.exists(cand)]
  if (length(hit)) normalizePath(hit[[1]]) else cand[[1]]
}

sc_load_preload_breakpoint <- function(path = sc_break_preload_path()) {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}
