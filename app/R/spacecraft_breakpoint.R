# spacecraft_breakpoint.R
# Demand sweep: raise the release rate from the current (or default) line and
# find the highest sustainable rate before mean cycle time crosses a multiple
# of the baseline, or a station or the technician pool hits a utilization cap.
# The crossing is interpolated between the last passing and first failing
# demand points. Common random numbers (the same seeds at every demand). The
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
# 3.5 times that rate (plus the exact baseline so the default line keeps
# its cycle-time match).
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
# covers cycle-time growth and station or labor saturation on the default line.
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
      shipped_mean = mean(.data$shipped, na.rm = TRUE),
      shipped_p10 = qfun(.data$shipped, 0.10),
      shipped_p90 = qfun(.data$shipped, 0.90),
      arrivals_mean = mean(.data$arrivals, na.rm = TRUE),
      labor_util_mean = mean(.data$labor_util, na.rm = TRUE),
      labor_hpu_mean = mean(.data$labor_hpu, na.rm = TRUE),
      wip_mean = mean(.data$wip_avg, na.rm = TRUE),
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

# Defaults for the cycle-time multiple and utilization cap.
sc_break_defaults <- function() list(cycle_mult = 1.5, sat_threshold = 0.90)

# Interpolate the demand where y first rises through `limit`. Returns a list
# with found, already (first point already above), demand (crossing), and the
# pass/fail grid points. Missing if the sweep never crosses.
sc_break_cross_up <- function(demand, y, limit) {
  ok <- is.finite(demand) & is.finite(y)
  demand <- demand[ok]
  y <- y[ok]
  empty <- list(found = FALSE, already = FALSE, demand = NA_real_,
                pass_demand = NA_real_, pass_y = NA_real_,
                fail_demand = NA_real_, fail_y = NA_real_)
  if (!length(demand)) return(empty)
  below <- which(y > limit)
  found <- length(below) > 0
  i_fail <- if (found) below[[1]] else NA_integer_
  fail_demand <- if (found) demand[[i_fail]] else NA_real_
  fail_y <- if (found) y[[i_fail]] else NA_real_
  i_pass <- if (found && i_fail > 1L) i_fail - 1L else NA_integer_
  can_interp <- isTRUE(found) && is.finite(i_pass) &&
    is.finite(y[[i_pass]]) && y[[i_pass]] <= limit
  pass_demand <- if (can_interp) demand[[i_pass]] else NA_real_
  pass_y <- if (can_interp) y[[i_pass]] else NA_real_
  if (can_interp) {
    d0 <- pass_demand; o0 <- pass_y
    d1 <- fail_demand; o1 <- fail_y
    cross <- if (abs(o1 - o0) < 1e-12) d1 else d0 + (limit - o0) / (o1 - o0) * (d1 - d0)
    cross <- max(min(cross, max(d0, d1)), min(d0, d1))
  } else {
    cross <- fail_demand
  }
  list(
    found = found,
    already = isTRUE(found) && !isTRUE(can_interp),
    demand = cross,
    pass_demand = pass_demand,
    pass_y = pass_y,
    fail_demand = fail_demand,
    fail_y = fail_y
  )
}

# Binding breakpoint: the earlier of a cycle-time crossing (baseline cycle
# times cycle_mult) and a utilization crossing (any resource, including the
# technician pool, at or above sat_threshold).
sc_break_detect <- function(bundle, cycle_mult = 1.5, sat_threshold = 0.90) {
  pts <- bundle$points %>% dplyr::arrange(.data$demand)
  i <- which(is.finite(pts$demand) & pts$is_baseline)
  i <- if (length(i)) i[[1]] else 1L
  baseline_cycle <- pts$cycle_mean[[i]]
  baseline_demand <- pts$demand[[i]]
  baseline_shipped <- pts$shipped_mean[[i]]
  cycle_limit <- as.numeric(baseline_cycle) * as.numeric(cycle_mult)
  cyc <- sc_break_cross_up(pts$demand, pts$cycle_mean, cycle_limit)
  util_by <- bundle$util %>%
    dplyr::group_by(.data$demand) %>%
    dplyr::summarise(
      util_max = max(.data$util_mean, na.rm = TRUE),
      sat_label = .data$label[which.max(.data$util_mean)],
      sat_resource = .data$resource[which.max(.data$util_mean)],
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$demand)
  sat <- sc_break_cross_up(util_by$demand, util_by$util_max, sat_threshold)
  # First resource to hit the cap at the first failing sat demand.
  sat_row <- if (isTRUE(sat$found) && is.finite(sat$fail_demand)) {
    bundle$util %>%
      dplyr::filter(abs(.data$demand - sat$fail_demand) < 1e-9) %>%
      dplyr::arrange(dplyr::desc(.data$util_mean)) %>%
      dplyr::slice(1)
  } else {
    NULL
  }
  candidates <- c(
    cycle = if (isTRUE(cyc$found)) cyc$demand else Inf,
    util = if (isTRUE(sat$found)) sat$demand else Inf
  )
  found <- any(is.finite(candidates) & candidates < Inf)
  if (found) {
    bind_name <- names(candidates)[which.min(candidates)]
    # If both land on essentially the same demand, call it both.
    if (isTRUE(cyc$found) && isTRUE(sat$found) &&
        abs(cyc$demand - sat$demand) < 1e-6 * max(1, cyc$demand, sat$demand)) {
      bind_name <- "both"
    }
    demand <- min(candidates)
  } else {
    bind_name <- NA_character_
    demand <- NA_real_
  }
  already <- (isTRUE(cyc$already) && (!isTRUE(sat$found) || cyc$demand <= sat$demand + 1e-9)) ||
    (isTRUE(sat$already) && (!isTRUE(cyc$found) || sat$demand <= cyc$demand + 1e-9))
  at <- if (found) {
    if (is.finite(cyc$fail_demand) && (identical(bind_name, "cycle") || identical(bind_name, "both"))) {
      cyc$fail_demand
    } else if (is.finite(sat$fail_demand)) {
      sat$fail_demand
    } else {
      max(pts$demand)
    }
  } else {
    max(pts$demand)
  }
  busiest <- bundle$util %>%
    dplyr::filter(abs(.data$demand - at) < 1e-9) %>%
    dplyr::arrange(dplyr::desc(.data$util_mean)) %>%
    dplyr::slice(1)
  list(
    cycle_mult = as.numeric(cycle_mult),
    sat_threshold = as.numeric(sat_threshold),
    cycle_limit = cycle_limit,
    found = found,
    already_below = already,
    reason = bind_name,
    demand = demand,
    pass_demand = cyc$pass_demand,
    pass_cycle = cyc$pass_y,
    fail_demand = if (identical(bind_name, "util")) sat$fail_demand else cyc$fail_demand,
    fail_cycle = if (identical(bind_name, "util")) {
      pts$cycle_mean[which.min(abs(pts$demand - sat$fail_demand))]
    } else cyc$fail_y,
    cycle_cross = cyc,
    util_cross = sat,
    cycle = if (found) {
      pts$cycle_mean[which.min(abs(pts$demand - (if (identical(bind_name, "util")) sat$fail_demand else cyc$fail_demand)))]
    } else NA_real_,
    baseline_demand = baseline_demand,
    baseline_cycle = baseline_cycle,
    baseline_shipped = baseline_shipped,
    last_demand = max(pts$demand),
    last_cycle = pts$cycle_mean[pts$demand == max(pts$demand)][[1]],
    last_shipped = pts$shipped_mean[pts$demand == max(pts$demand)][[1]],
    busiest = busiest,
    sat_found = isTRUE(sat$found),
    sat_demand = if (isTRUE(sat$found)) sat$demand else NA_real_,
    sat_station = if (!is.null(sat_row) && nrow(sat_row)) sat_row$label[[1]] else NA_character_,
    sat_util = if (!is.null(sat_row) && nrow(sat_row)) sat_row$util_mean[[1]] else NA_real_
  )
}

sc_break_story <- function(bundle, cycle_mult = 1.5, sat_threshold = 0.90) {
  if (is.null(bundle$points) || !nrow(bundle$points)) {
    return("Run a demand sweep to fill this summary.")
  }
  br <- sc_break_detect(bundle, cycle_mult, sat_threshold)
  demand_txt <- function(x) sprintf("%.0f units/year", x)
  bot <- br$busiest
  bot_name <- if (nrow(bot)) tolower(bot$label[[1]]) else "the busiest station"
  bot_util <- if (nrow(bot)) sprintf("%.0f%%", 100 * bot$util_mean[[1]]) else "unknown"
  bot_q <- if (nrow(bot)) bot$queue_mean[[1]] else 0
  q_txt <- if (isTRUE(bot_q < 0.05)) "almost no queue" else
    sprintf("an average queue of %.1f units", bot_q)
  sat_pct <- sprintf("%.0f%%", 100 * sat_threshold)
  sat_txt <- if (isTRUE(br$sat_found)) {
    sprintf(
      " The first resource to hit the %s busy cap is %s, reaching %.0f%% busy at about %s.",
      sat_pct, tolower(br$sat_station), 100 * br$sat_util, demand_txt(br$sat_demand)
    )
  } else {
    sprintf(" In this sweep no station or the technician pool reaches %s busy; at the top of the range the busiest is the %s (%s busy, %s).",
            sat_pct, bot_name, bot_util, q_txt)
  }
  limit_txt <- sprintf("%.1f× baseline cycle time (%.0f h)", br$cycle_mult, br$cycle_limit)
  marks <- " The dashed vertical line is the first binding limit; on the cycle-time chart the dashed horizontal line is the cycle-time cap, and on the saturation chart it is the utilization cap."
  if (!isTRUE(br$found)) {
    return(paste0(
      "The line stays within the ", limit_txt, " and under the ", sat_pct,
      " utilization cap across this sweep, up to about ",
      demand_txt(br$last_demand), " (mean cycle time ",
      sprintf("%.0f h", br$last_cycle), ", throughput ",
      sprintf("%.0f units", br$last_shipped), ").", sat_txt, marks
    ))
  }
  if (isTRUE(br$already_below)) {
    start_demand <- if (is.finite(br$fail_demand)) br$fail_demand else br$baseline_demand
    start_cycle <- if (is.finite(br$fail_cycle)) br$fail_cycle else br$baseline_cycle
    at_base <- is.finite(br$baseline_demand) && abs(start_demand - br$baseline_demand) < 1e-9
    where <- if (isTRUE(at_base)) {
      paste0("at the baseline rate of ", demand_txt(start_demand))
    } else {
      paste0("at the start of this sweep (", demand_txt(start_demand), ")")
    }
    return(paste0(
      "The line already sits outside the ", limit_txt, " or the ", sat_pct,
      " utilization cap ", where, " (mean cycle time ",
      sprintf("%.0f h", start_cycle), "). Cycle time and congestion keep rising, ",
      "to about ", sprintf("%.0f h", br$last_cycle), " at ",
      demand_txt(br$last_demand), ".", sat_txt, marks
    ))
  }
  reason_txt <- switch(
    br$reason %||% "",
    cycle = "mean cycle time crossing the cap",
    util = "utilization hitting the cap",
    both = "mean cycle time and utilization reaching their caps together",
    "the first binding limit"
  )
  cycle_pair <- if (is.finite(br$pass_cycle) && is.finite(br$fail_cycle)) {
    sprintf(" (%.0f h at %.0f, %.0f h at %.0f)",
            br$pass_cycle, br$pass_demand, br$fail_cycle,
            if (is.finite(br$fail_demand)) br$fail_demand else br$demand)
  } else ""
  paste0(
    "The line holds the ", limit_txt, " and stays under the ", sat_pct,
    " utilization cap up to about ", demand_txt(br$demand),
    ", where the first binding limit is ", reason_txt, cycle_pair,
    ". Beyond that, mean cycle time continues toward ",
    sprintf("%.0f h", br$last_cycle), " at ", demand_txt(br$last_demand), ".",
    sat_txt, marks
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
