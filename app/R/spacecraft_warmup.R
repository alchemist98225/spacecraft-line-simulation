# spacecraft_warmup.R
# Warm-up check: the line starts empty, so the first units see short queues
# and the first weeks can bias cycle time and on-time %. Welch (moving-window)
# smooth of cycle time by release order and of WIP by week, a suggested
# warm-up, and the KPI change if those early units are discarded. Common
# random numbers. The Warmup page, the preload script and the tests all go
# through these helpers.

sc_wu_week_h <- function() 7 * 24

# 1-based week of a time in hours. Week 1 is [0, 168).
sc_wu_week <- function(time_h) {
  pmax(1L, as.integer(floor(as.numeric(time_h) / sc_wu_week_h()) + 1L))
}

sc_wu_n_weeks <- function(horizon) {
  as.integer(ceiling(as.numeric(horizon) / sc_wu_week_h()))
}

sc_wu_weeks_to_h <- function(weeks) as.numeric(weeks) * sc_wu_week_h()

# Odd-width moving average (Welch). The window shrinks at the ends.
sc_wu_welch_roll <- function(y, window = 5L) {
  k <- as.integer(window)
  if (k < 1L) k <- 1L
  if (k %% 2L == 0L) k <- k + 1L
  half <- (k - 1L) %/% 2L
  n <- length(y)
  vapply(seq_len(n), function(i) {
    mean(y[max(1L, i - half):min(n, i + half)], na.rm = TRUE)
  }, numeric(1))
}

# Compact unit rows used to rebuild series and post-warmup KPIs.
sc_wu_compact_units <- function(runs) {
  purrr::map_dfr(runs, function(run) {
    sc_units(run) %>%
      dplyr::transmute(
        rep = .data$rep,
        start_time = .data$start_time,
        end_time = .data$end_time,
        status = .data$status,
        cycle_h = .data$cycle_h,
        lateness_h = .data$lateness_h
      )
  })
}

# Time-average WIP in each week, one row per replication.
sc_wu_wip_weekly <- function(units, horizon) {
  n_w <- sc_wu_n_weeks(horizon)
  weeks <- seq_len(n_w)
  units %>%
    dplyr::group_by(.data$rep) %>%
    dplyr::group_modify(function(d, key) {
      ev <- dplyr::bind_rows(
        tibble::tibble(time = d$start_time, dlt = 1),
        tibble::tibble(time = d$end_time[d$status != "wip"], dlt = -1)
      ) %>%
        dplyr::arrange(.data$time) %>%
        dplyr::mutate(level = cumsum(.data$dlt))
      tibble::tibble(
        week = weeks,
        wip = vapply(weeks, function(w) {
          a <- (w - 1L) * sc_wu_week_h()
          b <- min(w * sc_wu_week_h(), horizon)
          if (b <= a) return(NA_real_)
          sc_time_average(ev$time, ev$level, horizon = b, start = a)
        }, numeric(1))
      )
    }) %>%
    dplyr::ungroup()
}

# Cycle time of shipped units by release order (1 = first released in the rep).
sc_wu_cycle_by_order <- function(units) {
  units %>%
    dplyr::filter(.data$status == "shipped", is.finite(.data$cycle_h)) %>%
    dplyr::group_by(.data$rep) %>%
    dplyr::arrange(.data$start_time, .by_group = TRUE) %>%
    dplyr::mutate(ord = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(rep, ord, start_time, cycle_h, lateness_h)
}

# Ensemble mean and P10-P90, plus a Welch smooth of the mean (and of the
# percentile bounds, so the ribbon is a smooth band).
sc_wu_ensemble <- function(rep_tbl, x, y, window = 5L) {
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  qfun <- function(v, p) as.numeric(stats::quantile(v, p, na.rm = TRUE, names = FALSE))
  rep_tbl %>%
    dplyr::group_by(!!x) %>%
    dplyr::summarise(
      n_reps = dplyr::n(),
      y_mean = mean(!!y, na.rm = TRUE),
      y_p10 = qfun(!!y, 0.10),
      y_p90 = qfun(!!y, 0.90),
      .groups = "drop"
    ) %>%
    dplyr::arrange(!!x) %>%
    dplyr::mutate(
      y_smooth = sc_wu_welch_roll(.data$y_mean, window),
      y_p10_smooth = sc_wu_welch_roll(.data$y_p10, window),
      y_p90_smooth = sc_wu_welch_roll(.data$y_p90, window),
      welch_window = as.integer(window)
    )
}

# Suggested warm-up in weeks from the WIP fill-up: first week the smoothed
# WIP has reached a fraction of the later-year mean and holds there.
# Returns 0 if week 1 is already there (no discard suggested).
sc_wu_recommend_weeks <- function(wip_ens, settle_frac = 0.10, hold_weeks = 3L,
                                  end_lo = 0.40, end_hi = 0.80) {
  d <- wip_ens %>% dplyr::arrange(.data$week)
  if (!nrow(d) || !any(is.finite(d$y_smooth))) {
    return(list(warmup_weeks = NA_real_, steady_wip = NA_real_,
                settle_frac = settle_frac, hold_weeks = as.integer(hold_weeks),
                found = FALSE))
  }
  n <- nrow(d)
  lo <- max(1L, as.integer(floor(end_lo * n)))
  hi <- min(n, as.integer(ceiling(end_hi * n)))
  if (hi < lo) {
    lo <- 1L
    hi <- n
  }
  end_mean <- mean(d$y_smooth[lo:hi], na.rm = TRUE)
  thresh <- (1 - settle_frac) * end_mean
  ok <- is.finite(d$y_smooth) & d$y_smooth >= thresh
  hold <- max(1L, as.integer(hold_weeks))
  rec <- NA_real_
  for (i in seq_len(n - hold + 1L)) {
    if (all(ok[i:(i + hold - 1L)])) {
      rec <- d$week[[i]]
      break
    }
  }
  if (!is.finite(rec)) {
    hit <- which(ok)
    if (length(hit)) rec <- d$week[[hit[[1]]]]
  }
  # Week 1 already settled => no discard. Otherwise discard the first
  # (rec - 1) weeks, i.e. keep units from week `rec` onward.
  warmup_weeks <- if (!is.finite(rec)) NA_real_ else max(0, as.numeric(rec) - 1)
  list(
    warmup_weeks = warmup_weeks,
    recommend_week = rec,
    steady_wip = end_mean,
    thresh_wip = thresh,
    settle_frac = settle_frac,
    hold_weeks = hold,
    found = is.finite(warmup_weeks)
  )
}

# Per-rep then mean KPIs from compact units, optionally discarding early
# releases. Matches sc_summarise's "mean of per-rep means" for cycle and on-time.
sc_wu_kpis_from_units <- function(units, warmup_h = 0) {
  u <- units
  if (is.finite(warmup_h) && warmup_h > 0) {
    u <- u %>% dplyr::filter(.data$start_time >= warmup_h)
  }
  ship <- u %>% dplyr::filter(.data$status == "shipped", is.finite(.data$cycle_h))
  if (!nrow(ship)) {
    return(tibble::tibble(
      n_reps = 0L, n_ship_mean = 0, cycle_mean_h = NA_real_, on_time_pct = NA_real_
    ))
  }
  per <- ship %>%
    dplyr::group_by(.data$rep) %>%
    dplyr::summarise(
      n_ship = dplyr::n(),
      cycle_mean_h = mean(.data$cycle_h, na.rm = TRUE),
      on_time_pct = 100 * mean(.data$lateness_h <= 0, na.rm = TRUE),
      .groups = "drop"
    )
  tibble::tibble(
    n_reps = nrow(per),
    n_ship_mean = mean(per$n_ship),
    cycle_mean_h = mean(per$cycle_mean_h),
    on_time_pct = mean(per$on_time_pct)
  )
}

# KPI rows at a set of discard lengths (weeks), for the page table.
sc_wu_cutoff_table <- function(units, weeks = c(0, 1, 2, 3, 4, 6, 8),
                               recommend_weeks = NA_real_) {
  weeks <- sort(unique(c(as.numeric(weeks),
                         if (is.finite(recommend_weeks)) recommend_weeks)))
  weeks <- weeks[is.finite(weeks) & weeks >= 0]
  purrr::map_dfr(weeks, function(w) {
    k <- sc_wu_kpis_from_units(units, sc_wu_weeks_to_h(w))
    k$warmup_weeks <- w
    k$is_recommend <- is.finite(recommend_weeks) && abs(w - recommend_weeks) < 1e-9
    k
  })
}

sc_wu_order_at_weeks <- function(cycle_ord, weeks) {
  if (!is.finite(weeks) || weeks <= 0 || is.null(cycle_ord) || !nrow(cycle_ord)) {
    return(NA_real_)
  }
  cut <- sc_wu_weeks_to_h(weeks)
  # Last discarded order: max order whose median start is still before the cut.
  med <- cycle_ord %>%
    dplyr::group_by(.data$ord) %>%
    dplyr::summarise(start = stats::median(.data$start_time), .groups = "drop") %>%
    dplyr::arrange(.data$ord)
  hit <- med$ord[med$start < cut]
  if (!length(hit)) return(0)
  max(hit)
}

# Finish a set of runs (or a compact bundle) into the page payload.
sc_wu_bundle <- function(params, units, horizon = params$sim_hours,
                         n_reps = NA_integer_, base_seed = NA_integer_,
                         window = 5L) {
  wip_rep <- sc_wu_wip_weekly(units, horizon)
  cycle_ord <- sc_wu_cycle_by_order(units)
  wip_ens <- sc_wu_ensemble(wip_rep, week, wip, window)
  names(wip_ens) <- sub("^y_", "wip_", names(wip_ens))
  # Keep a 'week' column; ensemble used week as x so it's already there.
  cyc_ens <- sc_wu_ensemble(cycle_ord, ord, cycle_h, window)
  names(cyc_ens) <- sub("^y_", "cycle_", names(cyc_ens))
  rec <- sc_wu_recommend_weeks(wip_ens %>% dplyr::rename(y_smooth = wip_smooth))
  k0 <- sc_wu_kpis_from_units(units, 0)
  k1 <- sc_wu_kpis_from_units(units, sc_wu_weeks_to_h(rec$warmup_weeks %||% 0))
  cuts <- sc_wu_cutoff_table(units, recommend_weeks = rec$warmup_weeks)
  rec$order_cut <- sc_wu_order_at_weeks(cycle_ord, rec$warmup_weeks)
  first <- cycle_ord %>% dplyr::filter(.data$ord == 1L)
  later <- cycle_ord %>% dplyr::filter(.data$ord >= 10L)
  rec$first_cycle_h <- if (nrow(first)) mean(first$cycle_h) else NA_real_
  rec$later_cycle_h <- if (nrow(later)) mean(later$cycle_h) else NA_real_
  list(
    params = params,
    units = units,
    wip_ens = wip_ens,
    cycle_ens = cyc_ens,
    recommend = rec,
    kpis_all = k0,
    kpis_drop = k1,
    cutoffs = cuts,
    meta = list(
      n_reps = as.integer(n_reps),
      base_seed = as.integer(base_seed),
      horizon = horizon,
      n_weeks = sc_wu_n_weeks(horizon),
      window = as.integer(window),
      cancelled = FALSE,
      source = "live"
    )
  )
}

sc_wu_story <- function(bundle) {
  rec <- bundle$recommend
  k0 <- bundle$kpis_all
  k1 <- bundle$kpis_drop
  n_re <- bundle$meta$n_reps
  weeks <- rec$warmup_weeks
  fmt_h <- function(x) sprintf("%.1f h", x)
  fmt_pct <- function(x) sprintf("%.1f%%", x)
  if (!isTRUE(rec$found) || !is.finite(weeks)) {
    return(paste0(
      "The search did not find a clear fill-up point for this line. ",
      sprintf("After %s replications with the same seeds, mean cycle time is %s and on-time is %s ",
              if (is.finite(n_re)) as.integer(n_re) else "these",
              fmt_h(k0$cycle_mean_h), fmt_pct(k0$on_time_pct)),
      "when every unit is kept. Leave the warm-up setting at 0 (the default) so the other pages keep their current numbers. Numbers are illustrative."
    ))
  }
  if (weeks <= 0) {
    return(paste0(
      "The line looks settled from the first week, so no warm-up discard is suggested. ",
      sprintf("After %d replications with the same seeds, mean cycle time is %s and on-time is %s. ",
              as.integer(n_re), fmt_h(k0$cycle_mean_h), fmt_pct(k0$on_time_pct)),
      "Leave the warm-up setting at 0 (the default) so the other pages keep their current numbers. Numbers are illustrative."
    ))
  }
  d_cycle <- k1$cycle_mean_h - k0$cycle_mean_h
  d_ontime <- k1$on_time_pct - k0$on_time_pct
  first_bit <- if (is.finite(rec$first_cycle_h) && is.finite(rec$later_cycle_h) &&
                     rec$first_cycle_h + 5 < rec$later_cycle_h) {
    sprintf(" The first unit averages %s versus %s later on.",
            fmt_h(rec$first_cycle_h), fmt_h(rec$later_cycle_h))
  } else {
    ""
  }
  small <- is.finite(d_cycle) && abs(d_cycle) < 1 && is.finite(d_ontime) && abs(d_ontime) < 1
  kpi_bit <- if (isTRUE(small)) {
    sprintf("discarding units released in the first %.0f weeks barely changes the year-long KPIs: mean cycle time is %s versus %s, and on-time is %s versus %s. Leave the warm-up setting at 0 (the default) unless you want those KPIs to ignore the short start-up.",
            weeks, fmt_h(k1$cycle_mean_h), fmt_h(k0$cycle_mean_h),
            fmt_pct(k1$on_time_pct), fmt_pct(k0$on_time_pct))
  } else if (is.finite(d_cycle) && d_cycle > 0) {
    sprintf("discarding units released in the first %.0f weeks raises mean cycle time from %s to %s, and on-time moves from %s to %s.",
            weeks, fmt_h(k0$cycle_mean_h), fmt_h(k1$cycle_mean_h),
            fmt_pct(k0$on_time_pct), fmt_pct(k1$on_time_pct))
  } else {
    sprintf("discarding units released in the first %.0f weeks moves mean cycle time from %s to %s, and on-time from %s to %s.",
            weeks, fmt_h(k0$cycle_mean_h), fmt_h(k1$cycle_mean_h),
            fmt_pct(k0$on_time_pct), fmt_pct(k1$on_time_pct))
  }
  order_bit <- if (is.finite(rec$order_cut) && rec$order_cut > 0) {
    sprintf(" That is about the first %.0f units released.", rec$order_cut)
  } else {
    ""
  }
  paste0(
    sprintf("The line starts empty, so the first units see shorter queues. WIP reaches a typical level after about %.0f weeks.%s%s ",
            weeks, order_bit, first_bit),
    sprintf("After a %d-replication check with the same seeds, %s ",
            as.integer(n_re), kpi_bit),
    if (isTRUE(small)) "" else
      "The warm-up setting on Simulate and Settings is 0 by default, so the other pages keep their current numbers unless you opt in and raise it. ",
    "Numbers are illustrative."
  )
}

sc_load_preload_warmup <- function() {
  path <- file.path("app", "data", "preload_warmup.rds")
  if (!file.exists(path)) path <- file.path("data", "preload_warmup.rds")
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

# Blocking run for preload and tests. on_progress(done, total) and
# should_stop() are optional. One replication at a time so Cancel can bite.
sc_wu_run <- function(params = sc_default_params(), n_reps = 20L, base_seed = 42L,
                      window = 5L, on_progress = NULL, should_stop = NULL) {
  t0 <- Sys.time()
  n_reps <- as.integer(n_reps)
  base_seed <- as.integer(base_seed)
  runs <- vector("list", n_reps)
  cancelled <- FALSE
  for (r in seq_len(n_reps)) {
    if (is.function(should_stop) && isTRUE(should_stop())) {
      cancelled <- TRUE
      break
    }
    out <- sc_run_one(params, seed = base_seed + r - 1L)
    out$rep <- r
    runs[[r]] <- out
    if (is.function(on_progress)) on_progress(r, n_reps)
  }
  kept <- Filter(Negate(is.null), runs)
  if (!length(kept)) {
    return(list(
      params = params, units = tibble::tibble(), wip_ens = tibble::tibble(),
      cycle_ens = tibble::tibble(), recommend = list(found = FALSE, warmup_weeks = NA_real_),
      kpis_all = tibble::tibble(), kpis_drop = tibble::tibble(),
      cutoffs = tibble::tibble(),
      meta = list(n_reps = n_reps, base_seed = base_seed, cancelled = TRUE,
                  secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
                  source = "live")
    ))
  }
  units <- sc_wu_compact_units(kept)
  bundle <- sc_wu_bundle(params, units, horizon = params$sim_hours,
                         n_reps = length(kept), base_seed = base_seed,
                         window = window)
  bundle$meta$cancelled <- cancelled
  bundle$meta$secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  bundle$meta$source <- "live"
  bundle
}

# Cooperative stepper: one replication per tick so Shiny can Cancel.
sc_wu_new_state <- function(params, n_reps = 20L, base_seed = 42L, window = 5L) {
  list(
    params = params,
    n_reps = as.integer(n_reps),
    base_seed = as.integer(base_seed),
    window = as.integer(window),
    i = 1L,
    runs = list(),
    cancelled = FALSE,
    phase = "run",
    t0 = Sys.time()
  )
}

sc_wu_step <- function(state) {
  if (!identical(state$phase, "run")) return(state)
  if (isTRUE(state$cancelled) || state$i > state$n_reps) {
    state$phase <- "done"
    return(state)
  }
  out <- sc_run_one(state$params, seed = state$base_seed + state$i - 1L)
  out$rep <- state$i
  state$runs[[state$i]] <- out
  state$i <- state$i + 1L
  if (state$i > state$n_reps) state$phase <- "done"
  state
}

sc_wu_bundle_from_state <- function(state) {
  kept <- Filter(Negate(is.null), state$runs)
  if (!length(kept)) {
    return(list(
      params = state$params, units = tibble::tibble(),
      wip_ens = tibble::tibble(), cycle_ens = tibble::tibble(),
      recommend = list(found = FALSE, warmup_weeks = NA_real_),
      kpis_all = tibble::tibble(), kpis_drop = tibble::tibble(),
      cutoffs = tibble::tibble(),
      meta = list(n_reps = 0L, base_seed = state$base_seed, cancelled = TRUE,
                  secs = as.numeric(difftime(Sys.time(), state$t0, units = "secs")),
                  source = "live")
    ))
  }
  units <- sc_wu_compact_units(kept)
  bundle <- sc_wu_bundle(state$params, units, horizon = state$params$sim_hours,
                         n_reps = length(kept), base_seed = state$base_seed,
                         window = state$window)
  bundle$meta$cancelled <- isTRUE(state$cancelled)
  bundle$meta$secs <- as.numeric(difftime(Sys.time(), state$t0, units = "secs"))
  bundle$meta$source <- "live"
  bundle
}
