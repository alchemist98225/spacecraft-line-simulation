# spacecraft_ramp.R
# Capacity-ramp planner: step a target rate curve quarter by quarter, apply a
# Wright/Crawford learning curve to touch times, simulate each quarter at that
# rate with the capacity then in place, and recommend technician, shift, or
# station moves with an order-by date equal to the need date minus lead time.
# The headline check is whether annualized throughput meets the target rate
# (default 95%). Cycle time, HPU, labor utilization, station utilization and
# WIP are reported alongside. The Ramp page, the preload script and the tests
# all go through these helpers.

sc_ramp_year_hours <- function() sc_break_year_hours()

# Default 24-month ramp: 12 to 48 units/year, one row per quarter.
sc_ramp_default_schedule <- function() {
  tibble::tibble(
    quarter = 1:8,
    start_month = as.integer(c(0, 3, 6, 9, 12, 15, 18, 21)),
    target_rate = as.numeric(c(12, 16, 20, 24, 30, 36, 42, 48))
  )
}

# Lead times and learning defaults (illustrative). Settings process times are
# treated as the times at learning_ref_n cumulative units, so early units are
# slower when the curve is below 100%.
sc_ramp_defaults <- function() {
  list(
    schedule = sc_ramp_default_schedule(),
    learning_pct = 85,
    learning_ref_n = 80,
    tech_lead_months = 3,
    station_lead_months = 6,
    chamber_lead_months = 9,
    meet_frac = 0.95,
    n_reps = 10L,
    base_seed = 42L
  )
}

# A smaller early factory used by the preloaded plan so the 12-to-48 ramp
# actually needs capacity moves. Live runs can also start from current Settings.
sc_ramp_lean_params <- function(params = sc_default_params()) {
  p <- sc_params_copy(params)
  p$labor$pattern <- "day"
  p$labor$techs_per_shift <- as.integer(max(sc_labor_max_step(p), 2L))
  p$capacity$integration <- 1L
  p$capacity$func_test <- 1L
  p$capacity$chamber <- 1L
  p$capacity$review_board <- 1L
  p$capacity$acceptance <- 1L
  p
}

# Wright/Crawford exponent: T(n) = T(1) * n^b, with b = log2(learning fraction).
sc_ramp_learning_b <- function(learning_pct) {
  pct <- as.numeric(learning_pct)
  if (!is.finite(pct) || pct <= 0) pct <- 100
  log(pct / 100) / log(2)
}

# Scale for touch times at cumulative volume n, relative to the Settings
# times at learning_ref_n units. 100% (or ref_n <= 1) leaves times unchanged.
sc_ramp_learning_factor <- function(n, learning_pct = 85, learning_ref_n = 80) {
  pct <- as.numeric(learning_pct)
  ref <- as.numeric(learning_ref_n)
  if (!is.finite(pct) || pct >= 100 - 1e-9) return(1)
  if (!is.finite(ref) || ref <= 1) ref <- 1
  n <- max(1, as.numeric(n))
  (n / ref)^sc_ramp_learning_b(pct)
}

# Planned cumulative units at the start or midpoint of quarter i (1-based).
# Each quarter is three months (0.25 year).
sc_ramp_cumulative <- function(schedule, i, at = "mid") {
  rates <- as.numeric(schedule$target_rate)
  qtr_yr <- 0.25
  prior <- if (i <= 1) 0 else sum(rates[seq_len(i - 1)]) * qtr_yr
  if (identical(at, "start")) return(prior)
  prior + 0.5 * rates[[i]] * qtr_yr
}

# Scale Integration, test, MRB and acceptance touch means. Kit delay and
# fail chances are left alone (they are not learned labor).
sc_ramp_scale_touch <- function(params, factor) {
  p <- sc_params_copy(params)
  f <- as.numeric(factor)
  if (!is.finite(f) || abs(f - 1) < 1e-12) return(p)
  v <- p$variants
  v$route <- lapply(v$route, function(rt) {
    rt$mean_h <- as.numeric(rt$mean_h) * f
    rt
  })
  v$func_test <- lapply(v$func_test, function(x) {
    x$mean_h <- as.numeric(x$mean_h) * f
    x
  })
  v$env <- lapply(v$env, function(x) {
    x$vibe_mean_h <- as.numeric(x$vibe_mean_h) * f
    x$tvac_mean_h <- as.numeric(x$tvac_mean_h) * f
    x
  })
  p$variants <- v
  p$mrb$mean_h <- as.numeric(p$mrb$mean_h) * f
  p$acceptance$mean_h <- as.numeric(p$acceptance$mean_h) * f
  p
}

sc_ramp_apply_demand <- function(params, demand_per_year) {
  sc_break_apply_demand(params, demand_per_year)
}

# Sanitize an editable quarter / rate table.
sc_ramp_normalize_schedule <- function(schedule = sc_ramp_default_schedule()) {
  d <- tibble::as_tibble(schedule)
  if (!"quarter" %in% names(d)) d$quarter <- seq_len(nrow(d))
  if (!"start_month" %in% names(d)) d$start_month <- (as.integer(d$quarter) - 1L) * 3L
  if (!"target_rate" %in% names(d)) stop("schedule needs a target_rate column")
  d <- d %>%
    dplyr::mutate(
      quarter = as.integer(.data$quarter),
      start_month = as.integer(.data$start_month),
      target_rate = pmax(1, as.numeric(.data$target_rate))
    ) %>%
    dplyr::filter(is.finite(.data$target_rate), is.finite(.data$start_month)) %>%
    dplyr::arrange(.data$start_month, .data$quarter)
  if (!nrow(d)) stop("The ramp needs at least one quarter with a target rate.")
  d
}

# Capacity after every move that has arrived by this month.
sc_ramp_params_at <- function(params, moves, month) {
  p <- sc_params_copy(params)
  if (is.null(moves) || !nrow(moves)) return(p)
  arrived <- moves %>% dplyr::filter(.data$arrive_month <= month + 1e-9)
  for (i in seq_len(nrow(arrived))) {
    p <- sc_ramp_apply_move(p, arrived[i, , drop = FALSE])
  }
  p
}

sc_ramp_apply_move <- function(params, move) {
  p <- sc_params_copy(params)
  kind <- as.character(move$kind[[1]])
  if (identical(kind, "tech")) {
    n <- as.integer(p$labor$techs_per_shift) + as.integer(move$delta[[1]])
    p$labor$techs_per_shift <- as.integer(max(sc_labor_max_step(p), n))
  } else if (identical(kind, "shift")) {
    p$labor$pattern <- as.character(move$pattern[[1]])
  } else if (identical(kind, "station")) {
    res <- as.character(move$resource[[1]])
    p$capacity[[res]] <- as.integer(p$capacity[[res]] %||% 1L) + as.integer(move$delta[[1]])
  }
  p
}

sc_ramp_lead_months <- function(move, cfg) {
  kind <- as.character(move$kind[[1]])
  if (identical(kind, "station")) {
    res <- as.character(move$resource[[1]] %||% "")
    if (identical(res, "chamber")) return(as.numeric(cfg$chamber_lead_months %||% 9))
    return(as.numeric(cfg$station_lead_months %||% 6))
  }
  as.numeric(cfg$tech_lead_months %||% 3)
}

# Annualized throughput meets the quarter's target rate (default 95%).
sc_ramp_meets <- function(shipped, target_rate, meet_frac = 0.95) {
  is.finite(shipped) && is.finite(target_rate) &&
    shipped + 1e-9 >= as.numeric(meet_frac) * as.numeric(target_rate)
}

sc_ramp_station_label <- function(resource) {
  cat <- sc_resource_catalog()
  lab <- cat$label[match(resource, cat$resource)]
  if (length(lab) == 1 && is.finite(match(resource, cat$resource))) lab else resource
}

# Prefer a technician or shift add when labor is tight or when a station
# move for the current bottleneck is already on the book. Otherwise +1 of
# the busiest station (a second copy is allowed if one was not enough).
sc_ramp_labor_move <- function(params, qrow, moves, cfg) {
  month <- as.numeric(qrow$start_month)
  pending <- if (is.null(moves) || !nrow(moves)) tibble::tibble() else moves
  pat <- params$labor$pattern %||% "day"
  has_shift <- function(pattern) {
    nrow(pending) && any(pending$kind == "shift" & pending$pattern == pattern)
  }
  if (identical(pat, "day") && !has_shift("two_shift") && !has_shift("three_shift")) {
    return(sc_ramp_new_move("shift", pattern = "two_shift",
                            label = "Add a second shift",
                            detail = "Hire and train a second shift of technicians so the line covers 16 hours.",
                            quarter = qrow$quarter, need_month = month, cfg = cfg))
  }
  if (identical(pat, "two_shift") && !has_shift("three_shift")) {
    return(sc_ramp_new_move("shift", pattern = "three_shift",
                            label = "Add a third shift",
                            detail = "Hire and train a third shift so technicians cover 24 hours.",
                            quarter = qrow$quarter, need_month = month, cfg = cfg))
  }
  n_now <- as.integer(params$labor$techs_per_shift %||% 2L)
  pending_techs <- if (nrow(pending)) sum(pending$delta[pending$kind == "tech"], na.rm = TRUE) else 0
  if (n_now + pending_techs < 12) {
    return(sc_ramp_new_move("tech", delta = 1L,
                            label = "Add one technician per shift",
                            detail = sprintf("Raise the pool from %d to %d technicians per shift.",
                                             n_now, n_now + 1L),
                            quarter = qrow$quarter, need_month = month, cfg = cfg))
  }
  NULL
}

sc_ramp_station_move_label <- function(resource) {
  lab <- sc_ramp_station_label(resource)
  # Catalog labels are often plural ("Integration stations").
  one <- sub("s$", "", lab)
  sprintf("Order one more %s", tolower(one))
}

sc_ramp_recommend <- function(params, qrow, moves, cfg) {
  month <- as.numeric(qrow$start_month)
  pending <- if (is.null(moves) || !nrow(moves)) tibble::tibble() else moves
  labor_u <- as.numeric(qrow$labor_util_mean %||% 0)
  bot_res <- as.character(qrow$bottleneck %||% "")
  bot_u <- as.numeric(qrow$bottleneck_util %||% 0)
  labor_tight <- isTRUE(labor_u + 1e-9 >= bot_u) ||
    identical(bot_res, "techs") ||
    (is.finite(labor_u) && labor_u >= 0.80)
  res <- bot_res
  if (!nzchar(res) || identical(res, "techs")) {
    res <- as.character(qrow$busiest_station %||% "integration")
  }
  if (!nzchar(res) || identical(res, "techs")) res <- "integration"
  already_this_station <- nrow(pending) &&
    any(pending$kind == "station" & pending$resource == res)
  # If this station is already on order (or already added) and the quarter
  # still misses, try labor next rather than stalling on a duplicate.
  if (isTRUE(labor_tight) || isTRUE(already_this_station)) {
    lab_mv <- sc_ramp_labor_move(params, qrow, moves, cfg)
    if (!is.null(lab_mv)) return(lab_mv)
  }
  n_same <- if (nrow(pending)) sum(pending$kind == "station" & pending$resource == res) else 0
  cap_now <- as.integer(params$capacity[[res]] %||% 1L)
  if (cap_now + n_same < 4) {
    lab <- sc_ramp_station_move_label(res)
    noun <- sub("^order one more ", "", lab)
    return(sc_ramp_new_move("station", resource = res, delta = 1L,
                            label = lab,
                            detail = sprintf("Add one %s. Order-by is the need date minus the procurement lead time.",
                                             noun),
                            quarter = qrow$quarter, need_month = month, cfg = cfg))
  }
  sc_ramp_labor_move(params, qrow, moves, cfg)
}

# Earliest month a brand-new move could arrive if ordered at month 0.
sc_ramp_min_lead <- function(cfg) {
  min(c(as.numeric(cfg$tech_lead_months %||% 3),
        as.numeric(cfg$station_lead_months %||% 6),
        as.numeric(cfg$chamber_lead_months %||% 9)), na.rm = TRUE)
}

sc_ramp_new_move <- function(kind, cfg, quarter, need_month,
                             delta = 1L, pattern = NA_character_,
                             resource = NA_character_, label, detail) {
  lead <- sc_ramp_lead_months(
    tibble::tibble(kind = kind, resource = resource), cfg)
  order_month <- as.numeric(need_month) - lead
  late <- order_month < -1e-9
  arrive_month <- if (isTRUE(late)) lead else as.numeric(need_month)
  # If we have to order at month 0, the move arrives after `lead` months,
  # which may be after the need date.
  if (isTRUE(late)) {
    order_month <- 0
    arrive_month <- lead
  }
  tibble::tibble(
    move_id = NA_integer_,
    kind = kind,
    resource = resource,
    pattern = pattern,
    delta = as.integer(delta),
    label = label,
    detail = detail,
    quarter = as.integer(quarter),
    need_month = as.numeric(need_month),
    lead_months = as.numeric(lead),
    order_month = as.numeric(order_month),
    arrive_month = as.numeric(arrive_month),
    ordered_late = isTRUE(late)
  )
}

# Run n_reps at one quarter's rate, learning, and capacity. Same seeds
# (base_seed, base_seed+1, ...) at every quarter, like Breakpoint.
sc_ramp_sim_quarter <- function(params, schedule, i, cfg, n_reps, base_seed,
                                on_progress = NULL, should_stop = NULL,
                                done = 0L, total = 1L) {
  q <- schedule[i, , drop = FALSE]
  n_mid <- sc_ramp_cumulative(schedule, i, at = "mid")
  factor <- sc_ramp_learning_factor(n_mid, cfg$learning_pct, cfg$learning_ref_n)
  p <- sc_ramp_apply_demand(sc_ramp_scale_touch(params, factor), q$target_rate[[1]])
  runs <- vector("list", n_reps)
  cancelled <- FALSE
  for (r in seq_len(n_reps)) {
    if (is.function(should_stop) && isTRUE(should_stop())) {
      cancelled <- TRUE
      break
    }
    out <- sc_run_one(p, as.integer(base_seed + r - 1L))
    out$rep <- r
    runs[[r]] <- out
    done <- done + 1L
    if (is.function(on_progress)) {
      on_progress(done, total, sprintf("Q%d, %.0f units/year", q$quarter[[1]], q$target_rate[[1]]))
    }
  }
  if (cancelled || !length(Filter(Negate(is.null), runs))) {
    return(list(row = NULL, util = NULL, cancelled = TRUE, done = done))
  }
  runs <- Filter(Negate(is.null), runs)
  sumr <- sc_summarise(runs, p)
  util_rep <- purrr::map_dfr(runs, function(run) {
    sc_resource_use(run) %>% dplyr::mutate(rep = run$rep)
  })
  util_mean <- util_rep %>%
    dplyr::group_by(.data$resource, .data$label, .data$short) %>%
    dplyr::summarise(util = mean(.data$util, na.rm = TRUE),
                     queue_avg = mean(.data$queue_avg, na.rm = TRUE),
                     .groups = "drop")
  stations <- util_mean %>% dplyr::filter(.data$resource != "techs")
  busiest <- if (nrow(stations)) stations %>% dplyr::slice_max(.data$util, n = 1, with_ties = FALSE) else
    tibble::tibble(resource = NA_character_, util = NA_real_, label = NA_character_)
  techs_row <- util_mean %>% dplyr::filter(.data$resource == "techs")
  # Prefer the labor-utilization figure from scheduled tech-hours.
  labor_u <- as.numeric(sumr$summary$labor_util)
  bot_res <- as.character(busiest$resource[[1]])
  bot_u <- as.numeric(busiest$util[[1]])
  if (is.finite(labor_u) && (is.na(bot_u) || labor_u >= bot_u)) {
    bot_res <- "techs"
    bot_u <- labor_u
  }
  qfun <- function(x, pr) as.numeric(stats::quantile(x, pr, na.rm = TRUE, names = FALSE))
  pr <- sumr$per_rep
  row <- tibble::tibble(
    quarter = as.integer(q$quarter[[1]]),
    start_month = as.integer(q$start_month[[1]]),
    target_rate = as.numeric(q$target_rate[[1]]),
    learning_factor = factor,
    cum_units_mid = n_mid,
    pattern = p$labor$pattern,
    techs_per_shift = as.integer(p$labor$techs_per_shift),
    shipped_mean = mean(pr$shipped, na.rm = TRUE),
    shipped_p10 = qfun(pr$shipped, 0.1),
    shipped_p90 = qfun(pr$shipped, 0.9),
    cycle_mean = mean(pr$cycle_mean_h, na.rm = TRUE),
    cycle_p10 = qfun(pr$cycle_mean_h, 0.1),
    cycle_p90 = qfun(pr$cycle_mean_h, 0.9),
    hpu_mean = mean(pr$labor_hpu, na.rm = TRUE),
    hpu_p10 = qfun(pr$labor_hpu, 0.1),
    hpu_p90 = qfun(pr$labor_hpu, 0.9),
    labor_util_mean = labor_u,
    labor_util_p10 = qfun(pr$labor_util, 0.1),
    labor_util_p90 = qfun(pr$labor_util, 0.9),
    wip_mean = mean(pr$wip_avg, na.rm = TRUE),
    wip_p10 = qfun(pr$wip_avg, 0.1),
    wip_p90 = qfun(pr$wip_avg, 0.9),
    bottleneck = bot_res,
    bottleneck_label = if (identical(bot_res, "techs")) "Technicians" else
      sc_ramp_station_label(bot_res),
    bottleneck_util = bot_u,
    busiest_station = as.character(busiest$resource[[1]]),
    busiest_station_util = as.numeric(busiest$util[[1]])
  )
  row$meets_target <- sc_ramp_meets(row$shipped_mean, row$target_rate, cfg$meet_frac)
  list(row = row, util = util_mean %>% dplyr::mutate(quarter = row$quarter),
       cancelled = FALSE, done = done, per_rep = pr)
}

# Simulate every quarter with capacity as-of the start of that quarter.
sc_ramp_sim_path <- function(params, schedule, moves, cfg, n_reps, base_seed,
                             on_progress = NULL, should_stop = NULL,
                             done = 0L, total = 1L) {
  rows <- vector("list", nrow(schedule))
  utils <- vector("list", nrow(schedule))
  cancelled <- FALSE
  for (i in seq_len(nrow(schedule))) {
    if (is.function(should_stop) && isTRUE(should_stop())) {
      cancelled <- TRUE
      break
    }
    p_i <- sc_ramp_params_at(params, moves, schedule$start_month[[i]])
    out <- sc_ramp_sim_quarter(p_i, schedule, i, cfg, n_reps, base_seed,
                               on_progress, should_stop, done, total)
    done <- out$done
    if (isTRUE(out$cancelled)) {
      cancelled <- TRUE
      break
    }
    rows[[i]] <- out$row
    utils[[i]] <- out$util
  }
  points <- if (length(Filter(Negate(is.null), rows))) dplyr::bind_rows(rows) else tibble::tibble()
  util <- if (length(Filter(Negate(is.null), utils))) dplyr::bind_rows(utils) else tibble::tibble()
  list(points = points, util = util, cancelled = cancelled, done = done)
}

# Plan moves, then simulate the planned path and a delayed-move dip for each.
sc_ramp_run <- function(params,
                        schedule = sc_ramp_default_schedule(),
                        cfg = sc_ramp_defaults(),
                        n_reps = cfg$n_reps %||% 10L,
                        base_seed = cfg$base_seed %||% 42L,
                        on_progress = NULL, should_stop = NULL) {
  stopifnot(n_reps >= 1)
  t0 <- Sys.time()
  schedule <- sc_ramp_normalize_schedule(schedule)
  cfg$meet_frac <- cfg$meet_frac %||% 0.95
  n_q <- nrow(schedule)
  # Budget: first path + a few planning passes + one delayed quarter per move.
  max_moves <- 6L
  total <- as.integer(n_reps * n_q * (2L + max_moves))
  done <- 0L
  moves <- tibble::tibble()
  cancelled <- FALSE
  last <- NULL
  for (pass in seq_len(max_moves + 1L)) {
    path <- sc_ramp_sim_path(params, schedule, moves, cfg, n_reps, base_seed,
                             on_progress, should_stop, done, total)
    done <- path$done
    last <- path
    if (isTRUE(path$cancelled)) {
      cancelled <- TRUE
      break
    }
    pts <- path$points
    if (!nrow(pts)) break
    fail <- pts %>% dplyr::filter(!.data$meets_target)
    if (!nrow(fail)) break
    # Quarters before the shortest lead time cannot be saved by a new move
    # ordered at month 0. Still recommend once (so the order is on the
    # book for later quarters), then skip them on later passes.
    min_lead <- sc_ramp_min_lead(cfg)
    helpable <- fail %>% dplyr::filter(.data$start_month + 1e-9 >= min_lead)
    first <- if (nrow(helpable)) helpable %>% dplyr::slice(1) else if (!nrow(moves)) fail %>% dplyr::slice(1) else NULL
    if (is.null(first)) break
    # Capacity at the start of that quarter (arrived moves only).
    p_now <- sc_ramp_params_at(params, moves, first$start_month)
    rec <- sc_ramp_recommend(p_now, first, moves, cfg)
    if (is.null(rec)) break
    # If this exact move is already on the book, stop (would loop).
    if (nrow(moves)) {
      dup <- moves$kind == rec$kind &
        dplyr::coalesce(moves$resource, "") == dplyr::coalesce(rec$resource, "") &
        dplyr::coalesce(moves$pattern, "") == dplyr::coalesce(rec$pattern, "") &
        moves$delta == rec$delta
      if (any(dup)) break
    }
    rec$move_id <- nrow(moves) + 1L
    moves <- dplyr::bind_rows(moves, rec)
  }
  planned <- last
  # Frozen (no-move) path, for "what if you do nothing" context on the charts.
  frozen <- NULL
  if (!cancelled) {
    if (nrow(moves)) {
      frozen <- sc_ramp_sim_path(params, schedule, tibble::tibble(), cfg,
                                 n_reps, base_seed, on_progress, should_stop,
                                 done, total)
      done <- frozen$done
      cancelled <- isTRUE(frozen$cancelled)
    } else {
      frozen <- planned
    }
  }
  delays <- tibble::tibble()
  if (!cancelled && nrow(moves)) {
    delay_rows <- vector("list", nrow(moves))
    for (m in seq_len(nrow(moves))) {
      if (is.function(should_stop) && isTRUE(should_stop())) {
        cancelled <- TRUE
        break
      }
      slipped <- moves
      slipped$arrive_month[m] <- slipped$arrive_month[m] + slipped$lead_months[m]
      slipped$order_month[m] <- slipped$order_month[m] + slipped$lead_months[m]
      # Re-simulate the need quarter (and the next, if any) with this slip.
      q_need <- which(schedule$quarter == moves$quarter[[m]])
      if (!length(q_need)) q_need <- 1L
      q_need <- q_need[[1]]
      idx <- unique(c(q_need, min(q_need + 1L, n_q)))
      drows <- vector("list", length(idx))
      for (j in seq_along(idx)) {
        i <- idx[[j]]
        p_i <- sc_ramp_params_at(params, slipped, schedule$start_month[[i]])
        out <- sc_ramp_sim_quarter(p_i, schedule, i, cfg, n_reps, base_seed,
                                   on_progress, should_stop, done, total)
        done <- out$done
        if (isTRUE(out$cancelled)) {
          cancelled <- TRUE
          break
        }
        drows[[j]] <- out$row %>%
          dplyr::mutate(move_id = moves$move_id[[m]],
                        delayed_label = moves$label[[m]])
      }
      if (cancelled) break
      delay_rows[[m]] <- dplyr::bind_rows(drows)
    }
    if (!cancelled) delays <- dplyr::bind_rows(delay_rows)
  }
  points <- planned$points %||% tibble::tibble()
  if (nrow(points) && !is.null(frozen) && nrow(frozen$points)) {
    points <- points %>%
      dplyr::left_join(
        frozen$points %>%
          dplyr::select(quarter,
                        frozen_shipped = shipped_mean,
                        frozen_meets = meets_target,
                        frozen_cycle = cycle_mean,
                        frozen_labor = labor_util_mean,
                        frozen_hpu = hpu_mean,
                        frozen_wip = wip_mean),
        by = "quarter"
      )
  }
  bundle <- list(
    params = params,
    schedule = schedule,
    cfg = cfg,
    points = points,
    util = planned$util %||% tibble::tibble(),
    frozen = frozen,
    moves = if (nrow(moves)) moves else tibble::tibble(),
    delays = delays
  )
  bundle$story <- sc_ramp_story(bundle)
  bundle$meta <- list(
    n_reps = as.integer(n_reps),
    base_seed = as.integer(base_seed),
    secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    cancelled = cancelled,
    source = "live"
  )
  bundle
}

sc_ramp_story <- function(bundle) {
  pts <- bundle$points
  if (is.null(pts) || !nrow(pts)) {
    return("No ramp results yet. Run a plan to fill this summary.")
  }
  meet_pct <- round(100 * (bundle$cfg$meet_frac %||% 0.95))
  n_ok <- sum(pts$meets_target, na.rm = TRUE)
  n_q <- nrow(pts)
  first_miss <- pts %>% dplyr::filter(!.data$meets_target) %>% dplyr::slice(1)
  start_lab <- sc_ramp_start_label(bundle$params)
  learn <- bundle$cfg$learning_pct %||% 85
  learn_txt <- if (is.finite(learn) && learn < 100) {
    sprintf("Touch times follow an %.0f%% learning curve (each doubling of cumulative units multiplies labor time by %.0f%%), relative to the Settings times at %.0f units.",
            learn, learn, bundle$cfg$learning_ref_n %||% 80)
  } else {
    "No learning curve is applied, so touch times stay at the Settings values all the way up the ramp."
  }
  if (n_ok == n_q) {
    head <- sprintf(
      "Starting from %s, the line meets the target rate in every quarter (throughput at least %d%% of the planned units/year). %s No further capacity move is required on this ramp.",
      start_lab, meet_pct, learn_txt)
  } else {
    miss <- first_miss
    head <- sprintf(
      "Starting from %s, the line first misses the target in quarter %d (month %s), when the plan calls for %.0f units/year and mean throughput is %.1f. %s",
      start_lab, miss$quarter, miss$start_month, miss$target_rate, miss$shipped_mean, learn_txt)
  }
  moves <- bundle$moves
  if (is.null(moves) || !nrow(moves)) {
    plan <- "The plan has no capacity moves."
  } else {
    bits <- purrr::pmap_chr(moves, function(label, order_month, arrive_month,
                                            need_month, ordered_late, detail, ...) {
      late <- if (isTRUE(ordered_late)) {
        sprintf(" Order immediately; even then this move arrives in month %.0f, after it is first needed (month %.0f).",
                arrive_month, need_month)
      } else {
        ""
      }
      sprintf("%s: order in month %.0f so it is in place by month %.0f.%s",
              label, order_month, arrive_month, late)
    })
    plan <- paste("The recommended plan is:", paste(bits, collapse = " "))
  }
  delays <- bundle$delays
  dip <- ""
  if (!is.null(delays) && nrow(delays) && nrow(moves)) {
    # Compare delayed need-quarter throughput to the planned figure.
    d0 <- delays %>%
      dplyr::inner_join(moves %>% dplyr::select(move_id, label, quarter),
                        by = c("move_id", "quarter"))
    if (nrow(d0)) {
      d0 <- d0 %>%
        dplyr::left_join(pts %>% dplyr::select(quarter, planned_shipped = shipped_mean,
                                               planned_meets = meets_target),
                         by = "quarter")
      worst <- d0 %>% dplyr::slice(1)
      if (!isTRUE(worst$meets_target) && isTRUE(worst$planned_meets %||% FALSE)) {
        dip <- sprintf(" If %s slips by its lead time, quarter %d drops to %.1f units/year and misses the target (the planned path held %.1f).",
                       tolower(worst$label), worst$quarter, worst$shipped_mean,
                       worst$planned_shipped)
      } else if (!isTRUE(worst$meets_target)) {
        dip <- sprintf(" If %s slips by its lead time, quarter %d is still short of the target (%.1f units/year).",
                       tolower(worst$label), worst$quarter, worst$shipped_mean)
      }
    }
  }
  paste(head, plan, dip, "Numbers are illustrative.")
}

sc_ramp_start_label <- function(params) {
  pat <- params$labor$pattern %||% "day"
  pat_lab <- switch(pat,
                    day = "a day shift",
                    two_shift = "two shifts",
                    three_shift = "three shifts",
                    pat)
  sprintf("%s and %d technicians per shift, with %d integration station%s",
          pat_lab,
          as.integer(params$labor$techs_per_shift %||% 1L),
          as.integer(params$capacity$integration %||% 1L),
          if (as.integer(params$capacity$integration %||% 1L) == 1L) "" else "s")
}

sc_load_preload_ramp <- function() {
  path <- file.path("data", "preload_ramp.rds")
  if (!file.exists(path)) path <- file.path("app", "data", "preload_ramp.rds")
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}
