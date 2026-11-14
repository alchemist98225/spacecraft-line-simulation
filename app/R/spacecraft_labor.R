# spacecraft_labor.R
# Shared technician pool: shift schedule, per-step tech counts, and helpers
# the model and Settings page use. Capacity is a simmer::schedule so the
# pool shrinks to zero on off shifts. Each process step seizes its station
# and then this many technicians, in that order, so the line cannot deadlock.

# Named default headcount at each process step (Integration sub-steps first).
sc_labor_step_defaults <- function() {
  c(
    subassembly = 2L,
    structure = 2L, harness = 2L, avionics = 2L, propulsion = 2L,
    func_test = 1L, vibe = 1L, tvac = 1L,
    review = 1L, acceptance = 1L
  )
}

# Plain-English rows for the Settings per-step table.
sc_labor_step_catalog <- function() {
  tibble::tribble(
    ~step,         ~label,                    ~station,
    "subassembly", "Subassembly build-up",    "Subassembly build-up",
    "structure",   "Structure (Integration)", "Integration",
    "harness",     "Harness (Integration)",   "Integration",
    "avionics",    "Avionics (Integration)",  "Integration",
    "propulsion",  "Propulsion (Integration)","Integration",
    "func_test",   "Functional test",         "Functional test",
    "vibe",        "Vibration test",          "Environmental test",
    "tvac",        "Thermal-vacuum test",     "Environmental test",
    "review",      "Material review",         "Material review board",
    "acceptance",  "Final acceptance",        "Final acceptance"
  )
}

sc_labor_patterns <- function() {
  c("Day only (8 hours)" = "day",
    "Two shifts (16 hours)" = "two_shift",
    "Three shifts (24 hours)" = "three_shift")
}

sc_labor_work_weeks <- function() {
  c("Monday–Friday (5 days)" = "five_day",
    "All week (7 days)" = "seven_day")
}

# t = 0 is Monday 00:00. The horizon is calendar hours, including weekends.
sc_labor_is_five_day <- function(params) {
  identical(params$labor$work_week %||% "five_day", "five_day")
}

sc_labor_shift_hours <- function(params) {
  pat <- params$labor$pattern %||% "three_shift"
  len <- params$labor$shift_length_h %||% 8
  if (identical(pat, "three_shift")) 24 else if (identical(pat, "day")) len else 2 * len
}

sc_labor_always_on <- function(params) {
  !sc_labor_is_five_day(params) && identical(params$labor$pattern %||% "three_shift", "three_shift")
}

sc_is_work_time <- function(t, params) {
  t <- as.numeric(t)
  if (!is.finite(t)) return(FALSE)
  dow <- floor(t / 24) %% 7
  if (sc_labor_is_five_day(params) && dow >= 5) return(FALSE)
  (t %% 24) + 1e-9 < sc_labor_shift_hours(params)
}

# Calendar hours from t until the next working instant (0 if already working).
sc_wait_to_work <- function(t, params) {
  t <- as.numeric(t)
  if (sc_is_work_time(t, params)) return(0)
  dow <- floor(t / 24) %% 7
  hod <- t %% 24
  if (sc_labor_is_five_day(params) && dow >= 5) {
    return((7 - dow) * 24 - hod)
  }
  rest <- 24 - hod
  if (sc_labor_is_five_day(params) && dow == 4) rest + 48 else rest
}

# Continuous working hours available starting at t (0 if off shift or weekend).
sc_work_run_length <- function(t, params) {
  if (!sc_is_work_time(t, params)) return(0)
  sc_labor_shift_hours(params) - (as.numeric(t) %% 24)
}

sc_next_tag <- function(events, prefix) {
  if (!is.null(events)) {
    events$seq <- events$seq + 1L
    paste0(prefix, events$seq)
  } else {
    paste0(prefix, as.integer(stats::runif(1) * 1e9))
  }
}

# Technicians required at a named step; never below 1, never above the pool.
sc_step_techs <- function(params, step) {
  n <- params$labor$step_techs[[step]]
  if (is.null(n) || !is.finite(n) || n < 1) n <- 1
  as.integer(max(1L, round(n)))
}

sc_labor_max_step <- function(params) {
  vals <- unname(params$labor$step_techs %||% sc_labor_step_defaults())
  as.integer(max(1L, max(vals, na.rm = TRUE)))
}

# simmer capacity. Three-shift on a 7-day week is a constant. Otherwise a
# periodic schedule: techs_per_shift on each working shift, and 0 on off
# shifts and (when the work week is Monday–Friday) on Saturday and Sunday.
# t = 0 is Monday 00:00. The horizon itself stays in calendar hours.
sc_labor_capacity <- function(params) {
  n <- as.integer(params$labor$techs_per_shift %||% 3L)
  n <- max(sc_labor_max_step(params), n)
  if (sc_labor_always_on(params)) return(n)
  shift <- sc_labor_shift_hours(params)
  if (!sc_labor_is_five_day(params)) {
    vals <- if (shift >= 24 - 1e-9) n else c(n, 0)
    times <- if (shift >= 24 - 1e-9) 0 else c(0, shift)
    return(simmer::schedule(times, vals, period = 24))
  }
  times <- numeric(); vals <- numeric()
  for (d in 0:4) {
    times <- c(times, d * 24)
    vals <- c(vals, n)
    if (shift < 24 - 1e-9) {
      times <- c(times, d * 24 + shift)
      vals <- c(vals, 0)
    }
  }
  if (shift >= 24 - 1e-9) {
    times <- c(times, 5 * 24)
    vals <- c(vals, 0)
  }
  simmer::schedule(times, vals, period = 168)
}

# Planned ship takt: the mean time between releases, so the annual rate
# sets both the release plan and the committed ship cadence.
sc_commit_takt <- function(params) {
  takt <- params$arrival$mean_interarrival_h %||% 72
  if (!is.finite(takt) || takt <= 0) takt <- 72
  takt
}

# Committed date for unit i (1-based): first slot at lead_h, then one takt apart.
sc_commit_due <- function(unit_id, params) {
  lead <- params$commit$lead_h %||% 260
  lead + (as.numeric(unit_id) - 1) * sc_commit_takt(params)
}

# Append: seize `n` techs, do a random-duration touch (with mid-touch
# equipment repairs when reliability is on), add n * duration to the
# unit's labor_hpu attribute, then release the techs. The caller already
# holds the station, so a wait here is "waiting for technicians".
# HPU uses the drawn touch time, not wall-clock, so an off-shift pause
# or a repair does not inflate hours per unit. During a repair the
# technicians are released and re-seized afterwards.
sc_tech_touch <- function(trj, env, attr, n_fn, mean_fn, cv_fn,
                          station = NULL, params = NULL, events = NULL) {
  if (sc_reliability_active(params, station)) {
    return(
      sc_tech_touch_reliable(trj, env, attr, n_fn, mean_fn, cv_fn,
                             station, params, events)
    )
  }
  if (is.null(params) || sc_labor_always_on(params)) {
    return(
      trj %>%
        simmer::seize("techs", amount = n_fn) %>%
        simmer::set_attribute("touch_draw", function() sc_rlnorm(1, mean_fn(), cv_fn())) %>%
        simmer::timeout(function() attr("touch_draw")) %>%
        simmer::set_attribute(
          "labor_hpu",
          function() n_fn() * attr("touch_draw"),
          mod = "+"
        ) %>%
        simmer::release("techs", amount = n_fn)
    )
  }
  tag <- sc_next_tag(events, "cal_touch_")
  trj %>%
    simmer::set_attribute("touch_draw", function() sc_rlnorm(1, mean_fn(), cv_fn())) %>%
    simmer::set_attribute("work_left", function() attr("touch_draw")) %>%
    simmer::set_attribute("cal_mark", 0, tag = tag) %>%
    simmer::branch(
      function() as.integer(sc_wait_to_work(simmer::now(env), params) > 1e-9),
      continue = TRUE,
      simmer::trajectory() %>%
        simmer::timeout(function() sc_wait_to_work(simmer::now(env), params))
    ) %>%
    simmer::seize("techs", amount = n_fn) %>%
    simmer::set_attribute("cal_run", function() {
      min(attr("work_left"), sc_work_run_length(simmer::now(env), params))
    }) %>%
    simmer::timeout(function() max(0, attr("cal_run"))) %>%
    simmer::release("techs", amount = n_fn) %>%
    simmer::set_attribute("work_left", function() max(0, attr("work_left") - attr("cal_run"))) %>%
    simmer::branch(
      function() as.integer(attr("work_left") > 1e-9),
      continue = TRUE,
      simmer::trajectory() %>% simmer::rollback(tag)
    ) %>%
    simmer::set_attribute(
      "labor_hpu",
      function() n_fn() * attr("touch_draw"),
      mod = "+"
    )
}

# Reliable variant: one seize/release of techs per work chunk, so a mid-touch
# repair does not keep production technicians standing at a broken station.
# The failure clock only runs during work hours (not weekends or off shifts).
sc_tech_touch_reliable <- function(trj, env, attr, n_fn, mean_fn, cv_fn,
                                   station, params, events) {
  events$seq <- events$seq + 1L
  tag_work <- paste0("rel_work_", events$seq)
  events$seq <- events$seq + 1L
  tag_cal <- paste0("rel_cal_", events$seq)
  rec_fail <- function() {
    events$fail_id <- events$fail_id + 1L
    events$failures[[length(events$failures) + 1L]] <- tibble::tibble(
      station = station,
      time = simmer::now(env),
      failure_id = events$fail_id,
      name = simmer::get_name(env)
    )
    events$fail_id
  }
  always <- sc_labor_always_on(params)
  trj %>%
    simmer::set_attribute("touch_draw", function() sc_rlnorm(1, mean_fn(), cv_fn())) %>%
    simmer::set_attribute("work_left", function() attr("touch_draw")) %>%
    simmer::set_attribute(
      "ttf",
      function() stats::rexp(1, 1 / sc_reliability_mtbf(params, station)),
      tag = tag_work
    ) %>%
    simmer::set_attribute("cal_mark", 0, tag = tag_cal) %>%
    simmer::branch(
      function() as.integer(!always && sc_wait_to_work(simmer::now(env), params) > 1e-9),
      continue = TRUE,
      simmer::trajectory() %>%
        simmer::timeout(function() sc_wait_to_work(simmer::now(env), params))
    ) %>%
    simmer::seize("techs", amount = n_fn) %>%
    simmer::set_attribute("cal_run", function() {
      cap <- if (always) Inf else sc_work_run_length(simmer::now(env), params)
      min(attr("work_left"), attr("ttf"), cap)
    }) %>%
    simmer::timeout(function() max(0, attr("cal_run"))) %>%
    simmer::release("techs", amount = n_fn) %>%
    simmer::set_attribute("work_left", function() max(0, attr("work_left") - attr("cal_run"))) %>%
    simmer::set_attribute("ttf", function() max(0, attr("ttf") - attr("cal_run"))) %>%
    simmer::branch(
      function() as.integer(attr("work_left") > 1e-9 && attr("ttf") <= 1e-9),
      continue = TRUE,
      simmer::trajectory() %>%
        simmer::set_prioritization(c(.Machine$integer.max - 1L, .Machine$integer.max, 0L)) %>%
        simmer::set_attribute("cur_failure_id", function() rec_fail()) %>%
        simmer::set_attribute("repair_start", function() simmer::now(env)) %>%
        simmer::seize("maintenance") %>%
        simmer::set_attribute("last_repair_h", function() {
          stats::rexp(1, 1 / sc_reliability_mttr(params, station))
        }) %>%
        simmer::timeout(function() attr("last_repair_h")) %>%
        simmer::release("maintenance") %>%
        simmer::set_attribute("down_h", function() {
          wall <- max(0, simmer::now(env) - attr("repair_start"))
          events$repairs[[length(events$repairs) + 1L]] <- tibble::tibble(
            station = station,
            time = attr("repair_start"),
            duration = wall,
            repair_h = attr("last_repair_h"),
            failure_id = attr("cur_failure_id"),
            name = simmer::get_name(env)
          )
          attr("down_h") + wall
        }) %>%
        simmer::rollback(tag_work)
    ) %>%
    simmer::branch(
      function() as.integer(attr("work_left") > 1e-9),
      continue = TRUE,
      simmer::trajectory() %>% simmer::rollback(tag_cal)
    ) %>%
    simmer::set_attribute(
      "labor_hpu",
      function() n_fn() * attr("touch_draw"),
      mod = "+"
    )
}
