# spacecraft_labor.R
# Shared technician pool: shift schedule, per-step tech counts, and helpers
# the model and Settings page use. Capacity is a simmer::schedule so the
# pool shrinks to zero on off shifts. Each process step seizes its station
# and then this many technicians, in that order, so the line cannot deadlock.

# Named default headcount at each process step (Integration sub-steps first).
sc_labor_step_defaults <- function() {
  c(
    structure = 2L, harness = 2L, avionics = 2L, propulsion = 2L,
    func_test = 1L, vibe = 1L, tvac = 1L,
    review = 1L, acceptance = 1L
  )
}

# Plain-English rows for the Settings per-step table.
sc_labor_step_catalog <- function() {
  tibble::tribble(
    ~step,         ~label,                    ~station,
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

# simmer capacity: a fixed integer for three-shift coverage, otherwise a
# 24-hour periodic schedule with techs_per_shift on each working shift.
sc_labor_capacity <- function(params) {
  n <- as.integer(params$labor$techs_per_shift %||% 3L)
  n <- max(sc_labor_max_step(params), n)
  len <- params$labor$shift_length_h %||% 8
  pat <- params$labor$pattern %||% "two_shift"
  if (identical(pat, "three_shift")) return(n)
  vals <- if (identical(pat, "day")) c(n, 0, 0) else c(n, n, 0)
  simmer::schedule(c(0, len, 2 * len), vals, period = 24)
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

# Append: seize `n` techs, do a random-duration touch, add n * duration to
# the unit's labor_hpu attribute, then release the techs. The caller already
# holds the station, so a wait here is "waiting for technicians".
sc_tech_touch <- function(trj, env, attr, n_fn, mean_fn, cv_fn) {
  # HPU uses the drawn touch time, not wall-clock, so an off-shift pause
  # does not inflate hours per unit.
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
}
