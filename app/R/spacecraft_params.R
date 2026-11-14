# spacecraft_params.R
# Spacecraft line (optional, low-volume / high-mix): every tunable number lives
# in one list built by sc_default_params(). The simulation, the Shiny page and
# the GA adapter all read and write this one list, so nothing is hard-coded
# inside the model. Times are in HOURS throughout.
#
# Variant-specific inputs sit in a tibble with list columns:
#   route     tibble(step, mean_h, cv) of Integration sub-steps for that variant
#   func_test list(mean_h, cv)          Functional test touch time
#   env       list(vibe_mean_h, vibe_cv, tvac_mean_h, tvac_cv)
# Due dates are no longer a per-variant allowance. Each unit gets a committed
# ship date from the planned cadence (see params$commit and sc_arrival_plan()).

sc_variants_default <- function() {
  tibble::tibble(
    variant = c("A", "B", "C"),
    mix = c(0.5, 0.3, 0.2),
    route = list(
      tibble::tibble(step = c("structure", "harness"), mean_h = c(16, 12), cv = c(0.35, 0.35)),
      tibble::tibble(step = c("structure", "harness", "avionics"), mean_h = c(18, 14, 10), cv = c(0.35, 0.35, 0.45)),
      tibble::tibble(step = c("structure", "harness", "avionics", "propulsion"),
                     mean_h = c(20, 16, 12, 14), cv = c(0.35, 0.35, 0.45, 0.50))
    ),
    func_test = list(
      list(mean_h = 8, cv = 0.30),
      list(mean_h = 10, cv = 0.30),
      list(mean_h = 12, cv = 0.35)
    ),
    env = list(
      list(vibe_mean_h = 6, vibe_cv = 0.25, tvac_mean_h = 18, tvac_cv = 0.30),
      list(vibe_mean_h = 8, vibe_cv = 0.25, tvac_mean_h = 22, tvac_cv = 0.30),
      list(vibe_mean_h = 10, vibe_cv = 0.25, tvac_mean_h = 26, tvac_cv = 0.30)
    )
  )
}

sc_default_params <- function() {
  list(
    sim_hours = 24 * 7 * 52,          # one year of calendar hours
    arrival = list(
      process = "fixed",              # evenly scheduled to match the committed ship cadence
      mean_interarrival_h = 72
    ),
    # Committed ship cadence: slot i is lead_h + (i-1) * takt, with takt
    # equal to the planned mean time between releases (the annual rate).
    commit = list(
      lead_h = 165                    # committed date of the first unit (hours after its release on a matched cadence)
    ),
    variants = sc_variants_default(),
    kit = list(
      delay_mean_h = 24,              # lognormal long-lead parts delay after release
      delay_cv = 0.25,
      p_late = 0.02,                  # chance a kit also slips by an extra delay
      late_extra_mean_h = 36          # exponential mean of that extra slip
    ),
    capacity = list(
      integration = 2,
      func_test = 1,
      chamber = 1,                    # ONE shared vibe + TVAC chamber
      review_board = 1,
      acceptance = 1
    ),
    # Shared technician pool, scheduled by shift. Each process step seizes
    # its station and this many technicians (see labor$step_techs).
    labor = list(
      pattern = "three_shift",        # "day", "two_shift", or "three_shift"
      techs_per_shift = 4L,
      shift_length_h = 8,
      step_techs = sc_labor_step_defaults()
    ),
    p_fail_func = 0.05,
    p_fail_env = 0.03,
    max_fails = 2,                    # on this many fails a unit is forced to MRB
    mrb = list(
      mean_h = 48, cv = 0.45,
      p_use_as_is = 0.5, p_rework = 0.35, p_scrap = 0.15
    ),
    acceptance = list(mean_h = 6, cv = 0.25),
    chamber = list(
      preempt = TRUE,                 # urgent units may bump a running test
      restart = FALSE                 # FALSE = bumped unit resumes remaining time
    ),
    # Equipment breakdowns. Defaults are illustrative (see About).
    # Failures are operating-time based and can be turned off.
    reliability = sc_reliability_defaults()
  )
}

# Illustrative MTBF / MTTR (hours) for each station type and the chamber.
# Kept here so sc_default_params() does not depend on source order.
sc_reliability_defaults <- function() {
  list(
    enabled = TRUE,
    maintenance_techs = 1L,
    integration  = list(mtbf_h = 2000, mttr_h = 8),
    func_test    = list(mtbf_h = 2000, mttr_h = 8),
    chamber      = list(mtbf_h = 1000, mttr_h = 24),
    review_board = list(mtbf_h = 5000, mttr_h = 4),
    acceptance   = list(mtbf_h = 5000, mttr_h = 4)
  )
}

# Lognormal draws parameterised by mean and coefficient of variation.
# sdlog^2 = log(1 + cv^2); meanlog is shifted so the draw's mean is `mean`.
sc_rlnorm <- function(n, mean, cv) {
  sdlog <- sqrt(log1p(cv^2))
  stats::rlnorm(n, meanlog = log(mean) - sdlog^2 / 2, sdlog = sdlog)
}

# ---- Variant table <-> long editable form (used by the Shiny table) ----
# Long form: one row per number, e.g. (A, route, harness_mean_h, 12).
sc_variants_long <- function(variants) {
  variants %>%
    dplyr::mutate(
      route = purrr::map(route, function(r) {
        r %>%
          tidyr::pivot_longer(c(mean_h, cv), names_to = "stat") %>%
          dplyr::transmute(item = paste(step, stat, sep = "_"), value)
      }),
      dplyr::across(c(func_test, env), ~ purrr::map(.x, ~ tibble::enframe(unlist(.x), "item", "value"))),
      mix = purrr::map(mix, ~ tibble::tibble(item = "share", value = .x))
    ) %>%
    tidyr::pivot_longer(-variant, names_to = "group", values_to = "rows") %>%
    tidyr::unnest(rows)
}

# Write edited long values back into the list-column tibble.
sc_variants_from_long <- function(long, variants) {
  get <- function(v, g) long %>% dplyr::filter(variant == v, group == g) %>% { setNames(.$value, .$item) }
  variants %>%
    dplyr::mutate(
      mix = purrr::map_dbl(variant, ~ get(.x, "mix")[["share"]]),
      route = purrr::map2(variant, route, function(v, r) {
        x <- get(v, "route")
        dplyr::mutate(r, mean_h = x[paste0(step, "_mean_h")], cv = x[paste0(step, "_cv")]) %>%
          dplyr::mutate(dplyr::across(c(mean_h, cv), unname))
      }),
      dplyr::across(c(func_test, env), function(col) {
        g <- dplyr::cur_column()
        purrr::map(variant, ~ as.list(get(.x, g)))
      })
    )
}
