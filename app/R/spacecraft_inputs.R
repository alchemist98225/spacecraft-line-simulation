# spacecraft_inputs.R
# One tibble, sc_line_fields(), describes every line-wide input: its id, which
# section it sits in, its widget type, labels, a plain-English hover hint and
# the path into the params list it sets (NULL for run-only fields).
# The Simulate sidebar and the Settings > Line settings tab are both built
# from it, so the two always show the same fields. The Settings copy of a
# field has the id "set_<id>".

sc_line_fields <- function() {
  num <- function(id, panel, label, hint, path, step, default = NULL, max = NA_real_) {
    tibble::tibble(id = id, panel = panel, type = "numeric", label = label, hint = hint,
                   path = list(path), step = step, default = list(default), choices = list(NULL), max = max)
  }
  fields <- dplyr::bind_rows(
    num("n_reps", "run", "Number of replications", "How many independent runs to average. More runs give steadier results but take longer.", NULL, 1, 5),
    num("seed", "run", "Base seed", "Starting point for the random draws. The same seed with the same settings gives the same results.", NULL, 1, 42),
    num("warmup_weeks", "run", "Warm-up to discard (weeks)", "The line starts empty, so the first units see short queues. Leave this at 0 to keep the current cycle-time and throughput numbers on Simulate, Compare and the other pages. Raise it to drop units released in the first weeks from those KPIs, and to measure queues and utilization after that point.", NULL, 1, 0),
    # Horizon: three steppers (not in params directly); sc_horizon_hours() turns
    # them into params$sim_hours. The "text" row shows the total in hours.
    num("horizon_quarters", "run", "Horizon: quarters", "Whole quarters to simulate. One quarter is 13 calendar weeks (2,184 hours), including weekends.", NULL, 1, 4),
    num("horizon_weeks", "run", "Horizon: weeks", "Extra calendar weeks on top of the quarters. One week is 168 hours, including weekends.", NULL, 1, 0),
    num("horizon_days", "run", "Horizon: days", "Extra calendar days on top of the quarters and weeks. One day is 24 hours.", NULL, 1, 0),
    tibble::tibble(id = "horizon_hours_text", panel = "run", type = "text", label = "Horizon in hours",
                   hint = "Total length of each run in calendar hours, including weekends: quarters, weeks and days added up. The model clock is calendar time even when the work week is Monday to Friday.",
                   path = list(NULL), step = NA, default = list(NULL), choices = list(NULL)),
    tibble::tibble(id = "arrival_process", panel = "arrivals", type = "select", label = "Release spacing",
                   hint = "Evenly scheduled releases keep a steady rate (a unit every planned takt). Random spacing lets gaps vary.",
                   path = list(list("arrival", "process")), step = NA, default = list("poisson"),
                   choices = list(c("Random (exponential gaps)" = "poisson", "Evenly scheduled" = "fixed"))),
    num("mean_interarrival_h", "arrivals", "Mean time between releases (hours)", "The planned takt: on average, how many hours pass between one unit being released and the next.", list("arrival", "mean_interarrival_h"), 1),
    num("commit_lead_h", "advanced", "Committed lead time (first unit, hours)", "Sets chamber priority: units with earlier committed ship dates go first. The first unit's committed ship is this many calendar hours after release; later units are spaced one takt apart. A longer lead gives the line more float.", list("commit", "lead_h"), 1),
    tibble::tibble(id = "labor_work_week", panel = "labor", type = "select", label = "Work week",
                   hint = "Which days the technician pool is staffed. Monday–Friday leaves the pool at zero all weekend. Combined with the shift pattern. The simulation horizon stays in calendar hours, including weekends.",
                   path = list(list("labor", "work_week")), step = NA, default = list("five_day"),
                   choices = list(c("Monday–Friday (5 days)" = "five_day", "All week (7 days)" = "seven_day"))),
    tibble::tibble(id = "labor_pattern", panel = "labor", type = "select", label = "Shift pattern",
                   hint = "How many shifts technicians cover on each working day. Day only is 8 hours, two shifts is 16, three shifts is 24 hours on each working day (the weekend still follows the work-week setting). Work pauses when a shift or the work week ends, and resumes when the next shift starts.",
                   path = list(list("labor", "pattern")), step = NA, default = list("three_shift"),
                   choices = list(c("Day only (8 hours)" = "day", "Two shifts (16 hours)" = "two_shift", "Three shifts (24 hours)" = "three_shift"))),
    num("labor_techs", "labor", "Technicians per shift", "How many technicians are on the line during each working shift. They are a shared pool: every process step seizes its station and then the number of technicians set in Settings.", list("labor", "techs_per_shift"), 1),
    num("cap_subassembly", "subassembly", "Subassembly stations (units worked at once)", "How many units can be in subassembly build-up at the same time. Harnesses, avionics, propulsion and panels are built in parallel here to feed Integration.", list("capacity", "subassembly"), 1),
    tibble::tibble(id = "kit_late_parts", panel = "subassembly", type = "check",
                   label = "Long-lead parts can arrive late",
                   hint = "Off by default: parts are on hand when the unit is released, so there is no delay before subassembly. Turn this on to add a long-lead parts delay (lognormal lead time, with an optional extra slip).",
                   path = list(list("kit", "late_parts")), step = NA,
                   default = list(FALSE), choices = list(NULL)),
    num("kit_delay_mean_h", "subassembly", "Long-lead time (hours)", "Used only when long-lead parts can arrive late. Average wait for a unit's long-lead parts after release, before subassembly can start.", list("kit", "delay_mean_h"), 1),
    num("kit_delay_cv", "subassembly", "Long-lead coefficient of variation", "How much the long-lead time varies from unit to unit: the standard deviation divided by the mean. Used only when the late-parts toggle is on.", list("kit", "delay_cv"), 0.05),
    num("kit_late_extra_h", "subassembly", "Extra delay time (hours)", "When a long-lead kit is held up further, the average extra hours it adds. Used only when the late-parts toggle is on.", list("kit", "late_extra_mean_h"), 1),
    num("kit_p_late", "subassembly", "Extra delay probability", "Chance, from 0 to 1, that a long-lead kit is held up by an extra delay on top of its lead time. Used only when the late-parts toggle is on.", list("kit", "p_late"), 0.01, max = 1),
    num("qa_int_mean_h", "qa", "Buy-off after Integration (hours)", "Average calendar wait for inspection and paperwork after Integration. This hold needs no technicians; the station is already free.", list("qa", "after_integration", "mean_h"), 1),
    num("qa_int_cv", "qa", "After Integration, variation", "How much the post-Integration buy-off wait varies from unit to unit: the standard deviation divided by the mean.", list("qa", "after_integration", "cv"), 0.05),
    num("qa_ch_mean_h", "qa", "Buy-off after the chamber (hours)", "Average calendar wait for inspection and test-data review after environmental test. This hold needs no technicians.", list("qa", "after_chamber", "mean_h"), 1),
    num("qa_ch_cv", "qa", "After the chamber, variation", "How much the post-chamber buy-off wait varies from unit to unit: the standard deviation divided by the mean.", list("qa", "after_chamber", "cv"), 0.05),
    num("qa_acc_mean_h", "qa", "Paperwork before final acceptance (hours)", "Average calendar wait for paperwork and QA buy-off immediately before final acceptance. This hold needs no technicians.", list("qa", "before_acceptance", "mean_h"), 1),
    num("qa_acc_cv", "qa", "Before acceptance, variation", "How much the pre-acceptance paperwork wait varies from unit to unit: the standard deviation divided by the mean.", list("qa", "before_acceptance", "cv"), 0.05),
    num("cap_integration", "capacity", "Integration stations (units worked at once)", "How many units Integration can work on at the same time. Others wait in the queue.", list("capacity", "integration"), 1),
    num("cap_func_test", "capacity", "Functional test stands", "How many units can be in functional test at once.", list("capacity", "func_test"), 1),
    num("cap_chamber", "capacity", "Environmental chambers (vibration + thermal vacuum)", "How many chambers there are. Each unit needs a chamber for vibration and again for thermal vacuum.", list("capacity", "chamber"), 1),
    num("cap_review_board", "capacity", "Material review boards", "How many review boards can look at failed units at once.", list("capacity", "review_board"), 1),
    num("cap_acceptance", "capacity", "Final acceptance stands", "How many units can go through final acceptance at once.", list("capacity", "acceptance"), 1),
    num("p_fail_func", "quality", "Functional failure rate", "Share of units, from 0 to 1, that fail functional test and go back to Integration for rework.", list("p_fail_func"), 0.01, max = 1),
    num("p_fail_env", "quality", "Environmental failure rate", "Share of units, from 0 to 1, that fail environmental test and go to the material review board.", list("p_fail_env"), 0.01, max = 1),
    num("max_fails", "quality", "Failures before review", "After this many test failures a unit goes straight to the material review board, which can then only use it as is or scrap it.", list("max_fails"), 1),
    tibble::tibble(id = c("chamber_preempt", "chamber_restart"), panel = "chamber", type = "check",
                   label = c("Urgent units can bump a running test", "A bumped test starts over from zero"),
                   hint = c("Units with the earlier planned ship date go first. With this on, a unit that is closer to that date can take the chamber from a unit that has more time to spare.",
                            "With this off, a bumped unit later picks up where it left off. With it on, it repeats the whole test."),
                   path = list(list("chamber", "preempt"), list("chamber", "restart")), step = NA,
                   default = list(TRUE, FALSE), choices = list(NULL, NULL)),
    num("mrb_mean_h", "mrb", "Review hold (hours)", "Average time a unit waits while the material review board decides what to do with it.", list("mrb", "mean_h"), 1),
    num("mrb_cv", "mrb", "Coefficient of variation", "How much the review hold varies from unit to unit: the standard deviation divided by the mean. 0 means every review takes exactly the average.", list("mrb", "cv"), 0.05),
    num("mrb_p_use_as_is", "mrb", "Accepted", "Share of reviewed units, from 0 to 1, accepted as they are and sent on to final acceptance.", list("mrb", "p_use_as_is"), 0.05, max = 1),
    num("mrb_p_scrap", "mrb", "Scrap", "Share of reviewed units, from 0 to 1, that are scrapped and leave the line.", list("mrb", "p_scrap"), 0.05, max = 1),
    # Rework is not entered: it is 1 - Accepted - Scrap, shown read-only.
    tibble::tibble(id = "mrb_rework", panel = "mrb", type = "derived", label = "Rework",
                   hint = "Worked out for you as 1 - Accepted - Scrap: the share sent back to Integration. Once a unit has hit the failure limit, rework isn't offered and this share is split between Accepted and Scrap.",
                   path = list(NULL), step = NA, default = list(NULL), choices = list(NULL)),
    num("acc_mean_h", "acceptance", "Final acceptance (hours)", "Average time for final acceptance (inspection, paperwork and sign-off) before the unit ships.", list("acceptance", "mean_h"), 1),
    num("acc_cv", "acceptance", "Coefficient of variation", "How much final acceptance time varies from unit to unit: the standard deviation divided by the mean. 0 means every unit takes exactly the average.", list("acceptance", "cv"), 0.05),
    tibble::tibble(id = "reliability_enabled", panel = "reliability", type = "check",
                   label = "Equipment can break down",
                   hint = "When this is on, each station type and the environmental test chamber can fail while a unit is being worked on (operating time, not calendar time). The unit stays in place while a separate maintainer repairs the equipment. Turn it off to run a line with no equipment breakdowns.",
                   path = list(list("reliability", "enabled")), step = NA,
                   default = list(TRUE), choices = list(NULL)),
    num("rel_mtbf_subassembly", "reliability", "Subassembly MTBF (hours)",
        "Mean operating hours between failures of a subassembly station. Illustrative default: 2,000 hours.",
        list("reliability", "subassembly", "mtbf_h"), 50),
    num("rel_mttr_subassembly", "reliability", "Subassembly MTTR (hours)",
        "Mean hours to repair a subassembly station after a breakdown. Illustrative default: 8 hours.",
        list("reliability", "subassembly", "mttr_h"), 1),
    num("rel_mtbf_integration", "reliability", "Integration MTBF (hours)",
        "Mean operating hours between failures of an Integration station. The failure clock only runs while a unit is being worked on at the station. Illustrative default: 2,000 hours.",
        list("reliability", "integration", "mtbf_h"), 50),
    num("rel_mttr_integration", "reliability", "Integration MTTR (hours)",
        "Mean hours to repair an Integration station after a breakdown. The unit stays in the station during the repair. Illustrative default: 8 hours.",
        list("reliability", "integration", "mttr_h"), 1),
    num("rel_mtbf_func_test", "reliability", "Functional test MTBF (hours)",
        "Mean operating hours between failures of a functional test stand. Illustrative default: 2,000 hours.",
        list("reliability", "func_test", "mtbf_h"), 50),
    num("rel_mttr_func_test", "reliability", "Functional test MTTR (hours)",
        "Mean hours to repair a functional test stand. Illustrative default: 8 hours.",
        list("reliability", "func_test", "mttr_h"), 1),
    num("rel_mtbf_chamber", "reliability", "Chamber MTBF (hours)",
        "Mean operating hours between failures of the environmental test chamber (shared by vibration and thermal-vacuum). Chambers are typically the least reliable resource. Illustrative default: 1,000 hours.",
        list("reliability", "chamber", "mtbf_h"), 50),
    num("rel_mttr_chamber", "reliability", "Chamber MTTR (hours)",
        "Mean hours to repair the chamber (vacuum, thermal plant or instrumentation). Illustrative default: 24 hours.",
        list("reliability", "chamber", "mttr_h"), 1),
    num("rel_mtbf_review", "reliability", "Review board MTBF (hours)",
        "Mean operating hours between failures of material-review equipment. Illustrative default: 5,000 hours.",
        list("reliability", "review_board", "mtbf_h"), 50),
    num("rel_mttr_review", "reliability", "Review board MTTR (hours)",
        "Mean hours to repair material-review equipment. Illustrative default: 4 hours.",
        list("reliability", "review_board", "mttr_h"), 1),
    num("rel_mtbf_acceptance", "reliability", "Final acceptance MTBF (hours)",
        "Mean operating hours between failures of a final-acceptance stand. Illustrative default: 5,000 hours.",
        list("reliability", "acceptance", "mtbf_h"), 50),
    num("rel_mttr_acceptance", "reliability", "Final acceptance MTTR (hours)",
        "Mean hours to repair a final-acceptance stand. Illustrative default: 4 hours.",
        list("reliability", "acceptance", "mttr_h"), 1),
    num("rel_maint_techs", "reliability", "Maintenance technicians",
        "A dedicated maintenance crew, separate from the production technician pool. Repairs wait if every maintainer is already busy. Default: 1.",
        list("reliability", "maintenance_techs"), 1)
  )
  # Defaults come from sc_default_params() by walking each path,
  # e.g. list("kit", "p_late") -> p$kit$p_late.
  defaults <- sc_default_params()
  fields %>%
    dplyr::mutate(default = purrr::map2(path, default, function(pth, d) {
      if (is.null(pth)) d else purrr::reduce(pth, `[[`, .init = defaults)
    }))
}

# Section titles, in sidebar order.
sc_line_panels <- tibble::tribble(
  ~panel,       ~title,                          ~icon,
  "run",        "Run simulation",                "play-fill",
  "arrivals",   "Releases",                      "box-arrow-in-right",
  "labor",      "Technicians",                  "people",
  "subassembly","Subassembly build-up",          "boxes",
  "qa",         "Inspection and paperwork",      "clipboard-check",
  "capacity",   "Capacities",                    "diagram-3",
  "quality",    "Test failures",                 "exclamation-triangle",
  "chamber",    "Environmental chamber",         "thermometer-half",
  "mrb",        "Material review board",         "people",
  "acceptance", "Final acceptance",              "check2-circle",
  "reliability","Equipment reliability",         "wrench-adjustable",
  "advanced",   "Advanced",                       "gear"
)

# One widget per field. prefix = "set_" builds the Settings copy, which also
# carries the hover hint on its wrapper.
sc_field_widget <- function(id, type, label, hint, step, default, choices, max = NA, prefix = "", ...) {
  input_id <- paste0(prefix, id)
  w <- switch(type,
    numeric = shiny::numericInput(input_id, label, value = default, min = 0,
                                  max = if (is.na(max)) NA else max, step = step),
    text = shiny::div(class = "fw-semibold mb-3", shiny::textOutput(input_id, inline = TRUE)),
    # Read-only value that looks like a greyed-out input box.
    derived = shiny::div(class = "form-group shiny-input-container mb-3",
                         shiny::tags$label(class = "control-label", label),
                         shiny::div(class = "form-control bg-light text-muted", style = "cursor: not-allowed;",
                                    shiny::textOutput(input_id, inline = TRUE))),
    select = shiny::selectInput(input_id, label, choices = choices, selected = default),
    check = shiny::checkboxInput(input_id, label, value = default)
  )
  if (nzchar(prefix)) shiny::div(title = hint, class = "sc-setting", w) else w
}

sc_panel_inputs <- function(panel_id, prefix = "") {
  sc_line_fields() %>%
    dplyr::filter(panel == panel_id) %>%
    purrr::pmap(function(...) sc_field_widget(..., prefix = prefix))
}

# Inputs -> a full params list. Fields without a value keep their default.
sc_params_from_inputs <- function(input, variants = sc_variants_default(), step_techs = NULL) {
  set <- sc_line_fields() %>%
    dplyr::filter(!purrr::map_lgl(path, is.null)) %>%
    dplyr::mutate(value = purrr::map(id, ~ input[[.x]])) %>%
    dplyr::filter(!purrr::map_lgl(value, ~ is.null(.x) || (length(.x) == 1 && is.na(.x))))
  p <- purrr::reduce2(set$path, set$value, function(acc, path, value) purrr::assign_in(acc, path, value),
                      .init = sc_default_params())
  p$mrb$p_rework <- sc_rework_share(p$mrb$p_use_as_is, p$mrb$p_scrap)
  p$sim_hours <- sc_horizon_hours(input$horizon_quarters %||% 4, input$horizon_weeks %||% 0, input$horizon_days %||% 0)
  # Only set when the user opts in, so params stay identical to sc_default_params()
  # at the default of 0 and other pages keep their current numbers.
  wweeks <- suppressWarnings(as.numeric(input$warmup_weeks %||% 0))
  if (is.finite(wweeks) && wweeks > 0) {
    p$warmup_h <- wweeks * 7 * 24
  }
  p$chamber$preempt <- isTRUE(p$chamber$preempt)
  p$chamber$restart <- isTRUE(p$chamber$restart)
  p$reliability$enabled <- isTRUE(p$reliability$enabled)
  p$kit$late_parts <- isTRUE(p$kit$late_parts)
  p$reliability$maintenance_techs <- as.integer(max(1L, round(p$reliability$maintenance_techs %||% 1L)))
  p$variants <- variants
  p$labor$techs_per_shift <- as.integer(max(1L, round(p$labor$techs_per_shift %||% 4L)))
  if (!is.null(step_techs) && length(step_techs)) {
    base <- sc_labor_step_defaults()
    for (nm in names(step_techs)) {
      val <- suppressWarnings(as.numeric(step_techs[[nm]]))
      if (is.finite(val)) base[[nm]] <- as.integer(max(1L, round(val)))
    }
    p$labor$step_techs <- base
  }
  p
}

# Quarters, weeks and days -> hours (1 quarter = 13 weeks, 1 week = 7 days).
# Blank or negative entries count as zero.
sc_horizon_hours <- function(quarters = 4, weeks = 0, days = 0) {
  n <- purrr::map_dbl(list(quarters, weeks, days), ~ max(0, suppressWarnings(as.numeric(.x %||% 0)), na.rm = TRUE))
  sum(n * c(13 * 7 * 24, 7 * 24, 24))
}

sc_horizon_label <- function(hours) {
  if (hours <= 0) "Horizon: 0 hours. Add at least one day to run." else paste0("Horizon: ", format(hours, big.mark = ","), " hours")
}

# Review board split: Rework = 1 - Accepted - Scrap (never below 0).
sc_rework_share <- function(accepted, scrap) {
  max(0, round(1 - (accepted %||% 0) - (scrap %||% 0), 10))
}

# NULL when Accepted and Scrap are valid, otherwise a plain-English problem.
# A missing input (not drawn yet) counts as its default.
sc_mrb_problem <- function(accepted, scrap) {
  d <- sc_default_params()$mrb
  a <- suppressWarnings(as.numeric(accepted %||% d$p_use_as_is)); s <- suppressWarnings(as.numeric(scrap %||% d$p_scrap))
  if (is.na(a) || is.na(s)) return("Enter both Accepted and Scrap.")
  if (a < 0 || a > 1 || s < 0 || s > 1) return("Accepted and Scrap must each be between 0 and 1.")
  if (a + s > 1 + 1e-9) return(sprintf("Accepted + Scrap is %s, over 1. Lower one of them.", format(round(a + s, 3))))
  NULL
}

sc_rework_label <- function(accepted, scrap) {
  d <- sc_default_params()$mrb
  sc_mrb_problem(accepted, scrap) %||%
    format(round(sc_rework_share(accepted %||% d$p_use_as_is, scrap %||% d$p_scrap), 3), nsmall = 2)
}

# Sync-able fields only ("text" and "derived" rows are displays, not inputs).
sc_sync_fields <- function() dplyr::filter(sc_line_fields(), !type %in% c("text", "derived"))
