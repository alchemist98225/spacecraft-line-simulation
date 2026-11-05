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
    # Horizon: three steppers (not in params directly); sc_horizon_hours() turns
    # them into params$sim_hours. The "text" row shows the total in hours.
    num("horizon_quarters", "run", "Horizon: quarters", "Whole quarters to simulate. One quarter is 13 weeks (2,184 hours).", NULL, 1, 4),
    num("horizon_weeks", "run", "Horizon: weeks", "Extra weeks on top of the quarters. One week is 168 hours.", NULL, 1, 0),
    num("horizon_days", "run", "Horizon: days", "Extra days on top of the quarters and weeks. One day is 24 hours.", NULL, 1, 0),
    tibble::tibble(id = "horizon_hours_text", panel = "run", type = "text", label = "Horizon in hours",
                   hint = "Total length of each run: quarters, weeks and days added up, in hours. The model runs in hours.",
                   path = list(NULL), step = NA, default = list(NULL), choices = list(NULL)),
    tibble::tibble(id = "arrival_process", panel = "arrivals", type = "select", label = "Release spacing",
                   hint = "Random spacing means gaps between releases vary; evenly scheduled means every gap is the same.",
                   path = list(list("arrival", "process")), step = NA, default = list("poisson"),
                   choices = list(c("Random (exponential gaps)" = "poisson", "Evenly scheduled" = "fixed"))),
    num("mean_interarrival_h", "arrivals", "Mean time between releases (hours)", "On average, how many hours pass between one unit being released to the line and the next.", list("arrival", "mean_interarrival_h"), 1),
    num("kit_delay_mean_h", "kitting", "Lead time (hours)", "Average wait for a unit's long-lead parts kit after the unit is released. The unit can't start Integration until its kit arrives.", list("kit", "delay_mean_h"), 1),
    num("kit_delay_cv", "kitting", "Coefficient of variation", "How much the lead time varies from kit to kit: the standard deviation divided by the mean. 0 means every kit takes exactly the lead time; 0.6 means a typical kit is about 60% early or late.", list("kit", "delay_cv"), 0.05),
    num("kit_late_extra_h", "kitting", "Delay time (hours)", "When a kit is held up, the average number of extra hours it adds (each delay is drawn at random around this mean).", list("kit", "late_extra_mean_h"), 1),
    num("kit_p_late", "kitting", "Delay probability", "Chance, from 0 to 1, that a kit is held up by an extra delay on top of its lead time.", list("kit", "p_late"), 0.01, max = 1),
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
                   hint = c("Units closest to their due date go first. With this on, an urgent unit can take the chamber from a unit that has more time to spare.",
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
    num("acc_mean_h", "acceptance", "Final acceptance: mean (hours)", "How long final acceptance usually takes before the unit ships.", list("acceptance", "mean_h"), 1),
    num("acc_cv", "acceptance", "Final acceptance: coefficient of variation", "How much final acceptance time varies from unit to unit.", list("acceptance", "cv"), 0.05)
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
  "kitting",    "Kitting (long-lead parts)",     "boxes",
  "capacity",   "Capacities",                    "diagram-3",
  "quality",    "Test failures",                 "exclamation-triangle",
  "chamber",    "Environmental chamber",         "thermometer-half",
  "mrb",        "Material review board",         "people",
  "acceptance", "Final acceptance",              "check2-circle"
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
sc_params_from_inputs <- function(input, variants = sc_variants_default()) {
  set <- sc_line_fields() %>%
    dplyr::filter(!purrr::map_lgl(path, is.null)) %>%
    dplyr::mutate(value = purrr::map(id, ~ input[[.x]])) %>%
    dplyr::filter(!purrr::map_lgl(value, ~ is.null(.x) || (length(.x) == 1 && is.na(.x))))
  p <- purrr::reduce2(set$path, set$value, function(acc, path, value) purrr::assign_in(acc, path, value),
                      .init = sc_default_params())
  p$mrb$p_rework <- sc_rework_share(p$mrb$p_use_as_is, p$mrb$p_scrap)
  p$sim_hours <- sc_horizon_hours(input$horizon_quarters %||% 4, input$horizon_weeks %||% 0, input$horizon_days %||% 0)
  p$chamber$preempt <- isTRUE(p$chamber$preempt)
  p$chamber$restart <- isTRUE(p$chamber$restart)
  p$variants <- variants
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
