# spacecraft_inputs.R
# One tibble describes every scalar sidebar input: its id, label, accordion
# panel, and the path into the params list it sets. The sidebar is built from
# it, and sc_params_from_inputs() writes inputs back with purrr::reduce, so a
# new parameter needs one new row here and nothing else.

sc_input_spec <- function() {
  tibble::tribble(
    ~id,                  ~panel,       ~label,                                  ~path,                         ~step,
    "sim_hours",          "run",        "Horizon (hours)",                       list("sim_hours"),             168,
    "mean_interarrival_h","arrivals",   "Mean time between releases (h)",        list("arrival", "mean_interarrival_h"), 1,
    "kit_delay_mean_h",   "kitting",    "Kit delay mean (h)",                    list("kit", "delay_mean_h"),   1,
    "kit_delay_cv",       "kitting",    "Kit delay CV",                          list("kit", "delay_cv"),       0.05,
    "kit_p_late",         "kitting",    "Chance a kit slips",                    list("kit", "p_late"),         0.01,
    "kit_late_extra_h",   "kitting",    "Mean extra slip (h)",                   list("kit", "late_extra_mean_h"), 1,
    "cap_integration",    "capacity",   "Integration stations (units worked at once)",                      list("capacity", "integration"), 1,
    "cap_func_test",      "capacity",   "Functional test stands",                list("capacity", "func_test"), 1,
    "cap_chamber",        "capacity",   "Environmental chambers (vibe + TVAC)",  list("capacity", "chamber"),   1,
    "cap_review_board",   "capacity",   "MRB boards",                            list("capacity", "review_board"), 1,
    "cap_acceptance",     "capacity",   "Acceptance stands",                     list("capacity", "acceptance"), 1,
    "p_fail_func",        "quality",    "Functional test fail chance",           list("p_fail_func"),           0.01,
    "p_fail_env",         "quality",    "Environmental test fail chance",        list("p_fail_env"),            0.01,
    "max_fails",          "quality",    "Fails before forced MRB",               list("max_fails"),             1,
    "mrb_mean_h",         "mrb",        "MRB hold mean (h)",                     list("mrb", "mean_h"),         1,
    "mrb_cv",             "mrb",        "MRB hold CV",                           list("mrb", "cv"),             0.05,
    "mrb_p_use_as_is",    "mrb",        "Use-as-is share",                       list("mrb", "p_use_as_is"),    0.05,
    "mrb_p_rework",       "mrb",        "Rework share",                          list("mrb", "p_rework"),       0.05,
    "mrb_p_scrap",        "mrb",        "Scrap share",                           list("mrb", "p_scrap"),        0.05,
    "acc_mean_h",         "acceptance", "Acceptance mean (h)",                   list("acceptance", "mean_h"),  1,
    "acc_cv",             "acceptance", "Acceptance CV",                         list("acceptance", "cv"),      0.05
  ) %>%
    # Walk each path into the defaults list: list("kit", "p_late") -> p$kit$p_late
    dplyr::mutate(default = purrr::map_dbl(path, function(x) purrr::reduce(x, `[[`, .init = sc_default_params())))
}

sc_panel_inputs <- function(panel_id, ns = identity) {
  sc_input_spec() %>%
    dplyr::filter(panel == panel_id) %>%
    purrr::pmap(function(id, label, default, step, ...) {
      shiny::numericInput(ns(id), label, value = default, min = 0, step = step)
    })
}

# Inputs (plus the non-numeric ones) -> a full params list.
sc_params_from_inputs <- function(input, variants = sc_variants_default()) {
  p <- sc_input_spec() %>%
    dplyr::filter(!purrr::map_lgl(id, ~ is.null(input[[.x]]) || is.na(input[[.x]]))) %>%
    { purrr::reduce2(.$path, purrr::map(.$id, ~ input[[.x]]),
                     function(acc, path, value) purrr::assign_in(acc, path, value),
                     .init = sc_default_params()) }
  p$arrival$process <- input$arrival_process %||% p$arrival$process
  p$chamber$preempt <- isTRUE(input$chamber_preempt %||% p$chamber$preempt)
  p$chamber$restart <- isTRUE(input$chamber_restart %||% p$chamber$restart)
  p$variants <- variants
  p
}
