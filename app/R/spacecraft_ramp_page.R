# spacecraft_ramp_page.R
# Ramp page: a target rate curve, a learning curve, hire and procurement lead
# times, a recommended capacity-move timeline, and charts of throughput versus
# the target rate (the one simple "meets target rate" check) plus cycle time,
# HPU, labor utilization, bottleneck utilization and WIP. Instant from a
# preloaded plan; live runs have progress and Cancel.

sc_ramp_colors <- c(
  throughput = "#2c7fb8",
  target = "#1a1a1a",
  cycle = "#2c7fb8",
  hpu = "#2c7fb8",
  labor = "#17becf",
  station = "#e0a032",
  wip = "#2c7fb8",
  meet = "#1b9e77",
  miss = "#d95f02",
  tech = "#17becf",
  shift = "#2c7fb8",
  order = "#e0a032"
)

sc_ramp_progress <- function(state) {
  cls <- switch(state$kind,
                running = "alert alert-info py-2",
                done = "alert alert-success py-2",
                cancelled = "alert alert-warning py-2",
                error = "alert alert-danger py-2",
                "alert alert-light py-2")
  bar <- if (is.null(state$pct)) NULL else div(
    class = "progress mt-2 mb-0", style = "height: 0.7rem;",
    div(class = "progress-bar", role = "progressbar",
        style = sprintf("width: %.0f%%; background-color: #2c7fb8;", state$pct))
  )
  div(class = cls, p(class = "mb-0", state$text), bar)
}

sc_ramp_rate_ui <- function(schedule = sc_ramp_default_schedule()) {
  inputs <- lapply(seq_len(nrow(schedule)), function(i) {
    numericInput(
      paste0("ramp_rate_q", schedule$quarter[[i]]),
      sprintf("Q%d (month %s)", schedule$quarter[[i]], schedule$start_month[[i]]),
      value = schedule$target_rate[[i]], min = 1, max = 400, step = 1
    )
  })
  div(class = "sc-ramp-rates", inputs)
}

sc_ramp_schedule_from_input <- function(input, schedule = sc_ramp_default_schedule()) {
  rates <- vapply(seq_len(nrow(schedule)), function(i) {
    id <- paste0("ramp_rate_q", schedule$quarter[[i]])
    val <- input[[id]]
    if (is.null(val) || !is.finite(val)) schedule$target_rate[[i]] else as.numeric(val)
  }, numeric(1))
  schedule$target_rate <- pmax(1, rates)
  sc_ramp_normalize_schedule(schedule)
}

sc_ramp_cfg_from_input <- function(input) {
  d <- sc_ramp_defaults()
  d$learning_pct <- as.numeric(input$ramp_learning %||% d$learning_pct)
  d$learning_ref_n <- as.numeric(input$ramp_ref_n %||% d$learning_ref_n)
  d$tech_lead_months <- as.numeric(input$ramp_tech_lead %||% d$tech_lead_months)
  d$station_lead_months <- as.numeric(input$ramp_station_lead %||% d$station_lead_months)
  d$chamber_lead_months <- as.numeric(input$ramp_chamber_lead %||% d$chamber_lead_months)
  d$meet_frac <- 0.95
  d
}

sc_ramp_start_params <- function(params, start = "lean") {
  if (identical(start, "settings")) sc_params_copy(params) else sc_ramp_lean_params(params)
}

# --- plots -----------------------------------------------------------------

sc_plot_ramp_throughput <- function(bundle, narrow = FALSE) {
  d <- bundle$points
  if (is.null(d) || !nrow(d)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No ramp results yet"))
  }
  frac <- bundle$cfg$meet_frac %||% 0.95
  d$meet_lab <- ifelse(d$meets_target, "Meets target", "Misses target")
  xlab <- if (isTRUE(narrow)) "Month" else "Month from the start of the ramp"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = start_month, y = shipped_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = shipped_p10, ymax = shipped_p90),
                         fill = sc_ramp_colors[["throughput"]], alpha = 0.22, colour = NA) +
    ggplot2::geom_line(ggplot2::aes(y = target_rate), linetype = "dashed",
                       colour = sc_ramp_colors[["target"]], linewidth = 1) +
    ggplot2::geom_line(colour = sc_ramp_colors[["throughput"]], linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(colour = meet_lab, shape = meet_lab), size = 3) +
    ggplot2::scale_colour_manual(
      values = c("Meets target" = sc_ramp_colors[["meet"]],
                 "Misses target" = sc_ramp_colors[["miss"]]),
      name = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = c("Meets target" = 16, "Misses target" = 17),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = d$start_month,
                                expand = ggplot2::expansion(mult = c(0.04, 0.08))) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.10))
    ) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0, NA)) +
    ggplot2::labs(x = xlab, y = if (isTRUE(narrow)) "Units/year" else "Throughput (units/year)") +
    theme_sc() +
    sc_one_row_legend()
  p
}

sc_plot_ramp_metric <- function(bundle, y, lo, hi, ylab, colour = sc_ramp_colors[["cycle"]],
                                y_pct = FALSE, hline = NULL, narrow = FALSE) {
  d <- bundle$points
  if (is.null(d) || !nrow(d)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No ramp results yet"))
  }
  d$y <- d[[y]]
  d$lo <- d[[lo]]
  d$hi <- d[[hi]]
  xlab <- if (isTRUE(narrow)) "Month" else "Month from the start of the ramp"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = start_month, y = y)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                         fill = colour, alpha = 0.22, colour = NA) +
    ggplot2::geom_line(colour = colour, linewidth = 1.1) +
    ggplot2::geom_point(colour = colour, size = 2.8) +
    ggplot2::scale_x_continuous(breaks = d$start_month,
                                expand = ggplot2::expansion(mult = c(0.04, 0.08))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = xlab, y = ylab) +
    theme_sc() +
    ggplot2::theme(legend.position = "none")
  if (isTRUE(y_pct)) {
    p <- p + ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.10))
    )
  } else {
    p <- p + ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.10))
    )
  }
  if (!is.null(hline)) {
    p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  p
}

sc_plot_ramp_util <- function(bundle, narrow = FALSE) {
  d <- bundle$points
  if (is.null(d) || !nrow(d)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No ramp results yet"))
  }
  long <- dplyr::bind_rows(
    d %>% dplyr::transmute(start_month, series = "Labor", util = labor_util_mean),
    d %>% dplyr::transmute(start_month, series = "Busiest station", util = busiest_station_util)
  )
  pal <- c("Labor" = sc_ramp_colors[["labor"]],
           "Busiest station" = sc_ramp_colors[["station"]])
  shapes <- c("Labor" = 23, "Busiest station" = 17)
  xlab <- if (isTRUE(narrow)) "Month" else "Month from the start of the ramp"
  ggplot2::ggplot(long, ggplot2::aes(x = start_month, y = util,
                                     colour = series, shape = series)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.8, fill = "white") +
    ggplot2::geom_hline(yintercept = 0.90, linetype = "dashed",
                        linewidth = 1, colour = "#1a1a1a") +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_shape_manual(values = shapes, name = NULL) +
    ggplot2::scale_x_continuous(breaks = d$start_month,
                                expand = ggplot2::expansion(mult = c(0.04, 0.08))) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 5),
                                expand = ggplot2::expansion(mult = c(0.02, 0.10))) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0, NA)) +
    ggplot2::labs(x = xlab, y = if (isTRUE(narrow)) "Utilization" else "Utilization") +
    theme_sc() +
    sc_one_row_legend()
}

sc_plot_ramp_gantt <- function(bundle, narrow = FALSE) {
  m <- bundle$moves
  if (is.null(m) || !nrow(m)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No capacity moves on this plan"))
  }
  m <- m %>%
    dplyr::mutate(
      y = factor(.data$label, levels = rev(unique(.data$label))),
      series = dplyr::case_when(
        .data$kind == "tech" ~ "Technician hire",
        .data$kind == "shift" ~ "Shift add",
        TRUE ~ "Station order"
      )
    )
  pal <- c("Technician hire" = sc_ramp_colors[["tech"]],
           "Shift add" = sc_ramp_colors[["shift"]],
           "Station order" = sc_ramp_colors[["order"]])
  xmax <- max(c(m$arrive_month, m$need_month, bundle$schedule$start_month), na.rm = TRUE)
  xlab <- if (isTRUE(narrow)) "Month" else "Month from the start of the ramp"
  ggplot2::ggplot(m, ggplot2::aes(y = y, colour = series)) +
    ggplot2::geom_segment(ggplot2::aes(x = order_month, xend = arrive_month,
                                       yend = y), linewidth = 4, lineend = "butt") +
    ggplot2::geom_point(ggplot2::aes(x = order_month), shape = 15, size = 3.2) +
    ggplot2::geom_point(ggplot2::aes(x = need_month), shape = 17, size = 3.2,
                        colour = "#1a1a1a") +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, xmax + 1),
                                expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::labs(x = xlab, y = NULL) +
    theme_sc() +
    sc_one_row_legend()
}

# --- tables ----------------------------------------------------------------

sc_table_ramp_plan <- function(bundle) {
  d <- bundle$points
  if (is.null(d) || !nrow(d)) {
    return(gt::gt(tibble::tibble(Note = "Run a plan to fill this table.")) %>% sc_gt_style())
  }
  frac <- bundle$cfg$meet_frac %||% 0.95
  out <- d %>%
    dplyr::transmute(
      quarter,
      start_month,
      target_rate,
      shipped_mean,
      meets = ifelse(.data$meets_target, "Yes", "No"),
      cycle_mean,
      hpu_mean,
      labor_util_mean,
      wip_mean,
      bottleneck_label
    )
  gt::gt(out) %>%
    gt::fmt_number(c(target_rate, shipped_mean, cycle_mean, hpu_mean, wip_mean), decimals = 1) %>%
    gt::fmt_percent(labor_util_mean, decimals = 0) %>%
    gt::cols_label(
      quarter = "Quarter",
      start_month = "Month",
      target_rate = "Target (units/year)",
      shipped_mean = "Throughput (units/year)",
      meets = "Meets target",
      cycle_mean = "Cycle time (h)",
      hpu_mean = "HPU (h)",
      labor_util_mean = "Labor utilization",
      wip_mean = "WIP",
      bottleneck_label = "Projected bottleneck"
    ) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#fce4c2"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = meets == "No")
    ) %>%
    gt::tab_source_note(sprintf(
      "A quarter meets the target when mean annualized throughput is at least %.0f%% of the planned rate. Each quarter is a one-year snapshot at that rate, with touch times scaled by the learning curve at the midpoint cumulative volume. Numbers are illustrative.",
      100 * frac
    )) %>%
    sc_gt_style()
}

sc_table_ramp_moves <- function(bundle) {
  m <- bundle$moves
  if (is.null(m) || !nrow(m)) {
    return(gt::gt(tibble::tibble(Note = "No capacity moves are recommended on this ramp.")) %>% sc_gt_style())
  }
  out <- m %>%
    dplyr::transmute(
      label,
      order_month,
      arrive_month,
      need_month,
      lead_months,
      when = ifelse(.data$ordered_late, "Order now (arrives after it is first needed)",
                    "Order on time"),
      detail
    )
  gt::gt(out) %>%
    gt::fmt_number(c(order_month, arrive_month, need_month, lead_months), decimals = 0) %>%
    gt::cols_label(
      label = "Move",
      order_month = "Order by (month)",
      arrive_month = "In place (month)",
      need_month = "First needed (month)",
      lead_months = "Lead time (months)",
      when = "Timing",
      detail = "Why"
    ) %>%
    gt::tab_source_note("Order-by is the need date minus the hire-and-train or procurement lead time. Squares on the timeline are order dates; black triangles are the first quarter that needs the move. Numbers are illustrative.") %>%
    sc_gt_style()
}

sc_table_ramp_delay <- function(bundle) {
  d <- bundle$delays
  if (is.null(d) || !nrow(d)) {
    return(gt::gt(tibble::tibble(Note = "No delayed-move cases to show.")) %>% sc_gt_style())
  }
  planned <- bundle$points %>%
    dplyr::select(quarter, planned_shipped = shipped_mean, planned_meets = meets_target,
                  planned_cycle = cycle_mean, planned_labor = labor_util_mean)
  out <- d %>%
    dplyr::left_join(planned, by = "quarter") %>%
    dplyr::transmute(
      delayed_label,
      quarter,
      start_month,
      target_rate,
      shipped_mean,
      meets = ifelse(.data$meets_target, "Yes", "No"),
      planned_shipped,
      cycle_mean,
      labor_util_mean
    )
  gt::gt(out) %>%
    gt::fmt_number(c(target_rate, shipped_mean, planned_shipped, cycle_mean), decimals = 1) %>%
    gt::fmt_percent(labor_util_mean, decimals = 0) %>%
    gt::cols_label(
      delayed_label = "Move delayed",
      quarter = "Quarter",
      start_month = "Month",
      target_rate = "Target",
      shipped_mean = "Throughput if delayed",
      meets = "Still meets target",
      planned_shipped = "Planned throughput",
      cycle_mean = "Cycle time if delayed (h)",
      labor_util_mean = "Labor utilization if delayed"
    ) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#fce4c2"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = meets == "No")
    ) %>%
    gt::tab_source_note("Each row re-runs the affected quarter with that one move slipped by its lead time (the other moves stay on the planned dates). Numbers are illustrative.") %>%
    sc_gt_style()
}

sc_ramp_caption <- function(kind) {
  switch(kind,
    throughput = "The solid line is mean annualized throughput at each quarter's rate and capacity; the ribbon is the 10th to 90th percentile across replications. The dashed line is the target rate. Circles meet the target (at least 95% of the planned units/year); triangles miss it. Numbers are illustrative.",
    cycle = "Mean cycle time at each quarter's rate and capacity. The ribbon is the 10th to 90th percentile across replications. Touch times follow the learning curve, so cycle time can fall even as the rate rises. Numbers are illustrative.",
    hpu = "Mean touch labor hours per unit (technicians times touch time, including rework). The ribbon is the 10th to 90th percentile. A learning curve below 100% pulls HPU down as cumulative volume grows. Numbers are illustrative.",
    util = "Labor utilization is busy technician-hours divided by scheduled technician-hours. The busiest station is the highest station utilization in that quarter. The dashed line is a 90% busy cap. Numbers are illustrative.",
    wip = "Mean work in process at each quarter's rate and capacity. The ribbon is the 10th to 90th percentile across replications. Numbers are illustrative.",
    gantt = "Each bar runs from the order-by date to the date the move is in place. Squares are order dates; black triangles mark the first quarter that needs the move. Numbers are illustrative.",
    "Numbers are illustrative."
  )
}

# --- UI + server -----------------------------------------------------------

sc_ramp_ui <- function() {
  nav_panel(
    "Ramp",
    tags$style(HTML("
      .sc-ramp-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .sc-ramp-table-wrap .gt_table { width: 100%; }
      .sc-ramp-table-wrap .gt_sourcenote,
      .sc-ramp-table-wrap .gt_sourcenotes { white-space: normal; }
      .sc-ramp-rates .form-group { margin-bottom: 0.4rem; }
    ")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Ramp", width = 320, open = "desktop",
        p(class = "small",
          "Plan a capacity ramp against a rising target rate. Each quarter is simulated at that rate with the capacity then in place. The page recommends technician, shift, or station moves and an order-by date (need date minus lead time), and it shows the dip if a move is delayed."),
        p(class = "small text-muted",
          title = "A quarter meets the target when mean annualized throughput is at least 95% of the planned units/year.",
          "The one headline check is whether throughput meets the target rate (at least 95%). Cycle time, HPU, labor utilization and WIP are shown alongside."),
        actionButton("ramp_run", tagList(icon("play"), " Run plan"),
                     class = "btn-primary w-100 mb-2"),
        actionButton("ramp_restore", "Restore preloaded plan",
                     class = "btn-outline-secondary w-100 mb-2"),
        actionButton("ramp_cancel", "Cancel", class = "btn-outline-secondary w-100 mb-3"),
        radioButtons(
          "ramp_start", "Starting line",
          choiceNames = list(
            "Lean early line (day shift, 2 technicians)",
            "Current Settings"
          ),
          choiceValues = list("lean", "settings"),
          selected = "lean"
        ),
        numericInput("ramp_learning", "Learning curve (% per doubling)", 85,
                     min = 50, max = 100, step = 1),
        numericInput("ramp_ref_n", "Settings times after this many units", 80,
                     min = 1, max = 500, step = 1),
        numericInput("ramp_tech_lead", "Technician hire and train lead time (months)", 3,
                     min = 0, max = 24, step = 1),
        numericInput("ramp_station_lead", "Station / fixture lead time (months)", 6,
                     min = 0, max = 24, step = 1),
        numericInput("ramp_chamber_lead", "Chamber lead time (months)", 9,
                     min = 0, max = 36, step = 1),
        tags$strong("Target rate (units/year)"),
        sc_ramp_rate_ui(),
        numericInput("ramp_reps", "Replications", 10, min = 3, max = 20, step = 1),
        numericInput("ramp_seed", "Seed", 42, min = 1, step = 1),
        helpText("The preloaded view is a lean early line (day shift, two technicians, one of each station) ramping from 12 to 48 units a year over 24 months, with an 85% learning curve, 10 replications and seed 42. A default three-shift line already holds this ramp; the lean start is what makes the plan show real order dates.")
      ),
      uiOutput("ramp_status"),
      card(card_header("What this says"), uiOutput("ramp_story")),
      navset_card_tab(
        id = "ramp_result_tabs",
        nav_panel(
          "Plan",
          div(class = "sc-ramp-table-wrap", gt::gt_output("ramp_table_plan")),
          div(class = "sc-ramp-table-wrap mt-3", gt::gt_output("ramp_table_moves"))
        ),
        nav_panel(
          "Timeline",
          plotOutput("ramp_plot_gantt", height = 360),
          p(class = "small text-muted mb-0", sc_ramp_caption("gantt"))
        ),
        nav_panel(
          "Throughput",
          plotOutput("ramp_plot_throughput", height = 400),
          p(class = "small text-muted mb-0", sc_ramp_caption("throughput"))
        ),
        nav_panel(
          "Cycle time",
          plotOutput("ramp_plot_cycle", height = 400),
          p(class = "small text-muted mb-0", sc_ramp_caption("cycle"))
        ),
        nav_panel(
          "Touch labor",
          plotOutput("ramp_plot_hpu", height = 400),
          p(class = "small text-muted mb-0", sc_ramp_caption("hpu"))
        ),
        nav_panel(
          "Utilization",
          plotOutput("ramp_plot_util", height = 400),
          p(class = "small text-muted mb-0", sc_ramp_caption("util"))
        ),
        nav_panel(
          "WIP",
          plotOutput("ramp_plot_wip", height = 400),
          p(class = "small text-muted mb-0", sc_ramp_caption("wip"))
        ),
        nav_panel(
          "If a move is delayed",
          div(class = "sc-ramp-table-wrap", gt::gt_output("ramp_table_delay"))
        )
      )
    )
  )
}

sc_ramp_server <- function(input, output, session, current_params) {
  preload <- sc_load_preload_ramp()
  result <- reactiveVal(preload)

  preload_msg <- function() {
    if (is.null(preload)) {
      "No preloaded ramp plan found. Click Run plan to generate one from the starting line and the rate table."
    } else {
      sprintf(
        "Showing the preloaded ramp plan (%d replications, seed %s, a lean early line ramping from 12 to 48 units/year). Change the rate table, the learning curve or the starting line and run a live plan, or restore this result.",
        preload$meta$n_reps, preload$meta$base_seed
      )
    }
  }

  status <- reactiveVal(list(
    kind = if (is.null(preload)) "idle" else "done",
    text = preload_msg(),
    pct = NULL
  ))
  cancel_flag <- reactiveVal(FALSE)

  matches_preload <- function(params, cfg, schedule, reps, seed, start) {
    !is.null(preload) &&
      identical(start, preload$meta$start %||% "lean") &&
      reps == preload$meta$n_reps &&
      seed == preload$meta$base_seed &&
      isTRUE(all.equal(cfg$learning_pct, preload$cfg$learning_pct)) &&
      isTRUE(all.equal(cfg$learning_ref_n, preload$cfg$learning_ref_n)) &&
      isTRUE(all.equal(cfg$tech_lead_months, preload$cfg$tech_lead_months)) &&
      isTRUE(all.equal(cfg$station_lead_months, preload$cfg$station_lead_months)) &&
      isTRUE(all.equal(cfg$chamber_lead_months, preload$cfg$chamber_lead_months)) &&
      isTRUE(all.equal(schedule$target_rate, preload$schedule$target_rate)) &&
      identical(params, preload$params)
  }

  output$ramp_status <- renderUI(sc_ramp_progress(status()))
  output$ramp_story <- renderUI({
    b <- result()
    p(class = "mb-0", if (is.null(b)) {
      "Run a plan, or restore the preloaded result, to fill this summary."
    } else {
      b$story
    })
  })

  observeEvent(input$ramp_restore, {
    cancel_flag(FALSE)
    if (is.null(preload)) {
      result(NULL)
      status(list(kind = "idle", text = preload_msg(), pct = NULL))
    } else {
      result(preload)
      status(list(kind = "done", pct = NULL, text = preload_msg()))
    }
  })

  observeEvent(input$ramp_cancel, {
    cancel_flag(TRUE)
    status(list(kind = "cancelled", pct = status()$pct,
                text = "Cancelling after the current replication…"))
  })

  observeEvent(input$ramp_run, {
    p0 <- current_params()
    start <- input$ramp_start %||% "lean"
    params <- sc_ramp_start_params(p0, start)
    cfg <- sc_ramp_cfg_from_input(input)
    schedule <- sc_ramp_schedule_from_input(input)
    reps <- as.integer(input$ramp_reps %||% 10)
    seed <- as.integer(input$ramp_seed %||% 42)
    if (matches_preload(params, cfg, schedule, reps, seed, start)) {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Those settings match the preloaded plan, so the saved result was reused (%d replications, seed %s).",
                                 preload$meta$n_reps, preload$meta$base_seed)))
      return(invisible())
    }
    cancel_flag(FALSE)
    status(list(kind = "running", pct = 0,
                text = "Running the ramp plan…"))
    bundle <- tryCatch({
      sc_ramp_run(
        params, schedule, cfg, n_reps = reps, base_seed = seed,
        on_progress = function(done, total, label) {
          pct <- min(100, round(100 * done / max(1, total)))
          status(list(kind = "running", pct = pct,
                      text = sprintf("Running %s (%d of %d replications)…", label, done, total)))
        },
        should_stop = function() isTRUE(cancel_flag())
      )
    }, error = function(e) {
      status(list(kind = "error", pct = NULL, text = paste("The ramp plan failed:", conditionMessage(e))))
      NULL
    })
    if (is.null(bundle)) return(invisible())
    if (isTRUE(bundle$meta$cancelled)) {
      result(bundle)
      status(list(kind = "cancelled", pct = NULL,
                  text = "Cancelled. Partial results are shown if any quarters finished."))
    } else {
      result(bundle)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Finished the ramp plan (%d replications, seed %s, %.1f s).",
                                 bundle$meta$n_reps, bundle$meta$base_seed, bundle$meta$secs)))
    }
  })

  output$ramp_table_plan <- gt::render_gt({
    b <- result()
    if (is.null(b)) {
      gt::gt(tibble::tibble(Note = "Run a plan, or restore the preloaded result, to fill this table.")) %>% sc_gt_style()
    } else sc_table_ramp_plan(b)
  })
  output$ramp_table_moves <- gt::render_gt({
    b <- result()
    if (is.null(b)) {
      gt::gt(tibble::tibble(Note = "Run a plan to see recommended moves.")) %>% sc_gt_style()
    } else sc_table_ramp_moves(b)
  })
  output$ramp_table_delay <- gt::render_gt({
    b <- result()
    if (is.null(b)) {
      gt::gt(tibble::tibble(Note = "Run a plan to see delayed-move cases.")) %>% sc_gt_style()
    } else sc_table_ramp_delay(b)
  })
  output$ramp_plot_gantt <- renderPlot({
    b <- result()
    if (is.null(b)) {
      ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No ramp results yet")
    } else sc_plot_ramp_gantt(b)
  }, res = 96)
  output$ramp_plot_throughput <- renderPlot({
    b <- result()
    if (is.null(b)) {
      ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No ramp results yet")
    } else sc_plot_ramp_throughput(b)
  }, res = 96)
  output$ramp_plot_cycle <- renderPlot({
    b <- result()
    if (is.null(b)) {
      ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No ramp results yet")
    } else sc_plot_ramp_metric(b, "cycle_mean", "cycle_p10", "cycle_p90", "Mean cycle time (h)")
  }, res = 96)
  output$ramp_plot_hpu <- renderPlot({
    b <- result()
    if (is.null(b)) {
      ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No ramp results yet")
    } else sc_plot_ramp_metric(b, "hpu_mean", "hpu_p10", "hpu_p90", "Touch labor (HPU, hours)")
  }, res = 96)
  output$ramp_plot_util <- renderPlot({
    b <- result()
    if (is.null(b)) {
      ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No ramp results yet")
    } else sc_plot_ramp_util(b)
  }, res = 96)
  output$ramp_plot_wip <- renderPlot({
    b <- result()
    if (is.null(b)) {
      ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No ramp results yet")
    } else sc_plot_ramp_metric(b, "wip_mean", "wip_p10", "wip_p90", "WIP (units)",
                               colour = sc_ramp_colors[["wip"]])
  }, res = 96)

  invisible(list(result = result))
}
