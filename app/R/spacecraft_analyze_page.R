# spacecraft_analyze_page.R
# Analyze page: why the line performs as it does for the current Settings
# (or the last Simulate run). Instant from a preloaded baseline; live runs
# use a cooperative progress bar and Cancel, like Compare.

# 100% stacked bars of cycle-time share, one row per variant plus Overall.
sc_plot_analyze_breakdown <- function(breakdown, narrow = FALSE) {
  parts <- sc_time_parts()
  fill_col <- if (isTRUE(narrow)) "short" else "label"
  fill_levels <- if (isTRUE(narrow)) parts$short else parts$label
  pal <- stats::setNames(parts$color, fill_levels)
  d <- breakdown %>%
    dplyr::select(-dplyr::any_of(c("label", "short", "color", "tip"))) %>%
    dplyr::left_join(parts, by = "part") %>%
    dplyr::mutate(
      variant = factor(.data$variant, levels = rev(levels(.data$variant))),
      fill = factor(.data[[fill_col]], levels = fill_levels)
    )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = share, y = variant, fill = fill)) +
    ggplot2::geom_col(width = 0.72, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 4),
      expand = ggplot2::expansion(mult = c(0, if (isTRUE(narrow)) 0.12 else 0.04))
    ) +
    ggplot2::scale_fill_manual(values = pal, breaks = fill_levels, name = NULL) +
    ggplot2::labs(x = "Share of cycle time", y = NULL) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (isTRUE(narrow)) {
    # Extra right margin and scale expansion so the last tick ("100%")
    # is not clipped at phone width. A two-row legend still cuts "Rework"
    # at this width and base size 16, so keep three rows.
    p <- p +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 14, 5.5, 5.5)) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3, byrow = TRUE, title = NULL))
  } else {
    p <- p + sc_one_row_legend()
  }
  p
}

# Hours and share for each part, one row per variant.
sc_table_analyze_breakdown <- function(breakdown) {
  parts <- sc_time_parts()
  variant_levels <- c("Overall", setdiff(levels(breakdown$variant), "Overall"))
  cell <- function(h, share) sprintf("%.1f h (%.0f%%)", h, 100 * share)
  wide <- breakdown %>%
    dplyr::select(-dplyr::any_of(c("label", "short", "color", "tip"))) %>%
    dplyr::left_join(parts, by = "part") %>%
    dplyr::mutate(cell = cell(.data$mean_h, .data$share),
                  variant = factor(as.character(.data$variant), levels = variant_levels)) %>%
    dplyr::select(variant, label, cell, cycle_h) %>%
    tidyr::pivot_wider(names_from = label, values_from = cell) %>%
    dplyr::arrange(.data$variant) %>%
    dplyr::mutate(
      variant = as.character(.data$variant),
      `Cycle time` = sprintf("%.1f h", .data$cycle_h)
    ) %>%
    dplyr::select(Variant = variant, dplyr::all_of(parts$label), `Cycle time`)
  tips <- c(
    stats::setNames(
      lapply(seq_len(nrow(parts)), function(i) {
        gt::html(sprintf('<span title="%s">%s</span>',
                         htmltools::htmlEscape(parts$tip[[i]]),
                         htmltools::htmlEscape(parts$label[[i]])))
      }),
      parts$label
    ),
    list(`Cycle time` = "Cycle time")
  )
  wide %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_width(Variant ~ gt::px(90)) %>%
    gt::cols_align("right", columns = -Variant) %>%
    gt::cols_label(.list = tips) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4)) %>%
    gt::tab_source_note("Each cell is the mean across shipped units and replications. Hover a column name for a short description.")
}

# Utilization bars; the bottleneck is highlighted, queue is written on the bar,
# and a dashed line marks 100% busy.
sc_plot_analyze_util <- function(resources, narrow = FALSE) {
  d <- resources %>%
    dplyr::arrange(.data$util) %>%
    dplyr::mutate(
      txt = sprintf("%.0f%% busy, queue %.2f", 100 * .data$util, .data$queue_avg),
      y_lab = if (isTRUE(narrow)) .data$short else .data$label,
      y = factor(.data$y_lab, levels = .data$y_lab)
    )
  xmax <- if (isTRUE(narrow)) 1.05 else max(1.05, max(d$util) + 0.42)
  pal <- c("TRUE" = "#d95f02", "FALSE" = "#2c7fb8")
  p <- ggplot2::ggplot(d, ggplot2::aes(x = util, y = y, fill = is_bottleneck)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1, colour = "#1a1a1a") +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 4),
      limits = c(0, xmax),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::labs(x = "Utilization", y = NULL) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (!isTRUE(narrow)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = txt), hjust = -0.05,
                                size = 16 / ggplot2::.pt, colour = "#1a1a1a")
  }
  p
}


# Utilization and average queue, with the bottleneck row highlighted.
sc_table_analyze_util <- function(resources, narrow = FALSE) {
  name_col <- if (isTRUE(narrow)) "short" else "label"
  util_nm <- if (isTRUE(narrow)) "Util." else "Utilization"
  queue_nm <- if (isTRUE(narrow)) "Queue" else "Average queue (units)"
  d <- resources %>%
    dplyr::arrange(dplyr::desc(.data$util)) %>%
    dplyr::transmute(
      Station = .data[[name_col]],
      util_l = sprintf("%.0f%%", 100 * .data$util),
      queue_l = sprintf("%.2f", .data$queue_avg),
      is_bottleneck
    )
  names(d)[names(d) == "util_l"] <- util_nm
  names(d)[names(d) == "queue_l"] <- queue_nm
  g <- d %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_hide(is_bottleneck)
  g <- if (isTRUE(narrow)) {
    gt::cols_width(g, Station ~ gt::px(110))
  } else {
    gt::cols_width(g, Station ~ gt::px(280))
  }
  g %>%
    gt::cols_align("right", columns = dplyr::all_of(c(util_nm, queue_nm))) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#fce4c2"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = .data$is_bottleneck)
    ) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4)) %>%
    gt::tab_source_note("The highlighted row is the busiest station. Utilization is the time-average share of servers that are busy.")
}

# Queue length over the horizon for the top queues: mean line and a P10–P90 ribbon.
sc_plot_analyze_queues <- function(queues, narrow = FALSE) {
  cat <- sc_resource_catalog()
  pal <- c(Chamber = "#2c7fb8", Integration = "#1b9e77", `Func. test` = "#d95f02",
           MRB = "#e0a032", Acceptance = "#756bb1")
  d <- queues %>%
    dplyr::mutate(
      day = .data$time_h / 24,
      series = factor(.data$short, levels = unique(.data$short[order(.data$resource)]))
    )
  used <- levels(d$series)
  ggplot2::ggplot(d, ggplot2::aes(x = day, y = mean, colour = series, fill = series)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p10, ymax = p90), alpha = 0.22, colour = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = pal[used], breaks = used, name = NULL) +
    ggplot2::scale_fill_manual(values = pal[used], breaks = used, name = NULL) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 4),
                                expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::labs(x = "Day", y = "Queue (units)") +
    theme_sc() +
    if (isTRUE(narrow)) {
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, title = NULL),
                      fill = ggplot2::guide_legend(ncol = 1, title = NULL))
    } else {
      sc_one_row_legend()
    }
}

sc_analyze_status_ui <- function(state) {
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

sc_analyze_ui <- function() {
  nav_panel(
    "Analyze",
    tags$style(HTML("
      .sc-analyze-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .sc-analyze-table-wrap .gt_table { width: 100%; }
      .sc-analyze-table-wrap .gt_sourcenote,
      .sc-analyze-table-wrap .gt_sourcenotes { white-space: normal; }
      @media (max-width: 800px) {
        .sc-analyze-breakdown-wrap .gt_table { min-width: 640px; }
      }
    ")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Analyze", width = 320, open = "desktop",
        p(class = "small",
          "This page explains why the line performs as it does: where a unit's cycle time goes, which station is the bottleneck, and how the busiest queues move over the year."),
        actionButton("analyze_run", tagList(icon("play"), " Analyze current Settings"),
                     class = "btn-primary w-100 mb-2"),
        actionButton("analyze_from_sim", "Use last Simulate run", class = "btn-outline-primary w-100 mb-2"),
        actionButton("analyze_restore", "Restore preloaded baseline", class = "btn-outline-secondary w-100 mb-2"),
        actionButton("analyze_cancel", "Cancel", class = "btn-outline-secondary w-100 mb-3"),
        numericInput("analyze_reps", "Replications", 20, min = 3, max = 40, step = 1),
        numericInput("analyze_seed", "Seed", 42, min = 1, step = 1),
        helpText("The preloaded view is the default line, 20 replications, seed 42 (the same baseline as Compare). Live runs can be cancelled between replications.")
      ),
      uiOutput("analyze_status"),
      card(card_header("What this says"), uiOutput("analyze_story")),
      card(
        card_header("Time breakdown per unit"),
        plotOutput("analyze_breakdown_plot", height = 340),
        div(class = "sc-analyze-table-wrap sc-analyze-breakdown-wrap", gt::gt_output("analyze_breakdown_table")),
        p(class = "small text-muted mb-0",
          "Each bar is the share of cycle time (release to ship) for that variant.",
          "Hours are in the table. Failed / rework is the extra queue and work after a unit rolls back to Integration.",
          "MRB hold is waiting for the board plus the review itself.")
      ),
      navset_card_tab(
        id = "analyze_station_tabs",
        nav_panel(
          "Bottleneck",
          plotOutput("analyze_util_plot", height = 340),
          div(class = "sc-analyze-table-wrap", gt::gt_output("analyze_util_table")),
          p(class = "small text-muted mb-0",
            "The highlighted bar is the busiest station. The dashed line is 100% busy.",
            "The label also shows the time-average queue in front of that station, in units.")
        ),
        nav_panel(
          "Queue length",
          plotOutput("analyze_queue_plot", height = 440),
          p(class = "small text-muted mb-0",
            "The line is the mean queue across replications; the ribbon runs from the 10th to the 90th percentile.",
            "The plot shows the two or three busiest queues (and the bottleneck, if it is not already in that set).")
        )
      )
    )
  )
}

# Wire the page into sc_server(). simulate_result is the Simulate eventReactive
# (or NULL); when a run is available we can reuse its diagnostics instantly.
sc_analyze_server <- function(input, output, session, current_params, simulate_result = NULL) {
  preload <- sc_load_preload_analyze()
  result <- reactiveVal(preload)
  running <- reactiveVal(FALSE)
  cancel_flag <- reactiveVal(FALSE)
  job <- reactiveVal(NULL)
  status <- reactiveVal(list(
    kind = if (is.null(preload)) "idle" else "done",
    text = if (is.null(preload)) {
      "No preloaded results found. Click Analyze current Settings to generate them."
    } else {
      sprintf("Showing the preloaded baseline (%d replications, seed %s). Analyze the current Settings, or use the last Simulate run if you have one.",
              preload$meta$n_reps, preload$meta$base_seed)
    },
    pct = NULL
  ))

  matches_preload <- function(params, n_reps, seed) {
    !is.null(preload) &&
      n_reps == preload$meta$n_reps &&
      seed == preload$meta$base_seed &&
      identical(params, preload$params)
  }

  observeEvent(input$analyze_restore, {
    if (is.null(preload)) {
      result(NULL)
      status(list(kind = "idle", text = "No preloaded results found. Click Analyze current Settings to generate them.", pct = NULL))
    } else {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Showing the preloaded baseline (%d replications, seed %s).",
                                 preload$meta$n_reps, preload$meta$base_seed)))
    }
  })

  observeEvent(input$analyze_from_sim, {
    if (!is.function(simulate_result)) {
      showNotification("Run the line on the Simulate page first.", type = "warning")
      return()
    }
    sim <- tryCatch(simulate_result(), error = function(e) NULL)
    if (is.null(sim) || is.null(sim$analyze)) {
      showNotification("Run the line on the Simulate page first, then come back to use those results here.",
                       type = "warning")
      return()
    }
    result(sim$analyze)
    status(list(kind = "done", pct = NULL,
                text = sprintf("Showing the last Simulate run (%d replications, seed %s).",
                               sim$analyze$meta$n_reps %||% nrow(sim$per_rep),
                               input$seed %||% 42)))
  })

  observeEvent(input$analyze_run, {
    if (isTRUE(running())) return()
    p <- current_params()
    n_reps <- as.integer(input$analyze_reps %||% 20)
    seed <- as.integer(input$analyze_seed %||% 42)
    if (matches_preload(p, n_reps, seed)) {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Those settings match the preloaded baseline, so the saved results were reused (%d replications, seed %s).",
                                 n_reps, seed)))
      return()
    }
    cancel_flag(FALSE)
    running(TRUE)
    job(list(params = p, n_reps = n_reps, base_seed = seed,
             i_rep = 1L, acc = vector("list", n_reps),
             t0 = Sys.time(), done = 0L))
    status(list(kind = "running", pct = 0,
                text = sprintf("Analyzing current Settings, %d replications…", n_reps)))
  })

  observeEvent(input$analyze_cancel, {
    if (isTRUE(running())) {
      cancel_flag(TRUE)
      status(list(kind = "cancelled", pct = NULL, text = "Cancelling after the current replication…"))
    }
  })

  observe({
    req(running())
    invalidateLater(20)
    isolate({
      j <- job()
      if (is.null(j)) { running(FALSE); return() }
      if (isTRUE(cancel_flag())) {
        job(NULL); running(FALSE)
        status(list(kind = "cancelled", pct = 100 * j$done / max(j$n_reps, 1),
                    text = sprintf("Cancelled after %d of %d replications.", j$done, j$n_reps)))
        return()
      }
      r <- j$i_rep
      seed <- as.integer(j$base_seed + r - 1L)
      out <- sc_run_one(j$params, seed)
      out$rep <- r
      j$acc[[r]] <- out
      j$done <- j$done + 1L
      j$i_rep <- r + 1L
      pct <- 100 * j$done / j$n_reps
      status(list(kind = "running", pct = pct,
                  text = sprintf("Analyzing current Settings, replication %d of %d…", r, j$n_reps)))
      if (j$i_rep > j$n_reps) {
        a <- sc_analyze(j$acc, j$params)
        a$meta$n_reps <- j$n_reps
        a$meta$base_seed <- j$base_seed
        a$meta$secs <- as.numeric(difftime(Sys.time(), j$t0, units = "secs"))
        a$meta$source <- "live"
        result(a)
        job(NULL); running(FALSE)
        status(list(kind = "done", pct = 100,
                    text = sprintf("Done. %d replications in %.1f s, seed %s.",
                                   j$n_reps, a$meta$secs, j$base_seed)))
      } else {
        job(j)
      }
    })
  })

  output$analyze_status <- renderUI(sc_analyze_status_ui(status()))

  output$analyze_story <- renderUI({
    r <- result()
    validate(need(!is.null(r) && is.character(r$story) && nzchar(r$story),
                  "Run an analysis, or restore the preloaded baseline, to fill this summary."))
    p(class = "mb-0", r$story)
  })

  output$analyze_breakdown_plot <- renderPlot({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$breakdown) && nrow(r$breakdown),
                  "Run an analysis to draw the time breakdown."))
    w <- session$clientData$output_analyze_breakdown_plot_width
    sc_plot_analyze_breakdown(r$breakdown, narrow = isTRUE(w < 700))
  }, res = 96)

  output$analyze_breakdown_table <- gt::render_gt({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$breakdown) && nrow(r$breakdown),
                  "Run an analysis to fill this table."))
    sc_table_analyze_breakdown(r$breakdown)
  })

  output$analyze_util_plot <- renderPlot({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$resources) && nrow(r$resources),
                  "Run an analysis to draw the bottleneck view."))
    w <- session$clientData$output_analyze_util_plot_width
    sc_plot_analyze_util(r$resources, narrow = isTRUE(w < 700))
  }, res = 96)

  output$analyze_util_table <- gt::render_gt({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$resources) && nrow(r$resources),
                  "Run an analysis to fill this table."))
    w <- session$clientData$output_analyze_util_plot_width
    sc_table_analyze_util(r$resources, narrow = isTRUE(w < 700))
  })

  output$analyze_queue_plot <- renderPlot({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$queues) && nrow(r$queues),
                  "Run an analysis to draw the queue ribbons."))
    w <- session$clientData$output_analyze_queue_plot_width
    sc_plot_analyze_queues(r$queues, narrow = isTRUE(w < 700))
  }, res = 96)
}
