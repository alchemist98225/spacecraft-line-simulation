# spacecraft_warmup_page.R
# Warmup page: Welch-smoothed cycle time by release order and WIP by week,
# a suggested warm-up (dashed line), a plain-English summary of how the KPIs
# change if those early units are discarded, a preloaded result, and a live
# run with progress and Cancel.

sc_wu_colors <- c(Smooth = "#2c7fb8")
sc_wu_ribbon <- "#2c7fb8"

sc_plot_wu_cycle <- function(bundle, narrow = FALSE) {
  d <- bundle$cycle_ens
  if (is.null(d) || !nrow(d)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No warm-up series yet"))
  }
  rec <- bundle$recommend
  order_cut <- rec$order_cut
  p <- ggplot2::ggplot(d, ggplot2::aes(x = ord, y = cycle_smooth)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = cycle_p10_smooth, ymax = cycle_p90_smooth),
                         fill = sc_wu_ribbon, alpha = 0.22, colour = NA) +
    ggplot2::geom_line(colour = sc_wu_colors[["Smooth"]], linewidth = 1.15) +
    ggplot2::scale_x_continuous(
      breaks = if (isTRUE(narrow)) c(1, 40, 80, 120) else scales::breaks_pretty(n = 6),
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.14, 0.08) else c(0.03, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 5),
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.10, 0.16) else c(0.05, 0.10))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = if (isTRUE(narrow)) "Release order" else "Release order (first unit, then next, …)",
                  y = if (isTRUE(narrow)) "Cycle (h)" else "Cycle time (h)") +
    theme_sc()
  if (is.finite(order_cut) && order_cut > 0) {
    p <- p + ggplot2::geom_vline(xintercept = order_cut, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  if (isTRUE(narrow)) {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 16, 8, 4))
  }
  p
}

sc_plot_wu_wip <- function(bundle, narrow = FALSE) {
  d <- bundle$wip_ens
  if (is.null(d) || !nrow(d)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No warm-up series yet"))
  }
  rec <- bundle$recommend
  p <- ggplot2::ggplot(d, ggplot2::aes(x = week, y = wip_smooth)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = wip_p10_smooth, ymax = wip_p90_smooth),
                         fill = sc_wu_ribbon, alpha = 0.22, colour = NA) +
    ggplot2::geom_line(colour = sc_wu_colors[["Smooth"]], linewidth = 1.15) +
    ggplot2::geom_point(colour = sc_wu_colors[["Smooth"]], shape = 16, size = 2.2) +
    ggplot2::scale_x_continuous(
      breaks = if (isTRUE(narrow)) c(1, 20, 40, 52) else scales::breaks_pretty(n = 6),
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.10, 0.08) else c(0.03, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 5),
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.10, 0.16) else c(0.05, 0.10))
    ) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0, NA)) +
    ggplot2::labs(x = if (isTRUE(narrow)) "Week" else "Week of the year",
                  y = if (isTRUE(narrow)) "WIP" else "WIP (units)") +
    theme_sc()
  if (is.finite(rec$warmup_weeks) && rec$warmup_weeks > 0) {
    p <- p + ggplot2::geom_vline(xintercept = rec$warmup_weeks, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  if (isTRUE(narrow)) {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 16, 8, 4))
  }
  p
}

sc_wu_caption <- function(kind = "cycle") {
  if (identical(kind, "wip")) {
    "The solid line is a moving-window (Welch) smooth of mean WIP across replications; the ribbon is the same window on the 10th to 90th percentile. The dashed line is the suggested warm-up (weeks from the start). The line starts empty. Numbers are illustrative."
  } else {
    "The solid line is a moving-window (Welch) smooth of mean cycle time by release order across replications; the ribbon is the same window on the 10th to 90th percentile. The dashed line marks the last unit typically released during the suggested warm-up. Numbers are illustrative."
  }
}

sc_table_wu <- function(bundle, narrow = FALSE) {
  d <- bundle$cutoffs
  if (is.null(d) || !nrow(d)) {
    return(gt::gt(tibble::tibble(Note = "Run a warm-up check to fill this table.")) %>% sc_gt_style())
  }
  k0 <- bundle$kpis_all
  tab <- d %>%
    dplyr::arrange(.data$warmup_weeks) %>%
    dplyr::transmute(
      `Discard first` = ifelse(.data$warmup_weeks == 0, "None (default)",
                               sprintf("%.0f weeks", .data$warmup_weeks)),
      `Cycle time` = sprintf("%.1f h", .data$cycle_mean_h),
      `On time` = sprintf("%.1f%%", .data$on_time_pct),
      `vs default` = ifelse(
        .data$warmup_weeks == 0, "—",
        if (isTRUE(narrow)) {
          sprintf("%+.1f h<br>%+.1f pts",
                  .data$cycle_mean_h - k0$cycle_mean_h,
                  .data$on_time_pct - k0$on_time_pct)
        } else {
          sprintf("%+.1f h, %+.1f pts",
                  .data$cycle_mean_h - k0$cycle_mean_h,
                  .data$on_time_pct - k0$on_time_pct)
        }
      ),
      is_recommend = .data$is_recommend
    )
  if (isTRUE(narrow)) {
    names(tab)[names(tab) == "Discard first"] <- "Discard"
    names(tab)[names(tab) == "Cycle time"] <- "Cycle"
  }
  tips <- if (isTRUE(narrow)) {
    list(
      Discard = gt::html("<span title='How many weeks of early releases are dropped from the KPIs. None is the default, so other pages keep their current numbers.'>Discard</span>"),
      Cycle = gt::html("<span title='Mean cycle time of shipped units released after the warm-up.'>Cycle</span>"),
      `On time` = gt::html("<span title='Share of those shipped units that meet their due date.'>On time</span>"),
      `vs default` = gt::html("<span title='Change versus keeping every unit (the default).'>vs default</span>")
    )
  } else {
    list(
      `Discard first` = gt::html("<span title='How many weeks of early releases are dropped from the KPIs. None is the default, so other pages keep their current numbers.'>Discard first</span>"),
      `Cycle time` = gt::html("<span title='Mean cycle time of shipped units released after the warm-up.'>Cycle time</span>"),
      `On time` = gt::html("<span title='Share of those shipped units that meet their due date.'>On time</span>"),
      `vs default` = gt::html("<span title='Change versus keeping every unit (the default).'>vs default</span>")
    )
  }
  g <- tab %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_hide(is_recommend) %>%
    gt::cols_align("right", columns = -dplyr::any_of(c("Discard first", "Discard"))) %>%
    gt::cols_label(.list = tips) %>%
    gt::fmt_markdown(columns = `vs default`) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#c7e9c0"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = .data$is_recommend)
    ) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4))
  if (isTRUE(narrow)) {
    g <- gt::cols_width(g, Discard ~ gt::px(78), Cycle ~ gt::px(62),
                        `On time` ~ gt::px(62), `vs default` ~ gt::px(78))
    g <- g %>% gt::tab_options(table.font.size = gt::px(13),
                               data_row.padding = gt::px(4),
                               column_labels.padding = gt::px(3))
  } else {
    n_re <- bundle$meta$n_reps %||% NA
    g <- g %>% gt::tab_source_note(paste(
      sprintf("Each row uses the same %s replications and the same seeds.",
              if (is.finite(n_re)) as.integer(n_re) else "set of"),
      "The highlighted row is the suggested warm-up. 'None (default)' keeps every unit, which is how Simulate, Compare and the other pages report KPIs unless you raise the warm-up setting.",
      "Numbers are illustrative."
    ))
  }
  g
}

sc_wu_status_ui <- function(state) {
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

sc_warmup_ui <- function() {
  nav_panel(
    "Warmup",
    tags$style(HTML("
      .sc-wu-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .sc-wu-table-wrap .gt_table { width: 100%; }
      .sc-wu-table-wrap .gt_sourcenote,
      .sc-wu-table-wrap .gt_sourcenotes { white-space: normal; }
    ")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Warmup", width = 320, open = "desktop",
        p(class = "small",
          "The line starts empty, so the first units see short queues and the first weeks can bias cycle time and on-time %. This page charts that start-up, suggests a warm-up length, and shows how the KPIs change if those early units are discarded."),
        p(class = "small text-muted",
          title = "The default of 0 weeks is current behavior. Other pages do not change unless you raise this setting and run again.",
          "The Warm-up to discard setting on Simulate and Settings is 0 weeks by default, so the other pages keep their current numbers unless you opt in and raise it."),
        actionButton("wu_run", tagList(icon("play"), " Run check"),
                     class = "btn-primary w-100 mb-2"),
        actionButton("wu_restore", "Restore preloaded result",
                     class = "btn-outline-secondary w-100 mb-2"),
        actionButton("wu_cancel", "Cancel", class = "btn-outline-secondary w-100 mb-3"),
        numericInput("wu_reps", "Replications", 20, min = 3, max = 40, step = 1),
        numericInput("wu_seed", "Seed", 42, min = 1, step = 1),
        helpText("The preloaded view used 20 replications, seed 42, on the default line, with the same seeds as Compare so the 'keep every unit' cycle time is 179.2 h.")
      ),
      uiOutput("wu_status"),
      card(card_header("What this says"), uiOutput("wu_story")),
      card(
        card_header("Cycle time by release order"),
        plotOutput("wu_plot_cycle", height = 400),
        p(class = "small text-muted mb-0", sc_wu_caption("cycle"))
      ),
      card(
        card_header("WIP by week"),
        plotOutput("wu_plot_wip", height = 400),
        p(class = "small text-muted mb-0", sc_wu_caption("wip"))
      ),
      card(
        card_header("KPIs if you discard the first weeks"),
        div(class = "sc-wu-table-wrap", gt::gt_output("wu_table"))
      )
    )
  )
}

sc_warmup_server <- function(input, output, session, current_params) {
  preload <- sc_load_preload_warmup()
  result <- reactiveVal(preload)
  running <- reactiveVal(FALSE)
  cancel_flag <- reactiveVal(FALSE)
  job <- reactiveVal(NULL)
  preload_msg <- function() {
    if (is.null(preload)) {
      "No preloaded warm-up result found. Click Run check to generate one from the current Settings."
    } else {
      sprintf(
        "Showing the preloaded warm-up check (%d replications, seed %s, the default line). Change the settings and run a live check, or restore this result. The warm-up setting stays at 0 unless you opt in.",
        preload$meta$n_reps, preload$meta$base_seed
      )
    }
  }
  status <- reactiveVal(list(
    kind = if (is.null(preload)) "idle" else "done",
    text = preload_msg(),
    pct = NULL
  ))

  matches_preload <- function(params, reps, seed) {
    !is.null(preload) &&
      identical(params, preload$params) &&
      reps == preload$meta$n_reps &&
      seed == preload$meta$base_seed
  }

  observeEvent(input$wu_restore, {
    if (is.null(preload)) {
      result(NULL)
      status(list(kind = "idle", text = preload_msg(), pct = NULL))
    } else {
      result(preload)
      status(list(kind = "done", pct = NULL, text = preload_msg()))
    }
  })

  observeEvent(input$wu_run, {
    if (isTRUE(running())) return()
    p <- current_params()
    # The check always looks from t = 0; the discard is applied after the run.
    p$warmup_h <- NULL
    reps <- as.integer(input$wu_reps %||% 20)
    seed <- as.integer(input$wu_seed %||% 42)
    if (matches_preload(p, reps, seed)) {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Those settings match the preloaded result, so the saved check was reused (%d replications, seed %s).",
                                 reps, seed)))
      return()
    }
    cancel_flag(FALSE)
    running(TRUE)
    st <- sc_wu_new_state(p, reps, seed)
    job(st)
    status(list(kind = "running", pct = 0,
                text = sprintf("Starting replication 1 of %d…", reps)))
  })

  observeEvent(input$wu_cancel, {
    if (isTRUE(running())) {
      cancel_flag(TRUE)
      status(list(kind = "cancelled", pct = NULL,
                  text = "Cancelling after the current replication…"))
    }
  })

  observe({
    req(running())
    invalidateLater(20)
    isolate({
      st <- job()
      if (is.null(st)) { running(FALSE); return() }
      if (isTRUE(cancel_flag())) {
        st$cancelled <- TRUE
        bundle <- sc_wu_bundle_from_state(st)
        result(bundle)
        job(NULL); running(FALSE)
        n_done <- length(Filter(Negate(is.null), st$runs))
        status(list(kind = "cancelled", pct = NULL,
                    text = sprintf("Cancelled after %d of %d replications.", n_done, st$n_reps)))
        return()
      }
      if (identical(st$phase, "run")) {
        this <- st$i
        st <- sc_wu_step(st)
        job(st)
        if (identical(st$phase, "run")) {
          status(list(kind = "running", pct = 100 * (this / st$n_reps),
                      text = sprintf("Finished replication %d of %d. Starting %d of %d…",
                                     this, st$n_reps, st$i, st$n_reps)))
        } else {
          bundle <- sc_wu_bundle_from_state(st)
          result(bundle)
          job(NULL); running(FALSE)
          status(list(kind = "done", pct = 100,
                      text = sprintf("Done. %d replications, seed %s, in %.1f s.",
                                     bundle$meta$n_reps, bundle$meta$base_seed, bundle$meta$secs)))
        }
      } else {
        running(FALSE)
      }
    })
  })

  output$wu_status <- renderUI(sc_wu_status_ui(status()))

  output$wu_story <- renderUI({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$meta),
                  "Run a check, or restore the preloaded result, to fill this summary."))
    p(class = "mb-0", sc_wu_story(r))
  })

  output$wu_plot_cycle <- renderPlot({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$cycle_ens) && nrow(r$cycle_ens),
                  "Run a check to draw the chart."))
    w <- session$clientData$output_wu_plot_cycle_width
    sc_plot_wu_cycle(r, narrow = isTRUE(w < 700))
  }, res = 96)

  output$wu_plot_wip <- renderPlot({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$wip_ens) && nrow(r$wip_ens),
                  "Run a check to draw the chart."))
    w <- session$clientData$output_wu_plot_wip_width
    sc_plot_wu_wip(r, narrow = isTRUE(w < 700))
  }, res = 96)

  output$wu_table <- gt::render_gt({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$cutoffs) && nrow(r$cutoffs),
                  "Run a check to fill this table."))
    w <- session$clientData$output_wu_plot_cycle_width
    sc_table_wu(r, narrow = isTRUE(w < 700))
  })
}
