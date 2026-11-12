# spacecraft_tornado_page.R
# Tornado page: one-at-a-time low/high swings of key inputs, a tornado chart
# of a chosen output, a plain-English summary of the top drivers, a preloaded
# result so the first look is instant, and a live run with progress and Cancel.

# Low = filled dark-blue circle; High = open orange triangle (readable in
# greyscale). The connecting bar is a neutral grey. The dashed line is the baseline.
sc_tornado_colors <- c(Low = "#1b4f72", High = "#d95f02")
sc_tornado_shapes <- c(Low = 16, High = 2)


# Exactly two well-spaced ticks at a narrow width; pretty breaks otherwise.
sc_tornado_x_breaks <- function(d, narrow = FALSE) {
  rng <- range(c(d$low, d$high, d$base), na.rm = TRUE)
  if (!isTRUE(narrow)) return(scales::breaks_pretty(n = 4)(rng))
  span <- max(diff(rng), .Machine$double.eps)
  # Prefer a coarser unit so the two labels sit near the ends and stay apart.
  mag <- 10^floor(log10(span))
  unit <- mag
  for (u in c(5 * mag, 2 * mag, mag, 10 * mag)) {
    lo <- floor(rng[1] / u) * u
    hi <- ceiling(rng[2] / u) * u
    if (hi > lo) {
      unit <- u
      break
    }
  }
  lo <- floor(rng[1] / unit) * unit
  hi <- ceiling(rng[2] / unit) * unit
  if (hi <= lo) hi <- lo + unit
  c(lo, hi)
}

sc_plot_tornado <- function(bundle, metric_id = "cycle", narrow = FALSE) {
  metrics <- sc_tornado_metrics()
  met <- metrics[metrics$id == metric_id, ]
  stopifnot(nrow(met) == 1L)
  name_col <- if (isTRUE(narrow)) "short" else "label"
  d <- sc_tornado_effects_now(bundle) %>%
    dplyr::filter(.data$metric == metric_id) %>%
    dplyr::arrange(.data$swing, .data$label) %>%
    dplyr::mutate(
      y_lab = .data[[name_col]],
      y = factor(.data$y_lab, levels = .data$y_lab)
    )
  xlab <- if (isTRUE(narrow)) {
    switch(metric_id,
           cycle = "Cycle time (h)",
           ontime = "On time (%)",
           throughput = "Throughput",
           lateness = "Lateness (h)",
           met$label[[1]])
  } else {
    met$label[[1]]
  }
  x_br <- sc_tornado_x_breaks(d, narrow)
  p <- ggplot2::ggplot(d, ggplot2::aes(y = y)) +
    ggplot2::geom_vline(xintercept = d$base[[1]], linetype = "dashed",
                        linewidth = 1, colour = "#1a1a1a") +
    ggplot2::geom_segment(ggplot2::aes(x = low, xend = high, yend = y),
                          linewidth = 2.0, colour = "#737373") +
    ggplot2::geom_point(ggplot2::aes(x = low, colour = "Low", shape = "Low"),
                        size = 3.6) +
    ggplot2::geom_point(ggplot2::aes(x = high, colour = "High", shape = "High"),
                        size = 3.8, stroke = 1.25) +
    ggplot2::scale_colour_manual(values = sc_tornado_colors, breaks = c("Low", "High"),
                                 name = NULL) +
    ggplot2::scale_shape_manual(values = sc_tornado_shapes, breaks = c("Low", "High"),
                                name = NULL) +
    ggplot2::scale_x_continuous(
      breaks = x_br,
      limits = if (isTRUE(narrow)) range(c(x_br, d$low, d$high, d$base), na.rm = TRUE) else NULL,
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.12, 0.16) else c(0.04, 0.10))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = xlab, y = NULL, colour = NULL, shape = NULL) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 1, title = NULL),
      shape = ggplot2::guide_legend(nrow = 1, title = NULL)
    )
  if (isTRUE(narrow)) {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 20, 8, 4))
  }
  p
}

sc_table_tornado <- function(bundle, metric_id = "cycle", narrow = FALSE) {
  metrics <- sc_tornado_metrics()
  met <- metrics[metrics$id == metric_id, ]
  name_col <- if (isTRUE(narrow)) "short" else "label"
  factor_nm <- if (isTRUE(narrow)) "Factor" else "Factor"
  d <- sc_tornado_effects_now(bundle) %>%
    dplyr::filter(.data$metric == metric_id) %>%
    dplyr::left_join(
      bundle$levels %>% dplyr::select(id, low_label, high_label),
      by = "id"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$swing), .data$label) %>%
    dplyr::transmute(
      Factor = .data[[name_col]],
      tip = .data$tip,
      Low = paste0(sc_tornado_format_value(.data$low, metric_id),
                   " (", .data$low_label, ")"),
      Baseline = sc_tornado_format_value(.data$base, metric_id),
      High = paste0(sc_tornado_format_value(.data$high, metric_id),
                    " (", .data$high_label, ")"),
      Swing = sc_tornado_format_value(.data$swing, metric_id)
    )
  tips <- list(
    Factor = "The input that was moved low and high, one at a time.",
    Low = "Result when that input is at its low setting (usually 20% below the baseline, or one fewer station). The setting is in parentheses.",
    Baseline = "Result on the unperturbed line.",
    High = "Result when that input is at its high setting (usually 20% above the baseline, or one more station). The setting is in parentheses.",
    Swing = "Absolute gap between the low and high results. The chart is sorted by this."
  )
  labels <- lapply(names(tips), function(nm) {
    gt::html(sprintf('<span title="%s">%s</span>',
                     htmltools::htmlEscape(tips[[nm]]),
                     htmltools::htmlEscape(nm)))
  })
  names(labels) <- names(tips)
  g <- d %>%
    dplyr::select(-tip) %>%
    gt::gt() %>%
    sc_gt_style()
  g <- if (isTRUE(narrow)) {
    gt::cols_width(g, Factor ~ gt::px(130))
  } else {
    gt::cols_width(g, Factor ~ gt::px(260))
  }
  g <- g %>%
    gt::cols_align("right", columns = -Factor) %>%
    gt::cols_label(.list = labels) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4))
  if (!isTRUE(narrow)) {
    g <- g %>% gt::tab_source_note(paste(
      "Each row moves one input to a low or high setting; everything else stays at the baseline.",
      "Times and rates move by 20%. Station counts move by one, and never below one.",
      "The dashed line on the chart is the baseline. Common random numbers. Hover a column name for a short description."
    ))
  }
  g
}

sc_tornado_status_ui <- function(state) {
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

sc_tornado_ui <- function() {
  metrics <- sc_tornado_metrics()
  choices <- stats::setNames(metrics$id, metrics$label)
  nav_panel(
    "Tornado",
    tags$style(HTML("
      .sc-tornado-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .sc-tornado-table-wrap .gt_table { width: 100%; }
      .sc-tornado-table-wrap .gt_sourcenote,
      .sc-tornado-table-wrap .gt_sourcenotes { white-space: normal; }
      @media (max-width: 800px) {
        .sc-tornado-table-wrap .gt_table { min-width: 560px; }
      }
    ")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Tornado", width = 320, open = "desktop",
        p(class = "small",
          "This page moves each key input one at a time to a low and a high setting, then shows how far a chosen output swings. The dashed line is the baseline. The same seeds are used throughout, so a change comes from the input, not from a luckier arrival stream."),
        selectInput("tornado_metric", "Output", choices = choices, selected = "cycle"),
        actionButton("tornado_run", tagList(icon("play"), " Run tornado"),
                     class = "btn-primary w-100 mb-2"),
        actionButton("tornado_restore", "Restore preloaded tornado",
                     class = "btn-outline-secondary w-100 mb-2"),
        actionButton("tornado_cancel", "Cancel", class = "btn-outline-secondary w-100 mb-3"),
        numericInput("tornado_reps", "Replications", 20, min = 3, max = 40, step = 1),
        numericInput("tornado_seed", "Seed", 42, min = 1, step = 1),
        helpText("The preloaded view is the default line, 20 replications, seed 42 (the same baseline as Compare and Analyze). Times and rates move by 20%; station counts move by one. Live runs can be cancelled between replications.")
      ),
      uiOutput("tornado_status"),
      card(card_header("What this says"), uiOutput("tornado_story")),
      card(
        card_header("Sensitivity (tornado)"),
        plotOutput("tornado_plot", height = 700),
        div(class = "sc-tornado-table-wrap", gt::gt_output("tornado_table")),
        p(class = "small text-muted mb-0",
          "Each bar runs from the low setting of that input to the high setting.",
          "A filled dark-blue circle is the low setting; an open orange triangle is the high setting. The grey bar runs between them. The dashed line is the baseline.",
          "Bars are sorted by the size of the swing. Inputs are illustrative.")
      )
    )
  )
}

# Wire the page into sc_server(). A live run walks one replication at a time
# so Cancel can fire between them (the same cooperative pattern as Compare).
sc_tornado_server <- function(input, output, session, current_params) {
  preload <- sc_load_preload_tornado()
  result <- reactiveVal(preload)
  running <- reactiveVal(FALSE)
  cancel_flag <- reactiveVal(FALSE)
  job <- reactiveVal(NULL)
  status <- reactiveVal(list(
    kind = if (is.null(preload)) "idle" else "done",
    text = if (is.null(preload)) {
      "No preloaded tornado found. Click Run tornado to generate one from the current Settings."
    } else {
      sprintf("Showing the preloaded tornado (%d replications, seed %s). Change the output to redraw the chart, or run a live tornado from the current Settings.",
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

  observeEvent(input$tornado_restore, {
    if (is.null(preload)) {
      result(NULL)
      status(list(kind = "idle", text = "No preloaded tornado found. Click Run tornado to generate one from the current Settings.", pct = NULL))
    } else {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Showing the preloaded tornado (%d replications, seed %s).",
                                 preload$meta$n_reps, preload$meta$base_seed)))
    }
  })

  observeEvent(input$tornado_run, {
    if (isTRUE(running())) return()
    p <- current_params()
    n_reps <- as.integer(input$tornado_reps %||% 20)
    seed <- as.integer(input$tornado_seed %||% 42)
    if (matches_preload(p, n_reps, seed)) {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Those settings match the preloaded tornado, so the saved results were reused (%d replications, seed %s).",
                                 n_reps, seed)))
      return()
    }
    jobs <- sc_tornado_jobs(p)
    cancel_flag(FALSE)
    running(TRUE)
    job(list(params = p, n_reps = n_reps, base_seed = seed, jobs = jobs,
             i_job = 1L, i_rep = 1L, acc = vector("list", nrow(jobs)),
             plan_cache = list(), t0 = Sys.time(), done = 0L,
             total = nrow(jobs) * n_reps))
    status(list(kind = "running", pct = 0,
                text = sprintf("Running tornado, %d replications around the current Settings…", n_reps)))
  })

  observeEvent(input$tornado_cancel, {
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
        status(list(kind = "cancelled", pct = 100 * j$done / max(j$total, 1),
                    text = sprintf("Cancelled after %d of %d replications.", j$done, j$total)))
        return()
      }
      jobs <- j$jobs
      s <- j$i_job
      r <- j$i_rep
      seed <- as.integer(j$base_seed + r - 1L)
      p <- jobs$params[[s]]
      sig <- list(p$sim_hours, p$arrival, p$variants[, c("variant", "mix", "due")], p$kit)
      key <- paste(I(rlang::hash(sig)), seed, sep = ".")
      if (!key %in% names(j$plan_cache)) j$plan_cache[[key]] <- sc_arrival_plan(p, seed)
      out <- sc_run_one(p, seed, plan = j$plan_cache[[key]])
      out$rep <- r
      if (is.null(j$acc[[s]])) j$acc[[s]] <- vector("list", j$n_reps)
      j$acc[[s]][[r]] <- out
      j$done <- j$done + 1L
      label <- paste(jobs$label[[s]], jobs$level[[s]])
      pct <- 100 * j$done / j$total
      status(list(kind = "running", pct = pct,
                  text = sprintf("Running tornado: %s, replication %d of %d (%d / %d)…",
                                 label, r, j$n_reps, j$done, j$total)))
      if (r < j$n_reps) {
        j$i_rep <- r + 1L
        job(j)
      } else if (s < nrow(jobs)) {
        j$i_job <- s + 1L
        j$i_rep <- 1L
        job(j)
      } else {
        points <- purrr::imap(j$acc, function(runs, i) {
          sc_summarise(runs, jobs$params[[i]])$per_rep %>%
            dplyr::mutate(id = jobs$id[[i]], level = jobs$level[[i]], .before = 1)
        })
        bundle <- sc_tornado_bundle(j$params, sc_tornado_factors(), dplyr::bind_rows(points))
        bundle$meta <- list(
          n_reps = j$n_reps, base_seed = j$base_seed,
          secs = as.numeric(difftime(Sys.time(), j$t0, units = "secs")),
          cancelled = FALSE, source = "live"
        )
        result(bundle)
        job(NULL); running(FALSE)
        status(list(kind = "done", pct = 100,
                    text = sprintf("Done. %d replications around the current Settings in %.1f s, seed %s.",
                                   j$n_reps, bundle$meta$secs, j$base_seed)))
      }
    })
  })

  output$tornado_status <- renderUI(sc_tornado_status_ui(status()))

  output$tornado_story <- renderUI({
    r <- result()
    mid <- input$tornado_metric %||% "cycle"
    validate(need(!is.null(r) && !is.null(r$effects) && nrow(r$effects),
                  "Run a tornado, or restore the preloaded one, to fill this summary."))
    p(class = "mb-0", sc_tornado_story(r, mid))
  })

  output$tornado_plot <- renderPlot({
    r <- result()
    mid <- input$tornado_metric %||% "cycle"
    validate(need(!is.null(r) && !is.null(r$effects) && nrow(r$effects),
                  "Run a tornado to draw the chart."))
    w <- session$clientData$output_tornado_plot_width
    sc_plot_tornado(r, mid, narrow = isTRUE(w < 700))
  }, res = 96)

  output$tornado_table <- gt::render_gt({
    r <- result()
    mid <- input$tornado_metric %||% "cycle"
    validate(need(!is.null(r) && !is.null(r$effects) && nrow(r$effects),
                  "Run a tornado to fill this table."))
    w <- session$clientData$output_tornado_plot_width
    sc_table_tornado(r, mid, narrow = isTRUE(w < 700))
  })
}
