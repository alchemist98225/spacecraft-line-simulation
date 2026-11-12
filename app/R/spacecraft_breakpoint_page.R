# spacecraft_breakpoint_page.R
# Breakpoint page: raise demand from the current Settings, plot on-time % and
# cycle time against offered demand (P10–P90 ribbons), mark where mean on-time
# crosses the target (interpolated), highlight the first failing demand in the
# table, and say which station saturates first. Instant from a preloaded sweep;
# live runs have progress and Cancel.

sc_break_colors <- c(ontime = "#2c7fb8", cycle = "#2c7fb8")

# Utilization series: distinct hues and lightness (blue mid, gold light,
# near-black dark) plus distinct point shapes, so the chart stays readable
# in greyscale. Review-board purple and acceptance orange are fallbacks.
sc_break_util_pal <- c(chamber = "#2c7fb8", integration = "#e0a032",
                       func_test = "#1a1a1a", review_board = "#756bb1",
                       acceptance = "#e6550d")
sc_break_util_shapes <- c(chamber = 16, integration = 17, func_test = 15,
                          review_board = 18, acceptance = 8)

# Two well-spaced x ticks at phone width; pretty breaks otherwise.
sc_break_x_breaks <- function(x, narrow = FALSE) {
  rng <- range(x, na.rm = TRUE)
  if (!isTRUE(narrow)) return(scales::breaks_pretty(n = 5)(rng))
  # Two ticks near the ends of the data, without padding the axis out to a
  # much wider range (that squishes the points in the middle).
  span <- max(diff(rng), .Machine$double.eps)
  mag <- 10^floor(log10(span / 2))
  pick <- function(u) {
    lo <- floor(rng[1] / u) * u
    hi <- ceiling(rng[2] / u) * u
    if (hi <= lo) hi <- lo + u
    c(lo, hi)
  }
  chosen <- NULL
  for (u in c(mag / 2, mag, 2 * mag, 5 * mag, 10 * mag)) {
    if (!is.finite(u) || u <= 0) next
    br <- pick(u)
    pad <- (diff(range(br)) - span) / span
    if (is.finite(pad) && pad >= 0 && pad <= 0.3) {
      chosen <- br
      break
    }
  }
  if (is.null(chosen)) {
    u <- max(mag, 1)
    chosen <- pick(u)
  }
  chosen
}

sc_break_x_scale <- function(x, narrow = FALSE) {
  br <- sc_break_x_breaks(x, narrow)
  # Limits follow the data (plus a modest expansion), not a far-away tick.
  ggplot2::scale_x_continuous(
    breaks = br,
    expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.10, 0.14) else c(0.02, 0.06))
  )
}

sc_plot_break_ontime <- function(bundle, target = 80, narrow = FALSE) {
  br <- sc_break_detect(bundle, target)
  d <- bundle$points
  xlab <- if (isTRUE(narrow)) "Demand (units/year)" else "Offered demand (units/year)"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = demand, y = ontime_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ontime_p10, ymax = ontime_p90),
                         fill = sc_break_colors[["ontime"]], alpha = 0.22, colour = NA) +
    ggplot2::geom_line(colour = sc_break_colors[["ontime"]], linewidth = 1.1) +
    ggplot2::geom_point(colour = sc_break_colors[["ontime"]], size = 2.8) +
    ggplot2::geom_hline(yintercept = target, linetype = "dashed",
                        linewidth = 1, colour = "#1a1a1a") +
    sc_break_x_scale(d$demand, narrow) +
    ggplot2::scale_y_continuous(
      breaks = if (isTRUE(narrow)) c(0, 50, 100) else scales::breaks_pretty(n = 5),
      limits = c(0, 100),
      expand = ggplot2::expansion(mult = c(0.02, 0.08))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = xlab, y = if (isTRUE(narrow)) "On time (%)" else "On time (%)") +
    theme_sc() +
    ggplot2::theme(legend.position = "none")
  if (isTRUE(br$found)) {
    p <- p + ggplot2::geom_vline(xintercept = br$demand, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  if (isTRUE(narrow)) {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 8, 4))
  }
  p
}

sc_plot_break_cycle <- function(bundle, target = 80, narrow = FALSE) {
  br <- sc_break_detect(bundle, target)
  d <- bundle$points
  xlab <- if (isTRUE(narrow)) "Demand (units/year)" else "Offered demand (units/year)"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = demand, y = cycle_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = cycle_p10, ymax = cycle_p90),
                         fill = sc_break_colors[["cycle"]], alpha = 0.22, colour = NA) +
    ggplot2::geom_line(colour = sc_break_colors[["cycle"]], linewidth = 1.1) +
    ggplot2::geom_point(colour = sc_break_colors[["cycle"]], size = 2.8) +
    sc_break_x_scale(d$demand, narrow) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 4),
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.02, 0.14) else c(0.02, 0.08))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = xlab, y = if (isTRUE(narrow)) "Cycle time (h)" else "Mean cycle time (h)") +
    theme_sc() +
    ggplot2::theme(legend.position = "none")
  if (isTRUE(br$found)) {
    p <- p + ggplot2::geom_vline(xintercept = br$demand, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  if (isTRUE(narrow)) {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 8, 4))
  }
  p
}

# Utilization of the two or three busiest stations across the sweep, with a
# dashed 100% busy line. The on-time breakpoint is marked when it is found.
sc_plot_break_util <- function(bundle, target = 80, narrow = FALSE, n_stations = NULL) {
  br <- sc_break_detect(bundle, target)
  if (is.null(n_stations)) n_stations <- if (isTRUE(narrow)) 2L else 3L
  # Pick stations by their peak mean utilization on the sweep.
  top <- bundle$util %>%
    dplyr::group_by(.data$resource, .data$label, .data$short) %>%
    dplyr::summarise(peak = max(.data$util_mean, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$peak)) %>%
    dplyr::slice_head(n = n_stations)
  # At phone width, "Integration" in a one-row legend is clipped; use shorter names.
  narrow_short <- c(chamber = "Chamber", integration = "Integ.",
                    func_test = "Func.", review_board = "MRB",
                    acceptance = "Accept.")
  d <- bundle$util %>%
    dplyr::semi_join(top, by = "resource") %>%
    dplyr::mutate(
      series = if (isTRUE(narrow)) unname(narrow_short[.data$resource]) else .data$label
    )
  # Stable legend order: busiest first.
  order <- if (isTRUE(narrow)) unname(narrow_short[top$resource]) else top$label
  d$series <- factor(d$series, levels = order)
  pal <- stats::setNames(unname(sc_break_util_pal[top$resource]), order)
  pal[is.na(pal)] <- "#6c757d"
  shp <- stats::setNames(unname(sc_break_util_shapes[top$resource]), order)
  shp[is.na(shp)] <- 16
  xlab <- if (isTRUE(narrow)) "Demand (units/year)" else "Offered demand (units/year)"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = demand, y = util_mean,
                                       colour = series, fill = series, shape = series)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = util_p10, ymax = util_p90),
                         alpha = 0.18, colour = NA) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 3.2) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        linewidth = 1, colour = "#1a1a1a") +
    ggplot2::scale_colour_manual(values = pal, breaks = order, name = NULL) +
    ggplot2::scale_fill_manual(values = pal, breaks = order, name = NULL) +
    ggplot2::scale_shape_manual(values = shp, breaks = order, name = NULL) +
    sc_break_x_scale(d$demand, narrow) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = if (isTRUE(narrow)) c(0, 0.5, 1) else scales::breaks_pretty(n = 4),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0.02, 0.08))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = xlab, y = "Utilization") +
    theme_sc()
  if (isTRUE(br$found)) {
    p <- p + ggplot2::geom_vline(xintercept = br$demand, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  # Colour, fill and shape share one one-row legend.
  one_row <- ggplot2::guides(
    colour = ggplot2::guide_legend(nrow = 1, title = NULL),
    fill = ggplot2::guide_legend(nrow = 1, title = NULL),
    shape = ggplot2::guide_legend(nrow = 1, title = NULL)
  )
  if (isTRUE(narrow)) {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 8, 4)) + one_row
  } else {
    p <- p + one_row
  }
  p
}

sc_table_break <- function(bundle, target = 80, narrow = FALSE) {
  br <- sc_break_detect(bundle, target)
  bot <- bundle$util %>%
    dplyr::group_by(.data$demand) %>%
    dplyr::arrange(dplyr::desc(.data$util_mean), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      demand,
      bot_name = if (isTRUE(narrow)) .data$short else .data$label,
      bot_util = .data$util_mean,
      bot_q = .data$queue_mean
    )
  d <- bundle$points %>%
    dplyr::left_join(bot, by = "demand") %>%
    dplyr::arrange(.data$demand) %>%
    dplyr::transmute(
      Demand = sprintf("%.0f", .data$demand),
      `On time` = sprintf("%.1f%%", .data$ontime_mean),
      `Cycle time` = sprintf("%.1f h", .data$cycle_mean),
      Station = .data$bot_name,
      Utilization = sprintf("%.0f%%", 100 * .data$bot_util),
      Queue = sprintf("%.1f", .data$bot_q),
      is_break = isTRUE(br$found) & is.finite(br$fail_demand) &
        abs(.data$demand - br$fail_demand) < 1e-9
    )
  if (isTRUE(narrow)) {
    # Three columns fit at phone width without clipping a header; station
    # and queue are on the utilization chart and in the summary.
    d <- d %>% dplyr::select(Demand, `On time`, Cycle = `Cycle time`, is_break)
  }
  g <- d %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_hide(is_break) %>%
    gt::cols_align("right", columns = -dplyr::any_of(c("Demand", "Station")))
  # Numeric widths only: gt evaluates formulas in the data, not the caller.
  # Narrow has Demand / On time / Cycle only — do not name Station here.
  g <- if (isTRUE(narrow)) {
    gt::cols_width(g, Demand ~ gt::px(80), `On time` ~ gt::px(90), Cycle ~ gt::px(90))
  } else {
    gt::cols_width(g, Station ~ gt::px(240), Demand ~ gt::px(90))
  }
  g <- g %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#fce4c2"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = .data$is_break)
    ) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4))
  if (!isTRUE(narrow)) {
    g <- g %>% gt::tab_source_note(paste(
      "Each row is the mean across replications at that offered demand (releases per 52-week year).",
      "Station is the busiest one at that demand; Utilization is the time-average share of servers that are busy; Queue is the time-average number of units waiting in front of it.",
      "The highlighted row is the first demand where mean on-time falls below the target.",
      "The ribbon on the charts is the 10th to 90th percentile across replications. Common random numbers."
    ))
  }
  g
}

sc_break_status_ui <- function(state) {
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

sc_breakpoint_ui <- function() {
  nav_panel(
    "Breakpoint",
    tags$style(HTML("
      .sc-break-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .sc-break-table-wrap .gt_table { width: 100%; }
      .sc-break-table-wrap .gt_sourcenote,
      .sc-break-table-wrap .gt_sourcenotes { white-space: normal; }
      @media (max-width: 800px) {
        .sc-break-table-wrap .gt_table { min-width: 0; }
      }
    ")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Breakpoint", width = 320, open = "desktop",
        p(class = "small",
          "This page raises the release rate from the current Settings and finds where mean on-time crosses a target, interpolating between the last demand that still meets it and the first that falls short. The same seeds are used at every demand, so a change comes from the rate, not from a luckier arrival stream."),
        numericInput("break_target", "On-time target (%)", 80, min = 50, max = 99, step = 1),
        actionButton("break_run", tagList(icon("play"), " Run sweep"),
                     class = "btn-primary w-100 mb-2"),
        actionButton("break_restore", "Restore preloaded sweep",
                     class = "btn-outline-secondary w-100 mb-2"),
        actionButton("break_cancel", "Cancel", class = "btn-outline-secondary w-100 mb-3"),
        numericInput("break_reps", "Replications", 20, min = 3, max = 40, step = 1),
        numericInput("break_seed", "Seed", 42, min = 1, step = 1),
        helpText("The preloaded view is the default line, 20 replications, seed 42 (the same baseline as Compare, Analyze and Tornado: 179.2 h mean cycle time at 121 units/year). The default 80% target sits a little below that baseline's 86% on-time, so you can see how far demand can rise. Changing the target redraws the marks on the saved sweep. Live runs can be cancelled between replications.")
      ),
      uiOutput("break_status"),
      card(card_header("What this says"), uiOutput("break_story")),
      card(
        card_header("On time vs demand"),
        plotOutput("break_ontime_plot", height = 360),
        p(class = "small text-muted mb-0",
          "The line is the mean on-time share across replications; the ribbon runs from the 10th to the 90th percentile.",
          "The dashed horizontal line is the on-time target. The dashed vertical line is where mean on-time crosses the target.")
      ),
      card(
        card_header("Cycle time vs demand"),
        plotOutput("break_cycle_plot", height = 360),
        p(class = "small text-muted mb-0",
          "Mean cycle time (release to ship) with the 10th to 90th percentile ribbon.",
          "The dashed vertical line is where mean on-time crosses the target.")
      ),
      card(
        card_header("Which station saturates first"),
        plotOutput("break_util_plot", height = 400),
        p(class = "small text-muted mb-0",
          "Utilization of the busiest stations as demand rises. The dashed horizontal line is 100% busy.",
          "The dashed vertical line is where mean on-time crosses the target. Queue lengths are in the table.")
      ),
      card(
        card_header("Sweep results"),
        div(class = "sc-break-table-wrap", gt::gt_output("break_table"))
      )
    )
  )
}

sc_breakpoint_server <- function(input, output, session, current_params) {
  preload <- sc_load_preload_breakpoint()
  result <- reactiveVal(preload)
  running <- reactiveVal(FALSE)
  cancel_flag <- reactiveVal(FALSE)
  job <- reactiveVal(NULL)
  status <- reactiveVal(list(
    kind = if (is.null(preload)) "idle" else "done",
    text = if (is.null(preload)) {
      "No preloaded sweep found. Click Run sweep to generate one from the current Settings."
    } else {
      sprintf("Showing the preloaded demand sweep (%d replications, seed %s). Change the on-time target to redraw the marks, or run a live sweep from the current Settings.",
              preload$meta$n_reps, preload$meta$base_seed)
    },
    pct = NULL
  ))

  matches_preload <- function(params, n_reps, seed, demand) {
    !is.null(preload) &&
      n_reps == preload$meta$n_reps &&
      seed == preload$meta$base_seed &&
      identical(params, preload$params) &&
      isTRUE(all.equal(sort(demand), sort(preload$demand)))
  }

  observeEvent(input$break_restore, {
    if (is.null(preload)) {
      result(NULL)
      status(list(kind = "idle", text = "No preloaded sweep found. Click Run sweep to generate one from the current Settings.", pct = NULL))
    } else {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Showing the preloaded demand sweep (%d replications, seed %s).",
                                 preload$meta$n_reps, preload$meta$base_seed)))
    }
  })

  observeEvent(input$break_run, {
    if (isTRUE(running())) return()
    p <- current_params()
    n_reps <- as.integer(input$break_reps %||% 20)
    seed <- as.integer(input$break_seed %||% 42)
    demand <- if (isTRUE(all.equal(sc_break_demand(p), sc_break_demand(sc_default_params())))) {
      sc_break_default_grid(p)
    } else {
      sc_break_grid(p)
    }
    if (matches_preload(p, n_reps, seed, demand)) {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Those settings match the preloaded sweep, so the saved results were reused (%d replications, seed %s).",
                                 n_reps, seed)))
      return()
    }
    jobs <- sc_break_jobs(p, demand)
    cancel_flag(FALSE)
    running(TRUE)
    job(list(params = p, n_reps = n_reps, base_seed = seed, jobs = jobs,
             i_job = 1L, i_rep = 1L, acc = vector("list", nrow(jobs)),
             util_acc = vector("list", nrow(jobs)),
             t0 = Sys.time(), done = 0L,
             total = nrow(jobs) * n_reps))
    status(list(kind = "running", pct = 0,
                text = sprintf("Sweeping demand, %d replications around the current Settings…", n_reps)))
  })

  observeEvent(input$break_cancel, {
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
      out <- sc_run_one(jobs$params[[s]], seed)
      out$rep <- r
      if (is.null(j$acc[[s]])) j$acc[[s]] <- vector("list", j$n_reps)
      j$acc[[s]][[r]] <- out
      j$done <- j$done + 1L
      label <- sprintf("%.0f units/year", jobs$demand[[s]])
      pct <- 100 * j$done / j$total
      status(list(kind = "running", pct = pct,
                  text = sprintf("Sweeping demand: %s, replication %d of %d (%d / %d)…",
                                 label, r, j$n_reps, j$done, j$total)))
      if (r < j$n_reps) {
        j$i_rep <- r + 1L
        job(j)
      } else if (s < nrow(jobs)) {
        j$i_job <- s + 1L
        j$i_rep <- 1L
        job(j)
      } else {
        per_rep <- purrr::imap_dfr(j$acc, function(runs, i) {
          sc_summarise(runs, jobs$params[[i]])$per_rep %>%
            dplyr::mutate(demand = jobs$demand[[i]], is_baseline = jobs$is_baseline[[i]],
                          .before = 1)
        })
        util_rep <- purrr::imap_dfr(j$acc, function(runs, i) {
          purrr::map_dfr(runs, function(run) {
            sc_resource_use(run) %>% dplyr::mutate(rep = run$rep, demand = jobs$demand[[i]])
          })
        })
        bundle <- sc_break_bundle(j$params, jobs$demand, per_rep, util_rep)
        bundle$meta <- list(
          n_reps = j$n_reps, base_seed = j$base_seed,
          secs = as.numeric(difftime(Sys.time(), j$t0, units = "secs")),
          cancelled = FALSE, source = "live"
        )
        result(bundle)
        job(NULL); running(FALSE)
        status(list(kind = "done", pct = 100,
                    text = sprintf("Done. %d replications at %d demand points in %.1f s, seed %s.",
                                   j$n_reps, nrow(jobs), bundle$meta$secs, j$base_seed)))
      }
    })
  })

  output$break_status <- renderUI(sc_break_status_ui(status()))

  output$break_story <- renderUI({
    r <- result()
    target <- as.numeric(input$break_target %||% 80)
    validate(need(!is.null(r) && !is.null(r$points) && nrow(r$points),
                  "Run a demand sweep, or restore the preloaded one, to fill this summary."))
    p(class = "mb-0", sc_break_story(r, target))
  })

  output$break_ontime_plot <- renderPlot({
    r <- result()
    target <- as.numeric(input$break_target %||% 80)
    validate(need(!is.null(r) && !is.null(r$points) && nrow(r$points),
                  "Run a demand sweep to draw the on-time chart."))
    w <- session$clientData$output_break_ontime_plot_width
    sc_plot_break_ontime(r, target, narrow = isTRUE(w < 700))
  }, res = 96)

  output$break_cycle_plot <- renderPlot({
    r <- result()
    target <- as.numeric(input$break_target %||% 80)
    validate(need(!is.null(r) && !is.null(r$points) && nrow(r$points),
                  "Run a demand sweep to draw the cycle-time chart."))
    w <- session$clientData$output_break_cycle_plot_width
    sc_plot_break_cycle(r, target, narrow = isTRUE(w < 700))
  }, res = 96)

  output$break_util_plot <- renderPlot({
    r <- result()
    target <- as.numeric(input$break_target %||% 80)
    validate(need(!is.null(r) && !is.null(r$util) && nrow(r$util),
                  "Run a demand sweep to draw the saturation chart."))
    w <- session$clientData$output_break_util_plot_width
    sc_plot_break_util(r, target, narrow = isTRUE(w < 700))
  }, res = 96)

  output$break_table <- gt::render_gt({
    r <- result()
    target <- as.numeric(input$break_target %||% 80)
    validate(need(!is.null(r) && !is.null(r$points) && nrow(r$points),
                  "Run a demand sweep to fill this table."))
    w <- session$clientData$output_break_ontime_plot_width
    sc_table_break(r, target, narrow = isTRUE(w < 700))
  })
}
