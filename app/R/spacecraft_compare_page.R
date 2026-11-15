# spacecraft_compare_page.R
# Compare page UI, the results table, the spread plot, and the server that
# loads the preload, adds a scenario from current Settings, and runs a live
# comparison with a progress bar and a Cancel button.

sc_compare_colors <- c("#31a354", "#e0a032", "#c43c3c", "#17becf")

sc_compare_palette <- function(names) {
  stats::setNames(sc_compare_colors[seq_along(names)], names)
}

sc_compare_value_label <- function(mean, p10, p90, unit) {
  # p10/p90 are kept in the signature so callers stay stable; the spread now
  # lives on the per-metric charts, not in the table cell.
  sprintf("%.1f", mean)
}

sc_compare_delta_label <- function(delta, unit, is_baseline) {
  suffix <- dplyr::recode(unit, `%` = " points", hours = " h",
                          units = "", .default = "")
  dplyr::case_when(
    is_baseline | is.na(delta) ~ "\u2014",
    TRUE ~ paste0(ifelse(delta > 0, "+", "\u2212"), sprintf("%.1f", abs(delta)), suffix)
  )
}

# One row per metric, one column per scenario: the mean on the first line,
# the delta versus the baseline stacked underneath.
sc_table_compare <- function(tbl) {
  tbl <- sc_compare_mark_deltas(tbl)
  labels <- tbl %>%
    dplyr::distinct(id, scenario, short, is_baseline) %>%
    dplyr::arrange(dplyr::desc(is_baseline), id)
  metrics <- sc_compare_metrics()
  colour <- dplyr::case_when(
    tbl$is_baseline | is.na(tbl$notable) | !tbl$notable ~ "#6c757d",
    tbl$improved %in% TRUE ~ "#1b9e77",
    tbl$improved %in% FALSE ~ "#c43c3c",
    TRUE ~ "#6c757d"
  )
  weight <- ifelse(tbl$notable %in% TRUE, "700", "500")
  wide <- tbl %>%
    dplyr::mutate(
      value = sc_compare_value_label(mean, p10, p90, unit),
      vs = sc_compare_delta_label(delta, unit, is_baseline),
      cell = dplyr::if_else(
        is_baseline,
        value,
        sprintf("%s<br><span style='color:%s;font-weight:%s'>%s</span>", value, colour, weight, vs)
      )
    ) %>%
    dplyr::select(id, col, cell) %>%
    tidyr::pivot_wider(names_from = id, values_from = cell)
  shown <- metrics %>%
    dplyr::transmute(
      col,
      Metric = sprintf('<span title="%s">%s</span>',
                       htmltools::htmlEscape(tip), htmltools::htmlEscape(label))
    ) %>%
    dplyr::left_join(wide, by = "col") %>%
    dplyr::select(-col)
  scen_cols <- labels$id
  g <- shown %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::cols_label(Metric = "Metric") %>%
    gt::cols_align("left", columns = Metric) %>%
    gt::cols_align("right", columns = dplyr::any_of(scen_cols)) %>%
    gt::cols_label(.list = rlang::set_names(labels$short, labels$id)) %>%
    gt::cols_width(Metric ~ gt::px(210)) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4)) %>%
    gt::tab_source_note(paste(
      "Each cell is the mean across replications.",
      "The second line is the change versus the baseline: green is an improvement, red is a move the other way,",
      "and grey is a change smaller than a quarter of the baseline P10–P90 half-width, so it sits inside the usual run-to-run spread.",
      "The 10th to 90th percentile is on the spread charts below, one tab per metric.",
      "Hover a metric name for a short description."
    ))
  g
}

# One panel per metric: a point at the mean, a bar from P10 to P90, and a
# dashed line at the baseline mean. About three y-axis breaks so the labels
# do not overlap. No rugs; legend in one row at the bottom; base size 16.
sc_plot_compare <- function(tbl, narrow = FALSE, metric_id = NULL) {
  metrics <- sc_compare_metrics()
  if (!is.null(metric_id)) {
    metrics <- metrics %>% dplyr::filter(.data$id == metric_id)
    stopifnot(nrow(metrics) == 1L)
  }
  scen <- tbl %>%
    dplyr::distinct(id, scenario, short, is_baseline) %>%
    dplyr::arrange(dplyr::desc(is_baseline), id)
  scen_levels <- scen$short
  d <- tbl %>%
    dplyr::select(-dplyr::any_of("label")) %>%
    dplyr::inner_join(metrics %>% dplyr::select(col, label), by = "col") %>%
    dplyr::mutate(
      metric = factor(label, levels = metrics$label),
      scenario = factor(short, levels = scen_levels)
    )
  pal <- sc_compare_palette(scen_levels)
  shapes <- sc_compare_set_shapes(scen_levels)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = scenario, y = mean, colour = scenario, shape = scenario)) +
    ggplot2::geom_hline(data = d %>% dplyr::filter(is_baseline) %>% dplyr::select(metric, baseline_mean),
                        ggplot2::aes(yintercept = baseline_mean),
                        linetype = "dashed", linewidth = 1, colour = "#1a1a1a") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = p10, ymax = p90), linewidth = 2.4) +
    ggplot2::geom_point(size = 4) +
    ggplot2::scale_x_discrete(limits = scen_levels) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    ggplot2::scale_colour_manual(values = pal, breaks = scen_levels, name = NULL) +
    ggplot2::scale_shape_manual(values = shapes, breaks = scen_levels, name = NULL) +
    ggplot2::labs(x = NULL, y = if (nlevels(d$metric) > 1) NULL else as.character(d$metric[[1]])) +
    theme_sc() +
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, face = "bold"),
                   panel.spacing = ggplot2::unit(1.1, "lines"))
  if (nlevels(d$metric) > 1) {
    p <- p + ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 1)
  }
  if (isTRUE(narrow)) {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank()) +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, title = NULL))
  } else {
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 18, hjust = 1)) +
      sc_one_row_legend()
  }
  p
}


sc_compare_ui <- function() {
  nav_panel(
    "Compare",
    tags$style(HTML("
      .sc-compare-pick { cursor: pointer; border-width: 2px !important; }
      .sc-compare-pick:hover { border-color: #17becf; }
      .sc-compare-picked { border-color: #31a354 !important; background: #f3faf5; }
      .sc-compare-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .sc-compare-table-wrap .gt_table { width: 100%; }
      .sc-compare-table-wrap .gt_table thead th:first-child,
      .sc-compare-table-wrap .gt_table tbody td:first-child { white-space: nowrap; }
      .sc-compare-table-wrap .gt_sourcenote,
      .sc-compare-table-wrap .gt_sourcenotes { white-space: normal; }
      @media (max-width: 800px) {
        .sc-compare-table-wrap .gt_table { min-width: 640px; }
      }
    ")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Scenarios", width = 320, open = "desktop",
        p(class = "small",
          "The precomputed set is the default line plus five capacity moves, scored first by throughput gain, then by HPU, WIP and cycle time. Click two scenarios to put them on the detail charts; add a custom what-if from current Settings and run a comparison to score it."),
        uiOutput("compare_scenario_list"),
        actionButton("compare_add", tagList(icon("plus"), " Add from current Settings"),
                     class = "btn-outline-primary w-100 mb-2"),
        actionButton("compare_restore", "Restore preloaded set", class = "btn-outline-secondary w-100 mb-3"),
        numericInput("compare_reps", "Replications", 10, min = 3, max = 40, step = 1),
        numericInput("compare_seed", "Shared seed", 42, min = 1, step = 1),
        actionButton("compare_run", tagList(icon("play"), " Run comparison"), class = "btn-primary w-100 mb-2"),
        actionButton("compare_cancel", "Cancel", class = "btn-outline-secondary w-100 mb-3"),
        helpText("Live runs share one seed stream across every scenario (common random numbers). Cancel stops the work between replications. The preloaded set opens at once, with no run.")
      ),
      uiOutput("compare_status"),
      card(
        card_header("What each move buys"),
        plotOutput("compare_gains_plot", height = 280),
        div(class = "sc-compare-table-wrap", gt::gt_output("compare_gains_table"))
      ),
      card(card_header("How the scenarios differ"), tableOutput("compare_diff_table")),
      card(card_header("Results"),
           div(class = "sc-compare-table-wrap", gt::gt_output("compare_table"))),
      navset_card_tab(
        id = "compare_spread_tabs",
        title = "Spread across replications",
        nav_panel(
          "Throughput",
          plotly::plotlyOutput("compare_plot_throughput", height = "400px"),
          p(class = "small text-muted mt-2 mb-0",
            "Each curve is the density of throughput (units shipped in a replication) for one scenario.",
            "The rug and the shaped ticks mark every replication. Transparency lets the two densities overlap.",
            "The dashed line is the baseline mean when the baseline is one of the two selected scenarios.")
        ),
        nav_panel(
          "Cycle time",
          plotly::plotlyOutput("compare_plot_cycle", height = "400px"),
          p(class = "small text-muted mt-2 mb-0",
            "Overlapping histogram of cycle time for every shipped unit, one series per scenario, with transparency.",
            "The axis shows the central 95% of shipped units.")
        ),
        nav_panel(
          "Touch labor",
          plotly::plotlyOutput("compare_plot_hpu", height = "400px"),
          p(class = "small text-muted mt-2 mb-0",
            "Overlapping histogram of touch labor (hours per shipped unit), one series per scenario, with transparency.",
            "The axis shows the central 95% of shipped units.")
        ),
        nav_panel(
          "Downtime",
          plotOutput("compare_plot_down", height = 420),
          p(class = "small text-muted mt-2 mb-0",
            "Calendar downtime fraction by station (repair hours divided by capacity times the horizon).",
            "Bars are the mean across replications; thin error bars run from the 10th to the 90th percentile.")
        ),
        nav_panel(
          "Flow",
          plotOutput("compare_plot_wip_flow", height = 400),
          p(class = "small text-muted mt-2 mb-0",
            "WIP over time: faint lines are replications; bold lines are means.")
        ),
        nav_panel(
          "Downloads",
          uiOutput("compare_downloads"),
          p(class = "small text-muted mt-2 mb-0",
            "Tables include a set column so every scenario in this comparison is in the same file.",
            "Filenames start with compare_ so they do not overwrite a Simulate download of the same table.")
        ),
        footer = p(class = "small text-muted mb-0 px-3 pb-2",
          "Click a scenario in the sidebar to put it on the charts (two at a time).",
          "Throughput is a density with a rug of each replication; cycle time and touch labor are overlapping histograms of shipped units.",
          "Lower is better for cycle time, WIP and touch labor; higher is better for throughput.",
          "In the table, a grey change is smaller than a quarter of the baseline P10–P90 half-width, so it sits inside the usual run-to-run spread.")
      )
    )
  )
}

sc_compare_status_ui <- function(state) {
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

# Wire the page into sc_server(). current_params is the reactive used by Simulate.
sc_compare_server <- function(input, output, session, current_params) {
  preload <- sc_load_preload()
  scenarios <- reactiveVal(
    if (is.null(preload)) sc_compare_catalog() else preload$scenarios
  )
  selected <- reactiveVal(
    sc_compare_default_selection(
      if (is.null(preload)) sc_compare_catalog() else preload$scenarios,
      tbl = if (is.null(preload)) NULL else preload$table
    )
  )
  result <- reactiveVal(preload)
  running <- reactiveVal(FALSE)
  cancel_flag <- reactiveVal(FALSE)
  job <- reactiveVal(NULL)
  status <- reactiveVal(list(
    kind = if (is.null(preload)) "idle" else "done",
    text = if (is.null(preload)) {
      "No preloaded results found. Click Run comparison to generate them."
    } else {
      sprintf("Showing the preloaded set (%d replications, shared seed %s). Add a scenario from Settings or run a live comparison.",
              preload$meta$n_reps, preload$meta$base_seed)
    },
    pct = NULL
  ))

  output$compare_scenario_list <- renderUI({
    sc <- scenarios()
    picked <- selected()
    n_what <- sum(!sc$is_baseline)
    items <- purrr::pmap(sc, function(id, label, blurb, is_baseline, ...) {
      on <- id %in% picked
      div(
        class = paste("sc-compare-pick border rounded p-2 mb-2", if (on) "sc-compare-picked"),
        `data-id` = id,
        role = "button",
        tabindex = "0",
        onclick = sprintf("Shiny.setInputValue('compare_toggle', '%s', {priority: 'event'})", id),
        onkeydown = sprintf("if(event.key==='Enter'||event.key===' '){event.preventDefault();Shiny.setInputValue('compare_toggle','%s',{priority:'event'})}", id),
        tags$strong(label),
        if (is_baseline) tags$span(class = "badge bg-secondary ms-1", "Baseline"),
        if (on) tags$span(class = "badge bg-success ms-1", "In the charts"),
        p(class = "small mb-0", blurb)
      )
    })
    drop <- if (n_what) tagList(
      selectInput("compare_drop_pick", "Remove a what-if",
                  choices = stats::setNames(sc$id[!sc$is_baseline], sc$label[!sc$is_baseline])),
      actionButton("compare_drop", "Remove selected", class = "btn btn-sm btn-outline-secondary mb-2")
    )
    tagList(
      items, drop,
      p(class = "small text-muted mb-2",
        sprintf("%d what-if scenario%s in the list. %d in the charts (two at a time).",
                n_what, if (n_what == 1) "" else "s", length(picked)))
    )
  })

  observeEvent(input$compare_toggle, {
    id <- input$compare_toggle
    if (is.null(id) || !nzchar(id)) return()
    selected(sc_compare_toggle_selection(selected(), id, scenarios()$id, max_n = 2L))
  })

  observeEvent(input$compare_drop, {
    id <- input$compare_drop_pick
    if (!is.null(id) && nzchar(id)) {
      sc <- dplyr::filter(scenarios(), .data$id != id)
      scenarios(sc)
      cur <- setdiff(selected(), id)
      if (!length(cur)) cur <- sc_compare_default_selection(sc)
      selected(cur)
      showNotification("Removed that what-if scenario.", type = "message", duration = 3)
    }
  })

  observeEvent(input$compare_restore, {
    if (is.null(preload)) {
      scenarios(sc_compare_catalog())
      selected(sc_compare_default_selection(sc_compare_catalog()))
      result(NULL)
      status(list(kind = "idle", text = "Catalog restored. Click Run comparison to generate results.", pct = NULL))
    } else {
      scenarios(preload$scenarios)
      selected(sc_compare_default_selection(preload$scenarios, tbl = preload$table))
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Showing the preloaded set (%d replications, shared seed %s).",
                                 preload$meta$n_reps, preload$meta$base_seed)))
    }
  })

  observeEvent(input$compare_add, {
    sc <- scenarios()
    if (sum(!sc$is_baseline) >= 7) {
      showNotification("The page already has seven what-if scenarios. Remove one first.", type = "warning")
      return()
    }
    p <- current_params()
    dup <- purrr::map_lgl(sc$params, ~ identical(.x, p))
    if (any(dup)) {
      showNotification(sprintf("Those settings are already in the list as “%s”.", sc$label[dup][[1]]),
                       type = "warning")
      return()
    }
    changes <- sc_params_changes(sc$params[sc$is_baseline][[1]], p)
    blurb <- if (length(changes)) paste(changes, collapse = "; ") else
      "A snapshot of the current Settings (including any variant edits)."
    label <- if (length(changes) == 1) sub(":.*", "", changes[[1]]) else "Current settings"
    id <- paste0("custom_", format(Sys.time(), "%H%M%S"))
    scenarios(dplyr::bind_rows(sc, sc_scenario(id, label, blurb, p)))
    showNotification("Added the current Settings as a what-if scenario. Click Run comparison to score it.",
                     type = "message", duration = 5)
  })

  observeEvent(input$compare_run, {
    sc <- scenarios()
    if (!any(sc$is_baseline)) {
      showNotification("Keep a baseline scenario in the list.", type = "error")
      return()
    }
    if (isTRUE(running())) return()
    n_reps <- as.integer(input$compare_reps %||% 10)
    seed <- as.integer(input$compare_seed %||% 42)
    # Instant path: the open list matches the preload exactly.
    if (!is.null(preload) && n_reps == preload$meta$n_reps && seed == preload$meta$base_seed &&
        identical(sc$id, preload$scenarios$id) &&
        all(purrr::map2_lgl(sc$params, preload$scenarios$params, identical))) {
      result(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Those settings match the preloaded set, so the saved results were reused (%d replications, seed %s).",
                                 n_reps, seed)))
      return()
    }
    cancel_flag(FALSE)
    running(TRUE)
    job(list(scenarios = sc, n_reps = n_reps, base_seed = seed,
             i_scen = 1L, i_rep = 1L, runs = vector("list", nrow(sc)),
             acc = vector("list", n_reps), plan_cache = list(),
             t0 = Sys.time(), total = nrow(sc) * n_reps, done = 0L))
    status(list(kind = "running", pct = 0,
                text = sprintf("Running %d scenario(s) × %d replications…", nrow(sc), n_reps)))
  })

  observeEvent(input$compare_cancel, {
    if (isTRUE(running())) {
      cancel_flag(TRUE)
      status(list(kind = "cancelled", pct = NULL, text = "Cancelling after the current replication…"))
    }
  })

  # Cooperative stepper: one replication per tick so Cancel can land.
  observe({
    req(running())
    invalidateLater(20)
    isolate({
      j <- job()
      if (is.null(j)) { running(FALSE); return() }
      if (isTRUE(cancel_flag())) {
        job(NULL); running(FALSE)
        status(list(kind = "cancelled", pct = 100 * j$done / j$total,
                    text = sprintf("Cancelled after %d of %d replications.", j$done, j$total)))
        return()
      }
      s <- j$i_scen; r <- j$i_rep
      params <- j$scenarios$params[[s]]
      seed <- as.integer(j$base_seed + r - 1L)
      key <- paste(rlang::hash(sc_plan_signature(params)), seed, sep = ".")
      if (!key %in% names(j$plan_cache)) j$plan_cache[[key]] <- sc_arrival_plan(params, seed)
      out <- sc_run_one(params, seed, plan = j$plan_cache[[key]])
      out$rep <- r
      j$acc[[r]] <- out
      j$done <- j$done + 1L
      if (r == j$n_reps) {
        sm <- sc_summarise(j$acc, params)
        sm$flow <- sc_compare_flow(j$acc, params, j$scenarios$label[[s]])
        sm$units <- NULL
        j$runs[[s]] <- sm
        j$acc <- vector("list", j$n_reps)
        j$i_scen <- s + 1L
        j$i_rep <- 1L
      } else {
        j$i_rep <- r + 1L
      }
      pct <- 100 * j$done / j$total
      status(list(kind = "running", pct = pct,
                  text = sprintf("Running “%s”, replication %d of %d (%d of %d done)…",
                                 j$scenarios$label[[s]], r, j$n_reps, j$done, j$total)))
      if (j$i_scen > nrow(j$scenarios)) {
        results <- tibble::tibble(
          id = j$scenarios$id,
          per_rep = purrr::map(j$runs, "per_rep"),
          summary = purrr::map(j$runs, "summary"),
          flow = purrr::map(j$runs, "flow")
        )
        out <- list(
          scenarios = j$scenarios, results = results,
          table = sc_compare_table(j$scenarios, results),
          meta = list(n_reps = j$n_reps, base_seed = j$base_seed,
                      secs = as.numeric(difftime(Sys.time(), j$t0, units = "secs")),
                      cancelled = FALSE)
        )
        result(out)
        job(NULL); running(FALSE)
        status(list(kind = "done", pct = 100,
                    text = sprintf("Done. %d scenario(s) × %d replications in %.1f s, shared seed %s.",
                                   nrow(j$scenarios), j$n_reps, out$meta$secs, j$base_seed)))
      } else {
        job(j)
      }
    })
  })

  output$compare_status <- renderUI(sc_compare_status_ui(status()))

  output$compare_diff_table <- renderTable({
    sc <- scenarios()
    base <- sc$params[sc$is_baseline][[1]]
    what <- sc[!sc$is_baseline, ]
    if (!nrow(what)) return(tibble::tibble(Scenario = sc$label[sc$is_baseline], Difference = "Reference line (no changes)."))
    purrr::pmap_dfr(what, function(label, params, ...) {
      ch <- sc_params_changes(base, params)
      tibble::tibble(Scenario = label, Difference = if (length(ch)) paste(ch, collapse = "; ") else "No line-wide changes (check Variants on Settings).")
    })
  }, striped = TRUE)

  shown <- reactive({
    r <- result()
    ids <- selected()
    validate(need(!is.null(r) && !is.null(r$table) && nrow(r$table),
                  "Run a comparison, or restore the preloaded set, to fill this table."))
    tbl <- sc_compare_filter_table(r$table, ids)
    validate(need(nrow(tbl) > 0,
                  "Run a comparison for the selected scenario, or pick one that already has results."))
    list(table = tbl, points = sc_compare_rep_points(r, ids), ids = ids)
  })

  output$compare_table <- gt::render_gt({
    sc_table_compare(shown()$table)
  })

  compare_flows <- reactive({
    r <- result()
    if (is.null(r) || is.null(r$results) || !"flow" %in% names(r$results)) return(NULL)
    bound <- sc_compare_bind_flows(r$results$flow)
    sc_compare_filter_flow(bound, scenarios(), selected())
  })

  output$compare_plot_throughput <- plotly::renderPlotly({
    sh <- shown()
    validate(need(!is.null(sh$points) && nrow(sh$points),
                  "Run a comparison, or restore the preloaded set, to draw this plot."))
    sc_plotly_throughput_density(sh$points, sh$table)
  })
  output$compare_plot_cycle <- plotly::renderPlotly({
    fl <- compare_flows()
    u <- sc_compare_units(fl, scenarios(), selected())
    validate(need(!is.null(u) && nrow(u),
                  "Run a comparison, or restore the preloaded set, to draw this plot."))
    sc_plotly_overlap_hist(u, x = "cycle_h", xlab = "Cycle time (h)")
  })
  output$compare_plot_hpu <- plotly::renderPlotly({
    fl <- compare_flows()
    u <- sc_compare_units(fl, scenarios(), selected())
    validate(need(!is.null(u) && nrow(u),
                  "Run a comparison, or restore the preloaded set, to draw this plot."))
    sc_plotly_overlap_hist(u, x = "labor_hpu", xlab = "Touch labor (h per unit)")
  })
  output$compare_gains_plot <- renderPlot({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$table), "Run a comparison to fill this chart."))
    sc_plot_compare_gains(r$table)
  }, res = 96)
  output$compare_gains_table <- gt::render_gt({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$table), "Run a comparison to fill this table."))
    sc_table_compare_gains(r$table)
  })
  # Keep hidden metric tabs alive so a sidebar remount and a crosstalk
  # highlight still apply when the tab is not in view.
  for (nm in c("compare_plot_throughput", "compare_plot_cycle", "compare_plot_hpu")) {
    outputOptions(output, nm, suspendWhenHidden = FALSE)
  }

  # Attach a `scenario` column (the short label). Do not touch `short`, which
  # on downtime is the station name.
  compare_flow_with_short <- function(d) {
    if (is.null(d) || !nrow(d) || !"set" %in% names(d)) return(d)
    sc <- scenarios()
    m <- dplyr::bind_rows(
      sc %>% dplyr::transmute(set = as.character(.data$label), scenario = as.character(.data$short)),
      sc %>% dplyr::transmute(set = as.character(.data$short), scenario = as.character(.data$short))
    ) %>% dplyr::distinct(.data$set, .keep_all = TRUE)
    d %>%
      dplyr::mutate(set = as.character(.data$set)) %>%
      dplyr::left_join(m, by = "set") %>%
      dplyr::mutate(scenario = dplyr::coalesce(.data$scenario, .data$set))
  }

  output$compare_plot_wip_flow <- renderPlot({
    fl <- compare_flows()
    validate(need(!is.null(fl) && !is.null(fl$wip) && nrow(fl$wip),
                  "Run a comparison, or restore the preloaded set, to draw the WIP flow."))
    w <- session$clientData$output_compare_plot_wip_flow_width
    sc_plot_compare_wip_flow(compare_flow_with_short(fl$wip), narrow = isTRUE(w < 700))
  }, res = 96)

  output$compare_plot_down <- renderPlot({
    fl <- compare_flows()
    validate(need(!is.null(fl) && !is.null(fl$downtime) && nrow(fl$downtime),
                  "Run a comparison, or restore the preloaded set, to draw downtime."))
    w <- session$clientData$output_compare_plot_down_width
    sc_plot_compare_downtime(compare_flow_with_short(fl$downtime), narrow = isTRUE(w < 700))
  }, res = 96)

  output$compare_downloads <- renderUI({
    fl <- compare_flows()
    if (is.null(fl)) {
      return(p(class = "text-muted", "Run a comparison, or restore the preloaded set, to enable downloads."))
    }
    sc_download_buttons_ui("cmp_dl_", exclude = "arrivals_res", file_prefix = "compare_")
  })

  sc_bind_downloads(output, "cmp_dl_", function() {
    sc_compare_export_tables(compare_flows())
  }, zip_name = "spacecraft_compare", exclude = "arrivals_res", file_prefix = "compare_")
}
