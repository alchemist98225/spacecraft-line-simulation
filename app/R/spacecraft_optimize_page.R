# spacecraft_optimize_page.R
# Optimize page: genetic search over station counts, live best-so-far chart,
# recheck of the baseline and the top candidates with more replications, and
# a plain-English recommendation. Instant from a preloaded result; live runs
# have per-generation status, a progress bar and Cancel.

sc_opt_colors <- c(Best = "#2c7fb8", Mean = "#e0a032")
sc_opt_shapes <- c(Best = 16, Mean = 17)

sc_plot_opt_history <- function(bundle, narrow = FALSE) {
  hist <- bundle$history
  if (is.null(hist) || !nrow(hist)) {
    return(ggplot2::ggplot() + ggplot2::theme_void() +
             ggplot2::labs(title = "No search history yet"))
  }
  d <- hist %>%
    tidyr::pivot_longer(c(best_fitness, mean_fitness), names_to = "series") %>%
    dplyr::mutate(series = dplyr::recode(.data$series,
                                         best_fitness = "Best",
                                         mean_fitness = "Mean"))
  d$series <- factor(d$series, levels = c("Best", "Mean"))
  obj <- bundle$meta$objective %||% "ontime"
  d$value <- sc_opt_score_from_fitness(d$value, obj)
  base_fit <- NA_real_
  if (!is.null(bundle$evaluated) && nrow(bundle$evaluated)) {
    key <- sc_genome_key(bundle$defs$default)
    hit <- bundle$evaluated %>% dplyr::filter(.data$key == key)
    if (nrow(hit)) base_fit <- sc_opt_score_from_fitness(hit$fitness[[1]], obj)
  }
  xlab <- "Generation"
  ylab <- sc_opt_score_label(obj, narrow = narrow)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = generation, y = value,
                                       colour = series, shape = series)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 3.4) +
    ggplot2::scale_colour_manual(values = sc_opt_colors, breaks = names(sc_opt_colors), name = NULL) +
    ggplot2::scale_shape_manual(values = sc_opt_shapes, breaks = names(sc_opt_shapes), name = NULL) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(hist$generation)),
      minor_breaks = NULL,
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.12, 0.14) else c(0.04, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 5),
      expand = ggplot2::expansion(mult = if (isTRUE(narrow)) c(0.08, 0.20) else c(0.05, 0.10))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = xlab, y = ylab) +
    theme_sc() +
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 1, title = NULL),
      shape = ggplot2::guide_legend(nrow = 1, title = NULL)
    )
  if (is.finite(base_fit)) {
    p <- p + ggplot2::geom_hline(yintercept = base_fit, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  if (isTRUE(narrow)) {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(5.5, 18, 8, 4))
  }
  p
}

sc_opt_candidate_name <- function(recheck) {
  out <- rep(NA_character_, nrow(recheck))
  is_base <- recheck$is_baseline %in% TRUE
  is_best <- recheck$is_best %in% TRUE
  out[is_base] <- "Current line"
  # If the current line wins, keep that label and highlight the row.
  out[is_best & !is_base] <- "Recommended"
  rest <- is.na(out)
  if (any(rest)) out[rest] <- paste("Alternative", seq_len(sum(rest)))
  out
}

sc_table_opt <- function(bundle, narrow = FALSE) {
  rec <- bundle$recheck
  if (is.null(rec) || !nrow(rec)) {
    return(gt::gt(tibble::tibble(Note = "Run a search to fill this table.")) %>% sc_gt_style())
  }
  obj <- bundle$meta$objective %||% "ontime"
  cost <- bundle$meta$capacity_cost %||% 1
  if (!"score_mean" %in% names(rec) || !"delta_mean" %in% names(rec)) {
    rec <- sc_opt_attach_paired(rec, obj, cost)
  }
  rec <- rec %>% dplyr::arrange(.data$rank)
  names <- sc_opt_candidate_name(rec)
  cycle <- identical(obj, "cycle")
  score_lab <- sc_opt_score_label(obj, narrow = narrow)
  rec$score_txt <- if (cycle) {
    sprintf("%.1f h", rec$score_mean)
  } else {
    sprintf("%.1f%%", rec$score_mean)
  }
  rec$vs_txt <- vapply(seq_len(nrow(rec)), function(i) {
    sc_opt_format_delta(
      rec$delta_mean[[i]], rec$delta_lo[[i]], rec$delta_hi[[i]],
      obj, isTRUE(rec$within_noise[[i]]), isTRUE(rec$is_baseline[[i]]),
      narrow = narrow, html = TRUE
    )
  }, character(1))
  if (isTRUE(narrow)) {
    d <- tibble::tibble(
      Change = rec$change_short,
      score = rec$score_txt,
      `vs current` = rec$vs_txt,
      is_best = rec$is_best %in% TRUE,
      clearly_better = rec$clearly_better %in% TRUE,
      within_noise = rec$within_noise %in% TRUE
    )
    names(d)[names(d) == "score"] <- score_lab
  } else {
    d <- tibble::tibble(
      Candidate = names,
      Change = rec$change,
      `On time` = sprintf("%.1f%%", rec$on_time_pct),
      `Cycle time` = sprintf("%.1f h", rec$cycle_mean_h),
      Extra = as.integer(rec$extra_capacity),
      score = rec$score_txt,
      `vs current` = rec$vs_txt,
      is_best = rec$is_best %in% TRUE,
      clearly_better = rec$clearly_better %in% TRUE,
      within_noise = rec$within_noise %in% TRUE
    )
    names(d)[names(d) == "score"] <- score_lab
  }
  score_tip <- if (cycle) {
    "Net cycle time (h) = mean cycle time plus cost per extra station × extra stations. Lower is better."
  } else {
    "Net on-time score (%) = on-time % minus cost per extra station × extra stations. Higher is better."
  }
  vs_tip <- "Mean paired difference versus the current line across the same replications, with a 90% interval. A grey 'within noise' mark means that interval includes zero."
  tips_wide <- list(
    Candidate = gt::html("<span title='Current line is the baseline. Recommended is the best rechecked net score, highlighted in green only when the paired interval excludes zero in its favor.'>Candidate</span>"),
    Change = gt::html("<span title='How this mix differs from the current station counts.'>Change</span>"),
    `On time` = gt::html("<span title='Mean on-time share across the recheck replications.'>On time</span>"),
    `Cycle time` = gt::html("<span title='Mean cycle time (release to ship) across the recheck replications.'>Cycle time</span>"),
    Extra = gt::html("<span title='Stations, chambers or boards added over the current line. Reductions do not earn a credit.'>Extra</span>")
  )
  score_hdr <- if (cycle) {
    "Net cycle time (h)"
  } else {
    "Net on-time<br>score (%)"
  }
  tips_wide[[score_lab]] <- gt::html(sprintf("<span title='%s'>%s</span>", score_tip, score_hdr))
  tips_wide[["vs current"]] <- gt::html(sprintf("<span title='%s'>vs<br>current</span>", vs_tip))
  tips_narrow <- list(
    Change = gt::html("<span title='How this mix differs from the current station counts.'>Change</span>")
  )
  tips_narrow[[score_lab]] <- gt::html(sprintf("<span title='%s'>%s</span>", score_tip, score_lab))
  tips_narrow[["vs current"]] <- gt::html(sprintf("<span title='%s'>vs current</span>", vs_tip))
  g <- d %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_hide(columns = dplyr::any_of(c("is_best", "clearly_better", "within_noise"))) %>%
    gt::cols_align("right", columns = -dplyr::any_of(c("Candidate", "Change"))) %>%
    gt::cols_label(.list = if (isTRUE(narrow)) tips_narrow else tips_wide) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#c7e9c0"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = .data$is_best & .data$clearly_better)
    ) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#e9ecef"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = .data$is_best & !.data$clearly_better)
    ) %>%
    gt::tab_style(
      style = list(gt::cell_text(color = "#6c757d")),
      locations = gt::cells_body(columns = `vs current`, rows = .data$within_noise)
    ) %>%
    gt::fmt_markdown(columns = `vs current`) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4))
  if (isTRUE(narrow)) {
    if (cycle) {
      g <- gt::cols_width(g, Change ~ gt::px(86), `Net cycle (h)` ~ gt::px(68),
                          `vs current` ~ gt::px(118))
    } else {
      g <- gt::cols_width(g, Change ~ gt::px(86), `Net on-time` ~ gt::px(68),
                          `vs current` ~ gt::px(118))
    }
    g <- g %>% gt::tab_options(table.font.size = gt::px(13),
                               data_row.padding = gt::px(4),
                               column_labels.padding = gt::px(3))
  } else {
    g <- gt::cols_width(g, Candidate ~ gt::px(120), Change ~ gt::px(180), `vs current` ~ gt::px(150))
    n_re <- bundle$meta$elite_reps %||% NA
    n_set <- bundle$meta$n_recheck %||% nrow(rec)
    g <- g %>% gt::tab_source_note(paste(
      sprintf("Each row is a %s-replication recheck with the same seeds (common random numbers).",
              if (is.finite(n_re)) as.integer(n_re) else "multi"),
      sprintf("%d setups were re-checked: the current line, the top distinct search candidates, and each one-station change from the current line.",
              as.integer(n_set)),
      score_tip,
      "Change vs current is the mean paired difference with a 90% interval; a grey 'within noise' mark means that interval includes zero.",
      "The recommendation is highlighted in green only when that paired interval excludes zero in its favor.",
      "The dashed line on the chart is the current line's search score. Numbers are illustrative."
    ))
  }
  g
}

sc_opt_caption <- function(bundle) {
  if (is.null(bundle) || is.null(bundle$history) || !nrow(bundle$history)) {
    return("Run a search to draw the chart.")
  }
  obj <- bundle$meta$objective %||% "ontime"
  metric <- if (identical(obj, "cycle")) "net cycle time" else "net on-time score"
  dir <- sc_opt_score_direction(obj)
  bits <- c(
    sprintf("A filled blue circle is the best %s in that generation; a gold triangle is the population mean. %s",
            metric, dir),
    sprintf("The dashed horizontal line is the current line's %s during the search.", metric),
    "Search uses fewer replications; the table below re-checks the top candidates and each one-station change with more."
  )
  if (isTRUE(sc_opt_best_in_gen1(bundle$history))) {
    bits <- c(bits, "The best setup appeared in the first generation; later generations didn't beat it.")
  }
  paste(bits, collapse = " ")
}

sc_opt_status_ui <- function(state) {
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

# Exact recheck help: never "the top few".
sc_opt_help_text <- function(elite_n, elite_reps) {
  n <- as.integer(elite_n %||% 3)
  r <- as.integer(elite_reps %||% 15)
  paste(
    sprintf(
      "After the search, the current line, the top %d candidates from the search, and each one-station change from the current line (duplicates dropped) are re-checked with %d replications and the same seeds, so the table is fair to the current line.",
      n, r
    ),
    "The chart and table use a net on-time score (on-time % minus cost per extra station × extra stations; higher is better) or a net cycle time (mean cycle time plus the same cost term; lower is better).",
    "Change vs current is the mean paired difference across those replications, with a 90% interval. A grey 'within noise' mark means that interval includes zero."
  )
}

sc_optimize_ui <- function() {
  q <- sc_opt_presets()$quick
  nav_panel(
    "Optimize",
    tags$style(HTML("
      .sc-opt-table-wrap { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .sc-opt-table-wrap .gt_table { width: 100%; }
      .sc-opt-table-wrap .gt_sourcenote,
      .sc-opt-table-wrap .gt_sourcenotes { white-space: normal; }
      @media (max-width: 800px) {
        .sc-opt-table-wrap .gt_table { min-width: 0; }
      }
    ")),
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Optimize", width = 320, open = "desktop",
        p(class = "small",
          "This page searches station counts (integration stations, environmental chambers, functional test stands, MRB boards and acceptance stands) around the current Settings. The same seeds are used for every candidate, so a change comes from the mix, not from a luckier arrival stream."),
        selectInput("opt_objective", "Objective",
                    choices = stats::setNames(sc_ga_objectives()$id, sc_ga_objectives()$label),
                    selected = "ontime"),
        numericInput("opt_cost", "Cost per extra station", 1, min = 0, step = 0.5),
        p(class = "small text-muted", title = "Charged only for counts above the current line. Reductions do not earn a credit. The chart and table use the net score, not the internal fitness.",
          "Each extra station, chamber or board is charged as this many on-time points (or hours of cycle time). The chart and table report a net on-time score (on-time % minus cost × extra stations; higher is better) or a net cycle time (mean cycle time plus cost × extra; lower is better)."),
        actionButton("opt_run", tagList(icon("play"), " Run search"),
                     class = "btn-primary w-100 mb-2"),
        div(class = "d-flex flex-wrap gap-2 mb-2",
            actionButton("opt_preset_quick", "Quick", class = "btn-outline-primary btn-sm"),
            actionButton("opt_preset_thorough", "Thorough", class = "btn-outline-secondary btn-sm")),
        actionButton("opt_restore", "Restore preloaded result",
                     class = "btn-outline-secondary w-100 mb-2"),
        actionButton("opt_cancel", "Cancel", class = "btn-outline-secondary w-100 mb-3"),
        numericInput("opt_pop", "Population", q$pop_size, min = 4, max = 24, step = 1),
        numericInput("opt_gen", "Generations", q$n_gen, min = 2, max = 20, step = 1),
        numericInput("opt_reps", "Search replications", q$n_reps, min = 1, max = 10, step = 1),
        numericInput("opt_elite_n", "Recheck top candidates", q$elite_n, min = 1, max = 8, step = 1),
        numericInput("opt_elite_reps", "Recheck replications", q$elite_reps, min = 3, max = 30, step = 1),
        numericInput("opt_seed", "Seed", 42, min = 1, step = 1),
        uiOutput("opt_help")
      ),
      uiOutput("opt_status"),
      card(card_header("What this says"), uiOutput("opt_story")),
      card(
        card_header("Best so far vs generation"),
        plotOutput("opt_plot", height = 400),
        uiOutput("opt_caption")
      ),
      card(
        card_header("Top candidates (rechecked)"),
        div(class = "sc-opt-table-wrap", gt::gt_output("opt_table"))
      )
    )
  )
}

sc_optimize_server <- function(input, output, session, current_params) {
  preload <- sc_load_preload_optimize()
  result <- reactiveVal(preload)
  draft <- reactiveVal(preload)
  running <- reactiveVal(FALSE)
  cancel_flag <- reactiveVal(FALSE)
  job <- reactiveVal(NULL)
  preload_msg <- function() {
    if (is.null(preload)) {
      "No preloaded optimize result found. Click Run search to generate one from the current Settings."
    } else {
      sprintf(
        "Showing the preloaded Quick search (population %d, %d generations, %d search replications; %d setups re-checked with %d replications, seed %s — the current line, the top %d search candidates, and each one-station change). Change the settings and run a live search, or restore this result.",
        preload$meta$pop_size, preload$meta$n_gen, preload$meta$n_reps,
        preload$meta$n_recheck, preload$meta$elite_reps, preload$meta$base_seed,
        preload$meta$elite_n
      )
    }
  }
  status <- reactiveVal(list(
    kind = if (is.null(preload)) "idle" else "done",
    text = preload_msg(),
    pct = NULL
  ))

  matches_preload <- function(params, pop, gen, reps, elite_n, elite_reps, seed, cost, objective) {
    !is.null(preload) &&
      identical(params, preload$params) &&
      pop == preload$meta$pop_size &&
      gen == preload$meta$n_gen &&
      reps == preload$meta$n_reps &&
      elite_n == preload$meta$elite_n &&
      elite_reps == preload$meta$elite_reps &&
      seed == preload$meta$base_seed &&
      isTRUE(all.equal(cost, preload$meta$capacity_cost)) &&
      identical(objective, preload$meta$objective)
  }

  apply_preset <- function(name) {
    pr <- sc_opt_presets()[[name]]
    updateNumericInput(session, "opt_pop", value = pr$pop_size)
    updateNumericInput(session, "opt_gen", value = pr$n_gen)
    updateNumericInput(session, "opt_reps", value = pr$n_reps)
    updateNumericInput(session, "opt_elite_n", value = pr$elite_n)
    updateNumericInput(session, "opt_elite_reps", value = pr$elite_reps)
  }
  observeEvent(input$opt_preset_quick, apply_preset("quick"))
  observeEvent(input$opt_preset_thorough, apply_preset("thorough"))

  output$opt_help <- renderUI({
    helpText(sc_opt_help_text(input$opt_elite_n, input$opt_elite_reps),
             "Quick is sized to stay reasonable on a small server (about a minute on this model).",
             "The preloaded view used the Quick population and generations with 20 recheck replications, seed 42, on the default line.")
  })

  observeEvent(input$opt_restore, {
    if (is.null(preload)) {
      result(NULL); draft(NULL)
      status(list(kind = "idle", text = preload_msg(), pct = NULL))
    } else {
      result(preload); draft(preload)
      status(list(kind = "done", pct = NULL, text = preload_msg()))
    }
  })

  observeEvent(input$opt_run, {
    if (isTRUE(running())) return()
    p <- current_params()
    pop <- as.integer(input$opt_pop %||% 6)
    gen <- as.integer(input$opt_gen %||% 4)
    reps <- as.integer(input$opt_reps %||% 2)
    elite_n <- as.integer(input$opt_elite_n %||% 3)
    elite_reps <- as.integer(input$opt_elite_reps %||% 15)
    seed <- as.integer(input$opt_seed %||% 42)
    cost <- as.numeric(input$opt_cost %||% 2)
    objective <- input$opt_objective %||% "ontime"
    if (matches_preload(p, pop, gen, reps, elite_n, elite_reps, seed, cost, objective)) {
      result(preload); draft(preload)
      status(list(kind = "done", pct = NULL,
                  text = sprintf("Those settings match the preloaded result, so the saved search was reused (%d recheck replications, seed %s).",
                                 elite_reps, seed)))
      return()
    }
    cancel_flag(FALSE)
    running(TRUE)
    st <- sc_opt_new_state(p, pop, gen, reps, elite_n, elite_reps, seed, cost, objective)
    job(st)
    draft(list(history = tibble::tibble(), evaluated = tibble::tibble(),
               recheck = tibble::tibble(), defs = st$defs,
               meta = list(n_gen = gen, objective = objective)))
    status(list(kind = "running", pct = 0,
                text = sprintf("Starting generation 1 of %d…", gen)))
  })

  observeEvent(input$opt_cancel, {
    if (isTRUE(running())) {
      cancel_flag(TRUE)
      status(list(kind = "cancelled", pct = NULL,
                  text = "Cancelling after the current generation or recheck…"))
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
        bundle <- sc_opt_bundle(st)
        bundle$meta$source <- "live"
        result(bundle); draft(bundle)
        job(NULL); running(FALSE)
        n_done <- if (is.null(st$history)) 0L else nrow(st$history)
        status(list(kind = "cancelled", pct = NULL,
                    text = sprintf("Cancelled after %d of %d generations.", n_done, st$n_gen)))
        return()
      }
      if (identical(st$phase, "search")) {
        this_gen <- st$gen_i
        st <- sc_opt_search_step(st)
        job(st)
        draft(list(history = st$history, evaluated = st$evaluated,
                   recheck = tibble::tibble(), defs = st$defs,
                   meta = list(n_gen = st$n_gen, objective = st$objective)))
        best <- utils::tail(st$history$best_fitness, 1)
        if (identical(st$phase, "search")) {
          pct <- 75 * this_gen / st$n_gen
          status(list(kind = "running", pct = pct,
                      text = sprintf(
                        "Finished generation %d of %d (%s). Starting generation %d of %d…",
                        this_gen, st$n_gen,
                        if (identical(st$objective, "cycle"))
                          sprintf("best net cycle time %.1f h", sc_opt_score_from_fitness(best, st$objective))
                        else
                          sprintf("best net on-time score %.1f", sc_opt_score_from_fitness(best, st$objective)),
                        st$gen_i, st$n_gen)))
        } else {
          n_pool <- nrow(st$pool)
          pct <- 75
          status(list(kind = "running", pct = pct,
                      text = sprintf(
                        "Finished generation %d of %d (%s). Re-checking %d setups (the current line, the top %d search candidates, and each one-station change, duplicates dropped) with %d replications and the same seeds: starting 1 of %d…",
                        this_gen, st$n_gen,
                        if (identical(st$objective, "cycle"))
                          sprintf("best net cycle time %.1f h", sc_opt_score_from_fitness(best, st$objective))
                        else
                          sprintf("best net on-time score %.1f", sc_opt_score_from_fitness(best, st$objective)),
                        n_pool, st$elite_n, st$elite_reps, n_pool)))
        }
      } else if (identical(st$phase, "recheck")) {
        i <- st$i_recheck
        n_pool <- nrow(st$pool)
        st <- sc_opt_recheck_step(st)
        job(st)
        if (identical(st$phase, "recheck")) {
          pct <- 75 + 25 * i / n_pool
          status(list(kind = "running", pct = pct,
                      text = sprintf(
                        "Re-checking %d setups with %d replications and the same seeds: finished %d of %d. Starting %d of %d…",
                        n_pool, st$elite_reps, i, n_pool, st$i_recheck, n_pool)))
        } else {
          bundle <- sc_opt_bundle(st)
          bundle$meta$source <- "live"
          result(bundle); draft(bundle)
          job(NULL); running(FALSE)
          status(list(kind = "done", pct = 100,
                      text = sprintf(
                        "Done. Search of %d generations and a recheck of %d setups (%d replications, seed %s) in %.1f s.",
                        bundle$meta$n_gen, bundle$meta$n_recheck,
                        bundle$meta$elite_reps, bundle$meta$base_seed, bundle$meta$secs)))
        }
      } else {
        running(FALSE)
      }
    })
  })

  output$opt_status <- renderUI(sc_opt_status_ui(status()))

  output$opt_story <- renderUI({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$meta),
                  "Run a search, or restore the preloaded result, to fill this summary."))
    p(class = "mb-0", sc_opt_story(r))
  })

  output$opt_caption <- renderUI({
    r <- draft()
    if (is.null(r) || is.null(r$history) || !nrow(r$history)) return(NULL)
    p(class = "small text-muted mb-0", sc_opt_caption(r))
  })

  output$opt_plot <- renderPlot({
    r <- draft()
    validate(need(!is.null(r) && !is.null(r$history) && nrow(r$history),
                  "Run a search to draw the chart."))
    w <- session$clientData$output_opt_plot_width
    sc_plot_opt_history(r, narrow = isTRUE(w < 700))
  }, res = 96)

  output$opt_table <- gt::render_gt({
    r <- result()
    validate(need(!is.null(r) && !is.null(r$recheck) && nrow(r$recheck),
                  "Run a search to fill this table. The top candidates appear after the recheck."))
    w <- session$clientData$output_opt_plot_width
    sc_table_opt(r, narrow = isTRUE(w < 700))
  })
}
