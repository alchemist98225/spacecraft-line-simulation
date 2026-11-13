# spacecraft_server.R
# Server: build params from the sidebar + Settings page, run replications,
# render KPIs and plots, and wire Compare, Analyze, Tornado, Breakpoint, Optimize and Warmup.

sc_server <- function(input, output, session, update_fn = sc_update_input) {
  variants_tab <- sc_settings_variants_server(input, output, session)
  line_tab <- sc_settings_line_server(input, output, session, update_fn)

  current_params <- reactive(
    sc_params_from_inputs(input, variants_tab$variants())
  )

  sc_compare_server(input, output, session, current_params)


  output$horizon_hours_text <- renderText(
    sc_horizon_label(sc_horizon_hours(input$horizon_quarters, input$horizon_weeks, input$horizon_days))
  )

  output$mrb_rework <- renderText(sc_rework_label(input$mrb_p_use_as_is, input$mrb_p_scrap))

  result <- eventReactive(input$run, {
    p <- current_params()
    validate(need(is.null(sc_mrb_problem(input$mrb_p_use_as_is, input$mrb_p_scrap)),
                  paste("Material review board:", sc_mrb_problem(input$mrb_p_use_as_is, input$mrb_p_scrap))))
    validate(need(p$sim_hours > 0, "The horizon is 0 hours. Set at least one quarter, week or day, then run again."))
    t0 <- Sys.time()
    s <- withProgress(message = "Running replications…", {
      runs <- sc_run(p, n_reps = input$n_reps %||% 5, base_seed = input$seed %||% 42)
      out <- sc_summarise(runs, p)
      out$analyze <- sc_analyze(runs, p)
      out$analyze$meta$base_seed <- input$seed %||% 42
      out
    })
    c(s, list(params = p, secs = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }, ignoreNULL = FALSE)

  output$status <- renderUI({
    r <- result()
    div(class = "alert alert-light py-2", sprintf("%d replications in %.1f s. %d units released per replication on average.",
                                                  nrow(r$per_rep), r$secs, round(r$summary$arrivals)))
  })
  k <- function(f) { f <- rlang::as_function(f); renderText(f(result()$summary)) }
  output$kpi_cycle <- k(~ sprintf("%.0f h", .x$cycle_mean_h))
  output$kpi_ontime <- k(~ sprintf("%.0f%%", .x$on_time_pct))
  output$kpi_wip <- k(~ sprintf("%.1f units", .x$wip_avg))
  output$kpi_chamber <- k(~ sprintf("%.0f%%", 100 * .x$chamber_util))
  output$kpi_mrb <- k(~ sprintf("%.1f h", .x$mrb_q_mean_h))
  output$kpi_scrap <- k(~ sprintf("%.1f%%", 100 * .x$scrap_rate))
  output$table_cycle <- gt::render_gt(sc_table_cycle(result()$units, result()$params))
  output$table_lateness <- gt::render_gt(sc_table_lateness(result()$units, result()$params))
  output$plot_wip <- renderPlot(sc_plot_wip(result()$units, result()$params$sim_hours), res = 96)
  output$summary_table <- renderTable(sc_format_summary_table(result()$summary), digits = 2)

  sc_analyze_server(input, output, session, current_params, simulate_result = result)
  sc_tornado_server(input, output, session, current_params)
  sc_breakpoint_server(input, output, session, current_params)
  sc_optimize_server(input, output, session, current_params)
  sc_warmup_server(input, output, session, current_params)

  # Handles for tests (Shiny ignores a server's return value).
  invisible(list(line_tab = line_tab, variants_tab = variants_tab, current_params = current_params))
}
