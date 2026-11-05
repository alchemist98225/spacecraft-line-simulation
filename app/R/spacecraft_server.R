# spacecraft_server.R
# Server: build params from the sidebar + Variants table, run replications,
# render KPIs and plots, and run the GA on the capacities.

sc_server <- function(input, output, session) {
  variants_long <- reactiveVal(sc_variants_long(sc_variants_default()))
  observeEvent(input$variants_reset, variants_long(sc_variants_long(sc_variants_default())))
  output$variants_table <- DT::renderDT(
    DT::datatable(variants_long(), rownames = FALSE, editable = list(target = "cell", disable = list(columns = 0:2)),
                  options = list(pageLength = 50, dom = "t")),
    server = TRUE
  )
  observeEvent(input$variants_table_cell_edit, {
    e <- input$variants_table_cell_edit
    d <- variants_long()
    d$value[[e$row]] <- as.numeric(e$value)
    variants_long(d)
  })

  current_params <- reactive(
    sc_params_from_inputs(input, sc_variants_from_long(variants_long(), sc_variants_default()))
  )

  result <- eventReactive(input$run, {
    p <- current_params()
    t0 <- Sys.time()
    s <- withProgress(message = "Running replications…", {
      sc_summarise(sc_run(p, n_reps = input$n_reps %||% 5, base_seed = input$seed %||% 42), p)
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
  output$plot_cycle <- renderPlot(sc_plot_cycle(result()$units, result()$params), res = 96)
  output$plot_lateness <- renderPlot(sc_plot_lateness(result()$units, result()$params), res = 96)
  output$plot_wip <- renderPlot(sc_plot_wip(result()$units, result()$params$sim_hours), res = 96)
  output$plot_reps <- renderPlot(sc_plot_kpi_reps(result()$per_rep), res = 96)
  output$summary_table <- renderTable(
    result()$summary %>% tidyr::pivot_longer(dplyr::everything(), names_to = "metric"), digits = 2
  )

  ga <- eventReactive(input$ga_run, {
    p <- current_params()
    withProgress(message = "Optimizing capacities…", value = 0, {
      n_gen <- input$ga_gen
      sc_run_ga(p, pop_size = input$ga_pop, n_gen = n_gen, n_reps = input$ga_reps,
                base_seed = input$seed %||% 42, capacity_cost = input$ga_capacity_cost,
                on_generation = function(row) incProgress(1 / n_gen, detail = sprintf("Generation %d", row$generation)))
    })
  })
  output$ga_status <- renderUI({
    if (!isTruthy(input$ga_run)) return(div(class = "alert alert-light py-2", "Ready. Click Run optimize."))
    g <- ga()
    div(class = "alert alert-success py-2", sprintf("Done. Best fitness %.2f (current %.2f).", g$best$fitness, g$baseline$fitness))
  })
  output$plot_ga <- renderPlot({ req(input$ga_run); sc_plot_ga(ga()$history) }, res = 96)
  output$ga_table <- renderTable({
    req(input$ga_run)
    g <- ga()
    g$defs %>%
      dplyr::transmute(Resource = label, Current = g$baseline$genome[[1]], Best = g$best$genome[[1]]) %>%
      dplyr::bind_rows(tibble::tibble(
        Resource = c("On-time %", "Mean cycle time (h)", "Fitness"),
        Current = c(g$baseline$on_time_pct, g$baseline$cycle_mean_h, g$baseline$fitness),
        Best = c(g$best$on_time_pct, g$best$cycle_mean_h, g$best$fitness)
      ))
  }, digits = 1)
}
