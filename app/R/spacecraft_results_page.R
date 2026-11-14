# spacecraft_results_page.R
# Results dropdown (navbar menu), modelled on the automotive app: Bottleneck &
# capacity, Cycle time, WIP & flow, Labor, and Summary (including downloads).
# All pages read the last Simulate run (the same eventReactive as the value boxes).

sc_results_context_ui <- function(id) {
  uiOutput(id)
}

sc_results_context_html <- function(r) {
  if (is.null(r) || is.null(r$summary)) {
    return(div(class = "alert alert-light py-2",
               "Run a simulation on the Simulate page to fill these charts."))
  }
  seed <- r$params$seed %||% r$analyze$meta$base_seed %||% "—"
  div(class = "alert alert-light py-2",
      sprintf("Showing the last Simulate run: %d replications in %.1f s, seed %s. %d units released per replication on average.",
              nrow(r$per_rep), r$secs %||% 0, seed, round(r$summary$arrivals)))
}

sc_results_bottleneck_ui <- function() {
  nav_panel(
    title = "Bottleneck & capacity",
    value = "results_capacity",
    sc_results_context_ui("results_context_capacity"),
    p(class = "text-muted small mb-2",
      "The bottleneck is the station with the highest utilization (including time held for repair).",
      "These charts show whether waits, queues, labor, or downtime explain it."),
    navset_card_tab(
      id = "results_capacity_tabs",
      nav_panel(
        "Utilization",
        plotOutput("results_util_plot", height = 340),
        div(class = "sc-analyze-table-wrap", gt::gt_output("results_util_table")),
        p(class = "small text-muted mb-0",
          "Station, chamber and technician utilization. The highlighted row is the busiest station.",
          "Utilization includes units held in place during a repair.")
      ),
      nav_panel(
        "Why stations wait",
        plotOutput("results_wait_plot", height = 560),
        p(class = "small text-muted mb-0",
          "Mean hours per run, with 95% confidence intervals and one faint point per replication.",
          "Blocked is time units spend waiting to enter the station.",
          "Waiting for technicians is time holding the station for the shared labour pool.",
          "Down for repair is equipment repair, including any wait for a maintainer.",
          "Starved of upstream is idle server-hours with an empty queue.")
      ),
      nav_panel(
        "Queue length",
        plotOutput("results_queue_plot", height = 440),
        p(class = "small text-muted mb-0",
          "The line is the mean queue across replications; the ribbon runs from the 10th to the 90th percentile.")
      ),
      nav_panel(
        "Downtime",
        plotOutput("results_down_plot", height = 400),
        div(class = "sc-analyze-table-wrap", gt::gt_output("results_down_table")),
        p(class = "small text-muted mb-0",
          "Downtime fraction is repair hours divided by capacity times the horizon (calendar time).",
          "Operational availability is processing time divided by processing plus repair.")
      )
    )
  )
}

sc_results_cycle_ui <- function() {
  nav_panel(
    title = "Cycle time",
    value = "results_cycle",
    sc_results_context_ui("results_context_cycle"),
    navset_card_tab(
      id = "results_cycle_tabs",
      nav_panel(
        "Distribution",
        plotOutput("results_cycle_dist", height = 420),
        div(class = "sc-analyze-table-wrap", gt::gt_output("table_cycle")),
        p(class = "small text-muted mb-0",
          "Density of cycle time (release to ship) by variant. The table is the same summary that used to sit on Simulate.")
      ),
      nav_panel(
        "Per-step breakdown",
        plotOutput("results_cycle_breakdown", height = 340),
        div(class = "sc-analyze-table-wrap sc-analyze-breakdown-wrap",
            gt::gt_output("results_cycle_breakdown_table")),
        p(class = "small text-muted mb-0",
          "Each bar is the share of cycle time for that variant. Hours are in the table.")
      ),
      nav_panel(
        "Over time",
        plotOutput("results_cycle_over_time", height = 440),
        p(class = "small text-muted mb-0",
          "Each point is one shipped unit, placed at its release day. The smooth line is a loess fit by variant.")
      )
    )
  )
}

sc_results_flow_ui <- function() {
  nav_panel(
    title = "WIP & flow",
    value = "results_flow",
    sc_results_context_ui("results_context_flow"),
    navset_card_tab(
      id = "results_flow_tabs",
      nav_panel(
        "WIP",
        plotOutput("plot_wip", height = 480),
        p(class = "small text-muted mt-2 mb-0",
          "Faint lines are individual replications; the bold line is the mean; the dashed line is the time-average of that mean.")
      ),
      nav_panel(
        "Demand vs completions",
        plotOutput("plot_demand", height = 560),
        p(class = "small text-muted mt-2 mb-0",
          "Demand is units released. Completed is units that have shipped or been scrapped.",
          "The unfinished panel is the gap: work still in the line.")
      )
    )
  )
}

sc_results_labor_ui <- function() {
  nav_panel(
    title = "Labor",
    value = "results_labor",
    sc_results_context_ui("results_context_labor"),
    p(class = "text-muted small mb-2",
      "Technicians are a shared pool: every process step seizes the same technician resource after it seizes its station.",
      "The number of technicians required at each step is editable in Settings."),
    navset_card_tab(
      id = "results_labor_tabs",
      nav_panel(
        "Touch labor (HPU)",
        plotOutput("results_hpu_dist", height = 400),
        div(class = "sc-analyze-table-wrap", gt::gt_output("table_hpu")),
        p(class = "small text-muted mb-0",
          "HPU is technicians × touch time, summed over every step including rework, in hours per shipped unit.")
      ),
      nav_panel(
        "Labor utilization",
        plotOutput("results_labor_util", height = 400),
        p(class = "small text-muted mb-0",
          "Busy technician-hours divided by scheduled technician-hours, one bar per replication.",
          "The dashed line is the mean.")
      )
    )
  )
}

sc_results_summary_ui <- function() {
  nav_panel(
    title = "Summary",
    value = "results_summary",
    sc_results_context_ui("results_context_summary"),
    card(
      card_header("Run summary"),
      tableOutput("summary_table"),
      uiOutput("summary_caption")
    ),
    card(
      card_header("Download the tidy tables"),
      uiOutput("simulate_downloads"),
      p(class = "small text-muted mt-2 mb-0",
        "Each CSV is the tidy table behind the last Simulate run, including the seed and replication number.",
        "Download all packs every table into one zip.")
    )
  )
}

sc_results_menu <- function() {
  nav_menu(
    "Results",
    sc_results_bottleneck_ui(),
    sc_results_cycle_ui(),
    sc_results_flow_ui(),
    sc_results_labor_ui(),
    sc_results_summary_ui()
  )
}

# Wire the Results pages into sc_server(). `simulate_result` is the Simulate
# eventReactive (same object as the value boxes).
sc_results_server <- function(input, output, session, simulate_result) {
  for (nm in c("results_context_capacity", "results_context_cycle", "results_context_flow",
               "results_context_labor", "results_context_summary")) {
    local({
      id <- nm
      output[[id]] <- renderUI(sc_results_context_html(simulate_result()))
    })
  }

  analyze_of <- function() {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$analyze),
                  "Run a simulation on the Simulate page to fill these charts."))
    r$analyze
  }

  output$results_util_plot <- renderPlot({
    a <- analyze_of()
    w <- session$clientData$output_results_util_plot_width
    sc_plot_analyze_util(a$resources, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_util_table <- gt::render_gt({
    a <- analyze_of()
    w <- session$clientData$output_results_util_plot_width
    sc_table_analyze_util(a$resources, narrow = isTRUE(w < 700))
  })
  output$results_wait_plot <- renderPlot({
    a <- analyze_of()
    validate(need(!is.null(a$waits) && nrow(a$waits), "No wait-reason data in this run."))
    w <- session$clientData$output_results_wait_plot_width
    sc_plot_wait_reasons(a$waits, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_queue_plot <- renderPlot({
    a <- analyze_of()
    w <- session$clientData$output_results_queue_plot_width
    sc_plot_analyze_queues(a$queues, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_down_plot <- renderPlot({
    a <- analyze_of()
    validate(need(!is.null(a$downtime) && nrow(a$downtime), "No downtime data in this run."))
    w <- session$clientData$output_results_down_plot_width
    sc_plot_downtime(a$downtime, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_down_table <- gt::render_gt({
    a <- analyze_of()
    w <- session$clientData$output_results_down_plot_width
    sc_table_analyze_util(a$resources, narrow = isTRUE(w < 700))
  })

  output$results_cycle_dist <- renderPlot({
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    sc_plot_cycle_dist(r$units, r$params)
  }, res = 96)
  output$results_cycle_breakdown <- renderPlot({
    a <- analyze_of()
    w <- session$clientData$output_results_cycle_breakdown_width
    sc_plot_analyze_breakdown(a$breakdown, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_cycle_breakdown_table <- gt::render_gt({
    a <- analyze_of()
    sc_table_analyze_breakdown(a$breakdown)
  })
  output$results_cycle_over_time <- renderPlot({
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    sc_plot_cycle_over_time(r$units, r$params)
  }, res = 96)

  output$results_hpu_dist <- renderPlot({
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    sc_plot_hpu_dist(r$units, r$params)
  }, res = 96)
  output$results_labor_util <- renderPlot({
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$per_rep), "Run a simulation to draw this plot."))
    sc_plot_labor_util(r$per_rep)
  }, res = 96)
}
