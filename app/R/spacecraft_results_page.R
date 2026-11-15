# spacecraft_results_page.R
# Results dropdown (navbar menu): Bottleneck, Cycle time, By variant, Quality,
# Reliability, and Summary (throughput, WIP, labor, and downloads).
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
    title = "Bottleneck",
    value = "results_capacity",
    sc_results_context_ui("results_context_capacity"),
    p(class = "text-muted small mb-2",
      "The bottleneck is the station with the highest utilization (including time held for repair), called out in orange on the utilization chart.",
      "The first chart shows where a unit's cycle time goes; utilization and downtime follow as a mean with a 10–90 bar, busiest resource on top."),
    card(
      card_header("Where the time goes"),
      plotOutput("results_capacity_breakdown", height = 380),
      uiOutput("results_capacity_story")
    ),
    navset_card_tab(
      id = "results_capacity_tabs",
      nav_panel(
        "Utilization",
        plotOutput("results_util_plot", height = 440),
        div(class = "sc-analyze-table-wrap", gt::gt_output("results_util_table")),
        p(class = "small text-muted mb-0",
          "Points are the mean utilization across replications; bars run from the 10th to the 90th percentile.",
          "Resources are sorted so the busiest is on top, including the technician pool, the maintenance crew, and the QA inspectors.",
          "The highlighted row in the table is the bottleneck station.",
          "Utilization includes units held in place during a repair.")
      ),
      nav_panel(
        "Downtime",
        plotOutput("results_down_plot", height = 420),
        div(class = "sc-analyze-table-wrap", gt::gt_output("results_down_table")),
        p(class = "small text-muted mb-0",
          "Points are the mean calendar downtime fraction; bars run from the 10th to the 90th percentile, busiest on top.",
          "Downtime fraction is repair hours divided by capacity times the horizon (calendar time).",
          "Operational availability is processing time divided by processing plus repair.")
      ),
      nav_panel(
        "Queue length",
        plotOutput("results_queue_plot", height = 440),
        p(class = "small text-muted mb-0",
          "The line is the mean queue across replications; the ribbon runs from the 10th to the 90th percentile.")
      ),
      nav_panel(
        "Why stations wait",
        plotOutput("results_wait_plot", height = 560),
        p(class = "small text-muted mb-0",
          "The left panel is blocked, waiting for technicians and down for repair, dodged on a shared scale so those bars stay readable.",
          "The right panel is starved of upstream (idle server-hours with an empty queue) on its own scale.",
          "Bars are the mean hours per run at each station; thin error bars run from the 10th to the 90th percentile.",
          "Blocked is time units spend waiting to enter the station.",
          "Waiting for technicians is time holding the station for the shared labour pool.",
          "Down for repair is equipment repair, including any wait for a maintainer.",
          "QA inspectors appear as their own row: blocked is time units wait for an inspector, and starved is idle inspector hours.")
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
        "All shipped units",
        plotOutput("results_cycle_hist", height = 440),
        div(class = "sc-analyze-table-wrap", gt::gt_output("table_cycle")),
        p(class = "small text-muted mb-0",
          "Count histogram of every shipped unit. The dashed line is the mean.",
          "The table is the cycle-time summary by variant.")
      ),
      nav_panel(
        "Per-step breakdown",
        plotOutput("results_cycle_breakdown", height = 380),
        div(class = "sc-analyze-table-wrap sc-analyze-breakdown-wrap",
            gt::gt_output("results_cycle_breakdown_table")),
        p(class = "small text-muted mb-0",
          "Each bar is the share of cycle time for that variant. Hours are in the table.")
      ),
      nav_panel(
        "By variant",
        plotOutput("results_cycle_dist", height = 420),
        p(class = "small text-muted mb-0",
          "Overlapping count histogram of cycle time (release to ship) by variant.")
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
        p(class = "fw-semibold mt-3 mb-1", "Demand vs completions"),
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
          "Overlapping count histogram of touch labor by variant. HPU is technicians × touch time, summed over every step including rework, in hours per shipped unit.")
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

sc_results_quality_ui <- function() {
  nav_panel(
    title = "Quality",
    value = "results_quality",
    sc_results_context_ui("results_context_quality"),
    p(class = "text-muted small mb-2",
      "Fails, rework and scrap are counted by where they originated: functional test, environmental test, or the material review board disposition.",
      "Rates are per unit released. Points are the mean across replications; bars run from the 10th to the 90th percentile."),
    navset_card_tab(
      id = "results_quality_tabs",
      nav_panel(
        "Rework",
        plotOutput("results_quality_rework", height = 320),
        p(class = "small text-muted mb-0",
          "Functional-test rework is a rollback to Integration while the unit is still under the fail cap.",
          "Environmental-test failures go to the review board rather than rolling back immediately.",
          "MRB rework is the review-board disposition that sends a unit back to Integration.")
      ),
      nav_panel(
        "Scrap",
        plotOutput("results_quality_scrap", height = 280),
        p(class = "small text-muted mb-0",
          "Scrap is an MRB disposition. Functional and environmental failures that are not scrapped either rework or go through as use-as-is.")
      ),
      nav_panel(
        "Fails",
        plotOutput("results_quality_fail", height = 320),
        p(class = "small text-muted mb-0",
          "A functional or environmental test failure increments the fail count. The review board itself does not generate a new test failure.")
      )
    ),
    div(class = "sc-analyze-table-wrap mt-3", gt::gt_output("results_quality_table"))
  )
}

sc_results_reliability_ui <- function() {
  nav_panel(
    title = "Reliability",
    value = "results_reliability",
    sc_results_context_ui("results_context_reliability"),
    p(class = "text-muted small mb-2",
      "Observed MTBF and MTTR are compared with the configured Settings values.",
      "MTBF is operating hours per failure (the clock only advances while a unit is being worked on at that station).",
      "MTTR is repair hours per repair. Blue is the observed mean with a 10–90 bar; the orange diamond is the configured value."),
    navset_card_tab(
      id = "results_reliability_tabs",
      nav_panel(
        "MTBF",
        plotOutput("results_reliability_mtbf", height = 420)
      ),
      nav_panel(
        "MTTR",
        plotOutput("results_reliability_mttr", height = 420)
      )
    ),
    div(class = "sc-analyze-table-wrap mt-3", gt::gt_output("results_reliability_table"))
  )
}


sc_results_variant_ui <- function() {
  nav_panel(
    title = "By variant",
    value = "results_variant",
    sc_results_context_ui("results_context_variant"),
    p(class = "text-muted small mb-2",
      "Cycle time and touch labor for every shipped unit, split by variant, from the last Simulate run.",
      "The histograms overlap (they are not dodged or stacked) so you can compare the shapes."),
    navset_card_tab(
      id = "results_variant_tabs",
      nav_panel(
        "Cycle time",
        plotOutput("results_variant_cycle", height = 420),
        p(class = "small text-muted mb-0",
          "Overlapping count histogram of cycle time for shipped units, one fill per variant.",
          "Bars share one set of bins.")
      ),
      nav_panel(
        "Touch labor",
        plotOutput("results_variant_hpu", height = 420),
        p(class = "small text-muted mb-0",
          "Overlapping count histogram of touch labor (HPU) for shipped units, one fill per variant.",
          "HPU is technicians × touch time, summed over every step including rework.")
      )
    )
  )
}

sc_results_summary_ui <- function() {
  nav_panel(
    title = "Summary",
    value = "results_summary",
    sc_results_context_ui("results_context_summary"),
    navset_card_tab(
      id = "results_summary_tabs",
      nav_panel(
        "Throughput",
        plotOutput("results_throughput_bars", height = 400),
        p(class = "small text-muted mb-0",
          "The bar is mean throughput (units shipped in a replication) across the last Simulate run.",
          "The thin whisker runs from the 10th to the 90th percentile. There is no rug and no point per replication.")
      ),
      nav_panel(
        "Run summary",
        tableOutput("summary_table"),
        uiOutput("summary_caption")
      ),
      nav_panel(
        "WIP & flow",
        plotOutput("plot_wip", height = 480),
        p(class = "small text-muted mt-2 mb-0",
          "Faint lines are individual replications; the bold line is the mean; the dashed line is the time-average of that mean."),
        p(class = "fw-semibold mt-3 mb-1", "Demand vs completions"),
        plotOutput("plot_demand", height = 560),
        p(class = "small text-muted mt-2 mb-0",
          "Demand is units released. Completed is units that have shipped or been scrapped.",
          "The unfinished panel is the gap: work still in the line.")
      ),
      nav_panel(
        "Labor",
        plotOutput("results_hpu_dist", height = 400),
        div(class = "sc-analyze-table-wrap", gt::gt_output("table_hpu")),
        p(class = "small text-muted mb-0",
          "HPU is technicians × touch time, summed over every step including rework, in hours per shipped unit."),
        plotOutput("results_labor_util", height = 400),
        p(class = "small text-muted mt-2 mb-0",
          "Busy technician-hours divided by scheduled technician-hours, one bar per replication.",
          "The dashed line is the mean.")
      ),
      nav_panel(
        "Downloads",
        uiOutput("simulate_downloads"),
        p(class = "small text-muted mt-2 mb-0",
          "Each CSV is the tidy table behind the last Simulate run, including the seed and replication number.",
          "Download all packs every table into one zip.")
      )
    )
  )
}

sc_results_menu <- function() {
  nav_menu(
    "Results",
    sc_results_bottleneck_ui(),
    sc_results_cycle_ui(),
    sc_results_variant_ui(),
    sc_results_quality_ui(),
    sc_results_reliability_ui(),
    sc_results_summary_ui()
  )
}

# Wire the Results pages into sc_server(). `simulate_result` is the Simulate
# eventReactive (same object as the value boxes).
sc_results_server <- function(input, output, session, simulate_result) {
  for (nm in c("results_context_capacity", "results_context_cycle",
               "results_context_variant",
               "results_context_quality", "results_context_reliability",
               "results_context_summary")) {
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

  output$results_capacity_breakdown <- sc_render_plot("results_capacity_breakdown", session, {
    a <- analyze_of()
    w <- session$clientData$output_results_capacity_breakdown_width
    sc_plot_analyze_breakdown(a$breakdown, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_capacity_story <- renderUI({
    a <- analyze_of()
    if (!nzchar(a$story %||% "")) return(NULL)
    p(class = "small text-muted mb-0", a$story)
  })
  output$results_util_plot <- sc_render_plot("results_util_plot", session, {
    a <- analyze_of()
    use <- a$util_rep
    if (is.null(use) || !nrow(use)) {
      use <- a$resources %>% dplyr::mutate(rep = 1L)
    }
    bot <- a$bottleneck$resource %||% character()
    sc_plot_resource_spread(
      use, "util", "Utilization", percent = TRUE,
      highlight = bot, highlight_name = "Bottleneck",
      caption = "Points are the mean across replications; bars run from the 10th to the 90th percentile. The busiest resource is on top. The orange bar is the bottleneck station."
    )
  }, res = 96)
  output$results_util_table <- gt::render_gt({
    a <- analyze_of()
    w <- session$clientData$output_results_util_plot_width
    sc_table_analyze_util(a$resources, narrow = isTRUE(w < 700))
  })
  output$results_wait_plot <- sc_render_plot("results_wait_plot", session, {
    a <- analyze_of()
    validate(need(!is.null(a$waits) && nrow(a$waits), "No wait-reason data in this run."))
    w <- session$clientData$output_results_wait_plot_width
    sc_plot_wait_reasons(a$waits, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_queue_plot <- sc_render_plot("results_queue_plot", session, {
    a <- analyze_of()
    w <- session$clientData$output_results_queue_plot_width
    sc_plot_analyze_queues(a$queues, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_down_plot <- sc_render_plot("results_down_plot", session, {
    a <- analyze_of()
    validate(need(!is.null(a$downtime) && nrow(a$downtime), "No downtime data in this run."))
    sc_plot_resource_spread(
      a$downtime, "downtime_fraction", "Calendar downtime fraction", percent = TRUE,
      caption = "Points are the mean across replications; bars run from the 10th to the 90th percentile. The highest downtime is on top."
    )
  }, res = 96)
  output$results_down_table <- gt::render_gt({
    a <- analyze_of()
    w <- session$clientData$output_results_down_plot_width
    sc_table_analyze_util(a$resources, narrow = isTRUE(w < 700))
  })

  output$results_cycle_hist <- sc_render_plot("results_cycle_hist", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    sc_plot_cycle_hist_reps(r$units)
  }, res = 96)
  output$results_cycle_dist <- sc_render_plot("results_cycle_dist", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    sc_plot_cycle_dist(r$units, r$params)
  }, res = 96)
  output$results_cycle_breakdown <- sc_render_plot("results_cycle_breakdown", session, {
    a <- analyze_of()
    w <- session$clientData$output_results_cycle_breakdown_width
    sc_plot_analyze_breakdown(a$breakdown, narrow = isTRUE(w < 700))
  }, res = 96)
  output$results_cycle_breakdown_table <- gt::render_gt({
    a <- analyze_of()
    sc_table_analyze_breakdown(a$breakdown)
  })
  output$results_cycle_over_time <- sc_render_plot("results_cycle_over_time", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    sc_plot_cycle_over_time(r$units, r$params)
  }, res = 96)

  output$results_variant_cycle <- sc_render_plot("results_variant_cycle", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    d <- r$units %>%
      dplyr::filter(.data$status == "shipped", is.finite(.data$cycle_h)) %>%
      dplyr::mutate(variant = sc_variant_factor(.data$variant, r$params))
    validate(need(nrow(d) > 0, "No shipped units to plot."))
    sc_plot_overlap_hist(d, x = "cycle_h", group = "variant",
                         xlab = "Cycle time (h)", pal = sc_variant_colors)
  }, res = 96)
  output$results_variant_hpu <- sc_render_plot("results_variant_hpu", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units) && "labor_hpu" %in% names(r$units),
                  "Run a simulation to draw this plot."))
    d <- r$units %>%
      dplyr::filter(.data$status == "shipped", is.finite(.data$labor_hpu)) %>%
      dplyr::mutate(variant = sc_variant_factor(.data$variant, r$params))
    validate(need(nrow(d) > 0, "No shipped units to plot."))
    sc_plot_overlap_hist(d, x = "labor_hpu", group = "variant",
                         xlab = "Touch labor (h per unit)", pal = sc_variant_colors)
  }, res = 96)
  output$results_throughput_bars <- sc_render_plot("results_throughput_bars", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$per_rep) && "shipped" %in% names(r$per_rep),
                  "Run a simulation to draw this plot."))
    summ <- sc_throughput_bar_data(r$per_rep, label = "This run")
    validate(need(nrow(summ) > 0, "No throughput data to plot."))
    sc_plot_throughput_bars(summ)
  }, res = 96)
  output$results_hpu_dist <- sc_render_plot("results_hpu_dist", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to draw this plot."))
    sc_plot_hpu_dist(r$units, r$params)
  }, res = 96)
  output$results_labor_util <- sc_render_plot("results_labor_util", session, {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$per_rep), "Run a simulation to draw this plot."))
    sc_plot_labor_util(r$per_rep)
  }, res = 96)

  quality_of <- function() {
    r <- simulate_result()
    validate(need(!is.null(r) && !is.null(r$units), "Run a simulation to fill these charts."))
    sc_quality_by_rep(r$units)
  }
  output$results_quality_rework <- sc_render_plot("results_quality_rework", session, {
    sc_plot_quality_rate(
      quality_of(), "rework_rate", "Rework rate",
      caption = "Rework events per unit released, by origin. Points are the mean across replications; bars run from the 10th to the 90th percentile."
    )
  }, res = 96)
  output$results_quality_scrap <- sc_render_plot("results_quality_scrap", session, {
    sc_plot_quality_rate(
      quality_of(), "scrap_rate", "Scrap rate",
      caption = "Scrap events per unit released. Scrap is an MRB disposition in this model."
    )
  }, res = 96)
  output$results_quality_fail <- sc_render_plot("results_quality_fail", session, {
    sc_plot_quality_rate(
      quality_of(), "fail_rate", "Fail rate",
      caption = "Test failures per unit released, by the station that generated the fail."
    )
  }, res = 96)
  output$results_quality_table <- gt::render_gt({
    sc_table_quality(quality_of())
  })

  reliability_of <- function() {
    a <- analyze_of()
    r <- simulate_result()
    validate(need(!is.null(a$downtime) && nrow(a$downtime), "No reliability data in this run."))
    sc_reliability_observed(a$downtime, r$params)
  }
  output$results_reliability_mtbf <- sc_render_plot("results_reliability_mtbf", session, {
    sc_plot_reliability_obs(reliability_of(), "obs_mtbf", "cfg_mtbf", "MTBF (operating hours)")
  }, res = 96)
  output$results_reliability_mttr <- sc_render_plot("results_reliability_mttr", session, {
    sc_plot_reliability_obs(reliability_of(), "obs_mttr", "cfg_mttr", "MTTR (repair hours)")
  }, res = 96)
  output$results_reliability_table <- gt::render_gt({
    sc_table_reliability_obs(reliability_of())
  })

}
