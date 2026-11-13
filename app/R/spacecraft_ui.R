# spacecraft_ui.R
# Page UIs (the Settings page is in spacecraft_settings_*.R). The Simulate sidebar follows the automotive app's style: a 320px
# bslib sidebar with collapsible accordion panels and a full-width Run button.

# Sidebar sections come from sc_line_panels / sc_line_fields (spacecraft_inputs.R),
# the same table that builds Settings > Line settings.
sc_sidebar_accordion <- function() {
  notes <- list(
    run = actionButton("run", tagList(icon("play"), " Run simulation"), class = "btn-primary w-100 mb-3"),
    arrivals = helpText("Variant mix, touch times and due dates are on Settings > Variants."),
    chamber = helpText("Units closest to their due date go first."),
    mrb = helpText("Enter Accepted and Scrap; Rework is the rest, so the three add up to 1.")
  )
  panels <- purrr::pmap(sc_line_panels, function(panel, title, icon) {
    body <- sc_panel_inputs(panel)
    body <- if (panel == "run") c(list(notes$run), body) else c(body, list(notes[[panel]]))
    rlang::exec(accordion_panel, title = tagList(bs_icon(icon), " ", title), value = panel, !!!purrr::compact(body))
  })
  # Start with every panel closed, including Run simulation, so the first look
  # is the results rather than the run controls.
  rlang::exec(accordion, id = "sc_controls", open = FALSE, multiple = TRUE, !!!panels)
}

sc_simulate_ui <- function() {
  nav_panel(
    "Simulate",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        id = "sc_sidebar", title = "Line parameters", width = 320, open = "desktop",
        sc_sidebar_accordion()
      ),
      uiOutput("status"),
      layout_columns(
        fill = FALSE,
        value_box("Mean cycle time", textOutput("kpi_cycle"), showcase = bs_icon("clock")),
        value_box("On time", textOutput("kpi_ontime"), showcase = bs_icon("calendar-check")),
        value_box("Average WIP", textOutput("kpi_wip"), showcase = bs_icon("stack")),
        value_box("Chamber utilization", textOutput("kpi_chamber"), showcase = bs_icon("thermometer-half")),
        value_box("MRB queue time", textOutput("kpi_mrb"), showcase = bs_icon("hourglass-split")),
        value_box("Scrap rate", textOutput("kpi_scrap"), showcase = bs_icon("trash"))
      ),
      # Cycle Times, Schedule Slip, WIP and Summary — the old KPIs-by-replication
      # table was dropped, so there is no empty KPI tab.
      navset_card_tab(
        id = "simulate_result_tabs",
        nav_panel("Cycle Times", gt::gt_output("table_cycle")),
        nav_panel("Schedule Slip", gt::gt_output("table_lateness")),
        nav_panel("WIP", plotOutput("plot_wip", height = 480)),
        nav_panel("Summary", tableOutput("summary_table"))
      )
    )
  )
}

# sc_optimize_ui() lives in spacecraft_optimize_page.R.

sc_about_note <- function(icon, lead, ...) {
  tags$li(
    class = "mb-3 d-flex align-items-start gap-2",
    span(bs_icon(icon), class = "text-primary mt-1", style = "flex: 0 0 auto;"),
    span(tags$strong(lead), " ", ...)
  )
}

sc_about_ui <- function() {
  nav_panel(
    "About",
    card(card_body(
      h4("Spacecraft line (illustrative)"),
      p("A low-volume, high-mix line with six stations: kitting, integration, functional test,",
        "environmental test (vibe and TVAC share one chamber), MRB and final acceptance.",
        "Every rework loop returns to Integration. After the fail cap a unit is forced to MRB."),
      p(class = "mb-3", "All inputs are example values, not a calibrated program. See docs/SPACECRAFT.md."),
      tags$ul(class = "list-unstyled mb-0",
        sc_about_note("play-fill", "Simulate.",
          "Run the current line and read cycle time, schedule slip, WIP and a summary of the run."),
        sc_about_note("sliders", "Settings.",
          "Change the line and the three variants. The star-map shows how a unit moves through the stations."),
        sc_about_note("layout-three-columns", "Compare.",
          "Places the baseline next to up to three what-if scenarios, using the same seeds,",
          "and ships with a preloaded set so the first look is instant."),
        sc_about_note("search", "Analyze.",
          "Explains why the line performs as it does: a time breakdown of cycle time,",
          "the bottleneck station, and queue length over the year, with a short plain-English summary.",
          "It opens on a preloaded baseline (the same default line as Compare) and can also analyze the current Settings or the last Simulate run."),
        sc_about_note("bar-chart-steps", "Tornado.",
          "Moves each key input one at a time to a low and a high setting",
          "(times and rates by 20%, station counts by one) and shows the swing in a chosen output:",
          "mean cycle time, on-time %, throughput, or mean schedule slip. The dashed line is the baseline.",
          "It opens on a preloaded tornado (20 replications, seed 42, the same default line as Compare and Analyze) and can also run a live tornado from the current Settings."),
        sc_about_note("graph-up", "Breakpoint.",
          "Raises the release rate from the current Settings and finds the first demand",
          "where mean on-time falls below a target (80% by default, which you can change).",
          "It plots on-time % and cycle time against offered demand, marks the target and the breakpoint,",
          "and shows which station saturates first. A preloaded sweep (20 replications, seed 42,",
          "the same 179.2 h baseline as Compare) opens at once; you can also run a live sweep."),
        sc_about_note("gear", "Optimize.",
          "Searches station counts around the current Settings to raise on-time % or cut cycle time, charging a cost for each extra station so the trade-off stays honest. The chart and table use a net on-time score (higher is better) or a net cycle time (lower is better), plus a paired change versus the current line. It opens on a preloaded Quick search (seed 42) and can also run a live search with a progress bar and Cancel. After the search, the current line, the top candidates, and each one-station change are re-checked with more replications and the same seeds."),
        sc_about_note("hourglass-split", "Warmup.",
          "Checks how the empty start biases the first units. It plots a moving-window (Welch) smooth of cycle time by release order and of WIP by week, with a P10–P90 ribbon and a dashed line at a suggested warm-up, and it shows how on-time and cycle time change if those early units are discarded. The Warm-up to discard setting is 0 weeks by default, so the other pages keep their current numbers unless you opt in. It opens on a preloaded check (20 replications, seed 42) and can also run a live check with a progress bar and Cancel.")
      )
    ))
  )
}
