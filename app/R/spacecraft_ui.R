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
  rlang::exec(accordion, id = "sc_controls", open = "run", multiple = TRUE, !!!panels)
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
      card(card_header("Cycle time by variant"), gt::gt_output("table_cycle")),
      card(card_header("Lateness at ship"), gt::gt_output("table_lateness")),
      card(card_header("WIP over time"), plotOutput("plot_wip", height = 480)),
      card(card_header("KPIs by replication"), gt::gt_output("table_reps")),
      card(card_header("Summary table"), tableOutput("summary_table"))
    )
  )
}

# sc_optimize_ui() lives in spacecraft_optimize_page.R.

sc_about_ui <- function() {
  nav_panel(
    "About",
    card(card_body(
      h4("Spacecraft line (illustrative)"),
      p("A low-volume, high-mix line with six stations: kitting, integration, functional test,",
        "environmental test (vibe and TVAC share one chamber), MRB and final acceptance."),
      p("Every rework loop returns to Integration. After the fail cap a unit is forced to MRB."),
      p("The Compare page places the baseline next to up to three what-if scenarios, using the same seeds,",
        "and ships with a preloaded set so the first look is instant."),
      p("The Analyze page explains why the line performs as it does: a time breakdown of cycle time,",
        "the bottleneck station, and queue length over the year, with a short plain-English summary.",
        "It opens on a preloaded baseline (the same default line as Compare) and can also analyze the current Settings or the last Simulate run."),
      p("The Tornado page moves each key input one at a time to a low and a high setting",
        "(times and rates by 20%, station counts by one) and shows the swing in a chosen output:",
        "mean cycle time, on-time %, throughput, or mean lateness. The dashed line is the baseline.",
        "It opens on a preloaded tornado (20 replications, seed 42, the same default line as Compare and Analyze) and can also run a live tornado from the current Settings."),
      p("The Breakpoint page raises the release rate from the current Settings and finds the first demand",
        "where mean on-time falls below a target (80% by default, which you can change).",
        "It plots on-time % and cycle time against offered demand, marks the target and the breakpoint,",
        "and shows which station saturates first. A preloaded sweep (20 replications, seed 42,",
        "the same 179.2 h baseline as Compare) opens at once; you can also run a live sweep."),
      p("The Optimize page searches station counts around the current Settings to raise on-time % or cut cycle time, charging a cost for each extra station so the trade-off stays honest. The chart and table use a net on-time score (higher is better) or a net cycle time (lower is better), plus a paired change versus the current line. It opens on a preloaded Quick search (seed 42) and can also run a live search with a progress bar and Cancel. After the search, the current line, the top candidates, and each one-station change are re-checked with more replications and the same seeds."),
      p("All inputs are example values, not a calibrated program. See docs/SPACECRAFT.md.")
    ))
  )
}
