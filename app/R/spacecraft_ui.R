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
      sc_kpi_cards(),
      card(card_header("Cycle time by variant"), plotOutput("plot_cycle", height = 400)),
      card(card_header("Lateness at ship"), plotOutput("plot_lateness", height = 400)),
      card(card_header("WIP over time"), plotOutput("plot_wip", height = 380)),
      card(card_header("KPIs by replication"), plotOutput("plot_reps", height = 420)),
      card(card_header("Summary table"), tableOutput("summary_table"))
    )
  )
}

sc_optimize_ui <- function() {
  nav_panel(
    "Optimize",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        title = "Optimizer settings", width = 320,
        actionButton("ga_run", tagList(icon("play"), " Run optimize"), class = "btn-primary w-100 mb-3"),
        numericInput("ga_pop", "Population", 8, min = 4, max = 40, step = 1),
        numericInput("ga_gen", "Generations", 5, min = 2, max = 30, step = 1),
        numericInput("ga_reps", "Replications per candidate", 3, min = 1, max = 20, step = 1),
        numericInput("ga_capacity_cost", "Cost per extra resource (on-time points)", 2, min = 0, step = 0.5),
        helpText("Searches the capacities around the current Simulate settings.",
                 "Fitness = (100 - on-time %) + cost x extra resources. Lower is better.")
      ),
      uiOutput("ga_status"),
      card(card_header("Convergence"), plotOutput("plot_ga", height = 440)),
      card(card_header("Best vs current"), tableOutput("ga_table"))
    )
  )
}

sc_about_ui <- function() {
  nav_panel(
    "About",
    card(card_body(
      h4("Spacecraft line (illustrative)"),
      p("A low-volume, high-mix line with six stations: kitting, integration, functional test,",
        "environmental test (vibe and TVAC share one chamber), MRB and final acceptance."),
      p("Every rework loop returns to Integration. After the fail cap a unit is forced to MRB."),
      p("All inputs are example values, not a calibrated program. See docs/SPACECRAFT.md.")
    ))
  )
}

# KPI cards: big number, short subtitle, and a sparkline of the KPI over
# simulated time (mean across replications, band = lowest to highest).
sc_kpi_defs <- function() tibble::tribble(
  ~id,       ~title,               ~icon,              ~sub,                                   ~tip,
  "cycle",   "Mean cycle time",    "clock",            "Release to ship, units shipped",       "Average hours from release into the line to shipping, over shipped units. Sparkline: average for units shipped in each period.",
  "ontime",  "On time",            "calendar-check",   "Shipped by their due date",            "Share of shipped units that left on or before their due date. Sparkline: share for units shipped in each period.",
  "wip",     "Average WIP",        "stack",            "Units in the line at once",            "Time-average number of units released but not yet shipped or scrapped. Sparkline: average in each period.",
  "chamber", "Chamber utilization","thermometer-half", "Time the chamber is busy",             "Share of time the environmental test chamber is in use. Sparkline: share in each period.",
  "mrb",     "MRB queue time",     "hourglass-split",  "Wait before the review board",         "Average hours a unit waits for the material review board, per visit. Sparkline: average for visits ending in each period.",
  "scrap",   "Scrap rate",         "trash",            "Of units finished, share scrapped",    "Scrapped units as a share of all units that finished (shipped or scrapped). Sparkline: running total to date."
)

sc_kpi_cards <- function() {
  boxes <- purrr::pmap(sc_kpi_defs(), function(id, title, icon, sub, tip) {
    value_box(
      title = tooltip(span(title, bs_icon("info-circle", size = "0.8em")), tip),
      value = textOutput(paste0("kpi_", id), inline = TRUE),
      p(class = "small mb-1 opacity-75", sub),
      plotOutput(paste0("spark_", id), height = "56px"),
      showcase = bs_icon(icon), showcase_layout = "left center",
      theme = "primary", full_screen = FALSE, max_height = "190px"
    )
  })
  div(class = "mb-3", do.call(layout_columns, c(list(fill = FALSE, col_widths = c(4, 4, 4, 4, 4, 4)), boxes)),
      p(class = "small text-muted mt-1", textOutput("spark_note", inline = TRUE)))
}
