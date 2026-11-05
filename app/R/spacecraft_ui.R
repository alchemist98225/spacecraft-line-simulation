# spacecraft_ui.R
# Page UIs (the Parameters page is in spacecraft_parameters_page.R). The Simulate sidebar follows the automotive app's style: a 320px
# bslib sidebar with collapsible accordion panels and a full-width Run button.

sc_simulate_ui <- function() {
  panel <- function(title, icon, value, ...) accordion_panel(title = tagList(bs_icon(icon), " ", title), value = value, ...)
  nav_panel(
    "Simulate",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        id = "sc_sidebar", title = "Line parameters", width = 320, open = "desktop",
        accordion(
          id = "sc_controls", open = "run", multiple = TRUE,
          panel("Run simulation", "play-fill", "run",
                actionButton("run", tagList(icon("play"), " Run simulation"), class = "btn-primary w-100 mb-3"),
                numericInput("n_reps", "Number of replications", 5, min = 1, max = 50, step = 1),
                numericInput("seed", "Base seed", 42, min = 1, step = 1),
                sc_panel_inputs("run")),
          panel("Releases", "box-arrow-in-right", "arrivals",
                selectInput("arrival_process", "Release spacing",
                            c("Random (exponential gaps)" = "poisson", "Evenly scheduled" = "fixed")),
                sc_panel_inputs("arrivals"),
                helpText("Variant mix, touch times and due dates are on the Parameters page.")),
          panel("Kitting (long-lead parts)", "boxes", "kitting", sc_panel_inputs("kitting")),
          panel("Capacities", "diagram-3", "capacity", sc_panel_inputs("capacity")),
          panel("Test failures", "exclamation-triangle", "quality", sc_panel_inputs("quality")),
          panel("Environmental chamber", "thermometer-half", "chamber",
                checkboxInput("chamber_preempt", "Urgent units can bump a running test", TRUE),
                checkboxInput("chamber_restart", "Bumped test restarts from zero", FALSE),
                helpText("Priority comes from due-date slack: less slack goes first.")),
          panel("MRB", "people", "mrb", sc_panel_inputs("mrb"),
                helpText("Shares are normalised. Rework is not offered once a unit hits the fail cap.")),
          panel("Final acceptance", "check2-circle", "acceptance", sc_panel_inputs("acceptance"))
        )
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
      card(card_header("Cycle time by variant"), plotOutput("plot_cycle", height = 520)),
      card(card_header("Lateness"), plotOutput("plot_lateness", height = 480)),
      card(card_header("WIP over time"), plotOutput("plot_wip", height = 480)),
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
