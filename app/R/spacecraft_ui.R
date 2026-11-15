# spacecraft_ui.R
# Page UIs (the Settings page is in spacecraft_settings_*.R). The Simulate sidebar follows the automotive app's style: a 320px
# bslib sidebar with collapsible accordion panels and a full-width Run button.

# Sidebar sections come from sc_line_panels / sc_line_fields (spacecraft_inputs.R),
# the same table that builds Settings > Line settings.
sc_sidebar_accordion <- function() {
  notes <- list(
    run = actionButton("run", tagList(icon("play"), " Run simulation"), class = "btn-primary w-100 mb-3"),
    arrivals = helpText("Variant mix and touch times are on Settings > Variants."),
    chamber = helpText("Waiting units are served in planned-ship order."),
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
        value_box("Throughput", textOutput("kpi_throughput"), showcase = bs_icon("box-seam")),
        value_box("Mean cycle time", textOutput("kpi_cycle"), showcase = bs_icon("clock")),
        value_box("Touch labor (HPU)", textOutput("kpi_hpu"), showcase = bs_icon("person-workspace")),
        value_box("Average WIP", textOutput("kpi_wip"), showcase = bs_icon("stack"))
      ),
      # The detailed charts and tables now live under Results. The bottom of
      # this page reuses the Settings star-map (sc_flow_ui / sc_flow_svg).
      card(
        full_screen = TRUE,
        card_header("How a unit moves through the line"),
        sc_flow_ui()
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
      p("A low-volume, high-mix line: subassembly build-up, integration, functional test,",
        "environmental test (vibe and TVAC share one chamber), MRB and final acceptance.",
        "Every rework loop returns to Integration. After the fail cap a unit is forced to MRB."),
      p(class = "mb-3", "All inputs are example values, not a calibrated program. See docs/SPACECRAFT.md."),
      tags$ul(class = "list-unstyled mb-0",
        sc_about_note("wrench-adjustable", "Equipment reliability.",
          "Each station type and the environmental test chamber can break down while a unit is being worked on.",
          "Failures are operating-time based (the clock does not run while a station is idle). When a bay fails, the unit stays in place and a dedicated maintainer, separate from the production technician pool, repairs it.",
          "Illustrative defaults (hours): Integration and functional test MTBF 2,000 / MTTR 8; chamber MTBF 1,000 / MTTR 24; review board and final acceptance MTBF 5,000 / MTTR 4; one maintainer.",
          "Inherent availability at those means is about 99.6% for Integration, 97.7% for the chamber, and 99.9% for review and acceptance.",
          "A toggle in Settings turns breakdowns off. HPU still counts only touch time, not repair time."),
        sc_about_note("people", "Technicians and the work week.",
          "A shared technician pool is scheduled by shift and by work week. The default is Monday–Friday; on Saturday and Sunday the pool is zero.",
          "Day, two-shift and three-shift patterns apply on the working days. The simulation horizon stays in calendar hours, including weekends.",
          "Each process step seizes its station and then a set number of technicians (editable in Settings).",
          "Touch labor hours per unit (HPU) are the sum of technicians × touch time at each step, including rework, and are reported overall and by variant.",
          "Labor utilization is busy technician-hours divided by scheduled technician-hours.",
          "Work pauses when a shift or the work week ends, and resumes when the next shift starts."),
        sc_about_note("boxes", "Subassembly build-up.",
          "Harnesses, avionics, propulsion and panels are built in parallel to feed Integration, with a per-variant lognormal time, their own station capacity, and technicians from the shared pool (counted in HPU). Parts are on hand by default.",
          "Parts are on hand by default. A Settings toggle can turn back on a long-lead parts delay before subassembly starts."),
        sc_about_note("clipboard-check", "Records, test data and buy-off holds.",
          "After Integration the unit waits in Build records review (as-built paperwork, torque and inspection records, and nonconformance closure before test).",
          "After the chamber it waits in Test data review (environmental test data and anomaly review before the unit is released from test).",
          "Before final acceptance it waits in Customer buy-off (final inspection, government/customer source inspection and shipping paperwork (DD-250)).",
          "Each hold seizes one inspector from a day-shift QA inspector pool (default two people, following the line's work week) for a short touch, then the remaining paperwork wait runs on the calendar.",
          "Inspector hours are not counted in production touch labor (HPU). The pool size and the paperwork waits are editable in Settings."),
        sc_about_note("flag", "Committed lead time.",
          "The committed lead time for the first unit (moved to the Advanced section) sets chamber priority: units with earlier committed ship dates go first.",
          "Later units are spaced one takt apart. The horizon and this lead are both calendar hours."),
        sc_about_note("play-fill", "Simulate.",
          "Run the current line and read the core metrics: throughput as a rate (units per year, with units per week alongside), cycle time, touch labor (HPU) and average WIP. The star-map under the value boxes is the same picture as Settings; click a station to open the matching Line settings card. The detailed charts, tables and CSV downloads now live under Results."),
        sc_about_note("sliders", "Settings.",
          "Change the line, the technician pool and the three variants. The star-map shows how a unit moves through the stations."),
        sc_about_note("bar-chart", "Results.",
          "A dropdown menu, like the automotive app, with a closer look at the last Simulate run.",
          "The dropdown is a drill-down: Bottleneck (where the time goes, then station utilization and downtime as a mean with a 10–90 bar, bottleneck called out), Cycle time (count histogram of every shipped unit, and an overlapping count histogram by variant), By variant (overlapping cycle-time and touch-labor count histograms by variant), Quality (scrap and rework by origin: functional test, environmental test, and MRB disposition), Reliability (observed versus configured MTBF and MTTR, with failure and repair counts), then Summary (throughput as a mean bar with a 10–90 whisker, WIP, labor, the run table, and CSV downloads)."),
        sc_about_note("layout-three-columns", "Compare.",
          "The precomputed set is the default line plus six capacity moves: add a subassembly station, add an integration station, add a technician per shift, staff weekends (a 7-day week — the line already runs three shifts Monday–Friday), shorten the three calendar holds (Build records review, Test data review, and Customer buy-off) by 20%, and add a QA inspector (two to three on the day shift).",
          "Every move is scored first by the throughput gain versus the baseline, then by HPU, WIP and cycle time. Click two scenarios in the sidebar to put them on the detail charts, or add a custom snapshot of the current Settings.",
          "Cycle time and touch labor are overlapping count histograms of shipped units. A Flow tab shows WIP over time, and a Downtime tab compares equipment repair."),
        sc_about_note("bar-chart-steps", "Tornado.",
          "Moves each key input one at a time to a low and a high setting",
          "(times and rates by 20%, station counts and the technician pool by one, and MTBF / MTTR by 20%) and shows the swing in a chosen output:",
          "mean cycle time, throughput, touch labor (HPU), or labor utilization. The dashed line is the baseline.",
          "It opens on a preloaded tornado (20 replications, seed 42, the same default line as Compare) and can also run a live tornado from the current Settings."),
        sc_about_note("graph-up", "Breakpoint.",
          "Raises the release rate from the current Settings and finds the highest demand the line can hold",
          "before mean cycle time crosses a multiple of the baseline (1.5× by default) or a station or the technician pool hits a utilization cap (90% by default).",
          "It plots throughput, cycle time and utilization against offered demand, and marks the first binding limit.",
          "A preloaded sweep (20 replications, seed 42, the same default line as Compare) opens at once; you can also run a live sweep."),
        sc_about_note("gear", "Optimize.",
          "Searches station counts and technicians per shift around the current Settings to raise throughput at the current release rate, or to cut cycle time, charging a cost for each extra station or technician so the trade-off stays honest. Adding a technician is charged at a quarter of the cost of adding a station (0.25 versus 1.0 by default), because a person is cheaper than a new bay or chamber. The chart and table use a net throughput score (higher is better) or a net cycle time (lower is better), plus a paired change versus the current line. It opens on a preloaded Quick search (seed 42) and can also run a live search with a progress bar and Cancel. After the search, the current line, the top candidates, and each one-step change are re-checked with more replications and the same seeds."),
        sc_about_note("hourglass-split", "Warmup.",
          "Checks how the empty start biases the first units. It plots a moving-window (Welch) smooth of cycle time by release order and of WIP by week, with a P10–P90 ribbon and a dashed line at a suggested warm-up, and it shows how cycle time and throughput change if those early units are discarded. The Warm-up to discard setting is 0 weeks by default, so the other pages keep their current numbers unless you opt in. It opens on a preloaded check (20 replications, seed 42) and can also run a live check with a progress bar and Cancel."),
        sc_about_note("rocket-takeoff", "Ramp.",
          "Plans a capacity ramp against a rising target rate (12 to 48 units a year over 24 months by default, in an editable quarter table).",
          "Each quarter is a one-year snapshot at that rate, with touch times (including subassembly) scaled by a Wright/Crawford learning curve (85% per doubling by default, relative to the Settings times at a stated cumulative volume). Inspection holds and late-parts delay are not scaled.",
          "The one headline check is whether mean annualized throughput meets the target rate (at least 95%). Completions after a 16-week warm-up are scaled to a year, so a multi-week cycle time on an empty start is not counted as a rate miss. Cycle time, HPU, labor utilization, station utilization and WIP are shown alongside.",
          "The page recommends technician, shift, or station moves and an order-by date equal to the need date minus hire-and-train or procurement lead time, and it shows the dip if a move is delayed.",
          "It opens on a preloaded plan for a lean early line (day shift, two technicians) so the ramp actually needs capacity; a live plan can also start from the current Settings. The default three-shift line already holds this 12-to-48 ramp.")
      )
    ))
  )
}
