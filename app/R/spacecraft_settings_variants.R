# spacecraft_settings_variants.R
# Settings > Variants tab: the line's flow diagram and an editor for the
# per-variant settings (product mix, Integration steps, test times, due dates).
# Variant edits apply straight away; they have no Simulate sidebar copy.
# One variant is shown at a time; every edit is written into one long table
# that holds all three variants, so switching variants never loses an edit,
# and sc_variants_from_long() turns it back into the params the model and GA use.

# The flow picture is an inline SVG (sc_flow_ui() in spacecraft_flow.R),
# not a PNG. Layout follows docs/flow.mmd; the look is the approved flat star map.

# Plain-English names for the long-table codes. Every time is in hours (the
# caption says so), so labels stay short: a step's mean time is just its name
# and its coefficient of variation is "<name> variation".
sc_group_labels <- c(
  mix = "Product mix",
  route = "Integration",
  func_test = "Functional test",
  env = "Environmental test",
  due = "Due date"
)

sc_variation_tip <- paste(
  "Variation is the coefficient of variation: the standard deviation divided by the mean.",
  "At 0.5 a typical unit runs about 50% above or below the mean time. At 0 every unit takes exactly the mean.",
  "Higher values give a wider spread, with more slow units in the long tail, because the times are lognormal."
)

# item code -> short label and a per-row hover hint.
sc_item_labels <- function() {
  steps <- c(structure = "Structure", harness = "Harness", avionics = "Avionics", propulsion = "Propulsion",
             func = "Functional test", vibe = "Vibration", tvac = "Thermal vacuum")
  timed <- tibble::tibble(key = names(steps), name = unname(steps)) %>%
    tidyr::expand_grid(stat = c("mean_h", "cv")) %>%
    dplyr::transmute(
      item = paste(key, stat, sep = "_"),
      label = dplyr::if_else(stat == "mean_h", name, paste(name, "variation")),
      tip = dplyr::if_else(stat == "mean_h", paste0("Mean time for ", name, ", in hours."),
                           paste0("How much the ", name, " time varies from unit to unit. ", sc_variation_tip))
    )
  dplyr::bind_rows(
    timed,
    tibble::tribble(
      ~item,      ~label,               ~tip,
      "share",    "Share of releases",  "This variant's share of all released units, from 0 to 1.",
      "offset_h", "Due date allowance", "Hours from release to the due date.",
      "jitter_h", "Due date spread",    "Each unit's due date moves by a random amount up to this many hours earlier or later (an even spread, plus or minus)."
    )
  )
}

# Functional test uses plain mean_h / cv codes; give them the "func_" prefix.
sc_item_key <- function(item, group) dplyr::if_else(group == "func_test", paste0("func_", item), item)

sc_item_label <- function(item, group = rep("route", length(item))) {
  lab <- sc_item_labels()
  key <- sc_item_key(item, group)
  out <- lab$label[match(key, lab$item)]
  dplyr::coalesce(out, item)
}

sc_item_tip <- function(item, group) {
  lab <- sc_item_labels()
  lab$tip[match(sc_item_key(item, group), lab$item)]
}

# Column headers and the hover text for each, in display order.
sc_param_columns <- tibble::tribble(
  ~name,        ~tip,
  "Value",      "Double-click a number to change it. It is used the next time you run the simulation or the optimizer.",
  "Setting",    "What the number controls. Times are in hours. A name on its own is the mean time; \"variation\" is the coefficient of variation (standard deviation divided by the mean). Hover a row for more.",
  "Applies to", "The station, or the order detail (product mix or due date), that the number belongs to."
)

sc_settings_variants_tab <- function() {
  nav_panel(
    "Variants",
    tags$style(HTML("
      .sc-tip { text-decoration: underline dotted; cursor: help; }
      #variants_table th:first-child, #variants_table td:first-child { text-align: left !important; }
    ")),
    card(
      full_screen = TRUE,
      card_header("How a unit moves through the line"),
      sc_flow_ui()
    ),
    card(
      id = "sc-variant-settings",
      card_header("Settings for each variant"),
      p("The line builds three variants: A, B and C. Pick one below to see and change its settings:",
        "its share of releases, how long each Integration step and test takes, and how much time it has before it is due.",
        "Every time is in hours. Times differ from unit to unit, so each step has a mean time (the step name on its own)",
        "and a", span(class = "sc-tip", title = sc_variation_tip, "variation"), "(the step name followed by \"variation\").",
        "Your changes are kept when you switch variants and take effect the next time you run the simulation or the optimizer."),
      div(class = "d-flex align-items-end gap-3 mb-2",
          selectInput("variant_pick", "Variant", choices = c("Variant A" = "A", "Variant B" = "B", "Variant C" = "C"),
                      width = "220px"),
          actionButton("variants_reset", "Reset all variants to defaults", class = "btn-outline-secondary mb-3")),
      DT::DTOutput("variants_table")
    )
  )
}

# Server half. Returns the full variant tibble for the model (`variants`) and
# the rows currently on screen (`shown`), both as reactives.
sc_settings_variants_server <- function(input, output, session) {
  variants_long <- reactiveVal(sc_variants_long(sc_variants_default()))
  observeEvent(input$variants_reset, variants_long(sc_variants_long(sc_variants_default())))

  shown <- reactive({
    variants_long() %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      dplyr::filter(variant == (input$variant_pick %||% "A"))
  })

  header <- htmltools::withTags(table(thead(tr(
    purrr::pmap(sc_param_columns, function(name, tip) th(title = tip, name))
  ))))

  output$variants_table <- DT::renderDT({
    shown() %>%
      dplyr::transmute(Value = value, Setting = sc_item_label(item, group),
                       `Applies to` = unname(sc_group_labels[group]), tip = sc_item_tip(item, group)) %>%
      DT::datatable(
        container = header, rownames = FALSE, selection = "none", width = "640px",
        editable = list(target = "cell", disable = list(columns = 1:3)),
        options = list(
          dom = "t", pageLength = 100, ordering = FALSE, autoWidth = FALSE,
          columnDefs = list(list(targets = 0, width = "90px", className = "dt-left"),
                            list(targets = 1, width = "260px"),
                            list(targets = 2, width = "200px"),
                            list(targets = 3, visible = FALSE)),
          # Hover hint on each Setting cell, from the hidden tip column.
          rowCallback = DT::JS("function(row, data) { $('td:eq(1)', row).attr('title', data[3]); }")
        )
      )
  }, server = TRUE)

  # DT reports the row within the shown variant; map it back to the full table.
  observeEvent(input$variants_table_cell_edit, {
    e <- input$variants_table_cell_edit
    value <- suppressWarnings(as.numeric(e$value))
    if (is.na(value)) return()
    d <- variants_long()
    d$value[[shown()$row_id[[e$row]]]] <- value
    variants_long(d)
  })

  list(
    variants = reactive(sc_variants_from_long(variants_long(), sc_variants_default())),
    shown = shown
  )
}
