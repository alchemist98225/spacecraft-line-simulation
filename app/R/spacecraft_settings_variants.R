# spacecraft_settings_variants.R
# Settings > Variants tab: the line's flow diagram and an editor for the
# per-variant settings (product mix, Integration steps, test times, due dates).
# Variant edits apply straight away; they have no Simulate sidebar copy.
# One variant is shown at a time; every edit is written into one long table
# that holds all three variants, so switching variants never loses an edit,
# and sc_variants_from_long() turns it back into the params the model and GA use.

# Swap the picture by replacing this one file (served from app/www/).
# It is Doc's Mermaid flow (docs/flow.mmd) rendered with spelled-out labels.
sc_flow_image <- "spacecraft_line_flow.png"

# Plain-English names for the long-table codes.
sc_group_labels <- c(
  mix = "Product mix",
  route = "Integration",
  func_test = "Functional test",
  env = "Environmental test (vibration + thermal vacuum chamber)",
  due = "Due date"
)

sc_item_label <- function(item) {
  step_names <- c(structure = "Structure", harness = "Harness", avionics = "Avionics", propulsion = "Propulsion")
  fixed <- c(
    share = "Share of all releases (0 to 1)",
    mean_h = "Mean time (hours)",
    cv = "Coefficient of variation (spread of the time)",
    vibe_mean_h = "Vibration test: mean time (hours)",
    vibe_cv = "Vibration test: coefficient of variation",
    tvac_mean_h = "Thermal vacuum test: mean time (hours)",
    tvac_cv = "Thermal vacuum test: coefficient of variation",
    offset_h = "Time from release to due date (hours)",
    jitter_h = "Random shift of the due date, up to plus or minus (hours)"
  )
  step <- sub("_(mean_h|cv)$", "", item)
  stat <- sub("^.*_(mean_h|cv)$", "\\1", item)
  dplyr::case_when(
    item %in% names(fixed) ~ unname(fixed[item]),
    step %in% names(step_names) ~ paste0(step_names[step], " step: ",
                                         dplyr::if_else(stat == "mean_h", "mean time (hours)", "coefficient of variation")),
    TRUE ~ item
  )
}

# Column headers and the hover text for each.
sc_param_columns <- tibble::tribble(
  ~name,     ~tip,
  "Value",   "Type a new number here (double-click to edit). It is used the next time you run the simulation or the optimizer.",
  "Applies to", "The station on the line, or the order detail (product mix or due date), that this number belongs to.",
  "Setting", "What the number controls. Times are in hours. The coefficient of variation sets how much the time varies: 0 means always the same, and 1 means the spread is as large as the mean."
)

sc_settings_variants_tab <- function() {
  nav_panel(
    "Variants",
    card(
      card_header("How a unit moves through the line"),
      tags$img(src = sc_flow_image, alt = "Six-station spacecraft line flow diagram",
               style = "width: 100%; max-width: 1300px; height: auto;"),
      p(class = "mt-2 mb-0",
        "Every loop in the line returns to Integration. The picture leaves out one rule: once a unit has",
        "failed a test twice, it goes straight to the material review board. At that point the board can't",
        "send it back for rework, so the unit is either used as is or scrapped.")
    ),
    card(
      card_header("Settings for each variant"),
      p("The line builds three variants: A, B and C. Pick one below to see and change its settings:",
        "how often it is released, how long each Integration step and test takes, and how much time it has",
        "before it is due. Times vary from unit to unit, so each one is set by a mean and a coefficient of variation.",
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
      dplyr::transmute(Value = value, `Applies to` = unname(sc_group_labels[group]), Setting = sc_item_label(item)) %>%
      DT::datatable(
        container = header, rownames = FALSE, selection = "none",
        editable = list(target = "cell", disable = list(columns = 1:2)),
        options = list(dom = "t", pageLength = 100, ordering = FALSE, autoWidth = TRUE,
                       columnDefs = list(list(targets = 0, width = "110px", className = "dt-right"),
                                         list(targets = 1, width = "300px")))
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
