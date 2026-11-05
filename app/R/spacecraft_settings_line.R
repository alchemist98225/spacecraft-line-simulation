# spacecraft_settings_line.R
# Settings > Line settings tab and the sticky Save bar, plus the two-way sync
# with the Simulate sidebar:
#   * sidebar -> Settings: any sidebar change is copied to its "set_" twin
#     straight away (one observer per field).
#   * Settings -> sidebar: only when Save is clicked; only changed fields are sent.
# Loop guard: every copy first checks the two values differ, so a copy that
# lands triggers no copy back. Inputs are never rebuilt with renderUI; they are
# built once and changed with update*Input(), which avoids the selectize race.

# Default way to change an input in the browser. Tests pass their own.
sc_update_input <- function(session, type, id, value) {
  switch(type,
    numeric = shiny::updateNumericInput(session, id, value = value),
    select = shiny::updateSelectInput(session, id, selected = value),
    check = shiny::updateCheckboxInput(session, id, value = value)
  )
}

sc_same_value <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  isTRUE(all.equal(a, b, check.attributes = FALSE))
}

sc_save_bar_ui <- function() {
  div(
    class = "sc-save-bar d-flex flex-wrap align-items-center gap-3 px-3 py-2 mb-3",
    style = "position: sticky; top: 0; z-index: 1030; background: #eef6fb; border: 1px solid #9ecae1; border-radius: 6px;",
    actionButton("settings_save", tagList(icon("floppy-disk"), " Save to Simulate"), class = "btn-primary"),
    div(
      div(class = "fw-semibold", textOutput("settings_save_status", inline = TRUE)),
      div(class = "small", "Saving copies the line settings into the Simulate sidebar, so the next Run uses them.",
          "Variant edits apply straight away.")
    )
  )
}

sc_settings_line_tab <- function() {
  cards <- purrr::pmap(sc_line_panels, function(panel, title, icon) {
    rlang::exec(card, card_header(tagList(bs_icon(icon), " ", title)), !!!sc_panel_inputs(panel, prefix = "set_"))
  })
  nav_panel(
    "Line settings",
    p(class = "mt-2", "These settings apply to the whole line, whatever the variant. They mirror the Simulate sidebar:",
      "a change there shows up here straight away, and Save sends your changes here back to it.",
      "Hover over a setting for a short explanation."),
    rlang::exec(layout_column_wrap, width = "320px", fill = FALSE, heights_equal = "row", !!!cards)
  )
}

sc_settings_line_server <- function(input, output, session, update_fn = sc_update_input) {
  fields <- sc_line_fields()
  copies <- reactiveVal(0L)            # how many copies were sent (used by tests)
  saved <- reactiveVal(NULL)           # how many fields the last Save sent
  send <- function(type, id, value) {
    copies(isolate(copies()) + 1L)
    update_fn(session, type, id, value)
  }

  # Sidebar -> Settings, one observer per field.
  purrr::pwalk(fields[c("id", "type")], function(id, type) {
    set_id <- paste0("set_", id)
    observeEvent(input[[id]], {
      if (!sc_same_value(input[[id]], input[[set_id]])) send(type, set_id, input[[id]])
    }, ignoreInit = TRUE)
  })

  # Fields where Settings differs from the sidebar.
  unsaved <- reactive({
    fields %>%
      dplyr::filter(!purrr::map_lgl(id, ~ sc_same_value(input[[.x]], input[[paste0("set_", .x)]]) ||
                                       is.null(input[[paste0("set_", .x)]])))
  })
  observeEvent(unsaved(), if (nrow(unsaved()) > 0) saved(NULL))

  # Settings -> sidebar on Save. freezeReactiveValue stops anything reading
  # the old sidebar value until the browser confirms the new one.
  observeEvent(input$settings_save, {
    todo <- unsaved()
    purrr::pwalk(todo[c("id", "type")], function(id, type) {
      freezeReactiveValue(input, id)
      send(type, id, input[[paste0("set_", id)]])
    })
    saved(nrow(todo))
  })

  output$settings_save_status <- renderText({
    n <- nrow(unsaved())
    if (n > 0) {
      sprintf("Unsaved changes (%d): %s", n, paste(unsaved()$label, collapse = "; "))
    } else if (!is.null(saved())) {
      "Saved. The Simulate page now uses these settings."
    } else {
      "No unsaved changes. These match the Simulate page."
    }
  })

  list(copies = copies, unsaved = unsaved)
}
