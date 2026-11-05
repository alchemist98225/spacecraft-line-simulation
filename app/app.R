# =============================================================================
# Spacecraft Line Simulator — Shiny app
# Low-volume, high-mix discrete-event simulation with simmer.
# Inputs are ILLUSTRATIVE example values, not a calibrated program.
# =============================================================================
# WHERE THINGS LIVE: see docs/CODE_MAP.md

library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(simmer)
library(DT)

purrr::walk(
  c("spacecraft_params", "spacecraft_model", "spacecraft_metrics", "spacecraft_theme",
    "spacecraft_plots", "spacecraft_ga", "spacecraft_inputs", "spacecraft_ui", "spacecraft_settings_variants", "spacecraft_settings_line", "spacecraft_settings_page", "spacecraft_server"),
  ~ source(file.path("R", paste0(.x, ".R")), local = FALSE)
)

ui <- page_navbar(
  title = tags$span(class = "d-flex flex-column", tags$span("Spacecraft Line Simulator"),
                    tags$small("Illustrative only")),
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c7fb8", "navbar-bg" = "#1b4f72"),
  fillable = FALSE,
  sc_simulate_ui(),
  sc_settings_ui(),
  sc_optimize_ui(),
  sc_about_ui()
)

shinyApp(ui, sc_server)
