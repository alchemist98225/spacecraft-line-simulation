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
library(gt)
library(gtExtras)

purrr::walk(
  c("spacecraft_params", "spacecraft_model", "spacecraft_metrics", "spacecraft_theme",
    "spacecraft_plots", "spacecraft_tables", "spacecraft_ga", "spacecraft_inputs", "spacecraft_ui", "spacecraft_flow", "spacecraft_settings_variants", "spacecraft_settings_line", "spacecraft_settings_page", "spacecraft_compare", "spacecraft_compare_page", "spacecraft_analyze", "spacecraft_analyze_page", "spacecraft_tornado", "spacecraft_tornado_page", "spacecraft_breakpoint", "spacecraft_breakpoint_page", "spacecraft_optimize", "spacecraft_optimize_page", "spacecraft_server"),
  ~ source(file.path("R", paste0(.x, ".R")), local = FALSE)
)

ui <- page_navbar(
  title = tags$span(class = "d-flex flex-column", tags$span("Spacecraft Line Simulator"),
                    tags$small("Illustrative only")),
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c7fb8", "navbar-bg" = "#1b4f72"),
  fillable = FALSE,
  sc_simulate_ui(),
  sc_settings_ui(),
  sc_compare_ui(),
  sc_analyze_ui(),
  sc_tornado_ui(),
  sc_breakpoint_ui(),
  sc_optimize_ui(),
  sc_about_ui()
)

shinyApp(ui, sc_server)
