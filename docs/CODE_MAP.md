# Code map

| File | Purpose | Key functions |
|------|---------|---------------|
| `app/app.R` | Libraries, source order, `page_navbar` layout | `shinyApp(ui, sc_server)` |
| `app/R/spacecraft_params.R` | The one params list, the variant tibble (list columns), lognormal helper, variant long/wide converters | `sc_default_params`, `sc_variants_default`, `sc_rlnorm`, `sc_variants_long`, `sc_variants_from_long` |
| `app/R/spacecraft_model.R` | simmer model: arrival plan, six-station trajectory, kit generator, replications | `sc_arrival_plan`, `sc_build_env`, `sc_run_one`, `sc_run` |
| `app/R/spacecraft_metrics.R` | Per-unit table and KPIs from the monitor data | `sc_units`, `sc_rep_kpis`, `sc_summarise`, `sc_time_average` |
| `app/R/spacecraft_ga.R` | Knob table, GA operators, objective, search loop | `sc_knob_defs`, `sc_evaluate_genome`, `sc_run_ga` |
| `app/R/spacecraft_inputs.R` | One field table for every line-wide input (type, label, hover hint, params path); builds both the Simulate sidebar and Settings > Line settings | `sc_line_fields`, `sc_line_panels`, `sc_panel_inputs`, `sc_params_from_inputs`, `sc_horizon_hours`, `sc_horizon_label`, `sc_sync_fields` | `sc_rework_share()`, `sc_rework_label()`, `sc_mrb_problem()`: derived Rework and the Accepted + Scrap check.
| `app/R/spacecraft_theme.R` | Plot theme (base size 16, bottom one-row legend), variant colors | `theme_sc`, `sc_one_row_legend` |
| `app/R/spacecraft_plots.R` | Charts | `sc_plot_cycle`, `sc_plot_lateness`, `sc_plot_wip`, `sc_plot_kpi_reps`, `sc_plot_ga` |
| `app/R/spacecraft_ui.R` | Simulate (sidebar built from the field table), Optimize and About pages | `sc_simulate_ui`, `sc_optimize_ui`, `sc_about_ui` |
| `app/R/spacecraft_settings_page.R` | Settings page: sticky Save bar above the Variants and Line settings tabs | `sc_settings_ui` |
| `app/R/spacecraft_settings_variants.R` | Settings > Variants: full-screen flow-picture card and caption, variant dropdown, per-variant editor (Value | Setting | Applies to) with short labels, row and header hover hints | `sc_settings_variants_tab`, `sc_settings_variants_server`, `sc_item_labels`, `sc_item_label`, `sc_variation_tip`, `sc_flow_image` |
| `app/R/spacecraft_settings_line.R` | Settings > Line settings, Save bar status, two-way sync with the sidebar (loop-guarded, no renderUI) | `sc_settings_line_tab`, `sc_settings_line_server`, `sc_update_input`, `sc_save_bar_ui` |
| `app/www/spacecraft_line_flow.png` | Flow picture on Settings > Variants. Swap the picture by replacing this file. | — |
| `docs/flow.mmd`, `docs/flow_mermaid_config.json` | Mermaid source and theme for the flow picture | — |
| `app/R/spacecraft_server.R` | Reactive wiring: params, runs, KPIs, plots, GA | `sc_server` |
| `tests/spacecraft_checks.R` | Model acceptance checks | — |
| `tests/helpers/spacecraft_expected.R` | Test-only expected cycle time from an absorbing Markov chain | `sc_expected_cycle_h` |
| `tests/app_checks.R` | Shiny smoke checks, plus Settings: Save updates the sidebar, a sidebar change updates Settings, variant edits persist and reach the params, and the sync settles with no echo | — |
| `scripts/screenshots.R` | Headless page screenshots from a running app (`<port> [width] [prefix]`) | — |

## Chart → function

| Chart title | Function |
|-------------|----------|
| Cycle time by variant | `sc_plot_cycle` |
| Lateness at ship | `sc_plot_lateness` |
| Units in the line (replication 1) | `sc_plot_wip` |
| KPIs by replication | `sc_plot_kpi_reps` |
| Optimizer convergence | `sc_plot_ga` |

## Regenerate the flow picture

```bash
npx -y @mermaid-js/mermaid-cli@11 -i docs/flow.mmd -o app/www/spacecraft_line_flow.png \
  -c docs/flow_mermaid_config.json -s 3 -b white
```
