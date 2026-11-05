# Code map

| File | Purpose | Key functions |
|------|---------|---------------|
| `app/app.R` | Libraries, source order, `page_navbar` layout | `shinyApp(ui, sc_server)` |
| `app/R/spacecraft_params.R` | The one params list, the variant tibble (list columns), lognormal helper, variant long/wide converters | `sc_default_params`, `sc_variants_default`, `sc_rlnorm`, `sc_variants_long`, `sc_variants_from_long` |
| `app/R/spacecraft_model.R` | simmer model: arrival plan, six-station trajectory, kit generator, replications | `sc_arrival_plan`, `sc_build_env`, `sc_run_one`, `sc_run` |
| `app/R/spacecraft_metrics.R` | Per-unit table and KPIs from the monitor data | `sc_units`, `sc_rep_kpis`, `sc_summarise`, `sc_time_average` |
| `app/R/spacecraft_ga.R` | Knob table, GA operators, objective, search loop | `sc_knob_defs`, `sc_evaluate_genome`, `sc_run_ga` |
| `app/R/spacecraft_inputs.R` | Spec tibble for every sidebar number and its params path | `sc_input_spec`, `sc_panel_inputs`, `sc_params_from_inputs` |
| `app/R/spacecraft_theme.R` | Plot theme (base size 16, bottom one-row legend), variant colors | `theme_sc`, `sc_one_row_legend` |
| `app/R/spacecraft_plots.R` | Charts | `sc_plot_cycle`, `sc_plot_lateness`, `sc_plot_wip`, `sc_plot_kpi_reps`, `sc_plot_ga` |
| `app/R/spacecraft_ui.R` | Simulate, Variants, Optimize and About pages | `sc_simulate_ui`, `sc_variants_ui`, `sc_optimize_ui`, `sc_about_ui` |
| `app/R/spacecraft_server.R` | Reactive wiring: params, runs, KPIs, plots, GA | `sc_server` |
| `tests/spacecraft_checks.R` | Model acceptance checks | — |
| `tests/helpers/spacecraft_expected.R` | Test-only expected cycle time from an absorbing Markov chain | `sc_expected_cycle_h` |
| `tests/app_checks.R` | Shiny smoke checks | — |
| `scripts/screenshots.R` | Headless page screenshots from a running app | — |

## Chart → function

| Chart title | Function |
|-------------|----------|
| Cycle time by variant | `sc_plot_cycle` |
| Lateness at ship | `sc_plot_lateness` |
| Units in the line (replication 1) | `sc_plot_wip` |
| KPIs by replication | `sc_plot_kpi_reps` |
| Optimizer convergence | `sc_plot_ga` |
