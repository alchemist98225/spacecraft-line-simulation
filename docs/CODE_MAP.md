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
| `app/R/spacecraft_plots.R` | Charts | `sc_plot_wip`, `sc_plot_ga` |
| `app/R/spacecraft_tables.R` | Simulate tables with pictures in them (gt + gtExtras) | `sc_table_cycle`, `sc_table_lateness`, `sc_table_kpi_reps`, helpers `sc_mini_hist`, `sc_mini_bullet`, `sc_gt_plots` |
| `app/R/spacecraft_ui.R` | Simulate (sidebar built from the field table), Optimize and About pages | `sc_simulate_ui`, `sc_optimize_ui`, `sc_about_ui` |
| `app/R/spacecraft_settings_page.R` | Settings page: sticky Save bar above the Variants and Line settings tabs | `sc_settings_ui` |
| `app/R/spacecraft_flow.R` | Inline SVG of how a unit moves through the line (flat star-map, Settings > Variants, including full-screen) | `sc_flow_svg`, `sc_flow_ui`, `sc_flow_cols` |
| `app/R/spacecraft_settings_variants.R` | Settings > Variants: full-screen flow card and caption, variant dropdown, per-variant editor (Value | Setting | Applies to) with short labels, row and header hover hints | `sc_settings_variants_tab`, `sc_settings_variants_server`, `sc_item_labels`, `sc_item_label`, `sc_variation_tip` |
| `app/R/spacecraft_settings_line.R` | Settings > Line settings, Save bar status, two-way sync with the sidebar (loop-guarded, no renderUI) | `sc_settings_line_tab`, `sc_settings_line_server`, `sc_update_input`, `sc_save_bar_ui` |
| `docs/flow.mmd`, `docs/flow_mermaid_config.json` | Mermaid source for the original box-and-arrow layout the SVG follows | — |
| `app/www/spacecraft_line_flow.png` | Original Mermaid render of that layout. The Settings card now uses the inline SVG in `spacecraft_flow.R`, not this file. | — |
| `app/R/spacecraft_compare.R` | Compare a baseline with up to three what-if scenarios under common random numbers; load the preloaded set | `sc_compare_catalog`, `sc_compare_run`, `sc_compare_table`, `sc_compare_mark_deltas`, `sc_load_preload`, `sc_params_changes` |
| `app/R/spacecraft_compare_page.R` | Compare page UI, results table, spread plot, progress/Cancel server | `sc_compare_ui`, `sc_table_compare`, `sc_plot_compare`, `sc_compare_server` |
| `app/data/preload_scenarios.rds` | Saved 20-rep comparison (baseline + three what-ifs) shown instantly on Compare | rebuilt by `scripts/preload_scenarios.R` |
| `scripts/preload_scenarios.R` | Rebuild the preloaded comparison | — |
| `app/R/spacecraft_analyze.R` | Why the line performs as it does: cycle-time parts, utilization, queue ribbons, plain-English summary | `sc_analyze`, `sc_unit_breakdown`, `sc_resource_use`, `sc_analyze_story`, `sc_load_preload_analyze` |
| `app/R/spacecraft_analyze_page.R` | Analyze page UI, plots, table, progress/Cancel server | `sc_analyze_ui`, `sc_plot_analyze_breakdown`, `sc_plot_analyze_util`, `sc_plot_analyze_queues`, `sc_analyze_server` |
| `app/data/preload_analyze.rds` | Saved 20-rep baseline diagnostics shown instantly on Analyze | rebuilt by `scripts/preload_scenarios.R` |
| `app/R/spacecraft_tornado.R` | One-at-a-time sensitivity around the current (or default) line; load the preloaded tornado | `sc_tornado_factors`, `sc_tornado_run`, `sc_tornado_story`, `sc_load_preload_tornado` |
| `app/R/spacecraft_tornado_page.R` | Tornado page UI, chart, table, progress/Cancel server | `sc_tornado_ui`, `sc_plot_tornado`, `sc_table_tornado`, `sc_tornado_server` |
| `app/data/preload_tornado.rds` | Saved 20-rep tornado (seed 42) shown instantly on Tornado | rebuilt by `scripts/preload_tornado.R` |
| `scripts/preload_tornado.R` | Rebuild the preloaded tornado | — |
| `app/R/spacecraft_breakpoint.R` | Demand sweep and on-time breakpoint; load the preloaded sweep | `sc_break_run`, `sc_break_detect`, `sc_break_story`, `sc_load_preload_breakpoint` |
| `app/R/spacecraft_breakpoint_page.R` | Breakpoint page UI, charts, table, progress/Cancel server | `sc_breakpoint_ui`, `sc_plot_break_ontime`, `sc_plot_break_cycle`, `sc_plot_break_util`, `sc_breakpoint_server` |
| `app/data/preload_breakpoint.rds` | Saved 20-rep demand sweep (seed 42) shown instantly on Breakpoint | rebuilt by `scripts/preload_breakpoint.R` |
| `scripts/preload_breakpoint.R` | Rebuild the preloaded demand sweep | — |
| `app/R/spacecraft_server.R` | Reactive wiring: params, runs, KPIs, plots, GA, Compare, Analyze, Tornado, Breakpoint | `sc_server` |
| `tests/spacecraft_checks.R` | Model acceptance checks | — |
| `tests/helpers/spacecraft_expected.R` | Test-only expected cycle time from an absorbing Markov chain | `sc_expected_cycle_h` |
| `tests/app_checks.R` | Shiny smoke checks, plus Settings: Save updates the sidebar, a sidebar change updates Settings, variant edits persist and reach the params, and the sync settles with no echo | — |
| `scripts/screenshots.R` | Headless page screenshots from a running app (`<port> [width] [prefix]`) | — |

## Chart → function

| Chart title | Function |
|-------------|----------|
| Cycle time by variant (table, mini histogram per variant) | `sc_table_cycle` |
| Lateness at ship (table, mini histogram + median/mean vs due date bar) | `sc_table_lateness` |
| Units in the line (replication 1) | `sc_plot_wip` |
| KPIs by replication (table, mean/lowest/highest + per-replication sparkline) | `sc_table_kpi_reps` |
| Optimizer convergence | `sc_plot_ga` |
| Compare results (mean and P10–P90, delta vs baseline) | `sc_table_compare` |
| Compare spread across replications | `sc_plot_compare` |
| Analyze time breakdown (share of cycle time) | `sc_plot_analyze_breakdown` |
| Analyze bottleneck (utilization and queue) | `sc_plot_analyze_util` |
| Analyze queue length over time | `sc_plot_analyze_queues` |
| Tornado sensitivity (low/high bars, baseline dashed line) | `sc_plot_tornado` |
| Tornado results (low, baseline, high, swing) | `sc_table_tornado` |
| Breakpoint on-time vs demand | `sc_plot_break_ontime` |
| Breakpoint cycle time vs demand | `sc_plot_break_cycle` |
| Breakpoint station utilization vs demand | `sc_plot_break_util` |
| Breakpoint sweep results | `sc_table_break` |

## The flow picture

The Settings > Variants card draws an inline SVG from `app/R/spacecraft_flow.R`
(`sc_flow_ui()` / `sc_flow_svg()`). Edit that file to change the diagram.
`docs/flow.mmd` is the original box-and-arrow layout the SVG still follows;
the PNG under `app/www/` is kept only as a record of that render.


Screenshot scripts: `scripts/screenshots.R` (all pages), `scripts/screenshots_mrb.R`, `scripts/screenshots_tables.R`, `scripts/screenshots_flow.R`, `scripts/screenshots_compare.R`, `scripts/screenshots_analyze.R`, `scripts/screenshots_tornado.R`, `scripts/screenshots_breakpoint.R`.

