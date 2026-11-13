# Code map

| File | Purpose | Key functions |
|------|---------|---------------|
| `app/app.R` | Libraries, source order, `page_navbar` layout. `gtExtras`/`svglite` were dropped with the KPIs-by-replication sparkline table; remaining mini histograms use `gt::ggplot_image()` as PNG. | `shinyApp(ui, sc_server)` |
| `app/R/spacecraft_params.R` | The one params list, the variant tibble (list columns), lognormal helper, variant long/wide converters | `sc_default_params`, `sc_variants_default`, `sc_rlnorm`, `sc_variants_long`, `sc_variants_from_long` |
| `app/R/spacecraft_model.R` | simmer model: arrival plan, six-station trajectory, kit generator, replications | `sc_arrival_plan`, `sc_build_env`, `sc_run_one`, `sc_run` |
| `app/R/spacecraft_metrics.R` | Per-unit table and KPIs from the monitor data | `sc_units`, `sc_rep_kpis`, `sc_summarise`, `sc_time_average` |
| `app/R/spacecraft_ga.R` | Knob table, GA operators, on-time or cycle objective, search loop. The first generation is a random population that does not include the current line (which is still scored for the dashed reference and the recheck). | `sc_knob_defs`, `sc_ga_objectives`, `sc_evaluate_genome`, `sc_opt_random_pop`, `sc_run_ga` |
| `app/R/spacecraft_inputs.R` | One field table for every line-wide input (type, label, hover hint, params path); builds both the Simulate sidebar and Settings > Line settings | `sc_line_fields`, `sc_line_panels`, `sc_panel_inputs`, `sc_params_from_inputs`, `sc_horizon_hours`, `sc_horizon_label`, `sc_sync_fields` | `sc_rework_share()`, `sc_rework_label()`, `sc_mrb_problem()`: derived Rework and the Accepted + Scrap check.
| `app/R/spacecraft_theme.R` | Plot theme (base size 16, bottom one-row legend), variant colors | `theme_sc`, `sc_one_row_legend` |
| `app/R/spacecraft_plots.R` | Charts | `sc_plot_wip`, `sc_plot_ga` |
| `app/R/spacecraft_tables.R` | Simulate tables with mini histograms (gt) and human summary labels | `sc_table_cycle`, `sc_table_lateness`, `sc_format_summary_table`, `sc_summary_caption`, helpers `sc_mini_hist`, `sc_gt_plots` |
| `app/R/spacecraft_ui.R` | Simulate (sidebar from the field table; results in tabs) and About | `sc_simulate_ui`, `sc_about_ui` |
| `app/R/spacecraft_settings_page.R` | Settings page: sticky Save bar above the Variants and Line settings tabs | `sc_settings_ui` |
| `app/R/spacecraft_flow.R` | Inline SVG of how a unit moves through the line (flat star-map, Settings > Variants, including full-screen). No legend; Rework leaves Review horizontally and turns up into Integration with a rounded corner. Kitting, Functional test, Review, Environmental test and Final acceptance stars open Line settings at the matching card; Integration keeps its View settings link to the variant table. viewBox 1100×420 so the Scrap label is not clipped. | `sc_flow_svg`, `sc_flow_ui`, `sc_flow_hit`, `sc_flow_cols` |
| `app/R/spacecraft_settings_variants.R` | Settings > Variants: full-screen flow card and caption, variant dropdown, per-variant editor (Value | Setting | Applies to). Value cells are styled as editable (tinted, click to edit, pencil in the header). | `sc_settings_variants_tab`, `sc_settings_variants_server`, `sc_item_labels`, `sc_item_label`, `sc_variation_tip` |
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
| `app/R/spacecraft_optimize.R` | Optimize search + recheck bundle, net scores, paired change, story, preload load. Default cost is 1.0 per extra station. Algorithm settings (population, generations, mutation, crossover, elitism, recheck) sit in a collapsed accordion. | `sc_opt_run`, `sc_opt_attach_paired`, `sc_opt_story`, `sc_opt_default_cost`, `sc_load_preload_optimize` |
| `app/R/spacecraft_optimize_page.R` | Optimize page UI, chart, table, progress/Cancel server | `sc_optimize_ui`, `sc_plot_opt_history`, `sc_table_opt`, `sc_optimize_server` |
| `app/data/preload_optimize.rds` | Saved Quick search with a 20-rep recheck (seed 42) shown instantly on Optimize | rebuilt by `scripts/preload_optimize.R` |
| `scripts/preload_optimize.R` | Rebuild the preloaded optimize result | — |
| `app/R/spacecraft_warmup.R` | Warm-up check: Welch smooth of cycle time and WIP, suggested discard, KPI change, preload load | `sc_wu_run`, `sc_wu_recommend_weeks`, `sc_wu_story`, `sc_load_preload_warmup` |
| `app/R/spacecraft_warmup_page.R` | Warmup page UI, charts, table, progress/Cancel server | `sc_warmup_ui`, `sc_plot_wu_cycle`, `sc_plot_wu_wip`, `sc_table_wu`, `sc_warmup_server` |
| `app/data/preload_warmup.rds` | Saved 20-rep warm-up check (seed 42) shown instantly on Warmup | rebuilt by `scripts/preload_warmup.R` |
| `scripts/preload_warmup.R` | Rebuild the preloaded warm-up check | — |
| `app/R/spacecraft_server.R` | Reactive wiring: params, runs, KPIs, plots, Compare, Analyze, Tornado, Breakpoint, Optimize, Warmup | `sc_server` |
| `tests/spacecraft_checks.R` | Model acceptance checks | — |
| `tests/helpers/spacecraft_expected.R` | Test-only expected cycle time from an absorbing Markov chain | `sc_expected_cycle_h` |
| `tests/app_checks.R` | Shiny smoke checks, plus Settings: Save updates the sidebar, a sidebar change updates Settings, variant edits persist and reach the params, and the sync settles with no echo | — |
| `scripts/screenshots.R` | Headless page screenshots from a running app (`<port> [width] [prefix]`) | — |

## Chart → function

| Chart title | Function |
|-------------|----------|
| Cycle Times tab (table, mini histogram per variant) | `sc_table_cycle` |
| Schedule Slip tab (table, mini histogram; hours after the due date) | `sc_table_lateness` |
| WIP tab (units in the line, replication 1) | `sc_plot_wip` |
| Summary tab (human labels and a caption for the time basis and counts) | `sc_format_summary_table`, `sc_summary_caption` |
| Optimizer convergence | `sc_plot_opt_history` (legacy `sc_plot_ga`) |
| Optimize rechecked candidates | `sc_table_opt` |
| Warmup Cycle Time / WIP / KPIs tabs | `sc_plot_wu_cycle`, `sc_plot_wu_wip`, `sc_table_wu` |
| Compare results (mean and delta vs baseline) | `sc_table_compare` |
| Compare spread across replications (one plot per metric tab) | `sc_plot_compare` |
| Analyze Time Breakdown / Bottleneck / Queue Length tabs | `sc_plot_analyze_breakdown`, `sc_plot_analyze_util`, `sc_plot_analyze_queues` |
| Tornado sensitivity (filled dark-blue circle, filled orange triangle, baseline dashed line) | `sc_plot_tornado` |
| Tornado results (low, high, swing; baseline in the caption) | `sc_table_tornado` |
| Breakpoint Delivery / Cycle Time / Saturation tabs | `sc_plot_break_ontime`, `sc_plot_break_cycle`, `sc_plot_break_util`, `sc_table_break` |

## The flow picture

The Settings > Variants card draws an inline SVG from `app/R/spacecraft_flow.R`
(`sc_flow_ui()` / `sc_flow_svg()`). Edit that file to change the diagram.
`docs/flow.mmd` is the original box-and-arrow layout the SVG still follows;
the PNG under `app/www/` is kept only as a record of that render.


Screenshot scripts: `scripts/screenshots.R` (all pages), `scripts/screenshots_mrb.R`, `scripts/screenshots_tables.R`, `scripts/screenshots_flow.R`, `scripts/screenshots_compare.R`, `scripts/screenshots_analyze.R`, `scripts/screenshots_tornado.R`, `scripts/screenshots_breakpoint.R`, `scripts/screenshots_optimize.R`, `scripts/screenshots_warmup.R`.

