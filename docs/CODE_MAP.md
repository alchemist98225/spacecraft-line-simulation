# Code map

| File | Purpose | Key functions |
|------|---------|---------------|
| `app/app.R` | Libraries, source order, `page_navbar` layout (Simulate, Results, Settings, Compare, …). `gtExtras`/`svglite` were dropped with the KPIs-by-replication sparkline table; remaining mini histograms use `gt::ggplot_image()` as PNG. | `shinyApp(ui, sc_server)` |
| `app/www/sc-responsive.css`, `app/www/sc-responsive.js` | Below 1200px: sidebars start collapsed with a labeled Controls toggle; card tab strips become a `<select>` (client-side, no extra Shiny round-trip). Plotly/htmlwidgets resize when a tab is shown or a sidebar opens. | |
| `app/R/spacecraft_params.R` | The one params list, the variant tibble (list columns), lognormal helper, variant long/wide converters | `sc_default_params`, `sc_variants_default`, `sc_rlnorm`, `sc_variants_long`, `sc_variants_from_long` |
| `app/R/spacecraft_labor.R` | Shared technician pool: work week and shift patterns, per-step tech counts, committed-slot helpers used internally for chamber sequencing, the seize/touch/release wrapper (including mid-touch repairs and weekend pauses), and the day-shift QA inspector pool | `sc_labor_step_defaults`, `sc_labor_capacity`, `sc_labor_patterns`, `sc_labor_work_weeks`, `sc_tech_touch`, `sc_qa_inspector_capacity`, `sc_qa_inspector_touch`, `sc_commit_due` |
| `app/R/spacecraft_reliability.R` | Equipment MTBF/MTTR: event recording, downtime and wait-reason metrics, horizontal downtime bars (mean + 10–90, no points; Compare dodges by scenario), dodged horizontal wait-reason bars (mean + 10–90, starved in a free-x panel), WIP/demand grids, tidy CSV export helpers, Compare export binders | `sc_reliability_defaults`, `sc_downtime_by_rep`, `sc_wait_reasons`, `sc_plot_downtime`, `sc_plot_downtime_bars`, `sc_plot_compare_downtime`, `sc_plot_wait_reasons`, `sc_wip_grid`, `sc_demand_grid`, `sc_export_tables`, `sc_compare_flow`, `sc_compare_export_tables`, `sc_compare_units` |
| `app/R/spacecraft_model.R` | simmer model: arrival plan, six-station trajectory, kit generator, replications, maintenance and QA inspector resources; each paperwork hold is an inspector touch plus calendar wait | `sc_arrival_plan`, `sc_build_env`, `sc_qa_hold`, `sc_run_one`, `sc_run` |
| `app/R/spacecraft_metrics.R` | Per-unit table and KPIs from the monitor data, including HPU and labor utilization | `sc_units`, `sc_rep_kpis`, `sc_summarise`, `sc_util_from_monitor`, `sc_time_average` |
| `app/R/spacecraft_ga.R` | Knob table (station counts plus technicians per shift), GA operators, cycle or throughput objective with a cost for extras, search loop. The first generation is a random population that does not include the current line (which is still scored for the dashed reference and the recheck). | `sc_knob_defs`, `sc_ga_objectives`, `sc_ga_cost_term`, `sc_evaluate_genome`, `sc_opt_random_pop`, `sc_run_ga` |
| `app/R/spacecraft_inputs.R` | One field table for every line-wide input (type, label, hover hint, params path); builds both the Simulate sidebar and Settings > Line settings | `sc_line_fields`, `sc_line_panels`, `sc_panel_inputs`, `sc_params_from_inputs`, `sc_horizon_hours`, `sc_horizon_label`, `sc_sync_fields` | `sc_rework_share()`, `sc_rework_label()`, `sc_mrb_problem()`: derived Rework and the Accepted + Scrap check.
| `app/R/spacecraft_theme.R` | Plot theme (base size 16, bottom one-row legend), variant colors | `theme_sc`, `sc_one_row_legend` |
| `app/R/spacecraft_plots.R` | Charts | `sc_plot_wip`, `sc_plot_demand`, `sc_plot_cycle_dist`, `sc_plot_cycle_over_time`, `sc_plot_labor_util`, `sc_plot_hpu_dist`, `sc_plot_compare_wip_flow`, `sc_plot_compare_queue_flow`, `sc_plot_ga` |
| `app/R/spacecraft_results_plots.R` | Lead Results and Compare charts: cycle-time histogram with per-replication densities, utilization / downtime mean + 10–90 bars, quality rates by origin, observed vs configured MTBF/MTTR, what-if throughput-gains overview | `sc_plot_cycle_hist_reps`, `sc_plot_resource_spread`, `sc_plot_quality_rate`, `sc_quality_by_rep`, `sc_reliability_observed`, `sc_plot_reliability_obs`, `sc_plot_compare_gains`, `sc_table_compare_gains`, `sc_throughput_label` |
| `app/R/spacecraft_tables.R` | Simulate tables with mini histograms (gt) and human summary labels. Simulate headlines are throughput as a rate (units/year, with units/week), cycle time, HPU and WIP. Summary-table labels also include labor utilization and scrap. | `sc_table_cycle`, `sc_table_hpu`, `sc_format_summary_table`, `sc_summary_caption`, helpers `sc_mini_hist`, `sc_gt_plots` |
| `app/R/spacecraft_ui.R` | Simulate (sidebar, value boxes, and the Settings star-map) and About | `sc_simulate_ui`, `sc_about_ui` |
| `app/R/spacecraft_settings_page.R` | Settings page: sticky Save bar above the Variants and Line settings tabs | `sc_settings_ui` |
| `app/R/spacecraft_flow.R` | Inline SVG of how a unit moves through the line (flat star-map (Subassembly through Final acceptance), reused on Simulate and Settings > Variants, including full-screen). No legend; Rework leaves Review horizontally and turns up into Integration with a rounded corner. Diamond markers on the forward path are the three calendar holds: Records (Build records review after Integration), Test data (Test data review after the chamber), and Buy-off (Customer buy-off before Final acceptance); they open Line settings > Records, test data and buy-off. The MRB scrap pathway is in the model but is not drawn. Subassembly, Functional test, Review, Environmental test and Final acceptance stars open Line settings at the matching card; Integration keeps its View settings link to the variant table. viewBox 1100×350. | `sc_flow_svg`, `sc_flow_ui`, `sc_flow_hit`, `sc_flow_qa_marker`, `sc_flow_cols` |
| `app/R/spacecraft_settings_variants.R` | Settings > Variants: full-screen flow card and caption, variant dropdown, per-variant editor (Value | Setting | Applies to). Value cells are styled as editable (tinted, click to edit, pencil in the header). | `sc_settings_variants_tab`, `sc_settings_variants_server`, `sc_item_labels`, `sc_item_label`, `sc_variation_tip` |
| `app/R/spacecraft_settings_line.R` | Settings > Line settings, Save bar status, two-way sync with the sidebar (loop-guarded, no renderUI) | `sc_settings_line_tab`, `sc_settings_line_server`, `sc_update_input`, `sc_save_bar_ui` |
| `docs/flow.mmd`, `docs/flow_mermaid_config.json` | Mermaid source for the original box-and-arrow layout the SVG follows | — |
| `app/www/spacecraft_line_flow.png` | Original Mermaid render of that layout. The Settings card now uses the inline SVG in `spacecraft_flow.R`, not this file. | — |
| `app/R/spacecraft_compare.R` | Compare the default line with six capacity-move what-ifs (including +1 QA inspector) under common random numbers, scored first by throughput then HPU, WIP and cycle time; click-to-select two for the detail charts; load the preloaded set; differ table walks nested params including qa$ | `sc_compare_catalog`, `sc_compare_metrics`, `sc_compare_run`, `sc_compare_table`, `sc_compare_mark_deltas`, `sc_compare_default_selection`, `sc_compare_toggle_selection`, `sc_compare_rep_points`, `sc_load_preload`, `sc_params_changes` |
| `app/R/spacecraft_compare_plotly.R` | Plotly + crosstalk Compare charts (green/yellow/red/cyan, shapes): throughput mean bars with a thin 10–90 whisker, overlapping transparent histograms for cycle time and touch labor, linked replication highlight on the histograms | `sc_plot_throughput_bars`, `sc_plot_overlap_hist`, `sc_plotly_throughput_bars`, `sc_plotly_overlap_hist`, `sc_compare_set_colors` |
| `app/R/spacecraft_compare_page.R` | Compare page UI, gains overview, results table, sidebar picker, progress/Cancel server | `sc_compare_ui`, `sc_table_compare`, `sc_plot_compare`, `sc_compare_server` |
| `app/R/spacecraft_results_page.R` | Results navbar menu: Bottleneck, Cycle time, By variant, Quality, Reliability, Summary (including throughput bars) | `sc_results_menu`, `sc_results_server` |
| `app/data/preload_scenarios.rds` | Saved 20-rep comparison (baseline + six capacity moves, including +1 QA inspector) shown instantly on Compare | rebuilt by `scripts/preload_scenarios.R` |
| `scripts/preload_scenarios.R` | Rebuild the preloaded comparison | — |
| `app/R/spacecraft_analyze.R` | Diagnostics used by Results: cycle-time parts, utilization (including QA inspectors), queue ribbons, plain-English summary | `sc_analyze`, `sc_unit_breakdown`, `sc_resource_use`, `sc_analyze_story`, `sc_results_resource_catalog` |
| `app/R/spacecraft_analyze_page.R` | Plot and table helpers still used by Results (the Analyze nav tab was retired) | `sc_plot_analyze_breakdown`, `sc_plot_analyze_util`, `sc_plot_analyze_queues` |
| `app/R/spacecraft_tornado.R` | One-at-a-time sensitivity around the current (or default) line; load the preloaded tornado | `sc_tornado_factors`, `sc_tornado_run`, `sc_tornado_story`, `sc_load_preload_tornado` |
| `app/R/spacecraft_tornado_page.R` | Tornado page UI, chart, table, progress/Cancel server | `sc_tornado_ui`, `sc_plot_tornado`, `sc_table_tornado`, `sc_tornado_server` |
| `app/data/preload_tornado.rds` | Saved 20-rep tornado (seed 42) shown instantly on Tornado. Failure rates, MTBF and MTTR are omitted. | rebuilt by `scripts/preload_tornado.R` |
| `scripts/preload_tornado.R` | Rebuild the preloaded tornado | — |
| `app/R/spacecraft_breakpoint.R` | Demand sweep and cycle-time / utilization breakpoint; load the preloaded sweep | `sc_break_run`, `sc_break_detect`, `sc_break_story`, `sc_load_preload_breakpoint` |
| `app/R/spacecraft_breakpoint_page.R` | Breakpoint page UI, charts, table, progress/Cancel server | `sc_breakpoint_ui`, `sc_plot_break_throughput`, `sc_plot_break_cycle`, `sc_plot_break_util`, `sc_breakpoint_server` |
| `app/data/preload_breakpoint.rds` | Saved 20-rep demand sweep (seed 42) shown instantly on Breakpoint | rebuilt by `scripts/preload_breakpoint.R` |
| `scripts/preload_breakpoint.R` | Rebuild the preloaded demand sweep | — |
| `app/R/spacecraft_optimize.R` | Optimize search + recheck bundle, net scores, paired change, story, preload load. Default cost is 1.0 per extra station and 0.25 per extra technician. Algorithm settings sit in a collapsed accordion. | `sc_opt_run`, `sc_opt_attach_paired`, `sc_opt_story`, `sc_opt_default_cost`, `sc_opt_default_tech_cost`, `sc_load_preload_optimize` |
| `app/R/spacecraft_optimize_page.R` | Optimize page UI, chart, table, progress/Cancel server | `sc_optimize_ui`, `sc_plot_opt_history`, `sc_table_opt`, `sc_optimize_server` |
| `app/data/preload_optimize.rds` | Saved Quick search with a 20-rep recheck (seed 42) shown instantly on Optimize | rebuilt by `scripts/preload_optimize.R` |
| `scripts/preload_optimize.R` | Rebuild the preloaded optimize result | — |
| `app/R/spacecraft_warmup.R` | Warm-up check: Welch smooth of cycle time and WIP, suggested discard, KPI change, preload load | `sc_wu_run`, `sc_wu_recommend_weeks`, `sc_wu_story`, `sc_load_preload_warmup` |
| `app/R/spacecraft_warmup_page.R` | Warmup page UI, charts, table, progress/Cancel server | `sc_warmup_ui`, `sc_plot_wu_cycle`, `sc_plot_wu_wip`, `sc_table_wu`, `sc_warmup_server` |
| `app/data/preload_warmup.rds` | Saved 20-rep warm-up check (seed 42) shown instantly on Warmup | rebuilt by `scripts/preload_warmup.R` |
| `scripts/preload_warmup.R` | Rebuild the preloaded warm-up check | — |
| `app/R/spacecraft_ramp.R` | Capacity-ramp planner: target rate curve, Wright/Crawford learning, lead times, meets-target-rate check, recommended moves | `sc_ramp_run`, `sc_ramp_recommend`, `sc_ramp_story`, `sc_load_preload_ramp` |
| `app/R/spacecraft_ramp_page.R` | Ramp page UI, Gantt, throughput / cycle / HPU / utilization / WIP charts, plan and delay tables, progress/Cancel server | `sc_ramp_ui`, `sc_plot_ramp_throughput`, `sc_plot_ramp_gantt`, `sc_table_ramp_plan`, `sc_ramp_server` |
| `app/data/preload_ramp.rds` | Saved lean-line 12-to-48 ramp plan (10 replications, seed 42) shown instantly on Ramp | rebuilt by `scripts/preload_ramp.R` |
| `scripts/preload_ramp.R` | Rebuild the preloaded ramp plan | — |
| `app/R/spacecraft_server.R` | Reactive wiring: params, runs, KPIs, plots, Compare, Results, Tornado, Breakpoint, Optimize, Warmup, Ramp | `sc_server` |
| `tests/spacecraft_checks.R` | Model acceptance checks | — |
| `tests/helpers/spacecraft_expected.R` | Test-only expected cycle time from an absorbing Markov chain | `sc_expected_cycle_h` |
| `tests/app_checks.R` | Shiny smoke checks, plus Settings: Save updates the sidebar, a sidebar change updates Settings, variant edits persist and reach the params, and the sync settles with no echo | — |
| `scripts/screenshots.R` | Headless page screenshots from a running app (`<port> [width] [prefix]`) | — |

## Chart → function

| Chart title | Function |
|-------------|----------|
| Results > Cycle time (all-units histogram with per-rep densities, per-step breakdown, by variant, over time) | `sc_plot_cycle_hist_reps`, `sc_table_cycle`, `sc_plot_analyze_breakdown`, `sc_plot_cycle_dist`, `sc_plot_cycle_over_time` |
| Results > Labor (HPU table + plot, labor utilization) | `sc_table_hpu`, `sc_plot_hpu_dist`, `sc_plot_labor_util` |
| Results > WIP & flow | `sc_plot_wip`, `sc_plot_demand` |
| Results > Bottleneck (where the time goes, utilization and downtime mean + 10–90 with the bottleneck called out, queues, why stations wait as dodged horizontal bars, with starved in a free-x panel) | `sc_plot_analyze_breakdown`, `sc_plot_resource_spread`, `sc_plot_analyze_queues`, `sc_plot_wait_reasons`, `sc_table_analyze_util` |
| Results > Quality (rework, scrap and fails by origin, with 10–90 bars) | `sc_quality_by_rep`, `sc_plot_quality_rate`, `sc_table_quality` |
| Results > Reliability (observed vs configured MTBF and MTTR, failure and repair counts) | `sc_reliability_observed`, `sc_plot_reliability_obs`, `sc_table_reliability_obs` |
| Results / Compare CSV downloads | `sc_export_tables`, `sc_download_buttons_ui` |
| Results > Summary | `sc_format_summary_table`, `sc_summary_caption` |
| Optimizer convergence | `sc_plot_opt_history` (legacy `sc_plot_ga`) |
| Optimize rechecked candidates | `sc_table_opt` |
| Warmup Cycle Time / WIP / KPIs tabs | `sc_plot_wu_cycle`, `sc_plot_wu_wip`, `sc_table_wu` |
| Ramp plan / timeline / throughput / utilization | `sc_table_ramp_plan`, `sc_table_ramp_moves`, `sc_plot_ramp_gantt`, `sc_plot_ramp_throughput`, `sc_plot_ramp_metric`, `sc_plot_ramp_util`, `sc_table_ramp_delay` |
| Compare what each move buys (throughput gain, then HPU / WIP / cycle time) | `sc_plot_compare_gains`, `sc_table_compare_gains` |
| Compare results (mean and delta vs baseline) | `sc_table_compare` |
| Compare spread across replications, in order: throughput mean bars with a 10–90 whisker, cycle-time histogram, touch-labor histogram, downtime, WIP-over-time flow, downloads | `sc_plotly_throughput_bars`, `sc_plotly_overlap_hist`, `sc_plot_compare_downtime`, `sc_plot_compare_wip_flow`, `sc_compare_export_tables` |
| Compare Flow (WIP over time only) and Downtime (horizontal mean + 10–90, no points) | `sc_plot_compare_wip_flow`, `sc_plot_compare_downtime` |
| Results > By variant (cycle time and touch labor overlapping histograms) and Summary > Throughput (mean bars with a 10–90 whisker); Bottleneck still uses the diagnostic plots | `sc_plot_overlap_hist`, `sc_plot_throughput_bars`, `sc_plot_downtime`, `sc_plot_analyze_breakdown`, `sc_plot_analyze_util`, `sc_plot_analyze_queues`, `sc_plot_wait_reasons` |
| Tornado sensitivity (filled dark-blue circle, filled orange triangle, baseline dashed line) | `sc_plot_tornado` |
| Tornado results (low, high, swing; baseline in the caption) | `sc_table_tornado` |
| Breakpoint Throughput / Cycle Time / Saturation tabs | `sc_plot_break_throughput`, `sc_plot_break_cycle`, `sc_plot_break_util`, `sc_table_break` |

## The flow picture

Simulate and Settings > Variants both draw the same inline SVG from `app/R/spacecraft_flow.R`
(`sc_flow_ui()` / `sc_flow_svg()`). Edit that file to change the diagram.
`docs/flow.mmd` is the original box-and-arrow layout the SVG still follows;
the PNG under `app/www/` is kept only as a record of that render.


Screenshot scripts: `scripts/screenshots.R` (all pages), `scripts/screenshots_mrb.R`, `scripts/screenshots_tables.R`, `scripts/screenshots_flow.R`, `scripts/screenshots_compare.R`, `scripts/screenshots_tornado.R`, `scripts/screenshots_breakpoint.R`, `scripts/screenshots_optimize.R`, `scripts/screenshots_warmup.R`, `scripts/screenshots_reliability.R`, `scripts/screenshots_labor.R`, `scripts/screenshots_ramp.R`, `scripts/screenshots_results_compare.R`.

