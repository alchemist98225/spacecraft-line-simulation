# spacecraft_tables.R
# Simulate-page tables with small pictures inside them (gt):
#   sc_table_cycle()  one row per variant, summary hours + mini histogram
#   sc_table_hpu()    touch labor (HPU) by variant, plus a mini histogram
# The pictures are tiny ggplots dropped into cells with gt::text_transform().
# The old KPIs-by-replication sparkline table (gtExtras + svglite) was removed.

# Shipped units with a variant label, one list-column of values per variant.
sc_variant_values <- function(units, params, col) {
  lv <- params$variants$variant
  units %>%
    dplyr::filter(status == "shipped") %>%
    dplyr::mutate(variant = factor(lv[variant], levels = lv)) %>%
    dplyr::group_by(variant, .drop = FALSE) %>%
    dplyr::summarise(values = list({{ col }}), .groups = "drop")
}

# Summary columns shared by both variant tables.
sc_hour_summary <- function(d) {
  d %>% dplyr::mutate(
    n = purrr::map_int(values, length),
    mean = purrr::map_dbl(values, ~ if (length(.x)) mean(.x) else NA_real_),
    median = purrr::map_dbl(values, ~ if (length(.x)) stats::median(.x) else NA_real_),
    p90 = purrr::map_dbl(values, ~ if (length(.x)) unname(stats::quantile(.x, 0.9)) else NA_real_)
  )
}

# Mini histogram: same bins and x range on every row so shapes compare.
sc_mini_hist <- function(x, breaks, colour, ref = NULL) {
  g <- ggplot2::ggplot(tibble::tibble(x = x), ggplot2::aes(x)) +
    ggplot2::geom_histogram(breaks = breaks, fill = colour, alpha = 0.8, colour = NA)
  if (!is.null(ref)) g <- g + ggplot2::geom_vline(xintercept = ref, linetype = "dashed", linewidth = 3)
  g + ggplot2::coord_cartesian(xlim = range(breaks), expand = FALSE) + ggplot2::theme_void()
}

# Put a list of ggplots into a column as images.
sc_gt_plots <- function(tbl, column, plots, height = 52, aspect = 4.5) {
  gt::text_transform(tbl, locations = gt::cells_body(columns = {{ column }}), fn = function(x) {
    purrr::map_chr(plots, ~ gt::ggplot_image(.x, height = gt::px(height), aspect_ratio = aspect))
  })
}

# Shared look: plain labels, numbers to 0 dp, compact rows.
sc_gt_style <- function(tbl) {
  tbl %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(15),
                    data_row.padding = gt::px(4), column_labels.font.weight = "bold") %>%
    gt::cols_align("left", columns = 1) %>%
    gt::cols_width(1 ~ gt::px(90))
}

sc_breaks <- function(all, n = 25) {
  r <- range(all, na.rm = TRUE); if (!all(is.finite(r))) r <- c(0, 1); if (diff(r) == 0) r <- r + c(-1, 1)
  seq(r[1], r[2], length.out = n + 1)
}

sc_table_cycle <- function(units, params) {
  d <- sc_variant_values(units, params, cycle_h) %>% sc_hour_summary()
  br <- sc_breaks(unlist(d$values))
  plots <- purrr::pmap(d, function(variant, values, ...) sc_mini_hist(values, br, sc_variant_colors[[as.character(variant)]]))
  d %>%
    dplyr::transmute(variant = as.character(variant), n, mean, median, p90, shape = "") %>%
    gt::gt() %>%
    gt::fmt_number(c(mean, median, p90), decimals = 0) %>%
    gt::sub_missing(missing_text = "–") %>%
    gt::cols_label(variant = "Variant", n = "Units shipped", mean = "Mean (hours)", median = "Median (hours)",
                   p90 = "90% ship within (hours)", shape = "Spread") %>%
    sc_gt_plots(shape, plots) %>%
    gt::cols_width(shape ~ gt::px(260)) %>%
    gt::tab_source_note("Cycle time = release into the line to ship. Every row uses the same bins and axis, so shapes compare directly.") %>%
    sc_gt_style()
}

# Touch labor hours per unit (HPU) by variant: technicians × touch time,
# summed over every step including rework.
sc_table_hpu <- function(units, params) {
  d <- sc_variant_values(units, params, labor_hpu) %>% sc_hour_summary()
  br <- sc_breaks(unlist(d$values))
  plots <- purrr::pmap(d, function(variant, values, ...) sc_mini_hist(values, br, sc_variant_colors[[as.character(variant)]]))
  d %>%
    dplyr::transmute(variant = as.character(variant), n, mean, median, p90, shape = "") %>%
    gt::gt() %>%
    gt::fmt_number(c(mean, median, p90), decimals = 0) %>%
    gt::sub_missing(missing_text = "–") %>%
    gt::cols_label(variant = "Variant", n = "Units shipped", mean = "Mean (hours)", median = "Median (hours)",
                   p90 = "90% within (hours)", shape = "Spread") %>%
    sc_gt_plots(shape, plots) %>%
    gt::cols_width(shape ~ gt::px(260)) %>%
    gt::tab_source_note("Touch labor (HPU) is technicians times touch time, summed over every step including rework. Waiting time is not included. Every row uses the same bins and axis, so shapes compare directly.") %>%
    sc_gt_style()
}

# Human labels for the Simulate summary table (internal names stay snake_case).
sc_summary_labels <- function() c(
  arrivals = "Arrivals",
  shipped = "Units Shipped",
  scrapped = "Units Scrapped",
  wip_end = "WIP at End",
  wip_avg = "Average WIP",
  chamber_util = "Chamber Utilization",
  mrb_q_mean_h = "MRB Queue Time",
  scrap_rate = "Scrap Rate",
  cycle_mean_h = "Mean Cycle Time",
  labor_util = "Labor Utilization",
  labor_hpu = "Mean Touch Labor (HPU)",
  tech_wait_mean_h = "Mean Wait for Technicians",
  cycle_A_h = "Variant A Cycle Time",
  cycle_B_h = "Variant B Cycle Time",
  cycle_C_h = "Variant C Cycle Time",
  hpu_A_h = "Variant A Touch Labor (HPU)",
  hpu_B_h = "Variant B Touch Labor (HPU)",
  hpu_C_h = "Variant C Touch Labor (HPU)"
)

# Headline rows first; on-time and schedule slip stay off this table.
sc_summary_display_order <- function() c(
  "shipped", "arrivals", "wip_avg", "wip_end",
  "cycle_mean_h", "cycle_A_h", "cycle_B_h", "cycle_C_h",
  "labor_hpu", "hpu_A_h", "hpu_B_h", "hpu_C_h",
  "labor_util", "tech_wait_mean_h",
  "chamber_util", "mrb_q_mean_h", "scrapped", "scrap_rate"
)

sc_format_summary_table <- function(summary) {
  labs <- sc_summary_labels()
  keep <- intersect(sc_summary_display_order(), names(summary))
  extra <- setdiff(names(summary), c(keep, "on_time_pct", "lateness_mean_h", "labor_util_pct"))
  cols <- c(keep, extra)
  summary[, cols, drop = FALSE] %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "col", values_to = "value") %>%
    dplyr::mutate(Metric = dplyr::coalesce(unname(labs[col]), col)) %>%
    dplyr::select(Metric, Value = value)
}


# Caption for the Simulate summary table: time basis and how the counts relate,
# without putting extra text in the cells.
sc_summary_caption <- function(params = sc_default_params()) {
  hours <- params$sim_hours %||% (24 * 7 * 52)
  default_year <- isTRUE(all.equal(hours, 24 * 7 * 52))
  horizon <- if (default_year) {
    sprintf("%s hours (one year)", format(hours, big.mark = ","))
  } else {
    sprintf("%s hours", format(hours, big.mark = ","))
  }
  paste(
    sprintf("Each value is the mean across replications of that metric over the run horizon, here %s.", horizon),
    "Arrivals counts units released in that period (after any warm-up).",
    "Units Shipped and Units Scrapped are those of that set that finished; WIP at End is how many are still in the line when the clock stops, so the four typically add up (for example about 120 released, 116 shipped, 3 scrapped and 2 still in work on the default line).",
    "Average WIP is the time-average number of units in the line.",
    "Chamber Utilization, Labor Utilization and Scrap Rate are fractions from 0 to 1 (scrap is among finished units). Labor utilization is busy technician-hours divided by scheduled technician-hours.",
    "Cycle times are in hours from release to ship.",
    "Mean Touch Labor (HPU) is technicians times touch time, summed over every step including rework, in hours per shipped unit; variant rows are the same measure for that variant."
  )
}
