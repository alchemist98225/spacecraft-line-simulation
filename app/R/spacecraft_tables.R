# spacecraft_tables.R
# Simulate-page tables with small pictures inside them (gt):
#   sc_table_cycle()     one row per variant, summary hours + mini histogram
#   sc_table_lateness()  schedule slip at ship, plus a mini histogram
#                        (dashed line at the due date)
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
  d <- sc_variant_values(units, params, cycle_h) %>% sc_hour_summary() %>%
    dplyr::mutate(due = purrr::map_dbl(params$variants$due, "offset_h")[as.integer(variant)])
  br <- sc_breaks(c(unlist(d$values), d$due))
  plots <- purrr::pmap(d, function(variant, values, due, ...) sc_mini_hist(values, br, sc_variant_colors[[as.character(variant)]], due))
  d %>%
    dplyr::transmute(variant = as.character(variant), n, mean, median, p90, due, shape = "") %>%
    gt::gt() %>%
    gt::fmt_number(c(mean, median, p90, due), decimals = 0) %>%
    gt::sub_missing(missing_text = "–") %>%
    gt::cols_label(variant = "Variant", n = "Units shipped", mean = "Mean (hours)", median = "Median (hours)",
                   p90 = "90% ship within (hours)", due = "Allowed before due date (hours)",
                   shape = "Spread (dashed = due date)") %>%
    sc_gt_plots(shape, plots) %>%
    gt::cols_width(shape ~ gt::px(260)) %>%
    gt::tab_source_note("Cycle time = release into the line to ship. Every row uses the same bins and axis, so shapes compare directly.") %>%
    sc_gt_style()
}

# Schedule slip at ship (internal column still lateness_h). Negative means early.
sc_table_lateness <- function(units, params) {
  d <- sc_variant_values(units, params, lateness_h) %>% sc_hour_summary() %>%
    dplyr::mutate(late_pct = purrr::map_dbl(values, ~ if (length(.x)) 100 * mean(.x > 0) else NA_real_))
  br <- sc_breaks(c(unlist(d$values), 0))
  col <- function(v) sc_variant_colors[[as.character(v)]]
  hists <- purrr::map2(d$values, d$variant, ~ sc_mini_hist(.x, br, col(.y), 0))
  d %>%
    dplyr::transmute(variant = as.character(variant), n, mean, median, p90, late_pct, shape = "") %>%
    gt::gt() %>%
    gt::fmt_number(c(mean, median, p90), decimals = 0) %>%
    gt::fmt_number(late_pct, decimals = 0, pattern = "{x}%") %>%
    gt::sub_missing(missing_text = "–") %>%
    gt::cols_label(variant = "Variant", n = "Units shipped",
                   mean = "Mean (hours)", median = "Median (hours)",
                   p90 = "90% ship by (hours of slip)", late_pct = "Shipped after due date",
                   shape = "Spread (dashed = due date)") %>%
    sc_gt_plots(shape, hists) %>%
    gt::cols_width(shape ~ gt::px(260)) %>%
    gt::tab_source_note("Schedule slip is hours after the due date at ship; negative means early. All rows share one scale.") %>%
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
  on_time_pct = "On Time Percentage",
  lateness_mean_h = "Mean Schedule Slip",
  cycle_mean_h = "Mean Cycle Time",
  cycle_A_h = "Variant A Cycle Time",
  cycle_B_h = "Variant B Cycle Time",
  cycle_C_h = "Variant C Cycle Time"
)

sc_format_summary_table <- function(summary) {
  labs <- sc_summary_labels()
  summary %>%
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
    "Chamber Utilization and Scrap Rate are fractions from 0 to 1 (scrap is among finished units).",
    "On Time Percentage is the share of shipped units that met their due date, on a 0 to 100 scale.",
    "Cycle times are in hours from release to ship.",
    "Mean Schedule Slip is hours after the due date at ship; a negative value means the unit shipped early."
  )
}
