# spacecraft_tables.R
# Simulate-page tables with small pictures inside them (gt + gtExtras):
#   sc_table_cycle()     one row per variant, summary hours + mini histogram
#   sc_table_lateness()  same for lateness, plus a bar showing median and mean
#                        against the due date
#   sc_table_kpi_reps()  one row per KPI, mean / lowest / highest + sparkline of
#                        the value in each replication
# The pictures are tiny ggplots dropped into cells with gt::text_transform().

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

# Bar from the due date (0) to the median, dot at the mean, dashed due date.
sc_mini_bullet <- function(median, mean, lims, colour) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = min(0, median), xmax = max(0, median), ymin = 0.15, ymax = 0.85,
                      fill = colour, alpha = 0.8) +
    ggplot2::annotate("point", x = mean, y = 0.5, size = 22, shape = 23, fill = "white", colour = "black", stroke = 3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 3) +
    ggplot2::coord_cartesian(xlim = lims, ylim = c(0, 1), expand = FALSE) + ggplot2::theme_void()
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

sc_table_lateness <- function(units, params) {
  d <- sc_variant_values(units, params, lateness_h) %>% sc_hour_summary() %>%
    dplyr::mutate(late_pct = purrr::map_dbl(values, ~ if (length(.x)) 100 * mean(.x > 0) else NA_real_))
  br <- sc_breaks(c(unlist(d$values), 0))
  m <- max(abs(c(d$median, d$mean)), 1, na.rm = TRUE) * 1.25
  lims <- c(-m, m)
  col <- function(v) sc_variant_colors[[as.character(v)]]
  hists <- purrr::map2(d$values, d$variant, ~ sc_mini_hist(.x, br, col(.y), 0))
  bullets <- purrr::pmap(d, function(variant, median, mean, ...) {
    if (is.na(median)) return(ggplot2::ggplot() + ggplot2::theme_void())
    sc_mini_bullet(median, mean, lims, col(variant))
  })
  d %>%
    dplyr::transmute(variant = as.character(variant), n, mean, median, p90, late_pct, shape = "", vs_due = "") %>%
    gt::gt() %>%
    gt::fmt_number(c(mean, median, p90), decimals = 0) %>%
    gt::fmt_number(late_pct, decimals = 0, pattern = "{x}%") %>%
    gt::sub_missing(missing_text = "–") %>%
    gt::cols_label(variant = "Variant", n = "Units shipped", mean = "Mean (hours late)", median = "Median (hours late)",
                   p90 = "90% ship by (hours late)", late_pct = "Shipped late",
                   shape = "Spread (dashed = due date)", vs_due = "Median bar and mean diamond (left of dashed due date = early)") %>%
    sc_gt_plots(shape, hists) %>%
    sc_gt_plots(vs_due, bullets) %>%
    gt::cols_width(c(shape, vs_due) ~ gt::px(240)) %>%
    gt::tab_source_note("Hours late: negative means early. The bar runs from the due date to the median; the diamond is the mean. All rows share one scale.") %>%
    sc_gt_style()
}

# KPI rows: name, unit, how to pull it from the per-replication table.
sc_kpi_rows <- function() tibble::tribble(
  ~kpi,                  ~unit,     ~col,            ~scale,
  "Mean cycle time",     "hours",   "cycle_mean_h",  1,
  "On time",             "%",       "on_time_pct",   1,
  "Average WIP",         "units",   "wip_avg",       1,
  "Chamber utilization", "%",       "chamber_util",  100,
  "MRB queue time",      "hours",   "mrb_q_mean_h",  1,
  "Scrap rate",          "%",       "scrap_rate",    100
)

sc_table_kpi_reps <- function(per_rep) {
  d <- sc_kpi_rows() %>%
    dplyr::mutate(values = purrr::map2(col, scale, ~ per_rep[[.x]] * .y),
                  mean = purrr::map_dbl(values, ~ mean(.x, na.rm = TRUE)),
                  low = purrr::map_dbl(values, ~ suppressWarnings(min(.x, na.rm = TRUE))),
                  high = purrr::map_dbl(values, ~ suppressWarnings(max(.x, na.rm = TRUE))),
                  values = purrr::map(values, ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::select(kpi, unit, mean, low, high, values)
  d %>%
    gt::gt() %>%
    gt::fmt_number(c(mean, low, high), decimals = 1) %>%
    gt::cols_label(kpi = "KPI", unit = "Unit", mean = "Mean", low = "Lowest", high = "Highest",
                   values = sprintf("Each replication, 1 to %d", nrow(per_rep))) %>%
    gtExtras::gt_plt_sparkline(values, type = "points", palette = c("#2c7fb8", "black", "#d95f02", "#1b9e77", "lightgrey"),
                               same_limit = FALSE, label = FALSE, fig_dim = c(10, 40)) %>%
    gt::tab_source_note("Mean, lowest and highest across replications. The sparkline shows each replication's value in run order: orange = lowest, green = highest.") %>%
    sc_gt_style()
}
