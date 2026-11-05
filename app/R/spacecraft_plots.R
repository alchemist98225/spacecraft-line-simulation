# spacecraft_plots.R
# Charts for the Simulate and Optimize pages. All use theme_sc() (base size 16,
# legend at the bottom in one row), large points and dashed reference lines.

# Histogram per variant (one facet each, shared x axis) so the shapes and the
# due-date reference line can be compared top to bottom.
sc_plot_cycle <- function(units, params) {
  lv <- params$variants$variant
  d <- units %>%
    dplyr::filter(status == "shipped") %>%
    dplyr::mutate(variant = factor(lv[variant], levels = lv))
  due <- tibble::tibble(variant = factor(lv, levels = lv),
                        due = purrr::map_dbl(params$variants$due, "offset_h"))
  ggplot2::ggplot(d, ggplot2::aes(cycle_h, fill = variant)) +
    ggplot2::geom_histogram(bins = 30, colour = "white", alpha = 0.9) +
    ggplot2::geom_vline(data = due, ggplot2::aes(xintercept = due), linetype = "dashed", linewidth = 1) +
    ggplot2::facet_wrap(~variant, ncol = 1, labeller = ggplot2::labeller(variant = function(x) paste("Variant", x))) +
    ggplot2::scale_fill_manual(values = sc_variant_colors, name = "Variant") +
    ggplot2::labs(title = "Cycle time by variant", x = "Release to ship (hours)", y = "Units",
                  caption = "Dashed line = time allowed before the due date for that variant.") +
    theme_sc() + sc_one_row_legend()
}

sc_plot_wip <- function(units, horizon) {
  r1 <- units %>% dplyr::filter(rep == min(rep))
  ev <- dplyr::bind_rows(
    tibble::tibble(time = r1$start_time, d = 1),
    tibble::tibble(time = r1$end_time[r1$status != "wip"], d = -1)
  ) %>% dplyr::arrange(time) %>% dplyr::mutate(wip = cumsum(d))
  avg <- sc_time_average(ev$time, ev$wip, horizon)
  ggplot2::ggplot(ev, ggplot2::aes(time / 24, wip)) +
    ggplot2::geom_step(colour = "#2c7fb8", linewidth = 1) +
    ggplot2::geom_hline(yintercept = avg, linetype = "dashed", linewidth = 1) +
    ggplot2::labs(title = "Units in the line (replication 1)", x = "Day", y = "WIP (units)",
                  caption = sprintf("Dashed line = time-average WIP (%.1f).", avg)) +
    theme_sc()
}

# Histogram of lateness per variant; the dashed line at zero is the due date.
sc_plot_lateness <- function(units, params) {
  lv <- params$variants$variant
  d <- units %>%
    dplyr::filter(status == "shipped") %>%
    dplyr::mutate(variant = factor(lv[variant], levels = lv))
  ggplot2::ggplot(d, ggplot2::aes(lateness_h / 24, fill = variant)) +
    ggplot2::geom_histogram(bins = 30, colour = "white", alpha = 0.9) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    ggplot2::facet_wrap(~variant, ncol = 1, labeller = ggplot2::labeller(variant = function(x) paste("Variant", x))) +
    ggplot2::scale_fill_manual(values = sc_variant_colors, name = "Variant") +
    ggplot2::labs(title = "Lateness at ship", x = "Days late (negative = early)", y = "Units",
                  caption = "Dashed line = due date. Bars to the right of it shipped late.") +
    theme_sc() + sc_one_row_legend()
}

sc_plot_kpi_reps <- function(per_rep) {
  d <- per_rep %>%
    dplyr::select(rep, `Chamber utilization %` = chamber_util, `On-time %` = on_time_pct,
                  `Scrap %` = scrap_rate, `Mean WIP` = wip_avg) %>%
    dplyr::mutate(`Chamber utilization %` = 100 * `Chamber utilization %`, `Scrap %` = 100 * `Scrap %`) %>%
    tidyr::pivot_longer(-rep, names_to = "kpi")
  ggplot2::ggplot(d, ggplot2::aes(factor(rep), value)) +
    ggplot2::geom_hline(data = d %>% dplyr::group_by(kpi) %>% dplyr::summarise(m = mean(value)),
                        ggplot2::aes(yintercept = m), linetype = "dashed", linewidth = 1) +
    ggplot2::geom_point(size = 5, colour = "#2c7fb8") +
    ggplot2::facet_wrap(~kpi, scales = "free_y", nrow = 1) +
    ggplot2::labs(title = "KPIs by replication", x = "Replication", y = NULL,
                  caption = "Dashed line = mean across replications.") +
    theme_sc()
}

sc_plot_ga <- function(history) {
  d <- history %>% tidyr::pivot_longer(c(best_fitness, mean_fitness), names_to = "series") %>%
    dplyr::mutate(series = dplyr::recode(series, best_fitness = "Best", mean_fitness = "Mean"))
  ggplot2::ggplot(d, ggplot2::aes(generation, value, colour = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 4) +
    ggplot2::scale_linetype_manual(values = c(Best = "solid", Mean = "dashed"), name = NULL) +
    ggplot2::scale_colour_manual(values = c(Best = "#2c7fb8", Mean = "#d95f02"), name = NULL) +
    ggplot2::labs(title = "Optimizer convergence", x = "Generation", y = "Fitness (lower is better)") +
    theme_sc() + sc_one_row_legend()
}
