# spacecraft_plots.R
# Charts for the Simulate and Optimize pages. All use theme_sc() (base size 16,
# legend at the bottom in one row), large points and dashed reference lines.

sc_plot_cycle <- function(units, params) {
  d <- units %>%
    dplyr::filter(status == "shipped") %>%
    dplyr::mutate(variant = factor(params$variants$variant[variant], levels = params$variants$variant))
  due <- tibble::tibble(variant = factor(params$variants$variant, levels = params$variants$variant),
                        due = purrr::map_dbl(params$variants$due, "offset_h"))
  ggplot2::ggplot(d, ggplot2::aes(variant, cycle_h, colour = variant)) +
    ggplot2::geom_jitter(width = 0.2, size = 3, alpha = 0.5) +
    ggplot2::stat_summary(fun = mean, geom = "point", size = 6, shape = 18, colour = "#1a1a1a") +
    ggplot2::geom_errorbar(data = due, ggplot2::aes(x = variant, ymin = due, ymax = due), inherit.aes = FALSE,
                           linetype = "dashed", linewidth = 1, width = 0.6) +
    ggplot2::scale_colour_manual(values = sc_variant_colors, name = "Variant") +
    ggplot2::labs(title = "Cycle time by variant", x = NULL, y = "Release to ship (hours)",
                  caption = "Diamond = mean. Dashed line = nominal due-date offset.") +
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

sc_plot_lateness <- function(units, params) {
  d <- units %>%
    dplyr::filter(status == "shipped") %>%
    dplyr::mutate(variant = factor(params$variants$variant[variant], levels = params$variants$variant))
  ggplot2::ggplot(d, ggplot2::aes(lateness_h / 24, variant, colour = variant)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    ggplot2::geom_jitter(height = 0.2, size = 3, alpha = 0.5) +
    ggplot2::scale_colour_manual(values = sc_variant_colors, name = "Variant") +
    ggplot2::labs(title = "Lateness at ship", x = "Days late (negative = early)", y = NULL,
                  caption = "Dashed line = due date.") +
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
