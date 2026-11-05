# spacecraft_plots.R
# Charts for the Simulate and Optimize pages (cycle time, lateness and KPIs by
# replication are tables now: see spacecraft_tables.R). All use theme_sc() (base size 16,
# legend at the bottom in one row), large points and dashed reference lines.

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
