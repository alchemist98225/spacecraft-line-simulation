# spacecraft_plots.R
# Charts for the Simulate and Optimize pages (cycle time, schedule slip and KPIs by
# replication are tables now: see spacecraft_tables.R). All use theme_sc() (base size 16,
# legend at the bottom in one row), large points and dashed reference lines.

# All-replication WIP: faint per-rep traces and a bold mean, plus a dashed
# time-average of the mean path.
sc_plot_wip <- function(units, horizon, grid = NULL) {
  grid <- grid %||% sc_wip_grid(units, horizon)
  if (is.null(grid) || !nrow(grid)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No WIP to plot."))
  }
  avg_path <- grid %>%
    dplyr::group_by(time_h) %>%
    dplyr::summarise(wip = mean(.data$wip), .groups = "drop")
  avg <- sc_time_average(avg_path$time_h, avg_path$wip, horizon)
  n_rep <- dplyr::n_distinct(grid$rep)
  ggplot2::ggplot(grid, ggplot2::aes(x = time_h / 24, y = wip)) +
    ggplot2::geom_line(ggplot2::aes(group = rep), alpha = 0.22, linewidth = 0.4,
                       colour = "#2c7fb8", na.rm = TRUE) +
    ggplot2::geom_line(data = avg_path, linewidth = 1.2, colour = "#1b4f72") +
    ggplot2::geom_hline(yintercept = avg, linetype = "dashed", linewidth = 1, colour = "#1a1a1a") +
    ggplot2::labs(
      title = NULL, x = "Day", y = "WIP (units)",
      caption = sprintf(
        "Faint lines = each of %d replications; bold = mean; dashed = time-average of the mean (%.1f).",
        n_rep, avg
      )
    ) +
    theme_sc()
}

# Cumulative releases versus units that have left the line (shipped or scrapped).
# The gap is unfinished work (WIP), not a shop-floor backlog of orders sitting
# outside the line. A second panel shows that gap on its own scale, because at
# low volume it is only a few units on a cumulative axis of a hundred or more.
sc_plot_demand <- function(units, horizon, grid = NULL, sets = NULL) {
  grid <- grid %||% sc_demand_grid(units, horizon)
  if (is.null(grid) || !nrow(grid)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No release or completion series to plot."))
  }
  panel_lvls <- c("Cumulative units", "Unfinished (gap)")
  if (!"set" %in% names(grid)) {
    summ <- grid %>%
      dplyr::group_by(time_h) %>%
      dplyr::summarise(
        Demand = mean(.data$demand),
        Completed = mean(.data$completed),
        Unfinished = mean(.data$demand - .data$completed),
        .groups = "drop"
      )
    cum <- summ %>%
      tidyr::pivot_longer(c(Demand, Completed), names_to = "series", values_to = "n_units") %>%
      dplyr::mutate(
        series = factor(series, levels = c("Demand", "Completed", "Unfinished")),
        panel = factor("Cumulative units", levels = panel_lvls)
      )
    gap <- summ %>%
      dplyr::transmute(
        time_h,
        series = factor("Unfinished", levels = c("Demand", "Completed", "Unfinished")),
        n_units = Unfinished,
        panel = factor("Unfinished (gap)", levels = panel_lvls)
      )
    pal <- c(Demand = "#6c757d", Completed = "#2c7fb8", Unfinished = "#c0392b")
    lty <- c(Demand = "dashed", Completed = "solid", Unfinished = "solid")
    p <- ggplot2::ggplot(dplyr::bind_rows(cum, gap),
                         ggplot2::aes(x = time_h / 24, y = n_units,
                                      colour = series, linetype = series)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::scale_colour_manual(values = pal, name = NULL, drop = FALSE) +
      ggplot2::scale_linetype_manual(values = lty, name = NULL, drop = FALSE)
  } else {
    summ <- grid %>%
      dplyr::group_by(set, time_h) %>%
      dplyr::summarise(
        Demand = mean(.data$demand),
        Completed = mean(.data$completed),
        Unfinished = mean(.data$demand - .data$completed),
        .groups = "drop"
      )
    pal <- c("#2c7fb8", "#d95f02", "#1b9e77", "#756bb1")
    sets <- unique(as.character(summ$set))
    pal <- stats::setNames(pal[seq_along(sets)], sets)
    dem <- summ %>%
      dplyr::transmute(set, time_h, n_units = Demand,
                       panel = factor("Cumulative units", levels = panel_lvls))
    comp <- summ %>%
      dplyr::transmute(set, time_h, n_units = Completed,
                       panel = factor("Cumulative units", levels = panel_lvls))
    gap <- summ %>%
      dplyr::transmute(set, time_h, n_units = Unfinished,
                       panel = factor("Unfinished (gap)", levels = panel_lvls))
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = dem, ggplot2::aes(x = time_h / 24, y = n_units, group = set),
        colour = "#6c757d", linetype = "dashed", linewidth = 1.05
      ) +
      ggplot2::geom_line(
        data = dplyr::bind_rows(comp, gap),
        ggplot2::aes(x = time_h / 24, y = n_units, colour = set),
        linewidth = 1.2
      ) +
      ggplot2::scale_colour_manual(values = pal, name = NULL)
  }
  p +
    ggplot2::facet_grid(panel ~ ., scales = "free_y") +
    ggplot2::labs(
      x = "Day", y = NULL,
      caption = sc_caption("Demand (dashed) = units released. Completed (solid) = shipped or scrapped. The unfinished panel is the gap: work still in the line. Lines are means across replications.")
    ) +
    theme_sc() +
    sc_one_row_legend()
}

# Two-scenario WIP overlay (faint traces + bold mean per set).
sc_plot_compare_wip_flow <- function(grid, narrow = FALSE) {
  if (is.null(grid) || !nrow(grid)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No WIP series to plot."))
  }
  pal <- c("#2c7fb8", "#d95f02", "#1b9e77", "#756bb1")
  sets <- unique(as.character(grid$set))
  pal <- stats::setNames(pal[seq_along(sets)], sets)
  grid <- grid %>% dplyr::mutate(set = factor(as.character(.data$set), levels = sets))
  avg <- grid %>%
    dplyr::group_by(set, time_h) %>%
    dplyr::summarise(wip = mean(.data$wip), .groups = "drop")
  ggplot2::ggplot(grid, ggplot2::aes(x = time_h / 24, y = wip, colour = set, group = interaction(set, rep))) +
    ggplot2::geom_line(alpha = 0.18, linewidth = 0.4, na.rm = TRUE) +
    ggplot2::geom_line(data = avg, ggplot2::aes(group = set), linewidth = 1.2) +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::labs(
      x = "Day", y = "WIP (units)",
      caption = "Faint lines = each replication; bold = mean across replications."
    ) +
    theme_sc() +
    sc_one_row_legend()
}

# Two-scenario queue overlay, faceted by station.
sc_plot_compare_queue_flow <- function(grid, narrow = FALSE) {
  if (is.null(grid) || !nrow(grid)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No queue series to plot."))
  }
  pal <- c("#2c7fb8", "#d95f02", "#1b9e77", "#756bb1")
  sets <- unique(as.character(grid$set))
  pal <- stats::setNames(pal[seq_along(sets)], sets)
  grid <- grid %>% dplyr::mutate(set = factor(as.character(.data$set), levels = sets))
  avg <- grid %>%
    dplyr::group_by(set, short, time_h) %>%
    dplyr::summarise(queue = mean(.data$queue), .groups = "drop")
  ggplot2::ggplot(avg, ggplot2::aes(x = time_h / 24, y = queue, colour = set)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::facet_wrap(~short, ncol = if (isTRUE(narrow)) 1 else 2, scales = "free_y") +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::labs(
      x = "Day", y = "Queue (units)",
      caption = "Mean queue across replications, by station."
    ) +
    theme_sc() +
    sc_one_row_legend()
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
