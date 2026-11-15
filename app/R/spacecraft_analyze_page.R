# spacecraft_analyze_page.R
# Analyze plot helpers (used by Results): why the line performs as it does for the current Settings
# (or the last Simulate run). Instant from a preloaded baseline; live runs
# use a cooperative progress bar and Cancel, like Compare.

# 100% stacked bars of cycle-time share, one row per variant plus Overall.
sc_plot_analyze_breakdown <- function(breakdown, narrow = FALSE) {
  parts <- sc_time_parts()
  fill_col <- if (isTRUE(narrow)) "short" else "label"
  fill_levels <- if (isTRUE(narrow)) parts$short else parts$label
  pal <- stats::setNames(parts$color, fill_levels)
  d <- breakdown %>%
    dplyr::select(-dplyr::any_of(c("label", "short", "color", "tip"))) %>%
    dplyr::left_join(parts, by = "part") %>%
    dplyr::mutate(
      variant = factor(.data$variant, levels = rev(levels(.data$variant))),
      fill = factor(.data[[fill_col]], levels = fill_levels)
    )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = share, y = variant, fill = fill)) +
    ggplot2::geom_col(width = 0.72, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 4),
      expand = ggplot2::expansion(mult = c(0, if (isTRUE(narrow)) 0.12 else 0.04))
    ) +
    ggplot2::scale_fill_manual(values = pal, breaks = fill_levels, name = NULL) +
    ggplot2::labs(x = "Share of cycle time", y = NULL) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (isTRUE(narrow)) {
    # Extra right margin and scale expansion so the last tick ("100%")
    # is not clipped at phone width. A two-row legend still cuts "Rework"
    # at this width and base size 16, so keep three rows.
    p <- p +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 14, 5.5, 5.5)) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3, byrow = TRUE, title = NULL))
  } else {
    # Eight full labels do not fit in one row at desktop width (the last
    # entry, "Down for repair", was clipped). Two rows keep every label.
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE, title = NULL))
  }
  p
}

# Hours and share for each part, one row per variant.
sc_table_analyze_breakdown <- function(breakdown) {
  parts <- sc_time_parts()
  variant_levels <- c("Overall", setdiff(levels(breakdown$variant), "Overall"))
  cell <- function(h, share) sprintf("%.1f h (%.0f%%)", h, 100 * share)
  wide <- breakdown %>%
    dplyr::select(-dplyr::any_of(c("label", "short", "color", "tip"))) %>%
    dplyr::left_join(parts, by = "part") %>%
    dplyr::mutate(cell = cell(.data$mean_h, .data$share),
                  variant = factor(as.character(.data$variant), levels = variant_levels)) %>%
    dplyr::select(variant, label, cell, cycle_h) %>%
    tidyr::pivot_wider(names_from = label, values_from = cell) %>%
    dplyr::arrange(.data$variant) %>%
    dplyr::mutate(
      variant = as.character(.data$variant),
      `Cycle time` = sprintf("%.1f h", .data$cycle_h)
    ) %>%
    dplyr::select(Variant = variant, dplyr::all_of(parts$label), `Cycle time`)
  tips <- c(
    stats::setNames(
      lapply(seq_len(nrow(parts)), function(i) {
        gt::html(sprintf('<span title="%s">%s</span>',
                         htmltools::htmlEscape(parts$tip[[i]]),
                         htmltools::htmlEscape(parts$label[[i]])))
      }),
      parts$label
    ),
    list(`Cycle time` = "Cycle time")
  )
  wide %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_width(Variant ~ gt::px(90)) %>%
    gt::cols_align("right", columns = -Variant) %>%
    gt::cols_label(.list = tips) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4)) %>%
    gt::tab_source_note("Each cell is the mean across shipped units and replications. Hover a column name for a short description.")
}

# Utilization bars; the bottleneck is highlighted, queue is written on the bar,
# and a dashed line marks 100% busy.
sc_plot_analyze_util <- function(resources, narrow = FALSE) {
  d <- resources %>%
    dplyr::mutate(
      down_frac = dplyr::coalesce(.data$downtime_fraction, 0),
      prod = pmax(0, .data$util - .data$down_frac),
      txt = sprintf("%.0f%% busy, queue %.2f", 100 * .data$util, .data$queue_avg),
      y_lab = if (isTRUE(narrow)) .data$short else .data$label
    ) %>%
    dplyr::arrange(.data$util)
  y_levels <- d$y_lab
  long <- d %>%
    dplyr::select(y_lab, prod, down_frac, is_bottleneck, txt) %>%
    tidyr::pivot_longer(c(prod, down_frac), names_to = "part", values_to = "share") %>%
    dplyr::mutate(
      y = factor(.data$y_lab, levels = y_levels),
      part = factor(.data$part, levels = c("prod", "down_frac"),
                    labels = c("Busy (work)", "Down for repair"))
    )
  xmax <- if (isTRUE(narrow)) 1.05 else max(1.05, max(d$util) + 0.42)
  pal <- c("Busy (work)" = "#2c7fb8", "Down for repair" = "#c0392b")
  p <- ggplot2::ggplot(long, ggplot2::aes(x = share, y = y, fill = part)) +
    ggplot2::geom_col(width = 0.7, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1, colour = "#1a1a1a") +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = scales::breaks_pretty(n = if (isTRUE(narrow)) 3 else 4),
      limits = c(0, xmax),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::labs(x = "Utilization", y = NULL) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
    sc_one_row_legend()
  if (!isTRUE(narrow)) {
    lab <- d %>% dplyr::mutate(y = factor(.data$y_lab, levels = y_levels))
    p <- p + ggplot2::geom_text(
      data = lab, ggplot2::aes(x = util, y = y, label = txt),
      inherit.aes = FALSE, hjust = -0.05,
      size = 16 / ggplot2::.pt, colour = "#1a1a1a"
    )
  }
  p
}


# Utilization and average queue, with the bottleneck row highlighted.
sc_table_analyze_util <- function(resources, narrow = FALSE) {
  name_col <- if (isTRUE(narrow)) "short" else "label"
  util_nm <- if (isTRUE(narrow)) "Util." else "Utilization"
  queue_nm <- if (isTRUE(narrow)) "Queue" else "Average queue (units)"
  d <- resources %>%
    dplyr::arrange(dplyr::desc(.data$util)) %>%
    dplyr::transmute(
      Station = .data[[name_col]],
      util_l = sprintf("%.0f%%", 100 * .data$util),
      down_l = sprintf("%.1f%%", 100 * dplyr::coalesce(.data$downtime_fraction, 0)),
      avail_l = sprintf("%.1f%%", 100 * dplyr::coalesce(.data$availability, 1)),
      repairs_l = sprintf("%.1f", dplyr::coalesce(.data$n_repairs, 0)),
      queue_l = sprintf("%.2f", .data$queue_avg),
      is_bottleneck
    )
  names(d)[names(d) == "util_l"] <- util_nm
  names(d)[names(d) == "down_l"] <- "Downtime"
  names(d)[names(d) == "avail_l"] <- "Availability"
  names(d)[names(d) == "repairs_l"] <- "Repairs"
  names(d)[names(d) == "queue_l"] <- queue_nm
  g <- d %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_hide(is_bottleneck)
  g <- if (isTRUE(narrow)) {
    gt::cols_width(g, Station ~ gt::px(110))
  } else {
    gt::cols_width(g, Station ~ gt::px(280))
  }
  g %>%
    gt::cols_align("right", columns = dplyr::all_of(c(util_nm, "Downtime", "Availability", "Repairs", queue_nm))) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#fce4c2"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = .data$is_bottleneck)
    ) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4)) %>%
    gt::tab_source_note("The highlighted row is the busiest station. Utilization is the time-average share of servers that are occupied (including units held during a repair). Downtime is the calendar fraction of capacity-hours spent in repair. Availability is processing time divided by processing plus repair (operational). Repairs are the mean count per replication.")
}

# Queue length over the horizon for the top queues: mean line and a P10–P90 ribbon.
sc_plot_analyze_queues <- function(queues, narrow = FALSE) {
  cat <- sc_resource_catalog()
  pal <- c(Chamber = "#2c7fb8", Integration = "#1b9e77", `Func. test` = "#d95f02",
           MRB = "#e0a032", Acceptance = "#756bb1")
  d <- queues %>%
    dplyr::mutate(
      day = .data$time_h / 24,
      series = factor(.data$short, levels = unique(.data$short[order(.data$resource)]))
    )
  used <- levels(d$series)
  ggplot2::ggplot(d, ggplot2::aes(x = day, y = mean, colour = series, fill = series)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p10, ymax = p90), alpha = 0.22, colour = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = pal[used], breaks = used, name = NULL) +
    ggplot2::scale_fill_manual(values = pal[used], breaks = used, name = NULL) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 4),
                                expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
    ggplot2::labs(x = "Day", y = "Queue (units)") +
    theme_sc() +
    if (isTRUE(narrow)) {
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, title = NULL),
                      fill = ggplot2::guide_legend(ncol = 1, title = NULL))
    } else {
      sc_one_row_legend()
    }
}

sc_analyze_status_ui <- function(state) {
  cls <- switch(state$kind,
                running = "alert alert-info py-2",
                done = "alert alert-success py-2",
                cancelled = "alert alert-warning py-2",
                error = "alert alert-danger py-2",
                "alert alert-light py-2")
  bar <- if (is.null(state$pct)) NULL else div(
    class = "progress mt-2 mb-0", style = "height: 0.7rem;",
    div(class = "progress-bar", role = "progressbar",
        style = sprintf("width: %.0f%%; background-color: #2c7fb8;", state$pct))
  )
  div(class = cls, p(class = "mb-0", state$text), bar)
}

# The Analyze navbar tab was retired. Unique views (cycle time and touch labor
# by variant, and throughput) now live under Results. The plot and table
# helpers above stay here because Results still calls them.

