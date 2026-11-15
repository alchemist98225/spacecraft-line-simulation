# spacecraft_compare_plotly.R
# Plotly + crosstalk charts for the Compare page. Colours are green / yellow /
# red / cyan, each scenario also has a shape, the legend sits in one row at the
# bottom, and typography is base size 16. Clicking a replication highlights the
# same seed in every metric chart.

sc_compare_set_colors <- function(names) {
  pal <- c("#31a354", "#e0a032", "#c43c3c", "#17becf")
  stats::setNames(pal[seq_along(names)], names)
}

sc_compare_set_shapes <- function(names) {
  sh <- c(16, 17, 15, 18) # circle, triangle, square, diamond
  stats::setNames(sh[seq_along(names)], names)
}

sc_compare_clean_legend <- function(p) {
  # ggplotly can add extra traces named like "(Baseline,1)"; keep one row of plain labels.
  if (is.null(p$x$data)) return(p)
  for (i in seq_along(p$x$data)) {
    nm <- p$x$data[[i]]$name %||% ""
    if (grepl("^\\(", nm) || grepl(",\\s*\\d+\\)$", nm)) {
      p$x$data[[i]]$showlegend <- FALSE
    }
  }
  p
}

sc_compare_plotly_layout <- function(p, ylab, xlab = "") {
  plotly::layout(
    p,
    font = list(size = 16, color = "#1a1a1a"),
    xaxis = list(title = xlab, tickfont = list(size = 16), titlefont = list(size = 16)),
    yaxis = list(title = ylab, tickfont = list(size = 16), titlefont = list(size = 16)),
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.18,
                  font = list(size = 16)),
    margin = list(l = 60, r = 20, t = 20, b = 80),
    hoverlabel = list(font = list(size = 14)),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  ) %>%
    plotly::config(displayModeBar = FALSE)
}

# One metric tab: P10–P90 bar and mean point (static), plus jittered per-rep
# points from a crosstalk SharedData keyed by rep.
sc_plotly_compare_metric <- function(shared, summ, ylab) {
  empty <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    sc_compare_plotly_layout(ylab)
  if (is.null(summ) || !nrow(summ)) return(empty)
  names <- unique(as.character(summ$short))
  pal <- sc_compare_set_colors(names)
  shapes <- sc_compare_set_shapes(names)
  summ <- summ %>%
    dplyr::mutate(short = factor(as.character(.data$short), levels = names))
  baseline_mean <- if ("baseline_mean" %in% names(summ) && nrow(summ) && !all(is.na(summ$baseline_mean))) {
    summ$baseline_mean[[1]]
  } else if (any(summ$is_baseline %||% FALSE)) {
    summ$mean[which(summ$is_baseline)[[1]]]
  } else {
    NA_real_
  }
  g <- ggplot2::ggplot(shared, ggplot2::aes(x = short, y = value, colour = short, shape = short,
                                            text = paste0(short, "<br>Rep ", rep, "<br>",
                                                          ylab, ": ", signif(value, 4)))) +
    ggplot2::geom_hline(yintercept = baseline_mean, linetype = "dashed",
                        linewidth = 1, colour = "#1a1a1a", show.legend = FALSE) +
    ggplot2::geom_linerange(
      data = summ,
      ggplot2::aes(x = short, ymin = p10, ymax = p90, colour = short),
      inherit.aes = FALSE, linewidth = 2.4, show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = summ,
      ggplot2::aes(x = short, y = mean, colour = short, shape = short),
      inherit.aes = FALSE, size = 5, show.legend = FALSE
    ) +
    ggplot2::geom_jitter(width = 0.14, height = 0, size = 2.6, alpha = 0.75) +
    ggplot2::scale_colour_manual(values = pal, breaks = names, name = NULL) +
    ggplot2::scale_shape_manual(values = shapes, breaks = names, name = NULL) +
    ggplot2::scale_x_discrete(limits = names) +
    ggplot2::labs(x = NULL, y = ylab) +
    theme_sc() +
    sc_one_row_legend()
  plotly::ggplotly(g, tooltip = "text") %>%
    sc_compare_plotly_layout(ylab) %>%
    plotly::highlight(on = "plotly_click", off = "plotly_doubleclick",
                      color = "#17becf", opacityDim = 0.2, selected = plotly::attrs_selected(showlegend = FALSE)) %>%
    sc_compare_clean_legend()
}

sc_compare_metric_shared <- function(points, table, metric_id) {
  metrics <- sc_compare_metrics()
  m <- metrics %>% dplyr::filter(.data$id == metric_id)
  stopifnot(nrow(m) == 1L)
  col <- m$col[[1]]
  ylab <- m$label[[1]]
  if (is.null(points) || !nrow(points) || !col %in% names(points)) {
    return(list(shared = NULL, summ = NULL, ylab = ylab))
  }
  d <- points %>%
    dplyr::transmute(short = .data$short, rep = .data$rep, value = .data[[col]])
  scen <- unique(as.character(d$short))
  d$short <- factor(d$short, levels = scen)
  # One SharedData group for all metric tabs so a click is linked across them.
  # Include the selected set in the group name so a sidebar change remounts cleanly.
  ids <- if ("id" %in% names(points)) paste(sort(unique(as.character(points$id))), collapse = "_") else "set"
  group <- paste0("sc_compare_reps_", ids)
  shared <- crosstalk::SharedData$new(d, key = ~rep, group = group)
  summ <- table %>%
    dplyr::filter(.data$col == .env$col) %>%
    dplyr::mutate(short = factor(.data$short, levels = scen)) %>%
    dplyr::arrange(.data$short)
  list(shared = shared, summ = summ, ylab = ylab)
}

# Shared density xlim lives in spacecraft_results_plots.R (sc_density_xlim).

# Throughput: one density per group with fill transparency, plus a rug.
# Eddie asked for a rug on this chart (an exception to the usual no-rug default).
# `data` needs a numeric `value` column and an optional grouping column.
sc_plot_throughput_density <- function(data, value = "value", group = NULL,
                                       xlab = "Throughput (units per run)",
                                       vline = NULL, pal = NULL, shapes = NULL) {
  empty <- ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
    ggplot2::labs(subtitle = "No throughput data to plot.")
  if (is.null(data) || !nrow(data) || !value %in% names(data)) return(empty)
  d <- data %>%
    dplyr::mutate(value = .data[[value]]) %>%
    dplyr::filter(is.finite(.data$value))
  if (!nrow(d)) return(empty)
  has_group <- !is.null(group) && group %in% names(d) && dplyr::n_distinct(d[[group]]) > 0
  if (has_group) {
    levels <- if (is.factor(d[[group]])) levels(droplevels(d[[group]])) else unique(as.character(d[[group]]))
    d$grp <- factor(as.character(d[[group]]), levels = levels)
  } else {
    levels <- "Line"
    d$grp <- factor(levels, levels = levels)
  }
  if (is.null(pal)) pal <- sc_compare_set_colors(levels)
  if (is.null(shapes)) shapes <- sc_compare_set_shapes(levels)
  xlim <- sc_density_xlim(d$value, level = 0.95, bounds = c(0, Inf))
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$value, colour = .data$grp, fill = .data$grp)) +
    ggplot2::geom_density(alpha = 0.35, linewidth = 1.05, na.rm = TRUE) +
    ggplot2::geom_rug(ggplot2::aes(colour = .data$grp), sides = "b",
                      alpha = 0.7, linewidth = 0.55, length = ggplot2::unit(0.04, "npc"),
                      na.rm = TRUE, show.legend = FALSE) +
    # Shaped ticks on the axis so the rug still reads if colour is lost.
    ggplot2::geom_point(
      ggplot2::aes(y = 0, shape = .data$grp, colour = .data$grp),
      size = 2.2, alpha = 0.85, stroke = 0.4
    ) +
    ggplot2::scale_colour_manual(values = pal, breaks = levels, name = NULL) +
    ggplot2::scale_fill_manual(values = pal, breaks = levels, name = NULL) +
    ggplot2::scale_shape_manual(values = shapes, breaks = levels, name = NULL) +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(x = xlab, y = "Density") +
    theme_sc()
  if (has_group && dplyr::n_distinct(d$grp) > 1) g <- g + sc_one_row_legend()
  else g <- g + ggplot2::theme(legend.position = "none")
  if (length(vline) && is.finite(vline[[1]])) {
    g <- g + ggplot2::geom_vline(xintercept = vline, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  g
}

# Overlapping histogram with transparency, used for cycle time and touch labor
# on Compare and Analyze. Bars share one set of bins, sit on top of each other
# (position identity, not dodged or stacked), and use alpha around 0.45 so
# the overlap is visible. `data` needs the numeric `x` column and an optional
# grouping column.
sc_plot_overlap_hist <- function(data, x, group = NULL, xlab, bins = 30,
                                 pal = NULL, vline = NULL, alpha = 0.45) {
  empty <- ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
    ggplot2::labs(subtitle = "No data to plot.")
  if (is.null(data) || !nrow(data) || !x %in% names(data)) return(empty)
  d <- data %>%
    dplyr::mutate(xval = .data[[x]]) %>%
    dplyr::filter(is.finite(.data$xval))
  if (!nrow(d)) return(empty)
  has_group <- !is.null(group) && group %in% names(d) && dplyr::n_distinct(d[[group]]) > 0
  if (has_group) {
    levels <- if (is.factor(d[[group]])) levels(droplevels(d[[group]])) else unique(as.character(d[[group]]))
    d$grp <- factor(as.character(d[[group]]), levels = levels)
  } else {
    levels <- "Line"
    d$grp <- factor(levels, levels = levels)
  }
  if (is.null(pal)) pal <- sc_compare_set_colors(levels)
  xlim <- sc_density_xlim(d$xval, level = 0.95, bounds = c(0, Inf))
  # Shared bin edges from the combined data so every group is comparable.
  br <- graphics::hist(d$xval, breaks = bins, plot = FALSE)$breaks
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$xval, fill = .data$grp, colour = .data$grp)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      position = "identity", breaks = br, alpha = alpha, linewidth = 0.25, na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(values = pal, breaks = levels, name = NULL) +
    ggplot2::scale_colour_manual(values = pal, breaks = levels, name = NULL) +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(x = xlab, y = "Density") +
    theme_sc()
  if (has_group && dplyr::n_distinct(d$grp) > 1) g <- g + sc_one_row_legend()
  else g <- g + ggplot2::theme(legend.position = "none")
  if (length(vline) && is.finite(vline[[1]])) {
    g <- g + ggplot2::geom_vline(xintercept = vline, linetype = "dashed",
                                 linewidth = 1, colour = "#1a1a1a")
  }
  g
}

# Older name kept so existing tests and callers still resolve.
sc_plot_dodged_hist <- sc_plot_overlap_hist

# ggplotly wrapper that remounts the crosstalk group when the selected
# scenarios change, and keeps a click-to-highlight on the same replication.
sc_ggplotly_compare <- function(g, ylab, xlab = "", shared = NULL) {
  empty <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    sc_compare_plotly_layout(ylab, xlab = xlab)
  if (is.null(g)) return(empty)
  p <- plotly::ggplotly(g, tooltip = "text") %>%
    sc_compare_plotly_layout(ylab, xlab = xlab) %>%
    sc_compare_clean_legend()
  if (!is.null(shared)) {
    p <- plotly::highlight(
      p, on = "plotly_click", off = "plotly_doubleclick",
      color = "#17becf", opacityDim = 0.2,
      selected = plotly::attrs_selected(showlegend = FALSE)
    )
  }
  p
}

# Compare throughput: density + rug, dashed baseline mean, crosstalk by rep.
sc_plotly_throughput_density <- function(points, table = NULL) {
  ylab <- "Density"
  xlab <- "Throughput (units per run)"
  empty <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    sc_compare_plotly_layout(xlab)
  if (is.null(points) || !nrow(points) || !"shipped" %in% names(points)) return(empty)
  d <- points %>%
    dplyr::transmute(
      short = as.character(.data$short),
      id = if ("id" %in% names(points)) as.character(.data$id) else as.character(.data$short),
      rep = .data$rep,
      value = .data$shipped
    ) %>%
    dplyr::filter(is.finite(.data$value))
  if (!nrow(d)) return(empty)
  names <- unique(d$short)
  pal <- sc_compare_set_colors(names)
  shapes <- sc_compare_set_shapes(names)
  ids <- paste(sort(unique(d$id)), collapse = "_")
  group <- paste0("sc_compare_reps_", ids)
  shared <- crosstalk::SharedData$new(d, key = ~rep, group = group)
  vline <- NA_real_
  if (!is.null(table) && nrow(table)) {
    row <- table %>% dplyr::filter(.data$col == "shipped", isTRUE(.data$is_baseline))
    if (nrow(row) && "mean" %in% names(row)) vline <- row$mean[[1]]
  }
  g <- sc_plot_throughput_density(d, value = "value", group = "short",
                                  xlab = xlab, vline = vline, pal = pal, shapes = shapes)
  sc_ggplotly_compare(g, ylab = "Density", xlab = xlab, shared = shared)
}

# Compare cycle time / touch labor: overlapping transparent histogram of units.
# barmode overlay keeps plotly from stacking or dodging the converted traces.
sc_plotly_overlap_hist <- function(units, x, xlab, group = "short") {
  empty <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    sc_compare_plotly_layout(xlab)
  if (is.null(units) || !nrow(units) || !x %in% names(units)) return(empty)
  d <- units %>%
    dplyr::filter(is.finite(.data[[x]]))
  if ("status" %in% names(d)) d <- d %>% dplyr::filter(.data$status == "shipped")
  if (!nrow(d)) return(empty)
  names <- unique(as.character(d[[group]]))
  pal <- sc_compare_set_colors(names)
  g <- sc_plot_overlap_hist(d, x = x, group = group, xlab = xlab, pal = pal)
  p <- sc_ggplotly_compare(g, ylab = "Density", xlab = xlab, shared = NULL)
  # ggplotly defaults to barmode "relative" (stacked). Bake overlay in so the
  # traces stay overlapping after plotly_build, matching ggplot position identity.
  p <- plotly::layout(p, barmode = "overlay")
  plotly::plotly_build(p)
}

# Older name kept so existing tests and callers still resolve.
sc_plotly_dodged_hist <- sc_plotly_overlap_hist
