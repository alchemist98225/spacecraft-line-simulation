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

sc_compare_plotly_layout <- function(p, ylab) {
  plotly::layout(
    p,
    font = list(size = 16, color = "#1a1a1a"),
    xaxis = list(title = "", tickfont = list(size = 16)),
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
