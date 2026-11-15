# spacecraft_results_plots.R
# Lead charts for Results: a cycle-time count histogram of every shipped
# unit (no density overlay), and
# utilization / downtime by resource as a mean point with a 10–90 bar,
# sorted so the busiest resource is on top.

# Central x-range for a density/histogram so a long tail does not squash
# the main mass (the automotive chart uses the central 95%).
sc_density_xlim <- function(x, level = 0.95, bounds = c(0, Inf)) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(0, 1))
  alpha <- (1 - level) / 2
  lim <- stats::quantile(x, c(alpha, 1 - alpha), names = FALSE, na.rm = TRUE)
  lim[1] <- max(bounds[1], lim[1])
  if (is.finite(bounds[2])) lim[2] <- min(bounds[2], lim[2])
  if (!is.finite(lim[2]) || lim[2] <= lim[1]) lim[2] <- lim[1] + 1
  as.numeric(lim)
}

# Histogram of every shipped unit on a count scale. The dashed line is the mean.
sc_plot_cycle_hist_reps <- function(units) {
  d <- units %>%
    dplyr::filter(.data$status == "shipped", is.finite(.data$cycle_h))
  if (!nrow(d)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No shipped units to plot."))
  }
  mean_ct <- mean(d$cycle_h, na.rm = TRUE)
  xlim <- sc_density_xlim(d$cycle_h, level = 0.95, bounds = c(0, Inf))
  ggplot2::ggplot(d, ggplot2::aes(x = cycle_h)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count)),
      bins = 30, fill = "#41b6c4", colour = "white", alpha = 0.55
    ) +
    ggplot2::geom_vline(xintercept = mean_ct, linetype = "dashed",
                        colour = "#1a1a1a", linewidth = 1) +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(
      x = "Cycle time (h)", y = "Count",
      caption = sc_caption(sprintf(
        "Each bar is the number of shipped units in that cycle-time range. The dashed line is the mean (%.0f h). The axis shows the central 95%% of shipped units.",
        mean_ct
      ))
    ) +
    theme_sc()
}

# Mean point and 10–90 bar for a per-replication resource metric, highest at the top.
sc_plot_resource_spread <- function(per_rep, value, ylab, percent = TRUE,
                                    caption = NULL, highlight = NULL,
                                    highlight_name = "Bottleneck") {
  if (is.null(per_rep) || !nrow(per_rep) || !value %in% names(per_rep)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No data to plot."))
  }
  qfun <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
  summ <- per_rep %>%
    dplyr::group_by(.data$resource, .data$label, .data$short) %>%
    dplyr::summarise(
      mean = mean(.data[[value]], na.rm = TRUE),
      p10 = qfun(.data[[value]], 0.10),
      p90 = qfun(.data[[value]], 0.90),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$mean)
  summ$y <- factor(summ$label, levels = summ$label)
  summ$hl <- !is.null(highlight) & summ$resource %in% highlight
  cap <- caption %||% "Points are the mean across replications; bars run from the 10th to the 90th percentile."
  x_scale <- if (isTRUE(percent)) {
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.06)),
      breaks = scales::breaks_pretty(n = 5)
    )
  } else {
    ggplot2::scale_x_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.06)),
      breaks = scales::breaks_pretty(n = 5)
    )
  }
  pal <- c("FALSE" = "#2c7fb8", "TRUE" = "#d95f02")
  col_scale <- if (any(summ$hl)) {
    ggplot2::scale_colour_manual(
      values = pal, name = NULL,
      labels = c("TRUE" = highlight_name, "FALSE" = "Other"),
      breaks = c("TRUE", "FALSE")
    )
  } else {
    ggplot2::scale_colour_manual(values = pal, guide = "none")
  }
  p <- ggplot2::ggplot(summ, ggplot2::aes(y = y, x = mean, xmin = p10, xmax = p90,
                                          colour = factor(.data$hl))) +
    ggplot2::geom_linerange(linewidth = 1.6) +
    ggplot2::geom_point(size = 3.2) +
    col_scale +
    x_scale +
    ggplot2::labs(x = ylab, y = NULL, caption = sc_caption(cap)) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  if (any(summ$hl)) p <- p + sc_one_row_legend()
  p
}

# Headline numbers for the Simulate value boxes from an analyze bundle.
sc_headline_bottleneck <- function(analyze) {
  empty <- list(
    bot_short = "—", bot_util = NA_real_,
    down_short = "—", down_frac = NA_real_,
    queue_short = "—", queue_avg = NA_real_
  )
  if (is.null(analyze) || is.null(analyze$resources) || !nrow(analyze$resources)) {
    return(empty)
  }
  st <- analyze$resources %>%
    dplyr::filter(!.data$resource %in% c("techs", "maintenance"))
  bot <- analyze$bottleneck
  if (is.null(bot) || !nrow(bot)) {
    bot <- st %>% dplyr::slice_max(.data$util, n = 1, with_ties = FALSE)
  }
  down <- st %>% dplyr::slice_max(.data$downtime_fraction, n = 1, with_ties = FALSE)
  q <- st %>% dplyr::slice_max(.data$queue_avg, n = 1, with_ties = FALSE)
  list(
    bot_short = bot$short[[1]] %||% "—",
    bot_util = bot$util[[1]],
    down_short = down$short[[1]] %||% "—",
    down_frac = down$downtime_fraction[[1]],
    queue_short = q$short[[1]] %||% "—",
    queue_avg = q$queue_avg[[1]]
  )
}

sc_headline_bot_label <- function(h) {
  if (!is.finite(h$bot_util)) return("—")
  sprintf("%.0f%% · %s", 100 * h$bot_util, h$bot_short)
}
sc_headline_down_label <- function(h) {
  if (!is.finite(h$down_frac)) return("—")
  sprintf("%.1f%% · %s", 100 * h$down_frac, h$down_short)
}
sc_headline_queue_label <- function(h) {
  if (!is.finite(h$queue_avg)) return("—")
  sprintf("%.1f · %s", h$queue_avg, h$queue_short)
}

# Throughput-first gains for the Compare what-if set: one row per move.
sc_compare_gains_table <- function(tbl) {
  metrics <- sc_compare_metrics()
  wide <- tbl %>%
    dplyr::select(id, short, is_baseline, col, mean, delta) %>%
    tidyr::pivot_wider(
      names_from = col,
      values_from = c(mean, delta),
      names_sep = "__"
    )
  base <- wide %>% dplyr::filter(.data$is_baseline)
  moves <- wide %>%
    dplyr::filter(!.data$is_baseline) %>%
    dplyr::arrange(dplyr::desc(.data$delta__shipped))
  list(base = base, moves = moves, all = dplyr::bind_rows(base, moves), metrics = metrics)
}

# Horizontal bars of throughput gain vs the baseline, sorted so the best
# move is on top. Cycle time, HPU and WIP stay in the table beside it.
sc_plot_compare_gains <- function(tbl) {
  g <- sc_compare_gains_table(tbl)
  d <- g$moves
  if (is.null(d) || !nrow(d) || !"delta__shipped" %in% names(d)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No what-if moves to plot."))
  }
  d$y <- factor(d$short, levels = rev(d$short))
  d$fill <- dplyr::case_when(
    d$delta__shipped >  1e-9 ~ "#31a354",
    d$delta__shipped < -1e-9 ~ "#d95f02",
    TRUE ~ "#9e9e9e"
  )
  ggplot2::ggplot(d, ggplot2::aes(y = y, x = delta__shipped, fill = fill)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "#1a1a1a", linewidth = 1) +
    ggplot2::geom_col(width = 0.7, colour = "#1a1a1a", linewidth = 0.25) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = "Throughput gain vs baseline (units)",
      y = NULL,
      caption = sc_caption("Each capacity move is judged first by the throughput gain versus the baseline. Cycle time, touch labor (HPU) and WIP for the same moves are in the table. The dashed line is no change.")
    ) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

sc_table_compare_gains <- function(tbl) {
  g <- sc_compare_gains_table(tbl)
  d <- g$all
  if (is.null(d) || !nrow(d)) {
    return(gt::gt(tibble::tibble(Note = "No comparison results.")) %>% sc_gt_style())
  }
  fmt_mean <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "—")
  fmt_d <- function(x, better) {
    dplyr::case_when(
      is.na(x) ~ "—",
      abs(x) < 1e-9 ~ "0",
      TRUE ~ paste0(ifelse(x > 0, "+", "\u2212"), sprintf("%.1f", abs(x)))
    )
  }
  out <- d %>%
    dplyr::transmute(
      Move = .data$short,
      Throughput = fmt_mean(.data$mean__shipped),
      `Δ Throughput` = dplyr::if_else(.data$is_baseline, "\u2014", fmt_d(.data$delta__shipped, "higher")),
      HPU = fmt_mean(.data$mean__labor_hpu),
      `Δ HPU` = dplyr::if_else(.data$is_baseline, "\u2014", fmt_d(.data$delta__labor_hpu, "lower")),
      WIP = fmt_mean(.data$mean__wip_avg),
      `Δ WIP` = dplyr::if_else(.data$is_baseline, "\u2014", fmt_d(.data$delta__wip_avg, "lower")),
      `Cycle time` = fmt_mean(.data$mean__cycle_mean_h),
      `Δ Cycle` = dplyr::if_else(.data$is_baseline, "\u2014", fmt_d(.data$delta__cycle_mean_h, "lower")),
      is_baseline
    )
  out %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_hide(is_baseline) %>%
    gt::cols_align("right", columns = -Move) %>%
    gt::tab_style(
      style = list(gt::cell_fill(color = "#f3faf5"), gt::cell_text(weight = "bold")),
      locations = gt::cells_body(rows = which(d$is_baseline))
    ) %>%
    gt::tab_options(table.width = gt::pct(100), table.font.size = gt::px(14),
                    data_row.padding = gt::px(5), column_labels.padding = gt::px(4)) %>%
    gt::tab_source_note("Moves are sorted by throughput gain (the first test of a capacity change). Cycle time, HPU and WIP are shown as secondary effects. The highlighted row is the baseline. Numbers are means across replications.")
}

# Throughput as a rate, using the same 52-week year as the rest of the app.
sc_throughput_label <- function(shipped, sim_hours) {
  year_h <- 24 * 7 * 52
  week_h <- 24 * 7
  hours <- sim_hours %||% year_h
  if (!is.finite(hours) || hours <= 0) hours <- year_h
  per_year <- shipped * year_h / hours
  per_week <- shipped * week_h / hours
  sprintf("%.0f units/yr (%.1f/wk)", per_year, per_week)
}

# Quality origins: functional test, environmental test, and MRB disposition.
sc_quality_origins <- function() {
  tibble::tribble(
    ~origin,     ~label,                          ~short,
    "func_test", "Functional test",               "Func. test",
    "chamber",   "Environmental test",            "Env. test",
    "mrb",       "Material review board",         "MRB"
  )
}

# One row per replication and origin: fail, rework and scrap counts and rates
# (rates are per unit released in that replication).
sc_quality_by_rep <- function(units) {
  origins <- sc_quality_origins()
  if (is.null(units) || !nrow(units)) {
    return(origins %>%
             dplyr::mutate(rep = 1L, n_released = 0, n_finished = 0,
                           n_fail = 0, n_rework = 0, n_scrap = 0,
                           fail_rate = NA_real_, rework_rate = NA_real_,
                           scrap_rate = NA_real_))
  }
  u <- units %>%
    dplyr::mutate(
      n_fail_func   = dplyr::coalesce(.data$n_fail_func, 0),
      n_fail_env    = dplyr::coalesce(.data$n_fail_env, 0),
      n_rework_func = dplyr::coalesce(.data$n_rework_func, 0),
      n_rework_mrb  = dplyr::coalesce(.data$n_rework_mrb, 0),
      n_scrap_mrb   = dplyr::coalesce(.data$n_scrap_mrb,
                                      dplyr::if_else(.data$status == "scrapped", 1, 0)),
      released = 1
    )
  purrr::map_dfr(unique(u$rep), function(rp) {
    ur <- u %>% dplyr::filter(.data$rep == rp)
    n_rel <- nrow(ur)
    n_fin <- sum(ur$status %in% c("shipped", "scrapped"))
    den <- if (n_rel > 0) n_rel else NA_real_
    dplyr::bind_rows(
      tibble::tibble(origin = "func_test",
                     n_fail = sum(ur$n_fail_func), n_rework = sum(ur$n_rework_func), n_scrap = 0),
      tibble::tibble(origin = "chamber",
                     n_fail = sum(ur$n_fail_env), n_rework = 0, n_scrap = 0),
      tibble::tibble(origin = "mrb",
                     n_fail = 0, n_rework = sum(ur$n_rework_mrb), n_scrap = sum(ur$n_scrap_mrb))
    ) %>%
      dplyr::mutate(
        rep = rp, n_released = n_rel, n_finished = n_fin,
        fail_rate = .data$n_fail / den,
        rework_rate = .data$n_rework / den,
        scrap_rate = .data$n_scrap / den
      )
  }) %>%
    dplyr::left_join(origins, by = "origin") %>%
    dplyr::mutate(label = factor(.data$label, levels = origins$label))
}

# Observed MTBF / MTTR per station and replication, next to the configured values.
# MTBF is operating hours per failure; MTTR is repair hours per repair.
# Replications with no failures leave the observed values as NA.
sc_reliability_observed <- function(downtime, params = sc_default_params()) {
  cat <- sc_reliability_catalog()
  if (is.null(downtime) || !nrow(downtime)) {
    return(cat %>% dplyr::mutate(
      rep = 1L, n_repairs = 0, n_failures = 0, process_h = 0, downtime_h = 0,
      obs_mtbf = NA_real_, obs_mttr = NA_real_,
      cfg_mtbf = NA_real_, cfg_mttr = NA_real_
    ))
  }
  d <- downtime
  if (!"n_failures" %in% names(d)) d$n_failures <- d$n_repairs
  if (!"label" %in% names(d)) d <- d %>% dplyr::left_join(cat, by = "resource")
  d %>%
    dplyr::mutate(
      n_fail = dplyr::coalesce(.data$n_failures, .data$n_repairs, 0),
      n_rep = dplyr::coalesce(.data$n_repairs, 0),
      obs_mtbf = dplyr::if_else(.data$n_fail > 0, .data$process_h / .data$n_fail, NA_real_),
      obs_mttr = dplyr::if_else(.data$n_rep > 0, .data$downtime_h / .data$n_rep, NA_real_),
      cfg_mtbf = purrr::map_dbl(.data$resource, ~ sc_reliability_mtbf(params, .x)),
      cfg_mttr = purrr::map_dbl(.data$resource, ~ sc_reliability_mttr(params, .x))
    )
}

# Mean + 10-90 bar of an observed reliability metric, with the configured
# value as a diamond. Stations are sorted by the observed mean (NA at the bottom).
sc_plot_reliability_obs <- function(obs, value, cfg, ylab, caption = NULL) {
  if (is.null(obs) || !nrow(obs) || !value %in% names(obs)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No data to plot."))
  }
  qfun <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
  summ <- obs %>%
    dplyr::group_by(.data$resource, .data$label, .data$short) %>%
    dplyr::summarise(
      mean = mean(.data[[value]], na.rm = TRUE),
      p10 = qfun(.data[[value]], 0.10),
      p90 = qfun(.data[[value]], 0.90),
      cfg = mean(.data[[cfg]], na.rm = TRUE),
      n_fail = sum(.data$n_fail %||% .data$n_failures %||% 0, na.rm = TRUE),
      .groups = "drop"
    )
  summ <- summ %>% dplyr::arrange(dplyr::desc(dplyr::coalesce(.data$mean, -Inf)))
  summ$y <- factor(summ$label, levels = rev(summ$label))
  cap <- caption %||% "Blue points are the mean observed value across replications; bars run from the 10th to the 90th percentile. Diamonds are the configured Settings value. Replications with no failures are omitted from the observed mean."
  ggplot2::ggplot(summ, ggplot2::aes(y = y)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = p10, xmax = p90),
                            linewidth = 1.6, colour = "#2c7fb8", na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = mean, shape = "Observed", colour = "Observed"),
                        size = 3.2, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = cfg, shape = "Configured", colour = "Configured"),
                        size = 3.4, na.rm = TRUE) +
    ggplot2::scale_shape_manual(values = c(Observed = 16, Configured = 18), name = NULL) +
    ggplot2::scale_colour_manual(values = c(Observed = "#2c7fb8", Configured = "#d95f02"), name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.06)),
                                breaks = scales::breaks_pretty(n = 5)) +
    ggplot2::labs(x = ylab, y = NULL, caption = sc_caption(cap)) +
    theme_sc() +
    sc_one_row_legend() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

sc_table_reliability_obs <- function(obs) {
  if (is.null(obs) || !nrow(obs)) {
    return(gt::gt(tibble::tibble(Note = "No reliability data.")) %>% sc_gt_style())
  }
  fmt <- function(x) ifelse(is.finite(x), sprintf("%.1f", x), "—")
  summ <- obs %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarise(
      `Configured MTBF (h)` = mean(.data$cfg_mtbf, na.rm = TRUE),
      `Observed MTBF (h)` = mean(.data$obs_mtbf, na.rm = TRUE),
      Failures = sum(.data$n_fail, na.rm = TRUE),
      Repairs = sum(.data$n_rep, na.rm = TRUE),
      `Configured MTTR (h)` = mean(.data$cfg_mttr, na.rm = TRUE),
      `Observed MTTR (h)` = mean(.data$obs_mttr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(Failures)) %>%
    dplyr::mutate(
      dplyr::across(c(`Configured MTBF (h)`, `Observed MTBF (h)`,
                      `Configured MTTR (h)`, `Observed MTTR (h)`), fmt)
    )
  summ %>%
    dplyr::rename(Station = label) %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_align("right", columns = -Station) %>%
    gt::tab_source_note("Observed MTBF is operating hours per failure; observed MTTR is repair hours per repair. Counts are totals across replications. A dash means no failures in this run.")
}

sc_table_quality <- function(q) {
  if (is.null(q) || !nrow(q)) {
    return(gt::gt(tibble::tibble(Note = "No quality data.")) %>% sc_gt_style())
  }
  fmt_n <- function(x) sprintf("%.1f", x)
  fmt_p <- function(x) ifelse(is.finite(x), sprintf("%.1f%%", 100 * x), "—")
  summ <- q %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarise(
      `Fails / rep` = mean(.data$n_fail, na.rm = TRUE),
      `Fail rate` = mean(.data$fail_rate, na.rm = TRUE),
      `Reworks / rep` = mean(.data$n_rework, na.rm = TRUE),
      `Rework rate` = mean(.data$rework_rate, na.rm = TRUE),
      `Scraps / rep` = mean(.data$n_scrap, na.rm = TRUE),
      `Scrap rate` = mean(.data$scrap_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      `Fails / rep` = fmt_n(.data$`Fails / rep`),
      `Reworks / rep` = fmt_n(.data$`Reworks / rep`),
      `Scraps / rep` = fmt_n(.data$`Scraps / rep`),
      `Fail rate` = fmt_p(.data$`Fail rate`),
      `Rework rate` = fmt_p(.data$`Rework rate`),
      `Scrap rate` = fmt_p(.data$`Scrap rate`)
    )
  summ %>%
    dplyr::rename(Origin = label) %>%
    gt::gt() %>%
    sc_gt_style() %>%
    gt::cols_align("right", columns = -Origin) %>%
    gt::tab_source_note("Rates are per unit released, averaged across replications. Functional-test rework is a rollback to Integration while under the fail cap. Environmental-test failures go to the review board. MRB rework and scrap are the review-board dispositions.")
}

# Quality rates in a fixed origin order (functional test, environmental, MRB),
# not sorted by the mean, so the three sources stay in the story order.
sc_plot_quality_rate <- function(q, value, ylab, caption = NULL) {
  if (is.null(q) || !nrow(q) || !value %in% names(q)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No data to plot."))
  }
  qfun <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE))
  origins <- sc_quality_origins()
  summ <- q %>%
    dplyr::group_by(.data$origin, .data$label, .data$short) %>%
    dplyr::summarise(
      mean = mean(.data[[value]], na.rm = TRUE),
      p10 = qfun(.data[[value]], 0.10),
      p90 = qfun(.data[[value]], 0.90),
      .groups = "drop"
    )
  summ$y <- factor(as.character(summ$label), levels = rev(origins$label))
  cap <- caption %||% "Points are the mean across replications; bars run from the 10th to the 90th percentile. Rates are per unit released."
  ggplot2::ggplot(summ, ggplot2::aes(y = y, x = mean, xmin = p10, xmax = p90)) +
    ggplot2::geom_linerange(linewidth = 1.6, colour = "#2c7fb8") +
    ggplot2::geom_point(size = 3.2, colour = "#2c7fb8") +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.06)),
      breaks = scales::breaks_pretty(n = 5)
    ) +
    ggplot2::labs(x = ylab, y = NULL, caption = sc_caption(cap)) +
    theme_sc() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}
