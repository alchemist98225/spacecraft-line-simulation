# spacecraft_sparklines.R
# Time series behind the KPI cards on Simulate: each headline KPI binned over
# simulated time, averaged across replications, with the lowest-to-highest
# replication as a band. sc_kpi_series() builds them; sc_plot_spark() draws one.

# Bin width in hours: the widest of month (4 weeks) / week / day that still
# gives at least 6 bins. Wide bins keep a ~120-units-a-year line from gappy.
sc_spark_bin_h <- function(horizon) {
  for (w in c(672, 168, 24)) if (horizon / w >= 6) return(w)
  24
}

# Time-average of a step function inside each bin, sampled every 'step' hours.
sc_bin_step_avg <- function(time, level, edges, step) {
  grid <- seq(step / 2, max(edges), by = step)
  at <- if (length(time)) c(0, level)[findInterval(grid, time) + 1] else rep(0, length(grid))
  tibble::tibble(bin = findInterval(grid, edges, rightmost.closed = TRUE), v = at) %>%
    dplyr::group_by(bin) %>% dplyr::summarise(value = mean(v), .groups = "drop")
}

# One replication -> long tibble (kpi, bin, value). Empty bins are NA.
sc_rep_series <- function(run, edges) {
  u <- sc_units(run)
  nb <- length(edges) - 1
  b_of <- function(t) findInterval(t, edges, rightmost.closed = TRUE)
  all_bins <- tibble::tibble(bin = seq_len(nb))
  per_bin <- function(d, f) all_bins %>%
    dplyr::left_join(d %>% dplyr::group_by(bin) %>% dplyr::summarise(value = f(dplyr::pick(dplyr::everything())), .groups = "drop"), by = "bin")
  ship <- u %>% dplyr::filter(status == "shipped") %>% dplyr::mutate(bin = b_of(end_time))
  done <- u %>% dplyr::filter(status != "wip") %>% dplyr::mutate(bin = b_of(end_time))
  wip_ev <- dplyr::bind_rows(tibble::tibble(time = u$start_time, d = 1),
                             tibble::tibble(time = u$end_time[u$status != "wip"], d = -1)) %>%
    dplyr::arrange(time) %>% dplyr::mutate(level = cumsum(d))
  ch <- run$resources %>% dplyr::filter(resource == "chamber")
  step <- min(6, diff(edges)[1] / 4)
  # simmer logs mrb_q_time each time a unit leaves the MRB queue; the increase
  # is that visit's wait, stamped with the time it left.
  mrb <- run$attributes %>% dplyr::filter(key == "mrb_q_time") %>%
    dplyr::group_by(name) %>% dplyr::arrange(time, .by_group = TRUE) %>%
    dplyr::mutate(wait = value - dplyr::lag(value, default = 0)) %>% dplyr::ungroup() %>%
    dplyr::mutate(bin = b_of(time))
  scrap_cum <- all_bins %>%
    dplyr::left_join(done %>% dplyr::count(bin, scr = status == "scrapped") %>%
                       tidyr::pivot_wider(names_from = scr, values_from = n, values_fill = 0, names_prefix = "s"), by = "bin") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("s"), ~ tidyr::replace_na(.x, 0)))
  if (!"sTRUE" %in% names(scrap_cum)) scrap_cum$sTRUE <- 0
  if (!"sFALSE" %in% names(scrap_cum)) scrap_cum$sFALSE <- 0
  scrap_cum <- scrap_cum %>%
    dplyr::mutate(n = cumsum(sTRUE + sFALSE), value = dplyr::if_else(n > 0, 100 * cumsum(sTRUE) / n, NA_real_))
  dplyr::bind_rows(
    cycle = per_bin(ship, function(x) mean(x$cycle_h)),
    on_time = per_bin(ship, function(x) 100 * mean(x$lateness_h <= 0)),
    wip = all_bins %>% dplyr::left_join(sc_bin_step_avg(wip_ev$time, wip_ev$level, edges, step), by = "bin"),
    chamber = all_bins %>% dplyr::left_join(sc_bin_step_avg(ch$time, ch$server / max(ch$capacity, 1), edges, step) %>%
                                              dplyr::mutate(value = 100 * value), by = "bin"),
    mrb = per_bin(mrb, function(x) mean(x$wait)),
    scrap = scrap_cum %>% dplyr::select(bin, value),
    .id = "kpi"
  ) %>% dplyr::mutate(rep = run$rep %||% 1L)
}

# All replications -> mean, low and high per KPI per bin. Gaps (bins where no
# unit shipped in any replication) carry the last value forward.
sc_kpi_series <- function(runs) {
  horizon <- runs[[1]]$horizon
  w <- sc_spark_bin_h(horizon)
  edges <- unique(c(seq(0, horizon, by = w), horizon))
  purrr::map_dfr(runs, sc_rep_series, edges = edges) %>%
    dplyr::group_by(kpi, bin) %>%
    dplyr::summarise(mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
                     lo = suppressWarnings(min(value, na.rm = TRUE)),
                     hi = suppressWarnings(max(value, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(c(lo, hi), ~ dplyr::if_else(is.finite(.x), .x, NA_real_)),
                  day = (edges[bin] + edges[bin + 1]) / 2 / 24, bin_h = w) %>%
    dplyr::group_by(kpi) %>% dplyr::arrange(bin, .by_group = TRUE) %>%
    tidyr::fill(mean, lo, hi, .direction = "down") %>% dplyr::ungroup()
}

# A small line + faint band, no axes. Colour follows the app's primary blue.
sc_plot_spark <- function(series, which, colour = "#ffffff") {
  d <- series %>% dplyr::filter(kpi == which, !is.na(mean))
  if (!nrow(d)) return(ggplot2::ggplot() + ggplot2::theme_void())
  ggplot2::ggplot(d, ggplot2::aes(day, mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = colour, alpha = 0.25) +
    ggplot2::geom_line(colour = colour, linewidth = 1.1) +
    ggplot2::geom_point(data = dplyr::slice_tail(d, n = 1), colour = colour, size = 2.5) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA))
}
