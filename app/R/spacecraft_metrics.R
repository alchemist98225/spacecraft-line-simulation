# spacecraft_metrics.R
# Turn raw simmer monitor tables from sc_run() into one row per unit and the
# headline KPIs: cycle time by variant, time-average WIP, chamber utilization,
# MRB queue time, scrap rate and on-time %.

# Last value of every attribute per unit, joined to its start/end times.
sc_units <- function(run) {
  attrs <- run$attributes %>%
    dplyr::group_by(name, key) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(name, key, value) %>%
    tidyr::pivot_wider(names_from = key, values_from = value)
  run$arrivals %>%
    dplyr::filter(startsWith(name, "unit")) %>%
    dplyr::select(name, start_time, end_time) %>%
    dplyr::left_join(attrs, by = "name") %>%
    dplyr::mutate(
      variant = run$plan$variant[match(unit_id, run$plan$unit_id)],
      status = dplyr::case_when(shipped == 1 ~ "shipped", scrapped == 1 ~ "scrapped", TRUE ~ "wip"),
      cycle_h = dplyr::if_else(status == "shipped", end_time - start_time, NA_real_),
      rep = run$rep %||% 1L
    )
}

# Time-average of a step function given (time, level) change points on [0, T].
sc_time_average <- function(time, level, horizon) {
  tibble::tibble(time = c(0, time), level = c(0, level)) %>%
    dplyr::filter(time <= horizon) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(dt = dplyr::lead(time, default = horizon) - time) %>%
    dplyr::summarise(avg = sum(level * dt) / horizon) %>%
    dplyr::pull(avg)
}

sc_rep_kpis <- function(run, variant_names = c("A", "B", "C")) {
  u <- sc_units(run)
  horizon <- run$horizon
  wip_events <- dplyr::bind_rows(
    tibble::tibble(time = u$start_time, d = 1),
    tibble::tibble(time = u$end_time[u$status != "wip"], d = -1)
  ) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(level = cumsum(d))
  chamber <- run$resources %>% dplyr::filter(resource == "chamber")
  u_done <- u %>% dplyr::filter(status != "wip")
  u_ship <- u %>% dplyr::filter(status == "shipped")
  tibble::tibble(
    rep = run$rep %||% 1L,
    arrivals = nrow(u),
    shipped = nrow(u_ship),
    scrapped = sum(u$status == "scrapped"),
    wip_end = sum(u$status == "wip"),
    wip_avg = sc_time_average(wip_events$time, wip_events$level, horizon),
    chamber_util = sc_time_average(chamber$time, chamber$server, horizon) / max(chamber$capacity, 1),
    mrb_q_mean_h = mean(u$mrb_q_time[u$mrb_visits > 0]),
    scrap_rate = if (nrow(u_done)) mean(u_done$status == "scrapped") else NA_real_,
    on_time_pct = if (nrow(u_ship)) 100 * mean(u_ship$lateness_h <= 0) else NA_real_,
    lateness_mean_h = if (nrow(u_ship)) mean(u_ship$lateness_h) else NA_real_,
    cycle_mean_h = mean(u_ship$cycle_h)
  ) %>%
    dplyr::bind_cols(
      u_ship %>%
        dplyr::mutate(variant = factor(variant_names[variant], levels = variant_names)) %>%
        dplyr::group_by(variant, .drop = FALSE) %>%
        dplyr::summarise(m = mean(cycle_h), .groups = "drop") %>%
        dplyr::mutate(variant = paste0("cycle_", variant, "_h")) %>%
        tidyr::pivot_wider(names_from = variant, values_from = m)
    )
}

# All replications: per-rep KPI rows plus a mean summary.
sc_summarise <- function(runs, params = sc_default_params()) {
  per_rep <- purrr::map_dfr(runs, sc_rep_kpis, variant_names = params$variants$variant)
  list(
    per_rep = per_rep,
    summary = per_rep %>% dplyr::summarise(dplyr::across(-rep, ~ mean(.x, na.rm = TRUE))),
    units = purrr::map_dfr(runs, sc_units)
  )
}
