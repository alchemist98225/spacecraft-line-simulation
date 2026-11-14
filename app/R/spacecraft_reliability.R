# spacecraft_reliability.R
# Equipment breakdowns (MTBF / MTTR) for station types and the environmental
# test chamber. Failures are operating-time based: the clock only advances
# while a unit is being worked on at that resource. When a failure occurs
# mid-touch, the unit stays in place (it keeps the station) while a separate
# maintenance technician repairs the equipment; production techs are released
# around the repair so they are not tied up. Calendar-time "always on"
# failures are not used, because a low-volume line is idle most of the year.
#
# Availability is reported two ways:
#   operational = processing / (processing + downtime)
#   calendar downtime fraction = downtime / (capacity * horizon)
# Repair labour is a dedicated "maintenance" resource, not the production pool.

# Equipment that can break down (not the shared technician pool).
sc_reliability_catalog <- function() {
  tibble::tribble(
    ~resource,       ~label,                       ~short,
    "integration",   "Integration stations",       "Integration",
    "func_test",     "Functional test",            "Func. test",
    "chamber",       "Environmental test chamber", "Chamber",
    "review_board",  "Material review board",      "MRB",
    "acceptance",    "Final acceptance",           "Acceptance"
  )
}

sc_reliability_on <- function(params) {
  isTRUE(!is.null(params) && isTRUE(params$reliability$enabled %||% FALSE))
}

sc_reliability_mtbf <- function(params, resource) {
  val <- params$reliability[[resource]]$mtbf_h
  if (is.null(val) || !is.finite(val) || val <= 0) Inf else as.numeric(val)
}

sc_reliability_mttr <- function(params, resource) {
  val <- params$reliability[[resource]]$mttr_h
  if (is.null(val) || !is.finite(val) || val <= 0) 0 else as.numeric(val)
}

sc_reliability_active <- function(params, resource) {
  sc_reliability_on(params) &&
    is.finite(sc_reliability_mtbf(params, resource)) &&
    sc_reliability_mttr(params, resource) > 0
}

# Empty event tables so downstream joins always have the same columns.
sc_reliability_empty_failures <- function() {
  tibble::tibble(
    station = character(), time = numeric(), failure_id = integer(),
    name = character()
  )
}

sc_reliability_empty_repairs <- function() {
  tibble::tibble(
    station = character(), time = numeric(), duration = numeric(),
    repair_h = numeric(), failure_id = integer(), name = character()
  )
}

sc_reliability_events <- function() {
  e <- new.env(parent = emptyenv())
  e$failures <- list()
  e$repairs <- list()
  e$seq <- 0L
  e$fail_id <- 0L
  e
}

# Bind recorded events onto a finished replication.
sc_reliability_collect <- function(events, rep = 1L) {
  failures <- if (length(events$failures)) {
    dplyr::bind_rows(events$failures)
  } else {
    sc_reliability_empty_failures()
  }
  repairs <- if (length(events$repairs)) {
    dplyr::bind_rows(events$repairs)
  } else {
    sc_reliability_empty_repairs()
  }
  list(
    failures = failures %>% dplyr::mutate(rep = rep),
    repairs = repairs %>% dplyr::mutate(rep = rep)
  )
}

# Mean and 95% t-interval of a numeric vector.
sc_mean_ci <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (!n) return(tibble::tibble(n = 0L, mean = NA_real_, lower = NA_real_, upper = NA_real_))
  m <- mean(x)
  if (n < 2L || !is.finite(stats::sd(x)) || stats::sd(x) <= 0) {
    return(tibble::tibble(n = n, mean = m, lower = m, upper = m))
  }
  half <- stats::sd(x) / sqrt(n) * stats::qt(1 - (1 - conf) / 2, n - 1L)
  tibble::tibble(n = n, mean = m, lower = m - half, upper = m + half)
}

# Per-resource, per-replication downtime, availability and repair counts.
sc_downtime_by_rep <- function(runs, params = sc_default_params()) {
  cat <- sc_reliability_catalog()
  horizon <- runs[[1]]$horizon %||% params$sim_hours
  purrr::map_dfr(runs, function(run) {
    repairs <- run$repairs %||% sc_reliability_empty_repairs()
    if (!"rep" %in% names(repairs) || !nrow(repairs)) {
      repairs <- repairs %>% dplyr::mutate(rep = run$rep %||% 1L)
    }
    mon <- run$resources
    cap <- mon %>%
      dplyr::filter(.data$resource %in% cat$resource) %>%
      dplyr::group_by(resource) %>%
      dplyr::summarise(capacity = dplyr::last(.data$capacity), .groups = "drop")
    arrivals_res <- run$arrivals_res %||% tibble::tibble(
      name = character(), start_time = numeric(), end_time = numeric(),
      activity_time = numeric(), resource = character()
    )
    purrr::map_dfr(cat$resource, function(res) {
      cap_n <- cap$capacity[match(res, cap$resource)]
      if (length(cap_n) != 1 || is.na(cap_n)) cap_n <- params$capacity[[res]] %||% 1
      rr <- repairs %>% dplyr::filter(.data$station == res)
      down_h <- if (nrow(rr)) sum(rr$duration, na.rm = TRUE) else 0
      n_repairs <- if (nrow(rr)) nrow(rr) else 0L
      visits <- arrivals_res %>% dplyr::filter(.data$resource == res)
      # Processing hours: station activity minus tech wait minus downtime
      # attributed to units at this station. Fall back to activity_time if
      # we cannot split it.
      process_h <- if (nrow(visits)) sum(pmax(0, visits$activity_time), na.rm = TRUE) else 0
      process_h <- max(0, process_h - down_h)
      calendar_den <- cap_n * horizon
      op_den <- process_h + down_h
      tibble::tibble(
        resource = res,
        rep = run$rep %||% 1L,
        capacity = cap_n,
        downtime_h = down_h,
        n_repairs = as.integer(n_repairs),
        process_h = process_h,
        downtime_fraction = if (calendar_den > 0) down_h / calendar_den else 0,
        availability = if (op_den > 0) process_h / op_den else 1,
        calendar_availability = if (calendar_den > 0) 1 - down_h / calendar_den else 1
      )
    }) %>%
      dplyr::left_join(cat, by = "resource")
  })
}

# Why an occupied or idle station spends time the way it does, in hours,
# for one replication. Definitions:
#   blocked  — unit-hours waiting in this station's queue (cannot enter)
#   techs    — hours a unit holds the station waiting for technicians
#   down     — hours the station is held for repair (including wait for a maintainer)
#   starved  — idle server-hours with an empty queue (no upstream work)
sc_wait_reasons_one <- function(run, params = sc_default_params()) {
  cat <- sc_reliability_catalog()
  horizon <- run$horizon %||% params$sim_hours
  mon <- run$resources
  arrivals_res <- run$arrivals_res %||% tibble::tibble(
    name = character(), start_time = numeric(), end_time = numeric(),
    activity_time = numeric(), resource = character()
  )
  repairs <- run$repairs %||% sc_reliability_empty_repairs()
  cap <- mon %>%
    dplyr::group_by(resource) %>%
    dplyr::summarise(capacity = dplyr::last(.data$capacity), .groups = "drop")
  visits <- arrivals_res %>%
    dplyr::mutate(
      wait_h = pmax(0, .data$end_time - .data$start_time - .data$activity_time),
      act_h = pmax(0, .data$activity_time)
    )
  tech <- visits %>% dplyr::filter(.data$resource == "techs")
  purrr::map_dfr(cat$resource, function(res) {
    cap_n <- cap$capacity[match(res, cap$resource)]
    if (length(cap_n) != 1 || is.na(cap_n)) cap_n <- params$capacity[[res]] %||% 1
    st <- visits %>% dplyr::filter(.data$resource == res)
    blocked <- if (nrow(st)) sum(st$wait_h, na.rm = TRUE) else 0
    # Tech waits that overlap this station service interval (not its queue).
    techs_h <- 0
    if (nrow(st) && nrow(tech)) {
      ov <- st %>%
        dplyr::mutate(svc0 = .data$end_time - .data$act_h, svc1 = .data$end_time) %>%
        dplyr::select(name, svc0, svc1) %>%
        dplyr::inner_join(
          tech %>% dplyr::transmute(name, w0 = start_time, w1 = start_time + wait_h),
          by = "name",
          relationship = "many-to-many"
        ) %>%
        dplyr::mutate(overlap = pmax(0, pmin(.data$w1, .data$svc1) - pmax(.data$w0, .data$svc0)))
      if (nrow(ov)) techs_h <- sum(ov$overlap, na.rm = TRUE)
    }
    down_h <- {
      rr <- repairs %>% dplyr::filter(.data$station == res)
      if (nrow(rr)) sum(rr$duration, na.rm = TRUE) else 0
    }
    ev <- mon %>% dplyr::filter(.data$resource == res)
    starved <- if (!nrow(ev)) {
      cap_n * horizon
    } else {
      d <- ev %>%
        dplyr::arrange(.data$time) %>%
        dplyr::mutate(
          t1 = pmin(dplyr::lead(.data$time, default = horizon), horizon),
          t0 = pmax(.data$time, 0),
          dt = pmax(0, .data$t1 - .data$t0),
          idle = pmax(0, .data$capacity - .data$server),
          empty_q = .data$queue <= 0
        ) %>%
        dplyr::filter(.data$t0 < horizon)
      # Time before the first event is fully idle and starved.
      t_first <- min(ev$time, horizon)
      lead <- if (t_first > 0) cap_n * t_first else 0
      lead + sum(d$idle * as.numeric(d$empty_q) * d$dt, na.rm = TRUE)
    }
    tibble::tibble(
      resource = res,
      rep = run$rep %||% 1L,
      blocked = blocked,
      techs = techs_h,
      down = down_h,
      starved = starved
    )
  }) %>%
    dplyr::left_join(cat, by = "resource")
}

sc_wait_reasons <- function(runs, params = sc_default_params()) {
  purrr::map_dfr(runs, sc_wait_reasons_one, params = params)
}

# Long form used by the Analyze "Why stations wait" chart.
sc_wait_reason_levels <- function() {
  tibble::tribble(
    ~reason,    ~label,                    ~color,
    "blocked",  "Blocked (queueing)",      "#d95f02",
    "techs",    "Waiting for technicians", "#17becf",
    "down",     "Down for repair",         "#c0392b",
    "starved",  "Starved of upstream",     "#31a354"
  )
}

sc_wait_reasons_long <- function(waits) {
  labs <- sc_wait_reason_levels()
  # Drop the station `label` so the join can use that name for the reason.
  waits %>%
    dplyr::select(-dplyr::any_of("label")) %>%
    tidyr::pivot_longer(c(blocked, techs, down, starved),
                        names_to = "reason", values_to = "hours") %>%
    dplyr::mutate(hours = dplyr::if_else(abs(.data$hours) < 1e-8, 0, .data$hours)) %>%
    dplyr::left_join(labs, by = "reason") %>%
    dplyr::mutate(
      reason = factor(.data$reason, levels = labs$reason),
      label = factor(.data$label, levels = labs$label)
    )
}

# Mean ± 95% CI bars with faint per-rep points. Used for downtime fraction
# and for the wait-reason chart (dodged when `fill` is set).
sc_plot_mean_ci <- function(df, x, y, fill = NULL, ylab, caption,
                            percent = FALSE, pal = NULL, narrow = FALSE) {
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  # enquo first: `is.null(fill)` would force-evaluate a bare column name.
  fill_arg <- rlang::enquo(fill)
  fill_q <- if (rlang::quo_is_null(fill_arg) || rlang::quo_is_missing(fill_arg)) {
    NULL
  } else {
    rlang::ensym(fill)
  }
  df <- df %>% dplyr::filter(is.finite(!!y))
  empty <- ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
    ggplot2::labs(subtitle = "No replications to plot.")
  if (!nrow(df)) return(empty)
  group_cols <- c(rlang::as_string(x), if (!is.null(fill_q)) rlang::as_string(fill_q))
  summ <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(sc_mean_ci(!!y), .groups = "drop")
  dodge <- if (is.null(fill_q)) NULL else ggplot2::position_dodge(width = 0.8)
  p <- ggplot2::ggplot(summ, ggplot2::aes(x = !!x, y = mean))
  if (is.null(fill_q)) {
    p <- p +
      ggplot2::geom_col(width = 0.72, fill = "#2c7fb8", colour = "#1a1a1a", linewidth = 0.25) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                             width = 0.22, linewidth = 0.7, colour = "#1a1a1a") +
      ggplot2::geom_point(
        data = df, ggplot2::aes(y = !!y),
        size = 2.2, alpha = 0.45, colour = "#1a1a1a",
        position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 42)
      )
  } else {
    p <- p +
      ggplot2::aes(fill = !!fill_q) +
      ggplot2::geom_col(position = dodge, width = 0.72, colour = "#1a1a1a", linewidth = 0.25) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                             position = dodge, width = 0.22, linewidth = 0.7,
                             colour = "#1a1a1a") +
      ggplot2::geom_point(
        data = df, ggplot2::aes(y = !!y, colour = !!fill_q),
        size = 2.2, alpha = 0.45, show.legend = FALSE,
        position = ggplot2::position_jitterdodge(jitter.width = 0.08, dodge.width = 0.8, seed = 42)
      )
    if (!is.null(pal)) {
      p <- p +
        ggplot2::scale_fill_manual(values = pal, drop = FALSE, name = NULL) +
        ggplot2::scale_colour_manual(values = pal, drop = FALSE, guide = "none")
    }
  }
  y_scale <- if (isTRUE(percent)) {
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                                expand = ggplot2::expansion(mult = c(0, 0.08)))
  } else {
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 4),
                                expand = ggplot2::expansion(mult = c(0, 0.08)))
  }
  p <- p +
    y_scale +
    ggplot2::labs(x = NULL, y = ylab, caption = caption) +
    theme_sc() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = if (isTRUE(narrow)) 12 else 14,
        angle = if (isTRUE(narrow)) 28 else 0,
        hjust = if (isTRUE(narrow)) 1 else 0.5
      )
    )
  if (!is.null(fill_q)) p <- p + sc_one_row_legend()
  p
}

sc_plot_downtime <- function(downtime, narrow = FALSE) {
  d <- downtime %>%
    dplyr::mutate(station = factor(.data$short, levels = unique(.data$short[match(sc_reliability_catalog()$resource, .data$resource)])))
  sc_plot_mean_ci(
    d, station, downtime_fraction,
    ylab = "Downtime fraction",
    percent = TRUE,
    caption = sc_caption("Bars = mean across replications; error bars = 95% CI. Faint points = each replication. Fraction is repair hours divided by capacity \u00d7 horizon (calendar time)."),
    narrow = narrow
  )
}

sc_plot_wait_reasons <- function(waits, narrow = FALSE) {
  labs <- sc_wait_reason_levels()
  pal <- stats::setNames(labs$color, labs$label)
  d <- sc_wait_reasons_long(waits) %>%
    dplyr::mutate(station = factor(.data$short, levels = sc_reliability_catalog()$short))
  # Facet by reason with free y-scales: on a low-volume line, starved
  # (idle) hours dwarf blocked / technician / repair time.
  sc_plot_mean_ci(
    d, station, hours, fill = "label",
    ylab = "Hours per run",
    pal = pal,
    caption = sc_caption("Bars = mean across replications; error bars = 95% CI. Blocked = time units wait to enter the station. Waiting for technicians = time holding the station for the labour pool. Down for repair = equipment repair, including any wait for a maintainer. Starved = idle server-hours with an empty queue."),
    # Facets halve the width, so always angle the station labels.
    narrow = TRUE
  ) +
    ggplot2::facet_wrap(~label, ncol = if (isTRUE(narrow)) 1 else 2, scales = "free_y") +
    ggplot2::guides(fill = "none")
}

# Two-scenario downtime / repair view (Compare): dodged calendar downtime
# fraction by station, plus a caption that points at repair counts.
sc_plot_compare_downtime <- function(downtime, narrow = FALSE) {
  if (is.null(downtime) || !nrow(downtime)) {
    return(ggplot2::ggplot() + ggplot2::theme_void(base_size = 16) +
             ggplot2::labs(subtitle = "No downtime data to plot."))
  }
  pal <- c("#2c7fb8", "#d95f02", "#1b9e77", "#756bb1")
  sets <- unique(as.character(downtime$set))
  pal <- stats::setNames(pal[seq_along(sets)], sets)
  d <- downtime %>%
    dplyr::mutate(
      station = factor(.data$short, levels = sc_reliability_catalog()$short),
      set = factor(.data$set, levels = sets)
    )
  sc_plot_mean_ci(
    d, station, downtime_fraction, fill = "set",
    ylab = "Downtime fraction",
    percent = TRUE,
    pal = pal,
    caption = sc_caption("Bars = mean across replications; error bars = 95% CI. Faint points = each replication. Downtime fraction is repair hours divided by capacity \u00d7 horizon."),
    narrow = narrow
  )
}

# Daily (or coarser) WIP step for every replication, used by Simulate and Compare.
sc_wip_grid <- function(units, horizon, dt = 24) {
  if (is.null(units) || !nrow(units)) {
    return(tibble::tibble(rep = integer(), time_h = numeric(), wip = numeric()))
  }
  if (!"rep" %in% names(units)) units$rep <- 1L
  grid <- seq(0, horizon, by = dt)
  if (!horizon %in% grid) grid <- c(grid, horizon)
  purrr::map_dfr(sort(unique(units$rep)), function(r) {
    u <- units %>% dplyr::filter(.data$rep == r)
    ev <- dplyr::bind_rows(
      tibble::tibble(time = u$start_time, d = 1),
      tibble::tibble(time = u$end_time[u$status != "wip"], d = -1)
    ) %>%
      dplyr::arrange(.data$time) %>%
      dplyr::mutate(level = cumsum(.data$d))
    tibble::tibble(
      rep = r,
      time_h = grid,
      wip = sc_step_at_metrics(ev$time, ev$level, grid)
    )
  })
}

# Cumulative releases ("demand") vs units that have left the line (shipped or scrapped).
sc_demand_grid <- function(units, horizon, dt = 24) {
  if (is.null(units) || !nrow(units)) {
    return(tibble::tibble(rep = integer(), time_h = numeric(),
                          demand = numeric(), completed = numeric()))
  }
  if (!"rep" %in% names(units)) units$rep <- 1L
  grid <- seq(0, horizon, by = dt)
  if (!horizon %in% grid) grid <- c(grid, horizon)
  purrr::map_dfr(sort(unique(units$rep)), function(r) {
    u <- units %>% dplyr::filter(.data$rep == r)
    rel <- sort(u$start_time)
    fin <- sort(u$end_time[u$status != "wip"])
    tibble::tibble(
      rep = r,
      time_h = grid,
      demand = findInterval(grid, rel),
      completed = findInterval(grid, fin)
    )
  })
}

# Step-function helper (same rule as sc_step_at on the Analyze page).
sc_step_at_metrics <- function(time, level, grid) {
  if (!length(time)) return(rep(0, length(grid)))
  t <- c(0, time)
  y <- c(0, level)
  keep <- !duplicated(t, fromLast = TRUE)
  t <- t[keep]
  y <- y[keep]
  i <- findInterval(grid, t)
  i[i < 1] <- 1L
  y[i]
}

# Tidy tables for the Simulate / Compare download tabs.
sc_export_tables <- function(runs, params = sc_default_params()) {
  kpis <- sc_summarise(runs, params)$per_rep
  units <- purrr::map_dfr(runs, sc_units)
  arrivals <- purrr::map_dfr(runs, function(run) {
    run$arrivals %>% dplyr::mutate(rep = run$rep %||% 1L)
  })
  resources <- purrr::map_dfr(runs, function(run) {
    run$resources %>% dplyr::mutate(rep = run$rep %||% 1L)
  })
  arrivals_res <- purrr::map_dfr(runs, function(run) {
    (run$arrivals_res %||% tibble::tibble()) %>% dplyr::mutate(rep = run$rep %||% 1L)
  })
  failures <- purrr::map_dfr(runs, function(run) {
    (run$failures %||% sc_reliability_empty_failures()) %>%
      dplyr::mutate(rep = run$rep %||% 1L)
  })
  repairs <- purrr::map_dfr(runs, function(run) {
    (run$repairs %||% sc_reliability_empty_repairs()) %>%
      dplyr::mutate(rep = run$rep %||% 1L)
  })
  downtime <- sc_downtime_by_rep(runs, params)
  list(
    units = units,
    arrivals = arrivals,
    arrivals_res = arrivals_res,
    resources = resources,
    kpis = kpis,
    failures = failures,
    repairs = repairs,
    downtime = downtime
  )
}

sc_export_catalog <- function() {
  tibble::tribble(
    ~id,            ~file,                    ~title,                      ~blurb,
    "units",        "units.csv",              "Units",                     "One row per unit: variant, status, cycle time, touch labor, and the equipment-downtime hours held against that unit.",
    "kpis",         "kpis_by_rep.csv",        "KPIs by replication",       "Headline metrics (throughput, cycle time, HPU, labor utilization, WIP) for each replication.",
    "arrivals",     "arrivals.csv",           "Arrivals",                  "simmer arrival monitor: one row per unit (and kit) arrival, with start and end times.",
    "arrivals_res", "resource_arrivals.csv",  "Per-resource arrivals",     "Each seize of a station, the technician pool, or maintenance, with activity and waiting time.",
    "resources",    "resource_monitor.csv",   "Resource monitor",          "Time-stamped server, queue and capacity changes for every resource.",
    "failures",     "failures.csv",           "Failure events",            "One row per equipment breakdown: station, onset time, unit, and replication.",
    "repairs",      "repairs.csv",            "Repair events",             "One row per repair: station, start, wall-clock duration (including any wait for a maintainer), and sampled repair time.",
    "downtime",     "downtime_by_rep.csv",    "Downtime by replication",   "Repair hours, repair count, calendar downtime fraction, and operational availability by station and replication."
  )
}


# Download buttons for a Simulate or Compare export bundle.
sc_download_buttons_ui <- function(prefix, extra = NULL) {
  cat <- sc_export_catalog()
  buttons <- purrr::pmap(cat, function(id, file, title, blurb, ...) {
    shiny::div(
      class = "mb-3",
      shiny::tags$div(
        shiny::tags$strong(title),
        shiny::downloadButton(paste0(prefix, id), "Download .csv",
                              class = "btn-outline-secondary btn-sm ms-2")
      ),
      shiny::p(class = "small text-muted mb-0", blurb)
    )
  })
  shiny::tagList(
    extra,
    buttons,
    shiny::div(
      class = "mt-3",
      shiny::downloadButton(paste0(prefix, "all"), "Download all (zip)", class = "btn-secondary")
    )
  )
}

# Register downloadHandler entries for every catalog table plus a zip.
# `getter` is a function that returns a named list of tibbles (sc_export_tables).
sc_bind_downloads <- function(output, prefix, getter, zip_name = "spacecraft_run") {
  cat <- sc_export_catalog()
  purrr::pwalk(cat, function(id, file, ...) {
    output[[paste0(prefix, id)]] <- shiny::downloadHandler(
      filename = function() file,
      content = function(path) {
        tbl <- getter()
        shiny::validate(shiny::need(!is.null(tbl) && id %in% names(tbl), "Run the simulation first."))
        readr::write_csv(tbl[[id]], path)
      }
    )
  })
  output[[paste0(prefix, "all")]] <- shiny::downloadHandler(
    filename = function() paste0(zip_name, ".zip"),
    content = function(path) {
      tbl <- getter()
      shiny::validate(shiny::need(!is.null(tbl), "Run the simulation first."))
      td <- tempfile("sc_export_")
      dir.create(td)
      on.exit(unlink(td, recursive = TRUE), add = TRUE)
      files <- purrr::pmap_chr(cat, function(id, file, ...) {
        if (!id %in% names(tbl) || is.null(tbl[[id]])) return(NA_character_)
        dest <- file.path(td, file)
        readr::write_csv(tbl[[id]], dest)
        dest
      })
      files <- files[!is.na(files)]
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(path, files = basename(files), root = td)
      } else {
        owd <- setwd(td)
        on.exit(setwd(owd), add = TRUE)
        utils::zip(path, files = basename(files), flags = "-q")
      }
    }
  )
}

# Flow + export bundle for one Compare scenario.
sc_compare_flow <- function(runs, params, label) {
  units <- purrr::map_dfr(runs, sc_units)
  horizon <- params$sim_hours %||% runs[[1]]$horizon
  qgrid <- seq(0, horizon, by = 24)
  if (!horizon %in% qgrid) qgrid <- c(qgrid, horizon)
  queues <- purrr::map_dfr(runs, function(run) {
    sc_queue_grid(run, qgrid) %>% dplyr::mutate(rep = run$rep %||% 1L)
  })
  exports <- sc_export_tables(runs, params)
  # Keep Compare preloads small: replace the full event log with a daily
  # snapshot, and drop per-resource arrival rows (still available on Simulate).
  exports$resources <- purrr::map_dfr(runs, function(run) {
    mon <- run$resources
    grid <- seq(0, run$horizon, by = 24)
    if (!run$horizon %in% grid) grid <- c(grid, run$horizon)
    purrr::map_dfr(unique(mon$resource), function(res) {
      ev <- mon %>% dplyr::filter(.data$resource == res)
      tibble::tibble(
        rep = run$rep %||% 1L,
        resource = res,
        time_h = grid,
        server = sc_step_at_metrics(ev$time, ev$server, grid),
        queue = sc_step_at_metrics(ev$time, ev$queue, grid),
        capacity = sc_step_at_metrics(ev$time, ev$capacity, grid)
      )
    })
  })
  exports$arrivals_res <- NULL
  list(
    # `.env$label` is the scenario name; several tables already have a
    # station `label` column, and a bare `label` would overwrite `set` with that.
    wip = sc_wip_grid(units, horizon) %>% dplyr::mutate(set = .env$label),
    demand = sc_demand_grid(units, horizon) %>% dplyr::mutate(set = .env$label),
    queues = queues %>% dplyr::mutate(set = .env$label),
    downtime = sc_downtime_by_rep(runs, params) %>% dplyr::mutate(set = .env$label),
    exports = exports
  )
}

sc_compare_bind_flows <- function(flows) {
  if (!length(flows)) return(NULL)
  list(
    wip = purrr::map_dfr(flows, "wip"),
    demand = purrr::map_dfr(flows, "demand"),
    queues = purrr::map_dfr(flows, "queues"),
    downtime = purrr::map_dfr(flows, "downtime"),
    exports = flows
  )
}
