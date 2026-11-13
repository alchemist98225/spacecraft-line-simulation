# spacecraft_analyze.R
# Diagnostics that explain why the line performs as it does: how a shipped
# unit's cycle time is spent, which resource is the bottleneck, and how the
# busiest queues move over the horizon.

# Station names as they appear on the page (plain English, with a short form
# for tight legends).
sc_resource_catalog <- function() {
  tibble::tribble(
    ~resource,        ~label,                          ~short,
    "integration",    "Integration stations",          "Integration",
    "func_test",      "Functional test",               "Func. test",
    "chamber",        "Environmental test chamber",    "Chamber",
    "review_board",   "Material review board",         "MRB",
    "acceptance",     "Final acceptance",              "Acceptance",
    "techs",          "Technicians",                   "Techs"
  )
}

# Cycle-time parts. Colour follows the app theme: blue for forward work,
# light blue for waiting, yellow for fail/rework, orange for the MRB hold,
# and grey for kitting delay.
sc_time_parts <- function() {
  tibble::tribble(
    ~part,     ~label,              ~short,     ~color,    ~tip,
    "work",    "Working",                  "Working",  "#2c7fb8", "Time a unit is being worked on at a station (integration, test, or acceptance), not counting rework after a failure.",
    "queue",   "Waiting for a station",    "Station",  "#9ecae1", "Time waiting for a station to become free, not counting rework after a failure.",
    "tech",    "Waiting for technicians",  "Techs",    "#17becf", "Time holding a station and waiting for technicians from the shared pool, not counting rework after a failure.",
    "rework",  "Failed / rework",          "Rework",   "#e0a032", "Queue and work time after a unit rolls back to Integration following a failure or an MRB rework decision.",
    "mrb",     "MRB hold",                 "MRB",      "#d95f02", "Time waiting for the material review board plus the review itself.",
    "kit",     "Kitting delay",            "Kitting",  "#6c757d", "Time from release until the unit's long-lead kit arrives."
  )
}

# Step-function value at each grid time, given (time, level) change points.
# The level at t is the last recorded value at or before t, and 0 before the first.
sc_step_at <- function(time, level, grid) {
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

# Per shipped unit: hours in each cycle-time part. The five parts are meant
# to add up to cycle time (release to ship).
sc_unit_breakdown <- function(run) {
  units <- sc_units(run) %>% dplyr::filter(status == "shipped")
  if (!nrow(units)) {
    return(units %>% dplyr::mutate(work_h = numeric(), queue_h = numeric(), tech_h = numeric(),
                                   rework_h = numeric(), mrb_h = numeric(), kit_h = numeric()))
  }
  arrivals_res <- run$arrivals_res %||% tibble::tibble(
    name = character(), start_time = numeric(), end_time = numeric(),
    activity_time = numeric(), resource = character()
  )
  marks <- run$attributes %>% dplyr::filter(.data$key == "rework_mark")
  marks <- if (!nrow(marks)) {
    tibble::tibble(name = character(), first_rework = numeric())
  } else {
    marks %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(first_rework = min(.data$time), .groups = "drop")
  }
  visits <- arrivals_res %>%
    dplyr::filter(.data$name %in% units$name) %>%
    dplyr::left_join(marks, by = "name") %>%
    dplyr::mutate(
      wait_h = pmax(0, .data$end_time - .data$start_time - .data$activity_time),
      act_h = pmax(0, .data$activity_time),
      part = dplyr::case_when(
        .data$resource == "review_board" ~ "mrb",
        is.finite(.data$first_rework) & .data$start_time >= .data$first_rework - 1e-8 ~ "rework",
        TRUE ~ "process"
      )
    )
  # Technician visits are recorded separately. Waiting for technicians happens
  # while the unit already holds the station, so that wait sits inside the
  # station's activity_time; subtract it from "working" and show it on its own.
  tech_wait <- visits %>%
    dplyr::filter(.data$resource == "techs") %>%
    dplyr::mutate(
      tech_wait_h = dplyr::if_else(.data$part == "process", .data$wait_h, 0)
    ) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(tech_h = sum(.data$tech_wait_h, na.rm = TRUE), .groups = "drop")
  from_res <- visits %>%
    dplyr::filter(.data$resource != "techs") %>%
    dplyr::mutate(
      work_part   = dplyr::if_else(.data$part == "process", .data$act_h, 0),
      queue_part  = dplyr::if_else(.data$part == "process", .data$wait_h, 0),
      rework_part = dplyr::if_else(.data$part == "rework",  .data$act_h + .data$wait_h, 0),
      mrb_part    = dplyr::if_else(.data$part == "mrb",     .data$act_h + .data$wait_h, 0)
    ) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      work_h   = sum(.data$work_part, na.rm = TRUE),
      queue_h  = sum(.data$queue_part, na.rm = TRUE),
      rework_h = sum(.data$rework_part, na.rm = TRUE),
      mrb_h    = sum(.data$mrb_part, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(tech_wait, by = "name") %>%
    dplyr::mutate(
      tech_h = dplyr::coalesce(.data$tech_h, 0),
      work_h = pmax(0, .data$work_h - .data$tech_h)
    )
  units %>%
    dplyr::left_join(from_res, by = "name") %>%
    dplyr::mutate(
      dplyr::across(c(work_h, queue_h, tech_h, rework_h, mrb_h), ~ dplyr::coalesce(.x, 0)),
      kit_h = dplyr::coalesce(.data$kit_wait_h, 0)
    )
}

# Time-average utilization and queue for every resource in one replication.
sc_resource_use <- function(run) {
  cat <- sc_resource_catalog()
  horizon <- run$horizon
  mon <- run$resources
  cap <- mon %>%
    dplyr::group_by(resource) %>%
    dplyr::summarise(capacity = dplyr::last(.data$capacity), .groups = "drop")
  use <- purrr::map_dfr(cat$resource, function(res) {
    ev <- mon %>% dplyr::filter(.data$resource == res)
    cap_n <- cap$capacity[match(res, cap$resource)]
    if (length(cap_n) != 1 || is.na(cap_n)) cap_n <- 1
    if (!nrow(ev)) {
      return(tibble::tibble(resource = res, util = 0, queue_avg = 0, capacity = cap_n))
    }
    tibble::tibble(
      resource = res,
      util = sc_util_from_monitor(ev, horizon, capacity_fallback = cap_n),
      queue_avg = sc_time_average(ev$time, ev$queue, horizon),
      capacity = cap_n
    )
  })
  use %>% dplyr::left_join(cat, by = "resource")
}

# Queue length on a shared time grid for one replication.
sc_queue_grid <- function(run, grid) {
  cat <- sc_resource_catalog()
  mon <- run$resources
  purrr::map_dfr(cat$resource, function(res) {
    ev <- mon %>% dplyr::filter(.data$resource == res)
    tibble::tibble(
      resource = res,
      time_h = grid,
      queue = sc_step_at(ev$time, ev$queue, grid)
    )
  }) %>% dplyr::left_join(cat, by = "resource")
}

# Plain-English summary from the aggregated numbers.
sc_analyze_story <- function(breakdown, resources, cycle_mean_h) {
  bot <- resources %>% dplyr::filter(.data$is_bottleneck) %>% dplyr::slice(1)
  util_pct <- round(100 * bot$util)
  q <- bot$queue_avg
  q_txt <- if (q < 0.05) "almost no queue" else sprintf("average queue %.1f units", q)
  constraint <- if (bot$util >= 0.75 || q >= 0.5) {
    sprintf("%s is the constraint: %s%% busy, %s.", bot$label, util_pct, q_txt)
  } else if (bot$util >= 0.4) {
    sprintf("%s is the busiest station, at %s%% busy, with %s. No station is fully loaded.",
            bot$label, util_pct, q_txt)
  } else {
    sprintf("The line has spare capacity. The busiest station is %s at %s%% busy, and queues stay small (%s).",
            bot$label, util_pct, q_txt)
  }
  shares <- breakdown %>%
    dplyr::filter(.data$variant == "Overall") %>%
    dplyr::mutate(share = .data$mean_h / sum(.data$mean_h))
  share_of <- function(part) {
    v <- shares$share[shares$part == part]
    if (!length(v)) 0 else v[[1]]
  }
  hours_of <- function(part) {
    v <- shares$mean_h[shares$part == part]
    if (!length(v)) 0 else v[[1]]
  }
  parts <- sc_time_parts()
  phrase <- function(part) {
    sprintf("%s%% %s (%.1f h)",
            round(100 * share_of(part)),
            switch(part,
                   work = "working",
                   queue = "waiting for a station",
                   tech = "waiting for technicians",
                   rework = "on failed / rework",
                   mrb = "in an MRB hold",
                   kit = "waiting for kits",
                   "on other delays"),
            hours_of(part))
  }
  bits <- purrr::map_chr(parts$part, phrase)
  if (length(bits) > 1) bits[[length(bits)]] <- paste("and", bits[[length(bits)]])
  mix <- paste0(
    "Of the mean cycle time (", sprintf("%.0f", cycle_mean_h), " h), units spend ",
    paste(bits, collapse = ", "), "."
  )
  # Call out the largest non-working slice and any variant that stands out.
  other <- shares %>% dplyr::filter(.data$part != "work") %>% dplyr::slice_max(.data$share, n = 1, with_ties = FALSE)
  callout <- sprintf("The largest slice besides working is %s.", tolower(parts$label[parts$part == other$part[[1]]]))
  by_var <- breakdown %>%
    dplyr::filter(.data$variant != "Overall", .data$part == "work") %>%
    dplyr::select(variant, work_h = mean_h)
  if (nrow(by_var) >= 2 && diff(range(by_var$work_h)) >= 8) {
    long <- by_var$variant[which.max(by_var$work_h)]
    callout <- paste0(
      callout,
      sprintf(" Variant %s has the longest working time, which matches its longer integration route.", long)
    )
  }
  paste(constraint, mix, callout)
}

# Roll replications up into the tables and ribbons the page draws.
sc_analyze <- function(runs, params = sc_default_params()) {
  stopifnot(length(runs) >= 1)
  parts <- sc_time_parts()
  variant_names <- params$variants$variant
  units <- purrr::map_dfr(runs, sc_unit_breakdown)
  units <- units %>%
    dplyr::mutate(variant = factor(variant_names[.data$variant], levels = variant_names))
  sum_parts <- function(d) {
    d %>%
      dplyr::summarise(
        work_h = mean(.data$work_h), queue_h = mean(.data$queue_h),
        tech_h = mean(.data$tech_h), rework_h = mean(.data$rework_h),
        mrb_h = mean(.data$mrb_h), kit_h = mean(.data$kit_h),
        cycle_h = mean(.data$cycle_h),
        n = dplyr::n(), .groups = "drop"
      )
  }
  wide <- dplyr::bind_rows(
    units %>% dplyr::group_by(variant, .drop = FALSE) %>% sum_parts(),
    units %>% dplyr::mutate(variant = factor("Overall", levels = c("Overall", variant_names))) %>%
      dplyr::group_by(variant) %>% sum_parts()
  )
  breakdown <- wide %>%
    tidyr::pivot_longer(c(work_h, queue_h, tech_h, rework_h, mrb_h, kit_h),
                        names_to = "part_h", values_to = "mean_h") %>%
    dplyr::mutate(part = sub("_h$", "", .data$part_h),
                  share = dplyr::if_else(.data$cycle_h > 0, .data$mean_h / .data$cycle_h, NA_real_)) %>%
    dplyr::select(-"part_h") %>%
    dplyr::left_join(parts, by = "part") %>%
    dplyr::mutate(part = factor(.data$part, levels = parts$part),
                  variant = factor(.data$variant, levels = c("Overall", variant_names)))

  use <- purrr::map_dfr(runs, function(run) {
    sc_resource_use(run) %>% dplyr::mutate(rep = run$rep %||% 1L)
  })
  resources <- use %>%
    dplyr::group_by(resource, label, short) %>%
    dplyr::summarise(
      util = mean(.data$util),
      queue_avg = mean(.data$queue_avg),
      capacity = dplyr::first(.data$capacity),
      .groups = "drop"
    ) %>%
    # The shared technician pool is reported as labor utilization; the
    # bottleneck flag is the busiest station.
    dplyr::mutate(
      is_bottleneck = .data$resource != "techs" &
        .data$util == max(.data$util[.data$resource != "techs"], na.rm = TRUE) &
        .data$util > 0
    ) %>%
    dplyr::arrange(dplyr::desc(.data$util))
  # If two resources tie at the max, keep the first (already sorted) as the only flag.
  if (sum(resources$is_bottleneck) > 1) {
    resources$is_bottleneck <- seq_len(nrow(resources)) == which(resources$is_bottleneck)[[1]]
  }

  horizon <- runs[[1]]$horizon
  grid <- seq(0, horizon, length.out = 181)
  qlong <- purrr::map_dfr(runs, function(run) {
    sc_queue_grid(run, grid) %>% dplyr::mutate(rep = run$rep %||% 1L)
  })
  q_avg <- qlong %>%
    dplyr::group_by(resource, label, short) %>%
    dplyr::summarise(queue_avg = mean(.data$queue), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$queue_avg))
  top_n <- min(3L, sum(q_avg$queue_avg > 0.01))
  if (top_n < 2) top_n <- min(3L, nrow(q_avg))
  top_res <- q_avg$resource[seq_len(top_n)]
  # Always include the bottleneck in the ribbon if it is not already there.
  bot_res <- resources$resource[resources$is_bottleneck]
  if (length(bot_res) && !bot_res[[1]] %in% top_res) {
    top_res <- c(utils::head(top_res, 2), bot_res[[1]])
  }
  queues <- qlong %>%
    dplyr::filter(.data$resource %in% top_res) %>%
    dplyr::group_by(resource, label, short, time_h) %>%
    dplyr::summarise(
      mean = mean(.data$queue),
      p10 = unname(stats::quantile(.data$queue, 0.10, names = FALSE, na.rm = TRUE)),
      p90 = unname(stats::quantile(.data$queue, 0.90, names = FALSE, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(resource = factor(.data$resource, levels = top_res))

  cycle_mean_h <- mean(units$cycle_h, na.rm = TRUE)
  story <- sc_analyze_story(breakdown, resources, cycle_mean_h)
  list(
    params = params,
    breakdown = breakdown,
    resources = resources,
    queues = queues,
    story = story,
    cycle_mean_h = cycle_mean_h,
    bottleneck = resources %>% dplyr::filter(.data$is_bottleneck) %>% dplyr::slice(1),
    meta = list(n_reps = length(runs), horizon = horizon)
  )
}

sc_preload_analyze_path <- function() {
  cand <- c(
    file.path("app", "data", "preload_analyze.rds"),
    file.path("data", "preload_analyze.rds"),
    file.path("..", "data", "preload_analyze.rds")
  )
  hit <- cand[file.exists(cand)]
  if (!length(hit)) return(NULL)
  hit[[1]]
}

sc_load_preload_analyze <- function(path = sc_preload_analyze_path()) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  readRDS(path)
}
