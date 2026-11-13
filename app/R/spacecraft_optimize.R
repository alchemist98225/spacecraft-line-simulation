# spacecraft_optimize.R
# Optimize-page helpers on top of the GA adapter: presets, a step-wise search
# and recheck (so the page can show every generation and Cancel between
# steps), a bundle with P10–P90 recheck results, and a plain-English story.

sc_opt_presets <- function() {
  list(
    quick = list(pop_size = 6L, n_gen = 4L, n_reps = 2L,
                 elite_n = 3L, elite_reps = 15L, label = "Quick"),
    thorough = list(pop_size = 10L, n_gen = 8L, n_reps = 3L,
                    elite_n = 5L, elite_reps = 20L, label = "Thorough"),
    preload = list(pop_size = 6L, n_gen = 4L, n_reps = 2L,
                   elite_n = 3L, elite_reps = 20L, label = "Preload")
  )
}

# Default cost per extra station (hours of cycle time, or units of throughput).
# A recommendation has to earn a full hour of cycle time (or a full shipped
# unit) per extra station; cheaper costs tended to recommend adding several
# stations for a small gain. A technician is charged at a quarter of that,
# because a person is cheaper than a new bay or chamber.
sc_opt_default_cost <- function() 1
sc_opt_default_tech_cost <- function() 0.25

# GA operator defaults shown in the Algorithm settings accordion.
sc_opt_algo_defaults <- function() {
  list(p_mut = 0.25, p_cx = 0.8, n_elite = 1L)
}

# Upper bound on the recheck pool: current line + top N + one-station
# neighbors, before duplicates are dropped.
sc_opt_recheck_n_max <- function(elite_n, defs = sc_knob_defs()) {
  as.integer(elite_n) + 1L + length(sc_opt_neighbors(defs))
}

sc_opt_change_label <- function(genome, baseline, defs, narrow = FALSE) {
  genome <- as.integer(genome)
  baseline <- as.integer(baseline)
  if (identical(genome, baseline)) {
    return(if (isTRUE(narrow)) "No change" else "Current line (no change)")
  }
  wide_labs <- c(integration = "integration station",
                 func_test = "functional test stand",
                 chamber = "environmental chamber",
                 review_board = "MRB board",
                 acceptance = "acceptance stand",
                 techs = "technician per shift")
  bits <- purrr::pmap_chr(
    list(genome, baseline, defs$name, defs$short),
    function(g, b, name, short) {
      if (identical(g, b)) return(NA_character_)
      d <- g - b
      if (isTRUE(narrow)) return(sprintf("%+d %s", d, short))
      if (identical(name, "techs")) {
        noun <- if (abs(d) == 1L) "technician per shift" else "technicians per shift"
      } else {
        lab <- unname(wide_labs[[name]])
        noun <- if (abs(d) == 1L) lab else paste0(lab, "s")
      }
      if (d > 0) sprintf("add %d %s", d, noun) else sprintf("drop %d %s", abs(d), noun)
    }
  )
  bits <- bits[!is.na(bits)]
  paste(bits, collapse = if (isTRUE(narrow)) "; " else ", ")
}

# Recheck pool: the current line first, then the top unique search genomes,
# then every one-station change from the current line. Duplicates dropped.
# One-station neighbors are included even if a noisy short search ranked
# them poorly, so the recheck can still catch a real improvement.
sc_opt_recheck_pool <- function(evaluated, defs, elite_n = 3L) {
  base_key <- sc_genome_key(defs$default)
  ranked <- evaluated %>%
    dplyr::arrange(.data$fitness, .data$cost_term) %>%
    dplyr::distinct(.data$key, .keep_all = TRUE)
  top <- ranked %>%
    dplyr::filter(.data$key != base_key) %>%
    dplyr::slice_head(n = as.integer(elite_n))
  base <- ranked %>% dplyr::filter(.data$key == base_key)
  if (!nrow(base)) {
    base <- tibble::tibble(key = base_key, genome = list(as.integer(defs$default)),
                           fitness = NA_real_)
  }
  from_eval <- function(rows) rows %>% dplyr::transmute(key, genome, search_fitness = .data$fitness)
  neigh <- purrr::map_dfr(sc_opt_neighbors(defs), function(g) {
    key <- sc_genome_key(g)
    hit <- ranked %>% dplyr::filter(.data$key == .env$key)
    if (nrow(hit)) from_eval(hit)
    else tibble::tibble(key = key, genome = list(as.integer(g)), search_fitness = NA_real_)
  })
  dplyr::bind_rows(from_eval(base), from_eval(top), neigh) %>%
    dplyr::distinct(.data$key, .keep_all = TRUE)
}

# One-step neighbors of the current line (plus one, then minus one, in knob
# order). These are always included in the recheck pool, even though the
# first search generation is now a random population (not the current line).
sc_opt_neighbors <- function(defs) {
  base <- as.integer(defs$default)
  out <- list()
  for (i in seq_along(base)) {
    if (base[[i]] < defs$hi[[i]]) {
      g <- base
      g[[i]] <- base[[i]] + 1L
      out <- c(out, list(g))
    }
  }
  for (i in seq_along(base)) {
    if (base[[i]] > defs$lo[[i]]) {
      g <- base
      g[[i]] <- base[[i]] - 1L
      out <- c(out, list(g))
    }
  }
  out
}

sc_opt_new_state <- function(params, pop_size = 6L, n_gen = 4L, n_reps = 2L,
                             elite_n = 3L, elite_reps = 15L, base_seed = 42L,
                             capacity_cost = 1, objective = "cycle",
                             p_mut = 0.25, p_cx = 0.8, n_elite = 1L,
                             tech_cost = 0.25) {
  defs <- sc_knob_defs(params)
  set.seed(as.integer(base_seed))
  pop <- sc_opt_random_pop(defs, pop_size)
  # Score the current line for the dashed reference on the chart, but keep it
  # out of the breeding population so generation 1 is not already the baseline.
  baseline_row <- sc_evaluate_genome(
    defs$default, params = params, defs = defs, n_reps = n_reps,
    base_seed = as.integer(base_seed), capacity_cost = capacity_cost,
    objective = objective, tech_cost = tech_cost
  )
  list(
    params = params, defs = defs, pop = pop,
    evaluated = baseline_row,
    history = tibble::tibble(),
    gen_i = 1L,
    n_gen = as.integer(n_gen),
    pop_size = as.integer(pop_size),
    n_reps = as.integer(n_reps),
    elite_n = as.integer(elite_n),
    elite_reps = as.integer(elite_reps),
    base_seed = as.integer(base_seed),
    capacity_cost = as.numeric(capacity_cost),
    tech_cost = as.numeric(tech_cost),
    objective = objective,
    p_mut = as.numeric(p_mut),
    p_cx = as.numeric(p_cx),
    n_elite = as.integer(n_elite),
    phase = "search",
    pool = NULL,
    recheck = tibble::tibble(),
    i_recheck = 0L,
    cancelled = FALSE,
    t0 = Sys.time()
  )
}

# Evaluate the current population, record generation gen_i (1-based), breed the
# next population. After the last generation, builds the recheck pool and
# switches phase to "recheck".
sc_opt_search_step <- function(state) {
  ev <- state$evaluated
  new <- purrr::discard(state$pop, ~ sc_genome_key(.x) %in% ev$key) %>% unique()
  if (length(new)) {
    ev <- dplyr::bind_rows(ev, purrr::map_dfr(new, sc_evaluate_genome,
      params = state$params, defs = state$defs, n_reps = state$n_reps,
      base_seed = state$base_seed, capacity_cost = state$capacity_cost,
      objective = state$objective, tech_cost = state$tech_cost))
  }
  fitness <- ev$fitness[match(purrr::map_chr(state$pop, sc_genome_key), ev$key)]
  best <- state$pop[[which.min(fitness)]]
  row <- tibble::tibble(
    generation = state$gen_i,
    best_fitness = min(fitness),
    mean_fitness = mean(fitness),
    n_unique = dplyr::n_distinct(purrr::map_chr(state$pop, sc_genome_key))
  )
  state$evaluated <- ev
  state$history <- dplyr::bind_rows(state$history, row)
  if (state$gen_i < state$n_gen) {
    n_elite <- max(1L, min(state$n_elite %||% 1L, state$pop_size - 1L))
    elites <- state$pop[order(fitness)][seq_len(n_elite)]
    p_cx <- state$p_cx %||% 0.8
    offspring <- purrr::map(seq_len(state$pop_size - n_elite), function(i) {
      a <- state$pop[[sc_tournament(fitness)]]
      b <- state$pop[[sc_tournament(fitness)]]
      child <- if (stats::runif(1) < p_cx) sc_crossover(a, b) else if (stats::runif(1) < 0.5) a else b
      sc_mutate(child, state$defs, state$p_mut)
    })
    state$pop <- c(elites, offspring)
    state$gen_i <- state$gen_i + 1L
  } else {
    state$pool <- sc_opt_recheck_pool(ev, state$defs, state$elite_n)
    state$i_recheck <- 1L
    state$phase <- "recheck"
  }
  state
}

sc_opt_recheck_step <- function(state) {
  i <- state$i_recheck
  g <- state$pool$genome[[i]]
  ev <- sc_evaluate_genome(
    g, params = state$params, defs = state$defs, n_reps = state$elite_reps,
    base_seed = state$base_seed, capacity_cost = state$capacity_cost,
    objective = state$objective, keep_reps = TRUE, tech_cost = state$tech_cost
  )
  ev$search_fitness <- state$pool$search_fitness[[i]]
  ev$is_baseline <- ev$key == sc_genome_key(state$defs$default)
  ev$is_best <- FALSE
  ev$rank <- NA_integer_
  ev$change <- sc_opt_change_label(g, state$defs$default, state$defs, narrow = FALSE)
  ev$change_short <- sc_opt_change_label(g, state$defs$default, state$defs, narrow = TRUE)
  state$recheck <- dplyr::bind_rows(state$recheck, ev)
  if (i < nrow(state$pool)) {
    state$i_recheck <- i + 1L
  } else {
    state$phase <- "done"
  }
  state
}


# Display scores in natural units. The GA still minimizes internally
# (fitness = net cycle time, or fitness = -throughput + cost).
sc_opt_higher_is_better <- function(objective) identical(objective, "throughput")

sc_opt_display_score <- function(shipped, cycle_mean_h, cost_term,
                                 objective = "cycle") {
  cost_term <- as.numeric(cost_term)
  if (identical(objective, "throughput")) {
    as.numeric(shipped) - cost_term
  } else {
    as.numeric(cycle_mean_h) + cost_term
  }
}

# Convert a minimized fitness to the display score.
sc_opt_score_from_fitness <- function(fitness, objective = "cycle") {
  if (identical(objective, "throughput")) -as.numeric(fitness) else as.numeric(fitness)
}

sc_opt_score_label <- function(objective, narrow = FALSE) {
  if (identical(objective, "cycle")) {
    if (isTRUE(narrow)) "Net cycle (h)" else "Net cycle time (h)"
  } else {
    if (isTRUE(narrow)) "Net throughput" else "Net throughput (units)"
  }
}

sc_opt_score_direction <- function(objective) {
  if (identical(objective, "cycle")) "Lower is better." else "Higher is better."
}

# 90% two-sided Student-t interval for the mean of paired differences.
sc_opt_mean_ci90 <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_))
  m <- mean(x)
  if (length(x) < 2L) return(c(mean = m, lo = NA_real_, hi = NA_real_))
  h <- stats::qt(0.95, length(x) - 1L) * stats::sd(x) / sqrt(length(x))
  c(mean = m, lo = m - h, hi = m + h)
}

# Add net score and paired change-vs-current (mean and 90% interval) using
# per-replication differences under common random numbers. A candidate is
# within noise when that interval includes zero.
sc_opt_attach_paired <- function(recheck, objective = "cycle", capacity_cost = 1,
                                 tech_cost = 0.25) {
  if (is.null(recheck) || !nrow(recheck)) return(recheck)
  cost_term <- if ("cost_term" %in% names(recheck)) {
    recheck$cost_term
  } else {
    as.numeric(recheck$extra_capacity %||% 0) * as.numeric(capacity_cost) +
      as.numeric(recheck$extra_techs %||% 0) * as.numeric(tech_cost)
  }
  recheck$score_mean <- sc_opt_display_score(
    recheck$shipped, recheck$cycle_mean_h, cost_term, objective
  )
  recheck$delta_mean <- NA_real_
  recheck$delta_lo <- NA_real_
  recheck$delta_hi <- NA_real_
  recheck$within_noise <- NA
  recheck$clearly_better <- FALSE
  base_i <- which(recheck$is_baseline %in% TRUE)
  if (!length(base_i) || !"per_rep" %in% names(recheck)) return(recheck)
  base_i <- base_i[[1]]
  bpr <- recheck$per_rep[[base_i]]
  if (is.null(bpr) || !nrow(bpr)) return(recheck)
  b_score <- sc_opt_display_score(
    bpr$shipped, bpr$cycle_mean_h, 0, objective
  )
  b_df <- tibble::tibble(rep = bpr$rep, bscore = b_score)
  higher <- sc_opt_higher_is_better(objective)
  for (i in seq_len(nrow(recheck))) {
    if (isTRUE(recheck$is_baseline[[i]])) {
      recheck$delta_mean[[i]] <- 0
      recheck$delta_lo[[i]] <- 0
      recheck$delta_hi[[i]] <- 0
      recheck$within_noise[[i]] <- FALSE
      recheck$clearly_better[[i]] <- FALSE
      next
    }
    pr <- recheck$per_rep[[i]]
    if (is.null(pr) || !nrow(pr)) next
    c_score <- sc_opt_display_score(
      pr$shipped, pr$cycle_mean_h, cost_term[[i]], objective
    )
    joined <- dplyr::inner_join(
      tibble::tibble(rep = pr$rep, cscore = c_score),
      b_df, by = "rep"
    )
    if (!nrow(joined)) next
    ci <- sc_opt_mean_ci90(joined$cscore - joined$bscore)
    recheck$delta_mean[[i]] <- unname(ci[["mean"]])
    recheck$delta_lo[[i]] <- unname(ci[["lo"]])
    recheck$delta_hi[[i]] <- unname(ci[["hi"]])
    inc0 <- !is.finite(ci[["lo"]]) || !is.finite(ci[["hi"]]) ||
      (ci[["lo"]] <= 0 && ci[["hi"]] >= 0)
    recheck$within_noise[[i]] <- inc0
    recheck$clearly_better[[i]] <- !inc0 &&
      if (higher) unname(ci[["mean"]]) > 0 else unname(ci[["mean"]]) < 0
  }
  recheck
}

sc_opt_format_delta <- function(mean, lo, hi, objective, within_noise = FALSE,
                                is_baseline = FALSE, narrow = FALSE,
                                html = FALSE) {
  if (isTRUE(is_baseline)) return(if (isTRUE(html)) "—" else "—")
  if (!is.finite(mean)) return("—")
  unit <- if (identical(objective, "cycle")) "h" else "units"
  fmt <- function(x) sprintf("%+.1f", x)
  interval <- if (is.finite(lo) && is.finite(hi)) {
    sprintf("(%s to %s)", fmt(lo), fmt(hi))
  } else {
    ""
  }
  if (isTRUE(html)) {
    bits <- c(sprintf("%s %s", fmt(mean), unit))
    if (nzchar(interval)) bits <- c(bits, interval)
    if (isTRUE(within_noise)) bits <- c(bits, "within noise")
    paste(bits, collapse = "<br>")
  } else if (isTRUE(narrow)) {
    core <- sprintf("%s %s %s", fmt(mean), unit, interval)
    if (isTRUE(within_noise)) paste0(trimws(core), ", within noise") else trimws(core)
  } else {
    core <- sprintf("%s %s %s", fmt(mean), unit, interval)
    if (isTRUE(within_noise)) paste0(trimws(core), ", within noise") else trimws(core)
  }
}

# TRUE when the best-so-far in generation 1 already equals the final best.
sc_opt_best_in_gen1 <- function(history) {
  if (is.null(history) || nrow(history) < 2L) return(FALSE)
  h <- history %>% dplyr::arrange(.data$generation)
  isTRUE(abs(h$best_fitness[[1]] - min(h$best_fitness, na.rm = TRUE)) < 1e-9)
}

# Finish a completed (or cancelled) state into the page bundle.
sc_opt_bundle <- function(state) {
  defs <- state$defs
  base_key <- sc_genome_key(defs$default)
  recheck <- state$recheck
  if (nrow(recheck)) {
    recheck <- recheck %>%
      dplyr::arrange(.data$fitness, .data$cost_term) %>%
      dplyr::mutate(rank = dplyr::row_number())
    # Recommended = best fitness on the recheck; never worse than baseline.
    rec_idx <- which.min(recheck$fitness)
    recheck$is_best <- FALSE
    recheck$is_best[[rec_idx]] <- TRUE
    recheck <- sc_opt_attach_paired(recheck, state$objective, state$capacity_cost,
                                      state$tech_cost)
  }
  baseline <- if (nrow(recheck)) dplyr::filter(recheck, .data$is_baseline) else
    dplyr::filter(state$evaluated, .data$key == base_key)
  best <- if (nrow(recheck)) dplyr::filter(recheck, .data$is_best) else
    state$evaluated %>% dplyr::slice_min(fitness, n = 1, with_ties = FALSE)
  list(
    history = state$history,
    evaluated = state$evaluated,
    recheck = recheck,
    best = best,
    baseline = baseline,
    defs = defs,
    params = state$params,
    meta = list(
      pop_size = state$pop_size,
      n_gen = state$n_gen,
      n_reps = state$n_reps,
      elite_n = state$elite_n,
      elite_reps = state$elite_reps,
      n_recheck = nrow(state$pool %||% tibble::tibble()),
      base_seed = state$base_seed,
      capacity_cost = state$capacity_cost,
      tech_cost = state$tech_cost,
      objective = state$objective,
      p_mut = state$p_mut,
      p_cx = state$p_cx,
      n_elite = state$n_elite,
      cancelled = isTRUE(state$cancelled),
      secs = as.numeric(difftime(Sys.time(), state$t0, units = "secs")),
      source = "live"
    )
  )
}

# Synchronous search + recheck. on_generation / on_recheck are optional hooks
# (row, or i/n). cancel() is checked before each step.
sc_opt_run <- function(params = sc_default_params(), pop_size = 6L, n_gen = 4L,
                       n_reps = 2L, elite_n = 3L, elite_reps = 15L,
                       base_seed = 42L, capacity_cost = 1, objective = "cycle",
                       p_mut = 0.25, p_cx = 0.8, n_elite = 1L,
                       on_generation = NULL, on_recheck = NULL, cancel = NULL,
                       tech_cost = 0.25) {
  st <- sc_opt_new_state(params, pop_size, n_gen, n_reps, elite_n, elite_reps,
                         base_seed, capacity_cost, objective, p_mut, p_cx, n_elite,
                         tech_cost)
  while (identical(st$phase, "search")) {
    if (is.function(cancel) && isTRUE(cancel())) {
      st$cancelled <- TRUE
      break
    }
    if (is.function(on_generation)) on_generation(st$gen_i, st$n_gen)
    st <- sc_opt_search_step(st)
  }
  while (!isTRUE(st$cancelled) && identical(st$phase, "recheck")) {
    if (is.function(cancel) && isTRUE(cancel())) {
      st$cancelled <- TRUE
      break
    }
    if (is.function(on_recheck)) {
      on_recheck(st$i_recheck, nrow(st$pool), st$pool$key[[st$i_recheck]])
    }
    st <- sc_opt_recheck_step(st)
  }
  sc_opt_bundle(st)
}

sc_opt_story <- function(bundle) {
  if (isTRUE(bundle$meta$cancelled) && (is.null(bundle$recheck) || !nrow(bundle$recheck))) {
    n <- if (is.null(bundle$history)) 0L else nrow(bundle$history)
    return(sprintf(
      "The search was cancelled after %d of %d generations, so there is no rechecked recommendation yet. Run it again to finish, or restore the preloaded result.",
      n, bundle$meta$n_gen
    ))
  }
  if (is.null(bundle$recheck) || !nrow(bundle$recheck) ||
      is.null(bundle$best) || !nrow(bundle$best) ||
      is.null(bundle$baseline) || !nrow(bundle$baseline)) {
    return("Run an optimize search to fill this summary.")
  }
  # Use the attached paired columns when present (re-attach if an older bundle
  # is missing them but still has per-rep rows).
  rec <- bundle$recheck
  if (!"delta_mean" %in% names(rec) || !"score_mean" %in% names(rec)) {
    rec <- sc_opt_attach_paired(rec, bundle$meta$objective, bundle$meta$capacity_cost,
                                bundle$meta$tech_cost %||% sc_opt_default_tech_cost())
    bundle$recheck <- rec
    bundle$best <- rec %>% dplyr::filter(.data$is_best)
    bundle$baseline <- rec %>% dplyr::filter(.data$is_baseline)
  }
  best <- bundle$best
  base <- bundle$baseline
  meta <- bundle$meta
  cost <- meta$capacity_cost
  tech_cost <- meta$tech_cost %||% sc_opt_default_tech_cost()
  obj <- meta$objective
  n_re <- meta$elite_reps
  cycle <- identical(obj, "cycle")
  fmt_u <- function(x) sprintf("%.1f units", x)
  fmt_h <- function(x) sprintf("%.1f h", x)
  fmt_score <- function(x) if (cycle) fmt_h(x) else fmt_u(x)
  fmt_cost <- function(x) if (isTRUE(all.equal(x, round(x), tolerance = 1e-9))) sprintf("%.0f", x) else sprintf("%.2f", x)
  score_name <- if (cycle) "net cycle time" else "net throughput"
  direction <- if (cycle) "lower is better" else "higher is better"
  cost_unit <- if (cycle) "hours of cycle time" else "shipped units"
  paired_txt <- if (is.finite(best$delta_mean[[1]])) {
    sc_opt_format_delta(best$delta_mean[[1]], best$delta_lo[[1]], best$delta_hi[[1]],
                        obj, within_noise = FALSE, is_baseline = FALSE)
  } else {
    "not available"
  }
  start_bit <- paste(
    "The first generation is a random mix of station counts and technicians that does not include the current line,",
    "so the chart can show the search improving; the current line is the dashed reference and is always re-checked at the end. "
  )
  extra_n <- (best$extra_capacity[[1]] %||% 0) + (best$extra_techs[[1]] %||% 0)
  extra_bit <- if (isTRUE(extra_n > 0)) {
    if (cycle) {
      sprintf(" (mean cycle time %s plus %s h per extra station and %s h per extra technician)",
              fmt_h(best$cycle_mean_h), fmt_cost(cost), fmt_cost(tech_cost))
    } else {
      sprintf(" (throughput %s minus %s units per extra station and %s per extra technician)",
              fmt_u(best$shipped), fmt_cost(cost), fmt_cost(tech_cost))
    }
  } else {
    ""
  }
  recap <- paste0(
    sprintf("After a %d-replication recheck with the same seeds, its %s is %s versus %s on the current line%s. ",
            n_re, score_name, fmt_score(best$score_mean), fmt_score(base$score_mean), extra_bit),
    sprintf("The paired change is %s. ", paired_txt),
    sprintf("Mean cycle time is %s versus %s, and throughput is %s versus %s. ",
            fmt_h(best$cycle_mean_h), fmt_h(base$cycle_mean_h),
            fmt_u(best$shipped), fmt_u(base$shipped))
  )
  if (isTRUE(best$is_baseline) || identical(best$key, base$key)) {
    return(paste0(
      "No change clearly beats the current line at this cost ",
      sprintf("(%s %s per extra station or board, %s per extra technician). ",
              fmt_cost(cost), cost_unit, fmt_cost(tech_cost)),
      sprintf("After a %d-replication recheck with the same seeds, the current line's %s is %s (%s). ",
              n_re, score_name, fmt_score(base$score_mean), direction),
      sprintf("Throughput is %s (10th to 90th percentile %s–%s) and mean cycle time is %s. ",
              fmt_u(base$shipped), fmt_u(base$shipped_p10), fmt_u(base$shipped_p90),
              fmt_h(base$cycle_mean_h)),
      "Numbers are illustrative."
    ))
  }
  change <- best$change[[1]]
  if (isTRUE(best$clearly_better)) {
    return(paste0(
      "The search found a change that clearly beats the current line at this cost: ",
      change, ". ", recap, start_bit, "Numbers are illustrative."
    ))
  }
  paste0(
    "No change clearly beats the current line at this cost; the closest is ",
    change, ". ", recap,
    if (isTRUE(best$within_noise)) {
      "That interval includes zero, so the gain sits within the usual run-to-run noise. "
    } else "",
    start_bit,
    "Numbers are illustrative."
  )
}

sc_opt_preload_path <- function() {
  cand <- c(
    file.path("app", "data", "preload_optimize.rds"),
    file.path("data", "preload_optimize.rds"),
    file.path("..", "data", "preload_optimize.rds")
  )
  hit <- cand[file.exists(cand)]
  if (length(hit)) normalizePath(hit[[1]]) else cand[[1]]
}

sc_load_preload_optimize <- function(path = sc_opt_preload_path()) {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}
