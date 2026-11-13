# spacecraft_ga.R
# Small integer genetic algorithm (carried over from the automotive app's
# optimize_ga.R) wired straight to the spacecraft objective. A genome is an
# integer vector of station counts plus technicians per shift; lower fitness
# is better. Cycle: fitness = mean cycle time + cost of extras. Throughput:
# fitness = -units shipped + cost of extras. Extra stations cost capacity_cost
# each; extra technicians cost tech_cost each (default a quarter of a station).

# ---- Decision variables (knobs) ----
sc_knob_defs <- function(params = sc_default_params()) {
  caps <- tibble::tribble(
    ~name,          ~lo, ~hi, ~short,      ~label,
    "integration",   1L,  4L, "Integ.",    "Integration stations (units worked at once)",
    "func_test",     1L,  3L, "Func.",     "Functional test stands",
    "chamber",       1L,  2L, "Chamber",   "Environmental chambers",
    "review_board",  1L,  2L, "MRB",       "MRB boards",
    "acceptance",    1L,  2L, "Accept.",   "Acceptance stands"
  ) %>%
    dplyr::mutate(
      default = as.integer(unlist(params$capacity[name])),
      group = "capacity",
      .before = "short"
    )
  lo_tech <- as.integer(sc_labor_max_step(params))
  techs <- tibble::tibble(
    name = "techs",
    lo = lo_tech,
    hi = as.integer(max(lo_tech + 4L, 8L)),
    default = as.integer(params$labor$techs_per_shift),
    short = "Techs",
    label = "Technicians per shift",
    group = "labor"
  )
  dplyr::bind_rows(caps, techs)
}

sc_clip_genome <- function(g, defs) as.integer(pmin(defs$hi, pmax(defs$lo, round(g))))
sc_genome_key <- function(g) paste(g, collapse = "_")
sc_random_genome <- function(defs) purrr::map2_int(defs$lo, defs$hi, ~ sample(.x:.y, 1L))

# ---- GA operators ----
sc_tournament <- function(fitness, k = 3L) {
  idx <- sample.int(length(fitness), size = min(k, length(fitness)))
  idx[[which.min(fitness[idx])]]
}

sc_crossover <- function(a, b) {
  if (length(a) < 2L) return(a)
  cut <- sample.int(length(a) - 1L, 1L)
  c(a[seq_len(cut)], b[seq(cut + 1L, length(b))])
}

# Each gene moves to another allowed value with probability p_mut; if nothing
# changed, one gene is forced to move so every child differs from its parent.
sc_mutate <- function(g, defs, p_mut = 0.25) {
  move <- function(i, x) {
    choices <- setdiff(defs$lo[[i]]:defs$hi[[i]], x)
    if (length(choices)) choices[[sample.int(length(choices), 1L)]] else x
  }
  out <- purrr::imap_int(g, function(x, i) if (stats::runif(1) < p_mut) move(i, x) else x)
  if (identical(out, as.integer(g))) {
    i <- sample.int(length(g), 1L)
    out[[i]] <- move(i, g[[i]])
  }
  sc_clip_genome(out, defs)
}

# ---- Objective ----
# "cycle": fitness = mean cycle time (h) + cost of extras (lower is better).
# "throughput": fitness = -units shipped + cost of extras (lower is better;
#               display score is shipped minus cost, higher is better).
# Extra stations or boards cost capacity_cost each; extra technicians cost
# tech_cost each. Extras are never credited (floored at 0).
sc_ga_objectives <- function() {
  tibble::tibble(
    id = c("cycle", "throughput"),
    label = c("Minimize cycle time", "Maximize throughput"),
    verb = c("cycle time", "throughput")
  )
}

sc_ga_extras <- function(genome, defs) {
  extra <- pmax(0L, as.integer(genome) - as.integer(defs$default))
  is_lab <- defs$group == "labor"
  list(
    extra_capacity = as.integer(sum(extra[!is_lab])),
    extra_techs = as.integer(sum(extra[is_lab]))
  )
}

# Total extra stations (used where a single integer is enough).
sc_ga_extra <- function(genome, defs) {
  extras <- sc_ga_extras(genome, defs)
  extras$extra_capacity + extras$extra_techs
}

sc_ga_cost_term <- function(genome, defs, capacity_cost = 1, tech_cost = 0.25) {
  extra <- pmax(0, as.integer(genome) - as.integer(defs$default))
  w <- ifelse(defs$group == "labor", as.numeric(tech_cost), as.numeric(capacity_cost))
  sum(extra * w)
}

sc_ga_fitness_value <- function(shipped, cycle_mean_h, cost_term,
                                objective = "cycle") {
  cost_term <- as.numeric(cost_term)
  if (identical(objective, "throughput")) {
    -as.numeric(shipped) + cost_term
  } else {
    as.numeric(cycle_mean_h) + cost_term
  }
}

sc_params_from_genome <- function(genome, params = sc_default_params(), defs = sc_knob_defs(params)) {
  genome <- as.integer(genome)
  for (i in seq_len(nrow(defs))) {
    if (identical(defs$group[[i]], "labor")) {
      params$labor$techs_per_shift <- genome[[i]]
    } else {
      params$capacity[[defs$name[[i]]]] <- genome[[i]]
    }
  }
  params
}

sc_evaluate_genome <- function(genome, params = sc_default_params(), defs = sc_knob_defs(params),
                               n_reps = 3L, base_seed = 42L, capacity_cost = 1,
                               objective = "cycle", keep_reps = FALSE,
                               tech_cost = 0.25) {
  p <- sc_params_from_genome(genome, params, defs)
  # Isolate the global RNG so simmer's set.seed does not disturb GA operators.
  rng <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv) else NULL
  summed <- sc_summarise(sc_run(p, n_reps = n_reps, base_seed = base_seed), p)
  if (!is.null(rng)) assign(".Random.seed", rng, envir = .GlobalEnv)
  kpis <- summed$summary
  extras <- sc_ga_extras(genome, defs)
  cost_term <- sc_ga_cost_term(genome, defs, capacity_cost, tech_cost)
  q <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE, type = 7))
  out <- tibble::tibble(
    key = sc_genome_key(genome),
    genome = list(as.integer(genome)),
    fitness = sc_ga_fitness_value(kpis$shipped, kpis$cycle_mean_h, cost_term, objective),
    shipped = kpis$shipped,
    shipped_p10 = q(summed$per_rep$shipped, 0.1),
    shipped_p90 = q(summed$per_rep$shipped, 0.9),
    cycle_mean_h = kpis$cycle_mean_h,
    cycle_p10 = q(summed$per_rep$cycle_mean_h, 0.1),
    cycle_p90 = q(summed$per_rep$cycle_mean_h, 0.9),
    labor_hpu = kpis$labor_hpu,
    labor_util = kpis$labor_util,
    extra_capacity = extras$extra_capacity,
    extra_techs = extras$extra_techs,
    cost_term = cost_term,
    objective = objective,
    n_reps = as.integer(n_reps)
  )
  if (isTRUE(keep_reps)) out$per_rep <- list(summed$per_rep)
  out
}


# Random initial population that never includes the current line, so generation
# 1 starts away from the baseline and the best-so-far line can climb.
sc_opt_random_pop <- function(defs, n, avoid = NULL) {
  n <- as.integer(n)
  avoid <- unique(c(sc_genome_key(defs$default), avoid %||% character()))
  pop <- list()
  keys <- character()
  tries <- 0L
  while (length(pop) < n && tries < max(n * 40L, 80L)) {
    tries <- tries + 1L
    g <- sc_random_genome(defs)
    k <- sc_genome_key(g)
    if (k %in% avoid || k %in% keys) next
    pop <- c(pop, list(as.integer(g)))
    keys <- c(keys, k)
  }
  if (length(pop) < n) {
    while (length(pop) < n) {
      g <- sc_random_genome(defs)
      if (sc_genome_key(g) %in% avoid && length(pop) > 0L) next
      pop <- c(pop, list(as.integer(g)))
    }
  }
  pop
}

# ---- Search loop ----
# Same seeds for every genome (common random numbers), so differences come
# from the knobs, not the draw. The current line is scored for comparison
# but is not seeded into the first generation; the best of each generation
# is carried forward (elitism).
sc_run_ga <- function(params = sc_default_params(), pop_size = 8L, n_gen = 5L, n_reps = 3L,
                      base_seed = 42L, capacity_cost = 2, p_mut = 0.25, p_cx = 0.8,
                      n_elite = 1L, on_generation = NULL,
                      objective = "cycle", cancel = NULL, tech_cost = 0.25) {
  defs <- sc_knob_defs(params)
  set.seed(base_seed)
  evaluated <- sc_evaluate_genome(
    defs$default, params = params, defs = defs, n_reps = n_reps,
    base_seed = base_seed, capacity_cost = capacity_cost, objective = objective,
    tech_cost = tech_cost
  )
  eval_pop <- function(pop) {
    new <- purrr::discard(pop, ~ sc_genome_key(.x) %in% evaluated$key) %>% unique()
    evaluated <<- dplyr::bind_rows(evaluated, purrr::map_dfr(new, sc_evaluate_genome,
      params = params, defs = defs, n_reps = n_reps, base_seed = base_seed,
      capacity_cost = capacity_cost, objective = objective, tech_cost = tech_cost))
    evaluated$fitness[match(purrr::map_chr(pop, sc_genome_key), evaluated$key)]
  }
  pop <- sc_opt_random_pop(defs, pop_size)
  history <- tibble::tibble()
  cancelled <- FALSE
  for (gen in seq_len(n_gen) - 1L) {
    if (is.function(cancel) && isTRUE(cancel())) {
      cancelled <- TRUE
      break
    }
    fitness <- eval_pop(pop)
    best_i <- which.min(fitness)
    row <- tibble::tibble(generation = gen, generation_n = gen + 1L,
                          best_fitness = min(fitness), mean_fitness = mean(fitness))
    history <- dplyr::bind_rows(history, row)
    if (is.function(on_generation)) on_generation(row)
    n_keep <- max(1L, min(as.integer(n_elite), pop_size - 1L))
    elites <- pop[order(fitness)][seq_len(n_keep)]
    pop <- c(elites, purrr::map(seq_len(pop_size - n_keep), function(i) {
      a <- pop[[sc_tournament(fitness)]]
      b <- pop[[sc_tournament(fitness)]]
      child <- if (stats::runif(1) < p_cx) sc_crossover(a, b) else if (stats::runif(1) < 0.5) a else b
      sc_mutate(child, defs, p_mut)
    }))
  }
  baseline <- evaluated %>% dplyr::filter(key == sc_genome_key(defs$default))
  # Reported best is never worse than the current line (which was scored
  # for reference even though it was not in the search population).
  best <- evaluated %>% dplyr::slice_min(fitness, n = 1, with_ties = FALSE)
  list(best = best, baseline = baseline, history = history, evaluated = evaluated,
       defs = defs, cancelled = cancelled, objective = objective)
}
