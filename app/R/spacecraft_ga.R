# spacecraft_ga.R
# Small integer genetic algorithm (carried over from the automotive app's
# optimize_ga.R) wired straight to the spacecraft objective. A genome is an
# integer vector of resource capacities; lower fitness is better.
#   fitness = (100 - on-time %) + capacity_cost * (capacity added over default)

# ---- Decision variables (knobs) ----
sc_knob_defs <- function(params = sc_default_params()) {
  tibble::tribble(
    ~name,          ~lo, ~hi, ~label,
    "integration",   1L,  4L, "Integration stations (units worked at once)",
    "func_test",     1L,  3L, "Functional test stands",
    "chamber",       1L,  2L, "Environmental chambers",
    "review_board",  1L,  2L, "MRB boards",
    "acceptance",    1L,  2L, "Acceptance stands"
  ) %>%
    dplyr::mutate(default = as.integer(unlist(params$capacity[name])), .before = label)
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
sc_params_from_genome <- function(genome, params = sc_default_params(), defs = sc_knob_defs(params)) {
  params$capacity[defs$name] <- as.list(as.integer(genome))
  params
}

sc_evaluate_genome <- function(genome, params = sc_default_params(), defs = sc_knob_defs(params),
                               n_reps = 3L, base_seed = 42L, capacity_cost = 2) {
  p <- sc_params_from_genome(genome, params, defs)
  kpis <- sc_summarise(sc_run(p, n_reps = n_reps, base_seed = base_seed), p)$summary
  extra <- sum(pmax(0L, as.integer(genome) - defs$default))
  tibble::tibble(
    key = sc_genome_key(genome),
    genome = list(as.integer(genome)),
    fitness = (100 - kpis$on_time_pct) + capacity_cost * extra,
    on_time_pct = kpis$on_time_pct,
    cycle_mean_h = kpis$cycle_mean_h,
    extra_capacity = extra
  )
}

# ---- Search loop ----
# Same seeds for every genome (common random numbers), so differences come
# from the knobs, not the draw. The default genome is always in generation 0,
# and the best genome is carried into every next generation (elitism).
sc_run_ga <- function(params = sc_default_params(), pop_size = 8L, n_gen = 5L, n_reps = 3L,
                      base_seed = 42L, capacity_cost = 2, p_mut = 0.25, on_generation = NULL) {
  defs <- sc_knob_defs(params)
  set.seed(base_seed)
  evaluated <- tibble::tibble(key = character(), fitness = numeric())
  eval_pop <- function(pop) {
    new <- purrr::discard(pop, ~ sc_genome_key(.x) %in% evaluated$key) %>% unique()
    evaluated <<- dplyr::bind_rows(evaluated, purrr::map_dfr(new, sc_evaluate_genome,
      params = params, defs = defs, n_reps = n_reps, base_seed = base_seed, capacity_cost = capacity_cost))
    evaluated$fitness[match(purrr::map_chr(pop, sc_genome_key), evaluated$key)]
  }
  pop <- c(list(defs$default), purrr::map(seq_len(pop_size - 1L), ~ sc_random_genome(defs)))
  history <- purrr::map_dfr(seq_len(n_gen) - 1L, function(gen) {
    fitness <- eval_pop(pop)
    best <- pop[[which.min(fitness)]]
    row <- tibble::tibble(generation = gen, best_fitness = min(fitness), mean_fitness = mean(fitness))
    if (is.function(on_generation)) on_generation(row)
    pop <<- c(list(best), purrr::map(seq_len(pop_size - 1L), function(i) {
      sc_mutate(sc_crossover(pop[[sc_tournament(fitness)]], pop[[sc_tournament(fitness)]]), defs, p_mut)
    }))
    row
  })
  baseline <- evaluated %>% dplyr::filter(key == sc_genome_key(defs$default))
  best <- evaluated %>% dplyr::slice_min(fitness, n = 1, with_ties = FALSE)
  list(best = best, baseline = baseline, history = history, evaluated = evaluated, defs = defs)
}
