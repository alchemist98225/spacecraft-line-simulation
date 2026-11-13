# spacecraft_model.R
# Spacecraft line simulation (simmer). Six stations:
#   1 Kitting -> 2 Integration -> 3 Functional test -> 4 Environmental test
#   (vibe + TVAC in ONE shared chamber) -> 5 MRB (only after a failure)
#   -> 6 Final acceptance and ship.
# Every loop goes back to Integration. Parameters come from sc_default_params().
#
# simmer ideas used here (read these before the code):
# * attributes  Per-unit named numbers (set_attribute / get_attribute). A unit
#               carries its variant, committed due_date, fail_count, labor_hpu and a few flags.
# * globals     Shared named numbers on the environment (set_global/get_global).
#               A kit that lands before its unit is waiting leaves a flag here.
# * trap/wait   trap(signal) subscribes the unit to a named signal; wait() parks
#               it until that signal is sent. send(signal) wakes every unit that
#               trapped it. Signals are keyed per unit ("kit_<id>"), so a kit
#               can only release its own unit, even when kits land out of order.
# * branch      Picks a sub-trajectory by the index an option function returns
#               (0 = skip). continue = TRUE rejoins the main trajectory after.
# * rollback    Jumps the unit back to a tagged activity ("integration") and
#               replays everything from there. The fail_count cap stops loops.
# * priority    set_prioritization(c(priority, preemptible, restart)) before a
#               seize. Higher priority goes first in the queue. On a preemptive
#               resource, a unit whose priority is >= the occupant's
#               `preemptible` bumps it. restart = FALSE means the bumped unit
#               later resumes its remaining time; TRUE means it starts over.

# Arrival plan, drawn up front from the seed so the run is reproducible and
# kit times can be scheduled as their own generator.
sc_arrival_plan <- function(params, seed) {
  set.seed(seed)
  horizon <- params$sim_hours
  gap_mean <- params$arrival$mean_interarrival_h
  n_max <- ceiling(3 * horizon / gap_mean) + 10
  # First unit is released at time 0 so ship slots (lead, lead+takt, ...)
  # line up with a planned release cadence of 0, takt, 2*takt, ...
  gaps <- if (identical(params$arrival$process, "fixed")) rep(gap_mean, n_max) else stats::rexp(n_max, 1 / gap_mean)
  v <- params$variants
  k <- params$kit

  tibble::tibble(arrive_h = c(0, cumsum(gaps)[-length(gaps)])) %>%
    dplyr::filter(arrive_h < horizon) %>%
    dplyr::mutate(
      unit_id = dplyr::row_number(),
      variant = sample(seq_len(nrow(v)), dplyr::n(), replace = TRUE, prob = v$mix),
      # Committed ship date: slot i is lead + (i-1) * planned takt.
      due_h = sc_commit_due(unit_id, params),
      kit_delay_h = (if (k$delay_mean_h > 0) sc_rlnorm(dplyr::n(), k$delay_mean_h, k$delay_cv) else 0) +
        (stats::runif(dplyr::n()) < k$p_late) * stats::rexp(dplyr::n(), 1 / max(k$late_extra_mean_h, 1e-9)),
      kit_h = arrive_h + kit_delay_h
    )
}

# Integration sub-trajectory for one variant: hold the station for the whole
# route, and for each sub-step seize the technicians, work, then release them.
# A longer route is just more rows, chained with purrr::reduce.
sc_integration_branch <- function(route, env, attr, params) {
  purrr::reduce(
    purrr::transpose(route),
    function(trj, s) {
      n_fn <- function() sc_step_techs(params, s$step)
      sc_tech_touch(trj, env, attr, n_fn, function() s$mean_h, function() s$cv)
    },
    .init = simmer::trajectory() %>% simmer::seize("integration")
  ) %>%
    simmer::release("integration")
}

# branch() takes sub-trajectories through `...`; this lets us pass a list.
sc_branch_list <- function(trj, option, continue, trajs) {
  do.call(simmer::branch, c(list(trj, option, continue), trajs))
}

sc_build_env <- function(params, plan) {
  env <- simmer::simmer("spacecraft")
  v <- params$variants
  ft <- purrr::transpose(v$func_test)
  en <- purrr::transpose(v$env)
  m <- params$mrb
  cap <- params$capacity
  unit_row <- function() as.integer(sub("^unit", "", simmer::get_name(env))) + 1L
  attr <- function(key) simmer::get_attribute(env, key)
  kit_key <- function() paste0("kit_", attr("unit_id"))

  # Slack-based priority: hours of float left after the nominal remaining
  # env + acceptance work. Less slack -> larger integer priority.
  chamber_priority <- function(remaining_h) {
    function() {
      slack <- attr("due_date") - simmer::now(env) - remaining_h(attr("variant"))
      prio <- as.integer(min(2e6, max(0, round(1e6 - slack))))
      preemptible <- if (isTRUE(params$chamber$preempt)) prio + 1L else .Machine$integer.max
      c(prio, preemptible, as.integer(isTRUE(params$chamber$restart)))
    }
  }
  acc_mean <- params$acceptance$mean_h
  before_vibe <- function(i) en$vibe_mean_h[[i]] + en$tvac_mean_h[[i]] + acc_mean
  before_tvac <- function(i) en$tvac_mean_h[[i]] + acc_mean

  fail_and_route <- function() {
    # After a failure: under the cap -> rework at Integration; at the cap -> MRB.
    simmer::trajectory() %>%
      simmer::set_attribute("fail_count", 1, mod = "+") %>%
      simmer::branch(
        function() if (attr("fail_count") >= params$max_fails) 1L else 2L,
        continue = c(TRUE, TRUE),
        simmer::trajectory() %>% simmer::set_attribute("to_mrb", 1),
        simmer::trajectory() %>%
          simmer::set_attribute("rework_mark", function() simmer::now(env)) %>%
          simmer::rollback("integration")
      )
  }

  unit <- simmer::trajectory("unit") %>%
    simmer::set_attribute(
      c("unit_id", "variant", "due_date", "fail_count", "to_mrb", "scrapped", "shipped", "mrb_q_time", "mrb_visits", "labor_hpu"),
      function() {
        r <- plan[unit_row(), ]
        c(r$unit_id, r$variant, r$due_h, 0, 0, 0, 0, 0, 0, 0)
      }
    ) %>%
    # 1 Kitting. If the kit flag is already set the unit skips the wait.
    simmer::set_attribute("kit_start", function() simmer::now(env)) %>%
    simmer::branch(
      function() if (isTRUE(simmer::get_global(env, kit_key()) > 0)) 0L else 1L,
      continue = TRUE,
      simmer::trajectory() %>% simmer::trap(kit_key) %>% simmer::wait() %>% simmer::untrap(kit_key)
    ) %>%
    simmer::set_attribute("kit_wait_h", function() simmer::now(env) - attr("kit_start")) %>%
    # 2 Integration (the only rollback target), routed by variant.
    simmer::set_attribute("integration_visits", 1, mod = "+", tag = "integration") %>%
    sc_branch_list(
      function() attr("variant"),
      continue = rep(TRUE, nrow(v)),
      trajs = purrr::map(v$route, sc_integration_branch, env = env, attr = attr, params = params)
    ) %>%
    # 3 Functional test.
    simmer::seize("func_test") %>%
    { sc_tech_touch(., env, attr,
                    function() sc_step_techs(params, "func_test"),
                    function() ft$mean_h[[attr("variant")]],
                    function() ft$cv[[attr("variant")]]) } %>%
    simmer::release("func_test") %>%
    simmer::branch(function() as.integer(stats::runif(1) < params$p_fail_func), continue = TRUE, fail_and_route()) %>%
    # 4 Environmental test, skipped when the unit is already headed to MRB.
    # Priority is recomputed before each chamber seize (vibe, then TVAC).
    simmer::branch(
      function() as.integer(attr("to_mrb") == 0),
      continue = TRUE,
      simmer::trajectory() %>%
        simmer::set_prioritization(chamber_priority(before_vibe)) %>%
        simmer::seize("chamber") %>%
        { sc_tech_touch(., env, attr,
                        function() sc_step_techs(params, "vibe"),
                        function() en$vibe_mean_h[[attr("variant")]],
                        function() en$vibe_cv[[attr("variant")]]) } %>%
        simmer::release("chamber") %>%
        simmer::set_prioritization(chamber_priority(before_tvac)) %>%
        simmer::seize("chamber") %>%
        { sc_tech_touch(., env, attr,
                        function() sc_step_techs(params, "tvac"),
                        function() en$tvac_mean_h[[attr("variant")]],
                        function() en$tvac_cv[[attr("variant")]]) } %>%
        simmer::release("chamber") %>%
        simmer::set_prioritization(c(0, .Machine$integer.max, 0)) %>%
        simmer::branch(
          function() as.integer(stats::runif(1) < params$p_fail_env),
          continue = TRUE,
          simmer::trajectory() %>%
            simmer::set_attribute("fail_count", 1, mod = "+") %>%
            simmer::set_attribute("to_mrb", 1)
        )
    ) %>%
    # 5 MRB. Rework is only offered while fail_count is under the cap; at the
    # cap its probability is shared between use-as-is and scrap.
    simmer::branch(
      function() as.integer(attr("to_mrb") == 1),
      continue = TRUE,
      simmer::trajectory() %>%
        simmer::set_attribute("fails_at_first_mrb", function() {
          if (attr("mrb_visits") == 0) attr("fail_count") else attr("fails_at_first_mrb")
        }) %>%
        simmer::set_attribute("mrb_visits", 1, mod = "+") %>%
        simmer::set_attribute("mrb_q_start", function() simmer::now(env)) %>%
        simmer::seize("review_board") %>%
        simmer::set_attribute("mrb_q_time", function() simmer::now(env) - attr("mrb_q_start"), mod = "+") %>%
        { sc_tech_touch(., env, attr,
                        function() sc_step_techs(params, "review"),
                        function() m$mean_h,
                        function() m$cv) } %>%
        simmer::release("review_board") %>%
        simmer::set_attribute("to_mrb", 0) %>%
        simmer::branch(
          function() {
            p <- c(m$p_use_as_is, m$p_rework, m$p_scrap)
            if (attr("fail_count") >= params$max_fails) p[[2]] <- 0
            if (sum(p) <= 0) p[[1]] <- 1   # all-rework split at the cap: use as is
            sample(1:3, 1, prob = p)
          },
          continue = c(TRUE, TRUE, FALSE),
          simmer::trajectory(),
          simmer::trajectory() %>%
            simmer::set_attribute("rework_mark", function() simmer::now(env)) %>%
            simmer::rollback("integration"),
          simmer::trajectory() %>% simmer::set_attribute("scrapped", 1) %>% simmer::leave(1)
        )
    ) %>%
    # 6 Final acceptance and ship; schedule slip is measured against the
    # committed ship date (due_date from the planned cadence).
    simmer::seize("acceptance") %>%
    { sc_tech_touch(., env, attr,
                    function() sc_step_techs(params, "acceptance"),
                    function() params$acceptance$mean_h,
                    function() params$acceptance$cv) } %>%
    simmer::release("acceptance") %>%
    simmer::set_attribute("lateness_h", function() simmer::now(env) - attr("due_date")) %>%
    simmer::set_attribute("shipped", 1)

  # Long-lead parts: one kit per unit, scheduled at its own (possibly late,
  # possibly out-of-order) time. Kit n in time order belongs to unit kit_for[n].
  kit_for <- plan$unit_id[order(plan$kit_h)]
  kit_row <- function() kit_for[[as.integer(sub("^kit", "", simmer::get_name(env))) + 1L]]
  kits <- simmer::trajectory("kit") %>%
    simmer::set_global(function() paste0("kit_", kit_row()), 1) %>%
    simmer::send(function() paste0("kit_", kit_row()))

  env %>%
    simmer::add_resource("integration", cap$integration) %>%
    simmer::add_resource("func_test", cap$func_test) %>%
    simmer::add_resource("chamber", cap$chamber, preemptive = TRUE) %>%
    simmer::add_resource("review_board", cap$review_board) %>%
    simmer::add_resource("acceptance", cap$acceptance) %>%
    simmer::add_resource("techs", sc_labor_capacity(params), preemptive = TRUE) %>%
    simmer::add_generator("unit", unit, simmer::at(plan$arrive_h), mon = 2) %>%
    simmer::add_generator("kit", kits, simmer::at(sort(plan$kit_h)), mon = 0)
}

# One replication: build, run to the horizon, return raw monitor tables.
sc_run_one <- function(params, seed, plan = NULL) {
  plan <- plan %||% sc_arrival_plan(params, seed)
  set.seed(seed + 1e5)
  env <- sc_build_env(params, plan) %>% simmer::run(until = params$sim_hours)
  list(
    plan = plan,
    arrivals = simmer::get_mon_arrivals(env, ongoing = TRUE),
    arrivals_res = simmer::get_mon_arrivals(env, per_resource = TRUE),
    attributes = simmer::get_mon_attributes(env),
    resources = simmer::get_mon_resources(env),
    horizon = params$sim_hours
  )
}

# Replications with set seeds (base_seed, base_seed + 1, ...) via purrr.
sc_run <- function(params = sc_default_params(), n_reps = 5L, base_seed = 42L) {
  seq_len(n_reps) %>%
    purrr::map(function(r) {
      out <- sc_run_one(params, seed = base_seed + r - 1L)
      out$rep <- r
      out
    })
}
