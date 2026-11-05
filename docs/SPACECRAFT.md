# Spacecraft line model

All times are in hours. All numbers are illustrative. Every number lives in
`sc_default_params()`, and the Shiny sidebar, the Settings page and the GA all
write into that same list.

## Flow

1. **Kitting.** A unit is released, then waits for its long-lead kit. Each unit
   has its own kit, which arrives after a lognormal delay, plus an occasional extra
   exponential slip. Kits are a separate generator, so they can land out of order.
   The unit does `trap("kit_<id>")` and `wait()`. The kit does `send("kit_<id>")`.
   If the kit lands first, it leaves a global flag, and the unit skips the wait.
2. **Integration.** `branch()` on the `variant` attribute picks a variant-specific
   route: one lognormal timeout per route step, holding one Integration station the whole time.
   This is the only rollback target (tag `integration`).
3. **Functional test.** On a fail (`p_fail_func`), `fail_count` goes up by one.
   Below the cap the unit rolls back to Integration. At the cap it goes to MRB.
4. **Environmental test.** Vibe, then TVAC, each seizing the same preemptive
   chamber. Before each seize the unit's priority is set from its due-date
   slack, so less slack means higher priority. With preemption on, a strictly
   higher-priority unit bumps the one in the chamber. On a fail (`p_fail_env`),
   `fail_count` goes up by one and the unit goes to MRB.
5. **MRB.** One review board, with a lognormal hold. The disposition is
   use-as-is (go on to acceptance), rework (back to Integration) or scrap
   (leave). Rework is not offered once `fail_count` has reached the cap. Its
   share is then split between use-as-is and scrap in proportion.
6. **Final acceptance and ship.** Lognormal touch time. `lateness_h = ship time - due_date`.

## Parameters (defaults)

| Path | Default | Meaning |
|------|---------|---------|
| `sim_hours` | 8736 | Horizon (52 weeks) |
| `arrival$process` | `"poisson"` | `"poisson"` (exponential gaps) or `"fixed"` |
| `arrival$mean_interarrival_h` | 72 | Mean hours between releases |
| `kit$delay_mean_h`, `kit$delay_cv` | 48, 0.6 | Lognormal kit delay after release |
| `kit$p_late`, `kit$late_extra_mean_h` | 0.15, 120 | Chance of an extra slip, and its exponential mean |
| `capacity$integration` / `func_test` / `chamber` / `review_board` / `acceptance` | 2 / 1 / 1 / 1 / 1 | Servers per resource |
| `p_fail_func` | 0.15 | Functional test fail chance |
| `p_fail_env` | 0.10 | Environmental test fail chance |
| `max_fails` | 2 | Fails that force MRB |
| `mrb$mean_h`, `mrb$cv` | 60, 0.6 | MRB hold |
| `mrb$p_use_as_is` / `p_rework` / `p_scrap` | 0.5 / 0.35 / 0.15 | Disposition shares (normalised) |
| `acceptance$mean_h`, `acceptance$cv` | 6, 0.3 | Final acceptance |
| `chamber$preempt` | TRUE | Allow bumping a running chamber test |
| `chamber$restart` | FALSE | A bumped test resumes (FALSE) or starts over (TRUE) |

Variant tibble (`variants`, one row per variant):

| Variant | mix | route (step: mean h, CV) | func_test | env (vibe, TVAC) | due (offset ± jitter) |
|---|---|---|---|---|---|
| A | 0.5 | structure 16/0.5, harness 12/0.5 | 8 / 0.4 | 6/0.3, 18/0.4 | 220 ± 40 |
| B | 0.3 | + avionics 10/0.7 (structure 18, harness 14) | 10 / 0.4 | 8/0.3, 22/0.4 | 260 ± 40 |
| C | 0.2 | + propulsion 14/0.8 (structure 20, harness 16, avionics 12) | 12 / 0.5 | 10/0.3, 26/0.4 | 320 ± 50 |

Lognormal draws use the mean and CV: `sdlog = sqrt(log(1 + cv^2))`, `meanlog = log(mean) - sdlog^2 / 2`.

## Assumptions

- **Releases.** Release times, variants, due dates and kit times are drawn up
  front from the replication seed (`sc_arrival_plan`). Releases stop at the horizon.
- **Due date.** Release time plus the variant offset, plus a uniform jitter of ± `jitter_h`.
- **Kit signal.** Signals are keyed per unit. A kit can't release another unit,
  and a kit that lands before its unit is waiting is not lost.
- **Slack priority.** Slack = due date − now − nominal remaining chamber and
  acceptance time. It is computed at each chamber seize (once before vibe, once
  before TVAC). It is not refreshed while the unit waits in the queue.
- **Preemption.** A unit preempts when its priority is strictly higher than the
  occupant's. Bumped units resume their remaining time unless `chamber$restart` is on.
- **Forced MRB.** At the cap, MRB can only use-as-is or scrap, so no unit
  fails more than `max_fails` times.
- **Use-as-is after a functional-test failure** goes straight to acceptance,
  skipping the environmental test.
- **WIP** counts every released unit that hasn't shipped or been scrapped,
  including units waiting for kits.

## Outputs

Per replication (`sc_rep_kpis`): cycle time overall and per variant (shipped
units, release to ship), time-average WIP, chamber utilization, mean MRB queue
time, scrap rate (of units that left the line), and on-time % (of shipped
units). `sc_run()` runs replications with seeds `base_seed, base_seed + 1, …` via purrr.

## Optimizer

`sc_run_ga()` searches integer capacities (`sc_knob_defs()`). Fitness is
`(100 − on-time %) + capacity_cost × extra resources over the default`. It uses
common random numbers across genomes, keeps the best genome each generation, and
always includes the default genome.

## Tests

`tests/spacecraft_checks.R` covers the following:
- With no kit delay, no queueing and only functional-test rework, the simulated
  mean cycle time matches an expected value. A test-only helper computes that
  value at runtime from the same params, as an absorbing Markov chain.
- No unit exceeds the fail cap, and the cap always routes to MRB.
- Arrivals = shipped + scrapped + WIP at the end.
- An urgent unit preempts a relaxed one, and with restart on, the bumped unit ships later.
- The GA runs and never reports a result worse than the default.

## Settings page and the Simulate sidebar

The Settings page has two tabs:
- **Variants** holds the per-variant numbers: mix share, Integration step times,
  functional and environmental test times, and due-date offset and jitter. Edits
  apply straight away.
- **Line settings** holds the line-wide numbers. Both the Simulate sidebar and
  this tab are built from one field table (`sc_line_fields()`), so the two always
  list the same fields.

A change in the sidebar is copied to Line settings at once. Changes made on Line
settings reach the sidebar only when you click **Save to Simulate**. The sticky
Save bar lists any unsaved changes and confirms a save. No parameter appears in
both tabs.
