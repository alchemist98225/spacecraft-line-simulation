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
| `sim_hours` | 8736 | Horizon (52 weeks). In the app it's set by Quarters, Weeks and Days steppers (defaults 4, 0, 0; 1 quarter = 13 weeks), and the total in hours is shown below them. A zero horizon blocks the run. |
| `arrival$process` | `"poisson"` | `"poisson"` (exponential gaps) or `"fixed"` |
| `arrival$mean_interarrival_h` | 72 | Mean hours between releases |
| `kit$delay_mean_h`, `kit$delay_cv` | 48, 0.6 | Lognormal kit delay after release (app labels: Lead time (hours), Coefficient of variation) |
| `kit$p_late`, `kit$late_extra_mean_h` | 0.15, 120 | Chance of an extra slip, and its exponential mean (app labels: Delay probability, Delay time (hours)) |
| `capacity$integration` / `func_test` / `chamber` / `review_board` / `acceptance` | 2 / 1 / 1 / 1 / 1 | Servers per resource |
| `p_fail_func` | 0.15 | Functional test fail chance |
| `p_fail_env` | 0.10 | Environmental test fail chance |
| `max_fails` | 2 | Fails that force MRB |
| `mrb$mean_h`, `mrb$cv` | 60, 0.6 | MRB hold |
| `mrb$p_use_as_is` / `p_scrap` (`p_rework` derived) | 0.5 / 0.15 (0.35) | "Accepted" and "Scrap" in the app; Rework = 1 - Accepted - Scrap, shown read-only. Accepted + Scrap over 1 blocks Run and Optimize with a message |
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
A Warm-up to discard setting (weeks, default 0) can drop units released in the
first weeks from the unit KPIs and start the time-average WIP and utilization
after that point. Leave it at 0 to keep the current numbers; the Warmup page
explains when raising it would matter.

## Optimizer

The Optimize page searches integer station counts (`sc_knob_defs()`:
integration stations, environmental chambers, functional test stands, MRB
boards and acceptance stands) around the current Settings. You can maximize
on-time % or minimize cycle time. The chart, table, and summary use natural
units and state the direction: a net on-time score
(`on-time % − cost × extra stations`; higher is better) or a net cycle time
(`mean cycle time + cost × extra stations`; lower is better). The GA still
minimizes an internal fitness. Extra stations are counted only above the
current line (reductions do not earn a credit), so the search only adds staff
when the gain is worth the cost.

The search uses common random numbers (the same seeds at every candidate),
keeps the best mix each generation, and always includes the current line, so
the reported best is never worse than the baseline on the internal fitness.
After the search, the current line, the top N distinct candidates, and each
one-station change from the current line are re-checked (duplicates dropped)
with more replications and the same seeds. The table reports the net score
and a Change vs current column built from the paired per-replication
differences (mean and a 90% interval). A candidate is marked "within noise"
when that interval includes zero, and the summary only claims a clear gain
when the recommendation's interval excludes zero in its favor. Status names
every generation (1 of N through N of N) and every recheck candidate, and
the help text states exactly how many setups are re-checked. If the best
setup already appeared in generation 1, the chart caption says so.

A preloaded Quick search (seed 42, the same default line as Compare) is built
by `scripts/preload_optimize.R` and opens at once. You can also run a live
search with a progress bar and Cancel. Quick mode is sized to stay reasonable
on a small server. Numbers are illustrative.

## Warm-up

The line starts empty, so the first units see short queues and the first weeks
can bias cycle time and on-time %. The Warmup page charts a moving-window
(Welch) smooth of cycle time by release order and of WIP by week, with a
P10–P90 ribbon and a dashed line at a suggested warm-up — the first time
smoothed WIP has reached a typical later-year level and holds there. A table
and a plain-English summary show how the KPIs change if units released in
those first weeks are discarded. The same seeds are used across replications.

The Warm-up to discard setting on Simulate and Settings is 0 weeks by default,
so Simulate, Compare and the other pages keep their current numbers unless you
opt in and raise it. A preloaded check (seed 42, 20 replications, the same
default line as Compare) is built by `scripts/preload_warmup.R` and opens at
once. You can also run a live check with a progress bar and Cancel. Numbers
are illustrative.

## Tests

`tests/spacecraft_checks.R` covers the following:
- With no kit delay, no queueing and only functional-test rework, the simulated
  mean cycle time matches an expected value. A test-only helper computes that
  value at runtime from the same params, as an absorbing Markov chain.
- No unit exceeds the fail cap, and the cap always routes to MRB.
- Arrivals = shipped + scrapped + WIP at the end.
- An urgent unit preempts a relaxed one, and with restart on, the bumped unit ships later.
- The GA runs and never reports a result worse than the default. The Optimize
  page pipeline is reproducible under the same seeds, the preload loads, a
  mid-search cancel stops before later generations, the rechecked best is
  never worse than the current line, and the paired change equals the
  difference of per-replication scores under common random numbers.
- The Tornado preload loads, a short live tornado is reproducible under the
  same seeds, and the signs of the swings are sensible (longer times and
  higher fail rates raise cycle time; extra stations lower it).
- The Breakpoint preload loads, a short live demand sweep is reproducible
  under the same seeds, on-time falls and cycle time rises as demand rises,
  the interpolated breakpoint lies between the last passing and first failing
  demand, and a forced heavy load is detected as a breakpoint with a saturated
  station.
- The Warmup preload loads, a short live check is reproducible under the same
  seeds, warmup = 0 reproduces the current KPIs exactly, a forced empty-start
  case suggests a positive warm-up, and discarding those early units changes
  cycle time in the expected direction.

## Settings page and the Simulate sidebar

The Settings page has two tabs:
- **Variants** holds the per-variant numbers: share of releases, Integration step
  times, functional and environmental test times, and the due date allowance and
  spread. Edits apply straight away. Every time is in hours. A step name on its
  own is the mean time, and "<step> variation" is its coefficient of variation
  (standard deviation divided by the mean).
- **Line settings** holds the line-wide numbers. Both the Simulate sidebar and
  this tab are built from one field table (`sc_line_fields()`), so the two always
  list the same fields.

A change in the sidebar is copied to Line settings at once. Changes made on Line
settings reach the sidebar only when you click **Save to Simulate**. The sticky
Save bar lists any unsaved changes and confirms a save. No parameter appears in
both tabs.

## Compare

The Compare page runs the baseline line and up to three what-if scenarios with
the same seeds (common random numbers). It reports throughput (units shipped),
mean cycle time, on-time %, mean lateness and average WIP as the mean across
replications plus the 10th to 90th percentile, and a delta versus the baseline. For mean lateness, negative means early, so a
more negative value is an improvement. A change smaller than a quarter of the
baseline P10–P90 half-width is shown in grey, because it sits inside the usual
run-to-run spread.
A preloaded set (20 replications, seed 42) is built by `scripts/preload_scenarios.R`
and opens at once: a second environmental test chamber, a lower environmental
failure rate (10% to 4%), and an extra integration station. You can also add a
scenario from the current Settings and run a live comparison (with a progress
bar and a Cancel button). Numbers are illustrative.

## Analyze

The Analyze page explains why the line performs as it does for the current
Settings (or the last Simulate run, if you have one). It opens instantly on a
preloaded baseline — the same default line, 20 replications and seed 42 as the
Compare baseline — built by `scripts/preload_scenarios.R`. You can also analyze
the current Settings live, with a progress bar and a Cancel button.

For shipped units, cycle time is split into working, waiting in queue, failed /
rework (the extra queue and work after a rollback to Integration), MRB hold
(waiting for the board plus the review), and kitting delay. Those five parts
add up to cycle time. A stacked bar shows each part as a share of cycle time,
and a table reports the hours. A bottleneck view shows utilization at each
station, highlights the busiest one, and writes the time-average queue in
front of it. A ribbon plot follows the two or three busiest queues over the
horizon (mean and 10th to 90th percentile across replications). A short
plain-English "what this says" summary is generated from those numbers.
Numbers are illustrative.

## Tornado

The Tornado page is a one-at-a-time sensitivity study around the current
Settings (or the default line). Each key input — Integration times, functional
and environmental test times, functional and environmental failure rates, MRB
hold time, kitting lead time and delay probability, and station counts — is
moved to a low and a high setting while everything else stays at the baseline.
Times and rates move by 20% (probabilities are clamped to 0–1). Station counts
move by one and never go below one.

The same seeds are used throughout (common random numbers). Arrival plans are
reused when the kit, mix and release settings are unchanged, so a swing in
cycle time or on-time % comes from the input, not from a luckier arrival stream.

The chart shows a chosen output (mean cycle time, on-time %, throughput, or
mean lateness) as a bar from the low result to the high result, sorted by the
size of the swing, with a dashed line at the baseline. A short plain-English
summary names the top drivers. A preloaded tornado (20 replications, seed 42, the same default line as Compare and Analyze)
is built by `scripts/preload_tornado.R` and opens at once. You can also run a
live tornado from the current Settings, with a progress bar and a Cancel
button. Numbers are illustrative.

## Breakpoint

The Breakpoint page raises the release rate (offered demand, in units per
52-week year) from the current Settings and finds where mean on-time % crosses
a target (80% by default; you can change it), interpolating between the last
demand that still meets the target and the first that falls short. The same
seeds are used at every demand point (common random numbers). Arrival plans
are redrawn when the release rate changes, because the plan depends on the
time between releases.

The charts show on-time % and mean cycle time against offered demand, with a
ribbon from the 10th to the 90th percentile across replications, a dashed
horizontal line at the on-time target, and a dashed vertical line where mean
on-time crosses the target. The highlighted table row is the first failing
demand. A third chart follows utilization of the busiest stations, with a
dashed line at 100% busy, so you can see which station saturates first. A
short plain-English summary is generated from those numbers. Changing the
target redraws the marks and the summary without rerunning the sweep.

A preloaded sweep (20 replications, seed 42, the same default line as Compare,
Analyze and Tornado) is built by `scripts/preload_breakpoint.R` and opens at
once. The baseline point on that sweep is the default release rate, 121
units/year, with the same 179.2 h mean cycle time. You can also run a live
sweep from the current Settings, with a progress bar and a Cancel button.
Numbers are illustrative.
