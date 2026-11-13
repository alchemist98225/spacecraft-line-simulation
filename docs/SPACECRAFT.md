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
6. **Final acceptance and ship.** Lognormal touch time. A planned ship date is
   still computed internally (used for chamber sequencing); it is not a
   headline output.

## Parameters (defaults)

| Path | Default | Meaning |
|------|---------|---------|
| `sim_hours` | 8736 | Horizon (52 weeks). In the app it's set by Quarters, Weeks and Days steppers (defaults 4, 0, 0; 1 quarter = 13 weeks), and the total in hours is shown below them. A zero horizon blocks the run. |
| `arrival$process` | `"fixed"` | `"fixed"` (evenly scheduled releases) or `"poisson"` (exponential gaps) |
| `arrival$mean_interarrival_h` | 72 | Mean hours between releases (about 122 units/year) |
| `commit$lead_h` | 165 | Planned ship of the first unit, in hours after release; later units are spaced one takt apart. Used to sequence work at the chamber, not as a headline due date. |
| `labor$pattern` | `"three_shift"` | Shift pattern for the shared technician pool (`day`, `two_shift`, or `three_shift`) |
| `labor$techs_per_shift` | 4 | Technicians scheduled on each shift |
| `labor$step_techs` | integration steps 2, test/review/acceptance 1 | Technicians seized at each process step (editable in Settings) |
| `kit$delay_mean_h`, `kit$delay_cv` | 24, 0.25 | Lognormal kit delay after release (app labels: Lead time (hours), Coefficient of variation) |
| `kit$p_late`, `kit$late_extra_mean_h` | 0.02, 36 | Chance of an extra slip, and its exponential mean (app labels: Delay probability, Delay time (hours)) |
| `capacity$integration` / `func_test` / `chamber` / `review_board` / `acceptance` | 2 / 1 / 1 / 1 / 1 | Servers per resource |
| `p_fail_func` | 0.05 | Functional test fail chance |
| `p_fail_env` | 0.03 | Environmental test fail chance |
| `max_fails` | 2 | Fails that force MRB |
| `mrb$mean_h`, `mrb$cv` | 60, 0.6 | MRB hold |
| `mrb$p_use_as_is` / `p_scrap` (`p_rework` derived) | 0.5 / 0.15 (0.35) | "Accepted" and "Scrap" in the app; Rework = 1 - Accepted - Scrap, shown read-only. Accepted + Scrap over 1 blocks Run and Optimize with a message |
| `acceptance$mean_h`, `acceptance$cv` | 6, 0.3 | Final acceptance |
| `chamber$preempt` | TRUE | Allow bumping a running chamber test |
| `chamber$restart` | FALSE | A bumped test resumes (FALSE) or starts over (TRUE) |

Variant tibble (`variants`, one row per variant):

| Variant | mix | route (step: mean h, CV) | func_test | env (vibe, TVAC) |
|---|---|---|---|---|
| A | 0.5 | structure 16/0.5, harness 12/0.5 | 8 / 0.4 | 6/0.3, 18/0.4 |
| B | 0.3 | + avionics 10/0.7 (structure 18, harness 14) | 10 / 0.4 | 8/0.3, 22/0.4 |
| C | 0.2 | + propulsion 14/0.8 (structure 20, harness 16, avionics 12) | 12 / 0.5 | 10/0.3, 26/0.4 |

Lognormal draws use the mean and CV: `sdlog = sqrt(log(1 + cv^2))`, `meanlog = log(mean) - sdlog^2 / 2`.

## Assumptions

- **Releases.** Release times, variants, planned ship slots and kit times are drawn up
  front from the replication seed (`sc_arrival_plan`). Releases stop at the horizon.
  Planned ship slots follow the release takt and are used to sequence work at the chamber.
- **Technicians.** A shared pool is scheduled by shift (`simmer::schedule`, except
  a constant pool on three shifts). Each step seizes its station first, then its
  technicians, then draws a touch time. Work pauses when a shift ends and resumes
  when the next shift starts. Touch labor hours per unit (HPU) are technicians
  times that drawn touch time, summed over every step including rework; waiting
  does not count. Labor utilization is busy technician-hours divided by scheduled
  technician-hours (off-shift capacity of 0 is excluded). Adding a technician is
  assumed cheaper than adding a station (default Optimize costs 0.25 versus 1.0).
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

Per replication (`sc_rep_kpis`): throughput (units shipped), cycle time overall
and per variant (release to ship), touch labor hours per unit (HPU) overall and
per variant, labor utilization, time-average WIP, chamber utilization, mean
technician wait, mean MRB queue time, and scrap rate (of units that left the
line). Headlines on the Simulate page, the Summary table and Compare are
throughput, cycle time, HPU, labor utilization and WIP. `sc_run()` runs
replications with seeds `base_seed, base_seed + 1, …` via purrr.
A Warm-up to discard setting (weeks, default 0) can drop units released in the
first weeks from the unit KPIs and start the time-average WIP and utilization
after that point. Leave it at 0 to keep the current numbers; the Warmup page
explains when raising it would matter.

## Optimizer

The Optimize page searches integer station counts and technicians per shift
(`sc_knob_defs()`: integration stations, environmental chambers, functional
test stands, MRB boards, acceptance stands, and technicians per shift) around
the current Settings. You can minimize cycle time or maximize throughput at
the current release rate. The chart, table, and summary use natural units and
state the direction: a net cycle time (mean cycle time plus the cost of extra
stations and technicians; lower is better) or a net throughput (units shipped
minus that cost; higher is better). The GA still minimizes an internal
fitness. Extras are counted only above the current line (reductions do not
earn a credit). Default costs are 1.0 per extra station and 0.25 per extra
technician, because a person is cheaper than a new bay or chamber.

The search uses common random numbers (the same seeds at every candidate),
keeps the best mix each generation, and always includes the current line, so
the reported best is never worse than the baseline on the internal fitness.
After the search, the current line, the top N distinct candidates, and each
one-step change from the current line are re-checked (duplicates dropped)
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
can bias cycle time and throughput. The Warmup page charts a moving-window
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


## Capacity ramp

The Ramp page plans capacity against a rising target rate. The default curve
is 12 to 48 units a year over 24 months, in an editable table of quarters.
Each quarter is a one-year snapshot at that quarter's rate, using common
random numbers (seeds `base_seed, base_seed + 1, …`). Touch times follow a
Wright/Crawford learning curve (85% per doubling by default): the Settings
process times are treated as the times at a stated cumulative volume (80
units by default), so early units are slower and HPU falls as volume grows.
Kit delay and fail chances are not scaled.

The one headline check is whether mean annualized throughput meets the target
rate (at least 95%). Cycle time, touch labor (HPU), labor utilization,
station utilization and WIP are shown alongside. The page recommends
technician, shift, or station moves with an order-by date equal to the need
date minus the hire-and-train or procurement lead time (defaults: 3 months
for technicians and a new shift, 6 months for a station or fixture, 9 months
for a chamber). A table shows the dip if each move is delayed by its lead
time. Quarters that fall before the shortest lead time cannot be saved by a
new order placed at the start of the ramp; the plan still places those
orders for later quarters.

A preloaded plan (seed 42, 10 replications) uses a lean early line — day
shift, two technicians, one of each station — so the 12-to-48 ramp actually
needs capacity moves. A live plan can also start from the current Settings.
The default three-shift line already holds this ramp. Numbers are
illustrative.

## Tests

`tests/spacecraft_checks.R` covers the following:
- With no kit delay, no queueing and only functional-test rework, the simulated
  mean cycle time matches an expected value. A test-only helper computes that
  value at runtime from the same params, as an absorbing Markov chain.
- No unit exceeds the fail cap, and the cap always routes to MRB.
- Arrivals = shipped + scrapped + WIP at the end.
- Labor: HPU is finite and higher for the heavier variant; labor utilization stays in [0, 1]; an unconstrained technician pool does not leave a material tech wait.
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
  under the same seeds, cycle time rises as demand rises, the interpolated
  breakpoint (cycle-time cap or utilization cap) lies above the baseline, and
  a forced heavy load is detected as a breakpoint with a saturated station.
- The Warmup preload loads, a short live check is reproducible under the same
  seeds, warmup = 0 reproduces the current KPIs exactly, a forced empty-start
  case suggests a positive warm-up, and discarding those early units changes
  cycle time in the expected direction.
- The Ramp preload loads, a short live plan on a lean early line is
  reproducible under the same seeds, the 85% learning curve scales touch
  times (not kit delay), order-by equals need minus lead time, the line
  recommends capacity moves and a delayed shift can miss the target rate,
  and the default three-shift line holds 12 units/year without a move.


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
mean cycle time, touch labor (HPU), labor utilization and average WIP as the mean
across replications, and a delta versus the baseline. The 10th to 90th percentile
is on the spread charts (one tab per metric). Labor utilization is reported for
context and is not coloured as better or worse. A change smaller than a quarter of
the baseline P10–P90 half-width is shown in grey, because it sits inside the usual
run-to-run spread.
A preloaded set (20 replications, seed 42) is built by `scripts/preload_scenarios.R`
and opens at once: a rework surge, losing a shift, and a higher release rate
(40 hours between releases). You can also add a
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
hold time, kitting lead time and delay probability, station counts, and the
technician pool — is moved to a low and a high setting while everything else
stays at the baseline. Times and rates move by 20% (probabilities are clamped
to 0–1). Station counts and technicians per shift move by one and never go
below one (or below the largest per-step tech count).

The same seeds are used throughout (common random numbers). Arrival plans are
reused when the kit, mix and release settings are unchanged, so a swing in
cycle time or throughput comes from the input, not from a luckier arrival stream.

The chart shows a chosen output (mean cycle time, throughput, touch labor (HPU),
or labor utilization) as a bar from the low result to the high result, sorted by the
size of the swing, with a dashed line at the baseline. A short plain-English
summary names the top drivers. A preloaded tornado (20 replications, seed 42, the same default line as Compare and Analyze)
is built by `scripts/preload_tornado.R` and opens at once. You can also run a
live tornado from the current Settings, with a progress bar and a Cancel
button. Numbers are illustrative.

## Breakpoint

The Breakpoint page raises the release rate (offered demand, in units per
52-week year) from the current Settings and finds the highest demand the line
can hold before mean cycle time crosses a multiple of the baseline (1.5× by
default) or a station or the technician pool hits a utilization cap (90% by
default), interpolating between the last demand that still holds and the first
that does not. The same seeds are used at every demand point (common random
numbers). Arrival plans are redrawn when the release rate changes, because the
plan depends on the time between releases.

The charts show throughput, mean cycle time and utilization against offered
demand, with a ribbon from the 10th to the 90th percentile across replications,
a dashed diagonal where shipped units would match offered demand, a dashed
horizontal line at the cycle-time or utilization cap, and a dashed vertical
line at the first binding limit. The highlighted table row is the first grid
demand past that limit. A short plain-English summary is generated from those
numbers. Changing the caps redraws the marks and the summary without rerunning
the sweep.

A preloaded sweep (20 replications, seed 42, the same default line as Compare,
Analyze and Tornado) is built by `scripts/preload_breakpoint.R` and opens at
once. The baseline point on that sweep is the default release rate, about 121
units/year. You can also run a live sweep from the current Settings, with a
progress bar and a Cancel button. Numbers are illustrative.
