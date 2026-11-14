# Spacecraft Line Simulator

A discrete-event simulation of a low-volume, high-mix spacecraft line, built with
[simmer](https://r-simmer.org/) and Shiny. It started from the infrastructure of the
automotive line simulator (Shiny shell, sidebar style, plot theme, the small integer
GA, the test harness) and keeps only what this line needs.

Stations: Subassembly build-up, Integration, Functional test, Environmental test
(vibe and TVAC in one shared chamber), MRB and Final acceptance, plus inspection
and paperwork holds after key steps. Three variants (A, B, C) differ in route and
touch times. A shared technician pool is scheduled by shift and by work week
(Monday–Friday by default; the horizon stays in calendar hours). All inputs are illustrative.

## Run it

```bash
source /path/to/activate_r.sh      # an R env with shiny, bslib, bsicons, tidyverse, simmer, DT, gt, plotly, crosstalk
Rscript -e 'shiny::runApp("app")'
```

## Test it

```bash
Rscript tests/spacecraft_checks.R   # model: cycle time, loop cap, conservation, preemption, GA
Rscript tests/app_checks.R          # Shiny: parse, input spec, one server run
```

## Read next

- `docs/SPACECRAFT.md`: the model, its parameters and assumptions
- `docs/CODE_MAP.md`: which file does what

The app pages are Simulate, Settings, Results (a dropdown menu), Compare, Analyze, Tornado, Breakpoint, Optimize, Warmup and Ramp. Equipment can break down (operating-time MTBF / MTTR, off with a Settings toggle). Simulate headline value boxes are the core line metrics: throughput as a rate (units/year, with units/week alongside), cycle time, touch labor (HPU) and average WIP, with the same star-map as Settings underneath. Results is a drill-down of the last Simulate run: Bottleneck (where the time goes, then station utilization and downtime with a 10–90 spread and the bottleneck called out), Cycle time (all-units histogram with a density per replication), Quality (scrap and rework by origin), Reliability (observed versus configured MTBF and MTTR), and Summary (WIP, labor, and CSV downloads). Compare scores precomputed capacity moves against the baseline, first by throughput gain and then by HPU, WIP and cycle time; click two scenarios in the sidebar for the detail charts (plotly with crosstalk). Ramp is a separate planning tool: it plans capacity against a rising target rate, with a learning curve, hire and procurement lead times, and a simple meets-target-rate check.

## How this was built

This project was built with AI assistance. I (Eddie Benjamin) specified the process, the model logic, the parameters and the validation approach, reviewed the results, and directed every change; an AI coding assistant wrote most of the implementation. The acceptance tests check the simulation against analytical expectations, so the results can be verified without taking the code on faith.
