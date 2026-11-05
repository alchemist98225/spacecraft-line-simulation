# Spacecraft Line Simulator

A discrete-event simulation of a low-volume, high-mix spacecraft line, built with
[simmer](https://r-simmer.org/) and Shiny. It started from the infrastructure of the
automotive line simulator (Shiny shell, sidebar style, plot theme, the small integer
GA, the test harness) and keeps only what this line needs.

Six stations: Kitting, Integration, Functional test, Environmental test (vibe and
TVAC in one shared chamber), MRB and Final acceptance. Three variants (A, B, C)
differ in route, touch times and due dates. All inputs are illustrative.

## Run it

```bash
source /path/to/activate_r.sh      # an R env with shiny, bslib, bsicons, tidyverse, simmer, DT
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
