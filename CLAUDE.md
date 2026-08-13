# CLAUDE.md

Guidance for Claude Code working in the `citius` R package.

## Package Overview

`citius` estimates latent athlete ability from competition result histories and
simulates event outcomes for major multi-sport games. Athletics and swimming are
the two sports built out; the abstractions are deliberately sport-agnostic so
further sports slot in by extending the event registry only.

Named for *Citius, Altius, Fortius* — the package predicts marks and placings.

## The two abstractions everything rests on

### 1. The oriented performance scale (`R/marks.R`)

Athletics mixes events where lower is better (times) with events where higher is
better (throws, jumps, points). Every model is written against
`perf = orientation * log(mark)`, so **higher is always better** and no
downstream function branches on event type.

Logs, not raw units: performance spread is multiplicative, so one log-scale SD
means the same percentage spread for a 9.58s 100m and a 2:01 marathon. It also
enforces positivity.

**Never compare raw marks across events. Always go through `to_perf()`.**

### 2. The event registry (`R/events.R`)

`citius_events()` is the source of truth. Four columns carry modelling weight:

- `orientation` — feeds `to_perf()`
- `tactical` — championship finals decouple time from placing (800m up)
- `technical` — discrete foul/no-height failure mode, simulated separately
- `cv_prior` — within-athlete CV prior; swimming is much lower than athletics

Source-specific event names are resolved by `match_event()`, which returns `NA`
rather than guessing. **Do not add fuzzy fallback matching** — silently snapping
an unknown event onto a neighbour corrupts athlete histories undetectably.

## Everything is measured, nothing is assumed

**Hard rule: no hand-tuned constants in the models.** Every quantity that
affects an answer is estimated from data by `calibrate()`. If you find yourself
typing a number into a model, stop and estimate it.

`cv_prior` in the registry is a fallback placeholder, not an estimate — check
`calibration$events$calibrated` to see which events are on real numbers.

Every measured value, and the reasoning behind it, is in
[`../docs/reference/calibration-measured-values.md`](../docs/reference/calibration-measured-values.md).

## Read before you change the models

The empirical findings are in `../docs/reference/`. They are not background
reading — each one is a conclusion that cost a wrong answer to reach.

| Reading this before… | File |
|---|---|
| touching `calibrate()`, the aging curve, the decay or tail fits | [calibration-measured-values.md](../docs/reference/calibration-measured-values.md) |
| changing anything in the estimation or simulation path | [silent-bugs.md](../docs/reference/silent-bugs.md) |
| extending the model, or reasoning about conditions, tiers or rounds | [modelling-traps.md](../docs/reference/modelling-traps.md) |
| adding a source, or judging whether a competition is in the corpus | [harvesting.md](../docs/reference/harvesting.md) |
| quoting a skill number or comparing two calibrations | [backtests.md](../docs/reference/backtests.md) |

Full incident write-ups live in `../docs/incidents/`.

The single most important pattern across all of them: **the bugs in this package
change what the numbers are without changing whether the code runs.** Nothing
errors, tests pass, output looks clean. Sanity-check predictions against
athletes you recognise, and never add an outlier filter without first checking
the outliers are real.

## Performance

The rule in both hot paths is the same: **never loop at R level over the big
dimension** — simulations in the simulator, groups in the estimator. Both were
once written that way and both were several times slower for it.

`.rank_desc()` melts the simulation matrix and ranks within race in one
`frankv()` call. **Do not replace it with `apply(..., rank)`**, which loops over
`n_sims` and is two orders of magnitude slower. It previously used an all-pairs
column comparison — loop-free over simulations but O(field²); `frankv` measured
faster at *every* field size from 8 up (96 lanes: 2.6s → 0.14s), so there is no
crossover worth keeping.

`estimate_ability()`'s tactical trim is a vectorised rank-and-filter, not
`.SD[...]` per athlete-event group. The `.SD` form made data.table materialise a
sub-table per group and cost **74% of the function's runtime** for work that is
just "drop the worst k marks" (2.36s → 0.68s, output bit-identical). `.SD` with
a function call per group is the first thing to suspect if this gets slow again.

## Conventions

- `data.table` throughout; declare NSE symbols in `R/globals.R`
- snake_case columns everywhere
- Both source adapters return the *same canonical schema* so models never see
  feed-specific columns. Add new feeds by writing an adapter, not by teaching
  models about a new shape.
