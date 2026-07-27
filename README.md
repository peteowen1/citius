# citius

> Performance modelling and outcome simulation for major games athletics and swimming

`citius` turns competition result histories into the things you actually want to
know before a final: who wins, who medals, and how likely anyone is to break a
given mark.

Named for *Citius, Altius, Fortius*.

## What it does

1. **Sources** full result histories from World Athletics and World Aquatics —
   every performance, not top-lists.
2. **Estimates** each athlete's latent ability per event, with recency decay,
   empirical-Bayes shrinkage, and adjustment for the fact that heats are coasted
   and minor meets are shallow.
3. **Simulates** the event thousands of times, drawing shared race conditions
   and per-athlete performance, to produce medal and mark probabilities.

## Quick start

```r
library(citius)

# 1. Harvest histories for the athletes you care about
ids  <- vapply(c("Kishane Thompson", "Noah Lyles", "Letsile Tebogo"),
               \(n) find_athlete(n)$athlete_id[1], numeric(1))
hist <- data.table::rbindlist(lapply(ids, athlete_results), fill = TRUE)

# 2. Measure the model's parameters from the data
cal <- calibrate(add_race_key(hist))

# 3. Estimate ability and simulate
sprint <- hist[event_id == "AT-100Metres-M" & legal == TRUE]
ab  <- estimate_ability(sprint, calibration = cal)
sim <- simulate_event(ab, n_sims = 50000, calibration = cal)

medal_probs(sim)
prob_better_than(sim, 9.80, who = "any")
```

Swimming works the same way through `aquatics_competitions()`,
`aquatics_disciplines()` and `aquatics_results()`.

## Design notes

**Nothing is hand-tuned.** Every parameter that affects an answer is measured by
`calibrate()`: within-athlete spread, the size of the shared race shock, round
and tier offsets, how informative each context is, per-athlete condition
sensitivity, foul rates, and whether an event's races skew slow. The event
registry carries coarse fallbacks so the simulator runs before any harvest, but
those are labelled placeholders, not estimates.

**Whole fields are what make it work.** Splitting `perf = athlete + race +
residual` is the only way to tell "off day" apart from "slow race for everyone",
and it needs performances that shared a race. `add_race_key()` recovers those
groupings from athlete histories, so calibration quality scales with how many
athletes you harvest.

**Everything is on an oriented log scale.** `to_perf()` maps marks to a space
where higher is always better, so a 9.58s sprint and an 8.95m long jump are
directly comparable to the models.

**The event registry is the source of truth.** `citius_events()` records which
events are *tactical* (championship finals decouple time from placing) and which
are *technical* (fouls are a discrete failure mode, not a bad mark). These flags
change how ability and variance are estimated.

**Shared conditions move marks; sensitivity moves medals.** A race shock common
to the whole field cancels out of every pairwise comparison, so on its own it
affects `prob_better_than()` and leaves medal probabilities untouched. What
reorders the field is athlete-specific *sensitivity* to that shock — an
interaction, estimated per athlete by `calibrate()`.

## Data sources

| Sport | Source | Auth |
|-------|--------|------|
| Athletics | World Athletics (via community REST wrapper) | none |
| Swimming | World Aquatics official API | none |

## Status

Early. The ability model and simulator are working and tested against live data.
Round progression (heats → semis → final), aging curves and calibration
backtesting are in progress.

## License

MIT
