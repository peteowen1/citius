# CLAUDE.md

Guidance for Claude Code working in the `citius` R package.

## Package Overview

`citius` estimates latent athlete ability from competition result histories and
simulates event outcomes for major multi-sport games. Athletics and swimming are
the two sports built out; the abstractions are deliberately sport-agnostic so
further sports slot in by extending the event registry only.

Named for *Citius, Altius, Fortius* — the package predicts marks and placings.

## Development Commands

```r
devtools::load_all("citius")
devtools::test()
devtools::document()
devtools::check()
```

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
affects an answer is estimated from data by `calibrate()`:

| Quantity | How it is measured |
|----------|--------------------|
| `sigma_within` | Residual sd from the two-way decomposition, df-corrected |
| `condition_sd` | Sd of fitted race effects, de-biased for sampling noise |
| Round/tier offsets | Mean residual by context, referenced to final / top tier |
| Round/tier weights | **Precisions** — inverse residual variance, not preference |
| `sensitivity` | Per-athlete slope on the race effect, attenuation-corrected, EB-shrunk |
| `foul_rate` | Observed no-mark rate in technical events |
| `tactical` | Skewness of race effects; negative skew means races run slow |

The registry's `cv_prior` is a **fallback placeholder**, not an estimate. It
exists so the simulator runs before any harvest. Check
`calibration$events$calibrated` to see which events are on real numbers.
If you find yourself typing a number into a model, stop and estimate it.

### What the first real calibration measured

From 58,711 performances / 643 athletes / 9,551 multi-athlete races (2026-07-27):

- **Every hand-set guess was 2–7x too small.** Measured `condition_sd` for the
  men's 100m is 0.0134 against a guessed 0.0020. Discus (wind-exposed) is 0.068.
- **Within-athlete spread orders exactly as intuition would predict — but was
  measured, not assumed**: sprint 0.024 < distance 0.028 < hurdles 0.029 <
  middle 0.031 < road 0.037 < walk 0.040 < jump 0.046 < throw 0.068.
- **Finals are the *noisiest* round, not the most informative.** Measured
  precisions: quarter 1.00, semi 0.85, heat 0.58, **final 0.25**. The original
  hand-set ladder had final = 1.00 and heat = 0.75 — exactly backwards. Finals
  carry tactical racing and peaking, which makes them less predictable.
- **Semi-finals run *faster* than finals** (offset +0.0056), consistent with
  finals being tactical.
- Tier direction was right: top 1.00, high 0.62, mid 0.59, low 0.28; low-tier
  meets run 4.0% slower.
- Shared conditions dominate distance events (10,000m W `cond_share` 0.70,
  Marathon W 0.66) far more than sprints.
- Condition sensitivity genuinely varies: sd 0.235, 5–95% range 0.63–1.39.

**Do not reinstate the old weight ladders.** They were measurably wrong.

### Measured aging curves

Peak ages from within-athlete variation (58k performances). Six of eight
families are identified; two are not and are flagged as such.

| Family | Peak | Plateau | Identified |
|--------|------|---------|-----------|
| sprint | 25.2 | 23.4–27.8 | yes |
| jump | 26.0 | 24.6–27.7 | yes |
| distance | 26.6 | 25.0–33.5 | yes |
| middle | 27.6 | 25.9–33.2 | yes |
| throw | 30.9 | 28.6–33.4 | yes |
| walk | 32.9 | 30.7–35.5 | yes |
| hurdles | 33.6 | — | **no** |
| road | 44.2 | — | **no** |

Three things about this fit are load-bearing:

**Within-athlete only.** Performances are centred on their athlete-event mean
before fitting, so ability is removed and the curve comes from athletes observed
at multiple ages. A cross-sectional fit would measure which ages elite athletes
happen to be — sprinters still competing at 34 are a heavily selected group, and
their survivorship would read as longevity.

**Peak search is density-restricted.** Age curves are near-flat across the
plateau, so an unrestricted `which.max` latches onto smoother drift in the
sparse tail. Before the guard, middle-distance peaked at 37.2 on **3
observations** and road running at 49.0 on **1**.

**Boundary peaks are flagged, not reported.** When the curve is still rising
where data runs out, `peak_identified` is `FALSE` and a warning fires.
`peak_age` is then a lower bound. Do not quote it as an estimate.

**Known limitation:** the within-athlete design removes cross-sectional
selection but *not attrition* — athletes who decline stop appearing, so decline
at older ages is understated.

### Two bugs that produced plausible-looking nonsense

Both were caught only because tests planted known parameters and checked
recovery. Neither threw an error.

1. **Ability must be per athlete-*event*.** Grouping `a_i` by `athlete_id` alone
   pooled a sprinter's 100m (log ≈ 2.33) and 200m (log ≈ 3.05) into one number
   and dumped the 0.7 gap into the residual. Symptom: within-athlete CVs of
   20–130%.
2. **Singleton races must not get a free race effect.** A race with one
   harvested athlete fits its deviation exactly (residual 0) while still costing
   a degree of freedom, collapsing `df` and inflating every variance. They now
   keep `c_r = 0` and their deviation stays in the residual.

3. **`performanceValue` is not trustworthy — use `mark_string`.** Upstream drops
   trailing zeros on round marks, so a `"6.00"` metre vault arrives as
   `performanceValue = 6` and converts to six centimetres. It hit 6% of one
   elite vaulter's career and 441 marks (0.75%) across the harvest, concentrated
   in field and combined events. `.resolve_mark()` now parses the display string
   first and falls back to the integer only when the string is unusable.

4. **A tie-break offset can be annihilated by the sentinel it is added to.**
   Fouled athletes were pushed below the field with `-1e300 + runif(0, 1e-9)`.
   At that magnitude a double's ULP is ~1e284, so the offset vanished and every
   fouled athlete held *exactly* `-1e300`. The pairwise ranker gave them all the
   same placing, so a race could return ranks `1,2,3,3,3,3` — duplicated, with
   the tail never awarded. Whenever `n - k + 1 <= 3`, **every fouled athlete was
   credited with a medal**. Pole Vault M (13.9% no-marks) and 10,000m W (11.1%)
   are where this bit. The sentinel is now `-1e6 - runif(0, 1)`: far below any
   log-scale mark, with the random order fully representable. Invariants worth
   asserting anywhere placings are produced — both now in `test-simulate.R`:
   **every row is a strict permutation of `1:n`**, and **`sum(p_medal)` is
   exactly `min(n, 3)`**.

5. **Round labels nest, and `.round_class()` overwrites.** The feed's semi-final
   label is `"Semifinal - Heat"` — it contains `HEAT`, `SEMI` *and* `FINAL`.
   With the patterns applied heat → semi → quarter → final, the last match won
   and every semi-final classified as a **final**: 14,764 results, 4.79% of the
   harvest, folded into the reference context every other offset is measured
   against. The `semi` bucket was simply empty and nothing said so. Patterns now
   run least-specific to most-specific.

   **Measured impact: negligible.** Round offsets moved by ~3e-6 on the log
   scale (heat −0.002967 → −0.002964), because the recovered semi offset is
   +0.000033 — semis and finals run at effectively the same speed in this
   harvest. Worth fixing because the bucket now exists and the classifier is
   correct for feeds that label rounds differently, **not** because it moved the
   numbers. Round *precisions* are computed in `calibrate()`, not
   `estimate_context_effects()`, and have not been re-measured — that is where
   an effect is more plausible, since pooling more-predictable semis into finals
   would inflate the final precision and over-weight finals.

**The most important lesson from bug 3:** the Hampel filter in
`flag_implausible()` was *hiding* it. Those "implausible" 0.03m vaults were real
3.00m clearances. A robust outlier filter will absorb a systematic unit error
and produce clean-looking output with real data silently deleted — worse than
crashing. After the fix, flagging fell from 0.80% to 0.26%.

**Never add an outlier filter without first checking the outliers are real.**

### Harvest competitions, not athletes — the two endpoints differ

`athlete_results()` and `competition_results()` are **not** interchangeable
views of the same data. The athlete endpoint silently drops results with no
valid mark: an elite pole vaulter's 209-result history contains zero `NA` marks,
which cannot be true. The competition endpoint keeps them as empty mark strings.

Two things are therefore only measurable from competition-level harvests, and
both calibrate to zero if you harvest athletes instead:

- **No-mark rates** — fouls, no-heights, DNF, DNS
- **Shared race effects** — needs whole fields, not one athlete at a time

Use `harvest_competitions()`, and **do not filter `NA` marks before
calibrating** — they are the signal.

Measured no-mark rates (Tokyo 2025 + Paris 2024 + Birmingham 2022):

| Event | Rate |
|-------|------|
| Pole Vault M | 13.9% |
| 10,000m W | 11.1% |
| Marathon M | 10.0% |
| Shot Put M | 8.3% |
| Long Jump W | 5.6% |
| 100m M | 1.2% |

**No-marks apply to track events too.** A field athlete fouling out and a
distance runner failing to finish are the same event for ranking, and
double-digit DNF rates in championship distance races materially move medal
probabilities. `simulate_event()` applies the measured rate to every event type,
not just technical ones.

### Swimming calibration (Glasgow 2026, 882 swims / 109 races)

First swimming calibration, from the Commonwealth Games CRS scrape:

- Round offsets came out cleanly monotonic: heats **0.77% slower** than finals,
  semis 0.41% slower — the taper/effort gradient, measured with pool conditions
  held constant.
- Round precisions: heat 1.00, semi 0.35, **final 0.24**. Finals are the
  noisiest round *again*, now confirmed from data completely independent of the
  athletics harvest.

**Do not compare swimming's `sigma_within` (0.0073) to athletics' (0.0376)
directly.** The athletics figure is career-scale across a decade and every meet
tier; the swimming figure is within one three-day championship. Short-term
consistency is always tighter. They become comparable only once swimming is
harvested across seasons.

### The Commonwealth Games results system is real but unusable in CI

Glasgow 2026 runs its own Competition Results System (Microplus):
`https://crs-cg2026-api.glasgow2026.com/api/v2/` with `entries`, `results`,
`events`, `schedules/startList`, `medals`, `records`. Config is public at
`https://crs-cg2026.glasgow2026.com/assets/api_config.json`.

It is **not usable unattended**: Cloudflare challenges any non-browser client,
and the API returns `401 Unauthorized` without a bearer token that the Angular
app obtains at runtime (interceptor sets `Authorization`, `If-None-Match:
microplus`, `Cache-Control: Public` — replicating those headers is not enough).

Do not build the pipeline on it, and **do not extract the token**. For athletics,
World Athletics ingests Commonwealth results after the fact — Birmingham 2022
(`7147633`) is fully present — so `competition_results(7187518)` will work once
the feed populates. `citiusdata/scripts/watch_glasgow2026.R` polls for that.

For **swimming there is no federation route** — World Aquatics does not sanction
the Commonwealth Games — so the CRS is the only source. The app renders results
publicly, so they are scraped from the rendered pages, exported as JSON, and
read by `parse_crs_export()`. Recipe:
`citiusdata/scripts/scrape_games_crs.md`. The captured file lives at
`citiusdata/data/glasgow2026_swimming.json` (882 matched swims, 109 races).

Two traps in that scrape, both documented in the recipe: the results table is
**tab**-separated with each athlete spanning three lines (whitespace splitting
loses most rows), and Para events carry an extra sport-class column that shifts
the layout.

### The registry covers the union of Games programmes, not the Olympic subset

50m Backstroke, Breaststroke and Butterfly are contested at the Commonwealth
Games and World Championships but not the Olympics. Building the registry from
the Olympic programme left 296 Glasgow swims unmatched until they were added.
`match_event()` returning `NA` is what surfaced this — had it fuzzy-matched
"50M BUTTERFLY" onto "100m Butterfly", 76 swims would have been silently
corrupted. **Keep it strict.**

### Recency decay is fitted, and shrinkage uses total weight

`fit_half_life()` estimates how fast form decays by holding out each athlete's
most recent result and predicting it from earlier ones. Measured: **sprint and
walk ~135 days, distance/middle/road/jump/throw ~180**. The old hand-set 540-day
default was 3–4x too long and kept stale form alive.

Boundary optima are flagged `identified = FALSE` and replaced with the pooled
value — the same trap as the aging peak. `combined` fails this (n=6).

**Shrinkage uses `w_total` (total weight), not `n_eff`.** `n_eff` measures how
*evenly* weight is spread, so an athlete with 44 results all twenty years old
keeps a high `n_eff` — their weights are uniformly tiny — and escapes shrinkage
entirely, retaining a junior mark that tops the field. Total weight is absolute
evidence, so stale athletes regress to the event mean on their own. **This is
why there is no staleness cutoff**; `project_field(max_stale_years=)` defaults
to `Inf`.

### Three scaling traps that produced confident nonsense

All three passed every unit test and only showed up as implausible predictions.

1. **`age_ref` must be the weighted mean age.** Using an unweighted career mean
   double-counts ageing, because recency decay already makes the estimate
   current. It projected a sprinter *faster than his personal best* and made a
   4-result junior the 100m favourite. `estimate_ability()` now returns
   `age_ref` computed under the ability weights.
2. **Context precisions must normalise to mean 1**, not to "best context = 1".
   Normalising to the best made finals 0.254, and since finals are the most
   common round every weight was deflated ~4x. `w_total` left the
   "number of results" scale and shrinkage collapsed **833 of 1,784 estimates
   past 90%** — abilities became near-identical and the ranking was decided by
   whatever varied next.
3. **The age projection must scale by `(1 - shrinkage)`.** Ageing a number that
   is mostly the event mean is meaningless. Combined with trap 2 it put
   sprinters at the top of the triple jump. Age shifts fell from ±24% to
   −0.4/+1.4% once fixed.

The through-line: none of these changed *whether* the code ran, only what scale
things sat on. Sanity-check predictions against athletes you recognise — that is
what caught all three.

### Backtest: the model has skill, but the sample is small

Strict out-of-sample test over 133 harvested championships — ability estimated
per competition using only performances dated *before* it began, entrants taken
from the actual finals. 64 events, 601 predictions.

Restricting to the 50 events where the actual winner was in our harvested field
(the other 14 measure harvest coverage, not the model):

| | Brier | Baseline | Skill |
|---|-------|----------|-------|
| Gold | 0.065 | 0.085 | **+0.23** |
| Medal | 0.185 | 0.266 | **+0.30** |

Skill is against a uniform-within-event baseline. Absolute Brier is close to
meaningless here — a 30-entrant event scores far better than an 8-entrant one
purely from base rates.

**Do not over-read this.** 50 events with ~10 gold outcomes cannot distinguish
close configurations. It is enough to say the model beats "everyone equally
likely"; it is not enough to rank two calibrations against each other.

### Swimming backtest: validated at scale, over-confident at the top

59,642 swims / 8,141 athletes / 57 World Aquatics meets, 2017–2025. Backtest:
**895 scoreable races**, gold skill **+0.234**, medal skill **+0.417**. Coverage
92% (athletics 78%) because World Aquatics carries a stable `PersonId`, so
cross-meet athlete linking is exact rather than name-matched. Beats baseline on
600 of 895 races (67%), against the ~64% a perfect model manages.

**Calibration fails in the opposite direction from athletics.** Below 30% the
gap is +0.010; above 30% it is −0.094, reaching −0.23 in the top bin — when
swimming says 94%, the athlete wins 72%.

Diagnosis, not yet fixed: `sigma_within` for swimming is measured across heats,
semis and finals *within one meet*, but the backtest predicts a final from
*prior meets*. Between-meet variation — form change, taper timing — is larger
than within-meet variation, so the model treats favourites as far safer than
they are. Athletics avoids this because its harvest spans years per athlete, so
its sigma already reflects between-meet spread.

**Fixed by simulating ability uncertainty, not by inflating sigma.**
`estimate_ability()` now returns `ability_se`, the empirical-Bayes posterior sd
(`sigma / sqrt(w_total + kappa)`), and `simulate_event()` draws it alongside
performance noise. The two are different quantities and both belong in a
forecast: `sigma` is how much an athlete varies around their own true ability,
`ability_se` is how little we know that ability.

Swimming, 895 races, before → after:

| | before | after |
|---|--------|-------|
| gold skill | +0.234 | **+0.253** |
| medal skill | +0.417 | **+0.443** |
| mean \|gap\| | 0.0735 | **0.0345** |
| gap above 30% | −0.094 | **−0.023** |
| top bin | 0.94 → 0.72 | 0.93 → 0.96 |
| races beating baseline | 67% | **70%** |

Calibration *and* skill both improved. That is the signature of a correct
modelling fix; a tuning knob trades one against the other.

**Rejected hypothesis, recorded so it is not retried:** heat seeding. Swimming
heats are seeded by ability, so it looked like race effects might be absorbing
ability rather than conditions. Testing finals-only *raised* the shared share
(0.75 vs 0.71) instead of lowering it — the shared effect is real, and the
problem was the missing uncertainty term.

Athletics is inconclusive on this change: gold skill 0.125 → 0.120, medal
0.233 → 0.249, with only 26 predictions above 30%. Its `w_total` is much smaller
(short fitted half-life), so `ability_se` is proportionally larger there. Revisit
once backtest coverage improves.

Other measured swimming values: `tail_df` 20; shared-condition share 0.59–0.78,
far above athletics' 0.28 — in a pool that is session effects, not weather;
heats run 1.12% slower than finals. Half-life for sprint and middle distance
still pins to the 90-day grid floor and is flagged unidentified — swimming form
appears to decay faster than the grid can currently express.

### Tail weight is fitted, and kurtosis is the wrong tool

`fit_tail_df()` chooses the t degrees of freedom by matching observed
`P(|z| > k)` at moderate `k`, not by matching a moment.

Measured excess kurtosis on championship data was **32**, implying `df ≈ 4` —
but that is driven by a handful of `|z| > 10` values that are feed errors, not
performances. The actual tail mass sits barely above normal:

| | observed | normal | t(6) |
|---|---|---|---|
| P(\|z\|>2) | 0.054 | 0.046 | **0.154** |

The old hard-coded `df = 6` put roughly **three times** too much mass in the
shoulder that decides races, manufacturing upsets. Fitted values: 15–20.

Following kurtosis would have moved `df` the wrong way and made the
under-confidence worse. **Fit tails on probability mass, never on moments.**

Wiring it in cut the reliability gap above 30% from **+0.157 to −0.043**. Brier
skill also fell (0.23 → 0.13), which is a calibration/discrimination trade-off
that this sample size cannot resolve. Both configurations beat baseline.

### Why whole fields are required

The two-way decomposition `perf = athlete + race + residual` is what makes the
shared shock identifiable. A per-athlete history cannot distinguish "off day"
from "slow race for everyone". `add_race_key()` reconstructs race groupings from
athlete histories (same competition + event + round + date); coverage depends on
how much the harvested athletes overlap.

### Two corrections that are not optional

**De-biasing the shared shock.** Fitted race effects carry sampling noise, so
their raw variance overstates the true shock — with no shock at all, a
4-athlete field still produces apparent condition variance. Subtracting
`var(e)/n_r` fixes this. Verified in `test-calibrate.R`: bias falls from 0.0038
at 4-athlete fields to 0.0013 at 8 and 0.0004 with more races. **Small fields
leave residual upward bias**; this is a known limitation, not a bug.

**Attenuation in sensitivity.** Slopes are regressed on a *fitted* race effect
carrying error, which biases them toward zero. The correction rescales by the
ratio of observed to de-biased race-effect variance.

Sensitivity scale is **not identified** separately from `condition_sd` —
doubling all sensitivities and halving the shock gives identical performances.
It is therefore normalised to mean 1 and magnitude is left to
`race_conditions()`. Do not "fix" this by rescaling one without the other.

## Findings that are easy to get wrong

These were established empirically against live data. Re-deriving them wrongly
is the main risk when extending the package.

### A shared race shock changes placings only through sensitivity

If `perf_i = ability_i + c + eps_i` with `c` common to the field, `c` cancels
out of every pairwise comparison, so it affects **absolute marks only**, never
medal probabilities. Verified in `test-simulate.R`.

What makes conditions matter for placings is `condition_sensitivity()`: with
`perf_i = ability_i + s_i * c + eps_i` and `s_i` varying across athletes, the
shock becomes an interaction and genuinely reorders the field. Sensitivities
are estimated per athlete by `calibrate()`, not assigned.

Without a calibration every `s_i` is 1 and the shock is inert for medals. That
is the correct fallback — it fails visibly rather than encoding a guess.

### Top-lists destroy variance estimation

Ability can be estimated from bests; spread cannot. Any source ranked by mark
(e.g. all-time top-list dumps) is truncated at the good end. Use full result
histories — `athlete_results()` and `aquatics_results()` both return every
performance including heats, deliberately.

### Raw within-athlete CV is not performance noise

An elite 1500m runner's career spread is ~4.5%, but most of that is tactical
championship races, not form. Hence `tactical` + `trim_tactical`. Applying a
naive SD to distance events inflates upset probability badly.

### A weighted mean of all races under-predicts a final

Heats are coasted and minor meets are shallow. `estimate_context_effects()`
recovers round and tier offsets from within-athlete residuals (which removes
ability, so the fact that better athletes reach more finals is not absorbed into
the "final effect"), and `estimate_ability(adjust_context = TRUE)` puts every
performance on a final-equivalent, top-tier footing. Measured on real 100m data:
heats run ~0.9% slower than finals, low-tier meets ~3.3% slower.

## Data sources

| Sport | Endpoint | Auth | Notes |
|-------|----------|------|-------|
| Athletics | `worldathletics.nimarion.de` | none | Community REST wrapper over World Athletics' unofficial GraphQL. Absorbs the rotating upstream API key. Override via `options(citius.athletics_base=)`. |
| Swimming | `api.worldaquatics.com/fina` | none | **Official** and first-party. Archive back to 1896; LA 2028 already listed. |

The results feed for athletics carries no sex or DOB — `athlete_results()` looks
both up from the profile endpoint, because `match_event()` needs sex and the
aging curve needs DOB.

swimrankings.net is behind a Cloudflare JS challenge and is not usable from
plain HTTP. It is not needed.

World Athletics put results services out to tender from January 2026, so treat
the athletics base URL as configuration, never a constant.

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

Backtest cost is dominated by the per-meet refit, so `backtest_athletics.R`
restricts history to the meet's own events and to `HISTORY_DAYS` before the cut.
Both are exact given the recency decay, not approximations.

## Conventions

- `data.table` throughout; declare NSE symbols in `R/globals.R`
- snake_case columns everywhere
- Both source adapters return the *same canonical schema* so models never see
  feed-specific columns. Add new feeds by writing an adapter, not by teaching
  models about a new shape.
