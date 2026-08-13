#' Estimate the effect of any categorical context on marks
#'
#' Generalises the pattern used for venue: centre each mark on its own
#' athlete-event mean so ability is removed, then read the average deviation per
#' level of the covariate. Suitable for lane, indoor/outdoor, track surface, or
#' anything else recorded as a category.
#'
#' Prefer this over a bespoke function per covariate. Wind is the exception —
#' it is continuous and gets a slope rather than per-level means, so it keeps
#' [fit_wind_effect()].
#'
#' **This measures association, not necessarily cause.** Several plausible
#' covariates fail on inspection because they restate something already
#' modelled: within-meet race number reproduces the round effect almost exactly,
#' and racing at home is confounded with racing at minor domestic meets. Check a
#' new covariate against what the model already contains before adopting it.
#'
#' @param results Canonical results.
#' @param covariate Column name holding the category.
#' @param by_event Fit separately per event. `TRUE` is right when the effect
#'   plausibly differs by event; `FALSE` pools, which suits covariates with few
#'   levels and thin per-event data.
#' @param min_n Minimum marks for a level to be estimated.
#' @return A `data.table` of levels, `effect` (log-units, positive = better),
#'   and `n`. Effects are centred so they add no intercept.
#' @export
fit_context_effect <- function(results, covariate, by_event = TRUE, min_n = 30L) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(event_id = character(), level = character(),
                                  effect = numeric(), n = integer())
  if (!covariate %in% names(dt) ||
      !all(c("perf", "event_id", "athlete_id") %in% names(dt))) return(empty)

  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(get(covariate))]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  dt[, level := as.character(get(covariate))]

  # Alternating fit, NOT a single within-athlete centring. Centring once
  # attenuates the effect whenever exposure is unbalanced: an athlete racing
  # fraction p on level A has p*e_A already inside their own mean, so their
  # deviation carries only (1-p) of the true effect. Averaging over a field
  # where strong athletes favour one level shrinks the estimate badly - on a
  # planted 0.010 with an 85/15 split it recovered about 0.005.
  grp <- if (by_event) c("event_id", "level") else "level"
  dt[, lev_eff := 0]
  for (i in 1:50) {
    dt[, ath_eff := mean(perf - lev_eff), by = .(athlete_id, event_id)]
    new <- dt[, .(le = mean(perf - ath_eff)), by = grp]
    dt <- merge(dt, new, by = grp, all.x = TRUE, sort = FALSE)
    delta <- max(abs(dt$le - dt$lev_eff), na.rm = TRUE)
    dt[, lev_eff := le][, le := NULL]
    if (is.finite(delta) && delta < 1e-9) break
  }

  out <- dt[, .(effect = data.table::first(lev_eff), n = .N), by = grp][n >= min_n]
  if (!nrow(out)) return(empty)

  if (by_event) {
    out[, effect := effect - stats::weighted.mean(effect, n), by = event_id]
  } else {
    out[, effect := effect - stats::weighted.mean(effect, n)]
    out[, event_id := NA_character_]
  }
  data.table::setcolorder(out, c("event_id", "level", "effect", "n"))
  out[]
}


#' Estimate the effect of a continuous covariate on marks
#'
#' The continuous counterpart to [fit_context_effect()]: fits a slope rather
#' than per-level means. [fit_wind_effect()] is a specialisation of this for
#' wind; use this for anything else measured on a scale, such as swimming
#' reaction time.
#'
#' Fitted per event by default, because the same covariate can matter very
#' differently across events — wind moves a 100m far more than a 200m, where
#' half the race is on the bend.
#'
#' **Note on predictive use.** Some covariates are only known *after* the race.
#' Reaction time is the clear case: you cannot know a future one. Such covariates
#' still earn their place by cleaning historical marks — a swimmer who botched
#' one start should not carry that in their ability estimate — but they cannot
#' be supplied for a race yet to happen. The athlete's typical value is already
#' inside their ability.
#'
#' @param results Canonical results.
#' @param covariate Numeric column name.
#' @param by_event Fit a separate slope per event.
#' @param min_n Minimum marks for a slope to be fitted.
#' @param trim Ignore values beyond this many SDs from the covariate mean;
#'   extreme values are usually recording errors and would lever the slope.
#' @return A `data.table` of `event_id`, `beta`, `n` and `r2`.
#' @export
fit_numeric_effect <- function(results, covariate, by_event = TRUE,
                               min_n = 200L, trim = 5) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(event_id = character(), beta = numeric(),
                                  n = integer(), r2 = numeric())
  if (!covariate %in% names(dt) ||
      !all(c("perf", "event_id", "athlete_id") %in% names(dt))) return(empty)

  dt[, xv := suppressWarnings(as.numeric(get(covariate)))]
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(xv)]
  if (!nrow(dt)) return(empty)

  mu <- mean(dt$xv); s <- stats::sd(dt$xv)
  if (is.finite(s) && s > 0) dt <- dt[abs(xv - mu) <= trim * s]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  # Within athlete-event: the slope is the effect on the *same* athlete, so it
  # cannot absorb the fact that better athletes differ systematically on the
  # covariate (faster swimmers also tend to have faster starts).
  dt[, dev := perf - mean(perf), by = .(athlete_id, event_id)]
  dt[, xdev := xv - mean(xv), by = .(athlete_id, event_id)]

  grp <- if (by_event) "event_id" else character(0)
  out <- dt[, {
    if (.N < min_n || stats::sd(xdev) <= 0) {
      .(beta = NA_real_, n = .N, r2 = NA_real_)
    } else {
      f <- stats::lm(dev ~ xdev)
      .(beta = unname(stats::coef(f)[2]), n = .N, r2 = summary(f)$r.squared)
    }
  }, by = grp]
  if (!by_event) out[, event_id := NA_character_]
  out[is.finite(beta)][]
}


#' Remove a fitted continuous covariate effect from marks
#'
#' @param results Canonical results.
#' @param numeric_effect A table from [fit_numeric_effect()].
#' @param covariate Numeric column name; must match the fit.
#' @return `results` with `perf` adjusted, relative to each athlete-event's own
#'   mean covariate value, and `numeric_adj` recording the shift.
#' @export
adjust_numeric <- function(results, numeric_effect, covariate) {
  dt <- data.table::copy(data.table::as.data.table(results))
  if (is.null(numeric_effect) || !nrow(numeric_effect) || !covariate %in% names(dt)) {
    dt[, numeric_adj := 0]
    return(dt[])
  }
  dt[, xv := suppressWarnings(as.numeric(get(covariate)))]
  dt[, athlete_id := as.character(athlete_id)]
  # Adjust relative to the athlete's own typical value: their average start is
  # part of who they are and belongs in ability, only the deviation is context.
  dt[, xdev := xv - mean(xv, na.rm = TRUE), by = .(athlete_id, event_id)]

  pooled <- all(is.na(numeric_effect$event_id))
  b <- if (pooled) rep(numeric_effect$beta[1], nrow(dt)) else
    numeric_effect$beta[match(dt$event_id, numeric_effect$event_id)]
  b[!is.finite(b)] <- 0
  x <- dt$xdev
  x[!is.finite(x)] <- 0

  dt[, numeric_adj := b * x]
  dt[, perf := perf - numeric_adj]
  dt[, c("xv", "xdev") := NULL]
  dt[]
}


#' Remove a fitted context effect from marks
#'
#' @param results Canonical results.
#' @param context_effect A table from [fit_context_effect()].
#' @param covariate Column name; must match the fit.
#' @return `results` with `perf` adjusted and `context_adj` recording the shift.
#'   Levels with no estimate are unchanged.
#' @export
adjust_context <- function(results, context_effect, covariate) {
  dt <- data.table::copy(data.table::as.data.table(results))
  if (is.null(context_effect) || !nrow(context_effect) || !covariate %in% names(dt)) {
    dt[, context_adj := 0]
    return(dt[])
  }
  pooled <- all(is.na(context_effect$event_id))
  key <- if (pooled) as.character(dt[[covariate]]) else
    paste(dt$event_id, as.character(dt[[covariate]]))
  ck <- if (pooled) context_effect$level else
    paste(context_effect$event_id, context_effect$level)

  adj <- context_effect$effect[match(key, ck)]
  adj[!is.finite(adj)] <- 0
  dt[, context_adj := adj]
  dt[, perf := perf - context_adj]
  dt[]
}

#' Measure the indoor/outdoor offset
#'
#' Indoor athletics is run on a banked 200 m track with tighter bends and no
#' wind, and the effect differs in SIGN by event family — sprinters and
#' middle-distance runners are slower indoors, distance runners are **faster**.
#' A single global offset would cancel those against each other.
#'
#' Measured on the athletics corpus (within-athlete, after round and tier):
#' middle −0.57%, sprint −0.37%, distance **+0.37%**, jump −0.19%, with hurdles,
#' throws and combined events showing nothing.
#'
#' This is a **race-level** property — constant across everyone in a race — so it
#' belongs with the round and tier offsets and is screened the same way, on
#' within-athlete residuals. Screening it within-race would remove it entirely by
#' construction. (Contrast an athlete-level covariate such as race momentum,
#' which must be screened within-race precisely because it can be confounded with
#' meet quality.)
#'
#' @param results Canonical results with `perf`, `indoor` and `event_id`.
#' @param min_n Minimum marks in each of indoor and outdoor for a family to be
#'   fitted; below it the offset is zero rather than noise.
#' @return A `data.table` of `family` and `offset`, where `offset` is subtracted
#'   from `perf` for indoor marks.
#' @export
fit_indoor_effect <- function(results, min_n = 2000L) {
  dt <- data.table::as.data.table(results)
  need <- c("perf", "indoor", "event_id", "athlete_id")
  miss <- setdiff(need, names(dt))
  if (length(miss)) cli::cli_abort("{.arg results} is missing {.field {miss}}.")
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(indoor)]
  if ("family" %in% names(dt)) dt[, family := NULL]
  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt <- dt[!is.na(family)]
  if (!nrow(dt)) return(data.table::data.table(family = character(), offset = numeric()))
  # Within athlete-event, so ability is removed and what remains is the setting.
  dt[, .n := .N, by = c("athlete_id", "event_id")]
  dt <- dt[.n >= 4L]
  dt[, .r := perf - mean(perf), by = c("athlete_id", "event_id")]
  out <- dt[, {
    ins <- .r[indoor %in% TRUE]; out_ <- .r[!(indoor %in% TRUE)]
    if (length(ins) < min_n || length(out_) < min_n) .(offset = 0, n_indoor = length(ins))
    else .(offset = mean(ins) - mean(out_), n_indoor = length(ins))
  }, by = family]
  out[]
}


#' Measure how far championship spread departs from pooled spread
#'
#' `sigma_within` is fitted across an athlete's whole history, but a forecast
#' targets a top-tier final. Those are not the same distribution. A championship
#' throw final holds implement, circle and officiating far more constant than the
#' corpus average, so the pooled spread is too wide for it; road running inverts,
#' because a championship marathon is tactical while most corpus road races are
#' paced time-trials.
#'
#' The ratio is the correction, measured rather than assumed. Against the
#' standardised backtest error it lines up closely — throw 0.681 against a
#' measured `sd(z)` of 0.698, road 1.141 against 1.142, correlation 0.80 across
#' eight families.
#'
#' Fitted per family rather than per event: an event-level ratio is a variance
#' ratio on a few thousand marks, and variance estimates are far noisier than
#' means at the same sample size.
#'
#' @param results Canonical results with `perf`, `athlete_id`, `event_id`,
#'   `tier` and `round`.
#' @param min_history Minimum marks an athlete-event needs to contribute; below
#'   this the within-athlete spread is mostly estimation noise.
#' @param min_n Minimum championship marks for a family to be estimated. Families
#'   below it get a ratio of 1, leaving `sigma` untouched.
#' @return A `data.table` of `family`, `ratio`, `sigma_pooled`, `sigma_champ`
#'   and the `n` behind each.
#' @seealso [estimate_ability()], which applies this to the sigma it returns.
#' @export
fit_sigma_context <- function(results, min_history = 4L, min_n = 500L) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(family = character(), ratio = numeric(),
                                  sigma_pooled = numeric(), sigma_champ = numeric(),
                                  n_pooled = integer(), n_champ = integer())
  need <- c("perf", "athlete_id", "event_id")
  if (!all(need %in% names(dt))) return(empty)
  dt <- dt[!is.na(perf) & !is.na(event_id)]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt <- dt[!is.na(family)]
  if (!nrow(dt)) return(empty)

  dt[, rc := .round_class(if ("round" %in% names(dt)) round else NA_character_)]
  dt[, tc := .tier_class(if ("tier" %in% names(dt)) tier else NA_character_)]

  # Centre within athlete-event so what remains is performance spread, not
  # ability differences. Athletes with a short record are excluded because their
  # sample SD carries almost no information about their true spread.
  dt[, nn := .N, by = .(athlete_id, event_id)]
  d <- dt[nn >= min_history]
  if (!nrow(d)) return(empty)
  d[, r := perf - mean(perf), by = .(athlete_id, event_id)]

  pooled <- d[, .(sigma_pooled = stats::sd(r), n_pooled = .N), by = family]
  champ <- d[tc == "top" & rc == "final",
             .(sigma_champ = stats::sd(r), n_champ = .N), by = family]
  out <- merge(pooled, champ, by = "family", all.x = TRUE)
  out[, ratio := sigma_champ / sigma_pooled]
  out[!is.finite(ratio) | is.na(n_champ) | n_champ < min_n | sigma_pooled <= 0,
      ratio := 1]
  out[]
}


#' Measure the seasonal phase of performance
#'
#' Athletes are not equally sharp all year. Within-athlete, and net of round,
#' tier, wind and indoor, northern outdoor performance runs roughly 0.3% above an
#' athlete's own average in May–July and 0.4–0.6% below it in September–October;
#' for throws the swing exceeds 1.8%. Distance and road invert, peaking in
#' November and December, which is the marathon calendar rather than a track
#' season.
#'
#' **This is a correction to HISTORY, not a term on the forecast.** Championships
#' sit in a fixed seasonal slot while an athlete's record spans the calendar, so
#' averaging unadjusted marks drags every ability estimate below its
#' championship-day level — and drags it furthest for athletes whose history
#' happens to be early-season-heavy. That is the same argument as the round and
#' tier offsets, and the reason the effect matters more than its size suggests.
#'
#' Validated out of sample before adoption: offsets fitted pre-2020 improve
#' prediction of 2020+ top-tier finals by 0.66% relative RMSE across 9,696
#' athlete-events, with 88% coverage.
#'
#' **Winter months on the northern side measure temperature, not form, and are
#' excluded.** January and February competition in the north is 95–98% indoor;
#' the outdoor remnant is the European winter throwing circuit. Within the same
#' athletes and weeks, indoor marks sit at their own average (+0.004%) while
#' outdoor ones are 0.94% slower — so a January penalty fitted as "month" would
#' apply a form correction to athletes who were merely racing in the cold, and
#' would mis-adjust the many who raced indoors and were perfectly sharp.
#'
#' @param results Canonical results with `perf`, `athlete_id`, `event_id` and
#'   `date`. A `venue_country` column, when present, splits the northern and
#'   southern calendars; without it a single pooled calendar is fitted, which is
#'   wrong for southern-hemisphere athletes and is why the column is worth
#'   carrying.
#' @param min_history Minimum marks an athlete-event needs to contribute.
#' @param min_n Minimum marks for a family-hemisphere-month cell.
#' @return A `data.table` of `family`, `hemi`, `month`, `offset` and `n`.
#' @seealso [fit_indoor_effect()], [estimate_context_effects()]
#' @export
fit_season_effect <- function(results, min_history = 4L, min_n = 1000L) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(family = character(), hemi = character(),
                                  month = integer(), offset = numeric(),
                                  n = integer())
  if (!all(c("perf", "athlete_id", "event_id", "date") %in% names(dt))) return(empty)
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(date)]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt <- dt[!is.na(family)]
  if (!nrow(dt)) return(empty)

  dt[, month := as.integer(format(as.Date(date), "%m"))]
  dt[, hemi := if ("venue_country" %in% names(dt)) {
    data.table::fifelse(!is.na(venue_country) & venue_country %in% .citius_south, "S", "N")
  } else "N"]

  dt[, nn := .N, by = .(athlete_id, event_id)]
  d <- dt[nn >= min_history]
  if (!nrow(d)) return(empty)
  d[, r := perf - mean(perf), by = .(athlete_id, event_id)]

  # Drop northern winter outdoor: it measures cold, not season phase.
  if ("indoor" %in% names(d)) {
    d <- d[!(hemi == "N" & month %in% c(1L, 2L, 12L) & !(indoor %in% TRUE))]
  }

  out <- d[, .(offset = mean(r), n = .N), by = .(family, hemi, month)]
  out <- out[n >= min_n]
  if (!nrow(out)) return(empty)
  # Centre within family-hemisphere so the offsets are a phase, not an intercept
  # shift that would move every mark in the family.
  out[, offset := offset - stats::weighted.mean(offset, n), by = .(family, hemi)]
  out[]
}

#' Countries whose competitive season follows the southern calendar
#'
#' Used only to split the seasonal phase fit. Kenya and Ethiopia are included
#' because their domestic calendars track the southern pattern despite sitting
#' near the equator.
#'
#' @keywords internal
#' @noRd
.citius_south <- c("AUS", "NZL", "RSA", "ARG", "BRA", "CHI", "URU", "ZAF",
                   "KEN", "ETH", "PER", "COL", "BOL", "PAR", "NAM", "BOT",
                   "ZIM", "MOZ", "ANG", "FIJ", "PNG", "SAM")


#' Measure how a global championship differs from another top-tier final
#'
#' Round and tier offsets are referenced to "final" and "top", so a top-tier
#' final receives a zero adjustment **by construction**. If a global championship
#' final differs from a Diamond League final, the existing parameterisation can
#' only call that difference zero — and it is not zero.
#'
#' The sign flips by family, which is why a pooled version is worse than nothing.
#' Endurance events go tactical at championships and run slower; power and
#' technical events arrive tapered and peaked and run faster. Measured within
#' top-tier finals only, so round and tier are held constant by construction:
#' road −1.45%, walk −0.30%, distance −0.07%, sprint +0.87%, jump +1.20%,
#' throw +1.28%.
#'
#' Validated out of sample: offsets fitted pre-2020 cut RMSE on 2020+
#' championship finals by **10.6% relative** (3.7763% to 3.3766%, n = 1,628),
#' while a pooled offset is *worse* than no adjustment at all.
#'
#' **This is not the per-family tier offset in another guise.** That one failed
#' out of sample by 16–20% because it re-parameterised a context the model
#' already handles, fitted on the low-tier population that dominates the corpus
#' and does not transfer to championships. This measures a distinction the model
#' cannot currently express at all.
#'
#' @param results Canonical results with `perf`, `athlete_id`, `event_id`,
#'   `tier` and `round`.
#' @param min_n Minimum championship marks for a family to be estimated.
#'   Families below it are omitted, and callers leave those marks unadjusted
#'   rather than borrowing a pooled value that measures the wrong sign.
#' @return A `data.table` of `family`, `offset` and `n`.
#' @seealso [estimate_context_effects()], [fit_sigma_context()]
#' @export
fit_championship_effect <- function(results, min_n = 100L) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(family = character(), offset = numeric(),
                                  n = integer())
  if (!all(c("perf", "athlete_id", "event_id") %in% names(dt))) return(empty)
  dt <- dt[!is.na(perf) & !is.na(event_id)]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  reg <- .citius_event_registry[, c("event_id", "family")]
  dt <- merge(dt, reg, by = "event_id", all.x = TRUE, sort = FALSE)
  dt <- dt[!is.na(family)]
  if (!nrow(dt)) return(empty)

  dt[, rc := .round_class(if ("round" %in% names(dt)) round else NA_character_)]
  dt[, tc := .tier_class(if ("tier" %in% names(dt)) tier else NA_character_)]

  # Both sides restricted to top-tier finals. This is the whole point: it holds
  # round and tier fixed so the offset can only measure what is left over. A
  # first attempt compared championships against ALL other marks and produced
  # absurd values (throw +5.33%) because it was absorbing the round and tier
  # effects that are already applied elsewhere.
  tf <- dt[tc == "top" & rc == "final"]
  if (!nrow(tf)) return(empty)
  tf[, champ := .is_championship(tier)]
  tf[, `:=`(n_c = sum(champ), n_o = sum(!champ)), by = .(athlete_id, event_id)]
  # Only athletes seen in BOTH contexts identify the gap; anyone appearing in one
  # contributes their ability, not a comparison.
  d <- tf[n_c > 0L & n_o > 0L]
  if (!nrow(d)) return(empty)

  d[, r := perf - mean(perf), by = .(athlete_id, event_id)]
  # The CONTRAST between contexts, not the deviation from the athlete's pooled
  # mean. Those differ by a factor that depends on the context mix: with an even
  # split, the championship deviation from the pooled mean is only half the true
  # gap, and callers subtracting it would correct half the effect. Caught by a
  # test planting a known 2% gap and recovering 1%.
  out <- d[, .(offset = mean(r[champ == TRUE]) - mean(r[champ == FALSE]),
               n = sum(champ)), by = family]
  out[is.finite(offset) & n >= min_n][]
}

#' @keywords internal
#' @noRd
.is_championship <- function(tier) {
  # "OW" is the World Athletics code for the global-championship category:
  # Olympics, World Championships and the Commonwealth Games sit here, while
  # Diamond League meets carry "GL" and everything else A..F.
  grepl("^OW", toupper(trimws(as.character(tier))))
}


#' Project ability onto the championship context
#'
#' [estimate_ability()] returns ability on a non-championship top-tier-final
#' footing, because that is what the round, tier and championship adjustments
#' put every historical mark on. A forecast of a global championship needs the
#' championship offset added back.
#'
#' Splitting it this way is what makes the correction do real work. An athlete
#' whose record is entirely championships is unchanged by the round trip — the
#' offset comes off their history and goes back on the forecast. An athlete with
#' only Diamond League form is moved by the full offset, which is exactly the
#' case the model was getting wrong: a road runner with nothing but paced
#' big-city marathons was being credited with championship-marathon ability.
#'
#' Applied at full strength rather than scaled by `(1 - shrinkage)`. Unlike the
#' age projection, this is a property of the RACE being forecast, not of how well
#' the athlete is known — a heavily-shrunk athlete still runs the same tactical
#' championship as everyone else.
#'
#' @param ability A table from [estimate_ability()], with `event_id` and
#'   `ability`.
#' @param calibration A `citius_calibration` carrying a `championship` table, or
#'   the table itself. `NULL` returns `ability` unchanged.
#' @return `ability` with `ability` shifted and `champ_adj` recording the shift.
#' @seealso [fit_championship_effect()]
#' @export
project_championship <- function(ability, calibration = NULL) {
  ab <- data.table::copy(data.table::as.data.table(ability))
  ce <- if (is.null(calibration)) NULL
        else if (inherits(calibration, "citius_calibration") || is.list(calibration) &&
                 !is.data.frame(calibration)) calibration$championship
        else calibration
  if (is.null(ce) || !nrow(ce) || !"event_id" %in% names(ab)) {
    ab[, champ_adj := 0]
    return(ab[])
  }
  reg <- .citius_event_registry[, c("event_id", "family")]
  fam <- reg$family[match(ab$event_id, reg$event_id)]
  adj <- ce$offset[match(fam, ce$family)]
  adj[!is.finite(adj)] <- 0
  ab[, champ_adj := adj]
  ab[, ability := ability + champ_adj]
  if ("ability_peak" %in% names(ab)) ab[, ability_peak := ability_peak + champ_adj]
  ab[]
}

#' Put an ability estimate onto the tier of the race being predicted
#'
#' [estimate_ability()] with `adjust_context = TRUE` SUBTRACTS the round and tier
#' offsets from every historical mark, so the ability it returns describes a
#' final at the TOP tier. [simulate_event()] then predicts a mark straight from
#' that number. Nothing puts the tier back, so every race — a club meet, a
#' Diamond League, a Games final — is predicted as though it were a top-tier
#' final.
#'
#' Measured on 3,696 backtest finals (2026-07-31), signed so that negative means
#' athletes ran WORSE than predicted:
#'
#' | event | champs | pro | lower |
#' |---|---|---|---|
#' | 5000m M | +0.25 | -0.80 | **-2.39** |
#' | 1500m M | +0.53 | -0.15 | -1.59 |
#' | 800m M | +0.23 | -0.06 | -0.46 |
#'
#' Championship races are the reference and are unbiased; everything below is
#' predicted too fast. Fitted jointly, tier explains more of the error than race
#' distance does (R2 0.093 against 0.054).
#'
#' This is the exact counterpart of [project_championship()], which exists
#' because the championship half of the same correction "has to go back on --
#' without this half the correction runs one way and makes predictions worse".
#' The tier half was never written.
#'
#' **Not applied by default.** It changes every non-top-tier prediction, so it
#' belongs in its own backtest arm rather than riding along with another change.
#'
#' @param ability Ability rows, as returned by [estimate_ability()].
#' @param tier Tier code for the race being predicted, recycled to `ability`.
#'   Raw feed codes are accepted and classified by the same rule the calibration
#'   was fitted under.
#' @param calibration A `citius_calibration` carrying a `$tier` table. `NULL`
#'   returns `ability` unchanged, which is the honest fallback: without measured
#'   offsets there is nothing to add back.
#' @param shrink Fraction of the measured offset to apply. **0.5 by default, and
#'   the default is measured, not chosen.** The offsets are fitted on ALL
#'   within-athlete history, while the races actually being predicted are a
#'   selected subset -- good enough to attract elite fields, and therefore faster
#'   than the average meet at their tier. Applying them in full overshoots: low
#'   tier goes from -0.98% bias to +0.71%.
#'
#'   Fitted on backtest finals before 2023 and evaluated on those after:
#'
#'   | lambda | test MAE | test bias |
#'   |---|---|---|
#'   | 0.0 | 2.3173 | -0.460 |
#'   | 0.4 | 2.3015 | -0.241 |
#'   | **0.5** | **2.3021** | **-0.186** |
#'   | 1.0 | 2.3297 | +0.089 |
#'   | 1.3 | 2.3658 | +0.255 |
#'
#'   An interior optimum bracketed on both sides. Note that zeroing the bias
#'   (lambda 1.3) makes MAE clearly worse -- the two objectives conflict, and
#'   MAE is the one with a threshold attached. Fitting a separate offset per
#'   tier was also tried and OVERFITS: it reversed the bias out of sample
#'   (-0.460 to +0.116) and worsened MAE, because low-tier bias is not stable
#'   across eras (train -0.570 against a test reality of -1.19).
#' @return `ability` with `ability` shifted and `tier_adj` recording the shift.
#' @seealso [project_championship()], [estimate_ability()]
#' @export
project_tier <- function(ability, tier, calibration = NULL, shrink = 0.5) {
  ab <- data.table::copy(data.table::as.data.table(ability))
  tt <- if (is.null(calibration)) NULL
        else if (inherits(calibration, "citius_calibration") ||
                 (is.list(calibration) && !is.data.frame(calibration))) calibration$tier
        else calibration
  if (is.null(tt) || !nrow(tt) || !"offset" %in% names(tt) || !nrow(ab)) {
    ab[, tier_adj := 0]
    return(ab[])
  }
  tc <- .tier_class(rep_len(as.character(tier), nrow(ab)))
  adj <- tt$offset[match(tc, tt$tier_class)] * shrink
  adj[!is.finite(adj)] <- 0
  # estimate_ability() SUBTRACTED this offset to reach the reference footing, so
  # returning to the target tier means adding it back with the same sign.
  ab[, tier_adj := adj]
  ab[, ability := ability + tier_adj]
  ab[]
}


#' Estimate athlete-specific heat coasting traits
#'
#' Measures how much an athlete systematically eases off in qualification
#' heats relative to finals, beyond the uniform round offset. Shrinks estimates
#' toward 0 via Empirical Bayes.
#'
#' @param results Canonical results table containing `perf`, `athlete_id`,
#'   `event_id`, `round`, and `tier`.
#' @param min_heats Minimum heat marks required to report an athlete. Default 2.
#' @param shrink_k Prior weight for Empirical Bayes shrinkage. Default 5.
#' @return A `data.table` of `athlete_id`, `coasting_trait`, and `n_heats`.
#' @export
fit_coasting_trait <- function(results, min_heats = 2L, shrink_k = 5.0) {
  dt <- data.table::as.data.table(results)
  empty <- data.table::data.table(athlete_id = character(), coasting_trait = numeric(), n_heats = integer())
  need <- c("perf", "athlete_id", "event_id")
  if (!all(need %in% names(dt))) return(empty)
  dt <- dt[!is.na(perf) & !is.na(event_id) & !is.na(athlete_id)]
  if (!nrow(dt)) return(empty)

  dt[, athlete_id := as.character(athlete_id)]
  dt[, rc := .round_class(if ("round" %in% names(dt)) round else NA_character_)]

  # REFERENCE TO FINALS, NOT TO THE ATHLETE'S OVERALL MEAN (fixed 2026-08-13).
  #
  # This measured `perf - mean(perf over ALL rounds)`, while its own
  # documentation and DECISIONS 2026-08-01 both describe it as a "heat-vs-FINAL"
  # trait -- and the round offset it has to compose with in `estimate_ability()`
  # is final-referenced too. The two references are not interchangeable: an
  # athlete's overall mean already contains their slow heats, so deviation from
  # it is systematically SMALLER than deviation from their finals.
  #
  # The size of that error is not subtle. On the current corpus the old form gave
  # a trait mean of -0.00111 against a pooled heat offset of -0.00649, so
  # subtracting the pooled offset to get the athlete-specific excess produced
  # +0.00537 -- POSITIVE, i.e. it would have pushed every jogged heat further
  # DOWN, the exact opposite of the correction intended.
  #
  # Referencing finals makes the trait mean comparable to the pooled offset by
  # construction, so the excess is a real athlete-specific residual.
  ref <- dt[rc == "final", .(ref_mean = mean(perf, na.rm = TRUE)),
            by = .(athlete_id, event_id)]
  if (!nrow(ref)) return(empty)
  dt <- merge(dt, ref, by = c("athlete_id", "event_id"))
  dt[, r := perf - ref_mean]

  # An athlete with heats but no finals in an event has no reference and is
  # dropped by that join, which is correct: "how much easier than their final"
  # is undefined without a final. Silently defaulting them to 0 would put a
  # made-up trait on exactly the thin-evidence athletes shrinkage exists for.
  heats <- dt[rc == "heat", .(dev = mean(r, na.rm = TRUE), n_heats = .N), by = athlete_id]
  if (!nrow(heats)) return(empty)

  heats <- heats[n_heats >= min_heats]
  if (!nrow(heats)) return(empty)

  # SHRINK TOWARD THE POPULATION HEAT EFFECT, NOT TOWARD ZERO (fixed 2026-08-13).
  #
  # This was `(n/(n+k)) * dev`, which pulls a thin-evidence athlete toward
  # "races heats exactly as hard as finals". Nobody does; the population runs
  # heats measurably easier. Every other shrinkage in this package targets the
  # pooled value -- see `ability.R:332`,
  # `offset = pooled + (raw - pooled) * n/(n+k)` -- and this one did not.
  #
  # The consequence was not a small bias. Shrinking to zero drove the trait mean
  # to -0.0025 against a pooled heat offset of -0.00649, so the trait looked
  # like LESS coasting than the population average, and the athlete-specific
  # excess came out positive for most athletes. Shrinkage strength was
  # masquerading as a measurement.
  #
  # Referenced to finals (above) and shrunk to the pooled deviation (here), the
  # trait is now on the same scale as the round offset, so `estimate_ability()`
  # can subtract the difference and get a real athlete-specific residual.
  pooled_dev <- mean(dt[rc == "heat", r], na.rm = TRUE)
  if (!is.finite(pooled_dev)) pooled_dev <- 0
  heats[, coasting_trait := pooled_dev +
          (n_heats / (n_heats + shrink_k)) * (dev - pooled_dev)]
  heats[, .(athlete_id, coasting_trait, n_heats)]
}


