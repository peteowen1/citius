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
