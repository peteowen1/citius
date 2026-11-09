# Thin-record adjustments to an estimate_ability() table (citiusverse, 2026-11-09).
#
# WHY. estimate_ability() shrinks every athlete towards `prior_mu`, the mean
# ability of EVERYONE rated in the event, with weight kappa / (w_total + kappa).
# For an elite athlete with a thin recent record (few races inside the
# half-life) that pulls them towards a club-level mark: Faith Kipyegon, one
# 1500m in 2026, was rated 4:08.1 against 13 races since 2024 run between
# 3:48.68 and 4:04.79 (w_total 0.16, shrinkage 0.27, prior_mu ~4:38).
#
# WHAT. Two post-steps, applied in this order, both OFF unless a caller asks:
#   1. apply_own_record_prior(): keep the SAME shrinkage weight, but pull towards
#      the athlete's own longer record (ability_raw refitted with every event
#      half-life multiplied, `long_ability`) instead of the event mean.
#   2. apply_gap_offsets(): add a measured offset by days since the athlete's
#      last result, RELATIVE to athletes who raced within 60 days (so it carries
#      no overall level shift).
#
# MEASURED (citiusdata/scripts/diagnostics/backtest_thin_record_prior.R, 800 /
# 1500 / 5000 m both sexes, offsets fitted on 2024 cuts, verdict on 4 2025 cuts,
# 71,361 next results): marks error 2.250% deployed -> 2.081% with both steps;
# within-race concordance 67.296% -> 68.302%. Pete chose this version (C).
# It then FAILED the championship backtest (2026-11-09, 719 M1 meets; on R1 races
# 2025+ concordance -1.33, CI -2.58 to -0.14) and is not deployed. The successor,
# version E, is a recent/own-record blend with no event-mean term
# (citiusdata/scripts/backtest_athletics.R, CITIUS_THIN_BLEND).

#' Pull thin-record athletes towards their own longer record
#'
#' Replaces the shrinkage target `prior_mu` (the event-wide mean) with the
#' athlete's own long-horizon ability, keeping the shrinkage weight unchanged.
#' Rows with no long-horizon estimate keep the event-mean target.
#'
#' @param ability A table from [estimate_ability()]: needs `athlete_id`,
#'   `event_id`, `ability`, `ability_raw`, `shrinkage`, `prior_mu`.
#' @param long_ability A table from [estimate_ability()] fitted on the same
#'   history and `as_of` with every half-life multiplied (the measured version
#'   used x5); only `athlete_id`, `event_id` and `ability_raw` are read.
#' @param min_shrinkage Apply only where `shrinkage >= min_shrinkage`. The
#'   measured run applied it everywhere (0); a large-pull-only variant avoids a
#'   small cost on athletes with a light pull (~0.045 pp marks error).
#' @return `ability` (a copy) with `ability` replaced where applied, the old
#'   value in `ability_event_prior`, the target in `own_prior`, and a logical
#'   `own_prior_applied`.
#' @export
apply_own_record_prior <- function(ability, long_ability, min_shrinkage = 0) {
  need <- c("athlete_id", "event_id", "ability", "ability_raw", "shrinkage", "prior_mu")
  miss <- setdiff(need, names(ability))
  if (length(miss)) cli::cli_abort("{.arg ability} lacks {.field {miss}}.")
  miss <- setdiff(c("athlete_id", "event_id", "ability_raw"), names(long_ability))
  if (length(miss)) cli::cli_abort("{.arg long_ability} lacks {.field {miss}}.")
  ab <- data.table::copy(data.table::as.data.table(ability))
  lg <- data.table::as.data.table(long_ability)[, .(athlete_id = as.character(athlete_id),
                                                    event_id, own_prior = ability_raw)]
  lg <- unique(lg, by = c("athlete_id", "event_id"))
  key_type <- class(ab$athlete_id)
  ab[, athlete_id := as.character(athlete_id)]
  ab[lg, own_prior := i.own_prior, on = c("athlete_id", "event_id")]
  if (!"own_prior" %in% names(ab)) ab[, own_prior := NA_real_]
  ab[, ability_event_prior := ability]
  ab[, own_prior_applied := !is.na(own_prior) & !is.na(shrinkage) & shrinkage >= min_shrinkage]
  ab[own_prior_applied == TRUE, ability := (1 - shrinkage) * ability_raw + shrinkage * own_prior]
  n_ok <- ab[, sum(own_prior_applied)]
  cli::cli_alert_info(
    "own_record_prior: applied to {n_ok} of {nrow(ab)} ability rows (min_shrinkage {min_shrinkage}); {ab[is.na(own_prior), .N]} had no long-horizon estimate.")
  if (identical(key_type, "integer")) ab[, athlete_id := as.integer(athlete_id)]
  ab[]
}

#' Gap buckets for [apply_gap_offsets()]
#' @keywords internal
.gap_bucket <- function(days) {
  cut(days, c(-Inf, 60, 120, 240, 365, Inf),
      c("<60d", "60-120d", "120-240d", "240-365d", ">365d"))
}

#' Fit time-since-last-result offsets
#'
#' Mean of `perf - ability` by gap bucket, made relative to the `<60d` bucket so
#' the offsets carry no overall level shift.
#'
#' @param scored A table with `perf` (the next result), `ability` (the rating
#'   before it) and `gap_days` (days from the athlete's last result to the
#'   rating date).
#' @param min_n Buckets with fewer rows get offset 0.
#' @return A data.table `gap`, `n`, `offset`.
#' @export
fit_gap_offsets <- function(scored, min_n = 500L) {
  s <- data.table::as.data.table(scored)
  miss <- setdiff(c("perf", "ability", "gap_days"), names(s))
  if (length(miss)) cli::cli_abort("{.arg scored} lacks {.field {miss}}.")
  s <- s[!is.na(perf) & !is.na(ability) & !is.na(gap_days)]
  if (!nrow(s)) cli::cli_abort("No scoreable rows.")
  s[, gap := .gap_bucket(gap_days)]
  off <- s[, .(n = .N, offset = mean(perf - ability)), by = gap]
  base <- off[gap == "<60d", offset]
  if (length(base) != 1L) cli::cli_abort("No {.val <60d} rows to anchor the offsets on.")
  off[, offset := offset - base]
  off[n < min_n, offset := 0]
  off[, gap := as.character(gap)]
  off[order(match(gap, levels(.gap_bucket(0))))]
}

#' Add time-since-last-result offsets to an ability table
#'
#' @param ability A table from [estimate_ability()] with `ability` and
#'   `last_date`.
#' @param as_of The rating date.
#' @param offsets A table from [fit_gap_offsets()] (`gap`, `offset`).
#' @return `ability` (a copy) with `gap_days`, `gap` and `gap_offset` added and
#'   `ability` shifted by `gap_offset`.
#' @export
apply_gap_offsets <- function(ability, as_of, offsets) {
  miss <- setdiff(c("ability", "last_date"), names(ability))
  if (length(miss)) cli::cli_abort("{.arg ability} lacks {.field {miss}}.")
  off <- data.table::as.data.table(offsets)
  if (!all(c("gap", "offset") %in% names(off))) cli::cli_abort("{.arg offsets} needs {.field gap} and {.field offset}.")
  bad <- setdiff(off$gap, levels(.gap_bucket(0)))
  if (length(bad)) cli::cli_abort("Unknown gap bucket{?s} in {.arg offsets}: {.val {bad}}.")
  ab <- data.table::copy(data.table::as.data.table(ability))
  ab[, gap_days := as.numeric(as.Date(as_of) - as.Date(last_date))]
  ab[, gap := as.character(.gap_bucket(gap_days))]
  ab[off, gap_offset := i.offset, on = "gap"]
  if (!"gap_offset" %in% names(ab)) ab[, gap_offset := NA_real_]
  ab[is.na(gap_offset), gap_offset := 0]
  ab[, ability := ability + gap_offset]
  cli::cli_alert_info("gap_offsets: shifted {ab[gap_offset != 0, .N]} of {nrow(ab)} ability rows.")
  ab[]
}

#' Blend recent form with the athlete's own long record (version E)
#'
#' The thin-record rating that passed the championship backtest (2026-11-10; 719 M1
#' meets scored on R1 races, 2025+ marks error 1.480% -> 1.413%). For each row:
#' \deqn{rating = a R + (1 - a) L, \quad a = w_R / (w_R + m_R \kappa_R)}
#' where `R` is the athlete's recent level (an [estimate_ability()] refit with every
#' half-life multiplied, the measured version used x2), `w_R` its total result
#' weight and `kappa_R` its fitted kappa, and `L` the own long record (a refit with
#' a long half-life, measured at 365 days). There is NO pull towards the event mean:
#' tuned on R1 racers it came out at zero. Availability (has the athlete raced at
#' all lately) is a separate status, not part of this number.
#'
#' Rows where the blend applies get `shrinkage = 0`, because none of the rating is
#' the event mean any more; [condition_prior()] would otherwise add a field shift
#' to them that the tuning never saw. Rows without a finite blend keep their row.
#'
#' @param ability A table from [estimate_ability()] (needs `athlete_id`, `event_id`,
#'   `ability`, `shrinkage`).
#' @param recent,long Tables from [estimate_ability()] fitted on the same history and
#'   `as_of` with the recent and long half-lives; `recent` needs `ability_raw`,
#'   `w_total`, `shrinkage`; `long` needs `ability_raw`.
#' @param kappa_mult Multiplier on the recent fit's kappa (`m_R`); measured 0.5.
#' @return `ability` (a copy, same rows and order) with `ability` replaced and
#'   `shrinkage` set to 0 where applied, plus `blend_a`, `blend_wR` (the recent
#'   refit's total result weight, which the ratings page's uncertainty model reads)
#'   and logical `blend_applied`.
#' @export
apply_record_blend <- function(ability, recent, long, kappa_mult = 0.5) {
  need <- c("athlete_id", "event_id", "ability", "shrinkage")
  miss <- setdiff(need, names(ability))
  if (length(miss)) cli::cli_abort("{.arg ability} lacks {.field {miss}}.")
  miss <- setdiff(c("athlete_id", "event_id", "ability_raw", "w_total", "shrinkage"), names(recent))
  if (length(miss)) cli::cli_abort("{.arg recent} lacks {.field {miss}}.")
  miss <- setdiff(c("athlete_id", "event_id", "ability_raw"), names(long))
  if (length(miss)) cli::cli_abort("{.arg long} lacks {.field {miss}}.")
  if (!is.numeric(kappa_mult) || length(kappa_mult) != 1L || !is.finite(kappa_mult) || kappa_mult <= 0)
    cli::cli_abort("{.arg kappa_mult} must be one positive number.")
  ab <- data.table::copy(data.table::as.data.table(ability))
  key_type <- class(ab$athlete_id)
  ab[, athlete_id := as.character(athlete_id)]
  rr <- unique(data.table::as.data.table(recent)[, .(athlete_id = as.character(athlete_id), event_id,
    .R = ability_raw, .wR = w_total, .kR = shrinkage * w_total / (1 - shrinkage))], by = c("athlete_id", "event_id"))
  ll <- unique(data.table::as.data.table(long)[, .(athlete_id = as.character(athlete_id), event_id, .L = ability_raw)],
               by = c("athlete_id", "event_id"))
  ab[rr, c(".R", ".wR", ".kR") := .(i..R, i..wR, i..kR), on = c("athlete_id", "event_id")]
  ab[ll, .L := i..L, on = c("athlete_id", "event_id")]
  for (v in c(".R", ".wR", ".kR", ".L")) if (!v %in% names(ab)) ab[, (v) := NA_real_]
  ab[, blend_a := .wR / (.wR + kappa_mult * .kR)]
  ab[, blend_wR := .wR]
  ab[, .new := blend_a * .R + (1 - blend_a) * .L]
  ab[, blend_applied := is.finite(.new)]
  ab[blend_applied == TRUE, `:=`(ability = .new, shrinkage = 0)]
  ab[, c(".R", ".wR", ".kR", ".L", ".new") := NULL]
  cli::cli_alert_info("record_blend: applied to {ab[, sum(blend_applied)]} of {nrow(ab)} ability rows (kappa x{kappa_mult}).")
  if (identical(key_type, "integer")) ab[, athlete_id := as.integer(athlete_id)]
  ab[]
}
