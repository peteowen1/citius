#' Measure how much one event's ability says about another's
#'
#' Fits, per (target, neighbour) pair, the three quantities
#' [combine_neighbour_ability()] needs: the fixed offset between the two events
#' on the oriented log scale, how strongly they correlate, and how far athletes
#' sit off that conversion. Nothing is hand-set.
#'
#' Fitted on the ELITE subset of the target event, because that is the
#' population a championship final is drawn from and the relationship is not the
#' same lower down: measured 2026-09-14 on men's 1500m -> 5000m, the OLS slope
#' runs 0.799 across all 10,629 men with both ratings and 0.196 across the top
#' decile. Using the all-athlete relationship on an elite field would transfer
#' far too much.
#'
#' **`tau2` deliberately does not subtract estimation noise.** The textbook form
#' would be `var(ability) - mean(ability_se^2)`, but `ability_se` is as large as
#' the entire between-athlete spread at the elite tail, so that subtraction
#' returns ~0 and every neighbour then counts as a perfect proxy -- measured, it
#' let a four-year-old 800m rating take 28% of Ingebrigtsen's 5000m weight. The
#' observed spread is conservative (tau too large, neighbours under-weighted)
#' and never degenerate, which is the right way round for a term that decides
#' how much a different event gets to speak.
#'
#' @param ability An [estimate_ability()] table spanning target and neighbour
#'   events, for the whole rated population rather than one race's entrants --
#'   the offsets and correlations are population quantities.
#' @param target_events Events to fit links INTO.
#' @param neighbour_events Candidate events to fit links FROM. Defaults to every
#'   other event present in `ability`; pairs that fail `min_n` are dropped, so an
#'   unrelated event costs nothing but is not silently kept either.
#' @param elite_quantile Quantile of target `ability_raw` defining "elite".
#' @param min_n Minimum athletes with both ratings for a pair to be fitted.
#' @return A `data.table` with `event_id`, `neighbour_event_id`, `n`, `delta`,
#'   `r`, `sigma2_target` and `tau2`, suitable as `links` for
#'   [combine_neighbour_ability()].
#' @seealso [combine_neighbour_ability()]
#' @export
fit_neighbour_links <- function(ability, target_events,
                                neighbour_events = NULL,
                                elite_quantile = 0.90, min_n = 30L) {
  ab <- data.table::as.data.table(ability)
  req <- c("athlete_id", "event_id", "ability_raw")
  if (!all(req %in% names(ab))) {
    cli::cli_abort("{.arg ability} must come from {.fn estimate_ability}.")
  }
  ab <- ab[is.finite(ability_raw)]
  if (is.null(neighbour_events)) neighbour_events <- unique(ab$event_id)

  out <- lapply(target_events, function(tv) {
    tgt <- ab[event_id == tv]
    if (nrow(tgt) < min_n) return(NULL)
    te <- tgt[ability_raw >= stats::quantile(ability_raw, elite_quantile)]
    if (nrow(te) < min_n) return(NULL)
    sig2 <- stats::var(te$ability_raw)
    if (!is.finite(sig2) || sig2 <= 0) return(NULL)
    rows <- lapply(setdiff(neighbour_events, tv), function(nv) {
      b <- merge(te[, .(athlete_id, y = ability_raw)],
                 ab[event_id == nv, .(athlete_id, x = ability_raw)],
                 by = "athlete_id")
      if (nrow(b) < min_n || stats::var(b$x) <= 0) return(NULL)
      r <- stats::cor(b$x, b$y)
      if (!is.finite(r)) return(NULL)
      data.table::data.table(
        event_id = tv, neighbour_event_id = nv, n = nrow(b),
        delta = mean(b$y - b$x), r = r, sigma2_target = sig2)
    })
    data.table::rbindlist(rows)
  })
  links <- data.table::rbindlist(out)
  if (!nrow(links)) return(links)
  # A negative correlation between two events is not a usable transfer, it is a
  # sign the pair is noise; clamp at 0 so such a pair contributes tau2 =
  # sigma2_target and earns essentially no weight, rather than transferring
  # backwards.
  links[, r := pmax(r, 0)]
  links[, tau2 := (1 - r^2) * sigma2_target]
  links[]
}

#' Combine an athlete's neighbouring-event ratings into their own
#'
#' [estimate_ability()] groups everything `by = .(athlete_id, event_id)`, so a
#' man's 5000m rating never sees his 1500m marks. Ingebrigtsen won the 2026
#' Budapest 5000m in 12:59.24 rated last of twelve, predicted 13:14, because his
#' only 5000m races were tactical championship finals while three fast Diamond
#' League 1500ms in the previous month were invisible to that card.
#'
#' The athlete's own rating stays the baseline. Every neighbour event they are
#' rated in also speaks, weighted by two things and nothing else:
#'
#' 1. **how much we know that rating** -- its evidence variance `sigma^2 /
#'    w_total`
#' 2. **how much that event says about this one** -- `tau2` from
#'    [fit_neighbour_links()], which is `(1 - r^2)` of the target's own spread
#'
#' combined as one inverse-variance weight, `1 / (sigma^2/w_total + tau2)`. An
#' event we know little about is down-weighted; an event that says nothing about
#' the target (`r -> 0`) has `tau2` approaching the target's whole spread and
#' earns almost no weight, so an unrelated pair costs nothing and needs no
#' hand-maintained list of which events may talk to which.
#'
#' **Confidence is `w_total / sigma^2`, NOT `1 / ability_se^2`, and the
#' difference is not cosmetic.** `ability_se` is `sigma / sqrt(w_total + kappa)`
#' and is bounded by each event's own `sigma_between`, so it is not comparable
#' across events at all; worse, `temper_unevidenced()` overwrites it with the
#' event median for anyone under `w_total = 0.05`, which is correct for widening
#' that athlete's own simulated spread and actively wrong for deciding how far
#' to trust one rating against another. Measured on Ingebrigtsen 2026-09-14: an
#' `ability_se` weighting gave his 2022 800m (w_total 0.0128) **29%** of his
#' weight against his own 5000m's 23%; the evidence weighting gives it **1%**.
#'
#' Only `ability_raw` and `ability` move. `shrinkage`, `sigma` and `w_total` are
#' left alone: this adds evidence about where the athlete's ability sits, it
#' does not re-argue how confident the estimator should have been.
#'
#' @param ability An [estimate_ability()] table. Must contain the neighbour
#'   events' rows for these athletes, not only the target event's.
#' @param links Output of [fit_neighbour_links()]. `NULL` is a true no-op.
#' @return `ability` with `ability_raw` and `ability` updated for rows that had
#'   at least one usable neighbour; every other row untouched.
#' @seealso [fit_neighbour_links()], [estimate_ability()]
#' @export
combine_neighbour_ability <- function(ability, links = NULL) {
  ab <- data.table::copy(data.table::as.data.table(ability))
  if (is.null(links) || !nrow(data.table::as.data.table(links)) || !nrow(ab)) return(ab[])
  lk <- data.table::as.data.table(links)
  req <- c("athlete_id", "event_id", "ability_raw", "sigma", "w_total",
           "shrinkage", "prior_mu")
  missing_cols <- setdiff(req, names(ab))
  if (length(missing_cols)) {
    cli::cli_abort(c("{.arg ability} must come from {.fn estimate_ability}.",
                     "x" = "Missing column{?s}: {.field {missing_cols}}."))
  }
  req_lk <- c("event_id", "neighbour_event_id", "delta", "tau2")
  if (!all(req_lk %in% names(lk))) {
    cli::cli_abort("{.arg links} must have columns {.field {req_lk}}.")
  }

  # Evidence variance. w_total <= 0 means no usable evidence, which must become
  # an infinite variance (zero weight) rather than a division by zero -- an Inf
  # here silently became NaN in an earlier draft and dropped the athlete's own
  # rating out of their own combination.
  evar <- function(sigma, w) {
    v <- sigma^2 / w
    v[!is.finite(v) | w <= 0 | sigma <= 0] <- Inf
    v
  }

  own <- ab[, .(athlete_id, event_id, ability_raw, sigma, w_total)]
  own[, own_var := evar(sigma, w_total)]

  # ONE LINK AT A TIME, not one cartesian join.
  #
  # `merge(links, own, by = "event_id", allow.cartesian = TRUE)` materialises
  # (rows per target event) x (links into it) before anything is filtered, so on
  # a full-population table it peaks at several times the input for a result
  # that is then reduced to two columns per athlete. Looping the links -- there
  # are a couple of dozen, not thousands -- keeps peak memory at one link's
  # worth and gives the same answer, because the combination is a plain sum over
  # links. Measured 2026-09-14: identical output, peak intermediate rows down
  # from 4x the table to 1x.
  nb_all <- own[, .(athlete_id, neighbour_event_id = event_id,
                    nb_raw = ability_raw, nb_var = own_var)]
  data.table::setkey(nb_all, neighbour_event_id, athlete_id)
  acc <- vector("list", nrow(lk))
  for (i in seq_len(nrow(lk))) {
    tv <- lk$event_id[i]; nv <- lk$neighbour_event_id[i]
    tgt_ids <- own[event_id == tv, .(athlete_id)]
    if (!nrow(tgt_ids)) next
    part <- nb_all[.(nv, tgt_ids$athlete_id), nomatch = 0L]
    if (!nrow(part)) next
    part <- part[is.finite(nb_raw)]
    if (!nrow(part)) next
    w <- 1 / (part$nb_var + lk$tau2[i])
    keep <- is.finite(w) & w > 0
    if (!any(keep)) next
    acc[[i]] <- data.table::data.table(
      athlete_id = part$athlete_id[keep], event_id = tv,
      nb_w = w[keep], nb_wv = w[keep] * (part$nb_raw[keep] + lk$delta[i]))
  }
  e <- data.table::rbindlist(acc)
  rm(acc, nb_all)
  if (!nrow(e)) return(ab[])

  agg <- e[, .(nb_sw = sum(nb_w), nb_swv = sum(nb_wv)),
           by = .(athlete_id, event_id)]
  rm(e)

  ab <- merge(ab, agg, by = c("athlete_id", "event_id"), all.x = TRUE)
  ab[, own_w := 1 / evar(sigma, w_total)]
  hit <- !is.na(ab$nb_sw) & is.finite(ab$own_w) & (ab$own_w + ab$nb_sw) > 0
  ab[hit, ability_raw := (own_w * ability_raw + nb_swv) / (own_w + nb_sw)]
  ab[hit, ability := (1 - shrinkage) * ability_raw + shrinkage * prior_mu]
  ab[, c("nb_sw", "nb_swv", "own_w") := NULL]
  ab[]
}
