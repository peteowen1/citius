#' Parse a competition mark into a numeric value
#'
#' Handles the mark formats used across athletics and swimming: clock times
#' (`"3:37.84"`, `"1:44.53"`, `"2:03:21"`), plain seconds (`"9.58"`), field
#' measurements in metres (`"8.95"`) and combined-event points (`"9058"`).
#'
#' Times are returned in seconds; field marks and points are returned as-is.
#' Non-marks (`"DNF"`, `"DQ"`, `"NM"`, `"DNS"`) return `NA_real_`, which is the
#' correct behaviour: a failure to record a mark is not a slow mark, and must
#' not be treated as one when estimating ability.
#'
#' @param mark Character vector of marks.
#' @return Numeric vector. Times in seconds; other marks unchanged.
#' @examples
#' parse_mark(c("9.58", "3:37.84", "2:03:21", "8.95", "DNF"))
#' @export
parse_mark <- function(mark) {
  mark <- trimws(as.character(mark))

  # Strip World Athletics annotation suffixes: "9.58w" (wind-aided), "3:37.84h"
  # (hand-timed), "8.95i" (indoor), trailing "+"/"*"/"A" (altitude).
  clean <- sub("[wWhHiI+*Aa]$", "", mark)
  clean <- gsub("[^0-9:.]", "", clean)

  out <- rep(NA_real_, length(mark))
  ok <- !is.na(mark) & nzchar(clean) & !grepl("^(DNF|DNS|DQ|NM|NH|DNQ)$", mark, ignore.case = TRUE)

  parts <- strsplit(clean[ok], ":", fixed = TRUE)
  vals <- vapply(parts, function(p) {
    p <- suppressWarnings(as.numeric(p))
    if (anyNA(p) || !length(p)) return(NA_real_)
    # Horner over sexagesimal components: s, m:s, or h:m:s
    Reduce(function(acc, x) acc * 60 + x, p)
  }, numeric(1))

  out[ok] <- vals
  out
}


#' Format a numeric time back into clock notation
#'
#' @param seconds Numeric vector of times in seconds.
#' @param digits Decimal places to retain. Default 2.
#' @return Character vector, e.g. `217.84` becomes `"3:37.84"`.
#' @examples
#' format_mark(c(9.58, 217.84, 7401))
#' @export
format_mark <- function(seconds, digits = 2) {
  fmt1 <- function(s) {
    if (is.na(s)) return(NA_character_)
    if (s < 60) return(formatC(s, format = "f", digits = digits))
    mins <- floor(s / 60)
    secs <- s - mins * 60
    width <- if (digits > 0) digits + 3 else 2
    pad <- formatC(secs, format = "f", digits = digits, width = width, flag = "0")
    if (mins < 60) {
      return(sprintf("%d:%s", mins, pad))
    }
    hrs <- floor(mins / 60)
    mins <- mins - hrs * 60
    sprintf("%d:%02d:%s", hrs, mins, pad)
  }
  vapply(seconds, fmt1, character(1))
}


#' Convert marks to the oriented performance scale
#'
#' The single most important abstraction in the package. Athletics and swimming
#' mix events where *lower is better* (all track and swim times) with events
#' where *higher is better* (throws, jumps, combined-event points). Every
#' downstream model — ability estimation, variance, simulation, ranking — is
#' written against a scale where **higher is always better**, so that no
#' function needs to branch on event type.
#'
#' The transform is `perf = orientation * log(mark)`, with `orientation = -1`
#' for time events and `+1` for field events and points.
#'
#' Working in logs (rather than raw units) is deliberate: performance spread is
#' close to multiplicative, so a fixed log-scale standard deviation means the
#' same *percentage* spread across a 9.58s 100m and a 2:01:09 marathon. It also
#' enforces the physical constraint that times and distances are positive.
#'
#' @param mark Numeric vector of parsed marks (see [parse_mark()]).
#' @param orientation Integer vector, `-1` where lower is better, `+1` where
#'   higher is better. Recycled to the length of `mark`.
#' @return Numeric vector on the performance scale; higher is better.
#' @seealso [perf_to_mark()] for the inverse.
#' @examples
#' # A faster 100m yields a higher performance score
#' to_perf(c(9.58, 10.20), orientation = -1)
#' @export
to_perf <- function(mark, orientation) {
  mark <- as.numeric(mark)
  orientation <- rep_len(as.integer(orientation), length(mark))
  if (any(!is.na(orientation) & !orientation %in% c(-1L, 1L))) {
    cli::cli_abort("{.arg orientation} must be -1 (lower is better) or 1 (higher is better).")
  }
  ifelse(is.na(mark) | mark <= 0, NA_real_, orientation * log(mark))
}


#' Convert performance scores back to marks
#'
#' Inverse of [to_perf()].
#'
#' @inheritParams to_perf
#' @param perf Numeric vector on the performance scale.
#' @return Numeric vector of marks in the original units.
#' @export
perf_to_mark <- function(perf, orientation) {
  orientation <- rep_len(as.integer(orientation), length(perf))
  exp(perf * orientation)
}


#' Render a performance score as a display-ready mark and unit
#'
#' Takes an ability (or any score on the performance scale) straight to the
#' string a reader sees: `"9.84"`, `"1:56.34"`, `"2:08:29"`, `"8,813"`.
#'
#' This exists so that the *presentation* of a predicted mark has exactly one
#' implementation. The blog publishes predicted marks in two places — the
#' per-event card for a meet, and the evergreen athlete ratings table — and the
#' same athlete appearing with two different times on two pages is the kind of
#' inconsistency a reader reads as the site being wrong. Formatting here rather
#' than in each export (or, worse, in JavaScript on the page) also keeps the
#' event registry's `orientation` and the seconds/metres/points distinction out
#' of code that has no other reason to know about them.
#'
#' The three branches are unit conventions, not arbitrary: times under a minute
#' are bare seconds, times under an hour are `m:ss.hh`, longer ones drop to
#' whole seconds because no marathon is reported to a hundredth. Combined-event
#' points are thousands-separated; field marks are two decimals of metres.
#'
#' Note that what comes back is a *typical* mark whenever the input is a
#' recency-weighted ability: a championship final is closer to an athlete's best
#' day, so predictions built this way read slightly slow by design. Pages
#' showing them are expected to say so.
#'
#' @param perf Numeric vector on the performance scale (higher is better).
#' @param orientation Integer vector, `-1` where lower is better, `+1` where
#'   higher is better. Recycled to the length of `perf`.
#' @return A `data.frame` with two character columns: `mark` (the formatted
#'   value) and `unit` (`""` for times, `"m"` for field marks, `"pts"` for
#'   combined events). Non-finite inputs give `NA` marks.
#' @seealso [perf_to_mark()], which this wraps.
#' @examples
#' # A 100m ability and a shot put ability, formatted for display
#' predicted_mark(to_perf(c(9.84, 10.20), -1), orientation = -1)
#' predicted_mark(to_perf(c(22.52), 1), orientation = 1)
#' @export
predicted_mark <- function(perf, orientation) {
  value <- perf_to_mark(perf, orientation)
  orientation <- rep_len(as.integer(orientation), length(value))
  # Guard the INPUT, not just the exponentiated output. An infinite ability
  # comes back through exp() as a perfectly finite 0, which would format as a
  # zero-second 100m — a nonsense number that reads as a real one.
  value[!is.finite(perf)] <- NA_real_

  fmt1 <- function(v, o) {
    if (!is.finite(v) || is.na(o)) return(NA_character_)
    if (o < 0) {
      if (v < 60)   return(sprintf("%.2f", v))
      if (v < 3600) return(sprintf("%d:%05.2f", as.integer(v %/% 60), v %% 60))
      return(sprintf("%d:%02d:%02d", as.integer(v %/% 3600),
                     as.integer((v %% 3600) %/% 60), as.integer(round(v %% 60))))
    }
    if (v > 1000) return(format(round(v), big.mark = ","))
    sprintf("%.2f", v)
  }

  mark <- vapply(seq_along(value), function(i) fmt1(value[i], orientation[i]),
                 character(1))
  unit <- ifelse(is.na(orientation), NA_character_,
                 ifelse(orientation < 0, "",
                        ifelse(!is.na(value) & value > 1000, "pts", "m")))
  data.frame(mark = mark, unit = unit, stringsAsFactors = FALSE)
}
