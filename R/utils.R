#' @keywords internal
#' @noRd
copy_dt <- function(x) data.table::copy(x)

#' One independent data.table copy of any table-like input
#'
#' `copy(as.data.table(x))` costs TWO deep copies when `x` is already a
#' data.table, because `as.data.table()` is not the no-op it looks like (see
#' C:/dev/.claude/rules/r-datatable-gotchas.md). This is the single-copy idiom
#' `store.R` documents, extracted so corpus-sized callers stop paying twice.
#' @keywords internal
#' @noRd
.one_copy_dt <- function(x) {
  data.table::copy(if (data.table::is.data.table(x)) x else data.table::as.data.table(x))
}

#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

#' Perform a JSON GET with retry and a descriptive user agent
#'
#' Both upstream feeds are community- or federation-hosted and neither is
#' contractually ours, so requests are rate-limited by default and identify
#' themselves. Transient failures are retried; a 404 is returned as `NULL`
#' rather than retried, because "this athlete has no results" is a legitimate
#' answer and must be distinguishable from "the service is down".
#'
#' @param url Full request URL.
#' @param max_tries Retry budget for transient failures.
#' @param throttle Minimum seconds between requests to the same host.
#' @return Parsed JSON as a list, or `NULL` on a definitive 404.
#' @keywords internal
#' @noRd
citius_get_json <- function(url, max_tries = 4L, throttle = 0.25) {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, "citius R package (https://github.com/peteowen1/citius)")
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_throttle(req, rate = 1 / throttle)
  req <- httr2::req_retry(
    req,
    max_tries = max_tries,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(408, 425, 429, 500, 502, 503, 504)
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  if (status == 404L) return(NULL)
  if (status >= 400L) {
    cli::cli_abort("Request to {.url {url}} failed with status {status}.")
  }

  # A 200 that is not JSON -- a WAF or CDN interstitial, a truncated body --
  # otherwise surfaces as a raw jsonlite parse error pointing nowhere near the
  # cause, and only one of the eight call sites wraps this in tryCatch.
  tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(c(
        "Response from {.url {url}} returned status {status} but is not parseable JSON.",
        i = "Usually a CDN or WAF interstitial page served with a success status.",
        x = conditionMessage(e)
      ))
    }
  )
}

#' Perform an HTML GET with retry and a descriptive user agent
#'
#' The sibling of [citius_get_json()] for feeds that render HTML rather than
#' serving JSON. Retry, throttle and user-agent policy live in one place so a
#' new source cannot quietly adopt a different one.
#'
#' @inheritParams citius_get_json
#' @return A parsed `xml_document`, or `NULL` on a definitive 404.
#' @keywords internal
#' @noRd
citius_get_html <- function(url, max_tries = 4L, throttle = 0.25) {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, "citius R package (https://github.com/peteowen1/citius)")
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_throttle(req, rate = 1 / throttle)
  req <- httr2::req_retry(
    req,
    max_tries = max_tries,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(408, 425, 429, 500, 502, 503, 504)
  )
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status == 404L) return(NULL)
  if (status >= 400L) {
    cli::cli_abort("Request to {.url {url}} failed with status {status}.")
  }
  # Same guard as citius_get_json(): a 200 whose body is not parseable markup
  # must fail with the URL attached, not a bare xml2 error.
  tryCatch(
    xml2::read_html(httr2::resp_body_string(resp)),
    error = function(e) {
      cli::cli_abort(c(
        "Response from {.url {url}} returned status {status} but is not parseable HTML.",
        x = conditionMessage(e)
      ))
    }
  )
}

#' Drop ranked-list rows before estimating any variance
#'
#' Some sources supply an athlete's **best** mark for a period rather than every
#' mark — Swim England's rankings are one row per swimmer per event per season.
#' Those rows are fine for estimating *where* an athlete sits, and useless for
#' estimating how much they vary: a maximum is truncated at the good end by
#' construction, so its spread is not the athlete's spread.
#'
#' Feeding them to a variance estimator understates `sigma_within`, which makes
#' favourites look safer than they are. The swimming backtest was already
#' over-confident at the top — 94% predicted against 72% actual before
#' `ability_se` was added — and this would push it back the same way.
#'
#' Rows are dropped rather than down-weighted, and the drop is announced once,
#' because a silent filter is how a source quietly stops contributing.
#'
#' @param results Results table, possibly carrying an `is_best` flag.
#' @param who Name of the calling function, for the message.
#' @return `results` with any `is_best` rows removed.
#' @keywords internal
#' @noRd
.drop_best_only <- function(results, who) {
  if (!"is_best" %in% names(results)) return(results)
  n_best <- sum(isTRUE_vec(results$is_best))
  if (!n_best) return(results)
  cli::cli_inform(c(
    "!" = "{who}: dropping {format(n_best, big.mark = ',')} ranked-list row{?s} \\
           ({round(100 * n_best / nrow(results))}% of input).",
    "i" = "Ranked lists are truncated at the good end, so they cannot support \\
           variance estimation. They remain available to {.fn estimate_ability}."))
  results[!isTRUE_vec(results$is_best)]
}

#' @keywords internal
#' @noRd
isTRUE_vec <- function(x) !is.na(x) & as.logical(x)

#' @keywords internal
#' @noRd
as_date_safe <- function(x) {
  x <- as.character(x)
  suppressWarnings(as.Date(substr(x, 1, 10)))
}
