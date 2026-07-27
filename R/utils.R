#' @keywords internal
#' @noRd
copy_dt <- function(x) data.table::copy(x)

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

  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

#' @keywords internal
#' @noRd
as_date_safe <- function(x) {
  x <- as.character(x)
  suppressWarnings(as.Date(substr(x, 1, 10)))
}
