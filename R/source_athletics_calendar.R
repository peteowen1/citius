#' Fetch one page of World Athletics' public event calendar
#'
#' `athletics_find_competition()` and `harvest_missing_majors.R`'s discovery
#' list are built by KEYWORD SEARCH SWEEPS against the wrapper's `/athletes`
#' route family -- they can only find a competition someone already thought
#' to search a name for, and on 2026-08-30 that method missed two current
#' Diamond League legs (Xiamen, Shanghai) that a manual browser check found
#' by accident. This is a different, better source for discovery specifically
#' (not results): `worldathletics.org/competition/calendar-results` is a
#' Next.js page whose server-fetched, GraphQL-backed data is embedded
#' directly in the response HTML as `window.__NEXT_DATA__` -- a genuine
#' date-range-queryable calendar, 39,290 competitions as measured
#' 2026-08-30, spanning recorded history through scheduled future events
#' (the 2032 Brisbane Olympics is already listed). Confirmed via direct
#' `httr2` GET, no browser/JS execution needed -- the data is present on
#' the plain server-rendered response.
#'
#' Competition `id` values in the response were confirmed (2026-08-30,
#' cross-checked against Glasgow 2026 and three Diamond League legs already
#' in `championship_results.rds`) to be the SAME id namespace this package's
#' `athletics_competition_results()` already takes -- a calendar hit's `id`
#' is usable directly as a `competition_id` to fetch results.
#'
#' Pagination is via the `offset` query parameter alone, in fixed steps of
#' 100 results; a `limit` query parameter was tested (2026-08-30) and is
#' silently ignored by the endpoint, so it is not exposed here. Passing an
#' unsupported/invalid parameter combination (an earlier probe combined
#' `limit` with `offset`) can make the server-side data fetch fail
#' silently, returning a page with `hits = NULL` and zero results rather
#' than an HTTP error -- `athletics_calendar()` treats that shape as "no
#' data" rather than crashing, but callers changing the parameter set
#' should re-verify against a known query before trusting a zero-hit page.
#'
#' @param start_date,end_date `Date` or `"YYYY-MM-DD"` string, inclusive.
#'   `NULL` for no bound on that side. **The endpoint's own filter is not
#'   strictly enforced** -- confirmed 2026-08-30, a 2015-only query returned
#'   at least one 2007 result. It does most of the work; a caller needing a
#'   hard boundary must post-filter `start_date` on the returned table.
#' @param query Free-text competition name search (substring/fuzzy, as the
#'   site's own search box does), or `NULL`.
#' @param offset Row offset for pagination, in steps of 100. `NULL`/`0` for
#'   the first page.
#' @return A `data.table` with columns `competition_id`, `name`, `venue`,
#'   `area`, `ranking_category`, `disciplines`, `competition_group`,
#'   `competition_subgroup`, `start_date`, `has_results`, `has_api_results`,
#'   `has_startlist`, and two attributes: `"hits"` gives the total match
#'   count for the query (not just this page) -- read it with
#'   `attr(result, "hits")` -- and `"fetch_ok"` distinguishes a CONFIRMED
#'   zero-hit query (`TRUE`, well-formed response, genuinely nothing
#'   matched) from a request that could not be confirmed at all (`FALSE`):
#'   a missing/unparseable `__NEXT_DATA__` script tag, most often because a
#'   WAF/bot-detection interstitial returned HTTP 200 with no real payload,
#'   or a truncated/malformed response body. Both failure shapes return the
#'   *same* zero-row table shape as a genuine zero-hit result -- silently
#'   treating them as equivalent was the actual defect found in review
#'   2026-08-30, and this attribute exists to prevent it. Always check
#'   `fetch_ok` before trusting a zero-row result, and see
#'   [athletics_calendar_all()]'s `"complete"` attribute for the paginated
#'   equivalent.
#' @examples
#' \dontrun{
#' athletics_calendar(start_date = "2026-01-01", end_date = "2026-12-31")
#' }
#' @export
athletics_calendar <- function(start_date = NULL, end_date = NULL, query = NULL,
                                offset = NULL) {
  qs <- list(
    startDate = if (!is.null(start_date)) format(as.Date(start_date), "%Y-%m-%d"),
    endDate   = if (!is.null(end_date))   format(as.Date(end_date), "%Y-%m-%d"),
    query     = query,
    offset    = if (!is.null(offset)) as.integer(offset)
  )
  qs <- qs[!vapply(qs, is.null, logical(1))]
  url <- paste0(
    "https://worldathletics.org/competition/calendar-results",
    if (length(qs)) paste0("?", paste(names(qs), utils::URLencode(vapply(qs, as.character, character(1)), reserved = TRUE), sep = "=", collapse = "&")) else ""
  )

  doc <- citius_get_html(url)
  # citius_get_html() already distinguishes a definitive 404 (returns NULL)
  # from a loud failure (aborts) -- see its own docs. So NULL here is a
  # CONFIRMED absence, not an ambiguous one; the genuinely ambiguous case is
  # below, where HTML came back but its payload didn't (the WAF/interstitial
  # signature: HTTP 200 with no real __NEXT_DATA__). Corrected in review
  # 2026-08-30 after this exact confusion caused a test failure.
  if (is.null(doc)) return(.empty_calendar_dt(fetch_ok = TRUE))
  node <- xml2::xml_find_first(doc, "//script[@id='__NEXT_DATA__']")
  if (inherits(node, "xml_missing")) {
    cli::cli_warn(c(
      "No {.val __NEXT_DATA__} script found at {.url {url}}.",
      i = "Page structure changed, or the request was blocked (WAF/interstitial). Treating as UNCONFIRMED, not a real zero-hit result -- check attr(result, \"fetch_ok\")."
    ))
    return(.empty_calendar_dt(fetch_ok = FALSE, reason = "missing __NEXT_DATA__ node"))
  }

  j <- tryCatch(
    jsonlite::fromJSON(xml2::xml_text(node), simplifyVector = FALSE),
    error = function(e) {
      cli::cli_warn(c(
        "Could not parse {.val __NEXT_DATA__} JSON at {.url {url}}: {conditionMessage(e)}",
        i = "Likely a truncated response. Treating as UNCONFIRMED, not a real zero-hit result -- check attr(result, \"fetch_ok\")."
      ))
      NULL
    }
  )
  if (is.null(j)) return(.empty_calendar_dt(fetch_ok = FALSE, reason = "unparseable JSON"))

  ie <- j$props$pageProps$initialEvents
  if (is.null(ie) || is.null(ie$results) || !length(ie$results)) {
    # A confirmed, well-formed response that legitimately has no results --
    # distinct from every path above, which never got a real payload at all.
    out <- .empty_calendar_dt(fetch_ok = TRUE)
    data.table::setattr(out, "hits", as.integer(ie$hits %||% 0L))
    return(out[])
  }

  dt <- data.table::rbindlist(lapply(ie$results, function(r) data.table::data.table(
    competition_id        = as.integer(r$id %||% NA_integer_),
    name                  = r$name %||% NA_character_,
    venue                 = r$venue %||% NA_character_,
    area                  = r$area %||% NA_character_,
    ranking_category      = r$rankingCategory %||% NA_character_,
    disciplines           = r$disciplines %||% NA_character_,
    competition_group     = r$competitionGroup %||% NA_character_,
    competition_subgroup  = r$competitionSubgroup %||% NA_character_,
    start_date            = as_date_safe(r$startDate %||% NA),
    has_results           = isTRUE(r$hasResults),
    has_api_results       = isTRUE(r$hasApiResults),
    has_startlist         = isTRUE(r$hasStartlist)
  )), use.names = TRUE, fill = TRUE)

  data.table::setattr(dt, "hits", as.integer(ie$hits %||% nrow(dt)))
  data.table::setattr(dt, "fetch_ok", TRUE)
  dt[]
}

.empty_calendar_dt <- function(fetch_ok = TRUE, reason = NA_character_) {
  out <- data.table::data.table(
    competition_id = integer(), name = character(), venue = character(),
    area = character(), ranking_category = character(), disciplines = character(),
    competition_group = character(), competition_subgroup = character(),
    start_date = as.Date(character()), has_results = logical(),
    has_api_results = logical(), has_startlist = logical()
  )
  data.table::setattr(out, "hits", 0L)
  data.table::setattr(out, "fetch_ok", fetch_ok)
  if (!fetch_ok) data.table::setattr(out, "fetch_fail_reason", reason)
  out
}

#' Fetch the FULL World Athletics calendar for a query, paging automatically
#'
#' Loops [athletics_calendar()] over `offset` (steps of 100, the endpoint's
#' fixed page size) until every matching row has been fetched. For the
#' unfiltered world query that is ~393 requests (39,290 hits / 100) --
#' expect this to take real time, and narrow with `start_date`/`end_date`
#' when you don't actually need the whole history in one call.
#'
#' @inheritParams athletics_calendar
#' @param max_pages Safety cap on the number of pages fetched, in case
#'   `hits` is corrupt or the loop otherwise fails to terminate. Defaults to
#'   500 (50,000 rows), comfortably above the 2026-08-30 measured total.
#' @param throttle Seconds to sleep between page requests, additional to
#'   [citius_get_html()]'s own per-host throttle -- this is a real public
#'   website, not a bulk data endpoint, and pulling ~400 pages back-to-back
#'   with no pacing is not a reasonable way to use it.
#' @return A single combined `data.table`, same columns as
#'   [athletics_calendar()]. Attribute `"hits"` gives the total the server
#'   reported. Attribute `"complete"` is `TRUE` only if every page fetched
#'   cleanly (`fetch_ok`) AND the final row count matches `hits` (after
#'   dropping unresolvable-id rows, see below) -- **always check this before
#'   trusting the result is the whole calendar**, since a page can fail
#'   mid-run (a transient block, a WAF interstitial) without raising an
#'   error, per [athletics_calendar()]'s `fetch_ok`. On `FALSE`, attribute
#'   `"failed_offsets"` lists which page offsets did not fetch cleanly, and
#'   `"dropped_bad_id"` gives how many rows were dropped for having no
#'   parseable `competition_id` (found in review 2026-08-30: these used to
#'   be silently merged into one `NA`-keyed group by the de-duplication
#'   step, discarding every other genuinely distinct competition an
#'   unparseable id happened to share `NA` with).
#' @examples
#' \dontrun{
#' athletics_calendar_all(start_date = "2026-01-01", end_date = "2026-12-31")
#' }
#' @export
athletics_calendar_all <- function(start_date = NULL, end_date = NULL, query = NULL,
                                    max_pages = 500L, throttle = 1) {
  first <- athletics_calendar(start_date, end_date, query, offset = 0L)
  hits <- attr(first, "hits") %||% 0L
  failed_offsets <- integer()
  if (!isTRUE(attr(first, "fetch_ok"))) failed_offsets <- c(failed_offsets, 0L)
  if (!nrow(first)) {
    data.table::setattr(first, "complete", isTRUE(attr(first, "fetch_ok")) && hits == 0L)
    data.table::setattr(first, "failed_offsets", failed_offsets)
    data.table::setattr(first, "dropped_bad_id", 0L)
    return(first)
  }

  pages <- list(first)
  n_pages <- ceiling(hits / 100)
  if (n_pages > max_pages) {
    cli::cli_warn("{n_pages} pages needed for {hits} hits, capped at {max_pages} ({max_pages * 100} rows).")
    n_pages <- max_pages
  }
  if (n_pages > 1) {
    for (p in seq_len(n_pages - 1)) {
      Sys.sleep(throttle)
      offset <- p * 100L
      # A page fetch can throw (citius_get_html() aborts loudly on a
      # persistent non-404 HTTP failure) -- correct in isolation, but inside
      # a ~400-iteration loop an uncaught error unwinds the stack and
      # discards every page already fetched. Catch it, keep what's already
      # in hand, mark the run incomplete, and stop rather than losing
      # everything. Found in review 2026-08-30.
      pg <- tryCatch(
        athletics_calendar(start_date, end_date, query, offset = offset),
        error = function(e) {
          cli::cli_warn("Page at offset {offset} failed: {conditionMessage(e)}. Stopping pagination with {length(pages)} of {n_pages} pages -- result is INCOMPLETE.")
          NULL
        }
      )
      if (is.null(pg)) { failed_offsets <- c(failed_offsets, offset); break }
      if (!isTRUE(attr(pg, "fetch_ok"))) failed_offsets <- c(failed_offsets, offset)
      pages[[length(pages) + 1]] <- pg
    }
  }

  out <- data.table::rbindlist(pages, use.names = TRUE, fill = TRUE)
  n_bad_id <- sum(is.na(out$competition_id))
  if (n_bad_id) {
    cli::cli_warn("{n_bad_id} row{?s} had no parseable competition_id and {?was/were} dropped, not merged into one group.")
    out <- out[!is.na(competition_id)]
  }
  out <- unique(out, by = "competition_id")
  complete <- length(failed_offsets) == 0L && nrow(out) >= hits
  if (!complete) {
    cli::cli_warn(c(
      "Fetched {nrow(out)} rows but the server reported {hits} hits.",
      i = "{length(failed_offsets)} page{?s} did not fetch cleanly (offsets: {paste(failed_offsets, collapse=', ')}). Result is INCOMPLETE -- check attr(result, \"failed_offsets\") before treating this as the full calendar."
    ))
  }
  data.table::setattr(out, "hits", hits)
  data.table::setattr(out, "complete", complete)
  data.table::setattr(out, "failed_offsets", failed_offsets)
  data.table::setattr(out, "dropped_bad_id", n_bad_id)
  out[]
}
