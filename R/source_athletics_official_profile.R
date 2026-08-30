#' Fetch an athlete's OFFICIAL biographical data from worldathletics.org
#'
#' [athletics_athlete_profile()] reads the community wrapper's
#' (`worldathletics.nimarion.de`) `/athletes/{id}` route. This is a
#' different, first-party source: `worldathletics.org/athletes/-/-{id}`
#' (the country/name-slug segments in the URL are cosmetic -- confirmed
#' 2026-08-30 that `-/-` in their place resolves identically to the real
#' slug) embeds a Next.js `__NEXT_DATA__` payload with the athlete's
#' registered name split into `familyName`/`givenName`, exactly as World
#' Athletics itself records it.
#'
#' Built to close a real bug: a name-string sanity check this session
#' searched literally for `"Mondo Duplantis"` and `"Sydney McLaughlin"` and
#' found neither, because those are not the athletes' registered names --
#' the corpus (and this endpoint) has `"Armand Duplantis"` (Mondo is a
#' nickname) and `"Sydney Mclaughlin"` (lowercase second syllable). Looking
#' athletes up by `athlete_id` through this function sidesteps name-string
#' guessing entirely.
#'
#' @param athlete_id Integer World Athletics athlete id (the same
#'   `athlete_id`/`aaId` used throughout this package and the corpus).
#' @return A one-row `data.table` with `athlete_id`, `family_name`,
#'   `given_name`, `country_code`, `country_name`, `birthdate`, `sex`
#'   (`"M"`/`"W"`, from the `male` boolean), `iaaf_id` (a distinct secondary
#'   id some older records key on). Zero rows if the athlete id does not
#'   resolve -- but check attribute `"fetch_ok"` before treating a zero-row
#'   result as "this id doesn't exist": `fetch_ok = TRUE` means a
#'   well-formed page confirmed the id is unresolved; `fetch_ok = FALSE`
#'   means the request itself couldn't be confirmed at all (a missing/
#'   unparseable `__NEXT_DATA__` node, most often a WAF/bot-detection
#'   interstitial returning HTTP 200 with no real payload) -- the two used
#'   to return an identical shape, which is exactly wrong for a function
#'   whose caller is realistically a batch resolving many ids in a loop: a
#'   mid-run block would have silently read every subsequent athlete as
#'   "does not exist." Found in review 2026-08-30.
#' @examples
#' \dontrun{
#' athletics_athlete_official_profile(14679502)
#' }
#' @export
athletics_athlete_official_profile <- function(athlete_id) {
  athlete_id <- as.integer(athlete_id)
  url <- paste0("https://worldathletics.org/athletes/-/-", athlete_id)

  doc <- citius_get_html(url)
  # citius_get_html() already distinguishes a definitive 404 (returns NULL)
  # from a loud failure (aborts) -- see its own docs. So NULL here is a
  # CONFIRMED "id not found", not an ambiguous one; the genuinely ambiguous
  # case is below, where HTML came back but its payload didn't (the WAF/
  # interstitial signature: HTTP 200 with no real __NEXT_DATA__). Corrected
  # in review 2026-08-30 after this exact confusion caused a test failure --
  # athlete_id = 1 returns a real 404 here, not the all-NULL basicData shell
  # an earlier probe happened to observe for a different unresolved id.
  if (is.null(doc)) return(.empty_official_profile_dt(fetch_ok = TRUE))
  node <- xml2::xml_find_first(doc, "//script[@id='__NEXT_DATA__']")
  if (inherits(node, "xml_missing")) {
    cli::cli_warn(c(
      "No {.val __NEXT_DATA__} script found at {.url {url}}.",
      i = "Page structure changed, or the request was blocked (WAF/interstitial). Treating as UNCONFIRMED, not \"athlete not found\" -- check attr(result, \"fetch_ok\")."
    ))
    return(.empty_official_profile_dt(fetch_ok = FALSE))
  }

  j <- tryCatch(
    jsonlite::fromJSON(xml2::xml_text(node), simplifyVector = FALSE),
    error = function(e) {
      cli::cli_warn(c(
        "Could not parse {.val __NEXT_DATA__} JSON at {.url {url}}: {conditionMessage(e)}",
        i = "Likely a truncated response. Treating as UNCONFIRMED, not \"athlete not found\" -- check attr(result, \"fetch_ok\")."
      ))
      NULL
    }
  )
  if (is.null(j)) return(.empty_official_profile_dt(fetch_ok = FALSE))

  bd <- j$props$pageProps$competitor$basicData
  # An athlete id that doesn't resolve renders the page shell with every
  # basicData field NULL rather than a 404 -- confirmed 2026-08-30 (a
  # bare-numeric-path probe returned exactly this shape). familyName is
  # NULL both when the id is unknown and, in principle, for a genuinely
  # nameless record, but the former is what this package will actually hit;
  # treat NULL as a CONFIRMED "not found" (fetch_ok = TRUE, the page
  # rendered fine and said so) rather than returning an all-NA row that
  # looks like a hit.
  if (is.null(bd) || is.null(bd$familyName)) return(.empty_official_profile_dt(fetch_ok = TRUE))

  # World Athletics dates render as "10 NOV 1999" on this endpoint, not
  # ISO -- as_date_safe() does not know this format, so parse it here.
  bday <- tryCatch(as.Date(bd$birthDate, format = "%d %b %Y"), error = function(e) NA)

  out <- data.table::data.table(
    athlete_id   = as.integer(bd$aaId %||% athlete_id),
    family_name  = bd$familyName %||% NA_character_,
    given_name   = bd$givenName %||% NA_character_,
    country_code = bd$countryCode %||% NA_character_,
    country_name = bd$countryFullName %||% NA_character_,
    birthdate    = bday,
    sex          = data.table::fifelse(is.null(bd$male), NA_character_,
                                        data.table::fifelse(isTRUE(bd$male), "M", "W")),
    iaaf_id      = as.integer(bd$iaafId %||% NA_integer_)
  )
  data.table::setattr(out, "fetch_ok", TRUE)
  out[]
}

.empty_official_profile_dt <- function(fetch_ok = TRUE) {
  out <- data.table::data.table(
    athlete_id = integer(), family_name = character(), given_name = character(),
    country_code = character(), country_name = character(),
    birthdate = as.Date(character()), sex = character(), iaaf_id = integer()
  )
  data.table::setattr(out, "fetch_ok", fetch_ok)
  out
}
