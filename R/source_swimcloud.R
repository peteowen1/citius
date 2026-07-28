#' Open a SwimCloud browser session
#'
#' SwimCloud is fronted by a bot-management layer that returns **403 to every
#' non-browser client** — `httr2`, `curl` and headless Chrome alike (headless
#' advertises `HeadlessChrome` in its user agent and is served a challenge page).
#' A real Chrome, driven over the DevTools protocol, is served normally.
#'
#' So this launches an actual Chrome and attaches to it. The browser is *real*;
#' only the driving is automated. No attempt is made to disguise the client —
#' if a future change blocks script-driven browsers too, that is a decision to
#' respect rather than route around.
#'
#' An isolated `--user-data-dir` is used so the session cannot read or disturb
#' the user's own Chrome profile, cookies or logins.
#'
#' @param port DevTools port to listen on.
#' @param wait Seconds to allow the bot-management interstitial to clear. The
#'   first page load is challenged; subsequent requests in the session are not.
#' @return A list with the `chromote` session and a `close()` function. Always
#'   close it — an orphaned Chrome keeps running otherwise.
#' @export
swimcloud_session <- function(port = 9333L, wait = 7) {
  chrome <- Filter(file.exists, c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/usr/bin/google-chrome"))
  if (!length(chrome)) cli::cli_abort("Could not find a Chrome binary.")
  profile <- file.path(tempdir(), sprintf("citius-chrome-%s", port))
  dir.create(profile, showWarnings = FALSE, recursive = TRUE)

  system2(chrome[1], c(sprintf("--remote-debugging-port=%d", port),
                       sprintf('--user-data-dir="%s"', profile),
                       "--no-first-run", "--no-default-browser-check",
                       "about:blank"),
          wait = FALSE, stdout = NULL, stderr = NULL)
  Sys.sleep(4)

  ch <- chromote::Chromote$new(
    browser = chromote::ChromeRemote$new(host = "127.0.0.1", port = port))
  b <- ch$new_session()
  b$Page$navigate(swimcloud_base_url(), wait_ = TRUE)
  Sys.sleep(wait)

  title <- b$Runtime$evaluate("document.title", returnByValue = TRUE)$result$value
  if (isTRUE(grepl("just a moment", title, ignore.case = TRUE))) {
    cli::cli_abort(c("SwimCloud served a bot-verification page and it did not clear.",
                     i = "Try a longer {.arg wait}, or open the site once by hand."))
  }
  list(session = b,
       close = function() try({ b$close(); ch$get_browser()$close() }, silent = TRUE))
}

#' SwimCloud base URL
#' @return Base URL string.
#' @export
swimcloud_base_url <- function() {
  getOption("citius.swimcloud_base", "https://www.swimcloud.com")
}

#' Translate a SwimCloud event name to the registry's discipline
#'
#' SwimCloud writes US-style abbreviations — `"50 Breast Women"`, `"200 IM Men"`
#' — where [citius_events()] uses `"50m Breaststroke"`. Without translation
#' [match_event()] returns `NA` for **every** row, which is the correct
#' behaviour (it refuses to guess) but leaves the whole feed unusable.
#'
#' The mapping is explicit rather than fuzzy, for the same reason `match_event()`
#' is strict: silently snapping an unrecognised event onto a neighbour corrupts
#' athlete histories in a way nothing downstream can detect.
#'
#' @param x Character vector of SwimCloud event names.
#' @return A list with `discipline` and `sex`, both `NA` where unrecognised.
#' @examples
#' swimcloud_event("50 Breast Women")
#' swimcloud_event("200 IM Men")
#' @export
swimcloud_event <- function(x) {
  x <- trimws(as.character(x))
  sex <- data.table::fifelse(grepl("\\bWomen\\b", x, ignore.case = TRUE), "F",
         data.table::fifelse(grepl("\\bMen\\b", x, ignore.case = TRUE), "M", NA_character_))
  # RELAYS ARE NOT INDIVIDUAL EVENTS. "400 Medley Relay" contains "Medley", so
  # without this it mapped onto "400m Individual Medley" and relay times would
  # have entered the corpus as individual swims. Mixed relays escaped only
  # because their sex is neither M nor F; "400 Medley Relay Men" would have
  # matched cleanly and corrupted the event silently.
  #
  # Diving and open-water share the pages but are different sports entirely.
  is_relay <- grepl("relay|\\bmixed\\b", x, ignore.case = TRUE)
  is_other <- grepl("diving|platform|springboard|\\b[0-9]+M Diving\\b|open water",
                    x, ignore.case = TRUE)
  body <- trimws(gsub("\\b(Men|Women)\\b", "", x, ignore.case = TRUE))
  dist <- sub("^([0-9]+).*$", "\\1", body)
  dist[!grepl("^[0-9]+$", dist)] <- NA_character_

  stroke_map <- c(free = "Freestyle", back = "Backstroke", breast = "Breaststroke",
                  fly = "Butterfly", im = "Individual Medley",
                  medley = "Individual Medley")
  key <- tolower(gsub("^[0-9]+\\s*", "", body))
  key <- trimws(gsub("[^a-z ]", "", key))
  # Take the first recognised stroke word; anything else stays NA.
  stroke <- vapply(strsplit(key, "\\s+"), function(p) {
    hit <- p[p %in% names(stroke_map)]
    if (length(hit)) stroke_map[[hit[1]]] else NA_character_
  }, character(1))

  disc <- ifelse(is.na(dist) | is.na(stroke) | is_relay | is_other, NA_character_,
                 paste0(dist, "m ", stroke))
  # Sex is dropped along with the discipline: a relay row carrying a valid sex
  # but no discipline invites a later join from putting it back together wrongly.
  sex[is_relay | is_other] <- NA_character_
  list(discipline = disc, sex = sex)
}

#' Fetch SwimCloud pages through an open browser session
#'
#' Pages are retrieved with an in-page `fetch()` rather than by navigating.
#' Results are server-rendered, so the fetched HTML is complete, and it skips
#' the render, images, fonts and scripts a navigation would pay for — measured
#' at 97ms per page against seconds for a navigation.
#'
#' **Concurrency is capped deliberately.** Measured: 6 concurrent requests
#' return 14/14 pages; **10 concurrent returns HTTP 429 and silently loses 29%
#' of the bytes** while looking like the fastest setting. This function
#' therefore verifies every response and errors rather than returning a short
#' result — a harvest that quietly drops pages is worse than one that stops.
#'
#' @param sess A session from [swimcloud_session()].
#' @param paths Character vector of site-relative paths.
#' @param concurrency Simultaneous requests. Do not raise above 6 without
#'   re-measuring the 429 threshold.
#' @param retries Attempts for pages that come back non-200, with backoff.
#' @return Character vector of HTML, same length and order as `paths`.
#' @export
swimcloud_fetch <- function(sess, paths, concurrency = 6L, retries = 3L) {
  if (!length(paths)) return(character(0))
  out <- rep(NA_character_, length(paths))
  pending <- seq_along(paths)

  for (attempt in seq_len(retries)) {
    js <- sprintf('(async () => {
      const urls = %s;
      const conc = %d;
      const res = [];
      for (let i = 0; i < urls.length; i += conc) {
        const batch = urls.slice(i, i + conc);
        const got = await Promise.all(batch.map(u =>
          fetch(u, {credentials: "same-origin"})
            .then(async r => ({status: r.status, body: await r.text()}))
            .catch(e => ({status: 0, body: ""}))));
        got.forEach(g => res.push(g));
      }
      return JSON.stringify(res);
    })()', jsonlite::toJSON(paths[pending]), as.integer(concurrency))

    raw <- sess$session$Runtime$evaluate(js, awaitPromise = TRUE,
                                         returnByValue = TRUE)$result$value
    got <- jsonlite::fromJSON(raw, simplifyDataFrame = FALSE)
    ok <- vapply(got, function(g) isTRUE(g$status == 200L), logical(1))
    out[pending[ok]] <- vapply(got[ok], function(g) g$body, character(1))
    pending <- pending[!ok]
    if (!length(pending)) break
    # 429 means we asked too fast. Back off rather than hammering.
    Sys.sleep(2 * attempt)
  }
  if (length(pending)) {
    cli::cli_abort(c(
      "{length(pending)} of {length(paths)} SwimCloud page{?s} could not be fetched.",
      i = "Lower {.arg concurrency} (6 is the measured ceiling; 10 triggers 429)."))
  }
  out
}

#' Parse a SwimCloud event result page
#'
#' One race, whole field. Whole fields are what make the shared race effect
#' identifiable — a per-athlete history cannot separate "off day" from "slow
#' race for everyone" — which is why this is the endpoint worth harvesting.
#'
#' @param html Page HTML from [swimcloud_fetch()].
#' @param meet_id SwimCloud meet id, used to build the race key.
#' @param event_no Event number within the meet.
#' @return A `data.table` in the canonical result schema.
#' @export
swimcloud_parse_event <- function(html, meet_id, event_no) {
  doc <- xml2::read_html(html)
  title <- trimws(rvest::html_text(rvest::html_element(doc, "title")))
  # "LEN European U23 Championships - 50 Free Women"
  meet_name <- trimws(sub(" - [^-]*$", "", title))
  event_name <- trimws(sub("^.* - ", "", title))
  # SwimCloud gives each ROUND its own event number, and folds the round and an
  # age-band marker into the event name: "1500 Free Men (1+) Prelims". Those
  # suffixes must come off or match_event() returns NA -- correctly, since it
  # refuses to guess -- and the whole meet goes unmatched.
  event_name <- trimws(gsub("\\s*\\([^)]*\\)\\s*", " ", event_name))
  event_name <- trimws(sub(paste0("\\s+(Prelims|Preliminaries|Finals?|Semifinals?|",
                                  "Semis?|Heats?|Time ?Trials?|Swim-?offs?)$"),
                           "", event_name, ignore.case = TRUE))

  # Course is declared once at meet level, not per row -- which is why this feed
  # is safer than it first appears. Short course is ~5% faster, several times
  # the within-athlete spread, so it must never be inferred.
  page <- rvest::html_text(doc)
  course <- if (grepl("\\bLCM\\b", page)) "LCM" else if (grepl("\\bSCM\\b", page)) "SCM"
            else if (grepl("\\bSCY\\b", page)) "SCY" else NA_character_
  # The round label is each TABLE's caption, not a page heading -- so an event
  # with heats and finals has one table per round. Taking only the biggest table
  # would silently discard every other round, and nothing would fail.
  tabs <- rvest::html_elements(doc, "table")
  if (!length(tabs)) return(.empty_sc_dt())

  pull <- function(r, sel, attr = NULL) {
    e <- rvest::html_element(r, sel)
    if (is.na(e)) return(NA_character_)
    if (is.null(attr)) trimws(rvest::html_text(e)) else rvest::html_attr(e, attr)
  }

  dt <- data.table::rbindlist(lapply(tabs, function(tb) {
    rows <- rvest::html_elements(tb, "tbody tr")
    rows <- rows[vapply(rows, function(r)
      !is.na(rvest::html_element(r, "a[href*='/swimmer/']")), logical(1))]
    if (!length(rows)) return(NULL)
    cap <- rvest::html_element(tb, "caption")
    round <- if (is.na(cap)) NA_character_ else trimws(rvest::html_text(cap))

    one <- data.table::rbindlist(lapply(rows, function(r) {
      tds <- rvest::html_elements(r, "td")
      href <- pull(r, "a[href*='/swimmer/']", "href")
      data.table::data.table(
        # The stable SwimCloud id is the whole point -- it makes cross-meet
        # linking exact rather than name-based.
        athlete_id   = if (is.na(href)) NA_character_ else
                         paste0("SC", sub(".*/swimmer/([0-9]+)/.*", "\\1", href)),
        athlete_name = pull(r, "a[href*='/swimmer/']"),
        team         = pull(r, "a[href*='/team/']"),
        place        = suppressWarnings(as.integer(
                         trimws(if (length(tds)) rvest::html_text(tds[[1]]) else NA))),
        mark_string  = {
          txt <- trimws(rvest::html_text(tds))
          tm <- grep("^[0-9]{1,2}:[0-9]{2}\\.[0-9]{2}$|^[0-9]{1,3}\\.[0-9]{2}$",
                     txt, value = TRUE)
          if (length(tm)) tm[1] else NA_character_
        })
    }), fill = TRUE)
    one[, round := round]
    one[]
  }), fill = TRUE)

  if (is.null(dt) || !nrow(dt)) return(.empty_sc_dt())
  dt <- dt[!is.na(athlete_name) & nzchar(athlete_name)]
  if (!nrow(dt)) return(.empty_sc_dt())
  ev <- swimcloud_event(event_name)
  dt[, `:=`(sport = "Swimming",
            discipline = ev$discipline, sex = ev$sex,
            event_label = event_name, comp_name = meet_name,
            course = course,
            # Round belongs in the race key: heats and the final are different
            # races, and merging them was exactly the bug that collapsed 24,240
            # athletics races into far too few.
            race_key = paste("SC", meet_id, event_no, round, sep = "|"),
            meet_id = as.character(meet_id), is_best = FALSE)]
  dt[]
}

.empty_sc_dt <- function() {
  data.table::data.table(
    athlete_id = character(), athlete_name = character(), team = character(),
    place = integer(), mark_string = character(), sport = character(),
    discipline = character(), comp_name = character(), course = character(),
    round = character(), race_key = character(), meet_id = character(),
    is_best = logical())
}
