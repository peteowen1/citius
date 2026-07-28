#' Split a name into surname and given parts
#'
#' Feeds disagree about name order — World Aquatics writes `"SHORT Samuel"`, the
#' Commonwealth Games results system writes `"Sam SHORT"` — but both signal the
#' surname by capitalising it, so the order can be read off each name rather than
#' declared per source. That matters because the convention is not always kept:
#' a feed that is nominally surname-first still contains all-caps rows carrying
#' no case signal at all.
#'
#' @param x Character vector of names.
#' @param name_order Fallback when a name carries no case signal: one of
#'   `"surname_first"`, `"given_first"`. Used only for all-caps or all-lower
#'   names.
#' @return A list with character vectors `surname` and `given`.
#' @keywords internal
.split_name <- function(x, name_order = c("surname_first", "given_first")) {
  name_order <- match.arg(name_order)
  x <- gsub("[^A-Za-z ]", " ", as.character(x))
  parts <- strsplit(trimws(gsub("\\s+", " ", x)), " ")
  sur <- character(length(parts))
  giv <- character(length(parts))
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    p <- p[!is.na(p) & nzchar(p)]
    if (length(p) < 2L) {
      sur[i] <- if (length(p)) toupper(p) else NA_character_
      giv[i] <- NA_character_
      next
    }
    caps <- p == toupper(p) & nchar(p) > 1L
    if (any(caps) && !all(caps)) {
      s <- p[caps]
      g <- p[!caps]
    } else if (name_order == "surname_first") {
      # "VAN DER COLFF Deandra" — the surname can be several tokens, so it is
      # all-but-last rather than the first token.
      s <- p[-length(p)]
      g <- p[length(p)]
    } else {
      s <- p[length(p)]
      g <- p[-length(p)]
    }
    sur[i] <- toupper(paste(s, collapse = ""))
    giv[i] <- toupper(paste(g, collapse = ""))
  }
  list(surname = sur, given = giv)
}

#' Loose athlete key: surname plus given initial
#'
#' [athlete_key()] is exact, and exactness costs real matches: the same swimmer
#' is `"Sam SHORT"` at the Commonwealth Games and `"SHORT Samuel"` at World
#' Aquatics, so their two careers never join. Short and full forms appear in
#' *both* directions (`"Joshua LIENDO"` / `"LIENDO Josh"`), so no rule that
#' prefers the longer or the shorter form can work. Keying on surname plus the
#' first initial matches all of them.
#'
#' This key is **not safe on its own**. Measured on the swimming corpus it is
#' ambiguous for 4.85% of keys, concentrated exactly where you would expect:
#' `ZHANG|Y` covers **20 different swimmers**. Always drop keys that are
#' ambiguous within a source before joining on it — which is what
#' [athlete_crosswalk()] does.
#'
#' @inheritParams .split_name
#' @return Character vector of keys, `NA` where no given name is present.
#' @examples
#' loose_key("Sam SHORT", "given_first")
#' loose_key("SHORT Samuel", "surname_first")   # identical
#' @export
loose_key <- function(x, name_order = c("surname_first", "given_first")) {
  p <- .split_name(x, name_order)
  out <- paste0(p$surname, "|", substr(p$given, 1L, 1L))
  out[is.na(p$surname) | is.na(p$given)] <- NA_character_
  out
}

#' Merge person groups that share an unambiguous key
#'
#' One linking pass. Groups are merged only where the key is unambiguous
#' *within* each source — a key covering two different athletes in one source
#' identifies nobody, and merging on it silently fuses two careers.
#'
#' @param x Crosswalk table carrying `person_id`, `source` and `match_method`.
#' @param key Name of the key column to link on.
#' @param method Label recorded in `match_method` for links this pass creates.
#' @param guard Whether to apply the ambiguity and birthdate-conflict guards.
#'   Verified links skip them: they rest on evidence the name keys cannot see,
#'   so an ambiguous key is no longer grounds to refuse them.
#' @return `x`, modified by reference.
#' @keywords internal
.link_by_key <- function(x, key, method, guard = TRUE, scope = NULL) {
  d <- x[!is.na(get(key)), .(k = get(key), person_id, source, birthdate)]
  if (!nrow(d)) return(x)
  # Scope restricts which links may FIRE, not which keys exist. Both sides of a
  # link need a key -- blanking the counterparty's key makes the link
  # impossible rather than selective -- so instead require that each linked
  # group contains at least one in-scope athlete.
  if (!is.null(scope)) {
    keep <- d[source %in% scope, unique(k)]
    d <- d[k %in% keep]
    if (!nrow(d)) return(x)
  }
  if (guard) {
    amb <- d[, .(n = data.table::uniqueN(person_id)), by = .(source, k)][
      n > 1L, unique(k)]
    # A weaker key must never override a stronger disagreement. "Chris Bennett"
    # and "Christopher BENNETT" share surname and initial, but if both carry a
    # birthdate and the dates differ they are certainly two people -- and fusing
    # two careers is undetectable downstream.
    conflict <- d[!is.na(birthdate), .(n = data.table::uniqueN(birthdate)),
                  by = k][n > 1L, unique(k)]
    d <- d[!k %in% amb & !k %in% conflict]
  }
  # Only link ACROSS sources. Two spellings inside one source are likelier to be
  # two people than one, and the ambiguity guard has already cleared that case.
  link <- d[, .(n_src = data.table::uniqueN(source),
                n_person = data.table::uniqueN(person_id)), by = k][
    n_src > 1L & n_person > 1L, k]
  if (!length(link)) return(x)
  d <- d[k %in% link]
  canon <- d[order(person_id), .(canon = person_id[1L]), by = k]
  map <- unique(merge(d[, .(k, person_id)], canon, by = "k")[
    , .(person_id, canon)])[, .SD[1L], by = person_id]
  x[map, on = "person_id", canon := i.canon]
  x[!is.na(canon) & canon != person_id & is.na(match_method),
    match_method := method]
  x[!is.na(canon), person_id := canon]
  x[, canon := NULL]
  x
}

#' Build a cross-source athlete crosswalk
#'
#' Every source names the same athlete differently and only some carry a stable
#' id, so each script that touches two feeds ends up re-deriving its own match.
#' This builds the mapping **once**, as a table that can be inspected, corrected
#' by hand and reused — rather than a rule buried in a pipeline.
#'
#' Athletes are linked in three passes, strongest first, and the pass that
#' created each link is recorded so downstream users can choose how much to
#' trust it:
#'
#' 1. `"birthdate"` — same surname and exact date of birth. Two people sharing
#'    both is vanishingly rare, and it is completely immune to name-form
#'    variation. Only fires where a source supplies `birthdate`.
#' 2. `"exact"` — identical [athlete_key()]: the same name tokens in any order.
#' 3. `"loose"` — same surname and given initial. This is what recovers
#'    `"Thomas DEAN"` / `"DEAN Tom"` and `"Sam SHORT"` / `"SHORT Samuel"`; the
#'    ambiguity guard is what stops it merging twenty different `ZHANG Y`
#'    swimmers.
#'
#' Passes 1 and 2 were checked against each other on the Glasgow 2026 athletics
#' entry list: of 622 athletes matched by both, they agreed on **100%**.
#'
#' Athletes that link to nobody are still returned, with `match_method`
#' `"unmatched"` and a `person_id` of their own. The table is therefore a
#' complete registry of every athlete seen in any source, not just the
#' successful matches — **the unmatched rows are the harvest to-do list**.
#'
#' @param x A data frame of athletes with columns `source` and `athlete_name`,
#'   optionally `athlete_id`, `country`, `sport` and `birthdate`. One row per
#'   athlete per source; duplicates are collapsed.
#' @param name_order Named character vector giving the fallback name order per
#'   source, e.g. `c(worldaquatics = "surname_first", crs = "given_first")`.
#'   Consulted only for names carrying no capitalisation signal — and it matters:
#'   World Aquatics writes `"SHORT Samuel"` while World Athletics writes
#'   `"Taoufik Makhloufi"`.
#' @param fuzzy_scope Optional character vector of `source` names. The
#'   name-based passes (`birthdate`, `loose`) are attempted **only** for
#'   athletes appearing in one of these sources; everything else links on
#'   verified ids alone.
#'
#'   This exists because surname-plus-initial stops identifying anyone once the
#'   corpora are large. Validated against 364 Games swimmers it worked well; run
#'   across 40,000 British and 23,000 international swimmers it merged `Sophie
#'   Bateman` with `BATEMAN Sarah`, `Kate Ward` with `WARD Kristy` and `Emilia
#'   Vorster` with `VORSTER Eben`. The per-source ambiguity guard cannot catch
#'   those — each key is unique *within* its own source and still refers to two
#'   different people. Scoping to the field you actually need to resolve keeps
#'   the candidate pool small enough for the rule to mean something.
#' @param links Optional data frame of externally verified identities, with
#'   `source`, `link_id`, and `athlete_id` and/or `athlete_name`. Rows sharing a
#'   `link_id` are taken to be the same person and are merged before any key
#'   pass, overriding the ambiguity guards. Use it for identities confirmed
#'   outside the name keys — a resolver hit against a search endpoint, or a
#'   correction made by hand. Without it, an athlete whose loose key is
#'   ambiguous (`LEE|M`) stays unlinked even once their career is harvested.
#' @return A `data.table`, one row per athlete per source, with `key_exact`,
#'   `key_loose`, `key_dob`, `person_id` and `match_method`. Rows sharing a
#'   `person_id` are believed to be the same athlete, so joining two sources is
#'   a self-join on that column.
#' @seealso [athlete_key()] for the exact key, [loose_key()] for the loose one.
#' @export
athlete_crosswalk <- function(x, name_order = NULL, links = NULL,
                              fuzzy_scope = NULL) {
  x <- data.table::as.data.table(x)
  stopifnot(all(c("source", "athlete_name") %in% names(x)))
  for (col in c("athlete_id", "country", "sport", "birthdate")) {
    if (!col %in% names(x)) x[[col]] <- NA
  }
  x <- unique(x[!is.na(athlete_name) & nzchar(trimws(athlete_name)),
                .(sport, source, athlete_id = as.character(athlete_id),
                  athlete_name, country,
                  birthdate = as.Date(birthdate))])

  x[, key_exact := athlete_key(athlete_name)]
  x[, `:=`(key_loose = NA_character_, key_dob = NA_character_)]
  for (s in unique(x$source)) {
    ord <- if (!is.null(name_order) && s %in% names(name_order)) {
      name_order[[s]]
    } else {
      "surname_first"
    }
    x[source == s, key_loose := loose_key(athlete_name, ord)]
    x[source == s & !is.na(birthdate),
      key_dob := paste0(.split_name(athlete_name, ord)$surname, "|", birthdate)]
  }

  # Start every athlete in their own group, keyed by exact name so identical
  # names begin together and later passes merge whole clusters at a time.
  x[, person_id := key_exact]
  x[is.na(person_id), person_id := paste0("~", source, "~", seq_len(.N))]
  x[, match_method := NA_character_]

  # Verified links come first and are exempt from the ambiguity guards: they
  # were established by evidence the name keys cannot see, so an ambiguous key
  # is no longer a reason to refuse them.
  x[, key_manual := NA_character_]
  if (!is.null(links) && nrow(links)) {
    lk <- data.table::as.data.table(links)
    if ("athlete_id" %in% names(lk)) {
      x[lk[!is.na(athlete_id)], on = .(source, athlete_id),
        key_manual := i.link_id]
    }
    if ("athlete_name" %in% names(lk)) {
      x[lk[!is.na(athlete_name)], on = .(source, athlete_name),
        key_manual := data.table::fifelse(is.na(key_manual), i.link_id,
                                          key_manual)]
    }
  }

  x <- .link_by_key(x, "key_manual", "verified", guard = FALSE)

  for (pass in list(c("key_dob", "birthdate"), c("key_loose", "loose"))) {
    x <- .link_by_key(x, pass[1L], pass[2L], scope = fuzzy_scope)
  }

  # A pass marks only the rows whose group id moved, but the method describes
  # how the whole group was formed -- so propagate it across the group, keeping
  # the strongest pass that contributed.
  rank <- c(verified = 1L, birthdate = 2L, loose = 3L)
  x[, .m := rank[match_method]]
  if (any(!is.na(x$.m))) {
    grp <- x[!is.na(.m), .(best = names(rank)[min(.m)]), by = person_id]
    x[grp, on = "person_id", match_method := i.best]
  }
  x[, .m := NULL]

  n_src <- x[, .(n = data.table::uniqueN(source)), by = person_id]
  x[n_src, on = "person_id", n_sources := i.n]
  x[is.na(match_method), match_method := data.table::fifelse(
    n_sources > 1L, "exact", "unmatched")]
  x[, n_sources := NULL]
  x[]
}
