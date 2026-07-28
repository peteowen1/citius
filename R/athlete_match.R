#' Order-invariant athlete name key
#'
#' Feeds disagree about name order. World Aquatics writes `"SJOESTROEM Sarah"`;
#' the Commonwealth Games results system writes `"Hannah STERRY"`. Concatenating
#' the letters gives `SJOESTROEMSARAH` and `HANNAHSTERRY`, so a straight key
#' matched **1 of 388** Glasgow swimmers to their own history — which is why no
#' swimming predictions existed for that meet.
#'
#' Sorting the name *tokens* before joining makes the key order-invariant and
#' lifts that to 46%. It stays **exact**: two names match only if they contain
#' precisely the same parts. That matters more here than a higher hit rate would
#' — the same reasoning as [match_event()] returning `NA` rather than guessing.
#' A wrongly linked athlete silently merges two people's careers into one
#' ability estimate, and nothing downstream can detect it.
#'
#' What it deliberately does **not** do: fuzzy or phonetic matching. Diacritic
#' transliterations (`Sjöström` / `SJOESTROEM`) and abbreviated middle names stay
#' unmatched rather than being guessed at.
#'
#' @param x Character vector of athlete names.
#' @return Character vector of keys, safe to join on.
#' @examples
#' athlete_key(c("Hannah STERRY", "STERRY Hannah"))   # identical
#' athlete_key("Mohamed Aan HUSSAIN") == athlete_key("HUSSAIN Mohamed Aan")
#' @export
athlete_key <- function(x) {
  x <- toupper(as.character(x))
  # Hyphens and apostrophes need OPPOSITE treatment, because feeds disagree
  # about them in opposite ways.
  #
  # Apostrophes are REMOVED, never replaced with a space: substituting one
  # splits "O'BRIEN" into two tokens which sort apart, giving "BRIENOSEAN"
  # instead of "OBRIENSEAN". No feed writes "O BRIEN".
  #
  # Hyphens are SPLIT, because feeds genuinely disagree hyphen-vs-space for the
  # same person: "Imara-Bella Patricia THORPE" at the Commonwealth Games is
  # "THORPE Imara Bella Patricia" at World Aquatics. Removing the hyphen makes
  # "IMARABELLA", which can never equal {IMARA, BELLA}, and a semi-finalist
  # already present in both corpora stayed unmatched because of it.
  x <- gsub("-", " ", x)
  x <- gsub("[^A-Z ]", "", x)
  parts <- strsplit(trimws(x), "\\s+")
  vapply(parts, function(p) {
    # `nzchar(NA)` is TRUE, so an NA name survives a naive filter, `sort()` then
    # silently drops it, and the function returns "" -- an empty key that joins
    # to every other empty key. Unnamed athletes would be merged into one
    # person. Test both conditions explicitly.
    p <- p[!is.na(p) & nzchar(p)]
    if (!length(p)) return(NA_character_)
    paste(sort(p), collapse = "")
  }, character(1))
}
