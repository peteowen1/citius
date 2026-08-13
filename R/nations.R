#' Canonical nation name for a medal-table entry
#'
#' Medal tables name the same nation several ways, and two of those ways cost
#' real data before this existed:
#'
#' - Wikipedia's Pan American tables carry letter footnotes which `html_table()`
#'   flattens into the cell, so `"United States a"` (339 golds across three
#'   editions) was a different nation from `"United States"`.
#' - Nations rename. `"Western Samoa"`, `"Ceylon"` and `"Papua and New Guinea"`
#'   are the same competitors as Samoa, Sri Lanka and Papua New Guinea.
#'
#' Renames of one entity are merged. Successor *states* are not: the Soviet
#' Union is not Russia and East Germany is not Germany for the purpose of
#' ranking a performance, even though both resolve to a successor economy in
#' [nation_iso3()].
#'
#' **`"Korea"` is deliberately left alone.** At the 2018 Asian Games it is the
#' unified North/South team, which took 1 gold in the same table where South
#' Korea took 49; folding it in put two rows on one nation in a single edition.
#' Any merge rule here should be checked against the invariant that a canonical
#' nation appears at most once per edition.
#'
#' @param x Character vector of raw nation strings.
#' @return Character vector of canonical names.
#' @export
canonical_nation <- function(x) {
  st <- as.character(x)
  st <- gsub("\u00a0", " ", st)          # non-breaking space
  st <- gsub("\\.mw-parser-output.*$", "", st)
  st <- gsub("\\[.*?\\]|\\(.*?\\)|\\*|\u2020|\u2021", "", st)
  st <- trimws(st)
  st <- gsub("\\s+[0-9]+[a-z]?$|[0-9]+$", "", st)
  st <- gsub("\\s+[a-z]$", "", st)
  st <- trimws(gsub("\\s+", " ", st))

  renames <- c(
    "Western Samoa" = "Samoa", "Tahiti" = "French Polynesia",
    "Papua and New Guinea" = "Papua New Guinea",
    "British Solomon Islands" = "Solomon Islands", "New Hebrides" = "Vanuatu",
    "Bahama Islands" = "Bahamas", "Ceylon" = "Sri Lanka", "Burma" = "Myanmar",
    "Malaya" = "Malaysia", "British Guiana" = "Guyana", "Zaire" = "DR Congo",
    "Democratic Republic of the Congo" = "DR Congo",
    "Republic of the Congo" = "Congo", "Congo-Brazzaville" = "Congo",
    "Swaziland" = "Eswatini", "Northern Rhodesia" = "Zambia",
    "Southern Rhodesia" = "Zimbabwe", "Rhodesia" = "Zimbabwe",
    "Gold Coast" = "Ghana", "Dahomey" = "Benin", "Upper Volta" = "Burkina Faso",
    "Formosa" = "Chinese Taipei", "Republic of China" = "Chinese Taipei",
    # The 1992 post-Soviet team is "Unified Team" in medal tables but "CIS"
    # (or its IOC code EUN) in tournament standings, so its basketball gold
    # failed the check that a medallist appears in its own team list.
    "CIS" = "Unified Team", "EUN" = "Unified Team",
    "Commonwealth of Independent States" = "Unified Team",
    "United Kingdom" = "Great Britain",
    "Great Britain and Ireland" = "Great Britain",
    "Trinidad & Tobago" = "Trinidad and Tobago", "The Gambia" = "Gambia",
    "Saint Vincent" = "Saint Vincent and the Grenadines",
    "Northern Marianas" = "Northern Mariana Islands"
  )
  # Footnote letters glued straight onto the name ("United Statesa", "Cubab").
  # Previously a hand map of the six nations observed so far, which silently
  # missed any nation whose glued footnote had not been seen yet -- the exact
  # failure class this file exists to close. General rule instead: strip one
  # trailing lowercase letter ONLY when the string as written resolves to
  # nothing and the stripped string resolves to a known nation. "Cuba" is
  # untouched (it already resolves); "Cubab" is not a nation, "Cuba" is.
  known <- function(v) !is.na(nation_iso3(v)) | v %in% names(renames)
  cand <- sub("[a-z]$", "", st)
  fixable <- !is.na(st) & st != cand & !known(st) & known(cand)
  st[fixable] <- cand[fixable]

  hit2 <- unname(renames[st])
  ifelse(is.na(hit2), st, hit2)
}


#' ISO3 code for a canonical nation
#'
#' Returns `NA` where there is no match, never a guess. The function this
#' replaces fell back to `substr(toupper(name), 1, 3)`, which produced 37 codes
#' shared by more than one nation -- `"AUS"` for Australia, Austria *and*
#' Australasia; `"NOR"` for Norway, North Korea, Norfolk Island, North Macedonia
#' and three more; `"CHI"` for Chile and Chinese Taipei. Every one of those
#' compared a nation's medals against a different country's GDP and
#' double-counted the denominator.
#'
#' Historical entities map to a successor economy so that an economic
#' comparison is possible at all. That is a modelling choice, not a fact: the
#' Soviet Union's 1980 medals against Russia's population is a statement about
#' the successor state, and should be reported as such.
#'
#' Teams that are not economies -- mixed, neutral and refugee teams -- map to
#' `NA` on purpose and are excluded from economic shares rather than attached
#' to a country.
#'
#' @param canon Character vector from [canonical_nation()].
#' @return Character vector of ISO3 codes, `NA` where unresolved.
#' @export
nation_iso3 <- function(canon) {
  m <- c(
    "Soviet Union"="RUS", "Unified Team"="RUS", "Russian Empire"="RUS",
    "Russia"="RUS", "ROC"="RUS", "Olympic Athletes from Russia"="RUS",
    "East Germany"="DEU", "West Germany"="DEU", "Germany"="DEU",
    "United Team of Germany"="DEU", "Saar"="DEU",
    "Czechoslovakia"="CZE", "Czech Republic"="CZE", "Czechia"="CZE",
    "Bohemia"="CZE", "Yugoslavia"="SRB", "Serbia and Montenegro"="SRB",
    "Serbia"="SRB", "FR Yugoslavia"="SRB",
    "Australasia"="AUS", "British West Indies"="TTO",
    "Netherlands Antilles"="CUW", "Chinese Taipei"="TWN", "Taiwan"="TWN",
    "United Arab Republic"="EGY", "South Vietnam"="VNM",
    "Rhodesia and Nyasaland"="ZWE", "Khmer Republic"="KHM",
    "British Hong Kong"="HKG", "West Indies"="TTO",

    # Frequent medal-winners, listed explicitly rather than left to the World
    # Bank name lookup. Australia and Austria in particular must never collide
    # again: the prefix fallback this replaces gave both "AUS".
    "Australia"="AUS", "Austria"="AUT", "Canada"="CAN", "China"="CHN",
    "France"="FRA", "Italy"="ITA", "Japan"="JPN", "Spain"="ESP",
    "Netherlands"="NLD", "Hungary"="HUN", "Sweden"="SWE", "Norway"="NOR",
    "Finland"="FIN", "Poland"="POL", "Romania"="ROU", "Ukraine"="UKR",
    "New Zealand"="NZL", "South Africa"="ZAF", "Kenya"="KEN", "Jamaica"="JAM",
    "Cuba"="CUB", "Argentina"="ARG", "Mexico"="MEX", "Brazil"="BRA",
    "India"="IND", "Indonesia"="IDN", "Nigeria"="NGA", "Ethiopia"="ETH",
    "Switzerland"="CHE", "Belgium"="BEL", "Denmark"="DNK", "Greece"="GRC",
    "Grenada"="GRD", "Chile"="CHL", "Colombia"="COL", "Peru"="PER",
    "Thailand"="THA", "Malaysia"="MYS", "Singapore"="SGP", "Kazakhstan"="KAZ",
    "Uzbekistan"="UZB", "Belarus"="BLR", "Bulgaria"="BGR", "Croatia"="HRV",
    "Slovenia"="SVN", "Estonia"="EST", "Latvia"="LVA", "Lithuania"="LTU",
    "Ireland"="IRL", "Portugal"="PRT", "Israel"="ISR", "Morocco"="MAR",
    "Algeria"="DZA", "Tunisia"="TUN", "Ghana"="GHA", "Uganda"="UGA",
    "Zimbabwe"="ZWE", "Zambia"="ZMB", "Botswana"="BWA", "Namibia"="NAM",
    "Malawi"="MWI", "Lesotho"="LSO", "Rwanda"="RWA", "Cameroon"="CMR",
    "Barbados"="BRB", "Guyana"="GUY", "Malta"="MLT", "Mauritius"="MUS",

    "United States"="USA", "Great Britain"="GBR", "South Korea"="KOR",
    "Korea"="KOR", "Unified Korea"="KOR", "North Korea"="PRK",
    "Iran"="IRN", "Syria"="SYR", "Egypt"="EGY", "Venezuela"="VEN",
    "Vietnam"="VNM", "Laos"="LAO", "Brunei"="BRN", "Cape Verde"="CPV",
    "Ivory Coast"="CIV", "Cote d'Ivoire"="CIV", "DR Congo"="COD",
    "Congo"="COG", "Gambia"="GMB", "Tanzania"="TZA", "Turkey"="TUR",
    "Slovakia"="SVK", "Moldova"="MDA", "Kyrgyzstan"="KGZ",
    "Macedonia"="MKD", "North Macedonia"="MKD", "Bahamas"="BHS",
    "Saint Lucia"="LCA", "Saint Kitts and Nevis"="KNA",
    "Saint Vincent and the Grenadines"="VCT", "Trinidad and Tobago"="TTO",
    "Antigua and Barbuda"="ATG", "Hong Kong"="HKG", "Macau"="MAC",
    "Myanmar"="MMR", "Eswatini"="SWZ", "Cayman Islands"="CYM",
    "British Virgin Islands"="VGB", "Virgin Islands"="VIR",
    "US Virgin Islands"="VIR", "Puerto Rico"="PRI", "Guam"="GUM",
    "American Samoa"="ASM", "New Caledonia"="NCL", "French Polynesia"="PYF",
    "Solomon Islands"="SLB", "Papua New Guinea"="PNG", "Vanuatu"="VUT",
    "Samoa"="WSM", "Tonga"="TON", "Fiji"="FJI", "Kiribati"="KIR",
    "Tuvalu"="TUV", "Nauru"="NRU", "Palau"="PLW", "Marshall Islands"="MHL",
    "Micronesia"="FSM", "Federated States of Micronesia"="FSM",
    "Cook Islands"="COK", "Niue"="NIU",
    "Northern Mariana Islands"="MNP", "Norfolk Island"="NFK",
    "Wallis and Futuna"="WLF", "Tokelau"="TKL", "Bermuda"="BMU",
    "Isle of Man"="IMN", "Jersey"="JEY", "Guernsey"="GGY", "Gibraltar"="GIB",
    "Saint Helena"="SHN", "Falkland Islands"="FLK", "Anguilla"="AIA",
    "Montserrat"="MSR", "Turks and Caicos Islands"="TCA", "Dominica"="DMA",
    "Grenada"="GRD", "Belize"="BLZ", "Guyana"="GUY",
    "Sao Tome and Principe"="STP", "S\u00e3o Tom\u00e9 and Pr\u00edncipe"="STP",
    "Cyprus"="CYP", "Malta"="MLT", "Mauritius"="MUS", "Seychelles"="SYC",
    "Maldives"="MDV", "Sri Lanka"="LKA", "Bangladesh"="BGD", "Pakistan"="PAK",
    "Yemen"="YEM", "Palestine"="PSE", "Somalia"="SOM",

    # UK home nations; see harvest_gdp_population.R for the apportionment.
    "England"="ENG", "Scotland"="SCO", "Wales"="WAL", "Northern Ireland"="NIR",

    # Not economies.
    "Mixed team"=NA_character_, "Independent Olympic Athletes"=NA_character_,
    "Independent Olympic Participants"=NA_character_,
    "Independent Athletes Team"=NA_character_,
    "Individual Neutral Athletes"=NA_character_,
    "Athletes from Kuwait"=NA_character_,
    "Refugee Olympic Team"=NA_character_, "AIN"=NA_character_
  )
  unname(m[canon])
}


#' Nations that are competitors but not economies
#'
#' Mixed, neutral and refugee teams. Excluded from economic shares rather than
#' attached to a country.
#'
#' @return Character vector of canonical names.
#' @export
non_economy_teams <- function() {
  c("Mixed team", "Independent Olympic Athletes",
    "Independent Olympic Participants", "Independent Athletes Team",
    "Individual Neutral Athletes", "Athletes from Kuwait",
    "Refugee Olympic Team", "AIN")
}
