suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringi)
})

# Match the many spellings of a person's name across the sheets
# ("Res Altwegg", "Altwegg R", "R. Altwegg") to one standard name.
#
# 1. Corrections in data/name_aliases.csv are applied first (typos, nicknames,
#    reversed names). Edit that file to fix a wrong or missed match.
# 2. Otherwise names are matched on surname + first initial against the
#    people listed in the team, postdoc, alumni and student tabs.
# 3. Names that match nobody are grouped with each other the same way.

name_particles <- c("van", "der", "de", "du", "le", "la", "von", "da", "dos", "den")

clean_name <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\(.*?\\)", " ", x)            # "Izak Smith (SANParks)"
  x <- gsub("['’`]", "", x)            # O'Riain -> ORiain
  x <- gsub("[^-A-Za-z. ]", " ", x)
  x <- gsub("\\.", ". ", x)
  trimws(gsub("\\s+", " ", x))
}

# Returns surname and initials for one cleaned name
parse_name <- function(x) {
  toks <- strsplit(x, " ")[[1]]
  toks <- toks[toks != ""]
  if (length(toks) == 0) return(list(surname = NA, initials = NA))
  is_init <- function(t) grepl("^[A-Z]{1,3}\\.?$", t) || grepl("^([A-Z]\\.)+$", t)
  initials_of <- function(ts) paste(substr(gsub("\\.", "", ts), 1, 1), collapse = "")
  last <- toks[length(toks)]
  if (length(toks) >= 2 && grepl("^[A-Z]{1,3}$", last) && !is_init(toks[1])) {
    # "Altwegg R", "Colville JF", "Suresh Babu KV"
    surname <- paste(toks[-length(toks)], collapse = " ")
    initials <- last
  } else {
    # "R. Altwegg", "Jasper A. Slingsby", "Stephni van der Merwe"
    i <- length(toks)
    while (i > 2 && tolower(toks[i - 1]) %in% name_particles) i <- i - 1
    surname <- paste(toks[i:length(toks)], collapse = " ")
    given <- toks[seq_len(i - 1)]
    initials <- if (length(given)) {
      paste(vapply(given, function(g) if (is_init(g)) gsub("\\.", "", g) else substr(g, 1, 1),
                   character(1)), collapse = "")
    } else ""
  }
  list(surname = tolower(gsub("-", " ", surname)), initials = toupper(initials))
}

name_key <- function(x) {
  vapply(clean_name(x), function(n) {
    p <- parse_name(n)
    if (is.na(p$surname)) return(NA_character_)
    paste0(gsub(" ", "", p$surname), "_", substr(p$initials, 1, 1))
  }, character(1), USE.NAMES = FALSE)
}

# All initials, for a second try ("W. Chris Oosthuizen" -> W or C)
name_keys_all <- function(x) {
  p <- parse_name(clean_name(x))
  if (is.na(p$surname)) return(character(0))
  paste0(gsub(" ", "", p$surname), "_", strsplit(p$initials, "")[[1]])
}

load_aliases <- function(path = "data/name_aliases.csv") {
  if (!file.exists(path)) return(setNames(character(0), character(0)))
  a <- read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE, comment.char = "#")
  setNames(a$name, tolower(clean_name(a$alias)))
}

# registry: data frame with a `name` column, in priority order (first wins)
# raw: character vector of names to resolve
resolve_names <- function(raw, registry, aliases = load_aliases()) {
  reg <- registry %>%
    distinct(name, .keep_all = TRUE) %>%
    mutate(key = name_key(name))
  reg_by_key <- reg %>% filter(!is.na(key)) %>% distinct(key, .keep_all = TRUE)

  out <- tibble(raw = unique(raw[!is.na(raw) & trimws(raw) != ""])) %>%
    mutate(
      alias_hit = unname(aliases[tolower(clean_name(raw))]),
      key = name_key(coalesce(alias_hit, raw))
    )
  out$name <- out$alias_hit
  out$source <- ifelse(is.na(out$alias_hit), NA, "correction list")

  for (i in which(is.na(out$name))) {
    hit <- match(out$key[i], reg_by_key$key)
    if (is.na(hit)) {
      alt <- intersect(name_keys_all(out$raw[i]), reg_by_key$key)
      if (length(alt)) hit <- match(alt[1], reg_by_key$key)
    }
    if (!is.na(hit)) {
      out$name[i] <- reg_by_key$name[hit]
      out$source[i] <- "matched"
    }
  }

  # Unmatched names: group by key and show the fullest spelling
  un <- is.na(out$name)
  if (any(un)) {
    best <- out[un, ] %>%
      mutate(
        # Prefer full first names and normal capitalisation
        score = nchar(gsub("[^a-z]", "", raw)) -
          5 * grepl("^[A-Z]{1,3}\\.? |\\s[A-Z]{1,3}$", raw) +
          3 * !grepl("(^|\\s)[a-z]", raw)
      ) %>%
      group_by(key) %>%
      summarise(best = raw[which.max(score)], .groups = "drop")
    out$name[un] <- best$best[match(out$key[un], best$key)]
    out$source[un] <- "not in sheets"
  }
  out %>% transmute(raw, name = trimws(gsub("\\s+", " ", gsub("\\(.*?\\)", "", name))), source)
}
