# historical_odds.R
# Pull historical matchups (round match-ups, 3-balls, 72-hole) with bet outcomes from
# DataGolf Historical Match-Ups & 3-Balls API for every year, every sportsbook, every event (course).
# Writes one CSV: historical_matchups_outcomes.csv

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)
library(readr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

API_KEY <- Sys.getenv("DATAGOLF_API_KEY", "")
BASE_URL <- "https://feeds.datagolf.com/historical-odds/matchups"

# All books from API docs (Historical Match-Ups)
BOOKS <- c(
  "5dimes", "bet365", "betcris", "betmgm", "betonline", "bovada",
  "caesars", "circa", "draftkings", "fanduel", "pinnacle",
  "sportsbook", "williamhill", "unibet"
)

# Years with historical matchup data (adjust if API supports more)
YEARS <- 2019:2025

# Optional: only round match-ups and 3-balls (exclude 72-hole). Set to FALSE to include all bet types.
ONLY_ROUND_MATCHUPS <- FALSE

# Delay between API calls (seconds) to avoid rate limits
REQUEST_DELAY <- 0.5

# Output path
OUTPUT_CSV <- file.path(getwd(), "data", "historical_matchups_outcomes.csv")
if (!dir.exists(dirname(OUTPUT_CSV))) dir.create(dirname(OUTPUT_CSV), recursive = TRUE)

# Fetch one (year, book) response
fetch_matchups <- function(year, book, tour = "pga") {
  res <- GET(
    BASE_URL,
    query = list(
      tour = tour,
      event_id = "all",
      year = year,
      book = book,
      odds_format = "decimal",
      file_format = "json",
      key = API_KEY
    )
  )
  if (status_code(res) != 200) {
    message("  ", book, " ", year, ": HTTP ", status_code(res))
    return(NULL)
  }
  dat <- tryCatch(
    fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE),
    error = function(e) {
      message("  ", book, " ", year, ": parse error ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  dat
}

# Turn one odds row (list) into a flat row for CSV (handles 2-way and 3-way)
odds_row_to_tibble <- function(o, event_id, event_name, event_completed, season, year, book) {
  bet_type <- as.character(o$bet_type %||% "")
  if (ONLY_ROUND_MATCHUPS && nzchar(bet_type)) {
    round_bet <- grepl("^R[1-4]\\s", bet_type, ignore.case = TRUE) || grepl("3-Ball", bet_type, ignore.case = TRUE)
    if (!round_bet) return(NULL)
  }
  tibble(
    event_id = as.character(event_id),
    event_name = as.character(event_name),
    event_completed = as.character(event_completed),
    season = as.integer(season %||% NA),
    year = as.integer(year %||% NA),
    book = as.character(book),
    bet_type = bet_type,
    open_time = as.character(o$open_time %||% NA_character_),
    close_time = as.character(o$close_time %||% NA_character_),
    tie_rule = as.character(o$tie_rule %||% NA_character_),
    p1_dg_id = as.integer(o$p1_dg_id %||% NA_integer_),
    p1_player_name = as.character(o$p1_player_name %||% NA_character_),
    p1_open = as.numeric(o$p1_open %||% NA_real_),
    p1_close = as.numeric(o$p1_close %||% NA_real_),
    p1_outcome = as.numeric(o$p1_outcome %||% NA_real_),
    p1_outcome_text = as.character(o$p1_outcome_text %||% NA_character_),
    p2_dg_id = as.integer(o$p2_dg_id %||% NA_integer_),
    p2_player_name = as.character(o$p2_player_name %||% NA_character_),
    p2_open = as.numeric(o$p2_open %||% NA_real_),
    p2_close = as.numeric(o$p2_close %||% NA_real_),
    p2_outcome = as.numeric(o$p2_outcome %||% NA_real_),
    p2_outcome_text = as.character(o$p2_outcome_text %||% NA_character_),
    p3_dg_id = as.integer(o$p3_dg_id %||% NA_integer_),
    p3_player_name = as.character(o$p3_player_name %||% NA_character_),
    p3_open = as.numeric(o$p3_open %||% NA_real_),
    p3_close = as.numeric(o$p3_close %||% NA_real_),
    p3_outcome = as.numeric(o$p3_outcome %||% NA_real_),
    p3_outcome_text = as.character(o$p3_outcome_text %||% NA_character_)
  )
}

# Single-event response: odds is array of matchup rows
parse_single_event <- function(dat, book, year) {
  event_id   <- as.character(dat$event_id %||% NA_character_)
  event_name <- as.character(dat$event_name %||% NA_character_)
  event_completed <- as.character(dat$event_completed %||% NA_character_)
  season     <- as.integer(dat$season %||% NA_integer_)
  odds <- dat$odds
  if (is.null(odds) || length(odds) == 0) return(NULL)
  if (is.data.frame(odds)) {
    rows <- lapply(seq_len(nrow(odds)), function(i) as.list(odds[i, ]))
  } else {
    rows <- odds
  }
  out_list <- map(rows, function(o) {
    if (!is.list(o)) return(NULL)  # skip atomic elements (e.g. bare index in some API shapes)
    odds_row_to_tibble(o, event_id, event_name, event_completed, season, year, book)
  })
  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  if (length(out_list) == 0) return(NULL)
  out <- bind_rows(out_list)
  out <- out %>% filter(!is.na(bet_type) | !is.na(p1_dg_id))
  out
}

# Detect structure: single event vs list of events
parse_response_v2 <- function(dat, book, year) {
  if (is.null(dat) || length(dat) == 0) return(NULL)
  # Single event: has "odds" and event-level fields (must be list, not atomic)
  if (is.list(dat) && !is.data.frame(dat) && "odds" %in% names(dat)) {
    return(parse_single_event(dat, book, year))
  }
  # List of events (e.g. from event_id=all) — only recurse into list elements
  if (is.list(dat) && !is.data.frame(dat)) {
    pieces <- map(dat, function(ev) {
      if (!is.list(ev)) return(NULL)  # skip atomic vectors (e.g. bare event_id in some API shapes)
      parse_response_v2(ev, book, year)
    })
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(NULL)
    return(bind_rows(pieces))
  }
  if (is.data.frame(dat) && nrow(dat) > 0) {
    return(map_dfr(seq_len(nrow(dat)), function(i) parse_response_v2(as.list(dat[i, ]), book, year)))
  }
  NULL
}

# Main
message("Pulling historical matchups: tour=pga, event_id=all, years ", min(YEARS), "-", max(YEARS), ", books ", length(BOOKS))
all_rows <- list()
n_total <- 0
for (yr in YEARS) {
  for (bk in BOOKS) {
    Sys.sleep(REQUEST_DELAY)
    raw <- fetch_matchups(yr, bk)
    if (is.null(raw)) next
    parsed <- parse_response_v2(raw, bk, yr)
    if (is.null(parsed) || nrow(parsed) == 0) next
    all_rows[[length(all_rows) + 1]] <- parsed
    n_total <- n_total + nrow(parsed)
    message("  ", bk, " ", yr, ": ", nrow(parsed), " rows (total so far: ", n_total, ")")
  }
}

if (length(all_rows) == 0) {
  stop("No data retrieved. Check API key and network.")
}

out <- bind_rows(all_rows)
# Remove any all-NA rows
out <- out %>% filter(!is.na(event_id) | !is.na(p1_dg_id) | !is.na(book))

write_csv(out, OUTPUT_CSV, na = "")
message("Wrote ", nrow(out), " rows to ", OUTPUT_CSV)
