# historical_outrights.R
# Pull historical outright odds (win, top_5, top_10, top_20, make_cut, mc) with bet outcomes
# from DataGolf Historical Outrights API for every year, every sportsbook, every market, every event.
# American odds. Writes one CSV: historical_outrights_outcomes.csv

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)
library(readr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

API_KEY <- Sys.getenv("DATAGOLF_API_KEY", "")
BASE_URL <- "https://feeds.datagolf.com/historical-odds/outrights"

# Books from API docs (Historical Outrights)
BOOKS <- c(
  "bet365", "betcris", "betmgm", "betonline", "betway", "bovada",
  "caesars", "corale", "circa", "draftkings", "fanduel", "pinnacle",
  "skybet", "sportsbook", "unibet", "williamhill"
)

# Markets (required parameter)
MARKETS <- c("win", "top_5", "top_10", "top_20", "make_cut", "mc")

YEARS <- 2019:2025
REQUEST_DELAY <- 0.5
OUTPUT_CSV <- file.path(getwd(), "data", "historical_outrights_outcomes.csv")
if (!dir.exists(dirname(OUTPUT_CSV))) dir.create(dirname(OUTPUT_CSV), recursive = TRUE)

# Fetch one (year, book, market) response
fetch_outrights <- function(year, book, market, tour = "pga") {
  res <- GET(
    BASE_URL,
    query = list(
      tour = tour,
      event_id = "all",
      year = year,
      market = market,
      book = book,
      odds_format = "american",
      file_format = "json",
      key = API_KEY
    )
  )
  if (status_code(res) != 200) {
    message("  ", book, " ", year, " ", market, ": HTTP ", status_code(res))
    return(NULL)
  }
  dat <- tryCatch(
    fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE),
    error = function(e) {
      message("  ", book, " ", year, " ", market, ": parse error ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  dat
}

# One odds row (one player) -> one tibble row
outright_row_to_tibble <- function(o, event_id, event_name, event_completed, season, year, book, market) {
  if (!is.list(o)) return(NULL)
  tibble(
    event_id = as.character(event_id),
    event_name = as.character(event_name),
    event_completed = as.character(event_completed),
    season = as.integer(season %||% NA_integer_),
    year = as.integer(year %||% NA_integer_),
    book = as.character(book),
    market = as.character(market),
    dg_id = as.integer(o$dg_id %||% NA_integer_),
    player_name = as.character(o$player_name %||% NA_character_),
    open_odds = as.numeric(o$open_odds %||% NA_real_),
    close_odds = as.numeric(o$close_odds %||% NA_real_),
    open_time = as.character(o$open_time %||% NA_character_),
    close_time = as.character(o$close_time %||% NA_character_),
    bet_outcome_numeric = as.numeric(o$bet_outcome_numeric %||% NA_real_),
    bet_outcome_text = as.character(o$bet_outcome_text %||% NA_character_),
    outcome = as.character(o$outcome %||% NA_character_)
  )
}

# Single-event response
parse_single_event <- function(dat, book, year, market) {
  event_id <- as.character(dat$event_id %||% NA_character_)
  event_name <- as.character(dat$event_name %||% NA_character_)
  event_completed <- as.character(dat$event_completed %||% NA_character_)
  season <- as.integer(dat$season %||% NA_integer_)
  odds <- dat$odds
  if (is.null(odds) || length(odds) == 0) return(NULL)
  if (is.data.frame(odds)) {
    rows <- lapply(seq_len(nrow(odds)), function(i) as.list(odds[i, ]))
  } else {
    rows <- odds
  }
  out_list <- map(rows, function(o) {
    if (!is.list(o)) return(NULL)
    outright_row_to_tibble(o, event_id, event_name, event_completed, season, year, book, market)
  })
  out_list <- out_list[!vapply(out_list, is.null, logical(1))]
  if (length(out_list) == 0) return(NULL)
  out <- bind_rows(out_list)
  out <- out %>% filter(!is.na(dg_id) | !is.na(player_name))
  out
}

# Single event vs list of events
parse_response <- function(dat, book, year, market) {
  if (is.null(dat) || length(dat) == 0) return(NULL)
  if (is.list(dat) && !is.data.frame(dat) && "odds" %in% names(dat)) {
    return(parse_single_event(dat, book, year, market))
  }
  if (is.list(dat) && !is.data.frame(dat)) {
    pieces <- map(dat, function(ev) {
      if (!is.list(ev)) return(NULL)
      parse_response(ev, book, year, market)
    })
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(NULL)
    return(bind_rows(pieces))
  }
  if (is.data.frame(dat) && nrow(dat) > 0) {
    return(map_dfr(seq_len(nrow(dat)), function(i) parse_response(as.list(dat[i, ]), book, year, market)))
  }
  NULL
}

# Main
message("Pulling historical outrights: tour=pga, event_id=all, years ", min(YEARS), "-", max(YEARS),
        ", books ", length(BOOKS), ", markets ", paste(MARKETS, collapse = ", "))
all_rows <- list()
n_total <- 0
for (yr in YEARS) {
  for (bk in BOOKS) {
    for (mkt in MARKETS) {
      Sys.sleep(REQUEST_DELAY)
      raw <- fetch_outrights(yr, bk, mkt)
      if (is.null(raw)) next
      parsed <- parse_response(raw, bk, yr, mkt)
      if (is.null(parsed) || nrow(parsed) == 0) next
      all_rows[[length(all_rows) + 1]] <- parsed
      n_total <- n_total + nrow(parsed)
      message("  ", bk, " ", yr, " ", mkt, ": ", nrow(parsed), " rows (total so far: ", n_total, ")")
    }
  }
}

if (length(all_rows) == 0) {
  stop("No data retrieved. Check API key and network.")
}

out <- bind_rows(all_rows)
out <- out %>% filter(!is.na(event_id) | !is.na(dg_id) | !is.na(book))

write_csv(out, OUTPUT_CSV, na = "")
message("Wrote ", nrow(out), " rows to ", OUTPUT_CSV)
