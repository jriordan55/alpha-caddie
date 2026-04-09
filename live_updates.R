# live_updates.R
# Adds 2026 PGA data to three CSVs:
#   1) historical_rounds_all.csv  - Round Scoring, Stats & Strokes Gained
#   2) historical_matchups_outcomes.csv - Historical Match-Ups & 3-Balls
#   3) historical_outrights_outcomes.csv - Historical Outrights (win, top_5, make_cut, etc.)
# Safe to run repeatedly: appends new 2026 PGA data; LIV/other tours: use alpha-caddie-web npm run update:rounds (Node).
#
# Rounds API: https://feeds.datagolf.com/historical-raw-data/rounds
# Matchups API: https://feeds.datagolf.com/historical-odds/matchups
# Outrights API: https://feeds.datagolf.com/historical-odds/outrights

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(readr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Paths: always relative to project / model directory
MODEL_DIR <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd()
PATH_ROUNDS <- file.path(MODEL_DIR, "data", "historical_rounds_all.csv")
PATH_MATCHUPS <- file.path(MODEL_DIR, "data", "historical_matchups_outcomes.csv")
PATH_OUTRIGHTS <- file.path(MODEL_DIR, "data", "historical_outrights_outcomes.csv")

TARGET_YEAR <- 2026L
REQUEST_DELAY <- 0.5

# Exact column order and names to match historical_rounds_all.csv (and your spreadsheet view)
COL_ORDER <- c(
  "tour", "year", "season", "event_completed", "event_name", "event_id",
  "player_name", "dg_id", "fin_text", "round_num", "course_name", "course_num", "course_par",
  "start_hole", "teetime", "round_score", "sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total",
  "driving_dist", "driving_acc", "gir", "scrambling", "prox_rgh", "prox_fw", "great_shots", "poor_shots",
  "eagles_or_better", "birdies", "pars", "bogies", "doubles_or_worse"
)

# Map API column name variants (e.g. from CSV) to our canonical names
API_COL_RENAME <- c(
  score = "round_score",
  round_number = "round_num",
  round = "round_num",
  player = "player_name",
  event_n = "event_name",
  event_name = "event_name",
  event_c = "event_completed",
  event_completed = "event_completed",
  round_r = "round_num",
  round_s = "round_score",
  course = "course_name",
  start_ho = "start_hole",
  eagles = "eagles_or_better",
  scramb = "scrambling",
  prox_rgl = "prox_rgh",
  great_s = "great_shots",
  poor_sh = "poor_shots"
)

# Load fetch_year_rounds and COL_ORDER_LIVE from live_data.R
live_data_path <- file.path(MODEL_DIR, "live_data.R")
if (!file.exists(live_data_path)) {
  stop("live_data.R not found at ", live_data_path, ". Cannot fetch rounds without it.")
}
source(live_data_path, local = FALSE)

# Normalize fetched data to match historical_rounds_all.csv format: correct names and column order
normalize_to_csv_format <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  nms <- names(df)
  for (i in seq_along(API_COL_RENAME)) {
    alias <- names(API_COL_RENAME)[i]
    canon <- API_COL_RENAME[[i]]
    if (alias %in% nms && !canon %in% nms) {
      df[[canon]] <- df[[alias]]
      df[[alias]] <- NULL
      nms <- names(df)
    }
  }
  # Use COL_ORDER_LIVE from live_data if we have it, else COL_ORDER
  col_order <- if (exists("COL_ORDER_LIVE", inherits = TRUE)) COL_ORDER_LIVE else COL_ORDER
  for (c in col_order) if (!c %in% names(df)) df[[c]] <- NA
  df <- df %>% dplyr::select(dplyr::all_of(col_order))
  df
}

# ---- Step 1: Read historical_rounds_all.csv into R ----
# Returns a tibble; empty tibble if file missing or unreadable.
read_historical_rounds <- function(path = PATH_ROUNDS) {
  if (!file.exists(path)) {
    message("  File not found: ", path, " (returning empty tibble).")
    return(tibble::tibble())
  }
  out <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) {
      message("  Read error: ", conditionMessage(e))
      tibble::tibble()
    }
  )
  message("  Read ", nrow(out), " rows from ", path)
  out
}

# ---- Step 2: Fetch 2026 (or given year) PGA data and append ----
update_rounds_2026 <- function(try_year = TARGET_YEAR) {
  col_order <- if (exists("COL_ORDER_LIVE", inherits = TRUE)) COL_ORDER_LIVE else COL_ORDER

  message("Target CSV (read + write): ", PATH_ROUNDS)
  if (!dir.exists(dirname(PATH_ROUNDS))) {
    message("  ERROR: Directory does not exist. Create: ", dirname(PATH_ROUNDS))
    return(invisible(0L))
  }

  # 1) Read historical_rounds_all.csv into R
  message("Step 1: Reading historical_rounds_all.csv into R ...")
  historical_rounds <- read_historical_rounds(PATH_ROUNDS)
  if (nrow(historical_rounds) > 0 && "year" %in% names(historical_rounds)) {
    historical_rounds <- historical_rounds %>%
      dplyr::mutate(
        event_id = as.character(event_id),
        year = as.integer(year),
        event_completed = event_completed_chr_for_csv(.data$event_completed),
        event_name = as.character(event_name),
        course_name = as.character(course_name),
        teetime = teetime_chr_for_csv(.data$teetime)
      ) %>%
      dplyr::mutate(
        .tn = tolower(trimws(as.character(.data$tour))),
        .tn = dplyr::if_else(is.na(.tn) | .tn == "", "pga", .tn)
      ) %>%
      dplyr::filter(!(as.integer(.data$year) == try_year & .tn == "pga")) %>%
      dplyr::select(-.tn)
    for (c in col_order) if (!c %in% names(historical_rounds)) historical_rounds[[c]] <- NA
    historical_rounds <- historical_rounds %>% dplyr::select(dplyr::all_of(col_order))
  }

  # 2) Fetch new year data from API (PGA only here; use Node npm run update:rounds for PGA+LIV)
  message("Step 2: Fetching ", try_year, " PGA rounds from API ...")
  out_dir <- dirname(PATH_ROUNDS)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = TRUE)
    message("  Created dir: ", out_dir)
  }
  year_rounds <- fetch_year_rounds(try_year, "pga")
  n_new <- 0L
  if (!is.null(year_rounds) && nrow(year_rounds) > 0) {
    message("  Fetched ", nrow(year_rounds), " rows for ", try_year, ".")
    n_new <- nrow(year_rounds)
    year_rounds <- year_rounds %>%
      dplyr::mutate(
        event_id = as.character(event_id),
        year = as.integer(year),
        event_completed = event_completed_chr_for_csv(.data$event_completed),
        event_name = as.character(event_name),
        course_name = as.character(course_name),
        teetime = teetime_chr_for_csv(.data$teetime)
      )
    year_rounds <- normalize_to_csv_format(year_rounds)
    year_rounds <- year_rounds %>% dplyr::select(dplyr::all_of(col_order))
  } else {
    message("  No ", try_year, " rounds from API (HTTP/parse may have failed or year not available).")
    message("  Tip: If 2026 has no events yet, run: live_updates_2026(2025L) to add 2025 data.")
    year_rounds <- tibble::tibble()
  }

  # 3) Combine, then write CSV in place (same file we read from)
  combined <- dplyr::bind_rows(historical_rounds, year_rounds)
  # Ensure we have all columns so arrange/select never error on empty data
  for (c in col_order) if (!c %in% names(combined)) combined[[c]] <- NA
  combined <- combined %>% dplyr::select(dplyr::all_of(col_order))
  if (nrow(combined) > 0) {
    combined <- combined %>% dplyr::arrange(dplyr::desc(year), event_id, dg_id, round_num)
  }

  message("Step 3: Writing to: ", PATH_ROUNDS)
  err <- tryCatch(
    {
      readr::write_csv(combined, PATH_ROUNDS)
      NULL
    },
    error = function(e) e
  )
  if (!is.null(err)) {
    message("  WRITE FAILED: ", conditionMessage(err))
    message("  (Close the CSV in Excel/other apps and try again.)")
    assign("historical_rounds", combined, envir = .GlobalEnv)
    return(invisible(0L))
  }
  # Verify the file was actually updated
  fi <- file.info(PATH_ROUNDS)
  message("  OK: Wrote ", nrow(combined), " rows (", n_new, " new for ", try_year, ").")
  message("  File updated: ", PATH_ROUNDS)
  message("  Size: ", fi$size, " bytes  Modified: ", format(fi$mtime, "%Y-%m-%d %H:%M:%S"))
  assign("historical_rounds", combined, envir = .GlobalEnv)
  invisible(n_new)
}

# ---- Historical Match-Ups & 3-Balls: 2026 PGA -> historical_matchups_outcomes.csv ----
# API: https://feeds.datagolf.com/historical-odds/matchups?tour=pga&event_id=all&year=&book=&odds_format=decimal&file_format=json&key=...
MATCHUPS_BASE <- "https://feeds.datagolf.com/historical-odds/matchups"
MATCHUPS_BOOKS <- c(
  "5dimes", "bet365", "betcris", "betmgm", "betonline", "bovada",
  "caesars", "circa", "draftkings", "fanduel", "pinnacle",
  "sportsbook", "williamhill", "unibet"
)
MATCHUPS_COL_ORDER <- c(
  "event_id", "event_name", "event_completed", "season", "year", "book", "bet_type",
  "open_time", "close_time", "tie_rule",
  "p1_dg_id", "p1_player_name", "p1_open", "p1_close", "p1_outcome", "p1_outcome_text",
  "p2_dg_id", "p2_player_name", "p2_open", "p2_close", "p2_outcome", "p2_outcome_text",
  "p3_dg_id", "p3_player_name", "p3_open", "p3_close", "p3_outcome", "p3_outcome_text"
)

matchup_row_to_tibble <- function(o, event_id, event_name, event_completed, season, year, book) {
  if (!is.list(o) || is.data.frame(o)) return(NULL)
  tibble(
    event_id = as.character(event_id),
    event_name = as.character(event_name),
    event_completed = as.character(event_completed),
    season = as.integer(season %||% NA_integer_),
    year = as.integer(year %||% NA_integer_),
    book = as.character(book),
    bet_type = as.character(o$bet_type %||% ""),
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

parse_matchups_single <- function(dat, book, year) {
  if (is.null(dat) || length(dat) == 0) return(NULL)
  if (is.list(dat) && !is.data.frame(dat) && "odds" %in% names(dat)) {
    event_id <- as.character(dat$event_id %||% NA_character_)
    event_name <- as.character(dat$event_name %||% NA_character_)
    event_completed <- as.character(dat$event_completed %||% NA_character_)
    season <- as.integer(dat$season %||% NA_integer_)
    odds <- dat$odds
    if (is.null(odds) || length(odds) == 0) return(NULL)
    if (is.data.frame(odds)) {
      odds <- lapply(seq_len(nrow(odds)), function(i) as.list(odds[i, , drop = FALSE]))
    }
    # Normalize each element to a list (API may return mixed types when flatten=TRUE)
    odds <- lapply(odds, function(o) {
      if (is.list(o) && !is.data.frame(o)) return(o)
      if (is.data.frame(o) && nrow(o) >= 1) return(as.list(o[1, , drop = FALSE]))
      return(NULL)
    })
    odds <- odds[!vapply(odds, is.null, logical(1))]
    if (length(odds) == 0) return(NULL)
    out_list <- lapply(odds, function(o) matchup_row_to_tibble(o, event_id, event_name, event_completed, season, year, book))
    out_list <- out_list[!vapply(out_list, is.null, logical(1))]
    if (length(out_list) == 0) return(NULL)
    return(dplyr::bind_rows(out_list) %>% dplyr::filter(!is.na(bet_type) | !is.na(p1_dg_id)))
  }
  if (is.list(dat) && !is.data.frame(dat)) {
    pieces <- lapply(dat, function(ev) parse_matchups_single(ev, book, year))
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(NULL)
    return(dplyr::bind_rows(pieces))
  }
  NULL
}

fetch_matchups_year <- function(year, book, tour = "pga") {
  res <- httr::GET(
    MATCHUPS_BASE,
    query = list(
      tour = tour, event_id = "all", year = year, book = book,
      odds_format = "decimal", file_format = "json", key = API_KEY
    )
  )
  if (httr::status_code(res) != 200) return(NULL)
  body <- httr::content(res, as = "text", encoding = "UTF-8")
  dat <- tryCatch(
    jsonlite::fromJSON(body, flatten = TRUE, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  parse_matchups_single(dat, book, year)
}

update_matchups_2026 <- function(try_year = TARGET_YEAR) {
  message("Target matchups CSV (read + write): ", PATH_MATCHUPS)
  if (!dir.exists(dirname(PATH_MATCHUPS))) {
    message("  ERROR: Directory does not exist. Create: ", dirname(PATH_MATCHUPS))
    return(invisible(0L))
  }
  message("Step 1: Reading historical_matchups_outcomes.csv into R ...")
  existing <- tibble::tibble()
  if (file.exists(PATH_MATCHUPS)) {
    existing <- tryCatch(
      readr::read_csv(PATH_MATCHUPS, show_col_types = FALSE),
      error = function(e) { message("  Read error: ", conditionMessage(e)); tibble::tibble() }
    )
  }
  message("  Read ", nrow(existing), " rows.")
  if (nrow(existing) > 0 && "year" %in% names(existing)) {
    existing <- existing %>% dplyr::filter(as.integer(year) != try_year)
    for (c in MATCHUPS_COL_ORDER) if (!c %in% names(existing)) existing[[c]] <- NA
    existing <- existing %>% dplyr::select(dplyr::all_of(MATCHUPS_COL_ORDER))
    # Coerce to match API types so bind_rows(existing, new_rows) doesn't fail
    existing <- existing %>% dplyr::mutate(
      event_id = as.character(event_id),
      event_name = as.character(event_name),
      event_completed = as.character(event_completed),
      season = as.integer(season),
      year = as.integer(year),
      book = as.character(book),
      bet_type = as.character(bet_type),
      open_time = as.character(open_time),
      close_time = as.character(close_time),
      tie_rule = as.character(tie_rule),
      p1_dg_id = as.integer(p1_dg_id),
      p1_player_name = as.character(p1_player_name),
      p1_open = as.numeric(p1_open),
      p1_close = as.numeric(p1_close),
      p1_outcome = as.numeric(p1_outcome),
      p1_outcome_text = as.character(p1_outcome_text),
      p2_dg_id = as.integer(p2_dg_id),
      p2_player_name = as.character(p2_player_name),
      p2_open = as.numeric(p2_open),
      p2_close = as.numeric(p2_close),
      p2_outcome = as.numeric(p2_outcome),
      p2_outcome_text = as.character(p2_outcome_text),
      p3_dg_id = as.integer(p3_dg_id),
      p3_player_name = as.character(p3_player_name),
      p3_open = as.numeric(p3_open),
      p3_close = as.numeric(p3_close),
      p3_outcome = as.numeric(p3_outcome),
      p3_outcome_text = as.character(p3_outcome_text)
    )
  }

  message("Step 2: Fetching ", try_year, " PGA matchups from API (all books) ...")
  all_new <- list()
  for (bk in MATCHUPS_BOOKS) {
    Sys.sleep(REQUEST_DELAY)
    raw <- fetch_matchups_year(try_year, bk)
    if (is.null(raw) || nrow(raw) == 0) next
    all_new[[length(all_new) + 1L]] <- raw
  }
  new_rows <- if (length(all_new) > 0) {
    dplyr::bind_rows(all_new) %>% dplyr::filter(!is.na(event_id) | !is.na(p1_dg_id) | !is.na(book))
  } else {
    tibble::tibble()
  }
  n_new <- nrow(new_rows)
  if (n_new > 0) {
    message("  Fetched ", n_new, " matchup rows for ", try_year, ".")
    # Coerce to same types as existing so bind_rows succeeds
    new_rows <- new_rows %>% dplyr::mutate(
      event_id = as.character(event_id),
      event_name = as.character(event_name),
      event_completed = as.character(event_completed),
      season = as.integer(season),
      year = as.integer(year),
      book = as.character(book),
      bet_type = as.character(bet_type),
      open_time = as.character(open_time),
      close_time = as.character(close_time),
      tie_rule = as.character(tie_rule),
      p1_dg_id = as.integer(p1_dg_id),
      p1_player_name = as.character(p1_player_name),
      p1_open = as.numeric(p1_open),
      p1_close = as.numeric(p1_close),
      p1_outcome = as.numeric(p1_outcome),
      p1_outcome_text = as.character(p1_outcome_text),
      p2_dg_id = as.integer(p2_dg_id),
      p2_player_name = as.character(p2_player_name),
      p2_open = as.numeric(p2_open),
      p2_close = as.numeric(p2_close),
      p2_outcome = as.numeric(p2_outcome),
      p2_outcome_text = as.character(p2_outcome_text),
      p3_dg_id = as.integer(p3_dg_id),
      p3_player_name = as.character(p3_player_name),
      p3_open = as.numeric(p3_open),
      p3_close = as.numeric(p3_close),
      p3_outcome = as.numeric(p3_outcome),
      p3_outcome_text = as.character(p3_outcome_text)
    )
  } else {
    message("  No ", try_year, " matchups from API (all book calls failed or empty).")
  }

  combined <- dplyr::bind_rows(existing, new_rows)
  for (c in MATCHUPS_COL_ORDER) if (!c %in% names(combined)) combined[[c]] <- NA
  combined <- combined %>% dplyr::select(dplyr::all_of(MATCHUPS_COL_ORDER))
  if (nrow(combined) > 0) {
    combined <- combined %>% dplyr::arrange(dplyr::desc(year), event_id, book)
  }

  message("Step 3: Writing to: ", PATH_MATCHUPS)
  err <- tryCatch(
    { readr::write_csv(combined, PATH_MATCHUPS, na = ""); NULL },
    error = function(e) e
  )
  if (!is.null(err)) {
    message("  WRITE FAILED: ", conditionMessage(err))
    return(invisible(n_new))
  }
  fi <- file.info(PATH_MATCHUPS)
  message("  OK: Wrote ", nrow(combined), " rows (", n_new, " new for ", try_year, ").")
  message("  File updated: ", PATH_MATCHUPS, "  Modified: ", format(fi$mtime, "%Y-%m-%d %H:%M:%S"))
  assign("historical_matchups", combined, envir = .GlobalEnv)
  invisible(n_new)
}

# ---- Historical Outrights: 2026 PGA -> historical_outrights_outcomes.csv ----
# API: https://feeds.datagolf.com/historical-odds/outrights?tour=pga&event_id=all&year=&market=&book=&odds_format=american&file_format=json&key=...
OUTRIGHTS_BASE <- "https://feeds.datagolf.com/historical-odds/outrights"
OUTRIGHTS_BOOKS <- c(
  "bet365", "betcris", "betmgm", "betonline", "betway", "bovada",
  "caesars", "corale", "circa", "draftkings", "fanduel", "pinnacle",
  "skybet", "sportsbook", "unibet", "williamhill"
)
OUTRIGHTS_MARKETS <- c("win", "top_5", "top_10", "top_20", "make_cut", "mc")
OUTRIGHTS_COL_ORDER <- c(
  "event_id", "event_name", "event_completed", "season", "year", "book", "market",
  "dg_id", "player_name", "open_odds", "close_odds", "open_time", "close_time",
  "bet_outcome_numeric", "bet_outcome_text", "outcome"
)

outright_row_to_tibble <- function(o, event_id, event_name, event_completed, season, year, book, market) {
  if (!is.list(o) || is.data.frame(o)) return(NULL)
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

parse_outrights_single <- function(dat, book, year, market) {
  if (is.null(dat) || length(dat) == 0) return(NULL)
  if (is.list(dat) && !is.data.frame(dat) && "odds" %in% names(dat)) {
    event_id <- as.character(dat$event_id %||% NA_character_)
    event_name <- as.character(dat$event_name %||% NA_character_)
    event_completed <- as.character(dat$event_completed %||% NA_character_)
    season <- as.integer(dat$season %||% NA_integer_)
    odds <- dat$odds
    if (is.null(odds) || length(odds) == 0) return(NULL)
    if (is.data.frame(odds)) {
      odds <- lapply(seq_len(nrow(odds)), function(i) as.list(odds[i, , drop = FALSE]))
    }
    odds <- lapply(odds, function(o) {
      if (is.list(o) && !is.data.frame(o)) return(o)
      if (is.data.frame(o) && nrow(o) >= 1) return(as.list(o[1, , drop = FALSE]))
      return(NULL)
    })
    odds <- odds[!vapply(odds, is.null, logical(1))]
    if (length(odds) == 0) return(NULL)
    out_list <- lapply(odds, function(o) outright_row_to_tibble(o, event_id, event_name, event_completed, season, year, book, market))
    out_list <- out_list[!vapply(out_list, is.null, logical(1))]
    if (length(out_list) == 0) return(NULL)
    return(dplyr::bind_rows(out_list) %>% dplyr::filter(!is.na(dg_id) | !is.na(player_name)))
  }
  if (is.list(dat) && !is.data.frame(dat)) {
    pieces <- lapply(dat, function(ev) parse_outrights_single(ev, book, year, market))
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(NULL)
    return(dplyr::bind_rows(pieces))
  }
  NULL
}

fetch_outrights_year <- function(year, book, market, tour = "pga") {
  res <- httr::GET(
    OUTRIGHTS_BASE,
    query = list(
      tour = tour, event_id = "all", year = year, market = market, book = book,
      odds_format = "american", file_format = "json", key = API_KEY
    )
  )
  if (httr::status_code(res) != 200) return(NULL)
  body <- httr::content(res, as = "text", encoding = "UTF-8")
  dat <- tryCatch(
    jsonlite::fromJSON(body, flatten = TRUE, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  parse_outrights_single(dat, book, year, market)
}

update_outrights_2026 <- function(try_year = TARGET_YEAR) {
  message("Target outrights CSV (read + write): ", PATH_OUTRIGHTS)
  if (!dir.exists(dirname(PATH_OUTRIGHTS))) {
    message("  ERROR: Directory does not exist. Create: ", dirname(PATH_OUTRIGHTS))
    return(invisible(0L))
  }
  message("Step 1: Reading historical_outrights_outcomes.csv into R ...")
  existing <- tibble::tibble()
  if (file.exists(PATH_OUTRIGHTS)) {
    existing <- tryCatch(
      readr::read_csv(PATH_OUTRIGHTS, show_col_types = FALSE),
      error = function(e) { message("  Read error: ", conditionMessage(e)); tibble::tibble() }
    )
  }
  message("  Read ", nrow(existing), " rows.")
  if (nrow(existing) > 0 && "year" %in% names(existing)) {
    existing <- existing %>% dplyr::filter(as.integer(year) != try_year)
    for (c in OUTRIGHTS_COL_ORDER) if (!c %in% names(existing)) existing[[c]] <- NA
    existing <- existing %>% dplyr::select(dplyr::all_of(OUTRIGHTS_COL_ORDER))
    existing <- existing %>% dplyr::mutate(
      event_id = as.character(event_id),
      event_name = as.character(event_name),
      event_completed = as.character(event_completed),
      season = as.integer(season),
      year = as.integer(year),
      book = as.character(book),
      market = as.character(market),
      dg_id = as.integer(dg_id),
      player_name = as.character(player_name),
      open_odds = as.numeric(open_odds),
      close_odds = as.numeric(close_odds),
      open_time = as.character(open_time),
      close_time = as.character(close_time),
      bet_outcome_numeric = as.numeric(bet_outcome_numeric),
      bet_outcome_text = as.character(bet_outcome_text),
      outcome = as.character(outcome)
    )
  }

  message("Step 2: Fetching ", try_year, " PGA outrights from API (all books × markets) ...")
  all_new <- list()
  for (bk in OUTRIGHTS_BOOKS) {
    for (mkt in OUTRIGHTS_MARKETS) {
      Sys.sleep(REQUEST_DELAY)
      raw <- fetch_outrights_year(try_year, bk, mkt)
      if (is.null(raw) || nrow(raw) == 0) next
      all_new[[length(all_new) + 1L]] <- raw
    }
  }
  new_rows <- if (length(all_new) > 0) {
    dplyr::bind_rows(all_new) %>% dplyr::filter(!is.na(event_id) | !is.na(dg_id) | !is.na(book))
  } else {
    tibble::tibble()
  }
  n_new <- nrow(new_rows)
  if (n_new > 0) {
    message("  Fetched ", n_new, " outright rows for ", try_year, ".")
    new_rows <- new_rows %>% dplyr::mutate(
      event_id = as.character(event_id),
      event_name = as.character(event_name),
      event_completed = as.character(event_completed),
      season = as.integer(season),
      year = as.integer(year),
      book = as.character(book),
      market = as.character(market),
      dg_id = as.integer(dg_id),
      player_name = as.character(player_name),
      open_odds = as.numeric(open_odds),
      close_odds = as.numeric(close_odds),
      open_time = as.character(open_time),
      close_time = as.character(close_time),
      bet_outcome_numeric = as.numeric(bet_outcome_numeric),
      bet_outcome_text = as.character(bet_outcome_text),
      outcome = as.character(outcome)
    )
  } else {
    message("  No ", try_year, " outrights from API (all book/market calls failed or empty).")
  }

  combined <- dplyr::bind_rows(existing, new_rows)
  for (c in OUTRIGHTS_COL_ORDER) if (!c %in% names(combined)) combined[[c]] <- NA
  combined <- combined %>% dplyr::select(dplyr::all_of(OUTRIGHTS_COL_ORDER))
  if (nrow(combined) > 0) {
    combined <- combined %>% dplyr::arrange(dplyr::desc(year), event_id, book, market)
  }

  message("Step 3: Writing to: ", PATH_OUTRIGHTS)
  err <- tryCatch(
    { readr::write_csv(combined, PATH_OUTRIGHTS, na = ""); NULL },
    error = function(e) e
  )
  if (!is.null(err)) {
    message("  WRITE FAILED: ", conditionMessage(err))
    return(invisible(n_new))
  }
  fi <- file.info(PATH_OUTRIGHTS)
  message("  OK: Wrote ", nrow(combined), " rows (", n_new, " new for ", try_year, ").")
  message("  File updated: ", PATH_OUTRIGHTS, "  Modified: ", format(fi$mtime, "%Y-%m-%d %H:%M:%S"))
  assign("historical_outrights", combined, envir = .GlobalEnv)
  invisible(n_new)
}

# Run: read all three CSVs into R, add 2026 PGA data, write back in place.
# After running: historical_rounds, historical_matchups, historical_outrights are in .GlobalEnv.

#' Locate Rscript when not on PATH (typical on Windows).
find_rscript_exe <- function() {
  rs <- Sys.which("Rscript")
  if (nzchar(rs)) return(rs)
  rh <- Sys.getenv("R_HOME", unset = "")
  if (nzchar(rh)) {
    exe <- file.path(rh, "bin", if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    if (file.exists(exe)) return(exe)
  }
  if (.Platform$OS.type == "windows") {
    pf <- Sys.getenv("ProgramFiles", "C:/Program Files")
    rroot <- file.path(pf, "R")
    if (dir.exists(rroot)) {
      subs <- list.dirs(rroot, full.names = TRUE, recursive = FALSE)
      subs <- subs[grepl("^R-[0-9]", basename(subs))]
      if (length(subs)) {
        subs <- sort(subs, decreasing = TRUE)
        exe <- file.path(subs[[1]], "bin", "Rscript.exe")
        if (file.exists(exe)) return(exe)
      }
    }
  }
  ""
}

#' After CSVs refresh, rebuild alpha-caddie-web/player_round_history.json from
#' data/historical_rounds_all.csv (+ hole_data) via npm run build:history.
#' Optional: set env ALPHA_CADDIE_PGA_HISTORY=1 to then overwrite with pgatouR scorecards.
maybe_refresh_alpha_caddie_web_history <- function() {
  web_dir <- file.path(MODEL_DIR, "alpha-caddie-web")
  if (!dir.exists(web_dir)) {
    return(invisible(NULL))
  }
  npm_cmd <- Sys.which("npm")
  if (!nzchar(npm_cmd)) {
    message("npm not on PATH — skipped alpha-caddie-web player history (run: npm run build:history)")
    return(invisible(NULL))
  }
  message("Refreshing alpha-caddie-web player history (npm run build:history, DataGolf CSV) ...")
  out <- suppressWarnings(
    system2(
      npm_cmd,
      args = c("run", "build:history"),
      stdout = TRUE,
      stderr = TRUE,
      wait = TRUE,
      cwd = web_dir
    )
  )
  st <- attr(out, "status")
  if (!is.null(st) && !identical(as.integer(st), 0L)) {
    message(paste(c("  build:history exited non-zero:", out), collapse = "\n"))
  } else if (length(out)) {
    message(paste(" ", out, collapse = "\n"))
  }

  if (identical(Sys.getenv("ALPHA_CADDIE_PGA_HISTORY", "0"), "1")) {
    pga_script <- normalizePath(file.path(MODEL_DIR, "scripts", "build_alpha_caddie_web_history_pga.R"), winslash = "/", mustWork = FALSE)
    rscript <- find_rscript_exe()
    if (file.exists(pga_script) && nzchar(rscript) && requireNamespace("pgatouR", quietly = TRUE)) {
      message("ALPHA_CADDIE_PGA_HISTORY=1: overwriting with pgatouR pga_scorecard history ...")
      repo_arg <- normalizePath(MODEL_DIR, winslash = "/", mustWork = TRUE)
      out2 <- suppressWarnings(
        system2(
          rscript,
          args = c(pga_script, repo_arg),
          stdout = TRUE,
          stderr = TRUE,
          wait = TRUE
        )
      )
      st2 <- attr(out2, "status")
      if (!is.null(st2) && !identical(as.integer(st2), 0L)) {
        message("  PGA history script failed (status ", st2, ")")
        if (length(out2)) message(paste(out2, collapse = "\n"))
      } else if (length(out2)) {
        message(paste(out2, collapse = "\n"))
      }
    }
  }
  invisible(NULL)
}

#' @param try_year Year for matchups/outrights tables (default 2026). Rounds CSV uses
#'   \code{update_historical_rounds_live()} (current + prior calendar year + current event).
live_updates_2026 <- function(try_year = TARGET_YEAR) {
  if (exists("update_historical_rounds_live", mode = "function")) {
    message("Rounds: update_historical_rounds_live() -> ", PATH_ROUNDS)
    update_historical_rounds_live()
  } else {
    message("Rounds: fallback update_rounds_2026(", try_year, ")")
    update_rounds_2026(try_year)
  }
  update_matchups_2026(try_year)
  update_outrights_2026(try_year)
  maybe_refresh_alpha_caddie_web_history()
}
