# live_update_all.R
# Refreshes model inputs: DataGolf historical CSVs + latest PGA shot-by-shot data.
# Safe to run repeatedly. Rounds/outrights/matchups: full replace per calendar year from DataGolf
# (default years = current + previous — keeps 2026 complete during 2026 and refreshes into 2027).
# Shots: append the most recent tournament via pgatouR (helper.R) if not already in the CSV.
#
# 1) historical_rounds_all.csv
#    API: Historical Raw Data - Round Scoring, Stats & Strokes Gained
#    https://feeds.datagolf.com/historical-raw-data/rounds
#    Params: tour=pga, event_id=all, year=2026, file_format=csv|json, key=...
#    (Web: npm run update:rounds uses Node for PGA+LIV — no R.)
#    Target: C:\Users\student\Documents\golfModel\data\historical_rounds_all.csv
#
# 2) historical_outrights_outcomes.csv
#    API: Historical Outrights (win, top_5, top_10, top_20, make_cut, mc at multiple books)
#    https://feeds.datagolf.com/historical-odds/outrights
#    Params: tour=pga, event_id=all, year=2026, market=, book=, odds_format=american, key=...
#    Target: C:\Users\student\Documents\golfModel\data\historical_outrights_outcomes.csv
#
# 3) historical_matchups_outcomes.csv
#    API: Historical Match-Ups (round match-ups, 3-balls, etc.)
#    https://feeds.datagolf.com/historical-odds/matchups
#    Params: tour=pga, event_id=all, year=2026, book=, odds_format=decimal, key=...
#    Target: C:\Users\student\Documents\golfModel\data\historical_matchups_outcomes.csv
#    Only adds 2026 rows if not already present (existing year-2026 rows replaced by fresh fetch).
#
# 4) data/all_shots_2021_2026.csv (+ data/all_shots_progress.rds)
#    pgatouR only (not DataGolf): backfill tournaments with schedule end date after the latest
#    end date among IDs already in the CSV, through today. See helper.R: backfill_shots_after_csv_anchor().

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
suppressPackageStartupMessages(library(purrr))
library(readr)
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

# Canonical paths (app.R and pipeline use these)
MODEL_DIR <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd()
PATH_ROUNDS    <- normalizePath(file.path(MODEL_DIR, "data", "historical_rounds_all.csv"), mustWork = FALSE)
PATH_OUTRIGHTS <- normalizePath(file.path(MODEL_DIR, "data", "historical_outrights_outcomes.csv"), mustWork = FALSE)
PATH_MATCHUPS  <- normalizePath(file.path(MODEL_DIR, "data", "historical_matchups_outcomes.csv"), mustWork = FALSE)

API_KEY <- Sys.getenv("DATAGOLF_API_KEY", "")
REQUEST_DELAY <- 0.5

#' Which calendar years to pull from DataGolf each run (full replace per year in each CSV).
#' Default: \strong{current year} and \strong{prior year} — catches new tournaments, late SG updates,
#' and settled odds without missing 2026 (or any season in progress).
#' Optional env \code{GOLF_DATAGOLF_YEARS}: comma-separated years, e.g. \code{2026} or \code{2025,2026}
#' to force extra seasons (still clipped to the rolling 5-year window kept in \code{historical_rounds_all.csv}).
datagolf_years_to_update <- function() {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  min_y <- cy - 4L
  yrs <- c(cy, cy - 1L)
  extra <- Sys.getenv("GOLF_DATAGOLF_YEARS", "")
  if (nzchar(trimws(extra))) {
    parts <- strsplit(extra, "[,;\\s]+", perl = TRUE)[[1]]
    parts <- suppressWarnings(as.integer(parts))
    parts <- parts[!is.na(parts)]
    yrs <- c(yrs, parts)
  }
  yrs <- unique(as.integer(yrs))
  yrs <- yrs[!is.na(yrs) & yrs >= min_y & yrs <= cy]
  sort(unique(yrs))
}

# Retry transient DataGolf failures (rate limits, gateway errors).
http_get_retry <- function(url, query, max_attempts = 4L, label = "") {
  last <- NULL
  for (attempt in seq_len(max_attempts)) {
    last <- tryCatch(httr::GET(url, query = query), error = function(e) NULL)
    if (is.null(last)) {
      message("  HTTP error (", label, ") attempt ", attempt, ": connection failed; retrying...")
      Sys.sleep(min(2^attempt, 30))
      next
    }
    st <- httr::status_code(last)
    if (st == 200L) return(last)
    if (st %in% c(429L, 500L, 502L, 503L, 504L)) {
      message("  HTTP ", st, " (", label, ") attempt ", attempt, "; retrying...")
      Sys.sleep(min(2^attempt, 30))
      next
    }
    return(last)
  }
  last
}

# Show file status (exists, size, mtime)
report_file <- function(label, path) {
  if (file.exists(path)) {
    fi <- file.info(path)
    message("  ", label, ": ", path, " (", fi$size, " bytes, mtime ", format(fi$mtime, "%Y-%m-%d %H:%M:%S"), ")")
  } else {
    message("  ", label, ": ", path, " (file does not exist yet)")
  }
}

# ---- 1) Rounds: Historical Raw Data / rounds -> historical_rounds_all.csv ----
update_rounds_2026 <- function(try_year) {
  message("Live update: rounds (year ", try_year, ") -> ", PATH_ROUNDS)
  out_dir <- dirname(PATH_ROUNDS)
  if (!dir.exists(out_dir)) {
    message("  Creating dir: ", out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = TRUE)
  }
  live_path <- file.path(MODEL_DIR, "live_data.R")
  if (!file.exists(live_path)) {
    message("  live_data.R not found at ", live_path, "; skip rounds.")
    return(invisible(0L))
  }
  source(live_path, local = FALSE)
  year_rounds <- fetch_year_rounds(try_year, "pga")
  if (is.null(year_rounds) || nrow(year_rounds) == 0) {
    message("  No ", try_year, " rounds from API (HTTP/parse may have failed or year not available).")
    return(invisible(0L))
  }
  message("  Fetched ", nrow(year_rounds), " rounds for ", try_year, ".")

  existing <- tibble::tibble()
  if (file.exists(PATH_ROUNDS)) {
    existing <- tryCatch(
      readr::read_csv(PATH_ROUNDS, show_col_types = FALSE),
      error = function(e) { message("  Read error: ", conditionMessage(e)); tibble::tibble() }
    )
  }
  if (nrow(existing) > 0 && "year" %in% names(existing)) {
    existing <- existing %>% dplyr::mutate(
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
    for (c in COL_ORDER_LIVE) if (!c %in% names(existing)) existing[[c]] <- NA
    existing <- existing %>% dplyr::select(dplyr::any_of(COL_ORDER_LIVE))
  }
  year_rounds <- year_rounds %>%
    dplyr::mutate(
      event_id = as.character(event_id),
      year = as.integer(year),
      event_completed = event_completed_chr_for_csv(.data$event_completed),
      event_name = as.character(event_name),
      course_name = as.character(course_name),
      teetime = teetime_chr_for_csv(.data$teetime)
    )
  for (c in COL_ORDER_LIVE) if (!c %in% names(year_rounds)) year_rounds[[c]] <- NA
  year_rounds <- year_rounds %>% dplyr::select(dplyr::any_of(COL_ORDER_LIVE))
  combined <- dplyr::bind_rows(existing, year_rounds)
  # Keep only last 5 seasons in file
  current_yr <- as.integer(format(Sys.Date(), "%Y"))
  min_year <- current_yr - 4L
  if ("year" %in% names(combined)) {
    combined <- combined %>% dplyr::filter(as.integer(year) >= min_year)
  }
  combined <- combined %>% dplyr::arrange(dplyr::desc(year), event_id, dg_id, round_num)
  err <- tryCatch(
    { readr::write_csv(combined, PATH_ROUNDS); NULL },
    error = function(e) e
  )
  if (!is.null(err)) {
    message("  Write error: ", conditionMessage(err))
    return(invisible(0L))
  }
  message("  Wrote ", nrow(combined), " rows (", nrow(year_rounds), " new for ", try_year, ").")
  report_file("  Rounds file now", PATH_ROUNDS)
  invisible(nrow(year_rounds))
}

# ---- 2) Outrights: Historical Outrights API -> historical_outrights_outcomes.csv ----
OUTRIGHTS_BASE <- "https://feeds.datagolf.com/historical-odds/outrights"
OUTRIGHTS_BOOKS <- c(
  "bet365", "betcris", "betmgm", "betonline", "betway", "bovada",
  "caesars", "corale", "circa", "draftkings", "fanduel", "pinnacle",
  "skybet", "sportsbook", "unibet", "williamhill"
)
OUTRIGHTS_MARKETS <- c("win", "top_5", "top_10", "top_20", "make_cut", "mc")

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

parse_outrights_single <- function(dat, book, year, market) {
  if (is.null(dat) || length(dat) == 0) return(NULL)
  if (is.list(dat) && !is.data.frame(dat) && "odds" %in% names(dat)) {
    event_id <- as.character(dat$event_id %||% NA_character_)
    event_name <- as.character(dat$event_name %||% NA_character_)
    event_completed <- as.character(dat$event_completed %||% NA_character_)
    season <- as.integer(dat$season %||% NA_integer_)
    odds <- dat$odds
    if (is.null(odds) || length(odds) == 0) return(NULL)
    if (is.data.frame(odds)) odds <- lapply(seq_len(nrow(odds)), function(i) as.list(odds[i, ]))
    out_list <- map(odds, function(o) outright_row_to_tibble(o, event_id, event_name, event_completed, season, year, book, market))
    out_list <- out_list[!vapply(out_list, is.null, logical(1))]
    if (length(out_list) == 0) return(NULL)
    out <- bind_rows(out_list) %>% filter(!is.na(dg_id) | !is.na(player_name))
    return(out)
  }
  if (is.list(dat) && !is.data.frame(dat)) {
    pieces <- map(dat, function(ev) parse_outrights_single(ev, book, year, market))
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(NULL)
    return(bind_rows(pieces))
  }
  NULL
}

fetch_outrights_year <- function(year, book, market, tour = "pga", verbose = FALSE) {
  res <- http_get_retry(
    OUTRIGHTS_BASE,
    query = list(
      tour = tour, event_id = "all", year = year, market = market, book = book,
      odds_format = "american", file_format = "json", key = API_KEY
    ),
    label = paste0("outrights ", book, " ", market, " ", year)
  )
  if (is.null(res)) return(NULL)
  st <- status_code(res)
  if (verbose && st != 200) message("    ", book, " ", market, " ", year, ": HTTP ", st)
  if (st != 200) return(NULL)
  dat <- tryCatch(
    fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE),
    error = function(e) { if (verbose) message("    parse error: ", conditionMessage(e)); NULL }
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  parse_outrights_single(dat, book, year, market)
}

update_outrights_2026 <- function(try_year) {
  message("Live update: outrights (year ", try_year, ") -> ", PATH_OUTRIGHTS)
  out_dir <- dirname(PATH_OUTRIGHTS)
  if (!dir.exists(out_dir)) { dir.create(out_dir, recursive = TRUE, showWarnings = TRUE); message("  Created ", out_dir) }
  all_new <- list()
  for (bk in OUTRIGHTS_BOOKS) {
    for (mkt in OUTRIGHTS_MARKETS) {
      Sys.sleep(REQUEST_DELAY)
      raw <- fetch_outrights_year(try_year, bk, mkt, verbose = (length(all_new) == 0L))
      if (is.null(raw) || nrow(raw) == 0) next
      all_new[[length(all_new) + 1L]] <- raw
    }
  }
  new_rows <- if (length(all_new) > 0) bind_rows(all_new) %>% filter(!is.na(event_id) | !is.na(dg_id) | !is.na(book)) else tibble()
  if (nrow(new_rows) == 0) {
    message("  No ", try_year, " outrights from API (all book/market calls failed or empty).")
    return(invisible(0L))
  }
  message("  Fetched ", nrow(new_rows), " outright rows for ", try_year, ".")
  # Coerce new_rows to canonical types so bind_rows with existing never fails
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
  existing <- tibble()
  if (file.exists(PATH_OUTRIGHTS)) {
    existing <- tryCatch(readr::read_csv(PATH_OUTRIGHTS, show_col_types = FALSE), error = function(e) tibble())
  }
  if (nrow(existing) > 0 && "year" %in% names(existing)) {
    existing <- existing %>% dplyr::filter(as.integer(year) != try_year) %>%
      dplyr::mutate(
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
  combined <- dplyr::bind_rows(existing, new_rows)
  err <- tryCatch(
    { readr::write_csv(combined, PATH_OUTRIGHTS, na = ""); NULL },
    error = function(e) e
  )
  if (!is.null(err)) {
    message("  Write error: ", conditionMessage(err))
    return(invisible(0L))
  }
  message("  Wrote ", nrow(combined), " rows (", nrow(new_rows), " new for ", try_year, ").")
  report_file("  Outrights file now", PATH_OUTRIGHTS)
  invisible(nrow(new_rows))
}

# ---- 3) Matchups: Historical Match-Ups API -> historical_matchups_outcomes.csv (2026 only if not already there) ----
MATCHUPS_BASE <- "https://feeds.datagolf.com/historical-odds/matchups"
MATCHUPS_BOOKS <- c(
  "5dimes", "bet365", "betcris", "betmgm", "betonline", "bovada",
  "caesars", "circa", "draftkings", "fanduel", "pinnacle",
  "sportsbook", "williamhill", "unibet"
)

matchup_row_to_tibble <- function(o, event_id, event_name, event_completed, season, year, book) {
  tibble(
    event_id = as.character(event_id),
    event_name = as.character(event_name),
    event_completed = as.character(event_completed),
    season = as.integer(season %||% NA),
    year = as.integer(year %||% NA),
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
    if (is.data.frame(odds)) odds <- lapply(seq_len(nrow(odds)), function(i) as.list(odds[i, ]))
    out_list <- map(odds, function(o) matchup_row_to_tibble(o, event_id, event_name, event_completed, season, year, book))
    out_list <- out_list[!vapply(out_list, is.null, logical(1))]
    if (length(out_list) == 0) return(NULL)
    return(bind_rows(out_list) %>% filter(!is.na(bet_type) | !is.na(p1_dg_id)))
  }
  if (is.list(dat) && !is.data.frame(dat)) {
    pieces <- map(dat, function(ev) parse_matchups_single(ev, book, year))
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(NULL)
    return(bind_rows(pieces))
  }
  NULL
}

fetch_matchups_year <- function(year, book, tour = "pga", verbose = FALSE) {
  res <- http_get_retry(
    MATCHUPS_BASE,
    query = list(tour = tour, event_id = "all", year = year, book = book,
                 odds_format = "decimal", file_format = "json", key = API_KEY),
    label = paste0("matchups ", book, " ", year)
  )
  if (is.null(res)) return(NULL)
  st <- status_code(res)
  if (verbose && st != 200) message("    ", book, " ", year, ": HTTP ", st)
  if (st != 200) return(NULL)
  dat <- tryCatch(
    fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE),
    error = function(e) { if (verbose) message("    parse error: ", conditionMessage(e)); NULL }
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  parse_matchups_single(dat, book, year)
}

update_matchups_2026 <- function(try_year) {
  message("Live update: matchups (year ", try_year, ") -> ", PATH_MATCHUPS, " (full replace for this year)")
  out_dir <- dirname(PATH_MATCHUPS)
  if (!dir.exists(out_dir)) { dir.create(out_dir, recursive = TRUE, showWarnings = TRUE); message("  Created ", out_dir) }
  all_new <- list()
  for (bk in MATCHUPS_BOOKS) {
    Sys.sleep(REQUEST_DELAY)
    raw <- fetch_matchups_year(try_year, bk, verbose = (length(all_new) == 0L))
    if (is.null(raw) || nrow(raw) == 0) next
    all_new[[length(all_new) + 1L]] <- raw
  }
  new_rows <- if (length(all_new) > 0) bind_rows(all_new) %>% filter(!is.na(event_id) | !is.na(p1_dg_id) | !is.na(book)) else tibble()
  if (nrow(new_rows) == 0) {
    message("  No ", try_year, " matchups from API (all book calls failed or empty).")
    return(invisible(0L))
  }
  message("  Fetched ", nrow(new_rows), " matchup rows for ", try_year, ".")
  existing <- tibble()
  if (file.exists(PATH_MATCHUPS)) {
    existing <- tryCatch(readr::read_csv(PATH_MATCHUPS, show_col_types = FALSE), error = function(e) tibble())
  }
  # Only add try_year data: drop any existing rows for this year so we don't duplicate (replace with fresh fetch)
  if (nrow(existing) > 0 && "year" %in% names(existing)) {
    existing <- existing %>% dplyr::filter(as.integer(year) != try_year)
  }
  combined <- dplyr::bind_rows(existing, new_rows)
  err <- tryCatch(
    { readr::write_csv(combined, PATH_MATCHUPS, na = ""); NULL },
    error = function(e) e
  )
  if (!is.null(err)) {
    message("  Write error: ", conditionMessage(err))
    return(invisible(0L))
  }
  message("  Wrote ", nrow(combined), " rows (", nrow(new_rows), " new for ", try_year, ").")
  report_file("  Matchups file now", PATH_MATCHUPS)
  invisible(nrow(new_rows))
}

# ---- 4) Shot-by-shot (pgatouR only): backfill -> data/all_shots_2021_2026.csv ----
PATH_SHOTS <- normalizePath(file.path(MODEL_DIR, "data", "all_shots_2021_2026.csv"), mustWork = FALSE)
PATH_SHOTS_PROGRESS <- normalizePath(file.path(MODEL_DIR, "data", "all_shots_progress.rds"), mustWork = FALSE)

#' Backfill shot rows after latest schedule date in CSV through today (pgatouR). Continues on failure.
#' @return Character summary for logging (e.g. "3 events, 12,345 rows" or "failed: ...").
update_shots_latest <- function() {
  message("Live update: shot-by-shot (pgatouR backfill) -> ", PATH_SHOTS)
  helper_path <- file.path(MODEL_DIR, "helper.R")
  if (!file.exists(helper_path)) {
    message("  helper.R not found at ", helper_path, "; skip shots.")
    return("helper missing")
  }
  out <- tryCatch(
    {
      helper_env <- new.env(parent = globalenv())
      sys.source(helper_path, envir = helper_env)
      fn <- helper_env[["backfill_shots_after_csv_anchor"]]
      if (!is.function(fn)) {
        message("  backfill_shots_after_csv_anchor not found; skip shots.")
        return("no function")
      }
      fn(
        output_csv = PATH_SHOTS,
        progress_rds = PATH_SHOTS_PROGRESS,
        years = NULL,
        rounds = 1:4,
        sleep_seconds = 0.03,
        max_events = NULL,
        force = FALSE
      )
    },
    error = function(e) {
      message("  Shot append error: ", conditionMessage(e))
      paste0("error: ", conditionMessage(e))
    }
  )
  if (is.list(out)) {
    if (isTRUE(out$skipped)) return("skipped (already in CSV)")
    if (!is.null(out$events) && !is.null(out$rows)) {
      return(paste0(out$events, " event(s), ", format(out$rows, big.mark = ","), " rows"))
    }
    if (!is.null(out$rows)) return(paste0(format(out$rows, big.mark = ","), " rows"))
  }
  if (is.character(out) && length(out) == 1L) return(out)
  "done"
}

# ---- Run all updates (DataGolf: current + prior year by default; see datagolf_years_to_update) ----
#' Refresh DataGolf historical files (rounds, outrights, matchups) and latest shot-by-shot CSV.
#' Pulls a \strong{full replace} from DataGolf for each year in \code{datagolf_years_to_update()}
#' (default: this calendar year and last year, so 2026 stays current all season and into early 2027).
#' Safe to run repeatedly. Requires \code{DATAGOLF_API_KEY} for DataGolf; \code{pgatouR} for shots.
live_update_all_2026 <- function() {
  if (!nzchar(API_KEY)) {
    message("WARNING: DATAGOLF_API_KEY is not set — DataGolf API calls will likely fail.")
  }
  years <- datagolf_years_to_update()
  message("=== Live update: DataGolf (rounds, outrights, matchups) + shot-by-shot ===")
  message("DataGolf years to refresh (full API pull per year): ", paste(years, collapse = ", "))
  if (nzchar(Sys.getenv("GOLF_DATAGOLF_YEARS", ""))) {
    message("(Note: GOLF_DATAGOLF_YEARS is set — extra years merged into the list above.)")
  }
  message("Paths:")
  report_file("  rounds", PATH_ROUNDS)
  report_file("  outrights", PATH_OUTRIGHTS)
  report_file("  matchups", PATH_MATCHUPS)
  report_file("  shots", PATH_SHOTS)

  n_r <- 0L
  n_o <- 0L
  n_m <- 0L
  for (y in years) {
    message("--- DataGolf year ", y, " ---")
    n_r <- n_r + as.integer(update_rounds_2026(y))
    n_o <- n_o + as.integer(update_outrights_2026(y))
    n_m <- n_m + as.integer(update_matchups_2026(y))
  }

  shot_summary <- update_shots_latest()

  message("=== Done (rounds rows pulled this run: ", n_r, ", outrights: ", n_o, ", matchups: ", n_m, "; shots: ", shot_summary, ") ===")
}

#' Alias for \code{live_update_all_2026()} (same function).
live_update_all <- live_update_all_2026
