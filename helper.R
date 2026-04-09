library(pgatouR)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Find the first matching column name in a data frame.
pick_col <- function(df, candidates) {
  nms <- names(df)
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

# Build one schedule data frame across multiple years.
get_schedules <- function(years = 2021:2026) {
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    yr <- years[[i]]
    message("Fetching schedule for ", yr, "...")
    sch <- pga_schedule(yr)
    sch$season <- yr
    out[[i]] <- sch
  }
  do.call(rbind, out)
}

# Get player IDs that actually appear in a tournament field.
get_tournament_player_ids <- function(tournament_id) {
  lb <- tryCatch(pga_leaderboard(tournament_id), error = function(e) NULL)
  if (!is.null(lb) && is.data.frame(lb) && nrow(lb) > 0) {
    col <- pick_col(lb, c("player_id", "playerid", "id"))
    if (!is.na(col)) {
      ids <- unique(as.character(lb[[col]]))
      ids <- ids[!is.na(ids) & nzchar(ids)]
      if (length(ids) > 0) return(ids)
    }
  }

  # Fallback for events with no leaderboard rows yet.
  tt <- tryCatch(pga_tee_times(tournament_id), error = function(e) NULL)
  if (!is.null(tt) && is.data.frame(tt) && nrow(tt) > 0) {
    col <- pick_col(tt, c("player_id", "playerid", "id"))
    if (!is.na(col)) {
      ids <- unique(as.character(tt[[col]]))
      ids <- ids[!is.na(ids) & nzchar(ids)]
      if (length(ids) > 0) return(ids)
    }
  }

  character(0)
}

append_csv <- function(df, path) {
  if (is.null(path) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(
    df,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(path),
    append = file.exists(path),
    qmethod = "double"
  )
  invisible(NULL)
}

load_progress <- function(progress_rds) {
  if (!file.exists(progress_rds)) {
    return(list(
      completed_tournaments = character(0),
      attempts = 0L,
      successes = 0L,
      failures = 0L,
      rows_written = 0L
    ))
  }
  readRDS(progress_rds)
}

save_progress <- function(progress, progress_rds) {
  dir.create(dirname(progress_rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(progress, progress_rds)
}

# Optimized pull: only query players in each tournament field.
# Writes incremental CSV and progress file so you can resume.
get_all_shots <- function(
  years = 2021:2026,
  rounds = 1:4,
  output_csv = "data/all_shots_2021_2026.csv",
  progress_rds = "data/all_shots_progress.rds",
  sleep_seconds = 0.03,
  resume = TRUE
) {
  schedule_all <- get_schedules(years)
  event_id_col <- pick_col(schedule_all, c("tournament_id", "tournamentid", "event_id", "id"))
  event_name_col <- pick_col(schedule_all, c("tournament", "tournament_name", "event_name", "name"))

  if (is.na(event_id_col)) stop("Could not find a tournament ID column in pga_schedule().")

  schedule_all <- schedule_all[!is.na(schedule_all[[event_id_col]]) & nzchar(as.character(schedule_all[[event_id_col]])), , drop = FALSE]
  event_ids <- unique(as.character(schedule_all[[event_id_col]]))
  event_name_lookup <- if (!is.na(event_name_col)) {
    setNames(as.character(schedule_all[[event_name_col]]), as.character(schedule_all[[event_id_col]]))
  } else {
    NULL
  }

  progress <- if (resume) load_progress(progress_rds) else list(
    completed_tournaments = character(0),
    attempts = 0L,
    successes = 0L,
    failures = 0L,
    rows_written = 0L
  )

  if (!resume && file.exists(output_csv)) file.remove(output_csv)

  pending <- setdiff(event_ids, progress$completed_tournaments %||% character(0))
  message("Tournaments total: ", length(event_ids))
  message("Already completed: ", length(progress$completed_tournaments %||% character(0)))
  message("Pending: ", length(pending))
  message("Rounds per tournament: ", length(rounds))

  if (length(pending) == 0) {
    message("Nothing to do. All tournaments already completed.")
    return(invisible(progress))
  }

  for (ev in pending) {
    ev_name <- if (!is.null(event_name_lookup) && ev %in% names(event_name_lookup)) event_name_lookup[[ev]] else ""
    message("Tournament: ", ev, if (nzchar(ev_name)) paste0(" (", ev_name, ")") else "")

    player_ids <- get_tournament_player_ids(ev)
    message("  Field size: ", length(player_ids))
    if (length(player_ids) == 0) {
      progress$completed_tournaments <- unique(c(progress$completed_tournaments, ev))
      save_progress(progress, progress_rds)
      next
    }

    event_rows <- 0L
    for (rd in rounds) {
      for (pid in player_ids) {
        progress$attempts <- progress$attempts + 1L
        shots <- tryCatch(pga_shot_details(ev, pid, round = rd), error = function(e) NULL)

        if (is.null(shots) || !is.data.frame(shots) || nrow(shots) == 0) {
          progress$failures <- progress$failures + 1L
          if (sleep_seconds > 0) Sys.sleep(sleep_seconds)
          next
        }

        progress$successes <- progress$successes + 1L
        shots$tournament_id <- ev
        shots$player_id <- pid
        shots$round <- rd
        if (nzchar(ev_name)) shots$tournament_name <- ev_name

        append_csv(shots, output_csv)
        n_new <- nrow(shots)
        progress$rows_written <- progress$rows_written + n_new
        event_rows <- event_rows + n_new

        if (sleep_seconds > 0) Sys.sleep(sleep_seconds)
      }
    }

    message("  Event rows written: ", format(event_rows, big.mark = ","))
    progress$completed_tournaments <- unique(c(progress$completed_tournaments, ev))
    save_progress(progress, progress_rds)
    message("  Progress saved. Attempts=", progress$attempts,
            ", successes=", progress$successes,
            ", failures=", progress$failures,
            ", total rows=", format(progress$rows_written, big.mark = ","))
  }

  message("Done.")
  invisible(progress)
}

# Parse end date from schedule row (for ordering "most recent").
parse_schedule_end_date <- function(display_date, year_chr) {
  yr <- suppressWarnings(as.integer(year_chr))
  if (is.na(yr)) return(as.Date(NA))
  dd <- trimws(as.character(display_date))
  if (!nzchar(dd) || !grepl("-", dd, fixed = TRUE)) return(as.Date(NA))
  parts <- strsplit(dd, "\\s*-\\s*")[[1]]
  if (length(parts) < 2) return(as.Date(NA))
  left <- parts[[1]]
  right <- trimws(parts[[length(parts)]])
  mo_map <- c(
    Jan = 1L, Feb = 2L, Mar = 3L, Apr = 4L, May = 5L, Jun = 6L,
    Jul = 7L, Aug = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dec = 12L,
    January = 1L, February = 2L, March = 3L, April = 4L, May = 5L, June = 6L,
    July = 7L, August = 8L, September = 9L, October = 10L, November = 11L, December = 12L
  )
  if (grepl("^[0-9]{1,2}$", right)) {
    start_month <- trimws(strsplit(left, "\\s+")[[1]][1])
    m1 <- mo_map[[substr(start_month, 1, 3)]]
    if (is.null(m1) || is.na(m1)) m1 <- mo_map[[start_month]]
    if (is.null(m1) || is.na(m1)) return(as.Date(NA))
    day <- suppressWarnings(as.integer(right))
    if (is.na(day)) return(as.Date(NA))
    return(as.Date(sprintf("%04d-%02d-%02d", yr, m1, day)))
  }
  rw <- strsplit(right, "\\s+")[[1]]
  if (length(rw) >= 2) {
    endm <- mo_map[[rw[[1]]]]
    if (is.null(endm) || is.na(endm)) endm <- mo_map[[substr(rw[[1]], 1, 3)]]
    day <- suppressWarnings(as.integer(rw[[length(rw)]]))
    if (!is.null(endm) && !is.na(endm) && !is.na(day)) {
      return(as.Date(sprintf("%04d-%02d-%02d", yr, endm, day)))
    }
  }
  as.Date(NA)
}

# Most recent = IN_PROGRESS if any; else latest COMPLETED by parsed end date (this + last season).
get_most_recent_tournament <- function(years = NULL) {
  if (is.null(years)) {
    cy <- as.integer(format(Sys.Date(), "%Y"))
    years <- c(cy - 1L, cy)
  }
  sch <- get_schedules(years)
  st <- tolower(trimws(as.character(sch$status %||% "")))
  ip <- sch[st %in% c("in progress", "in_progress"), , drop = FALSE]
  if (nrow(ip) > 0) {
    return(ip[1, , drop = FALSE])
  }
  comp <- sch[grepl("complet", st, fixed = TRUE), , drop = FALSE]
  if (nrow(comp) == 0) {
    comp <- sch
  }
  end_dates <- mapply(
    function(dd, yr) parse_schedule_end_date(dd, yr),
    comp$display_date %||% rep(NA_character_, nrow(comp)),
    comp$year %||% rep(NA_character_, nrow(comp)),
    SIMPLIFY = TRUE
  )
  comp$end_date <- as.Date(end_dates, origin = "1970-01-01")
  ok <- !is.na(comp$end_date)
  if (any(ok)) {
    comp <- comp[ok, , drop = FALSE]
    comp <- comp[order(comp$end_date, decreasing = TRUE), , drop = FALSE]
    return(comp[1, , drop = FALSE])
  }
  # Fallback: last row of combined schedule
  sch[nrow(sch), , drop = FALSE]
}

# Fast check: does output CSV already contain any rows for this tournament_id?
csv_has_tournament <- function(output_csv, tournament_id) {
  if (!file.exists(output_csv) || !nzchar(as.character(tournament_id))) return(FALSE)
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::fread(output_csv, select = "tournament_id", showProgress = FALSE)
    if (!"tournament_id" %in% names(dt)) return(FALSE)
    return(any(dt$tournament_id == tournament_id, na.rm = TRUE))
  }
  con <- file(output_csv, "r")
  on.exit(close(con), add = TRUE)
  hdr <- readLines(con, 1)
  if (!nzchar(hdr)) return(FALSE)
  cols <- strsplit(hdr, ",", fixed = TRUE)[[1]]
  tid_idx <- which(cols == "tournament_id")
  if (length(tid_idx) != 1L) {
    warning("Could not find single 'tournament_id' column; scanning full file (slow).")
    df <- read.csv(output_csv, colClasses = "character", stringsAsFactors = FALSE, nrows = 500000L)
    if (!"tournament_id" %in% names(df)) return(FALSE)
    return(any(df$tournament_id == tournament_id, na.rm = TRUE))
  }
  tid_idx <- tid_idx[[1]]
  repeat {
    ln <- readLines(con, 1)
    if (length(ln) == 0) break
    fields <- strsplit(ln, ",", fixed = TRUE)[[1]]
    if (length(fields) >= tid_idx && fields[[tid_idx]] == tournament_id) return(TRUE)
  }
  FALSE
}

# Unique tournament_id values already stored (pgatouR IDs like R2021464).
unique_tournament_ids_in_shots_csv <- function(output_csv) {
  if (!file.exists(output_csv)) return(character(0))
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Install data.table (install.packages('data.table')) — required to scan tournament_id on large shot CSVs.")
  }
  dt <- data.table::fread(output_csv, select = "tournament_id", showProgress = FALSE)
  if (!"tournament_id" %in% names(dt)) return(character(0))
  u <- unique(as.character(dt$tournament_id))
  u[!is.na(u) & nzchar(u)]
}

# Pull one tournament via pga_shot_details; append rows. Returns counters (no progress file).
append_tournament_shots_core <- function(ev, ev_name, output_csv, rounds = 1:4, sleep_seconds = 0.03) {
  player_ids <- get_tournament_player_ids(ev)
  message("  Field size: ", length(player_ids))
  if (length(player_ids) == 0) {
    message("  No field/leaderboard — skipping.")
    return(list(rows_written = 0L, attempts = 0L, successes = 0L, failures = 0L))
  }
  attempts <- 0L
  successes <- 0L
  failures <- 0L
  rows_written <- 0L
  for (rd in rounds) {
    for (pid in player_ids) {
      attempts <- attempts + 1L
      shots <- tryCatch(pga_shot_details(ev, pid, round = rd), error = function(e) NULL)
      if (is.null(shots) || !is.data.frame(shots) || nrow(shots) == 0) {
        failures <- failures + 1L
        if (sleep_seconds > 0) Sys.sleep(sleep_seconds)
        next
      }
      successes <- successes + 1L
      shots$tournament_id <- ev
      shots$player_id <- pid
      shots$round <- rd
      if (nzchar(ev_name)) shots$tournament_name <- ev_name
      append_csv(shots, output_csv)
      rows_written <- rows_written + as.integer(nrow(shots))
      if (sleep_seconds > 0) Sys.sleep(sleep_seconds)
    }
  }
  message("  Rows appended: ", format(rows_written, big.mark = ","),
          " (attempts=", attempts, ", successes=", successes, ", failures=", failures, ")")
  list(rows_written = rows_written, attempts = attempts, successes = successes, failures = failures)
}

#' Backfill shot CSV from pgatouR only: tournaments with schedule end date after the latest
#' end date among events already in the file, through Sys.Date(), plus in-progress events not yet
#' in the file. Same paths as historical rounds: typically \code{data/all_shots_2021_2026.csv}.
#'
#' @param years Calendar years for PGA schedule fetch (default \code{2021:current_year}).
#' @param max_events Optional cap per run (env \code{GOLF_SHOTS_MAX_EVENTS} overrides when set).
backfill_shots_after_csv_anchor <- function(
  output_csv = "data/all_shots_2021_2026.csv",
  progress_rds = "data/all_shots_progress.rds",
  years = NULL,
  rounds = 1:4,
  sleep_seconds = 0.03,
  max_events = NULL,
  force = FALSE
) {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(years)) years <- seq(2021L, cy)
  mx <- Sys.getenv("GOLF_SHOTS_MAX_EVENTS", "")
  if (nzchar(mx)) {
    k <- suppressWarnings(as.integer(mx))
    if (!is.na(k) && k > 0L) max_events <- k
  }

  ids_in_csv <- unique_tournament_ids_in_shots_csv(output_csv)
  message("Shot CSV (pgatouR only): ", output_csv)
  message("  Tournaments already in file: ", length(ids_in_csv))

  sch <- get_schedules(years)
  tid_col <- pick_col(sch, c("tournament_id", "tournamentid", "event_id", "id"))
  if (is.na(tid_col)) stop("Could not find tournament_id column in schedule.")
  sch <- sch[!is.na(sch[[tid_col]]) & nzchar(as.character(sch[[tid_col]])), , drop = FALSE]
  sch$tournament_id_chr <- as.character(sch[[tid_col]])
  dd_col <- sch$display_date %||% rep(NA_character_, nrow(sch))
  yr_col <- if ("year" %in% names(sch)) {
    sch$year
  } else if ("season" %in% names(sch)) {
    sch$season
  } else {
    rep(NA_character_, nrow(sch))
  }
  sch$end_date <- as.Date(
    mapply(parse_schedule_end_date, dd_col, yr_col),
    origin = "1970-01-01"
  )
  sch$status_lc <- tolower(trimws(as.character(sch$status %||% "")))

  anchor <- if (length(ids_in_csv) == 0L) {
    as.Date("2020-12-31")
  } else {
    hit <- sch$tournament_id_chr %in% ids_in_csv
    eds <- sch$end_date[hit & !is.na(sch$end_date)]
    if (length(eds)) max(eds) else as.Date("2020-12-31")
  }
  message("  Anchor date (latest schedule end among IDs in CSV): ", anchor)

  today <- Sys.Date()
  name_col <- pick_col(sch, c("tournament", "tournament_name", "event_name", "name"))

  # Completed events: missing from CSV, end in (anchor, today]
  idx_comp <- which(
    !(sch$tournament_id_chr %in% ids_in_csv) &
      !is.na(sch$end_date) &
      sch$end_date > anchor &
      sch$end_date <= today &
      !grepl("in progress|in_progress", sch$status_lc)
  )
  if (length(idx_comp)) idx_comp <- idx_comp[order(sch$end_date[idx_comp])]

  # In-progress: not in CSV yet (first pull only; later days skip until new event — same as before)
  idx_ip <- which(
    !(sch$tournament_id_chr %in% ids_in_csv) &
      grepl("in progress|in_progress", sch$status_lc)
  )

  idx_all <- unique(c(idx_comp, idx_ip))
  if (length(idx_all) == 0L) {
    message("Nothing to backfill — file is current through ", today, " (pgatouR schedule).")
    return(invisible(list(events = 0L, rows = 0L)))
  }
  if (!is.null(max_events) && !is.na(max_events) && length(idx_all) > max_events) {
    message("Limiting to ", max_events, " tournament(s) this run (GOLF_SHOTS_MAX_EVENTS).")
    idx_all <- idx_all[seq_len(max_events)]
  }

  message("  Tournaments to pull: ", length(idx_all), " (pgatouR / pga_shot_details)")

  progress <- load_progress(progress_rds)
  tot_rows <- 0L
  n_ev <- 0L

  for (j in seq_along(idx_all)) {
    i <- idx_all[[j]]
    ev <- sch$tournament_id_chr[[i]]
    ev_name <- if (!is.na(name_col)) as.character(sch[[name_col]][[i]]) else ""
    message("[", j, "/", length(idx_all), "] ", ev, if (nzchar(ev_name)) paste0(" — ", ev_name) else "")

    if (!force && csv_has_tournament(output_csv, ev)) {
      message("  Already in CSV — skipping.")
      next
    }

    r <- append_tournament_shots_core(ev, ev_name, output_csv, rounds, sleep_seconds)
    tot_rows <- tot_rows + r$rows_written
    n_ev <- n_ev + 1L
    progress$completed_tournaments <- unique(c(progress$completed_tournaments %||% character(0), ev))
    progress$attempts <- (progress$attempts %||% 0L) + r$attempts
    progress$successes <- (progress$successes %||% 0L) + r$successes
    progress$failures <- (progress$failures %||% 0L) + r$failures
    progress$rows_written <- (progress$rows_written %||% 0L) + r$rows_written
    save_progress(progress, progress_rds)
    ids_in_csv <- unique(c(ids_in_csv, ev))
  }

  message("Backfill done: ", n_ev, " event(s) updated, ", format(tot_rows, big.mark = ","), " new rows.")
  invisible(list(events = n_ev, rows = tot_rows))
}

# Pull all shots for the most recent tournament and append to all_shots CSV if not already present.
append_latest_tournament_shots <- function(
  output_csv = "data/all_shots_2021_2026.csv",
  progress_rds = "data/all_shots_progress.rds",
  rounds = 1:4,
  sleep_seconds = 0.03,
  force = FALSE
) {
  latest <- get_most_recent_tournament()
  event_id_col <- pick_col(latest, c("tournament_id", "tournamentid", "event_id", "id"))
  event_name_col <- pick_col(latest, c("tournament", "tournament_name", "event_name", "name"))
  if (is.na(event_id_col)) stop("Could not find tournament_id in schedule row.")
  ev <- as.character(latest[[event_id_col]][[1]])
  ev_name <- if (!is.na(event_name_col)) as.character(latest[[event_name_col]][[1]]) else ""

  message("Most recent tournament: ", ev, if (nzchar(ev_name)) paste0(" (", ev_name, ")") else "")

  if (!force && csv_has_tournament(output_csv, ev)) {
    message("Already in ", output_csv, " — skipping (use force = TRUE to re-pull).")
    return(invisible(list(tournament_id = ev, skipped = TRUE)))
  }

  r <- append_tournament_shots_core(ev, ev_name, output_csv, rounds, sleep_seconds)

  if (file.exists(progress_rds)) {
    progress <- load_progress(progress_rds)
  } else {
    progress <- list(
      completed_tournaments = character(0),
      attempts = 0L,
      successes = 0L,
      failures = 0L,
      rows_written = 0L
    )
  }
  progress$completed_tournaments <- unique(c(progress$completed_tournaments %||% character(0), ev))
  progress$attempts <- (progress$attempts %||% 0L) + r$attempts
  progress$successes <- (progress$successes %||% 0L) + r$successes
  progress$failures <- (progress$failures %||% 0L) + r$failures
  progress$rows_written <- (progress$rows_written %||% 0L) + r$rows_written
  save_progress(progress, progress_rds)

  invisible(list(tournament_id = ev, skipped = FALSE, rows = r$rows_written))
}

# Example — full historical pull:
# progress <- get_all_shots(
#   years = 2021:2026,
#   rounds = 1:4,
#   output_csv = "data/all_shots_2021_2026.csv",
#   progress_rds = "data/all_shots_progress.rds",
#   sleep_seconds = 0.03,
#   resume = TRUE
# )
#
# Example — append latest tournament only if missing from CSV:
# append_latest_tournament_shots()
