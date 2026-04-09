# Map pgatouR tournament_id / tournament_name to DataGolf event_id / event_name (by season year).
# Data sources: pga_schedule (stacked years) + historical_rounds_all.csv (year, event_name, event_id, course_name).

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Lowercase, strip "The ", collapse punctuation for event name matching.
normalize_event_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("^the\\s+", "", x)
  x <- gsub("[^a-z0-9\\s]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Unique historical events (DataGolf): one row per year + event_id + event_name (+ course_name).
load_historical_events <- function(historical_path) {
  if (!file.exists(historical_path)) stop("Missing historical rounds file: ", historical_path)
  cols <- c("year", "event_name", "event_id", "course_name")
  if (requireNamespace("data.table", quietly = TRUE)) {
    h <- data.table::fread(historical_path, select = cols, showProgress = FALSE)
    h <- as.data.frame(h)
  } else {
    h <- utils::read.csv(historical_path, stringsAsFactors = FALSE, check.names = FALSE)[, cols, drop = FALSE]
  }
  miss <- setdiff(cols, names(h))
  if (length(miss)) stop("historical_rounds_all.csv missing columns: ", paste(miss, collapse = ", "))
  h$year <- suppressWarnings(as.integer(h$year))
  h$event_id <- suppressWarnings(as.integer(h$event_id))
  h$event_name <- trimws(as.character(h$event_name))
  h$course_name <- trimws(as.character(h$course_name))
  h <- h[is.finite(h$year) & is.finite(h$event_id) & nzchar(h$event_name), , drop = FALSE]
  h$event_key <- normalize_event_name(h$event_name)
  h <- unique(h[, c("year", "event_id", "event_name", "course_name", "event_key")])
  h
}

#' Build mapping: each pgatouR schedule row matched to at most one DataGolf event (same calendar year).
build_pga_dg_tournament_map <- function(schedule_map, historical_events_df) {
  sched <- schedule_map
  sched$year <- suppressWarnings(as.integer(sched$year))
  sched$pga_tournament_id <- as.character(sched$tournament_id)
  sched$pga_tournament_name <- as.character(sched$tournament_name)
  sched$pga_course_name <- as.character(sched$course_name)
  sched$name_key <- normalize_event_name(sched$pga_tournament_name)

  hist <- historical_events_df
  hist$dg_event_id <- hist$event_id
  hist$dg_event_name <- hist$event_name

  # Index historical by (year, event_key) — may have multiple course_name rows; keep first
  hist_u <- hist[!duplicated(paste(hist$year, hist$event_key, sep = "\t")), , drop = FALSE]

  map_rows <- vector("list", nrow(sched))
  for (i in seq_len(nrow(sched))) {
    y <- sched$year[i]
    nk <- sched$name_key[i]
    if (!is.finite(y) || !nzchar(nk)) {
      map_rows[[i]] <- data.frame(
        pga_tournament_id = sched$pga_tournament_id[i],
        pga_tournament_name = sched$pga_tournament_name[i],
        year = y,
        pga_course_name = sched$pga_course_name[i],
        dg_event_id = NA_integer_,
        dg_event_name = NA_character_,
        dg_course_name = NA_character_,
        match_method = "no_year_or_name",
        stringsAsFactors = FALSE
      )
      next
    }
    sub <- hist_u[hist_u$year == y & hist_u$event_key == nk, , drop = FALSE]
    method <- "exact_name_year"
    if (nrow(sub) == 0L) {
      # Substring / partial: PGA name contained in DG or vice versa
      h2 <- hist_u[hist_u$year == y, , drop = FALSE]
      if (nrow(h2) > 0L) {
        hit <- vapply(seq_len(nrow(h2)), function(j) {
          a <- nk
          b <- h2$event_key[j]
          nzchar(a) && nzchar(b) && (grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE))
        }, logical(1))
        if (any(hit)) sub <- h2[hit, , drop = FALSE][1, , drop = FALSE]
        if (nrow(sub) > 0L) method <- "partial_name_year"
      }
    }
    if (nrow(sub) == 0L) {
      map_rows[[i]] <- data.frame(
        pga_tournament_id = sched$pga_tournament_id[i],
        pga_tournament_name = sched$pga_tournament_name[i],
        year = y,
        pga_course_name = sched$pga_course_name[i],
        dg_event_id = NA_integer_,
        dg_event_name = NA_character_,
        dg_course_name = NA_character_,
        match_method = "unmatched",
        stringsAsFactors = FALSE
      )
      next
    }
    r <- sub[1, , drop = FALSE]
    map_rows[[i]] <- data.frame(
      pga_tournament_id = sched$pga_tournament_id[i],
      pga_tournament_name = sched$pga_tournament_name[i],
      year = y,
      pga_course_name = sched$pga_course_name[i],
      dg_event_id = as.integer(r$dg_event_id),
      dg_event_name = as.character(r$dg_event_name),
      dg_course_name = as.character(r$course_name),
      match_method = method,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, map_rows)
  rownames(out) <- NULL
  out
}

#' pgatouR tournament_ids that DataGolf lists as playing SELECTED_COURSE (normalized course match).
#' Requires `normalize_course_name` from R/course_context.R (source that file first).
pga_tournament_ids_for_dg_course <- function(selected_course, tournament_map, historical_events_df) {
  if (is.na(selected_course) || !nzchar(as.character(selected_course))) return(character(0))
  if (!exists("normalize_course_name", mode = "function")) {
    stop("source(\"R/course_context.R\") before calling pga_tournament_ids_for_dg_course()")
  }
  sk <- normalize_course_name(selected_course)
  h <- historical_events_df
  h$ck <- normalize_course_name(h$course_name)
  keep <- vapply(seq_len(nrow(h)), function(i) {
    if (!nzchar(h$ck[i])) return(FALSE)
    h$ck[i] == sk || grepl(sk, h$ck[i], fixed = TRUE) || grepl(h$ck[i], sk, fixed = TRUE)
  }, logical(1))
  ev <- h[keep, , drop = FALSE]
  if (nrow(ev) == 0L) return(character(0))
  # (year, event_id) pairs
  keys <- unique(paste(ev$year, ev$event_id, sep = ":"))
  tm <- tournament_map
  tm$key <- paste(tm$year, tm$dg_event_id, sep = ":")
  hit <- tm$key %in% keys & is.finite(tm$dg_event_id)
  unique(as.character(tm$pga_tournament_id[hit]))
}

#' Filter shot rows to tournaments where DataGolf historical data says the event used SELECTED_COURSE.
#' Returns NULL if no matching tournaments (caller may fall back to schedule-only filter).
filter_shots_for_selected_course_dg <- function(shots, selected_course, tournament_map, historical_events_df) {
  pids <- pga_tournament_ids_for_dg_course(selected_course, tournament_map, historical_events_df)
  if (length(pids) == 0L) return(NULL)
  out <- shots[as.character(shots$tournament_id) %in% pids, , drop = FALSE]
  message("DataGolf course mapping: ", length(pids), " pgatouR tournament_id(s) -> kept ", nrow(out), " / ", nrow(shots), " shot rows.")
  out
}
