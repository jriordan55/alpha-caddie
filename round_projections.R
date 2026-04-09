# round_projections.R — Standalone pipeline: field, live in-play, course, then decomp + strokes gained
# Builds simulated_round_table from DataGolf APIs; enhances with player decomps, course fit, SG-based counts.
# Leaderboard projections and outright model prices are tied together: section 4 runs a Monte Carlo
# tournament simulation and overwrites win/top5/top10/top20/make_cut with empirical frequencies.
# Default (when data/shot_transition_model.rds exists): shot-level empirical transitions from
# all_shots (fit_shot_transition_model.R) supply round-score noise; means follow DataGolf-style
# mu_sg + form shock per sim. Set GOLF_USE_SHOT_LEVEL_MC=0 to use Gaussian N(mu_sg, round_sd) rounds.
# The app can blend these with fair public odds (margin removed) for the displayed "Model" column.
#
# Methodology (DataGolf-aligned, no odds reverse-engineering):
# - Skill (mu_sg) from historical data only: weighted SG average + course fit. No DataGolf win/top5/top10 odds.
# - Standard deviation (2026 off-season): SD varies with skill (higher skill = lower residual SD) and driving
#   distance (longer hitters = higher variance). Driving accuracy has no meaningful predictive power for SD.
#   Baseline SD by skill from PGA Tour residual (actual minus predicted) SG table; then combined with
#   player's observed residual SD over last 150 rounds (modest impact) for final projected SD.
# - Course fit: attribute-based (OTT, APP, ARG, PUTT, driving distance/accuracy) vs course profile.
# - Counts (eagles/birdies/pars/bogeys/doubles): Monte Carlo mean from shot resampling (shot_transition_model.rds)
#   when available; else historical per-player/course means from historical_rounds_all — no fixed delta_stp baselines.
#   Per-round shape vs R1 from field averages at this course (round_num). GIR/fairways: shot MC uses each hole's
#   par (par-3/4/5 rules in hole_gir_fairway_from_to_codes). Historical fallbacks scale GIR x18; fairways x
#   (number of par-4 + par-5 holes) from the same 18-hole par vector as shot MC (shot_tpl / course layout).
# - Shot-level adjustments (diminishing returns to positive SG, penalty = 0.5 stroke) are applied in
#   DataGolf's live/pre-tournament feeds and are not reimplemented here.
# - Within-tournament form (GOLF_RAW_PROJECTIONS=0 only): books often move ~0.1 SG per 1 stroke R1 vs baseline;
#   we carry only a small fraction of prior-round SG *surplus vs baseline skill* into the next round’s mean SG
#   (default GOLF_WITHIN_EVENT_FORM_CARRY=0.02 ≈ 2% per round, capped by GOLF_WITHIN_EVENT_FORM_CAP). Uses
#   historical_rounds_all rows for the current event (event_name + year) when available.

library(dplyr)
library(tibble)
library(httr)
library(readr)
library(rlang)
library(jsonlite)
suppressPackageStartupMessages(library(purrr))
library(tidyr)
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

# Ensure model_dir and historical_rounds_all_path exist when this script is sourced standalone
if (!exists("model_dir")) model_dir <- getwd()
if (!exists("historical_rounds_all_path")) {
  historical_rounds_all_path <- file.path(model_dir, "data", "historical_rounds_all.csv")
}

normalize_course_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  n <- length(x)
  if (n == 0) return(character(0))
  out <- rep("", n)
  mask_pga <- grepl("pga national", x, fixed = TRUE) & grepl("champion", x, fixed = TRUE)
  out[mask_pga] <- "pga national champion"
  other <- !mask_pga & nzchar(x)
  if (any(other)) {
    y <- x[other]
    y <- gsub("resort", "", y, fixed = TRUE)
    y <- gsub(" course", "", y, fixed = TRUE)
    y <- gsub(" the ", " ", y, fixed = TRUE)
    y <- gsub("\\s+", " ", y)
    out[other] <- trimws(y)
  }
  out
}
normalize_event_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

if (!exists("API_KEY") || !nzchar(API_KEY %||% "")) API_KEY <- Sys.getenv("DATAGOLF_API_KEY", "")
model_dir <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd()

# Raw mode (default ON): projections = DataGolf API only (skill-ratings +/- optional fantasy defaults).
# No historical baselines, shrinkage, course-fit, shot MC, or internal round-SG multipliers.
# Set GOLF_RAW_PROJECTIONS=0 to restore the full pipeline.
RAW_PROJECTIONS <- tolower(trimws(Sys.getenv("GOLF_RAW_PROJECTIONS", "1"))) %in% c("1", "true", "yes")

# Round override: Shiny app writes golf_next_round.txt (content "1"-"4") so script always sees selected round
round_file <- file.path(model_dir, "golf_next_round.txt")
file_round <- if (file.exists(round_file)) suppressWarnings(as.integer(readLines(round_file, n = 1L, warn = FALSE))) else NA_integer_

# ========== 1) FETCH: field updates, live in-play (or pre-tournament), schedule for Mon/Tue/Wed ==========
field_updates_table <- tryCatch({
  res <- GET("https://feeds.datagolf.com/field-updates", query = list(tour = "pga", file_format = "csv", key = API_KEY))
  if (res$status_code != 200) stop("status ", res$status_code)
  read_csv(content(res, "raw"), show_col_types = FALSE)
}, error = function(e) { message("Field updates: ", conditionMessage(e)); tibble(dg_id = integer(), player_name = character()) })
for (col in c("r1_teetime", "r2_teetime", "r3_teetime", "r4_teetime")) if (col %in% names(field_updates_table)) field_updates_table[[col]] <- as.character(field_updates_table[[col]])
# Expose current tournament for app cache invalidation (so model uses this event, not a cached prior one)
dg_current_event_name <<- if (nrow(field_updates_table) > 0 && "event_name" %in% names(field_updates_table))
  trimws(as.character(field_updates_table$event_name[1])) else ""

UPCOMING_FROM_SCHEDULE <- NULL
PRET_EVENT_NAME <- ""
# Round: R1 Thu, R2 Fri, R3 Sat, R4 Sun. Shiny writes golf_next_round.txt; else options/env; else time-based.
env_round <- if (is.finite(file_round) && file_round >= 1L && file_round <= 4L) file_round else getOption("GOLF_NEXT_ROUND")
if (is.null(env_round)) env_round <- suppressWarnings(as.integer(Sys.getenv("GOLF_NEXT_ROUND", "")))
if (is.finite(env_round) && env_round >= 1L && env_round <= 4L) {
  next_round_num <- as.integer(env_round)
} else {
  tz_et <- "America/New_York"
  ROUND_SWITCH_HOUR_ET <- 21L
  now_et <- tryCatch(as.POSIXlt(Sys.time(), tz = tz_et), error = function(e) NULL)
  wday_et <- NA_integer_
  hour_et <- 0
  if (!is.null(now_et)) {
    wday_et <- as.integer(now_et$wday)
    hour_et <- as.numeric(now_et$hour) + as.numeric(now_et$min) / 60 + as.numeric(now_et$sec) / 3600
  }
  after_9pm <- is.finite(hour_et) && hour_et >= ROUND_SWITCH_HOUR_ET
  if (wday_et == 0L && after_9pm) next_round_num <- 1L
  else if (wday_et %in% 1L:3L) next_round_num <- 1L
  else if (wday_et == 4L && !after_9pm) next_round_num <- 1L
  else if (wday_et == 4L && after_9pm) next_round_num <- 2L
  else if (wday_et == 5L && !after_9pm) next_round_num <- 2L
  else if (wday_et == 5L && after_9pm) next_round_num <- 3L
  else if (wday_et == 6L && !after_9pm) next_round_num <- 3L
  else if (wday_et == 6L && after_9pm) next_round_num <- 4L
  else if (wday_et == 0L && !after_9pm) next_round_num <- 4L
  else next_round_num <- 1L
}
if (!is.finite(next_round_num) || next_round_num < 1L) next_round_num <- 1L
wday_et <- if (exists("wday_et")) wday_et else NA_integer_

# Fetch pre-tournament predictions (for R1 reverse-engineering and fallback when in-play empty)
pre_tournament_table <- NULL
fetch_pre_tournament <- function() {
  tryCatch({
    res <- GET("https://feeds.datagolf.com/preds/pre-tournament", query = list(tour = "pga", dead_heat = "no", odds_format = "percent", file_format = "json", key = API_KEY))
    if (res$status_code != 200) return(NULL)
    dat <- jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = TRUE)
    PRET_EVENT_NAME <<- trimws(as.character(dat$event_name %||% ""))
    bl <- NULL
    if ("baseline_history_fit" %in% names(dat) && is.data.frame(dat$baseline_history_fit) && nrow(dat$baseline_history_fit) > 0 && "dg_id" %in% names(dat$baseline_history_fit))
      bl <- dat$baseline_history_fit
    else if ("baseline" %in% names(dat) && is.data.frame(dat$baseline) && nrow(dat$baseline) > 0 && "dg_id" %in% names(dat$baseline))
      bl <- dat$baseline
    if (is.null(bl)) return(NULL)
    bl <- bl %>% mutate(
      win = as.numeric(win %||% 0), top_5 = as.numeric(top_5 %||% 0), top_10 = as.numeric(top_10 %||% 0),
      top_20 = as.numeric(top_20 %||% 0), make_cut = as.numeric(make_cut %||% 0)
    )
    if (any(is.finite(bl$win) & bl$win > 1.5)) bl <- bl %>% mutate(win = win / 100, top_5 = top_5 / 100, top_10 = top_10 / 100, top_20 = top_20 / 100, make_cut = make_cut / 100)
    else bl <- bl %>% mutate(win = win / 100, top_5 = top_5 / 100, top_10 = top_10 / 100, top_20 = top_20 / 100, make_cut = make_cut / 100)
    bl
  }, error = function(e) { message("Pre-tournament fetch: ", conditionMessage(e)); NULL })
}

fetch_schedule_upcoming_events <- function() {
  tryCatch({
    res <- GET("https://feeds.datagolf.com/get-schedule", query = list(tour = "pga", upcoming_only = "yes", file_format = "json", key = API_KEY))
    if (res$status_code != 200) return(list())
    dat <- jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE)
    events <- dat$events %||% dat$schedule %||% dat$tournaments %||% dat
    if (is.data.frame(events) && nrow(events) > 0) {
      lapply(seq_len(nrow(events)), function(i) as.list(events[i, , drop = FALSE]))
    } else if (is.list(events) && length(events) > 0) {
      events
    } else {
      list()
    }
  }, error = function(e) { message("Schedule fetch: ", conditionMessage(e)); list() })
}

schedule_match_event <- function(events, event_name) {
  if (!length(events) || !nzchar(event_name %||% "")) return(NULL)
  key <- normalize_event_name(event_name)
  if (!nzchar(key)) return(NULL)
  for (ev in events) {
    if (is.data.frame(ev) && nrow(ev) > 0) ev <- as.list(ev[1, , drop = FALSE])
    if (!is.list(ev)) next
    nm <- trimws(as.character(ev$event_name %||% ev$name %||% ev$tournament_name %||% ""))
    if (nzchar(nm) && identical(normalize_event_name(nm), key)) return(ev)
  }
  NULL
}

SCHEDULE_UPCOMING_EVENTS <- fetch_schedule_upcoming_events()

live_model_table <- tryCatch({
  res <- GET("https://feeds.datagolf.com/preds/in-play", query = list(tour = "pga", dead_heat = "no", odds_format = "percent", file_format = "csv", key = API_KEY))
  if (res$status_code != 200) stop("status ", res$status_code)
  read_csv(content(res, "raw"), show_col_types = FALSE)
}, error = function(e) tibble(dg_id = integer(), player_name = character(), win = numeric(), top_5 = numeric(), top_10 = numeric(), make_cut = numeric()))

# Round 1: always reverse-engineer from Pre-Tournament Predictions (full-field make cut, top 20, top 5, win)
if (next_round_num == 1L) {
  pre_tournament_table <- fetch_pre_tournament()
  if (!is.null(pre_tournament_table) && nrow(pre_tournament_table) > 0) {
    live_model_table <- pre_tournament_table %>% select(dg_id, player_name, win, top_5, top_10, any_of(c("top_20", "make_cut")))
    if (!"top_20" %in% names(live_model_table)) live_model_table$top_20 <- NA_real_
    if (!"make_cut" %in% names(live_model_table)) live_model_table$make_cut <- NA_real_
  }
}

if (nrow(live_model_table) == 0 && wday_et %in% c(1L, 2L, 3L)) {
  upcoming_from_api <- tryCatch({
    ev <- if (length(SCHEDULE_UPCOMING_EVENTS) > 0) SCHEDULE_UPCOMING_EVENTS[[1]] else NULL
    if (is.data.frame(ev) && nrow(ev) > 0) ev <- as.list(ev[1, , drop = FALSE])
    if (!is.list(ev)) return(NULL)
    list(event_name = trimws(as.character(ev$event_name %||% ev$name %||% "")), course_name = trimws(as.character(ev$course_name %||% ev$course %||% "")))
  }, error = function(e) NULL)
  if (!is.null(upcoming_from_api) && nzchar(upcoming_from_api$event_name)) {
    field_event <- if (nrow(field_updates_table) > 0 && "event_name" %in% names(field_updates_table)) trimws(as.character(na.omit(field_updates_table$event_name)[1])) else ""
    if (!identical(field_event, upcoming_from_api$event_name))
      UPCOMING_FROM_SCHEDULE <- list(event_name = upcoming_from_api$event_name, course_name = upcoming_from_api$course_name, use_it = TRUE)
    if (!is.finite(file_round) || file_round < 1L || file_round > 4L) next_round_num <- 1L
  }
  if (nrow(live_model_table) == 0) {
    pre_tournament_table <- fetch_pre_tournament()
    if (!is.null(pre_tournament_table) && nrow(pre_tournament_table) > 0 && "dg_id" %in% names(pre_tournament_table)) {
      live_model_table <- pre_tournament_table %>% select(dg_id, player_name, win, top_5, top_10, any_of(c("top_20", "make_cut")))
      if (!"top_20" %in% names(live_model_table)) live_model_table$top_20 <- NA_real_
      if (!"make_cut" %in% names(live_model_table)) live_model_table$make_cut <- NA_real_
    }
  }
}
# Fallback: tag current event for cache invalidation from schedule if field-updates had no event_name
if (!nzchar(dg_current_event_name %||% "") && !is.null(UPCOMING_FROM_SCHEDULE) && nzchar(UPCOMING_FROM_SCHEDULE$event_name %||% "")) {
  dg_current_event_name <<- trimws(as.character(UPCOMING_FROM_SCHEDULE$event_name))
}
if (!nzchar(dg_current_event_name %||% "")) {
  tryCatch({
    ev <- if (length(SCHEDULE_UPCOMING_EVENTS) > 0) SCHEDULE_UPCOMING_EVENTS[[1]] else NULL
    if (is.data.frame(ev) && nrow(ev) > 0) ev <- as.list(ev[1, , drop = FALSE])
    if (is.list(ev) && nzchar(trimws(as.character(ev$event_name %||% ev$name %||% ""))))
      dg_current_event_name <<- trimws(as.character(ev$event_name %||% ev$name %||% ""))
  }, error = function(e) NULL)
}
if (nrow(live_model_table) > 0) {
  for (col in c("win", "top_5", "top_10", "top_20")) {
    if (col %in% names(live_model_table)) { x <- as.numeric(live_model_table[[col]]); if (any(is.finite(x) & x > 1.5)) live_model_table[[col]] <- x / 100 else live_model_table[[col]] <- x }
    else live_model_table[[col]] <- NA_real_
  }
  # make_cut / mc (miss cut): important for R1/R2 reverse-engineering
  if ("mc" %in% names(live_model_table) && !"make_cut" %in% names(live_model_table)) {
    x <- as.numeric(live_model_table$mc)
    if (any(is.finite(x) & x > 1.5)) x <- x / 100
    live_model_table$make_cut <- 1 - x
  } else if ("make_cut" %in% names(live_model_table)) {
    x <- as.numeric(live_model_table$make_cut)
    if (any(is.finite(x) & x > 1.5)) live_model_table$make_cut <- x / 100 else live_model_table$make_cut <- x
  } else {
    live_model_table$make_cut <- NA_real_
  }
  live_model_table$make_cut <- pmax(1e-6, pmin(1 - 1e-6, as.numeric(live_model_table$make_cut)))
}

weekday_names <- c("Thursday", "Friday", "Saturday", "Sunday")
round_label <- paste0("R", next_round_num, " (", weekday_names[next_round_num], ")")

# ========== 2) COURSE: course_data, SELECTED_COURSE, course_profile, course_holes, course_env ==========
course_data_path <- file.path(model_dir, "data", "course_table.csv")
if (!file.exists(course_data_path)) course_data_path <- file.path(getwd(), "data", "course_table.csv")
course_data <- if (file.exists(course_data_path)) read_csv(course_data_path, show_col_types = FALSE) else tibble(course = character(), par = integer())
course_tokens_match <- function(raw_vals, ct) {
  raw_vals <- na.omit(trimws(as.character(raw_vals))); raw_vals <- raw_vals[nzchar(raw_vals)]
  if (length(raw_vals) == 0) return(character(0))
  tokens <- unique(trimws(unlist(strsplit(raw_vals, split = "[;,]", perl = TRUE)))); tokens <- tokens[nzchar(tokens)]
  tokens[tokens %in% ct$course]
}
# Prefer schedule course when we're in pre-tournament week (Mon/Tue/Wed and using upcoming event)
course_match_from_name <- function(name, ct) {
  if (!nzchar(name %||% "") || nrow(ct) == 0) return(character(0))
  n <- tolower(trimws(as.character(name)))
  courses_lower <- tolower(trimws(as.character(ct$course)))
  exact <- ct$course[courses_lower == n]
  if (length(exact) > 0) return(exact[1])
  partial <- ct$course[grepl(n, courses_lower, fixed = TRUE)]
  if (length(partial) > 0) return(partial[1])
  partial_rev <- ct$course[vapply(courses_lower, function(c) grepl(c, n, fixed = TRUE), logical(1))]
  if (length(partial_rev) > 0) return(partial_rev[1])
  character(0)
}
SELECTED_COURSE <- NULL
DISPLAY_COURSE <- NULL
# Primary: strict pre-tournament event match -> schedule upcoming course
if (nrow(course_data) > 0 && nzchar(PRET_EVENT_NAME %||% "")) {
  ev_match <- schedule_match_event(SCHEDULE_UPCOMING_EVENTS, PRET_EVENT_NAME)
  if (is.list(ev_match)) {
    schedule_course <- trimws(as.character(ev_match$course_name %||% ev_match$course %||% ev_match$coursename %||% ""))
    if (nzchar(schedule_course)) {
      DISPLAY_COURSE <- schedule_course
      matches <- course_tokens_match(schedule_course, course_data)
      if (length(matches) == 0) matches <- course_match_from_name(schedule_course, course_data)
      if (length(matches) > 0) SELECTED_COURSE <- matches[1]
    }
  }
}
# Secondary: field-updates course only when event_name matches pre-tournament event_name
if ((is.null(SELECTED_COURSE) || !nzchar(SELECTED_COURSE)) &&
    nrow(field_updates_table) > 0 && nrow(course_data) > 0 && "course_name" %in% names(field_updates_table)) {
  field_event <- if ("event_name" %in% names(field_updates_table)) trimws(as.character(na.omit(field_updates_table$event_name)[1])) else ""
  if (!nzchar(PRET_EVENT_NAME %||% "") || (nzchar(field_event) && identical(normalize_event_name(field_event), normalize_event_name(PRET_EVENT_NAME)))) {
    raw <- field_updates_table$course_name
    raw <- na.omit(trimws(unique(as.character(raw))))
    raw <- raw[nzchar(raw)]
    if (length(raw) > 0) {
      DISPLAY_COURSE <- raw[1]
      matches <- course_tokens_match(raw, course_data)
      if (length(matches) == 0) matches <- course_match_from_name(raw[1], course_data)
      if (length(matches) > 0) SELECTED_COURSE <- matches[1]
    }
  }
}
# Fallback: round-specific column then course column from field_updates
if ((is.null(SELECTED_COURSE) || !nzchar(SELECTED_COURSE)) && nrow(field_updates_table) > 0 && nrow(course_data) > 0) {
  round_course_col <- paste0("r", next_round_num, "_coursename")
  if (round_course_col %in% names(field_updates_table)) {
    raw <- field_updates_table[[round_course_col]]
    if (is.null(DISPLAY_COURSE) || !nzchar(DISPLAY_COURSE)) DISPLAY_COURSE <- na.omit(trimws(unique(as.character(raw))))[1]
    matches <- course_tokens_match(raw, course_data)
    if (length(matches) == 0 && length(na.omit(unique(as.character(raw)))) > 0) matches <- course_match_from_name(na.omit(unique(as.character(raw)))[1], course_data)
    if (length(matches) > 0) SELECTED_COURSE <- matches[1]
  }
  if ((is.null(SELECTED_COURSE) || !nzchar(SELECTED_COURSE)) && "course" %in% names(field_updates_table)) {
    raw <- field_updates_table$course
    if (is.null(DISPLAY_COURSE) || !nzchar(DISPLAY_COURSE)) DISPLAY_COURSE <- na.omit(trimws(unique(as.character(raw))))[1]
    matches <- course_tokens_match(raw, course_data)
    if (length(matches) == 0 && length(na.omit(unique(as.character(raw)))) > 0) matches <- course_match_from_name(na.omit(unique(as.character(raw)))[1], course_data)
    if (length(matches) > 0) SELECTED_COURSE <- matches[1]
  }
}
if (is.null(SELECTED_COURSE) || !nzchar(SELECTED_COURSE)) { if (nrow(course_data) > 0) SELECTED_COURSE <- course_data$course[1]; DISPLAY_COURSE <- DISPLAY_COURSE %||% SELECTED_COURSE }
if (is.null(DISPLAY_COURSE) || !nzchar(DISPLAY_COURSE)) DISPLAY_COURSE <- SELECTED_COURSE

# Persist for shot-level model scripts (fit_shot_transition_model.R) — same course as pipeline SELECTED_COURSE
tryCatch({
  sc <- SELECTED_COURSE %||% ""
  if (nzchar(sc)) {
    sel_path <- file.path(model_dir, "golf_selected_course.txt")
    writeLines(sc, sel_path, useBytes = TRUE)
  }
}, error = function(e) NULL)

course_profile <- course_data %>%
  mutate(course_key = normalize_course_name(course)) %>%
  {
    sel_key <- normalize_course_name(SELECTED_COURSE %||% course_data$course[1])
    out <- dplyr::filter(., course_key == sel_key)
    if (nrow(out) == 0) dplyr::slice(., 1) else out
  }
if (nrow(course_profile) == 0) course_profile <- course_data %>% slice(1)
if (nrow(course_profile) == 0) course_profile <- tibble(course = "Unknown", par = 72L, adj_score_to_par = 0)
total_par <- as.integer(course_profile$par[1])
if (!is.finite(total_par) || total_par < 69) total_par <- 72L

# Per-hole par + yardage from shots at this course (schedule filter), else generic shape from total par.
hole_tpl_live <- NULL
tryCatch(
  {
    source(file.path(model_dir, "R", "course_context.R"), encoding = "UTF-8")
    hole_tpl_live <- refresh_current_course_hole_template(model_dir, SELECTED_COURSE %||% "")
    if (!is.null(hole_tpl_live) && length(hole_tpl_live$par) == 18L) {
      tryCatch(
        saveRDS(
          list(
            course = SELECTED_COURSE %||% "",
            display_course = DISPLAY_COURSE %||% SELECTED_COURSE,
            holes_par = hole_tpl_live$par,
            holes_yardage = hole_tpl_live$yardage,
            source = hole_tpl_live$source,
            updated = Sys.time()
          ),
          file.path(model_dir, "data", "current_course_holes.rds")
        ),
        error = function(e) message("Could not write current_course_holes.rds: ", conditionMessage(e))
      )
    }
  },
  error = function(e) message("refresh_current_course_hole_template: ", conditionMessage(e))
)

if (!is.null(hole_tpl_live) && length(hole_tpl_live$par) == 18L) {
  course_holes <- tibble(hole = 1:18, par = as.integer(hole_tpl_live$par))
  course_par_18 <- as.integer(sum(course_holes$par))
} else {
  base_pars <- c(rep(3L, 4), rep(5L, 4), rep(4L, 10))
  diff <- total_par - sum(base_pars)
  if (diff != 0) { idx <- which(base_pars == 4L); base_pars[idx[1:abs(diff)]] <- base_pars[idx[1:abs(diff)]] + as.integer(sign(diff)) }
  course_holes <- tibble(hole = 1:18, par = base_pars)
  course_par_18 <- as.integer(sum(course_holes$par))
}
if (!is.finite(course_par_18) || course_par_18 < 69) course_par_18 <- 72L
# Fairways can only be hit on par 4s and par 5s (not par 3s)
n_fairway_holes <- as.integer(sum(course_holes$par == 4L, na.rm = TRUE) + sum(course_holes$par == 5L, na.rm = TRUE))
if (!is.finite(n_fairway_holes) || n_fairway_holes < 1L) n_fairway_holes <- 14L
adj_stp <- if ("adj_score_to_par" %in% names(course_profile)) as.numeric(course_profile$adj_score_to_par[1]) else 0
if (!is.finite(adj_stp)) adj_stp <- 0
# Scoring baselines: filled from historical_rounds_all at this course when MIN_ROUNDS_CALIBRATE_ENV met.
# No hardcoded default eagle/birdie counts — only adj_stp from course_profile until hist calibrates.
course_env <- tibble(
  avg_score_to_par = adj_stp,
  avg_eagles = NA_real_, avg_birdies = NA_real_, avg_bogeys = NA_real_, avg_doubles = NA_real_, avg_pars = NA_real_
)
course_avg_score_to_par <- as.numeric(course_env$avg_score_to_par[1])
if (!is.finite(course_avg_score_to_par)) course_avg_score_to_par <- 0
MIN_ROUNDS_CALIBRATE_ENV <- 40L

# Use the true course par as the baseline so that the field average round score is
# course_par_18 + course_avg_score_to_par inferred from historical_rounds_all for this course.
TARGET_FIELD_AVG_ROUND_SCORE <- NA_real_

# ========== 3) SKILL FROM HISTORICAL DATA ONLY (no DataGolf odds reverse-engineering) ==========
# Full field from field_updates; mu_sg from weighted historical SG + course fit only.
field_players <- NULL
if (nrow(field_updates_table) > 0) {
  fut <- field_updates_table
  if (!"player_name" %in% names(fut) && "player" %in% names(fut)) fut$player_name <- fut$player
  if (!"player_name" %in% names(fut) && "name" %in% names(fut)) fut$player_name <- fut$name
  if ("dg_id" %in% names(fut) && "player_name" %in% names(fut)) {
    field_players <- fut %>%
      filter(is.finite(dg_id), nzchar(trimws(as.character(player_name)))) %>%
      distinct(dg_id, .keep_all = TRUE)
    if (nrow(field_players) == 0) field_players <- NULL
  }
}

# Field: prefer field_updates; fallback to in-play/pre-tournament for player list only (no odds used)
dg <- if (!is.null(field_players) && nrow(field_players) > 0) {
  field_players %>% select(dg_id, player_name)
} else if (nrow(live_model_table) > 0) {
  live_model_table %>% filter(is.finite(dg_id), nzchar(trimws(as.character(player_name)))) %>%
    select(dg_id, player_name) %>% distinct(dg_id, .keep_all = TRUE)
} else {
  tibble(dg_id = integer(), player_name = character())
}
dg <- dg %>% mutate(win = NA_real_, top_5 = NA_real_, top_10 = NA_real_, top_20 = NA_real_, make_cut = NA_real_)
if (nrow(live_model_table) > 0 && nrow(dg) > 0) {
  lm_sel <- live_model_table %>% select(dg_id, win, top_5, top_10, any_of(c("top_20", "make_cut"))) %>% distinct(dg_id, .keep_all = TRUE)
  if (!"top_20" %in% names(lm_sel)) lm_sel$top_20 <- NA_real_
  if (!"make_cut" %in% names(lm_sel)) lm_sel$make_cut <- NA_real_
  dg <- dg %>% select(-any_of(c("win", "top_5", "top_10", "top_20", "make_cut"))) %>%
    left_join(lm_sel, by = "dg_id") %>%
    mutate(win = coalesce(win, NA_real_), top_5 = coalesce(top_5, NA_real_), top_10 = coalesce(top_10, NA_real_), top_20 = coalesce(top_20, NA_real_), make_cut = coalesce(make_cut, NA_real_))
}

if (nrow(dg) == 0) {
  simulated_round_table <<- tibble(dg_id = integer(), player_name = character(), position = integer(), total_score = numeric(), score_to_par = numeric(), mu_sg = numeric(), implied_mu_sg = numeric(), win = numeric(), top_5 = numeric(), top_10 = numeric(), top_20 = numeric(), make_cut = numeric(), eagles = numeric(), birdies = numeric(), pars = numeric(), bogeys = numeric(), doubles = numeric(), round_label = character(), course_used = character(), next_round = integer(), round = integer())
} else if (RAW_PROJECTIONS) {
  # ---------- RAW PROJECTIONS: preds/skill-ratings + optional preds/fantasy-projection-defaults ----------
  message("GOLF_RAW_PROJECTIONS: using DataGolf skill-ratings (and optional fantasy defaults); no internal baselines.")
  RAW_ROUND_SD <- suppressWarnings(as.numeric(Sys.getenv("GOLF_RAW_ROUND_SD", "2.75")))
  if (!is.finite(RAW_ROUND_SD) || RAW_ROUND_SD < 0.5) RAW_ROUND_SD <- 2.75
  fetch_skill_ratings_df <- function() {
    tryCatch({
      res <- GET("https://feeds.datagolf.com/preds/skill-ratings", query = list(display = "value", file_format = "json", key = API_KEY))
      if (res$status_code != 200) return(NULL)
      dat <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
      df <- if (is.data.frame(dat)) dat else if (is.list(dat) && !is.null(dat$data) && is.data.frame(dat$data)) dat$data else NULL
      if (is.null(df) || !"dg_id" %in% names(df)) return(NULL)
      tibble::as_tibble(df)
    }, error = function(e) {
      message("raw skill-ratings: ", conditionMessage(e))
      NULL
    })
  }
  sg_total_from_skill <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) return(rep(NA_real_, 0))
    if ("sg_total" %in% names(df)) return(suppressWarnings(as.numeric(df$sg_total)))
    if ("total" %in% names(df)) return(suppressWarnings(as.numeric(df$total)))
    if ("overall" %in% names(df)) return(suppressWarnings(as.numeric(df$overall)))
    rep(NA_real_, nrow(df))
  }
  fetch_fantasy_df <- function() {
    tryCatch({
      res <- GET(
        "https://feeds.datagolf.com/preds/fantasy-projection-defaults",
        query = list(tour = "pga", site = "draftkings", slate = "main", file_format = "json", key = API_KEY)
      )
      if (res$status_code != 200) return(NULL)
      dat <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
      df <- if (is.data.frame(dat)) dat else if (is.list(dat) && !is.null(dat$data) && is.data.frame(dat$data)) dat$data else NULL
      if (is.null(df) || !"dg_id" %in% names(df)) return(NULL)
      tibble::as_tibble(df)
    }, error = function(e) {
      message("raw fantasy defaults: ", conditionMessage(e))
      NULL
    })
  }
  first_num_col <- function(df, candidates) {
    for (nm in candidates) if (nm %in% names(df)) return(nm)
    NULL
  }
  skill_df <- fetch_skill_ratings_df()
  if (is.null(skill_df) || nrow(skill_df) == 0) {
    message("GOLF_RAW_PROJECTIONS: skill-ratings unavailable; empty simulated_round_table.")
    simulated_round_table <<- tibble(
      dg_id = integer(), player_name = character(), position = integer(), total_score = numeric(), score_to_par = numeric(),
      mu_sg = numeric(), implied_mu_sg = numeric(), win = numeric(), top_5 = numeric(), top_10 = numeric(), top_20 = numeric(), make_cut = numeric(),
      eagles = numeric(), birdies = numeric(), pars = numeric(), bogeys = numeric(), doubles = numeric(),
      round_label = character(), course_used = character(), next_round = integer(), round = integer(),
      round_sd = numeric()
    )
  } else {
    skill_df <- skill_df %>%
      dplyr::mutate(dg_id = suppressWarnings(as.integer(dg_id))) %>%
      dplyr::filter(is.finite(dg_id)) %>%
      dplyr::distinct(dg_id, .keep_all = TRUE)
    skill_df$mu_sg <- sg_total_from_skill(skill_df)
    fant_df <- fetch_fantasy_df()
    base <- dg %>%
      dplyr::left_join(skill_df %>% dplyr::select(dg_id, mu_sg), by = "dg_id") %>%
      dplyr::mutate(mu_sg = dplyr::coalesce(as.numeric(mu_sg), 0))
    if (!is.null(fant_df) && nrow(fant_df) > 0) {
      fd <- fant_df %>%
        dplyr::mutate(dg_id = suppressWarnings(as.integer(dg_id))) %>%
        dplyr::filter(is.finite(dg_id)) %>%
        dplyr::distinct(dg_id, .keep_all = TRUE)
      gv <- function(nm) {
        if (is.null(nm) || !nm %in% names(fd)) return(rep(NA_real_, nrow(fd)))
        suppressWarnings(as.numeric(fd[[nm]]))
      }
      bc <- first_num_col(fd, c("birdies", "birdie", "proj_birdies"))
      pc <- first_num_col(fd, c("pars", "par"))
      bgc <- first_num_col(fd, c("bogeys", "bogey", "bogies"))
      egc <- first_num_col(fd, c("eagles", "eagle_or_better"))
      dbc <- first_num_col(fd, c("doubles", "double_bogeys", "doubles_or_worse"))
      gc <- first_num_col(fd, c("gir", "greens_in_regulation", "gir_count"))
      fc <- first_num_col(fd, c("fairways", "driving_accuracy", "fw", "fairway"))
      fx <- tibble::tibble(
        dg_id = fd$dg_id,
        r_birdies = gv(bc),
        r_pars = gv(pc),
        r_bogeys = gv(bgc),
        r_eagles = gv(egc),
        r_doubles = gv(dbc),
        r_gir = gv(gc),
        r_fw = gv(fc)
      )
      base <- base %>%
        dplyr::left_join(fx, by = "dg_id") %>%
        dplyr::mutate(
          birdies = r_birdies,
          pars = r_pars,
          bogeys = r_bogeys,
          eagles = r_eagles,
          doubles = r_doubles,
          gir = dplyr::if_else(is.finite(r_gir) & r_gir > 0 & r_gir <= 1, r_gir * 18, r_gir),
          fairways = dplyr::if_else(is.finite(r_fw) & r_fw > 0 & r_fw <= 1, r_fw * max(1L, n_fairway_holes), r_fw)
        ) %>%
        dplyr::select(-dplyr::starts_with("r_"))
    }
    if (!"eagles" %in% names(base)) base$eagles <- NA_real_
    if (!"birdies" %in% names(base)) base$birdies <- NA_real_
    if (!"pars" %in% names(base)) base$pars <- NA_real_
    if (!"bogeys" %in% names(base)) base$bogeys <- NA_real_
    if (!"doubles" %in% names(base)) base$doubles <- NA_real_
    if (!"gir" %in% names(base)) base$gir <- NA_real_
    if (!"fairways" %in% names(base)) base$fairways <- NA_real_
    # Fantasy feed often omits pars/birdies columns or uses names we don't map — fill from mu_sg so Model O/U isn't all "—".
    impute_counts_from_mu_sg <- function(stp) {
      stp <- pmax(-8, pmin(8, as.numeric(stp)))
      eagles <- pmax(0, 0.15 - 0.02 * stp)
      birdies <- pmax(0.5, 3.8 - 0.45 * stp)
      bogeys <- pmax(0.5, 2.6 + 0.5 * stp)
      doubles <- pmax(0.1, 0.35 + 0.05 * stp)
      pars <- pmax(0.2, 18 - eagles - birdies - bogeys - doubles)
      s <- eagles + birdies + pars + bogeys + doubles
      k <- 18 / s
      list(
        eagles = eagles * k,
        birdies = birdies * k,
        pars = pars * k,
        bogeys = bogeys * k,
        doubles = doubles * k
      )
    }
    im <- impute_counts_from_mu_sg(-as.numeric(base$mu_sg))
    base$eagles <- dplyr::coalesce(base$eagles, im$eagles)
    base$birdies <- dplyr::coalesce(base$birdies, im$birdies)
    base$pars <- dplyr::coalesce(base$pars, im$pars)
    base$bogeys <- dplyr::coalesce(base$bogeys, im$bogeys)
    base$doubles <- dplyr::coalesce(base$doubles, im$doubles)
    # GIR / fairways as counts when still missing (rates ~ skill)
    stp_vec <- -as.numeric(base$mu_sg)
    base$gir <- dplyr::coalesce(
      base$gir,
      pmax(6, pmin(16, 11.5 - 0.25 * stp_vec))
    )
    base$fairways <- dplyr::coalesce(
      base$fairways,
      pmax(4, pmin(as.numeric(n_fairway_holes), 0.55 * as.numeric(n_fairway_holes) - 0.15 * stp_vec))
    )
    list_sim <- list()
    for (r in 1L:4L) {
      br <- base %>%
        dplyr::mutate(
          round = r,
          round_label = paste0("R", r, " (", weekday_names[r], ")"),
          next_round = r,
          implied_mu_sg = as.numeric(mu_sg),
          mu_sg = as.numeric(mu_sg),
          score_to_par = -mu_sg,
          total_score = as.numeric(course_par_18) + as.numeric(score_to_par),
          round_sd = RAW_ROUND_SD,
          course_used = DISPLAY_COURSE %||% SELECTED_COURSE
        ) %>%
        dplyr::arrange(total_score) %>%
        dplyr::mutate(position = dplyr::row_number())
      list_sim[[r]] <- br
    }
    dg_raw <- dplyr::bind_rows(list_sim) %>%
      dplyr::mutate(
        total_score = as.numeric(total_score),
        score_to_par = as.numeric(score_to_par),
        gir = as.numeric(gir),
        fairways = as.numeric(fairways),
        eagles = as.numeric(eagles),
        birdies = as.numeric(birdies),
        pars = as.numeric(pars),
        bogeys = as.numeric(bogeys),
        doubles = as.numeric(doubles)
      ) %>%
      dplyr::select(
        dg_id, player_name, position, total_score, score_to_par, gir, fairways,
        mu_sg, implied_mu_sg, round_sd,
        dplyr::any_of(c("win", "top_5", "top_10", "top_20", "make_cut")),
        eagles, birdies, pars, bogeys, doubles,
        round_label, course_used, next_round, round
      )
    if (!"top_20" %in% names(dg_raw)) dg_raw$top_20 <- NA_real_
    if (!"make_cut" %in% names(dg_raw)) dg_raw$make_cut <- NA_real_
    simulated_round_table <<- dg_raw
  }
  static_path_raw <- file.path(model_dir, "simulated_round_static.rds")
  tryCatch(
    saveRDS(simulated_round_table, static_path_raw),
    error = function(e) message("Could not write simulated_round_static.rds (raw): ", conditionMessage(e))
  )
} else {
  SKILL_SHRINKAGE_SLOPE <- 1.0

  player_weighted_sg <- NULL
  player_course_fit <- NULL
  power_rating_adj <- NULL
  HISTORICAL_ROUNDS_CACHE <- NULL
  HIST_COURSE_ROUNDS <- NULL
  ROUND_HIST_SG_MULT <- rep(1, 4)
  ROUND_HIST_SD_MULT <- rep(1, 4)
  ROUND_COUNT_RATIO_MAT <- NULL
  hist_gir_fw_tbl <- NULL
  hist_field_gir_fw_round <- NULL
  hist_player_kpi <- NULL
  field_kpi <- NULL
  projection_counts_round_tbl <- NULL
  # Prefer dataframe already loaded by app.R (historical_rounds_all); fall back to reading CSV if missing.
  # IMPORTANT: On shinyapps.io we MUST keep memory low, so we aggressively trim this dataset.
  hist_raw <- NULL
  if (exists("historical_rounds_all", envir = .GlobalEnv)) {
    candidate <- tryCatch(get("historical_rounds_all", envir = .GlobalEnv), error = function(e) NULL)
    if (is.data.frame(candidate) && nrow(candidate) > 0) hist_raw <- candidate
  }
  if (is.null(hist_raw)) {
    hist_path <- historical_rounds_all_path %||% file.path(model_dir, "data", "historical_rounds_all.csv")
    if (file.exists(hist_path)) {
      hist_raw <- tryCatch(read_csv(hist_path, show_col_types = FALSE), error = function(e) NULL)
    }
  }
  # Last 2 seasons only (current_year - 1 through current_year inclusive) to reduce memory on shinyapps.io
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  MIN_YEAR_2_SEASONS <- current_year - 1L
  if (!is.null(hist_raw) && "year" %in% names(hist_raw)) {
    hist_raw <- hist_raw %>% filter(as.integer(year) >= MIN_YEAR_2_SEASONS)
  }
  # Keep only columns actually used downstream to avoid carrying a very wide table in memory
  if (!is.null(hist_raw)) {
    needed_cols <- intersect(
      c(
        "dg_id", "player_name", "course_name", "sg_total",
        "round_num", "round", "round_score", "course_par",
        "birdies", "bogies", "eagles_or_better", "doubles_or_worse",
        "gir", "driving_acc", "pars",
        "event_completed", "teetime",
        "event_name", "year", "season"
      ),
      names(hist_raw)
    )
    if (length(needed_cols) > 0) {
      hist_raw <- hist_raw[, needed_cols, drop = FALSE]
    }
    # If still very large, randomly sample a cap of rows to keep regressions light
    MAX_HIST_ROWS <- 40000L
    if (nrow(hist_raw) > MAX_HIST_ROWS) {
      set.seed(123L)
      hist_raw <- hist_raw[sample.int(nrow(hist_raw), MAX_HIST_ROWS), , drop = FALSE]
    }
  }
  if (!is.null(hist_raw) && nzchar(SELECTED_COURSE %||% "")) {
    tryCatch({
      if (all(c("dg_id", "player_name", "course_name", "sg_total") %in% names(hist_raw)) && nrow(hist_raw) > 0) {
        hist_raw <- as_tibble(hist_raw) %>%
          mutate(course_key = normalize_course_name(course_name))
        round_col <- if ("round_num" %in% names(hist_raw)) "round_num" else if ("round" %in% names(hist_raw)) "round" else NULL
        if (!is.null(round_col)) {
          hist_by_round <- hist_raw %>%
            mutate(.round = as.integer(!!sym(round_col))) %>%
            filter(.round %in% 1L:4L, is.finite(sg_total))
          if (nrow(hist_by_round) > 0) {
            field_avg_by_round <- hist_by_round %>%
              group_by(.round) %>%
              summarise(field_avg_sg = mean(sg_total, na.rm = TRUE), .groups = "drop")
            player_avg_by_round <- hist_by_round %>%
              group_by(dg_id, player_name, .round) %>%
              summarise(avg_sg_round = mean(sg_total, na.rm = TRUE), n_round = n(), .groups = "drop") %>%
              left_join(field_avg_by_round, by = ".round") %>%
              mutate(power_adj = avg_sg_round - field_avg_sg)
            power_by_round <- player_avg_by_round %>% select(dg_id, player_name, .round, power_adj)
          }
        }
        decay_rate <- 0.995
        shrinkage_halflife <- 20
        time_decay_halflife_days <- 365
        has_dates <- "event_completed" %in% names(hist_raw)
        hist_with_date <- hist_raw %>% filter(is.finite(sg_total)) %>%
          mutate(.recency = if (has_dates) paste(coalesce(as.character(event_completed), ""), coalesce(as.character(teetime), "")) else row_number(), .date = suppressWarnings(as.Date(as.character(event_completed), optional = TRUE)))
        ref_date <- max(hist_with_date$.date, na.rm = TRUE)
        if (!is.finite(ref_date)) ref_date <- Sys.Date()
        hist_with_date <- hist_with_date %>% mutate(.days_ago = as.numeric(difftime(ref_date, .date, units = "days")), .days_ago = if_else(is.finite(.days_ago) & .days_ago >= 0, .days_ago, 0), .time_weight = exp(-.days_ago * log(2) / time_decay_halflife_days))
        player_weighted_sg <- hist_with_date %>% group_by(dg_id, player_name) %>% arrange(dg_id, player_name, desc(.recency)) %>%
          mutate(round_seq = row_number(), .seq_weight = decay_rate^(round_seq - 1), .time_w = if_else(is.finite(.time_weight), .time_weight, 1), weight = .seq_weight * .time_w) %>%
          summarise(weighted_avg_sg = sum(sg_total * weight, na.rm = TRUE) / sum(weight), n_rounds_hist = n(), .groups = "drop") %>%
          mutate(shrinkage = n_rounds_hist / (n_rounds_hist + shrinkage_halflife), mu_sg_from_hist = shrinkage * weighted_avg_sg)
        # Data-driven shrinkage: regress actual next-round SG on predicted (expanding-window weighted avg of prior rounds)
        SKILL_SHRINKAGE_SLOPE <- 1.0
        if (has_dates && nrow(hist_with_date) >= 5000L) {
          tryCatch({
            hist_asc <- hist_with_date %>%
              group_by(dg_id, player_name) %>%
              arrange(dg_id, player_name, .date) %>%
              mutate(seq_asc = row_number(), weight_f = decay_rate^(seq_asc - 1L) * if_else(is.finite(.time_weight), .time_weight, 1)) %>%
              mutate(cumw = cumsum(weight_f), cumsg = cumsum(sg_total * weight_f)) %>%
              mutate(predicted_sg = lag(cumsg) / lag(cumw), actual_sg = sg_total) %>%
              filter(!is.na(predicted_sg), seq_asc >= 10L)
            if (nrow(hist_asc) >= 1000L) {
              fit <- stats::lm(actual_sg ~ predicted_sg, data = hist_asc)
              b1 <- as.numeric(stats::coef(fit)["predicted_sg"])
              if (is.finite(b1) && b1 > 0.4 && b1 <= 1.1) SKILL_SHRINKAGE_SLOPE <<- b1
              else if (is.finite(b1) && b1 > 0) SKILL_SHRINKAGE_SLOPE <<- max(0.4, min(1, b1))
            }
          }, error = function(e) NULL)
        }
        sel_course_key <- normalize_course_name(SELECTED_COURSE %||% "")
        hist_course <- hist_raw %>%
          filter(course_key == sel_course_key, is.finite(sg_total))
        HIST_COURSE_ROUNDS <<- hist_course
        if (nrow(hist_course) > 0) {
          # Calibrate course_env (scoring baseline) from historical_rounds_all at this course
          env_cols <- c("round_score", "course_par", "birdies", "bogies", "eagles_or_better", "doubles_or_worse")
          if (nrow(hist_course) >= MIN_ROUNDS_CALIBRATE_ENV && all(env_cols %in% names(hist_course))) {
            cal <- hist_course %>%
              mutate(
                stp = as.numeric(round_score) - as.numeric(course_par),
                birdies = as.numeric(birdies),
                bogies = as.numeric(bogies),
                eagles = as.numeric(eagles_or_better),
                doubles = as.numeric(doubles_or_worse)
              ) %>%
              filter(is.finite(stp))
            if (nrow(cal) >= MIN_ROUNDS_CALIBRATE_ENV) {
              avg_stp <- mean(cal$stp, na.rm = TRUE)
              avg_bird <- mean(cal$birdies, na.rm = TRUE)
              avg_bog <- mean(cal$bogies, na.rm = TRUE)
              avg_eag <- mean(cal$eagles, na.rm = TRUE)
              avg_dbl <- mean(cal$doubles, na.rm = TRUE)
              if (is.finite(avg_stp)) { course_avg_score_to_par <<- avg_stp }
              if (all(is.finite(c(avg_eag, avg_bird, avg_bog, avg_dbl)))) {
                avg_pars <- 18 - avg_eag - avg_bird - avg_bog - avg_dbl
                avg_pars <- max(4, min(14, avg_pars))
                course_env <<- tibble(avg_score_to_par = course_avg_score_to_par, avg_eagles = avg_eag, avg_birdies = avg_bird, avg_bogeys = avg_bog, avg_doubles = avg_dbl, avg_pars = avg_pars)
              }
            }
          }
          course_fit_halflife <- 12
          player_course_fit <- hist_course %>% group_by(dg_id, player_name) %>% summarise(n_rounds_at_course = n(), avg_sg_at_course = mean(sg_total, na.rm = TRUE), .groups = "drop") %>%
            left_join(player_weighted_sg %>% select(dg_id, player_name, weighted_avg_sg), by = c("dg_id", "player_name")) %>%
            mutate(course_fit_raw = avg_sg_at_course - coalesce(weighted_avg_sg, 0), course_fit_shrink = n_rounds_at_course / (n_rounds_at_course + course_fit_halflife), course_fit_hist = course_fit_shrink * course_fit_raw,
                   w_hist = case_when(n_rounds_at_course < 5 ~ 0, n_rounds_at_course >= 15 ~ 0.35, TRUE ~ 0.25)) %>%
            select(dg_id, player_name, course_fit_hist, w_hist)
        }
        HISTORICAL_ROUNDS_CACHE <<- hist_raw
      }
    }, error = function(e) message("Historical/course fit: ", conditionMessage(e)))
    # Field round profiles (SG, SD, score-type mix vs R1) and GIR / fairway rates at this course from historical_rounds_all
    hc_r <- HIST_COURSE_ROUNDS
    if (!is.null(hc_r) && nrow(hc_r) > 0) {
      rc <- if ("round_num" %in% names(hc_r)) "round_num" else if ("round" %in% names(hc_r)) "round" else NULL
      if (!is.null(rc)) {
        hc2 <- hc_r %>% mutate(.rn = as.integer(!!rlang::sym(rc))) %>% dplyr::filter(.rn %in% 1L:4L)
        if (nrow(hc2) >= 20L) {
          sg_prof <- hc2 %>% dplyr::group_by(.rn) %>% dplyr::summarise(m = mean(sg_total, na.rm = TRUE), .groups = "drop") %>% dplyr::arrange(.rn)
          m1 <- sg_prof$m[sg_prof$.rn == 1L]
          if (length(m1) == 1L && is.finite(m1) && abs(m1) > 1e-6) {
            for (rr in 1L:4L) {
              mr <- sg_prof$m[sg_prof$.rn == rr]
              if (length(mr) == 1L && is.finite(mr)) ROUND_HIST_SG_MULT[rr] <- mr / m1
            }
          }
          sd_prof <- hc2 %>% dplyr::group_by(.rn) %>% dplyr::summarise(s = stats::sd(sg_total, na.rm = TRUE), .groups = "drop") %>% dplyr::arrange(.rn)
          s1 <- sd_prof$s[sd_prof$.rn == 1L]
          if (length(s1) == 1L && is.finite(s1) && s1 > 1e-6) {
            for (rr in 1L:4L) {
              sr <- sd_prof$s[sd_prof$.rn == rr]
              if (length(sr) == 1L && is.finite(sr)) ROUND_HIST_SD_MULT[rr] <- sr / s1
            }
          }
          c_prof <- hc2 %>% dplyr::group_by(.rn) %>% dplyr::summarise(
            eagles = mean(as.numeric(eagles_or_better), na.rm = TRUE),
            birdies = mean(as.numeric(birdies), na.rm = TRUE),
            bogeys = mean(as.numeric(bogies), na.rm = TRUE),
            doubles = mean(as.numeric(doubles_or_worse), na.rm = TRUE),
            .groups = "drop"
          ) %>% dplyr::arrange(.rn)
          c_prof$pars <- 18 - c_prof$eagles - c_prof$birdies - c_prof$bogeys - c_prof$doubles
          r1 <- c_prof %>% dplyr::filter(.rn == 1L)
          if (nrow(r1) == 1L) {
            mat <- matrix(1, nrow = 4L, ncol = 5L)
            colnames(mat) <- c("eagles", "birdies", "pars", "bogeys", "doubles_plus")
            r1v <- as.numeric(r1[1, c("eagles", "birdies", "pars", "bogeys", "doubles")])
            for (rr in 1L:4L) {
              row <- c_prof %>% dplyr::filter(.rn == rr)
              if (nrow(row) == 1L) {
                rv <- as.numeric(row[1, c("eagles", "birdies", "pars", "bogeys", "doubles")])
                mat[rr, ] <- ifelse(r1v > 1e-6, rv / r1v, 1)
              }
            }
            ROUND_COUNT_RATIO_MAT <- mat
          }
        }
      }
      hist_player_kpi <- hc_r %>% dplyr::group_by(dg_id) %>% dplyr::summarise(
        h_eagles = mean(as.numeric(eagles_or_better), na.rm = TRUE),
        h_birdies = mean(as.numeric(birdies), na.rm = TRUE),
        h_bogeys = mean(as.numeric(bogies), na.rm = TRUE),
        h_doubles = mean(as.numeric(doubles_or_worse), na.rm = TRUE),
        .groups = "drop"
      ) %>% dplyr::mutate(h_pars = pmax(0, 18 - h_eagles - h_birdies - h_bogeys - h_doubles))
      field_kpi <- hc_r %>% dplyr::summarise(
        f_eagles = mean(as.numeric(eagles_or_better), na.rm = TRUE),
        f_birdies = mean(as.numeric(birdies), na.rm = TRUE),
        f_bogeys = mean(as.numeric(bogies), na.rm = TRUE),
        f_doubles = mean(as.numeric(doubles_or_worse), na.rm = TRUE)
      )
      field_kpi$f_pars <- pmax(0, 18 - field_kpi$f_eagles - field_kpi$f_birdies - field_kpi$f_bogeys - field_kpi$f_doubles)
    }
    if (exists("HISTORICAL_ROUNDS_CACHE") && !is.null(HISTORICAL_ROUNDS_CACHE)) {
      HRC <- HISTORICAL_ROUNDS_CACHE
      if ("gir" %in% names(HRC) && "driving_acc" %in% names(HRC) && "course_key" %in% names(HRC)) {
        rc2 <- if ("round_num" %in% names(HRC)) "round_num" else if ("round" %in% names(HRC)) "round" else NULL
        sk <- normalize_course_name(SELECTED_COURSE %||% "")
        if (!is.null(rc2) && nzchar(sk)) {
          hist_gir_fw_tbl <- HRC %>%
            dplyr::filter(course_key == sk) %>%
            dplyr::mutate(round = as.integer(!!rlang::sym(rc2))) %>%
            dplyr::filter(round %in% 1L:4L, is.finite(dg_id)) %>%
            dplyr::group_by(dg_id, round) %>%
            dplyr::summarise(
              gir_m = mean(as.numeric(gir), na.rm = TRUE),
              fw_m = mean(as.numeric(driving_acc), na.rm = TRUE),
              .groups = "drop"
            )
          if (!is.null(hc_r) && nrow(hc_r) > 0 && "gir" %in% names(hc_r) && "driving_acc" %in% names(hc_r)) {
            rc3 <- if ("round_num" %in% names(hc_r)) "round_num" else if ("round" %in% names(hc_r)) "round" else NULL
            if (is.null(rc3)) {
              hist_field_gir_fw_round <- NULL
            } else {
            hc3 <- hc_r %>%
              dplyr::mutate(round = as.integer(!!rlang::sym(rc3))) %>%
              dplyr::filter(round %in% 1L:4L)
            hist_field_gir_fw_round <- hc3 %>% dplyr::group_by(round) %>% dplyr::summarise(
              gir_m = mean(as.numeric(gir), na.rm = TRUE),
              fw_m = mean(as.numeric(driving_acc), na.rm = TRUE),
              .groups = "drop"
            )
            }
          }
        }
      }
    }
    if (is.null(field_kpi) && !is.null(hist_raw) && nrow(hist_raw) > 0 &&
        all(c("eagles_or_better", "birdies", "bogies", "doubles_or_worse") %in% names(hist_raw))) {
      field_kpi <- hist_raw %>% dplyr::summarise(
        f_eagles = mean(as.numeric(eagles_or_better), na.rm = TRUE),
        f_birdies = mean(as.numeric(birdies), na.rm = TRUE),
        f_bogeys = mean(as.numeric(bogies), na.rm = TRUE),
        f_doubles = mean(as.numeric(doubles_or_worse), na.rm = TRUE)
      )
      field_kpi$f_pars <- pmax(0, 18 - field_kpi$f_eagles - field_kpi$f_birdies - field_kpi$f_bogeys - field_kpi$f_doubles)
    }
  }
  POWER_RATING_WEIGHT <- 0.35
  POWER_RATING_CAP <- 2.0
  if (!exists("power_by_round")) power_by_round <- NULL
  # Apply data-driven shrinkage (slope from actual_sg ~ predicted_sg; estimated above from historical rounds)
  shrink <- if (exists("SKILL_SHRINKAGE_SLOPE") && is.finite(SKILL_SHRINKAGE_SLOPE)) as.numeric(SKILL_SHRINKAGE_SLOPE) else 1.0

  # Optional: market-based skill adjustment using historical_matchups_outcomes_2025_2026.csv
  matchup_skill_tbl <- NULL
  matchup_path <- file.path(model_dir, "data", "historical_matchups_outcomes_2025_2026.csv")
  if (file.exists(matchup_path)) {
    tryCatch({
      hm <- readr::read_csv(matchup_path, show_col_types = FALSE)
      # Use R1 3-Ball and Match-Up markets; derive implied win probs from closing decimal odds
      hm <- hm %>%
        dplyr::filter(.data$bet_type %in% c("R1 3-Ball", "R1 Match-Up"))
      if (nrow(hm) > 0) {
        rows <- list()
        for (i in seq_len(nrow(hm))) {
          row <- hm[i, ]
          # Up to three players per row
          odds <- c(row$p1_close, row$p2_close, row$p3_close)
          dg_ids <- c(row$p1_dg_id, row$p2_dg_id, row$p3_dg_id)
          outcomes <- c(row$p1_outcome, row$p2_outcome, row$p3_outcome)
          odds <- suppressWarnings(as.numeric(odds))
          dg_ids <- suppressWarnings(as.integer(dg_ids))
          outcomes <- suppressWarnings(as.numeric(outcomes))
          valid <- is.finite(odds) & odds > 1 & is.finite(dg_ids)
          if (!any(valid)) next
          odds_v <- odds[valid]
          dg_v <- dg_ids[valid]
          out_v <- outcomes[valid]
          imp <- 1 / odds_v
          imp <- imp / sum(imp)
          rows[[length(rows) + 1L]] <- tibble(
            dg_id = dg_v,
            implied_win = imp,
            actual_win = dplyr::coalesce(out_v, 0)
          )
        }
        if (length(rows) > 0) {
          all_rows <- dplyr::bind_rows(rows)
          matchup_skill_tbl <- all_rows %>%
            dplyr::group_by(dg_id) %>%
            dplyr::summarise(
              implied_win = mean(implied_win, na.rm = TRUE),
              actual_win = mean(actual_win, na.rm = TRUE),
              n_bets = dplyr::n(),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              edge = pmax(-0.15, pmin(0.15, actual_win - implied_win)),
              matchup_skill_adj = pmax(-0.5, pmin(0.5, edge * 4))
            ) %>%
            dplyr::select(dg_id, matchup_skill_adj)
        }
      }
    }, error = function(e) message("matchup calibration: ", conditionMessage(e)))
  }
  if (is.null(matchup_skill_tbl)) {
    matchup_skill_tbl <- tibble(dg_id = integer(), matchup_skill_adj = numeric())
  }

  dg_base <- dg %>%
    left_join(if (!is.null(player_weighted_sg)) player_weighted_sg %>% select(dg_id, mu_sg_from_hist, n_rounds_hist) else tibble(dg_id = integer(), mu_sg_from_hist = numeric(), n_rounds_hist = integer()), by = "dg_id") %>%
    left_join(if (!is.null(player_course_fit)) player_course_fit %>% select(dg_id, course_fit_hist, w_hist) else tibble(dg_id = integer(), course_fit_hist = numeric(), w_hist = numeric()), by = "dg_id") %>%
    left_join(matchup_skill_tbl, by = "dg_id") %>%
    mutate(
      course_fit_hist = coalesce(course_fit_hist, 0),
      w_hist = coalesce(w_hist, 0),
      mu_sg_from_hist = coalesce(mu_sg_from_hist, 0),
      matchup_skill_adj = coalesce(matchup_skill_adj, 0),
      base_mu_sg = shrink * (mu_sg_from_hist + (w_hist * course_fit_hist)) + 0.25 * matchup_skill_adj,
      implied_mu_sg = base_mu_sg
    ) %>%
    select(-course_fit_hist, -w_hist, -n_rounds_hist, -mu_sg_from_hist, -matchup_skill_adj)

  source(file.path(model_dir, "R", "projection_outcome_helpers.R"), encoding = "UTF-8")
  resolve_shot_template_local <- function() {
    shot_rds_path <- file.path(model_dir, "data", "shot_transition_model.rds")
    par18 <- NULL
    y18 <- NULL
    ch_path <- file.path(model_dir, "data", "current_course_holes.rds")
    if (file.exists(ch_path)) {
      ch <- tryCatch(readRDS(ch_path), error = function(e) NULL)
      if (!is.null(ch) && length(ch$holes_par) == 18L) {
        skc <- normalize_course_name(ch$course %||% "")
        sks <- normalize_course_name(SELECTED_COURSE %||% "")
        if (nzchar(skc) && nzchar(sks) && identical(skc, sks)) {
          par18 <- as.integer(ch$holes_par)
          y18 <- as.integer(round(as.numeric(ch$holes_yardage)))
        }
      }
    }
    if (is.null(par18) && exists("hole_tpl_live") && !is.null(hole_tpl_live) && length(hole_tpl_live$par) == 18L) {
      par18 <- as.integer(hole_tpl_live$par)
      y18 <- as.integer(round(as.numeric(hole_tpl_live$yardage)))
    }
    if (is.null(par18) && file.exists(shot_rds_path)) {
      so <- tryCatch(readRDS(shot_rds_path), error = function(e) NULL)
      if (!is.null(so) && is.list(so) && !is.null(so$meta)) {
        par18 <- as.integer(unlist(so$meta$holes_par))
        y18 <- as.integer(round(as.numeric(unlist(so$meta$holes_yardage))))
      }
    }
    if (is.null(par18) || length(par18) != 18L || anyNA(par18)) return(NULL)
    if (is.null(y18) || length(y18) != 18L || anyNA(y18)) return(NULL)
    list(par = par18, yardage = y18)
  }
  shot_tbl <- NULL
  shot_tpl <- resolve_shot_template_local()
  n_sims_ct <- as.integer(max(5L, min(80L, as.integer(Sys.getenv("GOLF_SHOT_COUNT_MC_NSIM", "12")))))
  use_shot_counts <- !is.null(shot_tpl) && Sys.getenv("GOLF_USE_SHOT_LEVEL_PROJECTION_COUNTS", "1") != "0"
  if (isTRUE(use_shot_counts)) {
    shot_rds_path <- file.path(model_dir, "data", "shot_transition_model.rds")
    if (file.exists(shot_rds_path)) {
      so <- tryCatch(readRDS(shot_rds_path), error = function(e) NULL)
      if (!is.null(so) && is.list(so) && !is.null(so$fit)) {
        shot_tbl <- build_shot_outcome_counts_tbl(
          dg_base$dg_id, shot_tpl$par, shot_tpl$yardage, so$fit, model_dir, n_sims = n_sims_ct
        )
      }
    }
  }
  fe <- if (!is.null(field_kpi)) field_kpi$f_eagles else NA_real_
  fb <- if (!is.null(field_kpi)) field_kpi$f_birdies else NA_real_
  fp <- if (!is.null(field_kpi)) field_kpi$f_pars else NA_real_
  fbo <- if (!is.null(field_kpi)) field_kpi$f_bogeys else NA_real_
  fd <- if (!is.null(field_kpi)) field_kpi$f_doubles else NA_real_
  counts_base <- dg_base %>%
    dplyr::select(dg_id) %>%
    dplyr::left_join(
      if (!is.null(shot_tbl)) {
        shot_tbl
      } else {
        tibble(
          dg_id = integer(), s_eagles = numeric(), s_birdies = numeric(), s_pars = numeric(),
          s_bogeys = numeric(), s_doubles = numeric(), s_gir = numeric(), s_fairways = numeric()
        )
      },
      by = "dg_id"
    ) %>%
    dplyr::left_join(if (!is.null(hist_player_kpi)) hist_player_kpi else tibble(dg_id = integer(), h_eagles = numeric(), h_birdies = numeric(), h_pars = numeric(), h_bogeys = numeric(), h_doubles = numeric()), by = "dg_id") %>%
    dplyr::mutate(
      eagles  = dplyr::coalesce(s_eagles, h_eagles, fe),
      birdies = dplyr::coalesce(s_birdies, h_birdies, fb),
      pars    = dplyr::coalesce(s_pars, h_pars, fp),
      bogeys  = dplyr::coalesce(s_bogeys, h_bogeys, fbo),
      doubles = dplyr::coalesce(s_doubles, h_doubles, fd)
    )
  if (nrow(counts_base) > 0L) {
    for (i in seq_len(nrow(counts_base))) {
      v <- renormalize_counts_5(c(counts_base$eagles[i], counts_base$birdies[i], counts_base$pars[i], counts_base$bogeys[i], counts_base$doubles[i]))
      if (length(v) == 5L && all(is.finite(v))) {
        counts_base$eagles[i] <- v[1]
        counts_base$birdies[i] <- v[2]
        counts_base$pars[i] <- v[3]
        counts_base$bogeys[i] <- v[4]
        counts_base$doubles[i] <- v[5]
      }
    }
  }

  shot_gir_fw_tbl <- if (!is.null(shot_tbl) && all(c("s_gir", "s_fairways") %in% names(shot_tbl))) {
    shot_tbl %>% dplyr::distinct(dg_id, .keep_all = TRUE) %>% dplyr::select(dg_id, s_gir, s_fairways)
  } else {
    NULL
  }

  # Par-4 + par-5 count for THIS course layout — must match simulate_round_projection_metrics_one (per-hole par).
  # Prefer shot_tpl (same vectors passed to build_shot_outcome_counts_tbl); else course_holes from hole template.
  n_fw_holes_proj <- if (!is.null(shot_tpl) && length(shot_tpl$par) == 18L) {
    as.integer(sum(shot_tpl$par %in% c(4L, 5L), na.rm = TRUE))
  } else if (exists("course_holes") && is.data.frame(course_holes) && nrow(course_holes) == 18L && "par" %in% names(course_holes)) {
    as.integer(sum(as.integer(course_holes$par) %in% c(4L, 5L), na.rm = TRUE))
  } else {
    n_fairway_holes
  }
  if (!is.finite(n_fw_holes_proj) || n_fw_holes_proj < 1L) n_fw_holes_proj <- max(1L, as.integer(n_fairway_holes))

  # Section 4 SG hole-type lookup (expected_counts_from_sg_lookup) uses these counts — same 18-hole par vector as shot MC.
  HOLES_PAR_FOR_PROJECTION <- if (!is.null(shot_tpl) && length(shot_tpl$par) == 18L) {
    as.integer(shot_tpl$par)
  } else {
    as.integer(course_holes$par)
  }

  WITHIN_FORM_K <- suppressWarnings(as.numeric(Sys.getenv("GOLF_WITHIN_EVENT_FORM_CARRY", "0.02")))
  if (!is.finite(WITHIN_FORM_K)) WITHIN_FORM_K <- 0.02
  WITHIN_FORM_CAP <- suppressWarnings(as.numeric(Sys.getenv("GOLF_WITHIN_EVENT_FORM_CAP", "0.3")))
  if (!is.finite(WITHIN_FORM_CAP) || WITHIN_FORM_CAP <= 0) WITHIN_FORM_CAP <- 0.3
  within_event_form_tbl <- NULL
  if (WITHIN_FORM_K != 0 && exists("hist_raw") && is.data.frame(hist_raw) && nrow(hist_raw) > 0 &&
      nzchar(dg_current_event_name %||% "") && all(c("dg_id", "sg_total", "event_name") %in% names(hist_raw))) {
    rc_form <- if ("round_num" %in% names(hist_raw)) "round_num" else if ("round" %in% names(hist_raw)) "round" else NULL
    if (!is.null(rc_form)) {
      ev_k <- normalize_event_name(dg_current_event_name)
      cur_y <- as.integer(format(Sys.Date(), "%Y"))
      rc_sym <- rlang::sym(rc_form)
      hsub <- hist_raw %>%
        dplyr::mutate(
          .evn = normalize_event_name(as.character(.data$event_name)),
          .yr = if ("year" %in% names(hist_raw)) {
            suppressWarnings(as.integer(.data$year))
          } else {
            rep(cur_y, nrow(hist_raw))
          }
        ) %>%
        dplyr::filter(.data$.evn == ev_k, is.finite(.data$dg_id), is.finite(.data$sg_total)) %>%
        dplyr::mutate(.rn = suppressWarnings(as.integer(!!rc_sym))) %>%
        dplyr::filter(.data$.rn >= 1L, .data$.rn <= 3L)
      if ("year" %in% names(hist_raw)) {
        hsub <- hsub %>% dplyr::filter(is.na(.data$.yr) | .data$.yr == cur_y | .data$.yr == cur_y - 1L)
      }
      base_mu_ref <- dg_base %>% dplyr::select(dg_id, base_mu_sg)
      hsub <- hsub %>% dplyr::left_join(base_mu_ref, by = "dg_id") %>% dplyr::filter(is.finite(.data$base_mu_sg))
      if (nrow(hsub) > 0L) {
        by_rd <- hsub %>%
          dplyr::group_by(.data$dg_id, .data$.rn) %>%
          dplyr::summarise(
            surplus_m = mean(as.numeric(.data$sg_total) - as.numeric(.data$base_mu_sg), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::filter(is.finite(.data$surplus_m))
        if (nrow(by_rd) > 0L) {
          udg <- sort(unique(dg_base$dg_id))
          rows_form <- list()
          for (id in udg) {
            ssub <- by_rd %>% dplyr::filter(.data$dg_id == id) %>% dplyr::arrange(.data$.rn)
            for (tr in 2L:4L) {
              prev <- ssub %>% dplyr::filter(.data$.rn < tr)
              sh <- WITHIN_FORM_K * sum(prev$surplus_m, na.rm = TRUE)
              if (!is.finite(sh)) sh <- 0
              sh <- pmax(-WITHIN_FORM_CAP, pmin(WITHIN_FORM_CAP, sh))
              rows_form[[length(rows_form) + 1L]] <- tibble(dg_id = id, round = tr, within_form_shift = sh)
            }
          }
          within_event_form_tbl <- dplyr::bind_rows(rows_form)
        }
      }
    }
  }
  if (is.null(within_event_form_tbl) || nrow(within_event_form_tbl) == 0L) {
    within_event_form_tbl <- tibble(dg_id = integer(), round = integer(), within_form_shift = numeric())
  }

  round_score_baseline <- if (exists("TARGET_FIELD_AVG_ROUND_SCORE") && is.finite(TARGET_FIELD_AVG_ROUND_SCORE)) as.numeric(TARGET_FIELD_AVG_ROUND_SCORE) else as.numeric(course_par_18)
  weekday_names <- c("Thursday", "Friday", "Saturday", "Sunday")
  list_sim <- list()
  for (r in 1L:4L) {
    power_r <- if (!is.null(power_by_round) && nrow(power_by_round) > 0)
      power_by_round %>% filter(.round == r) %>% group_by(dg_id) %>% summarise(power_adj = mean(power_adj, na.rm = TRUE), .groups = "drop") else tibble(dg_id = integer(), power_adj = numeric())
    wf_r <- within_event_form_tbl %>% dplyr::filter(.data$round == r)
    dg_r <- dg_base %>%
      left_join(power_r, by = "dg_id") %>%
      left_join(wf_r, by = "dg_id") %>%
      mutate(
        power_adj = coalesce(pmax(-POWER_RATING_CAP, pmin(POWER_RATING_CAP, power_adj)), 0),
        within_form_shift = dplyr::coalesce(.data$within_form_shift, 0),
        mu_sg = pmax(-4, pmin(4, base_mu_sg + POWER_RATING_WEIGHT * power_adj + within_form_shift))
      ) %>%
      select(-base_mu_sg, -power_adj, -within_form_shift)
    dg_r <- dg_r %>% mutate(
      expected_score_to_par = -as.numeric(mu_sg),
      expected_round_score  = round_score_baseline + as.numeric(expected_score_to_par)
    )
    dg_r <- dg_r %>% arrange(expected_round_score) %>% mutate(position = row_number())
    cnt_r <- counts_base %>% dplyr::select(dg_id, eagles, birdies, pars, bogeys, doubles)
    if (nrow(cnt_r) > 0L && !is.null(ROUND_COUNT_RATIO_MAT)) {
      for (i in seq_len(nrow(cnt_r))) {
        v <- c(cnt_r$eagles[i], cnt_r$birdies[i], cnt_r$pars[i], cnt_r$bogeys[i], cnt_r$doubles[i])
        names(v) <- c("eagles", "birdies", "pars", "bogeys", "doubles_plus")
        v2 <- apply_round_count_ratios(v, r, ROUND_COUNT_RATIO_MAT)
        cnt_r$eagles[i] <- unname(v2["eagles"])
        cnt_r$birdies[i] <- unname(v2["birdies"])
        cnt_r$pars[i] <- unname(v2["pars"])
        cnt_r$bogeys[i] <- unname(v2["bogeys"])
        cnt_r$doubles[i] <- unname(v2["doubles_plus"])
      }
    }
    dg_r <- dg_r %>% dplyr::left_join(cnt_r, by = "dg_id")
    if (!is.null(shot_gir_fw_tbl)) {
      dg_r <- dg_r %>% dplyr::left_join(shot_gir_fw_tbl, by = "dg_id")
    }
    if (!"s_gir" %in% names(dg_r)) dg_r$s_gir <- NA_real_
    if (!"s_fairways" %in% names(dg_r)) dg_r$s_fairways <- NA_real_
    gir_pl_r <- if (!is.null(hist_gir_fw_tbl)) dplyr::filter(hist_gir_fw_tbl, round == r) else NULL
    gir_fd_r <- if (!is.null(hist_field_gir_fw_round)) dplyr::filter(hist_field_gir_fw_round, round == r) else NULL
    dg_r <- dg_r %>%
      dplyr::left_join(
        if (!is.null(gir_pl_r) && nrow(gir_pl_r) > 0) dplyr::select(gir_pl_r, dg_id, gir_m_pl = gir_m, fw_m_pl = fw_m) else tibble(dg_id = integer(), gir_m_pl = numeric(), fw_m_pl = numeric()),
        by = "dg_id"
      )
    gm_fd <- if (!is.null(gir_fd_r) && nrow(gir_fd_r) == 1L) as.numeric(gir_fd_r$gir_m[1]) else NA_real_
    fw_fd <- if (!is.null(gir_fd_r) && nrow(gir_fd_r) == 1L) as.numeric(gir_fd_r$fw_m[1]) else NA_real_
    dg_r <- dg_r %>%
      dplyr::mutate(
        gir_m = dplyr::coalesce(gir_m_pl, gm_fd),
        fw_m = dplyr::coalesce(fw_m_pl, fw_fd),
        gir = dplyr::case_when(
          is.finite(s_gir) ~ s_gir,
          is.finite(gir_m) & gir_m <= 1 ~ gir_m * 18,
          is.finite(gir_m) ~ gir_m,
          TRUE ~ NA_real_
        ),
        fairways = dplyr::case_when(
          is.finite(s_fairways) ~ s_fairways,
          is.finite(fw_m) & fw_m <= 1 ~ fw_m * n_fw_holes_proj,
          is.finite(fw_m) ~ fw_m,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::select(-dplyr::any_of(c("gir_m_pl", "fw_m_pl", "gir_m", "fw_m", "s_gir", "s_fairways")))
    dg_r <- dg_r %>%
      mutate(
        round       = r,
        round_label = paste0("R", r, " (", weekday_names[r], ")"),
        next_round  = r,
        course_used = DISPLAY_COURSE %||% SELECTED_COURSE
      )
    projection_counts_round_tbl <- dplyr::bind_rows(
      projection_counts_round_tbl,
      dg_r %>% dplyr::select(dg_id, round, eagles, birdies, pars, bogeys, doubles, gir, fairways)
    )
    list_sim[[r]] <- dg_r
  }
  dg_sim_all <- bind_rows(list_sim) %>%
    mutate(
      total_score  = as.numeric(expected_round_score),
      score_to_par = as.numeric(expected_score_to_par)
    ) %>%
    select(dg_id, player_name, position, total_score, score_to_par, gir, fairways, mu_sg, implied_mu_sg, win, top_5, top_10, any_of(c("top_20", "make_cut")), eagles, birdies, pars, bogeys, doubles, round_label, course_used, next_round, round)
  if (!"top_20" %in% names(dg_sim_all)) dg_sim_all$top_20 <- NA_real_
  if (!"make_cut" %in% names(dg_sim_all)) dg_sim_all$make_cut <- NA_real_

  # Model-derived outright probabilities for the current tournament field (from field_updates).
  # Placement markets sum to 5/10/20; make_cut sums to tournament cut line (PGA: 65 and ties).
  ref_round <- if (is.finite(next_round_num) && next_round_num %in% 1L:4L) as.integer(next_round_num) else 1L
  ref <- dg_sim_all %>% filter(round == ref_round) %>% distinct(dg_id, .keep_all = TRUE)
  if (nrow(ref) > 0) {
    pos <- ref$position
    n_players <- length(pos)
    # Steepness by field size: larger fields = more competitive, concentrate prob on top
    spread <- 2.2 * sqrt(n_players / 120)
    spread <- pmax(1.5, pmin(4, spread))
    # Win: inverse-power of position, normalized to sum to 1 (one winner)
    win_model <- (1 / pmax(1, pos)^1.2)
    win_model <- win_model / sum(win_model, na.rm = TRUE)
    # Top 5 / Top 10 / Top 20: logistic in rank, scaled so sum = 5, 10, 20 for this field
    logistic_top <- function(pos, k) 1 / (1 + exp((pos - (k + 0.5)) / spread))
    top_5_model  <- logistic_top(pos, 5)
    top_10_model <- logistic_top(pos, 10)
    top_20_model <- logistic_top(pos, 20)
    sum_5  <- sum(top_5_model,  na.rm = TRUE)
    sum_10 <- sum(top_10_model, na.rm = TRUE)
    sum_20 <- sum(top_20_model, na.rm = TRUE)
    if (is.finite(sum_5)  && sum_5  > 0) top_5_model  <- top_5_model  * (5  / sum_5)
    if (is.finite(sum_10) && sum_10 > 0) top_10_model <- top_10_model * (10 / sum_10)
    if (is.finite(sum_20) && sum_20 > 0) top_20_model <- top_20_model * (20 / sum_20)
    # Make cut: tournament-specific. PGA full-field = 65 and ties; limited field = proportional
    CUT_LINE_PGA <- 65L
    cut_target <- if (n_players >= 100L) min(n_players, CUT_LINE_PGA) else min(n_players, max(35L, round(0.45 * n_players)))
    make_cut_model <- pmax(0.02, pmin(0.98, 0.98 - 0.012 * pos))
    sum_mc <- sum(make_cut_model, na.rm = TRUE)
    if (is.finite(sum_mc) && sum_mc > 0) make_cut_model <- make_cut_model * (cut_target / sum_mc)
    make_cut_model <- pmin(1, pmax(0, make_cut_model))
    model_outrights <- tibble(
      dg_id = ref$dg_id,
      model_win = as.numeric(win_model),
      model_top_5 = as.numeric(top_5_model),
      model_top_10 = as.numeric(top_10_model),
      model_top_20 = as.numeric(top_20_model),
      model_make_cut = as.numeric(make_cut_model)
    )
    # Cap any probability at 95% so we never get 100% -> -9999 American odds
    cap_p <- function(x) pmin(0.95, pmax(0, as.numeric(x)))
    # Use model when API is NA or when API placement prob is unrealistically small (e.g. wrong scale)
    dg_sim_all <- dg_sim_all %>%
      left_join(model_outrights, by = "dg_id") %>%
      mutate(
        win      = cap_p(coalesce(win, model_win)),
        top_5    = cap_p(if_else(is.na(top_5)   | top_5   < 0.02, model_top_5,   top_5)),
        top_10   = cap_p(if_else(is.na(top_10)  | top_10  < 0.02, model_top_10,  top_10)),
        top_20   = cap_p(if_else(is.na(top_20)  | top_20  < 0.02, model_top_20,  top_20)),
        make_cut = cap_p(if_else(is.na(make_cut) | make_cut < 0.02, model_make_cut, make_cut))
      ) %>%
      select(-model_win, -model_top_5, -model_top_10, -model_top_20, -model_make_cut)
  }

  simulated_round_table <<- dg_sim_all
}

# ========== 4) ENHANCE: player decomps, course fit adj, strokes-gained counts (overwrite simulated_round_table) ==========
# SD model (2026-style): skill and driving distance. Shot-level adjustments are reflected in DataGolf's in-play/decompositions.
# Skipped when GOLF_RAW_PROJECTIONS=1 (API-only table already final).
if (!RAW_PROJECTIONS && exists("simulated_round_table") && !is.null(simulated_round_table) && nrow(simulated_round_table) > 0 && exists("course_profile") && exists("course_env") && exists("course_holes")) {

  course_par_18 <- if (exists("course_par_18")) course_par_18 else 72L
  course_avg_score_to_par <- if (exists("course_avg_score_to_par")) course_avg_score_to_par else 0
  round_label <- if (exists("round_label")) round_label else "R1"
  next_round_num <- if (exists("next_round_num")) next_round_num else 1L
  DISPLAY_COURSE <- if (exists("DISPLAY_COURSE")) DISPLAY_COURSE else NULL
  SELECTED_COURSE <- if (exists("SELECTED_COURSE")) SELECTED_COURSE else NULL
  if (!is.finite(course_par_18) || course_par_18 < 69) course_par_18 <- 72L
  if (!is.finite(course_avg_score_to_par)) course_avg_score_to_par <- 0

  cp <- course_profile
  for (col in c("putt_sg", "arg_sg", "app_sg", "ott_sg", "adj_driving_distance", "adj_driving_accuracy")) {
    if (!col %in% names(cp)) cp[[col]] <- 0
  }

  # Standard deviation (DataGolf 2026 off-season): SD of residual (actual - predicted) SG varies with skill
  # and driving distance. Driving accuracy is not used for SD. Table: skill band -> residual SD (PGA Tour).
  skill_sd_breaks <- c(-3.2, -2.8, -2.4, -2.0, -1.6, -1.2, -0.8, -0.4, 0, 0.4, 0.8)
  skill_sd_vals  <- c(3.23, 3.12, 3.04, 2.98, 2.90, 2.86, 2.82, 2.78, 2.75, 2.73, 2.72)
  baseline_sd_from_skill <- function(mu_sg) {
    if (!is.finite(mu_sg)) return(2.75)
    suppressWarnings(approx(skill_sd_breaks, skill_sd_vals, xout = mu_sg, rule = 2)$y)
  }
  PGA_AVG_DRIVING_YARDS <- 295
  SD_PER_YARD_ABOVE_AVG <- 0.004   # longer hitters tend to have higher variance (2026)
  MIN_ROUNDS_FOR_ACTUAL_SD <- 30L
  MAX_ROUNDS_ACTUAL_SD <- 150L     # observed SD over last 150 rounds; modest impact when blended
  SD_BLEND_WEIGHT_BASELINE <- 0.75 # baseline (skill + DD) primary; observed residual SD has modest impact

  # Player actual SD from last N rounds (reuse cache from section 3 to avoid second read)
  player_actual_sd <- NULL
  hist_sd_raw <- NULL
  if (exists("historical_rounds_all", envir = .GlobalEnv)) {
    candidate <- tryCatch(get("historical_rounds_all", envir = .GlobalEnv), error = function(e) NULL)
    if (is.data.frame(candidate) && nrow(candidate) > 0) hist_sd_raw <- candidate
  }
  if (is.null(hist_sd_raw)) {
    hist_path_sd <- historical_rounds_all_path %||% file.path(model_dir, "data", "historical_rounds_all.csv")
    if (file.exists(hist_path_sd)) {
      tryCatch({ hist_sd_raw <- read_csv(hist_path_sd, show_col_types = FALSE) }, error = function(e) NULL)
    }
  }
  # Last 2 seasons only (reuse MIN_YEAR_2_SEASONS from above)
  if (!is.null(hist_sd_raw) && "year" %in% names(hist_sd_raw)) {
    hist_sd_raw <- hist_sd_raw %>% filter(as.integer(year) >= MIN_YEAR_2_SEASONS)
  }
  if (!is.null(hist_sd_raw) && all(c("dg_id", "player_name", "sg_total") %in% names(hist_sd_raw))) {
    tryCatch({
      hist_sd_raw <- hist_sd_raw %>% filter(is.finite(sg_total))
      if (nrow(hist_sd_raw) > 0) {
        date_col <- if ("event_completed" %in% names(hist_sd_raw)) "event_completed" else NULL
        if (!is.null(date_col)) hist_sd_raw <- hist_sd_raw %>% arrange(dg_id, player_name, desc(!!sym(date_col)))
        # Residual SD (actual - predicted): per-player mean as proxy for predicted, then sd(residual)
        player_actual_sd <- hist_sd_raw %>%
          group_by(dg_id, player_name) %>%
          slice_head(n = MAX_ROUNDS_ACTUAL_SD) %>%
          mutate(player_mean_sg = mean(sg_total, na.rm = TRUE), residual_sg = sg_total - player_mean_sg) %>%
          summarise(
            actual_sd = sd(residual_sg, na.rm = TRUE),
            n_rounds_sd = n(),
            .groups = "drop"
          ) %>%
          filter(n_rounds_sd >= MIN_ROUNDS_FOR_ACTUAL_SD, is.finite(actual_sd), actual_sd > 0.5, actual_sd < 5) %>%
          select(dg_id, player_name, actual_sd)
      }
    }, error = function(e) message("round_projections player_actual_sd: ", conditionMessage(e)))
  }

  # Player decompositions
  player_decomp <- NULL
  USE_DECOMP_DRIVING_ACC_FOR_FAIRWAYS <- TRUE
  tryCatch({
    res <- GET("https://feeds.datagolf.com/preds/player-decompositions",
               query = list(tour = "pga", file_format = "csv", key = API_KEY))
    if (res$status_code == 200) {
      player_decomp <- read_csv(content(res, "raw"), show_col_types = FALSE) %>%
        select(dg_id, player_name, any_of(c("sg_putt", "sg_arg", "sg_app", "sg_ott", "driving_dist", "driving_acc")))
      for (c in c("sg_putt", "sg_arg", "sg_app", "sg_ott", "driving_dist", "driving_acc"))
        if (c %in% names(player_decomp)) player_decomp[[c]] <- as.numeric(player_decomp[[c]])
      # If driving_acc is near-constant across players, it won't produce meaningful fairway differences.
      if ("driving_acc" %in% names(player_decomp)) {
        da_sd <- suppressWarnings(stats::sd(player_decomp$driving_acc, na.rm = TRUE))
        if (is.finite(da_sd) && da_sd < 0.01) USE_DECOMP_DRIVING_ACC_FOR_FAIRWAYS <- FALSE
      }
    }
  }, error = function(e) message("round_projections decomp: ", conditionMessage(e)))

  # 2026 skill ratings (DataGolf preds/skill-ratings): overall SG + driving distance.
  # Used to refine mu_sg (blended with historical SG) and overwrite driving_dist when available.
  skill_ratings_tbl <- NULL
  tryCatch({
    res_sr <- GET(
      "https://feeds.datagolf.com/preds/skill-ratings",
      query = list(display = "value", file_format = "json", key = API_KEY)
    )
    if (res_sr$status_code == 200) {
      txt <- httr::content(res_sr, as = "text", encoding = "UTF-8")
      dat <- jsonlite::fromJSON(txt, flatten = TRUE)
      # API returns either a root data.frame or a list with $players; per docs we expect the latter.
      df <- NULL
      if (!is.null(dat$players) && is.data.frame(dat$players) &&
          all(c("dg_id", "player_name") %in% names(dat$players))) {
        df <- as_tibble(dat$players)
      } else if (is.data.frame(dat) && all(c("dg_id", "player_name") %in% names(dat))) {
        df <- as_tibble(dat)
      }
      if (!is.null(df)) {
        # Overall skill: total SG per round; driving distance varies by feed version.
        total_vec <- if ("sg_total" %in% names(df)) {
          suppressWarnings(as.numeric(df$sg_total))
        } else if ("total" %in% names(df)) {
          suppressWarnings(as.numeric(df$total))
        } else if ("overall" %in% names(df)) {
          suppressWarnings(as.numeric(df$overall))
        } else {
          rep(NA_real_, nrow(df))
        }
        dd_vec <- if ("driving_dist" %in% names(df)) {
          suppressWarnings(as.numeric(df$driving_dist))
        } else if ("distance" %in% names(df)) {
          suppressWarnings(as.numeric(df$distance))
        } else if ("dist" %in% names(df)) {
          suppressWarnings(as.numeric(df$dist))
        } else {
          rep(NA_real_, nrow(df))
        }
        skill_ratings_tbl <- tibble(
          dg_id = suppressWarnings(as.integer(df$dg_id)),
          skill_total_sg = total_vec,
          skill_driving_dist = dd_vec
        )
      }
    }
  }, error = function(e) message("round_projections skill-ratings: ", conditionMessage(e)))
  # Always have these columns defined so downstream mutate() calls don't fail when the feed is empty/unavailable.
  if (is.null(skill_ratings_tbl)) {
    skill_ratings_tbl <- tibble(dg_id = integer(), skill_total_sg = numeric(), skill_driving_dist = numeric())
  }

  # Strokes-gained count lookup: reuse historical cache when available
  hole_data_sg_rates <- NULL
  hist_sg_raw <- NULL
  if (exists("historical_rounds_all", envir = .GlobalEnv)) {
    candidate <- tryCatch(get("historical_rounds_all", envir = .GlobalEnv), error = function(e) NULL)
    if (is.data.frame(candidate) && nrow(candidate) > 0) hist_sg_raw <- as.data.frame(candidate)
  }
  if (is.null(hist_sg_raw)) {
    hist_path_sg <- historical_rounds_all_path %||% file.path(model_dir, "data", "historical_rounds_all.csv")
    if (file.exists(hist_path_sg)) tryCatch({ hist_sg_raw <- read.csv(hist_path_sg, stringsAsFactors = FALSE) }, error = function(e) NULL)
  }
  if (!is.null(hist_sg_raw) && "year" %in% names(hist_sg_raw)) {
    hist_sg_raw <- hist_sg_raw %>% filter(as.integer(year) >= MIN_YEAR_2_SEASONS)
  }
  hole_data_full_path <- file.path(model_dir, "data", "hole_data.csv")
  if (!file.exists(hole_data_full_path)) hole_data_full_path <- file.path(getwd(), "data", "hole_data.csv")
  if (!is.null(hist_sg_raw) && nrow(hist_sg_raw) > 0 && file.exists(hole_data_full_path)) {
    tryCatch({
      if (all(c("dg_id", "player_name", "sg_total") %in% names(hist_sg_raw))) {
        event_col <- if ("event_name" %in% names(hist_sg_raw)) "event_name" else if ("course_name" %in% names(hist_sg_raw)) "course_name" else NULL
        round_col <- if ("round_num" %in% names(hist_sg_raw)) "round_num" else if ("round" %in% names(hist_sg_raw)) "round" else NULL
        if (!is.null(event_col) && !is.null(round_col)) {
          name_norm <- function(x) {
            x <- trimws(as.character(x))
            has_comma <- grepl(",", x, fixed = TRUE)
            out <- x
            out[has_comma] <- vapply(strsplit(x[has_comma], ",\\s*"), function(p) paste(rev(trimws(p)), collapse = " "), character(1))
            tolower(trimws(out))
          }
          hist_sg <- as_tibble(hist_sg_raw) %>%
            filter(is.finite(sg_total)) %>%
            mutate(
              player_key = name_norm(player_name),
              event_key  = tolower(trimws(as.character(.data[[event_col]])))
            ) %>%
            select(player_key, event_key, round_num = !!sym(round_col), sg_total) %>%
            distinct(player_key, event_key, round_num, .keep_all = TRUE)
          hole_full <- read_csv(hole_data_full_path, show_col_types = FALSE)
          if (all(c("player_name", "par", "tournament_name", "round") %in% names(hole_full)) && "score_type" %in% names(hole_full)) {
            u <- toupper(trimws(as.character(hole_full$score_type)))
            hole_full$rel <- dplyr::case_when(
              grepl("EAGLE", u, fixed = TRUE) ~ -2L,
              grepl("BIRDIE", u, fixed = TRUE) ~ -1L,
              grepl("PAR", u, fixed = TRUE) ~ 0L,
              grepl("BOGEY", u, fixed = TRUE) & !grepl("DOUBLE", u, fixed = TRUE) ~ 1L,
              TRUE ~ 2L
            )
            hole_with_sg <- hole_full %>%
              mutate(
                player_key    = name_norm(player_name),
                tournament_key = tolower(trimws(as.character(tournament_name))),
                round         = as.integer(round)
              ) %>%
              left_join(hist_sg, by = c("player_key" = "player_key", "tournament_key" = "event_key", "round" = "round_num"))
            n_with_sg <- sum(!is.na(hole_with_sg$sg_total))
            if (n_with_sg >= 500) {
              hole_data_sg_rates <- hole_with_sg %>%
                filter(is.finite(sg_total)) %>%
                mutate(sg_bucket = cut(sg_total, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = 1:5, include.lowest = TRUE)) %>%
                group_by(par, sg_bucket, rel) %>%
                summarise(n = n(), .groups = "drop") %>%
                group_by(par, sg_bucket) %>%
                mutate(prob = n / sum(n)) %>%
                ungroup()
            }
          }
        }
      }
    }, error = function(e) message("round_projections hole_data_sg: ", conditionMessage(e)))
  }

  sg_to_bucket <- function(mu_sg) {
    b <- c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)
    idx <- findInterval(mu_sg, b, left.open = FALSE)
    pmin(5L, pmax(1L, as.integer(idx)))
  }
  expected_counts_from_sg_lookup <- function(mu_sg, n3, n4, n5, rates_table) {
    if (is.null(rates_table) || nrow(rates_table) == 0) return(NULL)
    bucket <- sg_to_bucket(mu_sg)
    get_prob <- function(par_val, rel_val) {
      sub <- rates_table %>%
        filter(par == par_val, rel == rel_val, as.integer(as.character(sg_bucket)) == bucket)
      if (nrow(sub) == 0) return(0)
      as.numeric(sub$prob[1])
    }
    exp_eagles  <- n3 * get_prob(3L, -2L) + n4 * get_prob(4L, -2L) + n5 * get_prob(5L, -2L)
    exp_birdies <- n3 * get_prob(3L, -1L) + n4 * get_prob(4L, -1L) + n5 * get_prob(5L, -1L)
    exp_pars    <- n3 * get_prob(3L,  0L) + n4 * get_prob(4L,  0L) + n5 * get_prob(5L,  0L)
    exp_bogeys  <- n3 * get_prob(3L,  1L) + n4 * get_prob(4L,  1L) + n5 * get_prob(5L,  1L)
    exp_doubles <- n3 * get_prob(3L,  2L) + n4 * get_prob(4L,  2L) + n5 * get_prob(5L,  2L)
    sum_five <- exp_eagles + exp_birdies + exp_pars + exp_bogeys + exp_doubles
    if (is.finite(sum_five) && sum_five > 0) {
      scale <- 18 / sum_five
      exp_eagles  <- exp_eagles * scale
      exp_birdies <- exp_birdies * scale
      exp_pars    <- exp_pars * scale
      exp_bogeys  <- exp_bogeys * scale
      exp_doubles <- exp_doubles * scale
    }
    list(
      exp_eagles  = pmax(0, exp_eagles),
      exp_birdies = pmax(0, exp_birdies),
      exp_pars    = pmax(0, exp_pars),
      exp_bogeys  = pmax(0, exp_bogeys),
      exp_doubles = pmax(0, exp_doubles)
    )
  }

  layout_par_sg <- if (exists("HOLES_PAR_FOR_PROJECTION") && length(HOLES_PAR_FOR_PROJECTION) == 18L) {
    HOLES_PAR_FOR_PROJECTION
  } else {
    course_holes$par
  }
  np_sg <- list(
    n3 = sum(layout_par_sg == 3L, na.rm = TRUE),
    n4 = sum(layout_par_sg == 4L, na.rm = TRUE),
    n5 = sum(layout_par_sg == 5L, na.rm = TRUE)
  )
  if (np_sg$n3 + np_sg$n4 + np_sg$n5 != 18L) { np_sg$n3 <- 4L; np_sg$n4 <- 10L; np_sg$n5 <- 4L }

  # Player driving accuracy from historical rounds (varies by golfer; robust fallback for fairways)
  driving_acc_hist_tbl <- NULL
  if (!is.null(hist_sd_raw) && all(c("dg_id", "driving_acc") %in% names(hist_sd_raw))) {
    tryCatch({
      tmp <- as_tibble(hist_sd_raw) %>%
        mutate(dg_id = as.integer(dg_id), driving_acc = as.numeric(driving_acc)) %>%
        filter(is.finite(dg_id), is.finite(driving_acc))
      if (nrow(tmp) > 0) {
        if ("event_completed" %in% names(tmp)) {
          tmp <- tmp %>%
            mutate(.date = suppressWarnings(as.Date(as.character(event_completed)))) %>%
            arrange(dg_id, desc(.date))
        } else {
          tmp <- tmp %>% arrange(dg_id)
        }
        driving_acc_hist_tbl <- tmp %>%
          group_by(dg_id) %>%
          slice_head(n = 80) %>%
          summarise(driving_acc_hist = mean(driving_acc, na.rm = TRUE), .groups = "drop")
      }
    }, error = function(e) NULL)
  }
  if (is.null(driving_acc_hist_tbl) || nrow(driving_acc_hist_tbl) == 0) {
    driving_acc_hist_tbl <- tibble(dg_id = integer(), driving_acc_hist = numeric())
  }

  # Calibrate fairway-accuracy from historical rounds using sg_ott + driving_dist.
  # This avoids the "everyone = 0.58" issue when the decomps feed's driving_acc is constant/NA.
  FW_FIT_OK <- FALSE
  fw_b0 <- NA_real_
  fw_b1 <- NA_real_
  fw_b2 <- NA_real_
  if (!is.null(hist_sd_raw) && all(c("driving_acc", "sg_ott", "driving_dist") %in% names(hist_sd_raw))) {
    tryCatch({
      cal <- as_tibble(hist_sd_raw) %>%
        mutate(
          driving_acc = as.numeric(driving_acc),
          sg_ott = as.numeric(sg_ott),
          driving_dist = as.numeric(driving_dist)
        ) %>%
        filter(
          is.finite(driving_acc), driving_acc > 0, driving_acc < 1,
          is.finite(sg_ott),
          is.finite(driving_dist)
        )
      if (nrow(cal) > 5000) {
        set.seed(1)
        cal <- dplyr::slice_sample(cal, n = min(20000, nrow(cal)))
      }
      if (nrow(cal) >= 500) {
        fit <- stats::lm(stats::qlogis(driving_acc) ~ sg_ott + driving_dist, data = cal)
        co <- stats::coef(fit)
        if (length(co) >= 3 && all(is.finite(co))) {
          fw_b0 <- as.numeric(co[[1]])
          fw_b1 <- as.numeric(co[["sg_ott"]] %||% NA_real_)
          fw_b2 <- as.numeric(co[["driving_dist"]] %||% NA_real_)
          FW_FIT_OK <- all(is.finite(c(fw_b0, fw_b1, fw_b2)))
        }
      }
    }, error = function(e) NULL)
  }

  decomp_cols <- c("sg_putt", "sg_arg", "sg_app", "sg_ott", "driving_dist", "driving_acc")
  empty_decomp <- tibble(dg_id = integer(), player_name = character(), sg_putt = NA_real_, sg_arg = NA_real_, sg_app = NA_real_, sg_ott = NA_real_, driving_dist = NA_real_, driving_acc = NA_real_)
  to_join <- if (!is.null(player_decomp) && nrow(player_decomp) > 0) {
    out <- as.data.frame(player_decomp)
    for (c in decomp_cols) if (!c %in% names(out)) out[[c]] <- NA_real_
    out <- as_tibble(out)
    out %>% select(dg_id, any_of(decomp_cols)) %>% distinct(dg_id, .keep_all = TRUE)
  } else tibble(dg_id = integer(), sg_putt = NA_real_, sg_arg = NA_real_, sg_app = NA_real_, sg_ott = NA_real_, driving_dist = NA_real_, driving_acc = NA_real_)
  if (!all(decomp_cols %in% names(to_join))) {
    for (c in setdiff(decomp_cols, names(to_join))) to_join[[c]] <- NA_real_
  }
  to_join_sd <- if (!is.null(player_actual_sd) && nrow(player_actual_sd) > 0) player_actual_sd %>% select(dg_id, actual_sd) %>% distinct(dg_id, .keep_all = TRUE) else tibble(dg_id = integer(), actual_sd = numeric())
  # Course fit: attribute-based (OTT, APP, ARG, PUTT, distance, accuracy); predictive power OTT > APP > ARG > PUTT
  round_score_baseline_s4 <- if (exists("TARGET_FIELD_AVG_ROUND_SCORE") && is.finite(TARGET_FIELD_AVG_ROUND_SCORE)) as.numeric(TARGET_FIELD_AVG_ROUND_SCORE) else as.numeric(course_par_18)
  if (!exists("ROUND_HIST_SG_MULT")) ROUND_HIST_SG_MULT <- rep(1, 4)
  if (!exists("ROUND_HIST_SD_MULT")) ROUND_HIST_SD_MULT <- rep(1, 4)
  dg_sim <- simulated_round_table %>%
    left_join(to_join, by = "dg_id") %>%
    left_join(to_join_sd, by = "dg_id") %>%
    # Blend in 2026 skill ratings: overall SG and driving distance.
    # This refines mu_sg beyond historical_rounds_all and ensures driving_dist matches current skill feed.
    { if (!is.null(skill_ratings_tbl) && nrow(skill_ratings_tbl) > 0)
        dplyr::left_join(., skill_ratings_tbl, by = "dg_id") else . } %>%
    # Ensure skill columns exist even if join brought nothing (all NA)
    { if (!"skill_total_sg" %in% names(.)) dplyr::mutate(., skill_total_sg = NA_real_) else . } %>%
    { if (!"skill_driving_dist" %in% names(.)) dplyr::mutate(., skill_driving_dist = NA_real_) else . } %>%
    mutate(
      # Historical-based baseline before decomp/course-fit: implied_mu_sg already set earlier in pipeline.
      base_mu_hist = as.numeric(implied_mu_sg),
      base_mu_sr = as.numeric(skill_total_sg),
      # Conservative blend: mostly historical, small pull towards 2026 skill ratings when available.
      w_sr = dplyr::case_when(
        is.finite(base_mu_sr) & is.finite(base_mu_hist) ~ 0.25,
        is.finite(base_mu_sr) & !is.finite(base_mu_hist) ~ 0.50,
        TRUE ~ 0.00
      ),
      implied_mu_sg = dplyr::case_when(
        is.finite(base_mu_sr) & is.finite(base_mu_hist) ~
          (1 - w_sr) * base_mu_hist + w_sr * base_mu_sr,
        is.finite(base_mu_sr) & !is.finite(base_mu_hist) ~ base_mu_sr,
        TRUE ~ implied_mu_sg
      ),
      # Use skill-rated driving distance when available; fallback to decompositions.
      driving_dist = dplyr::coalesce(skill_driving_dist, driving_dist)
    ) %>%
    left_join(driving_acc_hist_tbl, by = "dg_id") %>%
    mutate(
      course_fit_adj = coalesce(sg_putt, 0) * as.numeric(cp$putt_sg[1]) +
        coalesce(sg_arg, 0) * as.numeric(cp$arg_sg[1]) +
        coalesce(sg_app, 0) * as.numeric(cp$app_sg[1]) +
        coalesce(sg_ott, 0) * as.numeric(cp$ott_sg[1]),
      course_fit_adj = if ("adj_driving_distance" %in% names(cp) && "adj_driving_accuracy" %in% names(cp))
        course_fit_adj + 0.002 * (coalesce(driving_dist, 0) - as.numeric(cp$adj_driving_distance[1])) +
          0.5 * (coalesce(driving_acc, 0) - as.numeric(cp$adj_driving_accuracy[1])) else course_fit_adj,
      w_decomp = 0.30,
      mu_sg = mu_sg + (w_decomp * course_fit_adj),
      mu_sg = pmax(-4, pmin(4, mu_sg)),
      expected_score_to_par = -as.numeric(mu_sg),
      expected_round_score  = round_score_baseline_s4 + as.numeric(expected_score_to_par),
      baseline_sd = vapply(mu_sg, baseline_sd_from_skill, numeric(1)),
      dd_yards_above_avg = (coalesce(driving_dist, PGA_AVG_DRIVING_YARDS) - PGA_AVG_DRIVING_YARDS),
      sd_dd_bump = SD_PER_YARD_ABOVE_AVG * pmax(0, dd_yards_above_avg),
      round_sd = if_else(
        is.finite(actual_sd),
        pmin(3.5, pmax(2.2, SD_BLEND_WEIGHT_BASELINE * (baseline_sd + sd_dd_bump) + (1 - SD_BLEND_WEIGHT_BASELINE) * actual_sd)),
        pmin(3.5, pmax(2.2, baseline_sd + sd_dd_bump))
      )
    ) %>%
    # Round-to-round: field SG/SD profiles at this course from historical_rounds_all (vs fixed 0.99, 0.97, …).
    mutate(
      .ri = pmin(4L, pmax(1L, dplyr::coalesce(as.integer(round), 1L))),
      round_mu_mult = ROUND_HIST_SG_MULT[.ri],
      round_sd_mult = ROUND_HIST_SD_MULT[.ri],
      mu_sg = pmax(-4, pmin(4, as.numeric(mu_sg) * as.numeric(round_mu_mult))),
      round_sd = pmin(3.8, pmax(2.0, as.numeric(round_sd) * as.numeric(round_sd_mult))),
      expected_score_to_par = -as.numeric(mu_sg),
      expected_round_score  = round_score_baseline_s4 + as.numeric(expected_score_to_par)
    ) %>%
    { if ("round" %in% names(.)) dplyr::group_by(., round) else . } %>%
    dplyr::arrange(expected_round_score) %>%
    dplyr::mutate(position = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-course_fit_adj, -w_decomp, -baseline_sd, -dd_yards_above_avg, -sd_dd_bump, -actual_sd, -driving_acc_hist, -round_mu_mult, -round_sd_mult, -base_mu_hist, -base_mu_sr, -w_sr, -skill_total_sg, -skill_driving_dist, -.ri)

  # Counts + GIR/fairways from shot MC + historical fallbacks (section 3); restore after decomp/course-fit mu_sg tweaks.
  if (exists("projection_counts_round_tbl") && !is.null(projection_counts_round_tbl) && nrow(projection_counts_round_tbl) > 0) {
    dg_sim <- dg_sim %>%
      dplyr::select(-dplyr::any_of(c("eagles", "birdies", "pars", "bogeys", "doubles", "gir", "fairways"))) %>%
      dplyr::left_join(projection_counts_round_tbl, by = c("dg_id", "round"))
  }

  # Section 3 counts used pre–course-fit / pre–round-multiplier mu_sg; total_score was recomputed above from final mu_sg.
  # Re-derive expected score-type counts from final mu_sg so Median / O/U count markets match the same skill level.
  if (!is.null(hole_data_sg_rates) && nrow(hole_data_sg_rates) > 0 && nrow(dg_sim) > 0 && "mu_sg" %in% names(dg_sim) &&
      exists("np_sg") && is.list(np_sg) && all(is.finite(c(np_sg$n3, np_sg$n4, np_sg$n5)))) {
    tryCatch({
      n_sim <- nrow(dg_sim)
      ec_mat <- t(vapply(seq_len(n_sim), function(i) {
        out <- expected_counts_from_sg_lookup(
          as.numeric(dg_sim$mu_sg[i]),
          np_sg$n3, np_sg$n4, np_sg$n5,
          hole_data_sg_rates
        )
        if (is.null(out)) c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
        else c(out$exp_eagles, out$exp_birdies, out$exp_pars, out$exp_bogeys, out$exp_doubles)
      }, numeric(5)))
      ok_row <- apply(ec_mat, 1L, function(r) all(is.finite(r)))
      if (any(ok_row)) {
        dg_sim$eagles[ok_row]  <- ec_mat[ok_row, 1]
        dg_sim$birdies[ok_row] <- ec_mat[ok_row, 2]
        dg_sim$pars[ok_row]    <- ec_mat[ok_row, 3]
        dg_sim$bogeys[ok_row]  <- ec_mat[ok_row, 4]
        dg_sim$doubles[ok_row] <- ec_mat[ok_row, 5]
      }
    }, error = function(e) message("expected_counts_from_sg_lookup (post mu_sg): ", conditionMessage(e)))
  }

  # Monte Carlo tournament: either shot-level empirical round noise (preferred) or Gaussian N(mu_sg, round_sd).
  sim_outrights <- NULL
  if (nrow(dg_sim) > 0 && "round_sd" %in% names(dg_sim) && "mu_sg" %in% names(dg_sim)) {
    tryCatch({
      players_sim <- dg_sim %>% dplyr::filter(round == 1L) %>%
        dplyr::select(dg_id, player_name, mu_sg, round_sd, dplyr::any_of("position")) %>%
        dplyr::mutate(
          mu_sg = as.numeric(pmax(-4, pmin(4, mu_sg))),
          round_sd = as.numeric(pmax(2.0, pmin(3.5, round_sd)))
        )
      # If skill has no variation (e.g. no historical data), sim would give everyone ~43% make cut.
      # Derive mu_sg from position so top of field is favored and make-cut odds are sensible.
      if ("position" %in% names(players_sim)) {
        n_players <- nrow(players_sim)
        pos <- as.numeric(players_sim$position)
        mu_range <- diff(range(players_sim$mu_sg, na.rm = TRUE))
        if (!is.finite(mu_range) || mu_range < 0.5) {
          # Linear scale: position 1 -> +2, position n -> -2
          players_sim$mu_sg <- 2 - 4 * (pos - 1) / max(1, n_players - 1)
          players_sim$mu_sg <- pmax(-4, pmin(4, players_sim$mu_sg))
        }
      }
      n_players <- nrow(players_sim)
      if (n_players >= 2L) {
        shot_rds_path <- file.path(model_dir, "data", "shot_transition_model.rds")
        use_shot_mc <- isTRUE(file.exists(shot_rds_path)) && Sys.getenv("GOLF_USE_SHOT_LEVEL_MC", "1") != "0"
        shot_fit <- NULL
        holes_par_mc <- NULL
        holes_yardage_mc <- NULL
        if (use_shot_mc) {
          shot_obj <- tryCatch(readRDS(shot_rds_path), error = function(e) NULL)
          if (!is.null(shot_obj) && is.list(shot_obj) && !is.null(shot_obj$fit)) {
            shot_fit <- shot_obj$fit
            meta_mc <- shot_obj$meta %||% list()
            holes_par_mc <- as.integer(unlist(meta_mc$holes_par))
            holes_yardage_mc <- as.integer(round(as.numeric(unlist(meta_mc$holes_yardage))))
            # Align MC with live tournament layout (written by refresh_current_course_hole_template above)
            ch_path_mc <- file.path(model_dir, "data", "current_course_holes.rds")
            if (file.exists(ch_path_mc)) {
              ch_mc <- tryCatch(readRDS(ch_path_mc), error = function(e) NULL)
              if (!is.null(ch_mc) && length(ch_mc$holes_par) == 18L && length(ch_mc$holes_yardage) == 18L) {
                skc <- normalize_course_name(ch_mc$course %||% "")
                sks <- normalize_course_name(SELECTED_COURSE %||% "")
                if (nzchar(skc) && nzchar(sks) && identical(skc, sks)) {
                  holes_par_mc <- as.integer(ch_mc$holes_par)
                  holes_yardage_mc <- as.integer(round(as.numeric(ch_mc$holes_yardage)))
                }
              }
            }
            if (length(holes_par_mc) != 18L || length(holes_yardage_mc) != 18L || anyNA(holes_par_mc)) {
              shot_fit <- NULL
            }
          } else {
            shot_fit <- NULL
          }
          if (is.null(shot_fit)) use_shot_mc <- FALSE
        }

        cut_line <- min(65L, n_players)
        baseline <- as.numeric(round_score_baseline_s4)
        avg_stp <- as.numeric(course_avg_score_to_par)
        mu_mult <- if (exists("ROUND_HIST_SG_MULT") && length(ROUND_HIST_SG_MULT) == 4L) as.numeric(ROUND_HIST_SG_MULT) else c(1.00, 0.99, 0.97, 0.95)
        sd_mult <- if (exists("ROUND_HIST_SD_MULT") && length(ROUND_HIST_SD_MULT) == 4L) as.numeric(ROUND_HIST_SD_MULT) else c(1.00, 1.01, 1.03, 1.05)
        FORM_SD <- 0.25

        if (isTRUE(use_shot_mc)) {
          if (!exists("shot_level_mc_tournament_calibrated", mode = "function")) {
            source(file.path(model_dir, "R", "shot_level_model.R"), encoding = "UTF-8")
          }
          if (!exists("dg_id_to_pga_player_id", mode = "function")) {
            source(file.path(model_dir, "R", "player_id_mapping.R"), encoding = "UTF-8")
          }
          pga_map_tbl <- load_pga_datagolf_map(file.path(model_dir, "data", "pga_datagolf_player_map.csv"))
          pga_ids_mc <- dg_id_to_pga_player_id(players_sim$dg_id, pga_map_tbl)
          # Fewer outer sims when each sim runs full shot paths (field-size scaled)
          N_SIM_TOURNAMENT <- min(500L, max(150L, as.integer(2200L %/% max(1L, n_players))))
          res_mc <- shot_level_mc_tournament_calibrated(
            mu_sg = players_sim$mu_sg,
            pga_player_ids = pga_ids_mc,
            holes_par = holes_par_mc,
            holes_yardage = holes_yardage_mc,
            fit = shot_fit,
            baseline = baseline,
            avg_stp = avg_stp,
            mu_mult = mu_mult,
            form_sd = FORM_SD,
            n_sims = N_SIM_TOURNAMENT,
            player_weight = 0.65,
            seed = 42L,
            cut_line = cut_line
          )
          sim_outrights <- tibble(
            dg_id = players_sim$dg_id,
            sim_win = res_mc$sim_win,
            sim_top_5 = res_mc$sim_top_5,
            sim_top_10 = res_mc$sim_top_10,
            sim_top_20 = res_mc$sim_top_20,
            sim_make_cut = res_mc$sim_make_cut
          )
          message(
            "Tournament MC: shot-level (n=", N_SIM_TOURNAMENT, ", tour_mu_round≈",
            round(res_mc$tour_mu_round, 3), ")"
          )
        } else {
          N_SIM_TOURNAMENT <- 1000L
          set.seed(42L)
          wins <- integer(n_players)
          t5 <- integer(n_players)
          t10 <- integer(n_players)
          t20 <- integer(n_players)
          mc <- integer(n_players)
          for (iter in seq_len(N_SIM_TOURNAMENT)) {
            form_shock <- stats::rnorm(n_players, mean = 0, sd = FORM_SD)
            total_72 <- vapply(seq_len(n_players), function(p) {
              sg_draw <- vapply(1L:4L, function(r) {
                stats::rnorm(
                  1L,
                  mean = players_sim$mu_sg[p] * mu_mult[r] + form_shock[p],
                  sd   = players_sim$round_sd[p] * sd_mult[r]
                )
              }, numeric(1))
              round_scores <- baseline + (avg_stp - as.numeric(sg_draw))
              sum(round_scores, na.rm = TRUE)
            }, numeric(1))
            rk <- rank(total_72, ties.method = "random")
            wins <- wins + (rk == 1L)
            t5  <- t5  + (rk <= 5L)
            t10 <- t10 + (rk <= 10L)
            t20 <- t20 + (rk <= 20L)
            mc  <- mc  + (rk <= cut_line)
          }
          sim_outrights <- tibble(
            dg_id = players_sim$dg_id,
            sim_win = pmin(0.95, pmax(0.001, as.numeric(wins) / N_SIM_TOURNAMENT)),
            sim_top_5 = pmin(0.95, pmax(0.001, as.numeric(t5) / N_SIM_TOURNAMENT)),
            sim_top_10 = pmin(0.95, pmax(0.001, as.numeric(t10) / N_SIM_TOURNAMENT)),
            sim_top_20 = pmin(0.95, pmax(0.001, as.numeric(t20) / N_SIM_TOURNAMENT)),
            sim_make_cut = pmin(0.95, pmax(0.001, as.numeric(mc) / N_SIM_TOURNAMENT))
          )
          message("Tournament MC: Gaussian rounds (n=", N_SIM_TOURNAMENT, ")")
        }
      }
    }, error = function(e) message("Tournament simulation: ", conditionMessage(e)))
  }
  if (!is.null(sim_outrights) && nrow(sim_outrights) > 0) {
    dg_sim <- dg_sim %>%
      dplyr::left_join(sim_outrights %>% dplyr::select(dg_id, sim_win, sim_top_5, sim_top_10, sim_top_20, sim_make_cut), by = "dg_id") %>%
      dplyr::mutate(
        win      = dplyr::coalesce(sim_win, win),
        top_5    = dplyr::coalesce(sim_top_5, top_5),
        top_10   = dplyr::coalesce(sim_top_10, top_10),
        top_20   = dplyr::coalesce(sim_top_20, top_20),
        make_cut = dplyr::coalesce(sim_make_cut, make_cut)
      ) %>%
      dplyr::select(-dplyr::any_of(c("sim_win", "sim_top_5", "sim_top_10", "sim_top_20", "sim_make_cut")))
  }

  simulated_round_table <<- dg_sim %>%
    mutate(
      total_score  = as.numeric(expected_round_score),
      score_to_par = as.numeric(expected_score_to_par),
      gir          = as.numeric(gir),
      fairways     = as.numeric(fairways),
      eagles       = as.numeric(eagles),
      birdies      = as.numeric(birdies),
      pars         = as.numeric(pars),
      bogeys       = as.numeric(bogeys),
      doubles      = as.numeric(doubles),
      course_used  = DISPLAY_COURSE %||% SELECTED_COURSE
    ) %>%
    dplyr::select(
      dg_id, player_name, position, total_score, score_to_par, gir, fairways,
      mu_sg, implied_mu_sg, round_sd,
      win, top_5, top_10, top_20, make_cut,
      eagles, birdies, pars, bogeys, doubles,
      round_label, course_used, next_round, round,
      dplyr::any_of(c("sg_app", "driving_dist"))
    )
  # Also write a static snapshot for Shiny to use in low-memory environments (e.g. shinyapps.io)
  static_path <- file.path(model_dir, "simulated_round_static.rds")
  tryCatch(
    saveRDS(simulated_round_table, static_path),
    error = function(e) message("Could not write static projections RDS: ", conditionMessage(e))
  )
}
