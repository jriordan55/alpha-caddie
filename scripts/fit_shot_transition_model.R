#!/usr/bin/env Rscript
# Fit empirical shot transition model from all_shots_2021_2026.csv -> data/shot_transition_model.rds
# Uses SELECTED_COURSE: env SELECTED_COURSE, golf_selected_course.txt, or course_table.csv.
# Shot filtering (in order): (1) pgatouR tournament_ids that DataGolf historical data ties to SELECTED_COURSE
#   via pga_datagolf_tournament_map.csv; (2) else pgatouR schedule course_name; (3) else tour-wide.
#
# Usage: Rscript scripts/fit_shot_transition_model.R [max_rows] [--all-tour]
#   --all-tour  skip course filter (tour-wide empirical table)

options(warn = 1)

args <- commandArgs(trailingOnly = TRUE)
max_rows <- NA_integer_
all_tour <- FALSE
for (a in args) {
  if (a == "--all-tour") all_tour <- TRUE
  else if (!grepl("^-", a)) max_rows <- suppressWarnings(as.integer(a))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else NA_character_
repo_root <- if (is.finite(nchar(script_path)) && nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
setwd(repo_root)

source(file.path(repo_root, "R", "course_context.R"))
source(file.path(repo_root, "R", "tournament_pga_datagolf_mapping.R"))
source(file.path(repo_root, "R", "shot_level_model.R"))

shots_path <- file.path(repo_root, "data", "all_shots_2021_2026.csv")
if (!file.exists(shots_path)) stop("Missing ", shots_path)

selected <- read_selected_course(repo_root)
if (all_tour) selected <- NA_character_
message("SELECTED_COURSE for shot model: ", if (is.na(selected) || !nzchar(selected)) "(all tour / --all-tour)" else selected)

sched_path <- file.path(repo_root, "data", "pga_tournament_course_map.csv")
if (file.exists(sched_path)) {
  schedule_map <- utils::read.csv(sched_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  message("Building PGA schedule map (tournament_id -> course_name)...")
  schedule_map <- build_schedule_course_map(2021:2026)
  utils::write.csv(schedule_map, sched_path, row.names = FALSE)
  message("Wrote cache: ", sched_path)
}

message("Reading shots: ", shots_path, if (is.finite(max_rows)) paste0(" (max_rows=", max_rows, ")") else "")
if (requireNamespace("data.table", quietly = TRUE)) {
  if (is.finite(max_rows)) {
    sh <- data.table::fread(shots_path, nrows = max_rows, showProgress = TRUE)
  } else {
    sh <- data.table::fread(shots_path, showProgress = TRUE)
  }
  sh <- as.data.frame(sh)
} else {
  if (is.finite(max_rows)) {
    sh <- read.csv(shots_path, nrows = max_rows, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    sh <- read.csv(shots_path, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

message("Rows read: ", nrow(sh))
sh_raw_n <- nrow(sh)
sh_course <- NULL
shot_filter_source <- "all_tour"

if (!all_tour && nzchar(selected %||% "")) {
  hist_path <- file.path(repo_root, "data", "historical_rounds_all.csv")
  if (!file.exists(hist_path)) {
    warning("No historical_rounds_all.csv — cannot build PGA/DataGolf tournament map; using schedule filter only.")
    hist_ev <- NULL
    tmap <- NULL
  } else {
    message("Loading DataGolf events + building PGA <-> DataGolf tournament map...")
    hist_ev <- load_historical_events(hist_path)
    tmap <- build_pga_dg_tournament_map(schedule_map, hist_ev)
    tmap_path <- file.path(repo_root, "data", "pga_datagolf_tournament_map.csv")
    utils::write.csv(tmap, tmap_path, row.names = FALSE)
    message("Wrote ", tmap_path, " (dg matched: ", sum(is.finite(tmap$dg_event_id)), " / ", nrow(tmap), ")")
  }

  sh_dg <- if (!is.null(hist_ev)) filter_shots_for_selected_course_dg(sh, selected, tmap, hist_ev) else NULL

  if (!is.null(sh_dg) && nrow(sh_dg) >= 2000L) {
    sh_course <- sh_dg
    sh_fit <- sh_course
    shot_filter_source <- "datagolf_course_tournament_map"
  } else if (!is.null(sh_dg) && nrow(sh_dg) >= 1L) {
    message("DataGolf-mapped shots (", nrow(sh_dg), ") < 2000; trying pgatouR schedule course filter...")
    sh_sched <- filter_shots_for_selected_course(sh, selected, schedule_map)
    if (nrow(sh_sched) >= 2000L) {
      sh_course <- sh_sched
      sh_fit <- sh_course
      shot_filter_source <- "schedule_after_dg_sparse"
    } else {
      warning("Few shots after both filters. Fitting on ALL rows; distributions are tour-wide.")
      sh_fit <- sh
      shot_filter_source <- "tour_wide_fallback"
    }
  } else {
    message("No DataGolf course match for pgatouR tournaments; using schedule course_name filter.")
    sh_course <- filter_shots_for_selected_course(sh, selected, schedule_map)
    if (nrow(sh_course) >= 2000L) {
      sh_fit <- sh_course
      shot_filter_source <- "schedule_course_only"
    } else {
      warning("Few shots after schedule filter (", nrow(sh_course), "). Fitting on ALL rows; distributions are tour-wide.")
      sh_fit <- sh
      shot_filter_source <- "tour_wide_fallback"
    }
  }
} else {
  sh_fit <- sh
}

message("Fitting on ", nrow(sh_fit), " shot rows (input was ", sh_raw_n, ").")
message("Augmenting shots (state + yards)...")
sh_fit <- augment_shots(sh_fit)
message("Fitting transition tables...")
fit_tables <- fit_shot_transition_tables(sh_fit, min_player_shots = 30L)

# Hole template: use course-specific shots if enough; else course_table (matches SELECTED_COURSE par/yardage).
tpl <- course_hole_template_from_shots(if (!is.null(sh_course) && nrow(sh_course) >= 100L) sh_course else NULL)
if (is.null(tpl)) {
  message("Building hole template from data/course_table.csv for SELECTED_COURSE.")
  tpl <- course_hole_template_from_course_table(repo_root, selected %||% read_selected_course(repo_root))
}
if (is.null(tpl)) {
  tpl <- list(par = rep(4L, 18L), yardage = rep(400L, 18L))
}

out_rds <- file.path(repo_root, "data", "shot_transition_model.rds")
obj <- list(
  fit = fit_tables,
  meta = list(
    selected_course = selected,
    all_tour_flag = isTRUE(all_tour),
    shot_filter_source = shot_filter_source,
    holes_par = tpl$par,
    holes_yardage = tpl$yardage,
    n_shots_used = nrow(sh_fit),
    version = 3L
  )
)
saveRDS(obj, out_rds)
message("Saved: ", out_rds)
message("Tour states: ", length(fit_tables$tour), " | Players with own table: ", length(fit_tables$by_player))
