#!/usr/bin/env Rscript
# Build data/pga_datagolf_tournament_map.csv — pgatouR tournament_id <-> DataGolf event_id by year + name.
# Requires: pgatouR, data/historical_rounds_all.csv, builds schedule via build_schedule_course_map.

options(warn = 1)

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

sched_path <- file.path(repo_root, "data", "pga_tournament_course_map.csv")
if (file.exists(sched_path)) {
  schedule_map <- utils::read.csv(sched_path, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  schedule_map <- build_schedule_course_map(2021:2026)
  utils::write.csv(schedule_map, sched_path, row.names = FALSE)
}

hist_path <- file.path(repo_root, "data", "historical_rounds_all.csv")
message("Loading distinct DataGolf events from historical_rounds_all.csv ...")
hist_ev <- load_historical_events(hist_path)
message("Distinct event rows: ", nrow(hist_ev))

tmap <- build_pga_dg_tournament_map(schedule_map, hist_ev)
out_path <- file.path(repo_root, "data", "pga_datagolf_tournament_map.csv")
utils::write.csv(tmap, out_path, row.names = FALSE)
message("Wrote: ", out_path)
message("Matched with dg_event_id: ", sum(is.finite(tmap$dg_event_id)), " / ", nrow(tmap))
