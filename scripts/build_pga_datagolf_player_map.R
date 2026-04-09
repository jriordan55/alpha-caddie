#!/usr/bin/env Rscript
# Build data/pga_datagolf_player_map.csv for joining pgatouR player_id to DataGolf dg_id.
# Requires: pgatouR, readr; optional DATAGOLF_API_KEY for country tie-breaks from field-updates.

options(warn = 1)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else NA_character_
repo_root <- if (is.finite(nchar(script_path)) && nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
setwd(repo_root)

map_r <- file.path(repo_root, "R", "player_id_mapping.R")
if (!file.exists(map_r)) stop("Missing ", map_r)
source(map_r)

out_csv <- file.path(repo_root, "data", "pga_datagolf_player_map.csv")
message("Building mapping -> ", out_csv)
map_df <- build_pga_datagolf_map(
  historical_rounds_path = file.path(repo_root, "data", "historical_rounds_all.csv"),
  tour_code = "R",
  api_key = Sys.getenv("DATAGOLF_API_KEY", "")
)
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
write.csv(map_df, out_csv, row.names = FALSE)
message("Rows: ", nrow(map_df),
        " | last_first_exact: ", sum(map_df$match_method == "last_first_exact", na.rm = TRUE),
        " | name_key: ", sum(grepl("^name_key", map_df$match_method), na.rm = TRUE))
