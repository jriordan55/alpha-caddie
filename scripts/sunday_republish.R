#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x)) y else x
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

message("[", Sys.time(), "] Starting Sunday republish job in: ", repo_root)

# Latest PGA shots → append to data/all_shots_2021_2026.csv if not already present (requires pgatouR).
helper_path <- file.path(repo_root, "helper.R")
if (file.exists(helper_path)) {
  message("[", Sys.time(), "] Latest-tournament shot append (helper.R)...")
  tryCatch(
    {
      helper_env <- new.env(parent = globalenv())
      sys.source(helper_path, envir = helper_env)
      fn <- helper_env[["append_latest_tournament_shots"]]
      if (is.function(fn)) {
        fn(
          output_csv = file.path(repo_root, "data", "all_shots_2021_2026.csv"),
          progress_rds = file.path(repo_root, "data", "all_shots_progress.rds"),
          rounds = 1:4,
          sleep_seconds = 0.03,
          force = FALSE
        )
      } else {
        message("append_latest_tournament_shots() not found after sourcing helper.R; skipping.")
      }
    },
    error = function(e) {
      message("Shot append step failed (continuing with projections/deploy): ", conditionMessage(e))
    }
  )
} else {
  message("helper.R not found; skipping shot append.")
}

# Refresh pgatouR <-> DataGolf ID map (uses historical_rounds_all + field-updates when API key set).
map_r <- file.path(repo_root, "R", "player_id_mapping.R")
map_csv <- file.path(repo_root, "data", "pga_datagolf_player_map.csv")
if (file.exists(map_r) && file.exists(file.path(repo_root, "data", "historical_rounds_all.csv"))) {
  message("[", Sys.time(), "] Building pga_datagolf_player_map.csv...")
  tryCatch(
    {
      sys.source(map_r, envir = e_map <- new.env(parent = globalenv()))
      fn <- e_map[["build_pga_datagolf_map"]]
      if (is.function(fn)) {
        map_df <- fn(
          historical_rounds_path = file.path(repo_root, "data", "historical_rounds_all.csv"),
          tour_code = "R",
          api_key = Sys.getenv("DATAGOLF_API_KEY", "")
        )
        write.csv(map_df, map_csv, row.names = FALSE)
        message("Wrote ", map_csv, " (", nrow(map_df), " rows).")
      }
    },
    error = function(e) {
      message("Player ID map build failed (continuing): ", conditionMessage(e))
    }
  )
}

run_if_exists <- function(path) {
  if (!file.exists(path)) return(FALSE)
  message("[", Sys.time(), "] Running script: ", path)
  source(path, local = new.env(parent = globalenv()))
  TRUE
}

# Prefer explicit round summary script when present.
did_run <- run_if_exists(file.path(repo_root, "round_summary.R"))
if (!did_run) {
  message("[", Sys.time(), "] round_summary.R not found; using round_projections.R fallback")
  did_run <- run_if_exists(file.path(repo_root, "round_projections.R"))
}

if (!did_run) stop("No pre-publish script found (expected round_summary.R or round_projections.R).")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is not installed. Run install.packages('rsconnect').")
}

# Deploy using values already tracked in rsconnect metadata.
message("[", Sys.time(), "] Deploying app to shinyapps.io...")
rsconnect::deployApp(
  appDir = repo_root,
  appName = "AlphaCaddie",
  account = "jriordan55",
  server = "shinyapps.io",
  launch.browser = FALSE,
  forceUpdate = TRUE
)

message("[", Sys.time(), "] Sunday republish job complete.")
