#!/usr/bin/env Rscript
# Weekly Monday 6:00 AM job (see scripts/register_monday_republish_task.ps1):
#   1) live_update_all.R: DataGolf (rounds+SG, outrights, matchups) + shot-by-shot (all_shots CSV)
#   2) build_pga_datagolf_tournament_map.R
#   3) build_pga_datagolf_player_map.R
#   4) round_projections.R pass 1 -> golf_selected_course.txt + static RDS (pre-refit)
#   5) fit_shot_transition_model.R -> shot_transition_model.rds
#   6) round_projections.R pass 2 (uses fresh shot RDS)
#   7) Deploy Shiny app to shinyapps.io (optional: set SKIP_SHINY_DEPLOY=1)
#
# Requires: DATAGOLF_API_KEY (env or .Renviron), pgatouR for shot append, rsconnect for deploy.

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
Sys.setenv(GOLF_MODEL_DIR = repo_root)

message("[", Sys.time(), "] Monday full pipeline in: ", repo_root)

rscript_exe <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

#' Run an R script in a subprocess (clean global state for round_projections / fit).
run_rscript_file <- function(path, label = path) {
  exe <- rscript_exe()
  if (!file.exists(exe)) stop("Rscript not found at: ", exe)
  if (!file.exists(path)) stop("Missing script: ", path)
  message("[", Sys.time(), "] >>> ", label)
  # stdout/stderr to console so run_monday_republish.ps1 log captures subprocess output
  exit <- system2(exe, args = c("--vanilla", path), stdout = TRUE, stderr = TRUE, cwd = repo_root)
  ok <- length(exit) == 1L && is.numeric(exit) && !is.na(exit) && as.integer(exit) == 0L
  if (!ok) {
    stop("Step failed (exit ", paste(exit, collapse = ","), "): ", label)
  }
  invisible(TRUE)
}

# ---- 1) DataGolf historical CSVs: rounds+stats+SG, outrights, matchups ----
lu_path <- file.path(repo_root, "live_update_all.R")
if (!file.exists(lu_path)) stop("Missing: ", lu_path)
message("[", Sys.time(), "] (1/7) live_update_all: DataGolf + shot-by-shot (all_shots)")
source(lu_path, encoding = "UTF-8")
if (!exists("live_update_all_2026")) {
  stop("live_update_all.R did not define live_update_all_2026()")
}
live_update_all_2026()

# ---- 2) Tournament map (helps shot filter for upcoming course) ----
tmap_script <- file.path(repo_root, "scripts", "build_pga_datagolf_tournament_map.R")
if (file.exists(tmap_script) && file.exists(file.path(repo_root, "data", "historical_rounds_all.csv"))) {
  message("[", Sys.time(), "] (2/7) build_pga_datagolf_tournament_map.csv")
  tryCatch(
    run_rscript_file(tmap_script, "build_pga_datagolf_tournament_map.R"),
    error = function(e) message("Tournament map step failed (continuing): ", conditionMessage(e))
  )
}

# ---- 3) Player map (shot model <-> dg_id) ----
pmap_script <- file.path(repo_root, "scripts", "build_pga_datagolf_player_map.R")
if (file.exists(pmap_script)) {
  message("[", Sys.time(), "] (3/7) build_pga_datagolf_player_map.csv")
  tryCatch(
    run_rscript_file(pmap_script, "build_pga_datagolf_player_map.R"),
    error = function(e) message("Player map step failed (continuing): ", conditionMessage(e))
  )
} else {
  message("Missing scripts/build_pga_datagolf_player_map.R — skipping player map.")
}

# ---- 4) First projections run: DataGolf field-updates -> golf_selected_course.txt + simulated_round_static.rds ----
proj_path <- file.path(repo_root, "round_projections.R")
message("[", Sys.time(), "] (4/7) round_projections.R — course + static table (before shot refit)")
run_rscript_file(proj_path, "round_projections.R (pass 1)")

# ---- 5) Refit shot model for course in golf_selected_course.txt ----
fit_script <- file.path(repo_root, "scripts", "fit_shot_transition_model.R")
shots_csv <- file.path(repo_root, "data", "all_shots_2021_2026.csv")
if (file.exists(fit_script) && file.exists(shots_csv)) {
  message("[", Sys.time(), "] (5/7) fit_shot_transition_model.R -> shot_transition_model.rds")
  tryCatch(
    run_rscript_file(fit_script, "fit_shot_transition_model.R"),
    error = function(e) {
      message("Shot refit failed (continuing without new RDS): ", conditionMessage(e))
    }
  )
} else {
  message("[", Sys.time(), "] (5/7) Skipping shot refit (missing fit script or all_shots CSV).")
}

# ---- 6) Second projections run: picks up new shot_transition_model.rds ----
message("[", Sys.time(), "] (6/7) round_projections.R — static table with fresh shot model")
run_rscript_file(proj_path, "round_projections.R (pass 2)")

# ---- 7) Deploy app (bundles app.R + data + RDS) ----
if (Sys.getenv("SKIP_SHINY_DEPLOY", "") %in% c("1", "true", "TRUE", "yes")) {
  message("[", Sys.time(), "] (7/7) SKIP_SHINY_DEPLOY set — skipping rsconnect::deployApp.")
} else {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    message("[", Sys.time(), "] (7/7) Package rsconnect not installed — skipping deploy. Install with install.packages('rsconnect').")
  } else {
    message("[", Sys.time(), "] (7/7) Deploying app to shinyapps.io ...")
    rsconnect::deployApp(
      appDir = repo_root,
      appName = "AlphaCaddie",
      account = "jriordan55",
      server = "shinyapps.io",
      launch.browser = FALSE,
      forceUpdate = TRUE
    )
  }
}

message("[", Sys.time(), "] Monday full pipeline complete.")
