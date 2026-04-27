#!/usr/bin/env Rscript
# One command: full internal pipeline + export (sim placement, sim round stats when MC runs).
#
# Child steps use the same R installation as this process (R.home("bin")/Rscript).
#
# Env set for both steps:
#   GOLF_MODEL_DIR, GOLF_RAW_PROJECTIONS=0, GOLF_PLACEMENT_SOURCE=sim
#   GOLF_POLL_DATAGOLF_LIVE=0 unless you already exported a non-empty value
#
# Requires: DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json

options(warn = 1)

env_repo <- trimws(Sys.getenv("GOLF_MODEL_DIR", ""))
args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]], fixed = TRUE) else NA_character_
repo <- if (nzchar(env_repo) && file.exists(file.path(env_repo, "round_projections.R"))) {
  normalizePath(env_repo, winslash = "/", mustWork = FALSE)
} else if (length(script_path) && nzchar(script_path) && !is.na(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
if (!file.exists(rscript)) {
  message("Could not find Rscript at ", rscript)
  quit(status = 1)
}

e <- Sys.getenv()
e["GOLF_MODEL_DIR"] <- repo
e["GOLF_RAW_PROJECTIONS"] <- "0"
e["GOLF_PLACEMENT_SOURCE"] <- "sim"
if (!nzchar(trimws(Sys.getenv("GOLF_POLL_DATAGOLF_LIVE", "")))) {
  e["GOLF_POLL_DATAGOLF_LIVE"] <- "0"
}

run_step <- function(label, argv) {
  message("[", label, "] ", paste(argv, collapse = " "))
  # Do not capture stdout/stderr so errors from round_projections.R are visible in the terminal.
  rc <- system2(rscript, argv, wait = TRUE, env = e)
  if (!is.null(rc) && rc != 0L) {
    message(
      "Step failed (process exit ", rc, "). Typical causes:\n",
      "  - DATAGOLF_API_KEY missing or invalid (or no alpha-caddie-web/datagolf.local.json with apiKey)\n",
      "  - Empty field: no PGA field-updates rows and no preds/in-play / pre-tournament fallback (off-season or wrong GOLF_DATAGOLF_TOUR)\n",
      "  - Scroll up for the first 'Error in' / 'Field updates:' line from R.\n",
      "Re-run after fixing; npm will still report exit 1 even if Windows returned another code."
    )
    quit(save = "no", status = 1L)
  }
}

rp <- normalizePath(file.path(repo, "round_projections.R"), winslash = "/", mustWork = FALSE)
ex <- normalizePath(file.path(repo, "scripts", "export_projections_for_website.R"), winslash = "/", mustWork = FALSE)
if (!file.exists(rp)) {
  message("Missing ", rp)
  quit(status = 1)
}
if (!file.exists(ex)) {
  message("Missing ", ex)
  quit(status = 1)
}

dg_key <- nzchar(trimws(Sys.getenv("DATAGOLF_API_KEY", "")))
if (!dg_key) {
  for (p in c(file.path(repo, "alpha-caddie-web", "datagolf.local.json"), file.path(repo, "website", "datagolf.local.json"))) {
    if (file.exists(p)) {
      dg_key <- TRUE
      break
    }
  }
}
if (!dg_key) {
  message(
    "No DataGolf credentials: set environment variable DATAGOLF_API_KEY, or add JSON with \"apiKey\" to:\n  ",
    file.path(repo, "alpha-caddie-web", "datagolf.local.json")
  )
  quit(save = "no", status = 1L)
}

run_step("round_projections.R", rp)
run_step("export_projections_for_website.R", ex)

message("Done: simulated_round_static.rds + website/public/data/projections.json + alpha-caddie-web/projections.json")
