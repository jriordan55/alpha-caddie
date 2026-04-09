# Backfill data/all_shots_2021_2026.csv using pgatouR only (not DataGolf):
# pull every PGA tournament whose schedule end date is after the latest end date among
# tournament_ids already in the file, through Sys.Date(), plus in-progress events not yet stored.
# See helper.R::backfill_shots_after_csv_anchor().
#
# Usage: Rscript scripts/update_latest_shots.R <repo_root>
# Requires: pgatouR package, internet (PGA feeds).
#
# Env:
#   GOLF_MODEL_DIR     - repo root if no arg
#   GOLF_SHOTS_FORCE=1 - re-pull even if tournament_id already in CSV
#   GOLF_SKIP_SHOTS_UPDATE=1 - exit 0 immediately (for CI / machines without pgatouR)

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) {
  normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  rd <- Sys.getenv("GOLF_MODEL_DIR", unset = "")
  if (nzchar(rd)) normalizePath(rd, winslash = "/", mustWork = TRUE) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

Sys.setenv(GOLF_MODEL_DIR = repo)

if (identical(Sys.getenv("GOLF_SKIP_SHOTS_UPDATE", ""), "1")) {
  message("update_latest_shots: GOLF_SKIP_SHOTS_UPDATE=1 — skipping.")
  quit(save = "no", status = 0L)
}

helper_path <- file.path(repo, "helper.R")
if (!file.exists(helper_path)) {
  message("update_latest_shots: missing ", helper_path)
  quit(save = "no", status = 1L)
}

if (!requireNamespace("pgatouR", quietly = TRUE)) {
  message("update_latest_shots: package 'pgatouR' is not installed. Install with install.packages('pgatouR') or set GOLF_SKIP_SHOTS_UPDATE=1.")
  quit(save = "no", status = 1L)
}

source(helper_path, local = FALSE)

out_csv <- file.path(repo, "data", "all_shots_2021_2026.csv")
out_prog <- file.path(repo, "data", "all_shots_progress.rds")
force <- identical(Sys.getenv("GOLF_SHOTS_FORCE", ""), "1")

message("update_latest_shots: repo=", repo, if (force) " (GOLF_SHOTS_FORCE=1)" else "")
message("update_latest_shots: pgatouR only — backfill tournaments after latest schedule date in CSV through today.")
backfill_shots_after_csv_anchor(
  output_csv = out_csv,
  progress_rds = out_prog,
  years = NULL,
  rounds = 1:4,
  sleep_seconds = 0.03,
  max_events = NULL,
  force = force
)
message("update_latest_shots: done.")
