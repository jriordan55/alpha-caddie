# Optional R refresh for data/historical_rounds_all.csv (PGA slice only). Static web path: npm run update:rounds (Node, PGA+LIV).
# Usage: Rscript scripts/update_historical_rounds_datagolf.R <repo_root>
# Requires: DATAGOLF_API_KEY and/or alpha-caddie-web/datagolf.local.json (see live_data.R).

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) {
  normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  rd <- Sys.getenv("GOLF_MODEL_DIR", unset = "")
  if (nzchar(rd)) normalizePath(rd, winslash = "/", mustWork = TRUE) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

Sys.setenv(GOLF_MODEL_DIR = repo)

live_data_path <- file.path(repo, "live_data.R")
if (!file.exists(live_data_path)) {
  message("update_historical_rounds_datagolf: missing ", live_data_path)
  quit(save = "no", status = 1L)
}

message("update_historical_rounds_datagolf: repo=", repo)
# live_data.R uses %>% in update_historical_rounds_live(); Rscript does not attach dplyr.
suppressPackageStartupMessages({
  library(dplyr)
})
source(live_data_path, local = FALSE)
if (!nzchar(API_KEY %||% "")) {
  message("update_historical_rounds_datagolf: set DATAGOLF_API_KEY or create alpha-caddie-web/datagolf.local.json with apiKey.")
  quit(save = "no", status = 1L)
}
update_historical_rounds_live()
message("update_historical_rounds_datagolf: done.")
