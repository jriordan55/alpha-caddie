# Fetch DataGolf live in-play finish probabilities (updates ~every 5 minutes during events).
#
# Writes alpha-caddie-web/live-in-play.json (same shape as API) for the static app to poll.
# Never put your API key in the browser; this script runs server-side or locally.
#
# Usage:
#   Rscript scripts/fetch_datagolf_in_play.R
#   Rscript scripts/fetch_datagolf_in_play.R euro
#
# Key: DATAGOLF_API_KEY env, or alpha-caddie-web/datagolf.local.json { "apiKey": "..." } (gitignored).

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite")
})

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a

model_dir <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) {
  Sys.getenv("GOLF_MODEL_DIR")
} else {
  cmd <- commandArgs(trailingOnly = FALSE)
  idx <- grep("^--file=", cmd)
  if (length(idx)) {
    this_script <- sub("^--file=", "", cmd[idx[1]], fixed = TRUE)
    dirname(dirname(normalizePath(this_script, winslash = "/", mustWork = FALSE)))
  } else {
    getwd()
  }
}

load_datagolf_api_key <- function(root = model_dir) {
  k <- trimws(Sys.getenv("DATAGOLF_API_KEY", ""))
  if (nzchar(k)) return(k)
  p <- file.path(root, "alpha-caddie-web", "datagolf.local.json")
  if (!file.exists(p)) return("")
  raw <- tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(raw)) return("")
  kk <- raw$apiKey %||% raw$key %||% ""
  trimws(as.character(kk))
}

primary_tour <- if (length(commandArgs(trailingOnly = TRUE))) {
  trimws(as.character(commandArgs(trailingOnly = TRUE)[1]))
} else {
  trimws(Sys.getenv("GOLF_DATAGOLF_TOUR", "pga"))
}
if (!nzchar(primary_tour)) primary_tour <- "pga"
fallback_tour <- trimws(Sys.getenv("GOLF_IN_PLAY_FALLBACK_TOUR", "opp"))
dead_heat <- trimws(Sys.getenv("GOLF_IN_PLAY_DEAD_HEAT", "no"))
if (!nzchar(dead_heat)) dead_heat <- "no"
odds_fmt <- trimws(Sys.getenv("GOLF_IN_PLAY_ODDS_FORMAT", "percent"))
if (!nzchar(odds_fmt)) odds_fmt <- "percent"

key <- load_datagolf_api_key()
if (!nzchar(key)) {
  message("fetch_datagolf_in_play: set DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json")
  quit(status = 1)
}

in_play_url <- function(tour) {
  sprintf(
    "https://feeds.datagolf.com/preds/in-play?tour=%s&dead_heat=%s&odds_format=%s&file_format=json&key=%s",
    utils::URLencode(tour, reserved = TRUE),
    utils::URLencode(dead_heat, reserved = TRUE),
    utils::URLencode(odds_fmt, reserved = TRUE),
    utils::URLencode(key, reserved = TRUE)
  )
}

tours_try <- unique(c(primary_tour, fallback_tour))
tours_try <- tours_try[nzchar(tours_try)]
parsed <- NULL
tour_used <- NA_character_
for (tr in tours_try) {
  u <- in_play_url(tr)
  parsed <- tryCatch(
    jsonlite::fromJSON(u, simplifyVector = FALSE),
    error = function(e) {
      message("fetch_datagolf_in_play: request failed (tour=", tr, "): ", conditionMessage(e))
      NULL
    }
  )
  if (is.list(parsed) && is.list(parsed$data) && length(parsed$data) > 0L) {
    tour_used <- tr
    if (!identical(tr, tours_try[[1]])) message("fetch_datagolf_in_play: using tour ", tr, " (primary had 0 players)")
    break
  }
  parsed <- NULL
}
if (!is.list(parsed) || !is.list(parsed$data)) {
  message("fetch_datagolf_in_play: unexpected JSON or empty data[] for tours: ", paste(tours_try, collapse = ", "))
  quit(status = 1)
}

out <- file.path(model_dir, "alpha-caddie-web", "live-in-play.json")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(parsed, out, pretty = TRUE, auto_unbox = TRUE, null = "null")
message("fetch_datagolf_in_play: wrote ", normalizePath(out, winslash = "/", mustWork = FALSE),
        " (", length(parsed$data), " players, tour=", tour_used, ", dead_heat=", dead_heat, ", odds_format=", odds_fmt, ")")
