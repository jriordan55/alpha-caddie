# Export simulated_round_static.rds -> projections JSON for static sites.
#
# Run after round_projections.R (or your republish job):
#   Rscript scripts/export_projections_for_website.R
#
# Writes:
#   - website/public/data/projections.json  (Vite site)
#   - alpha-caddie-web/projections.json       (plain HTML app; fetch same schema)
#
# Env: GOLF_MODEL_DIR = repo root if not cwd.
#      GOLF_EXPORT_PROJECTIONS_EXTRA = optional extra path (one file) to also write the same JSON.
#      GOLF_POLL_DATAGOLF_LIVE = 1/true/yes → set meta.poll_datagolf_live_predictions so the static app
#        polls sibling live-in-play.json (from scripts/fetch_datagolf_in_play.R) for in-play finish probs.
#      GOLF_DATAGOLF_LIVE_POLL_SEC = optional client poll interval 30–600 (default 30).

suppressPackageStartupMessages({
  library(jsonlite)
  library(readr)
  library(dplyr)
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

ou_display_round_auto <- function(now = Sys.time(), tz = NULL) {
  if (is.null(tz) || !nzchar(tz)) tz <- Sys.getenv("GOLF_OU_TZ", "America/New_York")
  now_et <- tryCatch(as.POSIXlt(now, tz = tz), error = function(e) NULL)
  if (is.null(now_et)) return(1L)
  wday <- as.integer(now_et$wday)
  hour <- as.numeric(now_et$hour) + as.numeric(now_et$min) / 60 + as.numeric(now_et$sec) / 3600
  after_9pm <- is.finite(hour) && hour >= 21
  if (wday == 0L && after_9pm) return(1L)
  if (wday %in% 1L:3L) return(1L)
  if (wday == 4L && !after_9pm) return(1L)
  if (wday == 4L && after_9pm) return(2L)
  if (wday == 5L && !after_9pm) return(2L)
  if (wday == 5L && after_9pm) return(3L)
  if (wday == 6L && !after_9pm) return(3L)
  if (wday == 6L && after_9pm) return(4L)
  if (wday == 0L && !after_9pm) return(4L)
  1L
}

normalize_market <- function(x) {
  v <- trimws(tolower(as.character(x)))
  dplyr::case_when(
    v %in% c("total score", "round_score", "score", "round score", "total") ~ "Total Score",
    v %in% c("birdies", "birdie") ~ "Birdies",
    v %in% c("pars", "par") ~ "Pars",
    v %in% c("bogeys", "bogey") ~ "Bogeys",
    TRUE ~ NA_character_
  )
}

ensure_prop_cols <- function(tbl, default_market) {
  nm <- names(tbl)
  if (!"player_name" %in% nm) {
    if ("player" %in% nm) tbl$player_name <- tbl$player
    else if ("name" %in% nm) tbl$player_name <- tbl$name
    else if ("golfer" %in% nm) tbl$player_name <- tbl$golfer
    else return(NULL)
  }
  if (!"over_odds" %in% nm && "over" %in% nm) tbl$over_odds <- tbl$over
  if (!"under_odds" %in% nm && "under" %in% nm) tbl$under_odds <- tbl$under
  if (!all(c("player_name", "line", "over_odds", "under_odds") %in% names(tbl))) return(NULL)
  if ("stat" %in% names(tbl)) {
    tbl$market <- normalize_market(tbl$stat)
  } else if ("market" %in% names(tbl)) {
    tbl$market <- normalize_market(tbl$market)
  } else if ("prop_type" %in% names(tbl)) {
    tbl$market <- normalize_market(tbl$prop_type)
  } else {
    tbl$market <- default_market
  }
  tbl <- tbl %>% dplyr::filter(!is.na(.data$market))
  if (nrow(tbl) == 0) return(NULL)
  has_dg <- "dg_id" %in% names(tbl)
  out <- tbl %>%
    dplyr::transmute(
      player_name = trimws(as.character(.data$player_name)),
      line = as.numeric(.data$line),
      over_odds = as.numeric(.data$over_odds),
      under_odds = as.numeric(.data$under_odds),
      market = as.character(.data$market),
      dg_id = if (has_dg) suppressWarnings(as.integer(round(as.numeric(.data$dg_id)))) else NA_integer_
    ) %>%
    dplyr::mutate(
      line = dplyr::if_else(
        .data$market %in% c("Pars", "Birdies", "Bogeys") & is.finite(.data$line) & .data$line == floor(.data$line),
        .data$line + 0.5,
        .data$line
      )
    )
  if (!has_dg) {
    out <- dplyr::select(out, -.data$dg_id)
  }
  out
}

collect_props <- function(root) {
  paths <- list(
    "Total Score" = file.path(root, "data", "player_props_lines.csv"),
    "Birdies" = file.path(root, "data", "player_props_birdies.csv"),
    "Pars" = file.path(root, "data", "player_props_pars.csv"),
    "Bogeys" = file.path(root, "data", "player_props_bogeys.csv")
  )
  birdies_extra <- file.path(root, "data", "player_props_birdies_custom.csv")
  pieces <- list()
  for (mkt in names(paths)) {
    p <- paths[[mkt]]
    if (!file.exists(p)) next
    tbl <- tryCatch(read_csv(p, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(tbl) || nrow(tbl) == 0) next
    z <- ensure_prop_cols(tbl, mkt)
    if (!is.null(z) && nrow(z) > 0) pieces[[length(pieces) + 1L]] <- z
  }
  if (file.exists(birdies_extra)) {
    tbl <- tryCatch(read_csv(birdies_extra, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(tbl) && nrow(tbl) > 0) {
      z <- ensure_prop_cols(tbl, "Birdies")
      if (!is.null(z) && nrow(z) > 0) pieces[[length(pieces) + 1L]] <- z
    }
  }
  data_dir <- file.path(root, "data")
  if (dir.exists(data_dir)) {
    skip <- unlist(paths, use.names = FALSE)
    skip <- c(skip, birdies_extra)
    extra <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
    for (p in extra) {
      if (p %in% skip) next
      tbl <- tryCatch(read_csv(p, show_col_types = FALSE), error = function(e) NULL)
      if (is.null(tbl) || nrow(tbl) == 0) next
      z <- ensure_prop_cols(tbl, NA_character_)
      if (!is.null(z) && nrow(z) > 0) pieces[[length(pieces) + 1L]] <- z
    }
  }
  if (length(pieces) == 0) {
    return(tibble::tibble(
      player_name = character(),
      line = numeric(),
      over_odds = numeric(),
      under_odds = numeric(),
      market = character()
    ))
  }
  bind_rows(pieces) %>% dplyr::distinct(player_name, market, line, .keep_all = TRUE)
}

rds_path <- file.path(model_dir, "simulated_round_static.rds")
out_path <- file.path(model_dir, "website", "public", "data", "projections.json")

if (!file.exists(rds_path)) {
  message("Missing ", rds_path, " — run round_projections.R first.")
  quit(status = 1)
}

obj <- readRDS(rds_path)
model_next_round <- NA_integer_
if (is.data.frame(obj)) {
  tbl <- obj
  event_name <- ""
  course_used <- if ("course_used" %in% names(obj) && nrow(obj) > 0) {
    trimws(as.character(obj$course_used[1]))
  } else {
    ""
  }
} else if (is.list(obj) && is.data.frame(obj$data)) {
  tbl <- obj$data
  event_name <- trimws(as.character(obj$event_name %||% ""))
  course_used <- trimws(as.character(obj$course_used %||% ""))
  if (!nzchar(course_used) && "course_used" %in% names(tbl) && nrow(tbl) > 0) {
    course_used <- trimws(as.character(tbl$course_used[1]))
  }
  mr <- suppressWarnings(as.integer(obj$model_next_round %||% NA_integer_))
  if (is.finite(mr) && mr >= 1L && mr <= 4L) model_next_round <- mr
} else {
  stop("simulated_round_static.rds: expected data.frame or list(data = data.frame, ...)")
}

if (nrow(tbl) == 0) {
  message("simulated_round_static.rds has 0 rows; not writing projections.json")
  quit(status = 1)
}

want <- c(
  "dg_id", "player_name", "country", "round",
  "total_score", "round_sd", "score_to_par",
  "gir", "fairways", "eagles", "birdies", "pars", "bogeys", "doubles",
  "win", "top_5", "top_10", "top_20", "make_cut",
  "course_used", "mu_sg", "implied_mu_sg", "position", "round_label", "next_round",
  "driving_dist", "sg_app"
)
cols <- intersect(want, names(tbl))
players <- tbl[, cols, drop = FALSE]

props_df <- collect_props(model_dir)

tz_lab <- Sys.getenv("GOLF_OU_TZ", "America/New_York")
auto_r <- if (is.finite(model_next_round) && model_next_round %in% 1L:4L) {
  as.integer(model_next_round)
} else {
  ou_display_round_auto()
}
rnd_lab <- switch(as.character(auto_r),
  "1" = "R1 — next Thursday",
  "2" = "R2 — Friday",
  "3" = "R3 — Saturday",
  "4" = "R4 — Sunday",
  paste0("R", auto_r)
)
round_src <- if (is.finite(model_next_round) && model_next_round %in% 1L:4L) "model" else "auto"
display_round_label <- paste0(rnd_lab, " (", round_src, ", ", tz_lab, ")")

poll_live_raw <- tolower(trimws(Sys.getenv("GOLF_POLL_DATAGOLF_LIVE", "")))
poll_live_off <- poll_live_raw %in% c("0", "false", "no", "off")
# Poll in-play during any model round (R1–R4) so Thursday pricing picks up live standings, not only R2+.
poll_live <- !poll_live_off &&
  (poll_live_raw %in% c("1", "true", "yes") ||
    (is.finite(model_next_round) && model_next_round >= 1L && model_next_round <= 4L))
# Client checks preds/in-play often but only re-merges when info.last_update changes (~DataGolf cadence).
live_poll_sec <- suppressWarnings(as.integer(Sys.getenv("GOLF_DATAGOLF_LIVE_POLL_SEC", "30")))
if (!is.finite(live_poll_sec) || live_poll_sec < 30L) live_poll_sec <- 30L
if (live_poll_sec > 600L) live_poll_sec <- 600L

payload <- list(
  event_name = event_name,
  course_used = course_used,
  display_round = as.integer(auto_r),
  display_round_label = display_round_label,
  updated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  source = "simulated_round_static.rds (round_projections.R)",
  players = players,
  props = props_df
)
# Browser silent-refresh interval for projections.json (book odds / +EV). Override: GOLF_CLIENT_PROJECTIONS_POLL_SEC
proj_poll_sec <- suppressWarnings(as.integer(Sys.getenv("GOLF_CLIENT_PROJECTIONS_POLL_SEC", "30")))
if (!is.finite(proj_poll_sec) || proj_poll_sec < 15L) proj_poll_sec <- 30L
if (proj_poll_sec > 3600L) proj_poll_sec <- 3600L
payload$projections_poll_interval_sec <- proj_poll_sec
if (poll_live) {
  payload$poll_datagolf_live_predictions <- TRUE
  payload$datagolf_live_poll_interval_sec <- live_poll_sec
  payload$live_matchup_model_blend <- 0.35
}

out_paths <- c(
  out_path,
  file.path(model_dir, "alpha-caddie-web", "projections.json")
)
extra_export <- trimws(Sys.getenv("GOLF_EXPORT_PROJECTIONS_EXTRA", ""))
if (nzchar(extra_export)) out_paths <- c(out_paths, extra_export)
out_paths <- unique(out_paths[nzchar(out_paths)])

for (op in out_paths) {
  dir.create(dirname(op), recursive = TRUE, showWarnings = FALSE)
  write_json(
    payload,
    op,
    pretty = TRUE,
    dataframe = "rows",
    na = "null",
    auto_unbox = TRUE
  )
  message(
    "Wrote ", nrow(players), " projection rows, ", nrow(props_df), " prop rows -> ",
    normalizePath(op, winslash = "/", mustWork = FALSE)
  )
}
