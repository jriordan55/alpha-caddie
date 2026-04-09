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
  tbl %>%
    dplyr::transmute(
      player_name = trimws(as.character(.data$player_name)),
      line = as.numeric(.data$line),
      over_odds = as.numeric(.data$over_odds),
      under_odds = as.numeric(.data$under_odds),
      market = as.character(.data$market)
    ) %>%
    dplyr::mutate(
      line = dplyr::if_else(
        .data$market %in% c("Pars", "Birdies", "Bogeys") & is.finite(.data$line) & .data$line == floor(.data$line),
        .data$line + 0.5,
        .data$line
      )
    )
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

auto_r <- ou_display_round_auto()
tz_lab <- Sys.getenv("GOLF_OU_TZ", "America/New_York")
rnd_lab <- switch(as.character(auto_r),
  "1" = "R1 — next Thursday",
  "2" = "R2 — Friday",
  "3" = "R3 — Saturday",
  "4" = "R4 — Sunday",
  paste0("R", auto_r)
)
display_round_label <- paste0(rnd_lab, " (auto, ", tz_lab, ")")

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
