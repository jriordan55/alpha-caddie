#!/usr/bin/env Rscript
# Build alpha-caddie-web/player_round_history.json from pgatouR scorecards.
#
# - Round-level stats + hole-by-hole rows: pgatouR::pga_scorecard (see R/scorecard.R).
# - Which tournaments to pull: pgatouR::pga_player_results (newest tournaments first).
# - Maps DataGolf dg_id <-> PGA player_id via data/pga_datagolf_player_map.csv.
# - Keeps up to PGA_HISTORY_MAX_ROUNDS per player (default 100).
#
# Requires: pgatouR, jsonlite, dplyr, tibble, readr (via player_id_mapping).
#
# Usage (from repo root):
#   Rscript scripts/build_alpha_caddie_web_history_pga.R
#   Rscript scripts/build_alpha_caddie_web_history_pga.R "C:/path/to/golfModel"
#
# From alpha-caddie-web (npm):
#   npm run build:history:pga
#
# Env:
#   GOLF_MODEL_DIR     — repo root if no CLI arg
#   PGA_HISTORY_MAX_ROUNDS — default 100
#   PGA_HISTORY_SLEEP_SEC  — delay between scorecard calls (default 0.12)
#   PGA_HISTORY_MAX_PLAYERS — if set, only first N mapped players (testing)

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) {
  normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  rd <- Sys.getenv("GOLF_MODEL_DIR", unset = "")
  if (nzchar(rd)) normalizePath(rd, winslash = "/", mustWork = TRUE) else normalizePath(getwd(), winslash = "/")
}

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
  library(jsonlite)
})

if (!requireNamespace("pgatouR", quietly = TRUE)) {
  message("pgatouR not installed — install with: remotes::install_github('WalrusQuant/pgatouR')")
  quit(save = "no", status = 1L)
}

source(file.path(repo, "R", "scorecard.R"))
source(file.path(repo, "R", "player_id_mapping.R"))

max_rounds <- suppressWarnings(as.integer(Sys.getenv("PGA_HISTORY_MAX_ROUNDS", unset = "100")))
if (!is.finite(max_rounds) || max_rounds < 1L) max_rounds <- 100L
sleep_sec <- suppressWarnings(as.double(Sys.getenv("PGA_HISTORY_SLEEP_SEC", unset = "0.12")))
if (!is.finite(sleep_sec) || sleep_sec < 0) sleep_sec <- 0.12
max_players <- suppressWarnings(as.integer(Sys.getenv("PGA_HISTORY_MAX_PLAYERS", unset = "0")))
if (!is.finite(max_players)) max_players <- 0L

pick_col <- function(df, candidates) {
  nms <- names(df)
  hit <- candidates[candidates %in% nms]
  if (length(hit)) hit[[1]] else NA_character_
}

year_from_tid <- function(tid) {
  m <- regmatches(tid, regexpr("^R([0-9]{4})", tid, perl = TRUE))
  if (length(m)) suppressWarnings(as.integer(substr(m, 2L, 5L))) else NA_integer_
}

#' First calendar day of the tournament from schedule row (avoids mis-parsing ranges like "12-15" as 12/15).
tournament_anchor_date <- function(ssched_row) {
  if (nrow(ssched_row) < 1L) return(as.Date(NA))
  ss <- ssched_row[1, , drop = FALSE]
  dd <- trimws(as.character(ss$display_date[1]))
  yr <- suppressWarnings(as.integer(as.character(ss$year[1])))
  mochr <- trimws(as.character(ss$month[1]))

  first_slash_date <- function(s) {
    if (!nzchar(s)) return(as.Date(NA))
    regm <- gregexpr("[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}", s, perl = TRUE)
    hits <- regmatches(s, regm)[[1]]
    if (length(hits) == 0L) return(as.Date(NA))
    parts <- strsplit(hits[[1]], "/", fixed = TRUE)[[1]]
    if (length(parts) != 3L) return(as.Date(NA))
    mo <- suppressWarnings(as.integer(parts[1]))
    d <- suppressWarnings(as.integer(parts[2]))
    yy <- suppressWarnings(as.integer(parts[3]))
    if (is.na(yy)) return(as.Date(NA))
    if (yy < 100L) yy <- yy + if (yy >= 30L) 1900L else 2000L
    if (is.na(mo) || is.na(d) || !(mo %in% 1:12) || !(d %in% 1:31)) return(as.Date(NA))
    suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mo, d)))
  }

  dt <- first_slash_date(dd)
  if (!is.na(dt)) return(dt)

  if (nzchar(dd)) {
    mmm <- regexec(
      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\.?\\s+([0-9]{1,2})",
      dd,
      ignore.case = TRUE,
      perl = TRUE
    )
    rm <- regmatches(dd, mmm)[[1]]
    if (length(rm) >= 3L) {
      mname <- rm[2]
      dday <- suppressWarnings(as.integer(rm[3]))
      yy2 <- suppressWarnings(as.integer(sub(".*([0-9]{4})\\s*$", "\\1", dd, perl = TRUE)))
      if (is.na(yy2)) yy2 <- yr
      if (!is.na(yy2) && !is.na(dday)) {
        for (fmt in c("%b %d %Y", "%B %d %Y")) {
          dt2 <- suppressWarnings(as.Date(paste(mname, dday, yy2), format = fmt))
          if (!is.na(dt2)) return(dt2)
        }
      }
    }
  }

  if (!is.na(yr) && nzchar(mochr)) {
    for (fmt in c("%B 15 %Y", "%b 15 %Y")) {
      dt3 <- suppressWarnings(as.Date(sprintf("%s 15 %d", mochr, yr), format = fmt))
      if (!is.na(dt3)) return(dt3)
    }
  }

  if (!is.na(yr)) return(suppressWarnings(as.Date(sprintf("%s-07-01", yr))))
  as.Date(NA)
}

format_us_mdy <- function(rd) {
  if (!inherits(rd, "Date") || length(rd) != 1L || is.na(rd)) return("")
  sprintf("%d/%d/%d", as.integer(format(rd, "%m")), as.integer(format(rd, "%d")), as.integer(format(rd, "%Y")))
}

sort_key_from_date <- function(rd, round_num) {
  if (!inherits(rd, "Date") || is.na(rd)) return(0L)
  y <- as.integer(format(rd, "%Y"))
  mo <- as.integer(format(rd, "%m"))
  d <- as.integer(format(rd, "%d"))
  (y * 10000L + mo * 100L + d) * 10L + as.integer(round_num)
}

round_played_date <- function(anchor, round_num) {
  if (inherits(anchor, "Date") && !is.na(anchor)) {
    return(anchor + (as.integer(round_num) - 1L))
  }
  as.Date(NA)
}

score_type_from_hole <- function(score, parv) {
  if (length(score) != 1L || length(parv) != 1L) return("")
  if (is.na(score) || is.na(parv)) return("")
  d <- as.integer(score) - as.integer(parv)
  if (d <= -2L) return("eagle")
  if (d == -1L) return("birdie")
  if (d == 0L) return("par")
  if (d == 1L) return("bogey")
  if (d == 2L) return("double")
  if (d >= 3L) return("triple+")
  "other"
}

player_key_hist <- function(name) {
  s <- trimws(as.character(name))
  m <- regexec("^([^,]+),\\s*(.+)$", s)
  hit <- regmatches(s, m)[[1]]
  if (length(hit) == 3L) {
    paste0(tolower(trimws(hit[2])), "|", tolower(trimws(hit[3])))
  } else {
    tolower(s)
  }
}

load_schedule_stacked <- function(years) {
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[[i]]
    message("  pga_schedule(", y, ")")
    Sys.sleep(0.2)
    out[[i]] <- tryCatch(pgatouR::pga_schedule(y), error = function(e) tibble())
  }
  bind_rows(out)
}

process_player <- function(dg_id, player_name, pga_id, sched, max_r, sleep_s) {
  rounds_out <- list()
  holes_by_uid <- list()

  pr <- tryCatch(pgatouR::pga_player_results(pga_id), error = function(e) tibble())
  if (nrow(pr) == 0L || !"tournament_id" %in% names(pr)) {
    return(list(rounds = list(), holes = list()))
  }

  tids <- unique(as.character(pr$tournament_id))
  tids <- tids[!is.na(tids) & nzchar(tids)]
  if (length(tids) == 0L) return(list(rounds = list(), holes = list()))
  # Newest tournaments first using schedule dates (not raw id string — avoids stale ordering).
  anchor_num <- vapply(tids, function(tid) {
    ss <- sched[sched$tournament_id == tid, , drop = FALSE]
    ad <- tournament_anchor_date(ss)
    if (inherits(ad, "Date") && !is.na(ad)) as.numeric(ad) else 0
  }, numeric(1))
  tids <- tids[order(-anchor_num, -xtfrm(tids))]

  n_kept <- 0L
  for (tid in tids) {
    if (n_kept >= max_r) break
    Sys.sleep(sleep_s)
    sc <- pga_scorecard_safe(tid, pga_id)
    if (nrow(sc) == 0L) next

    rcol <- pick_col(sc, c("round_number", "roundNumber"))
    hcol <- pick_col(sc, c("hole_number", "holeNumber"))
    pcol <- pick_col(sc, c("par"))
    scol <- pick_col(sc, c("score"))
    ccol <- pick_col(sc, c("course_name", "courseName"))
    if (is.na(rcol) || is.na(hcol) || is.na(pcol) || is.na(scol)) next

    ssched <- sched[sched$tournament_id == tid, , drop = FALSE]
    event_name <- if (nrow(ssched)) as.character(ssched$tournament_name[1]) else tid
    course_sched <- if (nrow(ssched)) as.character(ssched$course_name[1]) else ""
    yr_sched <- if (nrow(ssched)) suppressWarnings(as.integer(ssched$year[1])) else NA_integer_

    anchor <- tournament_anchor_date(ssched)
    if (is.na(anchor)) {
      yf <- yr_sched %||% year_from_tid(tid)
      if (is.na(yf)) yf <- 2000L
      anchor <- suppressWarnings(as.Date(sprintf("%s-07-01", yf)))
    }
    year <- yr_sched %||% year_from_tid(tid)
    if (length(year) != 1L || is.na(year)) year <- as.integer(format(anchor, "%Y"))

    rnums <- sort(unique(suppressWarnings(as.integer(sc[[rcol]]))))
    rnums <- rnums[is.finite(rnums)]

    for (rn in rnums) {
      if (n_kept >= max_r) break
      h <- sc[as.integer(sc[[rcol]]) == as.integer(rn), , drop = FALSE]
      if (nrow(h) == 0L) next

      cn <- course_sched
      if (!is.na(ccol)) {
        v <- as.character(h[[ccol]][1])
        if (nzchar(v)) cn <- v
      }

      parv <- suppressWarnings(as.integer(h[[pcol]]))
      scv <- suppressWarnings(as.integer(h[[scol]]))
      ok <- is.finite(parv) & is.finite(scv)
      if (!any(ok)) next
      parv <- parv[ok]
      scv <- scv[ok]
      rel <- scv - parv
      round_score <- as.integer(sum(scv))
      birdies <- as.integer(sum(rel == -1L))
      pars <- as.integer(sum(rel == 0L))
      bogies <- as.integer(sum(rel == 1L))
      eob <- as.integer(sum(rel <= -2L))
      dow <- as.integer(sum(rel >= 2L))

      rdate <- round_played_date(anchor, rn)
      event_completed <- format_us_mdy(rdate)
      if (!nzchar(event_completed)) {
        event_completed <- sprintf("7/%d/%d", as.integer(rn), year)
      }
      sk <- sort_key_from_date(rdate, rn)
      if (sk == 0L) sk <- (as.integer(year) * 1000000L + as.integer(rn))

      rounds_out[[length(rounds_out) + 1L]] <- list(
        sortKey = sk,
        event_completed = event_completed,
        year = as.integer(year),
        event_name = event_name,
        event_id = tid,
        course_name = cn,
        round_num = as.integer(rn),
        fin_text = "",
        round_score = round_score,
        birdies = birdies,
        pars = pars,
        bogies = bogies,
        gir = NULL,
        fairways = NULL,
        eagles_or_better = eob,
        doubles_or_worse = dow,
        pga_tournament_id = tid,
        pga_player_id = pga_id
      )

      uid <- paste0(event_name, "\tR", rn)
      ord <- order(suppressWarnings(as.integer(h[[hcol]])))
      h2 <- h[ord, , drop = FALSE]
      hole_rows <- vector("list", nrow(h2))
      for (k in seq_len(nrow(h2))) {
        pv <- suppressWarnings(as.integer(h2[[pcol]][k]))
        sv <- suppressWarnings(as.integer(h2[[scol]][k]))
        hole_rows[[k]] <- list(
          hole = suppressWarnings(as.integer(h2[[hcol]][k])),
          par = pv,
          score = sv,
          score_type = score_type_from_hole(sv, pv)
        )
      }
      holes_by_uid[[uid]] <- hole_rows
      n_kept <- n_kept + 1L
    }
  }

  if (length(rounds_out)) {
    ord <- order(vapply(rounds_out, function(z) z$sortKey, integer(1)))
    rounds_out <- rounds_out[ord]
  }
  list(rounds = rounds_out, holes = holes_by_uid)
}

# --- main ---
message("Repo root: ", repo)
proj_path <- file.path(repo, "alpha-caddie-web", "projections.json")
if (!file.exists(proj_path)) {
  message("Missing projections.json at ", proj_path)
  quit(save = "no", status = 1L)
}

map_path <- file.path(repo, "data", "pga_datagolf_player_map.csv")
map_df <- load_pga_datagolf_map(map_path)
if (is.null(map_df) || nrow(map_df) == 0L) {
  message("Missing or empty ", map_path, " — run scripts/build_pga_datagolf_player_map.R")
  quit(save = "no", status = 1L)
}

pj <- jsonlite::fromJSON(proj_path, simplifyDataFrame = TRUE)
if (is.null(pj$players) || nrow(pj$players) == 0L) {
  message("No players in projections.json")
  quit(save = "no", status = 1L)
}
pl <- unique(pj$players[, c("dg_id", "player_name"), drop = FALSE])
pl <- pl[is.finite(pl$dg_id) & nzchar(as.character(pl$player_name)), , drop = FALSE]
pl <- pl[order(pl$dg_id), ]

if (max_players > 0L && nrow(pl) > max_players) {
  pl <- pl[seq_len(max_players), , drop = FALSE]
  message("PGA_HISTORY_MAX_PLAYERS=", max_players, " — subsetting players.")
}

yrs <- seq(2022L, as.integer(format(Sys.Date(), "%Y")) + 1L)
message("Loading PGA schedules (years ", min(yrs), "–", max(yrs), ") …")
sched <- load_schedule_stacked(yrs)
sched <- sched[!is.na(sched$tournament_id) & nzchar(as.character(sched$tournament_id)), ]
sched <- sched[!duplicated(sched$tournament_id), ]
message("  tournaments in schedule map: ", nrow(sched))

by_dg <- list()
holes_by_pk <- list()
n_ok <- 0L

for (i in seq_len(nrow(pl))) {
  dg <- as.integer(pl$dg_id[i])
  pname <- as.character(pl$player_name[i])
  pga_id <- dg_id_to_pga_player_id(dg, map_df)
  if (length(pga_id) != 1L || is.na(pga_id) || !nzchar(pga_id)) next

  message(sprintf("[%d/%d] dg_id=%d %s (pga %s)", i, nrow(pl), dg, pname, pga_id))
  res <- process_player(dg, pname, pga_id, sched, max_rounds, sleep_sec)
  if (length(res$rounds) == 0L) next

  by_dg[[as.character(dg)]] <- list(
    dg_id = jsonlite::unbox(dg),
    player_name = pname,
    rounds = res$rounds
  )
  pk <- player_key_hist(pname)
  if (is.null(holes_by_pk[[pk]])) holes_by_pk[[pk]] <- list()
  for (nm in names(res$holes)) {
    holes_by_pk[[pk]][[nm]] <- res$holes[[nm]]
  }
  n_ok <- n_ok + 1L
}

out <- list(
  meta = list(
    updated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    source = "pgatouR::pga_scorecard + pga_player_results + pga_schedule",
    max_rounds_per_player = jsonlite::unbox(max_rounds),
    players_built = jsonlite::unbox(n_ok),
    schedule_years = paste(range(yrs), collapse = "-")
  ),
  byDgId = by_dg,
  holesByPlayerKey = holes_by_pk
)

out_path <- file.path(repo, "alpha-caddie-web", "player_round_history.json")
json_txt <- jsonlite::toJSON(out, pretty = 2L, auto_unbox = TRUE, null = "null", na = "null")
writeLines(json_txt, out_path, useBytes = TRUE)
fi <- file.info(out_path)
message("Wrote ", out_path, " (", round(fi$size / 1024, 1), " KB) — ", n_ok, " players with data.")
