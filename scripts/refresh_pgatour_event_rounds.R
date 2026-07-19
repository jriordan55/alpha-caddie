#!/usr/bin/env Rscript
# Current PGA tournament round rows from pgatouR scorecards (official hole-by-hole).
# Writes alpha-caddie-web/data/pgatour_event_rounds.json for build-player-history merge.
#
# Usage: Rscript scripts/refresh_pgatour_event_rounds.R [repo_root]
# npm: npm run refresh:pgatour-event (from alpha-caddie-web)

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) {
  normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  rd <- Sys.getenv("GOLF_MODEL_DIR", unset = "")
  if (nzchar(rd)) normalizePath(rd, winslash = "/", mustWork = TRUE) else normalizePath(getwd(), winslash = "/")
}

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tibble)
})

if (!requireNamespace("pgatouR", quietly = TRUE)) {
  message("[refresh-pgatour-event] pgatouR not installed — skip (remotes::install_github('WalrusQuant/pgatouR'))")
  quit(save = "no", status = 0L)
}

source(file.path(repo, "R", "scorecard.R"))
source(file.path(repo, "R", "player_id_mapping.R"))

norm_evt <- function(s) {
  s <- tolower(trimws(as.character(s)))
  gsub("[^a-z0-9]+", " ", s)
}

events_likely_same <- function(a, b) {
  fa <- norm_evt(a)
  fb <- norm_evt(b)
  if (!nzchar(fa) || !nzchar(fb)) return(FALSE)
  if (fa == fb) return(TRUE)
  if (grepl(fa, fb, fixed = TRUE) || grepl(fb, fa, fixed = TRUE)) return(TRUE)
  if (nchar(fa) >= 8 && grepl(substr(fa, 1, 8), fb, fixed = TRUE)) return(TRUE)
  if (nchar(fb) >= 8 && grepl(substr(fb, 1, 8), fa, fixed = TRUE)) return(TRUE)
  FALSE
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) hit[[1]] else NA_character_
}

format_us_mdy <- function(rd) {
  if (!inherits(rd, "Date") || length(rd) != 1L || is.na(rd)) return("")
  sprintf("%d/%d/%d", as.integer(format(rd, "%m")), as.integer(format(rd, "%d")), as.integer(format(rd, "%Y")))
}

tournament_anchor_date <- function(ssched_row) {
  if (nrow(ssched_row) < 1L) return(as.Date(NA))
  dd <- trimws(as.character(ssched_row$display_date[1]))
  yr <- suppressWarnings(as.integer(as.character(ssched_row$year[1])))
  regm <- gregexpr("[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}", dd, perl = TRUE)
  hits <- regmatches(dd, regm)[[1]]
  if (length(hits) >= 1L) {
    parts <- strsplit(hits[[1]], "/", fixed = TRUE)[[1]]
    if (length(parts) == 3L) {
      mo <- suppressWarnings(as.integer(parts[1]))
      d <- suppressWarnings(as.integer(parts[2]))
      yy <- suppressWarnings(as.integer(parts[3]))
      if (yy < 100L) yy <- yy + if (yy >= 30L) 1900L else 2000L
      if (!is.na(mo) && !is.na(d) && !is.na(yy)) {
        return(suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mo, d))))
      }
    }
  }
  if (!is.na(yr)) return(suppressWarnings(as.Date(sprintf("%s-07-01", yr))))
  as.Date(NA)
}

round_played_date <- function(anchor, round_num) {
  if (inherits(anchor, "Date") && !is.na(anchor)) return(anchor + (as.integer(round_num) - 1L))
  as.Date(NA)
}

sort_key_from_date <- function(rd, round_num) {
  if (!inherits(rd, "Date") || is.na(rd)) return(0L)
  y <- as.integer(format(rd, "%Y"))
  mo <- as.integer(format(rd, "%m"))
  d <- as.integer(format(rd, "%d"))
  (y * 10000L + mo * 100L + d) * 10L + as.integer(round_num)
}

proj_path <- file.path(repo, "alpha-caddie-web", "projections.json")
out_path <- file.path(repo, "alpha-caddie-web", "data", "pgatour_event_rounds.json")
map_path <- file.path(repo, "data", "pga_datagolf_player_map.csv")

if (!file.exists(proj_path)) {
  message("[refresh-pgatour-event] Missing projections.json — skip")
  quit(save = "no", status = 0L)
}

pj <- jsonlite::fromJSON(proj_path, simplifyDataFrame = TRUE)
event_name <- trimws(as.character(if (is.null(pj$event_name)) "" else pj$event_name))
course_used <- trimws(as.character(if (is.null(pj$course_used)) "" else pj$course_used))
if (!nzchar(event_name)) {
  message("[refresh-pgatour-event] No event_name in projections — skip")
  quit(save = "no", status = 0L)
}

cy <- as.integer(format(Sys.Date(), "%Y"))
sched <- tryCatch(pgatouR::pga_schedule(cy), error = function(e) tibble())
if (nrow(sched) == 0L) {
  sched <- tryCatch(pgatouR::pga_schedule(cy - 1L), error = function(e) tibble())
}
if (nrow(sched) == 0L || !"tournament_id" %in% names(sched)) {
  message("[refresh-pgatour-event] Empty schedule — skip")
  quit(save = "no", status = 0L)
}

hit <- which(vapply(seq_len(nrow(sched)), function(i) {
  cn <- as.character(sched$course_name[i])
  nv <- norm_evt(course_used)
  nc <- norm_evt(cn)
  nzchar(nv) && nzchar(nc) && (grepl(nv, nc, fixed = TRUE) || grepl(nc, nv, fixed = TRUE))
}, logical(1)))
if (length(hit) == 0L) {
  hit <- which(vapply(sched$tournament_name, function(n) events_likely_same(n, event_name), logical(1)))
}
if (length(hit) > 1L && nzchar(course_used)) {
  hit <- hit[which(vapply(hit, function(i) {
    cn <- as.character(sched$course_name[i])
    nv <- norm_evt(course_used)
    nc <- norm_evt(cn)
    nzchar(nv) && nzchar(nc) && (grepl(nv, nc, fixed = TRUE) || grepl(nc, nv, fixed = TRUE))
  }, logical(1)))]
}
if (length(hit) == 0L && nzchar(course_used)) {
  hit <- which(vapply(sched$course_name, function(cn) {
    nc <- norm_evt(cn)
    nv <- norm_evt(course_used)
    nzchar(nc) && nzchar(nv) && (grepl(nv, nc, fixed = TRUE) || grepl(nc, nv, fixed = TRUE))
  }, logical(1)))
}
if (length(hit) == 0L) {
  message("[refresh-pgatour-event] No schedule row for event \"", event_name, "\" — skip")
  quit(save = "no", status = 0L)
}

ss <- sched[hit[1], , drop = FALSE]
tid <- as.character(ss$tournament_id[1])
tourn_name <- as.character(ss$tournament_name[1])
course_sched <- as.character(ss$course_name[1])
anchor <- tournament_anchor_date(ss)
year <- suppressWarnings(as.integer(ss$year[1]))
if (is.na(year)) year <- cy

read_event_date_start_iso <- function() {
  lip_paths <- c(
    file.path(repo, "alpha-caddie-web", "live-in-play.json"),
    file.path(repo, "website", "public", "data", "live-in-play.json")
  )
  for (p in lip_paths) {
    if (!file.exists(p)) next
    lip <- tryCatch(jsonlite::fromJSON(p, simplifyDataFrame = FALSE), error = function(e) NULL)
    if (is.null(lip)) next
    fu <- lip[["field_updates"]]
    if (is.list(fu) && !is.null(fu[["date_start"]])) {
      ds <- trimws(as.character(fu[["date_start"]]))
      if (grepl("^\\d{4}-\\d{2}-\\d{2}", ds)) return(substr(ds, 1L, 10L))
    }
    info <- lip[["info"]]
    if (is.list(info) && !is.null(info[["date_start"]])) {
      ds <- trimws(as.character(info[["date_start"]]))
      if (grepl("^\\d{4}-\\d{2}-\\d{2}", ds)) return(substr(ds, 1L, 10L))
    }
  }
  ""
}

date_start_iso <- read_event_date_start_iso()
if (nzchar(date_start_iso)) {
  m <- regmatches(date_start_iso, regexpr("^\\d{4}-\\d{2}-\\d{2}", date_start_iso, perl = TRUE))
  if (length(m) >= 1L) {
    anchor <- suppressWarnings(as.Date(m[[1]]))
    message("[refresh-pgatour-event] Using field date_start ", m[[1]], " for round played dates")
  }
}

map_df <- load_pga_datagolf_map(map_path)
if (is.null(map_df) || nrow(map_df) == 0L) {
  message("[refresh-pgatour-event] Missing pga_datagolf_player_map.csv — skip")
  quit(save = "no", status = 0L)
}

pl <- unique(pj$players[, c("dg_id", "player_name"), drop = FALSE])
pl <- pl[is.finite(pl$dg_id) & nzchar(as.character(pl$player_name)), , drop = FALSE]
sleep_sec <- suppressWarnings(as.double(Sys.getenv("PGA_EVENT_SLEEP_SEC", unset = "0.08")))
if (!is.finite(sleep_sec) || sleep_sec < 0) sleep_sec <- 0.08

# Tournament leaderboard supplies official player_id for field members missing from
# the static dg↔pga map (common for Open Championship internationals / amateurs).
as_chr <- function(x) {
  if (is.null(x) || length(x) < 1L) return("")
  s <- as.character(x[[1]])
  if (is.na(s)) return("")
  s
}
fold_ascii <- function(s) {
  s <- as_chr(s)
  if (!nzchar(s)) return("")
  out <- suppressWarnings(iconv(s, from = "", to = "ASCII//TRANSLIT"))
  if (is.na(out) || !nzchar(out)) out <- s
  # Common Nordic leftovers when iconv is unavailable/partial.
  out <- gsub("\u00C5|\u00E5", "a", out, perl = TRUE)
  out <- gsub("\u00D8|\u00F8", "o", out, perl = TRUE)
  out <- gsub("\u00C6|\u00E6", "ae", out, perl = TRUE)
  out <- gsub("\u00D6|\u00F6", "o", out, perl = TRUE)
  out <- gsub("\u00C4|\u00E4", "a", out, perl = TRUE)
  out
}
norm_person <- function(s) {
  s <- tolower(trimws(fold_ascii(s)))
  s <- gsub("[^a-z0-9]+", " ", s)
  trimws(gsub("\\s+", " ", s))
}
split_last_first <- function(display_or_last_first) {
  s <- trimws(as_chr(display_or_last_first))
  if (!nzchar(s)) return(c(last = "", first = ""))
  if (grepl(",", s, fixed = TRUE)) {
    parts <- strsplit(s, ",", fixed = TRUE)[[1]]
    return(c(last = norm_person(parts[[1]]), first = if (length(parts) >= 2L) norm_person(parts[[2]]) else ""))
  }
  parts <- strsplit(norm_person(s), "\\s+")[[1]]
  if (!length(parts)) return(c(last = "", first = ""))
  if (length(parts) == 1L) return(c(last = parts[[1]], first = ""))
  c(last = parts[[length(parts)]], first = paste(parts[-length(parts)], collapse = " "))
}
name_key_last_first <- function(display_or_last_first) {
  lf <- split_last_first(display_or_last_first)
  trimws(paste(lf[["last"]], lf[["first"]]))
}
first_token <- function(s) {
  parts <- strsplit(norm_person(s), "\\s+")[[1]]
  if (!length(parts)) return("")
  parts[[1]]
}

lb_by_key <- new.env(parent = emptyenv())
lb_by_last <- new.env(parent = emptyenv())
lb_by_last_first1 <- new.env(parent = emptyenv())
lb <- tryCatch(pgatouR::pga_leaderboard(tid), error = function(e) NULL)
if (!is.null(lb) && is.data.frame(lb) && nrow(lb) > 0L) {
  for (j in seq_len(nrow(lb))) {
    pid <- as_chr(lb$player_id[j])
    if (!nzchar(pid)) next
    last <- norm_person(lb$last_name[j])
    first <- norm_person(lb$first_name[j])
    # Compound last names (e.g. "Bjoernevikl Skogen") — also index final token.
    last_tokens <- strsplit(last, "\\s+")[[1]]
    last_tail <- if (length(last_tokens)) last_tokens[[length(last_tokens)]] else ""
    keys <- unique(c(
      name_key_last_first(lb$display_name[j]),
      name_key_last_first(paste(as_chr(lb$last_name[j]), as_chr(lb$first_name[j]), sep = ", ")),
      paste(last, first),
      if (nzchar(last_tail) && last_tail != last) paste(last_tail, first) else ""
    ))
    keys <- keys[nzchar(keys)]
    for (k in keys) assign(k, pid, envir = lb_by_key)
    for (lk in unique(c(last, last_tail))) {
      if (!nzchar(lk)) next
      prev <- if (exists(lk, envir = lb_by_last, inherits = FALSE)) get(lk, envir = lb_by_last, inherits = FALSE) else character()
      assign(lk, unique(c(prev, pid)), envir = lb_by_last)
      f1 <- substr(first, 1L, 1L)
      if (nzchar(f1)) {
        k2 <- paste(lk, f1)
        assign(k2, pid, envir = lb_by_last_first1)
      }
    }
  }
  message("[refresh-pgatour-event] Leaderboard name map: ", length(ls(lb_by_key)), " key(s)")
}

normalize_pga_player_id <- function(pid) {
  pid <- as_chr(pid)
  if (!nzchar(pid)) return("")
  # Keep official string form; pad classic short numeric ids (e.g. 9011 -> 09011).
  if (grepl("^[0-9]+$", pid) && nchar(pid) < 5L) {
    return(sprintf("%05d", as.integer(pid)))
  }
  pid
}

resolve_pga_id <- function(dg, pname) {
  pid <- dg_id_to_pga_player_id(dg, map_df)
  if (length(pid) == 1L && !is.na(pid) && nzchar(pid)) {
    # Prefer tournament leaderboard id when names match — map ids can drop leading zeros.
    k1 <- name_key_last_first(pname)
    if (nzchar(k1) && exists(k1, envir = lb_by_key, inherits = FALSE)) {
      return(normalize_pga_player_id(get(k1, envir = lb_by_key, inherits = FALSE)))
    }
    return(normalize_pga_player_id(pid))
  }
  k1 <- name_key_last_first(pname)
  if (nzchar(k1) && exists(k1, envir = lb_by_key, inherits = FALSE)) {
    return(normalize_pga_player_id(get(k1, envir = lb_by_key, inherits = FALSE)))
  }
  lf <- split_last_first(pname)
  last <- lf[["last"]]
  first <- lf[["first"]]
  f1 <- substr(first_token(first), 1L, 1L)
  if (nzchar(last) && nzchar(f1)) {
    k2 <- paste(last, f1)
    if (exists(k2, envir = lb_by_last_first1, inherits = FALSE)) {
      return(normalize_pga_player_id(get(k2, envir = lb_by_last_first1, inherits = FALSE)))
    }
  }
  if (nzchar(last) && exists(last, envir = lb_by_last, inherits = FALSE)) {
    cands <- get(last, envir = lb_by_last, inherits = FALSE)
    if (length(cands) == 1L) return(normalize_pga_player_id(cands[[1]]))
  }
  NA_character_
}

round_rows <- list()
n_ok <- 0L
n_map_miss <- 0L
n_lb_rescue <- 0L

for (i in seq_len(nrow(pl))) {
  dg <- as.integer(pl$dg_id[i])
  pname <- as.character(pl$player_name[i])
  mapped <- dg_id_to_pga_player_id(dg, map_df)
  pga_id <- resolve_pga_id(dg, pname)
  if (length(pga_id) != 1L || is.na(pga_id) || !nzchar(pga_id)) {
    n_map_miss <- n_map_miss + 1L
    next
  }
  if (length(mapped) != 1L || is.na(mapped) || !nzchar(mapped)) n_lb_rescue <- n_lb_rescue + 1L
  Sys.sleep(sleep_sec)
  sc <- pga_scorecard_safe(tid, pga_id)
  if (nrow(sc) == 0L) {
    Sys.sleep(max(0.15, sleep_sec))
    sc <- pga_scorecard_safe(tid, pga_id)
  }
  if (nrow(sc) == 0L) next

  rcol <- pick_col(sc, c("round_number", "roundNumber"))
  hcol <- pick_col(sc, c("hole_number", "holeNumber"))
  pcol <- pick_col(sc, c("par"))
  scol <- pick_col(sc, c("score"))
  ccol <- pick_col(sc, c("course_name", "courseName"))
  if (is.na(rcol) || is.na(hcol) || is.na(pcol) || is.na(scol)) next

  rnums <- sort(unique(suppressWarnings(as.integer(sc[[rcol]]))))
  rnums <- rnums[is.finite(rnums) & rnums >= 1L & rnums <= 4L]

  for (rn in rnums) {
    h <- sc[as.integer(sc[[rcol]]) == as.integer(rn), , drop = FALSE]
    if (nrow(h) == 0L) next
    cn <- course_sched
    if (!is.na(ccol)) {
      v <- as.character(h[[ccol]][1])
      if (nzchar(v)) cn <- v
    }
    if (nzchar(course_used)) cn <- course_used

    parv <- suppressWarnings(as.integer(h[[pcol]]))
    scv <- suppressWarnings(as.integer(h[[scol]]))
    ok <- is.finite(parv) & is.finite(scv)
    if (!any(ok)) next
    parv <- parv[ok]
    scv <- scv[ok]
    rel <- scv - parv
    round_score <- as.integer(sum(scv))
    if (!is.finite(round_score) || round_score <= 0L) next

    gir_col <- pick_col(h, c("green_in_regulation", "green_in_reg", "gir", "GIR"))
    fw_col <- pick_col(h, c("fairway_hit", "fairway", "fw", "FW"))
    putt_col <- pick_col(h, c("putts", "num_putts", "putt"))
    n_gir <- NA_integer_
    n_fw <- NA_integer_
    n_putts <- NA_integer_
    if (!is.na(gir_col)) {
      gv <- suppressWarnings(as.integer(h[[gir_col]][ok]))
      if (length(gv)) n_gir <- as.integer(sum(gv == 1L, na.rm = TRUE))
    }
    if (!is.na(fw_col)) {
      fv <- suppressWarnings(as.integer(h[[fw_col]][ok]))
      if (length(fv)) n_fw <- as.integer(sum(fv == 1L, na.rm = TRUE))
    }
    if (!is.na(putt_col)) {
      pv <- suppressWarnings(as.integer(h[[putt_col]][ok]))
      if (length(pv) && all(is.finite(pv))) n_putts <- as.integer(sum(pv))
    }

    rdate <- round_played_date(anchor, rn)
    event_completed <- format_us_mdy(rdate)
    sk <- sort_key_from_date(rdate, rn)

    round_rows[[length(round_rows) + 1L]] <- list(
      dg_id = dg,
      player_name = pname,
      sortKey = sk,
      event_completed = event_completed,
      year = as.integer(year),
      event_name = event_name,
      event_id = tid,
      course_name = cn,
      round_num = as.integer(rn),
      fin_text = "",
      round_score = round_score,
      birdies = as.integer(sum(rel == -1L)),
      pars = as.integer(sum(rel == 0L)),
      bogies = as.integer(sum(rel == 1L)),
      gir = if (is.finite(n_gir) && n_gir >= 0L) n_gir else NULL,
      fairways = if (is.finite(n_fw) && n_fw >= 0L) n_fw else NULL,
      putts = if (is.finite(n_putts) && n_putts > 0L) n_putts else NULL,
      eagles_or_better = as.integer(sum(rel <= -2L)),
      doubles_or_worse = as.integer(sum(rel >= 2L)),
      "_from_pgatour" = TRUE
    )
    n_ok <- n_ok + 1L
  }
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
payload <- list(
  meta = list(
    updated_at = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    source = "pgatouR::pga_scorecard",
    event_name = event_name,
    tournament_id = tid,
    tournament_name = tourn_name,
    course_name = if (nzchar(course_used)) course_used else course_sched,
    round_rows = length(round_rows)
  ),
  rounds = round_rows
)
writeLines(jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, null = "null"), out_path, useBytes = TRUE)
message(
  "[refresh-pgatour-event] Wrote ",
  out_path,
  " — ",
  length(round_rows),
  " player-round row(s) for ",
  tourn_name,
  " (",
  tid,
  "); leaderboard-rescued players=",
  n_lb_rescue,
  ", still-unmapped=",
  n_map_miss
)
