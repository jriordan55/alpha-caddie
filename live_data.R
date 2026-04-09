model_dir <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd()
BASE_URL <- "https://feeds.datagolf.com/historical-raw-data/rounds"
# Canonical path for app and pipeline: add 2026 (and current) data here; round_projections.R and app use this file
# For the static web app, refresh this file with Node (alpha-caddie-web): npm run update:rounds / npm run fetch:dg
OUT_PATH <- file.path(model_dir, "data", "historical_rounds_all.csv")
DELAY_SEC <- 5
MAX_RETRIES <- 4
YEARS_TO_KEEP <- 5L  # Only keep last 5 years in the file (matches round_projections.R)
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

#' Env \code{DATAGOLF_API_KEY} first; else \code{alpha-caddie-web/datagolf.local.json} (gitignored).
load_datagolf_api_key <- function(root = model_dir) {
  k <- trimws(Sys.getenv("DATAGOLF_API_KEY", ""))
  if (nzchar(k)) return(k)
  for (p in c(
    file.path(root, "alpha-caddie-web", "datagolf.local.json"),
    file.path(root, "website", "datagolf.local.json")
  )) {
    if (!file.exists(p)) next
    raw <- tryCatch(
      {
        if (!requireNamespace("jsonlite", quietly = TRUE)) NULL
        else jsonlite::fromJSON(p, simplifyVector = TRUE)
      },
      error = function(e) NULL
    )
    if (is.list(raw)) {
      kk <- raw$apiKey %||% raw$key %||% ""
      kk <- trimws(as.character(kk))
      if (nzchar(kk)) return(kk)
    }
  }
  ""
}
API_KEY <- load_datagolf_api_key()

round_to_row_live <- function(r, round_num, event_info, player_dg_id, player_name, fin_text) {
  if (!is.list(r) || length(r) == 0) return(NULL)
  score_val <- r$score %||% r$round_score %||% r$Score
  if (is.null(score_val) || (is.atomic(score_val) && is.na(score_val))) return(NULL)
  tibble::tibble(
    tour             = as.character(event_info$tour %||% "pga"),
    year             = as.integer(event_info$year %||% NA_integer_),
    season           = as.integer(event_info$season %||% event_info$year %||% NA_integer_),
    event_completed  = as.character(event_info$event_completed %||% ""),
    event_name       = as.character(event_info$event_name %||% ""),
    event_id         = as.character(event_info$event_id %||% ""),
    player_name      = as.character(player_name),
    dg_id            = as.integer(player_dg_id),
    fin_text         = as.character(fin_text %||% ""),
    round_num        = as.integer(round_num),
    course_name      = as.character(r$course_name %||% ""),
    course_num       = as.integer(r$course_num %||% NA_integer_),
    course_par       = as.integer(r$course_par %||% NA_integer_),
    start_hole       = as.integer(r$start_hole %||% NA_integer_),
    teetime          = as.character(r$teetime %||% ""),
    round_score      = as.integer(score_val %||% NA_integer_),
    sg_putt          = as.numeric(r$sg_putt %||% NA_real_),
    sg_arg           = as.numeric(r$sg_arg %||% NA_real_),
    sg_app           = as.numeric(r$sg_app %||% NA_real_),
    sg_ott           = as.numeric(r$sg_ott %||% NA_real_),
    sg_t2g           = as.numeric(r$sg_t2g %||% NA_real_),
    sg_total         = as.numeric(r$sg_total %||% NA_real_),
    driving_dist     = as.numeric(r$driving_dist %||% NA_real_),
    driving_acc      = as.numeric(r$driving_acc %||% NA_real_),
    gir              = as.numeric(r$gir %||% NA_real_),
    scrambling       = as.numeric(r$scrambling %||% NA_real_),
    prox_rgh         = as.numeric(r$prox_rgh %||% NA_real_),
    prox_fw          = as.numeric(r$prox_fw %||% NA_real_),
    great_shots      = as.numeric(r$great_shots %||% NA_real_),
    poor_shots       = as.numeric(r$poor_shots %||% NA_real_),
    eagles_or_better = as.numeric(r$eagles_or_better %||% NA_real_),
    birdies          = as.numeric(r$birdies %||% NA_real_),
    pars             = as.numeric(r$pars %||% NA_real_),
    bogies           = as.numeric(r$bogies %||% NA_real_),
    doubles_or_worse = as.numeric(r$doubles_or_worse %||% NA_real_)
  )
}

events_from_response_live <- function(res_body) {
  dat <- jsonlite::fromJSON(res_body, flatten = FALSE, simplifyDataFrame = FALSE)
  if (is.list(dat) && !is.null(dat$event_name) && !is.null(dat$scores)) {
    return(list(dat))
  }
  for (key in c("events", "data", "results")) {
    if (is.list(dat) && !is.null(dat[[key]]) && length(dat[[key]]) > 0) {
      return(dat[[key]])
    }
  }
  if (is.list(dat) && length(dat) == 1L && is.list(dat[[1]])) {
    only <- dat[[1]]
    if (is.list(only) && length(only) > 0 && (is.null(names(only)) || is.list(only[[1]]))) {
      return(only)
    }
  }
  if (is.list(dat) && length(dat) > 0) {
    first <- dat[[1]]
    if (is.list(first) && (!is.null(first$event_name) || !is.null(first$scores))) {
      return(dat)
    }
    if (is.list(first) && !is.null(first$events) && length(first$events) > 0) {
      return(do.call(c, lapply(dat, function(x) x$events %||% list())))
    }
  }
  if (is.data.frame(dat) && nrow(dat) > 0) {
    return(lapply(seq_len(nrow(dat)), function(i) as.list(dat[i, ])))
  }
  if (is.list(dat) && length(dat) > 0 && is.list(dat[[1]])) {
    return(dat)
  }
  list()
}

flattened_round_live <- function(s, rnum) {
  pre <- paste0("round_", rnum, ".")
  nms <- names(s)
  idx <- startsWith(nms, pre)
  if (!any(idx)) return(NULL)
  r <- list()
  for (k in nms[idx]) {
    r[[substring(k, nchar(pre) + 1L)]] <- s[[k]]
  }
  if (is.null(r$score)) r$score <- r$round_score
  r
}

#' Pull one round object from DataGolf JSON player row (round_1, Round1, or flattened round_N.*).
get_round_payload_live <- function(s, rnum) {
  if (!is.list(s)) return(NULL)
  for (k in c(
    paste0("round_", rnum),
    paste0("Round_", rnum),
    paste0("round", rnum),
    paste0("Round", rnum)
  )) {
    r <- s[[k]]
    if (is.data.frame(r) && nrow(r) > 0) r <- as.list(r[1, ])
    if (is.list(r) && length(r) > 0) return(r)
  }
  flattened_round_live(s, rnum)
}

event_to_rows_live <- function(ev) {
  scores <- ev$scores %||% list()
  if (is.data.frame(scores)) scores <- lapply(seq_len(nrow(scores)), function(i) as.list(scores[i, ]))
  if (length(scores) == 0) return(tibble::tibble())
  event_info <- list(
    tour = ev$tour, year = ev$year, season = ev$season,
    event_completed = ev$event_completed, event_name = ev$event_name, event_id = ev$event_id
  )
  out <- list()
  for (i in seq_along(scores)) {
    s <- scores[[i]]
    if (!is.list(s)) next
    dg_id   <- as.integer(s$dg_id %||% s$dgId %||% NA_integer_)
    pname   <- as.character(s$player_name %||% s$playerName %||% "")
    fin_txt <- as.character(s$fin_text %||% s$finText %||% "")
    for (rnum in seq_len(6L)) {
      r <- get_round_payload_live(s, rnum)
      if (is.null(r)) next
      row <- round_to_row_live(r, rnum, event_info, dg_id, pname, fin_txt)
      if (!is.null(row)) out[[length(out) + 1L]] <- row
    }
  }
  if (length(out) == 0) return(tibble::tibble())
  dplyr::bind_rows(out)
}

COL_ORDER_LIVE <- c(
  "tour", "year", "season", "event_completed", "event_name", "event_id",
  "player_name", "dg_id", "fin_text", "round_num", "course_name", "course_num", "course_par",
  "start_hole", "teetime", "round_score", "sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total",
  "driving_dist", "driving_acc", "gir", "scrambling", "prox_rgh", "prox_fw", "great_shots", "poor_shots",
  "eagles_or_better", "birdies", "pars", "bogies", "doubles_or_worse"
)

#' DataGolf CSV may parse event_completed as Date; on-disk CSV is m/d/yyyy character. Unify for bind_rows + web.
event_completed_chr_for_csv <- function(x) {
  if (length(x) == 0L) return(character(0))
  if (inherits(x, "Date")) {
    y <- as.integer(format(x, "%Y"))
    m <- as.integer(format(x, "%m"))
    d <- as.integer(format(x, "%d"))
    return(sprintf("%d/%d/%d", m, d, y))
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(event_completed_chr_for_csv(as.Date(x)))
  }
  xc <- as.character(x)
  parsed <- suppressWarnings(as.Date(xc))
  iso <- !is.na(parsed) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", xc)
  if (any(iso)) {
    dd <- parsed[iso]
    y <- as.integer(format(dd, "%Y"))
    m <- as.integer(format(dd, "%m"))
    d <- as.integer(format(dd, "%d"))
    xc[iso] <- sprintf("%d/%d/%d", m, d, y)
  }
  xc
}

#' readr may parse teetime as hms/time; JSON rows use character — unify for bind_rows + CSV.
teetime_chr_for_csv <- function(x) {
  if (length(x) == 0L) return(character(0))
  if (is.character(x)) return(x)
  out <- as.character(x)
  nna <- which(is.na(x))
  if (length(nna)) out[nna] <- NA_character_
  out
}

fetch_event_rounds <- function(year, event_id, tour = "pga") {
  # Try requested year first; on 400 (e.g. 2026 not yet in raw data) retry with previous year
  years_to_try <- unique(c(as.integer(year), as.integer(year) - 1L))
  years_to_try <- years_to_try[years_to_try >= 2004L]
  body <- NULL
  last_status <- NA_integer_
  for (yr in years_to_try) {
    q <- list(tour = tour, event_id = event_id, year = yr, file_format = "json", key = API_KEY)
    for (attempt in 1:MAX_RETRIES) {
      res <- httr::GET(BASE_URL, query = q)
      last_status <- httr::status_code(res)
      if (last_status == 200) {
        body <- httr::content(res, as = "text", encoding = "UTF-8")
        break
      }
      if (last_status == 429) {
        wait <- DELAY_SEC * (2^(attempt - 1))
        message("Live data event ", event_id, " (", yr, "): HTTP 429, waiting ", round(wait), "s before retry ", attempt, "/", MAX_RETRIES)
        Sys.sleep(wait)
      } else {
        if (last_status == 400 && yr != years_to_try[length(years_to_try)]) {
          message("Live data event ", event_id, " (", yr, "): HTTP 400, trying previous year.")
        } else {
          message("Live data event ", event_id, " (", yr, "): HTTP ", last_status)
        }
        break
      }
    }
    if (!is.null(body)) break
  }
  if (is.null(body)) {
    message("Live data event ", event_id, " (", year, "): failed, last HTTP ", last_status)
    return(tibble::tibble())
  }
  events <- events_from_response_live(body)
  if (length(events) == 0) return(tibble::tibble())
  all_rows <- lapply(events, event_to_rows_live)
  dplyr::bind_rows(all_rows)
}

# Retry transient failures when pulling full-year rounds from DataGolf.
dg_get_historical_rounds <- function(query, label, max_attempts = 4L) {
  last <- NULL
  for (attempt in seq_len(max_attempts)) {
    last <- tryCatch(httr::GET(BASE_URL, query = query), error = function(e) NULL)
    if (is.null(last)) {
      message("Live data: ", label, " connection failed (attempt ", attempt, "); retrying...")
      Sys.sleep(min(2^attempt, 30))
      next
    }
    st <- httr::status_code(last)
    if (st == 200L) return(last)
    if (st %in% c(429L, 500L, 502L, 503L, 504L)) {
      message("Live data: ", label, " HTTP ", st, " (attempt ", attempt, "); retrying...")
      Sys.sleep(min(2^attempt, 30))
      next
    }
    return(last)
  }
  last
}

# Fetch all rounds for event_id=all. For current and prior calendar year, JSON first — bulk CSV
# often lags the Raw Data Archive; older years stay CSV-first for speed.
fetch_year_rounds <- function(year, tour = "pga") {
  if (!nzchar(API_KEY)) {
    message("Live data: DATAGOLF_API_KEY empty; cannot fetch year=", year, ".")
    return(tibble::tibble())
  }

  cy <- as.integer(format(Sys.Date(), "%Y"))
  prefer_json_first <- year >= cy - 1L
  if (identical(Sys.getenv("GOLF_ROUNDS_PREFER_CSV_FIRST", ""), "1")) prefer_json_first <- FALSE
  if (identical(Sys.getenv("GOLF_ROUNDS_PREFER_JSON_FIRST", ""), "1")) prefer_json_first <- TRUE

  fetch_year_json <- function() {
    q_json <- list(tour = tour, event_id = "all", year = year, file_format = "json", key = API_KEY)
    res <- dg_get_historical_rounds(q_json, paste0("year=", year, " json"))
    if (is.null(res)) {
      message("Live data: event_id=all year=", year, " JSON: HTTP layer failed.")
      return(tibble::tibble())
    }
    status <- httr::status_code(res)
    message("Live data: event_id=all year=", year, " file_format=json HTTP ", status)
    if (status != 200) return(tibble::tibble())
    body <- httr::content(res, as = "text", encoding = "UTF-8")
    events <- events_from_response_live(body)
    if (length(events) == 0) {
      message("Live data: year=", year, " JSON returned 0 events.")
      return(tibble::tibble())
    }
    all_rows <- lapply(events, event_to_rows_live)
    out <- dplyr::bind_rows(all_rows)
    if (nrow(out) > 0) message("Live data: got ", nrow(out), " rows for year ", year, " (JSON).")
    out
  }

  fetch_year_csv <- function() {
    q_csv <- list(tour = tour, event_id = "all", year = year, file_format = "csv", key = API_KEY)
    res <- dg_get_historical_rounds(q_csv, paste0("year=", year, " csv"))
    if (is.null(res)) {
      message("Live data: event_id=all year=", year, " CSV: HTTP layer failed.")
      return(tibble::tibble())
    }
    status <- httr::status_code(res)
    message("Live data: event_id=all year=", year, " file_format=csv HTTP ", status)
    if (status != 200) return(tibble::tibble())
    body <- httr::content(res, as = "text", encoding = "UTF-8")
    tmp <- tryCatch({
      if (nchar(trimws(body)) == 0) NULL else readr::read_csv(I(body), show_col_types = FALSE)
    }, error = function(e) { message("Live data: CSV parse error ", conditionMessage(e)); NULL })
    if (!is.null(tmp) && nrow(tmp) > 0) {
      message("Live data: got ", nrow(tmp), " rows for year ", year, " (CSV).")
      return(tmp)
    }
    tibble::tibble()
  }

  if (prefer_json_first) {
    j <- fetch_year_json()
    if (nrow(j) > 0) return(j)
    message("Live data: JSON empty or failed for year ", year, "; trying CSV …")
    c <- fetch_year_csv()
    if (nrow(c) > 0) return(c)
  } else {
    c <- fetch_year_csv()
    if (nrow(c) > 0) return(c)
    message("Live data: CSV empty or failed for year ", year, "; trying JSON …")
    j <- fetch_year_json()
    if (nrow(j) > 0) return(j)
  }
  tibble::tibble()
}

# Extract event_id and year from a single event object (list or row)
ev_meta_from_one <- function(ev) {
  if (is.null(ev)) return(list(ev_id = NULL, yr = NULL, event_name = ""))
  if (is.data.frame(ev) && nrow(ev) >= 1) ev <- as.list(ev[1, ])
  if (!is.list(ev)) return(list(ev_id = NULL, yr = NULL, event_name = ""))
  ev_id <- ev$event_id %||% ev$eventId %||% ev$id %||% ev$tournament_id
  yr    <- ev$year %||% ev$season
  event_name <- as.character(ev$event_name %||% ev$name %||% ev$tournament_name %||% "")
  list(ev_id = ev_id, yr = yr, event_name = event_name)
}

current_event_meta <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  ev_id <- NULL
  yr <- NULL
  event_name <- ""

  # 1) Field-updates
  res <- tryCatch(httr::GET("https://feeds.datagolf.com/field-updates",
                            query = list(tour = "pga", file_format = "json", key = API_KEY)),
                  error = function(e) NULL)
  if (!is.null(res) && httr::status_code(res) == 200) {
    dat <- tryCatch(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                                       flatten = TRUE, simplifyDataFrame = FALSE),
                    error = function(e) NULL)
    if (is.list(dat)) {
      out <- ev_meta_from_one(dat)
      ev_id <- out$ev_id
      yr <- out$yr
      event_name <- out$event_name
    }
  }

  # 2) get-schedule (upcoming_only=yes then no) — event names/ids
  if (is.null(ev_id) || is.null(yr)) {
    for (upcoming in c("yes", "no")) {
      sched_res <- tryCatch(httr::GET("https://feeds.datagolf.com/get-schedule",
                                      query = list(tour = "pga", upcoming_only = upcoming, file_format = "json", key = API_KEY)),
                            error = function(e) NULL)
      if (is.null(sched_res) || httr::status_code(sched_res) != 200) next
      sched <- tryCatch(jsonlite::fromJSON(httr::content(sched_res, as = "text", encoding = "UTF-8"),
                                            flatten = TRUE, simplifyDataFrame = FALSE),
                        error = function(e) NULL)
      if (!is.list(sched)) next
      events <- sched$events %||% sched$schedule %||% sched$tournaments %||% sched
      if (is.data.frame(events) && nrow(events) > 0) {
        out <- ev_meta_from_one(events[1, ])
      } else if (is.list(events) && length(events) > 0) {
        out <- ev_meta_from_one(events[[1]])
      } else {
        next
      }
      if (!is.null(out$ev_id) && !is.null(out$yr)) {
        ev_id <- ev_id %||% out$ev_id
        yr <- yr %||% out$yr
        if (!nzchar(event_name %||% "")) event_name <- out$event_name
        break
      }
    }
  }

  # 3) historical-raw-data/event-list — returns event IDs for raw data API
  if (is.null(ev_id) || is.null(yr)) {
    el_res <- tryCatch(httr::GET("https://feeds.datagolf.com/historical-raw-data/event-list",
                                 query = list(tour = "pga", file_format = "json", key = API_KEY)),
                       error = function(e) NULL)
    if (!is.null(el_res) && httr::status_code(el_res) == 200) {
      el <- tryCatch(jsonlite::fromJSON(httr::content(el_res, as = "text", encoding = "UTF-8"),
                                        flatten = TRUE, simplifyDataFrame = FALSE),
                     error = function(e) NULL)
      if (is.list(el)) {
        events <- el$events %||% el$event_list %||% el$tournaments %||% el
        if (is.data.frame(events) && nrow(events) > 0) {
          # Prefer current year; use first row if only one column layout
          ev <- as.list(events[1, ])
          out <- ev_meta_from_one(ev)
          if (is.null(out$yr)) out$yr <- current_year
          ev_id <- ev_id %||% out$ev_id
          yr <- yr %||% out$yr
          if (!nzchar(event_name %||% "")) event_name <- out$event_name
        } else if (is.list(events) && length(events) > 0) {
          ev <- events[[1]]
          out <- ev_meta_from_one(ev)
          if (is.null(out$yr)) out$yr <- current_year
          ev_id <- ev_id %||% out$ev_id
          yr <- yr %||% out$yr
          if (!nzchar(event_name %||% "")) event_name <- out$event_name
        }
      }
    }
  }

  # Default year to current year if we have event_id but no year
  if (!is.null(ev_id) && is.null(yr)) yr <- current_year
  if (is.null(ev_id) || is.null(yr)) {
    message("Live data update: event_id or year missing from field-updates, get-schedule, and event-list.")
    return(NULL)
  }
  list(
    tour = "pga",
    event_id = as.character(ev_id),
    year = as.integer(yr),
    event_name = as.character(event_name %||% "")
  )
}

# Fallback: get (event_id, year) from most recent event in existing CSV when APIs fail
current_event_meta_from_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(readr::read_csv(path, show_col_types = FALSE, n_max = 1e6),
               error = function(e) NULL)
  if (is.null(x) || nrow(x) == 0 || !"event_id" %in% names(x) || !"year" %in% names(x)) return(NULL)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  x <- x %>% dplyr::filter(as.integer(year) >= current_year - 1L)
  if (nrow(x) == 0) return(NULL)
  # Most recent by event_completed then event_id
  if ("event_completed" %in% names(x)) {
    x <- x %>% dplyr::arrange(dplyr::desc(event_completed), dplyr::desc(event_id))
  } else {
    x <- x %>% dplyr::arrange(dplyr::desc(year), dplyr::desc(event_id))
  }
  ev_id <- as.character(x$event_id[1])
  yr <- as.integer(x$year[1])
  if (is.na(ev_id) || is.na(yr)) return(NULL)
  list(tour = "pga", event_id = ev_id, year = yr, event_name = as.character(x$event_name[1] %||% ""))
}

#' Calendar years to replace from DataGolf (\code{event_id=all}).
#' Default: every year kept in the CSV window \code{[current_year - YEARS_TO_KEEP + 1, current_year]}
#' so the file is fully refreshed through the latest archive (not only the last two years).
#' Light mode: \code{GOLF_HISTORICAL_ROUNDS_LIGHT=1} → only current and previous calendar year.
#' Override list: \code{GOLF_HISTORICAL_ROUNDS_YEARS=2024,2025,2026}.
historical_round_refresh_years <- function() {
  cy <- as.integer(format(Sys.Date(), "%Y"))
  ex <- trimws(Sys.getenv("GOLF_HISTORICAL_ROUNDS_YEARS", ""))
  if (nzchar(ex)) {
    parts <- strsplit(ex, "[,;\\s]+", perl = TRUE)[[1]]
    yrs <- unique(suppressWarnings(as.integer(parts)))
    yrs <- yrs[!is.na(yrs) & yrs >= 2004L & yrs <= cy + 1L]
    if (length(yrs)) return(sort(unique(yrs)))
  }
  if (identical(Sys.getenv("GOLF_HISTORICAL_ROUNDS_LIGHT", ""), "1")) {
    return(sort(unique(c(cy, cy - 1L))))
  }
  min_y <- cy - as.integer(YEARS_TO_KEEP) + 1L
  min_y <- max(2004L, min_y)
  seq(min_y, cy)
}

#' Update historical_rounds_all.csv with latest rounds. Uses event_id=all per refresh year
#' so the file is updated even when per-event fetch fails. Optionally merges current event too.
update_historical_rounds_live <- function() {
  message("Live data: OUT_PATH = ", OUT_PATH)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  existing <- tibble::tibble()
  if (file.exists(OUT_PATH)) {
    existing <- tryCatch(
      readr::read_csv(OUT_PATH, show_col_types = FALSE),
      error = function(e) { message("Live data: could not read ", OUT_PATH, " ", conditionMessage(e)); tibble::tibble() }
    )
  }
  # Trim to exactly last YEARS_TO_KEEP years (e.g. 2022–2026 when current_year=2026)
  if (nrow(existing) > 0 && "year" %in% names(existing)) {
    min_year <- current_year - YEARS_TO_KEEP + 1L
    n_before <- nrow(existing)
    existing <- existing %>% dplyr::filter(as.integer(year) >= min_year)
    if (nrow(existing) < n_before) {
      message("Live data: trimmed to ", YEARS_TO_KEEP, " years (", min_year, "–", current_year, "); ", n_before - nrow(existing), " older rows dropped.")
    }
  }
  # Normalize existing columns and types for bind_rows
  if (nrow(existing) > 0) {
    for (c in COL_ORDER_LIVE) if (!c %in% names(existing)) existing[[c]] <- NA
    existing <- existing %>% dplyr::select(dplyr::any_of(COL_ORDER_LIVE)) %>%
      dplyr::mutate(
        event_id = as.character(event_id),
        year = as.integer(year),
        event_completed = event_completed_chr_for_csv(.data$event_completed),
        teetime = teetime_chr_for_csv(.data$teetime)
      )
  }

  combined <- existing
  updated <- FALSE

  # 1) Fetch full year (event_id=all) for each refresh year — JSON-first for recent years in fetch_year_rounds
  yrs_refresh <- historical_round_refresh_years()
  message("Live data: refresh years: ", paste(yrs_refresh, collapse = ", "))
  if (length(yrs_refresh)) {
    message("Live data: Tip: newest calendar year (", max(yrs_refresh), ") is fetched last — scroll down after 2025 finishes; each year can take ~30–90s.")
  }
  n_y <- length(yrs_refresh)
  for (ii in seq_along(yrs_refresh)) {
    yr <- yrs_refresh[[ii]]
    if (yr < 2004L) next
    message("Live data: [", ii, "/", n_y, "] fetching calendar year ", yr, " (event_id=all) …")
    year_rounds <- fetch_year_rounds(yr, "pga")
    Sys.sleep(0.2)
    if (nrow(year_rounds) > 0) {
      year_rounds <- year_rounds %>% dplyr::mutate(
        event_id = as.character(event_id),
        year = as.integer(year),
        event_completed = event_completed_chr_for_csv(.data$event_completed),
        teetime = teetime_chr_for_csv(.data$teetime)
      )
      for (c in COL_ORDER_LIVE) if (!c %in% names(year_rounds)) year_rounds[[c]] <- NA
      year_rounds <- year_rounds %>% dplyr::select(dplyr::any_of(COL_ORDER_LIVE))
      combined <- combined %>% dplyr::mutate(
        event_completed = event_completed_chr_for_csv(.data$event_completed),
        teetime = teetime_chr_for_csv(.data$teetime)
      )
      # Only replace PGA rows for this calendar year (LIV/other tours may come from Node refresh)
      combined <- combined %>%
        dplyr::mutate(
          .tn = tolower(trimws(as.character(.data$tour))),
          .tn = dplyr::if_else(is.na(.tn) | .tn == "", "pga", .tn)
        ) %>%
        dplyr::filter(!(as.integer(.data$year) == yr & .tn == "pga")) %>%
        dplyr::select(-.tn)
      combined <- dplyr::bind_rows(combined, year_rounds)
      message("Live data: added ", nrow(year_rounds), " rounds for ", yr, " (event_id=all).")
      updated <- TRUE
    } else {
      message("Live data: WARNING year ", yr, " returned 0 rows — CSV will keep prior rows for that year if any.")
    }
  }

  # 2) Optionally merge current event (can add/freshen one tournament when API supports it)
  meta <- current_event_meta()
  if (is.null(meta) && file.exists(OUT_PATH) && nrow(existing) > 0) {
    meta <- current_event_meta_from_csv(OUT_PATH)
  }
  if (!is.null(meta)) {
    new_rounds <- fetch_event_rounds(meta$year, meta$event_id, meta$tour)
    if (nrow(new_rounds) > 0) {
      for (c in COL_ORDER_LIVE) if (!c %in% names(new_rounds)) new_rounds[[c]] <- NA
      new_rounds <- new_rounds %>% dplyr::select(dplyr::any_of(COL_ORDER_LIVE)) %>%
        dplyr::mutate(
          event_id = as.character(event_id),
          year = as.integer(year),
          event_completed = event_completed_chr_for_csv(.data$event_completed),
          teetime = teetime_chr_for_csv(.data$teetime)
        )
      ev_id <- as.character(meta$event_id)
      combined <- combined %>% dplyr::mutate(
        event_completed = event_completed_chr_for_csv(.data$event_completed),
        teetime = teetime_chr_for_csv(.data$teetime)
      )
      combined <- combined %>% dplyr::filter(as.character(event_id) != ev_id)
      combined <- dplyr::bind_rows(combined, new_rounds)
      message("Live data: added ", nrow(new_rounds), " rounds for ", meta$event_name, " (event_id=", ev_id, ", year=", meta$year, ").")
      updated <- TRUE
    }
  }

  if (nrow(combined) == 0) {
    message("Live data: no data to write; file unchanged.")
    return(invisible(nrow(existing)))
  }

  combined <- combined %>% dplyr::arrange(dplyr::desc(year), event_id, dg_id, round_num)
  # Ensure columns match COL_ORDER_LIVE for consistency
  for (c in COL_ORDER_LIVE) if (!c %in% names(combined)) combined[[c]] <- NA
  combined <- combined %>% dplyr::select(dplyr::any_of(COL_ORDER_LIVE))
  combined <- combined %>% dplyr::mutate(
    event_id = as.character(event_id),
    year = as.integer(year),
    event_completed = event_completed_chr_for_csv(.data$event_completed),
    teetime = teetime_chr_for_csv(.data$teetime)
  )

  write_path <- OUT_PATH
  out_dir <- dirname(write_path)
  if (!dir.exists(out_dir)) {
    message("Live data: creating directory ", out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = TRUE)
  }
  err <- tryCatch({
    readr::write_csv(combined, write_path)
    NULL
  }, error = function(e) e)
  if (!is.null(err)) {
    message("Live data: failed to write — ", conditionMessage(err))
    # Fallback: try current working directory
    fallback <- file.path(getwd(), "data", "historical_rounds_all.csv")
    if (fallback != write_path) {
      message("Live data: trying fallback path ", fallback)
      err2 <- tryCatch({ readr::write_csv(combined, fallback); NULL }, error = function(e) e)
      if (is.null(err2) && file.exists(fallback)) {
        message("Live data: wrote to fallback ", fallback, " (", nrow(combined), " rows)")
        return(invisible(nrow(combined)))
      }
    }
    return(invisible(0L))
  }
  if (file.exists(write_path)) {
    fi <- file.info(write_path)
    message("Live data: wrote ", write_path, " (", nrow(combined), " rows; ", fi$size, " bytes; mtime ", format(fi$mtime, "%Y-%m-%d %H:%M:%S"), ")")
  } else {
    message("Live data: WARNING — file not found after write at ", write_path)
  }
  invisible(nrow(combined))
}

#' Re-save historical_rounds_all.csv: read, trim to last 5 years, write back. No API calls.
#' Use this to verify the file path is writable and to refresh the file.
refresh_historical_rounds_file <- function() {
  message("Live data: refresh only (no API); OUT_PATH = ", OUT_PATH)
  if (!file.exists(OUT_PATH)) {
    message("Live data: file not found; nothing to refresh.")
    return(invisible(0L))
  }
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  x <- tryCatch(readr::read_csv(OUT_PATH, show_col_types = FALSE), error = function(e) { message("Read error: ", conditionMessage(e)); NULL })
  if (is.null(x) || nrow(x) == 0) {
    message("Live data: no rows to write.")
    return(invisible(0L))
  }
  if ("year" %in% names(x)) {
    min_year <- as.integer(format(Sys.Date(), "%Y")) - YEARS_TO_KEEP + 1L
    x <- x %>% dplyr::filter(as.integer(year) >= min_year)
  }
  if (nrow(x) == 0) {
    message("Live data: no rows left after trimming to ", YEARS_TO_KEEP, " years.")
    return(invisible(0L))
  }
  err <- tryCatch({ readr::write_csv(x, OUT_PATH); NULL }, error = function(e) e)
  if (!is.null(err)) {
    message("Live data: write failed — ", conditionMessage(err))
    return(invisible(0L))
  }
  fi <- file.info(OUT_PATH)
  message("Live data: refreshed ", OUT_PATH, " (", nrow(x), " rows; mtime ", format(fi$mtime, "%Y-%m-%d %H:%M:%S"), ")")
  invisible(nrow(x))
}

