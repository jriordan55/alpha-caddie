# Between-round projection nudges from DataGolf (batch / pipeline — no browser polling).
# Fetches preds/live-tournament-stats per completed round (sg_total) and builds
# within_event_form_tbl rows compatible with round_projections.R (mu_sg += within_form_shift).
#
# Set GOLF_DG_WITHIN_EVENT_FORM_API=0 to disable. Requires DATAGOLF_API_KEY (or existing API_KEY in env).

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

#' Parse live-tournament-stats JSON into a tibble with dg_id + sg_total.
dg_lts_dataframe <- function(dat) {
  if (is.null(dat)) return(NULL)
  df <- NULL
  if (is.data.frame(dat)) {
    df <- dat
  } else if (is.list(dat) && is.data.frame(dat$data)) {
    df <- dat$data
  } else if (is.list(dat) && is.list(dat$data) && length(dat$data) > 0L) {
    df <- tryCatch(
      dplyr::bind_rows(lapply(dat$data, function(row) {
        tibble::as_tibble(as.data.frame(row, stringsAsFactors = FALSE))
      })),
      error = function(e) NULL
    )
  }
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  if (!"dg_id" %in% names(df)) return(NULL)
  id <- suppressWarnings(as.integer(df$dg_id))
  sg <- if ("sg_total" %in% names(df)) {
    suppressWarnings(as.numeric(df$sg_total))
  } else if ("total" %in% names(df)) {
    suppressWarnings(as.numeric(df$total))
  } else {
    rep(NA_real_, nrow(df))
  }
  tibble::tibble(dg_id = id, sg_total = sg) %>%
    dplyr::filter(is.finite(.data$dg_id), is.finite(.data$sg_total))
}

#' One round slice from preds/live-tournament-stats (SG for that round only).
fetch_datagolf_live_tournament_stats_round <- function(api_key, tour, round_num) {
  if (!nzchar(api_key %||% "")) return(NULL)
  rn <- as.integer(round_num)
  if (!is.finite(rn) || rn < 1L || rn > 4L) return(NULL)
  tr <- trimws(as.character(tour %||% "pga"))
  if (!nzchar(tr)) tr <- "pga"
  tryCatch({
    res <- httr::GET(
      "https://feeds.datagolf.com/preds/live-tournament-stats",
      query = list(
        tour = tr,
        stats = "sg_total",
        round = as.character(rn),
        display = "value",
        file_format = "json",
        key = api_key
      )
    )
    if (res$status_code != 200L) return(NULL)
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    dat <- jsonlite::fromJSON(txt, flatten = TRUE, simplifyDataFrame = FALSE)
    dg_lts_dataframe(dat)
  }, error = function(e) {
    message("live-tournament-stats R", rn, ": ", conditionMessage(e))
    NULL
  })
}

#' Optional: preds/in-play JSON for event name + current_round (sanity check).
fetch_datagolf_in_play_meta <- function(api_key, tour = "pga") {
  if (!nzchar(api_key %||% "")) return(list(event_name = "", current_round = NA_integer_))
  tr <- trimws(as.character(tour %||% "pga"))
  tryCatch({
    res <- httr::GET(
      "https://feeds.datagolf.com/preds/in-play",
      query = list(
        tour = tr,
        dead_heat = "no",
        odds_format = "percent",
        file_format = "json",
        key = api_key
      )
    )
    if (res$status_code != 200L) return(list(event_name = "", current_round = NA_integer_))
    dat <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    info <- if (is.list(dat) && is.list(dat$info)) dat$info else list()
    en <- trimws(as.character(info$event_name %||% dat$event_name %||% ""))
    cr <- suppressWarnings(as.integer(info$current_round %||% NA_integer_))
    list(event_name = en, current_round = cr)
  }, error = function(e) {
    list(event_name = "", current_round = NA_integer_)
  })
}

#' Build within_event_form_tbl from API when historical_rounds_all has no current-event rows.
#'
#' For target rounds tr = 2..4, shift = K * (sum_{r<tr} sg_r - (tr-1)*base_mu_sg), capped.
#'
#' @param dg_base tibble with dg_id, base_mu_sg
#' @param within_form_k,cap same semantics as GOLF_WITHIN_EVENT_FORM_CARRY / _CAP
#' @return tibble(dg_id, round, within_form_shift) with one row per (dg_id, round) for rounds 2..4
build_within_event_form_tbl_from_datagolf_lts <- function(
    dg_base,
    api_key,
    tour = "pga",
    within_form_k = 0.02,
    within_form_cap = 0.3,
    event_name_norm = "",
    require_event_match = TRUE) {
  empty <- tibble::tibble(dg_id = integer(), round = integer(), within_form_shift = numeric())
  if (!is.data.frame(dg_base) || nrow(dg_base) == 0L || !all(c("dg_id", "base_mu_sg") %in% names(dg_base))) {
    return(empty)
  }
  if (!is.finite(within_form_k) || within_form_k == 0) return(empty)
  if (!is.finite(within_form_cap) || within_form_cap <= 0) within_form_cap <- 0.3

  meta <- fetch_datagolf_in_play_meta(api_key, tour)
  if (isTRUE(require_event_match) && nzchar(event_name_norm) && nzchar(meta$event_name)) {
    # Reuse normalizer from parent — caller passes normalized key
    api_key_ev <- if (exists("normalize_event_name", mode = "function")) {
      normalize_event_name(meta$event_name)
    } else {
      tolower(trimws(meta$event_name))
    }
    if (nzchar(api_key_ev) && !identical(api_key_ev, event_name_norm)) {
      message("within-event form API: skipping (in-play event does not match field-updates event).")
      return(empty)
    }
  }

  sg_by_round <- list()
  for (r in 1L:3L) {
    tb <- fetch_datagolf_live_tournament_stats_round(api_key, tour, r)
    if (!is.null(tb) && nrow(tb) > 0L) sg_by_round[[as.character(r)]] <- tb
  }
  if (length(sg_by_round) == 0L) return(empty)

  rows <- list()
  udg <- sort(unique(suppressWarnings(as.integer(dg_base$dg_id))))
  base_ref <- dg_base %>%
    dplyr::distinct(.data$dg_id, .keep_all = TRUE) %>%
    dplyr::transmute(
      dg_id = suppressWarnings(as.integer(.data$dg_id)),
      base_mu_sg = suppressWarnings(as.numeric(.data$base_mu_sg))
    ) %>%
    dplyr::filter(is.finite(.data$dg_id), is.finite(.data$base_mu_sg))

  for (tr in 2L:4L) {
    prev <- seq_len(tr - 1L)
    for (i in seq_along(udg)) {
      id <- udg[i]
      b <- base_ref$base_mu_sg[base_ref$dg_id == id]
      if (length(b) != 1L || !is.finite(b)) next
      cum_sg <- 0
      n_act <- 0L
      for (r in prev) {
        key <- as.character(r)
        if (!key %in% names(sg_by_round)) next
        tbr <- sg_by_round[[key]]
        v <- tbr$sg_total[tbr$dg_id == id]
        if (length(v) >= 1L && is.finite(v[1L])) {
          cum_sg <- cum_sg + v[1L]
          n_act <- n_act + 1L
        }
      }
      if (n_act < 1L) next
      total_surplus <- cum_sg - as.numeric(n_act) * as.numeric(b)
      sh <- within_form_k * total_surplus
      if (!is.finite(sh)) sh <- 0
      sh <- pmax(-within_form_cap, pmin(within_form_cap, sh))
      rows[[length(rows) + 1L]] <- tibble::tibble(dg_id = id, round = tr, within_form_shift = sh)
    }
  }
  if (length(rows) == 0L) return(empty)
  dplyr::bind_rows(rows)
}
