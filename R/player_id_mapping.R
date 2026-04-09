# PGA Tour (pgatouR) <-> DataGolf (dg_id) player ID mapping for combined modeling.
#
# DataGolf names in this project are typically "Last, First" (see historical_rounds_all.csv,
# field-updates). pgatouR provides first_name, last_name, display_name, player_id.
#
# Usage:
#   source("R/player_id_mapping.R")
#   map <- load_pga_datagolf_map()
#   dg <- pga_player_id_to_dg_id(c("11249", "20051"), map)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Bag-of-words key (same idea as app.R name_match_key, vectorized).
name_match_key_vec <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- gsub(",", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  vapply(
    strsplit(x, " ", fixed = TRUE),
    function(words) {
      words <- trimws(words[nzchar(words)])
      if (length(words) == 0) return("")
      paste(sort(words), collapse = " ")
    },
    character(1)
  )
}

# DataGolf-style "Last, First" from pgatouR columns (best exact match to dg player_name).
pga_last_first <- function(first_name, last_name) {
  paste(trimws(as.character(last_name)), trimws(as.character(first_name)), sep = ", ")
}

# Map full country names (pgatouR) to DataGolf 3-letter codes used in field-updates.
pga_country_to_dg_code <- c(
  "United States" = "USA", "USA" = "USA",
  "England" = "ENG", "Scotland" = "SCO", "Wales" = "WAL", "Northern Ireland" = "NIR",
  "Republic of Ireland" = "IRL", "Ireland" = "IRL",
  "South Africa" = "RSA", "Korea" = "KOR", "South Korea" = "KOR",
  "Chinese Taipei" = "TPE",
  "Czech Republic" = "CZE", "Czechia" = "CZE",
  "United Arab Emirates" = "UAE",
  "Venezuela" = "VEN", "Colombia" = "COL", "Argentina" = "ARG", "Australia" = "AUS",
  "Austria" = "AUT", "Belgium" = "BEL", "Brazil" = "BRA", "Canada" = "CAN",
  "Chile" = "CHL", "China" = "CHN", "Denmark" = "DEN", "Finland" = "FIN",
  "France" = "FRA", "Germany" = "GER", "India" = "IND", "Italy" = "ITA",
  "Japan" = "JPN", "Malaysia" = "MAS", "Mexico" = "MEX", "Netherlands" = "NED",
  "New Zealand" = "NZL", "Norway" = "NOR", "Paraguay" = "PAR", "Peru" = "PER",
  "Philippines" = "PHI", "Portugal" = "POR", "Puerto Rico" = "PUR", "Singapore" = "SGP",
  "Spain" = "ESP", "Sweden" = "SWE", "Switzerland" = "SUI", "Thailand" = "THA",
  "Zimbabwe" = "ZIM", "Fiji" = "FIJ", "Guam" = "GUM", "Jamaica" = "JAM"
)

lookup_pga_country_code <- function(country) {
  c <- trimws(as.character(country))
  out <- rep(NA_character_, length(c))
  hit <- c %in% names(pga_country_to_dg_code)
  out[hit] <- unname(pga_country_to_dg_code[c[hit]])
  out
}

#' Fetch DataGolf dg_id -> country (3-letter) from current field-updates (when available).
fetch_dg_id_country <- function(api_key = Sys.getenv("DATAGOLF_API_KEY", "")) {
  if (!nzchar(api_key)) return(NULL)
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("readr", quietly = TRUE)) return(NULL)
  res <- tryCatch(
    httr::GET(
      "https://feeds.datagolf.com/field-updates",
      query = list(tour = "pga", file_format = "csv", key = api_key)
    ),
    error = function(e) NULL
  )
  if (is.null(res) || httr::status_code(res) != 200) return(NULL)
  tab <- tryCatch(readr::read_csv(httr::content(res, "raw"), show_col_types = FALSE), error = function(e) NULL)
  if (is.null(tab) || nrow(tab) == 0 || !all(c("dg_id", "country") %in% names(tab))) return(NULL)
  u <- unique(tab[, c("dg_id", "country")])
  u$dg_id <- as.integer(u$dg_id)
  u$country <- toupper(trimws(as.character(u$country)))
  u[!is.na(u$dg_id) & nzchar(u$country), , drop = FALSE]
}

#' Build mapping table: pgatouR player_id <-> DataGolf dg_id.
#'
#' @param historical_rounds_path Path to historical_rounds_all.csv (has dg_id + DataGolf player_name).
#' @param tour_code pgatouR tour, default "R" (PGA Tour).
#' @param api_key DataGolf API key (optional; improves tie-breaks via field country).
#' @return A data.frame with pga_player_id, dg_id, match_method, confidence, etc.
build_pga_datagolf_map <- function(
  historical_rounds_path = NULL,
  tour_code = "R",
  api_key = Sys.getenv("DATAGOLF_API_KEY", "")
) {
  if (!requireNamespace("pgatouR", quietly = TRUE)) stop("Install pgatouR: remotes::install_github('WalrusQuant/pgatouR')")
  if (is.null(historical_rounds_path)) {
    historical_rounds_path <- file.path(
      if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd(),
      "data", "historical_rounds_all.csv"
    )
  }
  if (!file.exists(historical_rounds_path)) {
    stop("historical_rounds_all.csv not found at: ", historical_rounds_path)
  }

  if (!requireNamespace("readr", quietly = TRUE)) stop("Install readr.")
  hist <- readr::read_csv(historical_rounds_path, show_col_types = FALSE)
  if (!all(c("dg_id", "player_name") %in% names(hist))) {
    stop("historical file must contain dg_id and player_name.")
  }
  hist$player_name <- trimws(as.character(hist$player_name))
  dg_from_hist <- stats::aggregate(
    list(n_rounds = rep(1L, nrow(hist))),
    by = list(dg_id = hist$dg_id, player_name = hist$player_name),
    FUN = length
  )

  dg_from_hist$dg_id <- as.integer(dg_from_hist$dg_id)
  dg_from_hist$player_name <- trimws(as.character(dg_from_hist$player_name))
  dg_from_hist <- dg_from_hist[!is.na(dg_from_hist$dg_id) & nzchar(dg_from_hist$player_name), , drop = FALSE]
  dg_from_hist$name_norm <- tolower(dg_from_hist$player_name)
  dg_from_hist$name_key <- name_match_key_vec(dg_from_hist$player_name)

  dg_country <- fetch_dg_id_country(api_key)
  if (!is.null(dg_country) && nrow(dg_country) > 0) {
    dg_from_hist <- merge(
      dg_from_hist,
      dg_country,
      by = "dg_id",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    dg_from_hist$country <- NA_character_
  }

  pga <- pgatouR::pga_players(tour_code)
  pga <- as.data.frame(pga)
  pga$pga_player_id <- as.character(pga$player_id)
  pga$pga_last_first <- pga_last_first(pga$first_name, pga$last_name)
  pga$name_norm <- tolower(trimws(pga$pga_last_first))
  pga$name_key <- name_match_key_vec(pga$pga_last_first)
  pga$pga_country_code <- lookup_pga_country_code(pga$country)

  used_dg <- rep(FALSE, nrow(dg_from_hist))
  matched_pga <- rep(FALSE, nrow(pga))
  out_rows <- list()
  out_i <- 0L

  add_match <- function(pga_idx, dg_idx, method, conf) {
    out_i <<- out_i + 1L
    used_dg[dg_idx] <<- TRUE
    matched_pga[pga_idx] <<- TRUE
    d <- dg_from_hist[dg_idx, , drop = FALSE]
    p <- pga[pga_idx, , drop = FALSE]
    dg_ct <- d$country[[1]]
    if (is.null(dg_ct) || (length(dg_ct) == 1 && is.na(dg_ct))) dg_ct <- NA_character_
    out_rows[[out_i]] <<- data.frame(
      pga_player_id = p$pga_player_id[[1]],
      pga_display_name = p$display_name[[1]],
      pga_first_name = p$first_name[[1]],
      pga_last_name = p$last_name[[1]],
      pga_country = p$country[[1]],
      dg_id = d$dg_id[[1]],
      dg_player_name = d$player_name[[1]],
      dg_country = as.character(dg_ct %||% NA_character_),
      n_rounds_hist = d$n_rounds[[1]],
      match_method = method,
      confidence = conf,
      stringsAsFactors = FALSE
    )
  }

  # 1) Exact "Last, First" match (normalized to lower).
  for (i in seq_len(nrow(pga))) {
    hit <- which(!used_dg & dg_from_hist$name_norm == pga$name_norm[i])
    if (length(hit) == 1L) {
      add_match(i, hit[[1]], "last_first_exact", 1)
    } else if (length(hit) > 1L) {
      cc <- pga$pga_country_code[i]
      if (!is.na(cc) && nzchar(cc)) {
        hit2 <- hit[toupper(as.character(dg_from_hist$country[hit])) == cc]
        if (length(hit2) == 1L) add_match(i, hit2[[1]], "last_first_exact_country", 0.95)
      }
    }
  }

  # 2) Bag-of-words key match for unmatched PGA rows.
  for (i in seq_len(nrow(pga))) {
    if (matched_pga[i]) next
    if (!nzchar(pga$name_key[i])) next
    hit <- which(!used_dg & dg_from_hist$name_key == pga$name_key[i])
    if (length(hit) == 0L) next
    if (length(hit) == 1L) {
      add_match(i, hit[[1]], "name_key", 0.85)
      next
    }
    cc <- pga$pga_country_code[i]
    if (!is.na(cc) && nzchar(cc)) {
      hit2 <- hit[toupper(as.character(dg_from_hist$country[hit])) == cc]
      if (length(hit2) == 1L) {
        add_match(i, hit2[[1]], "name_key_country", 0.8)
        next
      }
    }
    # Ambiguous: pick dg_id with most historical rounds.
    best <- hit[[which.max(dg_from_hist$n_rounds[hit])]]
    add_match(i, best, "name_key_fallback_max_rounds", 0.5)
  }

  if (length(out_rows) == 0) return(data.frame())
  out <- do.call(rbind, out_rows)
  rownames(out) <- NULL
  out
}

#' @param map_path Path to CSV written by build script (default data/pga_datagolf_player_map.csv).
load_pga_datagolf_map <- function(map_path = NULL) {
  if (is.null(map_path)) {
    map_path <- file.path(
      if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd(),
      "data", "pga_datagolf_player_map.csv"
    )
  }
  if (!file.exists(map_path)) {
    warning("Mapping file not found: ", map_path, " — run scripts/build_pga_datagolf_player_map.R")
    return(NULL)
  }
  read.csv(map_path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Vectorized: pgatouR player_id -> DataGolf dg_id (NA if unknown).
pga_player_id_to_dg_id <- function(pga_player_id, map_df = NULL) {
  map_df <- map_df %||% load_pga_datagolf_map()
  if (is.null(map_df)) return(rep(NA_integer_, length(pga_player_id)))
  m <- match(as.character(pga_player_id), as.character(map_df$pga_player_id))
  as.integer(map_df$dg_id[m])
}

#' Vectorized: DataGolf dg_id -> pgatouR player_id (NA if unknown).
dg_id_to_pga_player_id <- function(dg_id, map_df = NULL) {
  map_df <- map_df %||% load_pga_datagolf_map()
  if (is.null(map_df)) return(rep(NA_character_, length(dg_id)))
  m <- match(as.integer(dg_id), as.integer(map_df$dg_id))
  as.character(map_df$pga_player_id[m])
}
