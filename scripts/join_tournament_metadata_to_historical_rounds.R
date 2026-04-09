#!/usr/bin/env Rscript

options(warn = 1)

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else NA_character_
repo_root <- if (is.finite(nchar(script_path)) && nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
setwd(repo_root)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl: install.packages('readxl')", call. = FALSE)
}

map_path <- file.path(repo_root, "data", "pga_datagolf_tournament_map.csv")
web_data_dir <- file.path(repo_root, "alpha-caddie-web", "data")
meta_path <- Sys.getenv("PGA_TOURNAMENT_METADATA_XLSX", "")
if (!nzchar(meta_path)) {
  if (dir.exists(web_data_dir)) {
    patt_files <- list.files(
      web_data_dir,
      pattern = "^pga_tournament_metadata_last_[0-9]+y\\.xlsx$",
      full.names = TRUE
    )
    if (length(patt_files)) {
      ns <- suppressWarnings(as.integer(sub(".*_last_([0-9]+)y\\.xlsx$", "\\1", basename(patt_files))))
      ok <- is.finite(ns)
      if (any(ok)) meta_path <- patt_files[ok][which.max(ns[ok])]
    }
  }
}
if (!nzchar(meta_path) || !file.exists(meta_path)) {
  fallback <- if (dir.exists(web_data_dir)) {
    list.files(web_data_dir, pattern = "^pga_tournament_metadata.*\\.xlsx$", full.names = TRUE)
  } else character()
  meta_path <- if (length(fallback)) fallback[[1]] else ""
}
hist_paths <- c(
  file.path(repo_root, "data", "historical_rounds_all.csv"),
  file.path(repo_root, "alpha-caddie-web", "data", "historical_rounds_all.csv")
)

if (!file.exists(map_path)) stop("Missing mapping file: ", map_path, call. = FALSE)
if (!nzchar(meta_path) || !file.exists(meta_path)) {
  stop(
    "Missing PGA metadata .xlsx. Set PGA_TOURNAMENT_METADATA_XLSX or run pull_pga_tournament_metadata_excel.R ",
    "into alpha-caddie-web/data/.",
    call. = FALSE
  )
}

message("Loading mapping: ", map_path)
tmap <- utils::read.csv(map_path, stringsAsFactors = FALSE, check.names = FALSE)
tmap$event_id <- suppressWarnings(as.integer(tmap$dg_event_id))
tmap$year_i <- suppressWarnings(as.integer(tmap$year))
tmap <- tmap[is.finite(tmap$event_id) & is.finite(tmap$year_i) & nzchar(as.character(tmap$pga_tournament_id)), , drop = FALSE]

mm <- tolower(trimws(as.character(tmap$match_method)))
tmap$._prio <- ifelse(mm == "exact_name_year", 1L,
                ifelse(mm == "partial_name_year", 2L,
                ifelse(mm == "contains_name_year", 3L, 9L)))
tmap <- tmap[order(tmap$event_id, tmap$year_i, tmap$._prio, tmap$pga_tournament_id), , drop = FALSE]
tmap <- tmap[!duplicated(paste(tmap$event_id, tmap$year_i, sep = "|")), , drop = FALSE]

tmap_small <- data.frame(
  event_id = tmap$event_id,
  year = tmap$year_i,
  pga_tournament_id = as.character(tmap$pga_tournament_id),
  pga_tournament_name = as.character(tmap$pga_tournament_name),
  pga_course_name = as.character(tmap$pga_course_name),
  dg_event_name_mapped = as.character(tmap$dg_event_name),
  dg_course_name_mapped = as.character(tmap$dg_course_name),
  tournament_map_match_method = as.character(tmap$match_method),
  stringsAsFactors = FALSE
)

message("Loading metadata: ", meta_path)
meta <- readxl::read_excel(meta_path, sheet = "tournaments")
meta <- as.data.frame(meta, stringsAsFactors = FALSE)
if (!("id" %in% names(meta))) stop("Metadata sheet missing `id` column.", call. = FALSE)
meta$id <- as.character(meta$id)
meta <- meta[nzchar(meta$id), , drop = FALSE]
meta <- meta[!duplicated(meta$id), , drop = FALSE]

meta_cols <- setdiff(names(meta), "id")
names(meta)[names(meta) %in% meta_cols] <- paste0("pga_meta_", meta_cols)

# Keep temperatures as plain numeric values for easier analysis/export.
if ("pga_meta_weather_temp_f" %in% names(meta)) {
  meta$pga_meta_weather_temp_f <- suppressWarnings(as.numeric(gsub("[^0-9.-]+", "", as.character(meta$pga_meta_weather_temp_f))))
}
if ("pga_meta_weather_temp_c" %in% names(meta)) {
  meta$pga_meta_weather_temp_c <- suppressWarnings(as.numeric(gsub("[^0-9.-]+", "", as.character(meta$pga_meta_weather_temp_c))))
}

for (hp in hist_paths) {
  if (!file.exists(hp)) {
    warning("Skipping missing file: ", hp)
    next
  }
  message("Joining into: ", hp)
  hist <- utils::read.csv(hp, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("event_id", "year") %in% names(hist))) {
    warning("Skipping (missing event_id/year): ", hp)
    next
  }
  hist$event_id <- suppressWarnings(as.integer(hist$event_id))
  hist$year <- suppressWarnings(as.integer(hist$year))

  add_cols <- unique(c(names(tmap_small)[!(names(tmap_small) %in% c("event_id", "year"))], names(meta)[names(meta) != "id"]))
  drop_existing <- intersect(names(hist), add_cols)
  if (length(drop_existing)) hist[drop_existing] <- NULL

  out <- merge(hist, tmap_small, by = c("event_id", "year"), all.x = TRUE, sort = FALSE)
  out <- merge(out, meta, by.x = "pga_tournament_id", by.y = "id", all.x = TRUE, sort = FALSE)

  # Keep original column order first, then appended fields.
  orig <- names(hist)
  appended <- setdiff(names(out), orig)
  out <- out[c(orig, appended)]

  wrote <- FALSE
  try({
    utils::write.csv(out, hp, row.names = FALSE, na = "")
    wrote <- TRUE
  }, silent = TRUE)
  if (!wrote) {
    alt <- sub("\\.csv$", "_with_tournament_metadata.csv", hp, ignore.case = TRUE)
    wrote_alt <- FALSE
    try({
      utils::write.csv(out, alt, row.names = FALSE, na = "")
      wrote_alt <- TRUE
    }, silent = TRUE)
    if (wrote_alt) {
      message("  Rows: ", nrow(out), " | Added columns: ", length(appended), " | Wrote fallback: ", alt)
    } else {
      alt2 <- sub("\\.csv$", paste0("_with_tournament_metadata_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"), hp, ignore.case = TRUE)
      utils::write.csv(out, alt2, row.names = FALSE, na = "")
      message("  Rows: ", nrow(out), " | Added columns: ", length(appended), " | Wrote timestamp fallback: ", alt2)
    }
  } else {
    message("  Rows: ", nrow(out), " | Added columns: ", length(appended))
  }
}

message("Done.")
