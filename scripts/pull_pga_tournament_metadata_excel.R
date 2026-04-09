# Pull pgatouR::pga_tournament metadata for tournaments on the PGA schedule over the last N
# calendar years. Default N = (current year - 2004 + 1), i.e. 2004 through current year.
#
# Requires: pgatouR, writexl (or install.packages("writexl")), jsonlite (usually with pgatouR)
#
# Usage (from repo root):
#   Rscript scripts/pull_pga_tournament_metadata_excel.R
# Optional args: [output_path] [years_back]
# Example (only last 10 years):
#   Rscript scripts/pull_pga_tournament_metadata_excel.R "" 10

args <- commandArgs(trailingOnly = TRUE)
args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else NA_character_
repo_root <- if (is.finite(nchar(script_path)) && nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

cur_y <- as.integer(format(Sys.Date(), "%Y"))
first_hist_y <- 2004L
years_back <- max(1L, cur_y - first_hist_y + 1L)
if (length(args) >= 2L) {
  yb <- suppressWarnings(as.integer(args[[2]]))
  if (is.finite(yb) && yb >= 1L) years_back <- yb
}

out_path <- file.path(
  repo_root, "alpha-caddie-web", "data",
  sprintf("pga_tournament_metadata_last_%dy.xlsx", years_back)
)
if (length(args) >= 1L && nzchar(args[[1]])) {
  out_path <- normalizePath(args[[1]], mustWork = FALSE)
}

years <- seq(cur_y - (years_back - 1L), cur_y)
message("years_back = ", years_back, " (calendar years ", min(years), "-", max(years), ")")

if (!requireNamespace("pgatouR", quietly = TRUE)) {
  stop("Install pgatouR, e.g. remotes::install_github('WalrusQuant/pgatouR')", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite: install.packages('jsonlite')", call. = FALSE)
}
if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("Install writexl: install.packages('writexl')", call. = FALSE)
}

message("Schedule years: ", paste(years, collapse = ", "))
ids_chr <- character(0)
for (y in years) {
  sch <- tryCatch(pgatouR::pga_schedule(y), error = function(e) {
    warning("pga_schedule(", y, "): ", conditionMessage(e))
    NULL
  })
  if (is.null(sch) || !nrow(sch)) next
  tid <- sch$tournament_id
  if (is.null(tid)) tid <- sch$tournamentId
  ids_chr <- c(ids_chr, as.character(tid))
}
ids <- sort(unique(ids_chr[nzchar(ids_chr)]))
message("Unique tournament IDs from schedule: ", length(ids))

chunk_size <- 35L
chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
meta_list <- vector("list", length(chunks))
for (i in seq_along(chunks)) {
  message("Fetching metadata batch ", i, " / ", length(chunks), " (", length(chunks[[i]]), " ids) …")
  meta_list[[i]] <- tryCatch(
    pgatouR::pga_tournaments(chunks[[i]]),
    error = function(e) {
      warning(conditionMessage(e))
      if (requireNamespace("tibble", quietly = TRUE)) tibble::tibble() else data.frame()
    }
  )
  if (i < length(chunks)) Sys.sleep(0.25)
}

meta_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, meta_list)
if (!length(meta_list)) stop("No tournament metadata returned.", call. = FALSE)
if (requireNamespace("dplyr", quietly = TRUE)) {
  meta <- dplyr::bind_rows(meta_list)
} else {
  meta <- do.call(rbind, meta_list)
}
# Excel-friendly: flatten list column `courses` to JSON text
if ("courses" %in% names(meta)) {
  meta$courses <- vapply(seq_len(nrow(meta)), function(r) {
    x <- meta$courses[[r]]
    if (is.null(x) || (inherits(x, "data.frame") && nrow(x) == 0L)) return(NA_character_)
    jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE, null = "null")
  }, character(1))
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writexl::write_xlsx(
  list(
    README = data.frame(
      note = c(
        "Source: pgatouR::pga_tournaments(ids); ids from pgatouR::pga_schedule() for listed years.",
        paste0("Generated: ", Sys.time()),
        paste0("Schedule years: ", paste(years, collapse = ", ")),
        "Column courses is JSON (nested course rows from the API)."
      ),
      stringsAsFactors = FALSE
    ),
    tournaments = meta
  ),
  path = out_path
)
message("Wrote ", nrow(meta), " rows -> ", normalizePath(out_path, winslash = "/", mustWork = FALSE))
