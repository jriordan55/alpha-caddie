# Hole-by-hole scorecards via pgatouR (WalrusQuant/pgatouR).
#
# Official function: pgatouR::pga_scorecard(tournament_id, player_id)
# See package docs: one row per hole per round (hole_number, par, score, yardage, round_number, course_name, ...).
#
# Usage:
#   source("R/scorecard.R")
#   pga_scorecard_safe("R2026475", "39971")  # empty tibble on error / no data

pga_scorecard_safe <- function(tournament_id, player_id) {
  if (!requireNamespace("pgatouR", quietly = TRUE)) {
    stop("Install pgatouR: remotes::install_github('WalrusQuant/pgatouR')")
  }
  tid <- as.character(tournament_id)
  pid <- as.character(player_id)
  if (!nzchar(tid) || !nzchar(pid)) {
    return(tibble::tibble())
  }
  out <- tryCatch(
    pgatouR::pga_scorecard(tid, pid),
    error = function(e) {
      tibble::tibble()
    }
  )
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) {
    return(tibble::tibble())
  }
  out
}
