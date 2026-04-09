#!/usr/bin/env Rscript
# Demo: load shot_transition_model.rds and run shot-level MC for a few PGA player_ids.
# Hole template (par/yardage) comes from model meta (fit for SELECTED_COURSE).
# Usage: Rscript scripts/demo_shot_level_mc.R

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

source(file.path(repo_root, "R", "shot_level_model.R"))

rds <- file.path(repo_root, "data", "shot_transition_model.rds")
if (!file.exists(rds)) {
  stop("Run scripts/fit_shot_transition_model.R first to create ", rds)
}
obj <- readRDS(rds)
if (is.list(obj) && !is.null(obj$fit)) {
  fit <- obj$fit
  meta <- obj$meta %||% list()
} else {
  fit <- obj
  meta <- list()
}

holes_par <- as.integer(unlist(meta$holes_par))
holes_yardage <- as.integer(round(as.numeric(unlist(meta$holes_yardage))))
if (length(holes_par) != 18L || length(holes_yardage) != 18L || anyNA(holes_par) || anyNA(holes_yardage)) {
  stop("Model missing invalid 18-hole template in meta — refit with scripts/fit_shot_transition_model.R")
}
message("Model course: ", meta$selected_course %||% "(unknown)")
message("Hole template: par sum = ", sum(holes_par), ", yardage sum = ", sum(holes_yardage))

# Example player_ids from pgatouR / shots CSV (strings) — must exist in fit$by_player
pids <- c("20229", "23778", "29910")
pids <- pids[pids %in% names(fit$by_player)]
if (length(pids) < 2L) {
  message("Need >=2 players in model for competitive win%; padding from fit$by_player.")
  extra <- head(setdiff(names(fit$by_player), pids), 5L - length(pids))
  pids <- c(pids, extra)
}
pids <- head(pids, 5L)

message("Running shot-level MC tournament (200 sims) for players: ", paste(pids, collapse = ", "))
res <- shot_level_mc_tournament(
  pids,
  holes_par,
  holes_yardage,
  fit,
  n_sims = 200L,
  player_weight = 0.65,
  seed = 42L
)
names(res$win_prob) <- pids
names(res$top5) <- pids
print(round(res$win_prob, 4))
print(round(res$top5, 4))
