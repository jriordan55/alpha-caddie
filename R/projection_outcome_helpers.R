# Helpers for round_projections.R: shot-MC outcome counts + per-round scaling vs historical field.
# No analytic default score priors — shot resampling uses empirical transitions in shot_transition_model.rds.

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (!is.null(a)) a else b

renormalize_counts_5 <- function(v) {
  v <- pmax(0, as.numeric(v))
  s <- sum(v)
  if (!is.finite(s) || s <= 0) return(rep(NA_real_, length(v)))
  v * 18 / s
}

#' Apply field-relative count ratios for R2–R4 vs R1 from historical_rounds_all at this course.
apply_round_count_ratios <- function(counts_named, round_idx, ratio_mat) {
  if (is.null(ratio_mat) || nrow(ratio_mat) < 1L) return(counts_named)
  round_idx <- as.integer(round_idx)
  if (!is.finite(round_idx) || round_idx < 1L || round_idx > nrow(ratio_mat)) return(counts_named)
  r <- ratio_mat[round_idx, , drop = TRUE]
  if (length(r) != length(counts_named)) return(counts_named)
  if (any(!is.finite(r))) return(counts_named)
  nm <- names(counts_named)
  scaled <- as.numeric(counts_named) * as.numeric(r)
  out <- renormalize_counts_5(scaled)
  names(out) <- nm
  out
}

#' Monte Carlo mean eagles/birdies/pars/bogeys/doubles+ per player (shot resampling).
build_shot_outcome_counts_tbl <- function(dg_ids, holes_par, holes_yardage, fit, model_dir, n_sims = 12L) {
  if (length(holes_par) != 18L || length(holes_yardage) != 18L || is.null(fit)) return(NULL)
  if (!requireNamespace("tibble", quietly = TRUE)) return(NULL)
  if (!exists("dg_id_to_pga_player_id", mode = "function")) {
    source(file.path(model_dir, "R", "player_id_mapping.R"), encoding = "UTF-8")
  }
  if (!exists("mc_mean_round_projection_metrics", mode = "function")) {
    source(file.path(model_dir, "R", "shot_level_model.R"), encoding = "UTF-8")
  }
  map_df <- load_pga_datagolf_map(file.path(model_dir, "data", "pga_datagolf_player_map.csv"))
  pga_ids <- dg_id_to_pga_player_id(dg_ids, map_df)
  n_sims <- as.integer(max(5L, min(80L, n_sims)))
  out <- tibble::tibble(
    dg_id = as.integer(dg_ids),
    s_eagles = NA_real_, s_birdies = NA_real_, s_pars = NA_real_, s_bogeys = NA_real_, s_doubles = NA_real_,
    s_gir = NA_real_, s_fairways = NA_real_
  )
  for (i in seq_along(dg_ids)) {
    pid <- pga_ids[i]
    if (!nzchar(pid %||% "") || is.na(pid)) pid <- "0"
    v <- tryCatch(
      mc_mean_round_projection_metrics(
        pid, holes_par, holes_yardage, fit,
        n_sims = n_sims, player_weight = 0.65,
        seed = (as.integer(dg_ids[i]) %% 100000L) + 1L
      ),
      error = function(e) rep(NA_real_, 7L)
    )
    if (length(v) == 7L && all(is.finite(v))) {
      out$s_eagles[i] <- v[["eagles"]]
      out$s_birdies[i] <- v[["birdies"]]
      out$s_pars[i] <- v[["pars"]]
      out$s_bogeys[i] <- v[["bogeys"]]
      out$s_doubles[i] <- v[["doubles_plus"]]
      out$s_gir[i] <- v[["gir"]]
      out$s_fairways[i] <- v[["fairways"]]
    }
  }
  out
}
