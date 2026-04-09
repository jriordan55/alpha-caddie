# Shot-level empirical model for round / tournament score distributions.
# Uses all_shots_2021_2026.csv: resamples realized shot outcomes conditional on (lie, distance-to-hole bin).
#
# Notation: distance_remaining on each row = yards (or ft/in) TO THE HOLE AFTER that stroke completes.
#
# Pipeline:
#   1) scripts/build_pga_datagolf_tournament_map.R — pgatouR tournament_id <-> DataGolf event_id
#   2) scripts/fit_shot_transition_model.R [max_rows] — writes data/shot_transition_model.rds (uses map + SELECTED_COURSE)
#   3) scripts/demo_shot_level_mc.R — example tournament MC
#
# Integration with round_projections.R: replace the Normal round draw (rnorm on mu_sg) with
# simulate_round_shot_level() per player when you want full shot-path uncertainty. Pass the same
# 18-hole par/yardage template as the event you care about.

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Parse distance_remaining / distance strings to yards to hole (putts: ft/in -> yds).
parse_to_yards <- function(x) {
  x <- trimws(as.character(x))
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    s <- x[i]
    if (is.na(s) || !nzchar(s)) {
      out[i] <- NA_real_
      next
    }
    if (grepl("in the hole", s, ignore.case = TRUE)) {
      out[i] <- 0
      next
    }
    # Yards: "132 yds"
    if (grepl("yds", s, ignore.case = TRUE)) {
      m <- regmatches(s, regexpr("[0-9]+(?:\\.[0-9]+)?", s, perl = TRUE))
      if (length(m) && nzchar(m)) {
        out[i] <- suppressWarnings(as.numeric(m))
        next
      }
    }
    # Inches only: "13 in"
    if (grepl("^[0-9]+\\s*in", s, ignore.case = TRUE)) {
      m <- regmatches(s, regexpr("^[0-9]+(?:\\.[0-9]+)?", s, perl = TRUE))
      if (length(m) && nzchar(m)) {
        out[i] <- suppressWarnings(as.numeric(m)) / 36
        next
      }
    }
    # Feet (optional inches): "24 ft 11 in." "10 ft 2 in." "2 ft 7 in."
    ft <- regmatches(s, regexpr("([0-9]+)\\s*ft", s, ignore.case = TRUE))
    inch <- rep(0, length(s))
    if (grepl("in\\.?", s, ignore.case = TRUE)) {
      im <- regmatches(s, regexpr("([0-9]+)\\s*in", s, ignore.case = TRUE))
      if (length(im) && nzchar(im)) {
        inch[i] <- suppressWarnings(as.numeric(regmatches(im, regexpr("[0-9]+", im))))
      }
    }
    if (length(ft) && nzchar(ft)) {
      f <- suppressWarnings(as.numeric(regmatches(ft, regexpr("[0-9]+", ft))))
      out[i] <- (f + inch[i] / 12) / 3
      next
    }
    # Fallback: first number assumed yards if small
    m <- suppressWarnings(as.numeric(regmatches(s, regexpr("[0-9]+(?:\\.[0-9]+)?", s))))
    if (is.finite(m) && m < 800) out[i] <- m / if (m > 100) 1 else 3
  }
  pmax(0, out)
}

#' Coarse bins for distance to hole (yards). Putting (< ~35 yd) uses finer bins in feet-equivalent.
dist_bin <- function(yards, from_code) {
  fc <- toupper(trimws(as.character(from_code)))
  y <- suppressWarnings(as.numeric(yards))
  y[!is.finite(y)] <- NA_real_
  is_green <- !is.na(fc) & fc == "OGR"
  bin <- rep(NA_character_, length(y))
  # Green: use yards to hole (putts)
  okg <- is_green & is.finite(y)
  bin[okg & y <= 0.01] <- "g_in_hole"
  bin[okg & y > 0.01 & y <= 5 / 36] <- "g_0_5ft"
  bin[okg & y > 5 / 36 & y <= 10 / 36] <- "g_5_10ft"
  bin[okg & y > 10 / 36 & y <= 20 / 36] <- "g_10_20ft"
  bin[okg & y > 20 / 36 & y <= 40 / 36] <- "g_20_40ft"
  bin[okg & y > 40 / 36] <- "g_40ft_plus"
  # Off green
  oko <- !is_green & is.finite(y)
  bin[oko & y <= 0.01] <- "x_in_hole"
  bin[oko & y > 0.01 & y < 50] <- "x_0_50"
  bin[oko & y >= 50 & y < 100] <- "x_50_100"
  bin[oko & y >= 100 & y < 150] <- "x_100_150"
  bin[oko & y >= 150 & y < 200] <- "x_150_200"
  bin[oko & y >= 200 & y < 250] <- "x_200_250"
  bin[oko & y >= 250] <- "x_250_plus"
  bin
}

state_key <- function(from_code, yards_to_hole, par) {
  fc <- toupper(trimws(as.character(from_code)))
  fc[is.na(fc) | !nzchar(fc)] <- "UNK"
  db <- dist_bin(yards_to_hole, fc)
  p <- suppressWarnings(as.integer(par))
  p[!is.finite(p) | p < 2L | p > 6L] <- NA_integer_
  paste(fc, db, paste0("p", p), sep = "|")
}

#' Augment raw shots table (one row per stroke) with dist_before_yds and state_before.
augment_shots <- function(d) {
  req <- c("hole_number", "par", "yardage", "stroke_number", "from_location_code", "to_location_code",
           "distance_remaining", "final_stroke", "player_id")
  miss <- setdiff(req, names(d))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  d <- d[order(d$tournament_id %||% "", d$player_id %||% "", d$round %||% 1L,
               d$hole_number, d$stroke_number), , drop = FALSE]
  d$dist_after_yds <- parse_to_yards(d$distance_remaining)
  d$dist_before_yds <- NA_real_
  is_first <- d$stroke_number == 1L
  d$dist_before_yds[is_first] <- suppressWarnings(as.numeric(d$yardage[is_first]))
  idx <- which(!is_first)
  if (length(idx)) {
    prev <- idx - 1L
    same_hole <- (d$hole_number[idx] == d$hole_number[prev]) &
      (d$player_id[idx] == d$player_id[prev]) &
      (d$round[idx] == d$round[prev]) &
      (d$tournament_id[idx] == d$tournament_id[prev])
    d$dist_before_yds[idx[same_hole]] <- d$dist_after_yds[prev[same_hole]]
  }
  d$state_before <- state_key(d$from_location_code, d$dist_before_yds, d$par)
  d$to_code <- toupper(trimws(as.character(d$to_location_code)))
  d$to_code[is.na(d$to_code)] <- ""
  d
}

#' Build outcome rows for transition sampling (one row per shot, keyed by state_before).
outcome_row <- function(d) {
  fs <- d$final_stroke
  if (is.character(fs)) fs <- toupper(trimws(fs)) %in% c("TRUE", "T", "1", "YES")
  data.frame(
    state_before = d$state_before,
    player_id = as.character(d$player_id),
    dist_after_yds = d$dist_after_yds,
    to_code = toupper(trimws(as.character(d$to_code))),
    final_stroke = as.logical(fs),
    from_code = toupper(trimws(as.character(d$from_location_code))),
    stringsAsFactors = FALSE
  )
}

#' Fit empirical shot transition tables (tour-wide and by player).
#' @param shots data.frame from augment_shots(outcome_row is internal — pass augmented df)
#' @param min_player_shots Minimum shots to keep player-specific bucket (else tour only)
#' @return list with tour (named list of data.frames per state), by_player (player_id -> list)
fit_shot_transition_tables <- function(shots_augmented, min_player_shots = 30L) {
  o <- outcome_row(shots_augmented)
  o <- o[is.finite(o$dist_after_yds) | o$final_stroke, , drop = FALSE]
  states <- unique(o$state_before)
  tour <- vector("list", length(states))
  names(tour) <- states
  for (s in states) {
    tour[[s]] <- o[o$state_before == s, , drop = FALSE]
  }
  by_player <- list()
  for (pid in unique(as.character(o$player_id))) {
    sub <- o[o$player_id == pid, , drop = FALSE]
    if (nrow(sub) < min_player_shots) next
    st <- unique(sub$state_before)
    pl <- vector("list", length(st))
    names(pl) <- st
    for (s in st) pl[[s]] <- sub[sub$state_before == s, , drop = FALSE]
    by_player[[pid]] <- pl
  }
  list(tour = tour, by_player = by_player, meta = list(version = 1L))
}

#' Find transition rows for a state; fallback to same lie (from_code) + any distance bin.
pick_transition_df <- function(state, player_id, fit, player_weight = 0.7) {
  pid <- as.character(player_id)
  df_t <- fit$tour[[state]]
  df_p <- if (!is.null(fit$by_player[[pid]])) fit$by_player[[pid]][[state]] else NULL
  use_p <- !is.null(df_p) && nrow(df_p) >= 5L
  if (use_p && runif(1) < player_weight && nrow(df_p) >= 1L) {
    return(df_p)
  }
  if (!is.null(df_t) && nrow(df_t) >= 1L) return(df_t)
  parts <- strsplit(state, "|", fixed = TRUE)[[1]]
  fc <- parts[1]
  cand <- names(fit$tour)[startsWith(names(fit$tour), paste0(fc, "|"))]
  for (st in cand) {
    d <- fit$tour[[st]]
    if (!is.null(d) && nrow(d) >= 1L) return(d)
  }
  NULL
}

#' Match PGA `to_location` text "Fairway" using empirical `to_location_code` (stored as to_code).
#' Raw shots use codes like ELF/ERF (left/right fairway), OFW (fairway lie); also match substring FAIR.
to_code_indicates_fairway <- function(to_code) {
  tc <- toupper(trimws(as.character(to_code)))
  if (!nzchar(tc) || tc == "HOLE") return(FALSE)
  if (grepl("FAIR", tc, fixed = TRUE)) return(TRUE)
  tc %in% c("ELF", "ERF", "OFW", "ECF", "XFW")
}

#' Match `to_location` "Green" — in shot data this is `to_location_code` == OGR (on green).
to_code_indicates_green <- function(to_code) {
  tc <- toupper(trimws(as.character(to_code)))
  if (!nzchar(tc)) return(FALSE)
  if (tc == "OGR") return(TRUE)
  grepl("GREEN", tc, fixed = TRUE)
}

#' Fairway hit (par 4/5): first shot lands on fairway. GIR: par 3 stroke 1, par 4 stroke 2, par 5 stroke 3 on green.
#' Vectorized over 18 holes via \code{simulate_round_projection_metrics_one} — \code{par} is that hole's par from the course layout.
hole_gir_fairway_from_to_codes <- function(par, to_codes) {
  par <- as.integer(par)[1]
  tc <- to_codes
  if (length(tc) == 0L) {
    return(list(fairway_hit = NA_integer_, gir = 0L))
  }
  fh <- NA_integer_
  if (par %in% c(4L, 5L) && length(tc) >= 1L) {
    fh <- if (to_code_indicates_fairway(tc[1])) 1L else 0L
  }
  gir <- 0L
  req <- if (par == 3L) 1L else if (par == 4L) 2L else if (par == 5L) 3L else NA_integer_
  if (!is.na(req) && length(tc) >= req) {
    gir <- if (to_code_indicates_green(tc[req])) 1L else 0L
  }
  list(fairway_hit = fh, gir = gir)
}

#' Simulate one hole; returns stroke total and vector of `to_code` per stroke (from transition model).
simulate_hole_shot_level_path <- function(par, yardage, player_id, fit,
                                          max_strokes = 18L, player_weight = 0.7) {
  par <- as.integer(par)[1]
  yardage <- suppressWarnings(as.numeric(yardage))
  if (!is.finite(yardage) || yardage <= 0) yardage <- 400
  strokes <- 0L
  from <- "OTB"
  dist <- yardage
  to_codes <- character()
  repeat {
    if (strokes >= max_strokes) {
      return(list(strokes = max_strokes, to_codes = to_codes))
    }
    st <- state_key(from, dist, par)
    df <- pick_transition_df(st, player_id, fit, player_weight = player_weight)
    if (is.null(df) || nrow(df) < 1L) {
      strokes <- strokes + max(1L, par - 1L)
      return(list(strokes = min(strokes, max_strokes), to_codes = to_codes))
    }
    j <- sample.int(nrow(df), 1L)
    r <- df[j, , drop = FALSE]
    strokes <- strokes + 1L
    tc <- toupper(trimws(as.character(r$to_code)))
    if (is.na(tc)) tc <- ""
    to_codes <- c(to_codes, tc)
    if (isTRUE(r$final_stroke) || tc == "HOLE") {
      return(list(strokes = strokes, to_codes = to_codes))
    }
    from <- tc
    if (!nzchar(from) || from == "HOLE") return(list(strokes = strokes, to_codes = to_codes))
    dist <- suppressWarnings(as.numeric(r$dist_after_yds))
    if (!is.finite(dist)) dist <- yardage * 0.3
  }
}

#' Simulate strokes for one hole (shot-level resampling until holed).
simulate_hole_shot_level <- function(par, yardage, player_id, fit,
                                     max_strokes = 18L, player_weight = 0.7) {
  simulate_hole_shot_level_path(par, yardage, player_id, fit, max_strokes, player_weight)$strokes
}

#' Map UI lie to PGA `from_location_code` used in shot_transition_model.
lie_code_from_ui <- function(lie) {
  x <- toupper(trimws(as.character(lie)))
  if (x %in% c("GREEN", "OGR")) return("OGR")
  if (x %in% c("SAND", "BUNKER")) return("OST")
  if (x == "ROUGH") return("ORF")
  "OFW"
}

#' Simulate remaining strokes from a mid-hole state (empirical shot transitions only).
#' @param dist_yds Distance to hole in yards (putting: use feet/3).
#' @param strokes_so_far Completed strokes on this hole before the next shot.
#' @param yardage Hole yardage (fallback if distance missing).
simulate_hole_shot_level_from_state <- function(par, yardage, from_code, dist_yds, strokes_so_far,
                                              player_id, fit,
                                              max_strokes = 18L, player_weight = 0.65) {
  par <- as.integer(par)[1]
  yardage <- suppressWarnings(as.numeric(yardage))
  if (!is.finite(yardage) || yardage <= 0) yardage <- 400
  strokes <- as.integer(strokes_so_far)
  if (!is.finite(strokes) || strokes < 0L) strokes <- 0L
  from <- toupper(trimws(as.character(from_code)))
  if (!nzchar(from)) from <- "OFW"
  dist <- suppressWarnings(as.numeric(dist_yds))
  if (!is.finite(dist) || dist < 0) dist <- yardage * 0.5
  to_codes <- character()
  repeat {
    if (strokes >= max_strokes) {
      return(min(strokes, max_strokes))
    }
    st <- state_key(from, dist, par)
    df <- pick_transition_df(st, player_id, fit, player_weight = player_weight)
    if (is.null(df) || nrow(df) < 1L) {
      strokes <- strokes + max(1L, par - 1L)
      return(min(strokes, max_strokes))
    }
    j <- sample.int(nrow(df), 1L)
    r <- df[j, , drop = FALSE]
    strokes <- strokes + 1L
    tc <- toupper(trimws(as.character(r$to_code)))
    if (is.na(tc)) tc <- ""
    to_codes <- c(to_codes, tc)
    if (isTRUE(r$final_stroke) || tc == "HOLE") {
      return(strokes)
    }
    from <- tc
    if (!nzchar(from) || from == "HOLE") return(strokes)
    dist <- suppressWarnings(as.numeric(r$dist_after_yds))
    if (!is.finite(dist)) dist <- yardage * 0.3
  }
}

#' Monte Carlo distribution of score-to-par buckets for one hole from a live state (Hole Hangout).
#' Returns named numeric vector probs: Eagle, Birdie, Par, Bogey, Double+
hole_outcome_probs_shot_mc <- function(par, yardage, lie_ui, dist_yds, putt_ft, shot_num,
                                       player_id, fit, n_sims = 400L, player_weight = 0.65,
                                       seed = NULL) {
  if (!is.null(seed)) set.seed(as.integer(seed))
  par <- as.integer(par)[1]
  if (!is.finite(par) || par < 3L || par > 5L) par <- 4L
  lie_ui <- as.character(lie_ui %||% "Fairway")
  fc <- lie_code_from_ui(lie_ui)
  strokes_taken <- max(0L, as.integer(shot_num) - 1L)
  if (!is.finite(strokes_taken) || strokes_taken < 0L) strokes_taken <- 0L
  if (identical(fc, "OGR")) {
    d <- suppressWarnings(as.numeric(putt_ft))
    if (!is.finite(d) || d <= 0) d <- 15
    dist_start <- d / 3
  } else {
    dist_start <- suppressWarnings(as.numeric(dist_yds))
    if (!is.finite(dist_start) || dist_start < 0) dist_start <- 150
  }
  e <- b <- pa <- bo <- db <- 0L
  for (i in seq_len(n_sims)) {
    st <- simulate_hole_shot_level_from_state(
      par, yardage, fc, dist_start, strokes_taken, player_id, fit,
      max_strokes = 18L, player_weight = player_weight
    )
    dff <- as.integer(st) - as.integer(par)
    if (dff <= -2L) {
      e <- e + 1L
    } else if (dff == -1L) {
      b <- b + 1L
    } else if (dff == 0L) {
      pa <- pa + 1L
    } else if (dff == 1L) {
      bo <- bo + 1L
    } else {
      db <- db + 1L
    }
  }
  tot <- e + b + pa + bo + db
  if (tot <= 0L) return(NULL)
  c(
    Eagle = e / tot, Birdie = b / tot, Par = pa / tot, Bogey = bo / tot, `Double+` = db / tot
  )
}

#' Simulate a full 18-hole round (course = vector of par, yardage per hole).
simulate_round_shot_level <- function(player_id, holes_par, holes_yardage, fit,
                                    player_weight = 0.7, max_strokes_hole = 18L) {
  if (length(holes_par) != 18L || length(holes_yardage) != 18L) {
    stop("holes_par and holes_yardage must have length 18.")
  }
  total <- 0
  for (h in 1:18) {
    total <- total + simulate_hole_shot_level(
      holes_par[h], holes_yardage[h], player_id, fit,
      max_strokes = max_strokes_hole, player_weight = player_weight
    )
  }
  total
}

#' Count eagles / birdies / pars / bogeys / double+ from one shot-resampled round (18 holes).
#' Uses only empirical transitions in `fit` — no analytic score priors.
simulate_round_outcome_counts <- function(player_id, holes_par, holes_yardage, fit,
                                          player_weight = 0.65, max_strokes_hole = 18L) {
  if (length(holes_par) != 18L || length(holes_yardage) != 18L) {
    stop("holes_par and holes_yardage must have length 18.")
  }
  e <- b <- pa <- bo <- db <- 0L
  for (h in 1:18) {
    st <- simulate_hole_shot_level(
      holes_par[h], holes_yardage[h], player_id, fit,
      max_strokes = max_strokes_hole, player_weight = player_weight
    )
    rel <- as.integer(st) - as.integer(holes_par[h])
    if (rel <= -2L) {
      e <- e + 1L
    } else if (rel == -1L) {
      b <- b + 1L
    } else if (rel == 0L) {
      pa <- pa + 1L
    } else if (rel == 1L) {
      bo <- bo + 1L
    } else {
      db <- db + 1L
    }
  }
  c(eagles = e, birdies = b, pars = pa, bogeys = bo, doubles_plus = db)
}

#' Monte Carlo mean counts per 18 holes (for projections). No default score priors.
mc_mean_round_outcome_counts <- function(player_id, holes_par, holes_yardage, fit,
                                         n_sims = 25L, player_weight = 0.65, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_sims <- as.integer(max(5L, min(80L, n_sims)))
  m <- matrix(0, nrow = 5L, ncol = n_sims)
  for (j in seq_len(n_sims)) {
    m[, j] <- simulate_round_outcome_counts(
      player_id, holes_par, holes_yardage, fit,
      player_weight = player_weight
    )
  }
  v <- rowMeans(m)
  names(v) <- c("eagles", "birdies", "pars", "bogeys", "doubles_plus")
  v
}

#' One MC replicate: score-type counts + GIR + fairways from the same simulated round (efficient).
simulate_round_projection_metrics_one <- function(player_id, holes_par, holes_yardage, fit,
                                                  player_weight = 0.65, max_strokes_hole = 18L) {
  if (length(holes_par) != 18L || length(holes_yardage) != 18L) {
    stop("holes_par and holes_yardage must have length 18.")
  }
  e <- b <- pa <- bo <- db <- 0L
  n_gir <- 0L
  n_fw <- 0L
  for (h in 1:18) {
    path <- simulate_hole_shot_level_path(
      holes_par[h], holes_yardage[h], player_id, fit,
      max_strokes = max_strokes_hole, player_weight = player_weight
    )
    rel <- as.integer(path$strokes) - as.integer(holes_par[h])
    if (rel <= -2L) {
      e <- e + 1L
    } else if (rel == -1L) {
      b <- b + 1L
    } else if (rel == 0L) {
      pa <- pa + 1L
    } else if (rel == 1L) {
      bo <- bo + 1L
    } else {
      db <- db + 1L
    }
    m <- hole_gir_fairway_from_to_codes(holes_par[h], path$to_codes)
    n_gir <- n_gir + as.integer(m$gir)
    if (!is.na(m$fairway_hit)) n_fw <- n_fw + as.integer(m$fairway_hit)
  }
  c(
    eagles = e, birdies = b, pars = pa, bogeys = bo, doubles_plus = db,
    gir = n_gir, fairways = n_fw
  )
}

#' Monte Carlo means: eagles…doubles+, GIR count (0–18), fairways hit (0–n par 4/5).
mc_mean_round_projection_metrics <- function(player_id, holes_par, holes_yardage, fit,
                                              n_sims = 12L, player_weight = 0.65, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_sims <- as.integer(max(5L, min(80L, n_sims)))
  m <- matrix(NA_real_, nrow = 7L, ncol = n_sims)
  for (j in seq_len(n_sims)) {
    m[, j] <- simulate_round_projection_metrics_one(
      player_id, holes_par, holes_yardage, fit,
      player_weight = player_weight
    )
  }
  v <- rowMeans(m, na.rm = TRUE)
  names(v) <- c("eagles", "birdies", "pars", "bogeys", "doubles_plus", "gir", "fairways")
  v
}

#' Monte Carlo tournament: many simulations of 72-hole totals for a vector of player_ids.
#' Uses the same 18-hole course template (par/yardage) repeated for 4 rounds, iid shot resampling.
shot_level_mc_tournament <- function(
  player_ids,
  holes_par,
  holes_yardage,
  fit,
  n_sims = 500L,
  player_weight = 0.7,
  seed = 42L
) {
  set.seed(seed)
  player_ids <- unique(as.character(player_ids))
  n_p <- length(player_ids)
  if (n_p < 1L) stop("No players.")
  totals <- matrix(NA_real_, nrow = n_sims, ncol = n_p)
  colnames(totals) <- player_ids
  for (s in seq_len(n_sims)) {
    for (j in seq_len(n_p)) {
      t72 <- 0
      for (r in 1:4) {
        t72 <- t72 + simulate_round_shot_level(
          player_ids[j], holes_par, holes_yardage, fit,
          player_weight = player_weight
        )
      }
      totals[s, j] <- t72
    }
  }
  # apply(..., 1) with vector FUN returns n_players x n_sims — average over sims with rowMeans.
  rk_win <- if (n_p >= 2L) {
    apply(totals, 1, function(row) rank(row, ties.method = "random") == 1L)
  } else {
    matrix(TRUE, nrow = 1L, ncol = n_sims)
  }
  rk5 <- if (n_p >= 2L) {
    apply(totals, 1, function(row) rank(row, ties.method = "random") <= 5L)
  } else {
    matrix(TRUE, nrow = 1L, ncol = n_sims)
  }
  list(
    totals = totals,
    win_prob = as.numeric(rowMeans(rk_win)),
    top5 = as.numeric(rowMeans(rk5))
  )
}

#' Calibrated shot-level tournament MC for projection pipeline (replaces Gaussian round draws).
#'
#' Empirical shot transitions supply **round-score noise** (tails, player-specific patterns).
#' DataGolf-style **mean skill** is enforced per round via `mu_target = baseline + avg_stp - mu_sg*mu_mult[r] - form_shock[p]`
#' and `R_final = mu_target + (R_shot - tour_mu_round)`, where `tour_mu_round` is the mean of tour-only shot sim
#' (player_weight = 0) so uncentered shot noise does not bias the field mean.
#'
#' @param mu_sg Numeric vector, field-relative SG (one per player, round-1 reference).
#' @param pga_player_ids Character vector of pgatouR `player_id`; use NA or "" for tour-only shot paths (player_weight forced to 0).
#' @param cut_line Players at or better than this rank count as "make cut" (same as round_projections default).
#' @return Named list: sim_win, sim_top_5, sim_top_10, sim_top_20, sim_make_cut (each length n_players).
shot_level_mc_tournament_calibrated <- function(
  mu_sg,
  pga_player_ids,
  holes_par,
  holes_yardage,
  fit,
  baseline,
  avg_stp,
  mu_mult = c(1, 0.99, 0.97, 0.95),
  form_sd = 0.25,
  n_sims = 400L,
  player_weight = 0.65,
  tour_mu_round = NULL,
  n_tour_calib = 80L,
  seed = 42L,
  cut_line = NULL,
  max_strokes_hole = 18L
) {
  set.seed(seed)
  mu_sg <- as.numeric(mu_sg)
  n_players <- length(mu_sg)
  if (length(pga_player_ids) != n_players) {
    stop("pga_player_ids must have same length as mu_sg.")
  }
  if (length(holes_par) != 18L || length(holes_yardage) != 18L) {
    stop("holes_par and holes_yardage must have length 18.")
  }
  if (length(mu_mult) != 4L) {
    stop("mu_mult must have length 4 (R1–R4).")
  }
  holes_par <- as.integer(holes_par)
  holes_yardage <- as.integer(round(as.numeric(holes_yardage)))

  if (is.null(tour_mu_round) || !is.finite(tour_mu_round)) {
    # Tour-only baseline mean strokes per round (any pid; player_weight = 0 uses tour tables)
    calib <- replicate(
      n_tour_calib,
      simulate_round_shot_level(
        "0", holes_par, holes_yardage, fit,
        player_weight = 0, max_strokes_hole = max_strokes_hole
      )
    )
    tour_mu_round <- mean(calib, na.rm = TRUE)
    if (!is.finite(tour_mu_round) || tour_mu_round <= 0) {
      stop("Could not calibrate tour_mu_round from shot model.")
    }
  }

  if (is.null(cut_line)) {
    cut_line <- min(65L, n_players)
  } else {
    cut_line <- as.integer(cut_line)
  }

  wins <- integer(n_players)
  t5 <- integer(n_players)
  t10 <- integer(n_players)
  t20 <- integer(n_players)
  mc <- integer(n_players)

  for (iter in seq_len(n_sims)) {
    form_shock <- stats::rnorm(n_players, mean = 0, sd = form_sd)
    total_72 <- numeric(n_players)
    for (p in seq_len(n_players)) {
      pid <- pga_player_ids[p]
      pw <- if (is.na(pid) || !nzchar(as.character(pid))) 0 else player_weight
      if (is.na(pid) || !nzchar(as.character(pid))) pid <- "tour"
      tot <- 0
      for (r in 1:4) {
        R_shot <- simulate_round_shot_level(
          pid, holes_par, holes_yardage, fit,
          player_weight = pw, max_strokes_hole = max_strokes_hole
        )
        mu_target <- as.numeric(baseline) + as.numeric(avg_stp) -
          mu_sg[p] * mu_mult[r] - form_shock[p]
        R_final <- mu_target + (R_shot - tour_mu_round)
        tot <- tot + R_final
      }
      total_72[p] <- tot
    }
    rk <- rank(total_72, ties.method = "random")
    wins <- wins + (rk == 1L)
    t5 <- t5 + (rk <= 5L)
    t10 <- t10 + (rk <= 10L)
    t20 <- t20 + (rk <= 20L)
    mc <- mc + (rk <= cut_line)
  }

  cap <- function(x) pmin(0.95, pmax(0.001, x / n_sims))
  list(
    sim_win = cap(wins),
    sim_top_5 = cap(t5),
    sim_top_10 = cap(t10),
    sim_top_20 = cap(t20),
    sim_make_cut = cap(mc),
    tour_mu_round = tour_mu_round,
    n_sims = n_sims
  )
}
