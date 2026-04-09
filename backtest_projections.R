# Backtest projection accuracy vs actual results in historical_rounds_all.
# Uses projection_from_prior.R as single source of truth so model changes
# (decay, course fit, blend, count derivation) are reflected here automatically.
#
# RULES: Prior data only (row_id < target); predicted counts sum to 18.
#
# Run from golfModel directory: source("backtest_projections.R")

library(dplyr)
library(tidyr)
library(readr)
library(tibble)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Load shared projection logic (projection_from_prior.R; used for backtest only; app uses simulate_upcoming_round_from_live.R)
proj_path <- file.path(getwd(), "projection_from_prior.R")
if (!file.exists(proj_path)) proj_path <- "projection_from_prior.R"
if (!file.exists(proj_path)) stop("projection_from_prior.R not found; backtest requires it for model logic.")
source(proj_path, local = FALSE)
if (exists("projection_constants_used")) {
  message("Projection constants in use: ", paste(names(projection_constants_used), "=", unlist(projection_constants_used), collapse = ", "))
}

# ---- 1) Load data (no API; backtest is self-contained) ----
historical_path <- file.path(getwd(), "historical_rounds_all.csv")
if (!file.exists(historical_path)) historical_path <- "historical_rounds_all.csv"
stopifnot(file.exists(historical_path))

hist <- read.csv(historical_path, stringsAsFactors = FALSE)
for (col in c("event_completed", "teetime")) {
  if (col %in% names(hist)) hist[[col]] <- as.character(hist[[col]])
}

course_path <- file.path(getwd(), "data", "course_table.csv")
if (!file.exists(course_path)) course_path <- "data/course_table.csv"
course_data <- if (file.exists(course_path)) read_csv(course_path, show_col_types = FALSE) else tibble(course = character(), par = integer())

# Ensure required columns exist
required <- c("dg_id", "player_name", "course_name", "course_par", "round_score", "sg_total", "birdies", "bogies", "eagles_or_better", "doubles_or_worse")
missing <- setdiff(required, names(hist))
if (length(missing) > 0) stop("historical_rounds_all missing columns: ", paste(missing, collapse = ", "))

# Pars per round (actual)
hist <- hist %>%
  mutate(
    pars_actual = 18L - coalesce(eagles_or_better, 0L) - coalesce(birdies, 0L) - coalesce(bogies, 0L) - coalesce(doubles_or_worse, 0L),
    pars_actual = pmax(0L, pmin(18L, pars_actual))
  )

# Global time order and date for time decay (prior data only)
hist <- hist %>%
  mutate(
    .date = suppressWarnings(as.Date(as.character(event_completed), optional = TRUE)),
    .recency = paste(coalesce(event_completed, ""), coalesce(teetime, ""), row_number()),
    row_id = row_number(),
    to_par = as.numeric(round_score) - as.numeric(course_par)
  ) %>%
  arrange(.recency) %>%
  mutate(row_id = row_number())

# Global count coefficient from full history (so every prediction uses data-driven sensitivity)
global_coef_birdies <- 0.35
tryCatch({
  fit_b <- stats::lm(birdies ~ to_par, data = hist)
  if (is.finite(coef(fit_b)[2])) {
    global_coef_birdies <- max(0.2, min(0.5, -as.numeric(coef(fit_b)[2])))
    message("Using data-driven count_coef_birdies = ", round(global_coef_birdies, 3))
  }
}, error = function(e) message("Using default count_coef_birdies = 0.35"))

# ---- 1b) Hole-data + round SG rates for count prediction (optional) ----
name_display_backtest <- function(x) {
  x <- trimws(as.character(x))
  out <- x
  has_comma <- grepl(",", x, fixed = TRUE)
  idx <- which(has_comma)
  if (length(idx) > 0) {
    p <- strsplit(x[idx], ",\\s*")
    out[idx] <- vapply(p, function(parts) {
      if (length(parts) >= 2) paste(trimws(parts[2]), trimws(parts[1])) else trimws(parts[1])
    }, character(1))
  }
  out
}
hole_data_sg_rates <- NULL
hole_data_path <- file.path(getwd(), "data", "hole_data.csv")
if (!file.exists(hole_data_path)) hole_data_path <- "data/hole_data.csv"
if (file.exists(hole_data_path)) {
  tryCatch({
    hole_data <- read_csv(hole_data_path, show_col_types = FALSE)
    if (all(c("player_name", "par", "score_type", "tournament_name", "round") %in% names(hole_data))) {
      u <- toupper(trimws(as.character(hole_data$score_type)))
      hole_data$rel <- dplyr::case_when(
        grepl("EAGLE", u, fixed = TRUE) ~ -2L,
        grepl("BIRDIE", u, fixed = TRUE) ~ -1L,
        grepl("PAR", u, fixed = TRUE) ~ 0L,
        grepl("BOGEY", u, fixed = TRUE) & !grepl("DOUBLE", u, fixed = TRUE) ~ 1L,
        TRUE ~ 2L
      )
      hist_sg <- hist %>%
        filter(is.finite(sg_total)) %>%
        mutate(
          player_key = tolower(trimws(name_display_backtest(player_name))),
          event_key  = tolower(trimws(as.character(event_name)))
        ) %>%
        select(player_key, event_key, round_num, sg_total) %>%
        distinct(player_key, event_key, round_num, .keep_all = TRUE)
      hole_with_sg <- hole_data %>%
        mutate(
          player_key    = tolower(trimws(as.character(player_name))),
          tournament_key = tolower(trimws(as.character(tournament_name))),
          round         = as.integer(round)
        ) %>%
        left_join(hist_sg, by = c("player_key" = "player_key", "tournament_key" = "event_key", "round" = "round_num"))
      n_with_sg <- sum(!is.na(hole_with_sg$sg_total))
      if (n_with_sg >= 500) {
        hole_with_sg <- hole_with_sg %>%
          filter(is.finite(sg_total)) %>%
          mutate(sg_bucket = cut(sg_total, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = 1:5, include.lowest = TRUE))
        hole_data_sg_rates <- hole_with_sg %>%
          group_by(par, sg_bucket, rel) %>%
          summarise(n = n(), .groups = "drop") %>%
          group_by(par, sg_bucket) %>%
          mutate(prob = n / sum(n)) %>%
          ungroup()
        message("Using hole_data + SG for counts (n holes with SG = ", n_with_sg, ")")
      }
    }
  }, error = function(e) message("hole_data SG rates (backtest): ", conditionMessage(e)))
}

# ---- 2) Sample target rounds to backtest (avoid running on every row) ----
set.seed(42)
n_total <- nrow(hist)
n_backtest <- min(1500, max(300, n_total %/% 5))
target_ids <- sort(sample(n_total, n_backtest))

# ---- 3) Build prediction by calling shared model (projection_from_prior.R) ----
build_prediction <- function(target_row, hist_df, count_coef = 0.35, hole_sg_rates = NULL) {
  prior <- hist_df %>% filter(row_id < target_row$row_id)
  out <- predict_round_from_prior(prior, target_row, count_coef_birdies = count_coef, hole_data_sg_rates = hole_sg_rates)
  out %>%
    mutate(
      actual_round_score = as.numeric(target_row$round_score),
      actual_birdies     = as.numeric(target_row$birdies %||% 0),
      actual_pars       = as.integer(target_row$pars_actual),
      actual_bogeys     = as.numeric(target_row$bogies %||% 0),
      actual_doubles    = as.numeric(target_row$doubles_or_worse %||% 0),
      actual_eagles     = as.numeric(target_row$eagles_or_better %||% 0)
    ) %>%
    select(row_id, dg_id, player_name, course_name, n_at_course,
           actual_round_score, pred_round_score,
           actual_birdies, pred_birdies, actual_pars, pred_pars,
           actual_bogeys, pred_bogeys, actual_doubles, pred_doubles,
           actual_eagles, pred_eagles)
}

# Run backtest (this can be slow for large samples)
message("Backtesting ", length(target_ids), " rounds...")
results_list <- vector("list", length(target_ids))
for (i in seq_along(target_ids)) {
  if (i %% 200 == 0) message("  ", i, "/", length(target_ids))
  target_row <- hist %>% filter(row_id == target_ids[i]) %>% slice(1)
  if (nrow(target_row) == 0) next
  results_list[[i]] <- tryCatch(
    build_prediction(target_row, hist, count_coef = global_coef_birdies, hole_sg_rates = hole_data_sg_rates),
    error = function(e) NULL
  )
}
results <- bind_rows(results_list[!sapply(results_list, is.null)])

# ---- 4) Metrics ----
results <- results %>%
  filter(
    is.finite(actual_round_score), is.finite(pred_round_score),
    actual_round_score >= 50, actual_round_score <= 120
  )

n_used <- nrow(results)
if (n_used == 0) stop("No valid rows after filtering. Check historical_rounds_all.")

mae_score   <- mean(abs(results$actual_round_score - results$pred_round_score), na.rm = TRUE)
rmse_score  <- sqrt(mean((results$actual_round_score - results$pred_round_score)^2, na.rm = TRUE))
cor_score   <- cor(results$actual_round_score, results$pred_round_score, use = "pairwise.complete.obs")

mae_birdies <- mean(abs(results$actual_birdies - results$pred_birdies), na.rm = TRUE)
mae_pars    <- mean(abs(results$actual_pars - results$pred_pars), na.rm = TRUE)
mae_bogeys  <- mean(abs(results$actual_bogeys - results$pred_bogeys), na.rm = TRUE)
mae_doubles <- mean(abs(results$actual_doubles - results$pred_doubles), na.rm = TRUE)
mae_eagles  <- mean(abs(results$actual_eagles - results$pred_eagles), na.rm = TRUE)

cor_birdies <- cor(results$actual_birdies, results$pred_birdies, use = "pairwise.complete.obs")
cor_pars    <- cor(results$actual_pars, results$pred_pars, use = "pairwise.complete.obs")
cor_bogeys  <- cor(results$actual_bogeys, results$pred_bogeys, use = "pairwise.complete.obs")

# ---- 5) Stratified metrics (course history + recent) ----
results <- results %>%
  mutate(
    has_course_history = n_at_course >= 5,
    recent = row_id >= quantile(row_id, 0.8, na.rm = TRUE)
  )
mae_by_course <- results %>% group_by(has_course_history) %>%
  summarise(
    n = n(),
    round_score_MAE = mean(abs(actual_round_score - pred_round_score), na.rm = TRUE),
    round_score_cor = cor(actual_round_score, pred_round_score, use = "pairwise.complete.obs"),
    .groups = "drop"
  )
mae_by_recent <- results %>% group_by(recent) %>%
  summarise(
    n = n(),
    round_score_MAE = mean(abs(actual_round_score - pred_round_score), na.rm = TRUE),
    round_score_cor = cor(actual_round_score, pred_round_score, use = "pairwise.complete.obs"),
    .groups = "drop"
  )

# ---- 6) Report ----
cat("\n========== Backtest: projected vs actual (historical_rounds_all) ==========\n")
cat("Rounds evaluated: ", n_used, "\n\n")

cat("Round score (strokes):\n")
cat("  MAE:  ", round(mae_score, 3), "\n")
cat("  RMSE: ", round(rmse_score, 3), "\n")
cat("  Cor:  ", round(cor_score, 3), "\n\n")

cat("Birdies per round:\n")
cat("  MAE: ", round(mae_birdies, 3), "  Cor: ", round(cor_birdies, 3), "\n")
cat("Pars per round:\n")
cat("  MAE: ", round(mae_pars, 3), "    Cor: ", round(cor_pars, 3), "\n")
cat("Bogeys per round:\n")
cat("  MAE: ", round(mae_bogeys, 3), "  Cor: ", round(cor_bogeys, 3), "\n")
cat("Doubles (or worse):\n")
cat("  MAE: ", round(mae_doubles, 3), "\n")
cat("Eagles (or better):\n")
cat("  MAE: ", round(mae_eagles, 3), "\n")

cat("\n--- Stratified by course history (n_at_course >= 5) ---\n")
print(mae_by_course)
cat("\n--- Stratified by recency (top 20% row_id = recent) ---\n")
print(mae_by_recent)

# Optional: save detailed results
out_path <- file.path(getwd(), "backtest_results.csv")
if (dirname(historical_path) != ".") out_path <- file.path(dirname(historical_path), "backtest_results.csv")
tryCatch({
  readr::write_csv(results, out_path)
  message("\nDetailed results written to: ", out_path)
}, error = function(e) message("\nCould not write CSV: ", conditionMessage(e)))

# Summary tibble for programmatic use
backtest_metrics <- tibble(
  metric = c("round_score_MAE", "round_score_RMSE", "round_score_cor",
              "birdies_MAE", "birdies_cor", "pars_MAE", "pars_cor",
              "bogeys_MAE", "bogeys_cor", "doubles_MAE", "eagles_MAE"),
  value  = c(mae_score, rmse_score, cor_score,
             mae_birdies, cor_birdies, mae_pars, cor_pars,
             mae_bogeys, cor_bogeys, mae_doubles, mae_eagles),
  n_rounds = n_used
)
cat("\n")
print(backtest_metrics)
