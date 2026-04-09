# backtest_outrights.R
# Backtest PnL and ROI by market and sportsbook using the calibrated model
# and historical outcomes from historical_outrights_outcomes.csv.
# Markets: outright winner, top 5, top 10, top 20, missed cut (mc), make cut (make_cut).

library(dplyr)
library(readr)
library(tidyr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Paths
model_dir <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd()
if (!dir.exists(model_dir)) model_dir <- getwd()
data_dir <- file.path(model_dir, "data")

outcomes_path <- file.path(data_dir, "historical_outrights_outcomes.csv")
cal_path <- file.path(data_dir, "outright_calibration.csv")

# American odds -> decimal (vectorized)
american_to_decimal <- function(american) {
  american <- as.numeric(american)
  out <- rep(NA_real_, length(american))
  ok <- is.finite(american)
  pos <- ok & american > 0
  neg <- ok & american <= 0
  out[pos] <- 1 + american[pos] / 100
  out[neg] <- 1 + 100 / abs(american[neg])
  out
}

# Load calibration and lookup calibrated prob for (market, implied_prob)
load_calibration <- function() {
  if (!file.exists(cal_path)) return(NULL)
  read_csv(cal_path, show_col_types = FALSE)
}

# Bucket implied prob like build_historical_calibration.R (so join matches)
bucket_prob <- function(p, n_buckets = 20) {
  p <- pmin(1, pmax(0, p))
  floor(p * n_buckets) / n_buckets
}

# ---- Load data ----
if (!file.exists(outcomes_path)) {
  stop("Not found: ", outcomes_path, ". Run from project root or set GOLF_MODEL_DIR.")
}

cal_df <- load_calibration()
if (is.null(cal_df) || nrow(cal_df) == 0) {
  message("No outright_calibration.csv found. Run build_historical_calibration.R first.")
}

df <- read_csv(outcomes_path, show_col_types = FALSE)

# Normalize column name (CSV may have "book" or "sportsbook")
if ("book" %in% names(df) && !"sportsbook" %in% names(df)) {
  df <- df %>% rename(sportsbook = book)
}

# Restrict to supported markets
MARKETS <- c("win", "top_5", "top_10", "top_20", "make_cut", "mc")
df <- df %>%
  filter(market %in% MARKETS) %>%
  mutate(
    # Use close_odds; fallback to open_odds
    odds_american = coalesce(close_odds, open_odds),
    decimal_odds = american_to_decimal(odds_american),
    implied_prob = if_else(is.finite(decimal_odds) & decimal_odds > 0, 1 / decimal_odds, NA_real_),
    hit = (bet_outcome_numeric == 1),
    void = (bet_outcome_numeric == 0.5)
  ) %>%
  filter(!void, is.finite(implied_prob), implied_prob > 0, implied_prob <= 1)

if (nrow(df) == 0) {
  stop("No valid rows after filtering (voids and invalid odds removed).")
}

# Attach model (calibrated) probability via join (vectorized)
df <- df %>% mutate(prob_bucket = bucket_prob(implied_prob))
if (!is.null(cal_df) && nrow(cal_df) > 0) {
  cal_join <- cal_df %>% select(market, prob_bucket_lo, calibrated_prob) %>% distinct(market, prob_bucket_lo, .keep_all = TRUE)
  df <- df %>%
    left_join(cal_join, by = c("market" = "market", "prob_bucket" = "prob_bucket_lo")) %>%
    rename(model_prob = calibrated_prob)
} else {
  df <- df %>% mutate(model_prob = NA_real_)
}
df <- df %>% select(-prob_bucket)

# Bet when model says +EV: model_prob > implied_prob (1 unit per bet)
# PnL: if hit, profit = decimal_odds - 1; else profit = -1
df <- df %>%
  mutate(
    would_bet = is.finite(model_prob) & model_prob > implied_prob,
    stake = if_else(would_bet, 1, 0),
    pnl = if_else(would_bet, if_else(hit, decimal_odds - 1, -1), 0),
    # Baseline: 1 unit on every outcome (for PnL when no model or no +EV bets)
    pnl_baseline = if_else(hit, decimal_odds - 1, -1)
  )

# ---- Baseline: bet every outcome (always shown so output is never blank) ----
baseline_market_book <- df %>%
  group_by(market, sportsbook) %>%
  summarise(
    n_bets = n(),
    n_hits = sum(hit, na.rm = TRUE),
    total_stake = n(),
    total_pnl = sum(pnl_baseline, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(roi_pct = if_else(total_stake > 0, 100 * total_pnl / total_stake, NA_real_))

baseline_market <- df %>%
  group_by(market) %>%
  summarise(
    n_bets = n(),
    n_hits = sum(hit, na.rm = TRUE),
    total_stake = n(),
    total_pnl = sum(pnl_baseline, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(roi_pct = if_else(total_stake > 0, 100 * total_pnl / total_stake, NA_real_))

baseline_sportsbook <- df %>%
  group_by(sportsbook) %>%
  summarise(
    n_bets = n(),
    n_hits = sum(hit, na.rm = TRUE),
    total_stake = n(),
    total_pnl = sum(pnl_baseline, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(roi_pct = if_else(total_stake > 0, 100 * total_pnl / total_stake, NA_real_))

# ---- Model: only +EV bets ----
by_market_book <- df %>%
  filter(would_bet) %>%
  group_by(market, sportsbook) %>%
  summarise(
    n_bets = n(),
    n_hits = sum(hit, na.rm = TRUE),
    total_stake = sum(stake, na.rm = TRUE),
    total_pnl = sum(pnl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(roi_pct = if_else(total_stake > 0, 100 * total_pnl / total_stake, NA_real_))

by_market <- df %>%
  filter(would_bet) %>%
  group_by(market) %>%
  summarise(
    n_bets = n(),
    n_hits = sum(hit, na.rm = TRUE),
    total_stake = sum(stake, na.rm = TRUE),
    total_pnl = sum(pnl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(roi_pct = if_else(total_stake > 0, 100 * total_pnl / total_stake, NA_real_))

by_sportsbook <- df %>%
  filter(would_bet) %>%
  group_by(sportsbook) %>%
  summarise(
    n_bets = n(),
    n_hits = sum(hit, na.rm = TRUE),
    total_stake = sum(stake, na.rm = TRUE),
    total_pnl = sum(pnl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(roi_pct = if_else(total_stake > 0, 100 * total_pnl / total_stake, NA_real_))

# Market display names
market_label <- function(m) {
  dplyr::recode(m, win = "Outright winner", top_5 = "Top 5", top_10 = "Top 10",
                top_20 = "Top 20", make_cut = "Make cut", mc = "Missed cut", .default = m)
}

# ---- Print and save ----
message("=== Backtest: Historical outright outcomes ===\n")
message("Data: ", nrow(df), " bets (voids removed). Markets: win, top_5, top_10, top_20, make_cut, mc.\n")

message("--- BASELINE: PnL if we bet 1 unit on every outcome (by market, all books) ---")
print(as.data.frame(baseline_market %>% mutate(market = market_label(market))))

message("\n--- BASELINE: By sportsbook (all markets) ---")
print(as.data.frame(baseline_sportsbook))

message("\n--- BASELINE: By market and sportsbook ---")
print(as.data.frame(baseline_market_book %>% mutate(market = market_label(market))))

n_ev_bets <- sum(df$would_bet, na.rm = TRUE)
if (n_ev_bets > 0) {
  message("\n--- MODEL: +EV bets only (model_prob > implied_prob), by market ---")
  print(as.data.frame(by_market %>% mutate(market = market_label(market))))
  message("\n--- MODEL: By sportsbook ---")
  print(as.data.frame(by_sportsbook))
  message("\n--- MODEL: By market and sportsbook ---")
  print(as.data.frame(by_market_book %>% mutate(market = market_label(market))))
} else {
  message("\n--- MODEL: No +EV bets (model_prob > implied_prob). Run build_historical_calibration.R to build calibration, or baseline above is the full history.) ---")
}

# Save summary to CSV
out_dir <- file.path(model_dir, "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write_csv(baseline_market_book, file.path(out_dir, "backtest_outrights_baseline_by_market_book.csv"))
write_csv(baseline_market, file.path(out_dir, "backtest_outrights_baseline_by_market.csv"))
write_csv(baseline_sportsbook, file.path(out_dir, "backtest_outrights_baseline_by_sportsbook.csv"))
if (n_ev_bets > 0) {
  write_csv(by_market_book, file.path(out_dir, "backtest_outrights_by_market_book.csv"))
  write_csv(by_market, file.path(out_dir, "backtest_outrights_by_market.csv"))
  write_csv(by_sportsbook, file.path(out_dir, "backtest_outrights_by_sportsbook.csv"))
}
message("\nWrote: backtest_outrights_baseline_*.csv", if (n_ev_bets > 0) ", backtest_outrights_by_*.csv (model)" else "")

# Optional: full bet-level detail for inspection
detail_path <- file.path(out_dir, "backtest_outrights_detail.csv")
df_export <- df %>%
  select(event_id, event_name, event_completed, year, sportsbook, market, dg_id, player_name,
         odds_american, decimal_odds, implied_prob, model_prob, would_bet, hit, stake, pnl)
write_csv(df_export, detail_path)
message("Wrote: backtest_outrights_detail.csv (all rows, key columns)")
