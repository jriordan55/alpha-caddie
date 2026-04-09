# historical_odds_projections.R
# Loads calibration tables built from historical_outrights_outcomes.csv and
# historical_matchups_outcomes.csv; provides calibrated probabilities for use
# in the app (3-ball model prices, outright projections).

library(dplyr)
library(readr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

data_dir <- file.path(getOption("golf_model_dir", getwd()), "data")
if (!dir.exists(data_dir)) data_dir <- file.path(getwd(), "data")
if (!dir.exists(data_dir)) data_dir <- "data"

# Lazy-load calibration tables (set on first use)
OUTRIGHT_CAL <- NULL
THREEBALL_CAL <- NULL

load_outright_calibration <- function() {
  if (!is.null(OUTRIGHT_CAL)) return(OUTRIGHT_CAL)
  path <- file.path(data_dir, "outright_calibration.csv")
  if (!file.exists(path)) return(NULL)
  OUTRIGHT_CAL <<- read_csv(path, show_col_types = FALSE)
  OUTRIGHT_CAL
}

load_threeball_calibration <- function() {
  if (!is.null(THREEBALL_CAL)) return(THREEBALL_CAL)
  path <- file.path(data_dir, "threeball_calibration.csv")
  if (!file.exists(path)) return(NULL)
  THREEBALL_CAL <<- read_csv(path, show_col_types = FALSE)
  THREEBALL_CAL
}

# Look up calibrated prob for a single implied prob in a table with prob_bucket_lo, prob_bucket_hi, calibrated_prob
lookup_calibrated <- function(implied_prob, cal_df, bucket_lo = "prob_bucket_lo", bucket_hi = "prob_bucket_hi") {
  if (is.null(cal_df) || nrow(cal_df) == 0 || !is.finite(implied_prob) || implied_prob <= 0 || implied_prob > 1)
    return(NA_real_)
  lo <- cal_df[[bucket_lo]]
  hi <- cal_df[[bucket_hi]]
  idx <- which(implied_prob >= lo & implied_prob < hi)
  if (length(idx) == 0) idx <- which.min(abs(implied_prob - (lo + hi) / 2))
  if (length(idx) == 0) return(NA_real_)
  cal_df$calibrated_prob[idx[1]]
}

# Calibrated outright probability for a given market and implied probability (0-1)
calibrated_outright_prob <- function(market, implied_prob) {
  cal <- load_outright_calibration()
  if (is.null(cal)) return(NA_real_)
  cal <- cal %>% filter(.data$market == .env$market)
  lookup_calibrated(implied_prob, cal)
}

# Calibrated 3-ball win probabilities from (decimal) odds for p1, p2, p3.
# Returns list(prob1, prob2, prob3) normalized to sum 1, or NULL if no calibration.
calibrated_threeball_probs <- function(odds_p1_decimal, odds_p2_decimal, odds_p3_decimal) {
  cal <- load_threeball_calibration()
  if (is.null(cal) || nrow(cal) == 0) return(NULL)
  imp1 <- if (is.finite(odds_p1_decimal) && odds_p1_decimal > 0) 1 / odds_p1_decimal else NA_real_
  imp2 <- if (is.finite(odds_p2_decimal) && odds_p2_decimal > 0) 1 / odds_p2_decimal else NA_real_
  imp3 <- if (is.finite(odds_p3_decimal) && odds_p3_decimal > 0) 1 / odds_p3_decimal else NA_real_
  if (is.na(imp1)) imp1 <- 0; if (is.na(imp2)) imp2 <- 0; if (is.na(imp3)) imp3 <- 0
  s <- imp1 + imp2 + imp3
  if (s <= 0) return(NULL)
  imp1 <- imp1 / s; imp2 <- imp2 / s; imp3 <- imp3 / s
  cal1 <- cal %>% filter(position == 1L)
  cal2 <- cal %>% filter(position == 2L)
  cal3 <- cal %>% filter(position == 3L)
  p1 <- lookup_calibrated(imp1, cal1)
  p2 <- lookup_calibrated(imp2, cal2)
  p3 <- lookup_calibrated(imp3, cal3)
  if (is.na(p1)) p1 <- imp1
  if (is.na(p2)) p2 <- imp2
  if (is.na(p3)) p3 <- imp3
  if (!is.finite(p1)) p1 <- 0
  if (!is.finite(p2)) p2 <- 0
  if (!is.finite(p3)) p3 <- 0
  tot <- p1 + p2 + p3
  if (tot <= 0) return(NULL)
  list(prob1 = p1 / tot, prob2 = p2 / tot, prob3 = p3 / tot)
}

# Check if historical calibration is available (for app to decide whether to use it)
historical_calibration_available <- function() {
  file.exists(file.path(data_dir, "threeball_calibration.csv")) ||
    file.exists(file.path(data_dir, "outright_calibration.csv"))
}
