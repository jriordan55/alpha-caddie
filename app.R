# app.R — PGA Round Simulator
# Shiny app: Model O/U, props, +EV, matchups, etc. Data from simulate_upcoming_round_from_live.R
# (live in-play probs → implied skill → one simulated round). No round_summary.

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(readr)
  library(plotly)
  library(httr)
  library(jsonlite)
  library(tidyr)
  library(htmltools)
  library(rlang)
})

# Sportsbook display name -> short badge label (fallback when logo fails)
book_badges <- c(
  "DraftKings" = "DK", "FanDuel" = "FD", "BetMGM" = "MGM", "Caesars" = "CZR",
  "Bet365" = "365", "Bovada" = "BOV", "PointsBet" = "PB", "William Hill" = "WH",
  "Betway" = "BW", "Unibet" = "UB", "SkyBet" = "SKY", "Pinnacle" = "PIN",
  "BetOnline" = "BOL", "DataGolf" = "DG", "Circa" = "CIRC", "BetCRIS" = "BC"
)
# Domain for favicon/logo (Google favicon API); also used for local www/logos/<slug>.png
book_domains <- c(
  "DraftKings" = "draftkings.com", "FanDuel" = "fanduel.com", "BetMGM" = "betmgm.com",
  "Caesars" = "caesars.com", "Bet365" = "bet365.com", "Bovada" = "bovada.lv",
  "PointsBet" = "pointsbet.com", "William Hill" = "williamhill.com", "Betway" = "betway.com",
  "Unibet" = "unibet.com", "SkyBet" = "skybet.com", "Pinnacle" = "pinnacle.com",
  "BetOnline" = "betonline.ag", "DataGolf" = "datagolf.com", "Circa" = "circasports.com",
  "BetCRIS" = "betcris.com"
)
book_badge <- function(display_name) {
  if (is.null(display_name) || !nzchar(trimws(display_name))) return(display_name)
  dn <- trimws(display_name)
  out <- book_badges[dn]
  if (length(out) == 0 || is.na(out) || !nzchar(out)) return(dn)
  out
}
# Local logo path (www/logos/<slug>.png); run scripts/download_sportsbook_logos.R to populate
book_logo_slug <- function(display_name) {
  if (is.null(display_name) || !nzchar(trimws(display_name))) return(NULL)
  dn <- trimws(display_name)
  domain <- book_domains[dn]
  if (length(domain) == 0 || is.na(domain) || !nzchar(domain)) return(NULL)
  gsub("[^a-z0-9]+", "_", tolower(domain))
}
book_favicon_url <- function(display_name) {
  if (is.null(display_name) || !nzchar(trimws(display_name))) return(NULL)
  dn <- trimws(display_name)
  domain <- book_domains[dn]
  if (length(domain) == 0 || is.na(domain) || !nzchar(domain)) return(NULL)
  paste0("https://www.google.com/s2/favicons?domain=", domain, "&sz=64")
}
# HTML for sportsbook logo (for matchups "Best book" column): img + badge fallback
book_logo_html <- function(display_name) {
  if (is.null(display_name) || !nzchar(trimws(display_name))) return("—")
  dn <- trimws(display_name)
  badge <- book_badge(dn)
  slug <- book_logo_slug(dn)
  favicon_url <- book_favicon_url(dn)
  if (is.null(slug) && is.null(favicon_url))
    return(as.character(tags$span(class = "book-badge", title = dn, dn)))
  local_src <- if (!is.null(slug)) paste0("logos/", slug, ".png") else NULL
  img_src <- local_src %||% favicon_url
  onerr_js <- if (!is.null(local_src) && !is.null(favicon_url))
    paste0("if(this.getAttribute('data-tried')!=='1'){this.setAttribute('data-tried','1');this.src='", favicon_url, "';}else{this.style.display='none';var s=this.nextElementSibling;if(s)s.style.display='inline-block';}")
  else
    "this.style.display='none';var s=this.nextElementSibling;if(s)s.style.display='inline-block';"
  as.character(tags$span(
    style = "display:inline-flex;align-items:center;justify-content:center;",
    tags$img(src = img_src, alt = dn, title = dn, style = "height:20px;width:auto;max-width:48px;object-fit:contain;vertical-align:middle;", `data-tried` = "0", onerror = onerr_js),
    tags$span(class = "book-badge", style = "display:none;", title = dn, badge)
  ))
}

# Paths — round_projections.R is the single pipeline (field, live in-play, course, decomp, SG counts)
model_dir <- if (nzchar(Sys.getenv("GOLF_MODEL_DIR"))) Sys.getenv("GOLF_MODEL_DIR") else getwd()
datagolf_api_key <- Sys.getenv("DATAGOLF_API_KEY", "")
# Live data: sourced only when pipeline runs (keeps app load fast)
live_data_path <- file.path(model_dir, "live_data.R")
live_update_all_path <- file.path(model_dir, "live_update_all.R")

player_props_path <- file.path(model_dir, "data", "player_props_lines.csv")
player_props_birdies_path <- file.path(model_dir, "data", "player_props_birdies.csv")
player_props_pars_path    <- file.path(model_dir, "data", "player_props_pars.csv")
player_props_bogeys_path  <- file.path(model_dir, "data", "player_props_bogeys.csv")

# Normalize course names so variants map to the same key (vectorized)
normalize_course_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  n <- length(x)
  if (n == 0) return(character(0))
  out <- rep("", n)
  # Specific handling for PGA National Champion variants
  mask_pga <- grepl("pga national", x, fixed = TRUE) & grepl("champion", x, fixed = TRUE)
  out[mask_pga] <- "pga national champion"
  # Generic cleanup for others
  other <- !mask_pga & nzchar(x)
  if (any(other)) {
    y <- x[other]
    y <- gsub("resort", "", y, fixed = TRUE)
    y <- gsub(" course", "", y, fixed = TRUE)
    y <- gsub(" the ", " ", y, fixed = TRUE)
    y <- gsub("\\s+", " ", y)
    out[other] <- trimws(y)
  }
  out
}

# Canonical name key for matching (used at startup and in server)
name_match_key <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- gsub(",", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  words <- strsplit(x, " ", fixed = TRUE)[[1]]
  if (length(words) == 0) return("")
  paste(sort(trimws(words)), collapse = " ")
}
event_name_key <- function(x) {
  x <- trimws(tolower(as.character(x)))
  gsub("\\s+", " ", x)
}

# Canonical historical data paths (live_update_all.R adds 2026 to all three; app and pipeline use these)
historical_rounds_all_path    <- file.path(model_dir, "data", "historical_rounds_all.csv")
historical_outrights_path    <- file.path(model_dir, "data", "historical_outrights_outcomes.csv")
historical_matchups_path     <- file.path(model_dir, "data", "historical_matchups_outcomes.csv")
hole_data_path               <- file.path(model_dir, "data", "hole_data.csv")
hole_data_2026_path          <- file.path(model_dir, "data", "all_2026_holes.csv")
# Limit hole_data rows to reduce memory on shinyapps.io (Hole Hangout tab)
HOLE_DATA_MAX_ROWS           <- 15000L
historical_rounds_all_df <- tibble::tibble()
historical_rounds_all <- historical_rounds_all_df

# Pipeline paths — run when user clicks Refresh (round shown is auto from clock; see ou_display_round_auto)
round_projections_path <- file.path(model_dir, "round_projections.R")
historical_projections_path <- file.path(model_dir, "historical_odds_projections.R")

# Auto round for Model O/U (and filtering static RDS): no user picker.
# Thu/Fri/Sat 9pm America/New_York → advance to R2/R3/R4; Sun 9pm → R1 (next week’s Thursday round).
# Mon–Wed and Thu before 9pm → R1. Sat 9pm–Sun before 9pm → R4 (Sunday round).
# Override timezone with env GOLF_OU_TZ (e.g. America/Los_Angeles).
ou_display_round_auto <- function(now = Sys.time(), tz = NULL) {
  if (is.null(tz) || !nzchar(tz)) tz <- Sys.getenv("GOLF_OU_TZ", "America/New_York")
  now_et <- tryCatch(as.POSIXlt(now, tz = tz), error = function(e) NULL)
  if (is.null(now_et)) return(1L)
  wday <- as.integer(now_et$wday) # 0 = Sunday, 1 = Monday, …, 6 = Saturday
  hour <- as.numeric(now_et$hour) + as.numeric(now_et$min) / 60 + as.numeric(now_et$sec) / 3600
  after_9pm <- is.finite(hour) && hour >= 21
  if (wday == 0L && after_9pm) return(1L)
  if (wday %in% 1L:3L) return(1L)
  if (wday == 4L && !after_9pm) return(1L)
  if (wday == 4L && after_9pm) return(2L)
  if (wday == 5L && !after_9pm) return(2L)
  if (wday == 5L && after_9pm) return(3L)
  if (wday == 6L && !after_9pm) return(3L)
  if (wday == 6L && after_9pm) return(4L)
  if (wday == 0L && !after_9pm) return(4L)
  1L
}

ou_display_round_label <- function(now = Sys.time(), tz = NULL) {
  if (is.null(tz) || !nzchar(tz)) tz <- Sys.getenv("GOLF_OU_TZ", "America/New_York")
  r <- ou_display_round_auto(now, tz)
  lab <- switch(as.character(r),
    "1" = "R1 — next Thursday",
    "2" = "R2 — Friday",
    "3" = "R3 — Saturday",
    "4" = "R4 — Sunday",
    paste0("R", r))
  paste0(lab, " (auto, ", tz, ")")
}

# DataGolf round id match (1–4) for teetimes rows
dg_round_int_equals <- function(rv, rn) {
  ri <- suppressWarnings(as.integer(rv))
  rnn <- suppressWarnings(as.integer(rn))
  is.finite(ri) && is.finite(rnn) && ri == rnn
}

# Pull tee time string for round rn from one field-updates player record (list).
dg_teetime_for_round_from_player <- function(p, rn) {
  if (is.null(p)) return(NA_character_)
  rnn <- suppressWarnings(as.integer(rn))
  if (!is.finite(rnn) || rnn < 1L || rnn > 4L) return(NA_character_)
  teetime_nonempty <- function(s) {
    s <- trimws(as.character(s))
    nzchar(s) && !tolower(s) %in% c("na", "n/a", "--", "none", "null")
  }
  # Flat columns (same naming as DataGolf CSV field-updates)
  for (key in c(
    paste0("r", rnn, "_teetime"),
    paste0("R", rnn, "_teetime"),
    paste0("r", rnn, "_tee_time"),
    paste0("tee_time_r", rnn),
    paste0("teetime_r", rnn)
  )) {
    v <- p[[key]]
    if (length(v) < 1L) next
    s <- if (is.list(v)) as.character(v[[1L]]) else as.character(v)
    if (teetime_nonempty(s)) return(trimws(s))
  }
  teetimes <- p$teetimes %||% p$tee_times %||% p$teeTimes
  if (is.data.frame(teetimes) && nrow(teetimes) > 0) {
    for (j in seq_len(nrow(teetimes))) {
      tt <- as.list(teetimes[j, , drop = FALSE])
      rv <- tt$round_num %||% tt$round %||% tt$Round %||% tt$round_number
      if (dg_round_int_equals(rv, rnn)) {
        s <- trimws(as.character(
          tt$teetime %||% tt$teetime_utc %||% tt$tee_time %||% tt$start_time %||% tt$time %||% ""
        ))
        if (teetime_nonempty(s)) return(s)
      }
    }
    return(NA_character_)
  }
  if (!is.list(teetimes) || length(teetimes) == 0L) return(NA_character_)
  tlist <- teetimes
  if (length(tlist) == 1L && is.list(tlist[[1L]])) {
    sole <- tlist[[1L]]
    if (is.list(sole) && ("round_num" %in% names(sole) || "round" %in% names(sole) || "Round" %in% names(sole))) {
      tlist <- list(sole)
    } else if (is.list(sole) && length(sole) > 0L) {
      ok_all <- vapply(sole, function(z) {
        is.list(z) && (("round_num" %in% names(z)) || ("round" %in% names(z)) || ("Round" %in% names(z)))
      }, logical(1L))
      if (length(ok_all) && all(ok_all)) tlist <- sole
    }
  }
  for (tt in tlist) {
    if (is.null(tt)) next
    if (is.data.frame(tt)) {
      if (nrow(tt) == 0L) next
      tt <- as.list(tt[1L, , drop = FALSE])
    }
    if (!is.list(tt)) next
    rv <- tt$round_num %||% tt$round %||% tt$Round %||% tt$round_number
    if (dg_round_int_equals(rv, rnn)) {
      s <- trimws(as.character(
        tt$teetime %||% tt$teetime_utc %||% tt$tee_time %||% tt$start_time %||% tt$time %||% ""
      ))
      if (teetime_nonempty(s)) return(s)
    }
  }
  NA_character_
}

# Tee sheet posted: keep only players with a tee time for the displayed round.
# Before tee times exist (common Mon–Wed / pre-R1), almost everyone is NA — keep full field so Model O/U is not empty.
filter_field_require_teetime <- function(field, rn = NULL) {
  if (is.null(field) || nrow(field) == 0) return(field)
  if (!"teetime_upcoming" %in% names(field)) return(field)
  tt <- trimws(as.character(field$teetime_upcoming))
  ok <- nzchar(tt) & !is.na(tt) & !tolower(tt) %in% c("na", "n/a", "--", "none", "null")
  n_ok <- sum(ok, na.rm = TRUE)
  min_tt <- 8L
  if (n_ok < min_tt) return(field)
  field[ok, , drop = FALSE]
}

# Display name: "Last, First" -> "First Last"
name_display <- function(x) {
  has_comma <- grepl(",", x, fixed = TRUE)
  out <- x
  out[has_comma] <- vapply(strsplit(x[has_comma], ",\\s*"), function(p) {
    if (length(p) >= 2) paste(trimws(p[2]), trimws(p[1])) else p[1]
  }, character(1))
  out
}

# American odds to decimal (return per unit bet)
american_to_decimal <- function(american) {
  if (is.na(american) || !is.finite(american)) return(NA_real_)
  if (american > 0) 1 + american / 100 else 1 + 100 / abs(american)
}

# Remove vig from two-way decimal odds: return list(fair1, fair2) in decimal
no_vig_fair_decimal <- function(d1, d2) {
  if (any(!is.finite(c(d1, d2))) || d1 <= 0 || d2 <= 0) return(list(fair1 = NA_real_, fair2 = NA_real_))
  imp1 <- 1 / d1
  imp2 <- 1 / d2
  total <- imp1 + imp2
  if (total <= 0) return(list(fair1 = NA_real_, fair2 = NA_real_))
  fair1 <- 1 / (imp1 / total)
  fair2 <- 1 / (imp2 / total)
  list(fair1 = fair1, fair2 = fair2)
}

# Decimal odds to American (for display); vectorized
decimal_to_american <- function(decimal) {
  out <- rep(NA_real_, length(decimal))
  ok <- is.finite(decimal) & decimal > 1
  out[ok & decimal >= 2] <- (decimal[ok & decimal >= 2] - 1) * 100
  out[ok & decimal < 2] <- -100 / (decimal[ok & decimal < 2] - 1)
  out
}

# Format American odds for display (e.g. +150, -110); vectorized
format_american <- function(american) {
  ifelse(is.na(american) | !is.finite(american), "—",
         ifelse(american > 0, paste0("+", round(american, 0)), as.character(round(american, 0))))
}

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a)) b else a

# Break-even American odds from win probability (no vig); vectorized
fair_american_from_prob <- function(p) {
  p <- as.numeric(p)
  out <- rep(NA_real_, length(p))
  ok <- is.finite(p) & p > 1e-8 & p < 1 - 1e-8
  out[ok] <- decimal_to_american(1 / p[ok])
  out
}

# Default two-way overround for Model O/U display (env GOLF_OU_HOLD overrides, e.g. 0.05)
OU_BOOK_HOLD <- suppressWarnings(as.numeric(Sys.getenv("GOLF_OU_HOLD", "0.048")))
if (!is.finite(OU_BOOK_HOLD) || OU_BOOK_HOLD < 0) OU_BOOK_HOLD <- 0.048

# Two-way O/U posted prices: implied probs get overround (hold) so Over/Under are not fair mirrors (+118/-118).
# inv_o = p_o*(1+h), inv_u = p_u*(1+h) => 1/dec_o + 1/dec_u = 1+h. Returns c(american_over, american_under).
juiced_american_ou_pair <- function(p_over, p_under, hold = NULL) {
  po <- suppressWarnings(as.numeric(p_over))
  pu <- suppressWarnings(as.numeric(p_under))
  if (!is.finite(po) || !is.finite(pu)) return(c(NA_real_, NA_real_))
  s <- po + pu
  if (s <= 1e-12) return(c(NA_real_, NA_real_))
  po <- po / s
  pu <- pu / s
  po <- max(1e-8, min(1 - 1e-8, po))
  pu <- max(1e-8, min(1 - 1e-8, pu))
  s2 <- po + pu
  po <- po / s2
  pu <- pu / s2
  h <- hold
  if (is.null(h) || !is.finite(h) || h < 0) h <- OU_BOOK_HOLD
  inv_o <- po * (1 + h)
  inv_u <- pu * (1 + h)
  dec_o <- 1 / inv_o
  dec_u <- 1 / inv_u
  c(
    as.numeric(decimal_to_american(dec_o))[1L],
    as.numeric(decimal_to_american(dec_u))[1L]
  )
}

# --- Model O/U: volatility (NB / wider Normal) + skill tilt from field-relative scoring ----------
# Elite vs field (lower score_to_par => positive skill_z): nudge implied Under up on “good” counting
# stats (birdies, pars, GIR, FW) and implied Over up on bogeys — book-style asymmetry vs plain Poisson.
OU_DISPERSION_BASE    <- 1.1
OU_DISPERSION_LAMBDA  <- 0.045  # extra tail mass when λ is small
OU_DISPERSION_SKILL   <- 0.055 # |skill| → wider own distribution
OU_DISPERSION_VOL     <- 0.075 # high round_sd vs field → wider counts
OU_JUICE_COUNTS       <- 0.165 # logit nudge on P(under) at 50–50, scaled by tanh(skill)
OU_JUICE_TOTAL        <- 0.072
OU_TOTAL_VOL_SCALE    <- 0.11  # inflate Normal SD from field-relative vol_z
# OU_BOOK_HOLD is set above (with juiced_american_ou_pair); override with env GOLF_OU_HOLD

# Logit shift toward Under (positive skill_z) or away (negative); symmetric for two-way renormalization.
apply_ou_skill_juice <- function(p_under, skill_z, juice_scale) {
  p <- pmin(pmax(as.numeric(p_under), 1e-8), 1 - 1e-8)
  e <- tanh(0.42 * skill_z)
  lp <- stats::qlogis(p)
  lp2 <- pmax(pmin(lp + juice_scale * e, 8), -8)
  stats::plogis(lp2)
}

count_dispersion <- function(lambda, skill_z, vol_z) {
  lam <- suppressWarnings(as.numeric(lambda))
  if (!is.finite(lam) || lam < 0) lam <- 0
  vz <- if (is.finite(vol_z)) vol_z else 0
  sz <- if (is.finite(skill_z)) skill_z else 0
  disp <- OU_DISPERSION_BASE + OU_DISPERSION_LAMBDA / sqrt(lam + 0.5) +
    OU_DISPERSION_SKILL * abs(sz) + OU_DISPERSION_VOL * pmax(vz, 0)
  pmin(pmax(disp, 1), 2.5)
}

# NB with Var ≈ μ * dispersion (dispersion >= 1); dispersion 1 => Poisson.
count_p_under <- function(mu, line, dispersion) {
  mu <- suppressWarnings(as.numeric(mu))
  if (!is.finite(mu) || mu < 0) mu <- 0
  if (mu <= 1e-12) return(if (line >= 0L) 1 else 0)
  disp <- max(1, as.numeric(dispersion))
  if (disp <= 1.0001) return(stats::ppois(line, mu))
  var_target <- mu * disp
  var_target <- max(var_target, mu + 1e-8)
  size <- mu^2 / (var_target - mu)
  if (!is.finite(size) || size <= 1e-8) return(stats::ppois(line, mu))
  stats::pnbinom(line, size = size, mu = mu)
}

count_median_dispersed <- function(mu, dispersion) {
  mu <- suppressWarnings(as.numeric(mu))
  if (!is.finite(mu) || mu < 0) return(NA_real_)
  if (mu <= 1e-12) return(0)
  disp <- max(1, as.numeric(dispersion))
  if (disp <= 1.0001) return(as.numeric(stats::qpois(0.5, mu)))
  var_target <- mu * disp
  var_target <- max(var_target, mu + 1e-8)
  size <- mu^2 / (var_target - mu)
  if (!is.finite(size) || size <= 1e-8) return(as.numeric(stats::qpois(0.5, mu)))
  as.numeric(stats::qnbinom(0.5, size = size, mu = mu))
}

# Field-relative z-scores for this event’s projection table (used in Model O/U pricing).
ou_field_skill_vol_z <- function(df) {
  n <- nrow(df)
  skill_z <- rep(0, n)
  vol_z <- rep(0, n)
  stp <- if ("score_to_par" %in% names(df)) suppressWarnings(as.numeric(df$score_to_par)) else rep(NA_real_, n)
  if (any(is.finite(stp))) {
    med <- stats::median(stp[is.finite(stp)])
    sc <- stats::sd(stp[is.finite(stp)])
    if (!is.finite(sc) || sc < 0.25) sc <- 0.25
    skill_z <- ifelse(is.finite(stp), (med - stp) / sc, 0)
  } else if ("total_score" %in% names(df)) {
    ts <- suppressWarnings(as.numeric(df$total_score))
    if (any(is.finite(ts))) {
      med <- stats::median(ts[is.finite(ts)])
      sc <- stats::sd(ts[is.finite(ts)])
      if (!is.finite(sc) || sc < 0.25) sc <- 0.25
      skill_z <- ifelse(is.finite(ts), (med - ts) / sc, 0)
    }
  }
  if ("round_sd" %in% names(df)) {
    rsd <- suppressWarnings(as.numeric(df$round_sd))
    if (any(is.finite(rsd))) {
      ok <- is.finite(rsd)
      med <- stats::median(rsd[ok])
      iqr <- stats::IQR(rsd[ok])
      madv <- stats::mad(rsd[ok], na.rm = TRUE)
      scale_sd <- max(iqr / 1.349, madv, 0.2, na.rm = TRUE)
      if (!is.finite(scale_sd) || scale_sd < 0.2) scale_sd <- 0.2
      vol_z <- ifelse(ok, (rsd - med) / scale_sd, 0)
    }
  }
  list(skill_z = skill_z, vol_z = vol_z)
}

# Integer total line L: Under = P(S <= L), Over = P(S > L); Normal with continuity correction (simple; no skill/vol layers).
model_total_score_ou_html <- function(mu, sigma, line, skill_z = 0, vol_z = 0) {
  if (!is.finite(mu)) return("—")
  sig <- if (is.finite(sigma) && sigma > 0.25) as.numeric(sigma) else 2.75
  sig <- max(sig, 0.35)
  p_under <- stats::pnorm(line + 0.5, mean = mu, sd = sig)
  p_over <- 1 - p_under
  p_over <- max(1e-8, min(1 - 1e-8, p_over))
  p_under <- max(1e-8, min(1 - 1e-8, p_under))
  am <- juiced_american_ou_pair(p_over, p_under)
  oa <- am[1L]
  ua <- am[2L]
  paste0(
    '<div class="ou-prices"><span class="ou-o">O ', format_american(oa),
    '</span><br/><span class="ou-u">U ', format_american(ua), "</span></div>"
  )
}

# Count props: Poisson at projected mean λ (simple; no NB / skill tilt).
model_count_ou_html <- function(lambda, line, skill_z = 0, vol_z = 0) {
  if (!is.finite(lambda)) return("—")
  lam <- pmax(0, as.numeric(lambda))
  k <- max(0L, floor(as.numeric(line)))
  p_under <- stats::ppois(k, lam)
  if (!is.finite(p_under)) return("—")
  p_over <- 1 - p_under
  p_over <- max(1e-8, min(1 - 1e-8, p_over))
  p_under <- max(1e-8, min(1 - 1e-8, p_under))
  am <- juiced_american_ou_pair(p_over, p_under)
  oa <- am[1L]
  ua <- am[2L]
  paste0(
    '<div class="ou-prices"><span class="ou-o">O ', format_american(oa),
    '</span><br/><span class="ou-u">U ', format_american(ua), "</span></div>"
  )
}

# Fixed integer lines per market (model O/U grid)
LB_OU_LINE_RANGES <- list(
  `Total score` = 64L:74L,
  `Birdies` = 0L:6L,
  `Pars` = 5L:15L,
  `Bogeys` = 0L:6L,
  `GIR` = 5L:15L,
  `Fairways hit` = 5L:15L
)

# Format tee time string to "7:00 A.M." style (hour:minute AM/PM, no timezone)
# Handles ISO datetimes (e.g. 2025-03-11T14:06:00) so year is not mistaken for hour
format_teetime_est <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- trimws(as.character(x))
  out <- character(length(x))
  tz_disp <- Sys.getenv("GOLF_TEETIME_TZ", "America/New_York")
  for (i in seq_along(x)) {
    if (is.na(x[i]) || !nzchar(x[i])) { out[i] <- "—"; next }
    s_raw <- x[i]
    s <- tolower(s_raw)
    # DataGolf: full ISO datetime in UTC (…T…Z or … … without TZ) — convert to local display TZ
    if (grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}[ t][0-9]{1,2}:[0-9]{2}", s_raw, ignore.case = TRUE)) {
      xc <- trimws(s_raw)
      xc <- sub("[zZ]$", "", xc)
      xc <- chartr("t", "T", xc)
      pt <- suppressWarnings(as.POSIXct(xc, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS"))
      if (is.na(pt)) pt <- suppressWarnings(as.POSIXct(xc, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S"))
      if (is.na(pt)) pt <- suppressWarnings(as.POSIXct(xc, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
      if (is.na(pt)) pt <- suppressWarnings(as.POSIXct(xc, tz = "UTC"))
      if (!is.na(pt)) {
        hr12 <- as.integer(format(pt, "%I", tz = tz_disp))
        mi <- as.integer(format(pt, "%M", tz = tz_disp))
        ap <- toupper(format(pt, "%p", tz = tz_disp))
        ampm <- if (ap == "AM") "A.M." else if (ap == "PM") "P.M." else if (nzchar(ap)) paste0(ap, ".") else "A.M."
        if (!is.finite(hr12) || hr12 < 1L) hr12 <- 12L
        out[i] <- sprintf("%d:%02d %s", hr12, mi, ampm)
        next
      }
    }
    hr <- NA_integer_
    min <- 0L
    # ISO format: ...T14:06 or T14:06:00 (hour:minute after T)
    if (grepl("t[0-9]{1,2}:[0-9]{2}", s, ignore.case = TRUE)) {
      time_part <- sub("^.*t([0-9]{1,2}:[0-9]{2})([^0-9].*)?$", "\\1", s, ignore.case = TRUE)
      if (nzchar(time_part) && time_part != s) {
        parts <- strsplit(time_part, ":", fixed = TRUE)[[1]]
        hr <- suppressWarnings(as.integer(parts[1]))
        if (length(parts) >= 2) min <- suppressWarnings(as.integer(parts[2]))
        if (is.na(min)) min <- 0L
      }
    }
    # YYYY-MM-DD HH:MM (space, not T) — avoid reading year as hour
    if (is.na(hr) || !is.finite(hr)) {
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}[ t]([0-9]{1,2}):([0-9]{2})", s, ignore.case = TRUE)) {
        time_part <- sub("^[0-9]{4}-[0-9]{2}-[0-9]{2}[ t]([0-9]{1,2}:[0-9]{2}).*$", "\\1", s, ignore.case = TRUE)
        parts <- strsplit(time_part, ":", fixed = TRUE)[[1]]
        hr <- suppressWarnings(as.integer(parts[1]))
        if (length(parts) >= 2) min <- suppressWarnings(as.integer(parts[2]))
        if (is.na(min)) min <- 0L
      }
    }
    if (is.na(hr) || !is.finite(hr)) {
      # Fallback: plain time 7:51am, 8:00 AM, 14:06 (not strings starting with YYYY-)
      if (!grepl("^[0-9]{4}-", s)) {
        hr <- suppressWarnings(as.integer(sub("^([0-9]{1,2})\\D", "\\1", s)))
        min_str <- sub("^[0-9]{1,2}[:.]?\\s*([0-9]{2})", "\\1", s)
        min <- suppressWarnings(as.integer(min_str))
        if (is.na(min)) min <- 0L
      }
    }
    if (is.finite(hr) && (hr > 23L || hr < 0L)) hr <- NA_integer_
    if (is.na(hr) || !is.finite(hr)) { out[i] <- x[i]; next }
    ampm <- "A.M."
    if (grepl("pm", s, fixed = TRUE)) ampm <- "P.M."
    else if (grepl("am", s, fixed = TRUE)) ampm <- "A.M."
    else if (is.finite(hr) && hr >= 12) ampm <- "P.M."
    if (ampm == "P.M." && is.finite(hr) && hr > 12) hr <- hr - 12
    if (ampm == "A.M." && is.finite(hr) && hr == 0) hr <- 12L
    if (is.finite(hr) && hr > 12) hr <- hr - 12
    out[i] <- sprintf("%d:%02d %s", hr, min, ampm)
  }
  out
}

# Country (3-letter code or name from DataGolf) to ISO 3166-1 alpha-2 (for flag image URL)
country_to_iso2 <- function(country) {
  if (is.null(country) || length(country) == 0) return(character(0))
  country <- trimws(toupper(as.character(country)))
  iso2 <- c(
    "USA" = "US", "UNITED STATES" = "US", "UNITED STATES OF AMERICA" = "US",
    "ENG" = "GB", "ENGLAND" = "GB", "SCO" = "GB", "SCOTLAND" = "GB", "WAL" = "GB", "WALES" = "GB", "NIR" = "GB", "NORTHERN IRELAND" = "GB", "GBR" = "GB", "UK" = "GB",
    "IRL" = "IE", "IRELAND" = "IE", "RSA" = "ZA", "SOUTH AFRICA" = "ZA", "AUS" = "AU", "AUSTRALIA" = "AU",
    "KOR" = "KR", "KOREA" = "KR", "SOUTH KOREA" = "KR", "JPN" = "JP", "JAPAN" = "JP",
    "CAN" = "CA", "CANADA" = "CA", "MEX" = "MX", "MEXICO" = "MX", "ARG" = "AR", "ARGENTINA" = "AR",
    "COL" = "CO", "COLOMBIA" = "CO", "CHI" = "CL", "CHILE" = "CL", "VEN" = "VE", "VENEZUELA" = "VE",
    "ESP" = "ES", "SPAIN" = "ES", "SWE" = "SE", "SWEDEN" = "SE", "NOR" = "NO", "NORWAY" = "NO",
    "ITA" = "IT", "ITALY" = "IT", "GER" = "DE", "GERMANY" = "DE", "FRA" = "FR", "FRANCE" = "FR",
    "AUT" = "AT", "AUSTRIA" = "AT", "BEL" = "BE", "BELGIUM" = "BE", "DEN" = "DK", "DENMARK" = "DK",
    "FIN" = "FI", "FINLAND" = "FI", "NED" = "NL", "NETHERLANDS" = "NL", "POR" = "PT", "PORTUGAL" = "PT",
    "SUI" = "CH", "SWITZERLAND" = "CH", "THA" = "TH", "THAILAND" = "TH", "IND" = "IN", "INDIA" = "IN",
    "CHN" = "CN", "CHINA" = "CN", "TPE" = "TW", "TAIWAN" = "TW", "ZIM" = "ZW", "ZIMBABWE" = "ZW",
    "NZL" = "NZ", "NEW ZEALAND" = "NZ", "FIJ" = "FJ", "FIJI" = "FJ", "SAM" = "WS", "SAMOA" = "WS",
    "SRI" = "LK", "SRI LANKA" = "LK", "MAS" = "MY", "MALAYSIA" = "MY", "PHI" = "PH", "PHILIPPINES" = "PH",
    "TRI" = "TT", "TRINIDAD" = "TT", "BAR" = "BB", "BARBADOS" = "BB", "PUR" = "PR", "PUERTO RICO" = "PR"
  )
  out <- character(length(country))
  for (i in seq_along(country)) {
    c2 <- iso2[country[i]]
    if (is.na(c2)) c2 <- if (nchar(country[i]) == 2L) country[i] else ""
    out[i] <- if (nzchar(c2) && nchar(c2) == 2L) tolower(c2) else ""
  }
  out
}

# Return HTML <img> for flag (flagcdn.com); use in table with escape = FALSE so image displays
country_to_flag_img <- function(country, height_px = 12L) {
  iso <- country_to_iso2(country)
  out <- character(length(iso))
  for (i in seq_along(iso)) {
    if (nzchar(iso[i])) {
      url <- paste0("https://flagcdn.com/w40/", iso[i], ".png")
      out[i] <- sprintf('<img src="%s" height="%d" alt="" style="vertical-align:middle"/>', url, height_px)
    } else {
      out[i] <- ""
    }
  }
  out
}

# Safe coercion to numeric (avoids "NAs introduced by coercion" warnings)
safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (length(x) == 0) return(NA_real_)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}
# Safe scalar text check for Shiny inputs (NULL/NA/blank -> FALSE)
has_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  isTRUE(nzchar(trimws(as.character(x[[1]]))))
}

# DraftKings props URLs (for reference; line/odds are loaded from CSV or pull_draftkings_birdie_par_bogey.R)
DRAFTKINGS_ROUND_SCORE_URL <- "https://sportsbook.draftkings.com/leagues/golf/the-genesis-invitational?category=popular&subcategory=round-score"
DRAFTKINGS_BIRDIE_PAR_BOGEY_URL <- "https://sportsbook.draftkings.com/leagues/golf/the-genesis-invitational?category=round&subcategory=birdie-par-bogey"

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      :root {
        --golf-bg: #050608;
        --golf-elevated: #111216;
        --golf-elevated-soft: #191b22;
        --golf-border: #2b2e36;
        --golf-card-border: #343742;
        --golf-green: #00c46b;
        --golf-green-soft: #00a85b;
        --golf-text: #f5f7fb;
        --golf-text-muted: #8b8f9c;
        --golf-danger: #ff4d4f;
      }
      * { box-sizing: border-box; }
      body {
        font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, sans-serif;
        background: #050608;
        color: var(--golf-text);
        margin: 0;
      }
      .app-nav {
        background: #050608;
        padding: 14px 24px;
        box-shadow: 0 12px 30px rgba(0,0,0,0.9);
        border-bottom: 1px solid var(--golf-border);
      }
      .app-nav .nav-title {
        color: #ffffff;
        font-weight: 700;
        font-size: 1.75rem;
        letter-spacing: 0.02em;
        margin: 0 0 4px 0;
        display: block;
      }
      .app-nav .nav-subtitle {
        color: var(--golf-text-muted);
        font-size: 0.8125rem;
        margin: 0;
        font-weight: 400;
      }
      .app-nav .btn-primary {
        background: linear-gradient(135deg, var(--golf-green) 0%, var(--golf-green-soft) 100%);
        border: 1px solid rgba(0,196,107,0.8);
        color: #ffffff;
        font-weight: 600;
        padding: 8px 18px;
        border-radius: 999px;
        font-family: inherit;
        box-shadow: 0 0 0 1px rgba(0,196,107,0.3), 0 10px 30px rgba(0,0,0,0.8);
      }
      .app-nav .btn-primary:hover {
        background: linear-gradient(135deg, var(--golf-green-soft) 0%, var(--golf-green) 100%);
        box-shadow: 0 0 0 1px rgba(0,196,107,0.6), 0 14px 40px rgba(0,0,0,0.9);
        color: #ffffff;
      }
      .app-nav select {
        background: var(--golf-elevated);
        color: var(--golf-text);
        border: 1px solid var(--golf-border);
        border-radius: 999px;
        padding: 6px 14px;
        font-family: inherit;
        font-weight: 500;
      }
      .content-wrapper {
        padding: 28px 32px 40px;
        max-width: 1200px;
        margin: 0 auto;
      }
      .content-wrapper select, .content-wrapper .selectize-input {
        background: var(--golf-elevated-soft) !important;
        color: var(--golf-text) !important;
        border: 1px solid var(--golf-border) !important;
        border-radius: 8px;
      }
      .content-wrapper input[type=\"number\"], .content-wrapper input[type=\"text\"] {
        background: var(--golf-elevated-soft) !important;
        color: var(--golf-text) !important;
        border: 1px solid var(--golf-border) !important;
        border-radius: 8px;
      }
      .section-title {
        font-size: 0.75rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: var(--golf-text-muted);
        margin: 0 0 12px 0;
        font-weight: 600;
      }
      .leaderboard-title {
        font-size: 0.75rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: var(--golf-text-muted);
        margin: 0 0 12px 0;
        font-weight: 600;
      }
      .app-panel, .leaderboard-card, .props-card, .hole-sim-panel {
        background: var(--golf-elevated);
        border-radius: 12px;
        border: 1px solid var(--golf-card-border);
        padding: 18px;
      }
      .leaderboard-card { overflow: hidden; padding: 18px; }
      .props-card { margin-bottom: 16px; }
      .app-panel { min-height: 0; }
      .hole-sim-panel { min-height: 280px; }
      .dataTables_wrapper {
        font-family: 'DM Sans', sans-serif !important;
      }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_info { color: var(--golf-text-muted); font-size: 0.78rem; }
      .dataTables_wrapper .dataTables_length select {
        background: var(--golf-elevated-soft);
        color: var(--golf-text);
        border: 1px solid var(--golf-border);
        border-radius: 6px;
        padding: 4px 8px;
      }
      .dataTables_wrapper .dataTables_filter { color: var(--golf-text-muted); font-size: 0.78rem; }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid var(--golf-border);
        background: var(--golf-elevated-soft);
        color: var(--golf-text);
        border-radius: 6px;
        padding: 6px 12px;
        font-family: inherit;
        margin-left: 8px;
      }
      .ev-cell-positive { background: rgba(0,196,107,0.25) !important; color: #fff !important; font-weight: 600; }
      table.dataTable {
        border-collapse: collapse !important;
        width: 100% !important;
      }
      table.dataTable thead th {
        background: #181a20 !important;
        color: var(--golf-text) !important;
        font-weight: 600 !important;
        font-size: 0.75rem !important;
        text-transform: uppercase !important;
        letter-spacing: 0.06em !important;
        padding: 14px 16px !important;
        border-bottom: 1px solid var(--golf-border) !important;
      }
      table.dataTable thead th.sorting:before,
      table.dataTable thead th.sorting:after,
      table.dataTable thead th.sorting_asc:before,
      table.dataTable thead th.sorting_asc:after,
      table.dataTable thead th.sorting_desc:before,
      table.dataTable thead th.sorting_desc:after {
        opacity: 0.6;
      }
      table.dataTable th.dt-center,
      table.dataTable td.dt-center { text-align: center !important; }
      table.dataTable tbody td {
        padding: 14px 16px !important;
        font-size: 0.9375rem !important;
        border-bottom: 1px solid var(--golf-border) !important;
        color: var(--golf-text) !important;
      }
      table.dataTable tbody tr:nth-child(even) td { background: #0d0e12 !important; }
      table.dataTable tbody tr:nth-child(odd) td { background: #101116 !important; }
      table.dataTable tbody tr:hover td { background: rgba(0,196,107,0.10) !important; }
      table.dataTable tbody tr:last-child td { border-bottom: none !important; }
      .status-text { color: var(--golf-text-muted); font-size: 0.8rem; margin-left: 12px; }
      .text-muted { color: var(--golf-text-muted) !important; }
      .props-card h3, .app-panel h3 {
        font-size: 0.75rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: var(--golf-text-muted);
        margin: 0 0 12px 0;
        font-weight: 600;
      }
      .props-label { font-weight: 600; color: var(--golf-text-muted); margin-bottom: 4px; font-size: 0.8rem; letter-spacing: 0.06em; text-transform: uppercase; }
      .props-input { border: 1px solid var(--golf-border); border-radius: 999px; padding: 8px 14px; font-family: inherit; background: var(--golf-elevated-soft); color: var(--golf-text); }
      .ev-positive { color: var(--golf-green); font-weight: 700; }
      .ev-negative { color: var(--golf-danger); font-weight: 700; }
      .nav-tabs { border-bottom: 2px solid var(--golf-border); margin-bottom: 20px; }
      .nav-tabs .nav-link { color: var(--golf-text-muted); font-weight: 600; background: transparent; border: none; }
      .nav-tabs .nav-link.active { color: var(--golf-green); border: none; border-bottom: 2px solid var(--golf-green); background: transparent; }
      .book-badge { display: inline-block; font-size: 0.7rem; font-weight: 700; letter-spacing: 0.04em; padding: 4px 8px; border-radius: 6px; background: var(--golf-elevated-soft); color: var(--golf-text); border: 1px solid var(--golf-border); white-space: nowrap; }
      .leaderboard-toolbar { display: flex; align-items: center; flex-wrap: wrap; gap: 12px; padding: 12px 18px; background: var(--golf-elevated); border-radius: 12px; border: 1px solid var(--golf-border); margin-bottom: 16px; }
      .leaderboard-toolbar-label { font-size: 0.75rem; font-weight: 600; letter-spacing: 0.06em; text-transform: uppercase; color: var(--golf-text-muted); }
      .leaderboard-toolbar .form-group { margin: 0; }
      .leaderboard-toolbar-status { font-size: 0.8rem; color: var(--golf-text-muted); margin-left: 4px; }
      .golfer-cell { display: inline-flex; align-items: center; gap: 6px; }
      .golfer-cell img { vertical-align: middle; flex-shrink: 0; border-radius: 2px; max-height: 12px; height: auto; }
      .golfer-name { font-weight: 500; }
      .ou-prices { font-size: 0.78rem; line-height: 1.4; text-align: center; }
      .ou-prices .ou-o { color: #ff8a8a; font-weight: 600; }
      .ou-prices .ou-u { color: var(--golf-green); font-weight: 600; }
      .hole-sim-top-bar { display: flex; align-items: center; justify-content: center; gap: 16px; padding: 14px 20px; background: var(--golf-elevated); border-radius: 12px; border: 1px solid var(--golf-border); margin-bottom: 16px; }
      .hole-sim-panel h4, .app-panel h4 { font-size: 0.75rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--golf-text-muted); margin: 0 0 12px 0; }
      .hole-sim-shot { padding: 8px 0; border-bottom: 1px solid var(--golf-border); font-size: 0.9rem; }
      .hole-sim-shot:last-child { border-bottom: none; }
      .hole-sim-outcome-row { display: flex; align-items: center; gap: 10px; margin-bottom: 8px; font-size: 0.9rem; }
      .hole-sim-outcome-bar { flex: 1; height: 8px; background: var(--golf-elevated-soft); border-radius: 4px; overflow: hidden; }
      .hole-sim-outcome-fill { height: 100%; border-radius: 4px; }
      /* Mobile-friendly: touch targets, readable text, safe area */
      @media (max-width: 768px) {
        .app-nav { padding: 12px 14px; }
        .app-nav .nav-title { font-size: 1.25rem; }
        .app-nav .nav-subtitle { font-size: 0.75rem; }
        .app-nav .btn-primary, .app-nav select, .btn { min-height: 44px; padding: 10px 16px; font-size: 1rem; }
        .content-wrapper { padding: 14px 12px 24px; }
        .leaderboard-title { margin-bottom: 10px; }
        .leaderboard-toolbar { padding: 10px 14px; gap: 10px; }
        .leaderboard-card { border-radius: 10px; overflow-x: auto; -webkit-overflow-scrolling: touch; }
        .props-card { padding: 16px; margin-bottom: 14px; }
        .props-card h3 { margin-bottom: 10px; }
        .nav-tabs .nav-link { padding: 12px 14px; font-size: 0.9rem; }
        table.dataTable thead th { padding: 10px 8px !important; font-size: 0.7rem !important; }
        table.dataTable tbody td { padding: 10px 8px !important; font-size: 0.85rem !important; }
        body { -webkit-text-size-adjust: 100%; }
      }
      @media (max-width: 480px) {
        .app-nav .nav-title { font-size: 1.1rem; }
        .content-wrapper { padding: 10px 8px 20px; }
      }
      /* Ensure tables scroll horizontally on small screens */
      .dataTables_wrapper { overflow-x: auto; -webkit-overflow-scrolling: touch; }
      .leaderboard-card .dataTables_wrapper { min-width: 0; }
    "))
  ),
  div(class = "app-nav",
    div(style = "display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 10px;",
      div(style = "min-width: 0; flex: 1 1 200px;",
        span(class = "nav-title", "AlphaCaddie"),
        p(class = "nav-subtitle", "Your Caddie for Smarter Golf Bets")
      )
    )
  ),
  fluidRow(
    column(12, class = "content-wrapper",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Model O/U",
          div(class = "leaderboard-toolbar",
            span(class = "leaderboard-toolbar-label", textOutput("auto_round_et", inline = TRUE)),
            span(class = "leaderboard-toolbar-label", "Market"),
            selectInput(
              "lb_stat",
              NULL,
              choices = c(
                "Total score",
                "Birdies",
                "Pars",
                "Bogeys",
                "GIR",
                "Fairways hit"
              ),
              selected = "Total score",
              width = "200px"
            ),
            actionButton("refresh_projections", "Refresh", class = "btn-primary btn-sm"),
            span(class = "leaderboard-toolbar-status", textOutput("status", inline = TRUE))
          ),
          uiOutput("leaderboard_title"),
          div(class = "leaderboard-card", DTOutput("leaderboard"))
        ),
        tabPanel(
          "Player Props",
          div(class = "props-card",
            h3("Prop inputs"),
            fluidRow(
              column(12, class = "col-sm-6 col-md-3", div(class = "props-label", "Golfer"), uiOutput("golfer_choice")),
              column(12, class = "col-sm-6 col-md-2", div(class = "props-label", "Stat"), selectInput("prop_stat", NULL, choices = c("Total Score", "Pars", "Birdies", "Bogeys"), width = "100%")),
              column(12, class = "col-sm-6 col-md-2", div(class = "props-label", "Line"), numericInput("prop_line", NULL, value = 70.5, min = 0, step = 0.5, width = "100%")),
              column(12, class = "col-sm-6 col-md-2", div(class = "props-label", "Over odds (American)"), numericInput("prop_odds_over", NULL, value = -110, width = "100%")),
              column(12, class = "col-sm-6 col-md-2", div(class = "props-label", "Under odds (American)"), numericInput("prop_odds_under", NULL, value = 110, width = "100%")),
              column(12, class = "col-sm-6 col-md-1", br(), actionButton("refresh_lines", "Refresh lines", class = "btn-default btn-sm", title = "Reload line/odds from CSV"))
            )
          ),
          fluidRow(
            column(12, class = "col-md-4", div(class = "props-card", h3("Expected value (on $100 bet)"), uiOutput("ev_over_under")))
          ),
          div(class = "props-card",
            h3("Probability distribution"),
            plotlyOutput("prop_histogram", height = "340px")
          )
        ),
        tabPanel(
          "+EV Bets",
          h2("+EV Bets", class = "leaderboard-title"),
          div(class = "leaderboard-card", DTOutput("ev_bets_table"))
        ),
        tabPanel(
          "Round Matchups",
          h2("Round Matchups", class = "leaderboard-title"),
          fluidRow(
            column(3, selectInput("matchups_market", "Market", choices = c("Tournament Matchups" = "tournament_matchups", "Round Matchups" = "round_matchups", "3 Balls" = "3_balls"), selected = "round_matchups", width = "100%"))
          ),
          div(class = "leaderboard-card", style = "overflow-x: auto;", DTOutput("round_matchups_table"))
        ),
        tabPanel(
          "Outrights",
          h2("Outrights: Model vs Sportsbooks", class = "leaderboard-title"),
          div(class = "leaderboard-card",
            p("Compare your model prices to current sportsbook odds for each market. Odds are for the loaded tournament field.", class = "text-muted"),
            uiOutput("outrights_event_info"),
            fluidRow(
              column(3, selectInput("outright_market", "Market", choices = c("Win" = "win", "Top 5" = "top_5", "Top 10" = "top_10", "Top 20" = "top_20", "Make Cut" = "make_cut", "Miss Cut" = "mc"), selected = "win", width = "100%")),
              column(2, br(), actionButton("refresh_projections_outrights", "Refresh projections", class = "btn-primary btn-sm", title = "Run pipeline to update model prices for all markets"))
            ),
            div(style = "overflow-x: auto;", DTOutput("outrights_table"))
          )
        ),
        tabPanel(
          "Hole Hangout",
          h2("Hole Hangout", class = "leaderboard-title"),
          p("Score odds (default): historical hole_data + player skill. Live inputs: odds from shot-by-shot empirical transitions only (data/shot_transition_model.rds). Run Simulate for the narrative hole path.", class = "text-muted", style = "margin-bottom: 12px;"),
          uiOutput("hole_hangout_course_display"),
          div(class = "props-card",
            fluidRow(
              column(12, class = "col-md-2", div(class = "props-label", "Hole"), uiOutput("hole_hangout_hole")),
              column(12, class = "col-md-4", div(class = "props-label", "Player"), uiOutput("hole_hangout_player")),
              column(12, class = "col-md-2", br(), actionButton("hole_sim_run", "Simulate hole", class = "btn-primary"))
            )
          ),
          div(class = "props-card", style = "margin-top: 10px;",
            fluidRow(
              column(12, class = "col-md-2",
                div(class = "props-label", 
                    HTML('Live inputs <span title="When on, SCORE ODDS uses Monte Carlo on the shot transition model (PGA shot-by-shot data) from your current lie/distance/shot # — not historical hole averages or the putting chart.">?</span>')),
                checkboxInput("hh_use_live_inputs", NULL, value = FALSE)
              ),
              column(12, class = "col-md-2",
                div(class = "props-label", "Shot #"),
                numericInput("hh_shot_num", NULL, value = 2, min = 1, max = 8, step = 1, width = "100%")
              ),
              column(12, class = "col-md-3",
                div(class = "props-label", "Distance to hole (yds)"),
                numericInput("hh_dist_yds", NULL, value = 150, min = 0, max = 600, step = 1, width = "100%")
              ),
              column(12, class = "col-md-3",
                div(class = "props-label", "Lie"),
                selectInput("hh_lie", NULL, choices = c("Fairway" = "Fairway", "Rough" = "Rough", "Sand" = "Sand", "Green" = "Green"), selected = "Fairway", width = "100%")
              ),
              column(12, class = "col-md-2",
                div(class = "props-label", "Putt (ft)"),
                numericInput("hh_putt_ft", NULL, value = 15, min = 1, max = 100, step = 1, width = "100%")
              )
            ),
            p("Turn on Live inputs to reprice from the shot model only (requires shot_transition_model.rds). Off = long-run hole history + skill tilt.", class = "text-muted", style = "margin: 6px 0 0 0; font-size: 0.9em;")
          ),
          uiOutput("hole_sim_top_bar"),
          fluidRow(
            column(12, class = "col-md-4", uiOutput("hole_sim_panel_outcomes")),
            column(12, class = "col-md-4", uiOutput("hole_sim_panel_hole")),
            column(12, class = "col-md-4", uiOutput("hole_sim_panel_sequence"))
          )
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {

  simulated_round_data <- reactiveVal(
    if (exists("simulated_round_table", envir = .GlobalEnv)) get("simulated_round_table", envir = .GlobalEnv) else NULL
  )
  is_running <- reactiveVal(FALSE)
  last_run_time <- reactiveVal(NULL)
  round_matchups_cache <- reactiveVal(list())
  outrights_cache <- reactiveVal(list())  # market -> list of player rows with book odds
  pre_tournament_outrights <- reactiveVal(NULL)  # fallback when pipeline has no win/top_5/etc.
  pre_tournament_event_cache <- reactiveVal(NULL) # event_name from preds/pre-tournament
  schedule_upcoming_cache <- reactiveVal(NULL)    # get-schedule upcoming payload (for course lookup)
  data_loaded <- reactiveVal(FALSE)
  # From simulated_round_static.rds $model_next_round when present (DataGolf live round); else use clock.
  model_next_round_from_static <- reactiveVal(NA_integer_)
  field_updates_cache <- reactiveVal(NULL)
  live_hole_stats_cache <- reactiveVal(NULL)
  hole_data_cache <- reactiveVal(NULL)
  hole_skill_cache <- reactiveVal(NULL)

  output$auto_round_et <- renderText({
    invalidateLater(60000L, session)
    paste0(" ", ou_display_round_label())
  })

  run_simulation <- function(force_refresh = FALSE) {
    # Static projections mode: never run the heavy pipeline on shinyapps.io.
    # Instead, load a precomputed table saved locally as simulated_round_static.rds.
    if (is_running()) return()
    static_path <- file.path(model_dir, "simulated_round_static.rds")
    if (!file.exists(static_path)) {
      message("Static projections file simulated_round_static.rds not found in ", model_dir)
      return()
    }
    is_running(TRUE)
    tbl <- NULL
    event_name <- ""
    cached <- tryCatch(readRDS(static_path), error = function(e) NULL)
    if (!is.null(cached)) {
      if (is.data.frame(cached)) {
        tbl <- cached
        model_next_round_from_static(NA_integer_)
      } else if (is.list(cached) && "data" %in% names(cached)) {
        tbl <- cached$data
        event_name <- as.character(cached$event_name %||% "")
        mr <- suppressWarnings(as.integer(cached$model_next_round %||% NA_integer_))
        if (is.finite(mr) && mr >= 1L && mr <= 4L) {
          model_next_round_from_static(as.integer(mr))
        } else {
          model_next_round_from_static(NA_integer_)
        }
      }
    }
    if (!is.null(tbl) && is.data.frame(tbl) && nrow(tbl) > 0) {
      simulated_round_data(tbl)
      last_run_time(Sys.time())
      data_loaded(TRUE)
      round_matchups_cache(list())
      if (nzchar(event_name)) {
        tryCatch({ dg_current_event_name <<- event_name }, error = function(e) NULL)
      }
    }
    is_running(FALSE)
  }

  # Display round (1-4): prefer round baked into static RDS (DataGolf current_round); else ET 9pm schedule.
  display_round_num <- reactive({
    invalidateLater(60000L, session)
    mr <- model_next_round_from_static()
    if (is.finite(mr) && mr >= 1L && mr <= 4L) return(as.integer(mr))
    ou_display_round_auto()
  })

  # One row per player for the selected round (instant when changing rounds; no re-run)
  round_display_data <- reactive({
    rs <- simulated_round_data()
    if (is.null(rs) || nrow(rs) == 0) return(NULL)
    rn <- display_round_num()
    if ("round" %in% names(rs)) rs %>% dplyr::filter(round == rn) else rs
  })

  # Intersect projections with field; use tee-filtered field when tee sheet is posted, else full field
  round_display_data_filtered <- reactive({
    rs <- round_display_data()
    field_full <- field_updates_table()
    field <- field_updates_filtered()
    if (is.null(rs) || nrow(rs) == 0) return(rs)
    if (is.null(field_full) || nrow(field_full) == 0) return(rs)
    keys_field <- if (!is.null(field) && nrow(field) > 0) field else field_full
    field_keys <- vapply(as.character(keys_field$player_name), function(n) name_match_key(n), character(1))
    field_keys <- unique(field_keys[nzchar(field_keys)])
    if (length(field_keys) == 0) return(rs)
    rs_keys <- vapply(as.character(rs$player_name), function(n) name_match_key(n), character(1))
    rs %>% dplyr::filter(rs_keys %in% field_keys)
  })

  # Model O/U: DataGolf field (tee time required for round) + projections; projections-only if field-updates never loaded
  leaderboard_full_field <- reactive({
    field <- field_updates_filtered()
    field_any <- field_updates_table()
    rs <- round_display_data()
    # Fallback: no field-updates response yet — use projections only (no tee time column from DG)
    if ((is.null(field_any) || nrow(field_any) == 0) && !is.null(rs) && nrow(rs) > 0) {
      df <- rs %>%
        dplyr::mutate(
          Golfer = name_display(player_name)
        )
      return(df %>%
        dplyr::select(
          Golfer,
          dplyr::any_of(c(
            "total_score", "round_sd", "score_to_par", "gir", "fairways",
            "eagles", "birdies", "pars", "bogeys", "doubles"
          ))
        ))
    }
    if (is.null(field) || nrow(field) == 0) {
      if (!is.null(field_any) && nrow(field_any) > 0) field <- field_any else return(NULL)
    }
    field_keys <- vapply(as.character(field$player_name), function(n) name_match_key(n), character(1))
    flag_html <- country_to_flag_img(field$country, 12L)
    field_df <- field %>%
      mutate(.key = field_keys, Golfer = paste0('<span class="golfer-cell">', flag_html, '<span class="golfer-name">', name_display(player_name), '</span></span>'))
    if (is.null(rs) || nrow(rs) == 0) {
      return(field_df %>% mutate(
        total_score = NA_real_, round_sd = NA_real_, score_to_par = NA_real_,
        gir = NA_real_, fairways = NA_real_,
        eagles = NA_integer_, birdies = NA_integer_, pars = NA_integer_, bogeys = NA_integer_, doubles = NA_integer_
      ) %>% select(-.key))
    }
    rs_keys <- vapply(as.character(rs$player_name), function(n) name_match_key(n), character(1))
    rs_lookup <- rs %>% mutate(.key = rs_keys) %>%
      select(.key, any_of(c("total_score", "round_sd", "score_to_par", "gir", "fairways", "eagles", "birdies", "pars", "bogeys", "doubles"))) %>%
      distinct(.key, .keep_all = TRUE)
    field_df %>%
      distinct(dg_id, .keep_all = TRUE) %>%
      left_join(rs_lookup, by = ".key") %>%
      select(-.key)
  })

  # Field updates from DataGolf API (field_updates endpoint, JSON): event info + list of players in tournament
  field_updates_raw <- reactive({
    field_updates_cache()
  })

  # Parsed field table: one row per player from field_updates "field" array (for Tournament Field tab and fallbacks)
  field_updates_table <- reactive({
    raw <- field_updates_raw()
    if (is.null(raw) || !is.list(raw)) return(NULL)
    flist <- raw$field
    if (is.null(flist) || length(flist) == 0) return(NULL)
    rn <- display_round_num()
    # Build one row per player; handle list of lists or data.frame (from fromJSON simplifyDataFrame)
    rows <- lapply(seq_len(if (is.data.frame(flist)) nrow(flist) else length(flist)), function(i) {
      if (is.data.frame(flist)) p <- as.list(flist[i, , drop = FALSE]) else p <- flist[[i]]
      if (is.null(p)) return(NULL)
      player_name <- as.character(p$player_name %||% p$name %||% p[["player.name"]] %||% "")
      if (!nzchar(trimws(player_name))) return(NULL)
      teetime_str <- dg_teetime_for_round_from_player(p, rn)
      made_cut <- NA_real_
      for (key in c("made_cut", "cut_made", "made_cut_flag")) {
        if (is.null(p[[key]])) next
        v <- p[[key]]
        if (is.logical(v)) {
          made_cut <- if (isTRUE(v[[1L]])) 1 else 0
          break
        }
        made_cut <- suppressWarnings(as.numeric(v))
        if (is.finite(made_cut)) break
        if (is.character(v)) {
          vv <- tolower(trimws(as.character(v[[1L]])))
          if (vv %in% c("y", "yes", "true", "1", "made")) {
            made_cut <- 1
            break
          }
          if (vv %in% c("n", "no", "false", "0", "miss", "missed")) {
            made_cut <- 0
            break
          }
        }
      }
      tibble(
        player_name = player_name,
        dg_id = as.integer(p$dg_id %||% NA_integer_),
        country = as.character(p$country %||% ""),
        dg_rank = as.character(p$dg_rank %||% ""),
        owgr_rank = as.character(p$owgr_rank %||% ""),
        tour_rank = as.character(p$tour_rank %||% ""),
        teetime_upcoming = teetime_str,
        made_cut = made_cut
      )
    })
    df <- dplyr::bind_rows(rows[!vapply(rows, is.null, logical(1))])
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>% distinct(dg_id, .keep_all = TRUE)
  })

  # Field rows for projections / Model O/U: Sat/Sun rounds exclude missed-cut players when DataGolf shows tee times or made_cut.
  field_updates_filtered <- reactive({
    fld <- field_updates_table()
    rn <- display_round_num()
    filter_field_require_teetime(fld, rn)
  })

  # Event info from field updates (for subtitles)
  field_updates_event_info <- reactive({
    raw <- field_updates_raw()
    if (is.null(raw) || !is.list(raw)) return(list(event_name = "", course_name = "", date_start = "", date_end = ""))
    list(
      event_name = as.character(raw$event_name %||% ""),
      course_name = as.character(raw$course_name %||% ""),
      date_start = as.character(raw$date_start %||% ""),
      date_end = as.character(raw$date_end %||% "")
    )
  })

  pre_tournament_event_info <- reactive({
    raw <- pre_tournament_event_cache()
    if (is.null(raw) || !is.list(raw)) return(list(event_name = "", last_updated = ""))
    list(
      event_name = trimws(as.character(raw$event_name %||% "")),
      last_updated = trimws(as.character(raw$last_updated %||% ""))
    )
  })

  schedule_event_course_info <- reactive({
    raw <- schedule_upcoming_cache()
    empty <- list(event_name = "", course_name = "")
    if (is.null(raw) || !is.list(raw)) return(empty)
    events <- raw$events %||% raw$schedule %||% raw$tournaments %||% raw
    target <- trimws(as.character(pre_tournament_event_info()$event_name %||% ""))
    norm_event <- function(x) {
      x <- tolower(trimws(as.character(x)))
      x <- gsub("&", " and ", x, fixed = TRUE)
      x <- gsub("[^a-z0-9]+", " ", x)
      x <- gsub("\\s+", " ", x)
      trimws(x)
    }
    target_key <- norm_event(target)
    evs <- list()
    if (is.data.frame(events) && nrow(events) > 0) {
      evs <- lapply(seq_len(nrow(events)), function(i) as.list(events[i, , drop = FALSE]))
    } else if (is.list(events) && length(events) > 0) {
      evs <- events
    } else {
      return(empty)
    }
    pick <- NULL
    if (nzchar(target_key)) {
      for (ev in evs) {
        if (is.data.frame(ev) && nrow(ev) > 0) ev <- as.list(ev[1, , drop = FALSE])
        if (!is.list(ev)) next
        ev_name <- trimws(as.character(ev$event_name %||% ev$name %||% ev$tournament_name %||% ""))
        if (nzchar(ev_name) && identical(norm_event(ev_name), target_key)) { pick <- ev; break }
      }
    }
    if (is.null(pick)) return(empty)
    list(
      event_name = trimws(as.character(pick$event_name %||% pick$name %||% pick$tournament_name %||% "")),
      course_name = trimws(as.character(pick$course_name %||% pick$course %||% pick$coursename %||% ""))
    )
  })

  refresh_field_updates <- function(context = "field updates fetch") {
    tryCatch({
      r <- httr::GET(
        "https://feeds.datagolf.com/field-updates",
        query = list(tour = "pga", file_format = "json", key = datagolf_api_key)
      )
      if (httr::status_code(r) == 200) {
        data <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE)
        field_updates_cache(data)
      }
    }, error = function(e) message(context, ": ", conditionMessage(e)))
  }

  refresh_pre_tournament_event <- function(context = "pre-tournament fetch") {
    tryCatch({
      r <- httr::GET(
        "https://feeds.datagolf.com/preds/pre-tournament",
        query = list(
          tour = "pga",
          dead_heat = "yes",
          odds_format = "percent",
          file_format = "json",
          key = datagolf_api_key
        )
      )
      if (httr::status_code(r) == 200) {
        data <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE)
        if (is.list(data)) pre_tournament_event_cache(data)
      }
    }, error = function(e) message(context, ": ", conditionMessage(e)))
  }

  refresh_schedule_upcoming <- function(context = "schedule fetch") {
    tryCatch({
      r <- httr::GET(
        "https://feeds.datagolf.com/get-schedule",
        query = list(tour = "pga", upcoming_only = "yes", file_format = "json", key = datagolf_api_key)
      )
      if (httr::status_code(r) == 200) {
        data <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE)
        if (is.list(data)) schedule_upcoming_cache(data)
      }
    }, error = function(e) message(context, ": ", conditionMessage(e)))
  }

  # No prefetch on load — defer RDS load to avoid OOM on shinyapps.io (load only when user clicks Refresh)
  # Do not auto-run run_simulation() on tab switch; user must click "Refresh" / "Refresh projections" to load.

  observeEvent(input$main_tabs, {
    tab <- req(input$main_tabs)
    if (identical(tab, "Model O/U") || identical(tab, "Outrights")) {
      refresh_field_updates("Field updates fetch")
      refresh_pre_tournament_event("Pre-tournament fetch")
      refresh_schedule_upcoming("Schedule fetch")
    }
    # Outrights: do not auto-load from cache here (avoids OOM); user clicks Refresh to load.
  }, ignoreInit = TRUE)

  observeEvent(list(input$refresh_projections, input$refresh_projections_outrights), {
    if (!is_running()) {
      refresh_field_updates("Field updates refresh")
      refresh_pre_tournament_event("Pre-tournament refresh")
      refresh_schedule_upcoming("Schedule refresh")
      data_loaded(FALSE)
      run_simulation(force_refresh = TRUE)
    }
  }, ignoreNULL = TRUE)

  props_lines_trigger <- reactiveVal(0)
  observeEvent(input$refresh_lines, {
    props_lines_trigger(isolate(props_lines_trigger()) + 1)
  })

  props_lines <- reactive({
    props_lines_trigger()
    stat <- input$prop_stat
    if (is.null(stat)) stat <- "Total Score"
    path <- switch(stat,
      "Total Score" = player_props_path,
      "Birdies"     = player_props_birdies_path,
      "Pars"        = player_props_pars_path,
      "Bogeys"      = player_props_bogeys_path,
      player_props_path
    )
    if (!file.exists(path)) return(tibble(player_name = character(), line = numeric(), over_odds = numeric(), under_odds = numeric()))
    read_csv(path, show_col_types = FALSE)
  })

  # Normalize CSV stat/market column to canonical: Total Score, Birdies, Pars, Bogeys
  normalize_market <- function(x) {
    v <- trimws(tolower(as.character(x)))
    dplyr::case_when(
      v %in% c("total score", "round_score", "score", "round score", "total") ~ "Total Score",
      v %in% c("birdies", "birdie") ~ "Birdies",
      v %in% c("pars", "par") ~ "Pars",
      v %in% c("bogeys", "bogey") ~ "Bogeys",
      TRUE ~ NA_character_
    )
  }

  # Canonical name key for matching (handles "Last, First" vs "First Last" and minor spacing)
  name_match_key <- function(x) {
    x <- trimws(tolower(as.character(x)))
    x <- gsub(",", " ", x, fixed = TRUE)
    x <- gsub("\\s+", " ", x)
    words <- strsplit(x, " ", fixed = TRUE)[[1]]
    if (length(words) == 0) return("")
    paste(sort(trimws(words)), collapse = " ")
  }

  all_props <- reactive({
    props_lines_trigger()
    paths <- list(
      "Total Score" = player_props_path,
      "Birdies"     = c(player_props_birdies_path, file.path(model_dir, "data", "player_props_birdies_custom.csv")),
      "Pars"        = player_props_pars_path,
      "Bogeys"      = player_props_bogeys_path
    )
    out <- list()
    for (market in names(paths)) {
      p_vec <- paths[[market]]
      if (!is.character(p_vec)) p_vec <- c(p_vec)
      tbl_list <- list()
      for (p in p_vec) {
        if (!file.exists(p)) next
        tbl <- tryCatch(read_csv(p, show_col_types = FALSE), error = function(e) NULL)
        if (!is.null(tbl) && nrow(tbl) > 0) tbl_list[[length(tbl_list) + 1L]] <- tbl
      }
      if (length(tbl_list) == 0) next
      tbl <- bind_rows(tbl_list)
      # Accept alternate column names so more CSVs load
      nm <- names(tbl)
      if (!"player_name" %in% nm) {
        if ("player" %in% nm) tbl$player_name <- tbl$player
        else if ("name" %in% nm) tbl$player_name <- tbl$name
        else if ("golfer" %in% nm) tbl$player_name <- tbl$golfer
      }
      if (!"over_odds" %in% nm && "over" %in% nm) tbl$over_odds <- tbl$over
      if (!"under_odds" %in% nm && "under" %in% nm) tbl$under_odds <- tbl$under
      if (!all(c("player_name", "line", "over_odds", "under_odds") %in% names(tbl))) next
      # Use per-row market from CSV if present (stat / market / prop_type), else file-based
      if ("stat" %in% names(tbl)) {
        tbl$market <- normalize_market(tbl$stat)
      } else if ("market" %in% names(tbl)) {
        tbl$market <- normalize_market(tbl$market)
      } else if ("prop_type" %in% names(tbl)) {
        tbl$market <- normalize_market(tbl$prop_type)
      } else {
        tbl$market <- market
      }
      tbl <- tbl %>% dplyr::filter(!is.na(.data$market))
      if (nrow(tbl) == 0) next
      out[[market]] <- tbl
    }
    # Also load any other CSV in data/ that has prop columns (so more bet lines show up)
    data_dir <- file.path(model_dir, "data")
    if (dir.exists(data_dir)) {
      extra <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
      for (p in extra) {
        if (p %in% unlist(paths)) next
        tbl <- tryCatch(read_csv(p, show_col_types = FALSE), error = function(e) NULL)
        if (is.null(tbl) || nrow(tbl) == 0) next
        nm <- names(tbl)
        if (!"player_name" %in% nm) {
          if ("player" %in% nm) tbl$player_name <- tbl$player
          else if ("name" %in% nm) tbl$player_name <- tbl$name
          else if ("golfer" %in% nm) tbl$player_name <- tbl$golfer
          else next
        }
        if (!"line" %in% nm) next
        if (!"over_odds" %in% nm && "over" %in% nm) tbl$over_odds <- tbl$over
        else if (!"over_odds" %in% nm) next
        if (!"under_odds" %in% nm && "under" %in% nm) tbl$under_odds <- tbl$under
        else if (!"under_odds" %in% nm) next
        if ("stat" %in% names(tbl)) tbl$market <- normalize_market(tbl$stat)
        else if ("market" %in% names(tbl)) tbl$market <- normalize_market(tbl$market)
        else if ("prop_type" %in% names(tbl)) tbl$market <- normalize_market(tbl$prop_type)
        else next
        tbl <- tbl %>% dplyr::filter(!is.na(.data$market))
        if (nrow(tbl) > 0) out[[paste0("extra_", basename(p))]] <- tbl
      }
    }
    if (length(out) == 0) return(tibble(player_name = character(), line = numeric(), over_odds = numeric(), under_odds = numeric(), market = character()))
    bind_rows(out) %>%
      mutate(line = if_else(market %in% c("Pars", "Birdies", "Bogeys") & is.finite(line) & line == floor(line), line + 0.5, line))
  })

  ev_bets_df <- reactive({
    rs <- round_display_data_filtered()
    props <- all_props()
    if (is.null(rs) || nrow(rs) == 0) return(NULL)
    rs_display <- rs %>%
      mutate(display_name = name_display(player_name)) %>%
      mutate(key_player = vapply(as.character(player_name), function(n) name_match_key(n), character(1)),
             key_display = vapply(as.character(display_name), function(n) name_match_key(n), character(1)))
    # If no CSV props, or to show all possible bets: add one line per (player, market) from model
    # so every player gets Total Score, Birdies, Pars, Bogeys with line = model mean, placeholder odds
    existing_key_market <- if (nrow(props) > 0) {
      props %>% mutate(.key = vapply(as.character(player_name), function(n) name_match_key(n), character(1))) %>%
        distinct(.key, market) %>% mutate(.id = paste(.key, market, sep = "|"))
    } else NULL
    synthetic <- list()
    for (j in seq_len(nrow(rs_display))) {
      r <- rs_display[j, ]
      key <- r$key_player[1]
      for (market in c("Total Score", "Birdies", "Pars", "Bogeys")) {
        if (!is.null(existing_key_market) && paste(key, market, sep = "|") %in% existing_key_market$.id) next
        mu <- switch(market,
          "Total Score" = as.numeric(r$total_score[1]),
          "Birdies"     = as.numeric(r$birdies[1]),
          "Pars"        = as.numeric(r$pars[1]),
          "Bogeys"      = as.numeric(r$bogeys[1]),
          NA_real_
        )
        if (!is.finite(mu)) next
        line <- if (market == "Total Score") round(mu, 1) else (floor(mu) + 0.5)
        synthetic[[length(synthetic) + 1]] <- tibble(
          player_name = as.character(r$player_name[1]),
          line = line,
          over_odds = -110,
          under_odds = 110,
          market = market
        )
      }
    }
    if (length(synthetic) > 0) {
      syn_df <- bind_rows(synthetic)
      props <- if (nrow(props) > 0) bind_rows(props, syn_df) else syn_df
    }
    if (is.null(props) || nrow(props) == 0) return(NULL)
    rows <- list()
    for (i in seq_len(nrow(props))) {
      row <- props[i, ]
      display <- trimws(as.character(row$player_name))
      prop_key <- name_match_key(display)
      if (!nzchar(prop_key)) next
      match_rs <- rs_display %>% dplyr::filter(key_player == prop_key | key_display == prop_key)
      if (nrow(match_rs) != 1) next
      mu <- switch(
        row$market,
        "Total Score" = as.numeric(match_rs$total_score[1]),
        "Birdies"     = as.numeric(match_rs$birdies[1]),
        "Pars"        = as.numeric(match_rs$pars[1]),
        "Bogeys"      = as.numeric(match_rs$bogeys[1]),
        NA_real_
      )
      if (is.na(mu)) next
      sd_score <- 2.5
      if (row$market == "Total Score" && "round_sd" %in% names(match_rs)) {
        rs <- as.numeric(match_rs$round_sd[1])
        if (is.finite(rs) && rs > 0.5 && rs < 5) sd_score <- rs
      }
      line <- as.numeric(row$line[1])
      over_odds <- as.numeric(row$over_odds[1])
      under_odds <- as.numeric(row$under_odds[1])
      if (!is.finite(line)) next
      dec_over  <- american_to_decimal(over_odds)
      dec_under <- american_to_decimal(under_odds)
      if (row$market == "Total Score") {
        p_over  <- 1 - pnorm(line, mean = mu, sd = sd_score)
        p_under <- pnorm(line, mean = mu, sd = sd_score)
      } else {
        k <- max(0, floor(line))
        p_under <- ppois(k, mu)
        p_over  <- 1 - ppois(k, mu)
      }
      impl_over  <- if (is.finite(dec_over) && dec_over > 0) 1 / dec_over else NA_real_
      impl_under <- if (is.finite(dec_under) && dec_under > 0) 1 / dec_under else NA_real_
      total_impl <- if (is.finite(impl_over) && is.finite(impl_under)) impl_over + impl_under else NA_real_
      market_over  <- if (is.finite(total_impl) && total_impl > 0) 100 * impl_over / total_impl else NA_real_
      market_under <- if (is.finite(total_impl) && total_impl > 0) 100 * impl_under / total_impl else NA_real_
      edge_over  <- if (is.finite(p_over) && is.finite(market_over)) (p_over * 100) - market_over else NA_real_
      edge_under <- if (is.finite(p_under) && is.finite(market_under)) (p_under * 100) - market_under else NA_real_
      fair_over  <- if (p_over > 0) 1 / p_over else NA_real_
      fair_under <- if (p_under > 0) 1 / p_under else NA_real_
      ev_over  <- p_over * dec_over - 1
      ev_under <- p_under * dec_under - 1
      sane_min <- 1.01
      sane_max <- 500
      over_ok  <- is.finite(fair_over)  & fair_over >= sane_min & fair_over <= sane_max
      under_ok <- is.finite(fair_under) & fair_under >= sane_min & fair_under <= sane_max
      over_price  <- if (over_ok)  format_american(decimal_to_american(fair_over))  else "None"
      under_price <- if (under_ok) format_american(decimal_to_american(fair_under)) else "None"
      over_ev  <- if (over_ok)  round(ev_over, 3)  else NA_real_
      under_ev <- if (under_ok) round(ev_under, 3) else NA_real_
      rows[[length(rows) + 1]] <- tibble(
        Player = display,
        Market = row$market,
        Bet = paste0("Over ", line),
        `Model %` = round(p_over * 100, 1),
        `Market %` = round(market_over, 1),
        `Edge (pp)` = round(edge_over, 1),
        Edge_num = edge_over,
        `My expected price` = over_price,
        `Actual price` = format_american(over_odds),
        EV = over_ev
      )
      rows[[length(rows) + 1]] <- tibble(
        Player = display,
        Market = row$market,
        Bet = paste0("Under ", line),
        `Model %` = round(p_under * 100, 1),
        `Market %` = round(market_under, 1),
        `Edge (pp)` = round(edge_under, 1),
        Edge_num = edge_under,
        `My expected price` = under_price,
        `Actual price` = format_american(under_odds),
        EV = under_ev
      )
    }
    if (length(rows) == 0) return(NULL)
    df <- bind_rows(rows) %>% arrange(desc(Edge_num), desc(EV))
    df
  })

  observeEvent(input$prop_stat, {
    stat <- input$prop_stat
    if (stat %in% c("Pars", "Birdies", "Bogeys")) {
      updateNumericInput(session, "prop_line", max = 18)
    } else {
      updateNumericInput(session, "prop_line", max = 85)
    }
  })

  observeEvent(list(input$golfer, input$prop_stat), {
    g <- input$golfer
    stat <- input$prop_stat
    if (is.null(g) || !nzchar(g)) return()
    pl <- props_lines()
    display <- name_display(g)
    row <- if (nrow(pl) > 0) pl %>% dplyr::filter(trimws(tolower(player_name)) == trimws(tolower(display))) else pl
    if (nrow(row) == 1) {
      line_val <- as.numeric(row$line[1])
      if (stat %in% c("Pars", "Birdies", "Bogeys")) line_val <- min(18, line_val)
      updateNumericInput(session, "prop_line", value = line_val)
      updateNumericInput(session, "prop_odds_over", value = as.numeric(row$over_odds[1]))
      updateNumericInput(session, "prop_odds_under", value = as.numeric(row$under_odds[1]))
    } else {
      def_line <- switch(stat, "Total Score" = 70.5, "Birdies" = 4.5, "Pars" = 10.5, "Bogeys" = 2.5, 70.5)
      updateNumericInput(session, "prop_line", value = def_line)
      updateNumericInput(session, "prop_odds_over", value = -110)
      updateNumericInput(session, "prop_odds_under", value = 110)
    }
  })

  output$golfer_choice <- renderUI({
    rs <- round_display_data_filtered()
    # Fallback: golfers from field_updates (tournament field) when no simulated round data yet
    if (is.null(rs) || nrow(rs) == 0) {
      field <- field_updates_filtered()
      if (!is.null(field) && nrow(field) > 0) {
        choices <- setNames(field$player_name, name_display(field$player_name))
        return(selectInput("golfer", NULL, choices = choices, selected = choices[1], width = "100%"))
      }
      return(selectInput("golfer", NULL, choices = c("(Loading...)" = ""), width = "100%"))
    }
    choices <- setNames(rs$player_name, name_display(rs$player_name))
    # Default lineup: Collin Morikawa, Total Score 69.5, -142 / 106 (from Prop inputs)
    default_golfer <- rs$player_name[name_display(rs$player_name) == "Collin Morikawa"]
    selected <- if (length(default_golfer) == 1L) default_golfer else choices[1]
    selectInput("golfer", NULL, choices = choices, selected = selected, width = "100%")
  })

  prop_player_row <- reactive({
    rs <- round_display_data_filtered()
    g <- input$golfer
    if (is.null(rs) || nrow(rs) == 0 || is.null(g) || !nzchar(g)) return(NULL)
    out <- rs %>% dplyr::filter(player_name == g)
    if (nrow(out) != 1) return(NULL)
    out
  })

  prop_dist <- reactive({
    row <- prop_player_row()
    stat <- input$prop_stat
    if (is.null(row) || is.null(stat)) return(NULL)
    mu_score <- as.numeric(row$total_score[1])
    if (!is.finite(mu_score) && "round_score" %in% names(row)) mu_score <- as.numeric(row$round_score[1])
    if (!is.finite(mu_score)) mu_score <- 70
    mu_pars    <- as.numeric(row$pars[1])
    mu_birdies <- as.numeric(row$birdies[1])
    mu_bogeys  <- as.numeric(row$bogeys[1])
    if (!is.finite(mu_pars))    mu_pars    <- 10
    if (!is.finite(mu_birdies)) mu_birdies <- 4
    if (!is.finite(mu_bogeys))  mu_bogeys  <- 2.5
    sd_score <- 2.5
    if ("round_sd" %in% names(row)) {
      rs <- as.numeric(row$round_sd[1])
      if (is.finite(rs) && rs > 0.5 && rs < 5) sd_score <- rs
    }
    list(
      stat = stat,
      mean = switch(stat, "Total Score" = mu_score, "Pars" = mu_pars, "Birdies" = mu_birdies, "Bogeys" = mu_bogeys),
      sd_score = sd_score,
      mu_pars = mu_pars, mu_birdies = mu_birdies, mu_bogeys = mu_bogeys
    )
  })

  output$ev_over_under <- renderUI({
    dist <- prop_dist()
    line <- input$prop_line
    odds_over <- input$prop_odds_over
    odds_under <- input$prop_odds_under
    if (is.null(dist) || is.null(line) || !is.finite(line)) {
      return(p("Select a golfer and enter line and odds.", style = "color: var(--golf-muted);"))
    }
    stat <- dist$stat
    dec_over <- american_to_decimal(odds_over)
    dec_under <- american_to_decimal(odds_under)
    if (!is.finite(dec_over) || !is.finite(dec_under)) return(p("Invalid odds.", style = "color: var(--golf-muted);"))
    if (stat == "Total Score") {
      p_over  <- 1 - pnorm(line, mean = dist$mean, sd = dist$sd_score)
      p_under <- pnorm(line, mean = dist$mean, sd = dist$sd_score)
    } else {
      lambda_default <- switch(stat, "Pars" = 10, "Birdies" = 4, "Bogeys" = 2.5, 10)
      lambda <- if (is.finite(dist$mean) && dist$mean >= 0) dist$mean else lambda_default
      k <- min(18, max(0, floor(line)))
      prob_trunc <- dpois(0:18, lambda)
      prob_trunc <- prob_trunc / sum(prob_trunc)
      p_under <- sum(prob_trunc[1:(k + 1)])
      p_over  <- 1 - p_under
    }
    ev_over  <- p_over * (dec_over - 1) - (1 - p_over) * 1
    ev_under <- p_under * (dec_under - 1) - (1 - p_under) * 1
    ev_over_pct  <- round(ev_over * 100, 1)
    ev_under_pct <- round(ev_under * 100, 1)
    ev_over_cl <- if (ev_over >= 0) "ev-positive" else "ev-negative"
    ev_under_cl <- if (ev_under >= 0) "ev-positive" else "ev-negative"
    tagList(
      p(HTML(paste0("<span class=\"props-label\">Over </span> <span class=\"", ev_over_cl, "\">", ifelse(ev_over_pct >= 0, "+", ""), ev_over_pct, "%</span>"))),
      p(HTML(paste0("<span class=\"props-label\">Under </span> <span class=\"", ev_under_cl, "\">", ifelse(ev_under_pct >= 0, "+", ""), ev_under_pct, "%</span>"))))
  })

  output$prop_histogram <- renderPlotly({
    dist <- prop_dist()
    line <- input$prop_line
    if (is.null(dist)) {
      return(plot_ly(x = 1, y = 1, type = "scatter", mode = "text", text = "Select a golfer. Data loads when a round is selected.",
                     textfont = list(size = 14, color = "#8b8f9c")) %>%
               layout(xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
                      yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
                      paper_bgcolor = "#111216", plot_bgcolor = "#111216"))
    }
    stat <- dist$stat
    line <- if (is.finite(line)) line else dist$mean
    vig <- 0.045
    vig_factor <- 1 / (1 - vig)
    # Vectorized American odds formatter (avoids scalar format_american when given long vectors)
    fmt_am_vec <- function(a) ifelse(is.na(a) | !is.finite(a), "—", ifelse(a > 0, paste0("+", round(a, 0)), as.character(round(a, 0))))
    if (stat == "Total Score") {
      mu   <- if (is.finite(dist$mean)) dist$mean else 70
      sd_s <- if (is.finite(dist$sd_score) && dist$sd_score > 0) dist$sd_score else 2.5
      x_lo <- max(60, floor(mu - 4 * sd_s))
      x_hi <- min(85, ceiling(mu + 4 * sd_s))
      if (!is.finite(x_lo) || !is.finite(x_hi) || x_lo >= x_hi) {
        x_lo <- 60
        x_hi <- 80
      }
      x_fine <- seq(x_lo, x_hi, by = 0.15)
      d <- dnorm(x_fine, mu, sd_s)
      x_round <- round(x_fine, 1)
      p_over <- 1 - pnorm(x_round, mu, sd_s)
      p_under <- pnorm(x_round, mu, sd_s)
      over_vig <- ifelse(p_over > 0.001, fmt_am_vec(decimal_to_american((1 / p_over) * vig_factor)), "—")
      under_vig <- ifelse(p_under > 0.001, fmt_am_vec(decimal_to_american((1 / p_under) * vig_factor)), "—")
      hover_txt <- sprintf("Total Score: %s<br>Over %s+: %s<br>Under %s: %s", x_round, x_round, over_vig, x_round, under_vig)
      df_left  <- data.frame(x = x_fine[x_fine <= line], y = d[x_fine <= line], hover = hover_txt[x_fine <= line])
      df_right <- data.frame(x = x_fine[x_fine >= line], y = d[x_fine >= line], hover = hover_txt[x_fine >= line])
      y_max <- max(d, na.rm = TRUE) * 1.08
      if (!is.finite(y_max) || y_max <= 0) y_max <- 0.2
      p <- plot_ly() %>%
        add_trace(x = df_left$x,  y = df_left$y,  type = "scatter", mode = "lines", fill = "tozeroy",
                  line = list(color = "rgba(0,196,107,0.4)", width = 0.6), fillcolor = "rgba(0,196,107,0.2)", text = df_left$hover, hoverinfo = "text") %>%
        add_trace(x = df_right$x, y = df_right$y, type = "scatter", mode = "lines", fill = "tozeroy",
                  line = list(color = "rgba(255,77,79,0.35)", width = 0.6), fillcolor = "rgba(255,77,79,0.12)", text = df_right$hover, hoverinfo = "text") %>%
        add_trace(x = x_fine, y = d, type = "scatter", mode = "lines",
                  line = list(color = "#00c46b", width = 1.8), text = hover_txt, hoverinfo = "text") %>%
        layout(
          shapes = list(list(type = "line", x0 = line, x1 = line, y0 = 0, y1 = y_max, yref = "y", xref = "x",
                             line = list(color = "#8b8f9c", width = 1.2, dash = "dash"))),
          xaxis = list(title = list(text = "Total Score", font = list(color = "#f5f7fb")), range = c(x_lo, x_hi),
                      tickfont = list(color = "#8b8f9c", size = 11), gridcolor = "#2b2e36", zeroline = FALSE),
          yaxis = list(title = list(text = "Density", font = list(color = "#f5f7fb")), range = c(0, y_max),
                      tickfont = list(color = "#8b8f9c", size = 11), gridcolor = "#2b2e36", zeroline = FALSE),
          margin = list(t = 50, b = 55, l = 60, r = 35),
          paper_bgcolor = "#111216", plot_bgcolor = "#181a20",
          font = list(family = "inherit", color = "#f5f7fb"),
          showlegend = FALSE
        ) %>%
        plotly::config(displayModeBar = FALSE)
      return(p)
    } else {
      lambda_default <- switch(stat, "Pars" = 10, "Birdies" = 4, "Bogeys" = 2.5, 10)
      lambda <- if (is.finite(dist$mean) && dist$mean >= 0) dist$mean else lambda_default
      x <- 0:18
      prob_raw <- dpois(x, lambda)
      prob <- prob_raw / sum(prob_raw)
      p_under_cum <- cumsum(prob)
      p_or_more <- 1 - p_under_cum
      over_vig <- ifelse(p_or_more > 0.0001, fmt_am_vec(decimal_to_american((1 / p_or_more) * vig_factor)), "—")
      under_vig <- ifelse(p_under_cum > 0.0001, fmt_am_vec(decimal_to_american((1 / p_under_cum) * vig_factor)), "—")
      hover_txt <- sprintf("%s: %s<br>Over %s+: %s<br>Under %s: %s", stat, x, x, over_vig, x, under_vig)
      line_plot <- min(18, max(0, as.numeric(line)))
      idx_left  <- x <= line_plot
      idx_right <- x >= line_plot
      x_left  <- x[idx_left]
      y_left  <- prob[idx_left]
      h_left  <- hover_txt[idx_left]
      x_right <- x[idx_right]
      y_right <- prob[idx_right]
      h_right <- hover_txt[idx_right]
      y_max <- max(prob, na.rm = TRUE) * 1.08
      if (!is.finite(y_max) || y_max <= 0) y_max <- 0.2
      x_range <- c(-0.5, 18.5)
      p <- plot_ly() %>%
        add_trace(x = x_left,  y = y_left,  type = "scatter", mode = "lines", fill = "tozeroy",
                  line = list(color = "rgba(0,196,107,0.4)", width = 0.6), fillcolor = "rgba(0,196,107,0.2)", text = h_left, hoverinfo = "text") %>%
        add_trace(x = x_right, y = y_right, type = "scatter", mode = "lines", fill = "tozeroy",
                  line = list(color = "rgba(255,77,79,0.35)", width = 0.6), fillcolor = "rgba(255,77,79,0.12)", text = h_right, hoverinfo = "text") %>%
        add_trace(x = x, y = prob, type = "scatter", mode = "lines+markers", marker = list(size = 6, color = "#00c46b"),
                  line = list(color = "#00c46b", width = 1.8), text = hover_txt, hoverinfo = "text") %>%
        layout(
          shapes = list(list(type = "line", x0 = line_plot, x1 = line_plot, y0 = 0, y1 = y_max, yref = "y", xref = "x",
                             line = list(color = "#8b8f9c", width = 1.2, dash = "dash"))),
          xaxis = list(title = list(text = stat, font = list(color = "#f5f7fb")), range = x_range,
                      tickfont = list(color = "#8b8f9c", size = 11), gridcolor = "#2b2e36", zeroline = FALSE, dtick = 1),
          yaxis = list(title = list(text = "Probability", font = list(color = "#f5f7fb")), range = c(0, y_max),
                      tickfont = list(color = "#8b8f9c", size = 11), gridcolor = "#2b2e36", zeroline = FALSE),
          margin = list(t = 50, b = 55, l = 60, r = 35),
          paper_bgcolor = "#111216", plot_bgcolor = "#181a20",
          font = list(family = "inherit", color = "#f5f7fb"),
          showlegend = FALSE
        ) %>%
        plotly::config(displayModeBar = FALSE)
      return(p)
    }
  })

  output$leaderboard_title <- renderUI({
    rs <- round_display_data_filtered()
    field <- field_updates_table()
    pt_info <- pre_tournament_event_info()
    sched_info <- schedule_event_course_info()
    fu_info <- field_updates_event_info()
    stat <- input$lb_stat %||% "Total score"
    if (is.null(rs) || nrow(rs) == 0) {
      if (!is.null(field) && nrow(field) > 0) {
        event <- if (nzchar(pt_info$event_name)) pt_info$event_name else if (nzchar(fu_info$event_name)) fu_info$event_name else "Upcoming tournament"
        return(h2(paste0("Tournament Field — ", event), class = "leaderboard-title"))
      }
      return(h2("Model O/U — Upcoming Round", class = "leaderboard-title"))
    }
    rn <- display_round_num()
    rnd <- paste0(
      "R", rn, " (",
      switch(as.character(rn), "1" = "Thursday", "2" = "Friday", "3" = "Saturday", "4" = "Sunday", "?"),
      ")"
    )
    norm_event <- function(x) {
      x <- tolower(trimws(as.character(x)))
      x <- gsub("&", " and ", x, fixed = TRUE)
      x <- gsub("[^a-z0-9]+", " ", x)
      x <- gsub("\\s+", " ", x)
      trimws(x)
    }
    pt_key <- norm_event(pt_info$event_name %||% "")
    fu_key <- norm_event(fu_info$event_name %||% "")
    sched_key <- norm_event(sched_info$event_name %||% "")
    course_txt <- trimws(as.character(sched_info$course_name %||% ""))
    if (!nzchar(pt_key) || !nzchar(sched_key) || !identical(pt_key, sched_key)) course_txt <- ""
    if (!nzchar(course_txt)) {
      fu_course <- trimws(as.character(fu_info$course_name %||% ""))
      if (nzchar(pt_key) && nzchar(fu_key) && identical(pt_key, fu_key)) course_txt <- fu_course
    }
    if (!nzchar(course_txt) && "course_used" %in% names(rs)) course_txt <- trimws(as.character(rs$course_used[1L] %||% ""))
    title_line <- if (nzchar(course_txt)) {
      paste0("Model O/U — ", stat, " — ", rnd, " · ", course_txt)
    } else {
      paste0("Model O/U — ", stat, " — ", rnd)
    }
    note <- if (identical(stat, "Total score")) {
      paste0(
        "Normal total with continuity correction at each integer line; SD from projected round_sd (or default). ",
        "Fair prices then two-way hold for displayed American pairs."
      )
    } else {
      paste0(
        "Counts: Poisson at projected means. Median \u2248 Poisson median. ",
        "Fair prices then two-way hold for displayed American pairs."
      )
    }
    tt_tz <- Sys.getenv("GOLF_TEETIME_TZ", "America/New_York")
    note <- paste(
      note,
      " Field list: DataGolf field-updates for the auto-selected round — only players with a tee time for that round (NA/blank excluded). UTC ISO tee times display in",
      tt_tz, "."
    )
    note <- paste(
      note,
      sprintf(
        " American O/U pairs include a two-way book hold (~%.1f%% overround), so favorites/longshots are not symmetric +n/-n fair mirrors.",
        100 * OU_BOOK_HOLD
      )
    )
    tagList(
      h2(title_line, class = "leaderboard-title"),
      p(class = "text-muted", style = "font-size:0.8rem;margin-top:-6px;margin-bottom:0px;", note)
    )
  })

  output$status <- renderText({
    if (is_running()) return("  Loading...")
    lt <- last_run_time()
    rs <- simulated_round_data()
    if (!data_loaded() || is.null(rs) || nrow(rs) == 0) return("  Click Refresh to load projections.")
    if (is.null(lt)) return("")
    paste0("  Updated ", format(lt, "%H:%M:%S"))
  })

  output$leaderboard <- renderDT({
    lb <- leaderboard_full_field()
    if (is.null(lb) || nrow(lb) == 0) {
      return(datatable(
        data.frame(Message = "Click Refresh to load projections. Tournament field will load when you open this tab."),
        rownames = FALSE
      ))
    }
    stat <- input$lb_stat %||% "Total score"
    tryCatch({
      lines <- LB_OU_LINE_RANGES[[stat]]
      if (is.null(lines)) lines <- 64L:74L

      df <- lb
      n <- nrow(df)
      out <- data.frame(Golfer = df$Golfer, stringsAsFactors = FALSE)

      if (identical(stat, "Total score")) {
        if (!"total_score" %in% names(df)) df$total_score <- NA_real_
        if (!"round_sd" %in% names(df)) df$round_sd <- NA_real_
        mu_vec <- suppressWarnings(as.numeric(df$total_score))
        sd_vec <- suppressWarnings(as.numeric(df$round_sd))
        out[["Median"]] <- ifelse(is.finite(mu_vec), sprintf("%.2f", mu_vec), "—")
        for (L in lines) {
          out[[as.character(L)]] <- vapply(seq_len(n), function(i) {
            model_total_score_ou_html(mu_vec[i], sd_vec[i], L)
          }, character(1))
        }
        ord <- ifelse(is.finite(mu_vec), mu_vec, 999)
        out <- out[order(ord, out$Golfer), , drop = FALSE]
      } else {
        col_map <- c(
          "Birdies" = "birdies",
          "Pars" = "pars",
          "Bogeys" = "bogeys",
          "GIR" = "gir",
          "Fairways hit" = "fairways"
        )
        colnm <- if (stat %in% names(col_map)) col_map[[stat]] else NA_character_
        if (is.na(colnm) || !nzchar(colnm)) colnm <- "birdies"
        if (!colnm %in% names(df)) df[[colnm]] <- NA_real_
        lambda_vec <- suppressWarnings(as.numeric(df[[colnm]]))
        med_num <- vapply(seq_len(n), function(i) {
          lam <- lambda_vec[i]
          if (!is.finite(lam) || lam < 0) return(NA_real_)
          as.numeric(stats::qpois(0.5, lam))
        }, numeric(1))
        out[["Median"]] <- ifelse(is.finite(med_num), as.character(as.integer(med_num)), "—")
        for (L in lines) {
          out[[as.character(L)]] <- vapply(seq_len(n), function(i) {
            model_count_ou_html(lambda_vec[i], L)
          }, character(1))
        }
        if (identical(stat, "Bogeys")) {
          ord <- ifelse(is.finite(lambda_vec), lambda_vec, 999)
          out <- out[order(ord, out$Golfer), , drop = FALSE]
        } else {
          ord <- ifelse(is.finite(lambda_vec), -lambda_vec, 999)
          out <- out[order(ord, out$Golfer), , drop = FALSE]
        }
      }

      if ("teetime_upcoming" %in% names(df)) {
        out[["Tee time"]] <- vapply(seq_len(n), function(i) {
          tt <- df$teetime_upcoming[i]
          if (is.na(tt) || !nzchar(trimws(as.character(tt)))) return("—")
          format_teetime_est(tt)
        }, character(1))
      }

      ncols <- ncol(out)
      column_defs <- lapply(seq_len(ncols) - 1L, function(i) {
        list(className = if (i == 0L) "dt-left" else "dt-center", targets = i)
      })
      datatable(
        out,
        rownames = FALSE,
        class = "stripe hover",
        escape = FALSE,
        options = list(
          pageLength = -1,
          lengthChange = FALSE,
          dom = "t",
          order = list(), # preserve R row order (sort differs by market)
          columnDefs = column_defs
        )
      )
    }, error = function(e) {
      datatable(
        data.frame(Message = paste0("Error building table: ", conditionMessage(e))),
        rownames = FALSE
      )
    })
  })

  output$ev_bets_table <- renderDT({
    df <- ev_bets_df()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "Loading... Ensure props CSVs exist and have matching players."),
        rownames = FALSE
      ))
    }
    ev_col_idx <- which(names(df) == "EV") - 1L
    edge_col_idx <- which(names(df) == "Edge_num") - 1L
    col_defs <- list(
      list(
        targets = ev_col_idx,
        render = DT::JS(
          "function(data, type, row) {",
          "  if (type !== 'display') return data;",
          "  if (data === null || data === '' || data === undefined) return 'None';",
          "  var n = parseFloat(data);",
          "  if (isNaN(n)) return 'None';",
          "  return (n * 100).toFixed(2) + '%';",
          "}"
        )
      )
    )
    sort_col <- ev_col_idx
    if (length(edge_col_idx) > 0) {
      col_defs <- c(col_defs, list(list(visible = FALSE, targets = edge_col_idx)))
      sort_col <- edge_col_idx
    }
    dt <- datatable(
      df,
      rownames = FALSE,
      class = "stripe hover",
      options = list(
        pageLength = -1,                 # show all rows
        lengthChange = FALSE,
        dom = "t",
        order = list(list(sort_col, "desc")),
        columnDefs = col_defs
      )
    ) %>%
      formatStyle(
        "EV",
        backgroundColor = styleInterval(
          c(-0.001, 0.001),
          c("#3a2020", "#111216", "rgba(0,196,107,0.35)")
        ),
        color = styleInterval(0, c("#ff7b7b", "#f5f7fb"))
      )
    if ("Edge (pp)" %in% names(df) && "Edge_num" %in% names(df)) {
      dt <- dt %>% formatStyle(
        "Edge (pp)",
        valueColumns = "Edge_num",
        backgroundColor = styleInterval(
          c(-5, 5),
          c("#f8e0e0", "#f5f7f2", "#d4edda")
        ),
        color = styleInterval(0, c("#a04444", "#1a472a"))
      )
    }
    dt
  })

  # Fetch round matchups (2-player) from matchups API (pairings + odds from multiple sportsbooks)
  observe({
    tab <- input$main_tabs
    mkt <- input$matchups_market
    cache <- round_matchups_cache()
    if (is.null(tab)) return()
    market_val <- if (!has_text(mkt)) "round_matchups" else as.character(mkt[[1]])
    cache_key <- paste0("market_", market_val)
    need_fetch <- (identical(tab, "Round Matchups") && (length(cache) == 0 || is.null(cache$raw) || is.null(cache$market) || cache$market != market_val))
    if (!need_fetch) return()
    round_matchups_cache(list(raw = "loading", books = list(), market = market_val))
    tryCatch({
      r <- httr::GET(
        "https://feeds.datagolf.com/betting-tools/matchups",
        query = list(tour = "pga", market = market_val, odds_format = "decimal", file_format = "json", key = datagolf_api_key)
      )
      if (httr::status_code(r) != 200) {
        round_matchups_cache(list(raw = list(event_name = "", match_list = list()), books = list(), market = market_val))
        return()
      }
      data <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE)
      round_matchups_cache(list(raw = data, books = data, market = market_val))
    }, error = function(e) {
      message("Round matchups fetch: ", conditionMessage(e))
      round_matchups_cache(list(raw = list(event_name = "", match_list = list()), books = list(), market = market_val))
    })
  })

  round_matchups_raw <- reactive({
    cache <- round_matchups_cache()
    raw <- cache$raw %||% list(event_name = "", match_list = list())
    if (identical(raw, "loading")) return(list(event_name = "", match_list = list()))
    raw
  })

  # When matchups market changes, clear cache so next open of tab refetches
  observe({
    mkt <- input$matchups_market
    if (!has_text(mkt)) return()
    cache <- round_matchups_cache()
    if (is.list(cache) && !is.null(cache$market) && cache$market != mkt)
      round_matchups_cache(list())
  })

  round_matchups_books_raw <- reactive({
    cache <- round_matchups_cache()
    cache$books %||% list()
  })

  # Capitalize sportsbook name for display
  book_cap <- function(x) {
    known <- c("draftkings" = "DraftKings", "fanduel" = "FanDuel", "bovada" = "Bovada", "bet365" = "Bet365",
               "datagolf" = "DataGolf", "pinnacle" = "Pinnacle", "betmgm" = "BetMGM", "caesars" = "Caesars",
               "unibet" = "Unibet", "betcris" = "BetCRIS", "pointsbet" = "PointsBet", "betonline" = "BetOnline")
    x <- tolower(trimws(as.character(x %||% "")))
    if (!nzchar(x)) return("")
    out <- known[x]
    if (is.null(out) || is.na(out)) out <- paste0(toupper(substring(x, 1, 1)), substring(x, 2))
    out
  }

  round_matchups_df <- reactive({
    raw <- round_matchups_raw()
    rs <- round_display_data_filtered()
    next_r <- display_round_num()
    match_list <- raw$match_list %||% raw$pairings %||% raw$matchups %||% raw$data %||% raw$matches
    if (is.null(match_list) && is.list(raw)) {
      rkey <- paste0("round_", next_r)
      if (rkey %in% names(raw)) match_list <- raw[[rkey]]$pairings %||% raw[[rkey]]$match_list %||% raw[[rkey]]$matchups %||% raw[[rkey]]
      if (is.null(match_list)) match_list <- raw$round_1 %||% raw$round_2 %||% raw$round_3 %||% raw$round_4
    }
    if (is.null(match_list) && is.data.frame(raw)) match_list <- raw
    if (is.null(match_list) && is.list(raw) && length(raw) > 0 && is.null(names(raw))) match_list <- raw
    if (is.null(match_list)) return(NULL)
    if (is.data.frame(match_list)) {
      match_list <- lapply(seq_len(nrow(match_list)), function(i) as.list(match_list[i, , drop = FALSE]))
    }
    if (!is.list(match_list) || length(match_list) == 0) return(NULL)
    get_m <- function(m, name) if (is.list(m)) m[[name]] else m[name]
    alt <- function(m, ...) { for (n in list(...)) { v <- get_m(m, n); if (!is.null(v)) return(v) }; NULL }
    has_two <- vapply(match_list, function(m) {
      !is.null(alt(m, "p1.dg_id", "p1_dg_id", "player_1_dg_id")) &&
        !is.null(alt(m, "p2.dg_id", "p2_dg_id", "player_2_dg_id"))
    }, logical(1))
    match_list <- match_list[has_two]
    if (length(match_list) == 0) return(NULL)

    use_dg_id <- !is.null(rs) && nrow(rs) > 0 && "dg_id" %in% names(rs)
    use_strength <- use_dg_id && ("mu_sg" %in% names(rs) || "implied_mu_sg" %in% names(rs))
    use_score <- use_dg_id && "score_to_par" %in% names(rs)
    by_strength <- character(0)
    by_score <- character(0)
    if (use_dg_id && nrow(rs) > 0) {
      if (use_strength && "mu_sg" %in% names(rs))
        by_strength <- setNames(safe_num(rs$mu_sg), as.character(rs$dg_id))
      else if (use_strength && "implied_mu_sg" %in% names(rs))
        by_strength <- setNames(safe_num(rs$implied_mu_sg), as.character(rs$dg_id))
      if (use_score)
        by_score <- setNames(safe_num(rs$score_to_par), as.character(rs$dg_id))
    }
    if (length(by_strength) == 0 && length(by_score) > 0) by_strength <- NULL
    by_name_score <- if (!use_dg_id && !is.null(rs) && nrow(rs) > 0 && "score_to_par" %in% names(rs))
      setNames(safe_num(rs$score_to_par), as.character(rs$player_name)) else character(0)
    k_softmax <- 0.5
    books_raw <- round_matchups_books_raw()
    book_odds_lookup <- list()
    if (is.list(books_raw) && length(books_raw) > 0) {
      blist <- books_raw$data %||% books_raw$match_list %||% books_raw$pairings
      if (is.data.frame(blist)) blist <- lapply(seq_len(nrow(blist)), function(i) as.list(blist[i, , drop = FALSE]))
      if (is.list(blist) && length(blist) > 0) {
        for (b in blist) {
          id1 <- as.character(b$p1_dg_id %||% b$p1.dg_id %||% b$player_1_dg_id %||% "")
          id2 <- as.character(b$p2_dg_id %||% b$p2.dg_id %||% b$player_2_dg_id %||% "")
          if (!all(nzchar(c(id1, id2)))) next
          key <- paste(sort(c(id1, id2)), collapse = "_")
          if (is.null(book_odds_lookup[[key]])) book_odds_lookup[[key]] <- list()
          if (is.list(b$odds) && length(b$odds) > 0) {
            for (book_name in names(b$odds)) {
              if (tolower(trimws(as.character(book_name %||% ""))) == "datagolf") next
              ob <- b$odds[[book_name]]
              if (is.null(ob)) next
              o1 <- safe_num(ob$p1 %||% ob[1]); o2 <- safe_num(ob$p2 %||% ob[2])
              if (length(ob) >= 2 && !is.list(ob)) { o1 <- safe_num(ob[1]); o2 <- safe_num(ob[2]) }
              if (any(is.finite(c(o1, o2))))
                book_odds_lookup[[key]] <- c(book_odds_lookup[[key]], list(list(book = book_name, p1 = o1, p2 = o2)))
            }
          }
          o1 <- safe_num(b$p1 %||% b$p1_close); o2 <- safe_num(b$p2 %||% b$p2_close)
          if (any(is.finite(c(o1, o2))))
            book_odds_lookup[[key]] <- c(book_odds_lookup[[key]], list(list(book = "API", p1 = o1, p2 = o2)))
        }
      }
      if (length(books_raw) > 0 && !is.null(names(books_raw))) {
        for (book_name in names(books_raw)) {
          if (book_name %in% c("data", "match_list", "pairings", "event_name", "round_num", "market")) next
          if (tolower(trimws(as.character(book_name %||% ""))) == "datagolf") next
          blist2 <- books_raw[[book_name]]
          if (is.data.frame(blist2)) blist2 <- lapply(seq_len(nrow(blist2)), function(i) as.list(blist2[i, , drop = FALSE]))
          if (!is.list(blist2) || length(blist2) == 0) next
          for (b in blist2) {
            id1 <- as.character(b$p1_dg_id %||% b$p1.dg_id %||% ""); id2 <- as.character(b$p2_dg_id %||% b$p2.dg_id %||% "")
            if (!all(nzchar(c(id1, id2)))) next
            key <- paste(sort(c(id1, id2)), collapse = "_")
            if (is.null(book_odds_lookup[[key]])) book_odds_lookup[[key]] <- list()
            o1 <- safe_num(b$p1 %||% b$p1_close); o2 <- safe_num(b$p2 %||% b$p2_close)
            if (any(is.finite(c(o1, o2))))
              book_odds_lookup[[key]] <- c(book_odds_lookup[[key]], list(list(book = book_name, p1 = o1, p2 = o2)))
          }
        }
      }
    }
    # Collect all book names for column headers (same format as outrights)
    all_books <- character(0)
    for (i in seq_along(match_list)) {
      m <- match_list[[i]]
      p1_id <- alt(m, "p1.dg_id", "pairings.p1.dg_id", "p1_dg_id", "player_1_dg_id")
      p2_id <- alt(m, "p2.dg_id", "pairings.p2.dg_id", "p2_dg_id", "player_2_dg_id")
      key <- paste(sort(c(as.character(p1_id), as.character(p2_id))), collapse = "_")
      from_books <- book_odds_lookup[[key]]
      if (is.list(from_books)) for (eb in from_books) {
        bname <- trimws(as.character(eb$book %||% ""))
        if (nzchar(bname) && tolower(bname) != "datagolf") all_books <- c(all_books, bname)
      }
    }
    all_books <- unique(all_books)

    out <- vector("list", length(match_list))
    for (i in seq_along(match_list)) {
      m <- match_list[[i]]
      p1_id <- alt(m, "p1.dg_id", "pairings.p1.dg_id", "p1_dg_id", "player_1_dg_id")
      p2_id <- alt(m, "p2.dg_id", "pairings.p2.dg_id", "p2_dg_id", "player_2_dg_id")
      p1_name <- as.character(alt(m, "p1.name", "pairings.p1.name", "p1_player_name", "player_1_name", "p1_name"))[1]
      p2_name <- as.character(alt(m, "p2.name", "pairings.p2.name", "p2_player_name", "player_2_name", "p2_name"))[1]
      s1 <- s2 <- NA_real_
      used_strength_row <- FALSE
      if (length(by_strength) > 0 && use_dg_id) {
        s1 <- as.numeric(by_strength[as.character(p1_id)])
        s2 <- as.numeric(by_strength[as.character(p2_id)])
        if (all(is.finite(c(s1, s2)))) used_strength_row <- TRUE
      }
      if (!used_strength_row && length(by_score) > 0) {
        s1 <- as.numeric(by_score[as.character(p1_id)]); s2 <- as.numeric(by_score[as.character(p2_id)])
      }
      if (any(!is.finite(c(s1, s2))) && length(by_name_score) > 0) {
        s1 <- by_name_score[p1_name] %||% by_name_score[name_display(p1_name)]
        s2 <- by_name_score[p2_name] %||% by_name_score[name_display(p2_name)]
        s1 <- as.numeric(s1); s2 <- as.numeric(s2)
      }
      if (any(!is.finite(c(s1, s2)))) {
        prob1 <- prob2 <- NA_real_
        fair1 <- fair2 <- NA_real_
      } else {
        ex1 <- if (used_strength_row) exp(k_softmax * s1) else exp(-k_softmax * s1)
        ex2 <- if (used_strength_row) exp(k_softmax * s2) else exp(-k_softmax * s2)
        prob1 <- ex1 / (ex1 + ex2)
        prob2 <- 1 - prob1
        fair1 <- 1 / prob1; fair2 <- 1 / prob2
      }
      key <- paste(sort(c(as.character(p1_id), as.character(p2_id))), collapse = "_")
      from_books <- book_odds_lookup[[key]]
      best_odds_p1 <- best_odds_p2 <- NA_real_
      if (is.list(from_books) && length(from_books) > 0) {
        if (is.numeric(from_books$p1)) {
          best_odds_p1 <- max(safe_num(from_books$p1), na.rm = TRUE)
          best_odds_p2 <- max(safe_num(from_books$p2), na.rm = TRUE)
        } else {
          for (eb in from_books) {
            if (is.list(eb) && !is.null(eb$p1)) {
              v1 <- safe_num(eb$p1); v2 <- safe_num(eb$p2)
              if (is.finite(v1)) best_odds_p1 <- max(c(best_odds_p1, v1), na.rm = TRUE)
              if (is.finite(v2)) best_odds_p2 <- max(c(best_odds_p2, v2), na.rm = TRUE)
            }
          }
        }
      }
      odds <- get_m(m, "odds")
      get_p <- function(o, name) {
        if (is.null(o)) return(NA_real_)
        if (is.list(o)) return(safe_num(o[[name]]))
        if (length(o) == 1L) return(safe_num(o))
        safe_num(o[name])
      }
      nm <- names(m)
      for (col in nm) {
        vals <- safe_num(m[[col]])
        if (length(vals) == 0) next
        vals <- vals[is.finite(vals)]
        if (length(vals) == 0) next
        vals <- vals[vals >= 1]
        if (length(vals) == 0) next
        val <- max(vals, na.rm = TRUE)
        if (!is.finite(val) || val < 1) next
        if (grepl("p1|player_1", col, ignore.case = TRUE) && (grepl("odds|close|open|decimal", col, ignore.case = TRUE) || grepl("\\.p1$", col)))
          best_odds_p1 <- max(best_odds_p1, val, na.rm = TRUE)
        if (grepl("p2|player_2", col, ignore.case = TRUE) && (grepl("odds|close|open|decimal", col, ignore.case = TRUE) || grepl("\\.p2$", col)))
          best_odds_p2 <- max(best_odds_p2, val, na.rm = TRUE)
      }
      if (length(nm) > 0) {
        p1_cols <- nm[grepl("\\.p1$|p1_odds|player_1|p1_close|p1_open", nm)]
        p2_cols <- nm[grepl("\\.p2$|p2_odds|player_2|p2_close|p2_open", nm)]
        if (length(p1_cols)) best_odds_p1 <- max(c(best_odds_p1, safe_num(m[p1_cols])), na.rm = TRUE)
        if (length(p2_cols)) best_odds_p2 <- max(c(best_odds_p2, safe_num(m[p2_cols])), na.rm = TRUE)
      }
      if (!is.null(odds)) {
        if (is.data.frame(odds) && nrow(odds) > 0) {
          if (all(c("p1", "p2") %in% names(odds))) {
            best_odds_p1 <- max(c(best_odds_p1, safe_num(odds$p1)), na.rm = TRUE)
            best_odds_p2 <- max(c(best_odds_p2, safe_num(odds$p2)), na.rm = TRUE)
          } else {
            for (j in seq_len(nrow(odds))) {
              row <- as.list(odds[j, ])
              o1 <- safe_num(row[[1]] %||% row$p1 %||% row$player_1)
              o2 <- safe_num(row[[2]] %||% row$p2 %||% row$player_2)
              if (is.finite(o1)) best_odds_p1 <- max(best_odds_p1, o1, na.rm = TRUE)
              if (is.finite(o2)) best_odds_p2 <- max(best_odds_p2, o2, na.rm = TRUE)
            }
          }
        } else if (is.list(odds)) {
          for (b in names(odds)) {
            ob <- odds[[b]]
            if (is.null(ob)) next
            o1 <- get_p(ob, "p1"); o2 <- get_p(ob, "p2")
            if (length(ob) >= 2 && !is.list(ob)) { o1 <- safe_num(ob[1]); o2 <- safe_num(ob[2]) }
            if (is.finite(o1)) best_odds_p1 <- max(best_odds_p1, o1, na.rm = TRUE)
            if (is.finite(o2)) best_odds_p2 <- max(best_odds_p2, o2, na.rm = TRUE)
          }
        }
      }
      if (!is.finite(best_odds_p1)) best_odds_p1 <- NA_real_
      if (!is.finite(best_odds_p2)) best_odds_p2 <- NA_real_
      if (is.finite(fair1) && !is.finite(best_odds_p1)) best_odds_p1 <- fair1
      if (is.finite(fair2) && !is.finite(best_odds_p2)) best_odds_p2 <- fair2
      model_margin <- 1.07
      model_price_p1 <- if (is.finite(fair1)) fair1 / model_margin else NA_real_
      model_price_p2 <- if (is.finite(fair2)) fair2 / model_margin else NA_real_
      model_p1_am <- format_american(decimal_to_american(model_price_p1))
      model_p2_am <- format_american(decimal_to_american(model_price_p2))
      p1_disp <- name_display(if (is.na(p1_name) || !nzchar(p1_name)) "—" else p1_name)
      p2_disp <- name_display(if (is.na(p2_name) || !nzchar(p2_name)) "—" else p2_name)
      books_for_key <- book_odds_lookup[[key]]
      if (is.null(books_for_key) || length(books_for_key) == 0) {
        od <- get_m(m, "odds")
        if (is.list(od) && length(names(od)) > 0) {
          books_for_key <- lapply(names(od), function(bname) {
            ob <- od[[bname]]
            p1 <- safe_num(ob$p1 %||% ob[1]); p2 <- safe_num(ob$p2 %||% ob[2])
            if (length(ob) >= 2 && !is.list(ob)) { p1 <- safe_num(ob[1]); p2 <- safe_num(ob[2]) }
            list(book = if (is.null(bname) || !nzchar(trimws(bname))) "" else bname, p1 = p1, p2 = p2)
          })
          books_for_key <- books_for_key[vapply(books_for_key, function(x) nzchar(trimws(x$book %||% "")) && tolower(trimws(x$book %||% "")) != "datagolf", logical(1))]
        } else {
          books_for_key <- list()
        }
      }
      best_ev <- -Inf
      best_ev1 <- best_ev2 <- NA_real_
      best_book_name <- ""
      best_bp1 <- best_odds_p1; best_bp2 <- best_odds_p2
      for (bi in seq_along(books_for_key)) {
        bk <- books_for_key[[bi]]
        if (tolower(trimws(as.character(bk$book %||% ""))) == "datagolf") next
        bp1 <- safe_num(bk$p1); bp2 <- safe_num(bk$p2)
        if (!any(is.finite(c(bp1, bp2)))) { bp1 <- best_odds_p1; bp2 <- best_odds_p2 }
        nv <- no_vig_fair_decimal(bp1, bp2)
        ev1 <- if (is.finite(model_price_p1) && is.finite(nv$fair1) && nv$fair1 > 0) ((model_price_p1 / nv$fair1) - 1) * 100 else NA_real_
        ev2 <- if (is.finite(model_price_p2) && is.finite(nv$fair2) && nv$fair2 > 0) ((model_price_p2 / nv$fair2) - 1) * 100 else NA_real_
        ev_candidates <- c(ev1, ev2)
        cand_ev <- if (any(is.finite(ev_candidates))) max(ev_candidates, na.rm = TRUE) else NA_real_
        if (is.finite(cand_ev) && cand_ev > best_ev) {
          best_ev <- cand_ev
          best_ev1 <- ev1; best_ev2 <- ev2
          best_book_name <- as.character(trimws(bk$book %||% ""))
          if (!nzchar(best_book_name)) best_book_name <- "—"
          best_bp1 <- bp1; best_bp2 <- bp2
        }
      }
      if (!nzchar(best_book_name) && is.list(books_for_key) && length(books_for_key) > 0) {
        for (bk in books_for_key) {
          if (tolower(trimws(as.character(bk$book %||% ""))) != "datagolf" && nzchar(trimws(as.character(bk$book %||% "")))) {
            best_book_name <- as.character(trimws(bk$book))
            break
          }
        }
        if (!nzchar(best_book_name)) best_book_name <- "—"
      } else if (!nzchar(best_book_name)) best_book_name <- "—"
      if (!is.finite(best_ev)) {
        nv_fallback <- no_vig_fair_decimal(best_odds_p1, best_odds_p2)
        best_ev1 <- if (is.finite(model_price_p1) && is.finite(nv_fallback$fair1) && nv_fallback$fair1 > 0) ((model_price_p1 / nv_fallback$fair1) - 1) * 100 else NA_real_
        best_ev2 <- if (is.finite(model_price_p2) && is.finite(nv_fallback$fair2) && nv_fallback$fair2 > 0) ((model_price_p2 / nv_fallback$fair2) - 1) * 100 else NA_real_
      }
      nv_best <- no_vig_fair_decimal(best_bp1, best_bp2)
      fair_p1_am <- format_american(decimal_to_american(nv_best$fair1))
      fair_p2_am <- format_american(decimal_to_american(nv_best$fair2))
      model_prob_p1 <- if (is.finite(fair1) && fair1 > 0) 100 / fair1 else NA_real_
      model_prob_p2 <- if (is.finite(fair2) && fair2 > 0) 100 / fair2 else NA_real_
      market_prob_p1 <- if (is.finite(nv_best$fair1) && nv_best$fair1 > 0) 100 / nv_best$fair1 else NA_real_
      market_prob_p2 <- if (is.finite(nv_best$fair2) && nv_best$fair2 > 0) 100 / nv_best$fair2 else NA_real_
      edge_p1 <- if (is.finite(model_prob_p1) && is.finite(market_prob_p1)) model_prob_p1 - market_prob_p1 else NA_real_
      edge_p2 <- if (is.finite(model_prob_p2) && is.finite(market_prob_p2)) model_prob_p2 - market_prob_p2 else NA_real_
      ev1_str <- if (is.finite(best_ev1)) paste0(if (best_ev1 >= 0) "+" else "", round(best_ev1, 1), "%") else "—"
      ev2_str <- if (is.finite(best_ev2)) paste0(if (best_ev2 >= 0) "+" else "", round(best_ev2, 1), "%") else "—"
      edge1_str <- if (is.finite(edge_p1)) paste0(if (edge_p1 >= 0) "+" else "", round(edge_p1, 1), "pp") else "—"
      edge2_str <- if (is.finite(edge_p2)) paste0(if (edge_p2 >= 0) "+" else "", round(edge_p2, 1), "pp") else "—"
      matchup_str <- paste(p1_disp, "vs", p2_disp)
      best_book_display <- if (best_book_name == "—" || !nzchar(best_book_name)) "—" else book_cap(best_book_name)
      best_side <- if (is.finite(best_ev1) && is.finite(best_ev2)) {
        if (best_ev1 >= best_ev2) 1L else 2L
      } else if (is.finite(best_ev1)) {
        1L
      } else if (is.finite(best_ev2)) {
        2L
      } else {
        NA_integer_
      }
      best_book_odds <- if (identical(best_side, 1L)) best_bp1 else if (identical(best_side, 2L)) best_bp2 else NA_real_
      best_book_cell <- if (best_book_name == "—" || !nzchar(best_book_name)) {
        "—"
      } else if (is.finite(best_book_odds) && best_book_odds > 1) {
        paste0(book_logo_html(best_book_display), " ", format_american(decimal_to_american(best_book_odds)))
      } else {
        book_logo_html(best_book_display)
      }
      # One row per matchup (outrights format): pick = side with higher EV
      pick_side <- if (is.finite(best_ev1) && is.finite(best_ev2) && best_ev2 > best_ev1) 2L else 1L
      pick_name <- if (pick_side == 1L) p1_disp else p2_disp
      model_decimal <- if (pick_side == 1L) model_price_p1 else model_price_p2
      model_am <- if (is.finite(model_decimal)) decimal_to_american(model_decimal) else NA_real_
      ev_val <- if (pick_side == 1L) best_ev1 else best_ev2
      ev_display <- if (is.finite(ev_val)) paste0(if (ev_val > 0) "<span style=\"color:#22c55e;font-weight:bold\">" else "<span style=\"color:#ef4444\">", if (ev_val > 0) "+" else "", round(ev_val, 1), "%</span>") else "—"
      row <- list(Matchup = matchup_str, Player = pick_name, Model = model_am, EV = ev_display, `Best Book` = best_book_cell, EV_num = ev_val)
      for (bk in all_books) {
        odds_pick <- NA_real_
        for (eb in books_for_key) {
          if (trimws(as.character(eb$book %||% "")) == bk) {
            odds_pick <- if (pick_side == 1L) safe_num(eb$p1) else safe_num(eb$p2)
            break
          }
        }
        row[[bk]] <- if (is.finite(odds_pick) && odds_pick > 1) decimal_to_american(odds_pick) else NA_real_
      }
      out[[i]] <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    }
    df <- dplyr::bind_rows(out)
    # Sort by EV best to worst (descending)
    if (nrow(df) > 0 && "EV_num" %in% names(df)) {
      df <- df %>% dplyr::arrange(dplyr::desc(EV_num))
    }
    if (nrow(df) > 0 && length(all_books) > 0) {
      for (b in all_books) if (!b %in% names(df)) df[[b]] <- NA_real_
    }
    df
  })

  # Book display map for matchups table (same as outrights)
  matchups_book_display_map <- c(datagolf = "DataGolf", draftkings = "DraftKings", fanduel = "FanDuel", betmgm = "BetMGM", caesars = "Caesars", pinnacle = "Pinnacle", bovada = "Bovada", bet365 = "Bet365", betonline = "BetOnline", betway = "Betway", unibet = "Unibet", williamhill = "William Hill", circa = "Circa", pointsbet = "PointsBet", betcris = "BetCRIS")

  output$round_matchups_table <- renderDT({
    df <- round_matchups_df()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "No round matchups found, or select a round to load model projections."),
        rownames = FALSE
      ))
    }
    ncols <- ncol(df)
    names(df)[names(df) == ""] <- " "
    book_keys <- setdiff(names(df), c("Matchup", "Player", "Model", "EV", "Best Book", "EV_num"))
    for (b in book_keys) {
      disp <- matchups_book_display_map[tolower(b)]
      if (!is.na(disp) && nzchar(disp)) names(df)[names(df) == b] <- disp
    }
    # Same header as outrights: text for Matchup/Player/Model/EV/Best Book; book logos for book columns
    thead_cells <- lapply(names(df), function(nm) {
      badge <- book_badge(nm)
      favicon_url <- book_favicon_url(nm)
      slug <- book_logo_slug(nm)
      if (is.null(slug) && is.null(favicon_url)) {
        return(tags$th(nm))
      }
      if (badge != nm && (!is.null(favicon_url) || !is.null(slug))) {
        local_src <- if (!is.null(slug)) paste0("logos/", slug, ".png") else NULL
        img_src <- local_src %||% favicon_url
        onerr_js <- if (!is.null(local_src) && !is.null(favicon_url))
          paste0("if(this.getAttribute('data-tried-favicon')!=='1'){ this.setAttribute('data-tried-favicon','1'); this.src='", favicon_url, "'; } else { this.style.display='none'; var s=this.nextElementSibling; if(s) s.style.display='inline-block'; }")
        else
          "this.style.display='none'; var s=this.nextElementSibling; if(s) s.style.display='inline-block';"
        tags$th(
          style = "text-align: center;",
          tags$span(
            style = "display: inline-flex; align-items: center; justify-content: center; min-height: 28px;",
            tags$img(src = img_src, alt = nm, title = nm, style = "height: 24px; width: auto; max-width: 64px; object-fit: contain; vertical-align: middle;", onerror = onerr_js),
            tags$span(class = "book-badge", title = nm, style = "display: none;", badge)
          )
        )
      } else {
        tags$th(nm)
      }
    })
    container <- tags$table(class = "display", tags$thead(tags$tr(thead_cells)))
    ev_col_idx <- which(names(df) == "EV") - 1L
    best_col_idx <- which(names(df) %in% c("Best Book", "Best.Book")) - 1L
    ev_num_col_idx <- which(names(df) == "EV_num") - 1L
    # Columns that contain HTML and must not be escaped (handle "Best.Book" from as.data.frame)
    no_escape_idx <- which(names(df) %in% c("Best Book", "Best.Book", "EV"))
    col_defs <- list(
      if (length(best_col_idx) > 0 && best_col_idx >= 0) list(targets = best_col_idx, orderable = FALSE),
      if (length(ev_col_idx) > 0 && ev_col_idx >= 0) list(targets = ev_col_idx, orderable = FALSE),
      if (length(ev_num_col_idx) > 0 && ev_num_col_idx >= 0) list(targets = ev_num_col_idx, visible = FALSE)
    )
    col_defs <- c(col_defs, list(list(
      targets = setdiff(seq_len(ncols) - 1L, c(ev_col_idx, best_col_idx, ev_num_col_idx)),
      render = DT::JS(
        "function(data, type, row, meta) {",
        " if (type !== 'display') return data;",
        " if (data == null || data === '' || String(data) === 'NA') return '—';",
        " var n = Math.round(Number(data));",
        " if (isNaN(n)) return data;",
        " return n > 0 ? '+' + n : String(n);",
        "}"
      )
    )))
    col_defs <- col_defs[!sapply(col_defs, is.null)]
    dt <- datatable(
      df,
      rownames = FALSE,
      container = container,
      class = "stripe hover",
      escape = setdiff(seq_len(ncols), no_escape_idx),
      options = list(
        pageLength = -1,
        lengthChange = FALSE,
        dom = "t",
        order = list(list(if (length(ev_num_col_idx) > 0) ev_num_col_idx else 0L, "desc")),
        columnDefs = col_defs
      )
    )
    dt
  })

  # ---- Outrights: model prices vs sportsbooks (American odds) ----
  output$outrights_event_info <- renderUI({
    info <- field_updates_event_info()
    event_name <- trimws(as.character(info$event_name %||% ""))
    course_name <- trimws(as.character(info$course_name %||% ""))
    date_start <- trimws(as.character(info$date_start %||% ""))
    if (!nzchar(event_name) && !nzchar(course_name)) return(NULL)
    line <- event_name
    if (nzchar(course_name)) line <- paste0(line, if (nzchar(line)) " \u00b7 " else "", course_name)
    if (!nzchar(line)) return(NULL)
    not_started <- FALSE
    if (nzchar(date_start)) {
      start_date <- tryCatch(as.Date(date_start), error = function(e) as.Date(NA))
      if (!is.na(start_date) && start_date > Sys.Date()) not_started <- TRUE
    }
    tagList(
      tags$p(style = "margin-bottom: 0.25em; font-weight: 500;", "Tournament: ", tags$span(line)),
      if (not_started) tags$p(style = "margin-bottom: 0.5em; font-size: 0.9em; color: #94a3b8;", "Pre-tournament odds — event has not started.")
    )
  })
  pct_to_american <- function(pct) {
    out <- rep(NA_real_, length(pct))
    pct <- pmin(99, pmax(0.1, as.numeric(pct)))
    ok <- is.finite(pct) & pct > 0
    p <- pct / 100
    ok_fav <- ok & p >= 0.5
    ok_dog <- ok & p < 0.5
    out[ok_fav] <- -100 * p[ok_fav] / (1 - p[ok_fav])
    out[ok_dog] <- 100 * (1 - p[ok_dog]) / p[ok_dog]
    out[ok_fav] <- pmax(-10000, out[ok_fav])
    out[ok_dog] <- pmin(10000, out[ok_dog])
    out
  }
  # Cap model favorite odds at -500 so display isn't extreme
  model_to_american <- function(pct) {
    x <- pct_to_american(pct)
    x[is.finite(x) & x < -500] <- -500
    x
  }
  observe({
    tab <- input$main_tabs
    mkt <- input$outright_market
    if (is.null(tab) || !identical(tab, "Outrights") || !has_text(mkt)) return()
    cache <- outrights_cache()
    if (is.list(cache) && identical(cache[[mkt]], "loading")) return()
    if (is.list(cache) && !is.null(cache[[mkt]])) return()
    cache[[mkt]] <- "loading"
    outrights_cache(cache)
    tryCatch({
      r <- httr::GET(
        "https://feeds.datagolf.com/betting-tools/outrights",
        query = list(tour = "pga", market = mkt, odds_format = "percent", file_format = "json", key = datagolf_api_key)
      )
      if (httr::status_code(r) != 200) { cache[[mkt]] <- NULL; outrights_cache(cache); return() }
      raw <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = FALSE)
      cache[[mkt]] <- raw
      outrights_cache(cache)
    }, error = function(e) { cache[[mkt]] <- NULL; outrights_cache(cache); message("Outrights fetch: ", conditionMessage(e)) })
  })

  fetch_pre_tournament_outrights <- function() {
    tryCatch({
      r <- httr::GET(
        "https://feeds.datagolf.com/preds/pre-tournament",
        query = list(tour = "pga", dead_heat = "no", odds_format = "percent", file_format = "json", key = datagolf_api_key)
      )
      if (httr::status_code(r) != 200) return(NULL)
      dat <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = TRUE)
      bl <- NULL
      if ("baseline_history_fit" %in% names(dat) && is.data.frame(dat$baseline_history_fit) && nrow(dat$baseline_history_fit) > 0)
        bl <- dat$baseline_history_fit
      else if ("baseline" %in% names(dat) && is.data.frame(dat$baseline) && nrow(dat$baseline) > 0)
        bl <- dat$baseline
      if (is.null(bl) || nrow(bl) == 0) return(NULL)
      if (!"dg_id" %in% names(bl) && "id" %in% names(bl)) bl$dg_id <- bl$id
      bl <- bl %>% dplyr::mutate(
        win = as.numeric(win %||% 0), top_5 = as.numeric(top_5 %||% 0), top_10 = as.numeric(top_10 %||% 0),
        top_20 = as.numeric(top_20 %||% 0), make_cut = as.numeric(make_cut %||% 0)
      )
      if (any(is.finite(bl$win) & bl$win > 1.5)) bl <- bl %>% dplyr::mutate(win = win / 100, top_5 = top_5 / 100, top_10 = top_10 / 100, top_20 = top_20 / 100, make_cut = make_cut / 100)
      else bl <- bl %>% dplyr::mutate(win = win / 100, top_5 = top_5 / 100, top_10 = top_10 / 100, top_20 = top_20 / 100, make_cut = make_cut / 100)
      bl
    }, error = function(e) { message("Pre-tournament fetch: ", conditionMessage(e)); NULL })
  }

  outrights_table_data <- reactive({
    mkt <- input$outright_market
    if (!has_text(mkt)) return(NULL)
    model_col <- switch(mkt, win = "win", top_5 = "top_5", top_10 = "top_10", top_20 = "top_20", make_cut = "make_cut", mc = "make_cut", NA_character_)
    if (is.na(model_col)) return(NULL)
    cache <- outrights_cache()
    raw <- if (is.list(cache)) cache[[mkt]] else NULL
    rs <- simulated_round_data()
    # Use projection table from cache file if not yet in memory (so MODEL works for all markets)
    if (is.null(rs) || nrow(rs) == 0 || !model_col %in% names(rs)) {
      cache_path <- file.path(model_dir, "simulated_round_cache.rds")
      if (file.exists(cache_path)) {
        cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
        if (!is.null(cached)) {
          tbl <- if (is.list(cached) && "data" %in% names(cached)) cached$data else cached
          if (is.data.frame(tbl) && nrow(tbl) > 0 && model_col %in% names(tbl)) rs <- tbl
        }
      }
    }
    .build_model_pct <- function() {
      out <- NULL
      if (!is.null(rs) && nrow(rs) > 0 && model_col %in% names(rs)) {
        round_val <- display_round_num()
        if (!is.finite(round_val) || round_val < 1L || round_val > 4L) round_val <- 1L
        one <- if ("round" %in% names(rs)) rs %>% dplyr::filter(round == round_val) %>% dplyr::distinct(dg_id, .keep_all = TRUE) else rs %>% dplyr::group_by(dg_id, player_name) %>% dplyr::slice(1L) %>% dplyr::ungroup()
        if (nrow(one) > 0) {
          p <- as.numeric(one[[model_col]])
          if (mkt == "mc") p <- 1 - p
          if (any(is.finite(p) & p > 0)) {
            if (any(is.finite(p) & p > 1.5)) p <- p / 100
            out <- tibble(dg_id = one$dg_id, player_name = as.character(one$player_name), `Model %` = pmax(0.01, p * 100))
          }
        }
      }
      if (is.null(out) || nrow(out) == 0) {
        pt <- pre_tournament_outrights()
        if (is.null(pt)) { pt <- fetch_pre_tournament_outrights(); if (!is.null(pt) && nrow(pt) > 0) pre_tournament_outrights(pt) }
        if (!is.null(pt) && nrow(pt) > 0 && model_col %in% names(pt)) {
          p <- as.numeric(pt[[model_col]])
          if (mkt == "mc") p <- 1 - p
          if (any(is.finite(p) & p > 1.5)) p <- p / 100
          p <- pmax(0.01, p * 100)
          if (any(is.finite(p) & p > 0)) out <- tibble(dg_id = pt$dg_id, player_name = as.character(pt$player_name %||% pt$name), `Model %` = p)
        }
      }
      out
    }
    # Keep your pipeline probabilities as-is (no market-wide renormalization);
    # just clamp to a sensible range so odds don't explode.
    .norm_model_pct <- function(tbl) {
      if (is.null(tbl) || nrow(tbl) == 0 || !"Model %" %in% names(tbl)) return(tbl)
      tbl$`Model %` <- pmin(99, pmax(0.1, as.numeric(tbl$`Model %`)))
      tbl
    }
    if (is.null(raw) || identical(raw, "loading")) {
      model_pct <- .build_model_pct()
      if (!is.null(model_pct) && nrow(model_pct) > 0)
        return(.norm_model_pct(model_pct) %>% dplyr::mutate(Player = name_display(player_name), Model = pct_to_american(`Model %`)) %>% dplyr::select(Player, Model) %>% dplyr::arrange(Model))
      return(NULL)
    }
    odds_arr <- raw$odds %||% raw$data %||% raw$field %||% raw$players %||%
      raw$baseline %||% raw$baseline_history_fit %||% raw
    if (is.data.frame(odds_arr)) odds_arr <- lapply(seq_len(nrow(odds_arr)), function(i) as.list(odds_arr[i, ]))
    book_display_map <- c(datagolf = "DataGolf", draftkings = "DraftKings", fanduel = "FanDuel", betmgm = "BetMGM", caesars = "Caesars", pinnacle = "Pinnacle", bovada = "Bovada", bet365 = "Bet365", betonline = "BetOnline", betway = "Betway", unibet = "Unibet", williamhill = "William Hill", circa = "Circa", pointsbet = "PointsBet", betcris = "BetCRIS")
    model_pct <- .build_model_pct()
    if (is.null(model_pct) || nrow(model_pct) == 0) return(NULL)
    model_pct <- .norm_model_pct(model_pct)
    api_df <- NULL
    if (is.list(odds_arr) && length(odds_arr) > 0) {
      rows <- lapply(odds_arr, function(row) {
        if (!is.list(row)) return(NULL)
        dg_id <- as.integer(row$dg_id %||% row$id %||% NA_integer_)
        pname <- as.character(row$player_name %||% row$name %||% "")
        out <- list(dg_id = dg_id, player_name = pname)
        for (k in names(row)) {
          if (k %in% c("dg_id", "id", "player_name", "name")) next
          val <- row[[k]]
          if (is.list(val)) val <- if (length(val) > 0) val[[1]] else NA_real_
          v <- suppressWarnings(as.numeric(val %||% NA_real_))
          if (is.finite(v) && v > 1) v <- v / 100
          if (is.finite(v)) out[[tolower(k)]] <- v * 100
        }
        out
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) {
        all_keys <- unique(unlist(lapply(rows, names)))
        book_keys <- setdiff(all_keys, c("dg_id", "player_name", "id", "name"))
        api_df <- dplyr::bind_rows(lapply(rows, function(r) {
          x <- as.data.frame(as.list(r), stringsAsFactors = FALSE)
          for (b in book_keys) if (!b %in% names(x)) x[[b]] <- NA_real_
          x
        }))
        api_df$dg_id <- as.integer(api_df$dg_id)
      }
    }
    if (is.null(api_df) || nrow(api_df) == 0) {
      return(model_pct %>% dplyr::mutate(Player = name_display(player_name), Model = pct_to_american(`Model %`), EV = NA_real_, `Best Book` = "—") %>% dplyr::select(Player, Model, EV, `Best Book`) %>% dplyr::arrange(Model))
    }
    df <- model_pct %>% dplyr::left_join(api_df, by = c("dg_id", "player_name"))
    df$dg_id <- as.integer(df$dg_id)

    # Raw probability to percent: accept decimal (0.15) or percent (15); no cap so we can normalize like a sportsbook
    to_model_pct_raw <- function(p) {
      p <- as.numeric(p)
      if (any(is.finite(p) & p > 1.5)) p <- p / 100
      pmax(0.01, p * 100)
    }

    # Primary: user's pipeline (rs already set above, possibly from cache) so MODEL reflects your projections
    if (!is.null(rs) && nrow(rs) > 0 && model_col %in% names(rs)) {
      round_val <- display_round_num()
      if (!is.finite(round_val) || round_val < 1L || round_val > 4L) round_val <- 1L
      one <- if ("round" %in% names(rs)) rs %>% dplyr::filter(round == round_val) %>% dplyr::distinct(dg_id, .keep_all = TRUE) else rs %>% dplyr::group_by(dg_id, player_name) %>% dplyr::slice(1L) %>% dplyr::ungroup()
      if (nrow(one) > 0) {
        p <- as.numeric(one[[model_col]])
        if (mkt == "mc") p <- 1 - p
        p <- to_model_pct_raw(p)
        pipe_lookup <- tibble(dg_id = as.integer(one$dg_id), pipe_pct = p)
        df <- df %>% dplyr::left_join(pipe_lookup, by = "dg_id") %>%
          dplyr::mutate(`Model %` = dplyr::coalesce(pipe_pct, `Model %`)) %>% dplyr::select(-dplyr::any_of("pipe_pct"))
        # Name-based fill for rows that didn't match by dg_id (e.g. API vs pipeline ID mismatch)
        need_pipe_name <- (is.na(df$`Model %`) | !is.finite(df$`Model %`)) & nzchar(df$player_name)
        if (any(need_pipe_name)) {
          pipe_name_key <- function(z) tolower(trimws(gsub("[^a-z0-9 ]", "", gsub(",", " ", as.character(z)))))
          one_key <- pipe_name_key(one$player_name)
          one_pct <- as.numeric(one[[model_col]])
          if (mkt == "mc") one_pct <- 1 - one_pct
          one_pct <- to_model_pct_raw(one_pct)
          pipe_by_name <- tibble(.name_key = one_key, pipe_pct = one_pct) %>%
            dplyr::filter(nzchar(.name_key)) %>% dplyr::distinct(.name_key, .keep_all = TRUE)
          df_key <- pipe_name_key(df$player_name)
          idx <- need_pipe_name & nzchar(df_key)
          if (any(idx) && nrow(pipe_by_name) > 0) {
            match_pct <- pipe_by_name$pipe_pct[match(df_key[idx], pipe_by_name$.name_key)]
            df$`Model %`[idx] <- dplyr::coalesce(match_pct, df$`Model %`[idx])
          }
        }
      }
    }

    # Fallback: pre-tournament (DataGolf) for rows still missing Model %
    pt <- pre_tournament_outrights()
    if (is.null(pt)) { pt <- fetch_pre_tournament_outrights(); if (!is.null(pt) && nrow(pt) > 0) pre_tournament_outrights(pt) }
    need_pt <- is.na(df$`Model %`) | !is.finite(df$`Model %`)
    if (any(need_pt) && !is.null(pt) && nrow(pt) > 0 && model_col %in% names(pt)) {
      pt_dg_id <- as.integer(pt$dg_id %||% pt$id)
      p <- as.numeric(pt[[model_col]])
      if (mkt == "mc") p <- 1 - p
      p <- to_model_pct_raw(p)
      pt_lookup <- tibble(dg_id = pt_dg_id, model_pct_pt = p)
      pt_lookup <- pt_lookup %>% dplyr::filter(is.finite(dg_id)) %>% dplyr::distinct(dg_id, .keep_all = TRUE)
      df <- df %>% dplyr::left_join(pt_lookup, by = "dg_id") %>%
        dplyr::mutate(`Model %` = dplyr::coalesce(`Model %`, model_pct_pt)) %>% dplyr::select(-dplyr::any_of("model_pct_pt"))
      need_name_fill <- (is.na(df$`Model %`) | !is.finite(df$`Model %`)) & nzchar(df$player_name)
      if (any(need_name_fill)) {
        name_key <- function(z) tolower(trimws(gsub("[^a-z0-9 ]", "", gsub(",", " ", as.character(z)))))
        pt_name_key <- name_key(pt$player_name %||% pt$name)
        p_pt <- as.numeric(pt[[model_col]])
        if (mkt == "mc") p_pt <- 1 - p_pt
        p_pt <- to_model_pct_raw(p_pt)
        pt_by_name <- tibble(.name_key = pt_name_key, model_pct_pt = p_pt)
        pt_by_name <- pt_by_name %>% dplyr::filter(nzchar(.name_key)) %>% dplyr::distinct(.name_key, .keep_all = TRUE)
        df_keys <- name_key(df$player_name)
        idx <- need_name_fill & nzchar(df_keys)
        if (any(idx) && nrow(pt_by_name) > 0) {
          match_pct <- pt_by_name$model_pct_pt[match(df_keys[idx], pt_by_name$.name_key)]
          df$`Model %`[idx] <- dplyr::coalesce(match_pct, df$`Model %`[idx])
        }
      }
    }
    if (!"Model %" %in% names(df)) df$`Model %` <- NA_real_
    n_players <- nrow(df)
    model_pct <- as.numeric(df$`Model %`)
    target_sum_pct <- switch(mkt,
      win = 100, top_5 = 500, top_10 = 1000, top_20 = 2000,
      make_cut = 6500, mc = 100 * max(0, n_players - 65), 100
    )
    # Market consensus: per-player average of fair implied % across books (excl. DataGolf).
    book_cols_for_consensus <- setdiff(intersect(names(df), book_keys), "datagolf")
    market_consensus <- NULL
    if (length(book_cols_for_consensus) > 0) {
      fair_list <- lapply(book_cols_for_consensus, function(bk) {
        bvec <- as.numeric(df[[bk]])
        # Make cut / miss cut: book odds are already per-player probabilities (0–100); do not normalize to sum 100
        if (mkt %in% c("make_cut", "mc")) {
          return(pmin(99, pmax(0.1, bvec)))
        }
        bsum <- sum(bvec, na.rm = TRUE)
        if (!is.finite(bsum) || bsum < 50 || bsum > 1e5) return(rep(NA_real_, n_players))
        (bvec / bsum) * 100
      })
      fair_mat <- do.call(cbind, fair_list)
      market_consensus <- rowMeans(fair_mat, na.rm = TRUE)
      market_consensus[!is.finite(market_consensus)] <- NA_real_
      market_consensus <- pmin(99, pmax(0.1, market_consensus))
    }
    # Bayesian blend: posterior = (model * tau_model + market * tau_market) / (tau_model + tau_market).
    tau_model <- 1
    tau_market <- 1
    if (!is.null(market_consensus) && length(market_consensus) == n_players) {
      blended <- (model_pct * tau_model + market_consensus * tau_market) / (tau_model + tau_market)
      blended <- ifelse(is.finite(market_consensus), blended, model_pct)
      blended <- pmin(99, pmax(0.1, blended))
      if (mkt %in% c("make_cut", "mc")) {
        df$`Model %` <- blended
      } else {
        bsum <- sum(blended, na.rm = TRUE)
        if (is.finite(bsum) && bsum > 0) blended <- blended * (target_sum_pct / bsum)
        df$`Model %` <- pmin(99, pmax(0.1, blended))
      }
    } else {
      if (mkt %in% c("make_cut", "mc")) {
        mean_mc <- mean(model_pct, na.rm = TRUE)
        if (is.finite(mean_mc) && mean_mc >= 35 && mean_mc <= 65 && n_players >= 2L) {
          o <- order(-model_pct, na.last = TRUE)
          rank_mc <- match(seq_len(n_players), o)
          p_make <- 0.95 - 0.9 * (rank_mc - 1) / max(1, n_players - 1)
          p_make <- p_make * (6500 / sum(p_make, na.rm = TRUE))
          df$`Model %` <- pmin(99, pmax(0.1, p_make))
        } else {
          df$`Model %` <- pmin(99, pmax(0.1, model_pct))
        }
      } else {
        pct_sum <- sum(model_pct, na.rm = TRUE)
        if (is.finite(pct_sum) && pct_sum > 0 && is.finite(target_sum_pct) && target_sum_pct > 0) {
          p <- pmin(0.99, pmax(0.001, model_pct / 100))
          p <- p * (target_sum_pct / 100) / sum(p, na.rm = TRUE)
          df$`Model %` <- pmin(99, pmax(0.1, p * 100))
        }
      }
    }
    # Best book (highest American odds per row), excluding DataGolf; EV = (model_prob * decimal_odds - 1) * 100
    book_cols <- intersect(names(df), book_keys)
    book_cols_best <- book_cols[tolower(book_cols) != "datagolf"]
    if (length(book_cols_best) == 0) book_cols_best <- book_cols
    book_american <- if (length(book_cols_best) > 0) as.data.frame(lapply(df[book_cols_best], function(x) pct_to_american(as.numeric(x))), optional = TRUE) else data.frame()
    if (length(book_cols_best) > 0 && ncol(book_american) > 0 && nrow(book_american) > 0) {
      book_american[!is.finite(as.matrix(book_american))] <- -Inf
      best_american <- do.call(pmax, c(book_american, list(na.rm = TRUE)))
      best_idx <- max.col(as.matrix(book_american), ties.method = "first")
      best_book_key <- book_cols_best[pmin(best_idx, length(book_cols_best))]
      best_american_clean <- replace(best_american, best_american == -Inf | !is.finite(best_american), NA_real_)
      american_to_decimal <- function(am) ifelse(is.finite(am) & am > 0, 1 + am / 100, ifelse(is.finite(am) & am < 0, 1 + 100 / abs(am), NA_real_))
      ev_pct <- (as.numeric(df$`Model %`) / 100 * american_to_decimal(best_american_clean) - 1) * 100
      ev_display <- vapply(ev_pct, function(e) {
        if (!is.finite(e)) return("—")
        pct <- paste0(if (e > 0) "+" else "", round(e, 1), "%")
        if (e > 0) paste0("<span style=\"color:#22c55e;font-weight:bold\">", pct, "</span>") else paste0("<span style=\"color:#ef4444\">", pct, "</span>")
      }, character(1))
      format_am <- function(am) ifelse(!is.finite(am), "—", paste0(if (am > 0) "+" else "", round(am)))
      Best_Book_HTML <- vapply(seq_len(nrow(df)), function(i) {
        k <- best_book_key[i]
        disp <- if (length(book_display_map) > 0 && tolower(k) %in% names(book_display_map)) book_display_map[tolower(k)] else k
        paste0(book_logo_html(disp %||% k), " ", format_am(best_american_clean[i]))
      }, character(1))
    } else {
      ev_pct <- rep(NA_real_, nrow(df))
      ev_display <- rep("—", nrow(df))
      Best_Book_HTML <- rep("—", nrow(df))
      best_american_clean <- rep(NA_real_, nrow(df))
    }
    df <- df %>% dplyr::mutate(
      Player = name_display(player_name),
      Model = model_to_american(`Model %`),
      EV = ev_pct,
      EV_display = ev_display,
      `Best Book` = Best_Book_HTML,
      dplyr::across(dplyr::any_of(book_keys), ~ pct_to_american(.x))
    ) %>%
      dplyr::arrange(dplyr::desc(EV), Model) %>%
      dplyr::mutate(EV = EV_display) %>%
      dplyr::select(Player, Model, EV, `Best Book`, dplyr::any_of(book_keys))
    for (b in book_keys) {
      disp <- book_display_map[tolower(b)]
      if (!is.na(disp) && nzchar(disp)) names(df)[names(df) == b] <- disp
    }
    df
  })

  output$outrights_table <- renderDT({
    df <- outrights_table_data()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "Click 'Refresh projections' above to load model prices and sportsbook odds."),
        rownames = FALSE
      ))
    }
    ncols <- ncol(df)
    if (ncols == 0) return(datatable(data.frame(Message = "No columns"), rownames = FALSE))
    names(df)[names(df) == ""] <- " "
    # Custom header: "Player" and "Model" as text; sportsbook columns = logo (local then favicon) with badge fallback
    thead_cells <- lapply(names(df), function(nm) {
      badge <- book_badge(nm)
      favicon_url <- book_favicon_url(nm)
      slug <- book_logo_slug(nm)
      if (is.null(slug) && is.null(favicon_url)) {
        return(tags$th(nm))
      }
      if (badge != nm && (!is.null(favicon_url) || !is.null(slug))) {
        local_src <- if (!is.null(slug)) paste0("logos/", slug, ".png") else NULL
        img_src <- local_src %||% favicon_url
        # On error: try favicon if we used local first; then show badge
        onerr_js <- if (!is.null(local_src) && !is.null(favicon_url))
          paste0("if(this.getAttribute('data-tried-favicon')!=='1'){ this.setAttribute('data-tried-favicon','1'); this.src='", favicon_url, "'; } else { this.style.display='none'; var s=this.nextElementSibling; if(s) s.style.display='inline-block'; }")
        else
          "this.style.display='none'; var s=this.nextElementSibling; if(s) s.style.display='inline-block';"
        tags$th(
          style = "text-align: center;",
          tags$span(
            style = "display: inline-flex; align-items: center; justify-content: center; min-height: 28px;",
            tags$img(
              src = img_src,
              alt = nm,
              title = nm,
              style = "height: 24px; width: auto; max-width: 64px; object-fit: contain; vertical-align: middle;",
              onerror = onerr_js
            ),
            tags$span(class = "book-badge", title = nm, style = "display: none;", badge)
          )
        )
      } else {
        tags$th(nm)
      }
    })
    container <- tags$table(
      class = "display",
      tags$thead(tags$tr(thead_cells))
    )
    # Column defs: EV and Best Book show raw HTML (no escape); other numeric = American odds
    ev_col_idx <- which(names(df) == "EV") - 1L
    best_col_idx <- which(names(df) == "Best Book") - 1L
    col_defs <- list(
      if (length(best_col_idx) > 0 && best_col_idx >= 0) list(targets = best_col_idx, orderable = FALSE),
      if (length(ev_col_idx) > 0 && ev_col_idx >= 0) list(targets = ev_col_idx, orderable = FALSE)
    )
    col_defs <- c(col_defs, list(list(
      targets = setdiff(seq_len(ncols) - 1L, c(ev_col_idx, best_col_idx)),
      render = DT::JS(
        "function(data, type, row, meta) {",
        " if (type !== 'display') return data;",
        " if (data == null || data === '' || String(data) === 'NA') return '—';",
        " var n = Math.round(Number(data));",
        " if (isNaN(n)) return data;",
        " return n > 0 ? '+' + n : String(n);",
        "}"
      )
    )))
    col_defs <- col_defs[!sapply(col_defs, is.null)]
    dt <- datatable(
      df,
      rownames = FALSE,
      container = container,
      class = "stripe hover",
      escape = setdiff(seq_len(ncols), which(names(df) %in% c("Best Book", "EV"))),
      options = list(
        pageLength = -1,
        lengthChange = FALSE,
        dom = "t",
        order = list(),
        columnDefs = col_defs
      )
    )
    odds_cols <- setdiff(which(vapply(df, is.numeric, logical(1))), which(names(df) == "EV"))
    if (length(odds_cols) > 0) {
      dt <- dt %>% formatStyle(columns = names(df)[odds_cols], color = "#c8d4ce")
    }
    dt
  })

  # ---- Hole Hangout: predict score on a hole (leaderboard course only; live-hole-stats + player past results) ----
  # Only load hole_data when user is on Hole Hangout tab so app startup and other tabs stay fast
  hole_data_df <- reactive({
    if (is.null(input$main_tabs) || !identical(input$main_tabs, "Hole Hangout")) {
      return(tibble::tibble(
        player_name = character(), tournament_name = character(), hole = integer(),
        par = integer(), score = integer(), round = integer()
      ))
    }
    cached <- hole_data_cache()
    if (!is.null(cached)) return(cached)
    path1 <- hole_data_path
    if (!file.exists(path1)) path1 <- file.path(getwd(), "data", "hole_data.csv")
    path2 <- hole_data_2026_path
    if (!file.exists(path2)) path2 <- file.path(getwd(), "data", "all_2026_holes.csv")
    out <- NULL
    if (file.exists(path1)) {
      out <- tryCatch(readr::read_csv(path1, show_col_types = FALSE, n_max = HOLE_DATA_MAX_ROWS), error = function(e) NULL)
      if (!is.null(out) && nrow(out) > 0 && !all(c("player_name", "tournament_name", "hole", "par", "score", "round") %in% names(out)))
        out <- NULL
    }
    if (is.null(out)) {
      return(tibble::tibble(
        player_name = character(),
        tournament_name = character(),
        hole = integer(),
        par = integer(),
        score = integer(),
        round = integer()
      ))
    }
    if (file.exists(path2)) {
      extra <- tryCatch(readr::read_csv(path2, show_col_types = FALSE), error = function(e) NULL)
      if (!is.null(extra) && nrow(extra) > 0 && all(c("player_name", "tournament_name", "hole", "par", "score") %in% names(extra))) {
        if (!"round" %in% names(extra)) extra$round <- 1L
        out <- dplyr::bind_rows(out, extra) %>% dplyr::distinct()
      }
    }
    out <- out %>% dplyr::mutate(
      hole = as.integer(hole),
      par = as.integer(par),
      score = as.integer(score),
      round = as.integer(round %||% 1L),
      tournament_name = trimws(as.character(tournament_name)),
      player_name = trimws(as.character(player_name))
    ) %>% dplyr::filter(
      is.finite(hole), hole >= 1L, hole <= 18L,
      is.finite(score), score >= 1L, score <= 10L,
      nzchar(player_name)
    )
    # Keep only last 3 years if year column present
    if ("year" %in% names(out)) {
      y <- as.integer(format(Sys.Date(), "%Y"))
      out <- out %>% dplyr::filter(as.integer(year) >= (y - 3L))
    }
    if (nrow(out) == 0) {
      return(out)
    }
    hole_data_cache(out)
    out
  })
  hole_data_current_event <- reactive({
    # For stability, use all historical hole_data; no event filter.
    hole_data_df()
  })

  # Live hole stats from DataGolf are no longer used in Hole Hangout; simulations use only historical hole_data.
  # Hole Hangout score odds do not use DataGolf live-hole-stats; live pricing uses shot_transition_model only.
  live_hole_stats <- reactive({
    NULL
  })

  # Putting distance chart (approx Tour average); used only for non-shot-model fallbacks elsewhere
  putt_chart <- list(
    ft = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 40),
    one_putt = c(0.99, 0.96, 0.88, 0.77, 0.66, 0.58, 0.50, 0.45, 0.40, 0.23, 0.15, 0.07, 0.04),
    three_putt = c(0.000, 0.001, 0.003, 0.004, 0.004, 0.005, 0.006, 0.007, 0.007, 0.013, 0.022, 0.050, 0.100)
  )
  putt_probs_from_ft <- function(ft) {
    ft <- suppressWarnings(as.numeric(ft))
    if (!is.finite(ft) || ft <= 0) return(list(p1 = NA_real_, p2 = NA_real_, p3 = NA_real_))
    f <- pmin(max(putt_chart$ft), pmax(min(putt_chart$ft), ft))
    p1 <- suppressWarnings(stats::approx(putt_chart$ft, putt_chart$one_putt, xout = f, rule = 2)$y)
    p3 <- suppressWarnings(stats::approx(putt_chart$ft, putt_chart$three_putt, xout = f, rule = 2)$y)
    p1 <- pmin(0.995, pmax(0.01, as.numeric(p1)))
    p3 <- pmin(0.25, pmax(0, as.numeric(p3)))
    p2 <- pmax(0.001, 1 - p1 - p3)
    tot <- p1 + p2 + p3
    list(p1 = p1 / tot, p2 = p2 / tot, p3 = p3 / tot)
  }

  # Try to determine wave (morning/afternoon) from player tee time when available
  player_wave_from_teetime <- function(teetime_str) {
    if (is.null(teetime_str) || !nzchar(trimws(as.character(teetime_str)))) return("total")
    s <- tolower(trimws(as.character(teetime_str)))
    # ISO: ...T14:06
    hr <- suppressWarnings(as.integer(sub("^.*t([0-9]{1,2}):.*$", "\\1", s)))
    if (!is.finite(hr)) {
      hr <- suppressWarnings(as.integer(sub("^([0-9]{1,2}).*$", "\\1", s)))
      if (grepl("pm", s, fixed = TRUE) && is.finite(hr) && hr < 12) hr <- hr + 12
      if (grepl("am", s, fixed = TRUE) && is.finite(hr) && hr == 12) hr <- 0
    }
    if (!is.finite(hr)) return("total")
    if (hr < 12) "morning_wave" else "afternoon_wave"
  }

  # Pre-aggregated scoring summary for every hole on every course/event.
  # This is computed once from hole_data_df() and then reused in Hole Hangout
  # so we don't keep scanning the full hole-by-hole table.
  hole_field_summary_df <- reactive({
    df <- hole_data_df()
    if (is.null(df) || nrow(df) == 0) {
      return(tibble::tibble(
        hole = integer(),
        par = integer(),
        n = integer(),
        avg_score = double(),
        avg_to_par = double(),
        eagles = integer(),
        birdies = integer(),
        pars = integer(),
        bogeys = integer(),
        doubles_plus = integer()
      ))
    }
    df <- df %>%
      dplyr::mutate(
        score_to_par = as.numeric(score) - as.numeric(par),
        score_type_u = toupper(trimws(as.character(score_type)))
      )
    df %>%
      dplyr::group_by(hole, par) %>%
      dplyr::summarise(
        n = dplyr::n(),
        avg_score = mean(score, na.rm = TRUE),
        avg_to_par = mean(score_to_par, na.rm = TRUE),
        eagles = sum(score_type_u %in% c("EAGLE", "EAGLE OR BETTER", "ALBATROSS"), na.rm = TRUE),
        birdies = sum(score_type_u == "BIRDIE", na.rm = TRUE),
        pars = sum(score_type_u == "PAR", na.rm = TRUE),
        bogeys = sum(score_type_u == "BOGEY", na.rm = TRUE),
        doubles_plus = sum(score_type_u %in% c("DOUBLE BOGEY", "DOUBLE", "DOUBLE OR WORSE", "TRIPLE"), na.rm = TRUE),
        .groups = "drop"
      )
  })

  # Convenience: summary used for the current event/course in Hole Hangout.
  # Currently we use a global-by-hole distribution (all courses, all events)
  # so this simply forwards the global summary; that keeps the tab fast and
  # ensures every hole has its own probability mix.
  hole_field_summary_current_event <- reactive({
    summary_df <- hole_field_summary_df()
    if (is.null(summary_df) || nrow(summary_df) == 0) return(summary_df)
    summary_df
  })

  # Prefetch field-updates once after the first flush so country/flag data is ready
  # This is a single lightweight API call and does NOT run the heavy projection pipeline.
  session$onFlushed(function() {
    if (is.null(isolate(field_updates_cache()))) refresh_field_updates("Field updates prefetch")
    if (is.null(isolate(pre_tournament_event_cache()))) refresh_pre_tournament_event("Pre-tournament prefetch")
    if (is.null(isolate(schedule_upcoming_cache()))) refresh_schedule_upcoming("Schedule prefetch")
  }, once = TRUE)

  output$hole_hangout_course_display <- renderUI({
    info <- field_updates_event_info()
    event_name <- trimws(as.character(info$event_name %||% ""))
    course_name <- trimws(as.character(info$course_name %||% ""))
    line <- event_name
    if (nzchar(course_name)) line <- paste0(line, if (nzchar(line)) " \u00b7 " else "", course_name)
    if (!nzchar(line)) line <- "Current event (load Model O/U for course)"
    div(class = "props-card", style = "margin-bottom: 16px;",
      tags$p(style = "margin: 0; font-weight: 600; color: var(--golf-text-muted);", "Course"),
      tags$p(style = "margin: 4px 0 0 0; font-size: 1rem;", line)
    )
  })

  output$hole_hangout_hole <- renderUI({
    selectInput("hole_hangout_hole", NULL, choices = setNames(1:18, paste0("Hole ", 1:18)), selected = 1, width = "100%")
  })

  hole_hangout_players <- reactive({
    field <- field_updates_table()
    rs <- simulated_round_data()
    if (!is.null(rs) && nrow(rs) > 0 && "player_name" %in% names(rs)) {
      return(sort(unique(rs$player_name)))
    }
    if (!is.null(field) && nrow(field) > 0 && "player_name" %in% names(field)) {
      return(sort(unique(field$player_name)))
    }
    character(0)
  })

  # Per-player skill for Hole Hangout: use DataGolf skill endpoints directly
  # - Driving distance from preds/skill-ratings (normalized across field)
  # - Approach skill from preds/approach-skill (bucketed SG per shot, averaged)
  hole_hangout_player_skill <- reactive({
    if (is.null(input$main_tabs) || !identical(input$main_tabs, "Hole Hangout")) {
      return(tibble::tibble(player_name = character(), driving_dd = numeric(), z_dd = numeric(), z_app = numeric()))
    }
    cached <- hole_skill_cache()
    if (!is.null(cached)) return(cached)

    skill_tbl <- NULL

    # Helper: safely parse JSON response into a data.frame with $data or root
    parse_datagolf <- function(resp) {
      if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
      txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) NULL)
      if (is.null(txt) || !nzchar(txt)) return(NULL)
      dat <- tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
      if (is.null(dat)) return(NULL)
      if (is.data.frame(dat)) return(dat)
      if (!is.null(dat$data) && is.data.frame(dat$data)) return(dat$data)
      NULL
    }

    # 1) Player skill ratings (driving, overall app), joined by dg_id
    skill_ratings <- NULL
    tryCatch({
      r_sr <- httr::GET(
        "https://feeds.datagolf.com/preds/skill-ratings",
        query = list(display = "value", file_format = "json", key = datagolf_api_key)
      )
      skill_ratings <- parse_datagolf(r_sr)
    }, error = function(e) NULL)

    sr_tbl <- if (!is.null(skill_ratings) && is.data.frame(skill_ratings) &&
      all(c("dg_id", "player_name") %in% names(skill_ratings))) {
      df <- tibble::as_tibble(skill_ratings)
      driving_cols <- intersect(c("driving_dist", "distance", "dist"), names(df))
      app_cols <- intersect(c("app", "sg_app", "approach"), names(df))
      tibble::tibble(
        dg_id = suppressWarnings(as.integer(df$dg_id)),
        player_name_api = as.character(df$player_name),
        driving_dist_raw = suppressWarnings(as.numeric(dplyr::coalesce(!!!rlang::syms(driving_cols)))),
        app_skill_raw = suppressWarnings(as.numeric(dplyr::coalesce(!!!rlang::syms(app_cols))))
      )
    } else {
      tibble::tibble(dg_id = integer(), player_name_api = character(), driving_dist_raw = numeric(), app_skill_raw = numeric())
    }

    # 2) Detailed approach skill buckets (averaged SG per shot), joined by dg_id
    approach_tbl <- NULL
    tryCatch({
      r_as <- httr::GET(
        "https://feeds.datagolf.com/preds/approach-skill",
        query = list(period = "l24", file_format = "json", key = datagolf_api_key)
      )
      as_dat <- parse_datagolf(r_as)
      if (!is.null(as_dat) && is.data.frame(as_dat) && all(c("dg_id", "player_name") %in% names(as_dat))) {
        df <- tibble::as_tibble(as_dat)
        sg_cols <- grep("_sg_per_shot$", names(df), value = TRUE)
        if (length(sg_cols) > 0) {
          sg_mat <- as.matrix(df[, sg_cols, drop = FALSE])
          row_means <- suppressWarnings(rowMeans(sg_mat, na.rm = TRUE))
        } else {
          row_means <- rep(NA_real_, nrow(df))
        }
        approach_tbl <- tibble::tibble(
          dg_id = suppressWarnings(as.integer(df$dg_id)),
          player_name_api = as.character(df$player_name),
          app_skill_bucket = as.numeric(row_means)
        )
      }
    }, error = function(e) NULL)

    if (is.null(approach_tbl)) {
      approach_tbl <- tibble::tibble(dg_id = integer(), player_name_api = character(), app_skill_bucket = numeric())
    }

    # Merge skill ratings + approach buckets on dg_id
    skill_tbl <- sr_tbl %>%
      dplyr::full_join(approach_tbl, by = "dg_id") %>%
      dplyr::mutate(
        player_name_api = dplyr::coalesce(player_name_api.x, player_name_api.y),
        approach_skill_raw = dplyr::coalesce(app_skill_bucket, app_skill_raw)
      ) %>%
      dplyr::select(dg_id, player_name_api, driving_dist_raw, approach_skill_raw)

    # Restrict to players actually in current field / simulation, using dg_id mapping
    field <- field_updates_table()
    rs <- simulated_round_data()
    field_map <- tibble::tibble()
    if (!is.null(rs) && nrow(rs) > 0 && all(c("dg_id", "player_name") %in% names(rs))) {
      field_map <- dplyr::bind_rows(field_map, rs %>% dplyr::select(dg_id, player_name))
    }
    if (!is.null(field) && nrow(field) > 0 && all(c("dg_id", "player_name") %in% names(field))) {
      field_map <- dplyr::bind_rows(field_map, field %>% dplyr::select(dg_id, player_name))
    }
    if (nrow(field_map) == 0 || nrow(skill_tbl) == 0) {
      return(tibble::tibble(player_name = character(), driving_dd = numeric(), z_dd = numeric(), z_app = numeric()))
    }
    field_map <- field_map %>%
      dplyr::mutate(dg_id = suppressWarnings(as.integer(dg_id))) %>%
      dplyr::filter(is.finite(dg_id)) %>%
      dplyr::distinct(dg_id, player_name, .keep_all = TRUE)

    skill_tbl <- field_map %>%
      dplyr::left_join(skill_tbl, by = "dg_id")

    # If we have driving_dist from the simulated round data, prefer that for distance.
    # To keep memory usage low on shinyapps.io, avoid loading historical_rounds_all.csv here.
    driving_source <- NULL
    if (!is.null(rs) && nrow(rs) > 0 && "driving_dist" %in% names(rs)) {
      driving_source <- rs %>%
        dplyr::mutate(dg_id = suppressWarnings(as.integer(dg_id))) %>%
        dplyr::filter(is.finite(dg_id)) %>%
        dplyr::group_by(dg_id) %>%
        dplyr::summarise(driving_dist_source = mean(as.numeric(driving_dist), na.rm = TRUE), .groups = "drop")
    }
    if (!is.null(driving_source) && nrow(driving_source) > 0) {
      skill_tbl <- skill_tbl %>%
        dplyr::left_join(driving_source, by = "dg_id") %>%
        dplyr::mutate(
          driving_dist_raw = dplyr::coalesce(driving_dist_source, driving_dist_raw)
        )
    }

    # Convert raw driving distance + approach skill into z-scores
    center_scale <- function(x) {
      x <- as.numeric(x)
      if (!any(is.finite(x))) return(rep(0, length(x)))
      m <- mean(x, na.rm = TRUE)
      s <- stats::sd(x, na.rm = TRUE)
      if (!is.finite(s) || s <= 0) return(rep(0, length(x)))
      z <- (x - m) / s
      pmax(pmin(z, 3), -3)
    }

    z_dd <- center_scale(skill_tbl$driving_dist_raw)
    z_app <- center_scale(skill_tbl$approach_skill_raw)

    out <- tibble::tibble(
      player_name = as.character(skill_tbl$player_name),
      driving_dd = as.numeric(skill_tbl$driving_dist_raw),
      z_dd = as.numeric(z_dd),
      z_app = as.numeric(z_app)
    )

    hole_skill_cache(out)
    out
  })

  output$hole_hangout_player <- renderUI({
    players <- hole_hangout_players()
    if (length(players) == 0) {
      return(selectInput("hole_hangout_player", NULL, choices = c("(Load Model O/U for field)" = ""), width = "100%"))
    }
    selectInput("hole_hangout_player", NULL, choices = setNames(players, name_display(players)), selected = players[1], width = "100%")
  })

  hole_hangout_prediction_result <- reactive({
    hole_num <- as.integer(input$hole_hangout_hole %||% 1L)
    if (!is.finite(hole_num) || hole_num < 1L || hole_num > 18L) hole_num <- 1L
    par_hole <- 4L
    field_avg <- NA_real_
    n_obs <- 0L
    summary_df <- hole_field_summary_current_event()
    if (!is.null(summary_df) && nrow(summary_df) > 0) {
      row_hole <- summary_df %>% dplyr::filter(hole == hole_num)
      if (nrow(row_hole) > 0) {
        par_guess <- suppressWarnings(as.integer(stats::median(row_hole$par, na.rm = TRUE)))
        if (is.finite(par_guess) && par_guess >= 3L && par_guess <= 5L) par_hole <- par_guess
        field_avg <- row_hole$avg_score[1]
        n_obs <- row_hole$n[1]
      }
    }
    # Expected score from player-adjusted outcome mix
    dist <- hole_outcome_distribution()
    if (!is.null(dist)) {
      probs <- dist$probs
      probs[is.na(probs)] <- 0
      par_hole <- dist$par
      expected <- as.numeric(
        par_hole - 2 * probs["Eagle"] - 1 * probs["Birdie"] +
          1 * probs["Bogey"] + 2 * probs["Double+"]
      )
      pred <- expected
      src <- "player_adjusted"
    } else if (is.finite(field_avg)) {
      pred <- field_avg
      src <- "field_history"
    } else {
      pred <- as.numeric(par_hole)
      src <- "par"
    }
    list(predicted = pred, par = par_hole, n_obs = n_obs, source = src, field_avg = field_avg)
  })

  hole_sim_trigger <- reactiveVal(0L)
  observeEvent(input$hole_sim_run, { hole_sim_trigger(isolate(hole_sim_trigger()) + 1L) })
  hole_outcome_distribution <- reactive({
    hole_num <- as.integer(input$hole_hangout_hole %||% 1L)
    player <- input$hole_hangout_player
    if (!is.finite(hole_num) || hole_num < 1L || hole_num > 18L) hole_num <- 1L
    par_hole <- 4L
    summary_df <- hole_field_summary_current_event()
    if (!is.null(summary_df) && nrow(summary_df) > 0) {
      row_hole <- summary_df %>% dplyr::filter(hole == hole_num)
      if (nrow(row_hole) > 0) {
        par_guess <- suppressWarnings(as.integer(stats::median(row_hole$par, na.rm = TRUE)))
        if (is.finite(par_guess) && par_guess >= 3L && par_guess <= 5L) par_hole <- par_guess
      }
    }

    # Live inputs: score odds from empirical shot-by-shot transitions only (shot_transition_model.rds)
    if (isTRUE(input$hh_use_live_inputs)) {
      shot_rds <- file.path(model_dir, "data", "shot_transition_model.rds")
      if (file.exists(shot_rds) && !is.null(player) && nzchar(as.character(player))) {
        pr_mc <- tryCatch({
          if (!exists("hole_outcome_probs_shot_mc", mode = "function")) {
            source(file.path(model_dir, "R", "shot_level_model.R"), encoding = "UTF-8")
          }
          if (!exists("load_pga_datagolf_map", mode = "function")) {
            source(file.path(model_dir, "R", "player_id_mapping.R"), encoding = "UTF-8")
          }
          obj <- readRDS(shot_rds)
          shot_fit <- obj$fit
          meta <- obj$meta %||% list()
          hp <- as.integer(unlist(meta$holes_par))
          hy <- as.integer(round(as.numeric(unlist(meta$holes_yardage))))
          ch_path <- file.path(model_dir, "data", "current_course_holes.rds")
          if (file.exists(ch_path)) {
            ch <- tryCatch(readRDS(ch_path), error = function(e) NULL)
            sel_norm <- ""
            gp <- file.path(model_dir, "golf_selected_course.txt")
            if (file.exists(gp)) sel_norm <- normalize_course_name(trimws(readLines(gp, n = 1L, warn = FALSE)))
            if (!nzchar(sel_norm)) {
              fi <- field_updates_table()
              if (!is.null(fi) && nrow(fi) > 0 && "course_name" %in% names(fi)) {
                sel_norm <- normalize_course_name(as.character(fi$course_name[1]))
              }
            }
            if (!is.null(ch) && length(ch$holes_par) == 18L && length(ch$holes_yardage) == 18L &&
                nzchar(sel_norm) && identical(normalize_course_name(ch$course %||% ""), sel_norm)) {
              hp <- as.integer(ch$holes_par)
              hy <- as.integer(round(as.numeric(ch$holes_yardage)))
            }
          }
          if (length(hp) != 18L || length(hy) != 18L) return(NULL)
          par_hole <- hp[hole_num]
          yard_h <- as.numeric(hy[hole_num])
          rs <- simulated_round_data()
          field <- field_updates_table()
          dg_id <- NA_integer_
          if (!is.null(rs) && nrow(rs) > 0 && all(c("dg_id", "player_name") %in% names(rs))) {
            rr <- rs %>% dplyr::filter(player_name == player)
            if (nrow(rr) > 0) dg_id <- suppressWarnings(as.integer(rr$dg_id[1]))
          }
          if (!is.finite(dg_id) && !is.null(field) && nrow(field) > 0 && all(c("dg_id", "player_name") %in% names(field))) {
            rr <- field %>% dplyr::filter(player_name == player)
            if (nrow(rr) > 0) dg_id <- suppressWarnings(as.integer(rr$dg_id[1]))
          }
          pmap <- load_pga_datagolf_map(file.path(model_dir, "data", "pga_datagolf_player_map.csv"))
          pga_id <- dg_id_to_pga_player_id(dg_id, pmap)
          pid <- if (is.na(pga_id) || !nzchar(as.character(pga_id))) "tour" else as.character(pga_id)
          pw <- if (identical(pid, "tour")) 0 else 0.65
          shot_num <- suppressWarnings(as.integer(input$hh_shot_num %||% 2L))
          dist_yds <- suppressWarnings(as.numeric(input$hh_dist_yds %||% NA_real_))
          lie <- as.character(input$hh_lie %||% "Fairway")
          putt_ft <- suppressWarnings(as.numeric(input$hh_putt_ft %||% NA_real_))
          if (!is.finite(shot_num) || shot_num < 1L) shot_num <- 2L
          if (!is.finite(dist_yds) || dist_yds < 0) dist_yds <- 150
          hole_outcome_probs_shot_mc(
            par_hole, yard_h, lie, dist_yds, putt_ft, shot_num,
            pid, shot_fit, n_sims = 400L, player_weight = pw,
            seed = as.integer(Sys.time()) %% 1e6
          )
        }, error = function(e) {
          message("Hole Hangout live shot MC: ", conditionMessage(e))
          NULL
        })
        if (!is.null(pr_mc) && length(pr_mc) == 5L) {
          probs <- suppressWarnings(as.numeric(pr_mc))
          names(probs) <- c("Eagle", "Birdie", "Par", "Bogey", "Double+")
          probs[is.na(probs) | !is.finite(probs) | probs < 0] <- 0
          if (sum(probs) > 0) {
            probs <- probs / sum(probs)
            return(list(
              par = par_hole,
              eagle = probs["Eagle"], birdie = probs["Birdie"], par_outcome = probs["Par"], bogey = probs["Bogey"], double = probs["Double+"],
              probs = probs
            ))
          }
        }
      }
    }

    E <- B <- P <- Bog <- D <- 0
    if (!is.null(summary_df) && nrow(summary_df) > 0) {
      row_hole <- summary_df %>% dplyr::filter(hole == hole_num)
      if (nrow(row_hole) > 0) {
        par_guess <- suppressWarnings(as.integer(stats::median(row_hole$par, na.rm = TRUE)))
        if (is.finite(par_guess) && par_guess >= 3L && par_guess <= 5L) par_hole <- par_guess
        E <- row_hole$eagles[1]
        B <- row_hole$birdies[1]
        P <- row_hole$pars[1]
        Bog <- row_hole$bogeys[1]
        D <- row_hole$doubles_plus[1]
      }
    }
    # Baseline prior: pars most common, birdies/bogeys less common, doubles rare, eagles very rare
    prior_E <- if (par_hole == 5L) 0.5 else 0.0
    prior_B <- 1.0
    prior_P <- 5.0
    prior_Bog <- 0.8
    prior_D <- 0.2
    E <- E + prior_E
    B <- B + prior_B
    P <- P + prior_P
    Bog <- Bog + prior_Bog
    D <- D + prior_D

    # Player-specific tilt using driving distance and approach skill (normalized z-scores)
    z_dd <- 0
    z_app <- 0
    skills <- hole_hangout_player_skill()
    if (!is.null(skills) && nrow(skills) > 0 && !is.null(player) && nzchar(player)) {
      row_p <- skills %>% dplyr::filter(player_name == player) %>% dplyr::slice(1L)
      if (nrow(row_p) > 0) {
        z_dd <- as.numeric(row_p$z_dd[1])
        z_app <- as.numeric(row_p$z_app[1])
      }
    }
    # Combine skills by hole type: par 3 = approach-heavy, par 5 = distance+approach, par 4 = balanced
    if (par_hole == 3L) {
      skill_score <- 0.1 * z_dd + 0.9 * z_app
    } else if (par_hole == 5L) {
      skill_score <- 0.5 * z_dd + 0.5 * z_app
    } else {
      skill_score <- 0.3 * z_dd + 0.7 * z_app
    }
    skill_score <- pmax(pmin(skill_score, 2.5), -2.5)

    # Tilt baseline counts toward better scores for positive skill, opposite for negative
    mult_E   <- 1 + 1.0 * skill_score
    mult_B   <- 1 + 0.7 * skill_score
    mult_P   <- 1 - 0.25 * skill_score
    mult_Bog <- 1 - 0.8 * skill_score
    mult_D   <- 1 - 1.0 * skill_score

    E <- pmax(0, E * mult_E)
    B <- pmax(0, B * mult_B)
    P <- pmax(0, P * mult_P)
    Bog <- pmax(0, Bog * mult_Bog)
    D <- pmax(0, D * mult_D)

    tot <- E + B + P + Bog + D
    if (!is.finite(tot) || tot <= 0) {
      probs <- c(Eagle = 0, Birdie = 0.15, Par = 0.7, Bogey = 0.13, `Double+` = 0.02)
      if (par_hole == 5L) probs["Eagle"] <- 0.03 else probs["Eagle"] <- 0
      probs <- probs / sum(probs)
    } else {
      probs <- c(Eagle = E / tot, Birdie = B / tot, Par = P / tot, Bogey = Bog / tot, `Double+` = D / tot)
      if (par_hole == 3L || par_hole == 4L) {
        probs["Eagle"] <- 0
        probs <- probs / sum(probs)
      }
    }
    probs <- suppressWarnings(as.numeric(probs))
    names(probs) <- c("Eagle", "Birdie", "Par", "Bogey", "Double+")
    probs[is.na(probs) | !is.finite(probs) | probs < 0] <- 0
    if (sum(probs) <= 0) probs <- c(Eagle = 0, Birdie = 0.15, Par = 0.7, Bogey = 0.13, `Double+` = 0.02)
    probs <- probs / sum(probs)
    list(
      par = par_hole,
      eagle = probs["Eagle"], birdie = probs["Birdie"], par_outcome = probs["Par"], bogey = probs["Bogey"], double = probs["Double+"],
      probs = probs
    )
  })

  hole_sim_result <- reactive({
    hole_sim_trigger()
    dist <- isolate(hole_outcome_distribution())
    hole_num <- isolate(as.integer(input$hole_hangout_hole %||% 1L))
    player <- isolate(input$hole_hangout_player)
    if (is.null(dist)) return(NULL)
    par_hole <- dist$par
    probs <- dist$probs
    probs[is.na(probs)] <- 0
    if (sum(probs) <= 0) {
      # Fallback: pars most likely, birdies/bogeys less common, doubles rare
      probs <- c(Eagle = 0, Birdie = 0.15, Par = 0.7, Bogey = 0.13, `Double+` = 0.02)
      if (par_hole == 5L) probs["Eagle"] <- 0.03
      if (par_hole != 5L) probs["Eagle"] <- 0
      probs <- probs / sum(probs)
    }

    shot_model_used <- FALSE
    use_shot <- FALSE
    shot_yardage <- NA_real_
    shot_rds <- file.path(model_dir, "data", "shot_transition_model.rds")
    if (file.exists(shot_rds) && !is.null(player) && nzchar(as.character(player))) {
      tryCatch({
        if (!exists("simulate_hole_shot_level", mode = "function")) {
          source(file.path(model_dir, "R", "shot_level_model.R"), encoding = "UTF-8")
        }
        if (!exists("dg_id_to_pga_player_id", mode = "function")) {
          source(file.path(model_dir, "R", "player_id_mapping.R"), encoding = "UTF-8")
        }
        obj <- readRDS(shot_rds)
        shot_fit <- obj$fit
        meta <- obj$meta %||% list()
        hp <- as.integer(unlist(meta$holes_par))
        hy <- as.integer(round(as.numeric(unlist(meta$holes_yardage))))
        # Prefer per-hole par/yardage for the current tournament (from round_projections / all_shots)
        ch_path <- file.path(model_dir, "data", "current_course_holes.rds")
        if (file.exists(ch_path)) {
          ch <- tryCatch(readRDS(ch_path), error = function(e) NULL)
          sel_norm <- ""
          gp <- file.path(model_dir, "golf_selected_course.txt")
          if (file.exists(gp)) sel_norm <- normalize_course_name(trimws(readLines(gp, n = 1L, warn = FALSE)))
          if (!nzchar(sel_norm)) {
            fi <- isolate(field_updates_table())
            if (!is.null(fi) && nrow(fi) > 0 && "course_name" %in% names(fi)) {
              sel_norm <- normalize_course_name(as.character(fi$course_name[1]))
            }
          }
          if (!is.null(ch) && length(ch$holes_par) == 18L && length(ch$holes_yardage) == 18L &&
              nzchar(sel_norm) && identical(normalize_course_name(ch$course %||% ""), sel_norm)) {
            hp <- as.integer(ch$holes_par)
            hy <- as.integer(round(as.numeric(ch$holes_yardage)))
          }
        }
        if (length(hp) == 18L && length(hy) == 18L && !is.null(shot_fit) &&
            is.finite(hole_num) && hole_num >= 1L && hole_num <= 18L) {
          par_hole <- hp[hole_num]
          shot_yardage <- as.numeric(hy[hole_num])
          rs <- isolate(simulated_round_data())
          field <- isolate(field_updates_table())
          dg_id <- NA_integer_
          if (!is.null(rs) && nrow(rs) > 0 && all(c("dg_id", "player_name") %in% names(rs))) {
            rr <- rs %>% dplyr::filter(player_name == player)
            if (nrow(rr) > 0) dg_id <- suppressWarnings(as.integer(rr$dg_id[1]))
          }
          if (!is.finite(dg_id) && !is.null(field) && nrow(field) > 0 && all(c("dg_id", "player_name") %in% names(field))) {
            rr <- field %>% dplyr::filter(player_name == player)
            if (nrow(rr) > 0) dg_id <- suppressWarnings(as.integer(rr$dg_id[1]))
          }
          pmap <- load_pga_datagolf_map(file.path(model_dir, "data", "pga_datagolf_player_map.csv"))
          pga_id <- dg_id_to_pga_player_id(dg_id, pmap)
          pw <- if (is.na(pga_id) || !nzchar(as.character(pga_id))) 0 else 0.65
          pid <- if (is.na(pga_id) || !nzchar(as.character(pga_id))) "tour" else as.character(pga_id)
          st <- simulate_hole_shot_level(par_hole, shot_yardage, pid, shot_fit, player_weight = pw, max_strokes = 18L)
          st <- as.integer(max(1L, min(18L, st)))
          score <- st
          strokes <- st
          dff <- st - par_hole
          outcome_name <- if (dff <= -2L) "Eagle" else if (dff == -1L) "Birdie" else if (dff == 0L) "Par" else if (dff == 1L) "Bogey" else "Double+"
          use_shot <- TRUE
          shot_model_used <- TRUE
        }
      }, error = function(e) {
        message("Hole Hangout shot model: ", conditionMessage(e))
      })
    }

    if (!use_shot) {
      outcome_name <- sample(names(probs), 1, prob = probs)
      score <- switch(outcome_name, Eagle = par_hole - 2, Birdie = par_hole - 1, Par = par_hole, Bogey = par_hole + 1, `Double+` = par_hole + 2, par_hole)
      strokes <- max(1, min(8, as.integer(score)))
      if (par_hole == 3L && strokes < 2L) strokes <- 2L
    }
    # Player-specific driving distance (for narrative only)
    z_dd <- 0
    dd_raw <- NA_real_
    skills <- hole_hangout_player_skill()
    if (!is.null(skills) && nrow(skills) > 0 && !is.null(player) && nzchar(player)) {
      row_p <- skills %>% dplyr::filter(player_name == player) %>% dplyr::slice(1L)
      if (nrow(row_p) > 0) {
        z_dd <- as.numeric(row_p$z_dd[1])
        dd_raw <- suppressWarnings(as.numeric(row_p$driving_dd[1]))
      }
    }
    base_dd <- if (par_hole == 5L) 295 else if (par_hole == 4L) 285 else 175
    # Prefer actual skill driving distance where available; otherwise use z-score adjustment
    tee_dd_mean <- if (is.finite(dd_raw)) dd_raw else base_dd + 8 * z_dd
    # Sample around mean with some spread so not every tee ball is identical (unless shot model fixed yardage)
    tee_sd <- if (par_hole == 5L) 12 else if (par_hole == 4L) 10 else 6
    if (shot_model_used && is.finite(shot_yardage) && shot_yardage > 0) {
      tee_dd <- shot_yardage
    } else {
      tee_dd <- stats::rnorm(1L, mean = tee_dd_mean, sd = tee_sd)
      tee_dd <- pmax(240, pmin(350, tee_dd))
    }

    # Simple fairway / rough / bunker outcome for tee shot, tilted by distance skill
    # Base fairway rate ~0.6; long hitters a bit lower, short / accurate a bit higher
    p_fw <- 0.6 - 0.04 * z_dd
    p_fw <- pmin(0.8, pmax(0.4, p_fw))
    p_bunker <- 0.07
    p_rough <- pmax(0, 1 - p_fw - p_bunker)
    tee_lie <- sample(c("Fairway", "Rough", "Bunker"), size = 1L,
                      prob = c(p_fw, p_rough, p_bunker))

    set.seed(as.integer(Sys.time()) %% 1e6)
    shots <- list()
    if (par_hole == 3L) {
      dd_str <- sprintf("%.0f yds", tee_dd)
      # On par 3s we still treat the target as the green; add putting/chipping variety.
      if (strokes == 2) {
        if (runif(1) < 0.5) {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = "Green", detail = ""),
            list(num = 2, type = "Putt", dist = "14 ft", result = "Birdie", detail = "")
          )
        } else {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = "Green", detail = ""),
            list(num = 2, type = "Putt", dist = "22 ft", result = "", detail = ""),
            list(num = 3, type = "Putt", dist = "4 ft", result = "Birdie", detail = "")
          )
        }
      } else if (strokes == 3) {
        if (runif(1) < 0.5) {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = "Green", detail = ""),
            list(num = 2, type = "Putt", dist = "28 ft", result = "", detail = ""),
            list(num = 3, type = "Putt", dist = "4 ft", result = "Par", detail = "")
          )
        } else {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = "Rough", detail = ""),
            list(num = 2, type = "Chip", dist = "18 yds", result = "Green", detail = ""),
            list(num = 3, type = "Putt", dist = "6 ft", result = "Par", detail = "")
          )
        }
      } else {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = "Rough", detail = ""),
          list(num = 2, type = "Chip", dist = "22 yds", result = "Green", detail = ""),
          list(num = 3, type = "Putt", dist = "18 ft", result = "", detail = ""),
          list(num = 4, type = "Putt", dist = "2 ft", result = "Bogey", detail = "")
        )
      }
    } else if (par_hole == 4L) {
      dd_str <- sprintf("%.0f yds", tee_dd)
      # Heuristic path forced par-4 "2 strokes" to 3 (avoid impossible sampled eagles); shot model can return true eagles.
      if (!shot_model_used && strokes == 2) strokes <- 3L
      if (shot_model_used && strokes == 2L) {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = "Green", detail = ""),
          list(num = 2, type = "Putt", dist = "14 ft", result = "Eagle", detail = "")
        )
      } else if (strokes == 3) {
        # Birdie look: sometimes one long putt, sometimes lag + tap-in
        if (runif(1) < 0.5) {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
            list(num = 2, type = "Approach", dist = "142 yds", result = "Green", detail = ""),
            list(num = 3, type = "Putt", dist = "11 ft", result = "Birdie", detail = "")
          )
        } else {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
            list(num = 2, type = "Approach", dist = "138 yds", result = "Green", detail = ""),
            list(num = 3, type = "Putt", dist = "22 ft", result = "", detail = ""),
            list(num = 4, type = "Putt", dist = "4 ft", result = "Birdie", detail = "")
          )
        }
      } else if (strokes == 4) {
        # Approach from rough/fairway/bunker: can land Green, Rough, or Bunker (not forced to same lie)
        from_rough_gir <- 0.55
        from_fairway_gir <- 0.75
        from_bunker_gir <- 0.35
        p_green <- switch(tee_lie, Fairway = from_fairway_gir, Rough = from_rough_gir, Bunker = from_bunker_gir, from_rough_gir)
        approach_land <- sample(c("Green", "Rough", "Bunker"), size = 1L,
          prob = c(p_green, pmax(0, 0.35 - (p_green - 0.5) * 0.2), pmax(0, 0.15)))
        if (approach_land == "Green") {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
            list(num = 2, type = "Approach", dist = "128 yds", result = "Green", detail = ""),
            list(num = 3, type = "Putt", dist = "24 ft", result = "", detail = ""),
            list(num = 4, type = "Putt", dist = "3 ft", result = "Par", detail = "")
          )
        } else if (approach_land == "Rough") {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
            list(num = 2, type = "Approach", dist = "152 yds", result = "Rough", detail = ""),
            list(num = 3, type = "Chip", dist = "18 yds", result = "Green", detail = ""),
            list(num = 4, type = "Putt", dist = "6 ft", result = "Par", detail = "")
          )
        } else {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
            list(num = 2, type = "Approach", dist = "148 yds", result = "Bunker", detail = ""),
            list(num = 3, type = "Chip", dist = "18 yds", result = "Green", detail = ""),
            list(num = 4, type = "Putt", dist = "6 ft", result = "Par", detail = "")
          )
        }
      } else if (strokes == 5) {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
          list(num = 2, type = "Approach", dist = "156 yds", result = "Green", detail = ""),
          list(num = 3, type = "Putt", dist = "35 ft", result = "", detail = ""),
          list(num = 4, type = "Putt", dist = "8 ft", result = "", detail = ""),
          list(num = 5, type = "Putt", dist = "2 ft", result = "Bogey", detail = "")
        )
      } else {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
          list(num = 2, type = "Approach", dist = "168 yds", result = "Bunker", detail = ""),
          list(num = 3, type = "Chip", dist = "18 yds", result = "Green", detail = ""),
          list(num = 4, type = "Putt", dist = "15 ft", result = "", detail = ""),
          list(num = 5, type = "Putt", dist = "5 ft", result = "", detail = ""),
          list(num = 6, type = "Putt", dist = "1 ft", result = "Double Bogey", detail = "")
        )
      }
    } else {
      dd_str <- sprintf("%.0f yds", tee_dd)
      if (shot_model_used && strokes == 2L) {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
          list(num = 2, type = "Putt", dist = "12 ft", result = "Double Eagle", detail = "")
        )
      } else if (strokes == 3) {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
          list(num = 2, type = "Second", dist = "248 yds", result = "Green", detail = ""),
          list(num = 3, type = "Putt", dist = "18 ft", result = "Eagle", detail = "")
        )
      } else if (strokes == 4) {
        if (runif(1) < 0.5) {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
            list(num = 2, type = "Second", dist = "235 yds", result = "Green", detail = ""),
            list(num = 3, type = "Putt", dist = "32 ft", result = "", detail = ""),
            list(num = 4, type = "Putt", dist = "6 ft", result = "Birdie", detail = "")
          )
        } else {
          shots <- list(
            list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
            list(num = 2, type = "Second", dist = "230 yds", result = "Bunker", detail = ""),
            list(num = 3, type = "Chip", dist = "15 yds", result = "Green", detail = ""),
            list(num = 4, type = "Putt", dist = "8 ft", result = "Birdie", detail = "")
          )
        }
      } else if (strokes == 5) {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
          list(num = 2, type = "Second", dist = "218 yds", result = "Green", detail = ""),
          list(num = 3, type = "Putt", dist = "45 ft", result = "", detail = ""),
          list(num = 4, type = "Putt", dist = "12 ft", result = "", detail = ""),
          list(num = 5, type = "Putt", dist = "2 ft", result = "Par", detail = "")
        )
      } else {
        shots <- list(
          list(num = 1, type = "Tee shot", dist = dd_str, result = tee_lie, detail = ""),
          list(num = 2, type = "Second", dist = "202 yds", result = "Rough", detail = ""),
          list(num = 3, type = "Approach", dist = "85 yds", result = "Green", detail = ""),
          list(num = 4, type = "Putt", dist = "22 ft", result = "", detail = ""),
          list(num = 5, type = "Putt", dist = "4 ft", result = "Bogey", detail = "")
        )
      }
    }
    # Heuristic templates don't cover every stroke count; shot model can return 1–18. Align narrative length to strokes.
    if (shot_model_used && (length(shots) == 0L || length(shots) != as.integer(strokes))) {
      dd_str <- sprintf("%.0f yds", tee_dd)
      ns <- min(as.integer(strokes), 12L)
      shots <- lapply(seq_len(ns), function(i) {
        list(
          num = i,
          type = if (i == 1L) "Tee shot" else if (i == ns) "Holed out" else "Shot",
          dist = if (i == 1L) dd_str else "\u2014",
          result = if (i == ns) as.character(outcome_name) else "",
          detail = if (i == ns) "empirical shot transitions" else ""
        )
      })
    } else if (!shot_model_used && length(shots) == 0L) {
      dd_str <- sprintf("%.0f yds", tee_dd)
      shots <- lapply(seq_len(min(as.integer(strokes), 12L)), function(i) {
        list(
          num = i,
          type = if (i == 1L) "Tee shot" else if (i == strokes) "Holed out" else "Shot",
          dist = if (i == 1L) dd_str else "\u2014",
          result = if (i == strokes) as.character(outcome_name) else "",
          detail = ""
        )
      })
    }
    outcome_display <- if (outcome_name == "Double+") "Double" else outcome_name
    outcome_display <- as.character(outcome_display)[1]
    list(
      outcome = outcome_name, outcome_display = outcome_display, score = score, par = par_hole, shots = shots, dist = dist,
      shot_model_used = shot_model_used
    )
  })

  output$hole_sim_top_bar <- renderUI({
    sim <- hole_sim_result()
    hole_num <- as.integer(input$hole_hangout_hole %||% 1L)
    if (is.null(sim)) return(NULL)
    par_hole <- sim$par
    div(class = "hole-sim-top-bar",
      tags$span(style = "font-weight: 700; font-size: 1.05rem;", paste0("Hole ", hole_num, " (Par ", par_hole, ")")),
      if (isTRUE(sim$shot_model_used)) tags$span(style = "margin-left: 10px; font-size: 0.85rem; color: var(--golf-text-muted);", "Empirical shot model")
    )
  })

  output$hole_sim_panel_hole <- renderUI({
    sim <- hole_sim_result()
    if (is.null(sim)) return(div(class = "hole-sim-panel", h4("HOLE"), tags$p("Select hole and player, then click Simulate hole.", class = "text-muted")))
    par_hole <- sim$par
    hole_num <- as.integer(input$hole_hangout_hole %||% 1L)
    if (!is.finite(hole_num) || hole_num < 1L || hole_num > 18L) hole_num <- 1L
    shots <- sim$shots
    n_shots <- length(shots)
    viewBox <- "0 0 200 360"
    stroke_col <- "#3d4a3d"
    fill_rough <- "#0f1a12"
    fill_fairway <- "#1e3d2a"
    fill_tee <- "#243d2e"
    fill_green <- "#2d5a3d"
    fill_water <- "#1e4a6a"
    fill_sand <- "#c9b896"
    green_cx <- 100
    green_cy <- 32
    green_rx <- 26
    green_ry <- 14
    r <- 10
    dogleg_right <- (hole_num %% 2L) == 1L
    if (par_hole == 3L) {
      path_d <- paste0("M ", 82 + r, " 340 L ", 118 - r, " 340 Q 118 340 118 ", 52 + r, " L 118 56 Q 118 32 100 32 Q 82 32 82 56 L 82 ", 52 + r, " Q 82 340 ", 82 + r, " 340 Z")
      tee_rect <- "M 86 348 L 114 348 L 114 332 L 86 332 Z"
      water_svg <- paste0('<ellipse cx="100" cy="52" rx="24" ry="8" fill="', fill_water, '" stroke="#2a5f7a" stroke-width="1" opacity="0.95"/>')
      bunker_svg <- paste0(
        '<ellipse cx="68" cy="30" rx="12" ry="10" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
        '<ellipse cx="132" cy="30" rx="12" ry="10" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>'
      )
      shot_pts <- if (n_shots > 0) {
        ys <- seq(320, 50, length.out = n_shots + 1)[-(n_shots + 1)]
        if (n_shots == 1) ys <- 90
        lapply(seq_len(n_shots), function(i) list(x = 100, y = ys[i]))
      } else list()
    } else if (par_hole == 5L) {
      if (dogleg_right) {
        path_d <- paste0("M 82 340 L 118 340 Q 128 340 128 230 L 128 150 Q 128 88 145 88 Q 162 88 162 98 L 162 92 Q 162 36 128 36 L 82 36 Q 72 36 72 78 L 72 92 Q 72 160 38 160 L 48 160 Q 38 220 72 220 L 72 330 Q 72 340 82 340 Z")
      } else {
        path_d <- paste0("M 82 340 L 118 340 Q 128 340 128 230 L 128 78 Q 128 88 128 88 L 118 88 Q 72 36 72 36 L 72 46 Q 72 88 38 88 L 28 88 Q 38 88 38 98 L 38 160 Q 72 160 72 160 L 82 220 Q 72 340 82 340 Z")
      }
      tee_rect <- "M 88 348 L 112 348 L 112 334 L 88 334 Z"
      water_svg <- paste0('<ellipse cx="100" cy="50" rx="28" ry="10" fill="', fill_water, '" stroke="#2a5f7a" stroke-width="1" opacity="0.9"/>')
      if (dogleg_right) {
        bunker_svg <- paste0(
          '<ellipse cx="68" cy="30" rx="11" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="132" cy="30" rx="11" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="155" cy="92" rx="14" ry="10" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="42" cy="165" rx="13" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>'
        )
      } else {
        bunker_svg <- paste0(
          '<ellipse cx="68" cy="30" rx="11" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="132" cy="30" rx="11" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="45" cy="92" rx="14" ry="10" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="158" cy="165" rx="13" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>'
        )
      }
      n_pt <- max(1L, n_shots)
      t_seq <- seq(0, 1, length.out = n_pt + 2)
      t_seq <- t_seq[-c(1, length(t_seq))]
      if (n_shots == 1) t_seq <- 0.5
      shot_pts <- lapply(seq_along(t_seq), function(i) {
        t <- t_seq[i]
        x <- 100 + (2 * t - 1) * (if (dogleg_right) 35 else -35)
        y <- 340 - t * 300
        list(x = x, y = y)
      })
    } else {
      if (dogleg_right) {
        path_d <- paste0("M 88 340 L 112 340 Q 122 340 122 210 L 122 108 Q 122 95 138 95 Q 152 95 152 105 L 152 98 Q 152 40 122 40 L 88 40 Q 78 40 78 85 L 78 98 Q 78 95 78 200 L 78 330 Q 78 340 88 340 Z")
      } else {
        path_d <- paste0("M 88 340 L 112 340 Q 122 340 122 210 L 122 85 Q 122 95 122 95 L 112 95 Q 78 40 78 40 L 78 50 Q 78 95 48 95 L 38 95 Q 48 95 48 105 L 48 98 Q 48 200 78 200 L 78 330 Q 78 340 88 340 Z")
      }
      tee_rect <- "M 84 346 L 116 346 L 116 332 L 84 332 Z"
      water_svg <- if (dogleg_right)
        paste0('<ellipse cx="130" cy="52" rx="20" ry="10" fill="', fill_water, '" stroke="#2a5f7a" stroke-width="1" opacity="0.95"/>')
      else
        paste0('<ellipse cx="70" cy="52" rx="20" ry="10" fill="', fill_water, '" stroke="#2a5f7a" stroke-width="1" opacity="0.95"/>')
      if (dogleg_right) {
        bunker_svg <- paste0(
          '<ellipse cx="72" cy="30" rx="10" ry="8" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="128" cy="30" rx="10" ry="8" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="148" cy="98" rx="12" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>'
        )
      } else {
        bunker_svg <- paste0(
          '<ellipse cx="72" cy="30" rx="10" ry="8" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="128" cy="30" rx="10" ry="8" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>',
          '<ellipse cx="52" cy="98" rx="12" ry="9" fill="', fill_sand, '" stroke="#a89870" stroke-width="1"/>'
        )
      }
      n_pt <- max(1L, n_shots)
      t_seq <- seq(0, 1, length.out = n_pt + 2)
      t_seq <- t_seq[-c(1, length(t_seq))]
      if (n_shots == 1) t_seq <- 0.5
      shot_pts <- lapply(seq_along(t_seq), function(i) {
        t <- t_seq[i]
        x <- 100 + (2 * t - 1) * (if (dogleg_right) 28 else -28)
        y <- 340 - t * 305
        list(x = x, y = y)
      })
    }
    if (par_hole == 3L && n_shots > 0) {
      ys <- seq(320, 50, length.out = n_shots + 1)[-(n_shots + 1)]
      if (n_shots == 1) ys <- 90
      shot_pts <- lapply(seq_len(n_shots), function(i) list(x = 100, y = ys[i]))
    }
    # Snap final shot marker to the pin/hole location
    if (n_shots > 0 && length(shot_pts) >= n_shots) {
      shot_pts[[n_shots]]$x <- green_cx
      shot_pts[[n_shots]]$y <- green_cy
    }
    shot_circles <- ""
    shot_lines <- ""
    if (n_shots > 0 && length(shot_pts) >= n_shots) {
      colors <- c("#3b82f6", "#22c55e", "#eab308", "#f97316", "#ef4444", "#8b5cf6")[seq_len(n_shots)]
      for (i in seq_len(n_shots)) {
        pt <- shot_pts[[i]]
        x <- pt$x; y <- pt$y
        if (i > 1L) {
          prev <- shot_pts[[i - 1L]]
          shot_lines <- paste0(
            shot_lines,
            sprintf(
              '<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#ffffff" stroke-width="2" stroke-linecap="round" opacity="0.9"/>',
              prev$x, prev$y, x, y
            )
          )
        }
        shot_circles <- paste0(shot_circles, sprintf('<circle cx="%s" cy="%s" r="10" fill="%s" stroke="#111" stroke-width="1.5"/><text x="%s" y="%s" text-anchor="middle" dominant-baseline="central" fill="#fff" font-size="10" font-weight="bold">%s</text>', x, y, colors[i], x, y, i))
      }
    }
    green_ellipse <- sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" fill="%s" stroke="%s" stroke-width="2"/>', green_cx, green_cy, green_rx, green_ry, fill_green, stroke_col)
    pin <- sprintf('<circle cx="%s" cy="%s" r="3" fill="#fff" opacity="0.95"/>', green_cx, green_cy)
    div(class = "hole-sim-panel",
      h4("HOLE"),
      HTML(paste0(
        '<svg viewBox="', viewBox, '" width="100%" style="max-height: 260px;" xmlns="http://www.w3.org/2000/svg">',
        '<rect x="0" y="0" width="200" height="360" fill="', fill_rough, '" stroke="', stroke_col, '" stroke-width="1"/>',
        water_svg,
        '<path d="', path_d, '" fill="', fill_fairway, '" stroke="', stroke_col, '" stroke-width="2"/>',
        bunker_svg,
        '<path d="', tee_rect, '" fill="', fill_tee, '" stroke="', stroke_col, '" stroke-width="2"/>',
        green_ellipse,
        shot_lines,
        pin,
        shot_circles,
        '</svg>'
      ))
    )
  })

  output$hole_sim_panel_sequence <- renderUI({
    sim <- hole_sim_result()
    if (is.null(sim)) return(div(class = "hole-sim-panel", h4("SHOT SEQUENCE"), tags$p("—", class = "text-muted")))
    outcome <- sim$outcome
    outcome_disp <- sim$outcome_display %||% outcome
    score <- sim$score
    par <- sim$par
    shots <- sim$shots
    outcome_style <- if (outcome == "Eagle" || outcome == "Birdie") "color: var(--golf-green);" else if (outcome == "Bogey" || outcome == "Double+") "color: var(--golf-danger);" else ""
    div(class = "hole-sim-panel",
      h4("SHOT SEQUENCE"),
      div(style = "text-align: right; margin-bottom: 10px;", tags$span(style = paste0("font-size: 0.9rem; font-weight: 700; padding: 4px 10px; ", outcome_style), paste0(score, " (", outcome_disp, ")"))),
      lapply(shots, function(s) {
        det <- if (nzchar(s$result)) paste0(" ", s$result) else ""
        if (nzchar(s$detail)) det <- paste0(det, " ", s$detail)
        tags$div(class = "hole-sim-shot",
          tags$span(style = "font-weight: 700; color: var(--golf-text-muted); margin-right: 8px;", s$num),
          tags$span(s$type),
          tags$span(style = "color: var(--golf-green); margin-left: 6px;", s$dist),
          if (nzchar(det)) tags$span(style = "color: var(--golf-text-muted); margin-left: 6px;", trimws(det))
        )
      })
    )
  })

  # Probability to American odds for display (cap extreme odds)
  prob_to_american <- function(p) {
    p <- suppressWarnings(as.numeric(p))
    if (!is.finite(p) || p <= 0 || p >= 1) return("—")
    p <- pmin(0.99, pmax(0.01, p))
    out <- ifelse(p >= 0.5, -100 * p / (1 - p), 100 * (1 - p) / p)
    out <- pmin(10000, pmax(-10000, out))
    if (!is.finite(out)) return("—")
    ifelse(out > 0, paste0("+", round(out)), as.character(round(out)))
  }
  output$hole_sim_panel_outcomes <- renderUI({
    dist <- hole_outcome_distribution()
    sim <- hole_sim_result()
    if (is.null(dist)) return(div(class = "hole-sim-panel", h4("SCORE ODDS"), tags$p("—", class = "text-muted")))
    probs <- dist$probs
    par_hole <- dist$par
    pred <- hole_hangout_prediction_result()
    pred_score <- if (!is.null(pred) && is.finite(pred$predicted)) sprintf("%.2f", round(pred$predicted, 2)) else "—"
    rows <- list(
      list(name = "Birdie", pct = probs["Birdie"], cl = "ev-positive"),
      list(name = "Par", pct = probs["Par"], cl = ""),
      list(name = "Bogey", pct = probs["Bogey"], cl = "ev-negative"),
      list(name = "Double", pct = probs["Double+"], cl = "ev-negative")
    )
    if (par_hole == 5L) rows <- c(list(list(name = "Eagle", pct = probs["Eagle"], cl = "ev-positive")), rows)
    div(class = "hole-sim-panel",
      h4("SCORE ODDS"),
      tags$p(style = "font-size: 1.1rem; font-weight: 700; margin-bottom: 12px;", paste0("Predicted score: ", pred_score)),
      lapply(rows, function(r) {
        p_raw <- suppressWarnings(as.numeric(r$pct %||% 0))
        if (!is.finite(p_raw) || p_raw < 0) p_raw <- 0
        pct <- p_raw * 100
        price <- prob_to_american(p_raw)
        fill_style <- if (r$name == "Eagle" || r$name == "Birdie") "background: var(--golf-green);" else if (r$name == "Bogey" || r$name == "Double") "background: var(--golf-danger);" else "background: var(--golf-border);"
        tags$div(class = "hole-sim-outcome-row",
          tags$span(style = "min-width: 70px;", r$name),
          tags$div(class = "hole-sim-outcome-bar", tags$div(class = "hole-sim-outcome-fill", style = paste0("width: ", max(0, min(100, pct)), "%; ", fill_style))),
          tags$span(style = "min-width: 52px; text-align: right; font-weight: 600; font-variant-numeric: tabular-nums;", price)
        )
      })
    )
  })

  hole_hangout_history_df <- reactive({
    df <- hole_data_current_event()
    hole_num <- as.integer(input$hole_hangout_hole %||% 1L)
    player <- input$hole_hangout_player
    if (is.null(df) || nrow(df) == 0 || is.null(player) || !nzchar(player)) return(NULL)
    if (!is.finite(hole_num) || hole_num < 1L || hole_num > 18L) hole_num <- 1L
    out <- df %>%
      dplyr::filter(hole == hole_num, player_name == player) %>%
      dplyr::arrange(tournament_name, round) %>%
      dplyr::select(tournament_name, round, score, score_type, par) %>%
      dplyr::distinct()
    if (nrow(out) == 0) return(NULL)
    out
  })

  output$hole_hangout_history_title <- renderUI({ NULL })

  output$hole_hangout_history <- renderDT({
    datatable(
      data.frame(),
      rownames = FALSE,
      options = list(pageLength = -1, lengthChange = FALSE, dom = "t"),
      class = "stripe hover"
    )
  })

}

# Opt-in: set GOLF_REFRESH_HISTORICAL_ON_START=1 to merge latest DataGolf rounds into
# data/historical_rounds_all.csv once when the Shiny process starts (local use; not shinyapps.io).
shinyApp(
  ui,
  server,
  onStart = function() {
    if (!identical(Sys.getenv("GOLF_REFRESH_HISTORICAL_ON_START", ""), "1")) return()
    if (!nzchar(Sys.getenv("DATAGOLF_API_KEY", ""))) {
      message("GOLF_REFRESH_HISTORICAL_ON_START=1 but DATAGOLF_API_KEY is empty; skipping live_data refresh.")
      return()
    }
    if (!file.exists(live_data_path)) return()
    message("GOLF_REFRESH_HISTORICAL_ON_START: updating data/historical_rounds_all.csv …")
    tryCatch(
      {
        source(live_data_path, local = FALSE)
        update_historical_rounds_live()
        message("GOLF_REFRESH_HISTORICAL_ON_START: done.")
      },
      error = function(e) {
        warning("GOLF_REFRESH_HISTORICAL_ON_START failed: ", conditionMessage(e))
      }
    )
  }
)
