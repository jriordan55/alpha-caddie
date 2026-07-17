/**
 * Shared env for push:live / backtest: same all-time venue player score every round;
 * round separation from weather, pin sheet, and tee wave only.
 */

export function flatVenueProjectionPipelineEnv() {
  return {
    GOLF_FLAT_VENUE_PLAYER_SCORE: "1",
    GOLF_FLAT_VENUE_MAX_PLAYER_SCORE_WEIGHT: "0.38",
    GOLF_MARKET_BOOK_CALIBRATION: "0",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "0",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0",
    GOLF_UNIFIED_BOUNCE_BACK_K: "0",
  };
}

/** Walk-forward backtest + live projections: historical venue anchor + prior-round difficulty + within-event form. */
export function walkforwardBacktestPipelineEnv() {
  return {
    ...flatVenueProjectionPipelineEnv(),
    GOLF_FLAT_VENUE_PLAYER_SCORE: "0",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "1",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0.1",
    GOLF_WITHIN_EVENT_FORM_CAP: "0.75",
  };
}

/** Alias — live week uses the same μ engine as honest walk-forward backtest. */
export function liveProjectionPipelineEnv() {
  return walkforwardBacktestPipelineEnv();
}

/** DraftKings round O/U scrape — headed browser on desktop (DK blocks headless Chromium). */
export function dkOuScrapeEnv() {
  const env = {};
  if (process.platform === "win32" || process.platform === "darwin") {
    env.DK_HEADLESS = "0";
  }
  return env;
}

/**
 * push:live / refresh:live — fail if DraftKings scrape returns 0 fresh round O/U props.
 * Mid-tournament (`GOLF_LIVE_WEEK_SOFT=1` or explicit `GOLF_REQUIRE_DK_OU=0`): keep prior lines and continue.
 */
export function requireDkOuEnv() {
  const soft =
    String(process.env.GOLF_LIVE_WEEK_SOFT || "").trim() === "1" ||
    String(process.env.GOLF_SKIP_DK_OU_REQUIRE || "").trim() === "1";
  if (soft) {
    return {
      GOLF_REQUIRE_DK_OU: "0",
      GOLF_SKIP_DK_OU_VALIDATE: process.env.GOLF_SKIP_DK_OU_VALIDATE || "1",
    };
  }
  // Honor explicit caller override (LiveWeekOnly sets GOLF_REQUIRE_DK_OU=0 before refresh:live).
  if (String(process.env.GOLF_REQUIRE_DK_OU || "").trim() !== "") {
    return {};
  }
  return { GOLF_REQUIRE_DK_OU: "1" };
}
