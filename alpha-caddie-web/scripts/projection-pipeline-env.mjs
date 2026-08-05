/**
 * Shared env for push:live / backtest projection pipelines.
 *
 * Default recipe (no sportsbook calibration):
 *   skill = last 12 rounds (decay) blended lightly toward year baseline
 *   course fit = venue SG-importance (OTT/APP/ARG/PUTT) + per-course hist calib
 *   weather by tee wave for all markets
 */

export function flatVenueProjectionPipelineEnv() {
  return {
    GOLF_FLAT_VENUE_PLAYER_SCORE: "1",
    GOLF_FLAT_VENUE_MAX_PLAYER_SCORE_WEIGHT: "0.06",
    GOLF_SCORE_SKILL_KEEP: "1",
    GOLF_SCORE_PLAYER_COURSE_MAX_W: "0.06",
    GOLF_MARKET_BOOK_CALIBRATION: "0",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "0",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0",
    GOLF_UNIFIED_BOUNCE_BACK_K: "0",
  };
}

/** Walk-forward backtest: skill12+year, course SG fit, wave weather — never book-calibrated. */
export function walkforwardBacktestPipelineEnv() {
  return {
    ...flatVenueProjectionPipelineEnv(),
    GOLF_FLAT_VENUE_PLAYER_SCORE: "0",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "1",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0.1",
    GOLF_WITHIN_EVENT_FORM_CAP: "0.75",
    // Primary skill window = last 12; year blend pulls lightly toward season baseline.
    GOLF_WF_SKILL_MAX_ROUNDS: process.env.GOLF_WF_SKILL_MAX_ROUNDS || "12",
    GOLF_WF_YEAR_ROUNDS: process.env.GOLF_WF_YEAR_ROUNDS || "48",
    GOLF_WF_YEAR_BLEND: process.env.GOLF_WF_YEAR_BLEND || "0.18",
    GOLF_WF_SKILL_DECAY: process.env.GOLF_WF_SKILL_DECAY || "0.86",
    GOLF_WF_YEAR_DECAY: process.env.GOLF_WF_YEAR_DECAY || "0.92",
    GOLF_MARKET_BOOK_CALIBRATION: "0",
    GOLF_OUTCOME_MU_DEBIAS: "0",
    GOLF_EXPORT_RAW_MODEL_MU: "1",
    GOLF_COURSE_SG_FIT: process.env.GOLF_COURSE_SG_FIT || "1",
    GOLF_UNIFIED_TEE_WAVE_W: process.env.GOLF_UNIFIED_TEE_WAVE_W || "0.30",
    // Player×course×hole SG from shot traces (cutoff-aware plays file).
    GOLF_HOLE_SG_BLEND: process.env.GOLF_HOLE_SG_BLEND || "1",
    GOLF_HOLE_SG_WEIGHT: process.env.GOLF_HOLE_SG_WEIGHT || "0.28",
    GOLF_DISTANCE_SG_BLEND: process.env.GOLF_DISTANCE_SG_BLEND || "1",
    GOLF_DISTANCE_SG_WEIGHT: process.env.GOLF_DISTANCE_SG_WEIGHT || "0.42",
    GOLF_DISTANCE_SG_COURSE_FOCUS: process.env.GOLF_DISTANCE_SG_COURSE_FOCUS || "0.88",
    GOLF_WF_WEATHER: process.env.GOLF_WF_WEATHER || "1",
  };
}

/**
 * Live week = OOS-winning reconstruction + skill-first score/GIR/FW origins.
 * Birdies/Bogeys markets always use birdie-or-better / bogey-or-worse.
 * Detroit North/South pools club hist in loadVenueHistoricalScoring (layout STP shift).
 */
export function liveProjectionPipelineEnv() {
  return {
    ...walkforwardBacktestPipelineEnv(),
    GOLF_FLAT_VENUE_PLAYER_SCORE: "0",
    GOLF_FLAT_VENUE_MAX_PLAYER_SCORE_WEIGHT: "0.06",
    GOLF_SCORE_SKILL_KEEP: process.env.GOLF_SCORE_SKILL_KEEP || "1",
    GOLF_SCORE_PLAYER_COURSE_MAX_W: process.env.GOLF_SCORE_PLAYER_COURSE_MAX_W || "0.06",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "1",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0.1",
    GOLF_WITHIN_EVENT_FORM_CAP: "0.75",
    GOLF_UNIFIED_BOUNCE_BACK_K: process.env.GOLF_UNIFIED_BOUNCE_BACK_K || "0.12",
    GOLF_WF_SKILL_MAX_ROUNDS: process.env.GOLF_WF_SKILL_MAX_ROUNDS || "12",
    GOLF_WF_YEAR_ROUNDS: process.env.GOLF_WF_YEAR_ROUNDS || "48",
    GOLF_WF_YEAR_BLEND: process.env.GOLF_WF_YEAR_BLEND || "0.18",
    GOLF_MARKET_BOOK_CALIBRATION: process.env.GOLF_MARKET_BOOK_CALIBRATION || "0",
    GOLF_SKIP_MARKET_BOOK_CALIBRATION: process.env.GOLF_SKIP_MARKET_BOOK_CALIBRATION || "1",
    GOLF_OUTCOME_MU_DEBIAS: process.env.GOLF_OUTCOME_MU_DEBIAS || "0",
    GOLF_EXPORT_RAW_MODEL_MU: process.env.GOLF_EXPORT_RAW_MODEL_MU || "1",
    GOLF_COURSE_SG_FIT: process.env.GOLF_COURSE_SG_FIT || "1",
    GOLF_UNIFIED_TEE_WAVE_W: process.env.GOLF_UNIFIED_TEE_WAVE_W || "0.30",
    GOLF_FIELD_DAY_COUNTING_LIFT_FRAC: process.env.GOLF_FIELD_DAY_COUNTING_LIFT_FRAC || "0",
    GOLF_WITHIN_EVENT_COUNTING_BLEND: process.env.GOLF_WITHIN_EVENT_COUNTING_BLEND || "0",
    GOLF_HOLE_SG_BLEND: process.env.GOLF_HOLE_SG_BLEND || "1",
    GOLF_HOLE_SG_WEIGHT: process.env.GOLF_HOLE_SG_WEIGHT || "0.28",
    GOLF_DISTANCE_SG_BLEND: process.env.GOLF_DISTANCE_SG_BLEND || "1",
    GOLF_DISTANCE_SG_WEIGHT: process.env.GOLF_DISTANCE_SG_WEIGHT || "0.42",
    GOLF_DISTANCE_SG_COURSE_FOCUS: process.env.GOLF_DISTANCE_SG_COURSE_FOCUS || "0.88",
    GOLF_WF_WEATHER: process.env.GOLF_WF_WEATHER || "1",
  };
}

/** DraftKings / Caesars / FanDuel round O/U scrape — headed browser on desktop (books often block headless Chromium). */
export function dkOuScrapeEnv() {
  const env = {};
  if (process.platform === "win32" || process.platform === "darwin") {
    env.DK_HEADLESS = "0";
    env.CZR_HEADLESS = "0";
    env.FD_HEADLESS = "0";
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
  if (String(process.env.GOLF_REQUIRE_DK_OU || "").trim() !== "") {
    return {};
  }
  return { GOLF_REQUIRE_DK_OU: "1" };
}
