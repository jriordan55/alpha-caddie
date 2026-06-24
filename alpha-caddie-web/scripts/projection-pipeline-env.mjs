/**
 * Shared env for push:live / backtest: same all-time venue player score every round;
 * round separation from weather, pin sheet, and tee wave only.
 */
export function flatVenueProjectionPipelineEnv() {
  return {
    GOLF_FLAT_VENUE_PLAYER_SCORE: "1",
    GOLF_FLAT_VENUE_MAX_PLAYER_SCORE_WEIGHT: "0.38",
    GOLF_MARKET_BOOK_CALIBRATION: "1",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "0",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0",
    GOLF_UNIFIED_BOUNCE_BACK_K: "0",
  };
}
