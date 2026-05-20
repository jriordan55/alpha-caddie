/**
 * When nothing else is set, cap DataGolf historical-raw-data/rounds API pulls to the last N calendar years
 * (older rows already on disk are kept). Avoids the default 2004→present sweep on every `fetch:dg` / `update:rounds`.
 *
 * Opt in to the old uncapped behavior: GOLF_HISTORICAL_ROUNDS_FETCH_ALL_YEARS=1
 * Or: GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1, or set GOLF_HISTORICAL_ROUNDS_YEARS explicitly.
 *
 * Default N from GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS_DEFAULT (falls back to 3).
 */
export function applyHistoricalRoundsMergeDefaults(base) {
  const out = { ...base };
  const full = String(out.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY || "").trim() === "1";
  const years = String(out.GOLF_HISTORICAL_ROUNDS_YEARS || "").trim();
  const rf = String(out.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "").trim();
  const fetchAll = String(out.GOLF_HISTORICAL_ROUNDS_FETCH_ALL_YEARS || "").trim() === "1";
  if (!full && !years && !fetchAll && (!rf || rf === "0")) {
    const def = parseInt(String(out.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS_DEFAULT || "3").trim(), 10);
    const n = Number.isFinite(def) && def > 0 ? def : 3;
    out.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS = String(n);
    console.log(
      `[historical rounds merge] Default GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=${n} (API only for the last ${n} calendar years; older CSV rows preserved). For 2004–present set GOLF_HISTORICAL_ROUNDS_FETCH_ALL_YEARS=1 or GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1.`,
    );
  }
  return out;
}

/**
 * Env for `build-player-history` on live-week pushes (faster; still merges live LTS + pgatouR).
 * @param {{ defaultLiveFast?: boolean }} [opts] — `refresh:live` passes `{ defaultLiveFast: true }`.
 */
export function fastHistoryBuildEnv(opts = {}) {
  if (String(process.env.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY || "").trim() === "1") return {};
  const liveFast = String(process.env.GOLF_REFRESH_LIVE_FAST_HISTORY ?? "").trim();
  const appFast = String(process.env.GOLF_REFRESH_APP_FAST_HISTORY || "").trim() === "1";
  const useFast =
    appFast || liveFast === "1" || (opts.defaultLiveFast === true && liveFast !== "0");
  if (!useFast) return {};
  const cy = new Date().getFullYear();
  const defMin = Math.max(2010, cy - 10);
  const out = { GOLF_SKIP_SHOTS_ROUND_AGG_MERGE: "1" };
  /** Hole Hangout only — scanning ~170MB hole_data.csv can look hung for 15–30+ min. */
  if (String(process.env.GOLF_BUILD_HISTORY_SKIP_HOLES || "").trim() !== "0") {
    out.HOLE_DATA_CSV = "";
  }
  if (!String(process.env.GOLF_HISTORY_MIN_YEAR ?? "").trim()) {
    out.GOLF_HISTORY_MIN_YEAR = String(defMin);
  }
  if (!String(process.env.GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER ?? "").trim()) {
    out.GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER = "500";
  }
  return out;
}
