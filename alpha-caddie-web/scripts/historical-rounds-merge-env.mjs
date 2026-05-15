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
