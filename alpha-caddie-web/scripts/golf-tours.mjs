/**
 * Shared tour codes for live projections and historical merges.
 * DataGolf uses `euro` for DP World Tour.
 */

/** Tours published for live round projections (dual JSON files). */
export const LIVE_PROJECTION_TOURS = ["pga", "euro"];

/** Tours merged into historical_rounds_all.csv and player history. */
export const HISTORY_TOURS = ["pga", "liv", "euro"];

export function normalizeTourCode(tour) {
  return String(tour || "pga").trim().toLowerCase() || "pga";
}

export function isHistoryTour(tour) {
  return HISTORY_TOURS.includes(normalizeTourCode(tour));
}

export function isLiveProjectionTour(tour) {
  return LIVE_PROJECTION_TOURS.includes(normalizeTourCode(tour));
}

export function tourDisplayLabel(tour) {
  const t = normalizeTourCode(tour);
  if (t === "euro") return "DP World Tour";
  if (t === "liv") return "LIV Golf";
  if (t === "opp") return "PGA (opposite field)";
  return "PGA Tour";
}

export function projectionsFilename(tour) {
  const t = normalizeTourCode(tour);
  return t === "pga" ? "projections-pga.json" : `projections-${t}.json`;
}

export function liveInPlayFilename(tour) {
  const t = normalizeTourCode(tour);
  return t === "pga" ? "live-in-play-pga.json" : `live-in-play-${t}.json`;
}

/** Legacy single-file names (PGA alias for backward compatibility). */
export const LEGACY_PROJECTIONS_FILE = "projections.json";
export const LEGACY_LIVE_IN_PLAY_FILE = "live-in-play.json";

export function fieldUpdatesTourCandidates(tour, env = process.env) {
  const raw = String(env.GOLF_FIELD_UPDATES_TOUR_CANDIDATES || "").trim();
  if (raw) {
    return [...new Set(raw.split(/[,;\s]+/).map((x) => x.trim().toLowerCase()).filter(Boolean))];
  }
  const t = normalizeTourCode(tour);
  if (t === "pga") return ["pga", "opp"];
  return [t];
}

export function tourFromEnv(env = process.env) {
  return normalizeTourCode(env.GOLF_DATAGOLF_TOUR || env.GOLF_TOUR || "pga");
}
