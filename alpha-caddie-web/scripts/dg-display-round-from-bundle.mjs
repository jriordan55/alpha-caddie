/** Shared helpers: tournament round from merged live bundle (field_updates + live_hole_stats + preds/in-play). */

export function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

/** True when field `date_start` (YYYY-MM-DD) is after today's UTC calendar day. */
export function dateStartIsFuture(dateStartIso) {
  const m = String(dateStartIso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return false;
  const start = Date.UTC(+m[1], +m[2] - 1, +m[3]);
  const now = new Date();
  const today = Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate());
  return Number.isFinite(start) && start > today;
}

/**
 * Highest plausible on-course round in 1..4 across DataGolf sources.
 * field-updates can lag preds/in-play / live-hole-stats after a rollover — use max(),
 * never "first finite wins".
 */
export function maxRoundFromFieldAndLiveHole(fieldRaw, liveHoleStats) {
  let best = NaN;
  for (const raw of [
    fieldRaw?.current_round ?? fieldRaw?.CurrentRound,
    liveHoleStats?.current_round ?? liveHoleStats?.info?.current_round,
  ]) {
    const rn = Math.round(num(raw, NaN));
    if (!Number.isFinite(rn) || rn < 1 || rn > 4) continue;
    best = Number.isFinite(best) ? Math.max(best, rn) : rn;
  }
  return best;
}

function considerRound(candidate, bump) {
  const rn = Math.round(num(candidate, NaN));
  if (!Number.isFinite(rn) || rn < 1 || rn > 4) return bump;
  return Number.isFinite(bump) ? Math.max(bump, rn) : rn;
}

/**
 * Reads preds/in-play `info`, root, and each `data` row round (moving-day signal when field_updates lags).
 * `live` is the full object written as live-in-play.json (spread parsed + field_updates + live_hole_stats).
 */
export function maxTournamentRoundFromLiveBundle(live, fieldRaw, liveHoleStats) {
  let best = maxRoundFromFieldAndLiveHole(fieldRaw, liveHoleStats);
  if (!live || typeof live !== "object") return best;
  best = considerRound(live.info?.current_round, best);
  best = considerRound(live.current_round, best);
  const rows = Array.isArray(live.data) ? live.data : [];
  for (const r of rows) {
    best = considerRound(r?.round ?? r?.Round, best);
  }
  return best;
}

export function exportDisplayRoundFromLiveBundle(live, fieldRaw, liveHoleStats) {
  const ds = String(
    fieldRaw?.date_start ?? fieldRaw?.dateStart ?? live?.field_updates?.date_start ?? "",
  ).trim();
  if (dateStartIsFuture(ds)) return 1;
  const m = maxTournamentRoundFromLiveBundle(live, fieldRaw, liveHoleStats);
  if (Number.isFinite(m) && m >= 1 && m <= 4) return Math.round(m);
  return 1;
}
