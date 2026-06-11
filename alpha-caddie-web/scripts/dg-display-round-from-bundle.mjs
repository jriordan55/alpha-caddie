/** Shared helpers: tournament round from merged live bundle (field_updates + live_hole_stats + preds/in-play). */

import { eventsLikelySame } from "./dg-events-align.mjs";

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

export function tournamentStartDayUtc(dateStartIso) {
  const m = String(dateStartIso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return NaN;
  return Date.UTC(+m[1], +m[2] - 1, +m[3]);
}

export function todayUtc() {
  const now = new Date();
  return Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate());
}

/** True on the UTC calendar day of field `date_start` (R1 tee sheet day). */
export function isTournamentStartDay(dateStartIso) {
  const start = tournamentStartDayUtc(dateStartIso);
  return Number.isFinite(start) && todayUtc() === start;
}

function bundleEventName(obj) {
  return String(obj?.event_name || obj?.eventName || "").trim();
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
export function maxTournamentRoundFromLiveBundle(live, fieldRaw, liveHoleStats, opts = {}) {
  const trustEvent = String(
    opts.trustEvent || bundleEventName(fieldRaw) || bundleEventName(live?.field_updates) || "",
  ).trim();

  let best = maxRoundFromFieldAndLiveHole(fieldRaw, liveHoleStats);
  if (!live || typeof live !== "object") return best;

  const infoEv = bundleEventName(live.info);
  const infoTrusted = !trustEvent || !infoEv || eventsLikelySame(trustEvent, infoEv);
  if (!infoTrusted) {
    const fieldOnly = maxRoundFromFieldAndLiveHole(fieldRaw, null);
    return Number.isFinite(fieldOnly) ? fieldOnly : best;
  }

  best = considerRound(live.info?.current_round, best);
  best = considerRound(live.current_round, best);
  const rows = Array.isArray(live.data) ? live.data : [];
  for (const r of rows) {
    best = considerRound(r?.round ?? r?.Round, best);
  }
  return best;
}

export function projectionDateStartIso(payload) {
  const meta = payload?._projection_export_meta;
  return String(
    payload?.datagolf_field_date_start ??
      meta?.datagolf_field_date_start ??
      payload?.date_start ??
      "",
  ).trim();
}

/**
 * Pre-tournament (field date_start in the future): pin sheet targets the sheet's round (usually R1),
 * not a stale display_round bumped by old live-in-play rows.
 */
export function effectiveDisplayRoundForPinSheet(payload, sheetRoundHint) {
  const ds = projectionDateStartIso(payload);
  const sheetR = Math.round(num(sheetRoundHint, NaN));
  if (dateStartIsFuture(ds) || (sheetR === 1 && isTournamentStartDay(ds))) {
    return Number.isFinite(sheetR) && sheetR >= 1 && sheetR <= 4 ? sheetR : 1;
  }
  const r = Math.round(
    num(payload?.display_round ?? payload?._projection_export_meta?.display_round, NaN),
  );
  return Number.isFinite(r) && r >= 1 && r <= 4 ? r : 1;
}

export function exportDisplayRoundFromLiveBundle(live, fieldRaw, liveHoleStats, opts = {}) {
  const ds = String(
    opts.projDateStart ||
      fieldRaw?.date_start ||
      fieldRaw?.dateStart ||
      live?.field_updates?.date_start ||
      "",
  ).trim();
  if (dateStartIsFuture(ds)) return 1;
  const m = maxTournamentRoundFromLiveBundle(live, fieldRaw, liveHoleStats, opts);
  if (Number.isFinite(m) && m >= 1 && m <= 4) return Math.round(m);
  return 1;
}
