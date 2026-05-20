/**
 * Calendar day for a tournament round row.
 * DataGolf historical-raw-data uses one event_completed (last day of event) for all rounds;
 * live / pgatouR rows store each round's real M/D/Y.
 */

export function parseEventCompletedChronoBase(s) {
  if (!s) return 0;
  const t = String(s).trim();
  const iso = t.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) {
    const y = parseInt(iso[1], 10);
    const mo = parseInt(iso[2], 10);
    const d = parseInt(iso[3], 10);
    if (Number.isFinite(y)) return y * 10000 + (mo || 0) * 100 + (d || 0);
  }
  const p = t.split("/").map((x) => x.trim());
  if (p.length !== 3) return 0;
  const mo = parseInt(p[0], 10);
  const d = parseInt(p[1], 10);
  let y = parseInt(p[2], 10);
  if (!Number.isFinite(y)) return 0;
  if (y < 100) y += y >= 70 ? 1900 : 2000;
  return y * 10000 + (mo || 0) * 100 + (d || 0);
}

export function eventStrokeRoundCap(tour) {
  return String(tour || "").toLowerCase() === "liv" ? 3 : 4;
}

/** M/D/YYYY from YYYYMMDD chrono base + day offset (UTC). */
export function mdyFromChronoBase(chronoBase, dayOffset) {
  if (!Number.isFinite(chronoBase) || chronoBase <= 0) return "";
  const y = Math.floor(chronoBase / 10000);
  const mo = Math.floor((chronoBase % 10000) / 100);
  const d = chronoBase % 100;
  const ms = Date.UTC(y, mo - 1, d) + Math.round(Number(dayOffset) || 0) * 86400000;
  const dt = new Date(ms);
  return `${dt.getUTCMonth() + 1}/${dt.getUTCDate()}/${dt.getUTCFullYear()}`;
}

/** True when row came from DG CSV with shared event-level event_completed. */
export function historyRowUsesEventEndAnchor(row) {
  if (!row || typeof row !== "object") return false;
  if (row._from_live_tournament_stats || row._from_pgatour || row._from_live_in_play) return false;
  if (row._from_dg_historical_rounds) return true;
  const ec = String(row.event_completed || "").trim();
  if (!ec) return false;
  const sk = Number(row.sortKey);
  if (Number.isFinite(sk) && sk > 9_999_999) {
    const skBase = Math.floor(sk / 10);
    const ecBase = parseEventCompletedChronoBase(ec);
    return ecBase > 0 && skBase === ecBase;
  }
  return true;
}

/** Day offset from event_completed anchor: CSV = last day of event (R4 on anchor day). */
export function historyRoundDayOffsetFromEventAnchor(row) {
  const rnd = Math.round(Number(row?.round_num));
  if (!Number.isFinite(rnd) || rnd < 1) return 0;
  if (!historyRowUsesEventEndAnchor(row)) return 0;
  const cap = eventStrokeRoundCap(row.tour);
  return -(Math.max(0, cap - rnd));
}

/** Play date M/D/YYYY for charts and filters. */
export function historyRoundPlayMdY(row) {
  if (!row || typeof row !== "object") return "";
  const ec = String(row.event_completed || "").trim();
  const offset = historyRoundDayOffsetFromEventAnchor(row);
  if (offset !== 0 && ec) {
    const base = parseEventCompletedChronoBase(ec);
    if (base > 0) return mdyFromChronoBase(base, offset);
  }
  return ec;
}

export function historyRoundChartUtcIsoDay(row) {
  const mdy = historyRoundPlayMdY(row);
  const base = parseEventCompletedChronoBase(mdy);
  if (!base) return "";
  const y = Math.floor(base / 10000);
  const mo = Math.floor((base % 10000) / 100);
  const d = base % 100;
  return `${y}-${String(mo).padStart(2, "0")}-${String(d).padStart(2, "0")}`;
}

export function roundEventCompletedMdYFromEventEnd(eventCompleted, roundNum, tour) {
  const ec = String(eventCompleted || "").trim();
  const base = parseEventCompletedChronoBase(ec);
  if (!base) return ec;
  const rnd = Math.round(Number(roundNum));
  if (!Number.isFinite(rnd) || rnd < 1) return ec;
  const cap = eventStrokeRoundCap(tour);
  return mdyFromChronoBase(base, -(Math.max(0, cap - rnd)));
}
