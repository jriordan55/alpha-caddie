/**
 * Shared field names + helpers for round-level approach SG by distance/lie bucket.
 * Built by build-round-sg-by-distance.mjs; merged into player-history shards.
 */

export const SG_DISTANCE_BUCKETS = [
  "50_100_fw",
  "100_150_fw",
  "150_200_fw",
  "over_200_fw",
  "under_150_rgh",
  "over_150_rgh",
];

/** History / CSV keys written onto each round row. */
export const SG_DISTANCE_VALUE_KEYS = [
  ...SG_DISTANCE_BUCKETS.map((b) => `sg_${b}`),
  "sg_app_dist_total",
];

export const SG_DISTANCE_COUNT_KEYS = [
  ...SG_DISTANCE_BUCKETS.map((b) => `n_${b}`),
  "n_app_dist",
];

export const SG_DISTANCE_ALL_KEYS = [...SG_DISTANCE_VALUE_KEYS, ...SG_DISTANCE_COUNT_KEYS];

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

/** Apply finite distance-SG fields from `src` onto `row` (mutates and returns row). */
export function applyDistSgFields(row, src) {
  if (!row || !src) return row;
  for (const k of SG_DISTANCE_ALL_KEYS) {
    const v = num(src[k], NaN);
    if (Number.isFinite(v)) row[k] = v;
  }
  if (src._from_sg_distance) row._from_sg_distance = true;
  return row;
}

/**
 * Index keys for a distance-SG CSV row or history round.
 * Prefer dg_id + year + event name + round; also tournament_id when present.
 */
/** Build R{year}{eventId} variants from history-style numeric event_id. */
export function syntheticTourIds(year, eventId) {
  const yr = Math.round(num(year, NaN));
  const eid = String(eventId || "").trim();
  if (!Number.isFinite(yr) || !eid || /^R/i.test(eid)) return [];
  if (!/^\d+$/.test(eid)) return [];
  const out = [`R${yr}${eid}`];
  const pad3 = eid.padStart(3, "0");
  if (pad3 !== eid) out.push(`R${yr}${pad3}`);
  return out;
}

export function distSgLookupKeys(rec) {
  const dg = Math.round(num(rec.dg_id, NaN));
  const rnd = Math.round(num(rec.round ?? rec.round_num, NaN));
  const yr = Math.round(num(rec.year, NaN));
  const keys = [];
  if (Number.isFinite(dg) && Number.isFinite(rnd)) {
    const evt = normEvt(rec.tournament_name || rec.event_name);
    if (Number.isFinite(yr) && evt) keys.push(`n|${dg}|${yr}|${evt}|${rnd}`);
    const tid = String(rec.tournament_id || rec.event_id || "").trim();
    if (tid && /^R/i.test(tid)) keys.push(`t|${dg}|${tid}|${rnd}`);
    for (const syn of syntheticTourIds(yr, rec.event_id || rec.tournament_id)) {
      keys.push(`t|${dg}|${syn}|${rnd}`);
    }
  }
  return keys;
}

/** Build Map of lookup key → plain object of SG_DISTANCE_ALL_KEYS (+ meta). */
export function indexDistSgRows(rows) {
  /** @type {Map<string, object>} */
  const idx = new Map();
  for (const r of rows) {
    const payload = { _from_sg_distance: true };
    let any = false;
    for (const k of SG_DISTANCE_ALL_KEYS) {
      const v = num(r[k], NaN);
      if (Number.isFinite(v)) {
        payload[k] = v;
        any = true;
      }
    }
    if (!any) continue;
    for (const key of distSgLookupKeys(r)) {
      if (!idx.has(key)) idx.set(key, payload);
    }
  }
  return idx;
}

export function findDistSgForRound(idx, roundRec) {
  if (!idx?.size) return null;
  for (const key of distSgLookupKeys(roundRec)) {
    const hit = idx.get(key);
    if (hit) return hit;
  }
  return null;
}
