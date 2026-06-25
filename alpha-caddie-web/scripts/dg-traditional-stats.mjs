/**
 * DataGolf traditional-stat rates (GIR%, fairways%) for round projections.
 *
 * Sources (priority):
 * 1. preds/live-tournament-stats `gir` / `accuracy` when in-play counting refresh
 * 2. preds/skill-ratings `driving_acc` / SG pillars (best player spread)
 * 3. Cached `dg_*_pct` from rolling traditional CSV
 * 4. SG:APP / SG:OTT curves (fallback)
 */

import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { existsSync } from "fs";

export const DG_TOUR_AVG_FAIRWAY_RATE = 0.6;
export const DG_TOUR_AVG_GIR_RATE = 0.65;

export function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

/** 0–1 fairway or GIR rate from a scalar (fraction, count on n holes, or 0–100 percent). */
export function traditionalRate01(raw, nHoles = 18) {
  const v = num(raw, NaN);
  const nh = num(nHoles, 18);
  if (!Number.isFinite(v) || !Number.isFinite(nh) || nh <= 0) return NaN;
  if (v > 0.05 && v <= 1.0001) return v;
  if (v > 1 && v <= nh + 0.51) return v / nh;
  if (v > nh && v <= 100) return v / 100;
  return NaN;
}

/** skill-ratings `driving_acc` / `driving_accuracy`: pp vs field, not always a 0–1 fairway share. */
export function fairwayRate01FromSkillRatingsPp(skRow, tourAvg = DG_TOUR_AVG_FAIRWAY_RATE, nFw = 14) {
  if (!skRow || typeof skRow !== "object") return NaN;
  const nh = Math.round(num(nFw, 14)) || 14;

  const accPp = num(skRow.driving_acc, NaN);
  if (Number.isFinite(accPp) && accPp > -0.55 && accPp < 0.55) {
    return Math.max(0.35, Math.min(0.88, tourAvg + accPp));
  }

  const acc = num(skRow.driving_accuracy, NaN);
  if (Number.isFinite(acc)) {
    if (acc >= 2 && acc <= nh + 1) return acc / nh;
    if (acc > 0.15 && acc < 0.88) return acc;
    if (acc > 1 && acc <= 100) return acc / 100;
  }

  for (const a of [accPp, acc].filter((x) => Number.isFinite(x))) {
    if (a > 0.15 && a < 0.88) return a;
  }
  return NaN;
}

/**
 * @param {object | null | undefined} liveTrad — { gir?, accuracy? } from live-tournament-stats
 */
export function fairwayRate01FromDg(skRow, liveTrad, nFw = 14) {
  if (liveTrad && typeof liveTrad === "object") {
    const live = traditionalRate01(liveTrad.accuracy, nFw);
    if (Number.isFinite(live)) return live;
  }
  if (skRow && typeof skRow === "object") {
    const fromSkill = fairwayRate01FromSkillRatingsPp(skRow, DG_TOUR_AVG_FAIRWAY_RATE, nFw);
    if (Number.isFinite(fromSkill)) return fromSkill;
    const cached = num(skRow.dg_fairway_pct, NaN);
    if (Number.isFinite(cached) && cached >= 0.15 && cached <= 0.88) return cached;
  }
  return NaN;
}

export function girRate01FromSgApp(muSg, sgApp, fieldMeanApp, tourAvg = DG_TOUR_AVG_GIR_RATE) {
  const a = num(sgApp, NaN);
  const m = num(fieldMeanApp, NaN);
  if (!Number.isFinite(a) || !Number.isFinite(m)) return NaN;
  const mu = num(muSg, 0);
  let rate = tourAvg + 0.34 * (a - m) + 0.04 * mu;
  return Math.max(0.48, Math.min(0.82, rate));
}

export function girRate01FromDg(skRow, liveTrad, opts = {}) {
  if (liveTrad && typeof liveTrad === "object") {
    const live = traditionalRate01(liveTrad.gir, 18);
    if (Number.isFinite(live)) return live;
  }
  if (skRow && typeof skRow === "object") {
    const fromSg = girRate01FromSgApp(opts.muSg, skRow.sg_app, opts.fieldMeanApp);
    if (Number.isFinite(fromSg)) return fromSg;
    for (const k of ["gir_pct", "gir", "greens_in_regulation", "greens_in_regulation_pct", "gir_percent"]) {
      const r = traditionalRate01(skRow[k], 18);
      if (Number.isFinite(r)) return r;
    }
    const cached = num(skRow.dg_gir_pct, NaN);
    if (Number.isFinite(cached) && cached >= 0.15 && cached <= 0.95) return cached;
  }
  return NaN;
}

export function fairwayHitsFromRate01(rate01, nFw) {
  const r = num(rate01, NaN);
  const n = num(nFw, NaN);
  if (!Number.isFinite(r) || !Number.isFinite(n) || n <= 0) return NaN;
  const x = r * n;
  if (x <= 0) return 0;
  if (x >= n) return n;
  return x;
}

export function girHitsFromRate01(rate01, nGir = 18) {
  return fairwayHitsFromRate01(rate01, nGir);
}

/**
 * Rolling PGA/LIV traditional rates per dg_id from DataGolf historical rounds export.
 * @param {string} csvPath
 * @param {Set<number>} dgIdSet
 */
export async function buildRollingTraditionalPctByDg(csvPath, dgIdSet, opts = {}) {
  const maxR = Math.max(8, Math.round(num(opts.maxRoundsPerPlayer, 36)));
  const cy = new Date().getFullYear();
  const minYear = cy - 2;
  /** @type {Map<number, { gir: number[], fw: number[] }>} */
  const buf = new Map();
  if (!existsSync(csvPath) || !dgIdSet?.size) return new Map();

  await new Promise((resolve, reject) => {
    const stream = createReadStream(csvPath).pipe(
      parse({
        columns: true,
        relax_quotes: true,
        relax_column_count: true,
        skip_records_with_error: true,
      }),
    );
    stream.on("data", (row) => {
      const tour = String(row.tour || "").toLowerCase();
      if (tour !== "pga" && tour !== "liv") return;
      const yr = parseInt(row.year, 10);
      if (Number.isFinite(yr) && yr < minYear) return;
      const id = Math.round(num(row.dg_id, NaN));
      if (!Number.isFinite(id) || !dgIdSet.has(id)) return;
      let slot = buf.get(id);
      if (!slot) {
        slot = { gir: [], fw: [] };
        buf.set(id, slot);
      }
      const girR = traditionalRate01(row.gir, 18);
      if (Number.isFinite(girR) && slot.gir.length < maxR) slot.gir.push(girR);
      const fwR = traditionalRate01(row.driving_acc, 14);
      if (Number.isFinite(fwR) && slot.fw.length < maxR) slot.fw.push(fwR);
    });
    stream.on("end", resolve);
    stream.on("error", reject);
  });

  const mean = (arr) => (arr.length ? arr.reduce((s, x) => s + x, 0) / arr.length : NaN);
  /** @type {Map<number, { girRate01: number, fwRate01: number, nGir: number, nFw: number }>} */
  const out = new Map();
  for (const [id, slot] of buf) {
    const girRate01 = mean(slot.gir);
    const fwRate01 = mean(slot.fw);
    if (Number.isFinite(girRate01) || Number.isFinite(fwRate01)) {
      out.set(id, {
        girRate01,
        fwRate01,
        nGir: slot.gir.length,
        nFw: slot.fw.length,
      });
    }
  }
  return out;
}
