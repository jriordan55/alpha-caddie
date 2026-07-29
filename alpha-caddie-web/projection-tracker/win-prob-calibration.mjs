/**
 * Map raw model P(win) → empirically calibrated confidence (reliability curve).
 * Artifact: ../data/win_prob_calibration.json (from fit-win-prob-calibration.mjs).
 */

import { clamp, num } from "./ev-math.mjs";

/** @type {object | null} */
let ARTIFACT = null;

const CALIBRATION_URL = "../data/win_prob_calibration.json";

export function setWinProbCalibration(artifact) {
  ARTIFACT = artifact && typeof artifact === "object" ? artifact : null;
}

export function getWinProbCalibration() {
  return ARTIFACT;
}

export async function loadWinProbCalibration(url = CALIBRATION_URL) {
  try {
    const res = await fetch(`${url}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) {
      ARTIFACT = null;
      return null;
    }
    ARTIFACT = await res.json();
    return ARTIFACT;
  } catch {
    ARTIFACT = null;
    return null;
  }
}

/**
 * Piecewise-linear interpolate rate from sorted {p, rate} points.
 * Falls back to rawP when no curve is available.
 */
export function calibrateWinProb(market, rawP) {
  const p = num(rawP, NaN);
  if (!Number.isFinite(p)) return NaN;
  const clamped = clamp(p, 0.01, 0.99);
  const pts = ARTIFACT?.markets?.[market]?.points;
  if (!Array.isArray(pts) || pts.length < 2) return clamped;

  if (clamped <= pts[0].p) {
    return clamp(pts[0].rate, 0.02, 0.98);
  }
  const last = pts[pts.length - 1];
  if (clamped >= last.p) {
    return clamp(last.rate, 0.02, 0.98);
  }
  for (let i = 1; i < pts.length; i++) {
    const a = pts[i - 1];
    const b = pts[i];
    if (clamped <= b.p) {
      const span = Math.max(1e-9, b.p - a.p);
      const t = (clamped - a.p) / span;
      return clamp(a.rate + t * (b.rate - a.rate), 0.02, 0.98);
    }
  }
  return clamped;
}

/**
 * @returns {{
 *   pRawOver: number, pRawUnder: number,
 *   pCalOver: number, pCalUnder: number,
 *   fairOver: number, fairUnder: number,
 *   postedOver: number, postedUnder: number,
 *   confEdgeOver: number, confEdgeUnder: number,
 *   confEdgePostedOver: number, confEdgePostedUnder: number,
 * }}
 */
export function priceSidesAgainstBook({
  market,
  pRawOver,
  fairOver,
  fairUnder,
  postedOver,
  postedUnder,
}) {
  const rawO = num(pRawOver, NaN);
  const rawU = Number.isFinite(rawO) ? 1 - rawO : NaN;
  const calO = Number.isFinite(rawO) ? calibrateWinProb(market, rawO) : NaN;
  const calU = Number.isFinite(rawU) ? calibrateWinProb(market, rawU) : NaN;
  // Renormalize so calibrated sides sum to 1 (keeps two-way price coherent).
  let pCalOver = calO;
  let pCalUnder = calU;
  if (Number.isFinite(calO) && Number.isFinite(calU) && calO + calU > 0) {
    const s = calO + calU;
    pCalOver = calO / s;
    pCalUnder = calU / s;
  }
  const fO = num(fairOver, NaN);
  const fU = num(fairUnder, NaN);
  const qO = num(postedOver, NaN);
  const qU = num(postedUnder, NaN);
  return {
    pRawOver: rawO,
    pRawUnder: rawU,
    pCalOver,
    pCalUnder,
    fairOver: fO,
    fairUnder: fU,
    postedOver: qO,
    postedUnder: qU,
    confEdgeOver: Number.isFinite(pCalOver) && Number.isFinite(fO) ? (pCalOver - fO) * 100 : NaN,
    confEdgeUnder: Number.isFinite(pCalUnder) && Number.isFinite(fU) ? (pCalUnder - fU) * 100 : NaN,
    confEdgePostedOver: Number.isFinite(pCalOver) && Number.isFinite(qO) ? (pCalOver - qO) * 100 : NaN,
    confEdgePostedUnder: Number.isFinite(pCalUnder) && Number.isFinite(qU) ? (pCalUnder - qU) * 100 : NaN,
  };
}
