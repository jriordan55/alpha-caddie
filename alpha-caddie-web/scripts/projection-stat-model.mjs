/**
 * Statistical modeling helpers for round projections (no DK book calibration).
 * - Recency-weighted venue priors
 * - Venue vs tour outcome intercepts
 * - Poisson / binomial O/U pricing
 * - Outcome-calibrated σ scales from historical residuals
 */
import { createReadStream, existsSync } from "fs";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import { birdiesPlusEaglesFromRow, num } from "./round-projection-mu.mjs";

const MS_PER_YEAR = 365.25 * 86400000;
const VENUE_RECENCY_LAMBDA = 0.42;

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function rowTimeMs(row) {
  const s = String(row?.event_completed || row?.projections_updated_at || row?.exported_at || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const yr = Math.round(num(row?.year, NaN));
  return Number.isFinite(yr) ? Date.parse(`${yr}-06-01T12:00:00Z`) : NaN;
}

function recencyWeight(rowMs, cutoffMs, lambda = VENUE_RECENCY_LAMBDA) {
  if (!Number.isFinite(rowMs) || !Number.isFinite(cutoffMs) || rowMs >= cutoffMs) return 0;
  const years = (cutoffMs - rowMs) / MS_PER_YEAR;
  return Math.exp(-lambda * Math.max(0, years));
}

function emptyWeightedMoments() {
  return {
    w: 0,
    sumStp: 0,
    sumBirdMkt: 0,
    sumGir: 0,
    sumFw: 0,
    nBird: 0,
    nGir: 0,
    nFw: 0,
  };
}

function accumulateWeightedMoments(acc, row, weight, nFairwayHoles = 14) {
  if (!Number.isFinite(weight) || weight <= 0) return acc;
  const cp = num(row.course_par, NaN);
  const rs = num(row.round_score, NaN);
  if (!Number.isFinite(cp) || !Number.isFinite(rs) || rs < 55 || rs > 95) return acc;
  acc.w += weight;
  acc.sumStp += (rs - cp) * weight;
  const bird = birdiesPlusEaglesFromRow(row);
  if (Number.isFinite(bird)) {
    acc.sumBirdMkt += bird * weight;
    acc.nBird += weight;
  }
  const gir = num(row.gir, NaN);
  if (Number.isFinite(gir) && gir >= 0 && gir <= 18) {
    acc.sumGir += gir * weight;
    acc.nGir += weight;
  }
  const fw = num(row.driving_acc, NaN);
  if (Number.isFinite(fw) && fw >= 0 && fw <= nFairwayHoles) {
    acc.sumFw += fw * weight;
    acc.nFw += weight;
  }
  return acc;
}

function finalizeWeightedMoments(acc) {
  if (acc.w < 8) return null;
  return {
    w: acc.w,
    avgStp: acc.sumStp / acc.w,
    avgBirdMkt: acc.nBird > 0 ? acc.sumBirdMkt / acc.nBird : NaN,
    avgGir: acc.nGir > 0 ? acc.sumGir / acc.nGir : NaN,
    avgFw: acc.nFw > 0 ? acc.sumFw / acc.nFw : NaN,
  };
}

/** Tour-wide priors from historical rounds before cutoff (recency-weighted). */
export function computeTourPriorsFromHist(histRows, cutoffMs) {
  const acc = emptyWeightedMoments();
  for (const row of histRows) {
    const t = rowTimeMs(row);
    const w = recencyWeight(t, cutoffMs);
    if (w <= 0) continue;
    accumulateWeightedMoments(acc, row, w);
  }
  const fin = finalizeWeightedMoments(acc);
  if (!fin) {
    return { avgStp: 0.5, avgBirdMkt: 4.2, avgGir: 12, avgFw: 9, nEff: 0 };
  }
  return {
    avgStp: fin.avgStp,
    avgBirdMkt: fin.avgBirdMkt,
    avgGir: fin.avgGir,
    avgFw: fin.avgFw,
    nEff: fin.w,
  };
}

/**
 * Venue intercepts vs tour (recency-weighted, James-Stein shrinkage).
 * @returns {{ scoreStp: number, birdMkt: number, gir: number, fw: number, nEff: number } | null}
 */
export function computeVenueStatisticalIntercept(histRows, courseKey, cutoffMs, tourPriors) {
  if (!courseKey) return null;
  const acc = emptyWeightedMoments();
  for (const row of histRows) {
    const ck = normCourseNameKey(row.course_name || "");
    if (!ck || ck !== courseKey) continue;
    const t = rowTimeMs(row);
    const w = recencyWeight(t, cutoffMs);
    if (w <= 0) continue;
    accumulateWeightedMoments(acc, row, w);
  }
  const fin = finalizeWeightedMoments(acc);
  if (!fin) return null;
  const shrink = fin.w / (fin.w + 45);
  return {
    scoreStp: shrink * (fin.avgStp - tourPriors.avgStp),
    birdMkt: shrink * (fin.avgBirdMkt - tourPriors.avgBirdMkt),
    gir: shrink * (fin.avgGir - tourPriors.avgGir),
    fw: shrink * (fin.avgFw - tourPriors.avgFw),
    nEff: fin.w,
  };
}

/** Apply venue score intercept (harder/easier vs tour) — counts use recency-weighted venue anchors. */
export function applyVenueScoreIntercept(row, intercept, coursePar18 = 72) {
  if (!row || !intercept) return;
  const scoreShift = num(intercept.scoreStp, NaN);
  if (!Number.isFinite(scoreShift) || Math.abs(scoreShift) < 0.02) return;
  const par = Math.round(num(coursePar18, NaN)) || 72;
  const stp = num(row.score_to_par, NaN);
  const ts = num(row.total_score, NaN);
  if (Number.isFinite(stp)) {
    row.score_to_par = Math.round((stp + scoreShift) * 100) / 100;
    row.total_score = Math.round((par + row.score_to_par) * 100) / 100;
  } else if (Number.isFinite(ts)) {
    row.total_score = Math.round((ts + scoreShift) * 100) / 100;
    row.score_to_par = Math.round((row.total_score - par) * 100) / 100;
  }
  if (Number.isFinite(num(row.mu_sg, NaN))) {
    row.mu_sg = Math.round((num(row.mu_sg, 0) - scoreShift) * 1000) / 1000;
  }
}

export function computeRecencyWeightedVenueMoments(histRows, courseKey, cutoffMs) {
  if (!courseKey) return null;
  const acc = emptyWeightedMoments();
  for (const row of histRows) {
    if (normCourseNameKey(row.course_name || "") !== courseKey) continue;
    const w = recencyWeight(rowTimeMs(row), cutoffMs);
    if (w <= 0) continue;
    accumulateWeightedMoments(acc, row, w);
  }
  return finalizeWeightedMoments(acc);
}

/** P(X > line) for Poisson(λ=μ) vs half-line (birdies market). */
export function poissonProbOver(mu, line) {
  const lam = num(mu, NaN);
  const L = num(line, NaN);
  if (!Number.isFinite(lam) || lam <= 0 || !Number.isFinite(L)) return NaN;
  const k = Math.floor(L + 1e-9);
  let cdf = 0;
  let term = Math.exp(-lam);
  cdf += term;
  for (let i = 1; i <= k; i++) {
    term *= lam / i;
    cdf += term;
    if (cdf > 0.999999) break;
  }
  return clamp(1 - cdf, 0, 1);
}

/** P(X > line) for Binomial(n, p=μ/n) vs half-line (GIR / fairways). */
export function binomialProbOver(mu, nTrials, line) {
  const n = Math.round(num(nTrials, NaN));
  const L = num(line, NaN);
  const m = num(mu, NaN);
  if (!Number.isFinite(n) || n < 1 || !Number.isFinite(m) || !Number.isFinite(L)) return NaN;
  const p = clamp(m / n, 0.02, 0.98);
  const k = Math.floor(L + 1e-9);
  let cdf = 0;
  for (let i = 0; i <= k; i++) {
    cdf += binomialPmf(n, p, i);
  }
  return clamp(1 - cdf, 0, 1);
}

function binomialPmf(n, p, k) {
  if (k < 0 || k > n) return 0;
  if (k === 0) return (1 - p) ** n;
  if (k === n) return p ** n;
  let logCoef = 0;
  for (let i = 0; i < k; i++) logCoef += Math.log(n - i) - Math.log(i + 1);
  return Math.exp(logCoef + k * Math.log(p) + (n - k) * Math.log(1 - p));
}

/** Normal fallback for total score. */
export function normalProbOver(mu, line, sigma) {
  const sig = num(sigma, NaN);
  if (!Number.isFinite(sig) || sig <= 0) return NaN;
  const z = (line - mu) / sig;
  const t = 1 / (1 + 0.2316419 * Math.abs(z));
  const d = 0.3989423 * Math.exp((-z * z) / 2);
  const p =
    d * t * (0.3193815 + t * (-0.3565638 + t * (1.7814779 + t * (-1.821256 + t * 1.3302744))));
  const cdf = z >= 0 ? 1 - p : p;
  return 1 - cdf;
}

const MARKET_COLS = {
  "Total score": { model: "round_score_line", actual: "actual_round_score" },
  Birdies: { model: "birdies_line", actual: "actual_birdies" },
  GIR: { model: "gir_line", actual: "actual_gir" },
  "Fairways hit": { model: "fairways_line", actual: "actual_fairways" },
};

/** Empirical / formula RMSE ratio → σ inflation (walk-forward on detail CSV). */
export async function fitOutcomeSigmaScales(csvPath, { minPairs = 120 } = {}) {
  const scales = {
    "Total score": 1.08,
    Birdies: 1.15,
    GIR: 1.02,
    "Fairways hit": 1.2,
  };
  if (!csvPath || !existsSync(csvPath)) return scales;
  /** @type {Record<string, { n: number, sq: number, formulaSq: number }>} */
  const acc = {};
  for (const m of Object.keys(MARKET_COLS)) {
    acc[m] = { n: 0, sq: 0, formulaSq: 0 };
  }
  await new Promise((resolve, reject) => {
    createReadStream(csvPath)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        for (const [market, cols] of Object.entries(MARKET_COLS)) {
          const model = num(row[cols.model], NaN);
          const actual = num(row[cols.actual], NaN);
          if (!Number.isFinite(model) || !Number.isFinite(actual)) continue;
          const a = acc[market];
          a.n++;
          const err = actual - model;
          a.sq += err * err;
          const formulaSig =
            market === "Total score"
              ? 2.75
              : market === "Birdies"
                ? Math.sqrt(Math.max(model, 0.2) * 1.08)
                : market === "GIR"
                  ? Math.sqrt(18 * clamp(model / 18, 0.07, 0.93) * (1 - clamp(model / 18, 0.07, 0.93)))
                  : Math.sqrt(14 * clamp(model / 14, 0.07, 0.93) * (1 - clamp(model / 14, 0.07, 0.93)));
          a.formulaSq += formulaSig * formulaSig;
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });
  for (const [market, a] of Object.entries(acc)) {
    if (a.n < minPairs || a.formulaSq <= 0) continue;
    const empRmse = Math.sqrt(a.sq / a.n);
    const formRmse = Math.sqrt(a.formulaSq / a.n);
    if (formRmse > 0.05) {
      scales[market] = Math.round(clamp(empRmse / formRmse, 0.85, 1.45) * 1000) / 1000;
    }
  }
  return scales;
}

let _sigmaScaleCache = null;

export function setOutcomeSigmaScales(scales) {
  _sigmaScaleCache = scales && typeof scales === "object" ? { ...scales } : null;
}

export function outcomeSigmaScale(market) {
  const s = _sigmaScaleCache?.[market];
  return Number.isFinite(s) && s > 0 ? s : 1;
}

/** Empirical model−actual μ correction from walk-forward residuals (not DK calibration). */
export async function fitOutcomeMuBiasCorrections(csvPath, { minPairs = 120 } = {}) {
  const corrections = {
    "Total score": -0.85,
    Birdies: -0.5,
    GIR: 0.55,
    "Fairways hit": 0.2,
  };
  if (!csvPath || !existsSync(csvPath)) return corrections;
  /** @type {Record<string, { n: number, sum: number }>} */
  const acc = {};
  for (const m of Object.keys(MARKET_COLS)) acc[m] = { n: 0, sum: 0 };
  await new Promise((resolve, reject) => {
    createReadStream(csvPath)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        for (const [market, cols] of Object.entries(MARKET_COLS)) {
          const model = num(row[cols.model], NaN);
          const actual = num(row[cols.actual], NaN);
          if (!Number.isFinite(model) || !Number.isFinite(actual)) continue;
          acc[market].n++;
          acc[market].sum += actual - model;
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });
  for (const [market, a] of Object.entries(acc)) {
    if (a.n < minPairs) continue;
    const mean = a.sum / a.n;
    const cap =
      market === "Total score" ? 2.5 : market === "Birdies" ? 0.85 : market === "GIR" ? 1.1 : 0.75;
    corrections[market] = Math.round(clamp(mean, -cap, cap) * 1000) / 1000;
  }
  return corrections;
}

let _muBiasCache = null;

export function setOutcomeMuBiasCorrections(corrections) {
  _muBiasCache = corrections && typeof corrections === "object" ? { ...corrections } : null;
}

/** Add to model μ so E[actual] aligns with walk-forward residuals. */
export function outcomeMuBiasCorrection(market) {
  const c = _muBiasCache?.[market];
  return Number.isFinite(c) ? c : 0;
}

export function applyOutcomeMuBiasCorrection(market, mu) {
  const m = num(mu, NaN);
  if (!Number.isFinite(m)) return m;
  return m + outcomeMuBiasCorrection(market);
}

/** Apply venue counting intercept (birdies/GIR/FW vs tour). */
export function applyVenueCountingIntercept(row, intercept, fairwayHoles = 14) {
  if (!row || !intercept) return;
  const bird = num(intercept.birdMkt, NaN);
  if (Number.isFinite(bird) && Math.abs(bird) >= 0.04) {
    const b = num(row.birdies, NaN);
    if (Number.isFinite(b)) row.birdies = Math.round((b + bird) * 100) / 100;
  }
  const gir = num(intercept.gir, NaN);
  if (Number.isFinite(gir) && Math.abs(gir) >= 0.08) {
    const g = num(row.gir, NaN);
    if (Number.isFinite(g)) row.gir = Math.round(clamp(g + gir, 0, 18) * 100) / 100;
  }
  const fw = num(intercept.fw, NaN);
  if (Number.isFinite(fw) && Math.abs(fw) >= 0.08) {
    const f = num(row.fairways, NaN);
    if (Number.isFinite(f)) {
      const n = Math.round(num(fairwayHoles, 14)) || 14;
      row.fairways = Math.round(clamp(f + fw, 0, n) * 100) / 100;
    }
  }
}

/** Venue-scaled birdie SG boost — dampen away from tour birdie rate. */
export function venueBirdieSgScale(venueBirdMkt, tourBirdMkt = 4.2) {
  const v = num(venueBirdMkt, NaN);
  const t = num(tourBirdMkt, 4.2);
  if (!Number.isFinite(v) || v <= 0 || !Number.isFinite(t) || t <= 0) return 1;
  const ratio = v / t;
  if (ratio >= 0.98 && ratio <= 1.04) return 1;
  if (ratio < 0.96) return clamp(0.78 + 0.22 * ratio, 0.8, 1);
  return clamp(1.04 - 0.12 * (ratio - 1.04), 0.88, 1);
}
