/**
 * Outcome μ debias + book residual blend for unbiased, both-side edges.
 *
 * μ_deb = μ − bias
 * μ*   = book + α (μ_deb − book)   when book present
 *
 * Fit: npm run fit:outcome-mu-debias
 * Eval: npm run compare:unbiased-edge-oos
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { MARKET_BOOK_CALIBRATION_MARKETS } from "./market-book-calibration.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
export const OUTCOME_MU_DEBIAS_PATH = join(WEB, "data", "outcome_mu_debias.json");

/** Valid book-line ranges (reject empty→0 pollution). */
export const BOOK_LINE_RANGE = {
  "Total score": [55, 95],
  Birdies: [0.5, 12],
  GIR: [4, 18],
  "Fairways hit": [2, 15],
};

/** @type {null | { markets: Record<string, object> }} */
let _cache = null;

export function outcomeMuDebiasPath() {
  return OUTCOME_MU_DEBIAS_PATH;
}

export function outcomeMuDebiasEnabled() {
  // Default OFF — sportsbook residual blend is not part of the skill/course recipe.
  const v = String(process.env.GOLF_OUTCOME_MU_DEBIAS || "0").trim().toLowerCase();
  return v === "1" || v === "true" || v === "yes" || v === "on";
}

export function loadOutcomeMuDebias(force = false) {
  if (_cache && !force) return _cache;
  if (!existsSync(OUTCOME_MU_DEBIAS_PATH)) {
    _cache = { markets: {} };
    return _cache;
  }
  try {
    _cache = JSON.parse(readFileSync(OUTCOME_MU_DEBIAS_PATH, "utf8"));
  } catch {
    _cache = { markets: {} };
  }
  return _cache;
}

export function bookLineValid(market, book) {
  if (!Number.isFinite(book)) return false;
  const r = BOOK_LINE_RANGE[market];
  if (!r) return book !== 0;
  return book >= r[0] && book <= r[1];
}

/**
 * @param {string} market
 * @param {number} mu
 * @param {number} [book]
 * @param {{ bias?: number, alpha?: number } | null} [override]
 */
export function applyOutcomeMuDebias(market, mu, book = NaN, override = null) {
  if (!Number.isFinite(mu)) return NaN;
  if (!outcomeMuDebiasEnabled() && !override) return mu;
  const cfg = override || loadOutcomeMuDebias()?.markets?.[market] || {};
  const bias = Number.isFinite(cfg.bias) ? cfg.bias : 0;
  const alpha = Number.isFinite(cfg.alpha) ? cfg.alpha : 1;
  const deb = mu - bias;
  if (bookLineValid(market, book) && Number.isFinite(alpha) && alpha < 0.999) {
    return book + alpha * (deb - book);
  }
  return deb;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

export function fitAlpha(pairs) {
  let sxx = 0;
  let sxy = 0;
  for (const p of pairs) {
    if (!Number.isFinite(p.model) || !Number.isFinite(p.book) || !Number.isFinite(p.actual)) continue;
    const x = p.model - p.book;
    const y = p.actual - p.book;
    sxx += x * x;
    sxy += x * y;
  }
  if (sxx < 1e-8) return 0;
  return clamp(sxy / sxx, 0, 1.25);
}

export function meanBias(pairs) {
  let n = 0;
  let s = 0;
  for (const p of pairs) {
    if (!Number.isFinite(p.model) || !Number.isFinite(p.actual)) continue;
    n++;
    s += p.model - p.actual;
  }
  return n ? s / n : 0;
}

/**
 * Event leave-one-out fit of bias + α; also searches min gap where both O/U flat ROI > 0.
 * @param {Array<{ event: string, model: number, book: number, actual: number, overOdds?: number, underOdds?: number, overRes?: string, underRes?: string }>} rows
 */
export function fitMarketDebiasLoo(rows) {
  const events = [...new Set(rows.map((r) => r.event).filter(Boolean))];
  if (!events.length) {
    return { bias: 0, alpha: 1, min_gap_both_sides: 0.5, n: 0, both_side_gaps: [] };
  }
  const biases = [];
  const alphas = [];
  /** @type {Array<{ event: string, pred: number, book: number, actual: number, oo: number, uo: number, oRes: string, uRes: string }>} */
  const scored = [];
  for (const ev of events) {
    const train = rows.filter((r) => r.event !== ev);
    const bias = meanBias(train);
    const alpha = fitAlpha(
      train.map((r) => ({
        model: r.model - bias,
        book: r.book,
        actual: r.actual,
      })),
    );
    biases.push(bias);
    alphas.push(alpha);
    for (const r of rows.filter((x) => x.event === ev)) {
      const deb = r.model - bias;
      const pred = bookLineValid(r.market || "", r.book)
        ? r.book + alpha * (deb - r.book)
        : deb;
      scored.push({
        event: ev,
        pred,
        book: r.book,
        actual: r.actual,
        oo: r.overOdds,
        uo: r.underOdds,
        oRes: r.overRes,
        uRes: r.underRes,
      });
    }
  }
  const bias = biases.reduce((a, b) => a + b, 0) / biases.length;
  const alpha = alphas.reduce((a, b) => a + b, 0) / alphas.length;
  const bothGaps = [];
  for (const g of [0.25, 0.35, 0.5, 0.75, 1, 1.25, 1.5]) {
    const s = gradeBothSides(scored, g);
    if (s.over?.roi_pct > 0 && s.under?.roi_pct > 0 && s.over.n >= 8 && s.under.n >= 8) {
      bothGaps.push({ gap: g, ...s });
    }
  }
  const pick = bothGaps[0] || null;
  return {
    bias: Math.round(bias * 1000) / 1000,
    alpha: Math.round(alpha * 1000) / 1000,
    min_gap_both_sides: pick?.gap ?? 0.75,
    n: rows.length,
    n_events: events.length,
    loo_mae: maeOf(scored.map((r) => ({ pred: r.pred, actual: r.actual }))),
    loo_bias: meanErr(scored.map((r) => ({ pred: r.pred, actual: r.actual }))),
    both_side_gaps: bothGaps,
    recommended_gap: pick?.gap ?? null,
  };
}

function amPnl(res, am) {
  if (res !== "W" && res !== "L") return NaN;
  const o = Number(am);
  if (!Number.isFinite(o) || o === 0) return NaN;
  if (res === "W") return o > 0 ? o / 100 : 100 / Math.abs(o);
  return -1;
}

function gradeBothSides(scored, minGap) {
  const o = { n: 0, u: 0, h: 0 };
  const u = { n: 0, u: 0, h: 0 };
  for (const r of scored) {
    if (!Number.isFinite(r.pred) || !Number.isFinite(r.book)) continue;
    const gap = r.pred - r.book;
    if (Math.abs(gap) < minGap) continue;
    if (gap > 0) {
      const pnl = amPnl(r.oRes, r.oo);
      if (!Number.isFinite(pnl)) continue;
      o.n++;
      o.u += pnl;
      if (r.oRes === "W") o.h++;
    } else {
      const pnl = amPnl(r.uRes, r.uo);
      if (!Number.isFinite(pnl)) continue;
      u.n++;
      u.u += pnl;
      if (r.uRes === "W") u.h++;
    }
  }
  const fmt = (s) =>
    s.n
      ? {
          n: s.n,
          hit_pct: Math.round((1000 * s.h) / s.n) / 10,
          roi_pct: Math.round((1000 * s.u) / s.n) / 10,
        }
      : null;
  return { over: fmt(o), under: fmt(u) };
}

function maeOf(pairs) {
  let n = 0;
  let a = 0;
  for (const p of pairs) {
    if (!Number.isFinite(p.pred) || !Number.isFinite(p.actual)) continue;
    n++;
    a += Math.abs(p.pred - p.actual);
  }
  return n ? Math.round((a / n) * 1000) / 1000 : null;
}

function meanErr(pairs) {
  let n = 0;
  let a = 0;
  for (const p of pairs) {
    if (!Number.isFinite(p.pred) || !Number.isFinite(p.actual)) continue;
    n++;
    a += p.pred - p.actual;
  }
  return n ? Math.round((a / n) * 1000) / 1000 : null;
}

export function writeOutcomeMuDebias(payload) {
  writeFileSync(OUTCOME_MU_DEBIAS_PATH, `${JSON.stringify(payload, null, 2)}\n`);
  _cache = payload;
  return OUTCOME_MU_DEBIAS_PATH;
}

export { MARKET_BOOK_CALIBRATION_MARKETS as DEBIAS_MARKETS };
