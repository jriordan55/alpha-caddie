/**
 * Continuous prior-round signal → μ boost (event-LOO ridge on centered signals).
 * Count markets: μ × (1 + β·z); total score: μ + β·z.
 * z = Σ wᵢ (signalᵢ − train_meanᵢ).
 */
import { priorSignalsFromRow } from "./sg-side-policy.mjs";

export const SHRINK_K = 50;
export const RIDGE_LAMBDA = 0.02;
export const MIN_FIT_SAMPLES = 15;

/** @type {Record<string, { features: string[], weights?: number[], relative: boolean, label: string }>} */
export const MARKET_BOOST_SPEC = Object.freeze({
  Birdies: {
    features: ["prev_bob_pct"],
    relative: true,
    label: "prev BoB%",
  },
  GIR: {
    features: ["prev_gir_pct"],
    relative: true,
    label: "prev GIR%",
  },
  "Fairways hit": {
    features: ["prev_sg_ott"],
    relative: true,
    label: "prev SG OTT",
  },
  Bogeys: {
    features: ["prev_sg_app"],
    relative: true,
    label: "prev SG APP",
  },
  "Total score": {
    features: ["prev_sg_app"],
    relative: false,
    label: "prev SG APP",
  },
  Pars: {
    features: ["prev_sg_app", "prev_sg_putt"],
    weights: [1, -1],
    relative: true,
    label: "prev SG APP − PUTT",
  },
});

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function shrinkWeight(n, k = SHRINK_K) {
  const c = Math.max(0, Number(n) || 0);
  return c / (c + k);
}

export function marketBoostMarkets() {
  return Object.keys(MARKET_BOOST_SPEC);
}

/** Weighted sum of centered prior signals. */
export function centeredPriorSignal(signals, spec, means = {}) {
  if (!spec?.features?.length) return NaN;
  const weights = spec.weights || spec.features.map(() => 1);
  let z = 0;
  for (let i = 0; i < spec.features.length; i++) {
    const field = spec.features[i];
    const v = num(signals?.[field], NaN);
    if (!Number.isFinite(v)) return NaN;
    const m = num(means[field], 0);
    z += (weights[i] ?? 1) * (v - m);
  }
  return z;
}

export function marketFitForEvent(fit, market, event = null) {
  if (event && fit?.loo?.[event]?.markets?.[market]) {
    return fit.loo[event].markets[market];
  }
  return fit?.markets?.[market] || null;
}

/**
 * Ridge slope on centered signal z vs residual (counts: actual/model − 1).
 * @param {object[]} rows each { signals, model, actual }
 */
export function fitContinuousMarket(market, rows, ridgeLambda = RIDGE_LAMBDA, shrinkK = SHRINK_K) {
  const spec = MARKET_BOOST_SPEC[market];
  if (!spec) return null;

  /** @type {Record<string, number>} */
  const means = {};
  for (const f of spec.features) {
    let s = 0;
    let c = 0;
    for (const r of rows) {
      const v = num(r.signals?.[f], NaN);
      if (!Number.isFinite(v)) continue;
      s += v;
      c++;
    }
    means[f] = c ? s / c : 0;
  }

  let szy = 0;
  let szz = 0;
  let n = 0;
  for (const r of rows) {
    const z = centeredPriorSignal(r.signals, spec, means);
    if (!Number.isFinite(z)) continue;
    const y = spec.relative
      ? r.model > 0
        ? r.actual / r.model - 1
        : NaN
      : r.actual - r.model;
    if (!Number.isFinite(y)) continue;
    szy += z * y;
    szz += z * z;
    n++;
  }

  if (n < MIN_FIT_SAMPLES || szz <= 0) {
    return {
      market,
      beta: 0,
      beta_raw: 0,
      means,
      n,
      relative: spec.relative,
      features: spec.features,
      weights: spec.weights || null,
      label: spec.label,
      shrunk: true,
    };
  }

  const betaRaw = szy / (szz + ridgeLambda);
  const w = shrinkWeight(n, shrinkK);
  return {
    market,
    beta: betaRaw * w,
    beta_raw: betaRaw,
    means,
    n,
    relative: spec.relative,
    features: spec.features,
    weights: spec.weights || null,
    label: spec.label,
  };
}

export function priorRoundMuBoostDelta(market, baseMu, signals, fit, event = null) {
  if (!fit?.enabled || !Number.isFinite(baseMu)) return 0;
  const spec = MARKET_BOOST_SPEC[market];
  const marketFit = marketFitForEvent(fit, market, event);
  if (!spec || !marketFit || !Number.isFinite(marketFit.beta)) return 0;

  const z = centeredPriorSignal(signals, spec, marketFit.means || {});
  if (!Number.isFinite(z) || Math.abs(z) < 1e-12) return 0;

  const beta = marketFit.beta;
  if (marketFit.relative) return baseMu * beta * z;
  return beta * z;
}

export function applyPriorRoundMuBoost(market, baseMu, signals, fit, event = null) {
  if (!Number.isFinite(baseMu)) return baseMu;
  const delta = priorRoundMuBoostDelta(market, baseMu, signals, fit, event);
  const out = baseMu + delta;
  if (market === "Birdies" || market === "Bogeys" || market === "GIR" || market === "Fairways hit") {
    return Math.max(0.05, out);
  }
  return out;
}

export function priorRoundMuBoostFromRow(market, baseMu, row, fit, event = null) {
  return applyPriorRoundMuBoost(market, baseMu, priorSignalsFromRow(row), fit, event);
}
