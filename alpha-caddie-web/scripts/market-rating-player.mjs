/**
 * Market-rating helpers (mirrors alpha-caddie-web/app.js).
 * UI column uses field-relative z → 1–100; tour benchmarks remain for internal μ nudges.
 */

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

const BENCHMARK_FALLBACK = {
  GIR: { mean: 0.597, sd: 0.228, higherBetter: true, unit: "rate" },
  "Fairways hit": { mean: 0.541, sd: 0.228, higherBetter: true, unit: "rate" },
  "Total score": { mean: 70.41, sd: 3.31, higherBetter: false, unit: "strokes" },
  Birdies: { mean: 3.65, sd: 1.9, higherBetter: true, unit: "count" },
  Pars: { mean: 10.81, sd: 2.89, higherBetter: true, unit: "count" },
  Bogeys: { mean: 2.5, sd: 1.69, higherBetter: false, unit: "count" },
};

function marketOpportunities(mKey) {
  if (mKey === "GIR") return 18;
  if (mKey === "Fairways hit") return 14;
  return null;
}

export function marketHigherBetter(mKey) {
  return mKey !== "Total score" && mKey !== "Bogeys";
}

/** Model / skill fallback when historical rounds lack counting stats (GIR often null in DG CSV). */
export function playerModelAvgForMarket(market, player, fairwayOpp = 14) {
  if (!player || typeof player !== "object") return NaN;
  const mKey = String(market || "GIR").trim();
  if (mKey === "GIR") {
    const rate = num(player.dg_gir_pct, NaN);
    if (Number.isFinite(rate) && rate > 0 && rate <= 1.05) return rate * 18;
    return num(player.gir, NaN);
  }
  if (mKey === "Fairways hit") {
    const rate = num(player.dg_fairway_pct, NaN);
    const opp = fairwayOpp;
    if (Number.isFinite(rate) && rate > 0 && rate <= 1.05 && Number.isFinite(opp)) return rate * opp;
    return num(player.fairways, NaN);
  }
  if (mKey === "Total score") return num(player.total_score, NaN);
  if (mKey === "Birdies") return num(player.birdies, NaN) + num(player.eagles, 0);
  if (mKey === "Pars") return num(player.pars, NaN);
  if (mKey === "Bogeys") return num(player.bogeys ?? player.bogies, NaN);
  return NaN;
}

export function marketRatingZ(market, playerAvg, benchmarks = BENCHMARK_FALLBACK) {
  const mKey = String(market || "GIR").trim();
  const mu = num(playerAvg, NaN);
  if (!Number.isFinite(mu)) return NaN;
  const b = benchmarks[mKey] || BENCHMARK_FALLBACK[mKey];
  if (!b) return NaN;
  let x = mu;
  let tourMu = b.mean;
  let tourSd = b.sd;
  if (b.unit === "rate") {
    const opp = marketOpportunities(mKey);
    if (!Number.isFinite(opp) || opp <= 0) return NaN;
    x = mu / opp;
  }
  if (!Number.isFinite(tourSd) || tourSd <= 1e-6) return NaN;
  let z = (x - tourMu) / tourSd;
  if (!b.higherBetter) z = -z;
  return z;
}

export function buildFieldMarketStats(players, markets) {
  const out = new Map();
  const list = Array.isArray(players) ? players : [];
  for (const market of markets) {
    const mKey = String(market || "Total score").trim();
    const vals = [];
    for (const p of list) {
      const v = playerModelAvgForMarket(mKey, p);
      if (Number.isFinite(v)) vals.push(v);
    }
    if (vals.length < 2) continue;
    const mean = vals.reduce((a, b) => a + b, 0) / vals.length;
    const variance = vals.reduce((a, v) => a + (v - mean) ** 2, 0) / (vals.length - 1);
    const sd = Math.sqrt(Math.max(variance, 0));
    out.set(mKey, { mean, sd: Math.max(sd, 1e-6), n: vals.length });
  }
  return out;
}

export function fieldMarketRatingZ(market, playerAvg, fieldStats) {
  const mKey = String(market || "Total score").trim();
  const mu = num(playerAvg, NaN);
  const fs = fieldStats?.get?.(mKey);
  if (!fs || !Number.isFinite(mu)) return NaN;
  const fieldMean = num(fs.mean, NaN);
  const fieldSd = num(fs.sd, NaN);
  if (!Number.isFinite(fieldMean) || !Number.isFinite(fieldSd) || fieldSd <= 1e-6) return NaN;
  let z = (mu - fieldMean) / fieldSd;
  if (!marketHigherBetter(mKey)) z = -z;
  return z;
}

export function zToRating100(z) {
  if (!Number.isFinite(z)) return NaN;
  const t = 1 / (1 + 0.3275911 * Math.abs(z));
  const erf =
    1 -
    t *
      (0.254829592 +
        t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429)))) *
      Math.exp(-z * z);
  const sign = z < 0 ? -1 : 1;
  const p = 0.5 * (1 + sign * erf);
  return Math.round(clamp(p * 100, 1, 100));
}

export function fieldMarketRating100ForPlayer(market, player, fieldStats) {
  const mKey = String(market || "GIR").trim();
  const avg = playerModelAvgForMarket(mKey, player);
  return zToRating100(fieldMarketRatingZ(mKey, avg, fieldStats));
}

/** Tour-relative (internal μ nudges). */
export function marketRating100ForPlayer(market, player, benchmarks, fairwayOpp = 14) {
  const mKey = String(market || "GIR").trim();
  const avg = playerModelAvgForMarket(mKey, player, fairwayOpp);
  return zToRating100(marketRatingZ(mKey, avg, benchmarks));
}
