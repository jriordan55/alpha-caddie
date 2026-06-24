/** Browser-safe O/U edge math (mirrors round-projection-mu.mjs). */

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function normalCdf(z) {
  const t = 1 / (1 + 0.2316419 * Math.abs(z));
  const d = 0.3989423 * Math.exp((-z * z) / 2);
  const p =
    d *
    t *
    (0.3193815 + t * (-0.3565638 + t * (1.7814779 + t * (-1.821256 + t * 1.3302744))));
  return z >= 0 ? 1 - p : p;
}

export function impliedProbFromAmerican(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return NaN;
  if (v < 0) return (-v) / (-v + 100);
  return 100 / (v + 100);
}

/** Multiplicative devig for two-way O/U — fair probs sum to 1 (no book margin). */
export function devigFairTwoWay(overOdds, underOdds) {
  const qOver = impliedProbFromAmerican(overOdds);
  const qUnder = impliedProbFromAmerican(underOdds);
  if (!Number.isFinite(qOver) || !Number.isFinite(qUnder)) {
    return { fairOver: NaN, fairUnder: NaN, overround: NaN };
  }
  const sum = qOver + qUnder;
  if (sum <= 0) return { fairOver: NaN, fairUnder: NaN, overround: NaN };
  return {
    fairOver: qOver / sum,
    fairUnder: qUnder / sum,
    overround: sum - 1,
  };
}

export function modelEdgeVsFairAtLine(market, mu, line, overOdds, underOdds, sigmaScale = 1) {
  const pOver = modelProbOver(market, mu, line, sigmaScale);
  if (!Number.isFinite(pOver)) {
    return { edgeFairOver: NaN, edgeFairUnder: NaN, fairOver: NaN, fairUnder: NaN, pOver, pUnder: NaN };
  }
  const pUnder = 1 - pOver;
  const { fairOver, fairUnder } = devigFairTwoWay(overOdds, underOdds);
  const edgeFairOver = Number.isFinite(fairOver) ? (pOver - fairOver) * 100 : NaN;
  const edgeFairUnder = Number.isFinite(fairUnder) ? (pUnder - fairUnder) * 100 : NaN;
  return { edgeFairOver, edgeFairUnder, fairOver, fairUnder, pOver, pUnder };
}

export function pickBetSideFair(edgeFairOver, edgeFairUnder, minEvPct) {
  const th = num(minEvPct, 0);
  if (!Number.isFinite(edgeFairOver) || !Number.isFinite(edgeFairUnder)) return null;
  if (edgeFairOver >= th && edgeFairOver >= edgeFairUnder) return { side: "over", edge: edgeFairOver };
  if (edgeFairUnder >= th && edgeFairUnder > edgeFairOver) return { side: "under", edge: edgeFairUnder };
  return null;
}

export function americanToDecimal(am) {
  const v = num(am, NaN);
  if (!Number.isFinite(v) || v === 0) return NaN;
  return v > 0 ? 1 + v / 100 : 1 + 100 / Math.abs(v);
}

export function formatAmerican(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return "";
  return v > 0 ? `+${v}` : String(v);
}

function sigmaDefault(market, muAbs) {
  const m = num(muAbs, NaN);
  if (!Number.isFinite(m) || m <= 0) return 2.4;
  if (market === "GIR") {
    const p = clamp(m / 18, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(18 * p * (1 - p)));
  }
  if (market === "Fairways hit") {
    const p = clamp(m / 14, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(14 * p * (1 - p)));
  }
  if (market === "Birdies" || market === "Bogeys") return clamp(Math.sqrt(m * 1.08), 1.05, 3.15);
  if (market === "Pars") return clamp(Math.sqrt(m * 1.06), 1.15, 3.35);
  return Math.max(0.55, Math.sqrt(Math.max(m, 0.2)) * 0.9);
}

export function modelProbOver(market, mu, line, sigmaScale = 1) {
  if (!Number.isFinite(mu) || !Number.isFinite(line)) return NaN;
  const scale = Number.isFinite(sigmaScale) && sigmaScale > 0 ? sigmaScale : 1;
  const sig = (market === "Total score" ? 2.75 : sigmaDefault(market, mu)) * scale;
  const z = (line - mu) / sig;
  return 1 - normalCdf(z);
}

export function modelEdgePctAtLine(market, mu, line, overOdds, underOdds, sigmaScale = 1) {
  const pOver = modelProbOver(market, mu, line, sigmaScale);
  if (!Number.isFinite(pOver)) return { edgeOver: NaN, edgeUnder: NaN, best: NaN };
  const pUnder = 1 - pOver;
  const pImpOver = Number.isFinite(num(overOdds, NaN)) ? impliedProbFromAmerican(overOdds) : 100 / 210;
  const pImpUnder = Number.isFinite(num(underOdds, NaN)) ? impliedProbFromAmerican(underOdds) : 100 / 210;
  const edgeOver = (pOver - pImpOver) * 100;
  const edgeUnder = (pUnder - pImpUnder) * 100;
  return { edgeOver, edgeUnder, best: Math.max(edgeOver, edgeUnder) };
}

export function pickBetSide(edgeOver, edgeUnder, minEvPct) {
  const th = num(minEvPct, 0);
  if (!Number.isFinite(edgeOver) || !Number.isFinite(edgeUnder)) return null;
  if (edgeOver >= th && edgeOver >= edgeUnder) return { side: "over", edge: edgeOver };
  if (edgeUnder >= th && edgeUnder > edgeOver) return { side: "under", edge: edgeUnder };
  return null;
}

export function pnlForResult(result, americanOdds) {
  if (result === "W") {
    const dec = americanToDecimal(americanOdds);
    return Number.isFinite(dec) ? dec - 1 : 0;
  }
  if (result === "L") return -1;
  return 0;
}

/** Full Kelly fraction f* = (p·dec − 1) / (dec − 1). */
export function kellyFractionRaw(modelP, dec) {
  if (!Number.isFinite(modelP) || modelP <= 0 || !Number.isFinite(dec) || dec <= 1) return 0;
  const edge = modelP * dec - 1;
  if (edge <= 0) return 0;
  const den = dec - 1;
  if (den <= 0) return 0;
  return edge / den;
}

/**
 * Stake in dollars for one bet.
 * @param {object} opts — bankroll0, unitPct (default 1), maxStakePct (default 5), kellyMult (default 0.25)
 */
export function computeStakeDollars(bankroll, modelP, dec, method, opts = {}) {
  if (!Number.isFinite(bankroll) || bankroll <= 0) return 0;
  const B0 = Number.isFinite(opts.bankroll0) && opts.bankroll0 > 0 ? opts.bankroll0 : bankroll;
  const unitPct = num(opts.unitPct, 1);
  const maxStakePct = num(opts.maxStakePct, 5);
  const kellyMult = num(opts.kellyMult, 0.25);
  const oneUnit = bankroll * (unitPct / 100);
  const fixedUnit = B0 * (unitPct / 100);

  if (method === "flat_fixed") return fixedUnit;
  if (method === "flat_compound") return oneUnit;

  const f = kellyFractionRaw(modelP, dec) * kellyMult;
  if (!Number.isFinite(f) || f <= 0) return 0;
  let stake = bankroll * Math.min(f, maxStakePct / 100);
  if (method === "kelly_unit_cap") stake = Math.min(stake, oneUnit);
  return Math.max(0, stake);
}
