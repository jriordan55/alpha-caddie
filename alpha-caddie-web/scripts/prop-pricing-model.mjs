/**
 * Prop_Pricing_Model_2.xlsx — distribution-based O/U pricing for golf props.
 * Shared by Round projections (app.js), projection backtest (ev-math / round-projection-mu).
 */

export const PROP_PRICING_MIN_HISTORY_N = 8;

const PGA_BENCHMARKS = Object.freeze({
  "Total score": { mean: 70.41, sd: 3.31 },
  Birdies: { mean: 3.65, sd: 1.9 },
  Pars: { mean: 10.81, sd: 2.89 },
  Bogeys: { mean: 2.5, sd: 1.69 },
  GIR: { mean: 10.75, sd: 2.07 },
  "Fairways hit": { mean: 9.74, sd: 2.25 },
});

const SD_MULT_FALLBACK = Object.freeze({
  "Total score": 0.047,
  Birdies: 0.52,
  Bogeys: 0.68,
  Pars: 0.27,
  GIR: 0.19,
  "Fairways hit": 0.21,
});

function num(v, fb = NaN) {
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

export function propPricingUsesLogNormalSkew(market) {
  return market === "Total score" || market === "Bogeys";
}

export function sigmaOuDiscreteCounting(market, muAbs, fairwayHoles = 14) {
  const m = num(muAbs, NaN);
  if (!Number.isFinite(m) || m <= 0) return 2.4;
  if (market === "GIR") {
    const p = clamp(m / 18, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(18 * p * (1 - p)));
  }
  if (market === "Fairways hit") {
    const n = Math.round(num(fairwayHoles, 14)) || 14;
    const p = clamp(m / n, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(n * p * (1 - p)));
  }
  if (market === "Birdies" || market === "Bogeys") return clamp(Math.sqrt(m * 1.08), 1.05, 3.15);
  if (market === "Pars") return clamp(Math.sqrt(m * 1.06), 1.15, 3.35);
  return Math.max(0.55, Math.sqrt(Math.max(m, 0.2)) * 0.9);
}

export function propPricingLeagueSdMultiplier(market, fairwayHoles = 14) {
  const bench = PGA_BENCHMARKS[market];
  if (bench && bench.mean > 0 && bench.sd > 0) return bench.sd / bench.mean;
  return SD_MULT_FALLBACK[market] || 0.35;
}

export function propPricingNormalProbOver(mu, sigma, line) {
  if (!Number.isFinite(mu) || !Number.isFinite(sigma) || !Number.isFinite(line) || sigma <= 0) return NaN;
  return 1 - normalCdf((line - mu) / sigma);
}

export function propPricingLogNormalProbOver(mu, sigma, line) {
  if (!Number.isFinite(mu) || !Number.isFinite(sigma) || !Number.isFinite(line)) return NaN;
  if (mu <= 0 || sigma <= 0 || line <= 0) return NaN;
  const sigmaLn2 = Math.log(1 + (sigma / mu) ** 2);
  const sigmaLn = Math.sqrt(sigmaLn2);
  const muLn = Math.log(mu) - sigmaLn2 / 2;
  return 1 - normalCdf((Math.log(line) - muLn) / sigmaLn);
}

/**
 * Layer 1–2 σ: player round_sd / measured history SD, else projection × league multiplier.
 * @param {{ market: string, mu: number, row?: object, fairwayHoles?: number, playerHistSd?: number }} opts
 */
export function propPricingSigma(opts = {}) {
  const market = opts.market || "Total score";
  const mu = num(opts.mu, NaN);
  const row = opts.row && typeof opts.row === "object" ? opts.row : {};
  const fairwayHoles = Math.round(num(opts.fairwayHoles, 14)) || 14;
  const playerHistSd = num(opts.playerHistSd, NaN);

  if (market === "Total score") {
    const s = num(row.round_sd ?? row.roundSd, NaN);
    if (Number.isFinite(s) && s > 0.05) return s;
  }

  if (Number.isFinite(playerHistSd) && playerHistSd > 0.05) return playerHistSd;

  if (Number.isFinite(mu) && mu > 0) {
    let sig = mu * propPricingLeagueSdMultiplier(market, fairwayHoles);
    if (market === "GIR" || market === "Fairways hit") {
      sig = Math.max(sig, sigmaOuDiscreteCounting(market, mu, fairwayHoles));
    }
    const bench = PGA_BENCHMARKS[market];
    if (bench && bench.sd > 0) sig = Math.max(sig, bench.sd * 0.88);
    return sig;
  }

  return sigmaOuDiscreteCounting(market, Math.abs(mu) || 2.75, fairwayHoles);
}

export function propPricingProbOver(market, mu, line, opts = {}) {
  if (!Number.isFinite(mu) || !Number.isFinite(line)) return NaN;
  const sig = propPricingSigma({ market, mu, ...opts });
  if (!Number.isFinite(sig) || sig <= 0) return NaN;
  if (propPricingUsesLogNormalSkew(market)) return propPricingLogNormalProbOver(mu, sig, line);
  return propPricingNormalProbOver(mu, sig, line);
}

export function propPricingExpectedRoi(prob, americanOdds) {
  const p = num(prob, NaN);
  const am = Math.round(num(americanOdds, NaN));
  if (!Number.isFinite(p) || !Number.isFinite(am) || am === 0) return NaN;
  const dec = am > 0 ? 1 + am / 100 : 1 + 100 / Math.abs(am);
  if (dec <= 1) return NaN;
  return p * dec - 1;
}

/** Sample SD from numeric round values (Layer 2 game-log builder). */
export function stdDevFromValues(vals) {
  if (!Array.isArray(vals) || vals.length < PROP_PRICING_MIN_HISTORY_N) return NaN;
  const nums = vals.filter((v) => Number.isFinite(v));
  if (nums.length < PROP_PRICING_MIN_HISTORY_N) return NaN;
  const mean = nums.reduce((a, b) => a + b, 0) / nums.length;
  const variance = nums.reduce((a, v) => a + (v - mean) ** 2, 0) / (nums.length - 1);
  const sd = Math.sqrt(variance);
  return Number.isFinite(sd) && sd > 0.05 ? sd : NaN;
}
