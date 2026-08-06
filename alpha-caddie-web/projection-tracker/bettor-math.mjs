/**
 * Bettor-desktop–style odds / CLV / variance math, customized for golf backtests.
 * CLV is measured in probability points (not decimal-odds ratio).
 */

import { americanToDecimal, impliedProbFromAmerican, num } from "./ev-math.mjs";

/** Cents axis between two American prices (handles crossing even money). */
export function centsBetween(americanA, americanB) {
  const a = Math.round(num(americanA, NaN));
  const b = Math.round(num(americanB, NaN));
  if (!Number.isFinite(a) || !Number.isFinite(b) || a === 0 || b === 0) return NaN;
  const toCents = (am) => (am > 0 ? am + 100 : am < 0 ? am - 100 : NaN);
  const ca = toCents(a);
  const cb = toCents(b);
  if (!Number.isFinite(ca) || !Number.isFinite(cb)) return NaN;
  return Math.abs(cb - ca);
}

/**
 * Closing line value — lead with probability points.
 * @param {number} betAmerican
 * @param {number} closeAmerican same side at close
 * @param {number} [opposingCloseAmerican] other side at close (required for fair EV)
 */
export function computeClv(betAmerican, closeAmerican, opposingCloseAmerican = NaN) {
  const betDec = americanToDecimal(betAmerican);
  const closeDec = americanToDecimal(closeAmerican);
  if (!Number.isFinite(betDec) || betDec <= 1 || !Number.isFinite(closeDec) || closeDec <= 1) {
    return null;
  }
  const betImplied = impliedProbFromAmerican(betAmerican);
  const closeImplied = impliedProbFromAmerican(closeAmerican);
  const ratio = betDec / closeDec - 1;
  const probPoints = closeImplied - betImplied;
  const cents = centsBetween(betAmerican, closeAmerican);
  const evVsRawClose = closeImplied * betDec - 1;

  let fairProb = NaN;
  let evVsFair = NaN;
  const opp = Math.round(num(opposingCloseAmerican, NaN));
  if (Number.isFinite(opp) && opp !== 0) {
    const oppImplied = impliedProbFromAmerican(opp);
    const total = closeImplied + oppImplied;
    if (total > 0) {
      fairProb = closeImplied / total;
      evVsFair = fairProb * betDec - 1;
    }
  }

  return {
    betDecimal: betDec,
    closeDecimal: closeDec,
    betImplied,
    closeImplied,
    ratio,
    probPoints,
    cents,
    evVsRawClose,
    fairProb: Number.isFinite(fairProb) ? fairProb : null,
    evVsFair: Number.isFinite(evVsFair) ? evVsFair : null,
    ratioToPointsDistortion:
      Math.abs(probPoints) > 1e-12 ? ratio / probPoints : NaN,
  };
}

/** Required hit rate to break even at American price (vig ignored on single price). */
export function breakevenWinRate(american) {
  const d = americanToDecimal(american);
  if (!Number.isFinite(d) || d <= 1) return NaN;
  return 1 / d;
}

/**
 * Unit-bet SD at a stated EV (as fraction, e.g. 0.05).
 * p = (1 + edge) / decimal so E[X] = edge.
 */
export function unitSdAtEdge(american, edgeFrac = 0.05) {
  const d = americanToDecimal(american);
  const e = num(edgeFrac, NaN);
  if (!Number.isFinite(d) || d <= 1 || !Number.isFinite(e)) return NaN;
  const p = (1 + e) / d;
  if (p <= 0 || p >= 1) return NaN;
  const win = d - 1;
  const ex2 = p * win * win + (1 - p) * 1;
  const varX = Math.max(0, ex2 - e * e);
  return Math.sqrt(varX);
}

/** Bets needed before mean edge clears 2σ of sampling noise. */
export function betsToClear2Sigma(american, edgeFrac = 0.05) {
  const sd = unitSdAtEdge(american, edgeFrac);
  const e = num(edgeFrac, NaN);
  if (!Number.isFinite(sd) || !Number.isFinite(e) || e <= 0) return NaN;
  return (2 * sd / e) ** 2;
}

export function priceBucket(american) {
  const a = Math.round(num(american, NaN));
  if (!Number.isFinite(a) || a === 0) return "unknown";
  if (a <= -200) return "≤-200";
  if (a <= -150) return "-199…-150";
  if (a <= -120) return "-149…-120";
  if (a < 0) return "-119…-100";
  if (a <= 120) return "+100…+120";
  if (a <= 200) return "+121…+200";
  if (a <= 400) return "+201…+400";
  return "≥+401";
}

const BUCKET_ORDER = [
  "≤-200",
  "-199…-150",
  "-149…-120",
  "-119…-100",
  "+100…+120",
  "+121…+200",
  "+201…+400",
  "≥+401",
  "unknown",
];

/** Mulberry32 — reproducible season sims (bettor-desktop seed idea). */
export function mulberry32(seed) {
  let t = seed >>> 0;
  return function next() {
    t += 0x6d2b79f5;
    let r = Math.imul(t ^ (t >>> 15), 1 | t);
    r ^= r + Math.imul(r ^ (r >>> 7), 61 | r);
    return ((r ^ (r >>> 14)) >>> 0) / 4294967296;
  };
}

/**
 * Monte Carlo equity paths for a fixed bet mix (1u each).
 * @param {{ odds: number, modelP?: number }[]} bets
 * @param {{ paths?: number, seed?: number, edgeFracFallback?: number }} [opts]
 */
export function simulateSeasonEquity(bets, opts = {}) {
  const paths = Math.max(100, Math.round(num(opts.paths, 2000)));
  const seed = Math.round(num(opts.seed, 42)) >>> 0;
  const edgeFb = num(opts.edgeFracFallback, 0.05);
  const rng = mulberry32(seed);
  const n = bets.length;
  if (!n) {
    return { seed, paths, nBets: 0, percentiles: null, pctDown: NaN, meanEnd: NaN };
  }

  const specs = bets.map((b) => {
    const d = americanToDecimal(b.odds);
    let p = num(b.modelP, NaN);
    if (!Number.isFinite(p) || p <= 0 || p >= 1) {
      p = Number.isFinite(d) && d > 1 ? Math.min(0.99, (1 + edgeFb) / d) : 0.5;
    }
    return { d: Number.isFinite(d) && d > 1 ? d : 1.91, p };
  });

  const ends = new Float64Array(paths);
  let down = 0;
  for (let i = 0; i < paths; i++) {
    let eq = 0;
    for (const s of specs) {
      eq += rng() < s.p ? s.d - 1 : -1;
    }
    ends[i] = eq;
    if (eq < 0) down++;
  }
  ends.sort();
  const pct = (q) => ends[Math.min(paths - 1, Math.max(0, Math.floor(q * (paths - 1))))];
  let sum = 0;
  for (let i = 0; i < paths; i++) sum += ends[i];
  return {
    seed,
    paths,
    nBets: n,
    meanEnd: sum / paths,
    pctDown: (100 * down) / paths,
    percentiles: {
      p5: pct(0.05),
      p25: pct(0.25),
      p50: pct(0.5),
      p75: pct(0.75),
      p95: pct(0.95),
    },
  };
}

/** Breakeven ladder for a list of representative American prices. */
export function breakevenLadder(americans, edgeFrac = 0.05) {
  return americans.map((am) => ({
    american: am,
    breakeven: breakevenWinRate(am),
    sdPerUnit: unitSdAtEdge(am, edgeFrac),
    betsTo2Sigma: betsToClear2Sigma(am, edgeFrac),
  }));
}

/**
 * Aggregate bet-mix stats by price bucket.
 * @param {{ odds: number, unitPnl: number, clv?: object|null, expectedVsFair?: number }[]} bets
 */
export function betMixByPrice(bets) {
  /** @type {Map<string, object>} */
  const m = new Map();
  for (const b of bets) {
    const key = priceBucket(b.odds);
    let g = m.get(key);
    if (!g) {
      g = {
        bucket: key,
        bets: 0,
        wins: 0,
        losses: 0,
        realized: 0,
        expectedFair: 0,
        expectedFairN: 0,
        clvPtsSum: 0,
        clvPtsN: 0,
        clvRatioSum: 0,
      };
      m.set(key, g);
    }
    g.bets++;
    if (b.res === "W" || b.unitPnl > 0) g.wins++;
    else if (b.res === "L" || b.unitPnl < 0) g.losses++;
    g.realized += num(b.unitPnl, 0);
    const exp = num(b.expectedVsFair, NaN);
    if (Number.isFinite(exp)) {
      g.expectedFair += exp;
      g.expectedFairN++;
    }
    const pts = num(b.clv?.probPoints, NaN);
    if (Number.isFinite(pts)) {
      g.clvPtsSum += pts;
      g.clvPtsN++;
      g.clvRatioSum += num(b.clv?.ratio, 0);
    }
  }
  return BUCKET_ORDER.filter((k) => m.has(k)).map((k) => {
    const g = m.get(k);
    const gap = g.expectedFairN
      ? g.realized - g.expectedFair
      : NaN;
    return {
      bucket: g.bucket,
      bets: g.bets,
      hit_pct: g.bets ? (100 * g.wins) / g.bets : NaN,
      realized_units: round4(g.realized),
      expected_fair_units: g.expectedFairN ? round4(g.expectedFair) : null,
      gap_units: Number.isFinite(gap) ? round4(gap) : null,
      mean_clv_prob_pts: g.clvPtsN ? round4((100 * g.clvPtsSum) / g.clvPtsN) : null,
      mean_clv_ratio_pct: g.clvPtsN ? round4((100 * g.clvRatioSum) / g.clvPtsN) : null,
      share_of_realized_abs:
        bets.length && Math.abs(g.realized) > 0
          ? round4(Math.abs(g.realized))
          : 0,
    };
  });
}

export function summarizeBetLog(bets) {
  let realized = 0;
  let expectedFair = 0;
  let expectedFairN = 0;
  let expectedRawClose = 0;
  let expectedRawN = 0;
  let clvPts = 0;
  let clvN = 0;
  let clvRatio = 0;
  let wins = 0;
  let losses = 0;
  let pushes = 0;
  for (const b of bets) {
    realized += num(b.unitPnl, 0);
    if (b.res === "W") wins++;
    else if (b.res === "L") losses++;
    else if (b.res === "P") pushes++;
    const ef = num(b.expectedVsFair, NaN);
    if (Number.isFinite(ef)) {
      expectedFair += ef;
      expectedFairN++;
    }
    const er = num(b.expectedVsRawClose, NaN);
    if (Number.isFinite(er)) {
      expectedRawClose += er;
      expectedRawN++;
    }
    const pts = num(b.clv?.probPoints, NaN);
    if (Number.isFinite(pts)) {
      clvPts += pts;
      clvN++;
      clvRatio += num(b.clv?.ratio, 0);
    }
  }
  const gap = expectedFairN ? realized - expectedFair : NaN;
  return {
    bets: bets.length,
    wins,
    losses,
    pushes,
    hit_pct: bets.length ? round4((100 * wins) / (wins + losses || bets.length)) : NaN,
    realized_units: round4(realized),
    expected_fair_units: expectedFairN ? round4(expectedFair) : null,
    expected_raw_close_units: expectedRawN ? round4(expectedRawClose) : null,
    gap_realized_minus_fair: Number.isFinite(gap) ? round4(gap) : null,
    mean_clv_prob_pts: clvN ? round4((100 * clvPts) / clvN) : null,
    mean_clv_ratio_pct: clvN ? round4((100 * clvRatio) / clvN) : null,
    clv_sample: clvN,
  };
}

function round4(x) {
  return Math.round(num(x, 0) * 1e4) / 1e4;
}

export { BUCKET_ORDER };
