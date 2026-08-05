/**
 * Walk-forward OOS bet policy — per-market EV, gap, side, and signal filters.
 * Gaps tuned for both-side play after outcome μ debias (see data/outcome_mu_debias.json).
 * Re-fit: npm run fit:outcome-mu-debias && npm run fit:bet-policy-from-oos
 */

export const DEFAULT_MIN_EV_PCT = 0;

/** @type {Record<string, object>} */
export const OOS_MARKET_POLICY = {
  GIR: {
    market: "GIR",
    minEv: 5,
    minGap: 0.5,
    side: "both",
    skipEventSubstrings: [],
  },
  Birdies: {
    market: "Birdies",
    minEv: 5,
    minGap: 0.35,
    side: "both",
    skipEventSubstrings: [],
  },
  "Total score": {
    market: "Total score",
    minEv: 5,
    minGap: 0.5,
    side: "both",
    skipEventSubstrings: [],
  },
  "Fairways hit": {
    market: "Fairways hit",
    minEv: 7.5,
    minGap: 1.0,
    side: "both",
    skipEventSubstrings: [],
  },
  Pars: {
    market: "Pars",
    minEv: 5,
    minGap: 0.5,
    side: "both",
    skipEventSubstrings: [],
  },
  Bogeys: {
    market: "Bogeys",
    minEv: 20,
    minGap: 1,
    side: "both",
    disabled: true,
    skipEventSubstrings: [],
  },
};

export const ACTION_MARKETS = new Set(
  Object.entries(OOS_MARKET_POLICY)
    .filter(([, p]) => !p.disabled)
    .map(([m]) => m),
);

/** @deprecated use ACTION_MARKETS */
export const PRIMARY_ACTION_MARKETS = ACTION_MARKETS;

export const MIN_EV_BY_MARKET = Object.fromEntries(
  Object.entries(OOS_MARKET_POLICY)
    .filter(([, p]) => !p.disabled && Number.isFinite(p.minEv))
    .map(([m, p]) => [m, p.minEv]),
);

export const MIN_LINE_GAP_BY_MARKET = Object.fromEntries(
  Object.entries(OOS_MARKET_POLICY)
    .filter(([, p]) => !p.disabled && Number.isFinite(p.minGap))
    .map(([m, p]) => [m, p.minGap]),
);

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function minEvForMarket(market, globalMinEv = DEFAULT_MIN_EV_PCT) {
  const p = OOS_MARKET_POLICY[market];
  if (p && !p.disabled && Number.isFinite(p.minEv)) return p.minEv;
  return Number.isFinite(globalMinEv) ? globalMinEv : DEFAULT_MIN_EV_PCT;
}

export function passesLineGap(market, modelLine, bookLine) {
  const p = OOS_MARKET_POLICY[market];
  if (!p || p.disabled) return false;
  if (!Number.isFinite(modelLine) || !Number.isFinite(bookLine)) return false;
  const gap = Math.abs(modelLine - bookLine);
  if (Number.isFinite(p.minGap) && gap < p.minGap) return false;
  if (Number.isFinite(p.maxGap) && gap > p.maxGap) return false;
  if (p.side === "over" && !(modelLine > bookLine)) return false;
  if (p.side === "under" && !(modelLine < bookLine)) return false;
  return true;
}

export function isActionableMarket(market) {
  const p = OOS_MARKET_POLICY[market];
  return Boolean(p && !p.disabled);
}

export function qualifiesBet({
  market,
  modelLine,
  bookLine,
  context = {},
  eventName = "",
  side = null,
  usePolicy = true,
}) {
  if (!usePolicy) return isActionableMarket(market);
  const p = OOS_MARKET_POLICY[market];
  if (!p || p.disabled) return false;
  if (!passesLineGap(market, modelLine, bookLine)) return false;
  if (side === "over" && p.side === "under") return false;
  if (side === "under" && p.side === "over") return false;
  if (Number.isFinite(p.minGirMinusFw) && num(context.gir_minus_fw) < p.minGirMinusFw) return false;
  if (Number.isFinite(p.minCourseFwWidth)) {
    const w = num(context.course_fw_width);
    if (Number.isFinite(w) && w < p.minCourseFwWidth) return false;
  }
  if (Array.isArray(p.rounds) && p.rounds.length) {
    const rnd = Math.round(num(context.round));
    if (!p.rounds.includes(rnd)) return false;
  }
  if (Array.isArray(p.skipEventSubstrings) && eventName) {
    if (p.skipEventSubstrings.some((s) => eventName.includes(s))) return false;
  }
  return true;
}
