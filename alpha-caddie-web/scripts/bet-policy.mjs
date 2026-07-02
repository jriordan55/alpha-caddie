/**
 * Honest bet qualification — uniform EV only; edge comes from projections, not side/gap filters.
 */

export const DEFAULT_MIN_EV_PCT = 10;

export const ACTION_MARKETS = new Set(["GIR", "Total score", "Birdies", "Fairways hit"]);

/** @deprecated use ACTION_MARKETS */
export const PRIMARY_ACTION_MARKETS = ACTION_MARKETS;

export const MIN_EV_BY_MARKET = {};
export const MIN_LINE_GAP_BY_MARKET = {};

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function minEvForMarket(_market, globalMinEv = DEFAULT_MIN_EV_PCT) {
  return Number.isFinite(globalMinEv) ? globalMinEv : DEFAULT_MIN_EV_PCT;
}

export function passesLineGap(_market, _modelLine, _bookLine) {
  return true;
}

export function isActionableMarket(market) {
  return ACTION_MARKETS.has(market);
}

export function qualifiesBet({ market, usePolicy = true }) {
  if (!usePolicy) return true;
  return isActionableMarket(market);
}
