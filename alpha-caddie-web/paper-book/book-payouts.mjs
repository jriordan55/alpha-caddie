/**
 * Book-accurate payout rules for paper trading golf props.
 */
import { americanToDecimal, formatAmerican } from "../projection-tracker/ev-math.mjs";
import { calcManualBetPnl } from "../projection-tracker/my-bets-journal.mjs";

/** PrizePicks Power Play multipliers (stake × mult on full win). */
export const PP_POWER_MULT = Object.freeze({
  2: 3,
  3: 5,
  4: 10,
  5: 20,
  6: 37.5,
});

/** PrizePicks Flex — hits → payout multiplier. */
export const PP_FLEX_MULT = Object.freeze({
  3: { 3: 3, 2: 1 },
  4: { 4: 6, 3: 1.5, 2: 0.4 },
  5: { 5: 10, 4: 2, 3: 0.4 },
  6: { 6: 25, 5: 2, 4: 0.4 },
});

import { legPayoutMultiplierFromBookOdds } from "./book-api-fetch.mjs";

export function oddsToMultiplier(amOrLeg) {
  if (amOrLeg && typeof amOrLeg === "object" && amOrLeg.bookOdds) {
    return legPayoutMultiplierFromBookOdds(amOrLeg.bookOdds);
  }
  if (amOrLeg && typeof amOrLeg === "object" && amOrLeg.kind) {
    return legPayoutMultiplierFromBookOdds(amOrLeg);
  }
  const dec = americanToDecimal(amOrLeg);
  if (!Number.isFinite(dec) || dec <= 1) return NaN;
  return dec;
}

export function formatMultiplier(mult) {
  const m = Number(mult);
  if (!Number.isFinite(m) || m <= 0) return "—";
  return `${(Math.round(m * 100) / 100).toFixed(2).replace(/\.?0+$/, "")}x`;
}

/** Single DraftKings O/U leg P/L. */
export function calcDkSinglePnl(stake, americanOdds, result) {
  return calcManualBetPnl(stake, americanOdds, result);
}

/**
 * Pick'em parlay payout multiplier = product of leg multipliers (Sleeper / Underdog style).
 * @param {object[]} legs — each { odds, result: W|L|P|open }
 */
export function calcPickemParlayMultiplier(legs) {
  let mult = 1;
  for (const leg of legs) {
    const r = String(leg.result || "").toUpperCase();
    if (r === "P") continue;
    if (r !== "W") return 0;
    const legMult =
      Number.isFinite(leg.payoutMultiplier) ? leg.payoutMultiplier : oddsToMultiplier(leg);
    if (!Number.isFinite(legMult)) return 0;
    mult *= legMult;
  }
  return mult;
}

/**
 * @param {object[]} legs
 * @param {number} stake
 */
export function calcPickemParlayPnl(legs, stake) {
  const s = Number(stake);
  if (!Number.isFinite(s) || s <= 0) return 0;
  const mult = calcPickemParlayMultiplier(legs);
  if (mult <= 0) return -s;
  return s * (mult - 1);
}

/**
 * PrizePicks entry payout after all legs settled.
 * @param {'power'|'flex'} playType
 */
export function calcPrizePicksEntryPnl(legs, stake, playType = "power") {
  const s = Number(stake);
  if (!Number.isFinite(s) || s <= 0) return 0;
  const n = legs.length;
  if (n < 2) return 0;

  let wins = 0;
  let losses = 0;
  let pushes = 0;
  for (const leg of legs) {
    const r = String(leg.result || "").toUpperCase();
    if (r === "W") wins++;
    else if (r === "L") losses++;
    else if (r === "P") pushes++;
    else return NaN;
  }

  if (playType === "power") {
    if (losses > 0) return -s;
    const mult = PP_POWER_MULT[n];
    if (!mult) return -s;
    return s * (mult - 1);
  }

  const table = PP_FLEX_MULT[n];
  if (!table) return -s;
  const mult = table[wins];
  if (!Number.isFinite(mult) || mult <= 0) return -s;
  return s * (mult - 1);
}

export function describeEntryPayout(bookId, legs, playType, stake) {
  const s = Number(stake) || 0;
  if (bookId === "draftkings") {
    const leg = legs[0];
    if (!leg) return "Add a pick";
    const disp = leg.bookOdds?.display || formatAmerican(leg.odds);
    return `${disp} · max win ${formatMultiplier(leg.payoutMultiplier || oddsToMultiplier(leg))}`;
  }
  if (bookId === "prizepicks") {
    const n = legs.length;
    if (n < 2) return `Pick ${2 - n} more`;
    const mult = playType === "flex" ? PP_FLEX_MULT[n]?.[n] : PP_POWER_MULT[n];
    const win = Number.isFinite(mult) ? s * mult : 0;
    return playType === "flex"
      ? `Flex ${n}-pick · up to ${formatMultiplier(mult)} ($${win.toFixed(0)})`
      : `Power ${n}-pick · ${formatMultiplier(mult)} ($${win.toFixed(0)})`;
  }
  const parMult = calcPickemParlayMultiplier(
    legs.map((l) => ({ ...l, result: "W" })),
  );
  if (!Number.isFinite(parMult) || legs.length < 2) {
    return legs.length < 2 ? `Pick ${2 - legs.length} more` : "—";
  }
  return `${formatMultiplier(parMult)} payout · $${(s * parMult).toFixed(0)} to win`;
}

export { formatAmerican };
