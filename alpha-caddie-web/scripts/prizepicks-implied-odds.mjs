/**
 * PrizePicks pick'em uses a fixed per-leg break-even (~57.6% at -136).
 * Env: PP_AMERICAN_ODDS — default -136 for both over and under.
 */
import { impliedProbFromAmerican } from "./round-projection-mu.mjs";

const PP_AMERICAN_ODDS = Math.round(Number(process.env.PP_AMERICAN_ODDS || -136));

/**
 * @param {object} prop
 */
export function applyPrizePicksImpliedOdds(prop) {
  if (!prop || typeof prop !== "object") return prop;
  const pImp = impliedProbFromAmerican(PP_AMERICAN_ODDS);
  return {
    ...prop,
    over_odds: PP_AMERICAN_ODDS,
    under_odds: PP_AMERICAN_ODDS,
    p_over_implied: pImp,
    p_under_implied: pImp,
    pp_odds_method: "prizepicks_fixed",
  };
}

/** @param {object[]} props */
export function applyPrizePicksImpliedOddsAll(props) {
  return (Array.isArray(props) ? props : []).map((r) => applyPrizePicksImpliedOdds(r));
}
