/**
 * Prop Pricing Model bet side (Excel) + prior-round SG / BoB% / GIR% filter.
 */
import { modelEdgePctAtLine } from "./round-projection-mu.mjs";
import { pickBetSideWithSgPolicy, priorSignalsFromRow } from "./sg-side-policy.mjs";
import { minEvForMarket } from "./bet-policy.mjs";

export function pricingRowFromBetRow(r) {
  return {
    round_sd: r.round_sd,
    total_score: r.market === "Total score" ? r.model : undefined,
  };
}

export function priorRowFromBetRow(r) {
  return priorSignalsFromRow(r);
}

/**
 * @returns {{ side: "OVER"|"UNDER", edge: number, sgReason: string, edgeOver: number, edgeUnder: number } | null}
 */
export function pickPropPricingSide(r, minEvPct) {
  const mu = Number.isFinite(r.adjModel) ? r.adjModel : r.model;
  if (!Number.isFinite(mu) || !Number.isFinite(r.book)) return null;
  const minEv = Number.isFinite(minEvPct) ? minEvPct : minEvForMarket(r.market, 0);
  const pricingRow = pricingRowFromBetRow({ ...r, model: mu });
  const { edgeOver, edgeUnder } = modelEdgePctAtLine(
    r.market,
    mu,
    r.book,
    pricingRow,
    {},
    r.overOdds,
    r.underOdds,
  );
  const pick = pickBetSideWithSgPolicy(edgeOver, edgeUnder, minEv, r.market, priorRowFromBetRow(r));
  if (!pick) return null;
  return {
    side: pick.side === "under" ? "UNDER" : "OVER",
    edge: pick.edge,
    sgReason: pick.sgReason || "",
    edgeOver,
    edgeUnder,
  };
}
