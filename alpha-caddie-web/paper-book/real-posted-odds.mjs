/**
 * Paper book only accepts odds returned directly from each book's API/scrape.
 */
import { legPayoutMultiplierFromBookOdds } from "./book-odds-display.mjs";

export function isDirectBookOddsCard(card) {
  if (!card || typeof card !== "object") return false;
  const src = String(card.oddsSource || "");
  if (!src || src === "projections_pipeline") return false;
  if (!card.overBookOdds || !card.underBookOdds) return false;
  if (!Number.isFinite(card.line)) return false;
  if (!Number.isFinite(legPayoutMultiplierFromBookOdds(card.overBookOdds))) return false;
  if (!Number.isFinite(legPayoutMultiplierFromBookOdds(card.underBookOdds))) return false;
  return true;
}

export function isDirectBookOddsLeg(leg) {
  if (!leg?.bookOdds) return false;
  return Number.isFinite(legPayoutMultiplierFromBookOdds(leg.bookOdds));
}
