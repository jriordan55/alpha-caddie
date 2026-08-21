/**
 * Browser-safe book odds display + payout helpers (no Node / scripts imports).
 */

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function formatAmericanDisplay(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return "—";
  return v > 0 ? `+${v}` : String(v);
}

export function formatBookOddsDisplay(bookOdds) {
  return bookOdds?.display || "—";
}

export function legPayoutMultiplierFromBookOdds(bookOdds) {
  if (!bookOdds) return NaN;
  if (bookOdds.kind === "multiplier") return num(bookOdds.raw, NaN);
  if (bookOdds.kind === "decimal") return num(bookOdds.raw, NaN);
  if (bookOdds.kind === "american") {
    const am = num(bookOdds.raw, NaN);
    if (!Number.isFinite(am)) return NaN;
    return am > 0 ? 1 + am / 100 : 1 + 100 / Math.abs(am);
  }
  return NaN;
}

export function sideBookOddsFromCard(card, side) {
  return side === "under" ? card.underBookOdds : card.overBookOdds;
}

export function sidePayoutMultiplierFromCard(card, side) {
  return side === "under" ? card.underPayoutMultiplier : card.overPayoutMultiplier;
}

export function lookupDirectCard(cards, dgId, market) {
  const mkt = String(market || "").trim();
  const id = dgId;
  return (
    cards.find(
      (c) =>
        c.market === mkt &&
        (c.dg_id === id || String(c.dg_id) === String(id) || c.cardKey?.startsWith(`${id}|`)),
    ) || null
  );
}

export function formatPostedOdds(_book, bookOddsOrAmerican) {
  if (bookOddsOrAmerican && typeof bookOddsOrAmerican === "object" && bookOddsOrAmerican.kind) {
    return formatBookOddsDisplay(bookOddsOrAmerican);
  }
  const am = Number(bookOddsOrAmerican);
  if (!Number.isFinite(am) || am === 0) return "—";
  return formatAmericanDisplay(am);
}
