/**
 * Paper book prop cards — from baked paper-book-lines.json (push:live), not browser API calls.
 */
import { bookById } from "./live-book-options-core.mjs";
import {
  formatBookOddsDisplay,
  lookupDirectCard,
  sideBookOddsFromCard,
  sidePayoutMultiplierFromCard,
} from "./book-api-fetch.mjs";

export {
  PAPER_BOOKS,
  bookById,
  liveTargetRound,
  playersForRound,
  formatLine,
  marketShortLabel,
} from "./live-book-options-core.mjs";

export { formatBookOddsDisplay, lookupDirectCard, sideBookOddsFromCard, sidePayoutMultiplierFromCard };

/** @type {object|null} */
let bakedCatalog = null;

export function setBakedBookCatalog(catalog) {
  bakedCatalog = catalog && typeof catalog === "object" ? catalog : null;
}

export function getBakedBookCatalog() {
  return bakedCatalog;
}

/**
 * @param {object} _projections
 * @param {string} bookId
 */
export function buildLivePropCards(_projections, bookId) {
  const book = bookById(bookId);
  const entry = bakedCatalog?.books?.[bookId];

  if (entry) {
    return {
      round: bakedCatalog.round,
      roundLabel: bakedCatalog.round_label || `R${bakedCatalog.round || 1}`,
      eventName: bakedCatalog.event_name || "",
      cards: Array.isArray(entry.cards) ? entry.cards : [],
      hasRealPostedOdds: (entry.cards || []).length > 0,
      linesInFeed: entry.count ?? (entry.cards || []).length,
      fetchError: entry.fetchError || "",
      fetchedAt: entry.fetchedAt || bakedCatalog.updated_at,
      book,
      fromBaked: true,
    };
  }

  return {
    round: bakedCatalog?.round || 1,
    roundLabel: bakedCatalog?.round_label || "R1",
    eventName: bakedCatalog?.event_name || "",
    cards: [],
    hasRealPostedOdds: false,
    linesInFeed: 0,
    fetchError: bakedCatalog
      ? `No ${book.label} lines in paper-book-lines.json`
      : "Missing paper-book-lines.json — run npm run push:live",
    fetchedAt: bakedCatalog?.updated_at,
    book,
    fromBaked: false,
  };
}

export function formatPostedOdds(_book, bookOddsOrAmerican) {
  if (bookOddsOrAmerican && typeof bookOddsOrAmerican === "object" && bookOddsOrAmerican.kind) {
    return formatBookOddsDisplay(bookOddsOrAmerican);
  }
  const am = Number(bookOddsOrAmerican);
  if (!Number.isFinite(am) || am === 0) return "—";
  return am > 0 ? `+${Math.round(am)}` : String(Math.round(am));
}
