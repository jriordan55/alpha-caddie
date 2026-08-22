/**
 * Auto-grade paper book bets from round_projection_vs_actual.csv (browser + Node).
 */
import { buildOuGradeIndex, gradeOuBet, normEventName, normPlayerName } from "../projection-tracker/my-bets-grade.mjs";
import { DETAIL_EXPORT_MARKETS } from "../projection-tracker/detail-market-specs.mjs";
import { bookById } from "./live-book-options-core.mjs";
import {
  calcDkSinglePnl,
  calcPickemParlayPnl,
  calcPrizePicksEntryPnl,
} from "./book-payouts.mjs";
import { normalizePersistedState, PAPER_BOOK_IDS } from "./paper-book-state.mjs";

const MARKET_SPECS = DETAIL_EXPORT_MARKETS.map((m) => {
  const stem = m.key === "total" ? "round_score" : m.key === "fairways" ? "fairways" : m.key;
  return {
    ...m,
    bookCol: m.bookLineCol,
    overRes: `${stem}_over`,
    underRes: `${stem}_under`,
    actual: m.key === "total" ? "actual_round_score" : `actual_${m.key === "fairways" ? "fairways" : m.key}`,
  };
});

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
      continue;
    }
    if (ch === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  out.push(cur);
  return out;
}

export function buildOuGradeIndexFromCsvText(text) {
  const lines = String(text || "")
    .split(/\r?\n/)
    .filter(Boolean);
  if (lines.length < 2) return null;
  const header = parseCsvLine(lines[0]);
  const rows = [];
  for (let i = 1; i < lines.length; i++) {
    const cells = parseCsvLine(lines[i]);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
    rows.push(row);
  }
  return buildOuGradeIndex(rows, MARKET_SPECS);
}

export function lookupRoundScore(entry, dgId, playerName, ouGradeIndex) {
  if (!ouGradeIndex) return NaN;
  const event = normEventName(entry.eventName);
  const round = Math.round(Number(entry.round));
  const dg = Math.round(Number(dgId));
  const player = normPlayerName(playerName);
  const byDg = Number.isFinite(dg) ? `${event}|${round}|dg:${dg}` : "";
  const byName = player ? `${event}|${round}|${player}` : "";
  const hit = (byDg && ouGradeIndex.get(byDg)) || (byName && ouGradeIndex.get(byName));
  const score = Number(hit?.actual_round_score);
  return Number.isFinite(score) ? score : NaN;
}

export function gradeLeg(leg, entry, ouGradeIndex) {
  if (leg.cardKind === "matchup") {
    const s1 = lookupRoundScore(entry, leg.p1_dg_id, leg.p1_player_name, ouGradeIndex);
    const s2 = lookupRoundScore(entry, leg.p2_dg_id, leg.p2_player_name, ouGradeIndex);
    if (!Number.isFinite(s1) || !Number.isFinite(s2)) return null;
    if (s1 === s2) return "P";
    const p1Wins = s1 < s2;
    const pickedP1 = leg.side === "p1";
    return pickedP1 === p1Wins ? "W" : "L";
  }
  if (!ouGradeIndex) return null;
  return gradeOuBet(
    {
      eventName: entry.eventName,
      round: entry.round,
      dg_id: leg.dg_id,
      playerName: leg.playerName,
      market: leg.market,
      side: leg.side,
      line: leg.line,
    },
    ouGradeIndex,
  );
}

export function settleEntry(entry, ouGradeIndex) {
  const book = bookById(entry.bookId);
  const legs = entry.legs.map((leg) => ({
    ...leg,
    result: gradeLeg(leg, entry, ouGradeIndex) || leg.result || "open",
  }));

  const allGraded = legs.every((l) => {
    const r = String(l.result || "").toUpperCase();
    return r === "W" || r === "L" || r === "P";
  });
  if (!allGraded) return false;

  let pnl = 0;
  let result = "loss";

  if (book.mode === "sportsbook") {
    const leg = legs[0];
    const r = String(leg.result).toUpperCase();
    pnl = calcDkSinglePnl(entry.stake, leg.odds, r);
    if (r === "W") result = "win";
    else if (r === "P") result = "push";
    else result = "loss";
  } else if (book.id === "prizepicks") {
    pnl = calcPrizePicksEntryPnl(legs, entry.stake, entry.playType);
    result = pnl > 0 ? "win" : "loss";
  } else {
    pnl = calcPickemParlayPnl(legs, entry.stake);
    result = pnl > 0 ? "win" : "loss";
  }

  entry.legs = legs;
  entry.result = result;
  entry.pnl = pnl;
  entry.settledAt = new Date().toISOString();
  return true;
}

/**
 * Grade all open bets in persisted state; returns bankroll deltas per book.
 */
export function gradePersistedState(persisted, ouGradeIndex) {
  const out = normalizePersistedState(persisted);
  if (!ouGradeIndex) return { persisted: out, changedCount: 0 };

  let changedCount = 0;
  for (const bookId of PAPER_BOOK_IDS) {
    const slice = out.books[bookId];
    if (!slice?.history?.length) continue;

    let bankroll = Number(slice.bankroll) || 0;
    let bookChanged = 0;
    for (const entry of slice.history) {
      if (entry.result !== "open") continue;
      if (settleEntry(entry, ouGradeIndex)) {
        bankroll += Number(entry.stake) + (Number(entry.pnl) || 0);
        bookChanged++;
      }
    }
    if (bookChanged > 0) {
      out.books[bookId] = { ...slice, bankroll, history: slice.history };
      changedCount += bookChanged;
    }
  }

  if (changedCount > 0) {
    out.updated_at = new Date().toISOString();
  }
  return { persisted: out, changedCount };
}
