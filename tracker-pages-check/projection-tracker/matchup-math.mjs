/**
 * Browser-safe round matchup pricing (mirrors app.js matchup +EV logic).
 */
import { clamp, num } from "./ev-math.mjs";

const ROUND_MATCHUP_MARKET = "Round matchups";
const MATCHUP_KIND = "round_matchups";
const SIGMA_DIFF = 2.85;
/** Round matchups in projection tracker use DraftKings only (matches O/U DK audit). */
export const ROUND_MATCHUP_BOOK = "draftkings";

function normMatchupBookKey(bk) {
  const norm = String(bk || "")
    .trim()
    .toLowerCase();
  if (norm === "dk") return "draftkings";
  return norm;
}

export function isDraftKingsMatchupBook(book) {
  const norm = normMatchupBookKey(book);
  return norm === "draftkings";
}

function normalCdf(z) {
  const t = 1 / (1 + 0.2316419 * Math.abs(z));
  const d = 0.3989423 * Math.exp((-z * z) / 2);
  const p =
    d *
    t *
    (0.3193815 + t * (-0.3565638 + t * (1.7814779 + t * (-1.821256 + t * 1.3302744))));
  return z >= 0 ? 1 - p : p;
}

export function modeledMuSgFromRow(row) {
  const mu = num(row?.mu_sg, NaN);
  const implied = num(row?.implied_mu_sg, NaN);
  if (Number.isFinite(implied) && Number.isFinite(mu) && Math.abs(mu) < 1e-9 && Math.abs(implied) > 1e-9) {
    return implied;
  }
  if (Number.isFinite(mu)) return mu;
  if (Number.isFinite(implied)) return implied;
  const stp = num(row?.score_to_par, NaN);
  if (Number.isFinite(stp)) return -stp;
  return NaN;
}

export function effectiveMuSgForMatchup(row) {
  return modeledMuSgFromRow(row);
}

export function matchupWinProb(mu1, mu2, marketKind = MATCHUP_KIND) {
  if (!Number.isFinite(mu1) || !Number.isFinite(mu2)) return NaN;
  const sigmaDiff = marketKind === "tournament_matchups" ? 1.55 : SIGMA_DIFF;
  const d = (mu1 - mu2) / sigmaDiff;
  return clamp(normalCdf(d), 0.12, 0.88);
}

export function matchupOddsDecodeScalar(raw, oddsFormat = "") {
  const v = num(raw, NaN);
  if (!Number.isFinite(v) || v === 0) return NaN;
  const fmt = String(oddsFormat || "").toLowerCase();
  if (fmt === "american" || fmt === "us") return americanToDecimal(Math.round(v));
  if (v > 1 && v <= 80) return v;
  if (v >= 100 || v <= -1) return americanToDecimal(Math.round(v));
  if (v > 80 && v < 100 && Number.isInteger(v)) return americanToDecimal(Math.round(v));
  return NaN;
}

export function matchupOddsTwoWayFromPack(pack, oddsFormat = "") {
  if (!pack || typeof pack !== "object") return { d1: NaN, d2: NaN };
  const d1 = matchupOddsDecodeScalar(pack.p1 ?? pack.P1 ?? pack.player_1 ?? pack.line_1 ?? pack.home, oddsFormat);
  const d2 = matchupOddsDecodeScalar(pack.p2 ?? pack.P2 ?? pack.player_2 ?? pack.line_2 ?? pack.away, oddsFormat);
  return { d1, d2 };
}

export function americanToDecimal(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return NaN;
  return v > 0 ? 1 + v / 100 : 1 + 100 / Math.abs(v);
}

export function decimalToAmerican(dec) {
  const d = num(dec, NaN);
  if (!Number.isFinite(d) || d <= 1) return NaN;
  if (d >= 2) return Math.round((d - 1) * 100);
  return Math.round(-100 / (d - 1));
}

const SKIP_BOOKS = new Set(["datagolf", "dg_model"]);

/** DraftKings two-way pack only — used for round matchup pricing in projection tracker. */
export function filterDraftKingsMatchupOdds(oddsObj) {
  if (!oddsObj || typeof oddsObj !== "object") return null;
  for (const [bk, pack] of Object.entries(oddsObj)) {
    const norm = normMatchupBookKey(bk);
    if (norm !== ROUND_MATCHUP_BOOK || SKIP_BOOKS.has(norm)) continue;
    if (!pack || typeof pack !== "object") continue;
    return pack;
  }
  return null;
}

/** @deprecated use filterDraftKingsMatchupOdds */
export function filterActionableMatchupOdds(oddsObj) {
  const pack = filterDraftKingsMatchupOdds(oddsObj);
  return pack ? { [ROUND_MATCHUP_BOOK]: pack } : {};
}

export function draftKingsMatchupDecimals(oddsObj, oddsFormat = "") {
  const pack = filterDraftKingsMatchupOdds(oddsObj);
  if (!pack) return { book: "", d1: NaN, d2: NaN };
  const { d1, d2 } = matchupOddsTwoWayFromPack(pack, oddsFormat);
  return { book: ROUND_MATCHUP_BOOK, d1, d2 };
}

export function bestBookDecimalForSide(oddsObj, side, oddsFormat = "") {
  const { d1, d2, book } = draftKingsMatchupDecimals(oddsObj, oddsFormat);
  const d = side === "p1" ? d1 : d2;
  return { book, dec: d };
}

export function modelEvPct(modelProb, decimalOdds) {
  const p = num(modelProb, NaN);
  const d = num(decimalOdds, NaN);
  if (!Number.isFinite(p) || !Number.isFinite(d) || d <= 1) return NaN;
  return (p * d - 1) * 100;
}

/**
 * @param {object} opts
 * @param {object} opts.projections
 * @param {Map<number, object>} opts.players
 * @param {number} opts.round
 * @param {number} opts.minEvPct
 * @param {(market: string, oos: object|null) => number} [opts.marketFactor]
 * @param {(market: string, labels: string[], signals: object|null) => {boost: number, tags: string[]}} [opts.signalBoost]
 */
export function buildRoundMatchupPicks({
  projections,
  players,
  round,
  minEvPct,
  marketFactor = () => 1,
  signalBoost = () => ({ boost: 1, tags: [] }),
  oos = null,
  signals = null,
}) {
  const list = projections?.matchups?.round_matchups?.match_list;
  if (!Array.isArray(list) || !list.length) return [];

  const oddsFormat = String(projections?.meta?.matchups_odds_format || "").trim();
  const minEdge = num(minEvPct, 0);
  /** @type {object[]} */
  const out = [];

  for (const m of list) {
    const id1 = Math.round(num(m.p1_dg_id, NaN));
    const id2 = Math.round(num(m.p2_dg_id, NaN));
    if (!Number.isFinite(id1) || !Number.isFinite(id2)) continue;

    const row1 = players.get(id1);
    const row2 = players.get(id2);
    const mu1 = effectiveMuSgForMatchup(row1);
    const mu2 = effectiveMuSgForMatchup(row2);
    if (!Number.isFinite(mu1) || !Number.isFinite(mu2)) continue;

    const p1 = matchupWinProb(mu1, mu2, MATCHUP_KIND);
    const dk = draftKingsMatchupDecimals(m.odds || {}, oddsFormat);
    if (!Number.isFinite(dk.d1) || !Number.isFinite(dk.d2) || dk.d1 <= 1 || dk.d2 <= 1) continue;
    const edge1 = modelEvPct(p1, dk.d1);
    const edge2 = modelEvPct(1 - p1, dk.d2);

    const sides = [
      {
        side: "p1",
        player: row1,
        dg_id: id1,
        player_name: String(m.p1_player_name || row1?.player_name || "").trim(),
        opponent_name: String(m.p2_player_name || row2?.player_name || "").trim(),
        opponent_dg_id: id2,
        winProb: p1,
        muSg: mu1,
        oppMuSg: mu2,
        book: dk.book,
        dec: dk.d1,
        edgePct: edge1,
      },
      {
        side: "p2",
        player: row2,
        dg_id: id2,
        player_name: String(m.p2_player_name || row2?.player_name || "").trim(),
        opponent_name: String(m.p1_player_name || row1?.player_name || "").trim(),
        opponent_dg_id: id1,
        winProb: 1 - p1,
        muSg: mu2,
        oppMuSg: mu1,
        book: dk.book,
        dec: dk.d2,
        edgePct: edge2,
      },
    ];

    let best = null;
    for (const s of sides) {
      if (!Number.isFinite(s.edgePct) || s.edgePct < minEdge) continue;
      if (!best || s.edgePct > best.edgePct) best = s;
    }
    if (!best) continue;

    const labels = [`vs ${best.opponent_name}`, `Round matchups ${best.side}`];
    const { boost, tags } = signalBoost(ROUND_MATCHUP_MARKET, labels, signals);
    const mFac = marketFactor(ROUND_MATCHUP_MARKET, oos);
    const impliedPct = Number.isFinite(best.dec) && best.dec > 1 ? (1 / best.dec) * 100 : NaN;

    out.push({
      player_name: best.player_name,
      opponent_name: best.opponent_name,
      dg_id: best.dg_id,
      opponent_dg_id: best.opponent_dg_id,
      round,
      market: ROUND_MATCHUP_MARKET,
      pickType: "matchup",
      side: best.side,
      mu: best.winProb,
      gap: best.muSg - best.oppMuSg,
      line: impliedPct,
      book: best.book,
      odds: decimalToAmerican(best.dec),
      edgePct: best.edgePct,
      score: best.edgePct * mFac * boost,
      histRoi: NaN,
      histBets: 0,
      contextTags: tags,
      tailoringTags: [],
    });
  }

  return out;
}

export { ROUND_MATCHUP_MARKET };
