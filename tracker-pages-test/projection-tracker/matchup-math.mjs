/**
 * Browser-safe round matchup + 3-ball pricing (mirrors app.js +EV logic).
 * Matchup tracker books: DraftKings, FanDuel, BetMGM only.
 */
import { clamp, num } from "./ev-math.mjs";

const ROUND_MATCHUP_MARKET = "Round matchups";
const THREE_BALL_MARKET = "3-balls";
const MATCHUP_KIND = "round_matchups";
const THREE_BALL_KIND = "3_balls";
const SIGMA_DIFF = 2.85;
/** Softmax temperature for 3-ball win shares (matches app.js). */
const THREE_BALL_TEMP = 2.05;

/** Historical + live matchup tracker: DraftKings, FanDuel, BetMGM only. */
export const MATCHUP_TRACKER_BOOKS = Object.freeze(["draftkings", "fanduel", "betmgm"]);
export const MATCHUP_TRACKER_BOOK_SET = new Set(MATCHUP_TRACKER_BOOKS);

/** @deprecated prefer MATCHUP_TRACKER_BOOKS */
export const ROUND_MATCHUP_BOOK = "draftkings";

const SKIP_BOOKS = new Set(["datagolf", "dg_model"]);

export function normMatchupBookKey(bk) {
  const norm = String(bk || "")
    .trim()
    .toLowerCase();
  if (norm === "dk") return "draftkings";
  if (norm === "fd") return "fanduel";
  if (norm === "mgm" || norm === "bet mgm") return "betmgm";
  return norm;
}

export function isAllowedMatchupTrackerBook(book) {
  return MATCHUP_TRACKER_BOOK_SET.has(normMatchupBookKey(book));
}

/** @deprecated use isAllowedMatchupTrackerBook */
export function isDraftKingsMatchupBook(book) {
  return normMatchupBookKey(book) === "draftkings";
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

/** Win shares for a 3-ball (lowest score wins); softmax on μ_SG. */
export function threeBallModelProbs(mu1, mu2, mu3) {
  const m = [mu1, mu2, mu3].map((x) => num(x, NaN));
  if (m.some((x) => !Number.isFinite(x))) return [1 / 3, 1 / 3, 1 / 3];
  const ex = m.map((v) => Math.exp(v / THREE_BALL_TEMP));
  const s = ex[0] + ex[1] + ex[2];
  if (!(s > 0)) return [1 / 3, 1 / 3, 1 / 3];
  return [ex[0] / s, ex[1] / s, ex[2] / s];
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

export function matchupOddsThreeWayFromPack(pack, oddsFormat = "") {
  if (!pack || typeof pack !== "object") return { d1: NaN, d2: NaN, d3: NaN };
  const d1 = matchupOddsDecodeScalar(pack.p1 ?? pack.P1 ?? pack.player_1 ?? pack.line_1 ?? pack.home, oddsFormat);
  const d2 = matchupOddsDecodeScalar(pack.p2 ?? pack.P2 ?? pack.player_2 ?? pack.line_2 ?? pack.away, oddsFormat);
  const d3 = matchupOddsDecodeScalar(pack.p3 ?? pack.P3 ?? pack.player_3 ?? pack.line_3, oddsFormat);
  return { d1, d2, d3 };
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

/** Odds packs for DraftKings / FanDuel / BetMGM only. */
export function filterMatchupTrackerOdds(oddsObj) {
  if (!oddsObj || typeof oddsObj !== "object") return {};
  /** @type {Record<string, object>} */
  const out = {};
  for (const [bk, pack] of Object.entries(oddsObj)) {
    const norm = normMatchupBookKey(bk);
    if (!MATCHUP_TRACKER_BOOK_SET.has(norm) || SKIP_BOOKS.has(norm)) continue;
    if (!pack || typeof pack !== "object") continue;
    out[norm] = pack;
  }
  return out;
}

/** @deprecated use filterMatchupTrackerOdds */
export function filterDraftKingsMatchupOdds(oddsObj) {
  const all = filterMatchupTrackerOdds(oddsObj);
  return all.draftkings || null;
}

/** @deprecated */
export function filterActionableMatchupOdds(oddsObj) {
  return filterMatchupTrackerOdds(oddsObj);
}

export function draftKingsMatchupDecimals(oddsObj, oddsFormat = "") {
  const pack = filterDraftKingsMatchupOdds(oddsObj);
  if (!pack) return { book: "", d1: NaN, d2: NaN };
  const { d1, d2 } = matchupOddsTwoWayFromPack(pack, oddsFormat);
  return { book: "draftkings", d1, d2 };
}

/** Best decimal for a side across allowed books (highest price). */
export function bestTrackerBookDecimalForSide(oddsObj, side, oddsFormat = "", threeWay = false) {
  const packs = filterMatchupTrackerOdds(oddsObj);
  let bestBook = "";
  let bestDec = NaN;
  for (const [book, pack] of Object.entries(packs)) {
    const decoded = threeWay
      ? matchupOddsThreeWayFromPack(pack, oddsFormat)
      : matchupOddsTwoWayFromPack(pack, oddsFormat);
    const d =
      side === "p1" ? decoded.d1 : side === "p2" ? decoded.d2 : side === "p3" ? decoded.d3 : NaN;
    if (!(d > 1)) continue;
    if (!Number.isFinite(bestDec) || d > bestDec) {
      bestDec = d;
      bestBook = book;
    }
  }
  return { book: bestBook, dec: bestDec };
}

export function modelEvPct(modelProb, decimalOdds) {
  const p = num(modelProb, NaN);
  const d = num(decimalOdds, NaN);
  if (!Number.isFinite(p) || !Number.isFinite(d) || d <= 1) return NaN;
  return (p * d - 1) * 100;
}

function pushMatchupSidePick(out, base) {
  if (!Number.isFinite(base.edgePct) || base.edgePct < base.minEdge) return;
  const impliedPct = Number.isFinite(base.dec) && base.dec > 1 ? (1 / base.dec) * 100 : NaN;
  const labels = base.labels || [];
  const { boost, tags } = base.signalBoost(base.marketLabel, labels, base.signals);
  const mFac = base.marketFactor(base.marketLabel, base.oos);
  out.push({
    player_name: base.player_name,
    opponent_name: base.opponent_name,
    dg_id: base.dg_id,
    opponent_dg_id: base.opponent_dg_id,
    round: base.round,
    market: base.marketLabel,
    pickType: base.pickType,
    side: base.side,
    mu: base.winProb,
    gap: base.gap,
    line: impliedPct,
    book: base.book,
    odds: decimalToAmerican(base.dec),
    edgePct: base.edgePct,
    score: base.edgePct * mFac * boost,
    histRoi: NaN,
    histBets: 0,
    contextTags: tags,
    tailoringTags: [],
  });
}

/**
 * Live round matchup picks — one row per allowed book×side that clears min EV
 * (keeps best side per match×book).
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
    const packs = filterMatchupTrackerOdds(m.odds || {});

    for (const [book, pack] of Object.entries(packs)) {
      const { d1, d2 } = matchupOddsTwoWayFromPack(pack, oddsFormat);
      if (!(d1 > 1) || !(d2 > 1)) continue;
      const edge1 = modelEvPct(p1, d1);
      const edge2 = modelEvPct(1 - p1, d2);
      const sides = [
        {
          side: "p1",
          player_name: String(m.p1_player_name || row1?.player_name || "").trim(),
          opponent_name: String(m.p2_player_name || row2?.player_name || "").trim(),
          dg_id: id1,
          opponent_dg_id: id2,
          winProb: p1,
          gap: mu1 - mu2,
          book,
          dec: d1,
          edgePct: edge1,
        },
        {
          side: "p2",
          player_name: String(m.p2_player_name || row2?.player_name || "").trim(),
          opponent_name: String(m.p1_player_name || row1?.player_name || "").trim(),
          dg_id: id2,
          opponent_dg_id: id1,
          winProb: 1 - p1,
          gap: mu2 - mu1,
          book,
          dec: d2,
          edgePct: edge2,
        },
      ];
      let best = null;
      for (const s of sides) {
        if (!Number.isFinite(s.edgePct) || s.edgePct < minEdge) continue;
        if (!best || s.edgePct > best.edgePct) best = s;
      }
      if (!best) continue;
      pushMatchupSidePick(out, {
        ...best,
        minEdge,
        round,
        marketLabel: ROUND_MATCHUP_MARKET,
        pickType: "matchup",
        labels: [`vs ${best.opponent_name}`, `Round matchups ${best.side}`, book],
        marketFactor,
        signalBoost,
        oos,
        signals,
      });
    }
  }

  return out;
}

/** Live 3-ball picks — DK / FanDuel / BetMGM only. */
export function buildThreeBallPicks({
  projections,
  players,
  round,
  minEvPct,
  marketFactor = () => 1,
  signalBoost = () => ({ boost: 1, tags: [] }),
  oos = null,
  signals = null,
}) {
  const list = projections?.matchups?.["3_balls"]?.match_list;
  if (!Array.isArray(list) || !list.length) return [];

  const oddsFormat = String(projections?.meta?.matchups_odds_format || "").trim();
  const minEdge = num(minEvPct, 0);
  /** @type {object[]} */
  const out = [];

  for (const m of list) {
    const id1 = Math.round(num(m.p1_dg_id, NaN));
    const id2 = Math.round(num(m.p2_dg_id, NaN));
    const id3 = Math.round(num(m.p3_dg_id, NaN));
    if (![id1, id2, id3].every(Number.isFinite)) continue;

    const row1 = players.get(id1);
    const row2 = players.get(id2);
    const row3 = players.get(id3);
    const mu1 = effectiveMuSgForMatchup(row1);
    const mu2 = effectiveMuSgForMatchup(row2);
    const mu3 = effectiveMuSgForMatchup(row3);
    if (![mu1, mu2, mu3].every(Number.isFinite)) continue;

    const [wp1, wp2, wp3] = threeBallModelProbs(mu1, mu2, mu3);
    const packs = filterMatchupTrackerOdds(m.odds || {});
    const names = [
      String(m.p1_player_name || row1?.player_name || "").trim(),
      String(m.p2_player_name || row2?.player_name || "").trim(),
      String(m.p3_player_name || row3?.player_name || "").trim(),
    ];

    for (const [book, pack] of Object.entries(packs)) {
      const { d1, d2, d3 } = matchupOddsThreeWayFromPack(pack, oddsFormat);
      if (!(d1 > 1) || !(d2 > 1) || !(d3 > 1)) continue;
      const sides = [
        { side: "p1", dg_id: id1, player_name: names[0], winProb: wp1, mu: mu1, dec: d1, edgePct: modelEvPct(wp1, d1) },
        { side: "p2", dg_id: id2, player_name: names[1], winProb: wp2, mu: mu2, dec: d2, edgePct: modelEvPct(wp2, d2) },
        { side: "p3", dg_id: id3, player_name: names[2], winProb: wp3, mu: mu3, dec: d3, edgePct: modelEvPct(wp3, d3) },
      ];
      let best = null;
      for (const s of sides) {
        if (!Number.isFinite(s.edgePct) || s.edgePct < minEdge) continue;
        if (!best || s.edgePct > best.edgePct) best = s;
      }
      if (!best) continue;
      const others = names.filter((n) => n && n !== best.player_name).join(" / ");
      pushMatchupSidePick(out, {
        side: best.side,
        player_name: best.player_name,
        opponent_name: others,
        dg_id: best.dg_id,
        opponent_dg_id: NaN,
        winProb: best.winProb,
        gap: best.mu - (mu1 + mu2 + mu3) / 3,
        book,
        dec: best.dec,
        edgePct: best.edgePct,
        minEdge,
        round,
        marketLabel: THREE_BALL_MARKET,
        pickType: "3ball",
        labels: [`3-ball vs ${others}`, book],
        marketFactor,
        signalBoost,
        oos,
        signals,
      });
    }
  }

  return out;
}

export { ROUND_MATCHUP_MARKET, THREE_BALL_MARKET, MATCHUP_KIND, THREE_BALL_KIND };
