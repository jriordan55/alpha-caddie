#!/usr/bin/env node
/**
 * Backtest historical book lines from data/odds.csv vs actual round stats.
 *
 * Metrics:
 *   - Flat 1u ROI / PnL at opening and closing American odds
 *   - Closing line value (CLV): bet_decimal / closing_decimal - 1
 *   - Beat true price: empirical hit rate minus average implied probability
 *   - Edge buckets: calibrated win% − opening implied (LOO by market × prob bucket)
 *
 *   npm run backtest:odds-csv
 *
 * Outputs:
 *   alpha-caddie-web/data/odds_backtest_detail.csv
 *   alpha-caddie-web/data/odds_backtest_summary.csv
 *   alpha-caddie-web/data/odds_backtest_edge_buckets.csv
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import {
  displayGolferName,
  golferNameMatchParts,
  golferNamesLikelySame,
  normNameLoose,
} from "./golfer-name-match.mjs";
import { birdiesPlusEaglesFromRow, impliedProbFromAmerican, ouSideResults } from "./round-projection-mu.mjs";
import { americanToDecimal, EV_THRESHOLDS_PCT } from "./round-projection-vs-actual-summary.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = resolve(WEB_ROOT, "..");
const ODDS_CSV = join(REPO_ROOT, "data", "odds.csv");
const HIST_CSV = join(REPO_ROOT, "data", "historical_rounds_all.csv");
const DETAIL_OUT = join(WEB_ROOT, "data", "odds_backtest_detail.csv");
const SUMMARY_OUT = join(WEB_ROOT, "data", "odds_backtest_summary.csv");
const EDGE_BUCKETS_OUT = join(WEB_ROOT, "data", "odds_backtest_edge_buckets.csv");

const OU_MARKETS = new Set([
  "GOLF:FT:CTBIR",
  "GOLF:FT:ROUNDNUMBIRDIES",
  "GOLF:FT:CTSTR",
  "GOLF:P:ROUND1OUSCORE",
]);
const MATCHUP_MARKETS = new Set(["GOLF:FT:2BL:AXB", "GOLF:P:ROUND1BESTDNBKBMI"]);

const MARKET_LABEL = {
  "GOLF:FT:CTBIR": "Birdies O/U",
  "GOLF:FT:ROUNDNUMBIRDIES": "Birdies or Better O/U",
  "GOLF:FT:CTSTR": "Round Score O/U",
  "GOLF:P:ROUND1OUSCORE": "Round 1 Score O/U",
  "GOLF:FT:2BL:AXB": "2-Ball Matchup",
  "GOLF:P:ROUND1BESTDNBKBMI": "Round 1 2-Ball",
};

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

function fmtNum(v, digits = 3) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10 ** digits) / 10 ** digits).toFixed(digits);
}

function fmtPct(v) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10) / 10).toFixed(1);
}

function formatAmericanOdds(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return "";
  return v > 0 ? `+${v}` : String(v);
}

/** DraftKings-style "G. Woodland" vs DataGolf "Woodland, Gary". */
function oddsPlayerMatchesHist(oddsLabel, histName) {
  if (golferNamesLikelySame(oddsLabel, histName)) return true;
  const s = String(oddsLabel || "").trim();
  const m = s.match(/^([A-Za-z])\.?\s+(.+)$/);
  if (!m) return false;
  const init = m[1].toLowerCase();
  const last = normNameLoose(m[2]);
  const ht = golferNameMatchParts(histName);
  if (!last || !ht.last || last !== ht.last) return false;
  if (ht.parts.length >= 2) return ht.parts[0].startsWith(init);
  return false;
}

function parseCompetition(comp, startUtc) {
  const raw = String(comp || "").trim();
  const ym = raw.match(/\s+(20\d{2})\s*$/);
  if (ym) {
    return {
      event: raw.replace(/\s+20\d{2}\s*$/, "").trim(),
      year: Number(ym[1]),
      competition: raw,
    };
  }
  const y = new Date(String(startUtc || "")).getUTCFullYear();
  return { event: raw, year: Number.isFinite(y) ? y : NaN, competition: raw };
}

function parseRoundFromText(...parts) {
  for (const p of parts) {
    const m = String(p || "").match(/Round\s*(\d)/i);
    if (m) return Number(m[1]);
  }
  return NaN;
}

function parseOuPlayer(marketName) {
  const n = String(marketName || "").trim();
  return n
    .replace(/\s*Total Birdies or Better.*$/i, "")
    .replace(/\s*Total Birdies.*$/i, "")
    .replace(/\s*Round Score.*$/i, "")
    .replace(/\s*-\s*Round\s*\d+\s*Score.*$/i, "")
    .replace(/\s*-\s*Round\s*\d+\s*Total Birdies.*$/i, "")
    .trim();
}

function parseSelection(sel) {
  const s = String(sel || "").trim();
  const m = s.match(/^(Over|Under)\s+([\d.]+)$/i);
  if (!m) return null;
  return { side: m[1].toLowerCase(), line: num(m[2]) };
}

function parseGroupPlayers(sportEvent, marketName, marketType) {
  if (marketType === "GOLF:P:ROUND1BESTDNBKBMI") {
    const m = String(marketName || "").match(/Round\s*\d+\s*-\s*(.+?)\s*-\s*2 Ball/i);
    if (m) return m[1].split("/").map((x) => x.trim()).filter(Boolean);
  }
  const se = String(sportEvent || "").replace(/\s*-\s*Round\s*\d+\s*$/i, "").trim();
  return se.split("/").map((x) => x.trim()).filter(Boolean);
}

function histBucketKey(year, eventName, round) {
  return `${year}|${foldComparableTitle(eventName)}|${round}`;
}

function eventMatchesHist(oddsEvent, year, histEventName, histYear) {
  if (Number(year) !== Number(histYear)) return false;
  return eventsLikelySame(oddsEvent, histEventName);
}

function buildHistIndex(rows) {
  const byKey = new Map();
  const byYear = new Map();

  for (const r of rows) {
    const year = num(r.year, NaN);
    const round = num(r.round_num, NaN);
    const event = String(r.event_name || "").trim();
    if (!Number.isFinite(year) || !Number.isFinite(round) || !event) continue;

    const key = histBucketKey(year, event, round);
    if (!byKey.has(key)) byKey.set(key, []);
    byKey.get(key).push({
      player_name: r.player_name,
      round_score: num(r.round_score, NaN),
      birdies: birdiesPlusEaglesFromRow(r),
      bogeys: num(r.bogeys ?? r.bogies, NaN),
      pars: num(r.pars, NaN),
      gir: num(r.gir, NaN),
      fairways: num(r.fairways, NaN),
      event_name: event,
      year,
      round,
    });

    if (!byYear.has(year)) byYear.set(year, []);
    byYear.get(year).push({ event, round, row: byKey.get(key)[byKey.get(key).length - 1] });
  }
  return { byKey, byYear };
}

function findHistBucket(byKey, byYear, oddsEvent, year, round) {
  const exact = byKey.get(histBucketKey(year, oddsEvent, round));
  if (exact?.length) return exact;

  const yearRows = byYear.get(year) || [];
  const seen = new Set();
  const out = [];
  for (const item of yearRows) {
    if (item.round !== round) continue;
    if (!eventMatchesHist(oddsEvent, year, item.event, year)) continue;
    const k = `${item.event}|${round}`;
    if (seen.has(k)) continue;
    seen.add(k);
    const bucket = byKey.get(histBucketKey(year, item.event, round));
    if (bucket?.length) out.push(...bucket);
  }
  return out;
}

function findPlayerInBucket(bucket, label) {
  if (!bucket?.length) return null;
  for (const h of bucket) {
    if (oddsPlayerMatchesHist(label, h.player_name)) return h;
  }
  return null;
}

function actualForMarket(marketType, histRow) {
  if (!histRow) return NaN;
  if (marketType === "GOLF:FT:CTBIR" || marketType === "GOLF:FT:ROUNDNUMBIRDIES") return histRow.birdies;
  if (marketType === "GOLF:FT:CTSTR" || marketType === "GOLF:P:ROUND1OUSCORE") return histRow.round_score;
  return NaN;
}

function settleOu(side, line, actual) {
  if (!Number.isFinite(actual) || !Number.isFinite(line) || !side) return "";
  if (actual === line) return "P";
  const sides = ouSideResults("Total score", actual, line);
  return side === "over" ? sides.over : sides.under;
}

function pnlForResult(result, american) {
  if (result === "P" || result === "") return 0;
  const dec = americanToDecimal(american);
  if (!Number.isFinite(dec)) return 0;
  return result === "W" ? dec - 1 : -1;
}

function clvPct(betAmerican, closeAmerican) {
  const betDec = americanToDecimal(betAmerican);
  const closeDec = americanToDecimal(closeAmerican);
  if (!Number.isFinite(betDec) || !Number.isFinite(closeDec) || betDec <= 1 || closeDec <= 1) return NaN;
  return (betDec / closeDec - 1) * 100;
}

/** 0.02 implied-prob buckets (same as build-results-backtest). */
function bucketProb(p) {
  if (!Number.isFinite(p)) return NaN;
  const cl = Math.max(0, Math.min(1, p));
  return Math.floor(cl * 50) / 50;
}

function calKey(market, bucket) {
  return `${market}|${bucket.toFixed(2)}`;
}

function buildCalibrationAgg(detail) {
  const wins = new Map();
  const totals = new Map();
  const marketWins = new Map();
  const marketTotals = new Map();

  for (const d of detail) {
    if (d.result !== "W" && d.result !== "L") continue;
    const imp = num(d.opening_implied, NaN);
    const b = bucketProb(imp);
    if (!Number.isFinite(b)) continue;
    const k = calKey(d.market_label, b);
    totals.set(k, (totals.get(k) || 0) + 1);
    if (d.result === "W") wins.set(k, (wins.get(k) || 0) + 1);

    const mk = d.market_label;
    marketTotals.set(mk, (marketTotals.get(mk) || 0) + 1);
    if (d.result === "W") marketWins.set(mk, (marketWins.get(mk) || 0) + 1);
  }
  return { wins, totals, marketWins, marketTotals };
}

/** Leave-one-out calibrated win prob for the posted side. */
function looCalibratedProb(d, agg) {
  const imp = num(d.opening_implied, NaN);
  const b = bucketProb(imp);
  if (!Number.isFinite(b)) return NaN;

  const k = calKey(d.market_label, b);
  const n = agg.totals.get(k) || 0;
  if (n > 1) {
    const w = agg.wins.get(k) || 0;
    const adjW = d.result === "W" ? w - 1 : w;
    return adjW / (n - 1);
  }

  const mn = agg.marketTotals.get(d.market_label) || 0;
  if (mn > 1) {
    const mw = agg.marketWins.get(d.market_label) || 0;
    const adjW = d.result === "W" ? mw - 1 : mw;
    return adjW / (mn - 1);
  }

  return imp;
}

function edgePctForBet(d, agg) {
  const imp = num(d.opening_implied, NaN);
  const modelP = looCalibratedProb(d, agg);
  if (!Number.isFinite(imp) || !Number.isFinite(modelP)) return NaN;
  return (modelP - imp) * 100;
}

function annotateEdge(detail) {
  const agg = buildCalibrationAgg(detail);
  for (const d of detail) {
    d.edge_pct = edgePctForBet(d, agg);
    d.calibrated_prob = Number.isFinite(d.edge_pct)
      ? num(d.opening_implied, NaN) + d.edge_pct / 100
      : NaN;
  }
  return agg;
}

function emptyAgg() {
  return {
    bets: 0,
    wins: 0,
    losses: 0,
    pushes: 0,
    units: 0,
    clvSum: 0,
    clvN: 0,
    clvPos: 0,
    impliedSum: 0,
    impliedN: 0,
  };
}

function addBet(agg, result, american, openingAmerican, closingAmerican) {
  if (result !== "W" && result !== "L" && result !== "P") return;
  agg.bets += 1;
  if (result === "W") agg.wins += 1;
  else if (result === "L") agg.losses += 1;
  else agg.pushes += 1;
  agg.units += pnlForResult(result, american);

  const imp = impliedProbFromAmerican(openingAmerican);
  if (Number.isFinite(imp) && (result === "W" || result === "L")) {
    agg.impliedSum += imp;
    agg.impliedN += 1;
  }

  const clv = clvPct(openingAmerican, closingAmerican);
  if (Number.isFinite(clv) && (result === "W" || result === "L")) {
    agg.clvSum += clv;
    agg.clvN += 1;
    if (clv > 0) agg.clvPos += 1;
  }
}

function settleMatchup(selection, players, bucket, incTie) {
  const scores = [];
  for (const pl of players) {
    const h = findPlayerInBucket(bucket, pl);
    if (!h || !Number.isFinite(h.round_score)) return { result: "", scores: [] };
    scores.push({ label: pl, name: displayGolferName(h.player_name), score: h.round_score });
  }
  const pick = findPlayerInBucket(bucket, selection);
  if (!pick || !Number.isFinite(pick.round_score)) return { result: "", scores };

  const minScore = Math.min(...scores.map((s) => s.score));
  const winners = scores.filter((s) => s.score === minScore);
  const pickScore = pick.round_score;

  if (pickScore > minScore) return { result: "L", scores };
  if (pickScore === minScore && winners.length > 1 && incTie) return { result: "P", scores };
  if (pickScore === minScore) return { result: "W", scores };
  return { result: "L", scores };
}

function processRows(oddsRows, histIndex) {
  const detail = [];
  const summaryOpen = new Map();
  const summaryClose = new Map();

  for (const row of oddsRows) {
    const marketType = String(row.MARKET_TYPE || "").trim();
    const marketLabel = MARKET_LABEL[marketType] || marketType;
    const isOu = OU_MARKETS.has(marketType);
    const isMatch = MATCHUP_MARKETS.has(marketType);
    if (!isOu && !isMatch) continue;

    const { event, year, competition } = parseCompetition(row.COMPETITION, row.EVENT_START_TIME_UTC);
    const round =
      parseRoundFromText(row.SPORT_EVENT, row.MARKET_NAME) ||
      (marketType === "GOLF:P:ROUND1OUSCORE" || marketType === "GOLF:P:ROUND1BESTDNBKBMI" ? 1 : NaN);
    if (!Number.isFinite(year) || !Number.isFinite(round)) continue;

    const bucket = findHistBucket(histIndex.byKey, histIndex.byYear, event, year, round);
    if (!bucket.length) continue;

    const openingAm = num(row.OPENING_AMERICAN_ODDS, NaN);
    const closingAm = num(row.CLOSING_AMERICAN_ODDS, NaN);
    const openingImp = num(row.OPENING_IMPLIED_PROB, impliedProbFromAmerican(openingAm));
    const closingImp = num(row.CLOSING_IMPLIED_PROB, impliedProbFromAmerican(closingAm));

    if (isOu) {
      const sel = parseSelection(row.SELECTION);
      if (!sel || !Number.isFinite(sel.line)) continue;
      const player = parseOuPlayer(row.MARKET_NAME);
      const hist = findPlayerInBucket(bucket, player);
      const actual = actualForMarket(marketType, hist);
      const result = settleOu(sel.side, sel.line, actual);
      if (!result) continue;

      const detailRow = {
        competition,
        event,
        year,
        sport_event: row.SPORT_EVENT,
        market_type: marketType,
        market_label: marketLabel,
        player,
        round,
        side: sel.side,
        line: sel.line,
        actual,
        result,
        opening_american: openingAm,
        closing_american: closingAm,
        opening_implied: openingImp,
        closing_implied: closingImp,
        pnl_open: pnlForResult(result, openingAm),
        pnl_close: pnlForResult(result, closingAm),
        clv_pct: clvPct(openingAm, closingAm),
        matched_player: hist ? displayGolferName(hist.player_name) : "",
      };
      detail.push(detailRow);

      for (const [map, priceKey, american] of [
        [summaryOpen, "opening", openingAm],
        [summaryClose, "closing", closingAm],
      ]) {
        const sk = `${marketLabel}|${priceKey}`;
        if (!map.has(sk)) map.set(sk, { market: marketLabel, price_source: priceKey, ...emptyAgg() });
        addBet(map.get(sk), result, american, openingAm, closingAm);
      }
      continue;
    }

    const players = parseGroupPlayers(row.SPORT_EVENT, row.MARKET_NAME, marketType);
    const selection = String(row.SELECTION || "").trim();
    if (!players.length || !selection) continue;
    const incTie = /inc tie/i.test(String(row.MARKET_NAME || ""));
    const { result, scores } = settleMatchup(selection, players, bucket, incTie);
    if (!result) continue;

    const detailRow = {
      competition,
      event,
      year,
      sport_event: row.SPORT_EVENT,
      market_type: marketType,
      market_label: marketLabel,
      player: selection,
      round,
      side: "pick",
      line: "",
      actual: scores.find((s) => oddsPlayerMatchesHist(selection, s.name) || oddsPlayerMatchesHist(selection, s.label))?.score ?? "",
      result,
      opening_american: openingAm,
      closing_american: closingAm,
      opening_implied: openingImp,
      closing_implied: closingImp,
      pnl_open: pnlForResult(result, openingAm),
      pnl_close: pnlForResult(result, closingAm),
      clv_pct: clvPct(openingAm, closingAm),
      matched_player: selection,
      group_scores: scores.map((s) => `${s.label}:${s.score}`).join("; "),
    };
    detail.push(detailRow);

    for (const [map, priceKey, american] of [
      [summaryOpen, "opening", openingAm],
      [summaryClose, "closing", closingAm],
    ]) {
      const sk = `${marketLabel}|${priceKey}`;
      if (!map.has(sk)) map.set(sk, { market: marketLabel, price_source: priceKey, ...emptyAgg() });
      addBet(map.get(sk), result, american, openingAm, closingAm);
    }
  }

  return { detail, summaryOpen, summaryClose };
}

function buildSummaryRows(summaryOpen, summaryClose, generatedAt) {
  const rows = [];
  const header =
    "section,generated_at,market,price_source,bets,wins,losses,pushes,units_net,roi_pct,hit_rate_pct,avg_implied_pct,beat_true_price_pct,avg_clv_pct,pct_positive_clv\n";

  const emit = (section, agg) => {
    const graded = agg.wins + agg.losses;
    const hitRate = graded ? (agg.wins / graded) * 100 : NaN;
    const avgImp = agg.impliedN ? (agg.impliedSum / agg.impliedN) * 100 : NaN;
    const beatTrue = Number.isFinite(hitRate) && Number.isFinite(avgImp) ? hitRate - avgImp : NaN;
    const roi = agg.bets > 0 ? (agg.units / agg.bets) * 100 : NaN;
    const avgClv = agg.clvN ? agg.clvSum / agg.clvN : NaN;
    const pctPosClv = agg.clvN ? (agg.clvPos / agg.clvN) * 100 : NaN;
    rows.push(
      [
        section,
        generatedAt,
        agg.market,
        agg.price_source,
        agg.bets,
        agg.wins,
        agg.losses,
        agg.pushes,
        fmtNum(agg.units, 2),
        fmtPct(roi),
        fmtPct(hitRate),
        fmtPct(avgImp),
        fmtPct(beatTrue),
        fmtPct(avgClv),
        fmtPct(pctPosClv),
      ]
        .map(csvCell)
        .join(","),
    );
  };

  const allOpen = emptyAgg();
  const allClose = emptyAgg();
  allOpen.market = "__all__";
  allOpen.price_source = "opening";
  allClose.market = "__all__";
  allClose.price_source = "closing";

  for (const agg of summaryOpen.values()) {
    emit("flat_roi", agg);
    allOpen.bets += agg.bets;
    allOpen.wins += agg.wins;
    allOpen.losses += agg.losses;
    allOpen.pushes += agg.pushes;
    allOpen.units += agg.units;
    allOpen.clvSum += agg.clvSum;
    allOpen.clvN += agg.clvN;
    allOpen.clvPos += agg.clvPos;
    allOpen.impliedSum += agg.impliedSum;
    allOpen.impliedN += agg.impliedN;
  }
  for (const agg of summaryClose.values()) {
    emit("flat_roi", agg);
    allClose.bets += agg.bets;
    allClose.wins += agg.wins;
    allClose.losses += agg.losses;
    allClose.pushes += agg.pushes;
    allClose.units += agg.units;
    allClose.clvSum += agg.clvSum;
    allClose.clvN += agg.clvN;
    allClose.clvPos += agg.clvPos;
    allClose.impliedSum += agg.impliedSum;
    allClose.impliedN += agg.impliedN;
  }

  emit("flat_roi_all", allOpen);
  emit("flat_roi_all", allClose);

  return { header, rows };
}

function emptyEdgeBucketAcc(market, side, threshold) {
  return {
    market,
    side,
    threshold,
    bets: 0,
    wins: 0,
    losses: 0,
    pushes: 0,
    units: 0,
    edgeSum: 0,
    edgeN: 0,
    impliedSum: 0,
    impliedN: 0,
    clvSum: 0,
    clvN: 0,
    openAmSum: 0,
    openAmN: 0,
    closeAmSum: 0,
    closeAmN: 0,
  };
}

function addEdgeBucketBet(acc, d) {
  acc.bets += 1;
  if (d.result === "W") acc.wins += 1;
  else if (d.result === "L") acc.losses += 1;
  else acc.pushes += 1;
  acc.units += d.pnl_open || 0;
  if (Number.isFinite(d.edge_pct)) {
    acc.edgeSum += d.edge_pct;
    acc.edgeN += 1;
  }
  if (Number.isFinite(d.opening_implied)) {
    acc.impliedSum += d.opening_implied;
    acc.impliedN += 1;
  }
  if (Number.isFinite(d.clv_pct) && (d.result === "W" || d.result === "L")) {
    acc.clvSum += d.clv_pct;
    acc.clvN += 1;
  }
  if (Number.isFinite(d.opening_american)) {
    acc.openAmSum += d.opening_american;
    acc.openAmN += 1;
  }
  if (Number.isFinite(d.closing_american)) {
    acc.closeAmSum += d.closing_american;
    acc.closeAmN += 1;
  }
}

function buildEdgeBucketRows(detail, generatedAt) {
  const header =
    "section,generated_at,market,side,edge_threshold_pct,avg_opening_american,avg_closing_american,bets,wins,losses,pushes,units_net,roi_pct,hit_rate_pct,avg_edge_pct,avg_implied_pct,avg_clv_pct\n";
  const rows = [];
  const byKey = new Map();

  for (const d of detail) {
    if (d.result !== "W" && d.result !== "L" && d.result !== "P") continue;
    if (!Number.isFinite(d.edge_pct)) continue;

    const side = String(d.side || "pick").toLowerCase();

    for (const th of EV_THRESHOLDS_PCT) {
      if (d.edge_pct < th) continue;
      for (const [market, aggSide] of [
        [d.market_label, side],
        [d.market_label, "__all__"],
        ["__all__", side],
        ["__all__", "__all__"],
      ]) {
        const k = `${market}\x1f${aggSide}\x1f${th}`;
        let acc = byKey.get(k);
        if (!acc) {
          acc = emptyEdgeBucketAcc(market, aggSide, th);
          byKey.set(k, acc);
        }
        addEdgeBucketBet(acc, d);
      }
    }
  }

  const sorted = [...byKey.values()].sort(
    (a, b) =>
      a.market.localeCompare(b.market) ||
      a.side.localeCompare(b.side) ||
      a.threshold - b.threshold,
  );

  for (const acc of sorted) {
    const graded = acc.wins + acc.losses;
    const hitRate = graded ? (acc.wins / graded) * 100 : NaN;
    const roi = acc.bets > 0 ? (acc.units / acc.bets) * 100 : NaN;
    const avgEdge = acc.edgeN ? acc.edgeSum / acc.edgeN : NaN;
    const avgImp = acc.impliedN ? (acc.impliedSum / acc.impliedN) * 100 : NaN;
    const avgClv = acc.clvN ? acc.clvSum / acc.clvN : NaN;
    const avgOpenAm = acc.openAmN ? acc.openAmSum / acc.openAmN : NaN;
    const avgCloseAm = acc.closeAmN ? acc.closeAmSum / acc.closeAmN : NaN;
    rows.push(
      [
        "edge_bucket",
        generatedAt,
        acc.market,
        acc.side,
        acc.threshold,
        formatAmericanOdds(avgOpenAm),
        formatAmericanOdds(avgCloseAm),
        acc.bets,
        acc.wins,
        acc.losses,
        acc.pushes,
        fmtNum(acc.units, 2),
        fmtPct(roi),
        fmtPct(hitRate),
        fmtPct(avgEdge),
        fmtPct(avgImp),
        fmtPct(avgClv),
      ]
        .map(csvCell)
        .join(","),
    );
  }

  return header + rows.join("\n") + (rows.length ? "\n" : "");
}

function writeDetailCsv(detail, generatedAt) {
  const header =
    "generated_at,competition,event,year,sport_event,market_type,market_label,player,matched_player,round,side,line,actual,result,opening_american,closing_american,opening_implied,calibrated_prob,edge_pct,closing_implied,pnl_open,pnl_close,clv_pct,group_scores\n";
  const lines = detail.map((r) =>
    [
      generatedAt,
      r.competition,
      r.event,
      r.year,
      r.sport_event,
      r.market_type,
      r.market_label,
      r.player,
      r.matched_player,
      r.round,
      r.side,
      r.line,
      r.actual,
      r.result,
      r.opening_american,
      r.closing_american,
      fmtNum(r.opening_implied, 6),
      fmtNum(r.calibrated_prob, 6),
      fmtNum(r.edge_pct, 2),
      fmtNum(r.closing_implied, 6),
      fmtNum(r.pnl_open, 3),
      fmtNum(r.pnl_close, 3),
      fmtNum(r.clv_pct, 2),
      r.group_scores || "",
    ]
      .map(csvCell)
      .join(","),
  );
  return header + lines.join("\n") + (lines.length ? "\n" : "");
}

async function main() {
  if (!existsSync(ODDS_CSV)) throw new Error(`Missing ${ODDS_CSV}`);
  if (!existsSync(HIST_CSV)) throw new Error(`Missing ${HIST_CSV}`);

  const oddsRows = parse(readFileSync(ODDS_CSV, "utf8"), { columns: true, skip_empty_lines: true });
  const histRows = parse(readFileSync(HIST_CSV, "utf8"), { columns: true, skip_empty_lines: true });
  const histIndex = buildHistIndex(histRows);
  const generatedAt = new Date().toISOString();

  const { detail, summaryOpen, summaryClose } = processRows(oddsRows, histIndex);
  annotateEdge(detail);

  mkdirSync(dirname(DETAIL_OUT), { recursive: true });
  writeFileSync(DETAIL_OUT, writeDetailCsv(detail, generatedAt));

  const flatSummary = buildSummaryRows(summaryOpen, summaryClose, generatedAt);
  const edgeSummary = buildEdgeBucketRows(detail, generatedAt);
  writeFileSync(SUMMARY_OUT, flatSummary.header + flatSummary.rows.join("\n") + (flatSummary.rows.length ? "\n" : ""));
  writeFileSync(EDGE_BUCKETS_OUT, edgeSummary);

  const graded = detail.filter((d) => d.result === "W" || d.result === "L");
  const openUnits = detail.reduce((s, d) => s + (d.pnl_open || 0), 0);
  const closeUnits = detail.reduce((s, d) => s + (d.pnl_close || 0), 0);
  const avgClv =
    graded.length > 0
      ? graded.reduce((s, d) => s + (Number.isFinite(d.clv_pct) ? d.clv_pct : 0), 0) / graded.length
      : NaN;
  const hitRate = graded.length ? (graded.filter((d) => d.result === "W").length / graded.length) * 100 : NaN;
  const avgOpenImp =
    graded.length > 0
      ? (graded.reduce((s, d) => s + (Number.isFinite(d.opening_implied) ? d.opening_implied : 0), 0) / graded.length) *
        100
      : NaN;

  console.log(`Wrote ${DETAIL_OUT} (${detail.length} rows)`);
  console.log(`Wrote ${SUMMARY_OUT}`);
  console.log(`Wrote ${EDGE_BUCKETS_OUT}`);
  console.log("");
  console.log("Backtest summary (flat 1u, bet at opening odds):");
  console.log(`  Settled bets: ${graded.length} (${detail.length - graded.length} pushes/unmatched omitted from hit rate)`);
  console.log(`  Hit rate: ${fmtPct(hitRate)}%`);
  console.log(`  Avg opening implied: ${fmtPct(avgOpenImp)}%`);
  console.log(`  Beat true price (hit - implied): ${fmtPct(hitRate - avgOpenImp)}%`);
  console.log(`  PnL @ opening: ${fmtNum(openUnits, 2)}u`);
  console.log(`  PnL @ closing: ${fmtNum(closeUnits, 2)}u`);
  console.log(`  ROI @ opening: ${fmtPct(graded.length ? (openUnits / graded.length) * 100 : NaN)}%`);
  console.log(`  Avg CLV vs close: ${fmtPct(avgClv)}%`);
  console.log("");
  console.log("Edge buckets (calibrated prob − opening implied, LOO; flat 1u @ opening):");
  for (const th of EV_THRESHOLDS_PCT) {
    const bucket = detail.filter(
      (d) => Number.isFinite(d.edge_pct) && d.edge_pct >= th && (d.result === "W" || d.result === "L" || d.result === "P"),
    );
    const units = bucket.reduce((s, d) => s + (d.pnl_open || 0), 0);
    const wins = bucket.filter((d) => d.result === "W").length;
    const losses = bucket.filter((d) => d.result === "L").length;
    const gradedN = wins + losses;
    const roi = bucket.length ? (units / bucket.length) * 100 : NaN;
    const hit = gradedN ? (wins / gradedN) * 100 : NaN;
    const avgOpen = bucket.length
      ? formatAmericanOdds(bucket.reduce((s, d) => s + num(d.opening_american, 0), 0) / bucket.length)
      : "";
    const avgClose = bucket.length
      ? formatAmericanOdds(bucket.reduce((s, d) => s + num(d.closing_american, 0), 0) / bucket.length)
      : "";
    console.log(
      `  ≥${th}% __all__: ${bucket.length} bets, ${fmtPct(hit)}% hit, open ${avgOpen} / close ${avgClose}, ${fmtNum(units, 1)}u, ${fmtPct(roi)}% ROI`,
    );
  }
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
