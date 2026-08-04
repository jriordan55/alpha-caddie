#!/usr/bin/env node
/**
 * Walk-forward round matchup + 3-ball backtest from DataGolf historical odds.
 * Books: DraftKings, FanDuel, BetMGM only.
 *
 *   npm run export:matchup-backtest
 *
 * Outputs (matchup tracker):
 *   alpha-caddie-web/data/matchup_backtest_detail.csv
 *   alpha-caddie-web/data/matchup_backtest_summary.csv
 */
import { createReadStream, existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { createInterface } from "readline";
import { parse } from "csv-parse/sync";
import { foldComparableTitle } from "./dg-events-align.mjs";
import {
  attachFieldDgIdsToProps,
  FullModelProjectionCache,
} from "./historical-walkforward-projections.mjs";
import { flatVenueProjectionPipelineEnv } from "./projection-pipeline-env.mjs";
import { EV_THRESHOLDS_PCT } from "./round-projection-vs-actual-summary.mjs";
import {
  matchupWinProb,
  modelEvPct,
  threeBallModelProbs,
  isAllowedMatchupTrackerBook,
  ROUND_MATCHUP_MARKET,
  THREE_BALL_MARKET,
} from "../projection-tracker/matchup-math.mjs";

Object.assign(process.env, flatVenueProjectionPipelineEnv());

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const MATCHUPS_CSV = join(REPO_ROOT, "data", "historical_matchups_outcomes.csv");
const HIST_CSV = join(REPO_ROOT, "data", "historical_rounds_all.csv");
const DETAIL_OUT = join(WEB_ROOT, "data", "matchup_backtest_detail.csv");
const SUMMARY_OUT = join(WEB_ROOT, "data", "matchup_backtest_summary.csv");

const MAX_ROWS = Math.max(0, Math.round(Number(process.env.GOLF_MATCHUP_BACKTEST_MAX_ROWS || "0")));
const SINCE_ISO = String(process.env.GOLF_MATCHUP_BACKTEST_SINCE || "").trim();
/** Comma list: round,3ball (default both). */
const MARKET_FILTER = new Set(
  String(process.env.GOLF_MATCHUP_MARKETS || "round,3ball")
    .toLowerCase()
    .split(/[,;\s]+/)
    .map((s) => s.trim())
    .filter(Boolean),
);
const WANT_ROUND = MARKET_FILTER.has("round") || MARKET_FILTER.has("round_matchups") || MARKET_FILTER.has("matchup");
const WANT_THREE = MARKET_FILTER.has("3ball") || MARKET_FILTER.has("3balls") || MARKET_FILTER.has("3-balls") || MARKET_FILTER.has("three");


const DETAIL_HEADER_COLS = [
  "exported_at",
  "event_name",
  "year",
  "round",
  "book",
  "bet_type",
  "market",
  "open_time",
  "close_time",
  "dg_id",
  "player_name",
  "opponent_dg_id",
  "opponent_name",
  "opponent2_dg_id",
  "opponent2_name",
  "model_mu_sg",
  "opp_mu_sg",
  "opp2_mu_sg",
  "sg_gap",
  "model_win_pct",
  "open_implied_pct",
  "close_implied_pct",
  "p1_open_dec",
  "p2_open_dec",
  "p3_open_dec",
  "p1_close_dec",
  "p2_close_dec",
  "p3_close_dec",
  "edge_p1_open_pct",
  "edge_p2_open_pct",
  "edge_p3_open_pct",
  "edge_p1_pct",
  "edge_p2_pct",
  "edge_p3_pct",
  "p1_result",
  "p2_result",
  "p3_result",
  "pick_side_at_10",
  "book_odds_source",
];
const DETAIL_HEADER = DETAIL_HEADER_COLS.join(",");

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

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

function betTimeMs(raw) {
  const s = String(raw || "").trim();
  const t = Date.parse(s.replace(" ", "T"));
  return Number.isFinite(t) ? t : 0;
}

function parseRoundFromBetType(bt) {
  const m = String(bt || "").match(/^R([1-4])\b/i);
  return m ? Number(m[1]) : NaN;
}

function isRoundMatchupBetType(bt) {
  const t = String(bt || "");
  return /^R[1-4]\s+Match/i.test(t) && !/3[\s-]*ball/i.test(t);
}

function isThreeBallBetType(bt) {
  return /3[\s-]*ball/i.test(String(bt || ""));
}

function marketLabelForBetType(bt) {
  if (isThreeBallBetType(bt)) return THREE_BALL_MARKET;
  if (isRoundMatchupBetType(bt)) return ROUND_MATCHUP_MARKET;
  return "";
}

function outcomeToResult(o) {
  if (o === 1) return "W";
  if (o === 0) return "L";
  if (o === 0.5) return "P";
  return "";
}

function isGradedOutcome(o) {
  return o === 0 || o === 1 || o === 0.5;
}

function pickMatchupSide(edges, minEv) {
  const th = num(minEv, 0);
  /** @type {{ side: string, edge: number } | null} */
  let best = null;
  for (const [side, edge] of Object.entries(edges)) {
    if (!Number.isFinite(edge) || edge < th) continue;
    if (!best || edge > best.edge) best = { side, edge };
  }
  return best;
}

function emptyEvAcc() {
  return { bets: 0, wins: 0, losses: 0, pushes: 0, units: 0 };
}

function addEvBet(acc, result, dec) {
  acc.bets += 1;
  if (result === "W") {
    acc.wins += 1;
    acc.units += Number.isFinite(dec) && dec > 1 ? dec - 1 : 0;
  } else if (result === "L") {
    acc.losses += 1;
    acc.units -= 1;
  } else if (result === "P") {
    acc.pushes += 1;
  } else {
    acc.pushes += 1;
  }
}

function fmtNum(v, d = 2) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10 ** d) / 10 ** d).toFixed(d);
}

function closeDateIso(row) {
  const s = String(row.close_time || row.open_time || "").trim();
  const m = s.match(/^(\d{4}-\d{2}-\d{2})/);
  return m ? m[1] : "";
}

function sampleFromDetailFields(row) {
  return {
    event_name: String(row.event_name || "").trim(),
    market: String(row.market || "").trim() || ROUND_MATCHUP_MARKET,
    model_win_pct: num(row.model_win_pct, NaN),
    close_implied_pct: num(row.close_implied_pct, NaN),
    edge_p1_pct: num(row.edge_p1_pct, NaN),
    edge_p2_pct: num(row.edge_p2_pct, NaN),
    edge_p3_pct: num(row.edge_p3_pct, NaN),
    p1_result: row.p1_result,
    p2_result: row.p2_result,
    p3_result: row.p3_result,
    p1_close_dec: num(row.p1_close_dec, NaN),
    p2_close_dec: num(row.p2_close_dec, NaN),
    p3_close_dec: num(row.p3_close_dec, NaN),
  };
}

function buildSummary(samples, exportedAt) {
  const evAcc = new Map();
  const lineByMarket = new Map();
  const byEventMarket = new Map();

  for (const s of samples) {
    const market = s.market || ROUND_MATCHUP_MARKET;
    if (Number.isFinite(s.model_win_pct) && Number.isFinite(s.close_implied_pct)) {
      const diff = s.model_win_pct - s.close_implied_pct;
      let la = lineByMarket.get(market);
      if (!la) {
        la = { sq: 0, abs: 0, n: 0 };
        lineByMarket.set(market, la);
      }
      la.sq += diff * diff;
      la.abs += Math.abs(diff);
      la.n += 1;

      const ek = `${s.event_name}\x1f${market}`;
      let ea = byEventMarket.get(ek);
      if (!ea) {
        ea = { event_name: s.event_name, market, sq: 0, abs: 0, n: 0 };
        byEventMarket.set(ek, ea);
      }
      ea.sq += diff * diff;
      ea.abs += Math.abs(diff);
      ea.n += 1;
    }

    for (const th of EV_THRESHOLDS_PCT) {
      const pick = pickMatchupSide(
        { p1: s.edge_p1_pct, p2: s.edge_p2_pct, p3: s.edge_p3_pct },
        th,
      );
      if (!pick) continue;
      const side = pick.side;
      const result = side === "p1" ? s.p1_result : side === "p2" ? s.p2_result : s.p3_result;
      const dec = side === "p1" ? s.p1_close_dec : side === "p2" ? s.p2_close_dec : s.p3_close_dec;
      if (result !== "W" && result !== "L" && result !== "P") continue;

      const gk = `${s.event_name}\x1f${market}\x1f${th}\x1f${side}`;
      let acc = evAcc.get(gk);
      if (!acc) {
        acc = { event_name: s.event_name, market, threshold: th, side, ...emptyEvAcc() };
        evAcc.set(gk, acc);
      }
      addEvBet(acc, result, dec);

      const mk = `${market}\x1f${th}\x1f${side}`;
      let macc = evAcc.get(`__market__\x1f${mk}`);
      if (!macc) {
        macc = { event_name: "", market, threshold: th, side, ...emptyEvAcc() };
        evAcc.set(`__market__\x1f${mk}`, macc);
      }
      addEvBet(macc, result, dec);

      const ek = `${s.event_name}\x1f${market}\x1f${th}`;
      let eacc = evAcc.get(`__pick__\x1f${ek}`);
      if (!eacc) {
        eacc = { event_name: s.event_name, market, threshold: th, side: "pick", ...emptyEvAcc() };
        evAcc.set(`__pick__\x1f${ek}`, eacc);
      }
      addEvBet(eacc, result, dec);
    }
  }

  const header =
    "section,exported_at,projections_updated_at,event_name,course_used,display_round,pricing_mode,pricing_skill,market,rmse,mae,n_line_pairs,ev_threshold_pct,bet_side,bets,wins,losses,pushes,units_net,roi_pct\n";
  const rows = [];

  const pushRow = (section, eventName, course, round, market, rmse, mae, nPairs, th, side, acc) => {
    const roi = acc.bets > 0 ? (acc.units / acc.bets) * 100 : NaN;
    rows.push(
      [
        section,
        exportedAt,
        "",
        eventName,
        course,
        round,
        "default",
        "default",
        market,
        rmse != null ? fmtNum(rmse, 3) : "",
        mae != null ? fmtNum(mae, 3) : "",
        nPairs != null ? nPairs : "",
        th != null ? fmtNum(th, 1) : "",
        side || "",
        acc.bets || "",
        acc.wins || "",
        acc.losses || "",
        acc.pushes || "",
        acc.bets ? fmtNum(acc.units, 2) : "",
        acc.bets ? fmtNum(roi, 1) : "",
      ]
        .map(csvCell)
        .join(","),
    );
  };

  for (const [market, la] of lineByMarket.entries()) {
    if (!la.n) continue;
    const rmse = Math.sqrt(la.sq / la.n);
    const mae = la.abs / la.n;
    pushRow("model_vs_book", "(all events)", "", "", market, rmse, mae, la.n, "", "", emptyEvAcc());
    pushRow("model_vs_book_by_market", "(all events)", "", "(all)", market, rmse, mae, la.n, "", "", emptyEvAcc());
  }

  for (const ea of byEventMarket.values()) {
    if (!ea.n) continue;
    pushRow(
      "model_vs_book",
      ea.event_name,
      "",
      "",
      ea.market,
      Math.sqrt(ea.sq / ea.n),
      ea.abs / ea.n,
      ea.n,
      "",
      "",
      emptyEvAcc(),
    );
  }

  for (const acc of evAcc.values()) {
    if (String(acc.side) === "pick") {
      pushRow(
        "ev_backtest_by_market",
        acc.event_name,
        "",
        "(all)",
        acc.market,
        "",
        "",
        "",
        acc.threshold,
        "pick",
        acc,
      );
    } else if (acc.event_name) {
      pushRow("ev_backtest", acc.event_name, "", "", acc.market, "", "", "", acc.threshold, acc.side, acc);
    } else {
      pushRow(
        "ev_backtest_by_market",
        "(all events)",
        "",
        "(all)",
        acc.market,
        "",
        "",
        "",
        acc.threshold,
        acc.side,
        acc,
      );
    }
  }

  return header + rows.join("\n") + (rows.length ? "\n" : "");
}

function rowToDetailLine(rowObj) {
  return DETAIL_HEADER_COLS.map((k) => csvCell(rowObj[k] ?? "")).join(",");
}

function loadExistingDetailForMerge(replaceEvents) {
  if (!SINCE_ISO || !existsSync(DETAIL_OUT)) return { lines: [], samples: [] };
  const text = readFileSync(DETAIL_OUT, "utf8").trim();
  const lines = text.split(/\r?\n/).filter(Boolean);
  if (lines.length < 2) return { lines: [], samples: [] };
  const header = parseCsvLine(lines[0]);
  /** @type {string[]} */
  const kept = [];
  /** @type {object[]} */
  const samples = [];
  let remapped = 0;
  for (let i = 1; i < lines.length; i++) {
    const cols = parseCsvLine(lines[i]);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cols[j] ?? "";
    if (replaceEvents.has(String(row.event_name || "").trim())) continue;
    if (!isAllowedMatchupTrackerBook(row.book)) continue;
    const market = String(row.market || "").trim();
    if (market !== ROUND_MATCHUP_MARKET && market !== THREE_BALL_MARKET) continue;
    // Remap by column name so adding close_time (etc.) does not drop prior history.
    const normalized = {};
    for (const k of DETAIL_HEADER_COLS) normalized[k] = row[k] ?? "";
    if (!normalized.close_time) remapped += 1;
    kept.push(rowToDetailLine(normalized));
    samples.push(sampleFromDetailFields(normalized));
  }
  if (header.join(",") !== DETAIL_HEADER && kept.length) {
    console.log(
      `[matchup-backtest] Remapped ${kept.length.toLocaleString()} prior detail row(s) to current schema` +
        (remapped ? ` (${remapped.toLocaleString()} missing close_time)` : ""),
    );
  }
  return { lines: kept, samples };
}

async function readMatchupRows() {
  /** @type {object[]} */
  const roundRows = [];
  /** @type {object[]} */
  const threeRows = [];
  const rl = createInterface({
    input: createReadStream(MATCHUPS_CSV, { encoding: "utf8" }),
    crlfDelay: Infinity,
  });
  let header = null;
  for await (const line of rl) {
    if (!line.trim()) continue;
    const cols = parseCsvLine(line);
    if (!header) {
      header = cols;
      continue;
    }
    const row = {};
    for (let i = 0; i < header.length; i++) row[header[i]] = cols[i] ?? "";
    const market = marketLabelForBetType(row.bet_type);
    if (!market) continue;
    if (market === ROUND_MATCHUP_MARKET && !WANT_ROUND) continue;
    if (market === THREE_BALL_MARKET && !WANT_THREE) continue;
    if (!isAllowedMatchupTrackerBook(row.book)) continue;

    const o1 = num(row.p1_outcome, NaN);
    const o2 = num(row.p2_outcome, NaN);
    if (!isGradedOutcome(o1) || !isGradedOutcome(o2)) continue;
    if (market === THREE_BALL_MARKET) {
      const o3 = num(row.p3_outcome, NaN);
      if (!isGradedOutcome(o3)) continue;
    }
    if (SINCE_ISO) {
      const d = closeDateIso(row);
      if (!d || d < SINCE_ISO) continue;
    }
    if (market === THREE_BALL_MARKET) threeRows.push(row);
    else roundRows.push(row);
  }

  // Round matchups first so capped / interrupted runs still have H2H history.
  let rows = [...roundRows, ...threeRows];
  if (MAX_ROWS > 0 && rows.length > MAX_ROWS) {
    const roundTake = Math.min(roundRows.length, Math.ceil(MAX_ROWS * 0.6));
    const threeTake = Math.min(threeRows.length, MAX_ROWS - roundTake);
    rows = [...roundRows.slice(0, roundTake), ...threeRows.slice(0, threeTake)];
  }
  return { rows, nRound: roundRows.length, nThree: threeRows.length };
}

async function main() {
  if (!existsSync(MATCHUPS_CSV)) {
    if (existsSync(DETAIL_OUT)) {
      console.warn(
        `Missing ${MATCHUPS_CSV} — keeping existing matchup_backtest_detail.csv (run npm run update:odds to refresh).`,
      );
      process.exit(0);
    }
    console.error(`Missing ${MATCHUPS_CSV} — run npm run update:odds`);
    process.exit(1);
  }
  if (!existsSync(HIST_CSV)) {
    console.error(`Missing ${HIST_CSV}`);
    process.exit(1);
  }

  console.log(
    `Reading historical_matchups_outcomes.csv (DK/FD/BetMGM; markets=${[WANT_ROUND ? "round" : null, WANT_THREE ? "3ball" : null].filter(Boolean).join("+")}) …`,
  );
  if (SINCE_ISO) console.log(`  Since filter: close_time >= ${SINCE_ISO}`);
  const { rows: matchupRows, nRound, nThree } = await readMatchupRows();
  console.log(
    `  ${matchupRows.length.toLocaleString()} graded rows to price (${nRound.toLocaleString()} round available / ${nThree.toLocaleString()} 3-ball available)`,
  );

  const replaceEvents = new Set(matchupRows.map((r) => String(r.event_name || "").trim()).filter(Boolean));
  const existing = loadExistingDetailForMerge(replaceEvents);
  if (SINCE_ISO && existing.lines.length) {
    console.log(`  Merging: keeping ${existing.lines.length.toLocaleString()} prior detail rows`);
  }

  /** @type {Map<string, object>} */
  const bundles = new Map();
  for (const row of matchupRows) {
    const round = parseRoundFromBetType(row.bet_type);
    const year = Math.round(num(row.year, NaN));
    const event = String(row.event_name || "").trim();
    if (!event || !Number.isFinite(round) || !Number.isFinite(year)) continue;
    const key = `${year}|${foldComparableTitle(event)}|${round}`;
    if (!bundles.has(key)) {
      bundles.set(key, {
        year,
        event,
        round,
        bet_time_ms: betTimeMs(row.close_time || row.open_time),
        dg_id: Math.round(num(row.p1_dg_id, NaN)),
      });
    }
  }

  console.log(`  ${bundles.size} unique event×round bundles for walk-forward μ_sg`);
  const histRows = parse(readFileSync(HIST_CSV, "utf8"), {
    columns: true,
    skip_empty_lines: true,
    relax_column_count: true,
  });
  attachFieldDgIdsToProps(bundles, histRows);
  const cache = new FullModelProjectionCache(REPO_ROOT, histRows);
  await cache.prewarm(bundles);

  const exportedAt = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  /** @type {object[]} */
  const samples = [...existing.samples];
  /** @type {string[]} */
  const detailLines = [DETAIL_HEADER, ...existing.lines];

  let skippedNoModel = 0;
  let i = 0;
  for (const row of matchupRows) {
    i += 1;
    if (i % 5000 === 0 || i === matchupRows.length) {
      process.stdout.write(`\r  Model pricing ${i}/${matchupRows.length}`);
    }

    const market = marketLabelForBetType(row.bet_type);
    const round = parseRoundFromBetType(row.bet_type);
    const year = Math.round(num(row.year, NaN));
    const event = String(row.event_name || "").trim();
    const book = String(row.book || "").trim().toLowerCase();
    const id1 = Math.round(num(row.p1_dg_id, NaN));
    const id2 = Math.round(num(row.p2_dg_id, NaN));
    const id3 = Math.round(num(row.p3_dg_id, NaN));
    const d1 = num(row.p1_close, NaN);
    const d2 = num(row.p2_close, NaN);
    const d3 = num(row.p3_close, NaN);
    const oDec1 = num(row.p1_open, NaN);
    const oDec2 = num(row.p2_open, NaN);
    const oDec3 = num(row.p3_open, NaN);
    if (!Number.isFinite(id1) || !Number.isFinite(id2) || !(d1 > 1) || !(d2 > 1)) continue;

    const prop = {
      year,
      event,
      round,
      bet_time_ms: betTimeMs(row.close_time || row.open_time),
      dg_id: id1,
    };

    const mu1 = await cache.muSgForProp(prop, id1);
    const mu2 = await cache.muSgForProp(prop, id2);
    if (!Number.isFinite(mu1) || !Number.isFinite(mu2)) {
      skippedNoModel += 1;
      continue;
    }

    let mu3 = NaN;
    let wp1;
    let wp2;
    let wp3 = NaN;
    let edge1;
    let edge2;
    let edge3 = NaN;
    let o3 = "";
    let opp2Id = "";
    let opp2Name = "";

    if (market === THREE_BALL_MARKET) {
      if (!Number.isFinite(id3) || !(d3 > 1)) continue;
      mu3 = await cache.muSgForProp(prop, id3);
      if (!Number.isFinite(mu3)) {
        skippedNoModel += 1;
        continue;
      }
      [wp1, wp2, wp3] = threeBallModelProbs(mu1, mu2, mu3);
      edge1 = modelEvPct(wp1, d1);
      edge2 = modelEvPct(wp2, d2);
      edge3 = modelEvPct(wp3, d3);
      o3 = outcomeToResult(num(row.p3_outcome, NaN));
      opp2Id = id3;
      opp2Name = row.p3_player_name;
    } else {
      wp1 = matchupWinProb(mu1, mu2, "round_matchups");
      wp2 = 1 - wp1;
      edge1 = modelEvPct(wp1, d1);
      edge2 = modelEvPct(wp2, d2);
    }

    const implied1 = (1 / d1) * 100;
    const openImplied1 = oDec1 > 1 ? (1 / oDec1) * 100 : NaN;
    let edge1Open = oDec1 > 1 ? modelEvPct(wp1, oDec1) : NaN;
    let edge2Open = oDec2 > 1 ? modelEvPct(wp2, oDec2) : NaN;
    let edge3Open = NaN;
    if (market === THREE_BALL_MARKET && oDec3 > 1 && Number.isFinite(wp3)) {
      edge3Open = modelEvPct(wp3, oDec3);
    }
    const pick10 = pickMatchupSide({ p1: edge1, p2: edge2, p3: edge3 }, 10);
    const o1 = outcomeToResult(num(row.p1_outcome, NaN));
    const o2 = outcomeToResult(num(row.p2_outcome, NaN));

    const openTime = String(row.open_time || "").trim();
    const closeTime = String(row.close_time || row.open_time || "").trim();

    samples.push({
      event_name: event,
      market,
      model_win_pct: wp1 * 100,
      close_implied_pct: implied1,
      edge_p1_pct: edge1,
      edge_p2_pct: edge2,
      edge_p3_pct: edge3,
      p1_result: o1,
      p2_result: o2,
      p3_result: o3,
      p1_close_dec: d1,
      p2_close_dec: d2,
      p3_close_dec: d3,
    });

    detailLines.push(
      rowToDetailLine({
        exported_at: exportedAt,
        event_name: event,
        year,
        round,
        book,
        bet_type: row.bet_type,
        market,
        open_time: openTime,
        close_time: closeTime,
        dg_id: id1,
        player_name: row.p1_player_name,
        opponent_dg_id: id2,
        opponent_name: row.p2_player_name,
        opponent2_dg_id: opp2Id,
        opponent2_name: opp2Name,
        model_mu_sg: fmtNum(mu1, 3),
        opp_mu_sg: fmtNum(mu2, 3),
        opp2_mu_sg: fmtNum(mu3, 3),
        sg_gap: fmtNum(mu1 - mu2, 3),
        model_win_pct: fmtNum(wp1 * 100, 2),
        open_implied_pct: Number.isFinite(openImplied1) ? fmtNum(openImplied1, 2) : "",
        close_implied_pct: fmtNum(implied1, 2),
        p1_open_dec: oDec1 > 1 ? fmtNum(oDec1, 4) : "",
        p2_open_dec: oDec2 > 1 ? fmtNum(oDec2, 4) : "",
        p3_open_dec: market === THREE_BALL_MARKET && oDec3 > 1 ? fmtNum(oDec3, 4) : "",
        p1_close_dec: fmtNum(d1, 4),
        p2_close_dec: fmtNum(d2, 4),
        p3_close_dec: market === THREE_BALL_MARKET ? fmtNum(d3, 4) : "",
        edge_p1_open_pct: Number.isFinite(edge1Open) ? fmtNum(edge1Open, 2) : "",
        edge_p2_open_pct: Number.isFinite(edge2Open) ? fmtNum(edge2Open, 2) : "",
        edge_p3_open_pct:
          market === THREE_BALL_MARKET && Number.isFinite(edge3Open) ? fmtNum(edge3Open, 2) : "",
        edge_p1_pct: fmtNum(edge1, 2),
        edge_p2_pct: fmtNum(edge2, 2),
        edge_p3_pct: market === THREE_BALL_MARKET ? fmtNum(edge3, 2) : "",
        p1_result: o1,
        p2_result: o2,
        p3_result: o3,
        pick_side_at_10: pick10?.side || "",
        book_odds_source: "historical_matchups_dk_fd_mgm_open_close",
      }),
    );
  }
  process.stdout.write("\n");

  mkdirSync(join(WEB_ROOT, "data"), { recursive: true });
  writeFileSync(DETAIL_OUT, `${detailLines.join("\n")}\n`, "utf8");
  writeFileSync(SUMMARY_OUT, buildSummary(samples, exportedAt), "utf8");

  console.log(`Wrote ${DETAIL_OUT} (${(detailLines.length - 1).toLocaleString()} rows)`);
  console.log(`Wrote ${SUMMARY_OUT}`);
  if (skippedNoModel) console.log(`  Skipped ${skippedNoModel.toLocaleString()} rows (no walk-forward μ_sg)`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
