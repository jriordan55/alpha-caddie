#!/usr/bin/env node
/**
 * Walk-forward round matchup backtest from data/historical_matchups_outcomes.csv.
 *
 *   npm run export:matchup-backtest
 *
 * Outputs (projection tracker):
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
import { walkforwardBacktestPipelineEnv } from "./projection-pipeline-env.mjs";
import { EV_THRESHOLDS_PCT } from "./round-projection-vs-actual-summary.mjs";
import { matchupWinProb, modelEvPct, decimalToAmerican, isDraftKingsMatchupBook } from "../projection-tracker/matchup-math.mjs";

Object.assign(process.env, walkforwardBacktestPipelineEnv());

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const MATCHUPS_CSV = join(REPO_ROOT, "data", "historical_matchups_outcomes.csv");
const HIST_CSV = join(REPO_ROOT, "data", "historical_rounds_all.csv");
const DETAIL_OUT = join(WEB_ROOT, "data", "matchup_backtest_detail.csv");
const SUMMARY_OUT = join(WEB_ROOT, "data", "matchup_backtest_summary.csv");

const MARKET_LABEL = "Round matchups";
const MAX_ROWS = Math.max(0, Math.round(Number(process.env.GOLF_MATCHUP_BACKTEST_MAX_ROWS || "0")));
const SINCE_ISO = String(process.env.GOLF_MATCHUP_BACKTEST_SINCE || "").trim();

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

function pickMatchupSide(edge1, edge2, minEv) {
  const th = num(minEv, 0);
  if (!Number.isFinite(edge1) || !Number.isFinite(edge2)) return null;
  if (edge1 >= th && edge1 >= edge2) return { side: "p1", edge: edge1 };
  if (edge2 >= th && edge2 > edge1) return { side: "p2", edge: edge2 };
  return null;
}

function pnlForDecimalResult(result, dec) {
  const r = String(result || "").toUpperCase();
  if (r === "W") return num(dec, NaN) - 1;
  if (r === "L") return -1;
  return 0;
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
  } else {
    acc.pushes += 1;
  }
}

function buildSummary(samples, exportedAt) {
  const evAcc = new Map();
  const lineAcc = { sq: 0, abs: 0, n: 0 };
  const byEvent = new Map();

  for (const s of samples) {
    if (Number.isFinite(s.model_win_pct) && Number.isFinite(s.close_implied_pct)) {
      const diff = s.model_win_pct - s.close_implied_pct;
      lineAcc.sq += diff * diff;
      lineAcc.abs += Math.abs(diff);
      lineAcc.n += 1;
    }

    for (const th of EV_THRESHOLDS_PCT) {
      const pick = pickMatchupSide(s.edge_p1_pct, s.edge_p2_pct, th);
      if (!pick) continue;
      const side = pick.side;
      const result = side === "p1" ? s.p1_result : s.p2_result;
      const dec = side === "p1" ? s.p1_close_dec : s.p2_close_dec;
      if (result !== "W" && result !== "L") continue;

      const gk = `${s.event_name}\x1f${th}\x1f${side}`;
      let acc = evAcc.get(gk);
      if (!acc) {
        acc = { event_name: s.event_name, threshold: th, side, ...emptyEvAcc() };
        evAcc.set(gk, acc);
      }
      addEvBet(acc, result, dec);

      const mk = `${th}\x1f${side}`;
      let macc = evAcc.get(`__market__\x1f${mk}`);
      if (!macc) {
        macc = { event_name: "", threshold: th, side, ...emptyEvAcc() };
        evAcc.set(`__market__\x1f${mk}`, macc);
      }
      addEvBet(macc, result, dec);

      const ek = `${s.event_name}\x1f${th}`;
      let eacc = byEvent.get(ek);
      if (!eacc) {
        eacc = { event_name: s.event_name, threshold: th, ...emptyEvAcc() };
        byEvent.set(ek, eacc);
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
      ].map(csvCell).join(","),
    );
  };

  if (lineAcc.n) {
    const rmse = Math.sqrt(lineAcc.sq / lineAcc.n);
    const mae = lineAcc.abs / lineAcc.n;
    pushRow("model_vs_book", "(all events)", "", "", MARKET_LABEL, rmse, mae, lineAcc.n, "", "", emptyEvAcc());
    pushRow(
      "model_vs_book_by_market",
      "(all events)",
      "",
      "(all)",
      MARKET_LABEL,
      rmse,
      mae,
      lineAcc.n,
      "",
      "",
      emptyEvAcc(),
    );
  }

  const events = [...new Set(samples.map((s) => s.event_name).filter(Boolean))].sort();
  for (const ev of events) {
    const evSamples = samples.filter((s) => s.event_name === ev);
    let sq = 0;
    let abs = 0;
    let n = 0;
    for (const s of evSamples) {
      if (!Number.isFinite(s.model_win_pct) || !Number.isFinite(s.close_implied_pct)) continue;
      const diff = s.model_win_pct - s.close_implied_pct;
      sq += diff * diff;
      abs += Math.abs(diff);
      n += 1;
    }
    if (n) {
      pushRow("model_vs_book", ev, "", "", MARKET_LABEL, Math.sqrt(sq / n), abs / n, n, "", "", emptyEvAcc());
    }
  }

  for (const acc of evAcc.values()) {
    if (acc.event_name) {
      pushRow("ev_backtest", acc.event_name, "", "", MARKET_LABEL, "", "", "", acc.threshold, acc.side, acc);
    } else {
      pushRow(
        "ev_backtest_by_market",
        "(all events)",
        "",
        "(all)",
        MARKET_LABEL,
        "",
        "",
        "",
        acc.threshold,
        acc.side,
        acc,
      );
    }
  }

  for (const [ek, acc] of byEvent.entries()) {
    const [event_name, thStr] = ek.split("\x1f");
    const th = num(thStr, NaN);
    pushRow("ev_backtest_by_market", event_name, "", "(all)", MARKET_LABEL, "", "", "", th, "pick", acc);
  }

  return header + rows.join("\n") + (rows.length ? "\n" : "");
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
    model_win_pct: num(row.model_win_pct, NaN),
    close_implied_pct: num(row.close_implied_pct, NaN),
    edge_p1_pct: num(row.edge_p1_pct, NaN),
    edge_p2_pct: num(row.edge_p2_pct, NaN),
    p1_result: row.p1_result,
    p2_result: row.p2_result,
    p1_close_dec: num(row.p1_close_dec, NaN),
    p2_close_dec: num(row.p2_close_dec, NaN),
  };
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
  for (let i = 1; i < lines.length; i++) {
    const cols = parseCsvLine(lines[i]);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cols[j] ?? "";
    if (replaceEvents.has(String(row.event_name || "").trim())) continue;
    if (!isDraftKingsMatchupBook(row.book)) continue;
    kept.push(lines[i]);
    samples.push(sampleFromDetailFields(row));
  }
  return { lines: kept, samples };
}

async function readMatchupRows() {
  /** @type {object[]} */
  const rows = [];
  const rl = createInterface({
    input: createReadStream(MATCHUPS_CSV, { encoding: "utf8" }),
    crlfDelay: Infinity,
  });
  let header = null;
  let n = 0;
  for await (const line of rl) {
    if (!line.trim()) continue;
    const cols = parseCsvLine(line);
    if (!header) {
      header = cols;
      continue;
    }
    const row = {};
    for (let i = 0; i < header.length; i++) row[header[i]] = cols[i] ?? "";
    if (!isRoundMatchupBetType(row.bet_type)) continue;
    const o1 = num(row.p1_outcome, NaN);
    const o2 = num(row.p2_outcome, NaN);
    if (!(o1 === 0 || o1 === 1) || !(o2 === 0 || o2 === 1)) continue;
    if (!isDraftKingsMatchupBook(row.book)) continue;
    if (SINCE_ISO) {
      const d = closeDateIso(row);
      if (!d || d < SINCE_ISO) continue;
    }
    rows.push(row);
    n += 1;
    if (MAX_ROWS > 0 && n >= MAX_ROWS) break;
  }
  return rows;
}

async function main() {
  if (!existsSync(MATCHUPS_CSV)) {
    console.error(`Missing ${MATCHUPS_CSV} — run npm run update:odds`);
    process.exit(1);
  }
  if (!existsSync(HIST_CSV)) {
    console.error(`Missing ${HIST_CSV}`);
    process.exit(1);
  }

  console.log("Reading historical_matchups_outcomes.csv (round matchups only) …");
  if (SINCE_ISO) console.log(`  Since filter: close_time >= ${SINCE_ISO}`);
  const matchupRows = await readMatchupRows();
  console.log(`  ${matchupRows.length.toLocaleString()} graded round matchup rows`);

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
  const histRows = parse(readFileSync(HIST_CSV, "utf8"), { columns: true, skip_empty_lines: true, relax_column_count: true });
  attachFieldDgIdsToProps(bundles, histRows);
  const cache = new FullModelProjectionCache(REPO_ROOT, histRows);
  await cache.prewarm(bundles);

  const exportedAt = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  /** @type {object[]} */
  const samples = [...existing.samples];
  /** @type {string[]} */
  const detailLines = [
    "exported_at,event_name,year,round,book,bet_type,dg_id,player_name,opponent_dg_id,opponent_name,market,model_mu_sg,opp_mu_sg,sg_gap,model_win_pct,close_implied_pct,p1_close_dec,p2_close_dec,edge_p1_pct,edge_p2_pct,p1_result,p2_result,pick_side_at_10,book_odds_source",
    ...existing.lines,
  ];

  let skippedNoModel = 0;
  let i = 0;
  for (const row of matchupRows) {
    i += 1;
    if (i % 5000 === 0 || i === matchupRows.length) {
      process.stdout.write(`\r  Model pricing ${i}/${matchupRows.length}`);
    }

    const round = parseRoundFromBetType(row.bet_type);
    const year = Math.round(num(row.year, NaN));
    const event = String(row.event_name || "").trim();
    const id1 = Math.round(num(row.p1_dg_id, NaN));
    const id2 = Math.round(num(row.p2_dg_id, NaN));
    const d1 = num(row.p1_close, NaN);
    const d2 = num(row.p2_close, NaN);
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

    const p1Win = matchupWinProb(mu1, mu2, "round_matchups");
    const edge1 = modelEvPct(p1Win, d1);
    const edge2 = modelEvPct(1 - p1Win, d2);
    const implied1 = (1 / d1) * 100;
    const pick10 = pickMatchupSide(edge1, edge2, 10);
    const o1 = num(row.p1_outcome, NaN) === 1 ? "W" : "L";
    const o2 = num(row.p2_outcome, NaN) === 1 ? "W" : "L";

    samples.push({
      event_name: event,
      model_win_pct: p1Win * 100,
      close_implied_pct: implied1,
      edge_p1_pct: edge1,
      edge_p2_pct: edge2,
      p1_result: o1,
      p2_result: o2,
      p1_close_dec: d1,
      p2_close_dec: d2,
    });

    detailLines.push(
      [
        exportedAt,
        event,
        year,
        round,
        String(row.book || "").trim().toLowerCase(),
        row.bet_type,
        id1,
        row.p1_player_name,
        id2,
        row.p2_player_name,
        MARKET_LABEL,
        fmtNum(mu1, 3),
        fmtNum(mu2, 3),
        fmtNum(mu1 - mu2, 3),
        fmtNum(p1Win * 100, 2),
        fmtNum(implied1, 2),
        fmtNum(d1, 4),
        fmtNum(d2, 4),
        fmtNum(edge1, 2),
        fmtNum(edge2, 2),
        o1,
        o2,
        pick10?.side || "",
        "historical_matchups_dk_close",
      ].map(csvCell).join(","),
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
