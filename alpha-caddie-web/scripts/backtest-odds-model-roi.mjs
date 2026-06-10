#!/usr/bin/env node
/**
 * Model-driven ROI/PnL on odds.csv lines vs actual outcomes.
 *
 * Uses the full round projection model (fetch:dg pipeline + default pricing mode
 * from round-projection-mu.mjs), walk-forward from historical_rounds_all only.
 *
 *   npm run backtest:odds-model-roi
 *
 * Outputs:
 *   alpha-caddie-web/data/odds_model_roi_summary.csv
 *   alpha-caddie-web/data/odds_model_roi_detail.csv
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
import {
  attachFieldDgIdsToProps,
  FullModelProjectionCache,
} from "./historical-walkforward-projections.mjs";
import {
  birdiesPlusEaglesFromRow,
  modelEdgePctAtLine,
  ouSideResults,
} from "./round-projection-mu.mjs";
import { americanToDecimal } from "./round-projection-vs-actual-summary.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = resolve(WEB_ROOT, "..");
const ODDS_CSV = join(REPO_ROOT, "data", "odds.csv");
const HIST_CSV = join(REPO_ROOT, "data", "historical_rounds_all.csv");
const SUMMARY_OUT = join(WEB_ROOT, "data", "odds_model_roi_summary.csv");
const DETAIL_OUT = join(WEB_ROOT, "data", "odds_model_roi_detail.csv");

const OU_MARKETS = new Set([
  "GOLF:FT:CTBIR",
  "GOLF:FT:ROUNDNUMBIRDIES",
  "GOLF:FT:CTSTR",
  "GOLF:P:ROUND1OUSCORE",
]);

const MARKET_LABEL = {
  "GOLF:FT:CTBIR": "Birdies",
  "GOLF:FT:ROUNDNUMBIRDIES": "Birdies",
  "GOLF:FT:CTSTR": "Total score",
  "GOLF:P:ROUND1OUSCORE": "Total score",
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

function fmtNum(v, d = 2) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10 ** d) / 10 ** d).toFixed(d);
}

function fmtPct(v) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10) / 10).toFixed(1);
}

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
    return { event: raw.replace(/\s+20\d{2}\s*$/, "").trim(), year: Number(ym[1]), competition: raw };
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

function statFromHistRow(row, stat) {
  if (stat === "birdies") return birdiesPlusEaglesFromRow(row);
  if (stat === "total") return num(row.round_score, NaN);
  return NaN;
}

function pnlForResult(result, american) {
  if (result === "P" || result === "") return 0;
  const dec = americanToDecimal(american);
  if (!Number.isFinite(dec)) return 0;
  return result === "W" ? dec - 1 : -1;
}

function parseOddsProps(oddsRows) {
  /** @type {Map<string, object>} */
  const props = new Map();

  for (const row of oddsRows) {
    const marketType = String(row.MARKET_TYPE || "").trim();
    if (!OU_MARKETS.has(marketType)) continue;

    const marketLabel = MARKET_LABEL[marketType] || marketType;
    const sel = parseSelection(row.SELECTION);
    if (!sel || !Number.isFinite(sel.line)) continue;

    const { event, year, competition } = parseCompetition(row.COMPETITION, row.EVENT_START_TIME_UTC);
    const round =
      parseRoundFromText(row.SPORT_EVENT, row.MARKET_NAME) ||
      (marketType === "GOLF:P:ROUND1OUSCORE" ? 1 : NaN);
    if (!Number.isFinite(year) || !Number.isFinite(round)) continue;

    const player = parseOuPlayer(row.MARKET_NAME);
    const betTimeMs =
      Date.parse(String(row.EVENT_START_TIME_UTC || "").replace(" ", "T")) ||
      Date.parse(`${year}-01-01T12:00:00Z`);

    const key = `${year}|${foldComparableTitle(event)}|${round}|${normNameLoose(player)}|${marketLabel}|${sel.line}`;
    let p = props.get(key);
    if (!p) {
      p = {
        competition,
        event,
        year,
        round,
        player,
        market_label: marketLabel,
        market_type: marketType,
        line: sel.line,
        bet_time_ms: betTimeMs,
        over_am: NaN,
        close_over_am: NaN,
        under_am: NaN,
        close_under_am: NaN,
      };
      props.set(key, p);
    }

    const openAm = num(row.OPENING_AMERICAN_ODDS, NaN);
    const closeAm = num(row.CLOSING_AMERICAN_ODDS, NaN);
    if (sel.side === "over") {
      p.over_am = openAm;
      p.close_over_am = closeAm;
    } else {
      p.under_am = openAm;
      p.close_under_am = closeAm;
    }
  }
  return props;
}

function attachActuals(props, histRows) {
  const buckets = new Map();
  for (const r of histRows) {
    const year = num(r.year, NaN);
    const round = num(r.round_num, NaN);
    const event = String(r.event_name || "").trim();
    if (!Number.isFinite(year) || !Number.isFinite(round) || !event) continue;
    const k = `${year}|${foldComparableTitle(event)}|${round}`;
    if (!buckets.has(k)) buckets.set(k, []);
    buckets.get(k).push(r);
  }

  for (const p of props.values()) {
    const k = `${p.year}|${foldComparableTitle(p.event)}|${p.round}`;
    let bucket = buckets.get(k) || [];
    if (!bucket.length) {
      for (const [bk, rows] of buckets.entries()) {
        const [y, ev, rnd] = bk.split("|");
        if (Number(y) !== p.year || Number(rnd) !== p.round) continue;
        if (eventsLikelySame(p.event, ev.replace(/-/g, " "))) {
          bucket = rows;
          break;
        }
      }
    }
    for (const h of bucket) {
      if (!oddsPlayerMatchesHist(p.player, h.player_name)) continue;
      p.matched_player = displayGolferName(h.player_name);
      p.dg_id = num(h.dg_id, NaN);
      p.actual_birdies = statFromHistRow(h, "birdies");
      p.actual_total = statFromHistRow(h, "total");
      break;
    }
  }
}

async function buildModelBets(props, projCache, playerRowByDg, opts = {}) {
  const minLineEdge = num(opts.minLineEdge, 0);
  const minEdgePct = num(opts.minEdgePct, NaN);
  const strategy = opts.strategy || "line";
  const bets = [];

  for (const p of props.values()) {
    const actual =
      p.market_label === "Birdies" ? p.actual_birdies : p.actual_total;
    if (!Number.isFinite(actual)) continue;

    const dgId = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dgId)) continue;

    const modelMu = await projCache.muForProp(p, dgId, p.market_label);
    if (!Number.isFinite(modelMu)) continue;

    const playerRow = playerRowByDg.get(`${projCache.eventKey(p)}|${dgId}`) || {};
    const sides = ouSideResults(p.market_label, actual, p.line);
    let pick = "";
    let edgePct = NaN;

    if (strategy === "ev") {
      const edge = modelEdgePctAtLine(
        p.market_label,
        modelMu,
        p.line,
        playerRow,
        { projection_course_basis: { fairway_holes_modeled: 14 } },
        p.over_am,
        p.under_am,
      );
      if (Number.isFinite(edge.edgeOver) && Number.isFinite(edge.edgeUnder)) {
        if (edge.edgeOver >= edge.edgeUnder && edge.edgeOver > 0) {
          pick = "over";
          edgePct = edge.edgeOver;
        } else if (edge.edgeUnder > 0) {
          pick = "under";
          edgePct = edge.edgeUnder;
        }
      }
      if (Number.isFinite(minEdgePct) && (!Number.isFinite(edgePct) || edgePct < minEdgePct)) continue;
    } else {
      const diff = modelMu - p.line;
      if (Math.abs(diff) < minLineEdge) continue;
      if (diff > 0) pick = "over";
      else if (diff < 0) pick = "under";
      edgePct = Math.abs(diff);
    }

    if (!pick) continue;
    const openAm = pick === "over" ? p.over_am : p.under_am;
    const closeAm = pick === "over" ? p.close_over_am : p.close_under_am;
    if (!Number.isFinite(openAm)) continue;

    const result = pick === "over" ? sides.over : sides.under;
    if (result !== "W" && result !== "L" && result !== "P") continue;

    bets.push({
      ...p,
      model_mu: modelMu,
      model_line_edge: modelMu - p.line,
      model_pick: pick,
      model_edge_pct: edgePct,
      result,
      opening_american: openAm,
      closing_american: closeAm,
      actual,
      pnl_open: pnlForResult(result, openAm),
      pnl_close: pnlForResult(result, closeAm),
    });
  }
  return bets;
}

function summarize(bets, tag) {
  const graded = bets.filter((b) => b.result === "W" || b.result === "L");
  const unitsOpen = bets.reduce((s, b) => s + (b.pnl_open || 0), 0);
  const unitsClose = bets.reduce((s, b) => s + (b.pnl_close || 0), 0);
  const wins = graded.filter((b) => b.result === "W").length;
  const hit = graded.length ? (wins / graded.length) * 100 : NaN;
  const roiOpen = bets.length ? (unitsOpen / bets.length) * 100 : NaN;
  const roiClose = bets.length ? (unitsClose / bets.length) * 100 : NaN;
  let sq = 0;
  let abs = 0;
  let nErr = 0;
  for (const b of bets) {
    if (!Number.isFinite(b.model_mu) || !Number.isFinite(b.actual)) continue;
    const e = b.model_mu - b.actual;
    sq += e * e;
    abs += Math.abs(e);
    nErr++;
  }
  return {
    tag,
    bets: bets.length,
    wins: graded.filter((b) => b.result === "W").length,
    losses: graded.filter((b) => b.result === "L").length,
    pushes: bets.filter((b) => b.result === "P").length,
    hit_rate_pct: hit,
    units_open: unitsOpen,
    units_close: unitsClose,
    roi_open_pct: roiOpen,
    roi_close_pct: roiClose,
    proj_rmse: nErr ? Math.sqrt(sq / nErr) : NaN,
    proj_mae: nErr ? abs / nErr : NaN,
  };
}

function writeOutputs(bets, summaries, generatedAt) {
  const sumHeader =
    "generated_at,strategy,min_line_edge,min_edge_pct,market,bets,wins,losses,pushes,hit_rate_pct,units_open,units_close,roi_open_pct,roi_close_pct,proj_rmse,proj_mae\n";
  const sumRows = summaries.map((s) =>
    [
      generatedAt,
      s.strategy,
      s.min_line_edge ?? "",
      s.min_edge_pct ?? "",
      s.market ?? "__all__",
      s.bets,
      s.wins,
      s.losses,
      s.pushes,
      fmtPct(s.hit_rate_pct),
      fmtNum(s.units_open, 2),
      fmtNum(s.units_close, 2),
      fmtPct(s.roi_open_pct),
      fmtPct(s.roi_close_pct),
      fmtNum(s.proj_rmse, 3),
      fmtNum(s.proj_mae, 3),
    ]
      .map(csvCell)
      .join(","),
  );

  const detHeader =
    "generated_at,competition,event,year,round,player,matched_player,market,model_mu,line,model_line_edge,model_pick,actual,result,opening_american,closing_american,pnl_open,pnl_close\n";
  const detRows = bets.map((b) =>
    [
      generatedAt,
      b.competition,
      b.event,
      b.year,
      b.round,
      b.player,
      b.matched_player || "",
      b.market_label,
      fmtNum(b.model_mu, 3),
      b.line,
      fmtNum(b.model_line_edge, 3),
      b.model_pick,
      b.actual,
      b.result,
      b.opening_american,
      b.closing_american,
      fmtNum(b.pnl_open, 3),
      fmtNum(b.pnl_close, 3),
    ]
      .map(csvCell)
      .join(","),
  );

  mkdirSync(dirname(SUMMARY_OUT), { recursive: true });
  writeFileSync(SUMMARY_OUT, sumHeader + sumRows.join("\n") + "\n");
  writeFileSync(DETAIL_OUT, detHeader + detRows.join("\n") + "\n");
}

async function main() {
  if (!existsSync(ODDS_CSV)) throw new Error(`Missing ${ODDS_CSV}`);
  if (!existsSync(HIST_CSV)) throw new Error(`Missing ${HIST_CSV}`);

  console.log("Loading odds.csv and historical_rounds_all.csv …");
  const oddsRows = parse(readFileSync(ODDS_CSV, "utf8"), { columns: true, skip_empty_lines: true });
  const histRows = parse(readFileSync(HIST_CSV, "utf8"), { columns: true, skip_empty_lines: true });
  const props = parseOddsProps(oddsRows);
  attachActuals(props, histRows);
  attachFieldDgIdsToProps(props, histRows);

  const projCache = new FullModelProjectionCache(REPO_ROOT, histRows);
  const playerRowByDg = new Map();

  console.log("Building full-model projections (cached by event×round) …");
  await projCache.prewarm(props);
  const generatedAt = new Date().toISOString();
  const summaries = [];
  const allDetail = [];

  const configs = [
    { strategy: "line", minLineEdge: 0, minEdgePct: "", label: "line_any" },
    { strategy: "line", minLineEdge: 0.25, minEdgePct: "", label: "line_0.25" },
    { strategy: "line", minLineEdge: 0.5, minEdgePct: "", label: "line_0.5" },
    { strategy: "ev", minLineEdge: 0, minEdgePct: 0, label: "ev_0" },
    { strategy: "ev", minLineEdge: 0, minEdgePct: 2.5, label: "ev_2.5" },
    { strategy: "ev", minLineEdge: 0, minEdgePct: 5, label: "ev_5" },
  ];

  for (const cfg of configs) {
    const bets = await buildModelBets(props, projCache, playerRowByDg, cfg);
    const all = summarize(bets, cfg.label);
    summaries.push({
      ...all,
      strategy: cfg.label,
      min_line_edge: cfg.minLineEdge,
      min_edge_pct: cfg.minEdgePct,
      market: "__all__",
    });
    if (cfg.label === "line_any") allDetail.push(...bets);

    for (const market of ["Birdies", "Total score"]) {
      const sub = bets.filter((b) => b.market_label === market);
      summaries.push({
        ...summarize(sub, `${cfg.label}|${market}`),
        strategy: cfg.label,
        min_line_edge: cfg.minLineEdge,
        min_edge_pct: cfg.minEdgePct,
        market,
      });
    }
  }

  writeOutputs(allDetail, summaries, generatedAt);

  const base = summaries.find((s) => s.strategy === "line_any" && s.market === "__all__");
  const bird = summaries.find((s) => s.strategy === "line_any" && s.market === "Birdies");
  const score = summaries.find((s) => s.strategy === "line_any" && s.market === "Total score");

  console.log(`Wrote ${SUMMARY_OUT}`);
  console.log(`Wrote ${DETAIL_OUT} (${allDetail.length} bets)\n`);
  console.log("Full-model ROI on odds.csv (default pricing mode, walk-forward, flat 1u @ closing):\n");
  if (base) {
    console.log(
      `  All O/U — ${base.bets} bets, ${fmtPct(base.hit_rate_pct)}% hit, ${fmtNum(base.units_close, 1)}u PnL, ${fmtPct(base.roi_close_pct)}% ROI`,
    );
    console.log(
      `           projection vs actual: RMSE ${fmtNum(base.proj_rmse, 2)}, MAE ${fmtNum(base.proj_mae, 2)}`,
    );
  }
  if (bird) console.log(`  Birdies — ${bird.bets} bets, ${fmtNum(bird.units_close, 1)}u, ${fmtPct(bird.roi_close_pct)}% ROI`);
  if (score) console.log(`  Total score — ${score.bets} bets, ${fmtNum(score.units_close, 1)}u, ${fmtPct(score.roi_close_pct)}% ROI`);
  const ev0 = summaries.find((s) => s.strategy === "ev_0" && s.market === "__all__");
  if (ev0) console.log(`\n  EV>0 filter — ${ev0.bets} bets, ${fmtNum(ev0.units_close, 1)}u, ${fmtPct(ev0.roi_close_pct)}% ROI`);
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
