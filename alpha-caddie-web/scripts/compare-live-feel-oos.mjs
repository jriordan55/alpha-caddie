#!/usr/bin/env node
/**
 * Backtest the levers that make live projections feel "off":
 *   flat venue (same R1–R4 score) vs day/form separation
 *   counting lift / within-event counting blend
 *   skill window (80 vs 36)
 *
 * Walk-forward OOS on pre-round DK rows; excludes the live event.
 * Does NOT change live projections — comparison only.
 *
 *   node scripts/compare-live-feel-oos.mjs
 *   → data/live_feel_oos_roi.json
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import {
  flatVenueProjectionPipelineEnv,
  liveProjectionPipelineEnv,
  walkforwardBacktestPipelineEnv,
} from "./projection-pipeline-env.mjs";
import { buildFullModelMuMapForEvent } from "./historical-walkforward-projections.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { MARKET_BOOK_CALIBRATION_MARKETS } from "./market-book-calibration.mjs";
import { EXPORT_MARKETS, modelProbOver, num } from "./round-projection-mu.mjs";
import { fitOutcomeSigmaScales, setOutcomeSigmaScales } from "./projection-stat-model.mjs";
import { capDirectionalPostedEdges, devigFairTwoWay, pickBetSide } from "../projection-tracker/ev-math.mjs";
import { DEFAULT_MIN_EV_PCT, minEvForMarket, qualifiesBet } from "./bet-policy.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const HIST = join(REPO, "data", "historical_rounds_all.csv");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
const OUT_JSON = join(WEB, "data", "sportsbook_live_oos_roi.json");

/** Head-to-head: old flat live vs new sportsbook-style live (+ WF reference). */
const STRATEGIES = [
  {
    id: "old_live_flat",
    name: "OLD live flat",
    note: "Previous push:live — flat venue, no form, no prior difficulty, no book cal",
    env: {
      ...flatVenueProjectionPipelineEnv(),
      GOLF_UNIFIED_TEE_WAVE_W: "0.30",
      GOLF_FIELD_DAY_COUNTING_LIFT_FRAC: "0",
      GOLF_WITHIN_EVENT_COUNTING_BLEND: "0",
      GOLF_WF_SKILL_MAX_ROUNDS: "80",
      GOLF_MARKET_BOOK_CALIBRATION: "0",
    },
  },
  {
    id: "new_sportsbook_live",
    name: "NEW sportsbook live",
    note: "Current push:live — day/form + skill 36 + soft book align + mild counting blend",
    env: { ...liveProjectionPipelineEnv() },
  },
  {
    id: "wf_skill36",
    name: "WF day+form + skill 36 (no book cal)",
    note: "Same reconstruction as NEW without DK book μ/σ shifts",
    env: {
      ...walkforwardBacktestPipelineEnv(),
      GOLF_WF_SKILL_MAX_ROUNDS: "36",
      GOLF_MARKET_BOOK_CALIBRATION: "0",
    },
  },
];

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? num(s, NaN) : NaN;
}

function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
}

const MARKET_COLS = Object.fromEntries(
  EXPORT_MARKETS.map((m) => [
    m.market,
    {
      model: m.lineCol,
      book: m.bookLineCol,
      overOdds: m.overOddsCol,
      underOdds: m.underOddsCol,
      overRes: m.overCol,
      underRes: m.underCol,
      actual: m.actualCol,
    },
  ]),
);

async function loadWalkForwardBetRowsWithDg() {
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const rows = [];
  await new Promise((resolvePromise, reject) => {
    Readable.from([aligned])
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.pricing_skill || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        const event = String(row.event_name || "").trim();
        if (!event) return;
        const dg = Math.round(num(row.dg_id, NaN));
        const rnd = Math.round(num(row.round, NaN));
        if (!Number.isFinite(dg) || !Number.isFinite(rnd)) return;
        const t = parseMs(row.projections_updated_at) || parseMs(row.exported_at);
        const context = {
          dg_id: dg,
          round: rnd,
          gir_minus_fw: num(row.gir_minus_fw, NaN),
          course_fw_width: num(row.course_fw_width, NaN),
        };
        for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const modelLine = parseLine(row[cols.model]);
          const bookLine = parseLine(row[cols.book]);
          if (!Number.isFinite(modelLine) || !Number.isFinite(bookLine)) continue;
          const actual = parseLine(row[cols.actual]);
          // Skip stub-zero actuals on counting markets (same rule as live sanitize).
          let actualOk = Number.isFinite(actual);
          if (actualOk && (market === "Birdies" || market === "Bogeys") && actual === 0) {
            const scoreAct = parseLine(row.actual_round_score);
            if (Number.isFinite(scoreAct) && scoreAct > 0) actualOk = false;
          }
          rows.push({
            event,
            eventMs: t,
            market,
            dg_id: dg,
            modelLine,
            modelBookDelta: modelLine - bookLine,
            bookLine,
            actual: actualOk ? actual : NaN,
            overOdds: num(row[cols.overOdds], NaN),
            underOdds: num(row[cols.underOdds], NaN),
            overRes: String(row[cols.overRes] || "").trim().toUpperCase(),
            underRes: String(row[cols.underRes] || "").trim().toUpperCase(),
            context,
          });
        }
      })
      .on("end", resolvePromise)
      .on("error", reject);
  });
  return rows;
}

function eventOrderFromRows(rows) {
  const m = new Map();
  for (const r of rows) {
    let e = m.get(r.event);
    if (!e) e = { ms: r.eventMs, n: 0 };
    if (Number.isFinite(r.eventMs) && (!Number.isFinite(e.ms) || r.eventMs < e.ms)) e.ms = r.eventMs;
    e.n++;
    m.set(r.event, e);
  }
  return [...m.entries()]
    .sort((a, b) => {
      const ta = Number.isFinite(a[1].ms) ? a[1].ms : 0;
      const tb = Number.isFinite(b[1].ms) ? b[1].ms : 0;
      return ta - tb || b[1].n - a[1].n;
    })
    .map(([ev]) => ev);
}

function implied(am) {
  const v = num(am, NaN);
  if (!Number.isFinite(v) || v === 0) return NaN;
  return v > 0 ? 100 / (v + 100) : -v / (-v + 100);
}

function roiOnRows(rows, minEvPct, { useRecommendedPolicy = false } = {}) {
  let units = 0;
  let n = 0;
  let wins = 0;
  let losses = 0;
  const byMarket = new Map();
  for (const b of rows) {
    const mu = b.modelLine;
    const pOver = modelProbOver(b.market, mu, b.bookLine, 1, 14);
    const pUnder = 1 - pOver;
    const { fairOver, fairUnder } = devigFairTwoWay(b.overOdds, b.underOdds);
    let edgeOver = Number.isFinite(fairOver) ? (pOver - fairOver) * 100 : (pOver - implied(b.overOdds)) * 100;
    let edgeUnder = Number.isFinite(fairUnder) ? (pUnder - fairUnder) * 100 : (pUnder - implied(b.underOdds)) * 100;
    ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, b.bookLine));
    const evTh = useRecommendedPolicy ? minEvForMarket(b.market, minEvPct) : minEvPct;
    const pick = pickBetSide(edgeOver, edgeUnder, evTh, mu, b.bookLine);
    if (!pick) continue;
    if (
      useRecommendedPolicy &&
      !qualifiesBet({
        market: b.market,
        modelLine: mu,
        bookLine: b.bookLine,
        context: b.context || {},
        eventName: b.event,
        side: pick.side,
      })
    ) {
      continue;
    }
    const res = pick.side === "over" ? b.overRes : b.underRes;
    const odds = pick.side === "over" ? b.overOdds : b.underOdds;
    if (res !== "W" && res !== "L" && res !== "P") continue;
    const pnl = res === "W" ? (odds > 0 ? odds / 100 : 100 / -odds) : res === "L" ? -1 : 0;
    units += pnl;
    n++;
    if (res === "W") wins++;
    else if (res === "L") losses++;
    const mk = byMarket.get(b.market) || { units: 0, bets: 0, wins: 0 };
    mk.units += pnl;
    mk.bets++;
    if (res === "W") mk.wins++;
    byMarket.set(b.market, mk);
  }
  return {
    units,
    bets: n,
    wins,
    losses,
    hit_pct: n > 0 ? (wins / n) * 100 : NaN,
    roi_pct: n > 0 ? (units / n) * 100 : NaN,
    byMarket,
  };
}

/** Model vs actual error (excludes stub zeros). */
function errorOnRows(rows) {
  const byMarket = new Map();
  for (const b of rows) {
    if (!Number.isFinite(b.actual) || !Number.isFinite(b.modelLine)) continue;
    const err = b.modelLine - b.actual;
    const mk = byMarket.get(b.market) || { n: 0, sumErr: 0, sumAbs: 0 };
    mk.n++;
    mk.sumErr += err;
    mk.sumAbs += Math.abs(err);
    byMarket.set(b.market, mk);
  }
  const out = {};
  for (const [m, s] of byMarket) {
    out[m] = {
      n: s.n,
      bias: Math.round((s.sumErr / s.n) * 100) / 100,
      mae: Math.round((s.sumAbs / s.n) * 100) / 100,
    };
  }
  return out;
}

function loadLiveEvent() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

async function loadHistRows() {
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);
  const rows = [];
  await new Promise((resolvePromise, reject) => {
    createReadStream(HIST)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", resolvePromise)
      .on("error", reject);
  });
  return rows;
}

function buildEventYearMap(histRows) {
  const m = new Map();
  for (const row of histRows) {
    const ev = String(row.event_name || "").trim();
    const yr = Math.round(num(row.year, NaN));
    if (!ev || !Number.isFinite(yr)) continue;
    const k = foldComparableTitle(ev);
    const prev = m.get(k);
    if (!prev || yr > prev) m.set(k, yr);
  }
  return m;
}

function buildFieldDgIdsByBundle(histRows) {
  const fields = new Map();
  for (const row of histRows) {
    const ev = String(row.event_name || "").trim();
    const yr = Math.round(num(row.year, NaN));
    const rnd = Math.round(num(row.round_num, NaN));
    const dg = Math.round(num(row.dg_id, NaN));
    if (!ev || !Number.isFinite(yr) || !Number.isFinite(rnd) || !Number.isFinite(dg)) continue;
    const k = `${yr}|${foldComparableTitle(ev)}|${rnd}`;
    if (!fields.has(k)) fields.set(k, new Set());
    fields.get(k).add(dg);
  }
  return fields;
}

function bundleKeysFromBetRows(betRows, eventYearMap) {
  const bundles = new Map();
  for (const b of betRows) {
    const yr = eventYearMap.get(foldComparableTitle(b.event));
    if (!Number.isFinite(yr)) continue;
    const rnd = Math.round(num(b.context?.round, NaN));
    const dg = Math.round(num(b.dg_id, NaN));
    if (!Number.isFinite(rnd) || !Number.isFinite(dg)) continue;
    const key = `${yr}|${foldComparableTitle(b.event)}|${rnd}`;
    if (!bundles.has(key)) {
      bundles.set(key, { event: b.event, year: yr, round: rnd, betTimeMs: b.eventMs, dgIds: new Set() });
    }
    bundles.get(key).dgIds.add(dg);
  }
  return bundles;
}

async function rebuildModelLines(betRows, histRows, eventYearMap, fieldMap, strategyEnv) {
  for (const k of Object.keys(process.env)) {
    if (k.startsWith("GOLF_") && k !== "GOLF_MODEL_DIR") delete process.env[k];
  }
  Object.assign(process.env, strategyEnv);
  const bundles = bundleKeysFromBetRows(betRows, eventYearMap);
  const muByBundle = new Map();

  const keys = [...bundles.values()];
  let i = 0;
  for (const b of keys) {
    const bundleKey = `${b.year}|${foldComparableTitle(b.event)}|${b.round}`;
    const fieldDgIds = [...(fieldMap.get(bundleKey) || b.dgIds)];
    const map = await buildFullModelMuMapForEvent({
      repoRoot: REPO,
      histRows,
      eventName: b.event,
      eventYear: b.year,
      targetRound: b.round,
      betTimeMs: b.betTimeMs,
      fieldDgIds,
      pipelineEnv: strategyEnv,
    });
    muByBundle.set(bundleKey, map);
    i++;
    if (i % 5 === 0 || i === keys.length) process.stdout.write(`\r    projections ${i}/${keys.length}`);
  }
  process.stdout.write("\n");

  return betRows.map((b) => {
    const yr = eventYearMap.get(foldComparableTitle(b.event));
    const rnd = Math.round(num(b.context?.round, NaN));
    const dg = Math.round(num(b.dg_id, NaN));
    if (!Number.isFinite(yr) || !Number.isFinite(rnd) || !Number.isFinite(dg)) return b;
    const bundleKey = `${yr}|${foldComparableTitle(b.event)}|${rnd}`;
    const mu = muByBundle.get(bundleKey)?.get(dg)?.get(b.market);
    if (!Number.isFinite(mu)) return b;
    return { ...b, modelLine: mu, modelBookDelta: mu - b.bookLine };
  });
}

function byMarketObj(m) {
  const out = {};
  for (const [k, v] of m.entries()) {
    out[k] = { units: Math.round(v.units * 10) / 10, bets: v.bets, wins: v.wins };
  }
  return out;
}

async function main() {
  const t0 = Date.now();
  const scales = await fitOutcomeSigmaScales(VS);
  setOutcomeSigmaScales(scales);

  const histRows = await loadHistRows();
  const eventYearMap = buildEventYearMap(histRows);
  const fieldMap = buildFieldDgIdsByBundle(histRows);
  const liveEvent = loadLiveEvent();
  let betRows = await loadWalkForwardBetRowsWithDg();

  const oosEvents = eventOrderFromRows(betRows).filter((ev) => !liveEvent || !eventsLikelySame(ev, liveEvent));
  betRows = betRows.filter((b) => oosEvents.includes(b.event));

  console.log(`[live-feel-oos] OOS bet rows: ${betRows.length} across ${oosEvents.length} events`);
  console.log(`[live-feel-oos] Events: ${oosEvents.join(" | ")}`);
  console.log(`[live-feel-oos] Live event excluded: ${liveEvent || "(none)"}\n`);

  const results = [];
  for (const strat of STRATEGIES) {
    console.log(`Testing: ${strat.name}`);
    console.log(`  ${strat.note}`);
    const graded = await rebuildModelLines(betRows, histRows, eventYearMap, fieldMap, strat.env);
    const rec = roiOnRows(graded, DEFAULT_MIN_EV_PCT, { useRecommendedPolicy: true });
    const unfiltered = roiOnRows(graded, 5);
    const errors = errorOnRows(graded);
    results.push({ id: strat.id, name: strat.name, note: strat.note, rec, unfiltered, errors });
    console.log(
      `  policy: ${rec.roi_pct?.toFixed(1)}% ROI, ${rec.units >= 0 ? "+" : ""}${rec.units.toFixed(2)}u, ${rec.bets} bets, ${rec.hit_pct?.toFixed(1)}% hit`,
    );
    const scoreErr = errors["Total score"];
    const birdErr = errors.Birdies;
    if (scoreErr) console.log(`  score bias ${scoreErr.bias >= 0 ? "+" : ""}${scoreErr.bias} mae ${scoreErr.mae} (n=${scoreErr.n})`);
    if (birdErr) console.log(`  bird bias ${birdErr.bias >= 0 ? "+" : ""}${birdErr.bias} mae ${birdErr.mae} (n=${birdErr.n})`);
    console.log("");
  }

  results.sort((a, b) => (b.rec.roi_pct || -999) - (a.rec.roi_pct || -999));

  console.log("=== Ranked by recommended-policy ROI ===\n");
  console.log(
    `${"strategy".padEnd(38)} ${"ROI%".padStart(7)} ${"PnL(u)".padStart(9)} ${"bets".padStart(6)} ${"hit%".padStart(6)}  ${"scoreBias".padStart(10)} ${"birdBias".padStart(9)}`,
  );
  for (const r of results) {
    const sb = r.errors["Total score"];
    const bb = r.errors.Birdies;
    console.log(
      `${r.name.padEnd(38)} ${String(r.rec.roi_pct?.toFixed(1) ?? "—").padStart(7)} ${`${r.rec.units >= 0 ? "+" : ""}${r.rec.units.toFixed(2)}`.padStart(9)} ${String(r.rec.bets).padStart(6)} ${String(r.rec.hit_pct?.toFixed(1) ?? "—").padStart(6)}  ${String(sb ? `${sb.bias >= 0 ? "+" : ""}${sb.bias}` : "—").padStart(10)} ${String(bb ? `${bb.bias >= 0 ? "+" : ""}${bb.bias}` : "—").padStart(9)}`,
    );
  }

  console.log("\n=== Per-market PnL (recommended) ===\n");
  const markets = [...new Set(results.flatMap((r) => [...r.rec.byMarket.keys()]))];
  console.log(`${"strategy".padEnd(38)} ${markets.map((m) => m.slice(0, 12).padStart(14)).join(" ")}`);
  for (const r of results) {
    const cells = markets.map((m) => {
      const s = r.rec.byMarket.get(m);
      if (!s) return "—".padStart(14);
      return `${s.units >= 0 ? "+" : ""}${s.units.toFixed(1)}u/${s.bets}b`.padStart(14);
    });
    console.log(`${r.name.padEnd(38)} ${cells.join(" ")}`);
  }

  const baseline = results.find((r) => r.id === "old_live_flat") || results[results.length - 1];
  const payload = {
    generated_at: new Date().toISOString(),
    hypothetical: true,
    note: "Walk-forward OOS: OLD flat live vs NEW sportsbook live (day/form + skill 36 + book align). Does not change live projections.",
    methodology: {
      oos_bet_rows: betRows.length,
      oos_event_count: oosEvents.length,
      excluded_live_event: liveEvent || null,
      events: oosEvents,
      policy: "recommended per-market EV/gap filters",
      elapsed_sec: Math.round((Date.now() - t0) / 1000),
    },
    baseline_id: baseline?.id,
    strategies: results.map((r) => ({
      id: r.id,
      name: r.name,
      note: r.note,
      is_current_live: r.id === "new_sportsbook_live",
      delta_vs_old_flat:
        baseline && r.id !== baseline.id
          ? {
              roi_pct: Math.round(((r.rec.roi_pct || 0) - (baseline.rec.roi_pct || 0)) * 10) / 10,
              units: Math.round((r.rec.units - baseline.rec.units) * 100) / 100,
            }
          : null,
      recommended: {
        roi_pct: Math.round((r.rec.roi_pct || 0) * 10) / 10,
        units: Math.round(r.rec.units * 100) / 100,
        bets: r.rec.bets,
        hit_pct: Math.round((r.rec.hit_pct || 0) * 10) / 10,
        by_market: byMarketObj(r.rec.byMarket),
      },
      unfiltered_at_5pct: {
        roi_pct: Math.round((r.unfiltered.roi_pct || 0) * 10) / 10,
        units: Math.round(r.unfiltered.units * 100) / 100,
        bets: r.unfiltered.bets,
      },
      model_vs_actual: r.errors,
    })),
  };
  writeFileSync(OUT_JSON, JSON.stringify(payload, null, 2));
  console.log(`\n[live-feel-oos] wrote ${OUT_JSON} (${payload.methodology.elapsed_sec}s)`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
