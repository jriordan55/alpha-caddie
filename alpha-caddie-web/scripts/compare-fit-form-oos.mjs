#!/usr/bin/env node
/**
 * Current kitchen-sink WF μ vs strict course-fit + recent-form μ.
 * Primary score = model vs actual MAE (projections), not bet-policy ROI.
 *
 *   npm run compare:fit-form-oos
 *   → data/fit_form_oos.json
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { walkforwardBacktestPipelineEnv } from "./projection-pipeline-env.mjs";
import { strictFitFormPipelineEnv } from "./strict-fit-form-mu.mjs";
import { buildFullModelMuMapForEvent } from "./historical-walkforward-projections.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { MARKET_BOOK_CALIBRATION_MARKETS } from "./market-book-calibration.mjs";
import { EXPORT_MARKETS, modelProbOver, num } from "./round-projection-mu.mjs";
import { fitOutcomeSigmaScales, setOutcomeSigmaScales } from "./projection-stat-model.mjs";
import { capDirectionalPostedEdges, devigFairTwoWay, pickBetSide } from "../projection-tracker/ev-math.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const HIST = join(REPO, "data", "historical_rounds_all.csv");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
const OUT_JSON = join(WEB, "data", "fit_form_oos.json");

const STRATEGIES = [
  {
    id: "current_wf",
    name: "Current WF stack",
    note: "Hole/distance SG, weather, form carry, round μ decay, venue intercepts",
    env: { ...walkforwardBacktestPipelineEnv() },
  },
  {
    id: "strict_fit_form",
    name: "Strict course fit + recent form",
    note: "fit+form + weather + tee wave + course distance SG + hole SG only if major (|stpAdj|≥0.25)",
    env: { ...strictFitFormPipelineEnv() },
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
        for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const modelLine = parseLine(row[cols.model]);
          const bookLine = parseLine(row[cols.book]);
          if (!Number.isFinite(bookLine)) continue;
          const actual = parseLine(row[cols.actual]);
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
            bookLine,
            actual: actualOk ? actual : NaN,
            overOdds: num(row[cols.overOdds], NaN),
            underOdds: num(row[cols.underOdds], NaN),
            overRes: String(row[cols.overRes] || "").trim().toUpperCase(),
            underRes: String(row[cols.underRes] || "").trim().toUpperCase(),
            context: { dg_id: dg, round: rnd },
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
    .sort((a, b) => (a[1].ms || 0) - (b[1].ms || 0) || b[1].n - a[1].n)
    .map(([ev]) => ev);
}

function implied(am) {
  const v = num(am, NaN);
  if (!Number.isFinite(v) || v === 0) return NaN;
  return v > 0 ? 100 / (v + 100) : -v / (-v + 100);
}

function roiOnRows(rows, minEvPct) {
  let units = 0;
  let n = 0;
  let wins = 0;
  const byMarket = new Map();
  for (const b of rows) {
    const mu = b.modelLine;
    if (!Number.isFinite(mu)) continue;
    const pOver = modelProbOver(b.market, mu, b.bookLine, { total_score: mu, round_sd: 3 }, {
      projection_course_basis: { fairway_holes_modeled: 14 },
    });
    if (!Number.isFinite(pOver)) continue;
    const pUnder = 1 - pOver;
    const { fairOver, fairUnder } = devigFairTwoWay(b.overOdds, b.underOdds);
    let edgeOver = Number.isFinite(fairOver) ? (pOver - fairOver) * 100 : (pOver - implied(b.overOdds)) * 100;
    let edgeUnder = Number.isFinite(fairUnder) ? (pUnder - fairUnder) * 100 : (pUnder - implied(b.underOdds)) * 100;
    ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, b.bookLine));
    const pick = pickBetSide(edgeOver, edgeUnder, minEvPct, mu, b.bookLine);
    if (!pick) continue;
    const res = pick.side === "over" ? b.overRes : b.underRes;
    const odds = pick.side === "over" ? b.overOdds : b.underOdds;
    if (res !== "W" && res !== "L") continue;
    const pnl = res === "W" ? (odds > 0 ? odds / 100 : 100 / -odds) : -1;
    units += pnl;
    n++;
    if (res === "W") wins++;
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
    hit_pct: n > 0 ? (wins / n) * 100 : NaN,
    roi_pct: n > 0 ? (units / n) * 100 : NaN,
    byMarket,
  };
}

function errorOnRows(rows) {
  const byMarket = new Map();
  for (const b of rows) {
    if (!Number.isFinite(b.actual) || !Number.isFinite(b.modelLine)) continue;
    const err = b.modelLine - b.actual;
    const vsBook = Number.isFinite(b.bookLine) ? b.modelLine - b.bookLine : NaN;
    const mk = byMarket.get(b.market) || { n: 0, sumErr: 0, sumAbs: 0, sumBookAbs: 0, nBook: 0 };
    mk.n++;
    mk.sumErr += err;
    mk.sumAbs += Math.abs(err);
    if (Number.isFinite(vsBook)) {
      mk.sumBookAbs += Math.abs(vsBook);
      mk.nBook++;
    }
    byMarket.set(b.market, mk);
  }
  const out = {};
  for (const [m, s] of byMarket) {
    out[m] = {
      n: s.n,
      bias: Math.round((s.sumErr / s.n) * 100) / 100,
      mae: Math.round((s.sumAbs / s.n) * 1000) / 1000,
      mae_vs_book: s.nBook ? Math.round((s.sumBookAbs / s.nBook) * 1000) / 1000 : null,
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
    const rnd = Math.round(num(b.context?.round, NaN));
    const dg = Math.round(num(b.dg_id, NaN));
    if (!Number.isFinite(yr) || !Number.isFinite(rnd) || !Number.isFinite(dg)) continue;
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
    if (!Number.isFinite(mu)) return { ...b, modelLine: NaN };
    return { ...b, modelLine: mu };
  });
}

function roundObj(m) {
  const out = {};
  for (const [k, v] of m.entries()) {
    out[k] = {
      units: Math.round(v.units * 10) / 10,
      bets: v.bets,
      roi_pct: v.bets ? Math.round((v.units / v.bets) * 1000) / 10 : null,
    };
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

  console.log(`[fit-form-oos] ${betRows.length} rows · ${oosEvents.length} events · exclude ${liveEvent || "none"}\n`);

  const results = [];
  for (const strat of STRATEGIES) {
    console.log(`Testing: ${strat.name}`);
    const graded = await rebuildModelLines(betRows, histRows, eventYearMap, fieldMap, strat.env);
    const errors = errorOnRows(graded);
    const unf = roiOnRows(graded, 5);
    results.push({ id: strat.id, name: strat.name, note: strat.note, errors, unfiltered: unf });
    console.log("  MAE vs actual:");
    for (const m of MARKET_BOOK_CALIBRATION_MARKETS) {
      const e = errors[m];
      if (!e) continue;
      console.log(
        `    ${m.padEnd(14)} mae ${String(e.mae).padStart(6)}  bias ${e.bias >= 0 ? "+" : ""}${e.bias}  vsBook ${e.mae_vs_book}  n=${e.n}`,
      );
    }
    console.log(
      `  unfiltered @5% EV: ${unf.roi_pct?.toFixed(1)}% · ${unf.units >= 0 ? "+" : ""}${unf.units.toFixed(1)}u · ${unf.bets} bets\n`,
    );
  }

  const payload = {
    generated_at: new Date().toISOString(),
    formula:
      "μ = venue + shrink(player@course) + shrink(recent form) + weather + tee wave + course distance SG + hole SG only if |stpAdj|≥0.25",
    constants: { k: 10, n_form: 10, n_skill: 36, decay: 0.86 },
    methodology: {
      oos_bet_rows: betRows.length,
      oos_event_count: oosEvents.length,
      excluded_live_event: liveEvent || null,
      events: oosEvents,
      elapsed_sec: Math.round((Date.now() - t0) / 1000),
      primary_metric: "mae_vs_actual",
    },
    strategies: results.map((r) => ({
      id: r.id,
      name: r.name,
      note: r.note,
      model_vs_actual: r.errors,
      unfiltered_at_5pct: {
        roi_pct: Math.round((r.unfiltered.roi_pct || 0) * 10) / 10,
        units: Math.round(r.unfiltered.units * 100) / 100,
        bets: r.unfiltered.bets,
        by_market: roundObj(r.unfiltered.byMarket),
      },
    })),
  };
  writeFileSync(OUT_JSON, `${JSON.stringify(payload, null, 2)}\n`);
  console.log(`[fit-form-oos] wrote ${OUT_JSON} (${payload.methodology.elapsed_sec}s)`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
