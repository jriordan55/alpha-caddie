#!/usr/bin/env node
/**
 * Fit per-market DK book-alignment calibration WITHOUT outcome lookahead.
 *
 * μ shift: shrunk mean(book − model) on training rows only (known pre-round).
 * σ scale: inflated from model-vs-book RMSE (no W/L optimization).
 *
 * Production file excludes the current live event in projections.json.
 * Reports walk-forward OOS ROI (fit on prior events, grade each event once).
 *
 *   node scripts/fit-market-book-calibration.mjs
 */
import { readFileSync, writeFileSync, existsSync, createReadStream } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import {
  MARKET_BOOK_CALIBRATION_MARKETS,
  marketBookCalibrationPath,
  fitMarketBookParamsFromDeltas,
} from "./market-book-calibration.mjs";
import { EXPORT_MARKETS, num, sigmaForOu } from "./round-projection-mu.mjs";
import { pickBetSide, pnlForResult } from "../projection-tracker/ev-math.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
const MIN_EV = 5;

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
    },
  ]),
);

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? num(s, NaN) : NaN;
}

function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
}

function loadCurrentLiveEventName() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

async function loadRows() {
  if (!existsSync(VS)) throw new Error(`Missing ${VS}`);
  /** @type {object[]} */
  const rows = [];
  await new Promise((resolve, reject) => {
    createReadStream(VS)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.pricing_skill || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        const event = String(row.event_name || "").trim();
        if (!event) return;
        const t = parseMs(row.projections_updated_at) || parseMs(row.exported_at);
        const meta = { projection_course_basis: { fairway_holes_modeled: 14 } };
        for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const modelLine = parseLine(row[cols.model]);
          const bookLine = parseLine(row[cols.book]);
          if (!Number.isFinite(modelLine) || !Number.isFinite(bookLine)) continue;
          const overOdds = num(row[cols.overOdds], NaN);
          const underOdds = num(row[cols.underOdds], NaN);
          const overRes = String(row[cols.overRes] || "").trim().toUpperCase();
          const underRes = String(row[cols.underRes] || "").trim().toUpperCase();
          const stubRow =
            market === "Total score"
              ? { total_score: modelLine, round_sd: 2.75 }
              : market === "Birdies"
                ? { birdies: modelLine }
                : market === "Bogeys"
                  ? { bogeys: modelLine }
                  : market === "GIR"
                    ? { gir: modelLine }
                    : market === "Fairways hit"
                      ? { fairways: modelLine }
                      : { pars: modelLine };
          rows.push({
            event,
            eventMs: t,
            market,
            modelLine,
            bookLine,
            modelBookDelta: modelLine - bookLine,
            overOdds,
            underOdds,
            overRes,
            underRes,
            stubRow,
            meta,
          });
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return rows;
}

function eventOrder(rows) {
  /** @type {Map<string, { ms: number, n: number }>} */
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
      if (ta !== tb) return ta - tb;
      return a[0].localeCompare(b[0]);
    })
    .map(([name]) => name);
}

function fitParamsFromTrainRows(trainRows) {
  /** @type {Record<string, { mu_shift: number, sigma_scale: number, n_fit: number, model_book_delta: number, model_book_rmse: number }>} */
  const markets = {};
  for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
    const deltas = trainRows.filter((r) => r.market === market).map((r) => r.modelBookDelta);
    markets[market] = fitMarketBookParamsFromDeltas(market, deltas);
  }
  return markets;
}

function normalCdf(z) {
  const t = 1 / (1 + 0.2316419 * Math.abs(z));
  const d = 0.3989423 * Math.exp((-z * z) / 2);
  const p =
    d * t * (0.3193815 + t * (-0.3565638 + t * (1.7814779 + t * (-1.821256 + t * 1.3302744))));
  return z >= 0 ? 1 - p : p;
}

function implied(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return 100 / 210;
  if (v < 0) return (-v) / (-v + 100);
  return 100 / (v + 100);
}

function roiOnRows(testRows, markets) {
  let units = 0;
  let n = 0;
  for (const b of testRows) {
    const m = markets[b.market];
    if (!m) continue;
    const mu = b.modelLine + m.mu_shift;
    const sigBase = sigmaForOu(b.market, b.stubRow, b.meta, 14);
    const sig = sigBase * m.sigma_scale;
    const z = (b.bookLine - mu) / sig;
    const pOver = 1 - normalCdf(z);
    const pUnder = 1 - pOver;
    const edgeOver = (pOver - implied(b.overOdds)) * 100;
    const edgeUnder = (pUnder - implied(b.underOdds)) * 100;
    const pick = pickBetSide(edgeOver, edgeUnder, MIN_EV);
    if (!pick) continue;
    const res = pick.side === "over" ? b.overRes : b.underRes;
    const odds = pick.side === "over" ? b.overOdds : b.underOdds;
    if (res !== "W" && res !== "L" && res !== "P") continue;
    units += pnlForResult(res, odds);
    n++;
  }
  return { units, n, roi: n > 0 ? (units / n) * 100 : NaN };
}

const allRows = await loadRows();
const liveEvent = loadCurrentLiveEventName();
const events = eventOrder(allRows);
const trainRows = liveEvent
  ? allRows.filter((r) => !eventsLikelySame(r.event, liveEvent))
  : allRows;

console.log(`Rows: ${allRows.length} market×player-round (default, DK audit)`);
console.log(`Events: ${events.join(" → ")}`);
if (liveEvent) console.log(`Training excludes current live week: ${liveEvent} (${trainRows.length} train rows)`);

const markets = fitParamsFromTrainRows(trainRows);
for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
  const m = markets[market];
  console.log(
    `  ${market.padEnd(14)} shift ${String(m.mu_shift).padStart(6)} σ×${m.sigma_scale}  ` +
      `Δmb ${m.model_book_delta}  rmse ${m.model_book_rmse}  n=${m.n_fit}`,
  );
}

// Walk-forward OOS: fit on prior events only, grade each event once.
let oosUnits = 0;
let oosBets = 0;
const oosByEvent = [];
for (let i = 1; i < events.length; i++) {
  const ev = events[i];
  if (liveEvent && eventsLikelySame(ev, liveEvent)) continue;
  const prior = events.slice(0, i);
  const train = allRows.filter((r) => prior.includes(r.event));
  const test = allRows.filter((r) => r.event === ev);
  if (train.length < 80 || test.length < 20) continue;
  const wfMarkets = fitParamsFromTrainRows(train);
  const r = roiOnRows(test, wfMarkets);
  if (r.n > 0) {
    oosUnits += r.units;
    oosBets += r.n;
    oosByEvent.push({ event: ev, ...r });
  }
}
const oosRoi = oosBets > 0 ? (oosUnits / oosBets) * 100 : NaN;
console.log(`\nWalk-forward OOS @ ${MIN_EV}% EV (${oosByEvent.length} events, no outcome fitting):`);
for (const e of oosByEvent) {
  console.log(`  ${e.event.slice(0, 42).padEnd(44)} ${e.units >= 0 ? "+" : ""}${e.units.toFixed(1)}u  ${e.n} bets  ${Number.isFinite(e.roi) ? e.roi.toFixed(1) : "—"}%`);
}
console.log(
  `  OOS combined: ${oosUnits >= 0 ? "+" : ""}${oosUnits.toFixed(1)}u / ${oosBets} bets = ${Number.isFinite(oosRoi) ? oosRoi.toFixed(1) : "—"}% ROI`,
);

const inSample = roiOnRows(trainRows, markets);
console.log(
  `\nIn-sample train (no live week) @ ${MIN_EV}%: ${inSample.units.toFixed(1)}u / ${inSample.n} bets = ${Number.isFinite(inSample.roi) ? inSample.roi.toFixed(1) : "—"}%`,
);

const out = {
  generated_at: new Date().toISOString(),
  fit_method: "book_alignment_no_outcome_peek",
  min_ev_pct: MIN_EV,
  excluded_live_event: liveEvent || null,
  train_events: events.filter((e) => !liveEvent || !eventsLikelySame(e, liveEvent)),
  walkforward_oos: {
    events: oosByEvent.length,
    bets: oosBets,
    units: Math.round(oosUnits * 100) / 100,
    roi_pct: Number.isFinite(oosRoi) ? Math.round(oosRoi * 10) / 10 : null,
  },
  markets,
};

writeFileSync(marketBookCalibrationPath(), `${JSON.stringify(out, null, 2)}\n`);
console.log(`\nWrote ${marketBookCalibrationPath()}`);
