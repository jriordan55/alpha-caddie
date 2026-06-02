#!/usr/bin/env node
/**
 * Wind > 10 mph vs calm: which O/U markets shift most.
 * - Full history: tour-benchmark lines on ~33k rounds with weather
 * - DK audit: real book lines where captured (PGA + Schwab samples)
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");

const TOUR_BENCHMARK_LINE = {
  "Total score": 70.383,
  Birdies: 3.664,
  Bogeys: 2.493,
  Pars: 10.814,
  GIR: 10.764,
  "Fairways hit": 7.588,
  Putts: 29.5,
};

const MARKETS = [
  { name: "Total score", actual: "round_score", lowerBetter: true },
  { name: "Birdies", actual: "birdies", lowerBetter: false },
  { name: "Bogeys", actual: "bogeys", lowerBetter: true },
  { name: "Pars", actual: "pars", lowerBetter: false },
  { name: "GIR", actual: "gir", lowerBetter: false },
  { name: "Fairways hit", actual: "fairways", lowerBetter: false },
  { name: "Putts", actual: "putts", lowerBetter: true },
];

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function normEv(s) {
  return String(s || "")
    .trim()
    .toLowerCase();
}

function parseYearFromRow(r) {
  const y = num(r.year, NaN);
  if (Number.isFinite(y)) return Math.round(y);
  const p = String(r.event_completed || "").split("/");
  const yr = parseInt(p[2], 10);
  return Number.isFinite(yr) ? yr : NaN;
}

function emptyBucket() {
  return { n: 0, over: 0, under: 0, push: 0, actualSum: 0, lineSum: 0 };
}

function rate(b, side) {
  const d = b.over + b.under;
  return d ? b[side] / d : NaN;
}

function ouResult(actual, line, lowerIsBetter) {
  if (!Number.isFinite(actual) || !Number.isFinite(line)) return null;
  if (actual === line) return "push";
  if (lowerIsBetter) return actual < line ? "under" : "over";
  return actual > line ? "over" : "under";
}

function addBucket(b, side, actual, line) {
  b.n++;
  b.actualSum += actual;
  b.lineSum += line;
  if (side === "over") b.over++;
  else if (side === "under") b.under++;
  else b.push++;
}

function birdiesWithEagles(rec) {
  const b = num(rec.birdies, NaN);
  const e = num(rec.eagles_or_better, NaN);
  if (!Number.isFinite(b)) return NaN;
  return Number.isFinite(e) ? b + e : b;
}

function buildWindIndex(hist) {
  const byDgId = new Map();
  const byKey = new Map();
  for (const b of Object.values(hist.byDgId || {})) {
    const dgBucket = Math.round(num(b.dg_id, NaN));
    for (const r of b.rounds || []) {
      const w = num(r.weather_wind_mph, NaN);
      if (!Number.isFinite(w)) continue;
      const dg = Math.round(num(r.dg_id ?? dgBucket, NaN));
      const rnd = Math.round(num(r.round_num ?? r.round, NaN));
      const yr = parseYearFromRow(r);
      const ev = normEv(r.event_name);
      if (!Number.isFinite(dg) || !Number.isFinite(rnd) || !Number.isFinite(yr)) continue;
      const rec = {
        wind: w,
        birdies: num(r.birdies, NaN),
        bogeys: num(r.bogeys ?? r.bogies, NaN),
        pars: num(r.pars, NaN),
        gir: num(r.gir, NaN),
        fairways: num(r.fairways, NaN),
        putts: num(r.putts, NaN),
        round_score: num(r.round_score ?? r.score, NaN),
        eagles_or_better: num(r.eagles_or_better, NaN),
      };
      byKey.set(`${dg}|${yr}|${rnd}|${ev}`, rec);
      if (!byDgId.has(dg)) byDgId.set(dg, []);
      byDgId.get(dg).push({ ...rec, yr, rnd, ev, event_name: r.event_name });
    }
  }
  return { byKey, byDgId };
}

function lookupWind(byKey, byDgId, dg, yr, rnd, eventName) {
  const ev = normEv(eventName);
  let rec = byKey.get(`${dg}|${yr}|${rnd}|${ev}`);
  if (rec) return rec;
  for (const alt of byDgId.get(dg) || []) {
    if (alt.rnd !== rnd || alt.yr !== yr) continue;
    if (eventsLikelySame(eventName, alt.event_name)) return alt;
  }
  return null;
}

function aggregateOu(byDgId, lineForMarket) {
  const agg = {};
  for (const m of MARKETS) agg[m.name] = { windy: emptyBucket(), calm: emptyBucket() };

  for (const rounds of byDgId.values()) {
    for (const r of rounds) {
      const bucket = r.wind > 10 ? "windy" : "calm";
      for (const m of MARKETS) {
        const line = lineForMarket(m);
        if (!Number.isFinite(line)) continue;
        let v = num(r[m.actual], NaN);
        if (m.name === "Birdies" && !Number.isFinite(v)) v = birdiesWithEagles(r);
        if (!Number.isFinite(v)) continue;
        const side = ouResult(v, line, m.lowerBetter);
        if (!side) continue;
        addBucket(agg[m.name][bucket], side, v, line);
      }
    }
  }
  return agg;
}

function summarizeOu(agg, label) {
  const rows = [];
  for (const m of MARKETS) {
    const w = agg[m.name].windy;
    const c = agg[m.name].calm;
    const overW = rate(w, "over");
    const overC = rate(c, "over");
    const delta = Number.isFinite(overW) && Number.isFinite(overC) ? overW - overC : NaN;
    rows.push({
      market: m.name,
      source: label,
      n_windy: w.over + w.under,
      n_calm: c.over + c.under,
      over_pct_windy: overW,
      over_pct_calm: overC,
      delta_over_pct: delta,
      abs_delta_over_pct: Number.isFinite(delta) ? Math.abs(delta) : NaN,
      under_pct_windy: rate(w, "under"),
      under_pct_calm: rate(c, "under"),
      avg_actual_windy: w.n ? w.actualSum / w.n : NaN,
      avg_actual_calm: c.n ? c.actualSum / c.n : NaN,
      actual_shift_windy_minus_calm: w.n && c.n ? w.actualSum / w.n - c.actualSum / c.n : NaN,
      lower_is_better: m.lowerBetter,
    });
  }
  rows.sort((a, b) => (b.abs_delta_over_pct || 0) - (a.abs_delta_over_pct || 0));
  return rows;
}

async function loadAuditRows() {
  const path = join(WEB, "data", "dk_round_projection_audit.csv");
  if (!existsSync(path)) return [];
  const rows = [];
  await new Promise((resolve, reject) => {
    createReadStream(path)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("error", reject)
      .on("end", resolve);
  });
  return rows;
}

function aggregateDkAudit(byKey, byDgId, auditRows) {
  const latest = new Map();
  for (const r of auditRows) {
    const dg = Math.round(num(r.dg_id, NaN));
    const rnd = Math.round(num(r.display_round ?? r.round, NaN));
    const mkt = String(r.market || "").trim();
    const ev = String(r.event_name || "").trim();
    const cap = String(r.captured_at || "");
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || !mkt) continue;
    const key = `${dg}|${normEv(ev)}|${rnd}|${mkt}`;
    const prev = latest.get(key);
    if (!prev || cap > prev.captured_at) latest.set(key, { ...r, dg, rnd, ev, mkt, captured_at: cap });
  }

  const agg = {};
  for (const m of MARKETS) agg[m.name] = { windy: emptyBucket(), calm: emptyBucket() };
  let windyN = 0;
  let calmN = 0;

  for (const r of latest.values()) {
    const mDef = MARKETS.find((m) => m.name === r.mkt || (r.mkt === "Total Score" && m.name === "Total score"));
    if (!mDef) continue;

    let wRec = null;
    for (const yr of [2026, 2025, 2024, 2023]) {
      wRec = lookupWind(byKey, byDgId, r.dg, yr, r.rnd, r.ev);
      if (wRec) break;
    }
    if (!wRec) continue;

    const line = num(r.dk_line, NaN);
    let actual = num(wRec[mDef.actual], NaN);
    if (mDef.name === "Birdies" && !Number.isFinite(actual)) actual = birdiesWithEagles(wRec);
    if (!Number.isFinite(actual) || !Number.isFinite(line)) continue;

    const side = ouResult(actual, line, mDef.lowerBetter);
    if (!side) continue;

    const bucket = wRec.wind > 10 ? "windy" : "calm";
    addBucket(agg[mDef.name][bucket], side, actual, line);
    if (bucket === "windy") windyN++;
    else calmN++;
  }

  return { agg, windyN, calmN };
}

async function main() {
  const hist = JSON.parse(readFileSync(join(WEB, "player_round_history.json"), "utf8"));
  const { byKey, byDgId } = buildWindIndex(hist);

  let windyRounds = 0;
  let calmRounds = 0;
  for (const rec of byKey.values()) {
    if (rec.wind > 10) windyRounds++;
    else calmRounds++;
  }

  const tourAgg = aggregateOu(byDgId, (m) => TOUR_BENCHMARK_LINE[m.name]);
  const tourRows = summarizeOu(tourAgg, "tour_benchmark_line");

  const auditRows = await loadAuditRows();
  const { agg: dkAgg, windyN: dkWindyObs, calmN: dkCalmObs } = aggregateDkAudit(byKey, byDgId, auditRows);
  const dkRows = summarizeOu(dkAgg, "dk_book_line");

  console.log(
    JSON.stringify(
      {
        meta: {
          wind_threshold_mph: 10,
          rounds_with_wind_data: windyRounds + calmRounds,
          windy_rounds: windyRounds,
          calm_rounds: calmRounds,
          tour_benchmark_lines: TOUR_BENCHMARK_LINE,
          dk_audit_windy_observations: dkWindyObs,
          dk_audit_calm_observations: dkCalmObs,
          note: "delta_over_pct = (over rate when windy) − (over rate when calm). For lower-is-better markets (score, bogeys, putts), higher over rate = harder conditions vs the line.",
        },
        ranked_by_ou_shift_tour_benchmark: tourRows,
        ranked_by_ou_shift_dk_lines: dkRows,
      },
      null,
      2,
    ),
  );
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
