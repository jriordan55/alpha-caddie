import { createReadStream, readFileSync, existsSync } from "fs";
import { parse } from "csv-parse";
import { join } from "path";

const WEB = join(import.meta.dirname, "..");
const REPO = join(WEB, "..");

function parseNum(x) {
  const s = String(x ?? "").trim();
  if (s === "") return null;
  const n = Number(s);
  return Number.isFinite(n) ? n : null;
}

const detail = [];
for await (const r of createReadStream(join(WEB, "data/round_projection_vs_actual.csv")).pipe(
  parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
)) {
  detail.push(r);
}
const missing = detail.filter((r) => parseNum(r.actual_round_score) === null);

const histPath = existsSync(join(REPO, "data/historical_rounds_all.csv"))
  ? join(REPO, "data/historical_rounds_all.csv")
  : join(WEB, "data/historical_rounds_all.csv");

/** @type {Map<string, object[]>} */
const histIndex = new Map();
for await (const row of createReadStream(histPath).pipe(
  parse({
    columns: true,
    relax_quotes: true,
    relax_column_count: true,
    skip_records_with_error: true,
  }),
)) {
  const dg = Math.round(Number(row.dg_id));
  const rnd = Math.round(Number(row.round_num));
  if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;
  const key = `${dg}|${rnd}|${String(row.event_name || "").trim()}`;
  if (!histIndex.has(key)) histIndex.set(key, []);
  histIndex.get(key).push(row);
}

const yearHits = [];
const noYearHit = [];
for (const m of missing) {
  const dg = Math.round(Number(m.dg_id));
  const rnd = Math.round(Number(m.round));
  if (!dg) {
    noYearHit.push({ reason: "dg0", p: m.player_name, ev: m.event_name, rnd });
    continue;
  }
  const rows = histIndex.get(`${dg}|${rnd}|${String(m.event_name || "").trim()}`) || [];
  const hit = rows.find((row) => {
    const yr = Math.round(Number(row.year));
    return yr === 2025 || yr === 2026;
  });
  if (hit) {
    yearHits.push({
      p: m.player_name,
      dg,
      rnd,
      ev: m.event_name,
      year: hit.year,
      score: hit.round_score,
      bird: hit.birdies,
      bog: hit.bogies || hit.bogeys,
    });
  } else {
    noYearHit.push({
      reason: "no2025",
      p: m.player_name,
      dg,
      rnd,
      ev: m.event_name,
      years: rows.map((r) => r.year),
    });
  }
}
console.log("2025/26 hist hits", yearHits.length);
console.log(JSON.stringify(yearHits, null, 2));
console.log("still missing", noYearHit.length);
console.log(JSON.stringify(noYearHit, null, 2));

// Why year key fails in export: inspect audit captured years for one event
const auditPath = join(WEB, "data/dk_round_model_audit.csv");
const schwab = [];
if (existsSync(auditPath)) {
  for await (const row of createReadStream(auditPath).pipe(
    parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
  )) {
    if (String(row.event_name || "").trim() !== "Charles Schwab Challenge") continue;
    if (Math.round(Number(row.dg_id)) !== 17365) continue; // Mitchell
    schwab.push({
      rnd: row.round_num || row.round,
      captured: row.captured_at || row.proj_updated_at || row.projections_updated_at,
      model: row.model_total_score,
    });
    if (schwab.length > 8) break;
  }
}
console.log("Mitchell Schwab audit sample", schwab);

const live = JSON.parse(readFileSync(join(WEB, "live-in-play.json"), "utf8"));
const openMiss = detail.filter(
  (r) => r.event_name === "The Open Championship" && parseNum(r.actual_birdies) === null,
);
const actSample = [];
let withActBird = 0;
for (const r of openMiss.slice(0, 20)) {
  const dg = String(Math.round(Number(r.dg_id)));
  const rnd = String(Math.round(Number(r.round)));
  const act = live.live_round_actuals_by_dg?.[dg]?.[rnd] || null;
  if (act && Number.isFinite(Number(act.birdies))) withActBird++;
  if (actSample.length < 5) actSample.push({ p: r.player_name, dg, rnd, act });
}
console.log({ openMiss: openMiss.length, withActBirdInFirst20: withActBird, actSample });

// PGATour missing players — do they appear in live data only?
const pga = JSON.parse(readFileSync(join(WEB, "data/pgatour_event_rounds.json"), "utf8"));
const pgaDgs = new Set(
  (pga.rounds || [])
    .filter((r) => r._from_pgatour && Number(r.round_num) === 1)
    .map((r) => Math.round(Number(r.dg_id))),
);
const openR1 = detail.filter((r) => r.event_name === "The Open Championship" && String(r.round) === "1");
const missingPga = openR1.filter((r) => !pgaDgs.has(Math.round(Number(r.dg_id))));
console.log("Open R1 not in PGATour", missingPga.length);
console.log(
  missingPga.slice(0, 15).map((r) => ({ p: r.player_name, dg: r.dg_id, bird: r.actual_birdies, src: r.actual_source })),
);
