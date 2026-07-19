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
console.log("missing score rows", missing.length);
console.log(
  missing.map((r) => ({ p: r.player_name, dg: r.dg_id, rnd: r.round, ev: r.event_name })),
);

const histPath = existsSync(join(REPO, "data/historical_rounds_all.csv"))
  ? join(REPO, "data/historical_rounds_all.csv")
  : join(WEB, "data/historical_rounds_all.csv");

const found = [];
const missKeys = missing.map((m) => ({
  ...m,
  dg: Math.round(Number(m.dg_id)),
  rnd: Math.round(Number(m.round)),
}));

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
  for (const m of missKeys) {
    if (dg !== m.dg || rnd !== m.rnd) continue;
    const ev = String(row.event_name || "").toLowerCase();
    const want = String(m.event_name || "").toLowerCase();
    if (ev === want || ev.includes(want.slice(0, 12)) || want.includes(ev.slice(0, 12))) {
      found.push({
        want: m.player_name,
        rnd: m.rnd,
        event: m.event_name,
        histEv: row.event_name,
        year: row.year,
        score: row.round_score,
        bird: row.birdies,
        bog: row.bogies || row.bogeys,
        gir: row.gir,
        fw: row.driving_acc || row.fairways,
      });
    }
  }
}
console.log("hist matches", found.length);
console.log(JSON.stringify(found.slice(0, 50), null, 2));

const openMiss = detail.filter(
  (r) => r.event_name === "The Open Championship" && parseNum(r.actual_birdies) === null,
);
console.log(
  "Open missing birdies",
  openMiss.length,
  "by round",
  openMiss.reduce((a, r) => {
    a[r.round] = (a[r.round] || 0) + 1;
    return a;
  }, {}),
);
console.log("sources", [...new Set(openMiss.map((r) => r.actual_source))]);

const pga = JSON.parse(readFileSync(join(WEB, "data/pgatour_event_rounds.json"), "utf8"));
const pgaBy = new Map();
for (const r of pga.rounds || []) {
  if (!r._from_pgatour) continue;
  pgaBy.set(`${Math.round(Number(r.dg_id))}|${Math.round(Number(r.round_num))}`, r);
}
let inPga = 0;
let notInPga = 0;
let pgaHasBird = 0;
const not = [];
for (const r of openMiss) {
  const k = `${Math.round(Number(r.dg_id))}|${Math.round(Number(r.round))}`;
  const p = pgaBy.get(k);
  if (!p) {
    notInPga++;
    if (not.length < 12) {
      not.push({
        p: r.player_name,
        dg: r.dg_id,
        rnd: r.round,
        score: r.actual_round_score,
        src: r.actual_source,
      });
    }
  } else {
    inPga++;
    if (Number.isFinite(Number(p.birdies))) pgaHasBird++;
  }
}
console.log({ inPga, notInPga, pgaHasBird, not });
console.log(
  "pga rounds by round",
  (pga.rounds || [])
    .filter((r) => r._from_pgatour)
    .reduce((a, r) => {
      a[r.round_num] = (a[r.round_num] || 0) + 1;
      return a;
    }, {}),
);
