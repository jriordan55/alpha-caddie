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

// For each missing row with valid dg, find 2025/2026 year match only
const yearHits = [];
const noYearHit = [];
for (const m of missing) {
  const dg = Math.round(Number(m.dg_id));
  const rnd = Math.round(Number(m.round));
  if (!dg) {
    noYearHit.push({ reason: "dg0", p: m.player_name, ev: m.event_name, rnd });
    continue;
  }
  let hit = null;
  for await (const row of createReadStream(histPath).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  )) {
    if (Math.round(Number(row.dg_id)) !== dg) continue;
    if (Math.round(Number(row.round_num)) !== rnd) continue;
    if (String(row.event_name || "").trim() !== String(m.event_name || "").trim()) continue;
    const yr = Math.round(Number(row.year));
    if (yr === 2025 || yr === 2026) {
      hit = {
        p: m.player_name,
        dg,
        rnd,
        ev: m.event_name,
        year: yr,
        score: row.round_score,
        bird: row.birdies,
        bog: row.bogies || row.bogeys,
      };
      break;
    }
  }
  if (hit) yearHits.push(hit);
  else noYearHit.push({ reason: "no2025", p: m.player_name, dg, rnd, ev: m.event_name });
}
console.log("2025/26 hist hits", yearHits.length, yearHits);
console.log("still missing", noYearHit.length, noYearHit);

// Open live counting availability
const live = JSON.parse(readFileSync(join(WEB, "live-in-play.json"), "utf8"));
const openMiss = detail.filter(
  (r) => r.event_name === "The Open Championship" && parseNum(r.actual_birdies) === null,
);
let liveHasBird = 0;
let liveHasScore = 0;
const sample = [];
for (const r of openMiss) {
  const dg = String(Math.round(Number(r.dg_id)));
  const rnd = String(Math.round(Number(r.round)));
  const act = live.live_round_actuals_by_dg?.[dg]?.[rnd];
  const stats =
    live.live_tournament_stats_by_round?.[rnd]?.find?.(
      (x) => Math.round(Number(x.dg_id)) === Math.round(Number(r.dg_id)),
    ) || null;
  // also search object form
  let stats2 = null;
  const byRound = live.live_tournament_stats_by_round;
  if (byRound && typeof byRound === "object" && !Array.isArray(byRound[rnd])) {
    const arr = byRound[rnd]?.players || byRound[rnd] || [];
    if (Array.isArray(arr)) {
      stats2 = arr.find((x) => Math.round(Number(x.dg_id)) === Math.round(Number(r.dg_id)));
    }
  }
  const bird = act?.birdies ?? stats?.birdies ?? stats2?.birdies;
  const score = act?.round_score ?? stats?.score ?? stats2?.score;
  if (Number.isFinite(Number(bird))) liveHasBird++;
  if (Number.isFinite(Number(score))) liveHasScore++;
  if (sample.length < 5) {
    sample.push({
      p: r.player_name,
      dg,
      rnd,
      actBird: act?.birdies,
      actBog: act?.bogeys,
      actPar: act?.pars,
      actScore: act?.round_score,
      statsKeys: stats ? Object.keys(stats).slice(0, 20) : stats2 ? Object.keys(stats2).slice(0, 20) : null,
      statsBird: stats?.birdies ?? stats2?.birdies,
    });
  }
}
console.log({ liveHasBird, liveHasScore, openMiss: openMiss.length, sample });

// inspect structure of live_tournament_stats_by_round
const lts = live.live_tournament_stats_by_round;
console.log(
  "lts type",
  typeof lts,
  Array.isArray(lts),
  lts && typeof lts === "object" ? Object.keys(lts).slice(0, 10) : null,
);
if (lts && typeof lts === "object") {
  for (const k of Object.keys(lts).slice(0, 3)) {
    const v = lts[k];
    console.log(
      "lts",
      k,
      Array.isArray(v) ? `arr ${v.length}` : typeof v,
      Array.isArray(v) ? Object.keys(v[0] || {}).slice(0, 30) : Object.keys(v || {}).slice(0, 20),
    );
    if (Array.isArray(v) && v[0]) console.log("sample0", JSON.stringify(v[0]).slice(0, 400));
  }
}
