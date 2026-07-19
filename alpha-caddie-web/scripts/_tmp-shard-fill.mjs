import { createReadStream, readFileSync, existsSync } from "fs";
import { parse } from "csv-parse";
import { join } from "path";
import { eventsLikelySame } from "./dg-events-align.mjs";

const WEB = join(import.meta.dirname, "..");
const detail = [];
for await (const r of createReadStream(join(WEB, "data/round_projection_vs_actual.csv")).pipe(
  parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
)) {
  detail.push(r);
}
const parseNum = (x) => {
  const s = String(x ?? "").trim();
  if (s === "") return null;
  const n = Number(s);
  return Number.isFinite(n) ? n : null;
};

const openMiss = detail.filter(
  (r) => r.event_name === "The Open Championship" && parseNum(r.actual_birdies) === null,
);

let shardHit = 0;
let shardMiss = 0;
const hits = [];
const misses = [];
for (const r of openMiss) {
  const dg = Math.round(Number(r.dg_id));
  const rnd = Math.round(Number(r.round));
  const p = join(WEB, "player-history", "by-dg", `${dg}.json`);
  if (!existsSync(p)) {
    shardMiss++;
    misses.push({ p: r.player_name, dg, rnd, why: "no_shard" });
    continue;
  }
  const j = JSON.parse(readFileSync(p, "utf8"));
  const row = (j.rounds || []).find(
    (x) =>
      Math.round(Number(x.round_num)) === rnd &&
      Math.round(Number(x.year)) === 2026 &&
      eventsLikelySame(String(x.event_name || ""), "The Open Championship"),
  );
  if (row && Number.isFinite(Number(row.birdies))) {
    shardHit++;
    hits.push({ p: r.player_name, dg, rnd, bird: row.birdies, bog: row.bogies || row.bogeys, score: row.round_score });
  } else {
    shardMiss++;
    misses.push({ p: r.player_name, dg, rnd, why: "no_round" });
  }
}
console.log({ openMiss: openMiss.length, shardHit, shardMiss });
console.log("hits sample", hits.slice(0, 15));
console.log(
  "miss unique",
  misses.filter((x, i, a) => a.findIndex((y) => y.dg === x.dg) === i).slice(0, 40),
);

// Prior missing scores: how many are CUT (no round played)?
const missingScore = detail.filter((r) => parseNum(r.actual_round_score) === null);
let cutLike = 0;
let fillableFromShard = 0;
const fillable = [];
for (const r of missingScore) {
  const dg = Math.round(Number(r.dg_id));
  const rnd = Math.round(Number(r.round));
  const year = 2026;
  if (!dg) continue;
  const p = join(WEB, "player-history", "by-dg", `${dg}.json`);
  if (!existsSync(p)) {
    cutLike++;
    continue;
  }
  const j = JSON.parse(readFileSync(p, "utf8"));
  const row = (j.rounds || []).find(
    (x) =>
      Math.round(Number(x.round_num)) === rnd &&
      Math.round(Number(x.year)) === year &&
      eventsLikelySame(String(x.event_name || ""), String(r.event_name || "")),
  );
  if (row && Number.isFinite(Number(row.round_score))) {
    fillableFromShard++;
    fillable.push({ p: r.player_name, ev: r.event_name, rnd, score: row.round_score, bird: row.birdies });
  } else {
    cutLike++;
  }
}
console.log({ missingScore: missingScore.length, fillableFromShard, cutLike, fillable });
