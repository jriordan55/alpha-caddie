#!/usr/bin/env node
/**
 * Fast path: build player-history/by-course/*.json from existing player_round_history.json
 * (no CSV re-scan). Run: npm run build:course-shards
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey, courseShardFileName } from "./course-name-key.mjs";
import { historyRoundChartUtcIsoDay } from "./history-round-dates.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const HISTORY_JSON = path.join(WEB_ROOT, "player_round_history.json");
const COURSE_SHARD_DIR = path.join(WEB_ROOT, "player-history", "by-course");
const COURSES_MANIFEST_JSON = path.join(WEB_ROOT, "player-history", "courses-manifest.json");

function chartUtcIsoDayFromHistoryRow(r) {
  return historyRoundChartUtcIsoDay(r);
}

function writeJsonAtomic(outPath, payload) {
  const tmpPath = `${outPath}.tmp`;
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(tmpPath, JSON.stringify(payload), "utf8");
  fs.renameSync(tmpPath, outPath);
}

function main() {
  if (!fs.existsSync(HISTORY_JSON)) {
    console.error("Missing", HISTORY_JSON, "— run npm run build:history first.");
    process.exit(1);
  }
  const out = JSON.parse(fs.readFileSync(HISTORY_JSON, "utf8"));
  const byCourse = new Map();
  for (const [dgId, bucket] of Object.entries(out.byDgId || {})) {
    const dg = Math.round(Number(dgId));
    if (!Number.isFinite(dg) || !bucket?.rounds) continue;
    const playerName = String(bucket.player_name || "").trim();
    for (const r of bucket.rounds) {
      const rs = Number(r.round_score);
      if (!Number.isFinite(rs) || rs <= 0) continue;
      const ck = normCourseNameKey(r.course_name);
      if (!ck) continue;
      let b = byCourse.get(ck);
      if (!b) {
        b = { dateSet: new Set(), entries: [] };
        byCourse.set(ck, b);
      }
      b.entries.push({ dg_id: dg, player_name: playerName, row: r });
      const iso = chartUtcIsoDayFromHistoryRow(r);
      if (iso) b.dateSet.add(iso);
    }
  }
  fs.mkdirSync(COURSE_SHARD_DIR, { recursive: true });
  const keep = new Set();
  const courses = [];
  for (const [courseKey, b] of byCourse) {
    const file = courseShardFileName(courseKey);
    keep.add(file);
    const days = [...b.dateSet].sort((a, c) => c.localeCompare(a));
    writeJsonAtomic(path.join(COURSE_SHARD_DIR, file), { course_key: courseKey, days, entries: b.entries });
    courses.push({ course_key: courseKey, file, days: days.length, entries: b.entries.length });
  }
  for (const entry of fs.readdirSync(COURSE_SHARD_DIR)) {
    if (entry.endsWith(".json") && !keep.has(entry)) {
      fs.unlinkSync(path.join(COURSE_SHARD_DIR, entry));
    }
  }
  courses.sort((a, b) => a.course_key.localeCompare(b.course_key));
  writeJsonAtomic(COURSES_MANIFEST_JSON, {
    meta: { updated_at: new Date().toISOString(), source: "player_round_history.json" },
    courses,
  });
  const tpc = courses.find((c) => c.course_key.includes("craig") || c.file.includes("craig"));
  console.log("Wrote", courses.length, "course shard(s) ->", COURSE_SHARD_DIR);
  if (tpc) console.log("TPC Craig Ranch shard:", tpc.file, "entries:", tpc.entries);
}

main();
