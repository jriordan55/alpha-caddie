#!/usr/bin/env node
/**
 * Guards Round Projections "At this course" averages against Course Fit history.
 * Harman @ Renaissance bogeys should be ~2.4–3.0 (not last-4 birdies ~5.0).
 *
 *   npm run verify:ou-proj-avg
 */
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");

function fail(msg) {
  console.error(`[verify:ou-proj-avg] FAIL: ${msg}`);
  process.exit(1);
}

function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

function normCourseNameKey(raw) {
  let s = String(raw || "").trim().toLowerCase();
  s = s.replace(/\([^)]*\)/g, " ");
  s = s.replace(/\bthe players\b/gi, " ");
  s = s.replace(/[^a-z0-9]+/g, " ");
  s = s.replace(/\s+/g, " ").trim();
  return s;
}

function historyRoundChronoKey(r) {
  const sk = num(r?.sortKey, NaN);
  if (Number.isFinite(sk) && sk > 0) return sk;
  return 0;
}

function actualBogeys(row) {
  return num(row?.bogeys ?? row?.bogies, NaN);
}

function actualBirdies(row) {
  return num(row?.birdies, NaN);
}

const playerShard = JSON.parse(readFileSync(join(WEB, "player-history/by-dg/8825.json"), "utf8"));
const courseShard = JSON.parse(
  readFileSync(join(WEB, "player-history/by-course/the-renaissance-club.json"), "utf8"),
);

const venueKey = normCourseNameKey("The Renaissance Club");
const harmanId = 8825;

const fromPlayer = (playerShard.rounds || [])
  .filter((r) => normCourseNameKey(r.course_name) === venueKey)
  .map(actualBogeys)
  .filter(Number.isFinite);
const fromCourse = (courseShard.entries || [])
  .filter((e) => num(e.dg_id ?? e.dgId, NaN) === harmanId)
  .map((e) => actualBogeys(e.row))
  .filter(Number.isFinite);

const allPlayer = [...(playerShard.rounds || [])]
  .sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a));
const last4Birdies = allPlayer
  .slice(0, 4)
  .map(actualBirdies)
  .filter(Number.isFinite);

const mean = (xs) => (xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : NaN);

const playerCourseMean = mean(fromPlayer);
const shardCourseMean = mean(fromCourse);
const last4BirdMean = mean(last4Birdies);

if (!Number.isFinite(shardCourseMean)) fail("no Harman bogeys in Renaissance course shard");
if (shardCourseMean < 2.2 || shardCourseMean > 3.4) {
  fail(`Renaissance shard bogeys mean out of range: ${shardCourseMean.toFixed(2)}`);
}
if (Math.abs(shardCourseMean - last4BirdMean) < 0.25) {
  fail(`course bogeys mean (${shardCourseMean}) equals last-4 birdies (${last4BirdMean}) — wrong stat/window`);
}
if (fromCourse.length < 10) {
  fail(`expected >=10 Harman Renaissance rounds in course shard, got ${fromCourse.length}`);
}

const appSrc = readFileSync(join(WEB, "app.js"), "utf8");
for (const needle of [
  "ouPlayerMarketCourseShardRounds",
  "courseFitDistRoundCountsAsActual(row)",
  "ensurePropsCourseIndexForKeyAsync(vk)",
]) {
  if (!appSrc.includes(needle)) fail(`app.js missing ${needle}`);
}

console.log(
  `[verify:ou-proj-avg] OK — Harman Renaissance bogeys: shard=${shardCourseMean.toFixed(2)} (n=${fromCourse.length}), player=${playerCourseMean.toFixed(2)} (n=${fromPlayer.length}), last4 birdies=${last4BirdMean.toFixed(2)}`,
);
