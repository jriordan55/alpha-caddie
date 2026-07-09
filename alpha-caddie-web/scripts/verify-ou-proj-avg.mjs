#!/usr/bin/env node
/**
 * Guards Round Projections "At this course" averages against Course Fit history.
 * Blocks publish when course shards are missing or app.js drops the course-shard path.
 *
 *   npm run verify:ou-proj-avg
 */
import { existsSync, readFileSync } from "fs";
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

function courseShardFileName(courseKey) {
  const safe = String(courseKey || "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 96);
  return `${safe || "unknown"}.json`;
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

function mean(xs) {
  return xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : NaN;
}

const appSrc = readFileSync(join(WEB, "app.js"), "utf8");
for (const needle of [
  "ouPlayerMarketCourseShardRounds",
  "courseFitDistRoundCountsAsActual(row)",
  "ensurePropsCourseIndexForKeyAsync(vk)",
  "ouModelMarketKey(col?.market)",
]) {
  if (!appSrc.includes(needle)) fail(`app.js missing ${needle}`);
}

let proj;
try {
  proj = JSON.parse(readFileSync(join(WEB, "projections.json"), "utf8"));
} catch (e) {
  fail(`invalid projections.json: ${e.message || e}`);
}

const venueRaw = String(proj?.meta?.course_used || proj?.course_used || "").trim();
if (!venueRaw) fail("projections.json missing meta.course_used");
const venueKey = normCourseNameKey(venueRaw);
const shardPath = join(WEB, "player-history/by-course", courseShardFileName(venueKey));
if (!existsSync(shardPath)) {
  fail(`missing course shard for ${venueRaw} (${shardPath}) — run build:course-shards on push:live`);
}

const courseShard = JSON.parse(readFileSync(shardPath, "utf8"));
if (!Array.isArray(courseShard.entries) || courseShard.entries.length < 50) {
  fail(`course shard ${courseShardFileName(venueKey)} has too few entries (${courseShard.entries?.length ?? 0})`);
}

// Canary: Brian Harman @ Renaissance — course bogeys must not equal last-4 birdies (~5.0 bug).
const harmanId = 8825;
const renaissanceKey = normCourseNameKey("The Renaissance Club");
const renaissanceShard = join(WEB, "player-history/by-course", courseShardFileName(renaissanceKey));
if (existsSync(renaissanceShard)) {
  const shard = JSON.parse(readFileSync(renaissanceShard, "utf8"));
  const fromCourse = (shard.entries || [])
    .filter((e) => num(e.dg_id ?? e.dgId, NaN) === harmanId)
    .map((e) => actualBogeys(e.row))
    .filter(Number.isFinite);
  const playerShardPath = join(WEB, "player-history/by-dg/8825.json");
  if (existsSync(playerShardPath)) {
    const playerShard = JSON.parse(readFileSync(playerShardPath, "utf8"));
    const last4Birdies = [...(playerShard.rounds || [])]
      .sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a))
      .slice(0, 4)
      .map(actualBirdies)
      .filter(Number.isFinite);
    const shardCourseMean = mean(fromCourse);
    const last4BirdMean = mean(last4Birdies);
    if (!Number.isFinite(shardCourseMean)) fail("no Harman bogeys in Renaissance course shard");
    if (shardCourseMean < 2.2 || shardCourseMean > 3.4) {
      fail(`Renaissance shard bogeys mean out of range: ${shardCourseMean.toFixed(2)}`);
    }
    if (Math.abs(shardCourseMean - last4BirdMean) < 0.25) {
      fail(
        `course bogeys mean (${shardCourseMean.toFixed(2)}) equals last-4 birdies (${last4BirdMean.toFixed(2)}) — wrong stat/window`,
      );
    }
    if (fromCourse.length < 10) {
      fail(`expected >=10 Harman Renaissance rounds in course shard, got ${fromCourse.length}`);
    }
  }
}

// Active venue: at least one field player with a posted DK/PP bogeys line should have course history.
const players = Array.isArray(proj.players) ? proj.players : [];
const props = Array.isArray(proj.props) ? proj.props : [];
const dr = Math.round(num(proj.display_round ?? proj.meta?.display_round, 1)) || 1;
const fieldIds = new Set(
  players.filter((p) => Math.round(num(p.round, NaN)) === dr).map((p) => Math.round(num(p.dg_id, NaN))),
);
const bogeysProps = props.filter(
  (r) =>
    String(r.market || "").trim() === "Bogeys" &&
    Math.round(num(r.round_num, dr)) === dr &&
    fieldIds.has(Math.round(num(r.dg_id, NaN))),
);
if (bogeysProps.length >= 5) {
  const sampleId = Math.round(num(bogeysProps[0].dg_id, NaN));
  const nAtVenue = (courseShard.entries || []).filter(
    (e) => num(e.dg_id ?? e.dgId, NaN) === sampleId && actualBogeys(e.row) >= 0,
  ).length;
  if (nAtVenue < 2) {
    fail(`field player ${sampleId} has DK bogeys line but only ${nAtVenue} rounds at ${venueRaw} in course shard`);
  }
}

console.log(
  `[verify:ou-proj-avg] OK — course shard for ${venueRaw} (${courseShard.entries.length} entries); app.js course-average path wired`,
);
