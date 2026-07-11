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

function historyRoundSeasonYear(r) {
  const y = num(r?.year, NaN);
  if (Number.isFinite(y) && y >= 1990 && y <= 2100) return Math.round(y);
  const ec = String(r?.event_completed || "").trim();
  const m = ec.match(/(\d{4})/);
  if (m) {
    const yy = parseInt(m[1], 10);
    if (yy >= 1990 && yy <= 2100) return yy;
  }
  return NaN;
}

function girFairwaysCountFromRawForOu(v, holes) {
  const n = num(v, NaN);
  if (!Number.isFinite(n)) return NaN;
  if (n > 0 && n <= 1.0001) return Math.min(holes, Math.max(0, Math.round(n * holes)));
  if (n > 1.0001 && n <= holes + 1e-6) return Math.min(holes, Math.max(0, n));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

function actualBirdiesForAvg(row) {
  const b = num(row?.birdies, NaN);
  if (Number.isFinite(b)) return b + Math.max(0, num(row?.eagles_or_better, 0));
  return NaN;
}

function historyRowHasStoredCountingStat(row, key) {
  if (!row || typeof row !== "object") return false;
  const v = row[key];
  if (v == null || v === "") return false;
  const n = Number(v);
  if (!Number.isFinite(n)) return false;
  if ((key === "gir" || key === "fairways" || key === "putts") && (n === 0 || n === 1)) return false;
  if (
    (key === "birdies" || key === "pars" || key === "bogies" || key === "bogeys") &&
    n === 0 &&
    row._from_live_tournament_stats &&
    !row._from_pgatour
  ) {
    return false;
  }
  return true;
}

function ouMarketAvgValueForRoundRow(statKey, row) {
  const v = statKey === "birdies" ? actualBirdiesForAvg(row) : statKey === "bogeys" ? actualBogeys(row) : NaN;
  if (!Number.isFinite(v)) return NaN;
  if (statKey === "birdies") {
    if (
      historyRowHasStoredCountingStat(row, "birdies") ||
      historyRowHasStoredCountingStat(row, "eagles_or_better") ||
      historyRowHasStoredCountingStat(row, "eagles")
    ) {
      return v;
    }
    return NaN;
  }
  if (statKey === "bogeys") {
    if (historyRowHasStoredCountingStat(row, "bogeys") || historyRowHasStoredCountingStat(row, "bogies")) return v;
    return NaN;
  }
  return v;
}

function actualGirFromHistoryRow(row) {
  let v = girFairwaysCountFromRawForOu(row?.gir, 18);
  if (!Number.isFinite(v) || v === 0 || v === 1) return NaN;
  return v;
}

function mean(xs) {
  return xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : NaN;
}

const appSrc = readFileSync(join(WEB, "app.js"), "utf8");
for (const needle of [
  "ouPlayerMarketCourseShardRounds",
  "ouPlayerMarketAvgHistoryStillLoading",
  "ouMarketAvgValueForRoundRow",
  "ouFieldHistoryReadyForAverages()",
  "courseFitDistRoundCountsAsActual(row)",
  "ensurePropsCourseIndexForKeyAsync(vk)",
  "ouModelMarketKey(col?.market)",
]) {
  if (!appSrc.includes(needle)) fail(`app.js missing ${needle}`);
}
const avgFnIdx = appSrc.indexOf("function ouPlayerMarketAverage");
const avgFnBody = avgFnIdx >= 0 ? appSrc.slice(avgFnIdx, avgFnIdx + 1200) : "";
if (avgFnBody.includes("ouPlayerModelAvgForMarket")) {
  fail("ouPlayerMarketAverage must not fall back to model/skill averages");
}
if (avgFnBody.includes('source: "model"')) {
  fail('ouPlayerMarketAverage must not emit source: "model"');
}
if (!avgFnBody.includes("ouMarketAvgValueForRoundRow")) {
  fail("ouPlayerMarketAverage must use ouMarketAvgValueForRoundRow for raw posted stats");
}
const avgValFnIdx = appSrc.indexOf("function ouMarketAvgValueForRoundRow");
const avgValFnBody = avgValFnIdx >= 0 ? appSrc.slice(avgValFnIdx, avgValFnIdx + 900) : "";
if (!avgValFnBody.includes("enrichHistoryRowFromLiveActuals(row)")) {
  fail("ouMarketAvgValueForRoundRow must enrich rows before actualForRoundRow (GIR/FW live fill)");
}
if (avgValFnBody.includes('historyRowHasStoredCountingStat(row, "gir")')) {
  fail("ouMarketAvgValueForRoundRow must not gate GIR on raw row when enrich supplies live actuals");
}
const ratingFnIdx = appSrc.indexOf("function ouPlayerAvgForMarketRating");
const ratingFnBody = ratingFnIdx >= 0 ? appSrc.slice(ratingFnIdx, ratingFnIdx + 500) : "";
if (ratingFnBody.includes("ouPlayerModelAvgForMarket")) {
  fail("ouPlayerAvgForMarketRating must not use model/skill averages (projections must stay independent)");
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
    // Original bug: course bogeys column showed last-4 birdies (~5.0), not venue bogeys (~2.6).
    const swappedStatBug =
      last4BirdMean >= 4.0 &&
      shardCourseMean >= 4.0 &&
      Math.abs(shardCourseMean - last4BirdMean) < 0.35;
    if (swappedStatBug) {
      fail(
        `course bogeys mean (${shardCourseMean.toFixed(2)}) matches last-4 birdies (${last4BirdMean.toFixed(2)}) — wrong stat/window`,
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

// Season GIR average must come from history, not dg_gir_pct×18 (Hovland dg_gir_pct=0.944 → bogus ~17).
const hovlandId = 18841;
const seasonYear = num(
  String(proj?.datagolf_field_date_start || proj?.meta?.datagolf_field_date_start || "").slice(0, 4),
  2026,
);
const hovlandShardPath = join(WEB, `player-history/by-dg/${hovlandId}.json`);
if (existsSync(hovlandShardPath)) {
  const hovlandShard = JSON.parse(readFileSync(hovlandShardPath, "utf8"));
  const seasonGir = (hovlandShard.rounds || [])
    .filter((r) => historyRoundSeasonYear(r) === seasonYear)
    .map(actualGirFromHistoryRow)
    .filter(Number.isFinite);
  const seasonGirMean = mean(seasonGir);
  const hovlandProj = players.find(
    (p) => Math.round(num(p.dg_id, NaN)) === hovlandId && Math.round(num(p.round, 1)) === dr,
  );
  const bogusDgPct = num(hovlandProj?.dg_gir_pct, NaN) * 18;
  if (!Number.isFinite(seasonGirMean) || seasonGir.length < 20) {
    fail(`Hovland ${seasonYear} season GIR history too thin (n=${seasonGir.length})`);
  }
  if (seasonGirMean < 10.5 || seasonGirMean > 14.5) {
    fail(`Hovland ${seasonYear} season GIR mean out of range: ${seasonGirMean.toFixed(2)} (expected ~12)`);
  }
  if (Number.isFinite(bogusDgPct) && bogusDgPct > 15.5 && Math.abs(bogusDgPct - seasonGirMean) < 1.5) {
    fail(
      `Hovland dg_gir_pct×18 (${bogusDgPct.toFixed(2)}) too close to real season GIR (${seasonGirMean.toFixed(2)}) — average column must use history only`,
    );
  }
}

// Field season birdies averages must be real history (~3–5), not bogus live-placeholder zeros.
const taylorId = 13126;
const taylorShardPath = join(WEB, `player-history/by-dg/${taylorId}.json`);
if (existsSync(taylorShardPath)) {
  const taylorShard = JSON.parse(readFileSync(taylorShardPath, "utf8"));
  const seasonBirdies = (taylorShard.rounds || [])
    .filter((r) => historyRoundSeasonYear(r) === seasonYear)
    .map((r) => ouMarketAvgValueForRoundRow("birdies", r))
    .filter(Number.isFinite);
  const seasonBirdMean = mean(seasonBirdies);
  if (!Number.isFinite(seasonBirdMean) || seasonBirdies.length < 20) {
    fail(`Taylor ${seasonYear} season birdies history too thin (n=${seasonBirdies.length})`);
  }
  if (seasonBirdMean < 2.5 || seasonBirdMean > 5.5) {
    fail(`Taylor ${seasonYear} season birdies mean out of range: ${seasonBirdMean.toFixed(2)}`);
  }
  if (seasonBirdMean < 0.05) {
    fail(`Taylor ${seasonYear} season birdies mean is ~0 — live placeholder rows leaking into averages`);
  }
}

// Field player with a posted GIR line must have a season GIR average from history (not blank).
const girProps = props.filter(
  (r) =>
    String(r.market || "").trim() === "GIR" &&
    fieldIds.has(Math.round(num(r.dg_id, NaN))),
);
if (girProps.length >= 3) {
  const sampleId = Math.round(num(girProps[0].dg_id, NaN));
  const sampleShardPath = join(WEB, `player-history/by-dg/${sampleId}.json`);
  if (existsSync(sampleShardPath)) {
    const sampleShard = JSON.parse(readFileSync(sampleShardPath, "utf8"));
    const seasonGir = (sampleShard.rounds || [])
      .filter((r) => historyRoundSeasonYear(r) === seasonYear)
      .map(actualGirFromHistoryRow)
      .filter(Number.isFinite);
    const seasonGirMean = mean(seasonGir);
    if (!Number.isFinite(seasonGirMean) || seasonGir.length < 10) {
      fail(
        `field GIR line player ${sampleId} ${seasonYear} season GIR history too thin (n=${seasonGir.length}) — Average column would show —`,
      );
    }
    if (seasonGirMean < 8 || seasonGirMean > 16) {
      fail(`field GIR line player ${sampleId} season GIR mean out of range: ${seasonGirMean.toFixed(2)}`);
    }
  }
}

console.log(
  `[verify:ou-proj-avg] OK — course shard for ${venueRaw} (${courseShard.entries.length} entries); app.js course-average path wired`,
);
