#!/usr/bin/env node
/**
 * Index Betsperts course facts + year metrics + hole stats for Plus4 / market projections.
 *
 * Reads:  ../data/betsperts/courses/by-id/*.json
 * Writes: data/betsperts_course_facts.json (+ root mirror)
 *         data/betsperts/courses/hole-stats/{id}.json
 *         merges grass / correlated / hole_stats into data/course_event_context.json
 *
 *   node scripts/build-betsperts-course-context.mjs
 *   npm run build:betsperts-course
 */
import { existsSync, mkdirSync, readFileSync, readdirSync, renameSync, unlinkSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import {
  buildBetspertsCourseFactsIndex,
  BETSPERTS_COURSE_FACTS_WEB,
  BETSPERTS_COURSE_FACTS_ROOT,
  BETSPERTS_COURSES_DIR,
  normalizeHoleStatsRows,
} from "./betsperts-course-context.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const ROOT = join(WEB, "..");
const CONTEXT_PATH = join(WEB, "data", "course_event_context.json");
const HOLE_STATS_DIR = join(ROOT, "data", "betsperts", "courses", "hole-stats");

function writeJsonAtomic(filePath, obj) {
  mkdirSync(dirname(filePath), { recursive: true });
  const text = JSON.stringify(obj, null, 2) + "\n";
  const tmp = `${filePath}.${process.pid}.tmp`;
  writeFileSync(tmp, text, "utf8");
  try {
    renameSync(tmp, filePath);
  } catch {
    writeFileSync(filePath, text, "utf8");
    try {
      unlinkSync(tmp);
    } catch {
      /* ignore */
    }
  }
}

function mergeGrassToken(existing, incoming) {
  const set = new Set(
    [...(Array.isArray(existing) ? existing : []), ...(Array.isArray(incoming) ? incoming : [])]
      .map((s) => String(s || "").trim())
      .filter(Boolean),
  );
  return [...set];
}

const index = buildBetspertsCourseFactsIndex();
writeJsonAtomic(BETSPERTS_COURSE_FACTS_WEB, index);
writeJsonAtomic(BETSPERTS_COURSE_FACTS_ROOT, index);
console.log(
  `[betsperts-course] indexed ${index.n} courses (${index.n_with_metrics} metrics, ${index.n_with_hole_stats} hole-stats) → data/betsperts_course_facts.json`,
);

// Write / refresh Hole Stats sidecars from by-id (screenshot schema).
mkdirSync(HOLE_STATS_DIR, { recursive: true });
let holeSidecars = 0;
if (existsSync(BETSPERTS_COURSES_DIR)) {
  for (const f of readdirSync(BETSPERTS_COURSES_DIR).filter((x) => x.endsWith(".json"))) {
    let payload;
    try {
      payload = JSON.parse(readFileSync(join(BETSPERTS_COURSES_DIR, f), "utf8"));
    } catch {
      continue;
    }
    const num = String(payload.courseNum || f.replace(/\.json$/i, ""));
    const holes =
      Array.isArray(payload.holeStats) && payload.holeStats.length
        ? payload.holeStats
        : normalizeHoleStatsRows(payload.auth?.holeBreakdown);
    writeJsonAtomic(join(HOLE_STATS_DIR, `${num}.json`), {
      courseNum: num,
      courseName: payload.courseName || payload.details?.CourseName || null,
      pulledAt: payload.auth?._meta?.pulledAt || payload._meta?.pulledAt || null,
      available: holes.length >= 18,
      holes,
    });
    // Persist normalized holeStats back onto by-id when missing.
    if ((!Array.isArray(payload.holeStats) || !payload.holeStats.length) && holes.length) {
      payload.holeStats = holes;
      payload.holeStatsAvailable = holes.length >= 18;
      writeJsonAtomic(join(BETSPERTS_COURSES_DIR, f), payload);
    }
    holeSidecars++;
  }
}
console.log(`[betsperts-course] wrote ${holeSidecars} hole-stats sidecars → data/betsperts/courses/hole-stats/`);

let ctx = { by_course: {} };
if (existsSync(CONTEXT_PATH)) {
  try {
    ctx = JSON.parse(readFileSync(CONTEXT_PATH, "utf8"));
  } catch {
    ctx = { by_course: {} };
  }
}
if (!ctx.by_course || typeof ctx.by_course !== "object") ctx.by_course = {};

let merged = 0;
for (const [key, rec] of Object.entries(index.byKey || {})) {
  if (!key || !rec) continue;
  const prev = ctx.by_course[key] && typeof ctx.by_course[key] === "object" ? ctx.by_course[key] : {};
  const grass = {
    ...(prev.grass && typeof prev.grass === "object" ? prev.grass : {}),
    greens: mergeGrassToken(prev.grass?.greens, rec.grass?.greens),
    fairways: mergeGrassToken(prev.grass?.fairways, rec.grass?.fairways),
    rough: mergeGrassToken(prev.grass?.rough, rec.grass?.rough),
    surrounds: mergeGrassToken(prev.grass?.surrounds, rec.grass?.surrounds),
  };
  if (Number.isFinite(rec.grass?.green_stimp)) grass.green_stimp = rec.grass.green_stimp;
  if (Number.isFinite(rec.grass?.green_size_sqft)) grass.green_size_sqft = rec.grass.green_size_sqft;
  if (Number.isFinite(rec.grass?.rough_length_in)) grass.rough_length_in = rec.grass.rough_length_in;

  ctx.by_course[key] = {
    ...prev,
    grass,
    betsperts: {
      course_num: rec.course_num,
      name: rec.name,
      locations: rec.locations,
      course_types: rec.course_types,
      par: rec.par,
      yards: rec.yards,
      difficulty: rec.difficulty,
      score_rtp: rec.score_rtp,
      skill_labels: rec.skill_labels,
      rates: rec.rates,
      metric_summary: rec.metric_summary,
      correlated: (rec.correlated || []).map((c) => c.name || c.key).filter(Boolean),
      hole_stats: rec.hole_stats || [],
      hole_stats_n: rec.hole_stats_n || 0,
      has_hole_stats: Boolean(rec.has_hole_stats),
      pulled_at: rec.pulled_at,
      has_metrics: rec.has_metrics,
    },
  };
  merged++;
}

ctx.betsperts_merged_at = new Date().toISOString();
ctx.betsperts_courses_merged = merged;
writeJsonAtomic(CONTEXT_PATH, ctx);
console.log(`[betsperts-course] merged ${merged} courses into data/course_event_context.json`);
