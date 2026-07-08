#!/usr/bin/env node
/**
 * Recompute `pga_tour_market_benchmarks` on projections.json from data/historical_rounds_all.csv.
 * Called at end of refresh:live (after post-live CSV merge) so Market rating uses fresh 2025–2026 rounds.
 *
 *   npm run refresh:market-benchmarks
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  loadPgaTourCourseBenchmarks,
  loadPgaTourMarketBenchmarks,
  serializePgaTourCourseBenchmarks,
  serializePgaTourMarketBenchmarks,
} from "./pga-tour-market-benchmarks.mjs";
import { loadVenueHistoricalScoring } from "./course-round-adjustments.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

async function main() {
  if (!existsSync(projPath)) {
    console.warn("[refresh:market-benchmarks] missing projections.json — skip");
    process.exit(0);
  }
  const modelRoot = process.env.GOLF_MODEL_DIR?.trim()
    ? resolve(process.env.GOLF_MODEL_DIR.trim())
    : resolve(WEB_ROOT, "..");
  const minYear = Math.round(num(process.env.GOLF_PGA_TOUR_BENCHMARK_MIN_YEAR, 2025)) || 2025;
  const maxYear = Math.round(num(process.env.GOLF_PGA_TOUR_BENCHMARK_MAX_YEAR, 2026)) || 2026;

  let proj;
  try {
    proj = JSON.parse(readFileSync(projPath, "utf8"));
  } catch (e) {
    console.warn("[refresh:market-benchmarks] parse error —", e.message || e);
    process.exit(0);
  }

  const benchOpts = { minYear, maxYear };
  const [raw, rawCourse] = await Promise.all([
    loadPgaTourMarketBenchmarks(modelRoot, benchOpts),
    loadPgaTourCourseBenchmarks(modelRoot, { recentYears: 12 }),
  ]);
  proj.pga_tour_market_benchmarks = serializePgaTourMarketBenchmarks(raw);
  proj.pga_tour_course_benchmarks = serializePgaTourCourseBenchmarks(rawCourse);

  const courseUsed = String(proj.course_used || proj.meta?.course_used || "").trim();
  const courseKey = normCourseNameKey(courseUsed);
  const histCsv = join(modelRoot, "data", "historical_rounds_all.csv");
  if (courseKey && existsSync(histCsv)) {
    const venue = await loadVenueHistoricalScoring(histCsv, courseKey, courseUsed, {});
    const basis = proj.projection_course_basis && typeof proj.projection_course_basis === "object"
      ? proj.projection_course_basis
      : {};
    if (Number.isFinite(venue.venueAvgStp)) basis.venue_avg_score_to_par = Math.round(venue.venueAvgStp * 1000) / 1000;
    if (Number.isFinite(venue.venueAvgBirdies)) {
      basis.historical_venue_avg_birdies = Math.round(venue.venueAvgBirdies * 1000) / 1000;
    }
    if (Number.isFinite(venue.venueAvgBogeys)) {
      basis.historical_venue_avg_bogeys = Math.round(venue.venueAvgBogeys * 1000) / 1000;
    }
    if (Number.isFinite(venue.venueAvgGir)) {
      basis.historical_venue_avg_gir = Math.round(venue.venueAvgGir * 1000) / 1000;
    }
    if (Number.isFinite(venue.venueAvgFairways)) {
      basis.historical_venue_avg_fairways = Math.round(venue.venueAvgFairways * 1000) / 1000;
    }
    if (Number.isFinite(venue.venueAvgScrambling)) {
      basis.historical_venue_avg_scrambling = Math.round(venue.venueAvgScrambling * 10000) / 10000;
    }
    proj.projection_course_basis = basis;
  }

  writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`, "utf8");

  const meta = raw.meta || {};
  const b = proj.pga_tour_market_benchmarks?.["Total score"];
  if (meta.skipped) {
    console.warn("[refresh:market-benchmarks] skipped (no CSV or empty sample)");
  } else {
    console.log(
      `[refresh:market-benchmarks] ${meta.min_year}–${meta.max_year} score μ=${b?.mean} σ=${b?.sd} (n=${meta.n?.score ?? "?"})`,
    );
  }
  const cm = rawCourse.meta || {};
  const cb = proj.pga_tour_course_benchmarks?.Birdies;
  if (!cm.skipped && cb?.mean != null) {
    console.log(
      `[refresh:market-benchmarks] course birdies μ=${cb.mean} σ=${cb.sd} (venues n=${cm.n_courses?.birdies ?? "?"})`,
    );
  }
  process.exit(0);
}

main().catch((e) => {
  console.error("[refresh:market-benchmarks] fatal:", e.message || e);
  process.exit(1);
});
