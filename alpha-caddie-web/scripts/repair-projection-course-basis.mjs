#!/usr/bin/env node
/**
 * Venue total-score calibration on projections.json (full field per round).
 * Does not re-run counting-market calibration — use bake:weather after for forecast wind.
 *   node scripts/repair-projection-course-basis.mjs
 */
import { readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import {
  calibrateProjectionTotalScoreToVenue,
  ensureProjectionCourseBasisComplete,
} from "./course-round-adjustments.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const path = join(WEB, "projections.json");

const proj = JSON.parse(readFileSync(path, "utf8"));
if (!proj.projection_course_basis || typeof proj.projection_course_basis !== "object") {
  proj.projection_course_basis = {};
}
const before = proj.projection_course_basis.venue_avg_round_score;
ensureProjectionCourseBasisComplete(proj.projection_course_basis, proj);
const cal = calibrateProjectionTotalScoreToVenue(proj, { minField: 8 });
const after = proj.projection_course_basis.venue_avg_round_score;

if (!Number.isFinite(Number(after))) {
  console.error("[repair-projection-course-basis] FAIL: venue_avg_round_score still missing");
  process.exit(1);
}

writeFileSync(path, `${JSON.stringify(proj, null, 2)}\n`);
console.log(
  `[repair-projection-course-basis] OK venue_avg_round_score ${before ?? "—"} (target); calibrated ${cal.rounds} round(s)`,
);
if (cal.rounds) {
  for (const [rnd, shift] of Object.entries(cal.shifts)) {
    console.log(`  R${rnd}: total_score shift ${shift >= 0 ? "+" : ""}${shift}`);
  }
}
