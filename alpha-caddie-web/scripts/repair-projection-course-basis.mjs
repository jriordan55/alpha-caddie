#!/usr/bin/env node
/**
 * Venue total-score repair on projections.json:
 *   1) sanitize stale event-week anchors
 *   2) re-apply per-player venue course history (or course average)
 *   3) calibrate only players without venue history toward historical venue targets
 *
 *   node scripts/repair-projection-course-basis.mjs
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  calibrateProjectionTotalScoreToVenue,
  ensureProjectionCourseBasisComplete,
  populateEventWeekFieldScoreAvgs,
  reapplyProjectionTotalScoresFromVenueHistory,
  sanitizeEventWeekProjectionBasis,
} from "./course-round-adjustments.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB, "..");
const path = join(WEB, "projections.json");
const livePath = join(WEB, "live-in-play.json");

const proj = JSON.parse(readFileSync(path, "utf8"));
if (!proj.projection_course_basis || typeof proj.projection_course_basis !== "object") {
  proj.projection_course_basis = {};
}
const coursePar18 = Math.round(Number(proj.course_par_18)) || 72;
const eventName = String(proj.event_name || "").trim();

if (sanitizeEventWeekProjectionBasis(proj.projection_course_basis)) {
  console.log("[repair-projection-course-basis] cleared stale event-week field / counting anchors");
}

if (existsSync(livePath)) {
  try {
    const live = JSON.parse(readFileSync(livePath, "utf8"));
    populateEventWeekFieldScoreAvgs(proj.projection_course_basis, live, coursePar18, {
      projectionsEvent: eventName,
    });
    sanitizeEventWeekProjectionBasis(proj.projection_course_basis);
    const ew = proj.projection_course_basis.event_week_field_avg_score_by_round;
    if (ew && typeof ew === "object") {
      console.log("[repair-projection-course-basis] event-week field avg by round:", ew);
    }
  } catch {
    /* keep historical venue target */
  }
}

const before = proj.projection_course_basis.venue_avg_round_score;
ensureProjectionCourseBasisComplete(proj.projection_course_basis, proj);

const { touched, venueScoring } = await reapplyProjectionTotalScoresFromVenueHistory(proj, {
  repoRoot: REPO_ROOT,
});
console.log(`[repair-projection-course-basis] re-applied venue player/course scores for ${touched} rows`);

const cal = calibrateProjectionTotalScoreToVenue(proj, {
  minField: 8,
  venueScoring,
});
const after = proj.projection_course_basis.venue_avg_round_score;

if (!Number.isFinite(Number(after))) {
  console.error("[repair-projection-course-basis] FAIL: venue_avg_round_score still missing");
  process.exit(1);
}

proj.updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
writeFileSync(path, `${JSON.stringify(proj, null, 2)}\n`);
console.log(
  `[repair-projection-course-basis] OK venue_avg_round_score ${before ?? "—"} (target); calibrated ${cal.rounds} round(s) for non-venue-history players`,
);
if (cal.rounds) {
  for (const [rnd, shift] of Object.entries(cal.shifts)) {
    console.log(`  R${rnd}: total_score shift ${shift >= 0 ? "+" : ""}${shift} (no-history cohort only)`);
  }
}

const sc = proj.players?.find(
  (p) => String(p.player_name || "").includes("Scheffler") && p.round === 1,
);
if (sc) {
  console.log(
    `[repair-projection-course-basis] Scheffler R1: total=${sc.total_score} source=${sc.score_source || "?"}`,
  );
}
