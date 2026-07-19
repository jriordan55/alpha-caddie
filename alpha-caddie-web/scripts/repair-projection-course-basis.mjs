#!/usr/bin/env node
/**
 * Venue total-score repair on projections.json:
 *   1) sanitize stale event-week anchors
 *   2) re-apply per-player venue course history (or course average)
 *   3) calibrate field total score toward venue_avg_round_score (full field when flat venue)
 *
 *   node scripts/repair-projection-course-basis.mjs
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  calibrateProjectionScoresToHistoricalVenue,
  calibrateProjectionTotalScoreToVenue,
  draftKingsDgIdsFromProjections,
  ensureProjectionCourseBasisComplete,
  flatVenuePlayerScoreAnchorEnabled,
  loadRecentVenueRoundRowsForProjections,
  populateEventWeekFieldScoreAvgs,
  reapplyProjectionTotalScoresFromVenueHistory,
  reconcileAllProjectionPlayerRows,
  sanitizeEventWeekProjectionBasis,
} from "./course-round-adjustments.mjs";
import { resolveLiveRoundActualsByDg, sanitizeLiveRoundActualsByDg } from "./dg-live-tournament-stats.mjs";
import { ensureProjectionCoursePar } from "./projection-course-par.mjs";
import { liveProjectionPipelineEnv } from "./projection-pipeline-env.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";

// Sportsbook-style defaults; honor already-set push:live / shell env.
for (const [k, v] of Object.entries(liveProjectionPipelineEnv())) {
  if (process.env[k] === undefined || String(process.env[k]).trim() === "") process.env[k] = v;
}

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB, "..");
const path = join(WEB, "projections.json");
const livePath = join(WEB, "live-in-play.json");

const proj = JSON.parse(readFileSync(path, "utf8"));
const parEnsure = ensureProjectionCoursePar(proj, { failOnMismatch: true });
if (!parEnsure.ok) {
  console.error(`[repair-projection-course-basis] FAIL: ${parEnsure.reason}`);
  process.exit(1);
}
const coursePar18 = parEnsure.coursePar18;
const eventName = String(proj.event_name || "").trim();
if (!proj.projection_course_basis || typeof proj.projection_course_basis !== "object") {
  proj.projection_course_basis = {};
}

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

if (flatVenuePlayerScoreAnchorEnabled()) {
  console.log(
    "[repair-projection-course-basis] flat venue player score: same all-time course avg R1–R4 (weather / pin / tee wave separate rounds)",
  );
}

/** Mid-week PGA + sanitized live counting only — never stub 0/0/0 hole counts. */
let extraRows = [];
try {
  let actualsByDg = {};
  if (existsSync(livePath)) {
    const live = JSON.parse(readFileSync(livePath, "utf8"));
    actualsByDg = resolveLiveRoundActualsByDg(live, {
      roundPar: coursePar18,
      fairwayHoles: Math.round(Number(proj.projection_course_basis?.fairway_holes_modeled) || 14) || 14,
    });
  }
  // Also strip any stub zeros already on projections.json before they re-enter venue means.
  if (proj.live_round_actuals_by_dg && typeof proj.live_round_actuals_by_dg === "object") {
    proj.live_round_actuals_by_dg = sanitizeLiveRoundActualsByDg(proj.live_round_actuals_by_dg);
  }
  extraRows = loadRecentVenueRoundRowsForProjections(WEB, {
    courseKey: normCourseNameKey(proj.course_used || ""),
    courseLabel: proj.course_used,
    coursePar18,
    eventName,
    actualsByDg,
  });
  if (extraRows.length) {
    console.log(
      `[repair-projection-course-basis] +${extraRows.length} recent venue round(s) (real counting only) for anchors`,
    );
  }
} catch (e) {
  console.warn("[repair-projection-course-basis] recent venue rows skipped:", e?.message || e);
}

const { touched, venueScoring } = await reapplyProjectionTotalScoresFromVenueHistory(proj, {
  repoRoot: REPO_ROOT,
  extraRows,
});
console.log(`[repair-projection-course-basis] re-applied venue player/course scores for ${touched} rows`);
if (Number.isFinite(Number(venueScoring?.venueAvgBirdies))) {
  console.log(
    `[repair-projection-course-basis] venue avg birdies=${Number(venueScoring.venueAvgBirdies).toFixed(2)} bogeys=${Number(venueScoring.venueAvgBogeys).toFixed(2)} (n=${venueScoring.nVenueRounds}, ${venueScoring.source})`,
  );
}

const cal = calibrateProjectionTotalScoreToVenue(proj, {
  minField: 8,
  venueScoring,
});
const histCal = flatVenuePlayerScoreAnchorEnabled()
  ? { rounds: 0, shifts: {} }
  : calibrateProjectionScoresToHistoricalVenue(proj, venueScoring, {
  minField: 8,
  useDkFieldFilter: String(process.env.GOLF_VENUE_CALIB_DK_FIELD_ONLY ?? "0").trim() === "1",
  dgFilter:
    String(process.env.GOLF_VENUE_CALIB_DK_FIELD_ONLY ?? "0").trim() === "1"
      ? draftKingsDgIdsFromProjections(proj)
      : null,
});
reconcileAllProjectionPlayerRows(proj, {
  minField: 8,
  venueScoring,
  skipVenueScoreCalibrate: true,
  skipHistVenueScoreCalibrate: true,
  skipMarketBookCalibration: true,
});
const after = proj.projection_course_basis.venue_avg_round_score;

if (!Number.isFinite(Number(after))) {
  console.error("[repair-projection-course-basis] FAIL: venue_avg_round_score still missing");
  process.exit(1);
}

if (!proj.meta || typeof proj.meta !== "object") proj.meta = {};
if (!proj.meta.projection_round_adjustments) proj.meta.projection_round_adjustments = {};
proj.meta.projection_round_adjustments.flat_venue_player_score = flatVenuePlayerScoreAnchorEnabled();
proj.updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
writeFileSync(path, `${JSON.stringify(proj, null, 2)}\n`);
console.log(
  `[repair-projection-course-basis] OK venue_avg_round_score ${before ?? "—"} (target); calibrated ${cal.rounds} round(s) toward course avg; field venue anchor ${histCal.rounds} round(s)`,
);
if (cal.rounds || histCal.rounds) {
  for (const [rnd, shift] of Object.entries(histCal.shifts || {})) {
    console.log(`  R${rnd}: field venue anchor shift ${shift >= 0 ? "+" : ""}${shift} (all players)`);
  }
  for (const [rnd, shift] of Object.entries(cal.shifts || {})) {
    console.log(
      `  R${rnd}: total_score shift ${shift >= 0 ? "+" : ""}${shift} (${flatVenuePlayerScoreAnchorEnabled() ? "full field" : "no-history cohort"})`,
    );
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
