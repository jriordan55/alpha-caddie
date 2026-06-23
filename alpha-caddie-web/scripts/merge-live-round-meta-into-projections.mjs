#!/usr/bin/env node
/**
 * npm run merge:live-round-meta-into-projections (push:all, after merge:live-hole-pars)
 *
 * fetch:dg runs before fetch:in-play; preds/live-hole-stats at that moment may still show R1
 * while live-in-play bundles fresher live_hole_stats + field_updates after rounds complete.
 *
 * Refresh top-level tournament round (`display_round`, `datagolf_field_current_round`) and the
 * prior-round course difficulty strokes applied to mu_sg — without re-running fetch:dg (would
 * clobber fetch:book-odds / finish-tool merges). Applies the same blending as fetch-datagolf:
 * blendedPriorRoundCourseExcess live_hole_stats + historical_rounds_all.csv.
 *
 * Round label: max(field_updates.current_round, live_hole_stats current_round / info, preds/in-play
 * meta + player rows `round`). Projections retain the **full tournament field**; MC/WD are hidden only
 * where the web app gates post-cut markets (O/U/+EV/etc.), not in Historical Trends field-by-course.
 *
 * Env: GOLF_MODEL_DIR → repo root (parent of alpha-caddie-web). Uses data/historical_rounds_all.csv
 * when present. GOLF_COURSE_PRIOR_ROUND_DIFFICULTY=0 skips mu adjustments (still updates rounds).
 * GOLF_MERGE_LIVE_ROUND_META_IGNORE_WEEK_KEY=1 — run merge even when field week key does not match projections.
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  blendedPriorRoundCourseExcess,
  courseDifficultyStrokeShift,
  flatVenuePlayerScoreAnchorEnabled,
  loadEventRoundContextFromHistoricalCsv,
} from "./course-round-adjustments.mjs";
import { eventsLikelySame, fieldWeekKey, fieldWeekKeysRoughMatch } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  dateStartIsFuture,
  exportDisplayRoundFromLiveBundle,
  num,
  projectionDateStartIso,
} from "./dg-display-round-from-bundle.mjs";
import { resolveLiveRoundActualsByDg } from "./dg-live-tournament-stats.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function displayRoundLabel(r, tz) {
  const lab =
    r === 1 ? "R1 — next Thursday" : r === 2 ? "R2 — Friday" : r === 3 ? "R3 — Saturday" : r === 4 ? "R4 — Sunday" : `R${r}`;
  return `${lab} (auto, ${tz})`;
}

function attachLiveRoundActualsToProjections(proj, live) {
  const fwHoles = Math.round(num(proj?.projection_course_basis?.fairway_holes_modeled, 14)) || 14;
  const actuals = resolveLiveRoundActualsByDg(live, {
    roundPar: num(proj.course_par_18, NaN) || 72,
    fairwayHoles: fwHoles,
  });
  if (actuals && typeof actuals === "object" && Object.keys(actuals).length) {
    proj.live_round_actuals_by_dg = actuals;
  }
}

function readPriorStrokeShiftsFromMeta(payload) {
  const o = payload?.prior_round_course_stroke_shift;
  const out = { 1: 0, 2: 0, 3: 0, 4: 0 };
  if (!o || typeof o !== "object") return out;
  for (let r = 1; r <= 4; r++) {
    const v = num(o[r] ?? o[String(r)], NaN);
    out[r] = Number.isFinite(v) ? v : 0;
  }
  return out;
}

async function main() {
  const projPath = join(WEB_ROOT, "projections.json");
  const livePath = join(WEB_ROOT, "live-in-play.json");
  if (!existsSync(projPath)) {
    console.warn("merge-live-round-meta: missing projections.json");
    process.exit(0);
  }
  if (!existsSync(livePath)) {
    console.log("merge-live-round-meta: no live-in-play.json — skip");
    process.exit(0);
  }

  const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
    ? resolve(process.env.GOLF_MODEL_DIR.trim())
    : resolve(WEB_ROOT, "..");
  const roundsCsv = join(GOLF_MODEL_ROOT, "data", "historical_rounds_all.csv");

  let proj;
  let live;
  try {
    proj = JSON.parse(readFileSync(projPath, "utf8"));
    live = JSON.parse(readFileSync(livePath, "utf8"));
  } catch (e) {
    console.warn("merge-live-round-meta: parse error —", e.message || e);
    process.exit(0);
  }

  const lhRaw = live.live_hole_stats;
  const hasLh = lhRaw && typeof lhRaw === "object";
  if (!hasLh) {
    console.warn(
      "merge-live-round-meta: live-in-play has no live_hole_stats — display_round + post-cut prune only (no prior-hole μ bump).",
    );
  }
  const lhEffective = hasLh ? lhRaw : {};

  const fieldRaw = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : null;
  const projEvent = String(proj.event_name || "").trim();
  const fuEvent = String(fieldRaw?.event_name || fieldRaw?.eventName || "").trim();
  const liveInfoEv = String(live?.info?.event_name || live?.event_name || "").trim();
  const liveEv = fuEvent || liveInfoEv;
  const hasLiveRows = Array.isArray(live?.data) && live.data.length > 0;
  if (hasLiveRows && !liveEv) {
    console.warn(
      "merge-live-round-meta: live-in-play has player rows but no event_name — skip (stale bundle would poison display_round)",
    );
    process.exit(0);
  }
  if (projEvent && liveEv && !eventsLikelySame(projEvent, liveEv)) {
    console.warn(`merge-live-round-meta: event mismatch projections="${projEvent}" vs live="${liveEv}" — skip`);
    process.exit(0);
  }

  const projKey = String(proj.datagolf_field_week_key || "").trim();
  const fuCourseFromField = String(fieldRaw?.course_name || fieldRaw?.course || "").trim();
  const fuCourseForKey = fuCourseFromField || String(proj.course_used || "").trim();
  const evForKey = String(liveEv || projEvent || "").trim();
  const fuKey = evForKey ? fieldWeekKey(evForKey, fuCourseForKey) : "";
  const ignoreWeek =
    String(process.env.GOLF_MERGE_LIVE_ROUND_META_IGNORE_WEEK_KEY || "").trim() === "1";
  if (!ignoreWeek && projKey && fuKey && !fieldWeekKeysRoughMatch(projKey, fuKey)) {
    console.warn(`merge-live-round-meta: week key mismatch proj=${projKey} vs live=${fuKey} — skip`);
    console.warn(
      "merge-live-round-meta: set GOLF_MERGE_LIVE_ROUND_META_IGNORE_WEEK_KEY=1 to override (only if you are sure it is the same event).",
    );
    process.exit(0);
  }

  const projDateStart = projectionDateStartIso(proj) || String(fieldRaw?.date_start || fieldRaw?.dateStart || "").trim();
  const dr = dateStartIsFuture(projDateStart)
    ? 1
    : exportDisplayRoundFromLiveBundle(live, fieldRaw, lhEffective, {
        projDateStart,
        trustEvent: projEvent || liveEv,
      });
  const tz = process.env.GOLF_OU_TZ || "America/New_York";
  attachLiveRoundActualsToProjections(proj, live);

  const prevDr = Math.round(num(proj.display_round, NaN));
  proj.display_round = dr;
  proj.display_round_label = displayRoundLabel(dr, tz);
  if (Number.isFinite(dr) && dr >= 1 && dr <= 4) {
    const r = Math.round(dr);
    proj.datagolf_field_current_round = r;
    /** Keeps disk JSON aligned with merge: preds/in-play poll alone often lags info.current_round. */
    proj.datagolf_live_current_round = r;
  }

  if (!hasLh) {
    writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
    console.log(`merge-live-round-meta: display_round ${prevDr}→${dr} (no live_hole_stats μ path); wrote ${projPath}`);
    return;
  }

  const applyPriorRoundAdj =
    !flatVenuePlayerScoreAnchorEnabled() &&
    String(process.env.GOLF_COURSE_PRIOR_ROUND_DIFFICULTY ?? "1").trim() !== "0";
  if (!applyPriorRoundAdj) {
    writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
    console.log(
      `merge-live-round-meta: display_round ${prevDr}→${dr} only (GOLF_COURSE_PRIOR_ROUND_DIFFICULTY=0); wrote ${projPath}`,
    );
    return;
  }

  const projCourse = String(proj.course_used || fuCourseFromField || "").trim();
  const courseKeyHist = normCourseNameKey(projCourse);

  let histEventCtx = null;
  if (projEvent && existsSync(roundsCsv)) {
    histEventCtx = await loadEventRoundContextFromHistoricalCsv(roundsCsv, projEvent, courseKeyHist);
  }

  const priorCourseExcessByRound = {};
  const priorCourseStrokeShiftByRound = {};
  for (let r = 1; r <= 4; r++) {
    const ex = blendedPriorRoundCourseExcess(lhRaw, histEventCtx, r, projEvent, projCourse);
    priorCourseExcessByRound[r] = Number.isFinite(ex) ? Math.round(ex * 1000) / 1000 : null;
    priorCourseStrokeShiftByRound[r] = Number.isFinite(ex)
      ? Math.round(courseDifficultyStrokeShift(ex) * 1000) / 1000
      : 0;
  }

  const priObj = proj.prior_round_course_stroke_shift;
  const canDeltaMu =
    priObj &&
    typeof priObj === "object" &&
    Object.keys(priObj).length > 0 &&
    Array.isArray(proj.players) &&
    proj.players.length > 0;

  if (!canDeltaMu) {
    writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
    console.warn(
      "merge-live-round-meta: no prior_round_course_stroke_shift on projections — updated display_round only; run fetch:dg to rebuild prior-shift + μ rows.",
    );
    console.log(`merge-live-round-meta: display_round ${prevDr}→${dr}; wrote ${projPath}`);
    return;
  }

  const oldShifts = readPriorStrokeShiftsFromMeta(proj);
  let playersTouched = 0;
  const coursePar18 = Math.round(num(proj.course_par_18, NaN)) || 72;

  for (const pl of proj.players) {
    if (!pl || typeof pl !== "object") continue;
    const r = Math.round(num(pl.round, NaN));
    if (!Number.isFinite(r) || r < 1 || r > 4) continue;
    const prevS = num(oldShifts[r], 0);
    const nextS = num(priorCourseStrokeShiftByRound[r], 0);
    const delta = prevS - nextS;
    if (Math.abs(delta) < 1e-12) continue;
    const mu0 = num(pl.mu_sg, NaN);
    if (!Number.isFinite(mu0)) continue;
    const mu1 = mu0 + delta;
    const muRounded = Math.round(mu1 * 1000) / 1000;
    pl.mu_sg = muRounded;
    if ("implied_mu_sg" in pl && Number.isFinite(num(pl.implied_mu_sg, NaN))) {
      pl.implied_mu_sg = Math.round((num(pl.implied_mu_sg, 0) + delta) * 1000) / 1000;
    }
    const stpRaw = -muRounded;
    pl.score_to_par = Math.round(stpRaw * 100) / 100;
    pl.total_score = Math.round((coursePar18 + stpRaw) * 100) / 100;
    playersTouched++;
  }

  proj.prior_round_course_excess_strokes = priorCourseExcessByRound;
  proj.prior_round_course_stroke_shift = priorCourseStrokeShiftByRound;

  writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
  const parts = [2, 3, 4]
    .filter((r) => Number.isFinite(priorCourseExcessByRound[r]))
    .map((r) => `R${r}:${priorCourseExcessByRound[r]}`);
  console.log(
    `merge-live-round-meta: display_round ${prevDr}→${dr}` +
      (parts.length ? ` | prior excess ${parts.join(", ")}` : "") +
      ` | bumped mu_sg on ${playersTouched} projection row(s); wrote ${projPath}`,
  );
}

main().catch((e) => {
  console.error("merge-live-round-meta:", e.message || e);
  process.exit(1);
});
