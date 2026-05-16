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
 * meta + player rows `round`) so field-updates lag does not stall R3→R4. When display_round≥3,
 * trims projection.players to weekend field (shrunk preds/in-play roster or CUT/WD status).
 *
 * Env: GOLF_MODEL_DIR → repo root (parent of alpha-caddie-web). Uses data/historical_rounds_all.csv
 * when present. GOLF_COURSE_PRIOR_ROUND_DIFFICULTY=0 skips mu adjustments (still updates rounds).
 * Optional: GOLF_POST_CUT_MIN_LIVE_IDS, GOLF_POST_CUT_SHRINK_AT_LEAST, GOLF_POST_CUT_MIN_KEEP_FRAC.
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  blendedPriorRoundCourseExcess,
  courseDifficultyStrokeShift,
  loadEventRoundContextFromHistoricalCsv,
} from "./course-round-adjustments.mjs";
import { eventsLikelySame, fieldWeekKey, fieldWeekKeysRoughMatch } from "./dg-events-align.mjs";
import { exportDisplayRoundFromLiveBundle, num } from "./dg-display-round-from-bundle.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function isEliminatedProjectionRow(pl) {
  if (!pl || typeof pl !== "object") return false;
  const mc = pl.make_cut;
  if (mc === false) return true;
  if (mc === true) return false;
  if (typeof mc === "boolean") return !mc;
  if (mc == null || mc === "") return false;
  const n = num(mc, NaN);
  if (Number.isFinite(n) && n <= 0) return true;
  const pos = String(pl.current_pos || "");
  return /\b(CUT|WD|DQ|MDF|DNS|W\/D|RET)\b/i.test(pos);
}

function isEliminatedLiveRow(inPlayRow) {
  if (!inPlayRow || typeof inPlayRow !== "object") return false;
  const thruRaw = inPlayRow.thru ?? inPlayRow.Thru;
  if (thruRaw != null && thruRaw !== "") {
    const s = String(thruRaw).trim();
    const u = s.toUpperCase();
    if (u.includes("CUT")) return true;
    if (/\b(WD|DQ|MDF|DNS|RET)\b/i.test(s)) return true;
  }
  const pos = String(inPlayRow.position ?? inPlayRow.Position ?? "").trim();
  return /\b(CUT|WD|DQ|MDF|DNS|W\/D|RET)\b/i.test(pos);
}

function pickBestStatusRow(rows) {
  let best = null;
  let br = NaN;
  for (const p of rows) {
    const rr = Math.round(num(p?.round, NaN));
    if (!Number.isFinite(rr)) continue;
    if (!best || rr >= br) {
      best = p;
      br = rr;
    }
  }
  return best || rows[0] || null;
}

/**
 * R3+: keep only players still in the event. Prefer preds/in-play roster when it shrunk vs projections;
 * else drop dg_ids flagged eliminated (mirror app.js isPlayerEliminatedFromEvent).
 */
function prunePostCutProjectionPlayers(players, live, dr) {
  if (!Array.isArray(players) || dr < 3) return { players, note: "" };

  /** @type {Set<number>} */
  const uniqProj = new Set();
  for (const p of players) {
    const id = Math.round(num(p?.dg_id, NaN));
    if (Number.isFinite(id)) uniqProj.add(id);
  }
  const uSize = uniqProj.size;
  if (uSize < 8) return { players, note: "" };

  const MIN_LIVE = Math.max(20, Number(process.env.GOLF_POST_CUT_MIN_LIVE_IDS || 35));
  const SHRINK_AT_LEAST = Math.max(12, Number(process.env.GOLF_POST_CUT_SHRINK_AT_LEAST || 15));

  const liveRows = Array.isArray(live?.data) ? live.data : [];
  /** @type {Map<number, object>} */
  const liveByDg = new Map();
  /** @type {Set<number>} */
  const liveIds = new Set();
  for (const r of liveRows) {
    const id = Math.round(num(r?.dg_id ?? r?.dgId, NaN));
    if (Number.isFinite(id)) {
      liveIds.add(id);
      if (!liveByDg.has(id)) liveByDg.set(id, r);
    }
  }

  /** @type {Set<number>} */
  let keep;
  let note = "";
  const liveLooksCut = liveIds.size >= MIN_LIVE && liveIds.size <= uSize - SHRINK_AT_LEAST;

  if (liveLooksCut) {
    keep = liveIds;
    note = `post-cut: preds/in-play roster ${liveIds.size} dg_id(s) (had ${uSize})`;
  } else {
    const byId = new Map();
    for (const p of players) {
      const id = Math.round(num(p?.dg_id, NaN));
      if (!Number.isFinite(id)) continue;
      if (!byId.has(id)) byId.set(id, []);
      byId.get(id).push(p);
    }
    keep = new Set();
    for (const [, rows] of byId) {
      const row = pickBestStatusRow(rows);
      const id = row ? Math.round(num(row.dg_id, NaN)) : NaN;
      if (!Number.isFinite(id)) continue;
      if (isEliminatedProjectionRow(row) || isEliminatedLiveRow(liveByDg.get(id))) continue;
      keep.add(id);
    }
    if (!keep.size) return { players, note: "" };
    note = `post-cut: status filter ${keep.size}/${uSize} dg_id(s)`;
  }

  const minKeepFrac = Number(process.env.GOLF_POST_CUT_MIN_KEEP_FRAC || 0.25);
  const minKeepRows = Math.max(MIN_LIVE, Math.floor(uSize * minKeepFrac));
  if (keep.size < minKeepRows) {
    console.warn(
      `merge-live-round-meta: post-cut prune would keep only ${keep.size} dg_id(s); need ≥${minKeepRows} — skipping`,
    );
    return { players, note: "" };
  }

  const out = [];
  let droppedRows = 0;
  for (const p of players) {
    const id = Math.round(num(p?.dg_id, NaN));
    if (Number.isFinite(id) && keep.has(id)) out.push(p);
    else droppedRows++;
  }
  if (droppedRows === 0) return { players, note: "" };

  return {
    players: out,
    note: `${note}; dropped ${droppedRows} projection row(s)`,
  };
}

function displayRoundLabel(r, tz) {
  const lab =
    r === 1 ? "R1 — next Thursday" : r === 2 ? "R2 — Friday" : r === 3 ? "R3 — Saturday" : r === 4 ? "R4 — Sunday" : `R${r}`;
  return `${lab} (auto, ${tz})`;
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

  const lh = live.live_hole_stats;
  const fieldRaw = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : null;
  if (!lh || typeof lh !== "object") {
    console.log("merge-live-round-meta: live-in-play has no live_hole_stats — skip");
    process.exit(0);
  }

  const projEvent = String(proj.event_name || "").trim();
  const fuEvent = String(fieldRaw?.event_name || fieldRaw?.eventName || "").trim();
  const liveInfoEv = String(live?.info?.event_name || live?.event_name || "").trim();
  const liveEv = fuEvent || liveInfoEv;
  if (projEvent && liveEv && !eventsLikelySame(projEvent, liveEv)) {
    console.warn(`merge-live-round-meta: event mismatch projections="${projEvent}" vs live="${liveEv}" — skip`);
    process.exit(0);
  }

  const projKey = String(proj.datagolf_field_week_key || "").trim();
  const fuCourse = String(fieldRaw?.course_name || fieldRaw?.course || "").trim();
  const fuKey = liveEv ? fieldWeekKey(liveEv, fuCourse) : "";
  if (projKey && fuKey && !fieldWeekKeysRoughMatch(projKey, fuKey)) {
    console.warn(`merge-live-round-meta: week key mismatch proj=${projKey} vs live=${fuKey} — skip`);
    process.exit(0);
  }

  const dr = exportDisplayRoundFromLiveBundle(live, fieldRaw, lh);
  const tz = process.env.GOLF_OU_TZ || "America/New_York";

  const prevDr = Math.round(num(proj.display_round, NaN));
  proj.display_round = dr;
  proj.display_round_label = displayRoundLabel(dr, tz);
  if (Number.isFinite(dr) && dr >= 1 && dr <= 4) {
    proj.datagolf_field_current_round = Math.round(dr);
  }

  const pruned = prunePostCutProjectionPlayers(proj.players, live, dr);
  if (pruned.note && Array.isArray(pruned.players)) {
    proj.players = pruned.players;
    console.log(`merge-live-round-meta: ${pruned.note}`);
  }

  const applyPriorRoundAdj = String(process.env.GOLF_COURSE_PRIOR_ROUND_DIFFICULTY ?? "1").trim() !== "0";
  if (!applyPriorRoundAdj) {
    writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
    console.log(
      `merge-live-round-meta: display_round ${prevDr}→${dr} only (GOLF_COURSE_PRIOR_ROUND_DIFFICULTY=0); wrote ${projPath}`,
    );
    return;
  }

  let histEventCtx = null;
  if (projEvent && existsSync(roundsCsv)) {
    histEventCtx = await loadEventRoundContextFromHistoricalCsv(roundsCsv, projEvent);
  }

  const priorCourseExcessByRound = {};
  const priorCourseStrokeShiftByRound = {};
  for (let r = 1; r <= 4; r++) {
    const ex = blendedPriorRoundCourseExcess(lh, histEventCtx, r);
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
