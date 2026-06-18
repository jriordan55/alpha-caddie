/**
 * Re-apply within-event prior-round form (field-average blend) on projections.json
 * after fetch:in-play — fetch:dg runs earlier in push:live and would otherwise bake stale R1 actuals.
 *
 * R2+ rows anchor to the same player's R1 projection (pre-tournament baseline) with a small
 * nudge from yesterday's field-weighted counting actuals — not a full rebuild from μ formulas.
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { resolveLiveRoundActualsByDg } from "./dg-live-tournament-stats.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  applyFieldDayCountingLiftNatural,
  augmentEventContextWithInPlayRounds,
  blendTowardWithinEventActuals,
  buildEventContextFromLiveBundle,
  buildPriorByStatForPlayer,
  buildWithinEventCountingMapFromLiveActuals,
  buildWithinEventFormMap,
  draftKingsDgIdsFromProjections,
  ensureProjectionCourseBasisComplete,
  fieldCountingMeansFromWithinEventMap,
  loadEventRoundContextFromHistoricalCsv,
  loadWithinEventCountingActualsFromHistoryJson,
  mergeFieldCountingMeansPreferWithin,
  reconcileAllProjectionPlayerRows,
  updateProjectionBasisFromEventWeek,
} from "./course-round-adjustments.mjs";
import { applyUnifiedProjectionFactors } from "./projection-unified-factors.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function roundCounts(row, keys) {
  const out = {};
  for (const k of keys) {
    const v = num(row[k], NaN);
    if (Number.isFinite(v)) out[k] = v;
  }
  return out;
}

/** R1 export row: prefer pre-weather snapshot when present (true pre-tournament baseline). */
function r1ProjectionAnchorRow(anchor) {
  if (!anchor || typeof anchor !== "object") return anchor;
  const pre = anchor._pre_weather_counts;
  if (!pre || typeof pre !== "object") return anchor;
  const out = { ...anchor };
  for (const k of ["birdies", "bogeys", "pars", "gir", "fairways", "putts", "eagles", "doubles", "mu_sg", "implied_mu_sg"]) {
    if (Number.isFinite(num(pre[k], NaN))) out[k] = pre[k];
  }
  return out;
}

function priorStrokeShiftForRound(proj, round) {
  const pack = proj?.prior_round_course_stroke_shift ?? proj?.meta?.prior_round_course_stroke_shift ?? {};
  return num(pack[round] ?? pack[String(round)], 0);
}

function buildR1AnchorMap(players) {
  /** @type {Map<number, object>} */
  const out = new Map();
  for (const pl of players || []) {
    if (!pl || typeof pl !== "object") continue;
    if (Math.round(num(pl.round, NaN)) !== 1) continue;
    const dg = Math.round(num(pl.dg_id, NaN));
    if (Number.isFinite(dg)) out.set(dg, pl);
  }
  return out;
}

function venueScoringStubFromMeta(basis, coursePar18) {
  const cp = num(coursePar18, 72);
  return {
    venueAvgBirdies: num(basis?.venue_avg_birdies, 2.88),
    venueAvgBogeys: num(basis?.venue_avg_bogeys, 2.93),
    venueAvgGir: num(basis?.venue_avg_gir, 12),
    venueAvgFairways: num(basis?.venue_avg_fairways, 9),
    venueAvgPars: num(basis?.venue_avg_pars, 11),
    venueAvgStp: num(basis?.venue_avg_score_to_par, 0),
    nVenueRounds: num(basis?.venue_historical_rounds, 0),
    source: String(basis?.venue_scoring_source || "meta"),
    fieldByRound: new Map(),
    playerByRound: new Map(),
    playerByVenue: new Map(),
    courseFitByDg: new Map(),
    venueAvgScore: Number.isFinite(num(basis?.venue_avg_round_score, NaN)) ? num(basis.venue_avg_round_score) : cp,
  };
}

/**
 * @param {object} proj — projections.json root (mutated in place)
 * @param {object} live — live-in-play.json
 * @param {object} [opts]
 * @returns {{ playersTouched: number, fieldMeans: object | null }}
 */
export async function reapplyWithinEventFormOnProjections(proj, live, opts = {}) {
  if (!proj || !Array.isArray(proj.players) || !live) {
    return { playersTouched: 0, fieldMeans: null };
  }

  const meta = proj.meta && typeof proj.meta === "object" ? proj.meta : {};
  const basis = meta.projection_course_basis && typeof meta.projection_course_basis === "object" ? meta.projection_course_basis : {};
  const adj =
    meta.projection_round_adjustments && typeof meta.projection_round_adjustments === "object"
      ? meta.projection_round_adjustments
      : {};
  const formK = num(adj.within_event_form_carry, num(process.env.GOLF_WITHIN_EVENT_FORM_CARRY, 0.025));
  const formCap = num(adj.within_event_form_cap, num(process.env.GOLF_WITHIN_EVENT_FORM_CAP, 0.15));
  const coursePar18 = Math.round(num(proj.course_par_18, NaN)) || 72;
  const fairwayHoles = Math.round(num(basis.fairway_holes_modeled, 14)) || 14;
  const eventName = String(proj.event_name || "").trim();
  const courseKey = normCourseNameKey(String(proj.course_used || "").trim());
  const venueScoring = venueScoringStubFromMeta(basis, coursePar18);
  const r1ByDg = buildR1AnchorMap(proj.players);

  const liveOnly =
    String(process.env.GOLF_WITHIN_EVENT_LIVE_ONLY ?? "1").trim() !== "0";
  const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
    ? resolve(process.env.GOLF_MODEL_DIR.trim())
    : resolve(WEB_ROOT, "..");
  const roundsCsv = join(GOLF_MODEL_ROOT, "data", "historical_rounds_all.csv");
  const historyJsonPath = join(WEB_ROOT, "player_round_history.json");

  const actualsByDg = resolveLiveRoundActualsByDg(live, { roundPar: coursePar18, fairwayHoles });
  let withinEventCountingMap = buildWithinEventCountingMapFromLiveActuals(
    actualsByDg,
    coursePar18,
    basis.venue_avg_birdies,
    basis.venue_avg_bogeys,
  );
  if (!liveOnly && eventName && existsSync(historyJsonPath)) {
    const fromHistory = loadWithinEventCountingActualsFromHistoryJson(
      historyJsonPath,
      eventName,
      courseKey,
      new Date().getFullYear(),
      coursePar18,
      basis.venue_avg_birdies,
      basis.venue_avg_bogeys,
    );
    if (fromHistory.size > withinEventCountingMap.size) withinEventCountingMap = fromHistory;
  }

  if (!withinEventCountingMap.size) {
    return { playersTouched: 0, fieldMeans: null };
  }

  const dkField = draftKingsDgIdsFromProjections(proj);
  const dkMinPlayers = 8;
  const useDkField = dkField.size >= dkMinPlayers;
  const dkFieldFilter = useDkField ? dkField : null;
  const fieldMeanOpts = useDkField ? { minPlayers: dkMinPlayers, dgFilter: dkField } : { minPlayers: 28 };

  const basePlayers = [
    ...new Map(
      proj.players
        .filter((p) => Math.round(num(p?.round, NaN)) === 1)
        .map((p) => [Math.round(num(p.dg_id, NaN)), { dg_id: Math.round(num(p.dg_id, NaN)), mu_sg: num(p.mu_sg, 0) }]),
    ).values(),
  ];

  let histEventCtx = null;
  if (liveOnly) {
    histEventCtx = buildEventContextFromLiveBundle(live, coursePar18, basePlayers, actualsByDg);
  } else if (eventName && existsSync(roundsCsv)) {
    histEventCtx = await loadEventRoundContextFromHistoricalCsv(roundsCsv, eventName, courseKey);
    if (histEventCtx && Array.isArray(live?.data) && live.data.length) {
      augmentEventContextWithInPlayRounds(histEventCtx, live.data, coursePar18, basePlayers);
    }
  } else {
    histEventCtx = buildEventContextFromLiveBundle(live, coursePar18, basePlayers, actualsByDg);
  }

  const fieldCountingMeans = fieldCountingMeansFromWithinEventMap(withinEventCountingMap, fieldMeanOpts);

  const withinFormMap =
    formK !== 0 && histEventCtx?.playerRounds?.length
      ? buildWithinEventFormMap(histEventCtx, basePlayers, formK, formCap, undefined, dkFieldFilter)
      : new Map();

  const countKeys = ["birdies", "bogeys", "gir", "fairways", "putts", "eagles", "doubles", "pars"];
  let playersTouched = 0;

  for (const pl of proj.players) {
    if (!pl || typeof pl !== "object") continue;
    const r = Math.round(num(pl.round, NaN));
    if (!Number.isFinite(r) || r < 2) continue;

    const dg = Math.round(num(pl.dg_id, NaN));
    const r1Raw = r1ByDg.get(dg);
    if (!r1Raw) continue;

    const priorByStat = buildPriorByStatForPlayer(withinEventCountingMap, pl.dg_id, r);
    if (!priorByStat) continue;

    const r1 = r1ProjectionAnchorRow(r1Raw);
    const strokeShift = priorStrokeShiftForRound(proj, r);
    const formShift = num(withinFormMap.get(`${dg}|${r}`), 0);
    const mu1 = num(r1.mu_sg, NaN);
    if (Number.isFinite(mu1)) {
      const muNext = Math.round((mu1 - strokeShift + formShift) * 1000) / 1000;
      pl.mu_sg = muNext;
      if ("implied_mu_sg" in pl) {
        pl.implied_mu_sg = Math.round((num(r1.implied_mu_sg, mu1) - strokeShift + formShift) * 1000) / 1000;
      }
      const stp = -muNext;
      pl.score_to_par = Math.round(stp * 100) / 100;
      pl.total_score = Math.round((coursePar18 + stp) * 100) / 100;
      pl.within_event_form_shift = Math.round(formShift * 1000) / 1000;
      if (strokeShift !== 0) pl.prior_round_course_stroke_shift = Math.round(strokeShift * 1000) / 1000;
    }

    const skillBase = roundCounts(r1, countKeys);
    const blended = blendTowardWithinEventActuals({ ...skillBase }, priorByStat, r, {
      playerRow: r1,
      skillCounts: skillBase,
      fieldMeans: fieldCountingMeans,
    });

    if (fieldCountingMeans) {
      applyFieldDayCountingLiftNatural(blended, r, fieldCountingMeans, venueScoring);
    }

    pl.eagles = Math.round(num(blended.eagles, r1.eagles) * 1000) / 1000;
    pl.birdies = Math.round(num(blended.birdies, r1.birdies) * 100) / 100;
    pl.pars = Math.round(num(blended.pars, r1.pars) * 100) / 100;
    pl.bogeys = Math.round(num(blended.bogeys, r1.bogeys) * 100) / 100;
    pl.doubles = Math.round(num(blended.doubles, r1.doubles) * 1000) / 1000;
    if (Number.isFinite(num(blended.gir, NaN))) pl.gir = Math.round(blended.gir * 100) / 100;
    if (Number.isFinite(num(blended.fairways, NaN))) pl.fairways = Math.round(blended.fairways * 100) / 100;
    if (Number.isFinite(num(blended.putts, NaN))) pl.putts = Math.round(blended.putts * 100) / 100;
    playersTouched++;
  }

  if (!meta.projection_course_basis) meta.projection_course_basis = {};
  meta.projection_course_basis.field_counting_means_by_round = fieldCountingMeans || null;
  updateProjectionBasisFromEventWeek(meta.projection_course_basis, fieldCountingMeans, { payload: proj });
  ensureProjectionCourseBasisComplete(meta.projection_course_basis, proj);
  proj.projection_course_basis = meta.projection_course_basis;
  if (!meta.projection_round_adjustments) meta.projection_round_adjustments = {};
  meta.projection_round_adjustments.within_event_counting_from_actuals = true;
  meta.projection_round_adjustments.within_event_r1_anchor = true;
  meta.projection_round_adjustments.within_event_field_scope = useDkField ? "draftkings" : "full";
  if (useDkField) meta.projection_round_adjustments.within_event_dk_field_size = dkField.size;
  meta.projection_round_adjustments.within_event_form_carry = formK;
  meta.projection_round_adjustments.within_event_form_cap = formCap;
  proj.meta = meta;

  reconcileAllProjectionPlayerRows(proj, {
    minField: dkMinPlayers,
  });

  return { playersTouched, fieldMeans: fieldCountingMeans };
}

async function main() {
  const projPath = join(WEB_ROOT, "projections.json");
  const livePath = join(WEB_ROOT, "live-in-play.json");
  if (!existsSync(projPath)) {
    console.warn("[within-event-form] missing projections.json — skip");
    process.exit(0);
  }
  if (!existsSync(livePath)) {
    console.warn("[within-event-form] missing live-in-play.json — skip");
    process.exit(0);
  }

  let proj;
  let live;
  try {
    proj = JSON.parse(readFileSync(projPath, "utf8"));
    live = JSON.parse(readFileSync(livePath, "utf8"));
  } catch (e) {
    console.warn("[within-event-form] parse error —", e.message || e);
    process.exit(0);
  }

  const { playersTouched, fieldMeans } = await reapplyWithinEventFormOnProjections(proj, live);
  if (!playersTouched) {
    console.log("[within-event-form] no R2+ rows updated (no within-event counting actuals yet)");
    process.exit(0);
  }

  await applyUnifiedProjectionFactors(proj, { liveBundle: live, skipReconcile: true });
  const dkField = draftKingsDgIdsFromProjections(proj);
  reconcileAllProjectionPlayerRows(proj, {
    dgFilter: dkField.size >= 8 ? dkField : null,
    minField: 8,
  });

  writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
  const fm = fieldMeans?.bogeys?.[1];
  const scope = proj.meta?.projection_round_adjustments?.within_event_field_scope || "full";
  console.log(
    `[within-event-form] R1-anchored prior-round form on ${playersTouched} row(s)` +
      (scope === "draftkings" ? " (DraftKings field)" : "") +
      (Number.isFinite(fm) ? ` | field R1 bogeys≈${fm}` : "") +
      ` → ${projPath}`,
  );
}

main().catch((e) => {
  console.error("[within-event-form]", e.message || e);
  process.exit(1);
});
