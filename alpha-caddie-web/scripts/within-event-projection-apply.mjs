/**
 * Re-apply within-event prior-round form (field-average blend) on projections.json
 * after fetch:in-play — fetch:dg runs earlier in push:live and would otherwise bake stale R1 actuals.
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
  buildPriorByStatForPlayer,
  buildWithinEventCountingMapFromLiveActuals,
  buildWithinEventFormMap,
  fieldCountingMeansFromEventContext,
  fieldCountingMeansFromWithinEventMap,
  loadEventRoundContextFromHistoricalCsv,
  loadWithinEventCountingActualsFromHistoryJson,
  priorRoundCountingTarget,
  withinEventCountingBlendWeight,
} from "./course-round-adjustments.mjs";

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

/** Prefer counts before weather bake so unblend is not thrown off by a prior push. */
function countingRowForUnblend(pl) {
  const pre = pl._pre_weather_counts;
  if (!pre || typeof pre !== "object") return pl;
  const out = { ...pl };
  for (const k of ["birdies", "bogeys", "pars", "gir", "fairways", "putts", "eagles", "doubles"]) {
    if (Number.isFinite(num(pre[k], NaN))) out[k] = pre[k];
  }
  return out;
}

/** Undo prior within-event counting blend so we can re-apply with fresh field means / R1 actuals. */
function unblendWithinEventCounts(current, priorByStat, fieldMeans, targetRound, playerRow, statKeys) {
  const tr = Math.round(num(targetRound, NaN));
  const wBird = withinEventCountingBlendWeight(
    Math.max(priorByStat.birdies?.length || 0, priorByStat.bogeys?.length || 0),
    playerRow,
  );
  const wBog = wBird;
  const out = { ...current };
  for (const k of statKeys) {
    const cur = num(current[k], NaN);
    if (!Number.isFinite(cur)) continue;
    const arr = priorByStat[k];
    if (!Array.isArray(arr) || !arr.length) continue;
    const oldT = priorRoundCountingTarget(k, arr, fieldMeans, tr);
    if (!Number.isFinite(oldT)) continue;
    let w = k === "bogeys" ? wBog : wBird;
    if (k === "gir" || k === "fairways" || k === "putts") w = Math.min(0.55, wBird * 0.65);
    if (w <= 0 || w >= 1) continue;
    out[k] = (cur - w * oldT) / (1 - w);
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
  const formK = num(adj.within_event_form_carry, num(process.env.GOLF_WITHIN_EVENT_FORM_CARRY, 0.05));
  const formCap = num(adj.within_event_form_cap, num(process.env.GOLF_WITHIN_EVENT_FORM_CAP, 0.35));
  const coursePar18 = Math.round(num(proj.course_par_18, NaN)) || 72;
  const fairwayHoles = Math.round(num(basis.fairway_holes_modeled, 14)) || 14;
  const eventName = String(proj.event_name || "").trim();
  const courseKey = normCourseNameKey(String(proj.course_used || "").trim());
  const venueScoring = venueScoringStubFromMeta(basis, coursePar18);

  const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
    ? resolve(process.env.GOLF_MODEL_DIR.trim())
    : resolve(WEB_ROOT, "..");
  const roundsCsv = join(GOLF_MODEL_ROOT, "data", "historical_rounds_all.csv");
  const historyJsonPath = join(WEB_ROOT, "player_round_history.json");

  let withinEventCountingMap = new Map();
  if (eventName && existsSync(historyJsonPath)) {
    withinEventCountingMap = loadWithinEventCountingActualsFromHistoryJson(
      historyJsonPath,
      eventName,
      courseKey,
      new Date().getFullYear(),
      coursePar18,
      basis.venue_avg_birdies,
      basis.venue_avg_bogeys,
    );
  }

  const actualsByDg = resolveLiveRoundActualsByDg(live, { roundPar: coursePar18, fairwayHoles });
  const liveCountingMap = buildWithinEventCountingMapFromLiveActuals(
    actualsByDg,
    coursePar18,
    basis.venue_avg_birdies,
    basis.venue_avg_bogeys,
  );
  if (liveCountingMap.size > withinEventCountingMap.size) {
    withinEventCountingMap = liveCountingMap;
  }

  if (!withinEventCountingMap.size) {
    return { playersTouched: 0, fieldMeans: null };
  }

  const oldFieldMeans = basis.field_counting_means_by_round || null;
  const fieldCountingFromEvent = null;
  let histEventCtx = null;
  if (eventName && existsSync(roundsCsv)) {
    histEventCtx = await loadEventRoundContextFromHistoricalCsv(roundsCsv, eventName, courseKey);
  }
  if (histEventCtx && Array.isArray(live?.data) && live.data.length) {
    const basePlayers = [];
    const seen = new Set();
    for (const pl of proj.players) {
      const id = Math.round(num(pl?.dg_id, NaN));
      const r = Math.round(num(pl?.round, NaN));
      if (!Number.isFinite(id) || r !== 1 || seen.has(id)) continue;
      seen.add(id);
      basePlayers.push({ dg_id: id, mu_sg: num(pl.mu_sg, 0) });
    }
    augmentEventContextWithInPlayRounds(histEventCtx, live.data, coursePar18, basePlayers);
  }

  const fieldFromEvent = histEventCtx ? fieldCountingMeansFromEventContext(histEventCtx) : null;
  const fieldFromMap = fieldCountingMeansFromWithinEventMap(withinEventCountingMap);
  const fieldCountingMeans = fieldFromEvent?.birdies?.[1]
    ? fieldFromEvent
    : fieldFromMap?.birdies?.[1]
      ? fieldFromMap
      : fieldFromEvent || fieldFromMap;

  const withinFormMap =
    formK !== 0 && histEventCtx
      ? buildWithinEventFormMap(
          histEventCtx,
          [...new Map(proj.players.filter((p) => Math.round(num(p?.round, NaN)) === 1).map((p) => [p.dg_id, { dg_id: p.dg_id, mu_sg: num(p.mu_sg, 0) }])).values()],
          formK,
          formCap,
        )
      : new Map();

  const countKeys = ["birdies", "bogeys", "gir", "fairways", "putts", "eagles", "doubles", "pars"];
  let playersTouched = 0;

  for (const pl of proj.players) {
    if (!pl || typeof pl !== "object") continue;
    const r = Math.round(num(pl.round, NaN));
    if (!Number.isFinite(r) || r < 2) continue;

    const priorByStat = buildPriorByStatForPlayer(withinEventCountingMap, pl.dg_id, r);
    if (!priorByStat) continue;

    const oldForm = num(pl.within_event_form_shift, 0);
    const newForm = num(withinFormMap.get(`${Math.round(num(pl.dg_id, NaN))}|${r}`), 0);
    const formDelta = newForm - oldForm;
    if (Math.abs(formDelta) > 1e-9) {
      const mu0 = num(pl.mu_sg, NaN);
      if (Number.isFinite(mu0)) {
        const mu1 = Math.round((mu0 + formDelta) * 1000) / 1000;
        pl.mu_sg = mu1;
        if ("implied_mu_sg" in pl && Number.isFinite(num(pl.implied_mu_sg, NaN))) {
          pl.implied_mu_sg = Math.round((num(pl.implied_mu_sg, 0) + formDelta) * 1000) / 1000;
        }
        const stp = -mu1;
        pl.score_to_par = Math.round(stp * 100) / 100;
        pl.total_score = Math.round((coursePar18 + stp) * 100) / 100;
        pl.within_event_form_shift = Math.round(newForm * 1000) / 1000;
      }
    }

    const current = roundCounts(countingRowForUnblend(pl), countKeys);
    const skillBeforeBlend = unblendWithinEventCounts(
      current,
      priorByStat,
      oldFieldMeans,
      r,
      pl,
      ["birdies", "bogeys", "gir", "fairways", "putts"],
    );

    const blended = blendTowardWithinEventActuals(skillBeforeBlend, priorByStat, r, {
      playerRow: pl,
      skillCounts: skillBeforeBlend,
      fieldMeans: fieldCountingMeans,
    });

    if (fieldCountingMeans) {
      applyFieldDayCountingLiftNatural(blended, r, fieldCountingMeans, venueScoring);
    }

    pl.eagles = Math.round(num(blended.eagles, pl.eagles) * 1000) / 1000;
    pl.birdies = Math.round(num(blended.birdies, pl.birdies) * 100) / 100;
    pl.pars = Math.round(num(blended.pars, pl.pars) * 100) / 100;
    pl.bogeys = Math.round(num(blended.bogeys, pl.bogeys) * 100) / 100;
    pl.doubles = Math.round(num(blended.doubles, pl.doubles) * 1000) / 1000;
    if (Number.isFinite(num(blended.gir, NaN))) pl.gir = Math.round(blended.gir * 100) / 100;
    if (Number.isFinite(num(blended.fairways, NaN))) pl.fairways = Math.round(blended.fairways * 100) / 100;
    if (Number.isFinite(num(blended.putts, NaN))) pl.putts = Math.round(blended.putts * 100) / 100;
    playersTouched++;
  }

  if (!meta.projection_course_basis) meta.projection_course_basis = {};
  meta.projection_course_basis.field_counting_means_by_round = fieldCountingMeans || null;
  if (!meta.projection_round_adjustments) meta.projection_round_adjustments = {};
  meta.projection_round_adjustments.within_event_counting_from_actuals = true;
  proj.meta = meta;

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

  writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
  const fm = fieldMeans?.bogeys?.[1];
  console.log(
    `[within-event-form] Re-applied field-average prior-round form on ${playersTouched} row(s)` +
      (Number.isFinite(fm) ? ` | field R1 bogeys≈${fm}` : "") +
      ` → ${projPath}`,
  );
}

main().catch((e) => {
  console.error("[within-event-form]", e.message || e);
  process.exit(1);
});
