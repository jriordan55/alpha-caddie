/**
 * Course-fit + recent-form round μ, plus weather, tee wave, course distance SG,
 * and hole SG only when it is a major strokes-gained factor.
 *
 *   μ = V + w_pc·(P − V) + w_form·(F − B)
 *     + weather + tee wave
 *     + course distance SG
 *     + hole SG if |stpAdj| is major
 */
import { join } from "path";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import { resolveCourseLayout } from "./course-hole-layout.mjs";
import { loadCourseTableAdjRate } from "./course-round-adjustments.mjs";
import { courseRequirementSgWeights, resolveCourseTableForVenue } from "./course-adaptive-pricing.mjs";
import { fieldSkillMedian, N_FAIRWAY_HOLES } from "./projection-core.mjs";
import { num } from "./round-projection-mu.mjs";
import {
  buildWalkForwardHistoryByDgId,
  loadVenueScoringBeforeCutoff,
  inferCourseNameFromHist,
  inferCourseParFromHist,
  resolveWalkforwardWeather,
} from "./historical-walkforward-projections.mjs";
import { buildHoleSgAdjustmentsAsOf, applyHoleSgToBirdies } from "./course-hole-sg-asof.mjs";
import { buildDistanceSgAdjustmentsAsOf, applyDistanceSgToBirdies } from "./course-distance-sg-asof.mjs";
import { weatherDifficultyDeltaFromSnapshot, statWeatherMuAdjustment } from "./weather-mu-adjustments.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";

export const STRICT_FIT_FORM_K = 10;
export const STRICT_FIT_FORM_N_FORM = 10;
export const STRICT_FIT_FORM_N_SKILL = 36;
export const STRICT_FIT_FORM_DECAY = 0.86;

/** Hole SG only applies when |stpAdj| reaches this (major SG factor, not noise). */
export const HOLE_SG_MAJOR_ABS_STP = 0.25;
const TEE_WAVE_W = 0.3;

export function strictFitFormPipelineEnv() {
  return {
    GOLF_STRICT_FIT_FORM: "1",
    GOLF_WF_SKILL_MAX_ROUNDS: String(STRICT_FIT_FORM_N_SKILL),
    GOLF_HOLE_SG_BLEND: "1",
    GOLF_DISTANCE_SG_BLEND: "1",
    GOLF_DISTANCE_SG_COURSE_FOCUS: "0.88",
    GOLF_WF_WEATHER: "1",
    GOLF_UNIFIED_TEE_WAVE_W: String(TEE_WAVE_W),
    GOLF_MARKET_BOOK_CALIBRATION: "0",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "0",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0",
    GOLF_WITHIN_EVENT_COUNTING_BLEND: "0",
    GOLF_FIELD_DAY_COUNTING_LIFT_FRAC: "0",
    GOLF_UNIFIED_BOUNCE_BACK_K: "0",
    GOLF_NODE_ROUND_MU_MULT: "1,1,1,1",
  };
}

export function strictFitFormEnabled() {
  return String(process.env.GOLF_STRICT_FIT_FORM || "").trim() === "1";
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function shrink(n, k = STRICT_FIT_FORM_K) {
  const nn = Math.max(0, n);
  return nn / (nn + k);
}

function girFwCount(raw, nHoles) {
  const v = num(raw, NaN);
  if (!Number.isFinite(v)) return NaN;
  const nh = nHoles;
  if (v > 0 && v <= 1.0001) return v * nh;
  if (v > 1 && v <= nh + 0.51) return v;
  if (v > nh && v <= 100) return (v / 100) * nh;
  return NaN;
}

function placeholderCounts(row) {
  const b = num(row.birdies, NaN);
  const p = num(row.pars, NaN);
  const bg = num(row.bogeys ?? row.bogies, NaN);
  if (b === 0 && bg === 0 && (!Number.isFinite(p) || p === 0 || p >= 10)) return true;
  return false;
}

function marketValueFromHistRound(market, row, coursePar18, fairwayHoles) {
  if (market === "Total score") {
    const rs = num(row.round_score, NaN);
    return Number.isFinite(rs) && rs >= 55 && rs <= 95 ? rs : NaN;
  }
  if (placeholderCounts(row) && (market === "Birdies" || market === "Bogeys" || market === "Pars")) {
    return NaN;
  }
  if (market === "Birdies") {
    const b = num(row.birdies, NaN);
    if (!Number.isFinite(b) || b < 0 || b > 18) return NaN;
    if (row.sortKey != null) return b;
    const e = num(row.eagles_or_better ?? row.eagles, 0);
    return b + Math.max(0, Number.isFinite(e) ? e : 0);
  }
  if (market === "Bogeys") {
    const bg = num(row.bogeys ?? row.bogies, NaN);
    if (!Number.isFinite(bg) || bg < 0 || bg > 18) return NaN;
    if (row.sortKey != null) return bg;
    const d = num(row.doubles_or_worse ?? row.doubles, 0);
    return bg + Math.max(0, Number.isFinite(d) ? d : 0);
  }
  if (market === "GIR") return girFwCount(row.gir, 18);
  if (market === "Fairways hit") {
    const raw = Number.isFinite(num(row.driving_acc, NaN)) ? row.driving_acc : row.fairways;
    return girFwCount(raw, fairwayHoles);
  }
  return NaN;
}

function weightedMean(values, decay = null) {
  let sum = 0;
  let wsum = 0;
  let n = 0;
  for (let i = 0; i < values.length; i++) {
    const v = values[i];
    if (!Number.isFinite(v)) continue;
    const w = decay == null ? 1 : decay ** i;
    sum += w * v;
    wsum += w;
    n++;
  }
  return { mean: wsum > 0 ? sum / wsum : NaN, n };
}

function sgMean(rounds, key, nMax = STRICT_FIT_FORM_N_SKILL) {
  const vals = [];
  for (const r of rounds.slice(0, nMax)) {
    const v = num(r[key], NaN);
    if (Number.isFinite(v)) vals.push(v);
  }
  return weightedMean(vals, STRICT_FIT_FORM_DECAY);
}

function holeSgIsMajor(holeAdj) {
  if (!holeAdj || !Number.isFinite(holeAdj.stpAdj)) return false;
  if (Math.abs(holeAdj.stpAdj) < HOLE_SG_MAJOR_ABS_STP) return false;
  if (num(holeAdj.coverage, 0) < 0.35) return false;
  if (num(holeAdj.nHoles, 0) < 8) return false;
  return true;
}

function rowCompletedMs(row) {
  const s = String(row?.event_completed || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) {
    return Date.parse(`${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`);
  }
  return NaN;
}

function teeWaveBiasFromHist(histRows, courseKey, cutoffMs) {
  const buckets = {
    morning: { n: 0, stp: 0, bird: 0, bog: 0 },
    afternoon: { n: 0, stp: 0, bird: 0, bog: 0 },
  };
  for (const row of histRows) {
    const t = rowCompletedMs(row);
    if (Number.isFinite(cutoffMs) && Number.isFinite(t) && t >= cutoffMs) continue;
    if (normCourseNameKey(row.course_name || "") !== courseKey) continue;
    const wave = teeWaveFromTeetimeAndLabel(row.teetime ?? row.tee_time, row.dg_tee_wave);
    if (wave !== "morning" && wave !== "afternoon") continue;
    const cp = num(row.course_par, NaN);
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(cp) || !Number.isFinite(rs)) continue;
    buckets[wave].n++;
    buckets[wave].stp += rs - cp;
    const bird = num(row.birdies, NaN);
    const bog = num(row.bogeys ?? row.bogies, NaN);
    if (Number.isFinite(bird)) buckets[wave].bird += bird;
    if (Number.isFinite(bog)) buckets[wave].bog += bog;
  }
  const m = buckets.morning;
  const a = buckets.afternoon;
  if (m.n <= 40 || a.n <= 40) {
    return { deltaAfternoonMinusMorning: 0, deltaBirdiesAfternoonMinusMorning: 0, deltaBogeysAfternoonMinusMorning: 0, n: m.n + a.n };
  }
  return {
    deltaAfternoonMinusMorning: a.stp / a.n - m.stp / m.n,
    deltaBirdiesAfternoonMinusMorning: a.bird / a.n - m.bird / m.n,
    deltaBogeysAfternoonMinusMorning: a.bog / a.n - m.bog / m.n,
    n: m.n + a.n,
  };
}

function playerWavesThisRound(histRows, eventName, eventYear, targetRound) {
  /** @type {Map<number, string>} */
  const out = new Map();
  for (const row of histRows) {
    const dg = Math.round(num(row.dg_id, NaN));
    const yr = Math.round(num(row.year, NaN));
    const rnd = Math.round(num(row.round_num, NaN));
    if (!Number.isFinite(dg) || yr !== eventYear || rnd !== targetRound) continue;
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const wave = teeWaveFromTeetimeAndLabel(row.teetime ?? row.tee_time, row.dg_tee_wave);
    if (wave) out.set(dg, wave);
  }
  return out;
}

function waveMarketDelta(market, wave, bias) {
  if (!wave || (wave !== "morning" && wave !== "afternoon")) return 0;
  const sign = wave === "afternoon" ? 1 : -1;
  const w = TEE_WAVE_W * 0.5;
  const stp = num(bias?.deltaAfternoonMinusMorning, 0) * w * sign;
  if (market === "Total score") return stp;
  if (market === "Birdies") return num(bias?.deltaBirdiesAfternoonMinusMorning, 0) * w * sign;
  if (market === "Bogeys") return num(bias?.deltaBogeysAfternoonMinusMorning, 0) * w * sign;
  if (market === "GIR") return -0.22 * stp;
  if (market === "Fairways hit") return -0.14 * stp;
  return 0;
}

/**
 * @returns {Promise<Map<number, Map<string, number>>>}
 */
export async function buildStrictFitFormMuMapForEvent({
  repoRoot,
  histRows,
  eventName,
  eventYear,
  targetRound,
  betTimeMs,
  fieldDgIds,
  courseName: courseNameOverride = "",
}) {
  const dgSet = new Set((fieldDgIds || []).filter((d) => Number.isFinite(d)));
  if (!dgSet.size) return new Map();

  const courseName =
    String(courseNameOverride || "").trim() || inferCourseNameFromHist(histRows, eventName, eventYear);
  const courseKey = normCourseNameKey(courseName);
  const webRoot = join(repoRoot, "alpha-caddie-web");
  const layout = resolveCourseLayout({
    coursePar18: inferCourseParFromHist(histRows, eventName, eventYear, courseKey),
    courseUsed: courseName,
    eventName,
    webRoot,
  });
  const coursePar18 = layout.course_par_18;
  const fairwayHoles = layout.fairway_holes_modeled || N_FAIRWAY_HOLES;

  const historyByDgId = buildWalkForwardHistoryByDgId(histRows, betTimeMs, dgSet);
  const [venueScoring, holeSgByDg, distSgByDg] = await Promise.all([
    loadVenueScoringBeforeCutoff(
      histRows,
      courseKey,
      courseName,
      betTimeMs,
      eventName,
      eventYear,
      targetRound,
    ),
    buildHoleSgAdjustmentsAsOf({
      webRoot,
      courseKey,
      courseName,
      cutoffMs: betTimeMs,
      eventName,
      eventYear,
      targetRound,
      fieldDgIds: dgSet,
    }),
    buildDistanceSgAdjustmentsAsOf({
      webRoot,
      courseKey,
      courseName,
      cutoffMs: betTimeMs,
      eventName,
      eventYear,
      targetRound,
      fieldDgIds: dgSet,
    }),
  ]);

  const weatherSnap = resolveWalkforwardWeather({
    webRoot,
    histRows,
    eventName,
    eventYear,
    targetRound,
  });
  const weatherD = weatherDifficultyDeltaFromSnapshot(weatherSnap);
  const waveBias = teeWaveBiasFromHist(histRows, courseKey, betTimeMs);
  const waveByDg = playerWavesThisRound(histRows, eventName, eventYear, targetRound);

  const ctRow = resolveCourseTableForVenue(courseName);
  const sgWeights = courseRequirementSgWeights(ctRow);
  const courseAdjStp = loadCourseTableAdjRate(courseName, "adj_score_to_par");
  const courseGirRate = loadCourseTableAdjRate(courseName, "adj_gir");
  const courseFwRate = loadCourseTableAdjRate(courseName, "adj_driving_accuracy");

  const venueV = {
    "Total score": Number.isFinite(venueScoring.venueAvgScore)
      ? venueScoring.venueAvgScore
      : Number.isFinite(courseAdjStp)
        ? coursePar18 + courseAdjStp
        : NaN,
    Birdies:
      Number.isFinite(venueScoring.venueAvgBirdies)
        ? venueScoring.venueAvgBirdies + Math.max(0, num(venueScoring.venueAvgEagles, 0))
        : NaN,
    Bogeys:
      Number.isFinite(venueScoring.venueAvgBogeys)
        ? venueScoring.venueAvgBogeys + Math.max(0, num(venueScoring.venueAvgDoubles, 0))
        : NaN,
    GIR: Number.isFinite(venueScoring.venueAvgGir)
      ? venueScoring.venueAvgGir
      : Number.isFinite(courseGirRate)
        ? courseGirRate * 18
        : NaN,
    "Fairways hit": Number.isFinite(venueScoring.venueAvgFairways)
      ? venueScoring.venueAvgFairways
      : Number.isFinite(courseFwRate)
        ? courseFwRate * fairwayHoles
        : NaN,
  };

  const ottSamples = [];
  const appSamples = [];
  const argSamples = [];
  const puttSamples = [];
  /** @type {Map<number, object>} */
  const playerPrep = new Map();

  for (const dg of dgSet) {
    const rec = historyByDgId[String(dg)];
    const rounds = rec?.rounds || [];
    const sameCourse = rounds.filter((r) => normCourseNameKey(r.course_name || "") === courseKey);
    const prep = { dg, rounds, sameCourse, name: rec?.player_name || "" };
    playerPrep.set(dg, prep);
    const ott = sgMean(rounds, "sg_ott");
    const app = sgMean(rounds, "sg_app");
    const arg = sgMean(rounds, "sg_arg");
    const putt = sgMean(rounds, "sg_putt");
    prep.sg = {
      sg_ott: ott.mean,
      sg_app: app.mean,
      sg_arg: arg.mean,
      sg_putt: putt.mean,
      n: Math.min(ott.n, app.n, arg.n, putt.n),
    };
    if (Number.isFinite(ott.mean)) ottSamples.push(ott.mean);
    if (Number.isFinite(app.mean)) appSamples.push(app.mean);
    if (Number.isFinite(arg.mean)) argSamples.push(arg.mean);
    if (Number.isFinite(putt.mean)) puttSamples.push(putt.mean);
  }

  const fieldMed = {
    sg_ott: fieldSkillMedian(ottSamples),
    sg_app: fieldSkillMedian(appSamples),
    sg_arg: fieldSkillMedian(argSamples),
    sg_putt: fieldSkillMedian(puttSamples),
  };

  const markets = ["Total score", "Birdies", "Bogeys", "GIR", "Fairways hit"];
  const fallbackV = {};
  for (const market of markets) {
    if (Number.isFinite(venueV[market])) {
      fallbackV[market] = venueV[market];
      continue;
    }
    const vals = [];
    for (const prep of playerPrep.values()) {
      const recent = [];
      for (const r of prep.rounds.slice(0, STRICT_FIT_FORM_N_SKILL)) {
        const v = marketValueFromHistRound(market, r, coursePar18, fairwayHoles);
        if (Number.isFinite(v)) recent.push(v);
      }
      const m = weightedMean(recent, STRICT_FIT_FORM_DECAY).mean;
      if (Number.isFinite(m)) vals.push(m);
    }
    fallbackV[market] = vals.length ? vals.reduce((s, x) => s + x, 0) / vals.length : NaN;
  }

  /** @type {Map<number, Map<string, number>>} */
  const byDg = new Map();
  for (const [dg, prep] of playerPrep) {
    const mus = new Map();
    let fitStrokes = 0;
    let fitW = 0;
    for (const [sk, w] of Object.entries(sgWeights || {})) {
      if (!(w > 0.03)) continue;
      const pv = num(prep.sg?.[sk], NaN);
      const fm = num(fieldMed[sk], NaN);
      if (!Number.isFinite(pv) || !Number.isFinite(fm)) continue;
      fitStrokes += w * (pv - fm);
      fitW += w;
    }
    if (fitW > 0) fitStrokes /= fitW;
    const wSg = shrink(prep.sg?.n || 0);
    const scoreFit = -fitStrokes * wSg;

    for (const market of markets) {
      const V = Number.isFinite(venueV[market]) ? venueV[market] : fallbackV[market];
      const pcVals = [];
      for (const r of prep.sameCourse) {
        if (
          eventsLikelySame(eventName, r.event_name) &&
          Number(r.year) === eventYear &&
          Number(r.round_num) >= targetRound
        ) {
          continue;
        }
        const v = marketValueFromHistRound(market, r, coursePar18, fairwayHoles);
        if (Number.isFinite(v)) pcVals.push(v);
      }
      const P = pcVals.length ? pcVals.reduce((s, x) => s + x, 0) / pcVals.length : NaN;
      const wPc = Number.isFinite(P) ? shrink(pcVals.length) : 0;

      const formVals = [];
      const baseVals = [];
      for (let i = 0; i < prep.rounds.length && i < STRICT_FIT_FORM_N_SKILL; i++) {
        const v = marketValueFromHistRound(market, prep.rounds[i], coursePar18, fairwayHoles);
        if (!Number.isFinite(v)) continue;
        if (formVals.length < STRICT_FIT_FORM_N_FORM) formVals.push(v);
        else baseVals.push(v);
      }
      const F = weightedMean(formVals).mean;
      const B = weightedMean(baseVals, STRICT_FIT_FORM_DECAY).mean;
      const wForm = Number.isFinite(F) && Number.isFinite(B) ? shrink(formVals.length) : 0;

      let mu = Number.isFinite(V) ? V : Number.isFinite(P) ? P : Number.isFinite(F) ? F : NaN;
      if (!Number.isFinite(mu)) continue;
      if (wPc && Number.isFinite(P) && Number.isFinite(V)) mu += wPc * (P - V);
      else if (wPc && Number.isFinite(P) && !Number.isFinite(V)) mu = P;
      if (wForm) mu += wForm * (F - B);
      if (market === "Total score") mu += scoreFit;

      const holeAdj = holeSgByDg.get(dg);
      const distAdj = distSgByDg.get(dg);
      if (market === "Total score") {
        if (Number.isFinite(distAdj?.stpAdj)) mu += distAdj.stpAdj;
        if (holeSgIsMajor(holeAdj)) mu += holeAdj.stpAdj;
      }
      if (market === "Birdies") {
        if (distAdj) mu = applyDistanceSgToBirdies(mu, distAdj);
        if (holeSgIsMajor(holeAdj)) mu = applyHoleSgToBirdies(mu, holeAdj);
      }
      if (Number.isFinite(weatherD)) {
        mu += statWeatherMuAdjustment(market, {
          weather_temp_f: weatherSnap.tempF,
          weather_wind_mph: weatherSnap.windMph,
          weather_humidity: weatherSnap.humidityPct,
          weather_condition: weatherSnap.condition,
        });
      }
      mu += waveMarketDelta(market, waveByDg.get(dg), waveBias);

      if (market === "Total score") mu = clamp(mu, coursePar18 - 8, coursePar18 + 14);
      else if (market === "Birdies") mu = clamp(mu, 0.4, 10);
      else if (market === "Bogeys") mu = clamp(mu, 0.4, 12);
      else if (market === "GIR") mu = clamp(mu, 5, 16.5);
      else if (market === "Fairways hit") mu = clamp(mu, 2, fairwayHoles + 0.5);

      mus.set(market, mu);
    }

    const total = mus.get("Total score");
    if (Number.isFinite(total)) mus.set("__mu_sg__", coursePar18 - total);
    byDg.set(dg, mus);
  }

  return byDg;
}
