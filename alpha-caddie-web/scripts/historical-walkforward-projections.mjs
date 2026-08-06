/**
 * Walk-forward full round projections (same pipeline as fetch:dg / export-round-projection-vs-actual).
 * Hole SG + course-focused approach/putt distance SG + historical round weather (Open-Meteo archive).
 */
import { join } from "path";
import { existsSync, readFileSync } from "fs";
import {
  applyVenueCountingIntercept,
  applyVenueScoreIntercept,
  clamp,
  computeTourPriorsFromHist,
  computeVenueStatisticalIntercept,
  venueBirdieSgScale,
  fitOutcomeSigmaScales,
  setOutcomeSigmaScales,
} from "./projection-stat-model.mjs";
import { walkforwardBacktestPipelineEnv } from "./projection-pipeline-env.mjs";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  applyFieldDayCountingLiftNatural,
  applyVenueCourseFitToMu,
  blendTowardWithinEventActuals,
  buildPriorByStatForPlayer,
  buildWithinEventFormMap,
  blendedPriorRoundCourseExcess,
  courseDifficultyStrokeShift,
  emptyVenueCountRaw,
  accumulateVenueCountRow,
  finalizeVenueAgg,
  fieldCountingMeansFromEventContext,
  fieldCountingMeansFromWithinEventMap,
  ensureProjectionCourseBasisComplete,
  flatVenuePlayerScoreAnchorEnabled,
  latestVenueFieldRoundRows,
  loadCourseTableAdjRate,
  reconcileAllProjectionPlayerRows,
  resolveProjectionCounts,
  resolveProjectionScoreToPar,
  syncVenueScoringToProjectionBasis,
} from "./course-round-adjustments.mjs";
import { resolveCourseLayout } from "./course-hole-layout.mjs";
import {
  applyHoleSgToBirdies,
  buildHoleSgAdjustmentsAsOf,
  holeSgBlendEnabled,
} from "./course-hole-sg-asof.mjs";
import {
  applyDistanceSgToBirdies,
  applyGranularSgToScoreStp,
  buildDistanceSgAdjustmentsAsOf,
  distanceSgBlendEnabled,
} from "./course-distance-sg-asof.mjs";
import { traditionalRate01 } from "./dg-traditional-stats.mjs";
import {
  RAW_ROUND_SD,
  N_FAIRWAY_HOLES,
  clampMuSg,
  derivedStatsFromMuSg,
  fieldSkillMedian,
  loadHistoricalCsvCalibration,
  parseRoundMuMult,
} from "./projection-core.mjs";
import {
  birdiesPlusEaglesFromRow,
  createProjectionContext,
  num,
  ouProjectedMeanForMode,
} from "./round-projection-mu.mjs";
import { resolveCourseTableForVenue } from "./course-adaptive-pricing.mjs";
import {
  collectVenueHistRowsForSgFit,
  fitVenueSgImportanceFromRows,
  mergeSgImportance,
  serializeSgImportanceForMeta,
  applyCourseTailoringToPlayers,
  fieldSgMedians,
} from "./course-skill-tailoring.mjs";
import {
  loadHistoricalRoundWeatherMap,
  roundWeatherKey,
  DEFAULT_ROUND_WEATHER_JSON,
} from "./historical-round-weather.mjs";
import { applyWeatherBakedCountsToAllPlayers } from "./weather-projection-adjustments.mjs";
import {
  HIST_TEE_WAVE_AFTERNOON_BOGEYS,
  HIST_TEE_WAVE_AFTERNOON_BIRDIES,
  HIST_TEE_WAVE_AFTERNOON_STP,
} from "./weather-mu-adjustments.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";
import { teeWaveStrokeShift, teeWaveCountingShifts } from "./projection-unified-factors.mjs";

function envOn(name, defaultOn = true) {
  const raw = String(process.env[name] ?? "").trim();
  if (!raw) return defaultOn;
  return raw !== "0" && raw.toLowerCase() !== "false" && raw !== "off";
}

function courseSgFitEnabled() {
  return envOn("GOLF_COURSE_SG_FIT", true);
}

function teeWaveBiasFromHist(histRows, courseKey, cutoffMs) {
  const buckets = {
    morning: { n: 0, stp: 0, bird: 0, bog: 0 },
    afternoon: { n: 0, stp: 0, bird: 0, bog: 0 },
  };
  for (const row of histRows || []) {
    const t = rowTimeMs(row);
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
  // Empirically: afternoon ~+0.13 STP vs morning (paired same event-round, PGA 2015+).
  const prior = {
    deltaAfternoonMinusMorning: HIST_TEE_WAVE_AFTERNOON_STP,
    deltaBirdiesAfternoonMinusMorning: HIST_TEE_WAVE_AFTERNOON_BIRDIES,
    deltaBogeysAfternoonMinusMorning: HIST_TEE_WAVE_AFTERNOON_BOGEYS,
    n: m.n + a.n,
    source: "hist_prior_paired",
  };
  if (m.n <= 40 || a.n <= 40) return prior;
  const rawStp = a.stp / a.n - m.stp / m.n;
  const rawBird = a.bird / a.n - m.bird / m.n;
  const rawBog = a.bog / a.n - m.bog / m.n;
  const nEff = Math.min(m.n, a.n);
  const shrink = nEff / (nEff + 80);
  return {
    deltaAfternoonMinusMorning: shrink * rawStp + (1 - shrink) * HIST_TEE_WAVE_AFTERNOON_STP,
    deltaBirdiesAfternoonMinusMorning:
      shrink * rawBird + (1 - shrink) * HIST_TEE_WAVE_AFTERNOON_BIRDIES,
    deltaBogeysAfternoonMinusMorning: shrink * rawBog + (1 - shrink) * HIST_TEE_WAVE_AFTERNOON_BOGEYS,
    n: m.n + a.n,
    morning_n: m.n,
    afternoon_n: a.n,
    source: "course_hist_shrink",
  };
}

function playerWavesThisRound(histRows, eventName, eventYear, targetRound) {
  /** @type {Map<number, string>} */
  const out = new Map();
  for (const row of histRows || []) {
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

function applyWaveWeatherToPlayers(players, waveByDg, waveBias, weatherSnap, fairwayHoles) {
  if (!players?.length) return 0;
  const w = num(process.env.GOLF_UNIFIED_TEE_WAVE_W, 0.3);
  if (!(w > 0)) return 0;
  // Single archive snap: use as both AM/PM baseline; hist wave Δ + difficulty still apply.
  const morningSnap = weatherSnap || null;
  const afternoonSnap = weatherSnap || null;
  let n = 0;
  for (const pl of players) {
    const dg = Math.round(num(pl.dg_id, NaN));
    const wave = waveByDg.get(dg) || "";
    if (!wave) continue;
    pl.dg_tee_wave = wave;
    const stroke = teeWaveStrokeShift(wave, waveBias, morningSnap, afternoonSnap);
    const counts = teeWaveCountingShifts(wave, waveBias, morningSnap, afternoonSnap);
    if (Number.isFinite(stroke) && Math.abs(stroke) > 1e-5) {
      const stp = num(pl.score_to_par, NaN);
      const ts = num(pl.total_score, NaN);
      if (Number.isFinite(stp) && Number.isFinite(ts)) {
        pl.score_to_par = Math.round((stp + stroke) * 100) / 100;
        pl.total_score = Math.round((ts + stroke) * 100) / 100;
      }
      if (Number.isFinite(num(pl.mu_sg, NaN))) {
        pl.mu_sg = Math.round((num(pl.mu_sg, 0) - stroke) * 1000) / 1000;
      }
      // GIR / FW move with scoring difficulty (softer weather → more greens/fairways).
      if (Number.isFinite(num(pl.gir, NaN))) {
        pl.gir = Math.round(clamp(num(pl.gir, 0) - 0.22 * stroke, 0, 18) * 100) / 100;
      }
      if (Number.isFinite(num(pl.fairways, NaN))) {
        const fh = Number.isFinite(fairwayHoles) ? fairwayHoles : N_FAIRWAY_HOLES;
        pl.fairways = Math.round(clamp(num(pl.fairways, 0) - 0.14 * stroke, 0, fh) * 100) / 100;
      }
      n++;
    }
    if (Number.isFinite(counts?.birdies) && Math.abs(counts.birdies) > 1e-5 && Number.isFinite(num(pl.birdies, NaN))) {
      pl.birdies = Math.round((num(pl.birdies, 0) + counts.birdies) * 100) / 100;
    }
    if (Number.isFinite(counts?.bogeys) && Math.abs(counts.bogeys) > 1e-5 && Number.isFinite(num(pl.bogeys, NaN))) {
      pl.bogeys = Math.round(Math.max(0, num(pl.bogeys, 0) + counts.bogeys) * 100) / 100;
    }
    pl._tee_wave_shift = stroke;
  }
  return n;
}

function walkforwardWeatherEnabled() {
  return envOn("GOLF_WF_WEATHER", true);
}

/** @type {{ map: Map<string, object>, byName: Map<string, object> } | null} */
let wfWeatherCache = null;

function loadWalkforwardWeatherIndex(webRoot) {
  if (wfWeatherCache) return wfWeatherCache;
  const jsonPath = join(webRoot, "data", "historical_round_weather.json");
  const file = existsSync(jsonPath) ? jsonPath : DEFAULT_ROUND_WEATHER_JSON;
  const map = loadHistoricalRoundWeatherMap(file);
  /** @type {Map<string, object>} */
  const byName = new Map();
  try {
    if (existsSync(file)) {
      const raw = JSON.parse(readFileSync(file, "utf8"));
      for (const v of Object.values(raw?.byKey || {})) {
        if (!v || typeof v !== "object") continue;
        const ev = String(v.event_name || "").trim();
        const yr = Math.round(Number(v.year));
        const rnd = Math.round(Number(v.round_num));
        if (!ev || !Number.isFinite(yr) || !Number.isFinite(rnd)) continue;
        const snap = {
          tempF: Number(v.tempF ?? v.weather_temp_f),
          windMph: Number(v.windMph ?? v.weather_wind_mph),
          humidityPct: Number(v.humidityPct ?? v.weather_humidity),
          condition: String(v.condition ?? v.weather_condition ?? "default").toLowerCase(),
          priorPrecipMm: Number(v.priorPrecipMm ?? v.weather_prior_precip_mm ?? NaN),
          priorRainSoft: Boolean(v.priorRainSoft ?? v.weather_prior_rain_soft),
          event_name: ev,
        };
        if (!Number.isFinite(snap.tempF)) continue;
        byName.set(`${foldComparableTitle(ev)}|${yr}|${rnd}`, snap);
      }
    }
  } catch {
    /* ignore */
  }
  wfWeatherCache = { map, byName };
  return wfWeatherCache;
}

function inferEventId(histRows, eventName, eventYear) {
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && yr !== eventYear) continue;
    const eid = Math.round(num(row.event_id, NaN));
    if (Number.isFinite(eid)) return eid;
  }
  return NaN;
}

export function resolveWalkforwardWeather({ webRoot, histRows, eventName, eventYear, targetRound }) {
  if (!walkforwardWeatherEnabled()) return null;
  const { map, byName } = loadWalkforwardWeatherIndex(webRoot);
  const yr = Math.round(num(eventYear, NaN));
  const rnd = Math.round(num(targetRound, NaN));
  if (!Number.isFinite(yr) || !Number.isFinite(rnd)) return null;

  const eid = inferEventId(histRows, eventName, yr);
  if (Number.isFinite(eid)) {
    const snap = map.get(roundWeatherKey(eid, yr, rnd));
    if (snap && Number.isFinite(snap.tempF)) return snap;
  }

  const exact = byName.get(`${foldComparableTitle(eventName)}|${yr}|${rnd}`);
  if (exact && Number.isFinite(exact.tempF)) return exact;

  for (const [k, snap] of byName) {
    const parts = k.split("|");
    if (Number(parts[1]) !== yr || Number(parts[2]) !== rnd) continue;
    if (eventsLikelySame(eventName, snap.event_name)) return snap;
  }
  return null;
}

function attachWeatherSnapshotToPlayers(players, snap) {
  if (!snap || !players?.length) return 0;
  const priorPrecipMm = Number.isFinite(Number(snap.priorPrecipMm)) ? Number(snap.priorPrecipMm) : 0;
  const priorRainSoft = priorPrecipMm >= 0.4 || Boolean(snap.priorRainSoft);
  let n = 0;
  for (const p of players) {
    if (!p || typeof p !== "object") continue;
    p.weather_temp_f = snap.tempF;
    p.weather_wind_mph = snap.windMph;
    p.weather_humidity = snap.humidityPct;
    p.weather_condition = snap.condition || "default";
    p.weather_prior_precip_mm = priorPrecipMm;
    p.weather_prior_rain_soft = priorRainSoft;
    p.dg_auto_weather = {
      tempF: snap.tempF,
      windMph: snap.windMph,
      humidityPct: snap.humidityPct,
      condition: snap.condition || "default",
      priorPrecipMm,
      priorRainSoft,
    };
    n++;
  }
  return n;
}

function rowTimeMs(row) {
  const s = String(row?.event_completed || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) return Date.parse(`${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`);
  const yr = Math.round(num(row?.year, NaN));
  return Number.isFinite(yr) ? Date.parse(`${yr}-06-01T12:00:00Z`) : NaN;
}

function historySortKey(row) {
  const t = rowTimeMs(row);
  const rnd = Math.round(num(row.round_num, 1));
  if (Number.isFinite(t)) return Math.floor(t / 86400000) * 10 + rnd;
  const yr = Math.round(num(row.year, NaN));
  return yr * 1000 + rnd;
}

function histRoundToHistoryRec(row) {
  const rnd = Math.round(num(row.round_num, 1));
  const yr = Math.round(num(row.year, NaN));
  return {
    sortKey: historySortKey(row),
    year: yr,
    round_num: rnd,
    event_name: String(row.event_name || ""),
    course_name: String(row.course_name || ""),
    sg_total: num(row.sg_total, NaN),
    sg_ott: num(row.sg_ott, NaN),
    sg_app: num(row.sg_app, NaN),
    sg_arg: num(row.sg_arg, NaN),
    sg_putt: num(row.sg_putt, NaN),
    sg_t2g: num(row.sg_t2g, NaN),
    round_score: num(row.round_score, NaN),
    birdies: birdiesPlusEaglesFromRow(row),
    pars: num(row.pars, NaN),
    bogeys: (() => {
      const bg = num(row.bogeys ?? row.bogies, NaN);
      const d = num(row.doubles_or_worse ?? row.doubles, 0);
      if (!Number.isFinite(bg)) return NaN;
      return bg + (Number.isFinite(d) ? Math.max(0, d) : 0);
    })(),
    eagles_or_better: num(row.eagles_or_better ?? row.eagles, NaN),
    doubles_or_worse: num(row.doubles_or_worse ?? row.doubles, NaN),
    gir: num(row.gir, NaN),
    driving_acc: num(row.driving_acc, NaN),
    fairways: num(row.fairways, NaN),
    putts: num(row.putts, NaN),
    driving_dist: num(row.driving_dist, NaN),
    teetime: String(row.teetime ?? row.tee_time ?? ""),
    event_completed: String(row.event_completed || ""),
  };
}

export function buildWalkForwardHistoryByDgId(histRows, cutoffMs, dgIds) {
  /** @type {Record<string, { dg_id: number, player_name: string, rounds: object[] }>} */
  const out = {};
  const allow = dgIds instanceof Set ? dgIds : new Set(dgIds || []);

  for (const row of histRows) {
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg) || (allow.size && !allow.has(dg))) continue;
    const t = rowTimeMs(row);
    if (Number.isFinite(cutoffMs) && Number.isFinite(t) && t >= cutoffMs) continue;
    const tour = String(row.tour || "").toLowerCase();
    if (tour && tour !== "pga" && tour !== "liv") continue;
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(rs) || rs < 55 || rs > 95) continue;

    const key = String(dg);
    if (!out[key]) {
      out[key] = {
        dg_id: dg,
        player_name: String(row.player_name || ""),
        rounds: [],
      };
    }
    if (!out[key].player_name) out[key].player_name = String(row.player_name || "");
    out[key].rounds.push(histRoundToHistoryRec(row));
  }

  for (const rec of Object.values(out)) {
    rec.rounds.sort((a, b) => num(b.sortKey, 0) - num(a.sortKey, 0));
    // Keep year window for baseline blend; skill rating uses first GOLF_WF_SKILL_MAX_ROUNDS.
    const yearCap = (() => {
      const env = Math.round(num(process.env.GOLF_WF_YEAR_ROUNDS, NaN));
      return Number.isFinite(env) && env >= 12 ? Math.min(env, 200) : 48;
    })();
    const skillCap = (() => {
      const env = Math.round(num(process.env.GOLF_WF_SKILL_MAX_ROUNDS, NaN));
      return Number.isFinite(env) && env >= 2 ? Math.min(env, 200) : 12;
    })();
    const cap = Math.max(yearCap, skillCap);
    if (rec.rounds.length > cap) rec.rounds = rec.rounds.slice(0, cap);
  }
  return out;
}

function recencyWeightedMean(rows, key, decay = 0.86) {
  let sum = 0;
  let wsum = 0;
  for (let i = 0; i < rows.length; i++) {
    const v = num(rows[i][key], NaN);
    if (!Number.isFinite(v)) continue;
    const w = decay ** i;
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

function skillDecay() {
  const d = num(process.env.GOLF_WF_SKILL_DECAY, 0.86);
  return Number.isFinite(d) && d > 0.5 && d < 1 ? d : 0.86;
}

function yearDecay() {
  const d = num(process.env.GOLF_WF_YEAR_DECAY, 0.92);
  return Number.isFinite(d) && d > 0.5 && d < 1 ? d : 0.92;
}

function yearBlendWeight() {
  const w = num(process.env.GOLF_WF_YEAR_BLEND, 0.18);
  return Number.isFinite(w) ? Math.min(0.45, Math.max(0, w)) : 0.18;
}

function skillWindowN() {
  const env = Math.round(num(process.env.GOLF_WF_SKILL_MAX_ROUNDS, NaN));
  return Number.isFinite(env) && env >= 2 ? Math.min(env, 80) : 12;
}

/** Blend last-N skill (strong decay) with year baseline (mild decay). */
function blendedSkillMean(rounds, key) {
  const nSkill = skillWindowN();
  const recent = rounds.slice(0, nSkill);
  const rMean = recencyWeightedMean(recent, key, skillDecay());
  const yMean = recencyWeightedMean(rounds, key, yearDecay());
  const wY = yearBlendWeight();
  if (Number.isFinite(rMean) && Number.isFinite(yMean) && rounds.length > nSkill) {
    return (1 - wY) * rMean + wY * yMean;
  }
  return Number.isFinite(rMean) ? rMean : yMean;
}

function skillRowFromHistory(rec) {
  const rounds = Array.isArray(rec?.rounds) ? rec.rounds : [];
  if (rounds.length < 3) return null;
  const sg_total = blendedSkillMean(rounds, "sg_total");
  if (!Number.isFinite(sg_total)) return null;
  const sk = {
    sg_total,
    sg_ott: blendedSkillMean(rounds, "sg_ott"),
    sg_app: blendedSkillMean(rounds, "sg_app"),
    sg_arg: blendedSkillMean(rounds, "sg_arg"),
    sg_putt: blendedSkillMean(rounds, "sg_putt"),
    sg_t2g: blendedSkillMean(rounds, "sg_t2g"),
  };
  const girR = blendedSkillMean(
    rounds.map((r) => ({ v: traditionalRate01(r.gir, 18) })),
    "v",
  );
  const fwR = blendedSkillMean(
    rounds.map((r) => ({
      v:
        traditionalRate01(r.driving_acc, N_FAIRWAY_HOLES) ??
        traditionalRate01(r.fairways, N_FAIRWAY_HOLES),
    })),
    "v",
  );
  if (Number.isFinite(girR)) sk.dg_gir_pct = girR;
  if (Number.isFinite(fwR)) sk.dg_fairway_pct = fwR;
  // histRoundToHistoryRec stores the DK Birdies market (birdies + eagles)
  // in `birdies`; adding eagles again would double-count them.
  sk.avg_birdies = blendedSkillMean(rounds, "birdies");
  sk.avg_bogeys = blendedSkillMean(rounds, "bogeys");
  sk.avg_eagles = blendedSkillMean(
    rounds.map((r) => ({ v: num(r.eagles_or_better, num(r.eagles, 0)) })),
    "v",
  );
  sk.avg_doubles = blendedSkillMean(
    rounds.map((r) => ({ v: num(r.doubles_or_worse, num(r.doubles, 0)) })),
    "v",
  );
  sk.avg_pars = blendedSkillMean(rounds, "pars");
  sk.avg_putts = blendedSkillMean(rounds, "putts");
  sk.avg_gir = blendedSkillMean(
    rounds.map((r) => ({ v: traditionalRate01(r.gir, 18) * 18 })),
    "v",
  );
  sk.avg_fairways = blendedSkillMean(
    rounds.map((r) => ({ v: traditionalRate01(r.fairways, N_FAIRWAY_HOLES) * N_FAIRWAY_HOLES })),
    "v",
  );
  const daRaw = blendedSkillMean(rounds, "driving_acc");
  if (Number.isFinite(daRaw)) sk.driving_acc = daRaw;
  const dist = blendedSkillMean(rounds, "driving_dist");
  if (Number.isFinite(dist) && dist >= 235 && dist <= 380) sk.driving_distance = dist;
  sk.counting_rounds = rounds.filter((r) => Number.isFinite(num(r.birdies, NaN))).length;
  sk.skill_rounds = Math.min(skillWindowN(), rounds.length);
  sk.year_rounds = rounds.length;
  return sk;
}

function buildRollingTradFromHist(histRows, dgIds, cutoffMs) {
  /** @type {Map<number, { gir: number[], fw: number[] }>} */
  const buf = new Map();
  for (const row of histRows) {
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg) || !dgIds.has(dg)) continue;
    const t = rowTimeMs(row);
    if (Number.isFinite(cutoffMs) && Number.isFinite(t) && t >= cutoffMs) continue;
    let slot = buf.get(dg);
    if (!slot) {
      slot = { gir: [], fw: [] };
      buf.set(dg, slot);
    }
    const girR = traditionalRate01(row.gir, 18);
    if (Number.isFinite(girR) && slot.gir.length < 36) slot.gir.push(girR);
    const fwR = traditionalRate01(row.driving_acc, N_FAIRWAY_HOLES);
    if (Number.isFinite(fwR) && slot.fw.length < 36) slot.fw.push(fwR);
  }
  const mean = (a) => (a.length ? a.reduce((s, x) => s + x, 0) / a.length : NaN);
  /** @type {Map<number, { girRate01: number, fwRate01: number }>} */
  const out = new Map();
  for (const [id, slot] of buf) {
    const girRate01 = mean(slot.gir);
    const fwRate01 = mean(slot.fw);
    if (Number.isFinite(girRate01) || Number.isFinite(fwRate01)) out.set(id, { girRate01, fwRate01 });
  }
  return out;
}

function buildWithinEventCountingMap(histRows, eventName, eventYear, courseKey, targetRound, venueScoring) {
  /** @type {Map<number, Map<number, object>>} */
  const out = new Map();
  const cpDefault = 72;
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && Number.isFinite(yr) && yr !== eventYear) continue;
    if (courseKey) {
      const ck = normCourseNameKey(row.course_name || "");
      if (ck && ck !== courseKey) continue;
    }
    const rnd = Math.round(num(row.round_num, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd >= targetRound) continue;
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const rec = {
      birdies: birdiesPlusEaglesFromRow(row),
      bogeys: num(row.bogeys ?? row.bogies, NaN),
      gir: num(row.gir, NaN),
      round_score: num(row.round_score, NaN),
    };
    let per = out.get(dg);
    if (!per) {
      per = new Map();
      out.set(dg, per);
    }
    per.set(rnd, rec);
  }
  return out;
}

function buildEventContextFromHist(histRows, eventName, eventYear, courseKey, targetRound) {
  const ctx = { byRound: new Map(), playerRounds: [] };
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && Number.isFinite(yr) && yr !== eventYear) continue;
    if (courseKey) {
      const ck = normCourseNameKey(row.course_name || "");
      if (ck && ck !== courseKey) continue;
    }
    const rnd = Math.round(num(row.round_num, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd >= targetRound) continue;
    const cp = num(row.course_par, NaN);
    const rs = num(row.round_score, NaN);
    if (Number.isFinite(cp) && Number.isFinite(rs)) {
      const stp = rs - cp;
      const b = ctx.byRound.get(rnd) || { n: 0, sumStp: 0, sumBird: 0, nBird: 0, sumBog: 0, nBog: 0, sumGir: 0, nGir: 0 };
      b.n++;
      b.sumStp += stp;
      const bird = birdiesPlusEaglesFromRow(row);
      if (Number.isFinite(bird)) {
        b.sumBird += bird;
        b.nBird++;
      }
      const bog = num(row.bogeys ?? row.bogies, NaN);
      if (Number.isFinite(bog)) {
        b.sumBog += bog;
        b.nBog++;
      }
      ctx.byRound.set(rnd, b);
    }
    const dg = Math.round(num(row.dg_id, NaN));
    const sg = num(row.sg_total, NaN);
    if (Number.isFinite(dg) && Number.isFinite(sg)) {
      ctx.playerRounds.push({ dg_id: dg, round: rnd, sg_total: sg });
    }
  }
  return ctx;
}

export async function loadVenueScoringBeforeCutoff(histRows, courseKey, courseLabel, cutoffMs, eventName, eventYear, targetRound) {
  const nFairwayHoles = N_FAIRWAY_HOLES;
  let venueTotals = emptyVenueCountRaw();
  const fieldRaw = new Map();
  const playerRaw = new Map();
  const playerAllRaw = new Map();
  const fitRaw = new Map();
  const eligibleVenueRows = [];

  function histRowToVenueRow(row) {
    return {
      course_par: num(row.course_par, NaN),
      round_score: num(row.round_score, NaN),
      birdies: num(row.birdies, NaN),
      eagles_or_better: num(row.eagles_or_better ?? row.eagles, NaN),
      pars: num(row.pars, NaN),
      bogies: num(row.bogeys ?? row.bogies, NaN),
      doubles_or_worse: num(row.doubles_or_worse ?? row.doubles, NaN),
      gir: num(row.gir, NaN),
      driving_acc: num(row.driving_acc, NaN),
    };
  }

  for (const row of histRows) {
    const t = rowTimeMs(row);
    const yr = Math.round(num(row.year, NaN));
    const rnd = Math.round(num(row.round_num, NaN));
    const sameCurrentEvent =
      eventsLikelySame(eventName, String(row.event_name || "").trim()) &&
      (!Number.isFinite(eventYear) || !Number.isFinite(yr) || yr === eventYear);
    const completedEarlierThisEvent =
      sameCurrentEvent && Number.isFinite(rnd) && rnd >= 1 && rnd < targetRound;
    if (
      Number.isFinite(cutoffMs) &&
      Number.isFinite(t) &&
      t >= cutoffMs &&
      !completedEarlierThisEvent
    ) {
      continue;
    }
    if (sameCurrentEvent && Number.isFinite(rnd) && rnd >= targetRound) continue;
    const cp = num(row.course_par, NaN);
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(cp) || cp < 63 || cp > 76) continue;
    if (!Number.isFinite(rs) || rs < 55 || rs > 95) continue;
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;

    const ckRow = normCourseNameKey(row.course_name || "");
    if (!courseKey || !ckRow || ckRow !== courseKey) continue;

    const vr = histRowToVenueRow(row);
    eligibleVenueRows.push(row);

    const dg = Math.round(num(row.dg_id, NaN));
    if (Number.isFinite(dg)) {
      const pk = `${dg}|${rnd}`;
      playerRaw.set(pk, accumulateVenueCountRow(playerRaw.get(pk) || emptyVenueCountRaw(), vr, nFairwayHoles));
      playerAllRaw.set(dg, accumulateVenueCountRow(playerAllRaw.get(dg) || emptyVenueCountRaw(), vr, nFairwayHoles));
      const sg = num(row.sg_total, NaN);
      if (Number.isFinite(sg)) {
        const cf = fitRaw.get(dg) || { sumSg: 0, n: 0 };
        cf.sumSg += sg;
        cf.n++;
        fitRaw.set(dg, cf);
      }
    }
  }

  const rolling = latestVenueFieldRoundRows(eligibleVenueRows, courseKey, 4);
  for (const row of rolling.rows) {
    const rnd = Math.round(num(row.round_num, NaN));
    const vr = histRowToVenueRow(row);
    venueTotals = accumulateVenueCountRow(venueTotals, vr, nFairwayHoles);
    fieldRaw.set(
      rnd,
      accumulateVenueCountRow(fieldRaw.get(rnd) || emptyVenueCountRaw(), vr, nFairwayHoles),
    );
  }

  const venueAgg = finalizeVenueAgg(venueTotals);
  const fieldByRound = new Map();
  for (const [rnd, raw] of fieldRaw) fieldByRound.set(rnd, finalizeVenueAgg(raw));
  const playerByRound = new Map();
  for (const [pk, raw] of playerRaw) playerByRound.set(pk, finalizeVenueAgg(raw));
  const playerByVenue = new Map();
  for (const [dg, raw] of playerAllRaw) playerByVenue.set(dg, finalizeVenueAgg(raw));
  const courseFitByDg = new Map();
  for (const [dg, raw] of fitRaw) courseFitByDg.set(dg, { avgSg: raw.sumSg / raw.n, n: raw.n });

  return {
    venueAvgStp: venueAgg.n > 0 ? venueAgg.avgStp : NaN,
    venueAvgScore: venueAgg.n > 0 ? venueAgg.avgScore : NaN,
    nVenueRounds: venueAgg.n,
    source: venueAgg.n > 0 ? "rolling_4_course_rounds_walkforward" : "none",
    rollingCourseRoundKeys: rolling.roundKeys,
    venueAvgBirdies: venueAgg.avgBirdies,
    venueAvgEagles: venueAgg.avgEagles,
    historicalVenueAvgBirdies: venueAgg.avgBirdies,
    historicalVenueAvgEagles: venueAgg.avgEagles,
    birdieTargetSource: venueAgg.n > 0 ? "rolling_4_course_rounds" : "none",
    birdieTargetRounds: rolling.roundKeys.length,
    venueAvgPars: venueAgg.avgPars,
    venueAvgBogeys: venueAgg.avgBogeys,
    venueAvgDoubles: venueAgg.avgDoubles,
    venueAvgGir: venueAgg.avgGir,
    venueAvgFairways: venueAgg.avgFairways,
    venueAvgPutts: venueAgg.avgPutts,
    fieldByRound,
    playerByRound,
    playerByVenue,
    courseFitByDg,
  };
}

/** @type {Map<string, Promise<object>>} */
const histCalibCache = new Map();

async function loadHistoricalCsvCalibrationCached(repoRoot, courseKey) {
  const key = courseKey || "__all__";
  if (!histCalibCache.has(key)) {
    const prevLog = console.log;
    console.log = (...args) => {
      if (!String(args[0] || "").includes("[fetch-dg] historical calibration")) prevLog(...args);
    };
    histCalibCache.set(key, loadHistoricalCsvCalibration(repoRoot, courseKey).finally(() => {
      console.log = prevLog;
    }));
  }
  return histCalibCache.get(key);
}

export function inferCourseParFromHist(histRows, eventName, eventYear, courseKey) {
  return inferCoursePar(histRows, eventName, eventYear, courseKey);
}

export function inferCourseNameFromHist(histRows, eventName, eventYear) {
  return inferCourseName(histRows, eventName, eventYear);
}

function inferCoursePar(histRows, eventName, eventYear, courseKey) {
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && yr !== eventYear) continue;
    if (courseKey && normCourseNameKey(row.course_name || "") !== courseKey) continue;
    const cp = num(row.course_par, NaN);
    if (Number.isFinite(cp) && cp >= 63 && cp <= 76) return cp;
  }
  return 72;
}

function inferCourseName(histRows, eventName, eventYear) {
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && yr !== eventYear) continue;
    const c = String(row.course_name || "").trim();
    if (c) return c;
  }
  return "";
}

/**
 * Build full-model μ for one player×round (default pricing mode — same as live export).
 * @returns {Promise<Map<string, number>>} market label -> μ
 */
export async function buildFullModelMuMapForEvent({
  repoRoot,
  histRows,
  eventName,
  eventYear,
  targetRound,
  betTimeMs,
  fieldDgIds,
  courseName: courseNameOverride = "",
  pipelineEnv = null,
}) {
  Object.assign(process.env, walkforwardBacktestPipelineEnv(), pipelineEnv || {});
  if (
    String(process.env.GOLF_DG_METHODOLOGY || "").trim() === "1" ||
    ["true", "yes", "on"].includes(String(process.env.GOLF_DG_METHODOLOGY || "").trim().toLowerCase())
  ) {
    const { buildDgMethodologyMuMapForEvent } = await import("./dg-methodology-mu.mjs");
    return buildDgMethodologyMuMapForEvent({
      repoRoot,
      histRows,
      eventName,
      eventYear,
      targetRound,
      betTimeMs,
      fieldDgIds,
      courseName: courseNameOverride,
    });
  }
  if (String(process.env.GOLF_STRICT_FIT_FORM || "").trim() === "1") {
    const { buildStrictFitFormMuMapForEvent } = await import("./strict-fit-form-mu.mjs");
    return buildStrictFitFormMuMapForEvent({
      repoRoot,
      histRows,
      eventName,
      eventYear,
      targetRound,
      betTimeMs,
      fieldDgIds,
      courseName: courseNameOverride,
    });
  }
  const dgSet = new Set(fieldDgIds.filter((d) => Number.isFinite(d)));
  if (!dgSet.size) return new Map();

  const courseName =
    String(courseNameOverride || "").trim() || inferCourseName(histRows, eventName, eventYear);
  const courseKey = normCourseNameKey(courseName);
  const layout = resolveCourseLayout({
    coursePar18: inferCoursePar(histRows, eventName, eventYear, courseKey),
    courseUsed: courseName,
    eventName,
    webRoot: join(repoRoot, "alpha-caddie-web"),
  });
  const coursePar18 = layout.course_par_18;
  const fairwayHoles = layout.fairway_holes_modeled;
  const webRoot = join(repoRoot, "alpha-caddie-web");
  const weatherSnap = resolveWalkforwardWeather({
    webRoot,
    histRows,
    eventName,
    eventYear,
    targetRound,
  });

  const historyByDgId = buildWalkForwardHistoryByDgId(histRows, betTimeMs, dgSet);
  const rollingTrad = buildRollingTradFromHist(histRows, dgSet, betTimeMs);

  const [histCalib, venueScoring, holeSgByDg, distSgByDg] = await Promise.all([
    loadHistoricalCsvCalibrationCached(repoRoot, courseKey),
    loadVenueScoringBeforeCutoff(histRows, courseKey, courseName, betTimeMs, eventName, eventYear, targetRound),
    holeSgBlendEnabled()
      ? buildHoleSgAdjustmentsAsOf({
          webRoot,
          courseKey,
          courseName,
          cutoffMs: betTimeMs,
          eventName,
          eventYear,
          targetRound,
          fieldDgIds: dgSet,
        })
      : Promise.resolve(new Map()),
    distanceSgBlendEnabled()
      ? buildDistanceSgAdjustmentsAsOf({
          webRoot,
          courseKey,
          courseName,
          cutoffMs: betTimeMs,
          eventName,
          eventYear,
          targetRound,
          fieldDgIds: dgSet,
        })
      : Promise.resolve(new Map()),
  ]);

  const tourPriors = computeTourPriorsFromHist(histRows, betTimeMs);
  const venueScoreIntercept = computeVenueStatisticalIntercept(histRows, courseKey, betTimeMs, tourPriors);
  const venueBirdMkt =
    num(venueScoring.venueAvgBirdies, NaN) + Math.max(0, num(venueScoring.venueAvgEagles, 0));
  const birdSgScale = venueBirdieSgScale(venueBirdMkt, tourPriors.avgBirdMkt);
  const countOpts = {
    venueBirdieSgScale: birdSgScale,
  };

  const histEventCtx = buildEventContextFromHist(histRows, eventName, eventYear, courseKey, targetRound);
  const withinEventCountingMap = buildWithinEventCountingMap(
    histRows,
    eventName,
    eventYear,
    courseKey,
    targetRound,
    venueScoring,
  );

  const courseFairwayRate01 = loadCourseTableAdjRate(courseName, "adj_driving_accuracy");
  const courseGirRate01 = loadCourseTableAdjRate(courseName, "adj_gir");
  const courseAdjDrivingDistance = loadCourseTableAdjRate(courseName, "adj_driving_distance");
  const courseFwWidth = loadCourseTableAdjRate(courseName, "fw_width");
  const courseFwDifficulty = loadCourseTableAdjRate(courseName, "fw_diff");
  const courseAdjScoreToPar = loadCourseTableAdjRate(courseName, "adj_score_to_par");
  const courseFwWidthNorm = Number.isFinite(courseFwWidth)
    ? Math.max(0, Math.min(1, (courseFwWidth - 23.5) / (71.9 - 23.5)))
    : NaN;
  const courseBirdieEase = Number.isFinite(courseAdjScoreToPar) ? -0.12 * courseAdjScoreToPar : 0;
  const courseSkillAnchor = Number.isFinite(courseFairwayRate01) || Number.isFinite(courseGirRate01);
  const courseCountOpts = {
    courseFairwayRate01,
    courseGirRate01,
    courseFwWidthNorm,
    courseAdjDrivingDistance,
    courseFwDifficulty,
    courseBirdieEase,
  };

  const base = [];
  for (const dg of dgSet) {
    const rec = historyByDgId[String(dg)];
    const skRow = skillRowFromHistory(rec);
    if (!skRow) continue;
    let mu_sg = skRow.sg_total;
    const liveTrad = rollingTrad.get(dg) || null;
    if (liveTrad) {
      if (Number.isFinite(liveTrad.girRate01)) skRow.dg_gir_pct = liveTrad.girRate01;
      if (Number.isFinite(liveTrad.fwRate01)) skRow.dg_fairway_pct = liveTrad.fwRate01;
    }
    const im = derivedStatsFromMuSg(mu_sg, fairwayHoles, {
      histCountFit: histCalib,
      skRow,
      liveTrad,
      fieldMeanApp: NaN,
      fieldMeanOtt: NaN,
      sg_ott: skRow.sg_ott,
      sg_app: skRow.sg_app,
      venueBird: venueScoring.venueAvgBirdies,
      venueBog: venueScoring.venueAvgBogeys,
      venuePars: venueScoring.venueAvgPars,
      venueGir: venueScoring.venueAvgGir,
      venueFairways: venueScoring.venueAvgFairways,
      venuePutts: venueScoring.venueAvgPutts,
      courseFairwayRate01,
      courseGirRate01,
      ...countOpts,
      ...courseCountOpts,
    });
    base.push({
      dg_id: dg,
      player_name: rec?.player_name || "",
      mu_sg,
      implied_mu_sg: mu_sg,
      ...skRow,
      eagles: im.eagles,
      birdies: im.birdies,
      pars: im.pars,
      bogeys: im.bogeys,
      doubles: im.doubles,
      gir: im.gir,
      fairways: im.fairways,
      putts: im.putts,
    });
  }

  if (!base.length) return new Map();

  const fieldMeanDgFairways14 =
    base.reduce((s, r) => s + num(r.fairways, 0), 0) / Math.max(1, base.length);
  const driveSamples = base
    .map((r) => num(r.driving_distance, NaN))
    .filter((d) => Number.isFinite(d) && d >= 235 && d <= 380);
  const fieldMeanDrive = driveSamples.length
    ? driveSamples.reduce((s, d) => s + d, 0) / driveSamples.length
    : courseAdjDrivingDistance;

  const fieldMeanMu = base.reduce((s, r) => s + num(r.mu_sg, 0), 0) / base.length;
  for (const row of base) {
    row.mu_sg = applyVenueCourseFitToMu(row.mu_sg, row.dg_id, venueScoring, fieldMeanMu);
    row.implied_mu_sg = row.mu_sg;
  }
  const ctRow = resolveCourseTableForVenue(courseName);
  const venueFitRows = collectVenueHistRowsForSgFit(histRows, courseKey, betTimeMs, rowTimeMs);
  const sgImportance = mergeSgImportance(fitVenueSgImportanceFromRows(venueFitRows), ctRow);
  const fieldMeanMuAdj = base.reduce((s, r) => s + num(r.mu_sg, 0), 0) / base.length;

  const ottSamples = base.map((r) => num(r.sg_ott, NaN)).filter(Number.isFinite);
  const appSamples = base.map((r) => num(r.sg_app, NaN)).filter(Number.isFinite);
  const puttSamples = base.map((r) => num(r.sg_putt, NaN)).filter(Number.isFinite);
  const argSamples = base.map((r) => num(r.sg_arg, NaN)).filter(Number.isFinite);
  const fieldMeanOtt = fieldSkillMedian(ottSamples);
  const fieldMeanApp = fieldSkillMedian(appSamples);
  const fieldMeanPutt = fieldSkillMedian(puttSamples);
  const fieldMeanArg = fieldSkillMedian(argSamples);

  const flatVenue = flatVenuePlayerScoreAnchorEnabled();
  const formK = flatVenue ? 0 : num(process.env.GOLF_WITHIN_EVENT_FORM_CARRY, 0.1);
  const withinFormMap =
    formK !== 0 && histEventCtx.playerRounds.length
      ? buildWithinEventFormMap(
          histEventCtx,
          base.map((r) => ({ dg_id: r.dg_id, mu_sg: r.mu_sg })),
          formK,
          num(process.env.GOLF_WITHIN_EVENT_FORM_CAP, 0.75),
        )
      : new Map();

  const priorExcess = flatVenue
    ? NaN
    : blendedPriorRoundCourseExcess(null, histEventCtx, targetRound, eventName, courseKey);
  const strokeShiftPrior =
    flatVenue || !Number.isFinite(priorExcess) ? 0 : courseDifficultyStrokeShift(priorExcess);

  const fieldCountingFromEvent = fieldCountingMeansFromEventContext(histEventCtx);
  const fieldCountingFromHistory =
    withinEventCountingMap.size > 0 ? fieldCountingMeansFromWithinEventMap(withinEventCountingMap) : null;
  const fieldCountingMeans = fieldCountingFromEvent?.birdies?.[1]
    ? fieldCountingFromEvent
    : fieldCountingFromHistory?.birdies?.[1]
      ? fieldCountingFromHistory
      : fieldCountingFromEvent || fieldCountingFromHistory;

  const roundMuMult = parseRoundMuMult();
  const flatRoundMuAfterR1 =
    !flatVenue &&
    targetRound >= 2 &&
    String(process.env.GOLF_ROUND_MU_FLAT_AFTER_R1 || "").trim() === "1" &&
    (histEventCtx?.playerRounds?.length > 0 || withinEventCountingMap.size > 0);
  const mult = flatVenue || flatRoundMuAfterR1 ? 1 : num(roundMuMult[targetRound - 1], 1);
  const players = [];

  for (const row of base) {
    const formShift = formK !== 0 ? num(withinFormMap.get(`${row.dg_id}|${targetRound}`), 0) : 0;
    let muForRound = targetRound === 1 ? row.mu_sg : row.mu_sg * mult;
    muForRound = clampMuSg(muForRound - strokeShiftPrior + formShift);

    const skRowR = {
      sg_total: row.sg_total,
      sg_ott: row.sg_ott,
      sg_app: row.sg_app,
      sg_arg: row.sg_arg,
      sg_putt: row.sg_putt,
      sg_t2g: row.sg_t2g,
      dg_gir_pct: row.dg_gir_pct,
      dg_fairway_pct: row.dg_fairway_pct,
      avg_birdies: row.avg_birdies,
      avg_bogeys: row.avg_bogeys,
      avg_eagles: row.avg_eagles,
      avg_doubles: row.avg_doubles,
      avg_pars: row.avg_pars,
      avg_putts: row.avg_putts,
      avg_gir: row.avg_gir,
      avg_fairways: row.avg_fairways,
      counting_rounds: row.counting_rounds,
      driving_acc: row.driving_acc,
      driving_distance: row.driving_distance,
    };
    const liveTrad = rollingTrad.get(row.dg_id) || null;

    let st =
      targetRound === 1 && strokeShiftPrior === 0 && formShift === 0
        ? {
            mu_sg: row.mu_sg,
            implied_mu_sg: row.implied_mu_sg,
            eagles: row.eagles,
            birdies: row.birdies,
            pars: row.pars,
            bogeys: row.bogeys,
            doubles: row.doubles,
            gir: row.gir,
            fairways: row.fairways,
            putts: row.putts,
          }
        : derivedStatsFromMuSg(muForRound, fairwayHoles, {
            histCountFit: histCalib,
            skRow: skRowR,
            liveTrad,
            fieldMeanOtt,
            fieldMeanApp,
            fieldMeanPutt,
            fieldMeanArg,
            sg_ott: row.sg_ott,
            sg_app: row.sg_app,
            driving_distance: row.driving_distance,
            nGirHoles: 18,
            venueBird: venueScoring.venueAvgBirdies,
            venueBog: venueScoring.venueAvgBogeys,
            venuePars: venueScoring.venueAvgPars,
            venueGir: venueScoring.venueAvgGir,
            venueFairways: venueScoring.venueAvgFairways,
            venuePutts: venueScoring.venueAvgPutts,
            courseFairwayRate01,
            courseGirRate01,
            fieldMeanDgFairways14,
            fieldMeanDrive,
            fieldMeanT2g: fieldSkillMedian(base.map((r) => num(r.sg_t2g, NaN)).filter(Number.isFinite)),
            ...countOpts,
            ...courseCountOpts,
          });

    const scoreRes = resolveProjectionScoreToPar({
      dg_id: row.dg_id,
      round: targetRound,
      muForRound,
      course_par_18: coursePar18,
      venueScoring,
      pretRoundScore: NaN,
      fieldMeanMu: fieldMeanMuAdj,
      courseAdjStp: courseAdjScoreToPar,
    });
    const holeAdj = holeSgByDg.get(Math.round(num(row.dg_id, NaN)));
    const distAdj = distSgByDg.get(Math.round(num(row.dg_id, NaN)));
    const scored = applyGranularSgToScoreStp(scoreRes.stp, holeAdj, distAdj, scoreRes.source);
    const stp = scored.stp;
    const ts = coursePar18 + stp;

    const venueCounts = resolveProjectionCounts({
      dg_id: row.dg_id,
      round: targetRound,
      muForRound,
      skillCounts: {
        eagles: st.eagles,
        birdies: st.birdies,
        pars: st.pars,
        bogeys: st.bogeys,
        doubles: st.doubles,
        gir: st.gir,
        fairways: st.fairways,
        putts: st.putts,
      },
      venueScoring,
      targetStp: stp,
      nFairwayHoles: fairwayHoles,
      courseSkillAnchor,
    });
    st.eagles = venueCounts.eagles;
    st.birdies = applyDistanceSgToBirdies(applyHoleSgToBirdies(venueCounts.birdies, holeAdj), distAdj);
    st.pars = venueCounts.pars;
    st.bogeys = venueCounts.bogeys;
    st.doubles = venueCounts.doubles;
    st.gir = venueCounts.gir;
    st.fairways = venueCounts.fairways;
    st.putts = venueCounts.putts;

    if (targetRound >= 2 && withinEventCountingMap.size) {
      const priorByStat = buildPriorByStatForPlayer(withinEventCountingMap, row.dg_id, targetRound);
      if (priorByStat) {
        const skillBeforeBlend = { ...st };
        const blended = blendTowardWithinEventActuals({ ...skillBeforeBlend }, priorByStat, targetRound, {
          playerRow: row,
          skillCounts: skillBeforeBlend,
          fieldMeans: fieldCountingMeans,
        });
        Object.assign(st, blended);
      }
    }
    if (targetRound >= 2 && fieldCountingMeans) {
      applyFieldDayCountingLiftNatural(st, targetRound, fieldCountingMeans, venueScoring);
    }

    players.push({
      dg_id: row.dg_id,
      player_name: row.player_name,
      round: targetRound,
      mu_sg: Math.round(st.mu_sg * 1000) / 1000,
      total_score: Math.round(ts * 100) / 100,
      score_to_par: Math.round(stp * 100) / 100,
      score_source: scored.source,
      round_sd: RAW_ROUND_SD,
      eagles: Math.round(st.eagles * 1000) / 1000,
      birdies: Math.round(st.birdies * 100) / 100,
      pars: Math.round(st.pars * 100) / 100,
      bogeys: Math.round(st.bogeys * 100) / 100,
      gir: Math.round(st.gir * 100) / 100,
      fairways: Math.round(st.fairways * 100) / 100,
      putts: Math.round(st.putts * 100) / 100,
      sg_total: row.sg_total,
      sg_ott: row.sg_ott,
      sg_app: row.sg_app,
      sg_arg: row.sg_arg,
      sg_putt: row.sg_putt,
      sg_t2g: row.sg_t2g,
      driving_distance: row.driving_distance,
    });
    const lastPl = players[players.length - 1];
    if (venueScoreIntercept?.scoreStp && venueScoreIntercept.nEff >= 35 && Math.abs(venueScoreIntercept.scoreStp) >= 0.12) {
      const flatVenue = flatVenuePlayerScoreAnchorEnabled();
      const scoreW = flatVenue
        ? clamp(0.38 + 0.1 * Math.log10(venueScoreIntercept.nEff / 35), 0.38, 0.62)
        : clamp(0.16 + 0.08 * Math.log10(venueScoreIntercept.nEff / 35), 0.16, 0.32);
      // Slight optimistic offset: DK-paired score μ ran ~0.3 high vs actual.
      const scoreShift =
        venueScoreIntercept.scoreStp * scoreW - Math.sign(venueScoreIntercept.scoreStp || 1) * 0.08;
      applyVenueScoreIntercept(lastPl, { scoreStp: scoreShift }, coursePar18);
    }
    if (venueScoreIntercept?.nEff >= 30) {
      const countW = clamp(venueScoreIntercept.nEff / (venueScoreIntercept.nEff + 32), 0.5, 0.92);
      applyVenueCountingIntercept(
        lastPl,
        {
          // Birdies are field-calibrated to the recency-weighted venue BoB target;
          // do not also apply the shrunk tour-relative bird intercept.
          birdMkt: 0,
          gir: venueScoreIntercept.gir * countW,
          fw: venueScoreIntercept.fw * countW,
        },
        fairwayHoles,
      );
    }
  }

  // Course SG-importance fit + recent form (8–12) baked into rows before weather/wave.
  const waveByDg = playerWavesThisRound(histRows, eventName, eventYear, targetRound);
  const waveBias = teeWaveBiasFromHist(histRows, courseKey, betTimeMs);
  /** @type {Map<number, number>} */
  const teeWaveShiftByDg = new Map();
  if (courseSgFitEnabled()) {
    const fieldMedians = fieldSgMedians(base);
    applyCourseTailoringToPlayers(players, {
      historyByDgId,
      sgImportance,
      fieldMedians,
      venueScoring,
      ctRow,
      coursePar18,
      teeWaveShiftByDg,
    });
  }
  const nWave = applyWaveWeatherToPlayers(players, waveByDg, waveBias, weatherSnap, fairwayHoles);

  const meta = {
    display_round: targetRound,
    course_used: courseName,
    course_par_18: coursePar18,
    projection_course_basis: {
      fairway_holes_modeled: fairwayHoles,
      course_adj_fairway_rate: Number.isFinite(courseFairwayRate01) ? courseFairwayRate01 : undefined,
      course_adj_gir_rate: Number.isFinite(courseGirRate01) ? courseGirRate01 : undefined,
      course_adj_score_to_par: Number.isFinite(courseAdjScoreToPar) ? courseAdjScoreToPar : undefined,
      course_birdie_ease: Number.isFinite(courseBirdieEase) ? courseBirdieEase : undefined,
    },
    projection_counts_weather_baked: false,
    projection_round_adjustments: {
      flat_venue_player_score: flatVenuePlayerScoreAnchorEnabled(),
      hole_sg_blend: holeSgBlendEnabled(),
      distance_sg_blend: distanceSgBlendEnabled(),
      weather_blend: Boolean(weatherSnap),
      course_sg_fit: courseSgFitEnabled(),
      skill_rounds: skillWindowN(),
      year_blend: yearBlendWeight(),
      tee_wave_players: nWave,
    },
    tee_wave_bias: waveBias,
  };
  syncVenueScoringToProjectionBasis(meta.projection_course_basis, venueScoring, coursePar18);
  // Walk-forward Birdies: recency-weighted venue BoB (all prior rounds, no min N).
  if (Number.isFinite(num(venueScoring.historicalVenueAvgBirdies, NaN))) {
    meta.projection_course_basis.historical_venue_avg_birdies =
      Math.round(venueScoring.historicalVenueAvgBirdies * 1000) / 1000;
  }
  if (Number.isFinite(num(venueScoring.historicalVenueAvgEagles, NaN))) {
    meta.projection_course_basis.historical_venue_avg_eagles =
      Math.round(venueScoring.historicalVenueAvgEagles * 1000) / 1000;
  }
  meta.projection_course_basis.birdie_target_source = venueScoring.birdieTargetSource;
  meta.projection_course_basis.birdie_target_rounds = venueScoring.birdieTargetRounds;
  meta.projection_course_basis.course_sg_importance = serializeSgImportanceForMeta(sgImportance);
  if (fieldCountingMeans) {
    meta.projection_course_basis.field_counting_means_by_round = fieldCountingMeans;
  }
  if (histEventCtx?.byRound?.size) {
    /** @type {Record<string, number>} */
    const ewScores = {};
    for (const [rnd, agg] of histEventCtx.byRound) {
      if (agg.n >= 8 && Number.isFinite(agg.sumStp) && agg.n > 0) {
        ewScores[String(rnd)] = Math.round((coursePar18 + agg.sumStp / agg.n) * 100) / 100;
      }
    }
    if (Object.keys(ewScores).length) {
      meta.projection_course_basis.event_week_field_avg_score_by_round = ewScores;
    }
  }

  const payload = {
    meta,
    course_used: courseName,
    course_par_18: coursePar18,
    players,
    venueScoring,
    historical_projection_calibration: histCalib,
    _webRoot: webRoot,
    display_round: targetRound,
  };
  payload.projection_course_basis = ensureProjectionCourseBasisComplete(
    meta.projection_course_basis,
    payload,
  );
  reconcileAllProjectionPlayerRows(payload, {
    minField: Math.min(8, players.length),
    venueScoring,
    skipMarketBookCalibration: true,
    skipEventPropBookAlignment: true,
    birdieFieldCalibStrength: 1,
    girBlend: 0,
    fairwaysBlend: 0,
  });

  if (weatherSnap) {
    attachWeatherSnapshotToPlayers(payload.players || players, weatherSnap);
    const nWx = applyWeatherBakedCountsToAllPlayers(payload, {
      forecastRound: targetRound,
      displayRound: targetRound,
      skipFieldCalibrate: true,
      minField: Math.min(8, players.length),
    });
    meta.projection_counts_weather_baked = nWx > 0;
    meta.projection_counts_weather_baked_round = targetRound;
    meta.projection_round_adjustments.weather = {
      tempF: weatherSnap.tempF,
      windMph: weatherSnap.windMph,
      humidityPct: weatherSnap.humidityPct,
      condition: weatherSnap.condition || "default",
      source: "open_meteo_archive",
    };
  }

  const playersReconciled = payload.players || players;

  payload.meta.historyByDgId = historyByDgId;
  const ctx = createProjectionContext(payload);
  ctx.historyByDgId = historyByDgId;
  ctx.modelRound = targetRound;

  /** @type {Map<number, Map<string, number>>} dg -> market -> mu */
  const byDg = new Map();
  const ALL_MARKETS = ["Total score", "Birdies", "Pars", "Bogeys", "GIR", "Fairways hit"];
  for (const pl of playersReconciled) {
    const dg = Math.round(num(pl.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const mus = new Map();
    mus.set("Total score", pl.total_score);
    mus.set("Birdies", num(pl.birdies, NaN) + num(pl.eagles, 0));
    mus.set("Pars", pl.pars);
    mus.set("Bogeys", pl.bogeys);
    mus.set("GIR", pl.gir);
    mus.set("Fairways hit", pl.fairways);
    const muSg = num(pl.mu_sg, NaN);
    if (Number.isFinite(muSg)) mus.set("__mu_sg__", muSg);
    for (const market of ALL_MARKETS) {
      // Birdies were already resolved from rolling BoB%, player-at-course,
      // spread-keep, and field-calibrated to the all-history venue target.
      // The generic O/U path adds SG/course tailoring, which would move the
      // final exported mean away from that target.
      if (market === "Birdies") continue;
      const mu = ouProjectedMeanForMode(market, pl, meta, "default", "default", ctx);
      if (Number.isFinite(mu)) mus.set(market, mu);
    }
    byDg.set(dg, mus);
  }
  return byDg;
}

/** Cache full-model μ lookups across odds props (keyed by event×year×round×betTime×course×field). */
export class FullModelProjectionCache {
  constructor(repoRoot, histRows) {
    this.repoRoot = repoRoot;
    this.histRows = histRows;
    /** @type {Map<string, Map<number, Map<string, number>>>} */
    this.cache = new Map();
  }

  eventKey(p) {
    const bt = Number.isFinite(p.bet_time_ms) ? Math.round(p.bet_time_ms) : "na";
    const course = String(p.course || "")
      .trim()
      .toLowerCase()
      .slice(0, 48);
    const nField = Array.isArray(p._field_dg_ids) ? p._field_dg_ids.length : 0;
    return `${p.year}|${foldComparableTitle(p.event)}|${p.round}|${bt}|${course}|${nField}`;
  }

  async ensureEvent(p) {
    const key = this.eventKey(p);
    if (this.cache.has(key)) return this.cache.get(key);
    const map = await buildFullModelMuMapForEvent({
      repoRoot: this.repoRoot,
      histRows: this.histRows,
      eventName: p.event,
      eventYear: p.year,
      targetRound: p.round,
      betTimeMs: p.bet_time_ms,
      fieldDgIds: p._field_dg_ids || (p.dg_id ? [Math.round(p.dg_id)] : []),
      courseName: p.course,
    });
    this.cache.set(key, map);
    return map;
  }

  async prewarm(props) {
    const seen = new Set();
    const keys = [];
    for (const p of props.values()) {
      const k = this.eventKey(p);
      if (seen.has(k)) continue;
      seen.add(k);
      keys.push(p);
    }
    console.log(`  ${keys.length} unique event×round bundles …`);
    let i = 0;
    for (const p of keys) {
      await this.ensureEvent(p);
      i++;
      if (i % 10 === 0 || i === keys.length) process.stdout.write(`\r  Projections ${i}/${keys.length}`);
    }
    process.stdout.write("\n");
  }

  async muForProp(p, dgId, marketLabel) {
    if (!Number.isFinite(dgId)) return NaN;
    const map = await this.ensureEvent(p);
    return map.get(Math.round(dgId))?.get(marketLabel) ?? NaN;
  }

  async muSgForProp(p, dgId) {
    return this.muForProp(p, dgId, "__mu_sg__");
  }
}

export function attachFieldDgIdsToProps(props, histRows) {
  /** @type {Map<string, Set<number>>} */
  const fields = new Map();
  for (const row of histRows) {
    const yr = Math.round(num(row.year, NaN));
    const rnd = Math.round(num(row.round_num, NaN));
    const ev = String(row.event_name || "").trim();
    const dg = Math.round(num(row.dg_id, NaN));
    if (!ev || !Number.isFinite(yr) || !Number.isFinite(rnd) || !Number.isFinite(dg)) continue;
    const k = `${yr}|${foldComparableTitle(ev)}|${rnd}`;
    if (!fields.has(k)) fields.set(k, new Set());
    fields.get(k).add(dg);
  }
  for (const p of props.values()) {
    const k = `${p.year}|${foldComparableTitle(p.event)}|${p.round}`;
    let ids = fields.get(k);
    if (!ids?.size) {
      for (const [fk, set] of fields.entries()) {
        const [y, ev, rnd] = fk.split("|");
        if (Number(y) === p.year && Number(rnd) === p.round && eventsLikelySame(p.event, ev.replace(/-/g, " "))) {
          ids = set;
          break;
        }
      }
    }
    p._field_dg_ids = ids ? [...ids] : p.dg_id ? [Math.round(p.dg_id)] : [];
  }
}
