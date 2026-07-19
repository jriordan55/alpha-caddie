/**
 * Unified projection factors: course-table fit, similar-course history, tee-wave scoring,
 * bounce-back / mean-reversion, Sunday pressure, per-round weather, player residual calibration,
 * and correlated market reconciliation (score ↔ bird/bog/GIR/FW/putts).
 *
 * Called from fetch-datagolf.mjs and refresh:live (after tee times + weather bake).
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  lookupAdjScoreToParFromCourseTable,
  reconcileAllProjectionPlayerRows,
  reconcileProjectionRowCountsToScore,
  flatVenuePlayerScoreAnchorEnabled,
  residualParsFromHoleCounts,
} from "./course-round-adjustments.mjs";
import { projectionExportMeta } from "./projection-export-meta.mjs";
import { ensureProjectionCoursePar } from "./projection-course-par.mjs";
import { waveScoringBiasFromLiveHoleStats } from "./dg-live-hole-pars.mjs";
import {
  applyWeatherBakedCountsToAllPlayers,
  effectiveWeatherForRow,
  statWeatherMuAdjustment,
  weatherDifficultyDeltaFromSnapshot,
} from "./weather-projection-adjustments.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function clamp(x, lo, hi) {
  return Math.max(lo, Math.min(hi, x));
}

function envNum(name, fallback) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return fallback;
  const n = Number(raw);
  return Number.isFinite(n) ? n : fallback;
}

function envOn(name, defaultOn = true) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultOn;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

export const UNIFIED_FACTOR_WEIGHTS = Object.freeze({
  courseTableFit: envNum("GOLF_UNIFIED_COURSE_TABLE_FIT_W", 0.16),
  similarCourse: envNum("GOLF_UNIFIED_SIMILAR_COURSE_W", 0.1),
  teeWave: envNum("GOLF_UNIFIED_TEE_WAVE_W", 0.28),
  bounceBack: envNum("GOLF_UNIFIED_BOUNCE_BACK_K", 0.032),
  bounceBackCap: envNum("GOLF_UNIFIED_BOUNCE_BACK_CAP", 0.11),
  sundayPressure: envNum("GOLF_UNIFIED_SUNDAY_PRESSURE_W", 1),
  playerResidual: envNum("GOLF_UNIFIED_PLAYER_RESIDUAL_W", 0.55),
  weatherAllRounds: envOn("GOLF_UNIFIED_WEATHER_ALL_ROUNDS", true),
});

export function teeWaveFromRow(row) {
  return teeWaveFromTeetimeAndLabel(row?.dg_teetime_local ?? row?.teetime, row?.dg_tee_wave);
}

export function loadCourseTablePayload() {
  for (const rel of ["course-table.json", join("data", "course-table.json")]) {
    const p = join(WEB_ROOT, rel);
    if (!existsSync(p)) continue;
    try {
      return JSON.parse(readFileSync(p, "utf8"));
    } catch {
      /* try next */
    }
  }
  return null;
}

export function resolveCourseTableRow(ctPayload, courseLabel) {
  const ck = normCourseNameKey(courseLabel);
  if (!ck || !ctPayload?.rows?.length) return null;
  let partial = null;
  for (const row of ctPayload.rows) {
    const rk = normCourseNameKey(row.course ?? row.course_name ?? "");
    if (!rk) continue;
    if (rk === ck) return row;
    if (rk.includes(ck) || ck.includes(rk)) partial = partial || row;
  }
  return partial;
}

/** SG × course_table coeffs (round_projections.R / app.js courseFitPlayerCourseFitRaw). */
export function courseTableFitStrokeShift(playerRow, ctRow) {
  if (!ctRow || typeof ctRow !== "object") return 0;
  let fit =
    num(playerRow.sg_putt, 0) * num(ctRow.putt_sg, 0) +
    num(playerRow.sg_arg, 0) * num(ctRow.arg_sg, 0) +
    num(playerRow.sg_app, 0) * num(ctRow.app_sg, 0) +
    num(playerRow.sg_ott, 0) * num(ctRow.ott_sg, 0);
  const acc = num(playerRow.driving_accuracy, NaN);
  if (Number.isFinite(acc) && acc > 1.02) {
    const frac = acc / 100;
    if (Number.isFinite(num(ctRow.adj_driving_accuracy, NaN))) {
      fit += 0.5 * (frac - num(ctRow.adj_driving_accuracy, 0));
    }
  } else if (Number.isFinite(acc) && Number.isFinite(num(ctRow.adj_driving_accuracy, NaN))) {
    fit += 0.5 * (acc - num(ctRow.adj_driving_accuracy, 0));
  }
  const dist = num(playerRow.avg_driving_distance ?? playerRow.driving_distance, NaN);
  if (Number.isFinite(dist) && Number.isFinite(num(ctRow.adj_driving_distance, NaN))) {
    fit += 0.002 * (dist - num(ctRow.adj_driving_distance, 0));
  }
  return -fit * UNIFIED_FACTOR_WEIGHTS.courseTableFit;
}

function courseTableFeatureVec(row) {
  if (!row) return null;
  const keys = ["adj_driving_accuracy", "ott_sg", "app_sg", "arg_sg", "putt_sg", "adj_score_to_par", "yardage"];
  const out = [];
  for (const k of keys) {
    const v = num(row[k], NaN);
    if (!Number.isFinite(v)) return null;
    out.push(v);
  }
  return out;
}

export function similarCoursesFromTable(ctPayload, venueKey, topN = 6) {
  if (!ctPayload?.rows?.length || !venueKey) return [];
  const venueRow = resolveCourseTableRow(ctPayload, venueKey);
  const ref = courseTableFeatureVec(venueRow);
  if (!ref) return [];
  const out = [];
  for (const row of ctPayload.rows) {
    const nk = normCourseNameKey(row.course ?? row.course_name ?? "");
    if (!nk || nk === normCourseNameKey(venueKey)) continue;
    const vec = courseTableFeatureVec(row);
    if (!vec) continue;
    const d = Math.hypot(...vec.map((x, j) => x - ref[j]));
    out.push({ key: nk, label: row.course ?? row.course_name, dist: d, sim: 1 / (1 + d) });
  }
  out.sort((a, b) => b.sim - a.sim);
  return out.slice(0, topN);
}

/** Stream CSV once: morning vs afternoon mean score-to-par + bird/bog at venue. */
export async function loadTeeWaveScoringBias(csvPath, courseKey) {
  const ck = normCourseNameKey(courseKey);
  const bias = {
    morning: { n: 0, stpSum: 0, birdSum: 0, bogSum: 0 },
    afternoon: { n: 0, stpSum: 0, birdSum: 0, bogSum: 0 },
  };
  if (!ck || !csvPath || !existsSync(csvPath)) {
    return {
      deltaAfternoonMinusMorning: 0,
      deltaBirdiesAfternoonMinusMorning: 0,
      deltaBogeysAfternoonMinusMorning: 0,
      n: 0,
    };
  }

  await new Promise((resolve, reject) => {
    const parser = createReadStream(csvPath).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    parser.on("data", (row) => {
      const ckRow = normCourseNameKey(row.course_name || row.Course_Name || "");
      if (!ckRow || ckRow !== ck) return;
      const tee = String(row.teetime ?? row.tee_time ?? "").trim();
      if (!tee) return;
      const cp = num(row.course_par, NaN);
      const rs = num(row.round_score, NaN);
      if (!Number.isFinite(cp) || !Number.isFinite(rs)) return;
      const stp = rs - cp;
      const bird = num(row.birdies, NaN);
      const bog = num(row.bogeys ?? row.bogies, NaN);
      const wave = teeWaveFromTeetimeAndLabel(tee, "");
      if (!wave) return;
      bias[wave].n++;
      bias[wave].stpSum += stp;
      if (Number.isFinite(bird)) bias[wave].birdSum += bird;
      if (Number.isFinite(bog)) bias[wave].bogSum += bog;
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });

  const mN = bias.morning.n;
  const aN = bias.afternoon.n;
  const mStp = mN > 40 ? bias.morning.stpSum / mN : NaN;
  const aStp = aN > 40 ? bias.afternoon.stpSum / aN : NaN;
  const mBird = mN > 40 && bias.morning.birdSum > 0 ? bias.morning.birdSum / mN : NaN;
  const aBird = aN > 40 && bias.afternoon.birdSum > 0 ? bias.afternoon.birdSum / aN : NaN;
  const mBog = mN > 40 && bias.morning.bogSum > 0 ? bias.morning.bogSum / mN : NaN;
  const aBog = aN > 40 && bias.afternoon.bogSum > 0 ? bias.afternoon.bogSum / aN : NaN;
  if (!Number.isFinite(mStp) || !Number.isFinite(aStp)) {
    return {
      deltaAfternoonMinusMorning: 0,
      deltaBirdiesAfternoonMinusMorning: 0,
      deltaBogeysAfternoonMinusMorning: 0,
      n: mN + aN,
    };
  }
  return {
    deltaAfternoonMinusMorning: aStp - mStp,
    deltaBirdiesAfternoonMinusMorning:
      Number.isFinite(mBird) && Number.isFinite(aBird) ? aBird - mBird : 0,
    deltaBogeysAfternoonMinusMorning:
      Number.isFinite(mBog) && Number.isFinite(aBog) ? aBog - mBog : 0,
    n: mN + aN,
    morning_n: mN,
    afternoon_n: aN,
  };
}

/** Similar-venue mean stp blend when primary venue sample is thin. */
export async function loadSimilarCourseStpBlend(csvPath, similarCourses) {
  if (!similarCourses?.length || !csvPath || !existsSync(csvPath)) return NaN;
  const keys = new Set(similarCourses.map((s) => s.key));
  const sums = new Map();
  await new Promise((resolve, reject) => {
    const parser = createReadStream(csvPath).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    parser.on("data", (row) => {
      const ck = normCourseNameKey(row.course_name || row.Course_Name || "");
      if (!keys.has(ck)) return;
      const cp = num(row.course_par, NaN);
      const rs = num(row.round_score, NaN);
      if (!Number.isFinite(cp) || !Number.isFinite(rs)) return;
      const b = sums.get(ck) || { n: 0, sum: 0 };
      b.n++;
      b.sum += rs - cp;
      sums.set(ck, b);
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });
  let wSum = 0;
  let stpSum = 0;
  for (const sim of similarCourses) {
    const b = sums.get(sim.key);
    if (!b || b.n < 30) continue;
    const w = sim.sim * Math.min(1, b.n / 120);
    stpSum += w * (b.sum / b.n);
    wSum += w;
  }
  return wSum > 0 ? stpSum / wSum : NaN;
}

function buildLiveLeaderboardMap(liveBundle) {
  const map = new Map();
  const data = Array.isArray(liveBundle?.data) ? liveBundle.data : [];
  let leaderScore = Infinity;
  for (const r of data) {
    const dg = Math.round(num(r.dg_id ?? r.dgId, NaN));
    const sc = num(r.current_score, NaN);
    if (Number.isFinite(dg) && Number.isFinite(sc)) {
      map.set(dg, { score: sc, pos: String(r.current_pos ?? "").trim() });
      if (sc < leaderScore) leaderScore = sc;
    }
  }
  if (!Number.isFinite(leaderScore)) leaderScore = NaN;
  return { byDg: map, leaderScore };
}

/** Partial mean-reversion after outlier prior round (complements within-event momentum carry). */
export function bounceBackStrokeShift(priorRoundStp, expectedStp, priorPriorStp, expectedPrior) {
  const k = UNIFIED_FACTOR_WEIGHTS.bounceBack;
  const cap = UNIFIED_FACTOR_WEIGHTS.bounceBackCap;
  if (!Number.isFinite(priorRoundStp) || !Number.isFinite(expectedStp) || k <= 0) return 0;
  const surprise = priorRoundStp - expectedStp;
  if (!Number.isFinite(surprise) || Math.abs(surprise) < 0.35) return 0;
  let z = surprise;
  if (Number.isFinite(priorPriorStp) && Number.isFinite(expectedPrior)) {
    const prevSur = priorPriorStp - expectedPrior;
    if (Math.abs(prevSur) > 0.01 && Math.sign(surprise) === Math.sign(prevSur)) {
      z *= 0.65;
    }
  }
  const shift = -k * z;
  return clamp(shift, -cap, cap);
}

/** R4 Sunday pressure from live leaderboard (strokes vs leader). */
export function sundayPressureStrokeShift(round, currentScore, leaderScore) {
  if (round !== 4 || !UNIFIED_FACTOR_WEIGHTS.sundayPressure) return 0;
  if (!Number.isFinite(currentScore) || !Number.isFinite(leaderScore)) return 0;
  const back = currentScore - leaderScore;
  if (back <= 0.5) return 0.09 * UNIFIED_FACTOR_WEIGHTS.sundayPressure;
  if (back <= 2) return 0.05 * UNIFIED_FACTOR_WEIGHTS.sundayPressure;
  if (back <= 4) return 0.02 * UNIFIED_FACTOR_WEIGHTS.sundayPressure;
  if (back >= 6 && back <= 12) return -0.07 * UNIFIED_FACTOR_WEIGHTS.sundayPressure;
  return 0;
}

export function teeWaveStrokeShift(wave, waveBias, morningSnap, afternoonSnap) {
  const w = UNIFIED_FACTOR_WEIGHTS.teeWave;
  if (w <= 0) return 0;
  let shift = 0;
  const histDelta = num(waveBias?.deltaAfternoonMinusMorning, 0);
  const liveStrength = waveBias?.source === "live_hole_stats" ? 0.85 : 0.5;
  if (wave === "afternoon") shift += histDelta * liveStrength * w;
  else if (wave === "morning") shift -= histDelta * liveStrength * w;
  const wxScale = waveBias?.source === "live_hole_stats" ? 0.15 : 0.35;
  if (morningSnap && afternoonSnap && wave) {
    const dM = weatherDifficultyDeltaFromSnapshot(morningSnap);
    const dA = weatherDifficultyDeltaFromSnapshot(afternoonSnap);
    if (Number.isFinite(dM) && Number.isFinite(dA)) {
      const waveDiff = dA - dM;
      shift += (wave === "afternoon" ? waveDiff : -waveDiff) * wxScale * w;
    }
  }
  return shift;
}

/** Venue-history + forecast AM/PM bird/bog differential (primary round separator under flat venue). */
export function teeWaveCountingShifts(wave, waveBias, morningSnap, afternoonSnap) {
  const w = UNIFIED_FACTOR_WEIGHTS.teeWave;
  if (w <= 0 || !wave) return { birdies: 0, bogeys: 0 };
  // Event-week DG live_hole_stats is the ground truth for this course/setup — apply most of the observed split.
  const liveStrength = waveBias?.source === "live_hole_stats" ? 0.85 : 0.5;
  let birdShift = 0;
  let bogShift = 0;
  const histBirdDelta = num(waveBias?.deltaBirdiesAfternoonMinusMorning, 0);
  const histBogDelta = num(waveBias?.deltaBogeysAfternoonMinusMorning, 0);
  if (wave === "afternoon") {
    birdShift += histBirdDelta * liveStrength * w;
    bogShift += histBogDelta * liveStrength * w;
  } else if (wave === "morning") {
    birdShift -= histBirdDelta * liveStrength * w;
    bogShift -= histBogDelta * liveStrength * w;
  }
  // Forecast weather AM/PM differential only when we lack live hole-stats wave (or as a small add-on).
  const wxScale = waveBias?.source === "live_hole_stats" ? 0.15 : 0.35;
  if (morningSnap && afternoonSnap) {
    const dM = weatherDifficultyDeltaFromSnapshot(morningSnap);
    const dA = weatherDifficultyDeltaFromSnapshot(afternoonSnap);
    if (Number.isFinite(dM) && Number.isFinite(dA)) {
      const waveDiff = dA - dM;
      const wxBird = -0.5 * waveDiff;
      const wxBog = 0.45 * waveDiff;
      birdShift += (wave === "afternoon" ? wxBird : -wxBird) * wxScale * w;
      bogShift += (wave === "afternoon" ? wxBog : -wxBog) * wxScale * w;
    }
  }
  return { birdies: birdShift, bogeys: bogShift };
}

/** Per-player residual bias from round_projection_vs_actual.csv (shrink by sample). */
export async function loadPlayerResidualCalibration(csvPath) {
  const out = new Map();
  if (!csvPath || !existsSync(csvPath)) return out;
  const acc = new Map();
  await new Promise((resolve, reject) => {
    const parser = createReadStream(csvPath).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    parser.on("data", (row) => {
      const dg = Math.round(num(row.dg_id, NaN));
      const market = String(row.market || row.stat || "").trim();
      const model = num(row.model_line ?? row.model_projection, NaN);
      const actual = num(row.actual, NaN);
      if (!Number.isFinite(dg) || !market || !Number.isFinite(model) || !Number.isFinite(actual)) return;
      const key = `${dg}|${market}`;
      const b = acc.get(key) || { n: 0, sum: 0 };
      b.n++;
      b.sum += actual - model;
      acc.set(key, b);
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });
  for (const [key, b] of acc) {
    if (b.n < 3) continue;
    const shrink = b.n / (b.n + 8);
    out.set(key, (b.sum / b.n) * shrink);
  }
  return out;
}

function priorRoundStpFromLive(liveBundle, dgId, roundNum, coursePar18) {
  const data = Array.isArray(liveBundle?.data) ? liveBundle.data : [];
  const row = data.find((r) => Math.round(num(r.dg_id, NaN)) === dgId);
  if (!row) return NaN;
  const cp =
    Math.round(
      num(
        coursePar18 ?? liveBundle?.course_par_18 ?? liveBundle?.meta?.course_par_18,
        NaN,
      ),
    ) || 72;
  const key = `R${roundNum}`;
  const gross = num(row[key], NaN);
  if (!Number.isFinite(gross)) return NaN;
  return gross - cp;
}

function applyStrokeShiftToRow(row, strokeShift, coursePar18, reasonParts) {
  if (!Number.isFinite(strokeShift) || Math.abs(strokeShift) < 1e-6) return;
  const par18 = Math.round(num(coursePar18, NaN)) || 72;
  const stp = num(row.score_to_par, NaN);
  const ts = num(row.total_score, NaN);
  if (Number.isFinite(stp)) {
    row.score_to_par = Math.round((stp + strokeShift) * 100) / 100;
    row.total_score = Math.round((par18 + row.score_to_par) * 100) / 100;
  } else if (Number.isFinite(ts)) {
    row.total_score = Math.round((ts + strokeShift) * 100) / 100;
    row.score_to_par = Math.round((row.total_score - par18) * 100) / 100;
  }
  if (Number.isFinite(num(row.mu_sg, NaN))) {
    row.mu_sg = Math.round((num(row.mu_sg, 0) - strokeShift) * 1000) / 1000;
  }
  if (reasonParts?.length) {
    row.unified_projection_shifts = [...(row.unified_projection_shifts || []), ...reasonParts];
  }
}

function applyStatShiftsFromWeather(row) {
  if (row?.weather_counts_baked) return;
  const markets = [
    ["birdies", "Birdies"],
    ["bogeys", "Bogeys"],
    ["gir", "GIR"],
    ["fairways", "Fairways hit"],
    ["putts", "Putts"],
    ["total_score", "Total score"],
  ];
  for (const [col, market] of markets) {
    const adj = statWeatherMuAdjustment(market, row);
    if (!Number.isFinite(adj) || Math.abs(adj) < 1e-6) continue;
    if (col === "total_score") continue;
    const v = num(row[col], NaN);
    if (!Number.isFinite(v)) continue;
    row[col] = Math.round((v + adj) * 100) / 100;
  }
}

function applyPlayerResidualToRow(row, residualMap, coursePar18) {
  const w = UNIFIED_FACTOR_WEIGHTS.playerResidual;
  if (w <= 0 || !residualMap?.size) return;
  const dg = Math.round(num(row.dg_id, NaN));
  if (!Number.isFinite(dg)) return;
  const specs = [
    ["Total score", "score_to_par", 1],
    ["Birdies", "birdies", 1],
    ["Bogeys", "bogeys", 1],
    ["GIR", "gir", 1],
    ["Fairways hit", "fairways", 1],
    ["Putts", "putts", 1],
  ];
  for (const [market, col, sign] of specs) {
    const bias = residualMap.get(`${dg}|${market}`);
    if (!Number.isFinite(bias)) continue;
    const delta = w * bias * sign;
    if (col === "score_to_par") {
      applyStrokeShiftToRow(row, delta, coursePar18, [`residual:${market}`]);
    } else {
      const v = num(row[col], NaN);
      if (Number.isFinite(v)) row[col] = Math.round((v + delta) * 100) / 100;
    }
  }
}

function assignPerRoundWeatherFromWaveSlots(row, meta) {
  if (!UNIFIED_FACTOR_WEIGHTS.weatherAllRounds) return;
  if (row?.weather_counts_baked || row?.dg_auto_weather) return;
  const slots = meta?.forecast_wave_slots;
  if (!slots || typeof slots !== "object") return;
  const wave = teeWaveFromRow(row);
  const snap = wave === "afternoon" ? slots.afternoon : wave === "morning" ? slots.morning : slots.morning;
  if (!snap || !Number.isFinite(num(snap.tempF, NaN))) return;
  row.dg_auto_weather = { ...snap };
  row.weather_temp_f = Math.round(num(snap.tempF, NaN) * 10) / 10;
  row.weather_wind_mph = Math.round(num(snap.windMph, NaN) * 10) / 10;
  row.weather_humidity = Math.round(num(snap.humidityPct, NaN));
  row.weather_condition = String(snap.condition || "default").toLowerCase();
}

function buildPlayerRoundIndex(players) {
  const byDgRound = new Map();
  for (const p of players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    const rnd = Math.round(num(p.round, NaN));
    if (Number.isFinite(dg) && Number.isFinite(rnd)) byDgRound.set(`${dg}|${rnd}`, p);
  }
  return byDgRound;
}

function restorePreWeatherBaselines(players) {
  let n = 0;
  for (const p of players || []) {
    const snap = p?._pre_weather_counts;
    if (snap && typeof snap === "object") {
      for (const k of Object.keys(snap)) {
        if (snap[k] !== undefined) p[k] = snap[k];
      }
      delete p._pre_weather_counts;
      n++;
    }
    p.weather_counts_baked = false;
    delete p.dg_auto_weather;
    p.weather_temp_f = null;
    p.weather_wind_mph = null;
    p.weather_humidity = null;
    p.weather_condition = "";
  }
  return n;
}

/**
 * Apply all unified projection factors to payload.players, then reconcile correlated markets.
 * @returns {{ adjusted: number, meta: object }}
 */
export async function applyUnifiedProjectionFactors(payload, opts = {}) {
  if (!envOn("GOLF_UNIFIED_PROJECTION_FACTORS", true)) {
    return { adjusted: 0, meta: { skipped: true } };
  }
  const players = Array.isArray(payload?.players) ? payload.players : [];
  if (!players.length) return { adjusted: 0, meta: { skipped: true, reason: "no_players" } };

  const meta = projectionExportMeta(payload);
  const parEnsure = ensureProjectionCoursePar(payload);
  if (!parEnsure.ok) {
    return { adjusted: 0, meta: { skipped: true, reason: parEnsure.reason || "missing_par" } };
  }
  const coursePar18 = parEnsure.coursePar18;

  const hadWeatherBaked = !!meta?.projection_counts_weather_baked || players.some((p) => p?.weather_counts_baked);
  const restored = restorePreWeatherBaselines(players);
  if (restored > 0) {
    meta.projection_counts_weather_baked = false;
  }
  const courseLabel = String(meta.course_used ?? payload.course_used ?? "").trim();
  const courseKey = normCourseNameKey(courseLabel);

  const csvPath =
    opts.csvPath ||
    [join(WEB_ROOT, "data", "historical_rounds_all.csv"), join(WEB_ROOT, "..", "data", "historical_rounds_all.csv")].find(
      (p) => existsSync(p),
    );
  const residualCsv =
    opts.residualCsv || join(WEB_ROOT, "data", "round_projection_vs_actual.csv");
  const liveBundle = opts.liveBundle ?? null;

  const ctPayload = loadCourseTablePayload();
  const ctRow = resolveCourseTableRow(ctPayload, courseLabel);
  const similar = similarCoursesFromTable(ctPayload, courseKey, 6);
  const [histWaveBias, similarStp, residualMap] = await Promise.all([
    loadTeeWaveScoringBias(csvPath, courseKey),
    loadSimilarCourseStpBlend(csvPath, similar),
    loadPlayerResidualCalibration(residualCsv),
  ]);

  // Prefer DataGolf live-hole-stats AM/PM (same feed as DG "SPLIT BY WAVE") over empty/thin hist CSV.
  const liveWaveBias = liveBundle?.live_hole_stats
    ? waveScoringBiasFromLiveHoleStats(
        liveBundle.live_hole_stats,
        courseLabel,
        liveBundle.field_updates,
        String(meta.event_name ?? payload.event_name ?? "").trim(),
      )
    : null;
  const waveBias =
    liveWaveBias && Number.isFinite(liveWaveBias.deltaAfternoonMinusMorning)
      ? liveWaveBias
      : histWaveBias;

  if (liveWaveBias?.total && meta.projection_course_basis && typeof meta.projection_course_basis === "object") {
    const tot = liveWaveBias.total;
    const fm = meta.projection_course_basis.field_counting_means_by_round || {
      birdies: {},
      bogeys: {},
      gir: {},
      fairways: {},
    };
    const rndKey = String(liveWaveBias.round || Math.round(num(payload.display_round, 1)) || 1);
    if (Number.isFinite(tot.birdies)) fm.birdies[rndKey] = tot.birdies;
    if (Number.isFinite(tot.bogeys)) fm.bogeys[rndKey] = tot.bogeys;
    meta.projection_course_basis.field_counting_means_by_round = fm;
    meta.projection_course_basis.live_hole_stats_wave = {
      round: liveWaveBias.round,
      morning: liveWaveBias.morning,
      afternoon: liveWaveBias.afternoon,
      total: liveWaveBias.total,
      delta_stp: liveWaveBias.deltaAfternoonMinusMorning,
      delta_birdies: liveWaveBias.deltaBirdiesAfternoonMinusMorning,
      delta_bogeys: liveWaveBias.deltaBogeysAfternoonMinusMorning,
    };
    // Flat export schema: keep root basis in sync for calibrateProjectionFieldMarkets.
    if (payload.projection_course_basis && payload.projection_course_basis !== meta.projection_course_basis) {
      payload.projection_course_basis.field_counting_means_by_round = fm;
      payload.projection_course_basis.live_hole_stats_wave =
        meta.projection_course_basis.live_hole_stats_wave;
    } else {
      payload.projection_course_basis = meta.projection_course_basis;
    }

    // Recenter bogey-or-worse toward DG hole-stats. Birdies stay on the
    // backtested BoB path (player BoB% + course spread/player@course + field cal);
    // do not pull them toward live-hole-stats here.
    const targetBog =
      num(tot.bogeys, NaN) + Math.max(0, num(tot.doubles ?? tot.doubles_or_worse, 0));
    if (Number.isFinite(targetBog)) {
      const displayRnd = Math.round(num(payload.display_round, liveWaveBias.round || 1)) || 1;
      const fieldRows = players.filter((p) => {
        const rnd = Math.round(num(p.round, NaN));
        return Number.isFinite(rnd) && rnd >= displayRnd && rnd <= 4;
      });
      const sampleRows =
        fieldRows.length >= 20
          ? fieldRows.filter((p) => Math.round(num(p.round, NaN)) === displayRnd)
          : fieldRows;
      const mean = (xs) => (xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : NaN);
      const bogMkt = (p) =>
        num(p.bogeys, NaN) + Math.max(0, num(p.doubles ?? p.doubles_or_worse, 0));
      const curBog = mean(sampleRows.map(bogMkt).filter(Number.isFinite));
      const dBog = Number.isFinite(curBog) ? targetBog - curBog : 0;
      if (Math.abs(dBog) > 0.05) {
        for (const p of players) {
          const rnd = Math.round(num(p.round, NaN));
          if (!Number.isFinite(rnd) || rnd < displayRnd) continue;
          if (Number.isFinite(num(p.bogeys, NaN))) {
            p.bogeys = Math.round(Math.max(0.15, num(p.bogeys, 0) + dBog) * 100) / 100;
          }
          const pars = residualParsFromHoleCounts(p);
          if (Number.isFinite(pars)) p.pars = Math.round(pars * 100) / 100;
        }
        console.log(
          `[unified-factors] recentered bogey-or-worse to DG live-hole-stats (bog ${curBog?.toFixed?.(2)}→${targetBog.toFixed?.(2)}; birdies unchanged / BoB model; rounds R${displayRnd}–R4)`,
        );
      }
    }
  }

  const morningSnap = meta?.forecast_wave_slots?.morning ?? meta?.forecast_weather_morning ?? null;
  const afternoonSnap = meta?.forecast_wave_slots?.afternoon ?? meta?.forecast_weather_afternoon ?? null;
  const liveLb = liveBundle ? buildLiveLeaderboardMap(liveBundle) : { byDg: new Map(), leaderScore: NaN };
  const byDgRound = buildPlayerRoundIndex(players);

  const venueThin = num(meta?.projection_course_basis?.venue_rounds, 0) < 80;
  const similarShift =
    venueThin && Number.isFinite(similarStp) && Number.isFinite(num(meta?.projection_course_basis?.venue_avg_stp, NaN))
      ? (similarStp - num(meta.projection_course_basis.venue_avg_stp, 0)) * UNIFIED_FACTOR_WEIGHTS.similarCourse
      : 0;

  let adjusted = 0;
  const factorCounts = {
    course_table_fit: 0,
    similar_course: 0,
    tee_wave: 0,
    bounce_back: 0,
    sunday_pressure: 0,
    weather_round: 0,
    player_residual: 0,
    live_hole_stats_wave: liveWaveBias ? 1 : 0,
  };

  for (const row of players) {
    const dg = Math.round(num(row.dg_id, NaN));
    const rnd = Math.round(num(row.round, NaN));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;

    let totalShift = 0;
    const reasons = [];

    const ctShift = courseTableFitStrokeShift(row, ctRow);
    if (Math.abs(ctShift) > 1e-5) {
      totalShift += ctShift;
      factorCounts.course_table_fit++;
      reasons.push(`course_fit:${ctShift.toFixed(3)}`);
    }

    if (Math.abs(similarShift) > 1e-5 && rnd === 1) {
      totalShift += similarShift;
      factorCounts.similar_course++;
      reasons.push(`similar_course:${similarShift.toFixed(3)}`);
    }

    assignPerRoundWeatherFromWaveSlots(row, meta);
    const wave = teeWaveFromRow(row);
    const twShift = teeWaveStrokeShift(wave, waveBias, morningSnap, afternoonSnap);
    if (Math.abs(twShift) > 1e-5) {
      totalShift += twShift;
      factorCounts.tee_wave++;
      reasons.push(`tee_wave:${twShift.toFixed(3)}`);
    }

    const countWave = teeWaveCountingShifts(wave, waveBias, morningSnap, afternoonSnap);
    if (Math.abs(countWave.birdies) > 1e-5 || Math.abs(countWave.bogeys) > 1e-5) {
      if (Number.isFinite(num(row.birdies, NaN))) {
        row.birdies = Math.round((num(row.birdies, 0) + countWave.birdies) * 100) / 100;
      }
      if (Number.isFinite(num(row.bogeys, NaN))) {
        row.bogeys = Math.round((num(row.bogeys, 0) + countWave.bogeys) * 100) / 100;
      }
      const pars = residualParsFromHoleCounts(row);
      if (Number.isFinite(pars)) row.pars = Math.round(pars * 100) / 100;
      factorCounts.tee_wave++;
      reasons.push(
        `tee_wave_counts:bird${countWave.birdies >= 0 ? "+" : ""}${countWave.birdies.toFixed(3)},bog${countWave.bogeys >= 0 ? "+" : ""}${countWave.bogeys.toFixed(3)}`,
      );
    }

    if (rnd >= 2 && !flatVenuePlayerScoreAnchorEnabled()) {
      const prior = byDgRound.get(`${dg}|${rnd - 1}`);
      const priorStpLive = priorRoundStpFromLive(liveBundle, dg, rnd - 1, coursePar18);
      const priorStp = Number.isFinite(priorStpLive) ? priorStpLive : num(prior?.score_to_par, NaN);
      const expected = Number.isFinite(num(prior?.mu_sg, NaN)) ? -num(prior.mu_sg, 0) : num(prior?.score_to_par, NaN);
      let priorPriorStp = NaN;
      let expectedPrior = NaN;
      if (rnd >= 3) {
        const pp = byDgRound.get(`${dg}|${rnd - 2}`);
        priorPriorStp = priorRoundStpFromLive(liveBundle, dg, rnd - 2, coursePar18);
        if (!Number.isFinite(priorPriorStp)) priorPriorStp = num(pp?.score_to_par, NaN);
        expectedPrior = Number.isFinite(num(pp?.mu_sg, NaN)) ? -num(pp.mu_sg, 0) : num(pp?.score_to_par, NaN);
      }
      const bb = bounceBackStrokeShift(priorStp, expected, priorPriorStp, expectedPrior);
      if (Math.abs(bb) > 1e-5) {
        totalShift += bb;
        factorCounts.bounce_back++;
        reasons.push(`bounce_back:${bb.toFixed(3)}`);
      }
    }

    if (rnd === 4) {
      const lb = liveLb.byDg.get(dg);
      const sp = sundayPressureStrokeShift(rnd, lb?.score, liveLb.leaderScore);
      if (Math.abs(sp) > 1e-5) {
        totalShift += sp;
        factorCounts.sunday_pressure++;
        reasons.push(`sunday_pressure:${sp.toFixed(3)}`);
      }
    }

    if (Math.abs(totalShift) > 1e-5) {
      applyStrokeShiftToRow(row, totalShift, coursePar18, reasons);
      adjusted++;
    }

    if (!row.weather_counts_baked) {
      const hadWeather = effectiveWeatherForRow(row);
      if (Number.isFinite(weatherDifficultyDeltaFromSnapshot(hadWeather)) && !hadWeatherBaked) {
        applyStatShiftsFromWeather(row);
        factorCounts.weather_round++;
      }
    }

    applyPlayerResidualToRow(row, residualMap, coursePar18);
    if (residualMap.has(`${dg}|Total score`)) factorCounts.player_residual++;
  }

  const willWeatherBake =
    hadWeatherBaked || meta?.forecast_wave_slots || meta?.forecast_weather_morning;

  if (willWeatherBake) {
    const forecastRound =
      Math.round(num(meta?.projection_counts_weather_baked_round ?? payload.display_round, NaN)) || 1;
    const nWx = applyWeatherBakedCountsToAllPlayers(payload, {
      forecastRound,
      skipReconcile: true,
      preserveBaselines: false,
    });
    if (nWx > 0) factorCounts.weather_round += nWx;
  }

  const rec = opts.skipReconcile
    ? null
    : reconcileAllProjectionPlayerRows(payload, {
        skipMarketBookCalibration: true,
        skipVenueScoreCalibrate: true,
        skipHistVenueScoreCalibrate: true,
        ...(opts.reconcileOpts || {}),
      });

  const summary = {
    applied_at: new Date().toISOString(),
    weights: { ...UNIFIED_FACTOR_WEIGHTS },
    factor_counts: factorCounts,
    tee_wave_bias: waveBias,
    tee_wave_source: waveBias?.source || "historical_csv",
    similar_courses: similar.map((s) => s.label).slice(0, 4),
    similar_stp_blend: Number.isFinite(similarStp) ? Math.round(similarStp * 1000) / 1000 : null,
    player_residuals_loaded: residualMap.size,
    rows_adjusted: adjusted,
    reconciled: rec?.reconciled ?? 0,
  };
  meta.projection_unified_factors = summary;
  meta.projection_round_adjustments = {
    ...(meta.projection_round_adjustments || {}),
    unified_factors_applied: true,
    projection_counts_coherent: true,
    skip_runtime_course_overlay: true,
    flat_venue_player_score: flatVenuePlayerScoreAnchorEnabled(),
  };

  console.log(
    `[unified-factors] adjusted ${adjusted}/${players.length} rows | course_fit=${factorCounts.course_table_fit} tee_wave=${factorCounts.tee_wave} (${waveBias?.source || "hist"} Δstp=${num(waveBias?.deltaAfternoonMinusMorning, 0).toFixed(2)} Δbog=${num(waveBias?.deltaBogeysAfternoonMinusMorning, 0).toFixed(2)}) bounce_back=${factorCounts.bounce_back} sunday=${factorCounts.sunday_pressure} weather=${factorCounts.weather_round} residual=${factorCounts.player_residual}`,
  );
  return { adjusted, meta: summary, reconciled: rec };
}
