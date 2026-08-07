/**
 * Weather → projection count adjustments (shared by bake:weather and round-projection-mu).
 * Negative difficulty delta = softer / easier scoring (more birdies, lower totals).
 */
import { projectionExportMeta } from "./projection-export-meta.mjs";
import {
  draftKingsDgIdsFromProjections,
  reconcileAllProjectionPlayerRows,
  reconcileProjectionRowCountsToScore,
} from "./course-round-adjustments.mjs";
import {
  clamp,
  effectiveWeatherForRow,
  num,
  statWeatherMuAdjustment,
  weatherDifficultyDeltaFromSnapshot,
  weatherSigmaMultiplierFromSnapshot,
  WEATHER_CONDITION_MEAN_DELTA,
  WEATHER_CONDITION_SIGMA_DELTA,
} from "./weather-mu-adjustments.mjs";

export {
  clamp,
  effectiveWeatherForRow,
  num,
  priorRainSoftDeltaFromMm,
  statWeatherMuAdjustment,
  weatherDifficultyDeltaFromSnapshot,
  weatherSigmaMultiplierFromSnapshot,
  windDifficultyDelta,
  HIST_TEE_WAVE_AFTERNOON_BOGEYS,
  HIST_TEE_WAVE_AFTERNOON_BIRDIES,
  HIST_TEE_WAVE_AFTERNOON_STP,
  WIND_EFFECT_FLOOR_MPH,
  WIND_STROKES_PER_MPH,
  WEATHER_CONDITION_MEAN_DELTA,
  WEATHER_CONDITION_SIGMA_DELTA,
} from "./weather-mu-adjustments.mjs";

function roundCountStat(market, v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x)) return NaN;
  if (market === "Total score") return Math.round(x * 100) / 100;
  if (market === "Putts") return Math.round(x * 10) / 10;
  return Math.round(x * 100) / 100;
}

function snapshotPlayerCounts(p) {
  return {
    total_score: p.total_score,
    score_to_par: p.score_to_par,
    birdies: p.birdies,
    pars: p.pars,
    bogeys: p.bogeys,
    eagles: p.eagles,
    doubles: p.doubles,
    gir: p.gir,
    fairways: p.fairways,
    putts: p.putts,
    mu_sg: p.mu_sg,
    implied_mu_sg: p.implied_mu_sg,
    round_sd: p.round_sd,
  };
}

/** After reconcile/calibrate, refresh weather baselines so re-bake does not restore stale counts. */
export function syncPreWeatherCountSnapshots(players) {
  for (const p of players || []) {
    if (!p || typeof p !== "object") continue;
    p._pre_weather_counts = snapshotPlayerCounts(p);
  }
}

function restorePlayerCountsFromSnapshot(p, snap) {
  if (!snap || typeof snap !== "object") return;
  for (const k of Object.keys(snap)) {
    if (snap[k] !== undefined) p[k] = snap[k];
  }
}

function renormalizePars(p) {
  const b = num(p.birdies, 0);
  const bg = num(p.bogeys, 0);
  const e = num(p.eagles, 0);
  const d = num(p.doubles, 0);
  if (![b, bg, e, d].every(Number.isFinite)) return;
  p.pars = Math.max(0, Math.round((18 - b - bg - e - d) * 100) / 100);
}

function playerSkillWeatherMuEdge(row) {
  const baseSg = num(row?.mu_sg ?? row?.implied_mu_sg ?? row?.sg_total, NaN);
  if (!Number.isFinite(baseSg)) return 0;
  const roundSd = num(row?.round_sd, NaN);
  const sgEdge = baseSg * 0.12;
  const consistencyEdge = Number.isFinite(roundSd) ? clamp((2.8 - roundSd) * 0.03, -0.06, 0.06) : 0;
  const d = weatherDifficultyDeltaFromSnapshot(effectiveWeatherForRow(row));
  if (!Number.isFinite(d)) return 0;
  return d * (sgEdge + consistencyEdge);
}

/**
 * Apply weather deltas to raw projection counts on one player row (mutates `p`).
 */
export function applyWeatherBakedCountsToPlayer(p, meta) {
  if (!p || typeof p !== "object") return false;
  const w = effectiveWeatherForRow(p);
  if (!Number.isFinite(w.tempF) || !Number.isFinite(w.windMph) || !Number.isFinite(w.humidityPct)) {
    return false;
  }

  const coursePar = num(meta?.course_par_18, NaN);
  const par18 = Number.isFinite(coursePar) ? coursePar : 71;

  const ts = num(p.total_score, NaN);
  if (Number.isFinite(ts)) {
    const adj = statWeatherMuAdjustment("Total score", p);
    p.total_score = roundCountStat("Total score", ts + adj);
    p.score_to_par = Math.round((p.total_score - par18) * 100) / 100;
  }

  const basis = meta?.projection_course_basis && typeof meta.projection_course_basis === "object" ? meta.projection_course_basis : {};
  const histCalib = meta?.historical_projection_calibration;
  reconcileProjectionRowCountsToScore(p, {
    coursePar18: par18,
    venueAvgBirdies: num(basis.venue_avg_birdies, 4.2),
    venueAvgBogeys: num(basis.venue_avg_bogeys, 2.1),
    venueAvgGir: num(basis.venue_avg_gir, 12),
    venueAvgFairways: num(basis.venue_avg_fairways, 9),
    venueAvgPars: num(basis.venue_avg_pars, 11.2),
    nFairwayHoles: Math.round(num(basis.fairway_holes_modeled, 14)) || 14,
    fwStpLine: histCalib?.fw_stp_line,
    scoreDeriveCounts: false,
    girBlend: 0,
    fairwaysBlend: 0,
  });

  for (const [market, field] of [
    ["GIR", "gir"],
    ["Fairways hit", "fairways"],
    ["Putts", "putts"],
  ]) {
    const v = num(p[field], NaN);
    if (!Number.isFinite(v)) continue;
    p[field] = roundCountStat(market, v + statWeatherMuAdjustment(market, p));
  }

  const baseMu = num(p.mu_sg ?? p.implied_mu_sg, NaN);
  if (Number.isFinite(baseMu)) {
    const muAdj = playerSkillWeatherMuEdge(p);
    p.mu_sg = Math.round((baseMu + muAdj) * 1000) / 1000;
    p.implied_mu_sg = p.mu_sg;
  }

  const baseSd = num(p.round_sd, NaN);
  if (Number.isFinite(baseSd) && baseSd > 0.05) {
    p.round_sd =
      Math.round(baseSd * weatherSigmaMultiplierFromSnapshot(w) * 1000) / 1000;
  }

  p._weather_bake_snapshot = { ...w };
  p.weather_difficulty_delta = Math.round(weatherDifficultyDeltaFromSnapshot(w) * 1000) / 1000;
  p.weather_counts_baked = true;
  return true;
}

/**
 * Restore pre-weather baselines, apply per-tee forecast, bake counts into projections.json.
 */
export function applyWeatherBakedCountsToAllPlayers(proj, opts = {}) {
  const players = Array.isArray(proj?.players) ? proj.players : [];
  const meta = projectionExportMeta(proj);
  const forecastRound = Math.round(num(opts.forecastRound, NaN));
  const preserveBaselines = opts.preserveBaselines === true;
  const coursePar = num(meta?.course_par_18, 71);
  const par18 = Number.isFinite(coursePar) ? coursePar : 71;

  // Reconcile used to overwrite `_pre_weather_counts` with post-weather values, which
  // makes re-bake restore a weathered baseline. Peel the last bake off first.
  for (const p of players) {
    if (!p?._weather_bake_snapshot || !p.weather_counts_baked) continue;
    const preTs = num(p._pre_weather_counts?.total_score, NaN);
    const curTs = num(p.total_score, NaN);
    if (!Number.isFinite(curTs)) continue;
    const preLooksSynced = !Number.isFinite(preTs) || Math.abs(preTs - curTs) < 0.02;
    if (!preLooksSynced) continue;
    const oldD = Number.isFinite(num(p.weather_difficulty_delta, NaN))
      ? num(p.weather_difficulty_delta, NaN)
      : weatherDifficultyDeltaFromSnapshot(p._weather_bake_snapshot);
    if (!Number.isFinite(oldD) || Math.abs(oldD) < 1e-6) continue;
    p.total_score = roundCountStat("Total score", curTs - oldD);
    p.score_to_par = Math.round((p.total_score - par18) * 100) / 100;
    for (const [market, field, k] of [
      ["Birdies", "birdies", -0.5],
      ["Bogeys", "bogeys", 0.45],
      ["Pars", "pars", 0.2],
      ["GIR", "gir", -0.22],
      ["Fairways hit", "fairways", -0.14],
      ["Putts", "putts", 0.35],
    ]) {
      const v = num(p[field], NaN);
      if (!Number.isFinite(v)) continue;
      p[field] = roundCountStat(market, v - k * oldD);
    }
    const baseMu = num(p.mu_sg ?? p.implied_mu_sg, NaN);
    if (Number.isFinite(baseMu)) {
      p.mu_sg = Math.round((baseMu - oldD * 0.12) * 1000) / 1000;
      p.implied_mu_sg = p.mu_sg;
    }
    p._pre_weather_counts = snapshotPlayerCounts(p);
    p.weather_counts_baked = false;
    delete p._weather_bake_snapshot;
    delete p.weather_difficulty_delta;
  }

  let n = 0;
  for (const p of players) {
    if (preserveBaselines && p._pre_weather_counts) {
      restorePlayerCountsFromSnapshot(p, p._pre_weather_counts);
    } else {
      p._pre_weather_counts = snapshotPlayerCounts(p);
    }

    const rnd = Math.round(num(p?.round, NaN));
    if (Number.isFinite(forecastRound) && forecastRound >= 1 && Number.isFinite(rnd) && rnd !== forecastRound) {
      delete p.dg_auto_weather;
      p.weather_temp_f = null;
      p.weather_wind_mph = null;
      p.weather_humidity = null;
      p.weather_condition = "";
      p.weather_prior_precip_mm = null;
      p.weather_prior_rain_soft = false;
      p.weather_counts_baked = false;
      continue;
    }
    if (applyWeatherBakedCountsToPlayer(p, meta)) n++;
  }
  meta.projection_counts_weather_baked = n > 0;
  if (Number.isFinite(forecastRound) && forecastRound >= 1) {
    meta.projection_counts_weather_baked_round = forecastRound;
  }
  if (n > 0) {
    meta.projection_counts_weather_baked_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  }
  if (!opts.skipReconcile) {
    reconcileAllProjectionPlayerRows(proj, {
      minField: opts.minField ?? 8,
      dkFieldOnly: opts.dkFieldOnly === true,
      dgFilter: opts.dkFieldOnly ? opts.dgFilter ?? draftKingsDgIdsFromProjections(proj) : null,
      skipFieldCalibrate: opts.skipFieldCalibrate === true || n <= 0,
      // Weather is the course-setup difficulty signal — do not yank totals back to dry venue anchors.
      skipVenueScoreCalibrate: true,
      skipHistVenueScoreCalibrate: true,
      displayRound: opts.displayRound ?? proj?.display_round,
      skipMarketBookCalibration: true,
      // Keep true pre-weather baselines for the next bake:weather pass.
      skipSyncPreWeather: true,
    });
  }
  return n;
}
