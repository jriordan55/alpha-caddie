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

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

/** Stroke difficulty bump: positive = harder round, negative = soft/easy (rain, calm wind). */
export const WEATHER_CONDITION_MEAN_DELTA = Object.freeze({
  default: 0,
  clear: 0,
  cloudy: 0.04,
  windy: 0.22,
  rain: -0.38,
  storm: 0.12,
});

export const WEATHER_CONDITION_SIGMA_DELTA = Object.freeze({
  default: 0,
  clear: 0,
  cloudy: 0.02,
  windy: 0.05,
  rain: 0.06,
  storm: 0.14,
});

export function effectiveWeatherForRow(row) {
  const auto = row?.dg_auto_weather;
  if (
    auto &&
    typeof auto === "object" &&
    Number.isFinite(num(auto.tempF)) &&
    Number.isFinite(num(auto.windMph)) &&
    Number.isFinite(num(auto.humidityPct))
  ) {
    return {
      tempF: auto.tempF,
      windMph: auto.windMph,
      humidityPct: auto.humidityPct,
      condition: String(auto.condition || "default").toLowerCase(),
    };
  }
  return {
    tempF: num(row?.weather_temp_f, 72),
    windMph: num(row?.weather_wind_mph, 8),
    humidityPct: num(row?.weather_humidity, 50),
    condition: String(row?.weather_condition || "default").toLowerCase(),
  };
}

export function weatherDifficultyDeltaFromSnapshot(w) {
  if (!w || typeof w !== "object") return NaN;
  const tempF = num(w.tempF, 72);
  const wind = num(w.windMph, 8);
  const hum = num(w.humidityPct, 55);
  const cond = String(w.condition || "default").toLowerCase();
  let tempAdj = tempF >= 72 ? 0.03 * (tempF - 72) : 0.02 * (tempF - 72);
  if (wind < 9 && cond !== "windy" && cond !== "storm") tempAdj *= 0.35;
  const windAdj = 0.045 * (wind - 8);
  const humAdj = 0.006 * (hum - 55);
  let d = tempAdj + windAdj + humAdj;
  if (cond !== "default") d += WEATHER_CONDITION_MEAN_DELTA[cond] ?? 0;
  const softTurf =
    cond === "rain" || (wind < 9 && cond !== "windy" && cond !== "storm");
  if (softTurf) d -= 0.12;
  // Rain + wind: archive rounds score softer (less roll, receptive greens) — dampen wind hardness.
  if (cond === "rain" && wind >= 18) {
    d -= 0.08 * Math.min(1, (wind - 18) / 22);
  }
  return clamp(d, -0.65, 0.85);
}

export function weatherSigmaMultiplierFromSnapshot(w) {
  if (!w || typeof w !== "object") return 1;
  const wind = num(w.windMph, 8);
  const hum = num(w.humidityPct, 55);
  const windVar = 0.01 * Math.max(0, wind - 8);
  const humVar = 0.0015 * Math.max(0, hum - 55);
  const cond = String(w.condition || "default").toLowerCase();
  if (cond === "default") return clamp(1 + windVar + humVar, 0.9, 1.5);
  const condVar = WEATHER_CONDITION_SIGMA_DELTA[cond] ?? 0;
  return clamp(1 + windVar + humVar + condVar, 0.9, 1.5);
}

export function statWeatherMuAdjustment(market, row) {
  const d = weatherDifficultyDeltaFromSnapshot(effectiveWeatherForRow(row));
  if (!Number.isFinite(d)) return 0;
  if (market === "Total score") return d;
  if (market === "Bogeys") return 0.45 * d;
  if (market === "Birdies") return -0.5 * d;
  if (market === "Putts") return 0.35 * d;
  if (market === "GIR") return -0.22 * d;
  if (market === "Fairways hit") return -0.14 * d;
  return 0;
}

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

  for (const [market, field] of [
    ["Birdies", "birdies"],
    ["Bogeys", "bogeys"],
    ["GIR", "gir"],
    ["Fairways hit", "fairways"],
    ["Putts", "putts"],
  ]) {
    const v = num(p[field], NaN);
    if (!Number.isFinite(v)) continue;
    p[field] = roundCountStat(market, v + statWeatherMuAdjustment(market, p));
  }

  renormalizePars(p);

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
  p.weather_counts_baked = true;
  const basis = meta?.projection_course_basis && typeof meta.projection_course_basis === "object" ? meta.projection_course_basis : {};
  reconcileProjectionRowCountsToScore(p, {
    coursePar18: par18,
    venueAvgBirdies: num(basis.venue_avg_birdies, 4.2),
    venueAvgBogeys: num(basis.venue_avg_bogeys, 2.1),
    venueAvgGir: num(basis.venue_avg_gir, 12),
    venueAvgFairways: num(basis.venue_avg_fairways, 9),
    nFairwayHoles: Math.round(num(basis.fairway_holes_modeled, 14)) || 14,
    alignStrength: 0.52,
    spreadStrength: 0.58,
  });
  return true;
}

/**
 * Restore pre-weather baselines, apply per-tee forecast, bake counts into projections.json.
 */
export function applyWeatherBakedCountsToAllPlayers(proj, opts = {}) {
  const players = Array.isArray(proj?.players) ? proj.players : [];
  const meta = projectionExportMeta(proj);
  const forecastRound = Math.round(num(opts.forecastRound, NaN));
  let n = 0;
  for (const p of players) {
    if (!p._pre_weather_counts) p._pre_weather_counts = snapshotPlayerCounts(p);
    else restorePlayerCountsFromSnapshot(p, p._pre_weather_counts);

    const rnd = Math.round(num(p?.round, NaN));
    if (Number.isFinite(forecastRound) && forecastRound >= 1 && Number.isFinite(rnd) && rnd !== forecastRound) {
      delete p.dg_auto_weather;
      p.weather_temp_f = null;
      p.weather_wind_mph = null;
      p.weather_humidity = null;
      p.weather_condition = "";
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
  const dkField = draftKingsDgIdsFromProjections(proj);
  reconcileAllProjectionPlayerRows(proj, {
    dgFilter: dkField.size >= 8 ? dkField : null,
    minField: 8,
    skipFieldCalibrate: n <= 0,
  });
  return n;
}
