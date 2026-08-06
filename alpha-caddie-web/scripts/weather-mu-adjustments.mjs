/**
 * Weather → O/U μ adjustments (browser-safe; no Node/fs deps).
 * Negative difficulty delta = softer / easier scoring (more birdies, lower totals).
 *
 * Calibrated on PGA rounds 2015+ joined to Open-Meteo archive weather:
 *   - Wind ≥5 mph: ~+0.10 strokes / mph excess over 5 (user + empirical slope ≈0.0997)
 *   - Overnight / pre-tee rain softens turf (easier scoring) via priorPrecipMm
 *   - Afternoon wave ~+0.13 strokes vs morning (paired within-round AM/PM)
 */

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

/**
 * Wind difficulty: each mph at/above 5 mph adds 0.1 strokes.
 * Below 5 mph → 0. At 5 → 0; at 15 → +1.0; at 25 → +2.0.
 */
export const WIND_EFFECT_FLOOR_MPH = 5;
export const WIND_STROKES_PER_MPH = 0.1;

/** Paired AM/PM venue history (PGA 2015+, same event-round): afternoon − morning STP. */
export const HIST_TEE_WAVE_AFTERNOON_STP = 0.13;
export const HIST_TEE_WAVE_AFTERNOON_BIRDIES = -0.07;
export const HIST_TEE_WAVE_AFTERNOON_BOGEYS = 0.08;

/**
 * During-play condition bumps (separate from overnight soft).
 * Empirically rain|low-wind scores harder than clear|low-wind (~+0.47 STP) —
 * keep a milder in-play rain penalty; overnight soft is applied via priorPrecipMm.
 */
export const WEATHER_CONDITION_MEAN_DELTA = Object.freeze({
  default: 0,
  clear: 0,
  cloudy: 0.04,
  windy: 0.05,
  rain: 0.18,
  storm: 0.28,
});

export const WEATHER_CONDITION_SIGMA_DELTA = Object.freeze({
  default: 0,
  clear: 0,
  cloudy: 0.02,
  windy: 0.05,
  rain: 0.06,
  storm: 0.14,
});

/** Overnight / pre-tee precip (mm) → stroke softener (negative = easier). */
export function priorRainSoftDeltaFromMm(priorPrecipMm) {
  const mm = num(priorPrecipMm, 0);
  if (!(mm >= 0.4)) return 0;
  if (mm < 1.5) return -0.12;
  if (mm < 4) return -0.25;
  if (mm < 8) return -0.35;
  if (mm < 15) return -0.45;
  return -0.55;
}

export function windDifficultyDelta(windMph) {
  const wind = num(windMph, 0);
  if (!Number.isFinite(wind)) return 0;
  return Math.max(0, wind - WIND_EFFECT_FLOOR_MPH) * WIND_STROKES_PER_MPH;
}

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
      priorPrecipMm: num(auto.priorPrecipMm, NaN),
      priorRainSoft: Boolean(auto.priorRainSoft),
    };
  }
  return {
    tempF: num(row?.weather_temp_f, 72),
    windMph: num(row?.weather_wind_mph, 8),
    humidityPct: num(row?.weather_humidity, 50),
    condition: String(row?.weather_condition || "default").toLowerCase(),
    priorPrecipMm: num(row?.weather_prior_precip_mm ?? row?.priorPrecipMm, NaN),
    priorRainSoft: Boolean(row?.weather_prior_rain_soft ?? row?.priorRainSoft),
  };
}

export function weatherDifficultyDeltaFromSnapshot(w) {
  if (!w || typeof w !== "object") return NaN;
  const tempF = num(w.tempF, 72);
  const wind = num(w.windMph, 8);
  const hum = num(w.humidityPct, 55);
  const cond = String(w.condition || "default").toLowerCase();

  let tempAdj = tempF >= 72 ? 0.03 * (tempF - 72) : 0.02 * (tempF - 72);
  // Calm air: temperature bite is muted (less evaporative stress / less firm).
  if (wind < WIND_EFFECT_FLOOR_MPH && cond !== "windy" && cond !== "storm") tempAdj *= 0.35;

  const windAdj = windDifficultyDelta(wind);
  const humAdj = 0.006 * (hum - 55);

  let d = tempAdj + windAdj + humAdj;
  if (cond !== "default") d += WEATHER_CONDITION_MEAN_DELTA[cond] ?? 0;

  // Overnight / morning-before rain softens turf → easier scoring.
  let priorMm = num(w.priorPrecipMm, NaN);
  if (!Number.isFinite(priorMm) && (w.priorRainSoft === true || w.priorRainSoft === 1)) {
    priorMm = 3; // boolean flag without mm → moderate soft
  }
  d += priorRainSoftDeltaFromMm(priorMm);

  // Mild calm-turf soft when dry overnight and not stormy (firmness ↔ hold tradeoff).
  if (
    !(priorMm >= 0.4) &&
    wind < WIND_EFFECT_FLOOR_MPH &&
    cond !== "windy" &&
    cond !== "storm" &&
    cond !== "rain"
  ) {
    d -= 0.08;
  }

  // Allow full wind ladder (25 mph → +2.0) plus overnight soft.
  return clamp(d, -1.2, 2.6);
}

export function weatherSigmaMultiplierFromSnapshot(w) {
  if (!w || typeof w !== "object") return 1;
  const wind = num(w.windMph, 8);
  const hum = num(w.humidityPct, 55);
  const windVar = 0.012 * Math.max(0, wind - WIND_EFFECT_FLOOR_MPH);
  const humVar = 0.0015 * Math.max(0, hum - 55);
  const cond = String(w.condition || "default").toLowerCase();
  const priorMm = num(w.priorPrecipMm, 0);
  const softVar = priorMm >= 2 ? 0.03 : 0;
  if (cond === "default") return clamp(1 + windVar + humVar + softVar, 0.9, 1.55);
  const condVar = WEATHER_CONDITION_SIGMA_DELTA[cond] ?? 0;
  return clamp(1 + windVar + humVar + condVar + softVar, 0.9, 1.55);
}

/**
 * Market μ shifts from difficulty d (harder round → higher totals / bogeys, fewer birdies / GIR / FW).
 */
export function statWeatherMuAdjustment(market, row) {
  const d = weatherDifficultyDeltaFromSnapshot(effectiveWeatherForRow(row));
  if (!Number.isFinite(d)) return 0;
  if (market === "Total score") return d;
  if (market === "Bogeys") return 0.45 * d;
  if (market === "Birdies") return -0.5 * d;
  if (market === "Pars") return 0.2 * d;
  if (market === "Putts") return 0.35 * d;
  if (market === "GIR") return -0.22 * d;
  if (market === "Fairways hit") return -0.14 * d;
  return 0;
}
