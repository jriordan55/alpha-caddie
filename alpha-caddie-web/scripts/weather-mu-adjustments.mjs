/**
 * Weather → O/U μ adjustments (browser-safe; no Node/fs deps).
 * Negative difficulty delta = softer / easier scoring (more birdies, lower totals).
 */

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
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
