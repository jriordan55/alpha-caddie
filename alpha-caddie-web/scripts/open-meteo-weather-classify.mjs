/** Open-Meteo WMO weathercode + precip → Alpha Caddie condition buckets (browser + bake share logic). */

export function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : NaN;
}

const RAINY_CODES = [51, 53, 55, 56, 57, 61, 63, 65, 66, 67, 80, 81, 82];
const STORM_CODES = [95, 96, 99];

/** Higher = more severe; used to pick representative code in a tee-time window. */
export function weatherCodeSeverity(c) {
  const code = Math.round(num(c, NaN));
  if (!Number.isFinite(code)) return 0;
  if (STORM_CODES.includes(code)) return 50;
  if ([61, 63, 65, 66, 67, 80, 81, 82].includes(code)) return 40;
  if ([51, 53, 55, 56, 57].includes(code)) return 35;
  if ([45, 48].includes(code)) return 25;
  if (code === 3) return 20;
  if (code === 2) return 15;
  if (code === 1) return 5;
  return 0;
}

export function openMeteoConditionFromHourSlice(codeWorst, maxPrecipProb, maxPrecipMm = 0, maxWindMph = 0) {
  const p = num(maxPrecipProb, 0);
  const mm = num(maxPrecipMm, 0);
  const c = Math.round(num(codeWorst, NaN));
  const w = num(maxWindMph, 0);

  if (STORM_CODES.includes(c)) return "storm";
  if (mm >= 0.05 || RAINY_CODES.includes(c)) return "rain";
  if (p >= 40 || (p >= 28 && (RAINY_CODES.includes(c) || c === 3))) return "rain";
  if (p >= 22 && c >= 51) return "rain";
  if (w >= 18 && p < 35 && !RAINY_CODES.includes(c) && !STORM_CODES.includes(c)) return "windy";
  if ([45, 48, 2, 3].includes(c)) return "cloudy";
  if (Number.isFinite(c) && c <= 1 && p < 15 && mm < 0.02) return "clear";
  if (c === 0) return "clear";
  return "cloudy";
}

/** Aggregate hourly arrays for a tee-time window. */
export function summarizeHourlyWeatherSlice(hourly, startIdx, spanHours) {
  const times = hourly?.time;
  const T = hourly?.temperature_2m;
  const W = hourly?.windspeed_10m;
  const H = hourly?.relativehumidity_2m;
  const P = hourly?.precipitation_probability;
  const R = hourly?.precipitation;
  const C = hourly?.weathercode;
  if (!Array.isArray(times) || startIdx < 0 || startIdx >= times.length) return null;

  const end = Math.min(times.length, startIdx + spanHours);
  let nt = 0;
  let sT = 0;
  let sW = 0;
  let sH = 0;
  let worstCode = NaN;
  let worstRank = -1;
  let maxPP = 0;
  let maxMm = 0;
  let peakWind = 0;

  for (let i = startIdx; i < end; i++) {
    const ti = num(T?.[i], NaN);
    if (!Number.isFinite(ti)) continue;
    sT += ti;
    const wi = num(W?.[i], 0);
    sW += wi;
    if (wi > peakWind) peakWind = wi;
    sH += num(H?.[i], 0);
    const cc = num(C?.[i], NaN);
    if (Number.isFinite(cc)) {
      const rank = weatherCodeSeverity(cc);
      if (rank > worstRank) {
        worstRank = rank;
        worstCode = cc;
      }
    }
    const pp = num(P?.[i], 0);
    if (pp > maxPP) maxPP = pp;
    const mm = num(R?.[i], 0);
    if (mm > maxMm) maxMm = mm;
    nt++;
  }

  if (!nt) return null;
  const cond = openMeteoConditionFromHourSlice(worstCode, maxPP, maxMm, peakWind);
  return {
    tempF: sT / nt,
    /** Mean sustained 10 m wind during the tee-time window (mph) — not gusts or window peak. */
    windMph: sW / nt,
    humidityPct: sH / nt,
    condition: cond,
  };
}
