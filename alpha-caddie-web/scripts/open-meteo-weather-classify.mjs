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

/** Documented wind metric for projections + historical archive backfill. */
export const OPEN_METEO_WIND_METRIC = "median_mph_between_mean_sustained_and_max_gust_in_tee_window";

/** Median of mean sustained wind and peak gust in a tee-time window (two-value median). */
export function windMphFromMeanSustainedAndMaxGust(meanSustainedMph, maxGustMph) {
  const s = num(meanSustainedMph, NaN);
  const g = num(maxGustMph, NaN);
  if (!Number.isFinite(s) && !Number.isFinite(g)) return NaN;
  if (!Number.isFinite(s)) return g;
  if (!Number.isFinite(g)) return s;
  return (s + g) / 2;
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

/**
 * Sum precip (mm) in the lookback hours ending at teeStartIdx (exclusive of the
 * tee window itself). Captures yesterday + overnight rain that softens turf.
 * Default 36h so afternoon tees still see prior-day storms.
 */
export function priorPrecipMmBeforeTee(hourly, teeStartIdx, lookbackHours = 36) {
  const R = hourly?.precipitation;
  if (!Array.isArray(R) || !Number.isFinite(teeStartIdx) || teeStartIdx <= 0) return 0;
  const start = Math.max(0, Math.floor(teeStartIdx) - Math.max(1, lookbackHours));
  const end = Math.floor(teeStartIdx);
  let mm = 0;
  for (let i = start; i < end; i++) {
    const v = num(R[i], 0);
    if (Number.isFinite(v) && v > 0) mm += v;
  }
  return Math.round(mm * 100) / 100;
}

/** Aggregate hourly arrays for a tee-time window. */
export function summarizeHourlyWeatherSlice(hourly, startIdx, spanHours, opts = {}) {
  const times = hourly?.time;
  const T = hourly?.temperature_2m;
  const W = hourly?.windspeed_10m;
  const G = hourly?.windgusts_10m;
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
  let peakGust = 0;

  for (let i = startIdx; i < end; i++) {
    const ti = num(T?.[i], NaN);
    if (!Number.isFinite(ti)) continue;
    sT += ti;
    const wi = num(W?.[i], NaN);
    if (Number.isFinite(wi)) {
      sW += wi;
      if (wi > peakWind) peakWind = wi;
    }
    const gi = num(G?.[i], NaN);
    if (Number.isFinite(gi) && gi > peakGust) peakGust = gi;
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
  const meanSustained = Number.isFinite(sW) && nt > 0 ? sW / nt : NaN;
  const maxGust = peakGust > 0 ? peakGust : peakWind;
  // μ wind = mean sustained 10m wind over the golfer's tee→play hours (not gust-blended).
  const windMph = meanSustained;
  const peakForCond = Math.max(peakWind, peakGust);
  const cond = openMeteoConditionFromHourSlice(worstCode, maxPP, maxMm, peakForCond);
  const lookback = Number.isFinite(num(opts.priorLookbackHours, NaN))
    ? Math.round(num(opts.priorLookbackHours, 36))
    : 36;
  const priorPrecipMm = priorPrecipMmBeforeTee(hourly, startIdx, lookback);
  const hourlyWinds = [];
  for (let i = startIdx; i < end; i++) {
    const wi = num(W?.[i], NaN);
    if (!Number.isFinite(wi)) continue;
    hourlyWinds.push({
      time: String(times[i] || "").slice(0, 16),
      windMph: Math.round(wi * 10) / 10,
    });
  }
  return {
    tempF: sT / nt,
    /** Mean sustained 10 m wind (mph) over tee-aligned play hours. */
    windMph,
    windMphPeak: peakWind > 0 ? peakWind : NaN,
    windMphGust: maxGust > 0 ? maxGust : NaN,
    hourlyWinds,
    humidityPct: sH / nt,
    condition: cond,
    priorPrecipMm,
    priorRainSoft: priorPrecipMm >= 0.4,
  };
}
