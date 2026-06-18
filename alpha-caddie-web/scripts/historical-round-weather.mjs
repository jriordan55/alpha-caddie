/**
 * Per-round historical weather from Open-Meteo archive at tee time.
 * Shared by backfill-historical-round-weather.mjs and build-player-history.mjs.
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { COURSE_COORDINATES_BY_NAME } from "./open-meteo-forecast.mjs";
import { parseDgTeetimeParts, hourlyIndexForDgTeetime } from "./open-meteo-forecast.mjs";
import { summarizeHourlyWeatherSlice } from "./open-meteo-weather-classify.mjs";
import { historyRoundChartUtcIsoDay } from "./history-round-dates.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const DEFAULT_COORDS_CACHE = path.join(WEB_ROOT, "data", "course_coordinates_cache.json");
const DEFAULT_ROUND_WEATHER_JSON = path.join(WEB_ROOT, "data", "historical_round_weather.json");

export { DEFAULT_ROUND_WEATHER_JSON };

/** Extra venues not yet in open-meteo-forecast.mjs */
const EXTRA_COURSE_COORDINATES = {
  "tpc toronto at osprey valley": { lat: 43.874, lon: -79.982, timezone: "America/Toronto" },
  "tpc toronto at osprey valley north course": { lat: 43.874, lon: -79.982, timezone: "America/Toronto" },
  "hamilton golf and country club": { lat: 43.267, lon: -79.934, timezone: "America/Toronto" },
  "glen abbey golf club": { lat: 43.452, lon: -79.691, timezone: "America/Toronto" },
};

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

export function roundWeatherKey(eventId, year, roundNum) {
  const eid = Math.round(Number(eventId));
  const yr = Math.round(Number(year));
  const rnd = Math.round(Number(roundNum));
  if (!Number.isFinite(eid) || !Number.isFinite(yr) || !Number.isFinite(rnd)) return "";
  return `${eid}|${yr}|${rnd}`;
}

export function mdyToIsoYmd(mdy) {
  const iso = historyRoundChartUtcIsoDay({ event_completed: String(mdy || "").trim() });
  return iso || "";
}

/** Historical CSV teetimes: "6:56am", "12:11pm", or ISO "2025-06-05 06:56". */
export function parseHistoricalRoundTeetime(teetimeStr, roundDateMdY) {
  const iso = parseDgTeetimeParts(teetimeStr);
  if (iso) return iso;

  const m = String(teetimeStr || "")
    .trim()
    .match(/^(\d{1,2}):(\d{2})\s*(am|pm)?$/i);
  if (!m) return null;

  let hh = parseInt(m[1], 10);
  const mm = parseInt(m[2], 10);
  const ap = String(m[3] || "").toLowerCase();
  if (ap === "pm" && hh < 12) hh += 12;
  if (ap === "am" && hh === 12) hh = 0;

  const ymd = mdyToIsoYmd(roundDateMdY);
  if (!ymd) return null;
  return { ymd, hh, mm };
}

export function teetimeStrForHourlyIndex(teetimeParts) {
  if (!teetimeParts?.ymd) return "";
  const hh = String(teetimeParts.hh ?? 0).padStart(2, "0");
  const mm = String(teetimeParts.mm ?? 0).padStart(2, "0");
  return `${teetimeParts.ymd} ${hh}:${mm}`;
}

function loadCoordsCache(cachePath = DEFAULT_COORDS_CACHE) {
  try {
    if (!fs.existsSync(cachePath)) return {};
    return JSON.parse(fs.readFileSync(cachePath, "utf8"));
  } catch {
    return {};
  }
}

function saveCoordsCache(cache, cachePath = DEFAULT_COORDS_CACHE) {
  try {
    fs.mkdirSync(path.dirname(cachePath), { recursive: true });
    fs.writeFileSync(cachePath, JSON.stringify(cache, null, 2));
  } catch {
    // non-fatal
  }
}

export function timezoneGuessFromCourse(courseName) {
  const k = normCourseNameKey(courseName);
  if (/\btoronto\b|\bhamilton\b|\bglen abbey\b|\bmississauga\b|\bmontreal\b|\bcalgary\b|\bvancouver\b/.test(k)) {
    return "America/Toronto";
  }
  if (/\bhawaii\b|\bkapalua\b|\bwaialae\b/.test(k)) return "Pacific/Honolulu";
  if (/\barizona\b|\bscottsdale\b|\bphoenix\b/.test(k)) return "America/Phoenix";
  if (/\bcalifornia\b|\bpebble\b|\btorrey\b|\briviera\b|\blos angeles\b/.test(k)) return "America/Los_Angeles";
  if (/\bcolorado\b|\bcastle pines\b/.test(k)) return "America/Denver";
  return "America/New_York";
}

export function courseCoordinatesForHistory(courseName, opts = {}) {
  const key = normCourseNameKey(courseName);
  if (!key) return null;

  const extra = EXTRA_COURSE_COORDINATES[key];
  if (extra) return { lat: extra.lat, lon: extra.lon, timezone: extra.timezone || timezoneGuessFromCourse(courseName) };

  const base = COURSE_COORDINATES_BY_NAME[key];
  if (base) {
    return { lat: base.lat, lon: base.lon, timezone: opts.timezone || timezoneGuessFromCourse(courseName) };
  }

  const cache = opts.coordsCache || loadCoordsCache(opts.coordsCachePath);
  const hit = cache[key];
  if (hit && Number.isFinite(hit.lat) && Number.isFinite(hit.lon)) {
    return {
      lat: hit.lat,
      lon: hit.lon,
      timezone: hit.timezone || opts.timezone || timezoneGuessFromCourse(courseName),
    };
  }
  return null;
}

export async function geocodeCourseName(courseName, opts = {}) {
  const key = normCourseNameKey(courseName);
  if (!key) return null;

  const existing = courseCoordinatesForHistory(courseName, opts);
  if (existing) return existing;

  const cache = opts.coordsCache || loadCoordsCache(opts.coordsCachePath);
  if (cache[key]) return courseCoordinatesForHistory(courseName, { ...opts, coordsCache: cache });

  const raw = String(courseName || "").trim();
  const queries = [];
  if (raw) queries.push(raw);
  const noParen = raw.replace(/\([^)]*\)/g, " ").replace(/\s+/g, " ").trim();
  if (noParen && noParen !== raw) queries.push(noParen);
  const beforeParen = raw.match(/^([^(]+)/)?.[1]?.trim();
  if (beforeParen && !queries.includes(beforeParen)) queries.push(beforeParen);
  const cityHint = String(opts.cityHint || "").trim();
  if (cityHint) {
    queries.push(`${beforeParen || noParen || raw}, ${cityHint}`.slice(0, 80));
    queries.push(`${cityHint} golf`.slice(0, 80));
  }

  for (const q of [...new Set(queries.filter(Boolean))]) {
    try {
      const u = new URL("https://geocoding-api.open-meteo.com/v1/search");
      u.searchParams.set("name", q.slice(0, 80));
      u.searchParams.set("count", "3");
      u.searchParams.set("language", "en");
      u.searchParams.set("format", "json");
      const res = await fetch(u.href);
      if (!res.ok) continue;
      const j = await res.json();
      const results = j?.results || [];
      const r =
        results.find((x) => /golf|country club|links/i.test(String(x?.name || ""))) ||
        results[0];
      if (!r || !Number.isFinite(r.latitude) || !Number.isFinite(r.longitude)) continue;
      cache[key] = {
        lat: r.latitude,
        lon: r.longitude,
        timezone: r.timezone || timezoneGuessFromCourse(courseName),
        label: r.name,
        geocode_query: q,
      };
      if (opts.persistCoordsCache !== false) saveCoordsCache(cache, opts.coordsCachePath);
      return {
        lat: r.latitude,
        lon: r.longitude,
        timezone: cache[key].timezone,
      };
    } catch {
      // try next query
    }
  }

  // City-level fallback — accurate enough for Open-Meteo hourly archive (~10–30 mi).
  const cityQueries = [];
  if (cityHint) cityQueries.push(cityHint);
  const eventCity = String(opts.eventName || "")
    .replace(/\([^)]*\)/g, " ")
    .trim();
  if (eventCity && !cityQueries.includes(eventCity)) cityQueries.push(eventCity);

  for (const q of cityQueries) {
    try {
      const u = new URL("https://geocoding-api.open-meteo.com/v1/search");
      u.searchParams.set("name", q.slice(0, 60));
      u.searchParams.set("count", "1");
      u.searchParams.set("language", "en");
      u.searchParams.set("format", "json");
      const res = await fetch(u.href);
      if (!res.ok) continue;
      const j = await res.json();
      const r = j?.results?.[0];
      if (!r || !Number.isFinite(r.latitude) || !Number.isFinite(r.longitude)) continue;
      cache[key] = {
        lat: r.latitude,
        lon: r.longitude,
        timezone: r.timezone || timezoneGuessFromCourse(courseName),
        label: r.name,
        geocode_query: q,
        geocode_level: "city",
      };
      if (opts.persistCoordsCache !== false) saveCoordsCache(cache, opts.coordsCachePath);
      return {
        lat: r.latitude,
        lon: r.longitude,
        timezone: cache[key].timezone,
      };
    } catch {
      // try next
    }
  }

  return null;
}

export function openMeteoArchiveUrl(lat, lon, startDate, endDate, timezone) {
  const u = new URL("https://archive-api.open-meteo.com/v1/archive");
  u.searchParams.set("latitude", String(lat));
  u.searchParams.set("longitude", String(lon));
  u.searchParams.set("start_date", startDate);
  u.searchParams.set("end_date", endDate);
  u.searchParams.set(
    "hourly",
    "temperature_2m,relativehumidity_2m,precipitation_probability,precipitation,windspeed_10m,windgusts_10m,weathercode",
  );
  u.searchParams.set("windspeed_unit", "mph");
  u.searchParams.set("temperature_unit", "fahrenheit");
  u.searchParams.set("timezone", timezone || "America/New_York");
  return u.href;
}

function medianFinite(vals) {
  const a = vals.filter((x) => Number.isFinite(x)).sort((x, y) => x - y);
  if (!a.length) return NaN;
  const mid = Math.floor(a.length / 2);
  return a.length % 2 ? a[mid] : (a[mid - 1] + a[mid]) / 2;
}

function medianWeatherFromSnapshots(samples) {
  if (!samples.length) return null;
  const mt = medianFinite(samples.map((s) => s.tempF));
  const mw = medianFinite(samples.map((s) => s.windMph));
  const mh = medianFinite(samples.map((s) => s.humidityPct));
  if (!Number.isFinite(mt)) return null;
  const rank = { storm: 5, rain: 4, windy: 3, cloudy: 2, clear: 1, default: 0 };
  let bestC = "default";
  let br = -1;
  for (const s of samples) {
    const c = String(s.condition || "default").toLowerCase();
    const r = rank[c] ?? 0;
    if (r > br) {
      br = r;
      bestC = c;
    }
  }
  return { tempF: mt, windMph: mw, humidityPct: mh, condition: bestC };
}

export function weatherSnapshotAtTeetime(hourly, teetimeParts, spanHours = 3) {
  if (!hourly?.time?.length || !teetimeParts) return null;
  const ttStr = teetimeStrForHourlyIndex(teetimeParts);
  const ix = hourlyIndexForDgTeetime(hourly.time, ttStr);
  if (ix < 0) return null;
  return summarizeHourlyWeatherSlice(hourly, ix, spanHours);
}

export class ArchiveHourlyCache {
  constructor() {
    this.byKey = new Map();
    this.inFlight = new Map();
  }

  cacheKey(lat, lon, startDate, endDate, timezone) {
    return `${lat.toFixed(4)}|${lon.toFixed(4)}|${startDate}|${endDate}|${timezone}`;
  }

  async fetch(lat, lon, startDate, endDate, timezone, opts = {}) {
    const key = this.cacheKey(lat, lon, startDate, endDate, timezone);
    if (this.byKey.has(key)) return this.byKey.get(key);
    if (this.inFlight.has(key)) return this.inFlight.get(key);

    const p = (async () => {
      if (opts.delayMs) await sleep(opts.delayMs);
      const url = openMeteoArchiveUrl(lat, lon, startDate, endDate, timezone);
      const res = await fetch(url);
      if (!res.ok) throw new Error(`Open-Meteo archive ${res.status}`);
      const j = await res.json();
      const hourly = j?.hourly;
      if (!hourly?.time?.length) throw new Error("Open-Meteo archive empty hourly");
      this.byKey.set(key, hourly);
      return hourly;
    })();

    this.inFlight.set(key, p);
    try {
      return await p;
    } finally {
      this.inFlight.delete(key);
    }
  }
}

export function roundWeatherFromHourly(hourly, teePartsList, spanHours = 3) {
  const samples = [];
  for (const parts of teePartsList) {
    const snap = weatherSnapshotAtTeetime(hourly, parts, spanHours);
    if (snap) samples.push(snap);
  }
  return medianWeatherFromSnapshots(samples);
}

export function loadHistoricalRoundWeatherMap(jsonPath = DEFAULT_ROUND_WEATHER_JSON) {
  const map = new Map();
  if (!jsonPath || !fs.existsSync(jsonPath)) return map;
  try {
    const raw = JSON.parse(fs.readFileSync(jsonPath, "utf8"));
    const rows = raw?.rounds || raw?.byKey || raw;
    if (Array.isArray(rows)) {
      for (const r of rows) {
        const k = r.key || roundWeatherKey(r.event_id, r.year, r.round_num);
        if (!k || !Number.isFinite(r.tempF ?? r.weather_temp_f)) continue;
        map.set(k, {
          tempF: Number(r.tempF ?? r.weather_temp_f),
          windMph: Number(r.windMph ?? r.weather_wind_mph),
          humidityPct: Number(r.humidityPct ?? r.weather_humidity),
          condition: String(r.condition ?? r.weather_condition ?? ""),
        });
      }
    } else if (rows && typeof rows === "object") {
      for (const [k, v] of Object.entries(rows)) {
        if (!v || typeof v !== "object") continue;
        map.set(k, {
          tempF: Number(v.tempF ?? v.weather_temp_f),
          windMph: Number(v.windMph ?? v.weather_wind_mph),
          humidityPct: Number(v.humidityPct ?? v.weather_humidity),
          condition: String(v.condition ?? v.weather_condition ?? ""),
        });
      }
    }
  } catch (e) {
    console.warn("[historical-round-weather] Failed to load", jsonPath, e?.message || e);
  }
  return map;
}
