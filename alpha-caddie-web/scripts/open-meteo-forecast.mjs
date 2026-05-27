/**
 * Open-Meteo venue forecast + per-tee weather (shared by bake-weather-into-projections.mjs).
 * Logic mirrors alpha-caddie-web/app.js refreshForecastWeatherFromOpenMeteo.
 */
import { eventsLikelySame, fieldWeekKey, fieldWeekKeysRoughMatch } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import { summarizeHourlyWeatherSlice } from "./open-meteo-weather-classify.mjs";
import { applyWeatherBakedCountsToAllPlayers } from "./weather-projection-adjustments.mjs";

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

/** Normalized course_used → lat/lon (extend as venues change). */
export const COURSE_COORDINATES_BY_NAME = {
  "aronimink golf club": { lat: 39.991, lon: -75.308 },
  "quail hollow club": { lat: 35.1158, lon: -80.8529 },
  "augusta national golf club": { lat: 33.503, lon: -82.0199 },
  "the stadium course at tpc sawgrass": { lat: 30.198, lon: -81.394 },
  "tpc sawgrass": { lat: 30.198, lon: -81.394 },
  "tpc craig ranch": { lat: 33.1972, lon: -96.7314 },
  "oak hill country club": { lat: 43.1227, lon: -77.5229 },
  "torrey pines golf course": { lat: 32.8955, lon: -117.246 },
  "the oceans course at half moon bay golf links": { lat: 37.4636, lon: -122.449 },
  "pebble beach golf links": { lat: 36.5698, lon: -121.9506 },
  "harbour town golf links": { lat: 32.1392, lon: -80.8107 },
  "east lake golf club": { lat: 33.7437, lon: -84.349 },
  "wilmington country club": { lat: 39.7878, lon: -84.2108 },
  "castle pines golf club": { lat: 39.4189, lon: -104.894 },
  "detroit golf club": { lat: 42.4369, lon: -83.161 },
  "royal liverpool golf club": { lat: 53.3728, lon: -3.184 },
  "the riviera country club": { lat: 34.0497, lon: -118.501 },
  "colonial country club": { lat: 32.7248, lon: -97.434 },
  "muirfield village golf club": { lat: 40.1416, lon: -82.791 },
  "congressional country club": { lat: 39.0299, lon: -77.164 },
};

export function courseCoordinatesForProjections(proj) {
  const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : proj;
  const raw = meta?.course_used ?? proj?.course_used ?? "";
  const key = normCourseNameKey(raw);
  return COURSE_COORDINATES_BY_NAME[key] || null;
}

export function forecastTimezoneFromProjections(proj) {
  const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : proj;
  const lab = String(meta?.display_round_label || "");
  const m = lab.match(/America\/[A-Za-z_/]+/);
  if (m) return m[0];
  return "America/New_York";
}

export function openMeteoForecastUrl(lat, lon, timezone) {
  const u = new URL("https://api.open-meteo.com/v1/forecast");
  u.searchParams.set("latitude", String(lat));
  u.searchParams.set("longitude", String(lon));
  u.searchParams.set(
    "hourly",
    "temperature_2m,relativehumidity_2m,precipitation_probability,precipitation,windspeed_10m,weathercode",
  );
  u.searchParams.set("windspeed_unit", "mph");
  u.searchParams.set("temperature_unit", "fahrenheit");
  u.searchParams.set("forecast_days", "8");
  u.searchParams.set("timezone", timezone || "America/New_York");
  return u.href;
}

export function parseDgTeetimeParts(teetimeStr) {
  const m = String(teetimeStr || "")
    .trim()
    .match(/^(\d{4}-\d{2}-\d{2})[ T](\d{1,2}):(\d{2})/);
  if (!m) return null;
  return { ymd: m[1], hh: parseInt(m[2], 10), mm: parseInt(m[3], 10) };
}

function teeHourFloorIsoFromDg(teetimeStr) {
  const p = parseDgTeetimeParts(teetimeStr);
  if (!p) return "";
  const hh = String(p.hh).padStart(2, "0");
  return `${p.ymd}T${hh}:00`;
}

export function hourlyIndexForDgTeetime(timesArr, teetimeStr) {
  const floorIso = teeHourFloorIsoFromDg(teetimeStr);
  const p = parseDgTeetimeParts(teetimeStr);
  if (!floorIso || !p || !Array.isArray(timesArr) || !timesArr.length) return -1;
  for (let i = 0; i < timesArr.length; i++) {
    const t = String(timesArr[i] || "");
    if (t.length >= 16 && t.slice(0, 16) >= floorIso.slice(0, 16)) return i;
  }
  let lastSameDay = -1;
  for (let i = 0; i < timesArr.length; i++) {
    const t = String(timesArr[i] || "");
    if (t.slice(0, 10) !== p.ymd) continue;
    lastSameDay = i;
  }
  return lastSameDay;
}

export function hourlySliceWeatherSnapshot(hourly, startIdx, spanHours) {
  return summarizeHourlyWeatherSlice(hourly, startIdx, spanHours);
}

function medianFinite(vals) {
  const a = vals.filter((x) => Number.isFinite(x)).sort((x, y) => x - y);
  if (!a.length) return NaN;
  const mid = Math.floor(a.length / 2);
  return a.length % 2 ? a[mid] : (a[mid - 1] + a[mid]) / 2;
}

function medianWeatherSnapshotFromSamples(samples) {
  if (!samples.length) return null;
  const mt = medianFinite(samples.map((s) => s.tempF));
  const mw = medianFinite(samples.map((s) => s.windMph));
  const mh = medianFinite(samples.map((s) => s.humidityPct));
  if (!Number.isFinite(mt) || !Number.isFinite(mw) || !Number.isFinite(mh)) return null;
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

function forecastAnchorDateYmd(hourly, players, meta) {
  const times = hourly?.time;
  if (!Array.isArray(times) || !times.length) return "";
  const hasDay = (ymd) => times.some((t) => String(t || "").slice(0, 10) === ymd);

  const majorityTeeDate = (roundFilter) => {
    const counts = new Map();
    for (const pl of players || []) {
      if (roundFilter != null && Math.round(num(pl?.round, NaN)) !== roundFilter) continue;
      const tt = parseDgTeetimeParts(pl?.dg_teetime_local);
      if (!tt || !hasDay(tt.ymd)) continue;
      counts.set(tt.ymd, (counts.get(tt.ymd) || 0) + 1);
    }
    let best = "";
    let bestN = -1;
    for (const [ymd, n] of counts) {
      if (n > bestN) {
        bestN = n;
        best = ymd;
      }
    }
    return best;
  };

  const displayRound = Math.round(num(meta?.display_round ?? meta?.datagolf_live_current_round, NaN));
  if (Number.isFinite(displayRound) && displayRound >= 1) {
    const roundDay = majorityTeeDate(displayRound);
    if (roundDay) return roundDay;
  }

  const fieldDay = majorityTeeDate(null);
  if (fieldDay) return fieldDay;

  const ds = String(meta?.datagolf_field_date_start || "").match(/^(\d{4}-\d{2}-\d{2})/);
  if (ds && hasDay(ds[1])) return ds[1];

  return String(times[0]).slice(0, 10);
}

function firstHourIndexOnDate(hourly, dateYmd) {
  const times = hourly?.time;
  if (!Array.isArray(times) || !dateYmd) return -1;
  for (let i = 0; i < times.length; i++) {
    if (String(times[i] || "").slice(0, 10) === dateYmd) return i;
  }
  return -1;
}

function hourlyIndexNearLocalHour(hourly, dateYmd, hour) {
  const times = hourly?.time;
  if (!Array.isArray(times) || !dateYmd) return -1;
  const want = `${dateYmd}T${String(Math.min(23, Math.max(0, hour))).padStart(2, "0")}`;
  let lastSameDay = -1;
  for (let i = 0; i < times.length; i++) {
    const t = String(times[i] || "");
    if (t.slice(0, 10) !== dateYmd) continue;
    lastSameDay = i;
    if (t.length >= 13 && t.slice(0, 13) >= want) return i;
  }
  return lastSameDay;
}

export function computeMorningAfternoonForecastSnapshots(hourly, players, meta) {
  if (!hourly) return { morning: null, afternoon: null };
  const timesArr = hourly.time;
  if (!Array.isArray(timesArr) || !timesArr.length) return { morning: null, afternoon: null };

  const dateYmd = forecastAnchorDateYmd(hourly, players, meta);
  if (!dateYmd) return { morning: null, afternoon: null };

  const dayStart = firstHourIndexOnDate(hourly, dateYmd);
  if (dayStart < 0) return { morning: null, afternoon: null };

  let ixM = hourlyIndexNearLocalHour(hourly, dateYmd, 8);
  let ixA = hourlyIndexNearLocalHour(hourly, dateYmd, 15);
  if (ixM < 0) ixM = dayStart;
  if (ixA < 0) ixA = hourlyIndexNearLocalHour(hourly, dateYmd, 14);
  if (ixA < 0) ixA = Math.min(timesArr.length - 5, dayStart + 7);

  const minGap = 5;
  if (ixA - ixM < minGap) ixA = Math.min(timesArr.length - 5, ixM + minGap);
  if (ixA <= ixM) ixA = Math.min(timesArr.length - 5, ixM + minGap);

  return {
    morning: hourlySliceWeatherSnapshot(hourly, ixM, 5),
    afternoon: hourlySliceWeatherSnapshot(hourly, ixA, 5),
  };
}

function buildForecastWaveSummaryString(morningSnap, afternoonSnap) {
  const fmt = (w) => {
    if (!w || !Number.isFinite(w.tempF)) return "";
    return `${w.tempF.toFixed(1)}°F · ${w.windMph.toFixed(1)} mph · ${w.humidityPct.toFixed(0)}% · ${String(w.condition || "neutral")}`;
  };
  const m = fmt(morningSnap);
  const a = fmt(afternoonSnap);
  if (!m && !a) return "";
  const parts = [];
  if (m) parts.push(`Morning tees: ${m}.`);
  if (a) parts.push(`Afternoon tees: ${a}.`);
  return parts.join("\n");
}

function applyWeatherSnapshotToPlayer(p, snap) {
  if (!snap || !Number.isFinite(snap.tempF) || !Number.isFinite(snap.windMph) || !Number.isFinite(snap.humidityPct)) {
    delete p.dg_auto_weather;
    p.weather_temp_f = null;
    p.weather_wind_mph = null;
    p.weather_humidity = null;
    p.weather_condition = "";
    return false;
  }
  p.dg_auto_weather = { ...snap };
  p.weather_temp_f = Math.round(snap.tempF * 10) / 10;
  p.weather_wind_mph = Math.round(snap.windMph * 10) / 10;
  p.weather_humidity = Math.round(snap.humidityPct);
  p.weather_condition = String(snap.condition || "default").toLowerCase();
  return true;
}

/** field_updates tee times → projection rows (dg_teetime_local). */
export function mergeFieldTeeTimesIntoProjections(proj, fieldUpdatesRaw) {
  const players = Array.isArray(proj?.players) ? proj.players : [];
  if (!fieldUpdatesRaw || typeof fieldUpdatesRaw !== "object" || !players.length) return 0;
  if (!proj.meta || typeof proj.meta !== "object") proj.meta = {};
  const ds = fieldUpdatesRaw.date_start != null ? String(fieldUpdatesRaw.date_start).trim() : "";
  if (ds) proj.meta.datagolf_field_date_start = ds;
  const flist =
    fieldUpdatesRaw.field ??
    fieldUpdatesRaw.field_updates ??
    fieldUpdatesRaw.players ??
    fieldUpdatesRaw.data;
  if (!Array.isArray(flist) || !flist.length) return 0;
  const byDg = new Map();
  for (const fp of flist) {
    const id = Math.round(num(fp?.dg_id ?? fp?.dgId, NaN));
    if (!Number.isFinite(id)) continue;
    byDg.set(id, Array.isArray(fp.teetimes) ? fp.teetimes : []);
  }
  let n = 0;
  for (const p of players) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const tt = byDg.get(id);
    if (!Array.isArray(tt)) {
      delete p.dg_teetime_local;
      delete p.dg_tee_wave;
      continue;
    }
    const rnd = Math.round(num(p.round, NaN));
    const slot = tt.find((t) => Math.round(num(t.round_num, NaN)) === rnd);
    if (slot && slot.teetime != null && String(slot.teetime).trim() !== "") {
      p.dg_teetime_local = String(slot.teetime).trim();
      p.dg_tee_wave = String(slot.wave || "").trim();
      n++;
    } else {
      delete p.dg_teetime_local;
      delete p.dg_tee_wave;
    }
  }
  return n;
}

export function fieldUpdatesAlignWithProjections(proj, fieldUpdatesRaw) {
  if (!fieldUpdatesRaw || typeof fieldUpdatesRaw !== "object") return false;
  const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : {};
  const modelEvent = String(meta.event_name || proj.event_name || "").trim();
  const fuEvent = String(fieldUpdatesRaw.event_name ?? fieldUpdatesRaw.eventName ?? "").trim();
  if (!modelEvent || !fuEvent) return true;
  if (eventsLikelySame(modelEvent, fuEvent)) return true;
  const projCourse = String(meta.course_used || proj.course_used || "").trim();
  const fuCourse = String(fieldUpdatesRaw.course_name ?? fieldUpdatesRaw.course ?? "").trim();
  return fieldWeekKeysRoughMatch(fieldWeekKey(modelEvent, projCourse), fieldWeekKey(fuEvent, fuCourse));
}

/**
 * Fetch Open-Meteo, merge tee times, write per-player weather + meta forecast fields.
 * @param {object} proj — projections payload (mutated)
 * @param {{ fieldUpdates?: object | null }} [opts]
 */
export async function bakeOpenMeteoWeatherIntoProjections(proj, opts = {}) {
  const players = Array.isArray(proj?.players) ? proj.players : [];
  if (!proj.meta || typeof proj.meta !== "object") proj.meta = {};
  const meta = proj.meta;

  const fieldUpdates = opts.fieldUpdates ?? null;
  if (fieldUpdates && fieldUpdatesAlignWithProjections(proj, fieldUpdates)) {
    const teeN = mergeFieldTeeTimesIntoProjections(proj, fieldUpdates);
    meta.forecast_weather_tee_times_merged = teeN;
  }

  const coords = courseCoordinatesForProjections(proj);
  if (!coords || !players.length) {
    for (const p of players) {
      delete p.dg_auto_weather;
      p.weather_temp_f = null;
      p.weather_wind_mph = null;
      p.weather_humidity = null;
      p.weather_condition = "";
    }
    meta.forecast_weather_status = coords ? "no_players" : "no_course_coords";
    meta.forecast_wave_slots = { morning: null, afternoon: null };
    meta.forecast_wave_summary = "";
    return { status: meta.forecast_weather_status, playerCount: players.length, playersWithWeather: 0, teeMatches: 0 };
  }

  const tz = forecastTimezoneFromProjections(proj);
  let hourly;
  try {
    const res = await fetch(openMeteoForecastUrl(coords.lat, coords.lon, tz));
    if (!res.ok) throw new Error(String(res.status));
    const j = await res.json();
    hourly = j.hourly;
  } catch (e) {
    meta.forecast_weather_status = "open_meteo_fetch_failed";
    meta.forecast_weather_error = String(e?.message || e);
    meta.forecast_wave_slots = { morning: null, afternoon: null };
    meta.forecast_wave_summary = "";
    return { status: meta.forecast_weather_status, playerCount: players.length, playersWithWeather: 0, teeMatches: 0 };
  }

  const timesArr = hourly?.time;
  if (!Array.isArray(timesArr) || !timesArr.length) {
    meta.forecast_weather_status = "empty_hourly";
    meta.forecast_wave_slots = { morning: null, afternoon: null };
    meta.forecast_wave_summary = "";
    return { status: meta.forecast_weather_status, playerCount: players.length, playersWithWeather: 0, teeMatches: 0 };
  }

  const perTeeSamples = [];
  for (const p of players) {
    const tt = p?.dg_teetime_local;
    if (!tt) continue;
    const ix = hourlyIndexForDgTeetime(timesArr, tt);
    if (ix < 0) continue;
    const snap = hourlySliceWeatherSnapshot(hourly, ix, 5);
    if (snap) perTeeSamples.push(snap);
  }
  const medianSnap = medianWeatherSnapshotFromSamples(perTeeSamples);

  let playersWithWeather = 0;
  for (const p of players) {
    const tt = p?.dg_teetime_local;
    let snap = null;
    if (tt) {
      const ix = hourlyIndexForDgTeetime(timesArr, tt);
      if (ix >= 0) snap = hourlySliceWeatherSnapshot(hourly, ix, 5);
    }
    if (!snap && medianSnap && Number.isFinite(medianSnap.tempF)) snap = { ...medianSnap };
    if (applyWeatherSnapshotToPlayer(p, snap)) playersWithWeather++;
  }

  meta.forecast_weather_status = perTeeSamples.length ? "ok_tee_time" : medianSnap ? "ok_median" : "no_tee_match";
  meta.forecast_weather_updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  meta.forecast_weather_coords = { lat: coords.lat, lon: coords.lon, timezone: tz };
  delete meta.forecast_weather_error;

  const { morning, afternoon } = computeMorningAfternoonForecastSnapshots(hourly, players, meta);
  meta.forecast_wave_slots = { morning, afternoon };
  meta.forecast_wave_summary = buildForecastWaveSummaryString(morning, afternoon);

  const countsBaked = applyWeatherBakedCountsToAllPlayers(proj);

  return {
    status: meta.forecast_weather_status,
    playerCount: players.length,
    playersWithWeather,
    teeMatches: perTeeSamples.length,
    countsWeatherBaked: countsBaked,
  };
}
