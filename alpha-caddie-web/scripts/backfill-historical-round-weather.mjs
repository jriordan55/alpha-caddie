#!/usr/bin/env node
/**
 * Backfill per-round weather (Open-Meteo archive at tee time) for historical rounds.
 * Writes alpha-caddie-web/data/historical_round_weather.json keyed by event_id|year|round_num.
 *
 * Run: npm run backfill:round-weather
 * Env: HISTORICAL_ROUNDS_CSV, GOLF_MODEL_DIR, HISTORICAL_ROUND_WEATHER_JSON
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { roundEventCompletedMdYFromEventEnd } from "./history-round-dates.mjs";
import {
  ArchiveHourlyCache,
  DEFAULT_ROUND_WEATHER_JSON,
  geocodeCourseName,
  mdyToIsoYmd,
  parseHistoricalRoundTeetime,
  roundWeatherFromHourly,
  roundWeatherKey,
} from "./historical-round-weather.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = path.resolve(WEB_ROOT, "..");

function resolveRoundsCsv() {
  const env = process.env.HISTORICAL_ROUNDS_CSV;
  if (env) return path.resolve(env);
  for (const p of [
    path.join(MODEL_ROOT, "data", "historical_rounds_all.csv"),
    path.join(WEB_ROOT, "data", "historical_rounds_all.csv"),
  ]) {
    if (fs.existsSync(p)) return p;
  }
  throw new Error("Missing historical_rounds_all.csv");
}

function num(x) {
  const n = Number(x);
  return Number.isFinite(n) ? n : NaN;
}

async function collectRoundGroups(csvPath) {
  /** @type {Map<string, { event_id:number, year:number, round_num:number, course_name:string, minYmd:string, maxYmd:string, teeParts: object[] }>} */
  const groups = new Map();

  const parser = createReadStream(csvPath).pipe(
    parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
  );

  for await (const row of parser) {
    const tour = String(row.tour || "").toLowerCase();
    if (tour !== "pga" && tour !== "liv") continue;
    const yr = parseInt(row.year, 10);
    const minYear = Math.round(num(process.env.GOLF_HISTORY_MIN_YEAR)) || 2004;
    if (!Number.isFinite(yr) || yr < minYear) continue;
    const eid = Math.round(num(row.event_id));
    const rnd = parseInt(row.round_num, 10) || 1;
    if (!Number.isFinite(eid)) continue;

    const roundDate = roundEventCompletedMdYFromEventEnd(row.event_completed, rnd, tour);
    const ymd = mdyToIsoYmd(roundDate);
    if (!ymd) continue;

    const teeParts = parseHistoricalRoundTeetime(row.teetime, roundDate);
    const key = roundWeatherKey(eid, yr, rnd);
    let g = groups.get(key);
    if (!g) {
      g = {
        event_id: eid,
        year: yr,
        round_num: rnd,
        event_name: String(row.event_name || "").trim(),
        course_name: String(row.course_name || row.course || "").trim(),
        minYmd: ymd,
        maxYmd: ymd,
        teeParts: [],
      };
      groups.set(key, g);
    }
    if (!g.course_name && row.course_name) g.course_name = String(row.course_name).trim();
    if (ymd < g.minYmd) g.minYmd = ymd;
    if (ymd > g.maxYmd) g.maxYmd = ymd;
    if (teeParts) g.teeParts.push(teeParts);
  }

  for (const g of groups.values()) {
    if (g.teeParts.length) continue;
    const ymd = g.minYmd;
    if (!ymd) continue;
    g.teeParts.push({ ymd, hh: 8, mm: 0 }, { ymd, hh: 13, mm: 0 });
  }

  return groups;
}

function dedupeTeeParts(list) {
  const seen = new Set();
  const out = [];
  for (const p of list) {
    const k = `${p.ymd}|${p.hh}|${p.mm}`;
    if (seen.has(k)) continue;
    seen.add(k);
    out.push(p);
  }
  return out;
}

async function loadEventCityHints() {
  /** @type {Map<string, string>} */
  const map = new Map();
  for (const dir of [path.join(WEB_ROOT, "data"), path.join(MODEL_ROOT, "data")]) {
    if (!fs.existsSync(dir)) continue;
    const files = fs
      .readdirSync(dir)
      .filter((f) => /^historical_rounds_all_with_tournament_metadata(_\d{8}_\d{6})?\.csv$/i.test(f));
    if (!files.length) continue;
    const overlayPath = path.join(dir, files.sort().pop());
    const parser = createReadStream(overlayPath).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    for await (const row of parser) {
      const eid = Math.round(num(row.event_id));
      const yr = parseInt(row.year, 10);
      if (!Number.isFinite(eid) || !Number.isFinite(yr)) continue;
      const city = String(row.pga_meta_city ?? "").trim();
      if (!city) continue;
      map.set(`${eid}|${yr}`, city.split(",")[0].trim());
    }
    if (map.size) break;
  }
  return map;
}

async function main() {
  const csvPath = resolveRoundsCsv();
  const outPath = process.env.HISTORICAL_ROUND_WEATHER_JSON
    ? path.resolve(process.env.HISTORICAL_ROUND_WEATHER_JSON)
    : DEFAULT_ROUND_WEATHER_JSON;

  console.log("[backfill:round-weather] Scanning", csvPath);
  const groups = await collectRoundGroups(csvPath);
  console.log("[backfill:round-weather] Unique event×round groups:", groups.size);

  const byEvent = new Map();
  for (const g of groups.values()) {
    const ek = `${g.event_id}|${g.year}`;
    let ev = byEvent.get(ek);
    if (!ev) {
      ev = {
        event_id: g.event_id,
        year: g.year,
        event_name: g.event_name,
        course_name: g.course_name,
        minYmd: g.minYmd,
        maxYmd: g.maxYmd,
        rounds: [],
      };
      byEvent.set(ek, ev);
    }
    if (!ev.course_name && g.course_name) ev.course_name = g.course_name;
    if (g.minYmd < ev.minYmd) ev.minYmd = g.minYmd;
    if (g.maxYmd > ev.maxYmd) ev.maxYmd = g.maxYmd;
    ev.rounds.push(g);
  }

  const archive = new ArchiveHourlyCache();
  const roundWeather = {};
  let fetchedEvents = 0;
  let roundsWithWeather = 0;
  let geocodeMiss = 0;

  const eventList = [...byEvent.values()].sort((a, b) => a.year - b.year || a.event_id - b.event_id);
  const cityHints = await loadEventCityHints();
  if (cityHints.size) console.log("[backfill:round-weather] City hints from metadata:", cityHints.size);

  for (const ev of eventList) {
    const coords = await geocodeCourseName(ev.course_name || ev.event_name, {
      persistCoordsCache: true,
      cityHint: cityHints.get(`${ev.event_id}|${ev.year}`) || "",
      eventName: ev.event_name,
    });
    if (!coords) {
      geocodeMiss++;
      console.warn(
        `[backfill:round-weather] No coords for event ${ev.event_id} ${ev.year} (${ev.event_name}) course=${ev.course_name}`,
      );
      continue;
    }

    let hourly;
    try {
      hourly = await archive.fetch(coords.lat, coords.lon, ev.minYmd, ev.maxYmd, coords.timezone, {
        delayMs: fetchedEvents > 0 ? 120 : 0,
      });
      fetchedEvents++;
    } catch (e) {
      console.warn(`[backfill:round-weather] Archive fetch failed ${ev.event_name} ${ev.year}:`, e?.message || e);
      continue;
    }

    for (const g of ev.rounds) {
      g.teeParts = dedupeTeeParts(g.teeParts);
      const snap = roundWeatherFromHourly(hourly, g.teeParts);
      if (!snap) continue;
      const k = roundWeatherKey(g.event_id, g.year, g.round_num);
      roundWeather[k] = {
        event_id: g.event_id,
        year: g.year,
        round_num: g.round_num,
        event_name: g.event_name,
        course_name: g.course_name,
        tempF: Math.round(snap.tempF * 10) / 10,
        windMph: Math.round(snap.windMph * 10) / 10,
        humidityPct: Math.round(snap.humidityPct),
        condition: snap.condition,
      };
      roundsWithWeather++;
    }

    if (fetchedEvents % 25 === 0) {
      console.log(`[backfill:round-weather] … ${fetchedEvents}/${eventList.length} events, ${roundsWithWeather} rounds`);
    }
  }

  let mergedPrior = 0;
  if (fs.existsSync(outPath)) {
    try {
      const prior = JSON.parse(fs.readFileSync(outPath, "utf8"));
      const prev = prior?.byKey && typeof prior.byKey === "object" ? prior.byKey : {};
      for (const [k, v] of Object.entries(prev)) {
        if (!roundWeather[k] && v && typeof v === "object") {
          roundWeather[k] = v;
          mergedPrior++;
        }
      }
    } catch {
      /* fresh write */
    }
  }

  const payload = {
    generated_at: new Date().toISOString(),
    source_csv: path.basename(csvPath),
    open_meteo: "archive-api.open-meteo.com/v1/archive",
    wind_metric: "mean_mph_sustained_10m_in_tee_window",
    min_year: Math.round(num(process.env.GOLF_HISTORY_MIN_YEAR)) || 2004,
    event_count: fetchedEvents,
    round_count: Object.keys(roundWeather).length,
    geocode_miss_events: geocodeMiss,
    merged_prior_rounds: mergedPrior,
    byKey: roundWeather,
  };

  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, JSON.stringify(payload, null, 2));
  console.log(`[backfill:round-weather] Wrote ${Object.keys(roundWeather).length} rounds → ${outPath}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
