#!/usr/bin/env node
/**
 * Copy `teetime` from historical_rounds_all.csv into player-history shards.
 * Apply Open-Meteo archive weather from data/historical_round_weather.json.
 *
 *   npm run patch:history-teetimes
 */
import fs from "fs";
import path from "path";
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { fileURLToPath } from "url";
import { resolveGolfModelDir } from "./resolve-golf-model-dir.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = resolveGolfModelDir(WEB_ROOT);
const ROUNDS_CSV =
  process.env.HISTORICAL_ROUNDS_CSV ||
  [path.join(REPO_ROOT, "data", "historical_rounds_all.csv"), path.join(WEB_ROOT, "data", "historical_rounds_all.csv")].find(
    (p) => fs.existsSync(p),
  ) ||
  path.join(REPO_ROOT, "data", "historical_rounds_all.csv");
const WEATHER_JSON = path.join(WEB_ROOT, "data", "historical_round_weather.json");
const SHARD_DIRS = [
  path.join(WEB_ROOT, "player-history", "by-dg"),
  path.join(WEB_ROOT, "player-history", "by-course"),
];

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

function roundKeys(dg, row) {
  const rn = Math.round(Number(row.round_num)) || 1;
  const y = Math.round(Number(row.year));
  const eid = String(row.event_id || "").trim();
  const evt = normEvt(row.event_name);
  const keys = [];
  if (eid) keys.push(`${dg}|eid:${eid}|${rn}`);
  if (evt && Number.isFinite(y)) keys.push(`${dg}|${evt}|${y}|${rn}`);
  return keys;
}

function shardRoundKeys(dg, r) {
  const rn = Math.round(Number(r.round_num)) || 1;
  const y = Math.round(Number(r.year));
  const eid = String(r.event_id || "").trim();
  const evt = normEvt(r.event_name);
  const keys = [];
  if (eid) keys.push(`${dg}|eid:${eid}|${rn}`);
  if (evt && Number.isFinite(y)) keys.push(`${dg}|${evt}|${y}|${rn}`);
  return keys;
}

function eventRoundWeatherKey(r) {
  const eid = Math.round(Number(r.event_id));
  const yr = Math.round(Number(r.year));
  const rnd = Math.round(Number(r.round_num)) || 1;
  if (!Number.isFinite(eid) || !Number.isFinite(yr)) return "";
  return `${eid}|${yr}|${rnd}`;
}

function loadWeatherMap() {
  const map = new Map();
  if (!fs.existsSync(WEATHER_JSON)) {
    console.warn(`[patch:history-teetimes] Missing weather JSON — skip weather patch: ${WEATHER_JSON}`);
    return map;
  }
  const j = JSON.parse(fs.readFileSync(WEATHER_JSON, "utf8"));
  for (const [k, v] of Object.entries(j?.byKey || {})) map.set(k, v);
  console.log(`[patch:history-teetimes] Weather keys ${map.size.toLocaleString()}`);
  return map;
}

function roundNeedsWeather(r) {
  const t = r?.weather_temp_f;
  return t == null || t === "" || !Number.isFinite(Number(t));
}

function applyWeatherToRound(r, weatherMap) {
  if (!r || typeof r !== "object" || !weatherMap?.size) return false;
  if (!roundNeedsWeather(r)) return false;
  const wKey = eventRoundWeatherKey(r);
  const snap = weatherMap.get(wKey);
  if (!snap) return false;
  if (Number.isFinite(Number(snap.tempF))) r.weather_temp_f = snap.tempF;
  if (Number.isFinite(Number(snap.windMph))) r.weather_wind_mph = snap.windMph;
  if (Number.isFinite(Number(snap.humidityPct))) r.weather_humidity = snap.humidityPct;
  if (snap.condition) r.weather_condition = String(snap.condition).toLowerCase();
  r.weather_source = "open_meteo_archive";
  return true;
}

async function loadTeetimeMap() {
  const map = new Map();
  if (!fs.existsSync(ROUNDS_CSV)) {
    throw new Error(`Missing rounds CSV: ${ROUNDS_CSV}`);
  }
  let rows = 0;
  let withTee = 0;
  const parser = createReadStream(ROUNDS_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  );
  for await (const row of parser) {
    rows++;
    const tee = String(row.teetime ?? row.tee_time ?? "").trim();
    if (!tee) continue;
    withTee++;
    const dg = Math.round(Number(row.dg_id));
    if (!Number.isFinite(dg)) continue;
    for (const k of roundKeys(dg, row)) {
      if (!map.has(k)) map.set(k, tee);
    }
  }
  console.log(`[patch:history-teetimes] CSV rows ${rows.toLocaleString()}, with teetime ${withTee.toLocaleString()}`);
  return map;
}

function patchRoundList(rounds, dgDefault, teeMap, weatherMap) {
  let teePatched = 0;
  let weatherPatched = 0;
  let changed = false;
  for (const r of rounds) {
    if (!r || typeof r !== "object") continue;
    const dg = Number.isFinite(dgDefault) ? dgDefault : Math.round(Number(r.dg_id));
    if (!String(r?.teetime ?? "").trim() && Number.isFinite(dg)) {
      let tee = "";
      for (const k of shardRoundKeys(dg, r)) {
        const hit = teeMap.get(k);
        if (hit) {
          tee = hit;
          break;
        }
      }
      if (tee) {
        r.teetime = tee;
        changed = true;
        teePatched++;
      }
    }
    if (applyWeatherToRound(r, weatherMap)) {
      changed = true;
      weatherPatched++;
    }
  }
  return { changed, teePatched, weatherPatched };
}

function patchShardDir(shardDir, teeMap, weatherMap, label) {
  if (!fs.existsSync(shardDir)) {
    console.warn(`[patch:history-teetimes] No ${label} dir — skip`);
    return { files: 0, teePatched: 0, weatherPatched: 0 };
  }
  let files = 0;
  let teePatched = 0;
  let weatherPatched = 0;
  for (const entry of fs.readdirSync(shardDir, { withFileTypes: true })) {
    if (!entry.isFile() || !entry.name.endsWith(".json")) continue;
    const dgFromName = Math.round(Number(entry.name.replace(/\.json$/i, "")));
    const fp = path.join(shardDir, entry.name);
    let payload;
    try {
      payload = JSON.parse(fs.readFileSync(fp, "utf8"));
    } catch {
      continue;
    }
    let changed = false;
    const rounds = Array.isArray(payload?.rounds) ? payload.rounds : [];
    if (rounds.length) {
      const hit = patchRoundList(rounds, dgFromName, teeMap, weatherMap);
      changed = changed || hit.changed;
      teePatched += hit.teePatched;
      weatherPatched += hit.weatherPatched;
    }
    const entries = Array.isArray(payload?.entries) ? payload.entries : [];
    if (entries.length) {
      for (const ent of entries) {
        const row = ent?.row;
        if (!row || typeof row !== "object") continue;
        const dg = Math.round(Number(ent?.dg_id ?? row?.dg_id ?? dgFromName));
        const hit = patchRoundList([row], dg, teeMap, weatherMap);
        changed = changed || hit.changed;
        teePatched += hit.teePatched;
        weatherPatched += hit.weatherPatched;
      }
    }
    if (changed) {
      fs.writeFileSync(fp, `${JSON.stringify(payload)}\n`, "utf8");
      files++;
    }
  }
  return { files, teePatched, weatherPatched };
}

async function main() {
  const [teeMap, weatherMap] = await Promise.all([loadTeetimeMap(), Promise.resolve(loadWeatherMap())]);
  let totalFiles = 0;
  let totalTee = 0;
  let totalWeather = 0;
  for (const dir of SHARD_DIRS) {
    const label = path.basename(path.dirname(dir)) + "/" + path.basename(dir);
    const { files, teePatched, weatherPatched } = patchShardDir(dir, teeMap, weatherMap, label);
    totalFiles += files;
    totalTee += teePatched;
    totalWeather += weatherPatched;
    console.log(
      `[patch:history-teetimes] ${label}: ${teePatched.toLocaleString()} teetime(s), ${weatherPatched.toLocaleString()} weather row(s) across ${files.toLocaleString()} file(s).`,
    );
  }
  console.log(
    `[patch:history-teetimes] Total: ${totalTee.toLocaleString()} teetime(s), ${totalWeather.toLocaleString()} weather row(s) across ${totalFiles.toLocaleString()} file(s).`,
  );
}

main().catch((e) => {
  console.error("[patch:history-teetimes]", e?.message || e);
  process.exit(1);
});
