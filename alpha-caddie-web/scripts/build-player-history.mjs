#!/usr/bin/env node
/**
 * Builds player_round_history.json for the static web app from:
 *   - historical_rounds_all.csv (PGA + LIV rows; refresh with npm run update:rounds / fetch:dg)
 *   - optional hole_data.csv (hole-by-hole rows; joined by player + event + round)
 *
 * Only players present in projections.json (unique dg_id) are included to keep the file small.
 *
 * Env:
 *   GOLF_MODEL_DIR   - repo root (parent of alpha-caddie-web). Default: parent of this package.
 *   HISTORICAL_ROUNDS_CSV - override rounds path
 *   HOLE_DATA_CSV    - override hole_data path; set empty to skip holes pass
 *   GOLF_HISTORY_MIN_YEAR - first calendar year to keep from CSV (default 2004)
 *   GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER - cap per player after sort (default 2000; min 50 max 5000)
 *   HISTORICAL_ROUNDS_METADATA_OVERLAY_CSV - path to *_with_tournament_metadata*.csv for pga_meta_* merge;
 *     unset = auto-pick newest under data/; "" = disable (canonical columns only)
 *
 * Run from alpha-caddie-web: npm run build:history
 *
 * Optional pgatouR-only JSON (overwrites CSV build): npm run build:history:pga.
 * npm run fetch:dg refreshes data/historical_rounds_all.csv then runs this script; set
 * ALPHA_CADDIE_PGA_HISTORY=1 on fetch:dg to run the PGA builder after the CSV build.
 *
 * CSV path: prefer golfModel/data/historical_rounds_all.csv (full history). Only if that is missing,
 * use the newest historical_rounds_all_with_tournament_metadata*.csv (snapshots are often partial).
 * Historical Trends and weather/meta filters use only this rounds CSV (+ join columns if present).
 * all_shots_2021_2026.csv is a different pipeline (build-player-shots-web.mjs); not used here.
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";
import { parse } from "csv-parse";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = process.env.GOLF_MODEL_DIR
  ? path.resolve(process.env.GOLF_MODEL_DIR)
  : path.resolve(WEB_ROOT, "..");

function resolveRoundsCsvPath() {
  if (process.env.HISTORICAL_ROUNDS_CSV) {
    return path.resolve(process.env.HISTORICAL_ROUNDS_CSV);
  }
  const canonicalModel = path.join(MODEL_ROOT, "data", "historical_rounds_all.csv");
  if (fs.existsSync(canonicalModel)) return canonicalModel;
  const canonicalWeb = path.join(WEB_ROOT, "data", "historical_rounds_all.csv");
  if (fs.existsSync(canonicalWeb)) return canonicalWeb;
  const inRoot = path.join(MODEL_ROOT, "historical_rounds_all.csv");
  if (fs.existsSync(inRoot)) return inRoot;

  const candidates = [];
  const dataDir = path.join(MODEL_ROOT, "data");
  const webDataDir = path.join(WEB_ROOT, "data");
  for (const dir of [dataDir, webDataDir]) {
    if (!fs.existsSync(dir)) continue;
    const files = fs
      .readdirSync(dir)
      .filter((f) => /^historical_rounds_all_with_tournament_metadata(_\d{8}_\d{6})?\.csv$/i.test(f))
      .map((f) => path.join(dir, f));
    for (const p of files) {
      try {
        const st = fs.statSync(p);
        candidates.push({ p, mtimeMs: st.mtimeMs });
      } catch (_) {
        // ignore
      }
    }
  }
  if (candidates.length) {
    candidates.sort((a, b) => b.mtimeMs - a.mtimeMs);
    return candidates[0].p;
  }
  return canonicalModel;
}

/**
 * When rounds come from canonical CSV, merge pga_meta_* from a join export (same event_id+year).
 * Skipped if the primary file is already a *_with_tournament_metadata*.csv.
 */
function resolveMetadataOverlayCsvPath() {
  const base = path.basename(ROUNDS_CSV);
  if (/historical_rounds_all_with_tournament_metadata/i.test(base)) return null;

  const raw = process.env.HISTORICAL_ROUNDS_METADATA_OVERLAY_CSV;
  if (raw !== undefined && String(raw).trim() === "") return null;
  if (raw !== undefined && String(raw).trim() !== "") {
    const p = path.resolve(String(raw).trim());
    return fs.existsSync(p) ? p : null;
  }

  const candidates = [];
  for (const dir of [path.join(MODEL_ROOT, "data"), path.join(WEB_ROOT, "data")]) {
    if (!fs.existsSync(dir)) continue;
    for (const f of fs.readdirSync(dir)) {
      if (!/^historical_rounds_all_with_tournament_metadata(_\d{8}_\d{6})?\.csv$/i.test(f)) continue;
      const p = path.join(dir, f);
      try {
        candidates.push({ p, mtimeMs: fs.statSync(p).mtimeMs });
      } catch (_) {
        // ignore
      }
    }
  }
  if (!candidates.length) return null;
  for (const c of candidates) {
    try {
      c.size = fs.statSync(c.p).size;
    } catch (_) {
      c.size = 0;
    }
  }
  candidates.sort((a, b) => b.size - a.size || b.mtimeMs - a.mtimeMs);
  return candidates[0].p;
}

function resolveHoleDataCsv() {
  if (process.env.HOLE_DATA_CSV === "") return null;
  if (process.env.HOLE_DATA_CSV) return path.resolve(process.env.HOLE_DATA_CSV);
  const inData = path.join(MODEL_ROOT, "data", "hole_data.csv");
  if (fs.existsSync(inData)) return inData;
  const inRoot = path.join(MODEL_ROOT, "hole_data.csv");
  if (fs.existsSync(inRoot)) return inRoot;
  const inWebData = path.join(WEB_ROOT, "data", "hole_data.csv");
  return fs.existsSync(inWebData) ? inWebData : null;
}

function relUnderModel(absPath) {
  const rel = path.relative(MODEL_ROOT, absPath);
  if (rel.startsWith("..") || path.isAbsolute(rel)) return path.basename(absPath);
  return rel.split(path.sep).join("/");
}

function shotsModelCsvMeta() {
  const p = path.join(MODEL_ROOT, "data", "all_shots_2021_2026.csv");
  if (!fs.existsSync(p)) {
    return { name: "all_shots_2021_2026.csv", present: false };
  }
  const st = fs.statSync(p);
  return {
    name: "all_shots_2021_2026.csv",
    present: true,
    mtime: new Date(st.mtimeMs).toISOString(),
    size_bytes: st.size,
  };
}

const ROUNDS_CSV = resolveRoundsCsvPath();
const METADATA_OVERLAY_CSV = resolveMetadataOverlayCsvPath();
const HOLES_CSV = resolveHoleDataCsv();
const PROJECTIONS_JSON = path.join(WEB_ROOT, "projections.json");
const OUT_JSON = path.join(WEB_ROOT, "player_round_history.json");

const CY = new Date().getFullYear();
const MIN_YEAR = (() => {
  const env = parseInt(String(process.env.GOLF_HISTORY_MIN_YEAR ?? "").trim(), 10);
  if (Number.isFinite(env) && env >= 1990 && env <= CY + 1) return env;
  return 2004;
})();
/** Max rounds stored per player (newest wins after sort). Keeps bundle size bounded. */
const MAX_ROUNDS_PER_PLAYER = (() => {
  const env = parseInt(String(process.env.GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER ?? "").trim(), 10);
  if (Number.isFinite(env) && env >= 50 && env <= 5000) return env;
  return 2000;
})();
/** Only attach hole-by-hole rows for this many most recent rounds per player (keeps JSON small). */
const HOLE_JOIN_TAIL = 28;

function num(x) {
  const n = Number(x);
  return Number.isFinite(n) ? n : NaN;
}

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

/** "Fleetwood, Tommy" -> fleetwood|tommy */
function playerKeyHistorical(name) {
  const s = String(name || "").trim();
  const m = s.match(/^(.+),\s*(.+)$/);
  if (m) return `${m[1].trim().toLowerCase()}|${m[2].trim().toLowerCase()}`;
  return s.toLowerCase();
}

/** "Tommy Fleetwood" -> fleetwood|tommy */
function playerKeyHole(name) {
  const parts = String(name || "")
    .trim()
    .split(/\s+/)
    .filter(Boolean);
  if (parts.length < 2) return parts.join(" ").toLowerCase();
  const last = parts[parts.length - 1].toLowerCase();
  const first = parts.slice(0, -1).join(" ").toLowerCase();
  return `${last}|${first}`;
}

function parseUsDateSortKey(s) {
  if (!s) return 0;
  const p = String(s).split("/");
  if (p.length !== 3) return 0;
  const mo = parseInt(p[0], 10);
  const d = parseInt(p[1], 10);
  const y = parseInt(p[2], 10);
  if (!Number.isFinite(y)) return 0;
  return y * 10000 + (mo || 0) * 100 + (d || 0);
}

function loadAllowedDgIds() {
  if (!fs.existsSync(PROJECTIONS_JSON)) {
    console.warn("No projections.json — export will include no players (add projections or run fetch:dg).");
    return new Set();
  }
  const raw = JSON.parse(fs.readFileSync(PROJECTIONS_JSON, "utf8"));
  const ids = new Set();
  for (const p of raw.players || []) {
    const id = Math.round(num(p.dg_id));
    if (Number.isFinite(id)) ids.add(id);
  }
  return ids;
}

function metricFields(row) {
  const gir = num(row.gir);
  const fa = num(row.driving_acc);
  const girCount = Number.isFinite(gir) ? (gir <= 2 ? Math.round(gir * 18) : Math.round(gir)) : null;
  const fwCount = Number.isFinite(fa) ? (fa <= 1 ? Math.round(fa * 14) : Math.round(fa)) : null;
  return {
    round_score: num(row.round_score),
    birdies: num(row.birdies),
    pars: num(row.pars),
    bogies: num(row.bogies),
    gir: girCount,
    fairways: fwCount,
    eagles_or_better: num(row.eagles_or_better),
    doubles_or_worse: num(row.doubles_or_worse),
  };
}

/** PGA CSV uses values like "71°F", "89%"; plain Number() is NaN — strip to a scalar like the web app filters. */
function parseWeatherScalar(v) {
  const s = String(v ?? "").trim();
  if (!s) return NaN;
  const direct = Number(s);
  if (Number.isFinite(direct)) return direct;
  const cleaned = s.replace(/[^0-9.-]+/g, "");
  const n = parseFloat(cleaned);
  return Number.isFinite(n) ? n : NaN;
}

function weatherFields(row) {
  const tempF = parseWeatherScalar(row.pga_meta_weather_temp_f ?? row.weather_temp_f);
  const windMph = parseWeatherScalar(row.pga_meta_weather_wind_mph ?? row.weather_wind_mph);
  const humidity = parseWeatherScalar(row.pga_meta_weather_humidity ?? row.weather_humidity);
  const condition = String(row.pga_meta_weather_condition ?? row.weather_condition ?? "").trim();
  return {
    weather_temp_f: Number.isFinite(tempF) ? tempF : null,
    weather_wind_mph: Number.isFinite(windMph) ? windMph : null,
    weather_humidity: Number.isFinite(humidity) ? humidity : null,
    weather_condition: condition || "",
  };
}

/** Strokes-gained columns for Skill focus pricing mode (from historical_rounds_all.csv). */
function sgFields(row) {
  const f = (k) => {
    const v = num(row[k]);
    return Number.isFinite(v) ? v : null;
  };
  return {
    sg_putt: f("sg_putt"),
    sg_app: f("sg_app"),
    sg_arg: f("sg_arg"),
    sg_ott: f("sg_ott"),
    sg_t2g: f("sg_t2g"),
    sg_total: f("sg_total"),
  };
}

function mergePgaMetaPatch(into, row) {
  for (const key of Object.keys(row)) {
    if (!key.startsWith("pga_meta_")) continue;
    const v = row[key];
    if (v == null || v === "") continue;
    if (into[key] == null || into[key] === "") into[key] = v;
  }
}

/** event_id|year -> merged pga_meta_* fields (first non-empty wins on duplicates). */
async function loadPgaMetaOverlayFromCsv(csvPath) {
  const map = new Map();
  if (!csvPath || !fs.existsSync(csvPath)) return map;
  const parser = createReadStream(csvPath).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );
  for await (const row of parser) {
    const eid = Math.round(num(row.event_id));
    const yr = parseInt(row.year, 10);
    if (!Number.isFinite(eid) || !Number.isFinite(yr)) continue;
    const k = `${eid}|${yr}`;
    let patch = map.get(k);
    if (!patch) {
      patch = {};
      map.set(k, patch);
    }
    mergePgaMetaPatch(patch, row);
  }
  console.log(
    "PGA metadata overlay:",
    map.size,
    "event-year keys from",
    path.basename(csvPath)
  );
  return map;
}

async function streamRounds(allowedDgIds, pgaMetaOverlay) {
  const byDgId = new Map();

  if (!fs.existsSync(ROUNDS_CSV)) {
    console.error("Missing rounds CSV:", ROUNDS_CSV);
    return { byDgId, allowedTriples: new Set() };
  }

  const parser = createReadStream(ROUNDS_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );

  for await (const row of parser) {
    const tour = String(row.tour || "").toLowerCase();
    if (tour !== "pga" && tour !== "liv") continue;
    const yr = parseInt(row.year, 10);
    if (Number.isFinite(yr) && yr < MIN_YEAR) continue;
    const dg = Math.round(num(row.dg_id));
    if (!Number.isFinite(dg) || !allowedDgIds.has(dg)) continue;
    const rs = num(row.round_score);
    if (!Number.isFinite(rs)) continue;

    const eid = Math.round(num(row.event_id));
    const metaPatch =
      pgaMetaOverlay && Number.isFinite(eid) ? pgaMetaOverlay.get(`${eid}|${yr}`) : null;
    const rowForWeather = metaPatch ? { ...row, ...metaPatch } : row;

    const sortKey = parseUsDateSortKey(row.event_completed) * 10 + (parseInt(row.round_num, 10) || 1);
    const eventName = String(row.event_name || "").trim();
    const courseRaw = String(
      row.course_name ||
        row.Course_Name ||
        row.course ||
        row.Course ||
        row.venue ||
        ""
    ).trim();
    const rec = {
      sortKey,
      event_completed: String(row.event_completed || ""),
      year: yr,
      event_name: eventName,
      event_id: String(row.event_id || ""),
      course_name: courseRaw || eventName,
      round_num: parseInt(row.round_num, 10) || 1,
      fin_text: String(row.fin_text || ""),
      ...metricFields(row),
      ...weatherFields(rowForWeather),
      ...sgFields(row),
    };

    if (!byDgId.has(dg)) byDgId.set(dg, { dg_id: dg, player_name: String(row.player_name || ""), rounds: [] });
    const bucket = byDgId.get(dg);
    if (!bucket.player_name) bucket.player_name = String(row.player_name || "");
    bucket.rounds.push(rec);
  }

  for (const [, bucket] of byDgId) {
    bucket.rounds.sort((a, b) => a.sortKey - b.sortKey);
    if (bucket.rounds.length > MAX_ROUNDS_PER_PLAYER) bucket.rounds = bucket.rounds.slice(-MAX_ROUNDS_PER_PLAYER);
  }

  const allowedTriples = new Set();
  for (const [, bucket] of byDgId) {
    const pk = playerKeyHistorical(bucket.player_name);
    const tail = bucket.rounds.slice(-HOLE_JOIN_TAIL);
    for (const r of tail) {
      allowedTriples.add(`${pk}|||${normEvt(r.event_name)}|||${r.round_num}`);
    }
  }

  return { byDgId, allowedTriples };
}

async function streamHoles(allowedTriples) {
  const holesByPlayerKey = {};
  if (!allowedTriples || allowedTriples.size === 0 || !HOLES_CSV || !fs.existsSync(HOLES_CSV)) {
    return holesByPlayerKey;
  }

  const parser = createReadStream(HOLES_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );

  for await (const row of parser) {
    const pk = playerKeyHole(row.player_name);
    const ev = normEvt(row.tournament_name);
    const rn = parseInt(row.round, 10) || 1;
    const triple = `${pk}|||${ev}|||${rn}`;
    if (!allowedTriples.has(triple)) continue;

    const uid = `${row.tournament_name || ""}\tR${rn}`;
    holesByPlayerKey[pk] ??= {};
    holesByPlayerKey[pk][uid] ??= [];
    holesByPlayerKey[pk][uid].push({
      hole: parseInt(row.hole, 10),
      par: parseInt(row.par, 10),
      score: parseInt(row.score, 10),
      score_type: String(row.score_type || ""),
    });
  }

  for (const pk of Object.keys(holesByPlayerKey)) {
    for (const uid of Object.keys(holesByPlayerKey[pk])) {
      holesByPlayerKey[pk][uid].sort((a, b) => a.hole - b.hole);
    }
  }

  return holesByPlayerKey;
}

async function main() {
  console.log("Rounds CSV:", ROUNDS_CSV);
  console.log("Metadata overlay CSV:", METADATA_OVERLAY_CSV || "(none)");
  console.log("min_year (CSV filter):", MIN_YEAR, "| max_rounds/player:", MAX_ROUNDS_PER_PLAYER);
  console.log("Holes CSV:", HOLES_CSV || "(skip)");
  const allowed = loadAllowedDgIds();
  console.log("Allowed dg_ids from projections:", allowed.size);

  const pgaMetaOverlay = METADATA_OVERLAY_CSV ? await loadPgaMetaOverlayFromCsv(METADATA_OVERLAY_CSV) : new Map();
  const { byDgId, allowedTriples } = await streamRounds(allowed, pgaMetaOverlay);
  console.log("Players with rounds:", byDgId.size);

  const holesByPlayerKey = await streamHoles(allowedTriples);
  const holePlayerCount = Object.keys(holesByPlayerKey).length;
  console.log("Players with hole rows matched:", holePlayerCount);

  const out = {
    meta: {
      updated_at: new Date().toISOString(),
      source_csv: path.basename(ROUNDS_CSV),
      rounds_csv_relpath: relUnderModel(ROUNDS_CSV),
      rounds_csv_mtime: fs.existsSync(ROUNDS_CSV)
        ? new Date(fs.statSync(ROUNDS_CSV).mtimeMs).toISOString()
        : null,
      metadata_overlay_csv: METADATA_OVERLAY_CSV ? path.basename(METADATA_OVERLAY_CSV) : null,
      metadata_overlay_csv_relpath: METADATA_OVERLAY_CSV ? relUnderModel(METADATA_OVERLAY_CSV) : null,
      metadata_overlay_csv_mtime:
        METADATA_OVERLAY_CSV && fs.existsSync(METADATA_OVERLAY_CSV)
          ? new Date(fs.statSync(METADATA_OVERLAY_CSV).mtimeMs).toISOString()
          : null,
      holes_csv: HOLES_CSV ? path.basename(HOLES_CSV) : null,
      holes_csv_relpath: HOLES_CSV ? relUnderModel(HOLES_CSV) : null,
      holes_csv_mtime:
        HOLES_CSV && fs.existsSync(HOLES_CSV)
          ? new Date(fs.statSync(HOLES_CSV).mtimeMs).toISOString()
          : null,
      /** Same repo file as the shot model; mirrored to alpha-caddie-web/data/ — not loaded in the browser. */
      shots_model_csv: shotsModelCsvMeta(),
      min_year: MIN_YEAR,
      max_rounds_per_player: MAX_ROUNDS_PER_PLAYER,
      players: byDgId.size,
    },
    byDgId: Object.fromEntries(
      [...byDgId.entries()].map(([k, v]) => [
        String(k),
        { dg_id: v.dg_id, player_name: v.player_name, rounds: v.rounds },
      ])
    ),
    holesByPlayerKey,
  };

  fs.writeFileSync(OUT_JSON, JSON.stringify(out), "utf8");
  const st = fs.statSync(OUT_JSON);
  console.log("Wrote", OUT_JSON, `(${(st.size / 1024).toFixed(1)} KB)`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
