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
 * Shot-derived round stats (putts / refreshed GIR & FW): when
 * data/all_shots_2022_2026_round_fairways_gir_putts.csv exists under repo or alpha-caddie-web/data,
 * merges onto PGA rounds by (dg_id or player name) + event date + round (see loadShotsRoundAggMaps).
 * Set GOLF_SKIP_SHOTS_ROUND_AGG_MERGE=1 to skip (Historical Trends putts stay blank — rounds CSV has no putts column).
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
  if (String(process.env.GOLF_USE_ALL_SHOTS_CSV || "").trim() !== "1") {
    return { name: "all_shots_2022_2026.csv", present: false, disabled: true };
  }
  const p = path.join(MODEL_ROOT, "data", "all_shots_2022_2026.csv");
  if (!fs.existsSync(p)) {
    return { name: "all_shots_2022_2026.csv", present: false };
  }
  const st = fs.statSync(p);
  return {
    name: "all_shots_2022_2026.csv",
    present: true,
    mtime: new Date(st.mtimeMs).toISOString(),
    size_bytes: st.size,
  };
}

const ROUNDS_CSV = resolveRoundsCsvPath();
const METADATA_OVERLAY_CSV = resolveMetadataOverlayCsvPath();
const HOLES_CSV = resolveHoleDataCsv();
const PROJECTIONS_JSON = path.join(WEB_ROOT, "projections.json");
const LIVE_IN_PLAY_JSON = path.join(WEB_ROOT, "live-in-play.json");
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
    .replace(/&/g, " and ")
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

/** Accepts either "Last, First" or "First Last" and normalizes to last|first for cross-source joins. */
function playerKeyCanonical(name) {
  const raw = String(name || "").trim().toLowerCase().replace(/\./g, "");
  if (!raw) return "";
  const clean = (x) => String(x || "").replace(/[^a-z0-9 ]+/g, "").replace(/\s+/g, " ").trim();
  if (raw.includes(",")) {
    const parts = raw.split(",");
    const last = clean(parts[0]);
    const first = clean(parts.slice(1).join(" "));
    if (last || first) return `${last}|${first}`;
  }
  const parts = clean(raw).split(" ").filter(Boolean);
  if (!parts.length) return "";
  if (parts.length === 1) return `${parts[0]}|`;
  const last = parts[parts.length - 1];
  const first = parts.slice(0, -1).join(" ");
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

function isoDateMdY(isoMaybe) {
  const t = String(isoMaybe || "").trim();
  const m = t.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (m) {
    const y = parseInt(m[1], 10);
    const mo = parseInt(m[2], 10);
    const d = parseInt(m[3], 10);
    if (Number.isFinite(y) && Number.isFinite(mo) && Number.isFinite(d)) return `${mo}/${d}/${y}`;
  }
  const d = new Date();
  return `${d.getMonth() + 1}/${d.getDate()}/${d.getFullYear()}`;
}

/**
 * Best-effort live round row from live-in-play + projections metadata so Historical Trends
 * includes the latest in-progress round after push:all (even before historical-raw-data catches up).
 */
function loadLiveRoundSnapshotByDg() {
  if (!fs.existsSync(PROJECTIONS_JSON) || !fs.existsSync(LIVE_IN_PLAY_JSON)) return null;
  let proj;
  let live;
  try {
    proj = JSON.parse(fs.readFileSync(PROJECTIONS_JSON, "utf8"));
    live = JSON.parse(fs.readFileSync(LIVE_IN_PLAY_JSON, "utf8"));
  } catch {
    return null;
  }
  const rows = Array.isArray(live?.data) ? live.data : [];
  if (!rows.length) return null;
  const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : {};
  const eventName = String(proj?.event_name || live?.info?.event_name || "").trim();
  if (!eventName) return null;
  const roundCandidates = [
    meta.datagolf_live_current_round,
    meta.display_round,
    live?.info?.current_round,
    live?.current_round,
  ];
  for (const r of rows) {
    roundCandidates.push(r?.round);
  }
  let roundNum = NaN;
  for (const cand of roundCandidates) {
    const rn = Math.round(num(cand, NaN));
    if (Number.isFinite(rn) && rn >= 1 && rn <= 4) {
      roundNum = rn;
      break;
    }
  }
  if (!Number.isFinite(roundNum) || roundNum < 1 || roundNum > 4) return null;
  const courseName = String(proj?.course_used || meta.course_used || "").trim() || eventName;
  const coursePar = num(
    proj?.course_par_18 ?? meta.course_par_18 ?? live?.info?.course_par ?? live?.course_par,
    NaN
  );
  const eventDate = isoDateMdY(live?.info?.last_update || live?.last_update || new Date().toISOString());
  const eventYear = parseInt(String(eventDate).split("/")[2] || "", 10);
  const projRows = Array.isArray(proj?.players) ? proj.players : [];
  const projByDgRound = new Map();
  for (const p of projRows) {
    const pdg = Math.round(num(p?.dg_id, NaN));
    const pr = Math.round(num(p?.round, NaN));
    if (!Number.isFinite(pdg) || !Number.isFinite(pr) || pr < 1 || pr > 4) continue;
    projByDgRound.set(`${pdg}|${pr}`, p);
  }
  /** @type {Map<number, any>} */
  const byDg = new Map();
  for (const r of rows) {
    const dg = Math.round(num(r?.dg_id ?? r?.dgId, NaN));
    if (!Number.isFinite(dg)) continue;
    const today = num(r?.today ?? r?.Today, NaN);
    const currentScore = num(r?.current_score ?? r?.currentScore, NaN);
    // in-play `today` is per-round relative-to-par and is safest for round_score derivation.
    if (!Number.isFinite(today) || !Number.isFinite(coursePar)) continue;
    const roundScore = Math.round((coursePar + today) * 10) / 10;
    const pp = projByDgRound.get(`${dg}|${roundNum}`);
    byDg.set(dg, {
      sortKey: parseUsDateSortKey(eventDate) * 10 + roundNum,
      event_completed: eventDate,
      year: Number.isFinite(eventYear) ? eventYear : new Date().getFullYear(),
      event_name: eventName,
      event_id: "",
      course_name: courseName,
      round_num: roundNum,
      fin_text: "",
      round_score: Number.isFinite(roundScore) ? roundScore : null,
      birdies: Number.isFinite(num(pp?.birdies, NaN)) ? num(pp?.birdies, NaN) : null,
      pars: Number.isFinite(num(pp?.pars, NaN)) ? num(pp?.pars, NaN) : null,
      bogies: Number.isFinite(num(pp?.bogeys, NaN)) ? num(pp?.bogeys, NaN) : null,
      gir: Number.isFinite(num(pp?.gir, NaN)) ? num(pp?.gir, NaN) : null,
      fairways: Number.isFinite(num(pp?.fairways, NaN)) ? num(pp?.fairways, NaN) : null,
      putts: Number.isFinite(num(pp?.putts, NaN)) ? num(pp?.putts, NaN) : null,
      eagles_or_better: null,
      doubles_or_worse: null,
      weather_temp_f: null,
      weather_wind_mph: null,
      weather_humidity: null,
      weather_condition: "",
      sg_putt: Number.isFinite(num(r?.sg_putt, NaN)) ? num(r.sg_putt, NaN) : null,
      sg_app: Number.isFinite(num(r?.sg_app, NaN)) ? num(r.sg_app, NaN) : null,
      sg_arg: Number.isFinite(num(r?.sg_arg, NaN)) ? num(r.sg_arg, NaN) : null,
      sg_ott: Number.isFinite(num(r?.sg_ott, NaN)) ? num(r.sg_ott, NaN) : null,
      sg_t2g: Number.isFinite(num(r?.sg_t2g, NaN)) ? num(r.sg_t2g, NaN) : null,
      sg_total: Number.isFinite(num(r?.sg_total, NaN)) ? num(r.sg_total, NaN) : null,
      current_score: Number.isFinite(currentScore) ? currentScore : null,
      today: Number.isFinite(today) ? today : null,
      _from_live_in_play: true,
    });
  }
  return byDg.size ? byDg : null;
}

function upsertLiveRoundRows(byDgId, liveByDg) {
  if (!liveByDg || !liveByDg.size) return 0;
  let n = 0;
  for (const [dg, liveRec] of liveByDg.entries()) {
    const bucket = byDgId.get(dg);
    if (!bucket || !Array.isArray(bucket.rounds)) continue;
    const wantEvt = normEvt(liveRec.event_name);
    const wantYr = parseInt(String(liveRec.year || ""), 10);
    const wantRnd = parseInt(String(liveRec.round_num || ""), 10);
    let hitIdx = -1;
    for (let i = bucket.rounds.length - 1; i >= 0; i--) {
      const rr = bucket.rounds[i];
      if (parseInt(String(rr.round_num || ""), 10) !== wantRnd) continue;
      if (Number.isFinite(wantYr) && parseInt(String(rr.year || ""), 10) !== wantYr) continue;
      if (normEvt(rr.event_name) !== wantEvt) continue;
      hitIdx = i;
      break;
    }
    if (hitIdx >= 0) bucket.rounds[hitIdx] = { ...bucket.rounds[hitIdx], ...liveRec };
    else bucket.rounds.push(liveRec);
    bucket.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
    if (bucket.rounds.length > MAX_ROUNDS_PER_PLAYER) bucket.rounds = bucket.rounds.slice(-MAX_ROUNDS_PER_PLAYER);
    n++;
  }
  return n;
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

/** (0, 1] = share-of-holes; otherwise treat as integer hole counts (not n<holes rate heuristic). */
function countFromRateOrRaw(raw, holes) {
  const n = num(raw, NaN);
  if (!Number.isFinite(n)) return null;
  if (n > 0 && n <= 1.0001) return Math.min(holes, Math.max(0, Math.round(n * holes)));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

/** 0 / 1 are almost always bad joins or placeholders — drop so charts and props do not treat them as real. */
function stripGirFairwaysPuttsIfGarbage(mf) {
  if (!mf || typeof mf !== "object") return;
  for (const k of ["gir", "fairways", "putts"]) {
    const v = mf[k];
    if (v === 0 || v === 1) delete mf[k];
  }
}

function metricFields(row) {
  const gir = num(row.gir);
  const fa = num(row.driving_acc);
  let girCount = Number.isFinite(gir) ? countFromRateOrRaw(gir, 18) : null;
  let fwCount = Number.isFinite(fa) ? countFromRateOrRaw(fa, 14) : null;
  if (girCount === 0 || girCount === 1) girCount = null;
  if (fwCount === 0 || fwCount === 1) fwCount = null;
  return {
    round_score: num(row.round_score),
    birdies: num(row.birdies),
    pars: num(row.pars),
    bogies: num(row.bogies),
    gir: girCount,
    fairways: fwCount,
    /** Filled from shot CSV aggregate when matched; else null. */
    putts: null,
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
function resolvePgaDgMapCsvPath() {
  if (process.env.PGA_DG_PLAYER_MAP_CSV) return path.resolve(String(process.env.PGA_DG_PLAYER_MAP_CSV).trim());
  const a = path.join(MODEL_ROOT, "data", "pga_datagolf_player_map.csv");
  if (fs.existsSync(a)) return a;
  const b = path.join(WEB_ROOT, "data", "pga_datagolf_player_map.csv");
  return fs.existsSync(b) ? b : a;
}

function resolveShotsRoundAggCsvPath() {
  if (String(process.env.GOLF_SKIP_SHOTS_ROUND_AGG_MERGE || "").trim() === "1") return null;
  if (process.env.SHOTS_ROUND_AGG_CSV) {
    const p = path.resolve(String(process.env.SHOTS_ROUND_AGG_CSV).trim());
    return fs.existsSync(p) ? p : null;
  }
  const web = path.join(WEB_ROOT, "data", "all_shots_2022_2026_round_fairways_gir_putts.csv");
  if (fs.existsSync(web)) return web;
  const model = path.join(MODEL_ROOT, "data", "all_shots_2022_2026_round_fairways_gir_putts.csv");
  return fs.existsSync(model) ? model : null;
}

/**
 * Maps from shot aggregate CSV:
 *   (dg_id|sortKey), (playerKey|sortKey) — when `date` is present
 *   (dg_id|evtNorm|year|round), (playerKey|evtNorm|year|round) — fallback when date was blank but evt_norm+year exist
 */
async function loadShotsRoundAggMaps() {
  const byDgSk = new Map();
  const byPkSk = new Map();
  const byDgEvtYrRnd = new Map();
  const byPkEvtYrRnd = new Map();
  const aggPath = resolveShotsRoundAggCsvPath();
  if (!aggPath || !fs.existsSync(aggPath)) {
    console.log(
      "Shots round aggregate CSV (optional): skipped —",
      process.env.GOLF_SKIP_SHOTS_ROUND_AGG_MERGE === "1"
        ? "GOLF_SKIP_SHOTS_ROUND_AGG_MERGE=1"
        : !aggPath
          ? "no all_shots_2022_2026_round_fairways_gir_putts.csv under data/"
          : path.basename(aggPath),
    );
    return { byDgSk, byPkSk, byDgEvtYrRnd, byPkEvtYrRnd, aggPath: null };
  }

  const pidToDg = new Map();
  const mapPath = resolvePgaDgMapCsvPath();
  if (fs.existsSync(mapPath)) {
    const parser = createReadStream(mapPath).pipe(
      parse({
        columns: true,
        relax_quotes: true,
        relax_column_count: true,
        skip_records_with_error: true,
      })
    );
    for await (const row of parser) {
      const pid = String(row.pga_player_id ?? "").trim();
      const dg = Math.round(num(row.dg_id));
      if (pid && Number.isFinite(dg)) pidToDg.set(pid, dg);
    }
  }

  const parser = createReadStream(aggPath).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );
  for await (const row of parser) {
    const date = String(row.date ?? "").trim();
    const rnd = parseInt(String(row.round ?? "").trim(), 10) || 1;
    const fairways = num(row.fairways);
    const gir = num(row.gir);
    const putts = num(row.putts);
    const val = {
      fairways: Number.isFinite(fairways) ? Math.round(fairways) : null,
      gir: Number.isFinite(gir) ? Math.round(gir) : null,
      putts: Number.isFinite(putts) ? Math.round(putts) : null,
    };
    const gl = String(row.golfer ?? "").trim();
    const evtNormAgg = String(row.evt_norm ?? "")
      .trim()
      .toLowerCase()
      .replace(/\s+/g, " ");
    const yrAgg = parseInt(String(row.year ?? "").trim(), 10);
    const evtRndKey =
      evtNormAgg && Number.isFinite(yrAgg) && yrAgg > 1900 ? `${evtNormAgg}|${yrAgg}|${rnd}` : "";

    if (date) {
      const sk = parseUsDateSortKey(date) * 10 + rnd;
      if (/^\d+$/.test(gl)) {
        const dg = pidToDg.get(gl);
        if (Number.isFinite(dg)) byDgSk.set(`${dg}|${sk}`, val);
      } else if (gl) {
        const pk = playerKeyCanonical(gl);
        if (pk) byPkSk.set(`${pk}|||${sk}`, val);
      }
    }
    if (evtRndKey) {
      if (/^\d+$/.test(gl)) {
        const dg = pidToDg.get(gl);
        if (Number.isFinite(dg)) byDgEvtYrRnd.set(`${dg}|${evtRndKey}`, val);
      } else if (gl) {
        const pk = playerKeyCanonical(gl);
        if (pk) byPkEvtYrRnd.set(`${pk}|${evtRndKey}`, val);
      }
    }
  }
  console.log(
    "Shots round aggregate:",
    byDgSk.size,
    "dg_id|date keys,",
    byPkSk.size,
    "name|date keys,",
    byDgEvtYrRnd.size,
    "dg_id|event|year|round keys,",
    byPkEvtYrRnd.size,
    "name|event|year|round keys from",
    path.basename(aggPath)
  );
  return { byDgSk, byPkSk, byDgEvtYrRnd, byPkEvtYrRnd, aggPath };
}

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

async function streamRounds(allowedDgIds, pgaMetaOverlay, shotsAgg) {
  const byDgId = new Map();
  const byDgSk = shotsAgg?.byDgSk || new Map();
  const byPkSk = shotsAgg?.byPkSk || new Map();
  const byDgEvtYrRnd = shotsAgg?.byDgEvtYrRnd || new Map();
  const byPkEvtYrRnd = shotsAgg?.byPkEvtYrRnd || new Map();

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
    const evtNormHist = normEvt(eventName);
    const yrHist = parseInt(String(row.year || "").trim(), 10);
    const rnHist = parseInt(String(row.round_num || "1").trim(), 10) || 1;
    const evtRndHistKey =
      evtNormHist && Number.isFinite(yrHist) && yrHist > 1900
        ? `${evtNormHist}|${yrHist}|${rnHist}`
        : "";
    const courseRaw = String(
      row.course_name ||
        row.Course_Name ||
        row.course ||
        row.Course ||
        row.venue ||
        ""
    ).trim();
    const pkHist = playerKeyCanonical(String(row.player_name || ""));
    const shotOv =
      (Number.isFinite(dg) ? byDgSk.get(`${dg}|${sortKey}`) : undefined) ??
      byPkSk.get(`${pkHist}|||${sortKey}`) ??
      (evtRndHistKey && Number.isFinite(dg) ? byDgEvtYrRnd.get(`${dg}|${evtRndHistKey}`) : undefined) ??
      (evtRndHistKey ? byPkEvtYrRnd.get(`${pkHist}|${evtRndHistKey}`) : undefined) ??
      null;
    const mf = metricFields(row);
    if (shotOv) {
      if (shotOv.putts != null) mf.putts = shotOv.putts;
      if (shotOv.gir != null) mf.gir = shotOv.gir;
      if (shotOv.fairways != null) mf.fairways = shotOv.fairways;
    }
    stripGirFairwaysPuttsIfGarbage(mf);
    const rec = {
      sortKey,
      event_completed: String(row.event_completed || ""),
      year: yr,
      event_name: eventName,
      event_id: String(row.event_id || ""),
      course_name: courseRaw || eventName,
      round_num: parseInt(row.round_num, 10) || 1,
      fin_text: String(row.fin_text || ""),
      ...mf,
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

  if (!fs.existsSync(ROUNDS_CSV)) {
    console.error("[build-player-history] Missing rounds CSV — run fetch:dg / update:rounds merge first:", ROUNDS_CSV);
  } else {
    try {
      const st = fs.statSync(ROUNDS_CSV);
      console.log("[build-player-history] Rounds CSV bytes:", st.size, "| exists OK");
    } catch (_) {
      /* ignore */
    }
  }

  const pgaMetaOverlay = METADATA_OVERLAY_CSV ? await loadPgaMetaOverlayFromCsv(METADATA_OVERLAY_CSV) : new Map();
  const shotsAgg = await loadShotsRoundAggMaps();
  const { byDgId, allowedTriples } = await streamRounds(allowed, pgaMetaOverlay, shotsAgg);
  const liveRoundByDg = loadLiveRoundSnapshotByDg();
  const liveMergedPlayers = upsertLiveRoundRows(byDgId, liveRoundByDg);
  console.log("Players with rounds:", byDgId.size);
  if (liveMergedPlayers > 0) {
    console.log("[build-player-history] live round snapshot merged for", liveMergedPlayers, "player(s) from live-in-play.json");
  }
  if (allowed.size > 0 && byDgId.size === 0 && fs.existsSync(ROUNDS_CSV)) {
    console.warn(
      "[build-player-history] 0 players matched: projections have",
      allowed.size,
      "dg_ids but CSV rows did not join (tour must be pga|liv, dg_id must match, MIN_YEAR filter, or CSV missing recent seasons — widen GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS / run full merge).",
    );
  }

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
      shots_round_agg_csv: shotsAgg.aggPath
        ? {
            name: path.basename(shotsAgg.aggPath),
            relpath: relUnderModel(shotsAgg.aggPath),
            mtime: fs.existsSync(shotsAgg.aggPath)
              ? new Date(fs.statSync(shotsAgg.aggPath).mtimeMs).toISOString()
              : null,
          }
        : null,
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
