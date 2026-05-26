#!/usr/bin/env node
/**
 * Builds player_round_history.json for the static web app from:
 *   - historical_rounds_all.csv (PGA + LIV rows; refresh with npm run update:rounds / fetch:dg)
 *   - optional hole_data.csv (hole-by-hole rows; joined by player + event + round)
 *
 * Allowed dg_ids: union of projections.json, preds/in-play `data`, and field_updates field list
 * so Historical Trends “field by course & date” includes the **full tournament field** (not post-cut only).
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
 * data/all_shots_2022_2026_round_fairways_gir_putts.csv exists (from pgatouR `pga_shot_details` via
 * npm run refresh:shots + build_shots_round_aggregate.py), merges onto rounds by (dg_id or player name)
 * + event date + round (see loadShotsRoundAggMaps). CSV column `putts` from DataGolf (when present) is used first.
 * Set GOLF_SKIP_SHOTS_ROUND_AGG_MERGE=1 to skip that merge.
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";
import { execFileSync } from "child_process";
import { parse } from "csv-parse";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import {
  resolveLiveRoundActualsByDg,
  sanitizeLiveCountingFields,
  countingFromInPlayRow,
} from "./dg-live-tournament-stats.mjs";
import { normCourseNameKey, courseShardFileName, formatCourseLabelForDisplay } from "./course-name-key.mjs";
import {
  historyRoundChartUtcIsoDay,
  roundEventCompletedMdYFromEventEnd,
  parseEventCompletedChronoBase,
} from "./history-round-dates.mjs";

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
const PGATOUR_EVENT_ROUNDS_JSON = path.join(WEB_ROOT, "data", "pgatour_event_rounds.json");
const OUT_JSON = path.join(WEB_ROOT, "player_round_history.json");
const SHARD_DIR = path.join(WEB_ROOT, "player-history", "by-dg");
const SHARD_MANIFEST_JSON = path.join(WEB_ROOT, "player-history", "manifest.json");
const COURSE_SHARD_DIR = path.join(WEB_ROOT, "player-history", "by-course");
const COURSES_MANIFEST_JSON = path.join(WEB_ROOT, "player-history", "courses-manifest.json");

function fmtBytes(n) {
  if (!Number.isFinite(n) || n < 0) return "—";
  if (n >= 1e9) return `${(n / 1e9).toFixed(2)} GB`;
  if (n >= 1e6) return `${(n / 1e6).toFixed(1)} MB`;
  if (n >= 1e3) return `${(n / 1e3).toFixed(0)} KB`;
  return `${Math.round(n)} B`;
}

function playerKeyFromName(full) {
  const s = String(full || "").trim();
  const i = s.indexOf(",");
  if (i > 0) {
    const last = s.slice(0, i).trim().toLowerCase();
    const first = (s.slice(i + 1).trim().split(/\s+/)[0] || "").toLowerCase();
    return `${last}|${first}`;
  }
  const tok = s.toLowerCase().split(/\s+/).filter(Boolean);
  if (tok.length >= 2) return `${tok[tok.length - 1]}|${tok[0]}`;
  return s.toLowerCase().replace(/\s+/g, "");
}

function writeJsonAtomic(outPath, payload) {
  const tmpPath = `${outPath}.tmp`;
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(tmpPath, JSON.stringify(payload), "utf8");
  fs.renameSync(tmpPath, outPath);
}

function chartUtcIsoDayFromHistoryRow(r) {
  return historyRoundChartUtcIsoDay(r);
}

/** Pre-aggregated rounds at each venue for fast Historical Trends “field by course & date”. */
function writeCourseHistoryShards(out) {
  fs.mkdirSync(COURSE_SHARD_DIR, { recursive: true });
  const byCourse = new Map();
  for (const [dgId, bucket] of Object.entries(out.byDgId || {})) {
    const dg = Math.round(Number(dgId));
    if (!Number.isFinite(dg) || !bucket?.rounds) continue;
    const playerName = String(bucket.player_name || "").trim();
    for (const r of bucket.rounds) {
      if (eventCompletedIsFutureMdY(r.event_completed) || historyRoundChartDateIsFuture(r)) continue;
      const rs = Number(r.round_score);
      if (!Number.isFinite(rs) || rs <= 0) continue;
      const ck = normCourseNameKey(r.course_name);
      if (!ck) continue;
      let b = byCourse.get(ck);
      if (!b) {
        b = { dateSet: new Set(), entries: [] };
        byCourse.set(ck, b);
      }
      b.entries.push({ dg_id: dg, player_name: playerName, row: r });
      const iso = chartUtcIsoDayFromHistoryRow(r);
      if (iso) b.dateSet.add(iso);
    }
  }
  const keep = new Set();
  const courses = [];
  for (const [courseKey, b] of byCourse) {
    const file = courseShardFileName(courseKey);
    keep.add(file);
    const days = [...b.dateSet].sort((a, c) => c.localeCompare(a));
    writeJsonAtomic(path.join(COURSE_SHARD_DIR, file), {
      course_key: courseKey,
      days,
      entries: b.entries,
    });
    courses.push({ course_key: courseKey, file, days: days.length, entries: b.entries.length });
  }
  for (const entry of fs.readdirSync(COURSE_SHARD_DIR, { withFileTypes: true })) {
    if (entry.isFile() && entry.name.endsWith(".json") && !keep.has(entry.name)) {
      fs.unlinkSync(path.join(COURSE_SHARD_DIR, entry.name));
    }
  }
  courses.sort((a, b) => a.course_key.localeCompare(b.course_key));
  writeJsonAtomic(COURSES_MANIFEST_JSON, { meta: { updated_at: out.meta?.updated_at || new Date().toISOString() }, courses });
  console.log("Wrote course history shards:", courses.length, "->", path.relative(WEB_ROOT, COURSE_SHARD_DIR));
}

function writePlayerHistoryShards(out) {
  fs.mkdirSync(SHARD_DIR, { recursive: true });
  const keep = new Set();
  const players = [];
  for (const [dgId, bucket] of Object.entries(out.byDgId || {})) {
    const id = String(dgId);
    keep.add(`${id}.json`);
    const pkey = playerKeyFromName(bucket?.player_name || "");
    const holesByPlayerKey = {};
    if (pkey && out.holesByPlayerKey?.[pkey]) holesByPlayerKey[pkey] = out.holesByPlayerKey[pkey];
    writeJsonAtomic(path.join(SHARD_DIR, `${id}.json`), {
      dg_id: bucket.dg_id,
      player_name: bucket.player_name,
      rounds: bucket.rounds,
    });
    players.push({
      dg_id: Number(dgId),
      player_name: bucket?.player_name || "",
      rounds: Array.isArray(bucket?.rounds) ? bucket.rounds.length : 0,
    });
  }
  for (const entry of fs.readdirSync(SHARD_DIR, { withFileTypes: true })) {
    if (entry.isFile() && entry.name.endsWith(".json") && !keep.has(entry.name)) {
      fs.unlinkSync(path.join(SHARD_DIR, entry.name));
    }
  }
  players.sort((a, b) => String(a.player_name).localeCompare(String(b.player_name)));
  writeJsonAtomic(SHARD_MANIFEST_JSON, { meta: out.meta, players });
  console.log("Wrote player history shards:", players.length, "->", path.relative(WEB_ROOT, SHARD_DIR));
}

/** Windows only: free bytes on the drive hosting `filePath`, or null if unknown. */
function tryFreeBytesOnDriveOf(filePath) {
  if (process.platform !== "win32") return null;
  const abs = path.resolve(filePath);
  const m = abs.match(/^([A-Za-z]):/);
  if (!m) return null;
  const letter = m[1].toUpperCase();
  try {
    const out = execFileSync(
      "powershell",
      ["-NoProfile", "-Command", `(Get-PSDrive -Name '${letter}').Free`],
      { encoding: "utf8", maxBuffer: 64 },
    );
    const n = parseInt(String(out).trim(), 10);
    return Number.isFinite(n) ? n : null;
  } catch {
    return null;
  }
}

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
  const t = String(s).trim();
  const iso = t.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) {
    const y = parseInt(iso[1], 10);
    const mo = parseInt(iso[2], 10);
    const d = parseInt(iso[3], 10);
    if (Number.isFinite(y) && Number.isFinite(mo) && Number.isFinite(d)) return y * 10000 + mo * 100 + d;
  }
  const p = t.split("/");
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

/** `date_start` is first competitive round (ISO YYYY-MM-DD); round 1 maps to that calendar day. */
function eventCompletedMdYForRound(dateStartIso, roundNum) {
  if (!dateStartIso || roundNum < 1) return "";
  const m = String(dateStartIso).match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return "";
  const t = Date.UTC(parseInt(m[1], 10), parseInt(m[2], 10) - 1, parseInt(m[3], 10)) + (roundNum - 1) * 86400000;
  const d = new Date(t);
  return `${d.getUTCMonth() + 1}/${d.getUTCDate()}/${d.getUTCFullYear()}`;
}

function dateOnlyUtcMsFromIso(dateStartIso) {
  const m = String(dateStartIso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return NaN;
  const y = parseInt(m[1], 10);
  const mo = parseInt(m[2], 10);
  const d = parseInt(m[3], 10);
  if (!Number.isFinite(y) || !Number.isFinite(mo) || !Number.isFinite(d)) return NaN;
  return Date.UTC(y, mo - 1, d);
}

function todayDateOnlyUtcMs() {
  const d = new Date();
  return Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate());
}

function dateStartIsFuture(dateStartIso) {
  const start = dateOnlyUtcMsFromIso(dateStartIso);
  return Number.isFinite(start) && start > todayDateOnlyUtcMs();
}

function eventCompletedIsFutureMdY(s) {
  const m = String(s || "").trim().match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  if (!m) return false;
  const t = Date.UTC(Number(m[3]), Number(m[1]) - 1, Number(m[2]));
  return Number.isFinite(t) && t > todayDateOnlyUtcMs();
}

/** Chart x-axis date from sortKey + round_num (matches app.js propsTrendChartDateFromRow). */
function historyRoundChartDateUtcMs(row) {
  if (!row || typeof row !== "object") return NaN;
  const sk = Math.round(num(row.sortKey, NaN));
  let y = NaN;
  let mo = NaN;
  let d = NaN;
  let rnd = Math.round(num(row.round_num, NaN));
  if (!Number.isFinite(rnd) || rnd < 1) rnd = 1;
  if (Number.isFinite(sk) && sk > 9_999_999) {
    const base = Math.floor(sk / 10);
    const rnTail = sk % 10;
    if (Number.isFinite(rnTail) && rnTail >= 1 && rnTail <= 9) rnd = rnTail;
    if (base >= 19_000_000 && base <= 2_100_1231) {
      y = Math.floor(base / 10000);
      mo = Math.floor((base % 10000) / 100);
      d = base % 100;
    }
  }
  if (!Number.isFinite(y)) {
    const ec = String(row.event_completed || "").trim();
    const mdy = ec.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
    if (!mdy) return NaN;
    mo = Number(mdy[1]);
    d = Number(mdy[2]);
    y = Number(mdy[3]);
  }
  const dayBump = 0;
  return Date.UTC(y, mo - 1, d) + dayBump * 86400000;
}

function buildExportLiveRoundCap() {
  if (!fs.existsSync(LIVE_IN_PLAY_JSON)) return NaN;
  try {
    const live = JSON.parse(fs.readFileSync(LIVE_IN_PLAY_JSON, "utf8"));
    const fu = live?.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
    const info = live?.info && typeof live.info === "object" ? live.info : {};
    const cands = [fu.current_round, info.current_round, live?.current_round];
    for (const c of cands) {
      const r = Math.round(num(c, NaN));
      if (Number.isFinite(r) && r >= 1 && r <= 4) return r;
    }
  } catch {
    /* ignore */
  }
  if (fs.existsSync(PROJECTIONS_JSON)) {
    try {
      const proj = JSON.parse(fs.readFileSync(PROJECTIONS_JSON, "utf8"));
      const r = Math.round(num(proj?.display_round ?? proj?.datagolf_field_current_round, NaN));
      if (Number.isFinite(r) && r >= 1 && r <= 4) return r;
    } catch {
      /* ignore */
    }
  }
  return NaN;
}

function historyRoundChartDateIsFuture(row) {
  const rnd = Math.round(num(row?.round_num, NaN));
  const cap = buildExportLiveRoundCap();
  const rs = num(row?.round_score, NaN);
  if (
    row?._from_pgatour &&
    Number.isFinite(rs) &&
    rs > 0 &&
    Number.isFinite(rnd) &&
    Number.isFinite(cap) &&
    rnd <= cap &&
    !eventCompletedIsFutureMdY(row?.event_completed)
  ) {
    return false;
  }
  if (Number.isFinite(rnd) && Number.isFinite(cap) && rnd <= cap && row?._from_pgatour) {
    const ms = historyRoundChartDateUtcMs(row);
    if (Number.isFinite(ms) && ms <= todayDateOnlyUtcMs()) return false;
  }
  const ms = historyRoundChartDateUtcMs(row);
  if (!Number.isFinite(ms)) return eventCompletedIsFutureMdY(row?.event_completed);
  return ms > todayDateOnlyUtcMs();
}

function liveInPlayGrossForRound(inPlayRow, rnd) {
  if (!inPlayRow) return NaN;
  const r = Math.round(num(rnd, NaN));
  if (!Number.isFinite(r) || r < 1 || r > 4) return NaN;
  return num(inPlayRow[`R${r}`] ?? inPlayRow[`r${r}`], NaN);
}

function liveHistoryEventsLikelySame(a, b) {
  const fa = foldComparableTitle(a);
  const fb = foldComparableTitle(b);
  if (!fa || !fb) return false;
  if (fa === fb || fa.includes(fb) || fb.includes(fa)) return true;
  return eventsLikelySame(a, b);
}

function liveSnapshotEventsCompatible({ projEvent, fieldEvent, inPlayEvent }) {
  const target = String(projEvent || fieldEvent || "").trim();
  if (target && fieldEvent && !liveHistoryEventsLikelySame(target, fieldEvent)) return false;
  if (target && inPlayEvent && !liveHistoryEventsLikelySame(target, inPlayEvent)) return false;
  return true;
}

function normalizeLiveRoundList(liveByDg) {
  if (!liveByDg) return [];
  if (Array.isArray(liveByDg)) return liveByDg;
  if (liveByDg instanceof Map) {
    return [...liveByDg.entries()].map(([dg, rec]) => ({
      ...rec,
      dg_id: Number.isFinite(num(rec?.dg_id, NaN)) ? num(rec.dg_id, NaN) : dg,
    }));
  }
  return [];
}

/**
 * Live-week rows for Historical Trends export: preds/live-tournament-stats (per-round) plus
 * preds/in-play `R1`–`R4` gross from `live_round_actuals_by_dg` on live-in-play.json.
 * Written into player_round_history.json / shards during npm run push:all → update:rounds.
 */
function buildLiveHistoryRowsFromBundle() {
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
  const fu = live?.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const projEvent = String(proj?.event_name || "").trim();
  const fieldEvent = String(fu.event_name || "").trim();
  const inPlayEvent = String(live?.info?.event_name || live?.event_name || "").trim();
  const eventName = String(projEvent || fieldEvent || inPlayEvent).trim();
  if (!eventName) return null;

  const dateStartIso = String(fu.date_start || live?.info?.date_start || "").trim();
  if (!liveSnapshotEventsCompatible({ projEvent, fieldEvent, inPlayEvent })) {
    console.warn(
      `[build-player-history] Skipping live history merge: event mismatch (projections="${projEvent || "?"}", field_updates="${fieldEvent || "?"}", in_play="${inPlayEvent || "?"}")`,
    );
    return null;
  }
  if (dateStartIsFuture(dateStartIso)) {
    console.warn(
      `[build-player-history] Skipping live history merge for future event ${eventName} (date_start=${dateStartIso}).`,
    );
    return null;
  }

  let courseName =
    String(proj?.course_used || meta.course_used || fu.course_name || "").trim() || eventName;
  courseName = formatCourseLabelForDisplay(courseName) || courseName;
  const roundPar = num(
    proj?.course_par_18 ??
      meta.course_par_18 ??
      fu.course_par ??
      live?.info?.course_par ??
      live?.course_par,
    72,
  );
  const eventIdStr = fu.event_id != null && fu.event_id !== "" ? String(fu.event_id) : "";
  const fairwayHoles = Math.round(
    num(
      proj?.projection_course_basis?.fairway_holes_modeled ??
        meta.projection_course_basis?.fairway_holes_modeled,
      NaN,
    ),
  );
  const actualsByDg = resolveLiveRoundActualsByDg(live, {
    roundPar: Number.isFinite(roundPar) ? roundPar : 72,
    fairwayHoles: Number.isFinite(fairwayHoles) && fairwayHoles >= 1 ? fairwayHoles : 14,
  });
  if (!actualsByDg || typeof actualsByDg !== "object" || !Object.keys(actualsByDg).length) {
    return loadLiveRoundSnapshotByDg();
  }

  const nameByDg = new Map();
  for (const r of rows) {
    const dg = Math.round(num(r?.dg_id ?? r?.dgId, NaN));
    if (!Number.isFinite(dg)) continue;
    const nm = String(r?.player_name ?? r?.playerName ?? "").trim();
    if (nm) nameByDg.set(dg, nm);
  }

  /** @type {any[]} */
  const out = [];
  for (const [dgKey, perRound] of Object.entries(actualsByDg)) {
    const dg = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    const displayName = nameByDg.get(dg) || "";

    for (const [rndKey, act] of Object.entries(perRound)) {
      if (!act || typeof act !== "object") continue;
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      let roundScore = num(act.round_score, NaN);
      const ipRow = rows.find((r) => Math.round(num(r?.dg_id ?? r?.dgId, NaN)) === dg);
      const playerR = Math.round(num(ipRow?.round ?? ipRow?.Round, NaN));
      if (ipRow) {
        const g = liveInPlayGrossForRound(ipRow, rnd);
        if (Number.isFinite(g)) roundScore = g;
      }
      if (!Number.isFinite(roundScore) || roundScore <= 0) continue;

      const eventDate = dateStartIso ? eventCompletedMdYForRound(dateStartIso, rnd) : "";
      if (!eventDate) continue;
      if (eventCompletedIsFutureMdY(eventDate) || historyRoundChartDateIsFuture({ event_completed: eventDate, round_num: rnd, event_name: eventName }))
        continue;

      const eventYear = parseInt(String(eventDate).split("/")[2] || "", 10);
      const today = ipRow && rnd === playerR ? num(ipRow?.today, NaN) : NaN;
      const currentScore = ipRow && rnd === playerR ? num(ipRow?.current_score, NaN) : NaN;

      let birdies = Number.isFinite(num(act.birdies, NaN)) ? Math.round(num(act.birdies, NaN)) : null;
      let pars = Number.isFinite(num(act.pars, NaN)) ? Math.round(num(act.pars, NaN)) : null;
      let bogeys = Number.isFinite(num(act.bogeys, NaN)) ? Math.round(num(act.bogeys, NaN)) : null;
      if (ipRow && playerR === rnd) {
        const thru = Math.round(num(act.thru ?? ipRow.thru, NaN));
        const ip = countingFromInPlayRow(ipRow, thru);
        if (Number.isFinite(ip.birdies)) birdies = Math.round(ip.birdies);
        if (Number.isFinite(ip.pars)) pars = Math.round(ip.pars);
        if (Number.isFinite(ip.bogeys)) bogeys = Math.round(ip.bogeys);
      }
      const girRaw = num(act.gir, NaN);
      let girVal = null;
      if (Number.isFinite(girRaw)) girVal = Math.round(girRaw > 0 && girRaw <= 1.0001 ? girRaw * 18 : girRaw);
      const fwVal = Number.isFinite(num(act.fairways, NaN)) ? Math.round(num(act.fairways, NaN)) : null;
      const puttsVal = Number.isFinite(num(act.putts, NaN)) ? Math.round(num(act.putts, NaN)) : null;

      out.push(
        sanitizeLiveCountingFields({
        dg_id: dg,
        player_name: displayName,
        sortKey: parseUsDateSortKey(eventDate) * 10 + rnd,
        event_completed: eventDate,
        year: Number.isFinite(eventYear) ? eventYear : new Date().getFullYear(),
        event_name: eventName,
        event_id: eventIdStr,
        course_name: courseName,
        round_num: rnd,
        fin_text: "",
        round_score: Math.round(roundScore * 10) / 10,
        birdies,
        pars,
        bogies: bogeys,
        gir: girVal,
        fairways: fwVal,
        putts: puttsVal,
        eagles_or_better: null,
        doubles_or_worse: null,
        weather_temp_f: null,
        weather_wind_mph: null,
        weather_humidity: null,
        weather_condition: "",
        sg_putt: Number.isFinite(num(act.sg_putt, NaN)) ? num(act.sg_putt, NaN) : null,
        sg_app: Number.isFinite(num(act.sg_app, NaN)) ? num(act.sg_app, NaN) : null,
        sg_arg: Number.isFinite(num(act.sg_arg, NaN)) ? num(act.sg_arg, NaN) : null,
        sg_ott: Number.isFinite(num(act.sg_ott, NaN)) ? num(act.sg_ott, NaN) : null,
        sg_t2g: Number.isFinite(num(act.sg_t2g, NaN)) ? num(act.sg_t2g, NaN) : null,
        sg_total: Number.isFinite(num(act.sg_total, NaN)) ? num(act.sg_total, NaN) : null,
        current_score: Number.isFinite(currentScore) ? currentScore : null,
        today: Number.isFinite(today) ? today : null,
        _from_live_tournament_stats: true,
        }),
      );
    }
  }

  return out.length ? out : loadLiveRoundSnapshotByDg();
}

function resolveEventDateStartIsoForPgatour() {
  const candidates = [
    LIVE_IN_PLAY_JSON,
    path.join(WEB_ROOT, "..", "website", "public", "data", "live-in-play.json"),
  ];
  for (const lip of candidates) {
    if (!fs.existsSync(lip)) continue;
    try {
      const live = JSON.parse(fs.readFileSync(lip, "utf8"));
      const fu = live?.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
      const iso = String(fu.date_start || live?.info?.date_start || "").trim();
      if (/^\d{4}-\d{2}-\d{2}/.test(iso)) return iso;
    } catch {
      /* next */
    }
  }
  return "";
}

/** pgatouR schedule anchor can be wrong (e.g. Jul 1 fallback); align to DataGolf field date_start. */
function normalizePgatourEventRoundDates(rows) {
  const dateStartIso = resolveEventDateStartIsoForPgatour();
  if (!dateStartIso || !Array.isArray(rows) || !rows.length) return rows;
  return rows.map((r) => {
    const rnd = Math.round(num(r?.round_num, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return r;
    const eventDate = eventCompletedMdYForRound(dateStartIso, rnd);
    if (!eventDate) return r;
    const eventYear = parseInt(String(eventDate).split("/")[2] || "", 10);
    return {
      ...r,
      event_completed: eventDate,
      sortKey: parseUsDateSortKey(eventDate) * 10 + rnd,
      year: Number.isFinite(eventYear) ? eventYear : r.year,
    };
  });
}

/** Current-event rows from pgatouR (npm run refresh:pgatour-event / push:all). */
function loadPgatourEventRoundRows() {
  if (!fs.existsSync(PGATOUR_EVENT_ROUNDS_JSON)) return null;
  try {
    const raw = JSON.parse(fs.readFileSync(PGATOUR_EVENT_ROUNDS_JSON, "utf8"));
    let list = Array.isArray(raw?.rounds) ? raw.rounds : [];
    if (!list.length) return null;
    const projEvent = fs.existsSync(PROJECTIONS_JSON)
      ? String(JSON.parse(fs.readFileSync(PROJECTIONS_JSON, "utf8"))?.event_name || "").trim()
      : "";
    const metaEvent = String(raw?.meta?.event_name || "").trim();
    const pe = foldComparableTitle(projEvent);
    const me = foldComparableTitle(metaEvent);
    if (projEvent && metaEvent && pe !== me && !eventsLikelySame(projEvent, metaEvent)) {
      console.warn(
        `[build-player-history] pgatour_event_rounds.json event "${metaEvent}" != projections "${projEvent}" — skip`,
      );
      return null;
    }
    list = list.filter((r) => r && typeof r === "object" && r._from_pgatour);
    list = normalizePgatourEventRoundDates(list);
    list = list.map((r) => {
      const cn = String(r.course_name || "").trim();
      if (!cn) return r;
      const pretty = formatCourseLabelForDisplay(cn);
      return pretty && pretty !== cn ? { ...r, course_name: pretty } : r;
    });
    return list;
  } catch (e) {
    console.warn("[build-player-history] pgatour_event_rounds.json:", e?.message || e);
    return null;
  }
}

/**
 * Fallback when live_tournament_stats payloads are empty: preds/in-play `R1`–`R4` gross only.
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
  const fu = live?.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const projEvent = String(proj?.event_name || "").trim();
  const fieldEvent = String(fu.event_name || "").trim();
  const inPlayEvent = String(live?.info?.event_name || live?.event_name || "").trim();
  const eventName = String(projEvent || fieldEvent || inPlayEvent).trim();
  if (!eventName) return null;

  const dateStartIso = String(fu.date_start || live?.info?.date_start || "").trim();
  if (!liveSnapshotEventsCompatible({ projEvent, fieldEvent, inPlayEvent })) {
    console.warn(
      `[build-player-history] Skipping live round snapshot: event mismatch (projections="${projEvent || "?"}", field_updates="${fieldEvent || "?"}", in_play="${inPlayEvent || "?"}")`,
    );
    return null;
  }
  if (dateStartIsFuture(dateStartIso)) {
    console.warn(
      `[build-player-history] Skipping live round snapshot for future event ${eventName} (date_start=${dateStartIso}); no completed rounds should be merged yet.`,
    );
    return null;
  }
  let courseName =
    String(proj?.course_used || meta.course_used || fu.course_name || "").trim() || eventName;
  courseName = formatCourseLabelForDisplay(courseName) || courseName;
  const coursePar = num(
    proj?.course_par_18 ??
      meta.course_par_18 ??
      fu.course_par ??
      live?.info?.course_par ??
      live?.course_par,
    NaN
  );
  const eventIdStr = fu.event_id != null && fu.event_id !== "" ? String(fu.event_id) : "";

  const roundCandidates = [
    meta.datagolf_live_current_round,
    meta.display_round,
    fu.current_round,
    live?.info?.current_round,
    live?.current_round,
  ];
  for (const r of rows) {
    roundCandidates.push(r?.round);
  }
  let fallbackRoundNum = NaN;
  for (const cand of roundCandidates) {
    const rn = Math.round(num(cand, NaN));
    if (!Number.isFinite(rn) || rn < 1 || rn > 4) continue;
    fallbackRoundNum = Number.isFinite(fallbackRoundNum) ? Math.max(fallbackRoundNum, rn) : rn;
  }

  /** @type {any[]} */
  const out = [];

  for (const r of rows) {
    const dg = Math.round(num(r?.dg_id ?? r?.dgId, NaN));
    if (!Number.isFinite(dg)) continue;

    const displayName = String(r?.player_name ?? r?.playerName ?? "").trim();

    const playerRound = Math.round(num(r?.round, NaN));
    const pr = Number.isFinite(playerRound) && playerRound >= 1 && playerRound <= 4 ? playerRound : fallbackRoundNum;
    if (!Number.isFinite(pr) || pr < 1 || pr > 4) continue;

    const today = num(r?.today ?? r?.Today, NaN);
    const currentScore = num(r?.current_score ?? r?.currentScore, NaN);

    if (dateStartIso) {
      for (let rnd = 1; rnd <= 4; rnd++) {
        const eventDate = eventCompletedMdYForRound(dateStartIso, rnd);
        if (!eventDate) continue;
        const gross = liveInPlayGrossForRound(r, rnd);
        if (eventCompletedIsFutureMdY(eventDate) || historyRoundChartDateIsFuture({ event_completed: eventDate, round_num: rnd }))
          continue;
        if (!Number.isFinite(gross)) continue;
        const roundScore = Math.round(gross * 10) / 10;

        const eventYear = parseInt(String(eventDate).split("/")[2] || "", 10);
        out.push({
          dg_id: dg,
          player_name: displayName,
          sortKey: parseUsDateSortKey(eventDate) * 10 + rnd,
          event_completed: eventDate,
          year: Number.isFinite(eventYear) ? eventYear : new Date().getFullYear(),
          event_name: eventName,
          event_id: eventIdStr,
          course_name: courseName,
          round_num: rnd,
          fin_text: "",
          round_score: roundScore,
          birdies: null,
          pars: null,
          bogies: null,
          gir: null,
          fairways: null,
          putts: null,
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
          current_score: rnd === pr && Number.isFinite(currentScore) ? currentScore : null,
          today: rnd === pr && Number.isFinite(today) ? today : null,
          _from_live_in_play: true,
        });
      }
    } else {
      const roundNum = pr;
      const gross = liveInPlayGrossForRound(r, roundNum);
      if (!Number.isFinite(gross)) continue;
      const roundScore = Math.round(gross * 10) / 10;
      const eventDate = isoDateMdY(live?.info?.last_update || live?.last_update || new Date().toISOString());
      const eventYear = parseInt(String(eventDate).split("/")[2] || "", 10);
      out.push({
        dg_id: dg,
        player_name: displayName,
        sortKey: parseUsDateSortKey(eventDate) * 10 + roundNum,
        event_completed: eventDate,
        year: Number.isFinite(eventYear) ? eventYear : new Date().getFullYear(),
        event_name: eventName,
        event_id: eventIdStr,
        course_name: courseName,
        round_num: roundNum,
        fin_text: "",
        round_score: Number.isFinite(roundScore) ? roundScore : null,
        birdies: null,
        pars: null,
        bogies: null,
        gir: null,
        fairways: null,
        putts: null,
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
  }

  return out.length ? out : null;
}

const LIVE_HISTORY_COUNTING_KEYS = ["birdies", "pars", "bogies", "bogeys", "gir", "fairways", "putts"];

function historyRowHasStoredCountingStat(row, key) {
  if (!row || typeof row !== "object") return false;
  const v = row[key];
  if (v == null || v === "") return false;
  const n = Number(v);
  if (!Number.isFinite(n)) return false;
  if ((key === "gir" || key === "fairways" || key === "putts") && (n === 0 || n === 1)) return false;
  if (
    (key === "birdies" || key === "pars" || key === "bogies" || key === "bogeys") &&
    n === 0 &&
    row._from_live_tournament_stats &&
    !row._from_pgatour
  ) {
    return false;
  }
  return true;
}

/** Live preds rows carry gross score only — never overwrite CSV GIR/FW/putts with projection formulas. */
function mergeLiveInPlayOntoHistoryRound(existing, liveRec) {
  const out = { ...existing, ...liveRec };
  if (!liveRec?._from_live_in_play) return out;
  for (const k of LIVE_HISTORY_COUNTING_KEYS) {
    if (historyRowHasStoredCountingStat(existing, k)) out[k] = existing[k];
    else out[k] = existing[k] ?? null;
  }
  return out;
}

const LIVE_HISTORY_SG_KEYS = ["sg_putt", "sg_app", "sg_arg", "sg_ott", "sg_t2g", "sg_total"];

/** Layer live LTS + shot aggregate onto pgatouR rows before upsert. */
function enrichCurrentEventRowsWithLiveAndShots(rows, liveRows, shotsAgg) {
  if (!Array.isArray(rows) || !rows.length) return rows;
  const liveByKey = new Map();
  for (const r of liveRows || []) {
    const dg = Math.round(num(r?.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    liveByKey.set(`${dg}|${r.year}|${r.round_num}|${normEvt(r.event_name)}`, r);
  }
  /** @type {Record<string, Record<string, object>>|null} */
  let actualsByDg = null;
  if (fs.existsSync(LIVE_IN_PLAY_JSON)) {
    try {
      const live = JSON.parse(fs.readFileSync(LIVE_IN_PLAY_JSON, "utf8"));
      const proj = fs.existsSync(PROJECTIONS_JSON)
        ? JSON.parse(fs.readFileSync(PROJECTIONS_JSON, "utf8"))
        : {};
      const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : {};
      const fairwayHoles = Math.round(
        num(proj?.projection_course_basis?.fairway_holes_modeled ?? meta.projection_course_basis?.fairway_holes_modeled, 14),
      );
      const roundPar = num(proj?.course_par_18 ?? meta.course_par_18, 72) || 72;
      actualsByDg = resolveLiveRoundActualsByDg(live, {
        roundPar,
        fairwayHoles: Number.isFinite(fairwayHoles) && fairwayHoles >= 1 ? fairwayHoles : 14,
      });
    } catch {
      /* ignore */
    }
  }
  const byDgSk = shotsAgg?.byDgSk || new Map();
  const byDgEvtYrRnd = shotsAgg?.byDgEvtYrRnd || new Map();
  const byPkEvtYrRnd = shotsAgg?.byPkEvtYrRnd || new Map();
  return rows.map((r) => {
    const dg = Math.round(num(r.dg_id, NaN));
    const sk = r.sortKey;
    const evtKey = `${normEvt(r.event_name)}|${r.year}|${r.round_num}`;
    const pk = playerKeyCanonical(String(r.player_name || ""));
    const live = liveByKey.get(`${dg}|${r.year}|${r.round_num}|${normEvt(r.event_name)}`);
    const act = actualsByDg?.[String(dg)]?.[String(r.round_num)];
    const shotOv =
      (Number.isFinite(dg) && Number.isFinite(sk) ? byDgSk.get(`${dg}|${sk}`) : undefined) ??
      (Number.isFinite(dg) ? byDgEvtYrRnd.get(`${dg}|${evtKey}`) : undefined) ??
      (pk ? byPkEvtYrRnd.get(`${pk}|${evtKey}`) : undefined) ??
      null;
    const out = { ...r };
    if (act && typeof act === "object") {
      const girRaw = num(act.gir, NaN);
      if (Number.isFinite(girRaw)) out.gir = Math.round(girRaw <= 1 ? girRaw * 18 : girRaw);
      if (Number.isFinite(num(act.fairways, NaN))) out.fairways = Math.round(num(act.fairways, NaN));
      if (Number.isFinite(num(act.putts, NaN))) out.putts = Math.round(num(act.putts, NaN));
      for (const k of LIVE_HISTORY_SG_KEYS) {
        if (Number.isFinite(num(act[k], NaN))) out[k] = act[k];
      }
    }
    for (const k of ["gir", "fairways", "putts", ...LIVE_HISTORY_SG_KEYS]) {
      if (Number.isFinite(num(out[k], NaN))) continue;
      if (live && Number.isFinite(num(live[k], NaN))) out[k] = live[k];
    }
    if (shotOv) {
      if (shotOv.gir != null && !Number.isFinite(num(out.gir, NaN))) out.gir = shotOv.gir;
      if (shotOv.fairways != null && !Number.isFinite(num(out.fairways, NaN))) out.fairways = shotOv.fairways;
      if (shotOv.putts != null && !Number.isFinite(num(out.putts, NaN))) out.putts = shotOv.putts;
    }
    const mf = metricFields(out);
    stripGirFairwaysPuttsIfGarbage(mf);
    return { ...out, ...mf };
  });
}

/** preds/live-tournament-stats during the live week — prefer CSV counting columns when already present. */
function mergeLiveTournamentStatsOntoHistoryRound(existing, liveRec) {
  if (liveRec?._from_pgatour) {
    const out = {
      ...existing,
      ...liveRec,
      _from_pgatour: true,
      _from_live_tournament_stats: true,
    };
    for (const k of [...LIVE_HISTORY_COUNTING_KEYS, ...LIVE_HISTORY_SG_KEYS]) {
      if (Number.isFinite(num(liveRec[k], NaN))) out[k] = liveRec[k];
      else if (Number.isFinite(num(existing[k], NaN))) out[k] = existing[k];
    }
    return scrubLivePlaceholderCountingOnRow(out);
  }
  const cleaned = sanitizeLiveCountingFields({ ...liveRec });
  const prev = sanitizeLivePlaceholderCountingOnRow(existing);
  const out = { ...existing, ...cleaned };
  for (const k of LIVE_HISTORY_COUNTING_KEYS) {
    if (historyRowHasStoredCountingStat(prev, k) && historyLiveCountingTrusted(prev)) out[k] = prev[k];
    else if (Number.isFinite(num(cleaned[k], NaN))) out[k] = cleaned[k];
    else if (historyLiveCountingTrusted(prev)) out[k] = prev[k];
    else out[k] = cleaned[k] ?? null;
  }
  out._from_live_tournament_stats = true;
  delete out._from_live_in_play;
  return out;
}

function scrubLivePlaceholderCountingOnRow(row) {
  return sanitizeLiveCountingFields(row && typeof row === "object" ? { ...row } : row);
}

function historyLiveCountingTrusted(row) {
  if (!row || typeof row !== "object") return false;
  if (row._from_pgatour || row._from_dg_historical_rounds) return true;
  if (!row._from_live_tournament_stats && !row._from_live_in_play) return true;
  const b = num(row.birdies, NaN);
  const p = num(row.pars, NaN);
  const bg = num(row.bogies ?? row.bogeys, NaN);
  if (Number.isFinite(b) || Number.isFinite(bg)) return true;
  if (Number.isFinite(p) && p > 0 && p < 14) return true;
  if (Number.isFinite(p) && p >= 10 && (!Number.isFinite(b) || b === 0) && (!Number.isFinite(bg) || bg === 0)) return false;
  return false;
}

function mergeLiveOntoHistoryRound(existing, liveRec) {
  if (liveRec?._from_live_tournament_stats || liveRec?._from_pgatour)
    return mergeLiveTournamentStatsOntoHistoryRound(existing, liveRec);
  return mergeLiveInPlayOntoHistoryRound(existing, liveRec);
}

function upsertLiveRoundRows(byDgId, liveByDg) {
  const liveList = normalizeLiveRoundList(liveByDg);
  if (!liveList.length) return 0;
  let n = 0;
  for (const liveRec of liveList) {
    const dg = Math.round(num(liveRec?.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    if (eventCompletedIsFutureMdY(liveRec.event_completed) || historyRoundChartDateIsFuture(liveRec)) continue;
    let bucket = byDgId.get(dg);
    if (!bucket || !Array.isArray(bucket.rounds)) {
      byDgId.set(dg, {
        dg_id: dg,
        player_name: String(liveRec.player_name || "").trim(),
        rounds: [],
      });
      bucket = byDgId.get(dg);
    } else if (!bucket.player_name && liveRec.player_name) {
      bucket.player_name = String(liveRec.player_name).trim();
    }
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
    if (hitIdx >= 0) bucket.rounds[hitIdx] = mergeLiveOntoHistoryRound(bucket.rounds[hitIdx], liveRec);
    else bucket.rounds.push(liveRec);
    bucket.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
    if (bucket.rounds.length > MAX_ROUNDS_PER_PLAYER) bucket.rounds = bucket.rounds.slice(-MAX_ROUNDS_PER_PLAYER);
    n++;
  }
  return n;
}

/** Field-update style JSON: dg_id roster can be broader than preds/in-play `data` (full tournament). */
function fieldRowsFromLiveUpdates(fu) {
  if (!fu || typeof fu !== "object") return [];
  const f = fu.field ?? fu.field_updates ?? fu.players ?? fu.data;
  return Array.isArray(f) ? f : [];
}

function loadAllowedDgIds() {
  /** @type {Set<number>} */
  const ids = new Set();

  if (fs.existsSync(PROJECTIONS_JSON)) {
    try {
      const raw = JSON.parse(fs.readFileSync(PROJECTIONS_JSON, "utf8"));
      for (const p of raw.players || []) {
        const id = Math.round(num(p.dg_id));
        if (Number.isFinite(id)) ids.add(id);
      }
    } catch {
      console.warn("[build-player-history] projections.json parse failed — allowlist may be incomplete");
    }
  } else {
    console.warn("No projections.json — allowlist falls back to live-in-play ids only.");
  }

  if (fs.existsSync(LIVE_IN_PLAY_JSON)) {
    try {
      const live = JSON.parse(fs.readFileSync(LIVE_IN_PLAY_JSON, "utf8"));
      for (const row of Array.isArray(live.data) ? live.data : []) {
        const id = Math.round(num(row?.dg_id ?? row?.dgId, NaN));
        if (Number.isFinite(id)) ids.add(id);
      }
      const fu = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : null;
      for (const p of fieldRowsFromLiveUpdates(fu)) {
        const id = Math.round(num(p?.dg_id ?? p?.dgId, NaN));
        if (Number.isFinite(id)) ids.add(id);
      }
    } catch {
      /* ignore malformed live snapshot */
    }
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
  const faDirect = num(row.fairways, NaN);
  const da = num(row.driving_acc, NaN);
  const fwRaw = Number.isFinite(faDirect) ? faDirect : da;
  let girCount = Number.isFinite(gir) ? countFromRateOrRaw(gir, 18) : null;
  let fwCount = Number.isFinite(fwRaw) ? countFromRateOrRaw(fwRaw, 14) : null;
  if (girCount === 0 || girCount === 1) girCount = null;
  if (fwCount === 0 || fwCount === 1) fwCount = null;
  const ptRaw = num(row.putts, NaN);
  let puttsCount = null;
  if (Number.isFinite(ptRaw) && ptRaw > 1.5 && ptRaw < 80) puttsCount = Math.round(ptRaw);
  return {
    round_score: num(row.round_score),
    birdies: num(row.birdies),
    pars: num(row.pars),
    bogies: num(row.bogies),
    gir: girCount,
    fairways: fwCount,
    /** From historical_rounds_all.csv when DataGolf supplies `putts`; else filled from shot-round aggregate. */
    putts: puttsCount,
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

function logCsvScanProgress(phase, rowsScanned, matchedRows, extra = "") {
  const interval = Math.max(50_000, parseInt(String(process.env.GOLF_BUILD_HISTORY_PROGRESS_EVERY || "200000"), 10) || 200000);
  if (rowsScanned > 0 && rowsScanned % interval === 0) {
    console.log(
      `[build-player-history] ${phase}: scanned ${rowsScanned.toLocaleString()} CSV rows, kept ${matchedRows.toLocaleString()}${extra ? ` (${extra})` : ""}…`,
    );
  }
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

  console.log(
    `[build-player-history] Scanning rounds CSV (${path.basename(ROUNDS_CSV)}; ${allowedDgIds.size} allowed dg_ids, min_year ${MIN_YEAR}) — usually 1–4 min…`,
  );
  let rowsScanned = 0;
  let matchedRows = 0;
  const t0 = Date.now();

  const parser = createReadStream(ROUNDS_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );

  for await (const row of parser) {
    rowsScanned++;
    logCsvScanProgress("rounds CSV", rowsScanned, matchedRows);
    const tour = String(row.tour || "").toLowerCase();
    if (tour !== "pga" && tour !== "liv") continue;
    const yr = parseInt(row.year, 10);
    if (Number.isFinite(yr) && yr < MIN_YEAR) continue;
    const dg = Math.round(num(row.dg_id));
    if (!Number.isFinite(dg) || !allowedDgIds.has(dg)) continue;
    const rs = num(row.round_score);
    if (!Number.isFinite(rs)) continue;
    if (eventCompletedIsFutureMdY(row.event_completed) || historyRoundChartDateIsFuture(row)) continue;

    const eid = Math.round(num(row.event_id));
    const metaPatch =
      pgaMetaOverlay && Number.isFinite(eid) ? pgaMetaOverlay.get(`${eid}|${yr}`) : null;
    const rowForWeather = metaPatch ? { ...row, ...metaPatch } : row;

    const rnd = parseInt(row.round_num, 10) || 1;
    const eventDate = roundEventCompletedMdYFromEventEnd(row.event_completed, rnd, tour);
    const sortKey = parseEventCompletedChronoBase(eventDate) * 10 + rnd;
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
      event_completed: eventDate || String(row.event_completed || ""),
      year: yr,
      event_name: eventName,
      event_id: String(row.event_id || ""),
      course_name: formatCourseLabelForDisplay(courseRaw) || courseRaw || eventName,
      round_num: parseInt(row.round_num, 10) || 1,
      fin_text: String(row.fin_text || ""),
      _from_dg_historical_rounds: true,
      ...mf,
      ...weatherFields(rowForWeather),
      ...sgFields(row),
    };

    if (!byDgId.has(dg)) byDgId.set(dg, { dg_id: dg, player_name: String(row.player_name || ""), rounds: [] });
    const bucket = byDgId.get(dg);
    if (!bucket.player_name) bucket.player_name = String(row.player_name || "");
    bucket.rounds.push(rec);
    matchedRows++;
  }

  console.log(
    `[build-player-history] Rounds CSV done in ${((Date.now() - t0) / 1000).toFixed(1)}s — scanned ${rowsScanned.toLocaleString()} rows, kept ${matchedRows.toLocaleString()} for ${byDgId.size} player(s).`,
  );

  for (const [, bucket] of byDgId) {
    bucket.rounds = bucket.rounds.filter(
      (r) => !eventCompletedIsFutureMdY(r.event_completed) && !historyRoundChartDateIsFuture(r),
    );
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
    if (HOLES_CSV === null || HOLES_CSV === "") {
      console.log("[build-player-history] Hole-by-hole CSV skipped (HOLE_DATA_CSV empty — Historical Trends unaffected).");
    }
    return holesByPlayerKey;
  }

  console.log(
    `[build-player-history] Scanning hole_data CSV (${path.basename(HOLES_CSV)}; can take 15–30+ min) — set HOLE_DATA_CSV="" on live push to skip…`,
  );
  let rowsScanned = 0;
  let matchedRows = 0;
  const t0 = Date.now();

  const parser = createReadStream(HOLES_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );

  for await (const row of parser) {
    rowsScanned++;
    logCsvScanProgress("hole_data CSV", rowsScanned, matchedRows);
    const pk = playerKeyHole(row.player_name);
    const ev = normEvt(row.tournament_name);
    const rn = parseInt(row.round, 10) || 1;
    const triple = `${pk}|||${ev}|||${rn}`;
    if (!allowedTriples.has(triple)) continue;
    matchedRows++;

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

  console.log(
    `[build-player-history] hole_data done in ${((Date.now() - t0) / 1000).toFixed(1)}s — scanned ${rowsScanned.toLocaleString()} rows, matched ${matchedRows.toLocaleString()} hole rows for ${Object.keys(holesByPlayerKey).length} player(s).`,
  );

  return holesByPlayerKey;
}

async function main() {
  console.log("Rounds CSV:", ROUNDS_CSV);
  console.log("Metadata overlay CSV:", METADATA_OVERLAY_CSV || "(none)");
  console.log("min_year (CSV filter):", MIN_YEAR, "| max_rounds/player:", MAX_ROUNDS_PER_PLAYER);
  console.log("Holes CSV:", HOLES_CSV || "(skip)");
  const allowed = loadAllowedDgIds();
  console.log("Allowed dg_ids (projections ∪ live-in-play ∪ field-updates):", allowed.size);

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
  let liveMergedRows = 0;
  const liveRows = buildLiveHistoryRowsFromBundle();
  if (liveRows?.length) {
    const nLive = upsertLiveRoundRows(byDgId, liveRows);
    liveMergedRows += nLive;
    console.log(
      `[build-player-history] Merged ${nLive} live-tournament round row(s) from live-in-play.json (preds/live-tournament-stats + in-play R* gross).`,
    );
  }
  let pgaRows = loadPgatourEventRoundRows();
  if (pgaRows?.length) {
    pgaRows = enrichCurrentEventRowsWithLiveAndShots(pgaRows, liveRows, shotsAgg);
    const nPga = upsertLiveRoundRows(byDgId, pgaRows);
    liveMergedRows += nPga;
    console.log(`[build-player-history] Merged ${nPga} pgatouR event round row(s) from pgatour_event_rounds.json.`);
  } else if (fs.existsSync(LIVE_IN_PLAY_JSON) && !liveRows?.length) {
    console.log("[build-player-history] No live-week history rows to merge (check event alignment / date_start).");
  }
  let futureRoundsStripped = 0;
  for (const [, bucket] of byDgId) {
    if (!bucket?.rounds) continue;
    const before = bucket.rounds.length;
    bucket.rounds = bucket.rounds.filter(
      (r) => !eventCompletedIsFutureMdY(r.event_completed) && !historyRoundChartDateIsFuture(r),
    );
    futureRoundsStripped += before - bucket.rounds.length;
  }
  if (futureRoundsStripped > 0) {
    console.log("[build-player-history] Removed", futureRoundsStripped, "future/unplayed round row(s) from history export");
  }

  // --- Regression guard: detect double-offset or dayBump bugs ---
  {
    const today = todayDateOnlyUtcMs();
    let dateErrors = 0;
    for (const [, bucket] of byDgId) {
      if (!bucket?.rounds) continue;
      for (const r of bucket.rounds) {
        if (!r._from_dg_historical_rounds) continue;
        const rs = num(r.round_score, NaN);
        if (!Number.isFinite(rs) || rs <= 0) continue;
        const ecBase = parseEventCompletedChronoBase(r.event_completed);
        if (!ecBase) continue;
        const skBase = Number.isFinite(r.sortKey) ? Math.floor(r.sortKey / 10) : 0;
        if (skBase > 0 && skBase !== ecBase) {
          if (dateErrors++ < 3) console.error(`[build-player-history] DATE BUG: sortKey date ${skBase} != event_completed date ${ecBase} for dg_id=${r.dg_id} R${r.round_num} ${r.event_name}`);
        }
        const chartMs = historyRoundChartDateUtcMs(r);
        if (Number.isFinite(chartMs) && chartMs > today) {
          if (dateErrors++ < 3) console.error(`[build-player-history] FUTURE BUG: chartDate > today for dg_id=${r.dg_id} R${r.round_num} ec=${r.event_completed} (score=${rs})`);
        }
      }
    }
    if (dateErrors > 0) {
      console.error(`[build-player-history] FATAL: ${dateErrors} round date error(s) detected. Fix historyRoundChartDateUtcMs or roundEventCompletedMdYFromEventEnd.`);
      process.exit(1);
    }
  }

  console.log("Players with rounds:", byDgId.size);
  console.log(
    "[build-player-history] Historical Trends: CSV (historical-raw-data/rounds) + live-week rows from live-in-play when present.",
  );
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

  console.log("[build-player-history] Serializing player_round_history.json (large JSON — may take 1–2 min)…");
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
      live_tournament_stats_merged: liveMergedRows,
      pgatour_event_rounds_json: fs.existsSync(PGATOUR_EVENT_ROUNDS_JSON)
        ? path.basename(PGATOUR_EVENT_ROUNDS_JSON)
        : null,
    },
    byDgId: Object.fromEntries(
      [...byDgId.entries()].map(([k, v]) => [
        String(k),
        { dg_id: v.dg_id, player_name: v.player_name, rounds: v.rounds },
      ])
    ),
    holesByPlayerKey,
  };

  let json;
  try {
    json = JSON.stringify(out);
  } catch (e) {
    console.error("[build-player-history] JSON.stringify failed (dataset may be too large for RAM):", e instanceof Error ? e.message : e);
    throw e;
  }
  const needBytes = Buffer.byteLength(json, "utf8");

  const free = tryFreeBytesOnDriveOf(OUT_JSON);
  if (free != null && needBytes > free) {
    console.error(
      `[build-player-history] Not enough free space on drive: need ~${fmtBytes(needBytes)} for a complete temp write before replacing ${OUT_JSON}, only ~${fmtBytes(free)} free. Empty Recycle Bin, delete large temp files, or set HOLE_DATA_CSV="" to skip hole rows / lower GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER.`,
    );
    process.exit(1);
  }

  // Write to *.tmp then rename over the destination so the previous JSON stays intact if write fails;
  // rename replaces OUT_JSON in one step (previous file is not deleted until the new bytes are ready).
  const tmpPath = `${OUT_JSON}.tmp`;
  try {
    fs.writeFileSync(tmpPath, json, "utf8");
    fs.renameSync(tmpPath, OUT_JSON);
  } catch (e) {
    const err = /** @type {NodeJS.ErrnoException} */ (e);
    try {
      if (fs.existsSync(tmpPath)) fs.unlinkSync(tmpPath);
    } catch (_) {}
    if (err && err.code === "ENOSPC") {
      console.error(
        `[build-player-history] ENOSPC (~${fmtBytes(needBytes)}). ${path.basename(OUT_JSON)} was not replaced; source CSVs are unchanged. ` +
          `Peak disk use briefly includes old + *.tmp during upgrade — free more space, then retry.`,
      );
    }
    throw e;
  }

  const st = fs.statSync(OUT_JSON);
  console.log("Wrote", OUT_JSON, `(${fmtBytes(st.size)})`);
  writePlayerHistoryShards(out);
  writeCourseHistoryShards(out);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
