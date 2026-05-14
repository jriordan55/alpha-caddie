/**
 * Pull PGA field + skill + pre-tournament probs from DataGolf (same idea as
 * round_projections.R RAW_PROJECTIONS / GOLF_RAW_PROJECTIONS=1) and write projections.json.
 * Counting stats and GIR/fairways are **skill + historical** from `data/historical_rounds_all.csv` when sample is
 * large enough. Fairways: blend SG:OTT vs field, optional driving-field rate, and a **tour-wide FW vs strokes-to-par**
 * regression (driving_acc as fraction, count, or percent) so OTT and miscoded driving scalars cannot both pin FW low.
 * Scoring uses **course_par_18** from resolved hole pars (not a fixed 72).
 * Optional **preds/pre-tournament** per-round stroke column (when present in the feed) nudges μ_sg toward that baseline.
 * Historical CSV still calibrates count curves vs (score−par); GIR uses SG:APP vs median field (no fantasy blend).
 * Hole counts: historical rounds regress eagles/birdies/bogeys/doubles vs (round_score − course_par), shrunk with a
 * ceiling so legacy μ curves still spread the field; pars are residual. A **soft** bird/bog nudge partially aligns
 * implied strokes vs par with **score_to_par = −μ_sg** without collapsing pars across players.
 * R2–R4 rows re-derive from scaled μ_SG (default multipliers 1, 0.945, 0.885, 0.82 — override GOLF_NODE_ROUND_MU_MULT).
 * Set GOLF_SKIP_HIST_STATS_ON_FETCH=1 to skip the historical CSV calibration pass (count curves only; GIR/FW are skill-only).
 * Set GOLF_RESET_PROPS=1 so fetch:dg does not copy prior `props` from projections.json (default: preserve DK round O/U when the same week/event).
 * Usage (from alpha-caddie-web/):
 *   set DATAGOLF_API_KEY=your_key
 *   npm run fetch:dg
 * Shots (heavy): all_shots CSV is **off by default**. `mirror-model-data-to-web` does not copy
 * all_shots_*.csv unless GOLF_USE_ALL_SHOTS_CSV=1 or `npm run refresh:shots` (passes includeAllShotsCsv).
 * Set GOLF_BUILD_SHOTS_WEB_ON_FETCH=1 to run build-player-shots-web.mjs after fetch (still writes a
 * minimal JSON unless GOLF_USE_ALL_SHOTS_CSV=1 and data/all_shots_2022_2026.csv exists).
 *
 * Or copy datagolf.local.example.json -> datagolf.local.json and put "apiKey" there.
 *
 * Multi-tour field: when GOLF_DATAGOLF_TOUR is `pga`, fetches field-updates for both `pga` and `opp` and picks the feed
 * with the newest `last_updated` (and sufficient players). Override list with GOLF_FIELD_UPDATES_TOUR_CANDIDATES=pga,euro,opp.
 * Writes projections.datagolf_feed_tour so in-play + book-odds use the same tour code.
 * get-schedule (upcoming_only yes/no) picks which field-updates row matches DG’s canonical next event (best row by date when many).
 * If schedule titles don’t match field-updates labels, preds/in-play info.event_name narrows candidates before timestamp tie-break.
 * Skip anchor with GOLF_SKIP_GET_SCHEDULE_FIELD_ANCHOR=1.
 *
 * Requires Node 18+ (global fetch).
 *
 * Hole Hangout: writes hole_pars (18 ints) — prefers preds/live-hole-stats per-hole par table (same feed as
 * live-in-play.json live_hole_stats), then field-updates when course label matches resolved course_used,
 * then course_holes*.json, hole_pars_from_shots.json, CSV, else generic. Set GOLF_SKIP_LIVE_HOLE_STATS_HOLE_PARS=1 to skip.
 * Override: GOLF_HOLES_CSV=path/to.csv
 *
 * After projections: refreshes repo data/historical_rounds_all.csv via Node
 * (scripts/update-historical-rounds-node.mjs — PGA + LIV, DataGolf historical-raw-data/rounds), then rebuilds player_round_history.json
 * and writes embedded-player-round-history.js (window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__).
 * Set ALPHA_CADDIE_EMBED_HISTORY=0 to skip that step. Set ALPHA_CADDIE_PGA_HISTORY=1 to run
 * the pgatouR history script after the CSV build (embed runs again after that).
 *
 * Outright EV rows use betting-tools/outrights, which backs the Finish Position Betting Tool.
 * preds/pre-tournament: GOLF_PRE_TOURNAMENT_ODDS_FORMAT defaults to decimal (docs show decimal odds; percent is ambiguous).
 */

import { spawnSync } from "child_process";
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { parse } from "csv-parse";
import { fileURLToPath } from "url";
import { findRscriptSync } from "./find-rscript.mjs";
import {
  coursesClearlyDistinct,
  eventsLikelySame,
  fieldWeekKey,
  foldComparableTitle,
  titleTokenOverlapRatio,
} from "./dg-events-align.mjs";
import { fetchDataGolfOutrightsApi } from "./datagolf-outrights-api.mjs";
import { holeParsFromLiveHoleStatsPayload } from "./dg-live-hole-pars.mjs";
import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";
import { resolveGolfModelDir } from "./resolve-golf-model-dir.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
/** Honors GOLF_MODEL_DIR env (Render / monorepo); otherwise resolve-golf-model-dir.mjs heuristics. */
const GOLF_MODEL_ROOT = resolveGolfModelDir(ROOT);

/** When no course/event match, Hole Hangout uses this 18-hole par pattern (same as web app fallback). */
const GENERIC_HOLE_PARS_FALLBACK = [4, 4, 3, 4, 4, 5, 4, 3, 4, 4, 4, 3, 4, 4, 5, 4, 3, 5];

function normHoleKey(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/\s+/g, " ")
    .trim();
}

/** Same rules as browser `normCourseNameKey` (app.js) — keep course_used aligned with embedded history keys. */
function normCourseNameKeyFetch(raw) {
  let s = String(raw || "").trim().toLowerCase();
  s = s.replace(/\([^)]*\)/g, " ");
  s = s.replace(/\b(blue monster|stadium course|championship course|club de golf)\b/g, " ");
  s = s.replace(/&/g, " and ");
  s = s.replace(/\bthe players\b/gi, " ");
  s = s.replace(/\bc\.?\s*c\.?\b/gi, "country club");
  s = s.replace(/\bg\.?\s*c\.?\b/gi, "golf club");
  s = s.replace(/\bg\.?\s*l\.?\b/gi, "golf links");
  s = s.replace(/\bgolf club(\s+golf club)+\b/gi, "golf club");
  s = s.replace(/\bcountry club(\s+country club)+\b/gi, "country club");
  s = s.replace(/\bgolf links(\s+golf links)+\b/gi, "golf links");
  s = s.replace(/[^a-z0-9]+/g, " ");
  s = s.replace(/\s+/g, " ").trim();
  const aliases = {
    albany: "albany golf club",
    "albany bahamas": "albany golf club",
    "sea island resort": "sea island golf club",
  };
  return aliases[s] || s;
}

/** Title-case label for projections/meta after normalizing abbreviations (Gc/Cc, etc.). */
function canonicalCourseLabelForProjections(raw) {
  const k = normCourseNameKeyFetch(raw);
  if (!k) return String(raw || "").trim();
  return k.replace(/\b\w/g, (c) => c.toUpperCase());
}

function parseCsvLine(line) {
  const fields = [];
  let i = 0;
  while (i < line.length) {
    if (line[i] === '"') {
      i++;
      let s = "";
      while (i < line.length) {
        if (line[i] === '"' && line[i + 1] === '"') {
          s += '"';
          i += 2;
          continue;
        }
        if (line[i] === '"') {
          i++;
          break;
        }
        s += line[i++];
      }
      fields.push(s);
      if (line[i] === ",") i++;
      continue;
    }
    const j = line.indexOf(",", i);
    if (j < 0) {
      fields.push(line.slice(i).trim());
      break;
    }
    fields.push(line.slice(i, j).trim());
    i = j + 1;
  }
  return fields;
}

function eventMatchesCsvTournament(eventName, csvTournamentName) {
  const a = normHoleKey(eventName);
  const b = normHoleKey(csvTournamentName);
  if (!a || !b) return false;
  if (a === b) return true;
  if (b.includes(a) || a.includes(b)) return true;
  const words = a.split(/\s+/).filter((w) => w.length >= 4);
  return words.length > 0 && words.every((w) => b.includes(w));
}

function extractHoleParsFromCsv(csvPath, eventName) {
  if (!existsSync(csvPath)) return null;
  let text;
  try {
    text = readFileSync(csvPath, "utf8");
  } catch {
    return null;
  }
  const lines = text.split(/\r?\n/);
  if (lines.length < 2) return null;
  const header = parseCsvLine(lines[0]).map((h) => h.replace(/^"|"$/g, "").toLowerCase());
  const ti = header.indexOf("tournament_name");
  const ri = header.indexOf("round");
  const hi = header.indexOf("hole");
  const pi = header.indexOf("par");
  if (ti < 0 || ri < 0 || hi < 0 || pi < 0) return null;
  const byHole = new Map();
  const ev = String(eventName || "").trim();
  if (!ev) return null;
  for (let k = 1; k < lines.length; k++) {
    if (!lines[k].trim()) continue;
    const row = parseCsvLine(lines[k]);
    if (row.length <= pi) continue;
    const tname = row[ti]?.replace(/^"|"$/g, "") || "";
    if (!eventMatchesCsvTournament(ev, tname)) continue;
    if (num(row[ri], NaN) !== 1) continue;
    const h = Math.round(num(row[hi], NaN));
    const pr = Math.round(num(row[pi], NaN));
    if (h < 1 || h > 18) continue;
    if (pr !== 3 && pr !== 4 && pr !== 5) continue;
    if (!byHole.has(h)) byHole.set(h, pr);
  }
  if (byHole.size < 18) return null;
  const arr = [];
  for (let h = 1; h <= 18; h++) {
    if (!byHole.has(h)) return null;
    arr.push(byHole.get(h));
  }
  return arr;
}

function loadCourseHolesMaps() {
  const out = { by_course: {}, by_event: {} };
  function mergeFile(p) {
    if (!existsSync(p)) return;
    try {
      const j = JSON.parse(readFileSync(p, "utf8"));
      if (j.by_course && typeof j.by_course === "object") Object.assign(out.by_course, j.by_course);
      if (j.by_event && typeof j.by_event === "object") Object.assign(out.by_event, j.by_event);
    } catch (e) {
      console.warn("course holes JSON skipped:", p, e.message);
    }
  }
  mergeFile(join(ROOT, "course_holes.json"));
  mergeFile(join(ROOT, "course_holes.local.json"));
  return out;
}

/** Prefer longest / most specific bundled key when multiple substring matches exist (unordered JSON keys). */
function bestBundledHoleParsMatch(byMap, needleKey) {
  if (!needleKey || !byMap || typeof byMap !== "object") return null;
  let best = null;
  let bestScore = -1;
  for (const [k, v] of Object.entries(byMap)) {
    if (!k || !Array.isArray(v) || v.length !== 18) continue;
    let score = 0;
    if (k === needleKey) score = 10000;
    else if (needleKey.includes(k)) score = 1000 + k.length;
    else if (k.includes(needleKey)) score = 500 + needleKey.length;
    else continue;
    if (score > bestScore) {
      bestScore = score;
      best = v;
    }
  }
  return bestScore > 0 ? best : null;
}

function lookupHoleParsFromMaps(maps, course_used, event_name) {
  const ck = normHoleKey(course_used);
  const ek = normHoleKey(event_name);
  const bc = maps.by_course || {};
  const be = maps.by_event || {};
  if (ck && bc[ck] && Array.isArray(bc[ck]) && bc[ck].length === 18) return { pars: bc[ck], source: "bundled" };
  if (ek && be[ek] && Array.isArray(be[ek]) && be[ek].length === 18) return { pars: be[ek], source: "bundled" };
  const fuzzyC = bestBundledHoleParsMatch(bc, ck);
  if (fuzzyC) return { pars: fuzzyC, source: "bundled" };
  const fuzzyE = bestBundledHoleParsMatch(be, ek);
  if (fuzzyE) return { pars: fuzzyE, source: "bundled" };
  return null;
}

/**
 * Build pars for holes 1–18 from a DataGolf-style array.
 * APIs often return `{ hole, par }[]` in arbitrary order — never assume index i is hole i+1.
 */
function normalizeHoleParsApiArray(a) {
  if (!Array.isArray(a) || a.length < 18) return null;
  let objectKeyed = false;
  for (const x of a) {
    if (x && typeof x === "object" && !Array.isArray(x)) {
      const h = num(x.hole ?? x.hole_number ?? x.hole_num ?? x.num ?? x.n, NaN);
      if (Number.isFinite(h) && h >= 1 && h <= 18) {
        objectKeyed = true;
        break;
      }
    }
  }
  if (objectKeyed) {
    const byHole = new Map();
    for (const x of a) {
      if (!x || typeof x !== "object" || Array.isArray(x)) continue;
      const h = Math.round(num(x.hole ?? x.hole_number ?? x.hole_num ?? x.num ?? x.n, NaN));
      let p = num(x.par ?? x.par_hole ?? x.hole_par ?? x.par_for_hole, NaN);
      if (!Number.isFinite(h) || h < 1 || h > 18) continue;
      if (!Number.isFinite(p) || p < 3 || p > 5) continue;
      byHole.set(h, Math.round(p));
    }
    if (byHole.size < 18) return null;
    const arr = [];
    for (let h = 1; h <= 18; h++) {
      if (!byHole.has(h)) return null;
      arr.push(byHole.get(h));
    }
    return arr;
  }
  const arr = [];
  for (let i = 0; i < 18; i++) {
    const x = a[i];
    let p = NaN;
    if (typeof x === "number") p = x;
    else if (typeof x === "string") p = num(x.trim(), NaN);
    else if (x && typeof x === "object") p = num(x.par ?? x.par_hole ?? x.hole_par, NaN);
    if (!Number.isFinite(p) || p < 3 || p > 5) return null;
    arr.push(Math.round(p));
  }
  return arr;
}

function holeParsFromFieldUpdates(raw) {
  if (!raw || typeof raw !== "object") return null;
  const tryOneObject = (obj) => {
    if (!obj || typeof obj !== "object") return null;
    const nested =
      normalizeHoleParsApiArray(obj.holes) || normalizeHoleParsApiArray(obj.course_holes);
    if (nested) return nested;
    if (Array.isArray(obj.hole_par) && obj.hole_par.length === 18) {
      const arr = obj.hole_par.map((x) => Math.round(num(x, NaN)));
      if (arr.every((n) => n >= 3 && n <= 5)) return arr;
    }
    return null;
  };
  const roots = [raw, raw.course, raw.event, raw.info, raw.metadata, raw.tournament].filter(
    (x) => x && typeof x === "object",
  );
  for (const r of roots) {
    const hit = tryOneObject(r);
    if (hit) return hit;
  }
  return null;
}

function lookupHoleParsFromShotsExport(event_name) {
  const p = join(ROOT, "hole_pars_from_shots.json");
  if (!existsSync(p)) return null;
  let j;
  try {
    j = JSON.parse(readFileSync(p, "utf8"));
  } catch {
    return null;
  }
  const map = j && typeof j === "object" ? j.hole_pars_by_event_norm : null;
  if (!map || typeof map !== "object") return null;
  const ek = normHoleKey(event_name);
  if (!ek) return null;
  const ekCompact = ek.replace(/[^a-z0-9]+/g, " ").replace(/\s+/g, " ").trim();
  for (const [k, arr] of Object.entries(map)) {
    if (!Array.isArray(arr) || arr.length !== 18) continue;
    const kk = normHoleKey(k).replace(/[^a-z0-9]+/g, " ").replace(/\s+/g, " ").trim();
    if (!kk) continue;
    if (kk === ekCompact || ekCompact.includes(kk) || kk.includes(ekCompact)) {
      const pars = arr.map((x) => Math.round(num(x, NaN)));
      if (pars.every((n) => n >= 3 && n <= 5)) return { pars, source: "shots_csv" };
    }
  }
  return null;
}

function resolveHoleParsForEvent({ fieldRaw, course_used, event_name, field_updates_course_used, liveHoleStats }) {
  if (String(process.env.GOLF_SKIP_LIVE_HOLE_STATS_HOLE_PARS || "").trim() !== "1") {
    const fromLh = holeParsFromLiveHoleStatsPayload(liveHoleStats, course_used, fieldRaw, event_name);
    if (fromLh) return { pars: fromLh, source: "live_hole_stats" };
  }

  /** Prefer live field-updates only when hole metadata matches the resolved course (pret may relabel course_used). */
  const fromField = holeParsFromFieldUpdates(fieldRaw);
  const fuCrs = String(field_updates_course_used ?? "").trim();
  const resCrs = String(course_used ?? "").trim();
  const courseLabelsMatch =
    !resCrs ||
    !fuCrs ||
    foldComparableTitle(fuCrs) === foldComparableTitle(resCrs);
  if (fromField && courseLabelsMatch) return { pars: fromField, source: "field_updates" };
  if (fromField && !courseLabelsMatch) {
    console.warn(
      `Hole pars: ignoring field-updates holes — course label "${fuCrs}" vs resolved "${resCrs}" (using bundled/CSV)`,
    );
  }

  const maps = loadCourseHolesMaps();
  const fromMap = lookupHoleParsFromMaps(maps, course_used, event_name);
  if (fromMap) return { pars: fromMap.pars, source: fromMap.source };

  const fromShots = lookupHoleParsFromShotsExport(event_name);
  if (fromShots) return fromShots;

  const csvCandidates = [
    process.env.GOLF_HOLES_CSV,
    join(GOLF_MODEL_ROOT, "all_2026_holes.csv"),
    join(GOLF_MODEL_ROOT, "data", "all_2026_holes.csv"),
  ].filter(Boolean);
  for (const csvPath of csvCandidates) {
    const hp = extractHoleParsFromCsv(csvPath, event_name);
    if (hp) return { pars: hp, source: "csv", detail: csvPath };
  }

  return { pars: [...GENERIC_HOLE_PARS_FALLBACK], source: "generic" };
}

const RAW_ROUND_SD = Number(process.env.GOLF_RAW_ROUND_SD) || 2.75;
const COURSE_PAR_18 = Number(process.env.GOLF_COURSE_PAR) || 72;
const N_FAIRWAY_HOLES = Number(process.env.GOLF_N_FAIRWAY_HOLES) || 14;
const TOUR = (process.env.GOLF_DATAGOLF_TOUR || process.env.GOLF_TOUR || "pga").trim() || "pga";

const WEEKDAYS = ["Thursday", "Friday", "Saturday", "Sunday"];

function loadApiKey() {
  const env = (process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = join(ROOT, "datagolf.local.json");
  if (existsSync(p)) {
    try {
      const j = JSON.parse(readFileSync(p, "utf8"));
      return String(j.apiKey || j.key || "").trim();
    } catch {
      return "";
    }
  }
  return "";
}

function asArray(x) {
  if (x == null) return [];
  if (Array.isArray(x)) return x;
  return [];
}

/** DataGolf JSON: root array, or common `{ data | players | field | rankings | rows | … }` wrappers */
function rowsFromResponse(dat) {
  if (dat == null) return [];
  if (Array.isArray(dat)) return dat;
  if (typeof dat !== "object") return [];
  for (const k of ["data", "players", "field", "baseline_history_fit", "baseline", "rankings", "results", "rows"]) {
    const v = dat[k];
    if (Array.isArray(v)) return v;
    if (v && typeof v === "object" && !Array.isArray(v) && dat.baseline_history_fit == null) {
      /* sometimes single object */
    }
  }
  if (Array.isArray(dat.baseline_history_fit)) return dat.baseline_history_fit;
  return [];
}

/** Slender export for the web Course Fit "shot bins" table (`approach_skill_ytd.json`). */
const APPROACH_SKILL_SLIM_KEYS = [
  "50_100_fw_shot_count",
  "50_100_fw_sg_per_shot",
  "100_150_fw_shot_count",
  "100_150_fw_sg_per_shot",
  "150_200_fw_shot_count",
  "150_200_fw_sg_per_shot",
  "over_200_fw_shot_count",
  "over_200_fw_sg_per_shot",
  "under_150_rgh_shot_count",
  "under_150_rgh_sg_per_shot",
  "over_150_rgh_shot_count",
  "over_150_rgh_sg_per_shot",
];

function slimApproachSkillPlayerRow(row) {
  const id = Math.round(num(row.dg_id, NaN));
  const o = { dg_id: id, player_name: String(row.player_name || "").trim() };
  for (const k of APPROACH_SKILL_SLIM_KEYS) {
    o[k] = num(row[k], NaN);
  }
  return o;
}

function parseDgTimestamp(raw) {
  if (raw == null) return 0;
  if (typeof raw === "number" && Number.isFinite(raw)) {
    const n = raw;
    return n > 1e12 ? Math.round(n) : Math.round(n * 1000);
  }
  const ms = Date.parse(String(raw).trim());
  return Number.isFinite(ms) ? ms : 0;
}

function buildFieldRowsFromFieldRaw(fieldRaw) {
  const fieldList = asArray(fieldRaw.field).length ? asArray(fieldRaw.field) : rowsFromResponse(fieldRaw);
  const fieldRows = [];
  for (const p of fieldList) {
    if (!p || typeof p !== "object") continue;
    const dg_id = num(p.dg_id ?? p.dgId, NaN);
    const player_name = String(p.player_name || p.name || p.playerName || "").trim();
    if (!Number.isFinite(dg_id) || !player_name) continue;
    fieldRows.push({
      dg_id: Math.round(dg_id),
      player_name,
      country: String(p.country || "").trim(),
    });
  }
  return fieldRows;
}

async function fetchDg(path, params, key) {
  const u = new URL(`https://feeds.datagolf.com${path}`);
  for (const [k, v] of Object.entries(params)) u.searchParams.set(k, String(v));
  u.searchParams.set("key", key);
  const res = await fetch(u.toString(), { headers: { Accept: "application/json" } });
  if (!res.ok) throw new Error(`${path} HTTP ${res.status}: ${await res.text().catch(() => "")}`);
  return res.json();
}

function flattenScheduleEventRows(raw) {
  if (!raw || typeof raw !== "object") return [];
  let events = raw.events ?? raw.schedule ?? raw.tournaments ?? raw.data ?? raw.results;
  if (Array.isArray(events)) return events;
  if (events && typeof events === "object") {
    for (const k of ["events", "data", "rows", "schedule"]) {
      if (Array.isArray(events[k])) return events[k];
    }
  }
  return [];
}

function scheduleRowStartMs(row) {
  const o = row && typeof row === "object" ? row : {};
  const cand = [
    o.start_date,
    o.date,
    o.event_date,
    o.calendar_date,
    o.week_start,
    o.first_round_date,
  ];
  for (const c of cand) {
    const ms = Date.parse(String(c ?? "").trim());
    if (Number.isFinite(ms)) return ms;
  }
  const y = Number(o.calendar_year ?? o.year ?? o.season ?? NaN);
  if (Number.isFinite(y) && y >= 1990 && y <= 2100) return Date.UTC(y, 5, 15);
  return NaN;
}

/** Prefer chronologically next row when DG returns a season-long list (not always row 0). */
function pickBestScheduleAnchor(rows, graceDays = 3) {
  const GRACE_MS = graceDays * 86400000;
  const now = Date.now();
  const enriched = [];
  for (const row of rows) {
    const o = row && typeof row === "object" ? row : {};
    const nm = String(o.event_name ?? o.name ?? o.tournament_name ?? "").trim();
    if (!nm) continue;
    let sd = scheduleRowStartMs(o);
    enriched.push({ nm, sd });
  }
  if (!enriched.length) return null;
  const near = enriched.filter((x) => !Number.isFinite(x.sd) || x.sd >= now - GRACE_MS);
  const pool = near.length ? near : enriched;
  pool.sort((a, b) => {
    const ad = Number.isFinite(a.sd) ? a.sd : Infinity;
    const bd = Number.isFinite(b.sd) ? b.sd : Infinity;
    return ad - bd;
  });
  return { name: pool[0].nm };
}

/** Same idea as round_projections.R / live_data.R — canonical upcoming vs lagging field-updates labels. */
async function fetchCanonicalScheduleAnchor(scheduleTour, key) {
  for (const upcoming of ["yes", "no"]) {
    try {
      const raw = await fetchDg(
        "/get-schedule",
        { tour: scheduleTour, upcoming_only: upcoming, file_format: "json" },
        key
      );
      const rows = flattenScheduleEventRows(raw);
      if (!rows.length) {
        if (raw && typeof raw === "object") {
          console.warn(`get-schedule (${scheduleTour} upcoming=${upcoming}) empty events; keys:`, Object.keys(raw));
        }
        continue;
      }
      const pick = pickBestScheduleAnchor(rows);
      if (pick?.name) return { name: pick.name, upcoming_only: upcoming };
    } catch (e) {
      console.warn(`get-schedule tour=${scheduleTour} upcoming=${upcoming}:`, e.message || e);
    }
  }
  return null;
}

async function fetchInPlayEventLabel(tour, key) {
  try {
    const raw = await fetchDg("/preds/in-play", {
      tour,
      dead_heat: "no",
      odds_format: "percent",
      file_format: "json",
    }, key);
    const info = raw?.info && typeof raw.info === "object" ? raw.info : {};
    const nm = String(info.event_name ?? raw.event_name ?? "").trim();
    const rows = Array.isArray(raw.data) ? raw.data.length : 0;
    return { nm, rows };
  } catch (e) {
    console.warn(`preds/in-play tour=${tour}:`, e.message || e);
    return { nm: "", rows: 0 };
  }
}

function fieldCandidateMatchesSchedule(candidateEv, anchorName) {
  if (!anchorName || !candidateEv) return false;
  if (foldComparableTitle(candidateEv) === foldComparableTitle(anchorName)) return true;
  return eventsLikelySame(candidateEv, anchorName);
}

function firstNumCol(obj, candidates) {
  if (!obj || typeof obj !== "object") return null;
  for (const c of candidates) {
    if (c in obj && obj[c] != null && obj[c] !== "") return c;
  }
  return null;
}

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

/** Like browser `historyScalarOrNaN`: `Number(null) === 0` must not coerce missing history into fake stats. */
function scalarOrNaN(v) {
  if (v == null || v === "") return NaN;
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function imputeCountsFromNegMu(muSg) {
  let stp = -num(muSg, 0);
  stp = Math.max(-8, Math.min(8, stp));
  let eagles = Math.max(0, 0.15 - 0.02 * stp);
  let birdies = Math.max(0.5, 3.8 - 0.45 * stp);
  let bogeys = Math.max(0.5, 2.6 + 0.5 * stp);
  let doubles = Math.max(0.1, 0.35 + 0.05 * stp);
  let pars = Math.max(0.2, 18 - eagles - birdies - bogeys - doubles);
  const s = eagles + birdies + pars + bogeys + doubles;
  const k = 18 / s;
  return {
    eagles: eagles * k,
    birdies: birdies * k,
    pars: pars * k,
    bogeys: bogeys * k,
    doubles: doubles * k,
  };
}

/**
 * Nudge bird/bog (pars residual) toward `targetStp` without re-solving to a single narrow `pars` band for the field.
 * `strength` ∈ (0,1]: fraction of score-vs-par gap to close in one pass.
 */
function softAlignHoleCountsToStp(counts, targetStp, strength = 0.58) {
  const e = Math.max(0, num(counts.eagles, 0));
  const d = Math.max(0, num(counts.doubles, 0));
  let b = num(counts.birdies, 0);
  let p = num(counts.pars, 0);
  let bg = num(counts.bogeys, 0);
  const t = num(targetStp, 0);
  const st = Math.max(0.08, Math.min(1, strength));
  const hat = -b - 2 * e + bg + 2 * d;
  const diff = t - hat;
  const delta = (st * diff) / 2;
  b = Math.max(0.15, b - delta);
  bg = Math.max(0.15, bg + delta);
  p = 18 - e - d - b - bg;
  if (p < 0.12) {
    const need = 0.12 - p;
    const take = Math.min(need / 2, b - 0.15, bg - 0.15);
    b -= take;
    bg -= take;
    p = 18 - e - d - b - bg;
  }
  const s = e + b + p + bg + d;
  if (s > 0.01 && Math.abs(s - 18) > 0.01) {
    const k = 18 / s;
    return {
      eagles: e * k,
      birdies: b * k,
      pars: p * k,
      bogeys: bg * k,
      doubles: d * k,
    };
  }
  return { eagles: e, birdies: b, pars: Math.max(0.12, p), bogeys: bg, doubles: d };
}

function imputeCountsWithHistory(muSg, countFit) {
  const legacy = imputeCountsFromNegMu(muSg);
  const stp = -clampMuSg(muSg);
  const x = Math.max(-8, Math.min(8, stp));
  if (!countFit || countFit.n_counts < 800 || !countFit.slopes) return { ...legacy };

  /** Cap how much population OLS can override per-player legacy μ curves (large n else → one profile per μ). */
  const rawShrink = countFit.n_counts / (countFit.n_counts + 2000);
  const shrink = Math.min(0.38, rawShrink);
  /** Do not regress `pars` vs stp — it is nearly collinear with bird/bog in data and kills cross-player spread; derive from the other four after blend. */
  const keysNoPar = ["eagles", "birdies", "bogeys", "doubles"];
  /** @type {Record<string, number>} */
  const out = { pars: legacy.pars };
  for (const k of keysNoPar) {
    const c = countFit.slopes[k];
    if (!c || !Number.isFinite(c.a) || !Number.isFinite(c.b)) {
      out[k] = legacy[k];
      continue;
    }
    const pred = c.a + c.b * x;
    const lo = k === "eagles" || k === "doubles" ? 0.04 : 0.2;
    out[k] = shrink * pred + (1 - shrink) * legacy[k];
    out[k] = Math.max(lo, out[k]);
  }
  out.pars = Math.max(0.2, 18 - out.eagles - out.birdies - out.bogeys - out.doubles);
  let s = out.eagles + out.birdies + out.pars + out.bogeys + out.doubles;
  if (!(s > 0.1)) return { ...legacy };
  const kf = 18 / s;
  for (const k of ["eagles", "birdies", "pars", "bogeys", "doubles"]) out[k] *= kf;
  return {
    eagles: out.eagles,
    birdies: out.birdies,
    pars: out.pars,
    bogeys: out.bogeys,
    doubles: out.doubles,
  };
}

/**
 * Fairway hit rate 0–1 from skill row: (0,1] share, else count on this course’s n_fw, else percent 0–100.
 * `driving_accuracy` is tried before `driving_acc`.
 */
function fairwayRate01FromDrivingSkill(skRow, nFw = N_FAIRWAY_HOLES) {
  if (!skRow || typeof skRow !== "object") return NaN;
  const denom = Number.isFinite(nFw) && nFw > 0 ? nFw : N_FAIRWAY_HOLES;
  const cands = [num(skRow.driving_accuracy, NaN), num(skRow.driving_acc, NaN)].filter((x) => Number.isFinite(x));
  for (const a of cands) {
    if (a > 0 && a < 1) return a;
  }
  for (const a of cands) {
    if (a >= 0 && a <= denom) return a / denom;
  }
  for (const a of cands) {
    if (a > 1 && a <= 100) return a / 100;
  }
  return NaN;
}

function isPlausibleDrivingDistanceYds(y) {
  const v = num(y, NaN);
  return Number.isFinite(v) && v >= 235 && v <= 380;
}

/** Yards for modeling (FW): measured carry/roll when present, else neutral + DG yards-vs-tour rating. */
function impliedDrivingYardsFromSkillRow(sk) {
  if (!sk || typeof sk !== "object") return NaN;
  const y = num(sk.driving_distance, NaN);
  if (Number.isFinite(y) && isPlausibleDrivingDistanceYds(y)) return y;
  const rt = num(sk.driving_distance_rating, NaN);
  if (Number.isFinite(rt) && rt >= -55 && rt <= 55) return 302 + rt;
  return NaN;
}

/** Expected fairways in [0, n_fw]: cannot exceed driving holes or be negative (count stat, not a tuned model cap). */
function fairwayHitsExpectation(x, nFw) {
  if (!Number.isFinite(nFw) || nFw <= 0 || !Number.isFinite(x)) return NaN;
  if (x <= 0) return 0;
  if (x >= nFw) return nFw;
  return x;
}

/**
 * Tour FW vs strokes-to-par line, evaluated at skill proxy x ≈ −μ_sg, then nudged by OTT vs field and overall μ_sg
 * (population line alone sits low for elite drivers because x is compressed vs real round score − par).
 */
function fairwaysFromHistoricalStp(mu_sg, nFw, histCalib, fieldMeanOtt, skRow) {
  const ln = histCalib?.fw_stp_line;
  if (!ln || !Number.isFinite(ln.a) || !Number.isFinite(ln.b)) return NaN;
  const x = Math.max(-10, Math.min(10, -clampMuSg(mu_sg)));
  let raw = ln.a + ln.b * x;
  const mu = clampMuSg(mu_sg);
  raw += 0.48 * Math.max(0, Math.min(2.5, mu));
  const ott = num(skRow?.sg_ott, NaN);
  const fo = num(fieldMeanOtt, NaN);
  if (Number.isFinite(ott) && Number.isFinite(fo)) {
    const edge = Math.max(-0.45, Math.min(1.15, ott - fo));
    raw += 2.05 * edge;
  }
  return fairwayHitsExpectation(raw, nFw);
}

/**
 * Fairways: SG:OTT curve + historical tour regression vs skill proxy + optional driving-field rate.
 * OTT and driving-only can both sit ~6; historical regression anchors counts to real rounds.
 */
function projectedFairwaysFromSkillOnly(
  mu_sg,
  skRow,
  nFw,
  fieldMeanOtt,
  drivingDistYds,
  fieldMeanDrive,
  histCalib,
) {
  const ottFw = fairwaysExpectedFromSkill(mu_sg, skRow?.sg_ott, nFw, fieldMeanOtt, drivingDistYds, fieldMeanDrive);
  const histFw = fairwaysFromHistoricalStp(mu_sg, nFw, histCalib, fieldMeanOtt, skRow);
  const fw01 = fairwayRate01FromDrivingSkill(skRow, nFw);
  const fromDrv = Number.isFinite(fw01) ? fw01 * nFw : NaN;

  let y = ottFw;
  if (Number.isFinite(histFw)) {
    y = Number.isFinite(y) ? 0.07 * y + 0.93 * histFw : histFw;
  }
  if (!Number.isFinite(y)) {
    return Number.isFinite(fromDrv) ? fairwayHitsExpectation(fromDrv, nFw) : NaN;
  }
  if (Number.isFinite(fromDrv)) {
    const diff = Math.abs(fromDrv - y);
    if (diff > 1.65) return fairwayHitsExpectation(y, nFw);
    return fairwayHitsExpectation(0.28 * fromDrv + 0.72 * y, nFw);
  }
  return fairwayHitsExpectation(y, nFw);
}

/**
 * Re-fetching DataGolf overwrites projections.json with `props: []` unless we carry forward prior DK / CSV rows
 * for the same event week. Set `GOLF_RESET_PROPS=1` to force an empty props array.
 */
function tryPreservePropsFromDisk(outPath, eventName, courseUsed) {
  if (String(process.env.GOLF_RESET_PROPS || "").trim() === "1") return [];
  try {
    if (!existsSync(outPath)) return [];
    const prev = JSON.parse(readFileSync(outPath, "utf8"));
    if (!Array.isArray(prev.props) || !prev.props.length) return [];
    const wk = fieldWeekKey(eventName, courseUsed);
    const prevWk = String(prev.datagolf_field_week_key || "").trim();
    const sameWeek = Boolean(wk && prevWk && wk === prevWk);
    const sameEvent =
      eventsLikelySame(String(prev.event_name || "").trim(), String(eventName || "").trim()) &&
      !coursesClearlyDistinct(String(prev.course_used || "").trim(), String(courseUsed || "").trim());
    if (sameWeek || sameEvent) {
      console.log(`[fetch-dg] preserving ${prev.props.length} props from prior projections.json`);
      return prev.props;
    }
  } catch (e) {
    console.warn("[fetch-dg] could not merge prior props:", e?.message || e);
  }
  return [];
}

/**
 * Stream `data/historical_rounds_all.csv`: OLS of hole counts vs (round_score − course_par), and R² for GIR~APP / FW~OTT.
 * Used for count-curve calibration (historical R² still logged for diagnostics).
 */
async function loadHistoricalCsvCalibration(modelRoot) {
  const empty = {
    skipped: false,
    n_counts: 0,
    n_gir_app: 0,
    n_fw_ott: 0,
    n_fw_stp: 0,
    r2_gir_app: NaN,
    r2_fw_ott: NaN,
    slopes: null,
    fw_stp_line: null,
    w_gir_skill: 0.78,
    w_ott_skill: 0.85,
    w_ott_decomp: 0.65,
    csv_path: null,
  };
  if (String(process.env.GOLF_SKIP_HIST_STATS_ON_FETCH || "").trim() === "1") {
    return { ...empty, skipped: true };
  }
  const csvPath = join(modelRoot, "data", "historical_rounds_all.csv");
  if (!existsSync(csvPath)) return { ...empty, csv_path: csvPath };

  let n = 0;
  let sx = 0;
  let sx2 = 0;
  const sy = { eagles: 0, birdies: 0, pars: 0, bogeys: 0, doubles: 0 };
  const sxy = { eagles: 0, birdies: 0, pars: 0, bogeys: 0, doubles: 0 };

  let ng = 0;
  let sga = 0;
  let sg2a = 0;
  let sgG = 0;
  let g2 = 0;
  let sgAg = 0;

  let nf = 0;
  let sgo = 0;
  let sg2o = 0;
  let sgF = 0;
  let f2 = 0;
  let sgOf = 0;

  let nFwR = 0;
  let sxFw = 0;
  let sFwR = 0;
  let sxxFw = 0;
  let sfxFw = 0;

  await new Promise((resolve, reject) => {
    const parser = createReadStream(csvPath).pipe(
      parse({
        columns: true,
        relax_quotes: true,
        relax_column_count: true,
        skip_records_with_error: true,
      }),
    );
    parser.on("data", (row) => {
      const tour = String(row.tour || "").toLowerCase();
      if (tour !== "pga" && tour !== "liv") return;

      const cp = num(row.course_par, NaN);
      const rs = num(row.round_score, NaN);
      if (!Number.isFinite(cp) || cp < 63 || cp > 76) return;
      if (!Number.isFinite(rs) || rs < 55 || rs > 95) return;

      const e = num(row.eagles_or_better, NaN);
      const b = num(row.birdies, NaN);
      const p = num(row.pars, NaN);
      const bg = num(row.bogies, NaN);
      const d = num(row.doubles_or_worse, NaN);
      if (![e, b, p, bg, d].every((v) => Number.isFinite(v) && v >= 0 && v <= 18)) return;
      const sumH = e + b + p + bg + d;
      if (Math.abs(sumH - 18) > 0.51) return;

      const stpRaw = rs - cp;
      const x = Math.max(-10, Math.min(10, stpRaw));
      n++;
      sx += x;
      sx2 += x * x;
      sy.eagles += e;
      sy.birdies += b;
      sy.pars += p;
      sy.bogeys += bg;
      sy.doubles += d;
      sxy.eagles += x * e;
      sxy.birdies += x * b;
      sxy.pars += x * p;
      sxy.bogeys += x * bg;
      sxy.doubles += x * d;

      const sgApp = num(row.sg_app, NaN);
      const girR = num(row.gir, NaN);
      if (Number.isFinite(sgApp) && Number.isFinite(girR) && girR > 0.05 && girR < 0.995) {
        const gc = girR * 18;
        ng++;
        sga += sgApp;
        sg2a += sgApp * sgApp;
        sgG += gc;
        g2 += gc * gc;
        sgAg += sgApp * gc;
      }

      const sgOtt = num(row.sg_ott, NaN);
      const da = num(row.driving_acc, NaN);
      if (Number.isFinite(sgOtt) && Number.isFinite(da) && da > 0.05 && da < 0.995) {
        const fc = da * N_FAIRWAY_HOLES;
        nf++;
        sgo += sgOtt;
        sg2o += sgOtt * sgOtt;
        sgF += fc;
        f2 += fc * fc;
        sgOf += sgOtt * fc;
      }

      let fwCt = NaN;
      if (Number.isFinite(da)) {
        if (da > 0.05 && da < 0.995) fwCt = da * N_FAIRWAY_HOLES;
        else if (da > 1 && da <= N_FAIRWAY_HOLES) fwCt = da;
        else if (da > N_FAIRWAY_HOLES && da <= 100) fwCt = (da / 100) * N_FAIRWAY_HOLES;
      }
      if (Number.isFinite(fwCt) && fwCt >= 0 && fwCt <= N_FAIRWAY_HOLES + 0.01) {
        nFwR++;
        sxFw += x;
        sFwR += fwCt;
        sxxFw += x * x;
        sfxFw += x * fwCt;
      }
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });

  /** @type {typeof empty} */
  const out = { ...empty, csv_path: csvPath, n_counts: n, n_gir_app: ng, n_fw_ott: nf, n_fw_stp: nFwR };

  if (n >= 400) {
    const vx = sx2 - (sx * sx) / n;
    if (vx > 1e-6) {
      /** @type {Record<string, { a: number; b: number }>} */
      const slopes = {};
      for (const k of ["eagles", "birdies", "pars", "bogeys", "doubles"]) {
        const vy = sy[k];
        const cov = sxy[k] - (sx * vy) / n;
        const b = cov / vx;
        const a = vy / n - b * (sx / n);
        slopes[k] = { a, b };
      }
      out.slopes = slopes;
    }
  }

  if (ng >= 400) {
    const vxa = sg2a - (sga * sga) / ng;
    const vyg = g2 - (sgG * sgG) / ng;
    const cag = sgAg - (sga * sgG) / ng;
    if (vxa > 1e-8 && vyg > 1e-8) out.r2_gir_app = (cag * cag) / (vxa * vyg);
  }
  if (nf >= 400) {
    const vxo = sg2o - (sgo * sgo) / nf;
    const vyf = f2 - (sgF * sgF) / nf;
    const cof = sgOf - (sgo * sgF) / nf;
    if (vxo > 1e-8 && vyf > 1e-8) out.r2_fw_ott = (cof * cof) / (vxo * vyf);
  }

  if (nFwR >= 400) {
    const denom = nFwR * sxxFw - sxFw * sxFw;
    if (denom > 1e-6) {
      const bFw = (nFwR * sfxFw - sxFw * sFwR) / denom;
      const aFw = (sFwR - bFw * sxFw) / nFwR;
      if (Number.isFinite(aFw) && Number.isFinite(bFw) && Math.abs(bFw) < 0.65) {
        out.fw_stp_line = { a: aFw, b: bFw, n: nFwR };
      }
    }
  }

  out.w_gir_skill = 1;
  out.w_ott_skill = 1;
  out.w_ott_decomp = 1;

  if (n >= 400) {
    console.log(
      `[fetch-dg] historical calibration: n_counts=${n}, n_gir_app=${ng} (R²≈${Number.isFinite(out.r2_gir_app) ? out.r2_gir_app.toFixed(3) : "?"}), n_fw_ott=${nf} (R²≈${Number.isFinite(out.r2_fw_ott) ? out.r2_fw_ott.toFixed(3) : "?"}), n_fw_stp=${nFwR}${out.fw_stp_line ? " (FW~stp line fit)" : ""}; projections blend GIR/fairways vs historical`,
    );
  } else {
    console.log(
      `[fetch-dg] historical calibration: only ${n} scored rounds in CSV (need ≥400 for count regression / stable R²) — using legacy count curve`,
    );
  }

  return out;
}

function clampMuSg(m) {
  const x = num(m, 0);
  if (!Number.isFinite(x)) return 0;
  return Math.max(-4, Math.min(4, x));
}

/** Robust field center for SG pillars (mean is pulled by long left tail and jams elites on FW/GIR caps). */
function fieldSkillMedian(samples) {
  const a = (samples || []).filter((x) => Number.isFinite(x)).slice().sort((p, q) => p - q);
  if (a.length < 8) return NaN;
  const mid = Math.floor(a.length / 2);
  return a.length % 2 ? a[mid] : (a[mid - 1] + a[mid]) / 2;
}

/** Fairway opportunities = # of par-4 + par-5 holes (driving holes) when all 18 pars are valid 3–5. */
function fairwayHoleCountFromPars(pars, fallback = N_FAIRWAY_HOLES) {
  if (!Array.isArray(pars) || pars.length !== 18) return fallback;
  let n = 0;
  for (const p of pars) {
    const v = Math.round(num(p, NaN));
    if (!Number.isFinite(v) || v < 3 || v > 5) return fallback;
    if (v === 4 || v === 5) n++;
  }
  if (n < 1) return fallback;
  return n;
}

/** preds/pre-tournament baseline_history_fit: expected strokes this round for this course (column names vary). */
function pretExpectedStrokesThisRound(row) {
  if (!row || typeof row !== "object") return NaN;
  const c = firstNumCol(row, [
    "predicted_round_score",
    "predicted_score",
    "round_score",
    "avg_score",
    "average_score",
    "adjusted_round_score",
    "adj_round_score",
    "model_prediction",
    "pred_score",
  ]);
  if (!c) return NaN;
  const v = num(row[c], NaN);
  if (!Number.isFinite(v) || v < 54 || v > 95) return NaN;
  return v;
}

/** μ-only fairways fallback: linear expected count vs strokes-to-par on n_fw scale. */
function fairwaysMuImputeOnly(stpVec, nFw) {
  if (!Number.isFinite(nFw) || nFw <= 0) return NaN;
  return 0.55 * nFw - 0.15 * stpVec;
}

/**
 * Expected fairways (N_fw hole scale) from SG:OTT vs the field median + small total-SG tilt (μ-only fallback mixed in lightly).
 */
function fairwaysExpectedFromSkill(muSg, sgOtt, nFw, fieldMeanOtt, drivingDistYds, fieldMeanDrive) {
  const mu = clampMuSg(muSg);
  const stp = -mu;
  const fallback = fairwaysMuImputeOnly(stp, nFw);
  const o = num(sgOtt, NaN);
  const m = num(fieldMeanOtt, NaN);
  if (!Number.isFinite(o) || !Number.isFinite(m)) return fairwayHitsExpectation(fallback, nFw);
  let rate = 0.56 + 0.72 * (o - m) + 0.08 * mu;
  let ottFw = rate * nFw;
  if (Number.isFinite(fallback)) ottFw = 0.02 * fallback + 0.98 * ottFw;
  const dy = num(drivingDistYds, NaN);
  const my = num(fieldMeanDrive, NaN);
  if (Number.isFinite(dy) && Number.isFinite(my) && dy >= 240 && dy <= 345 && my >= 265 && my <= 315) {
    ottFw += -0.021 * (dy - my);
  }
  return fairwayHitsExpectation(ottFw, nFw);
}

/** Expected GIR count (18-hole scale) from SG:APP vs field mean + small total-SG tilt (mirrors FW/OTT path). */
function girExpectedFromSkill(muSg, sgApp, nGirHoles, fieldMeanApp) {
  const mu = clampMuSg(muSg);
  const stp = -mu;
  const fallback = Math.max(6, Math.min(16, 11.5 - 0.25 * stp));
  const a = num(sgApp, NaN);
  const m = num(fieldMeanApp, NaN);
  if (!Number.isFinite(a) || !Number.isFinite(m)) return fallback;
  let rate = 0.6 + 0.34 * (a - m) + 0.04 * mu;
  rate = Math.max(0.48, Math.min(0.82, rate));
  const appGir = Math.max(6, Math.min(16, rate * nGirHoles));
  return Math.max(6, Math.min(16, 0.14 * fallback + 0.86 * appGir));
}

/** Default widens R2–R4 vs R1 so per-round projections separate (override GOLF_NODE_ROUND_MU_MULT). */
function parseRoundMuMult() {
  const def = [1, 0.945, 0.885, 0.82];
  const raw = process.env.GOLF_NODE_ROUND_MU_MULT;
  if (raw == null || !String(raw).trim()) return def;
  const parts = String(raw)
    .split(",")
    .map((s) => num(s.trim(), NaN));
  if (parts.length < 4 || parts.some((p) => !Number.isFinite(p))) return def;
  return parts.slice(0, 4);
}

function derivedStatsFromMuSg(muRaw, nFairwayHoles, opts = {}) {
  const mu_sg = clampMuSg(muRaw);
  const targetStp = -mu_sg;
  let im = imputeCountsWithHistory(mu_sg, opts.histCountFit);
  im = softAlignHoleCountsToStp(im, targetStp);
  const stpVec = -mu_sg;
  const nGir = Number.isFinite(opts.nGirHoles) ? opts.nGirHoles : 18;
  const skR = opts.skRow;
  let gir = Math.max(6, Math.min(16, 11.5 - 0.25 * stpVec));
  if (Number.isFinite(opts.sg_app) && Number.isFinite(opts.fieldMeanApp)) {
    gir = girExpectedFromSkill(mu_sg, opts.sg_app, nGir, opts.fieldMeanApp);
  }
  const distFw = isPlausibleDrivingDistanceYds(opts.driving_distance) ? opts.driving_distance : NaN;
  const fairways = projectedFairwaysFromSkillOnly(
    mu_sg,
    skR,
    nFairwayHoles,
    opts.fieldMeanOtt,
    distFw,
    opts.fieldMeanDrive,
    opts.histCountFit,
  );
  const putts = Math.max(22, Math.min(35, 28.5 + 0.32 * stpVec - 0.1 * (gir - 11)));
  return {
    mu_sg,
    implied_mu_sg: mu_sg,
    eagles: im.eagles,
    birdies: im.birdies,
    pars: im.pars,
    bogeys: im.bogeys,
    doubles: im.doubles,
    gir,
    fairways,
    putts,
  };
}

/** Lowercase / hyphen-normalized keys + compact (no underscore) aliases for column lookup. */
function normalizedScalarBag(row) {
  /** @type {Record<string, unknown>} */
  const bag = Object.create(null);
  if (!row || typeof row !== "object") return bag;
  for (const [k0, v] of Object.entries(row)) {
    const k = String(k0).trim().toLowerCase().replace(/-/g, "_");
    if (bag[k] == null) bag[k] = v;
    const compact = k.replace(/_/g, "");
    if (compact && bag[compact] == null) bag[compact] = v;
  }
  return bag;
}

function pickFromBag(bag, keys) {
  for (const key of keys) {
    const lk = String(key).trim().toLowerCase().replace(/-/g, "_");
    for (const cand of [lk, lk.replace(/_/g, "")]) {
      if (!cand || !(cand in bag) || bag[cand] == null) continue;
      const v = num(bag[cand], NaN);
      if (Number.isFinite(v)) return v;
    }
  }
  return NaN;
}

/**
 * Strokes-gained pillars from preds/skill-ratings merged with preds/player-decompositions (same dg_id).
 * Both endpoints may use different column names; we resolve via aliases + normalized keys.
 */
function skillPillarsFromSkillRow(row) {
  if (!row || typeof row !== "object") return null;
  const bag = normalizedScalarBag(row);
  const pick = (keys) => pickFromBag(bag, keys);

  const sg_total = pick(["sg_total", "total", "overall", "strokes_gained_total", "total_sg", "true_sg_total"]);
  let sg_ott = pick([
    "sg_ott",
    "ott",
    "off_the_tee",
    "off_the_tee_sg",
    "strokes_gained_ott",
    "sg_ot",
    "sg_off_the_tee",
    "true_sg_ott",
  ]);
  let sg_app = pick([
    "sg_app",
    "app",
    "approach",
    "strokes_gained_app",
    "sg_approach",
    "true_sg_app",
  ]);
  let sg_arg = pick([
    "sg_arg",
    "arg",
    "around_the_green",
    "around_green",
    "strokes_gained_arg",
    "sg_around_the_green",
    "true_sg_arg",
  ]);
  let sg_putt = pick([
    "sg_putt",
    "putt",
    "putting",
    "strokes_gained_putt",
    "sg_putting",
    "true_sg_putt",
  ]);
  let sg_t2g = pick(["sg_t2g", "t2g", "strokes_gained_t2g", "sg_teetogreen", "true_sg_t2g"]);
  if (!Number.isFinite(sg_t2g) && [sg_ott, sg_app, sg_arg].every(Number.isFinite)) {
    sg_t2g = sg_ott + sg_app + sg_arg;
  }
  return {
    sg_total,
    sg_ott,
    sg_app,
    sg_arg,
    sg_putt,
    sg_t2g,
  };
}

/** Driving distance + accuracy from preds/skill-ratings (+ decompositions). Distance may be absolute yards (≈235–380) or DG "yards vs tour average" (≈±50). */
function drivingAttrsFromSkillBag(row) {
  const empty = {
    driving_distance: NaN,
    driving_accuracy: NaN,
    driving_dist: NaN,
    driving_acc: NaN,
    driving_distance_rating: NaN,
  };
  if (!row || typeof row !== "object") return empty;
  const bag = normalizedScalarBag(row);
  const distRatingOnly = pickFromBag(bag, [
    "driving_distance_rating",
    "driving_distance_skill",
    "driving_dist_skill",
    "distance_skill",
    "distance_vs_avg",
    "driving_distance_vs_avg",
    "dd_skill",
  ]);
  const dist = pickFromBag(bag, [
    "avg_driving_distance",
    "average_driving_distance",
    "mean_driving_distance",
    "avg_drive_distance",
    "avg_drive_dist",
    "driving_dist",
    "predicted_driving_distance",
    "predicted_avg_driving_distance",
    "driving_distance",
    "drive_distance",
    "distance",
    "dd",
    "ott_distance",
  ]);
  let acc = pickFromBag(bag, [
    "driving_accuracy",
    "driving_acc",
    "fairway_pct",
    "fw_pct",
    "fairways_hit_pct",
    "driving_accuracy_pct",
    "accuracy_ot",
    "accuracy_off_the_tee",
    "predicted_fw_pct",
    "fairway_accuracy",
    "fw_accuracy",
  ]);
  const accRating = Number.isFinite(acc) && acc > 0 && acc <= 1 ? acc * 100 : acc;
  let driving_distance = NaN;
  let driving_dist = NaN;
  let driving_distance_rating = NaN;
  if (Number.isFinite(distRatingOnly) && distRatingOnly >= -55 && distRatingOnly <= 55) {
    driving_distance_rating = distRatingOnly;
  }
  if (Number.isFinite(dist)) {
    if (isPlausibleDrivingDistanceYds(dist)) {
      driving_distance = dist;
      driving_dist = dist;
    } else if (!Number.isFinite(driving_distance_rating) && dist >= -55 && dist <= 55) {
      driving_distance_rating = dist;
    }
  }
  return {
    driving_distance,
    driving_accuracy: accRating,
    driving_dist,
    driving_acc: accRating,
    driving_distance_rating,
  };
}

function mergeSkillDrivingProfile(mergedRow) {
  const pillars = skillPillarsFromSkillRow(mergedRow) || {};
  const drv = drivingAttrsFromSkillBag(mergedRow);
  return { ...pillars, ...drv };
}

/**
 * Normalize preds/pre-tournament placement fields to implied probability (0–1).
 * odds_format from API: percent (default), decimal (fair odds 1/p), american (+/−).
 */
function normProb01(v, oddsFormat = "percent") {
  const x = num(v, NaN);
  if (!Number.isFinite(x)) return NaN;
  const fmt = String(oddsFormat || "percent").toLowerCase();
  if (fmt === "decimal") {
    if (x > 1 && x < 2000) return 1 / x;
    if (x > 0 && x <= 1) return x;
    return NaN;
  }
  if (fmt === "american") {
    if (x > 0) return 100 / (x + 100);
    if (x < 0) return Math.abs(x) / (Math.abs(x) + 100);
    return NaN;
  }
  if (x > 1.5) return x / 100;
  return x;
}

function ouDisplayRoundAuto(now = new Date(), tz = "America/New_York") {
  const fmt = new Intl.DateTimeFormat("en-US", {
    timeZone: tz,
    weekday: "short",
    hour: "numeric",
    minute: "numeric",
    second: "numeric",
    hour12: false,
  });
  const parts = fmt.formatToParts(now);
  const getNum = (t) => parseInt(parts.find((p) => p.type === t)?.value, 10);
  const wdayStr = parts.find((p) => p.type === "weekday")?.value;
  const map = { Sun: 0, Mon: 1, Tue: 2, Wed: 3, Thu: 4, Fri: 5, Sat: 6 };
  const wday = map[wdayStr] ?? 0;
  const hourDec = getNum("hour") + getNum("minute") / 60 + getNum("second") / 3600;
  const after9pm = hourDec >= 21;
  if (wday === 0 && after9pm) return 1;
  if (wday >= 1 && wday <= 3) return 1;
  if (wday === 4 && !after9pm) return 1;
  if (wday === 4 && after9pm) return 2;
  if (wday === 5 && !after9pm) return 2;
  if (wday === 5 && after9pm) return 3;
  if (wday === 6 && !after9pm) return 3;
  if (wday === 6 && after9pm) return 4;
  if (wday === 0 && !after9pm) return 4;
  return 1;
}

function displayRoundLabel(r, tz) {
  const lab =
    r === 1 ? "R1 — next Thursday" : r === 2 ? "R2 — Friday" : r === 3 ? "R3 — Saturday" : r === 4 ? "R4 — Sunday" : `R${r}`;
  return `${lab} (auto, ${tz})`;
}

async function main() {
  const buildShotsWebOnFetch = process.env.GOLF_BUILD_SHOTS_WEB_ON_FETCH === "1";

  const key = loadApiKey();
  if (!key) {
    console.error(
      "Missing API key. Set DATAGOLF_API_KEY or create datagolf.local.json (see datagolf.local.example.json)."
    );
    process.exit(1);
  }

  /** Runs in parallel with DataGolf fetches — stream PGA/LIV historical CSV for count vs (score−par) and blend R². */
  const histCalibPromise = loadHistoricalCsvCalibration(GOLF_MODEL_ROOT);

  /** Dual-field weeks: `pga` feed may lag while `opp` already shows this week's opposite-field event. */
  function fieldUpdateTourCandidates() {
    const raw = String(process.env.GOLF_FIELD_UPDATES_TOUR_CANDIDATES || "").trim();
    if (raw) {
      const xs = [...new Set(raw.split(/[,;\s]+/).map((x) => x.trim().toLowerCase()).filter(Boolean))];
      return xs.length ? xs : [TOUR];
    }
    /** Prefer `pga` before `opp` so timestamp ties favor the main tour field (opp can be another tour/week with a fresher clock). */
    return TOUR === "pga" ? ["pga", "opp"] : [TOUR];
  }

  console.log("Fetching field-updates (comparing tours)…");
  const toursTry = fieldUpdateTourCandidates();
  const minPlayers = Math.max(8, Number(process.env.GOLF_FIELD_UPDATES_MIN_PLAYERS || "30"));
  const scored = [];
  for (const tour of toursTry) {
    try {
      const raw = await fetchDg("/field-updates", { tour, file_format: "json" }, key);
      const fieldRowsTry = buildFieldRowsFromFieldRaw(raw);
      const lu = raw.last_updated ?? raw.last_update ?? raw.updated_at;
      const ts = parseDgTimestamp(lu);
      const ev = String(raw.event_name || raw.eventName || "").trim();
      scored.push({ tour, raw, fieldRowsTry, ts, n: fieldRowsTry.length, ev });
    } catch (e) {
      console.warn(`field-updates tour=${tour}:`, e.message || e);
    }
  }
  const viable = scored.filter((s) => s.n >= minPlayers);
  const pool = viable.length ? viable : scored.filter((s) => s.n > 0);
  if (!pool.length) {
    console.error("No field-updates from tours:", toursTry.join(", "));
    process.exit(1);
  }

  const skipSchedule =
    String(process.env.GOLF_SKIP_GET_SCHEDULE_FIELD_ANCHOR || "").trim() === "1";
  let anchor = null;
  if (!skipSchedule) {
    anchor = await fetchCanonicalScheduleAnchor(TOUR, key);
    if (anchor) {
      console.log(`[get-schedule] canonical upcoming: "${anchor.name}" (upcoming_only=${anchor.upcoming_only})`);
    } else {
      console.warn("[get-schedule] could not resolve upcoming event — preds/in-play fallback may decide.");
    }
  }

  let poolUse = pool;
  let narrowedBySchedule = false;
  if (!skipSchedule && anchor?.name) {
    const matched = pool.filter((s) => fieldCandidateMatchesSchedule(s.ev, anchor.name));
    if (matched.length) {
      poolUse = matched;
      narrowedBySchedule = true;
      console.log("[field-updates] schedule anchor matched:", matched.map((m) => `${m.tour}("${m.ev}")`).join(", "));
    } else {
      console.warn(
        "[field-updates] schedule anchor matched none — candidates:",
        pool.map((s) => `${s.tour}:${s.ev}`).join(" | ")
      );
    }
  }

  if (!narrowedBySchedule) {
    const ipLabels = Object.fromEntries(
      await Promise.all(toursTry.map(async (t) => [t, await fetchInPlayEventLabel(t, key)]))
    );
    for (const t of toursTry) {
      const ip = ipLabels[t];
      if (ip?.nm) console.log(`[preds/in-play] tour=${t} info="${ip.nm}" rows=${ip.rows}`);
    }
    const matchedIp = pool.filter((s) => {
      const ip = ipLabels[s.tour];
      return ip?.nm && fieldCandidateMatchesSchedule(s.ev, ip.nm);
    });
    if (matchedIp.length) {
      poolUse = matchedIp;
      console.log("[field-updates] preds/in-play anchor matched:", matchedIp.map((m) => `${m.tour}("${m.ev}")`).join(", "));
    } else if (!narrowedBySchedule) {
      console.warn(
        "[field-updates] preds/in-play matched none — picking field-updates by timestamp/order; tours:",
        pool.map((s) => `${s.tour}:${s.ev}`).join(" | ")
      );
    }
  }

  /* Timestamp-only picks can grab the wrong tour (e.g. LIV / opposite-field with newer last_updated).
   * If get-schedule anchored an event and any viable feed matches it, restrict to those feeds before sorting. */
  if (!skipSchedule && anchor?.name) {
    const anchorHits = pool.filter((s) => fieldCandidateMatchesSchedule(s.ev, anchor.name));
    if (anchorHits.length) {
      poolUse = anchorHits;
      console.log(
        "[field-updates] final schedule-anchor pool:",
        anchorHits.map((m) => `${m.tour}("${m.ev}", ts=${m.ts})`).join(", ")
      );
    }
  }

  poolUse.sort((a, b) => {
    if (b.ts !== a.ts) return b.ts - a.ts;
    return toursTry.indexOf(a.tour) - toursTry.indexOf(b.tour);
  });
  const win = poolUse[0];
  const tourForFeeds = win.tour;
  const fieldRaw = win.raw;
  const fieldRows = win.fieldRowsTry;
  let event_name = String(fieldRaw.event_name || fieldRaw.eventName || "").trim();
  let course_used = String(fieldRaw.course_name || fieldRaw.courseName || fieldRaw.course || "").trim();
  /** Before preds/pre-tournament may relabel `course_used`, remember field-updates’ course for hole-par validation. */
  const field_updates_course_used = course_used;
  if (!skipSchedule && anchor?.name && fieldCandidateMatchesSchedule(event_name, anchor.name)) {
    event_name = anchor.name;
  }

  console.log(
    `[field-updates] chose tour=${tourForFeeds} "${event_name || "(unnamed)"}" (${fieldRows.length} players, ts=${win.ts}); scanned:`,
    scored.map((s) => `${s.tour}:${s.n}`).join(", ")
  );

  const byDg = new Map(fieldRows.map((r) => [r.dg_id, r]));

  console.log("Fetching player-decompositions (SG pillars)…");
  /** @type {Map<number, object>} */
  const decompByDgRaw = new Map();
  async function fetchPlayerDecompositions(tourCode) {
    return fetchDg("/preds/player-decompositions", { tour: tourCode, display: "value", file_format: "json" }, key);
  }
  try {
    let decompJson = await fetchPlayerDecompositions(tourForFeeds);
    let decompList = rowsFromResponse(decompJson);
    if (!decompList.length && tourForFeeds && String(tourForFeeds).toLowerCase() !== "pga") {
      decompJson = await fetchPlayerDecompositions("pga");
      decompList = rowsFromResponse(decompJson);
    }
    for (const row of decompList) {
      const id = num(row.dg_id ?? row.dgId, NaN);
      if (!Number.isFinite(id)) continue;
      decompByDgRaw.set(Math.round(id), row);
    }
    console.log(`player-decompositions: ${decompByDgRaw.size} player rows`);
  } catch (e) {
    console.warn("player-decompositions skipped — pillar SG may only come from skill-ratings:", e.message || e);
  }

  console.log("Fetching skill-ratings…");
  let skillJson = {};
  try {
    skillJson = await fetchDg("/preds/skill-ratings", { display: "value", file_format: "json" }, key);
  } catch (e) {
    console.warn("skill-ratings failed — continuing with mu_sg≈0 fallback so projections.json still refresh:", e.message || e);
  }
  const skillList = rowsFromResponse(skillJson);
  const skillByDg = new Map();
  for (const row of skillList) {
    const id = num(row.dg_id ?? row.dgId, NaN);
    if (!Number.isFinite(id)) continue;
    const rid = Math.round(id);
    const merged = { ...(decompByDgRaw.get(rid) || {}), ...row };
    skillByDg.set(rid, mergeSkillDrivingProfile(merged));
  }
  for (const fr of fieldRows) {
    const fid = Math.round(num(fr.dg_id, NaN));
    if (!Number.isFinite(fid) || skillByDg.has(fid)) continue;
    const rawOnly = decompByDgRaw.get(fid);
    if (rawOnly) skillByDg.set(fid, mergeSkillDrivingProfile(rawOnly));
  }

  console.log("Fetching preds/approach-skill (YTD → approach_skill_ytd.json for Course Fit shot bins)…");
  try {
    const asJson = await fetchDg("/preds/approach-skill", { period: "ytd", file_format: "json" }, key);
    const asList = rowsFromResponse(asJson);
    const slimPlayers = [];
    for (const row of asList) {
      const id = Math.round(num(row.dg_id, NaN));
      if (!Number.isFinite(id)) continue;
      slimPlayers.push(slimApproachSkillPlayerRow(row));
    }
    const approachPayload = {
      period: String(asJson.time_period || "ytd"),
      last_updated: asJson.last_updated ? String(asJson.last_updated) : new Date().toISOString(),
      players: slimPlayers,
    };
    writeFileSync(join(ROOT, "approach_skill_ytd.json"), JSON.stringify(approachPayload, null, 2), "utf8");
    console.log(`approach-skill: ${slimPlayers.length} players -> approach_skill_ytd.json`);
  } catch (e) {
    console.warn(
      "approach-skill skipped — Course Fit shot table needs `npm run fetch:dg` with a valid key:",
      e.message || e,
    );
  }

  /** event_avg driving distance (yards) + accuracy from preds/live-tournament-stats — matches DG Live Stats feed. */
  let liveDrivingByDg = new Map();
  try {
    const statsParam =
      String(process.env.GOLF_PROJECTIONS_MERGE_LIVE_STATS || "distance,accuracy").trim() || "distance,accuracy";
    const roundParam = String(process.env.GOLF_LIVE_TOURNAMENT_STATS_ROUND || "event_avg").trim() || "event_avg";
    const liveTsJson = await fetchDg(
      "/preds/live-tournament-stats",
      { stats: statsParam, round: roundParam, display: "value", file_format: "json" },
      key,
    );
    const lst = Array.isArray(liveTsJson?.live_stats)
      ? liveTsJson.live_stats
      : rowsFromResponse(liveTsJson);
    const evLive = String(liveTsJson?.event_name ?? liveTsJson?.eventName ?? "").trim();
    let overlapOk = false;
    if (lst.length && fieldRows.length) {
      const liveIds = new Set();
      for (const row of lst) {
        const lid = Math.round(num(row.dg_id ?? row.dgId, NaN));
        if (Number.isFinite(lid)) liveIds.add(lid);
      }
      let inter = 0;
      for (const fr of fieldRows) {
        if (liveIds.has(fr.dg_id)) inter++;
      }
      overlapOk = inter / fieldRows.length >= 0.72;
    }
    const titlesAlign =
      !evLive ||
      !event_name ||
      foldComparableTitle(evLive) === foldComparableTitle(event_name) ||
      eventsLikelySame(evLive, event_name);
    if (lst.length && (titlesAlign || overlapOk)) {
      for (const row of lst) {
        const pid = Math.round(num(row.dg_id ?? row.dgId, NaN));
        if (!Number.isFinite(pid)) continue;
        const dist = num(row.distance, NaN);
        let acc = num(row.accuracy, NaN);
        if (Number.isFinite(acc) && acc > 0 && acc <= 1) acc *= 100;
        liveDrivingByDg.set(pid, { distance: dist, accuracy: acc });
      }
      console.log(
        `live-tournament-stats: merged driving for ${liveDrivingByDg.size} players (round=${roundParam}${titlesAlign ? "" : ", title mismatch but field overlap OK"})`,
      );
    } else if (lst.length && evLive && event_name && !titlesAlign && !overlapOk) {
      console.warn(`live-tournament-stats event "${evLive}" vs field "${event_name}" — skip driving merge`);
    }
  } catch (e) {
    console.warn("live-tournament-stats merge skipped:", e.message || e);
  }

  const pretDeadHeat = (process.env.GOLF_PRE_TOURNAMENT_DEAD_HEAT || "yes").trim().toLowerCase();
  const pretOddsFormat = (process.env.GOLF_PRE_TOURNAMENT_ODDS_FORMAT || "decimal").trim().toLowerCase();
  const pretAddPos = (process.env.GOLF_PRE_TOURNAMENT_ADD_POSITION || "").trim();
  console.log("Fetching preds/pre-tournament…");
  let pretRaw = null;
  let pretList = [];
  try {
    const pretParams = {
      tour: tourForFeeds,
      dead_heat: pretDeadHeat === "no" ? "no" : "yes",
      odds_format: pretOddsFormat,
      file_format: "json",
    };
    if (pretAddPos) pretParams.add_position = pretAddPos;
    pretRaw = await fetchDg("/preds/pre-tournament", pretParams, key);
    pretList = asArray(pretRaw.baseline_history_fit).length
      ? asArray(pretRaw.baseline_history_fit)
      : asArray(pretRaw.baseline).length
        ? asArray(pretRaw.baseline)
        : rowsFromResponse(pretRaw);
  } catch (e) {
    console.warn("Pre-tournament skipped:", e.message);
  }

  /**
   * field-updates labels sometimes lag preds/pre-tournament; align titles when baseline covers the same field.
   * Never replace field-updates names from pret when titles/course disagree — high dg_id overlap alone can occur across
   * wrong-week or mismatched API metadata (e.g. stale pret event_name vs correct field-updates week).
   */
  const alignTitleFromPret =
    String(process.env.GOLF_ALIGN_EVENT_TITLE_FROM_PRETOURNAMENT || "0").trim() === "1";
  if (alignTitleFromPret && pretRaw && typeof pretRaw === "object" && pretList.length && fieldRows.length) {
    const row0 = pretList[0] && typeof pretList[0] === "object" ? pretList[0] : {};
    const pretEvt = String(
      pretRaw.event_name ||
        pretRaw.event ||
        pretRaw.Event ||
        row0.event_name ||
        row0.tournament_name ||
        row0.event ||
        "",
    ).trim();
    const pretCrs = String(
      pretRaw.course_name ||
        pretRaw.course ||
        pretRaw.course_used ||
        row0.course_name ||
        row0.course ||
        "",
    ).trim();
    const pretIds = new Set();
    for (const row of pretList) {
      const id = Math.round(num(row.dg_id ?? row.id ?? row.dgId, NaN));
      if (Number.isFinite(id)) pretIds.add(id);
    }
    let inter = 0;
    for (const fr of fieldRows) {
      if (pretIds.has(fr.dg_id)) inter++;
    }
    const cov = inter / fieldRows.length;
    const titlesComparable =
      pretEvt &&
      event_name &&
      (foldComparableTitle(pretEvt) === foldComparableTitle(event_name) ||
        eventsLikelySame(pretEvt, event_name) ||
        titleTokenOverlapRatio(pretEvt, event_name) >= 0.55);
    if (cov >= 0.92 && pretEvt && titlesComparable) {
      if (foldComparableTitle(pretEvt) !== foldComparableTitle(event_name)) {
        console.warn(
          `Using preds/pre-tournament event (${(cov * 100).toFixed(0)}% field dg_ids in baseline): "${pretEvt}" (field-updates had "${event_name}")`,
        );
        event_name = pretEvt;
      }
      if (
        pretCrs &&
        course_used &&
        foldComparableTitle(pretCrs) !== foldComparableTitle(course_used) &&
        !coursesClearlyDistinct(pretCrs, course_used)
      ) {
        console.warn(
          `Using preds/pre-tournament course (${(cov * 100).toFixed(0)}% overlap): "${pretCrs}" (field-updates had "${course_used}")`,
        );
        course_used = pretCrs;
      }
    } else if (cov >= 0.92 && pretEvt && foldComparableTitle(pretEvt) !== foldComparableTitle(event_name)) {
      console.warn(
        `Skipping preds/pre-tournament title "${pretEvt}" vs field "${event_name}" (${(cov * 100).toFixed(
          0,
        )}% dg overlap) — titles not comparable; keeping field-updates labels.`,
      );
    }
  }

  const pretByDg = new Map();
  for (const row of pretList) {
    const id = num(row.dg_id ?? row.id ?? row.dgId, NaN);
    if (!Number.isFinite(id)) continue;
    pretByDg.set(Math.round(id), {
      win: normProb01(row.win, pretOddsFormat),
      top_5: normProb01(row.top_5, pretOddsFormat),
      top_10: normProb01(row.top_10, pretOddsFormat),
      top_20: normProb01(row.top_20, pretOddsFormat),
      make_cut: normProb01(row.make_cut, pretOddsFormat),
    });
  }

  let liveHoleStatsForPars = null;
  if (String(process.env.GOLF_SKIP_LIVE_HOLE_STATS_HOLE_PARS || "").trim() !== "1") {
    try {
      console.log("Fetching preds/live-hole-stats (hole pars + course par for projections)…");
      liveHoleStatsForPars = await fetchDg(
        "/preds/live-hole-stats",
        { tour: tourForFeeds, file_format: "json" },
        key,
      );
    } catch (e) {
      console.warn("preds/live-hole-stats skipped:", e.message || e);
    }
  }

  const holeRes = resolveHoleParsForEvent({
    fieldRaw,
    course_used,
    event_name,
    field_updates_course_used,
    liveHoleStats: liveHoleStatsForPars,
  });
  const hole_pars = holeRes.pars.map((x) => Math.round(num(x, 4)));
  const course_par_18 =
    hole_pars.length === 18 ? hole_pars.reduce((sum, p) => sum + Math.round(num(p, 4)), 0) : COURSE_PAR_18;
  const fairwayHolesThisCourse = fairwayHoleCountFromPars(hole_pars, N_FAIRWAY_HOLES);
  const hole_pars_source = holeRes.source;
  if (hole_pars_source === "generic") {
    console.warn(
      "Hole pars: no course/event match in course_holes*.json or CSV — using generic layout; add course_holes.local.json for this venue.",
    );
  } else {
    console.log(
      `Hole pars: ${hole_pars_source}${holeRes.detail ? ` (${holeRes.detail})` : ""} — course_par_18=${course_par_18}, fairway hole scale=${fairwayHolesThisCourse}`,
    );
  }

  course_used = canonicalCourseLabelForProjections(course_used);

  const pretStrokesByDg = new Map();
  for (const row of pretList) {
    const pid = Math.round(num(row.dg_id ?? row.id ?? row.dgId, NaN));
    if (!Number.isFinite(pid)) continue;
    const ex = pretExpectedStrokesThisRound(row);
    if (Number.isFinite(ex)) pretStrokesByDg.set(pid, ex);
  }
  if (pretStrokesByDg.size >= Math.min(40, Math.max(12, Math.floor(fieldRows.length * 0.25)))) {
    console.log(
      `[fetch-dg] preds/pre-tournament expected round strokes for ${pretStrokesByDg.size}/${fieldRows.length} players (course-aware μ_sg nudge)`,
    );
  } else if (pretList.length) {
    console.log(
      `[fetch-dg] preds/pre-tournament: ${pretList.length} rows — placement-only baseline (no per-round stroke column); μ_sg uses skill ratings.`,
    );
  }

  const ottSamples = [];
  for (const fr of fieldRows) {
    const sid = Math.round(num(fr.dg_id, NaN));
    if (!Number.isFinite(sid)) continue;
    const sk = skillByDg.get(sid);
    const o = num(sk?.sg_ott, NaN);
    if (Number.isFinite(o)) ottSamples.push(o);
  }
  /** Median SG:OTT in this field (mean is too low vs elite cluster and pins FW rate at the cap). */
  const fieldMeanOtt = fieldSkillMedian(ottSamples);

  const appSamples = [];
  for (const fr of fieldRows) {
    const sid = Math.round(num(fr.dg_id, NaN));
    if (!Number.isFinite(sid)) continue;
    const sk = skillByDg.get(sid);
    const a = num(sk?.sg_app, NaN);
    if (Number.isFinite(a)) appSamples.push(a);
  }
  /** Median SG:APP in this field (same robustness as OTT for GIR rate). */
  const fieldMeanApp = fieldSkillMedian(appSamples);

  const distSamples = [];
  for (const fr of fieldRows) {
    const sid = Math.round(num(fr.dg_id, NaN));
    if (!Number.isFinite(sid)) continue;
    const sk = skillByDg.get(sid);
    const d = impliedDrivingYardsFromSkillRow(sk);
    if (Number.isFinite(d) && d >= 240 && d <= 345) distSamples.push(d);
  }
  const fieldMeanDrive =
    distSamples.length >= 8 ? distSamples.reduce((a, b) => a + b, 0) / distSamples.length : NaN;

  const histCalib = await histCalibPromise;

  const base = [];
  for (const fr of fieldRows) {
    const id = fr.dg_id;
    const skRow = skillByDg.get(id);
    let mu_sg = skRow && Number.isFinite(skRow.sg_total) ? skRow.sg_total : 0;
    if (!Number.isFinite(mu_sg)) mu_sg = 0;

    if (pretStrokesByDg.size) {
      const dgS = pretStrokesByDg.get(id);
      if (Number.isFinite(dgS) && Number.isFinite(course_par_18)) {
        const modelS = course_par_18 - mu_sg;
        if (Number.isFinite(modelS)) mu_sg = clampMuSg(mu_sg - 0.22 * (dgS - modelS));
      }
    }

    const liveDv = liveDrivingByDg.get(id);
    let driving_distance =
      skRow && Number.isFinite(skRow.driving_distance) && isPlausibleDrivingDistanceYds(skRow.driving_distance)
        ? skRow.driving_distance
        : liveDv && Number.isFinite(liveDv.distance) && isPlausibleDrivingDistanceYds(liveDv.distance)
          ? liveDv.distance
          : NaN;
    let driving_accuracy =
      skRow && Number.isFinite(skRow.driving_accuracy)
        ? skRow.driving_accuracy
        : liveDv && Number.isFinite(liveDv.accuracy)
          ? liveDv.accuracy
          : NaN;

    const im = imputeCountsWithHistory(mu_sg, histCalib);
    let eagles = im.eagles;
    let birdies = im.birdies;
    let pars = im.pars;
    let bogeys = im.bogeys;
    let doubles = im.doubles;
    const alignedCounts = softAlignHoleCountsToStp({ eagles, birdies, pars, bogeys, doubles }, -mu_sg);
    eagles = alignedCounts.eagles;
    birdies = alignedCounts.birdies;
    pars = alignedCounts.pars;
    bogeys = alignedCounts.bogeys;
    doubles = alignedCounts.doubles;

    const stpVec = -mu_sg;
    const gir = girExpectedFromSkill(mu_sg, skRow?.sg_app, 18, fieldMeanApp);

    const distForFw = isPlausibleDrivingDistanceYds(driving_distance)
      ? driving_distance
      : impliedDrivingYardsFromSkillRow(skRow);
    const fairways = projectedFairwaysFromSkillOnly(
      mu_sg,
      skRow,
      fairwayHolesThisCourse,
      fieldMeanOtt,
      distForFw,
      fieldMeanDrive,
      histCalib,
    );

    const putts = Math.max(22, Math.min(35, 28.5 + 0.32 * stpVec - 0.1 * (gir - 11)));

    const pt = pretByDg.get(id) || {};
    const rowOut = {
      dg_id: id,
      player_name: fr.player_name,
      country: fr.country || undefined,
      mu_sg,
      implied_mu_sg: mu_sg,
      eagles,
      birdies,
      pars,
      bogeys,
      doubles,
      gir,
      fairways,
      putts,
      win: pt.win,
      top_5: pt.top_5,
      top_10: pt.top_10,
      top_20: pt.top_20,
      make_cut: pt.make_cut,
    };
    if (skRow) {
      for (const k of ["sg_total", "sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_t2g"]) {
        if (Number.isFinite(skRow[k])) rowOut[k] = skRow[k];
      }
      if (Number.isFinite(skRow.driving_dist)) rowOut.driving_dist = skRow.driving_dist;
      if (Number.isFinite(skRow.driving_acc)) rowOut.driving_acc = skRow.driving_acc;
      if (Number.isFinite(skRow.driving_distance_rating)) {
        rowOut.driving_distance_rating = skRow.driving_distance_rating;
      }
    }
    if (isPlausibleDrivingDistanceYds(driving_distance)) {
      const dyInt = Math.round(driving_distance);
      rowOut.driving_distance = dyInt;
      rowOut.avg_driving_distance = dyInt;
    }
    if (Number.isFinite(driving_accuracy)) rowOut.driving_accuracy = driving_accuracy;
    base.push(rowOut);
  }

  const score_to_par = (mu) => -num(mu, 0);
  const total_score = (mu) => course_par_18 + score_to_par(mu);

  base.sort((a, b) => total_score(a.mu_sg) - total_score(b.mu_sg));
  const posMap = new Map(base.map((r, i) => [r.dg_id, i + 1]));

  const tz = process.env.GOLF_OU_TZ || "America/New_York";
  const dr = ouDisplayRoundAuto(new Date(), tz);
  const roundMuMult = parseRoundMuMult();

  function stripGirFairwaysPuttsIfTiny(o) {
    if (!o || typeof o !== "object") return;
    for (const k of ["gir", "fairways", "putts"]) {
      const v = o[k];
      if (v === 0 || v === 1) delete o[k];
    }
  }

  const players = [];
  for (let r = 1; r <= 4; r++) {
    const mult = num(roundMuMult[r - 1], 1);
    for (const row of base) {
      let st;
      if (r === 1) {
        st = {
          mu_sg: row.mu_sg,
          implied_mu_sg: row.implied_mu_sg,
          eagles: row.eagles,
          birdies: row.birdies,
          pars: row.pars,
          bogeys: row.bogeys,
          doubles: row.doubles,
          gir: row.gir,
          fairways: row.fairways,
          putts: row.putts,
        };
      } else {
        const skRowR = skillByDg.get(row.dg_id);
        const distR =
          isPlausibleDrivingDistanceYds(row.driving_distance) ? row.driving_distance : NaN;
        st = derivedStatsFromMuSg(row.mu_sg * mult, fairwayHolesThisCourse, {
          sg_ott: row.sg_ott,
          fieldMeanOtt,
          sg_app: row.sg_app,
          fieldMeanApp,
          nGirHoles: 18,
          driving_distance: distR,
          fieldMeanDrive,
          histCountFit: histCalib,
          skRow: skRowR,
        });
      }
      const stp = score_to_par(st.mu_sg);
      const ts = total_score(st.mu_sg);
      const pl = {
        dg_id: row.dg_id,
        player_name: row.player_name,
        country: row.country,
        round: r,
        round_label: `R${r} (${WEEKDAYS[r - 1]})`,
        next_round: r,
        position: posMap.get(row.dg_id),
        mu_sg: Math.round(st.mu_sg * 1000) / 1000,
        implied_mu_sg: Math.round(st.implied_mu_sg * 1000) / 1000,
        score_to_par: Math.round(stp * 100) / 100,
        total_score: Math.round(ts * 100) / 100,
        round_sd: RAW_ROUND_SD,
        gir: Math.round(st.gir * 100) / 100,
        fairways: Math.round(st.fairways * 100) / 100,
        putts: Math.round(st.putts * 100) / 100,
        eagles: Math.round(st.eagles * 1000) / 1000,
        birdies: Math.round(st.birdies * 100) / 100,
        pars: Math.round(st.pars * 100) / 100,
        bogeys: Math.round(st.bogeys * 100) / 100,
        doubles: Math.round(st.doubles * 1000) / 1000,
        win: Number.isFinite(row.win) ? Math.round(row.win * 10000) / 10000 : null,
        top_5: Number.isFinite(row.top_5) ? Math.round(row.top_5 * 10000) / 10000 : null,
        top_10: Number.isFinite(row.top_10) ? Math.round(row.top_10 * 10000) / 10000 : null,
        top_20: Number.isFinite(row.top_20) ? Math.round(row.top_20 * 10000) / 10000 : null,
        make_cut: Number.isFinite(row.make_cut) ? Math.round(row.make_cut * 10000) / 10000 : null,
        course_used: course_used || undefined,
      };
      stripGirFairwaysPuttsIfTiny(pl);
      for (const k of ["sg_total", "sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_t2g"]) {
        if (Number.isFinite(row[k])) pl[k] = Math.round(row[k] * 1000) / 1000;
      }
      if (Number.isFinite(row.driving_distance)) {
        const dy = Math.round(row.driving_distance);
        pl.avg_driving_distance = dy;
        pl.driving_distance = dy;
      }
      if (Number.isFinite(row.driving_distance_rating)) {
        pl.driving_distance_rating = Math.round(row.driving_distance_rating * 100) / 100;
      }
      if (Number.isFinite(row.driving_accuracy)) {
        const da = row.driving_accuracy;
        pl.driving_accuracy =
          da > 0 && da <= 1 ? Math.round(da * 1000) / 10 : Math.round(da * 10) / 10;
      } else {
        const fw = st.fairways;
        if (Number.isFinite(fw) && fw > 1.02) {
          pl.driving_accuracy = Math.round(((fw / fairwayHolesThisCourse) * 100) * 10) / 10;
        }
      }
      players.push(pl);
    }
  }

  let outrights = {};
  try {
    console.log("Fetching DataGolf betting-tools/outrights for finish-position EV data…");
    const dgOutrights = await fetchDataGolfOutrightsApi({ apiKey: key, tour: tourForFeeds, oddsFormat: "percent" });
    for (const msg of dgOutrights.logs || []) console.log("[datagolf-outrights]", msg);
    outrights = dgOutrights.outrights || {};
  } catch (e) {
    console.warn("[datagolf-outrights] skipped; fetch:book-odds can merge sportsbook lines later:", e.message || e);
  }

  /** betting-tools/matchups — same markets as Shiny (decimal odds); stored raw for browser (no CORS). */
  const matchupMarkets = ["tournament_matchups", "round_matchups", "3_balls"];
  const matchups = {};
  for (const m of matchupMarkets) {
    try {
      console.log(`Fetching betting-tools/matchups (${m})…`);
      const raw = await fetchDg(
        "/betting-tools/matchups",
        { tour: tourForFeeds, market: m, odds_format: "decimal", file_format: "json" },
        key
      );
      if (raw && typeof raw === "object") matchups[m] = raw;
    } catch (e) {
      console.warn(`Matchups ${m} skipped:`, e.message);
    }
  }

  const projectionsOutPath = join(ROOT, "projections.json");
  const preservedProps = tryPreservePropsFromDisk(projectionsOutPath, event_name, course_used);

  const payload = {
    event_name,
    course_used,
    /** Betting tools + preds feeds must use this tour (may be `opp` while env says `pga`). */
    datagolf_feed_tour: tourForFeeds,
    /** Which upcoming event get-schedule used to choose amongst field-updates feeds (debug). */
    datagolf_schedule_anchor_event: anchor?.name || undefined,
    /** Stable compare for fetch-book-odds vs `/field-updates` (surpasses fuzzy-only title bugs). */
    datagolf_field_week_key: fieldWeekKey(event_name, course_used),
    display_round: dr,
    display_round_label: displayRoundLabel(dr, tz),
    updated_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    source:
      "DataGolf API (field-updates, preds/live-hole-stats hole pars, skill-ratings, player-decompositions, preds/live-tournament-stats when available, preds/pre-tournament, betting-tools/outrights, betting-tools/matchups)",
    /** Web app: book columns are implied % (0–100); convert to American in UI like Shiny pct_to_american */
    outrights_odds_format: "percent",
    /** Stored raw from betting-tools/matchups with odds_format=decimal */
    matchups_odds_format: "decimal",
    /** Same-origin browser polls only; server pushes DataGolf via serve-with-refresh pollers. */
    projections_poll_interval_sec: 20,
    datagolf_live_poll_interval_sec: 20,
    poll_datagolf_live_predictions: true,
    /** +EV / outrights: raw export probs (no model↔consensus blend line; no live-board nudges unless enabled). */
    outrights_model_blend_weight: 1,
    outright_win_score_blend: 0,
    outright_live_score_placement_nudge: false,
    course_par_18,
    hole_pars,
    hole_pars_source,
    projection_course_basis: {
      fairway_holes_modeled: fairwayHolesThisCourse,
      pret_expected_round_strokes_players: pretStrokesByDg.size,
    },
    historical_projection_calibration: {
      skipped: !!histCalib.skipped,
      csv_path: histCalib.csv_path || undefined,
      n_counts: histCalib.n_counts,
      n_gir_app: histCalib.n_gir_app,
      n_fw_ott: histCalib.n_fw_ott,
      n_fw_stp: histCalib.n_fw_stp,
      fw_stp_line: histCalib.fw_stp_line
        ? {
            a: Math.round(histCalib.fw_stp_line.a * 10000) / 10000,
            b: Math.round(histCalib.fw_stp_line.b * 10000) / 10000,
            n: histCalib.fw_stp_line.n,
          }
        : null,
      r2_gir_app: Number.isFinite(histCalib.r2_gir_app) ? Math.round(histCalib.r2_gir_app * 10000) / 10000 : null,
      r2_fw_ott: Number.isFinite(histCalib.r2_fw_ott) ? Math.round(histCalib.r2_fw_ott * 10000) / 10000 : null,
      w_gir_skill: Math.round(histCalib.w_gir_skill * 1000) / 1000,
      w_ott_skill: Math.round(histCalib.w_ott_skill * 1000) / 1000,
      w_ott_decomp: Math.round(histCalib.w_ott_decomp * 1000) / 1000,
    },
    players,
    props: preservedProps,
    outrights,
    matchups,
  };

  writeFileSync(projectionsOutPath, JSON.stringify(payload, null, 2), "utf8");
  if (!preservedProps.length) {
    console.log(
      "[fetch-dg] props[] empty — run `npm run fetch:book-odds` (DraftKings round O/U) after fetch:dg unless you set GOLF_RESET_PROPS=1 and intentionally cleared lines.",
    );
  }
  console.log(
    `Wrote ${players.length} projection rows (${fieldRows.length} players × 4 rounds), outrights: ${Object.keys(outrights).join(", ")}, matchups: ${Object.keys(matchups).join(", ")} -> ${projectionsOutPath}`
  );

  const rscriptCmd = findRscriptSync();
  const renderHost = String(process.env.RENDER || "").toLowerCase() === "true";
  const skipHistoryEnv = String(process.env.GOLF_SKIP_HISTORY_ON_FETCH_DG || "").trim();
  const skipHistoryOnFetchDg = skipHistoryEnv === "1" || (renderHost && skipHistoryEnv !== "0");
  const updateRoundsNode = join(__dirname, "update-historical-rounds-node.mjs");
  if (skipHistoryOnFetchDg) {
    console.log(
      "Skipping fetch:dg history rebuild — keeping existing player_round_history.json / historical_rounds_all.csv."
    );
  } else if (existsSync(updateRoundsNode) && key) {
    console.log("Updating data/historical_rounds_all.csv (DataGolf, Node: PGA + LIV) …");
    const ur = spawnSync(process.execPath, [updateRoundsNode], {
      cwd: ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: GOLF_MODEL_ROOT, DATAGOLF_API_KEY: key },
    });
    if (ur.status !== 0) {
      console.warn(
        "update-historical-rounds-node.mjs exited with code",
        ur.status,
        "— continuing with existing historical_rounds_all.csv."
      );
    }
  }

  const historyScript = join(__dirname, "build-player-history.mjs");
  const embedHistoryScript = join(__dirname, "embed-player-history.mjs");
  if (!skipHistoryOnFetchDg && existsSync(historyScript)) {
    console.log("Rebuilding player_round_history.json from historical_rounds_all.csv (+ hole_data) …");
    const r = spawnSync(process.execPath, [historyScript], {
      cwd: ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: GOLF_MODEL_ROOT },
    });
    if (r.status !== 0) console.warn("build-player-history.mjs exited with code", r.status);
    else if (existsSync(embedHistoryScript)) {
      const er = spawnSync(process.execPath, [embedHistoryScript], {
        cwd: ROOT,
        stdio: "inherit",
        env: process.env,
      });
      if (er.status !== 0) console.warn("embed-player-history.mjs exited with code", er.status);
    }
  }

  const pgaHistoryR = join(GOLF_MODEL_ROOT, "scripts", "build_alpha_caddie_web_history_pga.R");
  if (
    process.env.ALPHA_CADDIE_PGA_HISTORY === "1" &&
    existsSync(pgaHistoryR) &&
    rscriptCmd
  ) {
    console.log("ALPHA_CADDIE_PGA_HISTORY=1: overwriting player_round_history.json from pgatouR …");
    const rr = spawnSync(rscriptCmd, [pgaHistoryR, GOLF_MODEL_ROOT], {
      cwd: ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: GOLF_MODEL_ROOT },
    });
    if (rr.status !== 0) {
      console.warn("build_alpha_caddie_web_history_pga.R exited with code", rr.status);
    } else if (existsSync(embedHistoryScript)) {
      const er2 = spawnSync(process.execPath, [embedHistoryScript], {
        cwd: ROOT,
        stdio: "inherit",
        env: process.env,
      });
      if (er2.status !== 0) console.warn("embed-player-history.mjs exited with code", er2.status);
    }
  }

  const shotsWebScript = join(__dirname, "build-player-shots-web.mjs");
  if (buildShotsWebOnFetch && existsSync(shotsWebScript)) {
    console.log(
      "Running build-player-shots-web.mjs (full shot rows only if GOLF_USE_ALL_SHOTS_CSV=1 and CSV exists) …",
    );
    const sr = spawnSync(process.execPath, [shotsWebScript], {
      cwd: ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: GOLF_MODEL_ROOT },
    });
    if (sr.status !== 0) console.warn("build-player-shots-web.mjs exited with code", sr.status);
  } else if (!buildShotsWebOnFetch) {
    console.log(
      "Skipping player_shots_web.json rebuild and all_shots CSV mirror (set GOLF_BUILD_SHOTS_WEB_ON_FETCH=1 to enable)."
    );
  }

  mirrorModelDataToWeb(GOLF_MODEL_ROOT, ROOT);

  const buildCourseTable = join(__dirname, "build-course-table-json.mjs");
  if (existsSync(buildCourseTable)) {
    const bt = spawnSync(process.execPath, [buildCourseTable], {
      cwd: ROOT,
      stdio: "inherit",
      env: process.env,
    });
    if (bt.status !== 0) {
      console.warn("build-course-table-json.mjs exited with code", bt.status, "— continuing.");
    }
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
