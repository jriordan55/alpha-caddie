/**
 * Pull PGA field + skill + fantasy + pre-tournament probs from DataGolf (same idea as
 * round_projections.R RAW_PROJECTIONS / GOLF_RAW_PROJECTIONS=1) and write projections.json.
 * R2–R4 rows: mean SG is scaled by per-round multipliers (default matches R shot-MC fallback
 * 1, 0.99, 0.97, 0.95) and counts/GIR/FW are re-derived so Model O/U changes when the round
 * selector changes. Override: GOLF_NODE_ROUND_MU_MULT=1,0.99,0.97,0.95
 *
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
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
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

function clampMuSg(m) {
  const x = num(m, 0);
  if (!Number.isFinite(x)) return 0;
  return Math.max(-4, Math.min(4, x));
}

/** Default matches R round_projections Gaussian-round mu_mult when ROUND_HIST_SG_MULT is unset. */
function parseRoundMuMult() {
  const def = [1, 0.99, 0.97, 0.95];
  const raw = process.env.GOLF_NODE_ROUND_MU_MULT;
  if (raw == null || !String(raw).trim()) return def;
  const parts = String(raw)
    .split(",")
    .map((s) => num(s.trim(), NaN));
  if (parts.length < 4 || parts.some((p) => !Number.isFinite(p))) return def;
  return parts.slice(0, 4);
}

function derivedStatsFromMuSg(muRaw, nFairwayHoles) {
  const mu_sg = clampMuSg(muRaw);
  const im = imputeCountsFromNegMu(mu_sg);
  const stpVec = -mu_sg;
  const gir = Math.max(6, Math.min(16, 11.5 - 0.25 * stpVec));
  const fairways = Math.max(4, Math.min(nFairwayHoles, 0.55 * nFairwayHoles - 0.15 * stpVec));
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

/** Driving distance + accuracy skill ratings, from skill-ratings / decompositions column aliases. */
function drivingAttrsFromSkillBag(row) {
  if (!row || typeof row !== "object") {
    return { driving_distance: NaN, driving_accuracy: NaN, driving_dist: NaN, driving_acc: NaN };
  }
  const bag = normalizedScalarBag(row);
  const dist = pickFromBag(bag, [
    "avg_driving_distance",
    "average_driving_distance",
    "mean_driving_distance",
    "avg_drive_distance",
    "avg_drive_dist",
    "driving_distance",
    "drive_distance",
    "distance",
    "driving_dist",
    "predicted_driving_distance",
    "predicted_avg_driving_distance",
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
  const accRating = Number.isFinite(acc) && acc > -1 && acc < 1 ? acc * 100 : acc;
  return { driving_distance: dist, driving_accuracy: accRating, driving_dist: dist, driving_acc: acc };
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

  console.log("Fetching fantasy-projection-defaults…");
  let fantasyList = [];
  try {
    const fant = await fetchDg(
      "/preds/fantasy-projection-defaults",
      { tour: tourForFeeds, site: "draftkings", slate: "main", file_format: "json" },
      key
    );
    fantasyList = rowsFromResponse(fant);
  } catch (e) {
    console.warn("Fantasy defaults skipped:", e.message);
  }

  const fantasyByDg = new Map();
  for (const row of fantasyList) {
    const id = num(row.dg_id ?? row.dgId, NaN);
    if (!Number.isFinite(id)) continue;
    const bc = firstNumCol(row, ["birdies", "birdie", "proj_birdies"]);
    const pc = firstNumCol(row, ["pars", "par"]);
    const bgc = firstNumCol(row, ["bogeys", "bogey", "bogies"]);
    const egc = firstNumCol(row, ["eagles", "eagle_or_better"]);
    const dbc = firstNumCol(row, ["doubles", "double_bogeys", "doubles_or_worse"]);
    const gc = firstNumCol(row, ["gir", "greens_in_regulation", "gir_count"]);
    const fc = firstNumCol(row, ["fairways", "driving_accuracy", "fw", "fairway"]);
    const ddc = firstNumCol(row, [
      "avg_driving_distance",
      "average_driving_distance",
      "driving_distance",
      "drive_distance",
      "distance",
    ]);
    fantasyByDg.set(Math.round(id), {
      birdies: bc ? num(row[bc]) : NaN,
      pars: pc ? num(row[pc]) : NaN,
      bogeys: bgc ? num(row[bgc]) : NaN,
      eagles: egc ? num(row[egc]) : NaN,
      doubles: dbc ? num(row[dbc]) : NaN,
      gir: gc ? num(row[gc]) : NaN,
      fairways: fc ? num(row[fc]) : NaN,
      driving_distance: ddc ? num(row[ddc]) : NaN,
    });
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

  const base = [];
  for (const fr of fieldRows) {
    const id = fr.dg_id;
    const skRow = skillByDg.get(id);
    let mu_sg = skRow && Number.isFinite(skRow.sg_total) ? skRow.sg_total : 0;
    if (!Number.isFinite(mu_sg)) mu_sg = 0;

    const liveDv = liveDrivingByDg.get(id);
    let driving_distance =
      skRow && Number.isFinite(skRow.driving_distance)
        ? skRow.driving_distance
        : liveDv && Number.isFinite(liveDv.distance)
          ? liveDv.distance
          : NaN;
    let driving_accuracy =
      skRow && Number.isFinite(skRow.driving_accuracy)
        ? skRow.driving_accuracy
        : liveDv && Number.isFinite(liveDv.accuracy)
          ? liveDv.accuracy
          : NaN;

    const fx = fantasyByDg.get(id) || {};
    let eagles = num(fx.eagles);
    let birdies = num(fx.birdies);
    let pars = num(fx.pars);
    let bogeys = num(fx.bogeys);
    let doubles = num(fx.doubles);
    let gir = num(fx.gir);
    let fairways = num(fx.fairways);
    if (!Number.isFinite(driving_distance)) driving_distance = num(fx.driving_distance);

    if (Number.isFinite(gir) && gir > 0 && gir <= 1) gir *= 18;
    if (Number.isFinite(fairways) && fairways > 0 && fairways <= 1) fairways *= N_FAIRWAY_HOLES;

    const im = imputeCountsFromNegMu(mu_sg);
    if (!Number.isFinite(eagles)) eagles = im.eagles;
    if (!Number.isFinite(birdies)) birdies = im.birdies;
    if (!Number.isFinite(pars)) pars = im.pars;
    if (!Number.isFinite(bogeys)) bogeys = im.bogeys;
    if (!Number.isFinite(doubles)) doubles = im.doubles;

    const stpVec = -mu_sg;
    if (!Number.isFinite(gir)) gir = Math.max(6, Math.min(16, 11.5 - 0.25 * stpVec));
    if (!Number.isFinite(fairways))
      fairways = Math.max(4, Math.min(N_FAIRWAY_HOLES, 0.55 * N_FAIRWAY_HOLES - 0.15 * stpVec));

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
    }
    if (Number.isFinite(driving_distance)) {
      const dyInt = Math.round(driving_distance);
      rowOut.driving_distance = dyInt;
      rowOut.avg_driving_distance = dyInt;
    }
    if (Number.isFinite(driving_accuracy)) rowOut.driving_accuracy = driving_accuracy;
    base.push(rowOut);
  }

  const score_to_par = (mu) => -num(mu, 0);
  const total_score = (mu) => COURSE_PAR_18 + score_to_par(mu);

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
        st = derivedStatsFromMuSg(row.mu_sg * mult, N_FAIRWAY_HOLES);
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
      if (Number.isFinite(row.driving_accuracy)) pl.driving_accuracy = Math.round(row.driving_accuracy * 10) / 10;
      else {
        const fw = st.fairways;
        if (Number.isFinite(fw) && fw > 1.02) {
          pl.driving_accuracy = Math.round(((fw / N_FAIRWAY_HOLES) * 100) * 10) / 10;
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

  let liveHoleStatsForPars = null;
  if (String(process.env.GOLF_SKIP_LIVE_HOLE_STATS_HOLE_PARS || "").trim() !== "1") {
    try {
      console.log("Fetching preds/live-hole-stats (per-hole par layout for Hole Hangout)…");
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
  const course_par_18 = hole_pars.length === 18 ? hole_pars.reduce((sum, p) => sum + Math.round(num(p, 4)), 0) : COURSE_PAR_18;
  const hole_pars_source = holeRes.source;
  if (hole_pars_source === "generic") {
    console.warn(
      "Hole Hangout: no course/event match in course_holes*.json or CSV — using generic 18-hole par layout. Add pars to course_holes.json or course_holes.local.json."
    );
  } else {
    console.log(
      `Hole Hangout hole pars: ${hole_pars_source}${holeRes.detail ? ` (${holeRes.detail})` : ""}`
    );
  }

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
      "DataGolf API (field-updates, preds/live-hole-stats hole pars, skill-ratings, player-decompositions, preds/live-tournament-stats when available, fantasy-projection-defaults, preds/pre-tournament, betting-tools/outrights, betting-tools/matchups)",
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
    players,
    props: [],
    outrights,
    matchups,
  };

  const outPath = join(ROOT, "projections.json");
  writeFileSync(outPath, JSON.stringify(payload, null, 2), "utf8");
  console.log(
    `Wrote ${players.length} projection rows (${fieldRows.length} players × 4 rounds), outrights: ${Object.keys(outrights).join(", ")}, matchups: ${Object.keys(matchups).join(", ")} -> ${outPath}`
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
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
