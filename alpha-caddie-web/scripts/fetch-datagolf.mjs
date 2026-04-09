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
 * Daily automation: refresh:daily runs this script then pgatouR shot backfill; shots CSV is mirrored to
 * alpha-caddie-web/data/all_shots_2021_2026.csv. This script then runs build-player-shots-web.mjs →
 * player_shots_web.json (2022+ rows) for the Player Props shot log. GOLF_SKIP_SHOTS_UPDATE=1 skips shots R step only.
 *
 * Or copy datagolf.local.example.json -> datagolf.local.json and put "apiKey" there.
 *
 * Requires Node 18+ (global fetch).
 *
 * Hole Hangout: writes hole_pars (18 ints) from course_holes.json + course_holes.local.json,
 * else DataGolf field-updates if it exposes holes, else all_2026_holes.csv (event name match),
 * else a generic layout. Override: GOLF_HOLES_CSV=path/to.csv
 *
 * After projections: refreshes repo data/historical_rounds_all.csv via Node
 * (scripts/update-historical-rounds-node.mjs — PGA + LIV, DataGolf historical-raw-data/rounds), then rebuilds player_round_history.json
 * and writes embedded-player-round-history.js (window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__).
 * Set ALPHA_CADDIE_EMBED_HISTORY=0 to skip that step. Set ALPHA_CADDIE_PGA_HISTORY=1 to run
 * the pgatouR history script after the CSV build (embed runs again after that).
 */

import { spawnSync } from "child_process";
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { findRscriptSync } from "./find-rscript.mjs";
import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = join(ROOT, "..");

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

function lookupHoleParsFromMaps(maps, course_used, event_name) {
  const ck = normHoleKey(course_used);
  const ek = normHoleKey(event_name);
  const bc = maps.by_course || {};
  const be = maps.by_event || {};
  if (ck && bc[ck] && Array.isArray(bc[ck]) && bc[ck].length === 18) return { pars: bc[ck], source: "bundled" };
  if (ek && be[ek] && Array.isArray(be[ek]) && be[ek].length === 18) return { pars: be[ek], source: "bundled" };
  for (const [k, v] of Object.entries(bc)) {
    if (!k || !Array.isArray(v) || v.length !== 18) continue;
    if (ck && (ck.includes(k) || k.includes(ck))) return { pars: v, source: "bundled" };
  }
  for (const [k, v] of Object.entries(be)) {
    if (!k || !Array.isArray(v) || v.length !== 18) continue;
    if (ek && (ek.includes(k) || k.includes(ek))) return { pars: v, source: "bundled" };
  }
  return null;
}

function holeParsFromFieldUpdates(raw) {
  if (!raw || typeof raw !== "object") return null;
  const tryArray = (a) => {
    if (!Array.isArray(a) || a.length < 18) return null;
    const arr = [];
    for (let i = 0; i < 18; i++) {
      const x = a[i];
      let p = NaN;
      if (typeof x === "number") p = x;
      else if (x && typeof x === "object") p = num(x.par ?? x.par_hole ?? x.hole_par, NaN);
      if (!Number.isFinite(p) || p < 3 || p > 5) return null;
      arr.push(Math.round(p));
    }
    return arr;
  };
  const nested = tryArray(raw.holes) || tryArray(raw.course_holes);
  if (nested) return nested;
  if (Array.isArray(raw.hole_par) && raw.hole_par.length === 18) {
    const arr = raw.hole_par.map((x) => Math.round(num(x, NaN)));
    if (arr.every((n) => n >= 3 && n <= 5)) return arr;
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

function resolveHoleParsForEvent({ fieldRaw, course_used, event_name }) {
  const maps = loadCourseHolesMaps();
  const fromMap = lookupHoleParsFromMaps(maps, course_used, event_name);
  if (fromMap) return { pars: fromMap.pars, source: fromMap.source };

  const fromField = holeParsFromFieldUpdates(fieldRaw);
  if (fromField) return { pars: fromField, source: "field_updates" };

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
const TOUR = process.env.GOLF_TOUR || "pga";

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

/** DataGolf JSON: root array, or { data }, { players }, { field }, { baseline_history_fit } */
function rowsFromResponse(dat) {
  if (dat == null) return [];
  if (Array.isArray(dat)) return dat;
  if (typeof dat !== "object") return [];
  for (const k of ["data", "players", "field", "baseline_history_fit", "baseline"]) {
    const v = dat[k];
    if (Array.isArray(v)) return v;
    if (v && typeof v === "object" && !Array.isArray(v) && dat.baseline_history_fit == null) {
      /* sometimes single object */
    }
  }
  if (Array.isArray(dat.baseline_history_fit)) return dat.baseline_history_fit;
  return [];
}

async function fetchDg(path, params, key) {
  const u = new URL(`https://feeds.datagolf.com${path}`);
  for (const [k, v] of Object.entries(params)) u.searchParams.set(k, String(v));
  u.searchParams.set("key", key);
  const res = await fetch(u.toString(), { headers: { Accept: "application/json" } });
  if (!res.ok) throw new Error(`${path} HTTP ${res.status}: ${await res.text().catch(() => "")}`);
  return res.json();
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

/** Default matches R round_projections shot-level fallback when ROUND_HIST_SG_MULT is unset. */
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
  };
}

function sgTotalFromSkillRow(row) {
  if (row.sg_total != null) return num(row.sg_total);
  if (row.total != null) return num(row.total);
  if (row.overall != null) return num(row.overall);
  return 0;
}

/** Normalize pre-tournament probs to 0–1 */
function normProb01(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x)) return NaN;
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
  const key = loadApiKey();
  if (!key) {
    console.error(
      "Missing API key. Set DATAGOLF_API_KEY or create datagolf.local.json (see datagolf.local.example.json)."
    );
    process.exit(1);
  }

  console.log("Fetching field-updates…");
  const fieldRaw = await fetchDg(
    "/field-updates",
    { tour: TOUR, file_format: "json" },
    key
  );
  const fieldList = asArray(fieldRaw.field).length ? asArray(fieldRaw.field) : rowsFromResponse(fieldRaw);
  const event_name = String(fieldRaw.event_name || fieldRaw.eventName || "").trim();
  const course_used = String(fieldRaw.course_name || fieldRaw.courseName || fieldRaw.course || "").trim();

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
  if (fieldRows.length === 0) {
    console.error("No players in field-updates response.");
    process.exit(1);
  }

  const byDg = new Map(fieldRows.map((r) => [r.dg_id, r]));

  console.log("Fetching skill-ratings…");
  let skillJson;
  try {
    skillJson = await fetchDg("/preds/skill-ratings", { display: "value", file_format: "json" }, key);
  } catch (e) {
    console.error(e.message);
    process.exit(1);
  }
  const skillList = rowsFromResponse(skillJson);
  const skillByDg = new Map();
  for (const row of skillList) {
    const id = num(row.dg_id ?? row.dgId, NaN);
    if (!Number.isFinite(id)) continue;
    skillByDg.set(Math.round(id), sgTotalFromSkillRow(row));
  }

  console.log("Fetching fantasy-projection-defaults…");
  let fantasyList = [];
  try {
    const fant = await fetchDg(
      "/preds/fantasy-projection-defaults",
      { tour: TOUR, site: "draftkings", slate: "main", file_format: "json" },
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
    fantasyByDg.set(Math.round(id), {
      birdies: bc ? num(row[bc]) : NaN,
      pars: pc ? num(row[pc]) : NaN,
      bogeys: bgc ? num(row[bgc]) : NaN,
      eagles: egc ? num(row[egc]) : NaN,
      doubles: dbc ? num(row[dbc]) : NaN,
      gir: gc ? num(row[gc]) : NaN,
      fairways: fc ? num(row[fc]) : NaN,
    });
  }

  console.log("Fetching preds/pre-tournament…");
  let pretList = [];
  try {
    const pret = await fetchDg(
      "/preds/pre-tournament",
      { tour: TOUR, dead_heat: "no", odds_format: "percent", file_format: "json" },
      key
    );
    pretList = asArray(pret.baseline_history_fit).length
      ? asArray(pret.baseline_history_fit)
      : asArray(pret.baseline).length
        ? asArray(pret.baseline)
        : rowsFromResponse(pret);
  } catch (e) {
    console.warn("Pre-tournament skipped:", e.message);
  }

  const pretByDg = new Map();
  for (const row of pretList) {
    const id = num(row.dg_id ?? row.id ?? row.dgId, NaN);
    if (!Number.isFinite(id)) continue;
    pretByDg.set(Math.round(id), {
      win: normProb01(row.win),
      top_5: normProb01(row.top_5),
      top_10: normProb01(row.top_10),
      top_20: normProb01(row.top_20),
      make_cut: normProb01(row.make_cut),
    });
  }

  const base = [];
  for (const fr of fieldRows) {
    const id = fr.dg_id;
    let mu_sg = skillByDg.has(id) ? skillByDg.get(id) : 0;
    if (!Number.isFinite(mu_sg)) mu_sg = 0;

    const fx = fantasyByDg.get(id) || {};
    let eagles = num(fx.eagles);
    let birdies = num(fx.birdies);
    let pars = num(fx.pars);
    let bogeys = num(fx.bogeys);
    let doubles = num(fx.doubles);
    let gir = num(fx.gir);
    let fairways = num(fx.fairways);

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

    const pt = pretByDg.get(id) || {};
    base.push({
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
      win: pt.win,
      top_5: pt.top_5,
      top_10: pt.top_10,
      top_20: pt.top_20,
      make_cut: pt.make_cut,
    });
  }

  const score_to_par = (mu) => -num(mu, 0);
  const total_score = (mu) => COURSE_PAR_18 + score_to_par(mu);

  base.sort((a, b) => total_score(a.mu_sg) - total_score(b.mu_sg));
  const posMap = new Map(base.map((r, i) => [r.dg_id, i + 1]));

  const tz = process.env.GOLF_OU_TZ || "America/New_York";
  const dr = ouDisplayRoundAuto(new Date(), tz);
  const roundMuMult = parseRoundMuMult();

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
        };
      } else {
        st = derivedStatsFromMuSg(row.mu_sg * mult, N_FAIRWAY_HOLES);
      }
      const stp = score_to_par(st.mu_sg);
      const ts = total_score(st.mu_sg);
      players.push({
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
      });
    }
  }

  /**
   * betting-tools/outrights — must use odds_format=percent (same as app.R).
   * Row list resolution matches Shiny outrights_table_data (odds → data → field → …).
   * Each numeric book field: if value > 1 treat as 0–100 scale and divide by 100 to prob, else already prob (R: is.finite(v) && v > 1).
   */
  function outrightOddsArrayFromResponse(raw) {
    if (raw == null) return [];
    if (Array.isArray(raw)) return raw;
    if (typeof raw !== "object") return [];
    const chain = [raw.odds, raw.data, raw.field, raw.players, raw.baseline, raw.baseline_history_fit];
    for (const c of chain) {
      if (Array.isArray(c)) return c;
    }
    return [];
  }

  const OUTRIGHTS_ROW_SKIP_KEYS = new Set(["dg_id", "id", "player_name", "name"]);

  function impliedPercentFromDgPercentField(v) {
    if (!Number.isFinite(v)) return NaN;
    let p = v;
    if (p > 1) p /= 100;
    return p * 100;
  }

  function parseOutrightsResponse(raw) {
    const arr = outrightOddsArrayFromResponse(raw);
    const rows = [];
    const bookSet = new Set();
    for (const row of arr) {
      if (!row || typeof row !== "object") continue;
      const dg_id = Math.round(num(row.dg_id ?? row.id, NaN));
      const player_name = String(row.player_name ?? row.name ?? "").trim();
      if (!Number.isFinite(dg_id) || !player_name) continue;
      const out = { dg_id, player_name };
      for (const k of Object.keys(row)) {
        const key = k.toLowerCase();
        if (OUTRIGHTS_ROW_SKIP_KEYS.has(key)) continue;
        let val = row[k];
        if (val != null && typeof val === "object" && !Array.isArray(val)) {
          const vs = Object.values(val);
          val = vs.length ? vs[0] : null;
        }
        if (Array.isArray(val) && val.length) val = val[0];
        const v = num(val, NaN);
        if (!Number.isFinite(v)) continue;
        const pct = impliedPercentFromDgPercentField(v);
        if (!Number.isFinite(pct)) continue;
        out[key] = pct;
        bookSet.add(key);
      }
      rows.push(out);
    }
    return { rows, bookKeys: [...bookSet].sort() };
  }

  const outrightsMarkets = ["win", "top_5", "top_10", "top_20", "make_cut", "mc"];
  const outrights = {};
  for (const m of outrightsMarkets) {
    try {
      console.log(`Fetching betting-tools/outrights (${m})…`);
      const raw = await fetchDg(
        "/betting-tools/outrights",
        { tour: TOUR, market: m, odds_format: "percent", file_format: "json" },
        key
      );
      const { rows, bookKeys } = parseOutrightsResponse(raw);
      if (rows.length > 0) outrights[m] = { rows, bookKeys };
    } catch (e) {
      console.warn(`Outrights ${m} skipped:`, e.message);
    }
  }

  /** betting-tools/matchups — same markets as Shiny (decimal odds); stored raw for browser (no CORS). */
  const matchupMarkets = ["tournament_matchups", "round_matchups", "3_balls"];
  const matchups = {};
  for (const m of matchupMarkets) {
    try {
      console.log(`Fetching betting-tools/matchups (${m})…`);
      const raw = await fetchDg(
        "/betting-tools/matchups",
        { tour: TOUR, market: m, odds_format: "decimal", file_format: "json" },
        key
      );
      if (raw && typeof raw === "object") matchups[m] = raw;
    } catch (e) {
      console.warn(`Matchups ${m} skipped:`, e.message);
    }
  }

  const holeRes = resolveHoleParsForEvent({ fieldRaw, course_used, event_name });
  const hole_pars = holeRes.pars.map((x) => Math.round(num(x, 4)));
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
    display_round: dr,
    display_round_label: displayRoundLabel(dr, tz),
    updated_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    source:
      "DataGolf API (field-updates, skill-ratings, fantasy-projection-defaults, preds/pre-tournament, betting-tools/outrights, betting-tools/matchups)",
    /** Web app: book columns are implied % (0–100); convert to American in UI like Shiny pct_to_american */
    outrights_odds_format: "percent",
    /** Stored raw from betting-tools/matchups with odds_format=decimal */
    matchups_odds_format: "decimal",
    course_par_18: COURSE_PAR_18,
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
  const updateRoundsNode = join(__dirname, "update-historical-rounds-node.mjs");
  if (existsSync(updateRoundsNode) && key) {
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
  if (existsSync(historyScript)) {
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
  if (existsSync(shotsWebScript)) {
    console.log("Building player_shots_web.json from all_shots CSV (2022+) …");
    const sr = spawnSync(process.execPath, [shotsWebScript], {
      cwd: ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: GOLF_MODEL_ROOT },
    });
    if (sr.status !== 0) console.warn("build-player-shots-web.mjs exited with code", sr.status);
  }

  mirrorModelDataToWeb(GOLF_MODEL_ROOT, ROOT);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
