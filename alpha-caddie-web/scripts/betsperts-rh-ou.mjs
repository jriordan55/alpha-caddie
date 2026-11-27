/**
 * Rabbit Hole → Round projections market stats (SG:TOT, BoB%, Dr Acc, GIR%, Bogey%/Dbl%).
 */
import { existsSync, readFileSync } from "fs";
import { join, resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");
const API = String(process.env.BETSPERTS_API_BASE || "https://api.betspertsgolf.com").replace(/\/$/, "");

export const RH_OU_VIEWS = Object.freeze([
  { view: "Strokes Gained", fields: ["SG:TOT"] },
  { view: "Scoring", fields: ["BoB %", "Bogey %", "Double Bogey+ %"] },
  { view: "Off the Tee", fields: ["Dr Accuracy %"] },
  { view: "Approach", fields: ["GIR %"] },
]);

export const RH_TIMEFRAME_PRESETS = Object.freeze([
  { value: "all", label: "All" },
  { value: "round:12", label: "Last 12 rounds" },
  { value: "round:24", label: "Last 24 rounds" },
  { value: "round:36", label: "Last 36 rounds" },
  { value: "round:50", label: "Last 50 rounds" },
  { value: "round:100", label: "Last 100 rounds" },
]);

export function resolveBetspertsSessionKey() {
  const env = String(process.env.BETSPERTS_SESSION_KEY || process.env.GOLF_BETSPERTS_SESSION_KEY || "").trim();
  if (env) return env;
  for (const p of [join(WEB, "betsperts.local.json"), join(ROOT, "betsperts.local.json")]) {
    if (!existsSync(p)) continue;
    try {
      const j = JSON.parse(readFileSync(p, "utf8"));
      const k = String(j.sessionKey || j.session_key || "").trim();
      if (k) return k;
    } catch {
      /* ignore */
    }
  }
  return "";
}

export function rabbitHoleLoadBody(overrides = {}) {
  return {
    playerID: "All Players",
    displayTourId: "All Players",
    tyear: null,
    tourIndex: 0,
    date: "All",
    region: [],
    ctype: [],
    view: "Strokes Gained",
    course: [],
    courseName: [],
    season: [],
    condition: "",
    clength: "",
    fieldStrength: "",
    par: "",
    cut: "",
    tournament: "",
    eventSeason: "",
    fieldSize: "",
    bunkerDanger: "",
    waterDanger: "",
    green: "",
    greenSize: "",
    speed: "",
    fairwaySurface: "",
    roughSurface: "",
    rough: "",
    elevation: "",
    architect: "",
    gainOTT: "",
    OTTClub: "",
    fwyAccuracy: "",
    missedFwyPenalty: "",
    roughPenalty: "",
    gainAPP: "",
    GIRAccuracy: "",
    par3Scoring: "",
    par4Scoring: "",
    par5Scoring: "",
    gainARG: "",
    scramblingRough: "",
    scramblingShortGrass: "",
    sandSavesCond: "",
    gainPutting: "",
    gainingPuttingInside15: "",
    gainingPuttingOutside15: "",
    PuttAVD: "",
    display: "Average",
    TempAVG: "",
    GrassCondition: "",
    PreferredLies: "",
    play: "",
    teeTime: "",
    startDate: "",
    endDate: "",
    round: "",
    wind: "",
    min_round: "",
    ...overrides,
  };
}

/** Scalar / array condition fields from Filter-by-Conditions UI. */
const RH_CONDITION_SCALAR_FIELDS = Object.freeze([
  "condition",
  "clength",
  "fieldStrength",
  "par",
  "cut",
  "tournament",
  "eventSeason",
  "fieldSize",
  "bunkerDanger",
  "waterDanger",
  "green",
  "greenSize",
  "speed",
  "fairwaySurface",
  "roughSurface",
  "rough",
  "elevation",
  "architect",
  "gainOTT",
  "OTTClub",
  "fwyAccuracy",
  "missedFwyPenalty",
  "roughPenalty",
  "teeTime",
]);

function normalizeRhFilterValue(v) {
  if (v == null) return null;
  if (Array.isArray(v)) {
    const arr = v.map(String).map((x) => x.trim()).filter((x) => x && x !== "all" && x !== "All");
    return arr.length ? arr : null;
  }
  const s = String(v).trim();
  if (!s || s === "all" || s === "All") return null;
  return s;
}

/** Parse UI filter values into RH load overrides (excluding view). */
export function rhFiltersToLoadOverrides(filters = {}) {
  const out = {
    date: "All",
    round: "",
    season: [],
    region: [],
    ctype: [],
    course: [],
    courseName: [],
    startDate: "",
    endDate: "",
    display: String(filters.display || "Average").trim() || "Average",
  };
  const tf = String(filters.timeframe || filters.time || "all").trim().toLowerCase();
  if (tf.startsWith("round:")) {
    const n = String(tf.slice(6)).replace(/\D/g, "");
    if (n) out.round = n;
  } else if (tf.startsWith("season:")) {
    const y = String(tf.slice(7)).replace(/\D/g, "");
    if (y) out.season = [y];
  } else if (/^\d{4}$/.test(tf)) {
    out.season = [tf];
  }
  // else all

  const region = normalizeRhFilterValue(filters.region);
  if (region) out.region = Array.isArray(region) ? region : [region];

  const ctype = normalizeRhFilterValue(filters.ctype || filters.courseType || filters.course_type);
  if (ctype) out.ctype = Array.isArray(ctype) ? ctype : [ctype];

  const course = normalizeRhFilterValue(filters.course);
  if (course) out.course = Array.isArray(course) ? course : [course];

  const conditions = filters.conditions && typeof filters.conditions === "object" ? filters.conditions : filters;
  for (const field of RH_CONDITION_SCALAR_FIELDS) {
    const raw = normalizeRhFilterValue(conditions[field] ?? filters[field]);
    if (raw == null) continue;
    // Multi-select condition fields are arrays on rabbit_hole/load.
    out[field] = Array.isArray(raw) ? raw : [raw];
  }

  return out;
}

function num(v, fallback = NaN) {
  if (v == null || v === "") return fallback;
  const n = Number(String(v).replace(/,/g, "").trim());
  return Number.isFinite(n) ? n : fallback;
}

function round3(x) {
  return Math.round(x * 1000) / 1000;
}

function round1(x) {
  return Math.round(x * 10) / 10;
}

export function loadPgaToDgMap(mapPath = join(ROOT, "data", "pga_datagolf_player_map.csv")) {
  const pgaToDg = new Map();
  if (!existsSync(mapPath)) return pgaToDg;
  const rows = parse(readFileSync(mapPath, "utf8"), {
    columns: true,
    skip_empty_lines: true,
    relax_column_count: true,
    bom: true,
  });
  for (const r of rows) {
    const pga = String(r.pga_player_id || "").replace(/\D/g, "");
    const dg = Math.round(num(r.dg_id, NaN));
    if (!pga || !Number.isFinite(dg)) continue;
    pgaToDg.set(pga, dg);
  }
  return pgaToDg;
}

export async function rhApiPost(path, body, sessionKey) {
  const res = await fetch(`${API}${path}`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Accept: "application/json",
      Origin: "https://betspertsgolf.com",
      Referer: "https://betspertsgolf.com/the-rabbit-hole",
      ...(sessionKey ? { session_key: sessionKey } : {}),
    },
    body: JSON.stringify(body ?? {}),
  });
  const text = await res.text();
  let json = null;
  try {
    json = text ? JSON.parse(text) : null;
  } catch {
    json = null;
  }
  return { ok: res.ok, status: res.status, json };
}

/** Merge RH view rows into by-pga / by-dg stats maps. */
export function mergeRhOuViewRows(byPga, rows, view) {
  if (!Array.isArray(rows)) return;
  for (const row of rows) {
    const pga = String(row.player_num ?? row.playerID ?? "").replace(/\D/g, "");
    if (!pga) continue;
    const prev = byPga.get(pga) || {
      pga_id: pga,
      player_name: row.playerName || null,
      rounds: null,
    };
    if (row.playerName) prev.player_name = row.playerName;
    const rounds = num(row.Rounds, NaN);
    if (Number.isFinite(rounds)) prev.rounds = Math.round(rounds);

    if (view === "Strokes Gained") {
      const v = num(row["SG:TOT"], NaN);
      if (Number.isFinite(v)) prev.sg_total = round3(v);
      const ott = num(row["SG:OTT"], NaN);
      const app = num(row["SG:APP"], NaN);
      const arg = num(row["SG:ARG"], NaN);
      const putt = num(row["SG:P"] ?? row["SG:PUTT"], NaN);
      const t2g = num(row["SG:T2G"], NaN);
      if (Number.isFinite(ott)) prev.sg_ott = round3(ott);
      if (Number.isFinite(app)) prev.sg_app = round3(app);
      if (Number.isFinite(arg)) prev.sg_arg = round3(arg);
      if (Number.isFinite(putt)) prev.sg_putt = round3(putt);
      if (Number.isFinite(t2g)) prev.sg_t2g = round3(t2g);
    } else if (view === "Scoring") {
      const bob = num(row["BoB %"], NaN);
      const bog = num(row["Bogey %"], NaN);
      const dbl = num(row["Double Bogey+ %"], NaN);
      if (Number.isFinite(bob)) prev.bob_pct = round1(bob);
      if (Number.isFinite(bog)) prev.bogey_pct = round1(bog);
      if (Number.isFinite(dbl)) prev.double_bogey_pct = round1(dbl);
    } else if (view === "Off the Tee") {
      const acc = num(row["Dr Accuracy %"], NaN);
      if (Number.isFinite(acc)) prev.dr_accuracy = round1(acc);
    } else if (view === "Approach") {
      const gir = num(row["GIR %"], NaN);
      if (Number.isFinite(gir)) prev.gir_pct = round1(gir);
    }
    byPga.set(pga, prev);
  }
}

export async function fetchRhOuStatsBundle(filters = {}, opts = {}) {
  const sessionKey = opts.sessionKey ?? resolveBetspertsSessionKey();
  if (!sessionKey) {
    return { ok: false, error: "missing_betsperts_session", players: {}, filters };
  }
  const base = rhFiltersToLoadOverrides(filters);
  const byPga = new Map();
  const viewStatus = {};
  for (const { view } of RH_OU_VIEWS) {
    const res = await rhApiPost("/user/rabbit_hole/load", rabbitHoleLoadBody({ ...base, view }), sessionKey);
    viewStatus[view] = res.status;
    const rows = Array.isArray(res.json?.data) ? res.json.data : [];
    if (res.ok && rows.length) mergeRhOuViewRows(byPga, rows, view);
  }
  const pgaToDg = opts.pgaToDg || loadPgaToDgMap();
  const players = {};
  for (const [pga, row] of byPga) {
    const dg = pgaToDg.get(pga);
    if (!Number.isFinite(dg)) continue;
    players[String(dg)] = { ...row, dg_id: dg };
  }
  return {
    ok: true,
    generatedAt: new Date().toISOString(),
    source: "betsperts rabbit_hole/load",
    filters: { ...base, timeframe: filters.timeframe || "all" },
    viewStatus,
    n: Object.keys(players).length,
    players,
  };
}

export function loadRhFilterOptionsFromDisk() {
  const regionsPath = join(ROOT, "data", "betsperts", "rabbit-hole", "regions.json");
  const typesPath = join(ROOT, "data", "betsperts", "rabbit-hole", "types.json");
  const coursesPath = join(ROOT, "data", "betsperts", "courses", "index.json");
  const regions = [];
  const types = [];
  const courses = [];
  try {
    if (existsSync(regionsPath)) {
      const j = JSON.parse(readFileSync(regionsPath, "utf8"));
      const arr = j?.json?.data || j?.data || [];
      if (Array.isArray(arr)) regions.push(...arr.map(String));
    }
  } catch {
    /* ignore */
  }
  try {
    if (existsSync(typesPath)) {
      const j = JSON.parse(readFileSync(typesPath, "utf8"));
      const arr = j?.json?.data || j?.data || [];
      if (Array.isArray(arr)) types.push(...arr.map(String));
    }
  } catch {
    /* ignore */
  }
  try {
    if (existsSync(coursesPath)) {
      const j = JSON.parse(readFileSync(coursesPath, "utf8"));
      for (const c of j?.data || []) {
        const num = String(c.CourseNum ?? c.courseNum ?? "").trim();
        const name = String(c.CourseName ?? c.courseName ?? "").trim();
        if (!num || !name) continue;
        courses.push({ id: num, name });
      }
    }
  } catch {
    /* ignore */
  }
  courses.sort((a, b) => a.name.localeCompare(b.name));
  const year = new Date().getFullYear();
  const timeframes = [
    ...RH_TIMEFRAME_PRESETS,
    { value: `season:${year}`, label: `${year} season` },
    { value: `season:${year - 1}`, label: `${year - 1} season` },
  ];
  return { timeframes, regions, types, courses };
}

export { WEB, ROOT, API };
