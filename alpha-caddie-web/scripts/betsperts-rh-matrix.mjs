/**
 * Full Rabbit Hole matrix: every UI view × Average/Total × all seasons + rolling windows.
 * Each slice is an All-Players `/user/rabbit_hole/load` response (all golfers in that filter).
 *
 * Writes: ../data/betsperts/rabbit-hole/matrix/
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync, renameSync, unlinkSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");
const OUT_DIR = join(ROOT, "data", "betsperts", "rabbit-hole", "matrix");
const API = String(process.env.BETSPERTS_API_BASE || "https://api.betspertsgolf.com").replace(/\/$/, "");

/** Views that return 200 on rabbit_hole/load (matches RH UI skill tabs). */
export const RH_MATRIX_VIEWS = Object.freeze([
  "Strokes Gained",
  "Off the Tee",
  "Approach",
  "Around the Green",
  "Putting",
  "Scoring",
]);

export const RH_MATRIX_DISPLAYS = Object.freeze(["Average", "Total"]);

/** Rolling round windows (API uses `round`, not date:"Last N Rounds"). */
export const RH_MATRIX_ROUNDS = Object.freeze(["12", "24", "36", "50", "100"]);

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

export function sliceKey({ view, display, timeKey }) {
  const v = String(view || "")
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
  const d = String(display || "")
    .trim()
    .toLowerCase();
  const t = String(timeKey || "")
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9-]+/g, "-");
  return `${v}__${d}__${t}`;
}

export function seasonYearRange(opts = {}) {
  const end = Math.round(Number(opts.endYear) || new Date().getUTCFullYear());
  const start = Math.round(Number(opts.startYear) || 2015);
  const years = [];
  for (let y = start; y <= end; y++) years.push(y);
  return years;
}

/** Build the full list of matrix slices to pull. */
export function buildRhMatrixSlicePlan(opts = {}) {
  const views = opts.views || RH_MATRIX_VIEWS;
  const displays = opts.displays || RH_MATRIX_DISPLAYS;
  const rounds = opts.rounds || RH_MATRIX_ROUNDS;
  const years = opts.years || seasonYearRange(opts);
  const slices = [];

  for (const view of views) {
    for (const display of displays) {
      slices.push({
        view,
        display,
        timeKey: "all",
        label: `${view} · ${display} · All years`,
        overrides: { view, display, date: "All", season: [], round: "" },
      });
      for (const r of rounds) {
        slices.push({
          view,
          display,
          timeKey: `round-${r}`,
          label: `${view} · ${display} · Last ${r} rounds`,
          overrides: { view, display, date: "All", season: [], round: String(r) },
        });
      }
      for (const y of years) {
        slices.push({
          view,
          display,
          timeKey: `season-${y}`,
          label: `${view} · ${display} · ${y}`,
          overrides: { view, display, date: "All", season: [String(y)], round: "" },
        });
      }
    }
  }
  return slices;
}

function writeJsonAtomic(filePath, obj) {
  mkdirSync(dirname(filePath), { recursive: true });
  const text = JSON.stringify(obj) + "\n";
  const tmp = `${filePath}.${process.pid}.tmp`;
  writeFileSync(tmp, text, "utf8");
  try {
    renameSync(tmp, filePath);
  } catch {
    writeFileSync(filePath, text, "utf8");
    try {
      unlinkSync(tmp);
    } catch {
      /* ignore */
    }
  }
}

async function apiPost(path, body, sessionKey) {
  const res = await fetch(`${API}${path}`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Accept: "application/json",
      "User-Agent": "Mozilla/5.0 (compatible; AlphaCaddie/1.0; +rh-matrix)",
      Origin: "https://betspertsgolf.com",
      Referer: "https://betspertsgolf.com/the-rabbit-hole",
      session_key: sessionKey,
    },
    body: JSON.stringify(body),
  });
  const text = await res.text();
  let json = null;
  try {
    json = JSON.parse(text);
  } catch {
    json = null;
  }
  return { ok: res.ok, status: res.status, json, textLen: text.length };
}

function extractRows(json) {
  if (Array.isArray(json?.data)) return json.data;
  if (Array.isArray(json?.data?.players)) return json.data.players;
  return [];
}

/**
 * Pull full RH matrix into data/betsperts/rabbit-hole/matrix/.
 * @returns {Promise<object>} manifest summary
 */
export async function pullRhMatrix(opts = {}) {
  const sessionKey = opts.sessionKey || resolveBetspertsSessionKey();
  if (!sessionKey) {
    return { ok: false, error: "no_session", nSlices: 0 };
  }
  const outDir = opts.outDir || OUT_DIR;
  mkdirSync(join(outDir, "by-slice"), { recursive: true });

  let slices = buildRhMatrixSlicePlan(opts);
  if (opts.limitSlices > 0) slices = slices.slice(0, opts.limitSlices);

  const concurrency = Math.max(1, Math.min(6, Number(opts.concurrency || process.env.BETSPERTS_RH_CONCURRENCY || 3) || 3));
  const started = new Date().toISOString();
  const results = [];
  let idx = 0;

  async function worker() {
    while (idx < slices.length) {
      const i = idx++;
      const slice = slices[i];
      const key = sliceKey(slice);
      const body = rabbitHoleLoadBody(slice.overrides);
      let res = await apiPost("/user/rabbit_hole/load", body, sessionKey);
      let rows = extractRows(res.json);
      // Retry once on empty/5xx
      if ((!res.ok || (!rows.length && !String(slice.timeKey).startsWith("season-"))) && res.status !== 401) {
        await new Promise((r) => setTimeout(r, 400));
        res = await apiPost("/user/rabbit_hole/load", body, sessionKey);
        rows = extractRows(res.json);
      }
      const payload = {
        key,
        view: slice.view,
        display: slice.display,
        timeKey: slice.timeKey,
        label: slice.label,
        pulledAt: new Date().toISOString(),
        status: res.status,
        rowCount: rows.length,
        columns: rows[0] ? Object.keys(rows[0]) : [],
        rows,
      };
      writeJsonAtomic(join(outDir, "by-slice", `${key}.json`), payload);
      results.push({
        key,
        view: slice.view,
        display: slice.display,
        timeKey: slice.timeKey,
        status: res.status,
        rowCount: rows.length,
        bytes: res.textLen,
      });
      if ((i + 1) % 10 === 0 || i + 1 === slices.length) {
        console.log(`  rh-matrix ${i + 1}/${slices.length} last=${key} n=${rows.length} status=${res.status}`);
      }
    }
  }

  console.log(
    `[rh-matrix] pulling ${slices.length} slices (views=${RH_MATRIX_VIEWS.length} displays=2) concurrency=${concurrency} → ${outDir}`,
  );
  await Promise.all(Array.from({ length: concurrency }, () => worker()));

  const manifest = {
    generatedAt: new Date().toISOString(),
    started,
    finished: new Date().toISOString(),
    api: API,
    views: [...RH_MATRIX_VIEWS],
    displays: [...RH_MATRIX_DISPLAYS],
    rounds: [...RH_MATRIX_ROUNDS],
    years: seasonYearRange(opts),
    nSlices: results.length,
    nOk: results.filter((r) => r.status === 200 && r.rowCount > 0).length,
    nEmpty: results.filter((r) => r.status === 200 && r.rowCount === 0).length,
    nFail: results.filter((r) => r.status !== 200).length,
    slices: results,
  };
  writeJsonAtomic(join(outDir, "manifest.json"), manifest);
  console.log(
    `[rh-matrix] done ok=${manifest.nOk} empty=${manifest.nEmpty} fail=${manifest.nFail} / ${manifest.nSlices}`,
  );
  return { ok: true, ...manifest };
}
