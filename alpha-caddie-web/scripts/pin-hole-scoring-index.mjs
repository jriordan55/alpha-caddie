/**
 * Index: pin sheet hole setups ↔ field scoring vs par (from hole_data.csv).
 * Cached at data/pin_hole_scoring_index.json for fast Bayesian pin calibration.
 */
import { createReadStream, existsSync, readFileSync, statSync, writeFileSync } from "fs";
import path from "path";
import readline from "readline";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { holePinDifficulty, num } from "./pin-sheet-difficulty.mjs";
import {
  defaultPinLocationsRoot,
  loadPinLocationSheetByPath,
  loadPinLocationsIndex,
} from "./pin-locations-db.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const MODEL_ROOT = path.resolve(__dirname, "..", "..");

export function defaultPinHoleScoringIndexPath() {
  if (process.env.PIN_HOLE_SCORING_INDEX_PATH) {
    return path.resolve(process.env.PIN_HOLE_SCORING_INDEX_PATH);
  }
  return path.join(MODEL_ROOT, "data", "pin_hole_scoring_index.json");
}

export function defaultHoleDataCsvPath() {
  if (process.env.HOLE_DATA_CSV) return path.resolve(process.env.HOLE_DATA_CSV);
  return path.join(MODEL_ROOT, "data", "hole_data.csv");
}

function normEvent(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (c === '"') {
      q = !q;
      continue;
    }
    if (c === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += c;
  }
  out.push(cur);
  return out;
}

function indexIsFresh(indexPath, pinRoot, holesCsv) {
  if (!existsSync(indexPath)) return false;
  try {
    const idx = JSON.parse(readFileSync(indexPath, "utf8"));
    const built = Date.parse(idx.built_at || "");
    if (!Number.isFinite(built)) return false;
    const pinIdx = path.join(pinRoot, "index.json");
    let pinM = 0;
    if (existsSync(pinIdx)) pinM = statSync(pinIdx).mtimeMs;
    let holeM = 0;
    if (existsSync(holesCsv)) holeM = statSync(holesCsv).mtimeMs;
    return idx.source_pin_mtime >= pinM && idx.source_hole_data_mtime >= holeM;
  } catch {
    return false;
  }
}

function loadPinSheetCatalog(pinRoot) {
  const catalog = loadPinLocationsIndex(pinRoot);
  const sheets = [];
  const eventRoundKeys = new Set();
  for (const ent of catalog.entries || []) {
    if (!ent?.path) continue;
    const sheet = loadPinLocationSheetByPath(pinRoot, ent.path);
    if (!sheet?.holes?.length) continue;
    const eventNorm = normEvent(sheet.event_name_ref || ent.event_name_ref || "");
    if (!eventNorm) continue;
    const courseKey = sheet.course_key || normCourseNameKey(sheet.course_name);
    sheets.push({
      key: ent.key,
      courseKey,
      courseName: sheet.course_name || ent.course_name,
      playDate: sheet.play_date || ent.play_date,
      round: sheet.round_num || ent.round_num,
      eventNorm,
      eventName: sheet.event_name_ref || ent.event_name_ref || "",
      holes: sheet.holes,
    });
    eventRoundKeys.add(`${eventNorm}|${sheet.round_num || ent.round_num}`);
  }
  return { sheets, eventRoundKeys };
}

function finalizeAgg(agg) {
  const observations = [];
  for (const rec of agg.values()) {
    if (rec.n < 20) continue;
    observations.push({
      course_key: rec.course_key,
      event_norm: rec.event_norm,
      play_date: rec.play_date,
      round: rec.round,
      hole: rec.hole,
      front: rec.front,
      side: rec.side,
      depth: rec.depth,
      hazard: rec.hazard,
      pin_score: Math.round(rec.pin_score * 1000) / 1000,
      vs_par: Math.round((rec.sum_vs_par / rec.n) * 1000) / 1000,
      n: rec.n,
    });
  }
  return observations;
}

/**
 * @param {{ force?: boolean, pinRoot?: string, holesCsv?: string, indexPath?: string }} [opts]
 */
export async function buildPinHoleScoringIndex(opts = {}) {
  const pinRoot = opts.pinRoot || defaultPinLocationsRoot();
  const holesCsv = opts.holesCsv || defaultHoleDataCsvPath();
  const indexPath = opts.indexPath || defaultPinHoleScoringIndexPath();

  if (!existsSync(holesCsv)) {
    throw new Error(`[pin-hole-index] missing ${holesCsv}`);
  }

  const { sheets, eventRoundKeys } = loadPinSheetCatalog(pinRoot);
  const sheetByEventRound = new Map();
  for (const s of sheets) {
    sheetByEventRound.set(`${s.eventNorm}|${s.round}`, s);
  }

  const agg = new Map();
  const rl = readline.createInterface({ input: createReadStream(holesCsv), crlfDelay: Infinity });
  let headers = null;
  let iEv = -1;
  let iRd = -1;
  let iHole = -1;
  let iPar = -1;
  let iScore = -1;

  for await (const line of rl) {
    if (!headers) {
      headers = parseCsvLine(line);
      iEv = headers.indexOf("tournament_name");
      iRd = headers.indexOf("round");
      iHole = headers.indexOf("hole");
      iPar = headers.indexOf("par");
      iScore = headers.indexOf("score");
      continue;
    }
    const cols = parseCsvLine(line);
    const evNorm = normEvent(cols[iEv]);
    const rd = Math.round(num(cols[iRd], NaN));
    if (!evNorm || !Number.isFinite(rd)) continue;
    const erKey = `${evNorm}|${rd}`;
    if (!eventRoundKeys.has(erKey)) continue;

    let sheet = sheetByEventRound.get(erKey);
    if (!sheet) {
      for (const s of sheets) {
        if (s.round !== rd) continue;
        if (eventsLikelySame(s.eventNorm, evNorm)) {
          sheet = s;
          break;
        }
      }
    }
    if (!sheet) continue;

    const hole = Math.round(num(cols[iHole], NaN));
    const par = num(cols[iPar], NaN);
    const score = num(cols[iScore], NaN);
    if (!Number.isFinite(hole) || !Number.isFinite(par) || !Number.isFinite(score)) continue;

    const pinHole = sheet.holes.find((h) => Math.round(num(h.hole, NaN)) === hole);
    if (!pinHole) continue;
    const d = holePinDifficulty(pinHole);

    const obsKey = `${sheet.key}|${hole}`;
    if (!agg.has(obsKey)) {
      agg.set(obsKey, {
        course_key: sheet.courseKey,
        event_norm: sheet.eventNorm,
        play_date: sheet.playDate,
        round: sheet.round,
        hole,
        front: num(pinHole.pin_from_front_yds, null),
        side: num(pinHole.pin_from_side_yds, null),
        depth: num(pinHole.green_depth_yds, null),
        hazard: Boolean(pinHole.near_hazard),
        pin_score: d.score,
        sum_vs_par: 0,
        n: 0,
      });
    }
    const rec = agg.get(obsKey);
    rec.sum_vs_par += score - par;
    rec.n++;
  }

  const observations = finalizeAgg(agg);
  const payload = {
    version: 1,
    built_at: new Date().toISOString(),
    source_pin_mtime: existsSync(path.join(pinRoot, "index.json"))
      ? statSync(path.join(pinRoot, "index.json")).mtimeMs
      : 0,
    source_hole_data_mtime: statSync(holesCsv).mtimeMs,
    observation_count: observations.length,
    sheet_count: sheets.length,
    observations,
  };

  writeFileSync(indexPath, `${JSON.stringify(payload, null, 2)}\n`, "utf8");
  console.log(
    `[pin-hole-index] Wrote ${observations.length} hole observations from ${sheets.length} pin sheets → ${indexPath}`,
  );
  return payload;
}

export async function loadPinHoleScoringIndex(opts = {}) {
  const pinRoot = opts.pinRoot || defaultPinLocationsRoot();
  const holesCsv = opts.holesCsv || defaultHoleDataCsvPath();
  const indexPath = opts.indexPath || defaultPinHoleScoringIndexPath();
  const force = opts.force === true || String(process.env.GOLF_REBUILD_PIN_HOLE_INDEX || "").trim() === "1";

  if (!force && indexIsFresh(indexPath, pinRoot, holesCsv)) {
    try {
      return JSON.parse(readFileSync(indexPath, "utf8"));
    } catch {
      /* rebuild */
    }
  }
  return buildPinHoleScoringIndex({ pinRoot, holesCsv, indexPath });
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1]);
if (isMain) {
  loadPinHoleScoringIndex().catch((e) => {
    console.error(e.message || e);
    process.exit(1);
  });
}
