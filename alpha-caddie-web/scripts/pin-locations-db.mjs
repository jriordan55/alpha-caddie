/**
 * Pin location sheets keyed by course + play date + round (not tournament).
 * Grid scale on official ShotLink sheets: each square side = 5 yards.
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { historyRoundChartUtcIsoDay } from "./history-round-dates.mjs";
import { roundEventCompletedMdYFromEventEnd } from "./history-round-dates.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = path.resolve(WEB_ROOT, "..");

export const PIN_GRID_YARDS_PER_SQUARE = 5;

export function defaultPinLocationsRoot() {
  if (process.env.PIN_LOCATIONS_DIR) return path.resolve(process.env.PIN_LOCATIONS_DIR);
  for (const p of [path.join(MODEL_ROOT, "data", "pin_locations"), path.join(WEB_ROOT, "data", "pin_locations")]) {
    if (fs.existsSync(p)) return p;
  }
  return path.join(MODEL_ROOT, "data", "pin_locations");
}

export function pinLocationKey(courseKey, playDateIso, roundNum) {
  const ck = String(courseKey || "").trim();
  const d = String(playDateIso || "").trim().slice(0, 10);
  const rnd = Math.round(Number(roundNum));
  if (!ck || !/^\d{4}-\d{2}-\d{2}$/.test(d) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) return "";
  return `${ck}|${d}|${rnd}`;
}

export function courseKeyFromName(courseName) {
  return normCourseNameKey(courseName);
}

/** M/D/YYYY or ISO → YYYY-MM-DD */
export function playDateIsoFromMdY(mdy) {
  return historyRoundChartUtcIsoDay({ event_completed: String(mdy || "").trim() });
}

/** Play date for a tournament round from field start or event-end anchor. */
export function playDateIsoForRound(ctx, roundNum) {
  const rnd = Math.round(Number(roundNum));
  if (!Number.isFinite(rnd) || rnd < 1) return "";

  const ds = String(ctx?.datagolf_field_date_start ?? ctx?.field_date_start ?? "").trim();
  const m = ds.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (m) {
    const y = parseInt(m[1], 10);
    const mo = parseInt(m[2], 10);
    const d = parseInt(m[3], 10);
    const ms = Date.UTC(y, mo - 1, d) + (rnd - 1) * 86400000;
    const dt = new Date(ms);
    return `${dt.getUTCFullYear()}-${String(dt.getUTCMonth() + 1).padStart(2, "0")}-${String(dt.getUTCDate()).padStart(2, "0")}`;
  }

  const tour = String(ctx?.tour || "pga").toLowerCase();
  const eventEnd = String(ctx?.event_completed ?? ctx?.event_end ?? "").trim();
  const mdy = roundEventCompletedMdYFromEventEnd(eventEnd, rnd, tour);
  return playDateIsoFromMdY(mdy);
}

export function enrichHolePinGrid(hole) {
  const h = { ...hole };
  const front = Number(h.pin_from_front_yds);
  const side = Number(h.pin_from_side_yds);
  if (Number.isFinite(front)) {
    h.pin_grid_from_front = Math.round((front / PIN_GRID_YARDS_PER_SQUARE) * 100) / 100;
  }
  if (Number.isFinite(side)) {
    h.pin_grid_from_side = Math.round((side / PIN_GRID_YARDS_PER_SQUARE) * 100) / 100;
  }
  h.grid_yards_per_square = PIN_GRID_YARDS_PER_SQUARE;
  return h;
}

export function normalizePinLocationSheet(raw, defaults = {}) {
  const courseName = String(raw?.course_name ?? defaults.course_name ?? "").trim();
  const courseKey = courseKeyFromName(raw?.course_key ? raw.course_key : courseName);
  const playDate = String(raw?.play_date ?? defaults.play_date ?? "")
    .trim()
    .slice(0, 10);
  const roundNum = Math.round(Number(raw?.round_num ?? raw?.round ?? defaults.round_num ?? NaN));
  const holesIn = Array.isArray(raw?.holes) ? raw.holes : [];
  const holes = holesIn
    .map((h) => {
      const hole = Math.round(Number(h?.hole));
      if (!Number.isFinite(hole) || hole < 1 || hole > 18) return null;
      return enrichHolePinGrid({
        hole,
        green_depth_yds: numOrNull(h.green_depth_yds),
        pin_from_front_yds: numOrNull(h.pin_from_front_yds),
        pin_from_side_yds: numOrNull(h.pin_from_side_yds),
        pin_side: String(h.pin_side ?? h.side ?? "").trim().toUpperCase().slice(0, 1) || null,
        near_hazard: Boolean(h.near_hazard),
        note: String(h.note ?? "").trim(),
      });
    })
    .filter(Boolean)
    .sort((a, b) => a.hole - b.hole);

  return {
    course_key: courseKey,
    course_name: courseName,
    play_date: playDate,
    round_num: roundNum,
    event_name_ref: String(raw?.event_name_ref ?? raw?.event_name ?? defaults.event_name_ref ?? "").trim(),
    grid_yards_per_square: PIN_GRID_YARDS_PER_SQUARE,
    source_image: String(raw?.source_image ?? defaults.source_image ?? "").trim(),
    source: String(raw?.source ?? "pin_locations_db").trim(),
    imported_at: raw?.imported_at || new Date().toISOString(),
    holes,
  };
}

function numOrNull(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : null;
}

export function sheetFileName(sheet) {
  const key = pinLocationKey(sheet.course_key, sheet.play_date, sheet.round_num);
  if (!key) return "";
  return `${sheet.play_date}_r${sheet.round_num}.json`;
}

export function sheetRelativePath(sheet) {
  const fn = sheetFileName(sheet);
  if (!fn || !sheet.course_key) return "";
  return path.join("sheets", sheet.course_key, fn);
}

export function loadPinLocationsIndex(rootDir = defaultPinLocationsRoot()) {
  const indexPath = path.join(rootDir, "index.json");
  if (!fs.existsSync(indexPath)) {
    return { version: 1, grid_yards_per_square: PIN_GRID_YARDS_PER_SQUARE, entries: [] };
  }
  try {
    return JSON.parse(fs.readFileSync(indexPath, "utf8"));
  } catch {
    return { version: 1, grid_yards_per_square: PIN_GRID_YARDS_PER_SQUARE, entries: [] };
  }
}

export function savePinLocationsIndex(index, rootDir = defaultPinLocationsRoot()) {
  fs.mkdirSync(rootDir, { recursive: true });
  fs.writeFileSync(path.join(rootDir, "index.json"), JSON.stringify(index, null, 2));
}

export function loadPinLocationSheetByPath(rootDir, relPath) {
  const full = path.join(rootDir, relPath);
  if (!fs.existsSync(full)) return null;
  try {
    return normalizePinLocationSheet(JSON.parse(fs.readFileSync(full, "utf8")));
  } catch {
    return null;
  }
}

export function loadPinLocationSheet(courseName, playDateIso, roundNum, rootDir = defaultPinLocationsRoot()) {
  const ck = courseKeyFromName(courseName);
  const key = pinLocationKey(ck, playDateIso, roundNum);
  if (!key) return null;

  const index = loadPinLocationsIndex(rootDir);
  const hit = (index.entries || []).find((e) => e.key === key);
  if (hit?.path) {
    const sheet = loadPinLocationSheetByPath(rootDir, hit.path);
    if (sheet?.holes?.length) return sheet;
  }

  const fn = `${String(playDateIso).slice(0, 10)}_r${Math.round(Number(roundNum))}.json`;
  const direct = path.join(rootDir, "sheets", ck, fn);
  if (fs.existsSync(direct)) {
    try {
      return normalizePinLocationSheet(JSON.parse(fs.readFileSync(direct, "utf8")));
    } catch {
      return null;
    }
  }
  return null;
}

export function savePinLocationSheet(sheet, rootDir = defaultPinLocationsRoot()) {
  const normalized = normalizePinLocationSheet(sheet);
  if (!normalized.course_key || !normalized.play_date || !normalized.round_num || normalized.holes.length < 9) {
    throw new Error("pin sheet: need course_key, play_date, round_num, and ≥9 holes");
  }

  const rel = sheetRelativePath(normalized);
  const full = path.join(rootDir, rel);
  fs.mkdirSync(path.dirname(full), { recursive: true });
  fs.writeFileSync(full, JSON.stringify(normalized, null, 2));

  const key = pinLocationKey(normalized.course_key, normalized.play_date, normalized.round_num);
  const index = loadPinLocationsIndex(rootDir);
  const entries = Array.isArray(index.entries) ? index.entries.filter((e) => e.key !== key) : [];
  entries.push({
    key,
    course_key: normalized.course_key,
    course_name: normalized.course_name,
    play_date: normalized.play_date,
    round_num: normalized.round_num,
    event_name_ref: normalized.event_name_ref,
    path: rel.replace(/\\/g, "/"),
    source_image: normalized.source_image,
    hole_count: normalized.holes.length,
    imported_at: normalized.imported_at,
  });
  entries.sort((a, b) =>
    `${a.course_key}|${a.play_date}|${a.round_num}`.localeCompare(`${b.course_key}|${b.play_date}|${b.round_num}`),
  );
  index.entries = entries;
  index.updated_at = new Date().toISOString();
  savePinLocationsIndex(index, rootDir);
  return { key, path: rel, sheet: normalized };
}

/** Map key → full sheet for embedding in player_round_history.json */
export function loadAllPinLocationSheetsMap(rootDir = defaultPinLocationsRoot()) {
  const index = loadPinLocationsIndex(rootDir);
  const map = {};
  for (const e of index.entries || []) {
    if (!e?.key || !e?.path) continue;
    const sheet = loadPinLocationSheetByPath(rootDir, e.path);
    if (sheet?.holes?.length) map[e.key] = sheet;
  }
  return map;
}

/** Pin sheet JSON shape expected by apply-pin-sheet / pin-sheet-difficulty */
export function pinSheetForProjections(sheet) {
  if (!sheet?.holes?.length) return null;
  return {
    event_name: sheet.event_name_ref || sheet.course_name,
    round: sheet.round_num,
    source: sheet.source || "pin_locations_db",
    course_name: sheet.course_name,
    course_key: sheet.course_key,
    play_date: sheet.play_date,
    grid_yards_per_square: sheet.grid_yards_per_square ?? PIN_GRID_YARDS_PER_SQUARE,
    holes: sheet.holes.map((h) => ({
      hole: h.hole,
      green_depth_yds: h.green_depth_yds,
      pin_from_front_yds: h.pin_from_front_yds,
      pin_from_side_yds: h.pin_from_side_yds,
      pin_side: h.pin_side,
      near_hazard: h.near_hazard,
      note: h.note,
      pin_grid_from_front: h.pin_grid_from_front,
      pin_grid_from_side: h.pin_grid_from_side,
    })),
  };
}

export function lookupPinSheetForProjections(payload, rootDir = defaultPinLocationsRoot()) {
  const course = String(payload?.course_used ?? payload?.event_name ?? "").trim();
  const rnd = Math.round(Number(payload?.display_round ?? 1));
  const playDate = playDateIsoForRound(payload, rnd);
  if (!course || !playDate) return null;
  const sheet = loadPinLocationSheet(course, playDate, rnd, rootDir);
  return sheet ? pinSheetForProjections(sheet) : null;
}

export function attachPinToHoleRows(holeRows, pinSheet) {
  if (!Array.isArray(holeRows) || !pinSheet?.holes?.length) return holeRows;
  const byHole = new Map(pinSheet.holes.map((h) => [h.hole, h]));
  return holeRows.map((row) => {
    const pin = byHole.get(Math.round(Number(row.hole)));
    if (!pin) return row;
    return {
      ...row,
      green_depth_yds: pin.green_depth_yds,
      pin_from_front_yds: pin.pin_from_front_yds,
      pin_from_side_yds: pin.pin_from_side_yds,
      pin_side: pin.pin_side,
      pin_near_hazard: pin.near_hazard,
      pin_grid_from_front: pin.pin_grid_from_front,
      pin_grid_from_side: pin.pin_grid_from_side,
      grid_yards_per_square: pin.grid_yards_per_square ?? PIN_GRID_YARDS_PER_SQUARE,
    };
  });
}
