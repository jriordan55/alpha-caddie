/**
 * Context signals for backtest rows: weather, SG, tee wave, pin sheet, course traits.
 */
import { existsSync, readdirSync, readFileSync } from "fs";
import { join } from "path";
import { normCourseNameKey } from "./course-name-key.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";
import {
  effectiveWeatherForRow,
  weatherDifficultyDeltaFromSnapshot,
} from "./weather-projection-adjustments.mjs";

export const EXPORT_SIGNAL_COLS = [
  "weather_wind_mph",
  "weather_temp_f",
  "weather_condition",
  "weather_difficulty",
  "sg_ott",
  "sg_app",
  "tee_wave",
  "pin_sheet_active",
  "gir_minus_fw",
  "course_fw_width",
];

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function fmtSignalCell(key, v) {
  if (key === "pin_sheet_active") return v ? "1" : v === 0 || v === false ? "0" : "";
  if (key === "tee_wave" || key === "weather_condition") return v ? String(v) : "";
  if (!Number.isFinite(num(v, NaN))) return "";
  const n = num(v, NaN);
  if (key === "weather_difficulty" || key === "sg_ott" || key === "sg_app" || key === "gir_minus_fw") {
    return String(Math.round(n * 1000) / 1000);
  }
  if (key === "course_fw_width") return String(Math.round(n * 10) / 10);
  return String(Math.round(n * 10) / 10);
}

export function signalCellsFromObject(sig = {}) {
  const out = {};
  for (const col of EXPORT_SIGNAL_COLS) {
    out[col] = fmtSignalCell(col, sig[col]);
  }
  return out;
}

let courseTableCache = null;

export function loadCourseTableByKey(webRoot) {
  if (courseTableCache) return courseTableCache;
  const p = join(webRoot, "data", "course_table.csv");
  const byKey = new Map();
  if (!existsSync(p)) {
    courseTableCache = byKey;
    return byKey;
  }
  const text = readFileSync(p, "utf8");
  const lines = text.split(/\r?\n/).filter(Boolean);
  const hdr = lines[0].split(",");
  const fwIdx = hdr.indexOf("fw_width");
  for (let i = 1; i < lines.length; i++) {
    const m = lines[i].match(/^"([^"]*)"/);
    const course = m ? m[1] : lines[i].split(",")[0];
    const cols = parseCsvRowSimple(lines[i]);
    const fw = fwIdx >= 0 ? num(cols[fwIdx], NaN) : NaN;
    const k = normCourseNameKey(course);
    if (k) byKey.set(k, { fw_width: fw });
  }
  courseTableCache = byKey;
  return byKey;
}

function courseFwWidth(webRoot, courseUsed) {
  const tbl = loadCourseTableByKey(webRoot);
  const k = normCourseNameKey(courseUsed);
  if (tbl.has(k)) return num(tbl.get(k).fw_width, NaN);
  for (const [ck, row] of tbl) {
    if (k.includes(ck) || ck.includes(k)) return num(row.fw_width, NaN);
  }
  return NaN;
}

let pinRoundIndexCache = null;

export function loadPinSheetRoundIndex(webRoot) {
  if (pinRoundIndexCache) return pinRoundIndexCache;
  const index = new Set();
  const root = join(webRoot, "data", "pin_locations", "sheets");
  if (!existsSync(root)) {
    pinRoundIndexCache = index;
    return index;
  }
  function walk(dir) {
    for (const ent of readdirSync(dir, { withFileTypes: true })) {
      const p = join(dir, ent.name);
      if (ent.isDirectory()) {
        walk(p);
        continue;
      }
      if (!/\.json$/i.test(ent.name)) continue;
      const m = ent.name.match(/^(\d{4}-\d{2}-\d{2})_r(\d)\.json$/i);
      if (!m) continue;
      const courseKey = normCourseNameKey(dir.split(/[/\\]/).pop());
      const playDate = m[1];
      const rnd = Number(m[2]);
      index.add(`${courseKey}|${rnd}|${playDate}`);
      try {
        const j = JSON.parse(readFileSync(p, "utf8"));
        const evRef = normCourseNameKey(String(j.event_name_ref || "").trim());
        if (evRef) index.add(`${evRef}|${rnd}|${playDate}`);
      } catch {
        /* ignore */
      }
    }
  }
  walk(root);
  pinRoundIndexCache = index;
  return index;
}

export function pinSheetActiveFor(webRoot, { courseUsed, eventName, round, projUpdatedAt }) {
  const index = loadPinSheetRoundIndex(webRoot);
  const rnd = Math.round(num(round, NaN));
  if (!Number.isFinite(rnd)) return 0;
  const courseKey = normCourseNameKey(courseUsed);
  const evKey = normCourseNameKey(eventName);
  const base = String(projUpdatedAt || "").slice(0, 10);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(base)) return 0;
  const dates = [base];
  try {
    const t = Date.parse(`${base}T12:00:00Z`);
    for (const off of [-1, 1, -2, 2]) {
      dates.push(new Date(t + off * 86400000).toISOString().slice(0, 10));
    }
  } catch {
    /* ignore */
  }
  for (const d of dates) {
    if (index.has(`${courseKey}|${rnd}|${d}`)) return 1;
    if (evKey && index.has(`${evKey}|${rnd}|${d}`)) return 1;
  }
  return 0;
}

export function teeWaveLabelFromRow(row) {
  const wave = teeWaveFromTeetimeAndLabel(row?.dg_teetime_local ?? row?.teetime, row?.dg_tee_wave);
  const w = String(wave || "").trim().toLowerCase();
  if (w === "morning" || w === "am") return "morning";
  if (w === "afternoon" || w === "pm") return "afternoon";
  return w || "";
}

export function extractSignalsFromPlayerRow(player, payload, webRoot) {
  const p = player && typeof player === "object" ? player : {};
  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : payload;
  const w = effectiveWeatherForRow(p);
  const diff = weatherDifficultyDeltaFromSnapshot(w);
  const gir = num(p.gir, NaN);
  const fw = num(p.fairways, NaN);
  let pinActive = 0;
  const ps = meta?.pin_sheet;
  if (ps && (ps.apply_to_projections === true || String(ps.apply_to_projections).toLowerCase() === "true")) {
    const psRnd = Math.round(num(ps.round_num ?? ps.display_round, NaN));
    const pRnd = Math.round(num(p.round, NaN));
    if (!Number.isFinite(psRnd) || psRnd === pRnd) pinActive = 1;
  }
  if (!pinActive && webRoot) {
    pinActive = pinSheetActiveFor(webRoot, {
      courseUsed: payload?.course_used || meta?.course_used,
      eventName: payload?.event_name,
      round: p.round,
      projUpdatedAt: payload?.updated_at || meta?.updated_at,
    });
  }
  return {
    weather_wind_mph: num(w.windMph, NaN),
    weather_temp_f: num(w.tempF, NaN),
    weather_condition: String(w.condition || "").toLowerCase() || "",
    weather_difficulty: Number.isFinite(diff) ? diff : NaN,
    sg_ott: num(p.sg_ott, NaN),
    sg_app: num(p.sg_app, NaN),
    tee_wave: teeWaveLabelFromRow(p),
    pin_sheet_active: pinActive,
    gir_minus_fw: Number.isFinite(gir) && Number.isFinite(fw) ? gir - fw : NaN,
    course_fw_width: webRoot ? courseFwWidth(webRoot, payload?.course_used || meta?.course_used) : NaN,
  };
}

export function extractSignalsFromHistRow(row, webRoot, ctx = {}) {
  const wind = num(row.weather_wind_mph ?? row.wind_mph, NaN);
  const temp = num(row.weather_temp_f ?? row.temp_f, NaN);
  const cond = String(row.weather_condition ?? row.condition ?? "").toLowerCase();
  let diff = NaN;
  if (Number.isFinite(wind)) {
    diff = weatherDifficultyDeltaFromSnapshot({
      tempF: Number.isFinite(temp) ? temp : 72,
      windMph: wind,
      humidityPct: num(row.weather_humidity, 55),
      condition: cond || "default",
    });
  }
  const girRaw = num(row.gir, NaN);
  const fwRate = num(row.driving_acc, NaN);
  const fwHoles = num(ctx.fairwayHoles, 14) || 14;
  const fwCount = Number.isFinite(fwRate) && fwRate <= 1 ? fwRate * fwHoles : NaN;
  const girCount = Number.isFinite(girRaw) && girRaw <= 1 ? girRaw * 18 : girRaw;
  const pinActive = webRoot
    ? pinSheetActiveFor(webRoot, {
        courseUsed: ctx.courseUsed || row.course_name,
        eventName: ctx.eventName || row.event_name,
        round: num(row.round_num, NaN),
        projUpdatedAt: ctx.projUpdatedAt || row.event_completed || row.event_date,
      })
    : 0;
  return {
    weather_wind_mph: wind,
    weather_temp_f: temp,
    weather_condition: cond,
    weather_difficulty: diff,
    sg_ott: num(row.sg_ott, NaN),
    sg_app: num(row.sg_app, NaN),
    tee_wave: teeWaveLabelFromRow(row),
    pin_sheet_active: pinActive,
    gir_minus_fw:
      Number.isFinite(girCount) && Number.isFinite(fwCount) ? girCount - fwCount : NaN,
    course_fw_width: webRoot ? courseFwWidth(webRoot, ctx.courseUsed || row.course_name) : NaN,
  };
}

export function alignDetailCsvContent(text, targetHeaderLine) {
  const lines = String(text || "").split(/\r?\n/).filter(Boolean);
  if (!lines.length) return targetHeaderLine;
  const oldHeader = lines[0].split(",");
  const newHeader = targetHeaderLine.replace(/\n$/, "").split(",");
  if (oldHeader.length === newHeader.length) {
    return lines.join("\n") + "\n";
  }
  const oldIdx = Object.fromEntries(oldHeader.map((h, i) => [h, i]));
  const out = [targetHeaderLine.replace(/\n$/, "")];
  for (let i = 1; i < lines.length; i++) {
    const cells = parseCsvRowSimple(lines[i]);
    const row = new Array(newHeader.length).fill("");
    for (let j = 0; j < newHeader.length; j++) {
      const oi = oldIdx[newHeader[j]];
      if (oi >= 0 && oi < cells.length) row[j] = cells[oi];
    }
    out.push(row.map(csvCell).join(","));
  }
  return out.join("\n") + "\n";
}

function parseCsvRowSimple(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
      continue;
    }
    if (ch === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  out.push(cur);
  return out;
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

export function buildDetailHeader(baseCols) {
  return baseCols.concat(EXPORT_SIGNAL_COLS).join(",") + "\n";
}
