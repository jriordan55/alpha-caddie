#!/usr/bin/env node
/**
 * Merge DataGolf historical-raw-data/rounds into repo data/historical_rounds_all.csv (no R).
 * Default: refresh every calendar year from 2004 through current (PGA); LIV from 2017 (skipped in-loop).
 *
 * Env:
 *   DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json (apiKey)
 *   GOLF_MODEL_DIR — repo root (parent of alpha-caddie-web). Default: parent of this package.
 *   GOLF_HISTORICAL_ROUNDS_TOURS — comma-separated (default: pga,liv). Use "pga" for PGA only.
 *   GOLF_HISTORICAL_ROUNDS_YEARS — override year list
 *   GOLF_HISTORICAL_ROUNDS_LIGHT=1 — only current + prior calendar year (fast; trims CSV to those years)
 *   Default without LIGHT: fetch 2004–current (PGA); LIV rows start 2017 (skipped automatically per tour).
 *   GOLF_ROUNDS_PREFER_CSV_FIRST / GOLF_ROUNDS_PREFER_JSON_FIRST — same idea as live_data.R
 *   GOLF_DG_ROUNDS_DELAY_MS — ms between each tour/year request (default 1500; reduces 429s)
 *   GOLF_DG_MAX_ATTEMPTS — retries per request on 429/5xx (default 12)
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = process.env.GOLF_MODEL_DIR ? path.resolve(process.env.GOLF_MODEL_DIR) : path.resolve(WEB_ROOT, "..");

const BASE = "https://feeds.datagolf.com/historical-raw-data/rounds";
/** When trimming the on-disk CSV before merge, never drop seasons before this year. */
const FIRST_HIST_YEAR = 2004;
/** Rolling window for GOLF_HISTORICAL_ROUNDS_LIGHT=1 only (years to keep + fetch). */
const LIGHT_YEARS = 2;

const COL_ORDER = [
  "tour",
  "year",
  "season",
  "event_completed",
  "event_name",
  "event_id",
  "player_name",
  "dg_id",
  "fin_text",
  "round_num",
  "course_name",
  "course_num",
  "course_par",
  "start_hole",
  "teetime",
  "round_score",
  "sg_putt",
  "sg_arg",
  "sg_app",
  "sg_ott",
  "sg_t2g",
  "sg_total",
  "driving_dist",
  "driving_acc",
  "gir",
  "scrambling",
  "prox_rgh",
  "prox_fw",
  "great_shots",
  "poor_shots",
  "eagles_or_better",
  "birdies",
  "pars",
  "bogies",
  "doubles_or_worse",
];

function loadApiKey() {
  const env = (process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = path.join(WEB_ROOT, "datagolf.local.json");
  if (fs.existsSync(p)) {
    try {
      const j = JSON.parse(fs.readFileSync(p, "utf8"));
      return String(j.apiKey || j.key || "").trim();
    } catch {
      return "";
    }
  }
  return "";
}

function minYearForTour(tour) {
  const t = String(tour || "pga").toLowerCase();
  return t === "pga" ? 2004 : 2017;
}

function toursToFetch() {
  const ex = (process.env.GOLF_HISTORICAL_ROUNDS_TOURS || "").trim();
  if (ex) {
    return [...new Set(ex.split(/[,;\s]+/).map((s) => s.trim().toLowerCase()).filter(Boolean))];
  }
  return ["pga", "liv"];
}

function refreshYears() {
  const cy = new Date().getFullYear();
  const ex = (process.env.GOLF_HISTORICAL_ROUNDS_YEARS || "").trim();
  if (ex) {
    const ys = [
      ...new Set(
        ex
          .split(/[,;\s]+/)
          .map((s) => parseInt(s, 10))
          .filter((n) => Number.isFinite(n) && n >= 2004 && n <= cy + 1)
      ),
    ];
    return ys.sort((a, b) => a - b);
  }
  if (process.env.GOLF_HISTORICAL_ROUNDS_LIGHT === "1") {
    const minY = Math.max(FIRST_HIST_YEAR, cy - LIGHT_YEARS + 1);
    const out = [];
    for (let y = minY; y <= cy; y++) out.push(y);
    return out;
  }
  const out = [];
  for (let y = FIRST_HIST_YEAR; y <= cy; y++) out.push(y);
  return out;
}

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

async function dgGet(url) {
  const maxAttempts = Math.max(4, Math.min(25, Number(process.env.GOLF_DG_MAX_ATTEMPTS || 12)));
  let lastErr;
  let lastStatus;
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const res = await fetch(url, { redirect: "follow" }).catch((e) => {
      lastErr = e;
      return null;
    });
    if (!res) {
      const w = Math.min(8000 + attempt * 2000, 60000);
      console.warn(`Connection error retry ${attempt}/${maxAttempts}; waiting ${Math.round(w / 1000)}s…`);
      await sleep(w);
      continue;
    }
    if (res.status === 200) return res;
    lastStatus = res.status;
    if ([429, 500, 502, 503, 504].includes(res.status)) {
      let waitMs = Math.min(25000 + attempt * 8000, 120000);
      const ra = res.headers.get("retry-after");
      if (ra) {
        const sec = parseInt(ra, 10);
        if (Number.isFinite(sec) && sec > 0) waitMs = Math.max(waitMs, sec * 1000);
      }
      console.warn(
        `HTTP ${res.status} retry ${attempt}/${maxAttempts}; waiting ${Math.round(waitMs / 1000)}s…`
      );
      await sleep(waitMs);
      continue;
    }
    return res;
  }
  throw lastErr || new Error(`DataGolf HTTP ${lastStatus ?? "?"} after ${maxAttempts} attempts`);
}

function eventsFromResponse(dat) {
  if (!dat || typeof dat !== "object") return [];
  if (dat.event_name != null && Array.isArray(dat.scores)) return [dat];
  for (const key of ["events", "data", "results"]) {
    const arr = dat[key];
    if (Array.isArray(arr) && arr.length) return arr;
  }
  if (Array.isArray(dat) && dat.length && dat[0]?.scores != null) return dat;
  const keys = Object.keys(dat);
  if (keys.length === 1 && Array.isArray(dat[keys[0]])) {
    const only = dat[keys[0]];
    if (only.length && typeof only[0] === "object") return only;
  }
  return [];
}

function getRoundPayload(s, rnum) {
  const keys = [`round_${rnum}`, `Round_${rnum}`, `round${rnum}`, `Round${rnum}`];
  for (const k of keys) {
    const r = s[k];
    if (r && typeof r === "object" && !Array.isArray(r)) return r;
    if (Array.isArray(r) && r.length) return r[0];
  }
  const pre = `round_${rnum}.`;
  const out = {};
  for (const k of Object.keys(s)) {
    if (k.startsWith(pre)) out[k.slice(pre.length)] = s[k];
  }
  return Object.keys(out).length ? out : null;
}

function eventCompletedChr(x) {
  if (x == null || x === "") return "";
  const str = String(x);
  const m = str.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (m) {
    const y = +m[1];
    const mo = +m[2];
    const d = +m[3];
    return `${mo}/${d}/${y}`;
  }
  return str;
}

function roundToRow(r, roundNum, evInfo, dgId, playerName, finText) {
  const scoreVal = r.score ?? r.round_score ?? r.Score;
  if (scoreVal == null || scoreVal === "") return null;
  const ei = evInfo;
  const idn = Math.round(Number(dgId));
  return {
    tour: String(ei.tour || "pga"),
    year: Number(ei.year),
    season: Number(ei.season ?? ei.year),
    event_completed: eventCompletedChr(ei.event_completed),
    event_name: String(ei.event_name || ""),
    event_id: String(ei.event_id || ""),
    player_name: String(playerName || ""),
    dg_id: Number.isFinite(idn) ? idn : "",
    fin_text: String(finText || ""),
    round_num: roundNum,
    course_name: String(r.course_name || ""),
    course_num: r.course_num ?? "",
    course_par: r.course_par ?? "",
    start_hole: r.start_hole ?? "",
    teetime: String(r.teetime ?? ""),
    round_score: Math.round(Number(scoreVal)),
    sg_putt: r.sg_putt ?? "",
    sg_arg: r.sg_arg ?? "",
    sg_app: r.sg_app ?? "",
    sg_ott: r.sg_ott ?? "",
    sg_t2g: r.sg_t2g ?? "",
    sg_total: r.sg_total ?? "",
    driving_dist: r.driving_dist ?? "",
    driving_acc: r.driving_acc ?? "",
    gir: r.gir ?? "",
    scrambling: r.scrambling ?? "",
    prox_rgh: r.prox_rgh ?? "",
    prox_fw: r.prox_fw ?? "",
    great_shots: r.great_shots ?? "",
    poor_shots: r.poor_shots ?? "",
    eagles_or_better: r.eagles_or_better ?? "",
    birdies: r.birdies ?? "",
    pars: r.pars ?? "",
    bogies: r.bogies ?? "",
    doubles_or_worse: r.doubles_or_worse ?? "",
  };
}

function eventToRows(ev) {
  const scores = Array.isArray(ev.scores) ? ev.scores : [];
  const eventInfo = {
    tour: ev.tour,
    year: ev.year,
    season: ev.season ?? ev.year,
    event_completed: ev.event_completed,
    event_name: ev.event_name,
    event_id: ev.event_id,
  };
  const rows = [];
  for (const s of scores) {
    if (!s || typeof s !== "object") continue;
    const dgId = s.dg_id ?? s.dgId;
    const pname = s.player_name ?? s.playerName ?? "";
    const finText = s.fin_text ?? s.finText ?? "";
    for (let rnum = 1; rnum <= 6; rnum++) {
      const r = getRoundPayload(s, rnum);
      if (!r) continue;
      const row = roundToRow(r, rnum, eventInfo, dgId, pname, finText);
      if (row) rows.push(row);
    }
  }
  return rows;
}

function csvEscape(val) {
  const t = val == null || val === "" ? "" : String(val);
  if (/[",\n\r]/.test(t)) return `"${t.replace(/"/g, '""')}"`;
  return t;
}

function writeCsv(filePath, rows) {
  const lines = [COL_ORDER.join(",")];
  for (const obj of rows) {
    lines.push(COL_ORDER.map((c) => csvEscape(obj[c])).join(","));
  }
  fs.writeFileSync(filePath, `${lines.join("\n")}\n`, "utf8");
}

function readCsv(csvPath) {
  if (!fs.existsSync(csvPath)) return [];
  const text = fs.readFileSync(csvPath, "utf8");
  if (!text.trim()) return [];
  return parse(text, { columns: true, relax_column_count: true, skip_empty_lines: true, trim: true });
}

function tourMatchesRefresh(tourVal, tourCode) {
  const tc = String(tourCode).toLowerCase();
  let tn = String(tourVal ?? "").trim().toLowerCase();
  if (!tn) tn = "pga";
  if (tc === "pga") return tn === "pga";
  return tn === tc;
}

async function fetchYearJson(year, tour, key) {
  const u = new URL(BASE);
  u.searchParams.set("tour", tour);
  u.searchParams.set("event_id", "all");
  u.searchParams.set("year", String(year));
  u.searchParams.set("file_format", "json");
  u.searchParams.set("key", key);
  const res = await dgGet(u.href);
  if (!res.ok) {
    console.warn(`  ${tour} year=${year} JSON HTTP ${res.status}`);
    return [];
  }
  let dat;
  try {
    dat = JSON.parse(await res.text());
  } catch {
    return [];
  }
  const events = eventsFromResponse(dat);
  const all = [];
  for (const ev of events) {
    all.push(...eventToRows(ev));
  }
  return all;
}

async function fetchYearCsv(year, tour, key) {
  const u = new URL(BASE);
  u.searchParams.set("tour", tour);
  u.searchParams.set("event_id", "all");
  u.searchParams.set("year", String(year));
  u.searchParams.set("file_format", "csv");
  u.searchParams.set("key", key);
  const res = await dgGet(u.href);
  if (!res.ok) return [];
  const body = await res.text();
  if (!body.trim()) return [];
  let recs;
  try {
    recs = parse(body, { columns: true, relax_column_count: true, skip_empty_lines: true, trim: true });
  } catch {
    return [];
  }
  return recs.map((raw) => {
    const o = { ...raw };
    if (o.round_score == null && o.score != null) o.round_score = o.score;
    if (o.round_num == null && o.round_number != null) o.round_num = o.round_number;
    if (o.round_num == null && o.round != null) o.round_num = o.round;
    if (!o.player_name && o.player) o.player_name = o.player;
    o.event_completed = eventCompletedChr(o.event_completed);
    return o;
  });
}

async function fetchYearRounds(year, tour, key) {
  if (year < minYearForTour(tour)) return [];
  const cy = new Date().getFullYear();
  let preferJson = year >= cy - 1;
  if (process.env.GOLF_ROUNDS_PREFER_CSV_FIRST === "1") preferJson = false;
  if (process.env.GOLF_ROUNDS_PREFER_JSON_FIRST === "1") preferJson = true;

  let rows = [];
  if (preferJson) {
    rows = await fetchYearJson(year, tour, key);
    if (rows.length) return rows;
    rows = await fetchYearCsv(year, tour, key);
  } else {
    rows = await fetchYearCsv(year, tour, key);
    if (rows.length) return rows;
    rows = await fetchYearJson(year, tour, key);
  }
  return rows;
}

function normalizeRow(r) {
  const o = {};
  for (const c of COL_ORDER) {
    let v = r[c];
    if (v === undefined || v === null || v === "") o[c] = "";
    else o[c] = v;
  }
  o.event_id = String(o.event_id);
  o.year = parseInt(String(o.year), 10);
  if (!Number.isFinite(o.year)) o.year = "";
  o.event_completed = eventCompletedChr(o.event_completed);
  o.teetime = String(o.teetime ?? "");
  return o;
}

function compareRows(a, b) {
  if (b.year !== a.year) return b.year - a.year;
  const ea = String(a.event_id);
  const eb = String(b.event_id);
  if (ea !== eb) return ea < eb ? -1 : 1;
  const da = Number(a.dg_id);
  const db = Number(b.dg_id);
  if (da !== db) return da - db;
  return (Number(a.round_num) || 0) - (Number(b.round_num) || 0);
}

async function main() {
  const key = loadApiKey();
  if (!key) {
    console.error("Set DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json with apiKey.");
    process.exit(1);
  }
  const outPath = path.join(MODEL_ROOT, "data", "historical_rounds_all.csv");
  const outDir = path.dirname(outPath);
  if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true });

  let existing = readCsv(outPath);
  const cy = new Date().getFullYear();
  const minKeep =
    process.env.GOLF_HISTORICAL_ROUNDS_LIGHT === "1"
      ? Math.max(FIRST_HIST_YEAR, cy - LIGHT_YEARS + 1)
      : FIRST_HIST_YEAR;
  existing = existing.filter((r) => parseInt(String(r.year), 10) >= minKeep);

  let combined = existing.map((r) => normalizeRow(r));
  const years = refreshYears();
  const tours = toursToFetch();
  console.log("historical_rounds_all (Node): years", years.join(", "), "| tours", tours.join(", "));
  console.log("OUT:", outPath);

  const betweenMs = Math.max(0, Number(process.env.GOLF_DG_ROUNDS_DELAY_MS || 1500));
  if (betweenMs) console.log(`Pause between requests: ${betweenMs}ms (set GOLF_DG_ROUNDS_DELAY_MS to change)`);

  const n = years.length;
  let hadFailure = false;
  for (let ii = 0; ii < years.length; ii++) {
    const yr = years[ii];
    for (const tourCode of tours) {
      if (yr < minYearForTour(tourCode)) continue;
      console.log(`[${ii + 1}/${n}] ${tourCode} ${yr} (event_id=all)…`);
      try {
        let yearRows = await fetchYearRounds(yr, tourCode, key);
        yearRows = yearRows.map((r) => normalizeRow(r));
        combined = combined.filter(
          (r) => !(parseInt(String(r.year), 10) === yr && tourMatchesRefresh(r.tour, tourCode))
        );
        combined.push(...yearRows);
        console.log(`  +${yearRows.length} rows`);
      } catch (e) {
        console.warn(`  SKIP ${tourCode} ${yr} (keeping existing CSV rows for this slice):`, e.message || e);
        hadFailure = true;
      }
      if (betweenMs) await sleep(betweenMs);
    }
  }

  combined.sort(compareRows);
  writeCsv(outPath, combined);
  console.log("Wrote", combined.length, "rows ->", outPath);
  if (hadFailure) {
    console.warn("Completed with one or more failed fetches; re-run fetch:dg later to fill gaps.");
    process.exit(1);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
