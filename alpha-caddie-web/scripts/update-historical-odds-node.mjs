#!/usr/bin/env node
/**
 * Refresh repo data/historical_outrights_outcomes.csv and data/historical_matchups_outcomes.csv
 * from DataGolf (same feeds as live_update_all.R).
 *
 * **Default (incremental):** `GOLF_ODDS_SINCE` defaults to **2026-03-01**. Only calendar year **2026**
 * is fetched; rows with `close_time` (else `open_time`) date **on or after** that day are replaced.
 * All older rows (including Jan–Feb 2026 and all prior years) are left unchanged.
 *
 * **Full refresh (like R):** set `GOLF_ODDS_FULL_REFRESH=1` or clear the cutoff with `GOLF_ODDS_SINCE=`
 * (empty). Then current + prior calendar years are pulled and whole-year rows are replaced.
 *
 * Env:
 *   DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json (apiKey)
 *   GOLF_MODEL_DIR — repo root (parent of alpha-caddie-web)
 *   GOLF_ODDS_SINCE — YYYY-MM-DD cutoff (default 2026-03-01). Empty string + see FULL_REFRESH for full years.
 *   GOLF_ODDS_FULL_REFRESH=1 — ignore default cutoff; use full year replace (cy, cy-1) + GOLF_DATAGOLF_YEARS
 *   GOLF_DATAGOLF_YEARS — comma-separated years (full mode only)
 *   GOLF_DG_ODDS_TOUR — default pga
 *   GOLF_DG_ODDS_DELAY_MS — delay between requests (default 1200; lowers 429 rate)
 *   GOLF_DG_MAX_ATTEMPTS — retries on 429/5xx (default 12)
 *   GOLF_MATCHUPS_BOOKS — comma list (e.g. draftkings,fanduel,betmgm) to limit matchup fetch
 *   GOLF_ODDS_SKIP_OUTRIGHTS=1 — matchups CSV only (faster for matchup-tracker refresh)
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = process.env.GOLF_MODEL_DIR ? path.resolve(process.env.GOLF_MODEL_DIR) : path.resolve(WEB_ROOT, "..");

const OUTRIGHTS_BASE = "https://feeds.datagolf.com/historical-odds/outrights";
const MATCHUPS_BASE = "https://feeds.datagolf.com/historical-odds/matchups";

const OUTRIGHTS_BOOKS = [
  "bet365",
  "betcris",
  "betmgm",
  "betonline",
  "betway",
  "bovada",
  "caesars",
  "corale",
  "circa",
  "draftkings",
  "fanduel",
  "pinnacle",
  "skybet",
  "sportsbook",
  "unibet",
  "williamhill",
];
const OUTRIGHTS_MARKETS = ["win", "top_5", "top_10", "top_20", "make_cut", "mc"];

const MATCHUPS_BOOKS_DEFAULT = [
  "5dimes",
  "bet365",
  "betcris",
  "betmgm",
  "betonline",
  "bovada",
  "caesars",
  "circa",
  "draftkings",
  "fanduel",
  "pinnacle",
  "sportsbook",
  "williamhill",
  "unibet",
];

/** Optional: GOLF_MATCHUPS_BOOKS=draftkings,fanduel,betmgm (matchup tracker). */
function resolveMatchupsBooks() {
  const raw = String(process.env.GOLF_MATCHUPS_BOOKS || "").trim();
  if (!raw) return MATCHUPS_BOOKS_DEFAULT;
  const books = raw
    .split(/[,;\s]+/)
    .map((s) => s.trim().toLowerCase())
    .filter(Boolean);
  return books.length ? books : MATCHUPS_BOOKS_DEFAULT;
}

const MATCHUPS_BOOKS = resolveMatchupsBooks();

const OUT_COLS = [
  "event_id",
  "event_name",
  "event_completed",
  "season",
  "year",
  "book",
  "market",
  "dg_id",
  "player_name",
  "open_odds",
  "close_odds",
  "open_time",
  "close_time",
  "bet_outcome_numeric",
  "bet_outcome_text",
  "outcome",
];

const MAT_COLS = [
  "event_id",
  "event_name",
  "event_completed",
  "season",
  "year",
  "book",
  "bet_type",
  "open_time",
  "close_time",
  "tie_rule",
  "p1_dg_id",
  "p1_player_name",
  "p1_open",
  "p1_close",
  "p1_outcome",
  "p1_outcome_text",
  "p2_dg_id",
  "p2_player_name",
  "p2_open",
  "p2_close",
  "p2_outcome",
  "p2_outcome_text",
  "p3_dg_id",
  "p3_player_name",
  "p3_open",
  "p3_close",
  "p3_outcome",
  "p3_outcome_text",
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
      console.warn(`HTTP ${res.status} retry ${attempt}/${maxAttempts}; waiting ${Math.round(waitMs / 1000)}s…`);
      await sleep(waitMs);
      continue;
    }
    const t = await res.text().catch(() => "");
    throw new Error(`DataGolf HTTP ${res.status}: ${t.slice(0, 200)}`);
  }
  throw lastErr || new Error(`DataGolf HTTP ${lastStatus ?? "?"} after ${maxAttempts} attempts`);
}

function datagolfYearsToUpdate() {
  const cy = new Date().getFullYear();
  const minY = cy - 4;
  /** @type {number[]} */
  let yrs = [cy, cy - 1];
  const extra = (process.env.GOLF_DATAGOLF_YEARS || "").trim();
  if (extra) {
    for (const p of extra.split(/[,;\s]+/)) {
      const n = parseInt(p, 10);
      if (Number.isFinite(n)) yrs.push(n);
    }
  }
  yrs = [...new Set(yrs)].filter((y) => y >= minY && y <= cy);
  yrs.sort((a, b) => a - b);
  return yrs;
}

/** @returns {string|null} ISO date YYYY-MM-DD, or null = full year refresh mode */
function resolveOddsSinceIso() {
  if (String(process.env.GOLF_ODDS_FULL_REFRESH || "").trim() === "1") return null;
  const ex = process.env.GOLF_ODDS_SINCE;
  if (ex !== undefined && String(ex).trim() === "") return null;
  const v = String(ex != null && String(ex).trim() !== "" ? ex : "2026-03-01").trim();
  const m = v.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) {
    console.warn("Invalid GOLF_ODDS_SINCE (want YYYY-MM-DD), using 2026-03-01:", v);
    return "2026-03-01";
  }
  return `${m[1]}-${m[2]}-${m[3]}`;
}

/** Calendar year to request from DataGolf when using a since-cutoff (one year only). */
function fetchYearFromSinceIso(sinceIso) {
  const y = parseInt(sinceIso.slice(0, 4), 10);
  const cy = new Date().getFullYear();
  if (!Number.isFinite(y) || y < 2004) return cy;
  return Math.min(y, cy);
}

/** First YYYY-MM-DD from close_time, else open_time (both files). */
function anchorDateIsoFromRow(row) {
  const close = String(row.close_time || "").trim();
  const open = String(row.open_time || "").trim();
  const pick = close.length >= 10 ? close : open.length >= 10 ? open : "";
  const m = pick.match(/^(\d{4}-\d{2}-\d{2})/);
  return m ? m[1] : "";
}

function yearsForOddsJob(sinceIso) {
  if (sinceIso) return [fetchYearFromSinceIso(sinceIso)];
  return datagolfYearsToUpdate();
}

function appendAll(dst, src) {
  for (let i = 0; i < src.length; i++) dst.push(src[i]);
}

function filterRowsOnOrAfterSince(rows, sinceIso) {
  return rows.filter((r) => {
    const d = anchorDateIsoFromRow(r);
    return d && d >= sinceIso;
  });
}

function csvEscape(val) {
  const t = val == null || val === "" ? "" : String(val);
  if (/[",\n\r]/.test(t)) return `"${t.replace(/"/g, '""')}"`;
  return t;
}

function rowLine(cols, row) {
  return cols.map((c) => csvEscape(row[c])).join(",");
}

/** Same idea as live_update_all.R parse_outrights_single / historical_outrights.R parse_response. */
function parseOutrightsResponse(dat, book, year, market) {
  if (dat == null) return [];
  if (Array.isArray(dat)) {
    const rows = [];
    for (const ev of dat) {
      const sub = parseOutrightsResponse(ev, book, year, market);
      appendAll(rows, sub);
    }
    return rows;
  }
  if (typeof dat !== "object") return [];
  if (Array.isArray(dat.odds)) {
    const rows = [];
    for (const o of dat.odds) {
      if (!o || typeof o !== "object") continue;
      rows.push(outrightRow(dat, o, book, year, market));
    }
    return rows;
  }
  const rows = [];
  for (const v of Object.values(dat)) {
    if (v != null && typeof v === "object") {
      const sub = parseOutrightsResponse(v, book, year, market);
      appendAll(rows, sub);
    }
  }
  return rows;
}

function outrightRow(ev, o, book, year, market) {
  const season = ev.season != null && ev.season !== "" ? parseInt(String(ev.season), 10) : year;
  return {
    event_id: ev.event_id != null ? String(ev.event_id) : "",
    event_name: ev.event_name != null ? String(ev.event_name) : "",
    event_completed: ev.event_completed != null ? String(ev.event_completed) : "",
    season: Number.isFinite(season) ? season : "",
    year,
    book: String(book),
    market: String(market),
    dg_id: o.dg_id != null && o.dg_id !== "" ? parseInt(String(o.dg_id), 10) : "",
    player_name: o.player_name != null ? String(o.player_name) : "",
    open_odds: o.open_odds ?? "",
    close_odds: o.close_odds ?? "",
    open_time: o.open_time != null ? String(o.open_time) : "",
    close_time: o.close_time != null ? String(o.close_time) : "",
    bet_outcome_numeric: o.bet_outcome_numeric ?? "",
    bet_outcome_text: o.bet_outcome_text != null ? String(o.bet_outcome_text) : "",
    outcome: o.outcome != null ? String(o.outcome) : "",
  };
}

function parseMatchupsResponse(dat, book, year) {
  if (dat == null) return [];
  if (Array.isArray(dat)) {
    const rows = [];
    for (const ev of dat) {
      const sub = parseMatchupsResponse(ev, book, year);
      appendAll(rows, sub);
    }
    return rows;
  }
  if (typeof dat !== "object") return [];
  if (Array.isArray(dat.odds)) {
    const rows = [];
    for (const o of dat.odds) {
      if (!o || typeof o !== "object") continue;
      rows.push(matchupRow(dat, o, book, year));
    }
    return rows;
  }
  const rows = [];
  for (const v of Object.values(dat)) {
    if (v != null && typeof v === "object") {
      const sub = parseMatchupsResponse(v, book, year);
      appendAll(rows, sub);
    }
  }
  return rows;
}

function matchupRow(ev, o, book, year) {
  const season = ev.season != null && ev.season !== "" ? parseInt(String(ev.season), 10) : year;
  return {
    event_id: ev.event_id != null ? String(ev.event_id) : "",
    event_name: ev.event_name != null ? String(ev.event_name) : "",
    event_completed: ev.event_completed != null ? String(ev.event_completed) : "",
    season: Number.isFinite(season) ? season : "",
    year,
    book: String(book),
    bet_type: o.bet_type != null ? String(o.bet_type) : "",
    open_time: o.open_time != null ? String(o.open_time) : "",
    close_time: o.close_time != null ? String(o.close_time) : "",
    tie_rule: o.tie_rule != null ? String(o.tie_rule) : "",
    p1_dg_id: o.p1_dg_id ?? "",
    p1_player_name: o.p1_player_name != null ? String(o.p1_player_name) : "",
    p1_open: o.p1_open ?? "",
    p1_close: o.p1_close ?? "",
    p1_outcome: o.p1_outcome ?? "",
    p1_outcome_text: o.p1_outcome_text != null ? String(o.p1_outcome_text) : "",
    p2_dg_id: o.p2_dg_id ?? "",
    p2_player_name: o.p2_player_name != null ? String(o.p2_player_name) : "",
    p2_open: o.p2_open ?? "",
    p2_close: o.p2_close ?? "",
    p2_outcome: o.p2_outcome ?? "",
    p2_outcome_text: o.p2_outcome_text != null ? String(o.p2_outcome_text) : "",
    p3_dg_id: o.p3_dg_id ?? "",
    p3_player_name: o.p3_player_name != null ? String(o.p3_player_name) : "",
    p3_open: o.p3_open ?? "",
    p3_close: o.p3_close ?? "",
    p3_outcome: o.p3_outcome ?? "",
    p3_outcome_text: o.p3_outcome_text != null ? String(o.p3_outcome_text) : "",
  };
}

async function fetchOutrightsYear(year, tour, key) {
  const delayMs = Math.max(0, Number(process.env.GOLF_DG_ODDS_DELAY_MS || 1200));
  /** @type {any[]} */
  const all = [];
  for (const book of OUTRIGHTS_BOOKS) {
    for (const market of OUTRIGHTS_MARKETS) {
      await sleep(delayMs);
      const u = new URL(OUTRIGHTS_BASE);
      u.searchParams.set("tour", tour);
      u.searchParams.set("event_id", "all");
      u.searchParams.set("year", String(year));
      u.searchParams.set("market", market);
      u.searchParams.set("book", book);
      u.searchParams.set("odds_format", "american");
      u.searchParams.set("file_format", "json");
      u.searchParams.set("key", key);
      const res = await dgGet(u.toString());
      const text = await res.text();
      let dat;
      try {
        dat = JSON.parse(text);
      } catch {
        console.warn(`  outrights JSON parse fail ${book} ${market} ${year}:`, text.slice(0, 160));
        continue;
      }
      const rows = parseOutrightsResponse(dat, book, year, market);
      if (rows.length) appendAll(all, rows);
    }
  }
  return all;
}

async function fetchMatchupsYear(year, tour, key) {
  const delayMs = Math.max(0, Number(process.env.GOLF_DG_ODDS_DELAY_MS || 1200));
  /** @type {any[]} */
  const all = [];
  for (const book of MATCHUPS_BOOKS) {
    await sleep(delayMs);
    const u = new URL(MATCHUPS_BASE);
    u.searchParams.set("tour", tour);
    u.searchParams.set("event_id", "all");
    u.searchParams.set("year", String(year));
    u.searchParams.set("book", book);
    u.searchParams.set("odds_format", "decimal");
    u.searchParams.set("file_format", "json");
    u.searchParams.set("key", key);
    const res = await dgGet(u.toString());
    const text = await res.text();
    let dat;
    try {
      dat = JSON.parse(text);
    } catch {
      console.warn(`  matchups JSON parse fail ${book} ${year}:`, text.slice(0, 160));
      continue;
    }
    const rows = parseMatchupsResponse(dat, book, year);
    if (rows.length) appendAll(all, rows);
  }
  return all;
}

/**
 * Stream-read CSV; write header + rows we keep (not in the replace window).
 * @param {{ mode: "years", replaceYears: Set<number> } | { mode: "since", sinceIso: string }} mergeOpts
 */
async function copyCsvRowsForMerge(srcPath, outStream, cols, mergeOpts) {
  if (!fs.existsSync(srcPath)) {
    outStream.write(`${cols.join(",")}\n`);
    return;
  }
  const parser = fs.createReadStream(srcPath, { encoding: "utf8" }).pipe(
    parse({
      columns: true,
      relax_column_count: true,
      trim: true,
      bom: true,
    }),
  );
  let headerWritten = false;
  for await (const row of parser) {
    if (!headerWritten) {
      outStream.write(`${cols.join(",")}\n`);
      headerWritten = true;
    }
    if (mergeOpts.mode === "years") {
      const y = parseInt(String(row.year), 10);
      if (mergeOpts.replaceYears.has(y)) continue;
    } else {
      const d = anchorDateIsoFromRow(row);
      if (d && d >= mergeOpts.sinceIso) continue;
    }
    const normalized = {};
    for (const c of cols) normalized[c] = row[c] != null ? row[c] : "";
    outStream.write(`${rowLine(cols, normalized)}\n`);
  }
  if (!headerWritten) {
    outStream.write(`${cols.join(",")}\n`);
  }
}

/**
 * @param {{ mode: "years", replaceYears: Set<number> } | { mode: "since", sinceIso: string }} mergeOpts
 */
async function mergeWriteCsv(destPath, cols, mergeOpts, newRows) {
  const tmp = `${destPath}.tmp.${process.pid}`;
  const ws = fs.createWriteStream(tmp, { encoding: "utf8" });
  try {
    await copyCsvRowsForMerge(destPath, ws, cols, mergeOpts);
    for (const r of newRows) {
      ws.write(`${rowLine(cols, r)}\n`);
    }
    await new Promise((resolve, reject) => {
      ws.end((err) => (err ? reject(err) : resolve()));
    });
    fs.renameSync(tmp, destPath);
  } catch (e) {
    try {
      fs.unlinkSync(tmp);
    } catch {
      /* ignore */
    }
    throw e;
  }
}

async function main() {
  const key = loadApiKey();
  if (!key) {
    console.error("Set DATAGOLF_API_KEY or create alpha-caddie-web/datagolf.local.json with apiKey.");
    process.exit(1);
  }
  const tour = (process.env.GOLF_DG_ODDS_TOUR || "pga").trim().toLowerCase() || "pga";
  const sinceIso = resolveOddsSinceIso();
  const years = yearsForOddsJob(sinceIso);
  const mergeOpts =
    sinceIso != null
      ? { mode: "since", sinceIso }
      : { mode: "years", replaceYears: new Set(years) };
  const dataDir = path.join(MODEL_ROOT, "data");
  fs.mkdirSync(dataDir, { recursive: true });
  const pathOut = path.join(dataDir, "historical_outrights_outcomes.csv");
  const pathMat = path.join(dataDir, "historical_matchups_outcomes.csv");

  console.log("Model dir:", MODEL_ROOT);
  if (sinceIso) {
    console.log(`Incremental since ${sinceIso} (replace rows with close/open date on or after this; fetch years: ${years.join(", ")})`);
  } else {
    console.log("Full year replace for years:", years.join(", "));
  }
  console.log("Tour:", tour);

  const skipOutrights = String(process.env.GOLF_ODDS_SKIP_OUTRIGHTS || "").trim() === "1";
  console.log("Matchup books:", MATCHUPS_BOOKS.join(", "));
  if (skipOutrights) {
    console.log("Skipping outrights (GOLF_ODDS_SKIP_OUTRIGHTS=1)");
  } else {
    /** @type {any[]} */
    const allOut = [];
    for (const y of years) {
      console.log(`\n--- Outrights ${y} ---`);
      const outRows = await fetchOutrightsYear(y, tour, key);
      console.log(`Fetched ${outRows.length.toLocaleString()} outright rows (API)`);
      appendAll(allOut, outRows);
    }
    const outToWrite = sinceIso ? filterRowsOnOrAfterSince(allOut, sinceIso) : allOut;
    if (sinceIso) {
      console.log(`After ${sinceIso} filter: ${outToWrite.length.toLocaleString()} outright rows to append`);
    }
    if (outToWrite.length === 0) {
      console.error("Refusing to write outrights: 0 rows to write (check API, filters, or GOLF_ODDS_SINCE).");
      process.exit(1);
    }
    console.log("\nWriting historical_outrights_outcomes.csv …");
    await mergeWriteCsv(pathOut, OUT_COLS, mergeOpts, outToWrite);
  }

  /** @type {any[]} */
  const allMat = [];
  for (const y of years) {
    console.log(`\n--- Matchups ${y} ---`);
    const rows = await fetchMatchupsYear(y, tour, key);
    console.log(`Fetched ${rows.length.toLocaleString()} matchup rows (API)`);
    appendAll(allMat, rows);
  }
  const matToWrite = sinceIso ? filterRowsOnOrAfterSince(allMat, sinceIso) : allMat;
  if (sinceIso) {
    console.log(`After ${sinceIso} filter: ${matToWrite.length.toLocaleString()} matchup rows to append`);
  }
  if (matToWrite.length === 0) {
    console.error("Refusing to write matchups: 0 rows to write.");
    process.exit(1);
  }
  console.log("\nWriting historical_matchups_outcomes.csv …");
  await mergeWriteCsv(pathMat, MAT_COLS, mergeOpts, matToWrite);

  console.log("\nDone.");
  console.log(" ", pathOut);
  console.log(" ", pathMat);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
