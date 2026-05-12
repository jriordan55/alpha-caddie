#!/usr/bin/env node
/**
 * Refresh only sportsbook columns: fetches model outrights + matchups (+ preds/pre-tournament
 * for datagolf column fill), then replaces parseable outright book columns with scraped sportsbook pages.
 *
 * Use between full `npm run fetch:dg` / R exports so the static app sees current lines without rebuilding players.
 * Preflight: compares `/field-updates` to projections (week key + fuzzy title); on mismatch runs `fetch-datagolf.mjs`
 * then re-reads JSON. Compares `GOLF_DATAGOLF_TOUR`/`pga` field-updates when feed tour differs so a stuck `liv`/snapshot
 * cannot self-match forever.
 * Skip with GOLF_SKIP_INLINE_FETCH_DG_ON_EVENT_MISMATCH=1.
 *
 *   npm run fetch:book-odds
 *
 * Env: DATAGOLF_API_KEY or datagolf.local.json; GOLF_MODEL_DIR (repo root); GOLF_DATAGOLF_TOUR / GOLF_TOUR fallback when
 *      projections.json lacks datagolf_feed_tour (written by fetch-datagolf when multiple tours are compared).
 *      preds/pre-tournament: GOLF_PRE_TOURNAMENT_DEAD_HEAT (default yes), GOLF_PRE_TOURNAMENT_ODDS_FORMAT (default decimal).
 *      betting-tools/outrights: GOLF_OUTRIGHTS_ODDS_FORMAT (default percent — same IMPLIED % as
 *      https://datagolf.com/betting-tool-finish; override with decimal|american if needed).
 *      GOLF_PRE_TOURNAMENT_ADD_POSITION (optional, e.g. "17,23").
 *      GOLF_OUTRIGHTS_DEAD_HEAT=yes|no — same as fetch-datagolf.mjs
 *      GOLF_SKIP_PROPS_CSV=1 — do not merge data/player_props_*.csv into projections.props (Model O/U DK lines).
 *      GOLF_SKIP_DK_OU=1 — do not pull DK round props (see draftkings-ou-props.mjs).
 *      GOLF_SKIP_MODEL_FALLBACK_OU=1 — do not synthesize GIR / fairways / putts from projections.players for players DK omits.
 *      GIR/FW/Putts: each run drops stale csv+model_fallback rows for those markets, then re-adds model lines from current
 *      payload.players for any player DK does not supply (DK rows stay authoritative when present).
 *
 * DraftKings round props (Birdies, Pars, Bogeys, Round Score → Total Score) use Playwright + Chromium.
 * Production (Render): `playwright` is a runtime dependency; build should run `npx playwright install chromium`.
 * Point DK at the active event: DK_LEAGUE_URL (e.g. …/leagues/golf/{event}?category=round) and DK_LEAGUE_ID (from DK URL).
 * If DK_LEAGUE_URL is unset, uses projections.event_name → slug (override with dk_league_slug on JSON or set DK_LEAGUE_URL when DK’s slug differs).
 * CSV files still override or fill gaps when DK omits a player or market.
 */
import { spawnSync } from "child_process";
import { parse } from "csv-parse/sync";
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  coursesClearlyDistinct,
  eventsLikelySame,
  fieldWeekKey,
  fieldWeekKeysRoughMatch,
  titleTokenOverlapRatio,
  tokenizeEventTitle,
} from "./dg-events-align.mjs";
import { fetchDraftKingsOuProps } from "./draftkings-ou-props.mjs";
import { fetchSportsbookOutrightsFromUrls } from "./sportsbook-outrights-scraper.mjs";
const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const ENV_DEFAULT_TOUR = ((process.env.GOLF_DATAGOLF_TOUR || process.env.GOLF_TOUR || "pga").trim() || "pga").toLowerCase();

function loadApiKey() {
  const env = (process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = join(WEB_ROOT, "datagolf.local.json");
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

/**
 * When `DK_LEAGUE_URL` is unset, point Playwright at the same DraftKings event as `projections.json`.
 * Slug from `event_name` can mismatch DK URLs — set `DK_LEAGUE_URL` or optional `dk_league_slug` on the payload.
 */
function inferDraftKingsLeagueUrlFromProjections(payload) {
  const envUrl = String(process.env.DK_LEAGUE_URL || "").trim();
  if (envUrl) return envUrl;
  const slug = String(
    payload?.dk_league_slug || payload?.draftkings_league_slug || payload?.dk_event_slug || "",
  ).trim();
  if (slug) return `https://sportsbook.draftkings.com/leagues/golf/${slug}?category=round`;
  const name = String(payload?.event_name || "").trim();
  if (!name) return "";
  const s = name
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
  if (!s) return "";
  return `https://sportsbook.draftkings.com/leagues/golf/${s}?category=round`;
}

/** Rough player-list shape from field-updates (matches fetch-datagolf parsing intent). */
function fieldRowsFromUpdates(raw) {
  if (!raw || typeof raw !== "object") return [];
  if (Array.isArray(raw.field) && raw.field.length) return raw.field;
  for (const k of ["data", "players", "baseline_history_fit", "baseline"]) {
    const v = raw[k];
    if (Array.isArray(v) && v.length) return v;
  }
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

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function snapHalfLine(x) {
  const v = num(x, NaN);
  if (!Number.isFinite(v)) return NaN;
  return Math.round(v - 0.5) + 0.5;
}

/** When DraftKings has no field O/U for GIR/FW/putts, expose model means as *.5 lines (-110/-110) so the UI loads. */
function modelFallbackOuForMarket(players, market) {
  const field =
    market === "GIR" ? "gir" : market === "Fairways hit" ? "fairways" : market === "Putts" ? "putts" : "";
  if (!field || !Array.isArray(players)) return [];
  const holes = market === "GIR" ? 18 : market === "Fairways hit" ? 14 : null;
  const out = [];
  for (const p of players) {
    const pn = String(p.player_name || "").trim();
    if (!pn) continue;
    let x = num(p[field], NaN);
    if (!Number.isFinite(x)) continue;
    if (x === 0 || x === 1) continue;
    if (holes != null) {
      /** (0, 1] = share of holes; (1, holes] = counts — do not treat 11.2 as a rate. */
      if (x > 0 && x <= 1.0001) {
        x = Math.min(holes, Math.max(0, Math.round(x * holes)));
      } else {
        x = Math.min(holes, Math.max(0, Math.round(x)));
      }
    } else {
      x = Math.round(x);
    }
    let L = snapHalfLine(x);
    if (market === "GIR") L = Math.min(16.5, Math.max(4.5, L));
    else if (market === "Fairways hit") L = Math.min(13.5, Math.max(2.5, L));
    else if (market === "Putts") L = Math.min(36.5, Math.max(22.5, L));
    if (!Number.isFinite(L)) continue;
    const dg = Math.round(num(p.dg_id, NaN));
    const o = { player_name: pn, line: L, over_odds: -110, under_odds: -110, market };
    if (Number.isFinite(dg) && dg > 0) o.dg_id = dg;
    out.push(o);
  }
  return out;
}

function withPropSource(rows, source) {
  const s = String(source || "unknown").trim();
  return (Array.isArray(rows) ? rows : []).map((r) => ({ ...r, source: s }));
}

const OU_COUNTING_MARKETS_FW = ["GIR", "Fairways hit", "Putts"];

/** Stable key: dg_id when set, else normalized player name — paired with market for DK coverage checks. */
function propPlayerMarketPresenceKey(r, market) {
  const id = Math.round(num(r.dg_id, NaN));
  if (Number.isFinite(id) && id > 0) return `id:${id}|${market}`;
  return `nm:${String(r.player_name || "").trim().toLowerCase()}|${market}`;
}

/** Remove stale csv / model_fallback rows for GIR, fairways, putts so they can be rebuilt from current players. */
function stripNonDkCountingProps(byKey) {
  for (const key of [...byKey.keys()]) {
    const r = byKey.get(key);
    const m = String(r.market || "").trim();
    if (!OU_COUNTING_MARKETS_FW.includes(m)) continue;
    const src = String(r.source || "").trim().toLowerCase();
    if (src === "csv" || src === "model_fallback") byKey.delete(key);
  }
}

const OU_PROP_CSV_FILES = [
  ["Total Score", "player_props_lines.csv"],
  ["Birdies", "player_props_birdies.csv"],
  ["Pars", "player_props_pars.csv"],
  ["Bogeys", "player_props_bogeys.csv"],
  ["GIR", "player_props_gir.csv"],
  ["Fairways hit", "player_props_fairways.csv"],
  ["Putts", "player_props_putts.csv"],
];

function normalizePropMarketFromRow(row, defaultMkt) {
  const v = String(row.stat || row.market || row.prop_type || "")
    .trim()
    .toLowerCase();
  if (!v) return defaultMkt;
  if (/total|round.?score|^score$|^total$/.test(v)) return "Total Score";
  if (/bog/.test(v)) return "Bogeys";
  if (/bird/.test(v)) return "Birdies";
  if (/par/.test(v)) return "Pars";
  if (/gir|green/.test(v)) return "GIR";
  if (/fairway/.test(v)) return "Fairways hit";
  if (/putt/.test(v)) return "Putts";
  return defaultMkt;
}

function parseOuPropsCsv(absPath, defaultMkt) {
  if (!existsSync(absPath)) return [];
  let rows;
  try {
    const text = readFileSync(absPath, "utf8");
    if (!String(text).trim()) return [];
    rows = parse(text, { columns: true, skip_empty_lines: true, trim: true, relax_column_count: true });
  } catch {
    return [];
  }
  if (!Array.isArray(rows)) return [];
  const out = [];
  for (const row of rows) {
    const pn = String(row.player_name || row.player || row.name || row.golfer || "").trim();
    if (!pn) continue;
    const over = num(row.over_odds ?? row.over, NaN);
    const under = num(row.under_odds ?? row.under, NaN);
    let line = num(row.line, NaN);
    if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;
    const mkt = normalizePropMarketFromRow(row, defaultMkt);
    if (mkt !== "Total Score" && line === Math.floor(line)) line += 0.5;
    const o = { player_name: pn, line, over_odds: over, under_odds: under, market: mkt };
    const dg = Math.round(num(row.dg_id ?? row.dgId, NaN));
    if (Number.isFinite(dg) && dg > 0) o.dg_id = dg;
    out.push(o);
  }
  return out;
}

function loadOuPropsFromRepoCsv() {
  const dataDir = join(GOLF_MODEL_ROOT, "data");
  const pieces = [];
  for (const [mkt, fn] of OU_PROP_CSV_FILES) {
    pieces.push(...parseOuPropsCsv(join(dataDir, fn), mkt));
  }
  pieces.push(...parseOuPropsCsv(join(dataDir, "player_props_birdies_custom.csv"), "Birdies"));
  const map = new Map();
  for (const r of pieces) {
    map.set(`${r.player_name}|${r.market}|${r.line}`, r);
  }
  return [...map.values()];
}

function asArray(x) {
  if (x == null) return [];
  if (Array.isArray(x)) return x;
  return [];
}

function rowsFromResponse(dat) {
  if (dat == null) return [];
  if (Array.isArray(dat)) return dat;
  if (typeof dat !== "object") return [];
  for (const k of ["data", "players", "field", "baseline_history_fit", "baseline"]) {
    const v = dat[k];
    if (Array.isArray(v)) return v;
  }
  if (Array.isArray(dat.baseline_history_fit)) return dat.baseline_history_fit;
  return [];
}

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

const OUTRIGHTS_ROW_SKIP_KEYS = new Set(["dg_id", "id", "player_name", "name"]);

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

const outrightsOddsFormat = (process.env.GOLF_OUTRIGHTS_ODDS_FORMAT || "percent").trim().toLowerCase();

function impliedPctFromOutrightsApiValue(v, oddsFormat) {
  const x = num(v, NaN);
  if (!Number.isFinite(x) || x <= 0) return NaN;
  const fmt = String(oddsFormat || "decimal").toLowerCase();
  if (fmt === "decimal") {
    if (x > 1 && x < 20000) return (1 / x) * 100;
    if (x > 0 && x <= 1) return x * 100;
    return NaN;
  }
  if (fmt === "american") {
    if (x > 0) return (100 / (x + 100)) * 100;
    if (x < 0) return (Math.abs(x) / (Math.abs(x) + 100)) * 100;
    return NaN;
  }
  if (fmt === "fraction") return NaN;
  let p = x;
  if (p > 1) p /= 100;
  return p * 100;
}

function outrightDeadHeatForMarket(market) {
  const g = String(process.env.GOLF_OUTRIGHTS_DEAD_HEAT || "").trim().toLowerCase();
  if (g === "yes" || g === "no") return g;
  return market === "win" ? "no" : "yes";
}

function outrightPretField(market) {
  if (market === "mc") return "make_cut";
  return market;
}

function enrichOutrightsRows(rows, market, pretByDg) {
  const pretKey = outrightPretField(market);
  const isMc = market === "mc";
  for (const r of rows) {
    let dgVal = num(r.datagolf, NaN);
    if (Number.isFinite(dgVal) && dgVal > 0) continue;
    for (const alt of ["model", "fair", "prediction", "dg_fair"]) {
      if (!(alt in r)) continue;
      const pv = num(r[alt], NaN);
      if (!Number.isFinite(pv) || pv === 0) continue;
      r.datagolf = impliedPctFromOutrightsApiValue(pv, outrightsOddsFormat);
      delete r[alt];
      break;
    }
    dgVal = num(r.datagolf, NaN);
    if (Number.isFinite(dgVal) && dgVal > 0) continue;
    const id = Math.round(num(r.dg_id, NaN));
    const pt = pretByDg.get(id);
    if (!pt) continue;
    let p = num(pt[pretKey], NaN);
    if (!Number.isFinite(p)) continue;
    if (isMc) p = 1 - p;
      const pct = Number.isFinite(p) && p > 0 ? p * 100 : NaN;
      if (Number.isFinite(pct) && pct > 0) r.datagolf = pct;
  }
}

function outrightBookKeysFromRows(rows) {
  const s = new Set();
  for (const r of rows) {
    for (const k of Object.keys(r)) {
      if (k === "dg_id" || k === "player_name") continue;
      s.add(k);
    }
  }
  return [...s].sort();
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
      const pct = impliedPctFromOutrightsApiValue(v, outrightsOddsFormat);
      if (!Number.isFinite(pct)) continue;
      out[key] = pct;
      bookSet.add(key);
    }
    rows.push(out);
  }
  return { rows, bookKeys: [...bookSet].sort() };
}

function mergeScrapedOutrightsIntoMarket(existingPack, scrapedPack) {
  if (!scrapedPack || !Array.isArray(scrapedPack.rows) || !scrapedPack.rows.length) return existingPack;
  const useScrapedDraftKings = String(process.env.GOLF_SKIP_SCRAPED_DK_OUTRIGHTS || "").trim() !== "1";
  const baseById = new Map();
  for (const r of Array.isArray(existingPack?.rows) ? existingPack.rows : []) {
    const id = Math.round(num(r.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    baseById.set(id, {
      ...r,
      dg_id: id,
      player_name: String(r.player_name || "").trim(),
    });
  }
  for (const r of scrapedPack.rows) {
    const id = Math.round(num(r.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const out = baseById.get(id) || { dg_id: id, player_name: String(r.player_name || "").trim() };
    for (const bk of scrapedPack.bookKeys || []) {
      if (String(bk).toLowerCase() === "draftkings" && !useScrapedDraftKings) continue;
      const pct = num(r[bk], NaN);
      if (Number.isFinite(pct) && pct > 0) out[bk] = pct;
    }
    baseById.set(id, out);
  }
  const rows = [...baseById.values()];
  const bookKeys = new Set(Array.isArray(existingPack?.bookKeys) ? existingPack.bookKeys.map((bk) => String(bk).toLowerCase()) : ["datagolf"]);
  for (const bk of scrapedPack.bookKeys || []) {
    const k = String(bk).toLowerCase();
    if (k === "draftkings" && !useScrapedDraftKings) continue;
    bookKeys.add(k);
  }
  return { rows, bookKeys: [...bookKeys].sort() };
}

function mergeScrapedOutrights(outrights, scrapedOutrights) {
  const next = { ...(outrights && typeof outrights === "object" ? outrights : {}) };
  for (const [market, scrapedPack] of Object.entries(scrapedOutrights || {})) {
    next[market] = mergeScrapedOutrightsIntoMarket(next[market], scrapedPack);
  }
  return next;
}

function scrapedOutrightsHasBook(scrapedOutrights, bookKey) {
  const want = String(bookKey || "").toLowerCase();
  for (const pack of Object.values(scrapedOutrights || {})) {
    const keys = Array.isArray(pack?.bookKeys) ? pack.bookKeys : [];
    if (keys.some((k) => String(k).toLowerCase() === want)) return true;
  }
  return false;
}

function removeBookFromOutrights(outrights, bookKey) {
  const want = String(bookKey || "").toLowerCase();
  const next = { ...(outrights && typeof outrights === "object" ? outrights : {}) };
  for (const [market, pack] of Object.entries(next)) {
    if (!pack || typeof pack !== "object") continue;
    const rows = Array.isArray(pack.rows)
      ? pack.rows.map((row) => {
          const out = { ...row };
          for (const k of Object.keys(out)) {
            if (String(k).toLowerCase() === want) delete out[k];
          }
          return out;
        })
      : pack.rows;
    const bookKeys = Array.isArray(pack.bookKeys)
      ? pack.bookKeys.filter((k) => String(k).toLowerCase() !== want)
      : pack.bookKeys;
    next[market] = { ...pack, rows, bookKeys };
  }
  return next;
}

async function main() {
  const key = loadApiKey();
  if (!key) {
    console.error("Missing API key. Set DATAGOLF_API_KEY or datagolf.local.json.");
    process.exit(1);
  }

  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("Missing", projPath);
    process.exit(1);
  }

  let payload;
  try {
    payload = JSON.parse(readFileSync(projPath, "utf8"));
  } catch (e) {
    console.error("Could not parse projections.json:", e.message);
    process.exit(1);
  }
  if (!payload || typeof payload !== "object") {
    console.error("Invalid projections.json root");
    process.exit(1);
  }

  const tourForFeeds = String(payload.datagolf_feed_tour || "").trim().toLowerCase() || ENV_DEFAULT_TOUR;

  /** DataGolf week rotated but git/deploy snapshot still has last event → merge odds only locks stale field. */
  const skipInlineDg = String(process.env.GOLF_SKIP_INLINE_FETCH_DG_ON_EVENT_MISMATCH || "").trim() === "1";
  if (!skipInlineDg) {
    try {
      const fu = await fetchDg("/field-updates", { tour: tourForFeeds, file_format: "json" }, key);
      /** Calendar week for staleness: always compare to ENV_DEFAULT_TOUR (usually `pga`), not only `datagolf_feed_tour`.
       * Otherwise a stuck `liv` / `opp` snapshot matches itself forever and never runs inline fetch:dg. */
      let fuCal = fu;
      if (tourForFeeds !== ENV_DEFAULT_TOUR) {
        try {
          fuCal = await fetchDg("/field-updates", { tour: ENV_DEFAULT_TOUR, file_format: "json" }, key);
        } catch (e) {
          console.warn(`[fetch-book-odds] field-updates ${ENV_DEFAULT_TOUR}:`, e.message || e);
        }
      }
      const fuEventFeed = String(fu.event_name || fu.eventName || "").trim();
      const fuCourseFeed = String(fu.course_name || fu.courseName || fu.course || "").trim();
      const calEvent = String(fuCal.event_name || fuCal.eventName || "").trim() || fuEventFeed;
      const calCourse = String(fuCal.course_name || fuCal.courseName || fuCal.course || "").trim() || fuCourseFeed;
      const fuRows = fieldRowsFromUpdates(fuCal).filter((p) => {
        if (!p || typeof p !== "object") return false;
        const id = num(p.dg_id ?? p.dgId, NaN);
        const pn = String(p.player_name || p.name || p.playerName || "").trim();
        return Number.isFinite(id) && pn.length > 0;
      });
      const projEvent = String(payload.event_name || "").trim();
      const fuKey = fieldWeekKey(calEvent, calCourse);
      const projKey = String(payload.datagolf_field_week_key || "").trim() || fieldWeekKey(projEvent, String(payload.course_used || ""));
      const hasFu = calEvent && fuRows.length >= 8;
      const keysOk = fieldWeekKeysRoughMatch(projKey, fuKey);
      const nameFuzzyOk = !!(projEvent && calEvent && eventsLikelySame(projEvent, calEvent));
      const ta = tokenizeEventTitle(projEvent);
      const tb = tokenizeEventTitle(calEvent);
      const tokenStale =
        projEvent &&
        calEvent &&
        ta.length >= 3 &&
        tb.length >= 3 &&
        titleTokenOverlapRatio(projEvent, calEvent) < 0.38;
      const courseStale = !!(projEvent && calEvent && nameFuzzyOk && coursesClearlyDistinct(payload.course_used, calCourse));

      const staleWeek =
        hasFu &&
        (!projEvent ||
          !keysOk ||
          !nameFuzzyOk ||
          courseStale ||
          tokenStale);

      console.log(
        `[fetch-book-odds] field-updates sync: feedTour=${tourForFeeds} vs calendarTour=${ENV_DEFAULT_TOUR} feed="${fuEventFeed}" calendar="${calEvent}" projKey=${JSON.stringify(
          projKey,
        )} fuKey=${JSON.stringify(fuKey)} keysOk=${keysOk} fuzzy=${nameFuzzyOk} stale=${staleWeek}`
      );

      if (staleWeek) {
        console.warn(
          `[fetch-book-odds] projections look stale vs ${ENV_DEFAULT_TOUR} field-updates ("${projEvent || "(none)"}" vs "${calEvent}") — running fetch:dg …`
        );
        const dgScript = join(WEB_ROOT, "scripts", "fetch-datagolf.mjs");
        const r = spawnSync(process.execPath, [dgScript], {
          cwd: WEB_ROOT,
          stdio: "inherit",
          env: { ...process.env, GOLF_MODEL_DIR: GOLF_MODEL_ROOT, DATAGOLF_API_KEY: key },
        });
        if (r.status !== 0) {
          console.warn("[fetch-book-odds] fetch:dg exited", r.status, "— merging book odds into existing projections.");
        } else {
          try {
            payload = JSON.parse(readFileSync(projPath, "utf8"));
          } catch (e) {
            console.warn("[fetch-book-odds] could not re-read projections after fetch:dg:", e.message);
          }
        }
      }
    } catch (e) {
      console.warn("[fetch-book-odds] field-updates preflight:", e.message || e);
    }
  }

  const pretByDg = new Map();
  if (process.env.GOLF_SKIP_PRET_FOR_ODDS !== "1") {
    try {
      const pretDeadHeat = (process.env.GOLF_PRE_TOURNAMENT_DEAD_HEAT || "yes").trim().toLowerCase();
      const pretOddsFormat = (process.env.GOLF_PRE_TOURNAMENT_ODDS_FORMAT || "decimal").trim().toLowerCase();
      const pretAddPos = (process.env.GOLF_PRE_TOURNAMENT_ADD_POSITION || "").trim();
      console.log("Fetching preds/pre-tournament (for outright datagolf fill)…");
      const pretParams = {
        tour: tourForFeeds,
        dead_heat: pretDeadHeat === "no" ? "no" : "yes",
        odds_format: pretOddsFormat,
        file_format: "json",
      };
      if (pretAddPos) pretParams.add_position = pretAddPos;
      const pret = await fetchDg("/preds/pre-tournament", pretParams, key);
      const pretList = asArray(pret.baseline_history_fit).length
        ? asArray(pret.baseline_history_fit)
        : asArray(pret.baseline).length
          ? asArray(pret.baseline)
          : rowsFromResponse(pret);
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
    } catch (e) {
      console.warn("preds/pre-tournament skipped:", e.message);
    }
  }

  const outrightsMarkets = ["win", "top_5", "top_10", "top_20", "make_cut", "mc"];
  let outrights = { ...(payload.outrights && typeof payload.outrights === "object" ? payload.outrights : {}) };
  for (const m of outrightsMarkets) {
    try {
      console.log(`Fetching betting-tools/outrights (${m}, dead_heat=${outrightDeadHeatForMarket(m)})…`);
      const raw = await fetchDg(
        "/betting-tools/outrights",
        {
          tour: tourForFeeds,
          market: m,
          odds_format: outrightsOddsFormat,
          dead_heat: outrightDeadHeatForMarket(m),
          file_format: "json",
        },
        key
      );
      const { rows } = parseOutrightsResponse(raw);
      enrichOutrightsRows(rows, m, pretByDg);
      if (rows.length > 0) outrights[m] = { rows, bookKeys: outrightBookKeysFromRows(rows) };
    } catch (e) {
      console.warn(`Outrights ${m} skipped:`, e.message);
    }
  }

  let sportsbookOutrights = {};
  try {
    console.log("Scraping sportsbook outright/finish pages…");
    const scraped = await fetchSportsbookOutrightsFromUrls({ players: payload.players });
    sportsbookOutrights = scraped.outrights || {};
    for (const msg of scraped.logs || []) console.log("[sportsbook-outrights]", msg);
    const n = Object.values(sportsbookOutrights).reduce((sum, pack) => sum + (Array.isArray(pack?.rows) ? pack.rows.length : 0), 0);
    if (n > 0) {
      if (scrapedOutrightsHasBook(sportsbookOutrights, "draftkings")) {
        outrights = removeBookFromOutrights(outrights, "draftkings");
      }
      Object.assign(outrights, mergeScrapedOutrights(outrights, sportsbookOutrights));
      console.log(`[sportsbook-outrights] Replaced DataGolf API sportsbook columns on ${Object.keys(sportsbookOutrights).join(", ")} with ${n} scraped rows.`);
    } else {
      console.warn("[sportsbook-outrights] No parseable sportsbook rows found; keeping existing DataGolf API book columns.");
    }
  } catch (e) {
    console.warn("[sportsbook-outrights] skipped:", e.message || e);
  }
  outrights = removeBookFromOutrights(outrights, "datagolf");

  const matchupMarkets = ["tournament_matchups", "round_matchups", "3_balls"];
  const matchups = { ...(payload.matchups && typeof payload.matchups === "object" ? payload.matchups : {}) };
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

  const next = {
    ...payload,
    outrights,
    matchups,
    outrights_odds_format: outrightsOddsFormat,
    matchups_odds_format: "decimal",
    updated_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    book_odds_refreshed_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
  };
  if (next.outrights_model_blend_weight == null) next.outrights_model_blend_weight = 1;
  if (next.outright_win_score_blend == null) next.outright_win_score_blend = 0;
  if (next.outright_live_score_placement_nudge == null) next.outright_live_score_placement_nudge = false;

  if (process.env.GOLF_SKIP_PROPS_CSV !== "1" || process.env.GOLF_SKIP_DK_OU !== "1") {
    const csvPropsRaw = process.env.GOLF_SKIP_PROPS_CSV === "1" ? [] : loadOuPropsFromRepoCsv();
    const csvProps = withPropSource(csvPropsRaw, "csv");
    let dkProps = [];
    if (process.env.GOLF_SKIP_DK_OU !== "1") {
      try {
        const dkLeagueUrl = inferDraftKingsLeagueUrlFromProjections(payload);
        console.log(
          "[fetch-book-odds] DK O/U scrape:",
          dkLeagueUrl ? dkLeagueUrl : "default URL (set DK_LEAGUE_URL or dk_league_slug on payload)",
        );
        const dk = await fetchDraftKingsOuProps({
          players: payload.players,
          ...(dkLeagueUrl ? { leagueUrl: dkLeagueUrl } : {}),
        });
        dkProps = withPropSource(dk.props || [], "draftkings");
        if (!dkProps.length && process.env.GOLF_SKIP_DK_OU !== "1") {
          console.warn(
            "DraftKings O/U:",
            dk.error && !String(dk.error).startsWith("skipped")
              ? dk.error
              : "0 props — check [draftkings-ou] logs above (Playwright, DK_SITE_SEGMENT, npm run fetch:dk-ou)",
          );
        } else if (dk.error && !String(dk.error).startsWith("skipped")) {
          console.warn("DraftKings O/U:", dk.error);
        }
        if (dkProps.length && dk.subcatsUsed && Object.keys(dk.subcatsUsed).length) {
          console.log("DraftKings props subcategories", dk.subcatsUsed);
        }
      } catch (e) {
        console.warn("DraftKings O/U skipped:", e.message);
      }
    }
    const byKey = new Map();
    for (const r of csvProps) {
      byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
    }
    for (const r of dkProps) {
      const m = String(r.market || "").trim();
      if (
        m === "Birdies" ||
        m === "Pars" ||
        m === "Bogeys" ||
        m === "Total Score" ||
        m === "GIR" ||
        m === "Fairways hit" ||
        m === "Putts"
      ) {
        byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
      }
    }

    stripNonDkCountingProps(byKey);

    const dkCountingPresence = new Set();
    for (const r of dkProps) {
      const m = String(r.market || "").trim();
      if (!OU_COUNTING_MARKETS_FW.includes(m)) continue;
      dkCountingPresence.add(propPlayerMarketPresenceKey(r, m));
    }

    if (String(process.env.GOLF_SKIP_MODEL_FALLBACK_OU || "").trim() !== "1") {
      for (const mkt of OU_COUNTING_MARKETS_FW) {
        const fresh = withPropSource(modelFallbackOuForMarket(payload.players, mkt), "model_fallback");
        for (const r of fresh) {
          if (dkCountingPresence.has(propPlayerMarketPresenceKey(r, mkt))) continue;
          byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
        }
      }
    }
    const merged = [...byKey.values()];
    if (merged.length) {
      next.props = merged;
      const nCsv = csvProps.length;
      const nDk = dkProps.length;
      console.log(
        "Merged",
        merged.length,
        "Model O/U prop rows (CSV:",
        nCsv,
        "rows; DK auto:",
        nDk,
        "rows; GIR/FW/Putts refreshed from players where DK omits)",
      );
    }
  }

  const outJson = JSON.stringify(next, null, 2);
  writeFileSync(projPath, outJson, "utf8");
  console.log("Wrote", projPath, "(outrights + matchups only; players unchanged)");

  const websiteDataDir = join(GOLF_MODEL_ROOT, "website", "public", "data");
  const websiteProj = join(websiteDataDir, "projections.json");
  if (existsSync(websiteDataDir)) {
    writeFileSync(websiteProj, outJson, "utf8");
    console.log("Wrote", websiteProj);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
