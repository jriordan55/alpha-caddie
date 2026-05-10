#!/usr/bin/env node
/**
 * Odds for Course Fit / +EV outright columns: align with DataGolf “Finish Position” scratch tool.
 *
 * **Same source as the website:** https://datagolf.com/betting-tool-finish loads lines from
 * `GET https://feeds.datagolf.com/betting-tools/outrights` (IMPLIED % ↔ `odds_format=percent`).
 * HTML scraping is not used — tables are Scratch-gated and SPA-rendered; the supported path is this API.
 *
 * Default: pulls every outright market with your API key and merges into `projections.json`
 * (same shape as `npm run fetch:dg` / `fetch:book-odds` outrights).
 *
 * Optional — literally capture JSON from the browser session:
 *   GOLF_FINISH_TOOL_PLAYWRIGHT=1
 *   DATAGOLF_PLAYWRIGHT_STORAGE_STATE=/path/to/storage.json  (log in once via `npx playwright codegen`
 *   datagolf.com --save-storage=storage.json`, then pass that path)
 *
 *   npm run fetch:finish-tool
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";

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

async function captureOutrightsViaPlaywright(tourForFeeds) {
  const storage = String(process.env.DATAGOLF_PLAYWRIGHT_STORAGE_STATE || "").trim();
  const { chromium } = await import("playwright");
  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext(
    storage && existsSync(storage) ? { storageState: storage } : {},
  );
  const page = await context.newPage();
  /** @type {Record<string, unknown>} */
  const captured = {};
  page.on("response", async (response) => {
    try {
      const url = response.url();
      if (!url.includes("feeds.datagolf.com")) return;
      if (!url.includes("betting-tools/outrights")) return;
      if (response.status() !== 200) return;
      const u = new URL(url);
      const market = u.searchParams.get("market");
      const tour = u.searchParams.get("tour") || "";
      if (!market) return;
      if (tour && tourForFeeds && tour !== tourForFeeds) return;
      const ct = (response.headers()["content-type"] || "").toLowerCase();
      if (!ct.includes("json")) return;
      const json = await response.json();
      captured[market] = json;
    } catch {
      /* ignore */
    }
  });
  await page.goto("https://datagolf.com/betting-tool-finish", {
    waitUntil: "domcontentloaded",
    timeout: 120000,
  });
  await page.waitForTimeout(10000);
  await browser.close();
  const n = Object.keys(captured).length;
  console.log(
    `[fetch:finish-tool] Playwright captured ${n} outright response(s): ${Object.keys(captured).join(", ") || "(none)"}`,
  );
  return captured;
}

async function main() {
  const key = loadApiKey();
  if (!key) {
    console.error("Set DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json apiKey.");
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

  const tourForFeeds = String(payload.datagolf_feed_tour || "").trim().toLowerCase() || ENV_DEFAULT_TOUR;

  /** @type {Record<string, unknown>} */
  let playwrightRawByMarket = {};
  if (String(process.env.GOLF_FINISH_TOOL_PLAYWRIGHT || "").trim() === "1") {
    console.log("[fetch:finish-tool] Loading datagolf.com/betting-tool-finish (network capture)…");
    playwrightRawByMarket = await captureOutrightsViaPlaywright(tourForFeeds);
  }

  const pretByDg = new Map();
  if (process.env.GOLF_SKIP_PRET_FOR_ODDS !== "1") {
    try {
      const pretDeadHeat = (process.env.GOLF_PRE_TOURNAMENT_DEAD_HEAT || "yes").trim().toLowerCase();
      const pretOddsFormat = (process.env.GOLF_PRE_TOURNAMENT_ODDS_FORMAT || "decimal").trim().toLowerCase();
      const pretAddPos = (process.env.GOLF_PRE_TOURNAMENT_ADD_POSITION || "").trim();
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
      console.warn("[fetch:finish-tool] preds/pre-tournament skipped:", e.message || e);
    }
  }

  const outrightsMarkets = ["win", "top_5", "top_10", "top_20", "make_cut", "mc"];
  const outrights = { ...(payload.outrights && typeof payload.outrights === "object" ? payload.outrights : {}) };

  for (const m of outrightsMarkets) {
    let raw = playwrightRawByMarket[m];
    try {
      if (raw == null) {
        console.log(
          `Fetching betting-tools/outrights (${m}, dead_heat=${outrightDeadHeatForMarket(m)}, odds_format=${outrightsOddsFormat})…`,
        );
        raw = await fetchDg(
          "/betting-tools/outrights",
          {
            tour: tourForFeeds,
            market: m,
            odds_format: outrightsOddsFormat,
            dead_heat: outrightDeadHeatForMarket(m),
            file_format: "json",
          },
          key,
        );
      } else {
        console.log(`Using Playwright-captured JSON for market=${m}`);
      }
      const { rows } = parseOutrightsResponse(raw);
      enrichOutrightsRows(rows, m, pretByDg);
      if (rows.length > 0) outrights[m] = { rows, bookKeys: outrightBookKeysFromRows(rows) };
    } catch (e) {
      console.warn(`[fetch:finish-tool] Outrights ${m} skipped:`, e.message || e);
    }
  }

  const next = {
    ...payload,
    outrights,
    outrights_odds_format: outrightsOddsFormat,
    updated_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    book_odds_refreshed_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
  };

  writeFileSync(projPath, JSON.stringify(next, null, 2), "utf8");
  console.log(`[fetch:finish-tool] Wrote ${projPath} (outrights only). Course Fit / +EV read DATA.outrights from this file.`);

  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  if (existsSync(dirname(websiteProj))) {
    writeFileSync(websiteProj, JSON.stringify(next, null, 2), "utf8");
    console.log(`[fetch:finish-tool] Mirrored -> ${websiteProj}`);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
