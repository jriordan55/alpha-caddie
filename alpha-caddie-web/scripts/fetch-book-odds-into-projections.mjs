#!/usr/bin/env node
/**
 * Refresh only sportsbook columns: fetches betting-tools/outrights + matchups (+ preds/pre-tournament
 * for datagolf column fill) and merges into existing alpha-caddie-web/projections.json.
 *
 * Use between full `npm run fetch:dg` / R exports so the static app sees current lines without rebuilding players.
 *
 *   npm run fetch:book-odds
 *
 * Env: DATAGOLF_API_KEY or datagolf.local.json; GOLF_MODEL_DIR (repo root); GOLF_TOUR (default pga).
 *      GOLF_OUTRIGHTS_DEAD_HEAT=yes|no — same as fetch-datagolf.mjs
 *      GOLF_SKIP_PROPS_CSV=1 — do not merge data/player_props_*.csv into projections.props (Model O/U DK lines).
 *      GOLF_SKIP_DK_OU=1 — do not pull DK round props (see draftkings-ou-props.mjs).
 *
 * DraftKings round props (Birdies, Pars, Bogeys, Round Score → Total Score) use Playwright + Chromium.
 * Production (Render): `playwright` is a runtime dependency; build should run `npx playwright install chromium`.
 * Point DK at the active event: DK_LEAGUE_URL (e.g. …/leagues/golf/pga?category=round) and DK_LEAGUE_ID (from DK URL).
 * CSV files still override or fill gaps when DK omits a player or market.
 */
import { parse } from "csv-parse/sync";
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { fetchDraftKingsOuProps } from "./draftkings-ou-props.mjs";
const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const TOUR = process.env.GOLF_TOUR || "pga";

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

const OU_PROP_CSV_FILES = [
  ["Total Score", "player_props_lines.csv"],
  ["Birdies", "player_props_birdies.csv"],
  ["Pars", "player_props_pars.csv"],
  ["Bogeys", "player_props_bogeys.csv"],
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

function normProb01(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x)) return NaN;
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

function impliedPercentFromDgPercentField(v) {
  if (!Number.isFinite(v)) return NaN;
  let p = v;
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
      r.datagolf = impliedPercentFromDgPercentField(pv);
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
    const pct = impliedPercentFromDgPercentField(p);
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
      const pct = impliedPercentFromDgPercentField(v);
      if (!Number.isFinite(pct)) continue;
      out[key] = pct;
      bookSet.add(key);
    }
    rows.push(out);
  }
  return { rows, bookKeys: [...bookSet].sort() };
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

  const pretByDg = new Map();
  if (process.env.GOLF_SKIP_PRET_FOR_ODDS !== "1") {
    try {
      console.log("Fetching preds/pre-tournament (for outright datagolf fill)…");
      const pret = await fetchDg(
        "/preds/pre-tournament",
        { tour: TOUR, dead_heat: "no", odds_format: "percent", file_format: "json" },
        key
      );
      const pretList = asArray(pret.baseline_history_fit).length
        ? asArray(pret.baseline_history_fit)
        : asArray(pret.baseline).length
          ? asArray(pret.baseline)
          : rowsFromResponse(pret);
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
    } catch (e) {
      console.warn("preds/pre-tournament skipped:", e.message);
    }
  }

  const outrightsMarkets = ["win", "top_5", "top_10", "top_20", "make_cut", "mc"];
  const outrights = { ...(payload.outrights && typeof payload.outrights === "object" ? payload.outrights : {}) };
  for (const m of outrightsMarkets) {
    try {
      console.log(`Fetching betting-tools/outrights (${m}, dead_heat=${outrightDeadHeatForMarket(m)})…`);
      const raw = await fetchDg(
        "/betting-tools/outrights",
        {
          tour: TOUR,
          market: m,
          odds_format: "percent",
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

  const matchupMarkets = ["tournament_matchups", "round_matchups", "3_balls"];
  const matchups = { ...(payload.matchups && typeof payload.matchups === "object" ? payload.matchups : {}) };
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

  const next = {
    ...payload,
    outrights,
    matchups,
    outrights_odds_format: "percent",
    matchups_odds_format: "decimal",
    updated_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    book_odds_refreshed_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
  };

  if (process.env.GOLF_SKIP_PROPS_CSV !== "1" || process.env.GOLF_SKIP_DK_OU !== "1") {
    const csvProps = process.env.GOLF_SKIP_PROPS_CSV === "1" ? [] : loadOuPropsFromRepoCsv();
    let dkProps = [];
    if (process.env.GOLF_SKIP_DK_OU !== "1") {
      try {
        const dk = await fetchDraftKingsOuProps({ players: payload.players });
        dkProps = dk.props || [];
        if (dk.error && !String(dk.error).startsWith("skipped")) console.warn("DraftKings O/U:", dk.error);
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
      if (m === "Birdies" || m === "Pars" || m === "Bogeys" || m === "Total Score") {
        byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
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
        "rows)",
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
