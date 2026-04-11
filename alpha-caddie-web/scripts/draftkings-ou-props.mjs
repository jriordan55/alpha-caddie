/**
 * Pull DraftKings round O/U props (Birdies or Better, Pars, Bogeys or Worse) via
 * sportsbook-nash leagueSubcategory markets API, using Playwright for session cookies.
 *
 * Env:
 *   GOLF_SKIP_DK_OU=1 — skip entirely
 *   DK_LEAGUE_URL — default https://sportsbook.draftkings.com/leagues/golf/us-masters?category=round
 *   DK_SITE_SEGMENT — default US-MA-SB (set to your state segment if requests fail)
 *   DK_LEAGUE_ID — default 92694 (The Masters)
 *   DK_SUBCAT_JSON — optional override, e.g. {"Birdies":"17299","Pars":"17300","Bogeys":"17301"}
 *
 * "Total Score" (18-hole round strokes ~66–80): verified absent from sportsbook-nash
 * leagueSubcategory/markets for every Masters (92694) nav subcategory — only nine-hole style
 * markets appear in that API (e.g. Front 9 / Back 9 Score ~35.5 on sub 19585 / 19586). Use
 * data/player_props_lines.csv for full-round stroke O/U when needed.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { chromium } from "playwright";

const __dirname = dirname(fileURLToPath(import.meta.url));

const DEFAULT_URL =
  process.env.DK_LEAGUE_URL?.trim() ||
  "https://sportsbook.draftkings.com/leagues/golf/us-masters?category=round";
const SITE = process.env.DK_SITE_SEGMENT?.trim() || "US-MA-SB";
const LEAGUE_ID = process.env.DK_LEAGUE_ID?.trim() || "92694";

const STAT_BY_SEO = {
  "birdies-or-better": "Birdies",
  pars: "Pars",
  "bogeys-or-worse": "Bogeys",
};

function marketsUrl(leagueId, subcatId, siteSegment) {
  const seg = String(siteSegment || SITE);
  const sub = String(subcatId);
  const lg = String(leagueId);
  const templateVars = `${lg}%2C${sub}`;
  const eventsQuery = encodeURIComponent(
    `$filter=leagueId eq '${lg}' AND clientMetadata/Subcategories/any(s: s/Id eq '${sub}')`,
  );
  const marketsQuery = encodeURIComponent(
    `$filter=clientMetadata/subCategoryId eq '${sub}' AND tags/all(t: t ne 'SportcastBetBuilder')`,
  );
  return `https://sportsbook-nash.draftkings.com/sites/${seg}/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=${templateVars}&eventsQuery=${eventsQuery}&marketsQuery=${marketsQuery}&include=Events&entity=events`;
}

function displayGolferName(name) {
  const s = String(name || "").trim();
  const m = s.match(/^([^,]+),\s*(.+)$/);
  if (m) return `${m[2].trim()} ${m[1].trim()}`.trim();
  return s;
}

function buildDgLookup(players) {
  const m = new Map();
  if (!Array.isArray(players)) return m;
  for (const p of players) {
    const id = Math.round(Number(p?.dg_id));
    const pn = String(p?.player_name || "").trim();
    if (!Number.isFinite(id) || id <= 0 || !pn) continue;
    m.set(pn.toLowerCase(), id);
    m.set(displayGolferName(pn).toLowerCase(), id);
  }
  return m;
}

function parseAmerican(raw) {
  if (raw == null) return NaN;
  const s = String(raw)
    .replace(/\u2212/g, "-")
    .replace(/−/g, "-")
    .trim();
  const n = parseInt(s.replace(/^\+/, ""), 10);
  return Number.isFinite(n) ? n : NaN;
}

function discoverSubcatsFromInitialState(ini, wantLeagueId) {
  const lg = String(wantLeagueId);
  const bySeo = new Map();
  function walk(o, depth) {
    if (!o || typeof o !== "object" || depth > 45) return;
    const p = o.parameters;
    if (p && String(p.leagueId) === lg && p.subcategoryId != null) {
      const seo = String(o.seoId || "").trim().toLowerCase();
      if (seo && STAT_BY_SEO[seo]) {
        bySeo.set(seo, String(p.subcategoryId));
      }
    }
    if (Array.isArray(o)) {
      for (const x of o) walk(x, depth + 1);
      return;
    }
    for (const k of Object.keys(o)) walk(o[k], depth + 1);
  }
  walk(ini, 0);
  return bySeo;
}

const NAME_RE = {
  Birdies: /^(.+?)\s+Birdies or Better\s+-\s+Round\s+(\d+)\s*$/i,
  Pars: /^(.+?)\s+Pars\s+-\s+Round\s+(\d+)\s*$/i,
  Bogeys: /^(.+?)\s+Bogeys or Worse\s+-\s+Round\s+(\d+)\s*$/i,
};

function parseMarketName(stat, marketName) {
  const re = NAME_RE[stat];
  if (!re) return null;
  const m = String(marketName || "").trim().match(re);
  if (!m) return null;
  return { dkPlayer: m[1].trim(), round: Number(m[2]) };
}

function propsFromMarketsBody(body, stat, dgByNameLower) {
  const markets = body?.markets;
  const selections = body?.selections;
  if (!Array.isArray(markets) || !Array.isArray(selections)) return [];
  const byMarket = new Map();
  for (const s of selections) {
    const mid = String(s.marketId || "");
    if (!mid) continue;
    if (!byMarket.has(mid)) byMarket.set(mid, []);
    byMarket.get(mid).push(s);
  }
  const out = [];
  for (const mk of markets) {
    const parsed = parseMarketName(stat, mk.name);
    if (!parsed) continue;
    const sel = byMarket.get(String(mk.id)) || [];
    let overSel;
    let underSel;
    for (const s of sel) {
      const ot = String(s.outcomeType || "").toLowerCase();
      const lab = String(s.label || "").toLowerCase();
      if (ot === "over" || lab === "over") overSel = s;
      else if (ot === "under" || lab === "under") underSel = s;
    }
    if (!overSel || !underSel) continue;
    const lineRaw = Number(overSel.points ?? underSel.points);
    if (!Number.isFinite(lineRaw)) continue;
    let line = lineRaw;
    if (stat !== "Total Score" && line === Math.floor(line)) line += 0.5;
    const over = parseAmerican(overSel.displayOdds?.american);
    const under = parseAmerican(underSel.displayOdds?.american);
    if (!Number.isFinite(over) || !Number.isFinite(under)) continue;
    const player_name = parsed.dkPlayer;
    const o = { player_name, line, over_odds: over, under_odds: under, market: stat };
    const dg = dgByNameLower.get(player_name.toLowerCase());
    if (Number.isFinite(dg) && dg > 0) o.dg_id = dg;
    out.push(o);
  }
  return out;
}

/**
 * @param {{ players?: unknown[], leagueUrl?: string, leagueId?: string, siteSegment?: string }} [opts]
 * @returns {Promise<{ props: object[], subcatsUsed: Record<string, string>, error?: string }>}
 */
export async function fetchDraftKingsOuProps(opts = {}) {
  if (process.env.GOLF_SKIP_DK_OU === "1") {
    return { props: [], subcatsUsed: {}, error: "skipped (GOLF_SKIP_DK_OU=1)" };
  }
  const players = opts.players;
  const leagueUrl = opts.leagueUrl || DEFAULT_URL;
  const leagueId = opts.leagueId || LEAGUE_ID;
  const siteSegment = opts.siteSegment || SITE;
  const dgByNameLower = buildDgLookup(players);

  let overrides = {};
  const rawOv = process.env.DK_SUBCAT_JSON?.trim();
  if (rawOv) {
    try {
      overrides = JSON.parse(rawOv);
    } catch {
      return { props: [], subcatsUsed: {}, error: "invalid DK_SUBCAT_JSON" };
    }
  }

  let browser;
  try {
    browser = await chromium.launch({ headless: true });
  } catch (e) {
    return { props: [], subcatsUsed: {}, error: `playwright: ${e.message}` };
  }

  const ctx = await browser.newContext({
    userAgent:
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    viewport: { width: 1400, height: 900 },
  });
  const page = await ctx.newPage();
  try {
    await page.goto(leagueUrl, { waitUntil: "domcontentloaded", timeout: 90000 });
    await page.waitForTimeout(8000);
  } catch (e) {
    await browser.close();
    return { props: [], subcatsUsed: {}, error: `goto: ${e.message}` };
  }

  const bySeo = await page.evaluate((lid) => {
    const ini = window.__INITIAL_STATE__;
    if (!ini) return {};
    const lg = String(lid);
    const want = { "birdies-or-better": 1, pars: 1, "bogeys-or-worse": 1 };
    const bySeo = {};
    function walk(o, depth) {
      if (!o || typeof o !== "object" || depth > 45) return;
      const p = o.parameters;
      if (p && String(p.leagueId) === lg && p.subcategoryId != null) {
        const seo = String(o.seoId || "").trim().toLowerCase();
        if (want[seo]) bySeo[seo] = String(p.subcategoryId);
      }
      if (Array.isArray(o)) {
        for (const x of o) walk(x, depth + 1);
        return;
      }
      for (const k of Object.keys(o)) walk(o[k], depth + 1);
    }
    walk(ini, 0);
    return bySeo;
  }, leagueId);

  const subcatsUsed = {};
  const statToSub = {};
  for (const [seo, stat] of Object.entries(STAT_BY_SEO)) {
    const fromNav = bySeo[seo];
    const fromEnv = overrides[stat];
    const sub = fromEnv || fromNav;
    if (!sub) continue;
    statToSub[stat] = sub;
    subcatsUsed[stat] = sub;
  }

  if (Object.keys(statToSub).length === 0) {
    await browser.close();
    return {
      props: [],
      subcatsUsed: {},
      error: "Could not resolve DK subcategory ids (try DK_SUBCAT_JSON or DK_LEAGUE_URL)",
    };
  }

  const all = [];
  try {
    const api = ctx.request;
    for (const [stat, sub] of Object.entries(statToSub)) {
      const u = marketsUrl(leagueId, sub, siteSegment);
      const res = await api.get(u, { timeout: 60000 });
      if (!res.ok()) continue;
      const body = await res.json();
      all.push(...propsFromMarketsBody(body, stat, dgByNameLower));
    }
  } finally {
    await browser.close();
  }

  const dedup = new Map();
  for (const r of all) {
    dedup.set(`${r.player_name}|${r.market}|${r.line}`, r);
  }
  return { props: [...dedup.values()], subcatsUsed };
}

async function main() {
  const { readFileSync, existsSync } = await import("fs");
  const { dirname, join } = await import("path");
  const { fileURLToPath } = await import("url");
  const __dirname = dirname(fileURLToPath(import.meta.url));
  const proj = join(__dirname, "..", "projections.json");
  let players = [];
  if (existsSync(proj)) {
    try {
      const payload = JSON.parse(readFileSync(proj, "utf8"));
      players = payload.players || [];
    } catch {
      /* ignore */
    }
  }
  const { props, subcatsUsed, error } = await fetchDraftKingsOuProps({ players });
  console.log(JSON.stringify({ n: props.length, subcatsUsed, error: error || null }, null, 2));
  if (props[0]) console.log("sample", props[0]);
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
