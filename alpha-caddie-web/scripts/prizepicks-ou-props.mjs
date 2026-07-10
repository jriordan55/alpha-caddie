/**
 * Pull PrizePicks PGA round projections (Strokes, Birdies, Pars, Bogeys, GIR, Fairways) via
 * partner-api.prizepicks.com (preferred) or Playwright session capture on app.prizepicks.com.
 *
 * CLI (`npm run fetch:pp-ou`): reads projections.json for player name matching + target round.
 *
 * Env:
 *   GOLF_SKIP_PP_OU=1 — skip entirely
 *   PP_LEAGUE_ID — default 1 (PGA)
 *   PP_API_BASE — default https://partner-api.prizepicks.com
 *   PP_STATE_CODE — geo filter (optional; partner API often omits)
 *   PP_GAME_MODE — pickem (default) or prizepools
 *   PP_HEADLESS — 0 = headed browser for Playwright fallback (Windows default: headed)
 *   PP_TARGET_ROUND — override round filter (else display_round from projections)
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { chromium } from "playwright";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import { applyPrizePicksImpliedOddsAll } from "./prizepicks-implied-odds.mjs";
import { dedupePpPropsOnePerPlayerMarket, ppLineIsSane } from "./pp-ou-line-sanity.mjs";
import { ppMatchingGameIds } from "./pp-field-align.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));

const PP_LEAGUE_ID = String(process.env.PP_LEAGUE_ID || "1").trim();
const PP_API_BASE = String(process.env.PP_API_BASE || "https://partner-api.prizepicks.com").replace(/\/$/, "");
const PP_GAME_MODE = String(process.env.PP_GAME_MODE || "pickem").trim() || "pickem";
const PP_INCLUDE_DEMON_GOBLIN = String(process.env.PP_INCLUDE_DEMON_GOBLIN || "").trim() === "1";

const FETCH_HEADERS = {
  Accept: "application/json",
  Referer: "https://app.prizepicks.com/",
  "User-Agent":
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
};

/** @type {Record<string, string>} */
const PP_STAT_TO_MARKET = {
  Strokes: "Total Score",
  "Birdies Or Better": "Birdies",
  "Birdies or Better": "Birdies",
  "Bogeys or Worse": "Bogeys",
  "Bogeys or worse": "Bogeys",
  Pars: "Pars",
  "Greens in Regulation": "GIR",
  "Greens In Regulation": "GIR",
  "Green in Regulation": "GIR",
  GIR: "GIR",
  "Fairways Hit": "Fairways hit",
  "Fairways hit": "Fairways hit",
  "Fairways hit": "Fairways hit",
};

const ROUND_MARKETS = new Set([
  "Total Score",
  "Birdies",
  "Pars",
  "Bogeys",
  "GIR",
  "Fairways hit",
]);

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function resolvePpHeadless() {
  const v = String(process.env.PP_HEADLESS ?? "").trim().toLowerCase();
  if (v === "0" || v === "false" || v === "no") return false;
  if (v === "1" || v === "true" || v === "yes") return true;
  return process.platform !== "win32" && process.platform !== "darwin";
}

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

function buildIncludedMaps(included) {
  const players = new Map();
  const stats = new Map();
  const games = new Map();
  for (const x of included || []) {
    if (x.type === "new_player") players.set(String(x.id), x.attributes || {});
    if (x.type === "stat_type") stats.set(String(x.id), x.attributes || {});
    if (x.type === "game") games.set(String(x.id), x.attributes || {});
  }
  return { players, stats, games };
}

function parseRoundFromText(...parts) {
  for (const raw of parts) {
    const s = String(raw || "");
    const m = s.match(/\bRD\s*(\d+)\b/i) || s.match(/\bR\s*(\d+)\b/i) || s.match(/\bRound\s+(\d+)\b/i);
    if (m) {
      const r = Math.round(num(m[1], NaN));
      if (r >= 1 && r <= 4) return r;
    }
  }
  return NaN;
}

function canonicalPpMarket(statName) {
  const s = String(statName || "").trim();
  if (!s) return "";
  if (PP_STAT_TO_MARKET[s]) return PP_STAT_TO_MARKET[s];
  const low = s.toLowerCase();
  if (low.includes("stroke")) return "Total Score";
  if (low.includes("bird")) return "Birdies";
  if (low.includes("bogey")) return "Bogeys";
  if (low === "pars" || low.startsWith("par")) return "Pars";
  if (low.includes("green")) return "GIR";
  if (low.includes("fairway")) return "Fairways hit";
  return "";
}

function projectionUrl(base, leagueId, page, extra = {}) {
  const q = new URLSearchParams({
    league_id: String(leagueId),
    per_page: "250",
    single_stat: "true",
    game_mode: PP_GAME_MODE,
    page: String(page),
    ...extra,
  });
  if (process.env.PP_STATE_CODE?.trim()) q.set("state_code", process.env.PP_STATE_CODE.trim());
  return `${base}/projections?${q}`;
}

async function fetchJsonWithRetry(url, attempts = 4) {
  let waitMs = 35000;
  for (let i = 0; i < attempts; i++) {
    const res = await fetch(url, { headers: FETCH_HEADERS });
    if (res.status === 429) {
      const retryAfter = Math.max(30, Math.round(num(res.headers.get("retry-after"), waitMs / 1000))) * 1000;
      console.warn(`[prizepicks-ou] rate limited — waiting ${Math.round(retryAfter / 1000)}s`);
      await sleep(retryAfter);
      waitMs = Math.min(120000, waitMs * 2);
      continue;
    }
    const text = await res.text();
    let body;
    try {
      body = JSON.parse(text);
    } catch {
      return { ok: false, status: res.status, error: `non-JSON (${text.slice(0, 120)})` };
    }
    if (!res.ok) return { ok: false, status: res.status, error: body?.title || body?.detail || `HTTP ${res.status}` };
    if (body?.status === 429 || body?.error_name === "rate_limited") {
      await sleep(waitMs);
      waitMs = Math.min(120000, waitMs * 2);
      continue;
    }
    return { ok: true, status: res.status, body };
  }
  return { ok: false, status: 429, error: "rate limited after retries" };
}

/**
 * @param {string} base
 * @param {string} leagueId
 */
async function fetchAllProjectionsFromApi(base, leagueId) {
  const all = [];
  let included = [];
  let page = 1;
  let totalPages = 1;
  while (page <= totalPages && page <= 20) {
    const url = projectionUrl(base, leagueId, page);
    const hit = await fetchJsonWithRetry(url);
    if (!hit.ok) return { body: null, error: hit.error, url };
    const body = hit.body;
    if (page === 1) included = body.included || [];
    const chunk = body.data || [];
    all.push(...chunk);
    totalPages = Math.max(1, Math.round(num(body.meta?.total_pages, 1)));
    if (!chunk.length) break;
    page++;
    if (page <= totalPages) await sleep(1200);
  }
  return { body: { data: all, included }, error: null };
}

async function fetchViaPlaywright(leagueId) {
  const headless = resolvePpHeadless();
  const browser = await chromium.launch({
    headless,
    args: headless ? ["--disable-blink-features=AutomationControlled"] : undefined,
  });
  try {
    const ctx = await browser.newContext({ viewport: { width: 1400, height: 900 }, locale: "en-US" });
    await ctx.addInitScript(() => {
      Object.defineProperty(navigator, "webdriver", { get: () => false });
    });
    const page = await ctx.newPage();
    let captured = null;
    const want = `league_id=${leagueId}`;
    page.on("response", async (res) => {
      const u = res.url();
      if (!u.includes("api.prizepicks.com/projections") || !u.includes(want) || res.status() !== 200) return;
      try {
        captured = await res.json();
      } catch (_) {}
    });
    await page.goto("https://app.prizepicks.com/", { waitUntil: "networkidle", timeout: 90000 }).catch(() => {});
    await page.waitForTimeout(4000);
    await page.evaluate(() => document.querySelector("#lanyard_root")?.remove());
    await page.evaluate(() => {
      const spans = [...document.querySelectorAll("span.name, [class*='name']")];
      const pga = spans.find((s) => /^PGA$/i.test((s.textContent || "").trim()));
      (pga?.closest("button,a,[role='button']") || pga)?.click?.();
    });
    await page.waitForTimeout(12000);
    if (!captured) {
      return { body: null, error: "Playwright session did not capture PGA projections (bot block or UI change)" };
    }
    return { body: captured, error: null };
  } finally {
    await browser.close();
  }
}

/**
 * @param {object} body
 * @param {object} [payload] projections.json (players + meta + DK props for odds)
 * @param {number} [targetRound]
 */
export function propsFromPrizePicksBody(body, payload = {}, targetRound = NaN) {
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const { players: playerMap, stats: statMap, games: gameMap } = buildIncludedMaps(body?.included);
  const wantRound = Math.round(num(targetRound, NaN));
  const matchingGameIds = ppMatchingGameIds(gameMap, payload);
  const out = [];
  for (const row of body?.data || []) {
    const attrs = row.attributes || {};
    const rel = row.relationships || {};
    const gameId = String(rel.game?.data?.id || "");
    if (matchingGameIds && gameId && !matchingGameIds.has(gameId)) continue;
    const pl = playerMap.get(String(rel.new_player?.data?.id || ""));
    const st = statMap.get(String(rel.stat_type?.data?.id || ""));
    const gm = gameMap.get(gameId);
    const statName = String(st?.name || attrs.stat_type || "").trim();
    const market = canonicalPpMarket(statName);
    if (!market || !ROUND_MARKETS.has(market)) continue;

    const line = num(attrs.line_score, NaN);
    if (!Number.isFinite(line)) continue;

    const oddsType = String(attrs.odds_type || "standard").trim().toLowerCase();
    if (!PP_INCLUDE_DEMON_GOBLIN && oddsType !== "standard") {
      const allowAlt =
        (market === "GIR" || market === "Fairways hit") && ppLineIsSane(market, line);
      if (!allowAlt) continue;
    }

    const playerLabel = String(pl?.name || pl?.display_name || "").trim();
    if (!playerLabel) continue;

    let roundNum = parseRoundFromText(attrs.description, gm?.metadata?.game_info, gm?.name, attrs.start_time);
    if (!Number.isFinite(roundNum) && Number.isFinite(wantRound)) roundNum = wantRound;

    const matched = matchPlayerByGolferLabel(players, playerLabel);
    const player_name = matched ? String(matched.player_name || "").trim() : playerLabel;
    const dg_id = matched ? Math.round(num(matched.dg_id, NaN)) : undefined;

    const prop = {
      player_name,
      line,
      market,
      source: "prizepicks",
    };
    if (Number.isFinite(dg_id) && dg_id > 0) prop.dg_id = dg_id;
    if (Number.isFinite(roundNum) && roundNum >= 1 && roundNum <= 4) prop.round_num = roundNum;
    out.push(prop);
  }

  const dkProps = (Array.isArray(payload?.props) ? payload.props : []).filter(
    (r) => String(r?.source || "").trim().toLowerCase() === "draftkings",
  );
  const deduped = dedupePpPropsOnePerPlayerMarket(
    out.map((r) => ({ ...r, source: "prizepicks" })),
    dkProps,
  );
  return applyPrizePicksImpliedOddsAll(deduped).filter(
    (r) =>
      Number.isFinite(num(r.over_odds, NaN)) &&
      Number.isFinite(num(r.under_odds, NaN)) &&
      num(r.over_odds, NaN) !== 0 &&
      num(r.under_odds, NaN) !== 0,
  );
}

function preferPropsForTargetRound(props, targetRound) {
  const want = Math.round(num(targetRound, NaN));
  if (!Number.isFinite(want) || want < 1 || want > 4) return props;
  const numbered = props.filter((r) => Number.isFinite(Math.round(num(r.round_num, NaN))));
  if (!numbered.length) return props;
  const forRound = numbered.filter((r) => Math.round(num(r.round_num, NaN)) === want);
  if (forRound.length) return forRound;
  const unnumbered = props.filter((r) => !Number.isFinite(Math.round(num(r.round_num, NaN))));
  return [...unnumbered, ...numbered];
}

/**
 * @param {{ players?: object[], payload?: object, targetRound?: number, leagueId?: string }} [opts]
 */
export async function fetchPrizePicksOuProps(opts = {}) {
  if (process.env.GOLF_SKIP_PP_OU === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_PP_OU=1)" };
  }
  const payload = opts.payload || { players: opts.players || [] };
  const players = Array.isArray(payload.players) ? payload.players : opts.players || [];
  const leagueId = String(opts.leagueId || PP_LEAGUE_ID).trim();
  const targetRound = Math.round(
    num(opts.targetRound ?? opts.displayRound ?? process.env.PP_TARGET_ROUND ?? NaN, NaN),
  );
  console.log(
    `[prizepicks-ou] league_id=${leagueId} api=${PP_API_BASE} players=${players.length}${Number.isFinite(targetRound) ? ` targetRound=R${targetRound}` : ""}`,
  );

  let body = null;
  let error = null;
  for (const base of [PP_API_BASE, "https://api.prizepicks.com"]) {
    const hit = await fetchAllProjectionsFromApi(base, leagueId);
    if (hit.body?.data?.length) {
      body = hit.body;
      break;
    }
    error = hit.error || error;
    if (base === PP_API_BASE) console.warn(`[prizepicks-ou] ${base}: ${hit.error || "0 rows"} — trying fallback`);
  }

  if (!body?.data?.length) {
    console.warn("[prizepicks-ou] direct API empty — trying Playwright capture");
    const pw = await fetchViaPlaywright(leagueId);
    if (pw.body?.data?.length) body = pw.body;
    else error = pw.error || error;
  }

  let props = propsFromPrizePicksBody(body, { ...payload, players }, targetRound);
  props = preferPropsForTargetRound(props, targetRound);

  if (!props.length) {
    const hint =
      error ||
      "0 parsed PrizePicks rows (no PGA lines posted, rate limited, or stat name mapping changed)";
    console.warn("[prizepicks-ou]", hint);
    return { props: [], error: hint };
  }
  console.log(`[prizepicks-ou] ${props.length} prop row(s)`);
  return { props, error: null };
}

async function main() {
  const proj = join(__dirname, "..", "projections.json");
  let payload = { players: [] };
  let targetRound = NaN;
  if (existsSync(proj)) {
    try {
      payload = JSON.parse(readFileSync(proj, "utf8"));
      targetRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN));
    } catch {
      /* ignore */
    }
  }
  const { props, error } = await fetchPrizePicksOuProps({ payload, targetRound });
  console.log(JSON.stringify({ n: props.length, error: error || null }, null, 2));
  if (props[0]) console.log("sample", props[0]);
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
