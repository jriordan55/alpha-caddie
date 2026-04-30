/**
 * Pull DraftKings round O/U props (Birdies, Pars, Bogeys, GIR, Fairways, Putts, Round Score) via
 * sportsbook-nash leagueSubcategory markets API, using Playwright for session cookies.
 *
 * Env:
 *   GOLF_SKIP_DK_OU=1 — skip entirely
 *   DK_LEAGUE_URL — e.g. https://sportsbook.draftkings.com/leagues/golf/rbc-heritage?category=round
 *   DK_SITE_SEGMENT — default US-MA-SB (set to your state segment if requests fail)
 *   DK_LEAGUE_ID — optional explicit league id (auto-detected from page if omitted).
 *   DK_SUBCAT_JSON — optional override per stat (skips subcategory probe for keys you set).
 *   Putts / GIR / fairways / Birdies / Pars: nav ids often point at hole, “2 ball”, “player most”, or Par-N props;
 *   we probe league subs for titles like “… Putts - Round 1” / “… Birdies or Better - Round 1”.
 *
 * Note: "Total Score" can be absent on some events; fallback CSV still applies in fetch-book-odds-into-projections.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { chromium } from "playwright";

const __dirname = dirname(fileURLToPath(import.meta.url));

const DEFAULT_URL =
  process.env.DK_LEAGUE_URL?.trim() ||
  "https://sportsbook.draftkings.com/leagues/golf/rbc-heritage?category=round";
const SITE = process.env.DK_SITE_SEGMENT?.trim() || "US-MA-SB";
const LEAGUE_ID = process.env.DK_LEAGUE_ID?.trim() || "";

const STAT_BY_SEO = {
  "birdies-or-better": "Birdies",
  pars: "Pars",
  "bogeys-or-worse": "Bogeys",
  "greens-in-regulation": "GIR",
  "fairways-hit": "Fairways hit",
  "total-putts": "Putts",
  putts: "Putts",
};

/** Legacy fallback ids for older pages where nav state omits round stats. */
const FALLBACK_SUBCAT_BY_STAT = {
  Birdies: "17299",
  Pars: "17300",
  Bogeys: "17301",
};

/**
 * DraftKings reuses subcategory ids across "2 Ball …" nav labels vs field round O/U.
 * Prefer subs whose market *names* look like "Player X Putts - Round 1", not group/hole props.
 */
const PROBE_SUBS_FIRST = {
  Putts: ["17304", "17399"],
  GIR: [],
  "Fairways hit": [],
};

/** When nav omits Round Score tabs, try these Masters subcategory ids (merge + dedupe players). */
const FALLBACK_ROUND_SCORE_SUBS = ["11786", "18987"];

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

const ROUND_TAIL = String.raw`(?:Round\s+(\d+)|R(\d+))\s*$`;

const NAME_RE = {
  Birdies: new RegExp(`^(.+?)\\s+Birdies or Better\\s+-\\s+${ROUND_TAIL}`, "i"),
  Pars: new RegExp(`^(.+?)\\s+Pars\\s+-\\s+${ROUND_TAIL}`, "i"),
  Bogeys: new RegExp(`^(.+?)\\s+Bogeys or Worse\\s+-\\s+${ROUND_TAIL}`, "i"),
  GIR: new RegExp(
    `^(.+?)\\s+(?:Greens?\\s+in\\s+Regulation|GIR)\\s+-\\s+${ROUND_TAIL}`,
    "i",
  ),
  "Fairways hit": new RegExp(`^(.+?)\\s+Fairways?\\s+Hit\\s+-\\s+${ROUND_TAIL}`, "i"),
  Putts: new RegExp(`^(.+?)\\s+(?:Total\\s+)?Putts\\s+-\\s+${ROUND_TAIL}`, "i"),
};

const NAME_RE_TOTAL_SCORE = new RegExp(`^(.+?)\\s+Round Score\\s+-\\s+${ROUND_TAIL}`, "i");

function roundFromMatch(m) {
  if (!m) return NaN;
  const a = m[m.length - 2];
  const b = m[m.length - 1];
  const n = Number(a || b);
  return Number.isFinite(n) ? n : NaN;
}

function parseMarketName(stat, marketName) {
  const raw = String(marketName || "").trim();
  if (stat === "Total Score") {
    const m = raw.match(NAME_RE_TOTAL_SCORE);
    if (!m) return null;
    const rd = roundFromMatch(m);
    if (!Number.isFinite(rd)) return null;
    return { dkPlayer: m[1].replace(/\s+/g, " ").trim(), round: rd };
  }
  const re = NAME_RE[stat];
  if (!re) return null;
  const m = raw.match(re);
  if (!m) return null;
  const rd = roundFromMatch(m);
  if (!Number.isFinite(rd)) return null;
  return { dkPlayer: m[1].replace(/\s+/g, " ").trim(), round: rd };
}

/** True if `name` looks like a per-player round O/U title (not hole / group / side markets). */
function isGoodPlayerRoundSampleName(stat, name) {
  const s = String(name || "").trim();
  if (!s) return false;
  if (/\bon\s+hole\b/i.test(s)) return false;
  if (/number\s+of\s+greens/i.test(s)) return false;
  if (/total\s+group|group\s+drives|to\s+hit\s+a\s+gir/i.test(s)) return false;
  if (/player\s+most\b/i.test(s)) return false;
  return !!parseMarketName(stat, s);
}

function buildProbeOrder(stat, preferredSub, allLeagueSubIds) {
  const first = PROBE_SUBS_FIRST[stat] || [];
  const out = [];
  const add = (x) => {
    const id = String(x || "").trim();
    if (id && !out.includes(id)) out.push(id);
  };
  /** Cap league-wide scans — enough to escape wrong nav ids without one fetch per tab. */
  const max = 36;
  for (const id of first) {
    add(id);
    if (out.length >= max) return out;
  }
  add(preferredSub);
  for (const id of allLeagueSubIds || []) {
    add(id);
    if (out.length >= max) break;
  }
  return out;
}

async function pickSubcategoryForStat(api, leagueId, siteSegment, stat, preferredSub, allLeagueSubIds, dgByNameLower) {
  const candidates = buildProbeOrder(stat, preferredSub, allLeagueSubIds);
  let bestSub = "";
  let bestScore = 0;
  const seen = new Set();
  for (const sub of candidates) {
    if (!sub || seen.has(sub)) continue;
    seen.add(sub);
    const u = marketsUrl(leagueId, sub, siteSegment);
    const res = await api.get(u, { timeout: 60000 });
    if (!res.ok()) continue;
    let body;
    try {
      body = await res.json();
    } catch {
      continue;
    }
    const mk = Array.isArray(body?.markets) ? body.markets : [];
    const sample = mk.slice(0, 20).map((m) => m.name);
    if (!sample.some((n) => isGoodPlayerRoundSampleName(stat, n))) continue;
    const nParsed = propsFromMarketsBody(body, stat, dgByNameLower).length;
    if (nParsed > bestScore) {
      bestScore = nParsed;
      bestSub = sub;
      if (nParsed >= 12) break;
    }
    await new Promise((r) => setTimeout(r, 45));
  }
  return bestScore > 0 ? bestSub : "";
}

function lineFromSelection(s) {
  const pts = s.points != null ? Number(s.points) : NaN;
  if (Number.isFinite(pts)) return pts;
  const lab = String(s.label || "");
  const m = lab.match(/(?:over|under)\s+([\d.]+)/i);
  return m ? Number(m[1]) : NaN;
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
      if (ot === "over" || lab === "over" || /^over\b/i.test(lab)) overSel = s;
      else if (ot === "under" || lab === "under" || /^under\b/i.test(lab)) underSel = s;
    }
    if (!overSel || !underSel) continue;
    const lo = lineFromSelection(overSel);
    const lu = lineFromSelection(underSel);
    const lineRaw = Number.isFinite(lo) ? lo : lu;
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
  const requestedLeagueId = String(opts.leagueId || LEAGUE_ID || "").trim();
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

  const nav = await page.evaluate((lidRaw) => {
    const ini = window.__INITIAL_STATE__;
    if (!ini)
      return { seoMap: {}, roundScoreSubs: [], detectedLeagueId: "", allSubIdsForLeague: [] };
    const requested = String(lidRaw || "").trim();
    const seoToStat = {
      "birdies-or-better": "Birdies",
      pars: "Pars",
      "bogeys-or-worse": "Bogeys",
      "greens-in-regulation": "GIR",
      "fairways-hit": "Fairways hit",
      "total-putts": "Putts",
      putts: "Putts",
    };
    const titleToStat = [
      [/birdies?\s+or\s+better/i, "Birdies"],
      [/\bpars?\b/i, "Pars"],
      [/bogeys?\s+or\s+worse/i, "Bogeys"],
      [/greens?\s+in\s+regulation|\bgir\b/i, "GIR"],
      [/fairways?\s+hit/i, "Fairways hit"],
      [/(?:total\s+)?putts?/i, "Putts"],
    ];
    const bySeo = {};
    const roundScoreSubs = new Set();
    const leagueRows = [];
    function walk(o, depth) {
      if (!o || typeof o !== "object" || depth > 45) return;
      const p = o.parameters;
      if (p && p.subcategoryId != null && p.leagueId != null) {
        const leagueId = String(p.leagueId);
        let seo = String(o.seoId || "").trim().toLowerCase();
        if (seo === "bogies-or-worse") seo = "bogeys-or-worse";
        const title = String(o.title || "").trim();
        let stat = seoToStat[seo] || null;
        if (!stat) {
          const t = title.toLowerCase();
          for (const [re, s] of titleToStat) {
            if (re.test(t)) {
              stat = s;
              break;
            }
          }
        }
        leagueRows.push({ leagueId, subcategoryId: String(p.subcategoryId), seo, title, stat });
        const titleLc = title.toLowerCase();
        if (titleLc === "round score" || /round[-_]?score/.test(seo)) {
          roundScoreSubs.add(`${leagueId}|||${String(p.subcategoryId)}`);
        }
      }
      if (Array.isArray(o)) {
        for (const x of o) walk(x, depth + 1);
        return;
      }
      for (const k of Object.keys(o)) walk(o[k], depth + 1);
    }
    walk(ini, 0);
    const counts = new Map();
    for (const r of leagueRows) {
      counts.set(r.leagueId, (counts.get(r.leagueId) || 0) + (r.stat ? 3 : 1));
    }
    let detectedLeagueId = "";
    if (requested) detectedLeagueId = requested;
    else {
      let best = -1;
      for (const [k, c] of counts.entries()) {
        if (c > best) {
          best = c;
          detectedLeagueId = k;
        }
      }
    }
    for (const r of leagueRows) {
      if (detectedLeagueId && r.leagueId !== detectedLeagueId) continue;
      if (r.seo) bySeo[r.seo] = r.subcategoryId;
      if (r.stat) bySeo[`__stat__${r.stat}`] = r.subcategoryId;
    }
    const scoreSubs = [];
    for (const tag of roundScoreSubs) {
      const [lg, sub] = String(tag).split("|||");
      if (!detectedLeagueId || lg === detectedLeagueId) scoreSubs.push(sub);
    }
    const allSubs = new Set();
    for (const r of leagueRows) {
      if (detectedLeagueId && r.leagueId !== detectedLeagueId) continue;
      allSubs.add(r.subcategoryId);
    }
    return {
      seoMap: bySeo,
      roundScoreSubs: scoreSubs,
      detectedLeagueId,
      allSubIdsForLeague: [...allSubs].sort(),
    };
  }, requestedLeagueId);

  const leagueId = String(nav?.detectedLeagueId || requestedLeagueId || "").trim();
  if (!leagueId) {
    await browser.close();
    return { props: [], subcatsUsed: {}, error: "Could not detect DK league id from page (set DK_LEAGUE_ID)." };
  }

  const bySeo = nav.seoMap || {};
  let roundScoreSubs = [];
  const tsOv = overrides["Total Score"] ?? overrides.TotalScore;
  if (tsOv != null) {
    roundScoreSubs = Array.isArray(tsOv) ? tsOv.map(String) : [String(tsOv)];
  } else if (nav.roundScoreSubs?.length) {
    roundScoreSubs = [...nav.roundScoreSubs];
  } else {
    roundScoreSubs = [...FALLBACK_ROUND_SCORE_SUBS];
  }
  roundScoreSubs = [...new Set(roundScoreSubs.map(String).filter(Boolean))];

  const subcatsUsed = {};
  const statToSub = {};
  for (const [seo, stat] of Object.entries(STAT_BY_SEO)) {
    const fromNav = bySeo[seo] || bySeo[`__stat__${stat}`];
    const fromEnv = overrides[stat];
    const sub = fromEnv || fromNav || FALLBACK_SUBCAT_BY_STAT[stat];
    if (!sub) continue;
    statToSub[stat] = sub;
    subcatsUsed[stat] = sub;
  }

  const allLeagueSubIds = Array.isArray(nav.allSubIdsForLeague) ? nav.allSubIdsForLeague : [];
  const api = ctx.request;
  for (const st of ["Putts", "GIR", "Fairways hit", "Birdies", "Pars"]) {
    if (overrides[st]) continue;
    const pref = statToSub[st] || "";
    if (!pref && !(PROBE_SUBS_FIRST[st] || []).length && st !== "Fairways hit" && st !== "GIR") continue;
    const picked = await pickSubcategoryForStat(api, leagueId, siteSegment, st, pref, allLeagueSubIds, dgByNameLower);
    if (picked) {
      statToSub[st] = picked;
      subcatsUsed[st] = picked;
    } else if (statToSub[st]) {
      delete statToSub[st];
      delete subcatsUsed[st];
    }
  }

  if (Object.keys(statToSub).length === 0 && roundScoreSubs.length === 0) {
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
    const entries = Object.entries(statToSub);
    for (let i = 0; i < entries.length; i++) {
      const [stat, sub] = entries[i];
      const u = marketsUrl(leagueId, sub, siteSegment);
      const res = await api.get(u, { timeout: 60000 });
      if (!res.ok()) continue;
      const body = await res.json();
      all.push(...propsFromMarketsBody(body, stat, dgByNameLower));
      await page.waitForTimeout(250);
    }
    for (let i = 0; i < roundScoreSubs.length; i++) {
      const sub = roundScoreSubs[i];
      const u = marketsUrl(leagueId, sub, siteSegment);
      const res = await api.get(u, { timeout: 60000 });
      if (!res.ok()) continue;
      const body = await res.json();
      all.push(...propsFromMarketsBody(body, "Total Score", dgByNameLower));
      const prev = subcatsUsed["Total Score"];
      subcatsUsed["Total Score"] = prev ? `${prev},${sub}` : sub;
      if (i < roundScoreSubs.length - 1) await page.waitForTimeout(250);
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
