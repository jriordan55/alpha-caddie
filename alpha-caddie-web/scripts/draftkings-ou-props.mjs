/**
 * Pull DraftKings round O/U props (Birdies, Pars, Bogeys, GIR, Fairways, Putts, Round Score) via
 * sportsbook-nash leagueSubcategory markets API, using Playwright for session cookies.
 *
 * CLI (`npm run fetch:dk-ou`): reads `projections.json` for players + league URL (event_name → slug, or dk_league_slug),
 * unless `DK_LEAGUE_URL` is set — same idea as fetch-book-odds.
 *
 * Env:
 *   GOLF_SKIP_DK_OU=1 — skip entirely
 *   DK_LEAGUE_URL — e.g. https://sportsbook.draftkings.com/leagues/golf/rbc-heritage?category=round
 *   DK_SITE_SEGMENT — default US-MA-SB (set to your state segment if requests fail)
 *   DK_LEAGUE_ID — optional explicit league id (auto-detected from page if omitted).
 *   DK_SUBCAT_JSON — optional override per stat (skips subcategory probe for keys you set).
 *   DK_OU_DEBUG_MARKETS=1 — extra selection key dump when a category returns markets but 0 parsed rows.
 *
 *   Putts / GIR / fairways / Birdies / Pars: nav ids often point at hole, “2 ball”, “player most”, or Par-N props;
 *   we probe league subs for titles like “… Putts - Round 1” / “… Birdies or Better - Round 1”.
 *
 * Note: "Total Score" can be absent on some events; fallback CSV still applies in fetch-book-odds-into-projections.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { chromium } from "playwright";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import { inferDraftKingsLeagueUrlFromProjections } from "./draftkings-league-url.mjs";

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
  "green-in-regulation": "GIR",
  "fairways-hit": "Fairways hit",
  "total-putts": "Putts",
  putts: "Putts",
};

/** Legacy fallback ids when nav state omits round stats (Memorial / most PGA events reuse these). */
const FALLBACK_SUBCAT_BY_STAT = {
  Birdies: "17299",
  Pars: "17300",
  Bogeys: "17301",
  GIR: "17302",
  "Fairways hit": "17303",
  Putts: "17304",
};

/**
 * DraftKings reuses subcategory ids across "2 Ball …" nav labels vs field round O/U.
 * Prefer subs whose market *names* look like "Player X Putts - Round 1", not group/hole props.
 */
const PROBE_SUBS_FIRST = {
  Putts: ["17304", "17399"],
  GIR: ["17302", "10154"],
  "Fairways hit": ["17303"],
  Pars: ["17300"],
  Birdies: ["17299"],
  Bogeys: ["17301"],
};

/** When nav omits Round Score tabs, try common PGA + US Open subcategory ids. */
const FALLBACK_ROUND_SCORE_SUBS = ["11015", "11786", "18987"];

/** U.S. Open @ Shinnecock (league 42731) — field round O/U subcategory ids (2026). */
const US_OPEN_LEAGUE_SUBCATS = {
  Birdies: ["19010", "17299"],
  Pars: ["19011", "17300"],
  Bogeys: ["19012", "17301"],
  "Total Score": ["11015", "11786", "18987"],
};

const PROBE_SUBS_ROUND_SCORE = ["11015", "11786", "18987"];

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

const ROUND_TAIL = String.raw`(?:Round\s+(\d+)|R(\d+)|Rd\.?\s*(\d+))(?:\s+O\/U)?\s*$`;

/** DK often uses en-dashes/em-dashes/spaces inconsistently — normalize before regex match. */
function normalizeMarketTitle(raw) {
  return String(raw || "")
    .replace(/\s+/g, " ")
    .replace(/[\u2013\u2014\u2212]/g, "-")
    .trim();
}

const SEP = String.raw`\s*[-–—]\s*`;

/** DK often embeds the printed line in the title, e.g. "Name o71.5 Round Score - Round 1". */
const OPTIONAL_TITLE_LINE = String.raw`(?:[oOuU]\s*\d+(?:\.\d+)?\s+)?`;

const NAME_RE = {
  Birdies: new RegExp(
    `^(.+?)\\s+${OPTIONAL_TITLE_LINE}Birdies or Better\\s+${SEP}${ROUND_TAIL}`,
    "i",
  ),
  Pars: new RegExp(`^(.+?)\\s+${OPTIONAL_TITLE_LINE}Pars\\s+${SEP}${ROUND_TAIL}`, "i"),
  Bogeys: new RegExp(
    `^(.+?)\\s+${OPTIONAL_TITLE_LINE}Bogeys or Worse\\s+${SEP}${ROUND_TAIL}`,
    "i",
  ),
  GIR: new RegExp(
    `^(.+?)\\s+${OPTIONAL_TITLE_LINE}(?:Greens?\\s+in\\s+Regulation|GIR)\\s+${SEP}${ROUND_TAIL}`,
    "i",
  ),
  "Fairways hit": new RegExp(
    `^(.+?)\\s+${OPTIONAL_TITLE_LINE}Fairways?\\s+Hit\\s+${SEP}${ROUND_TAIL}`,
    "i",
  ),
  Putts: new RegExp(
    `^(.+?)\\s+${OPTIONAL_TITLE_LINE}(?:Total\\s+)?Putts\\s+${SEP}${ROUND_TAIL}`,
    "i",
  ),
};

const NAME_RE_TOTAL_SCORE = new RegExp(
  `^(.+?)\\s+${OPTIONAL_TITLE_LINE}Round Score\\s+${SEP}${ROUND_TAIL}`,
  "i",
);

function roundFromMatch(m) {
  if (!m) return NaN;
  for (let i = 2; i < m.length; i++) {
    const n = Number(m[i]);
    if (Number.isFinite(n)) return n;
  }
  return NaN;
}

function parseMarketName(stat, marketName) {
  const raw = normalizeMarketTitle(marketName);
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

/** Click DK golf "Round N" tab so Nash API returns that round's O/U titles. */
async function selectDraftKingsRoundTab(page, roundNum) {
  const r = Math.round(Number(roundNum));
  if (!Number.isFinite(r) || r < 1 || r > 4) return false;
  try {
    const clicked = await page.evaluate((round) => {
      const re = new RegExp(`^(Round\\s+${round}|R${round})$`, "i");
      const nodes = document.querySelectorAll('button,a,[role="tab"],[role="button"]');
      for (const el of nodes) {
        const t = String(el.textContent || "").replace(/\s+/g, " ").trim();
        if (!t || t.length > 20) continue;
        if (re.test(t)) {
          el.click();
          return true;
        }
      }
      return false;
    }, r);
    if (clicked) {
      await page.waitForTimeout(Math.min(8000, Math.max(1500, Number(process.env.DK_ROUND_TAB_WAIT_MS || 2800))));
      console.log(`[draftkings-ou] selected Round ${r} tab on DK page`);
      return true;
    }
  } catch (e) {
    console.warn("[draftkings-ou] round tab:", e.message || e);
  }
  return false;
}

/** Click DK left-rail market tab (e.g. "Round Score") so Nash API authorizes that subcategory. */
async function selectDraftKingsMarketTab(page, labelPatterns) {
  const pats = (Array.isArray(labelPatterns) ? labelPatterns : [labelPatterns])
    .map((p) => String(p || "").trim())
    .filter(Boolean);
  if (!pats.length) return false;
  try {
    const label = await page.evaluate((patterns) => {
      const nodes = document.querySelectorAll('button,a,[role="tab"],[role="button"],[role="menuitem"]');
      for (const raw of patterns) {
        const re = new RegExp(raw, "i");
        for (const el of nodes) {
          const t = String(el.textContent || "").replace(/\s+/g, " ").trim();
          if (!t || t.length > 48) continue;
          if (re.test(t)) {
            el.click();
            return t;
          }
        }
      }
      return "";
    }, pats);
    if (label) {
      await page.waitForTimeout(Math.min(8000, Math.max(1500, Number(process.env.DK_MARKET_TAB_WAIT_MS || 2200))));
      console.log(`[draftkings-ou] selected market tab "${label}"`);
      return true;
    }
  } catch (e) {
    console.warn("[draftkings-ou] market tab:", e.message || e);
  }
  return false;
}

function preferPropsForTargetRound(props, targetRound) {
  const tr = Math.round(Number(targetRound));
  if (!Number.isFinite(tr) || tr < 1 || tr > 4) return props;
  const exact = props.filter((p) => Math.round(Number(p.round_num)) === tr);
  if (exact.length) return exact;
  const rounds = [...new Set(props.map((p) => Math.round(Number(p.round_num))).filter((n) => n >= 1 && n <= 4))].sort(
    (a, b) => b - a,
  );
  const pick = rounds.find((n) => n <= tr) ?? rounds[0];
  if (!Number.isFinite(pick)) return props;
  const fallback = props.filter((p) => Math.round(Number(p.round_num)) === pick);
  if (fallback.length) {
    console.warn(
      `[draftkings-ou] no Round ${tr} lines — using Round ${pick} (${fallback.length} props); check DK round tab or display_round`,
    );
  }
  return fallback.length ? fallback : props;
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

/** Quick check that a subcategory returns parseable per-player round O/U rows for `stat`. */
async function subcategoryYieldsStat(api, leagueId, siteSegment, stat, sub, players) {
  const u = marketsUrl(leagueId, sub, siteSegment);
  const res = await api.get(u, { timeout: 60000 });
  if (!res.ok()) return 0;
  try {
    const body = await res.json();
    const mk = Array.isArray(body?.markets) ? body.markets : [];
    if (!mk.some((m) => isGoodPlayerRoundSampleName(stat, m.name))) return 0;
    return propsFromMarketsBody(body, stat, players).length;
  } catch {
    return 0;
  }
}

/** All subcategory ids that return per-player round O/U rows for `stat` (merge every hit, not just the largest). */
async function findAllSubcategoriesForStat(
  api,
  leagueId,
  siteSegment,
  stat,
  preferredSub,
  navSubs,
  allLeagueSubIds,
  players,
) {
  const candidates = buildProbeOrder(stat, preferredSub, allLeagueSubIds);
  for (const id of navSubs || []) {
    const s = String(id || "").trim();
    if (s && !candidates.includes(s)) candidates.unshift(s);
  }
  const hits = [];
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
    const nParsed = propsFromMarketsBody(body, stat, players).length;
    if (nParsed > 0) hits.push({ sub, nParsed });
    await new Promise((r) => setTimeout(r, 45));
  }
  hits.sort((a, b) => b.nParsed - a.nParsed);
  return [...new Set(hits.map((h) => h.sub))];
}

function lineFromSelection(s) {
  const pts = s.points != null ? Number(s.points) : NaN;
  if (Number.isFinite(pts)) return pts;
  const lab = String(s.label || s.participantLabel || s.outcomeLabel || "");
  const m = lab.match(/(?:over|under)\s+([\d.]+)/i);
  return m ? Number(m[1]) : NaN;
}

function selectionMarketId(s) {
  const v =
    s.marketId ??
    s.marketID ??
    s.EventMarketId ??
    s.eventMarketId ??
    s.market_id ??
    s.parentMarketId;
  return v != null ? String(v) : "";
}

function americanFromSelection(s) {
  const d = s.displayOdds;
  if (d && d.american != null) return parseAmerican(d.american);
  if (s.americanOdds != null) return parseAmerican(s.americanOdds);
  if (s.trueOdds?.american != null) return parseAmerican(s.trueOdds.american);
  if (s.odds?.american != null) return parseAmerican(s.odds.american);
  return NaN;
}

function flattenSelectionsFromBody(body) {
  const top = Array.isArray(body?.selections) ? body.selections : [];
  const out = [...top];
  for (const mk of body?.markets || []) {
    if (Array.isArray(mk?.selections)) out.push(...mk.selections);
    if (Array.isArray(mk?.outcomes)) out.push(...mk.outcomes);
  }
  return out;
}

function logUnparsedSample(stat, body, reason) {
  const markets = body?.markets;
  const selections = body?.selections || [];
  if (!Array.isArray(markets) || !markets.length) return;
  const mk = markets[0];
  const mid = String(mk?.id ?? mk?.marketId ?? "");
  const related = selections.filter((s) => selectionMarketId(s) === mid);
  const name = normalizeMarketTitle(mk?.name).slice(0, 120);
  console.warn(
    `[draftkings-ou] ${reason} stat=${stat} nMarkets=${markets.length} first=${JSON.stringify(name)} mktId=${mid} selsForMkt=${related.length} totalSels=${selections.length}`,
  );
  if (process.env.DK_OU_DEBUG_MARKETS === "1" && (related[0] || selections[0])) {
    const one = related[0] || selections[0];
    console.warn(`[draftkings-ou] sel keys=${Object.keys(one).sort().join(",")}`);
  }
}

function propsFromMarketsBody(body, stat, players) {
  const markets = body?.markets;
  const selections = flattenSelectionsFromBody(body);
  if (!Array.isArray(markets) || !markets.length) return [];
  const byMarket = new Map();
  for (const s of selections) {
    const mid = selectionMarketId(s);
    if (!mid) continue;
    if (!byMarket.has(mid)) byMarket.set(mid, []);
    byMarket.get(mid).push(s);
  }
  const out = [];
  for (const mk of markets) {
    const mkId = String(mk.id ?? mk.marketId ?? mk.eventMarketId ?? "");
    const parsed = parseMarketName(stat, mk.name);
    if (!parsed) continue;
    let sel = byMarket.get(mkId) || [];
    if (!sel.length && mk.uuid) sel = byMarket.get(String(mk.uuid)) || [];
    if (!sel.length && mk.eventId) {
      sel = selections.filter((s) => String(s.eventId || s.eventID || "") === String(mk.eventId));
    }
    let overSel;
    let underSel;
    for (const s of sel) {
      const ot = String(s.outcomeType || s.type || "").toLowerCase();
      const labRaw = String(s.label || s.participantLabel || s.outcomeLabel || "");
      const lab = labRaw.toLowerCase();
      const overish =
        ot === "over" ||
        lab === "over" ||
        /^over\b/i.test(lab) ||
        /^\s*o\s*[\d.]+\b/i.test(lab) ||
        (ot.includes("over") && !ot.includes("under"));
      const underish =
        ot === "under" ||
        lab === "under" ||
        /^under\b/i.test(lab) ||
        /^\s*u\s*[\d.]+\b/i.test(lab) ||
        (ot.includes("under") && !ot.includes("over"));
      if (overish) overSel = s;
      else if (underish) underSel = s;
    }
    if (!overSel || !underSel) continue;
    const lo = lineFromSelection(overSel);
    const lu = lineFromSelection(underSel);
    const lineRaw = Number.isFinite(lo) ? lo : lu;
    if (!Number.isFinite(lineRaw)) continue;
    let line = lineRaw;
    if (stat !== "Total Score" && line === Math.floor(line)) line += 0.5;
    const over = americanFromSelection(overSel);
    const under = americanFromSelection(underSel);
    if (!Number.isFinite(over) || !Number.isFinite(under)) continue;
    const dkLabel = parsed.dkPlayer;
    const matched = matchPlayerByGolferLabel(players, dkLabel);
    const player_name = matched ? String(matched.player_name || "").trim() : dkLabel;
    const o = {
      player_name,
      line,
      over_odds: over,
      under_odds: under,
      market: stat,
      round_num: parsed.round,
    };
    if (matched) {
      const dg = Math.round(Number(matched.dg_id));
      if (Number.isFinite(dg) && dg > 0) o.dg_id = dg;
    }
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
  const targetRound = Math.round(
    Number(opts.targetRound ?? opts.displayRound ?? process.env.DK_TARGET_ROUND ?? NaN),
  );
  console.log(
    `[draftkings-ou] url=${leagueUrl} site=${siteSegment} players=${Array.isArray(players) ? players.length : 0}${Number.isFinite(targetRound) ? ` targetRound=R${targetRound}` : ""}`,
  );

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

  // Default Playwright UA — custom Chrome UA strings often get Nash API 403 on round props.
  const ctx = await browser.newContext({ viewport: { width: 1400, height: 900 } });
  const page = await ctx.newPage();
  try {
    await page.goto(leagueUrl, { waitUntil: "domcontentloaded", timeout: 90000 });
    await page
      .waitForFunction(() => typeof window !== "undefined" && window.__INITIAL_STATE__ != null, {
        timeout: 45000,
      })
      .catch(() => {});
    const extraMs = Math.min(
      30000,
      Math.max(2000, Number(process.env.DK_PAGE_WAIT_MS || 8000)),
    );
    await page.waitForTimeout(extraMs);
    if (Number.isFinite(targetRound)) await selectDraftKingsRoundTab(page, targetRound);
  } catch (e) {
    await browser.close();
    return { props: [], subcatsUsed: {}, error: `goto: ${e.message}` };
  }

  const nav = await page.evaluate((lidRaw) => {
    const ini = window.__INITIAL_STATE__;
    if (!ini)
      return {
        seoMap: {},
        subsByStat: {},
        roundScoreSubs: [],
        detectedLeagueId: "",
        allSubIdsForLeague: [],
      };
    const requested = String(lidRaw || "").trim();
    const seoToStat = {
      "birdies-or-better": "Birdies",
      pars: "Pars",
      "bogeys-or-worse": "Bogeys",
      "greens-in-regulation": "GIR",
      "green-in-regulation": "GIR",
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
    const subsByStat = {};
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
    const statCounts = new Map();
    const roundScoreLeagues = new Set();
    for (const r of leagueRows) {
      if (r.stat) statCounts.set(r.leagueId, (statCounts.get(r.leagueId) || 0) + 1);
      const titleLc = String(r.title || "").toLowerCase();
      if (titleLc === "round score" || String(r.seo || "").includes("round-score")) {
        roundScoreLeagues.add(r.leagueId);
      }
    }
    let detectedLeagueId = "";
    if (requested) detectedLeagueId = requested;
    else {
      let bestStat = -1;
      let bestRoundScore = -1;
      for (const [k, sc] of statCounts.entries()) {
        const rs = roundScoreLeagues.has(k) ? 1 : 0;
        if (sc > bestStat || (sc === bestStat && rs > bestRoundScore)) {
          bestStat = sc;
          bestRoundScore = rs;
          detectedLeagueId = k;
        }
      }
      if (!detectedLeagueId && roundScoreLeagues.size) {
        detectedLeagueId = [...roundScoreLeagues][0];
      }
    }
    for (const r of leagueRows) {
      if (detectedLeagueId && r.leagueId !== detectedLeagueId) continue;
      if (r.seo) bySeo[r.seo] = r.subcategoryId;
      if (r.stat) {
        bySeo[`__stat__${r.stat}`] = r.subcategoryId;
        if (!subsByStat[r.stat]) subsByStat[r.stat] = [];
        if (!subsByStat[r.stat].includes(r.subcategoryId)) subsByStat[r.stat].push(r.subcategoryId);
      }
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
      subsByStat,
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
  console.log(`[draftkings-ou] leagueId=${leagueId} navStats=${Object.keys(nav?.subsByStat || {}).join(",") || "(none)"}`);

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

  const subsByStatNav = nav.subsByStat || {};
  const subcatsUsed = {};
  const statToSubs = {};
  const addStatSubs = (stat, subOrList) => {
    const list = Array.isArray(subOrList) ? subOrList : subOrList ? [subOrList] : [];
    if (!list.length) return;
    if (!statToSubs[stat]) statToSubs[stat] = [];
    for (const id of list) {
      const s = String(id || "").trim();
      if (s && !statToSubs[stat].includes(s)) statToSubs[stat].push(s);
    }
  };
  for (const [seo, stat] of Object.entries(STAT_BY_SEO)) {
    const fromNav = bySeo[seo] || bySeo[`__stat__${stat}`];
    const fromEnv = overrides[stat];
    const navList = subsByStatNav[stat] || [];
    addStatSubs(stat, fromEnv || navList.length ? navList : fromNav ? [fromNav] : []);
    if (!statToSubs[stat]?.length && FALLBACK_SUBCAT_BY_STAT[stat]) {
      addStatSubs(stat, FALLBACK_SUBCAT_BY_STAT[stat]);
    }
  }

  const isUsOpenLeague =
    leagueId === "42731" || /\/us-open(?:\?|$)/i.test(String(leagueUrl || ""));
  if (isUsOpenLeague) {
    for (const [stat, subs] of Object.entries(US_OPEN_LEAGUE_SUBCATS)) {
      addStatSubs(stat, subs);
    }
  }

  const allLeagueSubIds = Array.isArray(nav.allSubIdsForLeague) ? nav.allSubIdsForLeague : [];
  const api = ctx.request;
  for (const st of ["Putts", "GIR", "Fairways hit", "Birdies", "Pars", "Bogeys"]) {
    if (overrides[st]) continue;
    const pref = statToSubs[st]?.[0] || "";
    const hasProbe = (PROBE_SUBS_FIRST[st] || []).length > 0 || FALLBACK_SUBCAT_BY_STAT[st];
    if (!pref && !hasProbe) continue;
    const picked = await findAllSubcategoriesForStat(
      api,
      leagueId,
      siteSegment,
      st,
      pref,
      statToSubs[st] || [],
      allLeagueSubIds,
      players,
    );
    if (picked.length) {
      statToSubs[st] = picked;
      subcatsUsed[st] = picked.join(",");
    } else {
      const trySubs = [
        ...new Set(
          [FALLBACK_SUBCAT_BY_STAT[st], ...(statToSubs[st] || []), ...(PROBE_SUBS_FIRST[st] || [])]
            .map((x) => String(x || "").trim())
            .filter(Boolean),
        ),
      ];
      const verified = [];
      for (const sub of trySubs) {
        const n = await subcategoryYieldsStat(api, leagueId, siteSegment, st, sub, players);
        if (n > 0) verified.push(sub);
        await new Promise((r) => setTimeout(r, 45));
      }
      if (verified.length) {
        statToSubs[st] = verified;
        subcatsUsed[st] = verified.join(",");
        console.log(`[draftkings-ou] ${st}: using fallback sub(s) ${verified.join(",")}`);
      } else {
        delete statToSubs[st];
        delete subcatsUsed[st];
      }
    }
  }

  if (tsOv == null && roundScoreSubs.length) {
    await selectDraftKingsMarketTab(page, ["^Round Score$", "Round Score"]);
    if (Number.isFinite(targetRound)) await selectDraftKingsRoundTab(page, targetRound);
    const probedScore = await findAllSubcategoriesForStat(
      api,
      leagueId,
      siteSegment,
      "Total Score",
      roundScoreSubs[0] || "",
      roundScoreSubs,
      [...new Set([...roundScoreSubs, ...PROBE_SUBS_ROUND_SCORE, ...allLeagueSubIds])],
      players,
    );
    if (probedScore.length) {
      roundScoreSubs = probedScore;
    } else {
      const verifiedScore = [];
      for (const sub of [
        ...new Set([...roundScoreSubs, ...PROBE_SUBS_ROUND_SCORE, ...allLeagueSubIds].map(String).filter(Boolean)),
      ].slice(0, 40)) {
        const n = await subcategoryYieldsStat(api, leagueId, siteSegment, "Total Score", sub, players);
        if (n > 0) verifiedScore.push(sub);
        await new Promise((r) => setTimeout(r, 45));
      }
      if (verifiedScore.length) {
        roundScoreSubs = verifiedScore;
        console.log(`[draftkings-ou] Total Score: verified sub(s) ${verifiedScore.join(",")}`);
      }
    }
  }

  if (Object.keys(statToSubs).length === 0 && roundScoreSubs.length === 0) {
    await browser.close();
    return {
      props: [],
      subcatsUsed: {},
      error: "Could not resolve DK subcategory ids (try DK_SUBCAT_JSON or DK_LEAGUE_URL)",
    };
  }

  const nAttempts =
    Object.values(statToSubs).reduce((n, subs) => n + (subs?.length || 0), 0) + roundScoreSubs.length;
  const all = [];
  let apiFail = 0;
  let apiBadShape = 0;
  try {
    const entries = Object.entries(statToSubs);
    for (let i = 0; i < entries.length; i++) {
      const [stat, subs] = entries[i];
      for (let j = 0; j < subs.length; j++) {
        const sub = subs[j];
        const u = marketsUrl(leagueId, sub, siteSegment);
        const res = await api.get(u, { timeout: 60000 });
        if (!res.ok()) {
          apiFail++;
          console.warn(`[draftkings-ou] markets HTTP ${res.status()} stat=${stat} sub=${sub}`);
          continue;
        }
        const body = await res.json();
        if (!Array.isArray(body?.markets)) apiBadShape++;
        const chunk = propsFromMarketsBody(body, stat, players);
        all.push(...chunk);
        if (!chunk.length && Array.isArray(body?.markets) && body.markets.length)
          logUnparsedSample(stat, body, "no-rows");
        if (j < subs.length - 1) await page.waitForTimeout(250);
      }
      if (i < entries.length - 1) await page.waitForTimeout(250);
    }
    await selectDraftKingsMarketTab(page, ["^Round Score$", "Round Score"]);
    if (Number.isFinite(targetRound)) await selectDraftKingsRoundTab(page, targetRound);

    for (let i = 0; i < roundScoreSubs.length; i++) {
      const sub = roundScoreSubs[i];
      const u = marketsUrl(leagueId, sub, siteSegment);
      let res = await api.get(u, { timeout: 60000 });
      if (!res.ok() && res.status() === 403) {
        await selectDraftKingsMarketTab(page, ["^Round Score$", "Round Score"]);
        if (Number.isFinite(targetRound)) await selectDraftKingsRoundTab(page, targetRound);
        res = await api.get(u, { timeout: 60000 });
      }
      if (!res.ok()) {
        apiFail++;
        console.warn(`[draftkings-ou] round-score markets HTTP ${res.status()} sub=${sub}`);
        continue;
      }
      const body = await res.json();
      if (!Array.isArray(body?.markets)) apiBadShape++;
      const chunkTs = propsFromMarketsBody(body, "Total Score", players);
      all.push(...chunkTs);
      if (!chunkTs.length && Array.isArray(body?.markets) && body.markets.length)
        logUnparsedSample("Total Score", body, "no-rows");
      const prev = subcatsUsed["Total Score"];
      subcatsUsed["Total Score"] = prev ? `${prev},${sub}` : sub;
      if (i < roundScoreSubs.length - 1) await page.waitForTimeout(250);
    }
  } finally {
    await browser.close();
  }

  const dedup = new Map();
  for (const r of all) {
    const rk = Number.isFinite(Number(r.round_num)) ? `|R${r.round_num}` : "";
    dedup.set(`${r.player_name}|${r.market}|${r.line}${rk}`, r);
  }
  let props = [...dedup.values()];
  props = preferPropsForTargetRound(props, Number.isFinite(targetRound) ? targetRound : NaN);
  if (!props.length && nAttempts > 0) {
    const hint =
      apiFail > 0
        ? `Nash API failures (${apiFail}); try DK_SITE_SEGMENT (e.g. US-VA-SB) or rerun.`
        : apiBadShape > 0
          ? "Markets JSON missing markets/selections arrays (DK shape change?)."
          : "Subcategories resolved but parsed 0 O/U rows (market titles vs regex, or empty category).";
    console.warn("[draftkings-ou]", hint);
    return {
      props,
      subcatsUsed,
      error: hint,
    };
  }
  return { props, subcatsUsed };
}

async function main() {
  const proj = join(__dirname, "..", "projections.json");
  let players = [];
  let opts = {};
  if (existsSync(proj)) {
    try {
      const payload = JSON.parse(readFileSync(proj, "utf8"));
      players = payload.players || [];
      const leagueUrl = inferDraftKingsLeagueUrlFromProjections(payload);
      if (leagueUrl) opts = { leagueUrl };
    } catch {
      /* ignore */
    }
  }
  const { props, subcatsUsed, error } = await fetchDraftKingsOuProps({ players, ...opts });
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
