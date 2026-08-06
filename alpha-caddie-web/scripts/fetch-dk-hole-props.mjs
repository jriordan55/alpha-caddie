/**
 * Pull DraftKings hole score + hole winner props via Nash leagueSubcategory markets API.
 *
 *   npm run fetch:dk-hole
 *
 * Env:
 *   GOLF_SKIP_DK_HOLE=1 — skip
 *   DK_LEAGUE_URL / DK_SITE_SEGMENT / DK_HEADLESS — same as draftkings-ou-props
 *   DK_HOLE_DEBUG=1 — log sample market titles when parse yields 0
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { chromium } from "playwright";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import { inferDraftKingsLeagueUrlFromProjections } from "./draftkings-league-url.mjs";
import { HOLE_PROP_MARKETS } from "./hole-props-model.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const OUT = join(WEB, "data", "dk_hole_props.json");

const DEFAULT_URL =
  process.env.DK_LEAGUE_URL?.trim() ||
  "https://sportsbook.draftkings.com/leagues/golf/wyndham-championship?category=hole";
const SITE = process.env.DK_SITE_SEGMENT?.trim() || "US-MA-SB";

function resolveDkHeadless() {
  const v = String(process.env.DK_HEADLESS ?? "").trim().toLowerCase();
  if (v === "0" || v === "false" || v === "no") return false;
  if (v === "1" || v === "true" || v === "yes") return true;
  return process.platform !== "win32" && process.platform !== "darwin";
}

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
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

function normalizeMarketTitle(raw) {
  return String(raw || "")
    .replace(/\s+/g, " ")
    .replace(/[\u2013\u2014\u2212]/g, "-")
    .trim();
}

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

async function waitForDkNashSession(page) {
  const ms = Math.min(45000, Math.max(8000, Number(process.env.DK_PAGE_WAIT_MS || 15000)));
  const ok = await page
    .waitForResponse(
      (r) => r.url().includes("sportsbook-nash.draftkings.com") && r.status() === 200,
      { timeout: ms },
    )
    .catch(() => null);
  if (!ok) await page.waitForTimeout(Math.min(12000, ms));
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

function lineFromSelection(s) {
  const pts = s.points != null ? Number(s.points) : NaN;
  if (Number.isFinite(pts)) return pts;
  const lab = String(s.label || s.participantLabel || s.outcomeLabel || "");
  const m = lab.match(/(?:over|under)\s+([\d.]+)/i);
  return m ? Number(m[1]) : NaN;
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

function extractHoleNumber(...parts) {
  for (const raw of parts) {
    const s = String(raw || "");
    const m =
      s.match(/\bhole\s*#?\s*(\d{1,2})\b/i) ||
      s.match(/\bon\s+(\d{1,2})\b/i) ||
      s.match(/\bh(\d{1,2})\b/i);
    if (m) {
      const h = Math.round(Number(m[1]));
      if (h >= 1 && h <= 18) return h;
    }
  }
  return NaN;
}

function extractRound(...parts) {
  for (const raw of parts) {
    const s = String(raw || "");
    const m = s.match(/\b(?:round|rd\.?|r)\s*(\d)\b/i);
    if (m) {
      const r = Math.round(Number(m[1]));
      if (r >= 1 && r <= 4) return r;
    }
  }
  return NaN;
}

function classifyHoleMarket(name, subHint = "") {
  const raw = normalizeMarketTitle(name);
  const low = raw.toLowerCase();
  const hint = String(subHint || "").toLowerCase();
  const hole = extractHoleNumber(raw);
  const round = extractRound(raw);

  // Subcategory accordion on DK: Hole Score | Hole Winner | Hole Matchup
  if (/matchup/.test(hint) || /\bmatchup\b/.test(low) || /\bvs\.?\b/.test(low) || /\bv\b/.test(low)) {
    if (!/score/.test(low) || /matchup/.test(hint) || /\bvs\.?\b/.test(low)) {
      return { kind: "matchup", hole, round, title: raw };
    }
  }

  if (
    /winner/.test(hint) ||
    /\bwinner\b/.test(low) ||
    /wins?\s+the\s+hole/.test(low) ||
    /lowest\s+score\s+on\s+hole/.test(low) ||
    /hole\s+winner/.test(low) ||
    (/2[- ]?ball/.test(low) && /\bhole\b/.test(low) && !/score/.test(low) && !/matchup/.test(low)) ||
    (/3[- ]?ball/.test(low) && /\bhole\b/.test(low) && !/score/.test(low))
  ) {
    return { kind: "winner", hole, round, title: raw };
  }

  if (
    /score/.test(hint) ||
    /hole\s+score/.test(low) ||
    /\bscore\s+on\s+hole\b/.test(low) ||
    /\bon\s+hole\s+\d/.test(low) ||
    /player\s+hole\s+score/.test(low) ||
    (Number.isFinite(hole) && /\bscore\b/.test(low))
  ) {
    return { kind: "score", hole, round, title: raw };
  }

  if (Number.isFinite(hole) && (/\b2[- ]?ball\b|\b3[- ]?ball\b|\b4[- ]?ball\b/.test(low) || /\bwinner\b/.test(low))) {
    return { kind: "winner", hole, round, title: raw };
  }
  // Under a known hole subcategory, accept remaining titled markets as that kind
  if (hint === "hole score" || hint === "score") return { kind: "score", hole, round, title: raw };
  if (hint === "hole winner" || hint === "winner") return { kind: "winner", hole, round, title: raw };
  if (hint === "hole matchup" || hint === "matchup") return { kind: "matchup", hole, round, title: raw };
  return null;
}

function playerNameFromScoreTitle(title) {
  const raw = normalizeMarketTitle(title);
  let m = raw.match(/^(.+?)\s+(?:hole\s+score|score\s+on\s+hole|on\s+hole)\b/i);
  if (m) return m[1].replace(/\s+/g, " ").trim();
  m = raw.match(/^(.+?)\s+o?\d+(?:\.\d+)?\s+.*hole/i);
  if (m) return m[1].replace(/\s+/g, " ").trim();
  return "";
}

/**
 * Parse one Nash markets body into normalized hole prop rows.
 * @param {object} body
 * @param {object[]} players
 * @param {number} targetRound
 * @param {string} [subHint] — "Hole Score" | "Hole Winner" | "Hole Matchup"
 */
export function holePropsFromMarketsBody(body, players = [], targetRound = NaN, subHint = "") {
  const markets = Array.isArray(body?.markets) ? body.markets : [];
  const selections = flattenSelectionsFromBody(body);
  if (!markets.length) return [];

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
    const cls = classifyHoleMarket(mk.name, subHint);
    if (!cls) continue;
    let hole = cls.hole;
    if (!Number.isFinite(hole)) hole = extractHoleNumber(mk.name, mk.subtitle, mk.marketType?.name);
    let round = cls.round;
    if (!Number.isFinite(round)) round = extractRound(mk.name, mk.subtitle);
    if (!Number.isFinite(round) && Number.isFinite(targetRound)) round = targetRound;

    let sel = byMarket.get(mkId) || [];
    if (!sel.length && mk.uuid) sel = byMarket.get(String(mk.uuid)) || [];

    if (cls.kind === "score") {
      let overSel;
      let underSel;
      /** @type {object[]} */
      const exact = [];
      for (const s of sel) {
        const ot = String(s.outcomeType || s.type || "").toLowerCase();
        const labRaw = String(s.label || s.participantLabel || s.outcomeLabel || "");
        const lab = labRaw.toLowerCase();
        const am = americanFromSelection(s);
        if (!Number.isFinite(am)) continue;
        if (ot === "over" || lab === "over" || /^over\b/i.test(lab) || /^\s*o\s*[\d.]+\b/i.test(lab)) {
          overSel = s;
        } else if (
          ot === "under" ||
          lab === "under" ||
          /^under\b/i.test(lab) ||
          /^\s*u\s*[\d.]+\b/i.test(lab)
        ) {
          underSel = s;
        } else if (labRaw.match(/\b([1-9]|1[0-2])\b/) || /birdie|eagle|par|bogey/i.test(lab)) {
          exact.push({ label: labRaw, american: am, outcomeType: ot });
        }
      }

      const dkLabel = playerNameFromScoreTitle(mk.name);
      const matched = dkLabel ? matchPlayerByGolferLabel(players, dkLabel) : null;
      const player_name = matched ? String(matched.player_name || "").trim() : dkLabel;
      if (!player_name) continue;

      const row = {
        source: "draftkings",
        market: HOLE_PROP_MARKETS.HOLE_SCORE,
        player_name,
        hole: Number.isFinite(hole) ? hole : null,
        round_num: Number.isFinite(round) ? round : null,
      };
      if (matched) {
        const dg = Math.round(Number(matched.dg_id));
        if (Number.isFinite(dg) && dg > 0) row.dg_id = dg;
      }

      if (overSel && underSel) {
        const lo = lineFromSelection(overSel);
        const lu = lineFromSelection(underSel);
        const lineRaw = Number.isFinite(lo) ? lo : lu;
        if (!Number.isFinite(lineRaw)) continue;
        row.line = lineRaw;
        row.over_odds = americanFromSelection(overSel);
        row.under_odds = americanFromSelection(underSel);
        if (!Number.isFinite(row.over_odds) || !Number.isFinite(row.under_odds)) continue;
      } else if (exact.length) {
        row.score_outcomes = exact;
      } else {
        continue;
      }
      out.push(row);
      continue;
    }

    // Winner or Matchup: each selection is a player moneyline
    /** @type {object[]} */
    const group = [];
    for (const s of sel) {
      const lab = String(
        s.label || s.participantLabel || s.outcomeLabel || s.participants?.[0]?.name || "",
      ).trim();
      if (!lab || /tie|push|draw/i.test(lab)) continue;
      const am = americanFromSelection(s);
      if (!Number.isFinite(am)) continue;
      const matched = matchPlayerByGolferLabel(players, lab);
      const entry = {
        player_name: matched ? String(matched.player_name || "").trim() : lab,
        american: am,
      };
      if (matched) {
        const dg = Math.round(Number(matched.dg_id));
        if (Number.isFinite(dg) && dg > 0) entry.dg_id = dg;
      }
      group.push(entry);
    }
    if (group.length < 2) continue;
    const isMatchup =
      cls.kind === "matchup" || group.length === 2 || /matchup/i.test(String(subHint || ""));
    const market = isMatchup ? HOLE_PROP_MARKETS.HOLE_MATCHUP : HOLE_PROP_MARKETS.HOLE_WINNER;
    const groupId = group
      .map((g) => g.dg_id || g.player_name)
      .sort()
      .join("|");
    for (const g of group) {
      out.push({
        source: "draftkings",
        market,
        player_name: g.player_name,
        dg_id: g.dg_id,
        hole: Number.isFinite(hole) ? hole : null,
        round_num: Number.isFinite(round) ? round : null,
        american: g.american,
        group_id: groupId,
        group: group.map((x) => ({
          dg_id: x.dg_id,
          player_name: x.player_name,
          american: x.american,
        })),
      });
    }
  }
  return out;
}

async function fetchMarketsJson(page, api, url) {
  try {
    const res = await api.get(url);
    const status = res.status();
    let body = null;
    try {
      body = await res.json();
    } catch {
      body = null;
    }
    return { ok: status >= 200 && status < 300, status, body };
  } catch (e) {
    return { ok: false, status: 0, body: null, error: e?.message || String(e) };
  }
}

const HOLE_SUB_TITLES = [
  { key: "score", re: /^hole\s*score$/i, hint: "Hole Score" },
  { key: "winner", re: /^hole\s*winner$/i, hint: "Hole Winner" },
  { key: "matchup", re: /^hole\s*matchup$/i, hint: "Hole Matchup" },
];

function classifySubcatRow(seo, title) {
  const seoLc = String(seo || "").toLowerCase();
  const titleLc = String(title || "").trim().toLowerCase();
  for (const t of HOLE_SUB_TITLES) {
    if (t.re.test(titleLc) || seoLc.includes(t.key) && seoLc.includes("hole")) {
      // Prefer exact title matches
      if (t.re.test(titleLc)) return t;
      if (seoLc === `hole-${t.key}` || seoLc.includes(`hole-${t.key}`) || seoLc.includes(`hole_${t.key}`)) {
        return t;
      }
    }
  }
  if (/^hole\s*score$/i.test(titleLc) || /hole-score/.test(seoLc)) return HOLE_SUB_TITLES[0];
  if (/^hole\s*winner$/i.test(titleLc) || /hole-winner/.test(seoLc)) return HOLE_SUB_TITLES[1];
  if (/^hole\s*matchup$/i.test(titleLc) || /hole-matchup/.test(seoLc)) return HOLE_SUB_TITLES[2];
  return null;
}

function discoverHoleSubcats(ini, leagueIdWanted) {
  /** @type {{ leagueId: string, subcategoryId: string, seo: string, title: string, hint: string, priority: number }[]} */
  const rows = [];
  function walk(o, depth) {
    if (!o || typeof o !== "object" || depth > 45) return;
    const p = o.parameters;
    if (p && p.subcategoryId != null && p.leagueId != null) {
      const leagueId = String(p.leagueId);
      const seo = String(o.seoId || "").trim().toLowerCase();
      const title = String(o.title || "").trim();
      const titleLc = title.toLowerCase();
      const typed = classifySubcatRow(seo, title);
      const isHole =
        Boolean(typed) ||
        seo.includes("hole") ||
        titleLc === "hole" ||
        /\bhole\b/.test(titleLc) ||
        /2-ball-hole|3-ball-hole|hole-score|hole-winner|hole-matchup/.test(seo);
      if (isHole) {
        rows.push({
          leagueId,
          subcategoryId: String(p.subcategoryId),
          seo,
          title,
          hint: typed?.hint || (titleLc.includes("score")
            ? "Hole Score"
            : titleLc.includes("winner")
              ? "Hole Winner"
              : titleLc.includes("matchup")
                ? "Hole Matchup"
                : "Hole"),
          priority: typed ? 0 : 5,
        });
      }
    }
    if (Array.isArray(o)) {
      for (const x of o) walk(x, depth + 1);
      return;
    }
    for (const k of Object.keys(o)) walk(o[k], depth + 1);
  }
  walk(ini, 0);

  const want = String(leagueIdWanted || "").trim();
  let filtered = want ? rows.filter((r) => r.leagueId === want) : rows;
  if (!filtered.length) filtered = rows;

  const byLg = new Map();
  for (const r of filtered) {
    byLg.set(r.leagueId, (byLg.get(r.leagueId) || 0) + 1);
  }
  let bestLg = want;
  let bestN = -1;
  for (const [lg, n] of byLg) {
    if (n > bestN) {
      bestN = n;
      bestLg = lg;
    }
  }
  const forLg = filtered
    .filter((r) => r.leagueId === bestLg)
    .sort((a, b) => a.priority - b.priority || a.title.localeCompare(b.title));

  // Prefer exact Hole Score / Winner / Matchup first; keep other hole subs after
  const preferred = [];
  const seen = new Set();
  for (const wantHint of ["Hole Score", "Hole Winner", "Hole Matchup"]) {
    for (const r of forLg) {
      if (r.hint === wantHint && !seen.has(r.subcategoryId)) {
        preferred.push(r);
        seen.add(r.subcategoryId);
      }
    }
  }
  for (const r of forLg) {
    if (!seen.has(r.subcategoryId)) {
      preferred.push(r);
      seen.add(r.subcategoryId);
    }
  }

  return {
    leagueId: bestLg || want,
    subs: preferred.map((r) => ({ id: r.subcategoryId, hint: r.hint, title: r.title, seo: r.seo })),
    rows: preferred,
  };
}

async function clickHoleAccordion(page, labels) {
  const pats = (Array.isArray(labels) ? labels : [labels]).map((s) => String(s || "").trim()).filter(Boolean);
  if (!pats.length) return false;
  try {
    const hit = await page.evaluate((patterns) => {
      const nodes = document.querySelectorAll(
        'button,a,[role="button"],[role="tab"],[class*="accordion"],[class*="Expandable"],summary,div[tabindex]',
      );
      for (const el of nodes) {
        const t = String(el.textContent || "").replace(/\s+/g, " ").trim();
        if (!t || t.length > 40) continue;
        for (const p of patterns) {
          const re = new RegExp(`^${p.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`, "i");
          if (re.test(t) || t.toLowerCase() === p.toLowerCase()) {
            el.click();
            return t;
          }
        }
      }
      return "";
    }, pats);
    if (hit) {
      await page.waitForTimeout(Math.min(5000, Math.max(1200, Number(process.env.DK_HOLE_TAB_WAIT_MS || 2200))));
      console.log(`[draftkings-hole] clicked accordion "${hit}"`);
      return true;
    }
  } catch (e) {
    console.warn("[draftkings-hole] accordion click:", e?.message || e);
  }
  return false;
}

/**
 * @param {{ players?: object[], leagueUrl?: string, siteSegment?: string, headless?: boolean, targetRound?: number }} [opts]
 */
export async function fetchDraftKingsHoleProps(opts = {}) {
  if (process.env.GOLF_SKIP_DK_HOLE === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_DK_HOLE=1)" };
  }
  const preferred = opts.headless ?? resolveDkHeadless();
  const attempts = [preferred];
  if (preferred) attempts.push(false);
  let last = { props: [], error: "no attempt" };
  for (let i = 0; i < attempts.length; i++) {
    const headless = attempts[i];
    last = await fetchDraftKingsHolePropsOnce({ ...opts, headless });
    if (last.props.length > 0) return last;
    const blocked = /403|Nash API failures/i.test(String(last.error || ""));
    if (!blocked || !headless || i === attempts.length - 1) return last;
    console.warn("[draftkings-hole] headless blocked — retrying headed (DK_HEADLESS=0)");
  }
  return last;
}

async function fetchDraftKingsHolePropsOnce(opts = {}) {
  const players = Array.isArray(opts.players) ? opts.players : [];
  const leagueUrl = String(opts.leagueUrl || DEFAULT_URL).trim() || DEFAULT_URL;
  const siteSegment = String(opts.siteSegment || SITE).trim() || SITE;
  const targetRound = Math.round(num(opts.targetRound, NaN));
  const headless = opts.headless ?? resolveDkHeadless();

  console.log(
    `[draftkings-hole] url=${leagueUrl} site=${siteSegment} headless=${headless} players=${players.length}`,
  );

  const browser = await chromium.launch({
    headless,
    args: headless ? ["--disable-blink-features=AutomationControlled"] : undefined,
  });
  const ctx = await browser.newContext({ viewport: { width: 1400, height: 900 }, locale: "en-US" });
  await ctx.addInitScript(() => {
    Object.defineProperty(navigator, "webdriver", { get: () => false });
  });
  const page = await ctx.newPage();
  const api = ctx.request;

  /** @type {object[]} */
  let all = [];
  let apiFail = 0;
  /** @type {{ id: string, hint: string, title: string, seo: string }[]} */
  let holeSubs = [];
  let leagueId = String(opts.leagueId || process.env.DK_LEAGUE_ID || "").trim();
  /** @type {object[]} */
  const nashCapture = [];

  page.on("response", async (res) => {
    const u = res.url();
    if (!u.includes("sportsbook-nash") || !u.includes("/markets") || res.status() !== 200) return;
    try {
      nashCapture.push({ url: u, body: await res.json() });
    } catch {
      /* ignore */
    }
  });

  try {
    const holeUrl = leagueUrl.includes("category=")
      ? leagueUrl.replace(/category=[^&]+/i, "category=hole")
      : `${leagueUrl}${leagueUrl.includes("?") ? "&" : "?"}category=hole`;

    await page.goto(holeUrl, { waitUntil: "domcontentloaded", timeout: 90000 });
    await page
      .waitForFunction(() => typeof window !== "undefined" && window.__INITIAL_STATE__ != null, {
        timeout: 45000,
      })
      .catch(() => {});
    await waitForDkNashSession(page);

    // Click top-level HOLE category tab
    try {
      await page.evaluate(() => {
        const nodes = document.querySelectorAll('button,a,[role="tab"],[role="button"]');
        for (const el of nodes) {
          const t = String(el.textContent || "").replace(/\s+/g, " ").trim();
          if (/^hole$/i.test(t) || /^holes$/i.test(t)) {
            el.click();
            return true;
          }
        }
        return false;
      });
      await page.waitForTimeout(2500);
    } catch {
      /* ignore */
    }

    if (Number.isFinite(targetRound)) {
      try {
        await page.evaluate((round) => {
          const re = new RegExp(`^(Round\\s+${round}|R${round})$`, "i");
          const nodes = document.querySelectorAll('button,a,[role="tab"],[role="button"]');
          for (const el of nodes) {
            const t = String(el.textContent || "").replace(/\s+/g, " ").trim();
            if (re.test(t)) {
              el.click();
              return true;
            }
          }
          return false;
        }, targetRound);
        await page.waitForTimeout(2000);
      } catch {
        /* ignore */
      }
    }

    // Expand Hole Score / Winner / Matchup accordions (as on DK HOLE page)
    for (const label of ["Hole Score", "Hole Winner", "Hole Matchup"]) {
      const before = nashCapture.length;
      await clickHoleAccordion(page, [label]);
      if (nashCapture.length > before) {
        const body = nashCapture[nashCapture.length - 1]?.body;
        const chunk = holePropsFromMarketsBody(body, players, targetRound, label);
        if (chunk.length) {
          all.push(...chunk);
          console.log(`[draftkings-hole] ${label}: ${chunk.length} row(s) via accordion capture`);
        }
      }
    }

    const nav = await page.evaluate((lidRaw) => {
      const ini = window.__INITIAL_STATE__;
      return { ini, lid: String(lidRaw || "") };
    }, leagueId);

    const disc = discoverHoleSubcats(nav.ini, leagueId || nav.lid);
    leagueId = disc.leagueId || leagueId;
    holeSubs = disc.subs.slice(0, 48);
    console.log(
      `[draftkings-hole] leagueId=${leagueId || "?"} hole subcats=${holeSubs.length} sample=${holeSubs
        .slice(0, 6)
        .map((r) => `${r.title || r.hint}/${r.seo}/${r.id}`)
        .join(" · ")}`,
    );

    for (let i = 0; i < holeSubs.length; i++) {
      const sub = holeSubs[i];
      if (!leagueId || !sub?.id) continue;
      const u = marketsUrl(leagueId, sub.id, siteSegment);
      const res = await fetchMarketsJson(page, api, u);
      if (!res.ok) {
        apiFail++;
        continue;
      }
      const chunk = holePropsFromMarketsBody(res.body, players, targetRound, sub.hint || sub.title);
      all.push(...chunk);
      if (chunk.length) {
        console.log(`[draftkings-hole] sub ${sub.hint || sub.title} (${sub.id}): ${chunk.length} row(s)`);
      } else if (
        process.env.DK_HOLE_DEBUG === "1" &&
        Array.isArray(res.body?.markets) &&
        res.body.markets.length
      ) {
        const names = res.body.markets.slice(0, 8).map((m) => normalizeMarketTitle(m.name));
        console.warn(`[draftkings-hole] unparsed sub=${sub.id} hint=${sub.hint}`, names);
      }
      if (i < holeSubs.length - 1) await page.waitForTimeout(100);
    }
  } finally {
    await browser.close();
  }

  const dedup = new Map();
  for (const r of all) {
    const key =
      r.market === HOLE_PROP_MARKETS.HOLE_WINNER || r.market === HOLE_PROP_MARKETS.HOLE_MATCHUP
        ? `${r.market}|${r.group_id}|${r.dg_id || r.player_name}|${r.hole}|${r.round_num}`
        : `${r.market}|${r.dg_id || r.player_name}|${r.hole}|${r.round_num}|${r.line ?? "x"}`;
    if (!dedup.has(key)) dedup.set(key, r);
  }
  let props = [...dedup.values()];
  if (Number.isFinite(targetRound)) {
    const exact = props.filter((p) => Math.round(num(p.round_num, NaN)) === targetRound);
    if (exact.length) props = exact;
  }

  const byMkt = {};
  for (const p of props) byMkt[p.market] = (byMkt[p.market] || 0) + 1;

  if (!props.length) {
    const hint =
      apiFail > 0
        ? `Nash API failures (${apiFail}); try DK_SITE_SEGMENT or headed browser`
        : holeSubs.length === 0
          ? "No Hole Score / Winner / Matchup subcategories found on DK league page"
          : "Hole subs found but 0 parseable hole score/winner/matchup rows (set DK_HOLE_DEBUG=1)";
    console.warn("[draftkings-hole]", hint);
    return { props, error: hint, leagueId, subcats: holeSubs, byMarket: byMkt };
  }
  console.log(`[draftkings-hole] ${props.length} prop row(s)`, byMkt);
  return { props, error: null, leagueId, subcats: holeSubs, byMarket: byMkt };
}

async function main() {
  const proj = join(WEB, "projections.json");
  let players = [];
  let leagueUrl = "";
  let targetRound = NaN;
  if (existsSync(proj)) {
    try {
      const payload = JSON.parse(readFileSync(proj, "utf8"));
      players = payload.players || [];
      leagueUrl = inferDraftKingsLeagueUrlFromProjections(payload) || "";
      targetRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN));
    } catch {
      /* ignore */
    }
  }
  const { props, error } = await fetchDraftKingsHoleProps({
    players,
    leagueUrl: leagueUrl || undefined,
    targetRound,
  });
  mkdirSync(dirname(OUT), { recursive: true });
  writeFileSync(
    OUT,
    `${JSON.stringify(
      {
        generated_at: new Date().toISOString(),
        source: "draftkings",
        error: error || null,
        n: props.length,
        props,
      },
      null,
      2,
    )}\n`,
  );
  console.log(`[draftkings-hole] wrote ${OUT} (${props.length})`);
  if (props[0]) console.log("sample", props[0]);
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
