/**
 * Dev-only: count stroke O/U markets in the full-round band (~66–82) vs nine-hole (~30–46).
 * When `ouMarkets_line_66_82` > 0, draftkings-ou-props.mjs can pull Round Score → Total Score.
 *
 *   node scripts/probe-dk-round-score.mjs
 */
import { chromium } from "playwright";

const URL =
  process.env.DK_LEAGUE_URL?.trim() ||
  "https://sportsbook.draftkings.com/leagues/golf/us-masters?category=round";
const SITE = process.env.DK_SITE_SEGMENT?.trim() || "US-MA-SB";
const LEAGUE = process.env.DK_LEAGUE_ID?.trim() || "92694";

function marketsUrl(sub) {
  const templateVars = `${LEAGUE}%2C${sub}`;
  const eventsQuery = encodeURIComponent(
    `$filter=leagueId eq '${LEAGUE}' AND clientMetadata/Subcategories/any(s: s/Id eq '${sub}')`,
  );
  const marketsQuery = encodeURIComponent(
    `$filter=clientMetadata/subCategoryId eq '${sub}' AND tags/all(t: t ne 'SportcastBetBuilder')`,
  );
  return `https://sportsbook-nash.draftkings.com/sites/${SITE}/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=${templateVars}&eventsQuery=${eventsQuery}&marketsQuery=${marketsQuery}&include=Events&entity=events`;
}

function lineFromSel(x) {
  const pts = x.points != null ? Number(x.points) : NaN;
  if (Number.isFinite(pts)) return pts;
  const lab = String(x.label || "");
  const m = lab.match(/(?:over|under)\s+([\d.]+)/i);
  return m ? Number(m[1]) : NaN;
}

function countOuInBand(body, lo, hi) {
  const markets = body?.markets || [];
  const selections = body?.selections || [];
  const byMid = new Map();
  for (const s of selections) {
    const mid = String(s.marketId || "");
    if (!byMid.has(mid)) byMid.set(mid, []);
    byMid.get(mid).push(s);
  }
  let n = 0;
  for (const m of markets) {
    const sel = byMid.get(String(m.id)) || [];
    let overL;
    let underL;
    for (const x of sel) {
      const ot = String(x.outcomeType || "").toLowerCase();
      const lab = String(x.label || "").toLowerCase();
      const L = lineFromSel(x);
      if (!Number.isFinite(L)) continue;
      if (ot === "over" || lab === "over" || /^over\b/i.test(lab)) overL = L;
      if (ot === "under" || lab === "under" || /^under\b/i.test(lab)) underL = L;
    }
    if (!Number.isFinite(overL) || !Number.isFinite(underL)) continue;
    if (Math.abs(overL - underL) > 0.01) continue;
    if (overL >= lo && overL <= hi) n++;
  }
  return n;
}

(async () => {
  const browser = await chromium.launch({ headless: true });
  const ctx = await browser.newContext({
    userAgent:
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  });
  const page = await ctx.newPage();
  await page.goto(URL, { waitUntil: "domcontentloaded", timeout: 90000 });
  await page.waitForTimeout(9000);

  const subs = await page.evaluate((lid) => {
    const ini = window.__INITIAL_STATE__;
    if (!ini) return [];
    const lg = String(lid);
    const set = new Set();
    function walk(o, depth) {
      if (!o || typeof o !== "object" || depth > 45) return;
      const p = o.parameters;
      if (p && String(p.leagueId) === lg && p.subcategoryId != null) set.add(String(p.subcategoryId));
      if (Array.isArray(o)) {
        for (const x of o) walk(x, depth + 1);
        return;
      }
      for (const k of Object.keys(o)) walk(o[k], depth + 1);
    }
    walk(ini, 0);
    return [...set];
  }, LEAGUE);

  let fullRound = 0;
  let nine = 0;
  const api = ctx.request;
  for (const sub of subs) {
    const res = await api.get(marketsUrl(sub), { timeout: 60000 });
    if (!res.ok()) continue;
    const body = await res.json();
    fullRound += countOuInBand(body, 66, 82);
    nine += countOuInBand(body, 30, 46);
  }
  await browser.close();
  console.log(
    JSON.stringify(
      {
        subs: subs.length,
        ouMarkets_line_66_82: fullRound,
        ouMarkets_line_30_46_nineHoleBand: nine,
        note: fullRound === 0 ? "no full-round stroke O/U in this API surface" : "found — consider wiring draftkings-ou-props.mjs",
      },
      null,
      2,
    ),
  );
})();
