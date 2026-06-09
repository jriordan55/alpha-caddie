import { chromium } from "playwright";

const URL =
  process.env.DK_LEAGUE_URL?.trim() ||
  "https://sportsbook.draftkings.com/leagues/golf/rbc-canadian-open?category=round";
const SITE = process.env.DK_SITE_SEGMENT?.trim() || "US-MA-SB";

function marketsUrl(leagueId, sub) {
  const templateVars = `${leagueId}%2C${sub}`;
  const eventsQuery = encodeURIComponent(
    `$filter=leagueId eq '${leagueId}' AND clientMetadata/Subcategories/any(s: s/Id eq '${sub}')`,
  );
  const marketsQuery = encodeURIComponent(
    `$filter=clientMetadata/subCategoryId eq '${sub}' AND tags/all(t: t ne 'SportcastBetBuilder')`,
  );
  return `https://sportsbook-nash.draftkings.com/sites/${SITE}/api/sportscontent/controldata/league/leagueSubcategory/v1/markets?isBatchable=false&templateVars=${templateVars}&eventsQuery=${eventsQuery}&marketsQuery=${marketsQuery}&include=Events&entity=events`;
}

const browser = await chromium.launch({ headless: true });
const ctx = await browser.newContext({
  userAgent:
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
});
const page = await ctx.newPage();
await page.goto(URL, { waitUntil: "domcontentloaded", timeout: 90000 });
await page.waitForTimeout(10000);

const lid = await page.evaluate(() => {
  const ini = window.__INITIAL_STATE__;
  const counts = new Map();
  function walk(o, d) {
    if (!o || typeof o !== "object" || d > 45) return;
    const p = o.parameters;
    if (p?.leagueId && p?.subcategoryId) {
      counts.set(String(p.leagueId), (counts.get(String(p.leagueId)) || 0) + 1);
    }
    if (Array.isArray(o)) o.forEach((x) => walk(x, d + 1));
    else Object.keys(o).forEach((k) => walk(o[k], d + 1));
  }
  walk(ini, 0);
  let best = "";
  let n = 0;
  for (const [k, c] of counts) {
    if (c > n) {
      n = c;
      best = k;
    }
  }
  return best;
});

const api = ctx.request;
for (const label of ["sequential-4"]) {
  for (const sub of ["11015", "19010", "19012", "11015"]) {
    const res = await api.get(marketsUrl(lid, sub), { timeout: 60000 });
    let n = 0;
    if (res.ok()) n = (await res.json()).markets?.length || 0;
    console.log(label, "sub", sub, "status", res.status(), "markets", n);
  }
}
await browser.close();
