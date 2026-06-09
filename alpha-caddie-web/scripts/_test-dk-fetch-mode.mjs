import { chromium } from "playwright";

const URL =
  "https://sportsbook.draftkings.com/leagues/golf/rbc-canadian-open?category=round";
const SITE = "US-MA-SB";

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

async function run(label, ctxOpts) {
  const browser = await chromium.launch({ headless: true });
  const ctx = await browser.newContext(ctxOpts);
  const page = await ctx.newPage();
  await page.goto(URL, { waitUntil: "domcontentloaded", timeout: 90000 });
  await page.waitForTimeout(12000);
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
    for (const [k, c] of counts) if (c > n) { n = c; best = k; }
    return best;
  });
  const u = marketsUrl(lid, "11015");
  const apiStatus = await ctx.request.get(u, { timeout: 60000 }).then((r) => r.status()).catch(() => -1);
  const pageStatus = await page.evaluate(async (url) => {
    try {
      const r = await fetch(url, { credentials: "include" });
      const j = await r.json();
      return { status: r.status, markets: j?.markets?.length || 0, sample: j?.markets?.[0]?.name || "" };
    } catch (e) {
      return { status: -1, err: String(e) };
    }
  }, u);
  console.log(label, { apiStatus, pageStatus });
  await browser.close();
}

await run("default-ctx", {});
await run("custom-ua", {
  userAgent:
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  viewport: { width: 1400, height: 900 },
});
