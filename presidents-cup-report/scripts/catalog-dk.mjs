import { chromium } from "../lib/playwright.mjs";

const LEAGUE_HOME = "https://sportsbook.draftkings.com/leagues/golf/presidents-cup";

function parseNash(data, out = []) {
  if (Array.isArray(data)) {
    for (const x of data) parseNash(x, out);
    return out;
  }
  if (!data || typeof data !== "object") return out;
  if (data.markets && data.selections) {
    const selBy = new Map();
    for (const s of data.selections) {
      if (!selBy.has(s.marketId)) selBy.set(s.marketId, []);
      selBy.get(s.marketId).push(s);
    }
    for (const m of data.markets) {
      for (const s of selBy.get(m.id) || []) {
        out.push({
          marketId: m.id,
          market: m.name,
          selection: s.label,
          american: s.displayOdds?.american,
        });
      }
    }
  }
  for (const v of Object.values(data)) parseNash(v, out);
  return out;
}

const browser = await chromium.launch({ headless: false });
const page = await browser.newPage();
await page.goto(LEAGUE_HOME, { waitUntil: "domcontentloaded", timeout: 60000 });
await page.waitForTimeout(8000);

const links = await page.evaluate(() =>
  [...document.querySelectorAll("a[href*='presidents-cup']")]
    .map((a) => ({
      sub: (a.getAttribute("href")?.match(/subcategory=([^&]+)/) || [])[1] || "",
      text: (a.textContent || "").trim(),
      href: a.getAttribute("href"),
    }))
    .filter((x) => x.sub),
);
const uniq = [...new Map(links.map((l) => [l.sub, l])).values()];
console.log("Subcategories:", uniq.map((l) => l.text + " (" + l.sub + ")").join("\n  "));

const bySub = {};
for (const l of uniq) {
  const url = l.href.startsWith("http") ? l.href : `https://sportsbook.draftkings.com${l.href}`;
  const captured = [];
  page.removeAllListeners("response");
  page.on("response", async (resp) => {
    if (!resp.url().includes("sportsbook-nash") || resp.status() !== 200) return;
    try {
      captured.push(await resp.json());
    } catch {}
  });
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 60000 });
  await page.waitForTimeout(6000);
  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
  await page.waitForTimeout(2000);

  let props = [];
  for (const b of captured) props.push(...parseNash(b));
  const seen = new Set();
  props = props.filter((p) => {
    const k = `${p.marketId}|${p.selection}|${p.american}`;
    if (seen.has(k)) return false;
    seen.add(k);
    return true;
  });

  const byMarket = new Map();
  for (const p of props) {
    if (!byMarket.has(p.marketId)) byMarket.set(p.marketId, { name: p.market, rows: [] });
    byMarket.get(p.marketId).rows.push(p);
  }
  bySub[l.sub] = [...byMarket.entries()].map(([id, v]) => ({ id, name: v.name, n: v.rows.length }));
}

await browser.close();

for (const [sub, markets] of Object.entries(bySub)) {
  console.log(`\n=== ${sub} (${markets.length} markets) ===`);
  for (const m of markets) console.log(`  [${m.id}] ${m.name} (${m.n})`);
}
