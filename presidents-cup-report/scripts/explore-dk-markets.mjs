import { chromium } from "../lib/playwright.mjs";

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
const captured = [];
page.on("response", async (resp) => {
  if (!resp.url().includes("sportsbook-nash") || resp.status() !== 200) return;
  try { captured.push(await resp.json()); } catch {}
});

await page.goto("https://sportsbook.draftkings.com/leagues/golf/presidents-cup", { waitUntil: "domcontentloaded", timeout: 60000 });
await page.waitForTimeout(8000);

const links = await page.evaluate(() =>
  [...document.querySelectorAll("a[href*='presidents-cup']")]
    .map((a) => a.getAttribute("href"))
    .filter((h) => h && h.includes("subcategory=")),
);
const uniq = [...new Set(links)];

for (const href of uniq) {
  const url = href.startsWith("http") ? href : `https://sportsbook.draftkings.com${href}`;
  console.log("visit", url.split("subcategory=")[1]);
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 60000 });
  await page.waitForTimeout(6000);
  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight));
  await page.waitForTimeout(2000);
}

await browser.close();

let props = parseNash(captured);
const seen = new Set();
props = props.filter((p) => {
  const k = `${p.marketId}|${p.selection}|${p.american}`;
  if (seen.has(k)) return false;
  seen.add(k);
  return true;
});

const byId = new Map();
for (const p of props) {
  if (!byId.has(p.marketId)) byId.set(p.marketId, { market: p.market, rows: [] });
  byId.get(p.marketId).rows.push(p);
}

for (const [id, v] of [...byId.entries()].sort((a, b) => a[0].localeCompare(b[0]))) {
  console.log(`\n[${id}] ${v.market} (${v.rows.length})`);
  console.log(v.rows.map((r) => `${r.selection} ${r.american}`).join(" | "));
}

console.log("\nTotal markets:", byId.size, "props:", props.length);
