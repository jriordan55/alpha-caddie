import { chromium } from "../lib/playwright.mjs";

function parseNashBlob(blob, out = []) {
  if (Array.isArray(blob)) {
    for (const x of blob) parseNashBlob(x, out);
    return out;
  }
  if (!blob || typeof blob !== "object") return out;
  if (blob.markets && blob.selections) {
    const selByMarket = new Map();
    for (const s of blob.selections) {
      if (!selByMarket.has(s.marketId)) selByMarket.set(s.marketId, []);
      selByMarket.get(s.marketId).push(s);
    }
    for (const m of blob.markets) {
      for (const s of selByMarket.get(m.id) || []) {
        out.push({ market: m.name, marketId: m.id, selection: s.label, american: s.displayOdds?.american });
      }
    }
  }
  for (const v of Object.values(blob)) parseNashBlob(v, out);
  return out;
}

const browser = await chromium.launch({ headless: false });
const page = await browser.newPage();
const captured = [];
page.on("response", async (resp) => {
  if (!resp.url().includes("sportsbook-nash") || resp.status() !== 200) return;
  try { captured.push(await resp.json()); } catch {}
});
await page.goto("https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=event-props", { waitUntil: "domcontentloaded", timeout: 60000 });
await page.waitForTimeout(12000);
await browser.close();

const props = parseNashBlob(captured);
const byId = new Map();
for (const p of props) {
  if (!byId.has(p.marketId)) byId.set(p.marketId, { market: p.market, rows: [] });
  byId.get(p.marketId).rows.push(p);
}
for (const [id, v] of byId) {
  console.log(`[${id}] ${v.market}`);
  console.log(" ", v.rows.map((r) => `${r.selection} ${r.american}`).join(" | "));
}
