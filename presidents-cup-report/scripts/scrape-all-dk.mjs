import { chromium } from "../lib/playwright.mjs";
import { writeFileSync } from "fs";

const URLS = [
  "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=player-props",
  "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=winner",
  "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=team-props",
  "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=event-props",
  "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=top-scorer",
  "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=popular",
  "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament&subcategory=outrights",
];

function parseNashBlob(blob) {
  const out = [];
  const markets = blob.markets || [];
  const selections = blob.selections || [];
  const selByMarket = new Map();
  for (const s of selections) {
    const mid = s.marketId;
    if (!selByMarket.has(mid)) selByMarket.set(mid, []);
    selByMarket.get(mid).push(s);
  }
  for (const m of markets) {
    const sels = selByMarket.get(m.id) || [];
    for (const s of sels) {
      const american = s.displayOdds?.american ?? s.oddsAmerican ?? null;
      out.push({
        market: m.name || m.marketType?.name || "Unknown",
        marketId: m.id,
        selection: s.label,
        american: parseAmerican(american),
        american_display: s.displayOdds?.american ?? null,
        raw: s,
      });
    }
  }
  return out;
}

function parseAmerican(raw) {
  const s = String(raw ?? "").trim();
  if (!s) return null;
  const neg = /^[-−–]/.test(s);
  const digits = s.replace(/[^0-9.]/g, "");
  const n = Number(digits);
  if (!Number.isFinite(n) || n === 0) return null;
  return neg ? -Math.abs(n) : Math.abs(n);
}

function walkNested(data, ctx = {}, out = []) {
  if (!data) return out;
  if (Array.isArray(data)) {
    for (const x of data) walkNested(x, ctx, out);
    return out;
  }
  if (typeof data !== "object") return out;
  const next = { ...ctx };
  if (data.subcategoryName || data.marketName) {
    next.market = data.subcategoryName || data.marketName;
  }
  if (data.label && (data.displayOdds || data.trueOdds)) {
    out.push({
      market: next.market || "Unknown",
      marketId: data.marketId || next.marketId || null,
      selection: data.label,
      american: parseAmerican(data.displayOdds?.american ?? data.trueOdds),
      american_display: data.displayOdds?.american ?? null,
      raw: data,
    });
  }
  for (const v of Object.values(data)) walkNested(v, next, out);
  return out;
}

function extractProps(blob) {
  if (blob.markets && blob.selections) return parseNashBlob(blob);
  return walkNested(blob);
}

const browser = await chromium.launch({ headless: false });
const page = await browser.newPage();
const captured = [];
page.on("response", async (resp) => {
  if (!resp.url().includes("sportsbook-nash") || resp.status() !== 200) return;
  try {
    const ct = resp.headers()["content-type"] || "";
    if (!ct.includes("json")) return;
    captured.push(await resp.json());
  } catch {
    /* ignore */
  }
});

for (const url of URLS) {
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 60000 });
  await page.waitForTimeout(5000);
}
await browser.close();

let props = [];
for (const b of captured) props.push(...extractProps(b));
const seen = new Set();
props = props.filter((p) => {
  const k = `${p.marketId}|${p.selection}|${p.american}`;
  if (seen.has(k)) return false;
  seen.add(k);
  return Number.isFinite(p.american);
});

writeFileSync("output/dk-all-props.json", JSON.stringify(props, null, 2));

const byMarket = new Map();
for (const p of props) {
  const k = p.marketId || p.market;
  if (!byMarket.has(k)) byMarket.set(k, { marketId: p.marketId, market: p.market, rows: [] });
  byMarket.get(k).rows.push(p);
}

for (const [, v] of byMarket) {
  const sels = v.rows.map((r) => `${r.selection} ${r.american_display || r.american}`);
  if (!sels.some((s) => /USA/i.test(s))) continue;
  console.log(`[${v.marketId}] ${v.market}`);
  console.log(" ", sels.join(" | "));
}

console.log("\nTotal props:", props.length, "markets:", byMarket.size);
