/**
 * One-off: list DK subcategories that return per-player round O/U market names.
 * Usage: node scripts/probe-dk-subs.mjs
 */
import { readFileSync } from "fs";
import { chromium } from "playwright";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const url =
  process.env.DK_LEAGUE_URL?.trim() ||
  "https://sportsbook.draftkings.com/leagues/golf/the-cj-cup-byron-nelson?category=round";
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

const SAMPLE_RE = {
  GIR: /Greens?\s+in\s+Regulation|GIR/i,
  Putts: /(?:Total\s+)?Putts/i,
  Birdies: /Birdies or Better/i,
  "Most Birdies": /Most Birdies or Better/i,
  Pars: /\bPars\b/i,
  Bogeys: /Bogeys or Worse/i,
  "Fairways hit": /Fairways?\s+Hit/i,
  "Total Score": /Round Score/i,
};

function countMatchingMarkets(stat, markets) {
  const re = SAMPLE_RE[stat];
  if (!re) return 0;
  let n = 0;
  for (const m of markets) {
    const name = String(m?.name || "").replace(/[\u2013\u2014\u2212]/g, "-");
    if (/\bon\s+hole\b/i.test(name)) continue;
    if (/player\s+most\b/i.test(name)) continue;
    if (re.test(name) && /Round\s+\d|R\d/i.test(name)) n++;
  }
  return n;
}

const browser = await chromium.launch({ headless: true });
const ctx = await browser.newContext();
const page = await ctx.newPage();
await page.goto(url, { waitUntil: "domcontentloaded", timeout: 90000 });
await page.waitForTimeout(10000);

const nav = await page.evaluate(() => {
  const ini = window.__INITIAL_STATE__;
  const rows = [];
  function walk(o, d) {
    if (!o || typeof o !== "object" || d > 45) return;
    const p = o.parameters;
    if (p?.subcategoryId != null && p?.leagueId != null) {
      rows.push({
        leagueId: String(p.leagueId),
        sub: String(p.subcategoryId),
        seo: String(o.seoId || ""),
        title: String(o.title || ""),
      });
    }
    if (Array.isArray(o)) for (const x of o) walk(x, d + 1);
    else for (const k of Object.keys(o)) walk(o[k], d + 1);
  }
  walk(ini, 0);
  const counts = new Map();
  for (const r of rows) counts.set(r.leagueId, (counts.get(r.leagueId) || 0) + 1);
  let lid = "";
  let best = -1;
  for (const [k, c] of counts) {
    if (c > best) {
      best = c;
      lid = k;
    }
  }
  const subs = [...new Set(rows.filter((r) => r.leagueId === lid).map((r) => r.sub))];
  return { lid, subs, rows: rows.filter((r) => r.leagueId === lid) };
});

const byTitle = new Map();
for (const r of nav.rows) {
  const k = `${r.seo}|${r.title}`;
  if (!byTitle.has(k)) byTitle.set(k, r.sub);
}
console.log("nav tabs (seo|title -> sub):");
for (const [k, sub] of [...byTitle.entries()].sort((a, b) => a[0].localeCompare(b[0]))) {
  console.log(`  ${k} -> ${sub}`);
}

console.log(`league=${nav.lid} subs=${nav.subs.length}`);
const api = ctx.request;
const stats = ["GIR", "Putts", "Birdies", "Most Birdies", "Pars", "Bogeys", "Fairways hit", "Total Score"];

for (const stat of stats) {
  const hits = [];
  for (const sub of nav.subs) {
    const res = await api.get(marketsUrl(nav.lid, sub), { timeout: 45000 });
    if (!res.ok()) continue;
    let body;
    try {
      body = await res.json();
    } catch {
      continue;
    }
    const mk = Array.isArray(body?.markets) ? body.markets : [];
    const n = countMatchingMarkets(stat, mk);
    if (n > 0) {
      const sample = mk.find((m) => countMatchingMarkets(stat, [m]) > 0)?.name;
      hits.push({ sub, n, sample: String(sample || "").slice(0, 90) });
    }
    await new Promise((r) => setTimeout(r, 25));
  }
  hits.sort((a, b) => b.n - a.n);
  const total = hits.reduce((s, h) => s + h.n, 0);
  console.log(`\n${stat}: ${hits.length} subs, ~${total} market titles`);
  for (const h of hits.slice(0, 8)) console.log(`  sub=${h.sub} n=${h.n}  ${h.sample}`);
}

await browser.close();
