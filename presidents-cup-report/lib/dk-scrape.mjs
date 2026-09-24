/**
 * DraftKings Presidents Cup props via Nash API + Playwright session.
 */
import { existsSync, readFileSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { chromium } from "./playwright.mjs";
import { DK_SUBCATEGORIES } from "./dk-tabs.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "../../alpha-caddie-web");

const LEAGUE_BASE = "https://sportsbook.draftkings.com/leagues/golf/presidents-cup?category=tournament";
const LEAGUE_HOME = "https://sportsbook.draftkings.com/leagues/golf/presidents-cup";

const PROP_URL = process.env.DK_PC_URL?.trim() || `${LEAGUE_BASE}&subcategory=player-props`;
const WINNER_URL = process.env.DK_PC_WINNER_URL?.trim() || `${LEAGUE_BASE}&subcategory=winner`;

function resolveHeadless() {
  const v = String(process.env.DK_HEADLESS ?? "").trim().toLowerCase();
  if (v === "1" || v === "true") return true;
  if (v === "0" || v === "false") return false;
  return process.platform !== "win32" && process.platform !== "darwin";
}

function subcategoryFromUrl(url) {
  const m = String(url || "").match(/subcategory=([^&]+)/i);
  return m ? decodeURIComponent(m[1]) : "popular";
}

async function expandDkSections(page) {
  await page.evaluate(() => {
    for (const el of document.querySelectorAll("button, [role='button'], summary")) {
      const t = (el.textContent || "").trim();
      if (/^\+|^expand|^show|^see all|^view all/i.test(t) || el.getAttribute("aria-expanded") === "false") {
        try {
          el.click();
        } catch {
          /* ignore */
        }
      }
    }
    window.scrollTo(0, document.body.scrollHeight);
  }).catch(() => {});
}

async function loadDkPage(page, url) {
  await page.goto(url, { waitUntil: "domcontentloaded", timeout: 60000 });
  try {
    await page.waitForResponse(
      (r) => r.url().includes("sportsbook-nash") && r.status() === 200,
      { timeout: 20000 },
    );
  } catch {
    /* may still parse page */
  }
  await page.waitForTimeout(5000);
  await expandDkSections(page);
  await page.waitForTimeout(2000);
  await expandDkSections(page);
  await page.waitForTimeout(1500);
}

function parseAmericanOdds(raw) {
  const s = String(raw ?? "").trim();
  if (!s) return null;
  const neg = /^[-−–]/.test(s) || /\s[-−–]\s*\d/.test(s);
  const digits = s.replace(/[^0-9.]/g, "");
  const n = Number(digits);
  if (!Number.isFinite(n) || n === 0) return null;
  return neg ? -Math.abs(n) : Math.abs(n);
}

function parseNashFlat(data, out = [], ctx = {}) {
  if (Array.isArray(data)) {
    for (const x of data) parseNashFlat(x, out, ctx);
    return out;
  }
  if (!data || typeof data !== "object") return out;

  if (Array.isArray(data.markets) && Array.isArray(data.selections)) {
    const selByMarket = new Map();
    for (const s of data.selections) {
      if (!selByMarket.has(s.marketId)) selByMarket.set(s.marketId, []);
      selByMarket.get(s.marketId).push(s);
    }
    for (const m of data.markets) {
      const marketName = m.name || m.marketType?.name || "Unknown";
      for (const s of selByMarket.get(m.id) || []) {
        const label = String(s.label || s.name || "").trim();
        if (!label) continue;
        const american = parseAmericanOdds(s.displayOdds?.american ?? s.oddsAmerican ?? s.trueOdds);
        if (!Number.isFinite(american)) continue;
        out.push({
          market: marketName,
          marketId: m.id,
          selection: label,
          american,
          american_display: s.displayOdds?.american ?? null,
          dk_subcategory: ctx.subcategory || null,
          raw: s,
        });
      }
    }
  }

  for (const v of Object.values(data)) {
    if (v && typeof v === "object") parseNashFlat(v, out, ctx);
  }
  return out;
}

function extractPropsFromCaptured(captured, subcategory) {
  const out = [];
  for (const blob of captured) parseNashFlat(blob, out, { subcategory });
  return normalizeDkMarkets(dedupeProps(out));
}

function normalizeDkMarkets(rows) {
  return rows.map((r) => {
    let market = r.market;
    if (/^tie$/i.test(String(r.selection || "").trim()) && /winner|outright|cup|foursomes|fourball|holes/i.test(market)) {
      market = market.includes("Cup") || market.includes("Outright") ? "Cup Result" : market;
    }
    return { ...r, market };
  });
}

function dedupeProps(rows) {
  const seen = new Set();
  const out = [];
  for (const r of rows) {
    const k = `${r.dk_subcategory || ""}|${r.marketId || r.market}|${r.selection}|${r.american}`;
    if (seen.has(k)) continue;
    seen.add(k);
    if (Number.isFinite(r.american)) out.push(r);
  }
  return out;
}

async function scrapeDomOdds(page, subcategory) {
  const rows = await page.evaluate(() => {
    const out = [];
    const re = /([+-]\d{3,4})/;
    const nodes = document.querySelectorAll(
      '[data-testid*="offer"], [class*="sportsbook-outcome"], [class*="outcome"], button, a',
    );
    for (const el of nodes) {
      const txt = (el.textContent || "").replace(/\s+/g, " ").trim();
      if (!txt || txt.length > 120) continue;
      const m = txt.match(re);
      if (!m) continue;
      const american = Number(m[1]);
      if (!Number.isFinite(american)) continue;
      const selection = txt.replace(re, "").trim();
      if (selection.length < 2) continue;
      out.push({ market: "Player prop", selection, american });
    }
    return out;
  });
  return rows.map((r) => ({ ...r, dk_subcategory: subcategory }));
}

function loadSeedLines() {
  const p = join(__dirname, "../data/dk-seed-lines.json");
  if (!existsSync(p)) return [];
  try {
    const j = JSON.parse(readFileSync(p, "utf8"));
    return (j.props || []).filter((x) => x.selection && Number.isFinite(x.american));
  } catch {
    return [];
  }
}

async function discoverSubcategoryUrls(page) {
  return page.evaluate(() =>
    [...document.querySelectorAll("a[href*='presidents-cup']")]
      .map((a) => a.getAttribute("href"))
      .filter((h) => h && h.includes("subcategory="))
      .map((href) => (href.startsWith("http") ? href : `https://sportsbook.draftkings.com${href}`)),
  );
}

export async function scrapeDraftKingsPresidentsCupProps() {
  const headless = resolveHeadless();
  const browser = await chromium.launch({ headless });
  const ctx = await browser.newContext({ viewport: { width: 1400, height: 900 }, locale: "en-US" });
  await ctx.addInitScript(() => {
    Object.defineProperty(navigator, "webdriver", { get: () => false });
  });
  const page = await ctx.newPage();

  try {
    let blocked = false;
    await loadDkPage(page, LEAGUE_HOME);
    const textMain = await page.innerText("body").catch(() => "");
    blocked = blocked || /log in to view|must be logged in/i.test(textMain);
    const discovered = await discoverSubcategoryUrls(page);

    const urlEntries = [];
    const seenSubs = new Set();
    for (const sub of DK_SUBCATEGORIES) {
      urlEntries.push({ url: `${LEAGUE_BASE}&subcategory=${sub}`, subcategory: sub });
      seenSubs.add(sub);
    }
    for (const url of [PROP_URL, WINNER_URL, ...discovered]) {
      const sub = subcategoryFromUrl(url);
      if (!seenSubs.has(sub)) {
        urlEntries.push({ url, subcategory: sub });
        seenSubs.add(sub);
      }
    }

    let props = [];
    for (const { url, subcategory } of urlEntries) {
      const captured = [];
      const handler = async (resp) => {
        const rurl = resp.url();
        if (!rurl.includes("sportsbook-nash") && !rurl.includes("sportsbook.draftkings.com/sites/")) return;
        if (resp.status() !== 200) return;
        try {
          const ct = resp.headers()["content-type"] || "";
          if (!ct.includes("json")) return;
          captured.push(await resp.json());
        } catch {
          /* ignore */
        }
      };
      page.on("response", handler);
      await loadDkPage(page, url);
      page.off("response", handler);
      const text = await page.innerText("body").catch(() => "");
      blocked = blocked || /log in to view|must be logged in/i.test(text);
      props.push(...extractPropsFromCaptured(captured, subcategory));
      if (!props.length) {
        props.push(...normalizeDkMarkets(dedupeProps(await scrapeDomOdds(page, subcategory))));
      }
    }

    props = dedupeProps(props);

    if (!props.length) {
      const seed = loadSeedLines();
      if (seed.length) props = seed.map((x) => ({ ...x, dk_subcategory: x.dk_subcategory || "player-props" }));
    }

    return {
      url: PROP_URL,
      winner_url: WINNER_URL,
      scraped_at: new Date().toISOString(),
      blocked,
      n_captured: urlEntries.length,
      props,
      note: props.length
        ? null
        : blocked
          ? "DraftKings requires login for this page; model-only prices shown."
          : "No player props parsed — markets may not be posted yet.",
    };
  } finally {
    await browser.close();
  }
}

/** Fallback: read paper-book if scrape empty */
export function loadPaperBookFallback() {
  const p = join(WEB_ROOT, "paper-book", "paper-book-lines.json");
  if (!existsSync(p)) return [];
  try {
    const j = JSON.parse(readFileSync(p, "utf8"));
    const cards = j.cards || j.lines || [];
    return cards
      .filter((c) => String(c.book || "").toLowerCase().includes("draftkings"))
      .map((c) => ({
        market: c.market || c.prop_type || "Prop",
        selection: c.selection || c.player_name || c.label || "",
        american: Number(c.american_odds ?? c.odds ?? c.american),
        dk_subcategory: "player-props",
      }))
      .filter((x) => x.selection && Number.isFinite(x.american));
  } catch {
    return [];
  }
}
