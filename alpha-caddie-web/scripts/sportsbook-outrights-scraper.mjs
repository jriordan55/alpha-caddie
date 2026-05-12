import { chromium } from "playwright";

export const DEFAULT_SPORTSBOOK_OUTRIGHT_URLS = [
  "https://sportsbook.draftkings.com/leagues/golf/uspga-championship?category=outrights",
  "https://sportsbook.fanduel.com/navigation/pga?tab=finishing-positions",
  "https://sportsbook.fanduel.com/navigation/pga",
  "https://sportsbook.fanduel.com/navigation/pga?tab=make-miss-cut",
  "https://www.ma.betmgm.com/en/sports/events/2026-us-pga-championship-17570970?tab=score&market=Place",
  "https://www.ma.betmgm.com/en/sports/events/2026-us-pga-championship-17570970?tab=score",
  "https://sportsbook.thescore.bet/sport/golf/organization/majors/competition/pga-championship#tournament_props",
  "https://sportsbook.thescore.bet/sport/golf/organization/majors/competition/pga-championship#tournament_winner",
];

const MARKET_LABELS = [
  ["win", /^(?:tournament\s+winner|outright\s+winner|winner)$/i],
  ["top_5", /^top\s*5(?:\s+finish)?(?:\s+\(.*\))?$/i],
  ["top_10", /^top\s*10(?:\s+finish)?(?:\s+\(.*\))?$/i],
  ["top_20", /^top\s*20(?:\s+finish)?(?:\s+\(.*\))?$/i],
  ["make_cut", /^(?:make\s+(?:the\s+)?cut|to\s+make\s+the\s+cut|yes)$/i],
  ["mc", /^(?:miss\s+(?:the\s+)?cut|to\s+miss\s+the\s+cut|no)$/i],
];

function displayGolferName(name) {
  const s = String(name || "").trim();
  const m = s.match(/^([^,]+),\s*(.+)$/);
  if (m) return `${m[2].trim()} ${m[1].trim()}`.trim();
  return s;
}

function normName(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/\./g, "")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function bookFromUrl(url) {
  const host = new URL(url).hostname.toLowerCase();
  if (host.includes("draftkings")) return "draftkings";
  if (host.includes("fanduel")) return "fanduel";
  if (host.includes("betmgm")) return "betmgm";
  if (host.includes("thescore")) return "thescore";
  return host.replace(/^www\./, "").split(".")[0] || "sportsbook";
}

function marketFromLabel(line) {
  const s = String(line || "").trim();
  for (const [key, re] of MARKET_LABELS) {
    if (re.test(s)) return key;
  }
  return "";
}

function marketHintsFromUrl(url) {
  const u = new URL(url);
  const text = `${u.pathname} ${u.search} ${u.hash}`.toLowerCase();
  const out = [];
  if (text.includes("make-miss-cut") || text.includes("make_cut")) out.push("make_cut", "mc");
  if (text.includes("finishing") || text.includes("place") || text.includes("tournament_props")) {
    out.push("top_5", "top_10", "top_20", "make_cut");
  }
  if (text.includes("winner") || text.includes("outrights") || text.includes("score")) out.push("win");
  return [...new Set(out)];
}

function impliedPctFromAmerican(am) {
  const n = Number(am);
  if (!Number.isFinite(n) || n === 0) return NaN;
  return n > 0 ? (100 / (n + 100)) * 100 : (Math.abs(n) / (Math.abs(n) + 100)) * 100;
}

function impliedPctFromDecimal(dec) {
  const n = Number(dec);
  if (!Number.isFinite(n) || n <= 1) return NaN;
  return (1 / n) * 100;
}

function parseOddsToken(raw) {
  const s = String(raw || "")
    .replace(/\u2212/g, "-")
    .replace(/−/g, "-")
    .trim();
  if (/^[+-]\d{2,6}$/.test(s)) return impliedPctFromAmerican(Number(s));
  if (/^\d+\.\d{2,}$/.test(s)) return impliedPctFromDecimal(Number(s));
  return NaN;
}

function buildPlayerLookup(players) {
  const out = new Map();
  for (const p of Array.isArray(players) ? players : []) {
    const id = Math.round(Number(p?.dg_id));
    const raw = String(p?.player_name || "").trim();
    if (!Number.isFinite(id) || id <= 0 || !raw) continue;
    const disp = displayGolferName(raw);
    out.set(normName(raw), { dg_id: id, player_name: raw, display: disp });
    out.set(normName(disp), { dg_id: id, player_name: raw, display: disp });
  }
  return out;
}

function pageLooksBlocked(text) {
  return /press\s*&\s*hold|confirm you are a human|not a bot|access denied/i.test(String(text || ""));
}

function parseTextOdds(text, players, url) {
  const book = bookFromUrl(url);
  const lookup = buildPlayerLookup(players);
  const lines = String(text || "")
    .split(/\r?\n/)
    .map((s) => s.trim())
    .filter(Boolean);
  const rows = [];
  let markets = [];
  for (let i = 0; i < lines.length; i++) {
    const mk = marketFromLabel(lines[i]);
    if (mk && !markets.includes(mk)) markets.push(mk);
    const player = lookup.get(normName(lines[i]));
    if (!player) continue;
    const vals = [];
    for (let j = i + 1; j < Math.min(lines.length, i + 6); j++) {
      const pct = parseOddsToken(lines[j]);
      if (!Number.isFinite(pct)) break;
      vals.push(pct);
      if (vals.length >= Math.max(1, markets.length || 1)) break;
    }
    const hinted = marketHintsFromUrl(url);
    const useMarkets = markets.length ? markets : hinted;
    if (!useMarkets.length || !vals.length) continue;
    for (let k = 0; k < Math.min(useMarkets.length, vals.length); k++) {
      rows.push({
        market: useMarkets[k],
        dg_id: player.dg_id,
        player_name: player.player_name,
        book,
        pct: vals[k],
      });
    }
  }
  return rows;
}

function mergeScrapedRows(rows) {
  const byMarket = {};
  for (const r of rows) {
    if (!r?.market || !Number.isFinite(r.dg_id) || !r.book || !Number.isFinite(r.pct) || r.pct <= 0) continue;
    if (!byMarket[r.market]) byMarket[r.market] = { rows: new Map(), bookKeys: new Set() };
    const pack = byMarket[r.market];
    const key = String(Math.round(r.dg_id));
    const prev = pack.rows.get(key) || { dg_id: Math.round(r.dg_id), player_name: r.player_name };
    prev[r.book] = r.pct;
    pack.rows.set(key, prev);
    pack.bookKeys.add(r.book);
  }
  return Object.fromEntries(
    Object.entries(byMarket).map(([market, pack]) => [
      market,
      {
        rows: [...pack.rows.values()],
        bookKeys: [...pack.bookKeys].sort(),
      },
    ]),
  );
}

export function sportsbookOutrightUrlsFromEnv() {
  const raw = String(process.env.GOLF_SPORTSBOOK_OUTRIGHT_URLS || "").trim();
  if (!raw) return DEFAULT_SPORTSBOOK_OUTRIGHT_URLS;
  return raw
    .split(/[\n,]+/)
    .map((s) => s.trim())
    .filter(Boolean);
}

export async function fetchSportsbookOutrightsFromUrls({ players, urls = sportsbookOutrightUrlsFromEnv() } = {}) {
  if (String(process.env.GOLF_SKIP_SPORTSBOOK_OUTRIGHT_SCRAPE || "").trim() === "1") {
    return { outrights: {}, logs: ["skipped by GOLF_SKIP_SPORTSBOOK_OUTRIGHT_SCRAPE=1"] };
  }
  const browser = await chromium.launch({ headless: true });
  const scraped = [];
  const logs = [];
  const blockedBooks = new Set();
  try {
    const page = await browser.newPage();
    page.setDefaultTimeout(15000);
    for (const url of urls) {
      const book = bookFromUrl(url);
      if (blockedBooks.has(book)) {
        logs.push(`${book}: skipped duplicate after challenge page`);
        continue;
      }
      try {
        await page.goto(url, { waitUntil: "domcontentloaded", timeout: 45000 });
        await page.waitForTimeout(4500);
        const text = await page.locator("body").innerText({ timeout: 10000 }).catch(() => "");
        if (pageLooksBlocked(text)) {
          blockedBooks.add(book);
          logs.push(`${book}: blocked/challenge page`);
          continue;
        }
        if (book === "thescore" && String(process.env.GOLF_ENABLE_THESCORE_OUTRIGHT_SCRAPE || "").trim() !== "1") {
          logs.push(`${book}: skipped mixed promo/parlay page (set GOLF_ENABLE_THESCORE_OUTRIGHT_SCRAPE=1 to force)`);
          continue;
        }
        const rows = parseTextOdds(text, players, url);
        scraped.push(...rows);
        logs.push(`${book}: ${rows.length} parsed odds from ${url}`);
      } catch (e) {
        logs.push(`${book}: ${e?.message || String(e)}`);
      }
    }
  } finally {
    await browser.close();
  }
  return { outrights: mergeScrapedRows(scraped), logs };
}
