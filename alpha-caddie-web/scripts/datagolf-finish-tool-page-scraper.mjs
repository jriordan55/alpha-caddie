import { existsSync, readFileSync } from "fs";
import { chromium } from "playwright";

const FINISH_TOOL_URL = "https://datagolf.com/betting-tool-finish";

const MARKET_LABEL_TO_KEY = {
  WIN: "win",
  "TOP 5": "top_5",
  "TOP 10": "top_10",
  "TOP 20": "top_20",
  "MAKE CUT": "make_cut",
  "MISS CUT": "mc",
};

const BOOK_CODE_TO_KEY = {
  b3: "bet365",
  wh: "williamhill",
  fd: "fanduel",
  dk: "draftkings",
  pb: "pointsbet",
  mgm: "betmgm",
  cz: "caesars",
  caesars: "caesars",
  bol: "betonline",
  bo: "betonline",
  pin: "pinnacle",
};

function normName(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/\./g, "")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function displayGolferName(name) {
  const s = String(name || "").trim();
  const m = s.match(/^([^,]+),\s*(.+)$/);
  if (m) return `${m[2].trim()} ${m[1].trim()}`.trim();
  return s;
}

function buildPlayerLookup(players) {
  const out = new Map();
  for (const p of Array.isArray(players) ? players : []) {
    const id = Math.round(Number(p?.dg_id));
    const raw = String(p?.player_name || "").trim();
    if (!Number.isFinite(id) || id <= 0 || !raw) continue;
    const display = displayGolferName(raw);
    const comma = raw.match(/^([^,]+),\s*(.+)$/);
    out.set(normName(raw), { dg_id: id, player_name: raw });
    out.set(normName(display), { dg_id: id, player_name: raw });
    if (comma) out.set(normName(`${comma[1]} ${comma[2]}`), { dg_id: id, player_name: raw });
  }
  return out;
}

function pctFromText(text) {
  const m = String(text || "").replace(/\u2212/g, "-").match(/([+-]?\d+(?:\.\d+)?)\s*%/);
  if (!m) return NaN;
  const n = Number(m[1]);
  return Number.isFinite(n) && n > 0 ? n : NaN;
}

function bookKeyFromClass(cls) {
  const text = String(cls || "");
  const m = text.match(/\b([a-z0-9]+)-table-col\b/i) || text.match(/\b([a-z0-9]+)-odds-row\b/i);
  if (!m) return "";
  const code = m[1].toLowerCase();
  return BOOK_CODE_TO_KEY[code] || code;
}

async function dismissCookieBanner(page) {
  await page
    .getByText("Got it!", { exact: true })
    .click({ timeout: 2500 })
    .catch(() => {});
}

function loadDataGolfStorageState() {
  const inlineJson = String(process.env.DATAGOLF_PLAYWRIGHT_STORAGE_STATE_JSON || "").trim();
  if (inlineJson) return JSON.parse(inlineJson);

  const inlineB64 = String(process.env.DATAGOLF_PLAYWRIGHT_STORAGE_STATE_B64 || "").trim();
  if (inlineB64) return JSON.parse(Buffer.from(inlineB64, "base64").toString("utf8"));

  const storagePath = String(process.env.DATAGOLF_PLAYWRIGHT_STORAGE_STATE || "").trim();
  if (storagePath && existsSync(storagePath)) return JSON.parse(readFileSync(storagePath, "utf8"));
  return undefined;
}

async function ensureImpliedPercentOdds(page) {
  const toggle = page.locator(".format-toggle").first();
  const current = await toggle.innerText({ timeout: 5000 }).catch(() => "");
  if (/IMPLIED\s*%/i.test(current)) return;
  await toggle.click({ timeout: 5000 });
  await page.getByText(/IMPLIED\s*%\s*\(25%\)/i).click({ timeout: 5000 });
  await page.waitForTimeout(800);
}

async function selectMarket(page, label) {
  const option = page.locator(".fin-options").filter({ hasText: new RegExp(`^\\s*${label}\\s*$`, "i") }).first();
  await option.click({ timeout: 10000 });
  await page.waitForTimeout(1200);
}

async function scrapeCurrentMarket(page, players, marketLabel) {
  const lookup = buildPlayerLookup(players);
  return page.locator(".table .datarow").evaluateAll(
    (rowEls, arg) => {
      const bookMap = arg.bookMap;
      const rows = [];
      let anonymized = 0;
      const pctFrom = (text) => {
        const m = String(text || "").replace(/\u2212/g, "-").match(/([+-]?\d+(?:\.\d+)?)\s*%/);
        return m ? Number(m[1]) : NaN;
      };
      const bookKey = (cls) => {
        const m = String(cls || "").match(/\b([a-z0-9]+)-table-col\b/i) || String(cls || "").match(/\b([a-z0-9]+)-odds-row\b/i);
        if (!m) return "";
        const code = m[1].toLowerCase();
        return bookMap[code] || code;
      };
      for (const rowEl of rowEls) {
        const name = (rowEl.querySelector(".name-col .big-only")?.textContent || rowEl.querySelector(".name-col")?.textContent || "").trim();
        if (!name) continue;
        if (/STOP\s+PEEKING/i.test(name)) {
          anonymized += 1;
          continue;
        }
        const out = { scraped_name: name };
        const dgPct = pctFrom(rowEl.querySelector(".dg-pred-col")?.textContent || "");
        if (Number.isFinite(dgPct) && dgPct > 0) out.dg_model = dgPct;
        for (const cell of rowEl.querySelectorAll(".book-pred-col")) {
          const key = bookKey(cell.className);
          if (!key) continue;
          const pct = pctFrom(cell.querySelector(".bookie-odds-row")?.textContent || cell.textContent || "");
          if (Number.isFinite(pct) && pct > 0) out[key] = pct;
        }
        rows.push(out);
      }
      return { rows, anonymized, marketLabel: arg.marketLabel };
    },
    { bookMap: BOOK_CODE_TO_KEY, marketLabel },
  ).then((result) => {
    const rows = [];
    const bookKeys = new Set();
    for (const row of result.rows || []) {
      const match = lookup.get(normName(row.scraped_name));
      if (!match) continue;
      const out = { dg_id: match.dg_id, player_name: match.player_name };
      for (const [key, value] of Object.entries(row)) {
        if (key === "scraped_name") continue;
        const n = Number(value);
        if (!Number.isFinite(n) || n <= 0) continue;
        out[key] = n;
        if (key !== "dg_model") bookKeys.add(key);
      }
      rows.push(out);
    }
    return { rows, bookKeys: [...bookKeys].sort(), anonymized: result.anonymized || 0 };
  });
}

export async function fetchDataGolfFinishToolOutrightsFromPage({ players, markets = MARKET_LABEL_TO_KEY, allowPartialPublic = true } = {}) {
  const storageState = loadDataGolfStorageState();
  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext(storageState ? { storageState } : {});
  const page = await context.newPage();
  page.setDefaultTimeout(20000);
  try {
    await page.goto(FINISH_TOOL_URL, { waitUntil: "domcontentloaded", timeout: 120000 });
    await dismissCookieBanner(page);
    await ensureImpliedPercentOdds(page);
    await page.waitForSelector(".table .datarow", { timeout: 30000 });
    const outrights = {};
    const logs = [];
    let gated = false;
    for (const [label, marketKey] of Object.entries(markets)) {
      await selectMarket(page, label);
      const scraped = await scrapeCurrentMarket(page, players, label);
      if (scraped.anonymized > 0) {
        gated = true;
        logs.push(
          `${marketKey}: Scratch-gated (${scraped.anonymized} anonymized rows); using ${scraped.rows.length} visible named rows only`,
        );
        if (!allowPartialPublic) {
          throw new Error(
            `DataGolf finish page is Scratch-gated (${scraped.anonymized} anonymized rows on ${label}); provide DATAGOLF_PLAYWRIGHT_STORAGE_STATE, DATAGOLF_PLAYWRIGHT_STORAGE_STATE_JSON, or DATAGOLF_PLAYWRIGHT_STORAGE_STATE_B64 for a logged-in Scratch session.`,
          );
        }
      }
      if (scraped.rows.length < 20 && !allowPartialPublic) {
        throw new Error(`DataGolf finish page scrape returned only ${scraped.rows.length} matched rows for ${label}; refusing to overwrite full EV data.`);
      }
      if (!scraped.rows.length) continue;
      outrights[marketKey] = { rows: scraped.rows, bookKeys: scraped.bookKeys };
      logs.push(`${marketKey}: ${scraped.rows.length} rows, books=${scraped.bookKeys.join(",") || "(none)"}`);
    }
    return { outrights, logs, gated };
  } finally {
    await browser.close();
  }
}

