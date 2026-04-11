import { writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { chromium } from "playwright";

const __dirname = dirname(fileURLToPath(import.meta.url));

function walkFind(obj, pred, path = "", out = [], depth = 0) {
  if (depth > 25 || out.length > 200) return out;
  if (obj == null) return out;
  if (typeof obj === "string") {
    if (pred(obj, path)) out.push({ path, value: obj.slice(0, 200) });
    return out;
  }
  if (typeof obj !== "object") return out;
  if (Array.isArray(obj)) {
    for (let i = 0; i < Math.min(obj.length, 400); i++) walkFind(obj[i], pred, `${path}[${i}]`, out, depth + 1);
    return out;
  }
  for (const k of Object.keys(obj)) {
    const p = path ? `${path}.${k}` : k;
    if (pred(k, p)) out.push({ path: p, key: k });
    walkFind(obj[k], pred, p, out, depth + 1);
  }
  return out;
}

(async () => {
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage({
    userAgent:
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
    viewport: { width: 1400, height: 900 },
  });
  await page.goto("https://sportsbook.draftkings.com/leagues/golf/us-masters", {
    waitUntil: "domcontentloaded",
    timeout: 90000,
  });
  await page.waitForTimeout(6000);
  const hits = await page.evaluate(() => {
    const ini = window.__INITIAL_STATE__;
    if (!ini) return [];
    const json = JSON.stringify(ini);
    const out = [];
    const needles = ["birdie-par-bogey", "round-score", "Round Score", "92694", "subcategoryId"];
    for (const n of needles) {
      let i = 0;
      while (i < json.length && out.length < 80) {
        const j = json.indexOf(n, i);
        if (j < 0) break;
        out.push({ n, ctx: json.slice(Math.max(0, j - 80), j + 120) });
        i = j + n.length;
      }
    }
    return out;
  });
  writeFileSync(join(__dirname, "dk-initial-needle-hits.json"), JSON.stringify(hits, null, 2), "utf8");
  console.log("hits", hits.length, "-> dk-initial-needle-hits.json");
  await browser.close();
})();
