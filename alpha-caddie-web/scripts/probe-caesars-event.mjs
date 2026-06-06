import { chromium } from "playwright";
import { writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const sport = process.argv[2]?.trim() || "basketball";
const loc = process.env.CAESARS_LOCATION?.trim() || "nj";

const browser = await chromium.launch({
  headless: true,
  args: ["--disable-blink-features=AutomationControlled"],
});
const ctx = await browser.newContext({
  userAgent:
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
  viewport: { width: 1400, height: 900 },
});
const page = await ctx.newPage();
const apis = [];

page.on("response", async (res) => {
  const u = res.url();
  if (!u.includes("americanwagering.com/sb/")) return;
  if (res.status() !== 200) return;
  const ct = res.headers()["content-type"] || "";
  if (!ct.includes("json")) return;
  try {
    apis.push({ url: u, body: await res.json() });
  } catch {
    /* ignore */
  }
});

await page.goto(`https://sportsbook.caesars.com/us/${loc}/bet/${sport}`, {
  waitUntil: "networkidle",
  timeout: 120000,
});
await page.waitForTimeout(4000);

const link = page.locator(`a[href*="/bet/${sport}/"]`).first();
if (await link.count()) {
  await link.click({ timeout: 15000 }).catch(() => {});
  await page.waitForTimeout(5000);
}

console.log("apis", apis.length);
for (const a of apis) {
  console.log(a.url.split("?")[0].replace(/.*\/sb\//, ""));
}

const eventApi = apis.find((a) => /\/events\/[^/]+$/i.test(a.url.split("?")[0]));
if (eventApi) {
  const outPath = join(__dirname, "..", "data", "caesars-event-sample.json");
  writeFileSync(outPath, JSON.stringify(eventApi.body, null, 2).slice(0, 500000));
  console.log("saved", outPath, "keys", Object.keys(eventApi.body));
  console.log(JSON.stringify(eventApi.body).slice(0, 2500));
}

const menu = apis.find((a) => a.url.includes("sports-menu"));
if (menu) {
  const golf = menu.body.find?.((s) => s.sportId === "golf");
  console.log("golf menu", JSON.stringify(golf)?.slice(0, 2000));
}

await browser.close();
