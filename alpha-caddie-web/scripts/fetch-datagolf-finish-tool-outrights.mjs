#!/usr/bin/env node
/**
 * Odds for Course Fit / +EV outright columns: align with DataGolf “Finish Position” scratch tool.
 *
 * Source: scrape the rendered table at https://datagolf.com/betting-tool-finish.
 * Do not call the DataGolf feed API for finish-tool EV rows.
 *
 * Scrapes every outright finish market from the rendered page and merges into `projections.json`.
 *
 * Optional auth for full Scratch table:
 *   DATAGOLF_PLAYWRIGHT_STORAGE_STATE=/path/to/storage.json
 *   DATAGOLF_PLAYWRIGHT_STORAGE_STATE_JSON='{"cookies":[...],"origins":[...]}'
 *   DATAGOLF_PLAYWRIGHT_STORAGE_STATE_B64=<base64 storage-state JSON>
 *
 *   npm run fetch:finish-tool
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { fetchDataGolfFinishToolOutrightsFromPage } from "./datagolf-finish-tool-page-scraper.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

function mergeOutrightPacks(existingPack, scrapedPack) {
  if (!scrapedPack || !Array.isArray(scrapedPack.rows) || !scrapedPack.rows.length) return existingPack;
  const byId = new Map();
  for (const row of Array.isArray(existingPack?.rows) ? existingPack.rows : []) {
    const id = Math.round(Number(row?.dg_id));
    if (Number.isFinite(id)) byId.set(id, { ...row, dg_id: id });
  }
  for (const row of scrapedPack.rows) {
    const id = Math.round(Number(row?.dg_id));
    if (!Number.isFinite(id)) continue;
    byId.set(id, { ...(byId.get(id) || {}), ...row, dg_id: id });
  }
  const bookKeys = new Set(Array.isArray(existingPack?.bookKeys) ? existingPack.bookKeys : []);
  for (const bk of scrapedPack.bookKeys || []) bookKeys.add(bk);
  return { rows: [...byId.values()], bookKeys: [...bookKeys].sort() };
}

async function main() {
  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("Missing", projPath);
    process.exit(1);
  }

  let payload;
  try {
    payload = JSON.parse(readFileSync(projPath, "utf8"));
  } catch (e) {
    console.error("Could not parse projections.json:", e.message);
    process.exit(1);
  }

  const outrights = { ...(payload.outrights && typeof payload.outrights === "object" ? payload.outrights : {}) };
  console.log("[fetch:finish-tool] Scraping datagolf.com/betting-tool-finish rendered table…");
  const scraped = await fetchDataGolfFinishToolOutrightsFromPage({ players: payload.players });
  for (const msg of scraped.logs || []) console.log("[fetch:finish-tool]", msg);
  for (const [market, pack] of Object.entries(scraped.outrights || {})) {
    outrights[market] = mergeOutrightPacks(outrights[market], pack);
  }

  const next = {
    ...payload,
    outrights,
    outrights_odds_format: "percent",
    updated_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    book_odds_refreshed_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
  };

  writeFileSync(projPath, JSON.stringify(next, null, 2), "utf8");
  console.log(`[fetch:finish-tool] Wrote ${projPath} (outrights only). Course Fit / +EV read DATA.outrights from this file.`);

  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  if (existsSync(dirname(websiteProj))) {
    writeFileSync(websiteProj, JSON.stringify(next, null, 2), "utf8");
    console.log(`[fetch:finish-tool] Mirrored -> ${websiteProj}`);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
