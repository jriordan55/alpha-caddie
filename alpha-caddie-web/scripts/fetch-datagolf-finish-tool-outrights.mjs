#!/usr/bin/env node
/**
 * Odds for Course Fit / +EV outright columns: align with DataGolf “Finish Position” scratch tool.
 *
 * Source: DataGolf betting-tools/outrights API, which backs https://datagolf.com/betting-tool-finish.
 *
 * Fetches every outright finish market and merges into `projections.json`.
 *
 *   npm run fetch:finish-tool
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { fetchDataGolfOutrightsApi } from "./datagolf-outrights-api.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

function loadApiKey() {
  const env = String(process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const localPath = join(WEB_ROOT, "datagolf.local.json");
  if (!existsSync(localPath)) return "";
  try {
    const json = JSON.parse(readFileSync(localPath, "utf8"));
    return String(json.apiKey || json.key || "").trim();
  } catch {
    return "";
  }
}

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
  const apiKey = loadApiKey();
  if (!apiKey) {
    console.error("Missing API key. Set DATAGOLF_API_KEY or datagolf.local.json.");
    process.exit(1);
  }

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
  const tourForFeeds = String(payload.datagolf_feed_tour || process.env.GOLF_DATAGOLF_TOUR || process.env.GOLF_TOUR || "pga").trim().toLowerCase() || "pga";
  console.log("[fetch:finish-tool] Fetching DataGolf betting-tools/outrights…");
  const dgOutrights = await fetchDataGolfOutrightsApi({ apiKey, tour: tourForFeeds, oddsFormat: "percent" });
  for (const msg of dgOutrights.logs || []) console.log("[fetch:finish-tool]", msg);
  for (const [market, pack] of Object.entries(dgOutrights.outrights || {})) {
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
