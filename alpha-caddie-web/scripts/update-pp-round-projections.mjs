#!/usr/bin/env node
/**
 * Update PrizePicks round projections in projections.json (props with source=prizepicks).
 * Does not replace DraftKings rows — merges alongside existing props.
 *
 *   npm run update:pp-round-projections
 *
 * Env (see prizepicks-ou-props.mjs):
 *   GOLF_SKIP_PP_OU=1 — exit without fetching
 *   PP_LEAGUE_ID, PP_API_BASE, PP_STATE_CODE, PP_GAME_MODE, PP_DEFAULT_ODDS
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { refreshPrizePicksRoundProps } from "./merge-pp-round-props.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

async function main() {
  if (String(process.env.GOLF_SKIP_PP_OU || "").trim() === "1") {
    console.error("[update:pp-round-projections] GOLF_SKIP_PP_OU=1 — nothing to do.");
    process.exit(0);
  }

  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("[update:pp-round-projections] Missing", projPath, "— run npm run fetch:dg first.");
    process.exit(1);
  }

  let payload;
  try {
    payload = JSON.parse(readFileSync(projPath, "utf8"));
  } catch (e) {
    console.error("[update:pp-round-projections] Could not parse projections.json:", e.message);
    process.exit(1);
  }
  if (!payload?.players?.length) {
    console.error("[update:pp-round-projections] projections.json has no players — run npm run fetch:dg first.");
    process.exit(1);
  }

  const { props, nPp, ppError } = await refreshPrizePicksRoundProps(payload);
  if (!nPp) {
    console.error("[update:pp-round-projections] No PrizePicks rows merged.", ppError ? `(${ppError})` : "");
    process.exit(1);
  }

  const now = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const next = {
    ...payload,
    props,
    updated_at: now,
    pp_round_props_refreshed_at: now,
  };

  const outJson = JSON.stringify(next, null, 2);
  writeFileSync(projPath, outJson, "utf8");
  console.log(`[update:pp-round-projections] Wrote ${projPath} — ${nPp} PrizePicks prop rows (${props.length} total props)`);

  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  const websiteDir = dirname(websiteProj);
  if (existsSync(websiteDir)) {
    writeFileSync(websiteProj, outJson, "utf8");
    console.log("[update:pp-round-projections] Wrote", websiteProj);
  }

  console.log("\n[update:pp-round-projections] Done. Hard-refresh the Round projections tab (Ctrl+Shift+R).\n");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
