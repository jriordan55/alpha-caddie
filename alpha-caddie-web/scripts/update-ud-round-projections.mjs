#!/usr/bin/env node
/**
 * Update Underdog round projections in projections.json (source=underdog).
 *   npm run update:ud-round-projections
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { refreshUnderdogRoundProps } from "./merge-underdog-round-props.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

async function main() {
  if (String(process.env.GOLF_SKIP_UD_OU || "").trim() === "1") {
    console.error("[update:ud-round-projections] GOLF_SKIP_UD_OU=1 — nothing to do.");
    process.exit(0);
  }
  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("[update:ud-round-projections] Missing", projPath);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  if (!payload?.players?.length) {
    console.error("[update:ud-round-projections] projections.json has no players");
    process.exit(1);
  }
  const { props, nUd, udError } = await refreshUnderdogRoundProps(payload);
  if (!nUd) {
    console.error("[update:ud-round-projections] No Underdog rows merged.", udError ? `(${udError})` : "");
    process.exit(1);
  }
  const now = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const next = { ...payload, props, updated_at: now, ud_round_props_refreshed_at: now };
  const outJson = JSON.stringify(next, null, 2);
  writeFileSync(projPath, outJson, "utf8");
  console.log(`[update:ud-round-projections] Wrote ${projPath} — ${nUd} Underdog prop rows`);
  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  if (existsSync(dirname(websiteProj))) writeFileSync(websiteProj, outJson, "utf8");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
