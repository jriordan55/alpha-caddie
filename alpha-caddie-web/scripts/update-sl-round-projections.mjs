#!/usr/bin/env node
/**
 * Update Sleeper round projections in projections.json (source=sleeper).
 *   npm run update:sl-round-projections
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { refreshSleeperRoundProps } from "./merge-sleeper-round-props.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

async function main() {
  if (String(process.env.GOLF_SKIP_SL_OU || "").trim() === "1") {
    console.error("[update:sl-round-projections] GOLF_SKIP_SL_OU=1 — nothing to do.");
    process.exit(0);
  }
  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("[update:sl-round-projections] Missing", projPath);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  if (!payload?.players?.length) {
    console.error("[update:sl-round-projections] projections.json has no players");
    process.exit(1);
  }
  const { props, nSl, slError } = await refreshSleeperRoundProps(payload);
  if (!nSl) {
    console.error("[update:sl-round-projections] No Sleeper rows merged.", slError ? `(${slError})` : "");
    process.exit(1);
  }
  const now = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const next = { ...payload, props, updated_at: now, sl_round_props_refreshed_at: now };
  const outJson = JSON.stringify(next, null, 2);
  writeFileSync(projPath, outJson, "utf8");
  console.log(`[update:sl-round-projections] Wrote ${projPath} — ${nSl} Sleeper prop rows`);
  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  if (existsSync(dirname(websiteProj))) writeFileSync(websiteProj, outJson, "utf8");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
