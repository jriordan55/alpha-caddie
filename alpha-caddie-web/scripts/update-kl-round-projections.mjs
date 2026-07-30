#!/usr/bin/env node
/**
 * Update Kalshi round-score projections in projections.json (source=kalshi).
 *   npm run update:kl-round-projections
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { refreshKalshiRoundProps } from "./merge-kalshi-round-props.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

async function main() {
  if (String(process.env.GOLF_SKIP_KL_OU || "").trim() === "1") {
    console.error("[update:kl-round-projections] GOLF_SKIP_KL_OU=1 — nothing to do.");
    process.exit(0);
  }
  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("[update:kl-round-projections] Missing", projPath);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  if (!payload?.players?.length) {
    console.error("[update:kl-round-projections] projections.json has no players");
    process.exit(1);
  }
  const { props, nKl, klError } = await refreshKalshiRoundProps(payload);
  if (!nKl) {
    console.error("[update:kl-round-projections] No Kalshi rows merged.", klError ? `(${klError})` : "");
    process.exit(1);
  }
  const now = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const next = { ...payload, props, updated_at: now, kl_round_props_refreshed_at: now };
  const outJson = JSON.stringify(next, null, 2);
  writeFileSync(projPath, outJson, "utf8");
  console.log(`[update:kl-round-projections] Wrote ${projPath} — ${nKl} Kalshi prop rows`);
  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  if (existsSync(dirname(websiteProj))) writeFileSync(websiteProj, outJson, "utf8");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
