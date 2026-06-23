#!/usr/bin/env node
/**
 * Apply unified projection factors (course fit, tee wave, bounce-back, Sunday pressure,
 * per-round weather, player residuals) and reconcile correlated markets.
 *
 *   npm run apply:unified-factors
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { applyUnifiedProjectionFactors } from "./projection-unified-factors.mjs";
import { flattenProjectionExportMeta } from "./projection-export-meta.mjs";
import { flatVenueProjectionPipelineEnv } from "./projection-pipeline-env.mjs";

Object.assign(process.env, flatVenueProjectionPipelineEnv());

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");
const livePath = join(WEB_ROOT, "live-in-play.json");

function readJson(p) {
  return JSON.parse(readFileSync(p, "utf8"));
}

async function main() {
  if (!existsSync(projPath)) {
    console.warn("[apply:unified-factors] missing projections.json — skip");
    process.exit(0);
  }
  const proj = readJson(projPath);
  let liveBundle = null;
  if (existsSync(livePath)) {
    try {
      liveBundle = readJson(livePath);
    } catch (e) {
      console.warn("[apply:unified-factors] could not read live-in-play.json:", e.message || e);
    }
  }
  await applyUnifiedProjectionFactors(proj, { liveBundle });
  flattenProjectionExportMeta(proj);
  writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`, "utf8");
  console.log("[apply:unified-factors] wrote projections.json");
}

main().catch((e) => {
  console.error("[apply:unified-factors]", e?.message || e);
  process.exit(1);
});
