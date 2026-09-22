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
import { liveProjectionPipelineEnv } from "./projection-pipeline-env.mjs";
import { resolveProjectionPaths } from "./projection-paths.mjs";

Object.assign(
  process.env,
  Object.fromEntries(
    Object.entries(liveProjectionPipelineEnv()).filter(
      ([k]) => process.env[k] === undefined || String(process.env[k]).trim() === "",
    ),
  ),
);

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const PROJECTION_PATHS = resolveProjectionPaths(WEB_ROOT);
const projPath = PROJECTION_PATHS.projectionsPath;
const livePath = PROJECTION_PATHS.liveInPlayPath;

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
  console.log(`[apply:unified-factors] wrote ${PROJECTION_PATHS.projectionsFile}`);
}

main().catch((e) => {
  console.error("[apply:unified-factors]", e?.message || e);
  process.exit(1);
});
