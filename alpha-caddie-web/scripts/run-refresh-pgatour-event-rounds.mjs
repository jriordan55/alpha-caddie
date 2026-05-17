#!/usr/bin/env node
/**
 * npm run refresh:pgatour-event — pgatouR scorecards for projections.json current event only.
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { findRscriptSync } from "./find-rscript.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const rScript = path.join(REPO_ROOT, "scripts", "refresh_pgatour_event_rounds.R");

if (!fs.existsSync(rScript)) {
  console.error("[refresh:pgatour-event] Missing:", rScript);
  process.exit(1);
}

const rscript = findRscriptSync();
if (rscript === "Rscript" && process.platform === "win32") {
  console.warn(
    "[refresh:pgatour-event] Rscript not found — skip pgatouR event refresh (set RSCRIPT_PATH or install R).",
  );
  process.exit(0);
}

console.log("[refresh:pgatour-event] Using:", rscript);
const result = spawnSync(rscript, [rScript, REPO_ROOT], {
  stdio: "inherit",
  cwd: WEB_ROOT,
  env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
});

if (result.status !== 0) {
  console.warn(
    "[refresh:pgatour-event] R script failed — push:all continues without pgatour_event_rounds.json (install R + pgatouR to enable).",
  );
}
process.exit(0);
