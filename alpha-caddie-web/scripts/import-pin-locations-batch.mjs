#!/usr/bin/env node
/**
 * Import all batch_*.json files from data/pin_locations/batches/ into the DB.
 *   npm run import:pin-locations:batch
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { spawnSync } from "child_process";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = path.resolve(WEB_ROOT, "..");
const BATCH_DIR = path.join(MODEL_ROOT, "data", "pin_locations", "batches");
const SAVE = path.join(WEB_ROOT, "scripts", "save-pin-sheets-batch.mjs");

if (!fs.existsSync(BATCH_DIR)) {
  console.log("[import:pin-locations:batch] No batches dir:", BATCH_DIR);
  process.exit(0);
}

const files = fs
  .readdirSync(BATCH_DIR)
  .filter((f) => /^batch_\d+\.json$/i.test(f))
  .sort();

if (!files.length) {
  console.log("[import:pin-locations:batch] No batch_*.json files");
  process.exit(0);
}

for (const f of files) {
  const p = path.join(BATCH_DIR, f);
  console.log(`\n[import:pin-locations:batch] ${f}…`);
  const r = spawnSync(process.execPath, [SAVE, p], { cwd: WEB_ROOT, stdio: "inherit" });
  if (r.status !== 0) process.exit(r.status ?? 1);
}

console.log("\n[import:pin-locations:batch] All batches imported.");
