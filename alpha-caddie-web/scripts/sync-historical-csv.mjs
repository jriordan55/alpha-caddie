#!/usr/bin/env node
/**
 * Copies repo data/*.csv mirrors into alpha-caddie-web/data/ (and historical → repo root).
 * Uses mirror-model-data-to-web.mjs for web copies, then mirrors historical to repo root
 * when possible (may warn if the root file is locked).
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const src = path.join(REPO_ROOT, "data", "historical_rounds_all.csv");
const dstRoot = path.join(REPO_ROOT, "historical_rounds_all.csv");

if (!fs.existsSync(src)) {
  console.error("Missing:", src);
  process.exit(1);
}

mirrorModelDataToWeb(REPO_ROOT, WEB_ROOT);

try {
  fs.copyFileSync(src, dstRoot);
  console.log("OK:", dstRoot);
} catch (e) {
  console.warn("Could not write root copy (file may be open):", dstRoot, e.message || e);
  process.exitCode = 2;
}
