#!/usr/bin/env node
/**
 * npm run build:history:pga — finds Rscript on Windows without PATH, then runs the PGA history R script.
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { findRscriptSync } from "./find-rscript.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const rScript = path.join(REPO_ROOT, "scripts", "build_alpha_caddie_web_history_pga.R");

if (!fs.existsSync(rScript)) {
  console.error("Missing:", rScript);
  process.exit(1);
}

const rscript = findRscriptSync();
console.log("Using:", rscript);

const result = spawnSync(rscript, [rScript, REPO_ROOT], {
  stdio: "inherit",
  cwd: WEB_ROOT,
  env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
});

let code = result.status ?? 1;
if (code !== 0 && rscript === "Rscript") {
  console.error(`
Rscript was not found. Either:
  - Add R's bin folder to your PATH (e.g. C:\\Program Files\\R\\R-4.x.x\\bin), or
  - Set R_HOME to that R install, or
  - Set RSCRIPT_PATH to the full path of Rscript.exe
`);
}
if (code !== 0) process.exit(code);

const embedHist = path.join(WEB_ROOT, "scripts", "embed-player-history.mjs");
if (fs.existsSync(embedHist)) {
  const e = spawnSync(process.execPath, [embedHist], { cwd: WEB_ROOT, stdio: "inherit", env: process.env });
  code = e.status ?? 1;
}
process.exit(code);
