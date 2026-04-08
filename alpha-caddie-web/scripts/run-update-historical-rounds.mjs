#!/usr/bin/env node
/**
 * npm run update:rounds — refresh data/historical_rounds_all.csv via DataGolf (Node; PGA + LIV).
 * Needs DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json (same as fetch:dg).
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const roundsScript = path.join(WEB_ROOT, "scripts", "update-historical-rounds-node.mjs");

function loadApiKey() {
  const env = (process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = path.join(WEB_ROOT, "datagolf.local.json");
  if (fs.existsSync(p)) {
    try {
      const j = JSON.parse(fs.readFileSync(p, "utf8"));
      return String(j.apiKey || j.key || "").trim();
    } catch {
      return "";
    }
  }
  return "";
}

if (!fs.existsSync(roundsScript)) {
  console.error("Missing:", roundsScript);
  process.exit(1);
}

const key = loadApiKey();
if (!key) {
  console.error("Set DATAGOLF_API_KEY or create datagolf.local.json with apiKey.");
  process.exit(1);
}

const result = spawnSync(process.execPath, [roundsScript], {
  stdio: "inherit",
  cwd: WEB_ROOT,
  env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
});

const code = result.status ?? 1;
if (code !== 0) process.exit(code);

const buildHist = path.join(WEB_ROOT, "scripts", "build-player-history.mjs");
const embedHist = path.join(WEB_ROOT, "scripts", "embed-player-history.mjs");
const buildShots = path.join(WEB_ROOT, "scripts", "build-player-shots-web.mjs");
console.log("Rebuilding player_round_history.json + app.js embed …");
const h = spawnSync(process.execPath, [buildHist], {
  cwd: WEB_ROOT,
  stdio: "inherit",
  env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
});
if (h.status !== 0) process.exit(h.status ?? 1);
if (fs.existsSync(embedHist)) {
  const e = spawnSync(process.execPath, [embedHist], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: process.env,
  });
  if (e.status !== 0) process.exit(e.status ?? 1);
}
if (fs.existsSync(buildShots)) {
  const s = spawnSync(process.execPath, [buildShots], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
  });
  if (s.status !== 0) process.exit(s.status ?? 1);
}
mirrorModelDataToWeb(REPO_ROOT, WEB_ROOT);
process.exit(0);
