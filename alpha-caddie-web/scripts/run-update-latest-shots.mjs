#!/usr/bin/env node
/**
 * npm run refresh:shots — pgatouR backfill for data/all_shots_2022_2026.csv (same folder as
 * historical_rounds_all.csv): tournaments after the file’s latest schedule end date through today,
 * using PGA schedules only for seasons from the earliest year in the CSV (≥ 2022) through the current year.
 * R: scripts/update_latest_shots.R → helper.R::backfill_shots_after_csv_anchor / pga_shot_details.
 * Then rebuilds data/all_shots_2022_2026_round_fairways_gir_putts.csv (Python), mirrors model CSVs
 * into alpha-caddie-web/data/, and rebuilds player_shots_web.json when build-player-shots-web.mjs exists.
 *
 * Skipped when GOLF_SKIP_SHOTS_UPDATE=1.
 * Skips the fairways/GIR aggregate only when GOLF_SKIP_SHOTS_AGGREGATE=1 (shots + mirror still run).
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { findRscriptSync } from "./find-rscript.mjs";
import { findPythonArgsPrefix } from "./find-python.mjs";
import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const rScript = path.join(REPO_ROOT, "scripts", "update_latest_shots.R");

if (process.env.GOLF_SKIP_SHOTS_UPDATE === "1") {
  console.log("[update-latest-shots] GOLF_SKIP_SHOTS_UPDATE=1 — skip.");
  process.exit(0);
}

if (!fs.existsSync(rScript)) {
  console.error("Missing:", rScript);
  process.exit(1);
}

const rscript = findRscriptSync();
console.log("Using:", rscript);
const result = spawnSync(rscript, [rScript, REPO_ROOT], {
  stdio: "inherit",
  cwd: REPO_ROOT,
  env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
});

const code = result.status ?? 1;
if (code !== 0) process.exit(code);

const aggScript = path.join(REPO_ROOT, "scripts", "build_shots_round_aggregate.py");
if (process.env.GOLF_SKIP_SHOTS_AGGREGATE === "1") {
  console.log("[update-latest-shots] GOLF_SKIP_SHOTS_AGGREGATE=1 — skip fairways/GIR aggregate.");
} else if (!fs.existsSync(aggScript)) {
  console.error("[update-latest-shots] Missing aggregate script:", aggScript);
  process.exit(1);
} else {
  const env = { ...process.env, GOLF_MODEL_DIR: REPO_ROOT };
  const py = findPythonArgsPrefix({ cwd: REPO_ROOT, env });
  if (!py) {
    console.error(
      "[update-latest-shots] Python 3 not found (tried py -3, python3, python). Install Python 3 or set GOLF_SKIP_SHOTS_AGGREGATE=1."
    );
    process.exit(1);
  }
  const args = [...py.argsPrefix, aggScript];
  const last = spawnSync(py.cmd, args, { stdio: "inherit", cwd: REPO_ROOT, env });
  if ((last.status ?? 1) !== 0) process.exit(last.status ?? 1);
}

mirrorModelDataToWeb(REPO_ROOT, WEB_ROOT, { includeAllShotsCsv: true });

const buildShots = path.join(WEB_ROOT, "scripts", "build-player-shots-web.mjs");
if (fs.existsSync(buildShots)) {
  const s = spawnSync(process.execPath, [buildShots], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
  });
  if (s.status !== 0) process.exit(s.status ?? 1);
}

process.exit(0);
