#!/usr/bin/env node
/**
 * Full Historical Trends depth: 2004→present rounds CSV + 2000 rounds/player + all-tour course shards.
 *
 *   npm run rebuild:history-deep
 *
 * Requires DATAGOLF_API_KEY (or datagolf.local.json). Takes ~15–45 min depending on API/disk.
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");

function run(script, label, extraEnv = {}) {
  console.log(`\n[rebuild:history-deep] ${label}…\n`);
  const r = spawnSync(process.execPath, [path.join(WEB, "scripts", script)], {
    cwd: WEB,
    stdio: "inherit",
    env: {
      ...process.env,
      GOLF_MODEL_DIR: REPO,
      ...extraEnv,
    },
  });
  if (r.status !== 0) {
    console.error(`[rebuild:history-deep] ${label} failed (exit ${r.status ?? "?"})`);
    process.exit(r.status ?? 1);
  }
}

const deepEnv = {
  GOLF_HISTORICAL_ROUNDS_FETCH_ALL_YEARS: "1",
  GOLF_HISTORICAL_ROUNDS_FULL_HISTORY: "1",
  GOLF_REFRESH_LIVE_FAST_HISTORY: "0",
  GOLF_REFRESH_APP_FAST_HISTORY: "0",
  GOLF_HISTORY_MIN_YEAR: "2004",
  GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER: "2000",
  GOLF_SKIP_SHOTS_ROUND_AGG_MERGE: "0",
  GOLF_BUILD_HISTORY_SKIP_HOLES: "1",
  HOLE_DATA_CSV: "",
};

console.log(
  "[rebuild:history-deep] Fetching PGA+LIV rounds 2004→present, rebuilding player + course history shards.\n",
);

run("update-historical-rounds-node.mjs", "DataGolf historical rounds → historical_rounds_all.csv", deepEnv);
run("build-player-history.mjs", "Rebuild player_round_history + by-dg shards", {
  ...deepEnv,
  GOLF_SKIP_COURSE_SHARD_WRITE: "1",
});
run("build-course-history-shards.mjs", "All-tour course shards from CSV", deepEnv);

if (!fs.existsSync(path.join(WEB, "player_round_history.json"))) {
  console.error("[rebuild:history-deep] build did not produce player_round_history.json");
  process.exit(1);
}

run("embed-player-history.mjs", "Embed history for static deploy");
run("patch-current-event-history-shards.mjs", "Patch current-event rounds into shards", deepEnv);

console.log("\n[rebuild:history-deep] Done. Hard-refresh the browser (Ctrl+Shift+R).\n");
