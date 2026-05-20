#!/usr/bin/env node
/**
 * Merge recent seasons into data/historical_rounds_all.csv only (no build:history).
 * Feeds fetch:dg course-history + within-event form (loadEventRoundContextFromHistoricalCsv).
 *
 *   npm run merge:recent-rounds
 *
 * Default: last 2 calendar years from DataGolf API; older CSV rows kept.
 */
import { spawnSync } from "child_process";
import path from "path";
import { fileURLToPath } from "url";
import { applyHistoricalRoundsMergeDefaults } from "./historical-rounds-merge-env.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

const years = String(process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "2").trim();

console.log(
  `[merge:recent-rounds] Merging DataGolf rounds for the last ${years} calendar year(s) into historical_rounds_all.csv (no shard rebuild).`,
);

const r = spawnSync(process.execPath, [path.join(WEB_ROOT, "scripts", "update-historical-rounds-node.mjs")], {
  cwd: WEB_ROOT,
  stdio: "inherit",
  env: applyHistoricalRoundsMergeDefaults({
    ...process.env,
    GOLF_MODEL_DIR: REPO_ROOT,
    GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: years,
    GOLF_UPDATE_ROUNDS_SKIP_BUILD: "1",
  }),
});

process.exit(r.status ?? 1);
