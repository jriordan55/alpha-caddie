#!/usr/bin/env node
/**
 * Final pass: tie bird/bog/par/GIR/FW to total_score for every projection row.
 * DK book-alignment is applied later via apply:market-book-calibration (after vs-actual export + fit).
 */
import { readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { reconcileAllProjectionPlayerRows, flatVenuePlayerScoreAnchorEnabled } from "./course-round-adjustments.mjs";
import { flatVenueProjectionPipelineEnv } from "./projection-pipeline-env.mjs";

Object.assign(process.env, flatVenueProjectionPipelineEnv());

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const projPath = join(WEB, "projections.json");

const proj = JSON.parse(readFileSync(projPath, "utf8"));
const { reconciled, calibrated } = reconcileAllProjectionPlayerRows(proj, {
  minField: 8,
  skipHistVenueScoreCalibrate: flatVenuePlayerScoreAnchorEnabled(),
  skipMarketBookCalibration: true,
});
writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`);
console.log(
  `[reconcile:projection-counts] OK — ${reconciled} row(s) score-anchored; field markets ${calibrated?.rounds ?? 0} round(s) (book cal deferred)`,
);
