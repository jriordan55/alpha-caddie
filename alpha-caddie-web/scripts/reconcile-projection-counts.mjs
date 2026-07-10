#!/usr/bin/env node
/**
 * Final pass: tie bird/bog/par/GIR/FW to total_score for every projection row.
 * DK book-alignment is applied later via apply:market-book-calibration (after vs-actual export + fit).
 */
import { readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { reconcileAllProjectionPlayerRows, flatVenuePlayerScoreAnchorEnabled } from "./course-round-adjustments.mjs";
import { liveProjectionPipelineEnv } from "./projection-pipeline-env.mjs";

Object.assign(process.env, liveProjectionPipelineEnv());

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const projPath = join(WEB, "projections.json");

const proj = JSON.parse(readFileSync(projPath, "utf8"));
const { reconciled, calibrated, venueScoreCalibrated } = reconcileAllProjectionPlayerRows(proj, {
  minField: 8,
  skipHistVenueScoreCalibrate: flatVenuePlayerScoreAnchorEnabled(),
  skipMarketBookCalibration: true,
  displayRound: proj.display_round,
});
writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`);
const venueShifts = venueScoreCalibrated?.shifts || {};
const venueShiftNote =
  Object.keys(venueShifts).length > 0
    ? `; venue total-score ${Object.entries(venueShifts)
        .map(([r, s]) => `R${r}${s >= 0 ? "+" : ""}${s}`)
        .join(", ")}`
    : "";
console.log(
  `[reconcile:projection-counts] OK — ${reconciled} row(s) score-anchored; field markets ${calibrated?.rounds ?? 0} round(s) (book cal deferred)${venueShiftNote}`,
);
