#!/usr/bin/env node
/**
 * Final pass: tie bird/bog/par/GIR/FW to total_score for every projection row.
 * DK book-alignment is applied later via apply:market-book-calibration (after vs-actual export + fit).
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { reconcileAllProjectionPlayerRows, flatVenuePlayerScoreAnchorEnabled } from "./course-round-adjustments.mjs";
import { liveProjectionPipelineEnv } from "./projection-pipeline-env.mjs";

Object.assign(
  process.env,
  Object.fromEntries(
    Object.entries(liveProjectionPipelineEnv()).filter(
      ([k]) => process.env[k] === undefined || String(process.env[k]).trim() === "",
    ),
  ),
);

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const projPath = join(WEB, "projections.json");

const proj = JSON.parse(readFileSync(projPath, "utf8"));
const nostradamusPath = join(WEB, "data", "gamedaymath_nostradamus_props.json");
if (existsSync(nostradamusPath)) {
  try {
    const raw = JSON.parse(readFileSync(nostradamusPath, "utf8"));
    const rows = Array.isArray(raw) ? raw : Array.isArray(raw?.props) ? raw.props : [];
    const round = Math.round(Number(proj.display_round || proj.meta?.display_round || 1)) || 1;
    const priors = rows
      .filter((row) => Math.round(Number(row.round_num ?? row.display_round ?? round)) === round)
      .map((row) => ({ ...row, source: "gamedaymath" }));
    if (priors.length) proj.props = [...(proj.props || []), ...priors];
  } catch (error) {
    console.warn(`[reconcile:projection-counts] Ignoring invalid Nostradamus prior file: ${error.message}`);
  }
}
const { reconciled, calibrated, venueScoreCalibrated } = reconcileAllProjectionPlayerRows(proj, {
  minField: 8,
  skipHistVenueScoreCalibrate: flatVenuePlayerScoreAnchorEnabled(),
  skipMarketBookCalibration: true,
  // Never pull μ toward sportsbook / sharp lines — model process only.
  applyBayesianMarketCalibration: false,
  skipEventPropBookAlignment: true,
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
