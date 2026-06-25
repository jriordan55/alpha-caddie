#!/usr/bin/env node
/**
 * Apply honest DK book-alignment shifts to projections.json (post export + fit).
 *   npm run apply:market-book-calibration
 */
import { readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import {
  applyMarketBookCalibrationToRow,
  applyEventPropBookAlignment,
  loadMarketBookCalibration,
  marketBookCalibrationEnabled,
} from "./market-book-calibration.mjs";
import { readCoursePar18, repairProjectionScoreParCoherence, syncProjectionPlayerCoursePar } from "./projection-course-par.mjs";
import { num } from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const projPath = join(WEB, "projections.json");

if (!marketBookCalibrationEnabled()) {
  console.log("[apply:market-book-calibration] Skipped (GOLF_MARKET_BOOK_CALIBRATION=0).");
  process.exit(0);
}

const cal = loadMarketBookCalibration(true);
const proj = JSON.parse(readFileSync(projPath, "utf8"));
const par = readCoursePar18(proj);
if (!Number.isFinite(par)) {
  console.error("[apply:market-book-calibration] FAIL: missing course_par_18 / hole_pars — run ensure:projection-course-par first");
  process.exit(1);
}
let n = 0;
for (const pl of proj.players || []) {
  if (!pl || typeof pl !== "object") continue;
  applyMarketBookCalibrationToRow(pl, par);
  n++;
}
const propAlign = applyEventPropBookAlignment(proj, { coursePar18: par });
syncProjectionPlayerCoursePar(proj, par);
const { fixed } = repairProjectionScoreParCoherence(proj, par);
if (!proj.meta || typeof proj.meta !== "object") proj.meta = {};
proj.meta.market_book_calibration = {
  generated_at: cal.generated_at,
  fit_method: cal.fit_method,
  markets: cal.markets,
};
writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`);

const mk = cal.markets || {};
console.log(
  `[apply:market-book-calibration] Applied to ${n} row(s) from ${cal.fit_method || "calibration"} ` +
    `(excludes ${cal.excluded_live_event || "n/a"} from fit).` +
    (fixed ? ` Repaired ${fixed} score↔par row(s).` : ""),
);
for (const [market, m] of Object.entries(mk)) {
  console.log(`  ${market}: μ shift ${m.mu_shift}, σ×${m.sigma_scale}`);
}
if (propAlign.applied) {
  for (const [market, m] of Object.entries(propAlign.markets)) {
    console.log(
      `  ${market} (event props): μ shift ${m.mu_shift}  n=${m.n_pairs}  meanΔ ${m.mean_delta}`,
    );
  }
}
