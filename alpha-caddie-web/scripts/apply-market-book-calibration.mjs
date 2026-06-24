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
  loadMarketBookCalibration,
  marketBookCalibrationEnabled,
} from "./market-book-calibration.mjs";
import { num } from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const projPath = join(WEB, "projections.json");

if (!marketBookCalibrationEnabled()) {
  console.log("[apply:market-book-calibration] Skipped (GOLF_MARKET_BOOK_CALIBRATION=0).");
  process.exit(0);
}

const cal = loadMarketBookCalibration(true);
const proj = JSON.parse(readFileSync(projPath, "utf8"));
const par = Math.round(num(proj.meta?.course_par ?? proj.course_par, NaN)) || 72;
let n = 0;
for (const pl of proj.players || []) {
  if (!pl || typeof pl !== "object") continue;
  applyMarketBookCalibrationToRow(pl, par);
  n++;
}
writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`);

const mk = cal.markets || {};
console.log(
  `[apply:market-book-calibration] Applied to ${n} row(s) from ${cal.fit_method || "calibration"} ` +
    `(excludes ${cal.excluded_live_event || "n/a"} from fit).`,
);
for (const [market, m] of Object.entries(mk)) {
  console.log(`  ${market}: μ shift ${m.mu_shift}, σ×${m.sigma_scale}`);
}
