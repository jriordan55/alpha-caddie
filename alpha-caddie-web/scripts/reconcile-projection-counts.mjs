#!/usr/bin/env node
/**
 * Final pass: tie bird/bog/par/GIR/FW to total_score for every projection row.
 * Runs at end of push:live so weather, pin sheet, and unified factors cannot leave par-heavy profiles.
 */
import { readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { reconcileAllProjectionPlayerRows } from "./course-round-adjustments.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const projPath = join(WEB, "projections.json");

const proj = JSON.parse(readFileSync(projPath, "utf8"));
const { reconciled } = reconcileAllProjectionPlayerRows(proj, {
  skipFieldCalibrate: true,
});
writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`);
console.log(`[reconcile:projection-counts] OK — ${reconciled} row(s) score-anchored (bird/bog/par from total)`);
