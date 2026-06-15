#!/usr/bin/env node
/**
 * Promote round_projection_vs_actual*.new → main files after Excel locked them during export.
 *   npm run promote:round-projection-vs-actual
 */
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { ensureRoundProjectionArtifactsPublished } from "./export-round-projection-vs-actual-csv.mjs";

const WEB_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const CSV = join(WEB_ROOT, "data", "round_projection_vs_actual.csv");
const SUMMARY = join(WEB_ROOT, "data", "round_projection_vs_actual_summary.csv");
const XLSX = join(WEB_ROOT, "data", "round_projection_vs_actual.xlsx");

const pub = ensureRoundProjectionArtifactsPublished(CSV, SUMMARY, XLSX);
if (pub.lockedCount > 0) {
  console.warn(
    `[promote] ${pub.lockedCount} file(s) still locked — close Excel/editor and run again.`,
  );
  process.exit(1);
}
console.log("[promote] All round projection vs actual artifacts published.");
