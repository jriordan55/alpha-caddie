#!/usr/bin/env node
/**
 * After export-round-projection-vs-actual-csv.mjs, copy round_projection_vs_actual.csv.new
 * over the main CSV when Excel left the target locked. Exits 1 if promotion fails.
 */
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { ensureRoundProjectionArtifactsPublished } from "./export-round-projection-vs-actual-csv.mjs";

const WEB_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const CSV = join(WEB_ROOT, "data", "round_projection_vs_actual.csv");
const SUMMARY = join(WEB_ROOT, "data", "round_projection_vs_actual_summary.csv");
const XLSX = join(WEB_ROOT, "data", "round_projection_vs_actual.xlsx");

try {
  ensureRoundProjectionArtifactsPublished(CSV, SUMMARY, XLSX);
} catch (e) {
  console.warn(String(e?.message || e));
  process.exit(0);
}
