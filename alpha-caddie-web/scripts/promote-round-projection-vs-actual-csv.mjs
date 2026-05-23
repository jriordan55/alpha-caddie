#!/usr/bin/env node
/**
 * After export-round-projection-vs-actual-csv.mjs, copy round_projection_vs_actual.csv.new
 * over the main CSV when Excel left the target locked. Exits 1 if promotion fails.
 */
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { ensureRoundProjectionCsvPublished } from "./export-round-projection-vs-actual-csv.mjs";

const WEB_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const CSV = join(WEB_ROOT, "data", "round_projection_vs_actual.csv");

try {
  ensureRoundProjectionCsvPublished(CSV);
} catch (e) {
  console.error(e?.message || e);
  process.exit(1);
}
