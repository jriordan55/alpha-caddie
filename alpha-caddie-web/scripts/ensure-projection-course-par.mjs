#!/usr/bin/env node
/**
 * Lock course_par_18 from hole card before scoring steps; repair total_score ↔ score_to_par.
 *   npm run ensure:projection-course-par
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { ensureProjectionCoursePar } from "./projection-course-par.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const projPath = join(WEB, "projections.json");

function envTruthy(name) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return false;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

if (!existsSync(projPath)) {
  console.error("[ensure:projection-course-par] missing projections.json");
  process.exit(1);
}

const proj = JSON.parse(readFileSync(projPath, "utf8"));
const res = ensureProjectionCoursePar(proj, { failOnMismatch: envTruthy("GOLF_FAIL_ON_PAR_MISMATCH") });

if (!res.ok) {
  console.error(`[ensure:projection-course-par] FAIL: ${res.reason}`);
  process.exit(1);
}

proj.updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`);

const parts = [`par ${res.coursePar18}`];
if (res.recalcRows) parts.push(`recalc ${res.recalcRows} row(s)`);
if (res.fixed) parts.push(`repaired ${res.fixed} score(s)`);
if (res.stamped) parts.push(`${res.stamped} player course_par stamped`);
console.log(`[ensure:projection-course-par] OK — ${parts.join("; ")}`);
