#!/usr/bin/env node
/**
 * Fail fast when projections.json still has the generic hole-par fallback.
 * validate-projections-for-publish rejects generic pars at the very end of refresh:live —
 * running this right after fetch:dg turns a 15-minute-late exit-code-1 into an immediate,
 * actionable error naming the venue that needs a course_holes.json entry.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const projPath = join(WEB_ROOT, "projections.json");

if (!existsSync(projPath)) {
  console.error("[check:hole-pars] missing projections.json — run fetch:dg first");
  process.exit(1);
}

let proj;
try {
  proj = JSON.parse(readFileSync(projPath, "utf8"));
} catch (e) {
  console.error(`[check:hole-pars] could not parse projections.json — ${e.message || e}`);
  process.exit(1);
}

const src = String(proj.hole_pars_source || "").trim().toLowerCase();
const course = String(proj.course_used || "?").trim();
const event = String(proj.event_name || "?").trim();
const par = Math.round(Number(proj.course_par_18)) || "?";

if (src === "generic") {
  console.error(
    `[check:hole-pars] FAIL: no hole card for "${course}" (${event}) — projections are on the generic par-71 fallback.\n` +
      `  Fix: add the venue's 18 hole pars to alpha-caddie-web/course_holes.json under by_course ("${course.toLowerCase()}")\n` +
      `  and/or by_event ("${event.toLowerCase()}"), then re-run npm run push:live.\n` +
      `  (DataGolf live-hole-stats only provides pars once the event is in play, so new venues need a bundled card.)`,
  );
  process.exit(1);
}

console.log(`[check:hole-pars] OK — par ${par} (${src || "?"}) for ${course}`);
