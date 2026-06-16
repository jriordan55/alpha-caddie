#!/usr/bin/env node
/**
 * If projections still have generic/wrong hole pars, apply bundled course_holes.json before publish.
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { recalcProjectionScoresForCoursePar, reconcileAllProjectionPlayerRows } from "./course-round-adjustments.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function foldKey(s) {
  return normCourseNameKey(String(s || "").trim());
}

function loadCourseHolesMaps() {
  const p = join(WEB_ROOT, "course_holes.json");
  if (!existsSync(p)) return { by_course: {}, by_event: {} };
  try {
    const j = JSON.parse(readFileSync(p, "utf8"));
    return { by_course: j.by_course || {}, by_event: j.by_event || {} };
  } catch {
    return { by_course: {}, by_event: {} };
  }
}

function lookupBundledPars(courseUsed, eventName) {
  const maps = loadCourseHolesMaps();
  const ck = foldKey(courseUsed);
  const ek = foldKey(eventName);
  for (const [k, pars] of Object.entries(maps.by_course || {})) {
    if (foldKey(k) === ck || ck.includes(foldKey(k)) || foldKey(k).includes(ck)) {
      if (Array.isArray(pars) && pars.length === 18) return { pars, source: "course_holes_json" };
    }
  }
  for (const [k, pars] of Object.entries(maps.by_event || {})) {
    if (foldKey(k) === ek || ek.includes(foldKey(k))) {
      if (Array.isArray(pars) && pars.length === 18) return { pars, source: "course_holes_event" };
    }
  }
  return null;
}

if (!existsSync(projPath)) {
  console.log("[sync:hole-par] no projections.json — skip");
  process.exit(0);
}

let proj;
try {
  proj = JSON.parse(readFileSync(projPath, "utf8"));
} catch (e) {
  console.warn("[sync:hole-par] parse error —", e.message || e);
  process.exit(0);
}

const bundled = lookupBundledPars(proj.course_used, proj.event_name);
if (!bundled) {
  console.log("[sync:hole-par] no bundled hole card for this venue — skip");
  process.exit(0);
}

const newPars = bundled.pars.map((p) => Math.round(num(p, 4)));
const newPar = newPars.reduce((s, p) => s + p, 0);
const oldPar = Math.round(num(proj.course_par_18, NaN));
const src = String(proj.hole_pars_source || "").trim().toLowerCase();
const prevJson = JSON.stringify(proj.hole_pars);

if (prevJson === JSON.stringify(newPars) && oldPar === newPar && src !== "generic") {
  console.log("[sync:hole-par] projections already match bundled hole card");
  process.exit(0);
}

if (src === "live_hole_stats" && oldPar === newPar && prevJson === JSON.stringify(newPars)) {
  process.exit(0);
}

const oldParFinite = Number.isFinite(oldPar) ? oldPar : newPar;
proj.hole_pars = newPars;
proj.course_par_18 = newPar;
proj.hole_pars_source = bundled.source;
if (oldParFinite !== newPar) {
  const { rows } = recalcProjectionScoresForCoursePar(proj, newPar, oldParFinite);
  reconcileAllProjectionPlayerRows(proj);
  console.log(`[sync:hole-par] course_par ${oldParFinite} → ${newPar} (${bundled.source}), ${rows} row(s)`);
} else {
  console.log(`[sync:hole-par] hole_pars updated (${bundled.source}), par ${newPar}`);
}

writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
