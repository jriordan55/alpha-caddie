/**
 * Course par + fairway-hole scale from hole cards (pars-based; not a fixed 72/14).
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { fairwayOpportunitiesFromCoursePar } from "./pga-tour-market-benchmarks.mjs";

export const N_FAIRWAY_HOLES_DEFAULT = 14;

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function foldKey(s) {
  return normCourseNameKey(String(s || "").trim());
}

/** Fairway opportunities = par-4 + par-5 holes when all 18 pars are valid 3–5. */
export function fairwayHoleCountFromPars(pars, fallback = N_FAIRWAY_HOLES_DEFAULT) {
  if (!Array.isArray(pars) || pars.length !== 18) return fallback;
  let n = 0;
  for (const p of pars) {
    const v = Math.round(num(p, NaN));
    if (!Number.isFinite(v) || v < 3 || v > 5) return fallback;
    if (v === 4 || v === 5) n++;
  }
  if (n < 1) return fallback;
  return n;
}

export function coursePar18FromHolePars(pars, fallback = 72) {
  if (!Array.isArray(pars) || pars.length !== 18) return fallback;
  let sum = 0;
  for (const p of pars) {
    const v = Math.round(num(p, NaN));
    if (!Number.isFinite(v) || v < 3 || v > 5) return fallback;
    sum += v;
  }
  return sum >= 63 && sum <= 76 ? sum : fallback;
}

function loadCourseHolesMaps(webRoot) {
  const p = join(webRoot, "course_holes.json");
  if (!existsSync(p)) return { by_course: {}, by_event: {} };
  try {
    const j = JSON.parse(readFileSync(p, "utf8"));
    return { by_course: j.by_course || {}, by_event: j.by_event || {} };
  } catch {
    return { by_course: {}, by_event: {} };
  }
}

/** Lookup bundled 18-hole par card from course_holes.json. */
export function lookupBundledHolePars(courseUsed, eventName, webRoot = defaultWebRoot()) {
  const maps = loadCourseHolesMaps(webRoot);
  const ck = foldKey(courseUsed);
  const ek = foldKey(eventName);
  for (const [k, pars] of Object.entries(maps.by_course || {})) {
    const fk = foldKey(k);
    if (fk === ck || (ck && (ck.includes(fk) || fk.includes(ck)))) {
      if (Array.isArray(pars) && pars.length === 18) {
        return { pars: pars.map((p) => Math.round(num(p, 4))), source: "course_holes_json" };
      }
    }
  }
  for (const [k, pars] of Object.entries(maps.by_event || {})) {
    const fk = foldKey(k);
    if (fk === ek || (ek && (ek.includes(fk) || fk.includes(ek)))) {
      if (Array.isArray(pars) && pars.length === 18) {
        return { pars: pars.map((p) => Math.round(num(p, 4))), source: "course_holes_event" };
      }
    }
  }
  return null;
}

function defaultWebRoot() {
  return join(dirname(fileURLToPath(import.meta.url)), "..");
}

function normalizeHolePars(holePars) {
  if (!Array.isArray(holePars) || holePars.length !== 18) return null;
  const out = holePars.map((p) => Math.round(num(p, NaN)));
  if (out.some((v) => !Number.isFinite(v) || v < 3 || v > 5)) return null;
  return out;
}

/**
 * Resolve course_par_18 and fairway_holes_modeled from hole pars (preferred) or par heuristic.
 */
export function resolveCourseLayout({
  holePars = null,
  coursePar18 = NaN,
  courseUsed = "",
  eventName = "",
  webRoot = defaultWebRoot(),
} = {}) {
  const fromPayload = normalizeHolePars(holePars);
  const bundled = fromPayload ? null : lookupBundledHolePars(courseUsed, eventName, webRoot);
  const pars = fromPayload || bundled?.pars || null;
  const par18 = pars
    ? coursePar18FromHolePars(pars)
    : Math.round(num(coursePar18, NaN)) || 72;
  const fairwayHoles = pars
    ? fairwayHoleCountFromPars(pars)
    : fairwayOpportunitiesFromCoursePar(par18);
  return {
    hole_pars: pars,
    hole_pars_source: fromPayload ? "payload" : bundled?.source || null,
    course_par_18: par18,
    fairway_holes_modeled: fairwayHoles,
  };
}

/** Bake resolved layout into projections.json (meta + basis). */
export function syncCourseLayoutIntoProjection(payload, webRoot = defaultWebRoot()) {
  if (!payload || typeof payload !== "object") return null;
  const layout = resolveCourseLayout({
    holePars: payload.hole_pars,
    coursePar18: payload.course_par_18 ?? payload.meta?.course_par_18,
    courseUsed: payload.course_used ?? payload.meta?.course_used,
    eventName: payload.event_name ?? payload.meta?.event_name,
    webRoot,
  });
  if (layout.hole_pars && !normalizeHolePars(payload.hole_pars)) {
    payload.hole_pars = layout.hole_pars;
  }
  payload.course_par_18 = layout.course_par_18;
  if (payload.meta && typeof payload.meta === "object") {
    payload.meta.course_par_18 = layout.course_par_18;
  }
  const basis =
    (payload.meta?.projection_course_basis && typeof payload.meta.projection_course_basis === "object"
      ? payload.meta.projection_course_basis
      : null) ||
    (payload.projection_course_basis && typeof payload.projection_course_basis === "object"
      ? payload.projection_course_basis
      : null);
  if (basis) {
    basis.fairway_holes_modeled = layout.fairway_holes_modeled;
    if (payload.meta) payload.meta.projection_course_basis = basis;
    payload.projection_course_basis = basis;
  }
  return layout;
}
