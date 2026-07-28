/**
 * Dual-course venues (Detroit North/South, Torrey, etc.): canonical labels + event routing.
 * Keep North/South outside ambiguous collapsing — pair with course-name-key.normCourseNameKey
 * which preserves side tokens from "(North Course)" / "North Course".
 */

import { normCourseNameKey } from "./course-name-key.mjs";

/** DataGolf course_num → canonical display name (side outside parentheses). */
export const COURSE_NUM_CANONICAL_NAME = Object.freeze({
  // Detroit Golf Club — 2026 Rocket Classic restored North routing
  947: "Detroit Golf Club North Course",
  // Legacy Rocket Mortgage / Rocket Classic tournament routing (pre-restoration)
  876: "Detroit Golf Club",
  // Torrey Pines
  104: "Torrey Pines Golf Course North Course",
  4: "Torrey Pines Golf Course South Course",
  939: "Torrey Pines Golf Course South Course",
});

/** 2026 Rocket Classic North hole pars (PGA Tour First Look / local coverage). */
export const DETROIT_NORTH_HOLE_PARS_2026 = Object.freeze([
  4, 4, 4, 5, 3, 4, 4, 4, 3, 4, 3, 4, 4, 5, 3, 4, 4, 4,
]);

/**
 * @param {string|null|undefined} courseName
 * @param {number|string|null|undefined} courseNum
 * @returns {string}
 */
export function canonicalizeCourseName(courseName, courseNum) {
  const n = Math.round(Number(courseNum));
  if (Number.isFinite(n) && COURSE_NUM_CANONICAL_NAME[n]) {
    return COURSE_NUM_CANONICAL_NAME[n];
  }
  const raw = String(courseName || "").trim();
  if (!raw) return raw;
  const lower = raw.toLowerCase();
  if (lower.includes("detroit")) {
    if (/\bsouth\b/.test(lower)) return "Detroit Golf Club South Course";
    if (/\bnorth\b/.test(lower)) return "Detroit Golf Club North Course";
  }
  if (lower.includes("torrey")) {
    if (/\bsouth\b/.test(lower)) return "Torrey Pines Golf Course South Course";
    if (/\bnorth\b/.test(lower)) return "Torrey Pines Golf Course North Course";
  }
  return raw;
}

/**
 * Collect distinct tee-time course_num values from a field-updates style payload.
 * @param {object|null|undefined} fieldRaw
 * @returns {Set<number>}
 */
export function courseNumsFromFieldRaw(fieldRaw) {
  /** @type {Set<number>} */
  const out = new Set();
  const field = fieldRaw?.field || fieldRaw?.players || [];
  if (!Array.isArray(field)) return out;
  for (const pl of field) {
    for (const t of pl?.teetimes || pl?.tee_times || []) {
      const n = Math.round(Number(t?.course_num ?? t?.courseNum));
      if (Number.isFinite(n) && n > 0) out.add(n);
    }
  }
  const top = Math.round(Number(fieldRaw?.course_num ?? fieldRaw?.courseNum));
  if (Number.isFinite(top) && top > 0) out.add(top);
  return out;
}

/**
 * Resolve the week’s course label so North/South are not collapsed into a parent club name.
 * @param {{ eventName?: string, courseUsed?: string, fieldRaw?: object }} opts
 */
export function resolveEventCourseUsed(opts = {}) {
  const eventName = String(opts.eventName || "").trim();
  const courseUsed = String(opts.courseUsed || "").trim();
  const fieldRaw = opts.fieldRaw || null;
  const ek = normCourseNameKey(eventName);
  const nums = courseNumsFromFieldRaw(fieldRaw);

  // Rocket Classic / Rocket Mortgage Classic @ Detroit
  if (/\brocket\b/.test(ek) && (/\bclassic\b/.test(ek) || /\bmortgage\b/.test(ek))) {
    if (nums.has(947) || (!nums.size && /detroit/i.test(courseUsed || ""))) {
      return "Detroit Golf Club North Course";
    }
    if (nums.has(876) && !nums.has(947)) {
      return canonicalizeCourseName(courseUsed || "Detroit Golf Club", 876);
    }
    if (/detroit/i.test(courseUsed) && /\bnorth\b/i.test(courseUsed)) {
      return "Detroit Golf Club North Course";
    }
    if (/detroit/i.test(courseUsed) && /\bsouth\b/i.test(courseUsed)) {
      return "Detroit Golf Club South Course";
    }
  }

  // Generic: prefer course_num canonical when a single course is posted
  if (nums.size === 1) {
    const only = [...nums][0];
    if (COURSE_NUM_CANONICAL_NAME[only]) return COURSE_NUM_CANONICAL_NAME[only];
  }

  return canonicalizeCourseName(courseUsed, nums.size === 1 ? [...nums][0] : NaN) || courseUsed;
}

/**
 * Canonical normalized key for a historical / live round row (course_num aware).
 * @param {{ course_name?: string, Course_Name?: string, course_num?: number|string, courseNum?: number|string }} row
 */
export function histCourseKeyFromRow(row) {
  if (!row || typeof row !== "object") return "";
  return normCourseNameKey(
    canonicalizeCourseName(row.course_name || row.Course_Name || "", row.course_num ?? row.courseNum),
  );
}

/**
 * Related-but-distinct club siblings (do not pool for venue anchors).
 * @param {string} courseKey normalized key
 * @returns {string[]} sibling normalized keys
 */
export function dualCourseSiblingKeys(courseKey) {
  const k = normCourseNameKey(courseKey);
  if (!k) return [];
  if (k.startsWith("detroit golf club")) {
    return [
      "detroit golf club",
      "detroit golf club north course",
      "detroit golf club south course",
    ].filter((x) => x !== k);
  }
  if (k.startsWith("torrey pines")) {
    return [
      "torrey pines golf course north course",
      "torrey pines golf course south course",
      "torrey pines north course",
      "torrey pines south course",
    ].filter((x) => x !== k);
  }
  return [];
}
