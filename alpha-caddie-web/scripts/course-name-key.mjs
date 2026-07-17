/**
 * Shared course-name normalization (must match app.js `normCourseNameKey`).
 */
const COURSE_NAME_CANONICAL_KEYS = {
  albany: "albany golf club",
  "albany bahamas": "albany golf club",
  "sea island resort": "sea island golf club",
  "royal birkdale": "royal birkdale golf club",
  "royal birkdale gc": "royal birkdale golf club",
};

export function normCourseNameKey(raw) {
  let s = String(raw || "").trim().toLowerCase();
  s = s.replace(/\([^)]*\)/g, " ");
  s = s.replace(/\b(blue monster|stadium course|championship course|club de golf)\b/g, " ");
  s = s.replace(/&/g, " and ");
  s = s.replace(/\bthe players\b/gi, " ");
  s = s.replace(/\bc\.?\s*c\.?\b/gi, "country club");
  s = s.replace(/\bg\.?\s*c\.?\b/gi, "golf club");
  s = s.replace(/\bg\.?\s*l\.?\b/gi, "golf links");
  s = s.replace(/\bgolf club(\s+golf club)+\b/gi, "golf club");
  s = s.replace(/\bcountry club(\s+country club)+\b/gi, "country club");
  s = s.replace(/\bgolf links(\s+golf links)+\b/gi, "golf links");
  s = s.replace(/[^a-z0-9]+/g, " ");
  s = s.replace(/\s+/g, " ").trim();
  const alias = COURSE_NAME_CANONICAL_KEYS[s];
  return alias || s;
}

/** Display labels: title-case normalized keys; keep **TPC** uppercase (not "Tpc" / "tpc"). */
export function enforceCourseDisplayAcronyms(label) {
  return String(label || "")
    .replace(/\b(tpc)\b/gi, "TPC")
    .replace(/\(tpc\b/gi, "(TPC")
    .replace(/\btpc-/gi, "TPC-");
}

export function formatCourseLabelForDisplay(raw) {
  const k = normCourseNameKey(raw);
  if (!k) return enforceCourseDisplayAcronyms(String(raw || "").trim());
  return enforceCourseDisplayAcronyms(k.replace(/\b\w/g, (c) => c.toUpperCase()));
}

export function courseShardFileName(courseKey) {
  const safe = String(courseKey || "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 96);
  return `${safe || "unknown"}.json`;
}
