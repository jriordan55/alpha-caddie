/**
 * Course/venue display labels (keep in sync with alpha-caddie-web/scripts/course-name-key.mjs).
 */
export function enforceCourseDisplayAcronyms(label: string): string {
  return String(label || "")
    .replace(/\b(tpc)\b/gi, "TPC")
    .replace(/\(tpc\b/gi, "(TPC")
    .replace(/\btpc-/gi, "TPC-");
}

export function formatCourseLabelForDisplay(raw: string): string {
  const k = normCourseNameKey(raw);
  if (!k) return enforceCourseDisplayAcronyms(String(raw || "").trim());
  return enforceCourseDisplayAcronyms(
    k.replace(/\b\w/g, (c) => c.toUpperCase()),
  );
}

function normCourseNameKey(raw: string): string {
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
  return s;
}
