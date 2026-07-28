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

/**
 * Pull North/South/East/West course side before stripping parentheses so
 * "Club (North Course)" and "Club (South Course)" stay distinct keys.
 * @param {string} s lowercased raw
 * @returns {string} e.g. "north course" or ""
 */
export function extractCourseSideToken(s) {
  const t = String(s || "").toLowerCase();
  const inParen = t.match(/\(\s*(north|south|east|west)\s*(course)?\s*\)/i);
  if (inParen) return `${inParen[1].toLowerCase()} course`;
  const outside = t.match(/\b(north|south|east|west)\s+course\b/i);
  if (outside) return `${outside[1].toLowerCase()} course`;
  // Short forms: "Torrey Pines (South)", "Firestone CC (South)"
  const shortParen = t.match(/\(\s*(north|south|east|west)\s*\)/i);
  if (shortParen) return `${shortParen[1].toLowerCase()} course`;
  return "";
}

export function normCourseNameKey(raw) {
  let s = String(raw || "").trim().toLowerCase();
  const side = extractCourseSideToken(s);
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
  // Drop "north course" tokens then re-append canonical side (do not strip bare "east"/"west"
  // — that would break East Lake, etc.).
  s = s.replace(/\b(north|south|east|west)\s+course\b/g, " ");
  s = s.replace(/[^a-z0-9]+/g, " ");
  s = s.replace(/\s+/g, " ").trim();
  if (side) s = `${s} ${side}`.replace(/\s+/g, " ").trim();
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
