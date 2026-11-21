/**
 * Shared course-name normalization (must match app.js `normCourseNameKey`).
 */
const COURSE_NAME_CANONICAL_KEYS = {
  albany: "albany golf club",
  "albany bahamas": "albany golf club",
  "sea island resort": "sea island golf club",
  "royal birkdale": "royal birkdale golf club",
  "royal birkdale gc": "royal birkdale golf club",
  // Baycurrent history is "Yokohama Country Club". The live feed says West Course
  // (16 West holes + 2 East holes). Same venue, same rounds.
  "yokohama country club west course": "yokohama country club",
};

/**
 * Pull course side before stripping parentheses so dual venues stay distinct
 * ("Sea Island (Seaside Course)" vs "(Plantation Course)", North/South, etc.).
 * @param {string} s lowercased raw
 * @returns {string} e.g. "north course", "seaside", or ""
 */
export function extractCourseSideToken(s) {
  const t = String(s || "").toLowerCase();
  const named =
    t.match(/\(\s*(seaside|plantation|ocean|resort|links|north|south|east|west)\s*(course)?\s*\)/i) ||
    t.match(/\b(seaside|plantation|ocean|resort|links|north|south|east|west)\s+course\b/i) ||
    t.match(/\(\s*(seaside|plantation|ocean|resort|links|north|south|east|west)\s*\)/i);
  if (!named) return "";
  const side = named[1].toLowerCase();
  if (side === "seaside" || side === "plantation" || side === "ocean" || side === "resort" || side === "links") {
    return side;
  }
  return `${side} course`;
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
  const s = String(raw || "").trim();
  if (!s) return "";
  // Dual-course weeks use " · " / ";" — format each side so we never collapse to
  // "Sea Island Golf Club Sea Island Golf Club" (which broke Open-Meteo coords).
  if (s.includes(" · ") || s.includes(";")) {
    const parts = s
      .split(/\s*[·;]\s*/)
      .map((part) => formatCourseLabelForDisplay(part.trim()) || part.trim())
      .filter(Boolean);
    const uniq = [...new Set(parts.map((p) => p.toLowerCase()))];
    if (uniq.length === 1 && parts.length > 1) return parts[0];
    return parts.join(" · ");
  }
  const k = normCourseNameKey(s);
  if (!k) return enforceCourseDisplayAcronyms(s);
  return enforceCourseDisplayAcronyms(k.replace(/\b\w/g, (c) => c.toUpperCase()));
}

export function courseShardFileName(courseKey) {
  const safe = String(courseKey || "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 96);
  return `${safe || "unknown"}.json`;
}
