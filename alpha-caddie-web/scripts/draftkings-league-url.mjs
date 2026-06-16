/**
 * Map DataGolf / PGA event titles to DraftKings sportsbook league URL slugs.
 * DK paths are short (e.g. the-memorial-tournament); DG titles often include sponsors.
 */

/** Lowercase stripped title → DK slug when naive slugification fails. */
const DK_SLUG_OVERRIDES = new Map([
  ["pga championship", "uspga-championship"],
  ["the open championship", "the-open-championship"],
  ["the open", "the-open-championship"],
  ["us open", "us-open"],
  ["u s open", "us-open"],
  ["the masters", "us-masters"],
]);

/** Legacy bad slugs written before overrides (e.g. u-s-open from dotted "U.S."). */
const DK_SLUG_ALIASES = new Map([
  ["u-s-open", "us-open"],
  ["u-s-masters", "us-masters"],
]);

function stripEventSponsorSuffix(name) {
  return String(name || "")
    .trim()
    .replace(/\s+presented\s+by\s+.+$/i, "")
    .replace(/\s+sponsored\s+by\s+.+$/i, "")
    .replace(/\s+hosted\s+by\s+.+$/i, "")
    .replace(/\s+benefiting\s+.+$/i, "")
    .trim();
}

function eventTitleLookupKey(name) {
  return stripEventSponsorSuffix(name)
    .toLowerCase()
    .replace(/\./g, "")
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

function normalizeDraftKingsSlug(slug) {
  const s = String(slug || "")
    .trim()
    .toLowerCase()
    .replace(/^-+|-+$/g, "");
  if (!s) return "";
  return DK_SLUG_ALIASES.get(s) || s;
}

/** DraftKings golf league path segment (no URL). */
export function eventNameToDraftKingsSlug(eventName) {
  const key = eventTitleLookupKey(eventName);
  if (DK_SLUG_OVERRIDES.has(key)) return DK_SLUG_OVERRIDES.get(key);
  const slug = key.replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "");
  return normalizeDraftKingsSlug(slug);
}

export function draftKingsLeagueUrlFromSlug(slug) {
  const s = String(slug || "").trim();
  if (!s) return "";
  const lower = s.toLowerCase();
  if (lower === "pga-championship") {
    return "https://sportsbook.draftkings.com/leagues/golf/uspga-championship?category=round";
  }
  return `https://sportsbook.draftkings.com/leagues/golf/${s}?category=round`;
}

/** DK_LEAGUE_URL → explicit slug fields on projections.json → event_name slug. */
export function inferDraftKingsLeagueUrlFromProjections(payload) {
  const envUrl = String(process.env.DK_LEAGUE_URL || "").trim();
  if (envUrl) return envUrl;

  const explicitSlug = normalizeDraftKingsSlug(
    payload?.dk_league_slug || payload?.draftkings_league_slug || payload?.dk_event_slug || "",
  );
  if (explicitSlug) return draftKingsLeagueUrlFromSlug(explicitSlug);

  return draftKingsLeagueUrlFromSlug(eventNameToDraftKingsSlug(payload?.event_name || ""));
}

/** Slug implied by payload (for persisting on projections.json). */
export function inferDraftKingsLeagueSlugFromProjections(payload) {
  const explicitSlug = normalizeDraftKingsSlug(
    payload?.dk_league_slug || payload?.draftkings_league_slug || payload?.dk_event_slug || "",
  );
  if (explicitSlug) return explicitSlug;
  return eventNameToDraftKingsSlug(payload?.event_name || "");
}
