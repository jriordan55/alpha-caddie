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
  ["the masters", "us-masters"],
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

/** DraftKings golf league path segment (no URL). */
export function eventNameToDraftKingsSlug(eventName) {
  const stripped = stripEventSponsorSuffix(eventName);
  const key = stripped.toLowerCase();
  if (DK_SLUG_OVERRIDES.has(key)) return DK_SLUG_OVERRIDES.get(key);
  const slug = key
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
  return slug || "";
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

  const explicitSlug = String(
    payload?.dk_league_slug || payload?.draftkings_league_slug || payload?.dk_event_slug || "",
  ).trim();
  if (explicitSlug) return draftKingsLeagueUrlFromSlug(explicitSlug);

  return draftKingsLeagueUrlFromSlug(eventNameToDraftKingsSlug(payload?.event_name || ""));
}

/** Slug implied by payload (for persisting on projections.json). */
export function inferDraftKingsLeagueSlugFromProjections(payload) {
  const explicitSlug = String(
    payload?.dk_league_slug || payload?.draftkings_league_slug || payload?.dk_event_slug || "",
  ).trim();
  if (explicitSlug) return explicitSlug;
  return eventNameToDraftKingsSlug(payload?.event_name || "");
}
