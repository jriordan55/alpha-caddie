/**
 * Shared helpers for pick'em round O/U books (PrizePicks-style: Sleeper, Underdog).
 */
import { impliedProbFromAmerican } from "./round-projection-mu.mjs";

export const ROUND_OU_MARKETS = new Set([
  "Total Score",
  "Birdies",
  "Pars",
  "Bogeys",
  "GIR",
  "Fairways hit",
  "Putts",
]);

/** Underdog / pick'em hole-pack O/U markets (not round counting props). */
export const HOLE_PACK_OU_MARKETS = new Set(["Holes 10-18", "Holes 16-17-18"]);

/**
 * Map provider labels → hole-pack markets (10–18 back nine, closing three).
 * @param {string} raw
 */
export function canonicalHolePackOuMarket(raw) {
  const s = String(raw || "").trim();
  if (!s) return "";
  // Keep hyphens for hole ranges; only collapse underscores / whitespace.
  const low = s
    .toLowerCase()
    .replace(/_+/g, " ")
    .replace(/\s+/g, " ")
    .replace(/[–—]/g, "-")
    .trim();
  if (
    /holes?\s*16\s*(?:\/|,|&|and|-)?\s*17\s*(?:\/|,|&|and|-)?\s*18/.test(low) ||
    /16\s*-\s*17\s*-\s*18/.test(low) ||
    /closing\s*three/.test(low) ||
    /last\s*3\s*holes?/.test(low) ||
    /holes?\s*16\s*to\s*18/.test(low)
  ) {
    return "Holes 16-17-18";
  }
  if (
    /holes?\s*10\s*(?:-|to|thru|through)\s*18/.test(low) ||
    /10\s*-\s*18/.test(low) ||
    /holes?\s*10\s+18/.test(low) ||
    /back\s*nine/.test(low) ||
    /holes?\s*10\s*to\s*18/.test(low) ||
    /back\s*9/.test(low)
  ) {
    return "Holes 10-18";
  }
  return "";
}

/**
 * Map provider stat / wager labels → our Round Projections market names.
 * @param {string} raw
 */
export function canonicalRoundOuMarket(raw) {
  const s = String(raw || "").trim();
  if (!s) return "";
  const low = s.toLowerCase().replace(/[_-]+/g, " ").replace(/\s+/g, " ").trim();
  // Prefer hole packs before generic "score" / "stroke" matches.
  if (canonicalHolePackOuMarket(raw)) return "";
  if (
    low.includes("stroke") ||
    low === "score" ||
    low.includes("total score") ||
    low === "strokes" ||
    low.includes("round score")
  ) {
    return "Total Score";
  }
  if (low.includes("bird")) return "Birdies";
  if (low.includes("bogey") || low.includes("bogeys")) return "Bogeys";
  if (low === "pars" || low.startsWith("par ") || low === "par") return "Pars";
  if (low.includes("green") || low === "gir") return "GIR";
  if (low.includes("fairway")) return "Fairways hit";
  if (low.includes("putt")) return "Putts";
  return "";
}

/** Decimal payout multiplier (e.g. 1.82) → American odds. */
export function americanFromPayoutMultiplier(mult) {
  const d = Number(mult);
  if (!Number.isFinite(d) || d <= 1) return NaN;
  if (d >= 2) return Math.round((d - 1) * 100);
  return Math.round(-100 / (d - 1));
}

export function parseAmericanOdds(raw) {
  const s = String(raw ?? "").trim().replace(/^\+/, "");
  if (!s) return NaN;
  const n = Number(s);
  return Number.isFinite(n) && n !== 0 ? Math.round(n) : NaN;
}

/**
 * Attach implied probs from American odds (when both sides present).
 * @param {object} prop
 */
export function withImpliedFromAmerican(prop) {
  if (!prop || typeof prop !== "object") return prop;
  const o = Number(prop.over_odds);
  const u = Number(prop.under_odds);
  const out = { ...prop };
  if (Number.isFinite(o)) out.p_over_implied = impliedProbFromAmerican(o);
  if (Number.isFinite(u)) out.p_under_implied = impliedProbFromAmerican(u);
  return out;
}

export function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

export function parseRoundFromText(...parts) {
  for (const raw of parts) {
    const s = String(raw || "");
    const m = s.match(/\bRD\s*(\d+)\b/i) || s.match(/\bR\s*(\d+)\b/i) || s.match(/\bRound\s+(\d+)\b/i);
    if (m) {
      const r = Math.round(num(m[1], NaN));
      if (r >= 1 && r <= 4) return r;
    }
  }
  return NaN;
}

export function propRowHasPostableLine(r) {
  return (
    Number.isFinite(num(r?.line, NaN)) &&
    Number.isFinite(num(r?.over_odds, NaN)) &&
    Number.isFinite(num(r?.under_odds, NaN))
  );
}

/**
 * Keep prior book rows when a refresh of another book runs.
 * @param {object} payload
 * @param {object[]} otherProps
 * @param {string} source
 */
export function preserveBookSourceProps(payload, otherProps, source) {
  const want = String(source || "").trim().toLowerCase();
  const prior = (Array.isArray(payload?.props) ? payload.props : []).filter(
    (r) => String(r?.source || "").trim().toLowerCase() === want && propRowHasPostableLine(r),
  );
  if (!prior.length) return otherProps;
  return [...otherProps, ...prior];
}

/**
 * Replace one book source in props array.
 * @param {object[]} allProps
 * @param {object[]} bookProps
 * @param {string} source
 */
export function mergeBookSourceIntoProps(allProps, bookProps, source) {
  const want = String(source || "").trim().toLowerCase();
  const prior = (Array.isArray(allProps) ? allProps : []).filter(
    (r) => String(r?.source || "").trim().toLowerCase() !== want,
  );
  const fresh = (Array.isArray(bookProps) ? bookProps : [])
    .map((r) => ({ ...r, source: want }))
    .filter(propRowHasPostableLine);
  return [...prior, ...fresh];
}

export function preferPropsForTargetRound(props, targetRound) {
  const want = Math.round(num(targetRound, NaN));
  if (!Number.isFinite(want) || want < 1 || want > 4) return props;
  const numbered = props.filter((r) => Number.isFinite(Math.round(num(r.round_num, NaN))));
  if (!numbered.length) return props;
  const forRound = numbered.filter((r) => Math.round(num(r.round_num, NaN)) === want);
  if (forRound.length) return forRound;
  const unnumbered = props.filter((r) => !Number.isFinite(Math.round(num(r.round_num, NaN))));
  return [...unnumbered, ...numbered];
}

/** One row per player×market (keep first). */
export function dedupePropsOnePerPlayerMarket(props) {
  const seen = new Set();
  const out = [];
  for (const r of props || []) {
    const dg = Math.round(num(r.dg_id, NaN));
    const name = String(r.player_name || "").trim().toLowerCase();
    const mkt = String(r.market || "").trim();
    const key = `${Number.isFinite(dg) && dg > 0 ? dg : name}|${mkt}`;
    if (!mkt || seen.has(key)) continue;
    seen.add(key);
    out.push(r);
  }
  return out;
}
