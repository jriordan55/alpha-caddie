/**
 * PrizePicks round O/U line sanity — reject milestone/goblin alts (e.g. Birdies 0.5)
 * when the real counting line is ~3.5–4.5. Shared by fetch, merge, validate, and UI.
 */

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

/** @type {Record<string, { min: number, max: number }>} */
export const PP_LINE_SANITY = {
  "Total Score": { min: 60, max: 85 },
  Birdies: { min: 1.5, max: 8.5 },
  Pars: { min: 6, max: 14.5 },
  Bogeys: { min: 1.5, max: 8.5 },
  GIR: { min: 5, max: 16.5 },
  "Fairways hit": { min: 3, max: 14.5 },
};

/** Higher is better; negative = reject. */
export function ppLineSanityScore(market, line) {
  const band = PP_LINE_SANITY[String(market || "").trim()];
  const v = num(line, NaN);
  if (!band || !Number.isFinite(v)) return -1;
  if (v >= band.min && v <= band.max) return 2;
  return -1;
}

export function ppLineIsSane(market, line) {
  return ppLineSanityScore(market, line) > 0;
}

function ppPropGroupKey(r) {
  const mk = String(r?.market || "").trim();
  const rnd = Math.round(num(r?.round_num, NaN));
  const rk = Number.isFinite(rnd) && rnd >= 1 && rnd <= 4 ? `|R${rnd}` : "";
  const id = Math.round(num(r?.dg_id, NaN));
  if (Number.isFinite(id) && id > 0) return `${id}|${mk}${rk}`;
  const name = String(r?.player_name || "")
    .trim()
    .toLowerCase();
  return `nm:${name}|${mk}${rk}`;
}

function dkRefIndex(dkProps) {
  const map = new Map();
  for (const r of dkProps || []) {
    if (String(r?.source || "").trim().toLowerCase() !== "draftkings") continue;
    map.set(ppPropGroupKey(r), r);
  }
  return map;
}

function pickBestPpRow(rows, dkRef) {
  if (!rows.length) return null;
  if (rows.length === 1) return rows[0];
  return [...rows].sort((a, b) => {
    const sa = ppLineSanityScore(a.market, a.line);
    const sb = ppLineSanityScore(b.market, b.line);
    if (sb !== sa) return sb - sa;
    const dkLine = num(dkRef?.line, NaN);
    if (Number.isFinite(dkLine)) {
      const ga = Math.abs(num(a.line, NaN) - dkLine);
      const gb = Math.abs(num(b.line, NaN) - dkLine);
      if (ga !== gb) return ga - gb;
    }
    return num(b.line, NaN) - num(a.line, NaN);
  })[0];
}

/**
 * One PP row per player+market+round; prefer sane counting lines over milestone alts.
 * @param {object[]} ppProps
 * @param {object[]} [dkProps] DraftKings rows for tie-break / line alignment
 */
export function dedupePpPropsOnePerPlayerMarket(ppProps, dkProps = []) {
  const dkIndex = dkRefIndex(dkProps);
  const groups = new Map();
  for (const r of ppProps || []) {
    if (String(r?.source || "").trim().toLowerCase() !== "prizepicks") continue;
    const key = ppPropGroupKey(r);
    if (!groups.has(key)) groups.set(key, []);
    groups.get(key).push(r);
  }
  const out = [];
  for (const [key, rows] of groups) {
    const best = pickBestPpRow(rows, dkIndex.get(key));
    if (best) out.push(best);
  }
  return out;
}

/**
 * Align insane PP lines to DK when books post the same market (fixed PP odds, wrong milestone line).
 * Drop PP rows that remain insane with no DK anchor.
 * @param {object[]} ppProps
 * @param {object[]} dkProps
 */
export function reconcilePpPropsWithDraftKings(ppProps, dkProps = []) {
  const dkIndex = dkRefIndex(dkProps);
  const out = [];
  for (const r of ppProps || []) {
    const row = { ...r };
    const dk = dkIndex.get(ppPropGroupKey(row));
    if (!ppLineIsSane(row.market, row.line)) {
      const dkLine = num(dk?.line, NaN);
      if (dk && ppLineIsSane(row.market, dkLine)) {
        row.line = dkLine;
      } else {
        continue;
      }
    }
    if (dk && ppLineIsSane(row.market, row.line)) {
      const gap = Math.abs(num(row.line, NaN) - num(dk.line, NaN));
      if (gap > 3) {
        row.line = num(dk.line, row.line);
      }
    }
    out.push(row);
  }
  return out;
}

/** Dedupe then reconcile — use before merging PP into projections.props. */
export function sanitizePpRoundProps(ppProps, allNonPpProps = []) {
  const dkProps = (allNonPpProps || []).filter(
    (r) => String(r?.source || "").trim().toLowerCase() === "draftkings",
  );
  let rows = dedupePpPropsOnePerPlayerMarket(ppProps, dkProps);
  rows = reconcilePpPropsWithDraftKings(rows, dkProps);
  return rows;
}

/**
 * Validate merged projections.props — returns error strings (empty = OK).
 * @param {object[]} props full projections.props
 */
export function validatePpOuLinesInProps(props) {
  const errors = [];
  const dkIndex = dkRefIndex(
    (props || []).filter((r) => String(r?.source || "").trim().toLowerCase() === "draftkings"),
  );
  const ppRows = (props || []).filter((r) => String(r?.source || "").trim().toLowerCase() === "prizepicks");
  const seen = new Map();
  for (const r of ppRows) {
    const key = ppPropGroupKey(r);
    if (seen.has(key)) {
      errors.push(`duplicate PP ${r.market} for ${r.player_name || key}`);
      continue;
    }
    seen.set(key, r);
    if (!ppLineIsSane(r.market, r.line)) {
      errors.push(`PP ${r.market} line ${r.line} for ${r.player_name || "?"} outside sane range`);
      continue;
    }
    const dk = dkIndex.get(key);
    if (dk) {
      const gap = Math.abs(num(r.line, NaN) - num(dk.line, NaN));
      if (gap > 3) {
        errors.push(
          `PP ${r.market} line ${r.line} vs DK ${dk.line} for ${r.player_name || "?"} (gap ${gap.toFixed(1)})`,
        );
      }
    }
  }
  return errors;
}
