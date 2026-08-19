/**
 * Book-like O/U props from projections.json (DK scrape, CSV, or model fallback when DK is blocked).
 * Browser-safe: do not import round-projection-mu.mjs (it pulls Node `fs`).
 */

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

/** DraftKings half-point buckets (same as round-projection-mu.enforceHalfLine). */
function enforceHalfLine(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x)) return NaN;
  return Math.round(x * 2) / 2;
}

/** Sources the tracker + vs-actual export treat as posted DK lines (not PP). */
export function isBookLikePropSource(source) {
  const s = String(source || "").trim().toLowerCase();
  return s === "draftkings" || s === "model_fallback" || s === "csv";
}

export function isPrizePicksPropSource(source) {
  return String(source || "").trim().toLowerCase() === "prizepicks";
}

export function isSleeperPropSource(source) {
  return String(source || "").trim().toLowerCase() === "sleeper";
}

export function isUnderdogPropSource(source) {
  return String(source || "").trim().toLowerCase() === "underdog";
}

export function isFanDuelPropSource(source) {
  return String(source || "").trim().toLowerCase() === "fanduel";
}

export function isCaesarsPropSource(source) {
  return String(source || "").trim().toLowerCase() === "caesars";
}

export function isKalshiPropSource(source) {
  return String(source || "").trim().toLowerCase() === "kalshi";
}

function normalizeIndexedPropLine(source, line) {
  const n = num(line, NaN);
  if (!Number.isFinite(n)) return NaN;
  if (
    isPrizePicksPropSource(source) ||
    isSleeperPropSource(source) ||
    isUnderdogPropSource(source)
  ) {
    return n;
  }
  return enforceHalfLine(n);
}

/**
 * Map `${dg}|${round}|${market}` → { line, over, under, source } for one book source.
 * @param {object} payload projections.json
 * @param {{ round?: number, markets?: Set<string>, source?: string }} [opts]
 */
function buildPropsIndexForSource(payload, opts = {}) {
  const map = new Map();
  const roundFilter = Math.round(num(opts.round, NaN));
  const displayRound = Math.round(
    num(opts.displayRound ?? payload?.display_round ?? payload?.meta?.display_round, 1),
  ) || 1;
  const markets = opts.markets instanceof Set ? opts.markets : null;
  const wantSource = String(opts.source || "").trim().toLowerCase();
  for (const r of Array.isArray(payload?.props) ? payload.props : []) {
    const src = String(r.source || "").trim().toLowerCase();
    if (wantSource) {
      if (src !== wantSource) continue;
    } else if (!isBookLikePropSource(r.source)) {
      continue;
    }
    const market = String(r.market || "").trim();
    if (markets && !markets.has(market)) continue;
    const dg = Math.round(num(r.dg_id, NaN));
    let rnd = Math.round(num(r.round_num ?? r.display_round, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) rnd = displayRound;
    const line = normalizeIndexedPropLine(src, r.line);
    const over = num(r.over_odds, NaN);
    const under = num(r.under_odds, NaN);
    if (!Number.isFinite(dg) || !market) continue;
    if (Number.isFinite(roundFilter) && rnd !== roundFilter) continue;
    if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;
    map.set(`${dg}|${rnd}|${market}`, {
      line,
      over,
      under,
      source: src,
      player_name: r.player_name,
    });
  }
  return map;
}

/**
 * Map `${dg}|${round}|${market}` → { line, over, under, source }.
 * @param {object} payload projections.json
 * @param {{ round?: number, markets?: Set<string> }} [opts]
 */
export function buildBookPropsIndex(payload, opts = {}) {
  return buildPropsIndexForSource(payload, opts);
}

/** PrizePicks-only index (same keys as buildBookPropsIndex). */
export function buildPpPropsIndex(payload, opts = {}) {
  return buildPropsIndexForSource(payload, { ...opts, source: "prizepicks" });
}

/** Sleeper-only index. */
export function buildSlPropsIndex(payload, opts = {}) {
  return buildPropsIndexForSource(payload, { ...opts, source: "sleeper" });
}

/** Underdog-only index. */
export function buildUdPropsIndex(payload, opts = {}) {
  return buildPropsIndexForSource(payload, { ...opts, source: "underdog" });
}

/** FanDuel-only index. */
export function buildFdPropsIndex(payload, opts = {}) {
  return buildPropsIndexForSource(payload, { ...opts, source: "fanduel" });
}

/** Caesars-only index. */
export function buildCzrPropsIndex(payload, opts = {}) {
  return buildPropsIndexForSource(payload, { ...opts, source: "caesars" });
}

/** Kalshi-only index. */
export function buildKlPropsIndex(payload, opts = {}) {
  return buildPropsIndexForSource(payload, { ...opts, source: "kalshi" });
}
