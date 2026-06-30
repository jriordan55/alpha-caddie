/**
 * Book-like O/U props from projections.json (DK scrape, CSV, or model fallback when DK is blocked).
 */
export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

/** Sources the tracker + vs-actual export treat as posted lines (not walk-forward model-only). */
export function isBookLikePropSource(source) {
  const s = String(source || "").trim().toLowerCase();
  return s === "draftkings" || s === "model_fallback" || s === "csv";
}

/**
 * Map `${dg}|${round}|${market}` → { line, over, under, source }.
 * @param {object} payload projections.json
 * @param {{ round?: number, markets?: Set<string> }} [opts]
 */
export function buildBookPropsIndex(payload, opts = {}) {
  const map = new Map();
  const roundFilter = Math.round(num(opts.round, NaN));
  const displayRound = Math.round(
    num(opts.displayRound ?? payload?.display_round ?? payload?.meta?.display_round, 1),
  ) || 1;
  const markets = opts.markets instanceof Set ? opts.markets : null;
  for (const r of Array.isArray(payload?.props) ? payload.props : []) {
    if (!isBookLikePropSource(r.source)) continue;
    const market = String(r.market || "").trim();
    if (markets && !markets.has(market)) continue;
    const dg = Math.round(num(r.dg_id, NaN));
    let rnd = Math.round(num(r.round_num ?? r.display_round, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) rnd = displayRound;
    const line = num(r.line, NaN);
    const over = num(r.over_odds, NaN);
    const under = num(r.under_odds, NaN);
    if (!Number.isFinite(dg) || !market) continue;
    if (Number.isFinite(roundFilter) && rnd !== roundFilter) continue;
    if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;
    const src = String(r.source || "").trim().toLowerCase();
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
