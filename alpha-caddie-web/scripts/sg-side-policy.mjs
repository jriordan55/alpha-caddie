/**
 * Prior-round signals → allowed O/U side by market.
 * Birdies = prior BoB%; GIR = prior GIR%; others use SG categories.
 */

export const SG_STRONG = 0.35;
export const SG_WEAK = -0.15;

/** ~4+ BoB per round; ~2 or fewer BoB. */
export const BOB_PCT_STRONG = 0.22;
export const BOB_PCT_WEAK = 0.11;

/** ~12+ GIR; ~10 or fewer GIR. */
export const GIR_PCT_STRONG = 12 / 18;
export const GIR_PCT_WEAK = 10 / 18;

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

/** GIR as 0–1 rate (handles count or fraction). */
export function girPctFromHistRow(row) {
  const girRaw = num(row?.gir, NaN);
  if (!Number.isFinite(girRaw)) return NaN;
  if (girRaw <= 1) return girRaw;
  return girRaw / 18;
}

/** Birdie-or-better rate = (birdies + eagles) / 18. */
export function bobPctFromHistRow(row) {
  if (!row || typeof row !== "object") return NaN;
  const bird = num(row.birdies, NaN);
  const eob = num(row.eagles_or_better, NaN);
  const eag = num(row.eagles, NaN);
  const eagleAdd = Number.isFinite(eob) ? eob : Number.isFinite(eag) ? eag : 0;
  if (!Number.isFinite(bird) && !Number.isFinite(eob) && !Number.isFinite(eag)) return NaN;
  return ((Number.isFinite(bird) ? bird : 0) + Math.max(0, eagleAdd)) / 18;
}

/** @returns {"strong"|"weak"|"neutral"|null} */
export function sgTier(v) {
  const n = num(v, NaN);
  if (!Number.isFinite(n)) return null;
  if (n >= SG_STRONG) return "strong";
  if (n <= SG_WEAK) return "weak";
  return "neutral";
}

/** @returns {"strong"|"weak"|"neutral"|null} */
export function rateTier(v, strong, weak) {
  const n = num(v, NaN);
  if (!Number.isFinite(n)) return null;
  if (n >= strong) return "strong";
  if (n <= weak) return "weak";
  return "neutral";
}

/** @param {Record<string, unknown>} row */
export function priorSgFromRow(row = {}) {
  return priorSignalsFromRow(row);
}

/** @param {Record<string, unknown>} row */
export function priorSignalsFromRow(row = {}) {
  return {
    prev_sg_ott: num(row.prev_sg_ott, NaN),
    prev_sg_app: num(row.prev_sg_app, NaN),
    prev_sg_putt: num(row.prev_sg_putt, NaN),
    prev_gir_pct: num(row.prev_gir_pct, NaN),
    prev_bob_pct: num(row.prev_bob_pct, NaN),
  };
}

/** Build prior signals from a raw historical round row (live enrichment). */
export function priorSignalsFromHistRow(row = {}) {
  return {
    prev_sg_ott: num(row.sg_ott, NaN),
    prev_sg_app: num(row.sg_app, NaN),
    prev_sg_putt: num(row.sg_putt, NaN),
    prev_gir_pct: girPctFromHistRow(row),
    prev_bob_pct: bobPctFromHistRow(row),
  };
}

/**
 * @returns {{ over: boolean, under: boolean, reason: string }}
 */
function sidesFromTier(tier, overTier, underTier, label) {
  if (tier === overTier) {
    return { over: true, under: false, reason: `${label}: ${tier}` };
  }
  if (tier === underTier) {
    return { over: false, under: true, reason: `${label}: ${underTier}` };
  }
  return { over: false, under: false, reason: `${label}: ${tier ?? "missing"}` };
}

/**
 * Map market + prior-round signals to allowed bet sides.
 *
 * @param {string} market
 * @param {object} signals
 */
export function sgAllowedSides(market, signals = {}) {
  const ott = sgTier(signals.prev_sg_ott);
  const app = sgTier(signals.prev_sg_app);
  const putt = sgTier(signals.prev_sg_putt);
  const bob = rateTier(signals.prev_bob_pct, BOB_PCT_STRONG, BOB_PCT_WEAK);
  const gir = rateTier(signals.prev_gir_pct, GIR_PCT_STRONG, GIR_PCT_WEAK);

  switch (String(market || "").trim()) {
    case "Total score":
      return sidesFromTier(app, "weak", "strong", "prior SG approach");
    case "Birdies":
      return sidesFromTier(bob, "strong", "weak", "prior BoB%");
    case "Bogeys":
      return sidesFromTier(app, "weak", "strong", "prior SG approach");
    case "Fairways hit":
      return sidesFromTier(ott, "strong", "weak", "prior SG off the tee");
    case "GIR":
      return sidesFromTier(gir, "strong", "weak", "prior GIR%");
    case "Pars": {
      if (app === "strong" && putt === "weak") {
        return { over: true, under: false, reason: "prior strong APP + weak PUTT" };
      }
      if (app === "weak" || putt === "strong") {
        return { over: false, under: true, reason: "prior weak APP or strong PUTT" };
      }
      return { over: false, under: false, reason: "prior SG neutral for pars" };
    }
    default:
      return { over: true, under: true, reason: "" };
  }
}

/**
 * @param {number} edgeOver
 * @param {number} edgeUnder
 * @param {number} minEvPct
 * @param {string} market
 * @param {Record<string, unknown>} row
 */
export function pickBetSideWithSgPolicy(edgeOver, edgeUnder, minEvPct, market, row = {}) {
  const signals = priorSignalsFromRow(row);
  const allowed = sgAllowedSides(market, signals);
  const th = num(minEvPct, 0);
  /** @type {{ side: "over"|"under", edge: number }[]} */
  const candidates = [];
  if (allowed.over && Number.isFinite(edgeOver) && edgeOver >= th) {
    candidates.push({ side: "over", edge: edgeOver });
  }
  if (allowed.under && Number.isFinite(edgeUnder) && edgeUnder >= th) {
    candidates.push({ side: "under", edge: edgeUnder });
  }
  if (!candidates.length) return null;
  candidates.sort((a, b) => b.edge - a.edge);
  return { ...candidates[0], sgReason: allowed.reason };
}
