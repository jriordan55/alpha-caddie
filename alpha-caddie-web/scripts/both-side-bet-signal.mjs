/**
 * Bet / no-bet signal matching projection-tracker live picks:
 * both-side+ markets only, policy gap_over/gap_under, optional odds floors.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");

/** DK / CSV market label → both_side_roi market key */
export const DK_TO_ROI_MARKET = {
  "Total Score": "Total score",
  "Total score": "Total score",
  Birdies: "Birdies",
  Bogeys: "Bogeys",
  Pars: "Pars",
  GIR: "GIR",
  "Fairways hit": "Fairways hit",
};

export function loadBothSidePolicy(webRoot = WEB) {
  const roiPath = join(webRoot, "data", "both_side_roi.json");
  const betsPath = join(webRoot, "data", "both_side_bets.json");
  const roi = existsSync(roiPath) ? JSON.parse(readFileSync(roiPath, "utf8")) : null;
  const bets = existsSync(betsPath) ? JSON.parse(readFileSync(betsPath, "utf8")) : null;
  return {
    roi,
    bets,
    pass: new Set(roi?.overall?.both_side_positive_markets || []),
    recommended: roi?.recommended || {},
    liveBias: bets?.live_bias || roi?.live_bias || {},
  };
}

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

/** Model μ for a player row (Birdies = birdies + eagles, matching tracker). */
export function modelMuForRoiMarket(player, roiMarket) {
  if (!player) return NaN;
  if (roiMarket === "Total score") return num(player.total_score);
  if (roiMarket === "Birdies") return num(player.birdies) + num(player.eagles || 0, 0);
  if (roiMarket === "Bogeys") {
    return num(player.bogeys) + num(player.doubles ?? player.doubles_or_worse, 0);
  }
  if (roiMarket === "Pars") return num(player.pars);
  if (roiMarket === "GIR") return num(player.gir);
  if (roiMarket === "Fairways hit") return num(player.fairways);
  return NaN;
}

/**
 * @param {object} opts
 * @param {string} opts.dkMarket — prop market label
 * @param {"over"|"under"|string} opts.side
 * @param {number} opts.line
 * @param {number} opts.americanOdds — odds for this side
 * @param {object|null} opts.player — projections player row
 * @param {object} opts.policy — from loadBothSidePolicy()
 * @param {boolean} [opts.biasAlreadyApplied] — μ already corrected in projections.json
 * @returns {{ bet: "YES"|"NO", reason: string, mu: number, gap: number, gap_need: number, roi_market: string }}
 */
export function evaluateBothSideBetSignal(opts = {}) {
  const dkMarket = String(opts.dkMarket || "").trim();
  const roiMarket = DK_TO_ROI_MARKET[dkMarket] || "";
  const side = String(opts.side || "").trim().toLowerCase();
  const line = num(opts.line);
  const odds = num(opts.americanOdds);
  const policy = opts.policy || {};
  const pass = policy.pass instanceof Set ? policy.pass : new Set(policy.pass || []);
  const rec = policy.recommended?.[roiMarket];
  const biasMap = policy.liveBias || {};

  const empty = (reason) => ({
    bet: "NO",
    reason,
    mu: NaN,
    gap: NaN,
    gap_need: NaN,
    roi_market: roiMarket,
  });

  if (!roiMarket) return empty("market_not_tracked");
  if (!pass.has(roiMarket) || !rec?.both_sides_positive) return empty("market_off");
  if (side !== "over" && side !== "under") return empty("bad_side");
  if (!Number.isFinite(line)) return empty("no_line");

  let mu = modelMuForRoiMarket(opts.player, roiMarket);
  if (!Number.isFinite(mu)) return empty("no_mu");

  // Tracker subtracts live_bias from raw μ; skip if already baked into projections.
  if (!opts.biasAlreadyApplied) {
    const b = num(biasMap[roiMarket], 0);
    if (Number.isFinite(b)) mu -= b;
  }

  const gapOver = num(rec.gap_over ?? rec.gap, 0);
  const gapUnder = num(rec.gap_under ?? rec.gap, 0);
  const delta = mu - line;
  let wantSide = null;
  if (delta > gapOver) wantSide = "over";
  else if (delta < -gapUnder) wantSide = "under";
  if (!wantSide) {
    return {
      bet: "NO",
      reason: "inside_gap",
      mu: Math.round(mu * 100) / 100,
      gap: Math.round(delta * 100) / 100,
      gap_need: side === "over" ? gapOver : gapUnder,
      roi_market: roiMarket,
    };
  }
  if (wantSide !== side) {
    return {
      bet: "NO",
      reason: "wrong_side",
      mu: Math.round(mu * 100) / 100,
      gap: Math.round(delta * 100) / 100,
      gap_need: wantSide === "over" ? gapOver : gapUnder,
      roi_market: roiMarket,
    };
  }

  if (!Number.isFinite(odds)) return empty("no_odds");
  const underMin = rec.odds_rule?.under_min_american;
  const overMin = rec.odds_rule?.over_min_american;
  if (side === "under" && Number.isFinite(underMin) && !(odds >= underMin)) {
    return {
      bet: "NO",
      reason: "odds_floor",
      mu: Math.round(mu * 100) / 100,
      gap: Math.round(delta * 100) / 100,
      gap_need: gapUnder,
      roi_market: roiMarket,
    };
  }
  if (side === "over" && Number.isFinite(overMin) && !(odds >= overMin)) {
    return {
      bet: "NO",
      reason: "odds_floor",
      mu: Math.round(mu * 100) / 100,
      gap: Math.round(delta * 100) / 100,
      gap_need: gapOver,
      roi_market: roiMarket,
    };
  }

  return {
    bet: "YES",
    reason: "policy",
    mu: Math.round(mu * 100) / 100,
    gap: Math.round(delta * 100) / 100,
    gap_need: side === "over" ? gapOver : gapUnder,
    roi_market: roiMarket,
  };
}
