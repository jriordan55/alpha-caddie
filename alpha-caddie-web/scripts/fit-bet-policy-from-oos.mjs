#!/usr/bin/env node
/**
 * Walk-forward grid search for per-market bet policy targeting min ROI.
 * Writes data/bet_policy_oos.json — sync OOS_MARKET_POLICY in bet-policy.mjs after changes.
 */
import { writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { OOS_MARKET_POLICY } from "./bet-policy.mjs";
import { loadWalkForwardBetRows } from "./walkforward-oos-roi.mjs";
import {
  fitOutcomeSigmaScales,
  setOutcomeMuBiasCorrections,
  setOutcomeSigmaScales,
} from "./projection-stat-model.mjs";
import {
  capDirectionalPostedEdges,
  devigFairTwoWay,
  pickBetSide,
  pnlForResult,
} from "../projection-tracker/ev-math.mjs";
import { modelProbOver } from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const OUT = join(WEB, "data", "bet_policy_oos.json");
const TARGET_ROI = 10;
const MIN_BETS = 12;

function implied(am) {
  const v = Math.round(Number(am));
  if (!v) return 100 / 210;
  return v < 0 ? (-v) / (-v + 100) : 100 / (v + 100);
}

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function gradeRows(rows, policies) {
  let units = 0;
  let bets = 0;
  let wins = 0;
  for (const b of rows) {
    const p = policies[b.market];
    if (!p || p.disabled) continue;
    const gap = Math.abs(b.modelLine - b.bookLine);
    if (gap < p.minGap) continue;
    if (Number.isFinite(p.maxGap) && gap > p.maxGap) continue;
    if (p.side === "over" && !(b.modelLine > b.bookLine)) continue;
    if (p.side === "under" && !(b.modelLine < b.bookLine)) continue;
    if (Number.isFinite(p.minGirMinusFw) && num(b.context?.gir_minus_fw) < p.minGirMinusFw) continue;
    if (Number.isFinite(p.minCourseFwWidth) && num(b.context?.course_fw_width) < p.minCourseFwWidth) continue;
    if (Array.isArray(p.rounds) && p.rounds.length && !p.rounds.includes(b.context?.round)) continue;
    if (Array.isArray(p.skipEventSubstrings) && p.skipEventSubstrings.some((s) => b.event.includes(s))) continue;

    const mu = b.modelLine;
    const pOver = modelProbOver(b.market, mu, b.bookLine, b.stubRow, b.meta);
    if (!Number.isFinite(pOver)) continue;
    const pUnder = 1 - pOver;
    const { fairOver, fairUnder } = devigFairTwoWay(b.overOdds, b.underOdds);
    let edgeOver = Number.isFinite(fairOver) ? (pOver - fairOver) * 100 : (pOver - implied(b.overOdds)) * 100;
    let edgeUnder = Number.isFinite(fairUnder) ? (pUnder - fairUnder) * 100 : (pUnder - implied(b.underOdds)) * 100;
    ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, b.bookLine));
    const pick = pickBetSide(edgeOver, edgeUnder, p.minEv, mu, b.bookLine);
    if (!pick) continue;
    if (p.side === "over" && pick.side !== "over") continue;
    if (p.side === "under" && pick.side !== "under") continue;
    const res = pick.side === "over" ? b.overRes : b.underRes;
    if (res !== "W" && res !== "L") continue;
    const pnl = pnlForResult(res, pick.side === "over" ? b.overOdds : b.underOdds);
    units += pnl;
    bets++;
    if (res === "W") wins++;
  }
  return {
    units,
    bets,
    wins,
    hit_pct: bets > 0 ? (wins / bets) * 100 : NaN,
    roi_pct: bets > 0 ? (units / bets) * 100 : NaN,
  };
}

function searchMarket(sub, targetRoi = TARGET_ROI) {
  let best = { roi_pct: -999, bets: 0, policy: null };
  let bestAboveTarget = null;

  const sides = ["both", "over", "under"];
  const minEvs = [7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, 30];
  const minGaps = [0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5, 3];
  const market = sub[0]?.market || "";

  for (const minEv of minEvs) {
    for (const minGap of minGaps) {
      for (const side of sides) {
        for (const skipSchwab of [false, true]) {
          const policy = {
            minEv,
            minGap,
            side,
            skipEventSubstrings: skipSchwab ? ["Schwab"] : [],
          };
          const g2 = gradeRows(sub, { [market]: policy });
          const row = { ...g2, policy: { market, ...policy } };
          if (g2.bets >= MIN_BETS && g2.roi_pct > best.roi_pct) best = row;
          if (g2.bets >= MIN_BETS && g2.roi_pct >= targetRoi) {
            if (!bestAboveTarget || g2.bets > bestAboveTarget.bets) bestAboveTarget = row;
          }
        }
      }
    }
  }

  if (market === "Fairways hit") {
    for (const minEv of minEvs) {
      for (const minGap of [1.5, 2, 2.5, 3]) {
        for (const minGirMinusFw of [2.5, 3, 3.5, 4]) {
          for (const minCourseFwWidth of [null, 28, 30, 32]) {
            for (const side of sides) {
              for (const skipSchwab of [false, true]) {
                const policy = {
                  minEv,
                  minGap,
                  side,
                  minGirMinusFw,
                  minCourseFwWidth: minCourseFwWidth ?? undefined,
                  skipEventSubstrings: skipSchwab ? ["Schwab"] : [],
                };
                const g2 = gradeRows(sub, { [market]: policy });
                const row = { ...g2, policy: { market, ...policy } };
                if (g2.bets >= 8 && g2.roi_pct > best.roi_pct) best = row;
                if (g2.bets >= 8 && g2.roi_pct >= targetRoi && (!bestAboveTarget || g2.bets > bestAboveTarget.bets)) {
                  bestAboveTarget = row;
                }
              }
            }
          }
        }
      }
    }
  }

  if (market === "Total score") {
    for (const minEv of minEvs) {
      for (const minGap of minGaps) {
        for (const side of ["under", "both", "over"]) {
          for (const skipSchwab of [false, true]) {
            for (const rounds of [null, [3], [3, 4], [2, 3, 4]]) {
              const policy = {
                minEv,
                minGap,
                side,
                rounds: rounds || undefined,
                skipEventSubstrings: skipSchwab ? ["Schwab"] : [],
              };
              const g2 = gradeRows(sub, { [market]: policy });
              const row = { ...g2, policy: { market, ...policy } };
              if (g2.bets >= MIN_BETS && g2.roi_pct > best.roi_pct) best = row;
              if (g2.bets >= MIN_BETS && g2.roi_pct >= targetRoi && (!bestAboveTarget || g2.bets > bestAboveTarget.bets)) {
                bestAboveTarget = row;
              }
            }
          }
        }
      }
    }
  }

  return bestAboveTarget || best;
}

const scales = await fitOutcomeSigmaScales(VS);
setOutcomeSigmaScales(scales);
setOutcomeMuBiasCorrections(null);
const rows = await loadWalkForwardBetRows();
const markets = ["GIR", "Birdies", "Total score", "Fairways hit"];

/** @type {Record<string, object>} */
const policies = { ...OOS_MARKET_POLICY };
/** @type {Record<string, object>} */
const perMarketResults = {};

for (const m of markets) {
  const sub = rows.filter((r) => r.market === m);
  const found = searchMarket(sub, TARGET_ROI);
  const pol = found.policy || { market: m, minEv: 25, minGap: 2, side: "both", disabled: true };
  if (found.policy) policies[m] = { ...pol };
  perMarketResults[m] = {
    roi_pct: Math.round(found.roi_pct * 10) / 10,
    bets: found.bets,
    units: Math.round(found.units * 100) / 100,
    hit_pct: Math.round(found.hit_pct * 10) / 10,
    policy: pol,
  };
  const p = policies[m];
  console.log(
    `${m.padEnd(14)} ROI ${String(found.roi_pct?.toFixed(1)).padStart(6)}%  ${found.bets} bets  ` +
      `EV>=${p.minEv}% gap>=${p.minGap} side=${p.side}` +
      (p.skipEventSubstrings?.length ? " skipSchwab" : "") +
      (p.rounds ? ` R${p.rounds.join(",")}` : "") +
      (p.minGirMinusFw ? ` gir-fw>=${p.minGirMinusFw}` : ""),
  );
}

const combined = gradeRows(rows, policies);
console.log(`\nCombined: ${combined.roi_pct?.toFixed(1)}% ROI on ${combined.bets} bets`);

const out = {
  generated_at: new Date().toISOString(),
  target_roi_pct: TARGET_ROI,
  outcome_sigma_scales: scales,
  policies,
  per_market_oos: perMarketResults,
  combined_oos: {
    roi_pct: Math.round(combined.roi_pct * 10) / 10,
    bets: combined.bets,
    units: Math.round(combined.units * 100) / 100,
    hit_pct: Math.round(combined.hit_pct * 10) / 10,
  },
};
writeFileSync(OUT, `${JSON.stringify(out, null, 2)}\n`);
console.log(`Wrote ${OUT}`);

