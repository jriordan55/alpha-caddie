#!/usr/bin/env node

/**

 * Honest walk-forward OOS ROI report (uniform EV, no DK calibration).

 *   npm run report:walkforward-oos-roi

 */

import { writeFileSync } from "fs";

import { DEFAULT_MIN_EV_PCT } from "./bet-policy.mjs";

import { runWalkForwardOosReport, WALKFORWARD_OOS_JSON } from "./walkforward-oos-roi.mjs";



const report = await runWalkForwardOosReport();

writeFileSync(WALKFORWARD_OOS_JSON, `${JSON.stringify(report, null, 2)}\n`);



const rec = report.combined_oos_recommended;

const unfiltered = report.combined_oos_unfiltered_at_5pct;

const peak = report.peak_oos_event_at_5pct;

const bestTh = report.best_oos_threshold;



console.log("=== Walk-forward OOS ROI (uniform EV, projection model, no DK calibration) ===\n");

console.log(`Events graded OOS: ${report.oos_event_count} (excludes ${report.excluded_live_event || "none"})`);

console.log(`Policy: ≥${DEFAULT_MIN_EV_PCT}% EV all markets, both sides, no gap/side filters\n`);



console.log(

  `Recommended @ ${DEFAULT_MIN_EV_PCT}% EV: ${rec?.units >= 0 ? "+" : ""}${rec?.units}u / ${rec?.bets} bets = ${rec?.roi_pct}% ROI (${rec?.hit_pct}% hit)`,

);

console.log(

  `Unfiltered @ 5% EV:              ${unfiltered?.units >= 0 ? "+" : ""}${unfiltered?.units}u / ${unfiltered?.bets} bets = ${unfiltered?.roi_pct}% ROI\n`,

);



console.log("By market @ recommended policy:");

for (const m of report.by_market_at_5pct) {

  console.log(`  ${m.market.padEnd(14)} ${m.roi_pct}%  ${m.bets} bets  ${m.units >= 0 ? "+" : ""}${m.units}u`);

}



console.log("\nPer-event @ recommended policy:");

for (const e of report.by_event) {

  const r = e.at_5pct;

  if (!r?.bets) continue;

  console.log(`  ${e.event.slice(0, 40).padEnd(42)} ${r.units >= 0 ? "+" : ""}${r.units}u  ${r.bets} bets  ${r.roi_pct}%`);

}



console.log(`\nPeak single-event: ${peak?.event} — ${peak?.roi_pct}% (${peak?.bets} bets)`);

console.log(

  `Best threshold (policy): ${bestTh?.min_ev_pct}% EV → ${bestTh?.roi_pct}% (${bestTh?.bets} bets)`,

);

console.log(`\nWrote ${WALKFORWARD_OOS_JSON}`);

