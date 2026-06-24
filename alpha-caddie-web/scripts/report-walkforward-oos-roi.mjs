#!/usr/bin/env node
/**
 * Honest walk-forward OOS ROI report (no outcome fitting).
 *   npm run report:walkforward-oos-roi
 */
import { writeFileSync } from "fs";
import { runWalkForwardOosReport, WALKFORWARD_OOS_JSON } from "./walkforward-oos-roi.mjs";

const report = await runWalkForwardOosReport();
writeFileSync(WALKFORWARD_OOS_JSON, `${JSON.stringify(report, null, 2)}\n`);

const c5 = report.combined_oos_at_5pct;
const peak = report.peak_oos_event_at_5pct;
const bestTh = report.best_oos_threshold_calibrated;

console.log("=== Walk-forward OOS ROI (honest) ===\n");
console.log(`Events graded OOS: ${report.oos_event_count} (excludes ${report.excluded_live_event || "none"})`);
console.log(`Method: ${report.methodology.fit} + ${report.methodology.grading}\n`);

console.log(`Combined @ 5% EV (calibrated): ${c5?.units >= 0 ? "+" : ""}${c5?.units}u / ${c5?.bets} bets = ${c5?.roi_pct}% ROI (${c5?.hit_pct}% hit)`);
console.log(`Combined @ 5% EV (raw model):     ${report.combined_oos_raw_at_5pct?.roi_pct}% ROI\n`);

console.log("By market @ 5% OOS:");
for (const m of report.by_market_at_5pct) {
  console.log(`  ${m.market.padEnd(14)} ${m.roi_pct}%  ${m.bets} bets  ${m.units >= 0 ? "+" : ""}${m.units}u`);
}

console.log("\nPer-event @ 5% OOS (calibrated):");
for (const e of report.by_event) {
  const r = e.at_5pct;
  if (!r?.bets) continue;
  console.log(`  ${e.event.slice(0, 40).padEnd(42)} ${r.units >= 0 ? "+" : ""}${r.units}u  ${r.bets} bets  ${r.roi_pct}%`);
}

console.log(`\nPeak single-event OOS @ 5%: ${peak?.event} — ${peak?.roi_pct}% (${peak?.bets} bets)`);
console.log(
  `Best threshold OOS (exploratory): ${bestTh?.min_ev_pct}% EV → ${bestTh?.calibrated?.roi_pct}% (${bestTh?.calibrated?.bets} bets)`,
);
console.log(`\nWrote ${WALKFORWARD_OOS_JSON}`);
