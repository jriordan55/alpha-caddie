#!/usr/bin/env node

/**
 * Walk-forward OOS ROI report + embed outcome calibration into projections.json.
 *   npm run report:walkforward-oos-roi
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { DEFAULT_MIN_EV_PCT, OOS_MARKET_POLICY } from "./bet-policy.mjs";
import { runWalkForwardOosReport, WALKFORWARD_OOS_JSON } from "./walkforward-oos-roi.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PROJ = join(WEB, "projections.json");

function embedOutcomeCalibrationInProjections(report) {
  if (!existsSync(PROJ)) return;
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    const calibration = {
      generated_at: report.generated_at,
      sigma_scales: report.outcome_sigma_scales,
      bet_policy: OOS_MARKET_POLICY,
    };
    j.outcome_calibration = calibration;
    if (!j.meta || typeof j.meta !== "object") j.meta = {};
    j.meta.outcome_calibration = calibration;
    writeFileSync(PROJ, `${JSON.stringify(j, null, 2)}\n`);
    console.log(`Embedded outcome_calibration into ${PROJ}`);
  } catch (e) {
    console.warn(`[report] Could not patch projections.json: ${e?.message || e}`);
  }
}

const report = await runWalkForwardOosReport();
writeFileSync(WALKFORWARD_OOS_JSON, `${JSON.stringify(report, null, 2)}\n`);
embedOutcomeCalibrationInProjections(report);

const rec = report.combined_oos_recommended;
const unfiltered = report.combined_oos_unfiltered_at_5pct;
const peak = report.peak_oos_event_at_5pct;
const bestTh = report.best_oos_threshold;

console.log("=== Walk-forward OOS ROI (per-market policy + calibrated pricing) ===\n");
console.log(`Events graded OOS: ${report.oos_event_count} (excludes ${report.excluded_live_event || "none"})`);
console.log("Policy: per-market EV/gap/side filters from bet-policy.mjs\n");

console.log(
  `Recommended policy: ${rec?.units >= 0 ? "+" : ""}${rec?.units}u / ${rec?.bets} bets = ${rec?.roi_pct}% ROI (${rec?.hit_pct}% hit)`,
);
console.log(
  `Unfiltered @ 5% EV:     ${unfiltered?.units >= 0 ? "+" : ""}${unfiltered?.units}u / ${unfiltered?.bets} bets = ${unfiltered?.roi_pct}% ROI\n`,
);

console.log("By market @ recommended policy:");
for (const m of report.by_market_at_5pct) {
  const pol = OOS_MARKET_POLICY[m.market];
  const polNote = pol
    ? `EV≥${pol.minEv}% gap≥${pol.minGap} ${pol.side}${pol.minGirMinusFw ? ` gir-fw≥${pol.minGirMinusFw}` : ""}`
    : "";
  console.log(
    `  ${m.market.padEnd(14)} ${m.roi_pct}%  ${m.bets} bets  ${m.units >= 0 ? "+" : ""}${m.units}u  ${polNote}`,
  );
}

console.log("\nPer-event @ recommended policy:");
for (const e of report.by_event) {
  const r = e.at_5pct;
  if (!r?.bets) continue;
  console.log(`  ${e.event.slice(0, 40).padEnd(42)} ${r.units >= 0 ? "+" : ""}${r.units}u  ${r.bets} bets  ${r.roi_pct}%`);
}

console.log(`\nPeak single-event: ${peak?.event} — ${peak?.roi_pct}% (${peak?.bets} bets)`);
console.log(`Best threshold (policy): ${bestTh?.min_ev_pct}% EV → ${bestTh?.roi_pct}% (${bestTh?.bets} bets)`);
console.log(`\nWrote ${WALKFORWARD_OOS_JSON}`);
