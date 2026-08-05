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
const money = report.money?.recommended_policy;
const moneyUnf = report.money?.unfiltered_at_5pct;
const flat = money?.flat_fixed;
const kelly = money?.kelly_unit_cap;
const peak$ = report.peak_oos_event_money;
const worst$ = report.worst_oos_event_money;

function usd(v, { signed = true } = {}) {
  if (!Number.isFinite(v)) return "—";
  const sign = !signed ? "" : v > 0 ? "+" : v < 0 ? "-" : "";
  return `${sign}$${Math.abs(v).toLocaleString("en-US", { maximumFractionDigits: 0 })}`;
}

console.log("=== Walk-forward OOS money ($10k bankroll) ===\n");
console.log(`Events graded OOS: ${report.oos_event_count} (excludes ${report.excluded_live_event || "none"})`);
console.log("Policy: per-market EV/gap/side filters · sequential event/round bankroll · 15% round cap\n");

console.log(
  `Flat $100/bet:     ${usd(flat?.pl)}  → ${usd(flat?.bankroll_end, { signed: false })}  (${flat?.roi_on_bankroll_pct}% on bankroll · ${flat?.roi_on_staked_pct}% on $${Math.round(flat?.total_staked || 0).toLocaleString()} staked · max DD ${usd(flat?.max_drawdown, { signed: false })} · ${flat?.bets} bets)`,
);
console.log(
  `¼ Kelly + 1% cap:  ${usd(kelly?.pl)}  → ${usd(kelly?.bankroll_end, { signed: false })}  (${kelly?.roi_on_bankroll_pct}% on bankroll · ${kelly?.roi_on_staked_pct}% on $${Math.round(kelly?.total_staked || 0).toLocaleString()} staked · max DD ${usd(kelly?.max_drawdown, { signed: false })} · ${kelly?.bets} bets)`,
);
console.log(
  `Unfiltered @5% flat: ${usd(moneyUnf?.flat_fixed?.pl)}   Kelly: ${usd(moneyUnf?.kelly_unit_cap?.pl)}\n`,
);

console.log(
  `Unit ROI (ref): recommended ${rec?.units >= 0 ? "+" : ""}${rec?.units}u / ${rec?.bets} = ${rec?.roi_pct}% · unfiltered ${unfiltered?.units >= 0 ? "+" : ""}${unfiltered?.units}u / ${unfiltered?.bets} = ${unfiltered?.roi_pct}%\n`,
);

console.log("By market (sequential $ P/L on shared $10k):");
for (const m of report.by_market_at_5pct) {
  const pol = OOS_MARKET_POLICY[m.market];
  const polNote = pol
    ? `EV≥${pol.minEv}% gap≥${pol.minGap} ${pol.side}${pol.minGirMinusFw ? ` gir-fw≥${pol.minGirMinusFw}` : ""}`
    : "";
  console.log(
    `  ${m.market.padEnd(14)} Kelly ${usd(m.kelly_pl).padStart(8)}  flat ${usd(m.flat_pl).padStart(8)}  ${String(m.bets).padStart(4)} bets  ${polNote}`,
  );
}

console.log("\nPer-event (sequential Kelly $ P/L):");
const eventsByMoney = [...(report.by_event || [])].sort(
  (a, b) => (b.money?.kelly_pl ?? -Infinity) - (a.money?.kelly_pl ?? -Infinity),
);
for (const e of eventsByMoney) {
  const r = e.at_5pct;
  if (!r?.bets && !(e.money?.kelly_bets || e.money?.flat_bets)) continue;
  console.log(
    `  ${e.event.slice(0, 40).padEnd(42)} Kelly ${usd(e.money?.kelly_pl).padStart(8)}  flat ${usd(e.money?.flat_pl).padStart(8)}  ${String(r?.bets || 0).padStart(4)} bets`,
  );
}

console.log(`\nBest event $:  ${peak$?.event || "—"} — Kelly ${usd(peak$?.pl)}`);
console.log(`Worst event $: ${worst$?.event || "—"} — Kelly ${usd(worst$?.pl)}`);
console.log(`\nWrote ${WALKFORWARD_OOS_JSON}`);
