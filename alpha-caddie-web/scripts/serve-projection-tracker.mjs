#!/usr/bin/env node
/**
 * Static server for projection-tracker dashboard.
 * Refreshes tracker CSV from projections.json before serve (skip: GOLF_SKIP_TRACKER_REFRESH=1).
 *
 *   npm run projection-tracker
 *   → http://localhost:5173/projection-tracker/
 */
import { spawn, spawnSync } from "child_process";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PORT = process.env.PORT || "5173";
const url = `http://localhost:${PORT}/projection-tracker/`;

function envTruthy(name) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return false;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function runRefreshScript(rel, label) {
  const script = join(WEB, "scripts", rel);
  console.log(`[projection-tracker] ${label}…`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB,
    stdio: "inherit",
    env: {
      ...process.env,
      GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX: "1",
    },
  });
  if (r.status !== 0) {
    console.warn(`[projection-tracker] ${label} failed (exit ${r.status ?? "?"}); serving stale CSV if present.`);
  }
}

if (!envTruthy("GOLF_SKIP_TRACKER_REFRESH")) {
  // Light refresh: keep patched counting/score μ; re-bake + OOS + live apply.
  // Full CSV rebuild: GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS=1 npm run projection-tracker:refresh
  runRefreshScript("patch-birdie-fairway-mu.mjs", "Patching counting/score μ (as-of)");
  runRefreshScript("bake-both-side-roi.mjs", "Baking both-side ROI");
  runRefreshScript("report-walkforward-oos-roi.mjs", "Refreshing walkforward OOS ROI");
  runRefreshScript("apply-dg-methodology-to-projections.mjs", "Applying DG μ to live projections");
  runRefreshScript("apply-both-side-bias-to-projections.mjs", "Applying both-side bias");
} else {
  console.log("[projection-tracker] GOLF_SKIP_TRACKER_REFRESH=1 — using existing CSV / JSON on disk.");
}

console.log(`[projection-tracker] Serving ${WEB}`);
console.log(`[projection-tracker] Both-side edge: ${url}`);
console.log("[projection-tracker] Data: data/both_side_roi.json + both_side_bets.json + walkforward_oos_roi.json");
console.log("[projection-tracker] Manual full rebuild: npm run projection-tracker:refresh");

const child = spawn("npx", ["--yes", "serve", ".", "-p", PORT], {
  cwd: WEB,
  stdio: "inherit",
  shell: true,
});

child.on("exit", (code) => process.exit(code ?? 0));
