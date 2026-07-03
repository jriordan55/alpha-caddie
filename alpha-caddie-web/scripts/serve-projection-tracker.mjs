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
  runRefreshScript("export-round-projection-vs-actual-csv.mjs", "Refreshing round_projection_vs_actual CSV");
  runRefreshScript("promote-round-projection-vs-actual-csv.mjs", "Publishing tracker CSV");
  runRefreshScript("export-matchup-backtest-csv.mjs", "Refreshing matchup backtest CSV");
} else {
  console.log("[projection-tracker] GOLF_SKIP_TRACKER_REFRESH=1 — using existing CSV on disk.");
}

console.log(`[projection-tracker] Serving ${WEB}`);
console.log(`[projection-tracker] Bet log: ${url}#bets`);
console.log(`[projection-tracker] Risk / bankroll: ${url}#risk`);
console.log(`[projection-tracker] Guide: ${url}#guide`);
console.log("[projection-tracker] CSV: data/round_projection_vs_actual_summary.csv + matchup_backtest_summary.csv");
console.log("[projection-tracker] Manual refresh: npm run projection-tracker:refresh");

const child = spawn("npx", ["--yes", "serve", ".", "-p", PORT], {
  cwd: WEB,
  stdio: "inherit",
  shell: true,
});

child.on("exit", (code) => process.exit(code ?? 0));
