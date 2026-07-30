#!/usr/bin/env node
/**
 * Static server for matchup-tracker (round matchups + 3-balls).
 * Refreshes DataGolf historical odds (DK/FD/MGM) + backtest CSV before serve
 * (skip: GOLF_SKIP_TRACKER_REFRESH=1).
 *
 *   npm run matchup-tracker
 *   → http://localhost:5173/matchup-tracker/
 */
import { spawn, spawnSync } from "child_process";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PORT = process.env.PORT || "5173";
const url = `http://localhost:${PORT}/matchup-tracker/`;

function envTruthy(name) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return false;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function runRefreshScript(rel, label, extraEnv = {}) {
  const script = join(WEB, "scripts", rel);
  console.log(`[matchup-tracker] ${label}…`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB,
    stdio: "inherit",
    env: {
      ...process.env,
      GOLF_MATCHUPS_BOOKS: process.env.GOLF_MATCHUPS_BOOKS || "draftkings,fanduel,betmgm",
      GOLF_ODDS_SKIP_OUTRIGHTS: process.env.GOLF_ODDS_SKIP_OUTRIGHTS || "1",
      ...extraEnv,
    },
  });
  if (r.status !== 0) {
    console.warn(`[matchup-tracker] ${label} failed (exit ${r.status ?? "?"}); serving stale CSV if present.`);
  }
}

if (!envTruthy("GOLF_SKIP_TRACKER_REFRESH")) {
  if (!envTruthy("GOLF_SKIP_MATCHUP_ODDS_UPDATE")) {
    runRefreshScript("update-historical-odds-node.mjs", "Refreshing DataGolf historical matchups (DK/FD/BetMGM)");
  }
  runRefreshScript("export-matchup-backtest-csv.mjs", "Rebuilding matchup + 3-ball backtest CSV");
} else {
  console.log("[matchup-tracker] GOLF_SKIP_TRACKER_REFRESH=1 — using existing CSV on disk.");
}

console.log(`[matchup-tracker] Serving ${WEB}`);
console.log(`[matchup-tracker] Open ${url}`);

const child = spawn(
  process.platform === "win32" ? "npx.cmd" : "npx",
  ["--yes", "serve", "-l", PORT, "."],
  { cwd: WEB, stdio: "inherit", shell: process.platform === "win32" },
);
child.on("exit", (code) => process.exit(code ?? 0));
