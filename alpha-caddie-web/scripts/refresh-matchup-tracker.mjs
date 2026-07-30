#!/usr/bin/env node
/**
 * Refresh matchup tracker data: DataGolf historical matchups (DK/FD/BetMGM)
 * then rebuild walk-forward round matchup + 3-ball backtest CSVs.
 *
 *   npm run matchup-tracker:refresh
 */
import { spawnSync } from "child_process";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");

function run(rel, label) {
  console.log(`\n[matchup-tracker:refresh] ${label}…\n`);
  const r = spawnSync(process.execPath, [join(WEB, "scripts", rel)], {
    cwd: WEB,
    stdio: "inherit",
    env: {
      ...process.env,
      GOLF_MATCHUPS_BOOKS: process.env.GOLF_MATCHUPS_BOOKS || "draftkings,fanduel,betmgm",
      GOLF_ODDS_SKIP_OUTRIGHTS: process.env.GOLF_ODDS_SKIP_OUTRIGHTS || "1",
    },
  });
  if (r.status !== 0) process.exit(r.status ?? 1);
}

if (String(process.env.GOLF_SKIP_MATCHUP_ODDS_UPDATE || "").trim() !== "1") {
  run("update-historical-odds-node.mjs", "DataGolf historical matchups (DK/FD/BetMGM)");
}
run("export-matchup-backtest-csv.mjs", "Walk-forward matchup + 3-ball backtest CSV");
console.log("\n[matchup-tracker:refresh] Done.\n");
