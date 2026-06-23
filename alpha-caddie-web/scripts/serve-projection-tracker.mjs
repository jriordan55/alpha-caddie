#!/usr/bin/env node
/**
 * Static server for projection-tracker dashboard.
 *   npm run projection-tracker
 *   → http://localhost:5173/projection-tracker/
 */
import { spawn } from "child_process";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PORT = process.env.PORT || "5173";
const url = `http://localhost:${PORT}/projection-tracker/`;

console.log(`[projection-tracker] Serving ${WEB}`);
console.log(`[projection-tracker] Bet log: ${url}#bets`);
console.log(`[projection-tracker] Risk / bankroll: ${url}#risk`);
console.log(`[projection-tracker] Guide: ${url}#guide`);
console.log("[projection-tracker] CSV: data/round_projection_vs_actual_summary.csv + round_projection_vs_actual.csv");
console.log("[projection-tracker] Regenerate: npm run export:round-projection-vs-actual");

const child = spawn("npx", ["--yes", "serve", ".", "-p", PORT], {
  cwd: WEB,
  stdio: "inherit",
  shell: true,
});

child.on("exit", (code) => process.exit(code ?? 0));
