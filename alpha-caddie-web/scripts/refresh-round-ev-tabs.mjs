#!/usr/bin/env node
/**
 * Refresh data for the Round projections grid and +EV bets tab only (no fetch:dg, no history).
 *
 *   npm run refresh:round-ev
 *
 *   1) fetch-live-in-play.mjs — live bundle (+EV live / placement context when the event is live)
 *   2) fetch-book-odds-into-projections.mjs — books + DK round O/U → projections.json
 *   3) fetch-datagolf-finish-tool-outrights.mjs — outright finish markets (+EV outrights)
 *   4) merge-live-hole-pars-into-projections.mjs — keep hole_pars aligned if book-odds ran inline fetch:dg
 *
 * Env: DATAGOLF_API_KEY or datagolf.local.json, GOLF_MODEL_DIR, GOLF_SKIP_DK_OU=1, etc.
 */
import { spawnSync } from "child_process";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const env = { ...process.env, GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT };

function run(rel, label) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh:round-ev] ${label}…\n`);
  const r = spawnSync(process.execPath, [script], { cwd: WEB_ROOT, stdio: "inherit", env });
  if (r.status !== 0) {
    console.error(`[refresh:round-ev] ${label} failed (exit ${r.status ?? "?"})`);
    process.exit(r.status ?? 1);
  }
}

run("fetch-live-in-play.mjs", "Live / in-play (fetch:in-play)");
run("fetch-book-odds-into-projections.mjs", "Sportsbook + DK round props (fetch:book-odds)");
run("fetch-datagolf-finish-tool-outrights.mjs", "Finish-tool outrights (fetch:finish-tool)");
run("merge-live-hole-pars-into-projections.mjs", "Merge live hole pars into projections");

console.log("\n[refresh:round-ev] Done. Hard-refresh the browser if the tab was already open.\n");
