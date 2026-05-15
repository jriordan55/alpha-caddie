#!/usr/bin/env node
/**
 * Live + sportsbook + DraftKings round props + fresh DataGolf field / round projections.
 * Does **not** run `update:rounds` or touch `data/historical_rounds_all.csv` / `build:history`.
 *
 *   npm run refresh:live-dk-round
 *
 * Steps:
 *   1) fetch-datagolf.mjs — GOLF_SKIP_HISTORY_ON_FETCH_DG=1 (no historical rounds merge)
 *   2) fetch-live-in-play.mjs — live-in-play.json
 *   3) fetch-book-odds-into-projections.mjs — books + DK → projections; optional dk_round_projection_audit.csv
 *   4) merge-live-hole-pars-into-projections.mjs
 *
 * To refresh historical_rounds_all.csv + player history, use `npm run refresh:app` or `npm run update:rounds`.
 *
 * Env: DATAGOLF_API_KEY or datagolf.local.json, GOLF_MODEL_DIR, GOLF_SKIP_DK_OU=1, etc.
 */
import { spawnSync } from "child_process";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const baseEnv = { ...process.env, GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT };

function run(rel, label, extraEnv = {}) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh:live-dk-round] ${label}…\n`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...baseEnv, ...extraEnv },
  });
  if (r.status !== 0) {
    console.error(`[refresh:live-dk-round] ${label} failed (exit ${r.status ?? "?"})`);
    process.exit(r.status ?? 1);
  }
}

const skipHistRaw = process.env.GOLF_SKIP_HISTORY_ON_FETCH_DG;
const skipHistDg =
  skipHistRaw != null && String(skipHistRaw).trim() !== "" ? String(skipHistRaw).trim() : "1";

run("fetch-datagolf.mjs", "DataGolf field + round projections (fetch:dg, no historical rounds)", {
  GOLF_SKIP_HISTORY_ON_FETCH_DG: skipHistDg,
});
run("fetch-live-in-play.mjs", "Live / in-play (fetch:in-play)");
run("fetch-book-odds-into-projections.mjs", "Sportsbook + DK round props (fetch:book-odds)");
run("merge-live-hole-pars-into-projections.mjs", "Merge live hole pars into projections");

console.log(
  "\n[refresh:live-dk-round] Done. Hard-refresh the browser (Ctrl+Shift+R) if the tab was already open.\n",
);
