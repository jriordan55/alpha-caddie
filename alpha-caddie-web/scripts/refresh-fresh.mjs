#!/usr/bin/env node
/**
 * One command for “fresh odds + live data” without a full model rebuild.
 *
 *   npm run refresh
 *
 * Runs:
 *   1) fetch-datagolf.mjs — only if alpha-caddie-web/projections.json is missing OR GOLF_REFRESH_DG=1
 *   2) fetch-live-in-play.mjs — always (live scores / in-play bundle → live-in-play.json)
 *   3) fetch-book-odds-into-projections.mjs — always (merges DataGolf books + DK props into existing projections.json)
 *
 * Env:
 *   DATAGOLF_API_KEY or datagolf.local.json
 *   GOLF_MODEL_DIR — repo root (defaults to parent of alpha-caddie-web)
 *   GOLF_REFRESH_DG=1 — also run full fetch:dg (field + model JSON); omit for faster refresh when projections.json exists
 *   GOLF_SKIP_DK_OU=1 — skip DraftKings Playwright in fetch-book-odds (same as elsewhere)
 */
import { spawnSync } from "child_process";
import { existsSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const env = { ...process.env, GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT };

function run(rel, label) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh] ${label}…\n`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env,
  });
  if (r.status !== 0) {
    console.error(`[refresh] ${label} failed (exit ${r.status ?? "?"})`);
    process.exit(r.status ?? 1);
  }
}

const projPath = path.join(WEB_ROOT, "projections.json");
const forceDg = String(process.env.GOLF_REFRESH_DG || "").trim() === "1";

if (!existsSync(projPath) || forceDg) {
  if (!existsSync(projPath)) {
    console.log("[refresh] No projections.json — running full fetch:dg first.\n");
  } else {
    console.log("[refresh] GOLF_REFRESH_DG=1 — running full fetch:dg.\n");
  }
  run("fetch-datagolf.mjs", "Full projections (fetch:dg)");
} else {
  console.log(
    "[refresh] Skipping fetch:dg (projections.json exists). Set GOLF_REFRESH_DG=1 to rebuild field/model from DataGolf.\n",
  );
}

run("fetch-live-in-play.mjs", "Live / in-play (fetch:in-play)");
run("fetch-book-odds-into-projections.mjs", "Sportsbook + DK round props (fetch:book-odds)");

console.log("\n[refresh] Done. Open the app and hard-refresh (Ctrl+Shift+R) if the browser was already open.\n");
