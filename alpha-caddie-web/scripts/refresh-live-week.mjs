#!/usr/bin/env node
/**
 * Live-week refresh: same operational data as push:all **without** reloading full history.
 *
 *   npm run refresh:live
 *
 * Skips:
 *   - DataGolf historical-raw-data/rounds → historical_rounds_all.csv merge (update:rounds)
 *   - build-player-history.mjs streaming the full ~70MB CSV into player_round_history.json
 *
 * Still runs:
 *   - fetch:dg (field, projections, SG, hole pars, outrights, matchups) — no inline history merge
 *   - course-table, in-play, pgatouR current-event scorecards, book odds, finish-tool
 *   - live hole pars + display_round / course-difficulty merge into projections
 *   - patch-current-event-history (pgatouR + live GIR/fairways into existing shards only)
 *   - embed:history (compact bundle from existing shards)
 *   - round projection vs actual CSV
 *   - mirror JSON → website/public/data/
 *
 * For full history (2004→present CSV + rebuild all shards), use push:all or npm run refresh:app with
 * GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1.
 *
 * Env: DATAGOLF_API_KEY or datagolf.local.json, GOLF_MODEL_DIR, GOLF_SKIP_DK_OU=1, etc.
 */
import { spawnSync } from "child_process";
import { copyFileSync, existsSync, mkdirSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

function buildBaseEnv() {
  const e = { ...process.env, GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT };
  delete e.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY;
  return e;
}

function run(rel, label, extraEnv = {}) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh:live] ${label}…\n`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...buildBaseEnv(), ...extraEnv },
  });
  if (r.status !== 0) {
    console.error(`[refresh:live] ${label} failed (exit ${r.status ?? "?"})`);
    process.exit(r.status ?? 1);
  }
}

function mirrorWebsitePublicData() {
  const destDir = path.join(REPO_ROOT, "website", "public", "data");
  mkdirSync(destDir, { recursive: true });
  const files = [
    "live-in-play.json",
    "projections.json",
    "course-table.json",
    "approach_skill_ytd.json",
    "approach_skill_l12.json",
  ];
  console.log("\n[refresh:live] Mirroring JSON → website/public/data/ …\n");
  for (const name of files) {
    const src = path.join(WEB_ROOT, name);
    const dest = path.join(destDir, name);
    if (!existsSync(src)) {
      console.log(`[refresh:live] skip (missing): ${name}`);
      continue;
    }
    copyFileSync(src, dest);
    console.log(`[refresh:live]   ${name}`);
  }
}

const skipDg = String(process.env.GOLF_REFRESH_LIVE_SKIP_DG || "").trim() === "1";

if (!skipDg) {
  run("fetch-datagolf.mjs", "Field + model (fetch:dg, no historical CSV merge)", {
    GOLF_SKIP_HISTORY_ON_FETCH_DG: "1",
  });
  run("build-course-table-json.mjs", "Course table JSON (build:course-table)");
} else {
  console.log("\n[refresh:live] GOLF_REFRESH_LIVE_SKIP_DG=1 — skipping fetch:dg + build:course-table.\n");
  if (!existsSync(path.join(WEB_ROOT, "projections.json"))) {
    console.error("[refresh:live] Missing projections.json; cannot skip fetch:dg.\n");
    process.exit(1);
  }
}

run("fetch-live-in-play.mjs", "Live / in-play (fetch:in-play)");
run("run-refresh-pgatour-event-rounds.mjs", "Current-event PGA rounds from pgatouR (refresh:pgatour-event)");
run("fetch-book-odds-into-projections.mjs", "Sportsbook + DK round props (fetch:book-odds)");
run("fetch-datagolf-finish-tool-outrights.mjs", "Finish-tool outrights (fetch:finish-tool)");
run("merge-live-hole-pars-into-projections.mjs", "Merge live hole pars into projections");
run("merge-live-round-meta-into-projections.mjs", "Merge live round meta into projections");
run("patch-current-event-history-shards.mjs", "Patch current-event rows into history shards (patch:current-event-history)");
run("embed-player-history.mjs", "Re-embed history from shards (embed:history)");
run("export-round-projection-vs-actual-csv.mjs", "Round projection vs actual CSV (export:round-projection-vs-actual)");

mirrorWebsitePublicData();

console.log(
  "\n[refresh:live] Done — projections, live-in-play, odds, and current-week history shards updated.",
);
console.log(
  "[refresh:live] Full historical_rounds_all.csv / build:history were NOT run. Use push:all or refresh:app for that.",
);
console.log("[refresh:live] Hard-refresh the browser (Ctrl+Shift+R). Commit/publish: npm run push:live\n");
