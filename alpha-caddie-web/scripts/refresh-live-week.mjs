#!/usr/bin/env node
/**
 * Live-week refresh: operational data + **recent tournament** in history/CSV, without a full 2004→present rebuild.
 *
 *   npm run refresh:live
 *
 * Skips:
 *   - GOLF_HISTORICAL_ROUNDS_FULL_HISTORY (entire DG archive re-merge)
 *   - build-player-history.mjs scanning all years into new shards from scratch
 *
 * Does:
 *   1) merge-recent-historical-rounds — last N years into historical_rounds_all.csv (current event scoring + SG)
 *   2) fetch:dg — round projections from skill-ratings (OTT/APP/PUT/T2G) + historical_calibration + course prior-round difficulty + within-event form from CSV
 *   3) in-play, pgatouR, book odds, finish-tool, projection merges
 *   4) rebuild-current-event-history-shards — **replace** all shard rows for this event (not patch-on-stale)
 *   5) embed, export, mirror website/public/data
 *
 * Env: DATAGOLF_API_KEY, GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS (default 2), GOLF_SKIP_DK_OU=1, etc.
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
const recentYears = String(process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "2").trim();

run("merge-recent-historical-rounds.mjs", `Merge recent historical rounds (${recentYears}yr CSV, for course-history in projections)`, {
  GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: recentYears,
});

if (!skipDg) {
  run("fetch-datagolf.mjs", "Field + round projections (μ_SG, SG pillars, course prior-round + within-event form)", {
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
run("rebuild-current-event-history-shards.mjs", "Rebuild current-event history shards (replace event rows)");
run("embed-player-history.mjs", "Re-embed history from shards (embed:history)");
run("export-round-projection-vs-actual-csv.mjs", "Round projection vs actual CSV (export:round-projection-vs-actual)");

mirrorWebsitePublicData();

console.log(
  "\n[refresh:live] Done — projections use course history + SG categories; current-event history rows were replaced (not patched onto stale data).",
);
console.log(
  "[refresh:live] For a full archive rebuild (2004→present), use push:all or refresh:app with GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1.",
);
console.log("[refresh:live] Hard-refresh the browser (Ctrl+Shift+R). Publish: npm run push:live\n");
