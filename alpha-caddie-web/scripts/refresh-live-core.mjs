#!/usr/bin/env node
/**
 * Lean live publish — what push:live actually needs:
 *   1) projections.json (field, μ, display round)
 *   2) sportsbook odds in projections + paper-book-lines.json
 *   3) prior-round / live tab data (live-in-play, Trends shards, projection tracker CSV)
 *
 * Full depth (weather archive, hole props, ROI backtests, full history rebuild):
 *   npm run refresh:live:full
 */
import { copyFileSync, existsSync, mkdirSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { spawnSync } from "child_process";
import { dkOuScrapeEnv, liveProjectionPipelineEnv, requireDkOuEnv } from "./projection-pipeline-env.mjs";
import { resolveOuIncrementalSinceIso } from "./tracker-incremental.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

function envTruthy(name, defaultVal) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function buildBaseEnv() {
  const pipeline = liveProjectionPipelineEnv();
  const e = {
    ...process.env,
    ...pipeline,
    ...dkOuScrapeEnv(),
    ...requireDkOuEnv(),
    GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT,
    GOLF_SKIP_HISTORY_ON_FETCH_DG: "1",
    GOLF_SKIP_OUTRIGHT_BAKE_ON_FETCH_DG: "1",
    GOLF_SKIP_SPORTSBOOK_OUTRIGHT_SCRAPE: "1",
    GOLF_DEFER_DK_ROUND_AUDIT_UNTIL_REPAIR: "1",
    GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX: "1",
    GOLF_SKIP_MARKET_BOOK_CALIBRATION: "1",
    GOLF_SKIP_PAPER_BOOK_BAKE: "0",
  };
  for (const key of Object.keys(pipeline)) {
    if (process.env[key] !== undefined && String(process.env[key]).trim() !== "") {
      e[key] = process.env[key];
    }
  }
  delete e.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY;
  return e;
}

/**
 * @param {string} rel
 * @param {string} label
 * @param {Record<string, string>} [extraEnv]
 * @param {{ optional?: boolean }} [opts]
 */
function run(rel, label, extraEnv = {}, opts = {}) {
  const optional = !!opts.optional;
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh:live] ${label}…\n`);
  const t0 = Date.now();
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...buildBaseEnv(), ...extraEnv },
  });
  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  const code = r.status;
  if (code !== 0) {
    if (optional) {
      console.warn(`[refresh:live] WARN: ${label} failed (exit ${code}) after ${elapsed}s — continuing.`);
      return false;
    }
    console.error(`[refresh:live] ${label} failed (exit ${code}) after ${elapsed}s`);
    process.exit(code || 1);
  }
  console.log(`[refresh:live] ${label} — ${elapsed}s`);
  return true;
}

function mirrorPublishArtifacts() {
  const destDir = path.join(REPO_ROOT, "website", "public", "data");
  mkdirSync(destDir, { recursive: true });
  const pairs = [
    [path.join(WEB_ROOT, "live-in-play.json"), path.join(destDir, "live-in-play.json")],
    [path.join(WEB_ROOT, "projections.json"), path.join(destDir, "projections.json")],
    [path.join(WEB_ROOT, "course-table.json"), path.join(destDir, "course-table.json")],
    [
      path.join(WEB_ROOT, "paper-book", "paper-book-lines.json"),
      path.join(REPO_ROOT, "website", "public", "paper-book", "paper-book-lines.json"),
    ],
  ];
  console.log("\n[refresh:live] Mirroring publish artifacts …\n");
  for (const [src, dest] of pairs) {
    if (!existsSync(src)) {
      console.log(`[refresh:live] skip (missing): ${path.basename(src)}`);
      continue;
    }
    mkdirSync(path.dirname(dest), { recursive: true });
    copyFileSync(src, dest);
    console.log(`[refresh:live]   ${path.relative(REPO_ROOT, src)} → ${path.relative(REPO_ROOT, dest)}`);
  }
}

const liveWeekSoft = envTruthy("GOLF_LIVE_WEEK_SOFT", true);
const softOpt = liveWeekSoft ? { optional: true } : {};
const ouInc = resolveOuIncrementalSinceIso({ overlapDays: 2, fallbackDays: 14 });
const rebuildPriorVsActual = envTruthy("GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS", false);

console.log(
  "\n[refresh:live] Core live publish: projections + book odds + prior-round tab data.\n" +
    "  (Full pipeline: npm run refresh:live:full)\n",
);

// —— Projections ——
run("fetch-datagolf.mjs", "DataGolf field + projections (fetch:dg)");
run("build-course-table-json.mjs", "Course table JSON (build:course-table)");

// —— Live + prior round (Live Stats / Trends inputs) ——
run("fetch-live-in-play.mjs", "Live in-play + LTS + round actuals → live-in-play.json");
run(
  "run-refresh-pgatour-event-rounds.mjs",
  "pgatouR scorecards for current event (prior-round birdies/pars)",
  {},
  softOpt,
);

// —— Sportsbook odds (+ paper-book-lines.json bake inside fetch:book-odds) ——
run(
  "fetch-book-odds-into-projections.mjs",
  "Sportsbook odds + DK/PP/SL/UD round props + paper book bake (fetch:book-odds)",
);

// —— Prior-round into projections (Round Projections / +EV tabs) ——
run("merge-live-round-meta-into-projections.mjs", "Display round + prior-round course difficulty");
run("merge-field-teetimes-into-projections.mjs", "Tee times → projections", {}, softOpt);
run("repair-projection-course-basis.mjs", "Venue player/course history blend", {}, softOpt);
run(
  "within-event-projection-apply.mjs",
  "Prior-round form from live-in-play",
  { GOLF_WITHIN_EVENT_LIVE_ONLY: "1" },
  softOpt,
);
run("apply-unified-projection-factors.mjs", "Course fit + tee wave on projections", {}, softOpt);
run("merge-live-in-play-scratch-into-projections.mjs", "Live thru/scores → projections", {}, softOpt);
run("reconcile-projection-counts.mjs", "Reconcile counting stats", {}, softOpt);

// —— Tab data: projection tracker O/U CSV ——
if (!envTruthy("GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL", false)) {
  run(
    "export-round-projection-vs-actual-csv.mjs",
    rebuildPriorVsActual
      ? "Projection tracker O/U CSV (full prior rebuild + current week)"
      : `Projection tracker O/U CSV (incremental since ${ouInc.sinceIso})`,
    {
      GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS: rebuildPriorVsActual ? "1" : "0",
      GOLF_OU_BACKTEST_SINCE: rebuildPriorVsActual ? "" : ouInc.sinceIso,
    },
    softOpt,
  );
  run(
    "promote-round-projection-vs-actual-csv.mjs",
    "Promote round_projection_vs_actual.csv",
    {},
    softOpt,
  );
}

// —— Tab data: Historical Trends prior-round shards (field only, not full rebuild) ——
if (!envTruthy("GOLF_REFRESH_LIVE_SKIP_HISTORY_SHARDS", false)) {
  run("sync-field-history-from-csv.mjs", "CSV → field player-history shards", {}, softOpt);
  run("patch-current-event-history-shards.mjs", "Patch live rows into field history shards", {}, softOpt);
  run("rebuild-field-season-bundle.mjs", "Rebuild field-{year}.json for Trends", {}, softOpt);
}

run(
  "validate-projections-for-publish.mjs",
  "Validate projections before publish",
  liveWeekSoft ? { GOLF_LIVE_VALIDATE_SOFT: "1", GOLF_SKIP_DK_OU_VALIDATE: "1" } : {},
  softOpt,
);

mirrorPublishArtifacts();

console.log("\n[refresh:live] Done — projections, sportsbook odds, and prior-round tab data updated.");
console.log("[refresh:live] Publish: npm run push:live (or git push if already committed)\n");
