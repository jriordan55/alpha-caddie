#!/usr/bin/env node
/**
 * Run the per-tour live projection pipeline for one DataGolf tour (pga | euro).
 *
 * Env:
 *   GOLF_DATAGOLF_TOUR / GOLF_TOUR — required tour code (euro = DP World Tour)
 *   GOLF_SKIP_DK_OU=1 — skip book scrapes for non-PGA tours (default for euro)
 */
import path from "path";
import { fileURLToPath } from "url";
import { spawnSync } from "child_process";
import { copyFileSync, existsSync } from "fs";
import { dkOuScrapeEnv, liveProjectionPipelineEnv, requireDkOuEnv } from "./projection-pipeline-env.mjs";
import { resolveProjectionPaths } from "./projection-paths.mjs";
import { normalizeTourCode, tourDisplayLabel } from "./golf-tours.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

const tour = normalizeTourCode(process.env.GOLF_DATAGOLF_TOUR || process.env.GOLF_TOUR || "pga");
const paths = resolveProjectionPaths(WEB_ROOT, { ...process.env, GOLF_DATAGOLF_TOUR: tour });

function envTruthy(name, defaultVal) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function buildBaseEnv() {
  const pipeline = liveProjectionPipelineEnv();
  const skipDkForEuro = tour === "euro" && process.env.GOLF_SKIP_DK_OU === undefined;
  const e = {
    ...process.env,
    ...pipeline,
    ...dkOuScrapeEnv(),
    ...requireDkOuEnv(),
    GOLF_DATAGOLF_TOUR: tour,
    GOLF_TOUR: tour,
    GOLF_PROJECTIONS_FILE: paths.projectionsFile,
    GOLF_LIVE_IN_PLAY_FILE: paths.liveInPlayFile,
    GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT,
    GOLF_SKIP_HISTORY_ON_FETCH_DG: "1",
    GOLF_SKIP_OUTRIGHT_BAKE_ON_FETCH_DG: "1",
    GOLF_SKIP_SPORTSBOOK_OUTRIGHT_SCRAPE: tour === "euro" ? "1" : process.env.GOLF_SKIP_SPORTSBOOK_OUTRIGHT_SCRAPE,
    GOLF_DEFER_DK_ROUND_AUDIT_UNTIL_REPAIR: "1",
    GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX: "1",
    GOLF_SKIP_MARKET_BOOK_CALIBRATION: "1",
    GOLF_SKIP_PAPER_BOOK_BAKE: tour === "euro" ? "1" : "0",
    ...(skipDkForEuro
      ? {
          GOLF_SKIP_DK_OU: "1",
          GOLF_SKIP_PP_OU: "1",
          GOLF_SKIP_SL_OU: "1",
          GOLF_SKIP_UD_OU: "1",
          GOLF_SKIP_FD_OU: "1",
          GOLF_SKIP_KL_OU: "1",
          GOLF_SKIP_CZR_OU: "1",
          GOLF_REQUIRE_DK_OU: "0",
        }
      : {}),
  };
  for (const key of Object.keys(pipeline)) {
    if (process.env[key] !== undefined && String(process.env[key]).trim() !== "") {
      e[key] = process.env[key];
    }
  }
  delete e.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY;
  return e;
}

function run(rel, label, extraEnv = {}, opts = {}) {
  const optional = !!opts.optional;
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh-live-tour:${tour}] ${label}…\n`);
  const t0 = Date.now();
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...buildBaseEnv(), ...extraEnv },
  });
  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  if (r.status !== 0) {
    if (optional) {
      console.warn(`[refresh-live-tour:${tour}] WARN: ${label} failed (exit ${r.status}) after ${elapsed}s — continuing.`);
      return false;
    }
    console.error(`[refresh-live-tour:${tour}] ${label} failed (exit ${r.status}) after ${elapsed}s`);
    process.exit(r.status || 1);
  }
  console.log(`[refresh-live-tour:${tour}] ${label} — ${elapsed}s`);
  return true;
}

function mirrorLegacyAliases() {
  if (tour !== "pga") return;
  for (const [src, dest] of [
    [paths.projectionsPath, paths.legacyProjectionsPath],
    [paths.liveInPlayPath, paths.legacyLiveInPlayPath],
  ]) {
    if (!existsSync(src)) continue;
    copyFileSync(src, dest);
    console.log(`[refresh-live-tour:pga] mirrored ${path.basename(src)} → ${path.basename(dest)}`);
  }
}

const liveWeekSoft = envTruthy("GOLF_LIVE_WEEK_SOFT", true);
const softOpt = liveWeekSoft ? { optional: true } : {};

console.log(`\n[refresh-live-tour] ${tourDisplayLabel(tour)} (${tour}) → ${paths.projectionsFile}\n`);

run("fetch-datagolf.mjs", "DataGolf field + projections");
if (tour === "pga") {
  run("build-course-table-json.mjs", "Course table JSON", {}, softOpt);
}
run("fetch-live-in-play.mjs", "Live in-play bundle");
if (tour === "pga") {
  run("run-refresh-pgatour-event-rounds.mjs", "pgatouR scorecards", {}, softOpt);
}
run("fetch-book-odds-into-projections.mjs", "Sportsbook odds merge", {}, softOpt);
run("merge-live-round-meta-into-projections.mjs", "Display round + prior-round difficulty", {}, softOpt);
run("merge-field-teetimes-into-projections.mjs", "Tee times", {}, softOpt);
run("repair-projection-course-basis.mjs", "Venue history blend", {}, softOpt);
run(
  "within-event-projection-apply.mjs",
  "Prior-round form",
  { GOLF_WITHIN_EVENT_LIVE_ONLY: "1" },
  softOpt,
);
run("apply-unified-projection-factors.mjs", "Course fit + tee wave", {}, softOpt);
run("merge-live-in-play-scratch-into-projections.mjs", "Live thru/scores", {}, softOpt);
run("reconcile-projection-counts.mjs", "Reconcile counting stats", {}, softOpt);

mirrorLegacyAliases();
console.log(`\n[refresh-live-tour] Done: ${tourDisplayLabel(tour)} → ${paths.projectionsFile}\n`);
