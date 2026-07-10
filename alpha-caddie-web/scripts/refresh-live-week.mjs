#!/usr/bin/env node
/**
 * Live-week refresh for `npm run push:live` — updates projections, live-in-play, book odds,
 * DraftKings + PrizePicks round props, field-updates tee times, venue history + skill repair, within-event form, weather bake,
 * unified factors, pin sheet, venue field reconcile (+ fairway driving-acc refresh), vs-actual export, walk-forward OOS report
 * (fit on prior events → apply to live projections), Parlay Pro correlations, walk-forward OOS report, and fast history patches.
 *
 *   npm run refresh:live
 *
 * **Default (push:live): no full rebuild.** Skips historical CSV merge, weather archive backfill,
 * and build-player-history / embed. Committed player_round_history + pin_locations stay on disk.
 *
 * Opt-in full rebuild (slow, ~20–30 min):
 *   GOLF_REFRESH_LIVE_FULL_REBUILD=1
 *
 * Other env: DATAGOLF_API_KEY, GOLF_SKIP_PIN_SHEET=1, GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL=1,
 *   GOLF_SKIP_MARKET_BOOK_CALIBRATION=1, GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX=1 (default on live refresh),
 *   GOLF_REFRESH_LIVE_SKIP_DG=1, GOLF_REFRESH_LIVE_SKIP_PGATOUR=1,
 *   GOLF_SKIP_BACKTEST_ODDS_MODEL_ROI=1, GOLF_SKIP_DK_ROUND_AUDIT_CSV=1
 *   GOLF_REQUIRE_DK_OU=1 (default on refresh:live) — abort if DK scrape returns 0 fresh props
 *   GOLF_SKIP_PP_OU=1 — skip PrizePicks round props in fetch:book-odds
 *   GOLF_REQUIRE_PP_OU=1 — abort if PrizePicks fetch returns 0 fresh props (optional)
 *   GOLF_SKIP_DK_OU_VALIDATE=1 — skip DK line-count gate (pre-tournament only)
 *   DK_HEADLESS=0 on Windows/macOS (dkOuScrapeEnv) — required for Nash API session
 */
import { spawnSync } from "child_process";
import { copyFileSync, existsSync, mkdirSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { fastHistoryBuildEnv } from "./historical-rounds-merge-env.mjs";
import {
  dkOuScrapeEnv,
  liveProjectionPipelineEnv,
  requireDkOuEnv,
} from "./projection-pipeline-env.mjs";

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
  const e = {
    ...process.env,
    ...liveProjectionPipelineEnv(),
    ...dkOuScrapeEnv(),
    ...requireDkOuEnv(),
    GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT,
  };
  delete e.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY;
  return e;
}

function run(rel, label, extraEnv = {}) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh:live] ${label}…\n`);
  const t0 = Date.now();
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...buildBaseEnv(), ...extraEnv },
  });
  const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
  if (r.status !== 0) {
    console.error(`[refresh:live] ${label} failed (exit ${r.status ?? "?"}) after ${elapsed}s`);
    process.exit(r.status ?? 1);
  }
  console.log(`[refresh:live] ${label} — ${elapsed}s`);
}

/** Always refresh DG field-updates, tee times, and Open-Meteo weather (never skipped on push:live). */
function runWeatherAndTeeTimesPass(phase) {
  run("refresh-field-updates-into-live.mjs", `Fresh field-updates → live-in-play (${phase})`);
  run(
    "merge-field-teetimes-into-projections.mjs",
    `field-updates tee times → projections.json (${phase})`,
  );
  run(
    "bake-weather-into-projections.mjs",
    `Open-Meteo tee-time weather → projections.json (${phase})`,
  );
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
  const dataFiles = ["parlay_correlations.json"];
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
  for (const name of dataFiles) {
    const src = path.join(WEB_ROOT, "data", name);
    const dest = path.join(destDir, name);
    if (!existsSync(src)) {
      console.log(`[refresh:live] skip (missing): data/${name}`);
      continue;
    }
    copyFileSync(src, dest);
    console.log(`[refresh:live]   data/${name}`);
  }
}

const fullRebuild = envTruthy("GOLF_REFRESH_LIVE_FULL_REBUILD", false);
const skipCsvMerge =
  fullRebuild ? false : envTruthy("GOLF_REFRESH_LIVE_SKIP_CSV_MERGE", true);
const skipPostCsvMerge =
  fullRebuild ? false : envTruthy("GOLF_REFRESH_LIVE_SKIP_POST_CSV_MERGE", true);
const skipHistoryRebuild =
  fullRebuild ? false : envTruthy("GOLF_REFRESH_LIVE_SKIP_HISTORY_REBUILD", true);
const skipWeatherBackfill =
  fullRebuild ? false : envTruthy("GOLF_SKIP_ROUND_WEATHER_BACKFILL", true);
const skipDg = envTruthy("GOLF_REFRESH_LIVE_SKIP_DG", false);
const skipPgatour = envTruthy("GOLF_REFRESH_LIVE_SKIP_PGATOUR", false);
const skipFinishTool = envTruthy("GOLF_REFRESH_LIVE_SKIP_FINISH_TOOL", true);
const liveFastEnv = {
  ...liveProjectionPipelineEnv(),
  GOLF_SKIP_OUTRIGHT_BAKE_ON_FETCH_DG: "1",
  GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX: "1",
  GOLF_SKIP_SPORTSBOOK_OUTRIGHT_SCRAPE: "1",
  GOLF_DEFER_DK_ROUND_AUDIT_UNTIL_REPAIR: "1",
};
let recentYears = String(process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "2").trim();
const fh = fullRebuild ? {} : fastHistoryBuildEnv({ defaultLiveFast: true });

if (fullRebuild) {
  console.log("\n[refresh:live] GOLF_REFRESH_LIVE_FULL_REBUILD=1 — including CSV merge + history + weather backfill.\n");
  recentYears = String(process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "25").trim();
} else {
  console.log(
    "\n[refresh:live] Live-week update only (no CSV/history/weather rebuild). Set GOLF_REFRESH_LIVE_FULL_REBUILD=1 for full rebuild.\n",
  );
}

if (!skipCsvMerge) {
  run("merge-recent-historical-rounds.mjs", `DataGolf historical-raw-data/rounds → CSV (${recentYears}yr, pre-fetch)`, {
    GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: recentYears,
    ...(fullRebuild ? { GOLF_HISTORICAL_ROUNDS_FETCH_ALL_YEARS: "1" } : {}),
  });
} else {
  console.log("[refresh:live] Skipping pre-fetch CSV merge (using committed historical_rounds_all.csv).\n");
}

if (!skipDg) {
  run("fetch-datagolf.mjs", "Field + projections (μ_SG, preds/pre-tournament or live driving stats)", {
    GOLF_SKIP_HISTORY_ON_FETCH_DG: "1",
    ...liveFastEnv,
  });
  run("build-course-table-json.mjs", "Course table JSON for unified course-fit factors (build:course-table)");
} else {
  console.log("\n[refresh:live] GOLF_REFRESH_LIVE_SKIP_DG=1 — skipping fetch:dg + build:course-table.\n");
  if (!existsSync(path.join(WEB_ROOT, "projections.json"))) {
    console.error("[refresh:live] Missing projections.json; cannot skip fetch:dg.\n");
    process.exit(1);
  }
}

run(
  "fetch-live-in-play.mjs",
  "Live feeds: preds/in-play + field-updates + preds/live-tournament-stats (R1–R4) → live-in-play.json",
);

if (!skipPgatour) {
  run("run-refresh-pgatour-event-rounds.mjs", "pgatouR scorecards for current event (refresh:pgatour-event)");
} else {
  console.log("[refresh:live] Skipping pgatouR refresh.\n");
}

run("fetch-book-odds-into-projections.mjs", "Sportsbook + DK + PrizePicks round props (fetch:book-odds)", liveFastEnv);
if (skipFinishTool) {
  console.log("[refresh:live] Skipping fetch:finish-tool (outrights already from fetch:dg + book-odds). Set GOLF_REFRESH_LIVE_SKIP_FINISH_TOOL=0 to re-run.\n");
} else {
  run("fetch-datagolf-finish-tool-outrights.mjs", "Finish-tool outrights (fetch:finish-tool)");
}
run("merge-live-hole-pars-into-projections.mjs", "Merge live hole pars into projections");
run("sync-bundled-hole-pars-into-projections.mjs", "Bundled course_holes.json → projections when live pars missing/wrong");
run(
  "ensure-projection-course-par.mjs",
  "Lock course_par_18 from hole card + score↔par coherence (before venue repair)",
  { GOLF_FAIL_ON_PAR_MISMATCH: "1" },
);
run(
  "merge-live-round-meta-into-projections.mjs",
  "Merge live round meta into projections (display_round for upcoming round)",
);
runWeatherAndTeeTimesPass("pre-repair");
run(
  "repair-projection-course-basis.mjs",
  "Venue player/course history + skill blend + total-score calibration (before within-event / course-fit)",
);
run(
  "within-event-projection-apply.mjs",
  "Prior-round form from live-in-play (after venue repair so R2+ builds on venue+skill base)",
  { GOLF_WITHIN_EVENT_LIVE_ONLY: "1" },
);
run(
  "apply-unified-projection-factors.mjs",
  "Unified projection factors (course fit, tee wave, residuals)",
);
run(
  "merge-live-in-play-scratch-into-projections.mjs",
  "Live thru/today/current_score → projections + in_play_affects_round_odds for +EV",
);

if (!envTruthy("GOLF_SKIP_PIN_SHEET", false)) {
  run(
    "apply-pin-sheet-to-projections.mjs",
    "Pin sheet → projections (Bayesian calibrated) + pin_locations DB when armed",
  );
  run("sync-pin-locations.mjs", "Mirror pin_locations DB → alpha-caddie-web/data (after tee sheet save)");
}

if (!envTruthy("GOLF_SKIP_DK_ROUND_AUDIT_CSV", false)) {
  run(
    "export-dk-round-model-audit-csv.mjs",
    "DK round audit CSV with post-repair model lines (model_total_score, birdies, …)",
  );
}

if (!envTruthy("GOLF_SKIP_PP_ROUND_AUDIT_CSV", false)) {
  run(
    "export-pp-round-model-audit-csv.mjs",
    "PrizePicks round audit CSV (all PP lines/odds + model snapshots)",
  );
}

run(
  "bake-outright-sim-probs.mjs",
  "Tournament MC outright probs → projections.json (precomputed for +EV)",
);

run(
  "reconcile-projection-counts.mjs",
  "Final score-anchored counts + venue field markets (book cal after vs-actual export)",
);

if (!envTruthy("GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL", false)) {
  run(
    "export-round-projection-vs-actual-csv.mjs",
    "Round projection vs actual CSV (walkforward backtest + current week)",
    {
      ...liveFastEnv,
      GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS: "1",
    },
  );
  run(
    "promote-round-projection-vs-actual-csv.mjs",
    "Publish round_projection_vs_actual.csv (promote .new if Excel had file open)",
  );
  run("build-parlay-correlations.mjs", "Parlay Pro leg co-hit correlations → parlay_correlations.json");
}

if (!envTruthy("GOLF_SKIP_MARKET_BOOK_CALIBRATION", true)) {
  run(
    "fit-market-book-calibration.mjs",
    "Fit DK book-alignment (prior events only, no outcome peek) → market_book_calibration.json",
  );
  run(
    "apply-market-book-calibration.mjs",
    "Apply book-alignment shifts to projections.json for live week",
  );
  run(
    "ensure-projection-course-par.mjs",
    "Repair score↔par after book calibration",
    { GOLF_FAIL_ON_PAR_MISMATCH: "1" },
  );
  run(
    "reconcile-projection-counts.mjs",
    "Reconcile counting stats after book calibration (book cal already applied)",
  );
} else {
  run(
    "apply-market-book-calibration.mjs",
    "Strip baked-in DK book shifts from projections.json (calibration disabled)",
  );
}

run(
  "report-walkforward-oos-roi.mjs",
  "Walk-forward OOS ROI report → walkforward_oos_roi.json",
);

runWeatherAndTeeTimesPass("publish");

run(
  "reconcile-projection-counts.mjs",
  "Reconcile counting stats after publish weather bake (before validate)",
);

run("validate-projections-for-publish.mjs", "Validate par, birdies/pars, and O/U prop coverage before publish");
run("verify-pp-round-props.mjs", "PrizePicks field alignment + projection-tracker PP columns");

if (!skipPostCsvMerge) {
  run(
    "merge-recent-historical-rounds.mjs",
    `DataGolf historical-raw-data/rounds → CSV (${recentYears}yr, post-live — completed rounds archive)`,
    { GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: recentYears },
  );
  run(
    "refresh-pga-tour-market-benchmarks.mjs",
    "PGA Tour 2025–2026 market benchmarks → projections.json (Market rating)",
  );
} else {
  console.log(
    "[refresh:live] Skipping post-live CSV merge + market-benchmarks (fetch:dg already wrote benchmarks).\n",
  );
}

if (!envTruthy("GOLF_SKIP_BACKTEST_ODDS_MODEL_ROI", false)) {
  run(
    "backtest-odds-model-roi.mjs",
    "Odds.csv model ROI backtest (walkforward venue-history projections)",
  );
}

if (skipHistoryRebuild) {
  run(
    "sync-field-history-from-csv.mjs",
    "Merge recent CSV rounds into field player-history shards",
  );
  run(
    "patch-current-event-history-shards.mjs",
    "Patch current-event live rows into player-history shards (no CSV rescan)",
  );
  run("rebuild-field-season-bundle.mjs", "Rebuild field-{year}.json for Historical Trends");
  run("build-course-history-shards.mjs", "Course history shards for At-this-course O/U averages");
} else {
  if (Object.keys(fh).length) {
    console.log(
      "\n[refresh:live] Fast history build: ~last 10 seasons / 500 rounds per player. GOLF_REFRESH_LIVE_FAST_HISTORY=0 for full depth.\n",
    );
  }
  if (!skipWeatherBackfill) {
    run(
      "backfill-historical-round-weather.mjs",
      "Per-round historical weather (Open-Meteo archive → historical_round_weather.json)",
    );
  }
  run(
    "build-player-history.mjs",
    "Historical Trends: CSV + live-tournament-stats + pgatouR → player_round_history + shards",
    {
      ...fh,
      ...(fullRebuild
        ? {
            GOLF_HISTORY_MIN_YEAR: "2004",
            GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER: "2000",
            GOLF_REFRESH_LIVE_FAST_HISTORY: "0",
          }
        : {}),
    },
  );
  run("embed-player-history.mjs", "Embed history for static deploy (embed:history)");
  run("build-course-history-shards.mjs", "Course history shards for At-this-course O/U averages");
}

run("verify-ou-round-projection-means.mjs", "Guard Round Projections Proj μ (no in-play collapse)");
run("verify-ou-proj-avg.mjs", "Guard Round Projections course averages vs Course Fit");

mirrorWebsitePublicData();

console.log("\n[refresh:live] Done.");
if (skipHistoryRebuild) {
  console.log("  • Live week updated; field shards + field-{year}.json refreshed for Historical Trends.");
  console.log("  • Full CSV/history rebuild: GOLF_REFRESH_LIVE_FULL_REBUILD=1 npm run refresh:live");
} else {
  console.log("  • Historical Trends rebuilt from CSV + live feeds.");
}
console.log("[refresh:live] Hard-refresh the browser (Ctrl+Shift+R). Publish: npm run push:live\n");
