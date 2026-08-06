#!/usr/bin/env node
/**
 * Live-week refresh for `npm run push:live` — projections, live-in-play, book odds,
 * DraftKings + PrizePicks + Sleeper + Underdog + FanDuel + Kalshi + Caesars round props, tee times, venue repair,
 * within-event form, weather bake, unified factors, odds audit CSVs, vs-actual (projection tracker),
 * and prior-round Trends patches.
 *
 * Round O/U counting markets match sportsbooks:
 *   Birdies = birdies + eagles (Birdies or Better)
 *   Bogeys  = bogeys + doubles (Bogeys or Worse)
 * plus venue / SG rates, AM/PM live_hole_stats wave, weather, and pin bake.
 *
 *   npm run refresh:live
 *
 * **Default (push:live lean):** projections + odds + current-event Trends patch + tracker.
 * Skips pre-fetch full CSV merge, weather archive backfill, and full build-player-history.
 * After live feeds: merges recent DataGolf rounds into CSV (post-live), then Trends via
 * sync-field-history + patch-current-event-history (live-in-play + matching pgatouR).
 *
 * Full rebuild (slow, ~20-30 min) — use push:all or:
 *   GOLF_REFRESH_LIVE_FULL_REBUILD=1
 *
 * Other env: DATAGOLF_API_KEY, GOLF_SKIP_PIN_SHEET=1, GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL=1,
 *   GOLF_SKIP_MARKET_BOOK_CALIBRATION=1, GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX=1 (default on live refresh),
 *   GOLF_REFRESH_LIVE_SKIP_DG=1, GOLF_REFRESH_LIVE_SKIP_PGATOUR=1,
 *   GOLF_SKIP_BACKTEST_ODDS_MODEL_ROI=1, GOLF_SKIP_DK_ROUND_AUDIT_CSV=1
 *   GOLF_REQUIRE_DK_OU=1 (default on refresh:live) — abort if DK scrape returns 0 fresh props
 *   GOLF_LIVE_WEEK_SOFT=1 (push:live) — soft DK require, skip odds ROI backtest,
 *     soft validate / optional late steps (never abort mid-tournament).
 *     O/U + matchup trackers: incremental from last recorded date by default
 *     (set GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS=1 for full O/U prior rebuild).
 *   GOLF_OU_BACKTEST_SINCE=YYYY-MM-DD — override O/U incremental watermark
 *   GOLF_MATCHUP_BACKTEST_SINCE=YYYY-MM-DD — override matchup incremental watermark
 *   GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS=0 — O/U incremental (default on push:live soft)
 *   GOLF_SKIP_PP_OU=1 — skip PrizePicks round props in fetch:book-odds
 *   GOLF_SKIP_SL_OU=1 — skip Sleeper round props in fetch:book-odds
 *   GOLF_SKIP_UD_OU=1 — skip Underdog round props in fetch:book-odds
 *   GOLF_SKIP_FD_OU=1 — skip FanDuel round props in fetch:book-odds
 *   GOLF_SKIP_KL_OU=1 — skip Kalshi round props in fetch:book-odds
 *   GOLF_SKIP_CZR_OU=1 — skip Caesars round props in fetch:book-odds
 *   GOLF_REQUIRE_PP_OU=1 / GOLF_REQUIRE_SL_OU=1 / GOLF_REQUIRE_UD_OU=1 — abort if 0 fresh props
 *   GOLF_REQUIRE_FD_OU=1 / GOLF_REQUIRE_KL_OU=1 / GOLF_REQUIRE_CZR_OU=1 — abort if 0 fresh props (optional; soft on push:live)
 *   GOLF_SKIP_DK_OU_VALIDATE=1 — skip DK line-count gate (pre-tournament only)
 *   DK_HEADLESS / CZR_HEADLESS / FD_HEADLESS =0 on Windows/macOS (dkOuScrapeEnv) — required for book sessions
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
import { resolveMatchupIncrementalSinceIso, resolveOuIncrementalSinceIso } from "./tracker-incremental.mjs";

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
  };
  // push:live sets flat-venue / wave weights on process.env before refresh:live — never clobber those.
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
 * @param {{ optional?: boolean }} [opts] — optional: warn and continue on non-zero / crash
 * @returns {boolean}
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
  const crashed = code == null && !!r.signal;
  if (code !== 0 || crashed) {
    const detail = crashed
      ? `signal ${r.signal}`
      : `exit ${code ?? "?"}`;
    if (optional) {
      console.warn(
        `[refresh:live] WARN: ${label} failed (${detail}) after ${elapsed}s — continuing live publish.`,
      );
      return false;
    }
    console.error(`[refresh:live] ${label} failed (${detail}) after ${elapsed}s`);
    process.exit(typeof code === "number" && code !== 0 ? code : 1);
  }
  console.log(`[refresh:live] ${label} — ${elapsed}s`);
  return true;
}

/** Always refresh DG field-updates, tee times, and Open-Meteo weather (never skipped on push:live). */
function runWeatherAndTeeTimesPass(phase, opts = {}) {
  const soft = opts.optional === true;
  run("refresh-field-updates-into-live.mjs", `Fresh field-updates → live-in-play (${phase})`, {}, soft ? { optional: true } : {});
  run(
    "merge-field-teetimes-into-projections.mjs",
    `field-updates tee times → projections.json (${phase})`,
    {},
    soft ? { optional: true } : {},
  );
  // Live Open-Meteo bake is required on every push:live — never optional.
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
const liveWeekSoft = envTruthy("GOLF_LIVE_WEEK_SOFT", false);
const skipCsvMerge =
  fullRebuild ? false : envTruthy("GOLF_REFRESH_LIVE_SKIP_CSV_MERGE", true);
/** Lean default: still merge recent DG rounds after live-in-play so Trends gets completed rounds. */
const skipPostCsvMerge =
  fullRebuild ? false : envTruthy("GOLF_REFRESH_LIVE_SKIP_POST_CSV_MERGE", false);
const skipHistoryRebuild =
  fullRebuild ? false : envTruthy("GOLF_REFRESH_LIVE_SKIP_HISTORY_REBUILD", true);
const skipWeatherBackfill =
  fullRebuild ? false : envTruthy("GOLF_SKIP_ROUND_WEATHER_BACKFILL", true);
const skipDg = envTruthy("GOLF_REFRESH_LIVE_SKIP_DG", false);
const skipPgatour = envTruthy("GOLF_REFRESH_LIVE_SKIP_PGATOUR", false);
const skipFinishTool = envTruthy("GOLF_REFRESH_LIVE_SKIP_FINISH_TOOL", true);
/** Mid-tournament soft: skip heavy odds ROI walk-forward (OOM / exit -1 on Windows). */
const skipBacktestRoi = envTruthy("GOLF_SKIP_BACKTEST_ODDS_MODEL_ROI", liveWeekSoft);
/**
 * Lean push:live keeps older tracker history cached and incrementally refreshes
 * from each tracker's last recorded date (full rebuild: GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS=1).
 */
const rebuildPriorVsActual = liveWeekSoft
  ? envTruthy("GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS", false)
  : envTruthy("GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS", true);
/** Both trackers: refresh from last recorded close/export date (+2d overlap). */
const matchupInc = resolveMatchupIncrementalSinceIso({ overlapDays: 2, fallbackDays: 14 });
const matchupSinceIso = matchupInc.sinceIso;
const ouInc = resolveOuIncrementalSinceIso({ overlapDays: 2, fallbackDays: 14 });
const ouSinceIso = ouInc.sinceIso;
const failOnParMismatch = liveWeekSoft
  ? envTruthy("GOLF_FAIL_ON_PAR_MISMATCH", false)
  : envTruthy("GOLF_FAIL_ON_PAR_MISMATCH", true);
const softOpt = liveWeekSoft ? { optional: true } : {};
/** Soft mid-tournament: heavy steps warn-and-continue. Hard fail only on lean non-soft runs. */
const heavyOpt = liveWeekSoft ? { optional: true } : {};
/** Nightly CI: force O/U + matchup tracker CSVs to refresh (do not soft-skip). */
const requireTrackers = envTruthy("GOLF_REQUIRE_TRACKER_REFRESH", false);
const trackerOpt = requireTrackers ? {} : heavyOpt;
const trackerOddsOpt = requireTrackers ? softOpt : softOpt;
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
  console.log("\n[refresh:live] GOLF_REFRESH_LIVE_FULL_REBUILD=1 — including CSV merge + history + weather archive backfill.\n");
  recentYears = String(process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "25").trim();
} else {
  console.log(
    "\n[refresh:live] Lean live-week: projections + odds + Trends + tracker. Live Open-Meteo weather bake always runs (archive backfill skipped).\n",
  );
}
if (liveWeekSoft) {
  console.log(
    "[refresh:live] GOLF_LIVE_WEEK_SOFT=1 — soft DK require, skip odds ROI backtest, optional late steps." +
      (rebuildPriorVsActual
        ? " Prior O/U vs-actual FULL rebuild ON.\n"
        : " Prior O/U vs-actual incremental from last recorded date.\n"),
  );
  console.log(
    `[refresh:live] O/U tracker incremental since ${ouSinceIso}` +
      (ouInc.lastRecordedIso
        ? ` (last recorded ${ouInc.lastRecordedIso} via ${ouInc.source})`
        : ` (${ouInc.source})`) +
      ".\n",
  );
  console.log(
    `[refresh:live] Matchup tracker incremental since ${matchupSinceIso}` +
      (matchupInc.lastRecordedIso
        ? ` (last recorded ${matchupInc.lastRecordedIso} via ${matchupInc.source})`
        : ` (${matchupInc.source})`) +
      ".\n",
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
  run(
    "run-refresh-pgatour-event-rounds.mjs",
    "pgatouR scorecards for current event (refresh:pgatour-event)",
    {},
    softOpt,
  );
} else {
  console.log("[refresh:live] Skipping pgatouR refresh.\n");
}

run(
  "fetch-book-odds-into-projections.mjs",
  "Sportsbook + DK + PP + SL + UD + FanDuel + Kalshi + Caesars round props (fetch:book-odds)",
  liveFastEnv,
  softOpt,
);
if (skipFinishTool) {
  console.log("[refresh:live] Skipping fetch:finish-tool (outrights already from fetch:dg + book-odds). Set GOLF_REFRESH_LIVE_SKIP_FINISH_TOOL=0 to re-run.\n");
} else {
  run("fetch-datagolf-finish-tool-outrights.mjs", "Finish-tool outrights (fetch:finish-tool)");
}
run("merge-live-hole-pars-into-projections.mjs", "Merge live hole pars into projections");
run("sync-bundled-hole-pars-into-projections.mjs", "Bundled course_holes.json → projections when live pars missing/wrong");
run(
  "check-hole-pars-resolved.mjs",
  "Fail fast if hole pars are still the generic fallback (new venue needs course_holes.json)",
  {},
  softOpt,
);
run(
  "ensure-projection-course-par.mjs",
  "Lock course_par_18 from hole card + score↔par coherence (before venue repair)",
  { GOLF_FAIL_ON_PAR_MISMATCH: failOnParMismatch ? "1" : "0" },
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
    {},
    softOpt,
  );
  run("sync-pin-locations.mjs", "Mirror pin_locations DB → alpha-caddie-web/data (after tee sheet save)", {}, softOpt);
}

if (!envTruthy("GOLF_SKIP_DK_ROUND_AUDIT_CSV", false)) {
  run(
    "export-dk-round-model-audit-csv.mjs",
    "DK round audit CSV with post-repair model lines (model_total_score, birdies, …)",
    {},
    softOpt,
  );
}

if (!envTruthy("GOLF_SKIP_PP_ROUND_AUDIT_CSV", false)) {
  run(
    "export-pp-round-model-audit-csv.mjs",
    "PrizePicks round audit CSV (all PP lines/odds + model snapshots)",
    {},
    softOpt,
  );
}

if (!envTruthy("GOLF_SKIP_SL_ROUND_AUDIT_CSV", false)) {
  run(
    "export-sl-round-model-audit-csv.mjs",
    "Sleeper round audit CSV (all SL lines/odds + model snapshots)",
    {},
    softOpt,
  );
}

if (!envTruthy("GOLF_SKIP_UD_ROUND_AUDIT_CSV", false)) {
  run(
    "export-ud-round-model-audit-csv.mjs",
    "Underdog round audit CSV (all UD lines/odds + model snapshots)",
    {},
    softOpt,
  );
}

if (!envTruthy("GOLF_SKIP_FD_ROUND_AUDIT_CSV", false)) {
  run(
    "export-fd-round-model-audit-csv.mjs",
    "FanDuel round audit CSV (all FD lines/odds + model snapshots)",
    {},
    softOpt,
  );
}

if (!envTruthy("GOLF_SKIP_CZR_ROUND_AUDIT_CSV", false)) {
  run(
    "export-czr-round-model-audit-csv.mjs",
    "Caesars round audit CSV (all CZR lines/odds + model snapshots)",
    {},
    softOpt,
  );
}

if (!envTruthy("GOLF_SKIP_KL_ROUND_AUDIT_CSV", false)) {
  run(
    "export-kl-round-model-audit-csv.mjs",
    "Kalshi round audit CSV (all KL lines/odds + model snapshots)",
    {},
    softOpt,
  );
}

run(
  "bake-outright-sim-probs.mjs",
  "Tournament MC outright probs → projections.json (precomputed for +EV)",
);

run(
  "reconcile-projection-counts.mjs",
  "Score-anchored counts + venue field markets (pre book-cal / weather publish)",
);

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
    { GOLF_FAIL_ON_PAR_MISMATCH: failOnParMismatch ? "1" : "0" },
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

runWeatherAndTeeTimesPass("publish");

// Re-apply unified factors AFTER weather/tee-time bake so DG live-hole-stats AM/PM wave
// + bird/bog recenter use current tee times / forecast slots.
run(
  "apply-unified-projection-factors.mjs",
  "Re-apply unified factors after publish weather (DG live-hole-stats AM/PM wave + bird/bog)",
);

// Final required Open-Meteo bake so weather difficulty always lands on the published scores
// (unified factors restore pre-weather baselines, then this re-applies live forecast).
run(
  "bake-weather-into-projections.mjs",
  "Final Open-Meteo weather bake into projections (required on every push:live)",
);

run(
  "reconcile-projection-counts.mjs",
  "Reconcile counting stats after publish weather bake (before validate)",
);

// Tracker backtest AFTER final weather/unified so current-week model lines match published
// projections. Both trackers (O/U projection-tracker + matchup-tracker) refresh here.
if (!envTruthy("GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL", false)) {
  run(
    "export-round-projection-vs-actual-csv.mjs",
    rebuildPriorVsActual
      ? "Projection tracker O/U CSV (full walkforward prior rebuild + current week)"
      : `Projection tracker O/U CSV (incremental since ${ouSinceIso} + current week)`,
    {
      ...liveFastEnv,
      GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS: rebuildPriorVsActual ? "1" : "0",
      GOLF_OU_BACKTEST_SINCE: rebuildPriorVsActual ? "" : ouSinceIso,
    },
    trackerOpt,
  );
  run(
    "promote-round-projection-vs-actual-csv.mjs",
    "Publish round_projection_vs_actual.csv (promote .new if Excel had file open)",
    {},
    trackerOpt,
  );
  // Keep matchup tracker current from last recorded date (all tabs: Overview/EV/Bet log + live Best bets).
  if (!envTruthy("GOLF_SKIP_MATCHUP_ODDS_UPDATE", false)) {
    run(
      "update-historical-odds-node.mjs",
      `DataGolf historical matchups since ${matchupSinceIso} (DK/FD/BetMGM)`,
      {
        GOLF_MATCHUPS_BOOKS: "draftkings,fanduel,betmgm",
        GOLF_ODDS_SKIP_OUTRIGHTS: "1",
        GOLF_ODDS_SINCE: matchupSinceIso,
      },
      trackerOddsOpt,
    );
  }
  run(
    "export-matchup-backtest-csv.mjs",
    `Matchup tracker CSV (round matchups + 3-balls) incremental since ${matchupSinceIso}`,
    {
      GOLF_MATCHUP_BACKTEST_SINCE: matchupSinceIso,
    },
    trackerOpt,
  );
  run(
    "build-parlay-correlations.mjs",
    "Parlay Pro leg co-hit correlations → parlay_correlations.json",
    {},
    softOpt,
  );
  run(
    "report-walkforward-oos-roi.mjs",
    "Walk-forward OOS ROI report → walkforward_oos_roi.json (projection-tracker Overview)",
    {},
    trackerOpt,
  );
}

run(
  "validate-projections-for-publish.mjs",
  "Validate par, birdies/pars, and O/U prop coverage before publish",
  liveWeekSoft ? { GOLF_LIVE_VALIDATE_SOFT: "1", GOLF_SKIP_DK_OU_VALIDATE: "1" } : {},
  softOpt,
);
run(
  "verify-pp-round-props.mjs",
  "PrizePicks field alignment + projection-tracker PP columns",
  {},
  softOpt,
);

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

// Approach SG by distance/lie (from shot traces) — refresh shots then upsert current event.
{
  const skipSgDist = String(process.env.GOLF_SKIP_SG_DISTANCE || "").trim() === "1";
  if (skipSgDist) {
    console.log("[refresh:live] Skipping round SG-by-distance (GOLF_SKIP_SG_DISTANCE=1).\n");
  } else {
    run(
      "run-update-latest-shots.mjs",
      "Append latest pgatouR shot traces → all_shots (for distance-bucket SG)",
      {},
      softOpt,
    );
    run(
      "build-round-sg-by-distance.mjs",
      "Round approach SG by distance/lie buckets (current event upsert)",
      { GOLF_SG_DISTANCE_LIVE: "1" },
      softOpt,
    );
    run(
      "build-round-sg-putt-by-distance.mjs",
      "Round putting SG by distance buckets (current event upsert)",
      { GOLF_SG_PUTT_DISTANCE_LIVE: "1" },
      softOpt,
    );
    // Full rebuild of player×course×hole SG (opt-in; ~1–2 min). Default off for live speed.
    if (String(process.env.GOLF_BUILD_COURSE_HOLE_SG || "").trim() === "1") {
      run(
        "build-player-course-hole-sg.mjs",
        "Player × course × hole strokes gained (from shot hole scores)",
        {},
        softOpt,
      );
    }
  }
}

if (!skipBacktestRoi) {
  run(
    "backtest-odds-model-roi.mjs",
    "Odds.csv model ROI backtest (walkforward venue-history projections)",
    {},
    heavyOpt,
  );
} else {
  console.log(
    "[refresh:live] Skipping odds-model ROI backtest (GOLF_SKIP_BACKTEST_ODDS_MODEL_ROI or live soft / non-full rebuild).\n",
  );
}

if (skipHistoryRebuild) {
  run(
    "sync-field-history-from-csv.mjs",
    "Merge CSV rounds into field + recent prior-event player-history shards",
  );
  run(
    "patch-current-event-history-shards.mjs",
    "Patch current-event live rows into player-history shards (no CSV rescan)",
  );
  run(
    "merge-round-sg-distance-into-history.mjs",
    "Merge approach SG-by-distance buckets onto field history shards",
    {},
    softOpt,
  );
  run(
    "verify-recent-event-history.mjs",
    "Verify latest completed event rounds are on Historical Trends shards",
    { GOLF_HISTORY_VERIFY_SOFT: liveWeekSoft ? "1" : "0" },
    softOpt,
  );
  run("rebuild-field-season-bundle.mjs", "Rebuild field-{year}.json for Historical Trends");
  run(
    "build-course-history-shards.mjs",
    "Course history shards for At-this-course O/U averages (all years from 2004)",
    { GOLF_HISTORY_MIN_YEAR: "2004" },
    softOpt,
  );
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
  run(
    "merge-round-sg-distance-into-history.mjs",
    "Merge approach SG-by-distance buckets onto history shards",
    { GOLF_SG_DISTANCE_MERGE_FIELD_ONLY: "0" },
    softOpt,
  );
  run("embed-player-history.mjs", "Embed history for static deploy (embed:history)");
  run(
    "build-course-history-shards.mjs",
    "Course history shards for At-this-course O/U averages",
    { GOLF_HISTORY_MIN_YEAR: "2004" },
  );
}

run("verify-ou-round-projection-means.mjs", "Guard Round Projections Proj μ (no in-play collapse)", {}, softOpt);
run("verify-ou-proj-avg.mjs", "Guard Round Projections course averages vs Course Fit", {}, softOpt);

if (String(process.env.GOLF_DG_METHODOLOGY || "1").trim() !== "0") {
  run(
    "apply-dg-methodology-to-projections.mjs",
    "DataGolf predictive methodology μ (apply:dg-methodology)",
    {},
    softOpt,
  );
  run(
    "apply-both-side-bias-to-projections.mjs",
    "Both-side chrono/loo μ bias (apply:both-side-bias)",
    {},
    softOpt,
  );
  run(
    "apply-both-side-bet-signals-to-projections.mjs",
    "Tracker bet YES/NO on DK props + round_projections.csv",
    {},
    softOpt,
  );
}

mirrorWebsitePublicData();

console.log("\n[refresh:live] Done.");
if (skipHistoryRebuild) {
  console.log("  • Live week updated; field shards + field-{year}.json refreshed for Historical Trends.");
  console.log("  • Full CSV/history rebuild: GOLF_REFRESH_LIVE_FULL_REBUILD=1 npm run refresh:live");
} else {
  console.log("  • Historical Trends rebuilt from CSV + live feeds.");
}
console.log("[refresh:live] Hard-refresh the browser (Ctrl+Shift+R). Publish: npm run push:live\n");
