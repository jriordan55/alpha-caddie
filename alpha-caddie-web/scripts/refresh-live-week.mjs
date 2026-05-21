#!/usr/bin/env node
/**
 * Live-week refresh for `npm run push:live`:
 *   - DataGolf historical-raw-data/rounds → CSV (recent years, twice: before + after live fetch)
 *   - preds/live-tournament-stats (per round 1–4) + preds/in-play + field-updates → live-in-play.json
 *   - pgatouR scorecards when R is installed
 *   - fetch:dg projections (skill + preds/pre-tournament when pre; live stats when in play)
 *   - bake-weather-into-projections → Open-Meteo per-tee weather baked into projections.json
 *   - build-player-history → player_round_history.json + per-player + by-course shards + embed
 *
 *   npm run refresh:live
 *
 * Skips GOLF_HISTORICAL_ROUNDS_FULL_HISTORY (no 2004→present rebuild).
 *
 * Env: DATAGOLF_API_KEY, GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS (default 2),
 *   GOLF_REFRESH_LIVE_FAST_HISTORY=0 to scan full CSV depth (slower),
 *   GOLF_REFRESH_LIVE_SKIP_POST_CSV_MERGE=1 to skip the second CSV merge after live fetch.
 */
import { spawnSync } from "child_process";
import { copyFileSync, existsSync, mkdirSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { fastHistoryBuildEnv } from "./historical-rounds-merge-env.mjs";

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
const fh = fastHistoryBuildEnv({ defaultLiveFast: true });

run("merge-recent-historical-rounds.mjs", `DataGolf historical-raw-data/rounds → CSV (${recentYears}yr, pre-fetch)`, {
  GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: recentYears,
});

if (!skipDg) {
  run("fetch-datagolf.mjs", "Field + projections (μ_SG, preds/pre-tournament or live driving stats)", {
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

run(
  "fetch-live-in-play.mjs",
  "Live feeds: preds/in-play + field-updates + preds/live-tournament-stats (R1–R4) → live-in-play.json",
);
run("run-refresh-pgatour-event-rounds.mjs", "pgatouR scorecards for current event (refresh:pgatour-event)");
run("fetch-book-odds-into-projections.mjs", "Sportsbook + DK round props (fetch:book-odds)");
run("fetch-datagolf-finish-tool-outrights.mjs", "Finish-tool outrights (fetch:finish-tool)");
run("merge-live-hole-pars-into-projections.mjs", "Merge live hole pars into projections");
run("merge-live-round-meta-into-projections.mjs", "Merge live round meta into projections");
run(
  "bake-weather-into-projections.mjs",
  "Open-Meteo tee-time weather → projections.json (bake:weather)",
);

if (String(process.env.GOLF_REFRESH_LIVE_SKIP_POST_CSV_MERGE || "").trim() !== "1") {
  run(
    "merge-recent-historical-rounds.mjs",
    `DataGolf historical-raw-data/rounds → CSV (${recentYears}yr, post-live — completed rounds archive)`,
    { GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: recentYears },
  );
}

if (Object.keys(fh).length) {
  console.log(
    "\n[refresh:live] Fast history build: skipping shots merge + 170MB hole_data.csv scan; ~last 10 seasons / 500 rounds per player. GOLF_REFRESH_LIVE_FAST_HISTORY=0 for full depth; GOLF_BUILD_HISTORY_SKIP_HOLES=0 to include holes.\n",
  );
}

run(
  "build-player-history.mjs",
  "Historical Trends: CSV + live-tournament-stats + pgatouR → player_round_history + shards",
  fh,
);
run("embed-player-history.mjs", "Embed history for static deploy (embed:history)");
run("export-round-projection-vs-actual-csv.mjs", "Round projection vs actual CSV (export:round-projection-vs-actual)");

mirrorWebsitePublicData();

console.log(
  "\n[refresh:live] Done — Historical Trends uses player_round_history.json / by-course shards built from:",
);
console.log(
  "  • historical-raw-data/rounds (CSV), preds/live-tournament-stats + in-play (live week), pgatouR when available.",
);
console.log(
  "  • Projections from fetch:dg (preds/pre-tournament pre-event; live stats + historical calibration in play).",
);
console.log(
  "[refresh:live] For full archive (2004→present), use push:all or GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1.",
);
console.log("[refresh:live] Hard-refresh the browser (Ctrl+Shift+R). Publish: npm run push:live\n");
