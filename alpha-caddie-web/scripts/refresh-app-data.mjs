#!/usr/bin/env node
/**
 * Near-push:all data refresh without the push:all *full historical* mode.
 *
 *   npm run refresh:app
 *
 * Runs the same steps as scripts/refresh-history-and-push.ps1 except **git** commit/push/cache-bump
 * and except the default **GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1** behavior (that re-fetches every PGA
 * season from 2004 through current on each run). Here we default to a **recent-year-only** rounds merge
 * so DataGolf can still append/update birdies, round_score, pars, bogeys, GIR, fairways, putts, etc.
 * into `data/historical_rounds_all.csv`, then `build:history` rebuilds shards + embed (via update:rounds).
 *
 * Steps:
 *   1) fetch-datagolf.mjs — field, round projections, SG, live-hole-stats, outrights, matchups
 *      (skips the inline history CSV rebuild; step 7 does a controlled merge instead)
 *   2) build-course-table-json.mjs — course-table.json (same as npm run build:course-table)
 *   3) fetch-live-in-play.mjs — tee times + live feeds
 *   6b) bake-weather-into-projections.mjs — Open-Meteo weather baked into projections.json
 *   4) fetch-book-odds-into-projections.mjs — books + DK round O/U; appends data/dk_round_projection_audit.csv
 *      (Excel-friendly CSV: model_total_score, model_birdies, model_pars, model_bogeys, model_gir, …)
 *   5) fetch-datagolf-finish-tool-outrights.mjs
 *   6) merge-live-hole-pars-into-projections.mjs
 *   7) run-update-historical-rounds.mjs — merge recent seasons into historical_rounds_all.csv, then
 *      build-player-history + embed + build-player-shots-web + mirror CSVs into alpha-caddie-web/data
 *   8) Mirror JSON → website/public/data/ (projections, live-in-play, course-table, approach_skill*)
 *
 * Env (high signal):
 *   DATAGOLF_API_KEY or datagolf.local.json
 *   GOLF_MODEL_DIR — repo root (defaults to parent of alpha-caddie-web)
 *   GOLF_SKIP_HISTORY_ON_FETCH_DG=1 — default for step 1 only (avoid duplicate heavy merge before step 7)
 *   GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=N — default **3** when FULL_HISTORY is off (only re-fetch
 *     the last N calendar years from DataGolf; older CSV rows stay). Set **0** to omit and use the
 *     updater’s default year list (still not the same as push:all’s FULL_HISTORY=1 semantics).
 *   GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1 — same wide historical merge as push:all (slow; overrides recent-only default)
 *   GOLF_REFRESH_APP_SKIP_DG=1 — skip step 1–2 (odds/live/history only; projections must already exist)
 *   GOLF_SKIP_DK_OU=1 — skip DraftKings Playwright in fetch-book-odds
 *   GOLF_SKIP_DK_ROUND_AUDIT_CSV=1 — do not append dk_round_projection_audit.csv
 *
 * Speed (step 7 dominates on a large historical_rounds_all.csv — full file is still streamed once):
 *   GOLF_REFRESH_APP_SKIP_HISTORY=1 — skip step 7 entirely (keeps existing player_round_history / shards;
 *     use when you only need projections, live, odds, finish-tool, hole pars).
 *   GOLF_REFRESH_APP_FAST_HISTORY=1 — during step 7’s build:history: skip shots round-aggregate merge, and
 *     if you did not set GOLF_HISTORY_MIN_YEAR / GOLF_HISTORY_MAX_ROUNDS_PER_PLAYER, default to ~last
 *     10 seasons + 500 rounds/player (faster, less deep history in the JSON).
 *   CLI: `--no-history` | `--live-only` skip step 7 only (no patch:current-event-history). Prefer **`npm run refresh:live`**
 *     for the full live-week pipeline (odds + pgatouR patch on shards, no CSV rebuild).
 */
import { spawnSync } from "child_process";
import { copyFileSync, existsSync, mkdirSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { fastHistoryBuildEnv } from "./historical-rounds-merge-env.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

for (const a of process.argv.slice(2)) {
  if (a === "--no-history" || a === "--live-only") process.env.GOLF_REFRESH_APP_SKIP_HISTORY = "1";
  if (a === "--fast-history") process.env.GOLF_REFRESH_APP_FAST_HISTORY = "1";
}

function buildBaseEnv() {
  const e = { ...process.env, GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT };
  // Avoid inheriting a stray FULL_HISTORY=1 from the shell when user wants the lighter default.
  if (String(process.env.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY || "").trim() !== "1") {
    delete e.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY;
  }
  return e;
}

function run(rel, label, extraEnv = {}) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh:app] ${label}…\n`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...buildBaseEnv(), ...extraEnv },
  });
  if (r.status !== 0) {
    console.error(`[refresh:app] ${label} failed (exit ${r.status ?? "?"})`);
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
  console.log("\n[refresh:app] Mirroring JSON → website/public/data/ …\n");
  for (const name of files) {
    const src = path.join(WEB_ROOT, name);
    const dest = path.join(destDir, name);
    if (!existsSync(src)) {
      console.log(`[refresh:app] skip (missing): ${name}`);
      continue;
    }
    copyFileSync(src, dest);
    console.log(`[refresh:app]   ${name}`);
  }
}

function historyMergeEnv() {
  const full = String(process.env.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY || "").trim() === "1";
  if (full) {
    return { GOLF_HISTORICAL_ROUNDS_FULL_HISTORY: "1" };
  }
  const raw = String(process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS ?? "").trim();
  const n = raw === "" ? 3 : parseInt(raw, 10);
  if (Number.isFinite(n) && n > 0) {
    return { GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: String(n) };
  }
  return {};
}

const skipDg = String(process.env.GOLF_REFRESH_APP_SKIP_DG || "").trim() === "1";

if (!skipDg) {
  const skipHistRaw = process.env.GOLF_SKIP_HISTORY_ON_FETCH_DG;
  const skipHistDg =
    skipHistRaw != null && String(skipHistRaw).trim() !== ""
      ? String(skipHistRaw).trim()
      : "1";
  run("fetch-datagolf.mjs", "Field + model (fetch:dg)", {
    GOLF_SKIP_HISTORY_ON_FETCH_DG: skipHistDg,
  });
  run("build-course-table-json.mjs", "Course table JSON (build:course-table)");
} else {
  console.log("\n[refresh:app] GOLF_REFRESH_APP_SKIP_DG=1 — skipping fetch:dg + build:course-table.\n");
  if (!existsSync(path.join(WEB_ROOT, "projections.json"))) {
    console.error("[refresh:app] Missing projections.json; cannot skip fetch:dg.\n");
    process.exit(1);
  }
}

run("fetch-live-in-play.mjs", "Live / in-play (fetch:in-play)");
run("run-refresh-pgatour-event-rounds.mjs", "Current-event PGA rounds from pgatouR (refresh:pgatour-event)");
run("fetch-book-odds-into-projections.mjs", "Sportsbook + DK round props (fetch:book-odds)");
run("fetch-datagolf-finish-tool-outrights.mjs", "Finish-tool outrights (fetch:finish-tool)");
run("merge-live-hole-pars-into-projections.mjs", "Merge live hole pars into projections");
run("merge-live-round-meta-into-projections.mjs", "Merge live round meta into projections (display_round)");
run(
  "bake-weather-into-projections.mjs",
  "Open-Meteo tee-time weather → projections.json for display_round (bake:weather)",
);
run(
  "apply-unified-projection-factors.mjs",
  "Unified projection factors (course fit, tee wave, bounce-back, Sunday pressure, correlated markets)",
);
run(
  "bake-outright-sim-probs.mjs",
  "Tournament MC outright probs → projections.json (precomputed for +EV)",
);

const skipHistory = String(process.env.GOLF_REFRESH_APP_SKIP_HISTORY || "").trim() === "1";
if (skipHistory) {
  console.log(
    "\n[refresh:app] Skipping update:rounds + build:history (GOLF_REFRESH_APP_SKIP_HISTORY=1 or --no-history). Existing player_round_history.json / shards unchanged.\n",
  );
} else {
  const fh = fastHistoryBuildEnv();
  if (Object.keys(fh).length) {
    console.log(
      "\n[refresh:app] Fast history mode: skipping shots round-aggregate merge; default min_year ≈ last 10 seasons and max 500 rounds/player unless you set GOLF_HISTORY_*.\n",
    );
  }
  run(
    "run-update-historical-rounds.mjs",
    "Historical rounds + player history (update:rounds)",
    { ...historyMergeEnv(), ...fh },
  );
}

mirrorWebsitePublicData();

console.log(
  "\n[refresh:app] Done. Weather is baked into projections.json (bake:weather) — hard-refresh the browser if needed. No git commit/push (use push:all for that).\n",
);
