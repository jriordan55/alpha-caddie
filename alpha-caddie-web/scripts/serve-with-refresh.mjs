#!/usr/bin/env node
/**
 * npm start / npm run dev — before serving static files:
 *   1) Merge latest rounds into data/historical_rounds_all.csv (DataGolf, Node — no R)
 *   2) Rebuild player_round_history.json from that CSV
 *   3) Write embedded-player-round-history.js (same as npm run build:history tail)
 *
 * Live model / current strokes and book lines live in projections.json + live-in-play.json — refresh those
 * from repo root with: scripts/refresh_projections_between_rounds.ps1 (R), not this file.
 *
 * Historical rounds: merges PGA+LIV from 2004 through current calendar year (same as npm run update:rounds).
 * For a faster local start without hitting the API, set GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START=1 (uses CSV on disk).
 * Optional: GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=N — only re-pull last N calendar years (keeps older CSV rows).
 *
 * Live model (pricing): runs npm run fetch:in-play → live-in-play.json (DataGolf preds/in-play) unless
 * GOLF_SKIP_LIVE_IN_PLAY_ON_START=1.
 * While serving, re-runs fetch:in-play on a short interval; the script skips writing when info.last_update
 * is unchanged (matches DataGolf refresh cadence). Disable: GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER=1.
 * Override interval (ms): GOLF_LIVE_IN_PLAY_SERVER_POLL_MS (default 60000)
 *
 * Env:
 *   GOLF_SKIP_REFRESH_ON_START=1 — only serve (no API)
 *   GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START=1 — skip rounds CSV merge; still build:history + fetch:in-play + mirror
 *   GOLF_SKIP_LIVE_IN_PLAY_ON_START=1 — skip preds/in-play fetch on start
 *   GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER=1 — do not re-fetch live-in-play.json every 5 min while serving
 *   GOLF_LIVE_IN_PLAY_SERVER_POLL_MS — disk refresh interval in ms (default 60000)
 *   GOLF_HISTORICAL_ROUNDS_LIGHT=1 — destructive: trim CSV to last 2 seasons (avoid unless you mean it)
 *   GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=N — partial API refresh only
 *   ALPHA_CADDIE_START_FETCH_DG=1 — run full fetch:dg instead of rounds+history only
 *   PORT — serve port (default 5173)
 */
import { spawn, spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { findRscriptSync } from "./find-rscript.mjs";
import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

function loadApiKey() {
  const env = (process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = path.join(WEB_ROOT, "datagolf.local.json");
  if (fs.existsSync(p)) {
    try {
      const j = JSON.parse(fs.readFileSync(p, "utf8"));
      return String(j.apiKey || j.key || "").trim();
    } catch {
      return "";
    }
  }
  return "";
}

function refreshBeforeServe() {
  if (process.env.ALPHA_CADDIE_START_FETCH_DG === "1") {
    console.log("[alpha-caddie-web] ALPHA_CADDIE_START_FETCH_DG=1 → npm run fetch:dg (projections + rounds + history) …");
    const r = spawnSync(process.execPath, [path.join(WEB_ROOT, "scripts", "fetch-datagolf.mjs")], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
    });
    if (r.status !== 0) {
      console.warn("[alpha-caddie-web] fetch:dg exited", r.status, "— serving anyway.");
    }
    return;
  }

  const key = loadApiKey();
  const roundsNode = path.join(WEB_ROOT, "scripts", "update-historical-rounds-node.mjs");

  if (process.env.GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START === "1") {
    console.log(
      "[alpha-caddie-web] GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START=1 — skipping DataGolf rounds merge (using historical_rounds_all.csv on disk)."
    );
  } else if (key && fs.existsSync(roundsNode)) {
    const roundsEnv = { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key };
    const hasYears = String(process.env.GOLF_HISTORICAL_ROUNDS_YEARS || "").trim();
    const light = process.env.GOLF_HISTORICAL_ROUNDS_LIGHT === "1";
    const recentFetch = String(roundsEnv.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "").trim();
    const recentMerge = !light && !!recentFetch;
    let mergeNote = "(full 2004–current PGA + LIV merge into repo CSV) …";
    if (light) mergeNote = "(LIGHT=1: trims CSV to recent seasons) …";
    else if (hasYears) mergeNote = "(custom GOLF_HISTORICAL_ROUNDS_YEARS) …";
    else if (recentMerge) mergeNote = `(API: last ${recentFetch} season(s) only; older rows on disk preserved) …`;
    console.log("[alpha-caddie-web] Merging DataGolf rounds → data/historical_rounds_all.csv", mergeNote);
    const u = spawnSync(process.execPath, [roundsNode], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: roundsEnv,
    });
    if (u.status !== 0) {
      console.warn("[alpha-caddie-web] Rounds update failed (code", u.status, "); continuing with existing CSV.");
    }
  } else {
    console.log(
      "[alpha-caddie-web] Skipping DataGolf merge (set DATAGOLF_API_KEY or datagolf.local.json); rebuilding JSON from CSV on disk."
    );
  }

  const buildHist = path.join(WEB_ROOT, "scripts", "build-player-history.mjs");
  const embedHist = path.join(WEB_ROOT, "scripts", "embed-player-history.mjs");
  const buildShots = path.join(WEB_ROOT, "scripts", "build-player-shots-web.mjs");
  console.log("[alpha-caddie-web] Rebuilding player_round_history.json …");
  const h = spawnSync(process.execPath, [buildHist], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
  });
  if (h.status !== 0) {
    console.warn("[alpha-caddie-web] build:history exited", h.status);
  } else if (fs.existsSync(embedHist)) {
    const e = spawnSync(process.execPath, [embedHist], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: process.env,
    });
    if (e.status !== 0) console.warn("[alpha-caddie-web] embed-player-history exited", e.status);
  }

  if (h.status === 0 && fs.existsSync(buildShots)) {
    const s = spawnSync(process.execPath, [buildShots], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
    });
    if (s.status !== 0) console.warn("[alpha-caddie-web] build-player-shots-web exited", s.status);
  }

  const livePlay = path.join(WEB_ROOT, "scripts", "fetch-live-in-play.mjs");
  if (key && fs.existsSync(livePlay) && process.env.GOLF_SKIP_LIVE_IN_PLAY_ON_START !== "1") {
    console.log("[alpha-caddie-web] DataGolf preds/in-play → live-in-play.json …");
    const lp = spawnSync(process.execPath, [livePlay], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
    });
    if (lp.status !== 0) {
      console.warn("[alpha-caddie-web] fetch:in-play exited", lp.status, "— keep existing live-in-play.json if present.");
    }
  }

  mirrorModelDataToWeb(REPO_ROOT, WEB_ROOT);
}

if (process.env.GOLF_SKIP_REFRESH_ON_START === "1") {
  console.log("[alpha-caddie-web] GOLF_SKIP_REFRESH_ON_START=1 — serving without refresh.");
} else {
  refreshBeforeServe();
}

const port = String(process.env.PORT || "5173");
console.log("[alpha-caddie-web] Starting static server on port", port, "…");

const child = spawn("npx", ["--yes", "serve", ".", "-p", port], {
  cwd: WEB_ROOT,
  stdio: "inherit",
  shell: true,
  env: process.env,
});

function startLiveInPlayDiskPoller() {
  if (process.env.GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER === "1") return;
  const key = loadApiKey();
  const script = path.join(WEB_ROOT, "scripts", "fetch-live-in-play.mjs");
  if (!key || !fs.existsSync(script)) return;
  const ms = Math.min(600_000, Math.max(30_000, Number(process.env.GOLF_LIVE_IN_PLAY_SERVER_POLL_MS || 60_000)));
  setInterval(() => {
    const bg = spawn(process.execPath, [script], {
      cwd: WEB_ROOT,
      stdio: "ignore",
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
    });
    bg.on("error", () => {});
  }, ms);
  console.log(
    "[alpha-caddie-web] live-in-play.json disk refresh every",
    Math.round(ms / 1000),
    "s (DataGolf preds/in-play). Set GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER=1 to disable."
  );
}
startLiveInPlayDiskPoller();

child.on("exit", (code, signal) => {
  if (signal) process.kill(process.pid, signal);
  process.exit(code ?? 1);
});
