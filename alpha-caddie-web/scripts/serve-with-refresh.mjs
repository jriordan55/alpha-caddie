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
 * By default we only re-fetch the last 2 calendar years of rounds from the API and merge into your existing CSV,
 * so 2004–present history on disk stays intact and you avoid 429s from re-pulling every season on every start.
 * Full historical re-merge: npm run update:rounds (or set GOLF_HISTORICAL_ROUNDS_FULL_ON_START=1).
 *
 * Env:
 *   GOLF_SKIP_REFRESH_ON_START=1  — only serve (no API / R)
 *   GOLF_HISTORICAL_ROUNDS_FULL_ON_START=1 — on start, fetch 2004–current like npm run update:rounds (slow)
 *   GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=N — override recent window (default on start: 2 when not full)
 *   ALPHA_CADDIE_START_FETCH_DG=1 — run full fetch:dg instead of rounds+history only
 *   PORT                          — serve port (default 5173)
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

  if (key && fs.existsSync(roundsNode)) {
    const roundsEnv = { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key };
    const fullStart = process.env.GOLF_HISTORICAL_ROUNDS_FULL_ON_START === "1";
    const hasYears = String(process.env.GOLF_HISTORICAL_ROUNDS_YEARS || "").trim();
    const light = process.env.GOLF_HISTORICAL_ROUNDS_LIGHT === "1";
    let explicitRecent = String(process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "").trim();
    if (!fullStart && !hasYears && !light && !explicitRecent) {
      roundsEnv.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS = "2";
      explicitRecent = "2";
    }
    const recentFetch = String(roundsEnv.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "").trim();
    const recentMerge = !light && !!recentFetch;
    let mergeNote = "…";
    if (light) mergeNote = "(LIGHT=1: trims CSV to recent seasons) …";
    else if (hasYears) mergeNote = "(custom GOLF_HISTORICAL_ROUNDS_YEARS) …";
    else if (recentMerge) mergeNote = `(API: last ${recentFetch} season(s); older 2004+ rows on disk kept) …`;
    else if (fullStart) mergeNote = "(full 2004–present from API) …";
    console.log("[alpha-caddie-web] Merging DataGolf rounds (PGA + LIV) → data/historical_rounds_all.csv", mergeNote);
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

child.on("exit", (code, signal) => {
  if (signal) process.kill(process.pid, signal);
  process.exit(code ?? 1);
});
