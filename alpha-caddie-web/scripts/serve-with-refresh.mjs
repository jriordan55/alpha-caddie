#!/usr/bin/env node
/**
 * npm start / npm run dev — default preflight is **light** (latest lines only):
 *   - Refreshes book odds → projections.json + live-in-play.json (when DATAGOLF_API_KEY is set)
 *   - Skips rounds CSV merge, build:history, shots web rebuild, and model CSV mirror unless you opt in
 *   - After refresh: ensure-static-data-files.mjs — local-only stubs; Render skips demo data (see module header).
 *
 * Full preflight (rounds merge + build:history + mirror + book odds + in-play): set GOLF_START_FULL_REFRESH=1 before start.
 *
 * Live model / player rows in projections.json from R: scripts/refresh_projections_between_rounds.ps1 (repo root).
 *
 * Historical rounds merge: npm run update:rounds or GOLF_START_FULL_REFRESH=1 on start.
 * Optional: GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=N — only re-pull last N calendar years (keeps older CSV rows).
 *
 * Live model (pricing): runs fetch:in-play → live-in-play.json unless GOLF_SKIP_LIVE_IN_PLAY_ON_START=1.
 * While serving, re-runs fetch:in-play on an interval (writes skip when in-play + live feed timestamps unchanged).
 * Disable: GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER=1. GOLF_LIVE_IN_PLAY_SERVER_POLL_MS (default 60000; min 30000; max 600000).
 *
 * Book odds: runs fetch-book-odds-into-projections.mjs unless GOLF_SKIP_BOOK_ODDS_ON_START=1.
 * While serving, polls that script on GOLF_BOOK_ODDS_SERVER_POLL_MS (default 60000, min 45000) unless the
 * unified pipeline is on (default) — then pre-round still runs fetch:book-odds every tick; full fetch:dg is throttled.
 * Disable: GOLF_SKIP_BOOK_ODDS_POLL_SERVER=1.
 *
 * Env:
 *   GOLF_SKIP_REFRESH_ON_START=1 — only serve (no API)
 *   GOLF_FAST_LOCAL_START=1 — same defaults as `npm run start:fast` (see block comment near imports)
 *   GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START=1 — skip rounds CSV merge (ignored for light default unless full refresh)
 *   GOLF_SKIP_LIVE_IN_PLAY_ON_START=1 — skip preds/in-play fetch on start
 *   GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER=1 — do not re-fetch live-in-play.json while serving
 *   GOLF_LIVE_IN_PLAY_SERVER_POLL_MS — live-in-play poll in ms (default 60000; raise if you hit API limits)
 *   GOLF_FETCH_DG_SERVER_POLL_MS — full `fetch-datagolf.mjs` interval (ms). Floored at 300000 (5m); larger values clamped to 24h max. Honored even when
 *     GOLF_UNIFIED_PROJECTIONS_PIPELINE=1 (live mode otherwise never refreshes field/course rows; unified mode used to ignore this env).
 *   GOLF_SKIP_BOOK_ODDS_ON_START=1 — skip book odds fetch on start
 *   GOLF_SKIP_BOOK_ODDS_POLL_SERVER=1 — do not refresh betting-tools odds into projections.json while serving
 *   GOLF_BOOK_ODDS_SERVER_POLL_MS — book odds + DK round props poll in ms (default 60000, min 45000)
 *   GOLF_UNIFIED_PROJECTIONS_PIPELINE=1 — single scheduler: pre-round fetch:dg before Thursday ET, then live fetch:in-play + fetch:book-odds from Thursday onward.
 *   GOLF_PRE_ROUND_PROJECTIONS_POLL_MS — unified pre-round fetch:dg interval (default 21600000; min 1800000)
 *   GOLF_LIVE_PROJECTIONS_POLL_MS — unified live tick interval for in-play + book odds (default 60000; min 30000)
 *   GOLF_HISTORICAL_ROUNDS_LIGHT=1 — destructive: trim CSV to last 2 seasons (avoid unless you mean it)
 *   GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=N — partial API refresh only
 *   ALPHA_CADDIE_START_FETCH_DG=1 — run full fetch:dg instead of rounds+history only
 *   Stale week vs DataGolf: fetch-book-odds compares `datagolf_field_week_key` + titles/courses vs `/field-updates`;
 *     opt out of inline rebuild: GOLF_SKIP_INLINE_FETCH_DG_ON_EVENT_MISMATCH=1.
 *   Render: GOLF_RENDER_SYNC_FETCH_DG_ON_BOOT=1 (default in render.yaml) runs perfect-parity boot chain.
 *   GOLF_MODEL_DIR: optional override for repo root (see resolve-golf-model-dir.mjs).
 *   PORT — serve port (default 5173)
 *
 * Fast local UI (no long preflight before the server listens):
 *   npm run start:fast   (= node scripts/serve-with-refresh.mjs --fast)
 *   or GOLF_FAST_LOCAL_START=1 — sets the skips below only where not already set.
 *   GOLF_SKIP_BUILD_HISTORY_ON_START=1 — skip player_round_history.json rebuild + embed
 *   GOLF_SKIP_SHOTS_WEB_ON_START=1 — skip build-player-shots-web.mjs
 *   GOLF_SKIP_MIRROR_MODEL_DATA_ON_START=1 — skip mirror-model-data-to-web (large CSV copies)
 *   GOLF_START_FULL_REFRESH=1 — on start, run rounds merge + build:history + mirror + book odds + in-play (old default weight)
 */
import { spawn, spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { findRscriptSync } from "./find-rscript.mjs";
import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";
import { ensureAlphaCaddieStaticArtifacts, renderHistoricalTrendsPayloadBroken } from "./ensure-static-data-files.mjs";
import { applyHistoricalRoundsMergeDefaults } from "./historical-rounds-merge-env.mjs";
import { resolveGolfModelDir } from "./resolve-golf-model-dir.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = resolveGolfModelDir(WEB_ROOT);

/** Full `fetch:dg` / projections rebuild — never more often than once per 5 minutes. */
const MIN_FETCH_DG_POLL_MS = 300_000;
const MAX_FETCH_DG_POLL_MS = 86_400_000;

/** @returns {number | null} clamped ms, or null if invalid */
function clampFetchDgPollMs(rawMs) {
  if (!Number.isFinite(rawMs) || rawMs <= 0) return null;
  const clamped = Math.min(MAX_FETCH_DG_POLL_MS, Math.max(MIN_FETCH_DG_POLL_MS, rawMs));
  if (rawMs < MIN_FETCH_DG_POLL_MS) {
    console.warn(
      `[alpha-caddie-web] GOLF_FETCH_DG_SERVER_POLL_MS (${rawMs}ms) below 5-minute minimum — using ${clamped}ms.`
    );
  }
  return clamped;
}

const fastLocal =
  process.argv.includes("--fast") ||
  String(process.env.GOLF_FAST_LOCAL_START || "").trim() === "1";
if (fastLocal) {
  const def = (k, v) => {
    if (!String(process.env[k] || "").trim()) process.env[k] = v;
  };
  def("GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START", "1");
  /** Keep book odds + in-play refresh (light API); only skip heavy CSV/history/mirror. */
  def("GOLF_SKIP_LIVE_IN_PLAY_ON_START", "1");
  def("GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER", "1");
  def("GOLF_SKIP_BUILD_HISTORY_ON_START", "1");
  def("GOLF_SKIP_SHOTS_WEB_ON_START", "1");
  def("GOLF_SKIP_MIRROR_MODEL_DATA_ON_START", "1");
  console.log(
    "[alpha-caddie-web] Fast start: skipping rounds merge, history+shots rebuild, model CSV mirror — still refreshes book odds + in-play when DATAGOLF_API_KEY is set. Heavy refresh: npm run fetch:dg / update:rounds."
  );
}

/**
 * Render: if Historical Trends JSON is missing or `byDgId` is empty after boot, repair runs update-historical-rounds → build → embed.
 * With GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1 (render.yaml), repair does one uncapped 2004→present PGA (+ LIV) merge (slow).
 * Without it, repair retries with GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS then optional wide / FULL_MERGE_IF_EMPTY passes.
 * Broken/populated checks avoid parsing huge JSON on tiny dynos (see renderHistoricalTrendsPayloadBroken).
 */

function repairRenderHistoricalTrendsIfNeeded() {
  if (String(process.env.RENDER || "").toLowerCase() !== "true") return;
  if (String(process.env.GOLF_SKIP_RENDER_HISTORY_REPAIR || "").trim() === "1") return;
  const key = loadApiKey();
  if (!key) return;
  const projPath = path.join(WEB_ROOT, "projections.json");
  if (!fs.existsSync(projPath)) return;
  if (!renderHistoricalTrendsPayloadBroken(WEB_ROOT)) return;

  const roundsNode = path.join(WEB_ROOT, "scripts", "update-historical-rounds-node.mjs");
  const buildHist = path.join(WEB_ROOT, "scripts", "build-player-history.mjs");
  const embedHist = path.join(WEB_ROOT, "scripts", "embed-player-history.mjs");
  const histEnv = { ...process.env, GOLF_MODEL_DIR: REPO_ROOT };

  const mergeBuildEmbed = (mergeEnv, logLine) => {
    console.warn("[alpha-caddie-web] Render repair:", logLine);
    if (fs.existsSync(roundsNode)) {
    const u = spawnSync(process.execPath, [roundsNode], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: applyHistoricalRoundsMergeDefaults(mergeEnv),
    });
    if (u.status !== 0) console.warn("[alpha-caddie-web] Render repair: update-historical-rounds-node exited", u.status);
    }
    if (fs.existsSync(buildHist)) {
      const h = spawnSync(process.execPath, [buildHist], {
        cwd: WEB_ROOT,
        stdio: "inherit",
        env: histEnv,
      });
      if (h.status !== 0) console.warn("[alpha-caddie-web] Render repair: build-player-history exited", h.status);
    }
    if (fs.existsSync(embedHist)) {
      const e = spawnSync(process.execPath, [embedHist], {
        cwd: WEB_ROOT,
        stdio: "inherit",
        env: histEnv,
      });
      if (e.status !== 0) console.warn("[alpha-caddie-web] Render repair: embed-player-history exited", e.status);
    }
  };

  const fullHistory = String(process.env.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY || "").trim() === "1";
  if (fullHistory) {
    const mergeEnv = {
      ...process.env,
      GOLF_MODEL_DIR: REPO_ROOT,
      DATAGOLF_API_KEY: key,
      GOLF_HISTORICAL_ROUNDS_FULL_HISTORY: "1",
    };
    delete mergeEnv.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS;
    delete mergeEnv.GOLF_HISTORICAL_ROUNDS_LIGHT;
    mergeBuildEmbed(
      mergeEnv,
      "full history (GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1 — 2004→present PGA years in refreshYears; LIV per tour rules; first boot can take many minutes).",
    );
    return;
  }

  const runRepairPass = (label, seasonsLabel, yearsStr) => {
    const mergeEnv = {
      ...process.env,
      GOLF_MODEL_DIR: REPO_ROOT,
      DATAGOLF_API_KEY: key,
      GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: yearsStr,
    };
    mergeBuildEmbed(mergeEnv, `${label}: GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS=${yearsStr} (${seasonsLabel})`);
  };

  const primaryYears =
    String(
      process.env.GOLF_RENDER_HISTORY_REPAIR_YEARS ||
        process.env.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS ||
        "22",
    ).trim() || "22";

  runRepairPass("primary", "aligned with dashboard RECENT_FETCH", primaryYears);

  if (!renderHistoricalTrendsPayloadBroken(WEB_ROOT)) return;
  if (String(process.env.GOLF_RENDER_WIDE_HISTORY_REPAIR || "1").trim() === "0") {
    console.warn("[alpha-caddie-web] Render repair: wide retry skipped (GOLF_RENDER_WIDE_HISTORY_REPAIR=0).");
    return;
  }

  const wideYears =
    String(process.env.GOLF_RENDER_HISTORY_WIDE_RETRY_YEARS || "55").trim() || "55";
  runRepairPass("wide retry", "extra merge if export still empty", wideYears);

  if (!renderHistoricalTrendsPayloadBroken(WEB_ROOT)) return;
  if (String(process.env.GOLF_RENDER_FULL_HISTORICAL_MERGE_IF_EMPTY || "").trim() !== "1") return;

  console.warn(
    "[alpha-caddie-web] Render repair: FULL historical merge (GOLF_RENDER_FULL_HISTORICAL_MERGE_IF_EMPTY=1 — no RECENT_FETCH cap; slow, may hit API limits).",
  );
  const fullMergeEnv = {
    ...process.env,
    GOLF_MODEL_DIR: REPO_ROOT,
    DATAGOLF_API_KEY: key,
  };
  delete fullMergeEnv.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS;
  delete fullMergeEnv.GOLF_HISTORICAL_ROUNDS_LIGHT;
  if (fs.existsSync(roundsNode)) {
    const u3 = spawnSync(process.execPath, [roundsNode], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: applyHistoricalRoundsMergeDefaults(fullMergeEnv),
    });
    if (u3.status !== 0) console.warn("[alpha-caddie-web] Render repair (full merge): update-historical-rounds-node exited", u3.status);
  }
  if (fs.existsSync(buildHist)) {
    const h3 = spawnSync(process.execPath, [buildHist], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: histEnv,
    });
    if (h3.status !== 0) console.warn("[alpha-caddie-web] Render repair (full merge): build-player-history exited", h3.status);
  }
  if (fs.existsSync(embedHist)) {
    const e3 = spawnSync(process.execPath, [embedHist], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: histEnv,
    });
    if (e3.status !== 0) console.warn("[alpha-caddie-web] Render repair (full merge): embed-player-history exited", e3.status);
  }
}

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

/** After fetch:dg / book odds: display_round meta, weather, armed pin sheet (no-op if not armed). */
function runRenderProjectionPostProcess(env = {}) {
  const bootEnv = { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, ...env };
  const steps = [
    ["merge-live-round-meta", "merge-live-round-meta-into-projections.mjs"],
    ["bake-weather", "bake-weather-into-projections.mjs"],
  ];
  if (String(process.env.GOLF_SKIP_PIN_SHEET || "").trim() !== "1") {
    steps.push(["apply-pin-sheet", "apply-pin-sheet-to-projections.mjs"]);
    steps.push(["sync-pin-locations", "sync-pin-locations.mjs"]);
  }
  for (const [label, scriptFile] of steps) {
    const scriptPath = path.join(WEB_ROOT, "scripts", scriptFile);
    if (!fs.existsSync(scriptPath)) continue;
    console.log(`[alpha-caddie-web] Render boot: ${label} …`);
    const r = spawnSync(process.execPath, [scriptPath], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: bootEnv,
    });
    if (r.status !== 0) console.warn(`[alpha-caddie-web] Render boot: ${label} exited`, r.status);
  }
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

  /** Default `npm start`: only refresh live odds + in-play; skip heavy CSV/history/mirror unless opted in. */
  const fullStart = String(process.env.GOLF_START_FULL_REFRESH || "").trim() === "1";
  if (!fullStart) {
    const def = (k, v) => {
      if (!String(process.env[k] || "").trim()) process.env[k] = v;
    };
    def("GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START", "1");
    def("GOLF_SKIP_BUILD_HISTORY_ON_START", "1");
    def("GOLF_SKIP_SHOTS_WEB_ON_START", "1");
    def("GOLF_SKIP_MIRROR_MODEL_DATA_ON_START", "1");
    console.log(
      "[alpha-caddie-web] Light start: refreshing book odds + in-play only. Full preflight: GOLF_START_FULL_REFRESH=1 (or npm run update:rounds / build:history separately)."
    );
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
    const fullHistBoot = String(roundsEnv.GOLF_HISTORICAL_ROUNDS_FULL_HISTORY || "").trim() === "1";
    const recentFetch = String(roundsEnv.GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS || "").trim();
    const recentMerge = !light && !fullHistBoot && !!recentFetch;
    let mergeNote = "(full 2004–current PGA + LIV merge into repo CSV) …";
    if (fullHistBoot) mergeNote = "(FULL_HISTORY=1: every PGA season 2004→current + LIV per tour rules) …";
    else if (light) mergeNote = "(LIGHT=1: trims CSV to recent seasons) …";
    else if (hasYears) mergeNote = "(custom GOLF_HISTORICAL_ROUNDS_YEARS) …";
    else if (recentMerge) mergeNote = `(API: last ${recentFetch} season(s) only; older rows on disk preserved) …`;
    console.log("[alpha-caddie-web] Merging DataGolf rounds → data/historical_rounds_all.csv", mergeNote);
    const u = spawnSync(process.execPath, [roundsNode], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: applyHistoricalRoundsMergeDefaults(roundsEnv),
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
  const skipHist = process.env.GOLF_SKIP_BUILD_HISTORY_ON_START === "1";
  let histOk = true;
  if (skipHist) {
    console.log(
      "[alpha-caddie-web] GOLF_SKIP_BUILD_HISTORY_ON_START=1 — skipping player_round_history.json rebuild and embed."
    );
  } else {
    console.log("[alpha-caddie-web] Rebuilding player_round_history.json …");
    const h = spawnSync(process.execPath, [buildHist], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
    });
    histOk = h.status === 0;
    if (!histOk) {
      console.warn("[alpha-caddie-web] build:history exited", h.status);
    } else if (fs.existsSync(embedHist)) {
      const e = spawnSync(process.execPath, [embedHist], {
        cwd: WEB_ROOT,
        stdio: "inherit",
        env: process.env,
      });
      if (e.status !== 0) console.warn("[alpha-caddie-web] embed-player-history exited", e.status);
    }
  }

  const skipShots = process.env.GOLF_SKIP_SHOTS_WEB_ON_START === "1";
  if (!skipShots && histOk && fs.existsSync(buildShots)) {
    const s = spawnSync(process.execPath, [buildShots], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT },
    });
    if (s.status !== 0) console.warn("[alpha-caddie-web] build-player-shots-web exited", s.status);
  } else if (skipShots) {
    console.log("[alpha-caddie-web] GOLF_SKIP_SHOTS_WEB_ON_START=1 — skipping build-player-shots-web.mjs.");
  }

  /**
   * `npm run perfect` runs fetch:dg → in-play → draftkings-ou-props → book-odds before serve.
   * Render used dg → book → in-play with no standalone DK probe — mismatched projections/DK merge vs local.
   */
  const fetchDgScriptPath = path.join(WEB_ROOT, "scripts", "fetch-datagolf.mjs");
  const skipRefreshStart = process.env.GOLF_SKIP_REFRESH_ON_START === "1";
  const isRenderHost = String(process.env.RENDER || "").toLowerCase() === "true";
  const forcePerfectBoot = String(process.env.GOLF_FORCE_PERFECT_BOOT_CHAIN || "").trim() === "1";
  const renderSyncDgOn = String(process.env.GOLF_RENDER_SYNC_FETCH_DG_ON_BOOT || "1").trim() !== "0";
  const renderPerfectBoot =
    (isRenderHost || forcePerfectBoot) &&
    key &&
    !skipRefreshStart &&
    renderSyncDgOn &&
    fs.existsSync(fetchDgScriptPath);

  const dgBootEnv = { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key };

  if ((isRenderHost || forcePerfectBoot) && !skipRefreshStart) {
    console.log("[alpha-caddie-web] Paths:", { WEB_ROOT, REPO_ROOT });
  }

  if (renderPerfectBoot) {
    console.log(
      "[alpha-caddie-web] Render boot = npm run perfect parity: fetch:dg → in-play → DK O/U probe → book odds …",
    );
    const runRenderBootStep = (label, scriptFile) => {
      const scriptPath = path.join(WEB_ROOT, "scripts", scriptFile);
      if (!fs.existsSync(scriptPath)) return;
      console.log(`[alpha-caddie-web] Render boot: ${label} …`);
      const r = spawnSync(process.execPath, [scriptPath], {
        cwd: WEB_ROOT,
        stdio: "inherit",
        env: dgBootEnv,
      });
      if (r.status !== 0) console.warn(`[alpha-caddie-web] ${label} exited`, r.status);
    };

    runRenderBootStep("fetch:dg", "fetch-datagolf.mjs");
    if (process.env.GOLF_SKIP_LIVE_IN_PLAY_ON_START !== "1") {
      runRenderBootStep("fetch:in-play", "fetch-live-in-play.mjs");
    }
    const skipDkProbe =
      String(process.env.PERFECT_SKIP_FETCH_DK_OU || "").trim() === "1" ||
      String(process.env.GOLF_RENDER_SKIP_DK_OU_PROBE || "").trim() === "1";
    if (!skipDkProbe) {
      runRenderBootStep("DraftKings round O/U probe", "draftkings-ou-props.mjs");
    }
    if (process.env.GOLF_SKIP_BOOK_ODDS_ON_START !== "1") {
      runRenderBootStep("fetch:book-odds", "fetch-book-odds-into-projections.mjs");
    }
    runRenderProjectionPostProcess(dgBootEnv);
  }

  const bookOdds = path.join(WEB_ROOT, "scripts", "fetch-book-odds-into-projections.mjs");
  if (!renderPerfectBoot && key && fs.existsSync(bookOdds) && process.env.GOLF_SKIP_BOOK_ODDS_ON_START !== "1") {
    console.log("[alpha-caddie-web] Book odds → projections.json …");
    const bo = spawnSync(process.execPath, [bookOdds], {
      cwd: WEB_ROOT,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
    });
    if (bo.status !== 0) {
      console.warn("[alpha-caddie-web] fetch:book-odds exited", bo.status, "— keeping existing projections.json.");
    }
  }

  const livePlay = path.join(WEB_ROOT, "scripts", "fetch-live-in-play.mjs");
  if (!renderPerfectBoot && key && fs.existsSync(livePlay) && process.env.GOLF_SKIP_LIVE_IN_PLAY_ON_START !== "1") {
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

  if (process.env.GOLF_SKIP_MIRROR_MODEL_DATA_ON_START === "1") {
    console.log(
      "[alpha-caddie-web] GOLF_SKIP_MIRROR_MODEL_DATA_ON_START=1 — skipping mirror-model-data-to-web (repo CSV → alpha-caddie-web/data)."
    );
  } else {
    mirrorModelDataToWeb(REPO_ROOT, WEB_ROOT);
  }
}

const isRenderHost = String(process.env.RENDER || "").toLowerCase() === "true";
const deferHeavyBoot =
  isRenderHost && String(process.env.GOLF_RENDER_DEFER_HEAVY_BOOT || "1").trim() !== "0";

function runHeavyBootArtifacts() {
  repairRenderHistoricalTrendsIfNeeded();
  ensureAlphaCaddieStaticArtifacts(WEB_ROOT, REPO_ROOT);
  logRenderBootstrapDiag();
}

if (process.env.GOLF_SKIP_REFRESH_ON_START === "1") {
  console.log("[alpha-caddie-web] GOLF_SKIP_REFRESH_ON_START=1 — serving without refresh.");
} else {
  refreshBeforeServe();
}

if (!deferHeavyBoot) {
  runHeavyBootArtifacts();
}

function logRenderBootstrapDiag() {
  if (String(process.env.RENDER || "").toLowerCase() !== "true") return;
  if (String(process.env.GOLF_SKIP_RENDER_BOOT_DIAG || "").trim() === "1") return;
  const proj = path.join(WEB_ROOT, "projections.json");
  const hist = path.join(WEB_ROOT, "player_round_history.json");
  const rb = path.join(WEB_ROOT, "data", "results_backtest.json");
  let historyPlayers = -1;
  if (fs.existsSync(hist)) {
    try {
      const st = fs.statSync(hist);
      if (st.size > 350_000) {
        historyPlayers = renderHistoricalTrendsPayloadBroken(WEB_ROOT) ? 0 : -3;
      } else {
        const j = JSON.parse(fs.readFileSync(hist, "utf8"));
        historyPlayers = Object.keys(j?.byDgId || {}).length;
      }
    } catch {
      historyPlayers = -2;
    }
  }
  console.log("[alpha-caddie-web] Render bootstrap diagnostics:", {
    WEB_ROOT,
    REPO_ROOT,
    repo_data_dir_exists: fs.existsSync(path.join(REPO_ROOT, "data")),
    web_data_dir_exists: fs.existsSync(path.join(WEB_ROOT, "data")),
    projections_bytes: fs.existsSync(proj) ? fs.statSync(proj).size : 0,
    history_player_buckets: historyPlayers,
    results_backtest_exists: fs.existsSync(rb),
    unified_pipeline: String(process.env.GOLF_UNIFIED_PROJECTIONS_PIPELINE ?? "(unset→1)"),
  });
}

const port = String(process.env.PORT || "5173");
process.env.PORT = port;
const staticServer = path.join(WEB_ROOT, "scripts", "serve-static-no-json-cache.mjs");
console.log("[alpha-caddie-web] Starting static server on port", port, "(no-store JSON) …");

const child = spawn(process.execPath, [staticServer], {
  cwd: WEB_ROOT,
  stdio: "inherit",
  env: process.env,
});

if (deferHeavyBoot) {
  console.log(
    "[alpha-caddie-web] Render: HTTP server listening — history repair/build running in background (GOLF_RENDER_DEFER_HEAVY_BOOT=1).",
  );
  setImmediate(() => {
    try {
      runHeavyBootArtifacts();
    } catch (e) {
      console.warn("[alpha-caddie-web] Deferred heavy boot failed:", e?.message || e);
    }
  });
}

function spawnScript(scriptPath, taskLabel, opts = {}) {
  if (!fs.existsSync(scriptPath)) return null;
  const key = loadApiKey();
  if (!key) {
    if (opts.logNoKey !== false) console.warn(`[alpha-caddie-web] No DATAGOLF_API_KEY — skipping ${taskLabel}.`);
    return null;
  }
  const bg = spawn(process.execPath, [scriptPath], {
    cwd: WEB_ROOT,
    stdio: ["ignore", "inherit", "inherit"],
    env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
  });
  bg.on("error", (err) => console.warn(`[alpha-caddie-web] ${taskLabel} spawn error:`, err.message));
  bg.on("exit", (code) => {
    if (code !== 0 && code != null) console.warn(`[alpha-caddie-web] ${taskLabel} exited`, code);
  });
  return bg;
}

function readJsonFileSafe(p) {
  if (!fs.existsSync(p)) return null;
  try {
    return JSON.parse(fs.readFileSync(p, "utf8"));
  } catch {
    return null;
  }
}

function inferLiveModeFromFiles() {
  const proj = readJsonFileSafe(path.join(WEB_ROOT, "projections.json")) || {};
  const live = readJsonFileSafe(path.join(WEB_ROOT, "live-in-play.json")) || {};
  const projPlayers = Array.isArray(proj.players) ? proj.players.length : 0;
  /** Empty payload → stay pre-round so unified pipeline keeps scheduling fetch:dg. */
  if (projPlayers === 0) return false;

  const projEvent = String(proj.event_name || "").trim();
  const info = live && typeof live.info === "object" ? live.info : {};
  const liveEvent = String(info.event_name || live.event_name || live?.live_tournament_stats?.event_name || "").trim();
  const liveRound = Number(info.current_round);
  const rows = Array.isArray(live.data) ? live.data.length : 0;
  const eventAligned = eventsLikelySame(projEvent, liveEvent);
  return eventAligned && Number.isFinite(liveRound) && liveRound >= 1 && liveRound <= 4 && rows > 0;
}

/** One projections pipeline: follow DataGolf state (pre-round until in-play aligns, then live). */
function startUnifiedProjectionPipeline() {
  const enabled = String(process.env.GOLF_UNIFIED_PROJECTIONS_PIPELINE || "1").trim() !== "0";
  if (!enabled) return false;

  const fetchDgScript = path.join(WEB_ROOT, "scripts", "fetch-datagolf.mjs");
  const inPlayScript = path.join(WEB_ROOT, "scripts", "fetch-live-in-play.mjs");
  const bookScript = path.join(WEB_ROOT, "scripts", "fetch-book-odds-into-projections.mjs");
  if (!fs.existsSync(fetchDgScript) || !fs.existsSync(inPlayScript) || !fs.existsSync(bookScript)) return false;

  const preMs = Math.max(1_800_000, Number(process.env.GOLF_PRE_ROUND_PROJECTIONS_POLL_MS || 21_600_000));
  const liveMs = Math.max(30_000, Number(process.env.GOLF_LIVE_PROJECTIONS_POLL_MS || 60_000));
  const probeMs = Math.max(30_000, Math.min(liveMs, Number(process.env.GOLF_LIVE_PROBE_POLL_MS || 60_000)));

  const envDgRaw = String(process.env.GOLF_FETCH_DG_SERVER_POLL_MS || "").trim();
  let dgPollMs = null;
  if (envDgRaw && envDgRaw !== "0") {
    const clamped = clampFetchDgPollMs(Number(envDgRaw));
    if (clamped != null) dgPollMs = clamped;
    else console.warn("[alpha-caddie-web] GOLF_FETCH_DG_SERVER_POLL_MS invalid — ignoring.");
  }
  /** Pre-round full rebuild spacing: explicit poll interval if set, else conservative default (6h). */
  const dgThrottlePreMs = dgPollMs ?? preMs;

  let mode = "";
  let busy = false;
  let lastNoKeyLog = 0;
  /** Last time we started fetch:dg (pre + optional live); 0 = never, allows first run immediately. */
  let lastFullDgRunAt = 0;
  const logNoKeyThrottled = () => {
    const now = Date.now();
    if (now - lastNoKeyLog > 600_000) {
      lastNoKeyLog = now;
      console.warn("[alpha-caddie-web] No DATAGOLF_API_KEY — unified projections pipeline tick skipped.");
    }
  };

  const runTick = () => {
    if (busy) return;
    const key = loadApiKey();
    if (!key) {
      logNoKeyThrottled();
      return;
    }
    busy = true;
    // Always probe DataGolf in-play first so mode follows their feed, not calendar assumptions.
    const probe = spawnScript(inPlayScript, "fetch:in-play", { logNoKey: false });
    if (!probe) {
      busy = false;
      return;
    }
    probe.on("exit", () => {
      const nextMode = inferLiveModeFromFiles() ? "live" : "pre";
      const now = Date.now();
      if (nextMode !== mode) {
        mode = nextMode;
        if (nextMode === "live") {
          console.log(
            "[alpha-caddie-web] Unified pipeline mode=live (DataGolf in-play aligned): fetch:in-play + fetch:book-odds" +
              (dgPollMs
                ? `; fetch:dg at most every ${Math.round(dgPollMs / 1000)}s (GOLF_FETCH_DG_SERVER_POLL_MS).`
                : ".")
          );
        } else {
          console.log(
            "[alpha-caddie-web] Unified pipeline mode=pre-round: fetch:book-odds every tick; fetch:dg at most every",
            Math.round(dgThrottlePreMs / 1000),
            "s.",
            dgPollMs ? "(GOLF_FETCH_DG_SERVER_POLL_MS)" : "(GOLF_PRE_ROUND_PROJECTIONS_POLL_MS default)"
          );
        }
      }
      if (nextMode === "live") {
        const bookJob = spawnScript(bookScript, "fetch:book-odds", { logNoKey: false });
        if (!bookJob) {
          busy = false;
          return;
        }
        bookJob.on("exit", () => {
          if (!dgPollMs) {
            busy = false;
            return;
          }
          const nowDg = Date.now();
          if (lastFullDgRunAt !== 0 && nowDg - lastFullDgRunAt < dgPollMs) {
            busy = false;
            return;
          }
          lastFullDgRunAt = nowDg;
          const dgJob = spawnScript(fetchDgScript, "fetch:dg (live)", { logNoKey: false });
          if (!dgJob) {
            busy = false;
            return;
          }
          dgJob.on("exit", () => {
            busy = false;
          });
        });
        return;
      }
      // Pre-round: always merge fresh sportsbook lines (was incorrectly gated on fetch:dg only — odds stayed stale for hours).
      const bookJob = spawnScript(bookScript, "fetch:book-odds", { logNoKey: false });
      if (!bookJob) {
        busy = false;
        return;
      }
      bookJob.on("exit", () => {
        const now2 = Date.now();
        if (lastFullDgRunAt !== 0 && now2 - lastFullDgRunAt < dgThrottlePreMs) {
          busy = false;
          return;
        }
        lastFullDgRunAt = now2;
        const preJob = spawnScript(fetchDgScript, "fetch:dg", { logNoKey: false });
        if (!preJob) {
          busy = false;
          return;
        }
        preJob.on("exit", () => {
          busy = false;
        });
      });
    });
  };

  const scheduler = () => {
    runTick();
    const wait = mode === "live" ? liveMs : probeMs;
    setTimeout(scheduler, wait);
  };
  scheduler();
  console.log(
    "[alpha-caddie-web] Unified projections pipeline enabled. fetch:dg throttle (pre-round):",
    Math.round(dgThrottlePreMs / 1000),
    "s;",
    dgPollMs ? `fetch:dg in live every ${Math.round(dgPollMs / 1000)}s;` : "no fetch:dg in live (set GOLF_FETCH_DG_SERVER_POLL_MS);",
    "live poll:",
    Math.round(liveMs / 1000),
    "s; probe:",
    Math.round(probeMs / 1000),
    "s. Disable with GOLF_UNIFIED_PROJECTIONS_PIPELINE=0."
  );
  return true;
}

function startLiveInPlayDiskPoller() {
  if (process.env.GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER === "1") {
    console.log("[alpha-caddie-web] GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER=1 — live-in-play disk poller off.");
    return;
  }
  const script = path.join(WEB_ROOT, "scripts", "fetch-live-in-play.mjs");
  if (!fs.existsSync(script)) return;
  const ms = Math.min(600_000, Math.max(30_000, Number(process.env.GOLF_LIVE_IN_PLAY_SERVER_POLL_MS || 60_000)));
  let lastLiveNoKeyLog = 0;
  const tick = () => {
    const key = loadApiKey();
    if (!key) {
      const t = Date.now();
      if (t - lastLiveNoKeyLog > 600_000) {
        lastLiveNoKeyLog = t;
        console.warn(
          "[alpha-caddie-web] No DATAGOLF_API_KEY — skipping live-in-play poll tick (set key in Render → Environment)."
        );
      }
      return;
    }
    const bg = spawn(process.execPath, [script], {
      cwd: WEB_ROOT,
      stdio: ["ignore", "inherit", "inherit"],
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
    });
    bg.on("error", (err) => console.warn("[alpha-caddie-web] fetch:in-play spawn error:", err.message));
    bg.on("exit", (code) => {
      if (code !== 0 && code != null) {
        console.warn("[alpha-caddie-web] fetch:in-play exited", code, "— see stderr above; live-in-play.json unchanged this tick.");
      }
    });
  };
  if (!loadApiKey()) {
    console.warn(
      "[alpha-caddie-web] No DATAGOLF_API_KEY (and no datagolf.local.json) — live-in-play.json will NOT update on disk until a key is set. Poller stays scheduled and will retry each interval."
    );
  }
  setInterval(tick, ms);
  console.log(
    "[alpha-caddie-web] live-in-play.json disk refresh every",
    Math.round(ms / 1000),
    "s (DataGolf preds/in-play + scores). Key re-read each tick. Set GOLF_SKIP_LIVE_IN_PLAY_POLL_SERVER=1 to disable."
  );
}
const usingUnifiedPipeline = startUnifiedProjectionPipeline();
if (!usingUnifiedPipeline) startLiveInPlayDiskPoller();

function startBookOddsDiskPoller() {
  if (process.env.GOLF_SKIP_BOOK_ODDS_POLL_SERVER === "1") {
    console.log("[alpha-caddie-web] GOLF_SKIP_BOOK_ODDS_POLL_SERVER=1 — book odds disk poller off.");
    return;
  }
  const script = path.join(WEB_ROOT, "scripts", "fetch-book-odds-into-projections.mjs");
  if (!fs.existsSync(script)) return;
  const ms = Math.min(600_000, Math.max(45_000, Number(process.env.GOLF_BOOK_ODDS_SERVER_POLL_MS || 60_000)));
  let lastBookNoKeyLog = 0;
  const tick = () => {
    const key = loadApiKey();
    if (!key) {
      const t = Date.now();
      if (t - lastBookNoKeyLog > 600_000) {
        lastBookNoKeyLog = t;
        console.warn("[alpha-caddie-web] No DATAGOLF_API_KEY — skipping book-odds poll tick.");
      }
      return;
    }
    const bg = spawn(process.execPath, [script], {
      cwd: WEB_ROOT,
      stdio: ["ignore", "inherit", "inherit"],
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
    });
    bg.on("error", (err) => console.warn("[alpha-caddie-web] fetch-book-odds spawn error:", err.message));
    bg.on("exit", (code) => {
      if (code !== 0 && code != null) {
        console.warn(
          "[alpha-caddie-web] fetch-book-odds exited",
          code,
          "— DK props need Playwright (build: npm install && npx playwright install chromium). Check logs above."
        );
      }
    });
  };
  if (!loadApiKey()) {
    console.warn(
      "[alpha-caddie-web] No DATAGOLF_API_KEY — book-odds poller idle until key is set; retries each interval."
    );
  }
  setInterval(tick, ms);
  console.log(
    "[alpha-caddie-web] projections + DraftKings props refresh every",
    Math.round(ms / 1000),
    "s (API key re-read each tick). Set GOLF_SKIP_BOOK_ODDS_POLL_SERVER=1 to disable."
  );
}
if (!usingUnifiedPipeline) startBookOddsDiskPoller();

/** Optional full projections rebuild from DataGolf (field + model rows). Interval floored at 5 minutes; skips overlapping runs. */
function startFetchDgDiskPoller() {
  const raw = String(process.env.GOLF_FETCH_DG_SERVER_POLL_MS || "").trim();
  if (!raw || raw === "0") return;
  const ms = clampFetchDgPollMs(Number(raw));
  if (ms == null) {
    console.warn("[alpha-caddie-web] GOLF_FETCH_DG_SERVER_POLL_MS invalid — ignoring full fetch:dg poller.");
    return;
  }
  const script = path.join(WEB_ROOT, "scripts", "fetch-datagolf.mjs");
  if (!fs.existsSync(script)) return;
  let busy = false;
  const tick = () => {
    const key = loadApiKey();
    if (!key) {
      console.warn("[alpha-caddie-web] fetch:dg poller tick skipped — no DATAGOLF_API_KEY.");
      return;
    }
    if (busy) {
      console.warn("[alpha-caddie-web] fetch:dg still running — skipping overlapping tick.");
      return;
    }
    busy = true;
    console.log("[alpha-caddie-web] GOLF_FETCH_DG_SERVER_POLL_MS: running fetch:dg …");
    const bg = spawn(process.execPath, [script], {
      cwd: WEB_ROOT,
      stdio: ["ignore", "inherit", "inherit"],
      env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, DATAGOLF_API_KEY: key },
    });
    bg.on("error", (err) => {
      busy = false;
      console.warn("[alpha-caddie-web] fetch:dg spawn error:", err.message);
    });
    bg.on("exit", (code) => {
      busy = false;
      if (code !== 0 && code != null) console.warn("[alpha-caddie-web] fetch:dg exited", code);
      else if (String(process.env.GOLF_SKIP_PIN_SHEET || "").trim() !== "1") {
        runRenderProjectionPostProcess({ DATAGOLF_API_KEY: key });
      }
    });
  };
  setInterval(tick, ms);
  console.log(
    "[alpha-caddie-web] Full DataGolf fetch:dg every",
    Math.round(ms / 1000),
    "s (GOLF_FETCH_DG_SERVER_POLL_MS). Heavy — raise interval if needed."
  );
}
if (!usingUnifiedPipeline) startFetchDgDiskPoller();

child.on("exit", (code, signal) => {
  if (signal) process.kill(process.pid, signal);
  process.exit(code ?? 1);
});
