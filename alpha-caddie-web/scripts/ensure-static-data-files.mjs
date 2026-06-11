/**
 * Before the static server listens: optional verification / rebuild helpers.
 * On Render (RENDER=true): never writes demo projections — real assets come from fetch:dg / committed CSV + history JSON.
 * Results/Kelly JSON is not generated here (Results tab removed).
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { buildOfflineDemoProjectionsPayload } from "./offline-demo-projections-payload.mjs";

export function onRenderHost() {
  return String(process.env.RENDER || "").toLowerCase() === "true";
}

/** Demo/offline stubs only when allowed (never on Render). */
export function allowMinimalStaticStubs() {
  if (onRenderHost()) return false;
  if (String(process.env.GOLF_NO_MINIMAL_STATIC_STUBS || "").trim() === "1") return false;
  return String(process.env.GOLF_ALLOW_MINIMAL_STATIC_STUBS || "").trim() !== "0";
}

export function ensureAlphaCaddieStaticArtifacts(webRoot, repoRoot) {
  const stubsOk = allowMinimalStaticStubs();
  ensureProjectionsJsonExists(webRoot, stubsOk);
  ensurePlayerRoundHistoryJson(webRoot, repoRoot, stubsOk);
  ensureResultsBacktestAndKelly(webRoot, repoRoot, stubsOk);
  ensureEmbeddedRoundHistoryJs(webRoot, stubsOk);
}

export function ensureProjectionsJsonExists(webRoot, stubsOk) {
  const projPath = path.join(webRoot, "projections.json");
  let needWrite = false;
  if (!fs.existsSync(projPath)) needWrite = true;
  else {
    try {
      if (fs.statSync(projPath).size < 32) needWrite = true;
    } catch {
      needWrite = true;
    }
  }
  if (!needWrite) {
    try {
      const j = JSON.parse(fs.readFileSync(projPath, "utf8"));
      if (!j || typeof j !== "object" || !Array.isArray(j.players)) needWrite = true;
    } catch {
      needWrite = true;
    }
  }
  if (!needWrite) return;

  if (!stubsOk) {
    console.error(
      "[alpha-caddie-web] projections.json missing or invalid — set DATAGOLF_API_KEY and ensure fetch:dg runs on boot (no demo stub written on Render).",
    );
    return;
  }
  fs.writeFileSync(projPath, JSON.stringify(buildOfflineDemoProjectionsPayload(), null, 2), "utf8");
  console.warn(
    "[alpha-caddie-web] Wrote projections.json offline stub (local only). Use DATAGOLF_API_KEY + fetch:dg for live data.",
  );
}

function resolveHistoricalRoundsCsv(webRoot, repoRoot) {
  const a = path.join(repoRoot, "data", "historical_rounds_all.csv");
  if (fs.existsSync(a)) return a;
  const b = path.join(webRoot, "data", "historical_rounds_all.csv");
  if (fs.existsSync(b)) return b;
  return null;
}

const HISTORY_JSON_PREFIX_BYTES = 262144;

/**
 * True when Historical Trends JSON is missing, unreadable, or has empty `byDgId`.
 * Large committed exports are multi‑MB; full `JSON.parse` here has OOM'd small Render dynos (exit 134).
 */
export function renderHistoricalTrendsPayloadBroken(webRoot) {
  const histJson = path.join(webRoot, "player_round_history.json");
  if (!fs.existsSync(histJson)) return true;
  let size = 0;
  try {
    size = fs.statSync(histJson).size;
  } catch {
    return true;
  }
  if (size === 0) return true;

  const prefixLen = Math.min(size, HISTORY_JSON_PREFIX_BYTES);
  let prefix = "";
  try {
    const fd = fs.openSync(histJson, "r");
    try {
      const buf = Buffer.alloc(prefixLen);
      fs.readSync(fd, buf, 0, prefixLen, 0);
      prefix = buf.toString("utf8");
    } finally {
      fs.closeSync(fd);
    }
  } catch {
    return true;
  }

  if (/"byDgId"\s*:\s*\{\s*\}/.test(prefix)) return true;
  if (prefix.includes('"render-history-shell"') || prefix.includes('"offline-stub"')) return true;

  if (size > 350_000) {
    const m = prefix.match(/"byDgId"\s*:\s*\{/);
    if (!m) return true;
    const afterBrace = prefix.slice(m.index + m[0].length).replace(/^\s*/, "");
    if (afterBrace.startsWith("}")) return true;
    return false;
  }

  try {
    const j = JSON.parse(fs.readFileSync(histJson, "utf8"));
    return !j?.byDgId || typeof j.byDgId !== "object" || Object.keys(j.byDgId).length === 0;
  } catch {
    return true;
  }
}

export function ensurePlayerRoundHistoryJson(webRoot, repoRoot, stubsOk) {
  const outPath = path.join(webRoot, "player_round_history.json");

  if (!renderHistoricalTrendsPayloadBroken(webRoot)) return;

  const projPath = path.join(webRoot, "projections.json");
  const csvPath = resolveHistoricalRoundsCsv(webRoot, repoRoot);

  const skipSyncBuild = String(process.env.GOLF_SKIP_BUILD_HISTORY_ON_START || "").trim() === "1";

  if (fs.existsSync(projPath) && csvPath && !skipSyncBuild) {
    const buildHist = path.join(webRoot, "scripts", "build-player-history.mjs");
    if (fs.existsSync(buildHist)) {
      console.log("[alpha-caddie-web] Building player_round_history.json (was missing or empty) …");
      const r = spawnSync(process.execPath, [buildHist], {
        cwd: webRoot,
        stdio: "inherit",
        env: { ...process.env, GOLF_MODEL_DIR: repoRoot },
      });
      if (r.status === 0 && !renderHistoricalTrendsPayloadBroken(webRoot)) return;
      if (r.status !== 0) console.warn("[alpha-caddie-web] build-player-history exited", r.status);
    }
  } else if (skipSyncBuild && onRenderHost()) {
    console.log(
      "[alpha-caddie-web] GOLF_SKIP_BUILD_HISTORY_ON_START=1 — deferring history build (shell JSON until background repair).",
    );
  }

  if (!stubsOk) {
    /** Without this file the browser gets HTTP 404 → HISTORY._ok false → “No history file.” Shell JSON = 200 + empty byDgId until merge/build succeeds. */
    const iso = new Date().toISOString();
    const note = csvPath
      ? "Render: rounds CSV on disk but no rows exported for current field dg_ids yet — increase GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS or check DataGolf merge logs."
      : "Render: historical_rounds_all.csv missing or empty — fetch:dg / rounds merge must succeed before Historical Trends fill in.";
    fs.writeFileSync(
      outPath,
      JSON.stringify({
        meta: { updated_at: iso, source: "render-history-shell", note },
        byDgId: {},
        holesByPlayerKey: {},
      }),
      "utf8",
    );
    console.warn(
      "[alpha-caddie-web] Wrote shell player_round_history.json (empty byDgId) so Historical Trends can load — fix CSV merge/build to populate.",
    );
    return;
  }

  const csvHint = csvPath
    ? "Rounds CSV present but export is empty for current field dg_ids, or build failed."
    : "No historical_rounds_all.csv — fetch:dg / update:rounds writes it.";
  fs.writeFileSync(
    outPath,
    JSON.stringify({
      meta: { updated_at: new Date().toISOString(), source: "offline-stub", note: csvHint },
      byDgId: {},
      holesByPlayerKey: {},
    }),
    "utf8",
  );
  console.warn("[alpha-caddie-web] Wrote minimal player_round_history.json (local stub only).");
}

/** Results/Kelly tab removed — no results JSON on boot (projections + Historical Trends unchanged). */
export function ensureResultsBacktestAndKelly(webRoot, repoRoot, stubsOk) {
  void webRoot;
  void repoRoot;
  void stubsOk;
}

export function ensureEmbeddedRoundHistoryJs(webRoot, stubsOk) {
  const p = path.join(webRoot, "embedded-player-round-history.js");
  if (fs.existsSync(p)) {
    try {
      if (fs.statSync(p).size >= 800) return;
    } catch {
      /* rewrite below when stubs allowed */
    }
  }
  if (!stubsOk) {
    console.warn(
      "[alpha-caddie-web] embedded-player-round-history.js missing — OK when fetch:dg writes embed; script tag may 404 until then.",
    );
    return;
  }
  const payload = {
    meta: { source: "offline-stub", updated_at: new Date().toISOString() },
    byDgId: {},
    holesByPlayerKey: {},
  };
  const body = `/** Offline stub — replaced by embed-player-history.mjs after build:history */\nwindow.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__=${JSON.stringify(payload)};\n`;
  fs.writeFileSync(p, body, "utf8");
  console.warn("[alpha-caddie-web] Wrote minimal embedded-player-round-history.js (local stub only).");
}
