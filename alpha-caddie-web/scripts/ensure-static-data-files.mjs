/**
 * Before the static server listens: optional verification / rebuild helpers.
 * On Render (RENDER=true): never writes demo projections or empty JSON stubs — real assets come from
 * fetch:dg / committed CSVs / npm run build:results during deploy (see render.yaml).
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { buildOfflineDemoProjectionsPayload } from "./offline-demo-projections-payload.mjs";

function onRenderHost() {
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

function historyJsonLooksPopulated(raw) {
  return raw && typeof raw.byDgId === "object" && Object.keys(raw.byDgId).length > 0;
}

export function ensurePlayerRoundHistoryJson(webRoot, repoRoot, stubsOk) {
  const outPath = path.join(webRoot, "player_round_history.json");
  const readHistory = () => {
    if (!fs.existsSync(outPath)) return null;
    try {
      return JSON.parse(fs.readFileSync(outPath, "utf8"));
    } catch {
      return null;
    }
  };

  const existing = readHistory();
  if (historyJsonLooksPopulated(existing)) return;

  const projPath = path.join(webRoot, "projections.json");
  const csvPath = resolveHistoricalRoundsCsv(webRoot, repoRoot);

  if (fs.existsSync(projPath) && csvPath) {
    const buildHist = path.join(webRoot, "scripts", "build-player-history.mjs");
    if (fs.existsSync(buildHist)) {
      console.log("[alpha-caddie-web] Building player_round_history.json (was missing or empty) …");
      const r = spawnSync(process.execPath, [buildHist], {
        cwd: webRoot,
        stdio: "inherit",
        env: { ...process.env, GOLF_MODEL_DIR: repoRoot },
      });
      const built = readHistory();
      if (r.status === 0 && historyJsonLooksPopulated(built)) return;
      if (r.status !== 0) console.warn("[alpha-caddie-web] build-player-history exited", r.status);
    }
  }

  if (!stubsOk) {
    console.error(
      "[alpha-caddie-web] player_round_history.json missing or empty after rebuild attempt — confirm fetch:dg completes (writes history) or ship CSV + run build:history.",
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

export function ensureResultsBacktestAndKelly(webRoot, repoRoot, stubsOk) {
  const dataDir = path.join(webRoot, "data");
  fs.mkdirSync(dataDir, { recursive: true });
  const backtestPath = path.join(dataDir, "results_backtest.json");
  const kellyPath = path.join(dataDir, "results_kelly_bets.json");
  const matchupCsv = path.join(repoRoot, "data", "historical_matchups_outcomes.csv");
  const outrightCsv = path.join(repoRoot, "data", "historical_outrights_outcomes.csv");
  const br = path.join(webRoot, "scripts", "build-results-backtest.mjs");

  const rebuildOnRenderBoot =
    onRenderHost() &&
    String(process.env.GOLF_SKIP_RESULTS_BUILD_ON_BOOT || "").trim() !== "1" &&
    fs.existsSync(matchupCsv) &&
    fs.existsSync(outrightCsv) &&
    fs.existsSync(br);

  if (rebuildOnRenderBoot) {
    console.log("[alpha-caddie-web] Render: rebuilding Results + Kelly JSON from outcomes CSVs …");
    const r = spawnSync(process.execPath, [br], {
      cwd: webRoot,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: repoRoot },
    });
    if (r.status !== 0) console.warn("[alpha-caddie-web] build-results-backtest on boot exited", r.status);
  }

  let needBacktest = false;
  if (!fs.existsSync(backtestPath)) needBacktest = true;
  else {
    try {
      const j = JSON.parse(fs.readFileSync(backtestPath, "utf8"));
      if (!j || typeof j !== "object" || !Array.isArray(j.rows)) needBacktest = true;
    } catch {
      needBacktest = true;
    }
  }

  let needKelly = false;
  if (!fs.existsSync(kellyPath)) needKelly = true;
  else {
    try {
      const k = JSON.parse(fs.readFileSync(kellyPath, "utf8"));
      if (!k || typeof k !== "object" || !Array.isArray(k.bets)) needKelly = true;
    } catch {
      needKelly = true;
    }
  }

  if ((needBacktest || needKelly) && fs.existsSync(matchupCsv) && fs.existsSync(outrightCsv) && fs.existsSync(br)) {
    console.log("[alpha-caddie-web] Running build-results-backtest.mjs (outcomes CSVs present, JSON missing) …");
    const r = spawnSync(process.execPath, [br], {
      cwd: webRoot,
      stdio: "inherit",
      env: { ...process.env, GOLF_MODEL_DIR: repoRoot },
    });
    if (r.status !== 0) console.warn("[alpha-caddie-web] build-results-backtest exited", r.status);
    else {
      needBacktest = false;
      needKelly = false;
      try {
        const j = JSON.parse(fs.readFileSync(backtestPath, "utf8"));
        if (!j || !Array.isArray(j.rows)) needBacktest = true;
      } catch {
        needBacktest = true;
      }
      try {
        const k = JSON.parse(fs.readFileSync(kellyPath, "utf8"));
        if (!k || !Array.isArray(k.bets)) needKelly = true;
      } catch {
        needKelly = true;
      }
    }
  }

  if (needBacktest) {
    if (!stubsOk) {
      console.error(
        "[alpha-caddie-web] results_backtest.json missing — add data/historical_*_outcomes.csv and run npm run build:results (see RESULTS_EXPORT_LAST_YEARS).",
      );
    } else {
      fs.writeFileSync(
        backtestPath,
        JSON.stringify({
          generated_at: new Date().toISOString(),
          ev_bin_step: 0.5,
          ev_bin_min: -10,
          ev_bin_max: 40,
          markets: { matchups: [], outrights: [] },
          books: { matchups: [], outrights: [] },
          rows: [],
          note: "Offline stub — run npm run build:results.",
        }),
        "utf8",
      );
      console.warn("[alpha-caddie-web] Wrote empty results_backtest.json (local stub only).");
    }
  }

  if (needKelly) {
    if (!stubsOk) {
      console.error("[alpha-caddie-web] results_kelly_bets.json missing — same fix as results_backtest.json.");
    } else {
      fs.writeFileSync(
        kellyPath,
        JSON.stringify({
          schema: 3,
          generated_at: new Date().toISOString(),
          bankroll0: 100,
          kelly_fraction: 0.25,
          max_kelly_stake_frac: 0.15,
          outrights_price: "close_american_decimal",
          cols: [
            "t",
            "date",
            "source",
            "market",
            "book",
            "ev_pct",
            "p",
            "dec",
            "win",
            "event_id",
            "player_name",
            "event_name",
            "dg_id",
          ],
          n: 0,
          bets: [],
          note: "Offline stub — run npm run build:results.",
        }),
        "utf8",
      );
      console.warn("[alpha-caddie-web] Wrote empty results_kelly_bets.json (local stub only).");
    }
  }
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
