#!/usr/bin/env node
/**
 * Fast path: refresh projections + pin sheet only (~2–4 min).
 * Skips historical rounds CSV, weather backfill, build:history, embed, pgatouR, audit CSVs.
 *
 *   npm run refresh:projections
 *
 * Env: DATAGOLF_API_KEY, GOLF_SKIP_PIN_SHEET=1, GOLF_REFRESH_PROJECTIONS_SKIP_DG=1 (reuse disk projections)
 */
import { spawnSync } from "child_process";
import { copyFileSync, existsSync, mkdirSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

function run(rel, label, extraEnv = {}) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[refresh:projections] ${label}…\n`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: {
      ...process.env,
      GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT,
      ...extraEnv,
    },
  });
  if (r.status !== 0) {
    console.error(`[refresh:projections] ${label} failed (exit ${r.status ?? "?"})`);
    process.exit(r.status ?? 1);
  }
}

const baseEnv = { GOLF_SKIP_HISTORY_ON_FETCH_DG: "1" };

if (String(process.env.GOLF_REFRESH_PROJECTIONS_SKIP_DG || "").trim() !== "1") {
  run("fetch-datagolf.mjs", "Field + projections (fetch:dg)", baseEnv);
} else if (!existsSync(path.join(WEB_ROOT, "projections.json"))) {
  console.error("[refresh:projections] Missing projections.json");
  process.exit(1);
} else {
  console.log("\n[refresh:projections] GOLF_REFRESH_PROJECTIONS_SKIP_DG=1 — reusing projections.json on disk.\n");
}

run("fetch-live-in-play.mjs", "field-updates tee times + live bundle (fetch:in-play)");
run("merge-live-round-meta-into-projections.mjs", "display_round from field_updates (merge-live-round-meta)");
run("merge-field-teetimes-into-projections.mjs", "field-updates tee times → dg_teetime_local");
run(
  "within-event-projection-apply.mjs",
  "Re-apply field-average prior-round form from live-in-play",
);
run(
  "repair-projection-course-basis.mjs",
  "Venue total-score calibration (repair:projection-course-basis)",
);
run("bake-weather-into-projections.mjs", "Tee-time weather for display_round (bake:weather)");
run("apply-unified-projection-factors.mjs", "Unified projection factors");

if (String(process.env.GOLF_SKIP_PIN_SHEET || "").trim() !== "1") {
  run("pin-hole-scoring-index.mjs", "Pin hole scoring index (Bayesian calibration)");
  run("apply-pin-sheet-to-projections.mjs", "Armed pin sheet → projections (Bayesian apply:pin-sheet)");
}

run(
  "reconcile-projection-counts.mjs",
  "Final bird/bog/par reconcile from total_score (after pin + weather + unified)",
);

const destDir = path.join(REPO_ROOT, "website", "public", "data");
mkdirSync(destDir, { recursive: true });
copyFileSync(path.join(WEB_ROOT, "projections.json"), path.join(destDir, "projections.json"));
console.log("\n[refresh:projections] Mirrored projections.json → website/public/data/\n");
console.log("[refresh:projections] Done. Publish: npm run push:projections\n");
