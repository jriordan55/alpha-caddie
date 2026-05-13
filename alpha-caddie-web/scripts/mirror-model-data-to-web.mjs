#!/usr/bin/env node
/**
 * Copies canonical model CSVs into alpha-caddie-web/data/ so the web folder and
 * Explorer stay aligned with golfModel/data/ after R refreshes.
 *
 *   - data/historical_rounds_all.csv
 *   - data/all_shots_2022_2026.csv + round fairways/GIR/putts aggregate (only if options.includeAllShotsCsv
 *     or GOLF_USE_ALL_SHOTS_CSV=1 — default off; huge files stay out of the web mirror)
 *   - hole_data.csv from data/hole_data.csv or repo root hole_data.csv
 *
 * Import: import { mirrorModelDataToWeb } from "./mirror-model-data-to-web.mjs";
 * CLI: node scripts/mirror-model-data-to-web.mjs
 */
import { execSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

function sleepSyncMs(ms) {
  const sec = Math.max(1, Math.ceil(ms / 1000));
  try {
    if (process.platform === "win32") {
      execSync(`timeout /t ${sec} /nobreak >nul 2>&1`, { stdio: "ignore" });
    } else {
      execSync(`sleep ${sec}`, { stdio: "ignore" });
    }
  } catch {
    /* timeout.exe may exit 1 */
  }
}

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");

/**
 * @param {string} repoRoot - golfModel root (parent of alpha-caddie-web)
 * @param {string} webRoot - alpha-caddie-web root
 * @param {{ includeAllShotsCsv?: boolean }} [options] — copy all_shots + round aggregate when true (or env GOLF_USE_ALL_SHOTS_CSV=1). Default: do not copy.
 */
/** Windows often returns EBUSY if Excel/OneDrive/AV has the file open — retry with short delay. */
function copyFileRetry(src, dest, label) {
  const max = 6;
  for (let i = 1; i <= max; i++) {
    try {
      fs.copyFileSync(src, dest);
      return true;
    } catch (e) {
      const busy = e && (e.code === "EBUSY" || e.code === "EPERM");
      if (busy && i < max) {
        console.warn(`[mirror-model-data-to-web] ${label} locked (${e.code}), retry ${i}/${max - 1} in 1.5s…`);
        sleepSyncMs(1500);
        continue;
      }
      console.warn("[mirror-model-data-to-web]", label, e.message || e);
      return false;
    }
  }
  return false;
}

export function mirrorModelDataToWeb(repoRoot, webRoot, options = {}) {
  const wantShots =
    options.includeAllShotsCsv === true ||
    String(process.env.GOLF_USE_ALL_SHOTS_CSV || "").trim() === "1";
  const webData = path.join(webRoot, "data");
  fs.mkdirSync(webData, { recursive: true });
  const copied = [];

  const hist = path.join(repoRoot, "data", "historical_rounds_all.csv");
  if (fs.existsSync(hist)) {
    const ok = copyFileRetry(hist, path.join(webData, "historical_rounds_all.csv"), "historical_rounds_all.csv");
    if (ok) copied.push("historical_rounds_all.csv");
  }

  const shots = path.join(repoRoot, "data", "all_shots_2022_2026.csv");
  if (wantShots && fs.existsSync(shots)) {
    const ok = copyFileRetry(shots, path.join(webData, "all_shots_2022_2026.csv"), "all_shots_2022_2026.csv");
    if (ok) copied.push("all_shots_2022_2026.csv");
  } else if (!wantShots) {
    console.log("[mirror-model-data-to-web] skip all_shots + round aggregate (set GOLF_USE_ALL_SHOTS_CSV=1 or includeAllShotsCsv:true to copy)");
  }

  const roundAgg = path.join(repoRoot, "data", "all_shots_2022_2026_round_fairways_gir_putts.csv");
  if (wantShots && fs.existsSync(roundAgg)) {
    const ok = copyFileRetry(
      roundAgg,
      path.join(webData, "all_shots_2022_2026_round_fairways_gir_putts.csv"),
      "all_shots_2022_2026_round_fairways_gir_putts.csv"
    );
    if (ok) copied.push("all_shots_2022_2026_round_fairways_gir_putts.csv");
  }

  const holeSrc = [path.join(repoRoot, "data", "hole_data.csv"), path.join(repoRoot, "hole_data.csv")].find((p) =>
    fs.existsSync(p)
  );
  if (holeSrc) {
    const ok = copyFileRetry(holeSrc, path.join(webData, "hole_data.csv"), "hole_data.csv");
    if (ok) copied.push("hole_data.csv");
  }

  const courseTable = path.join(repoRoot, "data", "course_table.csv");
  if (fs.existsSync(courseTable)) {
    const ok = copyFileRetry(courseTable, path.join(webData, "course_table.csv"), "course_table.csv");
    if (ok) copied.push("course_table.csv");
  }

  if (copied.length) {
    console.log("[mirror-model-data-to-web]", copied.join(", "), "→", webData);
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  mirrorModelDataToWeb(REPO_ROOT, WEB_ROOT);
}
