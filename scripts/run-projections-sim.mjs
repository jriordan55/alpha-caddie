#!/usr/bin/env node
/**
 * Single entry: full sim projections + JSON export (no Rscript on PATH required).
 *
 *   npm run projections:sim
 *   npm run projections:sim:fast   # or: node scripts/run-projections-sim.mjs --fast
 *
 * Runs R twice from Node with stdio inherited (so you always see R errors on Windows).
 * Finds Rscript via: RSCRIPT_EXE, Program Files/R/R-version/bin/Rscript.exe, then `where Rscript`.
 *
 * Defaults (R): round-level Gaussian tournament MC; shot paths only if GOLF_USE_SHOT_LEVEL_MC=1.
 * Speed (--fast): fewer outer sims, skips optional shot count MC and DG form API batch where unset.
 *   GOLF_TOURNAMENT_MC_NSIM=50..2000 — override outer tournament sim count (R).
 *   GOLF_USE_SHOT_LEVEL_MC=1 — shot-path tournament MC (slower).
 *   GOLF_USE_SHOT_LEVEL_PROJECTION_COUNTS=1 — inner shot count Monte Carlo for score-type columns.
 *   npm start: GOLF_SKIP_REFRESH_ON_START=1 or GOLF_SKIP_HISTORICAL_ROUNDS_MERGE_ON_START=1 (see serve-with-refresh.mjs).
 */
import { spawnSync, execSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(__dirname, "..");

function findRscript() {
  const fromEnv = process.env.RSCRIPT_EXE?.trim();
  if (fromEnv && fs.existsSync(fromEnv)) return fromEnv;

  const pf = process.env.ProgramFiles || "C:\\Program Files";
  const rRoot = path.join(pf, "R");
  if (fs.existsSync(rRoot)) {
    const dirs = fs
      .readdirSync(rRoot, { withFileTypes: true })
      .filter((d) => d.isDirectory() && /^R-/.test(d.name))
      .map((d) => d.name)
      .sort()
      .reverse();
    for (const d of dirs) {
      const exe = path.join(rRoot, d, "bin", "Rscript.exe");
      if (fs.existsSync(exe)) return exe;
    }
  }

  try {
    const out = execSync("where Rscript", { encoding: "utf8", windowsHide: true }).trim();
    const first = out.split(/\r?\n/).find((l) => l.trim().endsWith("Rscript.exe") || l.trim().endsWith("Rscript"));
    if (first && fs.existsSync(first.trim())) return first.trim();
  } catch {
    /* ignore */
  }

  return null;
}

function hasDataGolfCredentials() {
  if (process.env.DATAGOLF_API_KEY?.trim()) return true;
  for (const rel of ["alpha-caddie-web/datagolf.local.json", "website/datagolf.local.json"]) {
    if (fs.existsSync(path.join(repo, rel))) return true;
  }
  return false;
}

const rscript = findRscript();
if (!rscript) {
  console.error(
    "Could not find Rscript.exe. Install R, add it to PATH, or set RSCRIPT_EXE to the full path (e.g. C:\\Program Files\\R\\R-4.2.0\\bin\\Rscript.exe)."
  );
  process.exit(1);
}

if (!hasDataGolfCredentials()) {
  console.error(
    "No DataGolf credentials. Set environment variable DATAGOLF_API_KEY or create:\n" +
      `  ${path.join(repo, "alpha-caddie-web", "datagolf.local.json")}\n` +
      'with JSON like: { "apiKey": "YOUR_KEY" }'
  );
  process.exit(1);
}

const roundProj = path.join(repo, "round_projections.R");
const exportProj = path.join(repo, "scripts", "export_projections_for_website.R");
if (!fs.existsSync(roundProj)) {
  console.error("Missing", roundProj);
  process.exit(1);
}
if (!fs.existsSync(exportProj)) {
  console.error("Missing", exportProj);
  process.exit(1);
}

const env = {
  ...process.env,
  GOLF_MODEL_DIR: repo,
  GOLF_RAW_PROJECTIONS: "0",
  GOLF_PLACEMENT_SOURCE: "sim",
};
if (!String(process.env.GOLF_POLL_DATAGOLF_LIVE || "").trim()) {
  env.GOLF_POLL_DATAGOLF_LIVE = "0";
}

const fast =
  process.argv.includes("--fast") ||
  String(process.env.GOLF_FAST_PROJECTIONS || "").trim() === "1";
if (fast) {
  const setIfUnset = (k, v) => {
    if (!String(process.env[k] || "").trim()) env[k] = v;
  };
  setIfUnset("GOLF_USE_SHOT_LEVEL_MC", "0");
  setIfUnset("GOLF_USE_SHOT_LEVEL_PROJECTION_COUNTS", "0");
  setIfUnset("GOLF_TOURNAMENT_MC_NSIM", "280");
  setIfUnset("GOLF_SHOT_COUNT_MC_NSIM", "5");
  setIfUnset("GOLF_DG_WITHIN_EVENT_FORM_API", "0");
  console.log(
    "[run-projections-sim] fast mode — Gaussian MC, GOLF_TOURNAMENT_MC_NSIM=280, no shot count MC, no DG form API batch (override any with env before npm)."
  );
}

console.log("Using", rscript);

function runR(label, scriptPath) {
  console.log(`\n[${label}]`, scriptPath);
  const r = spawnSync(rscript, [scriptPath], { cwd: repo, env, stdio: "inherit" });
  const code = r.status ?? 1;
  if (code !== 0) {
    console.error(`\n${label} exited with code ${code}. Read the R messages above (Error / Field updates / empty field).`);
    process.exit(code);
  }
}

runR("round_projections.R", roundProj);
runR("export_projections_for_website.R", exportProj);
console.log("\nDone: simulated_round_static.rds + website/public/data/projections.json + alpha-caddie-web/projections.json");
