#!/usr/bin/env node
/**
 * Re-fetch DataGolf rounds for seasons with low teetime coverage, then patch history shards.
 * Requires DATAGOLF_API_KEY (or datagolf.local.json).
 *
 *   npm run backfill:history-teetimes
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { fileURLToPath } from "url";
import { resolveGolfModelDir } from "./resolve-golf-model-dir.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = resolveGolfModelDir(WEB_ROOT);
const ROUNDS_CSV =
  process.env.HISTORICAL_ROUNDS_CSV ||
  [path.join(REPO_ROOT, "data", "historical_rounds_all.csv"), path.join(WEB_ROOT, "data", "historical_rounds_all.csv")].find(
    (p) => fs.existsSync(p),
  ) ||
  path.join(REPO_ROOT, "data", "historical_rounds_all.csv");

async function yearsNeedingTeetimeBackfill(minCoverage = 0.95, minYear = 2014) {
  const byYear = new Map();
  const parser = createReadStream(ROUNDS_CSV).pipe(
    parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
  );
  for await (const row of parser) {
    const y = parseInt(String(row.year || ""), 10);
    if (!Number.isFinite(y) || y < minYear) continue;
    const tour = String(row.tour || "pga").toLowerCase();
    if (tour !== "pga" && tour !== "liv") continue;
    if (!byYear.has(y)) byYear.set(y, { total: 0, withTee: 0 });
    const b = byYear.get(y);
    b.total++;
    if (String(row.teetime ?? row.tee_time ?? "").trim()) b.withTee++;
  }
  const need = [];
  for (const [y, b] of [...byYear.entries()].sort((a, c) => a[0] - c[0])) {
    const cov = b.total ? b.withTee / b.total : 0;
    if (cov < minCoverage) need.push(y);
    console.log(`[backfill:history-teetimes] ${y}: ${b.withTee}/${b.total} (${(cov * 100).toFixed(1)}%)`);
  }
  return need;
}

function runNode(script, extraEnv = {}) {
  const r = spawnSync(process.execPath, [path.join(WEB_ROOT, "scripts", script)], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, ...extraEnv },
  });
  if (r.status !== 0) process.exit(r.status ?? 1);
}

async function main() {
  if (!fs.existsSync(ROUNDS_CSV)) {
    console.error("[backfill:history-teetimes] Missing CSV:", ROUNDS_CSV);
    process.exit(1);
  }
  const years = await yearsNeedingTeetimeBackfill();
  if (!years.length) {
    console.log("[backfill:history-teetimes] All seasons since 2014 meet teetime coverage threshold.");
    runNode("patch-history-teetimes-from-csv.mjs");
    return;
  }
  console.log("[backfill:history-teetimes] Re-fetching seasons:", years.join(", "));
  runNode("update-historical-rounds-node.mjs", {
    GOLF_HISTORICAL_ROUNDS_YEARS: years.join(","),
    GOLF_ROUNDS_PREFER_JSON_FIRST: "1",
  });
  runNode("patch-history-teetimes-from-csv.mjs");
}

main().catch((e) => {
  console.error("[backfill:history-teetimes]", e?.message || e);
  process.exit(1);
});
