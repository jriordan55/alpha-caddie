#!/usr/bin/env node
/**
 * Recompute `pga_tour_market_benchmarks` on projections.json from data/historical_rounds_all.csv.
 * Called at end of refresh:live (after post-live CSV merge) so Market rating uses fresh 2025–2026 rounds.
 *
 *   npm run refresh:market-benchmarks
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  loadPgaTourMarketBenchmarks,
  serializePgaTourMarketBenchmarks,
} from "./pga-tour-market-benchmarks.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

async function main() {
  if (!existsSync(projPath)) {
    console.warn("[refresh:market-benchmarks] missing projections.json — skip");
    process.exit(0);
  }
  const modelRoot = process.env.GOLF_MODEL_DIR?.trim()
    ? resolve(process.env.GOLF_MODEL_DIR.trim())
    : resolve(WEB_ROOT, "..");
  const minYear = Math.round(num(process.env.GOLF_PGA_TOUR_BENCHMARK_MIN_YEAR, 2025)) || 2025;
  const maxYear = Math.round(num(process.env.GOLF_PGA_TOUR_BENCHMARK_MAX_YEAR, 2026)) || 2026;

  let proj;
  try {
    proj = JSON.parse(readFileSync(projPath, "utf8"));
  } catch (e) {
    console.warn("[refresh:market-benchmarks] parse error —", e.message || e);
    process.exit(0);
  }

  const raw = await loadPgaTourMarketBenchmarks(modelRoot, { minYear, maxYear });
  proj.pga_tour_market_benchmarks = serializePgaTourMarketBenchmarks(raw);
  writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`, "utf8");

  const meta = raw.meta || {};
  const b = proj.pga_tour_market_benchmarks?.["Total score"];
  if (meta.skipped) {
    console.warn("[refresh:market-benchmarks] skipped (no CSV or empty sample)");
  } else {
    console.log(
      `[refresh:market-benchmarks] ${meta.min_year}–${meta.max_year} score μ=${b?.mean} σ=${b?.sd} (n=${meta.n?.score ?? "?"})`,
    );
  }
  process.exit(0);
}

main().catch((e) => {
  console.error("[refresh:market-benchmarks] fatal:", e.message || e);
  process.exit(1);
});
