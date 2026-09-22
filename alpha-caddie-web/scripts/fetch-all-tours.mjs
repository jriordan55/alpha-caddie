#!/usr/bin/env node
/**
 * Fetch live projections for PGA Tour and DP World Tour (DataGolf codes: pga, euro).
 *
 *   npm run fetch:dg:all-tours
 */
import path from "path";
import { fileURLToPath } from "url";
import { spawnSync } from "child_process";
import { LIVE_PROJECTION_TOURS } from "./golf-tours.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");

for (const tour of LIVE_PROJECTION_TOURS) {
  console.log(`\n[fetch:dg:all-tours] === ${tour} ===\n`);
  const r = spawnSync(process.execPath, ["scripts/refresh-live-tour.mjs"], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...process.env, GOLF_DATAGOLF_TOUR: tour, GOLF_TOUR: tour },
  });
  if (r.status !== 0) {
    console.error(`[fetch:dg:all-tours] ${tour} failed (exit ${r.status})`);
    process.exit(r.status || 1);
  }
}

console.log("\n[fetch:dg:all-tours] Done.\n");
