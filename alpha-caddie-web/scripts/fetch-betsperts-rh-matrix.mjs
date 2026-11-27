#!/usr/bin/env node
/**
 * Pull full Betsperts Rabbit Hole matrix for all players:
 *   all views × Average/Total × all seasons + rolling round windows.
 *
 *   node scripts/fetch-betsperts-rh-matrix.mjs
 *   node scripts/fetch-betsperts-rh-matrix.mjs --limit-slices=12
 *   npm run fetch:betsperts:rh-matrix
 *
 * Env: BETSPERTS_SESSION_KEY or alpha-caddie-web/betsperts.local.json
 *      GOLF_SKIP_BETSPERTS=1 to skip
 *      BETSPERTS_RH_CONCURRENCY (default 3)
 */
import { pullRhMatrix, resolveBetspertsSessionKey } from "./betsperts-rh-matrix.mjs";

function envTruthy(name, defaultVal = false) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

const args = process.argv.slice(2);
const limitArg = args.find((a) => a.startsWith("--limit-slices="));
const LIMIT = limitArg ? Math.max(0, Number(limitArg.split("=")[1]) || 0) : 0;
const startYearArg = args.find((a) => a.startsWith("--start-year="));
const START_YEAR = startYearArg ? Number(startYearArg.split("=")[1]) || 2015 : 2015;

async function main() {
  if (envTruthy("GOLF_SKIP_BETSPERTS", false)) {
    console.log("GOLF_SKIP_BETSPERTS=1 — skipping RH matrix pull.");
    return;
  }
  if (!resolveBetspertsSessionKey()) {
    console.error("No Betsperts sessionKey — set betsperts.local.json or BETSPERTS_SESSION_KEY.");
    process.exitCode = 1;
    return;
  }
  const res = await pullRhMatrix({
    limitSlices: LIMIT,
    startYear: START_YEAR,
  });
  if (!res.ok) {
    console.error("[rh-matrix] failed", res.error || res);
    process.exitCode = 1;
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
