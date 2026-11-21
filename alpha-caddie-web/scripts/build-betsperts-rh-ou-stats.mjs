#!/usr/bin/env node
/**
 * Pull Rabbit Hole OU market stats (All filters) → data/betsperts_rh_ou_stats.json
 * plus filter option lists for the Round projections toolbar.
 *
 *   npm run build:betsperts-rh-ou
 */
import { mkdirSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import {
  WEB,
  ROOT,
  fetchRhOuStatsBundle,
  loadRhFilterOptionsFromDisk,
  resolveBetspertsSessionKey,
} from "./betsperts-rh-ou.mjs";

function writeJson(path, obj) {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, JSON.stringify(obj) + "\n", "utf8");
}

const sessionKey = resolveBetspertsSessionKey();
if (!sessionKey) {
  console.warn("[betsperts-rh-ou] No session key — writing filter options only (stats unchanged if missing).");
}

const filtersMeta = loadRhFilterOptionsFromDisk();
const filtersOut = {
  generatedAt: new Date().toISOString(),
  source: "data/betsperts rabbit-hole + courses index",
  ...filtersMeta,
};
writeJson(join(WEB, "data", "betsperts_rh_filters.json"), filtersOut);
writeJson(join(ROOT, "data", "betsperts_rh_filters.json"), filtersOut);
console.log(
  `[betsperts-rh-ou] filters: regions=${filtersMeta.regions.length} types=${filtersMeta.types.length} courses=${filtersMeta.courses.length}`,
);

if (!sessionKey) {
  process.exit(0);
}

const bundle = await fetchRhOuStatsBundle({ timeframe: "all" }, { sessionKey });
if (!bundle.ok) {
  console.error("[betsperts-rh-ou] fetch failed:", bundle.error);
  process.exit(1);
}

const payload = {
  generatedAt: bundle.generatedAt,
  source: bundle.source,
  filters: bundle.filters,
  viewStatus: bundle.viewStatus,
  n: bundle.n,
  players: bundle.players,
};

writeJson(join(WEB, "data", "betsperts_rh_ou_stats.json"), payload);
writeJson(join(ROOT, "data", "betsperts_rh_ou_stats.json"), payload);
console.log(`[betsperts-rh-ou] wrote ${bundle.n} players → data/betsperts_rh_ou_stats.json`);
console.log("[betsperts-rh-ou] view status", bundle.viewStatus);
