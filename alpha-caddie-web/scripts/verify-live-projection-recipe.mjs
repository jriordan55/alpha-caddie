#!/usr/bin/env node
/**
 * Guard published projections.json μ recipe for push:live / refresh:live.
 * Fails if chrono both-side bias or sportsbook alignment stamps are present,
 * or if hierarchical μ was expected but missing.
 *
 *   node scripts/verify-live-projection-recipe.mjs
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PROJ = join(WEB, "projections.json");

function envOn(name, fallback = true) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return fallback;
  const s = String(raw).trim().toLowerCase();
  return !(s === "0" || s === "false" || s === "off" || s === "no");
}

if (!existsSync(PROJ)) {
  console.error(`[verify:live-recipe] missing ${PROJ}`);
  process.exit(1);
}

const proj = JSON.parse(readFileSync(PROJ, "utf8"));
const recipe = String(proj.projection_recipe || "");
const wantHier = envOn("GOLF_HIERARCHICAL_MU", true);
const allowBias = envOn("GOLF_APPLY_BOTH_SIDE_BIAS", false);
const errors = [];

if (wantHier) {
  if (!recipe.includes("hierarchical_mu") && !proj.hierarchical_mu?.model) {
    errors.push(`expected hierarchical_mu recipe, got "${recipe || "(empty)"}"`);
  }
  if (recipe.includes("both_side_bias") && !allowBias) {
    errors.push(`recipe still includes both_side_bias: "${recipe}"`);
  }
}

if (!allowBias && proj.both_side_bias_applied?.at) {
  errors.push(`both_side_bias_applied is set (${proj.both_side_bias_applied.at})`);
}

const bookCal = proj.meta?.market_book_calibration || proj.market_book_calibration;
if (bookCal && bookCal.enabled !== false && bookCal.applied === true) {
  errors.push("market_book_calibration applied on projections");
}

if (proj.meta?.event_prop_book_alignment?.markets && Object.keys(proj.meta.event_prop_book_alignment.markets).length) {
  errors.push("event_prop_book_alignment markets present");
}

if (proj.meta?.bayesian_market_posterior?.rows > 0 || proj.bayesian_market_posterior?.rows > 0) {
  errors.push("bayesian_market_posterior applied");
}

if (errors.length) {
  console.error("[verify:live-recipe] FAIL:");
  for (const e of errors) console.error(`  • ${e}`);
  process.exit(1);
}

console.log(
  `[verify:live-recipe] OK — recipe=${recipe || "(none)"} hier=${Boolean(proj.hierarchical_mu?.model)} bias=off book=off`,
);
