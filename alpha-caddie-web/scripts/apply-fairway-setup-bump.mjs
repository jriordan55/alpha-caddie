#!/usr/bin/env node
/**
 * +1 fairway hit per player on the display / weather-baked round (widened fairways).
 *
 *   npm run apply:fairway-setup-bump
 *   GOLF_FAIRWAY_SETUP_BUMP=0 to skip; default bump = 1
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { num } from "./pin-sheet-difficulty.mjs";
import { flattenProjectionExportMeta, projectionExportMeta } from "./projection-export-meta.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const PROJ_PATH = join(WEB_ROOT, "projections.json");

function resolveBumpRound(payload, meta) {
  const baked = Math.round(num(meta.projection_counts_weather_baked_round, NaN));
  if (Number.isFinite(baked) && baked >= 1 && baked <= 4) return baked;
  const dr = Math.round(num(payload.display_round ?? meta.display_round, NaN));
  if (Number.isFinite(dr) && dr >= 1 && dr <= 4) return dr;
  return 1;
}

/** @returns {number} rows updated */
export function applyFairwaySetupBump(payload, opts = {}) {
  const bump = num(process.env.GOLF_FAIRWAY_SETUP_BUMP ?? opts.bump, 1);
  if (!Number.isFinite(bump) || bump === 0) return 0;

  const meta = projectionExportMeta(payload);
  const basis = meta.projection_course_basis && typeof meta.projection_course_basis === "object" ? meta.projection_course_basis : {};
  const nFw = Math.round(num(basis.fairway_holes_modeled, 14)) || 14;
  const cap = nFw + 0.2;
  const rnd = resolveBumpRound(payload, meta);

  const prevBump = num(meta.fairway_setup_widen_bump, NaN);
  const prevRnd = Math.round(num(meta.fairway_setup_widen_round, NaN));

  let n = 0;
  for (const p of payload.players || []) {
    if (Math.round(num(p.round, NaN)) !== rnd) continue;
    const fw = num(p.fairways, NaN);
    if (!Number.isFinite(fw)) continue;

    if (Number.isFinite(prevBump) && prevBump !== 0 && prevRnd === rnd && p._fairway_bump_applied) {
      const base = num(p._fairway_bump_base, fw - prevBump);
      p._fairway_bump_base = base;
      p.fairways = Math.round(Math.min(cap, base + bump) * 100) / 100;
    } else {
      p._fairway_bump_base = fw;
      p.fairways = Math.round(Math.min(cap, fw + bump) * 100) / 100;
      p._fairway_bump_applied = true;
    }
    n++;
  }

  meta.fairway_setup_widen_bump = bump;
  meta.fairway_setup_widen_round = rnd;
  meta.fairway_setup_widen_applied_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  return n;
}

function main() {
  if (!existsSync(PROJ_PATH)) {
    console.error("[fairway-setup-bump] Missing projections.json");
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(PROJ_PATH, "utf8"));
  const n = applyFairwaySetupBump(payload);
  flattenProjectionExportMeta(payload);
  writeFileSync(PROJ_PATH, `${JSON.stringify(payload, null, 2)}\n`, "utf8");
  const meta = projectionExportMeta(payload);
  console.log(
    `[fairway-setup-bump] +${meta.fairway_setup_widen_bump ?? 1} fairway(s) on R${meta.fairway_setup_widen_round ?? "?"} for ${n} player row(s)`,
  );
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
if (isMain) {
  main();
}
