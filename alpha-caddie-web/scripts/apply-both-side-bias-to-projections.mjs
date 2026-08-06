#!/usr/bin/env node
/**
 * Apply walk-forward chrono/loo μ bias from both_side_roi.json onto live projections.json.
 * Only adjusts markets with both_sides_positive policies.
 *
 *   node scripts/apply-both-side-bias-to-projections.mjs
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PROJ = join(WEB, "projections.json");
const ROI = join(WEB, "data", "both_side_roi.json");
const BETS = join(WEB, "data", "both_side_bets.json");

const FIELD = {
  "Total score": "total_score",
  Birdies: "birdies",
  Bogeys: "bogeys",
  Pars: "pars",
  GIR: "gir",
  "Fairways hit": "fairways",
};

function main() {
  if (!existsSync(PROJ)) throw new Error(`Missing ${PROJ}`);
  const roi = existsSync(ROI) ? JSON.parse(readFileSync(ROI, "utf8")) : null;
  const bets = existsSync(BETS) ? JSON.parse(readFileSync(BETS, "utf8")) : null;
  const liveBias = bets?.live_bias || roi?.live_bias || {};
  const pass = new Set(roi?.overall?.both_side_positive_markets || []);
  const proj = JSON.parse(readFileSync(PROJ, "utf8"));
  if (proj.both_side_bias_applied?.at) {
    console.log("[both-side-bias] Already applied — re-run apply:dg-methodology first to reset raw μ.");
    return;
  }
  const players = Array.isArray(proj.players) ? proj.players : [];
  let n = 0;
  for (const p of players) {
    for (const [market, field] of Object.entries(FIELD)) {
      if (!pass.has(market)) continue;
      const bias = Number(liveBias[market]);
      if (!Number.isFinite(bias) || Math.abs(bias) < 1e-9) continue;
      const raw = Number(p[field]);
      if (!Number.isFinite(raw)) continue;
      p[field] = Math.round((raw - bias) * 1000) / 1000;
      n++;
    }
    if (Number.isFinite(Number(p.total_score)) && Number.isFinite(Number(proj.course_par_18))) {
      p.sg_total = Math.round((Number(proj.course_par_18) - Number(p.total_score)) * 1000) / 1000;
    }
  }
  proj.both_side_bias_applied = {
    at: new Date().toISOString(),
    live_bias: liveBias,
    markets: [...pass],
  };
  proj.projection_recipe = `${proj.projection_recipe || "dg_methodology"}+both_side_bias`;
  writeFileSync(PROJ, `${JSON.stringify(proj, null, 2)}\n`);
  console.log(`[both-side-bias] Adjusted ${n} player-market fields → ${PROJ}`);
  console.log(`[both-side-bias] live_bias`, liveBias);
}

main();
