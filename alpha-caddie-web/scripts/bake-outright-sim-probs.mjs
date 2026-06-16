#!/usr/bin/env node
/**
 * Pre-compute tournament MC placement probs into projections.json (browser reads, no runtime MC).
 *   node scripts/bake-outright-sim-probs.mjs
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { runTournamentMcFromProjections } from "./tournament-mc-outrights.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");
const websiteProjPath = join(WEB_ROOT, "..", "website", "public", "data", "projections.json");

export function bakeOutrightSimProbsForProjections(proj, opts = {}) {
  const nSims = Math.round(Number(opts.nSims ?? proj.outright_model_mc_sims ?? 500)) || 500;
  const mc = runTournamentMcFromProjections(proj, { nSims, seed: 42 });
  if (!mc?.maps || !mc.field?.length) return null;

  const by_dg = {};
  for (const f of mc.field) {
    const id = String(f.id);
    by_dg[id] = {
      win: mc.maps.win.get(f.id),
      top_5: mc.maps.top_5.get(f.id),
      top_10: mc.maps.top_10.get(f.id),
      top_20: mc.maps.top_20.get(f.id),
      make_cut: mc.maps.make_cut.get(f.id),
      frl: mc.maps.frl.get(f.id),
    };
  }

  const liveLu = String(
    proj.datagolf_live_last_update ?? proj.meta?.datagolf_live_last_update ?? "",
  );

  return {
    n_sims: mc.nSims,
    baked_at: new Date().toISOString(),
    ratings_updated_at: String(proj.updated_at || ""),
    live_through: liveLu,
    by_dg,
  };
}

export function applyOutrightSimProbsBake(proj, opts = {}) {
  const bake = bakeOutrightSimProbsForProjections(proj, opts);
  if (!bake) return null;
  proj.outright_sim_probs = bake;
  proj.outright_model_mc_in_browser = false;
  return bake;
}

function writeProjections(proj, paths) {
  const json = JSON.stringify(proj, null, 2);
  for (const p of paths) {
    if (!existsSync(dirname(p))) continue;
    writeFileSync(p, json, "utf8");
  }
}

function main() {
  if (!existsSync(projPath)) {
    console.error("[bake:outright-sim] missing projections.json");
    process.exit(1);
  }
  const proj = JSON.parse(readFileSync(projPath, "utf8"));
  const bake = applyOutrightSimProbsBake(proj);
  if (!bake) {
    console.error("[bake:outright-sim] MC produced no field");
    process.exit(1);
  }
  const paths = [projPath];
  if (existsSync(dirname(websiteProjPath))) paths.push(websiteProjPath);
  writeProjections(proj, paths);
  console.log(
    `[bake:outright-sim] Wrote outright_sim_probs for ${Object.keys(bake.by_dg).length} players (${bake.n_sims} sims) → ${paths.join(", ")}`,
  );
}

if (import.meta.url === `file://${process.argv[1]?.replace(/\\/g, "/")}` || process.argv[1]?.endsWith("bake-outright-sim-probs.mjs")) {
  main();
}
