#!/usr/bin/env node
/**
 * Overwrite live projections.json round O/U μ with DataGolf methodology.
 *
 *   node scripts/apply-dg-methodology-to-projections.mjs
 *
 * Uses historical_rounds_all.csv + current field/event from projections.json.
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { buildDgMethodologyMuMapForEvent, dgMethodologyEnabled } from "./dg-methodology-mu.mjs";
import { num } from "./round-projection-mu.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const PROJ = join(WEB, "projections.json");
const HIST = join(REPO, "data", "historical_rounds_all.csv");

const FIELD_MAP = {
  "Total score": "total_score",
  Birdies: "birdies",
  Bogeys: "bogeys",
  Pars: "pars",
  GIR: "gir",
  "Fairways hit": "fairways",
};

async function loadHistRows() {
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);
  /** @type {object[]} */
  const rows = [];
  await new Promise((resolvePromise, reject) => {
    createReadStream(HIST)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", resolvePromise)
      .on("error", reject);
  });
  return rows;
}

async function main() {
  if (!existsSync(PROJ)) throw new Error(`Missing ${PROJ}`);
  process.env.GOLF_DG_METHODOLOGY = process.env.GOLF_DG_METHODOLOGY || "1";
  if (!dgMethodologyEnabled()) {
    console.log("[dg-mu] GOLF_DG_METHODOLOGY off — skip");
    return;
  }

  const proj = JSON.parse(readFileSync(PROJ, "utf8"));
  const players = Array.isArray(proj.players) ? proj.players : [];
  if (!players.length) throw new Error("projections.json has no players");

  const eventName = String(proj.event_name || proj.meta?.event_name || "").trim();
  const courseName = String(proj.course_used || proj.course_name || "").trim();
  const targetRound = Math.round(
    num(proj.display_round ?? proj.datagolf_field_current_round ?? proj.meta?.round, 1),
  );
  const eventYear = Math.round(num(String(proj.datagolf_field_date_start || "").slice(0, 4), new Date().getFullYear()));
  const betTimeMs = Date.parse(proj.updated_at || "") || Date.now();
  const fieldDgIds = players.map((p) => Math.round(num(p.dg_id, NaN))).filter(Number.isFinite);

  console.log(
    `[dg-mu] Building DG methodology μ for ${eventName} R${targetRound} · ${fieldDgIds.length} players…`,
  );
  const histRows = await loadHistRows();
  const byDg = await buildDgMethodologyMuMapForEvent({
    repoRoot: REPO,
    histRows,
    eventName,
    eventYear,
    targetRound: Number.isFinite(targetRound) && targetRound >= 1 ? targetRound : 1,
    betTimeMs,
    fieldDgIds,
    courseName,
  });

  let n = 0;
  for (const p of players) {
    const dg = Math.round(num(p.dg_id, NaN));
    const mus = byDg.get(dg);
    if (!mus) continue;
    for (const [market, col] of Object.entries(FIELD_MAP)) {
      const mu = mus.get(market);
      if (!Number.isFinite(mu)) continue;
      p[col] = mu;
    }
    const muSg = mus.get("__mu_sg__");
    if (Number.isFinite(muSg)) {
      p.mu_sg = muSg;
      const par = num(p.course_par, 72);
      const total = mus.get("Total score");
      if (Number.isFinite(total)) p.score_to_par = Math.round((total - par) * 100) / 100;
    }
    p.projection_recipe = "dg_methodology";
    // Clear prior weather bake so the post-methodology weather pass starts from dg-μ baselines.
    delete p.weather_counts_baked;
    delete p._weather_bake_snapshot;
    delete p.weather_difficulty_delta;
    delete p._pre_weather_counts;
    n++;
  }

  proj.updated_at = new Date().toISOString();
  proj.projection_recipe = "dg_methodology";
  proj.projection_recipe_note =
    "DataGolf predictive methodology: seq⊕time SG decay, OTT>APP>ARG>PUTT reweight, shrunk course fit/history; Birdies BoB blend + Fairways driving-acc";
  delete proj.both_side_bias_applied;
  delete proj.projection_counts_weather_baked;
  delete proj.projection_counts_weather_baked_round;
  delete proj.projection_counts_weather_baked_at;
  writeFileSync(PROJ, `${JSON.stringify(proj)}\n`, "utf8");
  console.log(`[dg-mu] Updated ${n}/${players.length} players → ${PROJ}`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
