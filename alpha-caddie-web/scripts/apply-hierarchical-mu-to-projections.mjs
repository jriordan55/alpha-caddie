#!/usr/bin/env node
/**
 * Apply hierarchical round μ (baseline + skill×course + form + weather + NegBin λ)
 * onto projections.json for display_round.
 *
 * Owns weather in the μ — sets weather_counts_baked so a later bake:weather
 * re-applies from baselines cleanly (or skip bake when GOLF_HIER_OWNS_WEATHER=1).
 *
 *   node scripts/apply-hierarchical-mu-to-projections.mjs
 *   npm run apply:hierarchical-mu
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import {
  buildHierarchicalMuMapForEvent,
  hierarchicalMuEnabled,
  loadHierarchicalFit,
} from "./hierarchical-round-mu.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";
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

function weatherSnapFromPlayer(p) {
  const auto = p?.dg_auto_weather;
  if (auto && typeof auto === "object" && Number.isFinite(Number(auto.tempF))) {
    return {
      tempF: auto.tempF,
      windMph: auto.windMph,
      humidityPct: auto.humidityPct,
      condition: auto.condition,
      priorPrecipMm: auto.priorPrecipMm,
      priorRainSoft: auto.priorRainSoft,
    };
  }
  if (!Number.isFinite(Number(p?.weather_temp_f))) return null;
  return {
    tempF: p.weather_temp_f,
    windMph: p.weather_wind_mph,
    humidityPct: p.weather_humidity,
    condition: p.weather_condition,
    priorPrecipMm: p.weather_prior_precip_mm,
    priorRainSoft: p.weather_prior_rain_soft,
  };
}

async function main() {
  if (!hierarchicalMuEnabled()) {
    console.log("[hier-mu] GOLF_HIERARCHICAL_MU off — skip");
    return;
  }
  if (!existsSync(PROJ)) throw new Error(`Missing ${PROJ}`);

  const proj = JSON.parse(readFileSync(PROJ, "utf8"));
  const players = Array.isArray(proj.players) ? proj.players : [];
  if (!players.length) throw new Error("projections.json has no players");

  const eventName = String(proj.event_name || proj.meta?.event_name || "").trim();
  const courseName = String(proj.course_used || proj.course_name || "").trim();
  const targetRound = Math.round(
    num(proj.display_round ?? proj.datagolf_field_current_round ?? proj.meta?.round, 1),
  );
  const eventYear = Math.round(
    num(String(proj.datagolf_field_date_start || "").slice(0, 4), new Date().getFullYear()),
  );
  const betTimeMs = Date.parse(proj.updated_at || "") || Date.now();
  const fieldDgIds = players.map((p) => Math.round(num(p.dg_id, NaN))).filter(Number.isFinite);

  /** @type {Map<number, object>} */
  const weatherByDg = new Map();
  /** @type {Map<number, string>} */
  const waveByDg = new Map();
  for (const p of players) {
    if (Math.round(num(p.round, NaN)) !== targetRound) continue;
    const dg = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const snap = weatherSnapFromPlayer(p);
    if (snap) weatherByDg.set(dg, snap);
    const wave = teeWaveFromTeetimeAndLabel(p.dg_teetime_local, p.dg_tee_wave);
    if (wave) waveByDg.set(dg, wave);
  }

  // Ensure tee-window weather (incl. overnight precip) exists before hierarchical apply.
  if (weatherByDg.size < Math.min(20, fieldDgIds.length * 0.3)) {
    try {
      const { bakeOpenMeteoWeatherIntoProjections } = await import("./open-meteo-forecast.mjs");
      const livePath = join(WEB, "live-in-play.json");
      let fieldUpdates = null;
      if (existsSync(livePath)) {
        const live = JSON.parse(readFileSync(livePath, "utf8"));
        fieldUpdates = live?.field_updates || null;
      }
      await bakeOpenMeteoWeatherIntoProjections(proj, {
        fieldUpdates,
        skipFieldCalibrate: true,
      });
      // Re-collect snaps after bake (counts not yet hierarchical).
      for (const p of players) {
        if (Math.round(num(p.round, NaN)) !== targetRound) continue;
        const dg = Math.round(num(p.dg_id, NaN));
        if (!Number.isFinite(dg)) continue;
        const snap = weatherSnapFromPlayer(p);
        if (snap) weatherByDg.set(dg, snap);
        const wave = teeWaveFromTeetimeAndLabel(p.dg_teetime_local, p.dg_tee_wave);
        if (wave) waveByDg.set(dg, wave);
      }
      console.log(`[hier-mu] Open-Meteo tee weather ready (${weatherByDg.size} players)`);
    } catch (e) {
      console.warn("[hier-mu] weather pre-bake failed:", e?.message || e);
    }
  }

  console.log(
    `[hier-mu] Building hierarchical μ for ${eventName} R${targetRound} · ${fieldDgIds.length} players…`,
  );
  const histRows = await loadHistRows();
  const { byDg, decompByDg, meta } = await buildHierarchicalMuMapForEvent({
    repoRoot: REPO,
    histRows,
    eventName,
    eventYear,
    targetRound: Number.isFinite(targetRound) && targetRound >= 1 ? targetRound : 1,
    betTimeMs,
    fieldDgIds,
    courseNameOverride: courseName,
    coursePar18: Math.round(num(proj.course_par_18, 70)) || 70,
    fairwayHoles:
      Math.round(num(proj.projection_course_basis?.fairway_holes_modeled ?? proj.meta?.projection_course_basis?.fairway_holes_modeled, NaN)) ||
      14,
    holePars: proj.hole_pars || null,
    weatherByDg,
    waveByDgOverride: waveByDg,
  });

  const fit = loadHierarchicalFit(WEB);
  const par = Math.round(num(proj.course_par_18, meta.course_par_18 || 70)) || 70;
  let n = 0;
  for (const p of players) {
    const dg = Math.round(num(p.dg_id, NaN));
    const rnd = Math.round(num(p.round, NaN));
    if (!Number.isFinite(dg) || rnd !== targetRound) continue;
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
      p.implied_mu_sg = muSg;
      p.sg_total = muSg;
    }
    if (Number.isFinite(Number(p.total_score))) {
      p.score_to_par = Math.round((Number(p.total_score) - par) * 100) / 100;
    }

    const decomp = decompByDg.get(dg);
    if (decomp) {
      p.hierarchical_weather_stp = decomp.weather_stp;
      p.hierarchical_interaction_stp = decomp.interaction_stp;
    }
    p.projection_recipe = "hierarchical_mu";
    p.negbin_birdies_r = fit.negbin?.birdies_r;
    p.negbin_bogeys_r = fit.negbin?.bogeys_r;

    // Mark weather as owned by hierarchical μ (UI incremental weather adj → 0).
    const snap = weatherSnapFromPlayer(p);
    if (snap) {
      p._weather_bake_snapshot = { ...snap };
      p.weather_difficulty_delta = Number(mus.get("__weather_stp__")) || 0;
      p.weather_counts_baked = true;
      p._pre_weather_counts = {
        total_score: p.total_score,
        score_to_par: p.score_to_par,
        birdies: p.birdies,
        pars: p.pars,
        bogeys: p.bogeys,
        gir: p.gir,
        fairways: p.fairways,
        putts: p.putts,
        mu_sg: p.mu_sg,
        implied_mu_sg: p.implied_mu_sg,
        round_sd: p.round_sd,
      };
    }
    n++;
  }

  proj.updated_at = new Date().toISOString();
  proj.projection_recipe = "hierarchical_mu";
  proj.projection_recipe_note =
    "Hierarchical μ: shrunk player/course baseline + skill×course traits + form update + tee-window/overnight weather; Birdies/Bogeys NegBin λ";
  proj.hierarchical_mu = {
    ...meta,
    applied_at: proj.updated_at,
    n_players: n,
    owns_weather: true,
  };
  proj.projection_counts_weather_baked = n > 0;
  proj.projection_counts_weather_baked_round = targetRound;
  proj.projection_counts_weather_baked_at = proj.updated_at;
  if (!proj.meta || typeof proj.meta !== "object") proj.meta = {};
  proj.meta.projection_counts_weather_baked = n > 0;
  proj.meta.projection_counts_weather_baked_round = targetRound;
  proj.meta.projection_counts_weather_baked_at = proj.updated_at;
  proj.meta.hierarchical_mu = proj.hierarchical_mu;
  proj.meta.projection_recipe = "hierarchical_mu";
  delete proj.both_side_bias_applied;

  writeFileSync(PROJ, `${JSON.stringify(proj, null, 2)}\n`, "utf8");
  console.log(`[hier-mu] Updated ${n}/${players.length} display-round players → ${PROJ}`);
  if (meta.traits) console.log(`[hier-mu] course traits`, meta.traits);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
