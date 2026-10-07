/**
 * Live round μ: Bayesian hierarchical model.
 * Score ~ Normal(baseline + course + skill×traits + tee-window weather + form).
 * Birdies, bogeys, and pars ~ NegBin with the same inputs.
 *
 * Wind is mean sustained mph over the tee window (archive windMph). Gusts are not blended.
 * Putts are left unchanged (historical putts are NA).
 *
 *   node scripts/apply-bayesian-round-mu.mjs
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath, pathToFileURL } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { normalizeTourCode, tourDisplayLabel } from "./golf-tours.mjs";
import { resolveProjectionPaths } from "./projection-paths.mjs";
import { bakeOpenMeteoWeatherIntoProjections } from "./open-meteo-forecast.mjs";
import { hierarchicalMuEnabled } from "./hierarchical-round-mu.mjs";
import {
  fitRoundForward,
  loadCourseMoments,
  loadRounds,
  projectPlayer,
  weatherDesign,
  yokohamaTraits,
} from "./bayesian-round-model.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const HIST = join(REPO, "data", "historical_rounds_all.csv");
const WEATHER = join(WEB, "data", "historical_round_weather.json");

function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

function r3(x) {
  return Number.isFinite(x) ? Math.round(x * 1000) / 1000 : null;
}

function fairwayHoles(proj) {
  const basis = proj.projection_course_basis || proj.meta?.projection_course_basis || {};
  const fromBasis = Math.round(num(basis.fairway_holes_modeled, NaN));
  if (fromBasis >= 10 && fromBasis <= 18) return fromBasis;
  const pars = Array.isArray(proj.hole_pars) ? proj.hole_pars : [];
  const n = pars.filter((p) => Math.round(num(p, 0)) >= 4).length;
  return n >= 10 && n <= 18 ? n : 14;
}

function weatherSnapFromPlayer(p) {
  const auto = p?.dg_auto_weather;
  if (auto && typeof auto === "object" && Number.isFinite(num(auto.windMph, NaN))) {
    return auto;
  }
  if (!Number.isFinite(num(p?.weather_wind_mph, NaN))) return null;
  return {
    windMph: p.weather_wind_mph,
    tempF: p.weather_temp_f,
    humidityPct: p.weather_humidity,
    condition: p.weather_condition,
    precipMm: p.weather_precip_mm,
  };
}

function traitsForCourse(ck, byKey, moments) {
  if (ck === "yokohama country club") {
    const traits = yokohamaTraits(moments);
    byKey.set(ck, traits);
    return traits;
  }
  return byKey.get(ck) || { yardage_z: 0, narrow_z: 0, firm_hold_z: 0, putt_demand_z: 0 };
}

export async function applyBayesianRoundMu(opts = {}) {
  if (!hierarchicalMuEnabled()) {
    console.log("[bayes-mu] GOLF_HIERARCHICAL_MU off — skip");
    return 0;
  }
  const paths = resolveProjectionPaths(WEB);
  const projPath = opts.projectionsPath || paths.projectionsPath;
  const livePath = opts.liveInPlayPath || paths.liveInPlayPath;
  if (!existsSync(projPath)) throw new Error(`Missing ${projPath}`);
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);

  const proj = JSON.parse(readFileSync(projPath, "utf8"));
  const players = Array.isArray(proj.players) ? proj.players : [];
  const targetRound = Math.round(
    num(proj.display_round ?? proj.datagolf_field_current_round ?? proj.meta?.round, 1),
  );
  const tour = normalizeTourCode(proj.datagolf_feed_tour || proj.meta?.datagolf_feed_tour || "pga");
  const courseName = String(proj.course_used || proj.course_name || "").trim();
  const ck = normCourseNameKey(courseName);
  const par = Math.round(num(proj.course_par_18, 72)) || 72;
  const fwHoles = fairwayHoles(proj);

  console.log("[bayes-mu] loading history…");
  const wxFile = existsSync(WEATHER) ? JSON.parse(readFileSync(WEATHER, "utf8")) : { byKey: {} };
  const rounds = await loadRounds(HIST, wxFile.byKey || {}, { tours: ["pga", "euro"] });
  const { byKey, moments } = loadCourseMoments(WEB);
  const traits = traitsForCourse(ck, byKey, moments);
  const courseN = rounds.filter((r) => r.ck === ck).length;
  console.log(
    `[bayes-mu] ${tourDisplayLabel(tour)} event · ${rounds.length} PGA+DP rounds in the fit · course "${ck}" · ${courseN} historical rounds · yardage_z=${num(traits.yardage_z, 0).toFixed(2)}`,
  );
  const fit = fitRoundForward(rounds, byKey, { holdoutEvents: 12 });
  const courseEff = fit.coef.course?.score?.get(ck);
  console.log(
    `[bayes-mu] score course effect ${Number.isFinite(courseEff) ? courseEff.toFixed(3) : "0 (no history)"}`,
  );

  let fieldUpdates = null;
  if (existsSync(livePath)) {
    try {
      fieldUpdates = JSON.parse(readFileSync(livePath, "utf8"))?.field_updates || null;
    } catch {
      fieldUpdates = null;
    }
  }
  const baked = await bakeOpenMeteoWeatherIntoProjections(proj, {
    fieldUpdates,
    skipFieldCalibrate: true,
    preserveBaselines: true,
  });
  console.log(
    `[bayes-mu] tee weather ${baked.status} · ${baked.playersWithWeather} players · ${baked.teeMatches} tee matches`,
  );

  const birdR = fit.dispersion.birdies?.r;
  const bogR = fit.dispersion.bogeys?.r;
  const winds = [];
  let n = 0;
  let nWx = 0;
  for (const p of players) {
    if (Math.round(num(p.round, NaN)) !== targetRound) continue;
    const dg = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const snap = weatherSnapFromPlayer(p);
    const wx = weatherDesign(
      snap
        ? {
            windMph: snap.windMph,
            tempF: snap.tempF,
            humidityPct: snap.humidityPct,
            condition: snap.condition,
            precipMm: snap.precipMm ?? snap.rainMm,
          }
        : null,
    );
    if (snap && Number.isFinite(num(snap.windMph, NaN))) {
      winds.push(num(snap.windMph, NaN));
      nWx++;
    }
    const pred = projectPlayer(fit, {
      dg,
      traits,
      wx,
      ck,
      par,
      league: tour,
      projSkill: {
        ott: num(p.sg_ott, 0),
        app: num(p.sg_app, 0),
        arg: num(p.sg_arg, 0),
        putt: num(p.sg_putt, 0),
      },
    });
    if (!Number.isFinite(pred.score?.mu)) continue;
    const fwScale = fwHoles / 14;
    p.total_score = r3(pred.score.mu);
    p.score_to_par = r3(pred.score.mu - par);
    const sg = r3(par - pred.score.mu);
    p.mu_sg = sg;
    p.implied_mu_sg = sg;
    p.sg_total = sg;
    p.birdies = r3(pred.birdies.mu);
    p.bogeys = r3(pred.bogeys.mu);
    p.pars = r3(pred.pars.mu);
    p.gir = r3(pred.gir.mu);
    p.fairways = r3(pred.fairways.mu * fwScale);
    if (Number.isFinite(pred.score.sigma)) p.round_sd = r3(pred.score.sigma);
    p.projection_recipe = "hierarchical_mu";
    p.score_source = "bayesian_hierarchical_round";
    p.hierarchical_weather_stp = r3(pred.score.weather);
    p.hierarchical_interaction_stp = r3(pred.score.ix);
    p.negbin_birdies_r = birdR;
    p.negbin_bogeys_r = bogR;
    p.weather_counts_baked = true;
    p.weather_difficulty_delta = r3(pred.score.weather);
    p._weather_bake_snapshot = snap
      ? {
          tempF: snap.tempF,
          windMph: snap.windMph,
          humidityPct: snap.humidityPct,
          condition: snap.condition,
          priorPrecipMm: snap.priorPrecipMm,
        }
      : p._weather_bake_snapshot;
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
    n++;
  }

  proj.updated_at = new Date().toISOString();
  proj.projection_recipe = "hierarchical_mu";
  proj.projection_recipe_note =
    "Bayesian hierarchical round model: shrunk baseline + course + skill×course traits + tee-window sustained wind + small form update. Birdies/Bogeys/Pars are NegBin. Putts omitted.";
  proj.hierarchical_mu = {
    model: "bayesian_hierarchical_round",
    applied_at: proj.updated_at,
    n_players: n,
    owns_weather: true,
    tour,
    course_key: ck,
    course_rounds: courseN,
    course_effect_score: Number.isFinite(courseEff) ? r3(courseEff) : 0,
    fairway_holes: fwHoles,
    wind_feature: "mean sustained mph",
    wind_mph: winds.length
      ? [r3(Math.min(...winds)), r3(Math.max(...winds))]
      : null,
    negbin: { birdies_r: birdR, bogeys_r: bogR },
  };
  proj.projection_counts_weather_baked = n > 0;
  proj.projection_counts_weather_baked_round = targetRound;
  proj.projection_counts_weather_baked_at = proj.updated_at;
  if (!proj.meta || typeof proj.meta !== "object") proj.meta = {};
  proj.meta.projection_recipe = "hierarchical_mu";
  proj.meta.hierarchical_mu = proj.hierarchical_mu;
  proj.meta.projection_counts_weather_baked = n > 0;
  proj.meta.projection_counts_weather_baked_round = targetRound;
  proj.meta.projection_counts_weather_baked_at = proj.updated_at;
  delete proj.both_side_bias_applied;

  writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`, "utf8");
  const scores = players
    .filter((p) => Math.round(num(p.round, NaN)) === targetRound && Number.isFinite(num(p.total_score, NaN)))
    .map((p) => num(p.total_score, NaN));
  const mean = scores.length ? scores.reduce((a, b) => a + b, 0) / scores.length : NaN;
  console.log(
    `[bayes-mu] R${targetRound} ${n} players · weather on ${nWx} · field score ${Number.isFinite(mean) ? mean.toFixed(2) : "n/a"} · wind ${winds.length ? `${Math.min(...winds).toFixed(1)}–${Math.max(...winds).toFixed(1)} mph` : "none"} → ${projPath}`,
  );
  return n;
}

const isDirect = process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href;
if (isDirect) {
  applyBayesianRoundMu().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
