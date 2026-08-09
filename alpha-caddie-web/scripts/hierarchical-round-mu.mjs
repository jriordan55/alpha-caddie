/**
 * Bayesian hierarchical round model (empirical-Bayes / shrunk additive).
 *
 * Score(i,r) ~ Normal(μ(i,r), σ_i)
 * μ = player baseline + course effect + (skill × course traits)
 *     + tee-window weather + small recent-form update
 *
 * Birdies / Bogeys ~ NegBin(λ) with λ from the same μ stack (count scale).
 *
 * Builds on DG methodology effects (long-run shrunk baseline + course) then adds:
 *   - skill × course-table interactions (not raw small-n course finishes)
 *   - form as Bayesian pull toward recent mean (capped)
 *   - weather with explicit design features (wind, overnight precip, rain, temp)
 *
 *   GOLF_HIERARCHICAL_MU=1
 */
import { existsSync, readFileSync } from "fs";
import { join } from "path";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import { resolveCourseLayout } from "./course-hole-layout.mjs";
import {
  DG_MARKETS,
  effectsAtCutoff,
  predictDg,
  predictBirdiesBobLevel,
  predictBogeysLevel,
  predictFairwaysAccLevel,
  predictGirLevel,
  predictParsParMachine,
  predictScoreLevel,
  prefixBefore,
  typeHistRows,
  BIRDIE_BOB_BLEND,
  BOGEY_LEVEL_BLEND,
  FAIRWAY_ACC_BLEND,
  GIR_LEVEL_BLEND,
  PARS_PAR_MACHINE_BLEND,
  SCORE_LEVEL_BLEND,
} from "./dg-methodology-mu.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";
import {
  priorRainSoftDeltaFromMm,
  soakMuteWindFactor,
  weatherDifficultyDeltaFromSnapshot,
  WIND_EFFECT_FLOOR_MPH,
  WIND_STROKES_PER_MPH,
} from "./weather-mu-adjustments.mjs";
import { num } from "./round-projection-mu.mjs";

const N_FW = 14;

function numOr(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function shrink(n, k) {
  const nn = Math.max(0, numOr(n, 0));
  const kk = Math.max(1e-6, numOr(k, 1));
  return nn / (nn + kk);
}

export function hierarchicalMuEnabled() {
  const v = String(process.env.GOLF_HIERARCHICAL_MU || "1").trim().toLowerCase();
  return v !== "0" && v !== "false" && v !== "off" && v !== "no";
}

/** Default fit — overridden by data/hierarchical_round_mu_fit.json when present. */
export const DEFAULT_HIER_FIT = Object.freeze({
  version: 1,
  form: {
    window: 8,
    priorN: 12,
    /** Share of (recent − baseline) added to μ — small Bayesian update, not hot override. */
    weight: 0.14,
    maxAbsStp: 0.55,
  },
  /** Skill × course traits → score_to_par delta (positive = harder). */
  interactions: {
    ott_long: -0.22,
    ott_narrow: -0.28,
    app_firm_hold: -0.32,
    putt_demand: -0.18,
    maxAbsStp: 0.85,
  },
  /**
   * Weather design → STP (positive = harder). Overnight soft via
   * priorRainSoftDeltaFromMm + soak-muted wind (no soak floors / book shifts).
   */
  weather: {
    /** Locked design: +0.1 STP per mph over 5 (see WIND_STROKES_PER_MPH). */
    wind_per_mph_over_5: 0.1,
    rain_in_play: 0.1,
    storm_in_play: 0.16,
    temp_per_f_over_72: 0.02,
    humidity_per_pct_over_55: 0.004,
    afternoon_wave: 0.1,
    soak_mute_wind: true,
  },
  /** NegBin dispersion r (var = λ + λ²/r). Higher r → closer to Poisson. */
  negbin: {
    birdies_r: 8.5,
    bogeys_r: 7.0,
  },
  sigma: {
    score_floor: 2.6,
    score_ceil: 4.8,
    score_from_round_sd: true,
  },
});

export function loadHierarchicalFit(webRoot) {
  const path = join(webRoot, "data", "hierarchical_round_mu_fit.json");
  if (!existsSync(path)) return { ...DEFAULT_HIER_FIT, source: "defaults" };
  try {
    const raw = JSON.parse(readFileSync(path, "utf8"));
    return {
      ...DEFAULT_HIER_FIT,
      ...raw,
      form: { ...DEFAULT_HIER_FIT.form, ...(raw.form || {}) },
      interactions: { ...DEFAULT_HIER_FIT.interactions, ...(raw.interactions || {}) },
      weather: { ...DEFAULT_HIER_FIT.weather, ...(raw.weather || {}) },
      negbin: { ...DEFAULT_HIER_FIT.negbin, ...(raw.negbin || {}) },
      sigma: { ...DEFAULT_HIER_FIT.sigma, ...(raw.sigma || {}) },
      source: path,
    };
  } catch {
    return { ...DEFAULT_HIER_FIT, source: "defaults_parse_error" };
  }
}

export function loadCourseTraits(webRoot, courseName) {
  const path = join(webRoot, "course-table.json");
  if (!existsSync(path)) return null;
  try {
    const ct = JSON.parse(readFileSync(path, "utf8"));
    const key = normCourseNameKey(courseName);
    const row = ct?.byNormKey?.[key] || null;
    if (!row || typeof row !== "object") return null;
    const means = ct.means || {};
    const yard = numOr(row.yardage, NaN);
    const fw = numOr(row.fw_width, NaN);
    const gir = numOr(row.adj_gir, NaN);
    const putt = numOr(row.putt_sg, NaN);
    const miss = numOr(row.miss_fw_pen_frac, NaN);
    const yardMu = numOr(means.yardage, 7200);
    const yardSd = Math.max(80, numOr(means.yardage_sd, 350));
    const fwMu = numOr(means.fw_width, 32);
    const fwSd = Math.max(2, numOr(means.fw_width_sd, 5));
    return {
      yardage_z: Number.isFinite(yard) ? (yard - yardMu) / yardSd : 0,
      /** Positive = narrower than tour mean. */
      narrow_z: Number.isFinite(fw) ? (fwMu - fw) / fwSd : 0,
      /** Low GIR rate → harder to hold / smaller effective targets. */
      firm_hold_z: Number.isFinite(gir) ? (0.72 - gir) / 0.06 : 0,
      putt_demand_z: Number.isFinite(putt) ? -putt / 0.05 : 0,
      miss_fw_pen: Number.isFinite(miss) ? miss : 0.05,
      adj_stp: numOr(row.adj_score_to_par, 0),
      raw: row,
    };
  } catch {
    return null;
  }
}

/**
 * Skill × course traits — does NOT trust tiny player@course samples.
 * Positive return = higher expected score (harder).
 */
export function skillCourseInteractionStp(playerSkill, traits, fit = DEFAULT_HIER_FIT) {
  if (!playerSkill || !traits) return 0;
  const ix = fit.interactions || DEFAULT_HIER_FIT.interactions;
  const ott = numOr(playerSkill.ott, 0);
  const app = numOr(playerSkill.app, 0);
  const putt = numOr(playerSkill.putt, 0);
  // SG pillars: positive SG → lower score. Interactions scale that advantage by setup.
  let d = 0;
  d += ix.ott_long * ott * traits.yardage_z;
  d += ix.ott_narrow * ott * traits.narrow_z;
  d += ix.app_firm_hold * app * traits.firm_hold_z;
  d += ix.putt_demand * putt * traits.putt_demand_z;
  // Sign: ott/app/putt positive (good) × positive trait demand → negative STP via negative betas.
  return clamp(d, -ix.maxAbsStp, ix.maxAbsStp);
}

/** Recent form as shrunk update toward recent mean vs long-run level. */
export function recentFormStpUpdate(histPrefix, dg, market, baselineLevel, fit = DEFAULT_HIER_FIT) {
  const f = fit.form || DEFAULT_HIER_FIT.form;
  const win = Math.max(3, Math.round(f.window || 8));
  const vals = [];
  for (let i = (histPrefix || []).length - 1; i >= 0 && vals.length < win; i--) {
    const r = histPrefix[i];
    if (r.dg !== dg) continue;
    const v = r.vals?.[market];
    if (Number.isFinite(v)) vals.push(v);
  }
  if (vals.length < 3 || !Number.isFinite(baselineLevel)) return 0;
  const recent = vals.reduce((a, b) => a + b, 0) / vals.length;
  const w = shrink(vals.length, f.priorN) * numOr(f.weight, 0.14);
  const delta = w * (recent - baselineLevel);
  const cap = numOr(f.maxAbsStp, 0.55);
  // For score, delta is in strokes; for counts, same units as market.
  return clamp(delta, -cap, cap);
}

export function weatherFeaturesFromSnapshot(snap, wave = "") {
  const tempF = numOr(snap?.tempF ?? snap?.weather_temp_f, 72);
  const wind = numOr(snap?.windMph ?? snap?.weather_wind_mph, 8);
  const hum = numOr(snap?.humidityPct ?? snap?.weather_humidity, 55);
  const cond = String(snap?.condition ?? snap?.weather_condition ?? "default").toLowerCase();
  let priorMm = numOr(snap?.priorPrecipMm ?? snap?.weather_prior_precip_mm, NaN);
  if (!Number.isFinite(priorMm) && (snap?.priorRainSoft === true || snap?.weather_prior_rain_soft)) {
    priorMm = 3;
  }
  if (!Number.isFinite(priorMm)) priorMm = 0;
  const w = String(wave || "").toLowerCase();
  return {
    tempF,
    windMph: wind,
    humidityPct: hum,
    condition: cond,
    priorPrecipMm: priorMm,
    wind_excess: Math.max(0, wind - WIND_EFFECT_FLOOR_MPH),
    rain: cond === "rain" || cond === "storm" ? 1 : 0,
    storm: cond === "storm" ? 1 : 0,
    temp_dev: tempF - 72,
    humidity_dev: hum - 55,
    afternoon: w.includes("after") || w === "late" || w === "pm" ? 1 : 0,
  };
}

/** Hierarchical weather → STP (positive harder). Overnight soft is the process term. */
export function weatherLinearDelta(snap, wave = "", fit = DEFAULT_HIER_FIT) {
  const wcfg = fit.weather || DEFAULT_HIER_FIT.weather;
  const f = weatherFeaturesFromSnapshot(snap, wave);
  // Hard rule: +0.1 strokes per mph only after 5 mph (fit cannot override).
  let windTerm = WIND_STROKES_PER_MPH * f.wind_excess;
  if (wcfg.soak_mute_wind !== false) {
    windTerm *= soakMuteWindFactor(f.priorPrecipMm);
  }
  // In-play rain on already-soaked turf is not an extra hardness bump.
  let rainTerm = wcfg.rain_in_play * f.rain;
  let stormExtra = (wcfg.storm_in_play - wcfg.rain_in_play) * f.storm;
  if (f.priorPrecipMm >= 4) {
    rainTerm *= 0.35;
    stormExtra *= 0.35;
  }
  let d =
    windTerm +
    rainTerm +
    stormExtra +
    wcfg.temp_per_f_over_72 * f.temp_dev +
    wcfg.humidity_per_pct_over_55 * f.humidity_dev +
    wcfg.afternoon_wave * f.afternoon;

  d += priorRainSoftDeltaFromMm(f.priorPrecipMm);
  return clamp(d, -2.2, 2.6);
}

/**
 * Map STP delta → market μ (positive STP = harder scoring).
 * Soft/wet is already inside stpDelta via overnight precip — no additive overrides.
 */
export function weatherDeltaForMarket(market, stpDelta) {
  const d = numOr(stpDelta, 0);
  if (market === "Total score") return d;
  if (market === "Bogeys") return 0.45 * d;
  if (market === "Birdies") return -0.5 * d;
  if (market === "Pars") return 0.2 * d;
  if (market === "GIR") return -0.5 * d;
  if (market === "Fairways hit") return -0.4 * d;
  return 0;
}

/**
 * Direct skill × course effects on GIR / Fairways (count units), not STP-scaled.
 * Soft turf (prior precip) mildly amplifies approach→GIR and driving→FW hold.
 */
export function skillCourseInteractionCounts(playerSkill, traits, priorPrecipMm, fit = DEFAULT_HIER_FIT) {
  if (!playerSkill || !traits) return { gir: 0, fairways: 0 };
  const app = numOr(playerSkill.app, 0);
  const ott = numOr(playerSkill.ott, 0);
  const priorMm = Math.max(0, numOr(priorPrecipMm, 0));
  const soft = 1 + clamp(priorMm / 20, 0, 0.25);
  const gir = clamp((0.35 * app + 0.2 * app * Math.max(0, traits.firm_hold_z)) * soft, -0.4, 0.7);
  const fw = clamp(
    (0.3 * ott + 0.25 * ott * Math.max(0, traits.narrow_z) + 0.1 * Math.max(0, soft - 1) * Math.max(0, ott)) *
      soft,
    -0.35,
    0.65,
  );
  return { gir, fairways: fw };
}

/* ─── NegBin helpers ─────────────────────────────────────────────── */

export function negBinPmf(k, lambda, r) {
  const kk = Math.max(0, Math.round(k));
  const lam = Math.max(1e-6, numOr(lambda, 1e-6));
  const rr = Math.max(0.5, numOr(r, 8));
  // NB2: P(K=k) with mean λ, dispersion r (var = λ + λ²/r)
  const p = rr / (rr + lam);
  // Γ(k+r)/(k! Γ(r)) * p^r * (1-p)^k
  let logP = lnGamma(kk + rr) - lnGamma(kk + 1) - lnGamma(rr);
  logP += rr * Math.log(p) + kk * Math.log(1 - p);
  return Math.exp(clamp(logP, -700, 700));
}

function lnGamma(z) {
  // Lanczos approx for z > 0
  const g = 7;
  const c = [
    0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
    -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.984369654078563e-6,
    1.5056327351493116e-7,
  ];
  if (z < 0.5) {
    return Math.log(Math.PI / Math.sin(Math.PI * z)) - lnGamma(1 - z);
  }
  const x = z - 1;
  let a = c[0];
  for (let i = 1; i < g + 2; i++) a += c[i] / (x + i);
  const t = x + g + 0.5;
  return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
}

/** P(X > line) for half-lines (e.g. 4.5 → P(X >= 5)). */
export function negBinProbOver(line, lambda, r) {
  const L = numOr(line, NaN);
  const lam = numOr(lambda, NaN);
  if (!Number.isFinite(L) || !Number.isFinite(lam)) return NaN;
  const need = Math.floor(L) + 1; // clear over 4.5 → need >= 5
  let p = 0;
  const maxK = Math.max(need + 40, Math.ceil(lam + 8 * Math.sqrt(lam + 1)));
  for (let k = need; k <= maxK; k++) p += negBinPmf(k, lam, r);
  return clamp(p, 0, 1);
}

export function negBinProbUnder(line, lambda, r) {
  const over = negBinProbOver(line, lambda, r);
  if (!Number.isFinite(over)) return NaN;
  // Push — ignore exact integer mass for half-lines (standard O/U).
  return clamp(1 - over, 0, 1);
}

function clampMu(market, mu, par, fairwayHoles) {
  if (!Number.isFinite(mu)) return NaN;
  if (market === "Total score") return clamp(mu, (par || 72) - 12, (par || 72) + 12);
  if (market === "Fairways hit") return clamp(mu, 0, fairwayHoles || N_FW);
  if (market === "GIR") return clamp(mu, 0, 18);
  return clamp(mu, 0, 18);
}

/**
 * Build hierarchical μ map for a live/walk-forward event.
 * @returns {Promise<Map<number, Map<string, number>>>}
 */
export async function buildHierarchicalMuMapForEvent(opts) {
  const {
    repoRoot,
    histRows,
    eventName,
    eventYear,
    targetRound,
    betTimeMs,
    fieldDgIds,
    courseNameOverride = "",
    weatherByDg = null,
    waveByDgOverride = null,
  } = opts;

  const webRoot = join(repoRoot, "alpha-caddie-web");
  const fit = loadHierarchicalFit(webRoot);

  const dgSet = new Set((fieldDgIds || []).filter((d) => Number.isFinite(d)).map((d) => Math.round(d)));
  if (!dgSet.size) return { byDg: new Map(), meta: { fit_source: fit.source } };

  let courseName = String(courseNameOverride || "").trim();
  if (!courseName) {
    for (const row of histRows || []) {
      if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
      const yr = Math.round(num(row.year, NaN));
      if (Number.isFinite(eventYear) && yr !== eventYear) continue;
      const c = String(row.course_name || "").trim();
      if (c) {
        courseName = c;
        break;
      }
    }
  }
  const courseKey = normCourseNameKey(courseName);
  const traits = loadCourseTraits(webRoot, courseName);
  const layout = resolveCourseLayout({
    coursePar18: Number.isFinite(num(opts.coursePar18, NaN)) ? num(opts.coursePar18, 72) : 72,
    holePars: opts.holePars || null,
    courseUsed: courseName,
    eventName,
    webRoot,
  });
  const coursePar18 = Number.isFinite(num(opts.coursePar18, NaN))
    ? Math.round(num(opts.coursePar18, 70))
    : layout.course_par_18 || 70;
  // Prefer live projection basis FW holes (matches DK / venue averages) over par heuristic.
  const fairwayHoles = Math.round(num(opts.fairwayHoles, NaN)) > 0
    ? Math.round(num(opts.fairwayHoles, 14))
    : layout.fairway_holes_modeled || N_FW;

  const { resolveWalkforwardWeather } = await import("./historical-walkforward-projections.mjs");
  const fieldWeatherSnap = resolveWalkforwardWeather({
    webRoot,
    histRows,
    eventName,
    eventYear,
    targetRound,
  });

  const typed = typeHistRows(histRows, fairwayHoles);
  const prefix = prefixBefore(typed, betTimeMs, eventName, targetRound, eventYear);

  /** @type {Map<string, object>} */
  const effByMarket = new Map();
  for (const market of DG_MARKETS) {
    effByMarket.set(
      market,
      effectsAtCutoff(prefix, market, betTimeMs, eventName, eventYear, targetRound),
    );
  }

  /** @type {Map<number, string>} */
  const waveByDg = new Map(waveByDgOverride || []);
  if (!waveByDg.size) {
    for (const r of prefix) {
      if (!dgSet.has(r.dg)) continue;
      if (!eventsLikelySame(r.event, eventName)) continue;
      if (Number.isFinite(eventYear) && r.year !== eventYear) continue;
      if (r.wave) waveByDg.set(r.dg, r.wave);
    }
  }

  /** @type {Map<number, Map<string, number>>} */
  const byDg = new Map();
  /** @type {Map<number, object>} */
  const decompByDg = new Map();

  for (const dg of dgSet) {
    const mus = new Map();
    const wave = waveByDg.get(dg) || "";
    const wxSnap =
      (weatherByDg && weatherByDg.get(dg)) ||
      fieldWeatherSnap ||
      null;
    const scoreEff = effByMarket.get("Total score");
    const sk = scoreEff?.playerSkill?.get(dg);
    const ixStp = skillCourseInteractionStp(sk, traits, fit);
    const weatherStp = weatherLinearDelta(wxSnap, wave, fit);
    const priorMm = weatherFeaturesFromSnapshot(wxSnap, wave).priorPrecipMm;
    const ixCounts = skillCourseInteractionCounts(sk, traits, priorMm, fit);

    for (const market of DG_MARKETS) {
      const eff = effByMarket.get(market);
      // Baseline hierarchy WITHOUT DG's built-in weather (we own weather).
      let mu = predictDg(
        eff,
        {
          market,
          dg,
          courseKey,
          eventYear,
          round: targetRound,
          wave,
          startHole: NaN,
          par: coursePar18,
          weatherRow: null,
        },
        null,
        fairwayHoles,
      );

      if (market === "Birdies") {
        const bob = predictBirdiesBobLevel(prefix, dg, courseKey);
        if (Number.isFinite(bob)) {
          const a = clamp(BIRDIE_BOB_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * bob : bob;
        }
      } else if (market === "Fairways hit") {
        // Blend skill residual with driving-acc level (don't 100% overwrite — keeps OTT skill).
        const acc = predictFairwaysAccLevel(prefix, dg, courseKey);
        if (Number.isFinite(acc)) {
          const a = clamp(Math.min(FAIRWAY_ACC_BLEND, 0.65), 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * acc : acc;
        }
      } else if (market === "GIR") {
        const lvl = predictGirLevel(prefix, dg, courseKey);
        if (Number.isFinite(lvl)) {
          const a = clamp(Math.min(GIR_LEVEL_BLEND, 0.65), 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * lvl : lvl;
        }
      } else if (market === "Bogeys") {
        const lvl = predictBogeysLevel(prefix, dg, courseKey);
        if (Number.isFinite(lvl)) {
          const a = clamp(BOGEY_LEVEL_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * lvl : lvl;
        }
      } else if (market === "Pars") {
        const pm = predictParsParMachine(prefix, dg, courseKey);
        if (Number.isFinite(pm) && PARS_PAR_MACHINE_BLEND > 0) {
          const a = clamp(PARS_PAR_MACHINE_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * pm : pm;
        }
      } else if (market === "Total score") {
        const lvl = predictScoreLevel(prefix, dg, courseKey);
        if (Number.isFinite(lvl)) {
          const a = clamp(SCORE_LEVEL_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * lvl : lvl;
        }
      }

      const formDelta = recentFormStpUpdate(prefix, dg, market, mu, fit);
      mu += formDelta;
      mu += weatherDeltaForMarket(market, ixStp);
      mu += weatherDeltaForMarket(market, weatherStp);
      if (market === "GIR") mu += ixCounts.gir;
      if (market === "Fairways hit") mu += ixCounts.fairways;
      mu = clampMu(market, mu, coursePar18, fairwayHoles);
      if (Number.isFinite(mu)) mus.set(market, Math.round(mu * 1000) / 1000);
    }

    // Pars identity soft blend toward 18 − bird − bog
    const bird = mus.get("Birdies");
    const bog = mus.get("Bogeys");
    const pars = mus.get("Pars");
    if (Number.isFinite(bird) && Number.isFinite(bog)) {
      const ident = clamp(18 - bird - bog, 0, 18);
      if (Number.isFinite(pars)) mus.set("Pars", Math.round((0.65 * pars + 0.35 * ident) * 1000) / 1000);
      else mus.set("Pars", Math.round(ident * 1000) / 1000);
    }

    const total = mus.get("Total score");
    if (Number.isFinite(total)) {
      mus.set("__mu_sg__", Math.round((coursePar18 - total) * 1000) / 1000);
    }
    mus.set("__weather_stp__", Math.round(weatherStp * 1000) / 1000);
    mus.set("__interaction_stp__", Math.round(ixStp * 1000) / 1000);

    byDg.set(dg, mus);
    decompByDg.set(dg, {
      weather_stp: weatherStp,
      interaction_stp: ixStp,
      wave,
      prior_precip_mm: weatherFeaturesFromSnapshot(wxSnap, wave).priorPrecipMm,
    });
  }

  return {
    byDg,
    decompByDg,
    meta: {
      model: "hierarchical_round_mu",
      fit_source: fit.source,
      course_key: courseKey,
      course_par_18: coursePar18,
      fairway_holes: fairwayHoles,
      traits: traits
        ? {
            yardage_z: traits.yardage_z,
            narrow_z: traits.narrow_z,
            firm_hold_z: traits.firm_hold_z,
            putt_demand_z: traits.putt_demand_z,
          }
        : null,
      negbin: fit.negbin,
    },
  };
}

/** Convenience: NegBin clear probs for a player μ map. */
export function hierarchicalCountPropProbs(mus, fit = DEFAULT_HIER_FIT) {
  const nb = fit.negbin || DEFAULT_HIER_FIT.negbin;
  return {
    birdies_lambda: mus.get("Birdies"),
    bogeys_lambda: mus.get("Bogeys"),
    birdies_r: nb.birdies_r,
    bogeys_r: nb.bogeys_r,
    probOverBirdies: (line) => negBinProbOver(line, mus.get("Birdies"), nb.birdies_r),
    probOverBogeys: (line) => negBinProbOver(line, mus.get("Bogeys"), nb.bogeys_r),
  };
}

export { weatherDifficultyDeltaFromSnapshot };
