/**
 * Counting markets from player rolling rates + SG categories (no score-to-par derive).
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import { parse } from "csv-parse";
import {
  DG_TOUR_AVG_FAIRWAY_RATE,
  DG_TOUR_AVG_GIR_RATE,
  fairwayRate01FromDg,
  fairwayRate01FromSkillRatingsPp,
  girHitsFromRate01,
  girRate01FromDg,
  girRate01FromSgApp,
  traditionalRate01,
} from "./dg-traditional-stats.mjs";
import {
  blendWeightsFromHistCalib,
  optimizedGirCount,
  optimizedFairwayCount,
  optimizedHoleCounts,
  applyT2gPuttParShape,
} from "./optimized-counting-blend.mjs";
import { venueBirdieSgScale } from "./projection-stat-model.mjs";

/** Player FW spread vs tour when course anchor is off (near 1 = pure driving accuracy). */
const FAIRWAY_DRIVING_ACC_SPREAD = 0.94;

/** Backtested BoB model constants (DK Birdies market = birdies + eagles). */
export const BIRDIE_BOB_WINDOW = 50;
export const BIRDIE_COURSE_SPREAD_KEEP = 0.42;
export const BIRDIE_PLAYER_COURSE_MIN_ROUNDS = 4;
export const BIRDIE_PLAYER_COURSE_MAX_WEIGHT = 0.45;

/**
 * Dynamic spread: preserve separation for players clearly above *or* below course BoB.
 * Thin/stale unknowns were getting pulled to venue mean (keep≈0.42) and looking like birdie machines.
 */
export function birdieCourseSpreadKeep(playerMkt, venueMkt, baseSpread = BIRDIE_COURSE_SPREAD_KEEP) {
  let spread = num(baseSpread, BIRDIE_COURSE_SPREAD_KEEP);
  if (!Number.isFinite(playerMkt) || !Number.isFinite(venueMkt)) return spread;
  if (playerMkt > venueMkt) {
    const excess = playerMkt - venueMkt;
    spread = clamp(spread + 0.4 * excess + 0.04 * Math.max(0, playerMkt), spread, 0.9);
  } else if (playerMkt < venueMkt) {
    const deficit = venueMkt - playerMkt;
    spread = clamp(spread + 0.32 * deficit + 0.03 * Math.max(0, venueMkt - playerMkt), spread, 0.88);
  }
  return spread;
}

/** Local numeric parse with fallback (dg-traditional-stats `num` has no fallback arg). */
function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

/** Empirical DK-audit bias trim (model − actual); 0 = off. */
const BIRDIE_ACTUAL_BIAS_TRIM = num(process.env.GOLF_BIRDIE_ACTUAL_BIAS_TRIM, 0);

function birdiesFromHistRow(row) {
  const b = num(row.birdies, NaN);
  const e = num(row.eagles_or_better ?? row.eagles, 0);
  if (!Number.isFinite(b)) return NaN;
  return b + (Number.isFinite(e) ? Math.max(0, e) : 0);
}

/** Bogey-or-worse count (bogeys + doubles) — matches DK / PP Bogeys market. */
function bogeysFromHistRow(row) {
  const bg = num(row.bogeys ?? row.bogies, NaN);
  const d = num(row.doubles_or_worse ?? row.doubles, 0);
  if (!Number.isFinite(bg)) return NaN;
  return bg + (Number.isFinite(d) ? Math.max(0, d) : 0);
}

function countFromRateOrRaw(raw, nHoles) {
  const r = traditionalRate01(raw, nHoles);
  if (!Number.isFinite(r)) return NaN;
  return r * nHoles;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function sgDelta(sk, field, key) {
  const v = num(sk?.[key], NaN);
  const f = num(field?.[key], NaN);
  if (!Number.isFinite(v) || !Number.isFinite(f)) return 0;
  return v - f;
}

function histBlend(histVal, venueVal, nHist, venueWeight = 12) {
  const w = nHist > 0 ? Math.min(0.85, nHist / (nHist + venueWeight)) : 0;
  if (Number.isFinite(histVal)) return w * histVal + (1 - w) * venueVal;
  return venueVal;
}

/** Blend career hole-count mean toward skill path without washing out field spread. */
function histBlendSkillFirst(histVal, skillVal, nHist, maxHistWeight = 0.32) {
  if (!Number.isFinite(histVal)) return skillVal;
  const w = Math.min(maxHistWeight, nHist > 0 ? nHist / (nHist + 28) : 0);
  return w * histVal + (1 - w) * skillVal;
}

/** Keep player−course skill gap; anchor mean at course layout rate. */
function skillSpreadRate01(playerRate01, anchorRate01, spreadKeep = 0.85) {
  const p = num(playerRate01, NaN);
  const a = num(anchorRate01, NaN);
  if (!Number.isFinite(p) || !Number.isFinite(a)) return p;
  const k = num(spreadKeep, 0.72);
  return a + k * (p - a);
}

/** Course-heavy blend of layout table rate and venue historical mean. */
function courseVenueCountTarget(courseCount, venueCount, courseWeight = 0.82) {
  if (Number.isFinite(courseCount) && Number.isFinite(venueCount)) {
    const w = clamp(num(courseWeight, 0.82), 0.2, 0.92);
    return Math.round((w * courseCount + (1 - w) * venueCount) * 100) / 100;
  }
  if (Number.isFinite(courseCount)) return Math.round(courseCount * 100) / 100;
  return venueCount;
}

/** Field-mean calibration target from course-table driving accuracy. */
export function fairwayCalibrationTargetMean(opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const venue = num(opts.venueAvgFairways, NaN);
  const courseCount = num(opts.courseFairwayRate01, NaN) * nFw;
  return courseVenueCountTarget(courseCount, venue, num(opts.courseWeight, 0.5));
}

/** Shift all fairways by constant so field mean hits target (spread unchanged). */
export function calibrateFairwayFieldMean(players, opts = {}) {
  const round = opts.round;
  const target = fairwayCalibrationTargetMean(opts);
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const cap = nFw + 0.5;
  const minDelta = num(opts.minDelta, 0.04);
  if (!Number.isFinite(target)) return 0;

  const rows = (players || []).filter((p) => {
    if (Number.isFinite(round) && Math.round(num(p.round, NaN)) !== round) return false;
    return Number.isFinite(num(p.fairways, NaN));
  });
  if (rows.length < 8) return 0;

  const mean = rows.reduce((s, p) => s + num(p.fairways, 0), 0) / rows.length;
  const delta = mean - target;
  if (Math.abs(delta) < minDelta) return 0;

  for (const p of rows) {
    const fw = num(p.fairways, NaN);
    if (!Number.isFinite(fw)) continue;
    p.fairways = Math.round(clamp(fw - delta, 2, cap) * 100) / 100;
  }
  return rows.length;
}

/** Re-derive fairways from driving accuracy / SG:OTT (fixes stale dg_fairway_pct on live rows). */
export function refreshFairwaysFromDrivingAccuracy(players, opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  /** @type {Map<number, number[]>} */
  const ottByRound = new Map();
  for (const row of players || []) {
    const rnd = Math.round(num(row.round, NaN));
    const ott = num(row.sg_ott, NaN);
    if (!Number.isFinite(rnd) || !Number.isFinite(ott)) continue;
    if (!ottByRound.has(rnd)) ottByRound.set(rnd, []);
    ottByRound.get(rnd).push(ott);
  }
  const fieldMedianOtt = (rnd) => {
    const vals = ottByRound.get(rnd);
    if (!vals?.length) return num(opts.fieldMeans?.sg_ott, 0);
    vals.sort((a, b) => a - b);
    const mid = Math.floor(vals.length / 2);
    return vals.length % 2 === 1 ? vals[mid] : (vals[mid - 1] + vals[mid]) / 2;
  };
  let n = 0;
  for (const row of players || []) {
    if (!row || typeof row !== "object") continue;
    const rnd = Math.round(num(row.round, NaN));
    const dgPct = num(row.dg_fairway_pct, NaN);
    const drvAcc = num(row.driving_accuracy, NaN);
    const circularDrv =
      Number.isFinite(dgPct) &&
      dgPct >= 0.15 &&
      Number.isFinite(drvAcc) &&
      Math.abs(drvAcc / 100 - dgPct) < 0.012;
    const dgValid = Number.isFinite(dgPct) && dgPct >= 0.15 && dgPct <= 0.88 && !circularDrv;
    const skRow = {
      sg_ott: row.sg_ott,
      sg_app: row.sg_app,
      sg_putt: row.sg_putt,
      sg_arg: row.sg_arg,
      sg_total: row.sg_total,
      driving_acc: row.driving_acc,
      driving_accuracy: circularDrv || dgValid ? NaN : row.driving_accuracy,
      avg_fairways: row.avg_fairways,
    };
    if (dgValid) skRow.dg_fairway_pct = dgPct;
    const fw = fairwaysFromDrivingAccuracyAndCourse({
      skRow,
      muSg: num(row.mu_sg, num(row.implied_mu_sg, 0)),
      nFairwayHoles: nFw,
      fieldMeans: { sg_ott: fieldMedianOtt(rnd) },
    });
    if (!Number.isFinite(fw)) continue;
    const prev = num(row.fairways, NaN);
    row.fairways = fw;
    const rate01 = clamp(fw / nFw, 0.28, 0.88);
    row.dg_fairway_pct = Math.round(rate01 * 1000) / 1000;
    row.driving_accuracy = Math.round(rate01 * 1000) / 10;
    if (!Number.isFinite(prev) || Math.abs(prev - fw) > 0.04) n++;
  }
  return n;
}

/** Shrink player fairway spread toward field mean (weak cross-player signal). */
export function shrinkFairwayFieldSpread(players, opts = {}) {
  const round = opts.round;
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const cap = nFw + 0.5;
  const shrink = clamp(num(opts.shrink, 0.18), 0, 0.4);
  const rows = (players || []).filter((p) => {
    if (Number.isFinite(round) && Math.round(num(p.round, NaN)) !== round) return false;
    return Number.isFinite(num(p.fairways, NaN));
  });
  if (rows.length < 8 || shrink <= 0) return 0;
  const mean = rows.reduce((s, p) => s + num(p.fairways, 0), 0) / rows.length;
  for (const p of rows) {
    const fw = num(p.fairways, NaN);
    if (!Number.isFinite(fw)) continue;
    p.fairways = Math.round(clamp(mean + (1 - shrink) * (fw - mean), 2, cap) * 100) / 100;
  }
  return rows.length;
}

/** Field-mean GIR target from course-table GIR rate blended with venue. */
export function girCalibrationTargetMean(opts = {}) {
  const nGir = Math.round(num(opts.nGirHoles, 18)) || 18;
  const venue = num(opts.venueAvgGir, NaN);
  const courseCount = num(opts.courseGirRate01, NaN) * nGir;
  return courseVenueCountTarget(courseCount, venue, num(opts.courseWeight, 0.84));
}

/** Shift all GIR by constant so field mean hits course+venue target (spread unchanged). */
export function calibrateGirFieldMean(players, opts = {}) {
  const round = opts.round;
  const target = girCalibrationTargetMean(opts);
  const nGir = Math.round(num(opts.nGirHoles, 18)) || 18;
  const cap = nGir + 0.5;
  const minDelta = num(opts.minDelta, 0.04);
  if (!Number.isFinite(target)) return 0;

  const rows = (players || []).filter((p) => {
    if (Number.isFinite(round) && Math.round(num(p.round, NaN)) !== round) return false;
    return Number.isFinite(num(p.gir, NaN));
  });
  if (rows.length < 8) return 0;

  const mean = rows.reduce((s, p) => s + num(p.gir, 0), 0) / rows.length;
  const delta = mean - target;
  if (Math.abs(delta) < minDelta) return 0;

  for (const p of rows) {
    const g = num(p.gir, NaN);
    if (!Number.isFinite(g)) continue;
    p.gir = Math.round(clamp(g - delta, 4, cap) * 100) / 100;
  }
  return rows.length;
}

function birdiesPlusEaglesFromPlayer(pl) {
  const b = num(pl?.birdies, NaN);
  const e = num(pl?.eagles, 0);
  if (!Number.isFinite(b)) return NaN;
  return b + (Number.isFinite(e) ? Math.max(0, e) : 0);
}

/** Birdies+eagles field target: venue history adjusted by course scoring ease. */
export function birdieCalibrationTargetMean(opts = {}) {
  const venueBird = num(opts.venueAvgBirdies, NaN);
  const venueEag = num(opts.venueAvgEagles, 0.12);
  const courseEase = num(opts.courseBirdieEase, NaN);
  if (!Number.isFinite(venueBird)) return NaN;
  const venueMkt = venueBird + venueEag;
  if (!Number.isFinite(courseEase)) return venueMkt;
  const courseMkt = venueMkt + courseEase;
  return Math.round((0.8 * courseMkt + 0.2 * venueMkt) * 100) / 100;
}

/** Shift birdies so field mean(birdies+eagles) hits course-adjusted venue target. */
export function calibrateBirdiesFieldMean(players, opts = {}) {
  const round = opts.round;
  const target = birdieCalibrationTargetMean(opts);
  const minDelta = num(opts.minDelta, 0.04);
  if (!Number.isFinite(target)) return 0;
  const rows = (players || []).filter((p) => {
    if (Number.isFinite(round) && Math.round(num(p.round, NaN)) !== round) return false;
    return Number.isFinite(birdiesPlusEaglesFromPlayer(p));
  });
  if (rows.length < 8) return 0;

  const mean = rows.reduce((s, p) => s + birdiesPlusEaglesFromPlayer(p), 0) / rows.length;
  const gap = target - mean;
  if (Math.abs(gap) < minDelta) return 0;

  for (const p of rows) {
    const b = num(p.birdies, NaN);
    if (!Number.isFinite(b)) continue;
    p.birdies = Math.round(clamp(b + gap, 0.15, 7) * 100) / 100;
    const e = num(p.eagles, 0);
    const d = num(p.doubles, 0);
    const bg = num(p.bogeys, NaN);
    if (Number.isFinite(bg)) {
      p.pars = Math.max(0.12, Math.round((18 - e - d - p.birdies - bg) * 100) / 100);
    }
  }
  return rows.length;
}

/** Population OLS count vs strokes-to-par (same coeffs as imputeCountsWithHistory). */
function olsCountFromHistFit(histCountFit, key, stp) {
  const c = histCountFit?.slopes?.[key];
  if (!c || !Number.isFinite(c.a) || !Number.isFinite(c.b)) return NaN;
  const x = Math.max(-8, Math.min(8, num(stp, 0)));
  return c.a + c.b * x;
}

function histFitShrink(histCountFit) {
  if (!histCountFit || histCountFit.n_counts < 800) return 0;
  return Math.min(0.35, histCountFit.n_counts / (histCountFit.n_counts + 2500));
}

function finalizeHoleCounts(eagles, birdies, bogeys, doubles) {
  eagles = clamp(eagles, 0, 1.2);
  birdies = clamp(birdies, 0.15, 7.5);
  bogeys = clamp(bogeys, 0.15, 8.5);
  doubles = clamp(doubles, 0.04, 2.5);
  const sum4 = eagles + birdies + bogeys + doubles;
  if (sum4 > 17.88) {
    const k = 17.88 / sum4;
    eagles *= k;
    birdies *= k;
    bogeys *= k;
    doubles *= k;
  }
  const pars = Math.max(0.12, 18 - eagles - birdies - bogeys - doubles);
  return {
    eagles: Math.round(eagles * 1000) / 1000,
    birdies: Math.round(birdies * 100) / 100,
    bogeys: Math.round(bogeys * 100) / 100,
    doubles: Math.round(doubles * 1000) / 1000,
    pars: Math.round(pars * 100) / 100,
  };
}

function courseScalarNorm01(v, lo, hi) {
  if (!Number.isFinite(v) || !Number.isFinite(lo) || !Number.isFinite(hi) || hi - lo < 1e-9) return 0.5;
  return clamp((v - lo) / (hi - lo), 0, 1);
}

/** Narrow fairways (low width yards) → 0, wide → 1. */
function courseFwWidthNorm(fwWidthYds) {
  return courseScalarNorm01(num(fwWidthYds, NaN), 23.5, 71.9);
}

/**
 * Birdies market (birdies+eagles) before course calibration.
 * Prefer rolling player BoB; if missing, use SG/OLS optimized prior — never invent full venue
 * rates for skill-less longshots (that made sponsor invites look like field birdie leaders).
 */
function birdiesEaglesFromPlayerRates(opts = {}) {
  const sk = opts.skRow || {};
  const venueBird = num(opts.venueBird, 3.8);
  const venueEag = num(opts.venueEagles, 0.12);
  const venueMkt = venueBird + venueEag;
  const playerMkt = num(sk.avg_birdies, NaN);
  const optMkt = num(opts.optimizedBirdMarket, NaN);
  const makeCut = num(opts.makeCut ?? sk.make_cut, NaN);
  const hasSkill = opts.hasSkillRatings === true || Number.isFinite(num(sk.sg_total, NaN));

  let mkt;
  if (Number.isFinite(playerMkt)) {
    mkt = playerMkt;
  } else if (!hasSkill && Number.isFinite(makeCut) && makeCut < 0.25) {
    // No counting history + no skill ratings + longshot make-cut → conservative BoB prior.
    const weakScale = clamp(0.4 + 0.6 * Math.min(1, makeCut / 0.4), 0.4, 1);
    const weakMkt = venueMkt * weakScale;
    mkt = Number.isFinite(optMkt) ? 0.35 * optMkt + 0.65 * weakMkt : weakMkt;
  } else if (Number.isFinite(optMkt)) {
    mkt = optMkt;
  } else {
    mkt = venueMkt;
  }

  let eagles = num(sk.avg_eagles, NaN);
  if (!Number.isFinite(eagles)) eagles = num(opts.optimizedEagles, venueEag);
  if (!Number.isFinite(eagles)) eagles = venueEag;
  eagles = clamp(eagles, 0, 1.1);
  const birdies = clamp(mkt - eagles, 0.15, 7);
  return { birdies, eagles };
}

/**
 * Birdies / bogeys / eagles / doubles from rolling rates + SG; pars is always residual to 18 holes.
 * Good T2G + poor putting shifts birdie/bogey looks into pars (par-machine profile).
 */
export function holeCountsFromRatesAndSg(opts = {}) {
  const mu = num(opts.muSg, 0);
  const sk = opts.skRow || {};
  const field = opts.fieldMeans || {};
  const histCountFit = opts.histCountFit || null;
  const dApp = sgDelta(sk, field, "sg_app");
  const dPutt = sgDelta(sk, field, "sg_putt");
  const dArg = sgDelta(sk, field, "sg_arg");
  const dOtt = sgDelta(sk, field, "sg_ott");
  const hasT2g =
    Number.isFinite(num(sk.sg_t2g, NaN)) && Number.isFinite(num(field.sg_t2g, NaN));
  const dT2g = hasT2g ? sgDelta(sk, field, "sg_t2g") : dApp + dOtt + dArg;

  const opt = optimizedHoleCounts({
    histCountFit,
    skRow: sk,
    muSg: mu,
    venueBird: opts.venueBird,
    venueBog: opts.venueBog,
    venueEagles: opts.venueEagles,
    venueDoubles: opts.venueDoubles,
    fieldGir: num(opts.fieldGir ?? opts.venueGir, 12),
    sgAppDelta: dApp,
    sgPuttDelta: dPutt,
    sgArgDelta: dArg,
    sgOttDelta: dOtt,
    sgT2gDelta: dT2g,
    sgT2gKnown: hasT2g,
    birdieSkillSpreadKeep: num(opts.birdieSkillSpreadKeep, BIRDIE_COURSE_SPREAD_KEEP),
  });

  const birdEag = birdiesEaglesFromPlayerRates({
    skRow: sk,
    fieldMeans: field,
    muSg: mu,
    venueBird: opts.venueBird,
    venueEagles: opts.venueEagles,
    venueGir: opts.venueGir,
    projectedGir: opts.projectedGir,
    courseBirdieEase: opts.courseBirdieEase,
    birdieSkillSpreadKeep: num(opts.birdieSkillSpreadKeep, BIRDIE_COURSE_SPREAD_KEEP),
    venueBirdieSgScale: opts.venueBirdieSgScale,
    optimizedBirdMarket: Number.isFinite(opt.birdies) ? opt.birdies + opt.eagles : NaN,
    optimizedEagles: opt.eagles,
    makeCut: opts.makeCut,
    hasSkillRatings: opts.hasSkillRatings,
  });

  let { eagles, birdies, bogeys, doubles } = {
    eagles: birdEag.eagles,
    birdies: birdEag.birdies,
    bogeys: opt.bogeys,
    doubles: opt.doubles,
  };
  if (Number.isFinite(BIRDIE_ACTUAL_BIAS_TRIM) && BIRDIE_ACTUAL_BIAS_TRIM > 0 && Number.isFinite(birdies)) {
    birdies = Math.max(0.15, birdies - BIRDIE_ACTUAL_BIAS_TRIM);
  }

  // Apply once after BoB rate merge so rolling avg_birdies and SG bogeys both get the shift.
  const shaped = applyT2gPuttParShape(
    { eagles, birdies, bogeys, doubles },
    {
      sgT2gDelta: dT2g,
      sgT2gKnown: hasT2g,
      sgAppDelta: dApp,
      sgOttDelta: dOtt,
      sgArgDelta: dArg,
      sgPuttDelta: dPutt,
    },
  );

  return finalizeHoleCounts(shaped.eagles, shaped.birdies, shaped.bogeys, shaped.doubles);
}

/** GIR count from course baseline + GIR% + SG:APP/T2G skill blend. */
export function girFromRatesAndSg(opts = {}) {
  const mu = num(opts.muSg, 0);
  const sk = opts.skRow || {};
  const liveTrad = opts.liveTrad ?? null;
  const nGir = num(opts.nGirHoles, 18);
  const venueGir = num(opts.venueGir, 12);
  const histCountFit = opts.histCountFit || null;
  const blendW = blendWeightsFromHistCalib(histCountFit);
  const field = opts.fieldMeans || {};

  // Prefer traditional GIR% for the rate leg; SG is blended inside optimizedGirCount.
  let playerRate01 = NaN;
  if (liveTrad && typeof liveTrad === "object") {
    playerRate01 = traditionalRate01(liveTrad.gir, 18);
  }
  if (!Number.isFinite(playerRate01)) {
    const cached = num(sk.dg_gir_pct, NaN);
    if (Number.isFinite(cached) && cached >= 0.15 && cached <= 0.95) playerRate01 = cached;
  }
  if (!Number.isFinite(playerRate01)) {
    const histGir = num(sk.avg_gir, NaN);
    if (Number.isFinite(histGir) && histGir > 4) playerRate01 = clamp(histGir / nGir, 0.35, 0.85);
  }
  const sgRate = girRate01FromSgApp(mu, sk.sg_app, opts.fieldMeanApp ?? field.sg_app, DG_TOUR_AVG_GIR_RATE);
  if (Number.isFinite(playerRate01) && Number.isFinite(sgRate)) {
    playerRate01 = 0.55 * playerRate01 + 0.45 * sgRate;
  } else if (!Number.isFinite(playerRate01) && Number.isFinite(sgRate)) {
    playerRate01 = sgRate;
  }

  const gir = optimizedGirCount({
    histCountFit,
    skRow: sk,
    muSg: mu,
    nGirHoles: nGir,
    venueGir,
    courseGirRate01: opts.courseGirRate01,
    playerGirRate01: playerRate01,
    girSkillSpreadKeep: num(opts.girSkillSpreadKeep, blendW.gir.spreadKeep),
    sgAppDelta: sgDelta(sk, field, "sg_app"),
    sgT2gDelta: sgDelta(sk, field, "sg_t2g"),
    sgPuttDelta: sgDelta(sk, field, "sg_putt"),
  });

  if (!Number.isFinite(gir)) {
    const rate01 = clamp(num(playerRate01, venueGir / nGir), 0.4, 0.8);
    return Math.round(clamp(rate01 * nGir, 6, 16) * 100) / 100;
  }
  return Math.round(clamp(gir, 6, 16.5) * 100) / 100;
}

/** Resolve player fairway rate (0–1) — driving accuracy is the primary signal. */
function playerFairwayRate01FromDrivingAccuracy(sk, liveTrad, nFw, fieldMeans, mu) {
  if (liveTrad && typeof liveTrad === "object") {
    const live = traditionalRate01(liveTrad.accuracy, nFw);
    if (Number.isFinite(live)) return live;
  }

  let rate01 = fairwayRate01FromSkillRatingsPp(sk, DG_TOUR_AVG_FAIRWAY_RATE, nFw);
  if (!Number.isFinite(rate01)) rate01 = fairwayRate01FromDg(sk, liveTrad, nFw);
  if (!Number.isFinite(rate01)) {
    const cached = num(sk?.dg_fairway_pct, NaN);
    if (Number.isFinite(cached) && cached >= 0.15 && cached <= 0.88) rate01 = cached;
  }
  if (!Number.isFinite(rate01)) {
    const daCnt = num(sk?.avg_fairways, NaN);
    if (Number.isFinite(daCnt) && daCnt >= 2 && daCnt <= nFw + 1) {
      rate01 = clamp(daCnt / nFw, 0.28, 0.88);
    }
  }
  if (!Number.isFinite(rate01)) {
    const dOtt = clamp(sgDelta(sk, fieldMeans, "sg_ott"), -0.28, 0.28);
    rate01 = clamp(DG_TOUR_AVG_FAIRWAY_RATE + 0.72 * dOtt + 0.08 * num(mu, 0), 0.32, 0.88);
  }
  return rate01;
}

/**
 * Fairways: course baseline + driving accuracy + SG:OTT (skill-heavy).
 */
export function fairwaysFromDrivingAccuracyAndCourse(opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const sk = opts.skRow || {};
  const liveTrad = opts.liveTrad ?? null;
  const fieldMeans = opts.fieldMeans || {};
  const mu = num(opts.muSg, 0);
  const histCountFit = opts.histCountFit || null;
  const blendW = blendWeightsFromHistCalib(histCountFit);

  const playerRate01 = playerFairwayRate01FromDrivingAccuracy(sk, liveTrad, nFw, fieldMeans, mu);
  const fairways = optimizedFairwayCount({
    histCountFit,
    skRow: sk,
    muSg: mu,
    nFairwayHoles: nFw,
    venueFairways: num(opts.venueFairways, DG_TOUR_AVG_FAIRWAY_RATE * nFw),
    courseFairwayRate01: opts.courseFairwayRate01,
    playerFwRate01: playerRate01,
    fairwaySkillSpreadKeep: num(opts.fairwaySkillSpreadKeep, blendW.fairways.spreadKeep),
    sgOttDelta: sgDelta(sk, fieldMeans, "sg_ott"),
    sgAppDelta: sgDelta(sk, fieldMeans, "sg_app"),
  });
  if (Number.isFinite(fairways)) return Math.round(clamp(fairways, 2, nFw + 0.5) * 100) / 100;

  const tourHits = DG_TOUR_AVG_FAIRWAY_RATE * nFw;
  const dOtt = sgDelta(sk, fieldMeans, "sg_ott");
  return Math.round(clamp(tourHits + 0.9 * dOtt, 2, nFw + 0.5) * 100) / 100;
}

/** Fairways: driving accuracy primary (no course-average anchor). */
export function fairwaysFromRatesAndSg(opts = {}) {
  const fieldMeans = opts.fieldMeans || {
    sg_ott: opts.fieldMeanOtt,
    sg_app: opts.fieldMeanApp,
    sg_putt: opts.fieldMeanPutt,
    sg_arg: opts.fieldMeanArg,
  };
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const fairways = fairwaysFromDrivingAccuracyAndCourse({ ...opts, fieldMeans });
  if (Number.isFinite(fairways)) return fairways;
  return Math.round(clamp(DG_TOUR_AVG_FAIRWAY_RATE * nFw, 2, nFw + 0.5) * 100) / 100;
}

/** Putts from rolling rate + SG:PUTT (+ missed GIR proxy). */
export function puttsFromRatesAndSg(opts = {}) {
  const sk = opts.skRow || {};
  const field = opts.fieldMeans || {};
  const venuePutts = num(opts.venuePutts, 29);
  const gir = num(opts.gir, 11);
  const nHist = Math.round(num(sk.counting_rounds, 0)) || 0;

  let putts = histBlend(num(sk.avg_putts, NaN), venuePutts, nHist, 14);
  const dPutt = sgDelta(sk, field, "sg_putt");
  putts += -2.6 * dPutt + 0.12 * (11 - gir);
  return Math.round(clamp(putts, 24, 34) * 100) / 100;
}

/**
 * Full counting row — all markets from rates + SG.
 */
export function derivedStatsFromRatesAndSg(muRaw, nFairwayHoles, opts = {}) {
  const mu_sg = Math.max(-12, Math.min(12, num(muRaw, 0)));
  const skR = opts.skRow || {};
  const liveTrad = opts.liveTrad ?? null;
  const fieldMeans = {
    sg_app: opts.fieldMeanApp,
    sg_ott: opts.fieldMeanOtt,
    sg_putt: opts.fieldMeanPutt,
    sg_arg: opts.fieldMeanArg,
    sg_t2g: opts.fieldMeanT2g,
  };

  const blendW = blendWeightsFromHistCalib(opts.histCountFit);
  const girOpts = {
    muSg: mu_sg,
    skRow: skR,
    liveTrad,
    fieldMeanApp: opts.fieldMeanApp,
    fieldMeans,
    venueGir: opts.venueGir,
    nGirHoles: opts.nGirHoles ?? 18,
    courseGirRate01: opts.courseGirRate01,
    girSkillSpreadKeep: num(opts.girSkillSpreadKeep, blendW.gir.spreadKeep),
    histCountFit: opts.histCountFit,
  };
  const gir = girFromRatesAndSg(girOpts);

  const hole = holeCountsFromRatesAndSg({
    muSg: mu_sg,
    skRow: skR,
    fieldMeans,
    histCountFit: opts.histCountFit,
    venueBird: opts.venueBird,
    venueBog: opts.venueBog,
    venueEagles: opts.venueEagles,
    venueDoubles: opts.venueDoubles,
    venueGir: opts.venueGir,
    projectedGir: gir,
    courseBirdieEase: opts.courseBirdieEase,
    fieldGir: opts.venueGir,
    venueBirdieSgScale: opts.venueBirdieSgScale ?? venueBirdieSgScale(opts.venueBird, 4.2),
    makeCut: opts.makeCut,
    hasSkillRatings: opts.hasSkillRatings,
  });

  const fairways = fairwaysFromRatesAndSg({
    muSg: mu_sg,
    skRow: skR,
    liveTrad,
    fieldMeans,
    fieldMeanOtt: opts.fieldMeanOtt,
    fieldMeanDrive: opts.fieldMeanDrive,
    drivingDistance: opts.drivingDistance,
    nFairwayHoles,
    venueFairways: opts.venueFairways,
    courseFairwayRate01: opts.courseFairwayRate01,
    fieldMeanDgFairways14: opts.fieldMeanDgFairways14,
    courseFwWidthNorm: opts.courseFwWidthNorm,
    courseAdjDrivingDistance: opts.courseAdjDrivingDistance,
    courseFwDifficulty: opts.courseFwDifficulty,
    fairwaySkillSpreadKeep: num(opts.fairwaySkillSpreadKeep, blendW.fairways.spreadKeep),
    histCountFit: opts.histCountFit,
  });

  const putts = puttsFromRatesAndSg({
    skRow: skR,
    fieldMeans,
    venuePutts: opts.venuePutts,
    gir,
  });

  return {
    mu_sg,
    implied_mu_sg: mu_sg,
    eagles: hole.eagles,
    birdies: hole.birdies,
    pars: hole.pars,
    bogeys: hole.bogeys,
    doubles: hole.doubles,
    gir,
    fairways,
    putts,
  };
}

/**
 * Rolling per-player counting means from historical_rounds_all.csv.
 * Prefer last ~2 years; if that window is empty/thin, backfill older rounds (sponsor invites,
 * Monday qualifiers) so birdies don't fall through to venue mean.
 */
export async function buildRollingHoleCountRatesByDg(csvPath, dgIdSet, opts = {}) {
  const maxR = Math.max(8, Math.round(num(opts.maxRoundsPerPlayer, BIRDIE_BOB_WINDOW)));
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const cy = new Date().getFullYear();
  const minYearRecent = cy - 2;
  const minYearStale = cy - 20;
  /** @type {Map<number, object>} */
  const buf = new Map();
  if (!existsSync(csvPath) || !dgIdSet?.size) return new Map();

  await new Promise((resolve, reject) => {
    createReadStream(csvPath).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    )
      .on("data", (row) => {
        const tour = String(row.tour || "").toLowerCase();
        if (tour !== "pga" && tour !== "liv") return;
        const yr = parseInt(row.year, 10);
        if (Number.isFinite(yr) && yr < minYearStale) return;
        const id = Math.round(num(row.dg_id, NaN));
        if (!Number.isFinite(id) || !dgIdSet.has(id)) return;
        const rs = num(row.round_score, NaN);
        if (!Number.isFinite(rs) || rs < 55 || rs > 95) return;

        let slot = buf.get(id);
        if (!slot) {
          slot = { rows: [] };
          buf.set(id, slot);
        }
        const b = birdiesFromHistRow(row);
        const bg = bogeysFromHistRow(row);
        const e = num(row.eagles_or_better ?? row.eagles, 0);
        const d = num(row.doubles_or_worse ?? row.doubles, 0);
        const p = num(row.pars, NaN);
        const pt = num(row.putts, NaN);
        const g = countFromRateOrRaw(row.gir, 18);
        const f = countFromRateOrRaw(row.driving_acc, nFw);
        slot.rows.push({
          ts: Date.parse(row.event_completed || "") || (Number.isFinite(yr) ? Date.UTC(yr, 0, 1) : 0),
          year: Number.isFinite(yr) ? yr : 0,
          round: Math.round(num(row.round_num, 0)),
          bird: Number.isFinite(b) && b >= 0 && b <= 10 ? b : NaN,
          bog: Number.isFinite(bg) && bg >= 0 && bg <= 14 ? bg : NaN,
          eag: Number.isFinite(e) && e >= 0 && e <= 3 ? e : NaN,
          dbl: Number.isFinite(d) && d >= 0 && d <= 5 ? d : NaN,
          par: Number.isFinite(p) && p >= 4 && p <= 16 ? p : NaN,
          putt: Number.isFinite(pt) && pt >= 24 && pt <= 36 ? pt : NaN,
          gir: Number.isFinite(g) && g >= 4 && g <= 17 ? g : NaN,
          fw: Number.isFinite(f) && f >= 2 && f <= nFw + 1 ? f : NaN,
        });
      })
      .on("end", resolve)
      .on("error", reject);
  });

  return summarizeHoleCountRateBuf(buf, maxR, minYearRecent);
}

/**
 * @param {Map<number, { rows: object[] }>} buf
 * @param {number} maxR
 * @param {number} minYearRecent
 */
function summarizeHoleCountRateBuf(buf, maxR, minYearRecent) {
  const mean = (a) => (a.length ? a.reduce((s, x) => s + x, 0) / a.length : NaN);
  /** @type {Map<number, object>} */
  const out = new Map();
  for (const [id, slot] of buf) {
    const sorted = slot.rows.sort((a, b) => b.ts - a.ts || b.round - a.round);
    const recent = sorted.filter((r) => r.year >= minYearRecent);
    let chosen = recent.slice(0, maxR);
    let staleUsed = 0;
    if (chosen.length < 4) {
      const need = maxR - chosen.length;
      const older = sorted.filter((r) => r.year < minYearRecent).slice(0, need);
      staleUsed = older.length;
      chosen = chosen.concat(older);
    }
    const vals = (key) => chosen.map((row) => row[key]).filter(Number.isFinite);
    const bird = vals("bird");
    const bog = vals("bog");
    const nRaw = Math.max(bird.length, bog.length);
    if (!nRaw) continue;
    // Discount stale-only samples so 2016 major rates don't dominate Detroit soft-field priors.
    const nRecentBird = recent.map((r) => r.bird).filter(Number.isFinite).length;
    const nEff =
      nRecentBird >= 4
        ? nRaw
        : Math.max(1, Math.round(nRecentBird + staleUsed * 0.4));
    out.set(id, {
      avg_birdies: mean(bird),
      avg_bogeys: mean(bog),
      avg_eagles: mean(vals("eag")),
      avg_doubles: mean(vals("dbl")),
      avg_pars: mean(vals("par")),
      avg_putts: mean(vals("putt")),
      avg_gir: mean(vals("gir")),
      avg_fairways: mean(vals("fw")),
      counting_rounds: nEff,
      rates_stale: staleUsed > 0 && nRecentBird < 4,
    });
  }
  return out;
}

/**
 * Fill missing rolling rates from player-history/by-dg shards (invitees often absent from
 * repo data/historical_rounds_all.csv but present in web shards).
 * @param {Map<number, object>} ratesMap
 * @param {Set<number>} dgIdSet
 * @param {string} shardDir
 */
export function supplementHoleCountRatesFromPlayerShards(ratesMap, dgIdSet, shardDir) {
  if (!ratesMap || !dgIdSet?.size || !shardDir || !existsSync(shardDir)) return 0;
  const cy = new Date().getFullYear();
  const minYearRecent = cy - 2;
  const maxR = BIRDIE_BOB_WINDOW;
  /** @type {Map<number, object>} */
  const buf = new Map();
  let added = 0;
  for (const id of dgIdSet) {
    if (ratesMap.has(id)) continue;
    const shardPath = joinShardPath(shardDir, id);
    if (!existsSync(shardPath)) continue;
    let doc;
    try {
      doc = JSON.parse(readFileSync(shardPath, "utf8"));
    } catch {
      continue;
    }
    const rounds = Array.isArray(doc?.rounds) ? doc.rounds : [];
    if (!rounds.length) continue;
    const slot = { rows: [] };
    for (const row of rounds) {
      const yr = parseInt(row.year, 10);
      if (Number.isFinite(yr) && yr < cy - 20) continue;
      const rs = num(row.round_score, NaN);
      if (Number.isFinite(rs) && (rs < 55 || rs > 95)) continue;
      const b = birdiesFromHistRow(row);
      const bg = bogeysFromHistRow(row);
      const e = num(row.eagles_or_better ?? row.eagles, 0);
      const d = num(row.doubles_or_worse ?? row.doubles, 0);
      const p = num(row.pars, NaN);
      const pt = num(row.putts, NaN);
      const g = countFromRateOrRaw(row.gir, 18);
      slot.rows.push({
        ts: Date.parse(row.event_completed || "") || (Number.isFinite(yr) ? Date.UTC(yr, 0, 1) : 0),
        year: Number.isFinite(yr) ? yr : 0,
        round: Math.round(num(row.round_num, 0)),
        bird: Number.isFinite(b) && b >= 0 && b <= 10 ? b : NaN,
        bog: Number.isFinite(bg) && bg >= 0 && bg <= 14 ? bg : NaN,
        eag: Number.isFinite(e) && e >= 0 && e <= 3 ? e : NaN,
        dbl: Number.isFinite(d) && d >= 0 && d <= 5 ? d : NaN,
        par: Number.isFinite(p) && p >= 4 && p <= 16 ? p : NaN,
        putt: Number.isFinite(pt) && pt >= 24 && pt <= 36 ? pt : NaN,
        gir: Number.isFinite(g) && g >= 4 && g <= 17 ? g : NaN,
        fw: NaN,
      });
    }
    if (!slot.rows.length) continue;
    buf.set(id, slot);
  }
  const extra = summarizeHoleCountRateBuf(buf, maxR, minYearRecent);
  for (const [id, rates] of extra) {
    if (ratesMap.has(id)) continue;
    ratesMap.set(id, rates);
    added++;
  }
  return added;
}

function joinShardPath(shardDir, dgId) {
  // Avoid importing path — keep this module light; shards are always by-dg/<id>.json
  const sep = shardDir.includes("\\") ? "\\" : "/";
  const base = shardDir.endsWith(sep) ? shardDir.slice(0, -1) : shardDir;
  return `${base}${sep}${Math.round(dgId)}.json`;
}

/** Attach rolling counting rates onto a skill-ratings row object. */
export function mergeHoleCountRatesIntoSkillRow(skRow, rates) {
  if (!skRow || !rates) return skRow;
  for (const k of [
    "avg_birdies",
    "avg_bogeys",
    "avg_eagles",
    "avg_doubles",
    "avg_pars",
    "avg_putts",
    "avg_gir",
    "avg_fairways",
    "counting_rounds",
    "rates_stale",
  ]) {
    if (k === "rates_stale") {
      if (rates[k] != null) skRow[k] = !!rates[k];
      continue;
    }
    if (Number.isFinite(rates[k]) || k === "counting_rounds") skRow[k] = rates[k];
  }
  // avg_gir / avg_fairways are career hole-count means for optimized blend only —
  // do not write dg_*_pct (that flattens skill-ratings / traditional % spread).
  return skRow;
}
