/**
 * Counting markets from player rolling rates + SG categories (no score-to-par derive).
 */
import { createReadStream, existsSync } from "fs";
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
  optimizedHoleCounts,
} from "./optimized-counting-blend.mjs";
import { venueBirdieSgScale } from "./projection-stat-model.mjs";

/** Player FW spread vs tour when course anchor is off (near 1 = pure driving accuracy). */
const FAIRWAY_DRIVING_ACC_SPREAD = 0.94;

/** Venue birdie anchor: keep player spread vs course birdie mean (DK market ≈ birdies + eagles). */
const BIRDIE_COURSE_SPREAD_KEEP = 0.42;

/** Stronger spread for above-venue birdie rates so elite players reach DK 5.5 lines. */
function birdieSpreadKeepForPlayer(baseSpread, playerMkt, venueMkt, mu) {
  let spread = num(baseSpread, BIRDIE_COURSE_SPREAD_KEEP);
  if (Number.isFinite(playerMkt) && Number.isFinite(venueMkt) && playerMkt > venueMkt) {
    const excess = playerMkt - venueMkt;
    spread = clamp(spread + 0.4 * excess + 0.04 * Math.max(0, mu), spread, 0.9);
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
    const w = clamp(num(courseWeight, 0.82), 0.65, 0.92);
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
 * Birdies market (birdies+eagles): rolling rate + optimized SG blend + GIR opportunity.
 */
function birdiesEaglesFromPlayerRates(opts = {}) {
  const sk = opts.skRow || {};
  const field = opts.fieldMeans || {};
  const mu = num(opts.muSg, 0);
  const venueBird = num(opts.venueBird, 3.8);
  const venueEag = num(opts.venueEagles, 0.12);
  const venueMkt = venueBird + venueEag;
  const playerMkt = num(sk.avg_birdies, NaN);
  const spread = birdieSpreadKeepForPlayer(
    num(opts.birdieSkillSpreadKeep, BIRDIE_COURSE_SPREAD_KEEP),
    playerMkt,
    venueMkt,
    mu,
  );
  const dApp = sgDelta(sk, field, "sg_app");
  const dPutt = sgDelta(sk, field, "sg_putt");
  const dArg = sgDelta(sk, field, "sg_arg");
  const dOtt = sgDelta(sk, field, "sg_ott");

  let rateMkt = venueMkt;
  if (Number.isFinite(playerMkt)) rateMkt = venueMkt + spread * (playerMkt - venueMkt);
  const birdSgScale = num(opts.venueBirdieSgScale, 1);
  rateMkt += birdSgScale * (0.26 * dApp + 0.09 * dPutt + 0.03 * mu);

  const optMkt = num(opts.optimizedBirdMarket, NaN);
  let mkt = Number.isFinite(optMkt) ? 0.36 * rateMkt + 0.64 * optMkt : rateMkt;

  const gir = num(opts.projectedGir, num(sk.avg_gir, NaN));
  const venueGir = num(opts.venueGir, 12);
  if (Number.isFinite(gir) && Number.isFinite(venueGir)) {
    mkt += 0.28 * ((gir - venueGir) / 18);
  }

  const courseEase = num(opts.courseBirdieEase, 0);
  if (Number.isFinite(courseEase)) mkt += courseEase;

  mkt += birdSgScale * (0.14 * dArg + 0.06 * dOtt);

  const venueAnchor = venueMkt + (Number.isFinite(courseEase) ? courseEase : 0);
  mkt = 0.78 * mkt + 0.22 * venueAnchor;

  let eagles = num(opts.optimizedEagles, num(sk.avg_eagles, NaN));
  if (!Number.isFinite(eagles)) eagles = venueEag;
  eagles = clamp(eagles, 0, 1.1);
  const birdies = clamp(mkt - eagles, 0.15, 7);
  return { birdies, eagles };
}

/**
 * Birdies / bogeys / eagles / doubles from rolling rates + SG; pars is always residual to 18 holes.
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

  return finalizeHoleCounts(eagles, birdies, bogeys, doubles);
}

/** GIR count from optimized course + GIR% + SG:APP blend. */
export function girFromRatesAndSg(opts = {}) {
  const mu = num(opts.muSg, 0);
  const sk = opts.skRow || {};
  const liveTrad = opts.liveTrad ?? null;
  const nGir = num(opts.nGirHoles, 18);
  const venueGir = num(opts.venueGir, 12);
  const histCountFit = opts.histCountFit || null;
  const blendW = blendWeightsFromHistCalib(histCountFit);

  let playerRate01 = girRate01FromDg(sk, liveTrad, { muSg: mu, fieldMeanApp: opts.fieldMeanApp });
  if (!Number.isFinite(playerRate01)) {
    const histGir = num(sk.avg_gir, NaN);
    if (Number.isFinite(histGir) && histGir > 4) playerRate01 = clamp(histGir / nGir, 0.35, 0.85);
  }
  if (!Number.isFinite(playerRate01)) {
    playerRate01 = girRate01FromSgApp(mu, sk.sg_app, opts.fieldMeanApp, DG_TOUR_AVG_GIR_RATE);
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
    sgAppDelta: sgDelta(sk, opts.fieldMeans || {}, "sg_app"),
  });

  if (!Number.isFinite(gir)) {
    const rate01 = clamp(num(playerRate01, venueGir / nGir), 0.4, 0.8);
    return Math.round(clamp(rate01 * nGir, 6, 16) * 100) / 100;
  }
  return Math.round(clamp(gir, 6, 16) * 100) / 100;
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
    const dOtt = sgDelta(sk, fieldMeans, "sg_ott");
    rate01 = clamp(DG_TOUR_AVG_FAIRWAY_RATE + 0.72 * dOtt + 0.08 * num(mu, 0), 0.32, 0.88);
  }
  return rate01;
}

/**
 * Fairways from player driving accuracy (no course-average anchor).
 * μ ≈ player FW% × driving holes; small SG:OTT nudge when accuracy is pp-based.
 */
export function fairwaysFromDrivingAccuracyAndCourse(opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const sk = opts.skRow || {};
  const liveTrad = opts.liveTrad ?? null;
  const fieldMeans = opts.fieldMeans || {};
  const mu = num(opts.muSg, 0);

  const playerRate01 = playerFairwayRate01FromDrivingAccuracy(sk, liveTrad, nFw, fieldMeans, mu);
  const tourHits = DG_TOUR_AVG_FAIRWAY_RATE * nFw;
  const playerHits = playerRate01 * nFw;
  const spread = num(
    opts.fairwaySkillSpreadKeep,
    num(process.env.GOLF_FAIRWAY_SKILL_SPREAD_KEEP, FAIRWAY_DRIVING_ACC_SPREAD),
  );
  let fairways = tourHits + spread * (playerHits - tourHits);

  const dOtt = sgDelta(sk, fieldMeans, "sg_ott");
  fairways += 0.28 * dOtt;

  return Math.round(clamp(fairways, 2, nFw + 0.5) * 100) / 100;
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
 */
export async function buildRollingHoleCountRatesByDg(csvPath, dgIdSet, opts = {}) {
  const maxR = Math.max(8, Math.round(num(opts.maxRoundsPerPlayer, 36)));
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const cy = new Date().getFullYear();
  const minYear = cy - 2;
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
        if (Number.isFinite(yr) && yr < minYear) return;
        const id = Math.round(num(row.dg_id, NaN));
        if (!Number.isFinite(id) || !dgIdSet.has(id)) return;
        const rs = num(row.round_score, NaN);
        if (!Number.isFinite(rs) || rs < 55 || rs > 95) return;

        let slot = buf.get(id);
        if (!slot) {
          slot = { bird: [], bog: [], eag: [], dbl: [], par: [], putt: [], gir: [], fw: [] };
          buf.set(id, slot);
        }
        if (slot.bird.length >= maxR) return;

        const b = birdiesFromHistRow(row);
        if (Number.isFinite(b) && b >= 0 && b <= 10) slot.bird.push(b);
        const bg = num(row.bogeys ?? row.bogies, NaN);
        if (Number.isFinite(bg) && bg >= 0 && bg <= 12) slot.bog.push(bg);
        const e = num(row.eagles_or_better ?? row.eagles, 0);
        if (Number.isFinite(e) && e >= 0 && e <= 3) slot.eag.push(e);
        const d = num(row.doubles_or_worse ?? row.doubles, 0);
        if (Number.isFinite(d) && d >= 0 && d <= 5) slot.dbl.push(d);
        const p = num(row.pars, NaN);
        if (Number.isFinite(p) && p >= 4 && p <= 16) slot.par.push(p);
        const pt = num(row.putts, NaN);
        if (Number.isFinite(pt) && pt >= 24 && pt <= 36) slot.putt.push(pt);
        const g = countFromRateOrRaw(row.gir, 18);
        if (Number.isFinite(g) && g >= 4 && g <= 17) slot.gir.push(g);
        const f = countFromRateOrRaw(row.driving_acc, nFw);
        if (Number.isFinite(f) && f >= 2 && f <= nFw + 1) slot.fw.push(f);
      })
      .on("end", resolve)
      .on("error", reject);
  });

  const mean = (a) => (a.length ? a.reduce((s, x) => s + x, 0) / a.length : NaN);
  /** @type {Map<number, object>} */
  const out = new Map();
  for (const [id, slot] of buf) {
    const n = Math.max(slot.bird.length, slot.bog.length);
    if (!n) continue;
    out.set(id, {
      avg_birdies: mean(slot.bird),
      avg_bogeys: mean(slot.bog),
      avg_eagles: mean(slot.eag),
      avg_doubles: mean(slot.dbl),
      avg_pars: mean(slot.par),
      avg_putts: mean(slot.putt),
      avg_gir: mean(slot.gir),
      avg_fairways: mean(slot.fw),
      counting_rounds: n,
    });
  }
  return out;
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
  ]) {
    if (Number.isFinite(rates[k]) || k === "counting_rounds") skRow[k] = rates[k];
  }
  // avg_gir / avg_fairways are career hole-count means for optimized blend only —
  // do not write dg_*_pct (that flattens skill-ratings / traditional % spread).
  return skRow;
}
