/**
 * Data-driven blend weights for counting markets: course anchor + player rates + SG + stp OLS.
 * Weights derive from historical_projection_calibration (R², n_counts, fw_stp_line).
 */
import { num } from "./dg-traditional-stats.mjs";

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function weightedMean(pairs) {
  let sum = 0;
  let wsum = 0;
  for (const { w, v } of pairs) {
    const wt = num(w, NaN);
    const val = num(v, NaN);
    if (!Number.isFinite(wt) || wt <= 0 || !Number.isFinite(val)) continue;
    sum += wt * val;
    wsum += wt;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

function careerShrink(nHist, halfLife = 28, cap = 0.32) {
  const n = Math.max(0, Math.round(num(nHist, NaN)));
  if (n < 4) return 0;
  return Math.min(cap, n / (n + halfLife));
}

/** OLS hole-count at strokes-to-par from population historical fit. */
export function olsCountAtStp(histCalib, key, stp) {
  const c = histCalib?.slopes?.[key];
  if (!c || !Number.isFinite(c.a) || !Number.isFinite(c.b)) return NaN;
  const x = clamp(num(stp, 0), -8, 8);
  return c.a + c.b * x;
}

function olsTrust(histCalib) {
  const n = num(histCalib?.n_counts, 0);
  if (n < 800) return 0;
  return Math.min(0.42, n / (n + 2800));
}

/**
 * Blend weights from venue-course historical calibration (fetch:dg / walkforward).
 * @param {object | null | undefined} histCalib
 */
export function blendWeightsFromHistCalib(histCalib) {
  const r2Gir = clamp(num(histCalib?.r2_gir_app, NaN), 0.22, 0.88) || 0.48;
  const r2Fw = clamp(num(histCalib?.r2_fw_ott, NaN), 0.18, 0.82) || 0.36;
  const olsW = olsTrust(histCalib);
  const hasFwStp = !!(
    histCalib?.fw_stp_line &&
    Number.isFinite(histCalib.fw_stp_line.a) &&
    Number.isFinite(histCalib.fw_stp_line.b)
  );

  const wGirSkill = clamp(0.5 + 0.4 * Math.sqrt(r2Gir), 0.55, 0.92);
  const wOttSkill = clamp(0.46 + 0.42 * Math.sqrt(r2Fw), 0.5, 0.9);

  return {
    w_gir_skill: wGirSkill,
    w_ott_skill: wOttSkill,
    w_ott_decomp: clamp(0.52 + 0.32 * Math.sqrt(r2Fw), 0.52, 0.86),
    gir: {
      spreadKeep: clamp(0.76 + 0.18 * r2Gir, 0.76, 0.92),
      wVenue: 0.1,
      wRate: 0.58 + 0.22 * wGirSkill,
      wSg: wGirSkill,
      wCareerCap: 0.08 + 0.08 * (1 - r2Gir),
      sgAppCoeff: 0.38 + 0.32 * r2Gir,
      muCoeff: 0.035 + 0.03 * r2Gir,
    },
    fairways: {
      spreadKeep: clamp(0.74 + 0.18 * r2Fw, 0.74, 0.9),
      wVenue: 0.1,
      wRate: 0.56 + 0.24 * wOttSkill,
      wSg: wOttSkill,
      wStp: hasFwStp ? clamp(0.06 + 0.1 * r2Fw, 0.06, 0.16) : 0,
      wCareerCap: 0.06 + 0.08 * (1 - r2Fw),
      sgOttCoeff: 0.28 + 0.3 * r2Fw,
      muCoeff: 0.03 + 0.025 * r2Fw,
    },
    birdies: {
      wVenue: 0.2,
      wRate: 0.32,
      wSg: 0.38 + 0.18 * r2Gir,
      wOls: olsW * 0.5,
      sgApp: 0.52 + 0.24 * r2Gir,
      sgPutt: 0.38,
      sgOtt: 0.05,
      mu: 0.06 + 0.03 * r2Gir,
    },
    bogeys: {
      wVenue: 0.18,
      wRate: 0.3,
      wSg: 0.36 + 0.16 * r2Gir,
      wOls: olsW * 0.48,
      sgArg: 0.48,
      sgApp: 0.18,
      mu: 0.28,
      sgPutt: 0.08,
      sgOtt: 0.06,
      girMiss: 0.1,
      dblExcess: 0.12,
      wScoreStp: 0.22,
    },
  };
}

/** Tour FW vs stp line + OTT edge (population fit). */
export function fairwaysFromHistoricalStp(muSg, nFw, histCalib, fieldMeanOtt, skRow) {
  const ln = histCalib?.fw_stp_line;
  if (!ln || !Number.isFinite(ln.a) || !Number.isFinite(ln.b)) return NaN;
  const mu = clamp(num(muSg, 0), -4, 4);
  const x = clamp(-mu, -10, 10);
  let raw = ln.a + ln.b * x;
  raw += 0.42 * Math.max(0, Math.min(2.5, mu));
  const ott = num(skRow?.sg_ott, NaN);
  const fo = num(fieldMeanOtt, NaN);
  if (Number.isFinite(ott) && Number.isFinite(fo)) {
    const edge = clamp(ott - fo, -0.45, 1.15);
    raw += 1.85 * edge;
  }
  return clamp(raw, 0, num(nFw, 14));
}

/**
 * Optimized GIR: course adj rate + player GIR% + SG:APP delta + career mean.
 */
export function optimizedGirCount(opts = {}) {
  const hist = opts.histCountFit || null;
  const w = blendWeightsFromHistCalib(hist).gir;
  const sk = opts.skRow || {};
  const nGir = num(opts.nGirHoles, 18);
  const venueGir = num(opts.venueGir, 12);
  const courseRate = num(opts.courseGirRate01, NaN);
  const courseAnchor = Number.isFinite(courseRate) ? courseRate * nGir : venueGir;
  const nHist = Math.round(num(sk.counting_rounds, 0)) || 0;

  const playerRate01 = num(opts.playerGirRate01, NaN);
  const spreadKeep = num(opts.girSkillSpreadKeep, w.spreadKeep);
  const anchorRate = Number.isFinite(courseRate) ? courseRate : venueGir / nGir;
  let skillRate01 = playerRate01;
  if (Number.isFinite(skillRate01) && Number.isFinite(anchorRate)) {
    skillRate01 = anchorRate + spreadKeep * (skillRate01 - anchorRate);
  }
  const fromRate = Number.isFinite(skillRate01) ? skillRate01 * nGir : NaN;

  const dApp = num(opts.sgAppDelta, 0);
  const mu = num(opts.muSg, 0);
  const career = num(sk.avg_gir, NaN);
  const wCar = careerShrink(nHist, 28, w.wCareerCap);

  const core = weightedMean([
    { w: w.wVenue, v: courseAnchor },
    { w: w.wRate, v: fromRate },
    { w: wCar, v: career },
  ]);
  if (!Number.isFinite(core)) return NaN;
  return core + w.sgAppCoeff * dApp + w.muCoeff * mu;
}

/**
 * Optimized fairways: course adj + FW% + SG:OTT delta + stp line + career mean.
 */
export function optimizedFairwayCount(opts = {}) {
  const hist = opts.histCountFit || null;
  const w = blendWeightsFromHistCalib(hist).fairways;
  const sk = opts.skRow || {};
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const venueFw = num(opts.venueFairways, 9);
  const courseRate = num(opts.courseFairwayRate01, NaN);
  const courseAnchor = Number.isFinite(courseRate) ? courseRate * nFw : venueFw;
  const nHist = Math.round(num(sk.counting_rounds, 0)) || 0;

  const playerRate01 = num(opts.playerFwRate01, NaN);
  const spreadKeep = num(opts.fairwaySkillSpreadKeep, w.spreadKeep);
  const anchorRate = Number.isFinite(courseRate) ? courseRate : venueFw / nFw;
  let skillRate01 = playerRate01;
  if (Number.isFinite(skillRate01) && Number.isFinite(anchorRate)) {
    skillRate01 = anchorRate + spreadKeep * (skillRate01 - anchorRate);
  }
  const fromRate = Number.isFinite(skillRate01) ? skillRate01 * nFw : NaN;

  const dOtt = num(opts.sgOttDelta, 0);
  const mu = num(opts.muSg, 0);
  const fromStp = fairwaysFromHistoricalStp(mu, nFw, hist, opts.fieldMeanOtt, sk);

  const career = num(sk.avg_fairways, NaN);
  const wCar = careerShrink(nHist, 28, w.wCareerCap);

  const core = weightedMean([
    { w: w.wVenue, v: courseAnchor },
    { w: w.wRate, v: fromRate },
    { w: w.wStp, v: fromStp },
    { w: wCar, v: career },
  ]);
  if (!Number.isFinite(core)) return NaN;
  return core + w.sgOttCoeff * dOtt + w.muCoeff * mu;
}

/**
 * Optimized birdies / bogeys / eagles / doubles from venue + rates + SG + stp OLS.
 */
export function optimizedHoleCounts(opts = {}) {
  const hist = opts.histCountFit || null;
  const wBird = blendWeightsFromHistCalib(hist).birdies;
  const wBog = blendWeightsFromHistCalib(hist).bogeys;
  const sk = opts.skRow || {};
  const mu = num(opts.muSg, 0);
  const stp = -mu;
  const nHist = Math.round(num(sk.counting_rounds, 0)) || 0;

  const venueBird = num(opts.venueBird, 3.8);
  const venueBog = num(opts.venueBog, 2.6);
  const venueEag = num(opts.venueEagles, 0.12);
  const venueDbl = num(opts.venueDoubles, 0.32);

  const dApp = num(opts.sgAppDelta, 0);
  const dPutt = num(opts.sgPuttDelta, 0);
  const dArg = num(opts.sgArgDelta, 0);
  const dOtt = num(opts.sgOttDelta, 0);
  const fieldGir = num(opts.fieldGir, 12);
  const playerGir = num(sk.avg_gir, NaN);
  const girMiss = Number.isFinite(playerGir) ? fieldGir - playerGir : 0;

  const birdRate = num(sk.avg_birdies, NaN);
  const bogRate = num(sk.avg_bogeys, NaN);
  const eagRate = num(sk.avg_eagles, NaN);
  const dblRate = num(sk.avg_doubles, NaN);
  const dblExcess = Math.max(0, num(dblRate, venueDbl) - venueDbl);

  const birdSg =
    wBird.sgApp * dApp + wBird.sgPutt * dPutt + wBird.mu * mu + wBird.sgOtt * dOtt;
  const bogSg =
    wBog.sgArg * (-dArg) +
    wBog.sgApp * (-dApp) +
    wBog.mu * (-mu) +
    wBog.sgPutt * (-dPutt) +
    wBog.girMiss * girMiss +
    wBog.sgOtt * (-dOtt) +
    wBog.dblExcess * dblExcess;

  const birdCore = weightedMean([
    { w: wBird.wVenue, v: venueBird },
    { w: wBird.wRate, v: birdRate },
    { w: wBird.wOls, v: olsCountAtStp(hist, "birdies", stp) },
  ]);
  const bogCore = weightedMean([
    { w: wBog.wVenue, v: venueBog },
    { w: wBog.wRate, v: bogRate },
    { w: wBog.wOls, v: olsCountAtStp(hist, "bogeys", stp) },
  ]);

  let birdies = Number.isFinite(birdCore) ? birdCore + wBird.wSg * birdSg : birdSg + venueBird;
  let bogeys = Number.isFinite(bogCore) ? bogCore + wBog.wSg * bogSg : bogSg + venueBog;

  let eagles = weightedMean([
    { w: 0.35, v: venueEag },
    { w: 0.4, v: eagRate },
    { w: 0.25, v: venueEag + 0.32 * dApp + 0.18 * dOtt + 0.1 * Math.max(0, mu) },
  ]);
  let doubles = weightedMean([
    { w: 0.35, v: venueDbl },
    { w: 0.4, v: dblRate },
    {
      w: 0.25,
      v: venueDbl + 0.38 * (-dArg) + 0.18 * Math.max(0, -mu) + 0.08 * (-dApp),
    },
  ]);

  if (!Number.isFinite(birdies)) birdies = birdSg;
  if (!Number.isFinite(bogeys)) bogeys = bogSg;
  if (!Number.isFinite(eagles)) eagles = venueEag;
  if (!Number.isFinite(doubles)) doubles = venueDbl;

  const scoreBog = clamp(venueBog + stp * 0.56, 0.15, 8.5);
  bogeys = (1 - wBog.wScoreStp) * bogeys + wBog.wScoreStp * scoreBog;

  return { eagles, birdies, bogeys, doubles, nHist };
}
