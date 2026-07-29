/**
 * Data-driven blend weights for counting markets: course anchor + player rates + SG + stp OLS.
 * Weights derive from historical_projection_calibration (R², n_counts, fw_stp_line).
 */

function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

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

  const wGirSkill = clamp(0.5 + 0.4 * Math.sqrt(r2Gir), 0.55, 0.92);
  const wOttSkill = clamp(0.46 + 0.42 * Math.sqrt(r2Fw), 0.5, 0.9);

  return {
    w_gir_skill: wGirSkill,
    w_ott_skill: wOttSkill,
    w_ott_decomp: clamp(0.52 + 0.32 * Math.sqrt(r2Fw), 0.52, 0.86),
    gir: {
      // Skill-first: μ = course + keep·(rate/SG blend − course)
      spreadKeep: clamp(0.92 + 0.05 * r2Gir, 0.92, 0.98),
      wVenue: 0,
      wRate: 1,
      wSg: 0,
      wCareerCap: 0,
      sgAppCoeff: 0.62,
      muCoeff: 0.06,
    },
    fairways: {
      spreadKeep: clamp(0.9 + 0.06 * r2Fw, 0.9, 0.98),
      wVenue: 0,
      wRate: 1,
      wSg: 0,
      wStp: 0,
      wCareerCap: 0,
      sgOttCoeff: 0.95,
      muCoeff: 0.05,
    },
    birdies: {
      wVenue: 0.28,
      wRate: 0.38,
      wSg: 0.28 + 0.08 * r2Gir,
      wOls: olsW * 0.2,
      sgApp: 0.38 + 0.1 * r2Gir,
      sgPutt: 0.28,
      sgOtt: 0.08,
      mu: 0.04 + 0.02 * r2Gir,
      spreadKeep: clamp(0.55 + 0.15 * r2Gir, 0.55, 0.78),
    },
    bogeys: {
      wVenue: 0.12,
      wRate: 0.38,
      wSg: 0.4 + 0.12 * r2Gir,
      wOls: olsW * 0.2,
      sgArg: 0.55,
      sgApp: 0.22,
      mu: 0.22,
      sgPutt: 0.1,
      sgOtt: 0.08,
      girMiss: 0.12,
      dblExcess: 0.1,
      wScoreStp: 0.04,
      spreadKeep: clamp(0.75 + 0.12 * r2Gir, 0.75, 0.9),
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
 * GIR: course baseline + skill (GIR% and SG:APP / T2G), skill-heavy.
 * μ = course + keep·(skill − course)
 * skill = blend(rate hits, SG-implied hits)
 */
export function optimizedGirCount(opts = {}) {
  const hist = opts.histCountFit || null;
  const w = blendWeightsFromHistCalib(hist).gir;
  const sk = opts.skRow || {};
  const nGir = num(opts.nGirHoles, 18);
  const venueGir = num(opts.venueGir, 12);
  const courseRate = num(opts.courseGirRate01, NaN);
  const courseHits = Number.isFinite(courseRate) ? courseRate * nGir : venueGir;
  const tourRate = 0.665; // ~12/18 tour-ish baseline for SG→hits

  const playerRate01 = num(opts.playerGirRate01, NaN);
  const career = num(sk.avg_gir, NaN);
  const rateHits = Number.isFinite(playerRate01)
    ? playerRate01 * nGir
    : Number.isFinite(career)
      ? career
      : NaN;

  const dApp = num(opts.sgAppDelta, 0);
  const dT2g = num(opts.sgT2gDelta, 0);
  const dPutt = num(opts.sgPuttDelta, 0);
  const mu = num(opts.muSg, 0);
  // ~1 SG:APP ≈ +0.55–0.7 GIR historically; T2G adds approach+ott context.
  const sgHits =
    courseHits +
    0.62 * dApp +
    0.18 * dT2g -
    0.08 * dPutt + // strong putters often miss more GIR for birdie looks; small fade
    0.06 * mu;

  let skillHits = rateHits;
  if (Number.isFinite(rateHits) && Number.isFinite(sgHits)) {
    skillHits = 0.52 * rateHits + 0.48 * sgHits;
  } else if (Number.isFinite(sgHits)) {
    skillHits = sgHits;
  } else if (!Number.isFinite(skillHits)) {
    skillHits = courseHits + 0.55 * (tourRate * nGir - courseHits) + 0.5 * dApp;
  }

  const keep = clamp(num(opts.girSkillSpreadKeep, w.spreadKeep), 0.88, 0.98);
  return clamp(courseHits + keep * (skillHits - courseHits), 6, 16.5);
}

/**
 * Fairways: course baseline + driving accuracy / SG:OTT skill blend.
 */
export function optimizedFairwayCount(opts = {}) {
  const hist = opts.histCountFit || null;
  const w = blendWeightsFromHistCalib(hist).fairways;
  const sk = opts.skRow || {};
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const venueFw = num(opts.venueFairways, 9);
  const courseRate = num(opts.courseFairwayRate01, NaN);
  const courseHits = Number.isFinite(courseRate) ? courseRate * nFw : venueFw;

  const playerRate01 = num(opts.playerFwRate01, NaN);
  const career = num(sk.avg_fairways, NaN);
  const rateHits = Number.isFinite(playerRate01)
    ? playerRate01 * nFw
    : Number.isFinite(career)
      ? career
      : NaN;

  const dOtt = num(opts.sgOttDelta, 0);
  const dApp = num(opts.sgAppDelta, 0);
  const mu = num(opts.muSg, 0);
  // SG:OTT is the primary FW skill; mild APP correlation on tight courses.
  const sgHits = courseHits + 0.95 * dOtt + 0.12 * dApp + 0.05 * mu;

  let skillHits = rateHits;
  if (Number.isFinite(rateHits) && Number.isFinite(sgHits)) {
    skillHits = 0.55 * rateHits + 0.45 * sgHits;
  } else if (Number.isFinite(sgHits)) {
    skillHits = sgHits;
  } else if (!Number.isFinite(skillHits)) {
    skillHits = courseHits + 0.9 * dOtt;
  }

  const keep = clamp(num(opts.fairwaySkillSpreadKeep, w.spreadKeep), 0.85, 0.98);
  return clamp(courseHits + keep * (skillHits - courseHits), 2, nFw + 0.5);
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
  const venueEag = num(opts.venueEagles, 0.12);
  const venueMarket = venueBird + venueEag;
  const venueBog = num(opts.venueBog, 2.6);
  const venueDbl = num(opts.venueDoubles, 0.32);
  const venueBogMarket = venueBog + venueDbl;

  const dApp = num(opts.sgAppDelta, 0);
  const dPutt = num(opts.sgPuttDelta, 0);
  const dArg = num(opts.sgArgDelta, 0);
  const dOtt = num(opts.sgOttDelta, 0);
  const fieldGir = num(opts.fieldGir, 12);
  const playerGir = num(sk.avg_gir, NaN);
  const girMiss = Number.isFinite(playerGir) ? fieldGir - playerGir : 0;

  const birdRate = num(sk.avg_birdies, NaN);
  // avg_bogeys is bogey-or-worse (bogeys + doubles), same convention as avg_birdies.
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
    { w: wBird.wVenue, v: venueMarket },
    { w: wBird.wRate, v: birdRate },
    { w: wBird.wOls, v: olsCountAtStp(hist, "birdies", stp) },
  ]);
  const bogCore = weightedMean([
    { w: wBog.wVenue, v: venueBogMarket },
    { w: wBog.wRate, v: bogRate },
    { w: wBog.wOls, v: olsCountAtStp(hist, "bogeys", stp) },
  ]);

  let birdies = Number.isFinite(birdCore) ? birdCore + wBird.wSg * birdSg : birdSg + venueMarket;
  let bogeys = Number.isFinite(bogCore) ? bogCore + wBog.wSg * bogSg : bogSg + venueBogMarket;

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

  const birdSpread = num(opts.birdieSkillSpreadKeep, wBird.spreadKeep);
  const birdMarket = birdies + eagles;
  if (Number.isFinite(birdMarket) && Number.isFinite(venueMarket)) {
    const anchored = venueMarket + birdSpread * (birdMarket - venueMarket);
    birdies = Math.max(0.15, anchored - eagles);
  } else if (Number.isFinite(birdies) && Number.isFinite(venueBird)) {
    birdies = venueBird + birdSpread * (birdies - venueBird);
  }

  // bogeys variable is bogey-or-worse market; split doubles so score identity stays valid.
  const bogSpread = num(opts.bogeySkillSpreadKeep, wBog.spreadKeep ?? 0.75);
  let bogMarket = bogeys;
  if (Number.isFinite(bogMarket) && Number.isFinite(venueBogMarket)) {
    bogMarket = venueBogMarket + bogSpread * (bogMarket - venueBogMarket);
  }
  // Light score anchor only — hard courses already lift venueBogMarket.
  const scoreBogMkt = clamp(venueBogMarket + stp * 0.32, 0.15, 8.5);
  bogMarket = (1 - wBog.wScoreStp) * bogMarket + wBog.wScoreStp * scoreBogMkt;
  bogMarket = clamp(bogMarket, 0.35, 7.2);
  doubles = clamp(num(doubles, venueDbl), 0.04, 2.2);
  bogeys = Math.max(0.15, bogMarket - doubles);

  return { eagles, birdies, bogeys, doubles, nHist };
}
