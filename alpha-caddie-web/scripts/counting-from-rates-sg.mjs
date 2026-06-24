/**
 * Counting markets from player rolling rates + SG categories (no score-to-par derive).
 */
import { createReadStream, existsSync } from "fs";
import { parse } from "csv-parse";
import {
  DG_TOUR_AVG_FAIRWAY_RATE,
  DG_TOUR_AVG_GIR_RATE,
  fairwayHitsFromRate01,
  fairwayRate01FromDg,
  girHitsFromRate01,
  girRate01FromDg,
  girRate01FromSgApp,
  num,
  traditionalRate01,
} from "./dg-traditional-stats.mjs";

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

/**
 * Birdies / bogeys / eagles / doubles from rolling rates + SG; pars is always residual to 18 holes.
 */
export function holeCountsFromRatesAndSg(opts = {}) {
  const mu = num(opts.muSg, 0);
  const stp = -mu;
  const sk = opts.skRow || {};
  const field = opts.fieldMeans || {};
  const venueBird = num(opts.venueBird, 3.8);
  const venueBog = num(opts.venueBog, 2.6);
  const venueEag = num(opts.venueEagles, 0.12);
  const venueDbl = num(opts.venueDoubles, 0.32);
  const nHist = Math.round(num(sk.counting_rounds, 0)) || 0;
  const histCountFit = opts.histCountFit || null;
  const shrink = histFitShrink(histCountFit);

  let birdBase = histBlend(num(sk.avg_birdies, NaN), venueBird, nHist, 10);
  let bogBase = histBlend(num(sk.avg_bogeys, NaN), venueBog, nHist, 7);
  let eagles = histBlend(num(sk.avg_eagles, NaN), venueEag, nHist);
  let doubles = histBlend(num(sk.avg_doubles, NaN), venueDbl, nHist);
  const dblExcess = Math.max(0, doubles - venueDbl);

  const dApp = sgDelta(sk, field, "sg_app");
  const dPutt = sgDelta(sk, field, "sg_putt");
  const dArg = sgDelta(sk, field, "sg_arg");
  const dOtt = sgDelta(sk, field, "sg_ott");

  const fieldGir = num(opts.fieldGir ?? opts.venueGir, 12);
  const playerGir = num(sk.avg_gir, NaN);
  const girMiss = Number.isFinite(playerGir) ? fieldGir - playerGir : 0;

  let birdies = birdBase + 0.68 * dApp + 0.45 * dPutt + 0.08 * mu + 0.06 * dOtt;
  let bogeys =
    bogBase +
    0.55 * (-dArg) +
    0.22 * (-dApp) +
    0.32 * (-mu) +
    0.1 * (-dPutt) +
    0.12 * girMiss +
    0.08 * (-dOtt) +
    0.14 * dblExcess;
  eagles += 0.35 * dApp + 0.2 * dOtt + 0.12 * Math.max(0, mu);
  doubles += 0.42 * (-dArg) + 0.2 * Math.max(0, -mu) + 0.1 * (-dApp);

  if (shrink > 0) {
    const olsBird = olsCountFromHistFit(histCountFit, "birdies", stp);
    const olsBog = olsCountFromHistFit(histCountFit, "bogeys", stp);
    const olsEag = olsCountFromHistFit(histCountFit, "eagles", stp);
    const olsDbl = olsCountFromHistFit(histCountFit, "doubles", stp);
    if (Number.isFinite(olsBird)) birdies = shrink * olsBird + (1 - shrink) * birdies;
    if (Number.isFinite(olsBog)) bogeys = shrink * olsBog + (1 - shrink) * bogeys;
    if (Number.isFinite(olsEag)) eagles = shrink * olsEag + (1 - shrink) * eagles;
    if (Number.isFinite(olsDbl)) doubles = shrink * olsDbl + (1 - shrink) * doubles;
  }

  const scoreBog = clamp(venueBog + stp * 0.56, 0.15, 8.5);
  const thinW = nHist <= 4 ? 0.2 : nHist <= 10 ? 0.08 : 0;
  const bogScoreW = Math.min(0.28, thinW + 0.04);
  bogeys = (1 - bogScoreW) * bogeys + bogScoreW * scoreBog;

  return finalizeHoleCounts(eagles, birdies, bogeys, doubles);
}

/** GIR count from DG GIR% + SG:APP (no score fallback). */
export function girFromRatesAndSg(opts = {}) {
  const mu = num(opts.muSg, 0);
  const sk = opts.skRow || {};
  const liveTrad = opts.liveTrad ?? null;
  const nGir = num(opts.nGirHoles, 18);
  const venueGir = num(opts.venueGir, 12);
  const nHist = Math.round(num(sk.counting_rounds, 0)) || 0;

  let rate01 = girRate01FromDg(sk, liveTrad, { muSg: mu, fieldMeanApp: opts.fieldMeanApp });
  if (!Number.isFinite(rate01)) {
    const histGir = num(sk.avg_gir, NaN);
    if (Number.isFinite(histGir) && histGir > 4) {
      rate01 = clamp(histGir / nGir, 0.35, 0.85);
    }
  }
  if (!Number.isFinite(rate01)) {
    rate01 = girRate01FromSgApp(mu, sk.sg_app, opts.fieldMeanApp, DG_TOUR_AVG_GIR_RATE);
  }
  if (!Number.isFinite(rate01)) rate01 = clamp(venueGir / nGir, 0.4, 0.8);

  let gir = girHitsFromRate01(rate01, nGir);
  if (Number.isFinite(sk.avg_gir, NaN) && nHist >= 4) {
    gir = histBlend(sk.avg_gir, gir, nHist, 16);
  }
  const dApp = sgDelta(sk, opts.fieldMeans || {}, "sg_app");
  gir += 0.55 * dApp + 0.06 * mu;
  return Math.round(clamp(gir, 6, 16) * 100) / 100;
}

/** Anchor DG fairway rate to course layout + venue (elite fields inflate vs narrow courses). */
function fairwayAnchorRate01(opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const venueRate = clamp(num(opts.venueFairways, 9) / nFw, 0.35, 0.75);
  const courseRate = num(opts.courseFairwayRate01, NaN);
  if (Number.isFinite(courseRate)) {
    return clamp(0.35 * venueRate + 0.65 * clamp(courseRate, 0.35, 0.75), 0.35, 0.75);
  }
  return venueRate;
}

/** Field-mean calibration target: course-table driving accuracy × FW holes when available. */
export function fairwayCalibrationTargetMean(opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const venue = num(opts.venueAvgFairways, NaN);
  const courseCount = num(opts.courseFairwayRate01, NaN) * nFw;
  if (Number.isFinite(courseCount)) {
    let t = courseCount;
    // Venue CSV avg often runs hot vs actual event FW counts; nudge toward historical pace.
    if (Number.isFinite(venue) && venue - courseCount >= 0.15) {
      t = courseCount - 0.14;
    }
    return Math.round(Math.max(8.5, t) * 100) / 100;
  }
  return venue;
}

/** Shift fairways so field mean matches course/venue target (preserves player spread). */
export function calibrateFairwayFieldMean(players, opts = {}) {
  const round = opts.round;
  const target = fairwayCalibrationTargetMean(opts);
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const cap = nFw + 0.5;
  const minDelta = num(opts.minDelta, 0.1);
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

/** Fairways count from DG accuracy% + SG:OTT (no score fallback). */
export function fairwaysFromRatesAndSg(opts = {}) {
  const mu = num(opts.muSg, 0);
  const sk = opts.skRow || {};
  const liveTrad = opts.liveTrad ?? null;
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const venueFw = num(opts.venueFairways, 9);
  const nHist = Math.round(num(sk.counting_rounds, 0)) || 0;

  let rate01 = fairwayRate01FromDg(sk, liveTrad, nFw);
  if (!Number.isFinite(rate01)) {
    const histFw = num(sk.avg_fairways, NaN);
    if (Number.isFinite(histFw) && histFw > 1) {
      rate01 = clamp(histFw / nFw, 0.28, 0.88);
    }
  }
  if (!Number.isFinite(rate01)) {
    const dOtt = sgDelta(sk, opts.fieldMeans || {}, "sg_ott");
    rate01 = clamp(DG_TOUR_AVG_FAIRWAY_RATE + 0.72 * dOtt + 0.08 * mu, 0.28, 0.88);
  }
  if (!Number.isFinite(rate01)) rate01 = clamp(venueFw / nFw, 0.35, 0.75);

  const anchor = fairwayAnchorRate01({
    nFairwayHoles: nFw,
    venueFairways: venueFw,
    courseFairwayRate01: num(opts.courseFairwayRate01, NaN),
  });
  const spreadKeep = num(opts.fairwayCourseSpreadKeep, 0.24);
  if (Number.isFinite(rate01) && Number.isFinite(anchor)) {
    rate01 = anchor + spreadKeep * (rate01 - anchor);
  }

  let fairways = fairwayHitsFromRate01(rate01, nFw);
  if (Number.isFinite(sk.avg_fairways, NaN) && nHist >= 4) {
    fairways = histBlend(sk.avg_fairways, fairways, nHist, 16);
  }
  const dOtt = sgDelta(sk, opts.fieldMeans || {}, "sg_ott");
  fairways += 0.22 * dOtt + 0.025 * mu;
  return Math.round(clamp(fairways, 2, nFw + 0.5) * 100) / 100;
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
    fieldGir: opts.venueGir,
  });

  const girOpts = {
    muSg: mu_sg,
    skRow: skR,
    liveTrad,
    fieldMeanApp: opts.fieldMeanApp,
    fieldMeans,
    venueGir: opts.venueGir,
    nGirHoles: opts.nGirHoles ?? 18,
  };
  const gir = girFromRatesAndSg(girOpts);

  const fairways = fairwaysFromRatesAndSg({
    muSg: mu_sg,
    skRow: skR,
    liveTrad,
    fieldMeans,
    nFairwayHoles,
    venueFairways: opts.venueFairways,
    courseFairwayRate01: opts.courseFairwayRate01,
    fairwayCourseSpreadKeep: opts.fairwayCourseSpreadKeep,
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
  if (Number.isFinite(rates.avg_gir)) {
    skRow.dg_gir_pct = clamp(rates.avg_gir / 18, 0.35, 0.85);
  }
  if (Number.isFinite(rates.avg_fairways)) {
    const nFw = 14;
    skRow.dg_fairway_pct = clamp(rates.avg_fairways / nFw, 0.28, 0.88);
  }
  return skRow;
}
