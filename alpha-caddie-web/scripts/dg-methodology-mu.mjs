/**
 * DataGolf predictive-model methodology for round O/U μ.
 *
 * Based on https://datagolf.com/predictive-model-methodology/
 *   - Sequence ⊕ calendar exponential decay on adjusted residuals
 *   - Sample-size regression to the mean
 *   - SG-category reweight (OTT > APP > ARG > PUTT)
 *   - Shrunk course fit (dist / acc / APP / ARG / PUTT random-effects style)
 *   - Course / year / round / wave effects + weather
 *
 * Lifted from the MAE bake-off stack (compare-mae-round-oos.mjs).
 *
 *   GOLF_DG_METHODOLOGY=1
 */
import { join } from "path";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import { resolveCourseLayout } from "./course-hole-layout.mjs";
import { statWeatherMuAdjustment } from "./weather-mu-adjustments.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";
import { num } from "./round-projection-mu.mjs";

/** `num` from round-projection-mu has no fallback; wrappers need one. */
function numOr(v, fallback) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

export const LAMBDA_SEQ = -Math.log(0.3) / 50; // ~70% weight on last ~50 rounds (DG mid-2021)
export const LAMBDA_TIME = Math.log(2) / 150; // ~150-day half-life
export const SEQ_SHARE = 0.55;
/**
 * Mild within-event form carry (DG updates R2 skill ~0.1–0.3 SG from R1, not a rewrite).
 * Prior 2.4× made a single 0-birdie R1 crush Birdies μ to ~2.0 — not DG methodology.
 */
export const SAME_EVENT_BOOST = 1.28;
export const CAT_MIX = 0.28;
/** DG-ish category predictive betas (OTT > APP > ARG > PUTT). */
export const CAT_BETA = { ott: 1.15, app: 1.0, arg: 0.88, putt: 0.55 };
export const FIT_K = 90;
const N_FW = 14;

export const DG_MARKETS = ["Total score", "Birdies", "Bogeys", "Pars", "GIR", "Fairways hit"];

export const K = {
  // Higher player K = stronger RTM (DG: thin samples → tour; hot unknowns must not outrank elites).
  "Total score": { course: 8, year: 5, round: 8, wave: 12, player: 32, pc: 28, hole: 18 },
  Birdies: { course: 12, year: 6, round: 10, wave: 14, player: 36, pc: 28, hole: 20 },
  Bogeys: { course: 12, year: 6, round: 10, wave: 14, player: 36, pc: 28, hole: 20 },
  Pars: { course: 10, year: 5, round: 8, wave: 12, player: 28, pc: 22, hole: 18 },
  GIR: { course: 10, year: 5, round: 8, wave: 12, player: 26, pc: 20, hole: 18 },
  "Fairways hit": { course: 8, year: 4, round: 8, wave: 12, player: 18, pc: 16, hole: 16 },
};

/** Birdies: rolling BoB + course spread-keep (both-side+ bake). */
export const BIRDIE_BOB_WINDOW = 40;
export const BIRDIE_COURSE_SPREAD_KEEP = 0.42;
export const BIRDIE_PLAYER_COURSE_MIN = 4;
export const BIRDIE_PLAYER_COURSE_MAX_W = 0.45;
/** Blend residual DG Birdies μ with BoB path (1 = pure BoB). Tuned for both-side+. */
export const BIRDIE_BOB_BLEND = 0.5;

/** Fairways: rolling accuracy count + course spread-keep (both-side+ bake). */
export const FAIRWAY_ACC_WINDOW = 40;
export const FAIRWAY_COURSE_SPREAD_KEEP = 0.55;
/** Pure driving-acc path for Fairways (DG residual was both-side−). */
export const FAIRWAY_ACC_BLEND = 1.0;

/** GIR / Bogeys / Pars: same level+course pattern (OOS + both-side). */
export const GIR_LEVEL_WINDOW = 40;
export const GIR_COURSE_SPREAD_KEEP = 0.55;
export const GIR_LEVEL_BLEND = 1.0;

export const BOGEY_LEVEL_WINDOW = 40;
export const BOGEY_COURSE_SPREAD_KEEP = 0.42;
export const BOGEY_LEVEL_BLEND = 0.4;

export const PARS_LEVEL_WINDOW = 40;
export const PARS_COURSE_SPREAD_KEEP = 0.45;
/** Pure rolling-pars level broke both-side (under leak); keep off. */
export const PARS_LEVEL_BLEND = 0;
/**
 * Pars "par-machine": rolling good SG:OTT × poor SG:PUTT on a course level anchor.
 * Geometric interaction (same idea as applyT2gPuttParShape) — both legs required.
 * Same-round OTTH×PUTTL ≈ +0.8 pars vs OTTL×PUTTH; skill estimates carry a milder signal.
 */
export const PARS_OTT_PUTT_WINDOW = 50;
export const PARS_OTT_PUTT_SCALE = 2.2;
export const PARS_OTT_PUTT_ANTI = 0.55;
/** Blend residual DG Pars μ with par-machine path (1 = pure OTT×PUTT). */
export const PARS_PAR_MACHINE_BLEND = 0.75;
/** After markets settle, pull Pars toward 18 − Birdies − Bogeys (BoB / bogey-or-worse). */
export const PARS_IDENTITY_BLEND = 0.35;

/** Total score: mild level blend + actual-bias trim (model was ~+0.72 high). */
export const SCORE_LEVEL_WINDOW = 40;
export const SCORE_COURSE_SPREAD_KEEP = 0.55;
export const SCORE_LEVEL_BLEND = 0.35;
export const SCORE_BIAS_TRIM = 0.7;

export function dgMethodologyEnabled() {
  const v = String(process.env.GOLF_DG_METHODOLOGY || "").trim().toLowerCase();
  return v === "1" || v === "true" || v === "yes" || v === "on";
}

export function dgMethodologyPipelineEnv() {
  return {
    GOLF_DG_METHODOLOGY: "1",
    GOLF_STRICT_FIT_FORM: "0",
    GOLF_MARKET_BOOK_CALIBRATION: "0",
    GOLF_OUTCOME_MU_DEBIAS: "0",
    GOLF_EXPORT_RAW_MODEL_MU: "1",
    GOLF_WF_WEATHER: "1",
    // Disable kitchen-sink overlays — DG stack owns venue/fit/history.
    GOLF_COURSE_SG_FIT: "0",
    GOLF_HOLE_SG_BLEND: "0",
    GOLF_DISTANCE_SG_BLEND: "0",
    GOLF_COURSE_PRIOR_ROUND_DIFFICULTY: "0",
    GOLF_WITHIN_EVENT_FORM_CARRY: "0",
    GOLF_FIELD_DAY_COUNTING_LIFT_FRAC: "0",
    GOLF_WITHIN_EVENT_COUNTING_BLEND: "0",
  };
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}
function shrink(n, k) {
  return Math.max(0, n) / (Math.max(0, n) + k);
}
function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
}
function completedMs(row) {
  const s = String(row?.event_completed || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) {
    return Date.parse(
      `${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`,
    );
  }
  return parseMs(s);
}

/**
 * Hist CSVs stamp every round with Sunday `event_completed`. Map R1–R4 onto Thu–Sun
 * so walk-forward cutoffs / calendar decay match when each round was actually known.
 */
function roundCompletedMs(row) {
  const eventEnd = completedMs(row);
  const rnd = Math.round(num(row?.round_num ?? row?.round, NaN));
  if (!Number.isFinite(eventEnd) || eventEnd <= 0) return eventEnd;
  if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return eventEnd;
  return eventEnd - (4 - rnd) * 86400000;
}
function girFwCount(raw, nHoles) {
  const v = num(raw, NaN);
  if (!Number.isFinite(v)) return NaN;
  if (v > 0 && v <= 1.0001) return v * nHoles;
  if (v > 1 && v <= nHoles + 0.51) return v;
  if (v > nHoles && v <= 100) return (v / 100) * nHoles;
  return NaN;
}
function placeholderCounts(row) {
  const b = num(row.birdies, NaN);
  const p = num(row.pars, NaN);
  const bg = num(row.bogeys ?? row.bogies, NaN);
  return b === 0 && bg === 0 && (!Number.isFinite(p) || p === 0 || p >= 10);
}

export function marketVal(market, row, fairwayHoles = N_FW) {
  if (market === "Total score") {
    const rs = num(row.round_score, NaN);
    return Number.isFinite(rs) && rs >= 55 && rs <= 95 ? rs : NaN;
  }
  if (placeholderCounts(row)) return NaN;
  if (market === "Birdies") {
    const b = num(row.birdies, NaN);
    if (!Number.isFinite(b) || b < 0 || b > 18) return NaN;
    const e = num(row.eagles_or_better ?? row.eagles, 0);
    return b + Math.max(0, Number.isFinite(e) ? e : 0);
  }
  if (market === "Bogeys") {
    const bg = num(row.bogeys ?? row.bogies, NaN);
    if (!Number.isFinite(bg) || bg < 0 || bg > 18) return NaN;
    const d = num(row.doubles_or_worse ?? row.doubles, 0);
    return bg + Math.max(0, Number.isFinite(d) ? d : 0);
  }
  if (market === "Pars") {
    const p = num(row.pars, NaN);
    return Number.isFinite(p) && p >= 0 && p <= 18 ? p : NaN;
  }
  if (market === "GIR") return girFwCount(row.gir, 18);
  if (market === "Fairways hit") {
    const raw = Number.isFinite(num(row.driving_acc, NaN)) ? row.driving_acc : row.fairways;
    return girFwCount(raw, fairwayHoles);
  }
  return NaN;
}

function clampMu(market, mu, par = 72, fairwayHoles = N_FW) {
  if (!Number.isFinite(mu)) return NaN;
  const p = Number.isFinite(par) && par >= 68 && par <= 73 ? par : 72;
  if (market === "Total score") return clamp(mu, p - 8, p + 14);
  if (market === "Birdies") return clamp(mu, 0.4, 10);
  if (market === "Bogeys") return clamp(mu, 0.5, 12);
  if (market === "Pars") return clamp(mu, 4, 16);
  if (market === "GIR") return clamp(mu, 5, 16.5);
  if (market === "Fairways hit") return clamp(mu, 2, fairwayHoles + 0.5);
  return mu;
}

function wmean(items) {
  let sw = 0;
  let sx = 0;
  for (const it of items) {
    if (!Number.isFinite(it.v) || !(it.w > 0)) continue;
    sw += it.w;
    sx += it.w * it.v;
  }
  return sw > 0 ? { mean: sx / sw, n: sw } : { mean: NaN, n: 0 };
}

function neff(weights) {
  let s = 0;
  let s2 = 0;
  for (const w of weights) {
    if (!(w > 0)) continue;
    s += w;
    s2 += w * w;
  }
  return s2 > 0 ? (s * s) / s2 : 0;
}

export function decayWeight(roundsAgo, daysAgo, sameEventEarlier) {
  const seq = Math.exp(-LAMBDA_SEQ * Math.max(0, roundsAgo));
  const tim = Math.exp(-LAMBDA_TIME * Math.max(0, daysAgo));
  let w = SEQ_SHARE * seq + (1 - SEQ_SHARE) * tim;
  if (sameEventEarlier) w *= SAME_EVENT_BOOST;
  return w;
}

/** Convert raw hist CSV-like rows into typed DG rows. */
export function typeHistRows(histRows, fairwayHoles = N_FW) {
  /** @type {object[]} */
  const rows = [];
  for (const r of histRows || []) {
    const dg = Math.round(num(r.dg_id, NaN));
    const ck = normCourseNameKey(r.course_name || "");
    if (!Number.isFinite(dg) || !ck) continue;
    const t = roundCompletedMs(r);
    const par = num(r.course_par, NaN);
    const vals = {};
    let any = false;
    for (const m of DG_MARKETS) {
      const v = marketVal(m, r, fairwayHoles);
      vals[m] = v;
      if (Number.isFinite(v)) any = true;
    }
    if (!any) continue;
    const wave = teeWaveFromTeetimeAndLabel(r.teetime ?? r.tee_time, r.dg_tee_wave);
    const startHole = Math.round(num(r.start_hole, NaN));
    const accRaw = num(r.driving_acc, NaN);
    let acc = NaN;
    if (Number.isFinite(accRaw)) {
      if (accRaw > 0 && accRaw <= 1.0001) acc = accRaw * 100;
      else if (accRaw > 1 && accRaw <= 100) acc = accRaw;
    }
    rows.push({
      dg,
      t: Number.isFinite(t) ? t : 0,
      ck,
      par: Number.isFinite(par) && par >= 68 && par <= 73 ? par : 72,
      event: String(r.event_name || "").trim(),
      year: Math.round(num(r.year, NaN)),
      round: Math.round(num(r.round_num ?? r.round, NaN)),
      vals,
      sg: {
        total: num(r.sg_total, NaN),
        ott: num(r.sg_ott, NaN),
        app: num(r.sg_app, NaN),
        arg: num(r.sg_arg, NaN),
        putt: num(r.sg_putt, NaN),
      },
      dist: num(r.driving_dist, NaN),
      acc,
      wave,
      startHole: startHole === 1 || startHole === 10 ? startHole : NaN,
    });
  }
  rows.sort((a, b) => a.t - b.t || a.year - b.year || a.round - b.round);
  return rows;
}

export function prefixBefore(hist, cutoffMs, eventName, targetRound, eventYear) {
  const out = [];
  for (const r of hist) {
    if (r.t >= cutoffMs && !(r.t === 0 && cutoffMs > 1e11)) {
      const sameEv = eventsLikelySame(r.event, eventName);
      const sameYr = !Number.isFinite(eventYear) || !Number.isFinite(r.year) || r.year === eventYear;
      if (!(sameEv && sameYr && Number.isFinite(r.round) && r.round < targetRound)) continue;
    } else if (eventsLikelySame(r.event, eventName)) {
      const sameYr = !Number.isFinite(eventYear) || !Number.isFinite(r.year) || r.year === eventYear;
      if (sameYr && Number.isFinite(r.round) && r.round >= targetRound) continue;
    }
    out.push(r);
  }
  return out;
}

export function effectsAtCutoff(histPrefix, market, cutoffMs, eventName, eventYear, targetRound = NaN) {
  const kk = K[market] || K["Total score"];
  let tourSum = 0;
  let tourN = 0;
  /** @type {Map<string, { sum: number, n: number }>} */
  const course = new Map();
  for (const r of histPrefix) {
    const v = r.vals[market];
    if (!Number.isFinite(v)) continue;
    tourSum += v;
    tourN++;
    const c = course.get(r.ck) || { sum: 0, n: 0 };
    c.sum += v;
    c.n++;
    course.set(r.ck, c);
  }
  const tour = tourN ? tourSum / tourN : NaN;
  /** @type {Map<string, number>} */
  const courseEff = new Map();
  for (const [ck, c] of course) courseEff.set(ck, shrink(c.n, kk.course) * (c.sum / c.n - tour));

  /** @type {Map<string, { sum: number, n: number }>} */
  const courseYear = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseRound = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseWave = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseHole = new Map();

  for (const r of histPrefix) {
    const v = r.vals[market];
    if (!Number.isFinite(v)) continue;
    const ce = courseEff.get(r.ck) || 0;
    const resid = v - tour - ce;
    const yk = `${r.ck}|${r.year}`;
    const y = courseYear.get(yk) || { sum: 0, n: 0 };
    y.sum += resid;
    y.n++;
    courseYear.set(yk, y);
    if (Number.isFinite(r.round) && r.round >= 1 && r.round <= 4) {
      const rk = `${r.ck}|${r.round}`;
      const rr = courseRound.get(rk) || { sum: 0, n: 0 };
      rr.sum += resid;
      rr.n++;
      courseRound.set(rk, rr);
    }
    if (r.wave === "morning" || r.wave === "afternoon") {
      const wk = `${r.ck}|${r.wave}`;
      const w = courseWave.get(wk) || { sum: 0, n: 0 };
      w.sum += resid;
      w.n++;
      courseWave.set(wk, w);
    }
    if (r.startHole === 1 || r.startHole === 10) {
      const hk = `${r.ck}|${r.startHole}`;
      const h = courseHole.get(hk) || { sum: 0, n: 0 };
      h.sum += resid;
      h.n++;
      courseHole.set(hk, h);
    }
  }
  /** @type {Map<string, number>} */
  const yearEff = new Map();
  for (const [k, x] of courseYear) yearEff.set(k, shrink(x.n, kk.year) * (x.sum / x.n));
  /** @type {Map<string, number>} */
  const roundEff = new Map();
  for (const [k, x] of courseRound) roundEff.set(k, shrink(x.n, kk.round) * (x.sum / x.n));
  /** @type {Map<string, number>} */
  const waveEff = new Map();
  for (const [k, x] of courseWave) waveEff.set(k, shrink(x.n, kk.wave) * (x.sum / x.n));
  /** @type {Map<string, number>} */
  const holeEff = new Map();
  for (const [k, x] of courseHole) holeEff.set(k, shrink(x.n, kk.hole) * (x.sum / x.n));

  /** @type {Map<number, object[]>} */
  const byPlayer = new Map();
  for (const r of histPrefix) {
    if (!Number.isFinite(r.vals[market])) continue;
    const arr = byPlayer.get(r.dg) || [];
    arr.push(r);
    byPlayer.set(r.dg, arr);
  }

  /** @type {Map<number, object>} */
  const playerSkill = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const pc = new Map();

  for (const [dg, rounds] of byPlayer) {
    rounds.sort((a, b) => a.t - b.t || a.round - b.round);
    const items = [];
    const sgItems = [];
    const distItems = [];
    const accItems = [];
    const ottItems = [];
    const appItems = [];
    const argItems = [];
    const puttItems = [];
    const ws = [];
    for (let i = 0; i < rounds.length; i++) {
      const r = rounds[i];
      const v = r.vals[market];
      const ce = courseEff.get(r.ck) || 0;
      const ye = yearEff.get(`${r.ck}|${r.year}`) || 0;
      const re = roundEff.get(`${r.ck}|${r.round}`) || 0;
      const we = r.wave ? waveEff.get(`${r.ck}|${r.wave}`) || 0 : 0;
      const he = Number.isFinite(r.startHole) ? holeEff.get(`${r.ck}|${r.startHole}`) || 0 : 0;
      const resid = v - tour - ce - ye - re - we - he;
      const roundsAgo = rounds.length - 1 - i;
      const daysAgo =
        Number.isFinite(cutoffMs) && r.t > 0 ? (cutoffMs - r.t) / 86400000 : roundsAgo * 7;
      const sameEvEarlier =
        eventsLikelySame(r.event, eventName) &&
        (!Number.isFinite(eventYear) || r.year === eventYear) &&
        Number.isFinite(r.round) &&
        Number.isFinite(targetRound) &&
        r.round < targetRound;
      const w = decayWeight(roundsAgo, Math.max(0, daysAgo), sameEvEarlier);
      items.push({ v: resid, w });
      ws.push(w);
      if (Number.isFinite(r.sg.total)) sgItems.push({ v: r.sg.total, w });
      if (Number.isFinite(r.dist) && r.dist > 230 && r.dist < 380) distItems.push({ v: r.dist, w });
      if (Number.isFinite(r.acc)) accItems.push({ v: r.acc, w });
      if (Number.isFinite(r.sg.ott)) ottItems.push({ v: r.sg.ott, w });
      if (Number.isFinite(r.sg.app)) appItems.push({ v: r.sg.app, w });
      if (Number.isFinite(r.sg.arg)) argItems.push({ v: r.sg.arg, w });
      if (Number.isFinite(r.sg.putt)) puttItems.push({ v: r.sg.putt, w });
      const pk = `${dg}|${r.ck}`;
      const x = pc.get(pk) || { sum: 0, n: 0 };
      x.sum += resid * w;
      x.n += w;
      pc.set(pk, x);
    }
    const wm = wmean(items);
    const nE = neff(ws);
    const pe = shrink(nE, kk.player) * (Number.isFinite(wm.mean) ? wm.mean : 0);
    const sgW = wmean(sgItems);
    let sg = shrink(sgW.n, kk.player) * (Number.isFinite(sgW.mean) ? sgW.mean : 0);
    const ott = wmean(ottItems).mean;
    const app = wmean(appItems).mean;
    const arg = wmean(argItems).mean;
    const putt = wmean(puttItems).mean;
    if (market === "Total score" && [ott, app, arg, putt].every((x) => Number.isFinite(x))) {
      const cat =
        CAT_BETA.ott * ott + CAT_BETA.app * app + CAT_BETA.arg * arg + CAT_BETA.putt * putt;
      const catNorm = cat / ((CAT_BETA.ott + CAT_BETA.app + CAT_BETA.arg + CAT_BETA.putt) / 4);
      if (Number.isFinite(sgW.mean)) {
        sg = shrink(sgW.n, kk.player) * ((1 - CAT_MIX) * sgW.mean + CAT_MIX * catNorm);
      } else {
        sg = shrink(Math.min(sgW.n || 8, 20), kk.player) * catNorm;
      }
    }
    // Career establishment: DG RTM pulls unknowns toward tour mean (~0 SG). A 50-round
    // hot streak must not outrank a 400-round elite with similar recent form.
    const establish = shrink(rounds.length, 100);
    const TOUR_PRIOR_SG = 0;
    const TOUR_PRIOR_PE = 0;
    sg = establish * sg + (1 - establish) * TOUR_PRIOR_SG;
    const peEst = establish * pe + (1 - establish) * TOUR_PRIOR_PE;
    playerSkill.set(dg, {
      pe: peEst,
      sg,
      dist: wmean(distItems).mean,
      acc: wmean(accItems).mean,
      ott: Number.isFinite(ott) ? ott : 0,
      app: Number.isFinite(app) ? app : 0,
      arg: Number.isFinite(arg) ? arg : 0,
      putt: Number.isFinite(putt) ? putt : 0,
      n: nE,
      careerN: rounds.length,
    });
  }

  let distMu = 0;
  let distN = 0;
  let accMu = 0;
  let accN = 0;
  for (const s of playerSkill.values()) {
    if (Number.isFinite(s.dist)) {
      distMu += s.dist;
      distN++;
    }
    if (Number.isFinite(s.acc)) {
      accMu += s.acc;
      accN++;
    }
  }
  distMu = distN ? distMu / distN : 295;
  accMu = accN ? accMu / accN : 60;
  let distSd = 0;
  let accSd = 0;
  for (const s of playerSkill.values()) {
    if (Number.isFinite(s.dist)) distSd += (s.dist - distMu) ** 2;
    if (Number.isFinite(s.acc)) accSd += (s.acc - accMu) ** 2;
  }
  distSd = distN > 2 ? Math.sqrt(distSd / distN) : 12;
  accSd = accN > 2 ? Math.sqrt(accSd / accN) : 8;

  /** @type {Map<string, { xx: number[][], xy: number[], n: number }>} */
  const fitAcc = new Map();
  for (const r of histPrefix) {
    const v = r.vals[market];
    if (!Number.isFinite(v)) continue;
    const sk = playerSkill.get(r.dg);
    if (!sk || sk.n < 8) continue;
    const ce = courseEff.get(r.ck) || 0;
    const ye = yearEff.get(`${r.ck}|${r.year}`) || 0;
    const re = roundEff.get(`${r.ck}|${r.round}`) || 0;
    const y = v - tour - ce - ye - re - sk.pe;
    const distZ = Number.isFinite(sk.dist) ? (sk.dist - distMu) / distSd : 0;
    const accZ = Number.isFinite(sk.acc) ? (sk.acc - accMu) / accSd : 0;
    const x = [distZ, accZ, sk.app, sk.arg, sk.putt];
    const bucket =
      fitAcc.get(r.ck) ||
      { xx: [0, 0, 0, 0, 0].map(() => [0, 0, 0, 0, 0]), xy: [0, 0, 0, 0, 0], n: 0 };
    for (let i = 0; i < 5; i++) {
      bucket.xy[i] += x[i] * y;
      for (let j = 0; j < 5; j++) bucket.xx[i][j] += x[i] * x[j];
    }
    bucket.n++;
    fitAcc.set(r.ck, bucket);
  }
  /** @type {Map<string, number[]>} */
  const fitSlope = new Map();
  for (const [ck, b] of fitAcc) {
    const lam = FIT_K / Math.max(1, b.n);
    const slopes = [];
    for (let i = 0; i < 5; i++) {
      const denom = b.xx[i][i] + lam;
      slopes.push(denom > 1e-8 ? (b.xy[i] / denom) * shrink(b.n, FIT_K) : 0);
    }
    fitSlope.set(ck, slopes);
  }

  /** @type {Map<string, number>} */
  const pcEff = new Map();
  for (const [pk, x] of pc) {
    const dg = Number(pk.split("|")[0]);
    const pe = playerSkill.get(dg)?.pe || 0;
    const mean = x.n > 0 ? x.sum / x.n - pe : 0;
    pcEff.set(pk, shrink(x.n, kk.pc) * mean);
  }

  return {
    tour,
    courseEff,
    yearEff,
    roundEff,
    waveEff,
    holeEff,
    playerSkill,
    pcEff,
    fitSlope,
    distMu,
    distSd,
    accMu,
    accSd,
  };
}

export function predictDg(eff, row, weatherSnap, fairwayHoles = N_FW) {
  const market = row.market;
  const tour = eff?.tour;
  if (!Number.isFinite(tour)) return NaN;
  const ck = row.courseKey;
  const sk = eff.playerSkill.get(row.dg);
  const ce = eff.courseEff.get(ck) || 0;
  const ye = Number.isFinite(row.eventYear) ? eff.yearEff.get(`${ck}|${row.eventYear}`) || 0 : 0;
  const re = eff.roundEff.get(`${ck}|${row.round}`) || 0;
  const wave = row.wave || "";
  const we = wave ? eff.waveEff.get(`${ck}|${wave}`) || 0 : 0;
  const he = Number.isFinite(row.startHole) ? eff.holeEff.get(`${ck}|${row.startHole}`) || 0 : 0;
  const pe = sk?.pe || 0;
  const pce = eff.pcEff.get(`${row.dg}|${ck}`) || 0;
  let mu = tour + ce + ye + re + we + he + pe + pce;
  const slopes = eff.fitSlope.get(ck);
  if (slopes && sk) {
    const distZ = Number.isFinite(sk.dist) ? (sk.dist - eff.distMu) / (eff.distSd || 12) : 0;
    const accZ = Number.isFinite(sk.acc) ? (sk.acc - eff.accMu) / (eff.accSd || 8) : 0;
    const x = [distZ, accZ, sk.app, sk.arg, sk.putt];
    for (let i = 0; i < 5; i++) mu += (slopes[i] || 0) * x[i];
  }
  if (market === "Total score" && sk && Number.isFinite(sk.sg) && Math.abs(sk.sg) > 0) {
    // DG predicts adjusted SG first; map back to score. Weight SG path enough that
    // established ball-strikers outrank short-sample hot streaks on raw scores alone.
    const sgBridge = tour + ce + ye + re - sk.sg;
    const sgW = shrink(sk.n, 24);
    const w = 0.35 + 0.30 * sgW; // ~0.35 thin → ~0.65 full sample
    mu = (1 - w) * mu + w * sgBridge;
  }
  const wxRow = Number.isFinite(row.weatherRow?.weather_wind_mph)
    ? row.weatherRow
    : weatherSnap
      ? {
          weather_temp_f: weatherSnap.tempF,
          weather_wind_mph: weatherSnap.windMph,
          weather_humidity: weatherSnap.humidityPct,
          weather_condition: weatherSnap.condition,
        }
      : null;
  if (wxRow) mu += statWeatherMuAdjustment(market, wxRow);
  return clampMu(market, mu, row.par, fairwayHoles);
}

function wmeanLast(rows, market, win) {
  const vals = [];
  for (let i = rows.length - 1; i >= 0 && vals.length < win; i--) {
    const v = rows[i].vals?.[market];
    if (Number.isFinite(v)) vals.push(v);
  }
  if (!vals.length) return NaN;
  return vals.reduce((s, x) => s + x, 0) / vals.length;
}

/**
 * Level μ: rolling player count + course spread-keep (+ optional player@course).
 * Shared by Birdies / Fairways / GIR / Bogeys / Pars both-side+ paths.
 */
export function predictCountingLevel(histPrefix, dg, courseKey, market, opts = {}) {
  const win = Math.round(numOr(opts.window, 40)) || 40;
  const spread = numOr(opts.spreadKeep, 0.45);
  const usePc = opts.playerCourse !== false;
  const pcMin = Math.round(numOr(opts.playerCourseMin, BIRDIE_PLAYER_COURSE_MIN)) || BIRDIE_PLAYER_COURSE_MIN;
  const pcMaxW = numOr(opts.playerCourseMaxW, BIRDIE_PLAYER_COURSE_MAX_W);

  const playerRows = [];
  const courseRows = [];
  const pcRows = [];
  for (const r of histPrefix || []) {
    const v = r.vals?.[market];
    if (!Number.isFinite(v)) continue;
    if (r.dg === dg) {
      playerRows.push(r);
      if (r.ck === courseKey) pcRows.push(r);
    }
    if (r.ck === courseKey) courseRows.push(r);
  }
  const p = wmeanLast(playerRows, market, win);
  const c = wmeanLast(courseRows, market, 200);
  let mu = Number.isFinite(p) ? p : c;
  if (Number.isFinite(p) && Number.isFinite(c)) mu = c + spread * (p - c);
  if (usePc) {
    const pc = wmeanLast(pcRows, market, 12);
    let pcCount = 0;
    for (let i = pcRows.length - 1; i >= 0 && pcCount < 12; i--) {
      if (Number.isFinite(pcRows[i].vals?.[market])) pcCount++;
    }
    if (pcCount >= pcMin && Number.isFinite(pc) && Number.isFinite(mu)) {
      const w = Math.min(pcMaxW, pcCount / 12);
      mu = (1 - w) * mu + w * pc;
    }
  }
  return mu;
}

/**
 * Level BoB μ: rolling player birdies+eagles + course spread-keep + player@course.
 */
export function predictBirdiesBobLevel(histPrefix, dg, courseKey, opts = {}) {
  return predictCountingLevel(histPrefix, dg, courseKey, "Birdies", {
    window: numOr(opts.window, BIRDIE_BOB_WINDOW),
    spreadKeep: numOr(opts.spreadKeep, BIRDIE_COURSE_SPREAD_KEEP),
    playerCourse: true,
  });
}

/**
 * Level fairways μ from rolling driving-acc / FW counts + course spread-keep.
 */
export function predictFairwaysAccLevel(histPrefix, dg, courseKey, opts = {}) {
  return predictCountingLevel(histPrefix, dg, courseKey, "Fairways hit", {
    window: numOr(opts.window, FAIRWAY_ACC_WINDOW),
    spreadKeep: numOr(opts.spreadKeep, FAIRWAY_COURSE_SPREAD_KEEP),
    playerCourse: false,
  });
}

export function predictGirLevel(histPrefix, dg, courseKey, opts = {}) {
  return predictCountingLevel(histPrefix, dg, courseKey, "GIR", {
    window: numOr(opts.window, GIR_LEVEL_WINDOW),
    spreadKeep: numOr(opts.spreadKeep, GIR_COURSE_SPREAD_KEEP),
    playerCourse: true,
  });
}

export function predictBogeysLevel(histPrefix, dg, courseKey, opts = {}) {
  return predictCountingLevel(histPrefix, dg, courseKey, "Bogeys", {
    window: numOr(opts.window, BOGEY_LEVEL_WINDOW),
    spreadKeep: numOr(opts.spreadKeep, BOGEY_COURSE_SPREAD_KEEP),
    playerCourse: true,
  });
}

export function predictParsLevel(histPrefix, dg, courseKey, opts = {}) {
  return predictCountingLevel(histPrefix, dg, courseKey, "Pars", {
    window: numOr(opts.window, PARS_LEVEL_WINDOW),
    spreadKeep: numOr(opts.spreadKeep, PARS_COURSE_SPREAD_KEEP),
    playerCourse: true,
  });
}

function wmeanLastSg(playerRows, key, win) {
  const vals = [];
  for (let i = playerRows.length - 1; i >= 0 && vals.length < win; i--) {
    const v = playerRows[i].sg?.[key];
    if (Number.isFinite(v)) vals.push(v);
  }
  if (!vals.length) return NaN;
  return vals.reduce((s, x) => s + x, 0) / vals.length;
}

/**
 * Pars μ from course anchor + rolling good OTT × poor putting.
 * Classic par-machine: ball-striking looks that don't convert → pars.
 * Uses course mean (not player rolling pars — that path broke both-side unders).
 */
export function predictParsParMachine(histPrefix, dg, courseKey, opts = {}) {
  const win = Math.round(numOr(opts.window, PARS_OTT_PUTT_WINDOW)) || 50;
  const scale = numOr(opts.scale, PARS_OTT_PUTT_SCALE);
  const antiW = numOr(opts.anti, PARS_OTT_PUTT_ANTI);
  const base = courseRollingMean(histPrefix, courseKey, "Pars", 200);
  const tourFallback = (() => {
    const vals = [];
    for (let i = (histPrefix || []).length - 1; i >= 0 && vals.length < 400; i--) {
      const v = histPrefix[i].vals?.Pars;
      if (Number.isFinite(v)) vals.push(v);
    }
    return vals.length ? vals.reduce((s, x) => s + x, 0) / vals.length : 11.2;
  })();
  const anchor = Number.isFinite(base) ? base : tourFallback;

  const playerRows = [];
  for (const r of histPrefix || []) {
    if (r.dg === dg) playerRows.push(r);
  }
  const pOtt = wmeanLastSg(playerRows, "ott", win);
  // Putting mean-reverts faster — shorter window than OTT.
  const puttWin = Math.max(8, Math.round(win * 0.35));
  const pPutt = wmeanLastSg(playerRows, "putt", puttWin);
  if (!Number.isFinite(pOtt) || !Number.isFinite(pPutt)) return anchor;

  // SG categories are already field-relative (~0 tour mean).
  const ottGood = Math.max(0, pOtt);
  const puttPoor = Math.max(0, -pPutt);
  const ottPoor = Math.max(0, -pOtt);
  const puttGood = Math.max(0, pPutt);
  const shape = Math.sqrt(ottGood * puttPoor);
  const anti = Math.sqrt(ottPoor * puttGood);
  // Mild linear legs so one-sided profiles still move μ a bit.
  const linear = 0.35 * ottGood + 0.55 * puttPoor - 0.25 * ottPoor - 0.35 * puttGood;
  const adj = scale * (0.65 * clamp(shape, 0, 1.5) + 0.35 * clamp(linear, -1.2, 1.2) - antiW * 0.35 * clamp(anti, 0, 1.5));
  return anchor + adj;
}

export function predictScoreLevel(histPrefix, dg, courseKey, opts = {}) {
  return predictCountingLevel(histPrefix, dg, courseKey, "Total score", {
    window: numOr(opts.window, SCORE_LEVEL_WINDOW),
    spreadKeep: numOr(opts.spreadKeep, SCORE_COURSE_SPREAD_KEEP),
    playerCourse: true,
    playerCourseMaxW: 0.4,
  });
}

/** Shift field so mean(market) = target; preserves spreads. */
export function fieldCalibrateMuMap(byDg, market, target) {
  if (!Number.isFinite(target)) return;
  const vals = [];
  for (const mus of byDg.values()) {
    const v = mus.get(market);
    if (Number.isFinite(v)) vals.push(v);
  }
  if (vals.length < 2) return;
  const mean = vals.reduce((s, x) => s + x, 0) / vals.length;
  const delta = target - mean;
  if (!Number.isFinite(delta) || Math.abs(delta) < 1e-9) return;
  for (const mus of byDg.values()) {
    const v = mus.get(market);
    if (!Number.isFinite(v)) continue;
    mus.set(market, Math.round((v + delta) * 100) / 100);
  }
}

function courseRollingMean(prefix, courseKey, market, win = 200) {
  const vals = [];
  for (let i = prefix.length - 1; i >= 0 && vals.length < win; i--) {
    if (prefix[i].ck !== courseKey) continue;
    const v = prefix[i].vals?.[market];
    if (Number.isFinite(v)) vals.push(v);
  }
  return vals.length ? vals.reduce((s, x) => s + x, 0) / vals.length : NaN;
}

/**
 * @returns {Promise<Map<number, Map<string, number>>>}
 */
export async function buildDgMethodologyMuMapForEvent({
  repoRoot,
  histRows,
  eventName,
  eventYear,
  targetRound,
  betTimeMs,
  fieldDgIds,
  courseName: courseNameOverride = "",
}) {
  const dgSet = new Set((fieldDgIds || []).filter((d) => Number.isFinite(d)).map((d) => Math.round(d)));
  if (!dgSet.size) return new Map();

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
  const layout = resolveCourseLayout({
    coursePar18: 72,
    courseUsed: courseName,
    eventName,
    webRoot: join(repoRoot, "alpha-caddie-web"),
  });
  const coursePar18 = layout.course_par_18 || 72;
  const fairwayHoles = layout.fairway_holes_modeled || N_FW;
  const webRoot = join(repoRoot, "alpha-caddie-web");
  const { resolveWalkforwardWeather } = await import("./historical-walkforward-projections.mjs");
  const weatherSnap = resolveWalkforwardWeather({
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

  /** Infer wave from most recent same-event earlier round if available */
  /** @type {Map<number, string>} */
  const waveByDg = new Map();
  for (const r of prefix) {
    if (!dgSet.has(r.dg)) continue;
    if (!eventsLikelySame(r.event, eventName)) continue;
    if (Number.isFinite(eventYear) && r.year !== eventYear) continue;
    if (r.wave) waveByDg.set(r.dg, r.wave);
  }

  /** @type {Map<number, Map<string, number>>} */
  const byDg = new Map();
  for (const dg of dgSet) {
    const mus = new Map();
    const wave = waveByDg.get(dg) || "";
    for (const market of DG_MARKETS) {
      const eff = effByMarket.get(market);
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
        weatherSnap,
        fairwayHoles,
      );

      if (market === "Birdies") {
        const bob = predictBirdiesBobLevel(prefix, dg, courseKey);
        if (Number.isFinite(bob)) {
          const a = clamp(BIRDIE_BOB_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * bob : bob;
        }
      } else if (market === "Fairways hit") {
        const acc = predictFairwaysAccLevel(prefix, dg, courseKey);
        if (Number.isFinite(acc)) {
          const a = clamp(FAIRWAY_ACC_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * acc : acc;
        }
      } else if (market === "GIR") {
        const lvl = predictGirLevel(prefix, dg, courseKey);
        if (Number.isFinite(lvl)) {
          const a = clamp(GIR_LEVEL_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * lvl : lvl;
        }
      } else if (market === "Bogeys") {
        const lvl = predictBogeysLevel(prefix, dg, courseKey);
        if (Number.isFinite(lvl)) {
          const a = clamp(BOGEY_LEVEL_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * lvl : lvl;
        }
      } else if (market === "Pars") {
        const lvl = predictParsLevel(prefix, dg, courseKey);
        if (Number.isFinite(lvl) && PARS_LEVEL_BLEND > 0) {
          const a = clamp(PARS_LEVEL_BLEND, 0, 1);
          mu = Number.isFinite(mu) ? (1 - a) * mu + a * lvl : lvl;
        }
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

      if (Number.isFinite(mu)) mus.set(market, Math.round(clampMu(market, mu, coursePar18, fairwayHoles) * 100) / 100);
    }
    // Coherence: Birdies (BoB) + Bogeys (bogey-or-worse) + Pars ≈ 18.
    if (PARS_IDENTITY_BLEND > 0) {
      const bird = mus.get("Birdies");
      const bog = mus.get("Bogeys");
      const pars = mus.get("Pars");
      if (Number.isFinite(bird) && Number.isFinite(bog)) {
        const id = 18 - bird - bog;
        const a = clamp(PARS_IDENTITY_BLEND, 0, 1);
        const mixed = Number.isFinite(pars) ? (1 - a) * pars + a * id : id;
        mus.set("Pars", Math.round(clampMu("Pars", mixed, coursePar18, fairwayHoles) * 100) / 100);
      }
    }
    const total = mus.get("Total score");
    if (Number.isFinite(total)) mus.set("__mu_sg__", Math.round((coursePar18 - total) * 1000) / 1000);
    byDg.set(dg, mus);
  }

  // Field-mean → rolling course prior (only for markets using a level blend).
  const levelCalib = [
    ["Birdies", BIRDIE_BOB_BLEND],
    ["Fairways hit", FAIRWAY_ACC_BLEND],
    ["GIR", GIR_LEVEL_BLEND],
    ["Bogeys", BOGEY_LEVEL_BLEND],
    ["Pars", Math.max(PARS_LEVEL_BLEND, PARS_PAR_MACHINE_BLEND)],
    ["Total score", SCORE_LEVEL_BLEND],
  ];
  for (const [market, blend] of levelCalib) {
    if (!(blend > 0)) continue;
    fieldCalibrateMuMap(byDg, market, courseRollingMean(prefix, courseKey, market));
  }

  // Re-apply light identity after field calib so Pars stays coherent with Birdies/Bogeys.
  if (PARS_IDENTITY_BLEND > 0) {
    const a = clamp(PARS_IDENTITY_BLEND * 0.6, 0, 1);
    for (const mus of byDg.values()) {
      const bird = mus.get("Birdies");
      const bog = mus.get("Bogeys");
      const pars = mus.get("Pars");
      if (!Number.isFinite(bird) || !Number.isFinite(bog) || !Number.isFinite(pars)) continue;
      const mixed = (1 - a) * pars + a * (18 - bird - bog);
      mus.set("Pars", Math.round(clampMu("Pars", mixed, coursePar18, fairwayHoles) * 100) / 100);
    }
  }

  // Actual-bias trim after calib (DG residual path was ~+0.7 high vs outcomes).
  if (Number.isFinite(SCORE_BIAS_TRIM) && SCORE_BIAS_TRIM > 0) {
    for (const mus of byDg.values()) {
      const v = mus.get("Total score");
      if (!Number.isFinite(v)) continue;
      const trimmed = clampMu("Total score", v - SCORE_BIAS_TRIM, coursePar18, fairwayHoles);
      mus.set("Total score", Math.round(trimmed * 100) / 100);
      if (Number.isFinite(trimmed)) {
        mus.set("__mu_sg__", Math.round((coursePar18 - trimmed) * 1000) / 1000);
      }
    }
  }

  return byDg;
}
