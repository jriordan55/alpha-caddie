/**
 * Bayesian hierarchical round model (empirical Bayes, partial pooling).
 *
 * Score(i,r) ~ Normal(μ(i,r), σ_i)
 * μ = tour + shrunk course + shrunk player baseline
 *     + (skill × course traits) + tee-window weather + small form update
 *
 * Birdies and bogeys (and pars) use the same linear predictor as NegBin(λ, r).
 * GIR, fairways, and putts use the same predictor as a normal mean.
 *
 * No player×course random effect. Course fit is only the global interaction
 * of stable skills with course traits. Weather coefficients are learned from
 * within-course changes in historical round weather, with weak priors.
 * Form is an extra weight on the last 8 rounds, shrunk toward a small prior,
 * estimated on top of the long-run baseline (not a hot-hand override).
 *
 * Validation is strictly round-forward: an event is predicted with coefficients
 * and player states fit only on rounds that have already been played.
 */
import { createReadStream, readFileSync, existsSync } from "fs";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import { negBinProbOver } from "./hierarchical-round-mu.mjs";

const MARKETS = [
  { id: "score", dist: "normal", sigma2: 8.2, formCap: 0.4 },
  { id: "birdies", dist: "negbin", sigma2: 2.4, formCap: 0.35 },
  { id: "bogeys", dist: "negbin", sigma2: 2.2, formCap: 0.35 },
  { id: "pars", dist: "negbin", sigma2: 2.6, formCap: 0.35 },
  { id: "gir", dist: "normal", sigma2: 4.5, formCap: 0.45 },
  { id: "fairways", dist: "normal", sigma2: 3.2, formCap: 0.4 },
  { id: "putts", dist: "normal", sigma2: 3.0, formCap: 0.35 },
];

const IX_NAMES = ["ott_long", "ott_narrow", "app_firm", "putt_demand"];
const WX_NAMES = ["wind_mph", "rain", "temp_per_10f", "hum_per_15pct"];

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}
function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}
function shrink(n, k) {
  return Math.max(0, n) / (Math.max(0, n) + Math.max(1e-6, k));
}
function dot(a, b) {
  let s = 0;
  for (let i = 0; i < a.length; i++) s += (a[i] || 0) * (b[i] || 0);
  return s;
}
function zeros(k) {
  return Array(k).fill(0);
}
function eyeAdd(xtx, xty, prior) {
  const k = xty.length;
  const A = xtx.map((row) => row.slice());
  const b = xty.slice();
  for (let i = 0; i < k; i++) {
    const pr = prior[i] || { mean: 0, lambda: 1 };
    A[i][i] += pr.lambda;
    b[i] += pr.mean * pr.lambda;
  }
  return { A, b };
}
function solve(xtx, xty, prior) {
  const { A, b } = eyeAdd(xtx, xty, prior);
  const k = b.length;
  const m = A.map((row, i) => [...row, b[i]]);
  for (let col = 0; col < k; col++) {
    let piv = col;
    for (let r = col + 1; r < k; r++) if (Math.abs(m[r][col]) > Math.abs(m[piv][col])) piv = r;
    const tmp = m[col];
    m[col] = m[piv];
    m[piv] = tmp;
    const div = m[col][col] || 1e-12;
    for (let c = col; c <= k; c++) m[col][c] /= div;
    for (let r = 0; r < k; r++) {
      if (r === col) continue;
      const f = m[r][col];
      for (let c = col; c <= k; c++) m[r][c] -= f * m[col][c];
    }
  }
  return m.map((row) => row[k]);
}
function emptyXtx(k) {
  return Array.from({ length: k }, () => zeros(k));
}
function accumLin(xtx, xty, x, y) {
  for (let a = 0; a < x.length; a++) {
    xty[a] += x[a] * y;
    for (let b = 0; b < x.length; b++) xtx[a][b] += x[a] * x[b];
  }
}

function completedMs(row) {
  const s = String(row?.event_completed || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) return Date.parse(`${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`);
  const t = Date.parse(s);
  return Number.isFinite(t) ? t : NaN;
}
function roundMs(row) {
  const end = completedMs(row);
  const rnd = Math.round(num(row.round_num, NaN));
  if (!Number.isFinite(end)) return NaN;
  if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return end;
  return end - (4 - rnd) * 86400000;
}

function asFrac(v) {
  const n = num(v, NaN);
  if (!Number.isFinite(n) || n < 0) return NaN;
  if (n <= 1.0001) return n;
  if (n <= 100) return n / 100;
  return NaN;
}

function waveFromTeetime(teetime) {
  const m = String(teetime || "").trim().match(/(\d{1,2}):(\d{2})/);
  if (!m) return "";
  const hh = Number(m[1]);
  if (!Number.isFinite(hh)) return "";
  return hh < 12 ? "morning" : "afternoon";
}

/**
 * Wind feature is mean sustained mph over the tee window — the same `windMph`
 * stored on historical rounds. Gusts are not folded in; a 15 mph gust on a
 * 3 mph sustained day is not a 9 mph archive day.
 */
export function weatherDesign(snap) {
  if (!snap) return null;
  const sustained = num(snap.windMph, NaN);
  const wind = Number.isFinite(num(snap.windBlend, NaN)) ? num(snap.windBlend, NaN) : sustained;
  if (!Number.isFinite(wind)) return null;
  const temp = num(snap.tempF, 70);
  const hum = num(snap.humidityPct, 55);
  const cond = String(snap.condition || "").toLowerCase();
  const rainMm = num(snap.precipMm ?? snap.rainMm, 0);
  const rain = cond === "rain" || cond === "storm" || cond.includes("rain") || rainMm >= 0.4 ? 1 : 0;
  return [wind - 8, rain, (temp - 70) / 10, (hum - 55) / 15];
}

function weatherPrior(mkt) {
  // λ = σ² / τ². Weakly informative: wind makes scoring harder, birdies rarer, bogeys more common.
  const s2 = MARKETS.find((m) => m.id === mkt)?.sigma2 || 4;
  const lam = (tau) => s2 / (tau * tau);
  const windMean = mkt === "score" || mkt === "bogeys" || mkt === "putts" ? 0.06 : mkt === "birdies" || mkt === "gir" || mkt === "fairways" ? -0.035 : -0.01;
  const rainMean = mkt === "score" || mkt === "bogeys" ? 0.12 : mkt === "birdies" || mkt === "gir" ? -0.08 : 0;
  return [
    { mean: windMean, lambda: lam(mkt === "score" ? 0.045 : 0.03) },
    { mean: rainMean, lambda: lam(0.25) },
    { mean: 0, lambda: lam(0.2) },
    { mean: 0, lambda: lam(0.15) },
  ];
}
function ixPrior(mkt) {
  const s2 = MARKETS.find((m) => m.id === mkt)?.sigma2 || 4;
  const tau = mkt === "score" ? 0.18 : 0.12;
  const lambda = s2 / (tau * tau);
  return IX_NAMES.map(() => ({ mean: 0, lambda }));
}
function formPrior(mkt) {
  const s2 = MARKETS.find((m) => m.id === mkt)?.sigma2 || 4;
  const lambda = s2 / (0.1 * 0.1);
  return [{ mean: 0.06, lambda }];
}

function blankAcc() {
  return {
    ixXtx: emptyXtx(4),
    ixXty: zeros(4),
    ixN: 0,
    formXtx: [[0]],
    formXty: [0],
    formN: 0,
    errN: 0,
    errSum: 0,
    errSq: 0,
    ySum: 0,
    yN: 0,
  };
}

function blankPlayer() {
  const n = {};
  const sum = {};
  const sumSq = {};
  const recent = {};
  for (const m of MARKETS) {
    n[m.id] = 0;
    sum[m.id] = 0;
    sumSq[m.id] = 0;
    recent[m.id] = [];
  }
  return { n, sum, sumSq, recent, sgN: 0, sgSum: { ott: 0, app: 0, arg: 0, putt: 0 } };
}

function playerBaseline(p, mkt, k) {
  const n = p.n[mkt] || 0;
  if (n <= 0) return 0;
  return shrink(n, k) * (p.sum[mkt] / n);
}
function recentMean(p, mkt) {
  const a = p.recent[mkt];
  if (!a || a.length < 4) return NaN;
  return a.reduce((s, v) => s + v, 0) / a.length;
}
function playerSigma(p, mkt, tourSd, k) {
  const n = p.n[mkt] || 0;
  if (n < 8) return tourSd;
  const mean = p.sum[mkt] / n;
  const v = Math.max(0.25, p.sumSq[mkt] / n - mean * mean);
  const sd = Math.sqrt(v);
  const w = shrink(n, k);
  return Math.sqrt(w * sd * sd + (1 - w) * tourSd * tourSd);
}

export function loadCourseMoments(webRoot) {
  const path = `${webRoot}/course-table.json`;
  if (!existsSync(path)) return { byKey: new Map(), moments: null };
  const ct = JSON.parse(readFileSync(path, "utf8"));
  const rows = Object.values(ct.byNormKey || {});
  function mom(vals) {
    const a = vals.filter((v) => Number.isFinite(v));
    const mean = a.reduce((s, v) => s + v, 0) / Math.max(1, a.length);
    const sd = Math.sqrt(a.reduce((s, v) => s + (v - mean) ** 2, 0) / Math.max(1, a.length)) || 1;
    return { mean, sd: Math.max(sd, 1e-6) };
  }
  const moments = {
    yardage: mom(rows.map((r) => num(r.yardage, NaN))),
    fw: mom(rows.map((r) => num(r.fw_width, NaN))),
    gir: mom(rows.map((r) => num(r.adj_gir, NaN))),
    acc: mom(rows.map((r) => num(r.adj_driving_accuracy, NaN))),
    putt: mom(rows.map((r) => num(r.putt_sg, NaN))),
  };
  const byKey = new Map();
  for (const r of rows) {
    const key = r._normKey || normCourseNameKey(r.course);
    byKey.set(key, traitZFromRaw(r, moments));
  }
  return { byKey, moments };
}

function traitZFromRaw(raw, moments) {
  const yard = num(raw.yardage, NaN);
  const fw = num(raw.fw_width, NaN);
  const gir = num(raw.adj_gir ?? raw.gir, NaN);
  const acc = num(raw.adj_driving_accuracy ?? raw.acc, NaN);
  const putt = num(raw.putt_sg, NaN);
  const yardage_z = Number.isFinite(yard) ? (yard - moments.yardage.mean) / moments.yardage.sd : 0;
  let narrow_z = 0;
  if (Number.isFinite(fw)) narrow_z = (moments.fw.mean - fw) / moments.fw.sd;
  else if (Number.isFinite(acc)) narrow_z = (moments.acc.mean - acc) / moments.acc.sd;
  let firm_hold_z = 0;
  if (Number.isFinite(gir)) firm_hold_z = (moments.gir.mean - gir) / moments.gir.sd;
  const putt_demand_z = Number.isFinite(putt) ? (moments.putt.mean - putt) / moments.putt.sd : 0;
  return {
    yardage_z,
    narrow_z,
    firm_hold_z,
    putt_demand_z,
    yardage: Number.isFinite(yard) ? yard : null,
    gir: Number.isFinite(gir) ? gir : null,
    acc: Number.isFinite(acc) ? acc : null,
  };
}

/**
 * Yokohama has no row in course-table and no player-course history in the CSV.
 * Yardage is the published tournament scorecard (a measurement).
 * Fairway-hit and GIR rates are one prior season — shrunk hard toward the tour course mean.
 */
export function yokohamaTraits(moments) {
  if (!moments) return { yardage_z: 0, narrow_z: 0, firm_hold_z: 0, putt_demand_z: 0 };
  const k = 8;
  const accObs = 10.34 / 15;
  const girObs = 11.71 / 18;
  const acc = shrink(1, k) * accObs + (1 - shrink(1, k)) * moments.acc.mean;
  const gir = shrink(1, k) * girObs + (1 - shrink(1, k)) * moments.gir.mean;
  return {
    ...traitZFromRaw({ yardage: 7322, acc, gir, putt_sg: NaN, fw_width: NaN }, moments),
    note: "yardage=7322 scorecard; 2025 FW/GIR rates shrunk with k=8 events toward tour-course means; no player-course term",
    acc_shrunk: acc,
    gir_shrunk: gir,
    shrink_weight_on_2025: shrink(1, k),
  };
}

function pushRecent(arr, v, win = 8) {
  arr.push(v);
  if (arr.length > win) arr.shift();
}

function skillZ(player, sd) {
  const n = player.sgN;
  if (n < 12) return null;
  const w = shrink(n, 36);
  const z = (key) => (w * (player.sgSum[key] / n)) / (sd[key] || 0.4);
  return { ott: z("ott"), app: z("app"), arg: z("arg"), putt: z("putt"), n };
}

function ixFeatures(sk, traits) {
  if (!sk || !traits) return null;
  return [
    sk.ott * (traits.yardage_z || 0),
    sk.ott * (traits.narrow_z || 0),
    sk.app * (traits.firm_hold_z || 0),
    sk.putt * (traits.putt_demand_z || 0),
  ];
}

function clampMu(mkt, mu, par) {
  if (mkt === "score") return clamp(mu, (par || 71) - 10, (par || 71) + 12);
  if (mkt === "birdies" || mkt === "bogeys") return clamp(mu, 0.25, 10);
  if (mkt === "pars") return clamp(mu, 4, 16);
  if (mkt === "gir") return clamp(mu, 4, 16.5);
  if (mkt === "fairways") return clamp(mu, 3, 14);
  if (mkt === "putts") return clamp(mu, 24, 34);
  return mu;
}

function refit(state) {
  const snaps = state.snaps;
  const coef = {
    tour: {},
    course: {},
    courseK: {},
    courseTau: {},
    weather: {},
    ix: {},
    formW: {},
    kPlayer: {},
    skillSd: state.skillSd,
  };
  for (const m of MARKETS) {
    const acc = state.acc[m.id];
    coef.weather[m.id] = solve(emptyXtx(4), zeros(4), weatherPrior(m.id));
    // Replace with data-informed solve below once snaps are grouped.
    coef.ix[m.id] = solve(acc.ixXtx, acc.ixXty, ixPrior(m.id));
    const fw = solve(acc.formXtx, acc.formXty, formPrior(m.id));
    const cap = m.id === "score" ? 0.28 : 0.22;
    coef.formW[m.id] = clamp(fw[0], -0.02, cap);
    coef.kPlayer[m.id] = state.kPlayer[m.id];
  }

  // Within-course demeaned weather, per market.
  for (const m of MARKETS) {
    const byC = new Map();
    for (const s of snaps) {
      if (!s.wx || !Number.isFinite(s.field[m.id])) continue;
      const arr = byC.get(s.ck) || [];
      arr.push(s);
      byC.set(s.ck, arr);
    }
    const xtx = emptyXtx(4);
    const xty = zeros(4);
    let n = 0;
    for (const arr of byC.values()) {
      if (arr.length < 6) continue;
      const yMean = arr.reduce((s, r) => s + r.field[m.id], 0) / arr.length;
      const xMean = zeros(4);
      for (const r of arr) for (let i = 0; i < 4; i++) xMean[i] += r.wx[i];
      for (let i = 0; i < 4; i++) xMean[i] /= arr.length;
      for (const r of arr) {
        const x = r.wx.map((v, i) => v - xMean[i]);
        accumLin(xtx, xty, x, r.field[m.id] - yMean);
        n++;
      }
    }
    if (n >= 80) coef.weather[m.id] = solve(xtx, xty, weatherPrior(m.id));
    coef.weatherN = coef.weatherN || {};
    coef.weatherN[m.id] = n;
  }

  const now = snaps.length ? snaps[snaps.length - 1].t : Date.now();
  for (const m of MARKETS) {
    const beta = coef.weather[m.id];
    let sw = 0;
    let sy = 0;
    const withWx = snaps.filter((s) => s.wx && Number.isFinite(s.field[m.id]));
    let meanWxEff = 0;
    if (withWx.length) {
      meanWxEff = withWx.reduce((s, r) => s + dot(beta, r.wx), 0) / withWx.length;
    }
    for (const s of snaps) {
      if (!Number.isFinite(s.field[m.id])) continue;
      const years = Math.max(0, (now - s.t) / (365.25 * 86400000));
      const w = Math.exp((-Math.LN2 * years) / 2.5);
      const wxEff = s.wx ? dot(beta, s.wx) : meanWxEff;
      sw += w;
      sy += w * (s.field[m.id] - wxEff);
    }
    const tour = sw > 0 ? sy / sw : NaN;
    coef.tour[m.id] = tour;

    const byC = new Map();
    for (const s of snaps) {
      if (!Number.isFinite(s.field[m.id]) || !Number.isFinite(tour)) continue;
      const wxEff = s.wx ? dot(beta, s.wx) : meanWxEff;
      const dev = s.field[m.id] - tour - wxEff;
      const o = byC.get(s.ck) || { sum: 0, sumSq: 0, n: 0 };
      o.sum += dev;
      o.sumSq += dev * dev;
      o.n++;
      byC.set(s.ck, o);
    }
    const means = [];
    let withinNum = 0;
    let withinDen = 0;
    for (const o of byC.values()) {
      if (o.n < 2) continue;
      const mean = o.sum / o.n;
      means.push({ mean, n: o.n });
      const v = Math.max(0, o.sumSq / o.n - mean * mean);
      withinNum += (o.n - 1) * v;
      withinDen += o.n - 1;
    }
    const within = withinDen > 0 ? withinNum / withinDen : 1;
    let varMeans = 0;
    if (means.length > 2) {
      const wsum = means.reduce((s, r) => s + r.n, 0);
      const mbar = means.reduce((s, r) => s + r.n * r.mean, 0) / wsum;
      varMeans = means.reduce((s, r) => s + r.n * (r.mean - mbar) ** 2, 0) / wsum;
    }
    const wsum = means.reduce((s, r) => s + r.n, 0);
    const tau2 = Math.max(0.04, varMeans - within * (means.length / Math.max(1, wsum)));
    const k = clamp(within / tau2, m.id === "score" ? 4 : 6, 24);
    coef.courseK[m.id] = k;
    coef.courseTau[m.id] = Math.sqrt(tau2);
    const cmap = new Map();
    for (const [ck, o] of byC) {
      cmap.set(ck, shrink(o.n, k) * (o.sum / o.n));
    }
    coef.course[m.id] = cmap;
  }
  return coef;
}

function predictOne(mkt, player, traits, wx, ck, par, coef) {
  const spec = MARKETS.find((m) => m.id === mkt);
  const k = coef.kPlayer[mkt] || 24;
  const base = playerBaseline(player, mkt, k);
  const rec = recentMean(player, mkt);
  let form = 0;
  if (Number.isFinite(rec)) form = clamp((coef.formW[mkt] || 0) * (rec - base), -spec.formCap, spec.formCap);
  const sk = skillZ(player, coef.skillSd);
  const x = ixFeatures(sk, traits);
  const ix = x ? dot(coef.ix[mkt], x) : 0;
  const course = coef.course[mkt]?.get(ck) || 0;
  const weather = wx ? dot(coef.weather[mkt], wx) : 0;
  const tour = coef.tour[mkt];
  const mu = clampMu(mkt, tour + course + base + form + ix + weather, par);
  return { mu, base, form, ix, course, weather, tour, skillN: player.sgN || 0, n: player.n[mkt] || 0 };
}

function updatePlayer(player, resid, sg) {
  for (const m of MARKETS) {
    const v = resid[m.id];
    if (!Number.isFinite(v)) continue;
    player.n[m.id] += 1;
    player.sum[m.id] += v;
    player.sumSq[m.id] += v * v;
    pushRecent(player.recent[m.id], v);
  }
  if (sg) {
    player.sgN += 1;
    player.sgSum.ott += sg.ott;
    player.sgSum.app += sg.app;
    player.sgSum.arg += sg.arg;
    player.sgSum.putt += sg.putt;
  }
}

function recomputeK(players) {
  const kPlayer = {};
  for (const m of MARKETS) {
    const stats = [];
    for (const p of players.values()) {
      const n = p.n[m.id];
      if (n < 12) continue;
      const mean = p.sum[m.id] / n;
      const v = Math.max(0.05, p.sumSq[m.id] / n - mean * mean);
      stats.push({ n, mean, v });
    }
    if (stats.length < 30) {
      kPlayer[m.id] = m.id === "score" ? 28 : 24;
      continue;
    }
    const withinDen = stats.reduce((s, r) => s + (r.n - 1), 0);
    const within = stats.reduce((s, r) => s + (r.n - 1) * r.v, 0) / Math.max(1, withinDen);
    const wsum = stats.reduce((s, r) => s + r.n, 0);
    const mbar = stats.reduce((s, r) => s + r.n * r.mean, 0) / wsum;
    const varMeans = stats.reduce((s, r) => s + r.n * (r.mean - mbar) ** 2, 0) / wsum;
    // Precision-weighted MoM: average sampling variance is σ² × (players / total rounds).
    const tau2 = Math.max(0.02, varMeans - within * (stats.length / wsum));
    kPlayer[m.id] = clamp(within / tau2, 8, 45);
  }
  return kPlayer;
}

function recomputeSkillSd(players) {
  const keys = ["ott", "app", "arg", "putt"];
  const vals = { ott: [], app: [], arg: [], putt: [] };
  for (const p of players.values()) {
    if (p.sgN < 20) continue;
    for (const k of keys) vals[k].push(p.sgSum[k] / p.sgN);
  }
  const sd = {};
  for (const k of keys) {
    const a = vals[k];
    if (a.length < 30) {
      sd[k] = 0.4;
      continue;
    }
    const mean = a.reduce((s, v) => s + v, 0) / a.length;
    sd[k] = Math.max(0.15, Math.sqrt(a.reduce((s, v) => s + (v - mean) ** 2, 0) / a.length));
  }
  return sd;
}

/**
 * Load PGA rounds from 2017+ into compact records.
 * Counting stats: birdies and bogeys are the raw columns (not eagles/doubles).
 * GIR is greens (fraction × 18). Fairways are a 14-hole equivalent count (fraction × 14).
 */
export async function loadRounds(csvPath, weatherByKey) {
  /** @type {object[]} */
  const rows = [];
  await new Promise((resolve, reject) => {
    createReadStream(csvPath)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", resolve)
      .on("error", reject);
  });
  const out = [];
  for (const r of rows) {
    const tour = String(r.tour || "").toLowerCase();
    if (tour && tour !== "pga") continue;
    const year = Math.round(num(r.year, NaN));
    if (!Number.isFinite(year) || year < 2017) continue;
    const dg = Math.round(num(r.dg_id, NaN));
    const ck = normCourseNameKey(r.course_name || "");
    const rnd = Math.round(num(r.round_num, NaN));
    const t = roundMs(r);
    if (!Number.isFinite(dg) || !ck || !Number.isFinite(rnd) || !Number.isFinite(t)) continue;
    const par = Math.round(num(r.course_par, 72)) || 72;
    const score = num(r.round_score, NaN);
    const birdies = num(r.birdies, NaN);
    const bogeys = num(r.bogies ?? r.bogeys, NaN);
    const pars = num(r.pars, NaN);
    const placeholder = birdies === 0 && bogeys === 0 && (!Number.isFinite(pars) || pars === 0 || pars >= 17);
    const girF = asFrac(r.gir);
    const fwF = asFrac(r.driving_acc);
    const putts = num(r.putts, NaN);
    const vals = {
      score: Number.isFinite(score) && score >= 55 && score <= 95 ? score : NaN,
      birdies: !placeholder && Number.isFinite(birdies) && birdies >= 0 && birdies <= 14 ? birdies : NaN,
      bogeys: !placeholder && Number.isFinite(bogeys) && bogeys >= 0 && bogeys <= 14 ? bogeys : NaN,
      pars: !placeholder && Number.isFinite(pars) && pars >= 0 && pars <= 18 ? pars : NaN,
      gir: Number.isFinite(girF) ? girF * 18 : NaN,
      fairways: Number.isFinite(fwF) ? fwF * 14 : NaN,
      putts: Number.isFinite(putts) && putts >= 18 && putts <= 42 ? putts : NaN,
    };
    if (!Number.isFinite(vals.score)) continue;
    const sgOtt = num(r.sg_ott, NaN);
    const sgApp = num(r.sg_app, NaN);
    const sgArg = num(r.sg_arg, NaN);
    const sgPutt = num(r.sg_putt, NaN);
    const sg =
      [sgOtt, sgApp, sgArg, sgPutt].every((v) => Number.isFinite(v))
        ? { ott: sgOtt, app: sgApp, arg: sgArg, putt: sgPutt }
        : null;
    const event = `${String(r.event_id || r.event_name || "").trim()}|${year}`;
    const wxKey = `${String(r.event_id || "").trim()}|${year}|${rnd}`;
    const snap = weatherByKey?.[wxKey] || null;
    const wx = snap ? weatherDesign({ ...snap, windBlend: num(snap.windMph, NaN) }) : null;
    out.push({
      dg,
      t,
      year,
      rnd,
      ck,
      par,
      event,
      eventName: String(r.event_name || ""),
      vals,
      sg,
      wx,
      wave: waveFromTeetime(r.teetime),
    });
  }
  out.sort((a, b) => a.t - b.t || a.event.localeCompare(b.event) || a.rnd - b.rnd || a.dg - b.dg);
  return out;
}

/**
 * Walk history in time order.
 * holdoutN: last N events are scored round-forward, then folded in so the
 * returned coefficients include all history (the live fit).
 */
export function fitRoundForward(rounds, traitsByKey, opts = {}) {
  const holdoutN = opts.holdoutEvents ?? 12;
  const eventOrder = [];
  const seen = new Set();
  for (const r of rounds) {
    if (seen.has(r.event)) continue;
    seen.add(r.event);
    eventOrder.push(r.event);
  }
  const holdout = new Set(eventOrder.slice(Math.max(0, eventOrder.length - holdoutN)));

  const players = new Map();
  const state = {
    snaps: [],
    acc: Object.fromEntries(MARKETS.map((m) => [m.id, blankAcc()])),
    players,
    kPlayer: Object.fromEntries(MARKETS.map((m) => [m.id, m.id === "score" ? 28 : 24])),
    skillSd: { ott: 0.4, app: 0.45, arg: 0.28, putt: 0.4 },
  };
  let coef = refit(state);

  const oos = Object.fromEntries(
    MARKETS.map((m) => [
      m.id,
      { n: 0, abs: 0, absBase: 0, err: 0, sq: 0, cover: 0, sqBase: 0 },
    ]),
  );
  /** @type {Map<string, { name: string, n: number, err: number }>} */
  const oosEvents = new Map();
  const waveGaps = [];

  let i = 0;
  while (i < rounds.length) {
    const event = rounds[i].event;
    let j = i;
    while (j < rounds.length && rounds[j].event === event) j++;
    const isHold = holdout.has(event);
    coef = refit(state);

    // Process round by round inside the event.
    let p = i;
    while (p < j) {
      const rnd = rounds[p].rnd;
      let q = p;
      while (q < j && rounds[q].rnd === rnd) q++;
      const group = rounds.slice(p, q);
      const field = {};
      const fieldN = {};
      for (const m of MARKETS) {
        let s = 0;
        let n = 0;
        for (const row of group) {
          const v = row.vals[m.id];
          if (!Number.isFinite(v)) continue;
          s += v;
          n++;
        }
        field[m.id] = n >= 20 ? s / n : NaN;
        fieldN[m.id] = n;
      }

      if (Number.isFinite(field.score)) {
        const byWave = { morning: [], afternoon: [] };
        for (const row of group) {
          const pl = players.get(row.dg) || blankPlayer();
          if (!row.wave || pl.n.score < 8) continue;
          const base = playerBaseline(pl, "score", coef.kPlayer.score);
          byWave[row.wave].push(row.vals.score - field.score - base);
        }
        if (byWave.morning.length >= 12 && byWave.afternoon.length >= 12) {
          const mean = (a) => a.reduce((s, v) => s + v, 0) / a.length;
          waveGaps.push(mean(byWave.afternoon) - mean(byWave.morning));
        }
      }

      for (const row of group) {
        let pl = players.get(row.dg);
        if (!pl) {
          pl = blankPlayer();
          players.set(row.dg, pl);
        }
        const traits = traitsByKey.get(row.ck) || null;
        if (isHold) {
          for (const m of MARKETS) {
            const y = row.vals[m.id];
            if (!Number.isFinite(y) || !Number.isFinite(field[m.id]) || pl.n[m.id] < 8) continue;
            const pred = predictOne(m.id, pl, traits, row.wx, row.ck, row.par, coef);
            const baseOnly = clampMu(
              m.id,
              pred.tour + pred.course + pred.base,
              row.par,
            );
            const err = y - pred.mu;
            const bucket = oos[m.id];
            bucket.n++;
            bucket.abs += Math.abs(err);
            bucket.absBase += Math.abs(y - baseOnly);
            bucket.err += err;
            bucket.sq += err * err;
            bucket.sqBase += (y - baseOnly) ** 2;
            const sd = playerSigma(pl, m.id, Math.sqrt(m.sigma2) * 0.95, 30);
            if (Math.abs(err) <= sd) bucket.cover++;
            if (m.id === "score") {
              const ev = oosEvents.get(row.event) || { name: row.eventName, n: 0, err: 0 };
              ev.n++;
              ev.err += err;
              oosEvents.set(row.event, ev);
            }
          }
        }

        // Training accumulators use prior state only (this round not yet included).
        const sk = skillZ(pl, state.skillSd);
        const x = ixFeatures(sk, traits);
        for (const m of MARKETS) {
          const y = row.vals[m.id];
          if (!Number.isFinite(y) || !Number.isFinite(field[m.id])) continue;
          const resid = y - field[m.id];
          const base = playerBaseline(pl, m.id, state.kPlayer[m.id]);
          const acc = state.acc[m.id];
          if (x && pl.sgN >= 12) {
            accumLin(acc.ixXtx, acc.ixXty, x, resid - base);
            acc.ixN++;
          }
          const rec = recentMean(pl, m.id);
          if (Number.isFinite(rec) && pl.n[m.id] >= 12) {
            const gap = rec - base;
            acc.formXtx[0][0] += gap * gap;
            acc.formXty[0] += gap * (resid - base);
            acc.formN++;
          }
        }
      }

      if (Number.isFinite(field.score) && fieldN.score >= 30) {
        state.snaps.push({
          ck: group[0].ck,
          t: group[0].t,
          event: group[0].event,
          field,
          wx: group[0].wx,
        });
      }

      for (const row of group) {
        const pl = players.get(row.dg);
        const resid = {};
        for (const m of MARKETS) {
          const y = row.vals[m.id];
          resid[m.id] = Number.isFinite(y) && Number.isFinite(field[m.id]) ? y - field[m.id] : NaN;
        }
        updatePlayer(pl, resid, row.sg);
      }
      p = q;
    }

    state.kPlayer = recomputeK(players);
    state.skillSd = recomputeSkillSd(players);
    i = j;
  }

  coef = refit(state);
  const validation = {};
  for (const m of MARKETS) {
    const b = oos[m.id];
    validation[m.id] = b.n
      ? {
          n: b.n,
          mae: b.abs / b.n,
          mae_baseline: b.absBase / b.n,
          bias: b.err / b.n,
          rmse: Math.sqrt(b.sq / b.n),
          rmse_baseline: Math.sqrt(b.sqBase / b.n),
          cover68: b.cover / b.n,
        }
      : { n: 0 };
  }
  const waveMean = waveGaps.length ? waveGaps.reduce((s, v) => s + v, 0) / waveGaps.length : 0;
  const waveSd = waveGaps.length
    ? Math.sqrt(waveGaps.reduce((s, v) => s + (v - waveMean) ** 2, 0) / waveGaps.length)
    : 1;
  // Shrink the residual AM/PM gap. This is what is left after player baseline,
  // on days we did not have separate wave weather — do not stack it on a tee-window forecast
  // unless the forecast itself shows a meaningful AM/PM difference.
  const waveShrink = shrink(waveGaps.length, 40) * waveMean;

  // NegBin r and normal σ from round-forward errors when we have them; else in-sample residual scale.
  const dispersion = {};
  for (const m of MARKETS) {
    const v = validation[m.id];
    if (m.dist === "negbin" && v.n > 50) {
      const meanLam = coef.tour[m.id];
      const varErr = v.rmse ** 2;
      const r = (meanLam * meanLam) / Math.max(0.05, varErr - Math.max(0.3, meanLam));
      dispersion[m.id] = { r: clamp(r, 1.5, 40), source: "round_forward_mom" };
    } else if (m.dist === "normal" && v.n > 50) {
      dispersion[m.id] = { sigma: clamp(v.rmse, 0.8, 5.5), source: "round_forward_rmse" };
    } else {
      dispersion[m.id] = m.dist === "negbin" ? { r: 8, source: "default" } : { sigma: Math.sqrt(m.sigma2) * 0.9, source: "default" };
    }
  }

  return {
    coef,
    players,
    validation,
    dispersion,
    wave: {
      n_rounds: waveGaps.length,
      raw_mean_pm_minus_am: waveMean,
      sd: waveSd,
      shrunk_strokes: waveShrink,
    },
    n_events: eventOrder.length,
    holdout_events: [...oosEvents.entries()].map(([id, v]) => ({
      id,
      name: v.name,
      n: v.n,
      bias: v.n ? v.err / v.n : null,
    })),
    n_rounds: rounds.length,
    n_players: players.size,
    ixN: Object.fromEntries(MARKETS.map((m) => [m.id, state.acc[m.id].ixN])),
    formN: Object.fromEntries(MARKETS.map((m) => [m.id, state.acc[m.id].formN])),
  };
}

export function projectPlayer(fit, opts) {
  const { dg, traits, wx, ck, par, projSkill } = opts;
  const player = fit.players.get(dg) || blankPlayer();
  // Thin history: treat the published skill rating as 12 rounds, only for the interaction.
  let skillPlayer = player;
  if (projSkill && player.sgN < 40) {
    const add = 12;
    skillPlayer = {
      ...player,
      sgN: player.sgN + add,
      sgSum: {
        ott: player.sgSum.ott + add * num(projSkill.ott, 0),
        app: player.sgSum.app + add * num(projSkill.app, 0),
        arg: player.sgSum.arg + add * num(projSkill.arg, 0),
        putt: player.sgSum.putt + add * num(projSkill.putt, 0),
      },
    };
  }
  const out = {};
  for (const m of MARKETS) {
    const histPred = predictOne(m.id, player, traits, wx, ck, par, fit.coef);
    const skillPred = predictOne(m.id, skillPlayer, traits, wx, ck, par, fit.coef);
    const tourSd = fit.dispersion[m.id]?.sigma || Math.sqrt(m.sigma2) * 0.95;
    let sigma = m.dist === "normal" ? playerSigma(player, m.id, tourSd, 30) : NaN;
    if (m.id === "score") {
      const tau = fit.coef.courseTau.score || 0.6;
      const known = fit.coef.course.score?.has(ck);
      const courseVar = known ? 0.04 : tau * tau;
      sigma = Math.sqrt((sigma || tourSd) ** 2 + courseVar);
      sigma = clamp(sigma, 2.35, 4.4);
    }
    const mu = clampMu(m.id, histPred.mu - histPred.ix + skillPred.ix, par);
    out[m.id] = {
      mu,
      base: histPred.base,
      form: histPred.form,
      ix: skillPred.ix,
      course: histPred.course,
      weather: histPred.weather,
      tour: histPred.tour,
      sigma,
      n: player.n[m.id] || 0,
      skillN: player.sgN || 0,
    };
  }
  return out;
}

export function normCdf(z) {
  const zz = clamp(z, -8, 8);
  const t = 1 / (1 + 0.2316419 * Math.abs(zz));
  const d = 0.3989423 * Math.exp((-zz * zz) / 2);
  const p = d * t * (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))));
  return zz >= 0 ? 1 - p : p;
}

export function probOver(mkt, mu, sigma, line, r) {
  const L = num(line, NaN);
  if (!Number.isFinite(L) || !Number.isFinite(mu)) return NaN;
  if (mkt === "birdies" || mkt === "bogeys" || mkt === "pars") return negBinProbOver(L, mu, r);
  const sd = Math.max(0.4, num(sigma, NaN));
  if (!Number.isFinite(sd)) return NaN;
  return clamp(1 - normCdf((L - mu) / sd), 0, 1);
}

export { MARKETS, IX_NAMES, WX_NAMES };
