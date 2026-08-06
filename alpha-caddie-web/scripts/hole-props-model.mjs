/**
 * Hole-level projections from player × course hole average + strokes gained.
 *
 * μ_hole = blend( field_mean − shrink(n)×sg ,  par + round_stp/18 )
 * Discrete PMF for O/U + hole-winner probs; normal sum for 9/3-hole packs.
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import { holeSgShrinkPrior } from "./course-hole-sg-asof.mjs";
import { impliedProbFromAmerican } from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");

const HOLE_SIGMA = 0.85;
const SKILL_BLEND_PRIOR_N = 6;

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export const HOLE_PROP_MARKETS = Object.freeze({
  HOLE_SCORE: "Hole Score",
  HOLE_WINNER: "Hole Winner",
  HOLE_MATCHUP: "Hole Matchup",
  HOLES_10_18: "Holes 10-18",
  HOLES_16_17_18: "Holes 16-17-18",
});

function envNum(name, fb) {
  const n = Number(process.env[name]);
  return Number.isFinite(n) ? n : fb;
}

function csvPath(webRoot = WEB) {
  return (
    String(process.env.GOLF_COURSE_HOLE_SG_CSV || "").trim() ||
    join(webRoot, "data", "player_course_hole_sg.csv")
  );
}

function baselinesPath(webRoot = WEB) {
  return (
    String(process.env.GOLF_COURSE_HOLE_SG_BASELINES || "").trim() ||
    join(webRoot, "data", "course_hole_sg_baselines.json")
  );
}

/** @type {Promise<{ byKey: Map<string, object>, baselines: Map<string, object> } | null> | null} */
let tablePromise = null;

/**
 * Load aggregated SG CSV + baselines once.
 * Row key: `${course_key}|${dg_id}|${hole}`
 */
export async function loadHolePropsTables(webRoot = WEB) {
  if (tablePromise) return tablePromise;
  tablePromise = (async () => {
    /** @type {Map<string, object>} */
    const byKey = new Map();
    /** @type {Map<string, object>} */
    const baselines = new Map();

    const bp = baselinesPath(webRoot);
    if (existsSync(bp)) {
      try {
        const j = JSON.parse(readFileSync(bp, "utf8"));
        for (const [k, v] of Object.entries(j.baselines || {})) {
          const ck = normCourseNameKey(v?.course_key || String(k).split("|")[0] || "");
          const hole = Math.round(num(v?.hole ?? String(k).split("|")[1], NaN));
          if (!ck || hole < 1 || hole > 18) continue;
          baselines.set(`${ck}|${hole}`, {
            course_key: ck,
            hole,
            n: Math.round(num(v.n, 0)),
            mean_score: num(v.mean_score, NaN),
            mean_par: num(v.mean_par, NaN),
          });
        }
      } catch (e) {
        console.warn("[hole-props-model] baselines:", e?.message || e);
      }
    }

    const file = csvPath(webRoot);
    if (!existsSync(file)) {
      console.warn(`[hole-props-model] Missing ${file}`);
      return { byKey, baselines };
    }

    await new Promise((resolve, reject) => {
      createReadStream(file)
        .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
        .on("data", (r) => {
          const ck = normCourseNameKey(r.course_key || r.course_name || "");
          const dg = Math.round(num(r.dg_id, NaN));
          const hole = Math.round(num(r.hole, NaN));
          if (!ck || !Number.isFinite(dg) || hole < 1 || hole > 18) return;
          byKey.set(`${ck}|${dg}|${hole}`, {
            dg,
            course_key: ck,
            hole,
            par: Math.round(num(r.par, NaN)),
            n: Math.round(num(r.n, 0)),
            mean_score: num(r.mean_score, NaN),
            field_mean: num(r.field_mean, NaN),
            sg: num(r.sg, NaN),
          });
        })
        .on("end", resolve)
        .on("error", reject);
    });

    return { byKey, baselines };
  })();
  return tablePromise;
}

export function resolveCourseKey(payload) {
  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : {};
  const raw =
    payload?.course_used ||
    meta.course_used ||
    payload?.projection_course_basis?.course_name ||
    payload?.projection_course_basis?.course_key ||
    payload?.course_name ||
    "";
  return normCourseNameKey(raw);
}

export function holeParsFromPayload(payload) {
  const pars = Array.isArray(payload?.hole_pars) ? payload.hole_pars : [];
  /** @type {number[]} */
  const out = [];
  for (let h = 1; h <= 18; h++) {
    const p = Math.round(num(pars[h - 1], NaN));
    out.push(Number.isFinite(p) && p >= 3 && p <= 5 ? p : 4);
  }
  return out;
}

function roundStpForPlayer(player, coursePar18) {
  const stp = num(player?.score_to_par, NaN);
  if (Number.isFinite(stp)) return stp;
  const total = num(player?.total_score, NaN);
  const par = num(coursePar18, 72);
  if (Number.isFinite(total) && Number.isFinite(par)) return total - par;
  return 0;
}

/**
 * Projected mean score on one hole.
 * @returns {{ mu: number, field_mean: number, sg: number, n: number, par: number, source: string }}
 */
export function projectHoleMean(opts = {}) {
  const {
    tables,
    courseKey,
    dgId,
    hole,
    par = 4,
    roundStp = 0,
    priorN = holeSgShrinkPrior(),
  } = opts;
  const h = Math.round(num(hole, NaN));
  const dg = Math.round(num(dgId, NaN));
  const ck = normCourseNameKey(courseKey || "");
  const skillPrior = num(par, 4) + num(roundStp, 0) / 18;

  let fieldMean = NaN;
  let meanScore = NaN;
  let sg = NaN;
  let n = 0;
  let holePar = Math.round(num(par, 4));

  const bl = tables?.baselines?.get(`${ck}|${h}`);
  if (bl && Number.isFinite(bl.mean_score)) {
    fieldMean = bl.mean_score;
    if (Number.isFinite(bl.mean_par)) holePar = Math.round(bl.mean_par);
  }

  const row = tables?.byKey?.get(`${ck}|${dg}|${h}`);
  if (row) {
    n = Math.max(0, Math.round(num(row.n, 0)));
    if (Number.isFinite(row.field_mean)) fieldMean = row.field_mean;
    if (Number.isFinite(row.mean_score)) meanScore = row.mean_score;
    if (Number.isFinite(row.sg)) sg = row.sg;
    if (Number.isFinite(row.par)) holePar = Math.round(row.par);
  }

  if (!Number.isFinite(fieldMean)) fieldMean = holePar + 0.15;
  if (!Number.isFinite(sg) && Number.isFinite(meanScore)) sg = fieldMean - meanScore;
  if (!Number.isFinite(sg)) sg = 0;

  const shrink = n / (n + Math.max(1, priorN));
  const histMu = fieldMean - shrink * sg;
  // No hole plays: lean on field mean, not pure round skill (avoids absurd bogey% on soft holes).
  const wHist = n > 0 ? n / (n + SKILL_BLEND_PRIOR_N) : 0.72;
  const mu = wHist * histMu + (1 - wHist) * skillPrior;
  const source =
    n >= 3 ? "hole_avg+sg" : n >= 1 ? "hole_avg+sg+skill" : "field+skill_prior";

  return {
    mu: Math.round(mu * 1000) / 1000,
    field_mean: Math.round(fieldMean * 1000) / 1000,
    sg: Math.round(sg * 1000) / 1000,
    n,
    par: holePar,
    source,
  };
}

/**
 * Discrete score PMF on integers [lo..hi] centered on μ (Gaussian kernel).
 * @returns {Map<number, number>}
 */
export function holeScorePmf(mu, par = 4, sigma = HOLE_SIGMA) {
  const m = num(mu, NaN);
  const p = Math.round(num(par, 4));
  const s = Math.max(0.35, num(sigma, HOLE_SIGMA));
  const lo = Math.max(1, p - 2);
  const hi = p + 3;
  /** @type {Map<number, number>} */
  const raw = new Map();
  let z = 0;
  for (let x = lo; x <= hi; x++) {
    const w = Math.exp(-0.5 * ((x - m) / s) ** 2);
    raw.set(x, w);
    z += w;
  }
  /** @type {Map<number, number>} */
  const out = new Map();
  if (z <= 0) {
    out.set(Math.max(lo, Math.min(hi, Math.round(m))), 1);
    return out;
  }
  for (const [x, w] of raw) out.set(x, w / z);
  return out;
}

export function pmfMean(pmf) {
  let s = 0;
  for (const [x, p] of pmf || []) s += x * p;
  return s;
}

/** P(score > line), P(score < line), P(score === line) for half or whole lines. */
export function ouProbsFromPmf(pmf, line) {
  const L = num(line, NaN);
  if (!Number.isFinite(L)) return { pOver: NaN, pUnder: NaN, pPush: NaN };
  let pOver = 0;
  let pUnder = 0;
  let pPush = 0;
  for (const [x, p] of pmf || []) {
    if (x > L) pOver += p;
    else if (x < L) pUnder += p;
    else pPush += p;
  }
  // Half-lines: no push mass on integer scores
  if (Math.abs(L - Math.round(L)) > 1e-9) {
    pPush = 0;
  }
  return { pOver, pUnder, pPush };
}

/** Normal CDF approx. */
function normCdf(z) {
  const x = num(z, 0);
  const t = 1 / (1 + 0.2316419 * Math.abs(x));
  const d = 0.3989423 * Math.exp((-x * x) / 2);
  const p =
    d *
    t *
    (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))));
  return x > 0 ? 1 - p : p;
}

/**
 * Sum of holes μ / σ for pack markets (10-18, 16-17-18).
 */
export function projectHoleSum(opts = {}) {
  const { tables, courseKey, dgId, holes, pars, roundStp = 0 } = opts;
  const list = Array.isArray(holes) ? holes : [];
  let mu = 0;
  let varSum = 0;
  let nMin = Infinity;
  /** @type {object[]} */
  const parts = [];
  for (const h of list) {
    const par = Array.isArray(pars) ? pars[h - 1] : 4;
    const proj = projectHoleMean({
      tables,
      courseKey,
      dgId,
      hole: h,
      par,
      roundStp,
    });
    mu += proj.mu;
    varSum += HOLE_SIGMA * HOLE_SIGMA;
    nMin = Math.min(nMin, proj.n);
    parts.push({ hole: h, ...proj });
  }
  const sigma = Math.sqrt(Math.max(1e-6, varSum));
  return {
    mu: Math.round(mu * 1000) / 1000,
    sigma: Math.round(sigma * 1000) / 1000,
    n_min: Number.isFinite(nMin) && nMin !== Infinity ? nMin : 0,
    parts,
  };
}

export function ouProbsNormal(mu, sigma, line) {
  const L = num(line, NaN);
  const m = num(mu, NaN);
  const s = Math.max(0.25, num(sigma, 1));
  if (!Number.isFinite(L) || !Number.isFinite(m)) {
    return { pOver: NaN, pUnder: NaN, pPush: NaN };
  }
  // Continuity correction for whole lines
  const isHalf = Math.abs(L - Math.round(L)) > 1e-9;
  if (isHalf) {
    const pUnder = normCdf((L - m) / s);
    return { pOver: 1 - pUnder, pUnder, pPush: 0 };
  }
  const pUnder = normCdf((L - 0.5 - m) / s);
  const pOver = 1 - normCdf((L + 0.5 - m) / s);
  const pPush = Math.max(0, 1 - pOver - pUnder);
  return { pOver, pUnder, pPush };
}

/**
 * Outright win probs for a group (ties → no winner).
 * Enumerates joint score outcomes (small support).
 */
export function holeWinnerProbs(playerPmfs) {
  const players = (playerPmfs || []).filter((p) => p?.pmf?.size);
  const n = players.length;
  /** @type {Record<string, number>} */
  const win = {};
  for (const p of players) win[String(p.dg_id)] = 0;
  if (n === 0) return { win, pAnyTie: 1 };

  const lists = players.map((p) => [...p.pmf.entries()]);
  let pAnyTie = 0;

  function rec(i, scores, prob) {
    if (prob <= 0) return;
    if (i === n) {
      let best = Infinity;
      for (const s of scores) if (s < best) best = s;
      const winners = [];
      for (let j = 0; j < n; j++) if (scores[j] === best) winners.push(j);
      if (winners.length === 1) {
        const id = String(players[winners[0]].dg_id);
        win[id] = (win[id] || 0) + prob;
      } else {
        pAnyTie += prob;
      }
      return;
    }
    for (const [score, p] of lists[i]) {
      scores[i] = score;
      rec(i + 1, scores, prob * p);
    }
  }
  rec(0, new Array(n), 1);
  return { win, pAnyTie };
}

export function americanToEv(modelProb, american) {
  const p = num(modelProb, NaN);
  const a = Math.round(num(american, NaN));
  if (!Number.isFinite(p) || !Number.isFinite(a) || a === 0) return NaN;
  const profit = a > 0 ? a / 100 : 100 / Math.abs(a);
  return Math.round((p * profit - (1 - p)) * 10000) / 10000;
}

export function pickOuSide(pOver, pUnder, overOdds, underOdds) {
  const impO = impliedProbFromAmerican(overOdds);
  const impU = impliedProbFromAmerican(underOdds);
  const edgeO = Number.isFinite(pOver) && Number.isFinite(impO) ? pOver - impO : -Infinity;
  const edgeU = Number.isFinite(pUnder) && Number.isFinite(impU) ? pUnder - impU : -Infinity;
  if (edgeO >= edgeU && Number.isFinite(edgeO)) {
    return {
      side: "OVER",
      model_prob: pOver,
      implied: impO,
      edge: edgeO,
      odds: overOdds,
      ev: americanToEv(pOver, overOdds),
    };
  }
  if (Number.isFinite(edgeU)) {
    return {
      side: "UNDER",
      model_prob: pUnder,
      implied: impU,
      edge: edgeU,
      odds: underOdds,
      ev: americanToEv(pUnder, underOdds),
    };
  }
  return {
    side: null,
    model_prob: NaN,
    implied: NaN,
    edge: NaN,
    odds: NaN,
    ev: NaN,
  };
}

/**
 * Build per-player hole board for a venue (model-only, no book lines).
 */
export async function buildPlayerHoleBoard(payload, webRoot = WEB) {
  const tables = await loadHolePropsTables(webRoot);
  const courseKey = resolveCourseKey(payload);
  const pars = holeParsFromPayload(payload);
  const coursePar = num(payload?.course_par_18, pars.reduce((a, b) => a + b, 0));
  const round = Math.round(num(payload?.display_round, 1)) || 1;
  const players = (Array.isArray(payload?.players) ? payload.players : []).filter(
    (p) => Math.round(num(p.round, round)) === round || !Number.isFinite(num(p.round, NaN)),
  );

  /** @type {object[]} */
  const holes = [];
  let withHist = 0;
  for (const pl of players) {
    const dg = Math.round(num(pl.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const stp = roundStpForPlayer(pl, coursePar);
    let playerHist = 0;
    /** @type {object[]} */
    const perHole = [];
    for (let h = 1; h <= 18; h++) {
      const proj = projectHoleMean({
        tables,
        courseKey,
        dgId: dg,
        hole: h,
        par: pars[h - 1],
        roundStp: stp,
      });
      if (proj.n > 0) playerHist++;
      perHole.push({ hole: h, ...proj });
    }
    if (playerHist > 0) withHist++;
    const back = projectHoleSum({
      tables,
      courseKey,
      dgId: dg,
      holes: [10, 11, 12, 13, 14, 15, 16, 17, 18],
      pars,
      roundStp: stp,
    });
    const close = projectHoleSum({
      tables,
      courseKey,
      dgId: dg,
      holes: [16, 17, 18],
      pars,
      roundStp: stp,
    });
    holes.push({
      dg_id: dg,
      player: String(pl.player_name || ""),
      round,
      teetime: String(pl.dg_teetime_local || pl.tee_time || pl.teetime || ""),
      holes: perHole,
      holes_10_18: { mu: back.mu, sigma: back.sigma, n_min: back.n_min },
      holes_16_17_18: { mu: close.mu, sigma: close.sigma, n_min: close.n_min },
      hist_holes: playerHist,
    });
  }

  return {
    course_key: courseKey,
    hole_pars: pars,
    round,
    board: holes,
    coverage: {
      players: holes.length,
      with_hole_history: withHist,
      baseline_holes: [...tables.baselines.keys()].filter((k) => k.startsWith(`${courseKey}|`))
        .length,
    },
  };
}

export { HOLE_SIGMA, envNum };
