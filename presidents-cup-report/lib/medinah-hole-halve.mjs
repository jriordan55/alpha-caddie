/**
 * Medinah No. 3 hole-halve (tie) priors for match-play hole props.
 *
 * Sources:
 * - 2012 Ryder Cup @ Medinah: 21 halved holes / 68 foursomes holes (30.9%)
 * - Field scoring PMFs from Medinah ease (adj STP −1.16) + hole pars
 */
import { MEDINAH_HOLE_PARS, MEDINAH_ADJ_STP } from "./data.mjs";
import { holeScoreDistribution, averageHoleDists } from "./hole-dist.mjs";

/** Friday foursomes, 2012 Ryder Cup @ Medinah (4 matches). */
export const MEDINAH_FOURSOMES_HALVE_SESSION = 21 / 68;

const FIELD_AVG = { mu_sg: 1.1, birdies: 4.2, bogeys: 2.6 };

function clamp(x, lo, hi) {
  return Math.max(lo, Math.min(hi, x));
}

function distToScorePmf(dist, par) {
  return new Map([
    [par - 2, dist.eagle],
    [par - 1, dist.birdie],
    [par, dist.par],
    [par + 1, dist.bogey],
    [par + 2, dist.double],
  ]);
}

/** Foursomes (one ball): scores cluster tighter → more halved holes. */
function tightenFoursomesDist(dist) {
  const w = 0.72;
  const birdie = dist.birdie * w;
  const bogey = dist.bogey * w;
  const double = dist.double * w;
  const eagle = dist.eagle * w;
  const par = 1 - birdie - bogey - double - eagle;
  return {
    eagle: Math.max(0, eagle),
    birdie: Math.max(0, birdie),
    par: Math.max(0, par),
    bogey: Math.max(0, bogey),
    double: Math.max(0, double),
  };
}

function probSameScore(pmfA, pmfB) {
  let p = 0;
  for (const [score, pa] of pmfA) p += pa * (pmfB.get(score) || 0);
  return p;
}

function pairSkill(pair) {
  return ((pair[0]?.mu_sg ?? 0) + (pair[1]?.mu_sg ?? 0)) / 2;
}

function foursomesTeamPmf(pair, holeIdx, holePars, courseAdjStp, skillBump = 0) {
  const par = holePars[holeIdx] ?? 4;
  const players = pair.map((p) => ({ ...p, mu_sg: (p.mu_sg ?? 0) + skillBump }));
  const d1 = holeScoreDistribution(players[0], par, holeIdx, courseAdjStp);
  const d2 = holeScoreDistribution(players[1], par, holeIdx, courseAdjStp);
  return distToScorePmf(tightenFoursomesDist(averageHoleDists(d1, d2)), par);
}

function neutralHalveRate(holeIdx, holePars, courseAdjStp) {
  const pmf = foursomesTeamPmf([FIELD_AVG, FIELD_AVG], holeIdx, holePars, courseAdjStp);
  return probSameScore(pmf, pmf);
}

function buildHalveTable(holePars, courseAdjStp) {
  const ryder = MEDINAH_FOURSOMES_HALVE_SESSION;
  return holePars.map((_, holeIdx) => {
    const neutral = neutralHalveRate(holeIdx, holePars, courseAdjStp);
    return clamp(0.15 * ryder + 0.85 * neutral, 0.32, 0.56);
  });
}

export const MEDINAH_HALVE_BY_HOLE = buildHalveTable(MEDINAH_HOLE_PARS, MEDINAH_ADJ_STP);

/**
 * Target halve probability for a foursomes hole, adjusted for pairing skill gap.
 * Even matchups halve more often; large gaps halve less.
 */
export function medinahHoleHalveRate(holeIdx, usaPair, intPair, opts = {}) {
  const holePars = opts.holePars || MEDINAH_HOLE_PARS;
  const courseAdjStp = opts.courseAdjStp ?? MEDINAH_ADJ_STP;
  const idx = Math.max(0, Math.min(holePars.length - 1, holeIdx ?? 0));
  const base = MEDINAH_HALVE_BY_HOLE[idx] ?? MEDINAH_FOURSOMES_HALVE_SESSION;

  if (!usaPair?.length || !intPair?.length) return base;

  const usaBump = usaPair[0]?.team === "USA" ? (opts.usaHomeSg ?? 0) * 0.35 : 0;
  const pmfUsa = foursomesTeamPmf(usaPair, idx, holePars, courseAdjStp, usaBump);
  const pmfInt = foursomesTeamPmf(intPair, idx, holePars, courseAdjStp);
  const simHalve = probSameScore(pmfUsa, pmfInt);

  const gap = Math.abs(pairSkill(usaPair) - pairSkill(intPair));
  const closeness = Math.exp(-0.4 * gap);
  const histWeight = 0.55 + 0.25 * closeness;
  const blended = histWeight * base + (1 - histWeight) * simHalve;
  return clamp(blended, 0.32, 0.58);
}

/** Raise tie mass to Medinah halve target; shrink win probs proportionally. */
export function applyHoleHalvePrior(raw, targetTie) {
  const u = Math.max(0, raw.usa);
  const i = Math.max(0, raw.int);
  const t = Math.max(0, raw.tie);
  const sum = u + i + t;
  if (sum <= 0) {
    const tie = clamp(targetTie, 0.32, 0.58);
    const half = (1 - tie) / 2;
    return { usa: half, int: half, tie };
  }
  const nu = u / sum;
  const ni = i / sum;
  const nt = t / sum;
  const tie = clamp(Math.max(nt, targetTie), 0.32, 0.58);
  if (nt >= tie - 1e-12) return { usa: nu, int: ni, tie: nt };
  const scale = (1 - tie) / (1 - nt);
  return { usa: nu * scale, int: ni * scale, tie };
}
