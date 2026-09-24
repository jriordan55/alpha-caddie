/**
 * Hole-level scoring distributions per DataGolf Ryder Cup blog methodology:
 * adapt round-level ability + counting rates to per-hole eagle/birdie/par/bogey/double probs.
 */

const PAR3_BASE = { eagle: 0.008, birdie: 0.1, par: 0.62, bogey: 0.22, double: 0.05 };
const PAR4_BASE = { eagle: 0.012, birdie: 0.18, par: 0.56, bogey: 0.2, double: 0.048 };
const PAR5_BASE = { eagle: 0.035, birdie: 0.24, par: 0.48, bogey: 0.19, double: 0.045 };

function clamp(x, lo, hi) {
  return Math.max(lo, Math.min(hi, x));
}

function normalize(dist) {
  const keys = ["eagle", "birdie", "par", "bogey", "double"];
  let s = 0;
  for (const k of keys) s += dist[k];
  if (s <= 0) return { ...PAR4_BASE };
  const out = {};
  for (const k of keys) out[k] = dist[k] / s;
  return out;
}

function baseForPar(par) {
  if (par === 3) return { ...PAR3_BASE };
  if (par === 5) return { ...PAR5_BASE };
  return { ...PAR4_BASE };
}

/** Expected strokes on hole h from player skill + Medinah ease. */
export function expectedStrokesOnHole(player, par, holeIdx, courseAdjStp = -1.16) {
  const holeWeight = par === 3 ? 0.88 : par === 5 ? 1.12 : 1.0;
  const skillPerHole = (Number(player.mu_sg) || 0) / 18;
  const coursePerHole = courseAdjStp / 18;
  return par - (skillPerHole + coursePerHole) * holeWeight;
}

export function holeScoreDistribution(player, par, holeIdx, courseAdjStp = -1.16) {
  const exp = expectedStrokesOnHole(player, par, holeIdx, courseAdjStp);
  const edge = exp - par;
  const birdRate = (Number(player.birdies) || 3.8) / 18;
  const bogRate = (Number(player.bogeys) || 2.8) / 18;
  const base = baseForPar(par);

  const skillShift = clamp(-edge * 0.12, -0.12, 0.12);
  const birdBoost = clamp((birdRate - 0.21) * 0.35, -0.08, 0.12);
  const bogCut = clamp((0.16 - bogRate) * 0.35, -0.08, 0.1);

  const dist = {
    eagle: base.eagle * (1 + skillShift * 0.5),
    birdie: clamp(base.birdie + skillShift + birdBoost, 0.04, 0.42),
    par: base.par,
    bogey: clamp(base.bogey - skillShift - bogCut, 0.06, 0.38),
    double: base.double,
  };
  dist.par = clamp(1 - dist.eagle - dist.birdie - dist.bogey - dist.double, 0.25, 0.72);
  return normalize(dist);
}

export function buildPlayerHoleDists(player, holePars, courseAdjStp) {
  return holePars.map((par, i) => holeScoreDistribution(player, par, i, courseAdjStp));
}

/** Foursomes: average partner hole distributions (DG blog). */
export function averageHoleDists(d1, d2) {
  const keys = ["eagle", "birdie", "par", "bogey", "double"];
  const out = {};
  for (const k of keys) out[k] = (d1[k] + d2[k]) / 2;
  return normalize(out);
}

export function sampleScoreFromDist(dist, par, rng = Math.random) {
  const u = rng();
  let c = 0;
  const order = [
    ["eagle", par - 2],
    ["birdie", par - 1],
    ["par", par],
    ["bogey", par + 1],
    ["double", par + 2],
  ];
  for (const [k, strokes] of order) {
    c += dist[k];
    if (u <= c) return strokes;
  }
  return par + 2;
}
