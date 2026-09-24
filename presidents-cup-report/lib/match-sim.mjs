import { averageHoleDists, sampleScoreFromDist } from "./hole-dist.mjs";

function mulberry32(seed) {
  return function rng() {
    let t = (seed += 0x6d2b79f5);
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

export function makeRng(seed) {
  if (seed == null) return Math.random;
  return mulberry32(Number(seed) >>> 0);
}

/** Play probability by rank 1..12 (Presidents Cup captains rotate the bench). */
export function playProbability(rankIndex, teamSize = 12) {
  const r = Math.max(1, Math.min(teamSize, rankIndex));
  return 0.55 + (0.4 * (teamSize - r)) / (teamSize - 1);
}

function weightedSampleWithoutReplacement(pool, nPlay, rng) {
  const picked = [];
  const remaining = [...pool];
  while (picked.length < nPlay && remaining.length) {
    const total = remaining.reduce((s, x) => s + x.w, 0);
    let u = rng() * total;
    let idx = 0;
    for (; idx < remaining.length; idx++) {
      u -= remaining[idx].w;
      if (u <= 0) break;
    }
    idx = Math.min(idx, remaining.length - 1);
    picked.push(remaining[idx].p);
    remaining.splice(idx, 1);
  }
  return picked;
}

export function selectSessionRoster(teamPlayers, nPlay, rng) {
  const ranked = [...teamPlayers].sort((a, b) => b.mu_sg - a.mu_sg);
  const pool = ranked.map((p, i) => ({ p, w: playProbability(i + 1, ranked.length) }));
  return weightedSampleWithoutReplacement(pool, nPlay, rng);
}

/** Weighted roster with rotation: prioritize players who sat recent team sessions. */
export function selectSessionRosterRotated(teamPlayers, nPlay, rng, teamSitCounts) {
  const ranked = [...teamPlayers].sort((a, b) => b.mu_sg - a.mu_sg);
  const pool = ranked.map((p, i) => {
    const sits = teamSitCounts.get(p.dg_id) || 0;
    let w = playProbability(i + 1, ranked.length);
    w *= 1 + 0.75 * sits;
    if (sits >= 2) w *= 1.35;
    if (sits >= 3) w *= 2.2;
    return { p, w };
  });
  return weightedSampleWithoutReplacement(pool, nPlay, rng);
}

function shuffle(arr, rng) {
  const a = [...arr];
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(rng() * (i + 1));
    [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

export function pairPlayers(players, rng) {
  const sh = shuffle(players, rng);
  const pairs = [];
  for (let i = 0; i + 1 < sh.length; i += 2) pairs.push([sh[i], sh[i + 1]]);
  return pairs;
}

function pairStrength(pair) {
  return (pair[0]?.mu_sg ?? 0) + (pair[1]?.mu_sg ?? 0);
}

/** Foursomes: 1+2, 3+4. Fourball: 1+12, 2+11 (star anchors weak partner). */
export function pairPlayersByRank(players, format = "foursomes") {
  const ranked = [...players].sort((a, b) => b.mu_sg - a.mu_sg);
  const pairs = [];
  if (format === "fourball") {
    let lo = ranked.length - 1;
    for (let hi = 0; hi < lo; hi++, lo--) pairs.push([ranked[hi], ranked[lo]]);
    return pairs;
  }
  for (let i = 0; i + 1 < ranked.length; i += 2) pairs.push([ranked[i], ranked[i + 1]]);
  return pairs;
}

/** USA strongest pairs vs INT weakest (captain mismatch strategy). */
export function assignMatchupsByStrength(usaPairs, intPairs) {
  const u = [...usaPairs].sort((a, b) => pairStrength(b) - pairStrength(a));
  const v = [...intPairs].sort((a, b) => pairStrength(a) - pairStrength(b));
  const n = Math.min(u.length, v.length);
  return Array.from({ length: n }, (_, i) => ({ usa: u[i], int: v[i] }));
}

/** Top roster by skill (captains play their best). */
export function selectSessionRosterTop(teamPlayers, nPlay) {
  return [...teamPlayers].sort((a, b) => b.mu_sg - a.mu_sg).slice(0, nPlay);
}

/** Sunday singles: USA stars vs INT bottom half (standard anchor strategy). */
export function singlesLineup(usa, intl, n) {
  const uR = [...usa].sort((a, b) => b.mu_sg - a.mu_sg).slice(0, n);
  const iR = [...intl].sort((a, b) => b.mu_sg - a.mu_sg).slice(0, n);
  return uR.map((u, i) => ({ usa: u, int: iR[iR.length - 1 - i] }));
}

/**
 * Simulate one match. Returns { winner: 'USA'|'INT'|'halve', usaPoints, intPoints }
 */
function creditHoleWin(holeWins, frontWins, backWins, playerIds, holeIdx) {
  const target = holeIdx < 9 ? frontWins : backWins;
  for (const id of playerIds) {
    holeWins.set(id, (holeWins.get(id) || 0) + 1);
    target.set(id, (target.get(id) || 0) + 1);
  }
}

export function simulateMatch(format, usaSide, intSide, holePars, playerDists, rng, maxHoles = 18) {
  let usaUp = 0;
  let h = 0;
  const holeWins = new Map();
  const frontWins = new Map();
  const backWins = new Map();
  while (h < maxHoles) {
    let usaScore;
    let intScore;
    let usaPlayers = [];
    let intPlayers = [];
    if (format === "singles") {
      usaScore = sampleScoreFromDist(playerDists.get(usaSide.dg_id)[h], holePars[h], rng);
      intScore = sampleScoreFromDist(playerDists.get(intSide.dg_id)[h], holePars[h], rng);
      usaPlayers = [usaSide.dg_id];
      intPlayers = [intSide.dg_id];
    } else if (format === "foursomes") {
      const [u1, u2] = usaSide;
      const [i1, i2] = intSide;
      const uDist = averageHoleDists(playerDists.get(u1.dg_id)[h], playerDists.get(u2.dg_id)[h]);
      const iDist = averageHoleDists(playerDists.get(i1.dg_id)[h], playerDists.get(i2.dg_id)[h]);
      usaScore = sampleScoreFromDist(uDist, holePars[h], rng);
      intScore = sampleScoreFromDist(iDist, holePars[h], rng);
      usaPlayers = [u1.dg_id, u2.dg_id];
      intPlayers = [i1.dg_id, i2.dg_id];
    } else if (format === "fourball") {
      const [u1, u2] = usaSide;
      const [i1, i2] = intSide;
      usaScore = Math.min(
        sampleScoreFromDist(playerDists.get(u1.dg_id)[h], holePars[h], rng),
        sampleScoreFromDist(playerDists.get(u2.dg_id)[h], holePars[h], rng),
      );
      intScore = Math.min(
        sampleScoreFromDist(playerDists.get(i1.dg_id)[h], holePars[h], rng),
        sampleScoreFromDist(playerDists.get(i2.dg_id)[h], holePars[h], rng),
      );
      usaPlayers = [u1.dg_id, u2.dg_id];
      intPlayers = [i1.dg_id, i2.dg_id];
    } else {
      throw new Error(`Unknown format: ${format}`);
    }
    if (usaScore < intScore) creditHoleWin(holeWins, frontWins, backWins, usaPlayers, h);
    else if (usaScore > intScore) creditHoleWin(holeWins, frontWins, backWins, intPlayers, h);
    h++;
    if (usaScore < intScore) usaUp++;
    else if (usaScore > intScore) usaUp--;
    const left = maxHoles - h;
    if (Math.abs(usaUp) > left) break;
  }
  if (usaUp > 0) return { winner: "USA", usaPoints: 1, intPoints: 0, holeWins, frontWins, backWins };
  if (usaUp < 0) return { winner: "INT", usaPoints: 0, intPoints: 1, holeWins, frontWins, backWins };
  return { winner: "halve", usaPoints: 0.5, intPoints: 0.5, holeWins, frontWins, backWins };
}

/** Win/tie rates for a fixed pairing (Session 1 foursomes). */
export function simulatePairMatchProb(
  format,
  usaPair,
  intPair,
  holePars,
  playerDists,
  { n = 3000, seed = 1, holeIdx = null } = {},
) {
  const rng = makeRng(seed + usaPair[0].dg_id * 17 + intPair[0].dg_id * 31 + (holeIdx ?? 99) * 7);
  let usa = 0;
  let intl = 0;
  let tie = 0;
  for (let i = 0; i < n; i++) {
    if (holeIdx == null) {
      const res = simulateMatch(format, usaPair, intPair, holePars, playerDists, rng);
      if (res.winner === "USA") usa++;
      else if (res.winner === "INT") intl++;
      else tie++;
      continue;
    }
    const h = holeIdx;
    let usaScore;
    let intScore;
    if (format === "foursomes") {
      const uDist = averageHoleDists(playerDists.get(usaPair[0].dg_id)[h], playerDists.get(usaPair[1].dg_id)[h]);
      const iDist = averageHoleDists(playerDists.get(intPair[0].dg_id)[h], playerDists.get(intPair[1].dg_id)[h]);
      usaScore = sampleScoreFromDist(uDist, holePars[h], rng);
      intScore = sampleScoreFromDist(iDist, holePars[h], rng);
    } else if (format === "fourball") {
      usaScore = Math.min(
        sampleScoreFromDist(playerDists.get(usaPair[0].dg_id)[h], holePars[h], rng),
        sampleScoreFromDist(playerDists.get(usaPair[1].dg_id)[h], holePars[h], rng),
      );
      intScore = Math.min(
        sampleScoreFromDist(playerDists.get(intPair[0].dg_id)[h], holePars[h], rng),
        sampleScoreFromDist(playerDists.get(intPair[1].dg_id)[h], holePars[h], rng),
      );
    } else {
      usaScore = sampleScoreFromDist(playerDists.get(usaPair[0].dg_id)[h], holePars[h], rng);
      intScore = sampleScoreFromDist(playerDists.get(intPair[0].dg_id)[h], holePars[h], rng);
    }
    if (usaScore < intScore) usa++;
    else if (usaScore > intScore) intl++;
    else tie++;
  }
  const t = n;
  return { usa: usa / t, int: intl / t, tie: tie / t };
}
