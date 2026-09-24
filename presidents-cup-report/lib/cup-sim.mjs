import { buildPlayerHoleDists } from "./hole-dist.mjs";
import { SESSIONS, CUP_WIN_TARGET, USA_HOME_SG } from "./data.mjs";
import { calibrateCupProb } from "./prescup-history.mjs";
import {
  makeRng,
  pairPlayersByRank,
  assignMatchupsByStrength,
  selectSessionRosterRotated,
  singlesLineup,
  simulateMatch,
} from "./match-sim.mjs";

const WIN_TARGET = CUP_WIN_TARGET;
const USA_RETains_ON_TIE = true;

function skillForSim(player) {
  const bump = player.team === "USA" ? USA_HOME_SG : 0;
  return { ...player, mu_sg: player.mu_sg + bump };
}

export function buildPlayerDistMap(players, holePars, courseAdjStp) {
  const map = new Map();
  for (const p of players) {
    map.set(p.dg_id, buildPlayerHoleDists(skillForSim(p), holePars, courseAdjStp));
  }
  return map;
}

export function simulatePresidentsCup(field, opts = {}) {
  const nSims = Math.max(1000, Number(opts.nSims) || 25000);
  const seed = opts.seed ?? 20260924;
  const rng = makeRng(seed);
  const { usa, intl, hole_pars, course_adj_stp } = field;
  const all = [...usa, ...intl];
  const dists = buildPlayerDistMap(all, hole_pars, course_adj_stp);

  let usaCupWins = 0;
  let intCupWins = 0;
  let tieRetain = 0;
  const scoreBands = new Map();
  const sessionWins = Object.fromEntries(SESSIONS.map((s) => [s.id, { usa: 0, int: 0, halve: 0 }]));
  const playerPoints = new Map(all.map((p) => [p.dg_id, 0]));
  const singlesWin = new Map(all.map((p) => [p.dg_id, 0]));
  const singlesPlayed = new Map(all.map((p) => [p.dg_id, 0]));
  const teamSitCounts = new Map(all.map((p) => [p.dg_id, 0]));
  const teamSessionsPlayed = new Map(all.map((p) => [p.dg_id, 0]));
  const matchesPlayed = new Map(all.map((p) => [p.dg_id, 0]));
  const topScorerCount = new Map(all.map((p) => [p.dg_id, 0]));
  const usaTopScorerCount = new Map(usa.map((p) => [p.dg_id, 0]));
  const intTopScorerCount = new Map(intl.map((p) => [p.dg_id, 0]));
  const pointsAtLeast = new Map(all.map((p) => [p.dg_id, { ge2: 0, ge3: 0, ge4: 0 }]));

  const playerIndex = new Map(all.map((p, i) => [p.dg_id, i]));
  const sessionIndex = new Map(SESSIONS.map((s, i) => [s.id, i]));
  const nPlayers = all.length;
  const nSessions = SESSIONS.length;
  const pointSnapshots = new Float32Array(nSims * nPlayers);
  const sessionSnapshots = new Float32Array(nSims * nSessions * 2);
  const cupSnapshots = new Float32Array(nSims * 2);
  const holeWinSnapshots = new Float32Array(nSims * nPlayers);
  const holeWinFrontSnapshots = new Float32Array(nSims * nPlayers);
  const holeWinBackSnapshots = new Float32Array(nSims * nPlayers);
  const teamHoleSnapshots = new Float32Array(nSims * 2);
  const teamHoleFrontSnapshots = new Float32Array(nSims * 2);
  const teamHoleBackSnapshots = new Float32Array(nSims * 2);
  const halvedMatchSnapshots = new Uint16Array(nSims);

  const addSimPts = (simPts, id, pts) => {
    simPts.set(id, (simPts.get(id) || 0) + pts);
  };

  const mergeHoleWins = (simHole, src) => {
    if (!src) return;
    for (const [id, n] of src.entries()) {
      const idx = playerIndex.get(id);
      if (idx == null) continue;
      simHole[idx] += n;
    }
  };

  for (let sim = 0; sim < nSims; sim++) {
    let usaPts = 0;
    let intPts = 0;
    const simPts = new Map(all.map((p) => [p.dg_id, 0]));
    const simHoles = new Float32Array(nPlayers);
    const simHolesFront = new Float32Array(nPlayers);
    const simHolesBack = new Float32Array(nPlayers);
    let halvedMatches = 0;
    let sessIdx = 0;
    for (const session of SESSIONS) {
      let sessUsa = 0;
      let sessInt = 0;
      if (session.format === "singles") {
        for (const { usa: u, int: i } of singlesLineup(usa, intl, session.matches)) {
          const res = simulateMatch("singles", u, i, hole_pars, dists, rng);
          if (res.winner === "halve") halvedMatches++;
          mergeHoleWins(simHoles, res.holeWins);
          mergeHoleWins(simHolesFront, res.frontWins);
          mergeHoleWins(simHolesBack, res.backWins);
          usaPts += res.usaPoints;
          intPts += res.intPoints;
          sessUsa += res.usaPoints;
          sessInt += res.intPoints;
          singlesPlayed.set(u.dg_id, singlesPlayed.get(u.dg_id) + 1);
          singlesPlayed.set(i.dg_id, singlesPlayed.get(i.dg_id) + 1);
          matchesPlayed.set(u.dg_id, matchesPlayed.get(u.dg_id) + 1);
          matchesPlayed.set(i.dg_id, matchesPlayed.get(i.dg_id) + 1);
          if (res.winner === "USA") {
            addSimPts(simPts, u.dg_id, 1);
            singlesWin.set(u.dg_id, singlesWin.get(u.dg_id) + 1);
          } else if (res.winner === "INT") {
            addSimPts(simPts, i.dg_id, 1);
            singlesWin.set(i.dg_id, singlesWin.get(i.dg_id) + 1);
          } else {
            addSimPts(simPts, u.dg_id, 0.5);
            addSimPts(simPts, i.dg_id, 0.5);
          }
        }
      } else {
        const nPlay = session.matches * 2;
        const uRoster = selectSessionRosterRotated(usa, nPlay, rng, teamSitCounts);
        const iRoster = selectSessionRosterRotated(intl, nPlay, rng, teamSitCounts);
        const playedIds = new Set([...uRoster, ...iRoster].map((p) => p.dg_id));
        for (const p of all) {
          if (playedIds.has(p.dg_id)) {
            teamSessionsPlayed.set(p.dg_id, teamSessionsPlayed.get(p.dg_id) + 1);
          } else {
            teamSitCounts.set(p.dg_id, teamSitCounts.get(p.dg_id) + 1);
          }
        }
        const uPairs = pairPlayersByRank(uRoster, session.format);
        const iPairs = pairPlayersByRank(iRoster, session.format);
        const matchups = assignMatchupsByStrength(uPairs, iPairs);
        for (const { usa: uPair, int: iPair } of matchups) {
          for (const p of [...uPair, ...iPair]) matchesPlayed.set(p.dg_id, matchesPlayed.get(p.dg_id) + 1);
          const res = simulateMatch(session.format, uPair, iPair, hole_pars, dists, rng);
          if (res.winner === "halve") halvedMatches++;
          mergeHoleWins(simHoles, res.holeWins);
          mergeHoleWins(simHolesFront, res.frontWins);
          mergeHoleWins(simHolesBack, res.backWins);
          usaPts += res.usaPoints;
          intPts += res.intPoints;
          sessUsa += res.usaPoints;
          sessInt += res.intPoints;
          for (const p of uPair) {
            if (res.winner === "USA") addSimPts(simPts, p.dg_id, 1);
            else if (res.winner === "halve") addSimPts(simPts, p.dg_id, 0.5);
          }
          for (const p of iPair) {
            if (res.winner === "INT") addSimPts(simPts, p.dg_id, 1);
            else if (res.winner === "halve") addSimPts(simPts, p.dg_id, 0.5);
          }
        }
      }
      if (sessUsa > sessInt) sessionWins[session.id].usa++;
      else if (sessInt > sessUsa) sessionWins[session.id].int++;
      else sessionWins[session.id].halve++;

      const sBase = sim * nSessions * 2 + sessIdx * 2;
      sessionSnapshots[sBase] = sessUsa;
      sessionSnapshots[sBase + 1] = sessInt;
      sessIdx++;
    }

    for (const p of all) {
      const pts = simPts.get(p.dg_id) || 0;
      playerPoints.set(p.dg_id, playerPoints.get(p.dg_id) + pts);
      const band = pointsAtLeast.get(p.dg_id);
      if (pts >= 2) band.ge2++;
      if (pts >= 3) band.ge3++;
      if (pts >= 4) band.ge4++;
    }
    let bestPts = -1;
    const leaders = [];
    for (const p of all) {
      const pts = simPts.get(p.dg_id) || 0;
      if (pts > bestPts) {
        bestPts = pts;
        leaders.length = 0;
        leaders.push(p.dg_id);
      } else if (Math.abs(pts - bestPts) < 0.001) {
        leaders.push(p.dg_id);
      }
    }
    for (const id of leaders) topScorerCount.set(id, topScorerCount.get(id) + 1 / leaders.length);

    for (const roster of [usa, intl]) {
      const counts = roster === usa ? usaTopScorerCount : intTopScorerCount;
      let bestTeamPts = -1;
      const teamLeaders = [];
      for (const p of roster) {
        const pts = simPts.get(p.dg_id) || 0;
        if (pts > bestTeamPts) {
          bestTeamPts = pts;
          teamLeaders.length = 0;
          teamLeaders.push(p.dg_id);
        } else if (Math.abs(pts - bestTeamPts) < 0.001) {
          teamLeaders.push(p.dg_id);
        }
      }
      for (const id of teamLeaders) counts.set(id, counts.get(id) + 1 / teamLeaders.length);
    }

    const band = `${Math.round(usaPts)}-${Math.round(intPts)}`;
    scoreBands.set(band, (scoreBands.get(band) || 0) + 1);

    const pBase = sim * nPlayers;
    for (const p of all) {
      pointSnapshots[pBase + playerIndex.get(p.dg_id)] = simPts.get(p.dg_id) || 0;
    }
    holeWinSnapshots.set(simHoles, pBase);
    holeWinFrontSnapshots.set(simHolesFront, pBase);
    holeWinBackSnapshots.set(simHolesBack, pBase);

    let usaHoles = 0;
    let intHoles = 0;
    let usaHolesFront = 0;
    let intHolesFront = 0;
    let usaHolesBack = 0;
    let intHolesBack = 0;
    for (const p of usa) {
      const idx = playerIndex.get(p.dg_id);
      usaHoles += simHoles[idx] || 0;
      usaHolesFront += simHolesFront[idx] || 0;
      usaHolesBack += simHolesBack[idx] || 0;
    }
    for (const p of intl) {
      const idx = playerIndex.get(p.dg_id);
      intHoles += simHoles[idx] || 0;
      intHolesFront += simHolesFront[idx] || 0;
      intHolesBack += simHolesBack[idx] || 0;
    }
    const tBase = sim * 2;
    teamHoleSnapshots[tBase] = usaHoles;
    teamHoleSnapshots[tBase + 1] = intHoles;
    teamHoleFrontSnapshots[tBase] = usaHolesFront;
    teamHoleFrontSnapshots[tBase + 1] = intHolesFront;
    teamHoleBackSnapshots[tBase] = usaHolesBack;
    teamHoleBackSnapshots[tBase + 1] = intHolesBack;

    cupSnapshots[sim * 2] = usaPts;
    cupSnapshots[sim * 2 + 1] = intPts;
    halvedMatchSnapshots[sim] = halvedMatches;

    if (usaPts >= WIN_TARGET && usaPts > intPts) usaCupWins++;
    else if (intPts >= WIN_TARGET && intPts > usaPts) intCupWins++;
    else if (Math.abs(usaPts - intPts) < 0.01 && USA_RETains_ON_TIE) tieRetain++;
    else if (usaPts > intPts) usaCupWins++;
    else if (intPts > usaPts) intCupWins++;
    else tieRetain++;
  }

  const cupProbRaw = {
    usa_win: usaCupWins / nSims,
    int_win: intCupWins / nSims,
    usa_retain_tie: tieRetain / nSims,
  };

  const teamAbility = {
    usa: usa.reduce((s, p) => s + p.mu_sg, 0) / usa.length,
    int: intl.reduce((s, p) => s + p.mu_sg, 0) / intl.length,
  };

  const calibrated = calibrateCupProb(cupProbRaw, teamAbility.usa, teamAbility.int);
  const cupProb = {
    usa_win: calibrated.usa_win,
    int_win: calibrated.int_win,
    usa_retain_tie: calibrated.usa_retain_tie,
  };

  const expPointsById = new Map();
  const topScorer = [...all]
    .map((p) => ({
      ...p,
      exp_points: playerPoints.get(p.dg_id) / nSims,
      exp_matches: matchesPlayed.get(p.dg_id) / nSims,
      exp_team_sessions: teamSessionsPlayed.get(p.dg_id) / nSims,
      top_scorer_prob: topScorerCount.get(p.dg_id) / nSims,
      usa_top_scorer_prob: p.team === "USA" ? usaTopScorerCount.get(p.dg_id) / nSims : 0,
      int_top_scorer_prob: p.team === "INT" ? intTopScorerCount.get(p.dg_id) / nSims : 0,
      prob_ge2_pts: pointsAtLeast.get(p.dg_id).ge2 / nSims,
      prob_ge3_pts: pointsAtLeast.get(p.dg_id).ge3 / nSims,
      prob_ge4_pts: pointsAtLeast.get(p.dg_id).ge4 / nSims,
      singles_win_pct: singlesPlayed.get(p.dg_id) ? singlesWin.get(p.dg_id) / singlesPlayed.get(p.dg_id) : 0,
    }))
    .sort((a, b) => b.exp_points - a.exp_points);
  for (const p of topScorer) expPointsById.set(p.dg_id, p.exp_points);

  return {
    nSims,
    nPlayers,
    nSessions,
    playerIndex,
    sessionIndex,
    allPlayerIds: all.map((p) => p.dg_id),
    pointSnapshots,
    sessionSnapshots,
    cupSnapshots,
    holeWinSnapshots,
    holeWinFrontSnapshots,
    holeWinBackSnapshots,
    teamHoleSnapshots,
    teamHoleFrontSnapshots,
    teamHoleBackSnapshots,
    halvedMatchSnapshots,
    cupProb,
    cupProbRaw,
    tieCalibration: calibrated.calibration,
    sessionWins: Object.fromEntries(
      Object.entries(sessionWins).map(([k, v]) => [
        k,
        { usa: v.usa / nSims, int: v.int / nSims, halve: v.halve / nSims },
      ]),
    ),
    scoreBands: [...scoreBands.entries()]
      .sort((a, b) => b[1] - a[1])
      .slice(0, 15)
      .map(([score, count]) => ({ score, prob: count / nSims })),
    topScorer,
    expPointsById,
    abilityTable: all
      .map((p) => ({ name: p.name, team: p.team, mu_sg: p.mu_sg }))
      .sort((a, b) => b.mu_sg - a.mu_sg),
    teamAbility,
  };
}

/** Singles H2H win prob from 5000 quick match sims (for pricing vs DK). */
export function simulateSinglesH2H(playerA, playerB, holePars, courseAdjStp, n = 5000, seed = 1) {
  const rng = makeRng(seed + playerA.dg_id * 13 + playerB.dg_id);
  const dists = buildPlayerDistMap([playerA, playerB], holePars, courseAdjStp);
  let aWin = 0;
  let halve = 0;
  for (let i = 0; i < n; i++) {
    const res = simulateMatch("singles", playerA, playerB, holePars, dists, rng);
    if (res.winner === "USA" || res.winner === "INT") {
      const winnerId = res.winner === "USA" ? playerA.dg_id : playerB.dg_id;
      if (winnerId === playerA.dg_id) aWin++;
    } else halve++;
  }
  return { aWin: aWin / n, halve: halve / n, bWin: 1 - aWin / n - halve / n };
}
