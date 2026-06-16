#!/usr/bin/env node
/**
 * Alpha Caddie tournament outright Monte Carlo (mirrors round_projections.R shifted log-normal path).
 * Rating per golfer-round: μ_SG + round_sd from projections (not summed total_score strokes).
 */
export const TOURNAMENT_MC_MU_MULT = [1.0, 0.99, 0.97, 0.95];
export const TOURNAMENT_MC_SD_MULT = [1.0, 1.01, 1.03, 1.05];
export const TOURNAMENT_MC_FORM_SD = 0.25;

export function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

export function makeRng(seed = 42) {
  let s = seed >>> 0;
  return () => {
    s = (s * 1664525 + 1013904223) >>> 0;
    return s / 4294967296;
  };
}

export function randStdNormal(rng) {
  let u = 0;
  let v = 0;
  while (u === 0) u = rng();
  while (v === 0) v = rng();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}

export function drawShiftedLognormal(rng, meanX, sdX, sdlog = 0.45) {
  let mx = num(meanX, 0);
  let sx = num(sdX, NaN);
  if (!Number.isFinite(sx) || sx <= 0) return mx;
  let sl = num(sdlog, 0.45);
  sl = clamp(sl, 0.05, 1.5);
  if (!Number.isFinite(sl) || sl <= 0) return mx + randStdNormal(rng) * sx;
  const cv = Math.sqrt(Math.exp(sl * sl) - 1);
  if (!Number.isFinite(cv) || cv <= 0) return mx + randStdNormal(rng) * sx;
  const meanY = sx / cv;
  if (!Number.isFinite(meanY) || meanY <= 0) return mx + randStdNormal(rng) * sx;
  const ml = Math.log(meanY) - 0.5 * sl * sl;
  const eps = Math.exp(ml + sl * randStdNormal(rng)) - meanY;
  return mx + eps;
}

export function modeledMuSgFromRow(row) {
  const mu = num(row?.mu_sg, NaN);
  const implied = num(row?.implied_mu_sg, NaN);
  if (Number.isFinite(implied) && Number.isFinite(mu) && Math.abs(mu) < 1e-9 && Math.abs(implied) > 1e-9) {
    return implied;
  }
  if (Number.isFinite(mu)) return mu;
  if (Number.isFinite(implied)) return implied;
  const stp = num(row?.score_to_par, NaN);
  if (Number.isFinite(stp)) return -stp;
  return NaN;
}

/** Composite skill for one round row (export / within-event form; no browser-only pricing hooks). */
export function tournamentRatingFromRow(row, coursePar18 = 70) {
  let muSg = modeledMuSgFromRow(row);
  if (!Number.isFinite(muSg)) return null;
  const form = num(row?.within_event_form_shift, 0);
  if (Number.isFinite(form) && form !== 0) muSg += form;
  const roundSd = clamp(num(row?.round_sd, 2.75), 2.0, 3.5);
  return { muSg: clamp(muSg, -4, 4), roundSd };
}

export function tournamentScoringBasis(meta, coursePar18) {
  const par = Math.round(num(coursePar18, NaN)) || 70;
  const basis = meta?.projection_course_basis || {};
  const baseline = num(basis.venue_avg_round_score, NaN);
  const avgStp = num(basis.venue_avg_score_to_par, NaN);
  return {
    par,
    baseline: Number.isFinite(baseline) ? baseline : par + 3.2,
    avgStp: Number.isFinite(avgStp) ? avgStp : 3.2,
  };
}

function playerRoundRow(players, dgId, rnd) {
  const id = Math.round(num(dgId, NaN));
  const r = Math.round(num(rnd, NaN));
  return (
    players.find((p) => Math.round(num(p.dg_id)) === id && Math.round(num(p.round)) === r) || null
  );
}

function liveRoundPartialStp(row, par) {
  const thru = Math.round(num(row?.dg_live_thru ?? row?.thru, NaN));
  const today = num(row?.dg_live_today ?? row?.today, NaN);
  if (!Number.isFinite(thru) || thru < 1 || thru >= 18) return 0;
  if (!Number.isFinite(today)) return 0;
  return today;
}

function simPlayerTournamentStp(entry, formShock, ctx, rng) {
  const { baseline, avgStp, liveR, hasLive, lognSdlog, muMult, sdMult } = ctx;
  const { id, currentScore, ratings } = entry;

  if (!hasLive || !Number.isFinite(currentScore)) {
    let totalGross = 0;
    let r1Gross = NaN;
    for (let r = 1; r <= 4; r++) {
      const rt = ratings[r - 1];
      if (!rt) return null;
      const sg = drawShiftedLognormal(
        rng,
        rt.muSg * muMult[r - 1] + formShock,
        rt.roundSd * sdMult[r - 1],
        lognSdlog,
      );
      const gross = baseline + (avgStp - sg);
      if (r === 1) r1Gross = gross;
      totalGross += gross;
    }
    return { totalStp: totalGross - ctx.par * 4, r1Stp: Number.isFinite(r1Gross) ? r1Gross - ctx.par : NaN };
  }

  let totalStp = currentScore;
  let r1Stp = NaN;
  const startR = Math.max(1, Math.min(4, liveR));

  for (let r = startR; r <= 4; r++) {
    const rt = ratings[r - 1];
    if (!rt) return null;
    const sg = drawShiftedLognormal(
      rng,
      rt.muSg * muMult[r - 1] + formShock,
      rt.roundSd * sdMult[r - 1],
      lognSdlog,
    );
    const roundStp = avgStp - sg;
    if (r === 1 && startR === 1) r1Stp = roundStp;

    if (r === startR && hasLive) {
      const liveRow = playerRoundRow(ctx.players, id, r);
      const partial = liveRoundPartialStp(liveRow, ctx.par);
      totalStp += roundStp - partial;
    } else {
      totalStp += roundStp;
      if (r === 1) r1Stp = roundStp;
    }
  }
  return { totalStp, r1Stp };
}

/**
 * @param {object} proj projections.json root
 * @param {object} [opts]
 */
export function runTournamentMcFromProjections(proj, opts = {}) {
  const players = Array.isArray(proj?.players) ? proj.players : [];
  const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : {};
  const scoring = tournamentScoringBasis(meta, proj.course_par_18 ?? meta.course_par_18);
  const nSimsRaw = Math.round(num(opts.nSims ?? meta.outright_model_mc_sims, 420));
  const nSims = Number.isFinite(nSimsRaw) && nSimsRaw >= 100 ? Math.min(2500, nSimsRaw) : 420;
  const lognRaw = num(opts.lognSdlog ?? meta.outright_model_mc_logn_sdlog, 0.45);
  const lognSdlog = clamp(Number.isFinite(lognRaw) ? lognRaw : 0.45, 0.05, 1.5);
  const seed = Math.round(num(opts.seed, 42));
  const rng = makeRng(seed);
  const liveR = Math.max(1, Math.min(4, Math.round(num(meta.datagolf_live_current_round, 1)) || 1));
  const hasLive = meta.datagolf_live_in_tournament === true || Number.isFinite(num(meta.datagolf_live_last_update, NaN));

  const field = [];
  const seen = new Set();
  for (const p of players) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || seen.has(id)) continue;
    if (Math.round(num(p.round)) !== 1) continue;
    seen.add(id);

    const ratings = [];
    let ok = true;
    for (let rnd = 1; rnd <= 4; rnd++) {
      const row = playerRoundRow(players, id, rnd);
      const rt = row ? tournamentRatingFromRow(row, scoring.par) : null;
      if (!rt) {
        ok = false;
        break;
      }
      ratings.push(rt);
    }
    if (!ok) continue;

    let currentScore = NaN;
    for (const pr of players) {
      if (Math.round(num(pr.dg_id)) !== id) continue;
      const s = num(pr.current_score ?? pr.currentScore, NaN);
      if (Number.isFinite(s)) {
        currentScore = s;
        break;
      }
    }

    field.push({
      id,
      name: String(p.player_name || ""),
      ratings,
      currentScore,
    });
  }

  if (field.length < 5) {
    return { field: [], maps: {}, nSims: 0 };
  }

  const cutLine =
    Number.isFinite(num(opts.cutLine, NaN)) && num(opts.cutLine) > 0
      ? Math.round(num(opts.cutLine))
      : Math.min(65, Math.max(10, Math.floor(field.length * 0.42)));

  const mk = () => new Map(field.map((f) => [f.id, 0]));
  const winC = mk();
  const c5 = mk();
  const c10 = mk();
  const c20 = mk();
  const cCut = mk();
  const cFrl = mk();

  const ctx = {
    ...scoring,
    liveR,
    hasLive,
    lognSdlog,
    muMult: TOURNAMENT_MC_MU_MULT,
    sdMult: TOURNAMENT_MC_SD_MULT,
    players,
  };

  for (let rep = 0; rep < nSims; rep++) {
    const formShocks = field.map(() => drawShiftedLognormal(rng, 0, TOURNAMENT_MC_FORM_SD, lognSdlog));
    const perf = [];
    for (let i = 0; i < field.length; i++) {
      const sim = simPlayerTournamentStp(field[i], formShocks[i], ctx, rng);
      if (!sim) continue;
      perf.push({ id: field[i].id, totalStp: sim.totalStp, r1Stp: sim.r1Stp });
    }
    if (perf.length < 5) continue;

    perf.sort((a, b) => {
      const d = a.totalStp - b.totalStp;
      if (d !== 0) return d;
      return rng() - 0.5;
    });
    winC.set(perf[0].id, winC.get(perf[0].id) + 1);
    const n = perf.length;
    for (let i = 0; i < Math.min(5, n); i++) c5.set(perf[i].id, c5.get(perf[i].id) + 1);
    for (let i = 0; i < Math.min(10, n); i++) c10.set(perf[i].id, c10.get(perf[i].id) + 1);
    for (let i = 0; i < Math.min(20, n); i++) c20.set(perf[i].id, c20.get(perf[i].id) + 1);
    for (let i = 0; i < Math.min(cutLine, n); i++) cCut.set(perf[i].id, cCut.get(perf[i].id) + 1);

    perf.sort((a, b) => {
      const d = a.r1Stp - b.r1Stp;
      if (d !== 0) return d;
      return rng() - 0.5;
    });
    cFrl.set(perf[0].id, cFrl.get(perf[0].id) + 1);
  }

  const clampProb = (p) => clamp(p, 0.001, 0.95);
  const maps = { win: winC, top_5: c5, top_10: c10, top_20: c20, make_cut: cCut, frl: cFrl };
  for (const m of Object.values(maps)) {
    for (const [id, c] of m) m.set(id, clampProb(c / nSims));
  }
  return { field, maps, nSims, cutLine, scoring };
}
