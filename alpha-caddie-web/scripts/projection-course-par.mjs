/**
 * Single source of truth for course par on projections.json and player rows.
 */
import { num } from "./round-projection-mu.mjs";
import { recalcProjectionScoresForCoursePar } from "./course-round-adjustments.mjs";

export function parFromHoleParsArray(holePars) {
  if (!Array.isArray(holePars) || holePars.length !== 18) return NaN;
  const sum = holePars.reduce((s, p) => s + Math.round(num(p, 4)), 0);
  return Number.isFinite(sum) && sum >= 68 && sum <= 73 ? sum : NaN;
}

/** Resolve par 18 from payload (hole card sum wins when present). */
export function readCoursePar18(payload) {
  const fromHoles = parFromHoleParsArray(payload?.hole_pars);
  const fromMeta = Math.round(num(payload?.course_par_18 ?? payload?.meta?.course_par_18, NaN));
  if (Number.isFinite(fromHoles)) return fromHoles;
  if (Number.isFinite(fromMeta) && fromMeta >= 68 && fromMeta <= 73) return fromMeta;
  return NaN;
}

export function writeCoursePar18(payload, coursePar18) {
  const par = Math.round(num(coursePar18, NaN));
  if (!Number.isFinite(par)) return par;
  payload.course_par_18 = par;
  if (payload.meta && typeof payload.meta === "object") payload.meta.course_par_18 = par;
  return par;
}

export function syncProjectionPlayerCoursePar(payload, coursePar18Opt) {
  const par = Math.round(num(coursePar18Opt ?? readCoursePar18(payload), NaN));
  if (!Number.isFinite(par)) return 0;
  writeCoursePar18(payload, par);
  let n = 0;
  for (const pl of payload?.players || []) {
    if (!pl || typeof pl !== "object") continue;
    pl.course_par = par;
    n++;
  }
  return n;
}

/** Fix total_score ↔ score_to_par using course_par_18 (not implicit 72). */
export function repairProjectionScoreParCoherence(payload, coursePar18Opt) {
  const par = Math.round(num(coursePar18Opt ?? readCoursePar18(payload), NaN));
  if (!Number.isFinite(par)) return { fixed: 0, par: NaN };
  let fixed = 0;
  for (const pl of payload?.players || []) {
    if (!pl || typeof pl !== "object") continue;
    pl.course_par = par;
    const stp = num(pl.score_to_par, NaN);
    const ts = num(pl.total_score, NaN);
    if (Number.isFinite(stp)) {
      const expected = Math.round((par + stp) * 100) / 100;
      if (!Number.isFinite(ts) || Math.abs(ts - expected) > 0.03) {
        pl.total_score = expected;
        fixed++;
      }
    } else if (Number.isFinite(ts)) {
      pl.score_to_par = Math.round((ts - par) * 100) / 100;
      pl.total_score = Math.round((par + pl.score_to_par) * 100) / 100;
      fixed++;
    }
  }
  return { fixed, par };
}

/**
 * Lock par from hole card, recalc when par changed, stamp player rows, repair score coherence.
 */
export function ensureProjectionCoursePar(payload, opts = {}) {
  const parFromHoles = parFromHoleParsArray(payload?.hole_pars);
  let coursePar = readCoursePar18(payload);
  const oldPar = Math.round(num(payload?.course_par_18, NaN));

  if (!Number.isFinite(coursePar)) {
    return { ok: false, reason: "missing course_par_18 and hole_pars", coursePar18: NaN, recalcRows: 0, fixed: 0 };
  }

  if (Number.isFinite(parFromHoles) && parFromHoles !== coursePar) {
    coursePar = parFromHoles;
  }

  let recalcRows = 0;
  if (Number.isFinite(oldPar) && oldPar !== coursePar) {
    const res = recalcProjectionScoresForCoursePar(payload, coursePar, oldPar);
    recalcRows = res.rows || 0;
  } else {
    writeCoursePar18(payload, coursePar);
  }

  const stamped = syncProjectionPlayerCoursePar(payload, coursePar);
  const { fixed } = repairProjectionScoreParCoherence(payload, coursePar);

  const failOnMismatch = opts.failOnMismatch === true;
  const displayRound =
    Math.round(num(payload?.display_round ?? payload?.datagolf_field_current_round, NaN)) || 1;
  const roundRows = (payload?.players || []).filter((p) => Math.round(num(p.round, NaN)) === displayRound);
  let bad = 0;
  for (const pl of roundRows) {
    const stp = num(pl.score_to_par, NaN);
    const ts = num(pl.total_score, NaN);
    if (!Number.isFinite(stp) || !Number.isFinite(ts)) {
      bad++;
      continue;
    }
    if (Math.abs(ts - (coursePar + stp)) > 0.05) bad++;
  }

  if (failOnMismatch && bad > 0) {
    return {
      ok: false,
      reason: `${bad} R${displayRound} row(s) still incoherent after par repair`,
      coursePar18: coursePar,
      recalcRows,
      fixed,
      stamped,
      bad,
    };
  }

  return { ok: true, coursePar18: coursePar, recalcRows, fixed, stamped, bad };
}
