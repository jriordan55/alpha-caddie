/**
 * Prior-round course difficulty + within-event form for fetch-datagolf round projections.
 * Mirrors round_projections.R (GOLF_WITHIN_EVENT_FORM_*) and app.js liveCourseDifficultyDForMu.
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { liveHoleStatsUsableForProjections } from "./dg-live-hole-pars.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function clamp(x, lo, hi) {
  return Math.max(lo, Math.min(hi, x));
}

/** Sum_h (avg_score − par) for one round from preds/live-hole-stats (same as app.js). */
export function liveCourseRoundExcessForRound(payload, roundNum, minThru = 4, courseKeyOpt) {
  if (!payload || typeof payload !== "object") return NaN;
  const courses = payload.courses;
  if (!Array.isArray(courses) || !courses.length) return NaN;
  const rn = Math.round(num(roundNum, NaN));
  if (!Number.isFinite(rn) || rn < 1 || rn > 4) return NaN;
  const ckWant = courseKeyOpt ? normCourseNameKey(courseKeyOpt) : "";

  const perCourse = [];
  for (const c of courses) {
    if (ckWant) {
      const ck = normCourseNameKey(c.course_name ?? c.courseName ?? "");
      if (!ck || ck !== ckWant) continue;
    }
    const rounds = c.rounds;
    if (!Array.isArray(rounds)) continue;
    let sum = 0;
    let nh = 0;
    for (const rr of rounds) {
      if (Math.round(num(rr.round_num, NaN)) !== rn) continue;
      const holes = rr.holes;
      if (!Array.isArray(holes)) continue;
      for (const h of holes) {
        const par = num(h.par, NaN);
        const total = h.total && typeof h.total === "object" ? h.total : {};
        const avg = num(total.avg_score, NaN);
        const th = num(total.players_thru, NaN);
        if (!Number.isFinite(par) || !Number.isFinite(avg)) continue;
        if (Number.isFinite(th) && th < minThru) continue;
        sum += avg - par;
        nh++;
      }
    }
    if (nh > 0) perCourse.push(sum);
  }
  if (!perCourse.length) return NaN;
  if (perCourse.length === 1) return perCourse[0];
  const mean = perCourse.reduce((a, b) => a + b, 0) / perCourse.length;
  const mx = Math.max(...perCourse);
  return mean + 0.5 * (mx - mean);
}

/** Mean excess strokes for completed prior rounds 1..targetRound-1 (live hole stats). */
export function priorRoundsExcessFromLiveHoleStats(liveHoleStats, targetRound, minThru = 4, courseKeyOpt) {
  const tr = Math.round(num(targetRound, NaN));
  if (!Number.isFinite(tr) || tr < 2) return NaN;
  const exs = [];
  for (let rn = 1; rn < tr; rn++) {
    const ex = liveCourseRoundExcessForRound(liveHoleStats, rn, minThru, courseKeyOpt);
    if (Number.isFinite(ex)) exs.push(ex);
  }
  if (!exs.length) return NaN;
  return exs.reduce((a, b) => a + b, 0) / exs.length;
}

/**
 * @typedef {{ byRound: Map<number, { n: number, sumStp: number }>, playerRounds: { dg_id: number, round: number, sg_total: number }[] }} EventRoundContext
 */

/** Stream historical_rounds_all for current event — field scoring vs par by round + player SG rows. */
export async function loadEventRoundContextFromHistoricalCsv(csvPath, eventName, courseKeyOpt) {
  const ctx = {
    byRound: new Map(),
    playerRounds: [],
  };
  if (!eventName || !csvPath || !existsSync(csvPath)) return ctx;

  const cy = new Date().getFullYear();
  const ckWant = courseKeyOpt ? normCourseNameKey(courseKeyOpt) : "";

  await new Promise((resolve, reject) => {
    const parser = createReadStream(csvPath).pipe(
      parse({
        columns: true,
        relax_quotes: true,
        relax_column_count: true,
        skip_records_with_error: true,
      }),
    );
    parser.on("data", (row) => {
      if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) return;
      if (ckWant) {
        const ckRow = normCourseNameKey(row.course_name || row.Course_Name || "");
        if (!ckRow || ckRow !== ckWant) return;
      }
      const yr = parseInt(row.year, 10);
      if (Number.isFinite(yr) && yr !== cy) return;

      const rnd = Math.round(num(row.round_num, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return;

      const cp = num(row.course_par, NaN);
      const rs = num(row.round_score, NaN);
      if (Number.isFinite(cp) && Number.isFinite(rs)) {
        const stp = rs - cp;
        const b = ctx.byRound.get(rnd) || {
          n: 0,
          sumStp: 0,
          sumBird: 0,
          nBird: 0,
          sumBog: 0,
          nBog: 0,
          sumGir: 0,
          nGir: 0,
        };
        b.n++;
        b.sumStp += stp;
        const bird = num(row.birdies, NaN);
        if (Number.isFinite(bird) && bird >= 0 && bird <= 18) {
          b.sumBird += bird;
          b.nBird++;
        }
        const bog = num(row.bogies, NaN);
        if (Number.isFinite(bog) && bog >= 0 && bog <= 18) {
          b.sumBog += bog;
          b.nBog++;
        }
        const gir = girOrFwToCount(row.gir, 18);
        if (Number.isFinite(gir)) {
          b.sumGir += gir;
          b.nGir++;
        }
        ctx.byRound.set(rnd, b);
      }

      const dg = Math.round(num(row.dg_id, NaN));
      const sg = num(row.sg_total, NaN);
      if (Number.isFinite(dg) && Number.isFinite(sg)) {
        const pr = { dg_id: dg, round: rnd, sg_total: sg };
        const bird = num(row.birdies, NaN);
        const bog = num(row.bogies, NaN);
        const gir = girOrFwToCount(row.gir, 18);
        if (Number.isFinite(bird)) pr.birdies = bird;
        if (Number.isFinite(bog)) pr.bogeys = bog;
        if (Number.isFinite(gir)) pr.gir = gir;
        ctx.playerRounds.push(pr);
      }
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });

  return ctx;
}

/** Field mean (round_score − course_par) for rounds strictly before targetRound. */
export function priorRoundsExcessFromHistorical(ctx, targetRound) {
  const tr = Math.round(num(targetRound, NaN));
  if (!Number.isFinite(tr) || tr < 2 || !ctx?.byRound) return NaN;
  const exs = [];
  for (let rn = 1; rn < tr; rn++) {
    const b = ctx.byRound.get(rn);
    if (b && b.n > 0) exs.push(b.sumStp / b.n);
  }
  if (!exs.length) return NaN;
  return exs.reduce((a, x) => a + x, 0) / exs.length;
}

/** Blend live-hole-stats vs historical field excess (prefer live when both exist). */
export function blendedPriorRoundCourseExcess(liveHoleStats, histCtx, targetRound, eventName, courseKeyOpt) {
  const ck = courseKeyOpt ? normCourseNameKey(courseKeyOpt) : "";
  const liveOk =
    liveHoleStats &&
    liveHoleStatsUsableForProjections(liveHoleStats, eventName, courseKeyOpt || "");
  const liveEx = liveOk ? priorRoundsExcessFromLiveHoleStats(liveHoleStats, targetRound, 4, ck || undefined) : NaN;
  const histEx = priorRoundsExcessFromHistorical(histCtx, targetRound);
  if (Number.isFinite(liveEx) && Number.isFinite(histEx)) return 0.55 * liveEx + 0.45 * histEx;
  if (Number.isFinite(liveEx)) return liveEx;
  if (Number.isFinite(histEx)) return histEx;
  return NaN;
}

/** Stroke shift for total_score (+ = harder course vs par). Matches app.js O/U scaling. */
export function courseDifficultyStrokeShift(excessStrokes) {
  const exR = num(excessStrokes, NaN);
  if (!Number.isFinite(exR) || exR === 0) return 0;
  const kHard = num(process.env.GOLF_COURSE_PRIOR_ROUND_K_HARD, 1.5);
  const kEasy = num(process.env.GOLF_COURSE_PRIOR_ROUND_K_EASY, 0.8);
  const k = exR < 0 ? kEasy : kHard;
  const lo = num(process.env.GOLF_COURSE_PRIOR_ROUND_CLAMP_NEG, -1.2);
  const hi = num(process.env.GOLF_COURSE_PRIOR_ROUND_CLAMP_POS, 2.15);
  return clamp(exR * k, lo, hi);
}

/**
 * Map key `${dg_id}|${round}` → within_form_shift (SG) for rounds 2–4.
 * @param {EventRoundContext} ctx
 * @param {{ dg_id: number, mu_sg: number }[]} basePlayers
 */
/**
 * preds/in-play `R1`…`R4` gross when historical_rounds_all has not caught up yet this week.
 * sg_total proxy = −(round_score − par); surplus vs pre-tournament base μ_SG matches buildWithinEventFormMap.
 */
export function augmentEventContextWithInPlayRounds(ctx, inPlayRows, coursePar18, basePlayers) {
  if (!ctx || !Array.isArray(ctx.playerRounds) || !Array.isArray(inPlayRows)) return ctx;
  const par = num(coursePar18, NaN);
  if (!Number.isFinite(par)) return ctx;
  const baseMu = new Map();
  for (const p of basePlayers || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(id)) baseMu.set(id, num(p.mu_sg, 0));
  }
  const seen = new Set(ctx.playerRounds.map((pr) => `${Math.round(num(pr.dg_id, NaN))}|${Math.round(num(pr.round, NaN))}`));
  for (const row of inPlayRows) {
    if (!row || typeof row !== "object") continue;
    const id = Math.round(num(row.dg_id ?? row.dgId, NaN));
    if (!Number.isFinite(id) || !baseMu.has(id)) continue;
    for (let rnd = 1; rnd <= 3; rnd++) {
      const key = `${id}|${rnd}`;
      if (seen.has(key)) continue;
      const g = num(row[`R${rnd}`] ?? row[`r${rnd}`], NaN);
      if (!Number.isFinite(g) || g <= 0) continue;
      const sgProxy = -(g - par);
      ctx.playerRounds.push({ dg_id: id, round: rnd, sg_total: sgProxy });
      seen.add(key);
    }
  }
  return ctx;
}

/** Fast live-week context: in-play R* gross + live_round_actuals (no historical CSV scan). */
export function buildEventContextFromLiveBundle(live, coursePar18, basePlayers, actualsByDg = null) {
  const ctx = { playerRounds: [], byRound: new Map() };
  const par = num(coursePar18, NaN);
  if (!Number.isFinite(par)) return ctx;
  augmentEventContextWithInPlayRounds(ctx, live?.data || [], par, basePlayers);
  const baseMu = new Map();
  for (const p of basePlayers || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(id)) baseMu.set(id, num(p.mu_sg, 0));
  }
  const seen = new Set(
    ctx.playerRounds.map((pr) => `${Math.round(num(pr.dg_id, NaN))}|${Math.round(num(pr.round, NaN))}`),
  );
  const actuals = actualsByDg && typeof actualsByDg === "object" ? actualsByDg : {};
  for (const [dgKey, perRound] of Object.entries(actuals)) {
    const id = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(id) || !baseMu.has(id)) continue;
    if (!perRound || typeof perRound !== "object") continue;
    for (const [rndKey, act] of Object.entries(perRound)) {
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      const key = `${id}|${rnd}`;
      if (seen.has(key)) continue;
      let sg = num(act?.sg_total, NaN);
      const rs = num(act?.round_score, NaN);
      if (!Number.isFinite(sg) && Number.isFinite(rs) && rs > 0) sg = -(rs - par);
      if (!Number.isFinite(sg)) continue;
      ctx.playerRounds.push({ dg_id: id, round: rnd, sg_total: sg });
      seen.add(key);
    }
  }
  return ctx;
}

/** dg_ids with at least one DraftKings round O/U row in projections.props. */
export function draftKingsDgIdsFromProjections(proj) {
  const props = Array.isArray(proj?.props) ? proj.props : [];
  /** @type {Set<number>} */
  const dgIds = new Set();
  for (const r of props) {
    if (String(r?.source || "").trim().toLowerCase() !== "draftkings") continue;
    const dg = Math.round(num(r.dg_id, NaN));
    if (Number.isFinite(dg) && dg > 0) dgIds.add(dg);
  }
  return dgIds;
}

export function buildWithinEventFormMap(
  ctx,
  basePlayers,
  k = 0.02,
  cap = 0.3,
  fieldShare = WITHIN_EVENT_FORM_FIELD_SHARE,
  dgFilterForField = null,
) {
  const map = new Map();
  if (!k || !ctx?.playerRounds?.length) return map;

  const baseMu = new Map();
  for (const p of basePlayers || []) {
    const id = Math.round(num(p.dg_id));
    if (Number.isFinite(id)) baseMu.set(id, num(p.mu_sg, 0));
  }

  const byDgRound = new Map();
  /** @type {Map<number, { sum: number, n: number }>} */
  const fieldSurplusByRound = new Map();
  for (const pr of ctx.playerRounds) {
    const id = Math.round(num(pr.dg_id));
    const rnd = Math.round(num(pr.round));
    if (!Number.isFinite(id) || rnd < 1 || rnd > 3) continue;
    const base = baseMu.get(id);
    if (!Number.isFinite(base)) continue;
    const surplus = num(pr.sg_total, NaN) - base;
    if (!Number.isFinite(surplus)) continue;
    byDgRound.set(`${id}|${rnd}`, surplus);
    if (dgFilterForField && !dgFilterForField.has(id)) continue;
    const bucket = fieldSurplusByRound.get(rnd) || { sum: 0, n: 0 };
    bucket.sum += surplus;
    bucket.n += 1;
    fieldSurplusByRound.set(rnd, bucket);
  }

  const minFieldN = dgFilterForField ? 8 : 12;
  const fieldMeanSurplusByRound = new Map();
  for (const [rnd, b] of fieldSurplusByRound) {
    if (b.n >= minFieldN) fieldMeanSurplusByRound.set(rnd, b.sum / b.n);
  }

  const fs = Number.isFinite(num(fieldShare, NaN)) ? num(fieldShare, NaN) : WITHIN_EVENT_FORM_FIELD_SHARE;

  for (const [id] of baseMu) {
    for (let tr = 2; tr <= 4; tr++) {
      let sh = 0;
      for (let rn = 1; rn < tr; rn++) {
        const playerSur = byDgRound.get(`${id}|${rn}`);
        const fieldSur = fieldMeanSurplusByRound.get(rn);
        let target = playerSur;
        if (Number.isFinite(fieldSur) && Number.isFinite(playerSur)) {
          target = fs * fieldSur + (1 - fs) * playerSur;
        } else if (Number.isFinite(fieldSur)) {
          target = fieldSur;
        } else if (!Number.isFinite(playerSur)) {
          continue;
        }
        sh += k * target;
      }
      if (!Number.isFinite(sh)) sh = 0;
      sh = clamp(sh, -cap, cap);
      map.set(`${id}|${tr}`, sh);
    }
  }
  return map;
}

/**
 * This-week counting actuals from preds/in-play `live_round_actuals_by_dg` (when history JSON lags).
 * @param {Record<string, Record<string, object>>} actualsByDg
 * @returns {Map<number, Map<number, object>>}
 */
export function buildWithinEventCountingMapFromLiveActuals(
  actualsByDg,
  coursePar18,
  venueBirdies,
  venueBogeys,
) {
  /** @type {Map<number, Map<number, object>>} */
  const out = new Map();
  if (!actualsByDg || typeof actualsByDg !== "object") return out;
  const cp = num(coursePar18, NaN);
  for (const [dgKey, perRound] of Object.entries(actualsByDg)) {
    const dg = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    for (const [rndKey, act] of Object.entries(perRound)) {
      if (!act || typeof act !== "object") continue;
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      const rs = num(act.round_score, NaN);
      if (!Number.isFinite(rs) || rs <= 0) continue;
      let rec = {
        birdies: num(act.birdies, NaN),
        bogeys: num(act.bogeys ?? act.bogies, NaN),
        gir: girOrFwToCount(act.gir, 18),
        round_score: rs,
        eagles: num(act.eagles, NaN),
        doubles: num(act.doubles, NaN),
      };
      if (Number.isFinite(cp)) {
        rec = reconcileHoleCountsFromScore(rec, cp, num(venueBirdies, 2.88), num(venueBogeys, 2.93));
      }
      if (!Number.isFinite(rec.birdies) && !Number.isFinite(rec.bogeys) && !Number.isFinite(rec.gir)) continue;
      let per = out.get(dg);
      if (!per) {
        per = new Map();
        out.set(dg, per);
      }
      per.set(rnd, rec);
    }
  }
  return out;
}

/**
 * @typedef {{
 *   avgScore: number,
 *   avgStp: number,
 *   n: number,
 *   avgBirdies: number,
 *   avgPars: number,
 *   avgBogeys: number,
 *   avgEagles: number,
 *   avgDoubles: number,
 *   avgGir: number,
 *   avgFairways: number,
 *   avgPutts: number,
 * }} VenueScoreAgg
 * @typedef {{
 *   venueAvgStp: number,
 *   venueAvgScore: number,
 *   nVenueRounds: number,
 *   source: string,
 *   venueAvgBirdies: number,
 *   venueAvgPars: number,
 *   venueAvgBogeys: number,
 *   venueAvgGir: number,
 *   venueAvgFairways: number,
 *   venueAvgPutts: number,
 *   fieldByRound: Map<number, VenueScoreAgg>,
 *   playerByRound: Map<string, VenueScoreAgg>,
 *   playerByVenue: Map<number, VenueScoreAgg>,
 *   courseFitByDg: Map<number, { avgSg: number, n: number }>,
 * }} VenueHistoricalScoring
 */

function emptyVenueCountRaw() {
  return {
    sumScore: 0,
    sumStp: 0,
    n: 0,
    sumBird: 0,
    nBird: 0,
    sumPar: 0,
    nPar: 0,
    sumBog: 0,
    nBog: 0,
    sumEag: 0,
    nEag: 0,
    sumDbl: 0,
    nDbl: 0,
    sumGir: 0,
    nGir: 0,
    sumFw: 0,
    nFw: 0,
    sumPutts: 0,
    nPutts: 0,
  };
}

function girOrFwToCount(raw, nHoles) {
  const v = num(raw, NaN);
  if (!Number.isFinite(v)) return NaN;
  const nh = num(nHoles, 18);
  if (v > 0 && v <= 1.0001) return v * nh;
  if (v > 1 && v <= nh + 0.51) return v;
  if (v > nh && v <= 100) return (v / 100) * nh;
  return NaN;
}

function accumulateVenueCountRow(raw, row, nFairwayHoles = 14) {
  const cp = num(row.course_par, NaN);
  const rs = num(row.round_score, NaN);
  if (!Number.isFinite(cp) || !Number.isFinite(rs)) return raw;
  raw.n++;
  raw.sumScore += rs;
  raw.sumStp += rs - cp;

  const b = num(row.birdies, NaN);
  if (Number.isFinite(b) && b >= 0 && b <= 18) {
    raw.sumBird += b;
    raw.nBird++;
  }
  const p = num(row.pars, NaN);
  if (Number.isFinite(p) && p >= 0 && p <= 18) {
    raw.sumPar += p;
    raw.nPar++;
  }
  const bg = num(row.bogies, NaN);
  if (Number.isFinite(bg) && bg >= 0 && bg <= 18) {
    raw.sumBog += bg;
    raw.nBog++;
  }
  const e = num(row.eagles_or_better, NaN);
  if (Number.isFinite(e) && e >= 0 && e <= 6) {
    raw.sumEag += e;
    raw.nEag++;
  }
  const d = num(row.doubles_or_worse, NaN);
  if (Number.isFinite(d) && d >= 0 && d <= 10) {
    raw.sumDbl += d;
    raw.nDbl++;
  }
  const gc = girOrFwToCount(row.gir, 18);
  if (Number.isFinite(gc)) {
    raw.sumGir += gc;
    raw.nGir++;
  }
  const fc = girOrFwToCount(row.driving_acc, nFairwayHoles);
  if (Number.isFinite(fc)) {
    raw.sumFw += fc;
    raw.nFw++;
  }
  const put = num(row.putts, NaN);
  if (Number.isFinite(put) && put >= 20 && put <= 40) {
    raw.sumPutts += put;
    raw.nPutts++;
  }
  return raw;
}

function finalizeVenueAgg(raw) {
  const mean = (sum, n) => (n > 0 ? sum / n : NaN);
  return {
    avgScore: mean(raw.sumScore, raw.n),
    avgStp: mean(raw.sumStp, raw.n),
    n: raw.n,
    avgBirdies: mean(raw.sumBird, raw.nBird),
    avgPars: mean(raw.sumPar, raw.nPar),
    avgBogeys: mean(raw.sumBog, raw.nBog),
    avgEagles: mean(raw.sumEag, raw.nEag),
    avgDoubles: mean(raw.sumDbl, raw.nDbl),
    avgGir: mean(raw.sumGir, raw.nGir),
    avgFairways: mean(raw.sumFw, raw.nFw),
    avgPutts: mean(raw.sumPutts, raw.nPutts),
  };
}

/** Nudge bird/bog toward target score-vs-par (pars residual). */
export function softAlignHoleCountsToStp(counts, targetStp, strength = 0.58) {
  const e = Math.max(0, num(counts.eagles, 0));
  const d = Math.max(0, num(counts.doubles, 0));
  let b = num(counts.birdies, 0);
  let p = num(counts.pars, 0);
  let bg = num(counts.bogeys, 0);
  const t = num(targetStp, 0);
  const st = Math.max(0.08, Math.min(1, strength));
  const hat = -b - 2 * e + bg + 2 * d;
  const diff = t - hat;
  const delta = (st * diff) / 2;
  b = Math.max(0.15, b - delta);
  bg = Math.max(0.15, bg + delta);
  p = 18 - e - d - b - bg;
  if (p < 0.12) {
    const need = 0.12 - p;
    const take = Math.min(need / 2, b - 0.15, bg - 0.15);
    b -= take;
    bg -= take;
    p = 18 - e - d - b - bg;
  }
  const s = e + b + p + bg + d;
  if (s > 0.01 && Math.abs(s - 18) > 0.01) {
    const k = 18 / s;
    return { eagles: e * k, birdies: b * k, pars: p * k, bogeys: bg * k, doubles: d * k };
  }
  return { eagles: e, birdies: b, pars: Math.max(0.12, p), bogeys: bg, doubles: d };
}

/**
 * Tie bird/bog/pars/GIR/FW to projected total score (score_to_par anchor).
 * Used after within-event blend, pin sheet, and weather so O/U markets stay correlated.
 */
export function reconcileProjectionRowCountsToScore(row, opts = {}) {
  if (!row || typeof row !== "object") return row;
  const par18 = Math.round(num(opts.coursePar18 ?? row.course_par, NaN)) || 72;
  const stpRaw = num(row.score_to_par, NaN);
  const stp = Number.isFinite(stpRaw)
    ? stpRaw
    : Number.isFinite(num(row.total_score, NaN))
      ? num(row.total_score, NaN) - par18
      : Number.isFinite(num(row.mu_sg, NaN))
        ? -num(row.mu_sg, NaN)
        : NaN;
  if (!Number.isFinite(stp)) return row;

  const e = Math.max(0, num(row.eagles, 0));
  const d = Math.max(0, num(row.doubles, 0));
  let b = num(row.birdies, NaN);
  let bg = num(row.bogeys, NaN);
  let p = num(row.pars, NaN);
  if (!Number.isFinite(b) || !Number.isFinite(bg)) {
    const vBird = num(opts.venueAvgBirdies, 4.2);
    const vBog = num(opts.venueAvgBogeys, 2.1);
    const split = inferHoleCountsFromScoreSplit(stp, vBird, vBog);
    b = split.birdies;
    bg = split.bogeys;
    p = split.pars;
  } else {
    const aligned = softAlignHoleCountsToStp(
      { eagles: e, birdies: b, pars: p, bogeys: bg, doubles: d },
      stp,
      num(opts.alignStrength, 0.52),
    );
    b = aligned.birdies;
    bg = aligned.bogeys;
    p = aligned.pars;
    const spread = spreadParsIntoBirdBogPairs(
      { eagles: e, birdies: b, pars: p, bogeys: bg, doubles: d },
      {
        venueBirdies: num(opts.venueAvgBirdies, 4.2),
        venueBogeys: num(opts.venueAvgBogeys, 2.1),
        venuePars: num(opts.venueAvgPars, 11.2),
        spreadStrength: num(opts.spreadStrength, 0.58),
      },
    );
    b = spread.birdies;
    bg = spread.bogeys;
    p = spread.pars;
  }

  const venueGir = num(opts.venueAvgGir, 12);
  const venueFw = num(opts.venueAvgFairways, 9);
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const girFromScore = clamp(venueGir - stp * 0.82, 7.5, 16.2);
  const fwFromScore = clamp(venueFw - stp * 0.48, 4, nFw + 0.2);
  const girBlend = num(opts.girBlend, 0.48);
  const fwBlend = num(opts.fairwaysBlend, 0.45);
  let gir = num(row.gir, NaN);
  let fairways = num(row.fairways, NaN);
  if (Number.isFinite(gir)) gir = (1 - girBlend) * gir + girBlend * girFromScore;
  else gir = girFromScore;
  if (Number.isFinite(fairways)) fairways = (1 - fwBlend) * fairways + fwBlend * fwFromScore;
  else fairways = fwFromScore;

  row.eagles = Math.round(e * 1000) / 1000;
  row.birdies = Math.round(b * 100) / 100;
  row.pars = Math.round(p * 100) / 100;
  row.bogeys = Math.round(bg * 100) / 100;
  row.doubles = Math.round(d * 1000) / 1000;
  row.gir = Math.round(gir * 100) / 100;
  row.fairways = Math.round(fairways * 100) / 100;
  if (Number.isFinite(num(row.putts, NaN))) {
    row.putts = Math.round(clamp(num(row.putts, NaN), 24, 34) * 100) / 100;
  }
  return row;
}

/** Reconcile every projection row to its total_score / μ anchor. */
export function reconcileAllProjectionPlayerRows(payload, opts = {}) {
  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : payload;
  if (!meta.projection_course_basis || typeof meta.projection_course_basis !== "object") {
    meta.projection_course_basis = payload?.projection_course_basis || {};
  }
  ensureProjectionCourseBasisComplete(meta.projection_course_basis, payload);
  if (payload?.projection_course_basis !== meta.projection_course_basis) {
    payload.projection_course_basis = meta.projection_course_basis;
  }
  const basis = meta.projection_course_basis;
  const coursePar18 = Math.round(num(payload?.course_par_18 ?? meta?.course_par_18, NaN)) || 72;
  const recOpts = {
    coursePar18,
    venueAvgBirdies: num(basis.venue_avg_birdies, 4.2),
    venueAvgBogeys: num(basis.venue_avg_bogeys, 2.1),
    venueAvgGir: num(basis.venue_avg_gir, 12),
    venueAvgFairways: num(basis.venue_avg_fairways, 9),
    venueAvgPars: num(basis.venue_avg_pars, 11.2),
    nFairwayHoles: Math.round(num(basis.fairway_holes_modeled, 14)) || 14,
    alignStrength: 0.52,
    spreadStrength: 0.58,
    girBlend: 0.48,
    fairwaysBlend: 0.45,
    ...opts,
  };
  let n = 0;
  for (const pl of payload?.players || []) {
    if (!pl || typeof pl !== "object") continue;
    reconcileProjectionRowCountsToScore(pl, recOpts);
    n++;
  }
  if (meta?.projection_round_adjustments && typeof meta.projection_round_adjustments === "object") {
    meta.projection_round_adjustments.projection_counts_coherent = true;
  }
  const cal = calibrateProjectionFieldMarkets(payload, {
    dgFilter: opts.dgFilter,
    minField: opts.minField,
    skipCalibrate: opts.skipFieldCalibrate,
  });
  return { reconciled: n, calibrated: cal };
}

/**
 * Move mass from pars into matched bird+bog pairs (score-to-par neutral: −δ − δ + δ + δ = 0).
 * Books show more bird/bog volatility vs “all pars” profiles at the same projected score.
 */
export function spreadParsIntoBirdBogPairs(counts, opts = {}) {
  const e = Math.max(0, num(counts?.eagles, 0));
  const d = Math.max(0, num(counts?.doubles, 0));
  let b = num(counts?.birdies, 0);
  let bg = num(counts?.bogeys, 0);
  let p = num(counts?.pars, NaN);
  if (!Number.isFinite(p)) p = 18 - e - d - b - bg;

  const venueBird = num(opts?.venueBirdies, 3.2);
  const venueBog = num(opts?.venueBogeys, 2.9);
  const venuePar = num(opts?.venuePars, 11.5);
  const strength = num(opts?.spreadStrength, 0.55);
  const parFloor = num(opts?.parFloor, Math.max(9.8, venuePar * 0.88));

  const parsExcess = Math.max(0, p - parFloor);
  const pairRoom = parsExcess / 2;
  let shift = Math.min(strength * pairRoom, pairRoom);
  if (shift <= 1e-6) {
    return { eagles: e, birdies: b, pars: Math.max(parFloor, p), bogeys: bg, doubles: d };
  }
  b += shift;
  bg += shift;
  p -= 2 * shift;
  // Residual nudge toward venue bird/bog rates when pars still heavy
  const parSlack = Math.max(0, p - parFloor);
  const birdShort = Math.max(0, venueBird - b);
  const bogShort = Math.max(0, venueBog - bg);
  const extra = Math.min(parSlack / 2, birdShort, bogShort, 0.45);
  if (extra > 1e-6) {
    b += extra;
    bg += extra;
    p -= 2 * extra;
  }
  return {
    eagles: e,
    birdies: Math.max(0.15, b),
    pars: Math.max(parFloor, p),
    bogeys: Math.max(0.15, bg),
    doubles: d,
  };
}

function coalesceVenueCount(playerVal, fieldVal, skillVal) {
  if (Number.isFinite(playerVal)) return playerVal;
  if (Number.isFinite(fieldVal)) return fieldVal;
  return skillVal;
}

function envNum(name, fallback) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return fallback;
  const n = Number(raw);
  return Number.isFinite(n) ? n : fallback;
}

/** Venue CSV year window — include prior US Opens / setups (not only the latest ~8 seasons). */
function venueHistoryMinYear(calendarYear) {
  const cy = Math.round(num(calendarYear, new Date().getFullYear()));
  const lookback = Math.round(envNum("GOLF_VENUE_HIST_LOOKBACK_YEARS", 22));
  return Math.max(2000, cy - lookback);
}

/**
 * Round-bucket venue mean shrunk toward full-course historical average.
 * Dampens noisy R1/R4 spikes (e.g. 2018 Shinnecock R1 +6.5 vs venue +4.4 all-years).
 */
export function historicalVenueStpTargetForRound(venueScoring, round) {
  const overall = num(venueScoring?.venueAvgStp, NaN);
  const rnd = Math.round(num(round, NaN));
  if (!Number.isFinite(overall) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) {
    return overall;
  }
  const fr = venueScoring?.fieldByRound?.get(rnd);
  const roundStp = num(fr?.avgStp, NaN);
  const nRound = Math.round(num(fr?.n, 0));
  const minField = Math.round(envNum("GOLF_VENUE_ROUND_STP_MIN_N", 25));
  if (!Number.isFinite(roundStp) || nRound < minField) return overall;

  const maxDev = envNum("GOLF_VENUE_ROUND_STP_MAX_DEV", 0.95);
  const cappedRound = overall + clamp(roundStp - overall, -maxDev, maxDev);
  const shrinkK = envNum("GOLF_VENUE_ROUND_STP_SHRINK_K", 96);
  const wRound = clamp(nRound / (nRound + shrinkK), 0.38, 0.72);
  return wRound * cappedRound + (1 - wRound) * overall;
}

/** Shift total_score / score_to_par without changing μ_SG (venue calibration, not skill revision). */
export function shiftProjectionRowScore(row, strokeShift, coursePar18) {
  if (!row || typeof row !== "object") return;
  if (!Number.isFinite(strokeShift) || Math.abs(strokeShift) < 1e-6) return;
  const par18 = Math.round(num(coursePar18, NaN)) || 72;
  const stp = num(row.score_to_par, NaN);
  const ts = num(row.total_score, NaN);
  if (Number.isFinite(stp)) {
    row.score_to_par = Math.round((stp + strokeShift) * 100) / 100;
    row.total_score = Math.round((par18 + row.score_to_par) * 100) / 100;
  } else if (Number.isFinite(ts)) {
    row.total_score = Math.round((ts + strokeShift) * 100) / 100;
    row.score_to_par = Math.round((row.total_score - par18) * 100) / 100;
  }
}

/** Re-anchor gross totals when course_par_18 changes (score_to_par unchanged). */
export function recalcProjectionScoresForCoursePar(payload, newPar18, oldPar18Opt) {
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const newPar = Math.round(num(newPar18, NaN));
  if (!players.length || !Number.isFinite(newPar) || newPar < 68 || newPar > 73) {
    return { rows: 0, newPar };
  }
  const oldPar = Math.round(num(oldPar18Opt ?? payload?.course_par_18, NaN));
  let rows = 0;
  for (const pl of players) {
    if (!pl || typeof pl !== "object") continue;
    const stp = num(pl.score_to_par, NaN);
    const ts = num(pl.total_score, NaN);
    if (Number.isFinite(stp)) {
      pl.total_score = Math.round((newPar + stp) * 100) / 100;
      rows++;
    } else if (Number.isFinite(ts) && Number.isFinite(oldPar)) {
      pl.score_to_par = Math.round((ts - newPar) * 100) / 100;
      pl.total_score = Math.round((newPar + pl.score_to_par) * 100) / 100;
      rows++;
    }
  }
  payload.course_par_18 = newPar;
  if (payload.meta && typeof payload.meta === "object") payload.meta.course_par_18 = newPar;
  if (Number.isFinite(oldPar) && oldPar !== newPar) {
    const basis = payload.projection_course_basis || payload.meta?.projection_course_basis;
    if (basis && typeof basis === "object" && Number.isFinite(num(basis.venue_avg_score_to_par, NaN))) {
      const vstp = num(basis.venue_avg_score_to_par, NaN);
      basis.venue_avg_round_score = Math.round((newPar + vstp) * 100) / 100;
    }
  }
  return { rows, newPar, oldPar: Number.isFinite(oldPar) ? oldPar : null };
}

/**
 * After weather / unified factors, nudge field-average score-to-par toward shrunk historical venue targets.
 */
export function calibrateProjectionScoresToHistoricalVenue(payload, venueScoring, opts = {}) {
  if (String(process.env.GOLF_HIST_VENUE_SCORE_CALIB ?? "0").trim() === "0") {
    return { rounds: 0, shifts: {} };
  }
  const players = Array.isArray(payload?.players) ? payload.players : [];
  if (!players.length || !venueScoring || !Number.isFinite(num(venueScoring.venueAvgStp, NaN))) {
    return { rounds: 0, shifts: {} };
  }

  const coursePar18 = Math.round(num(payload.course_par_18, NaN)) || 72;
  const dgFilter = opts.dgFilter instanceof Set ? opts.dgFilter : null;
  const useDkFieldFilter = opts.useDkFieldFilter === true;
  const minField = Math.max(8, Math.round(num(opts.minField, 12)) || 12);
  const maxShift = envNum("GOLF_HIST_VENUE_CALIB_MAX_SHIFT", 2.1);
  const minShift = envNum("GOLF_HIST_VENUE_CALIB_MIN_SHIFT", 0.04);

  /** @type {Record<number, number>} */
  const shifts = {};
  let rounds = 0;

  for (let rnd = 1; rnd <= 4; rnd++) {
    let rows = players.filter((pl) => Math.round(num(pl.round, NaN)) === rnd);
    if (useDkFieldFilter && dgFilter?.size >= minField) {
      rows = rows.filter((pl) => dgFilter.has(Math.round(num(pl.dg_id, NaN))));
    }
    if (rows.length < minField) continue;

    const targetStp = historicalVenueStpTargetForRound(venueScoring, rnd);
    if (!Number.isFinite(targetStp)) continue;

    const curStp = meanFinite(rows.map((pl) => num(pl.score_to_par, NaN)));
    if (!Number.isFinite(curStp)) continue;

    const shift = clamp(targetStp - curStp, -maxShift, maxShift);
    if (Math.abs(shift) < minShift) continue;

    shifts[rnd] = Math.round(shift * 1000) / 1000;
    rounds++;
    for (const pl of players) {
      if (Math.round(num(pl.round, NaN)) !== rnd) continue;
      if (useDkFieldFilter && dgFilter?.size >= minField && !dgFilter.has(Math.round(num(pl.dg_id, NaN)))) {
        continue;
      }
      shiftProjectionRowScore(pl, shift, coursePar18);
    }
  }

  const basis =
    payload?.projection_course_basis && typeof payload.projection_course_basis === "object"
      ? payload.projection_course_basis
      : payload?.meta?.projection_course_basis;
  if (basis && typeof basis === "object") {
    const overall = num(venueScoring.venueAvgStp, NaN);
    if (Number.isFinite(overall)) {
      basis.venue_avg_score_to_par = Math.round(overall * 1000) / 1000;
      basis.venue_avg_round_score = Math.round((coursePar18 + overall) * 100) / 100;
      basis.venue_historical_rounds = num(venueScoring.nVenueRounds, basis.venue_historical_rounds);
      basis.venue_scoring_source = venueScoring.source || basis.venue_scoring_source;
    }
    const byRound = {};
    for (let rnd = 1; rnd <= 4; rnd++) {
      const t = historicalVenueStpTargetForRound(venueScoring, rnd);
      if (Number.isFinite(t)) byRound[String(rnd)] = Math.round((coursePar18 + t) * 100) / 100;
    }
    if (Object.keys(byRound).length) basis.historical_venue_avg_score_by_round = byRound;
    if (rounds > 0) basis.historical_venue_score_calibration_shifts = shifts;
    payload.projection_course_basis = basis;
    if (payload.meta && typeof payload.meta === "object") {
      payload.meta.projection_course_basis = basis;
    }
  }

  if (payload?.projection_round_adjustments && typeof payload.projection_round_adjustments === "object") {
    payload.projection_round_adjustments.historical_venue_score_calibrated = rounds > 0;
  } else if (payload?.meta?.projection_round_adjustments) {
    payload.meta.projection_round_adjustments.historical_venue_score_calibrated = rounds > 0;
  }

  return { rounds, shifts, venueAvgStp: venueScoring.venueAvgStp };
}

/** Stream all historical rounds at this venue (any event) — mirrors round_projections.R RAW hist path. */
export async function loadVenueHistoricalScoring(csvPath, courseKeyOpt, courseLabelOpt) {
  const empty = {
    venueAvgStp: NaN,
    venueAvgScore: NaN,
    nVenueRounds: 0,
    source: "none",
    fieldByRound: new Map(),
    playerByRound: new Map(),
    playerByVenue: new Map(),
    courseFitByDg: new Map(),
  };
  const ckWant = courseKeyOpt ? normCourseNameKey(courseKeyOpt) : "";
  if (!ckWant || !csvPath || !existsSync(csvPath)) {
    const adj = lookupAdjScoreToParFromCourseTable(courseLabelOpt || courseKeyOpt);
    if (Number.isFinite(adj)) {
      return { ...empty, venueAvgStp: adj, source: "course_table" };
    }
    return empty;
  }

  const cy = new Date().getFullYear();
  const nFairwayHoles = Math.max(
    10,
    Math.min(16, Math.round(num(process.env.GOLF_VENUE_HIST_N_FAIRWAY_HOLES, 14))),
  );
  let venueTotals = emptyVenueCountRaw();
  /** @type {Map<number, ReturnType<typeof emptyVenueCountRaw>>} */
  const fieldRaw = new Map();
  /** @type {Map<string, ReturnType<typeof emptyVenueCountRaw>>} */
  const playerRaw = new Map();
  /** @type {Map<number, ReturnType<typeof emptyVenueCountRaw>>} */
  const playerAllRaw = new Map();
  /** @type {Map<number, { sumSg: number, n: number }>} */
  const fitRaw = new Map();

  await new Promise((resolve, reject) => {
    const parser = createReadStream(csvPath).pipe(
      parse({
        columns: true,
        relax_quotes: true,
        relax_column_count: true,
        skip_records_with_error: true,
      }),
    );
    parser.on("data", (row) => {
      const ckRow = normCourseNameKey(row.course_name || row.Course_Name || "");
      if (!ckRow || ckRow !== ckWant) return;
      const yr = parseInt(row.year, 10);
      const minYr = venueHistoryMinYear(cy);
      if (Number.isFinite(yr) && (yr < minYr || yr > cy + 1)) return;

      const cp = num(row.course_par, NaN);
      const rs = num(row.round_score, NaN);
      if (!Number.isFinite(cp) || cp < 63 || cp > 76) return;
      if (!Number.isFinite(rs) || rs < 55 || rs > 95) return;

      const rnd = Math.round(num(row.round_num ?? row.round, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return;

      venueTotals = accumulateVenueCountRow(venueTotals, row, nFairwayHoles);

      const fr = fieldRaw.get(rnd) || emptyVenueCountRaw();
      fieldRaw.set(rnd, accumulateVenueCountRow(fr, row, nFairwayHoles));

      const dg = Math.round(num(row.dg_id, NaN));
      if (Number.isFinite(dg)) {
        const pk = `${dg}|${rnd}`;
        const pr = playerRaw.get(pk) || emptyVenueCountRaw();
        playerRaw.set(pk, accumulateVenueCountRow(pr, row, nFairwayHoles));
        const pa = playerAllRaw.get(dg) || emptyVenueCountRaw();
        playerAllRaw.set(dg, accumulateVenueCountRow(pa, row, nFairwayHoles));

        const sg = num(row.sg_total, NaN);
        if (Number.isFinite(sg)) {
          const cf = fitRaw.get(dg) || { sumSg: 0, n: 0 };
          cf.sumSg += sg;
          cf.n++;
          fitRaw.set(dg, cf);
        }
      }
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });

  const fieldByRound = new Map();
  for (const [rnd, raw] of fieldRaw) fieldByRound.set(rnd, finalizeVenueAgg(raw));

  const playerByRound = new Map();
  for (const [pk, raw] of playerRaw) playerByRound.set(pk, finalizeVenueAgg(raw));

  const playerByVenue = new Map();
  for (const [dg, raw] of playerAllRaw) playerByVenue.set(dg, finalizeVenueAgg(raw));

  const courseFitByDg = new Map();
  for (const [dg, raw] of fitRaw) {
    courseFitByDg.set(dg, { avgSg: raw.sumSg / raw.n, n: raw.n });
  }

  const venueAgg = finalizeVenueAgg(venueTotals);
  let venueAvgStp = venueAgg.n >= 40 ? venueAgg.avgStp : NaN;
  let source = venueAgg.n >= 40 ? "historical_csv" : "none";
  if (!Number.isFinite(venueAvgStp)) {
    const adj = lookupAdjScoreToParFromCourseTable(courseLabelOpt || courseKeyOpt);
    if (Number.isFinite(adj)) {
      venueAvgStp = adj;
      source = "course_table";
    }
  }

  return {
    venueAvgStp,
    venueAvgScore: venueAgg.n >= 40 ? venueAgg.avgScore : NaN,
    nVenueRounds: venueAgg.n,
    source,
    venueAvgBirdies: venueAgg.avgBirdies,
    venueAvgPars: venueAgg.avgPars,
    venueAvgBogeys: venueAgg.avgBogeys,
    venueAvgGir: venueAgg.avgGir,
    venueAvgFairways: venueAgg.avgFairways,
    venueAvgPutts: venueAgg.avgPutts,
    fieldByRound,
    playerByRound,
    playerByVenue,
    courseFitByDg,
  };
}

/** Venue-history weight in player/skill blends (books lean skill for elite players). */
const VENUE_PLAYER_BLEND_MIN_ROUNDS = 3;
const VENUE_PLAYER_BLEND_VENUE_MAX = 0.55;
const VENUE_PLAYER_BLEND_VENUE_BASE = 0.2;
const VENUE_PLAYER_BLEND_VENUE_PER_ROUND = 0.03;
const VENUE_PLAYER_BLEND_VENUE_FLOOR = 0.05;
const VENUE_PLAYER_BLEND_SKILL_PULL_BASE = 0.1;
const VENUE_PLAYER_BLEND_SKILL_PULL_MU = 0.26;
const VENUE_PLAYER_BLEND_SKILL_PULL_CAP = 0.52;
const VENUE_FIELD_NO_PLAYER_WEIGHT = 0.32;
const VENUE_COUNT_ALIGN_TO_STP = 0.48;

const VENUE_ROUND_BUCKET_BLEND_MIN = 0.44;
const VENUE_ROUND_BUCKET_BLEND_MAX = 0.74;
const VENUE_ROUND_BUCKET_BLEND_PER_ROUND = 0.03;

function blendVenueAggScalar(allVal, roundVal, wRound) {
  const a = num(allVal, NaN);
  const r = num(roundVal, NaN);
  if (!Number.isFinite(a) && !Number.isFinite(r)) return NaN;
  if (!Number.isFinite(a)) return r;
  if (!Number.isFinite(r)) return a;
  return wRound * r + (1 - wRound) * a;
}

/** Blend full-course venue history with this round's bucket (R1–R4 differ at same venue). */
function mergePlayerVenueAgg(pv, pr, minPlayerRounds) {
  const pvOk = pv && pv.n >= minPlayerRounds;
  const prOk = pr && pr.n >= minPlayerRounds;
  if (prOk && pvOk) {
    const wRound = Math.min(
      VENUE_ROUND_BUCKET_BLEND_MAX,
      VENUE_ROUND_BUCKET_BLEND_MIN + VENUE_ROUND_BUCKET_BLEND_PER_ROUND * (pr.n - minPlayerRounds),
    );
    const blend = (key) => blendVenueAggScalar(pv[key], pr[key], wRound);
    return {
      n: pr.n,
      avgScore: blend("avgScore"),
      avgStp: blend("avgStp"),
      avgBirdies: blend("avgBirdies"),
      avgPars: blend("avgPars"),
      avgBogeys: blend("avgBogeys"),
      avgEagles: blend("avgEagles"),
      avgDoubles: blend("avgDoubles"),
      avgGir: blend("avgGir"),
      avgFairways: blend("avgFairways"),
      avgPutts: blend("avgPutts"),
    };
  }
  if (prOk) return pr;
  if (pvOk) return pv;
  return null;
}

/** More venue rounds → more course history; higher μ_SG → more skill (star props). */
export function venuePlayerHistBlendWeight(nRounds, muForRound) {
  const n = Math.max(0, Math.round(num(nRounds, NaN)));
  if (n < VENUE_PLAYER_BLEND_MIN_ROUNDS) return 0;
  let wVenue = Math.min(
    VENUE_PLAYER_BLEND_VENUE_MAX,
    VENUE_PLAYER_BLEND_VENUE_BASE + VENUE_PLAYER_BLEND_VENUE_PER_ROUND * (n - VENUE_PLAYER_BLEND_MIN_ROUNDS),
  );
  const mu = num(muForRound, 0);
  const skillPull = clamp(
    VENUE_PLAYER_BLEND_SKILL_PULL_BASE + VENUE_PLAYER_BLEND_SKILL_PULL_MU * Math.max(0, mu),
    VENUE_PLAYER_BLEND_SKILL_PULL_BASE,
    VENUE_PLAYER_BLEND_SKILL_PULL_CAP,
  );
  wVenue = Math.max(VENUE_PLAYER_BLEND_VENUE_FLOOR, wVenue - skillPull);
  return wVenue;
}

/** When skill beats venue history, lean harder on skill — especially for above-average μ_SG. */
function reduceVenueWeightWhenSkillBetter(wVenue, muForRound, skillVal, histVal, higherIsBetter) {
  if (wVenue <= 0 || !Number.isFinite(skillVal) || !Number.isFinite(histVal)) return wVenue;
  const mu = num(muForRound, 0);
  if (mu < 0.12) return wVenue;
  const margin = higherIsBetter ? skillVal - histVal : histVal - skillVal;
  if (margin < 0.05) return wVenue;
  const pull = clamp(0.28 + 0.3 * Math.max(0, mu) + 0.18 * margin, 0.22, 0.92);
  return wVenue * (1 - pull);
}

function blendVenueSkillScalar(skillVal, playerVal, fieldVal, wVenue) {
  const sk = num(skillVal, NaN);
  if (!Number.isFinite(sk)) return NaN;
  if (Number.isFinite(playerVal)) return wVenue * playerVal + (1 - wVenue) * sk;
  if (Number.isFinite(fieldVal)) {
    return VENUE_FIELD_NO_PLAYER_WEIGHT * fieldVal + (1 - VENUE_FIELD_NO_PLAYER_WEIGHT) * sk;
  }
  return sk;
}

function skillScoreToPar({
  muForRound,
  course_par_18,
  venueScoring,
  round,
  fieldMeanMu,
  minFieldRounds,
}) {
  const cp = num(course_par_18, NaN);
  const mu = num(muForRound, 0);
  if (!Number.isFinite(cp)) return { stp: -mu, source: "skill_rating" };

  const rnd = Math.round(num(round, NaN));
  const fr = venueScoring?.fieldByRound?.get(rnd);
  let venueStp = num(venueScoring?.venueAvgStp, NaN);
  let source = "skill_around_venue_mean";
  const shrunk = historicalVenueStpTargetForRound(venueScoring, rnd);
  if (Number.isFinite(shrunk)) {
    venueStp = shrunk;
    source =
      fr && fr.n >= minFieldRounds && Number.isFinite(fr.avgStp)
        ? "skill_around_shrunk_round_venue_mean"
        : "skill_around_venue_mean";
  }
  const fm = num(fieldMeanMu, 0);
  if (Number.isFinite(venueStp)) return { stp: venueStp - (mu - fm), source };
  return { stp: -mu, source: "skill_rating" };
}

/**
 * Hole-count markets: blend skill projection with round-specific + full-course venue history,
 * then field venue averages; lightly align bird/bog to blended score-to-par.
 */
export function resolveProjectionCounts({
  dg_id,
  round,
  skillCounts,
  venueScoring,
  targetStp,
  muForRound,
  nFairwayHoles = 14,
  minPlayerRounds = 3,
  minFieldRounds = 25,
}) {
  const sk = skillCounts || {};
  const rnd = Math.round(num(round, NaN));
  const dg = Math.round(num(dg_id, NaN));
  const pk = `${dg}|${rnd}`;
  const pr = venueScoring?.playerByRound?.get(pk);
  const pv = venueScoring?.playerByVenue?.get(dg);
  const fr = venueScoring?.fieldByRound?.get(rnd);
  const frOk = fr && fr.n >= minFieldRounds;
  const prOk = pr && pr.n >= minPlayerRounds;
  const playerAgg = mergePlayerVenueAgg(pv, pr, minPlayerRounds);
  const histN = prOk ? pr.n : playerAgg?.n ?? 0;
  let wVenue = venuePlayerHistBlendWeight(histN, muForRound);
  const wEagles = reduceVenueWeightWhenSkillBetter(
    wVenue,
    muForRound,
    sk.eagles,
    playerAgg?.avgEagles,
    true,
  );
  const wBirdies = reduceVenueWeightWhenSkillBetter(
    wVenue,
    muForRound,
    sk.birdies,
    playerAgg?.avgBirdies,
    true,
  );
  const wBogeys = reduceVenueWeightWhenSkillBetter(
    wVenue,
    muForRound,
    sk.bogeys,
    playerAgg?.avgBogeys,
    false,
  );
  const wDoubles = reduceVenueWeightWhenSkillBetter(
    wVenue,
    muForRound,
    sk.doubles,
    playerAgg?.avgDoubles,
    false,
  );
  const wPars = wVenue;
  const wGir = reduceVenueWeightWhenSkillBetter(wVenue, muForRound, sk.gir, playerAgg?.avgGir, true);
  const wFairways = reduceVenueWeightWhenSkillBetter(
    wVenue,
    muForRound,
    sk.fairways,
    playerAgg?.avgFairways,
    true,
  );
  const wPutts = reduceVenueWeightWhenSkillBetter(
    wVenue,
    muForRound,
    sk.putts,
    playerAgg?.avgPutts,
    false,
  );

  let eagles = blendVenueSkillScalar(sk.eagles, playerAgg?.avgEagles, frOk ? fr.avgEagles : NaN, wEagles);
  let birdies = blendVenueSkillScalar(sk.birdies, playerAgg?.avgBirdies, frOk ? fr.avgBirdies : NaN, wBirdies);
  let bogeys = blendVenueSkillScalar(sk.bogeys, playerAgg?.avgBogeys, frOk ? fr.avgBogeys : NaN, wBogeys);
  let doubles = blendVenueSkillScalar(sk.doubles, playerAgg?.avgDoubles, frOk ? fr.avgDoubles : NaN, wDoubles);
  let pars = blendVenueSkillScalar(sk.pars, playerAgg?.avgPars, frOk ? fr.avgPars : NaN, wPars);
  let gir = blendVenueSkillScalar(sk.gir, playerAgg?.avgGir, frOk ? fr.avgGir : NaN, wGir);
  let fairways = blendVenueSkillScalar(sk.fairways, playerAgg?.avgFairways, frOk ? fr.avgFairways : NaN, wFairways);
  let putts = blendVenueSkillScalar(sk.putts, playerAgg?.avgPutts, frOk ? fr.avgPutts : NaN, wPutts);

  eagles = Math.max(0, num(eagles, 0));
  birdies = Math.max(0.15, num(birdies, 0));
  bogeys = Math.max(0.15, num(bogeys, 0));
  doubles = Math.max(0.04, num(doubles, 0));
  if (!Number.isFinite(pars)) pars = Math.max(0.12, 18 - eagles - birdies - bogeys - doubles);

  const stp = num(targetStp, NaN);
  if (Number.isFinite(stp)) {
    const aligned = softAlignHoleCountsToStp(
      { eagles, birdies, pars, bogeys, doubles },
      stp,
      VENUE_COUNT_ALIGN_TO_STP,
    );
    eagles = aligned.eagles;
    birdies = aligned.birdies;
    pars = aligned.pars;
    bogeys = aligned.bogeys;
    doubles = aligned.doubles;
  }

  if (Number.isFinite(gir)) gir = Math.max(6, Math.min(16, gir));
  if (Number.isFinite(fairways)) fairways = Math.max(2, Math.min(nFairwayHoles + 0.5, fairways));
  if (Number.isFinite(putts)) putts = Math.max(22, Math.min(36, putts));

  return { eagles, birdies, pars, bogeys, doubles, gir, fairways, putts };
}

/** Only bird/bog carry this-week form; pars are always residual to 18 holes + score-to-par. */
const WITHIN_EVENT_FORM_BLEND_KEYS = ["birdies", "bogeys"];
const WITHIN_EVENT_GIR_FW_BLEND_KEYS = ["gir", "fairways", "putts"];
/** Prior-round form targets: mostly field average, small player-specific residual. */
const WITHIN_EVENT_PRIOR_FIELD_SHARE = 0.85;
/** μ_SG within-event carry: blend field surplus vs player surplus (not pin / not individual-only). */
const WITHIN_EVENT_FORM_FIELD_SHARE = 0.85;
const WITHIN_EVENT_COUNT_BLEND_BASE = 0.12;
const WITHIN_EVENT_COUNT_BLEND_PER_ROUND = 0.04;
const WITHIN_EVENT_COUNT_BLEND_CAP = 0.28;
const WITHIN_EVENT_BOGEY_BLEND_SCALE = 1;
const WITHIN_EVENT_SKILL_ANCHOR_BASE = 0.22;
const WITHIN_EVENT_SKILL_ANCHOR_MU_SCALE = 0.06;
const WITHIN_EVENT_ALIGN_STRENGTH = 0.96;
const WITHIN_EVENT_PAR_SPREAD_STRENGTH = 0.55;
const FIELD_DAY_COUNTING_LIFT_FRAC = 0.48;

/** Clamp implausible field-day means (CSV quirks, birds-or-better columns, etc.). */
export function sanitizeFieldCountingMeans(fieldMeans) {
  if (!fieldMeans || typeof fieldMeans !== "object") return null;
  /** @type {Record<string, Record<number, number>>} */
  const out = { birdies: {}, bogeys: {}, gir: {}, fairways: {} };
  for (const stat of Object.keys(out)) {
    const bucket = fieldMeans[stat];
    if (!bucket || typeof bucket !== "object") continue;
    for (const [rndKey, raw] of Object.entries(bucket)) {
      const rnd = Math.round(num(rndKey, NaN));
      let v = num(raw, NaN);
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4 || !Number.isFinite(v)) continue;
      if (stat === "birdies") v = clamp(v, 2.2, 4.85);
      else if (stat === "bogeys") v = clamp(v, 1.35, 5.2);
      else if (stat === "gir") v = clamp(v, 8, 15.5);
      else if (stat === "fairways") v = clamp(v, 4, 13);
      out[stat][rnd] = Math.round(v * 100) / 100;
    }
  }
  return out;
}

/** Prefer live within-event field means over event-scoped CSV when both exist. */
export function mergeFieldCountingMeansPreferWithin(withinMeans, eventMeans) {
  const within = sanitizeFieldCountingMeans(withinMeans);
  const event = sanitizeFieldCountingMeans(eventMeans);
  if (!within && !event) return null;
  /** @type {Record<string, Record<number, number>>} */
  const out = { birdies: {}, bogeys: {}, gir: {}, fairways: {} };
  for (const stat of Object.keys(out)) {
    for (let rnd = 1; rnd <= 3; rnd++) {
      const w = num(within?.[stat]?.[rnd], NaN);
      const e = num(event?.[stat]?.[rnd], NaN);
      const pick = Number.isFinite(w) ? w : e;
      if (Number.isFinite(pick)) out[stat][rnd] = pick;
    }
  }
  return out;
}

function pooledFieldCountingMean(fieldMeans, statKey) {
  const bucket = fieldMeans?.[statKey];
  if (!bucket || typeof bucket !== "object") return NaN;
  const vals = Object.values(bucket).map((v) => num(v, NaN)).filter(Number.isFinite);
  if (!vals.length) return NaN;
  return vals.reduce((a, b) => a + b, 0) / vals.length;
}

function plausibleEventWeekMean(pooled, venue, statKey) {
  if (!Number.isFinite(pooled)) return NaN;
  if (!Number.isFinite(venue)) return pooled;
  const d = pooled - venue;
  if (statKey === "birdies") {
    if (d > 0.65 || d < -0.55) return Math.round((venue + (d > 0 ? 0.21 : -0.12)) * 100) / 100;
  } else if (statKey === "bogeys") {
    if (Math.abs(d) > 0.55) return Math.round((venue + Math.sign(d) * Math.min(0.18, Math.abs(d) * 0.35)) * 100) / 100;
  } else if (statKey === "gir" || statKey === "fairways") {
    if (Math.abs(d) > 1.1) return Math.round((venue + d * 0.35) * 100) / 100;
  }
  return Math.round(pooled * 100) / 100;
}

function guardedPooledFieldMean(fieldMeans, statKey, basis, basisKey, fallback) {
  const pooled = pooledFieldCountingMean(fieldMeans, statKey);
  const venue = num(basis?.[basisKey], fallback);
  if (!Number.isFinite(pooled)) return venue;
  const pl = plausibleEventWeekMean(pooled, venue, statKey);
  return Number.isFinite(pl) ? pl : venue;
}

function fieldCountingTargetForRound(fieldMeans, statKey, round, pooledFallback) {
  const bucket = fieldMeans?.[statKey];
  if (bucket && typeof bucket === "object") {
    const prior = [];
    const tr = Math.round(num(round, NaN));
    if (Number.isFinite(tr) && tr >= 2) {
      for (let rn = 1; rn < tr; rn++) {
        const v = num(bucket[rn] ?? bucket[String(rn)], NaN);
        if (Number.isFinite(v)) prior.push(v);
      }
    }
    if (prior.length) {
      const raw = prior.reduce((a, b) => a + b, 0) / prior.length;
      const adj = plausibleEventWeekMean(raw, pooledFallback, statKey);
      return Number.isFinite(adj) ? adj : raw;
    }
  }
  return num(pooledFallback, NaN);
}

function meanFinite(vals) {
  const v = vals.filter(Number.isFinite);
  if (!v.length) return NaN;
  return v.reduce((a, b) => a + b, 0) / v.length;
}

/**
 * Guarantee venue averages needed for O/U course ratings (Round score, GIR, FW, …).
 * Never strips existing keys — only backfills missing venue_avg_round_score / score_to_par / FW.
 */
export function ensureProjectionCourseBasisComplete(basis, payload = {}) {
  const out = basis && typeof basis === "object" ? basis : {};
  const coursePar18 = Math.round(num(payload.course_par_18, NaN)) || 72;
  const lo = coursePar18 - 14;
  const hi = coursePar18 + 22;
  out.fairway_holes_modeled = Math.round(num(out.fairway_holes_modeled, 14)) || 14;

  const roundMaps = [out.field_avg_score_by_round, out.event_week_field_avg_score_by_round];
  const roundScores = [];
  for (const m of roundMaps) {
    if (!m || typeof m !== "object") continue;
    for (const v of Object.values(m)) {
      const x = num(v, NaN);
      if (Number.isFinite(x) && x >= lo && x <= hi) roundScores.push(x);
    }
  }

  if (!Number.isFinite(num(out.venue_avg_round_score, NaN))) {
    const stp = num(out.venue_avg_score_to_par, NaN);
    if (Number.isFinite(stp)) out.venue_avg_round_score = Math.round((coursePar18 + stp) * 100) / 100;
  }
  const lockHistoricalVenue =
    num(out.venue_historical_rounds, 0) >= 40 &&
    String(out.venue_scoring_source || "") === "historical_csv" &&
    Number.isFinite(num(out.venue_avg_score_to_par, NaN));
  if (!Number.isFinite(num(out.venue_avg_round_score, NaN)) && roundScores.length && !lockHistoricalVenue) {
    out.venue_avg_round_score =
      Math.round((roundScores.reduce((a, b) => a + b, 0) / roundScores.length) * 100) / 100;
  }
  if (!lockHistoricalVenue && !Number.isFinite(num(out.venue_avg_round_score, NaN)) && Array.isArray(payload.players)) {
    const fromRows = [];
    for (const pl of payload.players) {
      const ts = num(pl.total_score, NaN);
      if (Number.isFinite(ts) && ts >= lo && ts <= hi) fromRows.push(ts);
    }
    if (fromRows.length >= 8) {
      out.venue_avg_round_score =
        Math.round((fromRows.reduce((a, b) => a + b, 0) / fromRows.length) * 100) / 100;
    }
  }
  if (
    !Number.isFinite(num(out.venue_avg_round_score, NaN)) &&
    Number.isFinite(num(out.venue_avg_birdies, NaN)) &&
    Number.isFinite(num(out.venue_avg_bogeys, NaN))
  ) {
    const stpEst = num(out.venue_avg_bogeys, 0) - num(out.venue_avg_birdies, 0);
    out.venue_avg_round_score = Math.round((coursePar18 + stpEst) * 100) / 100;
  }

  if (
    !lockHistoricalVenue &&
    !Number.isFinite(num(out.venue_avg_score_to_par, NaN)) &&
    Number.isFinite(num(out.venue_avg_round_score, NaN))
  ) {
    out.venue_avg_score_to_par = Math.round((out.venue_avg_round_score - coursePar18) * 1000) / 1000;
  }

  if (!Number.isFinite(num(out.venue_avg_fairways, NaN)) && Number.isFinite(num(out.venue_avg_gir, NaN))) {
    const nFw = out.fairway_holes_modeled;
    out.venue_avg_fairways = Math.round(num(out.venue_avg_gir, 0) * (nFw / 18) * 0.92 * 100) / 100;
  }

  if (
    !Number.isFinite(num(out.venue_avg_pars, NaN)) &&
    Number.isFinite(num(out.venue_avg_birdies, NaN)) &&
    Number.isFinite(num(out.venue_avg_bogeys, NaN))
  ) {
    const p = 18 - num(out.venue_avg_birdies, 0) - num(out.venue_avg_bogeys, 0);
    if (p > 8 && p < 14) out.venue_avg_pars = Math.round(p * 100) / 100;
  }

  return out;
}

/** Blend historical venue anchors toward this-week field counting (books/DK field pace). */
export function updateProjectionBasisFromEventWeek(basis, fieldMeans, opts = {}) {
  if (!basis || typeof basis !== "object") return basis;
  const eventShare = clamp(num(opts.eventShare, NaN), 0.35, 0.85) || 0.65;
  const histShare = 1 - eventShare;
  const blendKey = (histKey, eventVal, fallback) => {
    const hist = num(basis[histKey], NaN);
    let ev = num(eventVal, NaN);
    const stat =
      histKey === "venue_avg_birdies"
        ? "birdies"
        : histKey === "venue_avg_bogeys"
          ? "bogeys"
          : histKey === "venue_avg_gir"
            ? "gir"
            : histKey === "venue_avg_fairways"
              ? "fairways"
              : "";
    if (stat && Number.isFinite(ev)) ev = plausibleEventWeekMean(ev, hist, stat);
    if (!Number.isFinite(ev)) return;
    const next = Number.isFinite(hist) ? histShare * hist + eventShare * ev : ev;
    basis[histKey] = Math.round(next * 100) / 100;
  };
  const pooledBird = plausibleEventWeekMean(
    pooledFieldCountingMean(fieldMeans, "birdies"),
    num(basis.venue_avg_birdies, NaN),
    "birdies",
  );
  const pooledBog = plausibleEventWeekMean(
    pooledFieldCountingMean(fieldMeans, "bogeys"),
    num(basis.venue_avg_bogeys, NaN),
    "bogeys",
  );
  const pooledGir = plausibleEventWeekMean(
    pooledFieldCountingMean(fieldMeans, "gir"),
    num(basis.venue_avg_gir, NaN),
    "gir",
  );
  const pooledFw = plausibleEventWeekMean(
    pooledFieldCountingMean(fieldMeans, "fairways"),
    num(basis.venue_avg_fairways, NaN),
    "fairways",
  );
  blendKey("venue_avg_birdies", pooledBird, 4.2);
  blendKey("venue_avg_bogeys", pooledBog, 2.5);
  blendKey("venue_avg_gir", pooledGir, 12);
  blendKey("venue_avg_fairways", pooledFw, 9);
  if (Number.isFinite(basis.venue_avg_birdies) && Number.isFinite(basis.venue_avg_bogeys)) {
    const p = 18 - num(basis.venue_avg_birdies, 0) - num(basis.venue_avg_bogeys, 0);
    if (p > 8 && p < 14) basis.venue_avg_pars = Math.round(p * 100) / 100;
  }
  if (fieldMeans) basis.field_counting_means_by_round = fieldMeans;
  return ensureProjectionCourseBasisComplete(basis, opts?.payload || {});
}

/**
 * Shift projection field means toward this-week targets (DK field when ≥ minField).
 * Bird/bog/pars shifts are score-to-par neutral; GIR/FW/putts are additive on the field.
 */
export function calibrateProjectionFieldMarkets(payload, opts = {}) {
  if (opts.skipCalibrate) return { rounds: 0, shifts: {} };
  const players = Array.isArray(payload?.players) ? payload.players : [];
  if (!players.length) return { rounds: 0, shifts: {} };

  const basisRoot =
    payload?.projection_course_basis && typeof payload.projection_course_basis === "object"
      ? payload.projection_course_basis
      : payload?.meta?.projection_course_basis;
  if (!basisRoot || typeof basisRoot !== "object") return { rounds: 0, shifts: {} };

  const dgFilter = opts.dgFilter instanceof Set ? opts.dgFilter : null;
  const minField = Math.max(8, Math.round(num(opts.minField, 12)) || 12);
  const fieldMeans = sanitizeFieldCountingMeans(basisRoot.field_counting_means_by_round);
  const basisForTargets = { ...basisRoot };
  const pooled = {
    birdies: guardedPooledFieldMean(fieldMeans, "birdies", basisForTargets, "venue_avg_birdies", 4.2),
    bogeys: guardedPooledFieldMean(fieldMeans, "bogeys", basisForTargets, "venue_avg_bogeys", 2.5),
    gir: guardedPooledFieldMean(fieldMeans, "gir", basisForTargets, "venue_avg_gir", 12),
    fairways: guardedPooledFieldMean(fieldMeans, "fairways", basisForTargets, "venue_avg_fairways", 9),
    putts: num(basisRoot.venue_avg_putts, NaN),
  };
  updateProjectionBasisFromEventWeek(basisRoot, fieldMeans, { ...opts, payload });

  /** @type {Record<string, Record<number, number>>} */
  const shifts = { birdies: {}, bogeys: {}, gir: {}, fairways: {}, putts: {} };
  let rounds = 0;

  for (let rnd = 1; rnd <= 4; rnd++) {
    let rows = players.filter((pl) => Math.round(num(pl.round, NaN)) === rnd);
    if (dgFilter?.size >= minField) {
      rows = rows.filter((pl) => dgFilter.has(Math.round(num(pl.dg_id, NaN))));
    }
    if (rows.length < minField) continue;
    rounds++;

    const applyScoreNeutral = (stat, target) => {
      if (!Number.isFinite(target)) return;
      const cur = meanFinite(rows.map((pl) => num(pl[stat], NaN)));
      if (!Number.isFinite(cur)) return;
      const delta = Math.round((target - cur) * 1000) / 1000;
      if (Math.abs(delta) < 0.035) return;
      shifts[stat][rnd] = delta;
      for (const pl of rows) {
        const v = num(pl[stat], NaN);
        if (!Number.isFinite(v)) continue;
        pl[stat] = Math.round(Math.max(0.15, v + delta) * 100) / 100;
        if (stat === "birdies" || stat === "bogeys") {
          const pars = num(pl.pars, NaN);
          if (Number.isFinite(pars)) pl.pars = Math.round(Math.max(0.12, pars - delta) * 100) / 100;
        }
      }
    };

    const applyAdditive = (stat, target, lo, hi) => {
      if (!Number.isFinite(target)) return;
      const cur = meanFinite(rows.map((pl) => num(pl[stat], NaN)));
      if (!Number.isFinite(cur)) return;
      const delta = Math.round((target - cur) * 1000) / 1000;
      if (Math.abs(delta) < 0.035) return;
      shifts[stat][rnd] = delta;
      for (const pl of rows) {
        const v = num(pl[stat], NaN);
        if (!Number.isFinite(v)) continue;
        pl[stat] = Math.round(clamp(v + delta, lo, hi) * 100) / 100;
      }
    };

    applyScoreNeutral("birdies", pooled.birdies);
    applyScoreNeutral("bogeys", pooled.bogeys);
    applyAdditive("gir", pooled.gir, 6, 16.2);
    applyAdditive(
      "fairways",
      pooled.fairways,
      2,
      num(basisRoot.fairway_holes_modeled, 14) + 0.5,
    );
    if (Number.isFinite(pooled.putts) && pooled.putts >= 24) {
      applyAdditive("putts", pooled.putts, 24, 34);
    }
  }

  if (payload.meta && typeof payload.meta === "object") {
    payload.meta.projection_course_basis = basisRoot;
    if (!payload.meta.projection_round_adjustments) payload.meta.projection_round_adjustments = {};
    payload.meta.projection_round_adjustments.field_markets_calibrated = true;
    payload.meta.projection_round_adjustments.field_calibration_shifts = shifts;
  }
  payload.projection_course_basis = basisRoot;
  return { rounds, shifts, pooled };
}

/** Equal-weight mean of prior-round values (no recency tilt toward the latest round). */
export function plainMeanFromArr(arr) {
  if (!Array.isArray(arr) || !arr.length) return NaN;
  const vals = arr.map((v) => num(v, NaN)).filter(Number.isFinite);
  if (!vals.length) return NaN;
  return vals.reduce((a, b) => a + b, 0) / vals.length;
}

/**
 * Target for blending: field-average counting this week + player's prior-round mean.
 * @param {string} statKey — birdies | bogeys | gir | fairways | putts
 * @param {number[]} priorArr — player's prior rounds this event
 * @param {Record<string, Record<number, number>> | null} fieldMeans — field_counting_means_by_round
 */
export function priorRoundCountingTarget(statKey, priorArr, fieldMeans, targetRound, fieldShare = WITHIN_EVENT_PRIOR_FIELD_SHARE) {
  const playerMean = plainMeanFromArr(priorArr);
  const tr = Math.round(num(targetRound, NaN));
  if (!fieldMeans || !Number.isFinite(tr) || tr < 2) return playerMean;
  const rn = tr - 1;
  const bucket = fieldMeans[statKey];
  const fieldAvg = num(bucket?.[rn] ?? bucket?.[String(rn)], NaN);
  if (!Number.isFinite(fieldAvg)) return playerMean;
  if (!Number.isFinite(playerMean)) return fieldAvg;
  const fs = clamp(num(fieldShare, NaN), 0, 1);
  const share = Number.isFinite(fs) ? fs : WITHIN_EVENT_PRIOR_FIELD_SHARE;
  return share * fieldAvg + (1 - share) * playerMean;
}

/** @deprecated Use plainMeanFromArr / priorRoundCountingTarget — kept for callers that still import it. */
export function recencyWeightedMeanFromArr(arr, decay = 1) {
  if (!Array.isArray(arr) || !arr.length) return NaN;
  let sum = 0;
  let wsum = 0;
  for (let i = 0; i < arr.length; i++) {
    const v = num(arr[i], NaN);
    if (!Number.isFinite(v)) continue;
    const age = arr.length - 1 - i;
    const w = decay ** age;
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

/** Stars / contenders: books move their next-round props faster (less shrink to season prior). */
export function withinEventCountingStarTrustBoost(playerRow) {
  if (!playerRow || typeof playerRow !== "object") return 0;
  const win = num(playerRow.win, NaN);
  const t10 = num(playerRow.top_10, NaN);
  const t20 = num(playerRow.top_20, NaN);
  const mu = num(playerRow.mu_sg, NaN);
  let boost = 0;
  if (Number.isFinite(win) && win >= 0.045) boost = Math.max(boost, 0.05);
  else if (Number.isFinite(win) && win >= 0.018) boost = Math.max(boost, 0.04);
  else if (Number.isFinite(t10) && t10 >= 0.18) boost = Math.max(boost, 0.03);
  else if (Number.isFinite(t20) && t20 >= 0.32) boost = Math.max(boost, 0.02);
  if (Number.isFinite(mu) && mu >= 0.85) boost = Math.max(boost, 0.04);
  else if (Number.isFinite(mu) && mu >= 0.4) boost = Math.max(boost, 0.02);
  return Math.min(0.06, boost);
}

export function withinEventCountingBlendWeight(nPriorRounds, playerRow) {
  const n = Math.max(0, Math.round(num(nPriorRounds, NaN)));
  if (!n) return 0;
  let w = Math.min(WITHIN_EVENT_COUNT_BLEND_CAP, WITHIN_EVENT_COUNT_BLEND_BASE + WITHIN_EVENT_COUNT_BLEND_PER_ROUND * n);
  w += withinEventCountingStarTrustBoost(playerRow);
  return Math.min(0.92, w);
}

/** Field-wide lift when this week's completed round scored birdie-/bogey-heavy vs venue history. */
export function fieldDayCountingLift(fieldAvg, venueAvg, frac = FIELD_DAY_COUNTING_LIFT_FRAC) {
  const f = num(fieldAvg, NaN);
  const v = num(venueAvg, NaN);
  if (!Number.isFinite(f) || !Number.isFinite(v)) return 0;
  return frac * (f - v);
}

/** Pars as residual hole count — no score-to-par forcing. */
export function residualParsFromHoleCounts(counts) {
  const e = Math.max(0, num(counts?.eagles, 0));
  const d = Math.max(0, num(counts?.doubles, 0));
  const b = num(counts?.birdies, NaN);
  const bg = num(counts?.bogeys, NaN);
  if (!Number.isFinite(b) || !Number.isFinite(bg)) return num(counts?.pars, NaN);
  return Math.max(0.12, 18 - e - d - b - bg);
}

/** Trust skill/venue base over one-round actuals when tier and deviation disagree. */
export function withinEventSkillTrustFactor(playerRow) {
  const mu = num(playerRow?.mu_sg, 0);
  return clamp(0.32 + 0.14 * Math.max(0, mu), 0.32, 0.78);
}

function adjustWithinEventBlendWeight(w, priorAvg, skillBase, playerRow, statKey) {
  if (!Number.isFinite(priorAvg) || !Number.isFinite(skillBase)) return w;
  const dev = priorAvg - skillBase;
  const trust = withinEventSkillTrustFactor(playerRow);
  const mu = num(playerRow?.mu_sg, 0);
  if (mu >= 0.35 && dev < -0.75) {
    w *= clamp(1 - trust * Math.min(1, (-dev - 0.5) / 2.5), 0.18, 1);
  }
  if (mu < 0.25 && dev > 0.75) {
    w *= clamp(1 - (1 - trust) * Math.min(1, (dev - 0.5) / 2.5), 0.18, 1);
  }
  if (statKey === "bogeys" && mu >= 0.5 && dev < -0.5) {
    w *= clamp(1 - trust * 0.45, 0.35, 1);
  }
  return w;
}

/** Live rows with pars≈18 but score≠par, or hole mix far from card, are not usable as-is. */
export function liveCountingUntrustworthy(counts, coursePar18) {
  const cp = num(coursePar18, NaN);
  const rs = num(counts.round_score ?? counts.score, NaN);
  if (!Number.isFinite(cp) || !Number.isFinite(rs)) return false;
  const stp = rs - cp;
  const bird = num(counts.birdies, NaN);
  const par = num(counts.pars, NaN);
  const bog = num(counts.bogeys ?? counts.bogies, NaN);
  const e = num(counts.eagles ?? counts.eagles_or_better, 0);
  const d = num(counts.doubles ?? counts.doubles_or_worse, 0);
  if (Number.isFinite(par) && par >= 17.5 && Math.abs(stp) > 0.5) return true;
  if (Number.isFinite(bird) && bird <= 0.01 && stp < -0.5) return true;
  const hat = -(bird || 0) - 2 * e + (bog || 0) + 2 * d;
  return Math.abs(hat - stp) > 1.25;
}

/** Score-only bird/bog split when live hole counts are missing or inconsistent. */
export function inferHoleCountsFromScoreSplit(stp, venueBirdies = 2.88, venueBogeys = 2.93) {
  const vBird = num(venueBirdies, 2.88);
  const vBog = num(venueBogeys, 2.93);
  let bird = 0;
  let bog = 0;
  if (stp <= 0) {
    bird = Math.max(0, Math.min(7, vBird + -stp * 0.85));
    bog = Math.max(0, stp + bird);
  } else {
    bog = Math.max(0, Math.min(8, vBog + stp * 0.72));
    bird = Math.max(0, Math.min(6, vBird * 0.35 + Math.max(0, stp - bog) * 0.4));
  }
  bird = Math.round(bird * 100) / 100;
  bog = Math.round(bog * 100) / 100;
  return {
    birdies: bird,
    bogeys: bog,
    eagles: 0,
    doubles: 0,
    pars: Math.max(0.12, Math.round((18 - bird - bog) * 100) / 100),
  };
}

/**
 * Live DG stats often report bogeys=0 when missing. Infer from score + birdies when inconsistent.
 * stp = −bird − 2·eagle + bog + 2·double  →  bog = stp + bird + 2·eagle − 2·double
 */
export function reconcileHoleCountsFromScore(counts, coursePar18, venueBirdies, venueBogeys) {
  if (!counts || typeof counts !== "object") return counts;
  const cp = num(coursePar18, NaN);
  const rs = num(counts.round_score ?? counts.score, NaN);
  if (!Number.isFinite(cp) || !Number.isFinite(rs)) return counts;
  const stp = rs - cp;
  const vBird = num(venueBirdies, 2.88);
  const vBog = num(venueBogeys, 2.93);
  if (liveCountingUntrustworthy(counts, cp)) {
    const inf = inferHoleCountsFromScoreSplit(stp, vBird, vBog);
    return { ...counts, ...inf, round_score: rs };
  }
  const e = Math.max(0, num(counts.eagles ?? counts.eagles_or_better, 0));
  const d = Math.max(0, num(counts.doubles ?? counts.doubles_or_worse, 0));
  let bird = num(counts.birdies, NaN);
  if (!Number.isFinite(bird)) return counts;

  let bog = num(counts.bogeys ?? counts.bogies, NaN);
  const impliedBog = stp + bird + 2 * e - 2 * d;
  const hatStp = -bird - 2 * e + (Number.isFinite(bog) ? bog : 0) + 2 * d;
  const err = Math.abs(hatStp - stp);
  if (!Number.isFinite(bog) || (bog <= 0.01 && impliedBog >= 0.45) || err > 0.85) {
    bog = Math.max(0, Math.round(impliedBog * 100) / 100);
  }
  const pars = Math.max(0, Math.round((18 - bird - bog - e - d) * 100) / 100);
  return { ...counts, birdies: bird, bogeys: bog, pars, eagles: e, doubles: d };
}

/**
 * This-week field birdie/bogey/GIR means from event-scoped CSV context (not all-time venue R1/R2).
 * @param {ReturnType<typeof loadEventRoundContextFromHistoricalCsv>} ctx
 */
export function fieldCountingMeansFromEventContext(ctx, minPlayers = 28) {
  /** @type {Record<string, Record<number, number>>} */
  const out = { birdies: {}, bogeys: {}, gir: {} };
  if (!ctx?.byRound) return out;
  for (let rnd = 1; rnd <= 3; rnd++) {
    const b = ctx.byRound.get(rnd);
    if (!b) continue;
    if (b.nBird >= minPlayers) out.birdies[rnd] = Math.round((b.sumBird / b.nBird) * 100) / 100;
    if (b.nBog >= minPlayers) out.bogeys[rnd] = Math.round((b.sumBog / b.nBog) * 100) / 100;
    if (b.nGir >= minPlayers) out.gir[rnd] = Math.round((b.sumGir / b.nGir) * 100) / 100;
  }
  return out;
}

/**
 * Per-player prior-round counting actuals this week from player_round_history.json.
 * @returns {Map<number, Map<number, { birdies?: number, bogeys?: number, gir?: number, round_score?: number }>>}
 */
export function loadWithinEventCountingActualsFromHistoryJson(
  jsonPath,
  eventName,
  courseKeyOpt,
  yearOpt,
  coursePar18,
  venueBirdies,
  venueBogeys,
) {
  /** @type {Map<number, Map<number, object>>} */
  const out = new Map();
  if (!jsonPath || !existsSync(jsonPath) || !eventName) return out;
  const cy = Number.isFinite(num(yearOpt, NaN)) ? Math.round(num(yearOpt, NaN)) : new Date().getFullYear();
  const ckWant = courseKeyOpt ? normCourseNameKey(courseKeyOpt) : "";
  let j;
  try {
    j = JSON.parse(readFileSync(jsonPath, "utf8"));
  } catch {
    return out;
  }
  const byDg = j?.byDgId;
  if (!byDg || typeof byDg !== "object") return out;

  function rowPriority(row) {
    let p = 0;
    if (row._from_live_tournament_stats) p += 1_000_000;
    if (row._from_live_in_play) p += 500_000;
    return p + Math.round(num(row.sortKey, 0));
  }

  for (const bucket of Object.values(byDg)) {
    if (!bucket || !Array.isArray(bucket.rounds)) continue;
    const dg = Math.round(num(bucket.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    for (const r of bucket.rounds) {
      if (!r || typeof r !== "object") continue;
      if (!eventsLikelySame(eventName, String(r.event_name || "").trim())) continue;
      const yr = Math.round(num(r.year, NaN));
      if (Number.isFinite(yr) && yr !== cy) continue;
      if (ckWant) {
        const ckRow = normCourseNameKey(r.course_name || r.course || "");
        if (ckRow && ckRow !== ckWant) continue;
      }
      const rnd = Math.round(num(r.round_num ?? r.round, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      const cpRow = num(r.course_par, NaN);
      const cp = Number.isFinite(cpRow) ? cpRow : num(coursePar18, NaN);
      let rec = {
        birdies: num(r.birdies, NaN),
        bogeys: num(r.bogeys ?? r.bogies, NaN),
        gir: girOrFwToCount(r.gir, 18),
        round_score: num(r.round_score ?? r.score, NaN),
        eagles: num(r.eagles_or_better, NaN),
        doubles: num(r.doubles_or_worse, NaN),
      };
      rec = reconcileHoleCountsFromScore(
        rec,
        cp,
        num(venueBirdies, 2.88),
        num(venueBogeys, 2.93),
      );
      if (!Number.isFinite(rec.birdies) && !Number.isFinite(rec.bogeys) && !Number.isFinite(rec.gir)) continue;
      let per = out.get(dg);
      if (!per) {
        per = new Map();
        out.set(dg, per);
      }
      const pri = rowPriority(r);
      const prev = per.get(rnd);
      if (prev && num(prev._priority, -1) >= pri) continue;
      rec._priority = pri;
      per.set(rnd, rec);
    }
  }
  return out;
}

/** Build `{ birdies: [r1,…], bogeys: […], … }` arrays for rounds before `targetRound`. */
export function buildPriorByStatForPlayer(withinMap, dgId, targetRound) {
  const per = withinMap?.get?.(Math.round(num(dgId, NaN)));
  const tr = Math.round(num(targetRound, NaN));
  if (!per || !Number.isFinite(tr) || tr < 2) return null;
  /** @type {Record<string, number[]>} */
  const out = { birdies: [], bogeys: [], gir: [], fairways: [], putts: [] };
  for (let rn = 1; rn < tr; rn++) {
    const rec = per.get(rn);
    if (!rec) continue;
    for (const k of Object.keys(out)) {
      const v = num(rec[k], NaN);
      if (Number.isFinite(v)) out[k].push(v);
    }
  }
  const hasAny = Object.values(out).some((arr) => arr.length > 0);
  return hasAny ? out : null;
}

/**
 * Blend export hole counts toward this-week prior-round actuals — no softAlign / pars spreading.
 */
export function blendTowardWithinEventActuals(skillCounts, priorByStat, targetRound, opts = {}) {
  const tr = Math.round(num(targetRound, NaN));
  if (tr < 2 || !priorByStat || typeof priorByStat !== "object") return skillCounts || {};
  let nRounds = 0;
  for (const k of WITHIN_EVENT_FORM_BLEND_KEYS) {
    const arr = priorByStat[k];
    if (Array.isArray(arr) && arr.length > nRounds) nRounds = arr.length;
  }
  const out = { ...(skillCounts || {}) };
  const skill = opts?.skillCounts || {};
  const fieldMeans = opts?.fieldMeans || null;
  if (!nRounds) return out;
  let wBird = withinEventCountingBlendWeight(nRounds, opts?.playerRow);
  const wBogBase = wBird * WITHIN_EVENT_BOGEY_BLEND_SCALE;
  for (const k of WITHIN_EVENT_FORM_BLEND_KEYS) {
    const arr = priorByStat[k];
    if (!Array.isArray(arr) || !arr.length) continue;
    const avg = priorRoundCountingTarget(k, arr, fieldMeans, tr);
    const base = num(out[k], NaN);
    const skillBase = num(skill[k], base);
    if (!Number.isFinite(base) || !Number.isFinite(avg)) continue;
    let w = k === "bogeys" ? wBogBase : wBird;
    w = adjustWithinEventBlendWeight(w, avg, skillBase, opts?.playerRow, k);
    out[k] = (1 - w) * base + w * avg;
  }
  for (const k of WITHIN_EVENT_GIR_FW_BLEND_KEYS) {
    const arr = priorByStat[k];
    if (!Array.isArray(arr) || !arr.length) continue;
    const avg = priorRoundCountingTarget(k, arr, fieldMeans, tr);
    const base = num(out[k], NaN);
    if (!Number.isFinite(base) || !Number.isFinite(avg)) continue;
    const w = Math.min(0.48, wBird * 0.58);
    out[k] = (1 - w) * base + w * avg;
  }
  const mu = num(opts?.playerRow?.mu_sg, 0);
  const anchorW = Math.min(
    0.42,
    WITHIN_EVENT_SKILL_ANCHOR_BASE + WITHIN_EVENT_SKILL_ANCHOR_MU_SCALE * Math.max(0, mu),
  );
  if (anchorW > 0) {
    for (const k of ["birdies", "bogeys", "gir"]) {
      const sk = num(skill[k], NaN);
      const cur = num(out[k], NaN);
      if (Number.isFinite(sk) && Number.isFinite(cur)) {
        out[k] = (1 - anchorW) * cur + anchorW * sk;
      }
    }
  }
  const pars = residualParsFromHoleCounts(out);
  if (Number.isFinite(pars)) out.pars = pars;
  return out;
}

/** Field-day lift on bird/bog/GIR without score-to-par hole-count forcing. */
export function applyFieldDayCountingLiftNatural(st, targetRound, fieldMeans, venueScoring) {
  if (!st || typeof st !== "object") return st;
  const tr = Math.round(num(targetRound, NaN));
  if (tr < 2 || !fieldMeans || !venueScoring) return st;
  const rn = tr - 1;
  const birdLift = fieldDayCountingLift(fieldMeans.birdies?.[rn], venueScoring.venueAvgBirdies);
  const bogLift = fieldDayCountingLift(fieldMeans.bogeys?.[rn], venueScoring.venueAvgBogeys);
  const girLift = fieldDayCountingLift(fieldMeans.gir?.[rn], venueScoring.venueAvgGir);
  const fwLift = fieldDayCountingLift(fieldMeans.fairways?.[rn], venueScoring.venueAvgFairways);
  if (Number.isFinite(birdLift) && birdLift !== 0) st.birdies = Math.max(0.15, num(st.birdies, 0) + birdLift);
  if (Number.isFinite(bogLift) && bogLift !== 0) st.bogeys = Math.max(0.15, num(st.bogeys, 0) + bogLift);
  if (Number.isFinite(girLift) && girLift !== 0 && Number.isFinite(st.gir)) {
    st.gir = Math.max(6, Math.min(16, st.gir + girLift));
  }
  if (Number.isFinite(fwLift) && fwLift !== 0 && Number.isFinite(st.fairways)) {
    st.fairways = Math.max(2, st.fairways + fwLift);
  }
  const pars = residualParsFromHoleCounts(st);
  if (Number.isFinite(pars)) st.pars = pars;
  return st;
}

/** Blend skill/venue hole counts toward prior-round actuals this week (R2+). */
export function blendWithinEventProjectionCounts(skillCounts, priorByStat, targetRound, opts = {}) {
  const tr = Math.round(num(targetRound, NaN));
  if (tr < 2 || !priorByStat || typeof priorByStat !== "object") return skillCounts || {};
  let nRounds = 0;
  for (const k of WITHIN_EVENT_FORM_BLEND_KEYS) {
    const arr = priorByStat[k];
    if (Array.isArray(arr) && arr.length > nRounds) nRounds = arr.length;
  }
  const out = { ...(skillCounts || {}) };
  if (!nRounds) return out;
  const fieldMeans = opts?.fieldMeans || null;
  const wBird = withinEventCountingBlendWeight(nRounds, opts?.playerRow);
  const wBog = wBird * WITHIN_EVENT_BOGEY_BLEND_SCALE;
  for (const k of WITHIN_EVENT_FORM_BLEND_KEYS) {
    const arr = priorByStat[k];
    if (!Array.isArray(arr) || !arr.length) continue;
    const avg = priorRoundCountingTarget(k, arr, fieldMeans, tr);
    const base = num(out[k], NaN);
    if (!Number.isFinite(base) || !Number.isFinite(avg)) continue;
    const w = k === "bogeys" ? wBog : wBird;
    out[k] = (1 - w) * base + w * avg;
  }
  for (const k of WITHIN_EVENT_GIR_FW_BLEND_KEYS) {
    const arr = priorByStat[k];
    if (!Array.isArray(arr) || !arr.length) continue;
    const avg = priorRoundCountingTarget(k, arr, fieldMeans, tr);
    const base = num(out[k], NaN);
    if (!Number.isFinite(base) || !Number.isFinite(avg)) continue;
    const w = Math.min(0.55, wBird * 0.65);
    out[k] = (1 - w) * base + w * avg;
  }
  const stp = num(opts?.targetStp, NaN);
  if (Number.isFinite(stp)) {
    const aligned = softAlignHoleCountsToStp(
      {
        eagles: out.eagles,
        birdies: out.birdies,
        pars: out.pars,
        bogeys: out.bogeys,
        doubles: out.doubles,
      },
      stp,
      num(opts?.alignStrength, WITHIN_EVENT_ALIGN_STRENGTH),
    );
    out.eagles = aligned.eagles;
    out.birdies = aligned.birdies;
    out.pars = aligned.pars;
    out.bogeys = aligned.bogeys;
    out.doubles = aligned.doubles;
    const spread = spreadParsIntoBirdBogPairs(out, {
      venueBirdies: num(opts?.venueBirdies, NaN),
      venueBogeys: num(opts?.venueBogeys, NaN),
      venuePars: num(opts?.venuePars, NaN),
      spreadStrength: num(opts?.spreadStrength, WITHIN_EVENT_PAR_SPREAD_STRENGTH),
    });
    out.eagles = spread.eagles;
    out.birdies = spread.birdies;
    out.pars = spread.pars;
    out.bogeys = spread.bogeys;
    out.doubles = spread.doubles;
  } else {
    const e = Math.max(0, num(out.eagles, 0));
    const d = Math.max(0, num(out.doubles, 0));
    const b = num(out.birdies, NaN);
    const bg = num(out.bogeys, NaN);
    if (Number.isFinite(b) && Number.isFinite(bg)) {
      out.pars = Math.max(0.12, 18 - e - d - b - bg);
    }
  }
  return out;
}

export function fieldCountingMeansFromWithinEventMap(byDgRound, minPlayersOrOpts = 28) {
  const opts =
    typeof minPlayersOrOpts === "object" && minPlayersOrOpts !== null && !Array.isArray(minPlayersOrOpts)
      ? minPlayersOrOpts
      : { minPlayers: minPlayersOrOpts };
  const minPlayers = Math.max(1, Math.round(num(opts.minPlayers, 28)) || 28);
  const dgFilter = opts.dgFilter instanceof Set ? opts.dgFilter : null;
  /** @type {Record<string, Record<number, number>>} */
  const out = { birdies: {}, bogeys: {}, gir: {}, fairways: {} };
  if (!byDgRound || typeof byDgRound !== "object") return out;
  for (let rnd = 1; rnd <= 3; rnd++) {
    for (const key of Object.keys(out)) {
      const vals = [];
      for (const [dgKey, per] of byDgRound.entries()) {
        const dg = Math.round(num(dgKey, NaN));
        if (dgFilter && (!Number.isFinite(dg) || !dgFilter.has(dg))) continue;
        const rec = per?.get?.(rnd);
        if (rec && Number.isFinite(num(rec[key], NaN))) vals.push(num(rec[key], NaN));
      }
      if (vals.length >= minPlayers) {
        out[key][rnd] = Math.round((vals.reduce((a, b) => a + b, 0) / vals.length) * 100) / 100;
      }
    }
  }
  return out;
}

export function applyFieldDayCountingLiftToCounts(st, targetRound, fieldMeans, venueScoring, targetStpOpt) {
  if (!st || typeof st !== "object") return st;
  const tr = Math.round(num(targetRound, NaN));
  if (tr < 2 || !fieldMeans || !venueScoring) return st;
  const rn = tr - 1;
  const birdLift = fieldDayCountingLift(fieldMeans.birdies?.[rn], venueScoring.venueAvgBirdies);
  const bogLift = fieldDayCountingLift(fieldMeans.bogeys?.[rn], venueScoring.venueAvgBogeys);
  const girLift = fieldDayCountingLift(fieldMeans.gir?.[rn], venueScoring.venueAvgGir);
  const fwLift = fieldDayCountingLift(fieldMeans.fairways?.[rn], venueScoring.venueAvgFairways);
  if (Number.isFinite(birdLift) && birdLift !== 0) st.birdies = Math.max(0.15, num(st.birdies, 0) + birdLift);
  if (Number.isFinite(bogLift) && bogLift !== 0) st.bogeys = Math.max(0.15, num(st.bogeys, 0) + bogLift);
  const stp = num(targetStpOpt, NaN);
  if (Number.isFinite(stp)) {
    const aligned = softAlignHoleCountsToStp(
      {
        eagles: st.eagles,
        birdies: st.birdies,
        pars: st.pars,
        bogeys: st.bogeys,
        doubles: st.doubles,
      },
      stp,
      WITHIN_EVENT_ALIGN_STRENGTH,
    );
    st.birdies = aligned.birdies;
    st.pars = aligned.pars;
    st.bogeys = aligned.bogeys;
    const spread = spreadParsIntoBirdBogPairs(st, {
      venueBirdies: venueScoring?.venueAvgBirdies,
      venueBogeys: venueScoring?.venueAvgBogeys,
      venuePars: venueScoring?.venueAvgPars,
      spreadStrength: WITHIN_EVENT_PAR_SPREAD_STRENGTH,
    });
    st.birdies = spread.birdies;
    st.pars = spread.pars;
    st.bogeys = spread.bogeys;
  }
  if (Number.isFinite(girLift) && girLift !== 0 && Number.isFinite(st.gir)) {
    st.gir = Math.max(6, Math.min(16, st.gir + girLift));
  }
  if (Number.isFinite(fwLift) && fwLift !== 0 && Number.isFinite(st.fairways)) {
    st.fairways = Math.max(2, st.fairways + fwLift);
  }
  return st;
}

/** course_table.csv / course-table.json adj_score_to_par when CSV sample is thin. */
export function lookupAdjScoreToParFromCourseTable(courseLabel) {
  const ck = normCourseNameKey(courseLabel);
  if (!ck) return NaN;
  try {
    const root = join(dirname(fileURLToPath(import.meta.url)), "..");
    const paths = [join(root, "course-table.json"), join(root, "data", "course-table.json")];
    for (const p of paths) {
      if (!existsSync(p)) continue;
      const j = JSON.parse(readFileSync(p, "utf8"));
      const rows = Array.isArray(j?.rows) ? j.rows : [];
      for (const row of rows) {
        const rk = normCourseNameKey(row.course ?? row.course_name ?? "");
        if (rk && (rk === ck || rk.includes(ck) || ck.includes(rk))) {
          const v = num(row.adj_score_to_par, NaN);
          if (Number.isFinite(v) && Math.abs(v) < 6) return v;
        }
      }
    }
  } catch {
    /* ignore */
  }
  return NaN;
}

/** Shrink player SG at this venue vs field skill mean (round_projections.R course_fit_hist). */
export function applyVenueCourseFitToMu(mu_sg, dg_id, venueScoring, fieldMeanSg) {
  const cf = venueScoring?.courseFitByDg?.get(Math.round(num(dg_id, NaN)));
  const m = num(fieldMeanSg, NaN);
  if (!cf || cf.n < 5 || !Number.isFinite(m)) return mu_sg;
  const raw = cf.avgSg - m;
  const shrink = cf.n / (cf.n + 12);
  const w = cf.n >= 15 ? 0.35 : cf.n >= 5 ? 0.25 : 0;
  return clamp(mu_sg + w * shrink * raw, -4, 4);
}

/**
 * score_to_par: blend skill around venue mean with full-course player history (not hard anchor).
 */
export function resolveProjectionScoreToPar({
  dg_id,
  round,
  muForRound,
  course_par_18,
  venueScoring,
  pretRoundScore,
  fieldMeanMu,
  minPlayerRounds = 3,
  minFieldRounds = 25,
}) {
  const cp = num(course_par_18, NaN);
  if (!Number.isFinite(cp)) return { stp: -num(muForRound, 0), source: "skill_rating" };

  const pret = num(pretRoundScore, NaN);
  if (Number.isFinite(pret)) {
    return { stp: pret - cp, source: "pret_tournament" };
  }

  const skillRes = skillScoreToPar({
    muForRound,
    course_par_18,
    venueScoring,
    round,
    fieldMeanMu,
    minFieldRounds,
  });

  const dg = Math.round(num(dg_id, NaN));
  const pk = `${dg}|${Math.round(num(round, NaN))}`;
  const pr = venueScoring?.playerByRound?.get(pk);
  const pv = venueScoring?.playerByVenue?.get(dg);
  const playerAgg = mergePlayerVenueAgg(pv, pr, minPlayerRounds);
  if (!playerAgg || !Number.isFinite(playerAgg.avgScore)) return skillRes;

  const prOk = pr && pr.n >= minPlayerRounds;
  const playerStp = playerAgg.avgScore - cp;
  let wVenue = venuePlayerHistBlendWeight(prOk ? pr.n : playerAgg.n, muForRound);
  if (playerStp > skillRes.stp + 0.04) {
    wVenue = reduceVenueWeightWhenSkillBetter(wVenue, muForRound, skillRes.stp, playerStp, false);
  }
  if (wVenue <= 0) return skillRes;
  const stp = wVenue * playerStp + (1 - wVenue) * skillRes.stp;
  return {
    stp,
    source: wVenue >= 0.88 ? "player_venue_hist" : "player_venue_skill_blend",
  };
}
