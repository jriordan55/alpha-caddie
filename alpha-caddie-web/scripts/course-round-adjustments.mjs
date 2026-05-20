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
      if (Number.isFinite(yr) && (yr < cy - 1 || yr > cy + 1)) return;

      const rnd = Math.round(num(row.round_num, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return;

      const cp = num(row.course_par, NaN);
      const rs = num(row.round_score, NaN);
      if (Number.isFinite(cp) && Number.isFinite(rs)) {
        const stp = rs - cp;
        const b = ctx.byRound.get(rnd) || { n: 0, sumStp: 0 };
        b.n++;
        b.sumStp += stp;
        ctx.byRound.set(rnd, b);
      }

      const dg = Math.round(num(row.dg_id, NaN));
      const sg = num(row.sg_total, NaN);
      if (Number.isFinite(dg) && Number.isFinite(sg)) {
        ctx.playerRounds.push({ dg_id: dg, round: rnd, sg_total: sg });
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
export function buildWithinEventFormMap(ctx, basePlayers, k = 0.02, cap = 0.3) {
  const map = new Map();
  if (!k || !ctx?.playerRounds?.length) return map;

  const baseMu = new Map();
  for (const p of basePlayers || []) {
    const id = Math.round(num(p.dg_id));
    if (Number.isFinite(id)) baseMu.set(id, num(p.mu_sg, 0));
  }

  const byDgRound = new Map();
  for (const pr of ctx.playerRounds) {
    const id = Math.round(num(pr.dg_id));
    const rnd = Math.round(num(pr.round));
    if (!Number.isFinite(id) || rnd < 1 || rnd > 3) continue;
    const base = baseMu.get(id);
    if (!Number.isFinite(base)) continue;
    const surplus = num(pr.sg_total, NaN) - base;
    if (!Number.isFinite(surplus)) continue;
    byDgRound.set(`${id}|${rnd}`, surplus);
  }

  for (const [id, base] of baseMu) {
    for (let tr = 2; tr <= 4; tr++) {
      let sh = 0;
      for (let rn = 1; rn < tr; rn++) {
        const s = byDgRound.get(`${id}|${rn}`);
        if (Number.isFinite(s)) sh += k * s;
      }
      if (!Number.isFinite(sh)) sh = 0;
      sh = clamp(sh, -cap, cap);
      map.set(`${id}|${tr}`, sh);
    }
  }
  return map;
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
function softAlignHoleCountsToStp(counts, targetStp, strength = 0.58) {
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

function coalesceVenueCount(playerVal, fieldVal, skillVal) {
  if (Number.isFinite(playerVal)) return playerVal;
  if (Number.isFinite(fieldVal)) return fieldVal;
  return skillVal;
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
      if (Number.isFinite(yr) && (yr < cy - 8 || yr > cy + 1)) return;

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
    courseFitByDg,
  };
}

/**
 * Hole-count markets: player history at venue → field avg at venue (round) → skill-based projection.
 * Aligns eagles/birdies/bogeys/doubles/pars to `targetStp` when score came from skill path.
 */
export function resolveProjectionCounts({
  dg_id,
  round,
  skillCounts,
  venueScoring,
  targetStp,
  nFairwayHoles = 14,
  minPlayerRounds = 3,
  minFieldRounds = 25,
}) {
  const sk = skillCounts || {};
  const rnd = Math.round(num(round, NaN));
  const pk = `${Math.round(num(dg_id, NaN))}|${rnd}`;
  const pr = venueScoring?.playerByRound?.get(pk);
  const fr = venueScoring?.fieldByRound?.get(rnd);
  const prOk = pr && pr.n >= minPlayerRounds;
  const frOk = fr && fr.n >= minFieldRounds;

  let eagles = coalesceVenueCount(prOk ? pr.avgEagles : NaN, frOk ? fr.avgEagles : NaN, sk.eagles);
  let birdies = coalesceVenueCount(prOk ? pr.avgBirdies : NaN, frOk ? fr.avgBirdies : NaN, sk.birdies);
  let bogeys = coalesceVenueCount(prOk ? pr.avgBogeys : NaN, frOk ? fr.avgBogeys : NaN, sk.bogeys);
  let doubles = coalesceVenueCount(prOk ? pr.avgDoubles : NaN, frOk ? fr.avgDoubles : NaN, sk.doubles);
  let pars = coalesceVenueCount(prOk ? pr.avgPars : NaN, frOk ? fr.avgPars : NaN, sk.pars);
  let gir = coalesceVenueCount(prOk ? pr.avgGir : NaN, frOk ? fr.avgGir : NaN, sk.gir);
  let fairways = coalesceVenueCount(prOk ? pr.avgFairways : NaN, frOk ? fr.avgFairways : NaN, sk.fairways);
  let putts = coalesceVenueCount(prOk ? pr.avgPutts : NaN, frOk ? fr.avgPutts : NaN, sk.putts);

  eagles = Math.max(0, num(eagles, 0));
  birdies = Math.max(0.15, num(birdies, 0));
  bogeys = Math.max(0.15, num(bogeys, 0));
  doubles = Math.max(0.04, num(doubles, 0));
  if (!Number.isFinite(pars)) pars = Math.max(0.12, 18 - eagles - birdies - bogeys - doubles);

  const t = num(targetStp, NaN);
  if (Number.isFinite(t)) {
    const aligned = softAlignHoleCountsToStp({ eagles, birdies, pars, bogeys, doubles }, t);
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
 * score_to_par: player’s own history at this course when enough rounds; otherwise spread skill
 * around the venue’s historical scoring average (per-round field mean when available).
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

  const pk = `${Math.round(num(dg_id, NaN))}|${Math.round(num(round, NaN))}`;
  const pr = venueScoring?.playerByRound?.get(pk);
  if (pr && pr.n >= minPlayerRounds && Number.isFinite(pr.avgScore)) {
    return { stp: pr.avgScore - cp, source: "player_venue_hist" };
  }

  const pret = num(pretRoundScore, NaN);
  if (Number.isFinite(pret)) {
    return { stp: pret - cp, source: "pret_tournament" };
  }

  const rnd = Math.round(num(round, NaN));
  const fr = venueScoring?.fieldByRound?.get(rnd);
  let venueStp = num(venueScoring?.venueAvgStp, NaN);
  let source = "skill_around_venue_mean";
  if (fr && fr.n >= minFieldRounds && Number.isFinite(fr.avgStp)) {
    venueStp = fr.avgStp;
    source = "skill_around_round_venue_mean";
  }

  const mu = num(muForRound, 0);
  const fm = num(fieldMeanMu, 0);
  if (Number.isFinite(venueStp)) {
    return { stp: venueStp - (mu - fm), source };
  }
  return { stp: -mu, source: "skill_rating" };
}
