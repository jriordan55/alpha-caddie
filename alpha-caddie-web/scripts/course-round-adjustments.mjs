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
 * @typedef {{ avgScore: number, avgStp: number, n: number }} VenueScoreAgg
 * @typedef {{
 *   venueAvgStp: number,
 *   venueAvgScore: number,
 *   nVenueRounds: number,
 *   source: string,
 *   fieldByRound: Map<number, VenueScoreAgg>,
 *   playerByRound: Map<string, VenueScoreAgg>,
 *   courseFitByDg: Map<number, { avgSg: number, n: number }>,
 * }} VenueHistoricalScoring
 */

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
  let sumStp = 0;
  let sumScore = 0;
  let nVenue = 0;
  /** @type {Map<number, { sumScore: number, sumStp: number, n: number }>} */
  const fieldRaw = new Map();
  /** @type {Map<string, { sumScore: number, sumStp: number, n: number }>} */
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

      const stp = rs - cp;
      nVenue++;
      sumStp += stp;
      sumScore += rs;

      const fr = fieldRaw.get(rnd) || { sumScore: 0, sumStp: 0, n: 0 };
      fr.sumScore += rs;
      fr.sumStp += stp;
      fr.n++;
      fieldRaw.set(rnd, fr);

      const dg = Math.round(num(row.dg_id, NaN));
      if (Number.isFinite(dg)) {
        const pk = `${dg}|${rnd}`;
        const pr = playerRaw.get(pk) || { sumScore: 0, sumStp: 0, n: 0 };
        pr.sumScore += rs;
        pr.sumStp += stp;
        pr.n++;
        playerRaw.set(pk, pr);

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

  const finalize = (raw) => ({
    avgScore: raw.n > 0 ? raw.sumScore / raw.n : NaN,
    avgStp: raw.n > 0 ? raw.sumStp / raw.n : NaN,
    n: raw.n,
  });

  const fieldByRound = new Map();
  for (const [rnd, raw] of fieldRaw) fieldByRound.set(rnd, finalize(raw));

  const playerByRound = new Map();
  for (const [pk, raw] of playerRaw) playerByRound.set(pk, finalize(raw));

  const courseFitByDg = new Map();
  for (const [dg, raw] of fitRaw) {
    courseFitByDg.set(dg, { avgSg: raw.sumSg / raw.n, n: raw.n });
  }

  let venueAvgStp = nVenue >= 40 ? sumStp / nVenue : NaN;
  let source = nVenue >= 40 ? "historical_csv" : "none";
  if (!Number.isFinite(venueAvgStp)) {
    const adj = lookupAdjScoreToParFromCourseTable(courseLabelOpt || courseKeyOpt);
    if (Number.isFinite(adj)) {
      venueAvgStp = adj;
      source = "course_table";
    }
  }

  return {
    venueAvgStp,
    venueAvgScore: nVenue >= 40 ? sumScore / nVenue : NaN,
    nVenueRounds: nVenue,
    source,
    fieldByRound,
    playerByRound,
    courseFitByDg,
  };
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
