/**
 * Cutoff-aware player × course × hole strokes-gained for walk-forward projections.
 *
 * Requires plays file from:
 *   npm run build:course-hole-sg -- --plays
 *   (or GOLF_COURSE_HOLE_SG_PLAYS=1)
 *
 * SG_hole = field_mean_score(course, hole | as-of) − player_score
 * Round feature: sum over holes of shrink(n)×mean_sg  → score_to_par ↓ when positive.
 */
import { createReadStream, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function envNum(name, fb) {
  const n = Number(process.env[name]);
  return Number.isFinite(n) ? n : fb;
}

function envOn(name, defaultOn = true) {
  const raw = String(process.env[name] ?? "").trim();
  if (!raw) return defaultOn;
  return raw !== "0" && raw.toLowerCase() !== "false" && raw !== "off";
}

export function holeSgBlendEnabled() {
  return envOn("GOLF_HOLE_SG_BLEND", true);
}

export function holeSgBlendWeight() {
  return Math.min(0.75, Math.max(0, envNum("GOLF_HOLE_SG_WEIGHT", 0.28)));
}

export function holeSgShrinkPrior() {
  return Math.max(1, envNum("GOLF_HOLE_SG_PRIOR_N", 4));
}

export function holeSgMinBaselineN() {
  return Math.max(10, Math.round(envNum("GOLF_HOLE_SG_MIN_BASELINE_N", 30)));
}

export function holeSgMaxAbsStp() {
  return Math.max(0.25, envNum("GOLF_HOLE_SG_MAX_ABS_STP", 1.25));
}

function playsPath(webRoot = WEB) {
  return (
    String(process.env.GOLF_COURSE_HOLE_SG_PLAYS_PATH || "").trim() ||
    join(webRoot, "data", "player_course_hole_sg_plays.csv")
  );
}

/** @type {Promise<Map<string, object[]>> | null} */
let playsByCoursePromise = null;
let playsWarnedMissing = false;

/**
 * Stream plays CSV once → Map(course_key → play[]).
 * Play: { dg, year, round, hole, score, par, tname, tid, time_ms }
 */
export async function loadHoleSgPlaysByCourse(webRoot = WEB) {
  if (playsByCoursePromise) return playsByCoursePromise;
  playsByCoursePromise = (async () => {
    /** @type {Map<string, object[]>} */
    const byCourse = new Map();
    const file = playsPath(webRoot);
    if (!existsSync(file)) {
      if (!playsWarnedMissing) {
        playsWarnedMissing = true;
        console.warn(
          `[hole-sg] Missing ${file} — run: npm run build:course-hole-sg -- --plays (hole blend skipped)`,
        );
      }
      return byCourse;
    }
    let n = 0;
    await new Promise((resolve, reject) => {
      createReadStream(file)
        .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
        .on("data", (r) => {
          const ck = normCourseNameKey(r.course_key || r.course_name || "");
          const dg = Math.round(num(r.dg_id, NaN));
          const hole = Math.round(num(r.hole, NaN));
          const score = Math.round(num(r.score, NaN));
          if (!ck || !Number.isFinite(dg) || hole < 1 || hole > 18 || !Number.isFinite(score)) return;
          const play = {
            dg,
            year: Math.round(num(r.year, NaN)),
            round: Math.round(num(r.round, NaN)),
            hole,
            score,
            par: Math.round(num(r.par, NaN)),
            tname: String(r.tournament_name || "").trim(),
            tid: String(r.tournament_id || "").trim(),
            time_ms: num(r.time_ms, NaN),
          };
          let arr = byCourse.get(ck);
          if (!arr) {
            arr = [];
            byCourse.set(ck, arr);
          }
          arr.push(play);
          n++;
        })
        .on("end", resolve)
        .on("error", reject);
    });
    console.log(
      `[hole-sg] Loaded ${n.toLocaleString()} hole plays across ${byCourse.size} courses`,
    );
    return byCourse;
  })();
  return playsByCoursePromise;
}

export function clearHoleSgPlaysCache() {
  playsByCoursePromise = null;
  playsWarnedMissing = false;
}

/**
 * Honest as-of filter: earlier rounds of this event, or strictly before cutoff / prior years.
 */
export function holePlayEligible(play, { cutoffMs, eventName, eventYear, targetRound }) {
  const yr = play.year;
  const rnd = play.round;
  const sameEvent =
    eventName && play.tname ? eventsLikelySame(eventName, play.tname) : false;

  if (sameEvent && Number.isFinite(eventYear) && yr === eventYear) {
    if (!Number.isFinite(rnd) || !Number.isFinite(targetRound)) return false;
    return rnd >= 1 && rnd < targetRound;
  }

  if (Number.isFinite(play.time_ms) && Number.isFinite(cutoffMs)) {
    return play.time_ms < cutoffMs;
  }

  if (Number.isFinite(yr) && Number.isFinite(eventYear)) {
    return yr < eventYear;
  }
  return false;
}

/**
 * @returns {Promise<Map<number, { stpAdj: number, roundSg: number, nHoles: number, nPlays: number, coverage: number }>>}
 */
export async function buildHoleSgAdjustmentsAsOf({
  webRoot = WEB,
  courseKey,
  courseName = "",
  cutoffMs,
  eventName,
  eventYear,
  targetRound,
  fieldDgIds = null,
  weight = holeSgBlendWeight(),
  priorN = holeSgShrinkPrior(),
  minBaselineN = holeSgMinBaselineN(),
  maxAbsStp = holeSgMaxAbsStp(),
} = {}) {
  /** @type {Map<number, { stpAdj: number, roundSg: number, nHoles: number, nPlays: number, coverage: number }>} */
  const out = new Map();
  if (!holeSgBlendEnabled() || weight <= 0) return out;

  const ck = normCourseNameKey(courseKey || courseName || "");
  if (!ck) return out;

  const byCourse = await loadHoleSgPlaysByCourse(webRoot);
  const plays = byCourse.get(ck);
  if (!plays?.length) return out;

  const fieldSet =
    fieldDgIds instanceof Set
      ? fieldDgIds
      : Array.isArray(fieldDgIds) && fieldDgIds.length
        ? new Set(fieldDgIds.map((d) => Math.round(num(d, NaN))).filter(Number.isFinite))
        : null;

  const ctx = { cutoffMs, eventName, eventYear, targetRound };

  /** hole → { sum, n } */
  const baseline = new Map();
  /** dg|hole → { sumScore, n } */
  const playerHole = new Map();

  for (const p of plays) {
    if (!holePlayEligible(p, ctx)) continue;
    let bl = baseline.get(p.hole);
    if (!bl) {
      bl = { sum: 0, n: 0 };
      baseline.set(p.hole, bl);
    }
    bl.sum += p.score;
    bl.n += 1;

    if (fieldSet && !fieldSet.has(p.dg)) continue;
    const pk = `${p.dg}|${p.hole}`;
    let ph = playerHole.get(pk);
    if (!ph) {
      ph = { dg: p.dg, hole: p.hole, sum: 0, n: 0 };
      playerHole.set(pk, ph);
    }
    ph.sum += p.score;
    ph.n += 1;
  }

  /** hole → field mean */
  const fieldMean = new Map();
  for (const [hole, bl] of baseline) {
    if (bl.n >= minBaselineN) fieldMean.set(hole, bl.sum / bl.n);
  }
  if (!fieldMean.size) return out;

  /** dg → { sumSg, nHoles, nPlays, holesCovered } */
  const agg = new Map();
  for (const ph of playerHole.values()) {
    const fm = fieldMean.get(ph.hole);
    if (!Number.isFinite(fm) || ph.n < 1) continue;
    const meanScore = ph.sum / ph.n;
    const rawSg = fm - meanScore;
    const shrink = ph.n / (ph.n + priorN);
    const sg = shrink * rawSg;
    let a = agg.get(ph.dg);
    if (!a) {
      a = { sumSg: 0, nHoles: 0, nPlays: 0 };
      agg.set(ph.dg, a);
    }
    a.sumSg += sg;
    a.nHoles += 1;
    a.nPlays += ph.n;
  }

  for (const [dg, a] of agg) {
    if (a.nHoles < 4) continue;
    const coverage = Math.min(1, a.nHoles / 18);
    // Positive hole SG (better than field) → lower expected score_to_par.
    let stpAdj = -weight * coverage * a.sumSg;
    if (stpAdj > maxAbsStp) stpAdj = maxAbsStp;
    if (stpAdj < -maxAbsStp) stpAdj = -maxAbsStp;
    out.set(dg, {
      stpAdj: Math.round(stpAdj * 1000) / 1000,
      roundSg: Math.round(a.sumSg * 1000) / 1000,
      nHoles: a.nHoles,
      nPlays: a.nPlays,
      coverage: Math.round(coverage * 1000) / 1000,
    });
  }
  return out;
}

/**
 * Apply hole SG STP adjustment; returns new stp + source tag.
 */
export function applyHoleSgToScoreStp(stp, holeAdj, source = "") {
  if (!holeAdj || !Number.isFinite(holeAdj.stpAdj) || holeAdj.stpAdj === 0) {
    return { stp, source };
  }
  const next = Math.round((stp + holeAdj.stpAdj) * 1000) / 1000;
  const tag = source ? `${source}+hole_sg` : "hole_sg";
  return { stp: next, source: tag };
}

/**
 * Light birdie count nudge from positive hole SG (strokes → ~birdies).
 */
export function applyHoleSgToBirdies(birdies, holeAdj, frac = 0.12) {
  if (!holeAdj || !Number.isFinite(holeAdj.roundSg)) return birdies;
  const b = num(birdies, NaN);
  if (!Number.isFinite(b)) return birdies;
  const w = Math.min(0.35, Math.max(0, frac));
  return Math.round((b + w * holeAdj.roundSg) * 100) / 100;
}
