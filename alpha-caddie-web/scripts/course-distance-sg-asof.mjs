/**
 * Cutoff-aware approach + putting SG by distance bucket, with heavy weight on
 * the current course's shot mix and the player's history at that course.
 *
 * Sources:
 *   data/round_sg_by_distance.csv
 *   data/round_sg_putt_by_distance.csv
 * Joined to historical_rounds_all.csv for course_key + event_completed.
 *
 * Positive SG → lower expected score_to_par (same sign as hole SG).
 */
import { createReadStream, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  SG_DISTANCE_BUCKETS,
  SG_DISTANCE_VALUE_KEYS,
  SG_DISTANCE_COUNT_KEYS,
  syntheticTourIds as appSyntheticTourIds,
  normEvt,
} from "./sg-distance-fields.mjs";
import {
  SG_PUTT_DISTANCE_BUCKETS,
  SG_PUTT_DISTANCE_VALUE_KEYS,
  SG_PUTT_DISTANCE_COUNT_KEYS,
} from "./sg-putt-distance-fields.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = join(WEB, "..");

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

export function distanceSgBlendEnabled() {
  return envOn("GOLF_DISTANCE_SG_BLEND", true);
}

/** Overall STP weight for distance-bucket signal (on top of hole SG). */
export function distanceSgBlendWeight() {
  return Math.min(0.85, Math.max(0, envNum("GOLF_DISTANCE_SG_WEIGHT", 0.42)));
}

/** Share of signal from current-course history vs tour-wide (huge course focus). */
export function distanceSgCourseFocus() {
  return Math.min(0.95, Math.max(0.5, envNum("GOLF_DISTANCE_SG_COURSE_FOCUS", 0.88)));
}

export function distanceSgShrinkPrior() {
  return Math.max(1, envNum("GOLF_DISTANCE_SG_PRIOR_N", 6));
}

export function distanceSgMaxAbsStp() {
  return Math.max(0.25, envNum("GOLF_DISTANCE_SG_MAX_ABS_STP", 1.35));
}

/** Approach vs putt mix inside the distance signal. */
export function distanceSgAppShare() {
  return Math.min(0.85, Math.max(0.15, envNum("GOLF_DISTANCE_SG_APP_SHARE", 0.55)));
}

function parseEventCompletedMs(s) {
  const raw = String(s || "").trim();
  const iso = raw.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = raw.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) {
    return Date.parse(
      `${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`,
    );
  }
  return NaN;
}

function histPaths(webRoot = WEB) {
  const candidates = [
    join(webRoot, "data", "historical_rounds_all.csv"),
    join(REPO, "data", "historical_rounds_all.csv"),
  ];
  return candidates.filter((p) => existsSync(p));
}

/** @type {Promise<Map<string, { course_key: string, course_name: string, time_ms: number }>> | null} */
let histJoinPromise = null;

async function loadHistCourseTimeIndex(webRoot = WEB) {
  if (histJoinPromise) return histJoinPromise;
  histJoinPromise = (async () => {
    /** @type {Map<string, { course_key: string, course_name: string, time_ms: number }>} */
    const idx = new Map();
    for (const file of histPaths(webRoot)) {
      await new Promise((resolve, reject) => {
        createReadStream(file)
          .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
          .on("data", (r) => {
            const dg = Math.round(num(r.dg_id, NaN));
            const yr = Math.round(num(r.year, NaN));
            const rnd = Math.round(num(r.round_num, NaN));
            const cname = String(r.course_name || "").trim();
            if (!Number.isFinite(dg) || !Number.isFinite(yr) || !Number.isFinite(rnd) || !cname) return;
            let time_ms = parseEventCompletedMs(r.event_completed);
            if (!Number.isFinite(time_ms)) {
              time_ms = Date.parse(`${yr}-06-01T12:00:00Z`) + rnd * 86400000;
            }
            const payload = {
              course_key: normCourseNameKey(cname),
              course_name: cname,
              time_ms,
            };
            const evt = normEvt(r.event_name);
            if (evt) idx.set(`n|${dg}|${yr}|${evt}|${rnd}`, payload);
            const eid = String(r.event_id || "").trim();
            if (eid) {
              idx.set(`e|${dg}|${yr}|${eid}|${rnd}`, payload);
              for (const syn of appSyntheticTourIds(yr, eid)) {
                idx.set(`t|${dg}|${syn}|${rnd}`, payload);
              }
            }
          })
          .on("end", resolve)
          .on("error", reject);
      });
    }
    return idx;
  })();
  return histJoinPromise;
}

function resolveHistJoin(rec, histIdx) {
  const dg = Math.round(num(rec.dg_id, NaN));
  const rnd = Math.round(num(rec.round, NaN));
  const yr = Math.round(num(rec.year, NaN));
  if (!Number.isFinite(dg) || !Number.isFinite(rnd)) return null;
  const tid = String(rec.tournament_id || "").trim();
  if (tid) {
    const hit = histIdx.get(`t|${dg}|${tid}|${rnd}`);
    if (hit) return hit;
  }
  const evt = normEvt(rec.tournament_name);
  if (Number.isFinite(yr) && evt) {
    const hit = histIdx.get(`n|${dg}|${yr}|${evt}|${rnd}`);
    if (hit) return hit;
  }
  if (Number.isFinite(yr)) {
    for (const syn of appSyntheticTourIds(yr, tid.replace(/^R20\d{2}/i, ""))) {
      const hit = histIdx.get(`t|${dg}|${syn}|${rnd}`);
      if (hit) return hit;
    }
  }
  return null;
}

/**
 * Round eligible under walk-forward cutoff (same rules as hole SG).
 */
export function distanceRoundEligible(rec, { cutoffMs, eventName, eventYear, targetRound }) {
  const yr = rec.year;
  const rnd = rec.round;
  const sameEvent =
    eventName && rec.tname ? eventsLikelySame(eventName, rec.tname) : false;
  if (sameEvent && Number.isFinite(eventYear) && yr === eventYear) {
    if (!Number.isFinite(rnd) || !Number.isFinite(targetRound)) return false;
    return rnd >= 1 && rnd < targetRound;
  }
  if (Number.isFinite(rec.time_ms) && Number.isFinite(cutoffMs)) {
    return rec.time_ms < cutoffMs;
  }
  if (Number.isFinite(yr) && Number.isFinite(eventYear)) {
    return yr < eventYear;
  }
  return false;
}

function readBucketVals(r, valueKeys, countKeys) {
  /** @type {Record<string, number>} */
  const sg = {};
  /** @type {Record<string, number>} */
  const n = {};
  for (let i = 0; i < valueKeys.length; i++) {
    const vk = valueKeys[i];
    const ck = countKeys[i];
    const sv = num(r[vk], NaN);
    const nv = num(r[ck], 0);
    if (Number.isFinite(sv)) sg[vk] = sv;
    if (Number.isFinite(nv) && nv > 0) n[ck] = nv;
  }
  return { sg, n };
}

/** @type {Promise<{ byCourse: Map<string, object[]>, all: object[] }> | null} */
let distanceRoundsPromise = null;
let distanceWarned = false;

/**
 * Load approach + putt distance rounds joined to course.
 * Each enriched row: { dg, year, round, tname, tid, course_key, time_ms, appSg, appN, puttSg, puttN, appTotal, puttTotal }
 */
export async function loadDistanceSgRoundsByCourse(webRoot = WEB) {
  if (distanceRoundsPromise) return distanceRoundsPromise;
  distanceRoundsPromise = (async () => {
    const histIdx = await loadHistCourseTimeIndex(webRoot);
    const appFile = join(webRoot, "data", "round_sg_by_distance.csv");
    const puttFile = join(webRoot, "data", "round_sg_putt_by_distance.csv");
    if (!existsSync(appFile) && !existsSync(puttFile)) {
      if (!distanceWarned) {
        distanceWarned = true;
        console.warn("[distance-sg] Missing round_sg_*_by_distance.csv — distance blend skipped");
      }
      return { byCourse: new Map(), all: [] };
    }

    /** key dg|tid|rnd or dg|year|evt|rnd → partial */
    const merged = new Map();

    function upsert(r, kind) {
      const dg = Math.round(num(r.dg_id, NaN));
      const rnd = Math.round(num(r.round, NaN));
      const yr = Math.round(num(r.year, NaN));
      const tid = String(r.tournament_id || "").trim();
      const tname = String(r.tournament_name || "").trim();
      if (!Number.isFinite(dg) || !Number.isFinite(rnd)) return;
      const key = tid
        ? `t|${dg}|${tid}|${rnd}`
        : `n|${dg}|${yr}|${normEvt(tname)}|${rnd}`;
      let row = merged.get(key);
      if (!row) {
        const join = resolveHistJoin(
          { dg_id: dg, round: rnd, year: yr, tournament_id: tid, tournament_name: tname },
          histIdx,
        );
        if (!join?.course_key) return;
        row = {
          dg,
          year: yr,
          round: rnd,
          tid,
          tname,
          course_key: join.course_key,
          course_name: join.course_name,
          time_ms: join.time_ms,
          appSg: {},
          appN: {},
          puttSg: {},
          puttN: {},
          appTotal: NaN,
          appTotalN: 0,
          puttTotal: NaN,
          puttTotalN: 0,
        };
        merged.set(key, row);
      }
      if (kind === "app") {
        const { sg, n } = readBucketVals(r, SG_DISTANCE_VALUE_KEYS, SG_DISTANCE_COUNT_KEYS);
        Object.assign(row.appSg, sg);
        Object.assign(row.appN, n);
        row.appTotal = num(r.sg_app_dist_total, row.appTotal);
        row.appTotalN = num(r.n_app_dist, row.appTotalN);
      } else {
        const { sg, n } = readBucketVals(
          r,
          SG_PUTT_DISTANCE_VALUE_KEYS,
          SG_PUTT_DISTANCE_COUNT_KEYS,
        );
        Object.assign(row.puttSg, sg);
        Object.assign(row.puttN, n);
        row.puttTotal = num(r.sg_putt_dist_total, row.puttTotal);
        row.puttTotalN = num(r.n_putt_dist, row.puttTotalN);
      }
    }

    if (existsSync(appFile)) {
      await new Promise((resolve, reject) => {
        createReadStream(appFile)
          .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
          .on("data", (r) => upsert(r, "app"))
          .on("end", resolve)
          .on("error", reject);
      });
    }
    if (existsSync(puttFile)) {
      await new Promise((resolve, reject) => {
        createReadStream(puttFile)
          .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
          .on("data", (r) => upsert(r, "putt"))
          .on("end", resolve)
          .on("error", reject);
      });
    }

    /** @type {Map<string, object[]>} */
    const byCourse = new Map();
    const all = [...merged.values()];
    for (const row of all) {
      let arr = byCourse.get(row.course_key);
      if (!arr) {
        arr = [];
        byCourse.set(row.course_key, arr);
      }
      arr.push(row);
    }
    console.log(
      `[distance-sg] Loaded ${all.length.toLocaleString()} course-joined rounds across ${byCourse.size} courses`,
    );
    return { byCourse, all };
  })();
  return distanceRoundsPromise;
}

export function clearDistanceSgCache() {
  distanceRoundsPromise = null;
  histJoinPromise = null;
  distanceWarned = false;
}

function accumulatePlayerBuckets(store, dg, sgMap, nMap, buckets, prefix) {
  let a = store.get(dg);
  if (!a) {
    a = { dg, buckets: new Map(), totalSum: 0, totalN: 0, rounds: 0 };
    store.set(dg, a);
  }
  a.rounds += 1;
  for (const b of buckets) {
    const vk = `sg_${b}`;
    const ck = `n_${b}`;
    const sg = num(sgMap[vk], NaN);
    const n = num(nMap[ck], 0);
    if (!Number.isFinite(sg) || !(n > 0)) continue;
    let cell = a.buckets.get(b);
    if (!cell) {
      cell = { sumSgN: 0, n: 0 };
      a.buckets.set(b, cell);
    }
    // weight by shot count within round
    cell.sumSgN += sg * n;
    cell.n += n;
  }
  const totKey = prefix === "app" ? "appTotal" : "puttTotal";
  const totNKey = prefix === "app" ? "appTotalN" : "puttTotalN";
  // totals passed separately by caller
  void totKey;
  void totNKey;
}

function addTotals(store, dg, totalSg, totalN) {
  const a = store.get(dg);
  if (!a) return;
  if (Number.isFinite(totalSg) && totalN > 0) {
    a.totalSum += totalSg * totalN;
    a.totalN += totalN;
  }
}

function meanBucketSg(cell, priorN) {
  if (!cell || !(cell.n > 0)) return { mean: NaN, n: 0, shrunk: 0 };
  const mean = cell.sumSgN / cell.n;
  const shrink = cell.n / (cell.n + priorN);
  return { mean, n: cell.n, shrunk: shrink * mean };
}

function courseMixShares(rounds, buckets, nPrefix) {
  /** @type {Map<string, number>} */
  const counts = new Map();
  let total = 0;
  for (const r of rounds) {
    const nMap = nPrefix === "app" ? r.appN : r.puttN;
    for (const b of buckets) {
      const n = num(nMap[`n_${b}`], 0);
      if (!(n > 0)) continue;
      counts.set(b, (counts.get(b) || 0) + n);
      total += n;
    }
  }
  /** @type {Map<string, number>} */
  const shares = new Map();
  if (!(total > 0)) return shares;
  for (const [b, n] of counts) shares.set(b, n / total);
  return shares;
}

function blendCourseTour(courseCell, tourCell, courseFocus, priorN, minCourseN = 8) {
  const c = meanBucketSg(courseCell, priorN);
  const t = meanBucketSg(tourCell, priorN);
  if (!Number.isFinite(c.shrunk) && !Number.isFinite(t.shrunk)) return NaN;
  if (!Number.isFinite(c.shrunk)) return t.shrunk;
  if (!Number.isFinite(t.shrunk)) return c.shrunk;
  // Soften course focus when thin at this venue
  let w = courseFocus;
  if (c.n < minCourseN) w = courseFocus * (c.n / minCourseN);
  w = Math.min(courseFocus, Math.max(0.35, w));
  return w * c.shrunk + (1 - w) * t.shrunk;
}

/**
 * @returns {Promise<Map<number, {
 *   stpAdj: number,
 *   appSg: number,
 *   puttSg: number,
 *   combinedSg: number,
 *   courseRounds: number,
 *   tourRounds: number,
 * }>>}
 */
export async function buildDistanceSgAdjustmentsAsOf({
  webRoot = WEB,
  courseKey,
  courseName = "",
  cutoffMs,
  eventName,
  eventYear,
  targetRound,
  fieldDgIds = null,
  weight = distanceSgBlendWeight(),
  courseFocus = distanceSgCourseFocus(),
  priorN = distanceSgShrinkPrior(),
  maxAbsStp = distanceSgMaxAbsStp(),
  appShare = distanceSgAppShare(),
} = {}) {
  /** @type {Map<number, object>} */
  const out = new Map();
  if (!distanceSgBlendEnabled() || weight <= 0) return out;

  const ck = normCourseNameKey(courseKey || courseName || "");
  if (!ck) return out;

  const { byCourse, all } = await loadDistanceSgRoundsByCourse(webRoot);
  const courseRoundsAll = byCourse.get(ck) || [];
  if (!courseRoundsAll.length && !all.length) return out;

  const fieldSet =
    fieldDgIds instanceof Set
      ? fieldDgIds
      : Array.isArray(fieldDgIds) && fieldDgIds.length
        ? new Set(fieldDgIds.map((d) => Math.round(num(d, NaN))).filter(Number.isFinite))
        : null;

  const ctx = { cutoffMs, eventName, eventYear, targetRound };
  const courseRounds = courseRoundsAll.filter((r) => distanceRoundEligible(r, ctx));
  const tourRounds = all.filter((r) => distanceRoundEligible(r, ctx));

  const appMix = courseMixShares(courseRounds, SG_DISTANCE_BUCKETS, "app");
  const puttMix = courseMixShares(courseRounds, SG_PUTT_DISTANCE_BUCKETS, "putt");

  /** @type {Map<number, object>} */
  const courseApp = new Map();
  /** @type {Map<number, object>} */
  const coursePutt = new Map();
  /** @type {Map<number, object>} */
  const tourApp = new Map();
  /** @type {Map<number, object>} */
  const tourPutt = new Map();

  function ingest(rounds, appStore, puttStore, onlyField) {
    for (const r of rounds) {
      if (onlyField && fieldSet && !fieldSet.has(r.dg)) continue;
      accumulatePlayerBuckets(appStore, r.dg, r.appSg, r.appN, SG_DISTANCE_BUCKETS, "app");
      addTotals(appStore, r.dg, r.appTotal, r.appTotalN);
      accumulatePlayerBuckets(puttStore, r.dg, r.puttSg, r.puttN, SG_PUTT_DISTANCE_BUCKETS, "putt");
      addTotals(puttStore, r.dg, r.puttTotal, r.puttTotalN);
    }
  }

  // Course aggs: field players only (faster). Tour aggs: same field set.
  ingest(courseRounds, courseApp, coursePutt, true);
  ingest(tourRounds, tourApp, tourPutt, true);

  const dgs = new Set([...courseApp.keys(), ...tourApp.keys(), ...coursePutt.keys(), ...tourPutt.keys()]);
  const puttShare = 1 - appShare;

  for (const dg of dgs) {
    if (fieldSet && !fieldSet.has(dg)) continue;
    const cApp = courseApp.get(dg);
    const tApp = tourApp.get(dg);
    const cPutt = coursePutt.get(dg);
    const tPutt = tourPutt.get(dg);
    const courseRnd = Math.max(cApp?.rounds || 0, cPutt?.rounds || 0);
    const tourRnd = Math.max(tApp?.rounds || 0, tPutt?.rounds || 0);
    if (courseRnd < 1 && tourRnd < 2) continue;

    // Approach: course-mix-weighted buckets (fallback to total SG)
    let appSg = 0;
    let appW = 0;
    if (appMix.size) {
      for (const [b, share] of appMix) {
        const blended = blendCourseTour(
          cApp?.buckets.get(b),
          tApp?.buckets.get(b),
          courseFocus,
          priorN,
        );
        if (!Number.isFinite(blended)) continue;
        appSg += share * blended;
        appW += share;
      }
    }
    if (!(appW > 0.3)) {
      // fallback: totals
      const cTot = cApp && cApp.totalN > 0 ? { sumSgN: cApp.totalSum, n: cApp.totalN } : null;
      const tTot = tApp && tApp.totalN > 0 ? { sumSgN: tApp.totalSum, n: tApp.totalN } : null;
      const blended = blendCourseTour(cTot, tTot, courseFocus, priorN, 12);
      if (Number.isFinite(blended)) {
        appSg = blended;
        appW = 1;
      }
    } else {
      appSg /= appW;
    }

    let puttSg = 0;
    let puttW = 0;
    if (puttMix.size) {
      for (const [b, share] of puttMix) {
        const blended = blendCourseTour(
          cPutt?.buckets.get(b),
          tPutt?.buckets.get(b),
          courseFocus,
          priorN,
        );
        if (!Number.isFinite(blended)) continue;
        puttSg += share * blended;
        puttW += share;
      }
    }
    if (!(puttW > 0.3)) {
      const cTot = cPutt && cPutt.totalN > 0 ? { sumSgN: cPutt.totalSum, n: cPutt.totalN } : null;
      const tTot = tPutt && tPutt.totalN > 0 ? { sumSgN: tPutt.totalSum, n: tPutt.totalN } : null;
      const blended = blendCourseTour(cTot, tTot, courseFocus, priorN, 12);
      if (Number.isFinite(blended)) {
        puttSg = blended;
        puttW = 1;
      }
    } else {
      puttSg /= puttW;
    }

    if (!Number.isFinite(appSg) && !Number.isFinite(puttSg)) continue;
    const a = Number.isFinite(appSg) ? appSg : 0;
    const p = Number.isFinite(puttSg) ? puttSg : 0;
    const aOk = Number.isFinite(appSg);
    const pOk = Number.isFinite(puttSg);
    let combined;
    if (aOk && pOk) combined = appShare * a + puttShare * p;
    else if (aOk) combined = a;
    else combined = p;

    // Course-round confidence: more local rounds → trust more of the weight
    const conf = Math.min(1, (courseRnd + 0.35 * Math.min(tourRnd, 20)) / 8);
    let stpAdj = -weight * conf * combined;
    if (stpAdj > maxAbsStp) stpAdj = maxAbsStp;
    if (stpAdj < -maxAbsStp) stpAdj = -maxAbsStp;

    out.set(dg, {
      stpAdj: Math.round(stpAdj * 1000) / 1000,
      appSg: Math.round(a * 1000) / 1000,
      puttSg: Math.round(p * 1000) / 1000,
      combinedSg: Math.round(combined * 1000) / 1000,
      courseRounds: courseRnd,
      tourRounds: tourRnd,
      conf: Math.round(conf * 1000) / 1000,
    });
  }
  return out;
}

export function applyDistanceSgToScoreStp(stp, distAdj, source = "") {
  if (!distAdj || !Number.isFinite(distAdj.stpAdj) || distAdj.stpAdj === 0) {
    return { stp, source };
  }
  const next = Math.round((stp + distAdj.stpAdj) * 1000) / 1000;
  const tag = source ? `${source}+dist_sg` : "dist_sg";
  return { stp: next, source: tag };
}

/** Combine hole + distance adjustments into one STP update. */
export function applyGranularSgToScoreStp(stp, holeAdj, distAdj, source = "") {
  let cur = { stp, source };
  cur = applyHoleLike(cur.stp, holeAdj, cur.source, "hole_sg");
  cur = applyHoleLike(cur.stp, distAdj, cur.source, "dist_sg");
  return cur;
}

function applyHoleLike(stp, adj, source, tag) {
  if (!adj || !Number.isFinite(adj.stpAdj) || adj.stpAdj === 0) return { stp, source };
  const next = Math.round((stp + adj.stpAdj) * 1000) / 1000;
  return { stp: next, source: source ? `${source}+${tag}` : tag };
}

export function applyDistanceSgToBirdies(birdies, distAdj, frac = 0.08) {
  if (!distAdj || !Number.isFinite(distAdj.appSg)) return birdies;
  const b = num(birdies, NaN);
  if (!Number.isFinite(b)) return birdies;
  const w = Math.min(0.25, Math.max(0, frac));
  // Approach distance skill correlates more with birdies than putting distance.
  return Math.round((b + w * distAdj.appSg) * 100) / 100;
}
