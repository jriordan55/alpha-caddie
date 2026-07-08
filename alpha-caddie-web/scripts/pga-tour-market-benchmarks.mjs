/**
 * PGA Tour round-level means / SDs for Round projections "market rating" z-scores.
 * Used by fetch-datagolf (writes projections.json) and documented for app.js fallbacks.
 */
import { createReadStream, existsSync } from "fs";
import { join } from "path";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function pushSample(samples, v) {
  if (Number.isFinite(v)) samples.push(v);
}

function meanSd(samples, minN = 80) {
  const n = samples.length;
  if (n < minN) return { mean: NaN, sd: NaN, n };
  let s = 0;
  for (const x of samples) s += x;
  const mean = s / n;
  let v = 0;
  for (const x of samples) {
    const d = x - mean;
    v += d * d;
  }
  const sd = Math.sqrt(v / (n - 1));
  return { mean, sd: sd > 1e-6 ? sd : NaN, n };
}

/** Five-number summary + mean across per-course venue samples (for course-breakdown box plots). */
function quartilesFromSamples(samples, minN = 6) {
  const sorted = samples.filter(Number.isFinite).sort((a, b) => a - b);
  const n = sorted.length;
  if (n < minN) return null;
  const q = (p) => {
    const i = (n - 1) * p;
    const lo = Math.floor(i);
    const hi = Math.ceil(i);
    if (lo === hi) return sorted[lo];
    return sorted[lo] + (sorted[hi] - sorted[lo]) * (i - lo);
  };
  const mean = sorted.reduce((a, b) => a + b, 0) / n;
  return {
    min: sorted[0],
    q1: q(0.25),
    median: q(0.5),
    q3: q(0.75),
    max: sorted[n - 1],
    mean,
    n_courses: n,
  };
}

function girCount(row) {
  const raw = num(row.gir, NaN);
  if (!Number.isFinite(raw)) return NaN;
  if (raw > 0 && raw <= 1.0001) return Math.min(18, Math.max(0, Math.round(raw * 18)));
  return Math.min(18, Math.max(0, Math.round(raw)));
}

function fairwayCount(row) {
  const acc = num(row.driving_acc, NaN);
  if (!Number.isFinite(acc)) return NaN;
  const opp = fairwayOpportunitiesFromCoursePar(row.course_par);
  const p = acc > 0 && acc <= 1.0001 ? acc : acc / opp;
  return Math.min(opp, Math.max(0, Math.round(p * opp)));
}

/** Regulation driving holes from 18-hole par when per-hole pars are unavailable in CSV. */
export function fairwayOpportunitiesFromCoursePar(coursePar) {
  const cp = Math.round(num(coursePar, NaN));
  if (!Number.isFinite(cp) || cp < 67 || cp > 74) return 14;
  const par3 = Math.max(3, Math.min(6, 4 + (72 - cp)));
  return Math.max(10, Math.min(15, 18 - par3));
}

/** @returns {boolean} */
function yearInBenchmarkWindow(yr, minYear, maxYear) {
  if (!Number.isFinite(yr)) return false;
  if (Number.isFinite(minYear) && yr < minYear) return false;
  if (Number.isFinite(maxYear) && yr > maxYear) return false;
  return true;
}

/**
 * @param {string} modelRoot — repo root with data/historical_rounds_all.csv
 * @param {{ minYear?: number, maxYear?: number, recentYears?: number }} [opts]
 *   Default window: 2025–2026. Set `recentYears` (3–12) for a rolling window ending in the current calendar year instead.
 */
export async function loadPgaTourMarketBenchmarks(modelRoot, opts = {}) {
  const csvPath = join(modelRoot, "data", "historical_rounds_all.csv");
  const cy = new Date().getFullYear();
  let minYear = num(opts.minYear, NaN);
  let maxYear = num(opts.maxYear, NaN);
  if (!Number.isFinite(minYear) && !Number.isFinite(maxYear) && opts.recentYears != null) {
    const years = Math.max(3, Math.min(12, Math.round(num(opts.recentYears, 6) || 6)));
    minYear = cy - years;
    maxYear = cy;
  }
  if (!Number.isFinite(minYear) && !Number.isFinite(maxYear)) {
    minYear = 2025;
    maxYear = 2026;
  }
  if (!Number.isFinite(maxYear)) maxYear = cy;
  if (!Number.isFinite(minYear)) minYear = maxYear;

  const empty = {
    "Total score": { mean: NaN, sd: NaN, higherBetter: false, unit: "strokes" },
    Birdies: { mean: NaN, sd: NaN, higherBetter: true, unit: "count" },
    Pars: { mean: NaN, sd: NaN, higherBetter: true, unit: "count" },
    Bogeys: { mean: NaN, sd: NaN, higherBetter: false, unit: "count" },
    GIR: { mean: NaN, sd: NaN, higherBetter: true, unit: "rate" },
    "Fairways hit": { mean: NaN, sd: NaN, higherBetter: true, unit: "rate" },
    meta: { skipped: true, csv_path: csvPath, min_year: minYear, max_year: maxYear },
  };
  if (!existsSync(csvPath)) return empty;

  const score = [];
  const birdies = [];
  const pars = [];
  const bogeys = [];
  const girRates = [];
  const fwRates = [];

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
      if (String(row.tour || "").toLowerCase() !== "pga") return;
      const yr = parseInt(row.year, 10);
      if (!yearInBenchmarkWindow(yr, minYear, maxYear)) return;
      const rs = num(row.round_score, NaN);
      if (Number.isFinite(rs) && rs >= 55 && rs <= 95) pushSample(score, rs);
      const b = num(row.birdies, NaN);
      const p = num(row.pars, NaN);
      const bg = num(row.bogies, NaN);
      if (Number.isFinite(b) && b >= 0 && b <= 18) pushSample(birdies, b);
      if (Number.isFinite(p) && p >= 0 && p <= 18) pushSample(pars, p);
      if (Number.isFinite(bg) && bg >= 0 && bg <= 18) pushSample(bogeys, bg);
      const g = girCount(row);
      if (Number.isFinite(g)) pushSample(girRates, g / 18);
      const fw = fairwayCount(row);
      const fwOpp = fairwayOpportunitiesFromCoursePar(row.course_par);
      if (Number.isFinite(fw) && fwOpp > 0) pushSample(fwRates, fw / fwOpp);
    });
    parser.on("error", reject);
    parser.on("end", resolve);
  });

  const ms = meanSd(score);
  const mb = meanSd(birdies);
  const mp = meanSd(pars);
  const mbg = meanSd(bogeys);
  const mg = meanSd(girRates);
  const mf = meanSd(fwRates);

  return {
    "Total score": { mean: ms.mean, sd: ms.sd, higherBetter: false, unit: "strokes" },
    Birdies: { mean: mb.mean, sd: mb.sd, higherBetter: true, unit: "count" },
    Pars: { mean: mp.mean, sd: mp.sd, higherBetter: true, unit: "count" },
    Bogeys: { mean: mbg.mean, sd: mbg.sd, higherBetter: false, unit: "count" },
    GIR: { mean: mg.mean, sd: mg.sd, higherBetter: true, unit: "rate" },
    "Fairways hit": { mean: mf.mean, sd: mf.sd, higherBetter: true, unit: "rate" },
    meta: {
      skipped: false,
      csv_path: csvPath,
      min_year: minYear,
      max_year: maxYear,
      n: { score: ms.n, birdies: mb.n, pars: mp.n, bogeys: mbg.n, gir: mg.n, fairways: mf.n },
    },
  };
}

/** Min completed rounds at a venue before it counts toward cross-course averages. */
const COURSE_BENCHMARK_MIN_ROUNDS = 24;
const COURSE_BENCHMARK_MIN_VENUES = 6;

function emptyCourseAgg() {
  return {
    n: 0,
    sumScore: 0,
    sumStp: 0,
    sumBirdies: 0,
    sumPars: 0,
    sumBogeys: 0,
    sumGir: 0,
    sumFw: 0,
    sumFwOpp: 0,
    sumScramble: 0,
    nScramble: 0,
  };
}

function finalizeCourseAgg(agg) {
  const n = agg.n;
  if (n < 1) return null;
  const fwOpp = agg.sumFwOpp > 0 ? agg.sumFwOpp / n : NaN;
  return {
    score: agg.sumScore / n,
    scoreToPar: agg.sumStp / n,
    birdies: agg.sumBirdies / n,
    pars: agg.sumPars / n,
    bogeys: agg.sumBogeys / n,
    girRate: agg.sumGir > 0 ? agg.sumGir / (18 * n) : NaN,
    fwRate: Number.isFinite(fwOpp) && fwOpp > 0 ? agg.sumFw / agg.sumFwOpp : NaN,
    scrambleRate: agg.nScramble > 0 ? agg.sumScramble / agg.nScramble : NaN,
    n,
  };
}

/**
 * Mean / SD across PGA venues (one value per course) for Round projections "course rating".
 * Same markets as tour player benchmarks; units match venue averages in projection_course_basis.
 */
export async function loadPgaTourCourseBenchmarks(modelRoot, opts = {}) {
  const csvPath = join(modelRoot, "data", "historical_rounds_all.csv");
  const cy = new Date().getFullYear();
  let minYear = num(opts.minYear, NaN);
  let maxYear = num(opts.maxYear, NaN);
  if (!Number.isFinite(minYear) && !Number.isFinite(maxYear) && opts.recentYears != null) {
    const years = Math.max(4, Math.min(20, Math.round(num(opts.recentYears, 12) || 12)));
    minYear = cy - years;
    maxYear = cy;
  }
  if (!Number.isFinite(minYear) && !Number.isFinite(maxYear)) {
    minYear = cy - 12;
    maxYear = cy;
  }
  if (!Number.isFinite(maxYear)) maxYear = cy;
  if (!Number.isFinite(minYear)) minYear = maxYear;

  const empty = {
    "Total score": { mean: NaN, sd: NaN, higherBetter: false, unit: "strokes" },
    Birdies: { mean: NaN, sd: NaN, higherBetter: true, unit: "count" },
    Pars: { mean: NaN, sd: NaN, higherBetter: true, unit: "count" },
    Bogeys: { mean: NaN, sd: NaN, higherBetter: false, unit: "count" },
    GIR: { mean: NaN, sd: NaN, higherBetter: true, unit: "rate" },
    "Fairways hit": { mean: NaN, sd: NaN, higherBetter: true, unit: "rate" },
    Scrambling: { mean: NaN, sd: NaN, higherBetter: true, unit: "rate" },
    "Scoring vs Par": { mean: NaN, sd: NaN, higherBetter: false, unit: "strokes" },
    meta: { skipped: true, csv_path: csvPath, min_year: minYear, max_year: maxYear },
  };
  if (!existsSync(csvPath)) return empty;

  /** @type {Map<string, ReturnType<typeof emptyCourseAgg>>} */
  const byCourse = new Map();

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
      if (String(row.tour || "").toLowerCase() !== "pga") return;
      const yr = parseInt(row.year, 10);
      if (!yearInBenchmarkWindow(yr, minYear, maxYear)) return;
      const ck = normCourseNameKey(row.course_name || row.Course_Name || "");
      if (!ck) return;
      const rs = num(row.round_score, NaN);
      if (!Number.isFinite(rs) || rs < 55 || rs > 95) return;

      let agg = byCourse.get(ck);
      if (!agg) {
        agg = emptyCourseAgg();
        byCourse.set(ck, agg);
      }
      agg.n++;
      agg.sumScore += rs;
      const cp = num(row.course_par, NaN);
      if (Number.isFinite(cp)) agg.sumStp += rs - cp;
      const b = num(row.birdies, NaN);
      const p = num(row.pars, NaN);
      const bg = num(row.bogies, NaN);
      if (Number.isFinite(b) && b >= 0 && b <= 18) agg.sumBirdies += b;
      if (Number.isFinite(p) && p >= 0 && p <= 18) agg.sumPars += p;
      if (Number.isFinite(bg) && bg >= 0 && bg <= 18) agg.sumBogeys += bg;
      const g = girCount(row);
      if (Number.isFinite(g)) agg.sumGir += g;
      const fw = fairwayCount(row);
      const fwOpp = fairwayOpportunitiesFromCoursePar(row.course_par);
      if (Number.isFinite(fw) && fwOpp > 0) {
        agg.sumFw += fw;
        agg.sumFwOpp += fwOpp;
      }
      const scr = num(row.scrambling, NaN);
      if (Number.isFinite(scr) && scr >= 0 && scr <= 1.0001) {
        agg.sumScramble += scr;
        agg.nScramble++;
      }
    });
    parser.on("error", reject);
    parser.on("end", resolve);
  });

  const score = [];
  const birdies = [];
  const pars = [];
  const bogeys = [];
  const girRates = [];
  const fwRates = [];
  const scrambleRates = [];
  const scoreToPar = [];

  for (const agg of byCourse.values()) {
    if (agg.n < COURSE_BENCHMARK_MIN_ROUNDS) continue;
    const fin = finalizeCourseAgg(agg);
    if (!fin) continue;
    pushSample(score, fin.score);
    pushSample(birdies, fin.birdies);
    pushSample(pars, fin.pars);
    pushSample(bogeys, fin.bogeys);
    if (Number.isFinite(fin.girRate)) pushSample(girRates, fin.girRate);
    if (Number.isFinite(fin.fwRate)) pushSample(fwRates, fin.fwRate);
    if (Number.isFinite(fin.scrambleRate)) pushSample(scrambleRates, fin.scrambleRate);
    if (Number.isFinite(fin.scoreToPar)) pushSample(scoreToPar, fin.scoreToPar);
  }

  const ms = meanSd(score, COURSE_BENCHMARK_MIN_VENUES);
  const mb = meanSd(birdies, COURSE_BENCHMARK_MIN_VENUES);
  const mp = meanSd(pars, COURSE_BENCHMARK_MIN_VENUES);
  const mbg = meanSd(bogeys, COURSE_BENCHMARK_MIN_VENUES);
  const mg = meanSd(girRates, COURSE_BENCHMARK_MIN_VENUES);
  const mf = meanSd(fwRates, COURSE_BENCHMARK_MIN_VENUES);
  const msc = meanSd(scrambleRates, COURSE_BENCHMARK_MIN_VENUES);
  const mstp = meanSd(scoreToPar, COURSE_BENCHMARK_MIN_VENUES);

  const distScore = quartilesFromSamples(score, COURSE_BENCHMARK_MIN_VENUES);
  const distBirdies = quartilesFromSamples(birdies, COURSE_BENCHMARK_MIN_VENUES);
  const distPars = quartilesFromSamples(pars, COURSE_BENCHMARK_MIN_VENUES);
  const distBogeys = quartilesFromSamples(bogeys, COURSE_BENCHMARK_MIN_VENUES);
  const distGir = quartilesFromSamples(girRates, COURSE_BENCHMARK_MIN_VENUES);
  const distFw = quartilesFromSamples(fwRates, COURSE_BENCHMARK_MIN_VENUES);
  const distScramble = quartilesFromSamples(scrambleRates, COURSE_BENCHMARK_MIN_VENUES);
  const distStp = quartilesFromSamples(scoreToPar, COURSE_BENCHMARK_MIN_VENUES);

  return {
    "Total score": { mean: ms.mean, sd: ms.sd, higherBetter: false, unit: "strokes", distribution: distScore },
    Birdies: { mean: mb.mean, sd: mb.sd, higherBetter: true, unit: "count", distribution: distBirdies },
    Pars: { mean: mp.mean, sd: mp.sd, higherBetter: true, unit: "count", distribution: distPars },
    Bogeys: { mean: mbg.mean, sd: mbg.sd, higherBetter: false, unit: "count", distribution: distBogeys },
    GIR: { mean: mg.mean, sd: mg.sd, higherBetter: true, unit: "rate", distribution: distGir },
    "Fairways hit": { mean: mf.mean, sd: mf.sd, higherBetter: true, unit: "rate", distribution: distFw },
    Scrambling: { mean: msc.mean, sd: msc.sd, higherBetter: true, unit: "rate", distribution: distScramble },
    "Scoring vs Par": { mean: mstp.mean, sd: mstp.sd, higherBetter: false, unit: "strokes", distribution: distStp },
    meta: {
      skipped: false,
      csv_path: csvPath,
      min_year: minYear,
      max_year: maxYear,
      n_courses: {
        score: ms.n,
        birdies: mb.n,
        pars: mp.n,
        bogeys: mbg.n,
        gir: mg.n,
        fairways: mf.n,
        scrambling: msc.n,
        score_to_par: mstp.n,
      },
    },
  };
}

/** Rounded copy for projections.json */
export function serializePgaTourCourseBenchmarks(raw) {
  const keys = [
    "Total score",
    "Birdies",
    "Pars",
    "Bogeys",
    "GIR",
    "Fairways hit",
    "Scrambling",
    "Scoring vs Par",
  ];
  const out = {};
  for (const key of keys) {
    const b = raw[key];
    if (!b) continue;
    out[key] = {
      mean: Number.isFinite(b.mean) ? Math.round(b.mean * 1000) / 1000 : null,
      sd: Number.isFinite(b.sd) ? Math.round(b.sd * 1000) / 1000 : null,
      higherBetter: !!b.higherBetter,
      unit: b.unit || null,
    };
    if (b.distribution && typeof b.distribution === "object") {
      const d = b.distribution;
      out[key].distribution = {
        min: roundDist(d.min),
        q1: roundDist(d.q1),
        median: roundDist(d.median),
        q3: roundDist(d.q3),
        max: roundDist(d.max),
        mean: roundDist(d.mean),
        n_courses: Math.round(num(d.n_courses, 0)) || 0,
      };
    }
  }
  out.meta = raw.meta || {};
  return out;
}

function roundDist(v) {
  return Number.isFinite(v) ? Math.round(v * 10000) / 10000 : null;
}

/** Rounded copy for projections.json */
export function serializePgaTourMarketBenchmarks(raw) {
  const out = {};
  for (const key of ["Total score", "Birdies", "Pars", "Bogeys", "GIR", "Fairways hit"]) {
    const b = raw[key];
    if (!b) continue;
    out[key] = {
      mean: Number.isFinite(b.mean) ? Math.round(b.mean * 1000) / 1000 : null,
      sd: Number.isFinite(b.sd) ? Math.round(b.sd * 1000) / 1000 : null,
      higherBetter: !!b.higherBetter,
      unit: b.unit || null,
    };
  }
  out.meta = raw.meta || {};
  return out;
}
