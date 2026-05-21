/**
 * PGA Tour round-level means / SDs for Round projections "market rating" z-scores.
 * Used by fetch-datagolf (writes projections.json) and documented for app.js fallbacks.
 */
import { createReadStream, existsSync } from "fs";
import { join } from "path";
import { parse } from "csv-parse";

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function pushSample(samples, v) {
  if (Number.isFinite(v)) samples.push(v);
}

function meanSd(samples) {
  const n = samples.length;
  if (n < 80) return { mean: NaN, sd: NaN, n };
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

function girCount(row) {
  const raw = num(row.gir, NaN);
  if (!Number.isFinite(raw)) return NaN;
  if (raw > 0 && raw <= 1.0001) return Math.min(18, Math.max(0, Math.round(raw * 18)));
  return Math.min(18, Math.max(0, Math.round(raw)));
}

function fairwayCount(row) {
  const acc = num(row.driving_acc, NaN);
  if (!Number.isFinite(acc)) return NaN;
  const p = acc > 0 && acc <= 1.0001 ? acc : acc / 14;
  return Math.min(14, Math.max(0, Math.round(p * 14)));
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
    "Total score": { mean: NaN, sd: NaN, higherBetter: false, holes: 18 },
    Birdies: { mean: NaN, sd: NaN, higherBetter: true, holes: 18 },
    Pars: { mean: NaN, sd: NaN, higherBetter: true, holes: 18 },
    Bogeys: { mean: NaN, sd: NaN, higherBetter: false, holes: 18 },
    GIR: { mean: NaN, sd: NaN, higherBetter: true, holes: 18 },
    "Fairways hit": { mean: NaN, sd: NaN, higherBetter: true, holes: 14 },
    meta: { skipped: true, csv_path: csvPath, min_year: minYear, max_year: maxYear },
  };
  if (!existsSync(csvPath)) return empty;

  const score = [];
  const birdies = [];
  const pars = [];
  const bogeys = [];
  const gir = [];
  const fw = [];

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
      pushSample(gir, girCount(row));
      pushSample(fw, fairwayCount(row));
    });
    parser.on("error", reject);
    parser.on("end", resolve);
  });

  const ms = meanSd(score);
  const mb = meanSd(birdies);
  const mp = meanSd(pars);
  const mbg = meanSd(bogeys);
  const mg = meanSd(gir);
  const mf = meanSd(fw);

  return {
    "Total score": { mean: ms.mean, sd: ms.sd, higherBetter: false, holes: 18 },
    Birdies: { mean: mb.mean, sd: mb.sd, higherBetter: true, holes: 18 },
    Pars: { mean: mp.mean, sd: mp.sd, higherBetter: true, holes: 18 },
    Bogeys: { mean: mbg.mean, sd: mbg.sd, higherBetter: false, holes: 18 },
    GIR: { mean: mg.mean, sd: mg.sd, higherBetter: true, holes: 18 },
    "Fairways hit": { mean: mf.mean, sd: mf.sd, higherBetter: true, holes: 14 },
    meta: {
      skipped: false,
      csv_path: csvPath,
      min_year: minYear,
      max_year: maxYear,
      n: { score: ms.n, birdies: mb.n, pars: mp.n, bogeys: mbg.n, gir: mg.n, fairways: mf.n },
    },
  };
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
      holes: Math.round(num(b.holes, 18)) || 18,
    };
  }
  out.meta = raw.meta || {};
  return out;
}
