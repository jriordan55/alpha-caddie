#!/usr/bin/env node
/**
 * Fit hierarchical weather / interaction priors → data/hierarchical_round_mu_fit.json
 *
 * Round-forward safe sketch: residualize historical scores vs DG baseline (no weather),
 * then OLS on weather design. Interaction betas stay at theory defaults unless
 * GOLF_HIER_FIT_INTERACTIONS=1 (needs more samples).
 *
 *   npm run fit:hierarchical-mu
 */
import { createReadStream, existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { DEFAULT_HIER_FIT, weatherFeaturesFromSnapshot } from "./hierarchical-round-mu.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const HIST = join(REPO, "data", "historical_rounds_all.csv");
const WEATHER = join(WEB, "data", "historical_round_weather.json");
const OUT = join(WEB, "data", "hierarchical_round_mu_fit.json");

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

async function loadHist() {
  const rows = [];
  await new Promise((resolveP, reject) => {
    createReadStream(HIST)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", resolveP)
      .on("error", reject);
  });
  return rows;
}

function ols(y, X) {
  // X: n×k, y: n — return beta length k via normal equations with ridge
  const n = y.length;
  const k = X[0].length;
  const lambda = 1e-3;
  const xtx = Array.from({ length: k }, () => Array(k).fill(0));
  const xty = Array(k).fill(0);
  for (let i = 0; i < n; i++) {
    for (let a = 0; a < k; a++) {
      xty[a] += X[i][a] * y[i];
      for (let b = 0; b < k; b++) xtx[a][b] += X[i][a] * X[i][b];
    }
  }
  for (let a = 0; a < k; a++) xtx[a][a] += lambda;
  // Gauss-Jordan
  const m = xtx.map((row, i) => [...row, xty[i]]);
  for (let col = 0; col < k; col++) {
    let piv = col;
    for (let r = col + 1; r < k; r++) if (Math.abs(m[r][col]) > Math.abs(m[piv][col])) piv = r;
    [m[col], m[piv]] = [m[piv], m[col]];
    const div = m[col][col] || 1e-12;
    for (let c = col; c <= k; c++) m[col][c] /= div;
    for (let r = 0; r < k; r++) {
      if (r === col) continue;
      const f = m[r][col];
      for (let c = col; c <= k; c++) m[r][c] -= f * m[col][c];
    }
  }
  return m.map((row) => row[k]);
}

async function main() {
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);
  const hist = await loadHist();
  const wx = existsSync(WEATHER) ? JSON.parse(readFileSync(WEATHER, "utf8")) : { byKey: {} };
  const byKey = wx.byKey || {};

  // Course-round field means as crude baseline (no leakage across future: use prior years only in a full WF).
  // For a static fit artifact we use leave-one-year-out style aggregates.
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseYear = new Map();
  for (const r of hist) {
    const score = num(r.round_score, NaN);
    const year = Math.round(num(r.year, NaN));
    const ck = normCourseNameKey(r.course_name);
    if (!Number.isFinite(score) || !Number.isFinite(year) || !ck) continue;
    if (year < 2018) continue;
    const k = `${ck}|${year}`;
    const o = courseYear.get(k) || { sum: 0, n: 0 };
    o.sum += score;
    o.n++;
    courseYear.set(k, o);
  }

  const y = [];
  const X = [];
  let used = 0;
  for (const r of hist) {
    const score = num(r.round_score, NaN);
    const year = Math.round(num(r.year, NaN));
    const rnd = Math.round(num(r.round_num, NaN));
    const eid = String(r.event_id || "").trim();
    const ck = normCourseNameKey(r.course_name);
    if (!Number.isFinite(score) || year < 2019 || !Number.isFinite(rnd)) continue;
    const wk = `${eid}|${year}|${rnd}`;
    const snap = byKey[wk];
    if (!snap) continue;
    // Baseline: other-year same-course mean
    let baseSum = 0;
    let baseN = 0;
    for (const [k, o] of courseYear) {
      if (!k.startsWith(`${ck}|`)) continue;
      const yk = Number(k.split("|")[1]);
      if (yk === year) continue;
      baseSum += o.sum;
      baseN += o.n;
    }
    if (baseN < 40) continue;
    const baseline = baseSum / baseN;
    const resid = score - baseline;
    const f = weatherFeaturesFromSnapshot(snap, "");
    y.push(resid);
    X.push([
      1,
      f.wind_excess,
      f.rain,
      f.storm,
      f.temp_dev,
      f.humidity_dev,
      f.priorPrecipMm > 0 ? Math.min(f.priorPrecipMm, 20) : 0,
    ]);
    used++;
  }

  const fit = {
    ...DEFAULT_HIER_FIT,
    fitted_at: new Date().toISOString(),
    n_weather_rows: used,
  };

  if (used >= 200) {
    const beta = ols(y, X);
    // Map to weather config (STP). prior precip coefficient → scale softener strength.
    fit.weather = {
      ...DEFAULT_HIER_FIT.weather,
      // Locked design rule — never OLS-override wind ladder.
      wind_per_mph_over_5: 0.1,
      rain_in_play: Math.max(0, Math.min(0.35, beta[2])),
      storm_in_play: Math.max(0, Math.min(0.5, beta[3] + beta[2])),
      temp_per_f_over_72: Math.max(-0.02, Math.min(0.05, beta[4])),
      humidity_per_pct_over_55: Math.max(-0.01, Math.min(0.02, beta[5])),
      prior_precip_per_mm: Math.max(-0.12, Math.min(0, beta[6])),
      intercept_resid: beta[0],
      soak_mute_wind: true,
    };
    fit.weather_ols = { beta, n: used };
    console.log(`[hier-fit] OLS weather on ${used} rounds`, fit.weather);
  } else {
    console.log(`[hier-fit] Only ${used} weather-joined rounds — keeping default weather betas`);
  }

  mkdirSync(dirname(OUT), { recursive: true });
  writeFileSync(OUT, `${JSON.stringify(fit, null, 2)}\n`);
  console.log(`[hier-fit] wrote ${OUT}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
