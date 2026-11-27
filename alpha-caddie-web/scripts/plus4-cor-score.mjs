/**
 * Plus4-style COR (corollary-course) scoring.
 *
 * Residual r = sg_total − rolling baseline SG (time-adjusted). SG is already
 * field-relative. Course–course COR = Pearson of player mean residuals.
 * Player μ lift = shrunk mean residual on corollary courses for this venue.
 *
 * Env:
 *   GOLF_PROJECTION_COURSE_FIT=plus4_cor|legacy  (default plus4_cor)
 *   GOLF_COR_LOOKBACK_YEARS=25
 *   GOLF_COR_MIN_OVERLAP=12
 *   GOLF_COR_TOP_K=10
 *   GOLF_COR_MIN_ABS=0.12
 *   GOLF_COR_SHRINK_K=8
 *   GOLF_COR_MAX_MU_DELTA=0.85
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { resolveBetspertsCourseFacts } from "./betsperts-course-context.mjs";

const WEB_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const DEFAULT_MATRIX_PATH = join(WEB_ROOT, "data", "course_cor_matrix.json");
const DEFAULT_HISTORY_PATH = join(WEB_ROOT, "player_round_history.json");

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function envNum(name, fallback) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return fallback;
  const n = Number(raw);
  return Number.isFinite(n) ? n : fallback;
}

/** Course-fit recipe for seed μ. Default: plus4_cor. */
export function projectionCourseFitRecipe() {
  const raw = String(process.env.GOLF_PROJECTION_COURSE_FIT || "plus4_cor")
    .trim()
    .toLowerCase();
  if (raw === "legacy" || raw === "tailor" || raw === "skill_tailor") return "legacy";
  return "plus4_cor";
}

export function plus4CorEnabled() {
  return projectionCourseFitRecipe() === "plus4_cor";
}

function parseRoundTime(r) {
  const raw = String(r?.play_date || r?.event_completed || r?.date || "").trim();
  if (!raw) return NaN;
  // MM/DD/YYYY
  const m = raw.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (m) return Date.UTC(Number(m[3]), Number(m[1]) - 1, Number(m[2]));
  const t = Date.parse(raw);
  return Number.isFinite(t) ? t : NaN;
}

function courseKeyFromRound(r) {
  return normCourseNameKey(r?.course_name || r?.course || "");
}

/**
 * Exp-decay rolling baseline of sg_total using rounds strictly before each round.
 * Returns array parallel to input (chronological ascending).
 */
export function timeAdjustedResiduals(roundsAsc, { decay = 0.92, minPrior = 4 } = {}) {
  const out = [];
  let wSum = 0;
  let vSum = 0;
  const priors = [];
  for (let i = 0; i < roundsAsc.length; i++) {
    const sg = num(roundsAsc[i]?.sg_total, NaN);
    let baseline = NaN;
    if (priors.length >= minPrior && wSum > 0) {
      baseline = vSum / wSum;
    } else if (priors.length >= 2 && wSum > 0) {
      baseline = vSum / wSum;
    }
    const residual = Number.isFinite(sg) && Number.isFinite(baseline) ? sg - baseline : NaN;
    out.push({
      ...roundsAsc[i],
      _baseline_sg: baseline,
      _residual: residual,
    });
    if (Number.isFinite(sg)) {
      // Reweight prior mass by decay, then add current for next rounds.
      wSum *= decay;
      vSum *= decay;
      wSum += 1;
      vSum += sg;
      priors.push(sg);
    }
  }
  return out;
}

/**
 * Build player×course mean residual maps from history JSON.
 * @returns {{
 *   playerCourse: Map<string, { sum: number, n: number, puttSum: number, puttN: number, argSum: number, argN: number, appSum: number, appN: number }>,
 *   coursePlayers: Map<string, Map<number, number>>,
 *   playerRounds: Map<number, object[]>,
 *   courses: string[],
 *   meta: object
 * }}
 */
export function buildPlayerCourseResiduals(historyJson, opts = {}) {
  const lookbackYears = Math.round(envNum("GOLF_COR_LOOKBACK_YEARS", opts.lookbackYears ?? 25));
  const cutoffMs = Number.isFinite(opts.cutoffMs)
    ? opts.cutoffMs
    : Date.now() - lookbackYears * 365.25 * 24 * 3600 * 1000;
  const byDg = historyJson?.byDgId || {};
  const playerCourse = new Map();
  const coursePlayers = new Map();
  const playerRounds = new Map();
  let nRoundsUsed = 0;

  for (const [dgStr, bucket] of Object.entries(byDg)) {
    const dg = Math.round(num(dgStr, NaN));
    if (!Number.isFinite(dg)) continue;
    const raw = Array.isArray(bucket?.rounds) ? bucket.rounds : [];
    const dated = [];
    for (const r of raw) {
      const t = parseRoundTime(r);
      if (!Number.isFinite(t) || t < cutoffMs) continue;
      if (Number.isFinite(opts.asofMs) && t > opts.asofMs) continue;
      const ck = courseKeyFromRound(r);
      if (!ck) continue;
      if (!Number.isFinite(num(r.sg_total, NaN))) continue;
      dated.push({ ...r, _t: t, _ck: ck });
    }
    if (dated.length < 6) continue;
    dated.sort((a, b) => a._t - b._t);
    const withRes = timeAdjustedResiduals(dated);
    playerRounds.set(dg, withRes.filter((r) => Number.isFinite(r._residual)));
    for (const r of withRes) {
      if (!Number.isFinite(r._residual)) continue;
      nRoundsUsed++;
      const key = `${dg}|${r._ck}`;
      let agg = playerCourse.get(key);
      if (!agg) {
        agg = {
          dg,
          course: r._ck,
          sum: 0,
          n: 0,
          puttSum: 0,
          puttN: 0,
          argSum: 0,
          argN: 0,
          appSum: 0,
          appN: 0,
        };
        playerCourse.set(key, agg);
      }
      agg.sum += r._residual;
      agg.n += 1;
      const putt = num(r.sg_putt, NaN);
      if (Number.isFinite(putt)) {
        agg.puttSum += putt;
        agg.puttN += 1;
      }
      const arg = num(r.sg_arg, NaN);
      if (Number.isFinite(arg)) {
        agg.argSum += arg;
        agg.argN += 1;
      }
      const app = num(r.sg_app, NaN);
      if (Number.isFinite(app)) {
        agg.appSum += app;
        agg.appN += 1;
      }
      let cp = coursePlayers.get(r._ck);
      if (!cp) {
        cp = new Map();
        coursePlayers.set(r._ck, cp);
      }
      // store mean residual lazily after loop
      cp.set(dg, key);
    }
  }

  // Materialize course → player → mean residual
  const coursePlayerMean = new Map();
  for (const [ck, dgToKey] of coursePlayers.entries()) {
    const m = new Map();
    for (const [dg, key] of dgToKey.entries()) {
      const agg = playerCourse.get(key);
      if (!agg || agg.n < 1) continue;
      m.set(dg, agg.sum / agg.n);
    }
    if (m.size) coursePlayerMean.set(ck, m);
  }

  return {
    playerCourse,
    coursePlayerMean,
    playerRounds,
    courses: [...coursePlayerMean.keys()].sort(),
    meta: {
      lookback_years: lookbackYears,
      n_rounds_used: nRoundsUsed,
      n_players: playerRounds.size,
      n_courses: coursePlayerMean.size,
    },
  };
}

function pearsonMaps(a, b) {
  const xs = [];
  const ys = [];
  for (const [dg, xv] of a.entries()) {
    const yv = b.get(dg);
    if (!Number.isFinite(xv) || !Number.isFinite(yv)) continue;
    xs.push(xv);
    ys.push(yv);
  }
  const n = xs.length;
  if (n < 3) return { cor: NaN, n: 0 };
  let sx = 0;
  let sy = 0;
  for (let i = 0; i < n; i++) {
    sx += xs[i];
    sy += ys[i];
  }
  const mx = sx / n;
  const my = sy / n;
  let sxx = 0;
  let syy = 0;
  let sxy = 0;
  for (let i = 0; i < n; i++) {
    const dx = xs[i] - mx;
    const dy = ys[i] - my;
    sxx += dx * dx;
    syy += dy * dy;
    sxy += dx * dy;
  }
  if (sxx <= 1e-12 || syy <= 1e-12) return { cor: NaN, n };
  return { cor: sxy / Math.sqrt(sxx * syy), n };
}

/**
 * Build sparse top-K COR neighbors for every course.
 */
export function buildCourseCorMatrixFromResiduals(residualBundle, opts = {}) {
  const minOverlap = Math.round(envNum("GOLF_COR_MIN_OVERLAP", opts.minOverlap ?? 12));
  const topK = Math.round(envNum("GOLF_COR_TOP_K", opts.topK ?? 10));
  const minAbs = envNum("GOLF_COR_MIN_ABS", opts.minAbs ?? 0.12);
  const courses = residualBundle.courses || [];
  const cpm = residualBundle.coursePlayerMean;
  const neighbors = {};

  for (let i = 0; i < courses.length; i++) {
    const a = courses[i];
    const ma = cpm.get(a);
    if (!ma || ma.size < minOverlap) continue;
    const scored = [];
    for (let j = 0; j < courses.length; j++) {
      if (i === j) continue;
      const b = courses[j];
      const mb = cpm.get(b);
      if (!mb || mb.size < minOverlap) continue;
      const { cor, n } = pearsonMaps(ma, mb);
      if (!Number.isFinite(cor) || n < minOverlap || Math.abs(cor) < minAbs) continue;
      scored.push({ course: b, cor: Math.round(cor * 1000) / 1000, n });
    }
    scored.sort((x, y) => Math.abs(y.cor) - Math.abs(x.cor) || y.n - x.n);
    neighbors[a] = scored.slice(0, topK);
  }

  return {
    generated_at: new Date().toISOString(),
    method: "plus4_cor_player_residual_pearson",
    params: { min_overlap: minOverlap, top_k: topK, min_abs: minAbs },
    meta: residualBundle.meta,
    neighbors,
  };
}

export function loadCourseCorMatrix(path = DEFAULT_MATRIX_PATH) {
  if (!path || !existsSync(path)) return null;
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return null;
  }
}

export function saveCourseCorMatrix(matrix, path = DEFAULT_MATRIX_PATH) {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, JSON.stringify(matrix, null, 2));
  return path;
}

/**
 * Build (or load) COR matrix. Rebuilds when missing or force=true.
 */
export function ensureCourseCorMatrix(opts = {}) {
  const path = opts.path || DEFAULT_MATRIX_PATH;
  const historyPath = opts.historyPath || DEFAULT_HISTORY_PATH;
  if (!opts.force) {
    const existing = loadCourseCorMatrix(path);
    if (existing?.neighbors) return existing;
  }
  if (!existsSync(historyPath)) {
    return {
      generated_at: new Date().toISOString(),
      method: "plus4_cor_empty",
      neighbors: {},
      meta: { error: "missing_history" },
    };
  }
  const hist = JSON.parse(readFileSync(historyPath, "utf8"));
  const residuals = buildPlayerCourseResiduals(hist, opts);
  const matrix = buildCourseCorMatrixFromResiduals(residuals, opts);
  saveCourseCorMatrix(matrix, path);
  return matrix;
}

/**
 * Corollary courses for a venue key.
 */
export function corollaryCoursesForVenue(venueKey, matrix, opts = {}) {
  const ck = normCourseNameKey(venueKey);
  const topK = Math.round(envNum("GOLF_COR_TOP_K", opts.topK ?? 10));
  const minAbs = envNum("GOLF_COR_MIN_ABS", opts.minAbs ?? 0.12);
  const fromMatrix = (matrix?.neighbors?.[ck] || [])
    .filter((x) => Number.isFinite(x.cor) && Math.abs(x.cor) >= minAbs)
    .slice(0, topK)
    .map((x) => ({
      key: x.course,
      cor: x.cor,
      n: x.n,
      source: "residual_cor",
      weight: Math.abs(x.cor),
    }));

  // Betsperts CorrelatedCourse list (Course Facts) — blend / seed when residual COR sparse.
  const bp = resolveBetspertsCourseFacts(ck);
  if (bp?.correlated?.length) {
    for (const c of bp.correlated.slice(0, Math.max(8, topK))) {
      const key = c.key || normCourseNameKey(c.name || "");
      if (!key || key === ck) continue;
      const existing = fromMatrix.find((x) => x.key === key);
      if (existing) {
        existing.weight = Math.max(num(existing.weight, 0), 0.55);
        if (!String(existing.source || "").includes("betsperts")) {
          existing.source = `${existing.source}+betsperts`;
        }
      } else {
        fromMatrix.push({
          key,
          cor: 0.42,
          n: 0,
          source: "betsperts_correlated",
          weight: 0.5,
        });
      }
    }
  }

  // Weak feature-table prior when COR sparse.
  if (fromMatrix.length < 4) {
    for (const s of featureSimilarCourses(ck, 6)) {
      if (fromMatrix.some((x) => x.key === s.key)) continue;
      fromMatrix.push({
        key: s.key,
        cor: Math.round(num(s.sim, 0) * 1000) / 1000,
        n: 0,
        source: "course_table_sim",
        weight: 0.35 * num(s.sim, 0),
      });
    }
  }
  // Prefer residual_cor, then betsperts, then table sim.
  const rank = (s) => {
    const src = String(s.source || "");
    if (src.startsWith("residual_cor")) return 0;
    if (src.includes("betsperts")) return 1;
    return 2;
  };
  fromMatrix.sort((a, b) => rank(a) - rank(b) || num(b.weight, 0) - num(a.weight, 0));
  return fromMatrix.slice(0, topK);
}

/** Lightweight course-table neighbors (avoids importing projection-unified-factors). */
function featureSimilarCourses(venueKey, topN = 6) {
  const ck = normCourseNameKey(venueKey);
  const paths = [join(WEB_ROOT, "course-table.json"), join(WEB_ROOT, "data", "course-table.json")];
  let ct = null;
  for (const p of paths) {
    if (!existsSync(p)) continue;
    try {
      ct = JSON.parse(readFileSync(p, "utf8"));
      break;
    } catch {
      /* try next */
    }
  }
  if (!ct?.rows?.length || !ck) return [];
  const keys = ["adj_driving_accuracy", "ott_sg", "app_sg", "arg_sg", "putt_sg", "adj_score_to_par", "yardage"];
  const vec = (row) => {
    if (!row || typeof row !== "object") return null;
    const out = [];
    for (const k of keys) {
      const v = num(row[k], NaN);
      if (!Number.isFinite(v)) return null;
      out.push(v);
    }
    return out;
  };
  let venueRow = null;
  for (const row of ct.rows) {
    const rk = normCourseNameKey(row.course ?? row.course_name ?? "");
    if (rk === ck) {
      venueRow = row;
      break;
    }
  }
  const ref = vec(venueRow);
  if (!ref) return [];
  const scored = [];
  for (const row of ct.rows) {
    const rk = normCourseNameKey(row.course ?? row.course_name ?? "");
    if (!rk || rk === ck) continue;
    const v = vec(row);
    if (!v) continue;
    const d = Math.hypot(...v.map((x, j) => x - ref[j]));
    scored.push({ key: rk, sim: 1 / (1 + d), dist: d });
  }
  scored.sort((a, b) => b.sim - a.sim);
  return scored.slice(0, topN);
}

function playerMeanOnCourses(dg, courseKeys, playerCourse, sgField = "residual") {
  let sum = 0;
  let w = 0;
  let n = 0;
  for (const ck of courseKeys) {
    const agg = playerCourse.get(`${dg}|${ck}`);
    if (!agg || agg.n < 1) continue;
    let val = NaN;
    if (sgField === "residual") val = agg.sum / agg.n;
    else if (sgField === "putt" && agg.puttN) val = agg.puttSum / agg.puttN;
    else if (sgField === "arg" && agg.argN) val = agg.argSum / agg.argN;
    else if (sgField === "app" && agg.appN) val = agg.appSum / agg.appN;
    if (!Number.isFinite(val)) continue;
    const ww = Math.sqrt(agg.n);
    sum += ww * val;
    w += ww;
    n += agg.n;
  }
  if (w <= 0) return { mean: NaN, n: 0 };
  return { mean: sum / w, n };
}

/**
 * Apply Plus4 COR μ lifts (+ optional extras) to projection rows.
 * Mutates rows in place; returns summary meta.
 */
export function applyPlus4CorToPlayers(players, opts = {}) {
  const rows = Array.isArray(players) ? players : [];
  if (!rows.length) return { applied: 0, cor_courses: [], method: "plus4_cor" };

  const venueKey = normCourseNameKey(opts.venueKey || opts.courseUsed || "");
  const historyPath = opts.historyPath || DEFAULT_HISTORY_PATH;
  const matrix = opts.matrix || ensureCourseCorMatrix({ historyPath, force: !!opts.forceRebuild });
  const corCourses = corollaryCoursesForVenue(venueKey, matrix, opts);
  const corKeys = corCourses.map((c) => c.key);
  const shrinkK = envNum("GOLF_COR_SHRINK_K", 8);
  const maxDelta = envNum("GOLF_COR_MAX_MU_DELTA", 0.85);

  let residualBundle = opts.residualBundle || null;
  if (!residualBundle && existsSync(historyPath)) {
    const hist = JSON.parse(readFileSync(historyPath, "utf8"));
    residualBundle = buildPlayerCourseResiduals(hist, {
      asofMs: opts.asofMs,
      lookbackYears: opts.lookbackYears,
    });
  }
  const playerCourse = residualBundle?.playerCourse || new Map();
  const extrasFn = typeof opts.applyExtras === "function" ? opts.applyExtras : null;

  let applied = 0;
  const sample = [];
  for (const row of rows) {
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const mu0 = num(row.mu_sg, 0);
    const { mean, n } = playerMeanOnCourses(dg, corKeys, playerCourse, "residual");
    let corDelta = 0;
    if (Number.isFinite(mean) && n > 0) {
      const shrink = n / (n + shrinkK);
      corDelta = clamp(shrink * mean, -maxDelta, maxDelta);
    }

    let extraStroke = 0;
    let extraDetail = null;
    if (extrasFn) {
      extraDetail = extrasFn({
        row,
        dg,
        venueKey,
        corCourses,
        playerCourse,
        residualBundle,
        mu0,
        corDelta,
      });
      extraStroke = num(extraDetail?.totalStroke, 0);
    }

    // COR residual is in SG units (positive = better). Extra strokes: positive = harder (worse score).
    const muNext = clamp(mu0 + corDelta - extraStroke, -4, 4);
    row.mu_sg = Math.round(muNext * 1000) / 1000;
    row.implied_mu_sg = row.mu_sg;
    row.plus4_cor = {
      cor_delta: Math.round(corDelta * 1000) / 1000,
      cor_n: n,
      extra_stroke: Math.round(extraStroke * 1000) / 1000,
      extras: extraDetail || undefined,
    };
    if (Number.isFinite(extraDetail?.round_sd) && Number.isFinite(num(row.round_sd, NaN))) {
      row.round_sd = Math.round(extraDetail.round_sd * 100) / 100;
    } else if (Number.isFinite(extraDetail?.round_sd)) {
      row.round_sd = Math.round(extraDetail.round_sd * 100) / 100;
    }
    applied++;
    if (sample.length < 5 && Math.abs(corDelta) > 0.05) {
      sample.push({
        player: row.player_name,
        cor_delta: row.plus4_cor.cor_delta,
        n,
      });
    }
  }

  return {
    applied,
    method: "plus4_cor",
    venue: venueKey,
    cor_courses: corCourses.map((c) => ({
      course: c.key,
      cor: c.cor,
      n: c.n,
      source: c.source,
    })),
    matrix_generated_at: matrix?.generated_at || null,
    sample,
  };
}

/** CLI: rebuild data/course_cor_matrix.json */
export async function mainBuildCourseCorMatrix() {
  const force = process.argv.includes("--force");
  console.log(`[plus4-cor] building course COR matrix${force ? " (force)" : ""}…`);
  const t0 = Date.now();
  const matrix = ensureCourseCorMatrix({ force: true });
  const nCourses = Object.keys(matrix.neighbors || {}).length;
  let nEdges = 0;
  for (const arr of Object.values(matrix.neighbors || {})) nEdges += arr.length;
  console.log(
    `[plus4-cor] wrote ${DEFAULT_MATRIX_PATH} · courses=${nCourses} edges=${nEdges} · ${matrix.meta?.n_rounds_used || 0} rounds · ${((Date.now() - t0) / 1000).toFixed(1)}s`,
  );
}
