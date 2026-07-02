/**
 * Course-tailored projections: venue SG importance, recent form (8–12 rounds), birdie-heavy venues.
 * Shared by fetch-datagolf, walk-forward backtest, and live pricing.
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import { courseRequirementSgWeights } from "./course-adaptive-pricing.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";
import { teeWaveStrokeShift } from "./projection-unified-factors.mjs";

export const SG_KEYS = ["sg_ott", "sg_app", "sg_arg", "sg_putt"];
export const RECENT_FORM_MIN = 8;
export const RECENT_FORM_MAX = 12;
export const STABLE_BASELINE_DECAY = 0.92;
const TOUR_AVG_BIRDIES = 4.2;

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function pearson(xs, ys) {
  const n = Math.min(xs.length, ys.length);
  if (n < 12) return NaN;
  let sx = 0;
  let sy = 0;
  let sxx = 0;
  let syy = 0;
  let sxy = 0;
  let c = 0;
  for (let i = 0; i < n; i++) {
    const x = num(xs[i], NaN);
    const y = num(ys[i], NaN);
    if (!Number.isFinite(x) || !Number.isFinite(y)) continue;
    sx += x;
    sy += y;
    sxx += x * x;
    syy += y * y;
    sxy += x * y;
    c++;
  }
  if (c < 12) return NaN;
  const mx = sx / c;
  const my = sy / c;
  const vx = sxx / c - mx * mx;
  const vy = syy / c - my * my;
  if (vx <= 1e-10 || vy <= 1e-10) return NaN;
  return (sxy / c - mx * my) / Math.sqrt(vx * vy);
}

function roundStp(row) {
  const stp = num(row.stp, NaN);
  if (Number.isFinite(stp)) return stp;
  const rs = num(row.round_score, NaN);
  const cp = num(row.course_par, NaN);
  if (Number.isFinite(rs) && Number.isFinite(cp)) return rs - cp;
  const sg = num(row.sg_total, NaN);
  if (Number.isFinite(sg)) return -sg;
  return NaN;
}

/** Empirical SG category weights at a venue (lower score-to-par = success). */
export function fitVenueSgImportanceFromRows(rows) {
  if (!Array.isArray(rows) || rows.length < 40) return null;
  const ys = [];
  const xs = { sg_ott: [], sg_app: [], sg_arg: [], sg_putt: [] };
  for (const row of rows) {
    const stp = roundStp(row);
    if (!Number.isFinite(stp)) continue;
    const feat = {};
    let ok = true;
    for (const k of SG_KEYS) {
      const v = num(row[k], NaN);
      if (!Number.isFinite(v)) {
        ok = false;
        break;
      }
      feat[k] = v;
    }
    if (!ok) continue;
    ys.push(stp);
    for (const k of SG_KEYS) xs[k].push(feat[k]);
  }
  if (ys.length < 40) return null;

  const weights = {};
  let sum = 0;
  for (const k of SG_KEYS) {
    const corr = pearson(xs[k], ys.map((y) => -y));
    const w = Number.isFinite(corr) ? Math.max(0, corr) : 0;
    weights[k] = w;
    sum += w;
  }
  if (sum < 0.08) return null;
  for (const k of SG_KEYS) weights[k] /= sum;
  return { weights, n: ys.length, source: "venue_history" };
}

export function mergeSgImportance(venueFit, ctRow) {
  const tableW = courseRequirementSgWeights(ctRow);
  if (!venueFit?.weights) {
    return { weights: tableW, n: 0, source: "course_table", dominant: dominantSgKey(tableW) };
  }
  const out = {};
  let sum = 0;
  for (const k of SG_KEYS) {
    out[k] = 0.62 * venueFit.weights[k] + 0.38 * tableW[k];
    sum += out[k];
  }
  for (const k of SG_KEYS) out[k] /= sum;
  return {
    weights: out,
    n: venueFit.n,
    source: "venue_blended",
    dominant: dominantSgKey(out),
  };
}

export function dominantSgKey(weights) {
  if (!weights) return "sg_app";
  return SG_KEYS.reduce((best, k) => (num(weights[k], 0) > num(weights[best], 0) ? k : best), "sg_app");
}

export function stableBaselineMean(rounds, key, startIdx = RECENT_FORM_MIN, decay = STABLE_BASELINE_DECAY) {
  if (!rounds?.length || startIdx >= rounds.length) return NaN;
  let sum = 0;
  let wsum = 0;
  for (let i = startIdx; i < rounds.length; i++) {
    const v = num(rounds[i]?.[key], NaN);
    if (!Number.isFinite(v)) continue;
    const w = decay ** (i - startIdx);
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

function meanFromRounds(rounds, key) {
  let sum = 0;
  let n = 0;
  for (const r of rounds || []) {
    const v = num(r?.[key], NaN);
    if (!Number.isFinite(v)) continue;
    sum += v;
    n++;
  }
  return n > 0 ? sum / n : NaN;
}

/** Hot/cold vs stable baseline on last 8–12 rounds, weighted by course SG importance. */
export function recentFormStrokeShift(rounds, sgWeights, recentN = 10) {
  if (!rounds?.length || rounds.length < RECENT_FORM_MIN) return 0;
  const nRec = clamp(Math.round(num(recentN, 10)), RECENT_FORM_MIN, RECENT_FORM_MAX);
  const recent = rounds.slice(0, nRec);
  let formSum = 0;
  let formW = 0;
  for (const k of SG_KEYS) {
    const w = num(sgWeights?.[k], 0);
    if (w < 0.03) continue;
    const rMean = meanFromRounds(recent, k);
    const bMean = stableBaselineMean(rounds, k, nRec);
    if (Number.isFinite(rMean) && Number.isFinite(bMean)) {
      formSum += w * (rMean - bMean);
      formW += w;
    }
  }
  if (formW <= 0) return 0;
  return clamp(-(formSum / formW) * 0.88, -0.48, 0.48);
}

export function fieldSgMedians(players) {
  const out = {};
  for (const k of SG_KEYS) {
    const vals = (players || []).map((p) => num(p[k], NaN)).filter(Number.isFinite);
    if (vals.length < 4) {
      out[k] = NaN;
      continue;
    }
    vals.sort((a, b) => a - b);
    const mid = Math.floor(vals.length / 2);
    out[k] = vals.length % 2 === 1 ? vals[mid] : (vals[mid - 1] + vals[mid]) / 2;
  }
  return out;
}

/** Player SG edge on categories that matter most at this course. */
export function courseSkillFitStrokeShift(playerRow, sgWeights, fieldMedians) {
  let sum = 0;
  let wsum = 0;
  for (const k of SG_KEYS) {
    const w = num(sgWeights?.[k], 0);
    if (w < 0.03) continue;
    const pv = num(playerRow?.[k], NaN);
    const fm = num(fieldMedians?.[k], NaN);
    if (!Number.isFinite(pv) || !Number.isFinite(fm)) continue;
    sum += w * (pv - fm);
    wsum += w;
  }
  if (wsum <= 0) return 0;
  return clamp(-(sum / wsum) * 0.46, -0.42, 0.42);
}

export function birdieHeavyVenueAdjustments(venueScoring, ctRow, playerRow, sgWeights, fieldMedians) {
  const vBird = num(venueScoring?.venueAvgBirdies, TOUR_AVG_BIRDIES);
  const adjStp = num(ctRow?.adj_score_to_par, NaN);
  const birdHeavy = vBird > TOUR_AVG_BIRDIES + 0.32 || (Number.isFinite(adjStp) && adjStp < -0.22);
  if (!birdHeavy) return { scoreShift: 0, birdShift: 0, girShift: 0 };

  const appEdge = num(playerRow?.sg_app, 0) - num(fieldMedians?.sg_app, 0);
  const puttEdge = num(playerRow?.sg_putt, 0) - num(fieldMedians?.sg_putt, 0);
  const ottEdge = num(playerRow?.sg_ott, 0) - num(fieldMedians?.sg_ott, 0);
  const ease = clamp((vBird - TOUR_AVG_BIRDIES) * 0.09 + (Number.isFinite(adjStp) ? -adjStp * 0.045 : 0), 0, 0.38);
  const skillBoost = clamp(
    appEdge * num(sgWeights?.sg_app, 0.25) +
      puttEdge * num(sgWeights?.sg_putt, 0.25) +
      ottEdge * num(sgWeights?.sg_ott, 0.15) * 0.55,
    -0.18,
    0.26,
  );
  return {
    scoreShift: -(ease * 0.55 + skillBoost * 0.62),
    birdShift: ease * 2.4 + skillBoost * 2.1,
    girShift: ease * 0.85 + appEdge * num(sgWeights?.sg_app, 0.25) * 0.55,
  };
}

export function computeCourseTailoringShifts({
  row,
  rounds,
  sgImportance,
  fieldMedians,
  venueScoring,
  ctRow,
  teeWaveShift = 0,
}) {
  const weights = sgImportance?.weights || courseRequirementSgWeights(ctRow);
  const skillShift = courseSkillFitStrokeShift(row, weights, fieldMedians);
  const formShift = recentFormStrokeShift(rounds, weights);
  const bird = birdieHeavyVenueAdjustments(venueScoring, ctRow, row, weights, fieldMedians);
  const totalStroke = skillShift + formShift + bird.scoreShift + num(teeWaveShift, 0);
  return {
    skillShift,
    formShift,
    birdScoreShift: bird.scoreShift,
    birdCountShift: bird.birdShift,
    girShift: bird.girShift,
    teeWaveShift: num(teeWaveShift, 0),
    totalStroke,
    dominant: sgImportance?.dominant || dominantSgKey(weights),
  };
}

export function applyCourseTailoringShiftsToRow(row, shifts, coursePar18) {
  if (!row || !shifts) return false;
  const par18 = Math.round(num(coursePar18, NaN)) || 72;
  let changed = false;

  if (Number.isFinite(shifts.totalStroke) && Math.abs(shifts.totalStroke) > 1e-5) {
    const stp = num(row.score_to_par, NaN);
    const ts = num(row.total_score, NaN);
    if (Number.isFinite(stp)) {
      row.score_to_par = Math.round((stp + shifts.totalStroke) * 100) / 100;
      row.total_score = Math.round((par18 + row.score_to_par) * 100) / 100;
    } else if (Number.isFinite(ts)) {
      row.total_score = Math.round((ts + shifts.totalStroke) * 100) / 100;
      row.score_to_par = Math.round((row.total_score - par18) * 100) / 100;
    }
    if (Number.isFinite(num(row.mu_sg, NaN))) {
      row.mu_sg = Math.round((num(row.mu_sg, 0) - shifts.totalStroke) * 1000) / 1000;
      row.implied_mu_sg = row.mu_sg;
    }
    changed = true;
  }

  if (Number.isFinite(shifts.birdCountShift) && Math.abs(shifts.birdCountShift) > 1e-5) {
    const b = num(row.birdies, NaN);
    if (Number.isFinite(b)) {
      row.birdies = Math.round((b + shifts.birdCountShift) * 100) / 100;
      changed = true;
    }
  }

  if (Number.isFinite(shifts.girShift) && Math.abs(shifts.girShift) > 1e-5) {
    const g = num(row.gir, NaN);
    if (Number.isFinite(g)) {
      row.gir = Math.round(clamp(g + shifts.girShift, 0, 18) * 100) / 100;
      changed = true;
    }
  }

  if (changed) {
    row.course_tailoring_applied = true;
    row.course_tailoring_shifts = {
      skill: shifts.skillShift,
      form: shifts.formShift,
      bird_score: shifts.birdScoreShift,
      bird_count: shifts.birdCountShift,
      gir: shifts.girShift,
      tee_wave: shifts.teeWaveShift,
      dominant_sg: shifts.dominant,
    };
  }
  return changed;
}

export function collectVenueHistRowsForSgFit(histRows, courseKey, cutoffMs, timeFn) {
  const ck = normCourseNameKey(courseKey);
  const timeOf =
    timeFn ||
    ((row) => {
      const s = String(row.event_completed || row.projections_updated_at || "").trim();
      const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
      return iso ? Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`) : NaN;
    });
  const out = [];
  for (const row of histRows || []) {
    if (ck) {
      const rk = normCourseNameKey(row.course_name || "");
      if (rk && rk !== ck) continue;
    }
    const t = timeOf(row);
    if (Number.isFinite(cutoffMs) && Number.isFinite(t) && t >= cutoffMs) continue;
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(rs) || rs < 55 || rs > 95) continue;
    out.push(row);
  }
  return out;
}

export async function fitVenueSgImportanceFromCsv(csvPath, courseKey, cutoffMs = NaN) {
  const ck = normCourseNameKey(courseKey);
  if (!ck || !csvPath || !existsSync(csvPath)) return null;
  const rows = [];
  await new Promise((resolve, reject) => {
    const parser = createReadStream(csvPath).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    parser.on("data", (row) => {
      const rk = normCourseNameKey(row.course_name || row.Course_Name || "");
      if (!rk || rk !== ck) return;
      const completed = String(row.event_completed || "").trim();
      if (Number.isFinite(cutoffMs) && completed) {
        const t = Date.parse(`${completed.slice(0, 10)}T12:00:00Z`);
        if (Number.isFinite(t) && t >= cutoffMs) return;
      }
      rows.push({
        round_score: num(row.round_score, NaN),
        course_par: num(row.course_par, NaN),
        sg_total: num(row.sg_total, NaN),
        sg_ott: num(row.sg_ott, NaN),
        sg_app: num(row.sg_app, NaN),
        sg_arg: num(row.sg_arg, NaN),
        sg_putt: num(row.sg_putt, NaN),
      });
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });
  return fitVenueSgImportanceFromRows(rows);
}

export function loadPlayerHistoryRoundsByDg(historyJsonPath, dgIds) {
  const out = new Map();
  if (!historyJsonPath || !existsSync(historyJsonPath)) return out;
  try {
    const j = JSON.parse(readFileSync(historyJsonPath, "utf8"));
    const allow = dgIds instanceof Set ? dgIds : new Set(dgIds || []);
    for (const [dgStr, bucket] of Object.entries(j?.byDgId || {})) {
      const dg = Math.round(num(dgStr, NaN));
      if (!Number.isFinite(dg) || (allow.size && !allow.has(dg))) continue;
      const rounds = Array.isArray(bucket?.rounds) ? [...bucket.rounds] : [];
      rounds.sort((a, b) => {
        const ta = Date.parse(String(a.event_completed || a.date || "")) || 0;
        const tb = Date.parse(String(b.event_completed || b.date || "")) || 0;
        return tb - ta;
      });
      if (rounds.length) out.set(dg, rounds);
    }
  } catch {
    /* optional */
  }
  return out;
}

export function teeWaveShiftForRow(row, meta) {
  if (meta?.projection_round_adjustments?.unified_factors_applied) return 0;
  const waveBias = meta?.projection_unified_factors?.tee_wave_bias || meta?.tee_wave_bias;
  if (!waveBias) return 0;
  const wave = teeWaveFromTeetimeAndLabel(row?.dg_teetime_local ?? row?.teetime, row?.dg_tee_wave);
  if (!wave) return 0;
  const slots = meta?.forecast_wave_slots;
  return teeWaveStrokeShift(wave, waveBias, slots?.morning, slots?.afternoon);
}

export function serializeSgImportanceForMeta(sgImportance) {
  if (!sgImportance?.weights) return null;
  const w = sgImportance.weights;
  return {
    source: sgImportance.source,
    n_venue_rounds: sgImportance.n || 0,
    dominant_sg: sgImportance.dominant || dominantSgKey(w),
    weights: {
      ott: Math.round(num(w.sg_ott, 0) * 1000) / 1000,
      app: Math.round(num(w.sg_app, 0) * 1000) / 1000,
      arg: Math.round(num(w.sg_arg, 0) * 1000) / 1000,
      putt: Math.round(num(w.sg_putt, 0) * 1000) / 1000,
    },
  };
}

export function sgImportanceFromMeta(meta) {
  const sgMeta = meta?.projection_course_basis?.course_sg_importance;
  if (!sgMeta?.weights) return null;
  const w = sgMeta.weights;
  const weights = {
    sg_ott: num(w.ott, 0),
    sg_app: num(w.app, 0),
    sg_arg: num(w.arg, 0),
    sg_putt: num(w.putt, 0),
  };
  let sum = 0;
  for (const k of SG_KEYS) sum += weights[k];
  if (sum < 0.05) return null;
  for (const k of SG_KEYS) weights[k] /= sum;
  return {
    weights,
    dominant: sgMeta.dominant_sg || dominantSgKey(weights),
    n: Math.round(num(sgMeta.n_venue_rounds, 0)) || 0,
    source: sgMeta.source || "",
  };
}

export function courseTailoringMuDelta(market, shifts) {
  if (!shifts || typeof shifts !== "object") return 0;
  const m = market === "Total Score" ? "Total score" : market;
  if (m === "Total score") return num(shifts.totalStroke, 0);
  if (m === "Birdies") return num(shifts.birdCountShift, 0);
  if (m === "GIR") return num(shifts.girShift, 0);
  if (m === "Fairways hit") return -num(shifts.skillShift, 0) * 0.32;
  return 0;
}

/** Walk-forward / export μ adjustment (venue SG fit + 8–12 rd form + birdie-heavy venue). */
export function courseTailoringMuAdjustment(market, row, meta, ctx) {
  if (row?.course_tailoring_applied) return 0;
  const metaLive =
    meta?.meta && typeof meta.meta === "object" ? { ...meta, ...meta.meta } : meta || {};
  const sgImportance = sgImportanceFromMeta(metaLive);
  if (!sgImportance) return 0;
  const id = Math.round(num(row?.dg_id, NaN));
  if (!Number.isFinite(id)) return 0;
  const rounds = Array.isArray(ctx?.historyByDgId?.[String(id)]?.rounds)
    ? ctx.historyByDgId[String(id)].rounds
    : [];
  const shifts = computeCourseTailoringShifts({
    row,
    rounds,
    sgImportance,
    fieldMedians: fieldSgMedians(ctx?.players || []),
    venueScoring: ctx?.venueScoring || {},
    ctRow: ctx?.ctRow ?? null,
    teeWaveShift: num(row?._tee_wave_shift, 0),
  });
  return courseTailoringMuDelta(market, shifts);
}

export function applyCourseTailoringToPlayers(players, opts) {
  const {
    historyByDgId,
    sgImportance,
    fieldMedians,
    venueScoring,
    ctRow,
    coursePar18,
    teeWaveShiftByDg,
  } = opts || {};
  let n = 0;
  for (const row of players || []) {
    const id = Math.round(num(row?.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const rounds = historyByDgId?.[String(id)]?.rounds || [];
    const teeWaveShift =
      teeWaveShiftByDg instanceof Map ? num(teeWaveShiftByDg.get(id), 0) : num(teeWaveShiftByDg, 0);
    const shifts = computeCourseTailoringShifts({
      row,
      rounds,
      sgImportance,
      fieldMedians,
      venueScoring,
      ctRow,
      teeWaveShift,
    });
    if (applyCourseTailoringShiftsToRow(row, shifts, coursePar18)) n++;
  }
  return n;
}
