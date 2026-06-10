/**
 * Shared projection math (extracted from fetch-datagolf.mjs for historical backtests).
 */
import { createReadStream, existsSync } from "fs";
import { join } from "path";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  fairwayHitsFromRate01,
  fairwayRate01FromDg,
  girHitsFromRate01,
  girRate01FromDg,
  num,
} from "./dg-traditional-stats.mjs";

export const RAW_ROUND_SD = Number(process.env.GOLF_RAW_ROUND_SD) || 2.75;
export const N_FAIRWAY_HOLES = Number(process.env.GOLF_N_FAIRWAY_HOLES) || 14;

function imputeCountsFromNegMu(muSg) {
  let stp = -num(muSg, 0);
  stp = Math.max(-8, Math.min(8, stp));
  let eagles = Math.max(0, 0.15 - 0.02 * stp);
  let birdies = Math.max(0.5, 3.8 - 0.45 * stp);
  let bogeys = Math.max(0.5, 2.6 + 0.5 * stp);
  let doubles = Math.max(0.1, 0.35 + 0.05 * stp);
  let pars = Math.max(0.2, 18 - eagles - birdies - bogeys - doubles);
  const s = eagles + birdies + pars + bogeys + doubles;
  const k = 18 / s;
  return {
    eagles: eagles * k,
    birdies: birdies * k,
    pars: pars * k,
    bogeys: bogeys * k,
    doubles: doubles * k,
  };
}

/**
 * Nudge bird/bog (pars residual) toward `targetStp` without re-solving to a single narrow `pars` band for the field.
 * `strength` ∈ (0,1]: fraction of score-vs-par gap to close in one pass.
 */
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
    return {
      eagles: e * k,
      birdies: b * k,
      pars: p * k,
      bogeys: bg * k,
      doubles: d * k,
    };
  }
  return { eagles: e, birdies: b, pars: Math.max(0.12, p), bogeys: bg, doubles: d };
}

function imputeCountsWithHistory(muSg, countFit) {
  const legacy = imputeCountsFromNegMu(muSg);
  const stp = -clampMuSg(muSg);
  const x = Math.max(-8, Math.min(8, stp));
  if (!countFit || countFit.n_counts < 800 || !countFit.slopes) return { ...legacy };

  /** Cap how much population OLS can override per-player legacy μ curves (large n else → one profile per μ). */
  const rawShrink = countFit.n_counts / (countFit.n_counts + 2000);
  const shrink = Math.min(0.38, rawShrink);
  /** Do not regress `pars` vs stp — it is nearly collinear with bird/bog in data and kills cross-player spread; derive from the other four after blend. */
  const keysNoPar = ["eagles", "birdies", "bogeys", "doubles"];
  /** @type {Record<string, number>} */
  const out = { pars: legacy.pars };
  for (const k of keysNoPar) {
    const c = countFit.slopes[k];
    if (!c || !Number.isFinite(c.a) || !Number.isFinite(c.b)) {
      out[k] = legacy[k];
      continue;
    }
    const pred = c.a + c.b * x;
    const lo = k === "eagles" || k === "doubles" ? 0.04 : 0.2;
    out[k] = shrink * pred + (1 - shrink) * legacy[k];
    out[k] = Math.max(lo, out[k]);
  }
  out.pars = Math.max(0.2, 18 - out.eagles - out.birdies - out.bogeys - out.doubles);
  let s = out.eagles + out.birdies + out.pars + out.bogeys + out.doubles;
  if (!(s > 0.1)) return { ...legacy };
  const kf = 18 / s;
  for (const k of ["eagles", "birdies", "pars", "bogeys", "doubles"]) out[k] *= kf;
  return {
    eagles: out.eagles,
    birdies: out.birdies,
    pars: out.pars,
    bogeys: out.bogeys,
    doubles: out.doubles,
  };
}

/** @deprecated Use fairwayRate01FromDg — skill-ratings `driving_acc` is pp vs field, not a 0–1 share. */
function fairwayRate01FromDrivingSkill(skRow, nFw = N_FAIRWAY_HOLES, liveTrad = null) {
  return fairwayRate01FromDg(skRow, liveTrad, nFw);
}

function isPlausibleDrivingDistanceYds(y) {
  const v = num(y, NaN);
  return Number.isFinite(v) && v >= 235 && v <= 380;
}

/** Yards for modeling (FW): measured carry/roll when present, else neutral + DG yards-vs-tour rating. */
function impliedDrivingYardsFromSkillRow(sk) {
  if (!sk || typeof sk !== "object") return NaN;
  const y = num(sk.driving_distance, NaN);
  if (Number.isFinite(y) && isPlausibleDrivingDistanceYds(y)) return y;
  const rt = num(sk.driving_distance_rating, NaN);
  if (Number.isFinite(rt) && rt >= -55 && rt <= 55) return 302 + rt;
  return NaN;
}

/** Expected fairways in [0, n_fw]: cannot exceed driving holes or be negative (count stat, not a tuned model cap). */
function fairwayHitsExpectation(x, nFw) {
  if (!Number.isFinite(nFw) || nFw <= 0 || !Number.isFinite(x)) return NaN;
  if (x <= 0) return 0;
  if (x >= nFw) return nFw;
  return x;
}

/**
 * Tour FW vs strokes-to-par line, evaluated at skill proxy x ≈ −μ_sg, then nudged by OTT vs field and overall μ_sg
 * (population line alone sits low for elite drivers because x is compressed vs real round score − par).
 */
function fairwaysFromHistoricalStp(mu_sg, nFw, histCalib, fieldMeanOtt, skRow) {
  const ln = histCalib?.fw_stp_line;
  if (!ln || !Number.isFinite(ln.a) || !Number.isFinite(ln.b)) return NaN;
  const x = Math.max(-10, Math.min(10, -clampMuSg(mu_sg)));
  let raw = ln.a + ln.b * x;
  const mu = clampMuSg(mu_sg);
  raw += 0.48 * Math.max(0, Math.min(2.5, mu));
  const ott = num(skRow?.sg_ott, NaN);
  const fo = num(fieldMeanOtt, NaN);
  if (Number.isFinite(ott) && Number.isFinite(fo)) {
    const edge = Math.max(-0.45, Math.min(1.15, ott - fo));
    raw += 2.05 * edge;
  }
  return fairwayHitsExpectation(raw, nFw);
}

/**
 * Fairways: SG:OTT curve + historical tour regression vs skill proxy + optional driving-field rate.
 * OTT and driving-only can both sit ~6; historical regression anchors counts to real rounds.
 */
function projectedFairwaysFromSkillOnly(
  mu_sg,
  skRow,
  nFw,
  fieldMeanOtt,
  drivingDistYds,
  fieldMeanDrive,
  histCalib,
) {
  const fromApi = fairwayHitsFromRate01(fairwayRate01FromDg(skRow, null, nFw), nFw);
  if (Number.isFinite(fromApi)) return fromApi;

  const ottFw = fairwaysExpectedFromSkill(mu_sg, skRow?.sg_ott, nFw, fieldMeanOtt, drivingDistYds, fieldMeanDrive);
  const histFw = fairwaysFromHistoricalStp(mu_sg, nFw, histCalib, fieldMeanOtt, skRow);
  let y = ottFw;
  if (Number.isFinite(histFw)) {
    y = Number.isFinite(y) ? 0.07 * y + 0.93 * histFw : histFw;
  }
  return fairwayHitsExpectation(y, nFw);
}

/**
 * Re-fetching DataGolf overwrites projections.json with `props: []` unless we carry forward prior DK / CSV rows
 * for the same event week. Set `GOLF_RESET_PROPS=1` to force an empty props array.
 */
function tryPreservePropsFromDisk(outPath, eventName, courseUsed) {
  if (String(process.env.GOLF_RESET_PROPS || "").trim() === "1") return [];
  try {
    if (!existsSync(outPath)) return [];
    const prev = JSON.parse(readFileSync(outPath, "utf8"));
    if (!Array.isArray(prev.props) || !prev.props.length) return [];
    const wk = fieldWeekKey(eventName, courseUsed);
    const prevWk = String(prev.datagolf_field_week_key || "").trim();
    const sameWeek = Boolean(wk && prevWk && wk === prevWk);
    const sameEvent =
      eventsLikelySame(String(prev.event_name || "").trim(), String(eventName || "").trim()) &&
      !coursesClearlyDistinct(String(prev.course_used || "").trim(), String(courseUsed || "").trim());
    if (sameWeek || sameEvent) {
      console.log(`[fetch-dg] preserving ${prev.props.length} props from prior projections.json`);
      return prev.props;
    }
  } catch (e) {
    console.warn("[fetch-dg] could not merge prior props:", e?.message || e);
  }
  return [];
}

/** Rolling mean GIR% / FW% per dg_id from DataGolf historical rounds CSV (`gir`, `driving_acc` columns). */
async function loadRollingTraditionalPctByDg(csvPath, dgIdSet, maxRoundsPerPlayer = 36) {
  const maxR = Math.max(8, Math.round(num(maxRoundsPerPlayer, 36)));
  const minYear = new Date().getFullYear() - 2;
  /** @type {Map<number, { gir: number[], fw: number[] }>} */
  const buf = new Map();
  if (!existsSync(csvPath) || !dgIdSet?.size) return new Map();

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
      const tour = String(row.tour || "").toLowerCase();
      if (tour !== "pga" && tour !== "liv") return;
      const yr = parseInt(row.year, 10);
      if (Number.isFinite(yr) && yr < minYear) return;
      const id = Math.round(num(row.dg_id, NaN));
      if (!Number.isFinite(id) || !dgIdSet.has(id)) return;
      let slot = buf.get(id);
      if (!slot) {
        slot = { gir: [], fw: [] };
        buf.set(id, slot);
      }
      const girR = traditionalRate01(row.gir, 18);
      if (Number.isFinite(girR) && slot.gir.length < maxR) slot.gir.push(girR);
      const fwR = traditionalRate01(row.driving_acc, N_FAIRWAY_HOLES);
      if (Number.isFinite(fwR) && slot.fw.length < maxR) slot.fw.push(fwR);
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });

  const mean = (arr) => (arr.length ? arr.reduce((s, x) => s + x, 0) / arr.length : NaN);
  /** @type {Map<number, { girRate01: number, fwRate01: number }>} */
  const out = new Map();
  for (const [id, slot] of buf) {
    const girRate01 = mean(slot.gir);
    const fwRate01 = mean(slot.fw);
    if (Number.isFinite(girRate01) || Number.isFinite(fwRate01)) {
      out.set(id, { girRate01, fwRate01 });
    }
  }
  return out;
}

/**
 * Stream `data/historical_rounds_all.csv`: OLS of hole counts vs (round_score − course_par), and R² for GIR~APP / FW~OTT.
 * Used for count-curve calibration (historical R² still logged for diagnostics).
 */
async function loadHistoricalCsvCalibration(modelRoot, courseKeyOpt) {
  const empty = {
    skipped: false,
    n_counts: 0,
    n_gir_app: 0,
    n_fw_ott: 0,
    n_fw_stp: 0,
    r2_gir_app: NaN,
    r2_fw_ott: NaN,
    slopes: null,
    fw_stp_line: null,
    w_gir_skill: 0.78,
    w_ott_skill: 0.85,
    w_ott_decomp: 0.65,
    csv_path: null,
  };
  if (String(process.env.GOLF_SKIP_HIST_STATS_ON_FETCH || "").trim() === "1") {
    return { ...empty, skipped: true };
  }
  const csvPath = join(modelRoot, "data", "historical_rounds_all.csv");
  if (!existsSync(csvPath)) return { ...empty, csv_path: csvPath };

  const ckWant = courseKeyOpt ? normCourseNameKey(courseKeyOpt) : "";
  const cy = new Date().getFullYear();

  let n = 0;
  let sx = 0;
  let sx2 = 0;
  const sy = { eagles: 0, birdies: 0, pars: 0, bogeys: 0, doubles: 0 };
  const sxy = { eagles: 0, birdies: 0, pars: 0, bogeys: 0, doubles: 0 };

  let ng = 0;
  let sga = 0;
  let sg2a = 0;
  let sgG = 0;
  let g2 = 0;
  let sgAg = 0;

  let nf = 0;
  let sgo = 0;
  let sg2o = 0;
  let sgF = 0;
  let f2 = 0;
  let sgOf = 0;

  let nFwR = 0;
  let sxFw = 0;
  let sFwR = 0;
  let sxxFw = 0;
  let sfxFw = 0;

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
      const tour = String(row.tour || "").toLowerCase();
      if (tour !== "pga" && tour !== "liv") return;
      if (ckWant) {
        const ckRow = normCourseNameKey(row.course_name || row.Course_Name || "");
        if (!ckRow || ckRow !== ckWant) return;
        const yr = parseInt(row.year, 10);
        if (Number.isFinite(yr) && yr < cy - 8) return;
      }

      const cp = num(row.course_par, NaN);
      const rs = num(row.round_score, NaN);
      if (!Number.isFinite(cp) || cp < 63 || cp > 76) return;
      if (!Number.isFinite(rs) || rs < 55 || rs > 95) return;

      const e = num(row.eagles_or_better, NaN);
      const b = num(row.birdies, NaN);
      const p = num(row.pars, NaN);
      const bg = num(row.bogies, NaN);
      const d = num(row.doubles_or_worse, NaN);
      if (![e, b, p, bg, d].every((v) => Number.isFinite(v) && v >= 0 && v <= 18)) return;
      const sumH = e + b + p + bg + d;
      if (Math.abs(sumH - 18) > 0.51) return;

      const stpRaw = rs - cp;
      const x = Math.max(-10, Math.min(10, stpRaw));
      n++;
      sx += x;
      sx2 += x * x;
      sy.eagles += e;
      sy.birdies += b;
      sy.pars += p;
      sy.bogeys += bg;
      sy.doubles += d;
      sxy.eagles += x * e;
      sxy.birdies += x * b;
      sxy.pars += x * p;
      sxy.bogeys += x * bg;
      sxy.doubles += x * d;

      const sgApp = num(row.sg_app, NaN);
      const girR = num(row.gir, NaN);
      if (Number.isFinite(sgApp) && Number.isFinite(girR) && girR > 0.05 && girR < 0.995) {
        const gc = girR * 18;
        ng++;
        sga += sgApp;
        sg2a += sgApp * sgApp;
        sgG += gc;
        g2 += gc * gc;
        sgAg += sgApp * gc;
      }

      const sgOtt = num(row.sg_ott, NaN);
      const da = num(row.driving_acc, NaN);
      if (Number.isFinite(sgOtt) && Number.isFinite(da) && da > 0.05 && da < 0.995) {
        const fc = da * N_FAIRWAY_HOLES;
        nf++;
        sgo += sgOtt;
        sg2o += sgOtt * sgOtt;
        sgF += fc;
        f2 += fc * fc;
        sgOf += sgOtt * fc;
      }

      let fwCt = NaN;
      if (Number.isFinite(da)) {
        if (da > 0.05 && da < 0.995) fwCt = da * N_FAIRWAY_HOLES;
        else if (da > 1 && da <= N_FAIRWAY_HOLES) fwCt = da;
        else if (da > N_FAIRWAY_HOLES && da <= 100) fwCt = (da / 100) * N_FAIRWAY_HOLES;
      }
      if (Number.isFinite(fwCt) && fwCt >= 0 && fwCt <= N_FAIRWAY_HOLES + 0.01) {
        nFwR++;
        sxFw += x;
        sFwR += fwCt;
        sxxFw += x * x;
        sfxFw += x * fwCt;
      }
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });

  /** @type {typeof empty} */
  const out = { ...empty, csv_path: csvPath, n_counts: n, n_gir_app: ng, n_fw_ott: nf, n_fw_stp: nFwR };

  if (n >= 400) {
    const vx = sx2 - (sx * sx) / n;
    if (vx > 1e-6) {
      /** @type {Record<string, { a: number; b: number }>} */
      const slopes = {};
      for (const k of ["eagles", "birdies", "pars", "bogeys", "doubles"]) {
        const vy = sy[k];
        const cov = sxy[k] - (sx * vy) / n;
        const b = cov / vx;
        const a = vy / n - b * (sx / n);
        slopes[k] = { a, b };
      }
      out.slopes = slopes;
    }
  }

  if (ng >= 400) {
    const vxa = sg2a - (sga * sga) / ng;
    const vyg = g2 - (sgG * sgG) / ng;
    const cag = sgAg - (sga * sgG) / ng;
    if (vxa > 1e-8 && vyg > 1e-8) out.r2_gir_app = (cag * cag) / (vxa * vyg);
  }
  if (nf >= 400) {
    const vxo = sg2o - (sgo * sgo) / nf;
    const vyf = f2 - (sgF * sgF) / nf;
    const cof = sgOf - (sgo * sgF) / nf;
    if (vxo > 1e-8 && vyf > 1e-8) out.r2_fw_ott = (cof * cof) / (vxo * vyf);
  }

  if (nFwR >= 400) {
    const denom = nFwR * sxxFw - sxFw * sxFw;
    if (denom > 1e-6) {
      const bFw = (nFwR * sfxFw - sxFw * sFwR) / denom;
      const aFw = (sFwR - bFw * sxFw) / nFwR;
      if (Number.isFinite(aFw) && Number.isFinite(bFw) && Math.abs(bFw) < 0.65) {
        out.fw_stp_line = { a: aFw, b: bFw, n: nFwR };
      }
    }
  }

  out.w_gir_skill = 1;
  out.w_ott_skill = 1;
  out.w_ott_decomp = 1;

  const venueTag = ckWant ? ` at venue "${ckWant}"` : "";
  if (n >= 400) {
    console.log(
      `[fetch-dg] historical calibration${venueTag}: n_counts=${n}, n_gir_app=${ng} (R²≈${Number.isFinite(out.r2_gir_app) ? out.r2_gir_app.toFixed(3) : "?"}), n_fw_ott=${nf} (R²≈${Number.isFinite(out.r2_fw_ott) ? out.r2_fw_ott.toFixed(3) : "?"}), n_fw_stp=${nFwR}${out.fw_stp_line ? " (FW~stp line fit)" : ""}; projections blend GIR/fairways vs historical`,
    );
  } else {
    console.log(
      `[fetch-dg] historical calibration${venueTag}: only ${n} scored rounds in CSV (need ≥400 for count regression / stable R²) — using legacy count curve`,
    );
  }

  return out;
}

function clampMuSg(m) {
  const x = num(m, 0);
  if (!Number.isFinite(x)) return 0;
  return Math.max(-4, Math.min(4, x));
}

/** Robust field center for SG pillars (mean is pulled by long left tail and jams elites on FW/GIR caps). */
function fieldSkillMedian(samples) {
  const a = (samples || []).filter((x) => Number.isFinite(x)).slice().sort((p, q) => p - q);
  if (a.length < 8) return NaN;
  const mid = Math.floor(a.length / 2);
  return a.length % 2 ? a[mid] : (a[mid - 1] + a[mid]) / 2;
}

/** Fairway opportunities = # of par-4 + par-5 holes (driving holes) when all 18 pars are valid 3–5. */
function fairwayHoleCountFromPars(pars, fallback = N_FAIRWAY_HOLES) {
  if (!Array.isArray(pars) || pars.length !== 18) return fallback;
  let n = 0;
  for (const p of pars) {
    const v = Math.round(num(p, NaN));
    if (!Number.isFinite(v) || v < 3 || v > 5) return fallback;
    if (v === 4 || v === 5) n++;
  }
  if (n < 1) return fallback;
  return n;
}

/** preds/pre-tournament baseline_history_fit: expected strokes this round for this course (column names vary). */
function pretExpectedStrokesThisRound(row) {
  if (!row || typeof row !== "object") return NaN;
  const c = firstNumCol(row, [
    "predicted_round_score",
    "predicted_score",
    "round_score",
    "avg_score",
    "average_score",
    "adjusted_round_score",
    "adj_round_score",
    "model_prediction",
    "pred_score",
  ]);
  if (!c) return NaN;
  const v = num(row[c], NaN);
  if (!Number.isFinite(v) || v < 54 || v > 95) return NaN;
  return v;
}

/** μ-only fairways fallback: linear expected count vs strokes-to-par on n_fw scale. */
function fairwaysMuImputeOnly(stpVec, nFw) {
  if (!Number.isFinite(nFw) || nFw <= 0) return NaN;
  return 0.55 * nFw - 0.15 * stpVec;
}

/**
 * Expected fairways (N_fw hole scale) from SG:OTT vs the field median + small total-SG tilt (μ-only fallback mixed in lightly).
 */
function fairwaysExpectedFromSkill(muSg, sgOtt, nFw, fieldMeanOtt, drivingDistYds, fieldMeanDrive) {
  const mu = clampMuSg(muSg);
  const stp = -mu;
  const fallback = fairwaysMuImputeOnly(stp, nFw);
  const o = num(sgOtt, NaN);
  const m = num(fieldMeanOtt, NaN);
  if (!Number.isFinite(o) || !Number.isFinite(m)) return fairwayHitsExpectation(fallback, nFw);
  let rate = 0.56 + 0.72 * (o - m) + 0.08 * mu;
  let ottFw = rate * nFw;
  if (Number.isFinite(fallback)) ottFw = 0.02 * fallback + 0.98 * ottFw;
  const dy = num(drivingDistYds, NaN);
  const my = num(fieldMeanDrive, NaN);
  if (Number.isFinite(dy) && Number.isFinite(my) && dy >= 240 && dy <= 345 && my >= 265 && my <= 315) {
    ottFw += -0.021 * (dy - my);
  }
  return fairwayHitsExpectation(ottFw, nFw);
}

/** Expected GIR count (18-hole scale) from SG:APP vs field mean + small total-SG tilt (mirrors FW/OTT path). */
function girExpectedFromSkill(muSg, sgApp, nGirHoles, fieldMeanApp) {
  const mu = clampMuSg(muSg);
  const stp = -mu;
  const fallback = Math.max(6, Math.min(16, 11.5 - 0.25 * stp));
  const a = num(sgApp, NaN);
  const m = num(fieldMeanApp, NaN);
  if (!Number.isFinite(a) || !Number.isFinite(m)) return fallback;
  let rate = 0.6 + 0.34 * (a - m) + 0.04 * mu;
  rate = Math.max(0.48, Math.min(0.82, rate));
  const appGir = Math.max(6, Math.min(16, rate * nGirHoles));
  return Math.max(6, Math.min(16, 0.14 * fallback + 0.86 * appGir));
}

/** Default widens R2–R4 vs R1 so per-round projections separate (override GOLF_NODE_ROUND_MU_MULT). */
function parseRoundMuMult() {
  const def = [1, 0.945, 0.885, 0.82];
  const raw = process.env.GOLF_NODE_ROUND_MU_MULT;
  if (raw == null || !String(raw).trim()) return def;
  const parts = String(raw)
    .split(",")
    .map((s) => num(s.trim(), NaN));
  if (parts.length < 4 || parts.some((p) => !Number.isFinite(p))) return def;
  return parts.slice(0, 4);
}

function derivedStatsFromMuSg(muRaw, nFairwayHoles, opts = {}) {
  const mu_sg = clampMuSg(muRaw);
  let im = imputeCountsWithHistory(mu_sg, opts.histCountFit);
  const stpVec = -mu_sg;
  const nGir = Number.isFinite(opts.nGirHoles) ? opts.nGirHoles : 18;
  const skR = opts.skRow;
  const liveTrad = opts.liveTrad ?? null;
  let gir = girHitsFromRate01(
    girRate01FromDg(skR, liveTrad, { muSg: mu_sg, fieldMeanApp: opts.fieldMeanApp }),
    nGir,
  );
  if (!Number.isFinite(gir) && Number.isFinite(opts.sg_app) && Number.isFinite(opts.fieldMeanApp)) {
    gir = girExpectedFromSkill(mu_sg, opts.sg_app, nGir, opts.fieldMeanApp);
  }
  if (!Number.isFinite(gir)) gir = Math.max(6, Math.min(16, 11.5 - 0.25 * stpVec));
  const distFw = isPlausibleDrivingDistanceYds(opts.driving_distance) ? opts.driving_distance : NaN;
  let fairways = fairwayHitsFromRate01(fairwayRate01FromDg(skR, liveTrad, nFairwayHoles), nFairwayHoles);
  if (!Number.isFinite(fairways)) {
    fairways = projectedFairwaysFromSkillOnly(
      mu_sg,
      skR,
      nFairwayHoles,
      opts.fieldMeanOtt,
      distFw,
      opts.fieldMeanDrive,
      opts.histCountFit,
    );
  }
  const putts = Math.max(22, Math.min(35, 28.5 + 0.32 * stpVec - 0.1 * (gir - 11)));
  return {
    mu_sg,
    implied_mu_sg: mu_sg,
    eagles: im.eagles,
    birdies: im.birdies,
    pars: im.pars,
    bogeys: im.bogeys,
    doubles: im.doubles,
    gir,
    fairways,
    putts,
  };
}

export {
  clampMuSg,
  imputeCountsWithHistory,
  derivedStatsFromMuSg,
  fieldSkillMedian,
  parseRoundMuMult,
  loadHistoricalCsvCalibration,
  isPlausibleDrivingDistanceYds,
};
