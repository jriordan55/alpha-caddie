/**
 * Live-week projection μ for browser + shared scripts (weather, pin sheet, in-round scratch).
 * Mirrors round-projection-mu.mjs ouProjectedMeanForMode plus runtime pin-sheet deltas.
 */
import {
  HIST_TEE_WAVE_AFTERNOON_STP,
  statWeatherMuAdjustment,
  weatherDifficultyDeltaFromSnapshot,
} from "./weather-mu-adjustments.mjs";
import {
  liveCurrentRoundTotalScoreMuDelta,
  livePartialRoundCountPropAdjust,
} from "./live-in-play-pricing.mjs";

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

function birdiesPlusEaglesFromRow(row) {
  if (!row || typeof row !== "object") return NaN;
  const b = num(row.birdies, NaN);
  const eob = num(row.eagles_or_better, NaN);
  const eg = num(row.eagles, NaN);
  const eagleAdd = Number.isFinite(eob) ? eob : Number.isFinite(eg) ? eg : 0;
  if (!Number.isFinite(b) && !Number.isFinite(eob) && !Number.isFinite(eg)) return NaN;
  return (Number.isFinite(b) ? b : 0) + eagleAdd;
}

/** DK / PP “Bogeys or Worse”. */
function bogeysPlusDoublesFromRow(row) {
  if (!row || typeof row !== "object") return NaN;
  const bg = num(row.bogeys ?? row.bogies, NaN);
  const dow = num(row.doubles_or_worse, NaN);
  const dbl = num(row.doubles, NaN);
  const dblAdd = Number.isFinite(dow) ? dow : Number.isFinite(dbl) ? dbl : 0;
  if (!Number.isFinite(bg) && !Number.isFinite(dow) && !Number.isFinite(dbl)) return NaN;
  return (Number.isFinite(bg) ? bg : 0) + Math.max(0, dblAdd);
}

function girFairwaysCountFromRaw(v, holes) {
  const n = num(v, NaN);
  if (!Number.isFinite(n)) return NaN;
  if (n > 0 && n <= 1.0001) return Math.min(holes, Math.max(0, Math.round(n * holes)));
  if (n > 1.0001 && n <= holes + 1e-6) return Math.min(holes, Math.max(0, n));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

const OU_STAT_MAP = {
  "Total score": { field: "total_score" },
  Birdies: { field: "birdies" },
  Pars: { field: "pars" },
  Bogeys: { field: "bogeys" },
  GIR: { field: "gir" },
  "Fairways hit": { field: "fairways" },
};

function ouMeanCountingStat(market, row, fairwayHoles) {
  const rec = OU_STAT_MAP[market] || OU_STAT_MAP["Total score"];
  if (market === "Birdies") return birdiesPlusEaglesFromRow(row);
  if (market === "Bogeys") return bogeysPlusDoublesFromRow(row);
  const raw = num(row?.[rec.field], NaN);
  if (!Number.isFinite(raw)) return NaN;
  if (market === "GIR") return girFairwaysCountFromRaw(raw, 18);
  if (market === "Fairways hit") return girFairwaysCountFromRaw(raw, fairwayHoles);
  return raw;
}

function liveProjectionMeta(metaOrPayload) {
  if (!metaOrPayload || typeof metaOrPayload !== "object") return {};
  const nested =
    metaOrPayload.meta && typeof metaOrPayload.meta === "object" ? metaOrPayload.meta : {};
  return { ...metaOrPayload, ...nested };
}

function eventPropBookAlignedMarket(meta, market) {
  const mk = market === "Total Score" ? "Total score" : market;
  return Boolean(meta?.event_prop_book_alignment?.markets?.[mk]);
}

function pinSheetAppliesToRow(row, meta) {
  const ps = meta?.pin_sheet;
  if (!ps || typeof ps !== "object") return false;
  const rnd = Math.round(num(row?.round, NaN));
  const psRnd = Math.round(num(ps.round, NaN));
  return Number.isFinite(rnd) && Number.isFinite(psRnd) && rnd === psRnd;
}

function pinSheetMuAdjustment(market, row, meta) {
  if (!pinSheetAppliesToRow(row, meta)) return 0;
  if (row?._pin_adjusted) return 0;
  const ps = meta.pin_sheet;
  const mKey = market === "Total Score" ? "Total score" : market;
  if (mKey === "Total score") return num(ps.total_score_delta, 0);
  if (mKey === "Birdies") return num(ps.birdies_delta, 0);
  if (mKey === "Pars") return num(ps.pars_delta, 0);
  if (mKey === "Bogeys") return num(ps.bogeys_delta, 0);
  if (mKey === "GIR") return num(ps.gir_delta, 0);
  if (mKey === "Fairways hit") return num(ps.fairways_delta, 0);
  if (mKey === "Putts") return num(ps.putts_delta, num(ps.total_score_delta, 0) * 0.35);
  return 0;
}

function weatherAlreadyBaked(meta, row) {
  return Boolean(meta?.projection_counts_weather_baked && row?.weather_counts_baked);
}

function teeWaveFromRow(row) {
  const wave = String(row?.dg_tee_wave || "").trim().toLowerCase();
  if (wave === "morning" || wave === "afternoon") return wave;
  const tt = String(row?.dg_teetime_local ?? row?.teetime ?? "").trim();
  const m = tt.match(/(\d{1,2}):(\d{2})/);
  if (!m) return "";
  const hh = parseInt(m[1], 10);
  return Number.isFinite(hh) && hh < 13 ? "morning" : "afternoon";
}

function teeWaveMuAdjustment(market, row, meta) {
  if (meta?.projection_round_adjustments?.unified_factors_applied) return 0;
  if (row?.course_tailoring_applied) return 0;
  const waveBias = meta?.projection_unified_factors?.tee_wave_bias || meta?.tee_wave_bias;
  const wave = teeWaveFromRow(row);
  if (!wave) return 0;
  const w = 0.28;
  let shift = 0;
  let histDelta = num(waveBias?.deltaAfternoonMinusMorning, NaN);
  if (!Number.isFinite(histDelta) || Math.abs(histDelta) < 0.02) histDelta = HIST_TEE_WAVE_AFTERNOON_STP;
  if (wave === "afternoon") shift += histDelta * 0.55 * w;
  else shift -= histDelta * 0.55 * w;
  const slots = meta?.forecast_wave_slots;
  if (slots?.morning && slots?.afternoon) {
    const dM = weatherDifficultyDeltaFromSnapshot(slots.morning);
    const dA = weatherDifficultyDeltaFromSnapshot(slots.afternoon);
    if (Number.isFinite(dM) && Number.isFinite(dA)) {
      const waveDiff = dA - dM;
      shift += (wave === "afternoon" ? waveDiff : -waveDiff) * 0.4 * w;
    }
  }
  if (market === "Total score") return shift;
  if (market === "Bogeys") return 0.45 * shift;
  if (market === "Birdies") return -0.5 * shift;
  if (market === "Pars") return 0.2 * shift;
  if (market === "GIR") return -0.22 * shift;
  if (market === "Fairways hit") return -0.14 * shift;
  return 0;
}

/**
 * Model μ for live O/U — export row values + weather (when not baked) + pin sheet + in-round scratch.
 */
export function ouProjectedMeanForLive(market, row, meta) {
  const mKey = market === "Total Score" ? "Total score" : market;
  const metaLive = liveProjectionMeta(meta);
  const fairwayHoles = num(metaLive?.projection_course_basis?.fairway_holes_modeled, NaN);
  const fwHoles = Number.isFinite(fairwayHoles) && fairwayHoles > 0 ? Math.round(fairwayHoles) : 14;
  const base = ouMeanCountingStat(mKey, row, fwHoles);
  if (!Number.isFinite(base)) return NaN;

  const countLive = livePartialRoundCountPropAdjust(mKey, row, metaLive);
  const pinAdj = pinSheetMuAdjustment(mKey, row, metaLive);

  if (eventPropBookAlignedMarket(metaLive, mKey)) {
    const liveScore = mKey === "Total score" ? liveCurrentRoundTotalScoreMuDelta(row, metaLive) : 0;
    return base + pinAdj + countLive.muDelta + liveScore;
  }

  const weatherAdj = weatherAlreadyBaked(metaLive, row) ? 0 : statWeatherMuAdjustment(mKey, row);
  const teeAdj = teeWaveMuAdjustment(mKey, row, metaLive);
  const courseAdj = courseTailoringMuFromMeta(mKey, row, metaLive);
  const liveScore = mKey === "Total score" ? liveCurrentRoundTotalScoreMuDelta(row, metaLive) : 0;
  return base + weatherAdj + teeAdj + courseAdj + pinAdj + countLive.muDelta + liveScore;
}

function sgWeightsFromMeta(meta) {
  const w = meta?.projection_course_basis?.course_sg_importance?.weights;
  if (!w) return null;
  const weights = {
    sg_ott: num(w.ott, 0),
    sg_app: num(w.app, 0),
    sg_arg: num(w.arg, 0),
    sg_putt: num(w.putt, 0),
  };
  let sum = weights.sg_ott + weights.sg_app + weights.sg_arg + weights.sg_putt;
  if (sum < 0.05) return null;
  for (const k of Object.keys(weights)) weights[k] /= sum;
  return weights;
}

/** Live μ bump when tailoring is in meta but not yet baked into row counts. */
function courseTailoringMuFromMeta(market, row, meta) {
  if (row?.course_tailoring_applied) return 0;
  const weights = sgWeightsFromMeta(meta);
  if (!weights) return 0;
  let skill = 0;
  let wsum = 0;
  for (const [k, w] of Object.entries(weights)) {
    const v = num(row?.[k], NaN);
    if (!Number.isFinite(v) || w < 0.03) continue;
    skill += w * v;
    wsum += w;
  }
  if (wsum <= 0) return 0;
  const skillShift = clamp(-(skill / wsum) * 0.18, -0.28, 0.28);
  const basis = meta?.projection_course_basis || {};
  const vBird = num(basis.venue_avg_birdies, 4.2);
  const adjStp = num(basis.course_adj_score_to_par, NaN);
  const birdHeavy = vBird > 4.52 || (Number.isFinite(adjStp) && adjStp < -0.22);
  const birdEase = birdHeavy ? clamp((vBird - 4.2) * 0.06, 0, 0.25) : 0;
  const mKey = market === "Total Score" ? "Total score" : market;
  if (mKey === "Total score") return skillShift + (birdHeavy ? -birdEase * 0.55 : 0);
  if (mKey === "Birdies") return birdHeavy ? birdEase * 2.2 : 0;
  if (mKey === "GIR") return birdHeavy ? birdEase * 0.75 : 0;
  if (mKey === "Fairways hit") return -skillShift * 0.32;
  return 0;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

/** @deprecated Use ouProjectedMeanForLive — kept for existing imports. */
export function ouProjectedMeanWithLiveScratch(market, row, meta) {
  return ouProjectedMeanForLive(market, row, meta);
}
