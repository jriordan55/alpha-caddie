/**
 * Bayesian pin calibration: blend rule-based pin geometry with historical hole scoring
 * at similar setups. Same-course + same-hole matches weigh highest.
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import path from "path";
import readline from "readline";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { loadPinHoleScoringIndex } from "./pin-hole-scoring-index.mjs";
import {
  holePinDifficulty,
  holePriorVsParFromPinScore,
  num,
  roundAdjustmentsFromExcess,
  roundAdjustmentsFromPinSheet,
} from "./pin-sheet-difficulty.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const MODEL_ROOT = path.resolve(__dirname, "..", "..");
const ROUNDS_CSV = path.join(MODEL_ROOT, "data", "historical_rounds_all.csv");

const TIER_WEIGHT = { 1: 8, 2: 4, 3: 1.25 };
const MAX_OBS_PER_HOLE = 24;

function envNum(name, fallback) {
  const v = Number(process.env[name]);
  return Number.isFinite(v) && v > 0 ? v : fallback;
}

function normEvent(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (c === '"') {
      q = !q;
      continue;
    }
    if (c === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += c;
  }
  out.push(cur);
  return out;
}

function holeTier(obs, target, courseKey) {
  const sameCourse = obs.course_key === courseKey;
  const sameHole = obs.hole === target.hole;
  const frontT = num(target.front, NaN);
  const sideT = num(target.side, NaN);
  const frontO = num(obs.front, NaN);
  const sideO = num(obs.side, NaN);
  const pinT = num(target.pinScore, NaN);
  const pinO = num(obs.pin_score, NaN);

  if (sameCourse && sameHole && Number.isFinite(frontT) && Number.isFinite(frontO) && Math.abs(frontO - frontT) <= 4) {
    if (!Number.isFinite(sideT) || !Number.isFinite(sideO) || Math.abs(sideO - sideT) <= 3) return 1;
  }
  if (sameCourse && sameHole && Number.isFinite(pinT) && Number.isFinite(pinO) && Math.abs(pinO - pinT) <= 0.14) {
    return 2;
  }
  if (sameHole && Number.isFinite(pinT) && Number.isFinite(pinO) && Math.abs(pinO - pinT) <= 0.1) return 3;
  return 0;
}

function bayesianMean(priorMean, priorK, samples) {
  let w = priorK;
  let wx = priorK * priorMean;
  for (const s of samples) {
    const sw = s.weight * Math.min(Math.sqrt(Math.max(s.n, 1)), 18);
    if (sw <= 0) continue;
    w += sw;
    wx += sw * s.vs_par;
  }
  if (w <= 0) return priorMean;
  return wx / w;
}

function calibrateHole(target, observations, courseKey, excludeKey) {
  const prior = holePriorVsParFromPinScore(target.pinScore);
  const priorK = envNum("GOLF_PIN_BAYESIAN_PRIOR_K", 10);
  const courseK = envNum("GOLF_PIN_BAYESIAN_COURSE_K", 4);

  const rawSamples = [];
  let bestTier = 0;
  for (const obs of observations) {
    if (excludeKey && `${obs.course_key}|${obs.play_date}|${obs.round}` === excludeKey) continue;
    const tier = holeTier(obs, target, courseKey);
    if (!tier) continue;
    bestTier = Math.max(bestTier, tier);
    rawSamples.push({
      vs_par: obs.vs_par,
      n: obs.n,
      weight: TIER_WEIGHT[tier],
      tier,
      course_key: obs.course_key,
      play_date: obs.play_date,
      hole: obs.hole,
    });
  }

  rawSamples.sort((a, b) => a.tier - b.tier || b.n - a.n);
  const samples = rawSamples.slice(0, MAX_OBS_PER_HOLE);

  const hasCourse = samples.some((s) => s.tier <= 2 && s.course_key === courseKey);
  const k = hasCourse ? courseK : priorK;
  const posterior = bayesianMean(prior, k, samples);

  return {
    hole: target.hole,
    pinScore: target.pinScore,
    priorVsPar: prior,
    posteriorVsPar: posterior,
    nObs: samples.length,
    nPlayers: samples.reduce((a, s) => a + s.n, 0),
    bestTier,
    sameCourseObs: samples.filter((s) => s.tier <= 2 && s.course_key === courseKey).length,
  };
}

/** Course baseline round score + per event-round field averages. */
async function loadRoundScoringContext() {
  const courseBaselines = new Map();
  const eventRounds = new Map();
  if (!existsSync(ROUNDS_CSV)) return { courseBaselines, eventRounds };

  const rl = readline.createInterface({ input: createReadStream(ROUNDS_CSV), crlfDelay: Infinity });
  let headers = null;
  for await (const line of rl) {
    if (!headers) {
      headers = parseCsvLine(line);
      continue;
    }
    const cols = parseCsvLine(line);
    const row = Object.fromEntries(headers.map((h, i) => [h, cols[i]]));
    const ck = normCourseNameKey(row.course_name);
    const evNorm = normEvent(row.event_name);
    const rd = Math.round(num(row.round_num, NaN));
    const rs = num(row.round_score, NaN);
    if (!ck || !evNorm || !Number.isFinite(rd) || !Number.isFinite(rs) || rs < 50) continue;

    if (!courseBaselines.has(ck)) courseBaselines.set(ck, { sum: 0, n: 0 });
    const b = courseBaselines.get(ck);
    b.sum += rs;
    b.n++;

    const er = `${evNorm}|${rd}`;
    if (!eventRounds.has(er)) eventRounds.set(er, { sum: 0, n: 0, courseKey: ck, eventNorm: evNorm, round: rd });
    const erRec = eventRounds.get(er);
    erRec.sum += rs;
    erRec.n++;
  }

  for (const [k, v] of courseBaselines) {
    courseBaselines.set(k, v.n ? v.sum / v.n : NaN);
  }
  for (const [k, v] of eventRounds) {
    eventRounds.set(k, { ...v, avg: v.n ? v.sum / v.n : NaN });
  }
  return { courseBaselines, eventRounds };
}

function roundPinProfile(holes) {
  const rule = roundAdjustmentsFromPinSheet(holes);
  const per = rule.perHole || [];
  const scores = per.map((h) => h.score).sort((a, b) => b - a);
  const meanAll = scores.length ? scores.reduce((a, b) => a + b, 0) / scores.length : 0;
  const topK = Math.min(5, scores.length);
  const meanHardest = topK ? scores.slice(0, topK).reduce((a, b) => a + b, 0) / topK : meanAll;
  return {
    avgDifficulty: rule.avgDifficulty,
    excess: rule.excess,
    ruleTotalDelta: rule.totalScoreDelta,
  };
}

function findSimilarRoundSamples(sheetCatalog, target, roundCtx, excludeKey) {
  const out = [];
  const baseline = roundCtx.courseBaselines.get(target.courseKey);
  if (!Number.isFinite(baseline)) return out;

  for (const s of sheetCatalog) {
    if (excludeKey && s.key === excludeKey) continue;
    const prof = s.profile || roundPinProfile(s.holes);
    const sameCourse = s.courseKey === target.courseKey;
    const diff = Math.abs(prof.avgDifficulty - target.profile.avgDifficulty);
    let tier = 0;
    if (sameCourse && diff <= 0.06) tier = 1;
    else if (sameCourse && diff <= 0.12) tier = 2;
    if (!tier) continue;

    const er = roundCtx.eventRounds.get(`${s.eventNorm}|${s.round}`);
    if (!er || !Number.isFinite(er.avg) || er.n < 30) continue;
    const vsBaseline = er.avg - baseline;
    out.push({
      vs_baseline: vsBaseline,
      n: er.n,
      weight: TIER_WEIGHT[tier],
      tier,
      play_date: s.playDate,
      course_key: s.courseKey,
    });
  }
  return out;
}

function buildPinSummaryBayesian(adj, cal) {
  const sign = adj.totalScoreDelta >= 0 ? "+" : "";
  const sameCourse = cal.same_course_hole_obs || 0;
  const similarRounds = cal.similar_round_obs || 0;
  const src =
    sameCourse > 0
      ? `Bayesian · ${sameCourse} same-course hole match(es)${similarRounds ? `, ${similarRounds} similar round(s)` : ""}`
      : cal.total_hole_obs > 0
        ? `Bayesian · ${cal.total_hole_obs} cross-course similar pin hole(s)`
        : "Rule-based pin geometry";
  const dir =
    adj.excess > 0.04 ? "Harder than typical" : adj.excess < -0.04 ? "Easier than typical" : "Near-average";
  return `${dir} · ${sign}${adj.totalScoreDelta.toFixed(2)} on projected total · ${src}`;
}

/**
 * @param {object} sheet — pin sheet with holes[], course_name, play_date, round
 * @param {{ observations?: object[], sheetCatalog?: object[], roundCtx?: object, index?: object }} [cached]
 */
export async function roundAdjustmentsFromPinSheetBayesian(sheet, cached = {}) {
  const rule = roundAdjustmentsFromPinSheet(sheet.holes);
  if (String(process.env.GOLF_PIN_SHEET_RULE_ONLY || "").trim() === "1") {
    return { ...rule, calibration: { mode: "rule_only" } };
  }

  const index = cached.index || (await loadPinHoleScoringIndex());
  const observations = cached.observations || index.observations || [];
  const courseKey = normCourseNameKey(sheet.course_key || sheet.course_name);
  const excludeKey = sheet.play_date && sheet.round ? `${courseKey}|${sheet.play_date}|${sheet.round}` : "";

  const holeTargets = sheet.holes
    .map((raw) => {
      const d = holePinDifficulty(raw);
      return {
        hole: d.hole,
        pinScore: d.score,
        front: raw.pin_from_front_yds,
        side: raw.pin_from_side_yds,
        depth: raw.green_depth_yds,
        hazard: Boolean(raw.near_hazard),
      };
    })
    .filter((h) => h.hole >= 1 && h.hole <= 18);

  const holeCal = holeTargets.map((t) => calibrateHole(t, observations, courseKey, excludeKey));

  const priorSum = holeCal.reduce((a, h) => a + h.priorVsPar, 0);
  const postSum = holeCal.reduce((a, h) => a + h.posteriorVsPar, 0);
  const holeStrokeShift = postSum - priorSum;

  const targetProfile = roundPinProfile(sheet.holes);
  const roundCtx = cached.roundCtx || (await loadRoundScoringContext());

  let sheetCatalog = cached.sheetCatalog;
  if (!sheetCatalog) {
    sheetCatalog = [];
    const pinRoot = path.join(MODEL_ROOT, "data", "pin_locations");
    const idxPath = path.join(pinRoot, "index.json");
    if (existsSync(idxPath)) {
      const { loadPinLocationSheetByPath, loadPinLocationsIndex } = await import("./pin-locations-db.mjs");
      for (const ent of loadPinLocationsIndex(pinRoot).entries || []) {
        const s = loadPinLocationSheetByPath(pinRoot, ent.path);
        if (!s?.holes?.length) continue;
        sheetCatalog.push({
          key: ent.key,
          courseKey: ent.course_key,
          playDate: ent.play_date,
          round: ent.round_num,
          eventNorm: normEvent(ent.event_name_ref || ""),
          holes: s.holes,
          profile: roundPinProfile(s.holes),
        });
      }
    }
  }

  const roundSamples = findSimilarRoundSamples(
    sheetCatalog,
    { courseKey, profile: targetProfile },
    roundCtx,
    excludeKey,
  );
  const roundPriorK = envNum("GOLF_PIN_BAYESIAN_ROUND_PRIOR_K", 12);
  const roundCourseK = envNum("GOLF_PIN_BAYESIAN_ROUND_COURSE_K", 5);
  const hasCourseRound = roundSamples.some((s) => s.tier <= 2);
  const roundK = hasCourseRound ? roundCourseK : roundPriorK;
  const baseline = roundCtx.courseBaselines.get(courseKey);
  const ruleRoundVsBaseline = Number.isFinite(baseline) ? rule.totalScoreDelta * 0.85 : rule.totalScoreDelta;
  const empiricalRoundDelta = bayesianMean(
    ruleRoundVsBaseline,
    roundK,
    roundSamples.map((s) => ({ vs_par: s.vs_baseline, n: s.n, weight: s.weight })),
  );

  const hasCourseHole = holeCal.some((h) => h.sameCourseObs > 0 && h.bestTier <= 2);

  let holeWeight = hasCourseHole ? envNum("GOLF_PIN_BAYESIAN_HOLE_BLEND", 0.5) : 0;
  let roundWeight = hasCourseRound ? envNum("GOLF_PIN_BAYESIAN_ROUND_BLEND", 0.35) : 0;
  if (!hasCourseHole && holeCal.some((h) => h.nObs > 0 && h.bestTier === 3)) {
    holeWeight = envNum("GOLF_PIN_BAYESIAN_CROSS_COURSE_HOLE_BLEND", 0.25);
  }
  const ruleWeight = Math.max(0, 1 - holeWeight - roundWeight);

  const cappedHoleShift = Math.max(-0.65, Math.min(0.65, holeStrokeShift));
  const cappedRoundDelta = Math.max(-0.55, Math.min(0.85, empiricalRoundDelta));

  const totalScoreDelta =
    rule.totalScoreDelta * ruleWeight +
    (rule.totalScoreDelta + cappedHoleShift) * holeWeight +
    cappedRoundDelta * roundWeight;

  const neutral = 0.28;
  const calibratedExcess = totalScoreDelta / 3.0;
  const adj = roundAdjustmentsFromExcess(calibratedExcess, {
    perHole: rule.perHole,
    avgDifficulty: targetProfile.avgDifficulty,
    neutral,
  });
  adj.totalScoreDelta = Math.round(totalScoreDelta * 100) / 100;

  const sameCourseHoleObs = holeCal.reduce((a, h) => a + h.sameCourseObs, 0);
  const totalObs = holeCal.reduce((a, h) => a + h.nObs, 0);

  const calibration = {
    mode: "bayesian",
    hole_stroke_shift: Math.round(cappedHoleShift * 1000) / 1000,
    hole_stroke_shift_raw: Math.round(holeStrokeShift * 1000) / 1000,
    rule_total_score_delta: rule.totalScoreDelta,
    empirical_round_delta: Math.round(cappedRoundDelta * 1000) / 1000,
    empirical_round_delta_raw: Math.round(empiricalRoundDelta * 1000) / 1000,
    blend: { rule: ruleWeight, hole: holeWeight, round: roundWeight },
    same_course_hole_obs: sameCourseHoleObs,
    similar_round_obs: roundSamples.length,
    total_hole_obs: totalObs,
    course_baseline_score: Number.isFinite(baseline) ? Math.round(baseline * 100) / 100 : null,
    holes: holeCal.map((h) => ({
      hole: h.hole,
      prior_vs_par: Math.round(h.priorVsPar * 1000) / 1000,
      posterior_vs_par: Math.round(h.posteriorVsPar * 1000) / 1000,
      n_obs: h.nObs,
      same_course_obs: h.sameCourseObs,
      best_tier: h.bestTier,
    })),
  };

  adj.summary = buildPinSummaryBayesian(adj, calibration);
  adj.calibration = calibration;
  adj.rule_adjustments = {
    total_score_delta: rule.totalScoreDelta,
    excess: rule.excess,
  };
  return adj;
}
