/**
 * Walk-forward full round projections (same pipeline as fetch:dg / export-round-projection-vs-actual).
 * Builds projections.json-shaped player rows from historical_rounds_all only (no lookahead).
 */
import { join } from "path";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  applyFieldDayCountingLiftNatural,
  applyVenueCourseFitToMu,
  blendTowardWithinEventActuals,
  buildPriorByStatForPlayer,
  buildWithinEventFormMap,
  blendedPriorRoundCourseExcess,
  courseDifficultyStrokeShift,
  fieldCountingMeansFromEventContext,
  fieldCountingMeansFromWithinEventMap,
  resolveProjectionCounts,
  resolveProjectionScoreToPar,
} from "./course-round-adjustments.mjs";
import { traditionalRate01 } from "./dg-traditional-stats.mjs";
import {
  RAW_ROUND_SD,
  N_FAIRWAY_HOLES,
  clampMuSg,
  derivedStatsFromMuSg,
  fieldSkillMedian,
  loadHistoricalCsvCalibration,
  parseRoundMuMult,
} from "./projection-core.mjs";
import {
  birdiesPlusEaglesFromRow,
  createProjectionContext,
  num,
  ouProjectedMeanForMode,
} from "./round-projection-mu.mjs";

function rowTimeMs(row) {
  const s = String(row?.event_completed || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) return Date.parse(`${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`);
  const yr = Math.round(num(row?.year, NaN));
  return Number.isFinite(yr) ? Date.parse(`${yr}-06-01T12:00:00Z`) : NaN;
}

function historySortKey(row) {
  const t = rowTimeMs(row);
  const rnd = Math.round(num(row.round_num, 1));
  if (Number.isFinite(t)) return Math.floor(t / 86400000) * 10 + rnd;
  const yr = Math.round(num(row.year, NaN));
  return yr * 1000 + rnd;
}

function histRoundToHistoryRec(row) {
  const rnd = Math.round(num(row.round_num, 1));
  const yr = Math.round(num(row.year, NaN));
  return {
    sortKey: historySortKey(row),
    year: yr,
    round_num: rnd,
    event_name: String(row.event_name || ""),
    course_name: String(row.course_name || ""),
    sg_total: num(row.sg_total, NaN),
    sg_ott: num(row.sg_ott, NaN),
    sg_app: num(row.sg_app, NaN),
    sg_arg: num(row.sg_arg, NaN),
    sg_putt: num(row.sg_putt, NaN),
    sg_t2g: num(row.sg_t2g, NaN),
    round_score: num(row.round_score, NaN),
    birdies: birdiesPlusEaglesFromRow(row),
    pars: num(row.pars, NaN),
    bogeys: num(row.bogeys ?? row.bogies, NaN),
    gir: num(row.gir, NaN),
    fairways: num(row.driving_acc, NaN),
    event_completed: String(row.event_completed || ""),
  };
}

export function buildWalkForwardHistoryByDgId(histRows, cutoffMs, dgIds) {
  /** @type {Record<string, { dg_id: number, player_name: string, rounds: object[] }>} */
  const out = {};
  const allow = dgIds instanceof Set ? dgIds : new Set(dgIds || []);

  for (const row of histRows) {
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg) || (allow.size && !allow.has(dg))) continue;
    const t = rowTimeMs(row);
    if (Number.isFinite(cutoffMs) && Number.isFinite(t) && t >= cutoffMs) continue;
    const tour = String(row.tour || "").toLowerCase();
    if (tour && tour !== "pga" && tour !== "liv") continue;
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(rs) || rs < 55 || rs > 95) continue;

    const key = String(dg);
    if (!out[key]) {
      out[key] = {
        dg_id: dg,
        player_name: String(row.player_name || ""),
        rounds: [],
      };
    }
    if (!out[key].player_name) out[key].player_name = String(row.player_name || "");
    out[key].rounds.push(histRoundToHistoryRec(row));
  }

  for (const rec of Object.values(out)) {
    rec.rounds.sort((a, b) => num(b.sortKey, 0) - num(a.sortKey, 0));
    if (rec.rounds.length > 80) rec.rounds = rec.rounds.slice(0, 80);
  }
  return out;
}

function recencyWeightedMean(rows, key, decay = 0.86) {
  let sum = 0;
  let wsum = 0;
  for (let i = 0; i < rows.length; i++) {
    const v = num(rows[i][key], NaN);
    if (!Number.isFinite(v)) continue;
    const w = decay ** i;
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

function skillRowFromHistory(rec) {
  const rounds = Array.isArray(rec?.rounds) ? rec.rounds : [];
  if (rounds.length < 3) return null;
  const sg_total = recencyWeightedMean(rounds, "sg_total");
  if (!Number.isFinite(sg_total)) return null;
  const sk = {
    sg_total,
    sg_ott: recencyWeightedMean(rounds, "sg_ott"),
    sg_app: recencyWeightedMean(rounds, "sg_app"),
    sg_arg: recencyWeightedMean(rounds, "sg_arg"),
    sg_putt: recencyWeightedMean(rounds, "sg_putt"),
    sg_t2g: recencyWeightedMean(rounds, "sg_t2g"),
  };
  const girR = recencyWeightedMean(
    rounds.map((r) => ({ v: traditionalRate01(r.gir, 18) })),
    "v",
  );
  const fwR = recencyWeightedMean(
    rounds.map((r) => ({ v: traditionalRate01(r.fairways, N_FAIRWAY_HOLES) })),
    "v",
  );
  if (Number.isFinite(girR)) sk.dg_gir_pct = girR;
  if (Number.isFinite(fwR)) sk.dg_fairway_pct = fwR;
  return sk;
}

function buildRollingTradFromHist(histRows, dgIds, cutoffMs) {
  /** @type {Map<number, { gir: number[], fw: number[] }>} */
  const buf = new Map();
  for (const row of histRows) {
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg) || !dgIds.has(dg)) continue;
    const t = rowTimeMs(row);
    if (Number.isFinite(cutoffMs) && Number.isFinite(t) && t >= cutoffMs) continue;
    let slot = buf.get(dg);
    if (!slot) {
      slot = { gir: [], fw: [] };
      buf.set(dg, slot);
    }
    const girR = traditionalRate01(row.gir, 18);
    if (Number.isFinite(girR) && slot.gir.length < 36) slot.gir.push(girR);
    const fwR = traditionalRate01(row.driving_acc, N_FAIRWAY_HOLES);
    if (Number.isFinite(fwR) && slot.fw.length < 36) slot.fw.push(fwR);
  }
  const mean = (a) => (a.length ? a.reduce((s, x) => s + x, 0) / a.length : NaN);
  /** @type {Map<number, { girRate01: number, fwRate01: number }>} */
  const out = new Map();
  for (const [id, slot] of buf) {
    const girRate01 = mean(slot.gir);
    const fwRate01 = mean(slot.fw);
    if (Number.isFinite(girRate01) || Number.isFinite(fwRate01)) out.set(id, { girRate01, fwRate01 });
  }
  return out;
}

function buildWithinEventCountingMap(histRows, eventName, eventYear, courseKey, targetRound, venueScoring) {
  /** @type {Map<number, Map<number, object>>} */
  const out = new Map();
  const cpDefault = 72;
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && Number.isFinite(yr) && yr !== eventYear) continue;
    if (courseKey) {
      const ck = normCourseNameKey(row.course_name || "");
      if (ck && ck !== courseKey) continue;
    }
    const rnd = Math.round(num(row.round_num, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd >= targetRound) continue;
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const rec = {
      birdies: birdiesPlusEaglesFromRow(row),
      bogeys: num(row.bogeys ?? row.bogies, NaN),
      gir: num(row.gir, NaN),
      round_score: num(row.round_score, NaN),
    };
    let per = out.get(dg);
    if (!per) {
      per = new Map();
      out.set(dg, per);
    }
    per.set(rnd, rec);
  }
  return out;
}

function buildEventContextFromHist(histRows, eventName, eventYear, courseKey, targetRound) {
  const ctx = { byRound: new Map(), playerRounds: [] };
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && Number.isFinite(yr) && yr !== eventYear) continue;
    if (courseKey) {
      const ck = normCourseNameKey(row.course_name || "");
      if (ck && ck !== courseKey) continue;
    }
    const rnd = Math.round(num(row.round_num, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd >= targetRound) continue;
    const cp = num(row.course_par, NaN);
    const rs = num(row.round_score, NaN);
    if (Number.isFinite(cp) && Number.isFinite(rs)) {
      const stp = rs - cp;
      const b = ctx.byRound.get(rnd) || { n: 0, sumStp: 0, sumBird: 0, nBird: 0, sumBog: 0, nBog: 0, sumGir: 0, nGir: 0 };
      b.n++;
      b.sumStp += stp;
      const bird = birdiesPlusEaglesFromRow(row);
      if (Number.isFinite(bird)) {
        b.sumBird += bird;
        b.nBird++;
      }
      const bog = num(row.bogeys ?? row.bogies, NaN);
      if (Number.isFinite(bog)) {
        b.sumBog += bog;
        b.nBog++;
      }
      ctx.byRound.set(rnd, b);
    }
    const dg = Math.round(num(row.dg_id, NaN));
    const sg = num(row.sg_total, NaN);
    if (Number.isFinite(dg) && Number.isFinite(sg)) {
      ctx.playerRounds.push({ dg_id: dg, round: rnd, sg_total: sg });
    }
  }
  return ctx;
}

async function loadVenueScoringBeforeCutoff(histRows, courseKey, courseLabel, cutoffMs, eventName, eventYear, targetRound) {
  if (!courseKey) {
    return {
      venueAvgStp: NaN,
      nVenueRounds: 0,
      source: "none",
      fieldByRound: new Map(),
      playerByRound: new Map(),
      playerByVenue: new Map(),
      courseFitByDg: new Map(),
    };
  }

  let venueTotals = { n: 0, sumStp: 0, sumScore: 0, sumBird: 0, nBird: 0, sumBog: 0, nBog: 0 };
  const fieldRaw = new Map();
  const playerRaw = new Map();
  const playerAllRaw = new Map();
  const fitRaw = new Map();

  function accAgg(agg, row) {
    const cp = num(row.course_par, NaN);
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(cp) || !Number.isFinite(rs)) return agg;
    agg.n++;
    agg.sumScore += rs;
    agg.sumStp += rs - cp;
    const bird = birdiesPlusEaglesFromRow(row);
    if (Number.isFinite(bird)) {
      agg.sumBird += bird;
      agg.nBird++;
    }
    const bog = num(row.bogeys ?? row.bogies, NaN);
    if (Number.isFinite(bog)) {
      agg.sumBog += bog;
      agg.nBog++;
    }
    return agg;
  }

  for (const row of histRows) {
    const ckRow = normCourseNameKey(row.course_name || "");
    if (!ckRow || ckRow !== courseKey) continue;
    const t = rowTimeMs(row);
    if (Number.isFinite(cutoffMs) && Number.isFinite(t) && t >= cutoffMs) continue;
    if (eventsLikelySame(eventName, String(row.event_name || "").trim())) {
      const yr = Math.round(num(row.year, NaN));
      const rnd = Math.round(num(row.round_num, NaN));
      if (Number.isFinite(eventYear) && yr === eventYear && Number.isFinite(rnd) && rnd >= targetRound) continue;
    }
    const cp = num(row.course_par, NaN);
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(cp) || cp < 63 || cp > 76) continue;
    if (!Number.isFinite(rs) || rs < 55 || rs > 95) continue;
    const rnd = Math.round(num(row.round_num, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;

    venueTotals = accAgg(venueTotals, row);
    fieldRaw.set(rnd, accAgg(fieldRaw.get(rnd) || { n: 0, sumStp: 0, sumScore: 0, sumBird: 0, nBird: 0, sumBog: 0, nBog: 0 }, row));

    const dg = Math.round(num(row.dg_id, NaN));
    if (Number.isFinite(dg)) {
      const pk = `${dg}|${rnd}`;
      playerRaw.set(pk, accAgg(playerRaw.get(pk) || { n: 0, sumStp: 0, sumScore: 0, sumBird: 0, nBird: 0, sumBog: 0, nBog: 0 }, row));
      playerAllRaw.set(dg, accAgg(playerAllRaw.get(dg) || { n: 0, sumStp: 0, sumScore: 0, sumBird: 0, nBird: 0, sumBog: 0, nBog: 0 }, row));
      const sg = num(row.sg_total, NaN);
      if (Number.isFinite(sg)) {
        const cf = fitRaw.get(dg) || { sumSg: 0, n: 0 };
        cf.sumSg += sg;
        cf.n++;
        fitRaw.set(dg, cf);
      }
    }
  }

  const finalize = (raw) => ({
    n: raw.n,
    avgStp: raw.n ? raw.sumStp / raw.n : NaN,
    avgScore: raw.n ? raw.sumScore / raw.n : NaN,
    avgBirdies: raw.nBird ? raw.sumBird / raw.nBird : NaN,
    avgBogeys: raw.nBog ? raw.sumBog / raw.nBog : NaN,
  });

  const venueAgg = finalize(venueTotals);
  const fieldByRound = new Map();
  for (const [rnd, raw] of fieldRaw) fieldByRound.set(rnd, finalize(raw));
  const playerByRound = new Map();
  for (const [pk, raw] of playerRaw) playerByRound.set(pk, finalize(raw));
  const playerByVenue = new Map();
  for (const [dg, raw] of playerAllRaw) playerByVenue.set(dg, finalize(raw));
  const courseFitByDg = new Map();
  for (const [dg, raw] of fitRaw) courseFitByDg.set(dg, { avgSg: raw.sumSg / raw.n, n: raw.n });

  return {
    venueAvgStp: venueAgg.n >= 20 ? venueAgg.avgStp : NaN,
    venueAvgScore: venueAgg.n >= 20 ? venueAgg.avgScore : NaN,
    nVenueRounds: venueAgg.n,
    source: venueAgg.n >= 20 ? "historical_csv_walkforward" : "none",
    venueAvgBirdies: venueAgg.avgBirdies,
    venueAvgBogeys: venueAgg.avgBogeys,
    fieldByRound,
    playerByRound,
    playerByVenue,
    courseFitByDg,
  };
}

/** @type {Map<string, Promise<object>>} */
const histCalibCache = new Map();

async function loadHistoricalCsvCalibrationCached(repoRoot, courseKey) {
  const key = courseKey || "__all__";
  if (!histCalibCache.has(key)) {
    const prevLog = console.log;
    console.log = (...args) => {
      if (!String(args[0] || "").includes("[fetch-dg] historical calibration")) prevLog(...args);
    };
    histCalibCache.set(key, loadHistoricalCsvCalibration(repoRoot, courseKey).finally(() => {
      console.log = prevLog;
    }));
  }
  return histCalibCache.get(key);
}

function inferCoursePar(histRows, eventName, eventYear, courseKey) {
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && yr !== eventYear) continue;
    if (courseKey && normCourseNameKey(row.course_name || "") !== courseKey) continue;
    const cp = num(row.course_par, NaN);
    if (Number.isFinite(cp) && cp >= 63 && cp <= 76) return cp;
  }
  return 72;
}

function inferCourseName(histRows, eventName, eventYear) {
  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN));
    if (Number.isFinite(eventYear) && yr !== eventYear) continue;
    const c = String(row.course_name || "").trim();
    if (c) return c;
  }
  return "";
}

/**
 * Build full-model μ for one player×round (default pricing mode — same as live export).
 * @returns {Promise<Map<string, number>>} market label -> μ
 */
export async function buildFullModelMuMapForEvent({
  repoRoot,
  histRows,
  eventName,
  eventYear,
  targetRound,
  betTimeMs,
  fieldDgIds,
}) {
  const dgSet = new Set(fieldDgIds.filter((d) => Number.isFinite(d)));
  if (!dgSet.size) return new Map();

  const courseName = inferCourseName(histRows, eventName, eventYear);
  const courseKey = normCourseNameKey(courseName);
  const coursePar18 = inferCoursePar(histRows, eventName, eventYear, courseKey);
  const fairwayHoles = N_FAIRWAY_HOLES;

  const historyByDgId = buildWalkForwardHistoryByDgId(histRows, betTimeMs, dgSet);
  const rollingTrad = buildRollingTradFromHist(histRows, dgSet, betTimeMs);

  const [histCalib, venueScoring] = await Promise.all([
    loadHistoricalCsvCalibrationCached(repoRoot, courseKey),
    loadVenueScoringBeforeCutoff(histRows, courseKey, courseName, betTimeMs, eventName, eventYear, targetRound),
  ]);

  const histEventCtx = buildEventContextFromHist(histRows, eventName, eventYear, courseKey, targetRound);
  const withinEventCountingMap = buildWithinEventCountingMap(
    histRows,
    eventName,
    eventYear,
    courseKey,
    targetRound,
    venueScoring,
  );

  const base = [];
  for (const dg of dgSet) {
    const rec = historyByDgId[String(dg)];
    const skRow = skillRowFromHistory(rec);
    if (!skRow) continue;
    let mu_sg = skRow.sg_total;
    const liveTrad = rollingTrad.get(dg) || null;
    if (liveTrad) {
      if (Number.isFinite(liveTrad.girRate01)) skRow.dg_gir_pct = liveTrad.girRate01;
      if (Number.isFinite(liveTrad.fwRate01)) skRow.dg_fairway_pct = liveTrad.fwRate01;
    }
    const im = derivedStatsFromMuSg(mu_sg, fairwayHoles, {
      histCountFit: histCalib,
      skRow,
      liveTrad,
      fieldMeanApp: NaN,
      fieldMeanOtt: NaN,
      sg_ott: skRow.sg_ott,
      sg_app: skRow.sg_app,
    });
    base.push({
      dg_id: dg,
      player_name: rec?.player_name || "",
      mu_sg,
      implied_mu_sg: mu_sg,
      ...skRow,
      eagles: im.eagles,
      birdies: im.birdies,
      pars: im.pars,
      bogeys: im.bogeys,
      doubles: im.doubles,
      gir: im.gir,
      fairways: im.fairways,
      putts: im.putts,
    });
  }

  if (!base.length) return new Map();

  const fieldMeanMu = base.reduce((s, r) => s + num(r.mu_sg, 0), 0) / base.length;
  for (const row of base) {
    row.mu_sg = applyVenueCourseFitToMu(row.mu_sg, row.dg_id, venueScoring, fieldMeanMu);
    row.implied_mu_sg = row.mu_sg;
  }
  const fieldMeanMuAdj = base.reduce((s, r) => s + num(r.mu_sg, 0), 0) / base.length;

  const ottSamples = base.map((r) => num(r.sg_ott, NaN)).filter(Number.isFinite);
  const appSamples = base.map((r) => num(r.sg_app, NaN)).filter(Number.isFinite);
  const fieldMeanOtt = fieldSkillMedian(ottSamples);
  const fieldMeanApp = fieldSkillMedian(appSamples);

  const formK = num(process.env.GOLF_WITHIN_EVENT_FORM_CARRY, 0.1);
  const withinFormMap =
    formK !== 0 && histEventCtx.playerRounds.length
      ? buildWithinEventFormMap(
          histEventCtx,
          base.map((r) => ({ dg_id: r.dg_id, mu_sg: r.mu_sg })),
          formK,
          num(process.env.GOLF_WITHIN_EVENT_FORM_CAP, 0.75),
        )
      : new Map();

  const priorExcess = blendedPriorRoundCourseExcess(null, histEventCtx, targetRound, eventName, courseKey);
  const strokeShiftPrior = Number.isFinite(priorExcess) ? courseDifficultyStrokeShift(priorExcess) : 0;

  const fieldCountingFromEvent = fieldCountingMeansFromEventContext(histEventCtx);
  const fieldCountingFromHistory =
    withinEventCountingMap.size > 0 ? fieldCountingMeansFromWithinEventMap(withinEventCountingMap) : null;
  const fieldCountingMeans = fieldCountingFromEvent?.birdies?.[1]
    ? fieldCountingFromEvent
    : fieldCountingFromHistory?.birdies?.[1]
      ? fieldCountingFromHistory
      : fieldCountingFromEvent || fieldCountingFromHistory;

  const roundMuMult = parseRoundMuMult();
  const mult = num(roundMuMult[targetRound - 1], 1);
  const players = [];

  for (const row of base) {
    const formShift = formK !== 0 ? num(withinFormMap.get(`${row.dg_id}|${targetRound}`), 0) : 0;
    let muForRound = targetRound === 1 ? row.mu_sg : row.mu_sg * mult;
    muForRound = clampMuSg(muForRound - strokeShiftPrior + formShift);

    const skRowR = {
      sg_total: row.sg_total,
      sg_ott: row.sg_ott,
      sg_app: row.sg_app,
      sg_arg: row.sg_arg,
      sg_putt: row.sg_putt,
      sg_t2g: row.sg_t2g,
      dg_gir_pct: row.dg_gir_pct,
      dg_fairway_pct: row.dg_fairway_pct,
    };
    const liveTrad = rollingTrad.get(row.dg_id) || null;

    let st =
      targetRound === 1 && strokeShiftPrior === 0 && formShift === 0
        ? {
            mu_sg: row.mu_sg,
            implied_mu_sg: row.implied_mu_sg,
            eagles: row.eagles,
            birdies: row.birdies,
            pars: row.pars,
            bogeys: row.bogeys,
            doubles: row.doubles,
            gir: row.gir,
            fairways: row.fairways,
            putts: row.putts,
          }
        : derivedStatsFromMuSg(muForRound, fairwayHoles, {
            histCountFit: histCalib,
            skRow: skRowR,
            liveTrad,
            fieldMeanOtt,
            fieldMeanApp,
            sg_ott: row.sg_ott,
            sg_app: row.sg_app,
            nGirHoles: 18,
          });

    const scoreRes = resolveProjectionScoreToPar({
      dg_id: row.dg_id,
      round: targetRound,
      muForRound,
      course_par_18: coursePar18,
      venueScoring,
      pretRoundScore: NaN,
      fieldMeanMu: fieldMeanMuAdj,
    });
    const stp = scoreRes.stp;
    const ts = coursePar18 + stp;

    const venueCounts = resolveProjectionCounts({
      dg_id: row.dg_id,
      round: targetRound,
      muForRound,
      skillCounts: {
        eagles: st.eagles,
        birdies: st.birdies,
        pars: st.pars,
        bogeys: st.bogeys,
        doubles: st.doubles,
        gir: st.gir,
        fairways: st.fairways,
        putts: st.putts,
      },
      venueScoring,
      targetStp: stp,
      nFairwayHoles: fairwayHoles,
    });
    st.eagles = venueCounts.eagles;
    st.birdies = venueCounts.birdies;
    st.pars = venueCounts.pars;
    st.bogeys = venueCounts.bogeys;
    st.doubles = venueCounts.doubles;
    st.gir = venueCounts.gir;
    st.fairways = venueCounts.fairways;
    st.putts = venueCounts.putts;

    if (targetRound >= 2 && withinEventCountingMap.size) {
      const priorByStat = buildPriorByStatForPlayer(withinEventCountingMap, row.dg_id, targetRound);
      if (priorByStat) {
        const skillBeforeBlend = { ...st };
        const blended = blendTowardWithinEventActuals({ ...skillBeforeBlend }, priorByStat, targetRound, {
          playerRow: row,
          skillCounts: skillBeforeBlend,
          fieldMeans: fieldCountingMeans,
        });
        Object.assign(st, blended);
      }
    }
    if (targetRound >= 2 && fieldCountingMeans) {
      applyFieldDayCountingLiftNatural(st, targetRound, fieldCountingMeans, venueScoring);
    }

    players.push({
      dg_id: row.dg_id,
      player_name: row.player_name,
      round: targetRound,
      mu_sg: Math.round(st.mu_sg * 1000) / 1000,
      total_score: Math.round(ts * 100) / 100,
      score_to_par: Math.round(stp * 100) / 100,
      score_source: scoreRes.source,
      round_sd: RAW_ROUND_SD,
      eagles: Math.round(st.eagles * 1000) / 1000,
      birdies: Math.round(st.birdies * 100) / 100,
      pars: Math.round(st.pars * 100) / 100,
      bogeys: Math.round(st.bogeys * 100) / 100,
      gir: Math.round(st.gir * 100) / 100,
      fairways: Math.round(st.fairways * 100) / 100,
      putts: Math.round(st.putts * 100) / 100,
      sg_total: row.sg_total,
      sg_ott: row.sg_ott,
      sg_app: row.sg_app,
      sg_arg: row.sg_arg,
      sg_putt: row.sg_putt,
      sg_t2g: row.sg_t2g,
    });
  }

  const meta = {
    display_round: targetRound,
    course_used: courseName,
    course_par_18: coursePar18,
    projection_course_basis: { fairway_holes_modeled: fairwayHoles },
    projection_counts_weather_baked: false,
  };

  const payload = {
    meta,
    course_used: courseName,
    players,
    _webRoot: join(repoRoot, "alpha-caddie-web"),
  };
  payload.meta.historyByDgId = historyByDgId;
  const ctx = createProjectionContext(payload);
  ctx.historyByDgId = historyByDgId;
  ctx.modelRound = targetRound;

  /** @type {Map<number, Map<string, number>>} dg -> market -> mu */
  const byDg = new Map();
  const ALL_MARKETS = ["Total score", "Birdies", "Pars", "Bogeys", "GIR", "Fairways hit"];
  for (const pl of players) {
    const dg = Math.round(num(pl.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const mus = new Map();
    mus.set("Total score", pl.total_score);
    mus.set("Birdies", num(pl.birdies, NaN) + num(pl.eagles, 0));
    mus.set("Pars", pl.pars);
    mus.set("Bogeys", pl.bogeys);
    mus.set("GIR", pl.gir);
    mus.set("Fairways hit", pl.fairways);
    for (const market of ALL_MARKETS) {
      const mu = ouProjectedMeanForMode(market, pl, meta, "default", "default", ctx);
      if (Number.isFinite(mu)) mus.set(market, mu);
    }
    byDg.set(dg, mus);
  }
  return byDg;
}

/** Cache full-model μ lookups across odds props (keyed by event×year×round×betTime). */
export class FullModelProjectionCache {
  constructor(repoRoot, histRows) {
    this.repoRoot = repoRoot;
    this.histRows = histRows;
    /** @type {Map<string, Map<number, Map<string, number>>>} */
    this.cache = new Map();
  }

  eventKey(p) {
    return `${p.year}|${foldComparableTitle(p.event)}|${p.round}`;
  }

  async ensureEvent(p) {
    const key = this.eventKey(p);
    if (this.cache.has(key)) return this.cache.get(key);
    const map = await buildFullModelMuMapForEvent({
      repoRoot: this.repoRoot,
      histRows: this.histRows,
      eventName: p.event,
      eventYear: p.year,
      targetRound: p.round,
      betTimeMs: p.bet_time_ms,
      fieldDgIds: p._field_dg_ids || (p.dg_id ? [Math.round(p.dg_id)] : []),
    });
    this.cache.set(key, map);
    return map;
  }

  async prewarm(props) {
    const seen = new Set();
    const keys = [];
    for (const p of props.values()) {
      const k = this.eventKey(p);
      if (seen.has(k)) continue;
      seen.add(k);
      keys.push(p);
    }
    console.log(`  ${keys.length} unique event×round bundles …`);
    let i = 0;
    for (const p of keys) {
      await this.ensureEvent(p);
      i++;
      if (i % 10 === 0 || i === keys.length) process.stdout.write(`\r  Projections ${i}/${keys.length}`);
    }
    process.stdout.write("\n");
  }

  async muForProp(p, dgId, marketLabel) {
    if (!Number.isFinite(dgId)) return NaN;
    const map = await this.ensureEvent(p);
    return map.get(Math.round(dgId))?.get(marketLabel) ?? NaN;
  }
}

export function attachFieldDgIdsToProps(props, histRows) {
  /** @type {Map<string, Set<number>>} */
  const fields = new Map();
  for (const row of histRows) {
    const yr = Math.round(num(row.year, NaN));
    const rnd = Math.round(num(row.round_num, NaN));
    const ev = String(row.event_name || "").trim();
    const dg = Math.round(num(row.dg_id, NaN));
    if (!ev || !Number.isFinite(yr) || !Number.isFinite(rnd) || !Number.isFinite(dg)) continue;
    const k = `${yr}|${foldComparableTitle(ev)}|${rnd}`;
    if (!fields.has(k)) fields.set(k, new Set());
    fields.get(k).add(dg);
  }
  for (const p of props.values()) {
    const k = `${p.year}|${foldComparableTitle(p.event)}|${p.round}`;
    let ids = fields.get(k);
    if (!ids?.size) {
      for (const [fk, set] of fields.entries()) {
        const [y, ev, rnd] = fk.split("|");
        if (Number(y) === p.year && Number(rnd) === p.round && eventsLikelySame(p.event, ev.replace(/-/g, " "))) {
          ids = set;
          break;
        }
      }
    }
    p._field_dg_ids = ids ? [...ids] : p.dg_id ? [Math.round(p.dg_id)] : [];
  }
}
