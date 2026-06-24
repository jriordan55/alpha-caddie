/**
 * Round-projection μ / O-U helpers for export (mirrors app.js Round projections logic).
 * Used by export-round-projection-vs-actual-csv.mjs — not loaded in the browser.
 */
import { readFileSync, existsSync } from "fs";
import { join } from "path";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  statWeatherMuAdjustment,
} from "./weather-projection-adjustments.mjs";
import { marketBookSigmaScale } from "./market-book-calibration.mjs";
import {
  adaptiveVenueStatMuNudge,
  blendAdaptiveMuSgBonus,
  courseHistoryMuBonus,
  courseWeightedMarketMuNudge,
  courseWeightedSkillFormBonus,
  isAdaptivePricingMode,
  recentFormMuBonus,
  resolveCourseTableForVenue,
} from "./course-adaptive-pricing.mjs";

export const EXPORT_MARKETS = [
  {
    key: "fairways",
    market: "Fairways hit",
    propsMarket: "Fairways hit",
    lineCol: "fairways_line",
    bookLineCol: "fairways_book_line",
    overOddsCol: "fairways_over_odds",
    underOddsCol: "fairways_under_odds",
    actualCol: "actual_fairways",
    overCol: "fairways_over",
    underCol: "fairways_under",
  },
  {
    key: "total",
    market: "Total score",
    propsMarket: "Total Score",
    lineCol: "round_score_line",
    bookLineCol: "round_score_book_line",
    overOddsCol: "round_score_over_odds",
    underOddsCol: "round_score_under_odds",
    actualCol: "actual_round_score",
    overCol: "round_score_over",
    underCol: "round_score_under",
  },
  {
    key: "birdies",
    market: "Birdies",
    propsMarket: "Birdies",
    lineCol: "birdies_line",
    bookLineCol: "birdies_book_line",
    overOddsCol: "birdies_over_odds",
    underOddsCol: "birdies_under_odds",
    actualCol: "actual_birdies",
    overCol: "birdies_over",
    underCol: "birdies_under",
  },
  {
    key: "gir",
    market: "GIR",
    propsMarket: "GIR",
    lineCol: "gir_line",
    bookLineCol: "gir_book_line",
    overOddsCol: "gir_over_odds",
    underOddsCol: "gir_under_odds",
    actualCol: "actual_gir",
    overCol: "gir_over",
    underCol: "gir_under",
  },
];

export const EXPORT_ACTUAL_COLS = EXPORT_MARKETS.map((m) => m.actualCol).concat(["actual_source"]);
export const EXPORT_MODEL_LINE_COLS = EXPORT_MARKETS.map((m) => m.lineCol);
export const EXPORT_BOOK_LINE_COLS = EXPORT_MARKETS.map((m) => m.bookLineCol);
export const EXPORT_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.overOddsCol);
export const EXPORT_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.underOddsCol);
export const EXPORT_OVER_RESULT_COLS = EXPORT_MARKETS.map((m) => m.overCol);
export const EXPORT_UNDER_RESULT_COLS = EXPORT_MARKETS.map((m) => m.underCol);

/** Single projection path: recent form + course history + course-weighted SG. */
export const EXPORT_PRICING_MODES = [{ mode: "default", skill: "default" }];

const OU_STAT_MAP = {
  "Total score": { field: "total_score", sdKey: "round_sd" },
  Birdies: { field: "birdies", sdKey: null },
  Pars: { field: "pars", sdKey: null },
  Bogeys: { field: "bogeys", sdKey: null },
  GIR: { field: "gir", sdKey: null },
  "Fairways hit": { field: "fairways", sdKey: null },
};

const PRICING_SKILL_COLUMNS = ["sg_total", "sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_t2g"];

export function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

export function enforceHalfLine(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x)) return NaN;
  return Math.round(x * 2) / 2;
}

function normalCdf(z) {
  const t = 1 / (1 + 0.2316419 * Math.abs(z));
  const d = 0.3989423 * Math.exp((-z * z) / 2);
  const p =
    d *
    t *
    (0.3193815 +
      t * (-0.3565638 + t * (1.7814779 + t * (-1.821256 + t * 1.3302744))));
  return z >= 0 ? 1 - p : p;
}

export function birdiesPlusEaglesFromRow(row) {
  if (!row || typeof row !== "object") return NaN;
  const b = num(row.birdies, NaN);
  const eob = num(row.eagles_or_better, NaN);
  const eg = num(row.eagles, NaN);
  const eagleAdd = Number.isFinite(eob) ? eob : Number.isFinite(eg) ? eg : 0;
  if (!Number.isFinite(b) && !Number.isFinite(eob) && !Number.isFinite(eg)) return NaN;
  return (Number.isFinite(b) ? b : 0) + eagleAdd;
}

function girFairwaysCountFromRaw(v, holes) {
  const n = num(v, NaN);
  if (!Number.isFinite(n)) return NaN;
  if (n > 0 && n <= 1.0001) return Math.min(holes, Math.max(0, Math.round(n * holes)));
  if (n > 1.0001 && n <= holes + 1e-6) return Math.min(holes, Math.max(0, n));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

function courseNameMatchesVenue(courseRaw, venueRaw) {
  const ck = normCourseNameKey(courseRaw);
  const vk = normCourseNameKey(venueRaw);
  if (!ck || !vk) return false;
  return ck.includes(vk) || vk.includes(ck);
}

function historyRoundChronoKey(r) {
  const sk = num(r?.sortKey, NaN);
  if (Number.isFinite(sk)) return sk;
  const y = num(r?.year, 2000);
  const rn = num(r?.round_num, 1);
  return y * 1000 + rn;
}

function meanNumFromRounds(rounds, key) {
  const vals = [];
  for (const r of rounds) {
    const v = num(r[key], NaN);
    if (Number.isFinite(v)) vals.push(v);
  }
  return vals.length ? vals.reduce((a, b) => a + b, 0) / vals.length : NaN;
}

function meanNumFromRoundsRecencyWeighted(rounds, key, decay = 0.86) {
  if (!rounds?.length) return NaN;
  let sum = 0;
  let wsum = 0;
  for (let i = 0; i < rounds.length; i++) {
    const v = num(rounds[i][key], NaN);
    if (!Number.isFinite(v)) continue;
    const w = decay ** i;
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

function actualBirdiesFromHistoryRow(r) {
  const b = birdiesPlusEaglesFromRow(r);
  return Number.isFinite(b) ? b : NaN;
}

function meanNumFromRoundsRecencyWeightedStat(rounds, statKey, decay = 0.86) {
  if (!rounds?.length) return NaN;
  let sum = 0;
  let wsum = 0;
  for (let i = 0; i < rounds.length; i++) {
    let v = NaN;
    const row = rounds[i];
    if (statKey === "total") v = num(row.round_score, NaN);
    else if (statKey === "birdies") v = actualBirdiesFromHistoryRow(row);
    else if (statKey === "pars") v = num(row.pars, NaN);
    else if (statKey === "bogeys") v = num(row.bogies ?? row.bogeys, NaN);
    else if (statKey === "gir") v = num(row.gir, NaN);
    else if (statKey === "fairways") v = num(row.fairways, NaN);
    else if (statKey === "putts") v = num(row.putts, NaN);
    if (!Number.isFinite(v)) continue;
    const w = decay ** i;
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

function pricingSkillColumnKeyFromRaw(skillRaw) {
  const skRaw = String(skillRaw || "sg_total").toLowerCase();
  return skRaw === "default" ? "sg_total" : PRICING_SKILL_COLUMNS.includes(skRaw) ? skRaw : "sg_total";
}

function projectionSkillFocusNudgeFromField(dgId, skillKey, players, modelRound) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id) || !PRICING_SKILL_COLUMNS.includes(skillKey)) return 0;
  const row = players.find((p) => Math.round(num(p.dg_id)) === id && Math.round(num(p.round)) === modelRound);
  if (!row) return 0;
  const v = num(row[skillKey], NaN);
  if (!Number.isFinite(v)) return 0;
  const vals = [];
  for (const p of players) {
    if (Math.round(num(p.round)) !== modelRound) continue;
    const x = num(p[skillKey], NaN);
    if (Number.isFinite(x)) vals.push(x);
  }
  if (vals.length < 8) return 0;
  vals.sort((a, b) => a - b);
  const mid = Math.floor(vals.length / 2);
  const median = vals.length % 2 === 1 ? vals[mid] : (vals[mid - 1] + vals[mid]) / 2;
  return clamp((v - median) * 0.12, -0.22, 0.22);
}

function pricingModeMuSgBonusForMode(dgId, modeRaw, skillRaw, ctx) {
  const { historyByDgId, venueName, players, modelRound } = ctx;
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return 0;
  const mode = ["default", "recent", "course", "skill"].includes(String(modeRaw || "").toLowerCase())
    ? String(modeRaw || "").toLowerCase()
    : "default";
  const skillKey = String(skillRaw || "default").toLowerCase();
  const cacheKey = `${id}|${mode}|${skillKey}|${normCourseNameKey(venueName)}`;
  if (ctx.bonusCache.has(cacheKey)) return ctx.bonusCache.get(cacheKey);

  const rec = historyByDgId[String(id)];

  if (isAdaptivePricingMode(mode)) {
    const rounds = Array.isArray(rec?.rounds)
      ? rec.rounds.slice().sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a))
      : [];
    const ctRow = ctx.ctRow ?? resolveCourseTableForVenue(venueName);
    const playerRow = players.find(
      (p) => Math.round(num(p.dg_id)) === id && Math.round(num(p.round)) === modelRound,
    );
    let recent = 0;
    let course = 0;
    let skill = 0;
    let venueRounds = 0;
    if (rounds.length >= 4) {
      recent = recentFormMuBonus(rounds);
      const ch = courseHistoryMuBonus(rounds, venueName, courseNameMatchesVenue);
      course = ch.bonus;
      venueRounds = ch.venueRounds;
      skill = courseWeightedSkillFormBonus(rounds, ctRow, playerRow, players, modelRound);
    } else if (playerRow) {
      skill = courseWeightedSkillFormBonus([], ctRow, playerRow, players, modelRound);
    }
    const out = blendAdaptiveMuSgBonus(recent, course, skill, venueRounds);
    ctx.bonusCache.set(cacheKey, out);
    return out;
  }

  const rounds = Array.isArray(rec?.rounds)
    ? rec.rounds.slice().sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a))
    : [];
  if (rounds.length < 4) {
    if (mode === "skill") {
      const sk0 = pricingSkillColumnKeyFromRaw(skillRaw);
      const fb0 = projectionSkillFocusNudgeFromField(id, sk0, players, modelRound);
      ctx.bonusCache.set(cacheKey, fb0);
      return fb0;
    }
    ctx.bonusCache.set(cacheKey, 0);
    return 0;
  }

  if (mode === "recent") {
    const nRec = Math.min(6, Math.max(3, Math.floor(rounds.length / 2)));
    const recent = rounds.slice(0, nRec);
    const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 18));
    let rMean = meanNumFromRounds(recent, "sg_total");
    let oMean = meanNumFromRounds(older, "sg_total");
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      const out = clamp((rMean - oMean) * 0.9, -0.35, 0.35);
      ctx.bonusCache.set(cacheKey, out);
      return out;
    }
    rMean = meanNumFromRounds(recent, "round_score");
    oMean = meanNumFromRounds(older, "round_score");
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      const out = clamp(((oMean - rMean) / 6) * 0.85, -0.35, 0.35);
      ctx.bonusCache.set(cacheKey, out);
      return out;
    }
    ctx.bonusCache.set(cacheKey, 0);
    return 0;
  }

  if (mode === "course") {
    const vn = venueName;
    if (!vn) {
      ctx.bonusCache.set(cacheKey, 0);
      return 0;
    }
    const here = rounds.filter((r) => courseNameMatchesVenue(r.course_name, vn));
    if (here.length < 2) {
      ctx.bonusCache.set(cacheKey, 0);
      return 0;
    }
    const other = rounds.filter((r) => !courseNameMatchesVenue(r.course_name, vn));
    const hMean = meanNumFromRoundsRecencyWeighted(here, "sg_total", 0.84);
    const oMean = meanNumFromRoundsRecencyWeighted(other.length ? other : rounds, "sg_total", 0.9);
    if (Number.isFinite(hMean) && Number.isFinite(oMean)) {
      const out = clamp((hMean - oMean) * 1.05, -0.42, 0.42);
      ctx.bonusCache.set(cacheKey, out);
      return out;
    }
    ctx.bonusCache.set(cacheKey, 0);
    return 0;
  }

  if (mode === "skill") {
    const sk = pricingSkillColumnKeyFromRaw(skillRaw);
    const nRec = Math.min(8, Math.max(3, Math.floor(rounds.length / 2)));
    const recent = rounds.slice(0, nRec);
    const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 24));
    const rMean = meanNumFromRounds(recent, sk);
    const oMean = meanNumFromRounds(older, sk);
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      const out = clamp((rMean - oMean) * 0.75, -0.35, 0.35);
      ctx.bonusCache.set(cacheKey, out);
      return out;
    }
    const fb = projectionSkillFocusNudgeFromField(id, sk, players, modelRound);
    ctx.bonusCache.set(cacheKey, fb);
    return fb;
  }

  ctx.bonusCache.set(cacheKey, 0);
  return 0;
}

function pricingCourseVenueStatMuNudge(market, dgId, modeRaw, ctx) {
  const id = Math.round(num(dgId, NaN));
  const vn = ctx.venueName;
  if (!Number.isFinite(id) || !vn) return 0;
  if (!isAdaptivePricingMode(modeRaw) && String(modeRaw || "").toLowerCase() !== "course") return 0;
  const rec = ctx.historyByDgId[String(id)];
  const rounds = Array.isArray(rec?.rounds)
    ? rec.rounds.slice().sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a))
    : [];
  const here = rounds.filter((r) => courseNameMatchesVenue(r.course_name, vn));
  if (here.length < 2) return 0;
  const statKey =
    market === "Total score"
      ? "total"
      : market === "Birdies"
        ? "birdies"
        : market === "Pars"
          ? "pars"
          : market === "Bogeys"
            ? "bogeys"
            : market === "GIR"
              ? "gir"
              : market === "Fairways hit"
                ? "fairways"
                : market === "Putts"
                  ? "putts"
                  : "total";
  const venueAvg = meanNumFromRoundsRecencyWeightedStat(here, statKey, 0.82);
  const broadAvg = meanNumFromRoundsRecencyWeightedStat(rounds, statKey, 0.9);
  return adaptiveVenueStatMuNudge(market, venueAvg, broadAvg, modeRaw);
}

function pricingStatMuAdjustment(market, dgId, modeRaw, skillRaw, ctx) {
  const b = pricingModeMuSgBonusForMode(dgId, modeRaw, skillRaw, ctx);
  let out = 0;
  if (Number.isFinite(b) && b !== 0) {
    if (market === "Total score") out = -1.05 * b;
    else if (market === "Bogeys") out = -0.45 * b;
    else if (market === "Birdies") out = 0.5 * b;
    else if (market === "Pars") out = 0.08 * b;
    else if (market === "GIR") out = 0.35 * b;
    else if (market === "Fairways hit") out = 0.22 * b;
    else if (market === "Putts") out = -0.32 * b;
  }
  const id = Math.round(num(dgId, NaN));
  const playerRow = ctx.players.find(
    (p) => Math.round(num(p.dg_id)) === id && Math.round(num(p.round)) === ctx.modelRound,
  );
  const ctRow = ctx.ctRow ?? resolveCourseTableForVenue(ctx.venueName);
  const courseW =
    isAdaptivePricingMode(modeRaw) && playerRow
      ? courseWeightedMarketMuNudge(market, playerRow, ctRow, ctx.players, ctx.modelRound)
      : 0;
  return out + pricingCourseVenueStatMuNudge(market, dgId, modeRaw, ctx) + courseW;
}

function liveRowMatchesRound(row, meta) {
  const liveR = Math.round(num(meta?.datagolf_live_current_round ?? meta?.display_round, NaN));
  const pr = Math.round(num(row?.round, NaN));
  return Number.isFinite(liveR) && liveR >= 1 && liveR <= 4 && pr === liveR;
}

function livePartialRoundCountPropAdjust(market, row, meta) {
  const out = { muDelta: 0, sigmaScale: 1 };
  if (market !== "Birdies" && market !== "Pars" && market !== "Bogeys") return out;
  if (!liveRowMatchesRound(row, meta)) return out;
  const thru = Math.round(num(row.dg_live_thru, NaN));
  if (!Number.isFinite(thru) || thru < 1) return out;
  const rem = 18 - thru;
  if (rem < 0) return out;
  const field = market === "Birdies" ? "birdies" : market === "Pars" ? "pars" : "bogeys";
  const muFull = num(row[field], NaN);
  if (!Number.isFinite(muFull) || muFull < 0) return out;
  let b = num(row.dg_live_birdies_so_far, NaN);
  let bg = num(row.dg_live_bogeys_so_far, NaN);
  if (!Number.isFinite(b)) b = 0;
  if (!Number.isFinite(bg)) bg = 0;
  const eg = num(row.dg_live_eagles_so_far, NaN);
  const eagles = Number.isFinite(eg) && eg >= 0 ? Math.min(thru, Math.round(eg)) : 0;
  let pSo = num(row.dg_live_pars_so_far, NaN);
  if (!Number.isFinite(pSo)) {
    pSo = Math.max(0, Math.min(thru, thru - b - bg - eagles));
  }
  const rate = muFull / 18;
  let soFar = market === "Birdies" ? b + eagles : market === "Bogeys" ? bg : pSo;
  let muLive = clamp(soFar + rate * rem, 0, 18);
  out.muDelta = muLive - muFull;
  if (thru >= 18) out.sigmaScale = 0.26;
  else out.sigmaScale = clamp(Math.sqrt(rem / 18), 0.17, 1);
  return out;
}

function ouMeanCountingStat(market, row, fairwayHoles) {
  const rec = OU_STAT_MAP[market] || OU_STAT_MAP["Total score"];
  if (market === "Birdies") return birdiesPlusEaglesFromRow(row);
  const raw = num(row?.[rec.field], NaN);
  if (!Number.isFinite(raw)) return NaN;
  if (market === "GIR") return girFairwaysCountFromRaw(raw, 18);
  if (market === "Fairways hit") return girFairwaysCountFromRaw(raw, fairwayHoles);
  return raw;
}

function sigmaOuDiscreteCounting(market, muAbs, fairwayHoles) {
  const m = num(muAbs, NaN);
  if (!Number.isFinite(m) || m <= 0) return 2.4;
  if (market === "GIR") {
    const p = clamp(m / 18, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(18 * p * (1 - p)));
  }
  if (market === "Fairways hit") {
    const p = clamp(m / fairwayHoles, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(fairwayHoles * p * (1 - p)));
  }
  if (market === "Birdies" || market === "Bogeys") return clamp(Math.sqrt(m * 1.08), 1.05, 3.15);
  if (market === "Pars") return clamp(Math.sqrt(m * 1.06), 1.15, 3.35);
  return Math.max(0.55, Math.sqrt(Math.max(m, 0.2)) * 0.9);
}

export function sigmaForOu(market, row, meta, fairwayHoles) {
  const rec = OU_STAT_MAP[market] || OU_STAT_MAP["Total score"];
  if (rec.sdKey) {
    const s = num(row[rec.sdKey], NaN);
    if (Number.isFinite(s) && s > 0.05) return s;
  }
  const muAbs = ouMeanCountingStat(market, row, fairwayHoles);
  if (!Number.isFinite(muAbs) || muAbs <= 0) return 2.75;
  return sigmaOuDiscreteCounting(market, Math.abs(muAbs), fairwayHoles);
}

export function ouProjectedMeanForMode(market, row, meta, pricingMode, pricingSkill, ctx) {
  const dgId = Math.round(num(row?.dg_id, NaN));
  const fairwayHoles = num(meta?.projection_course_basis?.fairway_holes_modeled, NaN);
  const fwHoles = Number.isFinite(fairwayHoles) && fairwayHoles > 0 ? Math.round(fairwayHoles) : 14;
  const base = ouMeanCountingStat(market, row, fwHoles);
  if (!Number.isFinite(base)) return NaN;
  const countLive = livePartialRoundCountPropAdjust(market, row, meta);
  const weatherAdj =
    meta?.projection_counts_weather_baked && row?.weather_counts_baked
      ? 0
      : statWeatherMuAdjustment(market, row);
  return (
    base + weatherAdj + countLive.muDelta + pricingStatMuAdjustment(market, dgId, pricingMode, pricingSkill, ctx)
  );
}

export function modelProbOver(market, mu, line, row, meta) {
  if (!Number.isFinite(mu) || !Number.isFinite(line)) return NaN;
  const fairwayHoles = num(meta?.projection_course_basis?.fairway_holes_modeled, 14) || 14;
  const sig = sigmaForOu(market, row, meta, Math.round(fairwayHoles)) * marketBookSigmaScale(market);
  const z = (line - mu) / sig;
  return 1 - normalCdf(z);
}

/** W/L for over and under vs half-line (pushes blank). */
export function ouSideResults(market, actual, line) {
  if (!Number.isFinite(actual) || !Number.isFinite(line)) return { over: "", under: "" };
  if (actual > line) return { over: "W", under: "L" };
  if (actual < line) return { over: "L", under: "W" };
  return { over: "", under: "" };
}

export function impliedProbFromAmerican(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return NaN;
  if (v < 0) return (-v) / (-v + 100);
  return 100 / (v + 100);
}

/** Model edge % at a line (uses posted book odds when provided, else -110). */
export function modelEdgePctAtLine(market, mu, line, row, meta, overOdds, underOdds) {
  const pOver = modelProbOver(market, mu, line, row, meta);
  if (!Number.isFinite(pOver)) return { edgeOver: NaN, edgeUnder: NaN, best: NaN };
  const pUnder = 1 - pOver;
  const pImpOver = Number.isFinite(num(overOdds, NaN))
    ? impliedProbFromAmerican(overOdds)
    : 100 / 210;
  const pImpUnder = Number.isFinite(num(underOdds, NaN))
    ? impliedProbFromAmerican(underOdds)
    : 100 / 210;
  const edgeOver = (pOver - pImpOver) * 100;
  const edgeUnder = (pUnder - pImpUnder) * 100;
  return { edgeOver, edgeUnder, best: Math.max(edgeOver, edgeUnder) };
}

/** Best model edge vs -110 at the model line (max of over/under). */
export function bestModelEdgePct(market, mu, line, row, meta) {
  return modelEdgePctAtLine(market, mu, line, row, meta, NaN, NaN).best;
}

export function loadHistoryByDgId(webRoot) {
  const p = join(webRoot, "player_round_history.json");
  if (!existsSync(p)) return {};
  try {
    const j = JSON.parse(readFileSync(p, "utf8"));
    return j?.byDgId && typeof j.byDgId === "object" ? j.byDgId : {};
  } catch {
    return {};
  }
}

export function createProjectionContext(payload) {
  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : payload;
  const venueName = String(payload?.course_used || meta?.course_used || "").trim();
  return {
    meta,
    venueName,
    players: Array.isArray(payload?.players) ? payload.players : [],
    historyByDgId: loadHistoryByDgId(payload._webRoot || ""),
    bonusCache: new Map(),
    modelRound: Math.round(num(meta?.display_round, 1)) || 1,
    ctRow: resolveCourseTableForVenue(venueName),
  };
}
