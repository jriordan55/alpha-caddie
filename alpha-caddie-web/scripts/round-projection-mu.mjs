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
import { marketBookSigmaScale, eventPropBookAlignedMarket } from "./market-book-calibration.mjs";
import { applyOutcomeMuDebias } from "./outcome-mu-debias.mjs";
import {
  binomialProbOver,
  normalProbOver,
  outcomeSigmaScale,
  poissonProbOver,
} from "./projection-stat-model.mjs";
import { courseTailoringMuAdjustment, sgImportanceFromMeta } from "./course-skill-tailoring.mjs";
import {
  liveCurrentRoundTotalScoreMuDelta,
  livePartialRoundCountPropAdjust,
} from "./live-in-play-pricing.mjs";

export { liveCurrentRoundTotalScoreMuDelta } from "./live-in-play-pricing.mjs";
import {
  adaptiveVenueStatMuNudge,
  blendAdaptiveMuSgBonus,
  courseHistoryMuBonus,
  courseWeightedMarketMuNudge,
  courseWeightedSkillFormBonus,
  isAdaptivePricingMode,
  recentFormMuBonus,
  resolveCourseTableForVenue,
  RECENT_FORM_MIN,
  RECENT_FORM_MAX,
} from "./course-adaptive-pricing.mjs";

/** @param {string} stem fairways|round_score|birdies|bogeys|gir */
function altBookCols(stem) {
  const books = ["pp", "sl", "ud", "fd", "czr", "kl"];
  /** @type {Record<string, string>} */
  const cols = {};
  for (const b of books) {
    cols[`${b}LineCol`] = `${stem}_${b}_line`;
    cols[`${b}OverOddsCol`] = `${stem}_${b}_over_odds`;
    cols[`${b}UnderOddsCol`] = `${stem}_${b}_under_odds`;
    cols[`${b}OpenLineCol`] = `${stem}_${b}_open_line`;
    cols[`${b}OpenOverOddsCol`] = `${stem}_${b}_open_over_odds`;
    cols[`${b}OpenUnderOddsCol`] = `${stem}_${b}_open_under_odds`;
  }
  cols.bookOpenLineCol = `${stem}_book_open_line`;
  cols.openOverOddsCol = `${stem}_open_over_odds`;
  cols.openUnderOddsCol = `${stem}_open_under_odds`;
  return cols;
}

export const EXPORT_MARKETS = [
  {
    key: "fairways",
    market: "Fairways hit",
    propsMarket: "Fairways hit",
    lineCol: "fairways_line",
    bookLineCol: "fairways_book_line",
    ...altBookCols("fairways"),
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
    ...altBookCols("round_score"),
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
    ...altBookCols("birdies"),
    overOddsCol: "birdies_over_odds",
    underOddsCol: "birdies_under_odds",
    actualCol: "actual_birdies",
    overCol: "birdies_over",
    underCol: "birdies_under",
  },
  {
    key: "pars",
    market: "Pars",
    propsMarket: "Pars",
    lineCol: "pars_line",
    bookLineCol: "pars_book_line",
    ...altBookCols("pars"),
    overOddsCol: "pars_over_odds",
    underOddsCol: "pars_under_odds",
    actualCol: "actual_pars",
    overCol: "pars_over",
    underCol: "pars_under",
  },
  {
    key: "bogeys",
    market: "Bogeys",
    propsMarket: "Bogeys",
    lineCol: "bogeys_line",
    bookLineCol: "bogeys_book_line",
    ...altBookCols("bogeys"),
    overOddsCol: "bogeys_over_odds",
    underOddsCol: "bogeys_under_odds",
    actualCol: "actual_bogeys",
    overCol: "bogeys_over",
    underCol: "bogeys_under",
  },
  {
    key: "gir",
    market: "GIR",
    propsMarket: "GIR",
    lineCol: "gir_line",
    bookLineCol: "gir_book_line",
    ...altBookCols("gir"),
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
export const EXPORT_BOOK_OPEN_LINE_COLS = EXPORT_MARKETS.map((m) => m.bookOpenLineCol);
export const EXPORT_OPEN_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.openOverOddsCol);
export const EXPORT_OPEN_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.openUnderOddsCol);
export const EXPORT_PP_LINE_COLS = EXPORT_MARKETS.map((m) => m.ppLineCol);
export const EXPORT_SL_LINE_COLS = EXPORT_MARKETS.map((m) => m.slLineCol);
export const EXPORT_UD_LINE_COLS = EXPORT_MARKETS.map((m) => m.udLineCol);
export const EXPORT_FD_LINE_COLS = EXPORT_MARKETS.map((m) => m.fdLineCol);
export const EXPORT_CZR_LINE_COLS = EXPORT_MARKETS.map((m) => m.czrLineCol);
export const EXPORT_KL_LINE_COLS = EXPORT_MARKETS.map((m) => m.klLineCol);
export const EXPORT_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.overOddsCol);
export const EXPORT_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.underOddsCol);
export const EXPORT_PP_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.ppOverOddsCol);
export const EXPORT_PP_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.ppUnderOddsCol);
export const EXPORT_SL_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.slOverOddsCol);
export const EXPORT_SL_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.slUnderOddsCol);
export const EXPORT_UD_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.udOverOddsCol);
export const EXPORT_UD_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.udUnderOddsCol);
export const EXPORT_FD_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.fdOverOddsCol);
export const EXPORT_FD_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.fdUnderOddsCol);
export const EXPORT_CZR_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.czrOverOddsCol);
export const EXPORT_CZR_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.czrUnderOddsCol);
export const EXPORT_KL_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.klOverOddsCol);
export const EXPORT_KL_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.klUnderOddsCol);
export const EXPORT_PP_OPEN_LINE_COLS = EXPORT_MARKETS.map((m) => m.ppOpenLineCol);
export const EXPORT_SL_OPEN_LINE_COLS = EXPORT_MARKETS.map((m) => m.slOpenLineCol);
export const EXPORT_UD_OPEN_LINE_COLS = EXPORT_MARKETS.map((m) => m.udOpenLineCol);
export const EXPORT_FD_OPEN_LINE_COLS = EXPORT_MARKETS.map((m) => m.fdOpenLineCol);
export const EXPORT_CZR_OPEN_LINE_COLS = EXPORT_MARKETS.map((m) => m.czrOpenLineCol);
export const EXPORT_KL_OPEN_LINE_COLS = EXPORT_MARKETS.map((m) => m.klOpenLineCol);
export const EXPORT_PP_OPEN_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.ppOpenOverOddsCol);
export const EXPORT_PP_OPEN_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.ppOpenUnderOddsCol);
export const EXPORT_SL_OPEN_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.slOpenOverOddsCol);
export const EXPORT_SL_OPEN_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.slOpenUnderOddsCol);
export const EXPORT_UD_OPEN_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.udOpenOverOddsCol);
export const EXPORT_UD_OPEN_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.udOpenUnderOddsCol);
export const EXPORT_FD_OPEN_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.fdOpenOverOddsCol);
export const EXPORT_FD_OPEN_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.fdOpenUnderOddsCol);
export const EXPORT_CZR_OPEN_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.czrOpenOverOddsCol);
export const EXPORT_CZR_OPEN_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.czrOpenUnderOddsCol);
export const EXPORT_KL_OPEN_OVER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.klOpenOverOddsCol);
export const EXPORT_KL_OPEN_UNDER_ODDS_COLS = EXPORT_MARKETS.map((m) => m.klOpenUnderOddsCol);
export const EXPORT_OVER_RESULT_COLS = EXPORT_MARKETS.map((m) => m.overCol);
export const EXPORT_UNDER_RESULT_COLS = EXPORT_MARKETS.map((m) => m.underCol);

/** Extra sportsbooks beyond DraftKings (wide detail CSV columns). */
export const EXPORT_ALT_BOOKS = [
  {
    id: "prizepicks",
    short: "pp",
    sourceCol: "pp_book_odds_source",
    openAtCol: "pp_book_odds_open_at",
    closeAtCol: "pp_book_odds_close_at",
    lineCols: EXPORT_PP_LINE_COLS,
    overCols: EXPORT_PP_OVER_ODDS_COLS,
    underCols: EXPORT_PP_UNDER_ODDS_COLS,
    openLineCols: EXPORT_PP_OPEN_LINE_COLS,
    openOverCols: EXPORT_PP_OPEN_OVER_ODDS_COLS,
    openUnderCols: EXPORT_PP_OPEN_UNDER_ODDS_COLS,
    lineKey: "ppLineCol",
    overKey: "ppOverOddsCol",
    underKey: "ppUnderOddsCol",
    openLineKey: "ppOpenLineCol",
    openOverKey: "ppOpenOverOddsCol",
    openUnderKey: "ppOpenUnderOddsCol",
    liveOddsSource: "prizepicks_live",
    wholeLine: true,
  },
  {
    id: "sleeper",
    short: "sl",
    sourceCol: "sl_book_odds_source",
    openAtCol: "sl_book_odds_open_at",
    closeAtCol: "sl_book_odds_close_at",
    lineCols: EXPORT_SL_LINE_COLS,
    overCols: EXPORT_SL_OVER_ODDS_COLS,
    underCols: EXPORT_SL_UNDER_ODDS_COLS,
    openLineCols: EXPORT_SL_OPEN_LINE_COLS,
    openOverCols: EXPORT_SL_OPEN_OVER_ODDS_COLS,
    openUnderCols: EXPORT_SL_OPEN_UNDER_ODDS_COLS,
    lineKey: "slLineCol",
    overKey: "slOverOddsCol",
    underKey: "slUnderOddsCol",
    openLineKey: "slOpenLineCol",
    openOverKey: "slOpenOverOddsCol",
    openUnderKey: "slOpenUnderOddsCol",
    liveOddsSource: "sleeper_live",
    wholeLine: true,
  },
  {
    id: "underdog",
    short: "ud",
    sourceCol: "ud_book_odds_source",
    openAtCol: "ud_book_odds_open_at",
    closeAtCol: "ud_book_odds_close_at",
    lineCols: EXPORT_UD_LINE_COLS,
    overCols: EXPORT_UD_OVER_ODDS_COLS,
    underCols: EXPORT_UD_UNDER_ODDS_COLS,
    openLineCols: EXPORT_UD_OPEN_LINE_COLS,
    openOverCols: EXPORT_UD_OPEN_OVER_ODDS_COLS,
    openUnderCols: EXPORT_UD_OPEN_UNDER_ODDS_COLS,
    lineKey: "udLineCol",
    overKey: "udOverOddsCol",
    underKey: "udUnderOddsCol",
    openLineKey: "udOpenLineCol",
    openOverKey: "udOpenOverOddsCol",
    openUnderKey: "udOpenUnderOddsCol",
    liveOddsSource: "underdog_live",
    wholeLine: true,
  },
  {
    id: "fanduel",
    short: "fd",
    sourceCol: "fd_book_odds_source",
    openAtCol: "fd_book_odds_open_at",
    closeAtCol: "fd_book_odds_close_at",
    lineCols: EXPORT_FD_LINE_COLS,
    overCols: EXPORT_FD_OVER_ODDS_COLS,
    underCols: EXPORT_FD_UNDER_ODDS_COLS,
    openLineCols: EXPORT_FD_OPEN_LINE_COLS,
    openOverCols: EXPORT_FD_OPEN_OVER_ODDS_COLS,
    openUnderCols: EXPORT_FD_OPEN_UNDER_ODDS_COLS,
    lineKey: "fdLineCol",
    overKey: "fdOverOddsCol",
    underKey: "fdUnderOddsCol",
    openLineKey: "fdOpenLineCol",
    openOverKey: "fdOpenOverOddsCol",
    openUnderKey: "fdOpenUnderOddsCol",
    liveOddsSource: "fanduel_live",
    wholeLine: false,
  },
  {
    id: "caesars",
    short: "czr",
    sourceCol: "czr_book_odds_source",
    openAtCol: "czr_book_odds_open_at",
    closeAtCol: "czr_book_odds_close_at",
    lineCols: EXPORT_CZR_LINE_COLS,
    overCols: EXPORT_CZR_OVER_ODDS_COLS,
    underCols: EXPORT_CZR_UNDER_ODDS_COLS,
    openLineCols: EXPORT_CZR_OPEN_LINE_COLS,
    openOverCols: EXPORT_CZR_OPEN_OVER_ODDS_COLS,
    openUnderCols: EXPORT_CZR_OPEN_UNDER_ODDS_COLS,
    lineKey: "czrLineCol",
    overKey: "czrOverOddsCol",
    underKey: "czrUnderOddsCol",
    openLineKey: "czrOpenLineCol",
    openOverKey: "czrOpenOverOddsCol",
    openUnderKey: "czrOpenUnderOddsCol",
    liveOddsSource: "caesars_live",
    wholeLine: false,
  },
  {
    id: "kalshi",
    short: "kl",
    sourceCol: "kl_book_odds_source",
    openAtCol: "kl_book_odds_open_at",
    closeAtCol: "kl_book_odds_close_at",
    lineCols: EXPORT_KL_LINE_COLS,
    overCols: EXPORT_KL_OVER_ODDS_COLS,
    underCols: EXPORT_KL_UNDER_ODDS_COLS,
    openLineCols: EXPORT_KL_OPEN_LINE_COLS,
    openOverCols: EXPORT_KL_OPEN_OVER_ODDS_COLS,
    openUnderCols: EXPORT_KL_OPEN_UNDER_ODDS_COLS,
    lineKey: "klLineCol",
    overKey: "klOverOddsCol",
    underKey: "klUnderOddsCol",
    openLineKey: "klOpenLineCol",
    openOverKey: "klOpenOverOddsCol",
    openUnderKey: "klOpenUnderOddsCol",
    liveOddsSource: "kalshi_live",
    wholeLine: false,
  },
];

export const EXPORT_ALT_SOURCE_COLS = EXPORT_ALT_BOOKS.map((b) => b.sourceCol);
export const EXPORT_ALT_OPEN_AT_COLS = EXPORT_ALT_BOOKS.map((b) => b.openAtCol);
export const EXPORT_ALT_CLOSE_AT_COLS = EXPORT_ALT_BOOKS.map((b) => b.closeAtCol);

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
  if (v == null) return NaN;
  const s = String(v).trim();
  if (!s) return NaN;
  const x = Number(s);
  if (!Number.isFinite(x)) return NaN;
  return Math.round(x * 2) / 2;
}

/** Parse a DraftKings posted line (always half-point buckets). Blank → NaN (not 0). */
export function parseDkBookLine(v) {
  return enforceHalfLine(v);
}

/** Parse a PrizePicks posted line (whole numbers allowed). */
export function parsePpBookLine(v) {
  return num(v, NaN);
}

/** Format a DraftKings book line for CSV/display. */
export function fmtDkBookLine(market, v) {
  const n = parseDkBookLine(v);
  if (!Number.isFinite(n)) return "";
  if (market === "Total score") return (Math.round(n * 10) / 10).toFixed(1);
  return String(n);
}

/** Format a PrizePicks book line for CSV/display (preserve whole numbers). */
export function fmtPpBookLine(market, v) {
  const n = parsePpBookLine(v);
  if (!Number.isFinite(n)) return "";
  if (market === "Total score") {
    if (n === Math.round(n)) return `${Math.round(n)}.0`;
    return (Math.round(n * 10) / 10).toFixed(1);
  }
  if (n === Math.round(n)) return String(Math.round(n));
  return (Math.round(n * 10) / 10).toFixed(1);
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

/** DK / PP “Bogeys or Worse”: bogeys + doubles_or_worse. */
export function bogeysPlusDoublesFromRow(row) {
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
    else if (statKey === "bogeys") v = bogeysPlusDoublesFromRow(row);
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
    const nRec = Math.min(RECENT_FORM_MAX, Math.max(RECENT_FORM_MIN, rounds.length >= RECENT_FORM_MIN ? 10 : 6));
    const recent = rounds.slice(0, nRec);
    const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 24));
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
  return sigmaOuDiscreteCounting(market, Math.abs(muAbs), fairwayHoles) * outcomeSigmaScale(market);
}

function liveProjectionMeta(metaOrPayload) {
  if (!metaOrPayload || typeof metaOrPayload !== "object") return {};
  const nested =
    metaOrPayload.meta && typeof metaOrPayload.meta === "object" ? metaOrPayload.meta : {};
  return { ...metaOrPayload, ...nested };
}

export function ouProjectedMeanForMode(market, row, meta, pricingMode, pricingSkill, ctx) {
  const metaLive = liveProjectionMeta(meta);
  const dgId = Math.round(num(row?.dg_id, NaN));
  const fairwayHoles = num(metaLive?.projection_course_basis?.fairway_holes_modeled, NaN);
  const fwHoles = Number.isFinite(fairwayHoles) && fairwayHoles > 0 ? Math.round(fairwayHoles) : 14;
  const base = ouMeanCountingStat(market, row, fwHoles);
  if (!Number.isFinite(base)) return NaN;
  const countLive = livePartialRoundCountPropAdjust(market, row, metaLive);
  let mu;
  if (row?.bayesian_market_posterior?.[market]) {
    const liveScore =
      market === "Total score" ? liveCurrentRoundTotalScoreMuDelta(row, metaLive) : 0;
    mu = base + countLive.muDelta + liveScore;
  } else if (eventPropBookAlignedMarket(metaLive, market)) {
    const liveScore =
      market === "Total score" ? liveCurrentRoundTotalScoreMuDelta(row, metaLive) : 0;
    mu = base + countLive.muDelta + liveScore;
  } else {
    const weatherAdj =
      metaLive?.projection_counts_weather_baked && row?.weather_counts_baked
        ? 0
        : statWeatherMuAdjustment(market, row);
    const tailoringActive = Boolean(sgImportanceFromMeta(metaLive));
    const pricingAdj = tailoringActive
      ? 0
      : pricingStatMuAdjustment(market, dgId, pricingMode, pricingSkill, ctx);
    mu =
      base +
      weatherAdj +
      countLive.muDelta +
      liveCurrentRoundTotalScoreMuDelta(row, metaLive) +
      pricingAdj +
      courseTailoringMuAdjustment(market, row, metaLive, ctx);
  }
  const bookLine = num(
    row?.[`book_line_${market}`] ??
      row?.book_line ??
      (market === "Total score"
        ? row?.round_score_book_line
        : market === "Birdies"
          ? row?.birdies_book_line
          : market === "GIR"
            ? row?.gir_book_line
            : market === "Fairways hit"
              ? row?.fairways_book_line
              : NaN),
    NaN,
  );
  return applyOutcomeMuDebias(market, mu, bookLine);
}

function countingStubRowForMu(market, mu, row, fairwayHoles) {
  const base = row && typeof row === "object" ? { ...row } : {};
  if (market === "Birdies") return { ...base, birdies: mu };
  if (market === "Bogeys") return { ...base, bogeys: mu };
  if (market === "GIR") return { ...base, gir: mu };
  if (market === "Fairways hit") return { ...base, fairways: mu };
  return base;
}

export function modelProbOver(market, mu, line, row, meta) {
  if (!Number.isFinite(mu) || !Number.isFinite(line)) return NaN;
  const metaLive = liveProjectionMeta(meta);
  const fairwayHoles = Math.round(num(metaLive?.projection_course_basis?.fairway_holes_modeled, 14)) || 14;
  const bookSig = marketBookSigmaScale(market);
  // Sportsbook-style: Poisson bird/bog, binomial GIR/FW, normal total score.
  if (market === "Birdies" || market === "Bogeys" || market === "Pars") {
    const lam = Math.max(0.05, mu * clamp(bookSig, 0.85, 1.25));
    return poissonProbOver(lam, line);
  }
  if (market === "GIR") {
    const m = Math.max(0.05, mu * clamp(bookSig, 0.85, 1.25));
    return binomialProbOver(m, 18, line);
  }
  if (market === "Fairways hit") {
    const m = Math.max(0.05, mu * clamp(bookSig, 0.85, 1.25));
    return binomialProbOver(m, fairwayHoles, line);
  }
  const muRow = row;
  const sig = sigmaForOu(market, muRow, metaLive, fairwayHoles) * bookSig;
  return normalProbOver(mu, line, sig);
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
    historyByDgId: payload?.historyByDgId || loadHistoryByDgId(payload._webRoot || ""),
    venueScoring: payload?.venueScoring || null,
    bonusCache: new Map(),
    modelRound: Math.round(num(meta?.display_round, 1)) || 1,
    ctRow: resolveCourseTableForVenue(venueName),
  };
}
