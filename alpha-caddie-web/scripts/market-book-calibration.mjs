/**
 * DK book-alignment calibration per market: μ shift + σ scale from vs-actual backtest.
 * Fit: node scripts/fit-market-book-calibration.mjs
 */
import { readFileSync, existsSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const CAL_PATH = join(WEB, "data", "market_book_calibration.json");

/** @type {{ generated_at?: string, markets?: Record<string, { mu_shift: number, sigma_scale: number, n_fit?: number, model_book_delta?: number }> } | null} */
let _cache = null;

export const MARKET_BOOK_CALIBRATION_MARKETS = [
  "Total score",
  "Birdies",
  "GIR",
  "Fairways hit",
];

/** Markets excluded from book calibration (none — pars/bogeys removed from export). */
export const MARKETS_SKIP_BOOK_CALIBRATION = new Set();

export function marketSkipsBookCalibration(market) {
  return MARKETS_SKIP_BOOK_CALIBRATION.has(market);
}

/** RMSE baselines for σ inflation (model vs DK book, not outcomes). */
const BOOK_RMSE_BASELINE = {
  "Total score": 1.25,
  Birdies: 0.85,
  Bogeys: 0.75,
  GIR: 1.05,
  "Fairways hit": 1.0,
  Pars: 0.9,
};

const MU_SHIFT_CLAMP = {
  "Total score": 1.2,
  Birdies: 0.9,
  Bogeys: 0.9,
  GIR: 1.0,
  "Fairways hit": 1.0,
  Pars: 0.8,
};

/** Wider clamps for same-week DK prop alignment (centers field vs book, not cross-event generalization). */
const EVENT_PROP_SHIFT_CLAMP = {
  "Total score": 2.5,
  Birdies: 1.75,
  Bogeys: 1.5,
  GIR: 1.5,
  "Fairways hit": 1.5,
  Pars: 1.2,
};

function birdiesPlusEaglesLine(row) {
  const b = num(row?.birdies, NaN);
  if (!Number.isFinite(b)) return NaN;
  const e = num(row?.eagles ?? row?.eagles_or_better, 0);
  return b + (Number.isFinite(e) ? Math.max(0, e) : 0);
}

function modelLineForPropMarket(market, row, col) {
  if (market === "Birdies") return birdiesPlusEaglesLine(row);
  return num(row?.[col], NaN);
}

const PROP_MARKET_TO_ROW = {
  "Total Score": { market: "Total score", col: "total_score" },
  "Total score": { market: "Total score", col: "total_score" },
  Birdies: { market: "Birdies", col: "birdies" },
  GIR: { market: "GIR", col: "gir" },
  "Fairways hit": { market: "Fairways hit", col: "fairways" },
  Bogeys: { market: "Bogeys", col: "bogeys" },
  Pars: { market: "Pars", col: "pars" },
};

/** Conservative defaults until fit script runs. */
const DEFAULT_CALIBRATION = {
  generated_at: "defaults",
  fit_method: "book_alignment_no_outcome_peek",
  markets: {
    "Total score": { mu_shift: 0, sigma_scale: 1.1 },
    Birdies: { mu_shift: 0, sigma_scale: 1.12 },
    Bogeys: { mu_shift: -0.15, sigma_scale: 1.15 },
    GIR: { mu_shift: 0.35, sigma_scale: 1.18 },
    "Fairways hit": { mu_shift: 0.25, sigma_scale: 1.16 },
    Pars: { mu_shift: 0, sigma_scale: 1.12 },
  },
};

/**
 * Fit μ/σ from model−book deltas only (pre-round observable; no W/L).
 * μ_shift pulls model toward DK: shift = −shrink × mean(model − book).
 */
export function fitMarketBookParamsFromDeltas(market, deltas) {
  const xs = (deltas || []).filter((d) => Number.isFinite(d));
  const n = xs.length;
  if (!n) {
    return { mu_shift: 0, sigma_scale: 1.12, n_fit: 0, model_book_delta: 0, model_book_rmse: 0 };
  }
  const mean = xs.reduce((a, x) => a + x, 0) / n;
  const sq = xs.reduce((a, x) => a + x * x, 0) / n;
  const rmse = Math.sqrt(Math.max(0, sq));
  const shrink = n / (n + 50);
  const clampAbs = MU_SHIFT_CLAMP[market] ?? 0.9;
  const mu_shift = Math.round(clamp(-mean * shrink, -clampAbs, clampAbs) * 1000) / 1000;
  const base = BOOK_RMSE_BASELINE[market] ?? 1;
  const sigma_scale =
    Math.round(clamp(1 + 0.14 * (rmse / base - 1), 1.08, 1.32) * 1000) / 1000;
  return {
    mu_shift,
    sigma_scale,
    n_fit: n,
    model_book_delta: Math.round(mean * 1000) / 1000,
    model_book_rmse: Math.round(rmse * 1000) / 1000,
  };
}

export function marketBookCalibrationPath() {
  return CAL_PATH;
}

export function loadMarketBookCalibration(force = false) {
  if (_cache && !force) return _cache;
  if (existsSync(CAL_PATH)) {
    try {
      const j = JSON.parse(readFileSync(CAL_PATH, "utf8"));
      if (j?.markets && typeof j.markets === "object") {
        _cache = j;
        return _cache;
      }
    } catch {
      /* fall through */
    }
  }
  _cache = DEFAULT_CALIBRATION;
  return _cache;
}

export function marketBookCalibrationEnabled() {
  const v = String(process.env.GOLF_MARKET_BOOK_CALIBRATION ?? "0").trim();
  return v !== "0" && v.toLowerCase() !== "false";
}

export function marketBookMuShift(market) {
  if (marketSkipsBookCalibration(market)) return 0;
  if (!marketBookCalibrationEnabled()) return 0;
  const m = loadMarketBookCalibration().markets?.[market];
  const n = Number(m?.mu_shift);
  return Number.isFinite(n) ? n : 0;
}

export function marketBookSigmaScale(market) {
  if (marketSkipsBookCalibration(market)) return 1;
  if (!marketBookCalibrationEnabled()) return 1;
  const m = loadMarketBookCalibration().markets?.[market];
  const n = Number(m?.sigma_scale);
  return Number.isFinite(n) && n > 0 ? n : 1;
}

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

/** Apply a book-alignment μ shift to a single exported model line (counting clamps match live apply). */
export function applyMuShiftToModelLine(market, rawLine, muShift) {
  const raw = num(rawLine);
  const shift = num(muShift);
  if (!Number.isFinite(raw) || !Number.isFinite(shift) || shift === 0) return raw;
  const next = raw + shift;
  if (market === "Total score") return Math.round(next * 10) / 10;
  if (market === "Birdies") return Math.round(clamp(next, 0.1, 8) * 100) / 100;
  if (market === "Bogeys") return Math.round(clamp(next, 0.1, 9) * 100) / 100;
  if (market === "GIR") return Math.round(clamp(next, 4, 17) * 100) / 100;
  if (market === "Fairways hit") return Math.round(clamp(next, 2, 16) * 100) / 100;
  if (market === "Pars") return Math.round(clamp(next, 4, 16) * 100) / 100;
  return Math.round(next * 100) / 100;
}

/**
 * Fit per-market μ shifts from model − DK line on current props (same event, no outcomes).
 * Pulls model toward posted books so O/U edges are not all on one side.
 */
export function fitEventPropBookShifts(payload, opts = {}) {
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const props = Array.isArray(payload?.props) ? payload.props : [];
  const displayRound = Math.round(num(opts.displayRound ?? payload?.display_round ?? 1)) || 1;
  const minPairs = Math.max(8, Math.round(num(opts.minPairs, 12)) || 12);
  /** @type {Record<string, number[]>} */
  const deltasByMarket = {};

  for (const pr of props) {
    const spec = PROP_MARKET_TO_ROW[String(pr.market || "").trim()];
    if (!spec) continue;
    const rnd = Math.round(num(pr.round_num ?? pr.display_round ?? displayRound));
    if (rnd !== displayRound) continue;
    const dg = Math.round(num(pr.dg_id, NaN));
    const book = num(pr.line, NaN);
    if (!Number.isFinite(dg) || !Number.isFinite(book)) continue;
    const row = players.find(
      (p) => Math.round(num(p.dg_id, NaN)) === dg && Math.round(num(p.round, NaN)) === rnd,
    );
    if (!row) continue;
    const model = modelLineForPropMarket(spec.market, row, spec.col);
    if (!Number.isFinite(model)) continue;
    if (!deltasByMarket[spec.market]) deltasByMarket[spec.market] = [];
    deltasByMarket[spec.market].push(model - book);
  }

  /** @type {Record<string, { mu_shift: number, n_pairs: number, mean_delta: number }>} */
  const shifts = {};
  for (const [market, deltas] of Object.entries(deltasByMarket)) {
    const n = deltas.length;
    if (n < minPairs) continue;
    const mean = deltas.reduce((a, x) => a + x, 0) / n;
    const clampAbs = EVENT_PROP_SHIFT_CLAMP[market] ?? MU_SHIFT_CLAMP[market] ?? 1.2;
    const mu_shift = Math.round(clamp(-mean, -clampAbs, clampAbs) * 1000) / 1000;
    shifts[market] = {
      mu_shift,
      n_pairs: n,
      mean_delta: Math.round(mean * 1000) / 1000,
    };
  }
  return { shifts, display_round: displayRound };
}

/** Apply event-week prop book shifts to every player row (additive on counting columns). */
export function applyEventPropBookShiftsToRow(row, shifts, coursePar18 = 72) {
  if (!row || typeof row !== "object" || !shifts || typeof shifts !== "object") return;
  const par = Math.round(num(coursePar18, NaN)) || 72;

  const scoreShift = num(shifts["Total score"]?.mu_shift, NaN);
  if (scoreShift) {
    const stp = num(row.score_to_par, NaN);
    const ts = num(row.total_score, NaN);
    if (Number.isFinite(stp)) {
      row.score_to_par = Math.round((stp + scoreShift) * 100) / 100;
      row.total_score = Math.round((par + row.score_to_par) * 100) / 100;
    } else if (Number.isFinite(ts)) {
      row.total_score = Math.round((ts + scoreShift) * 100) / 100;
      row.score_to_par = Math.round((row.total_score - par) * 100) / 100;
    }
  }

  const applyCount = (market, col, lo, hi) => {
    const shift = num(shifts[market]?.mu_shift, NaN);
    if (!shift) return;
    const v = num(row[col], NaN);
    if (!Number.isFinite(v)) return;
    row[col] = Math.round(clamp(v + shift, lo, hi) * 100) / 100;
  };

  applyCount("Birdies", "birdies", 0.1, 8);
  applyCount("Bogeys", "bogeys", 0.1, 9);
  applyCount("GIR", "gir", 4, 17);
  applyCount("Fairways hit", "fairways", 2, 16);
  applyCount("Pars", "pars", 4, 16);

  const e = num(row.eagles, 0);
  const d = num(row.doubles, 0);
  const b = num(row.birdies, NaN);
  const bg = num(row.bogeys, NaN);
  if (Number.isFinite(b) && Number.isFinite(bg)) {
    row.pars = Math.max(0.1, Math.round((18 - e - d - b - bg) * 100) / 100);
  }
}

/**
 * Align model vs current DK props for the display round; stores meta.event_prop_book_alignment.
 * @returns {{ applied: boolean, markets: Record<string, object> }}
 */
export function applyEventPropBookAlignment(payload, opts = {}) {
  if (!marketBookCalibrationEnabled()) return { applied: false, markets: {} };
  const fit = fitEventPropBookShifts(payload, opts);
  const shiftMap = fit.shifts || {};
  if (!Object.keys(shiftMap).length) return { applied: false, markets: {} };

  const par = Math.round(num(opts.coursePar18 ?? payload?.course_par_18, NaN)) || 72;
  for (const pl of payload?.players || []) {
    if (!pl || typeof pl !== "object") continue;
    applyEventPropBookShiftsToRow(pl, shiftMap, par);
  }

  if (!payload.meta || typeof payload.meta !== "object") payload.meta = {};
  payload.meta.event_prop_book_alignment = {
    generated_at: new Date().toISOString(),
    display_round: fit.display_round,
    method: "current_dk_props_zero_mean_delta",
    markets: shiftMap,
  };
  return { applied: true, markets: shiftMap };
}

export function eventPropBookMuShift(market, meta) {
  const m = meta?.event_prop_book_alignment?.markets?.[market];
  const s = num(m?.mu_shift, NaN);
  return Number.isFinite(s) ? s : 0;
}

export function eventPropBookAlignedMarket(meta, market) {
  if (!marketBookCalibrationEnabled()) return false;
  return Boolean(meta?.event_prop_book_alignment?.markets?.[market]);
}

/**
 * Apply fitted μ shifts to projection player row (post-reconcile).
 */
export function applyMarketBookCalibrationToRow(row, coursePar18 = 72) {
  if (!marketBookCalibrationEnabled() || !row || typeof row !== "object") return;
  const par = Math.round(num(coursePar18, NaN)) || 72;

  const scoreShift = marketBookMuShift("Total score");
  if (scoreShift) {
    const stp = num(row.score_to_par, NaN);
    const ts = num(row.total_score, NaN);
    if (Number.isFinite(stp)) {
      row.score_to_par = Math.round((stp + scoreShift) * 100) / 100;
      row.total_score = Math.round((par + row.score_to_par) * 100) / 100;
    } else if (Number.isFinite(ts)) {
      row.total_score = Math.round((ts + scoreShift) * 100) / 100;
      row.score_to_par = Math.round((row.total_score - par) * 100) / 100;
    }
  }

  const applyCount = (market, col, lo, hi) => {
    const shift = marketBookMuShift(market);
    if (!shift) return;
    const v = num(row[col], NaN);
    if (!Number.isFinite(v)) return;
    row[col] = Math.round(clamp(v + shift, lo, hi) * 100) / 100;
  };

  applyCount("Birdies", "birdies", 0.1, 8);
  applyCount("Bogeys", "bogeys", 0.1, 9);
  applyCount("GIR", "gir", 4, 17);
  applyCount("Fairways hit", "fairways", 2, 16);
  applyCount("Pars", "pars", 4, 16);

  const e = num(row.eagles, 0);
  const d = num(row.doubles, 0);
  const b = num(row.birdies, NaN);
  const bg = num(row.bogeys, NaN);
  if (Number.isFinite(b) && Number.isFinite(bg)) {
    row.pars = Math.max(0.1, Math.round((18 - e - d - b - bg) * 100) / 100);
  }
}

function reverseMuShiftOnRow(row, shifts, coursePar18 = 72) {
  if (!row || typeof row !== "object" || !shifts || typeof shifts !== "object") return;
  const par = Math.round(num(coursePar18, NaN)) || 72;

  const scoreShift = num(shifts["Total score"]?.mu_shift, NaN);
  if (scoreShift) {
    const stp = num(row.score_to_par, NaN);
    const ts = num(row.total_score, NaN);
    if (Number.isFinite(stp)) {
      row.score_to_par = Math.round((stp - scoreShift) * 100) / 100;
      row.total_score = Math.round((par + row.score_to_par) * 100) / 100;
    } else if (Number.isFinite(ts)) {
      row.total_score = Math.round((ts - scoreShift) * 100) / 100;
      row.score_to_par = Math.round((row.total_score - par) * 100) / 100;
    }
  }

  const reverseCount = (market, col, lo, hi) => {
    const shift = num(shifts[market]?.mu_shift, NaN);
    if (!shift) return;
    const v = num(row[col], NaN);
    if (!Number.isFinite(v)) return;
    row[col] = Math.round(clamp(v - shift, lo, hi) * 100) / 100;
  };

  reverseCount("Birdies", "birdies", 0.1, 8);
  reverseCount("Bogeys", "bogeys", 0.1, 9);
  reverseCount("GIR", "gir", 4, 17);
  reverseCount("Fairways hit", "fairways", 2, 16);
  reverseCount("Pars", "pars", 4, 16);

  const e = num(row.eagles, 0);
  const d = num(row.doubles, 0);
  const b = num(row.birdies, NaN);
  const bg = num(row.bogeys, NaN);
  if (Number.isFinite(b) && Number.isFinite(bg)) {
    row.pars = Math.max(0.1, Math.round((18 - e - d - b - bg) * 100) / 100);
  }
}

/**
 * Remove baked-in DK book μ-shifts from projections.json (reverse apply order).
 * @returns {{ strippedGlobal: boolean, strippedEventProps: boolean, rows: number }}
 */
export function stripMarketBookCalibrationFromPayload(payload, coursePar18 = 72) {
  const par = Math.round(num(coursePar18, NaN)) || 72;
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : null;
  const eventShifts =
    meta?.event_prop_book_alignment?.markets || payload?.event_prop_book_alignment?.markets;
  let globalShifts =
    meta?.market_book_calibration?.markets || payload?.market_book_calibration?.markets;
  if (!globalShifts || !Object.keys(globalShifts).length) {
    const fileCal = loadMarketBookCalibration();
    if (fileCal?.markets && Object.keys(fileCal.markets).length) globalShifts = fileCal.markets;
  }
  let strippedEventProps = false;
  let strippedGlobal = false;

  if (eventShifts && typeof eventShifts === "object" && Object.keys(eventShifts).length) {
    for (const pl of players) reverseMuShiftOnRow(pl, eventShifts, par);
    strippedEventProps = true;
    if (meta) delete meta.event_prop_book_alignment;
    if (payload?.event_prop_book_alignment) delete payload.event_prop_book_alignment;
  }

  if (globalShifts && typeof globalShifts === "object" && Object.keys(globalShifts).length) {
    for (const pl of players) reverseMuShiftOnRow(pl, globalShifts, par);
    strippedGlobal = true;
    if (meta) delete meta.market_book_calibration;
    if (payload?.market_book_calibration) delete payload.market_book_calibration;
  }

  return { strippedGlobal, strippedEventProps, rows: players.length };
}
