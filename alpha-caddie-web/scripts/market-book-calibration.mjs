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
  const v = String(process.env.GOLF_MARKET_BOOK_CALIBRATION ?? "1").trim();
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
