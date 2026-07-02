/**
 * Walk-forward out-of-sample ROI — raw model μ vs pre-round DK only (no book calibration).
 */
import { readFileSync, existsSync, createReadStream } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import {
  DEFAULT_MIN_EV_PCT,
  isActionableMarket,
  minEvForMarket,
  qualifiesBet,
} from "./bet-policy.mjs";
import { RAW_ROUND_SD } from "./projection-core.mjs";
import { EXPORT_MARKETS, num, modelProbOver } from "./round-projection-mu.mjs";
import {
  fitOutcomeMuBiasCorrections,
  fitOutcomeSigmaScales,
  outcomeSigmaScale,
  setOutcomeMuBiasCorrections,
  setOutcomeSigmaScales,
} from "./projection-stat-model.mjs";
import { MARKET_BOOK_CALIBRATION_MARKETS } from "./market-book-calibration.mjs";
import { capDirectionalPostedEdges, pickBetSide, pnlForResult } from "../projection-tracker/ev-math.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
export const WALKFORWARD_OOS_JSON = join(WEB, "data", "walkforward_oos_roi.json");

const EV_THRESHOLDS = [0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20];
const MARKET_COLS = Object.fromEntries(
  EXPORT_MARKETS.map((m) => [
    m.market,
    {
      model: m.lineCol,
      book: m.bookLineCol,
      overOdds: m.overOddsCol,
      underOdds: m.underOddsCol,
      overRes: m.overCol,
      underRes: m.underCol,
    },
  ]),
);

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? num(s, NaN) : NaN;
}

function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
}

function loadCurrentLiveEventName() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

export async function loadWalkForwardBetRows() {
  if (!existsSync(VS)) throw new Error(`Missing ${VS}`);
  /** @type {object[]} */
  const rows = [];
  await new Promise((resolve, reject) => {
    createReadStream(VS)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.pricing_skill || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        const event = String(row.event_name || "").trim();
        if (!event) return;
        const t = parseMs(row.projections_updated_at) || parseMs(row.exported_at);
        const meta = { projection_course_basis: { fairway_holes_modeled: 14 } };
        const context = {
          gir_minus_fw: num(row.gir_minus_fw, NaN),
          round: Math.round(num(row.round, NaN)),
        };
        for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const modelLine = parseLine(row[cols.model]);
          const bookLine = parseLine(row[cols.book]);
          if (!Number.isFinite(modelLine) || !Number.isFinite(bookLine)) continue;
          const stubRow =
            market === "Total score"
              ? {
                  total_score: modelLine,
                  round_sd: RAW_ROUND_SD * outcomeSigmaScale("Total score"),
                }
              : market === "Birdies"
                ? { birdies: modelLine }
                : market === "Bogeys"
                  ? { bogeys: modelLine }
                  : market === "GIR"
                    ? { gir: modelLine }
                    : market === "Fairways hit"
                      ? { fairways: modelLine }
                      : { pars: modelLine };
          rows.push({
            event,
            eventMs: t,
            market,
            modelLine,
            modelBookDelta: modelLine - bookLine,
            bookLine,
            overOdds: num(row[cols.overOdds], NaN),
            underOdds: num(row[cols.underOdds], NaN),
            overRes: String(row[cols.overRes] || "").trim().toUpperCase(),
            underRes: String(row[cols.underRes] || "").trim().toUpperCase(),
            stubRow,
            meta,
            context,
          });
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return rows;
}

export function eventOrderFromRows(rows) {
  const m = new Map();
  for (const r of rows) {
    let e = m.get(r.event);
    if (!e) e = { ms: r.eventMs, n: 0 };
    if (Number.isFinite(r.eventMs) && (!Number.isFinite(e.ms) || r.eventMs < e.ms)) e.ms = r.eventMs;
    e.n++;
    m.set(r.event, e);
  }
  return [...m.entries()]
    .sort((a, b) => {
      const ta = Number.isFinite(a[1].ms) ? a[1].ms : 0;
      const tb = Number.isFinite(b[1].ms) ? b[1].ms : 0;
      if (ta !== tb) return ta - tb;
      return a[0].localeCompare(b[0]);
    })
    .map(([name]) => name);
}

function normalCdf(z) {
  const t = 1 / (1 + 0.2316419 * Math.abs(z));
  const d = 0.3989423 * Math.exp((-z * z) / 2);
  const p =
    d * t * (0.3193815 + t * (-0.3565638 + t * (1.7814779 + t * (-1.821256 + t * 1.3302744))));
  return z >= 0 ? 1 - p : p;
}

function implied(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return 100 / 210;
  if (v < 0) return (-v) / (-v + 100);
  return 100 / (v + 100);
}

/** Grade rows with raw walk-forward model μ (no DK book μ-shift or σ-scale). */
export function roiOnRows(testRows, minEvPct, { marketFilter = null, useRecommendedPolicy = false } = {}) {
  let units = 0;
  let wins = 0;
  let losses = 0;
  let n = 0;
  for (const b of testRows) {
    if (marketFilter && b.market !== marketFilter) continue;
    if (useRecommendedPolicy) {
      if (
        !qualifiesBet({
          market: b.market,
          modelLine: b.modelLine,
          bookLine: b.bookLine,
          context: b.context || {},
          eventName: b.event,
        })
      ) {
        continue;
      }
    }
    const mu = b.modelLine;
    const pOver = modelProbOver(b.market, mu, b.bookLine, b.stubRow, b.meta);
    if (!Number.isFinite(pOver)) continue;
    const pUnder = 1 - pOver;
    let edgeOver = (pOver - implied(b.overOdds)) * 100;
    let edgeUnder = (pUnder - implied(b.underOdds)) * 100;
    ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, b.bookLine));
    const evTh = useRecommendedPolicy ? minEvForMarket(b.market, minEvPct) : minEvPct;
    const pick = pickBetSide(edgeOver, edgeUnder, evTh, mu, b.bookLine);
    if (!pick) continue;
    const res = pick.side === "over" ? b.overRes : b.underRes;
    const odds = pick.side === "over" ? b.overOdds : b.underOdds;
    if (res !== "W" && res !== "L" && res !== "P") continue;
    const pnl = pnlForResult(res, odds);
    units += pnl;
    n++;
    if (res === "W") wins++;
    else if (res === "L") losses++;
  }
  return {
    units,
    bets: n,
    wins,
    losses,
    hit_pct: n > 0 ? (wins / n) * 100 : NaN,
    roi_pct: n > 0 ? (units / n) * 100 : NaN,
  };
}

/**
 * Walk-forward OOS: grade each completed event once with raw model μ vs pre-round DK.
 */
export function runWalkForwardOosReport({ excludeLiveEvent = true } = {}) {
  return Promise.all([fitOutcomeSigmaScales(VS), fitOutcomeMuBiasCorrections(VS)]).then(([scales, muBias]) => {
    setOutcomeSigmaScales(scales);
    setOutcomeMuBiasCorrections(muBias);
    return loadWalkForwardBetRows().then((allRows) => {
    const liveEvent = excludeLiveEvent ? loadCurrentLiveEventName() : "";
    const events = eventOrderFromRows(allRows);

    /** @type {Record<string, Record<string, object>>} */
    const byThreshold = {};
    /** @type {Record<string, Record<string, object>>} */
    const byThresholdPolicy = {};

    for (const th of EV_THRESHOLDS) {
      byThreshold[String(th)] = { __all__: { units: 0, bets: 0, wins: 0, losses: 0 } };
      byThresholdPolicy[String(th)] = { __all__: { units: 0, bets: 0, wins: 0, losses: 0 } };
      for (const m of MARKET_BOOK_CALIBRATION_MARKETS) {
        byThreshold[String(th)][m] = { units: 0, bets: 0, wins: 0, losses: 0 };
        byThresholdPolicy[String(th)][m] = { units: 0, bets: 0, wins: 0, losses: 0 };
      }
    }

    const eventDetails = [];
    const eventDetailsPolicy = [];

    for (const ev of events) {
      if (liveEvent && eventsLikelySame(ev, liveEvent)) continue;
      const test = allRows.filter((r) => r.event === ev);
      if (test.length < 20) continue;

      const perTh = {};
      const perThPolicy = {};

      for (const th of EV_THRESHOLDS) {
        const graded = roiOnRows(test, th);
        const gradedPolicy = roiOnRows(test, th, { useRecommendedPolicy: true });
        perTh[String(th)] = graded;
        perThPolicy[String(th)] = gradedPolicy;

        for (const [bucket, g] of [
          [byThreshold, graded],
          [byThresholdPolicy, gradedPolicy],
        ]) {
          const agg = bucket[String(th)].__all__;
          agg.units += g.units;
          agg.bets += g.bets;
          agg.wins += g.wins;
          agg.losses += g.losses;
        }

        for (const m of MARKET_BOOK_CALIBRATION_MARKETS) {
          const mc = roiOnRows(test, th, { marketFilter: m });
          const ma = byThreshold[String(th)][m];
          ma.units += mc.units;
          ma.bets += mc.bets;
          ma.wins += mc.wins;
          ma.losses += mc.losses;

          const mcp = roiOnRows(test, th, { marketFilter: m, useRecommendedPolicy: true });
          const map = byThresholdPolicy[String(th)][m];
          map.units += mcp.units;
          map.bets += mcp.bets;
          map.wins += mcp.wins;
          map.losses += mcp.losses;
        }
      }

      eventDetails.push({ event: ev, by_threshold: perTh });
      eventDetailsPolicy.push({ event: ev, by_threshold: perThPolicy });
    }

    function finalizeAgg(map) {
      const out = {};
      for (const [th, buckets] of Object.entries(map)) {
        out[th] = {};
        for (const [key, a] of Object.entries(buckets)) {
          out[th][key] = {
            units: Math.round(a.units * 100) / 100,
            bets: a.bets,
            wins: a.wins,
            losses: a.losses,
            hit_pct: a.bets > 0 ? Math.round((a.wins / a.bets) * 1000) / 10 : null,
            roi_pct: a.bets > 0 ? Math.round((a.units / a.bets) * 1000) / 10 : null,
          };
        }
      }
      return out;
    }

    const combined = finalizeAgg(byThreshold);
    const combinedPolicy = finalizeAgg(byThresholdPolicy);

    const policyTh = String(DEFAULT_MIN_EV_PCT);
    const legacyTh = "5";

    const peakEventPolicy = eventDetailsPolicy
      .map((e) => ({
        event: e.event,
        ...(e.by_threshold[policyTh] || {}),
      }))
      .filter((e) => e.bets > 0)
      .sort((a, b) => b.roi_pct - a.roi_pct);

    const thresholdSummaryPolicy = EV_THRESHOLDS.map((th) => ({
      min_ev_pct: th,
      ...(combinedPolicy[String(th)]?.__all__ || {}),
    })).filter((r) => r.bets > 0);

    const bestThresholdPolicy = [...thresholdSummaryPolicy].sort((a, b) => b.roi_pct - a.roi_pct)[0];
    const marketAtPolicy = MARKET_BOOK_CALIBRATION_MARKETS.map((m) => ({
      market: m,
      ...(combinedPolicy[policyTh]?.[m] || {}),
    })).filter((r) => r.bets > 0);

    const recommended = combinedPolicy[policyTh]?.__all__ || null;

    return {
      generated_at: new Date().toISOString(),
      methodology: {
        grading: "walk_forward_oos_one_side_per_line",
        model_lines: "walk_forward_stat_model_no_dk_calibration",
        pricing: "poisson_birdies_normal_other_outcome_sigma",
        odds: "dk_pre_round_audit",
        pricing_mode: "default",
      },
      outcome_sigma_scales: scales,
      outcome_mu_bias_corrections: muBias,
      recommended_policy: {
        min_ev_pct: DEFAULT_MIN_EV_PCT,
        uniform_ev_all_markets: true,
        no_side_or_gap_filters: true,
      },
      excluded_live_event: liveEvent || null,
      events_chronological: events,
      oos_event_count: eventDetailsPolicy.length,
      default_min_ev_pct: DEFAULT_MIN_EV_PCT,
      combined_oos_at_5pct: recommended,
      combined_oos_recommended: recommended,
      combined_oos_unfiltered_at_5pct: combined[legacyTh]?.__all__,
      combined_oos_raw_at_5pct: combined[legacyTh]?.__all__,
      peak_oos_event_at_5pct: peakEventPolicy[0] || null,
      worst_oos_event_at_5pct: peakEventPolicy[peakEventPolicy.length - 1] || null,
      threshold_sweep_oos: thresholdSummaryPolicy,
      best_oos_threshold: bestThresholdPolicy,
      by_market_at_5pct: marketAtPolicy.sort((a, b) => b.roi_pct - a.roi_pct),
      by_market_at_5pct_raw: marketAtPolicy.sort((a, b) => b.roi_pct - a.roi_pct),
      by_threshold: combinedPolicy,
      by_threshold_raw: combined,
      by_threshold_policy: combinedPolicy,
      by_event: eventDetailsPolicy.map((e) => ({
        event: e.event,
        at_5pct: e.by_threshold[policyTh],
        at_5pct_raw: e.by_threshold[policyTh],
        by_threshold: e.by_threshold,
      })),
    };
  });
  });
}
