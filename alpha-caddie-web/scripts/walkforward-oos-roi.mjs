/**
 * Walk-forward out-of-sample ROI (no outcome fitting, no live-week training).
 */
import { readFileSync, existsSync, createReadStream } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import {
  MARKET_BOOK_CALIBRATION_MARKETS,
  fitMarketBookParamsFromDeltas,
} from "./market-book-calibration.mjs";
import { EXPORT_MARKETS, num, sigmaForOu } from "./round-projection-mu.mjs";
import { pickBetSide, pnlForResult } from "../projection-tracker/ev-math.mjs";

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
        for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const modelLine = parseLine(row[cols.model]);
          const bookLine = parseLine(row[cols.book]);
          if (!Number.isFinite(modelLine) || !Number.isFinite(bookLine)) continue;
          const stubRow =
            market === "Total score"
              ? { total_score: modelLine, round_sd: 2.75 }
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
            bookLine,
            modelBookDelta: modelLine - bookLine,
            overOdds: num(row[cols.overOdds], NaN),
            underOdds: num(row[cols.underOdds], NaN),
            overRes: String(row[cols.overRes] || "").trim().toUpperCase(),
            underRes: String(row[cols.underRes] || "").trim().toUpperCase(),
            stubRow,
            meta,
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

function fitParamsFromTrainRows(trainRows) {
  const markets = {};
  for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
    const deltas = trainRows.filter((r) => r.market === market).map((r) => r.modelBookDelta);
    markets[market] = fitMarketBookParamsFromDeltas(market, deltas);
  }
  return markets;
}

/** Grade rows with optional calibration params (null = raw model lines). */
export function roiOnRows(testRows, markets, minEvPct, { marketFilter = null } = {}) {
  let units = 0;
  let wins = 0;
  let losses = 0;
  let n = 0;
  for (const b of testRows) {
    if (marketFilter && b.market !== marketFilter) continue;
    const shift = markets?.[b.market]?.mu_shift ?? 0;
    const sigScale = markets?.[b.market]?.sigma_scale ?? 1;
    const mu = b.modelLine + shift;
    const sigBase = sigmaForOu(b.market, b.stubRow, b.meta, 14);
    const sig = sigBase * sigScale;
    const z = (b.bookLine - mu) / sig;
    const pOver = 1 - normalCdf(z);
    const pUnder = 1 - pOver;
    const edgeOver = (pOver - implied(b.overOdds)) * 100;
    const edgeUnder = (pUnder - implied(b.underOdds)) * 100;
    const pick = pickBetSide(edgeOver, edgeUnder, minEvPct);
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
 * Walk-forward OOS: for each event, fit book-alignment on all prior events only, grade once.
 */
export function runWalkForwardOosReport({ excludeLiveEvent = true } = {}) {
  return loadWalkForwardBetRows().then((allRows) => {
    const liveEvent = excludeLiveEvent ? loadCurrentLiveEventName() : "";
    const events = eventOrderFromRows(allRows);
    const oosEvents = events.filter((e) => !liveEvent || !eventsLikelySame(e, liveEvent));

    /** @type {Record<string, Record<string, object>>} */
    const byThreshold = {};
    /** @type {Record<string, Record<string, object>>} */
    const byThresholdRaw = {};
    /** @type {Record<string, Record<string, object[]>>} */
    const byThresholdEvent = {};

    for (const th of EV_THRESHOLDS) {
      byThreshold[String(th)] = { __all__: { units: 0, bets: 0, wins: 0, losses: 0 } };
      byThresholdRaw[String(th)] = { __all__: { units: 0, bets: 0, wins: 0, losses: 0 } };
      byThresholdEvent[String(th)] = {};
      for (const m of MARKET_BOOK_CALIBRATION_MARKETS) {
        byThreshold[String(th)][m] = { units: 0, bets: 0, wins: 0, losses: 0 };
        byThresholdRaw[String(th)][m] = { units: 0, bets: 0, wins: 0, losses: 0 };
      }
    }

    const eventDetails = [];

    for (let i = 1; i < events.length; i++) {
      const ev = events[i];
      if (liveEvent && eventsLikelySame(ev, liveEvent)) continue;
      const prior = events.slice(0, i);
      const train = allRows.filter((r) => prior.includes(r.event));
      const test = allRows.filter((r) => r.event === ev);
      if (train.length < 80 || test.length < 20) continue;

      const wfMarkets = fitParamsFromTrainRows(train);
      const perTh = {};
      const perThRaw = {};

      for (const th of EV_THRESHOLDS) {
        const cal = roiOnRows(test, wfMarkets, th);
        const raw = roiOnRows(test, null, th);
        perTh[String(th)] = cal;
        perThRaw[String(th)] = raw;

        const agg = byThreshold[String(th)].__all__;
        agg.units += cal.units;
        agg.bets += cal.bets;
        agg.wins += cal.wins;
        agg.losses += cal.losses;

        const aggRaw = byThresholdRaw[String(th)].__all__;
        aggRaw.units += raw.units;
        aggRaw.bets += raw.bets;
        aggRaw.wins += raw.wins;
        aggRaw.losses += raw.losses;

        byThresholdEvent[String(th)][ev] = cal;

        for (const m of MARKET_BOOK_CALIBRATION_MARKETS) {
          const mc = roiOnRows(test, wfMarkets, th, { marketFilter: m });
          const ma = byThreshold[String(th)][m];
          ma.units += mc.units;
          ma.bets += mc.bets;
          ma.wins += mc.wins;
          ma.losses += mc.losses;
          const mr = roiOnRows(test, null, th, { marketFilter: m });
          const mar = byThresholdRaw[String(th)][m];
          mar.units += mr.units;
          mar.bets += mr.bets;
          mar.wins += mr.wins;
          mar.losses += mr.losses;
        }
      }

      eventDetails.push({
        event: ev,
        train_events: prior.length,
        by_threshold: perTh,
        by_threshold_raw: perThRaw,
      });
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

    const calibrated = finalizeAgg(byThreshold);
    const raw = finalizeAgg(byThresholdRaw);

    // Peak OOS event at default 5% (single-week high, not optimizable aggregate).
    const defaultTh = "5";
    const peakEventCal = eventDetails
      .map((e) => ({
        event: e.event,
        ...(e.by_threshold[defaultTh] || {}),
      }))
      .filter((e) => e.bets > 0)
      .sort((a, b) => b.roi_pct - a.roi_pct);

    const thresholdSummary = EV_THRESHOLDS.map((th) => ({
      min_ev_pct: th,
      calibrated: calibrated[String(th)]?.__all__,
      raw: raw[String(th)]?.__all__,
    })).filter((r) => r.calibrated?.bets > 0);

    const bestThresholdCal = [...thresholdSummary].sort((a, b) => b.calibrated.roi_pct - a.calibrated.roi_pct)[0];
    const marketAt5 = MARKET_BOOK_CALIBRATION_MARKETS.map((m) => ({
      market: m,
      ...(calibrated[defaultTh]?.[m] || {}),
    })).filter((r) => r.bets > 0);

    return {
      generated_at: new Date().toISOString(),
      methodology: {
        fit: "book_alignment_no_outcome_peek",
        grading: "walk_forward_oos_one_side_per_line",
        calibration: "per_event_fit_on_prior_events_only",
        odds: "dk_pre_round_audit",
        pricing_mode: "default",
      },
      excluded_live_event: liveEvent || null,
      events_chronological: events,
      oos_event_count: eventDetails.length,
      default_min_ev_pct: 5,
      combined_oos_at_5pct: calibrated[defaultTh]?.__all__,
      combined_oos_raw_at_5pct: raw[defaultTh]?.__all__,
      peak_oos_event_at_5pct: peakEventCal[0] || null,
      worst_oos_event_at_5pct: peakEventCal[peakEventCal.length - 1] || null,
      threshold_sweep_oos: thresholdSummary,
      best_oos_threshold_calibrated: bestThresholdCal,
      by_market_at_5pct: marketAt5.sort((a, b) => b.roi_pct - a.roi_pct),
      by_threshold: calibrated,
      by_threshold_raw: raw,
      by_event: eventDetails.map((e) => ({
        event: e.event,
        train_events: e.train_events,
        at_5pct: e.by_threshold["5"],
        at_5pct_raw: e.by_threshold_raw["5"],
        by_threshold: e.by_threshold,
      })),
    };
  });
}
