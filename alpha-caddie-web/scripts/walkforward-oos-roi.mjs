/**
 * Walk-forward out-of-sample ROI — raw model μ vs pre-round DK only (no book calibration).
 */
import { readFileSync, existsSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import {
  DEFAULT_MIN_EV_PCT,
  isActionableMarket,
  minEvForMarket,
  OOS_MARKET_POLICY,
  qualifiesBet,
} from "./bet-policy.mjs";
import { RAW_ROUND_SD } from "./projection-core.mjs";
import { EXPORT_MARKETS, num, modelProbOver } from "./round-projection-mu.mjs";
import {
  fitOutcomeSigmaScales,
  outcomeSigmaScale,
  setOutcomeSigmaScales,
} from "./projection-stat-model.mjs";
import { MARKET_BOOK_CALIBRATION_MARKETS } from "./market-book-calibration.mjs";
import { applyOutcomeMuDebias, bookLineValid, outcomeMuDebiasEnabled } from "./outcome-mu-debias.mjs";
import {
  americanToDecimal,
  capDirectionalPostedEdges,
  computeStakeDollars,
  devigFairTwoWay,
  pickBetSide,
  pnlForResult,
} from "../projection-tracker/ev-math.mjs";

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
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  /** @type {object[]} */
  const rows = [];
  await new Promise((resolve, reject) => {
    Readable.from([aligned])
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
          course_fw_width: num(row.course_fw_width, NaN),
          round: Math.round(num(row.round, NaN)),
        };
        for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const rawModelLine = parseLine(row[cols.model]);
          const bookLine = parseLine(row[cols.book]);
          if (!Number.isFinite(rawModelLine) || !Number.isFinite(bookLine)) continue;
          if (!bookLineValid(market, bookLine)) continue;
          const modelLine = outcomeMuDebiasEnabled()
            ? applyOutcomeMuDebias(market, rawModelLine, bookLine)
            : rawModelLine;
          if (!Number.isFinite(modelLine)) continue;
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
                  : market === "Pars"
                    ? { pars: modelLine }
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
            rawModelLine,
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

/** Collect graded OOS bets (one side per line) for unit ROI and $ bankroll sims. */
export function collectOosBets(testRows, minEvPct, { marketFilter = null, useRecommendedPolicy = false } = {}) {
  /** @type {object[]} */
  const bets = [];
  for (const b of testRows) {
    if (marketFilter && b.market !== marketFilter) continue;
    const mu = b.modelLine;
    if (useRecommendedPolicy) {
      if (
        !qualifiesBet({
          market: b.market,
          modelLine: mu,
          bookLine: b.bookLine,
          context: b.context || {},
          eventName: b.event,
        })
      ) {
        continue;
      }
    }
    const pOver = modelProbOver(b.market, mu, b.bookLine, b.stubRow, b.meta);
    if (!Number.isFinite(pOver)) continue;
    const pUnder = 1 - pOver;
    const { fairOver, fairUnder } = devigFairTwoWay(b.overOdds, b.underOdds);
    let edgeOver = Number.isFinite(fairOver) ? (pOver - fairOver) * 100 : (pOver - implied(b.overOdds)) * 100;
    let edgeUnder = Number.isFinite(fairUnder) ? (pUnder - fairUnder) * 100 : (pUnder - implied(b.underOdds)) * 100;
    ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, b.bookLine));
    const evTh = useRecommendedPolicy ? minEvForMarket(b.market, minEvPct) : minEvPct;
    const pick = pickBetSide(edgeOver, edgeUnder, evTh, mu, b.bookLine);
    if (!pick) continue;
    if (
      useRecommendedPolicy &&
      !qualifiesBet({
        market: b.market,
        modelLine: mu,
        bookLine: b.bookLine,
        context: b.context || {},
        eventName: b.event,
        side: pick.side,
      })
    ) {
      continue;
    }
    const res = pick.side === "over" ? b.overRes : b.underRes;
    const odds = pick.side === "over" ? b.overOdds : b.underOdds;
    if (res !== "W" && res !== "L" && res !== "P") continue;
    const dec = americanToDecimal(odds);
    const modelP = pick.side === "over" ? pOver : pUnder;
    if (!Number.isFinite(dec) || dec <= 1) continue;
    bets.push({
      event: b.event,
      eventMs: b.eventMs,
      round: Math.round(num(b.context?.round, 0)),
      market: b.market,
      side: pick.side,
      edge: pick.edge,
      modelP,
      odds,
      dec,
      res,
      unitPnl: pnlForResult(res, odds),
    });
  }
  return bets;
}

/** Grade rows with calibrated model μ (residual bias + devigged fair EV). */
export function roiOnRows(testRows, minEvPct, opts = {}) {
  const bets = collectOosBets(testRows, minEvPct, opts);
  let units = 0;
  let wins = 0;
  let losses = 0;
  for (const b of bets) {
    units += b.unitPnl;
    if (b.res === "W") wins++;
    else if (b.res === "L") losses++;
  }
  const n = bets.length;
  return {
    units,
    bets: n,
    wins,
    losses,
    hit_pct: n > 0 ? (wins / n) * 100 : NaN,
    roi_pct: n > 0 ? (units / n) * 100 : NaN,
  };
}

function riskKellyMult(method) {
  if (method === "kelly_half") return 0.5;
  return 0.25;
}

/**
 * Dollar bankroll path for a bet ledger. Sequential by event then round.
 * Round exposure capped; Kelly/flat sizing matches the Risk tab.
 */
export function simulateOosMoney(
  bets,
  {
    bankroll = 10000,
    method = "kelly_unit_cap",
    unitPct = 1,
    maxStakePct = 5,
    roundCapPct = 15,
  } = {},
) {
  const B0 = bankroll;
  let br = B0;
  let peak = B0;
  let maxDd = 0;
  let totalStaked = 0;
  let wins = 0;
  let losses = 0;
  let pushes = 0;
  const kellyMult = riskKellyMult(method);
  /** @type {Map<string, { pl: number, staked: number, bets: number }>} */
  const byMarket = new Map();
  /** @type {Map<string, { pl: number, staked: number, bets: number }>} */
  const byEvent = new Map();

  const groups = new Map();
  for (const bet of bets) {
    const key = `${bet.event}\x1f${bet.round || 0}`;
    if (!groups.has(key)) groups.set(key, []);
    groups.get(key).push(bet);
  }
  const keys = [...groups.keys()].sort((a, b) => {
    const ga = groups.get(a)[0];
    const gb = groups.get(b)[0];
    const ta = Number.isFinite(ga?.eventMs) ? ga.eventMs : 0;
    const tb = Number.isFinite(gb?.eventMs) ? gb.eventMs : 0;
    if (ta !== tb) return ta - tb;
    const ev = String(ga?.event || "").localeCompare(String(gb?.event || ""));
    if (ev) return ev;
    return (ga?.round || 0) - (gb?.round || 0);
  });

  let n = 0;
  for (const key of keys) {
    const group = groups.get(key) || [];
    const brBefore = br;
    const sized = [];
    for (const bet of group) {
      const stake = computeStakeDollars(brBefore, bet.modelP, bet.dec, method, {
        bankroll0: B0,
        unitPct,
        maxStakePct,
        kellyMult,
      });
      if (stake > 0) sized.push({ bet, stake });
    }
    const cap = brBefore * (roundCapPct / 100);
    const tot = sized.reduce((s, x) => s + x.stake, 0);
    const scale = tot > cap && cap > 0 ? cap / tot : 1;
    for (const { bet, stake: nom } of sized) {
      const stake = nom * scale;
      if (!(stake > 0)) continue;
      let pnl = 0;
      if (bet.res === "W") {
        pnl = stake * (bet.dec - 1);
        wins++;
      } else if (bet.res === "L") {
        pnl = -stake;
        losses++;
      } else {
        pushes++;
      }
      br += pnl;
      totalStaked += stake;
      n++;
      peak = Math.max(peak, br);
      maxDd = Math.max(maxDd, peak - br);
      const mk = byMarket.get(bet.market) || { pl: 0, staked: 0, bets: 0 };
      mk.pl += pnl;
      mk.staked += stake;
      mk.bets += 1;
      byMarket.set(bet.market, mk);
      const ek = byEvent.get(bet.event) || { pl: 0, staked: 0, bets: 0 };
      ek.pl += pnl;
      ek.staked += stake;
      ek.bets += 1;
      byEvent.set(bet.event, ek);
    }
  }

  const pl = br - B0;
  const roundMoney = (v) => Math.round(v * 100) / 100;
  return {
    method,
    bankroll_start: B0,
    bankroll_end: roundMoney(br),
    pl: roundMoney(pl),
    roi_on_bankroll_pct: B0 > 0 ? Math.round((pl / B0) * 1000) / 10 : null,
    roi_on_staked_pct: totalStaked > 0 ? Math.round((pl / totalStaked) * 1000) / 10 : null,
    total_staked: roundMoney(totalStaked),
    avg_stake: n > 0 ? roundMoney(totalStaked / n) : null,
    max_drawdown: roundMoney(maxDd),
    max_drawdown_pct: peak > 0 ? Math.round((maxDd / peak) * 1000) / 10 : null,
    bets: n,
    wins,
    losses,
    pushes,
    by_market: [...byMarket.entries()].map(([market, a]) => ({
      market,
      pl: roundMoney(a.pl),
      staked: roundMoney(a.staked),
      bets: a.bets,
      roi_on_staked_pct: a.staked > 0 ? Math.round((a.pl / a.staked) * 1000) / 10 : null,
    })),
    by_event: [...byEvent.entries()].map(([event, a]) => ({
      event,
      pl: roundMoney(a.pl),
      staked: roundMoney(a.staked),
      bets: a.bets,
      roi_on_staked_pct: a.staked > 0 ? Math.round((a.pl / a.staked) * 1000) / 10 : null,
    })),
  };
}

const MONEY_METHODS = [
  { id: "flat_fixed", label: "Flat $100 (1% of $10k, fixed)" },
  { id: "kelly_unit_cap", label: "¼ Kelly + 1% cap" },
  { id: "kelly_q", label: "¼ Kelly (max 5%)" },
];

function moneyBlock(bets, bankroll = 10000) {
  /** @type {Record<string, object>} */
  const sims = {};
  for (const m of MONEY_METHODS) {
    sims[m.id] = { label: m.label, ...simulateOosMoney(bets, { bankroll, method: m.id }) };
  }
  return {
    bankroll_start: bankroll,
    unit_pct: 1,
    round_cap_pct: 15,
    ...sims,
  };
}

/**
 * Walk-forward OOS: grade each completed event once with raw model μ vs pre-round DK.
 */
export function runWalkForwardOosReport({ excludeLiveEvent = true } = {}) {
  return Promise.all([fitOutcomeSigmaScales(VS)]).then(([scales]) => {
    setOutcomeSigmaScales(scales);
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
    /** @type {object[]} */
    const oosRows = [];

    for (const ev of events) {
      if (liveEvent && eventsLikelySame(ev, liveEvent)) continue;
      const test = allRows.filter((r) => r.event === ev);
      if (test.length < 20) continue;
      oosRows.push(...test);

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

    const recBets = collectOosBets(oosRows, DEFAULT_MIN_EV_PCT, { useRecommendedPolicy: true });
    const unfilteredBets = collectOosBets(oosRows, 5);
    const money = {
      recommended_policy: moneyBlock(recBets),
      unfiltered_at_5pct: moneyBlock(unfilteredBets),
    };

    const kellyMk = new Map((money.recommended_policy.kelly_unit_cap.by_market || []).map((m) => [m.market, m]));
    const flatMk = new Map((money.recommended_policy.flat_fixed.by_market || []).map((m) => [m.market, m]));
    const kellyEv = new Map((money.recommended_policy.kelly_unit_cap.by_event || []).map((e) => [e.event, e]));
    const flatEv = new Map((money.recommended_policy.flat_fixed.by_event || []).map((e) => [e.event, e]));

    const marketsWithMoney = marketAtPolicy.map((m) => ({
      ...m,
      flat_pl: flatMk.get(m.market)?.pl ?? null,
      kelly_pl: kellyMk.get(m.market)?.pl ?? null,
      flat_staked: flatMk.get(m.market)?.staked ?? null,
      kelly_staked: kellyMk.get(m.market)?.staked ?? null,
      flat_roi_on_staked_pct: flatMk.get(m.market)?.roi_on_staked_pct ?? null,
      kelly_roi_on_staked_pct: kellyMk.get(m.market)?.roi_on_staked_pct ?? null,
    }));

    const peakEventMoney = [...(money.recommended_policy.kelly_unit_cap.by_event || [])]
      .filter((e) => e.bets > 0)
      .sort((a, b) => b.pl - a.pl);

    return {
      generated_at: new Date().toISOString(),
      methodology: {
        grading: "walk_forward_oos_dollar_bankroll",
        model_lines: "walk_forward_stat_model_no_dk_calibration",
        pricing: "poisson_birdies_normal_other_outcome_sigma",
        odds: "dk_pre_round_audit",
        pricing_mode: "default",
        bankroll: "$10k start · flat $100/bet · ¼ Kelly + 1% cap · 15% round cap",
      },
      outcome_sigma_scales: scales,
      recommended_policy: {
        min_ev_pct: DEFAULT_MIN_EV_PCT,
        per_market: OOS_MARKET_POLICY,
        devigged_fair_odds: true,
        outcome_mu_bias_applied: outcomeMuDebiasEnabled(),
        outcome_mu_debias: outcomeMuDebiasEnabled() ? "bias+alpha_book_residual" : "off",
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
      peak_oos_event_money: peakEventMoney[0] || null,
      worst_oos_event_money: peakEventMoney[peakEventMoney.length - 1] || null,
      threshold_sweep_oos: thresholdSummaryPolicy,
      best_oos_threshold: bestThresholdPolicy,
      by_market_at_5pct: [...marketsWithMoney].sort((a, b) => (b.kelly_pl ?? -Infinity) - (a.kelly_pl ?? -Infinity)),
      by_market_at_5pct_raw: [...marketsWithMoney].sort((a, b) => (b.kelly_pl ?? -Infinity) - (a.kelly_pl ?? -Infinity)),
      by_threshold: combinedPolicy,
      by_threshold_raw: combined,
      by_threshold_policy: combinedPolicy,
      money,
      by_event: eventDetailsPolicy.map((e) => ({
        event: e.event,
        at_5pct: e.by_threshold[policyTh],
        at_5pct_raw: e.by_threshold[policyTh],
        by_threshold: e.by_threshold,
        money: {
          flat_pl: flatEv.get(e.event)?.pl ?? null,
          kelly_pl: kellyEv.get(e.event)?.pl ?? null,
          flat_staked: flatEv.get(e.event)?.staked ?? null,
          kelly_staked: kellyEv.get(e.event)?.staked ?? null,
          flat_bets: flatEv.get(e.event)?.bets ?? 0,
          kelly_bets: kellyEv.get(e.event)?.bets ?? 0,
        },
      })),
    };
  });
  });
}
