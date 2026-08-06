/**
 * Golf bettor variance report — bettor-desktop ideas on AlphaCaddie backtests.
 *
 * Bet log from:
 *   1) round O/U walk-forward (vs-actual + model μ)
 *   2) odds.csv model ROI detail (historical open/close)
 *   3) matchup backtest (DK/FD/MGM open/close)
 *
 * Reports: realized vs expected-at-fair-close (gap), CLV in probability points,
 * price-bucket mix, breakeven ladder, season Monte Carlo.
 *
 * Usage: node scripts/report-golf-bettor-variance.mjs
 */
import { readFileSync, writeFileSync, existsSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import {
  collectOosBets,
  eventOrderFromRows,
  loadWalkForwardBetRows,
} from "./walkforward-oos-roi.mjs";
import { num } from "./round-projection-mu.mjs";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { americanToDecimal, pnlForResult } from "../projection-tracker/ev-math.mjs";
import {
  betMixByPrice,
  breakevenLadder,
  computeClv,
  mulberry32,
  simulateSeasonEquity,
  summarizeBetLog,
} from "../projection-tracker/bettor-math.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const OUT = join(WEB, "data", "golf_bettor_variance.json");
const ODDS_DETAIL = join(WEB, "data", "odds_model_roi_detail.csv");
const MATCHUP_DETAIL = join(WEB, "data", "matchup_backtest_detail.csv");
const PROJ = join(WEB, "projections.json");

const LADDER_PRICES = [-200, -150, -130, -110, 100, 120, 150, 200, 300, 400, 600];
const EDGE_FRAC = 0.05;
const MATCHUP_MIN_EDGE = 10;

function parseCsv(path) {
  if (!existsSync(path)) return [];
  const raw = readFileSync(path, "utf8").replace(/^\uFEFF/, "");
  /** @type {object[]} */
  const rows = [];
  return new Promise((resolve, reject) => {
    Readable.from([raw])
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => rows.push(row))
      .on("end", () => resolve(rows))
      .on("error", reject);
  });
}

function loadLiveEventName() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

function roundPropBetFromOos(b) {
  const betAm = b.odds;
  const openAm =
    b.side === "over"
      ? (Number.isFinite(b.openOverOdds) ? b.openOverOdds : betAm)
      : (Number.isFinite(b.openUnderOdds) ? b.openUnderOdds : betAm);
  const closeAm = betAm;
  const oppClose = b.side === "over" ? b.underOdds : b.overOdds;
  const clv = computeClv(openAm, closeAm, oppClose);
  // Expected vs fair close only — do not substitute model edge (that inflates "skill")
  const expectedVsFair = clv?.evVsFair != null ? clv.evVsFair : NaN;
  return {
    source: "round_props",
    event: b.event,
    market: b.market,
    side: b.side,
    odds: betAm,
    openOdds: openAm,
    closeOdds: closeAm,
    res: b.res,
    unitPnl: b.unitPnl,
    modelP: b.modelP,
    edge: b.edge,
    clv,
    expectedVsFair,
    expectedVsRawClose: clv?.evVsRawClose ?? NaN,
  };
}

function collectRoundPropBets(wfRows, { usePolicy, minEv }) {
  return collectOosBets(wfRows, minEv, { useRecommendedPolicy: usePolicy }).map(roundPropBetFromOos);
}

async function loadOddsModelBets() {
  const rows = await parseCsv(ODDS_DETAIL);
  /** @type {object[]} */
  const bets = [];
  for (const r of rows) {
    const openAm = Math.round(num(r.opening_american, NaN));
    const closeAm = Math.round(num(r.closing_american, NaN));
    const res = String(r.result || "").trim().toUpperCase();
    if (!Number.isFinite(openAm) || openAm === 0) continue;
    if (res !== "W" && res !== "L" && res !== "P") continue;
    const unitPnl = Number.isFinite(num(r.pnl_open, NaN))
      ? num(r.pnl_open, NaN)
      : pnlForResult(res, openAm);
    const clv =
      Number.isFinite(closeAm) && closeAm !== 0 ? computeClv(openAm, closeAm, NaN) : null;
    bets.push({
      source: "odds_csv_model",
      event: String(r.event || r.competition || "").trim(),
      market: String(r.market || "").trim(),
      side: String(r.model_pick || "").trim().toLowerCase(),
      odds: openAm,
      openOdds: openAm,
      closeOdds: Number.isFinite(closeAm) ? closeAm : openAm,
      res,
      unitPnl,
      modelP: NaN,
      edge: num(r.model_line_edge, NaN),
      clv,
      expectedVsFair: NaN,
      expectedVsRawClose: clv?.evVsRawClose ?? NaN,
    });
  }
  return bets;
}

function decimalToAmerican(dec) {
  const d = num(dec, NaN);
  if (!Number.isFinite(d) || d <= 1) return NaN;
  if (d >= 2) return Math.round((d - 1) * 100);
  return Math.round(-100 / (d - 1));
}

async function loadMatchupBets(liveEvent) {
  const rows = await parseCsv(MATCHUP_DETAIL);
  /** @type {object[]} */
  const bets = [];
  for (const r of rows) {
    const event = String(r.event_name || "").trim();
    if (!event) continue;
    if (liveEvent && eventsLikelySame(event, liveEvent)) continue;
    const pick = String(r.pick_side_at_10 || "").trim().toLowerCase();
    if (pick !== "p1" && pick !== "p2" && pick !== "p3") continue;
    const edge = num(r[`edge_${pick}_open_pct`], NaN);
    if (!Number.isFinite(edge) || edge < MATCHUP_MIN_EDGE) continue;
    const openDec = num(r[`${pick}_open_dec`], NaN);
    const closeDec = num(r[`${pick}_close_dec`], NaN);
    const openAm = decimalToAmerican(openDec);
    const closeAm = decimalToAmerican(closeDec);
    if (!Number.isFinite(openAm)) continue;
    const res = String(r[`${pick}_result`] || "").trim().toUpperCase();
    if (res !== "W" && res !== "L" && res !== "P") continue;

    const closeDecs = ["p1", "p2", "p3"]
      .map((s) => num(r[`${s}_close_dec`], NaN))
      .filter((d) => Number.isFinite(d) && d > 1);
    const isTwoWay = closeDecs.length === 2;
    let oppCloseAm = NaN;
    if (isTwoWay) {
      for (const s of ["p1", "p2", "p3"]) {
        if (s === pick) continue;
        const d = num(r[`${s}_close_dec`], NaN);
        if (Number.isFinite(d) && d > 1) {
          oppCloseAm = decimalToAmerican(d);
          break;
        }
      }
    }

    // CLV ratio/points use same-side open→close; fair EV only when we can de-vig honestly
    const clv = Number.isFinite(closeAm)
      ? computeClv(openAm, closeAm, isTwoWay ? oppCloseAm : NaN)
      : null;

    let expectedVsFair = NaN;
    if (isTwoWay && clv?.evVsFair != null) {
      expectedVsFair = clv.evVsFair;
    } else if (closeDecs.length >= 2 && Number.isFinite(closeDec) && closeDec > 1) {
      // Multiplicative n-way fair at close, priced at open
      const qSum = closeDecs.reduce((s, d) => s + 1 / d, 0);
      const fair = qSum > 0 ? 1 / closeDec / qSum : NaN;
      if (Number.isFinite(fair) && Number.isFinite(openDec)) {
        expectedVsFair = fair * openDec - 1;
      }
    }

    bets.push({
      source: "matchups",
      event,
      market: String(r.market || r.bet_type || "Matchup").trim(),
      side: pick,
      odds: openAm,
      openOdds: openAm,
      closeOdds: Number.isFinite(closeAm) ? closeAm : openAm,
      res,
      unitPnl: pnlForResult(res, openAm),
      modelP: NaN,
      edge,
      clv,
      expectedVsFair,
      expectedVsRawClose: clv?.evVsRawClose ?? NaN,
    });
  }
  return bets;
}

function bySourceSummary(bets) {
  /** @type {Map<string, object[]>} */
  const m = new Map();
  for (const b of bets) {
    if (!m.has(b.source)) m.set(b.source, []);
    m.get(b.source).push(b);
  }
  return [...m.entries()].map(([source, list]) => ({ source, ...summarizeBetLog(list) }));
}

function byMarketSummary(bets) {
  /** @type {Map<string, object[]>} */
  const m = new Map();
  for (const b of bets) {
    const k = b.market || "unknown";
    if (!m.has(k)) m.set(k, []);
    m.get(k).push(b);
  }
  return [...m.entries()]
    .map(([market, list]) => ({ market, ...summarizeBetLog(list) }))
    .sort((a, b) => (b.realized_units || 0) - (a.realized_units || 0));
}

async function main() {
  const liveEvent = loadLiveEventName();
  console.log("[golf-bettor] Loading walk-forward round props…");
  let wfRows = await loadWalkForwardBetRows();
  if (liveEvent) {
    wfRows = wfRows.filter((r) => !eventsLikelySame(r.event, liveEvent));
  }

  const policyBets = collectRoundPropBets(wfRows, { usePolicy: true, minEv: 5 });
  const unfilteredBets = collectRoundPropBets(wfRows, { usePolicy: false, minEv: 5 });

  console.log("[golf-bettor] Loading odds.csv model ROI bets…");
  const oddsBets = await loadOddsModelBets();

  console.log("[golf-bettor] Loading matchup backtest bets…");
  const matchupBets = await loadMatchupBets(liveEvent);

  const allBets = [...policyBets, ...oddsBets, ...matchupBets];
  const summary = summarizeBetLog(allBets);
  const mix = betMixByPrice(allBets);
  const ladder = breakevenLadder(LADDER_PRICES, EDGE_FRAC);

  // Full replay of round-prop policy bets (has modelP) — honest path uncertainty
  const seasonRound = simulateSeasonEquity(policyBets, {
    paths: 4000,
    seed: 20260805,
    edgeFracFallback: EDGE_FRAC,
  });

  // Classic bettor-desktop demo: 200-bet season drawn from *your* price mix at 5% edge
  const prices = allBets.map((b) => b.odds).filter((a) => Number.isFinite(a) && a !== 0);
  const seasonMixBets = [];
  if (prices.length) {
    const rng = mulberry32(20260805);
    for (let i = 0; i < 200; i++) {
      seasonMixBets.push({ odds: prices[Math.floor(rng() * prices.length)], modelP: NaN });
    }
  }
  const season200 = simulateSeasonEquity(seasonMixBets, {
    paths: 5000,
    seed: 77,
    edgeFracFallback: EDGE_FRAC,
  });

  // Compare -110 vs +400 only (README punchline) with same 200-bet count
  const seasonFav = simulateSeasonEquity(
    Array.from({ length: 200 }, () => ({ odds: -110, modelP: NaN })),
    { paths: 5000, seed: 77, edgeFracFallback: EDGE_FRAC },
  );
  const seasonDog = simulateSeasonEquity(
    Array.from({ length: 200 }, () => ({ odds: 400, modelP: NaN })),
    { paths: 5000, seed: 77, edgeFracFallback: EDGE_FRAC },
  );

  const withClv = allBets.filter((b) => b.clv && Number.isFinite(b.clv.probPoints));
  const byRatio = [...withClv].sort((a, b) => (b.clv.ratio || 0) - (a.clv.ratio || 0)).slice(0, 5);
  const byPoints = [...withClv]
    .sort((a, b) => (b.clv.probPoints || 0) - (a.clv.probPoints || 0))
    .slice(0, 5);

  const report = {
    generated_at: new Date().toISOString(),
    thesis:
      "Price mix drives variance even at fixed EV. Measure CLV in probability points — not odds ratio. Gap (realized − expected vs fair close) is residual luck/skill, not either alone.",
    edge_frac_assumed: EDGE_FRAC,
    excluded_live_event: liveEvent || null,
    sources: {
      round_props_policy: policyBets.length,
      round_props_unfiltered_5pct: unfilteredBets.length,
      odds_csv_model: oddsBets.length,
      matchups_at_10pct: matchupBets.length,
      combined: allBets.length,
    },
    combined: summary,
    by_source: bySourceSummary(allBets),
    by_market: byMarketSummary(allBets),
    round_props_policy: summarizeBetLog(policyBets),
    round_props_unfiltered: summarizeBetLog(unfilteredBets),
    price_mix: mix,
    breakeven_ladder: ladder.map((r) => ({
      american: r.american,
      breakeven_pct: Math.round(r.breakeven * 10000) / 100,
      sd_per_unit: Math.round(r.sdPerUnit * 1000) / 1000,
      bets_to_2sigma: Math.round(r.betsTo2Sigma),
    })),
    season_sim: {
      round_props_policy: {
        ...seasonRound,
        note: "Monte Carlo replaying round-prop policy bets with model win probs.",
      },
      your_mix_200_at_5pct: {
        ...season200,
        note: "200 bets sampled from your historical price mix at a flat 5% edge — shows variance from prices alone.",
      },
      all_minus110_200_at_5pct: seasonFav,
      all_plus400_200_at_5pct: seasonDog,
    },
    clv_ranking_demo: {
      top_by_ratio: byRatio.map((b) => ({
        event: b.event,
        market: b.market,
        open: b.openOdds,
        close: b.closeOdds,
        ratio_pct: Math.round((b.clv.ratio || 0) * 10000) / 100,
        prob_pts: Math.round((b.clv.probPoints || 0) * 10000) / 100,
      })),
      top_by_prob_points: byPoints.map((b) => ({
        event: b.event,
        market: b.market,
        open: b.openOdds,
        close: b.closeOdds,
        ratio_pct: Math.round((b.clv.ratio || 0) * 10000) / 100,
        prob_pts: Math.round((b.clv.probPoints || 0) * 10000) / 100,
      })),
    },
    events_in_round_props: eventOrderFromRows(wfRows).length,
  };

  writeFileSync(OUT, `${JSON.stringify(report, null, 2)}\n`, "utf8");
  console.log(`[golf-bettor] Wrote ${OUT}`);
  console.log(
    `  combined ${summary.bets} bets · realized ${summary.realized_units}u · gap ${summary.gap_realized_minus_fair}u · mean CLV ${summary.mean_clv_prob_pts} pts`,
  );
  console.log(
    `  season 200-mix P(down)=${Number(season200.pctDown).toFixed(1)}% · -110 P(down)=${Number(seasonFav.pctDown).toFixed(1)}% · +400 P(down)=${Number(seasonDog.pctDown).toFixed(1)}%`,
  );
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
