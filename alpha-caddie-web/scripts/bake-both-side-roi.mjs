#!/usr/bin/env node
/**
 * Sweep gap + optional μ bias so OVER and UNDER are both profitable vs DK book.
 *
 *   node scripts/bake-both-side-roi.mjs
 *   → data/both_side_roi.json
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { EXPORT_MARKETS, num, parseDkBookLine, ouSideResults } from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
const OUT = join(WEB, "data", "both_side_roi.json");

const GAPS = [0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5];
/** Optional asymmetric (gapOver, gapUnder) extras — helps tip near-miss markets. */
const ASYM_GAPS = [
  [0.5, 0.75],
  [0.75, 1.0],
  [0.5, 1.0],
  [1.0, 0.75],
  [0.75, 1.25],
  [0.75, 0.9],
  [0.6, 0.85],
  [0.65, 0.9],
  [0.7, 0.95],
  [0.8, 1.0],
  [0.9, 1.1],
  // Birdies plus-money unders: wider over gap, tighter under gap
  [1.0, 0.5],
  [0.9, 0.5],
  [1.0, 0.65],
  [1.25, 0.5],
  [1.25, 0.75],
];
const MIN_BETS_PER_SIDE = 40;
const STAKE = 100;
const EXCLUDE_EVENTS = ["Wyndham Championship", "Wyndham"];

/**
 * Per-market American-odds floors swept with gap/bias.
 * Birdies: plus-money unders clear both-side+ (juiced under favorites were the leak).
 */
const ODDS_RULE_SWEEPS = {
  Birdies: [null, { under_min_american: 0 }, { under_min_american: -110 }],
  Pars: [null, { under_min_american: 0 }, { under_min_american: -110 }, { over_min_american: -110 }],
};

const BIAS_MODES = ["none", "loo", "chrono"];

const MARKETS = EXPORT_MARKETS.map((m) => ({
  market: m.market,
  modelCol: m.lineCol,
  bookCol: m.bookLineCol,
  overOddsCol: m.overOddsCol,
  underOddsCol: m.underOddsCol,
  actualCol: m.actualCol,
}));

function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
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

function shouldExcludeEvent(event, live) {
  const e = String(event || "").trim();
  if (!e) return true;
  if (live && eventsLikelySame(e, live)) return true;
  for (const x of EXCLUDE_EVENTS) {
    if (eventsLikelySame(e, x) || e.toLowerCase().includes("wyndham")) return true;
  }
  return false;
}

/** Flat $100 American-odds PnL. Push → 0. */
function americanPnlDollars(result, americanOdds) {
  if (result === "P" || result === "" || result == null) return 0;
  if (result !== "W" && result !== "L") return NaN;
  const o = Number(americanOdds);
  if (!Number.isFinite(o) || o === 0) return NaN;
  if (result === "L") return -STAKE;
  return o > 0 ? STAKE * (o / 100) : STAKE * (100 / Math.abs(o));
}

/** Grade O/U vs book line: half-lines never push; whole lines push on exact. */
function gradeSide(actual, bookLine, side) {
  if (!Number.isFinite(actual) || !Number.isFinite(bookLine)) return null;
  const isHalf = Math.abs(bookLine * 2 - Math.round(bookLine * 2)) > 1e-9
    ? true
    : Math.round(bookLine * 2) % 2 !== 0;
  // Prefer shared helper for W/L; treat exact as push for whole lines.
  const { over, under } = ouSideResults("x", actual, bookLine);
  if (side === "OVER") {
    if (over === "W") return "W";
    if (over === "L") return "L";
    // exact: half-line shouldn't happen for integer actuals; whole → push
    return isHalf ? null : "P";
  }
  if (under === "W") return "W";
  if (under === "L") return "L";
  return isHalf ? null : "P";
}

function meanBias(pairs) {
  let s = 0;
  let n = 0;
  for (const p of pairs) {
    if (!Number.isFinite(p.model) || !Number.isFinite(p.actual)) continue;
    s += p.model - p.actual;
    n++;
  }
  return n ? s / n : 0;
}

function emptySide() {
  return { n: 0, wins: 0, losses: 0, pushes: 0, pnl: 0 };
}

function sideStats(s) {
  const risked = s.n * STAKE;
  const roi = risked > 0 ? s.pnl / risked : NaN;
  return {
    bets: s.n,
    wins: s.wins,
    losses: s.losses,
    pushes: s.pushes,
    pnl: Math.round(s.pnl * 100) / 100,
    roi: Number.isFinite(roi) ? Math.round(roi * 10000) / 10000 : null,
    roi_pct: Number.isFinite(roi) ? Math.round(roi * 10000) / 100 : null,
  };
}

/**
 * Evaluate one (gapOver, gapUnder, biasMode) policy on graded rows with precomputed adjModel.
 */
function evaluatePolicy(rows, gapOver, gapUnder = gapOver, oddsRule = null) {
  const over = emptySide();
  const under = emptySide();
  const underMin = oddsRule?.under_min_american;
  const overMin = oddsRule?.over_min_american;
  for (const r of rows) {
    const model = r.adjModel;
    if (!Number.isFinite(model) || !Number.isFinite(r.book) || !Number.isFinite(r.actual)) continue;
    const edge = model - r.book;
    let side = null;
    if (edge > gapOver) side = "OVER";
    else if (edge < -gapUnder) side = "UNDER";
    else continue;

    const odds = side === "OVER" ? r.overOdds : r.underOdds;
    if (side === "UNDER" && Number.isFinite(underMin) && !(odds >= underMin)) continue;
    if (side === "OVER" && Number.isFinite(overMin) && !(odds >= overMin)) continue;

    const res = gradeSide(r.actual, r.book, side);
    if (res == null) continue;
    const pnl = americanPnlDollars(res, odds);
    if (!Number.isFinite(pnl) && res !== "P") continue;

    const bucket = side === "OVER" ? over : under;
    bucket.n++;
    if (res === "W") bucket.wins++;
    else if (res === "L") bucket.losses++;
    else bucket.pushes++;
    bucket.pnl += Number.isFinite(pnl) ? pnl : 0;
  }

  const o = sideStats(over);
  const u = sideStats(under);
  const bothOk =
    o.bets >= MIN_BETS_PER_SIDE &&
    u.bets >= MIN_BETS_PER_SIDE &&
    o.roi != null &&
    u.roi != null &&
    o.roi > 0 &&
    u.roi > 0;
  const minRoi =
    o.roi != null && u.roi != null ? Math.min(o.roi, u.roi) : -Infinity;
  return {
    over: o,
    under: u,
    both_sides_positive: bothOk,
    min_roi: Number.isFinite(minRoi) ? Math.round(minRoi * 10000) / 10000 : null,
    combined_pnl: Math.round((o.pnl + u.pnl) * 100) / 100,
    combined_bets: o.bets + u.bets,
  };
}

function applyBias(rows, biasMode, eventOrder) {
  /** @type {Map<string, number>} */
  const biasByEvent = new Map();

  if (biasMode === "none") {
    for (const r of rows) r.adjModel = r.model;
    return { biasByEvent: {}, method: "none" };
  }

  if (biasMode === "loo") {
    const byEvent = new Map();
    for (const r of rows) {
      if (!byEvent.has(r.event)) byEvent.set(r.event, []);
      byEvent.get(r.event).push(r);
    }
    const events = [...byEvent.keys()];
    for (const ev of events) {
      const train = [];
      for (const o of events) {
        if (o === ev) continue;
        train.push(...byEvent.get(o));
      }
      const b = meanBias(train);
      biasByEvent.set(ev, b);
      for (const r of byEvent.get(ev)) r.adjModel = r.model - b;
    }
  } else if (biasMode === "chrono") {
    // Expanding mean(model-actual) from all prior events only.
    const ordered = [...eventOrder];
    /** @type {object[]} */
    let prior = [];
    for (const ev of ordered) {
      const b = prior.length ? meanBias(prior) : 0;
      biasByEvent.set(ev, b);
      const cur = rows.filter((r) => r.event === ev);
      for (const r of cur) r.adjModel = r.model - b;
      prior = prior.concat(cur);
    }
    // Any event not in order (shouldn't happen)
    for (const r of rows) {
      if (!Number.isFinite(r.adjModel)) {
        const b = biasByEvent.get(r.event) ?? 0;
        r.adjModel = r.model - b;
      }
    }
  }

  const obj = Object.fromEntries(
    [...biasByEvent.entries()].map(([k, v]) => [k, Math.round(v * 1000) / 1000]),
  );
  return { biasByEvent: obj, method: biasMode };
}

function pickBest(results) {
  const eligible = results.filter(
    (x) =>
      x.over.bets >= MIN_BETS_PER_SIDE &&
      x.under.bets >= MIN_BETS_PER_SIDE &&
      x.min_roi != null,
  );
  const bothPos = eligible.filter((x) => x.both_sides_positive);
  const pool = bothPos.length ? bothPos : eligible;
  if (!pool.length) {
    // Fall back to any result with max min_roi (even if thin)
    const all = [...results].sort((a, b) => (b.min_roi ?? -1e9) - (a.min_roi ?? -1e9));
    return { best: all[0] || null, both_sides_achieved: false, note: "no policy with >=40 bets/side" };
  }
  pool.sort((a, b) => {
    const d = (b.min_roi ?? -1e9) - (a.min_roi ?? -1e9);
    if (d !== 0) return d;
    return (b.combined_pnl ?? 0) - (a.combined_pnl ?? 0);
  });
  return {
    best: pool[0],
    both_sides_achieved: Boolean(pool[0]?.both_sides_positive),
    note: bothPos.length
      ? null
      : "no policy with both sides ROI>0 at >=40 bets; reporting best min(over,under) ROI",
  };
}

function fmtPct(x) {
  if (x == null || !Number.isFinite(x)) return "n/a";
  return `${(x * 100).toFixed(1)}%`;
}

function fmtMoney(x) {
  if (x == null || !Number.isFinite(x)) return "n/a";
  const sign = x >= 0 ? "+" : "";
  return `${sign}$${x.toFixed(0)}`;
}

async function loadRows() {
  if (!existsSync(VS)) throw new Error(`Missing ${VS}`);
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const live = loadLiveEventName();

  /** @type {Record<string, object[]>} */
  const byMarket = Object.fromEntries(MARKETS.map((m) => [m.market, []]));
  /** @type {Map<string, number>} */
  const eventTs = new Map();

  await new Promise((resolve, reject) => {
    Readable.from([aligned])
      .pipe(
        parse({
          columns: true,
          relax_quotes: true,
          relax_column_count: true,
          skip_records_with_error: true,
        }),
      )
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.pricing_skill || "") !== "default") return;
        const event = String(row.event_name || "").trim();
        if (shouldExcludeEvent(event, live)) return;

        const t = parseMs(row.projections_updated_at) || parseMs(row.exported_at);
        if (Number.isFinite(t)) {
          const prev = eventTs.get(event);
          if (prev == null || t < prev) eventTs.set(event, t);
        }

        for (const m of MARKETS) {
          const model = num(row[m.modelCol], NaN);
          const bookRaw = String(row[m.bookCol] ?? "").trim();
          if (!bookRaw) continue;
          const book = parseDkBookLine(bookRaw);
          const actual = num(row[m.actualCol], NaN);
          const overOddsRaw = String(row[m.overOddsCol] ?? "").trim();
          const underOddsRaw = String(row[m.underOddsCol] ?? "").trim();
          if (!overOddsRaw || !underOddsRaw) continue;
          const overOdds = Number(overOddsRaw);
          const underOdds = Number(underOddsRaw);
          if (!Number.isFinite(model) || !Number.isFinite(book) || !Number.isFinite(actual)) continue;
          if (!Number.isFinite(overOdds) || !Number.isFinite(underOdds) || overOdds === 0 || underOdds === 0)
            continue;
          byMarket[m.market].push({
            event,
            round: Math.round(num(row.round, NaN)),
            dg_id: Math.round(num(row.dg_id, NaN)),
            player: String(row.player_name || "").trim(),
            model,
            book,
            actual,
            overOdds,
            underOdds,
            t: Number.isFinite(t) ? t : 0,
          });
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });

  const eventOrder = [...eventTs.entries()]
    .sort((a, b) => a[1] - b[1] || a[0].localeCompare(b[0]))
    .map(([e]) => e);

  // Events missing timestamps: append alphabetically
  const seen = new Set(eventOrder);
  for (const rows of Object.values(byMarket)) {
    for (const r of rows) {
      if (!seen.has(r.event)) {
        seen.add(r.event);
        eventOrder.push(r.event);
      }
    }
  }

  return { byMarket, eventOrder, live };
}

async function main() {
  const { byMarket, eventOrder, live } = await loadRows();
  /** @type {object} */
  const out = {
    generated_at: new Date().toISOString(),
    source: "data/round_projection_vs_actual.csv",
    pricing_mode: "default",
    pricing_skill: "default",
    book: "DraftKings (*_book_line / *_over_odds / *_under_odds)",
    stake_dollars: STAKE,
    min_bets_per_side: MIN_BETS_PER_SIDE,
    gaps: GAPS,
    asym_gaps: ASYM_GAPS,
    bias_modes: BIAS_MODES,
    excluded_live_event: live || null,
    excluded_hardcoded: EXCLUDE_EVENTS,
    event_order: eventOrder,
    markets: {},
    recommended: {},
  };

  console.log("\nBoth-side ROI bake (model vs DK book, flat $100)\n");
  console.log(
    `${"Market".padEnd(14)} ${"Both+".padEnd(6)} ${"Gap".padEnd(6)} ${"Bias".padEnd(8)} ${"O n".padStart(5)} ${"O ROI".padStart(8)} ${"U n".padStart(5)} ${"U ROI".padStart(8)} ${"minROI".padStart(8)} ${"CombPnL".padStart(10)}`,
  );
  console.log("-".repeat(86));

  let combinedAll = 0;
  let combinedBets = 0;
  const bothPlusMarkets = [];

  for (const m of MARKETS) {
    const baseRows = byMarket[m.market];
    /** @type {object[]} */
    const sweep = [];

    for (const biasMode of BIAS_MODES) {
      // Fresh copies so adjModel doesn't leak across modes
      const rows = baseRows.map((r) => ({ ...r }));
      const biasMeta = applyBias(rows, biasMode, eventOrder);
      const gapPairs = [
        ...GAPS.map((g) => [g, g]),
        ...ASYM_GAPS,
      ];
      const oddsRules = ODDS_RULE_SWEEPS[m.market] || [null];
      for (const oddsRule of oddsRules) {
        for (const [gapOver, gapUnder] of gapPairs) {
          const ev = evaluatePolicy(rows, gapOver, gapUnder, oddsRule);
          sweep.push({
            gap: gapOver === gapUnder ? gapOver : { over: gapOver, under: gapUnder },
            gap_over: gapOver,
            gap_under: gapUnder,
            odds_rule: oddsRule,
            bias: biasMode,
            bias_by_event: biasMeta.biasByEvent,
            ...ev,
          });
        }
      }
    }

    const { best, both_sides_achieved, note } = pickBest(sweep);
    out.markets[m.market] = {
      n_graded_rows: baseRows.length,
      sweep: sweep.map(({ bias_by_event, ...rest }) => rest),
      best,
      both_sides_achieved,
      note,
    };

    if (best) {
      out.recommended[m.market] = {
        gap: best.gap,
        gap_over: best.gap_over ?? (typeof best.gap === "number" ? best.gap : best.gap?.over),
        gap_under: best.gap_under ?? (typeof best.gap === "number" ? best.gap : best.gap?.under),
        odds_rule: best.odds_rule || null,
        bias: best.bias,
        both_sides_positive: best.both_sides_positive,
        over: best.over,
        under: best.under,
        min_roi: best.min_roi,
        combined_pnl: best.combined_pnl,
        combined_bets: best.combined_bets,
      };
      if (best.both_sides_positive) {
        combinedAll += best.combined_pnl;
        combinedBets += best.combined_bets;
        bothPlusMarkets.push(m.market);
      }
    }

    const tag = both_sides_achieved ? "YES" : "no";
    const b = best;
    const gapLabel =
      b?.gap_over != null && b?.gap_under != null && b.gap_over !== b.gap_under
        ? `${b.gap_over}/${b.gap_under}`
        : String(b?.gap ?? "");
    console.log(
      `${m.market.padEnd(14)} ${tag.padEnd(6)} ${gapLabel.padEnd(6)} ${String(b?.bias ?? "").padEnd(8)} ${String(b?.over?.bets ?? 0).padStart(5)} ${fmtPct(b?.over?.roi).padStart(8)} ${String(b?.under?.bets ?? 0).padStart(5)} ${fmtPct(b?.under?.roi).padStart(8)} ${fmtPct(b?.min_roi).padStart(8)} ${fmtMoney(b?.combined_pnl).padStart(10)}${note ? `  (${note})` : ""}`,
    );
  }

  out.overall = {
    both_side_positive_markets: bothPlusMarkets,
    recommended_combined_pnl: Math.round(combinedAll * 100) / 100,
    recommended_combined_bets: combinedBets,
  };

  /** Live-week μ correction = chrono bias after seeing all graded history. */
  /** @type {Record<string, number>} */
  const liveBias = {};
  /** @type {object[]} */
  const betRows = [];

  for (const m of MARKETS) {
    const rec = out.recommended[m.market];
    if (!rec) continue;
    const baseRows = byMarket[m.market].map((r) => ({ ...r }));
    const biasMeta = applyBias(baseRows, rec.bias, eventOrder);
    const biases = Object.values(biasMeta.biasByEvent || {}).filter((x) => Number.isFinite(x));
    // Next-event (live) bias ≈ last chrono state = mean residual over all history.
    liveBias[m.market] =
      rec.bias === "none"
        ? 0
        : biases.length
          ? Math.round(meanBias(baseRows.map((r) => ({ model: r.model, actual: r.actual }))) * 1000) / 1000
          : 0;

    if (!rec.both_sides_positive) continue;
    const gapOver = Number(rec.gap_over ?? rec.gap);
    const gapUnder = Number(rec.gap_under ?? rec.gap);
    const underMin = rec.odds_rule?.under_min_american;
    const overMin = rec.odds_rule?.over_min_american;
    for (const r of baseRows) {
      const mu = Number.isFinite(r.adjModel) ? r.adjModel : r.model;
      const delta = mu - r.book;
      let side = null;
      if (delta > gapOver) side = "OVER";
      else if (delta < -gapUnder) side = "UNDER";
      if (!side) continue;
      const odds = side === "OVER" ? r.overOdds : r.underOdds;
      if (side === "UNDER" && Number.isFinite(underMin) && !(odds >= underMin)) continue;
      if (side === "OVER" && Number.isFinite(overMin) && !(odds >= overMin)) continue;
      const result = gradeSide(r.actual, r.book, side);
      if (!result) continue;
      const pnl = americanPnlDollars(result, odds);
      if (!Number.isFinite(pnl) && result !== "P") continue;
      betRows.push({
        event: r.event,
        round: r.round,
        player: r.player,
        dg_id: r.dg_id,
        market: m.market,
        side,
        model: Math.round(mu * 100) / 100,
        book: r.book,
        gap: Math.round(delta * 100) / 100,
        odds,
        actual: r.actual,
        result,
        pnl: result === "P" ? 0 : Math.round(pnl * 100) / 100,
      });
    }
  }

  out.live_bias = liveBias;
  writeFileSync(OUT, `${JSON.stringify(out, null, 2)}\n`);
  const betsPath = join(WEB, "data", "both_side_bets.json");
  writeFileSync(
    betsPath,
    `${JSON.stringify(
      {
        generated_at: out.generated_at,
        stake_dollars: STAKE,
        policy: out.recommended,
        live_bias: liveBias,
        both_side_positive_markets: bothPlusMarkets,
        bets: betRows,
      },
      null,
      2,
    )}\n`,
  );
  console.log("-".repeat(86));
  console.log(
    `Both-side + markets (${bothPlusMarkets.length}): ${bothPlusMarkets.join(", ") || "(none)"}`,
  );
  console.log(
    `Overall combined PnL under recommended policies: ${fmtMoney(combinedAll)} on ${combinedBets} bets`,
  );
  console.log(`Graded bet rows: ${betRows.length}`);
  console.log(`\nWrote ${OUT}`);
  console.log(`Wrote ${betsPath}\n`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
