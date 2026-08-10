#!/usr/bin/env node
/**
 * Sweep gap (raw model μ — no chrono/loo bias) so OVER and UNDER are both profitable
 * vs all sportsbooks (DraftKings, PrizePicks, Sleeper, Underdog, FanDuel, Caesars, Kalshi).
 *
 *   node scripts/bake-both-side-roi.mjs
 *   → data/both_side_roi.json + data/both_side_bets.json
 *
 * Policy fit excludes the live event in projections.json (no leakage into gap pick).
 * Graded bets always include completed live-week rounds under the frozen policy.
 *
 * Bias modes are locked to "none" (raw hierarchical / export μ with weather + wave already in lines).
 * Opt back into chrono/loo sweep: GOLF_BOTH_SIDE_BIAS_SWEEP=1
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import {
  EXPORT_MARKETS,
  num,
  parseDkBookLine,
  parsePpBookLine,
  ouSideResults,
} from "./round-projection-mu.mjs";

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
/** Soft floor so GIR (39 overs at gap 0.5/0.75) can clear both-side+ without thinning other markets. */
const MIN_BETS_SOFT = 35;
const STAKE = 100;

/**
 * Per-market American-odds floors swept with gap/bias.
 * Birdies: plus-money unders clear both-side+ (juiced under favorites were the leak).
 */
const ODDS_RULE_SWEEPS = {
  Birdies: [null, { under_min_american: 0 }, { under_min_american: -110 }],
  Pars: [null, { under_min_american: 0 }, { under_min_american: -110 }, { over_min_american: -110 }],
};

const BIAS_MODES =
  String(process.env.GOLF_BOTH_SIDE_BIAS_SWEEP || "")
    .trim()
    .toLowerCase() === "1" ||
  String(process.env.GOLF_BOTH_SIDE_BIAS_SWEEP || "")
    .trim()
    .toLowerCase() === "true"
    ? ["none", "loo", "chrono"]
    : ["none"];

/** All O/U sportsbooks present in round_projection_vs_actual.csv. */
const BOOKS = [
  { id: "draftkings", label: "DraftKings", lineKey: "bookLineCol", overKey: "overOddsCol", underKey: "underOddsCol", wholeLine: false },
  { id: "prizepicks", label: "PrizePicks", lineKey: "ppLineCol", overKey: "ppOverOddsCol", underKey: "ppUnderOddsCol", wholeLine: true },
  { id: "sleeper", label: "Sleeper", lineKey: "slLineCol", overKey: "slOverOddsCol", underKey: "slUnderOddsCol", wholeLine: true },
  { id: "underdog", label: "Underdog", lineKey: "udLineCol", overKey: "udOverOddsCol", underKey: "udUnderOddsCol", wholeLine: true },
  { id: "fanduel", label: "FanDuel", lineKey: "fdLineCol", overKey: "fdOverOddsCol", underKey: "fdUnderOddsCol", wholeLine: false },
  { id: "caesars", label: "Caesars", lineKey: "czrLineCol", overKey: "czrOverOddsCol", underKey: "czrUnderOddsCol", wholeLine: false },
  { id: "kalshi", label: "Kalshi", lineKey: "klLineCol", overKey: "klOverOddsCol", underKey: "klUnderOddsCol", wholeLine: false },
];

const MARKETS = EXPORT_MARKETS.map((m) => ({
  market: m.market,
  modelCol: m.lineCol,
  actualCol: m.actualCol,
  spec: m,
}));

function parseBookLine(raw, wholeLine) {
  return wholeLine ? parsePpBookLine(raw) : parseDkBookLine(raw);
}

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

function isLiveEvent(event, live) {
  const e = String(event || "").trim();
  const L = String(live || "").trim();
  if (!e || !L) return false;
  return eventsLikelySame(e, L);
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

function rowIdentityKey(r) {
  return `${r.dg_id}|${r.event}|${r.round}`;
}

function uniqueModelRows(rows) {
  const seen = new Map();
  for (const r of rows) {
    const k = rowIdentityKey(r);
    if (!seen.has(k)) seen.set(k, r);
  }
  return [...seen.values()];
}

function meanBias(pairs) {
  let s = 0;
  let n = 0;
  for (const p of uniqueModelRows(pairs)) {
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
    o.bets >= MIN_BETS_SOFT &&
    u.bets >= MIN_BETS_SOFT &&
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
      prior = prior.concat(uniqueModelRows(cur));
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
  const meets = (x, n) => x.over.bets >= n && x.under.bets >= n && x.min_roi != null;
  const bothPos = results.filter((x) => x.both_sides_positive && meets(x, MIN_BETS_SOFT));
  // Prefer hard sample (>=40/side) when any both-side+ policy clears it (keeps Bogeys/FW thick).
  const bothHard = bothPos.filter((x) => meets(x, MIN_BETS_PER_SIDE));
  const eligibleHard = results.filter((x) => meets(x, MIN_BETS_PER_SIDE));
  const pool = bothHard.length
    ? bothHard
    : bothPos.length
      ? bothPos
      : eligibleHard;
  if (!pool.length) {
    const all = [...results].sort((a, b) => (b.min_roi ?? -1e9) - (a.min_roi ?? -1e9));
    return {
      best: all[0] || null,
      both_sides_achieved: false,
      note: `no policy with >=${MIN_BETS_PER_SIDE} bets/side`,
    };
  }
  pool.sort((a, b) => {
    const d = (b.min_roi ?? -1e9) - (a.min_roi ?? -1e9);
    if (d !== 0) return d;
    return (b.combined_pnl ?? 0) - (a.combined_pnl ?? 0);
  });
  return {
    best: pool[0],
    both_sides_achieved: Boolean(pool[0]?.both_sides_positive),
    note: bothHard.length
      ? null
      : bothPos.length
        ? `both-side+ at soft >=${MIN_BETS_SOFT} bets/side`
        : `no policy with both sides ROI>0 at >=${MIN_BETS_SOFT} bets; reporting best min(over,under) ROI`,
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

/** Reject stub 0/0/0 counting actuals that falsely grade Pars UNDER as wins. */
function countingActualTrusted(row, market, actual) {
  if (!Number.isFinite(actual)) return false;
  if (!["Pars", "Birdies", "Bogeys"].includes(market)) return true;
  const score = num(row.actual_round_score, NaN);
  const b = num(row.actual_birdies, NaN);
  const p = num(row.actual_pars, NaN);
  const bg = num(row.actual_bogeys, NaN);
  if (Number.isFinite(score) && score > 0) {
    const b0 = !Number.isFinite(b) || b === 0;
    const p0 = !Number.isFinite(p) || p === 0;
    const bg0 = !Number.isFinite(bg) || bg === 0;
    if (b0 && p0 && bg0) return false;
  }
  // 0 pars with a completed score and few bird/bog holes is almost always a missing-stat stub.
  if (market === "Pars" && actual === 0 && Number.isFinite(score) && score > 0) {
    const sum = (Number.isFinite(b) ? b : 0) + (Number.isFinite(bg) ? bg : 0);
    if (sum < 12) return false;
  }
  return true;
}

async function loadRows() {
  if (!existsSync(VS)) throw new Error(`Missing ${VS}`);
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const live = loadLiveEventName();

  /** @type {Record<string, object[]>} */
  const byMarket = Object.fromEntries(MARKETS.map((m) => [m.market, []]));
  /** Live event rows — graded under frozen policy, never used in gap fit. */
  /** @type {Record<string, object[]>} */
  const byMarketLive = Object.fromEntries(MARKETS.map((m) => [m.market, []]));
  /** @type {Map<string, number>} */
  const eventTs = new Map();
  /** @type {Record<string, number>} */
  const bookCounts = Object.fromEntries(BOOKS.map((b) => [b.id, 0]));
  /** @type {Record<string, number>} */
  const bookCountsLive = Object.fromEntries(BOOKS.map((b) => [b.id, 0]));

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
        if (!event) return;
        const liveWeek = isLiveEvent(event, live);
        const dest = liveWeek ? byMarketLive : byMarket;
        const counts = liveWeek ? bookCountsLive : bookCounts;

        const t = parseMs(row.projections_updated_at) || parseMs(row.exported_at);
        if (!liveWeek && Number.isFinite(t)) {
          const prev = eventTs.get(event);
          if (prev == null || t < prev) eventTs.set(event, t);
        }

        for (const m of MARKETS) {
          const model = num(row[m.modelCol], NaN);
          const actual = num(row[m.actualCol], NaN);
          if (!Number.isFinite(model) || !Number.isFinite(actual)) continue;
          if (!countingActualTrusted(row, m.market, actual)) continue;
          const round = Math.round(num(row.round, NaN));
          const dg_id = Math.round(num(row.dg_id, NaN));
          const player = String(row.player_name || "").trim();

          for (const bk of BOOKS) {
            const lineCol = m.spec[bk.lineKey];
            const overCol = m.spec[bk.overKey];
            const underCol = m.spec[bk.underKey];
            if (!lineCol || !overCol || !underCol) continue;
            const bookRaw = String(row[lineCol] ?? "").trim();
            if (!bookRaw) continue;
            const book = parseBookLine(bookRaw, bk.wholeLine);
            const overOddsRaw = String(row[overCol] ?? "").trim();
            const underOddsRaw = String(row[underCol] ?? "").trim();
            if (!overOddsRaw || !underOddsRaw) continue;
            const overOdds = Number(overOddsRaw);
            const underOdds = Number(underOddsRaw);
            if (!Number.isFinite(book)) continue;
            if (!Number.isFinite(overOdds) || !Number.isFinite(underOdds) || overOdds === 0 || underOdds === 0)
              continue;
            counts[bk.id]++;
            dest[m.market].push({
              event,
              round,
              dg_id,
              player,
              market: m.market,
              model,
              book,
              actual,
              overOdds,
              underOdds,
              book_id: bk.id,
              book_label: bk.label,
              t: Number.isFinite(t) ? t : 0,
              live_week: liveWeek,
            });
          }
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

  return { byMarket, byMarketLive, eventOrder, live, bookCounts, bookCountsLive };
}

/** Append policy-matching graded bets from rows into betRows. */
function appendGradedBets(betRows, rows, rec, liveWeek) {
  const gapOver = Number(rec.gap_over ?? rec.gap);
  const gapUnder = Number(rec.gap_under ?? rec.gap);
  const underMin = rec.odds_rule?.under_min_american;
  const overMin = rec.odds_rule?.over_min_american;
  for (const r of rows) {
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
    const ts = Number(r.t) || 0;
    betRows.push({
      event: r.event,
      round: r.round,
      date: ts ? new Date(ts).toISOString().slice(0, 10) : "",
      ts: ts || null,
      player: r.player,
      dg_id: r.dg_id,
      market: r.market,
      book_id: r.book_id,
      book_label: r.book_label,
      side,
      model: Math.round(mu * 100) / 100,
      book: r.book,
      gap: Math.round(delta * 100) / 100,
      odds,
      actual: r.actual,
      result,
      pnl: result === "P" ? 0 : Math.round(pnl * 100) / 100,
      live_week: Boolean(liveWeek),
    });
  }
}

async function main() {
  const { byMarket, byMarketLive, eventOrder, live, bookCounts, bookCountsLive } = await loadRows();
  /** @type {object} */
  const out = {
    generated_at: new Date().toISOString(),
    source: "data/round_projection_vs_actual.csv",
    pricing_mode: "default",
    pricing_skill: "default",
    books: BOOKS.map((b) => ({ id: b.id, label: b.label })),
    book_graded_rows: bookCounts,
    book_graded_rows_live: bookCountsLive,
    stake_dollars: STAKE,
    min_bets_per_side: MIN_BETS_PER_SIDE,
    gaps: GAPS,
    asym_gaps: ASYM_GAPS,
    bias_modes: BIAS_MODES,
    /** Live event is graded into bets but excluded from policy fit. */
    excluded_live_event_from_fit: live || null,
    excluded_live_event: live || null,
    event_order: eventOrder,
    markets: {},
    recommended: {},
  };

  const liveFitRows = Object.values(byMarketLive).reduce((n, rows) => n + rows.length, 0);
  console.log("\nBoth-side ROI bake (model vs all sportsbooks, flat $100)\n");
  console.log(
    `Graded book rows (fit): ${BOOKS.map((b) => `${b.label}=${bookCounts[b.id] || 0}`).join(" · ")}`,
  );
  console.log(
    `Graded book rows (live ${live || "—"}): ${BOOKS.map((b) => `${b.label}=${bookCountsLive[b.id] || 0}`).join(" · ")} (${liveFitRows} market rows)\n`,
  );
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
      n_graded_rows_live: byMarketLive[m.market]?.length || 0,
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
        combined_roi:
          best.combined_bets > 0 && Number.isFinite(best.combined_pnl)
            ? Math.round((best.combined_pnl / (best.combined_bets * STAKE)) * 10000) / 10000
            : null,
      };
      combinedAll += best.combined_pnl;
      combinedBets += best.combined_bets;
      if (best.both_sides_positive) bothPlusMarkets.push(m.market);
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
    /** @deprecated kept for older UI; prefer `markets` */
    both_side_positive_markets: bothPlusMarkets,
    markets: Object.keys(out.recommended),
    recommended_combined_pnl: Math.round(combinedAll * 100) / 100,
    recommended_combined_bets: combinedBets,
    recommended_combined_roi:
      combinedBets > 0 ? Math.round((combinedAll / (combinedBets * STAKE)) * 10000) / 10000 : null,
  };

  /** Live-week μ correction — always 0 (raw hierarchical μ; weather/wave already in export lines). */
  /** @type {Record<string, number>} */
  const liveBias = {};
  /** @type {object[]} */
  const betRows = [];

  for (const m of MARKETS) {
    const rec = out.recommended[m.market];
    if (!rec) continue;
    liveBias[m.market] = 0;

    const histRows = byMarket[m.market].map((r) => ({ ...r }));
    applyBias(histRows, "none", eventOrder);
    appendGradedBets(betRows, histRows, rec, false);

    const liveRows = (byMarketLive[m.market] || []).map((r) => ({ ...r }));
    applyBias(liveRows, "none", eventOrder);
    appendGradedBets(betRows, liveRows, rec, true);
  }

  const liveBetN = betRows.filter((b) => b.live_week).length;
  out.live_bias = liveBias;
  // Newest first + market mix so UI head-slices aren't Fairways-only (bets are appended per market).
  betRows.sort((a, b) => {
    const ta = Number(a.ts) || Date.parse(a.date) || 0;
    const tb = Number(b.ts) || Date.parse(b.date) || 0;
    if (ta !== tb) return tb - ta;
    // Live week first when timestamps collide / missing
    if (Boolean(a.live_week) !== Boolean(b.live_week)) return a.live_week ? -1 : 1;
    const rd = (Number(b.round) || 0) - (Number(a.round) || 0);
    if (rd !== 0) return rd;
    return (
      String(a.market || "").localeCompare(String(b.market || "")) ||
      String(a.player || "").localeCompare(String(b.player || ""))
    );
  });
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
        live_event: live || null,
        live_week_bets: liveBetN,
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
  console.log(`Graded bet rows: ${betRows.length} (live week: ${liveBetN})`);
  console.log(`\nWrote ${OUT}`);
  console.log(`Wrote ${betsPath}\n`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
