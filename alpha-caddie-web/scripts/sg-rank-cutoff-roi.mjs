#!/usr/bin/env node
/**
 * Sweep prior-round SG field rank cutoffs vs graded projection bets (gap policy from both_side_roi).
 *
 *   node scripts/sg-rank-cutoff-roi.mjs
 *   → data/sg_rank_cutoff_roi.json
 *
 * For each market side rule, tests taking only players who ranked top N (or bottom N / worse)
 * in the relevant SG category among everyone who completed the previous tournament round.
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { alignDetailCsvContent, bobPctFromHistRow, girPctFromHistRow, num } from "./projection-context-signals.mjs";
import { actualsKey, loadHistByKey, yearFromEventCompleted } from "./prior-round-context.mjs";
import {
  EXPORT_MARKETS,
  num,
  parseDkBookLine,
  parsePpBookLine,
  ouSideResults,
} from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const HIST = join(WEB, "data", "historical_rounds_all.csv");
const ROI_JSON = join(WEB, "data", "both_side_roi.json");
const OUT = join(WEB, "data", "sg_rank_cutoff_roi.json");
const STAKE = 100;

const BOOKS = [
  { id: "draftkings", lineKey: "bookLineCol", overKey: "overOddsCol", underKey: "underOddsCol", wholeLine: false },
  { id: "prizepicks", lineKey: "ppLineCol", overKey: "ppOverOddsCol", underKey: "ppUnderOddsCol", wholeLine: true },
  { id: "sleeper", lineKey: "slLineCol", overKey: "slOverOddsCol", underKey: "slUnderOddsCol", wholeLine: true },
  { id: "underdog", lineKey: "udLineCol", overKey: "udOverOddsCol", underKey: "udUnderOddsCol", wholeLine: true },
  { id: "caesars", lineKey: "czrLineCol", overKey: "czrOverOddsCol", underKey: "czrUnderOddsCol", wholeLine: false },
  { id: "kalshi", lineKey: "klLineCol", overKey: "klOverOddsCol", underKey: "klUnderOddsCol", wholeLine: false },
];

const MARKETS = EXPORT_MARKETS.map((m) => ({
  market: m.market,
  modelCol: m.lineCol,
  actualCol: m.actualCol,
  spec: m,
}));

const CUTOFF_LABELS = [
  { id: "top5", n: 5, mode: "symmetric" },
  { id: "top10", n: 10, mode: "symmetric" },
  { id: "top15", n: 15, mode: "symmetric" },
  { id: "top25", n: 25, mode: "symmetric" },
  { id: "top30", n: 30, mode: "symmetric" },
  { id: "worse", n: 30, mode: "worse" },
];

function parseBookLine(raw, wholeLine) {
  return wholeLine ? parsePpBookLine(raw) : parseDkBookLine(raw);
}

function americanPnl(result, americanOdds) {
  if (result === "P" || result == null || result === "") return 0;
  if (result !== "W" && result !== "L") return NaN;
  const o = Number(americanOdds);
  if (!Number.isFinite(o) || o === 0) return NaN;
  if (result === "L") return -STAKE;
  return o > 0 ? STAKE * (o / 100) : STAKE * (100 / Math.abs(o));
}

function gradeSide(actual, bookLine, side) {
  if (!Number.isFinite(actual) || !Number.isFinite(bookLine)) return null;
  const { over, under } = ouSideResults("x", actual, bookLine);
  if (side === "OVER") return over === "W" ? "W" : over === "L" ? "L" : "P";
  return under === "W" ? "W" : under === "L" ? "L" : "P";
}

function yearFromEventCompleted(s) {
  const m = String(s || "").match(/(\d{4})/);
  return m ? Number(m[1]) : NaN;
}

function fieldKey(event, year, roundNum) {
  return `${String(event).trim()}\x1f${year}\x1f${Math.round(roundNum)}`;
}

/** @returns {Map<string, Map<number, { app: number, ott: number, putt: number, field: number }>>} */
async function buildPriorRoundRankIndex(histPath) {
  /** @type {Map<string, { dg: number, sg_app: number, sg_ott: number, sg_putt: number, gir_pct: number, bob_pct: number }[]>} */
  const buckets = new Map();

  await new Promise((resolve, reject) => {
    createReadStream(histPath)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        const ev = String(row.event_name || "").trim();
        const rnd = Math.round(num(row.round_num, NaN));
        const dg = Math.round(num(row.dg_id, NaN));
        const score = num(row.round_score, NaN);
        if (!ev || !Number.isFinite(rnd) || !Number.isFinite(dg) || !Number.isFinite(score)) return;
        const yr = Math.round(num(row.year, NaN)) || yearFromEventCompleted(row.event_completed);
        if (!Number.isFinite(yr)) return;
        const k = fieldKey(ev, yr, rnd);
        let arr = buckets.get(k);
        if (!arr) {
          arr = [];
          buckets.set(k, arr);
        }
        arr.push({
          dg,
          sg_app: num(row.sg_app, NaN),
          sg_ott: num(row.sg_ott, NaN),
          sg_putt: num(row.sg_putt, NaN),
          gir_pct: girPctFromHistRow(row),
          bob_pct: bobPctFromHistRow(row),
        });
      })
      .on("end", resolve)
      .on("error", reject);
  });

  /** @type {Map<string, Map<number, object>>} */
  const index = new Map();

  for (const [k, players] of buckets) {
    const field = players.length;
    const rankOneMetric = (key) => {
      const vals = players
        .filter((p) => Number.isFinite(p[key]))
        .sort((a, b) => b[key] - a[key]);
      const rankByDg = new Map();
      vals.forEach((p, i) => rankByDg.set(p.dg, i + 1));
      return rankByDg;
    };
    const appR = rankOneMetric("sg_app");
    const ottR = rankOneMetric("sg_ott");
    const puttR = rankOneMetric("sg_putt");
    const girR = rankOneMetric("gir_pct");
    const bobR = rankOneMetric("bob_pct");
    const byDg = new Map();
    for (const p of players) {
      byDg.set(p.dg, {
        app: appR.get(p.dg) ?? NaN,
        ott: ottR.get(p.dg) ?? NaN,
        putt: puttR.get(p.dg) ?? NaN,
        gir: girR.get(p.dg) ?? NaN,
        bob: bobR.get(p.dg) ?? NaN,
        field,
        sg_app: p.sg_app,
        sg_ott: p.sg_ott,
        sg_putt: p.sg_putt,
        gir_pct: p.gir_pct,
        bob_pct: p.bob_pct,
      });
    }
    index.set(k, byDg);
  }
  return index;
}

function priorRanks(rankIndex, event, year, dgId, bettingRound) {
  const prevRnd = Math.round(bettingRound) - 1;
  if (prevRnd < 1) return null;
  const byDg = rankIndex.get(fieldKey(event, year, prevRnd));
  if (!byDg) return null;
  return byDg.get(Math.round(dgId)) || null;
}

/**
 * Side needs high SG in metric → top bucket; low SG → bottom bucket / worse.
 * @returns {{ over: boolean, under: boolean }}
 */
function rankAllowsSide(market, side, ranks, cutoff) {
  if (!ranks || !Number.isFinite(ranks.field) || ranks.field < 5) {
    return { over: false, under: false };
  }
  const { id, n, mode } = cutoff;
  const field = ranks.field;

  const topOk = (rank) => Number.isFinite(rank) && (mode === "worse" ? rank <= 30 : rank <= n);
  const bottomOk = (rank) => {
    if (!Number.isFinite(rank)) return false;
    if (mode === "worse") return rank > 30;
    const bottomStart = Math.max(1, field - n + 1);
    return rank >= bottomStart;
  };

  const appTop = topOk(ranks.app);
  const appBot = bottomOk(ranks.app);
  const ottTop = topOk(ranks.ott);
  const ottBot = bottomOk(ranks.ott);
  const puttTop = topOk(ranks.putt);
  const puttBot = bottomOk(ranks.putt);
  const girTop = topOk(ranks.gir);
  const girBot = bottomOk(ranks.gir);
  const bobTop = topOk(ranks.bob);
  const bobBot = bottomOk(ranks.bob);

  const s = String(side || "").toUpperCase();
  let over = false;
  let under = false;

  switch (market) {
    case "Total score":
      over = appBot;
      under = appTop;
      break;
    case "Birdies":
      over = bobTop;
      under = bobBot;
      break;
    case "Bogeys":
      over = appBot;
      under = appTop;
      break;
    case "Fairways hit":
      over = ottTop;
      under = ottBot;
      break;
    case "GIR":
      over = girTop;
      under = girBot;
      break;
    case "Pars":
      over = appTop && puttBot;
      under = appBot || puttTop;
      break;
    default:
      over = true;
      under = true;
  }

  if (s === "OVER") return { over, under: false };
  if (s === "UNDER") return { over: false, under };
  return { over, under };
}

function countingActualTrusted(row, market, actual) {
  if (!Number.isFinite(actual)) return false;
  if (!["Pars", "Birdies", "Bogeys"].includes(market)) return true;
  const score = num(row.actual_round_score, NaN);
  const b = num(row.actual_birdies, NaN);
  const p = num(row.actual_pars, NaN);
  const bg = num(row.actual_bogeys, NaN);
  if (Number.isFinite(score) && score > 0) {
    if ((!Number.isFinite(b) || b === 0) && (!Number.isFinite(p) || p === 0) && (!Number.isFinite(bg) || bg === 0)) {
      return false;
    }
  }
  if (market === "Pars" && actual === 0 && Number.isFinite(score) && score > 0) {
    const sum = (Number.isFinite(b) ? b : 0) + (Number.isFinite(bg) ? bg : 0);
    if (sum < 12) return false;
  }
  return true;
}

function loadRecommendedPolicy() {
  if (!existsSync(ROI_JSON)) return {};
  const j = JSON.parse(readFileSync(ROI_JSON, "utf8"));
  return j.recommended || {};
}

async function loadBetCandidates(rankIndex, recommended, liveEvent) {
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  /** @type {object[]} */
  const out = [];

  await new Promise((resolve, reject) => {
    Readable.from([aligned])
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.pricing_skill || "") !== "default") return;
        const event = String(row.event_name || "").trim();
        if (!event) return;
        if (liveEvent && eventsLikelySame(event, liveEvent)) return;

        const round = Math.round(num(row.round, NaN));
        const dg_id = Math.round(num(row.dg_id, NaN));
        const yr =
          Math.round(num(row.year, NaN)) ||
          yearFromEventCompleted(row.event_completed) ||
          yearFromEventCompleted(row.projections_updated_at);
        const ranks = priorRanks(rankIndex, event, yr, dg_id, round);

        for (const m of MARKETS) {
          const rec = recommended[m.market];
          if (!rec) continue;
          const gapOver = Number(rec.gap_over ?? rec.gap);
          const gapUnder = Number(rec.gap_under ?? rec.gap);
          const underMin = rec.odds_rule?.under_min_american;
          const overMin = rec.odds_rule?.over_min_american;
          const model = num(row[m.modelCol], NaN);
          const actual = num(row[m.actualCol], NaN);
          if (!Number.isFinite(model) || !Number.isFinite(actual)) continue;
          if (!countingActualTrusted(row, m.market, actual)) continue;

          for (const bk of BOOKS) {
            const lineCol = m.spec[bk.lineKey];
            const overCol = m.spec[bk.overKey];
            const underCol = m.spec[bk.underKey];
            if (!lineCol || !overCol || !underCol) continue;
            const bookRaw = String(row[lineCol] ?? "").trim();
            if (!bookRaw) continue;
            const book = parseBookLine(bookRaw, bk.wholeLine);
            const overOdds = num(row[overCol], NaN);
            const underOdds = num(row[underCol], NaN);
            if (!Number.isFinite(book) || !Number.isFinite(overOdds) || !Number.isFinite(underOdds)) continue;
            if (overOdds === 0 || underOdds === 0) continue;

            const delta = model - book;
            let side = null;
            if (delta > gapOver) side = "OVER";
            else if (delta < -gapUnder) side = "UNDER";
            if (!side) continue;
            const odds = side === "OVER" ? overOdds : underOdds;
            if (side === "UNDER" && Number.isFinite(underMin) && !(odds >= underMin)) continue;
            if (side === "OVER" && Number.isFinite(overMin) && !(odds >= overMin)) continue;
            const result = gradeSide(actual, book, side);
            if (!result) continue;
            const pnl = americanPnl(result, odds);
            if (!Number.isFinite(pnl) && result !== "P") continue;

            out.push({
              event,
              round,
              dg_id,
              player: String(row.player_name || "").trim(),
              market: m.market,
              book_id: bk.id,
              side,
              model,
              book,
              gap: delta,
              odds,
              actual,
              result,
              pnl: result === "P" ? 0 : pnl,
              ranks,
            });
          }
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });

  return out;
}

function dedupeCandidates(candidates, bookPreference = "draftkings") {
  const byKey = new Map();
  for (const c of candidates) {
    const k = `${c.event}\x1f${c.round}\x1f${c.dg_id}\x1f${c.market}\x1f${c.side}`;
    const prev = byKey.get(k);
    if (!prev) {
      byKey.set(k, c);
      continue;
    }
    if (c.book_id === bookPreference && prev.book_id !== bookPreference) {
      byKey.set(k, c);
    }
  }
  return [...byKey.values()];
}

function summarizeBets(bets) {
  let n = 0;
  let wins = 0;
  let losses = 0;
  let pushes = 0;
  let units = 0;
  for (const b of bets) {
    n++;
    units += b.pnl;
    if (b.result === "W") wins++;
    else if (b.result === "L") losses++;
    else pushes++;
  }
  const roi = n ? units / n : NaN;
  return {
    bets: n,
    wins,
    losses,
    pushes,
    units: Math.round(units * 100) / 100,
    roi: Math.round(roi * 1000) / 10,
    roi_pct: Math.round(roi * 10000) / 100,
  };
}

function evaluateCutoff(candidates, cutoff) {
  /** @type {Record<string, object[]>} */
  const byMarket = {};
  /** @type {object[]} */
  const all = [];
  for (const c of candidates) {
    const allow = rankAllowsSide(c.market, c.side, c.ranks, cutoff);
    const ok = c.side === "OVER" ? allow.over : allow.under;
    if (!ok) continue;
    all.push(c);
    if (!byMarket[c.market]) byMarket[c.market] = [];
    byMarket[c.market].push(c);
  }
  const markets = {};
  for (const m of MARKETS.map((x) => x.market)) {
    markets[m] = summarizeBets(byMarket[m] || []);
  }
  return { cutoff: cutoff.id, ...summarizeBets(all), markets };
}

function loadLiveEvent() {
  const p = join(WEB, "projections.json");
  if (!existsSync(p)) return "";
  try {
    return String(JSON.parse(readFileSync(p, "utf8")).event_name || "").trim();
  } catch {
    return "";
  }
}

async function main() {
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);
  if (!existsSync(VS)) throw new Error(`Missing ${VS}`);

  console.log("[sg-rank-cutoff] Building prior-round field ranks from historical_rounds_all.csv…");
  const rankIndex = await buildPriorRoundRankIndex(HIST);
  const recommended = loadRecommendedPolicy();
  const liveEvent = loadLiveEvent();
  const candidatesRaw = await loadBetCandidates(rankIndex, recommended, liveEvent);
  const candidates = dedupeCandidates(candidatesRaw);

  const withRanks = candidates.filter((c) => c.ranks && Number.isFinite(c.ranks.app));
  console.log(
    `[sg-rank-cutoff] ${candidatesRaw.length} gap-qualified rows → ${candidates.length} unique player-round-market-side (DK preferred); ${withRanks.length} with prior-round APP rank`,
  );

  const baseline = summarizeBets(candidates);
  const baselineNoSg = summarizeBets(withRanks);

  const results = CUTOFF_LABELS.map((c) => evaluateCutoff(candidates, c));
  results.sort((a, b) => b.roi - a.roi || b.bets - a.bets);

  const best = results[0];
  const out = {
    generated_at: new Date().toISOString(),
    source: {
      bets: "round_projection_vs_actual.csv + both_side_roi.json gap policy",
      ranks: "historical_rounds_all.csv prior in-event round",
    },
    excluded_live_event: liveEvent || null,
    dedupe: "one bet per event|round|player|market|side (DraftKings preferred)",
    baseline_all_gap_bets: baseline,
    baseline_with_prior_rank: baselineNoSg,
    cutoffs_tested: CUTOFF_LABELS.map((c) => c.id),
    best_combined: best,
    by_cutoff: results,
  };

  writeFileSync(OUT, `${JSON.stringify(out, null, 2)}\n`);

  console.log("\nCombined ROI by prior-round SG rank bucket (symmetric top/bottom N, worse = outside top 30):\n");
  console.log(
    `${"Cutoff".padEnd(8)} ${"Bets".padStart(6)} ${"W-L-P".padStart(12)} ${"Units".padStart(8)} ${"ROI".padStart(8)}`,
  );
  console.log("-".repeat(46));
  for (const r of results.sort((a, b) => a.cutoff.localeCompare(b.cutoff))) {
    console.log(
      `${r.cutoff.padEnd(8)} ${String(r.bets).padStart(6)} ${`${r.wins}-${r.losses}-${r.pushes}`.padStart(12)} ${((r.units >= 0 ? "+" : "") + r.units.toFixed(1) + "u").padStart(9)} ${((r.roi >= 0 ? "+" : "") + r.roi.toFixed(1) + "%").padStart(8)}`,
    );
  }
  console.log(`\nBest combined: ${best.cutoff} — ${best.roi >= 0 ? "+" : ""}${best.roi.toFixed(1)}% on ${best.bets} bets (${best.units >= 0 ? "+" : ""}${best.units.toFixed(1)}u)`);

  console.log("\nPer-market ROI at best cutoff (" + best.cutoff + "):\n");
  const mktRows = Object.entries(best.markets || {})
    .filter(([, s]) => s.bets >= 5)
    .sort((a, b) => b[1].roi - a[1].roi);
  for (const [m, s] of mktRows) {
    console.log(
      `  ${m.padEnd(14)} ${String(s.bets).padStart(4)} bets  ${s.roi >= 0 ? "+" : ""}${s.roi.toFixed(1)}%  (${s.units >= 0 ? "+" : ""}${s.units.toFixed(1)}u)`,
    );
  }

  console.log(`\nWrote ${OUT}\n`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
