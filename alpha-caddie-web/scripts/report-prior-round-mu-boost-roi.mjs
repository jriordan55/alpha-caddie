#!/usr/bin/env node
/**
 * OOS ROI: baseline μ vs prior-round boosted μ (event-LOO coefficients).
 * Uses Prop Pricing edge + SG side filters at min EV 0% (filters only).
 *
 *   node scripts/fit-prior-round-mu-boost.mjs
 *   node scripts/report-prior-round-mu-boost-roi.mjs
 *   → data/prior_round_mu_boost_roi.json
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { priorContextForBetRow, loadHistByKey } from "./prior-round-context.mjs";
import { pickPropPricingSide } from "./prop-pricing-bet-pick.mjs";
import { applyPriorRoundMuBoost } from "./prior-round-mu-boost.mjs";
import { priorSignalsFromRow } from "./sg-side-policy.mjs";
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
const PROJ = join(WEB, "projections.json");
const FIT = join(WEB, "data", "prior_round_mu_boost.json");
const OUT = join(WEB, "data", "prior_round_mu_boost_roi.json");

const STAKE = 100;
const MIN_EV = 0;
const BOOKS = [
  { id: "draftkings", label: "DraftKings", lineKey: "bookLineCol", overKey: "overOddsCol", underKey: "underOddsCol", wholeLine: false },
  { id: "prizepicks", label: "PrizePicks", lineKey: "ppLineCol", overKey: "ppOverOddsCol", underKey: "ppUnderOddsCol", wholeLine: true },
  { id: "sleeper", label: "Sleeper", lineKey: "slLineCol", overKey: "slOverOddsCol", underKey: "slUnderOddsCol", wholeLine: true },
  { id: "underdog", label: "Underdog", lineKey: "udLineCol", overKey: "udOverOddsCol", underKey: "udUnderOddsCol", wholeLine: true },
  { id: "fanduel", label: "FanDuel", lineKey: "fdLineCol", overKey: "fdOverOddsCol", underKey: "fdUnderOddsCol", wholeLine: false },
  { id: "caesars", label: "Caesars", lineKey: "czrLineCol", overKey: "czrOverOddsCol", underKey: "czrUnderOddsCol", wholeLine: false },
  { id: "kalshi", label: "Kalshi", lineKey: "klLineCol", overKey: "klOverOddsCol", underKey: "klUnderOddsCol", wholeLine: false },
];

const MARKETS = EXPORT_MARKETS.map((m) => ({ market: m.market, modelCol: m.lineCol, actualCol: m.actualCol, spec: m }));

function parseBookLine(raw, wholeLine) {
  return wholeLine ? parsePpBookLine(raw) : parseDkBookLine(raw);
}

function liveEvent() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

function americanPnl(result, odds) {
  if (result === "P") return 0;
  if (result !== "W" && result !== "L") return NaN;
  const o = Number(odds);
  if (!Number.isFinite(o) || o === 0) return NaN;
  if (result === "L") return -STAKE;
  return o > 0 ? STAKE * (o / 100) : STAKE * (100 / Math.abs(o));
}

function gradeSide(actual, bookLine, side) {
  const { over, under } = ouSideResults("x", actual, bookLine);
  if (side === "OVER") return over === "W" ? "W" : over === "L" ? "L" : "P";
  return under === "W" ? "W" : under === "L" ? "L" : "P";
}

function emptyStats() {
  return { bets: 0, wins: 0, losses: 0, pushes: 0, pnl: 0, mae: 0, mae_n: 0 };
}

function sideStats(s) {
  const roi = s.bets > 0 ? s.pnl / (s.bets * STAKE) : null;
  return {
    ...s,
    roi,
    roi_pct: roi != null ? Math.round(roi * 10000) / 100 : null,
    mae: s.mae_n > 0 ? s.mae / s.mae_n : null,
  };
}

function addMae(bucket, actual, mu) {
  if (!Number.isFinite(actual) || !Number.isFinite(mu)) return;
  bucket.mae += Math.abs(actual - mu);
  bucket.mae_n++;
}

function pickAndGrade(r, mu, actual) {
  const row = { ...r, model: mu, adjModel: mu };
  const pick = pickPropPricingSide(row, MIN_EV);
  if (!pick) return null;
  const side = pick.side;
  const odds = side === "OVER" ? r.overOdds : r.underOdds;
  const result = gradeSide(actual, r.book, side);
  if (!result) return null;
  const pnl = americanPnl(result, odds);
  if (!Number.isFinite(pnl) && result !== "P") return null;
  return { side, result, pnl: Number.isFinite(pnl) ? pnl : 0, edge: pick.edge };
}

async function loadRows() {
  if (!existsSync(VS)) throw new Error(`Missing ${VS}`);
  if (!existsSync(FIT)) throw new Error(`Missing ${FIT} — run fit-prior-round-mu-boost.mjs first`);
  const fit = JSON.parse(readFileSync(FIT, "utf8"));
  const histByKey = existsSync(HIST) ? await loadHistByKey(HIST) : new Map();
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const live = liveEvent();
  /** @type {object[]} */
  const rows = [];

  await new Promise((resolve, reject) => {
    Readable.from([aligned])
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.pricing_skill || "") !== "default") return;
        const event = String(row.event_name || "").trim();
        if (!event || (live && eventsLikelySame(event, live))) return;
        const round = Math.round(num(row.round, NaN));
        if (!(round >= 2)) return;

        for (const m of MARKETS) {
          const model = num(row[m.modelCol], NaN);
          const actual = num(row[m.actualCol], NaN);
          if (!Number.isFinite(model) || !Number.isFinite(actual)) continue;
          const prior = priorContextForBetRow(histByKey, row);
          const signals = priorSignalsFromRow({ ...row, ...prior });
          const boosted = applyPriorRoundMuBoost(m.market, model, signals, fit, event);

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

            rows.push({
              event,
              round,
              market: m.market,
              model,
              boosted,
              actual,
              book,
              overOdds,
              underOdds,
              book_id: bk.id,
              ...prior,
            });
          }
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });

  return { rows, fit };
}

function evaluate(rows) {
  /** @type {Record<string, { baseline: object, boosted: object }>} */
  const byMarket = {};
  const overall = { baseline: emptyStats(), boosted: emptyStats() };

  for (const r of rows) {
    if (!byMarket[r.market]) {
      byMarket[r.market] = { baseline: emptyStats(), boosted: emptyStats() };
    }

    addMae(byMarket[r.market].baseline, r.actual, r.model);
    addMae(byMarket[r.market].boosted, r.actual, r.boosted);
    addMae(overall.baseline, r.actual, r.model);
    addMae(overall.boosted, r.actual, r.boosted);

    const gBase = pickAndGrade(r, r.model, r.actual);
    if (gBase) {
      const b = byMarket[r.market].baseline;
      b.bets++;
      if (gBase.result === "W") b.wins++;
      else if (gBase.result === "L") b.losses++;
      else b.pushes++;
      b.pnl += gBase.pnl;
      overall.baseline.bets++;
      if (gBase.result === "W") overall.baseline.wins++;
      else if (gBase.result === "L") overall.baseline.losses++;
      else overall.baseline.pushes++;
      overall.baseline.pnl += gBase.pnl;
    }

    const gBoost = pickAndGrade(r, r.boosted, r.actual);
    if (gBoost) {
      const t = byMarket[r.market].boosted;
      t.bets++;
      if (gBoost.result === "W") t.wins++;
      else if (gBoost.result === "L") t.losses++;
      else t.pushes++;
      t.pnl += gBoost.pnl;
      overall.boosted.bets++;
      if (gBoost.result === "W") overall.boosted.wins++;
      else if (gBoost.result === "L") overall.boosted.losses++;
      else overall.boosted.pushes++;
      overall.boosted.pnl += gBoost.pnl;
    }
  }

  const markets = {};
  for (const [m, v] of Object.entries(byMarket)) {
    markets[m] = { baseline: sideStats(v.baseline), boosted: sideStats(v.boosted) };
  }
  return {
    markets,
    overall: { baseline: sideStats(overall.baseline), boosted: sideStats(overall.boosted) },
  };
}

async function main() {
  const { rows, fit } = await loadRows();
  const result = evaluate(rows);

  const out = {
    generated_at: new Date().toISOString(),
    source: "round_projection_vs_actual.csv + prior_round_mu_boost.json",
    method: "event_loo_boosted_mu",
    min_ev_pct: MIN_EV,
    sg_side_filter: true,
    stake_dollars: STAKE,
    n_graded_rows: rows.length,
    fit_generated_at: fit.generated_at,
    coefficients: fit.markets,
    ...result,
  };

  writeFileSync(OUT, JSON.stringify(out, null, 2));

  const b = result.overall.baseline;
  const t = result.overall.boosted;
  console.log("\nPrior-round μ boost ROI (event-LOO, min EV 0%, SG filters)\n");
  console.log(
    `${"Market".padEnd(14)} ${"Base ROI".padStart(9)} ${"Boost ROI".padStart(10)} ${"Base MAE".padStart(9)} ${"Boost MAE".padStart(10)} ${"Base n".padStart(7)} ${"Boost n".padStart(8)}`,
  );
  console.log("-".repeat(72));
  for (const m of Object.keys(result.markets).sort()) {
    const x = result.markets[m];
    const fmt = (v) => (Number.isFinite(v) ? `${(v * 100).toFixed(1)}%` : "—");
    const fmtM = (v) => (Number.isFinite(v) ? v.toFixed(3) : "—");
    console.log(
      `${m.padEnd(14)} ${fmt(x.baseline.roi).padStart(9)} ${fmt(x.boosted.roi).padStart(10)} ${fmtM(x.baseline.mae).padStart(9)} ${fmtM(x.boosted.mae).padStart(10)} ${String(x.baseline.bets).padStart(7)} ${String(x.boosted.bets).padStart(8)}`,
    );
  }
  console.log("-".repeat(72));
  console.log(
    `${"OVERALL".padEnd(14)} ${((b.roi ?? 0) * 100).toFixed(1).padStart(8)}% ${((t.roi ?? 0) * 100).toFixed(1).padStart(9)}% ${(b.mae ?? 0).toFixed(3).padStart(9)} ${(t.mae ?? 0).toFixed(3).padStart(10)} ${String(b.bets).padStart(7)} ${String(t.bets).padStart(8)}`,
  );
  console.log(`\nWrote ${OUT}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
