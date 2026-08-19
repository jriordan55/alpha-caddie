#!/usr/bin/env node
/**
 * Event leave-one-out fit: continuous prior-round signals → μ boost.
 *
 *   node scripts/fit-prior-round-mu-boost.mjs
 *   → data/prior_round_mu_boost.json
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { priorContextForBetRow, loadHistByKey } from "./prior-round-context.mjs";
import { priorSignalsFromRow } from "./sg-side-policy.mjs";
import {
  MARKET_BOOST_SPEC,
  centeredPriorSignal,
  fitContinuousMarket,
  marketBoostMarkets,
  RIDGE_LAMBDA,
  SHRINK_K,
  MIN_FIT_SAMPLES,
} from "./prior-round-mu-boost.mjs";
import { EXPORT_MARKETS, num } from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const HIST = join(WEB, "data", "historical_rounds_all.csv");
const PROJ = join(WEB, "projections.json");
const OUT = join(WEB, "data", "prior_round_mu_boost.json");

const MARKETS = marketBoostMarkets();

function liveEvent() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

function rowKey(r) {
  return `${r.event}|${r.round}|${r.dg_id}|${r.market}`;
}

function rowHasSignal(market, signals) {
  const spec = MARKET_BOOST_SPEC[market];
  if (!spec) return false;
  for (const f of spec.features) {
    if (!Number.isFinite(num(signals?.[f], NaN))) return false;
  }
  return true;
}

async function loadUniqueRows() {
  const histByKey = existsSync(HIST) ? await loadHistByKey(HIST) : new Map();
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const live = liveEvent();
  /** @type {Map<string, object>} */
  const uniq = new Map();

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
        if (!event || (live && eventsLikelySame(event, live))) return;
        const round = Math.round(num(row.round, NaN));
        if (!(round >= 2)) return;
        const dg_id = Math.round(num(row.dg_id, NaN));
        const prior = priorContextForBetRow(histByKey, row);
        const signals = priorSignalsFromRow({ ...row, ...prior });

        for (const m of EXPORT_MARKETS) {
          if (!MARKET_BOOST_SPEC[m.market]) continue;
          if (!rowHasSignal(m.market, signals)) continue;
          const model = num(row[m.lineCol], NaN);
          const actual = num(row[m.actualCol], NaN);
          if (!Number.isFinite(model) || !Number.isFinite(actual)) continue;
          if (model <= 0 && m.market !== "Total score") continue;

          const rec = {
            event,
            round,
            dg_id,
            market: m.market,
            model,
            actual,
            signals,
          };
          uniq.set(rowKey(rec), rec);
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });

  return [...uniq.values()];
}

function main() {
  return loadUniqueRows().then((rows) => {
    const events = [...new Set(rows.map((r) => r.event))].sort();
    /** @type {Record<string, object>} */
    const pooledMarkets = {};
    /** @type {Record<string, { markets: object }>} */
    const loo = {};

    for (const market of MARKETS) {
      pooledMarkets[market] = fitContinuousMarket(
        market,
        rows.filter((r) => r.market === market),
      );
    }

    for (const ev of events) {
      const train = rows.filter((r) => r.event !== ev);
      /** @type {Record<string, object>} */
      const markets = {};
      for (const market of MARKETS) {
        markets[market] = fitContinuousMarket(
          market,
          train.filter((r) => r.market === market),
        );
      }
      loo[ev] = { markets };
    }

    const out = {
      generated_at: new Date().toISOString(),
      source: "data/round_projection_vs_actual.csv",
      method: "event_leave_one_out_continuous_ridge",
      ridge_lambda: RIDGE_LAMBDA,
      shrink_k: SHRINK_K,
      min_fit_samples: MIN_FIT_SAMPLES,
      enabled: true,
      markets: pooledMarkets,
      loo,
      signal_definitions: Object.fromEntries(
        MARKETS.map((m) => [m, MARKET_BOOST_SPEC[m].label]),
      ),
      n_rows: rows.length,
      n_events: events.length,
    };

    writeFileSync(OUT, JSON.stringify(out, null, 2));
    console.log(`\nPrior-round μ boost (continuous, event LOO, ${rows.length} player-rounds, ${events.length} events)\n`);
    for (const market of MARKETS) {
      const mf = pooledMarkets[market];
      if (!mf) continue;
      const unit = mf.relative ? "rel" : "add";
      const meanBits = (mf.features || [])
        .map((f) => `${f}=${Number(mf.means?.[f] ?? 0).toFixed(3)}`)
        .join(", ");
      console.log(
        `${market.padEnd(14)} β=${(mf.beta ?? 0).toFixed(4)} (${unit}, n=${mf.n}) · ${mf.label} · means: ${meanBits}`,
      );
    }
    console.log(`\nWrote ${OUT}`);
  });
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
