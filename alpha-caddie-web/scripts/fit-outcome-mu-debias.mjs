#!/usr/bin/env node
/**
 * Fit walk-forward outcome μ bias + book residual α (event LOO).
 *   npm run fit:outcome-mu-debias
 *   → data/outcome_mu_debias.json
 */
import { existsSync, readFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { EXPORT_MARKETS, num } from "./round-projection-mu.mjs";
import {
  BOOK_LINE_RANGE,
  bookLineValid,
  DEBIAS_MARKETS,
  fitMarketDebiasLoo,
  writeOutcomeMuDebias,
} from "./outcome-mu-debias.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");

const MARKET_COLS = Object.fromEntries(
  EXPORT_MARKETS.map((m) => [
    m.market,
    {
      model: m.lineCol,
      book: m.bookLineCol,
      actual: m.actualCol,
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

function liveEvent() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

async function loadRows() {
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const live = liveEvent();
  /** @type {Record<string, object[]>} */
  const byM = Object.fromEntries(DEBIAS_MARKETS.map((m) => [m, []]));
  await new Promise((resolve, reject) => {
    Readable.from([aligned])
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        const event = String(row.event_name || "").trim();
        if (!event || (live && eventsLikelySame(event, live))) return;
        for (const market of DEBIAS_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const model = parseLine(row[cols.model]);
          const book = parseLine(row[cols.book]);
          const actual = parseLine(row[cols.actual]);
          if (!Number.isFinite(model) || !Number.isFinite(actual)) continue;
          if (!bookLineValid(market, book)) continue;
          if ((market === "Birdies" || market === "Bogeys") && actual === 0) {
            const sc = parseLine(row.actual_round_score);
            if (Number.isFinite(sc) && sc > 0) continue;
          }
          byM[market].push({
            market,
            event,
            model,
            book,
            actual,
            overOdds: parseLine(row[cols.overOdds]),
            underOdds: parseLine(row[cols.underOdds]),
            overRes: String(row[cols.overRes] || ""),
            underRes: String(row[cols.underRes] || ""),
          });
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return { byM, live };
}

async function main() {
  const { byM, live } = await loadRows();
  /** @type {Record<string, object>} */
  const markets = {};
  for (const m of DEBIAS_MARKETS) {
    const fit = fitMarketDebiasLoo(byM[m]);
    markets[m] = {
      ...fit,
      book_range: BOOK_LINE_RANGE[m],
      side: "both",
      notes:
        fit.recommended_gap != null
          ? `Both O/U flat ROI > 0 at |μ*−book|≥${fit.recommended_gap} (event LOO)`
          : "No gap found with both sides +ROI at n≥8; prefer pass / softer books",
    };
    console.log(
      `${m}: bias=${fit.bias} α=${fit.alpha} gap*=${fit.recommended_gap} loo_mae=${fit.loo_mae} loo_bias=${fit.loo_bias} n=${fit.n}`,
    );
    for (const g of fit.both_side_gaps.slice(0, 3)) {
      console.log(
        `  gap≥${g.gap}  O ${g.over?.roi_pct}% (n=${g.over?.n})  U ${g.under?.roi_pct}% (n=${g.under?.n})`,
      );
    }
  }
  const path = writeOutcomeMuDebias({
    generated_at: new Date().toISOString(),
    goal: "unbiased μ with both-side O/U edge vs DK",
    method:
      "Event LOO: bias = mean(model−actual); α from regressing (actual−book) on (debiased−book); μ*=book+α(μ_deb−book)",
    excluded_live_event: live || null,
    markets,
  });
  console.log(`\nWrote ${path}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
