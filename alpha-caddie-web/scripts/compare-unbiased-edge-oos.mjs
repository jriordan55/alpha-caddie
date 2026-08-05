#!/usr/bin/env node
/**
 * Unbiased μ + both-side O/U edge report (event LOO).
 *   npm run compare:unbiased-edge-oos
 *   → data/unbiased_edge_oos.json
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
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
  fitAlpha,
  meanBias,
} from "./outcome-mu-debias.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
const OUT = join(WEB, "data", "unbiased_edge_oos.json");

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

function amPnl(res, am) {
  if (res !== "W" && res !== "L") return NaN;
  const o = Number(am);
  if (!Number.isFinite(o) || o === 0) return NaN;
  if (res === "W") return o > 0 ? o / 100 : 100 / Math.abs(o);
  return -1;
}

function summarize(rows, predKey, minGap) {
  let n = 0;
  let bias = 0;
  let mae = 0;
  const o = { n: 0, u: 0, h: 0 };
  const u = { n: 0, u: 0, h: 0 };
  for (const r of rows) {
    const p = r[predKey];
    if (!Number.isFinite(p) || !Number.isFinite(r.actual)) continue;
    n++;
    bias += p - r.actual;
    mae += Math.abs(p - r.actual);
    if (!Number.isFinite(r.book)) continue;
    const gap = p - r.book;
    if (Math.abs(gap) < minGap) continue;
    if (gap > 0) {
      const pnl = amPnl(r.oRes, r.oo);
      if (!Number.isFinite(pnl)) continue;
      o.n++;
      o.u += pnl;
      if (r.oRes === "W") o.h++;
    } else {
      const pnl = amPnl(r.uRes, r.uo);
      if (!Number.isFinite(pnl)) continue;
      u.n++;
      u.u += pnl;
      if (r.uRes === "W") u.h++;
    }
  }
  const side = (s) =>
    s.n
      ? {
          n: s.n,
          hit_pct: Math.round((1000 * s.h) / s.n) / 10,
          roi_pct: Math.round((1000 * s.u) / s.n) / 10,
        }
      : null;
  return {
    n,
    bias: n ? Math.round((bias / n) * 1000) / 1000 : null,
    mae: n ? Math.round((mae / n) * 1000) / 1000 : null,
    over: side(o),
    under: side(u),
    both_positive: Boolean(side(o)?.roi_pct > 0 && side(u)?.roi_pct > 0),
  };
}

async function main() {
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
          byM[market].push({
            event,
            model,
            book,
            actual,
            oo: parseLine(row[cols.overOdds]),
            uo: parseLine(row[cols.underOdds]),
            oRes: String(row[cols.overRes] || ""),
            uRes: String(row[cols.underRes] || ""),
          });
        }
      })
      .on("end", resolve)
      .on("error", reject);
  });

  /** @type {Record<string, any>} */
  const report = {};
  for (const market of DEBIAS_MARKETS) {
    const rows = byM[market];
    const events = [...new Set(rows.map((r) => r.event))];
    for (const ev of events) {
      const train = rows.filter((r) => r.event !== ev);
      const bias = meanBias(train);
      const alpha = fitAlpha(train.map((r) => ({ model: r.model - bias, book: r.book, actual: r.actual })));
      for (const r of rows.filter((x) => x.event === ev)) {
        r.raw = r.model;
        r.deb = r.model - bias;
        r.star = r.book + alpha * (r.deb - r.book);
      }
    }
    const rawS = summarize(rows, "raw", 0.35);
    const debS = summarize(rows, "deb", 0.35);
    const star035 = summarize(rows, "star", 0.35);
    const gaps = {};
    let bestBoth = null;
    for (const g of [0.35, 0.5, 0.75, 1, 1.25]) {
      const s = summarize(rows, "star", g);
      gaps[String(g)] = s;
      if (s.both_positive && s.over.n >= 8 && s.under.n >= 8 && !bestBoth) bestBoth = { gap: g, ...s };
    }
    report[market] = {
      n: rows.length,
      book_range: BOOK_LINE_RANGE[market],
      raw_wf: rawS,
      debiased: debS,
      mu_star: star035,
      mu_star_by_gap: gaps,
      recommended: bestBoth,
    };
    console.log(`\n${market}`);
    console.log(`  raw     bias ${rawS.bias} mae ${rawS.mae}  O ${JSON.stringify(rawS.over)}  U ${JSON.stringify(rawS.under)}`);
    console.log(`  debiased bias ${debS.bias} mae ${debS.mae}  O ${JSON.stringify(debS.over)}  U ${JSON.stringify(debS.under)}`);
    console.log(`  μ*      bias ${star035.bias} mae ${star035.mae}  O ${JSON.stringify(star035.over)}  U ${JSON.stringify(star035.under)}`);
    if (bestBoth) {
      console.log(
        `  → both-side @ gap≥${bestBoth.gap}: O ${bestBoth.over.roi_pct}% (n=${bestBoth.over.n})  U ${bestBoth.under.roi_pct}% (n=${bestBoth.under.n})`,
      );
    } else {
      console.log(`  → no both-side +ROI window with n≥8 (FW often fails here)`);
    }
  }

  writeFileSync(
    OUT,
    `${JSON.stringify(
      {
        generated_at: new Date().toISOString(),
        goal: "unbiased projections with edge on overs AND unders",
        formula: "μ_deb=μ−bias_LOO; μ*=book+α(μ_deb−book); bet when |μ*−book|≥gap, side=both",
        excluded_live_event: live || null,
        by_market: report,
      },
      null,
      2,
    )}\n`,
  );
  console.log(`\nWrote ${OUT}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
