#!/usr/bin/env node
/**
 * Backtest: Round Projections skill window — last N rounds per player (4/12/24/36/48)
 * vs current (80 rounds, recency-decay 0.86). Walk-forward OOS; does NOT touch live projections.
 * Writes data/skill_window_oos_roi.json for projection-tracker Overview.
 *
 *   node scripts/compare-skill-window-oos.mjs
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { walkforwardBacktestPipelineEnv } from "./projection-pipeline-env.mjs";
import { buildFullModelMuMapForEvent } from "./historical-walkforward-projections.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { MARKET_BOOK_CALIBRATION_MARKETS } from "./market-book-calibration.mjs";
import { EXPORT_MARKETS, modelProbOver, num } from "./round-projection-mu.mjs";
import { fitOutcomeSigmaScales, setOutcomeSigmaScales } from "./projection-stat-model.mjs";
import { capDirectionalPostedEdges, devigFairTwoWay, pickBetSide } from "../projection-tracker/ev-math.mjs";
import { DEFAULT_MIN_EV_PCT, minEvForMarket, qualifiesBet } from "./bet-policy.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const HIST = join(REPO, "data", "historical_rounds_all.csv");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
const OUT_JSON = join(WEB, "data", "skill_window_oos_roi.json");

const STRATEGIES = [
  { name: "current (last 80 rounds)", skill_max_rounds: 80, env: { GOLF_WF_SKILL_MAX_ROUNDS: "80" } },
  { name: "last 48 rounds", skill_max_rounds: 48, env: { GOLF_WF_SKILL_MAX_ROUNDS: "48" } },
  { name: "last 36 rounds", skill_max_rounds: 36, env: { GOLF_WF_SKILL_MAX_ROUNDS: "36" } },
  { name: "last 24 rounds", skill_max_rounds: 24, env: { GOLF_WF_SKILL_MAX_ROUNDS: "24" } },
  { name: "last 12 rounds", skill_max_rounds: 12, env: { GOLF_WF_SKILL_MAX_ROUNDS: "12" } },
  { name: "last 4 rounds", skill_max_rounds: 4, env: { GOLF_WF_SKILL_MAX_ROUNDS: "4" } },
];

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? num(s, NaN) : NaN;
}

function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
}

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

async function loadWalkForwardBetRowsWithDg() {
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const rows = [];
  await new Promise((resolvePromise, reject) => {
    Readable.from([aligned])
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.pricing_skill || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        const event = String(row.event_name || "").trim();
        if (!event) return;
        const dg = Math.round(num(row.dg_id, NaN));
        const rnd = Math.round(num(row.round, NaN));
        if (!Number.isFinite(dg) || !Number.isFinite(rnd)) return;
        const t = parseMs(row.projections_updated_at) || parseMs(row.exported_at);
        const context = {
          dg_id: dg,
          round: rnd,
          gir_minus_fw: num(row.gir_minus_fw, NaN),
          course_fw_width: num(row.course_fw_width, NaN),
        };
        for (const market of MARKET_BOOK_CALIBRATION_MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const modelLine = parseLine(row[cols.model]);
          const bookLine = parseLine(row[cols.book]);
          if (!Number.isFinite(modelLine) || !Number.isFinite(bookLine)) continue;
          rows.push({
            event,
            eventMs: t,
            market,
            dg_id: dg,
            modelLine,
            modelBookDelta: modelLine - bookLine,
            bookLine,
            overOdds: num(row[cols.overOdds], NaN),
            underOdds: num(row[cols.underOdds], NaN),
            overRes: String(row[cols.overRes] || "").trim().toUpperCase(),
            underRes: String(row[cols.underRes] || "").trim().toUpperCase(),
            context,
          });
        }
      })
      .on("end", resolvePromise)
      .on("error", reject);
  });
  return rows;
}

function eventOrderFromRows(rows) {
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
      return ta - tb || b[1].n - a[1].n;
    })
    .map(([ev]) => ev);
}

function implied(am) {
  const v = num(am, NaN);
  if (!Number.isFinite(v) || v === 0) return NaN;
  return v > 0 ? 100 / (v + 100) : -v / (-v + 100);
}

function roiOnRows(rows, minEvPct, { useRecommendedPolicy = false } = {}) {
  let units = 0;
  let n = 0;
  let wins = 0;
  let losses = 0;
  const byMarket = new Map();
  for (const b of rows) {
    const mu = b.modelLine;
    const pOver = modelProbOver(b.market, mu, b.bookLine, 1, 14);
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
    const pnl = res === "W" ? (odds > 0 ? odds / 100 : 100 / -odds) : res === "L" ? -1 : 0;
    units += pnl;
    n++;
    if (res === "W") wins++;
    else if (res === "L") losses++;
    const mk = byMarket.get(b.market) || { units: 0, bets: 0, wins: 0 };
    mk.units += pnl;
    mk.bets++;
    if (res === "W") mk.wins++;
    byMarket.set(b.market, mk);
  }
  return {
    units,
    bets: n,
    wins,
    losses,
    hit_pct: n > 0 ? (wins / n) * 100 : NaN,
    roi_pct: n > 0 ? (units / n) * 100 : NaN,
    byMarket,
  };
}

function loadLiveEvent() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

async function loadHistRows() {
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);
  const rows = [];
  await new Promise((resolvePromise, reject) => {
    createReadStream(HIST)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", resolvePromise)
      .on("error", reject);
  });
  return rows;
}

function buildEventYearMap(histRows) {
  const m = new Map();
  for (const row of histRows) {
    const ev = String(row.event_name || "").trim();
    const yr = Math.round(num(row.year, NaN));
    if (!ev || !Number.isFinite(yr)) continue;
    const k = foldComparableTitle(ev);
    const prev = m.get(k);
    if (!prev || yr > prev) m.set(k, yr);
  }
  return m;
}

function buildFieldDgIdsByBundle(histRows) {
  const fields = new Map();
  for (const row of histRows) {
    const ev = String(row.event_name || "").trim();
    const yr = Math.round(num(row.year, NaN));
    const rnd = Math.round(num(row.round_num, NaN));
    const dg = Math.round(num(row.dg_id, NaN));
    if (!ev || !Number.isFinite(yr) || !Number.isFinite(rnd) || !Number.isFinite(dg)) continue;
    const k = `${yr}|${foldComparableTitle(ev)}|${rnd}`;
    if (!fields.has(k)) fields.set(k, new Set());
    fields.get(k).add(dg);
  }
  return fields;
}

function bundleKeysFromBetRows(betRows, eventYearMap) {
  const bundles = new Map();
  for (const b of betRows) {
    const yr = eventYearMap.get(foldComparableTitle(b.event));
    if (!Number.isFinite(yr)) continue;
    const rnd = Math.round(num(b.context?.round, NaN));
    const dg = Math.round(num(b.dg_id, NaN));
    if (!Number.isFinite(rnd) || !Number.isFinite(dg)) continue;
    const key = `${yr}|${foldComparableTitle(b.event)}|${rnd}`;
    if (!bundles.has(key)) {
      bundles.set(key, { event: b.event, year: yr, round: rnd, betTimeMs: b.eventMs, dgIds: new Set() });
    }
    bundles.get(key).dgIds.add(dg);
  }
  return bundles;
}

async function rebuildModelLines(betRows, histRows, eventYearMap, fieldMap, strategyEnv) {
  Object.assign(process.env, walkforwardBacktestPipelineEnv(), strategyEnv);
  const bundles = bundleKeysFromBetRows(betRows, eventYearMap);
  const muByBundle = new Map();

  const keys = [...bundles.values()];
  let i = 0;
  for (const b of keys) {
    const bundleKey = `${b.year}|${foldComparableTitle(b.event)}|${b.round}`;
    const fieldDgIds = [...(fieldMap.get(bundleKey) || b.dgIds)];
    const map = await buildFullModelMuMapForEvent({
      repoRoot: REPO,
      histRows,
      eventName: b.event,
      eventYear: b.year,
      targetRound: b.round,
      betTimeMs: b.betTimeMs,
      fieldDgIds,
      pipelineEnv: strategyEnv,
    });
    muByBundle.set(bundleKey, map);
    i++;
    if (i % 5 === 0 || i === keys.length) process.stdout.write(`\r    projections ${i}/${keys.length}`);
  }
  process.stdout.write("\n");

  return betRows.map((b) => {
    const yr = eventYearMap.get(foldComparableTitle(b.event));
    const rnd = Math.round(num(b.context?.round, NaN));
    const dg = Math.round(num(b.dg_id, NaN));
    if (!Number.isFinite(yr) || !Number.isFinite(rnd) || !Number.isFinite(dg)) return b;
    const bundleKey = `${yr}|${foldComparableTitle(b.event)}|${rnd}`;
    const mu = muByBundle.get(bundleKey)?.get(dg)?.get(b.market);
    if (!Number.isFinite(mu)) return b;
    return { ...b, modelLine: mu, modelBookDelta: mu - b.bookLine };
  });
}

async function main() {
  const scales = await fitOutcomeSigmaScales(VS);
  setOutcomeSigmaScales(scales);

  const histRows = await loadHistRows();
  const eventYearMap = buildEventYearMap(histRows);
  const fieldMap = buildFieldDgIdsByBundle(histRows);
  const liveEvent = loadLiveEvent();
  let betRows = await loadWalkForwardBetRowsWithDg();

  const oosEvents = eventOrderFromRows(betRows).filter((ev) => !liveEvent || !eventsLikelySame(ev, liveEvent));
  betRows = betRows.filter((b) => oosEvents.includes(b.event));

  console.log(`OOS bet rows: ${betRows.length} across ${oosEvents.length} events (live event "${liveEvent}" excluded)\n`);
  console.log(`Events: ${oosEvents.join(" | ")}\n`);

  const results = [];
  for (const strat of STRATEGIES) {
    console.log(`Testing: ${strat.name}`);
    const graded = await rebuildModelLines(betRows, histRows, eventYearMap, fieldMap, strat.env);
    const rec = roiOnRows(graded, DEFAULT_MIN_EV_PCT, { useRecommendedPolicy: true });
    const unfiltered = roiOnRows(graded, 5);
    results.push({ name: strat.name, skill_max_rounds: strat.skill_max_rounds, rec, unfiltered });
    console.log(
      `  policy @${DEFAULT_MIN_EV_PCT}%: ${rec.roi_pct?.toFixed(1)}% ROI, ${rec.units >= 0 ? "+" : ""}${rec.units.toFixed(2)}u PnL, ${rec.bets} bets, ${rec.hit_pct?.toFixed(1)}% hit`,
    );
    console.log(
      `  unfiltered @5%:   ${unfiltered.roi_pct?.toFixed(1)}% ROI, ${unfiltered.units >= 0 ? "+" : ""}${unfiltered.units.toFixed(2)}u PnL, ${unfiltered.bets} bets\n`,
    );
  }

  console.log("=== Skill window comparison (recommended policy) ===\n");
  console.log(
    `${"window".padEnd(26)} ${"ROI%".padStart(7)} ${"PnL(u)".padStart(9)} ${"bets".padStart(6)} ${"hit%".padStart(6)}   ${"ROI%@5".padStart(7)} ${"PnL@5".padStart(9)} ${"bets@5".padStart(7)}`,
  );
  for (const r of results) {
    console.log(
      `${r.name.padEnd(26)} ${String(r.rec.roi_pct?.toFixed(1) ?? "—").padStart(7)} ${`${r.rec.units >= 0 ? "+" : ""}${r.rec.units.toFixed(2)}`.padStart(9)} ${String(r.rec.bets).padStart(6)} ${String(r.rec.hit_pct?.toFixed(1) ?? "—").padStart(6)}   ${String(r.unfiltered.roi_pct?.toFixed(1) ?? "—").padStart(7)} ${`${r.unfiltered.units >= 0 ? "+" : ""}${r.unfiltered.units.toFixed(2)}`.padStart(9)} ${String(r.unfiltered.bets).padStart(7)}`,
    );
  }

  console.log("\n=== Per-market PnL (recommended policy) ===\n");
  const markets = [...new Set(results.flatMap((r) => [...r.rec.byMarket.keys()]))];
  console.log(`${"window".padEnd(26)} ${markets.map((m) => m.padStart(14)).join(" ")}`);
  for (const r of results) {
    const cells = markets.map((m) => {
      const s = r.rec.byMarket.get(m);
      if (!s) return "—".padStart(14);
      return `${s.units >= 0 ? "+" : ""}${s.units.toFixed(1)}u/${s.bets}b`.padStart(14);
    });
    console.log(`${r.name.padEnd(26)} ${cells.join(" ")}`);
  }

  const baseline = results.find((r) => r.skill_max_rounds === 80) || results[0];
  const byMarketObj = (m) => {
    const out = {};
    for (const [k, v] of m.entries()) {
      out[k] = { units: Math.round(v.units * 10) / 10, bets: v.bets, wins: v.wins };
    }
    return out;
  };
  const windows = results.map((r) => {
    const isCurrent = r.skill_max_rounds === 80;
    const row = {
      name: r.name,
      skill_max_rounds: r.skill_max_rounds,
      is_current: isCurrent,
      recommended: {
        roi_pct: Math.round(r.rec.roi_pct * 10) / 10,
        units: Math.round(r.rec.units * 100) / 100,
        bets: r.rec.bets,
        hit_pct: Math.round(r.rec.hit_pct * 10) / 10,
        by_market: byMarketObj(r.rec.byMarket),
      },
      unfiltered_at_5pct: {
        roi_pct: Math.round(r.unfiltered.roi_pct * 10) / 10,
        units: Math.round(r.unfiltered.units * 100) / 100,
        bets: r.unfiltered.bets,
      },
    };
    if (!isCurrent && baseline) {
      row.delta_vs_current = {
        roi_pct: Math.round((r.rec.roi_pct - baseline.rec.roi_pct) * 10) / 10,
        units: Math.round((r.rec.units - baseline.rec.units) * 100) / 100,
      };
    }
    return row;
  });
  const best = [...windows].sort((a, b) => (b.recommended.units || 0) - (a.recommended.units || 0))[0];
  const payload = {
    generated_at: new Date().toISOString(),
    hypothetical: true,
    note: "Walk-forward OOS: rebuild model μ using only each player's last N skill rounds (recency decay unchanged). Does not change live projections — comparison only.",
    methodology: {
      baseline_skill_max_rounds: 80,
      windows_tested: STRATEGIES.map((s) => s.skill_max_rounds),
      oos_bet_rows: betRows.length,
      oos_event_count: oosEvents.length,
      excluded_live_event: liveEvent || null,
      events: oosEvents,
      policy: "recommended per-market EV/gap filters (DEFAULT_MIN_EV_PCT / bet-policy.mjs)",
      unfiltered: "flat 5% EV threshold, no gap/side policy",
    },
    windows,
    best_window: best
      ? {
          skill_max_rounds: best.skill_max_rounds,
          name: best.name,
          roi_pct: best.recommended.roi_pct,
          units: best.recommended.units,
          vs_current_units: best.delta_vs_current?.units ?? 0,
        }
      : null,
  };
  writeFileSync(OUT_JSON, `${JSON.stringify(payload, null, 2)}\n`, "utf8");
  console.log(`\nWrote ${OUT_JSON}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
