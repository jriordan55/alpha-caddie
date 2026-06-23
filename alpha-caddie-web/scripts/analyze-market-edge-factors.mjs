#!/usr/bin/env node
/**
 * Market edge factor analysis — which signals predict profitable O/U bets.
 * Uses round_projection_vs_actual.csv + dk_round_projection_audit.csv (+ optional history for weather).
 *
 *   node scripts/analyze-market-edge-factors.mjs
 *   node scripts/analyze-market-edge-factors.mjs --market "Fairways hit"
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import {
  americanToDecimal,
  devigFairTwoWay,
  impliedProbFromAmerican,
  modelEdgePctAtLine,
  modelProbOver,
  num,
  pickBetSide,
  pnlForResult,
} from "../projection-tracker/ev-math.mjs";
import { eventsLikelySame } from "./dg-events-align.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS_ACTUAL = join(WEB, "data", "round_projection_vs_actual.csv");
const AUDIT = join(WEB, "data", "dk_round_projection_audit.csv");
const HIST_JSON = join(WEB, "player_round_history.json");
const HIST_CSV = [join(WEB, "data", "historical_rounds_all.csv"), join(WEB, "..", "data", "historical_rounds_all.csv")].find(
  (p) => existsSync(p),
);
const OUT = join(WEB, "data", "market_edge_factor_analysis.json");

const MARKETS = [
  { market: "Total score", modelCol: "round_score_line", bookCol: "round_score_book_line", overOdds: "round_score_over_odds", underOdds: "round_score_under_odds", overRes: "round_score_over", underRes: "round_score_under", actual: "actual_round_score", auditModel: "model_total_score", lowerBetter: true },
  { market: "Birdies", modelCol: "birdies_line", bookCol: "birdies_book_line", overOdds: "birdies_over_odds", underOdds: "birdies_under_odds", overRes: "birdies_over", underRes: "birdies_under", actual: "actual_birdies", auditModel: "model_birdies", lowerBetter: false },
  { market: "GIR", modelCol: "gir_line", bookCol: "gir_book_line", overOdds: "gir_over_odds", underOdds: "gir_under_odds", overRes: "gir_over", underRes: "gir_under", actual: "actual_gir", auditModel: "model_gir", lowerBetter: false },
  { market: "Fairways hit", modelCol: "fairways_line", bookCol: "fairways_book_line", overOdds: "fairways_over_odds", underOdds: "fairways_under_odds", overRes: "fairways_over", underRes: "fairways_under", actual: "actual_fairways", auditModel: "model_fairways", lowerBetter: false },
  { market: "Pars", modelCol: "pars_line", bookCol: "pars_book_line", overOdds: "pars_over_odds", underOdds: "pars_under_odds", overRes: "pars_over", underRes: "pars_under", actual: "actual_pars", auditModel: "model_pars", lowerBetter: false },
  { market: "Bogeys", modelCol: "bogeys_line", bookCol: "bogeys_book_line", overOdds: "bogeys_over_odds", underOdds: "bogeys_under_odds", overRes: "bogeys_over", underRes: "bogeys_under", actual: "actual_bogeys", auditModel: "model_bogeys", lowerBetter: true },
];

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? num(s, NaN) : NaN;
}

function parseCsvText(text) {
  const lines = String(text).split(/\r?\n/).filter(Boolean);
  if (!lines.length) return [];
  const header = lines[0].split(",");
  const rows = [];
  for (let i = 1; i < lines.length; i++) {
    const cells = [];
    let cur = "";
    let q = false;
    for (const ch of lines[i]) {
      if (ch === '"') {
        q = !q;
        continue;
      }
      if (ch === "," && !q) {
        cells.push(cur);
        cur = "";
        continue;
      }
      cur += ch;
    }
    cells.push(cur);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
    rows.push(row);
  }
  return rows;
}

async function loadAuditLatest() {
  if (!existsSync(AUDIT)) return new Map();
  const rows = [];
  await new Promise((resolve, reject) => {
    createReadStream(AUDIT)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("error", reject)
      .on("end", resolve);
  });
  const latest = new Map();
  for (const r of rows) {
    const dg = Math.round(num(r.dg_id, NaN));
    const rnd = Math.round(num(r.display_round, NaN));
    const mkt = String(r.market || "").trim();
    const ev = String(r.event_name || "").trim();
    const cap = String(r.captured_at || "");
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || !mkt) continue;
    const key = `${dg}|${ev}|${rnd}|${mkt}`;
    const prev = latest.get(key);
    if (!prev || cap > prev.captured_at) latest.set(key, r);
  }
  return latest;
}

function normEv(s) {
  return String(s || "")
    .trim()
    .toLowerCase();
}

function buildWindIndex(hist) {
  const byKey = new Map();
  for (const b of Object.values(hist.byDgId || {})) {
    for (const r of b.rounds || []) {
      const w = num(r.weather_wind_mph, NaN);
      if (!Number.isFinite(w)) continue;
      const dg = Math.round(num(r.dg_id ?? b.dg_id, NaN));
      const rnd = Math.round(num(r.round_num ?? r.round, NaN));
      const yr = num(r.year, NaN) || parseInt(String(r.event_completed || "").split("/")[2], 10);
      const ev = normEv(r.event_name);
      if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;
      byKey.set(`${dg}|${yr}|${rnd}|${ev}`, { wind: w, event_name: r.event_name });
    }
  }
  return byKey;
}

function lookupWind(byKey, dg, rnd, eventName) {
  for (const yr of [2026, 2025, 2024, 2023, 2022]) {
    const rec = byKey.get(`${dg}|${yr}|${rnd}|${normEv(eventName)}`);
    if (rec) return rec.wind;
  }
  for (const [k, rec] of byKey) {
    const [dgS, , rndS] = k.split("|");
    if (Number(dgS) === dg && Number(rndS) === rnd && eventsLikelySame(eventName, rec.event_name)) return rec.wind;
  }
  return NaN;
}

function segmentStats(bets, keyFn) {
  const groups = new Map();
  for (const b of bets) {
    const k = keyFn(b);
    if (!k) continue;
    let g = groups.get(k);
    if (!g) g = { key: k, bets: 0, wins: 0, losses: 0, units: 0, edgeSum: 0, deltaSum: 0 };
    g.bets++;
    g.edgeSum += b.edge;
    g.deltaSum += b.modelBookDelta;
    const res = String(b.result).toUpperCase();
    if (res === "W") g.wins++;
    else if (res === "L") g.losses++;
    g.units += b.pnl;
    groups.set(k, g);
  }
  return [...groups.values()]
    .map((g) => {
      const graded = g.wins + g.losses;
      return {
        ...g,
        hitPct: graded ? (g.wins / graded) * 100 : NaN,
        roiPct: g.bets ? (g.units / g.bets) * 100 : NaN,
        avgEdge: g.bets ? g.edgeSum / g.bets : NaN,
        avgModelBookDelta: g.bets ? g.deltaSum / g.bets : NaN,
      };
    })
    .sort((a, b) => (b.roiPct ?? -999) - (a.roiPct ?? -999));
}

function bucketLabel(v, edges, labels) {
  for (let i = 0; i < edges.length; i++) {
    if (v < edges[i]) return labels[i];
  }
  return labels[labels.length - 1];
}

function corr(xs, ys) {
  const pairs = xs.map((x, i) => [x, ys[i]]).filter(([x, y]) => Number.isFinite(x) && Number.isFinite(y));
  if (pairs.length < 8) return NaN;
  const n = pairs.length;
  const mx = pairs.reduce((s, [x]) => s + x, 0) / n;
  const my = pairs.reduce((s, [, y]) => s + y, 0) / n;
  let nume = 0;
  let dx = 0;
  let dy = 0;
  for (const [x, y] of pairs) {
    nume += (x - mx) * (y - my);
    dx += (x - mx) ** 2;
    dy += (y - my) ** 2;
  }
  const den = Math.sqrt(dx * dy);
  return den > 0 ? nume / den : NaN;
}

function buildBets(detailRows, auditMap, windByKey, minEv = 5) {
  const out = [];
  for (const row of detailRows) {
    if (row.pricing_mode !== "default" || row.pricing_skill !== "default") continue;
    if (row.book_odds_source !== "pre_round_audit") continue;
    const dg = Math.round(num(row.dg_id, NaN));
    const rnd = Math.round(num(row.round, NaN));
    const ev = String(row.event_name || "").trim();

    for (const spec of MARKETS) {
      const bookLine = parseLine(row[spec.bookCol]);
      if (!Number.isFinite(bookLine)) continue;
      const modelLine = parseLine(row[spec.modelCol]);
      const overOdds = num(row[spec.overOdds], NaN);
      const underOdds = num(row[spec.underOdds], NaN);
      const mu = Number.isFinite(modelLine) ? modelLine : NaN;
      const { edgeOver, edgeUnder } = modelEdgePctAtLine(spec.market, mu, bookLine, overOdds, underOdds);
      const pick = pickBetSide(edgeOver, edgeUnder, minEv);
      if (!pick) continue;
      const side = pick.side;
      const result = side === "over" ? row[spec.overRes] : row[spec.underRes];
      const res = String(result).toUpperCase();
      if (res !== "W" && res !== "L" && res !== "P") continue;
      const betOdds = side === "over" ? overOdds : underOdds;
      const actual = parseLine(row[spec.actual]);

      const audit = auditMap.get(`${dg}|${ev}|${rnd}|${spec.market}`);
      const modelFairways = audit ? num(audit.model_fairways, NaN) : NaN;
      const modelGir = audit ? num(audit.model_gir, NaN) : NaN;
      const modelScore = audit ? num(audit.model_total_score, NaN) : NaN;
      const modelBirdies = audit ? num(audit.model_birdies, NaN) : NaN;
      const wind = lookupWind(windByKey, dg, rnd, ev);

      const modelBookDelta = Number.isFinite(mu) ? mu - bookLine : NaN;
      const pOver = modelProbOver(spec.market, mu, bookLine);
      const pPick = side === "over" ? pOver : 1 - pOver;
      const { fairOver, fairUnder } = devigFairTwoWay(overOdds, underOdds);
      const fairPick = side === "over" ? fairOver : fairUnder;

      out.push({
        market: spec.market,
        event: ev,
        course: row.course_used,
        round: rnd,
        player: row.player_name,
        dg,
        side,
        edge: pick.edge,
        result: res,
        pnl: pnlForResult(res, betOdds),
        modelLine: mu,
        bookLine,
        modelBookDelta,
        actual,
        modelFairways,
        modelGir,
        modelScore,
        modelBirdies,
        modelPutts: audit ? num(audit.model_putts, NaN) : NaN,
        wind,
        pPick,
        fairPick,
        beatFair: res === "W" || res === "L" ? (res === "W" ? 1 : 0) - (Number.isFinite(fairPick) ? fairPick : NaN) : NaN,
        lowerBetter: spec.lowerBetter,
      });
    }
  }
  return out;
}

function analyzeMarket(bets, market) {
  const m = bets.filter((b) => b.market === market);
  const graded = m.filter((b) => b.result === "W" || b.result === "L");
  const wins = graded.filter((b) => b.result === "W").length;
  const units = m.reduce((s, b) => s + b.pnl, 0);
  const win01 = graded.map((b) => (b.result === "W" ? 1 : 0));

  const factors = {
    modelBookDelta: graded.map((b) => b.modelBookDelta),
    edge: graded.map((b) => b.edge),
    modelFairways: graded.filter((b) => Number.isFinite(b.modelFairways)).map((b) => b.modelFairways),
    modelGir: graded.filter((b) => Number.isFinite(b.modelGir)).map((b) => b.modelGir),
    modelScore: graded.filter((b) => Number.isFinite(b.modelScore)).map((b) => b.modelScore),
    wind: graded.filter((b) => Number.isFinite(b.wind)).map((b) => b.wind),
  };

  const correlations = {};
  for (const [name, vals] of Object.entries(factors)) {
    if (vals.length !== win01.length && name !== "modelFairways" && name !== "modelGir" && name !== "modelScore" && name !== "wind") continue;
    const aligned = graded
      .map((b, i) => ({
        x:
          name === "modelFairways"
            ? b.modelFairways
            : name === "modelGir"
              ? b.modelGir
              : name === "modelScore"
                ? b.modelScore
                : name === "wind"
                  ? b.wind
                  : name === "edge"
                    ? b.edge
                    : b.modelBookDelta,
        y: win01[i],
      }))
      .filter((p) => Number.isFinite(p.x));
    correlations[name] = corr(
      aligned.map((p) => p.x),
      aligned.map((p) => p.y),
    );
  }

  return {
    market,
    bets: m.length,
    wins,
    losses: graded.length - wins,
    units: Math.round(units * 100) / 100,
    roiPct: m.length ? Math.round((units / m.length) * 1000) / 10 : NaN,
    hitPct: graded.length ? Math.round((wins / graded.length) * 1000) / 10 : NaN,
    correlationsWithWin: Object.fromEntries(
      Object.entries(correlations)
        .filter(([, v]) => Number.isFinite(v))
        .sort((a, b) => Math.abs(b[1]) - Math.abs(a[1]))
        .map(([k, v]) => [k, Math.round(v * 1000) / 1000]),
    ),
    bySide: segmentStats(m, (b) => b.side),
    byEdgeBucket: segmentStats(m, (b) =>
      bucketLabel(b.edge, [7.5, 12.5, 20], ["5–7.5%", "7.5–12.5%", "12.5–20%", "20%+"]),
    ),
    byModelVsBook: segmentStats(m, (b) => {
      if (!Number.isFinite(b.modelBookDelta)) return null;
      if (b.modelBookDelta >= 0.35) return "Model ≥0.35 above DK";
      if (b.modelBookDelta <= -0.35) return "Model ≥0.35 below DK";
      return "Model near DK (±0.35)";
    }),
    byWind: segmentStats(
      m.filter((b) => Number.isFinite(b.wind)),
      (b) => (b.wind > 10 ? "Windy (>10 mph)" : "Calm (≤10 mph)"),
    ),
    byRound: segmentStats(m, (b) => `Round ${b.round}`),
    profitablePlaybook: derivePlaybook(m, market),
  };
}

function derivePlaybook(bets, market) {
  const overs = bets.filter((b) => b.side === "over");
  const unders = bets.filter((b) => b.side === "under");
  const overRoi = overs.length ? overs.reduce((s, b) => s + b.pnl, 0) / overs.length : NaN;
  const underRoi = unders.length ? unders.reduce((s, b) => s + b.pnl, 0) / unders.length : NaN;
  const tips = [];
  if (Number.isFinite(overRoi) && overRoi > 0.05 && (underRoi < 0 || !Number.isFinite(underRoi))) {
    tips.push(`Prefer OVER picks only — unders lose at ${Number.isFinite(underRoi) ? (underRoi * 100).toFixed(1) : "?"}% ROI.`);
  }
  const bigDelta = bets.filter((b) => b.modelBookDelta >= 0.5);
  if (bigDelta.length >= 10) {
    const u = bigDelta.reduce((s, b) => s + b.pnl, 0);
    const r = u / bigDelta.length;
    if (r > 0.08) tips.push(`When model is ≥0.5 above DK line, ROI ≈ ${(r * 100).toFixed(1)}% (${bigDelta.length} bets).`);
  }
  const windy = bets.filter((b) => Number.isFinite(b.wind) && b.wind > 10);
  const calm = bets.filter((b) => Number.isFinite(b.wind) && b.wind <= 10);
  if (windy.length >= 15 && calm.length >= 15) {
    const wRoi = windy.reduce((s, b) => s + b.pnl, 0) / windy.length;
    const cRoi = calm.reduce((s, b) => s + b.pnl, 0) / calm.length;
    if (Math.abs(wRoi - cRoi) > 0.05) {
      tips.push(
        wRoi > cRoi
          ? `Windy rounds (>10 mph) outperform calm for ${market} (+${((wRoi - cRoi) * 100).toFixed(1)} pts ROI). Model weather adj (−0.14× difficulty) may under-price FW in wind.`
          : `Calm rounds outperform windy for ${market}.`,
      );
    }
  }
  if (market === "Fairways hit" || market === "GIR") {
    tips.push("Skill signal: model uses driving accuracy / GIR skill blended with course history; largest edge when model fairways ≠ DK (see modelBookDelta correlation).");
  }
  return tips;
}

function printReport(payload) {
  console.log("\n=== MARKET EDGE FACTOR ANALYSIS (≥5% EV, pre-round DK, flat 1u) ===\n");
  console.log("Market ranking by ROI:");
  for (const m of payload.marketSummary) {
    console.log(`  ${m.market.padEnd(14)} ${String(m.bets).padStart(4)} bets  ROI ${m.roiPct >= 0 ? "+" : ""}${m.roiPct}%  hit ${m.hitPct}%  units ${m.units >= 0 ? "+" : ""}${m.units}`);
  }
  const focus = payload.markets[payload.focusMarket] || payload.markets["Fairways hit"];
  if (!focus) return;
  console.log(`\n--- ${payload.focusMarket} deep dive ---`);
  console.log(`Bets: ${focus.bets} | ROI: ${focus.roiPct}% | Hit: ${focus.hitPct}%`);
  console.log("\nCorrelations with win (higher = factor predicts wins):");
  for (const [k, v] of Object.entries(focus.correlationsWithWin || {})) {
    console.log(`  ${k.padEnd(18)} ${v >= 0 ? "+" : ""}${v}`);
  }
  console.log("\nBy side:");
  for (const s of focus.bySide || []) {
    console.log(`  ${s.key.padEnd(8)} n=${s.bets} ROI ${s.roiPct >= 0 ? "+" : ""}${s.roiPct?.toFixed(1)}% hit ${s.hitPct?.toFixed(1)}%`);
  }
  console.log("\nBy model vs DK line:");
  for (const s of focus.byModelVsBook || []) {
    console.log(`  ${s.key.padEnd(24)} n=${s.bets} ROI ${s.roiPct >= 0 ? "+" : ""}${s.roiPct?.toFixed(1)}% avg Δ ${s.avgModelBookDelta?.toFixed(2)}`);
  }
  if ((focus.byWind || []).length) {
    console.log("\nBy wind (when history available):");
    for (const s of focus.byWind) {
      console.log(`  ${s.key.padEnd(18)} n=${s.bets} ROI ${s.roiPct >= 0 ? "+" : ""}${s.roiPct?.toFixed(1)}%`);
    }
  }
  console.log("\nBy edge bucket:");
  for (const s of focus.byEdgeBucket || []) {
    console.log(`  ${s.key.padEnd(12)} n=${s.bets} ROI ${s.roiPct >= 0 ? "+" : ""}${s.roiPct?.toFixed(1)}%`);
  }
  console.log("\nHow to bet it:");
  for (const t of focus.profitablePlaybook || []) console.log(`  • ${t}`);
  console.log(`\nFull JSON: ${OUT}\n`);
}

async function main() {
  const args = process.argv.slice(2);
  const focusIdx = args.indexOf("--market");
  const focusMarket = focusIdx >= 0 ? args[focusIdx + 1] : "Fairways hit";

  if (!existsSync(VS_ACTUAL)) throw new Error(`Missing ${VS_ACTUAL}`);
  const detailRows = parseCsvText(readFileSync(VS_ACTUAL, "utf8"));
  const auditMap = await loadAuditLatest();

  let windByKey = new Map();
  let windNote = "player_round_history empty — wind segments skipped unless history CSV/JSON populated";
  if (existsSync(HIST_JSON)) {
    try {
      const hist = JSON.parse(readFileSync(HIST_JSON, "utf8"));
      if (Object.keys(hist.byDgId || {}).length) {
        windByKey = buildWindIndex(hist);
        windNote = `wind from player_round_history (${windByKey.size} keyed rounds)`;
      }
    } catch {
      /* ignore */
    }
  }

  const allBets = buildBets(detailRows, auditMap, windByKey, 5);
  const marketSummary = MARKETS.map((spec) => {
    const a = analyzeMarket(allBets, spec.market);
    return { market: spec.market, bets: a.bets, roiPct: a.roiPct, hitPct: a.hitPct, units: a.units };
  }).sort((a, b) => b.roiPct - a.roiPct);

  const markets = {};
  for (const spec of MARKETS) markets[spec.market] = analyzeMarket(allBets, spec.market);

  const payload = {
    generated_at: new Date().toISOString(),
    min_ev_pct: 5,
    wind_data: windNote,
    focusMarket,
    marketSummary,
    markets,
    modelFactorReference: {
      fairways_hit: {
        weather: "statWeatherMuAdjustment: −0.14 × weather difficulty delta (wind/rain → fewer fairways in model)",
        skill: "dg_fairway_pct / driving_accuracy + historical FW~SG:OTT calibration",
        course: "venue_avg_fairways, course-table fit, similar-course blend",
        correlated: "Reconciled with GIR, score, bird/bog counts after unified factors",
      },
      gir: {
        weather: "−0.22 × weather difficulty delta",
        skill: "dg_gir_pct + GIR~SG:APP historical calibration",
      },
      total_score: {
        weather: "+1.0 × weather difficulty (full stroke effect)",
      },
    },
  };

  writeFileSync(OUT, JSON.stringify(payload, null, 2));
  printReport(payload);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
