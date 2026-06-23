#!/usr/bin/env node
/** One-off / CLI: beat fair price stats from round_projection_vs_actual.csv */
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import {
  devigFairTwoWay,
  impliedProbFromAmerican,
  modelEdgePctAtLine,
  modelEdgeVsFairAtLine,
  modelProbOver,
  num,
  pickBetSide,
  pnlForResult,
} from "../projection-tracker/ev-math.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const CSV = join(WEB, "data/round_projection_vs_actual.csv");

const MARKETS = [
  { market: "Total score", modelCol: "round_score_line", bookCol: "round_score_book_line", overOdds: "round_score_over_odds", underOdds: "round_score_under_odds", overRes: "round_score_over", underRes: "round_score_under" },
  { market: "Birdies", modelCol: "birdies_line", bookCol: "birdies_book_line", overOdds: "birdies_over_odds", underOdds: "birdies_under_odds", overRes: "birdies_over", underRes: "birdies_under" },
  { market: "GIR", modelCol: "gir_line", bookCol: "gir_book_line", overOdds: "gir_over_odds", underOdds: "gir_under_odds", overRes: "gir_over", underRes: "gir_under" },
  { market: "Fairways hit", modelCol: "fairways_line", bookCol: "fairways_book_line", overOdds: "fairways_over_odds", underOdds: "fairways_under_odds", overRes: "fairways_over", underRes: "fairways_under" },
  { market: "Pars", modelCol: "pars_line", bookCol: "pars_book_line", overOdds: "pars_over_odds", underOdds: "pars_under_odds", overRes: "pars_over", underRes: "pars_under" },
  { market: "Bogeys", modelCol: "bogeys_line", bookCol: "bogeys_book_line", overOdds: "bogeys_over_odds", underOdds: "bogeys_under_odds", overRes: "bogeys_over", underRes: "bogeys_under" },
];

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"') {
      if (q && line[i + 1] === '"') {
        cur += '"';
        i++;
      } else q = !q;
      continue;
    }
    if (ch === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  out.push(cur);
  return out;
}

function parseCsv(text) {
  const lines = String(text).split(/\r?\n/).filter(Boolean);
  const header = parseCsvLine(lines[0]);
  const rows = [];
  for (let i = 1; i < lines.length; i++) {
    const cells = parseCsvLine(lines[i]);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
    rows.push(row);
  }
  return rows;
}

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? num(s, NaN) : NaN;
}

function analyze(minEv = 5) {
  const detail = parseCsv(readFileSync(CSV, "utf8"));
  const bets = [];
  const lines = [];

  for (const row of detail) {
    if (row.pricing_mode !== "default" || row.pricing_skill !== "default") continue;
    if (row.book_odds_source !== "pre_round_audit") continue;
    for (const spec of MARKETS) {
      const bookLine = parseLine(row[spec.bookCol]);
      if (!Number.isFinite(bookLine)) continue;
      const modelLine = parseLine(row[spec.modelCol]);
      const overOdds = num(row[spec.overOdds], NaN);
      const underOdds = num(row[spec.underOdds], NaN);
      const mu = Number.isFinite(modelLine) ? modelLine : NaN;
      const posted = modelEdgePctAtLine(spec.market, mu, bookLine, overOdds, underOdds);
      const fair = modelEdgeVsFairAtLine(spec.market, mu, bookLine, overOdds, underOdds);
      const pick = pickBetSide(posted.edgeOver, posted.edgeUnder, minEv);
      const bestFair = Math.max(fair.edgeFairOver, fair.edgeFairUnder);
      if (Number.isFinite(mu)) {
        lines.push({ market: spec.market, bestFair, beatsFair: Number.isFinite(bestFair) && bestFair > 0 });
      }
      if (!pick) continue;
      const side = pick.side;
      const betRes = side === "over" ? row[spec.overRes] : row[spec.underRes];
      const res = String(betRes).toUpperCase();
      if (res !== "W" && res !== "L" && res !== "P") continue;
      const fairProb = side === "over" ? fair.fairOver : fair.fairUnder;
      const postedProb = impliedProbFromAmerican(side === "over" ? overOdds : underOdds);
      const modelProb = side === "over" ? fair.pOver : fair.pUnder;
      bets.push({
        market: spec.market,
        event: row.event_name,
        res,
        fairProb,
        postedProb,
        modelProb,
        beatsFairPre: Number.isFinite(modelProb) && Number.isFinite(fairProb) && modelProb > fairProb,
        pnl: pnlForResult(res, side === "over" ? overOdds : underOdds),
      });
    }
  }

  const graded = bets.filter((b) => b.res === "W" || b.res === "L");
  const wins = graded.filter((b) => b.res === "W").length;
  const hitRate = graded.length ? (wins / graded.length) * 100 : NaN;
  const avgFair = graded.length ? (graded.reduce((s, b) => s + b.fairProb, 0) / graded.length) * 100 : NaN;
  const avgPosted = graded.length ? (graded.reduce((s, b) => s + b.postedProb, 0) / graded.length) * 100 : NaN;
  const units = bets.reduce((s, b) => s + b.pnl, 0);

  const byMarket = new Map();
  for (const b of graded) {
    let m = byMarket.get(b.market);
    if (!m) m = { wins: 0, n: 0, fairSum: 0, postedSum: 0 };
    m.n += 1;
    if (b.res === "W") m.wins += 1;
    m.fairSum += b.fairProb;
    m.postedSum += b.postedProb;
    byMarket.set(b.market, m);
  }

  return {
    minEv,
    bets: bets.length,
    graded: graded.length,
    hitRate,
    avgFair,
    avgPosted,
    beatFair: hitRate - avgFair,
    beatPosted: hitRate - avgPosted,
    vigCapture: hitRate - avgPosted - (hitRate - avgFair),
    preBetBeats: bets.filter((b) => b.beatsFairPre).length,
    linesWithModel: lines.length,
    linesBeatFair: lines.filter((l) => l.beatsFair).length,
    units,
    roi: bets.length ? (units / bets.length) * 100 : NaN,
    byMarket,
  };
}

for (const th of [0, 5, 10]) {
  const s = analyze(th);
  console.log(`\n=== +EV ≥ ${th}% (pre-round DK, flat 1u) ===`);
  console.log(`  Bets: ${s.bets} (${s.graded} W/L graded)`);
  console.log(`  Hit rate: ${s.hitRate.toFixed(1)}%`);
  console.log(`  Avg fair implied (devigged): ${s.avgFair.toFixed(1)}%`);
  console.log(`  Avg posted implied (with vig): ${s.avgPosted.toFixed(1)}%`);
  console.log(`  Beat FAIR price (hit − fair): ${s.beatFair >= 0 ? "+" : ""}${s.beatFair.toFixed(1)}%`);
  console.log(`  Beat POSTED price (hit − posted): ${s.beatPosted >= 0 ? "+" : ""}${s.beatPosted.toFixed(1)}%`);
  console.log(`  Vig captured (posted − fair beat): ${s.vigCapture >= 0 ? "+" : ""}${s.vigCapture.toFixed(1)}%`);
  console.log(`  Model > fair before bet: ${s.preBetBeats}/${s.bets} (${((s.preBetBeats / s.bets) * 100).toFixed(1)}%)`);
  console.log(`  Lines where model beats fair on a side: ${s.linesBeatFair}/${s.linesWithModel} (${((s.linesBeatFair / s.linesWithModel) * 100).toFixed(1)}%)`);
  console.log(`  ROI: ${s.roi >= 0 ? "+" : ""}${s.roi.toFixed(1)}% (${s.units >= 0 ? "+" : ""}${s.units.toFixed(1)}u)`);
  console.log("  By market (beat fair = hit% − fair%):");
  for (const [mk, m] of [...s.byMarket.entries()].sort((a, b) => a[0].localeCompare(b[0]))) {
    const hit = (m.wins / m.n) * 100;
    const fair = (m.fairSum / m.n) * 100;
    console.log(`    ${mk}: ${m.n} bets, hit ${hit.toFixed(1)}%, fair ${fair.toFixed(1)}%, beat fair ${(hit - fair).toFixed(1)}%`);
  }
}
