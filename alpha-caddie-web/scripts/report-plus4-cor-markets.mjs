#!/usr/bin/env node
/**
 * Per-market MAE / ROI / PnL for Plus4 COR vs published model lines.
 *
 * Total score μ is rebuilt with Plus4 COR. Counting markets are nudged from the
 * published model line by β × (plus4_score − published_score).
 *
 *   node scripts/report-plus4-cor-markets.mjs
 * → data/plus4_cor_market_report.json
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import { loadPlayerHistoryRoundsByDg } from "./course-skill-tailoring.mjs";
import {
  applyPlus4CorToPlayers,
  buildPlayerCourseResiduals,
  ensureCourseCorMatrix,
  num,
} from "./plus4-cor-score.mjs";
import { makePlus4ExtrasApplicator } from "./plus4-extra-signals.mjs";
import {
  americanToDecimal,
  modelEdgeVsFairAtLine,
  pickBetSideFair,
  pnlForResult,
} from "../projection-tracker/ev-math.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const HIST = join(WEB, "player_round_history.json");
const OUT = join(WEB, "data", "plus4_cor_market_report.json");

const MARKETS = [
  {
    key: "Total score",
    model: "round_score_line",
    book: "round_score_book_line",
    actual: "actual_round_score",
    over: "round_score_over_odds",
    under: "round_score_under_odds",
    beta: 1,
  },
  {
    key: "Birdies",
    model: "birdies_line",
    book: "birdies_book_line",
    actual: "actual_birdies",
    over: "birdies_over_odds",
    under: "birdies_under_odds",
    beta: -0.28, // lower score → more birdies
  },
  {
    key: "Pars",
    model: "pars_line",
    book: "pars_book_line",
    actual: "actual_pars",
    over: "pars_over_odds",
    under: "pars_under_odds",
    beta: 0.05,
  },
  {
    key: "Bogeys",
    model: "bogeys_line",
    book: "bogeys_book_line",
    actual: "actual_bogeys",
    over: "bogeys_over_odds",
    under: "bogeys_under_odds",
    beta: 0.22, // higher score → more bogeys
  },
  {
    key: "Fairways hit",
    model: "fairways_line",
    book: "fairways_book_line",
    actual: "actual_fairways",
    over: "fairways_over_odds",
    under: "fairways_under_odds",
    beta: -0.12,
  },
  {
    key: "GIR",
    model: "gir_line",
    book: "gir_book_line",
    actual: "actual_gir",
    over: "gir_over_odds",
    under: "gir_under_odds",
    beta: -0.35,
  },
];

function numStrict(x) {
  if (x === undefined || x === null) return NaN;
  const s = String(x).trim();
  if (!s) return NaN;
  const n = Number(s);
  return Number.isFinite(n) ? n : NaN;
}

function parseAm(x) {
  const s = String(x ?? "").trim().replace(/^\+/, "");
  if (!s) return NaN;
  const n = Number(s);
  return Number.isFinite(n) && n !== 0 ? n : NaN;
}

function skillSeedBefore(rounds, asofMs) {
  const prior = (rounds || []).filter((r) => {
    const t = Date.parse(String(r.play_date || r.event_completed || r.date || ""));
    return Number.isFinite(t) && t < asofMs && Number.isFinite(num(r.sg_total, NaN));
  });
  prior.sort((a, b) => {
    const ta = Date.parse(String(a.play_date || a.event_completed || "")) || 0;
    const tb = Date.parse(String(b.play_date || b.event_completed || "")) || 0;
    return tb - ta;
  });
  const take = prior.slice(0, 24);
  if (!take.length) return 0;
  let w = 0;
  let s = 0;
  take.forEach((r, i) => {
    const ww = 0.92 ** i;
    s += ww * num(r.sg_total, 0);
    w += ww;
  });
  return w > 0 ? s / w : 0;
}

function scoreFromMu(mu, fieldMeanMu, venueMeanScore) {
  return venueMeanScore - (mu - fieldMeanMu);
}

function gradeResult(side, actual, line) {
  if (!Number.isFinite(actual) || !Number.isFinite(line)) return "P";
  if (Math.abs(actual - line) < 1e-9) return "P";
  if (side === "over") return actual > line ? "W" : "L";
  return actual < line ? "W" : "L";
}

function emptyAcc() {
  return {
    mae_book_abs: 0,
    mae_book_n: 0,
    mae_act_abs: 0,
    mae_act_n: 0,
    bets: 0,
    wins: 0,
    losses: 0,
    pushes: 0,
    units: 0,
  };
}

async function loadDetail() {
  const rows = [];
  const parser = createReadStream(VS).pipe(
    parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
  );
  for await (const row of parser) {
    rows.push(row);
  }
  return rows;
}

async function main() {
  if (!existsSync(VS)) throw new Error(`missing ${VS}`);
  console.log("[plus4-markets] loading…");
  ensureCourseCorMatrix({ historyPath: HIST, force: false });
  const histJson = JSON.parse(readFileSync(HIST, "utf8"));
  const detail = await loadDetail();
  const dgIds = [
    ...new Set(detail.map((r) => Math.round(numStrict(r.dg_id))).filter(Number.isFinite)),
  ];
  const historyByDg = loadPlayerHistoryRoundsByDg(HIST, dgIds);

  // Build plus4 score by event|course|dg
  const groups = new Map();
  for (const row of detail) {
    const ck = normCourseNameKey(row.course_used || "");
    const event = String(row.event_name || "").trim();
    if (!ck || !event) continue;
    const k = `${event}\0${ck}`;
    if (!groups.has(k)) groups.set(k, []);
    groups.get(k).push(row);
  }

  const plus4ScoreByKey = new Map(); // event|ck|dg|round → score
  const publishedScoreByKey = new Map();

  for (const [gk, rows] of groups) {
    const [event, venueKey] = gk.split("\0");
    const asofMs = Math.min(
      ...rows.map((r) => Date.parse(String(r.projections_updated_at || r.exported_at || "")) || Date.now()),
    );
    const withBook = rows
      .map((r) => numStrict(r.round_score_book_line))
      .filter(Number.isFinite);
    const venueMeanBook =
      withBook.length > 0
        ? withBook.reduce((a, b) => a + b, 0) / withBook.length
        : rows.reduce((s, r) => s + (numStrict(r.round_score_line) || 70), 0) / rows.length;

    const byDg = new Map();
    for (const r of rows) {
      const dg = Math.round(numStrict(r.dg_id));
      if (!Number.isFinite(dg)) continue;
      if (!byDg.has(dg)) byDg.set(dg, r);
    }
    const seeds = [];
    for (const [dg, r] of byDg) {
      const rounds = historyByDg.get(dg) || [];
      const sg = skillSeedBefore(rounds, asofMs);
      seeds.push({
        dg_id: dg,
        player_name: r.player_name,
        mu_sg: sg,
        sg_total: sg,
        driving_distance_rating: NaN,
      });
    }
    const plus = seeds.map((s) => ({ ...s }));
    applyPlus4CorToPlayers(plus, {
      venueKey,
      courseUsed: rows[0].course_used,
      historyPath: HIST,
      asofMs,
      residualBundle: buildPlayerCourseResiduals(histJson, { asofMs }),
      applyExtras: makePlus4ExtrasApplicator(venueKey),
    });
    const fieldMean = plus.reduce((s, r) => s + r.mu_sg, 0) / Math.max(1, plus.length);
    const muMap = new Map(plus.map((r) => [r.dg_id, r.mu_sg]));

    for (const r of rows) {
      const dg = Math.round(numStrict(r.dg_id));
      const rnd = Math.round(numStrict(r.round) || numStrict(r.display_round) || 1);
      const key = `${event}|${venueKey}|${dg}|${rnd}`;
      const pub = numStrict(r.round_score_line);
      if (Number.isFinite(pub)) publishedScoreByKey.set(key, pub);
      const mu = muMap.get(dg);
      if (Number.isFinite(mu)) {
        plus4ScoreByKey.set(key, scoreFromMu(mu, fieldMean, venueMeanBook));
      }
    }
  }

  const recipes = {
    published: Object.fromEntries(MARKETS.map((m) => [m.key, emptyAcc()])),
    plus4_cor: Object.fromEntries(MARKETS.map((m) => [m.key, emptyAcc()])),
  };
  const recipes5 = {
    published: Object.fromEntries(MARKETS.map((m) => [m.key, emptyAcc()])),
    plus4_cor: Object.fromEntries(MARKETS.map((m) => [m.key, emptyAcc()])),
  };

  function addMae(acc, model, book, actual) {
    if (Number.isFinite(model) && Number.isFinite(book)) {
      acc.mae_book_abs += Math.abs(model - book);
      acc.mae_book_n++;
    }
    if (Number.isFinite(model) && Number.isFinite(actual)) {
      acc.mae_act_abs += Math.abs(model - actual);
      acc.mae_act_n++;
    }
  }

  function addBet(acc, side, odds, actual, line) {
    if (!side || !Number.isFinite(line) || !Number.isFinite(actual)) return;
    const am = Number.isFinite(odds) ? odds : -110;
    const res = gradeResult(side, actual, line);
    acc.bets++;
    if (res === "W") acc.wins++;
    else if (res === "L") acc.losses++;
    else acc.pushes++;
    acc.units += pnlForResult(res, am);
  }

  for (const row of detail) {
    const event = String(row.event_name || "").trim();
    const venueKey = normCourseNameKey(row.course_used || "");
    const dg = Math.round(numStrict(row.dg_id));
    const rnd = Math.round(numStrict(row.round) || numStrict(row.display_round) || 1);
    if (!event || !venueKey || !Number.isFinite(dg)) continue;
    const sk = `${event}|${venueKey}|${dg}|${rnd}`;
    const pubScore = publishedScoreByKey.get(sk);
    const p4Score = plus4ScoreByKey.get(sk);
    const scoreDelta =
      Number.isFinite(pubScore) && Number.isFinite(p4Score) ? p4Score - pubScore : 0;

    for (const m of MARKETS) {
      const book = numStrict(row[m.book]);
      const actual = numStrict(row[m.actual]);
      const published = numStrict(row[m.model]);
      let plus4Mu = published;
      if (m.key === "Total score" && Number.isFinite(p4Score)) plus4Mu = p4Score;
      else if (Number.isFinite(published) && Number.isFinite(scoreDelta)) {
        plus4Mu = published + m.beta * scoreDelta;
      }

      addMae(recipes.published[m.key], published, book, actual);
      addMae(recipes.plus4_cor[m.key], plus4Mu, book, actual);

      if (!Number.isFinite(book) || !Number.isFinite(actual)) continue;
      let overOdds = parseAm(row[m.over]);
      let underOdds = parseAm(row[m.under]);
      if (!Number.isFinite(overOdds)) overOdds = -110;
      if (!Number.isFinite(underOdds)) underOdds = -110;

      for (const [recipeName, mu] of [
        ["published", published],
        ["plus4_cor", plus4Mu],
      ]) {
        if (!Number.isFinite(mu)) continue;
        for (const [th, bucket] of [
          [0, recipes],
          [5, recipes5],
        ]) {
          const edge = modelEdgeVsFairAtLine(m.key, mu, book, overOdds, underOdds);
          const pick = pickBetSideFair(edge.edgeFairOver, edge.edgeFairUnder, th);
          if (!pick) continue;
          const odds = pick.side === "over" ? overOdds : underOdds;
          addBet(bucket[recipeName][m.key], pick.side, odds, actual, book);
        }
      }
    }
  }

  function summarize(accMap) {
    return MARKETS.map((m) => {
      const a = accMap[m.key];
      return {
        market: m.key,
        mae_vs_book: a.mae_book_n ? +(a.mae_book_abs / a.mae_book_n).toFixed(3) : null,
        mae_vs_actual: a.mae_act_n ? +(a.mae_act_abs / a.mae_act_n).toFixed(3) : null,
        n_mae_book: a.mae_book_n,
        n_mae_actual: a.mae_act_n,
        bets: a.bets,
        wins: a.wins,
        losses: a.losses,
        pushes: a.pushes,
        pnl_u: +a.units.toFixed(2),
        roi_pct: a.bets ? +((a.units / a.bets) * 100).toFixed(1) : null,
      };
    });
  }

  const out = {
    generated_at: new Date().toISOString(),
    source: "data/round_projection_vs_actual.csv",
    method:
      "Plus4 COR rebuilds Total score; counting μ = published + β×(plus4_score−published_score). EV uses fair-devig edges; missing odds → -110/-110.",
    n_detail_rows: detail.length,
    ev0: {
      published: summarize(recipes.published),
      plus4_cor: summarize(recipes.plus4_cor),
    },
    ev5: {
      published: summarize(recipes5.published),
      plus4_cor: summarize(recipes5.plus4_cor),
    },
  };

  // Combined totals
  for (const block of ["ev0", "ev5"]) {
    for (const recipe of ["published", "plus4_cor"]) {
      const rows = out[block][recipe];
      const bets = rows.reduce((s, r) => s + r.bets, 0);
      const units = rows.reduce((s, r) => s + r.pnl_u, 0);
      out[block][`${recipe}_combined`] = {
        bets,
        pnl_u: +units.toFixed(2),
        roi_pct: bets ? +((units / bets) * 100).toFixed(1) : null,
      };
    }
  }

  writeFileSync(OUT, JSON.stringify(out, null, 2));
  console.log(JSON.stringify({ ev0: out.ev0, ev5_combined: { published: out.ev5.published_combined, plus4: out.ev5.plus4_cor_combined } }, null, 2));
  console.log(`[plus4-markets] wrote ${OUT}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
