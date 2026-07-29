#!/usr/bin/env node
/**
 * Fit per-market reliability curves: raw model P(win) → empirical win rate.
 *
 * Uses graded DraftKings O/U sides from round_projection_vs_actual.csv and the
 * same discrete pricing as projection-tracker/ev-math.mjs.
 *
 * Output: alpha-caddie-web/data/win_prob_calibration.json
 *
 *   node scripts/fit-win-prob-calibration.mjs
 */
import { createReadStream, writeFileSync, existsSync } from "fs";
import { createInterface } from "readline";
import { dirname, join, resolve } from "path";
import { fileURLToPath, pathToFileURL } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const DETAIL = join(WEB, "data", "round_projection_vs_actual.csv");
const OUT = join(WEB, "data", "win_prob_calibration.json");

const { modelProbOver, impliedProbFromAmerican, num, clamp } = await import(
  pathToFileURL(join(WEB, "projection-tracker", "ev-math.mjs")).href
);

const MARKETS = [
  {
    market: "Total score",
    modelCol: "round_score_line",
    bookCol: "round_score_book_line",
    actualCol: "actual_round_score",
    overOdds: "round_score_over_odds",
    underOdds: "round_score_under_odds",
    overRes: "round_score_over",
    underRes: "round_score_under",
  },
  {
    market: "Birdies",
    modelCol: "birdies_line",
    bookCol: "birdies_book_line",
    actualCol: "actual_birdies",
    overOdds: "birdies_over_odds",
    underOdds: "birdies_under_odds",
    overRes: "birdies_over",
    underRes: "birdies_under",
  },
  {
    market: "Bogeys",
    modelCol: "bogeys_line",
    bookCol: "bogeys_book_line",
    actualCol: "actual_bogeys",
    overOdds: "bogeys_over_odds",
    underOdds: "bogeys_under_odds",
    overRes: "bogeys_over",
    underRes: "bogeys_under",
  },
  {
    market: "GIR",
    modelCol: "gir_line",
    bookCol: "gir_book_line",
    actualCol: "actual_gir",
    overOdds: "gir_over_odds",
    underOdds: "gir_under_odds",
    overRes: "gir_over",
    underRes: "gir_under",
  },
  {
    market: "Fairways hit",
    modelCol: "fairways_line",
    bookCol: "fairways_book_line",
    actualCol: "actual_fairways",
    overOdds: "fairways_over_odds",
    underOdds: "fairways_under_odds",
    overRes: "fairways_over",
    underRes: "fairways_under",
  },
];

const BUCKET = 0.04;
const PRIOR = 20; // shrink toward 0.5 when thin

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
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

function bucketProb(p) {
  const x = num(p, NaN);
  if (!Number.isFinite(x)) return NaN;
  return Math.round(clamp(x, 0.05, 0.95) / BUCKET) * BUCKET;
}

/** Pool-adjacent violators for monotone increasing rates. */
function pava(points) {
  const blocks = points.map((pt) => ({
    pSum: pt.p * pt.n,
    rateSum: pt.rate * pt.n,
    n: pt.n,
    pMin: pt.p,
    pMax: pt.p,
  }));
  let i = 0;
  while (i < blocks.length - 1) {
    const a = blocks[i];
    const b = blocks[i + 1];
    const rateA = a.rateSum / a.n;
    const rateB = b.rateSum / b.n;
    if (rateA <= rateB + 1e-12) {
      i++;
      continue;
    }
    blocks[i] = {
      pSum: a.pSum + b.pSum,
      rateSum: a.rateSum + b.rateSum,
      n: a.n + b.n,
      pMin: a.pMin,
      pMax: b.pMax,
    };
    blocks.splice(i + 1, 1);
    if (i > 0) i--;
  }
  return blocks.map((b) => ({
    p: b.pSum / b.n,
    rate: clamp(b.rateSum / b.n, 0.02, 0.98),
    n: b.n,
  }));
}

function ece(points, samples) {
  if (!samples.length) return NaN;
  let err = 0;
  for (const s of samples) {
    let rate = s.rawP;
    if (points.length) {
      if (s.rawP <= points[0].p) rate = points[0].rate;
      else if (s.rawP >= points[points.length - 1].p) rate = points[points.length - 1].rate;
      else {
        for (let i = 1; i < points.length; i++) {
          if (s.rawP <= points[i].p) {
            const a = points[i - 1];
            const b = points[i];
            const t = (s.rawP - a.p) / Math.max(1e-9, b.p - a.p);
            rate = a.rate + t * (b.rate - a.rate);
            break;
          }
        }
      }
    }
    err += Math.abs(rate - (s.won ? 1 : 0));
  }
  return err / samples.length;
}

async function loadRows(path) {
  if (!existsSync(path)) throw new Error(`Missing ${path}`);
  const rl = createInterface({ input: createReadStream(path, "utf8"), crlfDelay: Infinity });
  let header = null;
  /** @type {Record<string, string>[]} */
  const rows = [];
  for await (const line of rl) {
    if (!line.trim()) continue;
    const cells = parseCsvLine(line);
    if (!header) {
      header = cells;
      continue;
    }
    const row = {};
    for (let i = 0; i < header.length; i++) row[header[i]] = cells[i] ?? "";
    rows.push(row);
  }
  return rows;
}

function collectSamples(rows) {
  /** @type {Map<string, { wins: Map<number, number>, n: Map<number, number>, samples: object[] }>} */
  const byMarket = new Map();
  for (const m of MARKETS) byMarket.set(m.market, { wins: new Map(), n: new Map(), samples: [] });

  for (const row of rows) {
    if (String(row.pricing_mode || "") !== "default") continue;
    if (String(row.pricing_skill || "") !== "default") continue;
    const src = String(row.book_odds_source || "").trim();
    if (src !== "pre_round_audit" && src !== "draftkings_live") continue;

    for (const spec of MARKETS) {
      const mu = num(row[spec.modelCol], NaN);
      const line = num(row[spec.bookCol], NaN);
      if (!Number.isFinite(mu) || !Number.isFinite(line)) continue;
      const overOdds = num(row[spec.overOdds], NaN);
      const underOdds = num(row[spec.underOdds], NaN);
      if (!Number.isFinite(overOdds) && !Number.isFinite(underOdds)) continue;

      const pOver = modelProbOver(spec.market, mu, line);
      if (!Number.isFinite(pOver)) continue;

      const sides = [
        { side: "over", rawP: pOver, res: String(row[spec.overRes] || "").trim().toUpperCase(), odds: overOdds },
        { side: "under", rawP: 1 - pOver, res: String(row[spec.underRes] || "").trim().toUpperCase(), odds: underOdds },
      ];
      const bucket = byMarket.get(spec.market);
      for (const s of sides) {
        if (s.res !== "W" && s.res !== "L") continue;
        // Fit reliability on every graded side (richer curve); live picks still μ-align.
        const b = bucketProb(s.rawP);
        if (!Number.isFinite(b)) continue;
        bucket.n.set(b, (bucket.n.get(b) || 0) + 1);
        if (s.res === "W") bucket.wins.set(b, (bucket.wins.get(b) || 0) + 1);
        bucket.samples.push({
          rawP: s.rawP,
          won: s.res === "W",
          posted: impliedProbFromAmerican(s.odds),
        });
      }
    }
  }
  return byMarket;
}

function fitMarket(market, data) {
  const keys = [...data.n.keys()].sort((a, b) => a - b);
  const rawPts = [];
  for (const b of keys) {
    const n = data.n.get(b) || 0;
    if (n < 3) continue;
    const w = data.wins.get(b) || 0;
    const rate = (w + PRIOR * 0.5) / (n + PRIOR);
    rawPts.push({ p: b, rate, n });
  }
  if (rawPts.length < 2) {
    return {
      points: [
        { p: 0.35, rate: 0.4, n: 0 },
        { p: 0.5, rate: 0.5, n: 0 },
        { p: 0.65, rate: 0.6, n: 0 },
      ],
      n: data.samples.length,
      ece_raw: NaN,
      ece_cal: NaN,
      note: "thin_sample_fallback",
    };
  }
  const points = pava(rawPts);
  // Keep enough knots for interpolation; if PAVA collapses, seed identity anchors.
  let curve = points;
  if (curve.length < 2) {
    const mid = curve[0] || { p: 0.5, rate: 0.5, n: 0 };
    curve = [
      { p: Math.min(0.35, mid.p - 0.1), rate: clamp(mid.rate - 0.04, 0.02, 0.98), n: mid.n },
      { p: mid.p, rate: mid.rate, n: mid.n },
      { p: Math.max(0.65, mid.p + 0.1), rate: clamp(mid.rate + 0.04, 0.02, 0.98), n: mid.n },
    ];
  }
  let eceIdentity = 0;
  for (const s of data.samples) eceIdentity += Math.abs(s.rawP - (s.won ? 1 : 0));
  eceIdentity = data.samples.length ? eceIdentity / data.samples.length : NaN;
  const eceCal = ece(curve, data.samples);
  return {
    points: curve,
    n: data.samples.length,
    ece_raw: Number.isFinite(eceIdentity) ? Math.round(eceIdentity * 1e4) / 1e4 : NaN,
    ece_cal: Number.isFinite(eceCal) ? Math.round(eceCal * 1e4) / 1e4 : NaN,
  };
}

const rows = await loadRows(DETAIL);
const byMarket = collectSamples(rows);
/** @type {Record<string, object>} */
const markets = {};
for (const [market, data] of byMarket) {
  markets[market] = fitMarket(market, data);
}

const payload = {
  generated_at: new Date().toISOString(),
  source: "round_projection_vs_actual.csv",
  method: "bucket_pava_mu_aligned_dk_sides",
  bucket_width: BUCKET,
  prior: PRIOR,
  markets,
};

writeFileSync(OUT, JSON.stringify(payload, null, 2));
console.log(`[fit-win-prob-calibration] wrote ${OUT}`);
for (const [m, fit] of Object.entries(markets)) {
  console.log(
    `  ${m}: n=${fit.n} pts=${fit.points.length} ECE raw=${fit.ece_raw} cal=${fit.ece_cal}${fit.note ? ` (${fit.note})` : ""}`,
  );
}
