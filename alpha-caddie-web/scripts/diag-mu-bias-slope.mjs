#!/usr/bin/env node
/**
 * DK-paired μ vs actual bias / slope by market from round_projection_vs_actual.csv
 */
import { createReadStream } from "fs";
import { createInterface } from "readline";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PATH = join(WEB, "data", "round_projection_vs_actual.csv");

const MARKETS = [
  { market: "Total score", mu: "round_score_line", book: "round_score_book_line", act: "actual_round_score" },
  { market: "Birdies", mu: "birdies_line", book: "birdies_book_line", act: "actual_birdies" },
  { market: "Bogeys", mu: "bogeys_line", book: "bogeys_book_line", act: "actual_bogeys" },
  { market: "GIR", mu: "gir_line", book: "gir_book_line", act: "actual_gir" },
  { market: "Fairways hit", mu: "fairways_line", book: "fairways_book_line", act: "actual_fairways" },
];

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

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function fit(xs, ys) {
  const n = xs.length;
  if (n < 8) return { a: NaN, b: NaN, r: NaN, bias: NaN, rmse: NaN, n: 0 };
  let sx = 0;
  let sy = 0;
  let sxx = 0;
  let syy = 0;
  let sxy = 0;
  let bias = 0;
  let sse = 0;
  for (let i = 0; i < n; i++) {
    const x = xs[i];
    const y = ys[i];
    sx += x;
    sy += y;
    sxx += x * x;
    syy += y * y;
    sxy += x * y;
    bias += y - x;
    sse += (y - x) ** 2;
  }
  const den = n * sxx - sx * sx;
  const b = den !== 0 ? (n * sxy - sx * sy) / den : NaN;
  const a = (sy - b * sx) / n;
  const cov = sxy / n - (sx / n) * (sy / n);
  const vx = sxx / n - (sx / n) ** 2;
  const vy = syy / n - (sy / n) ** 2;
  const r = vx > 0 && vy > 0 ? cov / Math.sqrt(vx * vy) : NaN;
  return {
    a,
    b,
    r,
    bias: bias / n,
    rmse: Math.sqrt(sse / n),
    n,
  };
}

const rl = createInterface({ input: createReadStream(PATH, "utf8"), crlfDelay: Infinity });
let header = null;
/** @type {Record<string, string>[]} */
const rows = [];
const events = new Map();
for await (const line of rl) {
  if (!line.trim()) continue;
  const cells = parseCsvLine(line);
  if (!header) {
    header = cells;
    continue;
  }
  const row = {};
  for (let i = 0; i < header.length; i++) row[header[i]] = cells[i] ?? "";
  if (String(row.pricing_mode || "") !== "default") continue;
  if (String(row.pricing_skill || "") !== "default") continue;
  const ev = String(row.event_name || "");
  if (/rocket classic/i.test(ev)) continue;
  events.set(ev, (events.get(ev) || 0) + 1);
  rows.push(row);
}

console.log(`rows=${rows.length} events=${events.size}`);
console.log(
  [...events.entries()]
    .sort((a, b) => a[0].localeCompare(b[0]))
    .map(([e, c]) => `  ${e}: ${c}`)
    .join("\n"),
);
console.log("");
console.log("market\tn\tbias(act-μ)\tRMSE\ta\tb\tr");
for (const m of MARKETS) {
  const xs = [];
  const ys = [];
  for (const row of rows) {
    const mu = num(row[m.mu]);
    const book = num(row[m.book]);
    const act = num(row[m.act]);
    if (!Number.isFinite(mu) || !Number.isFinite(act) || !(book > 0)) continue;
    xs.push(mu);
    ys.push(act);
  }
  const f = fit(xs, ys);
  console.log(
    `${m.market}\t${f.n}\t${f.bias.toFixed(3)}\t${f.rmse.toFixed(3)}\t${f.a.toFixed(2)}\t${f.b.toFixed(3)}\t${f.r.toFixed(3)}`,
  );
}
