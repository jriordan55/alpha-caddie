/**
 * Exhaustive SG categories + SG combos × rolling windows (incl. last round)
 * vs sportsbook O/U lines (DK close).
 */
import { createReadStream, writeFileSync } from "fs";
import { parse } from "csv-parse";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const DETAIL = path.join(WEB, "data", "round_projection_vs_actual.csv");
const HIST = path.resolve(WEB, "..", "data", "historical_rounds_all.csv");
const OUT_JSON = path.join(WEB, "data", "_preround_sg_vs_book.json");
const OUT_HTML = path.join(WEB, "pre-round-book-lines.html");

const WINDOWS = [1, 2, 3, 4, 5, 6, 8, 10, 12, 16, 20, 24, 32];

/** Base SG fields on each historical round */
const BASE_SG = ["sg_total", "sg_t2g", "sg_ott", "sg_app", "sg_arg", "sg_putt"];

/**
 * Named combos = sum of base components (pre-round averages summed, or mean of sum).
 * We store per-round combo values then roll them.
 */
const COMBOS = [
  { key: "sg_total", parts: ["sg_total"], label: "SG:Total" },
  { key: "sg_t2g", parts: ["sg_t2g"], label: "SG:T2G" },
  { key: "sg_ott", parts: ["sg_ott"], label: "SG:OTT" },
  { key: "sg_app", parts: ["sg_app"], label: "SG:APP" },
  { key: "sg_arg", parts: ["sg_arg"], label: "SG:ARG" },
  { key: "sg_putt", parts: ["sg_putt"], label: "SG:PUTT" },
  // 2-way
  { key: "sg_ott_app", parts: ["sg_ott", "sg_app"], label: "SG:OTT+APP" },
  { key: "sg_ott_arg", parts: ["sg_ott", "sg_arg"], label: "SG:OTT+ARG" },
  { key: "sg_ott_putt", parts: ["sg_ott", "sg_putt"], label: "SG:OTT+PUTT" },
  { key: "sg_app_arg", parts: ["sg_app", "sg_arg"], label: "SG:APP+ARG" },
  { key: "sg_app_putt", parts: ["sg_app", "sg_putt"], label: "SG:APP+PUTT" },
  { key: "sg_arg_putt", parts: ["sg_arg", "sg_putt"], label: "SG:ARG+PUTT" },
  { key: "sg_t2g_putt", parts: ["sg_t2g", "sg_putt"], label: "SG:T2G+PUTT" },
  // 3-way
  { key: "sg_ott_app_arg", parts: ["sg_ott", "sg_app", "sg_arg"], label: "SG:OTT+APP+ARG" },
  { key: "sg_ott_app_putt", parts: ["sg_ott", "sg_app", "sg_putt"], label: "SG:OTT+APP+PUTT" },
  { key: "sg_ott_arg_putt", parts: ["sg_ott", "sg_arg", "sg_putt"], label: "SG:OTT+ARG+PUTT" },
  { key: "sg_app_arg_putt", parts: ["sg_app", "sg_arg", "sg_putt"], label: "SG:APP+ARG+PUTT" },
  // all four parts (≈ total without double-counting t2g)
  {
    key: "sg_ott_app_arg_putt",
    parts: ["sg_ott", "sg_app", "sg_arg", "sg_putt"],
    label: "SG:OTT+APP+ARG+PUTT",
  },
];

const COMBO_KEYS = COMBOS.map((c) => c.key);
const COMBO_LABEL = Object.fromEntries(COMBOS.map((c) => [c.key, c.label]));

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function parseUsSortKey(mdy) {
  const m = String(mdy || "").match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  if (!m) return 0;
  return (+m[3] * 10000 + +m[1] * 100 + +m[2]) * 10;
}

function americanToDec(am) {
  const a = num(am, NaN);
  if (!Number.isFinite(a) || a === 0) return NaN;
  return a > 0 ? 1 + a / 100 : 1 + 100 / Math.abs(a);
}

function pnlFlat1u(won, am) {
  if (won == null) return 0;
  if (won === "P") return 0;
  const dec = americanToDec(am);
  if (!Number.isFinite(dec)) return won === "W" ? 0.91 : -1;
  return won === "W" ? dec - 1 : -1;
}

function mean(arr, key) {
  let s = 0,
    n = 0;
  for (const r of arr) {
    const v = r[key];
    if (Number.isFinite(v)) {
      s += v;
      n++;
    }
  }
  return n ? s / n : NaN;
}

function ewma(arr, key, alpha = 0.3) {
  let v = NaN;
  for (const r of arr) {
    const x = r[key];
    if (!Number.isFinite(x)) continue;
    v = Number.isFinite(v) ? alpha * x + (1 - alpha) * v : x;
  }
  return v;
}

function featKey(combo, w) {
  if (w === "season") return `season_${combo}`;
  if (w === "career") return `career_${combo}`;
  if (w === "ewma") return `ewma_${combo}`;
  if (w === 1) return `last_${combo}`;
  return `l${w}_${combo}`;
}

function allFeatKeys(combo) {
  const keys = [];
  for (const w of WINDOWS) keys.push(featKey(combo, w));
  keys.push(featKey(combo, "season"), featKey(combo, "career"), featKey(combo, "ewma"));
  return keys;
}

async function loadCsv(file) {
  const rows = [];
  await new Promise((res, rej) => {
    createReadStream(file)
      .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", res)
      .on("error", rej);
  });
  return rows;
}

/**
 * Directional prior: high SG → which side for each market.
 * Combos inherit from their "primary" skill story.
 */
const MARKETS = [
  {
    key: "score",
    label: "Round score",
    lineCol: "round_score_book_line",
    overCol: "round_score_over_odds",
    underCol: "round_score_under_odds",
    actualCol: "actual_round_score",
    // better SG → lower score → under
    highMeansOver: [],
    highMeansUnder: COMBO_KEYS,
  },
  {
    key: "birdies",
    label: "Birdies",
    lineCol: "birdies_book_line",
    overCol: "birdies_over_odds",
    underCol: "birdies_under_odds",
    actualCol: "actual_birdies",
    highMeansOver: COMBO_KEYS,
    highMeansUnder: [],
  },
  {
    key: "bogeys",
    label: "Bogeys",
    lineCol: "bogeys_book_line",
    overCol: "bogeys_over_odds",
    underCol: "bogeys_under_odds",
    actualCol: "actual_bogeys",
    highMeansOver: [],
    highMeansUnder: COMBO_KEYS,
  },
  {
    key: "gir",
    label: "GIR",
    lineCol: "gir_book_line",
    overCol: "gir_over_odds",
    underCol: "gir_under_odds",
    actualCol: "actual_gir",
    // ball-striking heavy; still test putt combos for completeness
    highMeansOver: COMBO_KEYS.filter((k) => !k.includes("putt") || k.includes("t2g") || k === "sg_total" || k.includes("app") || k.includes("ott")),
    highMeansUnder: [],
  },
  {
    key: "fairways",
    label: "Fairways hit",
    lineCol: "fairways_book_line",
    overCol: "fairways_over_odds",
    underCol: "fairways_under_odds",
    actualCol: "actual_fairways",
    // OTT / T2G / total / combos with OTT
    highMeansOver: COMBO_KEYS.filter(
      (k) => k.includes("ott") || k === "sg_t2g" || k === "sg_total" || k === "sg_ott_app_arg",
    ),
    highMeansUnder: [],
  },
];

// For GIR ensure we still get all combos (user asked for all)
MARKETS.find((m) => m.key === "gir").highMeansOver = [...COMBO_KEYS];
MARKETS.find((m) => m.key === "fairways").highMeansOver = [...COMBO_KEYS];

console.log("Loading historical rounds…");
const histRaw = await loadCsv(HIST);
const byDg = new Map();
for (const r of histRaw) {
  if (String(r.tour || "").toLowerCase() !== "pga") continue;
  const dg = Math.round(num(r.dg_id));
  const rs = num(r.round_score);
  if (!Number.isFinite(dg) || !Number.isFinite(rs) || rs < 55) continue;
  const base = {
    dg,
    year: Math.round(num(r.year)),
    evtN: normEvt(r.event_name),
    sortKey: parseUsSortKey(r.event_completed) + (Math.round(num(r.round_num)) || 1),
    rnd: Math.round(num(r.round_num)) || 1,
  };
  for (const k of BASE_SG) base[k] = num(r[k]);
  // per-round combo sums (NaN if any part missing)
  for (const c of COMBOS) {
    if (c.parts.length === 1) {
      base[c.key] = base[c.parts[0]];
      continue;
    }
    let s = 0;
    let ok = true;
    for (const p of c.parts) {
      if (!Number.isFinite(base[p])) {
        ok = false;
        break;
      }
      s += base[p];
    }
    base[c.key] = ok ? s : NaN;
  }
  if (!byDg.has(dg)) byDg.set(dg, []);
  byDg.get(dg).push(base);
}
for (const arr of byDg.values()) arr.sort((a, b) => a.sortKey - b.sortKey || a.rnd - b.rnd);

function preSg(dg, eventName, year, roundNum) {
  const arr = byDg.get(dg);
  if (!arr?.length) return null;
  const evtN = normEvt(eventName);
  const sameEvent = arr.filter((r) => r.year === year && r.evtN === evtN);
  const prior = [];
  for (const r of arr) {
    if (r.year === year && r.evtN === evtN) {
      if (r.rnd < roundNum) prior.push(r);
      continue;
    }
    if (!sameEvent.length) {
      if (r.year < year) prior.push(r);
      continue;
    }
    const eventStart = Math.min(...sameEvent.map((x) => x.sortKey - x.rnd + 1));
    if (r.sortKey < eventStart) prior.push(r);
  }
  if (prior.length < 1) return null;

  const season = prior.filter((r) => r.year === year);
  const feat = { n_prior: prior.length };
  for (const combo of COMBO_KEYS) {
    for (const w of WINDOWS) {
      feat[featKey(combo, w)] = mean(prior.slice(-w), combo);
    }
    feat[featKey(combo, "season")] = mean(season, combo);
    feat[featKey(combo, "career")] = mean(prior, combo);
    feat[featKey(combo, "ewma")] = ewma(prior.slice(-24), combo, 0.3);
  }
  return feat;
}

console.log("Loading tracker detail (book lines)…");
const detail = await loadCsv(DETAIL);

const bets = [];
for (const row of detail) {
  if (String(row.pricing_mode || "default") !== "default") continue;
  const dg = Math.round(num(row.dg_id));
  const rnd = Math.round(num(row.round));
  const event = String(row.event_name || "").trim();
  const evtN = normEvt(event);
  let yr = parseInt(String(row.exported_at || "").slice(0, 4), 10);
  if (!Number.isFinite(yr)) yr = new Date().getFullYear();
  const arr = byDg.get(dg);
  if (arr) {
    const hit = arr.find((r) => r.evtN === evtN && r.rnd === rnd);
    if (hit) yr = hit.year;
    else {
      const any = arr.find((r) => r.evtN === evtN);
      if (any) yr = any.year;
    }
  }
  const feat = preSg(dg, event, yr, rnd);
  if (!feat) continue;

  for (const m of MARKETS) {
    const line = num(row[m.lineCol]);
    const actual = num(row[m.actualCol]);
    const overOdds = num(row[m.overCol]);
    const underOdds = num(row[m.underCol]);
    if (!Number.isFinite(line) || line <= 0 || !Number.isFinite(actual)) continue;
    if (!Number.isFinite(overOdds) || overOdds === 0 || !Number.isFinite(underOdds) || underOdds === 0)
      continue;
    let result;
    if (actual > line) result = "over";
    else if (actual < line) result = "under";
    else result = "push";

    bets.push({
      market: m.key,
      label: m.label,
      line,
      actual,
      result,
      overOdds,
      underOdds,
      modelLine: num(row[m.key === "score" ? "round_score_line" : `${m.key}_line`]),
      feat,
    });
  }
}

console.log("Matched bets:", bets.length);

function gradeSide(bet, side) {
  if (bet.result === "push") return "P";
  if (bet.result === side) return "W";
  return "L";
}

function summarize(picked) {
  let wins = 0,
    losses = 0,
    pushes = 0,
    units = 0,
    n = 0;
  for (const { bet, side } of picked) {
    const g = gradeSide(bet, side);
    const odds = side === "over" ? bet.overOdds : bet.underOdds;
    units += pnlFlat1u(g, odds);
    n++;
    if (g === "W") wins++;
    else if (g === "L") losses++;
    else pushes++;
  }
  if (n < 40) return null;
  const graded = wins + losses;
  return {
    n,
    wins,
    losses,
    pushes,
    hit: graded ? wins / graded : null,
    units: Math.round(units * 100) / 100,
    roi: graded ? Math.round((units / graded) * 1000) / 10 : null,
  };
}

function evalMedian(marketKey, fk, highMeansOver) {
  const rows = bets.filter((b) => b.market === marketKey && Number.isFinite(b.feat[fk]));
  if (rows.length < 40) return null;
  const sorted = [...rows].sort((a, b) => a.feat[fk] - b.feat[fk]);
  const med = sorted[Math.floor(sorted.length / 2)].feat[fk];
  const picked = rows.map((b) => {
    const high = b.feat[fk] > med;
    return { bet: b, side: highMeansOver ? (high ? "over" : "under") : high ? "under" : "over" };
  });
  const s = summarize(picked);
  if (!s) return null;
  return {
    market: marketKey,
    featKey: fk,
    mode: highMeansOver ? "SG high → over" : "SG high → under",
    ...s,
  };
}

function evalTertile(marketKey, fk, highMeansOver) {
  const rows = bets.filter((b) => b.market === marketKey && Number.isFinite(b.feat[fk]));
  if (rows.length < 60) return null;
  const sorted = [...rows].sort((a, b) => a.feat[fk] - b.feat[fk]);
  const loCut = sorted[Math.floor(sorted.length / 3)].feat[fk];
  const hiCut = sorted[Math.floor((2 * sorted.length) / 3)].feat[fk];
  const picked = [];
  for (const b of rows) {
    const x = b.feat[fk];
    let side = null;
    if (x >= hiCut) side = highMeansOver ? "over" : "under";
    else if (x <= loCut) side = highMeansOver ? "under" : "over";
    if (side) picked.push({ bet: b, side });
  }
  const s = summarize(picked);
  if (!s) return null;
  return {
    market: marketKey,
    featKey: fk,
    mode: highMeansOver ? "SG tertile high→over" : "SG tertile high→under",
    ...s,
  };
}

function evalModelVsBook(marketKey, gap = 0) {
  const rows = bets.filter((b) => b.market === marketKey && Number.isFinite(b.modelLine));
  const picked = [];
  for (const b of rows) {
    let side = null;
    if (b.modelLine > b.line + gap) side = "over";
    else if (b.modelLine < b.line - gap) side = "under";
    if (side) picked.push({ bet: b, side });
  }
  const s = summarize(picked);
  if (!s) return null;
  return {
    market: marketKey,
    featKey: "model_mu",
    mode: gap > 0 ? `model_vs_book_gap_${gap}` : "model_vs_book",
    ...s,
  };
}

function comboFromFeat(fk) {
  if (fk === "model_mu") return "model_mu";
  for (const c of [...COMBO_KEYS].sort((a, b) => b.length - a.length)) {
    if (fk.endsWith(c) || fk.includes(`_${c}`) || fk === `last_${c}`) return c;
  }
  return "other";
}

function windowFromFeat(fk) {
  if (fk.startsWith("last_")) return "L1";
  if (fk.startsWith("season_")) return "season";
  if (fk.startsWith("career_")) return "career";
  if (fk.startsWith("ewma_")) return "ewma";
  const m = fk.match(/^l(\d+)_/);
  return m ? `L${m[1]}` : "?";
}

function baseRate(marketKey) {
  const rows = bets.filter((b) => b.market === marketKey);
  let over = 0,
    under = 0,
    push = 0;
  for (const b of rows) {
    if (b.result === "over") over++;
    else if (b.result === "under") under++;
    else push++;
  }
  const n = over + under + push;
  return { n, overPct: n ? over / n : null, underPct: n ? under / n : null, pushPct: n ? push / n : null };
}

function alwaysSide(marketKey, side) {
  const s = summarize(bets.filter((b) => b.market === marketKey).map((bet) => ({ bet, side })));
  return s ? { side, ...s } : { side, n: 0, hit: null, units: 0, roi: null };
}

console.log("Evaluating SG categories + combos × windows…");
const strategies = [];
const seen = new Set();
function push(s, label) {
  if (!s) return;
  const id = `${s.market}|${s.featKey}|${s.mode}`;
  if (seen.has(id)) return;
  seen.add(id);
  const combo = comboFromFeat(s.featKey);
  strategies.push({
    ...s,
    label,
    featLabel: s.featKey,
    combo,
    comboLabel: COMBO_LABEL[combo] || combo,
    window: windowFromFeat(s.featKey),
  });
}

for (const m of MARKETS) {
  for (const combo of m.highMeansOver) {
    for (const fk of allFeatKeys(combo)) {
      push(evalMedian(m.key, fk, true), m.label);
      push(evalTertile(m.key, fk, true), m.label);
    }
  }
  for (const combo of m.highMeansUnder) {
    for (const fk of allFeatKeys(combo)) {
      push(evalMedian(m.key, fk, false), m.label);
      push(evalTertile(m.key, fk, false), m.label);
    }
  }
  push(evalModelVsBook(m.key, 0), m.label);
  push(evalModelVsBook(m.key, 0.5), m.label);
}

strategies.sort((a, b) => (b.roi ?? -999) - (a.roi ?? -999));

const byMarketSummary = MARKETS.map((m) => {
  const br = baseRate(m.key);
  const mine = strategies.filter((s) => s.market === m.key && s.featKey !== "model_mu");
  const best = mine.slice(0, 12);
  const byCombo = {};
  for (const s of mine) {
    if (!byCombo[s.combo] || (s.roi ?? -999) > (byCombo[s.combo].roi ?? -999)) {
      byCombo[s.combo] = s;
    }
  }
  const bestByCombo = Object.values(byCombo).sort((a, b) => (b.roi ?? -999) - (a.roi ?? -999));
  // best last-round (L1) only
  const bestL1 = mine.filter((s) => s.window === "L1").slice(0, 8);
  return {
    label: m.label,
    key: m.key,
    nLines: bets.filter((b) => b.market === m.key).length,
    base: br,
    best,
    bestByCombo,
    bestL1,
  };
});

const baselines = {};
for (const m of MARKETS) {
  baselines[m.key] = { over: alwaysSide(m.key, "over"), under: alwaysSide(m.key, "under") };
}

const payload = {
  meta: {
    sourceDetail: "round_projection_vs_actual.csv (DK book lines + odds)",
    sourceHist: "historical_rounds_all.csv",
    combos: COMBOS.map((c) => ({ key: c.key, label: c.label, parts: c.parts })),
    windows: [...WINDOWS, "season", "career", "ewma"],
    nBets: bets.length,
    nStrategies: strategies.length,
  },
  byMarketSummary,
  baselines,
  topStrategies: strategies.slice(0, 50),
};

writeFileSync(OUT_JSON, JSON.stringify(payload, null, 2));

console.log(`\nEvaluated ${strategies.length} strategies. Top 30:`);
for (const s of payload.topStrategies.slice(0, 30)) {
  console.log(
    `${s.label} | ${s.comboLabel} ${s.window} | ${s.mode} | n=${s.n} hit=${((s.hit ?? 0) * 100).toFixed(1)}% roi=${s.roi}%`,
  );
}
for (const m of byMarketSummary) {
  console.log(`\n=== ${m.label} (${m.nLines}) — best combo ===`);
  for (const s of m.bestByCombo.slice(0, 10)) {
    console.log(`  ${s.comboLabel} ${s.window}: ${s.mode} n=${s.n} roi=${s.roi}%`);
  }
  console.log("  best L1 (last round):");
  for (const s of m.bestL1.slice(0, 5)) {
    console.log(`    ${s.comboLabel}: roi=${s.roi}% n=${s.n}`);
  }
}

function pct(x) {
  if (x == null || !Number.isFinite(x)) return "—";
  return `${(x * 100).toFixed(1)}%`;
}
function esc(s) {
  return String(s ?? "").replace(/&/g, "&amp;").replace(/</g, "&lt;");
}

const html = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>SG categories + combos vs sportsbook O/U</title>
<style>
:root{--bg:#0f1218;--panel:#171b24;--text:#e8eaed;--muted:#9aa3b2;--line:#2a3140;--accent:#6ea8fe;--good:#3dd68c;--bad:#f07178;--warn:#f5a524}
*{box-sizing:border-box}body{margin:0;font:15px/1.5 system-ui,Segoe UI,sans-serif;background:var(--bg);color:var(--text);padding:32px 20px 64px}
main{max-width:1100px;margin:0 auto}h1{font-size:1.55rem;margin:0 0 8px}h2{font-size:1.15rem;margin:28px 0 12px}h3{font-size:1rem;margin:20px 0 8px}
.sub{color:var(--muted);margin:0 0 20px}.callout{background:var(--panel);border:1px solid var(--line);border-left:3px solid var(--accent);padding:14px 16px;margin:16px 0}
table{width:100%;border-collapse:collapse;font-size:.85rem}th,td{border-bottom:1px solid var(--line);padding:7px 6px;text-align:left;vertical-align:top}
th{color:var(--muted);font-weight:600}.pos{color:var(--good)}.neg{color:var(--bad)}
.stats{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:16px 0}
.stat{background:var(--panel);border:1px solid var(--line);padding:12px}.stat b{display:block;font-size:1.2rem;color:var(--accent)}.stat span{color:var(--muted);font-size:.8rem}
@media(max-width:700px){.stats{grid-template-columns:1fr 1fr}}
</style>
</head>
<body>
<main>
<h1>SG categories + combos vs sportsbook O/U</h1>
<p class="sub">All base SG (OTT/APP/ARG/PUTT/T2G/Total) + 2/3/4-way sums · rolling L1–L32, season, career, EWMA · median &amp; tertile · DK lines, flat 1u</p>
<div class="callout"><strong>${payload.meta.nBets.toLocaleString()}</strong> graded bets · <strong>${payload.meta.nStrategies.toLocaleString()}</strong> strategies · ${COMBOS.length} SG features × ${WINDOWS.length + 3} windows</div>

<div class="stats">
  <div class="stat"><b>${COMBOS.length}</b><span>SG cats + combos</span></div>
  <div class="stat"><b>${WINDOWS.length + 3}</b><span>Lookbacks (incl. L1)</span></div>
  <div class="stat"><b>${payload.meta.nStrategies}</b><span>Rules tested</span></div>
  <div class="stat"><b>${byMarketSummary.reduce((a, m) => a + m.nLines, 0)}</b><span>O/U decisions</span></div>
</div>

<h2>SG features tested</h2>
<p class="sub">${COMBOS.map((c) => esc(c.label)).join(" · ")}</p>

<h2>Base rates</h2>
<table>
<thead><tr><th>Market</th><th>n</th><th>Over</th><th>Under</th><th>Always over</th><th>Always under</th></tr></thead>
<tbody>
${byMarketSummary
  .map((m) => {
    const bo = baselines[m.key].over;
    const bu = baselines[m.key].under;
    return `<tr><td>${esc(m.label)}</td><td>${m.nLines}</td><td>${pct(m.base.overPct)}</td><td>${pct(m.base.underPct)}</td>
    <td class="${(bo.roi ?? 0) >= 0 ? "pos" : "neg"}">${bo.roi ?? "—"}%</td>
    <td class="${(bu.roi ?? 0) >= 0 ? "pos" : "neg"}">${bu.roi ?? "—"}%</td></tr>`;
  })
  .join("")}
</tbody>
</table>

<h2>Best SG combo per market</h2>
${byMarketSummary
  .map((m) => {
    const rows = m.bestByCombo
      .map(
        (s) =>
          `<tr><td>${esc(s.comboLabel)}</td><td>${esc(s.window)}</td><td>${esc(s.mode)}</td><td>${s.n}</td><td>${s.hit != null ? (s.hit * 100).toFixed(1) + "%" : "—"}</td><td class="${(s.roi ?? 0) >= 0 ? "pos" : "neg"}">${s.roi ?? "—"}%</td><td class="${s.units >= 0 ? "pos" : "neg"}">${s.units}u</td></tr>`,
      )
      .join("");
    return `<h3>${esc(m.label)} (${m.nLines})</h3>
    <table><thead><tr><th>Combo</th><th>Window</th><th>Rule</th><th>n</th><th>Hit</th><th>ROI</th><th>Units</th></tr></thead><tbody>${rows}</tbody></table>`;
  })
  .join("")}

<h2>Best last-round (L1) SG by market</h2>
${byMarketSummary
  .map((m) => {
    const rows = m.bestL1
      .map(
        (s) =>
          `<tr><td>${esc(s.comboLabel)}</td><td>${esc(s.mode)}</td><td>${s.n}</td><td>${s.hit != null ? (s.hit * 100).toFixed(1) + "%" : "—"}</td><td class="${(s.roi ?? 0) >= 0 ? "pos" : "neg"}">${s.roi ?? "—"}%</td><td>${s.units}u</td></tr>`,
      )
      .join("");
    return `<h3>${esc(m.label)}</h3>
    <table><thead><tr><th>Combo</th><th>Rule</th><th>n</th><th>Hit</th><th>ROI</th><th>Units</th></tr></thead><tbody>${rows || "<tr><td colspan=6>—</td></tr>"}</tbody></table>`;
  })
  .join("")}

<h2>Top 40 overall</h2>
<table>
<thead><tr><th>Market</th><th>Combo</th><th>Window</th><th>Rule</th><th>n</th><th>Hit</th><th>ROI</th><th>Units</th></tr></thead>
<tbody>
${payload.topStrategies
  .slice(0, 40)
  .map((s) => {
    const hit = s.hit != null ? `${(s.hit * 100).toFixed(1)}%` : "—";
    return `<tr>
      <td>${esc(s.label)}</td><td>${esc(s.comboLabel)}</td><td>${esc(s.window)}</td><td>${esc(s.mode)}</td>
      <td>${s.n}</td><td>${hit}</td>
      <td class="${(s.roi ?? 0) >= 0 ? "pos" : "neg"}">${s.roi ?? "—"}%</td>
      <td class="${s.units >= 0 ? "pos" : "neg"}">${s.units >= 0 ? "+" : ""}${s.units}u</td>
    </tr>`;
  })
  .join("")}
</tbody>
</table>

<div class="callout" style="border-left-color:var(--warn);margin-top:28px">
<strong>Note:</strong> ${strategies.length} comparisons → expect some lucky positives. Prefer combos that win across several nearby windows with n ≳ 300. Model μ rows are reference only.
</div>
</main>
</body>
</html>`;

writeFileSync(OUT_HTML, html);
console.log("\nWrote", OUT_HTML);
