/**
 * Projection tracker — summary + bet-level detail CSV
 * npm run projection-tracker  →  http://localhost:5173/projection-tracker/
 */
import {
  americanToDecimal,
  computeStakeDollars,
  capDirectionalPostedEdges,
  devigFairTwoWay,
  formatAmerican,
  impliedProbFromAmerican,
  modelEdgePctAtLine,
  modelEdgeVsFairAtLine,
  num as nNum,
  pickBetSide,
  pnlForResult,
} from "./ev-math.mjs";
import { buildEdgeSignalInsights } from "./edge-signal-insights.mjs";
import { buildLiveBestBets, loadLiveBestBetsContext, invalidateLiveBestBetsCache } from "./live-best-bets.mjs";
import {
  filterOddsLines,
  fmtNum as oddsFmtNum,
  fmtPct as oddsFmtPct,
  summarizeByCourse,
  summarizeByMarket,
  summarizeByPlayer,
  summarizeOddsLines,
  uniqueCourses,
} from "./odds-csv-tab.mjs";

const RISK_STORAGE_KEY = "alphaCaddie_projection_tracker_risk_v1";

const CSV_CANDIDATES = [
  "../data/round_projection_vs_actual_summary.csv",
  "../data/round_projection_vs_actual_summary.csv.new",
];

const DETAIL_CANDIDATES = [
  "../data/round_projection_vs_actual.csv",
  "../data/round_projection_vs_actual.csv.new",
];

const MARKET_SPECS = [
  { market: "Total score", modelCol: "round_score_line", bookCol: "round_score_book_line", overOdds: "round_score_over_odds", underOdds: "round_score_under_odds", overRes: "round_score_over", underRes: "round_score_under", actual: "actual_round_score", decimals: 2 },
  { market: "Birdies", modelCol: "birdies_line", bookCol: "birdies_book_line", overOdds: "birdies_over_odds", underOdds: "birdies_under_odds", overRes: "birdies_over", underRes: "birdies_under", actual: "actual_birdies", decimals: 1 },
  { market: "GIR", modelCol: "gir_line", bookCol: "gir_book_line", overOdds: "gir_over_odds", underOdds: "gir_under_odds", overRes: "gir_over", underRes: "gir_under", actual: "actual_gir", decimals: 0 },
  { market: "Fairways hit", modelCol: "fairways_line", bookCol: "fairways_book_line", overOdds: "fairways_over_odds", underOdds: "fairways_under_odds", overRes: "fairways_over", underRes: "fairways_under", actual: "actual_fairways", decimals: 0 },
];

const MARKET_ORDER = [
  "Total score",
  "Birdies",
  "GIR",
  "Fairways hit",
];

/** Markets with real DK closing lines in odds.csv backtest (actionable book). */
const BETTABLE_MARKETS = new Set(["Birdies", "Total score"]);

/** Min |model − DK| before counting a bet — avoids fake edge on flat DK buckets. */
const MIN_LINE_GAP_BY_MARKET = {
  "Total score": 0.5,
  Birdies: 1.25,
  GIR: 1.0,
  "Fairways hit": 1.0,
};

/** @type {Record<string, string>[]} */
let ALL_ROWS = [];

/** @type {Record<string, string>[]} */
let DETAIL_ROWS = [];

/** @type {object | null} */
let OOS_REPORT = null;

/** @type {object | null} */
let ODDS_MODEL_ROI = null;

/** @type {Record<string, string>[]} */
let ODDS_LINES_ROWS = [];

const OOS_JSON_URL = "../data/walkforward_oos_roi.json";
const ODDS_ROI_URL = "../data/odds_model_roi_summary.csv";
const ODDS_LINES_URL = "../data/odds_model_roi_lines.csv";

const state = {
  tab: "overview",
  /** "" = all tournaments combined; otherwise event name */
  tournament: "",
  market: "Total score",
  minEv: 5,
  side: "",
  player: "",
  show: "bets",
  risk: loadRiskPrefs(),
  oddsCourse: "",
  oddsAt: "close",
  oddsModelOnly: false,
  oddsView: "lines",
};

/** @type {Awaited<ReturnType<typeof loadLiveBestBetsContext>> | null} */
let LIVE_CTX = null;

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function loadRiskPrefs() {
  const defaults = {
    bankroll: 10000,
    method: "flat_compound",
    unitPct: 1,
    maxStakePct: 5,
    roundCapPct: 15,
  };
  try {
    const raw = localStorage.getItem(RISK_STORAGE_KEY);
    if (!raw) return defaults;
    const j = JSON.parse(raw);
    return {
      bankroll: Math.max(100, num(j.bankroll) || defaults.bankroll),
      method: String(j.method || defaults.method),
      unitPct: Math.min(5, Math.max(0.25, num(j.unitPct) || defaults.unitPct)),
      maxStakePct: Math.min(25, Math.max(1, num(j.maxStakePct) || defaults.maxStakePct)),
      roundCapPct: Math.min(50, Math.max(5, num(j.roundCapPct) || defaults.roundCapPct)),
    };
  } catch {
    return defaults;
  }
}

function saveRiskPrefs() {
  try {
    localStorage.setItem(RISK_STORAGE_KEY, JSON.stringify(state.risk));
  } catch {
    /* ignore */
  }
}

function fmtUsd(v) {
  if (!Number.isFinite(v)) return "—";
  const sign = v < 0 ? "−" : "";
  return `${sign}$${Math.abs(v).toLocaleString("en-US", { minimumFractionDigits: 0, maximumFractionDigits: 0 })}`;
}

function fmtUsdPrecise(v) {
  if (!Number.isFinite(v)) return "—";
  const sign = v < 0 ? "−" : "";
  return `${sign}$${Math.abs(v).toLocaleString("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
}

function riskKellyMult(method) {
  if (method === "kelly_half") return 0.5;
  return 0.25;
}

function qualifiedBetRowsForRisk() {
  return explodeDetailToBets(DETAIL_ROWS)
    .filter((r) => {
      if (!r.qualified) return false;
      if (state.tournament && r.event_name !== state.tournament) return false;
      if (state.market && r.market !== state.market) return false;
      if (state.side && r.pickSide !== state.side) return false;
      if (state.player) {
        const q = state.player.toLowerCase();
        if (!String(r.player_name).toLowerCase().includes(q)) return false;
      }
      const res = String(r.betRes || "").toUpperCase();
      return res === "W" || res === "L" || res === "P";
    })
    .sort((a, b) => {
      const ta = String(a.exported_at || "").localeCompare(String(b.exported_at || ""));
      if (ta) return ta;
      const ev = String(a.event_name).localeCompare(String(b.event_name));
      if (ev) return ev;
      const rd = num(a.round) - num(b.round);
      if (rd) return rd;
      return String(a.player_name).localeCompare(String(b.player_name));
    });
}

function simulateBankrollHistory(bets, risk) {
  const B0 = risk.bankroll;
  let bankroll = B0;
  let peak = B0;
  let maxDd = 0;
  let totalStaked = 0;
  /** @type {object[]} */
  const ledger = [];
  /** @type {object[]} */
  const series = [{ i: 0, bankroll: B0 }];

  const roundGroups = new Map();
  for (const bet of bets) {
    const key = `${bet.event_name}\x1f${bet.round}`;
    if (!roundGroups.has(key)) roundGroups.set(key, []);
    roundGroups.get(key).push(bet);
  }

  const roundKeys = [...roundGroups.keys()].sort((a, b) => {
    const [ae, ar] = a.split("\x1f");
    const [be, br] = b.split("\x1f");
    const ea = roundGroups.get(a)[0]?.exported_at || "";
    const eb = roundGroups.get(b)[0]?.exported_at || "";
    const t = String(ea).localeCompare(String(eb));
    if (t) return t;
    const ev = ae.localeCompare(be);
    if (ev) return ev;
    return num(ar) - num(br);
  });

  const stakeOpts = () => ({
    bankroll0: B0,
    unitPct: risk.unitPct,
    maxStakePct: risk.maxStakePct,
    kellyMult: riskKellyMult(risk.method),
  });

  for (const rk of roundKeys) {
    const group = roundGroups.get(rk) || [];
    const brBeforeRound = bankroll;
    const sized = [];
    for (const bet of group) {
      const dec = bet.betDec;
      const modelP = bet.modelProb;
      if (!Number.isFinite(dec) || dec <= 1 || !Number.isFinite(modelP)) continue;
      const nominal = computeStakeDollars(brBeforeRound, modelP, dec, risk.method, stakeOpts());
      if (nominal <= 0) continue;
      sized.push({ bet, nominal });
    }
    if (!sized.length) continue;

    const cap = brBeforeRound * (risk.roundCapPct / 100);
    const nominalTotal = sized.reduce((s, x) => s + x.nominal, 0);
    const scale = nominalTotal > cap && cap > 0 ? cap / nominalTotal : 1;

    for (const { bet, nominal } of sized) {
      const stake = nominal * scale;
      if (stake <= 0) continue;
      const dec = bet.betDec;
      const res = String(bet.betRes).toUpperCase();
      const brBefore = bankroll;
      let pnl = 0;
      if (res === "W") {
        pnl = stake * (dec - 1);
        bankroll += pnl;
      } else if (res === "L") {
        pnl = -stake;
        bankroll -= stake;
      }
      totalStaked += stake;
      peak = Math.max(peak, bankroll);
      maxDd = Math.max(maxDd, peak - bankroll);
      const entry = {
        ...bet,
        stake,
        pnl,
        bankrollAfter: bankroll,
        bankrollBefore: brBefore,
      };
      ledger.push(entry);
      series.push({ i: ledger.length, bankroll });
    }
  }

  const ending = bankroll;
  const pl = ending - B0;
  const roi = B0 > 0 ? (pl / B0) * 100 : NaN;
  const avgStake = ledger.length ? totalStaked / ledger.length : NaN;
  const maxDdPct = peak > 0 ? (maxDd / peak) * 100 : NaN;

  return { B0, ending, pl, roi, peak, maxDd, maxDdPct, ledger, series, bets: ledger.length, avgStake, totalStaked };
}

function stakeGuideByMarket(bets, bankroll, risk) {
  /** @type {Map<string, { edges: number[], odds: number[], probs: number[] }>} */
  const m = new Map();
  for (const b of bets) {
    if (!Number.isFinite(b.pickEdge) || !Number.isFinite(b.betOdds)) continue;
    let acc = m.get(b.market);
    if (!acc) acc = { edges: [], odds: [], probs: [] };
    acc.edges.push(b.pickEdge);
    acc.odds.push(b.betOdds);
    if (Number.isFinite(b.modelProb)) acc.probs.push(b.modelProb);
    m.set(b.market, acc);
  }
  const median = (arr) => {
    if (!arr.length) return NaN;
    const s = [...arr].sort((a, b) => a - b);
    const mid = Math.floor(s.length / 2);
    return s.length % 2 ? s[mid] : (s[mid - 1] + s[mid]) / 2;
  };
  return sortMarkets(
    [...m.entries()].map(([market, acc]) => {
      const medEdge = median(acc.edges);
      const medOdds = median(acc.odds);
      const medProb = median(acc.probs);
      const dec = americanToDecimal(medOdds);
      const stake = computeStakeDollars(bankroll, medProb, dec, risk.method, {
        bankroll0: bankroll,
        unitPct: risk.unitPct,
        maxStakePct: risk.maxStakePct,
        kellyMult: riskKellyMult(risk.method),
      });
      return {
        market,
        medEdge,
        medOdds,
        stake,
        pct: bankroll > 0 ? (stake / bankroll) * 100 : NaN,
      };
    }),
  );
}

function renderBankrollChart(svgEl, series, B0) {
  if (!svgEl || series.length < 2) {
    if (svgEl) svgEl.innerHTML = '<text x="12" y="24" fill="#94a3b8" font-size="14">Not enough bets to chart.</text>';
    return;
  }
  const w = 800;
  const h = 220;
  const pad = { t: 16, r: 12, b: 28, l: 56 };
  const vals = series.map((p) => p.bankroll);
  const minV = Math.min(B0, ...vals);
  const maxV = Math.max(B0, ...vals);
  const span = Math.max(maxV - minV, B0 * 0.02);
  const y0 = minV - span * 0.05;
  const y1 = maxV + span * 0.05;
  const xScale = (i) => pad.l + (i / (series.length - 1)) * (w - pad.l - pad.r);
  const yScale = (v) => pad.t + (1 - (v - y0) / (y1 - y0)) * (h - pad.t - pad.b);
  const pts = series.map((p) => `${xScale(p.i).toFixed(1)},${yScale(p.bankroll).toFixed(1)}`).join(" ");
  const startY = yScale(B0);
  const endColor = series[series.length - 1].bankroll >= B0 ? "#10b981" : "#f87171";
  svgEl.innerHTML = `
    <line x1="${pad.l}" y1="${startY.toFixed(1)}" x2="${w - pad.r}" y2="${startY.toFixed(1)}" stroke="rgba(255,255,255,0.12)" stroke-dasharray="4 4"/>
    <text x="${pad.l}" y="${h - 6}" fill="#64748b" font-size="11">Start</text>
    <text x="${w - pad.r - 28}" y="${h - 6}" fill="#64748b" font-size="11" text-anchor="end">End</text>
    <text x="8" y="${pad.t + 8}" fill="#64748b" font-size="10">${fmtUsd(y1)}</text>
    <text x="8" y="${h - pad.b}" fill="#64748b" font-size="10">${fmtUsd(y0)}</text>
    <polyline fill="none" stroke="${endColor}" stroke-width="2" points="${pts}"/>
    <circle cx="${xScale(series[series.length - 1].i)}" cy="${yScale(series[series.length - 1].bankroll)}" r="4" fill="${endColor}"/>
  `;
}

function renderRisk() {
  const risk = state.risk;
  const bankrollEl = document.getElementById("risk-bankroll");
  const methodEl = document.getElementById("risk-method");
  const unitEl = document.getElementById("risk-unit-pct");
  const maxEl = document.getElementById("risk-max-stake");
  const capEl = document.getElementById("risk-round-cap");
  if (bankrollEl) bankrollEl.value = String(risk.bankroll);
  if (methodEl) methodEl.value = risk.method;
  if (unitEl) unitEl.value = String(risk.unitPct);
  if (maxEl) maxEl.value = String(risk.maxStakePct);
  if (capEl) capEl.value = String(risk.roundCapPct);

  const bets = qualifiedBetRowsForRisk();
  const sim = simulateBankrollHistory(bets, risk);
  const oneUnit = risk.bankroll * (risk.unitPct / 100);
  const methodLabel =
    methodEl?.selectedOptions?.[0]?.textContent || risk.method;

  document.getElementById("risk-kpis").innerHTML = `
    <div class="kpi-card">
      <div class="kpi-label">Starting bankroll</div>
      <div class="kpi-value">${fmtUsd(sim.B0)}</div>
      <div class="kpi-sub">1 unit = ${fmtUsdPrecise(oneUnit)} (${risk.unitPct}%)</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Would have now</div>
      <div class="kpi-value ${clsSigned(sim.pl)}">${fmtUsd(sim.ending)}</div>
      <div class="kpi-sub">${sim.pl >= 0 ? "+" : ""}${fmtUsdPrecise(sim.pl)} · ${methodLabel}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Simulated ROI</div>
      <div class="kpi-value ${clsSigned(sim.roi)}">${fmtPct(sim.roi)}</div>
      <div class="kpi-sub">${sim.bets} bets · ≥${state.minEv}% EV</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Max drawdown</div>
      <div class="kpi-value neg">${fmtUsd(sim.maxDd)}</div>
      <div class="kpi-sub">${fmt(sim.maxDdPct, 1)}% from peak ${fmtUsd(sim.peak)}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Avg stake</div>
      <div class="kpi-value">${fmtUsdPrecise(sim.avgStake)}</div>
      <div class="kpi-sub">${fmtUsd(sim.totalStaked)} total risked</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Should risk (typical)</div>
      <div class="kpi-value">${fmtUsdPrecise(oneUnit)}</div>
      <div class="kpi-sub">1 unit at current bankroll · see market table</div>
    </div>
  `;

  renderBankrollChart(document.getElementById("risk-chart"), sim.series, sim.B0);

  const guide = stakeGuideByMarket(bets, risk.bankroll, risk);
  document.querySelector("#risk-stake-table tbody").innerHTML = guide.length
    ? guide
        .map(
          (g) => `<tr>
        <td>${g.market}</td>
        <td class="num ${clsSigned(g.medEdge)}">${fmtPct(g.medEdge)}</td>
        <td class="num">${formatAmerican(g.medOdds)}</td>
        <td class="num">${fmtUsdPrecise(g.stake)}</td>
        <td class="num">${fmt(g.pct, 2)}%</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="5">No qualified bets for stake guide at current filters.</td></tr>`;

  const recent = sim.ledger.slice(-25).reverse();
  document.querySelector("#risk-recent-table tbody").innerHTML = recent.length
    ? recent
        .map(
          (r) => `<tr>
        <td class="player-cell">${r.event_name}</td>
        <td class="num">${r.round}</td>
        <td class="player-cell">${r.player_name}</td>
        <td>${r.market}</td>
        <td class="num">${fmtUsdPrecise(r.stake)}</td>
        <td>${resultBadge(r.betRes)}</td>
        <td class="num ${clsSigned(r.pnl)}">${r.pnl >= 0 ? "+" : ""}${fmtUsdPrecise(r.pnl)}</td>
        <td class="num">${fmtUsd(r.bankrollAfter)}</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="8">No simulated bets — adjust filters or min EV %.</td></tr>`;
}

function syncRiskFromForm() {
  state.risk = {
    bankroll: Math.max(100, num(document.getElementById("risk-bankroll")?.value) || 10000),
    method: String(document.getElementById("risk-method")?.value || "flat_compound"),
    unitPct: Math.min(5, Math.max(0.25, num(document.getElementById("risk-unit-pct")?.value) || 1)),
    maxStakePct: Math.min(25, Math.max(1, num(document.getElementById("risk-max-stake")?.value) || 5)),
    roundCapPct: Math.min(50, Math.max(5, num(document.getElementById("risk-round-cap")?.value) || 15)),
  };
  saveRiskPrefs();
  renderRisk();
}

function fmt(v, d = 2) {
  if (!Number.isFinite(v)) return "—";
  return (Math.round(v * 10 ** d) / 10 ** d).toFixed(d);
}

function fmtPct(v) {
  if (!Number.isFinite(v)) return "—";
  const sign = v > 0 ? "+" : "";
  return `${sign}${fmt(v, 1)}%`;
}

function clsSigned(v) {
  if (!Number.isFinite(v) || Math.abs(v) < 0.05) return "neutral";
  return v > 0 ? "pos" : "neg";
}

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
  const lines = String(text || "").split(/\r?\n/).filter((l) => l.length > 0);
  if (!lines.length) return [];
  const header = parseCsvLine(lines[0]);
  const rows = [];
  for (let i = 1; i < lines.length; i++) {
    const cells = parseCsvLine(lines[i]);
    /** @type {Record<string, string>} */
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
    rows.push(row);
  }
  return rows;
}

function uniqueSorted(vals) {
  return [...new Set(vals.filter(Boolean))].sort((a, b) => a.localeCompare(b));
}

function marketSortKey(m) {
  const i = MARKET_ORDER.indexOf(m);
  return i >= 0 ? i : 99;
}

function sortMarkets(rows) {
  return [...rows].sort((a, b) => marketSortKey(a.market) - marketSortKey(b.market));
}

function exportTimestamps(rows) {
  return uniqueSorted(rows.map((r) => r.exported_at)).reverse();
}

function allTournamentNames(rows = ALL_ROWS) {
  return uniqueSorted(
    rows.map((r) => r.event_name).filter((e) => e && e !== "(all events)"),
  );
}

function isPerEventRow(r) {
  const s = String(r.section || "");
  return (
    s === "model_vs_book" ||
    s === "ev_backtest" ||
    s === "model_vs_book_by_market" ||
    s === "ev_backtest_by_market"
  );
}

/** EV threshold buckets are cumulative — use one slice from ev_backtest_by_market. */
function evRowsAtMinEdge(rows = activeRows(), { bettableOnly = false } = {}) {
  return rows.filter((r) => {
    if (r.section !== "ev_backtest_by_market") return false;
    if (num(r.ev_threshold_pct) !== state.minEv) return false;
    if (!num(r.bets)) return false;
    if (bettableOnly && !BETTABLE_MARKETS.has(r.market)) return false;
    if (state.market && r.market !== state.market) return false;
    if (state.side && r.bet_side !== state.side) return false;
    return true;
  });
}

function tournamentScoreLineStats(rows) {
  const tsRows = rows.filter(
    (r) => r.section === "model_vs_book_by_market" && r.market === "Total score" && num(r.n_line_pairs) > 0,
  );
  if (!tsRows.length) {
    const fallback = rows.find(
      (r) => r.section === "model_vs_book" && r.market === "Total score" && r.pricing_mode === "default",
    );
    return { rmse: num(fallback?.rmse), mae: num(fallback?.mae) };
  }
  let sq = 0;
  let abs = 0;
  let n = 0;
  for (const r of tsRows) {
    const nn = num(r.n_line_pairs);
    const rmse = num(r.rmse);
    const mae = num(r.mae);
    if (!nn || !Number.isFinite(rmse)) continue;
    sq += rmse * rmse * nn;
    abs += (Number.isFinite(mae) ? mae : rmse) * nn;
    n += nn;
  }
  return { rmse: n ? Math.sqrt(sq / n) : NaN, mae: n ? abs / n : NaN };
}

function tournamentEvTotals(rows, { bettableOnly = false } = {}) {
  const evRows = evRowsAtMinEdge(rows, { bettableOnly });
  const units = evRows.reduce((s, r) => s + (num(r.units_net) || 0), 0);
  const bets = evRows.reduce((s, r) => s + (num(r.bets) || 0), 0);
  return { units, bets, roi: bets > 0 ? (units / bets) * 100 : NaN };
}

/** Each tournament's rows live under the export run that built them. */
function latestExportForTournament(name, rows = ALL_ROWS) {
  return exportTimestamps(rows.filter((r) => r.event_name === name && isPerEventRow(r)))[0] || "";
}

/** Latest per-tournament model + EV rows (one export per event). */
function latestRowsForTournament(name, rows = ALL_ROWS) {
  const exp = latestExportForTournament(name, rows);
  if (!exp) return [];
  return rows.filter((r) => r.exported_at === exp && r.event_name === name && isPerEventRow(r));
}

/** All tournaments: stitch each event's latest export rows together. */
function combinedTournamentRows(rows = ALL_ROWS) {
  const out = [];
  for (const name of allTournamentNames(rows)) {
    out.push(...latestRowsForTournament(name, rows));
  }
  return out;
}

function activeRows() {
  let rows = state.tournament
    ? latestRowsForTournament(state.tournament)
    : combinedTournamentRows();
  if (state.market) rows = rows.filter((r) => r.market === state.market);
  return rows;
}

function lineRows() {
  return sortMarkets(
    activeRows().filter((r) => r.section === "model_vs_book" && num(r.n_line_pairs) > 0),
  );
}

function parseLine(v) {
  const s = String(v ?? "").trim();
  if (!s) return NaN;
  return nNum(s, NaN);
}

function resultBadge(res) {
  const r = String(res || "").trim().toUpperCase();
  if (!r) return '<span class="res res-na">—</span>';
  const cls = r === "W" ? "res-win" : r === "L" ? "res-loss" : "res-push";
  return `<span class="res ${cls}">${r}</span>`;
}

function explodeDetailToBets(rows) {
  /** @type {object[]} */
  const out = [];
  for (const row of rows) {
    if (row.pricing_mode !== "default" || row.pricing_skill !== "default") continue;
    if (row.book_odds_source !== "pre_round_audit") continue;
    for (const spec of MARKET_SPECS) {
      const bookLine = parseLine(row[spec.bookCol]);
      if (!Number.isFinite(bookLine)) continue;
      const modelLine = parseLine(row[spec.modelCol]);
      const overOdds = nNum(row[spec.overOdds], NaN);
      const underOdds = nNum(row[spec.underOdds], NaN);
      const actual = parseLine(row[spec.actual]);
      const mu = Number.isFinite(modelLine) ? modelLine : NaN;
      let { edgeOver, edgeUnder } = modelEdgePctAtLine(spec.market, mu, bookLine, overOdds, underOdds);
      ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, bookLine));
      const fair = modelEdgeVsFairAtLine(spec.market, mu, bookLine, overOdds, underOdds);
      const pModelOver = fair.pOver;
      const pModelUnder = fair.pUnder;
      const pick = pickBetSide(edgeOver, edgeUnder, state.minEv, mu, bookLine);
      const bestSide =
        Number.isFinite(edgeOver) && Number.isFinite(edgeUnder)
          ? edgeOver >= edgeUnder
            ? { side: "over", edge: edgeOver }
            : { side: "under", edge: edgeUnder }
          : null;
      const activePick = pick || (state.show === "all" ? bestSide : null);
      const side = activePick?.side || null;
      const betRes = side === "over" ? row[spec.overRes] : side === "under" ? row[spec.underRes] : "";
      const betOdds = side === "over" ? overOdds : side === "under" ? underOdds : NaN;
      const fairProb = side === "over" ? fair.fairOver : side === "under" ? fair.fairUnder : NaN;
      const postedProb =
        side === "over"
          ? impliedProbFromAmerican(overOdds)
          : side === "under"
            ? impliedProbFromAmerican(underOdds)
            : NaN;
      const modelProb = side === "over" ? pModelOver : side === "under" ? pModelUnder : NaN;
      const edgeFairPick =
        side === "over" ? fair.edgeFairOver : side === "under" ? fair.edgeFairUnder : NaN;
      const lineGap = Number.isFinite(modelLine) ? Math.abs(modelLine - bookLine) : NaN;
      const minGap = MIN_LINE_GAP_BY_MARKET[spec.market] ?? 0.5;
      const gapOk = Number.isFinite(lineGap) && lineGap >= minGap;
      const qualified = Boolean(pick) && gapOk;
      out.push({
        event_name: row.event_name,
        round: row.round,
        dg_id: row.dg_id,
        player_name: row.player_name,
        market: spec.market,
        modelLine,
        bookLine,
        diff: Number.isFinite(modelLine) ? modelLine - bookLine : NaN,
        overOdds,
        underOdds,
        overRes: row[spec.overRes],
        underRes: row[spec.underRes],
        actual,
        edgeOver,
        edgeUnder,
        edgeFairOver: fair.edgeFairOver,
        edgeFairUnder: fair.edgeFairUnder,
        fairOver: fair.fairOver,
        fairUnder: fair.fairUnder,
        pModelOver,
        pModelUnder,
        pickSide: side,
        pickEdge: activePick?.edge ?? NaN,
        edgeFairPick,
        modelProb,
        fairProb,
        postedProb,
        beatsFairPreBet:
          qualified && Number.isFinite(modelProb) && Number.isFinite(fairProb) ? modelProb > fairProb : null,
        qualified,
        betRes,
        betOdds,
        betDec: americanToDecimal(betOdds),
        exported_at: row.exported_at,
        pnl: qualified && side ? pnlForResult(String(betRes).trim().toUpperCase(), betOdds) : NaN,
        decimals: spec.decimals,
      });
    }
  }
  return out;
}

function activeBetRows() {
  let rows = explodeDetailToBets(DETAIL_ROWS);
  if (state.tournament) rows = rows.filter((r) => r.event_name === state.tournament);
  if (state.market) rows = rows.filter((r) => r.market === state.market);
  if (state.side) rows = rows.filter((r) => r.pickSide === state.side);
  if (state.player) {
    const q = state.player.toLowerCase();
    rows = rows.filter((r) => String(r.player_name).toLowerCase().includes(q));
  }
  if (state.show === "bets") rows = rows.filter((r) => r.qualified);
  return rows.sort((a, b) => {
    const ev = String(a.event_name).localeCompare(String(b.event_name));
    if (ev) return ev;
    const rd = num(a.round) - num(b.round);
    if (rd) return rd;
    const pl = String(a.player_name).localeCompare(String(b.player_name));
    if (pl) return pl;
    return marketSortKey(a.market) - marketSortKey(b.market);
  });
}

function aggregateBeatFairStats(rows) {
  const qualified = rows.filter((r) => r.qualified);
  const graded = qualified.filter((r) => {
    const res = String(r.betRes).toUpperCase();
    return res === "W" || res === "L";
  });
  const wins = graded.filter((r) => String(r.betRes).toUpperCase() === "W").length;
  const hitRate = graded.length ? (wins / graded.length) * 100 : NaN;
  const fairN = graded.filter((r) => Number.isFinite(r.fairProb)).length;
  const postedN = graded.filter((r) => Number.isFinite(r.postedProb)).length;
  const avgFair =
    fairN > 0
      ? (graded.reduce((s, r) => s + (Number.isFinite(r.fairProb) ? r.fairProb : 0), 0) / fairN) * 100
      : NaN;
  const avgPosted =
    postedN > 0
      ? (graded.reduce((s, r) => s + (Number.isFinite(r.postedProb) ? r.postedProb : 0), 0) / postedN) * 100
      : NaN;
  const preBetEligible = qualified.filter((r) => r.beatsFairPreBet !== null);
  const preBetBeats = preBetEligible.filter((r) => r.beatsFairPreBet).length;
  const withModel = rows.filter(
    (r) => Number.isFinite(r.pModelOver) && Number.isFinite(r.fairOver) && Number.isFinite(r.fairUnder),
  );
  const modelBeatsFairLine = withModel.filter((r) => {
    const bestFair = Math.max(r.edgeFairOver, r.edgeFairUnder);
    return Number.isFinite(bestFair) && bestFair > 0;
  }).length;
  return {
    hitRate,
    avgFair,
    avgPosted,
    beatFair: Number.isFinite(hitRate) && Number.isFinite(avgFair) ? hitRate - avgFair : NaN,
    beatPosted: Number.isFinite(hitRate) && Number.isFinite(avgPosted) ? hitRate - avgPosted : NaN,
    preBetPct: preBetEligible.length ? (preBetBeats / preBetEligible.length) * 100 : NaN,
    preBetBeats,
    preBetEligible: preBetEligible.length,
    modelBeatsFairPct: withModel.length ? (modelBeatsFairLine / withModel.length) * 100 : NaN,
    modelBeatsFairLine,
    withModel: withModel.length,
    graded: graded.length,
  };
}

function renderBets() {
  const showEvent = !state.tournament;
  document.getElementById("bets-col-event").hidden = !showEvent;
  const rows = activeBetRows();
  const qualified = rows.filter((r) => r.qualified);
  const bets = qualified.length;
  const units = qualified.reduce((s, r) => s + (Number.isFinite(r.pnl) ? r.pnl : 0), 0);
  const wins = qualified.filter((r) => String(r.betRes).toUpperCase() === "W").length;
  const losses = qualified.filter((r) => String(r.betRes).toUpperCase() === "L").length;
  const pushes = qualified.filter((r) => String(r.betRes).toUpperCase() === "P").length;
  const roi = bets > 0 ? (units / bets) * 100 : NaN;
  const fairStats = aggregateBeatFairStats(rows);

  document.getElementById("bets-kpis").innerHTML = `
    <div class="kpi-card">
      <div class="kpi-label">Rows shown</div>
      <div class="kpi-value">${rows.length}</div>
      <div class="kpi-sub">${state.show === "bets" ? `≥${state.minEv}% edge` : "all graded lines"}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Qualified bets</div>
      <div class="kpi-value">${bets}</div>
      <div class="kpi-sub">${wins}W · ${losses}L · ${pushes}P</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Units</div>
      <div class="kpi-value ${clsSigned(units)}">${units >= 0 ? "+" : ""}${fmt(units, 2)}u</div>
      <div class="kpi-sub">flat 1u · pre-round DK</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">ROI</div>
      <div class="kpi-value ${clsSigned(roi)}">${fmtPct(roi)}</div>
      <div class="kpi-sub">qualified bets only</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Beat fair price</div>
      <div class="kpi-value ${clsSigned(fairStats.beatFair)}">${fmtPct(fairStats.beatFair)}</div>
      <div class="kpi-sub">${fmt(fairStats.hitRate, 1)}% hit vs ${fmt(fairStats.avgFair, 1)}% fair (${fairStats.graded} graded)</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">DK margin</div>
      <div class="kpi-value">${fmt(Number.isFinite(fairStats.avgPosted) && Number.isFinite(fairStats.avgFair) ? fairStats.avgPosted - fairStats.avgFair : NaN, 1)}%</div>
      <div class="kpi-sub">posted − fair implied on picks</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Model &gt; fair (pre-bet)</div>
      <div class="kpi-value">${fmt(fairStats.preBetPct, 1)}%</div>
      <div class="kpi-sub">${fairStats.preBetBeats}/${fairStats.preBetEligible} qualified picks</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Lines model beats fair</div>
      <div class="kpi-value">${fmt(fairStats.modelBeatsFairPct, 1)}%</div>
      <div class="kpi-sub">${fairStats.modelBeatsFairLine}/${fairStats.withModel} with model + DK odds</div>
    </div>
  `;

  const fmtLine = (v, d) => (Number.isFinite(v) ? fmt(v, d) : "—");
  document.querySelector("#bets-table tbody").innerHTML = rows.length
    ? rows
        .map((r) => {
          const pickCls = r.qualified ? "pick-qualified" : "pick-muted";
          const pickLabel = r.pickSide
            ? `<span class="${pickCls}">${r.pickSide}</span>`
            : "—";
          const betCell = r.qualified ? resultBadge(r.betRes) : "—";
          const pnlCell = r.qualified && Number.isFinite(r.pnl)
            ? `<span class="${clsSigned(r.pnl)}">${r.pnl >= 0 ? "+" : ""}${fmt(r.pnl, 2)}</span>`
            : "—";
          return `<tr>
        ${showEvent ? `<td>${r.event_name}</td>` : ""}
        <td class="num">${r.round}</td>
        <td class="player-cell">${r.player_name}</td>
        <td>${r.market}</td>
        <td class="num line-model">${fmtLine(r.modelLine, r.decimals)}</td>
        <td class="num line-book">${fmtLine(r.bookLine, r.decimals)}</td>
        <td class="num ${clsSigned(-r.diff)}">${Number.isFinite(r.diff) ? (r.diff > 0 ? "+" : "") + fmt(r.diff, r.decimals) : "—"}</td>
        <td class="num">${formatAmerican(r.overOdds) || "—"}</td>
        <td>${resultBadge(r.overRes)}</td>
        <td class="num">${formatAmerican(r.underOdds) || "—"}</td>
        <td>${resultBadge(r.underRes)}</td>
        <td>${pickLabel}</td>
        <td class="num ${clsSigned(r.pickEdge)}">${Number.isFinite(r.pickEdge) ? fmtPct(r.pickEdge) : "—"}</td>
        <td class="num ${clsSigned(r.edgeFairPick)}" title="Model edge vs devigged fair price (no margin)">${Number.isFinite(r.edgeFairPick) ? fmtPct(r.edgeFairPick) : "—"}</td>
        <td class="num">${fmtLine(r.actual, r.decimals)}</td>
        <td>${betCell}</td>
        <td class="num">${pnlCell}</td>
      </tr>`;
        })
        .join("")
    : `<tr><td colspan="${showEvent ? 17 : 16}">No bet rows — lower min EV %, switch to “All graded lines”, or pick another tournament.</td></tr>`;
}

function evRows() {
  return evRowsAtMinEdge(activeRows(), { bettableOnly: false })
    .sort((a, b) => {
      const ev = String(a.event_name).localeCompare(String(b.event_name));
      if (ev) return ev;
      const mk = marketSortKey(a.market) - marketSortKey(b.market);
      if (mk) return mk;
      const th = num(a.ev_threshold_pct) - num(b.ev_threshold_pct);
      if (th) return th;
      return String(a.bet_side).localeCompare(String(b.bet_side));
    });
}

function aggregateLineByMarket(rows) {
  /** @type {Map<string, { market: string, sq: number, abs: number, n: number }>} */
  const m = new Map();
  const byMarket = rows.filter(
    (r) => r.section === "model_vs_book_by_market" && num(r.n_line_pairs) > 0,
  );
  const pool = byMarket.length
    ? byMarket
    : rows.filter((r) => r.section === "model_vs_book" && num(r.n_line_pairs) > 0);
  for (const r of pool) {
    const n = num(r.n_line_pairs);
    const rmse = num(r.rmse);
    const mae = num(r.mae);
    if (!n || !Number.isFinite(rmse)) continue;
    let acc = m.get(r.market);
    if (!acc) acc = { market: r.market, sq: 0, abs: 0, n: 0 };
    acc.sq += rmse * rmse * n;
    acc.abs += (Number.isFinite(mae) ? mae : rmse) * n;
    acc.n += n;
    m.set(r.market, acc);
  }
  return sortMarkets(
    [...m.values()].map((a) => ({
      market: a.market,
      rmse: a.n ? Math.sqrt(a.sq / a.n) : NaN,
      mae: a.n ? a.abs / a.n : NaN,
      n_line_pairs: a.n,
    })),
  );
}

function aggregateEvByMarketSide(rows) {
  /** @type {Map<string, { market: string, side: string, bets: number, wins: number, losses: number, pushes: number, units: number }>} */
  const m = new Map();
  for (const r of rows) {
    if (r.section !== "ev_backtest_by_market") continue;
    const key = `${r.market}\x1f${r.bet_side}`;
    let acc = m.get(key);
    if (!acc) {
      acc = { market: r.market, side: r.bet_side, bets: 0, wins: 0, losses: 0, pushes: 0, units: 0 };
      m.set(key, acc);
    }
    acc.bets += num(r.bets) || 0;
    acc.wins += num(r.wins) || 0;
    acc.losses += num(r.losses) || 0;
    acc.pushes += num(r.pushes) || 0;
    acc.units += num(r.units_net) || 0;
  }
  return [...m.values()].map((a) => ({
    ...a,
    roi: a.bets > 0 ? (a.units / a.bets) * 100 : NaN,
    winPct: a.bets > 0 ? (a.wins / a.bets) * 100 : NaN,
  }));
}

function bestEvPerMarket(rows) {
  const agg = aggregateEvByMarketSide(rows);
  const best = new Map();
  for (const a of agg) {
    const cur = best.get(a.market);
    if (!cur || a.roi > cur.roi) best.set(a.market, a);
  }
  return sortMarkets([...best.values()]);
}

function heatmapData(rows) {
  const evOnly = rows.filter((r) => r.section === "ev_backtest_by_market" && num(r.bets) > 0);
  const thresholds = uniqueSorted(evOnly.map((r) => r.ev_threshold_pct))
    .map(Number)
    .filter(Number.isFinite)
    .sort((a, b) => a - b);
  const markets = uniqueSorted(evOnly.map((r) => r.market)).sort(
    (a, b) => marketSortKey(a) - marketSortKey(b),
  );
  const cell = new Map();
  for (const m of markets) {
    for (const th of thresholds) {
      let units = 0;
      let bets = 0;
      for (const c of evOnly.filter((r) => r.market === m && num(r.ev_threshold_pct) === th)) {
        units += num(c.units_net) || 0;
        bets += num(c.bets) || 0;
      }
      cell.set(`${m}\x1f${th}`, bets > 0 ? (units / bets) * 100 : NaN);
    }
  }
  return { thresholds, markets, cell };
}

function roiHeatColor(roi) {
  if (!Number.isFinite(roi)) return "transparent";
  if (roi >= 15) return "rgba(0, 196, 107, 0.45)";
  if (roi >= 5) return "rgba(0, 196, 107, 0.22)";
  if (roi >= 0) return "rgba(139, 143, 156, 0.15)";
  if (roi >= -10) return "rgba(255, 77, 79, 0.18)";
  return "rgba(255, 77, 79, 0.35)";
}

function renderBarChart(el, items, { valueKey, format = (v) => fmt(v, 2), invert = false }) {
  el.innerHTML = "";
  if (!items.length) {
    el.innerHTML = '<p class="note">No data for current filters.</p>';
    return;
  }
  const vals = items.map((it) => Math.abs(num(it[valueKey]))).filter(Number.isFinite);
  const max = Math.max(...vals, 0.001);
  for (const it of items) {
    const v = num(it[valueKey]);
    const row = document.createElement("div");
    row.className = "bar-row";
    const pct = Math.min(100, (Math.abs(v) / max) * 100);
    let fillClass = "";
    if (invert) {
      if (v <= max * 0.35) fillClass = "";
      else if (v <= max * 0.6) fillClass = "warn";
      else fillClass = "bad";
    } else if (v < 0) fillClass = "bad";
    row.innerHTML = `
      <span class="bar-label">${it.market || it.label}</span>
      <div class="bar-track"><div class="bar-fill ${fillClass}" style="width:${pct}%"></div></div>
      <span class="bar-val ${clsSigned(invert ? -v : v)}">${format(v)}</span>
    `;
    el.appendChild(row);
  }
}

function overviewLineRows() {
  const raw = lineRows();
  if (state.tournament) return raw;
  return aggregateLineByMarket(raw);
}

function renderOverview() {
  const lines = overviewLineRows();
  const evRows = evRowsAtMinEdge(undefined, { bettableOnly: false });
  const evAgg = aggregateEvByMarketSide(evRows);
  const totalUnits = evAgg.reduce((s, a) => s + a.units, 0);
  const totalBets = evAgg.reduce((s, a) => s + a.bets, 0);
  const totalScore = lines.find((r) => r.market === "Total score");
  const bestRoi = evAgg.reduce((best, a) => (!best || a.roi > best.roi ? a : best), null);
  const worstRmse = [...lines].sort((a, b) => num(b.rmse) - num(a.rmse))[0];

  document.getElementById("overview-kpis").innerHTML = `
    <div class="kpi-card">
      <div class="kpi-label">Total score RMSE</div>
      <div class="kpi-value">${fmt(num(totalScore?.rmse), 2)}</div>
      <div class="kpi-sub">${state.tournament || "combined"} · strokes vs book</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Total score MAE</div>
      <div class="kpi-value">${fmt(num(totalScore?.mae), 2)}</div>
      <div class="kpi-sub">${totalScore?.n_line_pairs || 0} line pairs</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">EV units</div>
      <div class="kpi-value ${clsSigned(totalUnits)}">${totalUnits >= 0 ? "+" : ""}${fmt(totalUnits, 1)}u</div>
      <div class="kpi-sub">${totalBets} bets · all markets · ${state.minEv}% edge</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">ROI</div>
      <div class="kpi-value ${clsSigned(totalBets ? (totalUnits / totalBets) * 100 : NaN)}">${fmtPct(totalBets ? (totalUnits / totalBets) * 100 : NaN)}</div>
      <div class="kpi-sub">flat 1u per bet</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Best edge pocket</div>
      <div class="kpi-value ${clsSigned(bestRoi?.roi)}">${bestRoi ? `${bestRoi.market} ${bestRoi.side}` : "—"}</div>
      <div class="kpi-sub">${bestRoi ? `${fmtPct(bestRoi.roi)} · ${bestRoi.bets} bets` : "try min EV 0%"}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Largest line gap</div>
      <div class="kpi-value">${worstRmse?.market || "—"}</div>
      <div class="kpi-sub">${worstRmse ? `RMSE ${fmt(num(worstRmse.rmse), 2)}` : ""}</div>
    </div>
  `;

  renderBarChart(document.getElementById("overview-rmse-chart"), lines, {
    valueKey: "rmse",
    invert: true,
    format: (v) => fmt(v, 2),
  });

  renderBarChart(
    document.getElementById("overview-roi-chart"),
    bestEvPerMarket(evRows).map((a) => ({ market: `${a.market} (${a.side})`, roi: a.roi })),
    { valueKey: "roi", format: (v) => fmtPct(v) },
  );
  renderHonestOos();
  renderOddsModelRoi();
}

function renderOddsModelRoi() {
  const card = document.getElementById("odds-model-roi-card");
  if (!card) return;
  if (!ODDS_MODEL_ROI?.closeAll) {
    card.hidden = true;
    return;
  }
  card.hidden = false;
  const note = document.getElementById("odds-model-roi-note");
  if (note) {
    const gen = ODDS_MODEL_ROI.generated_at
      ? new Date(ODDS_MODEL_ROI.generated_at).toLocaleString()
      : "";
    note.innerHTML =
      `Full-model walk-forward on <code>odds.csv</code> closing lines (178 event×round bundles). ` +
      `Regenerate: <code>npm run backtest:odds-model-roi</code>` +
      (gen ? ` · updated ${gen}` : "");
  }
  const rows = [
    { label: "All O/U @ close", row: ODDS_MODEL_ROI.closeAll },
    { label: "All O/U @ 5% EV", row: ODDS_MODEL_ROI.ev5All },
    { label: "Birdies @ close", row: ODDS_MODEL_ROI.closeBirdies },
    { label: "Birdies @ 5% EV", row: ODDS_MODEL_ROI.ev5Birdies },
    { label: "Total score @ close", row: ODDS_MODEL_ROI.closeScore },
    { label: "Total score @ 5% EV", row: ODDS_MODEL_ROI.ev5Score },
  ].filter((x) => x.row);
  document.getElementById("odds-model-roi-kpis").innerHTML = rows
    .map(({ label, row }) => {
      const roi = num(row.roi_close_pct);
      const bets = Math.round(num(row.bets));
      const units = num(row.units_close);
      return `<div class="kpi-card">
        <div class="kpi-label">${label}</div>
        <div class="kpi-value ${clsSigned(roi)}">${fmtPct(roi)}</div>
        <div class="kpi-sub">${bets} bets · ${units >= 0 ? "+" : ""}${fmt(units, 1)}u · ${fmt(num(row.hit_rate_pct), 1)}% hit</div>
      </div>`;
    })
    .join("");
}

function oddsCsvFilters() {
  return {
    course: state.oddsCourse,
    player: state.player,
    market: state.market,
    side: state.side,
    minEv: state.minEv,
    modelOnly: state.oddsModelOnly,
  };
}

function populateOddsCourseSelect() {
  const sel = document.getElementById("odds-filter-course");
  if (!sel) return;
  const prev = state.oddsCourse;
  const courses = uniqueCourses(ODDS_LINES_ROWS);
  sel.innerHTML =
    `<option value="">All courses (${courses.length})</option>` +
    courses.map((c) => `<option value="${esc(c)}">${esc(c)}</option>`).join("");
  sel.value = prev && courses.includes(prev) ? prev : "";
  state.oddsCourse = sel.value;
}

function renderOddsCsv() {
  const kpis = document.getElementById("odds-csv-kpis");
  const thead = document.getElementById("odds-csv-thead");
  const tbody = document.getElementById("odds-csv-tbody");
  const note = document.getElementById("odds-csv-note");
  if (!kpis || !thead || !tbody) return;

  if (!ODDS_LINES_ROWS.length) {
    if (note) {
      note.innerHTML =
        `No <code>odds_model_roi_lines.csv</code> loaded. Run <code>npm run backtest:odds-model-roi</code> then Reload data.`;
    }
    kpis.innerHTML = "";
    thead.innerHTML = "";
    tbody.innerHTML = `<tr><td colspan="16" class="muted">No odds.csv line data.</td></tr>`;
    return;
  }

  const gen = ODDS_LINES_ROWS[0]?.generated_at;
  if (note) {
    note.innerHTML =
      `Every graded Over/Under side from <code>odds.csv</code> (Birdies &amp; Total score). ` +
      `Walk-forward model μ vs DK odds. Toolbar Market, Min EV %, Bet side, and Player apply.` +
      (gen ? ` Updated ${new Date(gen).toLocaleString()}.` : "");
  }

  const filtered = filterOddsLines(ODDS_LINES_ROWS, oddsCsvFilters());
  const opts = { oddsAt: state.oddsAt };
  const sum = summarizeOddsLines(filtered, opts);
  const pnlKey = state.oddsAt === "open" ? "pnl_open" : "pnl_close";
  const oddsLabel = state.oddsAt === "open" ? "opening" : "closing";

  kpis.innerHTML = `
    <div class="kpi-card highlight">
      <div class="kpi-label">Lines</div>
      <div class="kpi-value">${filtered.length.toLocaleString()}</div>
      <div class="kpi-sub">${sum.bets.toLocaleString()} graded · flat 1u @ ${oddsLabel}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Hit rate</div>
      <div class="kpi-value">${oddsFmtNum(sum.hit_pct, 1)}%</div>
      <div class="kpi-sub">${sum.wins}W–${sum.losses}L–${sum.pushes}P</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">PnL</div>
      <div class="kpi-value ${clsSigned(sum.units)}">${sum.units >= 0 ? "+" : ""}${fmt(sum.units, 1)}u</div>
      <div class="kpi-sub">${oddsLabel} odds</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">ROI</div>
      <div class="kpi-value ${clsSigned(sum.roi_pct)}">${oddsFmtPct(sum.roi_pct)}</div>
      <div class="kpi-sub">per graded bet</div>
    </div>
  `;

  const aggHead = `<tr>
    <th>Name</th>
    <th class="num">Bets</th>
    <th class="num">W–L–P</th>
    <th class="num">Hit %</th>
    <th class="num">PnL</th>
    <th class="num">ROI</th>
  </tr>`;

  if (state.oddsView === "market") {
    const rows = summarizeByMarket(filtered, opts);
    thead.innerHTML = aggHead.replace("Name", "Market");
    tbody.innerHTML = rows.length
      ? rows
          .map(
            (r) => `<tr>
        <td>${esc(r.market)}</td>
        <td class="num">${r.bets}</td>
        <td class="num">${r.wins}–${r.losses}–${r.pushes}</td>
        <td class="num">${oddsFmtNum(r.hit_pct, 1)}%</td>
        <td class="num ${clsSigned(r.units)}">${r.units >= 0 ? "+" : ""}${fmt(r.units, 1)}u</td>
        <td class="num ${clsSigned(r.roi_pct)}">${oddsFmtPct(r.roi_pct)}</td>
      </tr>`,
          )
          .join("")
      : `<tr><td colspan="6" class="muted">No rows match filters.</td></tr>`;
    return;
  }

  if (state.oddsView === "course") {
    const rows = summarizeByCourse(filtered, opts);
    thead.innerHTML = aggHead.replace("Name", "Course");
    tbody.innerHTML = rows.length
      ? rows
          .map(
            (r) => `<tr>
        <td>${esc(r.course)}</td>
        <td class="num">${r.bets}</td>
        <td class="num">${r.wins}–${r.losses}–${r.pushes}</td>
        <td class="num">${oddsFmtNum(r.hit_pct, 1)}%</td>
        <td class="num ${clsSigned(r.units)}">${r.units >= 0 ? "+" : ""}${fmt(r.units, 1)}u</td>
        <td class="num ${clsSigned(r.roi_pct)}">${oddsFmtPct(r.roi_pct)}</td>
      </tr>`,
          )
          .join("")
      : `<tr><td colspan="6" class="muted">No rows match filters.</td></tr>`;
    return;
  }

  if (state.oddsView === "player") {
    const rows = summarizeByPlayer(filtered, opts).slice(0, 500);
    thead.innerHTML = aggHead.replace("Name", "Player");
    tbody.innerHTML = rows.length
      ? rows
          .map(
            (r) => `<tr>
        <td>${esc(r.player)}</td>
        <td class="num">${r.bets}</td>
        <td class="num">${r.wins}–${r.losses}–${r.pushes}</td>
        <td class="num">${oddsFmtNum(r.hit_pct, 1)}%</td>
        <td class="num ${clsSigned(r.units)}">${r.units >= 0 ? "+" : ""}${fmt(r.units, 1)}u</td>
        <td class="num ${clsSigned(r.roi_pct)}">${oddsFmtPct(r.roi_pct)}</td>
      </tr>`,
          )
          .join("")
      : `<tr><td colspan="6" class="muted">No rows match filters.</td></tr>`;
    return;
  }

  thead.innerHTML = `<tr>
    <th>Course</th>
    <th>Event</th>
    <th class="num">Rnd</th>
    <th>Player</th>
    <th>Market</th>
    <th class="num">Line</th>
    <th>Side</th>
    <th class="num">μ</th>
    <th class="num">Edge %</th>
    <th class="num">Actual</th>
    <th>Result</th>
    <th class="num">Open</th>
    <th class="num">Close</th>
    <th class="num">PnL</th>
  </tr>`;

  const sorted = [...filtered].sort((a, b) => {
    const ev = String(b.event || "").localeCompare(String(a.event || ""));
    if (ev) return ev;
    return num(a.round) - num(b.round);
  });

  tbody.innerHTML = sorted.length
    ? sorted
        .slice(0, 2000)
        .map((r) => {
          const pnl = num(r[pnlKey], NaN);
          const edge = num(r.model_edge_pct, NaN);
          const res = String(r.result || "").toUpperCase();
          const resCls = res === "W" ? "pos" : res === "L" ? "neg" : "";
          return `<tr>
        <td>${esc(r.course_name || "—")}</td>
        <td>${esc(r.event || "")}</td>
        <td class="num">${esc(r.round || "")}</td>
        <td>${esc(r.matched_player || r.player || "")}</td>
        <td>${esc(r.market || "")}</td>
        <td class="num">${fmt(num(r.line), r.market === "Total score" ? 1 : 1)}</td>
        <td>${String(r.side).toLowerCase() === "over" ? "Over" : "Under"}</td>
        <td class="num">${fmt(num(r.model_mu), 2)}</td>
        <td class="num ${clsSigned(edge)}">${Number.isFinite(edge) ? `${edge >= 0 ? "+" : ""}${edge.toFixed(1)}%` : "—"}</td>
        <td class="num">${fmt(num(r.actual), r.market === "Total score" ? 0 : 1)}</td>
        <td class="${resCls}">${res || "—"}</td>
        <td class="num">${esc(formatAmerican(num(r.opening_american, NaN)))}</td>
        <td class="num">${esc(formatAmerican(num(r.closing_american, NaN)))}</td>
        <td class="num ${clsSigned(pnl)}">${Number.isFinite(pnl) ? `${pnl >= 0 ? "+" : ""}${pnl.toFixed(2)}u` : "—"}</td>
      </tr>`;
        })
        .join("")
    : `<tr><td colspan="14" class="muted">No rows match filters — try Birdies or Total score market, or lower Min EV %.</td></tr>`;

  if (sorted.length > 2000) {
    tbody.innerHTML += `<tr><td colspan="14" class="muted">Showing first 2,000 of ${sorted.length.toLocaleString()} lines.</td></tr>`;
  }
}

function renderHonestOos() {
  const card = document.getElementById("oos-honest-card");
  const note = document.getElementById("oos-honest-note");
  if (!card) return;
  if (!OOS_REPORT?.combined_oos_at_5pct) {
    card.hidden = true;
    return;
  }
  card.hidden = false;
  const c5 = OOS_REPORT.combined_oos_at_5pct;
  const peak = OOS_REPORT.peak_oos_event_at_5pct;
  const worst = OOS_REPORT.worst_oos_event_at_5pct;
  const bestTh = OOS_REPORT.best_oos_threshold_calibrated;
  const raw5 = OOS_REPORT.combined_oos_raw_at_5pct;

  if (note) {
    note.innerHTML =
      `Walk-forward OOS across <strong>${OOS_REPORT.oos_event_count}</strong> completed events` +
      (OOS_REPORT.excluded_live_event ? ` (excludes live week: ${OOS_REPORT.excluded_live_event})` : "") +
      `. Calibration fit uses model−DK lines only — never bet results. ` +
      `Regenerate: <code>npm run report:walkforward-oos-roi</code>`;
  }

  document.getElementById("oos-honest-kpis").innerHTML = `
    <div class="kpi-card highlight">
      <div class="kpi-label">OOS ROI @ 5% EV</div>
      <div class="kpi-value ${clsSigned(c5.roi_pct)}">${fmtPct(c5.roi_pct)}</div>
      <div class="kpi-sub">${c5.bets} bets · ${fmt(c5.hit_pct, 1)}% hit · +${fmt(c5.units, 0)}u</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Peak event OOS</div>
      <div class="kpi-value ${clsSigned(peak?.roi_pct)}">${peak ? fmtPct(peak.roi_pct) : "—"}</div>
      <div class="kpi-sub">${peak?.event ? peak.event.replace(/ presented by.*/i, "") : ""}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Worst event OOS</div>
      <div class="kpi-value ${clsSigned(worst?.roi_pct)}">${worst ? fmtPct(worst.roi_pct) : "—"}</div>
      <div class="kpi-sub">${worst?.event ? worst.event.replace(/ presented by.*/i, "") : ""}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Raw model @ 5%</div>
      <div class="kpi-value ${clsSigned(raw5?.roi_pct)}">${raw5 ? fmtPct(raw5.roi_pct) : "—"}</div>
      <div class="kpi-sub">no book calibration</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Best threshold (exploratory)</div>
      <div class="kpi-value ${clsSigned(bestTh?.calibrated?.roi_pct)}">${bestTh ? fmtPct(bestTh.calibrated.roi_pct) : "—"}</div>
      <div class="kpi-sub">${bestTh ? `≥${bestTh.min_ev_pct}% EV · ${bestTh.calibrated.bets} bets` : ""}</div>
    </div>
  `;

  const markets = OOS_REPORT.by_market_at_5pct || [];
  document.querySelector("#oos-market-table tbody").innerHTML = markets.length
    ? markets
        .map(
          (m) => `<tr>
        <td>${m.market}</td>
        <td class="num ${clsSigned(m.roi_pct)}">${fmtPct(m.roi_pct)}</td>
        <td class="num">${m.bets}</td>
        <td class="num ${clsSigned(m.units)}">${m.units >= 0 ? "+" : ""}${fmt(m.units, 1)}u</td>
        <td class="num">${fmt(m.hit_pct, 1)}%</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="5">No OOS market rows</td></tr>`;

  const events = (OOS_REPORT.by_event || [])
    .filter((e) => e.at_5pct?.bets > 0)
    .sort((a, b) => num(b.at_5pct.roi_pct) - num(a.at_5pct.roi_pct));
  document.querySelector("#oos-event-table tbody").innerHTML = events.length
    ? events
        .map(
          (e) => `<tr>
        <td>${e.event}</td>
        <td class="num ${clsSigned(e.at_5pct.roi_pct)}">${fmtPct(e.at_5pct.roi_pct)}</td>
        <td class="num">${e.at_5pct.bets}</td>
        <td class="num ${clsSigned(e.at_5pct.units)}">${e.at_5pct.units >= 0 ? "+" : ""}${fmt(e.at_5pct.units, 1)}u</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="4">No OOS event rows</td></tr>`;
}

function renderAccuracy() {
  const showEvent = !state.tournament;
  document.getElementById("accuracy-col-event").hidden = !showEvent;
  const lines = lineRows();
  document.querySelector("#accuracy-table tbody").innerHTML = lines.length
    ? lines
        .map(
          (r) => `<tr>
        ${showEvent ? `<td>${r.event_name}</td>` : ""}
        <td>${r.market}</td>
        <td class="num">${fmt(num(r.rmse), 3)}</td>
        <td class="num">${fmt(num(r.mae), 3)}</td>
        <td class="num">${r.n_line_pairs}</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="${showEvent ? 5 : 4}">No line data — try a different tournament or market filter.</td></tr>`;
}

function renderEv() {
  const showEvent = !state.tournament;
  document.getElementById("ev-col-event").hidden = !showEvent;
  const rows = evRows();
  document.querySelector("#ev-table tbody").innerHTML = rows.length
    ? rows
        .map((r) => {
          const roi = num(r.roi_pct);
          const bets = num(r.bets);
          const winPct = bets > 0 ? (num(r.wins) / bets) * 100 : NaN;
          return `<tr>
        ${showEvent ? `<td>${r.event_name}</td>` : ""}
        <td>${r.market}</td>
        <td class="num">${fmt(num(r.ev_threshold_pct), 1)}</td>
        <td>${r.bet_side}</td>
        <td class="num">${r.bets}</td>
        <td class="num">${r.wins}–${r.losses}–${r.pushes}</td>
        <td class="num ${clsSigned(num(r.units_net))}">${num(r.units_net) >= 0 ? "+" : ""}${fmt(num(r.units_net), 2)}</td>
        <td class="num ${clsSigned(roi)}">${fmtPct(roi)}</td>
        <td class="num">${fmt(winPct, 1)}%</td>
      </tr>`;
        })
        .join("")
    : `<tr><td colspan="${showEvent ? 9 : 8}">No EV rows at ≥${state.minEv}% edge — lower min EV % to see more bets.</td></tr>`;

  const hm = heatmapData(
    activeRows().filter((r) => {
      if (r.section !== "ev_backtest_by_market") return false;
      if (!num(r.bets)) return false;
      if (state.market && r.market !== state.market) return false;
      return true;
    }),
  );
  const table = document.getElementById("ev-heatmap");
  if (!hm.markets.length || !hm.thresholds.length) {
    table.innerHTML = "<tbody><tr><td>No EV data for heatmap at this min edge.</td></tr></tbody>";
    return;
  }
  let html = "<thead><tr><th>Market</th>";
  for (const th of hm.thresholds) html += `<th class="num">${th}%</th>`;
  html += "</tr></thead><tbody>";
  for (const m of hm.markets) {
    html += `<tr><td>${m}</td>`;
    for (const th of hm.thresholds) {
      const roi = hm.cell.get(`${m}\x1f${th}`);
      html += `<td class="heat-cell num ${clsSigned(roi)}" style="background:${roiHeatColor(roi)}">${Number.isFinite(roi) ? fmtPct(roi) : "—"}</td>`;
    }
    html += "</tr>";
  }
  html += "</tbody>";
  table.innerHTML = html;
}

function renderEvents() {
  const grid = document.getElementById("event-grid");
  grid.innerHTML = allTournamentNames()
    .map((name) => {
      const rows = latestRowsForTournament(name);
      const ts = tournamentScoreLineStats(rows);
      const { units, bets, roi } = tournamentEvTotals(rows);
      const course = rows.find((r) => r.section === "model_vs_book")?.course_used || "";
      const exp = latestExportForTournament(name);
      const exportLabel = exp ? new Date(exp).toLocaleDateString() : "";
      const selected = state.tournament === name ? " selected" : "";
      return `<article class="event-card${selected}" data-tournament="${name.replace(/"/g, "&quot;")}">
        <h3>${name}</h3>
        <div class="course">${course}${exportLabel ? ` · ${exportLabel}` : ""}</div>
        <div class="event-metrics">
          <div><span>Score RMSE</span><strong>${fmt(ts.rmse, 2)}</strong></div>
          <div><span>Score MAE</span><strong>${fmt(ts.mae, 2)}</strong></div>
          <div><span>EV units (${state.minEv}%, book)</span><strong class="${clsSigned(units)}">${units >= 0 ? "+" : ""}${fmt(units, 1)}u</strong></div>
          <div><span>ROI (${state.minEv}%, book)</span><strong class="${clsSigned(roi)}">${fmtPct(roi)}</strong></div>
        </div>
      </article>`;
    })
    .join("");

  grid.querySelectorAll(".event-card").forEach((card) => {
    card.addEventListener("click", () => {
      selectTournament(card.getAttribute("data-tournament") || "");
      setTab("overview");
    });
  });
}

function buildInsights() {
  const lines = state.tournament ? lineRows() : overviewLineRows();
  const ev = evRowsAtMinEdge(undefined, { bettableOnly: false });
  const insights = [];

  const score = lines.find((r) => r.market === "Total score");
  if (score) {
    const rmse = num(score.rmse);
    const label = state.tournament || "Combined";
    if (rmse > 5) {
      insights.push({ tone: "warn", text: `${label}: total score RMSE ${fmt(rmse, 2)} vs book — check round-score calibration.` });
    } else {
      insights.push({ tone: "", text: `${label}: total score RMSE ${fmt(rmse, 2)} — lines track the book reasonably well.` });
    }
  }

  const agg = aggregateEvByMarketSide(ev);
  const profitable = agg.filter((a) => a.roi > 5 && a.bets >= 5).sort((a, b) => b.roi - a.roi);
  const losing = agg.filter((a) => a.roi < -5 && a.bets >= 5).sort((a, b) => a.roi - b.roi);
  if (profitable[0]) {
    const t = profitable[0];
    insights.push({ tone: "", text: `Best pocket: ${t.market} ${t.side} — ${fmtPct(t.roi)} ROI on ${t.bets} bets (+${fmt(t.units, 1)}u).` });
  }

  const fairStats = aggregateBeatFairStats(explodeDetailToBets(DETAIL_ROWS).filter((r) => {
    if (state.tournament && r.event_name !== state.tournament) return false;
    if (state.market && r.market !== state.market) return false;
    if (!r.qualified) return false;
    return true;
  }).filter((r) => {
    const th = num(r.pickEdge);
    return Number.isFinite(th) && th >= state.minEv;
  }));
  if (Number.isFinite(fairStats.beatFair) && fairStats.graded >= 10) {
    insights.push({
      tone: fairStats.beatFair >= 2 ? "" : fairStats.beatFair >= 0 ? "warn" : "bad",
      text: `Beat fair price (devigged, no margin): ${fmtPct(fairStats.beatFair)} — ${fmt(fairStats.hitRate, 1)}% hit vs ${fmt(fairStats.avgFair, 1)}% fair implied on ${fairStats.graded} graded picks (posted ${fmt(fairStats.avgPosted, 1)}%).`,
    });
  }
  if (losing[0]) {
    const t = losing[0];
    insights.push({ tone: "bad", text: `Weakest pocket: ${t.market} ${t.side} — ${fmtPct(t.roi)} on ${t.bets} bets.` });
  }

  if (!state.tournament) {
    insights.push({
      tone: "",
      text: `Viewing all ${allTournamentNames().length} tournaments combined (each event uses its latest export). Pick one from the Tournament dropdown for a single-event drill-down.`,
    });
  }

  const signalPool = explodeDetailToBets(DETAIL_ROWS).filter((r) => {
    if (state.tournament && r.event_name !== state.tournament) return false;
    if (state.market && r.market !== state.market) return false;
    if (state.side && r.pickSide !== state.side) return false;
    return r.qualified;
  });
  for (const si of buildEdgeSignalInsights(signalPool, DETAIL_ROWS)) {
    insights.push(si);
  }

  if (!insights.length) {
    insights.push({ tone: "warn", text: "No insights at current filters — try min EV 0% or pick a specific tournament." });
  }

  document.getElementById("insight-list").innerHTML = insights
    .map((i) => `<li class="${i.tone}">${i.text}</li>`)
    .join("");
}

function renderHeader() {
  const names = allTournamentNames();
  let sub = "";
  if (state.tournament) {
    const exp = latestExportForTournament(state.tournament);
    sub = exp ? `Data from ${new Date(exp).toLocaleString()}` : "";
  } else {
    sub = `${names.length} tournaments combined`;
  }
  document.getElementById("header-meta").innerHTML = `
    <div><strong>${state.tournament || "All tournaments"}</strong></div>
    <div>${sub}</div>
  `;
}

function populateTournamentSelect() {
  const sel = document.getElementById("filter-tournament");
  const names = allTournamentNames();
  sel.innerHTML =
    `<option value="">All tournaments (${names.length})</option>` +
    names.map((n) => `<option value="${n.replace(/"/g, "&quot;")}">${n}</option>`).join("");
  sel.value = state.tournament;
}

function populateMarketFilter() {
  const rows = state.tournament ? latestRowsForTournament(state.tournament) : combinedTournamentRows();
  const markets = uniqueSorted(rows.map((r) => r.market)).sort((a, b) => marketSortKey(a) - marketSortKey(b));
  const mSel = document.getElementById("filter-market");
  const prev = state.market;
  mSel.innerHTML =
    `<option value="">All markets</option>` + markets.map((m) => `<option value="${m}">${m}</option>`).join("");
  mSel.value = prev && markets.includes(prev) ? prev : markets.includes("Total score") ? "Total score" : "";
  state.market = mSel.value;
}

function selectTournament(name) {
  state.tournament = name;
  document.getElementById("filter-tournament").value = name;
  populateMarketFilter();
  renderAll();
}

function renderLivePicks() {
  const card = document.getElementById("live-picks-card");
  const tbody = document.getElementById("live-picks-tbody");
  const titleEl = document.getElementById("live-picks-title");
  const noteEl = document.getElementById("live-picks-note");
  if (!card || !tbody) return;

  if (!LIVE_CTX?.projections) {
    card.hidden = false;
    if (titleEl) titleEl.textContent = "Best bets — live week";
    if (noteEl) noteEl.textContent = "Could not load projections.json — run npm run refresh:live or npm start in alpha-caddie-web.";
    tbody.innerHTML = `<tr><td colspan="10" class="live-picks-empty">No live projections available.</td></tr>`;
    return;
  }

  const built = buildLiveBestBets({
    projections: LIVE_CTX.projections,
    oos: LIVE_CTX.oos || OOS_REPORT,
    signals: LIVE_CTX.signals,
    courseRow: LIVE_CTX.courseRow,
    minEvPct: state.minEv,
  });

  card.hidden = false;
  if (titleEl) {
    titleEl.textContent = `Best bets — ${built.roundLabel}${built.eventName ? ` · ${built.eventName}` : ""}`;
  }
  const oosRoi = num((LIVE_CTX.oos || OOS_REPORT)?.combined_oos_at_5pct?.roi_pct, NaN);
  const oosN = Math.round(num((LIVE_CTX.oos || OOS_REPORT)?.combined_oos_at_5pct?.bets, NaN)) || 0;
  if (noteEl) {
    const venue = built.venueNote ? ` ${built.venueNote}.` : "";
    const dkNote = built.modelLinesOnly
      ? " DraftKings scrape unavailable — showing model half-lines at −110 (not real +EV until DK posts)."
      : "";
    noteEl.textContent =
      `Upcoming round picks from projections.json (${built.updatedAt ? `updated ${new Date(built.updatedAt).toLocaleString()}` : "live"}).${venue}${dkNote}` +
      ` Ranked by model EV vs posted lines, walk-forward OOS market ROI` +
      (Number.isFinite(oosRoi) ? ` (+${oosRoi.toFixed(1)}% on ${oosN} OOS bets @ 5%)` : "") +
      `, and historical context signals. Uses toolbar Min EV %.`;
  }

  if (!built.picks.length) {
    tbody.innerHTML = `<tr><td colspan="10" class="live-picks-empty">No picks at ≥${state.minEv}% EV for ${built.roundLabel} — lower Min EV or refresh DK props.</td></tr>`;
    return;
  }

  tbody.innerHTML = built.picks
    .map((p, i) => {
      const hist =
        Number.isFinite(p.histRoi) && p.histBets > 0
          ? `${p.histRoi >= 0 ? "+" : ""}${p.histRoi.toFixed(1)}%`
          : "—";
      const edgeCls = p.edgePct > 0 ? "pos" : p.edgePct < 0 ? "neg" : "";
      const tags = (p.contextTags || [])
        .map((t) => {
          const warn = String(t).startsWith("fade") || String(t).includes("% -");
          return `<span class="live-picks-tag${warn ? " warn" : ""}">${esc(t)}</span>`;
        })
        .join("");
      return `<tr class="live-picks-row" data-player="${esc(p.player_name)}" data-market="${esc(p.market)}" data-side="${esc(p.side)}">
        <td class="num">${i + 1}</td>
        <td>${esc(p.player_name)}</td>
        <td>${esc(p.market)}</td>
        <td class="num">${p.side === "over" ? "Over" : "Under"}</td>
        <td class="num">${Number.isFinite(p.line) ? p.line : "—"}</td>
        <td class="num">${esc(formatAmerican(p.odds))}</td>
        <td class="num ${edgeCls}">${p.edgePct >= 0 ? "+" : ""}${p.edgePct.toFixed(1)}%</td>
        <td class="num" title="${p.histBets ? `${p.histBets} OOS bets` : ""}">${hist}</td>
        <td class="live-picks-context">${tags || '<span class="muted">—</span>'}</td>
        <td class="num">${p.score.toFixed(1)}</td>
      </tr>`;
    })
    .join("");
}

function esc(s) {
  return String(s ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function renderAll() {
  renderHeader();
  renderOverview();
  renderAccuracy();
  renderEv();
  renderOddsCsv();
  renderBets();
  renderRisk();
  renderEvents();
  buildInsights();
  renderLivePicks();
}

function setTab(name) {
  state.tab = name;
  document.querySelectorAll(".tab").forEach((t) => {
    t.classList.toggle("active", t.getAttribute("data-tab") === name);
  });
  document.querySelectorAll(".panel").forEach((p) => {
    p.classList.toggle("active", p.id === `panel-${name}`);
  });
  if (location.hash !== `#${name}`) {
    history.replaceState(null, "", `#${name}`);
  }
}

async function loadDetailCsvText() {
  /** @type {{ url: string, text: string, rows: number }[]} */
  const loaded = [];
  for (const url of DETAIL_CANDIDATES) {
    try {
      const res = await fetch(`${url}?t=${Date.now()}`, { cache: "no-store" });
      if (!res.ok) continue;
      const text = await res.text();
      const rows = text.split(/\r?\n/).filter(Boolean).length - 1;
      if (rows > 0) loaded.push({ url, text, rows });
    } catch {
      /* try next */
    }
  }
  if (!loaded.length) return "";
  loaded.sort((a, b) => b.rows - a.rows);
  const pick = loaded[0];
  if (pick.url.endsWith(".new")) {
    console.warn("[projection-tracker] Using .new detail CSV (main file locked in Excel?)");
  }
  return pick.text;
}

async function loadSummaryCsvText() {
  /** @type {{ url: string, text: string, rows: number }[]} */
  const loaded = [];
  for (const url of CSV_CANDIDATES) {
    try {
      const res = await fetch(`${url}?t=${Date.now()}`, { cache: "no-store" });
      if (!res.ok) continue;
      const text = await res.text();
      const rows = text.split(/\r?\n/).filter(Boolean).length - 1;
      if (rows > 0) loaded.push({ url, text, rows });
    } catch {
      /* try next */
    }
  }
  if (!loaded.length) {
    throw new Error("Could not load summary CSV (main or .new)");
  }
  loaded.sort((a, b) => b.rows - a.rows);
  const pick = loaded[0];
  if (pick.url.endsWith(".new")) {
    console.warn("[projection-tracker] Using .new summary (main file locked in Excel?)");
  }
  return pick.text;
}

async function loadOosReport() {
  try {
    const res = await fetch(`${OOS_JSON_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return null;
    return await res.json();
  } catch {
    return null;
  }
}

async function loadOddsModelRoi() {
  try {
    const res = await fetch(`${ODDS_ROI_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return null;
    const rows = parseCsv(await res.text());
    if (!rows.length) return null;
    const pick = (strategy, market) =>
      rows.find((r) => r.strategy === strategy && r.market === market) || null;
    return {
      generated_at: rows[0]?.generated_at || "",
      closeAll: pick("line_any", "__all__"),
      ev5All: pick("ev_5", "__all__"),
      closeBirdies: pick("line_any", "Birdies"),
      closeScore: pick("line_any", "Total score"),
      ev5Birdies: pick("ev_5", "Birdies"),
      ev5Score: pick("ev_5", "Total score"),
    };
  } catch {
    return null;
  }
}

async function loadOddsLinesCsv() {
  try {
    const res = await fetch(`${ODDS_LINES_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return [];
    return parseCsv(await res.text());
  } catch {
    return [];
  }
}

async function loadData() {
  const errEl = document.getElementById("error-banner");
  errEl.hidden = true;
  try {
    const [summaryText, detailText, oos, oddsRoi, oddsLines] = await Promise.all([
      loadSummaryCsvText(),
      loadDetailCsvText(),
      loadOosReport(),
      loadOddsModelRoi(),
      loadOddsLinesCsv(),
    ]);
    OOS_REPORT = oos;
    ODDS_MODEL_ROI = oddsRoi;
    ODDS_LINES_ROWS = oddsLines;
    ALL_ROWS = parseCsv(summaryText);
    if (!ALL_ROWS.length) throw new Error("Summary CSV is empty");
    DETAIL_ROWS = detailText ? parseCsv(detailText) : [];
    invalidateLiveBestBetsCache();
    LIVE_CTX = await loadLiveBestBetsContext();
    populateTournamentSelect();
    populateMarketFilter();
    populateOddsCourseSelect();
    renderAll();
  } catch (e) {
    errEl.hidden = false;
    if (location.protocol === "file:") {
      errEl.innerHTML = `<strong>Cannot load CSV.</strong> Run <code>npm run projection-tracker</code> then open <a href="http://localhost:5173/projection-tracker/">http://localhost:5173/projection-tracker/</a>`;
    } else {
      errEl.textContent = `Failed to load CSV: ${e?.message || e}. Run npm run export:round-projection-vs-actual first.`;
    }
  }
}

function bindUi() {
  document.querySelectorAll(".tab").forEach((btn) => {
    btn.addEventListener("click", () => setTab(btn.getAttribute("data-tab")));
  });
  document.getElementById("filter-tournament").addEventListener("change", (e) => {
    selectTournament(e.target.value);
  });
  document.getElementById("filter-market").addEventListener("change", (e) => {
    state.market = e.target.value;
    renderAll();
  });
  document.getElementById("filter-min-ev").addEventListener("change", (e) => {
    state.minEv = num(e.target.value) || 0;
    renderAll();
  });
  document.getElementById("filter-side").addEventListener("change", (e) => {
    state.side = e.target.value;
    renderAll();
  });
  document.getElementById("filter-player").addEventListener("input", (e) => {
    state.player = e.target.value.trim();
    renderBets();
    renderRisk();
    renderOddsCsv();
  });
  document.getElementById("filter-show").addEventListener("change", (e) => {
    state.show = e.target.value;
    renderBets();
  });
  for (const id of ["risk-bankroll", "risk-method", "risk-unit-pct", "risk-max-stake", "risk-round-cap"]) {
    const el = document.getElementById(id);
    if (!el) continue;
    el.addEventListener("input", syncRiskFromForm);
    el.addEventListener("change", syncRiskFromForm);
  }
  const oddsCourse = document.getElementById("odds-filter-course");
  if (oddsCourse) {
    oddsCourse.addEventListener("change", (e) => {
      state.oddsCourse = e.target.value;
      renderOddsCsv();
    });
  }
  const oddsAt = document.getElementById("odds-filter-at");
  if (oddsAt) {
    oddsAt.value = state.oddsAt;
    oddsAt.addEventListener("change", (e) => {
      state.oddsAt = e.target.value === "open" ? "open" : "close";
      renderOddsCsv();
    });
  }
  const oddsModelOnly = document.getElementById("odds-filter-model-only");
  if (oddsModelOnly) {
    oddsModelOnly.checked = state.oddsModelOnly;
    oddsModelOnly.addEventListener("change", (e) => {
      state.oddsModelOnly = e.target.checked;
      renderOddsCsv();
    });
  }
  const oddsView = document.getElementById("odds-filter-view");
  if (oddsView) {
    oddsView.value = state.oddsView;
    oddsView.addEventListener("change", (e) => {
      state.oddsView = e.target.value || "lines";
      renderOddsCsv();
    });
  }
  document.getElementById("btn-reload").addEventListener("click", loadData);
  document.getElementById("live-picks-tbody")?.addEventListener("click", (ev) => {
    const tr = ev.target.closest("tr.live-picks-row");
    if (!tr) return;
    const player = tr.dataset.player || "";
    const market = tr.dataset.market || "";
    const side = tr.dataset.side || "";
    const pIn = document.getElementById("filter-player");
    const mSel = document.getElementById("filter-market");
    const sSel = document.getElementById("filter-side");
    if (pIn && player) pIn.value = player;
    if (mSel && market) mSel.value = market;
    if (sSel && side) sSel.value = side;
    state.market = market;
    state.side = side;
    state.player = player;
    setTab("bets");
    renderAll();
  });
}

bindUi();
const initialTab = String(location.hash || "").replace(/^#/, "");
if (["overview", "accuracy", "ev", "odds-csv", "bets", "risk", "events", "insights", "picks", "guide"].includes(initialTab)) {
  setTab(initialTab);
}

document.querySelectorAll("#panel-guide a[href^='#']").forEach((a) => {
  a.addEventListener("click", (e) => {
    const tab = a.getAttribute("href")?.replace(/^#/, "");
    if (tab && tab !== "guide") {
      e.preventDefault();
      setTab(tab);
    }
  });
});
loadData();
