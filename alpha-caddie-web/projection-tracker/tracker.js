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
import { patchDetailRowsFromLiveSources } from "./live-detail-patch.mjs";
import { alignDetailCsvText } from "./detail-csv-align.mjs";
import {
  ouSideResults,
  parseDkBookLine,
  parsePpBookLine,
  fmtDkBookLine,
  fmtPpBookLine,
  TRACKER_OU_BOOKS,
  DETAIL_EXPORT_MARKETS,
} from "./detail-market-specs.mjs";
import {
  DEFAULT_MIN_EV_PCT,
  isActionableMarket,
  minEvForMarket,
  MIN_LINE_GAP_BY_MARKET,
  OOS_MARKET_POLICY,
  qualifiesBet,
} from "./bet-policy.mjs";
import { buildLiveBestBets, buildAllLiveDkBetOptions, loadLiveBestBetsContext, invalidateLiveBestBetsCache } from "./live-best-bets.mjs";
import { simulateMyBetsLedger, myBetsSummaryByMarket } from "./my-bets-journal.mjs";
import { autoGradeMyBets } from "./my-bets-grade.mjs";
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
import {
  loadWinProbCalibration,
  priceSidesAgainstBook,
  getWinProbCalibration,
} from "./win-prob-calibration.mjs";

const RISK_STORAGE_KEY = "alphaCaddie_projection_tracker_risk_v1";
const OVERVIEW_KELLY_STORAGE_KEY = "alphaCaddie_projection_tracker_overview_kelly_v1";
const MY_BETS_STORAGE_KEY = "alphaCaddie_my_dk_bets_v1";

const OVERVIEW_KELLY_METHODS = new Set(["kelly_unit_cap", "kelly_q", "kelly_half"]);

function loadOverviewKellyMethod() {
  try {
    const raw = localStorage.getItem(OVERVIEW_KELLY_STORAGE_KEY);
    if (OVERVIEW_KELLY_METHODS.has(raw)) return raw;
  } catch {
    /* ignore */
  }
  return "kelly_unit_cap";
}

function saveOverviewKellyMethod() {
  try {
    localStorage.setItem(OVERVIEW_KELLY_STORAGE_KEY, state.overviewKellyMethod);
  } catch {
    /* ignore */
  }
}

function overviewKellyMethodLabel(method) {
  if (method === "kelly_half") return "½ Kelly";
  if (method === "kelly_q") return "¼ Kelly";
  return "¼ Kelly +1%";
}

const CSV_CANDIDATES = [
  "../data/round_projection_vs_actual_summary.csv",
  "../data/round_projection_vs_actual_summary.csv.new",
];

const DETAIL_CANDIDATES = [
  "../data/round_projection_vs_actual.csv",
  "../data/round_projection_vs_actual.csv.new",
];

const MARKET_DECIMALS = {
  "Total score": 2,
  Birdies: 1,
  Bogeys: 1,
  GIR: 0,
  "Fairways hit": 0,
};

/** Close line/odds = most recent pre-tee audit; open = earliest. Pricing uses close. */
const MARKET_SPECS = DETAIL_EXPORT_MARKETS.map((m) => {
  const stem = m.key === "total" ? "round_score" : m.key === "fairways" ? "fairways" : m.key;
  return {
    ...m,
    modelCol: m.lineCol,
    bookCol: m.bookLineCol,
    overOdds: m.overOddsCol,
    underOdds: m.underOddsCol,
    overRes: `${stem}_over`,
    underRes: `${stem}_under`,
    actual: m.key === "total" ? "actual_round_score" : `actual_${m.key === "fairways" ? "fairways" : m.key}`,
    decimals: MARKET_DECIMALS[m.market] ?? 1,
  };
});

const MARKET_ORDER = [
  "Total score",
  "Birdies",
  "Bogeys",
  "GIR",
  "Fairways hit",
];

/** Markets with real closing lines in odds.csv backtest (actionable book). */
const BETTABLE_MARKETS = new Set([
  "GIR",
  "Total score",
  "Birdies",
  "Bogeys",
  "Fairways hit",
]);

function setOverviewHistoricalVisible(show) {
  for (const id of ["oos-honest-card", "odds-model-roi-card", "skill-window-card"]) {
    const el = document.getElementById(id);
    if (el) el.hidden = !show;
  }
  const panel = document.getElementById("panel-overview");
  if (!panel) return;
  for (const card of panel.querySelectorAll(".chart-card")) {
    const id = card.id || "";
    if (id === "oos-honest-card" || id === "odds-model-roi-card" || id === "skill-window-card") continue;
    card.hidden = !show;
  }
}

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
const SKILL_WINDOW_JSON_URL = "../data/skill_window_oos_roi.json";

/** @type {object | null} */
let SKILL_WINDOW_REPORT = null;

const state = {
  tab: "overview",
  /** "" = all tournaments combined; otherwise event name */
  tournament: "",
  market: "Total score",
  /** Best bets tab only — "" = all live markets */
  picksMarket: "",
  /** "" = all books; otherwise TRACKER_OU_BOOKS id */
  book: "",
  minEv: DEFAULT_MIN_EV_PCT,
  side: "",
  player: "",
  show: "bets",
  overviewKellyMethod: loadOverviewKellyMethod(),
  risk: loadRiskPrefs(),
  myBets: {
    ...loadMyBetsPrefs(),
    view: "browse",
    browseMarket: "",
    browseSearch: "",
  },
  oddsCourse: "",
  oddsAt: "close",
  oddsModelOnly: false,
  oddsView: "lines",
};

/** @type {Awaited<ReturnType<typeof loadLiveBestBetsContext>> | null} */
let LIVE_CTX = null;

/** @type {object[] | null} */
let EXPLODED_BET_ROWS = null;
/** @type {string | null} */
let EXPLODED_BET_ROWS_KEY = null;
let explodedBetRowsGen = 0;

function explodedBetRowsCacheKey() {
  return `${explodedBetRowsGen}|ev:${state.minEv}|show:${state.show}`;
}

function invalidateExplodedBetRows() {
  explodedBetRowsGen++;
  EXPLODED_BET_ROWS = null;
  EXPLODED_BET_ROWS_KEY = null;
}

function allExplodedBetRows() {
  const key = explodedBetRowsCacheKey();
  if (EXPLODED_BET_ROWS && EXPLODED_BET_ROWS_KEY === key) return EXPLODED_BET_ROWS;
  EXPLODED_BET_ROWS = explodeAllBookDetailToBets(DETAIL_ROWS);
  EXPLODED_BET_ROWS_KEY = key;
  return EXPLODED_BET_ROWS;
}

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

function loadMyBetsPrefs() {
  const defaults = { bankroll: 10000, bets: [] };
  try {
    const raw = localStorage.getItem(MY_BETS_STORAGE_KEY);
    if (!raw) return defaults;
    const j = JSON.parse(raw);
    const bets = Array.isArray(j.bets)
      ? j.bets.map((b) => ({
          id: String(b.id || crypto.randomUUID()),
          placedAt: String(b.placedAt || new Date().toISOString()),
          eventName: String(b.eventName || ""),
          round: Math.min(4, Math.max(1, num(b.round) || 1)),
          playerName: String(b.playerName || ""),
          opponentName: String(b.opponentName || ""),
          dg_id: num(b.dg_id),
          opponent_dg_id: num(b.opponent_dg_id),
          market: String(b.market || "Total score"),
          side: String(b.side || "over"),
          line: num(b.line),
          odds: num(b.odds),
          stake: Math.max(0, num(b.stake) || 0),
          lineKey: String(b.lineKey || ""),
          result: (() => {
            const raw = String(b.result || "open");
            if (raw.toLowerCase() === "open") return "open";
            return raw.toUpperCase();
          })(),
          notes: String(b.notes || ""),
        }))
      : [];
    return {
      bankroll: Math.max(100, num(j.bankroll) || defaults.bankroll),
      bets,
    };
  } catch {
    return defaults;
  }
}

function saveMyBetsPrefs() {
  try {
    localStorage.setItem(
      MY_BETS_STORAGE_KEY,
      JSON.stringify({
        bankroll: state.myBets.bankroll,
        bets: state.myBets.bets,
      }),
    );
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
  return allExplodedBetRows()
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

/** Overview Kelly sizing — match flat EV summary (DK) unless Book filter is set. */
function qualifiedBetRowsForOverviewKelly() {
  const bookId = state.book || "draftkings";
  return qualifiedBetRowsForRisk().filter((r) => r.bookId === bookId);
}

/**
 * Size qualified bets with Kelly (or flat) and return units + stake-weighted ROI.
 * Units = $ P/L ÷ starting 1-unit ($), matching Risk tab unit %.
 */
function overviewSizedStaking(method) {
  const risk = {
    ...state.risk,
    method: OVERVIEW_KELLY_METHODS.has(method) || method === "flat_fixed" || method === "flat_compound"
      ? method
      : "kelly_unit_cap",
  };
  const bets = qualifiedBetRowsForOverviewKelly();
  const sim = simulateBankrollHistory(bets, risk);
  const oneUnit = risk.bankroll * (risk.unitPct / 100);
  const units = oneUnit > 0 ? sim.pl / oneUnit : NaN;
  const roi = sim.totalStaked > 0 ? (sim.pl / sim.totalStaked) * 100 : NaN;

  /** @type {Map<string, { market: string, pl: number, staked: number, bets: number }>} */
  const byMarket = new Map();
  for (const e of sim.ledger) {
    const m = e.market || "—";
    let acc = byMarket.get(m);
    if (!acc) acc = { market: m, pl: 0, staked: 0, bets: 0 };
    acc.pl += num(e.pnl) || 0;
    acc.staked += num(e.stake) || 0;
    acc.bets += 1;
    byMarket.set(m, acc);
  }
  const marketRoi = sortMarkets(
    [...byMarket.values()].map((a) => ({
      market: a.market,
      units: oneUnit > 0 ? a.pl / oneUnit : NaN,
      roi: a.staked > 0 ? (a.pl / a.staked) * 100 : NaN,
      bets: a.bets,
    })),
  );

  return {
    units,
    roi,
    bets: sim.bets,
    totalStaked: sim.totalStaked,
    pl: sim.pl,
    marketRoi,
  };
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
  if (state.tab === "overview") renderOverview();
}

function myBetsPickLabel(bet) {
  const side = String(bet.side || "").toLowerCase() === "under" ? "Under" : "Over";
  const line = Number.isFinite(bet.line) ? ` ${bet.line}` : "";
  return `${side}${line}`;
}

function myBetsOptionPickLabel(opt) {
  const side = String(opt.side || "").toLowerCase() === "under" ? "Under" : "Over";
  return `${side} ${Number.isFinite(opt.line) ? opt.line : "—"}`;
}

function myBetsMatchesSearch(text, q) {
  if (!q) return true;
  return String(text || "").toLowerCase().includes(q);
}

function myBetsOptionMatchesSearch(opt, q) {
  if (!q) return true;
  const pick = myBetsOptionPickLabel(opt);
  return (
    myBetsMatchesSearch(opt.playerName, q) ||
    myBetsMatchesSearch(opt.market, q) ||
    myBetsMatchesSearch(pick, q) ||
    myBetsMatchesSearch(opt.side, q)
  );
}

function myBetsBetMatchesSearch(bet, q) {
  if (!q) return true;
  return (
    myBetsMatchesSearch(bet.playerName, q) ||
    myBetsMatchesSearch(bet.opponentName, q) ||
    myBetsMatchesSearch(bet.market, q) ||
    myBetsMatchesSearch(bet.eventName, q) ||
    myBetsMatchesSearch(myBetsPickLabel(bet), q)
  );
}

function myBetsAddButton(lineKey, inSlip, label = "Add") {
  if (inSlip.has(lineKey)) {
    return `<button type="button" class="btn btn-sm my-bets-remove-line" data-line-key="${esc(lineKey)}">Remove</button>`;
  }
  return `<button type="button" class="btn btn-sm my-bets-add-line" data-line-key="${esc(lineKey)}">${esc(label)}</button>`;
}

function renderMyBetsBrowseOptionRow(o, inSlip) {
  const added = inSlip.has(o.lineKey);
  const lineCell = Number.isFinite(o.line) ? o.line : "—";
  return `<tr class="my-bets-available-row${added ? " my-bets-in-slip" : ""}" data-line-key="${esc(o.lineKey)}">
    <td class="player-cell">${esc(o.playerName)}</td>
    <td>${esc(o.market)}</td>
    <td>${esc(myBetsOptionPickLabel(o))}</td>
    <td class="num">${lineCell}</td>
    <td class="num">${formatAmerican(o.odds)}</td>
    <td class="num">${myBetsAddButton(o.lineKey, inSlip)}</td>
  </tr>`;
}

function renderMyBetsBrowseTableHtml(options, inSlip) {
  return options.map((o) => renderMyBetsBrowseOptionRow(o, inSlip)).join("");
}

function myBetsLineKeyFromBet(bet) {
  if (bet.lineKey) return bet.lineKey;
  return `${bet.playerName}|${bet.market}|${bet.side}|${bet.line}`;
}

function myBetsSlipKeys() {
  return new Set(state.myBets.bets.map((b) => myBetsLineKeyFromBet(b)));
}

function setMyBetsView(view) {
  state.myBets.view = view === "slip" ? "slip" : "browse";
  document.querySelectorAll(".my-bets-subtab").forEach((btn) => {
    btn.classList.toggle("active", btn.getAttribute("data-my-bets-view") === state.myBets.view);
  });
  const browse = document.getElementById("my-bets-browse");
  const slip = document.getElementById("my-bets-slip");
  if (browse) browse.hidden = state.myBets.view !== "browse";
  if (slip) slip.hidden = state.myBets.view !== "slip";
}

function syncMyBetsBankrollFromForm() {
  state.myBets.bankroll = Math.max(100, num(document.getElementById("my-bets-bankroll")?.value) || 10000);
  saveMyBetsPrefs();
  renderMyBets();
}

/** @param {object} option */
function addOptionToMyBetsSlip(option) {
  const lineKey = option.lineKey || myBetsLineKeyFromBet(option);
  const exists = state.myBets.bets.find(
    (b) =>
      myBetsLineKeyFromBet(b) === lineKey &&
      b.eventName === option.eventName &&
      num(b.round) === num(option.round),
  );
  if (!exists) {
    state.myBets.bets.push({
      id: crypto.randomUUID(),
      placedAt: new Date().toISOString(),
      lineKey,
      eventName: option.eventName || "",
      round: num(option.round) || 1,
      playerName: option.playerName || "",
      opponentName: option.opponentName || "",
      dg_id: num(option.dg_id),
      opponent_dg_id: num(option.opponent_dg_id),
      market: option.market || "",
      side: option.side || "",
      line: num(option.line),
      odds: num(option.odds),
      stake: 0,
      result: "open",
      notes: "",
    });
    saveMyBetsPrefs();
  }
  setTab("my-bets");
  setMyBetsView("slip");
  renderMyBets();
}

/** @param {object} pick */
function prefillMyBetsFromPick(pick) {
  const dg = pick.dg_id || pick.playerName;
  addOptionToMyBetsSlip({
    lineKey: `${dg}|${pick.market}|${pick.side}`,
    eventName: pick.eventName || "",
    round: pick.round || 1,
    playerName: pick.player_name || pick.playerName || "",
    dg_id: num(pick.dg_id),
    market: pick.market || "",
    side: pick.side || "",
    line: num(pick.line),
    odds: num(pick.odds),
  });
}

function updateMyBetStake(id, stake) {
  const bet = state.myBets.bets.find((b) => b.id === id);
  if (!bet) return;
  bet.stake = Math.max(0, num(stake) || 0);
  saveMyBetsPrefs();
  renderMyBets();
}

function updateMyBetResult(id, result) {
  const bet = state.myBets.bets.find((b) => b.id === id);
  if (!bet) return;
  bet.result = result === "open" ? "open" : String(result).toUpperCase();
  delete bet.autoGradedAt;
  saveMyBetsPrefs();
  renderMyBets();
}

function deleteMyBet(id) {
  state.myBets.bets = state.myBets.bets.filter((b) => b.id !== id);
  saveMyBetsPrefs();
  renderMyBets();
}

function removeMyBetByLineKey(lineKey) {
  if (!lineKey) return;
  const before = state.myBets.bets.length;
  state.myBets.bets = state.myBets.bets.filter((b) => {
    const key = b.lineKey || myBetsLineKeyFromBet(b);
    return key !== lineKey;
  });
  if (state.myBets.bets.length !== before) {
    saveMyBetsPrefs();
    renderMyBets();
  }
}

function gradeMyBetsFromLoadedData() {
  const changed = autoGradeMyBets(state.myBets.bets, {
    detailRows: DETAIL_ROWS,
    marketSpecs: MARKET_SPECS,
  });
  if (changed > 0) saveMyBetsPrefs();
  return changed;
}

function renderMyBetsBrowse() {
  const title = document.getElementById("my-bets-browse-title");
  const note = document.getElementById("my-bets-browse-note");
  const tbody = document.getElementById("my-bets-available-tbody");
  const marketSel = document.getElementById("my-bets-browse-market");
  if (!tbody) return;

  if (marketSel && marketSel.value !== state.myBets.browseMarket) {
    marketSel.value = state.myBets.browseMarket;
  }
  const playerIn = document.getElementById("my-bets-search");
  if (playerIn && playerIn.value !== state.myBets.browseSearch) {
    playerIn.value = state.myBets.browseSearch;
  }

  if (!LIVE_CTX?.projections) {
    if (title) title.textContent = "Available lines";
    if (note) note.textContent = "Could not load projections.json — run npm run refresh:live.";
    tbody.innerHTML = `<tr><td colspan="6">No live lines available.</td></tr>`;
    return;
  }

  const built = buildAllLiveDkBetOptions(LIVE_CTX.projections);
  if (title) {
    title.textContent = `Available lines — ${built.roundLabel}${built.eventName ? ` · ${built.eventName}` : ""}`;
  }
  if (note) {
    note.textContent = built.modelLinesOnly
      ? "DraftKings scrape unavailable — showing model half-lines only."
      : `${built.options.length} DraftKings sides · click Add to move to your slip`;
  }

  let options = built.options;
  if (state.myBets.browseMarket) {
    options = options.filter((o) => o.market === state.myBets.browseMarket);
  }
  if (state.myBets.browseSearch) {
    const q = state.myBets.browseSearch.toLowerCase();
    options = options.filter((o) => myBetsOptionMatchesSearch(o, q));
  }

  const inSlip = myBetsSlipKeys();
  if (!options.length) {
    tbody.innerHTML = `<tr><td colspan="6">No lines match filters.</td></tr>`;
    return;
  }

  tbody.innerHTML = renderMyBetsBrowseTableHtml(options, inSlip);

}

function addMyBetsLineByKey(lineKey) {
  if (!LIVE_CTX?.projections || !lineKey) return;
  const opt = buildAllLiveDkBetOptions(LIVE_CTX.projections).options.find((o) => o.lineKey === lineKey);
  if (opt) addOptionToMyBetsSlip(opt);
}

function renderMyBetsSlip() {
  const tbody = document.getElementById("my-bets-slip-tbody");
  const badge = document.getElementById("my-bets-slip-count");
  if (badge) badge.textContent = String(state.myBets.bets.length);

  const sim = simulateMyBetsLedger(state.myBets.bets, state.myBets.bankroll);
  const byId = new Map(sim.ledger.map((r) => [r.id, r]));

  if (!tbody) return;
  const rows = [...state.myBets.bets].reverse();
  if (!rows.length) {
    tbody.innerHTML = `<tr><td colspan="10">No bets on your slip — pick lines from Available lines.</td></tr>`;
    return;
  }

  tbody.innerHTML = rows
    .map((bet) => {
      const ledger = byId.get(bet.id);
      const status = ledger?.status || "draft";
      const pnlCell =
        status === "draft"
          ? `<span class="muted">—</span>`
          : status === "open"
            ? `<span class="muted">pending</span>`
            : `<span class="num ${clsSigned(ledger.pnl)}">${ledger.pnl >= 0 ? "+" : ""}${fmtUsdPrecise(ledger.pnl)}</span>`;
      const resVal = String(bet.result || "open").toUpperCase() === "OPEN" ? "open" : String(bet.result || "open").toUpperCase();
      const stakeVal = Number.isFinite(bet.stake) && bet.stake > 0 ? bet.stake : "";
      const autoNote = bet.autoGradedAt ? ' title="Auto-graded from refreshed results"' : "";
      return `<tr data-bet-id="${esc(bet.id)}">
        <td class="player-cell">${esc(bet.eventName || "—")}</td>
        <td class="num">${bet.round}</td>
        <td class="player-cell">${esc(bet.playerName)}</td>
        <td>${esc(bet.market)}</td>
        <td>${esc(myBetsPickLabel(bet))}</td>
        <td class="num">${formatAmerican(bet.odds)}</td>
        <td class="num"><input type="number" class="my-bets-stake-input" data-id="${esc(bet.id)}" min="0" step="1" placeholder="0" value="${stakeVal}" /></td>
        <td${autoNote}>
          <select class="my-bets-result-select" data-id="${esc(bet.id)}" aria-label="Result">
            <option value="open"${resVal === "open" ? " selected" : ""}>Open</option>
            <option value="W"${resVal === "W" ? " selected" : ""}>W</option>
            <option value="L"${resVal === "L" ? " selected" : ""}>L</option>
            <option value="P"${resVal === "P" ? " selected" : ""}>P</option>
          </select>
        </td>
        <td class="num">${pnlCell}</td>
        <td class="num"><button type="button" class="btn btn-sm my-bets-del" data-id="${esc(bet.id)}" title="Remove from slip">Remove</button></td>
      </tr>`;
    })
    .join("");
}

function renderMyBets() {
  const bankrollEl = document.getElementById("my-bets-bankroll");
  if (bankrollEl) bankrollEl.value = String(state.myBets.bankroll);
  setMyBetsView(state.myBets.view);
  renderMyBetsBrowse();
  renderMyBetsSlip();

  const sim = simulateMyBetsLedger(state.myBets.bets, state.myBets.bankroll);
  const activeN = state.myBets.bets.filter((b) => (num(b.stake) || 0) > 0).length;

  document.getElementById("my-bets-kpis").innerHTML = `
    <div class="kpi-card">
      <div class="kpi-label">Starting bankroll</div>
      <div class="kpi-value">${fmtUsd(sim.B0)}</div>
      <div class="kpi-sub">${state.myBets.bets.length} on slip · ${activeN} staked</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Bankroll now</div>
      <div class="kpi-value ${clsSigned(sim.pl)}">${fmtUsd(sim.ending)}</div>
      <div class="kpi-sub">${sim.pl >= 0 ? "+" : ""}${fmtUsdPrecise(sim.pl)} settled P/L</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">ROI</div>
      <div class="kpi-value ${clsSigned(sim.roi)}">${fmtPct(sim.roi)}</div>
      <div class="kpi-sub">${fmtPct(sim.roiOnStaked)} on ${fmtUsd(sim.totalStaked)} staked</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Max drawdown</div>
      <div class="kpi-value neg">${fmtUsd(sim.maxDd)}</div>
      <div class="kpi-sub">${fmt(sim.maxDdPct, 1)}% from peak ${fmtUsd(sim.peak)}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Avg stake</div>
      <div class="kpi-value">${fmtUsdPrecise(sim.avgStake)}</div>
      <div class="kpi-sub">${fmtUsd(sim.totalStaked)} total risked (settled)</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Record</div>
      <div class="kpi-value">${sim.wins}-${sim.losses}-${sim.pushes}</div>
      <div class="kpi-sub">${Number.isFinite(sim.hitPct) ? `${fmt(sim.hitPct, 1)}% hit` : "—"} · ${fmtUsd(sim.openStake)} open</div>
    </div>
  `;

  renderBankrollChart(document.getElementById("my-bets-chart"), sim.series, sim.B0);

  const byMarket = myBetsSummaryByMarket(sim.ledger);
  document.querySelector("#my-bets-market-table tbody").innerHTML = byMarket.length
    ? byMarket
        .map(
          (g) => `<tr>
        <td>${esc(g.market)}</td>
        <td class="num">${g.bets}</td>
        <td class="num">${Number.isFinite(g.hitPct) ? `${fmt(g.hitPct, 1)}%` : "—"}</td>
        <td class="num ${clsSigned(g.roi)}">${fmtPct(g.roi)}</td>
        <td class="num">${fmtUsdPrecise(g.avgStake)}</td>
        <td class="num ${clsSigned(g.totalPnl)}">${g.totalPnl >= 0 ? "+" : ""}${fmtUsdPrecise(g.totalPnl)}</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="6">No settled staked bets yet.</td></tr>`;

  renderMyBetsHistory();
}

function clearAllMyBets() {
  if (!state.myBets.bets.length) return;
  if (!window.confirm("Remove all bets from your journal? This cannot be undone.")) return;
  state.myBets.bets = [];
  saveMyBetsPrefs();
  renderMyBets();
}

function renderMyBetsHistory() {
  const tbody = document.querySelector("#my-bets-history-table tbody");
  if (!tbody) return;

  const sim = simulateMyBetsLedger(state.myBets.bets, state.myBets.bankroll);
  const byId = new Map(sim.ledger.map((r) => [r.id, r]));
  const rows = [...state.myBets.bets].reverse();

  if (!rows.length) {
    tbody.innerHTML = `<tr><td colspan="10">No logged bets.</td></tr>`;
    return;
  }

  tbody.innerHTML = rows
    .map((bet) => {
      const ledger = byId.get(bet.id);
      const status = ledger?.status || "draft";
      const resVal = String(bet.result || "open").toUpperCase() === "OPEN" ? "open" : String(bet.result || "open").toUpperCase();
      const pnlCell =
        status === "draft" || (num(bet.stake) || 0) <= 0
          ? `<span class="muted">—</span>`
          : status === "open"
            ? `<span class="muted">pending</span>`
            : `<span class="num ${clsSigned(ledger.pnl)}">${ledger.pnl >= 0 ? "+" : ""}${fmtUsdPrecise(ledger.pnl)}</span>`;
      return `<tr data-bet-id="${esc(bet.id)}">
        <td class="player-cell">${esc(bet.eventName || "—")}</td>
        <td class="num">${bet.round}</td>
        <td class="player-cell">${esc(bet.playerName)}</td>
        <td>${esc(bet.market)}</td>
        <td>${esc(myBetsPickLabel(bet))}</td>
        <td class="num">${formatAmerican(bet.odds)}</td>
        <td class="num">${(num(bet.stake) || 0) > 0 ? fmtUsdPrecise(bet.stake) : "—"}</td>
        <td>${resultBadge(resVal === "open" ? "" : resVal)}</td>
        <td class="num">${pnlCell}</td>
        <td class="num"><button type="button" class="btn btn-sm my-bets-del" data-id="${esc(bet.id)}">Remove</button></td>
      </tr>`;
    })
    .join("");
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
  return explodeAllBookDetailToBets(rows).filter((r) => r.bookId === "draftkings");
}

function explodeProjectionActualToBets(rows) {
  /** @type {object[]} */
  const out = [];
  for (const row of rows) {
    if (row.pricing_mode !== "default" || row.pricing_skill !== "default") continue;
    for (const spec of MARKET_SPECS) {
      let hasAnyBook = false;
      for (const book of TRACKER_OU_BOOKS) {
        const col = spec[book.lineKey];
        if (col && Number.isFinite(parseLine(row[col]))) {
          hasAnyBook = true;
          break;
        }
      }
      if (hasAnyBook) continue;
      const actual = parseLine(row[spec.actual]);
      const modelLine = parseLine(row[spec.modelCol]);
      if (!Number.isFinite(actual) || !Number.isFinite(modelLine)) continue;
      if (!isActionableMarket(spec.market)) continue;
      out.push({
        event_name: row.event_name,
        round: row.round,
        dg_id: row.dg_id,
        player_name: row.player_name,
        market: spec.market,
        book: "Model vs actual",
        bookId: "model",
        modelLine,
        bookLine: NaN,
        diff: modelLine - actual,
        overOdds: NaN,
        underOdds: NaN,
        overRes: row[spec.overRes],
        underRes: row[spec.underRes],
        actual,
        edgeOver: NaN,
        edgeUnder: NaN,
        edgeFairOver: NaN,
        edgeFairUnder: NaN,
        fairOver: NaN,
        fairUnder: NaN,
        pModelOver: NaN,
        pModelUnder: NaN,
        pickSide: null,
        pickEdge: NaN,
        edgeFairPick: NaN,
        modelProb: NaN,
        rawModelProb: NaN,
        fairProb: NaN,
        postedProb: NaN,
        beatsFairPreBet: null,
        qualified: false,
        betRes: "",
        betOdds: NaN,
        betDec: NaN,
        exported_at: row.exported_at,
        pnl: NaN,
        decimals: spec.decimals,
        projectionActual: true,
      });
    }
  }
  return out;
}

function explodePpDetailToBets(rows) {
  return explodeAllBookDetailToBets(rows).filter((r) => r.bookId === "prizepicks");
}

function explodeAllBookDetailToBets(rows) {
  /** @type {object[]} */
  const out = [];
  for (const row of rows) {
    if (row.pricing_mode !== "default" || row.pricing_skill !== "default") continue;
    for (const book of TRACKER_OU_BOOKS) {
      const src = String(row[book.sourceCol] || "").trim();
      if (!book.acceptSources.includes(src)) continue;
      const isLive = src !== "pre_round_audit";
      for (const spec of MARKET_SPECS) {
        out.push(
          ...explodeDetailBetForBook(row, spec, {
            bookCol: spec[book.lineKey],
            overOddsCol: spec[book.overKey],
            underOddsCol: spec[book.underKey],
            openLineCol: spec[book.openLineKey],
            openOverOddsCol: spec[book.openOverKey],
            openUnderOddsCol: spec[book.openUnderKey],
            openAt: String(row[book.openAtCol] || "").trim(),
            closeAt: String(row[book.closeAtCol] || "").trim(),
            bookId: book.id,
            bookLabel: isLive ? book.liveLabel : book.label,
            wholeLine: book.wholeLine,
            isPrizePicks: book.wholeLine,
          }),
        );
      }
    }
  }
  return out;
}

function explodeDetailBetForBook(row, spec, book) {
  // Close = most recent pre-tee capture (primary pricing / grading line).
  const rawLine = parseLine(row[book.bookCol]);
  const bookLine = book.wholeLine || book.isPrizePicks ? parsePpBookLine(rawLine) : parseDkBookLine(rawLine);
  if (!Number.isFinite(bookLine)) return [];
  const modelLine = parseLine(row[spec.modelCol]);
  const overOdds = nNum(row[book.overOddsCol], NaN);
  const underOdds = nNum(row[book.underOddsCol], NaN);
  const openRaw = book.openLineCol ? parseLine(row[book.openLineCol]) : NaN;
  const openLine =
    Number.isFinite(openRaw)
      ? book.wholeLine || book.isPrizePicks
        ? parsePpBookLine(openRaw)
        : parseDkBookLine(openRaw)
      : NaN;
  const openOverOdds = book.openOverOddsCol ? nNum(row[book.openOverOddsCol], NaN) : NaN;
  const openUnderOdds = book.openUnderOddsCol ? nNum(row[book.openUnderOddsCol], NaN) : NaN;
  const actual = parseLine(row[spec.actual]);
  const mu = Number.isFinite(modelLine) ? modelLine : NaN;
  if (!isActionableMarket(spec.market)) return [];
  const fair = modelEdgeVsFairAtLine(spec.market, mu, bookLine, overOdds, underOdds);
  const postedOver = impliedProbFromAmerican(overOdds);
  const postedUnder = impliedProbFromAmerican(underOdds);
  const priced = priceSidesAgainstBook({
    market: spec.market,
    pRawOver: fair.pOver,
    fairOver: fair.fairOver,
    fairUnder: fair.fairUnder,
    postedOver,
    postedUnder,
  });
  // Confidence edge (calibrated P − fair) is the primary price signal.
  let edgeOver = priced.confEdgeOver;
  let edgeUnder = priced.confEdgeUnder;
  if (!Number.isFinite(edgeOver) || !Number.isFinite(edgeUnder)) {
    edgeOver = fair.edgeFairOver;
    edgeUnder = fair.edgeFairUnder;
  }
  if (!Number.isFinite(edgeOver) || !Number.isFinite(edgeUnder)) {
    const posted = modelEdgePctAtLine(spec.market, mu, bookLine, overOdds, underOdds);
    edgeOver = posted.edgeOver;
    edgeUnder = posted.edgeUnder;
  }
  ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, bookLine));
  const pModelOver = priced.pCalOver;
  const pModelUnder = priced.pCalUnder;
  const pick = pickBetSide(edgeOver, edgeUnder, state.minEv, mu, bookLine);
  const bestSide =
    Number.isFinite(edgeOver) && Number.isFinite(edgeUnder)
      ? edgeOver >= edgeUnder
        ? { side: "over", edge: edgeOver }
        : { side: "under", edge: edgeUnder }
      : null;
  const activePick = pick || (state.show === "all" ? bestSide : null);
  const side = activePick?.side || null;
  const graded =
    Number.isFinite(actual) && Number.isFinite(bookLine)
      ? ouSideResults(actual, bookLine)
      : { over: row[spec.overRes], under: row[spec.underRes] };
  const overRes = graded.over;
  const underRes = graded.under;
  const betRes = side === "over" ? overRes : side === "under" ? underRes : "";
  const betOdds = side === "over" ? overOdds : side === "under" ? underOdds : NaN;
  const betOpenOdds = side === "over" ? openOverOdds : side === "under" ? openUnderOdds : NaN;
  const fairProb = side === "over" ? priced.fairOver : side === "under" ? priced.fairUnder : NaN;
  const postedProb = side === "over" ? postedOver : side === "under" ? postedUnder : NaN;
  const modelProb = side === "over" ? pModelOver : side === "under" ? pModelUnder : NaN;
  const rawModelProb =
    side === "over" ? priced.pRawOver : side === "under" ? priced.pRawUnder : NaN;
  const edgeFairPick = side === "over" ? priced.confEdgeOver : side === "under" ? priced.confEdgeUnder : NaN;
  const qualified =
    Boolean(pick) &&
    qualifiesBet({
      market: spec.market,
      modelLine,
      bookLine,
      context: {
        gir_minus_fw: nNum(row.gir_minus_fw, NaN),
        course_fw_width: nNum(row.course_fw_width, NaN),
        round: Math.round(nNum(row.round, NaN)),
      },
      eventName: row.event_name,
      side: pick?.side || null,
    });
  return [
    {
      event_name: row.event_name,
      round: row.round,
      dg_id: row.dg_id,
      player_name: row.player_name,
      market: spec.market,
      book: book.bookLabel,
      bookId: book.bookId || "",
      modelLine,
      bookLine,
      openLine,
      diff: Number.isFinite(modelLine) ? modelLine - bookLine : NaN,
      overOdds,
      underOdds,
      openOverOdds,
      openUnderOdds,
      openAt: book.openAt || "",
      closeAt: book.closeAt || "",
      overRes,
      underRes,
      actual,
      edgeOver,
      edgeUnder,
      edgeFairOver: priced.confEdgeOver,
      edgeFairUnder: priced.confEdgeUnder,
      fairOver: priced.fairOver,
      fairUnder: priced.fairUnder,
      pModelOver,
      pModelUnder,
      pRawOver: priced.pRawOver,
      pRawUnder: priced.pRawUnder,
      pickSide: side,
      pickEdge: activePick?.edge ?? NaN,
      edgeFairPick,
      modelProb,
      rawModelProb,
      fairProb,
      postedProb,
      beatsFairPreBet:
        qualified && Number.isFinite(modelProb) && Number.isFinite(fairProb) ? modelProb > fairProb : null,
      qualified,
      betRes,
      betOdds,
      betOpenOdds,
      betDec: americanToDecimal(betOdds),
      exported_at: row.exported_at,
      pnl: qualified && side ? pnlForResult(String(betRes).trim().toUpperCase(), betOdds) : NaN,
      decimals: spec.decimals,
      isPrizePicks: Boolean(book.wholeLine || book.isPrizePicks),
    },
  ];
}

function activeBetRows() {
  let rows = allExplodedBetRows();
  if (state.tournament) rows = rows.filter((r) => r.event_name === state.tournament);
  if (state.book) rows = rows.filter((r) => r.bookId === state.book);
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
    const bk = String(a.book).localeCompare(String(b.book));
    if (bk) return bk;
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
      <div class="kpi-sub">${state.show === "bets" ? `conf edge ≥${state.minEv}%` : "all graded lines"}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Beat-book bets</div>
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
      <div class="kpi-sub">beat-book bets only</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Hit vs fair</div>
      <div class="kpi-value ${clsSigned(fairStats.beatFair)}">${fmtPct(fairStats.beatFair)}</div>
      <div class="kpi-sub">${fmt(fairStats.hitRate, 1)}% hit vs ${fmt(fairStats.avgFair, 1)}% fair (${fairStats.graded} graded)</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">DK margin</div>
      <div class="kpi-value">${fmt(Number.isFinite(fairStats.avgPosted) && Number.isFinite(fairStats.avgFair) ? fairStats.avgPosted - fairStats.avgFair : NaN, 1)}%</div>
      <div class="kpi-sub">posted − fair implied on picks</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Conf &gt; fair (pre-bet)</div>
      <div class="kpi-value">${fmt(fairStats.preBetPct, 1)}%</div>
      <div class="kpi-sub">${fairStats.preBetBeats}/${fairStats.preBetEligible} beat-book picks</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Lines conf beats fair</div>
      <div class="kpi-value">${fmt(fairStats.modelBeatsFairPct, 1)}%</div>
      <div class="kpi-sub">${fairStats.modelBeatsFairLine}/${fairStats.withModel} with calibrated price + DK odds</div>
    </div>
  `;

  const fmtLine = (v, d) => (Number.isFinite(v) ? fmt(v, d) : "—");
  const fmtProb = (v) => (Number.isFinite(v) ? `${fmt(v * 100, 1)}%` : "—");
  const fmtBookLineCell = (r, lineVal) => {
    if (!Number.isFinite(lineVal)) return "—";
    const isWhole = Boolean(r.isPrizePicks);
    const formatted = isWhole ? fmtPpBookLine(r.market, lineVal) : fmtDkBookLine(r.market, lineVal);
    const prefix =
      r.bookId === "prizepicks"
        ? "PP"
        : r.bookId === "sleeper"
          ? "SL"
          : r.bookId === "underdog"
            ? "UD"
            : r.bookId === "fanduel"
              ? "FD"
              : r.bookId === "caesars"
                ? "CZR"
                : r.bookId === "kalshi"
                  ? "KL"
                  : r.bookId === "draftkings"
                    ? "DK"
                    : String(r.book || "").slice(0, 3).toUpperCase() || "BK";
    return `${prefix} ${formatted}`;
  };
  document.querySelector("#bets-table tbody").innerHTML = rows.length
    ? rows
        .map((r) => {
          const pickCls = r.qualified ? "pick-qualified" : "pick-muted";
          const pickLabel = r.pickSide ? `<span class="${pickCls}">${r.pickSide}</span>` : "—";
          const modelCell = fmtLine(r.modelLine, r.decimals);
          const openLineCell = fmtBookLineCell(r, r.openLine);
          const closeLineCell = fmtBookLineCell(r, r.bookLine);
          const betCell = r.qualified ? resultBadge(r.betRes) : "—";
          const pnlCell = r.qualified && Number.isFinite(r.pnl)
            ? `<span class="${clsSigned(r.pnl)}">${r.pnl >= 0 ? "+" : ""}${fmt(r.pnl, 2)}</span>`
            : "—";
          const tsTitle =
            r.openAt || r.closeAt
              ? `title="Open ${r.openAt || "—"} · Close ${r.closeAt || "—"}"`
              : "";
          return `<tr ${tsTitle}>
        ${showEvent ? `<td>${r.event_name}</td>` : ""}
        <td class="num">${r.round}</td>
        <td class="player-cell">${r.player_name}</td>
        <td>${r.market}</td>
        <td>${r.book || "—"}</td>
        <td class="num line-model">${modelCell}</td>
        <td class="num line-book">${openLineCell}</td>
        <td class="num line-book">${closeLineCell}</td>
        <td class="num ${clsSigned(-r.diff)}">${Number.isFinite(r.diff) ? (r.diff > 0 ? "+" : "") + fmt(r.diff, r.decimals) : "—"}</td>
        <td class="num">${formatAmerican(r.openOverOdds) || "—"}</td>
        <td class="num">${formatAmerican(r.overOdds) || "—"}</td>
        <td>${resultBadge(r.overRes)}</td>
        <td class="num">${formatAmerican(r.openUnderOdds) || "—"}</td>
        <td class="num">${formatAmerican(r.underOdds) || "—"}</td>
        <td>${resultBadge(r.underRes)}</td>
        <td>${pickLabel}</td>
        <td class="num" title="Calibrated P(pick wins)">${fmtProb(r.modelProb)}</td>
        <td class="num" title="Book fair (devigged) price">${fmtProb(r.fairProb)}</td>
        <td class="num ${clsSigned(r.edgeFairPick)}" title="Calibrated confidence − book fair">${Number.isFinite(r.edgeFairPick) ? fmtPct(r.edgeFairPick) : "—"}</td>
        <td class="num">${fmtLine(r.actual, r.decimals)}</td>
        <td>${betCell}</td>
        <td class="num">${pnlCell}</td>
      </tr>`;
        })
        .join("")
    : `<tr><td colspan="${showEvent ? 22 : 21}">No bet rows — set Min confidence to 0%, switch to “All graded lines”, or pick another tournament.</td></tr>`;
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
  setOverviewHistoricalVisible(true);

  const kellyEl = document.getElementById("overview-kelly-method");
  if (kellyEl) kellyEl.value = state.overviewKellyMethod;

  const lines = overviewLineRows();
  const evRows = evRowsAtMinEdge(undefined, { bettableOnly: false });
  const evAgg = aggregateEvByMarketSide(evRows);
  const totalUnits = evAgg.reduce((s, a) => s + a.units, 0);
  const totalBets = evAgg.reduce((s, a) => s + a.bets, 0);
  const flatRoi = totalBets ? (totalUnits / totalBets) * 100 : NaN;
  const kelly = overviewSizedStaking(state.overviewKellyMethod);
  const kellyLabel = overviewKellyMethodLabel(state.overviewKellyMethod);
  const totalScore = state.market
    ? lines.find((r) => r.market === state.market)
    : lines.find((r) => r.market === "Total score");
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
    <div class="kpi-card kpi-dual-card">
      <div class="kpi-label">Units won</div>
      <div class="kpi-dual-row">
        <div>
          <div class="kpi-value ${clsSigned(totalUnits)}">${totalUnits >= 0 ? "+" : ""}${fmt(totalUnits, 1)}u</div>
          <div class="kpi-sub">flat 1u</div>
        </div>
        <div>
          <div class="kpi-value ${clsSigned(kelly.units)}">${Number.isFinite(kelly.units) ? `${kelly.units >= 0 ? "+" : ""}${fmt(kelly.units, 1)}u` : "—"}</div>
          <div class="kpi-sub">${esc(kellyLabel)}</div>
        </div>
      </div>
      <div class="kpi-sub">${totalBets} flat · ${kelly.bets} Kelly · ${state.minEv}% edge</div>
    </div>
    <div class="kpi-card kpi-dual-card">
      <div class="kpi-label">ROI</div>
      <div class="kpi-dual-row">
        <div>
          <div class="kpi-value ${clsSigned(flatRoi)}">${fmtPct(flatRoi)}</div>
          <div class="kpi-sub">flat 1u / bet</div>
        </div>
        <div>
          <div class="kpi-value ${clsSigned(kelly.roi)}">${fmtPct(kelly.roi)}</div>
          <div class="kpi-sub">on $ risked</div>
        </div>
      </div>
      <div class="kpi-sub">${esc(kellyLabel)} · Risk bankroll settings</div>
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

  const roiNote = document.getElementById("overview-roi-chart-note");
  if (roiNote) {
    roiNote.textContent = `Flat 1u by market below. Kelly (${kellyLabel}): ${
      Number.isFinite(kelly.roi) ? fmtPct(kelly.roi) : "—"
    } overall ROI on amount risked · ${kelly.bets} sized bets.`;
  }

  renderBarChart(
    document.getElementById("overview-roi-chart"),
    bestEvPerMarket(evRows).map((a) => ({ market: `${a.market} (${a.side})`, roi: a.roi })),
    { valueKey: "roi", format: (v) => fmtPct(v) },
  );
  renderHonestOos();
  renderSkillWindowOos();
  renderOddsModelRoi();
}

function renderSkillWindowOos() {
  const card = document.getElementById("skill-window-card");
  if (!card) return;
  const windows = SKILL_WINDOW_REPORT?.windows;
  if (!Array.isArray(windows) || !windows.length) {
    card.hidden = true;
    return;
  }
  card.hidden = false;
  const note = document.getElementById("skill-window-note");
  const meth = SKILL_WINDOW_REPORT.methodology || {};
  const best = SKILL_WINDOW_REPORT.best_window;
  if (note) {
    const gen = SKILL_WINDOW_REPORT.generated_at
      ? new Date(SKILL_WINDOW_REPORT.generated_at).toLocaleString()
      : "";
    note.innerHTML =
      `<strong>Hypothetical only</strong> — live Round projections still use last ${meth.baseline_skill_max_rounds || 80} rounds. ` +
      `Walk-forward OOS on <strong>${meth.oos_event_count || windows.length}</strong> completed events` +
      (meth.excluded_live_event ? ` (excludes ${esc(meth.excluded_live_event)})` : "") +
      `. Recommended bet policy ROI/PnL if skill history were capped at N rounds.` +
      (best
        ? ` Best window: <strong>last ${best.skill_max_rounds}</strong> (${fmtPct(best.roi_pct)}, ${best.units >= 0 ? "+" : ""}${fmt(best.units, 1)}u` +
          (best.vs_current_units
            ? `, ${best.vs_current_units >= 0 ? "+" : ""}${fmt(best.vs_current_units, 1)}u vs current`
            : "") +
          `).`
        : "") +
      ` Regenerate: <code>node scripts/compare-skill-window-oos.mjs</code>` +
      (gen ? ` · ${gen}` : "");
  }

  const current = windows.find((w) => w.is_current) || windows[0];
  const bestWin = best ? windows.find((w) => w.skill_max_rounds === best.skill_max_rounds) : null;
  document.getElementById("skill-window-kpis").innerHTML = `
    <div class="kpi-card highlight">
      <div class="kpi-label">Current (last ${current?.skill_max_rounds || 80})</div>
      <div class="kpi-value ${clsSigned(current?.recommended?.roi_pct)}">${fmtPct(current?.recommended?.roi_pct)}</div>
      <div class="kpi-sub">${current?.recommended?.units >= 0 ? "+" : ""}${fmt(current?.recommended?.units, 1)}u · ${current?.recommended?.bets || 0} bets</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Best hypothetical</div>
      <div class="kpi-value ${clsSigned(bestWin?.recommended?.roi_pct)}">${bestWin ? fmtPct(bestWin.recommended.roi_pct) : "—"}</div>
      <div class="kpi-sub">${bestWin ? `last ${bestWin.skill_max_rounds} · ${bestWin.recommended.units >= 0 ? "+" : ""}${fmt(bestWin.recommended.units, 1)}u` : ""}</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Last 12 vs current</div>
      <div class="kpi-value ${clsSigned(windows.find((w) => w.skill_max_rounds === 12)?.delta_vs_current?.units)}">${(() => {
        const d = windows.find((w) => w.skill_max_rounds === 12)?.delta_vs_current?.units;
        return Number.isFinite(num(d)) ? `${d >= 0 ? "+" : ""}${fmt(d, 1)}u` : "—";
      })()}</div>
      <div class="kpi-sub">PnL delta (recommended policy)</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Last 4 vs current</div>
      <div class="kpi-value ${clsSigned(windows.find((w) => w.skill_max_rounds === 4)?.delta_vs_current?.units)}">${(() => {
        const d = windows.find((w) => w.skill_max_rounds === 4)?.delta_vs_current?.units;
        return Number.isFinite(num(d)) ? `${d >= 0 ? "+" : ""}${fmt(d, 1)}u` : "—";
      })()}</div>
      <div class="kpi-sub">PnL delta (recommended policy)</div>
    </div>
  `;

  const tbody = document.querySelector("#skill-window-table tbody");
  if (tbody) {
    tbody.innerHTML = windows
      .map((w) => {
        const rec = w.recommended || {};
        const d = w.delta_vs_current;
        const vs =
          w.is_current || !d
            ? "—"
            : `${d.units >= 0 ? "+" : ""}${fmt(d.units, 1)}u (${d.roi_pct >= 0 ? "+" : ""}${fmt(d.roi_pct, 1)}pt)`;
        const label = w.is_current
          ? `<strong>${esc(w.name)}</strong>`
          : esc(w.name);
        const uf = w.unfiltered_at_5pct;
        return `<tr${w.is_current ? ' class="row-current"' : bestWin && w.skill_max_rounds === bestWin.skill_max_rounds ? ' class="row-best"' : ""}>
          <td>${label}</td>
          <td class="num ${clsSigned(rec.roi_pct)}">${fmtPct(rec.roi_pct)}</td>
          <td class="num ${clsSigned(rec.units)}">${rec.units >= 0 ? "+" : ""}${fmt(rec.units, 1)}u</td>
          <td class="num ${clsSigned(d?.units)}">${vs}</td>
          <td class="num">${rec.bets ?? "—"}</td>
          <td class="num">${fmt(rec.hit_pct, 1)}%</td>
          <td class="num ${clsSigned(uf?.roi_pct)}">${uf ? fmtPct(uf.roi_pct) : "—"}</td>
        </tr>`;
      })
      .join("");
  }

  const breakdown = document.getElementById("skill-window-market-breakdown");
  if (breakdown) {
    const marketOrder = ["Total score", "Birdies", "Bogeys", "GIR", "Fairways hit"];
    const present = marketOrder.filter((m) =>
      windows.some((w) => w.recommended?.by_market?.[m]),
    );
    const marketRoi = (cell) => {
      if (!cell || !Number.isFinite(num(cell.bets)) || num(cell.bets) === 0) return NaN;
      return (num(cell.units) / num(cell.bets)) * 100;
    };
    breakdown.innerHTML = present
      .map((market) => {
        const baseCell = current?.recommended?.by_market?.[market];
        const baseUnits = num(baseCell?.units, NaN);
        // Best window for this market = most PnL.
        let bestSkill = null;
        let bestUnits = -Infinity;
        for (const w of windows) {
          const c = w.recommended?.by_market?.[market];
          const u = num(c?.units, NaN);
          if (Number.isFinite(u) && u > bestUnits) {
            bestUnits = u;
            bestSkill = w.skill_max_rounds;
          }
        }
        const rows = windows
          .map((w) => {
            const c = w.recommended?.by_market?.[market];
            const roi = marketRoi(c);
            const units = num(c?.units, NaN);
            const bets = Math.round(num(c?.bets, 0));
            const vs =
              w.is_current || !Number.isFinite(units) || !Number.isFinite(baseUnits)
                ? "—"
                : `${units - baseUnits >= 0 ? "+" : ""}${fmt(units - baseUnits, 1)}u`;
            const cls = w.is_current
              ? "row-current"
              : bestSkill === w.skill_max_rounds
                ? "row-best"
                : "";
            return `<tr${cls ? ` class="${cls}"` : ""}>
              <td>${w.is_current ? `<strong>${esc(w.name)}</strong>` : esc(w.name)}</td>
              <td class="num ${clsSigned(roi)}">${Number.isFinite(roi) ? fmtPct(roi) : "—"}</td>
              <td class="num ${clsSigned(units)}">${Number.isFinite(units) ? `${units >= 0 ? "+" : ""}${fmt(units, 1)}u` : "—"}</td>
              <td class="num ${clsSigned(w.is_current ? NaN : units - baseUnits)}">${vs}</td>
              <td class="num">${bets || "—"}</td>
            </tr>`;
          })
          .join("");
        return `<div class="table-wrap" style="margin-top:0.75rem">
          <table class="data-table skill-window-market">
            <thead>
              <tr><th colspan="5">${esc(market)}</th></tr>
              <tr>
                <th>Skill window</th>
                <th class="num">ROI</th>
                <th class="num">PnL</th>
                <th class="num">vs current</th>
                <th class="num">Bets</th>
              </tr>
            </thead>
            <tbody>${rows}</tbody>
          </table>
        </div>`;
      })
      .join("");
  }
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
  const rec = OOS_REPORT.combined_oos_recommended || OOS_REPORT.combined_oos_at_5pct;
  const peak = OOS_REPORT.peak_oos_event_at_5pct;
  const worst = OOS_REPORT.worst_oos_event_at_5pct;
  const bestTh = OOS_REPORT.best_oos_threshold;

  if (note) {
    const pol = OOS_MARKET_POLICY;
    note.innerHTML =
      `Walk-forward OOS across <strong>${OOS_REPORT.oos_event_count}</strong> completed events` +
      (OOS_REPORT.excluded_live_event ? ` (excludes live week: ${OOS_REPORT.excluded_live_event})` : "") +
      `. Per-market policy: GIR EV≥${pol.GIR?.minEv}% gap≥${pol.GIR?.minGap}; Total EV≥${pol["Total score"]?.minEv}%; Birdies EV≥${pol.Birdies?.minEv}%; FW under-only + gir−fw≥${pol["Fairways hit"]?.minGirMinusFw}. ` +
      `Regenerate: <code>npm run report:walkforward-oos-roi</code>`;
  }

  const unfiltered = OOS_REPORT.combined_oos_unfiltered_at_5pct;

  document.getElementById("oos-honest-kpis").innerHTML = `
    <div class="kpi-card highlight">
      <div class="kpi-label">OOS ROI (recommended)</div>
      <div class="kpi-value ${clsSigned(rec?.roi_pct)}">${fmtPct(rec?.roi_pct)}</div>
      <div class="kpi-sub">per-market policy · ${rec?.bets || 0} bets · ${fmt(rec?.hit_pct, 1)}% hit · ${rec?.units >= 0 ? "+" : ""}${fmt(rec?.units, 0)}u</div>
    </div>
    <div class="kpi-card">
      <div class="kpi-label">Unfiltered @ 5%</div>
      <div class="kpi-value ${clsSigned(unfiltered?.roi_pct)}">${unfiltered ? fmtPct(unfiltered.roi_pct) : "—"}</div>
      <div class="kpi-sub">all markets, no line-gap filter</div>
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
      <div class="kpi-label">Best threshold (exploratory)</div>
      <div class="kpi-value ${clsSigned(bestTh?.roi_pct)}">${bestTh ? fmtPct(bestTh.roi_pct) : "—"}</div>
      <div class="kpi-sub">${bestTh ? `≥${bestTh.min_ev_pct}% EV · ${bestTh.bets} bets` : ""}</div>
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

  const fairStats = aggregateBeatFairStats(allExplodedBetRows().filter((r) => {
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

  const signalPool = allExplodedBetRows().filter((r) => {
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
  const live = LIVE_CTX?.projections;
  const liveMeta = live
    ? {
        event: String(live.event_name || live.meta?.event_name || "").trim(),
        course: String(live.course_used || live.meta?.course_used || "").trim(),
        updated: String(live.updated_at || live.meta?.updated_at || "").trim(),
      }
    : null;
  const liveLine =
    liveMeta?.event && liveMeta?.course
      ? `<div class="header-live">Live week: <strong>${esc(liveMeta.event)}</strong> · ${esc(liveMeta.course)}${
          liveMeta.updated ? ` · projections ${esc(new Date(liveMeta.updated).toLocaleString())}` : ""
        } · Reload merges pgatour actuals + DK/PP book props into bet log</div>`
      : "";
  const cal = getWinProbCalibration();
  const calNote = cal?.generated_at
    ? `<div class="header-cal">Confidence = calibrated P(win) vs book fair · fit ${esc(new Date(cal.generated_at).toLocaleDateString())}</div>`
    : `<div class="header-cal">Confidence = raw model P(win) vs book fair (run <code>npm run fit:win-prob-calibration</code>)</div>`;
  document.getElementById("header-meta").innerHTML = `
    <div><strong>${state.tournament || "All tournaments"}</strong></div>
    <div>${sub}</div>
    ${calNote}
    ${liveLine}
  `;
}

function renderLiveFactorsPanel(summary) {
  const el = document.getElementById("live-picks-factors");
  if (!el) return;
  if (!summary?.chips?.length) {
    el.hidden = true;
    el.innerHTML = "";
    return;
  }
  el.hidden = false;
  const chips = summary.chips
    .map((c) => {
      const tone = c.tone ? ` live-factor-chip--${c.tone}` : "";
      return `<span class="live-factor-chip${tone}"><span class="live-factor-label">${esc(c.label)}</span> ${esc(c.value)}</span>`;
    })
    .join("");
  const bars = (summary.sgBars || [])
    .map((b) => {
      const w = Math.max(4, Math.round(b.pct * 100));
      return `<div class="live-sg-bar" title="${esc(b.key)} ${w}%"><span class="live-sg-bar-fill" style="width:${w}%"></span><span class="live-sg-bar-label">${esc(b.key)}</span></div>`;
    })
    .join("");
  const metaBits = [];
  if (summary.sgSource) metaBits.push(`SG weights: ${summary.sgSource}`);
  if (summary.sgVenueRounds > 0) metaBits.push(`${summary.sgVenueRounds} venue rounds`);
  if (summary.recentFormWindow) metaBits.push(summary.recentFormWindow);
  el.innerHTML = `
    <div class="live-factors-chips">${chips}</div>
    ${bars ? `<div class="live-sg-bars">${bars}</div>` : ""}
    ${metaBits.length ? `<p class="live-factors-meta">${esc(metaBits.join(" · "))}</p>` : ""}
  `;
}

function formatModelMu(market, mu) {
  if (!Number.isFinite(mu)) return "—";
  if (market === "Total score") return mu.toFixed(2);
  if (market === "Birdies" || market === "Bogeys") return mu.toFixed(1);
  return String(Math.round(mu));
}

function formatGap(market, gap) {
  if (!Number.isFinite(gap)) return "—";
  const sign = gap > 0 ? "+" : "";
  if (market === "Total score") return `${sign}${gap.toFixed(2)}`;
  if (market === "Birdies" || market === "Bogeys") return `${sign}${gap.toFixed(1)}`;
  return `${sign}${Math.round(gap)}`;
}

function populateTournamentSelect() {
  const sel = document.getElementById("filter-tournament");
  const names = allTournamentNames();
  sel.innerHTML =
    `<option value="">All tournaments (${names.length})</option>` +
    names.map((n) => `<option value="${n.replace(/"/g, "&quot;")}">${n}</option>`).join("");
  sel.value = state.tournament;
}

function populatePicksMarketFilter() {
  const sel = document.getElementById("picks-filter-market");
  if (!sel) return;
  const markets = MARKET_ORDER.filter((m) => BETTABLE_MARKETS.has(m));
  const prev = state.picksMarket;
  sel.innerHTML =
    `<option value="">All markets</option>` + markets.map((m) => `<option value="${m}">${m}</option>`).join("");
  sel.value = prev && markets.includes(prev) ? prev : "";
  state.picksMarket = sel.value;
}

function populateMarketFilter() {
  const rows = state.tournament ? latestRowsForTournament(state.tournament) : combinedTournamentRows();
  const markets = uniqueSorted(rows.map((r) => r.market)).sort(
    (a, b) => marketSortKey(a) - marketSortKey(b),
  );
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
    renderLiveFactorsPanel(null);
    tbody.innerHTML = `<tr><td colspan="13" class="live-picks-empty">No live projections available.</td></tr>`;
    return;
  }

  const built = buildLiveBestBets({
    projections: LIVE_CTX.projections,
    oos: LIVE_CTX.oos || OOS_REPORT,
    signals: LIVE_CTX.signals,
    courseRow: LIVE_CTX.courseRow,
    minEvPct: state.minEv,
    marketFilter: state.picksMarket,
  });

  card.hidden = false;
  renderLiveFactorsPanel(built.factorsSummary);
  if (titleEl) {
    titleEl.textContent = `Best bets — ${built.roundLabel}${built.eventName ? ` · ${built.eventName}` : ""}`;
  }
  const oosRoi = num((LIVE_CTX.oos || OOS_REPORT)?.combined_oos_at_5pct?.roi_pct, NaN);
  const oosN = Math.round(num((LIVE_CTX.oos || OOS_REPORT)?.combined_oos_at_5pct?.bets, NaN)) || 0;
  if (noteEl) {
    const venue = built.venueNote ? ` ${built.venueNote}.` : "";
    const factors = built.factorsNote ? ` ${built.factorsNote}.` : "";
    const dkNote = built.modelLinesOnly
      ? " DraftKings scrape unavailable — showing model half-lines at −110 (not real +EV until DK posts)."
      : "";
    noteEl.textContent =
      `Upcoming round picks from projections.json (${built.updatedAt ? `updated ${new Date(built.updatedAt).toLocaleString()}` : "live"}).${factors}${venue}${dkNote}` +
      ` Ranked by calibrated confidence vs book fair, walk-forward OOS market ROI` +
      (Number.isFinite(oosRoi) ? ` (${oosRoi >= 0 ? "+" : ""}${oosRoi.toFixed(1)}% on ${oosN} OOS policy bets)` : "") +
      `, and historical context signals. Uses toolbar Min confidence edge %.`;
  }

  let picks = built.picks;
  if (state.side) {
    picks = picks.filter((p) => String(p.side).toLowerCase() === state.side.toLowerCase());
  }
  if (state.player) {
    const q = state.player.toLowerCase();
    picks = picks.filter((p) => String(p.player_name || "").toLowerCase().includes(q));
  }

  if (!picks.length) {
    const mktLabel = state.picksMarket ? ` for ${state.picksMarket}` : "";
    tbody.innerHTML = `<tr><td colspan="14" class="live-picks-empty">No picks${mktLabel} at ≥${state.minEv}% confidence edge for ${built.roundLabel} — set Min confidence to 0%, try All markets, or refresh DK props.</td></tr>`;
    return;
  }

  tbody.innerHTML = picks
    .map((p, i) => {
      const hist =
        Number.isFinite(p.histRoi) && p.histBets > 0
          ? `${p.histRoi >= 0 ? "+" : ""}${p.histRoi.toFixed(1)}%`
          : "—";
      const edgeCls = p.edgePct > 0 ? "pos" : p.edgePct < 0 ? "neg" : "";
      const gapCls = Number.isFinite(p.gap) ? (p.gap > 0 ? "pos" : p.gap < 0 ? "neg" : "") : "";
      const confPct = Number.isFinite(p.confP) ? `${(p.confP * 100).toFixed(1)}%` : "—";
      const bookPct = Number.isFinite(p.fairP) ? `${(p.fairP * 100).toFixed(1)}%` : "—";
      const tags = (p.contextTags || [])
        .map((t) => {
          const warn = String(t).startsWith("fade") || String(t).includes("% -");
          const tailor = String(t).startsWith("Course fit") || String(t).includes("recent form") || String(t).includes("Birdie-heavy");
          const cls = warn ? " warn" : tailor ? " tailor" : "";
          return `<span class="live-picks-tag${cls}">${esc(t)}</span>`;
        })
        .join("");
      const sideLabel = p.side === "over" ? "Over" : "Under";
      const lineLabel = Number.isFinite(p.line) ? p.line : "—";
      return `<tr class="live-picks-row"
        data-event="${esc(built.eventName || "")}"
        data-round="${built.round || ""}"
        data-player="${esc(p.player_name)}"
        data-market="${esc(p.market)}"
        data-side="${esc(p.side)}"
        data-line="${Number.isFinite(p.line) ? p.line : ""}"
        data-odds="${Number.isFinite(p.odds) ? Math.round(p.odds) : ""}"
        data-dg-id="${p.dg_id || ""}">
        <td class="num">${i + 1}</td>
        <td>${esc(p.player_name)}</td>
        <td>${esc(p.market)}</td>
        <td class="num">${esc(sideLabel)}</td>
        <td class="num" title="Model μ">${formatModelMu(p.market, p.mu)}</td>
        <td class="num">${lineLabel}</td>
        <td class="num ${gapCls}">${formatGap(p.market, p.gap)}</td>
        <td class="num">${esc(formatAmerican(p.odds))}</td>
        <td class="num" title="Calibrated P(win)">${confPct}</td>
        <td class="num" title="Book fair">${bookPct}</td>
        <td class="num ${edgeCls}">${p.edgePct >= 0 ? "+" : ""}${p.edgePct.toFixed(1)}%</td>
        <td class="num" title="${p.histBets ? `${p.histBets} OOS bets` : ""}">${hist}</td>
        <td class="live-picks-context">${tags || '<span class="muted">—</span>'}</td>
        <td class="num">${p.score.toFixed(1)}</td>
        <td class="num"><button type="button" class="btn btn-sm btn-journal-add" title="Add to My bets">+</button></td>
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
  renderMyBets();
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
  if (name === "picks") populatePicksMarketFilter();
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
  return alignDetailCsvText(pick.text);
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

async function loadSkillWindowReport() {
  try {
    const res = await fetch(`${SKILL_WINDOW_JSON_URL}?t=${Date.now()}`, { cache: "no-store" });
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

async function loadPgatourEventRounds() {
  try {
    const res = await fetch(`../data/pgatour_event_rounds.json?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return null;
    return await res.json();
  } catch {
    return null;
  }
}

async function loadLiveEventBookProps() {
  try {
    const res = await fetch(`../data/live_event_book_props.json?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return null;
    return await res.json();
  } catch {
    return null;
  }
}

async function loadData() {
  const errEl = document.getElementById("error-banner");
  errEl.hidden = true;
  try {
    const [summaryText, detailText, oos, oddsRoi, oddsLines, skillWindow] = await Promise.all([
      loadSummaryCsvText(),
      loadDetailCsvText(),
      loadOosReport(),
      loadOddsModelRoi(),
      loadOddsLinesCsv(),
      loadSkillWindowReport(),
    ]);
    await loadWinProbCalibration();
    OOS_REPORT = oos;
    ODDS_MODEL_ROI = oddsRoi;
    ODDS_LINES_ROWS = oddsLines;
    SKILL_WINDOW_REPORT = skillWindow;
    ALL_ROWS = parseCsv(summaryText).filter((r) => String(r.market || "").trim() !== "Round matchups");
    if (!ALL_ROWS.length) throw new Error("Summary CSV is empty");
    DETAIL_ROWS = detailText ? parseCsv(detailText) : [];
    invalidateExplodedBetRows();
    invalidateLiveBestBetsCache();
    const [liveCtx, pgRounds, liveBookProps] = await Promise.all([
      loadLiveBestBetsContext(),
      loadPgatourEventRounds(),
      loadLiveEventBookProps(),
    ]);
    LIVE_CTX = liveCtx;
    DETAIL_ROWS = patchDetailRowsFromLiveSources(DETAIL_ROWS, LIVE_CTX?.projections, pgRounds, liveBookProps);
    invalidateExplodedBetRows();
    populateTournamentSelect();
    populateMarketFilter();
    populatePicksMarketFilter();
    populateOddsCourseSelect();
    gradeMyBetsFromLoadedData();
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
  const filterBook = document.getElementById("filter-book");
  if (filterBook) {
    filterBook.addEventListener("change", (e) => {
      state.book = e.target.value;
      renderAll();
    });
  }
  document.getElementById("filter-min-ev").addEventListener("change", (e) => {
    state.minEv = num(e.target.value) || 0;
    invalidateExplodedBetRows();
    renderAll();
  });
  document.getElementById("overview-kelly-method")?.addEventListener("change", (e) => {
    const v = String(e.target.value || "");
    state.overviewKellyMethod = OVERVIEW_KELLY_METHODS.has(v) ? v : "kelly_unit_cap";
    saveOverviewKellyMethod();
    renderOverview();
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
    renderLivePicks();
  });
  const picksMarket = document.getElementById("picks-filter-market");
  if (picksMarket) {
    picksMarket.addEventListener("change", (e) => {
      state.picksMarket = e.target.value;
      renderLivePicks();
    });
  }
  document.getElementById("filter-show").addEventListener("change", (e) => {
    state.show = e.target.value;
    invalidateExplodedBetRows();
    renderAll();
  });
  for (const id of ["risk-bankroll", "risk-method", "risk-unit-pct", "risk-max-stake", "risk-round-cap"]) {
    const el = document.getElementById(id);
    if (!el) continue;
    el.addEventListener("input", syncRiskFromForm);
    el.addEventListener("change", syncRiskFromForm);
  }
  const myBetsBankroll = document.getElementById("my-bets-bankroll");
  if (myBetsBankroll) {
    myBetsBankroll.addEventListener("input", syncMyBetsBankrollFromForm);
    myBetsBankroll.addEventListener("change", syncMyBetsBankrollFromForm);
  }
  document.querySelectorAll(".my-bets-subtab").forEach((btn) => {
    btn.addEventListener("click", () => {
      setMyBetsView(btn.getAttribute("data-my-bets-view") || "browse");
      renderMyBets();
    });
  });
  const browseSearch = document.getElementById("my-bets-search");
  if (browseSearch) {
    browseSearch.addEventListener("input", (e) => {
      state.myBets.browseSearch = e.target.value.trim();
      renderMyBetsBrowse();
    });
  }
  document.getElementById("btn-my-bets-clear-all")?.addEventListener("click", clearAllMyBets);
  const browseMarket = document.getElementById("my-bets-browse-market");
  if (browseMarket) {
    browseMarket.addEventListener("change", (e) => {
      state.myBets.browseMarket = e.target.value;
      renderMyBetsBrowse();
    });
  }
  document.getElementById("my-bets-available-tbody")?.addEventListener("click", (ev) => {
    const addBtn = ev.target.closest(".my-bets-add-line");
    if (addBtn) {
      addMyBetsLineByKey(addBtn.dataset.lineKey || "");
      return;
    }
    const remBtn = ev.target.closest(".my-bets-remove-line");
    if (remBtn) removeMyBetByLineKey(remBtn.dataset.lineKey || "");
  });
  document.getElementById("my-bets-slip-tbody")?.addEventListener("change", (ev) => {
    const inp = ev.target.closest(".my-bets-stake-input");
    if (inp) {
      updateMyBetStake(inp.dataset.id, inp.value);
      return;
    }
    const sel = ev.target.closest(".my-bets-result-select");
    if (!sel) return;
    updateMyBetResult(sel.dataset.id, sel.value);
  });
  document.getElementById("my-bets-slip-tbody")?.addEventListener("click", (ev) => {
    const btn = ev.target.closest(".my-bets-del");
    if (!btn) return;
    deleteMyBet(btn.dataset.id);
  });
  document.getElementById("my-bets-history-table")?.addEventListener("click", (ev) => {
    const btn = ev.target.closest(".my-bets-del");
    if (!btn) return;
    deleteMyBet(btn.dataset.id);
  });
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
    const addBtn = ev.target.closest(".btn-journal-add");
    if (addBtn) {
      ev.stopPropagation();
      const tr = addBtn.closest("tr.live-picks-row");
      if (!tr) return;
      prefillMyBetsFromPick({
        eventName: tr.dataset.event || "",
        round: num(tr.dataset.round) || 1,
        playerName: tr.dataset.player || "",
        market: tr.dataset.market || "",
        side: tr.dataset.side || "",
        line: num(tr.dataset.line),
        odds: num(tr.dataset.odds),
        dg_id: tr.dataset.dgId || "",
      });
      return;
    }
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
if (["overview", "accuracy", "ev", "odds-csv", "bets", "risk", "my-bets", "events", "insights", "picks", "guide"].includes(initialTab)) {
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
