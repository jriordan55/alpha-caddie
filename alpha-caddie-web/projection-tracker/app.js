/**
 * Lean both-side edge tracker — replaces the old multi-tab projection tracker.
 */
const ROI_URL = "../data/both_side_roi.json";
const BETS_URL = "../data/both_side_bets.json";
const PROJ_URL = "../projections.json";
const LIVE_PROPS_URL = "../data/live_event_book_props.json";

const MARKET_TO_PLAYER = {
  "Total score": (p) => Number(p.total_score),
  Birdies: (p) => Number(p.birdies) + Number(p.eagles || 0),
  Bogeys: (p) => Number(p.bogeys) + Number(p.doubles || p.doubles_or_worse || 0),
  Pars: (p) => Number(p.pars),
  GIR: (p) => Number(p.gir),
  "Fairways hit": (p) => Number(p.fairways),
};

const DK_MARKET = {
  "Total score": "Total Score",
  Birdies: "Birdies",
  Bogeys: "Bogeys",
  Pars: "Pars",
  GIR: "GIR",
  "Fairways hit": "Fairways hit",
};

let ROI = null;
let BETS = null;
let PROJ = null;
let LIVE_PROPS = null;
/** @type {{ pnl?: import("chart.js").Chart, bank?: import("chart.js").Chart, roi?: import("chart.js").Chart }} */
let AN_CHARTS = {};

const STAKE = 100;

function $(id) {
  return document.getElementById(id);
}

function fmtMoney(x) {
  if (!Number.isFinite(x)) return "—";
  const s = x >= 0 ? "+" : "−";
  return `${s}$${Math.abs(Math.round(x)).toLocaleString()}`;
}

function fmtPct(x) {
  if (!Number.isFinite(x)) return "—";
  const v = Math.abs(x) <= 1.5 ? x * 100 : x;
  return `${v >= 0 ? "+" : ""}${v.toFixed(1)}%`;
}

function clsSigned(x) {
  if (!Number.isFinite(x)) return "";
  return x > 0 ? "pos" : x < 0 ? "neg" : "";
}

function americanToImplied(o) {
  if (!Number.isFinite(o) || o === 0) return NaN;
  return o > 0 ? 100 / (o + 100) : Math.abs(o) / (Math.abs(o) + 100);
}

async function loadJson(url) {
  const res = await fetch(`${url}?t=${Date.now()}`, { cache: "no-store" });
  if (!res.ok) throw new Error(`${url} → ${res.status}`);
  return res.json();
}

function passingMarkets() {
  return ROI?.overall?.both_side_positive_markets || [];
}

function renderHero() {
  const o = ROI?.overall || {};
  const pass = o.both_side_positive_markets || [];
  $("kpi-markets").textContent = String(pass.length);
  const pnl = o.recommended_combined_pnl;
  const el = $("kpi-pnl");
  el.textContent = fmtMoney(pnl);
  el.className = clsSigned(pnl);
  $("kpi-bets").textContent = String(o.recommended_combined_bets ?? "—");
  let minRoi = Infinity;
  for (const m of pass) {
    const r = ROI.recommended?.[m];
    if (Number.isFinite(r?.min_roi)) minRoi = Math.min(minRoi, r.min_roi);
  }
  const mr = $("kpi-minroi");
  if (!Number.isFinite(minRoi) || minRoi === Infinity) {
    mr.textContent = "—";
    mr.className = "";
  } else {
    mr.textContent = fmtPct(minRoi);
    mr.className = clsSigned(minRoi);
  }
}

function renderMarketTable() {
  const tbody = $("market-table").querySelector("tbody");
  const rec = ROI?.recommended || {};
  const order = [
    "Total score",
    "Pars",
    "Bogeys",
    "GIR",
    "Birdies",
    "Fairways hit",
  ];
  tbody.innerHTML = order
    .filter((m) => rec[m])
    .map((m) => {
      const r = rec[m];
      const ok = r.both_sides_positive;
      const gapLabel =
        r.gap_over != null && r.gap_under != null && r.gap_over !== r.gap_under
          ? `${r.gap_over}/${r.gap_under}`
          : typeof r.gap === "object"
            ? `${r.gap.over}/${r.gap.under}`
            : String(r.gap ?? "—");
      const biasLabel = r.odds_rule?.under_min_american != null
        ? `${r.bias} · U≥${r.odds_rule.under_min_american}`
        : r.bias;
      return `<tr>
        <td>${m}</td>
        <td><span class="badge ${ok ? "badge-on" : "badge-off"}">${ok ? "Both sides +" : "Off"}</span></td>
        <td class="num">${gapLabel}</td>
        <td>${biasLabel}</td>
        <td class="num">${r.over?.bets ?? "—"}</td>
        <td class="num ${clsSigned(r.over?.roi)}">${fmtPct(r.over?.roi)}</td>
        <td class="num">${r.under?.bets ?? "—"}</td>
        <td class="num ${clsSigned(r.under?.roi)}">${fmtPct(r.under?.roi)}</td>
        <td class="num ${clsSigned(r.min_roi)}">${fmtPct(r.min_roi)}</td>
        <td class="num ${clsSigned(r.combined_pnl)}">${fmtMoney(r.combined_pnl)}</td>
      </tr>`;
    })
    .join("");
}

function fillMarketSelects() {
  const pass = passingMarkets();
  for (const id of ["live-market", "hist-market", "an-market"]) {
    const sel = $(id);
    if (!sel) continue;
    const cur = sel.value;
    const allLabel =
      id === "live-market"
        ? "Both-side markets only"
        : id === "an-market"
          ? "All passing markets"
          : "All passing markets";
    sel.innerHTML =
      `<option value="">${allLabel}</option>` +
      pass.map((m) => `<option value="${m}">${m}</option>`).join("");
    if ([...sel.options].some((o) => o.value === cur)) sel.value = cur;
  }
  const events = [...new Set((BETS?.bets || []).map((b) => b.event).filter(Boolean))];
  const order = ROI?.event_order || [];
  events.sort((a, b) => {
    const ia = order.indexOf(a);
    const ib = order.indexOf(b);
    if (ia >= 0 && ib >= 0) return ia - ib;
    if (ia >= 0) return -1;
    if (ib >= 0) return 1;
    return String(a).localeCompare(String(b));
  });
  for (const id of ["hist-event", "an-event"]) {
    const es = $(id);
    if (!es) continue;
    const evCur = es.value;
    es.innerHTML =
      `<option value="">All events</option>` +
      events.map((e) => `<option value="${e}">${e}</option>`).join("");
    if ([...es.options].some((o) => o.value === evCur)) es.value = evCur;
  }
}

function histFiltered() {
  const pass = new Set(passingMarkets());
  const mkt = $("hist-market").value;
  const side = $("hist-side").value;
  const ev = $("hist-event").value;
  return (BETS?.bets || []).filter((b) => {
    if (!pass.has(b.market)) return false;
    if (mkt && b.market !== mkt) return false;
    if (side && String(b.side).toLowerCase() !== side) return false;
    if (ev && b.event !== ev) return false;
    return true;
  });
}

function renderHist() {
  const rows = histFiltered();
  let pnl = 0;
  let w = 0;
  let l = 0;
  let p = 0;
  for (const b of rows) {
    pnl += Number(b.pnl) || 0;
    if (b.result === "W") w++;
    else if (b.result === "L") l++;
    else p++;
  }
  const staked = rows.length * 100;
  const roi = staked > 0 ? pnl / staked : NaN;
  $("hist-kpis").innerHTML = `
    <span>Bets <strong>${rows.length}</strong></span>
    <span>Record <strong>${w}W-${l}L-${p}P</strong></span>
    <span>PnL <strong class="${clsSigned(pnl)}">${fmtMoney(pnl)}</strong></span>
    <span>ROI <strong class="${clsSigned(roi)}">${fmtPct(roi)}</strong></span>
  `;
  const show = rows.slice(0, 500);
  $("hist-table").querySelector("tbody").innerHTML = show
    .map((b) => {
      const resCls = b.result === "W" ? "pos" : b.result === "L" ? "neg" : "";
      return `<tr>
        <td>${b.event}</td>
        <td class="num">${b.round ?? ""}</td>
        <td>${b.player || ""}</td>
        <td>${b.market}</td>
        <td class="num">${b.side}</td>
        <td class="num">${Number(b.model).toFixed(1)}</td>
        <td class="num">${b.book}</td>
        <td class="num">${b.odds > 0 ? "+" : ""}${b.odds}</td>
        <td class="num">${b.actual}</td>
        <td class="${resCls}">${b.result}</td>
        <td class="num ${clsSigned(b.pnl)}">${fmtMoney(b.pnl)}</td>
      </tr>`;
    })
    .join("");
}

function dkPropIndex() {
  const pack = LIVE_PROPS?.pre_round_dk || LIVE_PROPS?.live_dk || {};
  return pack;
}

function livePicks() {
  if (!PROJ?.players?.length) return [];
  const pass = passingMarkets();
  const mktFilter = $("live-market").value;
  const gapMode = $("live-gap").value;
  const rnd = Math.round(Number(PROJ.display_round || 1)) || 1;
  const bias = BETS?.live_bias || ROI?.live_bias || {};
  const props = dkPropIndex();
  const out = [];

  for (const p of PROJ.players) {
    if (Math.round(Number(p.round)) !== rnd) continue;
    const dg = Math.round(Number(p.dg_id));
    const name = String(p.player_name || "");
    for (const market of pass) {
      if (mktFilter && market !== mktFilter) continue;
      const rec = ROI.recommended?.[market];
      if (!rec?.both_sides_positive) continue;
      const rawMu = MARKET_TO_PLAYER[market]?.(p);
      if (!Number.isFinite(rawMu)) continue;
      const b = Number(bias[market]) || 0;
      const mu = rawMu - b;
      const key = `${dg}|${rnd}|${DK_MARKET[market]}`;
      const prop = props[key];
      const line = Number(prop?.line);
      const over = Number(prop?.over);
      const under = Number(prop?.under);
      if (!Number.isFinite(line)) continue;
      const gapOver = Number(rec.gap_over ?? rec.gap);
      const gapUnder = Number(rec.gap_under ?? rec.gap);
      const gapNeedOver = gapMode === "policy" ? gapOver : Number(gapMode);
      const gapNeedUnder = gapMode === "policy" ? gapUnder : Number(gapMode);
      const delta = mu - line;
      let side = null;
      if (delta > gapNeedOver) side = "OVER";
      else if (delta < -gapNeedUnder) side = "UNDER";
      if (!side) continue;
      const odds = side === "OVER" ? over : under;
      if (!Number.isFinite(odds)) continue;
      const underMin = rec.odds_rule?.under_min_american;
      const overMin = rec.odds_rule?.over_min_american;
      if (side === "UNDER" && Number.isFinite(underMin) && !(odds >= underMin)) continue;
      if (side === "OVER" && Number.isFinite(overMin) && !(odds >= overMin)) continue;
      const fair = americanToImplied(odds);
      // crude edge: distance past line as confidence proxy
      const gapNeed = side === "OVER" ? gapNeedOver : gapNeedUnder;
      const edge = Math.abs(delta) - gapNeed;
      out.push({
        player: name,
        market,
        side,
        mu: Math.round(mu * 10) / 10,
        line,
        gap: Math.round(delta * 100) / 100,
        odds,
        fair,
        edge,
      });
    }
  }
  out.sort((a, b) => b.edge - a.edge);
  return out;
}

function renderLive() {
  const picks = livePicks();
  const note = $("live-note");
  if (!PROJ) {
    note.textContent = "No projections.json — run refresh:live / apply:dg-methodology.";
  } else {
    note.textContent = `${PROJ.event_name || "Live"} · R${PROJ.display_round || "?"} · μ corrected by chrono bias · only both-side+ markets`;
  }
  $("live-table").querySelector("tbody").innerHTML = picks.length
    ? picks
        .slice(0, 80)
        .map(
          (p) => `<tr>
        <td>${p.player}</td>
        <td>${p.market}</td>
        <td class="num">${p.side}</td>
        <td class="num">${p.mu.toFixed(1)}</td>
        <td class="num">${p.line}</td>
        <td class="num ${clsSigned(p.gap)}">${p.gap > 0 ? "+" : ""}${p.gap.toFixed(2)}</td>
        <td class="num">${p.odds > 0 ? "+" : ""}${p.odds}</td>
        <td class="num">${p.edge.toFixed(2)}</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="8">No live DK props past policy gap for both-side markets.</td></tr>`;
}

function betTs(b) {
  const t = Number(b?.ts);
  if (Number.isFinite(t) && t > 0) return t;
  const d = Date.parse(String(b?.date || ""));
  return Number.isFinite(d) ? d : NaN;
}

function fmtChartDate(ms) {
  if (!Number.isFinite(ms)) return "";
  return new Date(ms).toLocaleDateString(undefined, {
    month: "short",
    day: "numeric",
    year: "2-digit",
  });
}

function anFiltered() {
  const pass = new Set(passingMarkets());
  const mkt = $("an-market")?.value || "";
  const ev = $("an-event")?.value || "";
  return (BETS?.bets || []).filter((b) => {
    if (!pass.has(b.market)) return false;
    if (mkt && b.market !== mkt) return false;
    if (ev && b.event !== ev) return false;
    return true;
  });
}

function sortBetsChrono(bets) {
  return [...bets].sort((a, b) => {
    const ta = betTs(a);
    const tb = betTs(b);
    const fa = Number.isFinite(ta);
    const fb = Number.isFinite(tb);
    if (fa && fb && ta !== tb) return ta - tb;
    if (fa !== fb) return fa ? -1 : 1;
    const ra = Number(a.round) || 0;
    const rb = Number(b.round) || 0;
    if (ra !== rb) return ra - rb;
    const ma = String(a.market || "").localeCompare(String(b.market || ""));
    if (ma) return ma;
    return String(a.player || "").localeCompare(String(b.player || ""));
  });
}

function buildAnalyticsSeries(bets, startBankroll) {
  const stake = Number(BETS?.stake_dollars) || STAKE;
  const start = Number.isFinite(startBankroll) && startBankroll > 0 ? startBankroll : 10000;
  const sorted = sortBetsChrono(bets);
  const meta = [];
  const cumPnl = [];
  const bankroll = [];
  const drawdown = [];
  const cumRoi = [];
  let pnl = 0;
  let bank = start;
  let peak = start;
  let maxDd = 0;
  let dayBucket = "";
  /** Flush one chart point per calendar day (end-of-day) for smooth curves. */
  const flushDay = (m, values) => {
    meta.push(m);
    cumPnl.push({ x: m.x, y: values.pnl });
    bankroll.push({ x: m.x, y: values.bank });
    drawdown.push({ x: m.x, y: values.dd });
    cumRoi.push({ x: m.x, y: values.roi });
  };

  let pending = null;
  for (let i = 0; i < sorted.length; i++) {
    const b = sorted[i];
    const p = Number(b.pnl);
    const dp = Number.isFinite(p) ? p : 0;
    pnl += dp;
    bank += dp;
    if (bank > peak) peak = bank;
    const ddPct = peak > 0 ? ((peak - bank) / peak) * 100 : 0;
    if (ddPct > maxDd) maxDd = ddPct;
    const base = betTs(b);
    const day = Number.isFinite(base)
      ? new Date(base).toISOString().slice(0, 10)
      : `idx-${i}`;
    if (day !== dayBucket) {
      if (pending) flushDay(pending.meta, pending.values);
      dayBucket = day;
    }
    const x = Number.isFinite(base) ? base : i;
    pending = {
      meta: {
        x,
        date: b.date || (Number.isFinite(base) ? new Date(base).toISOString().slice(0, 10) : ""),
        event: b.event,
        round: b.round,
        player: b.player,
        market: b.market,
        side: b.side,
        betIndex: i + 1,
        betsThrough: i + 1,
      },
      values: {
        pnl: Math.round(pnl * 100) / 100,
        bank: Math.round(bank * 100) / 100,
        dd: Math.round(ddPct * 100) / 100,
        roi: Math.round((pnl / ((i + 1) * stake)) * 10000) / 100,
      },
    };
  }
  if (pending) flushDay(pending.meta, pending.values);

  return {
    meta,
    cumPnl,
    bankroll,
    drawdown,
    cumRoi,
    n: sorted.length,
    pnl,
    bank,
    peak,
    maxDd,
    finalRoi: sorted.length ? pnl / (sorted.length * stake) : NaN,
    start,
    stake,
  };
}

const CHART_DEFAULTS = {
  color: "#9aab9c",
  borderColor: "#2c382e",
  font: { family: "'Instrument Sans', system-ui, sans-serif", size: 11 },
};

function chartCommonOptions(yTitle, yTickFn, meta) {
  return {
    responsive: true,
    maintainAspectRatio: false,
    parsing: false,
    interaction: { mode: "nearest", axis: "x", intersect: false },
    plugins: {
      legend: {
        display: true,
        labels: { color: CHART_DEFAULTS.color, boxWidth: 12, font: CHART_DEFAULTS.font },
      },
      tooltip: {
        callbacks: {
          title(items) {
            const i = items?.[0]?.dataIndex ?? 0;
            const m = meta?.[i];
            if (!m) return `Bet #${i + 1}`;
            const d = m.date || fmtChartDate(m.x);
            return `${d} · ${m.betsThrough || m.betIndex} bets`;
          },
          afterTitle(items) {
            const i = items?.[0]?.dataIndex ?? 0;
            const m = meta?.[i];
            if (!m) return "";
            return `${m.event || ""} · R${m.round ?? "?"}`;
          },
        },
      },
    },
    scales: {
      x: {
        type: "linear",
        title: { display: true, text: "Date", color: CHART_DEFAULTS.color },
        ticks: {
          color: CHART_DEFAULTS.color,
          maxRotation: 45,
          autoSkip: true,
          maxTicksLimit: 10,
          callback(val) {
            return fmtChartDate(val);
          },
        },
        grid: { color: "rgba(44,56,46,0.45)" },
      },
      y: {
        title: { display: true, text: yTitle, color: CHART_DEFAULTS.color },
        ticks: {
          color: CHART_DEFAULTS.color,
          callback: yTickFn || ((v) => v),
        },
        grid: { color: "rgba(44,56,46,0.45)" },
      },
    },
  };
}

function upsertChart(key, canvasId, config) {
  const canvas = $(canvasId);
  if (!canvas || typeof Chart === "undefined") return;
  if (AN_CHARTS[key]) {
    AN_CHARTS[key].destroy();
    delete AN_CHARTS[key];
  }
  AN_CHARTS[key] = new Chart(canvas.getContext("2d"), config);
}

function renderAnalytics() {
  const startEl = $("an-bankroll");
  const start = Number(startEl?.value);
  const series = buildAnalyticsSeries(anFiltered(), start);
  const kpi = $("an-kpis");
  if (kpi) {
    kpi.innerHTML = `
      <span>Bets <strong>${series.n}</strong></span>
      <span>PnL <strong class="${clsSigned(series.pnl)}">${fmtMoney(series.pnl)}</strong></span>
      <span>ROI <strong class="${clsSigned(series.finalRoi)}">${fmtPct(series.finalRoi)}</strong></span>
      <span>End bankroll <strong>$${Math.round(series.bank).toLocaleString()}</strong></span>
      <span>Max drawdown <strong class="neg">${series.maxDd.toFixed(1)}%</strong></span>
    `;
  }

  if (!series.n) {
    for (const k of Object.keys(AN_CHARTS)) {
      AN_CHARTS[k]?.destroy();
      delete AN_CHARTS[k];
    }
    return;
  }

  const moneyTick = (v) =>
    `${v < 0 ? "−" : ""}$${Math.abs(Math.round(v)).toLocaleString()}`;
  const pctTick = (v) => `${v}%`;
  const baseOpts = (yTitle, yTick) => chartCommonOptions(yTitle, yTick, series.meta);
  const curve = {
    tension: 0.45,
    cubicInterpolationMode: "monotone",
    pointRadius: 0,
    pointHoverRadius: 4,
    borderWidth: 2.5,
  };

  upsertChart("pnl", "chart-pnl", {
    type: "line",
    data: {
      datasets: [
        {
          label: "Cumulative PnL",
          data: series.cumPnl,
          borderColor: "#c4a35a",
          backgroundColor: "rgba(196,163,90,0.12)",
          fill: true,
          ...curve,
        },
      ],
    },
    options: baseOpts("PnL ($)", moneyTick),
  });

  const bankOpts = baseOpts("Bankroll ($)", moneyTick);
  bankOpts.scales.y1 = {
    position: "right",
    title: { display: true, text: "Drawdown %", color: CHART_DEFAULTS.color },
    ticks: { color: CHART_DEFAULTS.color, callback: pctTick },
    grid: { drawOnChartArea: false },
    min: 0,
  };

  upsertChart("bank", "chart-bankroll", {
    type: "line",
    data: {
      datasets: [
        {
          label: "Bankroll",
          data: series.bankroll,
          borderColor: "#5dcc7a",
          backgroundColor: "transparent",
          ...curve,
          yAxisID: "y",
        },
        {
          label: "Drawdown %",
          data: series.drawdown,
          borderColor: "#e07070",
          backgroundColor: "rgba(224,112,112,0.15)",
          fill: true,
          ...curve,
          borderWidth: 2,
          yAxisID: "y1",
        },
      ],
    },
    options: bankOpts,
  });

  upsertChart("roi", "chart-roi", {
    type: "line",
    data: {
      datasets: [
        {
          label: "Cumulative ROI %",
          data: series.cumRoi,
          borderColor: "#7eb8e8",
          backgroundColor: "rgba(126,184,232,0.12)",
          fill: true,
          ...curve,
        },
      ],
    },
    options: baseOpts("ROI %", pctTick),
  });
}

function renderAll() {
  renderHero();
  renderMarketTable();
  fillMarketSelects();
  renderAnalytics();
  renderHist();
  renderLive();
}

async function boot() {
  const err = $("error");
  err.hidden = true;
  try {
    [ROI, BETS, PROJ, LIVE_PROPS] = await Promise.all([
      loadJson(ROI_URL),
      loadJson(BETS_URL),
      loadJson(PROJ_URL).catch(() => null),
      loadJson(LIVE_PROPS_URL).catch(() => null),
    ]);
    renderAll();
  } catch (e) {
    err.hidden = false;
    err.textContent = String(e?.message || e);
  }
}

$("btn-reload")?.addEventListener("click", () => boot());
for (const id of ["hist-market", "hist-side", "hist-event"]) {
  $(id)?.addEventListener("change", () => renderHist());
}
for (const id of ["live-market", "live-gap"]) {
  $(id)?.addEventListener("change", () => renderLive());
}
for (const id of ["an-market", "an-event"]) {
  $(id)?.addEventListener("change", () => renderAnalytics());
}
$("an-bankroll")?.addEventListener("change", () => renderAnalytics());
$("an-bankroll")?.addEventListener("input", () => renderAnalytics());

boot();
