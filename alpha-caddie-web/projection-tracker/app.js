/**
 * Lean both-side edge tracker — replaces the old multi-tab projection tracker.
 */
const ROI_URL = "../data/both_side_roi.json";
const BETS_URL = "../data/both_side_bets.json";
const PROJ_URL = "../projections.json";
const LIVE_PROPS_URL = "../data/live_event_book_props.json";
const HOLE_PROPS_URL = "../data/live_hole_props.json";

const MARKET_TO_PLAYER = {
  "Total score": (p) => Number(p.total_score),
  Birdies: (p) => Number(p.birdies) + Number(p.eagles || 0),
  Bogeys: (p) => Number(p.bogeys) + Number(p.doubles || p.doubles_or_worse || 0),
  Pars: (p) => Number(p.pars),
  GIR: (p) => Number(p.gir),
  "Fairways hit": (p) => Number(p.fairways),
};

/** ROI market → live props market label */
const PROP_MARKET = {
  "Total score": "Total Score",
  Birdies: "Birdies",
  Bogeys: "Bogeys",
  Pars: "Pars",
  GIR: "GIR",
  "Fairways hit": "Fairways hit",
};

const LIVE_BOOKS = [
  { id: "draftkings", label: "DraftKings", short: "dk" },
  { id: "prizepicks", label: "PrizePicks", short: "pp" },
  { id: "sleeper", label: "Sleeper", short: "sl" },
  { id: "underdog", label: "Underdog", short: "ud" },
  { id: "fanduel", label: "FanDuel", short: "fd" },
  { id: "caesars", label: "Caesars", short: "czr" },
  { id: "kalshi", label: "Kalshi", short: "kl" },
];

let ROI = null;
let BETS = null;
let PROJ = null;
let LIVE_PROPS = null;
let HOLE_PROPS = null;
/** @type {{ pnl?: import("chart.js").Chart, bank?: import("chart.js").Chart, roi?: import("chart.js").Chart }} */
let AN_CHARTS = {};
let ACTIVE_TAB = "both-side";

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

function bookCatalog() {
  const fromRoi = Array.isArray(ROI?.books) ? ROI.books : [];
  if (fromRoi.length) return fromRoi.map((b) => ({ id: b.id, label: b.label || b.id }));
  const ids = new Set();
  for (const b of BETS?.bets || []) {
    if (b.book_id) ids.add(b.book_id);
  }
  if (ids.size) {
    return [...ids].map((id) => {
      const hit = LIVE_BOOKS.find((x) => x.id === id);
      return { id, label: hit?.label || id };
    });
  }
  return LIVE_BOOKS.map((b) => ({ id: b.id, label: b.label }));
}

function fillBookSelects() {
  const books = bookCatalog();
  for (const id of ["live-book", "hist-book", "an-book"]) {
    const sel = $(id);
    if (!sel) continue;
    const cur = sel.value;
    sel.innerHTML =
      `<option value="">All books</option>` +
      books.map((b) => `<option value="${b.id}">${b.label}</option>`).join("");
    if ([...sel.options].some((o) => o.value === cur)) sel.value = cur;
  }
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
  fillBookSelects();
}

function histFiltered() {
  const pass = new Set(passingMarkets());
  const mkt = $("hist-market").value;
  const side = $("hist-side").value;
  const ev = $("hist-event").value;
  const book = $("hist-book")?.value || "";
  return (BETS?.bets || []).filter((b) => {
    if (!pass.has(b.market)) return false;
    if (mkt && b.market !== mkt) return false;
    if (side && String(b.side).toLowerCase() !== side) return false;
    if (ev && b.event !== ev) return false;
    if (book && b.book_id !== book) return false;
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
      const bookLabel = b.book_label || b.book_id || "—";
      return `<tr>
        <td>${b.event}</td>
        <td class="num">${b.round ?? ""}</td>
        <td>${b.player || ""}</td>
        <td>${bookLabel}</td>
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

function bookPropPack(book) {
  if (!LIVE_PROPS) return {};
  const pre = LIVE_PROPS[`pre_round_${book.short}`];
  const live = LIVE_PROPS[`live_${book.short}`];
  if (pre && Object.keys(pre).length) return pre;
  return live || {};
}

function livePicks() {
  if (!PROJ?.players?.length) return [];
  const pass = passingMarkets();
  const mktFilter = $("live-market").value;
  const bookFilter = $("live-book")?.value || "";
  const gapMode = $("live-gap").value;
  const rnd = Math.round(Number(PROJ.display_round || 1)) || 1;
  const biasAlreadyApplied = Boolean(PROJ?.both_side_bias_applied?.at);
  const bias = biasAlreadyApplied ? {} : BETS?.live_bias || ROI?.live_bias || {};
  const out = [];
  const books = bookFilter
    ? LIVE_BOOKS.filter((b) => b.id === bookFilter)
    : LIVE_BOOKS;

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
      const gapOver = Number(rec.gap_over ?? rec.gap);
      const gapUnder = Number(rec.gap_under ?? rec.gap);
      const gapNeedOver = gapMode === "policy" ? gapOver : Number(gapMode);
      const gapNeedUnder = gapMode === "policy" ? gapUnder : Number(gapMode);
      const propMarket = PROP_MARKET[market];
      const key = `${dg}|${rnd}|${propMarket}`;

      for (const book of books) {
        const props = bookPropPack(book);
        const prop = props[key];
        const line = Number(prop?.line);
        const over = Number(prop?.over);
        const under = Number(prop?.under);
        if (!Number.isFinite(line)) continue;
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
        const gapNeed = side === "OVER" ? gapNeedOver : gapNeedUnder;
        const edge = Math.abs(delta) - gapNeed;
        out.push({
          player: name,
          book_id: book.id,
          book_label: book.label,
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
    note.textContent = `${PROJ.event_name || "Live"} · R${PROJ.display_round || "?"} · μ corrected by chrono bias · all sportsbooks · only both-side+ markets`;
  }
  $("live-table").querySelector("tbody").innerHTML = picks.length
    ? picks
        .slice(0, 120)
        .map(
          (p) => `<tr>
        <td>${p.player}</td>
        <td>${p.book_label}</td>
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
    : `<tr><td colspan="9">No live props past policy gap for both-side markets.</td></tr>`;
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
  const book = $("an-book")?.value || "";
  return (BETS?.bets || []).filter((b) => {
    if (!pass.has(b.market)) return false;
    if (mkt && b.market !== mkt) return false;
    if (ev && b.event !== ev) return false;
    if (book && b.book_id !== book) return false;
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
  renderHoleProps();
}

function setTab(tab) {
  ACTIVE_TAB = tab === "hole-props" ? "hole-props" : "both-side";
  const both = $("panel-both-side");
  const hole = $("panel-hole-props");
  if (both) both.hidden = ACTIVE_TAB !== "both-side";
  if (hole) hole.hidden = ACTIVE_TAB !== "hole-props";
  document.querySelectorAll(".tab-btn").forEach((btn) => {
    btn.classList.toggle("active", btn.dataset.tab === ACTIVE_TAB);
  });
  const title = $("page-title");
  const lede = $("page-lede");
  if (ACTIVE_TAB === "hole-props") {
    if (title) title.textContent = "Hole Props";
    if (lede) {
      lede.textContent =
        "Hole score, winner & matchup (DraftKings) plus holes 10–18 / 16–17–18 (Underdog). Model μ from course hole average + strokes gained.";
    }
  } else {
    if (title) title.textContent = "Both-side edge";
    if (lede) {
      lede.textContent =
        "Only markets where OVER and UNDER both print historically. Gap + bias from walk-forward bake.";
    }
  }
  if (ACTIVE_TAB === "both-side") renderAnalytics();
}

function fmtProb(x) {
  if (!Number.isFinite(x)) return "—";
  return `${(x * 100).toFixed(1)}%`;
}

function fmtOdds(o) {
  if (!Number.isFinite(o)) return "—";
  return `${o > 0 ? "+" : ""}${o}`;
}

function holesLabel(r) {
  if (Array.isArray(r.holes) && r.holes.length > 1) {
    if (r.holes.length === 9 && r.holes[0] === 10) return "10–18";
    if (r.holes.length === 3 && r.holes[0] === 16) return "16–17–18";
    return r.holes.join(",");
  }
  if (Number.isFinite(Number(r.hole))) return String(r.hole);
  return "—";
}

function hpFiltered() {
  const mkt = $("hp-market")?.value || "";
  const book = $("hp-book")?.value || "";
  const hole = $("hp-hole")?.value || "";
  const minEdge = Number($("hp-min-edge")?.value || 0);
  const q = String($("hp-player")?.value || "")
    .trim()
    .toLowerCase();
  return (HOLE_PROPS?.projections || []).filter((r) => {
    if (mkt && r.market !== mkt) return false;
    if (book && r.book !== book) return false;
    if (hole) {
      const h = Math.round(Number(hole));
      if (Number.isFinite(Number(r.hole))) {
        if (Math.round(Number(r.hole)) !== h) return false;
      } else if (Array.isArray(r.holes)) {
        if (!r.holes.includes(h)) return false;
      } else return false;
    }
    if (Number.isFinite(minEdge) && minEdge > 0) {
      if (!(Number.isFinite(r.edge) && r.edge >= minEdge)) return false;
    }
    if (q && !String(r.player || "").toLowerCase().includes(q)) return false;
    return true;
  });
}

function fillHoleSelect() {
  const sel = $("hp-hole");
  if (!sel) return;
  const cur = sel.value;
  const holes = new Set();
  for (const r of HOLE_PROPS?.projections || []) {
    if (Number.isFinite(Number(r.hole))) holes.add(Math.round(Number(r.hole)));
    for (const h of r.holes || []) {
      if (Number.isFinite(Number(h))) holes.add(Math.round(Number(h)));
    }
  }
  const list = [...holes].sort((a, b) => a - b);
  sel.innerHTML =
    `<option value="">All</option>` + list.map((h) => `<option value="${h}">${h}</option>`).join("");
  if ([...sel.options].some((o) => o.value === cur)) sel.value = cur;
}

function renderHoleProps() {
  const note = $("hp-note");
  if (!HOLE_PROPS) {
    if (note) {
      note.textContent =
        "No live_hole_props.json yet — run npm run bake:hole-props (or push:live).";
    }
    const tbody = $("hp-table")?.querySelector("tbody");
    if (tbody) tbody.innerHTML = `<tr><td colspan="12">No hole props data.</td></tr>`;
    return;
  }

  fillHoleSelect();
  const cov = HOLE_PROPS.coverage || {};
  const meta = HOLE_PROPS.meta || {};
  if (note) {
    const dkE = HOLE_PROPS.odds?.dk_error ? ` · DK: ${HOLE_PROPS.odds.dk_error}` : "";
    const udE = HOLE_PROPS.odds?.ud_error ? ` · UD: ${HOLE_PROPS.odds.ud_error}` : "";
    note.textContent = `${HOLE_PROPS.event_name || "Event"} · R${HOLE_PROPS.round || "?"} · ${HOLE_PROPS.course_key || "course"} · model ${meta.model || "hole_avg+sg"} · DK ${meta.n_dk ?? 0} / UD ${meta.n_ud ?? 0} odds${dkE}${udE}`;
  }

  const nEl = $("hp-kpi-n");
  const eEl = $("hp-kpi-edge");
  const vEl = $("hp-kpi-ev");
  const cEl = $("hp-kpi-cov");
  if (nEl) nEl.textContent = String(meta.n_projections ?? HOLE_PROPS.projections?.length ?? "—");
  if (eEl) eEl.textContent = String(meta.n_positive_edge ?? "—");
  if (vEl) {
    const ev = meta.best_ev;
    vEl.textContent = Number.isFinite(ev) ? `${ev >= 0 ? "+" : ""}${(ev * 100).toFixed(1)}%` : "—";
    vEl.className = clsSigned(ev);
  }
  if (cEl) {
    cEl.textContent =
      cov.with_hole_history != null ? `${cov.with_hole_history}/${cov.players || "?"}` : "—";
  }

  const rows = hpFiltered();
  const tbody = $("hp-table")?.querySelector("tbody");
  if (!tbody) return;
  if (!rows.length) {
    tbody.innerHTML = `<tr><td colspan="12">No rows match filters.</td></tr>`;
    return;
  }
  tbody.innerHTML = rows
    .slice(0, 400)
    .map((r) => {
      const edge = Number(r.edge);
      const ev = Number(r.ev);
      return `<tr>
        <td>${r.player || ""}</td>
        <td>${r.book || ""}</td>
        <td>${r.market || ""}</td>
        <td class="num">${holesLabel(r)}</td>
        <td class="num">${Number.isFinite(Number(r.mu)) ? Number(r.mu).toFixed(2) : "—"}</td>
        <td class="num">${r.line == null || r.line === "" ? "—" : r.line}</td>
        <td class="num">${r.side || "—"}</td>
        <td class="num">${fmtProb(Number(r.model_prob))}</td>
        <td class="num">${fmtProb(Number(r.implied))}</td>
        <td class="num ${clsSigned(edge)}">${Number.isFinite(edge) ? `${(edge * 100).toFixed(1)}%` : "—"}</td>
        <td class="num ${clsSigned(ev)}">${Number.isFinite(ev) ? `${(ev * 100).toFixed(1)}%` : "—"}</td>
        <td class="num">${fmtOdds(Number(r.odds))}</td>
      </tr>`;
    })
    .join("");
}

async function boot() {
  const err = $("error");
  err.hidden = true;
  try {
    [ROI, BETS, PROJ, LIVE_PROPS, HOLE_PROPS] = await Promise.all([
      loadJson(ROI_URL),
      loadJson(BETS_URL),
      loadJson(PROJ_URL).catch(() => null),
      loadJson(LIVE_PROPS_URL).catch(() => null),
      loadJson(HOLE_PROPS_URL).catch(() => null),
    ]);
    renderAll();
  } catch (e) {
    err.hidden = false;
    err.textContent = String(e?.message || e);
  }
}

$("btn-reload")?.addEventListener("click", () => boot());
document.querySelectorAll(".tab-btn").forEach((btn) => {
  btn.addEventListener("click", () => setTab(btn.dataset.tab));
});
for (const id of ["hist-market", "hist-side", "hist-event", "hist-book"]) {
  $(id)?.addEventListener("change", () => renderHist());
}
for (const id of ["live-market", "live-gap", "live-book"]) {
  $(id)?.addEventListener("change", () => renderLive());
}
for (const id of ["an-market", "an-event", "an-book"]) {
  $(id)?.addEventListener("change", () => renderAnalytics());
}
$("an-bankroll")?.addEventListener("change", () => renderAnalytics());
$("an-bankroll")?.addEventListener("input", () => renderAnalytics());
for (const id of ["hp-market", "hp-book", "hp-hole", "hp-min-edge"]) {
  $(id)?.addEventListener("change", () => renderHoleProps());
}
$("hp-player")?.addEventListener("input", () => renderHoleProps());

const hash = String(location.hash || "").replace(/^#/, "");
if (hash === "hole-props") setTab("hole-props");

boot();
