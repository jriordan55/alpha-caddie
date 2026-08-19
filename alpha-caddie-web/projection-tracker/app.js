/**
 * Model vs market tracker — projection μ performance against sportsbook lines.
 */
import { sgAllowedSides } from "../scripts/sg-side-policy.mjs";

const ROI_URL = "../data/both_side_roi.json";
const BETS_URL = "../data/both_side_bets.json";
const PROJ_URL = "../projections.json";
const LIVE_PROPS_URL = "../data/live_event_book_props.json";
const HOLE_PROPS_URL = "../data/live_hole_props.json";
const PREV_SG_URL = "../data/prev_round_sg_index.json";

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

const MARKET_ORDER = [
  "Total score",
  "Pars",
  "Bogeys",
  "GIR",
  "Birdies",
  "Fairways hit",
];

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
/** @type {Record<string, { prev_sg_ott?: number, prev_sg_app?: number, prev_sg_putt?: number }>} */
let PREV_SG_INDEX = {};
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

/** All markets with a recommended gap policy (not both-side-only). */
function trackedMarkets() {
  const fromRoi = Array.isArray(ROI?.overall?.markets) ? ROI.overall.markets : [];
  if (fromRoi.length) return fromRoi.filter((m) => ROI?.recommended?.[m]);
  const rec = ROI?.recommended || {};
  const keys = Object.keys(rec);
  if (keys.length) return MARKET_ORDER.filter((m) => rec[m]).concat(keys.filter((m) => !MARKET_ORDER.includes(m)));
  return [...new Set((BETS?.bets || []).map((b) => b.market).filter(Boolean))];
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
  const markets = trackedMarkets();
  $("kpi-markets").textContent = String(markets.length || "—");
  const pnl = o.recommended_combined_pnl;
  const el = $("kpi-pnl");
  el.textContent = fmtMoney(pnl);
  el.className = clsSigned(pnl);
  $("kpi-bets").textContent = String(o.recommended_combined_bets ?? "—");
  let combRoi = o.recommended_combined_roi;
  if (!Number.isFinite(combRoi) && Number.isFinite(pnl) && o.recommended_combined_bets > 0) {
    combRoi = pnl / (o.recommended_combined_bets * STAKE);
  }
  const mr = $("kpi-minroi");
  if (!Number.isFinite(combRoi)) {
    mr.textContent = "—";
    mr.className = "";
  } else {
    mr.textContent = fmtPct(combRoi);
    mr.className = clsSigned(combRoi);
  }
}

function gapLabelFor(r) {
  const base =
    r.gap_over != null && r.gap_under != null && r.gap_over !== r.gap_under
      ? `${r.gap_over}/${r.gap_under}`
      : typeof r.gap === "object"
        ? `${r.gap.over}/${r.gap.under}`
        : String(r.gap ?? "—");
  const bits = [];
  if (r.odds_rule?.under_min_american != null) bits.push(`U≥${r.odds_rule.under_min_american}`);
  if (r.odds_rule?.over_min_american != null) bits.push(`O≥${r.odds_rule.over_min_american}`);
  return bits.length ? `${base} · ${bits.join(" ")}` : base;
}

function combinedRoiFor(r) {
  if (Number.isFinite(r?.combined_roi)) return r.combined_roi;
  const bets = Number(r?.combined_bets);
  const pnl = Number(r?.combined_pnl);
  if (bets > 0 && Number.isFinite(pnl)) return pnl / (bets * STAKE);
  return NaN;
}

function renderMarketTable() {
  const tbody = $("market-table").querySelector("tbody");
  const rec = ROI?.recommended || {};
  const order = MARKET_ORDER.filter((m) => rec[m]).concat(
    Object.keys(rec).filter((m) => !MARKET_ORDER.includes(m)),
  );
  tbody.innerHTML = order
    .map((m) => {
      const r = rec[m];
      const comb = combinedRoiFor(r);
      return `<tr>
        <td>${m}</td>
        <td class="num">${gapLabelFor(r)}</td>
        <td class="num">${r.over?.bets ?? "—"}</td>
        <td class="num ${clsSigned(r.over?.roi)}">${fmtPct(r.over?.roi)}</td>
        <td class="num">${r.under?.bets ?? "—"}</td>
        <td class="num ${clsSigned(r.under?.roi)}">${fmtPct(r.under?.roi)}</td>
        <td class="num ${clsSigned(comb)}">${fmtPct(comb)}</td>
        <td class="num ${clsSigned(r.combined_pnl)}">${fmtMoney(r.combined_pnl)}</td>
      </tr>`;
    })
    .join("");
}

function fillMarketSelects() {
  const markets = trackedMarkets();
  for (const id of ["live-market", "hist-market", "an-market"]) {
    const sel = $(id);
    if (!sel) continue;
    const cur = sel.value;
    sel.innerHTML =
      `<option value="">All markets</option>` +
      markets.map((m) => `<option value="${m}">${m}</option>`).join("");
    if ([...sel.options].some((o) => o.value === cur)) sel.value = cur;
  }
  const events = [...new Set((BETS?.bets || []).map((b) => b.event).filter(Boolean))];
  const liveEv = String(BETS?.live_event || ROI?.excluded_live_event || "").trim();
  const order = ROI?.event_order || [];
  events.sort((a, b) => {
    if (liveEv) {
      const aLive = a === liveEv;
      const bLive = b === liveEv;
      if (aLive !== bLive) return aLive ? -1 : 1;
    }
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
  const tracked = new Set(trackedMarkets());
  const mkt = $("hist-market").value;
  const side = $("hist-side").value;
  const ev = $("hist-event").value;
  const book = $("hist-book")?.value || "";
  return (BETS?.bets || []).filter((b) => {
    if (tracked.size && !tracked.has(b.market)) return false;
    if (mkt && b.market !== mkt) return false;
    if (side && String(b.side).toLowerCase() !== side) return false;
    if (ev && b.event !== ev) return false;
    if (book && b.book_id !== book) return false;
    return true;
  });
}

function renderHist() {
  // Newest first so Fairways-first bake order doesn't fill the visible window alone.
  const rows = sortBetsChrono(histFiltered()).reverse();
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
  const show = rows.slice(0, 800);
  const note = $("hist-table-note");
  if (note) {
    const liveN = rows.filter((b) => b.live_week).length;
    const liveLabel = BETS?.live_event ? ` · ${liveN} ${BETS.live_event}` : liveN ? ` · ${liveN} live-week` : "";
    note.textContent =
      rows.length > show.length
        ? `Showing ${show.length} of ${rows.length} graded bets (newest first, all markets mixed)${liveLabel}. Filter by market/event to focus.`
        : `${rows.length} graded bets (newest first)${liveLabel}.`;
  }
  $("hist-table").querySelector("tbody").innerHTML = show.length
    ? show
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
        <td class="muted sg-reason" title="${b.sg_reason || ""}">${b.sg_reason || "—"}</td>
      </tr>`;
        })
        .join("")
    : `<tr><td colspan="13">No graded bets for this filter.</td></tr>`;
}

function priorSgForLivePick(eventName, dgId, roundNum) {
  const k = `${eventName}|${Math.round(Number(dgId))}|${Math.round(Number(roundNum))}`;
  const hit = PREV_SG_INDEX[k] || {};
  return {
    prev_sg_ott: hit.prev_sg_ott,
    prev_sg_app: hit.prev_sg_app,
    prev_sg_putt: hit.prev_sg_putt,
    prev_gir_pct: hit.prev_gir_pct,
    prev_bob_pct: hit.prev_bob_pct,
  };
}

function liveSideAllowedBySg(market, side, signals) {
  const allowed = sgAllowedSides(market, signals);
  const s = String(side || "").toUpperCase();
  if (s === "OVER") return allowed.over;
  if (s === "UNDER") return allowed.under;
  return false;
}

function bookPropPack(book) {
  if (!LIVE_PROPS) return {};
  const pre = LIVE_PROPS[`pre_round_${book.short}`] || {};
  const live = LIVE_PROPS[`live_${book.short}`] || {};
  const rnd = Math.round(Number(PROJ?.display_round || 1)) || 1;
  const packHasRound = (pack, r) =>
    Object.keys(pack).some((k) => {
      const parts = String(k).split("|");
      return parts.length >= 3 && parts[1] === String(r);
    });
  // Prefer packs that match display_round (frozen pre_round R1 must not hide Sunday live lines).
  if (packHasRound(live, rnd)) return live;
  if (packHasRound(pre, rnd)) return pre;
  if (Object.keys(live).length) return live;
  return pre;
}

function livePicks() {
  if (!PROJ?.players?.length) return [];
  const markets = trackedMarkets();
  const mktFilter = $("live-market").value;
  const bookFilter = $("live-book")?.value || "";
  const gapMode = $("live-gap").value;
  const rnd = Math.round(Number(PROJ.display_round || 1)) || 1;
  const out = [];
  const books = bookFilter
    ? LIVE_BOOKS.filter((b) => b.id === bookFilter)
    : LIVE_BOOKS;

  for (const p of PROJ.players) {
    if (Math.round(Number(p.round)) !== rnd) continue;
    const dg = Math.round(Number(p.dg_id));
    const name = String(p.player_name || "");
    for (const market of markets) {
      if (mktFilter && market !== mktFilter) continue;
      const rec = ROI.recommended?.[market];
      if (!rec) continue;
      const mu = MARKET_TO_PLAYER[market]?.(p);
      if (!Number.isFinite(mu)) continue;
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
        const priorSg = priorSgForLivePick(PROJ.event_name, dg, rnd);
        const sgAllowed = sgAllowedSides(market, priorSg);
        if (!liveSideAllowedBySg(market, side, priorSg)) continue;
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
          sg_reason: sgAllowed.reason || "",
        });
      }
    }
  }
  out.sort((a, b) => b.edge - a.edge);
  return out;
}

function esc(s) {
  return String(s ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/"/g, "&quot;");
}

function renderLive() {
  const picks = livePicks();
  const note = $("live-note");
  if (!PROJ) {
    note.textContent = "No projections.json — run refresh:live / apply:dg-methodology.";
  } else {
    note.textContent = `${PROJ.event_name || "Live"} · R${PROJ.display_round || "?"} · model μ vs sportsbook lines · prior-rnd SG side filter · all markets`;
  }
  $("live-table").querySelector("tbody").innerHTML = picks.length
    ? picks
        .slice(0, 120)
        .map((p) => `<tr title="${p.sg_reason ? esc(p.sg_reason) : ""}">
        <td>${p.player}</td>
        <td>${p.book_label}</td>
        <td>${p.market}</td>
        <td class="num">${p.side}</td>
        <td class="num">${p.mu.toFixed(1)}</td>
        <td class="num">${p.line}</td>
        <td class="num ${clsSigned(p.gap)}">${p.gap > 0 ? "+" : ""}${p.gap.toFixed(2)}</td>
        <td class="num">${p.odds > 0 ? "+" : ""}${p.odds}</td>
        <td class="num">${p.edge.toFixed(2)}</td>
        <td class="muted sg-reason">${p.sg_reason ? esc(p.sg_reason) : "—"}</td>
      </tr>`,
        )
        .join("")
    : `<tr><td colspan="10">No live props past gap + prior-round SG side rules.</td></tr>`;
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
  const tracked = new Set(trackedMarkets());
  const mkt = $("an-market")?.value || "";
  const ev = $("an-event")?.value || "";
  const book = $("an-book")?.value || "";
  return (BETS?.bets || []).filter((b) => {
    if (tracked.size && !tracked.has(b.market)) return false;
    if (mkt && b.market !== mkt) return false;
    if (ev && b.event !== ev) return false;
    if (book && b.book_id !== book) return false;
    return true;
  });
}

function sortBetsChrono(bets) {
  return [...bets].sort((a, b) => {
    // Live-week grades after hist when timestamps missing/equal (newest-first UI reverses this).
    const la = Boolean(a.live_week);
    const lb = Boolean(b.live_week);
    if (la !== lb) return la ? 1 : -1;
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
    if (title) title.textContent = "Model vs market";
    if (lede) {
      lede.textContent =
        "How model μ performs vs sportsbook lines at each market’s recommended gap. Flat $100 · raw hierarchical μ (weather + tee wave).";
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
    const [roi, bets, proj, liveProps, holeProps, prevSgPayload] = await Promise.all([
      loadJson(ROI_URL),
      loadJson(BETS_URL),
      loadJson(PROJ_URL).catch(() => null),
      loadJson(LIVE_PROPS_URL).catch(() => null),
      loadJson(HOLE_PROPS_URL).catch(() => null),
      loadJson(PREV_SG_URL).catch(() => null),
    ]);
    ROI = roi;
    BETS = bets;
    PROJ = proj;
    LIVE_PROPS = liveProps;
    HOLE_PROPS = holeProps;
    PREV_SG_INDEX = prevSgPayload?.index || {};
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
