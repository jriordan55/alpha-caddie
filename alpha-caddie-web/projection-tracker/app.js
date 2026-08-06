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
  for (const id of ["live-market", "hist-market"]) {
    const sel = $(id);
    const cur = sel.value;
    sel.innerHTML =
      `<option value="">${id === "live-market" ? "Both-side markets only" : "All passing markets"}</option>` +
      pass.map((m) => `<option value="${m}">${m}</option>`).join("");
    if ([...sel.options].some((o) => o.value === cur)) sel.value = cur;
  }
  const events = [...new Set((BETS?.bets || []).map((b) => b.event))].sort();
  const es = $("hist-event");
  const evCur = es.value;
  es.innerHTML =
    `<option value="">All events</option>` + events.map((e) => `<option value="${e}">${e}</option>`).join("");
  if ([...es.options].some((o) => o.value === evCur)) es.value = evCur;
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

function renderAll() {
  renderHero();
  renderMarketTable();
  fillMarketSelects();
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

boot();
