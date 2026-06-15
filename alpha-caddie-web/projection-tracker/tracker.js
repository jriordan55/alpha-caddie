/**
 * Projection tracker — round_projection_vs_actual_summary.csv
 * npm run projection-tracker  →  http://localhost:5173/projection-tracker/
 */
const CSV_CANDIDATES = [
  "../data/round_projection_vs_actual_summary.csv",
  "../data/round_projection_vs_actual_summary.csv.new",
];

const MARKET_ORDER = [
  "Total score",
  "Birdies",
  "GIR",
  "Fairways hit",
  "Pars",
  "Bogeys",
];

/** @type {Record<string, string>[]} */
let ALL_ROWS = [];

const state = {
  tab: "overview",
  /** "" = all tournaments combined; otherwise event name */
  tournament: "",
  market: "",
  minEv: 5,
  side: "",
};

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
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
  return s === "model_vs_book" || s === "ev_backtest";
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

function evRows() {
  return activeRows()
    .filter((r) => r.section === "ev_backtest")
    .filter((r) => {
      const th = num(r.ev_threshold_pct);
      if (!Number.isFinite(th) || th < state.minEv) return false;
      if (state.side && r.bet_side !== state.side) return false;
      return num(r.bets) > 0;
    })
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
  for (const r of rows) {
    if (r.section !== "model_vs_book") continue;
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
    if (r.section !== "ev_backtest") continue;
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
  const evOnly = rows.filter((r) => r.section === "ev_backtest" && num(r.bets) > 0);
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
      let best = NaN;
      for (const c of evOnly.filter((r) => r.market === m && num(r.ev_threshold_pct) === th)) {
        const roi = num(c.roi_pct);
        if (!Number.isFinite(roi)) continue;
        if (!Number.isFinite(best) || roi > best) best = roi;
      }
      cell.set(`${m}\x1f${th}`, best);
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
  const ev = evRows();
  const evAgg = aggregateEvByMarketSide(ev);
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
      <div class="kpi-sub">${totalBets} bets · ≥${state.minEv}% edge</div>
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
    bestEvPerMarket(ev).map((a) => ({ market: `${a.market} (${a.side})`, roi: a.roi })),
    { valueKey: "roi", format: (v) => fmtPct(v) },
  );
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
      if (r.section !== "ev_backtest") return false;
      const th = num(r.ev_threshold_pct);
      return Number.isFinite(th) && th >= state.minEv && num(r.bets) > 0;
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
      const ts = rows.find((r) => r.section === "model_vs_book" && r.market === "Total score");
      const evEv = rows.filter(
        (r) => r.section === "ev_backtest" && num(r.ev_threshold_pct) >= 5 && num(r.bets) > 0,
      );
      const units = evEv.reduce((s, r) => s + (num(r.units_net) || 0), 0);
      const bets = evEv.reduce((s, r) => s + (num(r.bets) || 0), 0);
      const roi = bets > 0 ? (units / bets) * 100 : NaN;
      const course = rows.find((r) => r.section === "model_vs_book")?.course_used || "";
      const exp = latestExportForTournament(name);
      const exportLabel = exp ? new Date(exp).toLocaleDateString() : "";
      const selected = state.tournament === name ? " selected" : "";
      return `<article class="event-card${selected}" data-tournament="${name.replace(/"/g, "&quot;")}">
        <h3>${name}</h3>
        <div class="course">${course}${exportLabel ? ` · ${exportLabel}` : ""}</div>
        <div class="event-metrics">
          <div><span>Score RMSE</span><strong>${fmt(num(ts?.rmse), 2)}</strong></div>
          <div><span>Score MAE</span><strong>${fmt(num(ts?.mae), 2)}</strong></div>
          <div><span>EV units (5%+)</span><strong class="${clsSigned(units)}">${units >= 0 ? "+" : ""}${fmt(units, 1)}u</strong></div>
          <div><span>ROI (5%+)</span><strong class="${clsSigned(roi)}">${fmtPct(roi)}</strong></div>
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
  const ev = evRows();
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
  mSel.value = prev && markets.includes(prev) ? prev : "";
  state.market = mSel.value;
}

function selectTournament(name) {
  state.tournament = name;
  document.getElementById("filter-tournament").value = name;
  populateMarketFilter();
  renderAll();
}

function renderAll() {
  renderHeader();
  renderOverview();
  renderAccuracy();
  renderEv();
  renderEvents();
  buildInsights();
}

function setTab(name) {
  state.tab = name;
  document.querySelectorAll(".tab").forEach((t) => {
    t.classList.toggle("active", t.getAttribute("data-tab") === name);
  });
  document.querySelectorAll(".panel").forEach((p) => {
    p.classList.toggle("active", p.id === `panel-${name}`);
  });
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

async function loadData() {
  const errEl = document.getElementById("error-banner");
  errEl.hidden = true;
  try {
    ALL_ROWS = parseCsv(await loadSummaryCsvText());
    if (!ALL_ROWS.length) throw new Error("CSV is empty");
    populateTournamentSelect();
    populateMarketFilter();
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
  document.getElementById("btn-reload").addEventListener("click", loadData);
}

bindUi();
loadData();
