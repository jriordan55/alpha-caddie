const $ = (s, r = document) => r.querySelector(s);
const $$ = (s, r = document) => [...r.querySelectorAll(s)];

const SORT_COLUMNS = [
  { id: "selection", label: "Selection", type: "text", get: (r) => r.display || r.selection || "" },
  { id: "expected_points", label: "Expected Points", type: "num", get: (r) => r.expected_points },
  { id: "model_pct", label: "Model", type: "num", get: (r) => r.model_pct },
  { id: "dk_pct", label: "DK Implied", type: "num", get: (r) => r.dk_pct },
  { id: "model_odds", label: "Model Odds", type: "num", get: (r) => r.model_odds },
  { id: "dk_odds", label: "DK Odds", type: "num", get: (r) => r.dk_odds },
  { id: "edge_pct", label: "Edge", type: "num", get: (r) => r.edge_pct },
  { id: "ev_per_unit", label: "EV/u", type: "num", get: (r) => r.ev_per_unit },
];

function fmtPct(v, d = 1) {
  if (v == null || !Number.isFinite(v)) return "—";
  return `${v.toFixed(d)}%`;
}

function fmtPts(v) {
  if (v == null || !Number.isFinite(v)) return "—";
  return v.toFixed(2);
}

function fmtAm(v) {
  if (v == null) return "—";
  if (typeof v === "string") {
    const s = v.trim().replace(/\u2212|\u2013/g, "-");
    return s || "—";
  }
  if (!Number.isFinite(v)) return "—";
  const n = Math.round(v);
  return n > 0 ? `+${n}` : String(n);
}

function fmtDkOdds(row) {
  if (row.dk_odds_display != null && String(row.dk_odds_display).trim()) {
    return fmtAm(row.dk_odds_display);
  }
  return fmtAm(row.dk_odds);
}

function edgeCls(e) {
  if (e == null || !Number.isFinite(e)) return "edge-neu";
  if (e >= 2) return "edge-pos";
  if (e <= -2) return "edge-neg";
  return "edge-neu";
}

function teamBadgeForRow(row, badges) {
  if (row.team === "USA") return badges.usa;
  if (row.team === "INT") return badges.int;
  const market = String(row.market || row.market_group || "").toLowerCase();
  if (/total usa|team usa\b|usa total|usa points/i.test(market)) return badges.usa;
  if (/total international|team int|international total|international points/i.test(market)) {
    return badges.int;
  }
  const sel = String(row.selection || "").trim();
  if (/^usa$/i.test(sel) || /^usa\s/i.test(sel)) return badges.usa;
  if (/^international$/i.test(sel) || /^international\s/i.test(sel)) return badges.int;
  return null;
}

function imgOrBadge(row, badges, logos) {
  if (row.headshot) return `<img src="${row.headshot}" alt="" loading="lazy" />`;
  const badge = teamBadgeForRow(row, badges);
  if (badge) {
    const cls = row.team === "INT" || /international/i.test(row.market || "") ? "badge-sm badge-int" : "badge-sm";
    return `<img class="${cls}" src="${badge}" alt="" />`;
  }
  if (logos?.event) return `<img class="badge-sm" src="${logos.event}" alt="" />`;
  return `<img class="badge-sm" src="${badges.int || badges.usa}" alt="" />`;
}

function columnsForRows(rows) {
  const showExp = rows.some((r) => r.show_expected_points);
  return showExp ? SORT_COLUMNS : SORT_COLUMNS.filter((c) => c.id !== "expected_points");
}

function renderRow(row, badges, logos, columns) {
  const badge = teamBadgeForRow(row, badges);
  const tag =
    row.team === "USA" || badge === badges.usa
      ? '<span class="tag tag-usa">USA</span>'
      : row.team === "INT" || badge === badges.int
        ? '<span class="tag tag-int">INT</span>'
        : "";
  const cells = columns.map((col) => {
    if (col.id === "selection") {
      return `<td><div class="sel-cell">${imgOrBadge(row, badges, logos)}<div><div class="sel-name">${row.display || row.selection}${tag}</div></div></div></td>`;
    }
    if (col.id === "expected_points") return `<td class="num">${fmtPts(row.expected_points)}</td>`;
    if (col.id === "model_pct") return `<td class="num">${fmtPct(row.model_pct)}</td>`;
    if (col.id === "dk_pct") {
      return `<td class="num">${row.dk_pct != null ? fmtPct(row.dk_pct) : '<span class="no-dk">—</span>'}</td>`;
    }
    if (col.id === "model_odds") return `<td class="num">${fmtAm(row.model_odds)}</td>`;
    if (col.id === "dk_odds") {
      return `<td class="num">${row.dk_pct != null ? fmtDkOdds(row) : '<span class="no-dk">—</span>'}</td>`;
    }
    if (col.id === "edge_pct") {
      return `<td class="num ${edgeCls(row.edge_pct)}">${row.edge_pct != null ? `${row.edge_pct >= 0 ? "+" : ""}${row.edge_pct.toFixed(1)}%` : "—"}</td>`;
    }
    if (col.id === "ev_per_unit") {
      return `<td class="num">${row.ev_per_unit != null ? row.ev_per_unit.toFixed(3) : "—"}</td>`;
    }
    return `<td class="num">—</td>`;
  });
  return `<tr>${cells.join("")}</tr>`;
}

function compareRows(a, b, col, dir) {
  const va = col.get(a);
  const vb = col.get(b);
  if (col.type === "text") {
    const sa = String(va || "").toLowerCase();
    const sb = String(vb || "").toLowerCase();
    return dir * sa.localeCompare(sb);
  }
  const na = va != null && Number.isFinite(va) ? va : null;
  const nb = vb != null && Number.isFinite(vb) ? vb : null;
  if (na == null && nb == null) return 0;
  if (na == null) return 1;
  if (nb == null) return -1;
  return dir * (na - nb);
}

function sortRowList(rows, colId, dir, columns) {
  const col = columns.find((c) => c.id === colId) || columns.find((c) => c.id === "edge_pct") || columns[0];
  return [...rows].sort((a, b) => compareRows(a, b, col, dir));
}

function mountSortableTable(container, rows, badges, logos, sortState = { col: "edge_pct", dir: -1 }) {
  const columns = columnsForRows(rows);
  const state = { ...sortState };
  if (!columns.some((c) => c.id === state.col)) state.col = "edge_pct";

  function renderTable() {
    const sorted = sortRowList(rows, state.col, state.dir, columns);
    const headerCells = columns.map((col) => {
      const active = state.col === col.id;
      const arrow = active ? (state.dir > 0 ? "▲" : "▼") : "↕";
      const cls = col.id === "selection" ? "sortable" : "num sortable";
      return `<th class="${cls}${active ? " sorted" : ""}" data-col="${col.id}">${col.label}<span class="sort-ind">${arrow}</span></th>`;
    }).join("");

    container.innerHTML = `<table>
      <thead><tr>${headerCells}</tr></thead>
      <tbody>${sorted.map((r) => renderRow(r, badges, logos, columns)).join("")}</tbody>
    </table>`;

    container.querySelectorAll("th.sortable").forEach((th) => {
      th.addEventListener("click", () => {
        const col = th.dataset.col;
        if (state.col === col) state.dir *= -1;
        else {
          state.col = col;
          state.dir = col === "selection" ? 1 : -1;
        }
        renderTable();
      });
    });
  }

  renderTable();
}

function renderGroupedTables(rows, badges, logos) {
  if (!rows?.length) return `<p class="loading">No lines in this market.</p>`;
  const groups = new Map();
  for (const r of rows) {
    const g = r.market_group || r.dk_market || "Market";
    if (!groups.has(g)) groups.set(g, []);
    groups.get(g).push(r);
  }

  const wrap = document.createElement("div");
  wrap.className = "market-groups";
  for (const [name, groupRows] of groups) {
    const group = document.createElement("div");
    group.className = "market-group";
    group.innerHTML = `<h3 class="market-group-title">${name}</h3>`;
    const tblWrap = document.createElement("div");
    tblWrap.className = "tbl-wrap";
    group.appendChild(tblWrap);
    mountSortableTable(tblWrap, groupRows, badges, logos, { col: "edge_pct", dir: -1 });
    wrap.appendChild(group);
  }
  return wrap;
}

function renderCupBar(data) {
  const c = data.cup;
  const usaW = c.usa_implied;
  const intW = c.int_win;
  const tieW = c.tie;
  return `
    <div class="cup-bar-inner">
      <div class="team-side usa">
        <img class="team-badge team-badge-usa" src="${data.badges.usa}" alt="USA" />
        <div class="team-label">Team USA</div>
        <div class="prob-box usa">${usaW.toFixed(1)}%</div>
      </div>
      <div class="cup-center">
        <div class="scoreline">0 — 0</div>
        <div class="tie-box">${tieW.toFixed(1)}% tie</div>
        <div class="sub">15.5 pts to win</div>
      </div>
      <div class="team-side int">
        <img class="team-badge team-badge-int" src="${data.badges.int}" alt="International" />
        <div class="team-label">International</div>
        <div class="prob-box int">${intW.toFixed(1)}%</div>
      </div>
    </div>
    <div class="progress-wrap">
      <div class="progress-usa" style="width:${usaW}%"></div>
      <div class="progress-int" style="width:${intW}%"></div>
    </div>`;
}

function renderKpis(data) {
  const top = data.top_scorers[0];
  const likely = data.score_bands[0];
  return `
    <div class="kpi"><div class="lbl">Most likely score</div><div class="val">${likely?.score || "—"}</div></div>
    <div class="kpi"><div class="lbl">Top scorer (model)</div><div class="val">${top?.name?.split(" ")[0] || "—"}</div></div>
    <div class="kpi"><div class="lbl">USA μ SG</div><div class="val">${data.cup.usa_skill.toFixed(2)}</div></div>
    <div class="kpi"><div class="lbl">INT μ SG</div><div class="val">${data.cup.int_skill.toFixed(2)}</div></div>`;
}

async function init() {
  const root = $("#app");
  root.innerHTML = `<div class="loading">Loading model…</div>`;
  try {
    const data = await fetch("/api/model.json").then((r) => r.json());

    const eventLogo = data.logos?.event || data.logos?.full || "/assets/pc-logo-full.png";
    const logos = { event: eventLogo };
    root.innerHTML = `
      <header class="site-header">
        <div class="brand">
          <img class="pc-logo" src="${eventLogo}" alt="Presidents Cup 2026" />
          <div class="brand-meta">
            <div class="brand-course">${data.course}</div>
            <div class="brand-dates">${data.dates}</div>
          </div>
        </div>
        <div class="header-teams">
          <div class="header-team">
            <img class="team-badge-usa" src="${data.badges.usa}" alt="USA" />
            <span>USA</span>
          </div>
          <span class="header-vs">vs</span>
          <div class="header-team">
            <img class="team-badge-int" src="${data.badges.int}" alt="International" />
            <span>INT</span>
          </div>
        </div>
      </header>

      <section class="cup-bar">${renderCupBar(data)}</section>
      <section class="kpi-strip">${renderKpis(data)}</section>

      <nav class="tabs" id="tabs"></nav>
      <main class="main" id="panels"></main>`;

    const tabsEl = $("#tabs");
    const panelsEl = $("#panels");

    data.tabs.forEach((tab, i) => {
      const btn = document.createElement("button");
      btn.className = `tab${i === 0 ? " active" : ""}`;
      btn.textContent = tab.label;
      btn.dataset.id = tab.id;
      tabsEl.appendChild(btn);

      const panel = document.createElement("div");
      panel.className = `panel${i === 0 ? " active" : ""}`;
      panel.id = `panel-${tab.id}`;
      const withModel = tab.rows.filter((r) => r.model_pct != null).length;
      const hint = `${tab.market_count || 0} markets · ${tab.rows.length} lines · ${withModel} modeled`;

      const head = document.createElement("div");
      head.className = "panel-head";
      head.innerHTML = `<h2>${tab.label}</h2><span class="hint">${hint}</span>`;
      panel.appendChild(head);
      panel.appendChild(renderGroupedTables(tab.rows, data.badges, logos));
      panelsEl.appendChild(panel);

      btn.addEventListener("click", () => {
        $$(".tab").forEach((t) => t.classList.remove("active"));
        $$(".panel").forEach((p) => p.classList.remove("active"));
        btn.classList.add("active");
        panel.classList.add("active");
      });
    });
  } catch (e) {
    root.innerHTML = `<div class="loading">Failed to load: ${e.message}</div>`;
  }
}

init();
