import "./styles.css";
import {
  displayRoundAuto,
  displayRoundLabel,
  filterByRound,
  flagImg,
  loadProjections,
  nameDisplay,
  type ProjectionPlayer,
  type ProjectionsFile,
} from "./lib/projections";
import { fetchJson, type FieldUpdates } from "./api/datagolf";
import { LB_OU_LINES, modelCountOuHtml, modelTotalScoreOuHtml } from "./lib/odds";

const TZ = "America/New_York";

let projectionsCache: ProjectionsFile | null = null;
let fieldCache: FieldUpdates | null = null;

function el<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  cls?: string,
  text?: string,
): HTMLElementTagNameMap[K] {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
}

function teetimeForRound(p: Record<string, unknown>, rn: number): string {
  const keys = [`r${rn}_teetime`, `R${rn}_teetime`, `r${rn}_tee_time`];
  for (const k of keys) {
    const v = p[k];
    if (typeof v === "string" && v.trim()) return v.trim();
  }
  const tu = p.teetime_upcoming;
  if (typeof tu === "string" && tu.trim()) return tu.trim();
  return "";
}

function mergeFieldAndProjections(
  field: FieldUpdates | null,
  projRows: ProjectionPlayer[],
  roundNum: number,
): Array<ProjectionPlayer & { GolferHtml?: string; teetime_upcoming?: string }> {
  if (!field?.field || !Array.isArray(field.field) || field.field.length === 0) {
    return projRows.map((r) => ({
      ...r,
      GolferHtml: `${flagImg(r.country)}<span class="golfer-name">${escapeHtml(nameDisplay(r.player_name))}</span>`,
    }));
  }
  const byId = new Map<number, ProjectionPlayer>();
  for (const r of projRows) {
    if (r.dg_id != null) byId.set(Number(r.dg_id), r);
  }
  const out: Array<ProjectionPlayer & { GolferHtml?: string }> = [];
  for (const raw of field.field) {
    const p = raw as Record<string, unknown>;
    const name = String(p.player_name ?? p.name ?? "").trim();
    if (!name) continue;
    const dg = p.dg_id != null ? Number(p.dg_id) : NaN;
    const tt = teetimeForRound(p, roundNum);
    const country = String(p.country ?? "");
    const pr = Number.isFinite(dg) ? byId.get(dg) : undefined;
    const row: ProjectionPlayer & { GolferHtml?: string; teetime_upcoming?: string } = {
      player_name: name,
      dg_id: Number.isFinite(dg) ? dg : pr?.dg_id,
      country: country || pr?.country,
      teetime_upcoming: tt || pr?.teetime_upcoming,
      total_score: pr?.total_score,
      round_sd: pr?.round_sd,
      score_to_par: pr?.score_to_par,
      gir: pr?.gir,
      fairways: pr?.fairways,
      birdies: pr?.birdies,
      pars: pr?.pars,
      bogeys: pr?.bogeys,
      doubles: pr?.doubles,
      eagles: pr?.eagles,
      GolferHtml: `${flagImg(country || pr?.country)}<span class="golfer-name">${escapeHtml(nameDisplay(name))}</span>`,
    };
    out.push(row);
  }
  return out;
}

function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function renderModelOuTable(
  container: HTMLElement,
  stat: string,
  rows: Array<ProjectionPlayer & { GolferHtml?: string; teetime_upcoming?: string }>,
  roundNum: number,
) {
  container.innerHTML = "";
  if (rows.length === 0) {
    container.appendChild(el("p", "message-box", "No rows. Add public/data/projections.json or click Refresh after configuring the API proxy."));
    return;
  }
  const lines = LB_OU_LINES[stat] ?? LB_OU_LINES["Total score"]!;
  const table = el("table", "data");
  const thead = el("thead");
  const hr = el("tr");
  hr.appendChild(th("Golfer", false));
  hr.appendChild(th("Median", true));
  for (const L of lines) hr.appendChild(th(String(L), true));
  if (rows.some((r) => r.teetime_upcoming)) hr.appendChild(th("Tee time", true));
  thead.appendChild(hr);
  table.appendChild(thead);
  const tb = el("tbody");

  const colMap: Record<string, keyof ProjectionPlayer> = {
    Birdies: "birdies",
    Pars: "pars",
    Bogeys: "bogeys",
    GIR: "gir",
    "Fairways hit": "fairways",
  };

  const sorted = [...rows].sort((a, b) => {
    if (stat === "Total score") {
      const ma = Number(a.total_score);
      const mb = Number(b.total_score);
      if (Number.isFinite(ma) && Number.isFinite(mb)) return ma - mb;
    }
    if (stat === "Bogeys") {
      const la = Number(a.bogeys);
      const lb = Number(b.bogeys);
      if (Number.isFinite(la) && Number.isFinite(lb)) return la - lb;
    }
    const key = colMap[stat];
    if (key) {
      const la = Number(a[key]);
      const lb = Number(b[key]);
      if (Number.isFinite(la) && Number.isFinite(lb)) return lb - la;
    }
    return String(a.player_name).localeCompare(String(b.player_name));
  });

  for (const r of sorted) {
    const tr = el("tr");
    const tdG = el("td");
    tdG.innerHTML = `<span class="golfer-cell">${r.GolferHtml ?? escapeHtml(r.player_name)}</span>`;
    tr.appendChild(tdG);

    if (stat === "Total score") {
      const mu = Number(r.total_score);
      const sd = Number(r.round_sd);
      const med = Number.isFinite(mu) ? mu.toFixed(2) : "—";
      tr.appendChild(td(med, true));
      for (const L of lines) {
        const tdv = el("td");
        tdv.className = "dt-center";
        tdv.innerHTML = modelTotalScoreOuHtml(mu, sd, L);
        tr.appendChild(tdv);
      }
    } else {
      const key = colMap[stat] ?? "birdies";
      const lam = Number(r[key]);
      const med = Number.isFinite(lam) ? String(Math.round(medianPoisson(lam))) : "—";
      tr.appendChild(td(med, true));
      for (const L of lines) {
        const tdv = el("td");
        tdv.className = "dt-center";
        tdv.innerHTML = modelCountOuHtml(lam, L);
        tr.appendChild(tdv);
      }
    }
    if (rows.some((x) => x.teetime_upcoming)) {
      tr.appendChild(td(r.teetime_upcoming ? formatTeetime(r.teetime_upcoming) : "—", true));
    }
    tb.appendChild(tr);
  }
  table.appendChild(tb);
  container.appendChild(table);
}

function th(text: string, center: boolean): HTMLTableCellElement {
  const x = document.createElement("th");
  x.textContent = text;
  if (center) x.className = "dt-center";
  return x;
}

function td(text: string, center: boolean): HTMLTableCellElement {
  const x = document.createElement("td");
  x.textContent = text;
  if (center) x.className = "dt-center";
  return x;
}

function medianPoisson(lambda: number): number {
  if (!Number.isFinite(lambda) || lambda <= 0) return 0;
  let k = 0;
  let cdf = Math.exp(-lambda);
  const target = 0.5;
  while (cdf < target && k < 100) {
    k += 1;
    cdf += (Math.exp(-lambda) * Math.pow(lambda, k)) / factorial(k);
  }
  return k;
}

function factorial(n: number): number {
  if (n <= 1) return 1;
  let r = 1;
  for (let i = 2; i <= n; i++) r *= i;
  return r;
}

function formatTeetime(s: string): string {
  const raw = s.trim();
  if (/^\d{4}-\d{2}-\d{2}[ tT]\d{1,2}:\d{2}/.test(raw)) {
    const d = new Date(raw.replace(" ", "T").replace(/Z$/i, "") + (raw.endsWith("Z") ? "" : "Z"));
    if (!Number.isNaN(d.getTime())) {
      return d.toLocaleTimeString("en-US", { hour: "numeric", minute: "2-digit", timeZone: TZ });
    }
  }
  return raw;
}

async function refreshField(): Promise<void> {
  const data = await fetchJson<FieldUpdates>("field-updates", { tour: "pga", file_format: "json" });
  fieldCache = data;
}

function mountApp() {
  const app = document.getElementById("app");
  if (!app) return;

  app.innerHTML = `
    <header class="app-nav">
      <span class="nav-title">AlphaCaddie</span>
      <p class="nav-subtitle">Your Caddie for Smarter Golf Bets</p>
    </header>
    <div class="content-wrapper">
      <nav class="tabs" id="tabs"></nav>
      <div id="panel-mu" class="panel active"></div>
      <div id="panel-props" class="panel"></div>
      <div id="panel-ev" class="panel"></div>
      <div id="panel-matchups" class="panel"></div>
      <div id="panel-outrights" class="panel"></div>
      <div id="panel-hole" class="panel"></div>
    </div>
  `;

  const tabsSpec: { id: string; label: string }[] = [
    { id: "mu", label: "Model O/U" },
    { id: "props", label: "Player Props" },
    { id: "ev", label: "+EV Bets" },
    { id: "matchups", label: "Round Matchups" },
    { id: "outrights", label: "Outrights" },
    { id: "hole", label: "Hole Hangout" },
  ];

  const tabsEl = document.getElementById("tabs")!;
  for (const t of tabsSpec) {
    const b = el("button", "", t.label);
    b.dataset.tab = t.id;
    if (t.id === "mu") b.classList.add("active");
    b.addEventListener("click", () => activateTab(t.id));
    tabsEl.appendChild(b);
  }

  function activateTab(id: string) {
    tabsEl.querySelectorAll("button").forEach((btn) => {
      btn.classList.toggle("active", (btn as HTMLButtonElement).dataset.tab === id);
    });
    document.querySelectorAll(".panel").forEach((p) => p.classList.remove("active"));
    document.getElementById(`panel-${id}`)?.classList.add("active");
    if (id === "hole") renderHolePanel();
  }

  const panelMu = document.getElementById("panel-mu")!;
  panelMu.innerHTML = `
    <div class="leaderboard-toolbar">
      <span class="leaderboard-toolbar-label" id="auto-round"></span>
      <span class="leaderboard-toolbar-label">Market</span>
      <select id="lb-stat">
        <option>Total score</option>
        <option>Birdies</option>
        <option>Pars</option>
        <option>Bogeys</option>
        <option>GIR</option>
        <option>Fairways hit</option>
      </select>
      <button type="button" class="btn-primary" id="btn-refresh">Refresh</button>
      <span class="status-text" id="status-mu"></span>
    </div>
    <h2 class="leaderboard-heading" id="mu-title">Model O/U</h2>
    <p class="text-muted" id="mu-note"></p>
    <div class="leaderboard-card" id="mu-table-wrap"></div>
  `;

  const lbStat = panelMu.querySelector("#lb-stat") as HTMLSelectElement;
  const statusMu = panelMu.querySelector("#status-mu") as HTMLSpanElement;
  const autoRound = panelMu.querySelector("#auto-round") as HTMLSpanElement;
  const muTitle = panelMu.querySelector("#mu-title") as HTMLHeadingElement;
  const muNote = panelMu.querySelector("#mu-note") as HTMLParagraphElement;
  const muWrap = panelMu.querySelector("#mu-table-wrap") as HTMLDivElement;

  function updateAutoRoundLabel() {
    const r = displayRoundAuto(new Date(), TZ);
    autoRound.textContent = ` ${displayRoundLabel(r, TZ)}`;
    return r;
  }

  async function runRefresh() {
    statusMu.textContent = " Loading…";
    await refreshField();
    projectionsCache = await loadProjections();
    updateAutoRoundLabel();
    renderMu();
    statusMu.textContent = projectionsCache
      ? ` Updated ${new Date().toLocaleTimeString()}`
      : " No projections.json — using example or empty.";
  }

  function renderMu() {
    const roundNum = updateAutoRoundLabel();
    const stat = lbStat.value;
    const data = projectionsCache;
    const projRows = data ? filterByRound(data, roundNum) : [];
    const rows = mergeFieldAndProjections(fieldCache, projRows, roundNum);
    const ev = fieldCache?.event_name ?? data?.event_name ?? "";
    const course = fieldCache?.course_name ?? "";
    const day =
      roundNum === 1 ? "Thursday" : roundNum === 2 ? "Friday" : roundNum === 3 ? "Saturday" : "Sunday";
    muTitle.textContent = `Model O/U — ${stat} — R${roundNum} (${day})${course ? ` · ${course}` : ev ? ` · ${ev}` : ""}`;
    muNote.textContent =
      stat === "Total score"
        ? "Normal total with continuity correction; SD from projected round_sd. American pairs use ~4.8% two-way hold."
        : "Counts: Poisson at projected means. American pairs use ~4.8% two-way hold.";
    renderModelOuTable(muWrap, stat, rows, roundNum);
  }

  lbStat.addEventListener("change", renderMu);

  async function refreshAll() {
    await runRefresh();
    fillPropGolfers();
    propEv();
  }

  panelMu.querySelector("#btn-refresh")!.addEventListener("click", () => void refreshAll());

  setInterval(() => {
    updateAutoRoundLabel();
    renderMu();
  }, 60_000);

  // Player Props (lightweight)
  const panelProps = document.getElementById("panel-props")!;
  panelProps.innerHTML = `
    <div class="props-card">
      <h2 class="leaderboard-heading">Player Props</h2>
      <p class="text-muted">Distribution preview: select golfer after loading projections.</p>
      <div class="props-grid" style="margin-bottom:16px;">
        <div><div class="props-label">Golfer</div><select id="prop-golfer"></select></div>
        <div><div class="props-label">Stat</div>
          <select id="prop-stat"><option>Total Score</option><option>Pars</option><option>Birdies</option><option>Bogeys</option></select>
        </div>
        <div><div class="props-label">Line</div><input type="number" id="prop-line" value="70.5" step="0.5" /></div>
      </div>
      <div id="prop-ev" class="props-card" style="background:var(--golf-elevated-soft);"></div>
    </div>
  `;

  function fillPropGolfers() {
    const sel = panelProps.querySelector("#prop-golfer") as HTMLSelectElement;
    sel.innerHTML = "";
    const rows = projectionsCache?.players ?? [];
    if (rows.length === 0) {
      sel.appendChild(new Option("(Load projections)", ""));
      return;
    }
    for (const r of rows) {
      sel.appendChild(new Option(nameDisplay(r.player_name), r.player_name));
    }
  }

  function propEv() {
    const box = panelProps.querySelector("#prop-ev") as HTMLDivElement;
    const g = (panelProps.querySelector("#prop-golfer") as HTMLSelectElement).value;
    const st = (panelProps.querySelector("#prop-stat") as HTMLSelectElement).value;
    const line = parseFloat((panelProps.querySelector("#prop-line") as HTMLInputElement).value);
    const row = projectionsCache?.players.find((p) => p.player_name === g);
    if (!row || !Number.isFinite(line)) {
      box.innerHTML = "<p class='text-muted'>Select golfer and line.</p>";
      return;
    }
    let mean = 70;
    let sd = 2.5;
    if (st === "Total Score") {
      mean = Number(row.total_score) || 70;
      sd = Number(row.round_sd);
      if (!Number.isFinite(sd) || sd < 0.5 || sd > 5) sd = 2.5;
      const pUnder = normalCdfLine(line, mean, sd);
      const pOver = 1 - pUnder;
      const decO = americanToDecimal(-110);
      const decU = americanToDecimal(110);
      const evO = pOver * (decO - 1) - (1 - pOver);
      const evU = pUnder * (decU - 1) - (1 - pUnder);
      box.innerHTML = `<p><span class="props-label">Over </span><span class="${evO >= 0 ? "ev-positive" : "ev-negative"}">${(evO * 100).toFixed(1)}%</span></p>
        <p><span class="props-label">Under </span><span class="${evU >= 0 ? "ev-positive" : "ev-negative"}">${(evU * 100).toFixed(1)}%</span></p>
        <p class="text-muted">Uses -110 / +110 placeholder juice.</p>`;
    } else {
      const key = st === "Pars" ? "pars" : st === "Birdies" ? "birdies" : "bogeys";
      mean = Number(row[key]) || 4;
      const k = Math.min(18, Math.max(0, Math.floor(line)));
      let pUnder = 0;
      for (let i = 0; i <= k; i++) pUnder += poissonPmf(i, mean);
      const pOver = 1 - pUnder;
      const decO = americanToDecimal(-110);
      const decU = americanToDecimal(110);
      const evO = pOver * (decO - 1) - (1 - pOver);
      const evU = pUnder * (decU - 1) - (1 - pUnder);
      box.innerHTML = `<p><span class="props-label">Over </span><span class="${evO >= 0 ? "ev-positive" : "ev-negative"}">${(evO * 100).toFixed(1)}%</span></p>
        <p><span class="props-label">Under </span><span class="${evU >= 0 ? "ev-positive" : "ev-negative"}">${(evU * 100).toFixed(1)}%</span></p>`;
    }
  }

  panelProps.querySelectorAll("#prop-golfer, #prop-stat, #prop-line").forEach((x) => {
    x.addEventListener("change", propEv);
    x.addEventListener("input", propEv);
  });

  function normalCdfLine(line: number, mean: number, sd: number): number {
    const z = (line - mean) / (sd * Math.SQRT2);
    return 0.5 * (1 + erf(z));
  }
  function erf(x: number): number {
    const sign = x < 0 ? -1 : 1;
    const ax = Math.abs(x);
    const a1 = 0.254829592;
    const a2 = -0.284496736;
    const a3 = 1.421413741;
    const a4 = -1.453152027;
    const a5 = 1.061405429;
    const p = 0.3275911;
    const t = 1 / (1 + p * ax);
    const y = 1 - (((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t) * Math.exp(-ax * ax);
    return sign * y;
  }
  function poissonPmf(k: number, lambda: number): number {
    return (Math.exp(-lambda) * Math.pow(lambda, k)) / factorial(k);
  }
  function americanToDecimal(a: number): number {
    if (a > 0) return 1 + a / 100;
    return 1 + 100 / Math.abs(a);
  }

  // +EV stub
  document.getElementById("panel-ev")!.innerHTML = `
    <h2 class="leaderboard-heading">+EV Bets</h2>
    <div class="leaderboard-card message-box">
      Load <code>public/data/player_props_lines.csv</code> export as JSON to extend this tab; core table lives in Shiny <code>app.R</code> today.
    </div>
  `;

  // Matchups
  const panelMatch = document.getElementById("panel-matchups")!;
  panelMatch.innerHTML = `
    <h2 class="leaderboard-heading">Round Matchups</h2>
    <p class="text-muted">Fetches DataGolf matchups when dev proxy is configured.</p>
    <select id="mkt-match" style="margin-bottom:12px;">
      <option value="round_matchups">Round Matchups</option>
      <option value="tournament_matchups">Tournament Matchups</option>
      <option value="3_balls">3 Balls</option>
    </select>
    <button type="button" class="btn-primary" id="btn-match">Load matchups</button>
    <div class="leaderboard-card" id="match-out" style="margin-top:12px;"></div>
  `;
  panelMatch.querySelector("#btn-match")!.addEventListener("click", async () => {
    const mkt = (panelMatch.querySelector("#mkt-match") as HTMLSelectElement).value;
    const out = panelMatch.querySelector("#match-out") as HTMLDivElement;
    out.textContent = "Loading…";
    const data = await fetchJson<Record<string, unknown>>("betting-tools/matchups", {
      tour: "pga",
      market: mkt,
      odds_format: "decimal",
      file_format: "json",
    });
    if (!data) {
      out.innerHTML =
        "<p class='message-box'>No data (check <code>DATAGOLF_API_KEY</code> in <code>.env</code> and dev server proxy).</p>";
      return;
    }
    out.innerHTML = `<pre style="color:var(--golf-text-muted);font-size:0.75rem;overflow:auto;max-height:400px;">${escapeHtml(JSON.stringify(data, null, 2).slice(0, 8000))}</pre>`;
  });

  // Outrights
  const panelOut = document.getElementById("panel-outrights")!;
  panelOut.innerHTML = `
    <h2 class="leaderboard-heading">Outrights</h2>
    <p class="text-muted">Model vs books — fetch sample from DataGolf.</p>
    <select id="mkt-out" style="margin-bottom:12px;">
      <option value="win">Win</option>
      <option value="top_5">Top 5</option>
      <option value="top_10">Top 10</option>
      <option value="top_20">Top 20</option>
      <option value="make_cut">Make Cut</option>
    </select>
    <button type="button" class="btn-primary" id="btn-out">Load outrights</button>
    <div class="leaderboard-card" id="out-out" style="margin-top:12px;"></div>
  `;
  panelOut.querySelector("#btn-out")!.addEventListener("click", async () => {
    const mkt = (panelOut.querySelector("#mkt-out") as HTMLSelectElement).value;
    const out = panelOut.querySelector("#out-out") as HTMLDivElement;
    out.textContent = "Loading…";
    const data = await fetchJson<Record<string, unknown>>("betting-tools/outrights", {
      tour: "pga",
      market: mkt,
      odds_format: "percent",
      file_format: "json",
    });
    if (!data) {
      out.innerHTML =
        "<p class='message-box'>No data (proxy / API key).</p>";
      return;
    }
    out.innerHTML = `<pre style="color:var(--golf-text-muted);font-size:0.75rem;overflow:auto;max-height:400px;">${escapeHtml(JSON.stringify(data, null, 2).slice(0, 8000))}</pre>`;
  });

  function renderHolePanel() {
    const panelHole = document.getElementById("panel-hole")!;
    if (panelHole.dataset.mounted === "1") return;
    panelHole.dataset.mounted = "1";
    panelHole.innerHTML = `
      <h2 class="leaderboard-heading">Hole Hangout</h2>
      <p class="text-muted">Static site: score odds and shot simulation stay in R/Shiny unless you port <code>R/shot_level_model.R</code> to WebAssembly or a small Node service.</p>
      <div class="props-card">
        <p>This tab is a placeholder. Export hole outcome JSON from R or add a future JS Monte Carlo module.</p>
      </div>
    `;
  }

  void refreshAll();
}

mountApp();
