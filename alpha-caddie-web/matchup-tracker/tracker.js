/**
 * Matchup tracker — round matchups + 3-balls (DK / FanDuel / BetMGM).
 * npm run matchup-tracker → http://localhost:5173/matchup-tracker/
 */
import {
  buildRoundMatchupPicks,
  buildThreeBallPicks,
  MATCHUP_TRACKER_BOOKS,
  isAllowedMatchupTrackerBook,
  decimalToAmerican,
} from "./matchup-math.mjs";

const SUMMARY_URLS = ["../data/matchup_backtest_summary.csv", "../data/matchup_backtest_summary.csv.new"];
const DETAIL_URLS = ["../data/matchup_backtest_detail.csv", "../data/matchup_backtest_detail.csv.new"];
const PROJECTIONS_URL = "../projections.json";

const state = {
  tab: "overview",
  summary: [],
  detail: [],
  bets: [],
  picks: [],
};

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function escapeHtml(s) {
  return String(s ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
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
  const lines = String(text || "").split(/\r?\n/).filter(Boolean);
  if (!lines.length) return [];
  const header = parseCsvLine(lines[0]);
  const rows = [];
  for (let i = 1; i < lines.length; i++) {
    const cells = parseCsvLine(lines[i]);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
    rows.push(row);
  }
  return rows;
}

async function fetchFirstOk(urls) {
  let lastErr = null;
  for (const url of urls) {
    try {
      const res = await fetch(`${url}?t=${Date.now()}`, { cache: "no-store" });
      if (!res.ok) throw new Error(`${url} → ${res.status}`);
      return { text: await res.text(), url };
    } catch (e) {
      lastErr = e;
    }
  }
  throw lastErr || new Error("fetch failed");
}

function filters() {
  return {
    tournament: String(document.getElementById("filter-tournament")?.value || "").trim(),
    market: String(document.getElementById("filter-market")?.value || "").trim(),
    book: String(document.getElementById("filter-book")?.value || "").trim().toLowerCase(),
    minEv: num(document.getElementById("filter-min-ev")?.value, 0),
    side: String(document.getElementById("filter-side")?.value || "").trim().toLowerCase(),
    player: String(document.getElementById("filter-player")?.value || "").trim().toLowerCase(),
    show: String(document.getElementById("filter-show")?.value || "bets"),
  };
}

function resultFromPick(row, side) {
  if (side === "p1") return String(row.p1_result || "").toUpperCase();
  if (side === "p2") return String(row.p2_result || "").toUpperCase();
  if (side === "p3") return String(row.p3_result || "").toUpperCase();
  return "";
}

function edgeForSide(row, side, oddsAt = "close") {
  const open = oddsAt === "open";
  if (side === "p1") return num(open ? row.edge_p1_open_pct : row.edge_p1_pct, NaN);
  if (side === "p2") return num(open ? row.edge_p2_open_pct : row.edge_p2_pct, NaN);
  if (side === "p3") return num(open ? row.edge_p3_open_pct : row.edge_p3_pct, NaN);
  return NaN;
}

function bestSide(row, minEv, oddsAt = "close") {
  const sides = ["p1", "p2", "p3"];
  let best = null;
  for (const side of sides) {
    const edge = edgeForSide(row, side, oddsAt);
    if (!Number.isFinite(edge) || edge < minEv) continue;
    if (!best || edge > best.edge) best = { side, edge };
  }
  return best;
}

function decForSide(row, side, oddsAt = "close") {
  const open = oddsAt === "open";
  if (side === "p1") return num(open ? row.p1_open_dec : row.p1_close_dec, NaN);
  if (side === "p2") return num(open ? row.p2_open_dec : row.p2_close_dec, NaN);
  if (side === "p3") return num(open ? row.p3_open_dec : row.p3_close_dec, NaN);
  return NaN;
}

function explodeDetailBets(detailRows) {
  const f = filters();
  /** @type {object[]} */
  const out = [];
  for (const row of detailRows) {
    if (!isAllowedMatchupTrackerBook(row.book)) continue;
    if (f.tournament && String(row.event_name || "").trim() !== f.tournament) continue;
    if (f.market && String(row.market || "").trim() !== f.market) continue;
    if (f.book && String(row.book || "").toLowerCase() !== f.book) continue;
    if (f.player) {
      const blob = `${row.player_name} ${row.opponent_name} ${row.opponent2_name}`.toLowerCase();
      if (!blob.includes(f.player)) continue;
    }

    // Qualify and P/L always on close (most recent DG closing price).
    const pick = bestSide(row, f.minEv, "close");
    if (f.show === "bets" && !pick) continue;
    const side = f.side || pick?.side || String(row.pick_side_at_10 || "").trim() || "p1";
    if (f.side && side !== f.side) continue;
    const edge = edgeForSide(row, side, "close");
    if (f.show === "bets" && !(Number.isFinite(edge) && edge >= f.minEv)) continue;

    const result = resultFromPick(row, side);
    const closeDec = decForSide(row, side, "close");
    const openDec = decForSide(row, side, "open");
    let units = 0;
    if (result === "W" && closeDec > 1) units = closeDec - 1;
    else if (result === "L") units = -1;

      out.push({
      ...row,
      side,
      edge,
      edgeOpen: edgeForSide(row, side, "open"),
      result,
      dec: closeDec,
      openDec,
      units,
      american: decimalToAmerican(closeDec),
      americanOpen: decimalToAmerican(openDec),
    });
  }
  return out;
}

function fmtPct(v, d = 1) {
  if (!Number.isFinite(v)) return "—";
  return `${v.toFixed(d)}%`;
}

function fmtNum(v, d = 2) {
  if (!Number.isFinite(v)) return "—";
  return v.toFixed(d);
}

function resultBadge(r) {
  const s = String(r || "").toUpperCase();
  if (s === "W") return `<span class="badge win">W</span>`;
  if (s === "L") return `<span class="badge loss">L</span>`;
  if (s === "P") return `<span class="badge push">P</span>`;
  return "—";
}

function bookLabel(b) {
  const n = String(b || "").toLowerCase();
  if (n === "draftkings") return "DraftKings";
  if (n === "fanduel") return "FanDuel";
  if (n === "betmgm") return "BetMGM";
  return b || "—";
}

function fillTournaments(detail) {
  const sel = document.getElementById("filter-tournament");
  if (!sel) return;
  const cur = sel.value;
  const events = [...new Set(detail.map((r) => String(r.event_name || "").trim()).filter(Boolean))].sort();
  sel.innerHTML = `<option value="">All tournaments</option>${events
    .map((e) => `<option value="${escapeHtml(e)}">${escapeHtml(e)}</option>`)
    .join("")}`;
  if (cur && events.includes(cur)) sel.value = cur;
}

function aggregateBets(bets, keyFn) {
  const map = new Map();
  for (const b of bets) {
    const k = keyFn(b);
    let a = map.get(k);
    if (!a) {
      a = { key: k, bets: 0, wins: 0, losses: 0, pushes: 0, units: 0 };
      map.set(k, a);
    }
    a.bets += 1;
    if (b.result === "W") a.wins += 1;
    else if (b.result === "L") a.losses += 1;
    else a.pushes += 1;
    a.units += num(b.units, 0);
  }
  return [...map.values()].sort((x, y) => y.bets - x.bets);
}

function renderOverview() {
  const bets = state.bets;
  const units = bets.reduce((s, b) => s + num(b.units, 0), 0);
  const wins = bets.filter((b) => b.result === "W").length;
  const losses = bets.filter((b) => b.result === "L").length;
  const roi = bets.length ? (units / bets.length) * 100 : NaN;
  const hit = wins + losses > 0 ? (wins / (wins + losses)) * 100 : NaN;

  document.getElementById("overview-kpis").innerHTML = `
    <div class="kpi"><div class="kpi-label">Qualified bets</div><div class="kpi-value">${bets.length.toLocaleString()}</div></div>
    <div class="kpi"><div class="kpi-label">Units</div><div class="kpi-value">${fmtNum(units, 1)}</div></div>
    <div class="kpi"><div class="kpi-label">ROI</div><div class="kpi-value">${fmtPct(roi)}</div></div>
    <div class="kpi"><div class="kpi-label">Hit rate</div><div class="kpi-value">${fmtPct(hit)}</div></div>
    <div class="kpi"><div class="kpi-label">Books</div><div class="kpi-value">${MATCHUP_TRACKER_BOOKS.map(bookLabel).join(" · ")}</div></div>
  `;

  const byMarket = aggregateBets(bets, (b) => String(b.market || ""));
  document.querySelector("#overview-market-table tbody").innerHTML = byMarket
    .map((a) => {
      const hitPct = a.wins + a.losses > 0 ? (a.wins / (a.wins + a.losses)) * 100 : NaN;
      const roiPct = a.bets ? (a.units / a.bets) * 100 : NaN;
      return `<tr>
        <td>${escapeHtml(a.key)}</td>
        <td class="num">${a.bets}</td>
        <td class="num">${fmtPct(hitPct)}</td>
        <td class="num">${fmtNum(a.units, 1)}</td>
        <td class="num">${fmtPct(roiPct)}</td>
        </tr>`;
      })
    .join("") || `<tr><td colspan="5">No bets for current filters.</td></tr>`;

  const byBook = aggregateBets(bets, (b) => String(b.book || "").toLowerCase());
  document.querySelector("#overview-book-table tbody").innerHTML = byBook
    .map((a) => {
      const hitPct = a.wins + a.losses > 0 ? (a.wins / (a.wins + a.losses)) * 100 : NaN;
      const roiPct = a.bets ? (a.units / a.bets) * 100 : NaN;
      return `<tr>
        <td>${escapeHtml(bookLabel(a.key))}</td>
        <td class="num">${a.bets}</td>
        <td class="num">${fmtPct(hitPct)}</td>
        <td class="num">${fmtNum(a.units, 1)}</td>
        <td class="num">${fmtPct(roiPct)}</td>
            </tr>`;
          })
    .join("") || `<tr><td colspan="5">No bets for current filters.</td></tr>`;
}

function renderAccuracy() {
  const f = filters();
  const rows = state.summary.filter((r) => {
    const sec = String(r.section || "");
    if (sec !== "model_vs_book" && sec !== "model_vs_book_by_market") return false;
    if (f.market && String(r.market || "") !== f.market) return false;
    if (f.tournament && String(r.event_name || "") !== f.tournament && r.event_name !== "(all events)") return false;
    return true;
  });
  document.querySelector("#accuracy-table tbody").innerHTML = rows
    .slice(0, 200)
          .map(
            (r) => `<tr>
      <td>${escapeHtml(r.event_name)}</td>
      <td>${escapeHtml(r.market)}</td>
      <td class="num">${escapeHtml(r.rmse || "—")}</td>
      <td class="num">${escapeHtml(r.mae || "—")}</td>
      <td class="num">${escapeHtml(r.n_line_pairs || "—")}</td>
      </tr>`,
          )
    .join("") || `<tr><td colspan="5">No accuracy rows.</td></tr>`;
}

function renderEv() {
  const f = filters();
  const rows = state.summary
    .filter((r) => String(r.section || "").startsWith("ev_backtest"))
    .filter((r) => {
      if (f.market && String(r.market || "") !== f.market) return false;
      if (f.tournament && r.event_name && r.event_name !== "(all events)" && r.event_name !== f.tournament) {
        return false;
      }
      if (f.side && String(r.bet_side || "").toLowerCase() !== f.side && r.bet_side !== "pick") return false;
      const th = num(r.ev_threshold_pct, NaN);
      if (Number.isFinite(f.minEv) && Number.isFinite(th) && th < f.minEv) return false;
      return true;
    })
    .sort((a, b) => num(b.roi_pct, -999) - num(a.roi_pct, -999));

  document.querySelector("#ev-table tbody").innerHTML = rows
    .slice(0, 300)
        .map((r) => {
      const wlp = `${r.wins || 0}-${r.losses || 0}-${r.pushes || 0}`;
          return `<tr>
        <td>${escapeHtml(r.market)}</td>
        <td class="num">${escapeHtml(r.ev_threshold_pct)}</td>
        <td>${escapeHtml(r.bet_side)}</td>
        <td class="num">${escapeHtml(r.bets)}</td>
        <td class="num">${wlp}</td>
        <td class="num">${escapeHtml(r.units_net)}</td>
        <td class="num">${escapeHtml(r.roi_pct)}${r.roi_pct ? "%" : ""}</td>
      </tr>`;
        })
    .join("") || `<tr><td colspan="7">No EV rows.</td></tr>`;
}

function fmtAmerican(am) {
  if (!Number.isFinite(am)) return "—";
  return am > 0 ? `+${Math.round(am)}` : String(Math.round(am));
}

function renderBets() {
  const rows = state.bets.slice(0, 500);
  document.querySelector("#bets-table tbody").innerHTML = rows
        .map((r) => {
      const opp =
        String(r.market || "") === "3-balls"
          ? [r.opponent_name, r.opponent2_name].filter(Boolean).join(" / ")
          : r.opponent_name;
          return `<tr>
        <td>${escapeHtml(r.event_name)}</td>
        <td>${escapeHtml(r.round)}</td>
        <td>${escapeHtml(r.market)}</td>
        <td>${escapeHtml(bookLabel(r.book))}</td>
        <td>${escapeHtml(r.player_name)} <span class="muted">(${escapeHtml(r.side)})</span></td>
        <td>${escapeHtml(opp)}</td>
        <td class="num">${escapeHtml(r.model_win_pct)}</td>
        <td class="num" title="Open ${escapeHtml(r.open_time || "—")}">${escapeHtml(r.open_implied_pct || "—")}</td>
        <td class="num" title="Close ${escapeHtml(r.close_time || "—")}">${escapeHtml(r.close_implied_pct)}</td>
        <td class="num">${fmtAmerican(r.americanOpen)}</td>
        <td class="num">${fmtAmerican(r.american)}</td>
        <td class="num">${Number.isFinite(r.edgeOpen) ? fmtPct(r.edgeOpen) : "—"}</td>
        <td class="num">${Number.isFinite(r.edge) ? fmtPct(r.edge) : "—"}</td>
        <td>${escapeHtml(r.pick_side_at_10 || r.side)}</td>
        <td>${resultBadge(r.result)}</td>
      </tr>`;
        })
    .join("") || `<tr><td colspan="15">No graded bets for filters.</td></tr>`;
}

function playersByDg(projections, round) {
  /** @type {Map<number, object>} */
  const map = new Map();
  for (const p of projections?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    const rnd = Math.round(num(p.round, NaN));
    if (!Number.isFinite(dg)) continue;
    if (Number.isFinite(round) && Number.isFinite(rnd) && rnd !== round) continue;
    if (!map.has(dg)) map.set(dg, p);
  }
  return map;
}

async function loadLivePicks() {
  try {
    const res = await fetch(`${PROJECTIONS_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) throw new Error(`projections ${res.status}`);
    const projections = await res.json();
    const round = Math.round(num(projections.display_round, 1)) || 1;
    const players = playersByDg(projections, round);
    const minEv = filters().minEv;
    const roundPicks = buildRoundMatchupPicks({ projections, players, round, minEvPct: minEv });
    const threePicks = buildThreeBallPicks({ projections, players, round, minEvPct: minEv });
    let picks = [...roundPicks, ...threePicks].sort((a, b) => num(b.edgePct, 0) - num(a.edgePct, 0));
    const f = filters();
    if (f.market) picks = picks.filter((p) => p.market === f.market);
    if (f.book) picks = picks.filter((p) => String(p.book || "").toLowerCase() === f.book);
    if (f.player) {
      picks = picks.filter((p) =>
        `${p.player_name} ${p.opponent_name}`.toLowerCase().includes(f.player),
      );
    }
    state.picks = picks.slice(0, 40);
    const note = document.getElementById("picks-note");
    if (note) {
      note.textContent = `${projections.event_name || "Live"} · R${round} · ${state.picks.length} picks ≥ ${minEv}% edge (DK/FD/BetMGM)`;
    }
  } catch (e) {
    state.picks = [];
    const note = document.getElementById("picks-note");
    if (note) note.textContent = `Could not load live picks: ${e?.message || e}`;
  }
}

function renderPicks() {
  const rows = state.picks;
  document.querySelector("#picks-table tbody").innerHTML = rows
    .map((p) => {
      const odds = Number.isFinite(p.odds) ? (p.odds > 0 ? `+${p.odds}` : String(p.odds)) : "—";
      return `<tr>
        <td>${escapeHtml(p.market)}</td>
        <td>${escapeHtml(bookLabel(p.book))}</td>
        <td>${escapeHtml(p.player_name)}</td>
        <td>${escapeHtml(p.opponent_name)}</td>
        <td class="num">${fmtPct(num(p.mu, NaN) * 100)}</td>
        <td class="num">${odds}</td>
        <td class="num">${fmtPct(p.edgePct)}</td>
      </tr>`;
    })
    .join("") || `<tr><td colspan="7">No live DK/FD/BetMGM matchup edges for current filters.</td></tr>`;
}

function renderAll() {
  state.bets = explodeDetailBets(state.detail);
  renderOverview();
  renderAccuracy();
  renderEv();
  renderBets();
  renderPicks();
}

function setTab(tab) {
  state.tab = tab;
  document.querySelectorAll(".tab").forEach((el) => {
    el.classList.toggle("active", el.dataset.tab === tab);
  });
  document.querySelectorAll(".panel").forEach((el) => {
    el.classList.toggle("active", el.id === `panel-${tab}`);
  });
}

async function loadAll() {
  const err = document.getElementById("error-banner");
  err.hidden = true;
  try {
    const [sum, det] = await Promise.all([fetchFirstOk(SUMMARY_URLS), fetchFirstOk(DETAIL_URLS)]);
    state.summary = parseCsv(sum.text);
    state.detail = parseCsv(det.text).filter((r) => isAllowedMatchupTrackerBook(r.book));
    fillTournaments(state.detail);
    const exported = state.detail[0]?.exported_at || state.summary[0]?.exported_at || "";
    document.getElementById("header-meta").textContent =
      `${state.detail.length.toLocaleString()} graded lines · exported ${exported || "—"}`;
    await loadLivePicks();
    renderAll();
  } catch (e) {
    err.hidden = false;
    err.innerHTML = `<strong>Cannot load matchup CSVs.</strong> Run <code>npm run matchup-tracker:refresh</code> then open this page. (${escapeHtml(e?.message || e)})`;
  }
}

function wire() {
  document.querySelectorAll(".tab").forEach((btn) => {
    btn.addEventListener("click", () => setTab(btn.dataset.tab));
  });
  for (const id of [
    "filter-tournament",
    "filter-market",
    "filter-book",
    "filter-min-ev",
    "filter-side",
    "filter-show",
  ]) {
    document.getElementById(id)?.addEventListener("change", async () => {
      await loadLivePicks();
    renderAll();
    });
  }
  document.getElementById("filter-player")?.addEventListener("input", () => {
    renderAll();
  });
  document.getElementById("btn-reload")?.addEventListener("click", () => loadAll());
}

wire();
loadAll();
