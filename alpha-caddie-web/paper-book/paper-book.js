/**
 * Paper trade book — golf props with odds fetched directly from each book's API.
 */
import {
  PAPER_BOOKS,
  bookById,
  buildLivePropCards,
  formatLine,
  formatPostedOdds,
  lookupDirectCard,
  marketShortLabel,
  setBakedBookCatalog,
  sideBookOddsFromCard,
  sidePayoutMultiplierFromCard,
} from "./live-book-options.mjs";
import {
  describeEntryPayout,
  calcDkSinglePnl,
  calcPickemParlayPnl,
  calcPrizePicksEntryPnl,
} from "./book-payouts.mjs";
import { gradeOuBet, buildOuGradeIndex } from "../projection-tracker/my-bets-grade.mjs";
import { DETAIL_EXPORT_MARKETS } from "../projection-tracker/detail-market-specs.mjs";
import {
  applyBookSlice,
  bookSlice,
  downloadHistoryBackup,
  importHistoryBackupFile,
  loadPersistedState,
} from "./paper-book-state.mjs";

const MARKET_SPECS = DETAIL_EXPORT_MARKETS.map((m) => {
  const stem = m.key === "total" ? "round_score" : m.key === "fairways" ? "fairways" : m.key;
  return {
    ...m,
    bookCol: m.bookLineCol,
    overRes: `${stem}_over`,
    underRes: `${stem}_under`,
    actual: m.key === "total" ? "actual_round_score" : `actual_${m.key === "fairways" ? "fairways" : m.key}`,
  };
});

const PROJECTIONS_URL = "../projections.json";
const PAPER_BOOK_LINES_URL = "./paper-book-lines.json";
const VS_ACTUAL_URL = "../data/round_projection_vs_actual.csv";

/** @type {object|null} */
let projections = null;
/** @type {Map<string, object>|null} */
let ouGradeIndex = null;
/** @type {object|null} */
let liveBuilt = null;

/** @type {object|null} */
let persisted = null;

function readBookFromUrl() {
  const q = new URLSearchParams(window.location.search).get("book");
  return PAPER_BOOKS.some((b) => b.id === q) ? q : null;
}

const state = {
  bookId: readBookFromUrl() || "draftkings",
  search: "",
  market: "",
  playType: "power",
  stake: 10,
  slip: [],
  bankroll: 1000,
  startingBankroll: 1000,
  history: [],
};

function syncStateFromPersisted(bookId = state.bookId) {
  const slice = bookSlice(persisted, bookId);
  state.bankroll = slice.bankroll;
  state.startingBankroll = slice.startingBankroll;
  state.playType = slice.playType;
  state.stake = slice.stake;
  state.history = slice.history;
}

function persistCurrentBook() {
  if (!persisted) return;
  persisted = applyBookSlice(persisted, state.bookId, {
    bankroll: state.bankroll,
    startingBankroll: state.startingBankroll,
    playType: state.playType,
    stake: state.stake,
    history: state.history,
  });
}

function saveBookState() {
  persistCurrentBook();
}

function esc(s) {
  return String(s ?? "")
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/"/g, "&quot;");
}

function fmtUsd(n) {
  const v = Number(n);
  if (!Number.isFinite(v)) return "—";
  return v.toLocaleString(undefined, { style: "currency", currency: "USD", maximumFractionDigits: 2 });
}

function showToast(msg) {
  const el = document.getElementById("toast");
  if (!el) return;
  el.textContent = msg;
  el.hidden = false;
  clearTimeout(showToast._t);
  showToast._t = setTimeout(() => {
    el.hidden = true;
  }, 2800);
}

function sideLabel(book, side) {
  return side === "under" ? book.underLabel : book.overLabel;
}

function pickLabel(book, side, line) {
  return `${sideLabel(book, side)} ${formatLine(line, book.wholeLine)}`;
}

function slipLineKey(leg) {
  return leg.lineKey || `${leg.dg_id}|${leg.market}|${leg.side}`;
}

function legFromCard(card, side) {
  const bookOdds = sideBookOddsFromCard(card, side);
  const payoutMultiplier = sidePayoutMultiplierFromCard(card, side);
  return {
    lineKey: `${card.dg_id}|${card.market}|${side}`,
    cardKey: card.cardKey,
    eventName: card.eventName,
    round: card.round,
    dg_id: card.dg_id,
    playerName: card.playerName,
    market: card.market,
    side,
    line: card.line,
    bookOdds,
    payoutMultiplier,
    odds: bookOdds?.kind === "american" ? bookOdds.raw : undefined,
    oddsSource: card.oddsSource,
    fetchedAt: card.fetchedAt,
  };
}

function refreshLiveBookLines() {
  if (!projections) return null;
  liveBuilt = buildLivePropCards(projections, state.bookId);
  return liveBuilt;
}

function paintBoard(built) {
  liveBuilt = built;
  renderEventBar(built);
  renderMarketFilter(built.cards);
  renderBoardSync();
}

function setBook(bookId) {
  if (bookId === state.bookId) return;
  persistCurrentBook();
  state.bookId = bookId;
  state.slip = [];
  syncStateFromPersisted(bookId);
  const url = new URL(window.location.href);
  url.searchParams.set("book", bookId);
  window.history.replaceState({}, "", url);
  applyBookTheme();
  renderAll();
}

function applyBookTheme() {
  const book = bookById(state.bookId);
  document.body.className = `book-${book.id}`;
  document.getElementById("book-title").textContent = book.label;
  document.getElementById("slip-title").textContent =
    book.mode === "sportsbook" ? "Bet slip" : "Entry slip";
  document.getElementById("btn-place").textContent =
    book.mode === "sportsbook" ? "Place bet" : "Place entry";

  document.querySelectorAll(".book-tab").forEach((btn) => {
    btn.classList.toggle("active", btn.getAttribute("data-book") === book.id);
  });

  document.getElementById("pp-play-toggle").hidden = book.id !== "prizepicks";

  document.querySelectorAll(".play-type").forEach((btn) => {
    btn.classList.toggle("active", btn.getAttribute("data-play") === state.playType);
  });
}

function toggleSlipLeg(option) {
  const book = bookById(state.bookId);
  const key = option.lineKey;
  const idx = state.slip.findIndex((l) => slipLineKey(l) === key);

  if (idx >= 0) {
    state.slip.splice(idx, 1);
    renderSlip();
    renderBoardSync();
    return;
  }

  if (book.mode === "sportsbook") {
    state.slip = [option];
  } else {
    if (state.slip.length >= book.maxPicks) {
      showToast(`Max ${book.maxPicks} picks per entry`);
      return;
    }
    const cardTaken = state.slip.findIndex((l) => l.cardKey === option.cardKey);
    if (cardTaken >= 0) state.slip.splice(cardTaken, 1);
    state.slip.push(option);
  }
  renderSlip();
  renderBoardSync();
}

function removeSlipLeg(lineKey) {
  state.slip = state.slip.filter((l) => slipLineKey(l) !== lineKey);
  renderSlip();
  renderBoardSync();
}

function canPlace() {
  const book = bookById(state.bookId);
  const stake = Number(state.stake);
  if (!state.slip.length || !Number.isFinite(stake) || stake <= 0) return false;
  if (stake > state.bankroll) return false;
  if (book.mode === "sportsbook") return state.slip.length >= 1;
  return state.slip.length >= book.minPicks;
}

function placeBet() {
  if (!canPlace()) return;
  const book = bookById(state.bookId);
  const stake = Math.round(Number(state.stake));
  const built = refreshLiveBookLines();
  if (!built) return;

  const lockedLegs = [];
  for (const leg of state.slip) {
    const card = lookupDirectCard(built.cards, leg.dg_id, leg.market);
    if (!card) {
      showToast(`${leg.playerName} — book line no longer posted`);
      return;
    }
    lockedLegs.push(legFromCard(card, leg.side));
  }

  state.bankroll -= stake;
  state.history.unshift({
    id: crypto.randomUUID(),
    placedAt: new Date().toISOString(),
    bookId: book.id,
    eventName: built.eventName,
    round: built.round,
    stake,
    playType: book.id === "prizepicks" ? state.playType : book.mode === "sportsbook" ? "single" : "parlay",
    result: "open",
    legs: lockedLegs,
  });
  state.slip = [];
  saveBookState();
  gradeOpenEntries();
  renderAll();
  showToast(`Placed ${fmtUsd(stake)} · ${book.label}`);
}

function gradeLeg(leg, entry) {
  if (!ouGradeIndex) return null;
  return gradeOuBet(
    {
      eventName: entry.eventName,
      round: entry.round,
      dg_id: leg.dg_id,
      playerName: leg.playerName,
      market: leg.market,
      side: leg.side,
      line: leg.line,
    },
    ouGradeIndex,
  );
}

function settleEntry(entry) {
  const book = bookById(entry.bookId);
  const legs = entry.legs.map((leg) => ({
    ...leg,
    result: gradeLeg(leg, entry) || leg.result || "open",
  }));

  const allGraded = legs.every((l) => {
    const r = String(l.result || "").toUpperCase();
    return r === "W" || r === "L" || r === "P";
  });
  if (!allGraded) return false;

  let pnl = 0;
  let result = "loss";

  if (book.mode === "sportsbook") {
    const leg = legs[0];
    const r = String(leg.result).toUpperCase();
    pnl = calcDkSinglePnl(entry.stake, leg.odds, r);
    if (r === "W") result = "win";
    else if (r === "P") result = "push";
    else result = "loss";
  } else if (book.id === "prizepicks") {
    pnl = calcPrizePicksEntryPnl(legs, entry.stake, entry.playType);
    result = pnl > 0 ? "win" : "loss";
  } else {
    pnl = calcPickemParlayPnl(legs, entry.stake);
    result = pnl > 0 ? "win" : "loss";
  }

  entry.legs = legs;
  entry.result = result;
  entry.pnl = pnl;
  entry.settledAt = new Date().toISOString();
  state.bankroll += entry.stake + pnl;
  return true;
}

function gradeOpenEntries() {
  if (!ouGradeIndex) return;
  let changed = 0;
  for (const entry of state.history) {
    if (entry.result !== "open") continue;
    if (settleEntry(entry)) changed++;
  }
  if (changed > 0) saveBookState();
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

async function loadGradeData() {
  try {
    const res = await fetch(`${VS_ACTUAL_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return;
    const text = await res.text();
    const lines = text.split(/\r?\n/).filter(Boolean);
    if (lines.length < 2) return;
    const header = parseCsvLine(lines[0]);
    const rows = [];
    for (let i = 1; i < lines.length; i++) {
      const cells = parseCsvLine(lines[i]);
      const row = {};
      for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
      rows.push(row);
    }
    ouGradeIndex = buildOuGradeIndex(rows, MARKET_SPECS);
  } catch {
    /* optional */
  }
}

async function loadPaperBookLines() {
  const res = await fetch(`${PAPER_BOOK_LINES_URL}?t=${Date.now()}`, { cache: "no-store" });
  if (!res.ok) {
    setBakedBookCatalog(null);
    return false;
  }
  setBakedBookCatalog(await res.json());
  return true;
}

async function loadProjections() {
  const res = await fetch(`${PROJECTIONS_URL}?t=${Date.now()}`, { cache: "no-store" });
  if (!res.ok) throw new Error("Could not load projections.json");
  projections = await res.json();
}

function fetchHintForBook(bookId) {
  return "Run npm run push:live (or npm run bake:paper-book) to refresh book odds.";
}

function renderEventBar(built) {
  document.getElementById("event-label").textContent = `${built.roundLabel} · ${built.eventName || "Golf"}`;
  const when = built.fetchedAt ? new Date(built.fetchedAt).toLocaleString() : "";
  document.getElementById("event-updated").textContent = when
    ? `Book odds baked @ ${when} (push:live)`
    : "";

  const banner = document.getElementById("lines-banner");
  if (built.hasRealPostedOdds) {
    banner.hidden = true;
    return;
  }

  banner.hidden = false;
  banner.textContent = built.fetchError
    ? `${built.book.label}: ${built.fetchError}. ${fetchHintForBook(built.book.id)}`
    : `No ${built.book.label} props with book-posted odds right now. ${fetchHintForBook(built.book.id)}`;
}

function renderMarketFilter(cards) {
  const sel = document.getElementById("filter-market");
  const markets = [...new Set(cards.map((c) => c.market))].sort();
  const cur = state.market;
  sel.innerHTML =
    `<option value="">All</option>` +
    markets
      .map((m) => `<option value="${esc(m)}"${m === cur ? " selected" : ""}>${esc(marketShortLabel(m))}</option>`)
      .join("");
}

function renderBoardSync() {
  const board = document.getElementById("props-board");
  const built = liveBuilt;
  if (!built) return;

  const book = bookById(state.bookId);
  let cards = built.cards;
  if (state.market) cards = cards.filter((c) => c.market === state.market);
  if (state.search) {
    const q = state.search.toLowerCase();
    cards = cards.filter(
      (c) =>
        c.playerName.toLowerCase().includes(q) ||
        c.market.toLowerCase().includes(q) ||
        marketShortLabel(c.market).toLowerCase().includes(q),
    );
  }

  const slipKeys = new Set(state.slip.map(slipLineKey));

  if (!cards.length) {
    board.innerHTML = `<div class="empty-board">No ${book.label} props with book-posted odds.${built.fetchError ? ` ${built.fetchError}` : ""}</div>`;
    return;
  }

  board.innerHTML = cards
    .map((card) => {
      const inSlip = state.slip.some((l) => l.cardKey === card.cardKey);
      const overSel = slipKeys.has(`${card.dg_id}|${card.market}|over`);
      const underSel = slipKeys.has(`${card.dg_id}|${card.market}|under`);
      const lineTxt = formatLine(card.line, book.wholeLine);

      const mkSide = (side, selected) => {
        const bookOdds = sideBookOddsFromCard(card, side);
        return `<button type="button" class="side-btn ${side}${selected ? " selected" : ""}" data-side="${side}" data-line-key="${esc(`${card.dg_id}|${card.market}|${side}`)}">
          <span class="side-label">${esc(sideLabel(book, side))}</span>
          <span class="side-odds">${esc(formatPostedOdds(book, bookOdds))}</span>
        </button>`;
      };

      return `<article class="prop-card${inSlip ? " in-slip" : ""}">
        <div class="prop-player">${esc(card.playerName)}</div>
        <div class="prop-meta">
          <span class="prop-market">${esc(marketShortLabel(card.market))}</span>
          <span class="prop-line">${esc(lineTxt)}</span>
        </div>
        <div class="prop-sides">${mkSide("over", overSel)}${mkSide("under", underSel)}</div>
      </article>`;
    })
    .join("");

  board.querySelectorAll(".side-btn").forEach((btn) => {
    btn.addEventListener("click", () => {
      const lineKey = btn.getAttribute("data-line-key");
      const side = btn.getAttribute("data-side");
      const card = cards.find((c) => `${c.dg_id}|${c.market}|${side}` === lineKey);
      if (card) toggleSlipLeg(legFromCard(card, side));
    });
  });
}

function renderBoard() {
  const board = document.getElementById("props-board");
  if (!projections) {
    board.innerHTML = `<div class="empty-board">Loading…</div>`;
    return;
  }

  paintBoard(buildLivePropCards(projections, state.bookId));
}

function renderSlip() {
  const book = bookById(state.bookId);
  const legsEl = document.getElementById("slip-legs");
  const emptyEl = document.getElementById("slip-empty");

  if (!state.slip.length) {
    legsEl.innerHTML = "";
    emptyEl.hidden = false;
    emptyEl.textContent =
      book.mode === "sportsbook"
        ? "Select Over or Under on a prop."
        : `Pick ${book.minPicks}–${book.maxPicks} legs for your entry.`;
  } else {
    emptyEl.hidden = true;
    legsEl.innerHTML = state.slip
      .map(
        (leg) => `<div class="slip-leg">
        <div class="slip-leg-main">
          <div class="slip-leg-player">${esc(leg.playerName)}</div>
          <div class="slip-leg-pick">${esc(pickLabel(book, leg.side, leg.line))} · ${esc(marketShortLabel(leg.market))} · ${esc(formatPostedOdds(book, leg.bookOdds))}</div>
        </div>
        <button type="button" class="slip-leg-remove" data-line-key="${esc(slipLineKey(leg))}" aria-label="Remove">×</button>
      </div>`,
      )
      .join("");

    legsEl.querySelectorAll(".slip-leg-remove").forEach((btn) => {
      btn.addEventListener("click", () => removeSlipLeg(btn.getAttribute("data-line-key")));
    });
  }

  document.getElementById("payout-preview").textContent = describeEntryPayout(
    book.id,
    state.slip,
    state.playType,
    state.stake,
  );
  document.getElementById("stake-input").value = String(state.stake);
  document.getElementById("balance-pill").textContent = fmtUsd(state.bankroll);
  document.getElementById("btn-place").disabled = !canPlace();
}

function renderHistory() {
  const settled = state.history.filter((e) => e.result !== "open");
  const open = state.history.filter((e) => e.result === "open");
  const totalPnl = settled.reduce((s, e) => s + (Number(e.pnl) || 0), 0);
  const wins = settled.filter((e) => e.result === "win").length;
  const losses = settled.filter((e) => e.result === "loss").length;

  document.getElementById("history-stats").innerHTML = `
    <div class="stat-box"><div class="stat-label">Balance</div><div class="stat-value">${fmtUsd(state.bankroll)}</div></div>
    <div class="stat-box"><div class="stat-label">P/L</div><div class="stat-value ${totalPnl >= 0 ? "pos" : "neg"}">${totalPnl >= 0 ? "+" : ""}${fmtUsd(totalPnl)}</div></div>
    <div class="stat-box"><div class="stat-label">Record</div><div class="stat-value">${wins}-${losses} · ${open.length} open</div></div>
  `;

  const list = document.getElementById("history-list");
  if (!state.history.length) {
    list.innerHTML = `<p class="empty-board">No bets yet.</p>`;
    return;
  }

  list.innerHTML = state.history
    .map((entry) => {
      const book = bookById(entry.bookId);
      const status = entry.result;
      const pnl =
        status === "open" ? "pending" : `${(entry.pnl || 0) >= 0 ? "+" : ""}${fmtUsd(entry.pnl)}`;
      const legsTxt = entry.legs
        .map(
          (l) =>
            `${l.playerName.split(",")[0]} ${pickLabel(book, l.side, l.line)} (${formatPostedOdds(book, l.bookOdds || l.odds)})`,
        )
        .join(" · ");
      return `<div class="history-entry">
        <div class="entry-head">
          <span>${fmtUsd(entry.stake)} · ${new Date(entry.placedAt).toLocaleDateString()}</span>
          <span class="entry-status ${status}">${status}${status !== "open" ? ` · ${pnl}` : ""}</span>
        </div>
        <div class="entry-legs">${esc(legsTxt)}</div>
      </div>`;
    })
    .join("");
}

function renderAll() {
  renderSlip();
  renderHistory();
  document.getElementById("bankroll-reset").value = String(state.startingBankroll);
  renderBoard();
}

async function refreshGradeDataInBackground() {
  await loadGradeData();
  gradeOpenEntries();
  renderHistory();
}

function bindUi() {
  document.querySelectorAll(".book-tab").forEach((btn) => {
    btn.addEventListener("click", () => setBook(btn.getAttribute("data-book")));
  });

  document.getElementById("filter-search").addEventListener("input", (ev) => {
    state.search = ev.target.value.trim();
    renderBoardSync();
  });

  document.getElementById("filter-market").addEventListener("change", (ev) => {
    state.market = ev.target.value;
    renderBoardSync();
  });

  document.getElementById("stake-input").addEventListener("change", (ev) => {
    state.stake = Math.max(1, Math.round(Number(ev.target.value) || 10));
    saveBookState();
    renderSlip();
  });

  document.querySelectorAll(".quick-stake").forEach((btn) => {
    btn.addEventListener("click", () => {
      state.stake = Number(btn.getAttribute("data-amt")) || 10;
      saveBookState();
      renderSlip();
    });
  });

  document.getElementById("btn-place").addEventListener("click", () => {
    placeBet();
  });
  document.getElementById("btn-clear-slip").addEventListener("click", () => {
    state.slip = [];
    renderAll();
  });

  document.querySelectorAll(".play-type").forEach((btn) => {
    btn.addEventListener("click", () => {
      document.querySelectorAll(".play-type").forEach((b) => b.classList.remove("active"));
      btn.classList.add("active");
      state.playType = btn.getAttribute("data-play");
      saveBookState();
      renderSlip();
    });
  });

  document.getElementById("btn-history").addEventListener("click", () => {
    renderHistory();
    document.getElementById("history-dialog").showModal();
  });
  document.getElementById("btn-close-history").addEventListener("click", () => {
    document.getElementById("history-dialog").close();
  });

  document.getElementById("btn-reset-bankroll").addEventListener("click", () => {
    const v = Math.max(100, Number(document.getElementById("bankroll-reset").value) || 1000);
    state.startingBankroll = v;
    state.bankroll = v;
    state.history = [];
    saveBookState();
    renderAll();
    showToast(`Bankroll reset to ${fmtUsd(v)}`);
  });

  document.getElementById("btn-export-history")?.addEventListener("click", () => {
    persistCurrentBook();
    if (!persisted) return;
    downloadHistoryBackup(persisted);
    showToast("Downloaded paper-book-history.json — commit to GitHub to restore everywhere");
  });

  const importInput = document.getElementById("history-import-file");
  document.getElementById("btn-import-history")?.addEventListener("click", () => {
    importInput?.click();
  });
  importInput?.addEventListener("change", async () => {
    const file = importInput.files?.[0];
    importInput.value = "";
    if (!file) return;
    try {
      persisted = await importHistoryBackupFile(file);
      syncStateFromPersisted(state.bookId);
      renderAll();
      showToast("History imported");
    } catch {
      showToast("Could not import history file");
    }
  });
}

async function boot() {
  persisted = await loadPersistedState();
  syncStateFromPersisted(state.bookId);
  applyBookTheme();
  bindUi();
  renderSlip();
  renderHistory();

  window.addEventListener("beforeunload", () => {
    persistCurrentBook();
  });

  document.getElementById("props-board").innerHTML = `<div class="empty-board">Loading book odds…</div>`;

  try {
    const linesOk = await loadPaperBookLines();
    if (!linesOk) {
      document.getElementById("props-board").innerHTML =
        `<div class="empty-board">Missing paper-book-lines.json — run npm run push:live or npm run bake:paper-book</div>`;
      return;
    }
    renderAll();
  } catch (err) {
    document.getElementById("props-board").innerHTML = `<div class="empty-board">${esc(err.message)}</div>`;
    return;
  }

  loadProjections()
    .then(() => {
      gradeOpenEntries();
      refreshGradeDataInBackground();
    })
    .catch(() => {
      /* grading optional */
    });

  setInterval(async () => {
    try {
      await loadPaperBookLines();
      renderAll();
      if (projections) {
        await loadProjections();
        gradeOpenEntries();
      }
    } catch {
      /* ignore poll errors */
    }
  }, 120_000);
}

boot();
