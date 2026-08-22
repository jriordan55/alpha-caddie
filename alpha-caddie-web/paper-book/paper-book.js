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
  lookupMatchupCard,
  marketShortLabel,
  isMatchupBook,
  setBakedBookCatalog,
  sideBookOddsFromCard,
  sidePayoutMultiplierFromCard,
} from "./live-book-options.mjs";
import {
  describeEntryPayout,
} from "./book-payouts.mjs";
import {
  applyBookSlice,
  bookSlice,
  downloadHistoryBackup,
  importHistoryBackupFile,
  loadPersistedState,
  writePersistedState,
} from "./paper-book-state.mjs";
import {
  buildOuGradeIndexFromCsvText,
  gradePersistedState,
} from "./paper-book-grade.mjs";

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
  return slice;
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
  if (leg.lineKey) return leg.lineKey;
  if (leg.cardKind === "matchup") return `${leg.cardKey}|${leg.side}`;
  return `${leg.dg_id}|${leg.market}|${leg.side}`;
}

function legFromCard(card, side) {
  if (card.cardKind === "matchup") {
    const isP1 = side === "p1";
    const bookOdds = sideBookOddsFromCard(card, side);
    const payoutMultiplier = sidePayoutMultiplierFromCard(card, side);
    const playerName = isP1 ? card.p1_player_name : card.p2_player_name;
    const opponentName = isP1 ? card.p2_player_name : card.p1_player_name;
    return {
      lineKey: `${card.cardKey}|${side}`,
      cardKey: card.cardKey,
      cardKind: "matchup",
      eventName: card.eventName,
      round: card.round,
      side,
      p1_dg_id: card.p1_dg_id,
      p2_dg_id: card.p2_dg_id,
      p1_player_name: card.p1_player_name,
      p2_player_name: card.p2_player_name,
      playerName,
      opponentName,
      market: card.market,
      bookOdds,
      payoutMultiplier,
      odds: bookOdds?.kind === "american" ? bookOdds.raw : undefined,
      oddsSource: card.oddsSource,
      fetchedAt: card.fetchedAt,
    };
  }

  const bookOdds = sideBookOddsFromCard(card, side);
  const payoutMultiplier = sidePayoutMultiplierFromCard(card, side);
  return {
    lineKey: `${card.dg_id}|${card.market}|${side}`,
    cardKey: card.cardKey,
    cardKind: "ou",
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

function describeLegPick(book, leg) {
  if (leg.cardKind === "matchup") {
    return `${leg.playerName} vs ${leg.opponentName} · ${formatPostedOdds(book, leg.bookOdds)}`;
  }
  return `${pickLabel(book, leg.side, leg.line)} · ${marketShortLabel(leg.market)} · ${formatPostedOdds(book, leg.bookOdds)}`;
}

function legResultLabel(result) {
  const r = String(result || "").toUpperCase();
  if (r === "W") return "Win";
  if (r === "L") return "Loss";
  if (r === "P") return "Push";
  return "";
}

function entryStatusLabel(entry) {
  if (entry.result === "win") return "Win";
  if (entry.result === "loss") return "Loss";
  if (entry.result === "push") return "Push";
  return "Open";
}

function describeEntryBet(entry) {
  const book = bookById(entry.bookId);
  return entry.legs
    .map((leg) => {
      const res = legResultLabel(leg.result);
      const pick = describeLegPick(book, leg);
      return res ? `${pick} → ${res}` : pick;
    })
    .join(" · ");
}

function gradeAllBooksInPersisted() {
  if (!ouGradeIndex || !persisted) return 0;
  const { persisted: graded, changedCount } = gradePersistedState(persisted, ouGradeIndex);
  if (changedCount > 0) {
    persisted = writePersistedState(graded);
    syncStateFromPersisted(state.bookId);
  }
  return changedCount;
}

function refreshLiveBookLines() {
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
    let card;
    if (leg.cardKind === "matchup") {
      card = lookupMatchupCard(built.cards, leg.p1_dg_id, leg.p2_dg_id);
      if (!card) {
        showToast(`${leg.playerName} matchup — book line no longer posted`);
        return;
      }
      lockedLegs.push(legFromCard(card, leg.side));
    } else {
      card = lookupDirectCard(built.cards, leg.dg_id, leg.market);
      if (!card) {
        showToast(`${leg.playerName} — book line no longer posted`);
        return;
      }
      lockedLegs.push(legFromCard(card, leg.side));
    }
  }

  state.bankroll -= stake;
  const entry = {
    id: crypto.randomUUID(),
    placedAt: new Date().toISOString(),
    bookId: book.id,
    eventName: built.eventName,
    round: built.round,
    stake,
    playType: book.id === "prizepicks" ? state.playType : book.mode === "sportsbook" ? "single" : "parlay",
    result: "open",
    legs: lockedLegs,
  };
  state.history = [entry, ...state.history];
  state.slip = [];
  saveBookState();
  syncStateFromPersisted(state.bookId);
  gradeAllBooksInPersisted();
  renderAll();
  showToast(`Placed ${fmtUsd(stake)} · ${book.label}`);
}

async function loadGradeData() {
  try {
    const res = await fetch(`${VS_ACTUAL_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return false;
    ouGradeIndex = buildOuGradeIndexFromCsvText(await res.text());
    return Boolean(ouGradeIndex);
  } catch {
    return false;
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
    cards = cards.filter((c) => {
      if (c.cardKind === "matchup") {
        return (
          String(c.p1_player_name || "").toLowerCase().includes(q) ||
          String(c.p2_player_name || "").toLowerCase().includes(q) ||
          String(c.p1_short || "").toLowerCase().includes(q) ||
          String(c.p2_short || "").toLowerCase().includes(q)
        );
      }
      return (
        c.playerName.toLowerCase().includes(q) ||
        c.market.toLowerCase().includes(q) ||
        marketShortLabel(c.market).toLowerCase().includes(q)
      );
    });
  }

  const slipKeys = new Set(state.slip.map(slipLineKey));

  if (!cards.length) {
    board.innerHTML = `<div class="empty-board">No ${book.label} lines with book-posted odds.${built.fetchError ? ` ${built.fetchError}` : ""}</div>`;
    return;
  }

  if (isMatchupBook(book)) {
    board.innerHTML = cards
      .map((card) => {
        const inSlip = state.slip.some((l) => l.cardKey === card.cardKey);
        const p1Sel = slipKeys.has(`${card.cardKey}|p1`);
        const p2Sel = slipKeys.has(`${card.cardKey}|p2`);
        const mkSide = (side, label, selected) => {
          const bookOdds = sideBookOddsFromCard(card, side);
          return `<button type="button" class="side-btn matchup-side ${side}${selected ? " selected" : ""}" data-side="${side}" data-card-key="${esc(card.cardKey)}">
            <span class="side-label">${esc(label)}</span>
            <span class="side-odds">${esc(formatPostedOdds(book, bookOdds))}</span>
          </button>`;
        };
        return `<article class="prop-card matchup-card${inSlip ? " in-slip" : ""}">
          <div class="prop-meta"><span class="prop-market">${esc(marketShortLabel(card.market))}</span></div>
          <div class="matchup-sides">
            ${mkSide("p1", card.p1_short || card.p1_player_name, p1Sel)}
            <span class="matchup-vs">vs</span>
            ${mkSide("p2", card.p2_short || card.p2_player_name, p2Sel)}
          </div>
        </article>`;
      })
      .join("");

    board.querySelectorAll(".matchup-side").forEach((btn) => {
      btn.addEventListener("click", () => {
        const side = btn.getAttribute("data-side");
        const cardKey = btn.getAttribute("data-card-key");
        const card = cards.find((c) => c.cardKey === cardKey);
        if (card) toggleSlipLeg(legFromCard(card, side));
      });
    });
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
  paintBoard(buildLivePropCards(projections, state.bookId));
}

function renderSlip() {
  const book = bookById(state.bookId);
  const legsEl = document.getElementById("slip-legs");
  const emptyEl = document.getElementById("slip-empty");

  if (!state.slip.length) {
    legsEl.innerHTML = "";
    emptyEl.hidden = false;
    emptyEl.textContent = isMatchupBook(book)
      ? "Pick a golfer in a round matchup."
      : book.mode === "sportsbook"
        ? "Select Over or Under on a prop."
        : `Pick ${book.minPicks}–${book.maxPicks} legs for your entry.`;
  } else {
    emptyEl.hidden = true;
    legsEl.innerHTML = state.slip
      .map(
        (leg) => `<div class="slip-leg">
        <div class="slip-leg-main">
          <div class="slip-leg-player">${esc(leg.playerName)}</div>
          <div class="slip-leg-pick">${esc(describeLegPick(book, leg))}</div>
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
      const statusKey = entry.result === "push" ? "push" : entry.result;
      const statusText = entryStatusLabel(entry);
      const pnl =
        entry.result === "open"
          ? ""
          : `${(entry.pnl || 0) >= 0 ? "+" : ""}${fmtUsd(entry.pnl)}`;
      const roundLabel = entry.round ? `R${entry.round}` : "";
      const when = new Date(entry.placedAt).toLocaleString(undefined, {
        month: "short",
        day: "numeric",
        hour: "numeric",
        minute: "2-digit",
      });
      return `<div class="history-entry">
        <div class="entry-head">
          <div class="entry-title">
            <span class="entry-book">${esc(book.label)}</span>
            <span class="entry-stake">${fmtUsd(entry.stake)}</span>
            ${roundLabel ? `<span class="entry-round">${esc(roundLabel)}</span>` : ""}
          </div>
          <span class="entry-status ${esc(statusKey)}">${esc(statusText)}${pnl ? ` · ${esc(pnl)}` : ""}</span>
        </div>
        <div class="entry-legs">${esc(describeEntryBet(entry))}</div>
        <div class="entry-meta">${esc(entry.eventName || "Golf")} · ${esc(when)}</div>
      </div>`;
    })
    .join("");
}

function renderAll() {
  document.getElementById("filter-market").closest(".toolbar-select").hidden = isMatchupBook(bookById(state.bookId));
  renderSlip();
  renderHistory();
  document.getElementById("bankroll-reset").value = String(state.startingBankroll);
  renderBoard();
}

async function refreshGradeDataInBackground() {
  if (await loadGradeData()) {
    const n = gradeAllBooksInPersisted();
    if (n > 0) showToast(`Graded ${n} bet${n === 1 ? "" : "s"}`);
  }
  renderHistory();
  renderSlip();
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
    const [linesOk] = await Promise.all([loadPaperBookLines(), loadGradeData()]);
    if (!linesOk) {
      document.getElementById("props-board").innerHTML =
        `<div class="empty-board">Missing paper-book-lines.json — run npm run push:live or npm run bake:paper-book</div>`;
      return;
    }
    gradeAllBooksInPersisted();
    renderAll();
  } catch (err) {
    document.getElementById("props-board").innerHTML = `<div class="empty-board">${esc(err.message)}</div>`;
    return;
  }

  refreshGradeDataInBackground();

  setInterval(async () => {
    try {
      await loadPaperBookLines();
      if (await loadGradeData()) gradeAllBooksInPersisted();
      renderAll();
    } catch {
      /* ignore poll errors */
    }
  }, 120_000);
}

boot();
