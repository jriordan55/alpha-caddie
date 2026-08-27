/**
 * Paper book bankroll + bet history persistence (localStorage + optional repo JSON).
 */

export const STORAGE_VERSION = 2;
export const HISTORY_URL = "./paper-book-history.json";

const BOOK_IDS = ["draftkings", "prizepicks", "sleeper", "underdog", "kalshi", "dk_matchups"];
export const PAPER_BOOK_IDS = BOOK_IDS;

/** Stable key — do not scope by hostname (localhost vs 127.0.0.1 vs Pages was wiping history). */
function storageKey() {
  return `alphaCaddie_paperBook_all_v${STORAGE_VERSION}`;
}

function legacyHostnameKey() {
  const host = typeof location !== "undefined" ? location.hostname || "local" : "local";
  return `alphaCaddie_paperBook_all_${host}_v${STORAGE_VERSION}`;
}

function defaultBookState() {
  return {
    bankroll: 1000,
    startingBankroll: 1000,
    playType: "power",
    stake: 10,
    history: [],
  };
}

export function emptyPersistedState() {
  /** @type {Record<string, object>} */
  const books = {};
  for (const id of BOOK_IDS) books[id] = defaultBookState();
  return { version: STORAGE_VERSION, updated_at: new Date().toISOString(), books };
}

function normalizeBookState(raw) {
  const d = defaultBookState();
  if (!raw || typeof raw !== "object") return d;
  return {
    bankroll: Math.max(0, Number(raw.bankroll) || d.bankroll),
    startingBankroll: Math.max(100, Number(raw.startingBankroll) || d.startingBankroll),
    playType: raw.playType === "flex" ? "flex" : "power",
    stake: Math.max(1, Number(raw.stake) || d.stake),
    history: Array.isArray(raw.history) ? raw.history : [],
  };
}

export function normalizePersistedState(raw) {
  const out = emptyPersistedState();
  if (!raw || typeof raw !== "object") return out;
  for (const id of BOOK_IDS) {
    if (raw.books?.[id]) out.books[id] = normalizeBookState(raw.books[id]);
  }
  if (raw.updated_at) out.updated_at = String(raw.updated_at);
  return out;
}

function parseStored(raw) {
  if (!raw) return null;
  try {
    return normalizePersistedState(JSON.parse(raw));
  } catch {
    return null;
  }
}

function readLocalRaw(key) {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
}

function migrateV1() {
  /** @type {Record<string, object>} */
  const books = {};
  let found = false;
  for (const id of BOOK_IDS) {
    try {
      const raw = localStorage.getItem(`alphaCaddie_paperBook_${id}_v1`);
      if (!raw) continue;
      books[id] = normalizeBookState(JSON.parse(raw));
      found = true;
    } catch {
      /* ignore */
    }
  }
  return found ? normalizePersistedState({ books }) : null;
}

/** Collect every known local copy (stable key + old hostname keys + v1). */
function collectLocalCandidates() {
  /** @type {object[]} */
  const found = [];
  const seen = new Set();

  const tryAdd = (state) => {
    if (!state) return;
    const sig = JSON.stringify(state);
    if (seen.has(sig)) return;
    seen.add(sig);
    found.push(state);
  };

  tryAdd(parseStored(readLocalRaw(storageKey())));
  tryAdd(parseStored(readLocalRaw(legacyHostnameKey())));

  // Sweep other hostname-scoped v2 keys left from older builds
  try {
    for (let i = 0; i < localStorage.length; i++) {
      const key = localStorage.key(i);
      if (!key?.startsWith("alphaCaddie_paperBook_all_") || !key.endsWith(`_v${STORAGE_VERSION}`)) {
        continue;
      }
      if (key === storageKey()) continue;
      tryAdd(parseStored(readLocalRaw(key)));
    }
  } catch {
    /* private mode */
  }

  tryAdd(migrateV1());
  return found;
}

function mergeHistories(localHist, remoteHist) {
  const byId = new Map();
  for (const e of [...(remoteHist || []), ...(localHist || [])]) {
    if (!e?.id) continue;
    byId.set(String(e.id), e);
  }
  return [...byId.values()].sort((a, b) => String(b.placedAt || "").localeCompare(String(a.placedAt || "")));
}

function hasAnyHistory(state) {
  return BOOK_IDS.some((id) => (state?.books?.[id]?.history?.length || 0) > 0);
}

export function mergePersistedStates(local, remote) {
  const a = normalizePersistedState(local);
  const b = normalizePersistedState(remote);
  const out = emptyPersistedState();
  out.updated_at = [a.updated_at, b.updated_at].filter(Boolean).sort().pop() || new Date().toISOString();

  for (const id of BOOK_IDS) {
    const la = a.books[id];
    const lb = b.books[id];
    const localTs = Date.parse(a.updated_at || "") || 0;
    const remoteTs = Date.parse(b.updated_at || "") || 0;
    const pickBank = remoteTs > localTs ? lb : la;
    out.books[id] = {
      ...pickBank,
      history: mergeHistories(la.history, lb.history),
    };
  }
  return out;
}

function mergeMany(states) {
  return states.reduce((acc, s) => mergePersistedStates(acc, s), emptyPersistedState());
}

export function readLocal() {
  const candidates = collectLocalCandidates();
  if (!candidates.length) return null;
  return mergeMany(candidates);
}

export async function loadPersistedState() {
  let state = readLocal() || emptyPersistedState();

  try {
    const res = await fetch(`${HISTORY_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (res.ok) {
      const remote = normalizePersistedState(await res.json());
      // Only merge repo JSON when it actually has bets. Empty committed file must never
      // clobber browser history on first load after a hostname/key change.
      if (hasAnyHistory(remote)) {
        state = mergePersistedStates(state, remote);
      }
    }
  } catch {
    /* optional repo file */
  }

  // Always rewrite under the stable key so future loads find history
  // (also migrates any hostname-scoped leftovers).
  writePersistedState(state);
  return state;
}

export function writePersistedState(persisted) {
  const payload = normalizePersistedState(persisted);
  payload.updated_at = new Date().toISOString();
  const json = JSON.stringify(payload);
  try {
    localStorage.setItem(storageKey(), json);
    // Keep legacy hostname key in sync so older tabs still see updates.
    localStorage.setItem(legacyHostnameKey(), json);
  } catch (err) {
    console.warn("[paper-book] Could not save history to localStorage", err);
  }
  return payload;
}

export function bookSlice(persisted, bookId) {
  const slice = normalizeBookState(persisted?.books?.[bookId]);
  return {
    ...slice,
    history: mergeHistories([], slice.history),
  };
}

/** Flat history across all books, newest first. */
export function allHistory(persisted) {
  const rows = [];
  for (const id of BOOK_IDS) {
    const hist = persisted?.books?.[id]?.history;
    if (!Array.isArray(hist)) continue;
    for (const e of hist) {
      if (e?.id) rows.push({ ...e, bookId: e.bookId || id });
    }
  }
  return mergeHistories(rows, []);
}

export function applyBookSlice(persisted, bookId, slice) {
  const latest = readLocal() || persisted;
  const out = normalizePersistedState(latest);
  const storedHistory = out.books[bookId]?.history || [];
  const nextHistory =
    slice?.history != null ? mergeHistories(storedHistory, slice.history) : storedHistory;
  out.books[bookId] = normalizeBookState({
    ...out.books[bookId],
    ...slice,
    history: nextHistory,
  });
  return writePersistedState(out);
}

export function downloadHistoryBackup(persisted) {
  const payload = normalizePersistedState(persisted);
  payload.updated_at = new Date().toISOString();
  const blob = new Blob([`${JSON.stringify(payload, null, 2)}\n`], { type: "application/json" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = "paper-book-history.json";
  a.click();
  URL.revokeObjectURL(url);
}

export async function importHistoryBackupFile(file) {
  const text = await file.text();
  const imported = normalizePersistedState(JSON.parse(text));
  const current = readLocal() || emptyPersistedState();
  return writePersistedState(mergePersistedStates(current, imported));
}
