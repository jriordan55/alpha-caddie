/**
 * Paper book bankroll + bet history persistence (localStorage + optional repo JSON).
 */

export const STORAGE_VERSION = 2;
export const HISTORY_URL = "./paper-book-history.json";

const BOOK_IDS = ["draftkings", "prizepicks", "sleeper", "underdog", "kalshi", "dk_matchups"];

function storageKey() {
  return `alphaCaddie_paperBook_all_${location.hostname || "local"}_v${STORAGE_VERSION}`;
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

function readLocal() {
  try {
    const raw = localStorage.getItem(storageKey());
    if (!raw) return null;
    return normalizePersistedState(JSON.parse(raw));
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

function mergeHistories(localHist, remoteHist) {
  const byId = new Map();
  for (const e of [...(remoteHist || []), ...(localHist || [])]) {
    if (!e?.id) continue;
    byId.set(String(e.id), e);
  }
  return [...byId.values()].sort((a, b) => String(b.placedAt || "").localeCompare(String(a.placedAt || "")));
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

export async function loadPersistedState() {
  let state = readLocal() || migrateV1() || emptyPersistedState();

  try {
    const res = await fetch(`${HISTORY_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (res.ok) {
      const remote = normalizePersistedState(await res.json());
      state = mergePersistedStates(state, remote);
      writePersistedState(state);
    }
  } catch {
    /* optional repo file */
  }

  return state;
}

export function writePersistedState(persisted) {
  const payload = normalizePersistedState(persisted);
  payload.updated_at = new Date().toISOString();
  try {
    localStorage.setItem(storageKey(), JSON.stringify(payload));
  } catch {
    /* quota / private mode */
  }
  return payload;
}

export function bookSlice(persisted, bookId) {
  return normalizeBookState(persisted?.books?.[bookId]);
}

export function applyBookSlice(persisted, bookId, slice) {
  const out = normalizePersistedState(persisted);
  out.books[bookId] = normalizeBookState({ ...out.books[bookId], ...slice });
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
