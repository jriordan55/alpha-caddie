/**
 * Fetch golf props with odds exactly as each book's API returns them (no pipeline conversion).
 */
import { matchPlayerByGolferLabel } from "../scripts/golfer-name-match.mjs";
import { canonicalRoundOuMarket, num, ROUND_OU_MARKETS } from "../scripts/pickem-ou-shared.mjs";
import { bookById, liveTargetRound, playersForRound } from "./live-book-options-core.mjs";

const SL_HEADERS = {
  Accept: "application/json",
  Origin: "https://sleeper.com",
  Referer: "https://sleeper.com/",
};
const UD_HEADERS = {
  Accept: "application/json",
  Origin: "https://underdogfantasy.com",
  Referer: "https://underdogfantasy.com/",
};

const FETCH_TIMEOUT_MS = 12_000;
const BOOK_CACHE_MS = 45_000;
const SL_PLAYERS_CACHE_MS = 24 * 60 * 60 * 1000;
const SL_PLAYERS_STORAGE_KEY = "alphaCaddie_sleeperGolfPlayers_v1";
const SL_PLAYERS_TS_KEY = "alphaCaddie_sleeperGolfPlayers_ts_v1";

/** @type {Map<string, { built: object, at: number }>} */
const bookCardsCache = new Map();
/** @type {Map<string, Promise<object>>} */
const inFlight = new Map();
/** @type {Map<string, string>|null} */
let sleeperPlayerMapMem = null;
let sleeperPlayerMapMemAt = 0;

const MARKET_CANON = {
  "Total Score": "Total score",
  Birdies: "Birdies",
  Bogeys: "Bogeys",
  GIR: "GIR",
  "Fairways hit": "Fairways hit",
  Pars: "Pars",
  Putts: "Putts",
};

function canonMarket(raw) {
  return MARKET_CANON[raw] || raw;
}

function formatAmericanDisplay(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return "—";
  return v > 0 ? `+${v}` : String(v);
}

/** @returns {{ kind: 'american', raw: number, display: string }|null} */
export function bookOddsFromAmericanRaw(raw) {
  const s = String(raw ?? "").trim().replace(/^\+/, "");
  const v = Number(s);
  if (!Number.isFinite(v) || v === 0) return null;
  const rounded = Math.round(v);
  return { kind: "american", raw: rounded, display: formatAmericanDisplay(rounded) };
}

/** @returns {{ kind: 'multiplier', raw: number, display: string }|null} */
export function bookOddsFromMultiplierRaw(raw) {
  const v = num(raw, NaN);
  if (!Number.isFinite(v) || v <= 1) return null;
  const rounded = Math.round(v * 100) / 100;
  return { kind: "multiplier", raw: rounded, display: `${rounded}x` };
}

/** @returns {{ kind: 'decimal', raw: number, display: string }|null} */
export function bookOddsFromDecimalRaw(raw) {
  const v = num(raw, NaN);
  if (!Number.isFinite(v) || v <= 1) return null;
  const rounded = Math.round(v * 100) / 100;
  return { kind: "decimal", raw: rounded, display: `${rounded}x` };
}

/** Underdog posts american_price first, then decimal_price / payout_multiplier. */
export function bookOddsFromUnderdogOption(opt) {
  if (!opt) return null;
  const am = bookOddsFromAmericanRaw(opt.american_price);
  if (am) return am;
  const dec = bookOddsFromDecimalRaw(opt.decimal_price);
  if (dec) return dec;
  return bookOddsFromMultiplierRaw(opt.payout_multiplier);
}

export function legPayoutMultiplierFromBookOdds(bookOdds) {
  if (!bookOdds) return NaN;
  if (bookOdds.kind === "multiplier") return num(bookOdds.raw, NaN);
  if (bookOdds.kind === "decimal") return num(bookOdds.raw, NaN);
  if (bookOdds.kind === "american") {
    const am = num(bookOdds.raw, NaN);
    if (!Number.isFinite(am)) return NaN;
    return am > 0 ? 1 + am / 100 : 1 + 100 / Math.abs(am);
  }
  return NaN;
}

export function formatBookOddsDisplay(bookOdds) {
  return bookOdds?.display || "—";
}

function fetchWithTimeout(url, options = {}, ms = FETCH_TIMEOUT_MS) {
  const ctrl = new AbortController();
  const timer = setTimeout(() => ctrl.abort(), ms);
  return fetch(url, { ...options, signal: ctrl.signal }).finally(() => clearTimeout(timer));
}

function readSleeperPlayersFromSession() {
  try {
    if (typeof sessionStorage === "undefined") return null;
    const ts = Number(sessionStorage.getItem(SL_PLAYERS_TS_KEY) || 0);
    if (!ts || Date.now() - ts > SL_PLAYERS_CACHE_MS) return null;
    const raw = sessionStorage.getItem(SL_PLAYERS_STORAGE_KEY);
    if (!raw) return null;
    const obj = JSON.parse(raw);
    const map = new Map();
    for (const [id, name] of Object.entries(obj || {})) {
      if (name) map.set(String(id), String(name));
    }
    return map.size ? map : null;
  } catch {
    return null;
  }
}

function writeSleeperPlayersToSession(map) {
  try {
    if (typeof sessionStorage === "undefined") return;
    const obj = {};
    for (const [id, name] of map) obj[id] = name;
    sessionStorage.setItem(SL_PLAYERS_STORAGE_KEY, JSON.stringify(obj));
    sessionStorage.setItem(SL_PLAYERS_TS_KEY, String(Date.now()));
  } catch {
    /* quota / private mode */
  }
}

async function loadSleeperGolfPlayerMap() {
  if (sleeperPlayerMapMem && Date.now() - sleeperPlayerMapMemAt < SL_PLAYERS_CACHE_MS) {
    return sleeperPlayerMapMem;
  }
  const cached = readSleeperPlayersFromSession();
  if (cached) {
    sleeperPlayerMapMem = cached;
    sleeperPlayerMapMemAt = Date.now();
    return cached;
  }

  const res = await fetchWithTimeout("https://api.sleeper.app/players/golf", { headers: SL_HEADERS });
  if (!res.ok) return new Map();
  const body = await res.json();
  const map = new Map();
  for (const [id, p] of Object.entries(body || {})) {
    const name =
      String(p?.full_name || p?.metadata?.full_name || "").trim() ||
      `${p?.first_name || ""} ${p?.last_name || ""}`.trim();
    if (name) map.set(String(id), name);
  }
  sleeperPlayerMapMem = map;
  sleeperPlayerMapMemAt = Date.now();
  writeSleeperPlayersToSession(map);
  return map;
}

function matchFieldPlayer(fieldPlayers, label) {
  return matchPlayerByGolferLabel(fieldPlayers, label);
}

function pushCard(out, ctx) {
  const {
    dg_id,
    playerName,
    market,
    line,
    overBookOdds,
    underBookOdds,
    round,
    eventName,
    bookId,
    cardKey,
    gradeable = true,
  } = ctx;
  if (!Number.isFinite(line) || !overBookOdds || !underBookOdds) return;
  out.push({
    cardKey: cardKey || `${dg_id}|${market}`,
    eventName,
    round,
    dg_id,
    playerName,
    market: canonMarket(market),
    line,
    overOdds: overBookOdds.kind === "american" ? overBookOdds.raw : undefined,
    underOdds: underBookOdds.kind === "american" ? underBookOdds.raw : undefined,
    overBookOdds,
    underBookOdds,
    overPayoutMultiplier: legPayoutMultiplierFromBookOdds(overBookOdds),
    underPayoutMultiplier: legPayoutMultiplierFromBookOdds(underBookOdds),
    oddsSource: "book_api_live",
    bookId,
    gradeable,
  });
}

async function fetchSleeperCards(projections, round, bookId) {
  const fieldPlayers = Array.isArray(projections?.players) ? projections.players : [];
  const eventName = String(projections?.event_name || "").trim();
  const [linesRes, playerMap] = await Promise.all([
    fetchWithTimeout("https://api.sleeper.app/lines/available?sport=golf", { headers: SL_HEADERS }),
    loadSleeperGolfPlayerMap(),
  ]);
  if (!linesRes.ok) throw new Error(`Sleeper API HTTP ${linesRes.status}`);
  const lines = await linesRes.json();
  /** @type {object[]} */
  const cards = [];

  for (const row of Array.isArray(lines) ? lines : []) {
    if (String(row?.sport || "").toLowerCase() !== "golf") continue;
    if (String(row?.status || "").toLowerCase() !== "active") continue;
    const opts = Array.isArray(row.options) ? row.options : [];
    const overOpt = opts.find((o) => String(o.outcome || "").toLowerCase() === "over");
    const underOpt = opts.find((o) => String(o.outcome || "").toLowerCase() === "under");
    if (!overOpt || !underOpt) continue;

    const market = canonicalRoundOuMarket(String(overOpt.wager_type || row.wager_type || row.market_type || ""));
    if (!market || !ROUND_OU_MARKETS.has(market)) continue;

    const lineVal = num(overOpt.outcome_value ?? underOpt.outcome_value ?? row.line, NaN);
    const overBookOdds = bookOddsFromMultiplierRaw(overOpt.payout_multiplier);
    const underBookOdds = bookOddsFromMultiplierRaw(underOpt.payout_multiplier);
    if (!overBookOdds || !underBookOdds) continue;

    const subjectId = String(row.subject_id || overOpt.subject_id || "");
    const playerLabel = playerMap.get(subjectId) || "";
    if (!playerLabel) continue;

    const matched = matchFieldPlayer(fieldPlayers, playerLabel);
    const dg_id = matched ? Math.round(num(matched.dg_id, NaN)) : NaN;
    const playerName = String(matched?.player_name || playerLabel).trim();
    const cardKey = Number.isFinite(dg_id) ? `${dg_id}|${market}` : `sl:${subjectId}|${market}`;

    pushCard(cards, {
      dg_id: Number.isFinite(dg_id) ? dg_id : subjectId,
      playerName,
      market,
      line: lineVal,
      overBookOdds,
      underBookOdds,
      round,
      eventName,
      bookId,
      cardKey,
      gradeable: Number.isFinite(dg_id),
    });
  }
  return cards;
}

async function fetchUnderdogCards(projections, round, bookId) {
  const fieldPlayers = Array.isArray(projections?.players) ? projections.players : [];
  const eventName = String(projections?.event_name || "").trim();
  const sportId = String(projections?.meta?.underdog_sport_id || "PGA").trim() || "PGA";
  let res = await fetchWithTimeout(
    `https://api.underdogfantasy.com/beta/v5/over_under_lines?sport_id=${encodeURIComponent(sportId)}`,
    { headers: UD_HEADERS },
  );
  if (!res.ok) throw new Error(`Underdog API HTTP ${res.status}`);
  let body = await res.json();
  if (!(body?.over_under_lines || []).length) {
    res = await fetchWithTimeout("https://api.underdogfantasy.com/beta/v5/over_under_lines", {
      headers: UD_HEADERS,
    });
    if (!res.ok) throw new Error(`Underdog API HTTP ${res.status}`);
    body = await res.json();
  }

  const players = new Map((body?.players || []).map((p) => [String(p.id), p]));
  const appearances = new Map((body?.appearances || []).map((a) => [String(a.id), a]));

  /** @type {object[]} */
  const cards = [];
  for (const line of body?.over_under_lines || []) {
    if (String(line?.status || "").toLowerCase() === "suspended") continue;
    const ou = line?.over_under || {};
    const stat = ou?.appearance_stat || {};
    const market = canonicalRoundOuMarket(stat.display_stat || stat.stat || ou.title);
    if (!market || !ROUND_OU_MARKETS.has(market)) continue;

    const lineVal = num(line?.stat_value, NaN);
    if (!Number.isFinite(lineVal)) continue;

    const opts = Array.isArray(line.options) ? line.options : [];
    const higher =
      opts.find((o) => String(o.choice || "").toLowerCase() === "higher") ||
      opts.find((o) => String(o.choice_id || "").toLowerCase().includes("over"));
    const lower =
      opts.find((o) => String(o.choice || "").toLowerCase() === "lower") ||
      opts.find((o) => String(o.choice_id || "").toLowerCase().includes("under"));

    const overBookOdds = bookOddsFromUnderdogOption(higher);
    const underBookOdds = bookOddsFromUnderdogOption(lower);
    if (!overBookOdds || !underBookOdds) continue;

    const appearanceId = String(stat.appearance_id || "");
    const app = appearances.get(appearanceId);
    const player = players.get(String(app?.player_id || ""));
    if (!player) continue;

    const playerLabel = `${player.first_name || ""} ${player.last_name || ""}`.trim();
    if (!playerLabel) continue;

    const matched = matchFieldPlayer(fieldPlayers, playerLabel);
    const dg_id = matched ? Math.round(num(matched.dg_id, NaN)) : NaN;
    if (!Number.isFinite(dg_id)) continue;

    pushCard(cards, {
      dg_id,
      playerName: String(matched.player_name || playerLabel).trim(),
      market,
      line: lineVal,
      overBookOdds,
      underBookOdds,
      round,
      eventName,
      bookId,
    });
  }
  return cards;
}

/** DraftKings — browser uses server-scraped projections (Playwright); odds are DK displayOdds american. */
function fetchDraftKingsCards(projections, round, bookId) {
  const players = playersForRound(projections, round);
  const eventName = String(projections?.event_name || "").trim();
  /** @type {object[]} */
  const cards = [];

  for (const row of Array.isArray(projections?.props) ? projections.props : []) {
    if (String(row?.source || "").toLowerCase() !== "draftkings") continue;
    let rnd = Math.round(num(row.round_num ?? row.display_round, NaN));
    if (!Number.isFinite(rnd) || rnd < 1) rnd = round;
    if (rnd !== round) continue;

    const market = canonMarket(String(row.market || "").trim());
    const dg_id = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg_id)) continue;
    const player = players.get(dg_id);
    if (!player) continue;

    const overBookOdds = bookOddsFromAmericanRaw(row.over_odds);
    const underBookOdds = bookOddsFromAmericanRaw(row.under_odds);
    if (!overBookOdds || !underBookOdds) continue;

    pushCard(cards, {
      dg_id,
      playerName: String(player.player_name || row.player_name || "").trim(),
      market,
      line: num(row.line, NaN),
      overBookOdds,
      underBookOdds,
      round,
      eventName,
      bookId,
    });
  }
  return cards;
}

/**
 * Return cached book lines if still fresh (for instant paint).
 */
export function peekCachedBookCards(bookId) {
  const hit = bookCardsCache.get(bookId);
  if (!hit) return null;
  return { ...hit.built, fromCache: true, cacheAgeMs: Date.now() - hit.at };
}

async function fetchDirectBookCardsUncached(projections, bookId) {
  const book = bookById(bookId);
  const round = liveTargetRound(projections);
  const roundLabel =
    String(projections?.meta?.display_round_label || projections?.display_round_label || "").trim() ||
    `R${round}`;
  const eventName = String(projections?.event_name || projections?.meta?.event_name || "").trim();
  const fetchedAt = new Date().toISOString();

  let cards = [];
  let fetchError = "";

  try {
    if (book.id === "draftkings") {
      cards = fetchDraftKingsCards(projections, round, book.id);
    } else if (book.id === "sleeper") {
      cards = await fetchSleeperCards(projections, round, book.id);
    } else if (book.id === "underdog") {
      cards = await fetchUnderdogCards(projections, round, book.id);
    } else if (book.id === "prizepicks") {
      fetchError = "PrizePicks partner API does not expose per-pick posted odds for standard lines";
    }
  } catch (err) {
    fetchError =
      err?.name === "AbortError" ? "Book API timed out — try again" : err?.message || String(err);
  }

  for (const c of cards) {
    c.fetchedAt = fetchedAt;
    c.oddsSource = book.id === "draftkings" ? "draftkings_api_scrape" : "book_api_live";
  }

  cards.sort((a, b) => a.playerName.localeCompare(b.playerName));

  return {
    round,
    roundLabel,
    eventName,
    cards,
    hasRealPostedOdds: cards.length > 0,
    linesInFeed: cards.length,
    fetchError,
    fetchedAt,
    book,
    fromCache: false,
  };
}

/**
 * Live book lines for one sportsbook.
 * @param {object} projections — for field matching + DK scrape cache
 * @param {string} bookId
 * @param {{ force?: boolean }} [opts]
 */
export async function fetchDirectBookCards(projections, bookId, opts = {}) {
  const force = opts.force === true;
  const cached = bookCardsCache.get(bookId);
  if (!force && cached && Date.now() - cached.at < BOOK_CACHE_MS) {
    return { ...cached.built, fromCache: true, cacheAgeMs: Date.now() - cached.at };
  }

  const pending = inFlight.get(bookId);
  if (pending) return pending;

  const job = fetchDirectBookCardsUncached(projections, bookId)
    .then((built) => {
      bookCardsCache.set(bookId, { built, at: Date.now() });
      return built;
    })
    .finally(() => {
      inFlight.delete(bookId);
    });

  inFlight.set(bookId, job);
  return job;
}

export function sideBookOddsFromCard(card, side) {
  return side === "under" ? card.underBookOdds : card.overBookOdds;
}

export function sidePayoutMultiplierFromCard(card, side) {
  return side === "under" ? card.underPayoutMultiplier : card.overPayoutMultiplier;
}

export function lookupDirectCard(cards, dgId, market) {
  const mkt = canonMarket(market);
  const id = dgId;
  return (
    cards.find(
      (c) =>
        c.market === mkt &&
        (c.dg_id === id || String(c.dg_id) === String(id) || c.cardKey.startsWith(`${id}|`)),
    ) || null
  );
}
