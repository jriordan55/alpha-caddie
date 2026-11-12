/**
 * Courtside golf round O/U → projections.props from api.courtside.app
 * (Total Score / Birdies / Bogeys / Pars / GIR / Fairways hit).
 *
 * Public board (same shape as Onyx Round fixtures):
 *   GET /api/odds/mainLines?league=PGA
 * Fixtures: awayTeam = "Round N", homeTeam = tournament.
 *
 *   npm run fetch:cs-ou
 *
 * Env:
 *   GOLF_SKIP_CS_OU=1 — skip
 *   COURTSIDE_LEAGUE — default PGA
 *   CS_TARGET_ROUND — override
 *   COURTSIDE_COOKIE / COURTSIDE_TOKEN — optional (public board works without)
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import {
  dedupePropsOnePerPlayerMarket,
  num,
  preferPropsForTargetRound,
  withImpliedFromAmerican,
} from "./pickem-ou-shared.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const COURTSIDE_API = String(process.env.COURTSIDE_API_BASE || "https://api.courtside.app").replace(/\/$/, "");
const MARKET_MAP = {
  player_strokes: "Total Score",
  player_birdies_or_better: "Birdies",
  player_bogeys_or_worse: "Bogeys",
  player_pars: "Pars",
  player_greens_in_regulation: "GIR",
  player_fairways_hit: "Fairways hit",
};

function roundFromAwayTeam(away) {
  const m = String(away || "").trim().match(/^Round\s+(\d+)$/i);
  return m ? Math.round(Number(m[1])) : NaN;
}

function courtsideHeaders() {
  const headers = {
    "User-Agent": "Mozilla/5.0",
    Accept: "application/json",
    "Accept-Encoding": "gzip, deflate",
    "Content-Type": "application/json",
    "x-platform": "web",
    Origin: "https://pick.courtside.app",
    Referer: "https://pick.courtside.app/",
  };
  const cookie = String(process.env.COURTSIDE_COOKIE || "").trim();
  const token = String(process.env.COURTSIDE_TOKEN || "").trim();
  if (cookie) headers.Cookie = cookie;
  if (token) headers.Authorization = token.toLowerCase().startsWith("bearer ") ? token : `Bearer ${token}`;
  return headers;
}

async function fetchCourtsideMainLines(league) {
  const url = `${COURTSIDE_API}/api/odds/mainLines?league=${encodeURIComponent(league)}`;
  const res = await fetch(url, { headers: courtsideHeaders() });
  if (!res.ok) throw new Error(`Courtside ${league} HTTP ${res.status}`);
  return res.json();
}

/** Courtside returns fixtures at the top level; Onyx wraps them in `{ data }`. */
function fixturesFromBoard(board) {
  if (!board || typeof board !== "object") return {};
  if (board.data && typeof board.data === "object" && !Array.isArray(board.data)) return board.data;
  return board;
}

/**
 * @param {object} board
 * @param {object} payload
 * @param {number} wantRound
 */
export function propsFromCourtsideBoard(board, payload = {}, wantRound = NaN) {
  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  const data = fixturesFromBoard(board);
  /** @type {Map<string, { player: string, market: string, round: number, line: number, over: number, under: number }>} */
  const pairs = new Map();

  for (const game of Object.values(data)) {
    if (!game || typeof game !== "object") continue;
    if (game.isFuture) continue;
    const round = roundFromAwayTeam(game.awayTeam);
    if (!Number.isFinite(round)) continue;
    if (Number.isFinite(wantRound) && round !== wantRound) continue;
    const markets = game.markets && typeof game.markets === "object" ? game.markets : {};
    for (const [mktKey, sels] of Object.entries(markets)) {
      const market = MARKET_MAP[mktKey];
      if (!market) continue;
      if (!sels || typeof sels !== "object") continue;
      for (const sel of Object.values(sels)) {
        if (!sel || typeof sel !== "object") continue;
        if (sel.isLocked) continue;
        const player = String(sel.selection || "").trim();
        const side = String(sel.selectionLine || "").trim().toLowerCase();
        const line = num(sel.selectionPoints ?? sel.betPoints, NaN);
        const price = Math.round(num(sel.lastPrice, NaN));
        if (!player || !Number.isFinite(line) || !Number.isFinite(price) || price === 0) continue;
        if (side !== "over" && side !== "under") continue;
        const key = `${player.toLowerCase()}|${market}|${round}|${line}`;
        const hit = pairs.get(key) || { player, market, round, line, over: NaN, under: NaN };
        if (side === "over") hit.over = price;
        else hit.under = price;
        pairs.set(key, hit);
      }
    }
  }

  const rows = [];
  for (const hit of pairs.values()) {
    if (!Number.isFinite(hit.over) || !Number.isFinite(hit.under)) continue;
    const matched = matchPlayerByGolferLabel(fieldPlayers, hit.player);
    rows.push(
      withImpliedFromAmerican({
        player_name: matched ? String(matched.player_name || "").trim() : hit.player,
        dg_id: matched ? Math.round(num(matched.dg_id, NaN)) : NaN,
        market: hit.market,
        line: hit.line,
        over_odds: hit.over,
        under_odds: hit.under,
        round_num: hit.round,
        source: "courtside",
      }),
    );
  }
  return dedupePropsOnePerPlayerMarket(rows);
}

/**
 * @param {{ payload?: object, targetRound?: number }} opts
 */
export async function fetchCourtsideOuProps(opts = {}) {
  if (String(process.env.GOLF_SKIP_CS_OU || "").trim() === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_CS_OU=1)" };
  }
  const payload = opts.payload || {};
  const wantRound =
    Math.round(num(opts.targetRound, NaN)) ||
    Math.round(num(process.env.CS_TARGET_ROUND, NaN)) ||
    Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) ||
    1;

  const primary = String(process.env.COURTSIDE_LEAGUE || "PGA").trim() || "PGA";
  const leagues = [...new Set([primary, "PGA"])];
  let props = [];
  let lastErr = "";
  for (const league of leagues) {
    try {
      const board = await fetchCourtsideMainLines(league);
      props = propsFromCourtsideBoard(board, payload, wantRound);
      if (props.length) break;
    } catch (e) {
      lastErr = e?.message || String(e);
    }
  }
  props = preferPropsForTargetRound(props, wantRound);
  if (!props.length && lastErr) return { props: [], error: lastErr };
  return { props, error: props.length ? undefined : "Courtside returned 0 round O/U props" };
}

async function main() {
  const projPath = join(
    __dirname,
    "..",
    String(process.env.GOLF_PROJECTIONS_FILE || "projections.json").trim() || "projections.json",
  );
  const payload = existsSync(projPath) ? JSON.parse(readFileSync(projPath, "utf8")) : {};
  const hit = await fetchCourtsideOuProps({ payload });
  console.log(`[courtside-ou] ${hit.props.length} props${hit.error ? ` (${hit.error})` : ""}`);
  const byM = {};
  for (const p of hit.props) byM[p.market] = (byM[p.market] || 0) + 1;
  if (Object.keys(byM).length) console.log(byM);
  for (const p of hit.props.slice(0, 8)) {
    console.log(`  ${p.market} ${p.player_name} R${p.round_num} ${p.line} O${p.over_odds}/U${p.under_odds}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
