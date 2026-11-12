/**
 * Onyx Odds golf round O/U → projections.props
 * (Total Score / Birdies / Bogeys / Pars) from api.onyxodds.com.
 *
 * Fixtures: awayTeam = "Round N", homeTeam = tournament name.
 * Markets: player_strokes, player_birdies_or_better, player_bogeys_or_worse, player_pars.
 *
 *   npm run fetch:ox-ou
 *
 * Env:
 *   GOLF_SKIP_OX_OU=1 — skip
 *   ONYX_LEAGUE — default PGA (also tries GOLF)
 *   ONYX_TARGET_ROUND — override
 *   ONYX_COOKIE / ONYX_TOKEN / ONYX_CLIENT_KEY — optional (public board usually works)
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import {
  canonicalRoundOuMarket,
  dedupePropsOnePerPlayerMarket,
  num,
  preferPropsForTargetRound,
  withImpliedFromAmerican,
} from "./pickem-ou-shared.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ONYX_API = String(process.env.ONYX_API_BASE || "https://api.onyxodds.com").replace(/\/$/, "");
const MARKET_MAP = {
  player_strokes: "Total Score",
  player_birdies_or_better: "Birdies",
  player_bogeys_or_worse: "Bogeys",
  player_pars: "Pars",
};

function roundFromAwayTeam(away) {
  const m = String(away || "").trim().match(/^Round\s+(\d+)$/i);
  return m ? Math.round(Number(m[1])) : NaN;
}

function onyxHeaders(league) {
  const headers = {
    "User-Agent": "Mozilla/5.0",
    Accept: "application/json",
    "Accept-Encoding": "gzip, deflate",
    Origin: "https://app.onyxodds.com",
    Referer: `https://app.onyxodds.com/leagues/${league}`,
  };
  const clientKey = String(process.env.ONYX_CLIENT_KEY || "").trim();
  const cookie = String(process.env.ONYX_COOKIE || "").trim();
  const token = String(process.env.ONYX_TOKEN || "").trim();
  if (clientKey) headers["X-Client-Key"] = clientKey;
  if (cookie) headers.Cookie = cookie;
  if (token) headers.Authorization = token.toLowerCase().startsWith("bearer ") ? token : `Bearer ${token}`;
  return headers;
}

async function fetchOnyxMainLines(league) {
  const res = await fetch(`${ONYX_API}/api/odds/mainLines/${encodeURIComponent(league)}`, {
    headers: onyxHeaders(league),
  });
  if (!res.ok) throw new Error(`Onyx ${league} HTTP ${res.status}`);
  return res.json();
}

/**
 * @param {object} board
 * @param {object} payload
 * @param {number} wantRound
 */
export function propsFromOnyxBoard(board, payload = {}, wantRound = NaN) {
  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  const data = board?.data && typeof board.data === "object" ? board.data : {};
  /** @type {Map<string, { player: string, market: string, round: number, line: number, over: number, under: number }>} */
  const pairs = new Map();

  for (const game of Object.values(data)) {
    if (!game || typeof game !== "object") continue;
    const round = roundFromAwayTeam(game.awayTeam);
    if (!Number.isFinite(round)) continue;
    if (Number.isFinite(wantRound) && round !== wantRound) continue;
    const markets = game.markets && typeof game.markets === "object" ? game.markets : {};
    for (const [mktKey, sels] of Object.entries(markets)) {
      const market = MARKET_MAP[mktKey] || canonicalRoundOuMarket(mktKey);
      if (!market || !MARKET_MAP[mktKey]) continue;
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
        source: "onyx",
      }),
    );
  }
  return dedupePropsOnePerPlayerMarket(rows);
}

/**
 * @param {{ payload?: object, targetRound?: number }} opts
 */
export async function fetchOnyxOuProps(opts = {}) {
  if (String(process.env.GOLF_SKIP_OX_OU || "").trim() === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_OX_OU=1)" };
  }
  const payload = opts.payload || {};
  const wantRound =
    Math.round(num(opts.targetRound, NaN)) ||
    Math.round(num(process.env.ONYX_TARGET_ROUND, NaN)) ||
    Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) ||
    1;

  const primary = String(process.env.ONYX_LEAGUE || "PGA").trim() || "PGA";
  const leagues = [...new Set([primary, "PGA", "GOLF"])];
  let props = [];
  let lastErr = "";
  for (const league of leagues) {
    try {
      const board = await fetchOnyxMainLines(league);
      props = propsFromOnyxBoard(board, payload, wantRound);
      if (props.length) break;
    } catch (e) {
      lastErr = e?.message || String(e);
    }
  }
  props = preferPropsForTargetRound(props, wantRound);
  if (!props.length && lastErr) return { props: [], error: lastErr };
  return { props, error: props.length ? undefined : "Onyx returned 0 round O/U props" };
}

async function main() {
  const projPath = join(__dirname, "..", String(process.env.GOLF_PROJECTIONS_FILE || "projections.json").trim() || "projections.json");
  const payload = existsSync(projPath) ? JSON.parse(readFileSync(projPath, "utf8")) : {};
  const hit = await fetchOnyxOuProps({ payload });
  console.log(`[onyx-ou] ${hit.props.length} props${hit.error ? ` (${hit.error})` : ""}`);
  const byM = {};
  for (const p of hit.props) byM[p.market] = (byM[p.market] || 0) + 1;
  console.log(byM);
  for (const p of hit.props.slice(0, 6)) {
    console.log(`  ${p.market} ${p.player_name} R${p.round_num} ${p.line} O${p.over_odds}/U${p.under_odds}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
