/**
 * Kalshi golf round-score markets (KXPGAROUNDSCORE) → projections.props rows.
 *
 * Binary YES = Under X.5 strokes. Convert contract $ prices → American O/U odds.
 *
 *   npm run fetch:kl-ou
 *
 * Env:
 *   GOLF_SKIP_KL_OU=1 — skip
 *   KL_SERIES_TICKER — default KXPGAROUNDSCORE
 *   KL_TARGET_ROUND — override round filter
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
const KL_SERIES = String(process.env.KL_SERIES_TICKER || "KXPGAROUNDSCORE").trim();
const KL_API = String(
  process.env.KL_API_BASE || "https://api.elections.kalshi.com/trade-api/v2",
).replace(/\/$/, "");

function dollarsToAmerican(p) {
  const x = Number(p);
  if (!(x > 0.01 && x < 0.99)) return NaN;
  if (x >= 0.5) return Math.round((-100 * x) / (1 - x));
  return Math.round((100 * (1 - x)) / x);
}

function midDollars(bid, ask, last) {
  const b = Number(bid);
  const a = Number(ask);
  if (Number.isFinite(b) && Number.isFinite(a) && b > 0 && a > 0) return (b + a) / 2;
  if (Number.isFinite(a) && a > 0) return a;
  if (Number.isFinite(b) && b > 0) return b;
  const L = Number(last);
  return Number.isFinite(L) && L > 0 ? L : NaN;
}

/**
 * @param {string} title
 * @returns {{ player: string, line: number, round: number } | null}
 */
export function parseKalshiRoundScoreTitle(title) {
  const t = String(title || "").trim();
  // Will Xander Schauffele shoot under 67.5 in Round 1?
  const m = t.match(/^Will\s+(.+?)\s+shoot\s+under\s+(\d+(?:\.\d+)?)\s+in\s+Round\s+(\d+)/i);
  if (!m) return null;
  return { player: m[1].trim(), line: Number(m[2]), round: Math.round(Number(m[3])) };
}

async function fetchAllOpenMarkets(seriesTicker) {
  const out = [];
  let cursor = "";
  for (let i = 0; i < 30; i++) {
    const qs = new URLSearchParams({
      series_ticker: seriesTicker,
      status: "open",
      limit: "200",
    });
    if (cursor) qs.set("cursor", cursor);
    const res = await fetch(`${KL_API}/markets?${qs}`, {
      headers: { Accept: "application/json" },
    });
    if (!res.ok) throw new Error(`Kalshi markets HTTP ${res.status}`);
    const body = await res.json();
    out.push(...(body.markets || []));
    cursor = String(body.cursor || "").trim();
    if (!cursor) break;
  }
  return out;
}

/**
 * @param {{ payload?: object, targetRound?: number }} opts
 */
export async function fetchKalshiOuProps(opts = {}) {
  if (String(process.env.GOLF_SKIP_KL_OU || "").trim() === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_KL_OU=1)" };
  }
  const payload = opts.payload || {};
  const fieldPlayers = Array.isArray(payload.players) ? payload.players : [];
  const wantRound =
    Math.round(num(opts.targetRound, NaN)) ||
    Math.round(num(process.env.KL_TARGET_ROUND, NaN)) ||
    Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) ||
    1;

  let markets;
  try {
    markets = await fetchAllOpenMarkets(KL_SERIES);
  } catch (e) {
    return { props: [], error: e?.message || String(e) };
  }

  const rows = [];
  for (const m of markets || []) {
    const parsed = parseKalshiRoundScoreTitle(m.title || m.yes_sub_title || "");
    if (!parsed || !Number.isFinite(parsed.line)) continue;
    if (Number.isFinite(wantRound) && parsed.round !== wantRound) continue;

    const underP = midDollars(m.yes_bid_dollars, m.yes_ask_dollars, m.last_price_dollars);
    const overP = midDollars(m.no_bid_dollars, m.no_ask_dollars, NaN);
    const underOdds = dollarsToAmerican(underP);
    const overOdds = dollarsToAmerican(
      Number.isFinite(overP) ? overP : Number.isFinite(underP) ? 1 - underP : NaN,
    );
    if (!Number.isFinite(underOdds) || !Number.isFinite(overOdds)) continue;

    const matched = matchPlayerByGolferLabel(fieldPlayers, parsed.player);
    const prop = withImpliedFromAmerican({
      player_name: matched ? String(matched.player_name || "").trim() : parsed.player,
      dg_id: matched ? Math.round(num(matched.dg_id, NaN)) : NaN,
      market: "Total Score",
      line: parsed.line,
      over_odds: overOdds,
      under_odds: underOdds,
      round_num: parsed.round,
      source: "kalshi",
      kalshi_ticker: m.ticker,
    });
    rows.push(prop);
  }

  let props = dedupePropsOnePerPlayerMarket(rows);
  props = preferPropsForTargetRound(props, wantRound);
  return { props, error: props.length ? undefined : "no open Kalshi round-score markets matched field" };
}

async function main() {
  const projPath = join(__dirname, "..", "projections.json");
  const payload = existsSync(projPath) ? JSON.parse(readFileSync(projPath, "utf8")) : {};
  const hit = await fetchKalshiOuProps({ payload });
  console.log(`[kalshi-ou] ${hit.props.length} props${hit.error ? ` (${hit.error})` : ""}`);
  for (const p of hit.props.slice(0, 5)) {
    console.log(`  ${p.player_name} R${p.round_num} ${p.line} O${p.over_odds}/U${p.under_odds}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
