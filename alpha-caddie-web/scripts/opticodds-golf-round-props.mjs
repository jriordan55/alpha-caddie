/**
 * Shared OpticOdds golf round-prop fetch (Courtside / Onyx Odds via aggregator).
 *
 * Env:
 *   OPTICODDS_API_KEY — required
 *   OPTICODDS_API_BASE — default https://api.opticodds.com/api/v3
 *   OPTICODDS_GOLF_LEAGUE — default pga
 */
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import {
  canonicalRoundOuMarket,
  dedupePropsOnePerPlayerMarket,
  num,
  preferPropsForTargetRound,
  withImpliedFromAmerican,
} from "./pickem-ou-shared.mjs";

const API_BASE = String(process.env.OPTICODDS_API_BASE || "https://api.opticodds.com/api/v3").replace(/\/$/, "");

const MARKET_MAP = {
  player_strokes: "Total Score",
  strokes: "Total Score",
  round_score: "Total Score",
  player_round_score: "Total Score",
  player_birdies_or_better: "Birdies",
  birdies_or_better: "Birdies",
  birdies: "Birdies",
  player_birdies: "Birdies",
  player_bogeys_or_worse: "Bogeys",
  bogeys_or_worse: "Bogeys",
  bogeys: "Bogeys",
  player_bogeys: "Bogeys",
  player_pars: "Pars",
  pars: "Pars",
};

function opticKey() {
  return String(process.env.OPTICODDS_API_KEY || process.env.OPTIC_ODDS_API_KEY || "").trim();
}

function roundFromDisplay(away) {
  const m = String(away || "").trim().match(/^Round\s+(\d+)$/i);
  return m ? Math.round(Number(m[1])) : NaN;
}

function marketFromOptic(idOrName) {
  const raw = String(idOrName || "").trim();
  const key = raw.toLowerCase().replace(/\s+/g, "_");
  if (MARKET_MAP[key]) return MARKET_MAP[key];
  const canon = canonicalRoundOuMarket(raw);
  return MARKET_MAP[String(canon || "").toLowerCase().replace(/\s+/g, "_")] || canon || "";
}

async function opticGet(path, params = {}) {
  const key = opticKey();
  if (!key) throw new Error("OPTICODDS_API_KEY not set");
  const url = new URL(`${API_BASE}${path.startsWith("/") ? path : `/${path}`}`);
  for (const [k, v] of Object.entries(params)) {
    if (v == null || v === "") continue;
    if (Array.isArray(v)) {
      for (const item of v) url.searchParams.append(k, String(item));
    } else {
      url.searchParams.set(k, String(v));
    }
  }
  const res = await fetch(url, {
    headers: {
      Accept: "application/json",
      "User-Agent": "Mozilla/5.0",
      "X-Api-Key": key,
    },
  });
  if (!res.ok) {
    const text = await res.text().catch(() => "");
    throw new Error(`OpticOdds ${path} HTTP ${res.status}${text ? `: ${text.slice(0, 160)}` : ""}`);
  }
  return res.json();
}

/**
 * @param {{ sportsbook: string, payload?: object, targetRound?: number, source?: string }} opts
 */
export async function fetchOpticOddsGolfRoundProps(opts = {}) {
  const sportsbook = String(opts.sportsbook || "").trim();
  if (!sportsbook) return { props: [], error: "sportsbook required" };
  if (!opticKey()) {
    return {
      props: [],
      error: `skipped (OPTICODDS_API_KEY unset — required for ${sportsbook})`,
    };
  }
  const payload = opts.payload || {};
  const source = String(opts.source || sportsbook).trim().toLowerCase() || sportsbook;
  const wantRound =
    Math.round(num(opts.targetRound, NaN)) ||
    Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) ||
    1;
  const league = String(process.env.OPTICODDS_GOLF_LEAGUE || "pga").trim() || "pga";

  let fixturesPayload;
  try {
    fixturesPayload = await opticGet("/fixtures", { sport: "golf", league });
  } catch (e) {
    return { props: [], error: e?.message || String(e) };
  }
  const fixtures = Array.isArray(fixturesPayload?.data)
    ? fixturesPayload.data
    : Array.isArray(fixturesPayload)
      ? fixturesPayload
      : [];

  const roundFixtures = fixtures.filter((f) => {
    const away = f?.away_team_display ?? f?.away_competitors_display ?? f?.away_team ?? "";
    const round = roundFromDisplay(away);
    if (!Number.isFinite(round)) return false;
    if (Number.isFinite(wantRound) && round !== wantRound) return false;
    return true;
  });
  if (!roundFixtures.length) {
    return { props: [], error: `OpticOdds: no Round ${wantRound} golf fixtures for ${league}` };
  }

  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  /** @type {Map<string, { player: string, market: string, round: number, line: number, over: number, under: number }>} */
  const pairs = new Map();

  for (const fix of roundFixtures) {
    const fixtureId = String(fix?.id || fix?.fixture_id || "").trim();
    if (!fixtureId) continue;
    const round = roundFromDisplay(fix?.away_team_display ?? fix?.away_team ?? "");
    let oddsPayload;
    try {
      oddsPayload = await opticGet("/fixtures/odds", {
        sportsbook,
        fixture_id: fixtureId,
      });
    } catch (e) {
      console.warn(`[opticodds-${source}] fixture ${fixtureId}:`, e?.message || e);
      continue;
    }
    const oddsRows = Array.isArray(oddsPayload?.data)
      ? oddsPayload.data
      : Array.isArray(oddsPayload)
        ? oddsPayload
        : [];
    for (const row of oddsRows) {
      const markets = Array.isArray(row?.odds)
        ? row.odds
        : Array.isArray(row?.markets)
          ? row.markets
          : Array.isArray(row)
            ? row
            : [];
      const list = markets.length ? markets : Array.isArray(row?.selections) ? [row] : [];
      for (const odd of list) {
        const market = marketFromOptic(odd?.market_id || odd?.market || odd?.name || row?.market_id || row?.market);
        if (!market || !["Total Score", "Birdies", "Bogeys", "Pars"].includes(market)) continue;
        const player = String(
          odd?.player_name || odd?.selection_name || odd?.name || odd?.selection || "",
        )
          .replace(/\s+(over|under)\b.*/i, "")
          .trim();
        const side = String(odd?.selection_line || odd?.selectionLine || odd?.side || "")
          .trim()
          .toLowerCase();
        const line = num(odd?.points ?? odd?.selection_points ?? odd?.line, NaN);
        let price = Math.round(num(odd?.price ?? odd?.american_odds ?? odd?.odds, NaN));
        if (!Number.isFinite(price) || price === 0) {
          const dec = num(odd?.decimal_price ?? odd?.decimal, NaN);
          if (Number.isFinite(dec) && dec > 1) {
            price = dec >= 2 ? Math.round((dec - 1) * 100) : Math.round(-100 / (dec - 1));
          }
        }
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
        source,
      }),
    );
  }
  let props = preferPropsForTargetRound(dedupePropsOnePerPlayerMarket(rows), wantRound);
  return {
    props,
    error: props.length ? undefined : `OpticOdds ${sportsbook}: 0 round O/U props`,
  };
}
