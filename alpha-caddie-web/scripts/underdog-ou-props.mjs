/**
 * Pull Underdog Fantasy PGA round O/U props via api.underdogfantasy.com.
 *
 *   npm run fetch:ud-ou
 *
 * Env:
 *   GOLF_SKIP_UD_OU=1 — skip
 *   UD_SPORT_ID — default PGA
 *   UD_API_BASE — default https://api.underdogfantasy.com/beta/v5
 *   UD_TARGET_ROUND — override round filter
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import {
  ROUND_OU_MARKETS,
  americanFromPayoutMultiplier,
  canonicalRoundOuMarket,
  dedupePropsOnePerPlayerMarket,
  num,
  parseAmericanOdds,
  parseRoundFromText,
  preferPropsForTargetRound,
  withImpliedFromAmerican,
} from "./pickem-ou-shared.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const UD_SPORT_ID = String(process.env.UD_SPORT_ID || "PGA").trim() || "PGA";
const UD_API_BASE = String(
  process.env.UD_API_BASE || "https://api.underdogfantasy.com/beta/v5",
).replace(/\/$/, "");

const FETCH_HEADERS = {
  Accept: "application/json",
  Origin: "https://underdogfantasy.com",
  Referer: "https://underdogfantasy.com/",
  "User-Agent":
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
};

function buildMaps(body) {
  const players = new Map();
  const appearances = new Map();
  const games = new Map();
  const soloGames = new Map();
  for (const p of body?.players || []) players.set(String(p.id), p);
  for (const a of body?.appearances || []) appearances.set(String(a.id), a);
  for (const g of body?.games || []) games.set(String(g.id), g);
  for (const g of body?.solo_games || []) soloGames.set(String(g.id), g);
  return { players, appearances, games, soloGames };
}

function optionAmerican(opt) {
  const am = parseAmericanOdds(opt?.american_price);
  if (Number.isFinite(am)) return am;
  return americanFromPayoutMultiplier(opt?.payout_multiplier ?? opt?.decimal_price);
}

/**
 * @param {object} body
 * @param {object} payload
 * @param {number} targetRound
 */
export function propsFromUnderdogBody(body, payload = {}, targetRound = NaN) {
  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  const { players, appearances, games, soloGames } = buildMaps(body);
  const wantRound = Math.round(num(targetRound, NaN));
  const out = [];

  for (const line of body?.over_under_lines || []) {
    if (String(line?.status || "").toLowerCase() === "suspended") continue;
    const ou = line?.over_under || {};
    const stat = ou?.appearance_stat || {};
    const market = canonicalRoundOuMarket(stat.display_stat || stat.stat || ou.title);
    if (!market || !ROUND_OU_MARKETS.has(market)) continue;

    const lineVal = num(line?.stat_value, NaN);
    if (!Number.isFinite(lineVal)) continue;

    const appearanceId = String(stat.appearance_id || "");
    const app = appearances.get(appearanceId);
    const player = players.get(String(app?.player_id || ""));
    if (!player) continue;
    const sid = String(player.sport_id || "").toUpperCase();
    if (sid && sid !== UD_SPORT_ID.toUpperCase() && sid !== "GOLF") continue;

    const playerLabel = `${player.first_name || ""} ${player.last_name || ""}`.trim();
    if (!playerLabel) continue;

    const opts = Array.isArray(line.options) ? line.options : [];
    const higher = opts.find((o) => String(o.choice || "").toLowerCase() === "higher")
      || opts.find((o) => String(o.choice_id || "").toLowerCase().includes("over"));
    const lower = opts.find((o) => String(o.choice || "").toLowerCase() === "lower")
      || opts.find((o) => String(o.choice_id || "").toLowerCase().includes("under"));
    const overOdds = optionAmerican(higher);
    const underOdds = optionAmerican(lower);
    if (!Number.isFinite(overOdds) || !Number.isFinite(underOdds)) continue;

    let roundNum = parseRoundFromText(
      ou.title,
      stat.display_stat,
      higher?.selection_subheader,
      lower?.selection_subheader,
    );
    const matchId = app?.match_id;
    const game = games.get(String(matchId)) || soloGames.get(String(matchId));
    if (!Number.isFinite(roundNum)) {
      roundNum = parseRoundFromText(game?.title, game?.abbreviated_title, game?.match_progress);
    }
    if (!Number.isFinite(roundNum) && Number.isFinite(wantRound)) roundNum = wantRound;

    const matched = matchPlayerByGolferLabel(fieldPlayers, playerLabel);
    const prop = withImpliedFromAmerican({
      player_name: matched ? String(matched.player_name || "").trim() : playerLabel,
      line: lineVal,
      over_odds: overOdds,
      under_odds: underOdds,
      market,
      source: "underdog",
      ud_odds_method: "underdog_api",
    });
    if (matched && Number.isFinite(num(matched.dg_id, NaN))) {
      prop.dg_id = Math.round(num(matched.dg_id, NaN));
    }
    if (Number.isFinite(roundNum) && roundNum >= 1 && roundNum <= 4) prop.round_num = roundNum;
    out.push(prop);
  }

  return preferPropsForTargetRound(dedupePropsOnePerPlayerMarket(out), targetRound);
}

async function fetchUnderdogBody(sportId) {
  const url = `${UD_API_BASE}/over_under_lines?sport_id=${encodeURIComponent(sportId)}`;
  const res = await fetch(url, { headers: FETCH_HEADERS });
  const text = await res.text();
  let body;
  try {
    body = JSON.parse(text);
  } catch {
    return { body: null, error: `non-JSON (${text.slice(0, 120)})`, url };
  }
  if (!res.ok) {
    return { body: null, error: body?.error || `HTTP ${res.status}`, url };
  }
  // Fallback: unfiltered catalog if sport-scoped is empty (some weeks PGA is nested differently).
  if (!(body?.over_under_lines || []).length && sportId) {
    const allUrl = `${UD_API_BASE}/over_under_lines`;
    const allRes = await fetch(allUrl, { headers: FETCH_HEADERS });
    const allText = await allRes.text();
    try {
      const allBody = JSON.parse(allText);
      if (allRes.ok && (allBody?.over_under_lines || []).length) {
        return { body: allBody, error: null, url: allUrl };
      }
    } catch {
      /* keep sport-scoped empty */
    }
  }
  return { body, error: null, url };
}

/**
 * @param {{ payload?: object, players?: object[], targetRound?: number, sportId?: string }} [opts]
 */
export async function fetchUnderdogOuProps(opts = {}) {
  if (process.env.GOLF_SKIP_UD_OU === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_UD_OU=1)" };
  }
  const payload = opts.payload || { players: opts.players || [] };
  const players = Array.isArray(payload.players) ? payload.players : opts.players || [];
  const sportId = String(opts.sportId || UD_SPORT_ID).trim() || "PGA";
  const targetRound = Math.round(
    num(opts.targetRound ?? opts.displayRound ?? process.env.UD_TARGET_ROUND ?? NaN, NaN),
  );
  console.log(
    `[underdog-ou] sport_id=${sportId} api=${UD_API_BASE} players=${players.length}${Number.isFinite(targetRound) ? ` targetRound=R${targetRound}` : ""}`,
  );

  const hit = await fetchUnderdogBody(sportId);
  if (!hit.body) {
    console.warn("[underdog-ou]", hit.error || "empty");
    return { props: [], error: hit.error || "empty Underdog response" };
  }

  let props = propsFromUnderdogBody(hit.body, { ...payload, players }, targetRound);
  if (!props.length) {
    const hint =
      hit.error ||
      "0 parsed Underdog PGA rows (no golf lines posted yet, or stat mapping changed)";
    console.warn("[underdog-ou]", hint);
    return { props: [], error: hint };
  }
  console.log(`[underdog-ou] ${props.length} prop row(s)`);
  return { props, error: null };
}

async function main() {
  const proj = join(__dirname, "..", "projections.json");
  let payload = { players: [] };
  let targetRound = NaN;
  if (existsSync(proj)) {
    try {
      payload = JSON.parse(readFileSync(proj, "utf8"));
      targetRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN));
    } catch {
      /* ignore */
    }
  }
  const { props, error } = await fetchUnderdogOuProps({ payload, targetRound });
  console.log(JSON.stringify({ n: props.length, error: error || null }, null, 2));
  if (props[0]) console.log("sample", props[0]);
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
