/**
 * Pull Sleeper Picks golf round O/U props via api.sleeper.app/lines/available.
 *
 *   npm run fetch:sl-ou
 *
 * Env:
 *   GOLF_SKIP_SL_OU=1 — skip
 *   SL_SPORT — default golf
 *   SL_LINES_URL — default https://api.sleeper.app/lines/available
 *   SL_TARGET_ROUND — override round filter
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
  preferPropsForTargetRound,
  withImpliedFromAmerican,
} from "./pickem-ou-shared.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const SL_SPORT = String(process.env.SL_SPORT || "golf").trim().toLowerCase() || "golf";
const SL_LINES_URL = String(
  process.env.SL_LINES_URL || "https://api.sleeper.app/lines/available",
).trim();

const FETCH_HEADERS = {
  Accept: "application/json",
  Origin: "https://sleeper.com",
  Referer: "https://sleeper.com/",
  "User-Agent":
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36",
};

async function loadSleeperGolfPlayers() {
  const url = "https://api.sleeper.app/players/golf";
  const res = await fetch(url, { headers: FETCH_HEADERS });
  if (!res.ok) return new Map();
  const body = await res.json();
  const map = new Map();
  for (const [id, p] of Object.entries(body || {})) {
    const name =
      String(p?.full_name || p?.metadata?.full_name || "").trim() ||
      `${p?.first_name || ""} ${p?.last_name || ""}`.trim();
    if (name) map.set(String(id), name);
  }
  return map;
}

function optionAmerican(opt) {
  return americanFromPayoutMultiplier(opt?.payout_multiplier);
}

/**
 * @param {object[]} lines
 * @param {object} payload
 * @param {Map<string,string>} sleeperPlayers
 * @param {number} targetRound
 */
export function propsFromSleeperLines(lines, payload = {}, sleeperPlayers = new Map(), targetRound = NaN) {
  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  const wantRound = Math.round(num(targetRound, NaN));
  const out = [];

  for (const row of lines || []) {
    const sport = String(row?.sport || "").trim().toLowerCase();
    if (sport !== SL_SPORT && sport !== "pga" && sport !== "golf") continue;
    if (String(row?.status || "").toLowerCase() !== "active") continue;

    const opts = Array.isArray(row.options) ? row.options : [];
    const overOpt = opts.find((o) => String(o.outcome || "").toLowerCase() === "over");
    const underOpt = opts.find((o) => String(o.outcome || "").toLowerCase() === "under");
    if (!overOpt || !underOpt) continue;

    const wager = String(overOpt.wager_type || row.wager_type || row.market_type || "").trim();
    const market = canonicalRoundOuMarket(wager);
    if (!market || !ROUND_OU_MARKETS.has(market)) continue;

    const lineVal = num(overOpt.outcome_value ?? underOpt.outcome_value ?? row.line, NaN);
    if (!Number.isFinite(lineVal)) continue;

    const overOdds = optionAmerican(overOpt);
    const underOdds = optionAmerican(underOpt);
    if (!Number.isFinite(overOdds) || !Number.isFinite(underOdds)) continue;

    const subjectId = String(row.subject_id || overOpt.subject_id || "");
    const playerLabel = sleeperPlayers.get(subjectId) || "";
    if (!playerLabel) continue;

    const matched = matchPlayerByGolferLabel(fieldPlayers, playerLabel);
    const prop = withImpliedFromAmerican({
      player_name: matched ? String(matched.player_name || "").trim() : playerLabel,
      line: lineVal,
      over_odds: overOdds,
      under_odds: underOdds,
      market,
      source: "sleeper",
      sl_odds_method: "sleeper_multiplier",
    });
    if (matched && Number.isFinite(num(matched.dg_id, NaN))) {
      prop.dg_id = Math.round(num(matched.dg_id, NaN));
    }
    if (Number.isFinite(wantRound) && wantRound >= 1 && wantRound <= 4) {
      prop.round_num = wantRound;
    }
    out.push(prop);
  }

  return preferPropsForTargetRound(dedupePropsOnePerPlayerMarket(out), targetRound);
}

/**
 * @param {{ payload?: object, players?: object[], targetRound?: number }} [opts]
 */
export async function fetchSleeperOuProps(opts = {}) {
  if (process.env.GOLF_SKIP_SL_OU === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_SL_OU=1)" };
  }
  const payload = opts.payload || { players: opts.players || [] };
  const players = Array.isArray(payload.players) ? payload.players : opts.players || [];
  const targetRound = Math.round(
    num(opts.targetRound ?? opts.displayRound ?? process.env.SL_TARGET_ROUND ?? NaN, NaN),
  );
  console.log(
    `[sleeper-ou] sport=${SL_SPORT} api=${SL_LINES_URL} players=${players.length}${Number.isFinite(targetRound) ? ` targetRound=R${targetRound}` : ""}`,
  );

  let lines = [];
  let error = null;
  try {
    const res = await fetch(SL_LINES_URL, { headers: FETCH_HEADERS });
    const text = await res.text();
    let body;
    try {
      body = JSON.parse(text);
    } catch {
      return { props: [], error: `non-JSON (${text.slice(0, 120)})` };
    }
    if (!res.ok) return { props: [], error: body?.errors?.message || `HTTP ${res.status}` };
    lines = Array.isArray(body) ? body : [];
  } catch (e) {
    return { props: [], error: e.message || String(e) };
  }

  const sleeperPlayers = await loadSleeperGolfPlayers();
  let props = propsFromSleeperLines(lines, { ...payload, players }, sleeperPlayers, targetRound);

  if (!props.length) {
    const golfish = lines.filter((r) => /golf|pga/i.test(String(r?.sport || "")));
    error =
      golfish.length === 0
        ? "0 Sleeper golf lines posted (lines/available has no sport=golf rows yet)"
        : "0 parsed Sleeper golf rows (stat mapping / player id miss)";
    console.warn("[sleeper-ou]", error);
    return { props: [], error };
  }
  console.log(`[sleeper-ou] ${props.length} prop row(s)`);
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
  const { props, error } = await fetchSleeperOuProps({ payload, targetRound });
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
