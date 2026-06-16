/**
 * Merge DraftKings (and optional CSV / model fallback) round O/U rows into projections.props
 * for the Round projections tab.
 */
import { parse } from "csv-parse/sync";
import { existsSync, readFileSync } from "fs";
import { join, resolve } from "path";
import {
  eventNameToDraftKingsSlug,
  inferDraftKingsLeagueSlugFromProjections,
  inferDraftKingsLeagueUrlFromProjections,
} from "./draftkings-league-url.mjs";
import { fetchDraftKingsOuProps } from "./draftkings-ou-props.mjs";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";

export { eventNameToDraftKingsSlug, inferDraftKingsLeagueUrlFromProjections };

export function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function displayGolferName(nameRaw) {
  const s = String(nameRaw || "").trim();
  const m = s.match(/^([^,]+),\s*(.+)$/);
  if (m) return `${m[2].trim()} ${m[1].trim()}`.trim();
  return s;
}

function normNameLoose(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

export function canonicalizeDkOuPropsAgainstProjections(dkProps, players) {
  if (!Array.isArray(dkProps) || !dkProps.length) return dkProps;
  if (!Array.isArray(players) || !players.length) return dkProps;
  const officialNameByDgId = new Map();
  for (const p of players) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || id <= 0) continue;
    const pn = String(p.player_name || "").trim();
    if (!pn) continue;
    if (!officialNameByDgId.has(id)) officialNameByDgId.set(id, pn);
  }
  for (const r of dkProps) {
    let id = Math.round(num(r.dg_id, NaN));
    if (Number.isFinite(id) && id > 0) {
      const canon = officialNameByDgId.get(id);
      if (canon) r.player_name = canon;
      continue;
    }
    const row = matchPlayerByGolferLabel(players, r.player_name);
    if (row) {
      r.player_name = String(row.player_name || "").trim();
      id = Math.round(num(row.dg_id, NaN));
      if (Number.isFinite(id) && id > 0) r.dg_id = id;
    }
  }
  return dkProps;
}

const ALL_OU_COUNTING_MARKETS = [
  "Total Score",
  "Birdies",
  "Pars",
  "Bogeys",
  "GIR",
  "Fairways hit",
  "Putts",
];

/** Round projections tab: DraftKings only — never store model/CSV lines for these. */
const DK_POSTED_ONLY_MARKETS = new Set([
  "Total Score",
  "Birdies",
  "Pars",
  "Bogeys",
  "GIR",
  "Fairways hit",
]);

const MODEL_FALLBACK_MARKETS = ALL_OU_COUNTING_MARKETS.filter((m) => !DK_POSTED_ONLY_MARKETS.has(m));

const OU_MARKET_PLAYER_FIELD = {
  "Total Score": "total_score",
  Birdies: "birdies",
  Pars: "pars",
  Bogeys: "bogeys",
  GIR: "gir",
  "Fairways hit": "fairways",
  Putts: "putts",
};

function snapHalfLine(x) {
  const v = num(x, NaN);
  if (!Number.isFinite(v)) return NaN;
  return Math.round(v - 0.5) + 0.5;
}

function modelFallbackOuForMarket(players, market, preferredRound) {
  const field = OU_MARKET_PLAYER_FIELD[market];
  if (!field || !Array.isArray(players)) return [];
  const holes = market === "GIR" ? 18 : market === "Fairways hit" ? 14 : null;
  const wantRound = Math.round(num(preferredRound, NaN));
  const roundFilter = Number.isFinite(wantRound) && wantRound >= 1 && wantRound <= 4 ? wantRound : null;
  const out = [];
  for (const p of players) {
    if (roundFilter != null && Math.round(num(p.round, NaN)) !== roundFilter) continue;
    const pn = String(p.player_name || "").trim();
    if (!pn) continue;
    let x = num(p[field], NaN);
    if (!Number.isFinite(x)) continue;
    if (x === 0 || x === 1) continue;
    if (holes != null) {
      if (x > 0 && x <= 1.0001) {
        x = Math.min(holes, Math.max(0, Math.round(x * holes)));
      } else {
        x = Math.min(holes, Math.max(0, Math.round(x)));
      }
    } else if (market !== "Total Score") {
      x = Math.round(x * 10) / 10;
    }
    let L = snapHalfLine(x);
    if (market === "Total Score") L = Math.min(85.5, Math.max(63.5, L));
    else if (market === "GIR") L = Math.min(16.5, Math.max(4.5, L));
    else if (market === "Fairways hit") L = Math.min(13.5, Math.max(2.5, L));
    else if (market === "Putts") L = Math.min(36.5, Math.max(22.5, L));
    else if (market === "Birdies" || market === "Bogeys") L = Math.min(8.5, Math.max(0.5, L));
    else if (market === "Pars") L = Math.min(14.5, Math.max(4.5, L));
    if (!Number.isFinite(L)) continue;
    const dg = Math.round(num(p.dg_id, NaN));
    const o = { player_name: pn, line: L, over_odds: -110, under_odds: -110, market };
    if (Number.isFinite(dg) && dg > 0) o.dg_id = dg;
    out.push(o);
  }
  return out;
}

function withPropSource(rows, source) {
  const s = String(source || "unknown").trim();
  return (Array.isArray(rows) ? rows : []).map((r) => ({ ...r, source: s }));
}

function propPlayerMarketPresenceKey(r, market) {
  const id = Math.round(num(r.dg_id, NaN));
  if (Number.isFinite(id) && id > 0) return `id:${id}|${market}`;
  return `nm:${String(r.player_name || "").trim().toLowerCase()}|${market}`;
}

function propRowHasPostableOdds(r) {
  return (
    Number.isFinite(snapHalfLine(num(r?.line, NaN))) &&
    Number.isFinite(num(r?.over_odds, NaN)) &&
    Number.isFinite(num(r?.under_odds, NaN))
  );
}

/** Active field for the projection round (one row per dg_id). */
export function roundProjectionActivePlayers(players, preferredRound) {
  const wantRound = Math.round(num(preferredRound, NaN));
  const roundFilter = Number.isFinite(wantRound) && wantRound >= 1 && wantRound <= 4 ? wantRound : null;
  const seen = new Set();
  const out = [];
  for (const p of players || []) {
    if (roundFilter != null && Math.round(num(p.round, NaN)) !== roundFilter) continue;
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || id <= 0 || seen.has(id)) continue;
    const pn = String(p.player_name || "").trim();
    if (!pn) continue;
    seen.add(id);
    out.push(p);
  }
  return out;
}

function propRowMatchesPlayerMarket(r, player, market) {
  if (String(r.market || "").trim() !== market) return false;
  if (!propRowHasPostableOdds(r)) return false;
  const wantId = Math.round(num(player.dg_id, NaN));
  const rid = Math.round(num(r.dg_id, NaN));
  if (Number.isFinite(wantId) && wantId > 0 && rid === wantId) return true;
  const pn = String(player.player_name || "").trim().toLowerCase();
  const rn = String(r.player_name || "").trim().toLowerCase();
  if (pn && rn && pn === rn) return true;
  return normNameLoose(displayGolferName(player.player_name)) === normNameLoose(displayGolferName(r.player_name));
}

/** Every active player should have O/U for each counting market (DK, CSV, or model_fallback). */
export function validateRoundProjectionPropsCoverage(players, props, preferredRound) {
  const active = roundProjectionActivePlayers(players, preferredRound);
  const missing = [];
  for (const p of active) {
    for (const mkt of ALL_OU_COUNTING_MARKETS) {
      const hit = (props || []).some((r) => propRowMatchesPlayerMarket(r, p, mkt));
      if (!hit) missing.push({ dg_id: p.dg_id, player_name: p.player_name, market: mkt });
    }
  }
  return { ok: missing.length === 0, missing, activeCount: active.length };
}

function stripNonDkCountingProps(byKey) {
  for (const key of [...byKey.keys()]) {
    const r = byKey.get(key);
    const m = String(r.market || "").trim();
    if (!ALL_OU_COUNTING_MARKETS.includes(m)) continue;
    const src = String(r.source || "").trim().toLowerCase();
    if (src === "csv" || src === "model_fallback") byKey.delete(key);
  }
}

const OU_PROP_CSV_FILES = [
  ["Total Score", "player_props_lines.csv"],
  ["Birdies", "player_props_birdies.csv"],
  ["Pars", "player_props_pars.csv"],
  ["Bogeys", "player_props_bogeys.csv"],
  ["GIR", "player_props_gir.csv"],
  ["Fairways hit", "player_props_fairways.csv"],
  ["Putts", "player_props_putts.csv"],
];

function normalizePropMarketFromRow(row, defaultMkt) {
  const v = String(row.stat || row.market || row.prop_type || "")
    .trim()
    .toLowerCase();
  if (!v) return defaultMkt;
  if (/total|round.?score|^score$|^total$/.test(v)) return "Total Score";
  if (/bog/.test(v)) return "Bogeys";
  if (/bird/.test(v)) return "Birdies";
  if (/par/.test(v)) return "Pars";
  if (/gir|green/.test(v)) return "GIR";
  if (/fairway/.test(v)) return "Fairways hit";
  if (/putt/.test(v)) return "Putts";
  return defaultMkt;
}

function parseOuPropsCsv(absPath, defaultMkt) {
  if (!existsSync(absPath)) return [];
  let rows;
  try {
    const text = readFileSync(absPath, "utf8");
    if (!String(text).trim()) return [];
    rows = parse(text, { columns: true, skip_empty_lines: true, trim: true, relax_column_count: true });
  } catch {
    return [];
  }
  if (!Array.isArray(rows)) return [];
  const out = [];
  for (const row of rows) {
    const pn = String(row.player_name || row.player || row.name || row.golfer || "").trim();
    if (!pn) continue;
    const over = num(row.over_odds ?? row.over, NaN);
    const under = num(row.under_odds ?? row.under, NaN);
    let line = num(row.line, NaN);
    if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;
    const mkt = normalizePropMarketFromRow(row, defaultMkt);
    if (mkt !== "Total Score" && line === Math.floor(line)) line += 0.5;
    const o = { player_name: pn, line, over_odds: over, under_odds: under, market: mkt };
    const dg = Math.round(num(row.dg_id ?? row.dgId, NaN));
    if (Number.isFinite(dg) && dg > 0) o.dg_id = dg;
    out.push(o);
  }
  return out;
}

function loadOuPropsFromRepoCsv(golfModelRoot) {
  const dataDir = join(golfModelRoot, "data");
  const pieces = [];
  for (const [mkt, fn] of OU_PROP_CSV_FILES) {
    pieces.push(...parseOuPropsCsv(join(dataDir, fn), mkt));
  }
  pieces.push(...parseOuPropsCsv(join(dataDir, "player_props_birdies_custom.csv"), "Birdies"));
  const map = new Map();
  for (const r of pieces) {
    map.set(`${r.player_name}|${r.market}|${r.line}`, r);
  }
  return [...map.values()];
}

/**
 * Scrape DraftKings round props and merge into one `props` array (DK authoritative for counting markets).
 * @returns {{ props: object[], nCsv: number, nDk: number, dkError?: string, subcatsUsed?: object }}
 */
export async function refreshRoundProjectionProps(payload, golfModelRoot) {
  const skipCsv = process.env.GOLF_SKIP_PROPS_CSV === "1";
  const skipDk = process.env.GOLF_SKIP_DK_OU === "1";
  const skipModelFallback = String(process.env.GOLF_SKIP_MODEL_FALLBACK_OU || "").trim() === "1";

  if (skipCsv && skipDk) {
    return { props: [], nCsv: 0, nDk: 0, dkError: "skipped (GOLF_SKIP_PROPS_CSV and GOLF_SKIP_DK_OU)" };
  }

  const csvPropsRaw = skipCsv ? [] : loadOuPropsFromRepoCsv(golfModelRoot);
  const csvProps = withPropSource(csvPropsRaw, "csv");
  let dkProps = [];
  let dkError;
  let subcatsUsed;
  let dkLeagueUrl = "";
  let dkLeagueSlug = "";
  let nDkFresh = 0;

  if (!skipDk) {
    try {
      dkLeagueUrl = inferDraftKingsLeagueUrlFromProjections(payload);
      dkLeagueSlug = inferDraftKingsLeagueSlugFromProjections(payload);
      console.log(
        "[dk-round-props] DK scrape:",
        dkLeagueUrl ? dkLeagueUrl : "default URL (set DK_LEAGUE_URL or dk_league_slug on projections.json)",
      );
      const modelRound =
        Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
      const dk = await fetchDraftKingsOuProps({
        players: payload.players,
        targetRound: modelRound,
        ...(dkLeagueUrl ? { leagueUrl: dkLeagueUrl } : {}),
      });
      dkProps = withPropSource(dk.props || [], "draftkings");
      nDkFresh = dkProps.length;
      canonicalizeDkOuPropsAgainstProjections(dkProps, payload.players);
      dkError = dk.error;
      subcatsUsed = dk.subcatsUsed;
      if (!dkProps.length) {
        const priorDk = (Array.isArray(payload.props) ? payload.props : []).filter(
          (r) => String(r?.source || "").trim().toLowerCase() === "draftkings" && propRowHasPostableOdds(r),
        );
        if (priorDk.length) {
          dkProps = priorDk;
          console.warn(
            `[dk-round-props] DK scrape returned 0 rows for ${dkLeagueUrl || "(no url)"} — keeping ${priorDk.length} prior draftkings props`,
          );
        } else {
          console.warn(
            "DraftKings O/U:",
            dk.error && !String(dk.error).startsWith("skipped")
              ? dk.error
              : `0 props for ${dkLeagueUrl || "league URL"} — check slug (dk_league_slug) or Playwright / DK_SITE_SEGMENT`,
          );
        }
      } else if (dk.error && !String(dk.error).startsWith("skipped")) {
        console.warn("DraftKings O/U:", dk.error);
      }
      if (dkProps.length && subcatsUsed && Object.keys(subcatsUsed).length) {
        console.log("DraftKings props subcategories", subcatsUsed);
      }
    } catch (e) {
      console.warn("DraftKings O/U skipped:", e.message);
      dkError = e.message;
    }
  }

  const byKey = new Map();
  for (const r of csvProps) {
    byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
  }
  for (const r of dkProps) {
    const m = String(r.market || "").trim();
    if (
      m === "Birdies" ||
      m === "Pars" ||
      m === "Bogeys" ||
      m === "Total Score" ||
      m === "GIR" ||
      m === "Fairways hit" ||
      m === "Putts"
    ) {
      byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
    }
  }

  stripNonDkCountingProps(byKey);

  const dkPlayerMarketPresence = new Set();
  for (const r of dkProps) {
    const m = String(r.market || "").trim();
    if (!ALL_OU_COUNTING_MARKETS.includes(m)) continue;
    dkPlayerMarketPresence.add(propPlayerMarketPresenceKey(r, m));
  }

  const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;

  if (!skipModelFallback) {
    for (const mkt of MODEL_FALLBACK_MARKETS) {
      const fresh = withPropSource(modelFallbackOuForMarket(payload.players, mkt, modelRound), "model_fallback");
      for (const r of fresh) {
        if (dkPlayerMarketPresence.has(propPlayerMarketPresenceKey(r, mkt))) continue;
        byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
      }
    }
  }

  let merged = [...byKey.values()];

  if (!skipModelFallback) {
    const presence = new Set();
    for (const r of merged) {
      if (!propRowHasPostableOdds(r)) continue;
      presence.add(propPlayerMarketPresenceKey(r, String(r.market || "").trim()));
    }
    let gapFill = 0;
    for (const mkt of MODEL_FALLBACK_MARKETS) {
      for (const r of withPropSource(modelFallbackOuForMarket(payload.players, mkt, modelRound), "model_fallback")) {
        const pk = propPlayerMarketPresenceKey(r, mkt);
        if (presence.has(pk)) continue;
        merged.push(r);
        presence.add(pk);
        gapFill++;
      }
    }
    if (gapFill > 0) {
      console.warn(`[dk-round-props] filled ${gapFill} missing player×market lines with model_fallback`);
    }
  }

  const coverage = validateRoundProjectionPropsCoverage(payload.players, merged, modelRound);
  if (!coverage.ok) {
    const sample = coverage.missing
      .slice(0, 8)
      .map((m) => `${m.player_name || m.dg_id} ${m.market}`)
      .join("; ");
    const msg = `[dk-round-props] ${coverage.missing.length}/${coverage.activeCount * ALL_OU_COUNTING_MARKETS.length} player×market pairs still lack lines after merge${sample ? ` (e.g. ${sample})` : ""}`;
    if (skipModelFallback) console.error(msg);
    else console.warn(msg);
  } else {
    const nDkPm = dkPlayerMarketPresence.size;
    const nActive = coverage.activeCount;
    console.log(
      `[dk-round-props] coverage OK — ${nActive} players × ${ALL_OU_COUNTING_MARKETS.length} markets; DK posted for ${nDkPm} player×market pairs`,
    );
  }

  merged = merged.filter((r) => {
    const m = String(r.market || "").trim();
    if (!DK_POSTED_ONLY_MARKETS.has(m)) return true;
    return String(r.source || "").trim().toLowerCase() === "draftkings";
  });

  return {
    props: merged,
    nCsv: csvProps.length,
    nDk: dkProps.filter((r) => String(r?.source || "").trim().toLowerCase() === "draftkings").length,
    nDkFresh,
    dkError,
    subcatsUsed,
    coverage,
    dkLeagueUrl,
    dkLeagueSlug,
  };
}
