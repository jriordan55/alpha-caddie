/**
 * Merge DraftKings (and optional CSV / model fallback) round O/U rows into projections.props
 * for the Round projections tab.
 */
import { parse } from "csv-parse/sync";
import { existsSync, readFileSync } from "fs";
import { join, resolve } from "path";
import { fetchDraftKingsOuProps } from "./draftkings-ou-props.mjs";

export function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

/** When `DK_LEAGUE_URL` is unset, point Playwright at the same DraftKings event as `projections.json`. */
export function inferDraftKingsLeagueUrlFromProjections(payload) {
  const envUrl = String(process.env.DK_LEAGUE_URL || "").trim();
  if (envUrl) return envUrl;
  const slug = String(
    payload?.dk_league_slug || payload?.draftkings_league_slug || payload?.dk_event_slug || "",
  ).trim();
  if (slug) {
    if (slug.toLowerCase() === "pga-championship") {
      return "https://sportsbook.draftkings.com/leagues/golf/uspga-championship?category=round";
    }
    return `https://sportsbook.draftkings.com/leagues/golf/${slug}?category=round`;
  }
  const name = String(payload?.event_name || "").trim();
  if (!name) return "";
  const s = name
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
  if (!s) return "";
  if (s === "pga-championship") {
    return "https://sportsbook.draftkings.com/leagues/golf/uspga-championship?category=round";
  }
  return `https://sportsbook.draftkings.com/leagues/golf/${s}?category=round`;
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

function matchProjectionPlayerByDkLabel(players, dkLabel) {
  if (!Array.isArray(players) || !players.length) return null;
  const raw = String(dkLabel || "").trim();
  if (!raw) return null;
  const dkl = raw.toLowerCase();
  const dkn = normNameLoose(raw);
  for (const p of players) {
    const pn = String(p.player_name || "").trim();
    if (!pn) continue;
    if (pn.toLowerCase() === dkl) return p;
    if (displayGolferName(pn).toLowerCase() === dkl) return p;
    if (normNameLoose(displayGolferName(pn)) === dkn) return p;
    if (normNameLoose(pn) === dkn) return p;
  }
  return null;
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
    const row = matchProjectionPlayerByDkLabel(players, r.player_name);
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

const OU_COUNTING_MARKETS_FW = ["GIR", "Fairways hit", "Putts"];
const OU_COUNTING_MARKETS_ALL = ALL_OU_COUNTING_MARKETS;

function propPlayerMarketPresenceKey(r, market) {
  const id = Math.round(num(r.dg_id, NaN));
  if (Number.isFinite(id) && id > 0) return `id:${id}|${market}`;
  return `nm:${String(r.player_name || "").trim().toLowerCase()}|${market}`;
}

function stripNonDkCountingProps(byKey) {
  for (const key of [...byKey.keys()]) {
    const r = byKey.get(key);
    const m = String(r.market || "").trim();
    if (!OU_COUNTING_MARKETS_FW.includes(m)) continue;
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

  if (!skipDk) {
    try {
      const dkLeagueUrl = inferDraftKingsLeagueUrlFromProjections(payload);
      console.log(
        "[dk-round-props] DK scrape:",
        dkLeagueUrl ? dkLeagueUrl : "default URL (set DK_LEAGUE_URL or dk_league_slug on projections.json)",
      );
      const dk = await fetchDraftKingsOuProps({
        players: payload.players,
        ...(dkLeagueUrl ? { leagueUrl: dkLeagueUrl } : {}),
      });
      dkProps = withPropSource(dk.props || [], "draftkings");
      canonicalizeDkOuPropsAgainstProjections(dkProps, payload.players);
      dkError = dk.error;
      subcatsUsed = dk.subcatsUsed;
      if (!dkProps.length) {
        console.warn(
          "DraftKings O/U:",
          dk.error && !String(dk.error).startsWith("skipped")
            ? dk.error
            : "0 props — check Playwright / DK_SITE_SEGMENT (npx playwright install chromium)",
        );
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

  const dkCountingPresence = new Set();
  for (const r of dkProps) {
    const m = String(r.market || "").trim();
    if (!OU_COUNTING_MARKETS_FW.includes(m)) continue;
    dkCountingPresence.add(propPlayerMarketPresenceKey(r, m));
  }

  if (!skipModelFallback) {
    const modelRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) || 1;
    for (const mkt of OU_COUNTING_MARKETS_ALL) {
      const fresh = withPropSource(modelFallbackOuForMarket(payload.players, mkt, modelRound), "model_fallback");
      for (const r of fresh) {
        if (dkCountingPresence.has(propPlayerMarketPresenceKey(r, mkt))) continue;
        byKey.set(`${r.player_name}|${r.market}|${r.line}`, r);
      }
    }
  }

  const merged = [...byKey.values()];
  return { props: merged, nCsv: csvProps.length, nDk: dkProps.length, dkError, subcatsUsed };
}
