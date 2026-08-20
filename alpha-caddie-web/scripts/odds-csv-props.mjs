/**
 * Parse Hard Rock (odds.csv) O/U props: Birdies + Round Score.
 * Shared by hr-pre-round-props-from-odds-csv.mjs and backtest-odds-model-roi.mjs.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import {
  displayGolferName,
  golferNameMatchParts,
  golferNamesLikelySame,
  normNameLoose,
} from "./golfer-name-match.mjs";
import { birdiesPlusEaglesFromRow } from "./round-projection-mu.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = resolve(WEB_ROOT, "..");

export const ODDS_CSV_OU_MARKETS = new Set([
  "GOLF:FT:CTBIR",
  "GOLF:FT:ROUNDNUMBIRDIES",
  "GOLF:FT:CTSTR",
  "GOLF:P:ROUND1OUSCORE",
]);

export const ODDS_CSV_MARKET_LABEL = {
  "GOLF:FT:CTBIR": "Birdies",
  "GOLF:FT:ROUNDNUMBIRDIES": "Birdies",
  "GOLF:FT:CTSTR": "Total score",
  "GOLF:P:ROUND1OUSCORE": "Total score",
};

/** propsMarket labels used in round_projection_vs_actual / EXPORT_MARKETS */
export const ODDS_CSV_PROPS_MARKET = {
  Birdies: "Birdies",
  "Total score": "Total Score",
};

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function defaultOddsCsvPath() {
  return join(REPO_ROOT, "data", "odds.csv");
}

export function oddsPlayerMatchesHist(oddsLabel, histName) {
  if (golferNamesLikelySame(oddsLabel, histName)) return true;
  const s = String(oddsLabel || "").trim();
  const m = s.match(/^([A-Za-z])\.?\s+(.+)$/);
  if (!m) return false;
  const init = m[1].toLowerCase();
  const last = normNameLoose(m[2]);
  const ht = golferNameMatchParts(histName);
  if (!last || !ht.last || last !== ht.last) return false;
  if (ht.parts.length >= 2) return ht.parts[0].startsWith(init);
  return false;
}

export function parseOddsCsvCompetition(comp, startUtc) {
  const raw = String(comp || "").trim();
  const ym = raw.match(/\s+(20\d{2})\s*$/);
  if (ym) {
    return { event: raw.replace(/\s+20\d{2}\s*$/, "").trim(), year: Number(ym[1]), competition: raw };
  }
  const y = new Date(String(startUtc || "")).getUTCFullYear();
  return { event: raw, year: Number.isFinite(y) ? y : NaN, competition: raw };
}

export function parseOddsCsvRoundFromText(...parts) {
  for (const p of parts) {
    const m = String(p || "").match(/Round\s*(\d)/i);
    if (m) return Number(m[1]);
  }
  return NaN;
}

export function parseOddsCsvOuPlayer(marketName) {
  const n = String(marketName || "").trim();
  return n
    .replace(/\s*Total Birdies or Better.*$/i, "")
    .replace(/\s*Total Birdies.*$/i, "")
    .replace(/\s*Round Score.*$/i, "")
    .replace(/\s*-\s*Round\s*\d+\s*Score.*$/i, "")
    .replace(/\s*-\s*Round\s*\d+\s*Total Birdies.*$/i, "")
    .trim();
}

export function parseOddsCsvSelection(sel) {
  const s = String(sel || "").trim();
  const m = s.match(/^(Over|Under)\s+([\d.]+)$/i);
  if (!m) return null;
  return { side: m[1].toLowerCase(), line: num(m[2]) };
}

function statFromHistRow(row, stat) {
  if (stat === "birdies") return birdiesPlusEaglesFromRow(row);
  if (stat === "total") return num(row.round_score, NaN);
  return NaN;
}

/**
 * @param {object[]} oddsRows raw odds.csv rows
 * @returns {Map<string, object>}
 */
export function parseOddsCsvProps(oddsRows) {
  /** @type {Map<string, object>} */
  const props = new Map();

  for (const row of oddsRows) {
    const marketType = String(row.MARKET_TYPE || "").trim();
    if (!ODDS_CSV_OU_MARKETS.has(marketType)) continue;

    const marketLabel = ODDS_CSV_MARKET_LABEL[marketType] || marketType;
    const sel = parseOddsCsvSelection(row.SELECTION);
    if (!sel || !Number.isFinite(sel.line)) continue;

    const { event, year, competition } = parseOddsCsvCompetition(
      row.COMPETITION,
      row.EVENT_START_TIME_UTC,
    );
    const round =
      parseOddsCsvRoundFromText(row.SPORT_EVENT, row.MARKET_NAME) ||
      (marketType === "GOLF:P:ROUND1OUSCORE" ? 1 : NaN);
    if (!Number.isFinite(year) || !Number.isFinite(round)) continue;

    const player = parseOddsCsvOuPlayer(row.MARKET_NAME);
    const betTimeMs =
      Date.parse(String(row.EVENT_START_TIME_UTC || "").replace(" ", "T")) ||
      Date.parse(`${year}-01-01T12:00:00Z`);

    const key = `${year}|${foldComparableTitle(event)}|${round}|${normNameLoose(player)}|${marketLabel}|${sel.line}`;
    let p = props.get(key);
    if (!p) {
      p = {
        competition,
        event,
        year,
        round,
        player,
        market_label: marketLabel,
        props_market: ODDS_CSV_PROPS_MARKET[marketLabel] || marketLabel,
        market_type: marketType,
        line: sel.line,
        bet_time_ms: betTimeMs,
        over_am: NaN,
        close_over_am: NaN,
        under_am: NaN,
        close_under_am: NaN,
        open_over_am: NaN,
        open_under_am: NaN,
      };
      props.set(key, p);
    }

    const openAm = num(row.OPENING_AMERICAN_ODDS, NaN);
    const closeAm = num(row.CLOSING_AMERICAN_ODDS, NaN);
    if (sel.side === "over") {
      p.over_am = closeAm;
      p.close_over_am = closeAm;
      p.open_over_am = openAm;
      if (!Number.isFinite(p.over_am)) p.over_am = openAm;
    } else {
      p.under_am = closeAm;
      p.close_under_am = closeAm;
      p.open_under_am = openAm;
      if (!Number.isFinite(p.under_am)) p.under_am = openAm;
    }
  }
  return props;
}

/** @param {Map<string, object>} props */
export function attachOddsCsvDgIds(props, histRows) {
  const buckets = new Map();
  for (const r of histRows) {
    const year = num(r.year, NaN);
    const round = num(r.round_num, NaN);
    const event = String(r.event_name || "").trim();
    if (!Number.isFinite(year) || !Number.isFinite(round) || !event) continue;
    const k = `${year}|${foldComparableTitle(event)}|${round}`;
    if (!buckets.has(k)) buckets.set(k, []);
    buckets.get(k).push(r);
  }

  for (const p of props.values()) {
    const k = `${p.year}|${foldComparableTitle(p.event)}|${p.round}`;
    let bucket = buckets.get(k) || [];
    if (!bucket.length) {
      for (const [bk, rows] of buckets.entries()) {
        const [y, ev, rnd] = bk.split("|");
        if (Number(y) !== p.year || Number(rnd) !== p.round) continue;
        if (eventsLikelySame(p.event, ev.replace(/-/g, " "))) {
          bucket = rows;
          break;
        }
      }
    }
    for (const h of bucket) {
      if (!oddsPlayerMatchesHist(p.player, h.player_name)) continue;
      p.matched_player = displayGolferName(h.player_name);
      p.dg_id = num(h.dg_id, NaN);
      p.course_name = String(h.course_name || "").trim();
      p.actual_birdies = statFromHistRow(h, "birdies");
      p.actual_total = statFromHistRow(h, "total");
      break;
    }
  }
}

let oddsCsvCache = null;
let oddsCsvPathCache = "";

/**
 * @param {string} [oddsPath]
 * @param {object[]} [histRows]
 */
export function loadOddsCsvPropsIndex(oddsPath = defaultOddsCsvPath(), histRows = null) {
  const path = String(oddsPath || defaultOddsCsvPath());
  if (oddsCsvCache && oddsCsvPathCache === path) return oddsCsvCache;
  if (!existsSync(path)) {
    oddsCsvCache = new Map();
    oddsCsvPathCache = path;
    return oddsCsvCache;
  }
  const rows = parse(readFileSync(path, "utf8"), {
    columns: true,
    relax_quotes: true,
    skip_empty_lines: true,
    trim: true,
  });
  if (!histRows?.length) {
    const histPath = join(REPO_ROOT, "data", "historical_rounds_all.csv");
    if (existsSync(histPath)) {
      histRows = parse(readFileSync(histPath, "utf8"), {
        columns: true,
        relax_quotes: true,
        skip_empty_lines: true,
        trim: true,
      });
    }
  }
  const props = parseOddsCsvProps(rows);
  if (histRows?.length) attachOddsCsvDgIds(props, histRows);
  oddsCsvCache = props;
  oddsCsvPathCache = path;
  return props;
}

export function invalidateOddsCsvPropsCache() {
  oddsCsvCache = null;
  oddsCsvPathCache = "";
}
