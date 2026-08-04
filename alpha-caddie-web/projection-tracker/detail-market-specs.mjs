/** Browser-safe detail CSV market columns (subset of scripts/round-projection-mu.mjs). */

function altBookCols(stem) {
  const books = ["pp", "sl", "ud", "fd", "czr", "kl"];
  /** @type {Record<string, string>} */
  const cols = {};
  for (const b of books) {
    cols[`${b}LineCol`] = `${stem}_${b}_line`;
    cols[`${b}OverOddsCol`] = `${stem}_${b}_over_odds`;
    cols[`${b}UnderOddsCol`] = `${stem}_${b}_under_odds`;
    cols[`${b}OpenLineCol`] = `${stem}_${b}_open_line`;
    cols[`${b}OpenOverOddsCol`] = `${stem}_${b}_open_over_odds`;
    cols[`${b}OpenUnderOddsCol`] = `${stem}_${b}_open_under_odds`;
  }
  cols.bookOpenLineCol = `${stem}_book_open_line`;
  cols.openOverOddsCol = `${stem}_open_over_odds`;
  cols.openUnderOddsCol = `${stem}_open_under_odds`;
  return cols;
}

export const DETAIL_EXPORT_MARKETS = [
  {
    key: "fairways",
    market: "Fairways hit",
    propsMarket: "Fairways hit",
    lineCol: "fairways_line",
    bookLineCol: "fairways_book_line",
    ...altBookCols("fairways"),
    overOddsCol: "fairways_over_odds",
    underOddsCol: "fairways_under_odds",
  },
  {
    key: "total",
    market: "Total score",
    propsMarket: "Total Score",
    lineCol: "round_score_line",
    bookLineCol: "round_score_book_line",
    ...altBookCols("round_score"),
    overOddsCol: "round_score_over_odds",
    underOddsCol: "round_score_under_odds",
  },
  {
    key: "birdies",
    market: "Birdies",
    propsMarket: "Birdies",
    lineCol: "birdies_line",
    bookLineCol: "birdies_book_line",
    ...altBookCols("birdies"),
    overOddsCol: "birdies_over_odds",
    underOddsCol: "birdies_under_odds",
  },
  {
    key: "bogeys",
    market: "Bogeys",
    propsMarket: "Bogeys",
    lineCol: "bogeys_line",
    bookLineCol: "bogeys_book_line",
    ...altBookCols("bogeys"),
    overOddsCol: "bogeys_over_odds",
    underOddsCol: "bogeys_under_odds",
  },
  {
    key: "gir",
    market: "GIR",
    propsMarket: "GIR",
    lineCol: "gir_line",
    bookLineCol: "gir_book_line",
    ...altBookCols("gir"),
    overOddsCol: "gir_over_odds",
    underOddsCol: "gir_under_odds",
  },
];

/** Books exploded in the projection tracker bet log / EV tabs. */
export const TRACKER_OU_BOOKS = [
  {
    id: "draftkings",
    label: "DraftKings",
    liveLabel: "DraftKings (live)",
    sourceCol: "book_odds_source",
    acceptSources: ["pre_round_audit", "live_snapshot"],
    lineKey: "bookLineCol",
    overKey: "overOddsCol",
    underKey: "underOddsCol",
    openLineKey: "bookOpenLineCol",
    openOverKey: "openOverOddsCol",
    openUnderKey: "openUnderOddsCol",
    openAtCol: "book_odds_open_at",
    closeAtCol: "book_odds_close_at",
    wholeLine: false,
  },
  {
    id: "prizepicks",
    label: "PrizePicks",
    liveLabel: "PrizePicks (live)",
    sourceCol: "pp_book_odds_source",
    acceptSources: ["pre_round_audit", "prizepicks_live"],
    lineKey: "ppLineCol",
    overKey: "ppOverOddsCol",
    underKey: "ppUnderOddsCol",
    openLineKey: "ppOpenLineCol",
    openOverKey: "ppOpenOverOddsCol",
    openUnderKey: "ppOpenUnderOddsCol",
    openAtCol: "pp_book_odds_open_at",
    closeAtCol: "pp_book_odds_close_at",
    wholeLine: true,
  },
  {
    id: "sleeper",
    label: "Sleeper",
    liveLabel: "Sleeper (live)",
    sourceCol: "sl_book_odds_source",
    acceptSources: ["pre_round_audit", "sleeper_live"],
    lineKey: "slLineCol",
    overKey: "slOverOddsCol",
    underKey: "slUnderOddsCol",
    openLineKey: "slOpenLineCol",
    openOverKey: "slOpenOverOddsCol",
    openUnderKey: "slOpenUnderOddsCol",
    openAtCol: "sl_book_odds_open_at",
    closeAtCol: "sl_book_odds_close_at",
    wholeLine: true,
  },
  {
    id: "underdog",
    label: "Underdog",
    liveLabel: "Underdog (live)",
    sourceCol: "ud_book_odds_source",
    acceptSources: ["pre_round_audit", "underdog_live"],
    lineKey: "udLineCol",
    overKey: "udOverOddsCol",
    underKey: "udUnderOddsCol",
    openLineKey: "udOpenLineCol",
    openOverKey: "udOpenOverOddsCol",
    openUnderKey: "udOpenUnderOddsCol",
    openAtCol: "ud_book_odds_open_at",
    closeAtCol: "ud_book_odds_close_at",
    wholeLine: true,
  },
  {
    id: "fanduel",
    label: "FanDuel",
    liveLabel: "FanDuel (live)",
    sourceCol: "fd_book_odds_source",
    acceptSources: ["pre_round_audit", "fanduel_live"],
    lineKey: "fdLineCol",
    overKey: "fdOverOddsCol",
    underKey: "fdUnderOddsCol",
    openLineKey: "fdOpenLineCol",
    openOverKey: "fdOpenOverOddsCol",
    openUnderKey: "fdOpenUnderOddsCol",
    openAtCol: "fd_book_odds_open_at",
    closeAtCol: "fd_book_odds_close_at",
    wholeLine: false,
  },
  {
    id: "caesars",
    label: "Caesars",
    liveLabel: "Caesars (live)",
    sourceCol: "czr_book_odds_source",
    acceptSources: ["pre_round_audit", "caesars_live"],
    lineKey: "czrLineCol",
    overKey: "czrOverOddsCol",
    underKey: "czrUnderOddsCol",
    openLineKey: "czrOpenLineCol",
    openOverKey: "czrOpenOverOddsCol",
    openUnderKey: "czrOpenUnderOddsCol",
    openAtCol: "czr_book_odds_open_at",
    closeAtCol: "czr_book_odds_close_at",
    wholeLine: false,
  },
  {
    id: "kalshi",
    label: "Kalshi",
    liveLabel: "Kalshi (live)",
    sourceCol: "kl_book_odds_source",
    acceptSources: ["pre_round_audit", "kalshi_live"],
    lineKey: "klLineCol",
    overKey: "klOverOddsCol",
    underKey: "klUnderOddsCol",
    openLineKey: "klOpenLineCol",
    openOverKey: "klOpenOverOddsCol",
    openUnderKey: "klOpenUnderOddsCol",
    openAtCol: "kl_book_odds_open_at",
    closeAtCol: "kl_book_odds_close_at",
    wholeLine: false,
  },
];

export function enforceHalfLine(v) {
  const n = Number(v);
  if (!Number.isFinite(n)) return NaN;
  return Math.round(n * 2) / 2;
}

/** Parse a DraftKings posted line (always half-point buckets). */
export function parseDkBookLine(v) {
  return enforceHalfLine(Number(v));
}

/** Parse a PrizePicks / pick'em posted line (whole numbers allowed). */
export function parsePpBookLine(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

/** Format a DraftKings book line for CSV/display. */
export function fmtDkBookLine(market, v) {
  const n = parseDkBookLine(v);
  if (!Number.isFinite(n)) return "";
  if (market === "Total score") return (Math.round(n * 10) / 10).toFixed(1);
  return String(n);
}

/** Format a PrizePicks book line for CSV/display (preserve whole numbers). */
export function fmtPpBookLine(market, v) {
  const n = parsePpBookLine(v);
  if (!Number.isFinite(n)) return "";
  if (market === "Total score") {
    if (n === Math.round(n)) return `${Math.round(n)}.0`;
    return (Math.round(n * 10) / 10).toFixed(1);
  }
  if (n === Math.round(n)) return String(Math.round(n));
  return (Math.round(n * 10) / 10).toFixed(1);
}

function parseLine(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

/** W/L for over and under vs the posted book line (pushes when actual equals line). */
export function ouSideResults(actual, line) {
  if (!Number.isFinite(actual) || !Number.isFinite(line)) return { over: "", under: "" };
  if (actual > line) return { over: "W", under: "L" };
  if (actual < line) return { over: "L", under: "W" };
  return { over: "P", under: "P" };
}

/** Grade line priority matches export CSV: DK book, then PP, then model μ. */
export function gradeLineForDetailRow(row, spec) {
  if (!row || !spec) return NaN;
  const dk = parseDkBookLine(row[spec.bookLineCol]);
  if (Number.isFinite(dk)) return dk;
  for (const book of TRACKER_OU_BOOKS) {
    if (book.id === "draftkings") continue;
    const col = spec[book.lineKey];
    if (!col) continue;
    const line = book.wholeLine ? parsePpBookLine(row[col]) : parseDkBookLine(row[col]);
    if (Number.isFinite(line)) return line;
  }
  return parseLine(row[spec.lineCol]);
}
