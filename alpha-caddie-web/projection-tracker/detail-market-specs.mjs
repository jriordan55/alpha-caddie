/** Browser-safe detail CSV market columns (subset of scripts/round-projection-mu.mjs). */

export const DETAIL_EXPORT_MARKETS = [
  {
    key: "fairways",
    market: "Fairways hit",
    propsMarket: "Fairways hit",
    lineCol: "fairways_line",
    bookLineCol: "fairways_book_line",
    ppLineCol: "fairways_pp_line",
    overOddsCol: "fairways_over_odds",
    underOddsCol: "fairways_under_odds",
    ppOverOddsCol: "fairways_pp_over_odds",
    ppUnderOddsCol: "fairways_pp_under_odds",
  },
  {
    key: "total",
    market: "Total score",
    propsMarket: "Total Score",
    lineCol: "round_score_line",
    bookLineCol: "round_score_book_line",
    ppLineCol: "round_score_pp_line",
    overOddsCol: "round_score_over_odds",
    underOddsCol: "round_score_under_odds",
    ppOverOddsCol: "round_score_pp_over_odds",
    ppUnderOddsCol: "round_score_pp_under_odds",
  },
  {
    key: "birdies",
    market: "Birdies",
    propsMarket: "Birdies",
    lineCol: "birdies_line",
    bookLineCol: "birdies_book_line",
    ppLineCol: "birdies_pp_line",
    overOddsCol: "birdies_over_odds",
    underOddsCol: "birdies_under_odds",
    ppOverOddsCol: "birdies_pp_over_odds",
    ppUnderOddsCol: "birdies_pp_under_odds",
  },
  {
    key: "bogeys",
    market: "Bogeys",
    propsMarket: "Bogeys",
    lineCol: "bogeys_line",
    bookLineCol: "bogeys_book_line",
    ppLineCol: "bogeys_pp_line",
    overOddsCol: "bogeys_over_odds",
    underOddsCol: "bogeys_under_odds",
    ppOverOddsCol: "bogeys_pp_over_odds",
    ppUnderOddsCol: "bogeys_pp_under_odds",
  },
  {
    key: "gir",
    market: "GIR",
    propsMarket: "GIR",
    lineCol: "gir_line",
    bookLineCol: "gir_book_line",
    ppLineCol: "gir_pp_line",
    overOddsCol: "gir_over_odds",
    underOddsCol: "gir_under_odds",
    ppOverOddsCol: "gir_pp_over_odds",
    ppUnderOddsCol: "gir_pp_under_odds",
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

/** Parse a PrizePicks posted line (whole numbers allowed). */
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
  const pp = parsePpBookLine(row[spec.ppLineCol]);
  const model = parseLine(row[spec.lineCol]);
  if (Number.isFinite(dk)) return dk;
  if (Number.isFinite(pp)) return pp;
  return model;
}

export function ouSideResultsForDetailRow(row, spec, actual) {
  return ouSideResults(actual, gradeLineForDetailRow(row, spec));
}
