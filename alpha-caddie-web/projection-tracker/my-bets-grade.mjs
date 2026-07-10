/**
 * Auto-grade My bets slip from refreshed projection vs actual CSVs.
 */

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

export function normEventName(s) {
  return String(s || "")
    .trim()
    .toLowerCase()
    .replace(/\s+/g, " ");
}

export function normPlayerName(s) {
  return String(s || "")
    .trim()
    .toLowerCase();
}

function parseLine(v) {
  const n = num(v);
  return Number.isFinite(n) ? n : NaN;
}

function normResult(res) {
  const r = String(res || "").trim().toUpperCase();
  if (r === "W" || r === "L" || r === "P") return r;
  return "";
}

function ouSideResults(actual, line) {
  if (!Number.isFinite(actual) || !Number.isFinite(line)) return { over: "", under: "" };
  if (actual > line) return { over: "W", under: "L" };
  if (actual < line) return { over: "L", under: "W" };
  return { over: "P", under: "P" };
}

/**
 * @param {object[]} detailRows
 * @param {object[]} marketSpecs
 */
export function buildOuGradeIndex(detailRows, marketSpecs) {
  /** @type {Map<string, object>} */
  const idx = new Map();
  for (const row of detailRows) {
    if (row.pricing_mode !== "default" || row.pricing_skill !== "default") continue;
    const event = normEventName(row.event_name);
    const round = Math.round(num(row.round, NaN));
    const player = normPlayerName(row.player_name);
    const dg = Math.round(num(row.dg_id, NaN));
    if (!event || !Number.isFinite(round) || !player) continue;

    const key = `${event}|${round}|${player}`;
    const byDg = Number.isFinite(dg) ? `${event}|${round}|dg:${dg}` : "";
    let acc = idx.get(key);
    if (!acc) {
      acc = { dg_id: dg, actual_round_score: parseLine(row.actual_round_score) };
      idx.set(key, acc);
    }
    if (byDg && !idx.has(byDg)) idx.set(byDg, acc);

    for (const spec of marketSpecs) {
      acc[spec.market] = {
        actual: parseLine(row[spec.actual]),
        overRes: normResult(row[spec.overRes]),
        underRes: normResult(row[spec.underRes]),
        bookLine: parseLine(row[spec.bookCol]),
      };
    }
  }
  return idx;
}

function lookupOuEntry(bet, ouIndex) {
  const event = normEventName(bet.eventName);
  const round = Math.round(num(bet.round, NaN));
  const dg = Math.round(num(bet.dg_id, NaN));
  if (Number.isFinite(dg)) {
    const byDg = ouIndex.get(`${event}|${round}|dg:${dg}`);
    if (byDg) return byDg;
  }
  return ouIndex.get(`${event}|${round}|${normPlayerName(bet.playerName)}`);
}

/** @param {object} bet @param {Map<string, object>} ouIndex */
export function gradeOuBet(bet, ouIndex) {
  const entry = lookupOuEntry(bet, ouIndex);
  if (!entry) return null;
  const mkt = entry[bet.market];
  if (!mkt) return null;

  const side = String(bet.side || "").toLowerCase();
  const line = parseLine(bet.line);

  if (Number.isFinite(mkt.actual) && Number.isFinite(line)) {
    const sides = ouSideResults(mkt.actual, line);
    const res = side === "under" ? sides.under : sides.over;
    if (res) return res;
  }

  const stored = side === "under" ? mkt.underRes : mkt.overRes;
  if (stored) return stored;
  return null;
}

/**
 * @param {object[]} bets
 * @param {{ detailRows: object[], marketSpecs: object[] }} ctx
 * @returns {number} count of bets newly graded
 */
export function autoGradeMyBets(bets, ctx) {
  const ouIndex = buildOuGradeIndex(ctx.detailRows || [], ctx.marketSpecs || []);
  let changed = 0;

  for (const bet of bets) {
    const cur = String(bet.result || "open").toLowerCase();
    if (cur !== "open") continue;

    const graded = gradeOuBet(bet, ouIndex);
    if (graded) {
      bet.result = graded;
      bet.autoGradedAt = new Date().toISOString();
      changed++;
    }
  }
  return changed;
}
