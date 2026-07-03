/**
 * Auto-grade My bets slip from refreshed projection vs actual + matchup outcome CSVs.
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

function matchupPairKey(dgA, dgB) {
  const a = Math.round(num(dgA, NaN));
  const b = Math.round(num(dgB, NaN));
  if (!Number.isFinite(a) || !Number.isFinite(b)) return "";
  return a < b ? `${a}|${b}` : `${b}|${a}`;
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

/** @param {object[]} matchupRows */
export function buildMatchupGradeIndex(matchupRows) {
  /** @type {Map<string, { p1Res: string, p2Res: string, p1Dg: number, p2Dg: number }>} */
  const idx = new Map();
  for (const row of matchupRows) {
    const book = String(row.book || "").trim().toLowerCase();
    if (book && book !== "draftkings") continue;
    const event = normEventName(row.event_name);
    const round = Math.round(num(row.round, NaN));
    const dg1 = Math.round(num(row.dg_id, NaN));
    const dg2 = Math.round(num(row.opponent_dg_id, NaN));
    if (!event || !Number.isFinite(round) || !Number.isFinite(dg1) || !Number.isFinite(dg2)) continue;
    const pk = matchupPairKey(dg1, dg2);
    if (!pk) continue;
    const key = `${event}|${round}|${pk}`;
    idx.set(key, {
      p1Dg: dg1,
      p2Dg: dg2,
      p1Res: normResult(row.p1_result),
      p2Res: normResult(row.p2_result),
    });
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

function parseMatchupIds(bet) {
  const dg = Math.round(num(bet.dg_id, NaN));
  const opp = Math.round(num(bet.opponent_dg_id, NaN));
  if (Number.isFinite(dg) && Number.isFinite(opp)) return { dg, opp };
  const parts = String(bet.lineKey || "").split("|");
  if (parts.length >= 4 && parts[1] === "Round matchups") {
    return { dg: Math.round(num(parts[0], NaN)), opp: Math.round(num(parts[3], NaN)) };
  }
  return { dg: NaN, opp: NaN };
}

/** @param {object} bet @param {Map<string, object>} matchupIndex @param {Map<string, object>} ouIndex */
export function gradeMatchupBet(bet, matchupIndex, ouIndex) {
  const event = normEventName(bet.eventName);
  const round = Math.round(num(bet.round, NaN));
  const { dg, opp } = parseMatchupIds(bet);
  if (!event || !Number.isFinite(round) || !Number.isFinite(dg) || !Number.isFinite(opp)) return null;

  const pk = matchupPairKey(dg, opp);
  const row = matchupIndex.get(`${event}|${round}|${pk}`);
  if (row) {
    if (dg === row.p1Dg) return row.p1Res || null;
    if (dg === row.p2Dg) return row.p2Res || null;
  }

  const e1 = lookupOuEntry({ ...bet, dg_id: dg, playerName: bet.playerName }, ouIndex);
  const e2 = lookupOuEntry({ ...bet, dg_id: opp, playerName: bet.opponentName }, ouIndex);
  const s1 = parseLine(e1?.actual_round_score);
  const s2 = parseLine(e2?.actual_round_score);
  if (!Number.isFinite(s1) || !Number.isFinite(s2) || s1 <= 0 || s2 <= 0) return null;
  if (Math.abs(s1 - s2) < 0.001) return "P";
  const pickedWins = s1 < s2 ? dg : opp;
  return pickedWins === dg ? "W" : "L";
}

/**
 * @param {object[]} bets
 * @param {{ detailRows: object[], matchupRows: object[], marketSpecs: object[] }} ctx
 * @returns {number} count of bets newly graded
 */
export function autoGradeMyBets(bets, ctx) {
  const ouIndex = buildOuGradeIndex(ctx.detailRows || [], ctx.marketSpecs || []);
  const matchupIndex = buildMatchupGradeIndex(ctx.matchupRows || []);
  let changed = 0;

  for (const bet of bets) {
    const cur = String(bet.result || "open").toLowerCase();
    if (cur !== "open") continue;

    let graded = null;
    if (String(bet.market) === "Round matchups") {
      graded = gradeMatchupBet(bet, matchupIndex, ouIndex);
    } else {
      graded = gradeOuBet(bet, ouIndex);
    }
    if (graded) {
      bet.result = graded;
      bet.autoGradedAt = new Date().toISOString();
      changed++;
    }
  }
  return changed;
}
