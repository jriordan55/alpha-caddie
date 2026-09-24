/**
 * Organize DraftKings props and pick canonical market IDs for model matching.
 */

function isUsaSel(s) {
  return /^(usa|united states)$/i.test(String(s || "").trim());
}

function isIntSel(s) {
  return /international|rest of the world/i.test(String(s || "").trim());
}

function isTieSel(s) {
  return /^tie$/i.test(String(s || "").trim());
}

function isPlayerSel(s) {
  const t = String(s || "").trim();
  return t && !isUsaSel(t) && !isIntSel(t) && !isTieSel(t);
}

function groupByMarketId(props) {
  const groups = new Map();
  for (const p of props) {
    const id = String(p.marketId || p.market || "unknown");
    if (!groups.has(id)) groups.set(id, []);
    groups.get(id).push(p);
  }
  return groups;
}

function isTeamWinGroup(rows) {
  const labels = rows.map((r) => String(r.selection || "").trim());
  return labels.some(isUsaSel) && labels.some(isIntSel);
}

function scoreCupGroup(rows) {
  const name = String(rows[0]?.market || "").toLowerCase();
  if (
    /foursomes day|fourballs day|correct score|most holes|lift the trophy|team usa|team int|rookie|qualifier|captain|top 3/i.test(
      name,
    )
  ) {
    return -1;
  }
  if (!isTeamWinGroup(rows)) return -1;
  let score = 10;
  if (rows.some((r) => isTieSel(r.selection))) score += 100;
  if (/outright winner|cup winner/i.test(name)) score += 50;
  score += rows.length;
  return score;
}

function scoreSessionGroup(rows) {
  const name = String(rows[0]?.market || "").toLowerCase();
  if (!/foursomes day|fourballs day|singles day|sunday singles|tournament foursomes winner/.test(name)) return -1;
  if (/correct score|most holes/.test(name)) return -1;
  if (!isTeamWinGroup(rows)) return -1;
  return 10 + rows.length;
}

function pickCupWinnerGroup(groups) {
  let best = null;
  let bestScore = -1;
  for (const [id, rows] of groups) {
    const score = scoreCupGroup(rows);
    if (score > bestScore) {
      bestScore = score;
      best = { id, rows, name: rows[0]?.market || null };
    }
  }
  return best;
}

function isTeamSpecificScorerMarket(name) {
  return /team usa top|team int top|international team rookie|top international|qualifier|captain's pick|captains pick|top 3 finish/i.test(
    String(name || "").toLowerCase(),
  );
}

function pickTopScorerGroup(groups, excludeIds = new Set(), namePattern = null) {
  let best = null;
  let bestCount = 0;
  for (const [id, rows] of groups) {
    if (excludeIds.has(id)) continue;
    const marketName = String(rows[0]?.market || "").toLowerCase();
    if (namePattern && !namePattern.test(marketName)) continue;
    if (!namePattern && isTeamSpecificScorerMarket(marketName)) continue;
    const players = rows.filter((r) => isPlayerSel(r.selection));
    if (players.length > bestCount) {
      bestCount = players.length;
      best = { id, rows: players, name: rows[0]?.market || null };
    }
  }
  return best;
}

function pickUsaTopScorerGroup(groups, excludeIds = new Set()) {
  for (const [id, rows] of groups) {
    if (excludeIds.has(id)) continue;
    if (!/team usa top points scorer/i.test(String(rows[0]?.market || "").toLowerCase())) continue;
    const players = rows.filter((r) => isPlayerSel(r.selection));
    if (players.length) return { id, rows: players, name: rows[0]?.market || null };
  }
  return null;
}

export function sessionIdFromDkMarket(name) {
  const n = String(name || "").toLowerCase();
  if (/session 1 winner/.test(n)) return "fri_foursomes";
  if (/foursomes day 2 winner/.test(n)) return "fri_foursomes";
  if (/foursomes day 3 winner/.test(n)) return "sat_foursomes";
  if (/tournament foursomes winner/.test(n)) return "tournament_foursomes";
  if (/fourballs day 1 winner/.test(n)) return "fri_fourball";
  if (/fourballs day 2 winner|fourballs day 3 winner/.test(n)) return "sat_fourball";
  if (/singles day 3 winner|sunday singles winner/.test(n)) return "sun_singles";
  return null;
}

function pickSessionWinnerGroups(groups, excludeIds = new Set()) {
  const out = new Map();
  for (const [id, rows] of groups) {
    if (excludeIds.has(id)) continue;
    if (scoreSessionGroup(rows) < 0) continue;
    const sessionId = sessionIdFromDkMarket(rows[0]?.market);
    if (!sessionId) continue;
    out.set(sessionId, { id, rows, name: rows[0]?.market || null });
  }
  return out;
}

function parsePctFromRaw(raw) {
  const pct = raw?.displayOdds?.percentage ?? raw?.displayOdds?.percent;
  if (pct != null) {
    const m = String(pct).match(/([\d.]+)/);
    if (m) return Number(m[1]) / 100;
  }
  if (raw?.trueOdds != null && Number(raw.trueOdds) > 1) {
    return 1 / Number(raw.trueOdds);
  }
  return null;
}

export function enrichDkProp(p) {
  const dk_implied = parsePctFromRaw(p.raw);
  return {
    ...p,
    american_display: p.raw?.displayOdds?.american ?? p.american_display ?? null,
    true_odds: p.raw?.trueOdds ?? null,
    dk_implied_prob: dk_implied,
  };
}

const SUBCATEGORY_PRIORITY = {
  matchups: 1,
  hole: 2,
  outrights: 3,
  "correct-score": 4,
  "event-props": 5,
  "round-props": 6,
  "player-props": 7,
  "top-scorer": 8,
  "usa-props": 9,
  "int.-props": 10,
  "team-props": 11,
  popular: 90,
  winner: 91,
};

function dedupeDkProps(props) {
  const sorted = [...props].sort(
    (a, b) =>
      (SUBCATEGORY_PRIORITY[a.dk_subcategory] ?? 50) - (SUBCATEGORY_PRIORITY[b.dk_subcategory] ?? 50),
  );
  const seen = new Set();
  const out = [];
  for (const p of sorted) {
    const k = `${p.marketId || p.market}|${p.selection}|${p.american}`;
    if (seen.has(k)) continue;
    seen.add(k);
    out.push(p);
  }
  return out;
}

/** Keep all DK lines; build metadata for model matching. */
export function organizeDkProps(props) {
  const enriched = dedupeDkProps(props.map(enrichDkProp));
  const groups = groupByMarketId(enriched);

  const cup = pickCupWinnerGroup(groups);
  const exclude = new Set(cup ? [cup.id] : []);
  const scorerMostHoles = pickTopScorerGroup(groups, exclude, /most holes|player to win the most holes/i);
  if (scorerMostHoles) exclude.add(scorerMostHoles.id);
  const scorerTopPoints = pickTopScorerGroup(groups, exclude, /top points scorer/i);
  if (scorerTopPoints) exclude.add(scorerTopPoints.id);
  const usaScorer = pickUsaTopScorerGroup(groups, exclude);
  if (usaScorer) exclude.add(usaScorer.id);
  const sessions = pickSessionWinnerGroups(groups, exclude);

  const meta = {
    cup_market_id: cup?.id ?? null,
    cup_market_name: cup?.name ?? null,
    scorer_market_id: scorerMostHoles?.id ?? null,
    scorer_market_name: scorerMostHoles?.name ?? null,
    alt_scorer_market_id: scorerTopPoints?.id ?? null,
    alt_scorer_market_name: scorerTopPoints?.name ?? null,
    usa_scorer_market_id: usaScorer?.id ?? null,
    usa_scorer_market_name: usaScorer?.name ?? null,
    session_market_ids: Object.fromEntries([...sessions.entries()].map(([sid, g]) => [sid, g.id])),
    session_market_names: Object.fromEntries([...sessions.entries()].map(([sid, g]) => [sid, g.name])),
    market_ids: [...groups.keys()],
    market_count: groups.size,
  };

  return { props: enriched, meta };
}

/** @deprecated use organizeDkProps */
export function canonicalizeDkProps(props) {
  return organizeDkProps(props);
}

export function cupMarketId(meta) {
  return meta?.cup_market_id ?? null;
}

export function scorerMarketId(meta) {
  return meta?.scorer_market_id ?? null;
}
