import { SESSIONS } from "./data.mjs";
import { namesMatch } from "./markets.mjs";
import { americanToProb, evPerUnit, probToAmerican } from "./odds.mjs";
import { buildPlayerDistMap } from "./cup-sim.mjs";
import { simulatePairMatchProb } from "./match-sim.mjs";
import { capThreeWayProb } from "./prescup-history.mjs";
import { USA_HOME_SG } from "./data.mjs";
import { applyHoleHalvePrior, medinahHoleHalveRate } from "./medinah-hole-halve.mjs";

const SESSION_BY_DK = [
  { pattern: /session 1 winner|session 1 correct score/i, id: "fri_foursomes" },
  { pattern: /foursomes day 2|day 2 foursomes/i, id: "fri_foursomes" },
  { pattern: /foursomes day 3|day 3 foursomes/i, id: "sat_foursomes" },
  { pattern: /fourballs day 1|day 1 fourballs/i, id: "fri_fourball" },
  { pattern: /fourballs day 3|day 3 fourballs/i, id: "sat_fourball" },
  { pattern: /singles day 4|day 4 singles/i, id: "sun_singles" },
  { pattern: /tournament foursomes/i, id: "tournament_foursomes" },
  { pattern: /tournament fourballs/i, id: "tournament_fourballs" },
];

function normName(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9,\s.+/-]/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function buildPlayerIndex(field) {
  const byName = new Map();
  for (const p of field.players) {
    byName.set(normName(p.name), p);
    const display = p.name.includes(",") ? p.name.split(",").reverse().join(" ").trim() : p.name;
    byName.set(normName(display), p);
    byName.set(normName(p.name.split(",")[0]), p);
  }
  return byName;
}

function resolvePlayer(name, field, playerIndex) {
  if (isTeamSelection(name)) return null;
  const n = normName(name);
  if (playerIndex.has(n)) return playerIndex.get(n);
  for (const p of field.players) {
    if (namesMatch(p.name, name)) return p;
  }
  return resolveAbbreviatedPlayer(name, field);
}

function resolveAbbreviatedPlayer(label, field) {
  const cleaned = String(label || "").replace(/\s+/g, " ").trim();
  if (!cleaned) return null;
  const parts = cleaned.split(" ").filter(Boolean);
  const last = parts[parts.length - 1]?.replace(/\./g, "").toLowerCase();
  if (!last) return null;
  const initials = parts
    .slice(0, -1)
    .map((p) => p.replace(/\./g, "").toLowerCase())
    .join("");
  const candidates = field.players.filter((p) => {
    const ln = p.name.split(",")[0]?.trim().toLowerCase();
    return ln === last;
  });
  if (candidates.length === 1) return candidates[0];
  if (!candidates.length) return null;
  if (!initials) return candidates[0];
  for (const p of candidates) {
    const fn = (p.name.split(",")[1] || "").trim().toLowerCase();
    const fnCompact = fn.replace(/\s+/g, "");
    const fnInitials = fn
      .split(/\s+/)
      .map((x) => x[0])
      .join("");
    if (fnCompact.startsWith(initials) || fnInitials.startsWith(initials) || initials.startsWith(fnInitials)) {
      return p;
    }
  }
  return candidates[0];
}

function parsePairing(selection, field, playerIndex) {
  if (/^tie$/i.test(String(selection || "").trim())) return null;
  const parts = String(selection || "")
    .split("/")
    .map((s) => s.trim())
    .filter(Boolean);
  if (parts.length !== 2) return null;
  const players = parts.map((p) => resolveAbbreviatedPlayer(p, field) || resolvePlayer(p, field, playerIndex));
  if (players.some((p) => !p)) return null;
  if (players[0].team !== players[1].team) return null;
  return { team: players[0].team, players };
}

function pairSidesFromPool(poolSelections, field, playerIndex) {
  const sides = [];
  for (const sel of poolSelections || []) {
    if (/^tie$/i.test(String(sel || "").trim())) continue;
    const pair = parsePairing(sel, field, playerIndex);
    if (pair) sides.push({ sel, ...pair });
  }
  return {
    usa: sides.find((p) => p.team === "USA") || null,
    intl: sides.find((p) => p.team === "INT") || null,
  };
}

function pairMatchRates(ctx, usaSide, intlSide, holeIdx = null) {
  if (!usaSide?.players?.length || !intlSide?.players?.length) return null;
  const key = [
    usaSide.players.map((p) => p.dg_id).join("+"),
    intlSide.players.map((p) => p.dg_id).join("+"),
    holeIdx ?? "match",
  ].join("|");
  if (ctx.matchCache.has(key)) return ctx.matchCache.get(key);
  const raw = simulatePairMatchProb(
    "foursomes",
    usaSide.players,
    intlSide.players,
    ctx.field.hole_pars,
    ctx.playerDists,
    { holeIdx, seed: 20260924 },
  );
  let rates;
  if (holeIdx != null) {
    const targetTie = medinahHoleHalveRate(holeIdx, usaSide.players, intlSide.players, {
      holePars: ctx.field.hole_pars,
      courseAdjStp: ctx.field.course_adj_stp,
      usaHomeSg: USA_HOME_SG,
    });
    rates = applyHoleHalvePrior(raw, targetTie);
  } else {
    rates = capThreeWayProb(raw.usa, raw.int, raw.tie);
  }
  ctx.matchCache.set(key, rates);
  return rates;
}

function probPairSelection(market, selection, poolSelections, ctx) {
  const { usa, intl } = pairSidesFromPool(poolSelections, ctx.field, ctx.playerIndex);
  if (!usa || !intl) return null;
  const holeM = String(market).match(/hole\s+(\d+)/i);
  const holeIdx = holeM ? Number(holeM[1]) - 1 : null;
  const rates = pairMatchRates(ctx, usa, intl, holeIdx);
  if (!rates) return null;
  if (/^tie$/i.test(String(selection || "").trim())) return rates.tie;
  const pair = parsePairing(selection, ctx.field, ctx.playerIndex);
  if (!pair) return null;
  const isHoleProp = holeIdx != null;
  if (isHoleProp) {
    if (pair.team === "USA") return rates.usa;
    if (pair.team === "INT") return rates.int;
    return null;
  }
  const winMass = rates.usa + rates.int;
  if (winMass <= 0) return null;
  if (pair.team === "USA") return rates.usa / winMass;
  if (pair.team === "INT") return rates.int / winMass;
  return null;
}

function probHalvedMatchesOu(sim, selection) {
  const parsed = parsePointsLine(selection);
  if (!parsed || !sim.halvedMatchSnapshots) return null;
  let hit = 0;
  for (let s = 0; s < sim.nSims; s++) {
    const n = sim.halvedMatchSnapshots[s] || 0;
    if (parsed.side === "over" && n >= parsed.line - 0.001) hit++;
    else if (parsed.side === "under" && n < parsed.line - 0.001) hit++;
  }
  return hit / sim.nSims;
}

function parseMatchCount(selection) {
  const s = String(selection || "").trim();
  const orMore = /or more/i.test(s);
  const m = s.match(/(\d+)/);
  if (!m) return null;
  const n = Number(m[1]);
  return orMore ? { min: n } : { exact: n };
}

function probTiedMatchCount(sim, selection) {
  const parsed = parseMatchCount(selection);
  if (!parsed || !sim.halvedMatchSnapshots) return null;
  let hit = 0;
  for (let s = 0; s < sim.nSims; s++) {
    const n = sim.halvedMatchSnapshots[s] || 0;
    if (parsed.exact != null && n === parsed.exact) hit++;
    else if (parsed.min != null && n >= parsed.min) hit++;
  }
  return hit / sim.nSims;
}

function playerIdx(sim, dgId) {
  return sim.playerIndex.get(dgId);
}

function ptsAt(sim, simIdx, dgId) {
  return sim.pointSnapshots[simIdx * sim.nPlayers + playerIdx(sim, dgId)] || 0;
}

function topInPool(sim, poolIds, simIdx) {
  let best = -1;
  const leaders = [];
  for (const id of poolIds) {
    const pts = ptsAt(sim, simIdx, id);
    if (pts > best) {
      best = pts;
      leaders.length = 0;
      leaders.push(id);
    } else if (Math.abs(pts - best) < 0.001) {
      leaders.push(id);
    }
  }
  return leaders;
}

function probTopInPool(sim, poolIds, targetId) {
  if (!poolIds.length || targetId == null) return 0;
  let p = 0;
  for (let s = 0; s < sim.nSims; s++) {
    const leaders = topInPool(sim, poolIds, s);
    if (leaders.includes(targetId)) p += 1 / leaders.length;
  }
  return p / sim.nSims;
}

function probTopNInPool(sim, poolIds, targetId, n) {
  let p = 0;
  for (let s = 0; s < sim.nSims; s++) {
    const target = ptsAt(sim, s, targetId);
    const strictlyBetter = poolIds.filter(
      (id) => id !== targetId && ptsAt(sim, s, id) > target + 0.001,
    ).length;
    if (strictlyBetter >= n) continue;
    const tiedAtTarget = poolIds.filter((id) => Math.abs(ptsAt(sim, s, id) - target) < 0.001).length;
    const slots = n - strictlyBetter;
    p += Math.min(1, slots / tiedAtTarget);
  }
  return p / sim.nSims;
}

function probExactPoints(sim, dgId, pts) {
  let hit = 0;
  for (let s = 0; s < sim.nSims; s++) {
    if (Math.abs(ptsAt(sim, s, dgId) - pts) < 0.001) hit++;
  }
  return hit / sim.nSims;
}

function probGePoints(sim, dgId, line) {
  let p = 0;
  for (let s = 0; s < sim.nSims; s++) {
    if (ptsAt(sim, s, dgId) >= line - 0.001) p++;
  }
  return p / sim.nSims;
}

function probLtPoints(sim, dgId, line) {
  return 1 - probGePoints(sim, dgId, line);
}

function teamHoleSnaps(sim, { front9 = false, back9 = false } = {}) {
  if (front9) return sim.teamHoleFrontSnapshots;
  if (back9) return sim.teamHoleBackSnapshots;
  return sim.teamHoleSnapshots;
}

function usaSessionBoost(skillGap) {
  if (!skillGap || skillGap <= 0) return 0;
  return Math.min(0.1, 0.018 + skillGap * 0.065);
}

function probThreeWayFromCounts(usa, intl, tie, selection, skillGap = 0) {
  const n = usa + intl + tie;
  if (!n) return null;
  let capped = capThreeWayProb(usa / n, intl / n, tie / n);
  const b = usaSessionBoost(skillGap);
  if (b > 0) {
    capped = capThreeWayProb(capped.usa + b, Math.max(0.03, capped.int - b), capped.tie);
  }
  if (/^tie$/i.test(selection)) return capped.tie;
  if (/^usa$/i.test(selection)) return capped.usa;
  if (/^international$/i.test(selection)) return capped.int;
  return null;
}

function teamSkillGap(sim) {
  return (sim.teamAbility?.usa ?? 0) - (sim.teamAbility?.int ?? 0);
}

function probTeamMostHoles(sim, selection, { front9 = false, back9 = false } = {}) {
  const snap = teamHoleSnaps(sim, { front9, back9 });
  if (!snap) return null;
  let usa = 0;
  let intl = 0;
  let tie = 0;
  for (let s = 0; s < sim.nSims; s++) {
    const u = snap[s * 2] || 0;
    const i = snap[s * 2 + 1] || 0;
    if (u > i) usa++;
    else if (i > u) intl++;
    else tie++;
  }
  return probThreeWayFromCounts(usa, intl, tie, selection, teamSkillGap(sim));
}

function probMostHoles(sim, poolIds, targetId, { front9 = false, back9 = false } = {}) {
  let p = 0;
  for (let s = 0; s < sim.nSims; s++) {
    let best = -1;
    const leaders = [];
    for (const id of poolIds) {
      const idx = playerIdx(sim, id);
      let holes =
        sim.holeWinSnapshots[s * sim.nPlayers + idx] +
        (front9 ? 0 : 0); // full match tracked in hole wins
      if (front9) holes = sim.holeWinFrontSnapshots[s * sim.nPlayers + idx] || 0;
      else if (back9) holes = sim.holeWinBackSnapshots[s * sim.nPlayers + idx] || 0;
      else holes = sim.holeWinSnapshots[s * sim.nPlayers + idx] || 0;

      if (holes > best) {
        best = holes;
        leaders.length = 0;
        leaders.push(id);
      } else if (holes === best) {
        leaders.push(id);
      }
    }
    if (leaders.includes(targetId)) p += 1 / leaders.length;
  }
  return p / sim.nSims;
}

function sessionIdFromMarket(market) {
  for (const row of SESSION_BY_DK) {
    if (row.pattern.test(market)) return row.id;
  }
  return null;
}

function sessionScore(sim, simIdx, sessionId) {
  if (sessionId === "tournament_foursomes") {
    const a = sim.sessionSnapshots[simIdx * sim.nSessions + sim.sessionIndex.get("fri_foursomes")] || 0;
    const b = sim.sessionSnapshots[simIdx * sim.nSessions + sim.sessionIndex.get("sat_foursomes")] || 0;
    const c =
      sim.sessionSnapshots[simIdx * sim.nSessions + sim.sessionIndex.get("fri_foursomes") + sim.nSessions] || 0;
    // usa stored as pair [usa,int] per session - need proper layout
    return null;
  }
  const si = sim.sessionIndex.get(sessionId);
  if (si == null) return null;
  const base = simIdx * sim.nSessions * 2 + si * 2;
  return { usa: sim.sessionSnapshots[base] || 0, int: sim.sessionSnapshots[base + 1] || 0 };
}

function probSessionWinner(sim, sessionId, side) {
  let usa = 0;
  let int = 0;
  let halve = 0;
  const si = sim.sessionIndex.get(sessionId);
  if (si == null) return side === "USA" ? 0.5 : 0.5;
  for (let s = 0; s < sim.nSims; s++) {
    const base = s * sim.nSessions * 2 + si * 2;
    const u = sim.sessionSnapshots[base] || 0;
    const i = sim.sessionSnapshots[base + 1] || 0;
    if (u > i) usa++;
    else if (i > u) int++;
    else halve++;
  }
  return probThreeWayFromCounts(usa, int, halve, side, teamSkillGap(sim)) ?? 0;
}

function probCombinedSessions(sim, sessionIds, side) {
  let usa = 0;
  let int = 0;
  let halve = 0;
  for (let s = 0; s < sim.nSims; s++) {
    let u = 0;
    let i = 0;
    for (const sid of sessionIds) {
      const si = sim.sessionIndex.get(sid);
      if (si == null) continue;
      const base = s * sim.nSessions * 2 + si * 2;
      u += sim.sessionSnapshots[base] || 0;
      i += sim.sessionSnapshots[base + 1] || 0;
    }
    if (u > i) usa++;
    else if (i > u) int++;
    else halve++;
  }
  return probThreeWayFromCounts(usa, int, halve, side, teamSkillGap(sim)) ?? 0;
}

function probTournamentFoursomes(sim, side) {
  return probCombinedSessions(sim, ["fri_foursomes", "sat_foursomes"], side);
}

function probTournamentFourballs(sim, side) {
  return probCombinedSessions(sim, ["fri_fourball", "sat_fourball"], side);
}

/**
 * DK lists scores from the named team's perspective: "International 16-14" → USA 14, INT 16.
 */
function parseCorrectScore(selection) {
  const s = String(selection || "").trim();
  let m = s.match(/^(USA|International|Tie)\s+([\d.]+)\s*[-–]\s*([\d.]+)$/i);
  if (m) {
    const tag = m[1].toLowerCase();
    const a = Number(m[2]);
    const b = Number(m[3]);
    if (tag === "international") return { team: "International", usa: b, int: a };
    if (tag === "tie") return { team: "Tie", usa: a, int: b };
    return { team: "USA", usa: a, int: b };
  }
  m = s.match(/^([\d.]+)\s*[-–]\s*([\d.]+)$/);
  if (m) return { team: null, usa: Number(m[1]), int: Number(m[2]) };
  return null;
}

function scoreKey(usa, int) {
  return `${Math.round(usa * 2) / 2}-${Math.round(int * 2) / 2}`;
}

function combinedSessionScore(sim, simIdx, sessionIds) {
  let usa = 0;
  let intl = 0;
  for (const sid of sessionIds) {
    const row = sessionScore(sim, simIdx, sid);
    if (!row) return null;
    usa += row.usa;
    intl += row.int;
  }
  return { usa, int: intl };
}

/** Map DK correct-score market titles to sim score source. */
function correctScoreContext(market) {
  const name = String(market || "").trim();
  const lower = name.toLowerCase();

  if (/^day 3 correct score$/i.test(name)) {
    return { type: "combined", sessionIds: ["sat_foursomes", "sat_fourball"] };
  }
  if (/session 1 correct score/i.test(lower)) {
    return { type: "session", sessionIds: ["fri_foursomes"] };
  }

  const sid = sessionIdFromMarket(name);
  if (sid && sid !== "tournament_foursomes" && sid !== "tournament_fourballs") {
    return { type: "session", sessionIds: [sid] };
  }

  if (/correct score/i.test(lower)) return { type: "cup" };

  return { type: "cup" };
}

function probCorrectScore(sim, market, selection) {
  const parsed = parseCorrectScore(selection);
  if (!parsed) return null;
  const target = scoreKey(parsed.usa, parsed.int);
  const ctx = correctScoreContext(market);
  let hit = 0;

  for (let s = 0; s < sim.nSims; s++) {
    let row = null;
    if (ctx.type === "cup") {
      row = { usa: sim.cupSnapshots[s * 2] || 0, int: sim.cupSnapshots[s * 2 + 1] || 0 };
    } else if (ctx.type === "session") {
      row = sessionScore(sim, s, ctx.sessionIds[0]);
    } else if (ctx.type === "combined") {
      row = combinedSessionScore(sim, s, ctx.sessionIds);
    }
    if (row && scoreKey(row.usa, row.int) === target) hit++;
  }

  return hit / sim.nSims;
}

function parsePointsLine(selection) {
  const s = String(selection || "");
  const m = s.match(/(over|under)\s*([\d.]+)/i);
  if (m) return { side: m[1].toLowerCase(), line: Number(m[2]) };
  const m2 = s.match(/([\d.]+)\+?$/);
  if (m2 && /over|under/i.test(s)) return null;
  return null;
}

function parsePlayerFromMarketName(market) {
  const m = String(market || "").match(/^(.+?)\s+(to win a point|total points)/i);
  return m ? m[1].trim() : null;
}

function isTeamSelection(sel) {
  const s = String(sel || "").trim();
  if (/^(usa|united states|international|tie)$/i.test(s)) return true;
  if (/^(usa|international|tie)\s[\d.+-]/i.test(s)) return true;
  return false;
}

function isTeamFormatMarket(market, selection) {
  const m = String(market || "").toLowerCase();
  const s = String(selection || "").trim();
  if (isTeamSelection(s)) return true;
  if (/correct score|team most holes|total usa points|total international points/i.test(m)) return true;
  if (/tournament fourballs winner|tournament foursomes winner|clean sweep|leading after/i.test(m)) return true;
  if (/match winner|hole \d+|outright winner|lift the trophy|cup result/i.test(m)) return true;
  if (/biggest|total halved|total tied|total matches|go to 1[78]|margin/i.test(m)) return true;
  if (isYesNo(s) && /clean sweep|go to|halved|tied/i.test(m)) return true;
  return false;
}

function showExpectedPointsForProp(prop) {
  if (isTeamFormatMarket(prop.market, prop.selection)) return false;
  const m = String(prop.market || "").toLowerCase();
  const s = String(prop.selection || "").trim();
  if (/ to win a point$| total points$/.test(m)) return true;
  if (isYesNo(s) || isTeamSelection(s) || /^(over|under)\b/i.test(s)) return false;
  if (/^\d+(\.\d+)?$/.test(s)) return false;
  if (/scorer|qualifier|debutant|wildcard|rookie|captain|most holes|most birdies|play most holes/i.test(m)) {
    return !/team most holes/i.test(m);
  }
  return false;
}

function isYesNo(sel) {
  return /^(yes|no)$/i.test(String(sel || "").trim());
}

function marketKind(market, selection) {
  const m = String(market || "").toLowerCase();
  const s = String(selection || "").trim();

  if (/correct score/i.test(m)) return "correct_score";
  if (/match winner/i.test(m)) return /3\s*way/i.test(m) ? "pair_match_3way" : "pair_match_2way";
  if (/hole\s+\d+.*3\s*way/i.test(m)) return "pair_hole_3way";
  if (/total halved matches/i.test(m)) return "halved_matches_ou";
  if (/total tied matches/i.test(m)) return "tied_matches_count";
  if (/total usa points|total international points|team total/i.test(m)) return "team_points_ou";
  if (/total points/i.test(m) && !/scorer/i.test(m)) {
    return /^\d+(\.\d+)?$/.test(s) ? "player_points_exact" : "player_points_ou";
  }
  if (/to win a point/i.test(m)) return "win_point";
  if (/team most holes won on front/i.test(m)) return "team_most_holes_front";
  if (/team most holes won on back/i.test(m)) return "team_most_holes_back";
  if (/most holes won on front/i.test(m)) return "most_holes_front";
  if (/most holes won on back/i.test(m)) return "most_holes_back";
  if (/most holes|win the most holes|play most holes/i.test(m)) return "most_holes";
  if (/most birdies/i.test(m)) return "most_birdies";
  if (/debutant|wildcard|rookie/i.test(m) && !/team/i.test(m)) return "debutant_pool";
  if (/top 3 finish|top three/i.test(m)) return "top3_pool";
  if (/outright winner|lift the trophy|cup result/i.test(m) && isTeamSelection(s)) return "cup_winner";
  if (/tournament foursomes winner/i.test(m) && isTeamSelection(s)) return "tournament_foursomes_winner";
  if (/tournament fourballs winner/i.test(m) && isTeamSelection(s)) return "tournament_fourballs_winner";
  if (/\bwinner\b|leading after|clean sweep|after day/i.test(m) && isTeamSelection(s)) return "team_winner";
  if (isYesNo(s) && /go to 1[78]|clean sweep|halved|tied|momentum|margin/i.test(m)) return "binary_event";
  if (/^[\d.]+\s*[-–]\s*[\d.]+$/.test(s) || /^(USA|International|Tie)\s+[\d.]/i.test(s)) return "correct_score";
  return "top_pool";
}

/**
 * Price a single DK line from Monte Carlo snapshots.
 */
export function priceDkLine({ market, selection, poolSelections }, ctx) {
  const { sim, field, playerIndex } = ctx;
  const kind = marketKind(market, selection);
  const sel = String(selection || "").trim();
  const poolNames = poolSelections || [sel];
  const poolPlayers = poolNames
    .map((n) => resolvePlayer(n, field, playerIndex))
    .filter(Boolean);
  const poolIds = poolPlayers.map((p) => p.dg_id);
  const target = resolvePlayer(sel, field, playerIndex);

  switch (kind) {
    case "cup_winner": {
      if (/^usa$/i.test(sel)) return sim.cupProb.usa_win + sim.cupProb.usa_retain_tie;
      if (/^international$/i.test(sel)) return sim.cupProb.int_win;
      if (/^tie$/i.test(sel)) return sim.cupProb.usa_retain_tie;
      return null;
    }
    case "tournament_foursomes_winner":
      return probTournamentFoursomes(sim, sel);
    case "tournament_fourballs_winner":
      return probTournamentFourballs(sim, sel);
    case "team_winner": {
      const sid = sessionIdFromMarket(market);
      if (sid) return probSessionWinner(sim, sid, sel);
      if (/leading after day 2/i.test(market)) {
        let usa = 0;
        let intl = 0;
        let tie = 0;
        for (let s = 0; s < sim.nSims; s++) {
          const u = sim.cupSnapshots[s * 2] || 0;
          const i = sim.cupSnapshots[s * 2 + 1] || 0;
          if (u > i) usa++;
          else if (i > u) intl++;
          else tie++;
        }
        return probThreeWayFromCounts(usa, intl, tie, sel, teamSkillGap(sim));
      }
      return probSessionWinner(sim, sessionIdFromMarket(market) || "fri_foursomes", sel);
    }
    case "correct_score":
      return probCorrectScore(sim, market, sel);
    case "pair_match_2way":
    case "pair_match_3way":
    case "pair_hole_3way":
      return probPairSelection(market, sel, poolNames, ctx);
    case "halved_matches_ou":
      return probHalvedMatchesOu(sim, sel);
    case "tied_matches_count":
      return probTiedMatchCount(sim, sel);
    case "win_point": {
      const pname = parsePlayerFromMarketName(market) || sel;
      const p = resolvePlayer(pname, field, playerIndex);
      if (!p) return null;
      return probGePoints(sim, p.dg_id, 1);
    }
    case "player_points_exact": {
      const pname = parsePlayerFromMarketName(market);
      const p = resolvePlayer(pname, field, playerIndex);
      const pts = Number(sel);
      if (!p || !Number.isFinite(pts)) return null;
      return probExactPoints(sim, p.dg_id, pts);
    }
    case "player_points_ou": {
      const parsed = parsePointsLine(sel);
      const pname = parsePlayerFromMarketName(market);
      const p = resolvePlayer(pname, field, playerIndex);
      if (!p || !parsed) return null;
      return parsed.side === "over"
        ? probGePoints(sim, p.dg_id, parsed.line)
        : probLtPoints(sim, p.dg_id, parsed.line);
    }
    case "team_points_ou": {
      const parsed = parsePointsLine(sel);
      const line = parsed?.line ?? 15.5;
      let hit = 0;
      for (let s = 0; s < sim.nSims; s++) {
        const u = sim.cupSnapshots[s * 2] || 0;
        const i = sim.cupSnapshots[s * 2 + 1] || 0;
        const val = /international/i.test(market) ? i : u;
        if (parsed?.side === "under") {
          if (val < line) hit++;
        } else if (val >= line) hit++;
      }
      return hit / sim.nSims;
    }
    case "team_most_holes_front":
      return probTeamMostHoles(sim, sel, { front9: true });
    case "team_most_holes_back":
      return probTeamMostHoles(sim, sel, { back9: true });
    case "most_holes_front":
      if (!target) return null;
      return probMostHoles(sim, poolIds.length ? poolIds : sim.allPlayerIds, target.dg_id, { front9: true });
    case "most_holes_back":
      if (!target) return null;
      return probMostHoles(sim, poolIds.length ? poolIds : sim.allPlayerIds, target.dg_id, { back9: true });
    case "most_holes":
      if (!target) return null;
      return probMostHoles(sim, poolIds.length ? poolIds : sim.allPlayerIds, target.dg_id);
    case "most_birdies":
      if (!target) return null;
      return probTopInPool(sim, poolIds.length ? poolIds : sim.allPlayerIds, target.dg_id);
    case "top3_pool":
      if (!target) return null;
      return probTopNInPool(sim, poolIds, target.dg_id, 3);
    case "debutant_pool":
      if (!target || !poolIds.length) return null;
      return probTopInPool(sim, poolIds, target.dg_id);
    case "binary_event":
      return priceBinaryEvent(market, sel, sim);
    case "top_pool":
    default:
      if (!target || !poolIds.length) return null;
      if (/top 3|top three/i.test(market)) return probTopNInPool(sim, poolIds, target.dg_id, 3);
      return probTopInPool(sim, poolIds, target.dg_id);
  }
}

function priceBinaryEvent(market, selection, sim) {
  const yes = /^yes$/i.test(selection);
  const m = market.toLowerCase();
  let p = 0.5;
  if (/18th hole|17th hole|go to 18|go to 17/i.test(m)) p = 0.35;
  if (/clean sweep/i.test(m)) p = 0.08;
  if (/halved|tied match/i.test(m)) p = 0.12;
  return yes ? p : 1 - p;
}

export function buildPricingContext(sim, field) {
  return {
    sim,
    field,
    playerIndex: buildPlayerIndex(field),
    matchCache: new Map(),
    playerDists: buildPlayerDistMap(field.players, field.hole_pars, field.course_adj_stp),
  };
}

export function priceDkProp(prop, poolSelections, ctx) {
  let prob = priceDkLine(
    {
      market: prop.market,
      selection: prop.selection,
      poolSelections,
    },
    ctx,
  );
  if (prob == null || !Number.isFinite(prob)) {
    return { model_prob: null, model_american: null };
  }
  const clamped = Math.max(0.0005, Math.min(0.9995, prob));
  return {
    model_prob: clamped,
    model_american: probToAmerican(clamped),
  };
}

function expectedPointsForProp(prop, ctx) {
  if (!showExpectedPointsForProp(prop)) return null;
  if (isTeamSelection(prop.selection)) return null;
  let player = resolvePlayer(prop.selection, ctx.field, ctx.playerIndex);
  if (!player) {
    const fromMarket = parsePlayerFromMarketName(prop.market);
    if (fromMarket) player = resolvePlayer(fromMarket, ctx.field, ctx.playerIndex);
  }
  if (!player) return null;
  return ctx.sim.expPointsById?.get(player.dg_id) ?? null;
}

function marketGroupForProp(prop, poolSelections, ctx) {
  const m = prop.market;
  if (/match winner|hole \d+.*3\s*way/i.test(m)) {
    const { usa, intl } = pairSidesFromPool(poolSelections, ctx.field, ctx.playerIndex);
    if (usa?.sel && intl?.sel) return `${m} · ${usa.sel} vs ${intl.sel}`;
  }
  return prop.market;
}

export function attachModelToRow(prop, poolSelections, ctx, manifestExtras = {}) {
  const priced = priceDkProp(prop, poolSelections, ctx);
  const marketPlayer = parsePlayerFromMarketName(prop.market);
  const player = isTeamSelection(prop.selection)
    ? null
    : resolvePlayer(prop.selection, ctx.field, ctx.playerIndex) ||
      (marketPlayer ? resolvePlayer(marketPlayer, ctx.field, ctx.playerIndex) : null);
  const team = /^usa$/i.test(prop.selection)
    ? "USA"
    : /^international$/i.test(prop.selection)
      ? "INT"
      : (player?.team ?? null);
  const bookProb =
    prop.dk_implied_prob != null && Number.isFinite(prop.dk_implied_prob)
      ? prop.dk_implied_prob
      : prop.american != null
        ? americanToProb(prop.american)
        : null;
  const modelProb = priced?.model_prob ?? null;
  return {
    ...manifestExtras,
    market: prop.market,
    selection: prop.selection,
    team,
    expected_points: expectedPointsForProp(prop, ctx),
    show_expected_points: showExpectedPointsForProp(prop),
    model_prob: modelProb,
    model_american: priced?.model_american ?? null,
    book_american: prop.american,
    book_american_display: prop.american_display ?? null,
    book_prob: bookProb,
    edge_pct: modelProb != null && bookProb != null ? (modelProb - bookProb) * 100 : null,
    ev_per_unit: modelProb != null && prop.american != null ? evPerUnit(modelProb, prop.american) : null,
    dk_market: prop.market,
    dk_market_id: prop.marketId ?? null,
    market_group: marketGroupForProp(prop, poolSelections, ctx),
  };
}
