/**
 * Match PrizePicks prop rows to projections.json field (dg_id + name).
 */
import { golferNamesLikelySame, matchPlayerByGolferLabel } from "./golfer-name-match.mjs";

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

/** Active field rows for one round (one row per dg_id). */
export function projectionFieldPlayersForRound(players, round) {
  const want = Math.round(num(round, NaN));
  const byId = new Map();
  for (const p of players || []) {
    const id = Math.round(num(p?.dg_id, NaN));
    const rnd = Math.round(num(p?.round, NaN));
    if (!Number.isFinite(id) || id <= 0) continue;
    if (Number.isFinite(want) && want >= 1 && want <= 4 && rnd !== want) continue;
    if (!byId.has(id)) byId.set(id, p);
  }
  return [...byId.values()];
}

/** True when a PP/DK prop row matches a projections field player. */
export function ppPropMatchesProjectionField(prop, players) {
  if (!prop || !Array.isArray(players) || !players.length) return false;
  const id = Math.round(num(prop?.dg_id, NaN));
  if (Number.isFinite(id) && id > 0) {
    for (const p of players) {
      if (Math.round(num(p?.dg_id, NaN)) === id) return true;
    }
  }
  const label = String(prop?.player_name || "").trim();
  if (!label) return false;
  if (matchPlayerByGolferLabel(players, label)) return true;
  for (const p of players) {
    if (golferNamesLikelySame(p?.player_name, label)) return true;
  }
  return false;
}

/** Drop PP rows that do not match the weekly field (stale cross-event slate). */
export function filterPpPropsToProjectionField(ppProps, players, round = NaN) {
  const field = projectionFieldPlayersForRound(players, round);
  if (!field.length) return ppProps || [];
  return (ppProps || []).filter((r) => ppPropMatchesProjectionField(r, field));
}

/**
 * PrizePicks game metadata vs projections event/course/slug.
 * @param {object} gameAttrs
 * @param {object} payload projections.json root
 */
export function ppGameMatchesProjectionEvent(gameAttrs, payload) {
  const hints = [
    gameAttrs?.metadata?.game_info,
    gameAttrs?.name,
    gameAttrs?.team,
    gameAttrs?.title,
    gameAttrs?.description,
  ]
    .map((s) => String(s || "").toLowerCase())
    .filter(Boolean);
  if (!hints.length) return true;
  const blob = hints.join(" ");
  const tokens = [
    payload?.event_name,
    payload?.course_used,
    String(payload?.dk_league_slug || "").replace(/-/g, " "),
    payload?.datagolf_schedule_anchor_event,
  ]
    .map((s) => String(s || "").toLowerCase())
    .filter(Boolean);
  for (const t of tokens) {
    const words = t.split(/\s+/).filter((w) => w.length >= 4);
    if (words.some((w) => blob.includes(w))) return true;
    if (t.length >= 6 && blob.includes(t)) return true;
  }
  return false;
}

/**
 * Game ids that look like this week's event. null = do not filter by game.
 * @param {Map<string, object>} gameMap
 * @param {object} payload
 */
export function ppMatchingGameIds(gameMap, payload) {
  if (!gameMap?.size) return null;
  const ids = new Set();
  let identifiable = 0;
  for (const [id, gm] of gameMap) {
    const hasHint = Boolean(
      gm?.metadata?.game_info || gm?.name || gm?.team || gm?.title || gm?.description,
    );
    if (!hasHint) {
      ids.add(id);
      continue;
    }
    identifiable++;
    if (ppGameMatchesProjectionEvent(gm, payload)) ids.add(id);
  }
  if (!identifiable) return null;
  return ids.size ? ids : null;
}
