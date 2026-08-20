/**
 * Hard Rock pre-round O/U from repo data/odds.csv (Birdies + Round Score).
 * Index shape matches pickem-pre-round-props / dk-pre-round-props.
 */
import { eventsLikelySame } from "./dg-events-align.mjs";
import { num } from "./dk-pre-round-props.mjs";
import {
  defaultOddsCsvPath,
  loadOddsCsvPropsIndex,
} from "./odds-csv-props.mjs";

function completeHrOddsPair(over, under) {
  if (Number.isFinite(over) && Number.isFinite(under)) return { over, under };
  if (Number.isFinite(over)) return { over, under: -110 };
  if (Number.isFinite(under)) return { over: -110, under };
  return { over: NaN, under: NaN };
}

function mergeHrSnap(best, key, snap) {
  const prev = best.get(key);
  if (!prev) {
    best.set(key, { ...snap });
    return;
  }
  const next = { ...prev };
  if (!Number.isFinite(prev.openCapturedMs) || snap.openCapturedMs < prev.openCapturedMs) {
    next.openLine = snap.openLine;
    next.openOver = snap.openOver;
    next.openUnder = snap.openUnder;
    next.openCapturedMs = snap.openCapturedMs;
  }
  if (!Number.isFinite(prev.capturedMs) || snap.capturedMs > prev.capturedMs) {
    Object.assign(next, snap, {
      openLine: next.openLine,
      openOver: next.openOver,
      openUnder: next.openUnder,
      openCapturedMs: next.openCapturedMs,
    });
  }
  best.set(key, next);
}

/**
 * @param {string} eventName DataGolf / audit event title
 * @param {string} oddsPath path to odds.csv
 * @param {Map<number, number>} _roundStartUtcMs unused (tee times embedded in odds.csv)
 * @param {{ histRows?: object[] }} [opts]
 * @returns {Promise<Map<string, object>>}
 */
export async function loadPreRoundHrPropsFromOddsCsv(
  eventName,
  oddsPath,
  _roundStartUtcMs,
  opts = {},
) {
  const best = new Map();
  const ev = String(eventName || "").trim();
  if (!ev) return best;

  const props = loadOddsCsvPropsIndex(oddsPath || defaultOddsCsvPath(), opts.histRows || null);
  for (const p of props.values()) {
    if (!eventsLikelySame(ev, p.event)) continue;
    const dg = Math.round(num(p.dg_id, NaN));
    const rnd = Math.round(num(p.round, NaN));
    const market = String(p.props_market || "").trim();
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
    if (market !== "Birdies" && market !== "Total Score") continue;
    const odds = completeHrOddsPair(p.over_am, p.under_am);
    if (!Number.isFinite(p.line) || !Number.isFinite(odds.over) || !Number.isFinite(odds.under)) continue;

    const capturedMs = num(p.bet_time_ms, NaN);
    const snap = {
      line: p.line,
      over: odds.over,
      under: odds.under,
      capturedMs,
      dg,
      playerName: String(p.matched_player || p.player || "").trim(),
      market,
      course: String(p.course_name || "").trim(),
      projAt: "",
      openLine: p.line,
      openOver: Number.isFinite(p.open_over_am) ? p.open_over_am : odds.over,
      openUnder: Number.isFinite(p.open_under_am) ? p.open_under_am : odds.under,
      openCapturedMs: capturedMs,
      displayRound: rnd,
    };
    mergeHrSnap(best, `${dg}|${rnd}|${market}`, snap);
  }
  return best;
}

export { defaultOddsCsvPath };
