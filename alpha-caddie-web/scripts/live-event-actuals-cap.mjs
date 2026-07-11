/**
 * Live-week actuals: last completed round cap + pgatour row event validation.
 */
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function courseNamesMatch(a, b) {
  const ak = normCourseNameKey(a);
  const bk = normCourseNameKey(b);
  if (!ak || !bk) return false;
  return ak === bk || ak.includes(bk) || bk.includes(ak);
}

/** Discriminating tokens (e.g. "scottish") must appear on both sides when present in projections event. */
export function pgatourRowBelongsToEvent(row, eventName, opts = {}) {
  const want = String(eventName || "").trim();
  const courseUsed = String(opts.courseUsed || "").trim();
  const rowCourse = String(row?.course_name || "").trim();
  if (want && courseUsed && rowCourse && courseNamesMatch(courseUsed, rowCourse)) return true;

  const rowEv = String(row?.event_name || row?.tournament_name || "").trim();
  if (!rowEv || !want) return false;
  const pe = foldComparableTitle(want);
  const re = foldComparableTitle(rowEv);
  const skip = new Set(["genesis", "the", "open", "pga", "tour"]);
  const projWords = pe.split(/\s+/).filter((w) => w.length >= 5 && !skip.has(w));
  for (const w of projWords) {
    if (!re.includes(w)) return false;
  }
  const rowWords = re.split(/\s+/).filter((w) => w.length >= 5 && !skip.has(w));
  for (const w of rowWords) {
    if (!pe.includes(w)) return false;
  }
  return eventsLikelySame(rowEv, want);
}

/**
 * Highest round with a finished score in live_round_actuals_by_dg (thru >= 18 or thru unset).
 * @param {object} payload projections.json or meta wrapper
 */
export function completedRoundCapFromPayload(payload) {
  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : payload;
  const actuals = meta?.live_round_actuals_by_dg ?? payload?.live_round_actuals_by_dg;
  let maxComplete = 0;
  if (actuals && typeof actuals === "object") {
    for (const perRound of Object.values(actuals)) {
      if (!perRound || typeof perRound !== "object") continue;
      for (const [rndKey, act] of Object.entries(perRound)) {
        const rnd = Math.round(num(rndKey, NaN));
        const score = num(act?.round_score, NaN);
        const thru = Math.round(num(act?.thru, NaN));
        if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
        if (!Number.isFinite(score) || score <= 0) continue;
        if (!Number.isFinite(thru) || thru >= 18) maxComplete = Math.max(maxComplete, rnd);
      }
    }
  }
  if (maxComplete > 0) return maxComplete;

  const liveR = Math.round(
    num(meta?.datagolf_field_current_round ?? meta?.datagolf_live_current_round ?? payload?.display_round, NaN),
  );
  if (Number.isFinite(liveR) && liveR > 1) return liveR - 1;
  return Number.isFinite(liveR) ? liveR : NaN;
}

/** @param {Map<string, object>} map `${dg}|${rnd}` */
export function liveScoreInMap(map, dg, rnd) {
  const act = map.get(`${dg}|${rnd}`);
  const score = num(act?.total_score ?? act?.round_score, NaN);
  return Number.isFinite(score) && score > 0 ? score : NaN;
}
