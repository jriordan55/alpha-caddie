/**
 * Shared parsing for DataGolf preds/live-hole-stats (same JSON shape as live-in-play.json `live_hole_stats`).
 * Used by fetch-datagolf.mjs and merge-live-hole-pars-into-projections.mjs (npm run push:all).
 */
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function asArray(x) {
  if (x == null) return [];
  if (Array.isArray(x)) return x;
  return [];
}

function rowsFromResponse(dat) {
  if (dat == null) return [];
  if (Array.isArray(dat)) return dat;
  if (typeof dat !== "object") return [];
  for (const k of ["data", "players", "field", "baseline_history_fit", "baseline", "rankings", "results", "rows"]) {
    const v = dat[k];
    if (Array.isArray(v)) return v;
  }
  if (Array.isArray(dat.baseline_history_fit)) return dat.baseline_history_fit;
  return [];
}

/** Distinct DataGolf `course_num` from field-updates teetimes — aligns with live_hole_stats `course_key`. */
export function courseNumsFromFieldRaw(fieldRaw) {
  const nums = new Set();
  const fieldList = asArray(fieldRaw?.field).length ? asArray(fieldRaw.field) : rowsFromResponse(fieldRaw || {});
  for (const p of fieldList) {
    const tt = p?.teetimes;
    if (!Array.isArray(tt)) continue;
    for (const t of tt) {
      const n = t?.course_num ?? t?.courseNum;
      if (n == null || n === "") continue;
      nums.add(String(n).trim());
    }
  }
  return nums;
}

/** True when live-hole-stats event/course align with this week's projections venue. */
export function liveHoleStatsUsableForProjections(lh, event_name, course_used) {
  if (!lh || typeof lh !== "object") return false;
  const lhEv = String(lh.event_name ?? "").trim();
  const ev = String(event_name ?? "").trim();
  if (lhEv && ev && foldComparableTitle(lhEv) !== foldComparableTitle(ev) && !eventsLikelySame(lhEv, ev)) {
    return false;
  }
  const cu = String(course_used ?? "").trim();
  if (!cu) return true;
  return !!pickLiveHoleStatsCourseEntry(lh, cu, null);
}

export function pickLiveHoleStatsCourseEntry(lh, course_used, fieldRaw) {
  const courses = lh?.courses;
  if (!Array.isArray(courses) || !courses.length) return null;
  if (courses.length === 1) return courses[0];

  const nums = courseNumsFromFieldRaw(fieldRaw);
  if (nums.size) {
    for (const c of courses) {
      const ck = String(c.course_key ?? c.courseKey ?? "").trim();
      if (ck && nums.has(ck)) return c;
    }
  }

  const cu = foldComparableTitle(course_used);
  for (const c of courses) {
    const cn = String(c.course_name ?? c.courseName ?? "").trim();
    if (cn && cu && (foldComparableTitle(cn) === cu || eventsLikelySame(cn, course_used))) return c;
  }

  console.warn(
    "Hole pars: preds/live-hole-stats lists multiple courses — no teetimes course_num/course_name match; skipping DG hole table",
  );
  return null;
}

/**
 * Per-hole par from preds/live-hole-stats (same structure as live-in-play.json `live_hole_stats`).
 * Requires event_name to match feed when the feed includes event_name (avoids stale prior-week data).
 */
export function holeParsFromLiveHoleStatsPayload(lh, course_used, fieldRaw, event_name) {
  if (!lh || typeof lh !== "object") return null;

  const lhEv = String(lh.event_name ?? "").trim();
  const ev = String(event_name ?? "").trim();
  if (lhEv && ev && foldComparableTitle(lhEv) !== foldComparableTitle(ev) && !eventsLikelySame(lhEv, ev)) {
    return null;
  }

  const courseEntry = pickLiveHoleStatsCourseEntry(lh, course_used, fieldRaw);
  if (!courseEntry) return null;

  const rounds = courseEntry.rounds;
  if (!Array.isArray(rounds) || !rounds.length) return null;

  const cr = num(lh.current_round, NaN);
  let roundPick = rounds;
  if (Number.isFinite(cr)) {
    const matched = rounds.filter((r) => num(r.round_num ?? r.roundNum, NaN) === cr);
    if (matched.length) roundPick = matched;
  } else {
    let maxRn = -Infinity;
    for (const r of rounds) {
      const rn = num(r.round_num ?? r.roundNum, NaN);
      if (Number.isFinite(rn)) maxRn = Math.max(maxRn, rn);
    }
    if (Number.isFinite(maxRn)) {
      const matched = rounds.filter((r) => num(r.round_num ?? r.roundNum, NaN) === maxRn);
      if (matched.length) roundPick = matched;
    }
  }

  const holes = roundPick[0]?.holes;
  if (!Array.isArray(holes) || holes.length < 18) return null;

  const byHole = new Map();
  for (const h of holes) {
    if (!h || typeof h !== "object") continue;
    const hn = Math.round(num(h.hole, NaN));
    const p = num(h.par, NaN);
    if (!Number.isFinite(hn) || hn < 1 || hn > 18) continue;
    if (!Number.isFinite(p) || p < 3 || p > 5) continue;
    byHole.set(hn, Math.round(p));
  }
  if (byHole.size < 18) return null;
  const arr = [];
  for (let i = 1; i <= 18; i++) {
    if (!byHole.has(i)) return null;
    arr.push(byHole.get(i));
  }
  return arr;
}
