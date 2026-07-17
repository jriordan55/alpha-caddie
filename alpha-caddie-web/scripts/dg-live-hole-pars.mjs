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

/**
 * Morning vs afternoon scoring + counting averages from preds/live-hole-stats
 * (same feed as DataGolf "SPLIT BY WAVE" hole-by-hole course stats).
 * Counts on each hole are field totals; divide by players_thru for per-player means.
 *
 * @returns {{ round: number, morning: object, afternoon: object, total: object, deltaAfternoonMinusMorning: number, deltaBirdiesAfternoonMinusMorning: number, deltaBogeysAfternoonMinusMorning: number, n: number, source: string } | null}
 */
export function waveScoringBiasFromLiveHoleStats(lh, course_used, fieldRaw, event_name, opts = {}) {
  if (!lh || typeof lh !== "object") return null;
  if (!liveHoleStatsUsableForProjections(lh, event_name, course_used)) return null;

  const courseEntry = pickLiveHoleStatsCourseEntry(lh, course_used, fieldRaw);
  if (!courseEntry) return null;
  const rounds = Array.isArray(courseEntry.rounds) ? courseEntry.rounds : [];
  if (!rounds.length) return null;

  const preferRound = Math.round(num(opts.round, NaN));
  let roundPick = null;
  if (Number.isFinite(preferRound)) {
    roundPick =
      rounds.find((r) => Math.round(num(r.round_num ?? r.roundNum, NaN)) === preferRound) || null;
  }
  if (!roundPick) {
    // Prefer latest round with both waves populated.
    for (let i = rounds.length - 1; i >= 0; i--) {
      const holes = rounds[i]?.holes;
      if (!Array.isArray(holes) || holes.length < 9) continue;
      const hasAm = holes.some((h) => h?.morning_wave && num(h.morning_wave.players_thru, 0) > 0);
      const hasPm = holes.some((h) => h?.afternoon_wave && num(h.afternoon_wave.players_thru, 0) > 0);
      if (hasAm && hasPm) {
        roundPick = rounds[i];
        break;
      }
    }
  }
  if (!roundPick) roundPick = rounds[0];
  const holes = Array.isArray(roundPick.holes) ? roundPick.holes : [];
  if (holes.length < 9) return null;

  function aggWave(key) {
    let stp = 0;
    let bird = 0;
    let bog = 0;
    let pars = 0;
    let eag = 0;
    let dbl = 0;
    let nHoles = 0;
    let thru = 0;
    for (const h of holes) {
      const w = h?.[key];
      if (!w || typeof w !== "object") continue;
      const n = num(w.players_thru, 0);
      if (n < 8) continue;
      const par = num(h.par, NaN);
      const avg = num(w.avg_score, NaN);
      if (!Number.isFinite(par) || !Number.isFinite(avg)) continue;
      stp += avg - par;
      bird += num(w.birdies, 0) / n;
      bog += num(w.bogeys, 0) / n;
      pars += num(w.pars, 0) / n;
      eag += num(w.eagles_or_better, 0) / n;
      dbl += num(w.doubles_or_worse, 0) / n;
      nHoles++;
      thru = Math.max(thru, n);
    }
    if (nHoles < 9) return null;
    return {
      stp: Math.round(stp * 1000) / 1000,
      birdies: Math.round(bird * 1000) / 1000,
      bogeys: Math.round(bog * 1000) / 1000,
      pars: Math.round(pars * 1000) / 1000,
      eagles: Math.round(eag * 1000) / 1000,
      doubles: Math.round(dbl * 1000) / 1000,
      holes: nHoles,
      players_thru: thru,
    };
  }

  const morning = aggWave("morning_wave");
  const afternoon = aggWave("afternoon_wave");
  const total = aggWave("total");
  if (!morning || !afternoon) return null;

  const rnd = Math.round(num(roundPick.round_num ?? roundPick.roundNum, NaN));
  return {
    round: Number.isFinite(rnd) ? rnd : null,
    morning,
    afternoon,
    total,
    deltaAfternoonMinusMorning: Math.round((afternoon.stp - morning.stp) * 1000) / 1000,
    deltaBirdiesAfternoonMinusMorning: Math.round((afternoon.birdies - morning.birdies) * 1000) / 1000,
    deltaBogeysAfternoonMinusMorning: Math.round((afternoon.bogeys - morning.bogeys) * 1000) / 1000,
    n: morning.players_thru + afternoon.players_thru,
    source: "live_hole_stats",
  };
}

