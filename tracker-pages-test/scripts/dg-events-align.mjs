/** Shared fuzzy comparison for DataGolf tournament titles (projections vs field-updates vs preds/in-play). */

const TITLE_STOP = new Set(["the", "and", "for", "with", "from", "that", "this", "its"]);

/** Lowercase folded title for stable week keys (year stripped). */
export function foldComparableTitle(s) {
  return String(s || "")
    .trim()
    .toLowerCase()
    .replace(/\b20\d{2}\b/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

/** Compare projections snapshot vs DataGolf field-updates (`event|||course`). */
export function fieldWeekKey(eventName, courseName) {
  return `${foldComparableTitle(eventName)}|||${foldComparableTitle(courseName)}`;
}

/** Allow minor wording differences between feeds (substring on event/course). */
export function fieldWeekKeysRoughMatch(projKey, fuKey) {
  if (!projKey || !fuKey) return false;
  if (projKey === fuKey) return true;
  const [ea, ca] = projKey.split("|||");
  const [eb, cb] = fuKey.split("|||");
  if (!ea || !eb) return false;
  const evOk = ea === eb || ea.includes(eb) || eb.includes(ea);
  const coOk =
    !ca ||
    !cb ||
    ca === cb ||
    ca.includes(cb) ||
    cb.includes(ca);
  return evOk && coOk;
}

export function tokenizeEventTitle(s) {
  return foldComparableTitle(s)
    .split(/[^a-z0-9]+/)
    .filter((t) => t.length >= 3 && !TITLE_STOP.has(t));
}

/** 0–1 overlap of token bags; 1 if either side has no tokens (no signal). */
export function titleTokenOverlapRatio(a, b) {
  const ta = tokenizeEventTitle(a);
  const tb = tokenizeEventTitle(b);
  if (!ta.length || !tb.length) return 1;
  const sb = new Set(tb);
  let hit = 0;
  for (const t of ta) if (sb.has(t)) hit++;
  return hit / Math.min(ta.length, tb.length);
}

/** Both courses known and clearly unrelated (TPC vs Pebble). */
export function coursesClearlyDistinct(projCourse, fuCourse) {
  const x = foldComparableTitle(projCourse);
  const y = foldComparableTitle(fuCourse);
  if (!x || !y) return false;
  if (x === y) return false;
  if (x.includes(y) || y.includes(x)) return false;
  return true;
}

export function normEventName(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/\b(the|pga|liv\s*golf|dp\s*world)\b/g, " ")
    .replace(/\b(championship|tournament|invitational|classic|open)\b/g, " ")
    .replace(/[^a-z0-9]+/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

export function eventsLikelySame(a, b) {
  const fa = foldComparableTitle(a);
  const fb = foldComparableTitle(b);
  if (fa && fb && (fa === fb || fa.includes(fb) || fb.includes(fa))) return true;

  const x = normEventName(a);
  const y = normEventName(b);
  if (!x || !y) return false;
  if (x === y || x.includes(y) || y.includes(x)) return true;
  const xt = x.split(" ").filter((t) => t.length >= 4);
  const yt = y.split(" ").filter((t) => t.length >= 4);
  if (!xt.length || !yt.length) return false;
  const hit = xt.filter((t) => yt.some((u) => u.includes(t) || t.includes(u))).length;
  return hit >= Math.min(2, Math.min(xt.length, yt.length));
}
