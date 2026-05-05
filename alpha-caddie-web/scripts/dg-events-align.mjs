/** Shared fuzzy comparison for DataGolf tournament titles (projections vs field-updates vs preds/in-play). */

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
