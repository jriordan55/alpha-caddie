/**
 * Match DraftKings golfer labels to DataGolf / projections field names.
 * Handles "Last, First" vs "First Last", nicknames (John/Johnny), and shortened first names (Matti/Matthias).
 */

export function displayGolferName(nameRaw) {
  const s = String(nameRaw || "").trim();
  const m = s.match(/^([^,]+),\s*(.+)$/);
  if (m) return `${m[2].trim()} ${m[1].trim()}`.trim();
  return s;
}

export function normNameLoose(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

export function golferNameMatchParts(name) {
  const disp = displayGolferName(name);
  const loose = normNameLoose(disp || name);
  const parts = loose.split(/\s+/).filter((t) => t.length >= 2);
  return { loose, parts, last: parts.length ? parts[parts.length - 1] : "" };
}

function firstNamesLikelySame(a, b) {
  if (!a || !b) return false;
  const la = String(a).toLowerCase();
  const lb = String(b).toLowerCase();
  if (la === lb) return true;
  if (la.length >= 2 && lb.length >= 2 && (la.startsWith(lb) || lb.startsWith(la))) return true;
  if (la.length >= 3 && lb.length >= 3 && la.slice(0, 3) === lb.slice(0, 3)) return true;
  return false;
}

/** True when two golfer labels likely refer to the same person. */
export function golferNamesLikelySame(nameA, nameB) {
  const a = String(nameA || "").trim();
  const b = String(nameB || "").trim();
  if (!a || !b) return false;
  if (a.toLowerCase() === b.toLowerCase()) return true;
  const ta = golferNameMatchParts(a);
  const tb = golferNameMatchParts(b);
  if (ta.loose && tb.loose && ta.loose === tb.loose) return true;
  if (!ta.last || !tb.last || ta.last !== tb.last) return false;
  if (ta.parts.length >= 2 && tb.parts.length >= 2) {
    const fa = ta.parts[0];
    const fb = tb.parts[0];
    if (firstNamesLikelySame(fa, fb)) return true;
  }
  const setB = new Set(tb.parts);
  const overlap = ta.parts.filter((t) => setB.has(t)).length;
  if (overlap >= 2) return true;
  if (overlap >= 1 && ta.parts.length <= 2 && tb.parts.length <= 2) {
    return firstNamesLikelySame(ta.parts[0], tb.parts[0]);
  }
  if (ta.loose.length >= 4 && tb.loose.length >= 4) {
    if (ta.loose.includes(tb.loose) || tb.loose.includes(ta.loose)) return true;
  }
  return false;
}

/**
 * Find projections player row for a sportsbook label (any round row with matching dg_id / name).
 * @param {object[]} players
 * @param {string} label
 */
export function matchPlayerByGolferLabel(players, label) {
  if (!Array.isArray(players) || !players.length) return null;
  const raw = String(label || "").trim();
  if (!raw) return null;
  const dkl = raw.toLowerCase();
  const dkn = normNameLoose(displayGolferName(raw));
  for (const p of players) {
    const pn = String(p.player_name || "").trim();
    if (!pn) continue;
    if (pn.toLowerCase() === dkl) return p;
    if (displayGolferName(pn).toLowerCase() === dkl) return p;
    if (normNameLoose(displayGolferName(pn)) === dkn) return p;
    if (normNameLoose(pn) === dkn) return p;
    if (golferNamesLikelySame(pn, raw)) return p;
  }
  return null;
}
