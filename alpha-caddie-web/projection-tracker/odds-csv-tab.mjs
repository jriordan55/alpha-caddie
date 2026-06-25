/**
 * odds.csv explorer tab — filter graded O/U lines by course, player, market.
 */
import { formatAmerican, num } from "./ev-math.mjs";

export function normCourseKey(raw) {
  return String(raw || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

export function filterOddsLines(rows, filters) {
  const courseKey = normCourseKey(filters.course);
  const playerQ = String(filters.player || "")
    .trim()
    .toLowerCase();
  const market = String(filters.market || "").trim();
  const side = String(filters.side || "").trim().toLowerCase();
  const minEv = num(filters.minEv, 0);
  const modelOnly = filters.modelOnly === true;

  return rows.filter((r) => {
    if (market && r.market !== market) return false;
    if (side && String(r.side).toLowerCase() !== side) return false;
    if (courseKey) {
      const ck = normCourseKey(r.course_name);
      if (!ck.includes(courseKey) && !courseKey.includes(ck)) return false;
    }
    if (playerQ) {
      const name = String(r.matched_player || r.player || "").toLowerCase();
      if (!name.includes(playerQ)) return false;
    }
    if (minEv > 0) {
      const edge = num(r.model_edge_pct, NaN);
      if (!Number.isFinite(edge) || edge < minEv) return false;
    }
    if (modelOnly) {
      const mu = num(r.model_mu, NaN);
      const line = num(r.line, NaN);
      const edge = num(r.model_edge_pct, NaN);
      const s = String(r.side).toLowerCase();
      if (s === "over" && !(mu > line && edge >= minEv)) return false;
      if (s === "under" && !(mu < line && edge >= minEv)) return false;
    }
    return true;
  });
}

export function summarizeOddsLines(rows, { oddsAt = "close" } = {}) {
  const pnlKey = oddsAt === "open" ? "pnl_open" : "pnl_close";
  let units = 0;
  let wins = 0;
  let losses = 0;
  let pushes = 0;
  let bets = 0;
  for (const r of rows) {
    const res = String(r.result || "").toUpperCase();
    if (res !== "W" && res !== "L" && res !== "P") continue;
    bets++;
    const pnl = num(r[pnlKey], 0);
    units += pnl;
    if (res === "W") wins++;
    else if (res === "L") losses++;
    else pushes++;
  }
  const graded = wins + losses;
  return {
    bets,
    wins,
    losses,
    pushes,
    units,
    hit_pct: graded > 0 ? (wins / graded) * 100 : NaN,
    roi_pct: bets > 0 ? (units / bets) * 100 : NaN,
  };
}

export function uniqueCourses(rows) {
  const m = new Map();
  for (const r of rows) {
    const c = String(r.course_name || "").trim();
    if (!c) continue;
    m.set(normCourseKey(c), c);
  }
  return [...m.values()].sort((a, b) => a.localeCompare(b));
}

export function summarizeByCourse(rows, opts) {
  const by = new Map();
  for (const r of rows) {
    const c = String(r.course_name || "(unknown)").trim() || "(unknown)";
    if (!by.has(c)) by.set(c, []);
    by.get(c).push(r);
  }
  return [...by.entries()]
    .map(([course, rs]) => ({ course, ...summarizeOddsLines(rs, opts) }))
    .filter((x) => x.bets > 0)
    .sort((a, b) => b.roi_pct - a.roi_pct);
}

export function summarizeByMarket(rows, opts) {
  const order = { Birdies: 0, "Total score": 1 };
  const by = new Map();
  for (const r of rows) {
    const m = String(r.market || "").trim();
    if (!m) continue;
    if (!by.has(m)) by.set(m, []);
    by.get(m).push(r);
  }
  return [...by.entries()]
    .map(([market, rs]) => ({ market, ...summarizeOddsLines(rs, opts) }))
    .filter((x) => x.bets > 0)
    .sort((a, b) => (order[a.market] ?? 9) - (order[b.market] ?? 9));
}

export function summarizeByPlayer(rows, opts) {
  const by = new Map();
  for (const r of rows) {
    const p = String(r.matched_player || r.player || "").trim();
    if (!p) continue;
    if (!by.has(p)) by.set(p, []);
    by.get(p).push(r);
  }
  return [...by.entries()]
    .map(([player, rs]) => ({ player, ...summarizeOddsLines(rs, opts) }))
    .filter((x) => x.bets > 0)
    .sort((a, b) => b.bets - a.bets);
}

export function fmtPct(v) {
  if (!Number.isFinite(v)) return "—";
  return `${v >= 0 ? "+" : ""}${v.toFixed(1)}%`;
}

export function fmtNum(v, d = 2) {
  if (!Number.isFinite(v)) return "—";
  return v.toFixed(d);
}
