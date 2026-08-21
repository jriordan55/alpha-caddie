export const GOLF_PROP_MARKETS = new Set([
  "Total score",
  "Total Score",
  "Birdies",
  "Bogeys",
  "GIR",
  "Fairways hit",
  "Pars",
  "Putts",
]);

export const PAPER_BOOKS = Object.freeze([
  {
    id: "draftkings",
    label: "DraftKings",
    short: "DK",
    source: "draftkings",
    mode: "sportsbook",
    minPicks: 1,
    maxPicks: 12,
    wholeLine: false,
    overLabel: "Over",
    underLabel: "Under",
  },
  {
    id: "prizepicks",
    label: "PrizePicks",
    short: "PP",
    source: "prizepicks",
    mode: "pickem",
    minPicks: 2,
    maxPicks: 6,
    wholeLine: true,
    overLabel: "More",
    underLabel: "Less",
  },
  {
    id: "sleeper",
    label: "Sleeper",
    short: "SL",
    source: "sleeper",
    mode: "pickem",
    minPicks: 2,
    maxPicks: 8,
    wholeLine: true,
    overLabel: "Over",
    underLabel: "Under",
  },
  {
    id: "underdog",
    label: "Underdog",
    short: "UD",
    source: "underdog",
    mode: "pickem",
    minPicks: 2,
    maxPicks: 6,
    wholeLine: true,
    overLabel: "Higher",
    underLabel: "Lower",
  },
]);

export function bookById(id) {
  return PAPER_BOOKS.find((b) => b.id === id) || PAPER_BOOKS[0];
}

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function liveTargetRound(projections) {
  const meta = projections?.meta || {};
  const dr = Math.round(num(meta.display_round ?? projections.display_round, NaN));
  const live = Math.round(num(meta.datagolf_field_current_round, NaN));

  const propRoundCounts = new Map();
  for (const r of Array.isArray(projections?.props) ? projections.props : []) {
    let rnd = Math.round(num(r.round_num ?? r.display_round, NaN));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) {
      rnd = Math.round(num(meta.display_round ?? projections.display_round, 1)) || 1;
    }
    propRoundCounts.set(rnd, (propRoundCounts.get(rnd) || 0) + 1);
  }
  const propRound =
    propRoundCounts.size > 0
      ? [...propRoundCounts.entries()].sort((a, b) => b[1] - a[1] || b[0] - a[0])[0][0]
      : NaN;

  if (Number.isFinite(propRound)) {
    if (Number.isFinite(dr) && dr >= 1 && dr <= 4 && propRoundCounts.has(dr)) return dr;
    return propRound;
  }
  if (Number.isFinite(dr) && dr >= 1 && dr <= 4) return dr;
  if (Number.isFinite(live) && live >= 1 && live <= 4) return live;
  return 1;
}

export function playersForRound(projections, round) {
  const byDg = new Map();
  for (const p of Array.isArray(projections?.players) ? projections.players : []) {
    const pr = Math.round(num(p.round, NaN));
    if (pr !== round) continue;
    const dg = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    byDg.set(dg, p);
  }
  return byDg;
}

export function formatLine(line, wholeLine) {
  const n = num(line, NaN);
  if (!Number.isFinite(n)) return "—";
  return wholeLine && Math.abs(n - Math.round(n)) < 0.001 ? String(Math.round(n)) : String(n);
}

export function marketShortLabel(market) {
  const m = String(market || "");
  if (m === "Total score") return "Round score";
  if (m === "Fairways hit") return "Fairways";
  return m;
}
