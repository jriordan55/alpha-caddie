import { SESSIONS } from "./data.mjs";
import {
  displayName,
  badgeUrl,
  headshotForSelection,
  playerHeadshotUrl,
  playerKey,
  eventLogoUrl,
} from "./branding.mjs";
import { DK_TABS } from "./dk-tabs.mjs";
import { buildPricingContext, attachModelToRow } from "./derive-pricing.mjs";

const SESSION_LABELS = Object.fromEntries(SESSIONS.map((s) => [s.id, s.label]));

function teamFromMarket(market) {
  const m = String(market || "").toLowerCase();
  if (/total usa|team usa\b|usa total|usa points/i.test(m)) return "USA";
  if (/total international|team int|international total|international points/i.test(m)) return "INT";
  return null;
}

function playerLabelFromMarket(market) {
  const m = String(market || "").match(/^(.+?)\s+Total Points$/i);
  return m ? m[1].trim() : null;
}

function enrichRow(row, manifest) {
  const marketPlayer = playerLabelFromMarket(row.market);
  const name = marketPlayer || row.selection;
  const isTeam = /^(USA|International|Tie)/i.test(row.selection);
  const team =
    row.team ||
    teamFromMarket(row.market) ||
    (name === "USA" ? "USA" : name === "International" ? "INT" : null);
  const display = isTeam
    ? name
    : marketPlayer
      ? `${displayName(marketPlayer)} · ${row.selection} pts`
      : displayName(name);
  return {
    ...row,
    display,
    headshot: isTeam ? null : headshotForSelection(marketPlayer || name, row.market, manifest),
    team,
    model_pct: row.model_prob != null ? row.model_prob * 100 : null,
    dk_pct: row.book_prob != null ? row.book_prob * 100 : null,
    model_odds: row.model_american,
    dk_odds: row.book_american,
    dk_odds_display: row.book_american_display ?? row.book_american,
    dk_market_id: row.dk_market_id ?? null,
    market_group: row.market_group || row.dk_market || row.market || null,
  };
}

function compareByEdge(a, b) {
  const ea = a.edge_pct;
  const eb = b.edge_pct;
  if (ea != null && eb != null && eb !== ea) return eb - ea;
  if (ea != null && eb == null) return -1;
  if (ea == null && eb != null) return 1;
  return (b.model_prob ?? b.book_prob ?? 0) - (a.model_prob ?? a.book_prob ?? 0);
}

function sortByEdge(rows) {
  return [...rows].sort(compareByEdge);
}

function sortRows(rows) {
  return [...rows].sort((a, b) => {
    const g = String(a.market_group || "").localeCompare(String(b.market_group || ""));
    if (g !== 0) return g;
    return compareByEdge(a, b);
  });
}

function isPlayerEventMarket(groupName) {
  return / to win a point$/i.test(String(groupName || ""));
}

function sortEventPropRows(rows) {
  const byGroup = new Map();
  for (const r of rows) {
    const g = r.market_group || r.market;
    if (!byGroup.has(g)) byGroup.set(g, []);
    byGroup.get(g).push(r);
  }

  const teamGroups = [];
  const playerGroups = [];
  for (const [g, grp] of byGroup) {
    (isPlayerEventMarket(g) ? playerGroups : teamGroups).push({ g, grp });
  }
  teamGroups.sort((a, b) => a.g.localeCompare(b.g));
  playerGroups.sort((a, b) => a.g.localeCompare(b.g));

  return [...teamGroups, ...playerGroups].flatMap(({ grp }) => sortByEdge(grp));
}

function buildDkTabs(dkProps, ctx, manifest) {
  const tabs = [];

  for (const tab of DK_TABS) {
    const tabProps = dkProps.filter((p) => {
      if (p.dk_subcategory !== tab.id) return false;
      if (tab.id === "outrights") {
        if (/^tie$/i.test(String(p.selection || "").trim())) return false;
        if (/lift the trophy/i.test(String(p.market || ""))) return false;
      }
      return true;
    });
    if (!tabProps.length) continue;

    const byMarket = new Map();
    for (const p of tabProps) {
      const key = `${p.marketId || ""}|${p.market}`;
      if (!byMarket.has(key)) byMarket.set(key, []);
      byMarket.get(key).push(p);
    }

    const rows = [];
    for (const group of byMarket.values()) {
      const poolSelections = group.map((g) => g.selection);
      for (const p of group) {
        rows.push(enrichRow(attachModelToRow(p, poolSelections, ctx), manifest));
      }
    }

    const sorted = tab.id === "event-props" ? sortEventPropRows(rows) : sortRows(rows);
    const marketGroups = [...new Set(sorted.map((r) => r.market_group))];
    tabs.push({
      id: tab.id,
      label: tab.label,
      rows: sorted,
      market_count: marketGroups.length,
      dk_line_count: sorted.length,
    });
  }

  return tabs;
}

export function buildDashboardPayload({ field, sim, dkMeta, manifest, nSims }) {
  const usaRetain = sim.cupProb.usa_win + sim.cupProb.usa_retain_tie;
  const dkProps = dkMeta.props || [];
  const ctx = buildPricingContext(sim, field);
  const tabs = buildDkTabs(dkProps, ctx, manifest);

  const priced = tabs.reduce((n, t) => n + t.rows.filter((r) => r.model_prob != null).length, 0);

  return {
    generated_at: new Date().toISOString(),
    event: field.event_name,
    course: field.course,
    dates: "September 22–27, 2026",
    n_sims: nSims,
    cup: {
      usa_win: sim.cupProb.usa_win * 100,
      usa_retain: sim.cupProb.usa_retain_tie * 100,
      usa_implied: usaRetain * 100,
      int_win: sim.cupProb.int_win * 100,
      tie: sim.cupProb.usa_retain_tie * 100,
      usa_skill: sim.teamAbility.usa,
      int_skill: sim.teamAbility.int,
    },
    logos: {
      event: eventLogoUrl(manifest),
    },
    badges: {
      usa: badgeUrl("USA", manifest),
      int: badgeUrl("INT", manifest),
    },
    teams: {
      usa: field.usa.map((p) => ({
        name: displayName(p.name),
        key: playerKey(p.name),
        mu_sg: p.mu_sg,
        headshot: playerHeadshotUrl(p.name, manifest),
      })),
      int: field.intl.map((p) => ({
        name: displayName(p.name),
        key: playerKey(p.name),
        mu_sg: p.mu_sg,
        headshot: playerHeadshotUrl(p.name, manifest),
      })),
    },
    top_scorers: sim.topScorer.slice(0, 12).map((p) => ({
      name: displayName(p.name),
      team: p.team,
      exp_points: p.exp_points,
      top_scorer_prob: p.top_scorer_prob * 100,
      headshot: playerHeadshotUrl(p.name, manifest),
    })),
    score_bands: sim.scoreBands.slice(0, 8),
    session_wins: Object.entries(sim.sessionWins).map(([id, sw]) => ({
      id,
      label: SESSION_LABELS[id] || id,
      usa: sw.usa * 100,
      int: sw.int * 100,
      halve: sw.halve * 100,
    })),
    tabs,
    dk: {
      url: dkMeta.url,
      count: dkProps.length,
      raw_count: dkMeta.props_raw_count || dkProps.length,
      matched: priced,
      market_ids: dkMeta.market_ids || null,
      note: dkMeta.note,
    },
  };
}
