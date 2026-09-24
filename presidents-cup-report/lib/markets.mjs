import { simulateSinglesH2H } from "./cup-sim.mjs";
import { americanToProb, edgePct, evPerUnit, formatAmerican, formatPct, probToAmerican } from "./odds.mjs";

function normName(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z,\s]/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function nameTokens(s) {
  return normName(s)
    .replace(/,/g, " ")
    .split(/\s+/)
    .filter(Boolean);
}

export function namesMatch(a, b) {
  const na = normName(a);
  const nb = normName(b);
  if (!na || !nb) return false;
  if (na === nb) return true;
  const ta = nameTokens(a);
  const tb = nameTokens(b);
  if (ta.length >= 2 && tb.length >= 2) {
    const lastA = ta[0];
    const lastB = tb[0];
    const firstA = ta[ta.length - 1];
    const firstB = tb[tb.length - 1];
    if (lastA === lastB && firstA === firstB) return true;
    if (lastA === lastB && (na.includes(firstB) || nb.includes(firstA))) return true;
  }
  const la = na.split(",")[0]?.trim();
  const lb = nb.split(",")[0]?.trim();
  if (la && lb && (na.includes(lb) || nb.includes(la))) return true;
  return false;
}

export function buildModelMarkets(field, sim) {
  const { usa, intl, hole_pars, course_adj_stp } = field;
  const markets = [];

  const usaCupWin = sim.cupProb.usa_win + sim.cupProb.usa_retain_tie;
  markets.push({
    market: "Cup Winner",
    selection: "USA",
    model_prob: usaCupWin,
    model_american: probToAmerican(usaCupWin),
    detail: `${formatPct(sim.cupProb.usa_win)} outright · ${formatPct(sim.cupProb.usa_retain_tie)} retain on tie`,
  });
  markets.push({
    market: "Cup Winner",
    selection: "International",
    model_prob: sim.cupProb.int_win,
    model_american: probToAmerican(sim.cupProb.int_win),
  });
  markets.push({
    market: "Cup Result",
    selection: "Tie (USA retains)",
    model_prob: sim.cupProb.usa_retain_tie,
    model_american: probToAmerican(sim.cupProb.usa_retain_tie),
  });
  for (const [sel, prob] of [
    ["USA", usaCupWin],
    ["International", sim.cupProb.int_win],
    ["Tie", sim.cupProb.usa_retain_tie],
  ]) {
    markets.push({
      market: "Outright Winner",
      selection: sel,
      model_prob: prob,
      model_american: probToAmerican(prob),
    });
  }

  const sessionAlias = {
    fri_foursomes: "Foursomes Day 2 Winner",
    sat_foursomes: "Foursomes Day 3 Winner",
  };
  for (const [sid, sw] of Object.entries(sim.sessionWins)) {
    markets.push({
      market: `Session Winner — ${sid}`,
      selection: "USA",
      model_prob: sw.usa,
      model_american: probToAmerican(sw.usa),
    });
    markets.push({
      market: `Session Winner — ${sid}`,
      selection: "International",
      model_prob: sw.int,
      model_american: probToAmerican(sw.int),
    });
    if (sessionAlias[sid]) {
      for (const [sel, prob] of [
        ["USA", sw.usa],
        ["International", sw.int],
      ]) {
        markets.push({
          market: sessionAlias[sid],
          selection: sel,
          model_prob: prob,
          model_american: probToAmerican(prob),
        });
      }
    }
  }

  const friF = sim.sessionWins.fri_foursomes || { usa: 0, int: 0 };
  const satF = sim.sessionWins.sat_foursomes || { usa: 0, int: 0 };
  for (const [sel, prob] of [
    ["USA", (friF.usa + satF.usa) / 2],
    ["International", (friF.int + satF.int) / 2],
  ]) {
    markets.push({
      market: "Tournament Foursomes Winner",
      selection: sel,
      model_prob: prob,
      model_american: probToAmerican(prob),
    });
  }

  const scorerMarketNames = [
    "Top Cup Points Scorer",
    "Player to Win the Most Holes",
    "Top Points Scorer",
  ];
  const intPropMarkets = [
    "Top International Team Rookie",
    "Top International Qualifier",
    "Top International Team Captain's Pick",
    "Top International Points Scorer Top 3 Finish",
  ];

  for (const p of sim.topScorer) {
    for (const marketName of scorerMarketNames) {
      markets.push({
        market: marketName,
        selection: p.name,
        team: p.team,
        model_prob: p.top_scorer_prob,
        model_american: probToAmerican(p.top_scorer_prob),
        detail: `E[points] ${p.exp_points.toFixed(2)}`,
      });
    }
    if (p.team === "USA" && p.usa_top_scorer_prob > 0) {
      markets.push({
        market: "Team USA Top Points Scorer",
        selection: p.name,
        team: "USA",
        model_prob: p.usa_top_scorer_prob,
        model_american: probToAmerican(p.usa_top_scorer_prob),
        detail: `E[points] ${p.exp_points.toFixed(2)}`,
      });
    }
    if (p.team === "INT" && p.int_top_scorer_prob > 0) {
      for (const marketName of intPropMarkets) {
        markets.push({
          market: marketName,
          selection: p.name,
          team: "INT",
          model_prob: p.int_top_scorer_prob,
          model_american: probToAmerican(p.int_top_scorer_prob),
          detail: `E[points] ${p.exp_points.toFixed(2)}`,
        });
      }
    }
    for (const [line, prob] of [
      [2.5, p.prob_ge3_pts],
      [3.5, p.prob_ge4_pts],
    ]) {
      markets.push({
        market: `Cup Points O/U ${line}`,
        selection: `${p.name} Over ${line}`,
        model_prob: prob,
        model_american: probToAmerican(prob),
      });
      markets.push({
        market: `Cup Points O/U ${line}`,
        selection: `${p.name} Under ${line}`,
        model_prob: 1 - prob,
        model_american: probToAmerican(1 - prob),
      });
    }
  }

  const u1 = usa[0];
  const u2 = usa[1];
  const i1 = intl[0];
  const i2 = intl[1];
  if (u1 && i1) {
    const h2h = simulateSinglesH2H(u1, i1, hole_pars, course_adj_stp, 3000, 42);
    markets.push({
      market: "Singles H2H (model)",
      selection: `${u1.name} vs ${i1.name} — ${u1.name.split(",")[0]}`,
      model_prob: h2h.aWin + h2h.halve * 0.5,
      model_american: probToAmerican(h2h.aWin + h2h.halve * 0.5),
    });
    markets.push({
      market: "Singles H2H (model)",
      selection: `${u1.name} vs ${i1.name} — ${i1.name.split(",")[0]}`,
      model_prob: h2h.bWin + h2h.halve * 0.5,
      model_american: probToAmerican(h2h.bWin + h2h.halve * 0.5),
    });
  }
  if (u2 && i2) {
    const h2h = simulateSinglesH2H(u2, i2, hole_pars, course_adj_stp, 3000, 43);
    markets.push({
      market: "Singles H2H (model)",
      selection: `${u2.name} vs ${i2.name} — ${u2.name.split(",")[0]}`,
      model_prob: h2h.aWin + h2h.halve * 0.5,
      model_american: probToAmerican(h2h.aWin + h2h.halve * 0.5),
    });
  }

  for (const band of sim.scoreBands.slice(0, 8)) {
    markets.push({
      market: "Final Score Band",
      selection: band.score,
      model_prob: band.prob,
      model_american: probToAmerican(band.prob),
    });
  }

  return markets;
}

function isPointsOu(selection) {
  return /\bover\b|\bunder\b/i.test(String(selection || ""));
}

function sessionIdFromModelMarket(market) {
  const m = String(market || "");
  const hit = m.match(/session winner\s*[—-]\s*(\w+)/i);
  return hit?.[1] || null;
}

function marketFamily(market) {
  const m = String(market || "").toLowerCase();
  if (m.includes("session winner")) return "session_winner";
  if (m.includes("team usa top")) return "usa_top_scorer";
  if (m.includes("international") && (m.includes("rookie") || m.includes("qualifier") || m.includes("captain") || m.includes("top 3"))) {
    return "int_top_scorer";
  }
  if (m.includes("most holes")) return "top_scorer";
  if (m.includes("top") && (m.includes("point") || m.includes("scorer"))) return "top_scorer";
  if (m.includes("cup winner") || m.includes("tournament winner") || m.includes("outright winner")) return "cup_winner";
  if (m.includes("cup result") || m === "tie") return "cup_result";
  if (m.includes("o/u") || m.includes("over") || m.includes("under")) return "points_ou";
  if (m.includes("h2h") || m.includes("matchup")) return "h2h";
  return "other";
}

function bookProbFromDk(book) {
  if (!book) return null;
  if (book.dk_implied_prob != null && Number.isFinite(book.dk_implied_prob)) return book.dk_implied_prob;
  if (book.american != null) return americanToProb(book.american);
  return null;
}

function inMarketId(p, fam, dkMeta, modelMarket = null) {
  if (!dkMeta) return true;
  const id = String(p.marketId || "");
  if (fam === "cup_winner" || fam === "cup_result") {
    return !dkMeta.cup_market_id || id === String(dkMeta.cup_market_id);
  }
  if (fam === "top_scorer") {
    const ids = [dkMeta.scorer_market_id, dkMeta.alt_scorer_market_id].filter(Boolean).map(String);
    return !ids.length || ids.includes(id);
  }
  if (fam === "usa_top_scorer") {
    return !dkMeta.usa_scorer_market_id || id === String(dkMeta.usa_scorer_market_id);
  }
  if (fam === "session_winner" && modelMarket) {
    const sid = sessionIdFromModelMarket(modelMarket.market);
    const target = sid && dkMeta.session_market_ids?.[sid];
    return Boolean(target) && id === String(target);
  }
  return true;
}

function findBookLine(modelMarket, dkProps, used, dkMeta) {
  const fam = marketFamily(modelMarket.market);
  const sel = modelMarket.selection;

  for (let i = 0; i < dkProps.length; i++) {
    const p = dkProps[i];
    if (used.has(i)) continue;
    if (!inMarketId(p, fam, dkMeta, modelMarket)) continue;

    if (fam === "top_scorer" || fam === "usa_top_scorer" || fam === "int_top_scorer") {
      if (isPointsOu(sel) || isPointsOu(p.selection)) continue;
      if (namesMatch(p.selection, sel)) {
        used.add(i);
        return p;
      }
    } else if (fam === "cup_winner") {
      if (namesMatch(p.selection, sel) || (sel === "USA" && /^usa$/i.test(String(p.selection || "").trim()))) {
        used.add(i);
        return p;
      }
      if (sel === "International" && /international|rest of the world/i.test(String(p.selection || ""))) {
        used.add(i);
        return p;
      }
    } else if (fam === "cup_result") {
      if (/^tie$/i.test(String(p.selection || "").trim()) && /^tie/i.test(sel)) {
        used.add(i);
        return p;
      }
    } else if (fam === "session_winner") {
      if (namesMatch(p.selection, sel) || (sel === "USA" && /^usa$/i.test(String(p.selection || "").trim()))) {
        used.add(i);
        return p;
      }
      if (sel === "International" && /international|rest of the world/i.test(String(p.selection || ""))) {
        used.add(i);
        return p;
      }
    } else if (fam === "points_ou") {
      if (!isPointsOu(sel) || !isPointsOu(p.selection)) continue;
      if (namesMatch(p.selection, sel)) {
        used.add(i);
        return p;
      }
    } else if (fam === "h2h") {
      if (namesMatch(p.selection, sel.split("—").pop()?.trim() || sel)) {
        used.add(i);
        return p;
      }
    }
  }
  return null;
}

export function attachBookLines(modelMarkets, dkProps, dkMeta = null) {
  const used = new Set();
  return modelMarkets.map((m) => {
    const book = findBookLine(m, dkProps, used, dkMeta);
    const bookAmerican = book?.american ?? null;
    const bookProb = bookProbFromDk(book);
    return {
      ...m,
      book_american: bookAmerican,
      book_american_display: book?.american_display ?? null,
      book_prob: bookProb,
      edge_pct: bookProb != null ? (m.model_prob - bookProb) * 100 : null,
      ev_per_unit: bookAmerican != null ? evPerUnit(m.model_prob, bookAmerican) : null,
      dk_market: book?.market || null,
      dk_market_id: book?.marketId ?? null,
    };
  });
}

export function matchDkOnlyProps(dkProps, modelMarkets, attached = [], dkMeta = null) {
  const matchedDk = new Set(
    attached
      .filter((m) => m.book_american != null)
      .map((m) => `${m.dk_market_id || ""}|${normName(m.selection)}`),
  );
  const extra = [];
  for (const p of dkProps) {
    const pk = `${p.marketId || ""}|${normName(p.selection)}`;
    if (matchedDk.has(pk)) continue;
    const fam = marketFamily(p.market);
    const hit = modelMarkets.find((m) => {
      if (marketFamily(m.market) !== fam && !(fam === "top_scorer" && marketFamily(m.market) === "top_scorer"))
        return false;
      if (fam === "cup_winner") {
        return (
          namesMatch(p.selection, m.selection) ||
          (m.selection === "USA" && /^usa$/i.test(p.selection)) ||
          (m.selection === "International" && /international/i.test(p.selection))
        );
      }
      if (fam === "cup_result") return /^tie$/i.test(p.selection) && /^tie/i.test(m.selection);
      return namesMatch(p.selection, m.selection);
    });
    if (hit && attached.some((a) => a.book_american != null && namesMatch(a.selection, hit.selection)))
      continue;
    const bp = bookProbFromDk(p);
    extra.push({
      market: p.market,
      selection: p.selection,
      book_american: p.american,
      book_american_display: p.american_display ?? null,
      book_prob: bp,
      model_prob: hit?.model_prob ?? null,
      model_american: hit?.model_american ?? null,
      edge_pct: hit && bp != null ? (hit.model_prob - bp) * 100 : null,
      ev_per_unit: hit && p.american != null ? evPerUnit(hit.model_prob, p.american) : null,
      dk_market_id: p.marketId ?? null,
      note: hit ? null : "No model match — DK only",
    });
  }
  return extra;
}
