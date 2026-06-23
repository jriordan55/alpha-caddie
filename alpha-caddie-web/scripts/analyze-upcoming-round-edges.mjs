#!/usr/bin/env node
/**
 * Model vs book edges for the upcoming round (projections.json + data/odds.csv).
 *
 *   node scripts/analyze-upcoming-round-edges.mjs
 *   node scripts/analyze-upcoming-round-edges.mjs --min-ev 5
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { displayGolferName, golferNamesLikelySame, normNameLoose } from "./golfer-name-match.mjs";
import {
  birdiesPlusEaglesFromRow,
  createProjectionContext,
  modelEdgePctAtLine,
  ouProjectedMeanForMode,
} from "./round-projection-mu.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const PROJ = join(WEB, "projections.json");
const ODDS = join(REPO, "data", "odds.csv");
const OUT = join(WEB, "data", "upcoming_round_edge_scan.json");

const OU_TYPES = new Set([
  "GOLF:FT:CTBIR",
  "GOLF:FT:ROUNDNUMBIRDIES",
  "GOLF:FT:CTSTR",
  "GOLF:P:ROUND1OUSCORE",
]);

const MARKET_LABEL = {
  "GOLF:FT:CTBIR": "Birdies",
  "GOLF:FT:ROUNDNUMBIRDIES": "Birdies",
  "GOLF:FT:CTSTR": "Total score",
  "GOLF:P:ROUND1OUSCORE": "Total score",
};

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function parseArgs() {
  const args = process.argv.slice(2);
  let minEv = 5;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--min-ev") minEv = num(args[++i], 5);
  }
  return { minEv };
}

function parseRoundFromText(...parts) {
  for (const p of parts) {
    const m = String(p || "").match(/Round\s*(\d)/i);
    if (m) return Number(m[1]);
  }
  return NaN;
}

function parseOuPlayer(marketName) {
  return String(marketName || "")
    .trim()
    .replace(/\s*Total Birdies or Better.*$/i, "")
    .replace(/\s*Total Birdies.*$/i, "")
    .replace(/\s*Round Score.*$/i, "")
    .replace(/\s*-\s*Round\s*\d+\s*Score.*$/i, "")
    .replace(/\s*-\s*Round\s*\d+\s*Total Birdies.*$/i, "")
    .trim();
}

function parseSelection(sel) {
  const m = String(sel || "").trim().match(/^(Over|Under)\s+([\d.]+)$/i);
  if (!m) return null;
  return { side: m[1].toLowerCase(), line: num(m[2]) };
}

function parseCompetition(comp, startUtc) {
  const raw = String(comp || "").trim();
  const ym = raw.match(/\s+(20\d{2})\s*$/);
  if (ym) return { event: raw.replace(/\s+20\d{2}\s*$/, "").trim(), year: Number(ym[1]) };
  const y = new Date(String(startUtc || "")).getUTCFullYear();
  return { event: raw, year: Number.isFinite(y) ? y : NaN };
}

function eventYearFromPayload(payload) {
  const ds = String(payload.datagolf_field_date_start || "").slice(0, 4);
  const y = num(ds, NaN);
  if (Number.isFinite(y)) return y;
  return new Date(String(payload.updated_at || "")).getUTCFullYear();
}

function parseOddsForEvent(oddsRows, eventName, targetYear, targetRound) {
  /** @type {Map<string, object>} */
  const byKey = new Map();
  for (const row of oddsRows) {
    const mt = String(row.MARKET_TYPE || "").trim();
    if (!OU_TYPES.has(mt)) continue;
    const { event, year } = parseCompetition(row.COMPETITION, row.EVENT_START_TIME_UTC);
    if (!eventsLikelySame(eventName, event)) continue;
    if (Number.isFinite(targetYear) && Number.isFinite(year) && year !== targetYear) continue;
    const rnd =
      parseRoundFromText(row.SPORT_EVENT, row.MARKET_NAME) ||
      (mt === "GOLF:P:ROUND1OUSCORE" || mt === "GOLF:FT:ROUNDNUMBIRDIES" ? 1 : NaN);
    if (Number.isFinite(targetRound) && rnd !== targetRound) continue;
    const sel = parseSelection(row.SELECTION);
    if (!sel || !Number.isFinite(sel.line)) continue;
    const market = MARKET_LABEL[mt] || mt;
    const player = parseOuPlayer(row.MARKET_NAME);
    const key = `${player}|${market}|${sel.line}`;
    let p = byKey.get(key);
    if (!p) {
      p = {
        player,
        market,
        line: sel.line,
        round: rnd,
        competition: row.COMPETITION,
        over_am: NaN,
        under_am: NaN,
      };
      byKey.set(key, p);
    }
    const close = num(row.CLOSING_AMERICAN_ODDS, NaN);
    if (sel.side === "over") p.over_am = close;
    else p.under_am = close;
  }
  return [...byKey.values()].filter((p) => Number.isFinite(p.over_am) && Number.isFinite(p.under_am));
}

function findPlayerRow(players, oddsLabel) {
  for (const p of players) {
    const pn = displayGolferName(p.player_name);
    if (golferNamesLikelySame(oddsLabel, pn) || golferNamesLikelySame(oddsLabel, p.player_name)) return p;
  }
  const s = String(oddsLabel || "").trim();
  const m = s.match(/^([A-Za-z])\.?\s+(.+)$/);
  if (m) {
    const init = m[1].toLowerCase();
    const last = normNameLoose(m[2]);
    for (const p of players) {
      const parts = normNameLoose(displayGolferName(p.player_name)).split(/\s+/);
      if (parts.length >= 2 && parts[parts.length - 1] === last && parts[0].startsWith(init)) return p;
    }
  }
  return null;
}

function pickSide(edgeOver, edgeUnder, minEv) {
  if (!Number.isFinite(edgeOver) || !Number.isFinite(edgeUnder)) return null;
  if (edgeOver >= minEv && edgeOver >= edgeUnder) return { side: "over", edge: edgeOver };
  if (edgeUnder >= minEv && edgeUnder > edgeOver) return { side: "under", edge: edgeUnder };
  return null;
}

function median(vals) {
  const v = vals.filter(Number.isFinite).sort((a, b) => a - b);
  if (!v.length) return NaN;
  const mid = Math.floor(v.length / 2);
  return v.length % 2 ? v[mid] : (v[mid - 1] + v[mid]) / 2;
}

function buildModelRows(payload, ctx) {
  const rnd = Math.round(num(payload.display_round, 1)) || 1;
  const rows = [];
  for (const p of payload.players || []) {
    if (Math.round(num(p.round)) !== rnd) continue;
    const scoreMu = ouProjectedMeanForMode("Total score", p, payload, "default", "default", ctx);
    const birdMu = ouProjectedMeanForMode("Birdies", p, payload, "default", "default", ctx);
    rows.push({
      dg_id: p.dg_id,
      player_name: displayGolferName(p.player_name),
      score_mu: scoreMu,
      bird_mu: birdMu,
      sg_total: num(p.sg_total, NaN),
      sg_ott: num(p.sg_ott, NaN),
      sg_app: num(p.sg_app, NaN),
    });
  }
  return rows;
}

function analyzeBookLines(props, payload, ctx, minEv) {
  const bets = [];
  const unmatched = [];
  for (const prop of props) {
    const p = findPlayerRow(payload.players, prop.player);
    if (!p) {
      unmatched.push(prop.player);
      continue;
    }
    const mu =
      prop.market === "Total score"
        ? ouProjectedMeanForMode("Total score", p, payload, "default", "default", ctx)
        : ouProjectedMeanForMode("Birdies", p, payload, "default", "default", ctx);
    if (!Number.isFinite(mu)) continue;
    const { edgeOver, edgeUnder } = modelEdgePctAtLine(
      prop.market,
      mu,
      prop.line,
      p,
      payload,
      prop.over_am,
      prop.under_am,
    );
    const pick = pickSide(edgeOver, edgeUnder, minEv);
    if (!pick) continue;
    bets.push({
      player: displayGolferName(p.player_name),
      market: prop.market,
      side: pick.side,
      line: prop.line,
      model: Math.round(mu * 100) / 100,
      delta: Math.round((mu - prop.line) * 100) / 100,
      edge_pct: Math.round(pick.edge * 10) / 10,
      odds: pick.side === "over" ? prop.over_am : prop.under_am,
      fair_edge_over: Math.round(edgeOver * 10) / 10,
      fair_edge_under: Math.round(edgeUnder * 10) / 10,
      competition: prop.competition,
    });
  }
  bets.sort((a, b) => b.edge_pct - a.edge_pct);
  return { bets, unmatched: [...new Set(unmatched)] };
}

function proxyFieldLines(modelRows) {
  return {
    score: median(modelRows.map((r) => r.score_mu)),
    bird: median(modelRows.map((r) => r.bird_mu)),
  };
}

function proxyEdges(modelRows, proxy, minEv) {
  const bets = [];
  for (const r of modelRows) {
    for (const spec of [
      { market: "Total score", mu: r.score_mu, line: proxy.score },
      { market: "Birdies", mu: r.bird_mu, line: proxy.bird },
    ]) {
      if (!Number.isFinite(spec.mu) || !Number.isFinite(spec.line)) continue;
      const { edgeOver, edgeUnder } = modelEdgePctAtLine(spec.market, spec.mu, spec.line, {}, {}, -110, -110);
      const pick = pickSide(edgeOver, edgeUnder, minEv);
      if (!pick) continue;
      bets.push({
        player: r.player_name,
        market: spec.market,
        side: pick.side,
        line: Math.round(spec.line * 100) / 100,
        model: Math.round(spec.mu * 100) / 100,
        delta: Math.round((spec.mu - spec.line) * 100) / 100,
        edge_pct: Math.round(pick.edge * 10) / 10,
        odds: -110,
        note: "proxy vs field-median @ -110 (no book lines in odds.csv)",
      });
    }
  }
  bets.sort((a, b) => b.edge_pct - a.edge_pct);
  return bets;
}

function main() {
  const { minEv } = parseArgs();
  if (!existsSync(PROJ)) throw new Error(`Missing ${PROJ}`);
  const payload = JSON.parse(readFileSync(PROJ, "utf8"));
  payload._webRoot = WEB;
  const ctx = createProjectionContext(payload);
  const eventName = String(payload.event_name || "").trim();
  const course = String(payload.course_used || "").trim();
  const rnd = Math.round(num(payload.display_round, 1)) || 1;
  const year = eventYearFromPayload(payload);

  const oddsRows = existsSync(ODDS)
    ? parse(readFileSync(ODDS, "utf8"), { columns: true, relax_quotes: true, skip_empty_lines: true })
    : [];
  const props = parseOddsForEvent(oddsRows, eventName, year, rnd);
  const modelRows = buildModelRows(payload, ctx);
  const { bets, unmatched } = analyzeBookLines(props, payload, ctx, minEv);
  const proxy = proxyFieldLines(modelRows);
  const proxyBets = props.length ? [] : proxyEdges(modelRows, proxy, minEv);

  const payloadOut = {
    generated_at: new Date().toISOString(),
    event: eventName,
    course,
    round: rnd,
    year,
    min_ev_pct: minEv,
    odds_csv: ODDS,
    book_lines_matched: props.length,
    qualified_bets: bets.length,
    unmatched_odds_players: unmatched.slice(0, 20),
    field_median_proxy: proxy,
    bets,
    proxy_bets_when_no_lines: proxyBets.slice(0, 40),
    model_leaders: {
      lowest_scores: [...modelRows].sort((a, b) => a.score_mu - b.score_mu).slice(0, 15),
      highest_birdies: [...modelRows].sort((a, b) => b.bird_mu - a.bird_mu).slice(0, 15),
    },
  };
  writeFileSync(OUT, JSON.stringify(payloadOut, null, 2));

  console.log(`\n=== ${eventName} R${rnd} @ ${course} ===`);
  console.log(`Year ${year} · min EV ${minEv}% · odds.csv lines matched: ${props.length}`);
  if (!props.length) {
    console.log(`\nNo ${eventName} ${year} R${rnd} score/birdie lines in odds.csv yet.`);
    console.log(`Field median proxy: score ${proxy.score?.toFixed(2)}, birdies ${proxy.bird?.toFixed(2)}`);
    console.log(`\n--- Model leaders (projection) ---`);
    console.log("Lowest R1 scores:");
    for (const r of payloadOut.model_leaders.lowest_scores.slice(0, 10)) {
      console.log(`  ${r.player_name.padEnd(22)} ${r.score_mu.toFixed(2)}  (bird ${r.bird_mu.toFixed(2)})`);
    }
    console.log("\nHighest R1 birdies:");
    for (const r of payloadOut.model_leaders.highest_birdies.slice(0, 10)) {
      console.log(`  ${r.player_name.padEnd(22)} ${r.bird_mu.toFixed(2)}  (score ${r.score_mu.toFixed(2)})`);
    }
    if (proxyBets.length) {
      console.log(`\n--- Proxy value vs field median @ -110 (until real lines land) ---`);
      for (const b of proxyBets.slice(0, 15)) {
        console.log(
          `  ${b.player.padEnd(22)} ${b.market} ${b.side.toUpperCase()} ${b.line}  model ${b.model}  Δ${b.delta >= 0 ? "+" : ""}${b.delta}  EV ${b.edge_pct}%`,
        );
      }
    }
  } else {
    console.log(`\n--- Best bets (≥${minEv}% EV vs closing odds) ---`);
    for (const b of bets.slice(0, 25)) {
      console.log(
        `  ${b.player.padEnd(22)} ${b.market} ${b.side.toUpperCase()} ${b.line} @ ${b.odds > 0 ? "+" : ""}${b.odds}  model ${b.model}  Δ${b.delta >= 0 ? "+" : ""}${b.delta}  EV ${b.edge_pct}%`,
      );
    }
    if (unmatched.length) {
      console.log(`\nUnmatched odds players (${unmatched.length}): ${unmatched.slice(0, 8).join(", ")}`);
    }
  }
  console.log(`\nFull output: ${OUT}\n`);
}

main();
