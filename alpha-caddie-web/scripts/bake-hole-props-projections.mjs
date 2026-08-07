/**
 * Bake hole-prop projections vs DraftKings / Underdog lines.
 *
 *   node scripts/bake-hole-props-projections.mjs
 *   → data/live_hole_props.json (+ projections.hole_props pointer)
 *
 * Reads optional odds packs:
 *   data/dk_hole_props.json
 *   data/ud_hole_props.json
 * Always builds model board from player_course_hole_sg even when odds are empty.
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import {
  HOLE_PROP_MARKETS,
  americanToEv,
  buildPlayerHoleBoard,
  holeScorePmf,
  holeWinnerProbs,
  loadHolePropsTables,
  ouProbsFromPmf,
  ouProbsNormal,
  pickOuSide,
  projectHoleMean,
  projectHoleSum,
  resolveCourseKey,
  holeParsFromPayload,
} from "./hole-props-model.mjs";
import { impliedProbFromAmerican } from "./round-projection-mu.mjs";
import { fetchDraftKingsHoleProps } from "./fetch-dk-hole-props.mjs";
import { fetchUnderdogHoleProps } from "./fetch-ud-hole-props.mjs";
import { inferDraftKingsLeagueUrlFromProjections } from "./draftkings-league-url.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PROJ = join(WEB, "projections.json");
const OUT = join(WEB, "data", "live_hole_props.json");
const DK_ODDS = join(WEB, "data", "dk_hole_props.json");
const UD_ODDS = join(WEB, "data", "ud_hole_props.json");

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function readJson(path) {
  if (!existsSync(path)) return null;
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return null;
  }
}

function roundStp(player, coursePar) {
  const stp = num(player?.score_to_par, NaN);
  if (Number.isFinite(stp)) return stp;
  const total = num(player?.total_score, NaN);
  if (Number.isFinite(total) && Number.isFinite(coursePar)) return total - coursePar;
  return 0;
}

function playerByDg(payload, round) {
  /** @type {Map<number, object>} */
  const m = new Map();
  for (const p of payload?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const r = Math.round(num(p.round, NaN));
    if (Number.isFinite(r) && Number.isFinite(round) && r !== round) continue;
    m.set(dg, p);
  }
  return m;
}

/**
 * @param {object} payload
 * @param {object[]} dkProps
 * @param {object[]} udProps
 * @param {object} boardMeta from buildPlayerHoleBoard
 */
async function buildProjectionRows(payload, dkProps, udProps, boardMeta) {
  const tables = await loadHolePropsTables(WEB);
  const courseKey = boardMeta.course_key || resolveCourseKey(payload);
  const pars = boardMeta.hole_pars || holeParsFromPayload(payload);
  const coursePar = num(payload?.course_par_18, pars.reduce((a, b) => a + b, 0));
  const round = boardMeta.round || Math.round(num(payload?.display_round, 1)) || 1;
  const byDg = playerByDg(payload, round);
  /** @type {object[]} */
  const rows = [];

  // --- Underdog hole packs ---
  for (const prop of udProps || []) {
    const dg = Math.round(num(prop.dg_id, NaN));
    const pl = Number.isFinite(dg) ? byDg.get(dg) : null;
    if (!pl && !Number.isFinite(dg)) continue;
    const holes =
      prop.market === HOLE_PROP_MARKETS.HOLES_10_18
        ? [10, 11, 12, 13, 14, 15, 16, 17, 18]
        : prop.market === HOLE_PROP_MARKETS.HOLES_16_17_18
          ? [16, 17, 18]
          : Array.isArray(prop.holes)
            ? prop.holes
            : [];
    if (!holes.length) continue;
    const stp = roundStp(pl || {}, coursePar);
    const sum = projectHoleSum({
      tables,
      courseKey,
      dgId: dg,
      holes,
      pars,
      roundStp: stp,
    });
    const line = num(prop.line, NaN);
    const { pOver, pUnder } = ouProbsNormal(sum.mu, sum.sigma, line);
    const pick = pickOuSide(pOver, pUnder, prop.over_odds, prop.under_odds);
    rows.push({
      book: "underdog",
      market: prop.market,
      dg_id: dg,
      player: String(prop.player_name || pl?.player_name || ""),
      holes,
      hole: null,
      mu: sum.mu,
      sigma: sum.sigma,
      line,
      over_odds: prop.over_odds,
      under_odds: prop.under_odds,
      side: pick.side,
      model_prob: pick.model_prob,
      implied: pick.implied,
      edge: Number.isFinite(pick.edge) ? Math.round(pick.edge * 10000) / 10000 : null,
      ev: pick.ev,
      odds: pick.odds,
      n_min: sum.n_min,
      source_model: "hole_avg+sg_sum",
    });
  }

  // --- DK Hole Score ---
  for (const prop of dkProps || []) {
    if (prop.market !== HOLE_PROP_MARKETS.HOLE_SCORE) continue;
    const dg = Math.round(num(prop.dg_id, NaN));
    const pl = Number.isFinite(dg) ? byDg.get(dg) : null;
    const hole = Math.round(num(prop.hole, NaN));
    if (!Number.isFinite(hole)) continue;
    const stp = roundStp(pl || {}, coursePar);
    const proj = projectHoleMean({
      tables,
      courseKey,
      dgId: dg,
      hole,
      par: pars[hole - 1],
      roundStp: stp,
    });
    const pmf = holeScorePmf(proj.mu, proj.par);

    if (Number.isFinite(num(prop.line, NaN)) && Number.isFinite(num(prop.over_odds, NaN))) {
      const line = num(prop.line, NaN);
      const { pOver, pUnder } = ouProbsFromPmf(pmf, line);
      const pick = pickOuSide(pOver, pUnder, prop.over_odds, prop.under_odds);
      rows.push({
        book: "draftkings",
        market: HOLE_PROP_MARKETS.HOLE_SCORE,
        dg_id: dg,
        player: String(prop.player_name || pl?.player_name || ""),
        hole,
        holes: [hole],
        mu: proj.mu,
        field_mean: proj.field_mean,
        sg: proj.sg,
        n: proj.n,
        line,
        over_odds: prop.over_odds,
        under_odds: prop.under_odds,
        side: pick.side,
        model_prob: pick.model_prob,
        implied: pick.implied,
        edge: Number.isFinite(pick.edge) ? Math.round(pick.edge * 10000) / 10000 : null,
        ev: pick.ev,
        odds: pick.odds,
        source_model: proj.source,
      });
    } else if (Array.isArray(prop.score_outcomes) && prop.score_outcomes.length) {
      // Exact / bucket markets: Par, Birdie or Better, Bogey or Worse, or integer scores
      let best = null;
      const par = proj.par;
      for (const oc of prop.score_outcomes) {
        const lab = String(oc.label || "");
        const am = num(oc.american, NaN);
        if (!Number.isFinite(am)) continue;
        let p = 0;
        let sideLabel = lab;
        const low = lab.toLowerCase();
        if (/birdie\s+or\s+better|eagle\s+or\s+better/.test(low)) {
          for (const [x, w] of pmf) if (x < par) p += w;
          sideLabel = "Birdie+";
        } else if (/^par$/i.test(lab.trim()) || /\bpar\b/i.test(low) && !/better|worse/.test(low)) {
          p = pmf.get(par) || 0;
          sideLabel = "Par";
        } else if (/bogey\s+or\s+worse|double/.test(low)) {
          for (const [x, w] of pmf) if (x > par) p += w;
          sideLabel = "Bogey+";
        } else {
          const m = lab.match(/\b([1-9]|1[0-2])\b/);
          if (!m) continue;
          const score = Math.round(Number(m[1]));
          p = pmf.get(score) || 0;
          sideLabel = String(score);
        }
        const imp = impliedProbFromAmerican(am);
        const edge = p - (Number.isFinite(imp) ? imp : 0);
        const ev = americanToEv(p, am);
        if (!best || (Number.isFinite(ev) && ev > (best.ev ?? -Infinity))) {
          best = {
            side: sideLabel,
            model_prob: p,
            implied: imp,
            edge,
            ev,
            odds: am,
          };
        }
      }
      if (best) {
        rows.push({
          book: "draftkings",
          market: HOLE_PROP_MARKETS.HOLE_SCORE,
          dg_id: dg,
          player: String(prop.player_name || pl?.player_name || ""),
          hole,
          holes: [hole],
          mu: proj.mu,
          field_mean: proj.field_mean,
          sg: proj.sg,
          n: proj.n,
          line: par,
          side: best.side,
          model_prob: Math.round(best.model_prob * 10000) / 10000,
          implied: Number.isFinite(best.implied) ? Math.round(best.implied * 10000) / 10000 : null,
          edge: Number.isFinite(best.edge) ? Math.round(best.edge * 10000) / 10000 : null,
          ev: best.ev,
          odds: best.odds,
          source_model: proj.source,
          exact: true,
        });
      }
    }
  }

  // --- DK Hole Winner / Hole Matchup (one group once) ---
  const seenGroups = new Set();
  for (const prop of dkProps || []) {
    if (
      prop.market !== HOLE_PROP_MARKETS.HOLE_WINNER &&
      prop.market !== HOLE_PROP_MARKETS.HOLE_MATCHUP
    ) {
      continue;
    }
    const gid = String(prop.group_id || "");
    const hole = Math.round(num(prop.hole, NaN));
    const gkey = `${prop.market}|${gid}|${hole}|${prop.round_num}`;
    if (seenGroups.has(gkey)) continue;
    seenGroups.add(gkey);
    const group = Array.isArray(prop.group) ? prop.group : [];
    if (group.length < 2 || !Number.isFinite(hole)) continue;

    const playerPmfs = [];
    for (const g of group) {
      const dg = Math.round(num(g.dg_id, NaN));
      const pl = Number.isFinite(dg) ? byDg.get(dg) : null;
      const stp = roundStp(pl || {}, coursePar);
      const proj = projectHoleMean({
        tables,
        courseKey,
        dgId: dg,
        hole,
        par: pars[hole - 1],
        roundStp: stp,
      });
      playerPmfs.push({
        dg_id: dg,
        player_name: g.player_name,
        american: g.american,
        mu: proj.mu,
        n: proj.n,
        pmf: holeScorePmf(proj.mu, proj.par),
      });
    }
    const { win } = holeWinnerProbs(playerPmfs);
    for (const g of playerPmfs) {
      const id = String(g.dg_id);
      const modelProb = win[id] || 0;
      const am = num(g.american, NaN);
      const imp = impliedProbFromAmerican(am);
      const edge = modelProb - (Number.isFinite(imp) ? imp : 0);
      rows.push({
        book: "draftkings",
        market: prop.market,
        dg_id: g.dg_id,
        player: g.player_name,
        hole,
        holes: [hole],
        mu: g.mu,
        n: g.n,
        line: null,
        side: prop.market === HOLE_PROP_MARKETS.HOLE_MATCHUP ? "WIN" : "WIN",
        model_prob: Math.round(modelProb * 10000) / 10000,
        implied: Number.isFinite(imp) ? Math.round(imp * 10000) / 10000 : null,
        edge: Math.round(edge * 10000) / 10000,
        ev: americanToEv(modelProb, am),
        odds: am,
        group_id: gid,
        group: group.map((x) => x.player_name),
        source_model:
          prop.market === HOLE_PROP_MARKETS.HOLE_MATCHUP
            ? "hole_avg+sg_matchup"
            : "hole_avg+sg_winner",
      });
    }
  }

  // Model-only board rows for packs (when no UD line) — useful coverage display
  if (!(udProps || []).length) {
    for (const b of boardMeta.board || []) {
      for (const [market, pack] of [
        [HOLE_PROP_MARKETS.HOLES_10_18, b.holes_10_18],
        [HOLE_PROP_MARKETS.HOLES_16_17_18, b.holes_16_17_18],
      ]) {
        rows.push({
          book: "model",
          market,
          dg_id: b.dg_id,
          player: b.player,
          holes: market === HOLE_PROP_MARKETS.HOLES_10_18 ? [10, 11, 12, 13, 14, 15, 16, 17, 18] : [16, 17, 18],
          hole: null,
          mu: pack.mu,
          sigma: pack.sigma,
          line: null,
          side: null,
          model_prob: null,
          implied: null,
          edge: null,
          ev: null,
          odds: null,
          n_min: pack.n_min,
          source_model: "hole_avg+sg_sum",
          model_only: true,
        });
      }
    }
  }

  rows.sort((a, b) => (num(b.ev, -999) || -999) - (num(a.ev, -999) || -999));
  return rows;
}

async function loadOrFetchOdds(payload, targetRound) {
  const skipFetch = String(process.env.GOLF_HOLE_PROPS_SKIP_FETCH || "").trim() === "1";
  let dkProps = [];
  let udProps = [];
  let dkError = null;
  let udError = null;

  const dkCached = readJson(DK_ODDS);
  const udCached = readJson(UD_ODDS);

  if (!skipFetch && process.env.GOLF_SKIP_DK_HOLE !== "1") {
    try {
      const leagueUrl = inferDraftKingsLeagueUrlFromProjections(payload) || undefined;
      const hit = await fetchDraftKingsHoleProps({
        players: payload.players || [],
        leagueUrl,
        targetRound,
      });
      dkProps = hit.props || [];
      dkError = hit.error || null;
      mkdirSync(dirname(DK_ODDS), { recursive: true });
      writeFileSync(
        DK_ODDS,
        `${JSON.stringify({
          generated_at: new Date().toISOString(),
          source: "draftkings",
          error: dkError,
          n: dkProps.length,
          props: dkProps,
        })}\n`,
      );
    } catch (e) {
      dkError = e?.message || String(e);
      console.warn("[bake-hole-props] DK fetch failed:", dkError);
      dkProps = dkCached?.props || [];
    }
  } else {
    dkProps = dkCached?.props || [];
    dkError = dkCached?.error || (skipFetch ? "skip fetch" : null);
  }

  if (!skipFetch && process.env.GOLF_SKIP_UD_HOLE !== "1") {
    try {
      const hit = await fetchUnderdogHoleProps({ payload, targetRound });
      udProps = hit.props || [];
      udError = hit.error || null;
      mkdirSync(dirname(UD_ODDS), { recursive: true });
      writeFileSync(
        UD_ODDS,
        `${JSON.stringify({
          generated_at: new Date().toISOString(),
          source: "underdog",
          error: udError,
          n: udProps.length,
          props: udProps,
        })}\n`,
      );
    } catch (e) {
      udError = e?.message || String(e);
      console.warn("[bake-hole-props] UD fetch failed:", udError);
      udProps = udCached?.props || [];
    }
  } else {
    udProps = udCached?.props || [];
    udError = udCached?.error || (skipFetch ? "skip fetch" : null);
  }

  return { dkProps, udProps, dkError, udError };
}

async function main() {
  if (!existsSync(PROJ)) throw new Error(`Missing ${PROJ}`);
  const payload = JSON.parse(readFileSync(PROJ, "utf8"));
  const targetRound = Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, 1)) || 1;

  const boardMeta = await buildPlayerHoleBoard(payload, WEB);
  const { dkProps, udProps, dkError, udError } = await loadOrFetchOdds(payload, targetRound);
  const projections = await buildProjectionRows(payload, dkProps, udProps, boardMeta);

  const withEdge = projections.filter((r) => Number.isFinite(r.edge) && r.edge > 0);
  const bestEv = projections.reduce((m, r) => Math.max(m, num(r.ev, -Infinity)), -Infinity);

  const out = {
    generated_at: new Date().toISOString(),
    event_name: payload.event_name || "",
    round: targetRound,
    course_key: boardMeta.course_key,
    hole_pars: boardMeta.hole_pars,
    coverage: boardMeta.coverage,
    // Keep odds packs in dk_hole_props.json / ud_hole_props.json — do not embed (~2× size).
    odds: {
      dk_error: dkError,
      ud_error: udError,
      n_dk: dkProps.length,
      n_ud: udProps.length,
      dk_path: "data/dk_hole_props.json",
      ud_path: "data/ud_hole_props.json",
    },
    projections,
    meta: {
      model: "hole_avg+sg",
      n_dk: dkProps.length,
      n_ud: udProps.length,
      n_projections: projections.length,
      n_positive_edge: withEdge.length,
      best_ev: Number.isFinite(bestEv) && bestEv > -1e8 ? Math.round(bestEv * 10000) / 10000 : null,
    },
  };

  mkdirSync(dirname(OUT), { recursive: true });
  // Compact JSON (no pretty-print) — UI fetches this every reload.
  writeFileSync(OUT, `${JSON.stringify(out)}\n`);

  payload.hole_props = {
    generated_at: out.generated_at,
    path: "data/live_hole_props.json",
    n_picks: projections.length,
    n_positive_edge: withEdge.length,
    course_key: boardMeta.course_key,
    round: targetRound,
  };
  writeFileSync(PROJ, `${JSON.stringify(payload, null, 2)}\n`);

  console.log(
    `[bake-hole-props] ${projections.length} rows (DK odds ${dkProps.length}, UD ${udProps.length}) → ${OUT}`,
  );
  console.log(
    `[bake-hole-props] coverage ${boardMeta.coverage.with_hole_history}/${boardMeta.coverage.players} players with hole history · +edge ${withEdge.length}`,
  );
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
