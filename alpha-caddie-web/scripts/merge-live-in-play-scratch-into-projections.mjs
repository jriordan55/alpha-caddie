#!/usr/bin/env node
/**
 * Bake preds/in-play live scratch (thru, today, current_score, hole counts) onto
 * projections.json player rows and enable in-play model pricing for +EV / O/U.
 *
 * Browser +EV uses `meta.in_play_affects_round_odds` + dg_live_* fields; without this
 * step push:live ships pre-round μ only until the client polls live-in-play.json.
 *
 * npm run merge:live-in-play-scratch-into-projections
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { pickNum, resolveLiveRoundActualsByDg } from "./dg-live-tournament-stats.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function clearDgLiveRoundScratch(p) {
  delete p.dg_live_thru;
  delete p.dg_live_today;
  delete p.dg_live_birdies_so_far;
  delete p.dg_live_bogeys_so_far;
  delete p.dg_live_pars_so_far;
  delete p.dg_live_eagles_so_far;
}

function mergeDgLiveScorecardCounts(p, inPlayRow, thruRounded) {
  const th = Math.round(num(thruRounded, NaN));
  const cap = Number.isFinite(th) && th > 0 ? th + 3 : 22;
  const q = (aliases) => {
    const v = pickNum(inPlayRow, aliases);
    if (!Number.isFinite(v) || v < 0) return NaN;
    const r = Math.round(v);
    return r <= cap ? r : NaN;
  };
  const setCt = (val, key) => {
    if (Number.isFinite(val) && val >= 0 && val <= 22) p[key] = val;
    else delete p[key];
  };
  setCt(q(["today_birdies", "round_birdies", "birdies_today", "birdies_thru", "n_birdies"]), "dg_live_birdies_so_far");
  setCt(
    q(["today_bogeys", "round_bogeys", "bogeys_today", "bogies_today", "today_bogies", "bogeys_thru"]),
    "dg_live_bogeys_so_far",
  );
  setCt(q(["today_pars", "round_pars", "pars_today", "pars_thru"]), "dg_live_pars_so_far");
  setCt(q(["today_eagles", "eagles_today", "eagles_or_better_today", "eagles_thru"]), "dg_live_eagles_so_far");
  const genB = pickNum(inPlayRow, ["birdies"]);
  if (
    !Object.prototype.hasOwnProperty.call(p, "dg_live_birdies_so_far") &&
    Number.isFinite(genB) &&
    Number.isFinite(th) &&
    th >= 1 &&
    genB >= 0 &&
    genB <= th
  ) {
    p.dg_live_birdies_so_far = Math.round(genB);
  }
}

function scoreFromFieldPlayerRow(fp) {
  const sc = num(fp?.current_score ?? fp?.currentScore, NaN);
  return Number.isFinite(sc) ? sc : NaN;
}

/**
 * @param {object} proj
 * @param {object} live
 * @returns {{ playersTouched: number, inPlayPricing: boolean }}
 */
export function mergeLiveInPlayScratchOntoProjections(proj, live) {
  if (!proj || !Array.isArray(proj.players) || !live) {
    return { playersTouched: 0, inPlayPricing: false };
  }

  const meta = proj.meta && typeof proj.meta === "object" ? proj.meta : {};
  if (!proj.meta) proj.meta = meta;

  const info = live.info && typeof live.info === "object" ? live.info : {};
  const fu = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const liveEv = String(fu.event_name || fu.eventName || info.event_name || live.event_name || "").trim();
  const projEv = String(proj.event_name || meta.event_name || "").trim();
  if (liveEv && projEv && !eventsLikelySame(liveEv, projEv)) {
    console.warn(`[merge-live-scratch] event mismatch live="${liveEv}" vs proj="${projEv}" — skip`);
    return { playersTouched: 0, inPlayPricing: false };
  }

  const liveR = Math.round(
    num(
      meta.datagolf_live_current_round ?? proj.datagolf_live_current_round ?? fu.current_round ?? info.current_round,
      NaN,
    ),
  );

  const fwHoles = Math.round(num(meta.projection_course_basis?.fairway_holes_modeled, 14)) || 14;
  const actualsByDg = resolveLiveRoundActualsByDg(live, {
    roundPar: num(proj.course_par_18, NaN) || 72,
    fairwayHoles: fwHoles,
  });
  if (actualsByDg && Object.keys(actualsByDg).length) {
    meta.live_round_actuals_by_dg = actualsByDg;
    proj.live_round_actuals_by_dg = actualsByDg;
  }

  const inPlayRows = Array.isArray(live.data) ? live.data : [];
  const fieldList = fu.field ?? fu.field_updates ?? fu.players ?? fu.data;
  const fieldByDg = new Map();
  if (Array.isArray(fieldList)) {
    for (const fp of fieldList) {
      const id = Math.round(num(fp?.dg_id ?? fp?.dgId, NaN));
      if (!Number.isFinite(id)) continue;
      const sc = scoreFromFieldPlayerRow(fp);
      if (Number.isFinite(sc)) fieldByDg.set(id, sc);
    }
  }

  let playersTouched = 0;
  let hasInRoundScratch = false;
  let hasTournamentScore = false;

  for (const row of inPlayRows) {
    if (!row || typeof row !== "object") continue;
    const id = Math.round(num(row.dg_id ?? row.dgId, NaN));
    if (!Number.isFinite(id)) continue;
    const dgRound = Math.round(num(row.round ?? row.Round, NaN));
    const thruLive = num(row.thru ?? row.Thru, NaN);
    const todayLive = num(row.today ?? row.Today, NaN);
    const curScore = num(row.current_score ?? row.currentScore, fieldByDg.get(id));
    const byId = proj.players.filter((p) => Math.round(num(p.dg_id, NaN)) === id);
    if (!byId.length) continue;

    for (const p of byId) {
      if (Number.isFinite(curScore)) {
        p.current_score = curScore;
        hasTournamentScore = true;
        playersTouched++;
      }
      const pr = Math.round(num(p.round, NaN));
      if (!Number.isFinite(dgRound) || dgRound < 1 || dgRound > 4) {
        clearDgLiveRoundScratch(p);
        continue;
      }
      if (pr !== dgRound) {
        clearDgLiveRoundScratch(p);
        continue;
      }
      if (Number.isFinite(thruLive) && Math.round(thruLive) >= 1) {
        p.dg_live_thru = Math.round(thruLive);
      } else {
        delete p.dg_live_thru;
      }
      if (Number.isFinite(todayLive)) p.dg_live_today = todayLive;
      else delete p.dg_live_today;
      if (Number.isFinite(thruLive) && Math.round(thruLive) >= 1) {
        hasInRoundScratch = true;
        mergeDgLiveScorecardCounts(p, row, thruLive);
        playersTouched++;
      } else {
        delete p.dg_live_birdies_so_far;
        delete p.dg_live_bogeys_so_far;
        delete p.dg_live_pars_so_far;
        delete p.dg_live_eagles_so_far;
      }
    }
  }

  const displayR = Math.round(num(proj.display_round ?? meta.display_round, NaN));
  const lastUpdate = String(live.last_update || info.last_update || live.fetched_at || "").trim();
  if (lastUpdate) meta.datagolf_live_last_update = lastUpdate;

  const inPlayPricing =
    Boolean(liveEv || inPlayRows.length) &&
    (hasInRoundScratch || hasTournamentScore || (Number.isFinite(displayR) && displayR > 1));

  if (inPlayPricing) {
    meta.in_play_affects_round_odds = true;
    meta.datagolf_live_in_tournament = true;
    if (Number.isFinite(liveR) && liveR >= 1 && liveR <= 4) {
      meta.datagolf_live_current_round = liveR;
      proj.datagolf_live_current_round = liveR;
    }
    if (!Object.prototype.hasOwnProperty.call(meta, "live_matchup_model_blend")) {
      meta.live_matchup_model_blend = 0;
    }
  } else {
    delete meta.in_play_affects_round_odds;
    delete meta.datagolf_live_in_tournament;
  }

  return { playersTouched, inPlayPricing };
}

async function main() {
  const projPath = join(WEB_ROOT, "projections.json");
  const livePath = join(WEB_ROOT, "live-in-play.json");
  if (!existsSync(projPath)) {
    console.warn("[merge-live-scratch] missing projections.json — skip");
    return;
  }
  if (!existsSync(livePath)) {
    console.warn("[merge-live-scratch] missing live-in-play.json — skip");
    return;
  }

  let proj;
  let live;
  try {
    proj = JSON.parse(readFileSync(projPath, "utf8"));
    live = JSON.parse(readFileSync(livePath, "utf8"));
  } catch (e) {
    console.warn("[merge-live-scratch] parse error —", e.message || e);
    return;
  }

  const { playersTouched, inPlayPricing } = mergeLiveInPlayScratchOntoProjections(proj, live);
  writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`, "utf8");
  console.log(
    `[merge-live-scratch] touched ${playersTouched} player row(s); in_play_affects_round_odds=${inPlayPricing}; wrote ${projPath}`,
  );
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  main().catch((e) => {
    console.error("[merge-live-scratch] fatal:", e.message || e);
    process.exit(1);
  });
}
