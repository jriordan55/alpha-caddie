/**
 * DataGolf preds/live-tournament-stats — Live Tournament Stats feed (Thu–Sun in-play weeks).
 * https://feeds.datagolf.com/preds/live-tournament-stats
 *
 * Used for Historical Trends current-event rounds and live projection row updates.
 * Completed seasons still come from historical-raw-data/rounds (CSV).
 */

import { reconcileHoleCountsFromScore } from "./course-round-adjustments.mjs";

export const DEFAULT_LIVE_TOURNAMENT_STATS =
  "sg_ott,distance,accuracy,sg_app,sg_arg,gir,prox_fw,sg_putt,sg_t2g,sg_total,scrambling";

export function num(v, fallback = NaN) {
  if (v == null || v === "") return fallback;
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

export function pickNum(row, aliases) {
  if (!row || typeof row !== "object") return NaN;
  for (const k of aliases) {
    const v = num(row[k], NaN);
    if (Number.isFinite(v)) return v;
  }
  return NaN;
}

export function liveStatsList(payload) {
  if (!payload || typeof payload !== "object") return [];
  if (Array.isArray(payload.live_stats)) return payload.live_stats;
  return [];
}

export function liveTournamentStatsUrl(key, roundParam, statsParam) {
  const u = new URL("https://feeds.datagolf.com/preds/live-tournament-stats");
  u.searchParams.set(
    "stats",
    String(statsParam || "").trim() || DEFAULT_LIVE_TOURNAMENT_STATS,
  );
  u.searchParams.set("round", String(roundParam ?? "event_avg").trim() || "event_avg");
  u.searchParams.set("display", "value");
  u.searchParams.set("file_format", "json");
  u.searchParams.set("key", key);
  return u.href;
}

/** @param {(url: string) => Promise<unknown>} fetchJson */
export async function fetchLiveTournamentStatsByRound(key, fetchJson, rounds = [1, 2, 3, 4]) {
  const statsParam =
    String(process.env.GOLF_LIVE_TOURNAMENT_STATS_STATS || "").trim() || DEFAULT_LIVE_TOURNAMENT_STATS;
  /** @type {Record<string, unknown>} */
  const out = {};
  for (const r of rounds) {
    const rn = Math.round(num(r));
    if (!Number.isFinite(rn) || rn < 1 || rn > 4) continue;
    try {
      out[String(rn)] = await fetchJson(liveTournamentStatsUrl(key, String(rn), statsParam));
    } catch (e) {
      console.warn(`[live-tournament-stats] round=${rn}:`, e?.message || e);
    }
  }
  return out;
}

export function grossFromInPlayRow(row, roundNum) {
  if (!row || typeof row !== "object") return NaN;
  const rn = Math.round(num(roundNum));
  if (!Number.isFinite(rn) || rn < 1 || rn > 4) return NaN;
  return num(row[`R${rn}`] ?? row[`r${rn}`], NaN);
}

function inPlayField(row, aliases) {
  if (!row || typeof row !== "object") return NaN;
  for (const k of aliases) {
    if (!(k in row)) continue;
    const v = num(row[k], NaN);
    if (Number.isFinite(v)) return v;
  }
  return NaN;
}

/** preds/in-play scorecard fields (DataGolf) when present on the same player row. */
export function countingFromInPlayRow(ipRow, thruRounded) {
  if (!ipRow || typeof ipRow !== "object") return {};
  const th = Math.round(num(thruRounded, NaN));
  const cap = Number.isFinite(th) && th > 0 ? th + 3 : 22;
  const capCt = (v) => {
    if (!Number.isFinite(v) || v < 0) return NaN;
    const r = Math.round(v);
    return r <= cap ? r : NaN;
  };
  const birdies = capCt(
    inPlayField(ipRow, ["today_birdies", "round_birdies", "birdies_today", "birdies_thru", "n_birdies", "birdies"]),
  );
  const bogeys = capCt(
    inPlayField(ipRow, [
      "today_bogeys",
      "round_bogeys",
      "bogeys_today",
      "bogies_today",
      "today_bogies",
      "bogeys_thru",
      "bogeys",
    ]),
  );
  let pars = capCt(inPlayField(ipRow, ["today_pars", "round_pars", "pars_today", "pars_thru", "pars"]));
  const eagles = capCt(
    inPlayField(ipRow, ["today_eagles", "eagles_today", "eagles_or_better_today", "eagles_thru"]),
  );
  if (!Number.isFinite(pars) && Number.isFinite(th) && th >= 1) {
    const b = Number.isFinite(birdies) ? birdies : 0;
    const bg = Number.isFinite(bogeys) ? bogeys : 0;
    const e = Number.isFinite(eagles) ? eagles : 0;
    pars = Math.max(0, Math.min(th, th - b - bg - e));
  }
  return { birdies, pars, bogeys, eagles };
}

/** Drop DG live-stats placeholders (e.g. pars=18 with no birdies/bogeys, or 0/0/0 stubs). */
export function sanitizeLiveCountingFields(act) {
  if (!act || typeof act !== "object") return act;
  const thru = Math.round(num(act.thru, NaN));
  let b = num(act.birdies, NaN);
  let p = num(act.pars, NaN);
  let bg = num(act.bogeys, NaN);
  const gir = num(act.gir, NaN);
  if (Number.isFinite(p) && (p >= 14 || (Number.isFinite(thru) && thru >= 10 && p >= thru - 1))) {
    if (!Number.isFinite(b) && !Number.isFinite(bg)) p = NaN;
  }
  // Explicit zero triad (or zero bird+bog with empty pars) is a missing-stat stub, not a real round.
  if (b === 0 && bg === 0 && (!Number.isFinite(p) || p === 0)) {
    b = NaN;
    p = NaN;
    bg = NaN;
  } else if (b === 0 && bg === 0 && Number.isFinite(p) && p >= 10) {
    b = NaN;
    p = NaN;
    bg = NaN;
  }
  if (Number.isFinite(gir) && Number.isFinite(p) && Math.round(gir) === Math.round(p)) p = NaN;
  act.birdies = Number.isFinite(b) ? Math.round(b) : null;
  act.pars = Number.isFinite(p) ? Math.round(p) : null;
  act.bogeys = Number.isFinite(bg) ? Math.round(bg) : null;
  return act;
}

/**
 * One player-round from live-tournament-stats `live_stats[]` (+ optional preds/in-play row for gross fallback).
 * @param {object} statsRow
 * @param {object} [inPlayRow]
 * @param {number} roundPar — regulation par for the course (typically 70–72)
 */
export function parseLiveTournamentStatsCounting(statsRow, inPlayRow, roundPar, roundNum, fairwayHoles = 14) {
  if (!statsRow || typeof statsRow !== "object") return null;
  const thru = Math.round(num(statsRow.thru ?? statsRow.Thru, NaN));
  const today = num(statsRow.today ?? statsRow.Today, NaN);
  const rnd = Math.round(num(roundNum, NaN));

  let roundScore = pickNum(statsRow, [
    "round_score",
    "score",
    "strokes",
    "round_strokes",
    "gross",
    "round_gross",
  ]);
  if (!Number.isFinite(roundScore) && inPlayRow) {
    roundScore = grossFromInPlayRow(inPlayRow, rnd);
  }
  if (!Number.isFinite(roundScore) && Number.isFinite(today) && Number.isFinite(roundPar) && thru >= 18) {
    roundScore = Math.round(roundPar + today);
  }

  let birdies = pickNum(statsRow, ["birdies", "n_birdies", "birdie", "birdies_made"]);
  let pars = pickNum(statsRow, ["pars", "n_pars", "par", "pars_made"]);
  let bogeys = pickNum(statsRow, ["bogeys", "bogeys", "n_bogeys", "bogey", "bogeys_made"]);

  const ip = inPlayRow ? countingFromInPlayRow(inPlayRow, thru) : {};
  if (!Number.isFinite(birdies) && Number.isFinite(ip.birdies)) birdies = ip.birdies;
  if (!Number.isFinite(pars) && Number.isFinite(ip.pars)) pars = ip.pars;
  if (!Number.isFinite(bogeys) && Number.isFinite(ip.bogeys)) bogeys = ip.bogeys;

  const girRaw = num(statsRow.gir, NaN);
  const gir = Number.isFinite(girRaw) && girRaw > 0 && girRaw <= 1.0001 ? Math.round(girRaw * 18) : girRaw;
  const fwHoles =
    Number.isFinite(num(fairwayHoles, NaN)) && fairwayHoles >= 1 ? Math.round(num(fairwayHoles, NaN)) : 14;
  const accRaw = pickNum(statsRow, ["accuracy", "driving_accuracy", "fairways", "fw_pct"]);
  let fairways = NaN;
  if (Number.isFinite(accRaw)) {
    if (accRaw > 0 && accRaw <= 1.0001) fairways = Math.round(accRaw * fwHoles);
    else if (accRaw > 1 && accRaw <= fwHoles) fairways = Math.round(accRaw);
  }

  const out = {
    round_score: Number.isFinite(roundScore) ? Math.round(roundScore * 10) / 10 : null,
    birdies: Number.isFinite(birdies) ? Math.round(birdies) : null,
    pars: Number.isFinite(pars) ? Math.round(pars) : null,
    bogeys: Number.isFinite(bogeys) ? Math.round(bogeys) : null,
    gir: Number.isFinite(gir) ? gir : null,
    fairways: Number.isFinite(fairways) ? fairways : null,
    thru: Number.isFinite(thru) ? thru : null,
    today: Number.isFinite(today) ? today : null,
    sg_putt: num(statsRow.sg_putt, NaN),
    sg_app: num(statsRow.sg_app, NaN),
    sg_arg: num(statsRow.sg_arg, NaN),
    sg_ott: num(statsRow.sg_ott, NaN),
    sg_t2g: num(statsRow.sg_t2g, NaN),
    sg_total: num(statsRow.sg_total, NaN),
  };

  sanitizeLiveCountingFields(out);
  if (Number.isFinite(roundPar) && Number.isFinite(out.round_score) && Number.isFinite(out.birdies)) {
    Object.assign(out, reconcileHoleCountsFromScore(out, roundPar));
  }

  const hasCounting =
    Number.isFinite(out.round_score) ||
    Number.isFinite(out.birdies) ||
    Number.isFinite(out.pars) ||
    Number.isFinite(out.bogeys);
  if (!hasCounting) return null;
  return out;
}

function mergeRoundActualIntoMap(byDg, dg, rnd, parsed, source) {
  if (!parsed || typeof parsed !== "object") return;
  const key = String(dg);
  if (!byDg[key]) byDg[key] = {};
  const rk = String(rnd);
  const prev = byDg[key][rk];
  if (prev && typeof prev === "object") {
    const merged = { ...prev, ...parsed };
    if (Number.isFinite(num(prev.round_score, NaN)) && !Number.isFinite(num(parsed.round_score, NaN)))
      merged.round_score = prev.round_score;
    for (const k of ["birdies", "pars", "bogeys", "gir", "thru"]) {
      if (Number.isFinite(num(prev[k], NaN)) && !Number.isFinite(num(parsed[k], NaN))) merged[k] = prev[k];
    }
    byDg[key][rk] = { ...merged, source: prev.source || source };
    return;
  }
  byDg[key][rk] = { ...parsed, source };
}

/** Gross-only row from preds/in-play `R1`…`R4` when LTS has no row for that round. */
export function parseInPlayGrossRoundActual(inPlayRow, roundNum, roundPar) {
  if (!inPlayRow || typeof inPlayRow !== "object") return null;
  const rnd = Math.round(num(roundNum, NaN));
  if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return null;
  const roundScore = grossFromInPlayRow(inPlayRow, rnd);
  if (!Number.isFinite(roundScore)) return null;
  const playerR = Math.round(num(inPlayRow.round ?? inPlayRow.Round, NaN));
  const thru =
    playerR === rnd
      ? Math.round(num(inPlayRow.thru ?? inPlayRow.Thru ?? inPlayRow.thru_hole, NaN))
      : 18;
  const ipCount =
    playerR === rnd && Number.isFinite(thru) && thru > 0
      ? countingFromInPlayRow(inPlayRow, thru)
      : {};
  const out = {
    round_score: Math.round(roundScore * 10) / 10,
    birdies: Number.isFinite(ipCount.birdies) ? Math.round(ipCount.birdies) : null,
    pars: Number.isFinite(ipCount.pars) ? Math.round(ipCount.pars) : null,
    bogeys: Number.isFinite(ipCount.bogeys) ? Math.round(ipCount.bogeys) : null,
    gir: null,
    thru: Number.isFinite(thru) && thru > 0 ? thru : 18,
    today: null,
    sg_putt: NaN,
    sg_app: NaN,
    sg_arg: NaN,
    sg_ott: NaN,
    sg_t2g: NaN,
    sg_total: NaN,
  };
  if (playerR === rnd) {
    const today = num(inPlayRow.today ?? inPlayRow.Today, NaN);
    if (Number.isFinite(today)) out.today = today;
    for (const k of ["sg_putt", "sg_app", "sg_arg", "sg_ott", "sg_t2g", "sg_total"]) {
      const v = num(inPlayRow[k], NaN);
      if (Number.isFinite(v)) out[k] = v;
    }
  }
  sanitizeLiveCountingFields(out);
  return out;
}

/**
 * @param {Record<string, unknown>} statsByRound — keys "1".."4" → API payload
 * @param {Map<number, object>} inPlayByDg
 * @param {{ roundPar?: number }} opts
 */
export function buildLiveRoundActualsByDg(statsByRound, inPlayByDg, opts = {}) {
  const roundPar = Number.isFinite(num(opts.roundPar, NaN)) ? num(opts.roundPar, NaN) : 72;
  const fairwayHoles = Number.isFinite(num(opts.fairwayHoles, NaN)) ? Math.round(num(opts.fairwayHoles, NaN)) : 14;
  /** @type {Record<string, Record<string, object>>} */
  const byDg = {};

  for (let rnd = 1; rnd <= 4; rnd++) {
    const list = liveStatsList(statsByRound?.[String(rnd)]);
    for (const statsRow of list) {
      const dg = Math.round(num(statsRow.dg_id ?? statsRow.dgId, NaN));
      if (!Number.isFinite(dg)) continue;
      const ip = inPlayByDg.get(dg);
      const parsed = parseLiveTournamentStatsCounting(statsRow, ip, roundPar, rnd, fairwayHoles);
      if (!parsed) continue;
      mergeRoundActualIntoMap(byDg, dg, rnd, parsed, "live_tournament_stats");
    }
  }

  for (const [dg, ip] of inPlayByDg) {
    const playerR = Math.round(num(ip.round ?? ip.Round, NaN));
    for (let rnd = 1; rnd <= 4; rnd++) {
      const gross = grossFromInPlayRow(ip, rnd);
      if (!Number.isFinite(gross)) continue;
      const key = String(dg);
      const rk = String(rnd);
      const prev = byDg[key]?.[rk] && typeof byDg[key][rk] === "object" ? byDg[key][rk] : {};
      const fromIp = parseInPlayGrossRoundActual(ip, rnd, roundPar);
      /** @type {Record<string, unknown>} */
      const next = {
        ...prev,
        round_score: Math.round(gross * 10) / 10,
        source: prev.source || "in_play_gross",
      };
      if (fromIp && playerR === rnd) {
        if (Number.isFinite(num(fromIp.birdies, NaN))) next.birdies = fromIp.birdies;
        if (Number.isFinite(num(fromIp.pars, NaN))) next.pars = fromIp.pars;
        if (Number.isFinite(num(fromIp.bogeys, NaN))) next.bogeys = fromIp.bogeys;
        if (Number.isFinite(num(fromIp.today, NaN))) next.today = fromIp.today;
        if (Number.isFinite(num(fromIp.thru, NaN))) next.thru = fromIp.thru;
      }
      sanitizeLiveCountingFields(next);
      if (!byDg[key]) byDg[key] = {};
      byDg[key][rk] = next;
    }
  }

  return byDg;
}

/** Per-round player counts for fetch logging. */
export function liveRoundActualsRoundCounts(byDg) {
  /** @type {Record<string, number>} */
  const counts = { "1": 0, "2": 0, "3": 0, "4": 0 };
  if (!byDg || typeof byDg !== "object") return counts;
  for (const per of Object.values(byDg)) {
    if (!per || typeof per !== "object") continue;
    for (const [rk, rec] of Object.entries(per)) {
      if (!rec || typeof rec !== "object") continue;
      if (Number.isFinite(num(rec.round_score, NaN))) counts[rk] = (counts[rk] || 0) + 1;
    }
  }
  return counts;
}

/** Strip DG/live 0/0/0 hole-count stubs from every player-round in a live_round_actuals map. */
export function sanitizeLiveRoundActualsByDg(byDg) {
  if (!byDg || typeof byDg !== "object") return byDg || {};
  for (const per of Object.values(byDg)) {
    if (!per || typeof per !== "object") continue;
    for (const [rk, rec] of Object.entries(per)) {
      if (!rec || typeof rec !== "object") continue;
      per[rk] = sanitizeLiveCountingFields({ ...rec });
    }
  }
  return byDg;
}

/**
 * Build or augment `live_round_actuals_by_dg` from a preds/in-play bundle
 * (precomputed block, per-round LTS payloads, and/or in-play `R*` columns).
 * Always sanitizes so push:live never re-bakes placeholder bird/par/bog zeros.
 */
export function resolveLiveRoundActualsByDg(bundle, opts = {}) {
  if (!bundle || typeof bundle !== "object") return {};
  const fu = bundle.field_updates && typeof bundle.field_updates === "object" ? bundle.field_updates : {};
  const roundPar = Number.isFinite(num(opts.roundPar, NaN))
    ? num(opts.roundPar, NaN)
    : num(fu.course_par ?? fu.coursePar, 72) || 72;
  const inPlayByDg = new Map();
  for (const row of Array.isArray(bundle.data) ? bundle.data : []) {
    const id = Math.round(num(row?.dg_id ?? row?.dgId, NaN));
    if (Number.isFinite(id)) inPlayByDg.set(id, row);
  }
  const statsByRound =
    bundle.live_tournament_stats_by_round && typeof bundle.live_tournament_stats_by_round === "object"
      ? bundle.live_tournament_stats_by_round
      : {};
  const fairwayHoles = Number.isFinite(num(opts.fairwayHoles, NaN)) ? Math.round(num(opts.fairwayHoles, NaN)) : 14;
  const built = sanitizeLiveRoundActualsByDg(
    buildLiveRoundActualsByDg(statsByRound, inPlayByDg, { roundPar, fairwayHoles }),
  );
  const pre = bundle.live_round_actuals_by_dg;
  if (!pre || typeof pre !== "object") return built;
  /** @type {Record<string, Record<string, object>>} */
  const out = sanitizeLiveRoundActualsByDg(JSON.parse(JSON.stringify(pre)));
  for (const [dgKey, per] of Object.entries(built)) {
    if (!per || typeof per !== "object") continue;
    if (!out[dgKey]) out[dgKey] = {};
    for (const [rk, rec] of Object.entries(per)) {
      const prev = out[dgKey][rk];
      if (prev && typeof prev === "object") {
        const merged = { ...prev, ...rec };
        if (Number.isFinite(num(prev.round_score, NaN)) && !Number.isFinite(num(rec.round_score, NaN)))
          merged.round_score = prev.round_score;
        // Never keep prior stub zeros when the fresh build cleared counting.
        for (const k of ["birdies", "pars", "bogeys"]) {
          if (rec[k] == null && (prev[k] === 0 || prev[k] == null)) merged[k] = null;
        }
        out[dgKey][rk] = sanitizeLiveCountingFields(merged);
      } else {
        out[dgKey][rk] = rec;
      }
    }
  }
  return sanitizeLiveRoundActualsByDg(out);
}
