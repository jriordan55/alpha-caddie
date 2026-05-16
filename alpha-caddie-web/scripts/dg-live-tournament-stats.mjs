/**
 * DataGolf preds/live-tournament-stats — Live Tournament Stats feed (Thu–Sun in-play weeks).
 * https://feeds.datagolf.com/preds/live-tournament-stats
 *
 * Used for Historical Trends current-event rounds and live projection row updates.
 * Completed seasons still come from historical-raw-data/rounds (CSV).
 */

export const DEFAULT_LIVE_TOURNAMENT_STATS =
  "sg_ott,distance,accuracy,sg_app,gir,prox_fw,sg_putt,scrambling";

export function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
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

/**
 * One player-round from live-tournament-stats `live_stats[]` (+ optional preds/in-play row for gross fallback).
 * @param {object} statsRow
 * @param {object} [inPlayRow]
 * @param {number} roundPar — regulation par for the course (typically 70–72)
 */
export function parseLiveTournamentStatsCounting(statsRow, inPlayRow, roundPar, roundNum) {
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

  const out = {
    round_score: Number.isFinite(roundScore) ? Math.round(roundScore * 10) / 10 : null,
    birdies: Number.isFinite(birdies) ? Math.round(birdies) : null,
    pars: Number.isFinite(pars) ? Math.round(pars) : null,
    bogeys: Number.isFinite(bogeys) ? Math.round(bogeys) : null,
    gir: Number.isFinite(gir) ? gir : null,
    thru: Number.isFinite(thru) ? thru : null,
    today: Number.isFinite(today) ? today : null,
    sg_putt: num(statsRow.sg_putt, NaN),
    sg_app: num(statsRow.sg_app, NaN),
    sg_arg: num(statsRow.sg_arg, NaN),
    sg_ott: num(statsRow.sg_ott, NaN),
    sg_t2g: num(statsRow.sg_t2g, NaN),
    sg_total: num(statsRow.sg_total, NaN),
  };

  const hasCounting =
    Number.isFinite(out.round_score) ||
    Number.isFinite(out.birdies) ||
    Number.isFinite(out.pars) ||
    Number.isFinite(out.bogeys);
  if (!hasCounting) return null;
  return out;
}

/**
 * @param {Record<string, unknown>} statsByRound — keys "1".."4" → API payload
 * @param {Map<number, object>} inPlayByDg
 * @param {{ roundPar?: number, maxRound?: number }} opts
 */
export function buildLiveRoundActualsByDg(statsByRound, inPlayByDg, opts = {}) {
  const roundPar = num(opts.roundPar, NaN);
  const maxRound = Math.min(4, Math.max(1, Math.round(num(opts.maxRound, 4))));
  /** @type {Record<string, Record<string, object>>} */
  const byDg = {};

  for (let rnd = 1; rnd <= maxRound; rnd++) {
    const payload = statsByRound[String(rnd)];
    const list = liveStatsList(payload);
    if (!list.length) continue;
    for (const statsRow of list) {
      const dg = Math.round(num(statsRow.dg_id ?? statsRow.dgId, NaN));
      if (!Number.isFinite(dg)) continue;
      const ip = inPlayByDg.get(dg);
      const parsed = parseLiveTournamentStatsCounting(
        statsRow,
        ip,
        Number.isFinite(roundPar) ? roundPar : 72,
        rnd,
      );
      if (!parsed) continue;
      const key = String(dg);
      if (!byDg[key]) byDg[key] = {};
      byDg[key][String(rnd)] = { ...parsed, source: "live_tournament_stats" };
    }
  }

  return byDg;
}
