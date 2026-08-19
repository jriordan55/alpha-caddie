/**
 * Prior in-event round field ranks (higher metric = rank 1).
 * Used for graded-bet UI filters and sg-rank-cutoff backtests.
 */
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { fairwayPctFromHistRow, girPctFromHistRow, bobPctFromHistRow } from "./sg-side-policy.mjs";
import { yearFromEventCompleted } from "./prior-round-context.mjs";

export const RANK_CUTOFFS = [5, 10, 15, 20, 25, 30, 35, 40, 45, 50];

/** UI metric id → rank record key */
export const RANK_METRIC_KEYS = Object.freeze({
  app: "app",
  putt: "putt",
  fw: "fw",
  gir: "gir",
  bob: "bob",
});

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

export function fieldRoundKey(event, year, roundNum) {
  return `${String(event).trim()}\x1f${Math.round(year)}\x1f${Math.round(roundNum)}`;
}

export function betRankLookupKey(event, dgId, roundNum) {
  return `${String(event).trim()}|${Math.round(num(dgId, NaN))}|${Math.round(num(roundNum, NaN))}`;
}

/**
 * @returns {Promise<Map<string, Map<number, { app: number, putt: number, fw: number, gir: number, bob: number, field: number }>>>}
 */
export async function buildPriorRoundFieldRankIndex(histPath) {
  /** @type {Map<string, { dg: number, sg_app: number, sg_putt: number, fw_pct: number, gir_pct: number, bob_pct: number }[]>} */
  const buckets = new Map();

  await new Promise((resolve, reject) => {
    createReadStream(histPath)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        const ev = String(row.event_name || "").trim();
        const rnd = Math.round(num(row.round_num, NaN));
        const dg = Math.round(num(row.dg_id, NaN));
        const score = num(row.round_score, NaN);
        if (!ev || !Number.isFinite(rnd) || !Number.isFinite(dg) || !Number.isFinite(score)) return;
        const yr = Math.round(num(row.year, NaN)) || yearFromEventCompleted(row.event_completed);
        if (!Number.isFinite(yr)) return;
        const k = fieldRoundKey(ev, yr, rnd);
        let arr = buckets.get(k);
        if (!arr) {
          arr = [];
          buckets.set(k, arr);
        }
        arr.push({
          dg,
          sg_app: num(row.sg_app, NaN),
          sg_putt: num(row.sg_putt, NaN),
          fw_pct: fairwayPctFromHistRow(row),
          gir_pct: girPctFromHistRow(row),
          bob_pct: bobPctFromHistRow(row),
        });
      })
      .on("end", resolve)
      .on("error", reject);
  });

  /** @type {Map<string, Map<number, object>>} */
  const index = new Map();

  for (const [k, players] of buckets) {
    const field = players.length;
    const rankMetric = (key) => {
      const vals = players.filter((p) => Number.isFinite(p[key])).sort((a, b) => b[key] - a[key]);
      const rankByDg = new Map();
      vals.forEach((p, i) => rankByDg.set(p.dg, i + 1));
      return rankByDg;
    };
    const appR = rankMetric("sg_app");
    const puttR = rankMetric("sg_putt");
    const fwR = rankMetric("fw_pct");
    const girR = rankMetric("gir_pct");
    const bobR = rankMetric("bob_pct");
    const byDg = new Map();
    for (const p of players) {
      byDg.set(p.dg, {
        app: appR.get(p.dg) ?? NaN,
        putt: puttR.get(p.dg) ?? NaN,
        fw: fwR.get(p.dg) ?? NaN,
        gir: girR.get(p.dg) ?? NaN,
        bob: bobR.get(p.dg) ?? NaN,
        field,
      });
    }
    index.set(k, byDg);
  }
  return index;
}

/** Ranks in the prior in-event round for a betting row (round N uses field from round N−1). */
export function priorFieldRanksForBet(rankIndex, event, year, dgId, bettingRound) {
  const prevRnd = Math.round(num(bettingRound, NaN)) - 1;
  if (prevRnd < 1 || !Number.isFinite(year)) return null;
  const byDg = rankIndex.get(fieldRoundKey(event, year, prevRnd));
  if (!byDg) return null;
  return byDg.get(Math.round(num(dgId, NaN))) || null;
}

export function yearFromBetRow(b) {
  const y = Math.round(num(b?.year, NaN));
  if (Number.isFinite(y)) return y;
  const d = Date.parse(String(b?.date || ""));
  if (Number.isFinite(d)) return new Date(d).getUTCFullYear();
  const t = Number(b?.ts);
  if (Number.isFinite(t) && t > 0) return new Date(t).getUTCFullYear();
  const m = String(b?.event || "").match(/(\d{4})/);
  return m ? Number(m[1]) : NaN;
}

/** @returns {Record<string, object>} lookup key → ranks for betting round */
export function buildBetRankLookupIndex(rankIndex, bets) {
  /** @type {Record<string, object>} */
  const out = {};
  for (const b of bets) {
    const k = betRankLookupKey(b.event, b.dg_id, b.round);
    if (out[k]) continue;
    const ranks = priorFieldRanksForBet(rankIndex, b.event, yearFromBetRow(b), b.dg_id, b.round);
    if (ranks) out[k] = ranks;
  }
  return out;
}

export function rankPassesTopN(ranks, metricKey, topN) {
  const n = Math.round(num(topN, NaN));
  if (!Number.isFinite(n) || n < 1) return true;
  if (!ranks || typeof ranks !== "object") return false;
  const rank = num(ranks[metricKey], NaN);
  return Number.isFinite(rank) && rank <= n;
}

export function rankPassesBottomN(ranks, metricKey, bottomN) {
  const n = Math.round(num(bottomN, NaN));
  if (!Number.isFinite(n) || n < 1) return true;
  if (!ranks || typeof ranks !== "object") return false;
  const field = Math.round(num(ranks.field, NaN));
  const rank = num(ranks[metricKey], NaN);
  if (!Number.isFinite(field) || !Number.isFinite(rank)) return false;
  const bottomStart = Math.max(1, field - n + 1);
  return rank >= bottomStart;
}
