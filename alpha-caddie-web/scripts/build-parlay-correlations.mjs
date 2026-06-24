#!/usr/bin/env node
/**
 * Empirical O/U leg co-hit rates from round_projection_vs_actual.csv for Parlay Pro.
 *   npm run build:parlay-correlations
 */
import { createReadStream, writeFileSync, existsSync } from "fs";
import { createInterface } from "readline";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const CSV = join(WEB, "data", "round_projection_vs_actual.csv");
const OUT = join(WEB, "data", "parlay_correlations.json");

const MARKET_KEYS = [
  ["round_score", "Total Score"],
  ["birdies", "Birdies"],
  ["pars", "Pars"],
  ["bogeys", "Bogeys"],
  ["gir", "GIR"],
  ["fairways", "Fairways hit"],
];

function legKey(market, side) {
  return `${market}|${side}`;
}

function pairKey(a, b) {
  return a < b ? `${a}+${b}` : `${b}+${a}`;
}

function bump(map, key, hitA, hitB, both) {
  if (!map.has(key)) map.set(key, { n: 0, hitA: 0, hitB: 0, both: 0 });
  const r = map.get(key);
  r.n++;
  if (hitA) r.hitA++;
  if (hitB) r.hitB++;
  if (both) r.both++;
}

function finalizeMap(map, minN = 12) {
  const out = {};
  for (const [k, r] of map) {
    if (r.n < minN) continue;
    const pA = r.hitA / r.n;
    const pB = r.hitB / r.n;
    const pBoth = r.both / r.n;
    const indep = pA * pB;
    const denom = Math.sqrt(pA * (1 - pA) * pB * (1 - pB));
    out[k] = {
      n: r.n,
      p_a: Math.round(pA * 1000) / 1000,
      p_b: Math.round(pB * 1000) / 1000,
      co_hit: Math.round(pBoth * 1000) / 1000,
      indep: Math.round(indep * 1000) / 1000,
      lift: indep > 1e-6 ? Math.round((pBoth / indep) * 1000) / 1000 : 1,
      rho:
        indep > 1e-6 && pA > 0.02 && pA < 0.98 && pB > 0.02 && pB < 0.98 && denom > 1e-9
          ? Math.round(((pBoth - indep) / denom) * 1000) / 1000
          : 0,
    };
  }
  return out;
}

/** Good / bad round script (matches parlay-pro.js). */
function legSentiment(market, side) {
  const over = side === "over";
  if (market === "Total Score" || market === "Bogeys") return over ? "bad" : "good";
  if (market === "Birdies" || market === "Fairways hit") return over ? "good" : "bad";
  return null;
}

function sentimentPairKey(sa, sb) {
  if (!sa || !sb) return "neutral";
  if (sa === sb) return sa === "good" ? "good_good" : "bad_bad";
  return "good_bad";
}

function parsePairKey(pk) {
  const [ka, kb] = pk.split("+");
  const [ma, sa] = ka.split("|");
  const [mb, sb] = kb.split("|");
  return { ma, sa, mb, sb };
}

function buildSentimentBuckets(...maps) {
  const acc = {
    good_good: { ws: 0, n: 0 },
    bad_bad: { ws: 0, n: 0 },
    good_bad: { ws: 0, n: 0 },
    neutral: { ws: 0, n: 0 },
  };
  for (const map of maps) {
    for (const [pk, r] of map.entries()) {
      if (r.n < 12) continue;
      const fin = finalizeMap(new Map([[pk, r]]), 12)[pk];
      if (!fin || !Number.isFinite(fin.rho)) continue;
      const { ma, sa, mb, sb } = parsePairKey(pk);
      const sk = sentimentPairKey(legSentiment(ma, sa), legSentiment(mb, sb));
      acc[sk].ws += fin.rho * r.n;
      acc[sk].n += r.n;
    }
  }
  const out = {};
  for (const [k, v] of Object.entries(acc)) {
    out[k] = {
      n: Math.round(v.n),
      rho: v.n > 0 ? Math.round((v.ws / v.n) * 1000) / 1000 : 0,
    };
  }
  return out;
}

function weightedMeanRho(map, filter) {
  let ws = 0;
  let n = 0;
  for (const [pk, r] of map.entries()) {
    if (r.n < 12) continue;
    const fin = finalizeMap(new Map([[pk, r]]), 12)[pk];
    if (!fin || !Number.isFinite(fin.rho)) continue;
    if (filter && !filter(pk, r, fin)) continue;
    ws += fin.rho * r.n;
    n += r.n;
  }
  return n > 0 ? Math.round((ws / n) * 1000) / 1000 : 0;
}

function parseRowLegs(c, idx) {
  /** @type {{ market: string, side: string, hit: boolean }[]} */
  const legs = [];
  for (const [col, label] of MARKET_KEYS) {
    for (const side of ["over", "under"]) {
      const colName = `${col}_${side}`;
      const i = idx[colName];
      if (i === undefined) continue;
      const res = String(c[i] || "").trim().toUpperCase();
      if (res !== "W" && res !== "L") continue;
      legs.push({ market: label, side, hit: res === "W" });
    }
  }
  return legs;
}

function bumpCrossPairs(hits, map) {
  for (let i = 0; i < hits.length; i++) {
    for (let j = i + 1; j < hits.length; j++) {
      if (hits[i].playerKey === hits[j].playerKey) continue;
      const a = legKey(hits[i].market, hits[i].side);
      const b = legKey(hits[j].market, hits[j].side);
      bump(map, pairKey(a, b), hits[i].hit, hits[j].hit, hits[i].hit && hits[j].hit);
    }
  }
}

async function readCsvRows() {
  if (!existsSync(CSV)) {
    console.error("[build:parlay-correlations] missing", CSV);
    process.exit(1);
  }
  /** @type {Record<string, number>} */
  const idx = {};
  /** @type {{ eventRound: string, waveKey: string, dgId: string, legs: { market: string, side: string, hit: boolean }[] }[]} */
  const rows = [];
  const rl = createInterface({ input: createReadStream(CSV), crlfDelay: Infinity });
  let header = false;
  for await (const line of rl) {
    if (!header) {
      line.split(",").forEach((h, i) => {
        idx[h] = i;
      });
      header = true;
      continue;
    }
    if (!line.trim()) continue;
    const c = line.split(",");
    const wave = String(c[idx.tee_wave] || "").trim().toLowerCase();
    const eventRound = `${c[idx.event_name]}|${c[idx.round]}`;
    const waveKey = wave ? `${eventRound}|${wave}` : "";
    const dgId = String(c[idx.dg_id] || "");
    const legs = parseRowLegs(c, idx);
    if (legs.length < 1) continue;
    rows.push({ eventRound, waveKey, dgId, legs });
  }
  return rows;
}

async function main() {
  const rows = await readCsvRows();
  const samePlayer = new Map();
  const sameWave = new Map();
  const sameRound = new Map();

  for (const row of rows) {
    const { legs } = row;
    if (legs.length < 2) continue;
    for (let i = 0; i < legs.length; i++) {
      for (let j = i + 1; j < legs.length; j++) {
        const a = legKey(legs[i].market, legs[i].side);
        const b = legKey(legs[j].market, legs[j].side);
        bump(samePlayer, pairKey(a, b), legs[i].hit, legs[j].hit, legs[i].hit && legs[j].hit);
      }
    }
  }

  const waveHits = new Map();
  const roundHits = new Map();
  for (const row of rows) {
    if (!row.waveKey) continue;
    const playerKey = `${row.waveKey}|${row.dgId}`;
    const tagged = row.legs.map((l) => ({ ...l, playerKey }));
    if (!waveHits.has(row.waveKey)) waveHits.set(row.waveKey, []);
    waveHits.get(row.waveKey).push(...tagged);
    if (!roundHits.has(row.eventRound)) roundHits.set(row.eventRound, []);
    roundHits.get(row.eventRound).push(...tagged);
  }

  for (const hits of waveHits.values()) bumpCrossPairs(hits, sameWave);
  for (const hits of roundHits.values()) bumpCrossPairs(hits, sameRound);

  const sentiment_buckets = buildSentimentBuckets(samePlayer, sameWave, sameRound);

  const default_rho = {
    same_player_same_market: weightedMeanRho(samePlayer, (pk) => {
      const { ma, mb } = parsePairKey(pk);
      return ma === mb;
    }),
    same_player_cross_market: weightedMeanRho(samePlayer, (pk) => {
      const { ma, mb } = parsePairKey(pk);
      return ma !== mb;
    }),
    same_wave_same_market: weightedMeanRho(sameWave, (pk) => {
      const { ma, sa, mb, sb } = parsePairKey(pk);
      return ma === mb && sa === sb;
    }),
    same_round_same_market: weightedMeanRho(sameRound, (pk) => {
      const { ma, sa, mb, sb } = parsePairKey(pk);
      return ma === mb && sa === sb;
    }),
    cross_market_good_good: sentiment_buckets.good_good?.rho ?? 0,
    cross_market_bad_bad: sentiment_buckets.bad_bad?.rho ?? 0,
    cross_market_good_bad: sentiment_buckets.good_bad?.rho ?? 0,
    cross_market_neutral: sentiment_buckets.neutral?.rho ?? 0,
  };

  const payload = {
    generated_at: new Date().toISOString(),
    source: "round_projection_vs_actual.csv",
    rows_scored: rows.filter((r) => r.legs.length >= 2).length,
    same_player: finalizeMap(samePlayer),
    same_tee_wave: finalizeMap(sameWave),
    same_round: finalizeMap(sameRound),
    sentiment_buckets,
    default_rho,
  };

  writeFileSync(OUT, `${JSON.stringify(payload, null, 2)}\n`);
  console.log(
    `[build:parlay-correlations] OK — ${payload.rows_scored} player-rounds; ` +
      `${Object.keys(payload.same_player).length} same-player pairs; ` +
      `${Object.keys(payload.same_tee_wave).length} same-wave pairs; ` +
      `${Object.keys(payload.same_round).length} same-round pairs → ${OUT}`,
  );
  console.log(
    `  sentiment ρ: good↔good ${sentiment_buckets.good_good.rho} (n=${sentiment_buckets.good_good.n}) ` +
      `bad↔bad ${sentiment_buckets.bad_bad.rho} good↔bad ${sentiment_buckets.good_bad.rho}`,
  );
}

main().catch((e) => {
  console.error("[build:parlay-correlations]", e?.message || e);
  process.exit(1);
});
