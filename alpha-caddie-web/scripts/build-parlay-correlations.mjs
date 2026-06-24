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

function finalizeMap(map) {
  const out = {};
  for (const [k, r] of map) {
    if (r.n < 12) continue;
    const pA = r.hitA / r.n;
    const pB = r.hitB / r.n;
    const pBoth = r.both / r.n;
    const indep = pA * pB;
    out[k] = {
      n: r.n,
      p_a: Math.round(pA * 1000) / 1000,
      p_b: Math.round(pB * 1000) / 1000,
      co_hit: Math.round(pBoth * 1000) / 1000,
      indep: Math.round(indep * 1000) / 1000,
      lift: indep > 1e-6 ? Math.round((pBoth / indep) * 1000) / 1000 : 1,
      rho:
        indep > 1e-6 && pA > 0.02 && pA < 0.98 && pB > 0.02 && pB < 0.98
          ? Math.round(((pBoth - indep) / Math.sqrt(pA * (1 - pA) * pB * (1 - pB))) * 1000) / 1000
          : 0,
    };
  }
  return out;
}

async function main() {
  if (!existsSync(CSV)) {
    console.error("[build:parlay-correlations] missing", CSV);
    process.exit(1);
  }

  let header = null;
  const idx = {};
  const samePlayer = new Map();
  const sameWave = new Map();
  let rowN = 0;

  const rl = createInterface({ input: createReadStream(CSV), crlfDelay: Infinity });
  for await (const line of rl) {
    if (!header) {
      header = line.split(",");
      header.forEach((h, i) => {
        idx[h] = i;
      });
      continue;
    }
    if (!line.trim()) continue;
    const c = line.split(",");
    const wave = String(c[idx.tee_wave] || "").trim().toLowerCase();
    const eventRound = `${c[idx.event_name]}|${c[idx.round]}`;
    const waveKey = wave ? `${eventRound}|${wave}` : "";

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
    if (legs.length < 2) continue;
    rowN++;

    for (let i = 0; i < legs.length; i++) {
      for (let j = i + 1; j < legs.length; j++) {
        const a = legKey(legs[i].market, legs[i].side);
        const b = legKey(legs[j].market, legs[j].side);
        const pk = pairKey(a, b);
        const both = legs[i].hit && legs[j].hit;
        bump(samePlayer, pk, legs[i].hit, legs[j].hit, both);
      }
    }

    if (!waveKey) continue;
    // cross-player same wave: aggregate at wave bucket level in second pass — use row as one player
    // store legs on wave for merging — simplified: only same-player for wave-specific we use tee_wave tag on leg pairs where same row is trivial; for cross-player need wave-level store
  }

  // Second pass for same-wave cross-player: re-read with player grouping
  const waveLegsByKey = new Map();
  const rl2 = createInterface({ input: createReadStream(CSV), crlfDelay: Infinity });
  let header2 = null;
  for await (const line of rl2) {
    if (!header2) {
      header2 = line.split(",");
      continue;
    }
    if (!line.trim()) continue;
    const c = line.split(",");
    const wave = String(c[idx.tee_wave] || "").trim().toLowerCase();
    if (!wave) continue;
    const waveKey = `${c[idx.event_name]}|${c[idx.round]}|${wave}`;
    const dg = c[idx.dg_id];
    const playerKey = `${waveKey}|${dg}`;

    const hits = [];
    for (const [col, label] of MARKET_KEYS) {
      for (const side of ["over", "under"]) {
        const colName = `${col}_${side}`;
        const i = idx[colName];
        if (i === undefined) continue;
        const res = String(c[i] || "").trim().toUpperCase();
        if (res !== "W" && res !== "L") continue;
        hits.push({ market: label, side, hit: res === "W", playerKey });
      }
    }
    if (!waveLegsByKey.has(waveKey)) waveLegsByKey.set(waveKey, []);
    waveLegsByKey.get(waveKey).push(...hits);
  }

  for (const hits of waveLegsByKey.values()) {
    for (let i = 0; i < hits.length; i++) {
      for (let j = i + 1; j < hits.length; j++) {
        if (hits[i].playerKey === hits[j].playerKey) continue;
        const a = legKey(hits[i].market, hits[i].side);
        const b = legKey(hits[j].market, hits[j].side);
        const pk = pairKey(a, b);
        bump(sameWave, pk, hits[i].hit, hits[j].hit, hits[i].hit && hits[j].hit);
      }
    }
  }

  const payload = {
    generated_at: new Date().toISOString(),
    source: "round_projection_vs_actual.csv",
    rows_scored: rowN,
    same_player: finalizeMap(samePlayer),
    same_tee_wave: finalizeMap(sameWave),
    default_rho: {
      same_player_same_market: 0.55,
      same_player_cross_market: 0.28,
      same_wave_same_market: 0.18,
      same_wave_cross_market: 0.1,
      different_wave: 0.04,
      different_player_diff_round: 0.02,
    },
  };

  writeFileSync(OUT, `${JSON.stringify(payload, null, 2)}\n`);
  console.log(
    `[build:parlay-correlations] OK — ${rowN} player-rounds; ` +
      `${Object.keys(payload.same_player).length} same-player pairs; ` +
      `${Object.keys(payload.same_tee_wave).length} same-wave pairs → ${OUT}`,
  );
}

main().catch((e) => {
  console.error("[build:parlay-correlations]", e?.message || e);
  process.exit(1);
});
