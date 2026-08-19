#!/usr/bin/env node
/**
 * Build prior-round field rank lookup for graded bets UI.
 *   node scripts/bake-prior-round-field-ranks.mjs
 *   → data/prior_round_field_ranks.json
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import {
  RANK_CUTOFFS,
  buildBetRankLookupIndex,
  buildPriorRoundFieldRankIndex,
} from "./prior-round-field-ranks.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const HIST = join(WEB, "data", "historical_rounds_all.csv");
const BETS = join(WEB, "data", "both_side_bets.json");
const OUT = join(WEB, "data", "prior_round_field_ranks.json");

async function main() {
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);
  const rankIndex = await buildPriorRoundFieldRankIndex(HIST);
  /** @type {object[]} */
  let bets = [];
  if (existsSync(BETS)) {
    try {
      bets = JSON.parse(readFileSync(BETS, "utf8"))?.bets || [];
    } catch {
      bets = [];
    }
  }
  const index = buildBetRankLookupIndex(rankIndex, bets);
  const out = {
    generated_at: new Date().toISOString(),
    source: "historical_rounds_all.csv",
    metrics: {
      app: "SG approach (prior round field rank)",
      putt: "SG putting",
      fw: "Fairway hit %",
      gir: "GIR %",
      bob: "BoB %",
    },
    cutoffs: RANK_CUTOFFS,
    n_keys: Object.keys(index).length,
    index,
  };
  writeFileSync(OUT, JSON.stringify(out, null, 2));
  console.log(`Wrote ${OUT} (${out.n_keys} player-round rank entries)`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
