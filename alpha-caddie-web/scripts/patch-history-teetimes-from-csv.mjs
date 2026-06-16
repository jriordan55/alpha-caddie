#!/usr/bin/env node
/**
 * Copy `teetime` from historical_rounds_all.csv into player-history/by-dg/*.json rounds.
 * Run after CSV gains tee times or when shards were built before teetime was exported.
 *
 *   npm run patch:history-teetimes
 */
import fs from "fs";
import path from "path";
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { fileURLToPath } from "url";
import { resolveGolfModelDir } from "./resolve-golf-model-dir.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = resolveGolfModelDir(WEB_ROOT);
const ROUNDS_CSV =
  process.env.HISTORICAL_ROUNDS_CSV ||
  [path.join(REPO_ROOT, "data", "historical_rounds_all.csv"), path.join(WEB_ROOT, "data", "historical_rounds_all.csv")].find(
    (p) => fs.existsSync(p),
  ) ||
  path.join(REPO_ROOT, "data", "historical_rounds_all.csv");
const SHARD_DIR = path.join(WEB_ROOT, "player-history", "by-dg");

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

function roundKeys(dg, row) {
  const rn = Math.round(Number(row.round_num)) || 1;
  const y = Math.round(Number(row.year));
  const eid = String(row.event_id || "").trim();
  const evt = normEvt(row.event_name);
  const keys = [];
  if (eid) keys.push(`${dg}|eid:${eid}|${rn}`);
  if (evt && Number.isFinite(y)) keys.push(`${dg}|${evt}|${y}|${rn}`);
  return keys;
}

function shardRoundKeys(dg, r) {
  const rn = Math.round(Number(r.round_num)) || 1;
  const y = Math.round(Number(r.year));
  const eid = String(r.event_id || "").trim();
  const evt = normEvt(r.event_name);
  const keys = [];
  if (eid) keys.push(`${dg}|eid:${eid}|${rn}`);
  if (evt && Number.isFinite(y)) keys.push(`${dg}|${evt}|${y}|${rn}`);
  return keys;
}

async function loadTeetimeMap() {
  const map = new Map();
  if (!fs.existsSync(ROUNDS_CSV)) {
    throw new Error(`Missing rounds CSV: ${ROUNDS_CSV}`);
  }
  let rows = 0;
  let withTee = 0;
  const parser = createReadStream(ROUNDS_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  );
  for await (const row of parser) {
    rows++;
    const tee = String(row.teetime ?? row.tee_time ?? "").trim();
    if (!tee) continue;
    withTee++;
    const dg = Math.round(Number(row.dg_id));
    if (!Number.isFinite(dg)) continue;
    for (const k of roundKeys(dg, row)) {
      if (!map.has(k)) map.set(k, tee);
    }
  }
  console.log(`[patch:history-teetimes] CSV rows ${rows.toLocaleString()}, with teetime ${withTee.toLocaleString()}`);
  return map;
}

function patchShards(teeMap) {
  if (!fs.existsSync(SHARD_DIR)) {
    console.warn("[patch:history-teetimes] No shard dir — skip");
    return { files: 0, roundsPatched: 0 };
  }
  let files = 0;
  let roundsPatched = 0;
  for (const entry of fs.readdirSync(SHARD_DIR, { withFileTypes: true })) {
    if (!entry.isFile() || !entry.name.endsWith(".json")) continue;
    const dg = Math.round(Number(entry.name.replace(/\.json$/i, "")));
    if (!Number.isFinite(dg)) continue;
    const fp = path.join(SHARD_DIR, entry.name);
    let payload;
    try {
      payload = JSON.parse(fs.readFileSync(fp, "utf8"));
    } catch {
      continue;
    }
    const rounds = Array.isArray(payload?.rounds) ? payload.rounds : [];
    if (!rounds.length) continue;
    let changed = false;
    for (const r of rounds) {
      if (String(r?.teetime ?? "").trim()) continue;
      let tee = "";
      for (const k of shardRoundKeys(dg, r)) {
        const hit = teeMap.get(k);
        if (hit) {
          tee = hit;
          break;
        }
      }
      if (!tee) continue;
      r.teetime = tee;
      changed = true;
      roundsPatched++;
    }
    if (changed) {
      fs.writeFileSync(fp, `${JSON.stringify(payload)}\n`, "utf8");
      files++;
    }
  }
  return { files, roundsPatched };
}

async function main() {
  const teeMap = await loadTeetimeMap();
  const { files, roundsPatched } = patchShards(teeMap);
  console.log(
    `[patch:history-teetimes] Patched ${roundsPatched.toLocaleString()} round(s) across ${files.toLocaleString()} shard file(s).`,
  );
}

main().catch((e) => {
  console.error("[patch:history-teetimes]", e?.message || e);
  process.exit(1);
});
