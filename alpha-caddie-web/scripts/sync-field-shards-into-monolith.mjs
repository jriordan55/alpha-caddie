#!/usr/bin/env node
/**
 * Optional: upsert recent field-player rounds from shards into player_round_history.json.
 * Default push:live does NOT run this — merging full career shards can push the monolith
 * past GitHub's 100 MB limit. Live publish uses field by-dg shards + in-browser live merge.
 *
 * Set GOLF_SYNC_FIELD_SHARDS_INTO_MONOLITH=1 to opt in (caps rounds per player via
 * GOLF_MONOLITH_SYNC_MAX_ROUNDS, default 48).
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const MONOLITH = path.join(WEB, "player_round_history.json");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const PROJ_JSON = path.join(WEB, "projections.json");

const MAX_ROUNDS = (() => {
  const n = parseInt(String(process.env.GOLF_MONOLITH_SYNC_MAX_ROUNDS ?? "48").trim(), 10);
  return Number.isFinite(n) && n > 0 ? Math.min(n, 200) : 48;
})();

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function roundIdentity(r) {
  const dg = Math.round(num(r?.dg_id, NaN));
  const yr = parseInt(String(r?.year || ""), 10);
  const rnd = Math.round(num(r?.round_num, NaN));
  return `${dg}|${yr}|${normEvt(r?.event_name)}|${rnd}`;
}

function writeJsonAtomic(outPath, payload) {
  const tmpPath = `${outPath}.tmp`;
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(tmpPath, JSON.stringify(payload), "utf8");
  try {
    fs.renameSync(tmpPath, outPath);
  } catch {
    fs.writeFileSync(outPath, JSON.stringify(payload), "utf8");
    try {
      fs.unlinkSync(tmpPath);
    } catch {
      /* ignore */
    }
  }
}

function fieldDgIds(proj) {
  const ids = new Set();
  for (const p of proj?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(dg) && dg > 0) ids.add(dg);
  }
  return ids;
}

function upsertRounds(existingRounds, incomingRounds) {
  /** @type {Map<string, object>} */
  const byKey = new Map();
  for (const r of existingRounds) byKey.set(roundIdentity(r), r);
  for (const r of incomingRounds) {
    const k = roundIdentity(r);
    const prev = byKey.get(k);
    byKey.set(k, prev && typeof prev === "object" ? { ...prev, ...r } : r);
  }
  const merged = [...byKey.values()];
  merged.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  return merged;
}

function recentShardRounds(shardRounds) {
  const sorted = [...shardRounds].sort((a, b) => num(b.sortKey, 0) - num(a.sortKey, 0));
  return sorted.slice(0, MAX_ROUNDS);
}

if (String(process.env.GOLF_SYNC_FIELD_SHARDS_INTO_MONOLITH || "").trim() !== "1") {
  console.log("[sync-field-monolith] Skipped (set GOLF_SYNC_FIELD_SHARDS_INTO_MONOLITH=1 to opt in).");
  process.exit(0);
}

if (!fs.existsSync(PROJ_JSON)) {
  console.warn("[sync-field-monolith] No projections.json — skip");
  process.exit(0);
}

const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
const ids = fieldDgIds(proj);
if (!ids.size) {
  console.log("[sync-field-monolith] Empty field — skip");
  process.exit(0);
}

/** @type {{ meta?: object, byDgId?: Record<string, object>, _ok?: boolean, _partial?: boolean }} */
let monolith = { meta: {}, byDgId: {}, _ok: true, _partial: false };
if (fs.existsSync(MONOLITH)) {
  try {
    monolith = JSON.parse(fs.readFileSync(MONOLITH, "utf8"));
  } catch (e) {
    console.warn(`[sync-field-monolith] Could not parse monolith — starting fresh shell (${e?.message || e})`);
    monolith = { meta: {}, byDgId: {}, _ok: true, _partial: true };
  }
}
if (!monolith.byDgId || typeof monolith.byDgId !== "object") monolith.byDgId = {};

let playersPatched = 0;
let roundsUpserted = 0;

for (const dg of ids) {
  const shardPath = path.join(SHARD_DIR, `${dg}.json`);
  if (!fs.existsSync(shardPath)) continue;
  let shard;
  try {
    shard = JSON.parse(fs.readFileSync(shardPath, "utf8"));
  } catch {
    continue;
  }
  const shardRounds = Array.isArray(shard?.rounds) ? shard.rounds : [];
  if (!shardRounds.length) continue;

  const key = String(dg);
  const prev = monolith.byDgId[key];
  const prevRounds = Array.isArray(prev?.rounds) ? prev.rounds : [];
  const incoming = recentShardRounds(shardRounds);
  const mergedRounds = upsertRounds(prevRounds, incoming);
  roundsUpserted += Math.max(0, mergedRounds.length - prevRounds.length);

  monolith.byDgId[key] = {
    dg_id: dg,
    player_name:
      String(shard.player_name || prev?.player_name || "").trim() ||
      String([...(proj?.players || [])].find((p) => Math.round(num(p.dg_id, NaN)) === dg)?.player_name || "").trim(),
    rounds: mergedRounds,
  };
  playersPatched += 1;
}

monolith.meta = {
  ...(monolith.meta && typeof monolith.meta === "object" ? monolith.meta : {}),
  updated_at: new Date().toISOString(),
  field_shard_sync_at: new Date().toISOString(),
};
monolith._ok = true;

writeJsonAtomic(MONOLITH, monolith);
console.log(
  `[sync-field-monolith] Patched ${playersPatched} field player(s) into player_round_history.json (~+${roundsUpserted} round row(s), max ${MAX_ROUNDS} recent/shard).`,
);
