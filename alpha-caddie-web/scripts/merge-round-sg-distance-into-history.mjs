/**
 * Merge approach + putting SG-by-distance CSVs onto player-history by-dg shards.
 * Used after build:round-sg-distance / build:round-sg-putt-distance on push:live.
 */
import fs from "fs";
import path from "path";
import { createReadStream } from "fs";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import {
  applyDistSgFields,
  findDistSgForRound,
  indexDistSgRows,
  num,
} from "./sg-distance-fields.mjs";
import {
  applyPuttDistSgFields,
  findPuttDistSgForRound,
  indexPuttDistSgRows,
} from "./sg-putt-distance-fields.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const APP_CSV = path.join(WEB, "data", "round_sg_by_distance.csv");
const PUTT_CSV = path.join(WEB, "data", "round_sg_putt_by_distance.csv");
const PROJ_JSON = path.join(WEB, "projections.json");

async function loadCsv(file) {
  const rows = [];
  if (!fs.existsSync(file)) return rows;
  await new Promise((res, rej) => {
    createReadStream(file)
      .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", res)
      .on("error", rej);
  });
  return rows;
}

function fieldDgIds() {
  const ids = new Set();
  if (!fs.existsSync(PROJ_JSON)) return ids;
  try {
    const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
    for (const p of proj?.players || []) {
      const dg = Math.round(num(p.dg_id, NaN));
      if (Number.isFinite(dg)) ids.add(dg);
    }
  } catch {
    /* ignore */
  }
  return ids;
}

// Default: merge onto all by-dg shards (every player). Set =1 to limit to current field.
const fieldOnly = String(process.env.GOLF_SG_DISTANCE_MERGE_FIELD_ONLY || "0").trim() === "1";
const fieldIds = fieldDgIds();

console.log("[merge-sg-distance] Loading approach + putt distance CSVs…");
const appRows = await loadCsv(APP_CSV);
const puttRows = await loadCsv(PUTT_CSV);
const appIdx = appRows.length ? indexDistSgRows(appRows) : null;
const puttIdx = puttRows.length ? indexPuttDistSgRows(puttRows) : null;
console.log(
  `  approach: ${appRows.length} rows / ${appIdx?.size ?? 0} keys; putt: ${puttRows.length} rows / ${puttIdx?.size ?? 0} keys`,
);
if (!appIdx?.size && !puttIdx?.size) {
  console.warn("[merge-sg-distance] No SG-by-distance rows — skip.");
  process.exit(0);
}

if (!fs.existsSync(SHARD_DIR)) {
  console.warn("[merge-sg-distance] Missing shard dir", SHARD_DIR);
  process.exit(0);
}

let shardsTouched = 0;
let roundsPatched = 0;
const files = fs.readdirSync(SHARD_DIR).filter((f) => f.endsWith(".json"));
for (const f of files) {
  const dg = Math.round(num(f.replace(/\.json$/i, ""), NaN));
  if (!Number.isFinite(dg)) continue;
  if (fieldOnly && fieldIds.size && !fieldIds.has(dg)) continue;

  const fp = path.join(SHARD_DIR, f);
  let shard;
  try {
    shard = JSON.parse(fs.readFileSync(fp, "utf8"));
  } catch {
    continue;
  }
  if (!Array.isArray(shard?.rounds)) continue;

  let changed = false;
  for (let i = 0; i < shard.rounds.length; i++) {
    const rr = shard.rounds[i];
    const keyRec = {
      dg_id: dg,
      year: rr.year,
      event_name: rr.event_name,
      tournament_name: rr.event_name,
      tournament_id: rr.event_id,
      event_id: rr.event_id,
      round: rr.round_num,
      round_num: rr.round_num,
    };
    const before = JSON.stringify(rr);
    if (appIdx?.size) {
      const hit = findDistSgForRound(appIdx, keyRec);
      if (hit) applyDistSgFields(rr, hit);
    }
    if (puttIdx?.size) {
      const hit = findPuttDistSgForRound(puttIdx, keyRec);
      if (hit) applyPuttDistSgFields(rr, hit);
    }
    if (JSON.stringify(rr) !== before) {
      changed = true;
      roundsPatched++;
    }
  }
  if (changed) {
    shard.updated_at = new Date().toISOString();
    fs.writeFileSync(fp, JSON.stringify(shard));
    shardsTouched++;
  }
}

console.log(
  `[merge-sg-distance] Patched ${roundsPatched} rounds across ${shardsTouched} shards` +
    (fieldOnly && fieldIds.size ? ` (field-only, ${fieldIds.size} players)` : ""),
);
