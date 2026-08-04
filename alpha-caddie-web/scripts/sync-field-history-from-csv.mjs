#!/usr/bin/env node
/**
 * Merge historical_rounds_all.csv rows into field-player shards (adds completed tournaments).
 * Fast path for push:live when full build:history is skipped.
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";
import { parse } from "csv-parse";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const PROJ_JSON = path.join(WEB, "projections.json");
const ROUNDS_CSV = path.join(REPO, "data", "historical_rounds_all.csv");

const MIN_YEAR = (() => {
  const env = parseInt(String(process.env.GOLF_HISTORY_MIN_YEAR ?? "").trim(), 10);
  if (Number.isFinite(env) && env >= 1990) return env;
  return 2004;
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

function parseUsSortKey(mdy, rnd) {
  const m = String(mdy || "").match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  if (!m) return 0;
  const y = +m[3];
  const mo = +m[1];
  const d = +m[2];
  return (y * 10000 + mo * 100 + d) * 10 + (rnd || 1);
}

function csvRowToHistoryRec(row) {
  const dg = Math.round(num(row.dg_id, NaN));
  const rnd = parseInt(String(row.round_num || "1"), 10) || 1;
  const eventDate = String(row.event_completed || "").trim();
  const yr = parseInt(String(row.year || ""), 10);
  return {
    dg_id: dg,
    player_name: String(row.player_name || "").trim(),
    sortKey: parseUsSortKey(eventDate, rnd),
    event_completed: eventDate,
    year: Number.isFinite(yr) ? yr : new Date().getFullYear(),
    event_name: String(row.event_name || "").trim(),
    event_id: String(row.event_id || ""),
    course_name: String(row.course_name || row.event_name || "").trim(),
    round_num: rnd,
    fin_text: String(row.fin_text || ""),
    round_score: num(row.round_score),
    birdies: num(row.birdies),
    pars: num(row.pars),
    bogies: num(row.bogeys ?? row.bogies),
    gir: num(row.gir),
    fairways: num(row.fairways),
    putts: num(row.putts),
    eagles_or_better: num(row.eagles_or_better),
    doubles_or_worse: num(row.doubles_or_worse),
    sg_putt: num(row.sg_putt),
    sg_app: num(row.sg_app),
    sg_arg: num(row.sg_arg),
    sg_ott: num(row.sg_ott),
    sg_t2g: num(row.sg_t2g),
    sg_total: num(row.sg_total),
    sg_50_100_fw: num(row.sg_50_100_fw),
    sg_100_150_fw: num(row.sg_100_150_fw),
    sg_150_200_fw: num(row.sg_150_200_fw),
    sg_over_200_fw: num(row.sg_over_200_fw),
    sg_under_150_rgh: num(row.sg_under_150_rgh),
    sg_over_150_rgh: num(row.sg_over_150_rgh),
    sg_app_dist_total: num(row.sg_app_dist_total),
    n_50_100_fw: num(row.n_50_100_fw),
    n_100_150_fw: num(row.n_100_150_fw),
    n_150_200_fw: num(row.n_150_200_fw),
    n_over_200_fw: num(row.n_over_200_fw),
    n_under_150_rgh: num(row.n_under_150_rgh),
    n_over_150_rgh: num(row.n_over_150_rgh),
    n_app_dist: num(row.n_app_dist),
    sg_putt_0_5ft: num(row.sg_putt_0_5ft),
    sg_putt_5_10ft: num(row.sg_putt_5_10ft),
    sg_putt_10_15ft: num(row.sg_putt_10_15ft),
    sg_putt_15_25ft: num(row.sg_putt_15_25ft),
    sg_putt_25plus_ft: num(row.sg_putt_25plus_ft),
    sg_putt_dist_total: num(row.sg_putt_dist_total),
    n_putt_0_5ft: num(row.n_putt_0_5ft),
    n_putt_5_10ft: num(row.n_putt_5_10ft),
    n_putt_10_15ft: num(row.n_putt_10_15ft),
    n_putt_15_25ft: num(row.n_putt_15_25ft),
    n_putt_25plus_ft: num(row.n_putt_25plus_ft),
    n_putt_dist: num(row.n_putt_dist),
    _from_dg_historical_rounds: true,
  };
}

function roundDedupeKey(r) {
  const yr = parseInt(String(r?.year || ""), 10);
  const rnd = Math.round(num(r?.round_num, NaN));
  return `${yr}|${normEvt(r?.event_name)}|${rnd}`;
}

function fieldDgIds(proj) {
  const ids = new Set();
  for (const p of proj?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(dg)) ids.add(dg);
  }
  return ids;
}

async function loadCsvRowsForField(fieldIds) {
  /** @type {Map<number, object[]>} */
  const byDg = new Map();
  if (!fs.existsSync(ROUNDS_CSV)) return byDg;
  await new Promise((resolve, reject) => {
    const parser = createReadStream(ROUNDS_CSV).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    parser.on("data", (row) => {
      const tour = String(row.tour || "").toLowerCase();
      if (tour !== "pga" && tour !== "liv") return;
      const yr = parseInt(row.year, 10);
      if (Number.isFinite(yr) && yr < MIN_YEAR) return;
      const id = Math.round(num(row.dg_id, NaN));
      if (!fieldIds.has(id)) return;
      const rs = num(row.round_score);
      if (!Number.isFinite(rs) || rs <= 0) return;
      const rec = csvRowToHistoryRec(row);
      if (!byDg.has(id)) byDg.set(id, []);
      byDg.get(id).push(rec);
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });
  for (const rows of byDg.values()) {
    rows.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  }
  return byDg;
}

const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
const fieldIds = fieldDgIds(proj);

if (!fs.existsSync(ROUNDS_CSV)) {
  console.warn("[sync-field-history] No CSV — skipping:", ROUNDS_CSV);
  process.exit(0);
}

const csvByDg = await loadCsvRowsForField(fieldIds);
let patched = 0;
let roundsAdded = 0;

for (const dg of fieldIds) {
  const csvRows = csvByDg.get(dg);
  if (!csvRows?.length) continue;
  const shardPath = path.join(SHARD_DIR, `${dg}.json`);
  /** @type {{ dg_id: number, player_name: string, rounds: object[] }} */
  let shard = {
    dg_id: dg,
    player_name: String(csvRows[csvRows.length - 1]?.player_name || "").trim(),
    rounds: [],
  };
  if (fs.existsSync(shardPath)) {
    try {
      shard = JSON.parse(fs.readFileSync(shardPath, "utf8"));
      if (!Array.isArray(shard.rounds)) shard.rounds = [];
    } catch {
      shard.rounds = [];
    }
  }
  const index = new Map();
  for (let i = 0; i < shard.rounds.length; i++) {
    index.set(roundDedupeKey(shard.rounds[i]), i);
  }
  let added = 0;
  for (const rec of csvRows) {
    const key = roundDedupeKey(rec);
    const hit = index.get(key);
    if (hit !== undefined) {
      shard.rounds[hit] = { ...shard.rounds[hit], ...rec };
    } else {
      index.set(key, shard.rounds.length);
      shard.rounds.push(rec);
      added += 1;
    }
  }
  if (!added && shard.rounds.length === index.size) continue;
  shard.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  if (!shard.player_name) {
    shard.player_name = String(csvRows[csvRows.length - 1]?.player_name || "").trim();
  }
  fs.mkdirSync(SHARD_DIR, { recursive: true });
  fs.writeFileSync(shardPath, JSON.stringify(shard));
  patched += 1;
  roundsAdded += added;
}

console.log(
  `[sync-field-history] Patched ${patched} field shard(s) from CSV (+${roundsAdded} new round row(s)).`,
);
