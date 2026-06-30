#!/usr/bin/env node
/**
 * Fast path when full build:history was skipped: write per-player shards for projection-field
 * dg_ids that have rows in historical_rounds_all.csv but no shard yet (real CSV data only).
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
const MANIFEST_JSON = path.join(WEB, "player-history", "manifest.json");
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
    _from_dg_historical_rounds: true,
  };
}

function fieldDgIds(proj) {
  const ids = new Set();
  for (const p of proj?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(dg)) ids.add(dg);
  }
  return ids;
}

async function loadCsvRowsForDg(dg) {
  /** @type {object[]} */
  const rows = [];
  if (!fs.existsSync(ROUNDS_CSV)) return rows;
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
      if (id !== dg) return;
      const rs = num(row.round_score);
      if (!Number.isFinite(rs) || rs <= 0) return;
      rows.push(csvRowToHistoryRec(row));
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });
  rows.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  return rows;
}

const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
const fieldIds = fieldDgIds(proj);
const missing = [...fieldIds].filter((dg) => !fs.existsSync(path.join(SHARD_DIR, `${dg}.json`)));

if (!missing.length) {
  console.log("[sync-missing-field-history] All field players already have shards.");
  process.exit(0);
}

if (!fs.existsSync(ROUNDS_CSV)) {
  console.warn("[sync-missing-field-history] No CSV — run update:rounds / fetch:dg first:", ROUNDS_CSV);
  process.exit(0);
}

let built = 0;
let roundCount = 0;
/** @type {{ dg_id: number, player_name: string, rounds: number }[]} */
const manifestAdds = [];

for (const dg of missing) {
  const rounds = await loadCsvRowsForDg(dg);
  if (!rounds.length) continue;
  const playerName =
    rounds[rounds.length - 1]?.player_name ||
    String([...(proj?.players || [])].find((p) => Math.round(num(p.dg_id, NaN)) === dg)?.player_name || "").trim();
  fs.mkdirSync(SHARD_DIR, { recursive: true });
  fs.writeFileSync(
    path.join(SHARD_DIR, `${dg}.json`),
    JSON.stringify({ dg_id: dg, player_name: playerName, rounds }),
  );
  built += 1;
  roundCount += rounds.length;
  manifestAdds.push({ dg_id: dg, player_name: playerName, rounds: rounds.length });
}

if (built && fs.existsSync(MANIFEST_JSON)) {
  try {
    const manifest = JSON.parse(fs.readFileSync(MANIFEST_JSON, "utf8"));
    const players = Array.isArray(manifest.players) ? [...manifest.players] : [];
    const have = new Set(players.map((p) => Math.round(num(p.dg_id, NaN))));
    for (const p of manifestAdds) {
      if (!have.has(p.dg_id)) players.push(p);
    }
    players.sort((a, b) => String(a.player_name).localeCompare(String(b.player_name)));
    manifest.players = players;
    manifest.meta = { ...(manifest.meta || {}), updated_at: new Date().toISOString() };
    fs.writeFileSync(MANIFEST_JSON, JSON.stringify(manifest));
  } catch {
    /* non-fatal */
  }
}

console.log(
  `[sync-missing-field-history] Built ${built} shard(s) from CSV (${roundCount} round row(s)); ${missing.length - built} field player(s) still have no CSV rows.`,
);
