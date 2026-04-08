#!/usr/bin/env node
/**
 * Streams data/all_shots_2021_2026.csv into player_shots_web.json for the static app.
 * - Drops PGA seasons before 2022 (skips R2021… tournament_ids — removes 2021 shot rows).
 * - Keeps shot rows only for the last SHOT_ROUND_TAIL rounds per projected player (default 14;
 *   override with env SHOT_ROUND_TAIL=4–28). Narrower than the hole CSV tail keeps JSON size down.
 * - Maps pga player_id → dg_id via data/pga_datagolf_player_map.csv.
 *
 * Env: GOLF_MODEL_DIR (repo root). Run from alpha-caddie-web: npm run build:shots-web
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";
import { parse } from "csv-parse";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = process.env.GOLF_MODEL_DIR
  ? path.resolve(process.env.GOLF_MODEL_DIR)
  : path.resolve(WEB_ROOT, "..");

function resolveRoundsCsvPath() {
  if (process.env.HISTORICAL_ROUNDS_CSV) return path.resolve(process.env.HISTORICAL_ROUNDS_CSV);
  const canonical = path.join(MODEL_ROOT, "data", "historical_rounds_all.csv");
  if (fs.existsSync(canonical)) return canonical;
  const inWebData = path.join(WEB_ROOT, "data", "historical_rounds_all.csv");
  if (fs.existsSync(inWebData)) return inWebData;
  const inRoot = path.join(MODEL_ROOT, "historical_rounds_all.csv");
  if (fs.existsSync(inRoot)) return inRoot;
  return canonical;
}

const ROUNDS_CSV = resolveRoundsCsvPath();
const SHOTS_CSV = path.join(MODEL_ROOT, "data", "all_shots_2021_2026.csv");
const PLAYER_MAP_CSV = path.join(MODEL_ROOT, "data", "pga_datagolf_player_map.csv");
const PROJECTIONS_JSON = path.join(WEB_ROOT, "projections.json");
const OUT_JSON = path.join(WEB_ROOT, "player_shots_web.json");

const MIN_YEAR = new Date().getFullYear() - 4;
const MAX_ROUNDS_PER_PLAYER = 200;
/** Last N historical rounds per player eligible for shot rows (smaller than hole CSV tail keeps JSON fetchable). */
const SHOT_ROUND_TAIL = Number.isFinite(Number(process.env.SHOT_ROUND_TAIL))
  ? Math.max(4, Math.min(28, Math.round(Number(process.env.SHOT_ROUND_TAIL))))
  : 14;
const MIN_SHOT_SEASON_YEAR = 2022;
const PLAY_BY_PLAY_MAX = 72;

function num(x) {
  const n = Number(x);
  return Number.isFinite(n) ? n : NaN;
}

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function playerKeyHistorical(name) {
  const s = String(name || "").trim();
  const m = s.match(/^(.+),\s*(.+)$/);
  if (m) return `${m[1].trim().toLowerCase()}|${m[2].trim().toLowerCase()}`;
  return s.toLowerCase();
}

function parseUsDateSortKey(s) {
  if (!s) return 0;
  const p = String(s).split("/");
  if (p.length !== 3) return 0;
  const mo = parseInt(p[0], 10);
  const d = parseInt(p[1], 10);
  const y = parseInt(p[2], 10);
  if (!Number.isFinite(y)) return 0;
  return y * 10000 + (mo || 0) * 100 + (d || 0);
}

function metricFields(row) {
  const gir = num(row.gir);
  const fa = num(row.driving_acc);
  const girCount = Number.isFinite(gir) ? (gir <= 2 ? Math.round(gir * 18) : Math.round(gir)) : null;
  const fwCount = Number.isFinite(fa) ? (fa <= 1 ? Math.round(fa * 14) : Math.round(fa)) : null;
  return {
    round_score: num(row.round_score),
    birdies: num(row.birdies),
    pars: num(row.pars),
    bogies: num(row.bogies),
    gir: girCount,
    fairways: fwCount,
    eagles_or_better: num(row.eagles_or_better),
    doubles_or_worse: num(row.doubles_or_worse),
  };
}

function shotCsvSeasonYear(tournamentId) {
  const m = String(tournamentId || "").match(/^R(\d{4})/);
  if (!m) return NaN;
  return parseInt(m[1], 10);
}

function parseYards(distanceStr) {
  const s = String(distanceStr || "");
  const y = s.match(/(\d+(?:\.\d+)?)\s*yds?/i);
  if (y) return Math.round(num(y[1], NaN));
  return null;
}

function loadAllowedDgIds() {
  if (!fs.existsSync(PROJECTIONS_JSON)) {
    console.warn("No projections.json — no players.");
    return new Set();
  }
  const raw = JSON.parse(fs.readFileSync(PROJECTIONS_JSON, "utf8"));
  const ids = new Set();
  for (const p of raw.players || []) {
    const id = Math.round(num(p.dg_id));
    if (Number.isFinite(id)) ids.add(id);
  }
  return ids;
}

async function loadPgaToDgMap() {
  const m = new Map();
  if (!fs.existsSync(PLAYER_MAP_CSV)) {
    console.warn("Missing player map:", PLAYER_MAP_CSV);
    return m;
  }
  const parser = createReadStream(PLAYER_MAP_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );
  for await (const row of parser) {
    const pid = String(row.pga_player_id ?? "").trim().replace(/^"(.*)"$/, "$1");
    const dg = Math.round(num(row.dg_id));
    if (!pid || !Number.isFinite(dg)) continue;
    m.set(pid, dg);
  }
  console.log("PGA→dg_id map rows:", m.size);
  return m;
}

async function buildAllowedTriplesAndNames(allowedDgIds) {
  const byDgId = new Map();
  if (!fs.existsSync(ROUNDS_CSV)) {
    console.error("Missing rounds CSV:", ROUNDS_CSV);
    return { byDgId, allowedTriplesShots: new Set(), dgToPlayerName: new Map() };
  }

  const parser = createReadStream(ROUNDS_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );

  for await (const row of parser) {
    if (String(row.tour || "").toLowerCase() !== "pga") continue;
    const yr = parseInt(row.year, 10);
    if (Number.isFinite(yr) && yr < MIN_YEAR) continue;
    const dg = Math.round(num(row.dg_id));
    if (!Number.isFinite(dg) || !allowedDgIds.has(dg)) continue;
    const rs = num(row.round_score);
    if (!Number.isFinite(rs)) continue;

    const sortKey = parseUsDateSortKey(row.event_completed) * 10 + (parseInt(row.round_num, 10) || 1);
    const eventName = String(row.event_name || "").trim();
    const courseRaw = String(row.course_name || row.course || "").trim();
    const rec = {
      sortKey,
      event_completed: String(row.event_completed || ""),
      year: yr,
      event_name: eventName,
      event_id: String(row.event_id || ""),
      course_name: courseRaw || eventName,
      round_num: parseInt(row.round_num, 10) || 1,
      fin_text: String(row.fin_text || ""),
      ...metricFields(row),
    };

    if (!byDgId.has(dg)) byDgId.set(dg, { dg_id: dg, player_name: String(row.player_name || ""), rounds: [] });
    const bucket = byDgId.get(dg);
    if (!bucket.player_name) bucket.player_name = String(row.player_name || "");
    bucket.rounds.push(rec);
  }

  for (const [, bucket] of byDgId) {
    bucket.rounds.sort((a, b) => a.sortKey - b.sortKey);
    if (bucket.rounds.length > MAX_ROUNDS_PER_PLAYER) bucket.rounds = bucket.rounds.slice(-MAX_ROUNDS_PER_PLAYER);
  }

  const allowedTriplesShots = new Set();
  const dgToPlayerName = new Map();
  for (const [dg, bucket] of byDgId) {
    dgToPlayerName.set(dg, bucket.player_name);
    const pk = playerKeyHistorical(bucket.player_name);
    const tail = bucket.rounds.slice(-SHOT_ROUND_TAIL);
    for (const r of tail) {
      allowedTriplesShots.add(`${pk}|||${normEvt(r.event_name)}|||${r.round_num}`);
    }
  }

  return { byDgId, allowedTriplesShots, dgToPlayerName };
}

/**
 * Nested: byDgId[dg][roundUid][holeStr] = array of compact shots (sorted by stroke_number).
 */
async function streamShotsIntoBuckets(pgaToDg, allowedTriplesShots, dgToPlayerName) {
  /** @type {Map<string, Map<string, Map<string, Array<object>>>>} */
  const byDg = new Map();
  let kept = 0;
  let skippedYear = 0;
  let skippedTriple = 0;
  let skippedMap = 0;

  if (!fs.existsSync(SHOTS_CSV)) {
    console.warn("Missing shots CSV:", SHOTS_CSV);
    return { byDg, kept: 0, skippedYear: 0, skippedTriple: 0, skippedMap: 0 };
  }

  const parser = createReadStream(SHOTS_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    })
  );

  for await (const row of parser) {
    const tid = String(row.tournament_id || "").trim();
    const sy = shotCsvSeasonYear(tid);
    if (!Number.isFinite(sy) || sy < MIN_SHOT_SEASON_YEAR) {
      skippedYear++;
      continue;
    }

    const pid = String(row.player_id ?? "").trim();
    const dg = pgaToDg.get(pid);
    if (!Number.isFinite(dg)) {
      skippedMap++;
      continue;
    }

    const pname = dgToPlayerName.get(dg) || "";
    const pk = playerKeyHistorical(pname);
    const tname = String(row.tournament_name || "").trim();
    const rn = parseInt(row.round, 10) || 1;
    const triple = `${pk}|||${normEvt(tname)}|||${rn}`;
    if (!allowedTriplesShots.has(triple)) {
      skippedTriple++;
      continue;
    }

    const uid = `${tname}\tR${rn}`;
    const hole = String(parseInt(row.hole_number, 10) || 0);
    if (hole === "0") continue;

    const sn = parseInt(row.stroke_number, 10) || 0;
    const pbp = String(row.play_by_play || "").trim().slice(0, PLAY_BY_PLAY_MAX);
    const fin =
      String(row.final_stroke || "").toLowerCase() === "true" || row.final_stroke === true;
    const compact = {
      sn,
      f: String(row.from_location_code || "").trim(),
      t: String(row.to_location_code || "").trim(),
      d: parseYards(row.distance),
      ...(pbp ? { p: pbp } : {}),
      ...(fin ? { fin: true } : {}),
    };

    if (!byDg.has(String(dg))) byDg.set(String(dg), new Map());
    const roundMap = byDg.get(String(dg));
    if (!roundMap.has(uid)) roundMap.set(uid, new Map());
    const holeMap = roundMap.get(uid);
    if (!holeMap.has(hole)) holeMap.set(hole, []);
    holeMap.get(hole).push(compact);
    kept++;
  }

  for (const [, roundMap] of byDg) {
    for (const [, holeMap] of roundMap) {
      for (const [h, arr] of holeMap) {
        arr.sort((a, b) => a.sn - b.sn);
        holeMap.set(h, arr);
      }
    }
  }

  return { byDg, kept, skippedYear, skippedTriple, skippedMap };
}

function serializeNestedMaps(byDg) {
  const out = {};
  for (const [dg, roundMap] of byDg) {
    out[dg] = {};
    for (const [uid, holeMap] of roundMap) {
      out[dg][uid] = {};
      for (const [h, arr] of holeMap) {
        out[dg][uid][h] = arr;
      }
    }
  }
  return out;
}

async function main() {
  console.log("Shots CSV:", SHOTS_CSV);
  console.log("Rounds CSV (allowlist):", ROUNDS_CSV);
  const allowed = loadAllowedDgIds();
  console.log("Allowed dg_ids:", allowed.size);

  const pgaToDg = await loadPgaToDgMap();
  const { allowedTriplesShots, dgToPlayerName } = await buildAllowedTriplesAndNames(allowed);
  console.log("Allowlist triples (shots, last", SHOT_ROUND_TAIL, "rounds/player):", allowedTriplesShots.size);

  const { byDg, kept, skippedYear, skippedTriple, skippedMap } = await streamShotsIntoBuckets(
    pgaToDg,
    allowedTriplesShots,
    dgToPlayerName
  );
  console.log(
    "Shot rows kept:",
    kept,
    "| skipped (year<2022):",
    skippedYear,
    "| skipped (no triple):",
    skippedTriple,
    "| skipped (no pga map):",
    skippedMap
  );

  const payload = {
    meta: {
      updated_at: new Date().toISOString(),
      min_shot_season_year: MIN_SHOT_SEASON_YEAR,
      shot_round_tail: SHOT_ROUND_TAIL,
      source_csv: path.basename(SHOTS_CSV),
      player_map: path.basename(PLAYER_MAP_CSV),
      rows_used: kept,
    },
    byDgId: serializeNestedMaps(byDg),
  };

  fs.writeFileSync(OUT_JSON, JSON.stringify(payload), "utf8");
  const st = fs.statSync(OUT_JSON);
  console.log("Wrote", OUT_JSON, `(${(st.size / 1024).toFixed(1)} KB)`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
