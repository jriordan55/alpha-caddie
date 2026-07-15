#!/usr/bin/env node
/**
 * Build player-history/by-course/*.json for Historical Trends “field by course”.
 *
 * Scans historical_rounds_all.csv for all PGA/LIV rounds at each venue (not just this week's field).
 * Enriched rows from player_round_history.json override CSV rows when present (weather, pins, etc.).
 *
 *   npm run build:course-shards
 */
import fs from "fs";
import path from "path";
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { fileURLToPath } from "url";
import {
  normCourseNameKey,
  courseShardFileName,
  formatCourseLabelForDisplay,
} from "./course-name-key.mjs";
import {
  historyRoundChartUtcIsoDay,
  roundEventCompletedMdYFromEventEnd,
} from "./history-round-dates.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR
  ? path.resolve(process.env.GOLF_MODEL_DIR)
  : path.resolve(WEB_ROOT, "..");
const HISTORY_JSON = path.join(WEB_ROOT, "player_round_history.json");
const ROUNDS_CSV = path.join(REPO_ROOT, "data", "historical_rounds_all.csv");
const COURSE_SHARD_DIR = path.join(WEB_ROOT, "player-history", "by-course");
const COURSES_MANIFEST_JSON = path.join(WEB_ROOT, "player-history", "courses-manifest.json");

const MIN_YEAR = (() => {
  const env = parseInt(String(process.env.GOLF_HISTORY_MIN_YEAR ?? "").trim(), 10);
  return Number.isFinite(env) && env >= 1990 ? env : 2004;
})();

function num(v) {
  // Number("") === 0, but an empty CSV cell means "missing", not zero.
  if (v == null || String(v).trim() === "") return NaN;
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

/** Round a rate (0–1) or raw count to a whole number of holes; null when out of range. */
function countFromRateOrRaw(raw, holes) {
  const n = num(raw);
  if (!Number.isFinite(n)) return null;
  const c = n > 0 && n <= 1.0001 ? Math.round(n * holes) : Math.round(n);
  return Math.min(holes, Math.max(0, c));
}

/** Per-round strokes-gained columns straight from historical_rounds_all.csv (null when absent). */
function sgFieldsFromCsv(row) {
  const f = (k) => {
    const v = num(row[k]);
    return Number.isFinite(v) ? v : null;
  };
  return {
    sg_putt: f("sg_putt"),
    sg_app: f("sg_app"),
    sg_arg: f("sg_arg"),
    sg_ott: f("sg_ott"),
    sg_t2g: f("sg_t2g"),
    sg_total: f("sg_total"),
  };
}

/** Counting stats from CSV; 0/1 GIR/fairways/putts are almost always bad joins, so drop them. */
function countingFieldsFromCsv(row) {
  const faDirect = num(row.fairways);
  const fwRaw = Number.isFinite(faDirect) ? faDirect : num(row.driving_acc);
  let girCount = countFromRateOrRaw(row.gir, 18);
  let fwCount = Number.isFinite(fwRaw) ? countFromRateOrRaw(fwRaw, 14) : null;
  if (girCount === 0 || girCount === 1) girCount = null;
  if (fwCount === 0 || fwCount === 1) fwCount = null;
  const ptRaw = num(row.putts);
  const puttsCount = Number.isFinite(ptRaw) && ptRaw > 1.5 && ptRaw < 80 ? Math.round(ptRaw) : null;
  return {
    gir: girCount,
    fairways: fwCount,
    putts: puttsCount,
    eagles_or_better: Number.isFinite(num(row.eagles_or_better)) ? num(row.eagles_or_better) : undefined,
    doubles_or_worse: Number.isFinite(num(row.doubles_or_worse)) ? num(row.doubles_or_worse) : undefined,
  };
}

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

function parseEventCompletedChronoBase(mdy) {
  const m = String(mdy || "").trim().match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  if (!m) return 0;
  return Number(m[3]) * 10000 + Number(m[1]) * 100 + Number(m[2]);
}

function chartUtcIsoDayFromHistoryRow(r) {
  return historyRoundChartUtcIsoDay(r);
}

function courseShardEntryKey(entry) {
  const rr = entry?.row && typeof entry.row === "object" ? entry.row : entry;
  const dg = Math.round(Number(entry?.dg_id ?? rr?.dg_id));
  const sk = Math.round(Number(rr?.sortKey));
  if (Number.isFinite(sk) && sk > 0) return `${dg}|${sk}`;
  const yr = parseInt(String(rr?.year || ""), 10);
  const rn = Math.round(Number(rr?.round_num));
  const ev = normEvt(rr?.event_name);
  return `${dg}|${yr}|${rn}|${ev}`;
}

function mergeCourseShardEntries(existing, incoming) {
  // Freshly rebuilt rows (incoming) are derived from the current CSV + enriched index and are the
  // source of truth (e.g. they carry strokes gained); let them override same-key rows from a prior
  // shard, while still preserving any prior-only rows not present in this build.
  const byKey = new Map();
  for (const e of existing || []) byKey.set(courseShardEntryKey(e), e);
  for (const e of incoming || []) byKey.set(courseShardEntryKey(e), e);
  return [...byKey.values()];
}

function writeJsonAtomic(outPath, payload) {
  const tmpPath = `${outPath}.tmp`;
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(tmpPath, JSON.stringify(payload), "utf8");
  fs.renameSync(tmpPath, outPath);
}

/** dg|event|year|round → enriched row from player_round_history.json */
function buildEnrichedRowIndex(out) {
  const idx = new Map();
  for (const [dgId, bucket] of Object.entries(out?.byDgId || {})) {
    const dg = Math.round(Number(dgId));
    if (!Number.isFinite(dg) || !bucket?.rounds) continue;
    const playerName = String(bucket.player_name || "").trim();
    for (const r of bucket.rounds) {
      const yr = parseInt(String(r.year || ""), 10);
      const rn = Math.round(Number(r.round_num));
      const ev = normEvt(r.event_name);
      if (!ev || !Number.isFinite(yr)) continue;
      idx.set(`${dg}|${ev}|${yr}|${rn}`, { dg_id: dg, player_name: playerName, row: r });
    }
  }
  return idx;
}

function csvRowToHistoryEntry(row, enrichedIdx) {
  const tour = String(row.tour || "").toLowerCase();
  const dg = Math.round(num(row.dg_id));
  const rnd = parseInt(row.round_num, 10) || 1;
  const yr = parseInt(row.year, 10);
  const eventName = String(row.event_name || "").trim();
  const ev = normEvt(eventName);
  const hit = enrichedIdx.get(`${dg}|${ev}|${yr}|${rnd}`);
  if (hit) return hit;

  const eventDate = roundEventCompletedMdYFromEventEnd(row.event_completed, rnd, tour);
  const sortKey = parseEventCompletedChronoBase(eventDate) * 10 + rnd;
  const courseRaw = String(row.course_name || "").trim();
  return {
    dg_id: dg,
    player_name: String(row.player_name || "").trim(),
    row: {
      sortKey,
      event_completed: eventDate || String(row.event_completed || ""),
      year: yr,
      event_name: eventName,
      event_id: String(row.event_id || ""),
      course_name: formatCourseLabelForDisplay(courseRaw) || courseRaw || eventName,
      round_num: rnd,
      fin_text: String(row.fin_text || ""),
      round_score: num(row.round_score),
      birdies: num(row.birdies),
      pars: num(row.pars),
      bogies: num(row.bogies),
      ...countingFieldsFromCsv(row),
      ...sgFieldsFromCsv(row),
      _from_dg_historical_rounds: true,
    },
  };
}

async function streamCourseShardsFromCsv(enrichedIdx) {
  const byCourse = new Map();
  if (!fs.existsSync(ROUNDS_CSV)) {
    console.warn("[build:course-shards] Missing CSV:", ROUNDS_CSV);
    return byCourse;
  }

  console.log(
    `[build:course-shards] Scanning ${path.basename(ROUNDS_CSV)} (all PGA/LIV venues, min_year ${MIN_YEAR})…`,
  );
  let rowsScanned = 0;
  const parser = createReadStream(ROUNDS_CSV).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  );

  for await (const row of parser) {
    rowsScanned++;
    if (rowsScanned % 200000 === 0) {
      console.log(`[build:course-shards] …${rowsScanned.toLocaleString()} CSV rows, ${byCourse.size} course(s)`);
    }
    const tour = String(row.tour || "").toLowerCase();
    if (tour !== "pga" && tour !== "liv") continue;
    const yr = parseInt(row.year, 10);
    if (Number.isFinite(yr) && yr < MIN_YEAR) continue;
    const dg = Math.round(num(row.dg_id));
    const rs = num(row.round_score);
    if (!Number.isFinite(dg) || !Number.isFinite(rs) || rs <= 0) continue;

    const courseRaw = String(row.course_name || "").trim();
    const ck = normCourseNameKey(formatCourseLabelForDisplay(courseRaw) || courseRaw);
    if (!ck) continue;

    let b = byCourse.get(ck);
    if (!b) {
      b = { dateSet: new Set(), entries: [] };
      byCourse.set(ck, b);
    }
    const entry = csvRowToHistoryEntry(row, enrichedIdx);
    b.entries.push(entry);
    const iso = chartUtcIsoDayFromHistoryRow(entry.row);
    if (iso) b.dateSet.add(iso);
  }

  console.log(
    `[build:course-shards] CSV done — ${rowsScanned.toLocaleString()} rows, ${byCourse.size} course(s).`,
  );
  return byCourse;
}

async function main() {
  let enrichedIdx = new Map();
  if (fs.existsSync(HISTORY_JSON)) {
    const out = JSON.parse(fs.readFileSync(HISTORY_JSON, "utf8"));
    enrichedIdx = buildEnrichedRowIndex(out);
    console.log(`[build:course-shards] Enriched row index: ${enrichedIdx.size} from player_round_history.json`);
  }

  const byCourse = await streamCourseShardsFromCsv(enrichedIdx);
  if (!byCourse.size) {
    console.error("[build:course-shards] No course rows — run update:rounds / rebuild:history-deep first.");
    process.exit(1);
  }

  fs.mkdirSync(COURSE_SHARD_DIR, { recursive: true });
  const keep = new Set();
  const courses = [];
  let n = 0;
  for (const [courseKey, b] of byCourse) {
    n++;
    if (n % 25 === 0) console.log(`[build:course-shards] Writing shard ${n}/${byCourse.size}…`);
    const file = courseShardFileName(courseKey);
    keep.add(file);
    const outPath = path.join(COURSE_SHARD_DIR, file);
    let mergedEntries = b.entries;
    if (fs.existsSync(outPath)) {
      try {
        const prev = JSON.parse(fs.readFileSync(outPath, "utf8"));
        mergedEntries = mergeCourseShardEntries(prev?.entries, b.entries);
      } catch {
        /* use new */
      }
    }
    const daysSet = new Set(b.dateSet);
    for (const e of mergedEntries) {
      const iso = chartUtcIsoDayFromHistoryRow(e.row || e);
      if (iso) daysSet.add(iso);
    }
    const days = [...daysSet].sort((a, c) => c.localeCompare(a));
    writeJsonAtomic(outPath, { course_key: courseKey, days, entries: mergedEntries });
    courses.push({ course_key: courseKey, file, days: days.length, entries: mergedEntries.length });
  }

  for (const entry of fs.readdirSync(COURSE_SHARD_DIR)) {
    if (!entry.endsWith(".json") || keep.has(entry)) continue;
    const p = path.join(COURSE_SHARD_DIR, entry);
    try {
      const prev = JSON.parse(fs.readFileSync(p, "utf8"));
      if (Array.isArray(prev?.entries) && prev.entries.length > 0) {
        courses.push({
          course_key: prev.course_key || entry.replace(/\.json$/, ""),
          file: entry,
          days: (prev.days || []).length,
          entries: prev.entries.length,
          preserved: true,
        });
        continue;
      }
    } catch {
      /* drop */
    }
    fs.unlinkSync(p);
  }

  courses.sort((a, b) => a.course_key.localeCompare(b.course_key));
  writeJsonAtomic(COURSES_MANIFEST_JSON, {
    meta: {
      updated_at: new Date().toISOString(),
      source: path.basename(ROUNDS_CSV),
      min_year: MIN_YEAR,
    },
    courses,
  });

  const shin = courses.find((c) => c.course_key === "shinnecock hills golf club");
  console.log("Wrote", courses.length, "course shard(s) ->", path.relative(WEB_ROOT, COURSE_SHARD_DIR));
  if (shin) console.log("Shinnecock Hills:", shin.entries, "entries,", shin.days, "days");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
