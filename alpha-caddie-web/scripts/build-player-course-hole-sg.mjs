/**
 * Build strokes gained by hole at every course for every player who has played that hole.
 *
 * Source: all_shots_2022_2026.csv hole scores + historical_rounds_all.csv for course
 *         (critical for multi-course events). Fallback: pga_tournament_course_map.csv.
 *
 * Definition (per hole play):
 *   SG = field_mean_score(course, hole) − player_score
 * Aggregate row: mean SG over all plays of that (player, course, hole).
 *
 * Usage:
 *   npm run build:course-hole-sg
 *   node scripts/build-player-course-hole-sg.mjs
 */
import { createReadStream, createWriteStream, writeFileSync, existsSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { finished } from "stream/promises";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");

const SHOTS = path.join(REPO, "data", "all_shots_2022_2026.csv");
const HIST = path.join(WEB, "data", "historical_rounds_all.csv");
const HIST_FALLBACK = path.join(REPO, "data", "historical_rounds_all.csv");
const MAP = path.join(REPO, "data", "pga_datagolf_player_map.csv");
const COURSE_MAP = path.join(REPO, "data", "pga_tournament_course_map.csv");
const OUT_CSV = path.join(WEB, "data", "player_course_hole_sg.csv");
const OUT_BASE = path.join(WEB, "data", "course_hole_sg_baselines.json");
const OUT_PLAYS = path.join(WEB, "data", "player_course_hole_sg_plays.csv");

const args = process.argv.slice(2);
function hasFlag(name) {
  return args.includes(`--${name}`) || args.includes(`-${name}`);
}
function argNum(name, fb) {
  const hit = args.find((a) => a.startsWith(`--${name}=`));
  if (!hit) return fb;
  const n = Number(hit.split("=")[1]);
  return Number.isFinite(n) ? n : fb;
}
const MAX_ROWS = argNum("max-rows", Infinity);
const WRITE_PLAYS =
  !hasFlag("no-plays") &&
  (hasFlag("plays") ||
    String(process.env.GOLF_COURSE_HOLE_SG_PLAYS || "1").trim() !== "0");
const MIN_BASELINE_N = argNum("min-baseline-n", 30);

function parseEventCompletedMs(s) {
  const raw = String(s || "").trim();
  const iso = raw.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = raw.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) {
    return Date.parse(
      `${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`,
    );
  }
  return NaN;
}

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function normPgaId(pid) {
  const s = String(pid ?? "").trim();
  if (!s) return "";
  if (/^\d+$/.test(s) && s.length < 5) return s.padStart(5, "0");
  return s;
}

function yearFromTid(tid) {
  const m = String(tid || "").match(/R(20\d{2})/i);
  return m ? Number(m[1]) : NaN;
}

async function loadCsv(file) {
  const rows = [];
  if (!existsSync(file)) return rows;
  await new Promise((res, rej) => {
    createReadStream(file)
      .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", res)
      .on("error", rej);
  });
  return rows;
}

async function loadDgMap() {
  const map = new Map();
  for (const r of await loadCsv(MAP)) {
    const pid = normPgaId(r.pga_player_id);
    const dg = Math.round(num(r.dg_id, NaN));
    if (pid && Number.isFinite(dg)) map.set(pid, dg);
  }
  return map;
}

async function loadTournamentCourseFallback() {
  /** @type {Map<string, {course_name:string, course_key:string}>} */
  const map = new Map();
  for (const r of await loadCsv(COURSE_MAP)) {
    const tid = String(r.tournament_id || "").trim();
    const name = String(r.course_name || "").trim();
    if (!tid || !name) continue;
    map.set(tid, { course_name: name, course_key: normCourseNameKey(name) });
  }
  return map;
}

/**
 * Index historical rounds for course lookup.
 * Keys: n|dg|year|evt|rnd  and  c|dg|year|courseKey|rnd (unused for join)
 */
async function loadHistoricalCourseIndex() {
  const file = existsSync(HIST) ? HIST : HIST_FALLBACK;
  /** @type {Map<string, {course_num:number|null, course_name:string, course_key:string, time_ms:number}>} */
  const idx = new Map();
  let n = 0;
  if (!existsSync(file)) {
    console.warn("[course-hole-sg] Missing historical rounds:", file);
    return idx;
  }
  await new Promise((res, rej) => {
    createReadStream(file)
      .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (r) => {
        n++;
        const dg = Math.round(num(r.dg_id, NaN));
        const yr = Math.round(num(r.year, NaN));
        const rnd = Math.round(num(r.round_num, NaN));
        const cname = String(r.course_name || "").trim();
        if (!Number.isFinite(dg) || !Number.isFinite(yr) || !Number.isFinite(rnd) || !cname) return;
        let time_ms = parseEventCompletedMs(r.event_completed);
        if (!Number.isFinite(time_ms) && Number.isFinite(yr)) {
          // Mid-year proxy so as-of can still order seasons without dates.
          time_ms = Date.parse(`${yr}-06-01T12:00:00Z`) + rnd * 86400000;
        }
        const payload = {
          course_num: Number.isFinite(num(r.course_num, NaN)) ? Math.round(num(r.course_num)) : null,
          course_name: cname,
          course_key: normCourseNameKey(cname),
          time_ms,
        };
        const evt = normEvt(r.event_name);
        if (evt) idx.set(`n|${dg}|${yr}|${evt}|${rnd}`, payload);
        const eid = String(r.event_id || "").trim();
        if (eid) idx.set(`e|${dg}|${yr}|${eid}|${rnd}`, payload);
      })
      .on("end", res)
      .on("error", rej);
  });
  console.log(`[course-hole-sg] Historical course index: ${idx.size.toLocaleString()} keys from ${n.toLocaleString()} rounds`);
  return idx;
}

/**
 * Aggregate unique hole scores from shot traces.
 * @returns {Promise<Map<string, {tid,pid,rnd,hole,par,score,tname}>>}
 */
async function loadHolePlaysFromShots() {
  /** @type {Map<string, object>} */
  const holes = new Map();
  let rows = 0;
  let stopped = false;

  await new Promise((resolve, reject) => {
    let settled = false;
    const done = () => {
      if (settled) return;
      settled = true;
      resolve();
    };
    const parser = createReadStream(SHOTS).pipe(
      parse({ columns: true, relax_column_count: true, skip_records_with_error: true }),
    );
    parser.on("data", (r) => {
      if (stopped) return;
      rows++;
      if (rows > MAX_ROWS) {
        stopped = true;
        parser.destroy();
        done();
        return;
      }
      if (rows % 1_000_000 === 0) {
        console.log(`  … ${rows.toLocaleString()} shot rows, ${holes.size.toLocaleString()} hole plays`);
      }
      const tid = String(r.tournament_id || "").trim();
      const pid = normPgaId(r.player_id);
      const rnd = Math.round(num(r.round, NaN));
      const hole = Math.round(num(r.hole_number, NaN));
      const score = Math.round(num(r.hole_score, NaN));
      const par = Math.round(num(r.par, NaN));
      if (!tid || !pid || !Number.isFinite(rnd) || !Number.isFinite(hole) || !Number.isFinite(score)) return;
      if (hole < 1 || hole > 18 || score < 1 || score > 15) return;
      const key = `${tid}|${pid}|${rnd}|${hole}`;
      if (holes.has(key)) return;
      holes.set(key, {
        tid,
        pid,
        rnd,
        hole,
        par: Number.isFinite(par) ? par : null,
        score,
        tname: String(r.tournament_name || ""),
        year: yearFromTid(tid),
      });
    });
    parser.on("error", (err) => {
      if (stopped) done();
      else reject(err);
    });
    parser.on("end", done);
    parser.on("close", done);
  });

  console.log(
    `[course-hole-sg] Shot scan: ${rows.toLocaleString()} rows → ${holes.size.toLocaleString()} hole plays`,
  );
  return holes;
}

function resolveCourse(play, dg, histIdx, tidCourse) {
  if (Number.isFinite(dg) && Number.isFinite(play.year)) {
    const evt = normEvt(play.tname);
    if (evt) {
      const hit = histIdx.get(`n|${dg}|${play.year}|${evt}|${play.rnd}`);
      if (hit) return hit;
    }
  }
  const fb = tidCourse.get(play.tid);
  if (fb) {
    const yr = play.year;
    const time_ms = Number.isFinite(yr)
      ? Date.parse(`${yr}-06-01T12:00:00Z`) + (Number.isFinite(play.rnd) ? play.rnd * 86400000 : 0)
      : NaN;
    return {
      course_num: null,
      course_name: fb.course_name,
      course_key: fb.course_key,
      time_ms,
    };
  }
  return null;
}

function csvEscape(v) {
  const s = String(v ?? "");
  if (/[",\n]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

if (!existsSync(SHOTS)) {
  console.error("Missing shots file:", SHOTS);
  process.exit(1);
}

console.log("Loading maps…");
const dgMap = await loadDgMap();
const tidCourse = await loadTournamentCourseFallback();
const histIdx = await loadHistoricalCourseIndex();
console.log(`  dg map ${dgMap.size}; tournament→course fallback ${tidCourse.size}`);

console.log("Pass 1 — hole plays from shots…");
const holePlays = await loadHolePlaysFromShots();

console.log("Pass 2 — attach course + accumulate baselines…");
/** baseline: course_key|hole → {sum,n,parSum,parN,course_name,course_num} */
const baseline = new Map();
/** enriched plays for pass 3 */
const enriched = [];
let withCourse = 0;
let noCourse = 0;
let noDg = 0;

for (const play of holePlays.values()) {
  const dg = dgMap.get(play.pid) ?? dgMap.get(String(play.pid).replace(/^0+/, "")) ?? null;
  if (!Number.isFinite(dg)) {
    noDg++;
    continue;
  }
  const course = resolveCourse(play, dg, histIdx, tidCourse);
  if (!course?.course_key) {
    noCourse++;
    continue;
  }
  withCourse++;
  const bKey = `${course.course_key}|${play.hole}`;
  let bl = baseline.get(bKey);
  if (!bl) {
    bl = {
      course_key: course.course_key,
      course_name: course.course_name,
      course_num: course.course_num,
      hole: play.hole,
      sum: 0,
      n: 0,
      parSum: 0,
      parN: 0,
    };
    baseline.set(bKey, bl);
  }
  bl.sum += play.score;
  bl.n += 1;
  if (play.par != null) {
    bl.parSum += play.par;
    bl.parN += 1;
  }
  if (course.course_num != null && bl.course_num == null) bl.course_num = course.course_num;
  enriched.push({
    dg_id: dg,
    year: play.year,
    round: play.rnd,
    hole: play.hole,
    par: play.par,
    score: play.score,
    tournament_id: play.tid,
    tournament_name: play.tname,
    course_key: course.course_key,
    course_name: course.course_name,
    course_num: course.course_num,
    time_ms: Number.isFinite(course.time_ms) ? course.time_ms : NaN,
  });
}

console.log(
  `  with course: ${withCourse.toLocaleString()}; no dg: ${noDg.toLocaleString()}; no course: ${noCourse.toLocaleString()}`,
);
console.log(`  baseline cells (course×hole): ${baseline.size.toLocaleString()}`);

const baselineOut = {};
for (const [k, bl] of baseline) {
  if (bl.n < MIN_BASELINE_N) continue;
  baselineOut[k] = {
    course_key: bl.course_key,
    course_name: bl.course_name,
    course_num: bl.course_num,
    hole: bl.hole,
    n: bl.n,
    mean_score: bl.sum / bl.n,
    mean_par: bl.parN ? bl.parSum / bl.parN : null,
  };
}
console.log(`  baselines with n≥${MIN_BASELINE_N}: ${Object.keys(baselineOut).length.toLocaleString()}`);

console.log("Pass 3 — player × course × hole SG…");
/** key dg|course_key|hole → agg */
const playerAgg = new Map();
let playsUsed = 0;
let playsSkippedBaseline = 0;

let playsOut = null;
if (WRITE_PLAYS) {
  playsOut = createWriteStream(OUT_PLAYS, { encoding: "utf8" });
  playsOut.write(
    [
      "dg_id",
      "year",
      "tournament_id",
      "tournament_name",
      "round",
      "course_num",
      "course_name",
      "course_key",
      "hole",
      "par",
      "score",
      "field_mean",
      "sg",
      "time_ms",
    ].join(",") + "\n",
  );
}

for (const p of enriched) {
  const bKey = `${p.course_key}|${p.hole}`;
  const bl = baselineOut[bKey];
  if (!bl) {
    playsSkippedBaseline++;
    continue;
  }
  const sg = bl.mean_score - p.score;
  playsUsed++;
  const aKey = `${p.dg_id}|${p.course_key}|${p.hole}`;
  let a = playerAgg.get(aKey);
  if (!a) {
    a = {
      dg_id: p.dg_id,
      course_key: p.course_key,
      course_name: p.course_name,
      course_num: p.course_num ?? bl.course_num,
      hole: p.hole,
      par: p.par ?? bl.mean_par,
      n: 0,
      sum_sg: 0,
      sum_score: 0,
      field_mean: bl.mean_score,
      first_year: p.year,
      last_year: p.year,
    };
    playerAgg.set(aKey, a);
  }
  a.n += 1;
  a.sum_sg += sg;
  a.sum_score += p.score;
  if (Number.isFinite(p.year)) {
    if (!Number.isFinite(a.first_year) || p.year < a.first_year) a.first_year = p.year;
    if (!Number.isFinite(a.last_year) || p.year > a.last_year) a.last_year = p.year;
  }
  if (p.par != null) a.par = p.par;
  if (p.course_num != null) a.course_num = p.course_num;

  if (playsOut) {
    playsOut.write(
      [
        p.dg_id,
        p.year || "",
        p.tournament_id,
        csvEscape(p.tournament_name),
        p.round,
        p.course_num ?? "",
        csvEscape(p.course_name),
        csvEscape(p.course_key),
        p.hole,
        p.par ?? "",
        p.score,
        bl.mean_score.toFixed(4),
        sg.toFixed(4),
        Number.isFinite(p.time_ms) ? Math.round(p.time_ms) : "",
      ].join(",") + "\n",
    );
  }
}

if (playsOut) {
  playsOut.end();
  await finished(playsOut);
  console.log(`  wrote plays → ${OUT_PLAYS}`);
}

const header = [
  "dg_id",
  "course_num",
  "course_name",
  "course_key",
  "hole",
  "par",
  "n",
  "mean_score",
  "field_mean",
  "sg",
  "first_year",
  "last_year",
];
const out = createWriteStream(OUT_CSV, { encoding: "utf8" });
out.write(header.join(",") + "\n");
let nOut = 0;
for (const a of playerAgg.values()) {
  const mean_score = a.sum_score / a.n;
  const sg = a.sum_sg / a.n;
  out.write(
    [
      a.dg_id,
      a.course_num ?? "",
      csvEscape(a.course_name),
      csvEscape(a.course_key),
      a.hole,
      a.par != null && Number.isFinite(Number(a.par)) ? Math.round(Number(a.par)) : "",
      a.n,
      mean_score.toFixed(4),
      Number(a.field_mean).toFixed(4),
      sg.toFixed(4),
      a.first_year ?? "",
      a.last_year ?? "",
    ].join(",") + "\n",
  );
  nOut++;
}
out.end();
await finished(out);

writeFileSync(
  OUT_BASE,
  JSON.stringify(
    {
      meta: {
        source_shots: SHOTS,
        method:
          "SG_hole = field mean score(course, hole) − player score; aggregated mean over plays. Course from historical_rounds (multi-course aware) with tournament course-map fallback.",
        min_baseline_n: MIN_BASELINE_N,
        hole_plays_with_course: withCourse,
        hole_plays_used: playsUsed,
        hole_plays_skipped_thin_baseline: playsSkippedBaseline,
        player_course_hole_rows: nOut,
        created_at: new Date().toISOString(),
      },
      baselines: baselineOut,
    },
    null,
    2,
  ),
);

console.log(`\nWrote ${nOut.toLocaleString()} player×course×hole rows → ${OUT_CSV}`);
console.log(`Baselines → ${OUT_BASE}`);
console.log(`  plays used for SG: ${playsUsed.toLocaleString()}`);
