/**
 * Build round-level approach SG by distance/lie buckets from pgatouR shot traces.
 *
 * DataGolf does not publish round × distance-bucket SG. This estimates it via
 * classic strokes-gained: for each approach shot,
 *   SG = E[strokes_to_hole | bucket] − actual_strokes_to_hole
 * where E[·] is the tour empirical mean from all_shots_2022_2026.csv.
 *
 * Usage (from alpha-caddie-web):
 *   npm run build:round-sg-distance              # full rebuild
 *   npm run build:round-sg-distance:live         # current-event upsert (push:live)
 *   node scripts/build-round-sg-by-distance.mjs --live
 *   node scripts/build-round-sg-by-distance.mjs --max-rows=500000
 *
 * Writes:
 *   data/round_sg_by_distance.csv
 *   data/round_sg_by_distance_baselines.json
 */
import { createReadStream, createWriteStream, writeFileSync, readFileSync, existsSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { finished } from "stream/promises";
import { SG_DISTANCE_BUCKETS as BUCKETS } from "./sg-distance-fields.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");
const SHOTS = path.join(REPO, "data", "all_shots_2022_2026.csv");
const MAP = path.join(REPO, "data", "pga_datagolf_player_map.csv");
const OUT_CSV = path.join(WEB, "data", "round_sg_by_distance.csv");
const OUT_BASE = path.join(WEB, "data", "round_sg_by_distance_baselines.json");
const PGA_JSON = path.join(WEB, "data", "pgatour_event_rounds.json");

const args = process.argv.slice(2);
function argNum(name, fb) {
  const hit = args.find((a) => a.startsWith(`--${name}=`));
  if (!hit) return fb;
  const n = Number(hit.split("=")[1]);
  return Number.isFinite(n) ? n : fb;
}
function hasFlag(name) {
  return args.includes(`--${name}`) || args.includes(`-${name}`);
}
const MAX_ROWS = argNum("max-rows", Infinity);
const LIVE_MODE =
  hasFlag("live") || String(process.env.GOLF_SG_DISTANCE_LIVE || "").trim() === "1";
const TID_ARG = (() => {
  const hit = args.find((a) => a.startsWith("--tournament-id="));
  return hit ? String(hit.split("=")[1] || "").trim() : "";
})();

function parseYards(s) {
  const t = String(s ?? "").trim();
  if (!t) return NaN;
  if (/in the hole/i.test(t)) return 0;
  if (/yds/i.test(t)) {
    const m = t.match(/([0-9]+(?:\.[0-9]+)?)/);
    return m ? Number(m[1]) : NaN;
  }
  const ft = t.match(/([0-9]+)\s*ft/i);
  if (ft) {
    const inch = t.match(/([0-9]+)\s*in/i);
    return (Number(ft[1]) + (inch ? Number(inch[1]) / 12 : 0)) / 3;
  }
  if (/^[0-9]+\s*in/i.test(t)) {
    const m = t.match(/([0-9]+)/);
    return m ? Number(m[1]) / 36 : NaN;
  }
  const m = t.match(/([0-9]+(?:\.[0-9]+)?)/);
  if (!m) return NaN;
  const n = Number(m[1]);
  return n > 100 ? n : n / 3;
}

function isFairwayCode(code) {
  const c = String(code || "").toUpperCase().trim();
  if (!c) return false;
  if (c.includes("FAIR")) return true;
  return ["ELF", "ERF", "OFW", "ECF", "XFW"].includes(c);
}

function isGreenCode(code) {
  const c = String(code || "").toUpperCase().trim();
  return c === "OGR" || c.includes("GREEN");
}

function isTeeCode(code) {
  const c = String(code || "").toUpperCase().trim();
  return c === "OTB" || c.includes("TEE");
}

function isRoughCode(code) {
  const c = String(code || "").toUpperCase().trim();
  if (!c || isFairwayCode(c) || isGreenCode(c) || isTeeCode(c)) return false;
  if (/BUNK|SAND|WATER|HAZ|OB|PENAL/i.test(c)) return false;
  if (c.includes("ROU") || c === "ORO" || c === "ELR" || c === "ERR" || c === "IR" || c === "OR")
    return true;
  if ((c.includes("IR") || c === "IRF" || c === "ILF") && !c.includes("FAIR")) return true;
  return false;
}

function approachBucket(fromCode, distBefore, par, strokeNum) {
  if (!Number.isFinite(distBefore) || distBefore < 50) return null;
  if (isGreenCode(fromCode)) return null;
  if (isTeeCode(fromCode) && strokeNum === 1 && (par === 4 || par === 5)) return null;
  if (isFairwayCode(fromCode) || (isTeeCode(fromCode) && par === 3)) {
    if (distBefore < 100) return "50_100_fw";
    if (distBefore < 150) return "100_150_fw";
    if (distBefore < 200) return "150_200_fw";
    return "over_200_fw";
  }
  if (isRoughCode(fromCode)) {
    return distBefore < 150 ? "under_150_rgh" : "over_150_rgh";
  }
  if (!isTeeCode(fromCode) && distBefore >= 50) {
    if (distBefore < 100) return "50_100_fw";
    if (distBefore < 150) return "100_150_fw";
    if (distBefore < 200) return "150_200_fw";
    return "over_200_fw";
  }
  return null;
}

function yearFromTid(tid) {
  const m = String(tid || "").match(/R(20\d{2})/i);
  return m ? Number(m[1]) : NaN;
}

function normPgaId(pid) {
  const s = String(pid ?? "").trim();
  if (!s) return "";
  if (/^\d+$/.test(s) && s.length < 5) return s.padStart(5, "0");
  return s;
}

async function loadDgMap() {
  const map = new Map();
  if (!existsSync(MAP)) return map;
  await new Promise((res, rej) => {
    createReadStream(MAP)
      .pipe(parse({ columns: true, relax_column_count: true }))
      .on("data", (r) => {
        const pid = normPgaId(r.pga_player_id);
        const dg = Math.round(Number(r.dg_id));
        if (pid && Number.isFinite(dg)) map.set(pid, dg);
      })
      .on("end", res)
      .on("error", rej);
  });
  return map;
}

async function streamApproachShots(onApproach, opts = {}) {
  const tidFilter = opts.tournamentIds?.size ? opts.tournamentIds : null;
  let rows = 0;
  let approachShots = 0;
  let cur = null;
  let stopped = false;

  function flushHole() {
    if (!cur || !cur.strokes.length) {
      cur = null;
      return;
    }
    const list = cur.strokes;
    const maxSn = Math.max(...list.map((s) => s.sn));
    for (const s of list) {
      const bucket = approachBucket(s.from, s.distBefore, s.par, s.sn);
      if (!bucket) continue;
      const strokesToHole = maxSn - s.sn + 1;
      if (!Number.isFinite(strokesToHole) || strokesToHole < 1) continue;
      approachShots++;
      onApproach(bucket, strokesToHole, cur);
    }
    cur = null;
  }

  await new Promise((resolve, reject) => {
    let settled = false;
    const done = () => {
      if (settled) return;
      settled = true;
      flushHole();
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
        console.log(`  … ${rows.toLocaleString()} rows, ${approachShots.toLocaleString()} approach shots`);
      }

      const tid = String(r.tournament_id || "");
      if (tidFilter && !tidFilter.has(tid)) return;

      const pid = String(r.player_id || "");
      const rnd = Math.round(Number(r.round)) || 1;
      const hole = Math.round(Number(r.hole_number)) || 0;
      const sn = Math.round(Number(r.stroke_number)) || 0;
      if (!tid || !pid || !hole || !sn) return;

      const hk = `${tid}|${pid}|${rnd}|${hole}`;
      if (!cur || cur.hk !== hk) {
        flushHole();
        cur = {
          hk,
          tid,
          pid,
          rnd,
          hole,
          tname: String(r.tournament_name || ""),
          strokes: [],
          lastAfter: NaN,
        };
      }

      const par = Math.round(Number(r.par)) || 4;
      const yardage = Number(r.yardage);
      const after = parseYards(r.distance_remaining);
      let distBefore;
      if (sn === 1) distBefore = Number.isFinite(yardage) ? yardage : NaN;
      else distBefore = Number.isFinite(cur.lastAfter) ? cur.lastAfter : NaN;

      cur.strokes.push({
        sn,
        par,
        from: String(r.from_location_code || "").toUpperCase().trim(),
        distBefore,
      });
      if (Number.isFinite(after)) cur.lastAfter = after;
    });
    parser.on("error", (err) => {
      if (stopped) done();
      else reject(err);
    });
    parser.on("end", done);
    parser.on("close", done);
  });

  return { rows, approachShots };
}

function resolveLiveTournamentIds() {
  const ids = new Set();
  if (TID_ARG) ids.add(TID_ARG);
  for (const id of String(process.env.GOLF_SG_DISTANCE_TOURNAMENT_IDS || "")
    .split(/[,;\s]+/)
    .map((s) => s.trim())
    .filter(Boolean)) {
    ids.add(id);
  }
  if (existsSync(PGA_JSON)) {
    try {
      const j = JSON.parse(readFileSync(PGA_JSON, "utf8"));
      const tid = String(j?.meta?.tournament_id || "").trim();
      if (tid) ids.add(tid);
    } catch {
      /* ignore */
    }
  }
  return ids;
}

function loadBaselinesFromDisk() {
  if (!existsSync(OUT_BASE)) return null;
  try {
    const j = JSON.parse(readFileSync(OUT_BASE, "utf8"));
    const bl = j?.baselines;
    if (!bl || typeof bl !== "object") return null;
    const out = {};
    for (const b of BUCKETS) {
      const mean = Number(bl[b]?.mean_strokes_to_hole);
      if (!Number.isFinite(mean)) return null;
      out[b] = { n: Math.round(Number(bl[b]?.n)) || 0, mean_strokes_to_hole: mean };
    }
    return out;
  } catch {
    return null;
  }
}

async function accumulateRounds(baselineOut, tidFilter) {
  const rounds = new Map();
  const stats = await streamApproachShots(
    (bucket, strokes, meta) => {
      const eMean = baselineOut[bucket]?.mean_strokes_to_hole;
      if (eMean == null) return;
      const sg = eMean - strokes;
      const key = `${meta.tid}|${meta.pid}|${meta.rnd}`;
      let rec = rounds.get(key);
      if (!rec) {
        rec = {
          tournament_id: meta.tid,
          tournament_name: meta.tname,
          pga_player_id: meta.pid,
          round: meta.rnd,
          year: yearFromTid(meta.tid),
          by: Object.fromEntries(BUCKETS.map((b) => [b, { sg: 0, n: 0 }])),
          sg_app_dist_total: 0,
          n_app_dist: 0,
        };
        rounds.set(key, rec);
      }
      rec.by[bucket].sg += sg;
      rec.by[bucket].n += 1;
      rec.sg_app_dist_total += sg;
      rec.n_app_dist += 1;
      if (!rec.tournament_name && meta.tname) rec.tournament_name = meta.tname;
    },
    { tournamentIds: tidFilter },
  );
  return { rounds, stats };
}

const CSV_HEADER = [
  "dg_id",
  "pga_player_id",
  "tournament_id",
  "tournament_name",
  "year",
  "round",
  ...BUCKETS.flatMap((b) => [`sg_${b}`, `n_${b}`]),
  "sg_app_dist_total",
  "n_app_dist",
];

function rowCells(rec, dgMap) {
  const pid = normPgaId(rec.pga_player_id);
  const dg = dgMap.get(pid) ?? dgMap.get(String(rec.pga_player_id)) ?? "";
  const cells = [
    dg,
    pid || rec.pga_player_id,
    rec.tournament_id,
    JSON.stringify(rec.tournament_name || ""),
    rec.year || "",
    rec.round,
  ];
  for (const b of BUCKETS) {
    const x = rec.by[b];
    cells.push(x.n ? (Math.round(x.sg * 1000) / 1000).toFixed(3) : "");
    cells.push(x.n || "");
  }
  cells.push(rec.n_app_dist ? (Math.round(rec.sg_app_dist_total * 1000) / 1000).toFixed(3) : "");
  cells.push(rec.n_app_dist || "");
  return { cells, mapped: dg !== "" };
}

async function writeFullCsv(rounds, dgMap) {
  const out = createWriteStream(OUT_CSV, { encoding: "utf8" });
  out.write(CSV_HEADER.join(",") + "\n");
  let nOut = 0;
  let nMapped = 0;
  for (const rec of rounds.values()) {
    const { cells, mapped } = rowCells(rec, dgMap);
    if (mapped) nMapped++;
    out.write(cells.join(",") + "\n");
    nOut++;
  }
  out.end();
  await finished(out);
  return { nOut, nMapped };
}

async function upsertCsvForTournaments(rounds, dgMap, tournamentIds) {
  const kept = [];
  if (existsSync(OUT_CSV)) {
    await new Promise((res, rej) => {
      createReadStream(OUT_CSV)
        .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
        .on("data", (r) => {
          if (tournamentIds.has(String(r.tournament_id || "").trim())) return;
          kept.push(r);
        })
        .on("end", res)
        .on("error", rej);
    });
  }
  const out = createWriteStream(OUT_CSV, { encoding: "utf8" });
  out.write(CSV_HEADER.join(",") + "\n");
  for (const r of kept) {
    out.write(
      CSV_HEADER.map((h) => {
        const v = r[h];
        if (h === "tournament_name") return JSON.stringify(String(v ?? ""));
        return v == null ? "" : String(v);
      }).join(",") + "\n",
    );
  }
  let nNew = 0;
  let nMapped = 0;
  for (const rec of rounds.values()) {
    const { cells, mapped } = rowCells(rec, dgMap);
    if (mapped) nMapped++;
    out.write(cells.join(",") + "\n");
    nNew++;
  }
  out.end();
  await finished(out);
  return { nKept: kept.length, nNew, nMapped };
}

async function runFullRebuild(dgMap) {
  console.log("Pass 1/2 — tour baselines…");
  const baseline = new Map();
  for (const b of BUCKETS) baseline.set(b, { sum: 0, n: 0 });
  const pass1 = await streamApproachShots((bucket, strokes) => {
    const bl = baseline.get(bucket);
    bl.sum += strokes;
    bl.n += 1;
  });
  const baselineOut = {};
  for (const b of BUCKETS) {
    const bl = baseline.get(b);
    baselineOut[b] = {
      n: bl.n,
      mean_strokes_to_hole: bl.n ? bl.sum / bl.n : null,
    };
  }
  console.log(
    `  scanned ${pass1.rows.toLocaleString()} rows, ${pass1.approachShots.toLocaleString()} approach shots`,
  );
  for (const b of BUCKETS) {
    const x = baselineOut[b];
    console.log(
      `  ${b}: n=${x.n.toLocaleString()}  E=${x.mean_strokes_to_hole != null ? x.mean_strokes_to_hole.toFixed(3) : "—"}`,
    );
  }

  console.log("Pass 2/2 — player-round SG…");
  const { rounds, stats: pass2 } = await accumulateRounds(baselineOut, null);
  console.log(`  ${rounds.size.toLocaleString()} player-rounds`);

  writeFileSync(
    OUT_BASE,
    JSON.stringify(
      {
        meta: {
          source_shots: SHOTS,
          method:
            "Empirical strokes-gained vs tour mean strokes-to-hole by distance/lie bucket (from pgatouR shot traces).",
          note: "Not official PGA Tour / DataGolf ShotLink SG. Approach shots only (≥50 yds).",
          rows_scanned: pass2.rows,
          approach_shots: pass2.approachShots,
          player_rounds: rounds.size,
          created_at: new Date().toISOString(),
        },
        baselines: baselineOut,
      },
      null,
      2,
    ),
  );
  const { nOut, nMapped } = await writeFullCsv(rounds, dgMap);
  console.log(`\nWrote ${nOut.toLocaleString()} player-rounds → ${OUT_CSV}`);
  console.log(`  with dg_id: ${nMapped.toLocaleString()}`);
  console.log(`Baselines → ${OUT_BASE}`);
}

if (!existsSync(SHOTS)) {
  console.error("Missing shot file:", SHOTS);
  process.exit(1);
}

console.log("Loading dg map…");
const dgMap = await loadDgMap();
console.log(`  ${dgMap.size} mapped PGA→DG ids`);

if (LIVE_MODE) {
  const liveTids = resolveLiveTournamentIds();
  const baselineOut = loadBaselinesFromDisk();
  if (liveTids.size && baselineOut) {
    console.log(`[sg-distance:live] Upserting: ${[...liveTids].join(", ")}`);
    const { rounds, stats } = await accumulateRounds(baselineOut, liveTids);
    console.log(
      `  ${stats.approachShots.toLocaleString()} approach shots → ${rounds.size} player-rounds`,
    );
    if (!rounds.size) {
      console.warn(
        "[sg-distance:live] No approach shots for current tournament — ensure refresh:shots ran.",
      );
      process.exit(0);
    }
    const up = await upsertCsvForTournaments(rounds, dgMap, liveTids);
    console.log(`Upserted ${up.nNew} rows (kept ${up.nKept}) → ${OUT_CSV}`);
    try {
      const prev = JSON.parse(readFileSync(OUT_BASE, "utf8"));
      prev.meta = {
        ...(prev.meta || {}),
        last_live_upsert_at: new Date().toISOString(),
        last_live_tournaments: [...liveTids],
        last_live_player_rounds: rounds.size,
      };
      writeFileSync(OUT_BASE, JSON.stringify(prev, null, 2));
    } catch {
      /* ignore */
    }
    process.exit(0);
  }
  if (!baselineOut) {
    console.log("[sg-distance:live] Baselines missing — full rebuild…");
  } else {
    console.warn("[sg-distance:live] No tournament_id — full rebuild…");
  }
}

await runFullRebuild(dgMap);
