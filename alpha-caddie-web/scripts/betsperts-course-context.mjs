/**
 * Load / normalize Betsperts course facts + year metrics for projections & Plus4 COR.
 *
 * Source files: ../data/betsperts/courses/by-id/*.json
 * Sidecar: alpha-caddie-web/data/betsperts_course_facts.json
 */
import { existsSync, readdirSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");

export const BETSPERTS_COURSES_DIR = join(ROOT, "data", "betsperts", "courses", "by-id");
export const BETSPERTS_COURSE_FACTS_WEB = join(WEB, "data", "betsperts_course_facts.json");
export const BETSPERTS_COURSE_FACTS_ROOT = join(ROOT, "data", "betsperts_course_facts.json");

export const METRIC_VIEWS = Object.freeze([
  "Off the Tee",
  "Approach",
  "Around the Green",
  "Putting",
  "Scoring",
  "Strokes Gained",
]);

function num(v, fallback = NaN) {
  const n = Number(String(v ?? "").replace(/,/g, "").trim());
  return Number.isFinite(n) ? n : fallback;
}

function pickYearRow(rows, prefer = "5-Year Course AVG") {
  if (!Array.isArray(rows) || !rows.length) return null;
  const hit = rows.find((r) => String(r.year || "").trim() === prefer);
  if (hit) return hit;
  // Prefer most recent calendar year over PGA Tour AVG
  const years = rows
    .filter((r) => /^\d{4}$/.test(String(r.year || "").trim()))
    .sort((a, b) => Number(b.year) - Number(a.year));
  return years[0] || rows[0];
}

function parseMetricNum(row, ...keys) {
  if (!row) return NaN;
  for (const k of keys) {
    if (row[k] != null && String(row[k]).trim() !== "") {
      const n = num(row[k], NaN);
      if (Number.isFinite(n)) return n;
    }
  }
  return NaN;
}

/**
 * Normalize Betsperts hole_stats/get_breakdown rows into the Hole Stats UI schema
 * (Hole, Par, Yards, AVG, AVG +/-, Rank, Eagles, Birdies, Pars, Bogeys, Doubles/+).
 * Prefers one row per hole (latest / highest AvgTotal when duplicates).
 */
export function normalizeHoleStatsRows(rawRows) {
  if (!Array.isArray(rawRows) || !rawRows.length) return [];
  const byHole = new Map();
  for (const r of rawRows) {
    const hole = Math.round(num(r.Hole ?? r.hole, NaN));
    if (!Number.isFinite(hole) || hole < 1 || hole > 18) continue;
    const sample = num(r.AvgTotal ?? r.avg_total, 0);
    const prev = byHole.get(hole);
    if (prev && num(prev._sample, 0) >= sample) continue;
    byHole.set(hole, {
      hole,
      par: Math.round(num(r.AvgPar ?? r.par, NaN)),
      yards: Math.round(num(r.AvgYards ?? r.yards, NaN)),
      avg: num(r.AvgScore ?? r.avg, NaN),
      avg_plus_minus: num(r.Avg_plus_minus ?? r.avg_plus_minus, NaN),
      rank: Math.round(num(r.HoleRank ?? r.Rank ?? r.rank, NaN)),
      eagles_pct: num(r.AvgEaglesPct ?? r.eagles_pct, NaN),
      birdies_pct: num(r.AvgBirdiesPct ?? r.birdies_pct, NaN),
      pars_pct: num(r.AvgParsPct ?? r.pars_pct, NaN),
      bogeys_pct: num(r.AvgBogeysPct ?? r.bogeys_pct, NaN),
      doubles_plus_pct: num(r.AvgDblBogOtherPct ?? r.doubles_plus_pct, NaN),
      tournament: String(r.TourneyName ?? r.tournament ?? "").trim() || null,
      tnum: String(r.TNum ?? r.tnum ?? "").trim() || null,
      _sample: sample,
    });
  }
  return [...byHole.values()]
    .map(({ _sample, ...row }) => row)
    .sort((a, b) => a.hole - b.hole);
}

/** Normalize one stored course JSON into a facts+metrics+hole-stats record. */
export function normalizeBetspertsCoursePayload(payload) {
  if (!payload || typeof payload !== "object") return null;
  const d = payload.details && typeof payload.details === "object" ? payload.details : {};
  const name = String(payload.courseName || d.CourseName || payload.list?.CourseName || "").trim();
  if (!name) return null;
  const key = normCourseNameKey(name);
  const holeStats = normalizeHoleStatsRows(
    Array.isArray(payload.holeStats) && payload.holeStats.length
      ? payload.holeStats
      : payload.auth?.holeBreakdown,
  );
  const correlatedNames = Array.isArray(d.CorrelatedCourse)
    ? d.CorrelatedCourse.map((x) => String(x || "").trim()).filter(Boolean)
    : [];
  const correlatedAuth = Array.isArray(payload.auth?.correlated)
    ? payload.auth.correlated
        .map((x) => ({
          courseNum: String(x.CourseNum ?? x.courseNum ?? ""),
          name: String(x.CourseName ?? x.courseName ?? "").trim(),
          key: normCourseNameKey(x.CourseName ?? x.courseName ?? ""),
        }))
        .filter((x) => x.key)
    : [];

  const metricsRaw =
    payload.auth?.metrics && typeof payload.auth.metrics === "object" ? payload.auth.metrics : {};
  const metrics = {};
  for (const view of METRIC_VIEWS) {
    const rows = Array.isArray(metricsRaw[view]) ? metricsRaw[view] : [];
    const avg = pickYearRow(rows, "5-Year Course AVG");
    const tour = rows.find((r) => String(r.year || "").trim() === "PGA Tour AVG") || null;
    metrics[view] = {
      rows,
      five_year: avg || null,
      tour_avg: tour || null,
    };
  }

  const ott = metrics["Off the Tee"]?.five_year;
  const ottTour = metrics["Off the Tee"]?.tour_avg;
  const app = metrics["Approach"]?.five_year;
  const appTour = metrics["Approach"]?.tour_avg;
  const arg = metrics["Around the Green"]?.five_year;
  const putt = metrics["Putting"]?.five_year;
  const scoring = metrics["Scoring"]?.five_year;
  const scoringTour = metrics["Scoring"]?.tour_avg;

  return {
    key,
    course_num: String(payload.courseNum || d.CourseNum || ""),
    name,
    locations: Array.isArray(d.Locations) ? d.Locations : [],
    course_types: Array.isArray(d.CourseTypes) ? d.CourseTypes : [],
    par: num(d.ParScore, NaN),
    yards: num(d.Yards, NaN),
    architects: Array.isArray(d.Architects) ? d.Architects : [],
    redesign_architects: Array.isArray(d.RedesignArchitects) ? d.RedesignArchitects : [],
    elevation_ft: num(d.Elevation, NaN),
    bunkers: num(d.Bunkers, NaN),
    water_holes: num(d.WaterHoles, NaN),
    difficulty: d.Difficulty || null,
    score_rtp: num(d.ScoreRTP, NaN),
    grass: {
      greens: d.GreenGrass ? [String(d.GreenGrass)] : [],
      fairways: d.FwyGrass ? [String(d.FwyGrass)] : [],
      rough: d.RoughGrass ? [String(d.RoughGrass)] : [],
      surrounds: d.RoughType ? [String(d.RoughType)] : [],
      green_stimp: num(d.GreenStimp, NaN),
      green_size_sqft: num(d.GreenSqFt, NaN),
      rough_length_in: num(d.RoughLengthInc, NaN),
    },
    skill_labels: {
      ott: d.GainOTT || null,
      app: d.GainAPP || null,
      arg: d.GainARG || null,
      putt: d.GainPutting || null,
      fwy_accuracy: d.FwyAccuracy || null,
      gir_accuracy: d.GIRAccuracy || null,
      ott_club: d.OTTClub || null,
    },
    rates: {
      fwy_pct: num(d.FwyPct, NaN),
      gir_pct: num(d.GIRPct, NaN),
    },
    correlated_names: correlatedNames,
    correlated: correlatedAuth.length
      ? correlatedAuth
      : correlatedNames.map((n) => ({ name: n, key: normCourseNameKey(n), courseNum: "" })),
    metrics,
    metric_summary: {
      driving_distance: parseMetricNum(ott, "Driving Distance (yds)"),
      driver_usage_pct: parseMetricNum(ott, "Driver Usage %"),
      driving_accuracy_pct: parseMetricNum(ott, "Driving Accuracy %"),
      tour_driving_accuracy_pct: parseMetricNum(ottTour, "Driving Accuracy %"),
      gir_pct: parseMetricNum(app, "GIR %"),
      tour_gir_pct: parseMetricNum(appTour, "GIR %"),
      scrambling_pct: parseMetricNum(arg, "Scrambling %"),
      sand_save_pct: parseMetricNum(arg, "Sand Save %"),
      one_putt_pct: parseMetricNum(putt, "One-Putt %"),
      three_putt_avd_pct: parseMetricNum(putt, "3-Putt AVD %"),
      gir_putting_avg: parseMetricNum(putt, "GIR Putting Average"),
      scoring_avg: parseMetricNum(scoring, "Scoring AVG"),
      bob_pct: parseMetricNum(scoring, "Birdie or Better %"),
      tour_bob_pct: parseMetricNum(scoringTour, "Birdie or Better %"),
      bogey_avd_pct: parseMetricNum(scoring, "Bogey AVD %"),
    },
    hole_stats: holeStats,
    hole_stats_n: holeStats.length,
    pulled_at: payload.auth?._meta?.pulledAt || payload._meta?.pulledAt || null,
    has_metrics: Boolean(
      metrics["Off the Tee"]?.five_year || metrics["Approach"]?.five_year || metrics["Scoring"]?.five_year,
    ),
    has_hole_stats: holeStats.length >= 18,
  };
}

export function buildBetspertsCourseFactsIndex(coursesDir = BETSPERTS_COURSES_DIR) {
  const byKey = {};
  const byNum = {};
  if (!existsSync(coursesDir)) {
    return { generatedAt: new Date().toISOString(), n: 0, byKey, byNum };
  }
  for (const f of readdirSync(coursesDir).filter((x) => x.endsWith(".json"))) {
    let payload;
    try {
      payload = JSON.parse(readFileSync(join(coursesDir, f), "utf8"));
    } catch {
      continue;
    }
    const rec = normalizeBetspertsCoursePayload(payload);
    if (!rec) continue;
    byKey[rec.key] = rec;
    if (rec.course_num) byNum[rec.course_num] = rec.key;
    const alt = normCourseNameKey(payload.list?.CourseName || "");
    if (alt && !byKey[alt]) byKey[alt] = rec;
  }
  const vals = Object.values(byKey);
  return {
    generatedAt: new Date().toISOString(),
    source: "data/betsperts/courses/by-id",
    n: vals.length,
    n_with_metrics: vals.filter((r) => r.has_metrics).length,
    n_with_hole_stats: vals.filter((r) => r.has_hole_stats).length,
    byKey,
    byNum,
  };
}

let _factsCache = null;
export function loadBetspertsCourseFacts(force = false) {
  if (_factsCache && !force) return _factsCache;
  for (const p of [BETSPERTS_COURSE_FACTS_WEB, BETSPERTS_COURSE_FACTS_ROOT]) {
    if (!existsSync(p)) continue;
    try {
      _factsCache = JSON.parse(readFileSync(p, "utf8"));
      return _factsCache;
    } catch {
      /* try next */
    }
  }
  _factsCache = buildBetspertsCourseFactsIndex();
  return _factsCache;
}

export function resolveBetspertsCourseFacts(courseLabel, facts = null) {
  const idx = facts || loadBetspertsCourseFacts();
  const ck = normCourseNameKey(courseLabel);
  if (!ck || !idx?.byKey) return null;
  if (idx.byKey[ck]) return idx.byKey[ck];
  for (const [k, rec] of Object.entries(idx.byKey)) {
    if (k.includes(ck) || ck.includes(k)) return rec;
  }
  return null;
}

/**
 * Fill thin venue historical baselines from Betsperts 5-yr course metrics / facts.
 * Mutates venueScoring; returns { applied, facts } meta.
 */
export function enrichVenueScoringWithBetsperts(venueScoring, courseLabel, opts = {}) {
  const facts = resolveBetspertsCourseFacts(courseLabel);
  const out = { applied: false, facts: null, fills: [] };
  if (!venueScoring || !facts) return out;
  out.facts = {
    key: facts.key,
    name: facts.name,
    course_num: facts.course_num,
    green_grass: facts.grass?.greens?.[0] || null,
    correlated: (facts.correlated || []).slice(0, 10).map((c) => c.name || c.key),
    metric_summary: facts.metric_summary,
    rates: facts.rates,
    skill_labels: facts.skill_labels,
  };

  const nFw = Math.max(10, Math.min(16, Math.round(num(opts.nFw, 14)) || 14));
  const thin = (v, minN = 40) =>
    !Number.isFinite(v) || num(venueScoring.nVenueRounds, 0) < minN;

  const ms = facts.metric_summary || {};
  const rates = facts.rates || {};

  // Fairways: Betsperts Driving Accuracy % → expected hits on nFw scale
  const fwPct = Number.isFinite(ms.driving_accuracy_pct)
    ? ms.driving_accuracy_pct
    : rates.fwy_pct;
  if (Number.isFinite(fwPct) && thin(venueScoring.venueAvgFairways, 60)) {
    const hits = (fwPct / 100) * nFw;
    if (hits > 2 && hits < nFw + 0.5) {
      venueScoring.venueAvgFairways = Math.round(hits * 100) / 100;
      venueScoring.betsperts_fairways_pct = fwPct;
      out.fills.push("fairways");
      out.applied = true;
    }
  }

  // GIR: GIR % → expected greens / 18
  const girPct = Number.isFinite(ms.gir_pct) ? ms.gir_pct : rates.gir_pct;
  if (Number.isFinite(girPct) && thin(venueScoring.venueAvgGir, 60)) {
    const gir = (girPct / 100) * 18;
    if (gir > 4 && gir < 18.5) {
      venueScoring.venueAvgGir = Math.round(gir * 100) / 100;
      venueScoring.betsperts_gir_pct = girPct;
      out.fills.push("gir");
      out.applied = true;
    }
  }

  // Birdies: Birdie or Better % of holes → count / 18
  if (Number.isFinite(ms.bob_pct) && thin(venueScoring.venueAvgBirdies, 60)) {
    const bob = (ms.bob_pct / 100) * 18;
    if (bob > 0.5 && bob < 12) {
      venueScoring.venueAvgBirdies = Math.round(bob * 100) / 100;
      venueScoring.betsperts_bob_pct = ms.bob_pct;
      out.fills.push("birdies");
      out.applied = true;
    }
  }

  // Bogeys: Betsperts "Bogey AVD %" is a hole bogey rate (~12–15%), not an avoidance share.
  if (Number.isFinite(ms.bogey_avd_pct) && thin(venueScoring.venueAvgBogeys, 60)) {
    const bogeys = (ms.bogey_avd_pct / 100) * 18;
    if (bogeys > 0.5 && bogeys < 12) {
      venueScoring.venueAvgBogeys = Math.round(bogeys * 100) / 100;
      venueScoring.betsperts_bogey_avd_pct = ms.bogey_avd_pct;
      out.fills.push("bogeys");
      out.applied = true;
    }
  }

  // Putts: GIR Putting Average × expected GIR + ~2.05 on non-GIR holes.
  if (Number.isFinite(ms.gir_putting_avg) && thin(venueScoring.venueAvgPutts, 60)) {
    const girHoles = Number.isFinite(girPct) ? (girPct / 100) * 18 : Number.isFinite(venueScoring.venueAvgGir)
      ? venueScoring.venueAvgGir
      : 12;
    const puttProxy = girHoles * ms.gir_putting_avg + Math.max(0, 18 - girHoles) * 2.05;
    if (puttProxy >= 26 && puttProxy <= 34) {
      venueScoring.venueAvgPutts = Math.round(puttProxy * 100) / 100;
      venueScoring.betsperts_gir_putting_avg = ms.gir_putting_avg;
      out.fills.push("putts");
      out.applied = true;
    }
  }

  // Score to par from ScoreRTP when venue STP missing
  if (Number.isFinite(facts.score_rtp) && thin(venueScoring.venueAvgStp, 40)) {
    venueScoring.venueAvgStp = Math.round(facts.score_rtp * 100) / 100;
    venueScoring.betsperts_score_rtp = facts.score_rtp;
    out.fills.push("stp");
    out.applied = true;
  }

  venueScoring.betsperts_course = out.facts;
  if (out.applied && !venueScoring.source) venueScoring.source = "betsperts_course_metrics";
  else if (out.applied) venueScoring.source = `${venueScoring.source}+betsperts`;

  return out;
}

/** Player SG on green surfaces from Betsperts player greenGrass endpoint. */
export function loadPlayerGreenGrassByDg(opts = {}) {
  const mapPath = opts.mapPath || join(ROOT, "data", "pga_datagolf_player_map.csv");
  const playersDir = opts.playersDir || join(ROOT, "data", "betsperts", "players", "by-id");
  const out = new Map();
  if (!existsSync(playersDir)) return out;

  const pgaToDg = new Map();
  if (existsSync(mapPath)) {
    try {
      const rows = parse(readFileSync(mapPath, "utf8"), {
        columns: true,
        skip_empty_lines: true,
        relax_column_count: true,
        bom: true,
      });
      for (const r of rows) {
        const pga = String(r.pga_player_id || "").replace(/\D/g, "");
        const dg = Math.round(num(r.dg_id, NaN));
        if (pga && Number.isFinite(dg)) pgaToDg.set(pga, dg);
      }
    } catch {
      /* optional */
    }
  }

  for (const f of readdirSync(playersDir).filter((x) => x.endsWith(".json"))) {
    const pgaId = f.replace(/\.json$/i, "").replace(/\D/g, "");
    const dg = pgaToDg.get(pgaId);
    if (!Number.isFinite(dg)) continue;
    let j;
    try {
      j = JSON.parse(readFileSync(join(playersDir, f), "utf8"));
    } catch {
      continue;
    }
    const gg = j.stats?.greenGrass || j.greenGrass || [];
    if (!Array.isArray(gg) || !gg.length) continue;
    const rows = gg
      .map((r) => ({
        surface: String(r.GreenSurface || r.surface || "")
          .toLowerCase()
          .trim(),
        sg: num(r.StrokesGainedAverage ?? r.sg, NaN),
        n: Math.round(num(r.Rounds ?? r.n, 0)),
      }))
      .filter((r) => r.surface && Number.isFinite(r.sg));
    if (rows.length) out.set(dg, rows);
  }
  return out;
}

/** Absolute SG for a player on a given green surface token (e.g. bermuda). */
export function playerGreenSurfaceSg(greenGrassRows, surfaceToken) {
  const want = String(surfaceToken || "")
    .toLowerCase()
    .trim();
  if (!want || !Array.isArray(greenGrassRows) || !greenGrassRows.length) return { sg: NaN, n: 0 };
  let best = null;
  for (const r of greenGrassRows) {
    if (!r.surface) continue;
    if (r.surface === want || r.surface.includes(want) || want.includes(r.surface)) {
      if (!best || r.n > best.n) best = r;
    }
  }
  if (!best) return { sg: NaN, n: 0 };
  return { sg: best.sg, n: best.n };
}

/** Relative SG vs player's cross-surface mean (positive = better on this grass). */
export function playerGreenSurfaceSgRelative(greenGrassRows, surfaceToken) {
  const hit = playerGreenSurfaceSg(greenGrassRows, surfaceToken);
  if (!Number.isFinite(hit.sg) || !Array.isArray(greenGrassRows) || greenGrassRows.length < 2) {
    return { sg: NaN, n: hit.n, relative: NaN };
  }
  let wSum = 0;
  let vSum = 0;
  for (const r of greenGrassRows) {
    const w = Math.max(1, Math.sqrt(r.n || 1));
    wSum += w;
    vSum += w * r.sg;
  }
  const mean = wSum > 0 ? vSum / wSum : NaN;
  if (!Number.isFinite(mean)) return { sg: hit.sg, n: hit.n, relative: NaN };
  return { sg: hit.sg, n: hit.n, relative: hit.sg - mean };
}
