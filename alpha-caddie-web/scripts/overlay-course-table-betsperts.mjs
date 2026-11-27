#!/usr/bin/env node
/**
 * Prefer Betsperts course facts/metrics as the primary course-table source.
 *
 * Reads alpha-caddie-web/course-table.json (CSV build) + data/betsperts_course_facts.json,
 * overlays Betsperts yardage/par/accuracy/GIR/score/RTP/skill demand onto matching rows,
 * adds Betsperts-only courses, recomputes means/ranges, and rewrites course-table.json.
 *
 * DataGolf CSV remains fallback for axes Betsperts does not publish (ARG bunker splits, etc.).
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { robustWriteFileSync } from "./robust-copy-file.mjs";
import { BETSPERTS_COURSE_FACTS_WEB, BETSPERTS_COURSE_FACTS_ROOT } from "./betsperts-course-context.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const CT_PATH = join(WEB, "course-table.json");

const RADAR_KEYS = [
  "adj_driving_accuracy",
  "ott_sg",
  "app_sg",
  "arg_sg",
  "putt_sg",
  "adj_driving_distance",
];

const NUMERIC_KEYS = [
  "par",
  "yardage",
  "yardage_4_5",
  "yardage_3",
  "adj_score_to_par",
  "adj_par_3_score",
  "adj_par_4_score",
  "adj_par_5_score",
  "adj_driving_distance",
  "adj_sd_distance",
  "adj_driving_accuracy",
  "putt_sg",
  "arg_sg",
  "app_sg",
  "ott_sg",
  "fw_width",
  "fw_diff",
  "rgh_diff",
  "non_rgh_diff",
  "miss_fw_pen_frac",
  "adj_gir",
  "less_150_sg",
  "greater_150_sg",
  "arg_fairway_sg",
  "arg_rough_sg",
  "arg_bunker_sg",
  "less_5_ft_sg",
  "greater_5_less_15_sg",
  "greater_15_sg",
  "adj_penalties",
  "adj_ob",
];

function num(v, fallback = NaN) {
  const n = Number(String(v ?? "").replace(/,/g, "").trim());
  return Number.isFinite(n) ? n : fallback;
}

/** Map Betsperts skill labels → course-table SG demand coeffs. */
function labelToCoeff(label) {
  const s = String(label || "")
    .trim()
    .toLowerCase();
  if (!s) return NaN;
  if (s.includes("very difficult") || s.includes("extremely")) return 0.12;
  if (s.includes("difficult") || s.includes("hard")) return 0.08;
  if (s.includes("average") || s.includes("neutral") || s.includes("moderate")) return 0.02;
  if (s.includes("very easy")) return -0.12;
  if (s.includes("easy")) return -0.05;
  return NaN;
}

function pctToFrac(pct) {
  const n = num(pct, NaN);
  if (!Number.isFinite(n)) return NaN;
  return n > 1.5 ? n / 100 : n;
}

function loadFacts() {
  for (const p of [BETSPERTS_COURSE_FACTS_WEB, BETSPERTS_COURSE_FACTS_ROOT]) {
    if (!existsSync(p)) continue;
    try {
      return JSON.parse(readFileSync(p, "utf8"));
    } catch {
      /* try next */
    }
  }
  return null;
}

function factToPartialRow(fact) {
  if (!fact || typeof fact !== "object") return null;
  const ms = fact.metric_summary || {};
  const labels = fact.skill_labels || {};
  const ott = fact.metrics?.["Off the Tee"]?.five_year || {};
  const sg = fact.metrics?.["Strokes Gained"]?.five_year || {};
  const scoring = fact.metrics?.["Scoring"]?.five_year || {};

  const row = {
    course: String(fact.name || "").trim(),
    _normKey: fact.key || normCourseNameKey(fact.name),
    _source: "betsperts",
    _betsperts_course_num: String(fact.course_num || ""),
    _betsperts_correlated: Array.isArray(fact.correlated)
      ? fact.correlated.map((c) => c.key || normCourseNameKey(c.name)).filter(Boolean)
      : [],
    _betsperts_grass: fact.grass || null,
    _betsperts_hole_stats_n: num(fact.hole_stats_n, 0),
  };

  const par = num(fact.par, NaN);
  if (Number.isFinite(par)) row.par = par;
  const yards = num(fact.yards, NaN);
  if (Number.isFinite(yards)) row.yardage = yards;

  const dist = num(ms.driving_distance, NaN);
  if (Number.isFinite(dist)) row.adj_driving_distance = dist;

  const acc = pctToFrac(ms.driving_accuracy_pct);
  if (Number.isFinite(acc)) row.adj_driving_accuracy = acc;

  const gir = pctToFrac(ms.gir_pct);
  if (Number.isFinite(gir)) row.adj_gir = gir;

  const rtp = num(fact.score_rtp, NaN);
  if (Number.isFinite(rtp)) row.adj_score_to_par = rtp;

  const ottC = labelToCoeff(labels.ott);
  const appC = labelToCoeff(labels.app);
  const argC = labelToCoeff(labels.arg);
  const puttC = labelToCoeff(labels.putt);
  if (Number.isFinite(ottC)) row.ott_sg = ottC;
  if (Number.isFinite(appC)) row.app_sg = appC;
  if (Number.isFinite(argC)) row.arg_sg = argC;
  if (Number.isFinite(puttC)) row.putt_sg = puttC;

  // Accuracy label also nudges driving-accuracy demand when numeric % missing.
  const fwyLabel = labelToCoeff(labels.fwy_accuracy);
  if (!Number.isFinite(row.adj_driving_accuracy) && Number.isFinite(fwyLabel)) {
    // Difficult fairways → lower hit rate proxy for demand context
    row.adj_driving_accuracy = clamp01(0.6 - fwyLabel);
  }

  const leftRtp = num(ott["Left Rough Tendency RTP"], NaN);
  const rightRtp = num(ott["Right Rough Tendency RTP"], NaN);
  const roughRtps = [leftRtp, rightRtp].filter(Number.isFinite);
  if (roughRtps.length) {
    const avg = roughRtps.reduce((a, b) => a + b, 0) / roughRtps.length;
    // RTP is strokes vs fairway (negative = harder). Store absolute difficulty.
    row.rgh_diff = Math.abs(avg);
    row.fw_diff = Math.max(0, -avg);
  }

  const p3 = num(sg["Par 3 Scoring AVG"], NaN);
  const p4 = num(sg["Par 4 Scoring AVG"], NaN);
  const p5 = num(sg["Par 5 Scoring AVG"], NaN);
  if (Number.isFinite(p3)) row.adj_par_3_score = p3 - 3;
  if (Number.isFinite(p4)) row.adj_par_4_score = p4 - 4;
  if (Number.isFinite(p5)) row.adj_par_5_score = p5 - 5;

  const scoringAvg = num(ms.scoring_avg ?? scoring["Scoring AVG"], NaN);
  if (Number.isFinite(scoringAvg) && Number.isFinite(par) && !Number.isFinite(row.adj_score_to_par)) {
    row.adj_score_to_par = scoringAvg - par;
  }

  return row.course ? row : null;
}

function clamp01(x) {
  return Math.max(0, Math.min(1, x));
}

function mergeRow(base, overlay) {
  const out = { ...(base || {}) };
  for (const [k, v] of Object.entries(overlay || {})) {
    if (v == null) continue;
    if (typeof v === "number" && !Number.isFinite(v)) continue;
    // Prefer Betsperts for all overlapping numeric / meta fields.
    if (k.startsWith("_") || NUMERIC_KEYS.includes(k) || k === "course") {
      out[k] = v;
    }
  }
  out._source = overlay?._source || out._source || "merged";
  return out;
}

function recomputeStats(rows) {
  const ranges = {};
  for (const k of [...RADAR_KEYS, ...NUMERIC_KEYS]) {
    ranges[k] = { lo: Infinity, hi: -Infinity };
  }
  for (const row of rows) {
    for (const k of Object.keys(ranges)) {
      const n = row[k];
      if (!Number.isFinite(n)) continue;
      ranges[k].lo = Math.min(ranges[k].lo, n);
      ranges[k].hi = Math.max(ranges[k].hi, n);
    }
  }
  for (const r of Object.values(ranges)) {
    if (!Number.isFinite(r.lo) || !Number.isFinite(r.hi) || r.hi - r.lo < 1e-9) {
      r.lo = 0;
      r.hi = 1;
    }
  }
  const means = {};
  for (const k of [...RADAR_KEYS, ...NUMERIC_KEYS]) {
    let s = 0;
    let c = 0;
    for (const row of rows) {
      if (Number.isFinite(row[k])) {
        s += row[k];
        c++;
      }
    }
    means[k] = c ? s / c : NaN;
  }
  return { ranges, means };
}

function main() {
  if (!existsSync(CT_PATH)) {
    console.error("[overlay-course-table-betsperts] missing", CT_PATH);
    process.exit(1);
  }
  const facts = loadFacts();
  if (!facts?.byKey || !Object.keys(facts.byKey).length) {
    console.warn("[overlay-course-table-betsperts] no betsperts_course_facts.json — leaving course-table unchanged");
    process.exit(0);
  }

  const ct = JSON.parse(readFileSync(CT_PATH, "utf8"));
  const byNormKey = { ...(ct.byNormKey || {}) };
  const rowsByKey = new Map();
  for (const row of ct.rows || []) {
    const nk = row._normKey || normCourseNameKey(row.course);
    if (!nk) continue;
    rowsByKey.set(nk, { ...row, _normKey: nk });
  }

  let overlaid = 0;
  let added = 0;
  for (const fact of Object.values(facts.byKey)) {
    const partial = factToPartialRow(fact);
    if (!partial?._normKey) continue;
    const nk = partial._normKey;
    // Fuzzy match when exact key differs
    let base = rowsByKey.get(nk) || byNormKey[nk] || null;
    if (!base) {
      for (const [k, row] of rowsByKey) {
        if (k.includes(nk) || nk.includes(k)) {
          base = row;
          break;
        }
      }
    }
    if (base) {
      const merged = mergeRow(base, partial);
      rowsByKey.set(base._normKey || nk, merged);
      byNormKey[base._normKey || nk] = merged;
      // Also index under Betsperts key
      byNormKey[nk] = merged;
      overlaid++;
    } else {
      // Minimal row so Course Fit can resolve Betsperts-only venues
      const stub = mergeRow(
        {
          course: partial.course,
          _normKey: nk,
          par: null,
          yardage: null,
          adj_driving_accuracy: null,
          ott_sg: null,
          app_sg: null,
          arg_sg: null,
          putt_sg: null,
          adj_driving_distance: null,
        },
        partial,
      );
      rowsByKey.set(nk, stub);
      byNormKey[nk] = stub;
      added++;
    }
  }

  const rows = [...rowsByKey.values()];
  const { ranges, means } = recomputeStats(rows);
  const payload = {
    ...ct,
    version: Math.max(2, num(ct.version, 1) + 0),
    generatedAt: new Date().toISOString(),
    sourceCsv: ct.sourceCsv,
    sourcePrimary: "betsperts",
    betspertsOverlay: {
      overlaid,
      added,
      facts_n: facts.n,
      facts_generatedAt: facts.generatedAt || null,
    },
    radarKeys: ct.radarKeys?.length === 6 ? ct.radarKeys : [...RADAR_KEYS],
    ranges,
    means,
    byNormKey,
    rows,
  };

  const wrote = robustWriteFileSync(CT_PATH, JSON.stringify(payload));
  if (!wrote.ok) throw wrote.error || new Error(`write failed: ${CT_PATH}`);
  console.log(
    `[overlay-course-table-betsperts] wrote ${CT_PATH} (overlaid=${overlaid} added=${added} rows=${rows.length})`,
  );
}

main();
