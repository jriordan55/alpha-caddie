#!/usr/bin/env node
/**
 * Reads repo data/course_table.csv and writes alpha-caddie-web/course-table.json
 * for browser course mapping (Course Fit, Historical Trends alignment, etc.).
 * Keep normCourseNameKeyFetch in sync with app.js normCourseNameKey.
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";
import { resolveGolfModelDir } from "./resolve-golf-model-dir.mjs";
import { formatCourseLabelForDisplay } from "./course-name-key.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = resolveGolfModelDir(WEB_ROOT);

function normCourseNameKeyFetch(raw) {
  let s = String(raw || "").trim().toLowerCase();
  s = s.replace(/\([^)]*\)/g, " ");
  s = s.replace(/\b(blue monster|stadium course|championship course|club de golf)\b/g, " ");
  s = s.replace(/&/g, " and ");
  s = s.replace(/\bthe players\b/gi, " ");
  s = s.replace(/\bc\.?\s*c\.?\b/gi, "country club");
  s = s.replace(/\bg\.?\s*c\.?\b/gi, "golf club");
  s = s.replace(/\bg\.?\s*l\.?\b/gi, "golf links");
  s = s.replace(/\bgolf club(\s+golf club)+\b/gi, "golf club");
  s = s.replace(/\bcountry club(\s+country club)+\b/gi, "country club");
  s = s.replace(/\bgolf links(\s+golf links)+\b/gi, "golf links");
  s = s.replace(/[^a-z0-9]+/g, " ");
  s = s.replace(/\s+/g, " ").trim();
  const aliases = {
    albany: "albany golf club",
    "albany bahamas": "albany golf club",
    "sea island resort": "sea island golf club",
  };
  return aliases[s] || s;
}

const NUMERIC_HEADERS = new Set([
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
]);

/** Same six axes as Course Fit radar (course stress vs player profile). */
const COURSE_FIT_TABLE_RADAR_KEYS = [
  "adj_driving_accuracy",
  "ott_sg",
  "app_sg",
  "arg_sg",
  "putt_sg",
  "adj_driving_distance",
];

function main() {
  const csvPath = path.join(REPO_ROOT, "data", "course_table.csv");
  if (!fs.existsSync(csvPath)) {
    console.error("[build-course-table-json] missing", csvPath);
    process.exit(1);
  }
  const raw = fs.readFileSync(csvPath, "utf8");
  const rows = parse(raw, { columns: true, skip_empty_lines: true, relax_column_count: true });
  /** @type {Record<string, { lo: number, hi: number }>} */
  const ranges = {};
  for (const k of COURSE_FIT_TABLE_RADAR_KEYS) {
    ranges[k] = { lo: Infinity, hi: -Infinity };
  }
  for (const k of NUMERIC_HEADERS) {
    if (!ranges[k]) ranges[k] = { lo: Infinity, hi: -Infinity };
  }

  const outRows = [];
  const byNormKey = {};

  for (const rec of rows) {
    const courseRaw = String(rec.course ?? "").trim();
    if (!courseRaw) continue;
    const course = formatCourseLabelForDisplay(courseRaw) || courseRaw;
    const o = { course };
    for (const [k, v] of Object.entries(rec)) {
      if (k === "course") continue;
      if (NUMERIC_HEADERS.has(k)) {
        const n = typeof v === "number" ? v : parseFloat(String(v).replace(/,/g, ""));
        o[k] = Number.isFinite(n) ? n : null;
      } else {
        o[k] = v;
      }
    }
    const nk = normCourseNameKeyFetch(course);
    o._normKey = nk;
    outRows.push(o);
    if (nk && !byNormKey[nk]) byNormKey[nk] = o;

    for (const k of Object.keys(ranges)) {
      const n = o[k];
      if (!Number.isFinite(n)) continue;
      const r = ranges[k];
      r.lo = Math.min(r.lo, n);
      r.hi = Math.max(r.hi, n);
    }
  }

  for (const r of Object.values(ranges)) {
    if (!Number.isFinite(r.lo) || !Number.isFinite(r.hi) || r.hi - r.lo < 1e-9) {
      r.lo = 0;
      r.hi = 1;
    }
  }

  /** @type {Record<string, number>} */
  const means = {};
  const nCourses = outRows.length || 1;
  for (const k of [...COURSE_FIT_TABLE_RADAR_KEYS, ...NUMERIC_HEADERS]) {
    let s = 0;
    let c = 0;
    for (const row of outRows) {
      const v = row[k];
      if (Number.isFinite(v)) {
        s += v;
        c++;
      }
    }
    means[k] = c ? s / c : NaN;
  }

  const payload = {
    version: 1,
    generatedAt: new Date().toISOString(),
    sourceCsv: path.relative(REPO_ROOT, csvPath).replace(/\\/g, "/"),
    radarKeys: [...COURSE_FIT_TABLE_RADAR_KEYS],
    ranges,
    means,
    byNormKey,
    rows: outRows,
  };

  const outPath = path.join(WEB_ROOT, "course-table.json");
  fs.writeFileSync(outPath, JSON.stringify(payload), "utf8");
  console.log("[build-course-table-json] wrote", outPath, `(${outRows.length} courses)`);
}

main();
