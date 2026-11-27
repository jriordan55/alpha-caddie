#!/usr/bin/env node
/**
 * Slim Betsperts payload for betting visuals on the website.
 *
 * Reads course facts + hole stats + RH matrix windows + player greenGrass.
 * Writes: alpha-caddie-web/data/betsperts_betting_viz.json (+ root mirror)
 *
 *   node scripts/build-betsperts-betting-viz.mjs
 *   npm run build:betsperts-betting-viz
 */
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync, renameSync, unlinkSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";
import { loadBetspertsCourseFacts } from "./betsperts-course-context.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");
const OUT_WEB = join(WEB, "data", "betsperts_betting_viz.json");
const OUT_ROOT = join(ROOT, "data", "betsperts_betting_viz.json");
const MATRIX_DIR = join(ROOT, "data", "betsperts", "rabbit-hole", "matrix", "by-slice");
const PLAYERS_DIR = join(ROOT, "data", "betsperts", "players", "by-id");
const MAP_PATH = join(ROOT, "data", "pga_datagolf_player_map.csv");

function writeJsonAtomic(path, obj) {
  mkdirSync(dirname(path), { recursive: true });
  const text = JSON.stringify(obj) + "\n";
  const tmp = `${path}.${process.pid}.tmp`;
  writeFileSync(tmp, text, "utf8");
  try {
    renameSync(tmp, path);
  } catch {
    writeFileSync(path, text, "utf8");
    try {
      unlinkSync(tmp);
    } catch {
      /* ignore */
    }
  }
}

function num(v, fb = NaN) {
  const n = Number(String(v ?? "").replace(/,/g, "").trim());
  return Number.isFinite(n) ? n : fb;
}

function leanChipsFromCourse(rec) {
  const ms = rec.metric_summary || {};
  const chips = [];
  const push = (market, lean, detail) => {
    if (!lean) return;
    chips.push({ market, lean, detail });
  };
  if (Number.isFinite(ms.driving_accuracy_pct) && Number.isFinite(ms.tour_driving_accuracy_pct)) {
    const d = ms.driving_accuracy_pct - ms.tour_driving_accuracy_pct;
    push(
      "Fairways",
      Math.abs(d) >= 2 ? (d > 0 ? "Over" : "Under") : "Neutral",
      `Acc ${ms.driving_accuracy_pct.toFixed(1)}% vs Tour ${ms.tour_driving_accuracy_pct.toFixed(1)}% (${d >= 0 ? "+" : ""}${d.toFixed(1)})`,
    );
  }
  if (Number.isFinite(ms.gir_pct) && Number.isFinite(ms.tour_gir_pct)) {
    const d = ms.gir_pct - ms.tour_gir_pct;
    push(
      "GIR",
      Math.abs(d) >= 2 ? (d > 0 ? "Over" : "Under") : "Neutral",
      `GIR ${ms.gir_pct.toFixed(1)}% vs Tour ${ms.tour_gir_pct.toFixed(1)}% (${d >= 0 ? "+" : ""}${d.toFixed(1)})`,
    );
  }
  if (Number.isFinite(ms.bob_pct) && Number.isFinite(ms.tour_bob_pct)) {
    const d = ms.bob_pct - ms.tour_bob_pct;
    push(
      "Birdies",
      Math.abs(d) >= 0.8 ? (d > 0 ? "Over" : "Under") : "Neutral",
      `BoB ${ms.bob_pct.toFixed(1)}% vs Tour ${ms.tour_bob_pct.toFixed(1)}%`,
    );
  }
  if (Number.isFinite(ms.scoring_avg) || Number.isFinite(rec.score_rtp)) {
    const rtp = Number.isFinite(rec.score_rtp) ? rec.score_rtp : ms.scoring_avg - (rec.par || 72);
    push(
      "Total score",
      Math.abs(rtp) >= 0.35 ? (rtp < 0 ? "Under" : "Over") : "Neutral",
      `Course RTP ${rtp >= 0 ? "+" : ""}${Number(rtp).toFixed(2)} (neg = easier)`,
    );
  }
  const club = rec.skill_labels?.ott_club;
  if (club && /less than driver/i.test(club)) {
    push("Driving distance", "Under", `OTT club: ${club} — fade pure bomber overs`);
  } else if (club && /driver heavy/i.test(club)) {
    push("Driving distance", "Over", `OTT club: ${club}`);
  }
  return chips;
}

function loadPgaToDg() {
  const map = new Map();
  if (!existsSync(MAP_PATH)) return map;
  const rows = parse(readFileSync(MAP_PATH, "utf8"), {
    columns: true,
    skip_empty_lines: true,
    relax_column_count: true,
    bom: true,
  });
  for (const r of rows) {
    const pga = String(r.pga_player_id || "").replace(/\D/g, "");
    const dg = Math.round(num(r.dg_id, NaN));
    if (pga && Number.isFinite(dg)) map.set(pga, dg);
  }
  return map;
}

function loadPlayerSurfaces(pgaToDg) {
  const byDg = {};
  if (!existsSync(PLAYERS_DIR)) return byDg;
  for (const f of readdirSync(PLAYERS_DIR).filter((x) => x.endsWith(".json"))) {
    const pga = f.replace(/\.json$/i, "").replace(/\D/g, "");
    const dg = pgaToDg.get(pga);
    if (!Number.isFinite(dg)) continue;
    let j;
    try {
      j = JSON.parse(readFileSync(join(PLAYERS_DIR, f), "utf8"));
    } catch {
      continue;
    }
    const gg = Array.isArray(j.stats?.greenGrass) ? j.stats.greenGrass : [];
    const greenGrass = gg
      .map((r) => ({
        surface: String(r.GreenSurface || "").trim(),
        sg: num(r.StrokesGainedAverage, NaN),
        n: Math.round(num(r.Rounds, 0)),
      }))
      .filter((r) => r.surface && Number.isFinite(r.sg));
    const byLen = Array.isArray(j.stats?.byCourseLength) ? j.stats.byCourseLength : [];
    const courseLength = byLen
      .map((r) => ({
        label: String(r.CourseLength || r.Length || r.Condition || "").trim(),
        sg: num(r.StrokesGainedAverage ?? r["SG:TOT"], NaN),
        n: Math.round(num(r.Rounds, 0)),
      }))
      .filter((r) => r.label && Number.isFinite(r.sg));
    const byCond = Array.isArray(j.stats?.byScoringCondition) ? j.stats.byScoringCondition : [];
    const scoringCondition = byCond
      .map((r) => ({
        label: String(r.Condition || r.ScoringCondition || "").trim(),
        sg: num(r.StrokesGainedAverage ?? r["SG:TOT"], NaN),
        n: Math.round(num(r.Rounds, 0)),
      }))
      .filter((r) => r.label && Number.isFinite(r.sg));
    if (!greenGrass.length && !courseLength.length && !scoringCondition.length) continue;
    byDg[String(dg)] = {
      pga_id: pga,
      name: j.playerName || null,
      greenGrass,
      courseLength,
      scoringCondition,
    };
  }
  return byDg;
}

/** Attach SG:TOT windows from RH matrix slices onto playersByDg. */
function attachRhWindows(playersByDg, pgaToDg) {
  if (!existsSync(MATRIX_DIR)) return { attached: 0, files: 0 };
  const want = new Set([
    "strokes-gained__average__all",
    "strokes-gained__total__all",
    "strokes-gained__average__round-12",
    "strokes-gained__average__round-24",
    "strokes-gained__average__round-50",
    "strokes-gained__average__round-100",
    "strokes-gained__total__round-50",
    "strokes-gained__total__round-100",
  ]);
  for (let y = 2018; y <= new Date().getUTCFullYear(); y++) {
    want.add(`strokes-gained__average__season-${y}`);
    want.add(`strokes-gained__total__season-${y}`);
  }
  let files = 0;
  let attached = 0;
  for (const key of want) {
    const fp = join(MATRIX_DIR, `${key}.json`);
    if (!existsSync(fp)) continue;
    files++;
    let slice;
    try {
      slice = JSON.parse(readFileSync(fp, "utf8"));
    } catch {
      continue;
    }
    const display = String(slice.display || "").toLowerCase() === "total" ? "total" : "average";
    const timeKey = String(slice.timeKey || key.split("__").pop());
    for (const row of slice.rows || []) {
      const pga = String(row.player_num || "").replace(/\D/g, "");
      const dg = pgaToDg.get(pga);
      if (!Number.isFinite(dg)) continue;
      const id = String(dg);
      if (!playersByDg[id]) playersByDg[id] = { pga_id: pga, name: row.playerName || null };
      if (!playersByDg[id].windows) playersByDg[id].windows = { average: {}, total: {} };
      playersByDg[id].windows[display][timeKey] = {
        sg_tot: num(row["SG:TOT"], NaN),
        rounds: Math.round(num(row.Rounds, 0)),
      };
      attached++;
    }
  }
  return { attached, files };
}

const facts = loadBetspertsCourseFacts(true);
const courses = {};
for (const [key, rec] of Object.entries(facts.byKey || {})) {
  if (!rec || courses[key]) continue;
  courses[key] = {
    key,
    name: rec.name,
    course_num: rec.course_num,
    par: rec.par,
    yards: rec.yards,
    grass: rec.grass,
    skill_labels: rec.skill_labels,
    rates: rec.rates,
    score_rtp: rec.score_rtp,
    difficulty: rec.difficulty,
    metric_summary: rec.metric_summary,
    correlated: (rec.correlated || []).map((c) => c.name || c.key).filter(Boolean),
    hole_stats: rec.hole_stats || [],
    leans: leanChipsFromCourse(rec),
  };
}

const pgaToDg = loadPgaToDg();
const playersByDg = loadPlayerSurfaces(pgaToDg);
const winMeta = attachRhWindows(playersByDg, pgaToDg);

const payload = {
  generatedAt: new Date().toISOString(),
  source: "betsperts course facts + hole stats + RH matrix windows + player greenGrass",
  nCourses: Object.keys(courses).length,
  nPlayers: Object.keys(playersByDg).length,
  windowFiles: winMeta.files,
  courses,
  playersByDg,
};

writeJsonAtomic(OUT_WEB, payload);
writeJsonAtomic(OUT_ROOT, payload);
console.log(
  `[betting-viz] courses=${payload.nCourses} players=${payload.nPlayers} windowFiles=${winMeta.files} → data/betsperts_betting_viz.json`,
);
