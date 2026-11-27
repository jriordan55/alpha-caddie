/**
 * Bake the week’s static app payload so the browser paints from cache, not live work.
 *
 * Writes:
 *   data/app_week_precompute.json  — prefetch manifest + inlined tab payloads
 *   data/course_key_stats.json     — via build-course-key-stats.mjs
 *
 *   node scripts/build-app-precompute.mjs
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const ROOT = join(WEB, "..");

function num(v, d = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : d;
}

function readJson(path) {
  if (!existsSync(path)) return null;
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return null;
  }
}

function writeJson(path, obj) {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, `${JSON.stringify(obj)}\n`, "utf8");
}

function displayName(raw) {
  const s = String(raw || "").trim();
  if (!s) return "";
  if (s.includes(",")) {
    const [last, first] = s.split(",").map((x) => x.trim());
    if (first && last) return `${first} ${last}`;
  }
  return s;
}

/** Field SG pillar spread → Course Breakdown impact segments. */
function fieldSgImpact(players) {
  const seen = new Set();
  const rows = [];
  for (const p of players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || seen.has(id)) continue;
    const round = Math.round(num(p.round, NaN));
    const hasR1 = (players || []).some(
      (x) => Math.round(num(x.dg_id, NaN)) === id && Math.round(num(x.round, NaN)) === 1,
    );
    if (hasR1 && round !== 1) continue;
    seen.add(id);
    rows.push(p);
  }
  const pillars = [
    { key: "sg_ott", label: "Off-the-Tee" },
    { key: "sg_app", label: "Approach" },
    { key: "sg_arg", label: "Around-the-Green" },
    { key: "sg_putt", label: "Putting" },
  ];
  const spreads = pillars.map((p) => {
    const vals = rows.map((r) => num(r[p.key], NaN)).filter((v) => Number.isFinite(v));
    if (vals.length < 4) return { ...p, spread: 0, pct: 0 };
    const mean = vals.reduce((a, b) => a + b, 0) / vals.length;
    const spread = Math.sqrt(vals.reduce((a, b) => a + (b - mean) ** 2, 0) / vals.length);
    return { ...p, spread, pct: 0 };
  });
  const tot = spreads.reduce((a, s) => a + s.spread, 0) || 1;
  for (const s of spreads) s.pct = Math.round((s.spread / tot) * 1000) / 10;
  spreads.sort((a, b) => b.pct - a.pct);
  return spreads;
}

function approachBinLeaders(players, approachJson) {
  const byDg = new Map();
  for (const p of approachJson?.players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(id)) byDg.set(id, p);
  }
  const fieldIds = new Set();
  for (const p of players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(id)) fieldIds.add(id);
  }
  const bins = [
    { sgKey: "100_150_fw_sg_per_shot", countKey: "100_150_fw_shot_count", label: "100–150 fw" },
    { sgKey: "150_200_fw_sg_per_shot", countKey: "150_200_fw_shot_count", label: "150–200 fw" },
    { sgKey: "50_100_fw_sg_per_shot", countKey: "50_100_fw_shot_count", label: "50–100 fw" },
    { sgKey: "over_200_fw_sg_per_shot", countKey: "over_200_fw_shot_count", label: "200+ fw" },
  ];
  const out = [];
  for (const bin of bins) {
    const scored = [];
    for (const id of fieldIds) {
      const skill = byDg.get(id);
      if (!skill) continue;
      const sg = num(skill[bin.sgKey], NaN);
      const n = num(skill[bin.countKey], NaN);
      if (!Number.isFinite(sg) || !Number.isFinite(n) || n < 15) continue;
      const nameRow = (players || []).find((p) => Math.round(num(p.dg_id, NaN)) === id);
      scored.push({ dgId: id, name: displayName(nameRow?.player_name), sg, n });
    }
    scored.sort((a, b) => b.sg - a.sg);
    out.push({ label: bin.label, leaders: scored.slice(0, 3) });
  }
  return out;
}

function main() {
  const t0 = Date.now();

  // Course Key Stats first (own artifact).
  const cks = spawnSync(process.execPath, [join(__dirname, "build-course-key-stats.mjs")], {
    cwd: WEB,
    stdio: "inherit",
  });
  if (cks.status !== 0) {
    console.warn("[app-precompute] WARN: course-key-stats build failed — continuing with prior file if any");
  }

  const proj =
    readJson(join(WEB, "projections-pga.json")) ||
    readJson(join(WEB, "projections.json")) ||
    {};
  const cksPayload = readJson(join(WEB, "data", "course_key_stats.json")) || {};
  const approach =
    readJson(join(WEB, "approach_skill_ytd.json")) ||
    readJson(join(WEB, "approach_skill_l12.json")) ||
    { players: [] };

  const venue = String(proj.course_used || proj.meta?.course_used || cksPayload.venue || "").trim();
  const eventName = String(proj.event_name || proj.meta?.event_name || cksPayload.eventName || "").trim();
  const seasonYear = (() => {
    const ds = String(proj.meta?.datagolf_field_date_start || proj.datagolf_field_date_start || "").match(/^(\d{4})-/);
    if (ds) return parseInt(ds[1], 10);
    return new Date().getFullYear();
  })();

  const prefetch = [
    "projections.json",
    "projections-pga.json",
    "live-in-play.json",
    "live-in-play-pga.json",
    "course-table.json",
    "approach_skill_ytd.json",
    "approach_skill_l12.json",
    "data/course_key_stats.json",
    "data/app_week_precompute.json",
    "data/betsperts_betting_viz.json",
    "data/betsperts_course_facts.json",
    "data/betsperts_rh_filters.json",
    "data/betsperts_rh_ou_stats.json",
    "data/betsperts_sg_by_dg.json",
    "data/player_headshots.json",
    "data/course_event_context.json",
    "data/historical_round_weather.json",
    "data/pgatour_event_rounds.json",
    "player-history/courses-manifest.json",
    `player-history/field-${seasonYear}.json`,
    `player-history/ou-season-avgs-${seasonYear}.json`,
  ];

  // Warm the venue course-history shard(s) the UI would otherwise fetch on first open.
  const manifest = readJson(join(WEB, "player-history", "courses-manifest.json")) || { courses: [] };
  const venueKey = normCourseNameKey(venue);
  const parts = venue.includes(" · ") || venue.includes(";") ? venue.split(/\s*[·;]\s*/) : [venue];
  const courseKeys = new Set([venueKey, ...parts.map((p) => normCourseNameKey(p)).filter(Boolean)].filter(Boolean));
  for (const k of [...courseKeys]) {
    const tokens = String(k).split(/\s+/).filter(Boolean);
    for (let i = tokens.length - 1; i >= 2; i--) courseKeys.add(tokens.slice(0, i).join(" "));
  }
  for (const c of manifest.courses || []) {
    const ck = normCourseNameKey(c.course_key || "");
    if (!ck) continue;
    if ([...courseKeys].some((k) => k.includes(ck) || ck.includes(k))) {
      courseKeys.add(ck);
      if (c.file) prefetch.push(`player-history/by-course/${c.file}`);
    }
  }
  for (const k of courseKeys) {
    const file = `${String(k).replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 96)}.json`;
    const rel = `player-history/by-course/${file}`;
    if (existsSync(join(WEB, rel)) && !prefetch.includes(rel)) prefetch.push(rel);
  }

  const payload = {
    generatedAt: new Date().toISOString(),
    eventName,
    venue,
    venueKey,
    seasonYear,
    displayRound: proj.display_round ?? proj.meta?.display_round ?? null,
    weatherBaked: Boolean(
      proj.meta?.forecast_weather_updated_at ||
        proj.forecast_weather_updated_at ||
        (proj.players || []).some((p) => p.dg_auto_weather),
    ),
    prefetch: [...new Set(prefetch)],
    courseKeyStats: cksPayload.current || null,
    courseBreakdown: {
      sgImpact: fieldSgImpact(proj.players || []),
      approachBinLeaders: approachBinLeaders(proj.players || [], approach),
      nField: new Set(
        (proj.players || []).map((p) => Math.round(num(p.dg_id, NaN))).filter((id) => Number.isFinite(id)),
      ).size,
    },
  };

  const outWeb = join(WEB, "data", "app_week_precompute.json");
  const outRoot = join(ROOT, "data", "app_week_precompute.json");
  writeJson(outWeb, payload);
  try {
    writeJson(outRoot, payload);
  } catch {
    /* optional */
  }

  console.log(
    `[app-precompute] ${eventName || "—"} @ ${venue || "—"} prefetch=${payload.prefetch.length} cks=${payload.courseKeyStats?.metrics?.length || 0} in ${Date.now() - t0}ms → data/app_week_precompute.json`,
  );
}

main();
