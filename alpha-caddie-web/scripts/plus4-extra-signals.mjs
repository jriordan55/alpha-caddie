/**
 * Plus4 extra measures beyond COR:
 *  - SG:APP / SG:Putt distance bins (caller also applies dist_sg to STP)
 *  - Putt / ARG on corollary surfaces (grass + course-table similarity)
 *  - Regional residual pool (lat/lon clusters)
 *  - Player volatility → round_sd
 *  - Ball-speed proxy via driving_distance_rating
 *
 * Returns stroke shifts (positive = higher score / worse) for applyPlus4CorToPlayers.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { COURSE_COORDINATES_BY_NAME } from "./open-meteo-forecast.mjs";
import { loadCourseTablePayload, resolveCourseTableRow } from "./projection-unified-factors.mjs";
import { num } from "./plus4-cor-score.mjs";
import {
  loadBetspertsCourseFacts,
  loadPlayerGreenGrassByDg,
  playerGreenSurfaceSgRelative,
  resolveBetspertsCourseFacts,
} from "./betsperts-course-context.mjs";

const WEB_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const CONTEXT_PATH = join(WEB_ROOT, "data", "course_event_context.json");

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function envNum(name, fallback) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return fallback;
  const n = Number(raw);
  return Number.isFinite(n) ? n : fallback;
}

let _contextCache = null;
export function loadCourseEventContext() {
  if (_contextCache) return _contextCache;
  if (!existsSync(CONTEXT_PATH)) {
    _contextCache = { by_course: {} };
    return _contextCache;
  }
  try {
    _contextCache = JSON.parse(readFileSync(CONTEXT_PATH, "utf8"));
  } catch {
    _contextCache = { by_course: {} };
  }
  return _contextCache;
}

function grassTokens(ctxRow, which = "greens") {
  const arr = ctxRow?.grass?.[which];
  if (!Array.isArray(arr)) return [];
  return arr
    .map((s) =>
      String(s || "")
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, " ")
        .trim(),
    )
    .filter(Boolean);
}

function grassOverlap(a, b) {
  if (!a.length || !b.length) return 0;
  const sb = new Set(b);
  let hit = 0;
  for (const t of a) if (sb.has(t)) hit++;
  return hit / Math.max(a.length, b.length);
}

function coordsForCourse(ck) {
  if (!ck) return null;
  if (COURSE_COORDINATES_BY_NAME[ck]) return COURSE_COORDINATES_BY_NAME[ck];
  for (const [k, c] of Object.entries(COURSE_COORDINATES_BY_NAME)) {
    if (k.includes(ck) || ck.includes(k)) return c;
  }
  return null;
}

function haversineKm(a, b) {
  const R = 6371;
  const toR = (d) => (d * Math.PI) / 180;
  const dLat = toR(b.lat - a.lat);
  const dLon = toR(b.lon - a.lon);
  const la1 = toR(a.lat);
  const la2 = toR(b.lat);
  const h =
    Math.sin(dLat / 2) ** 2 + Math.cos(la1) * Math.cos(la2) * Math.sin(dLon / 2) ** 2;
  return 2 * R * Math.asin(Math.min(1, Math.sqrt(h)));
}

/**
 * Courses with similar putting / short-game surfaces to venue.
 */
export function surfaceCorollaryCourses(venueKey, opts = {}) {
  const ck = normCourseNameKey(venueKey);
  const ctx = loadCourseEventContext();
  const bpFacts = loadBetspertsCourseFacts();
  const venueBp = resolveBetspertsCourseFacts(ck, bpFacts);
  const venueCtx = ctx.by_course?.[ck];
  // Prefer explicit course_event_context grass; fall back to Betsperts GreenGrass / surrounds.
  const greens = grassTokens(venueCtx, "greens").length
    ? grassTokens(venueCtx, "greens")
    : grassTokens({ grass: venueBp?.grass }, "greens");
  const surrounds = grassTokens(venueCtx, "surrounds").length
    ? grassTokens(venueCtx, "surrounds")
    : grassTokens({ grass: venueBp?.grass }, "surrounds");
  const ct = loadCourseTablePayload();
  const venueRow = resolveCourseTableRow(ct, ck);
  const out = { putt: [], arg: [] };

  const allKeys = new Set([
    ...Object.keys(ctx.by_course || {}),
    ...Object.keys(bpFacts?.byKey || {}),
    ...((ct?.rows || []).map((r) => normCourseNameKey(r.course || r.course_name || "")).filter(Boolean)),
  ]);

  // Seed with Betsperts correlated courses at a floor score when grass matches.
  for (const c of venueBp?.correlated || []) {
    const other = c.key || normCourseNameKey(c.name || "");
    if (!other || other === ck) continue;
    out.putt.push({ key: other, score: 0.4, source: "betsperts_correlated" });
    out.arg.push({ key: other, score: 0.35, source: "betsperts_correlated" });
  }

  for (const other of allKeys) {
    if (!other || other === ck) continue;
    const oCtx = ctx.by_course?.[other];
    const oBp = bpFacts?.byKey?.[other];
    const oGrass = oCtx?.grass?.greens?.length ? oCtx.grass : oBp?.grass;
    const gOverlap = grassOverlap(greens, grassTokens({ grass: oGrass }, "greens"));
    const sOverlap = grassOverlap(
      surrounds,
      grassTokens({ grass: oGrass || oCtx?.grass }, "surrounds"),
    );
    const oRow = resolveCourseTableRow(ct, other);
    let puttFeat = 0;
    let argFeat = 0;
    if (venueRow && oRow) {
      const vp = num(venueRow.putt_sg, NaN);
      const op = num(oRow.putt_sg, NaN);
      if (Number.isFinite(vp) && Number.isFinite(op)) {
        puttFeat = 1 / (1 + Math.abs(vp - op) * 40);
      }
      const va = num(venueRow.arg_sg, NaN);
      const oa = num(oRow.arg_sg, NaN);
      if (Number.isFinite(va) && Number.isFinite(oa)) {
        argFeat = 1 / (1 + Math.abs(va - oa) * 40);
      }
    }
    const puttScore = 0.65 * gOverlap + 0.35 * puttFeat;
    const argScore = 0.55 * sOverlap + 0.45 * argFeat;
    if (puttScore >= 0.18) {
      const prev = out.putt.find((x) => x.key === other);
      if (prev) prev.score = Math.max(prev.score, puttScore);
      else out.putt.push({ key: other, score: puttScore });
    }
    if (argScore >= 0.18) {
      const prev = out.arg.find((x) => x.key === other);
      if (prev) prev.score = Math.max(prev.score, argScore);
      else out.arg.push({ key: other, score: argScore });
    }
  }
  out.putt.sort((a, b) => b.score - a.score);
  out.arg.sort((a, b) => b.score - a.score);
  const lim = Math.round(opts.limit ?? 12);
  return { putt: out.putt.slice(0, lim), arg: out.arg.slice(0, lim) };
}

/**
 * Regional courses within maxKm of venue.
 */
export function regionalCorollaryCourses(venueKey, opts = {}) {
  const ck = normCourseNameKey(venueKey);
  const origin = coordsForCourse(ck);
  if (!origin) return [];
  const maxKm = envNum("GOLF_COR_REGION_KM", opts.maxKm ?? 900);
  const out = [];
  for (const [k, c] of Object.entries(COURSE_COORDINATES_BY_NAME)) {
    if (k === ck) continue;
    const d = haversineKm(origin, c);
    if (d <= maxKm) out.push({ key: k, km: Math.round(d), score: 1 / (1 + d / 400) });
  }
  out.sort((a, b) => b.score - a.score);
  return out.slice(0, Math.round(opts.limit ?? 15));
}

function meanResidualOnKeys(dg, keys, playerCourse, field = "residual") {
  let sum = 0;
  let w = 0;
  let n = 0;
  for (const ck of keys) {
    const agg = playerCourse.get(`${dg}|${ck}`);
    if (!agg || agg.n < 1) continue;
    let val = NaN;
    if (field === "residual") val = agg.sum / agg.n;
    else if (field === "putt" && agg.puttN) val = agg.puttSum / agg.puttN;
    else if (field === "arg" && agg.argN) val = agg.argSum / agg.argN;
    else if (field === "app" && agg.appN) val = agg.appSum / agg.appN;
    if (!Number.isFinite(val)) continue;
    const ww = Math.sqrt(agg.n);
    sum += ww * val;
    w += ww;
    n += agg.n;
  }
  if (w <= 0) return { mean: NaN, n: 0 };
  return { mean: sum / w, n };
}

function playerVolatility(dg, residualBundle, corKeys) {
  const rounds = residualBundle?.playerRounds?.get(dg) || [];
  const vals = [];
  for (const r of rounds) {
    if (!Number.isFinite(r._residual)) continue;
    if (corKeys?.length && r._ck && !corKeys.includes(r._ck)) continue;
    vals.push(r._residual);
  }
  if (vals.length < 6) {
    // fall back to all residuals
    for (const r of rounds) {
      if (Number.isFinite(r._residual)) vals.push(r._residual);
    }
  }
  if (vals.length < 5) return NaN;
  const mean = vals.reduce((a, b) => a + b, 0) / vals.length;
  let ss = 0;
  for (const v of vals) ss += (v - mean) ** 2;
  return Math.sqrt(ss / Math.max(1, vals.length - 1));
}

/**
 * Build extras applicator bound to a venue.
 */
export function makePlus4ExtrasApplicator(venueKey, opts = {}) {
  const surfaces = surfaceCorollaryCourses(venueKey, opts);
  const regional = regionalCorollaryCourses(venueKey, opts);
  const puttKeys = surfaces.putt.map((x) => x.key);
  const argKeys = surfaces.arg.map((x) => x.key);
  const regionKeys = regional.map((x) => x.key);
  const wPutt = envNum("GOLF_COR_PUTT_SURFACE_W", 0.12);
  const wArg = envNum("GOLF_COR_ARG_SURFACE_W", 0.1);
  const wRegion = envNum("GOLF_COR_REGION_W", 0.08);
  const wDistProxy = envNum("GOLF_COR_DIST_PROXY_W", 0.04);
  const wGreenGrass = envNum("GOLF_COR_GREEN_GRASS_W", 0.14);
  const baseSd = envNum("GOLF_COR_BASE_ROUND_SD", 2.75);
  const venueBp = resolveBetspertsCourseFacts(venueKey);
  const venueGreen = venueBp?.grass?.greens?.[0] || null;
  const greenGrassByDg = venueGreen ? loadPlayerGreenGrassByDg() : new Map();

  return function applyExtras(ctx) {
    const { dg, playerCourse, residualBundle, row, corCourses } = ctx;
    const corKeys = (corCourses || []).map((c) => c.key);

    const putt = meanResidualOnKeys(dg, puttKeys, playerCourse, "putt");
    const arg = meanResidualOnKeys(dg, argKeys, playerCourse, "arg");
    const region = meanResidualOnKeys(dg, regionKeys, playerCourse, "residual");

    // Positive SG → negative stroke (better). Convert SG edge to stroke shift.
    const puttStroke = Number.isFinite(putt.mean) ? -wPutt * clamp(putt.mean, -1.2, 1.2) : 0;
    const argStroke = Number.isFinite(arg.mean) ? -wArg * clamp(arg.mean, -1.2, 1.2) : 0;
    const regionStroke = Number.isFinite(region.mean)
      ? -wRegion * clamp(region.mean, -1.5, 1.5)
      : 0;

    // Betsperts player greenGrass vs venue GreenGrass (relative to player's other surfaces).
    let greenGrassStroke = 0;
    let greenGrassDetail = null;
    if (venueGreen && greenGrassByDg.has(dg)) {
      const rel = playerGreenSurfaceSgRelative(greenGrassByDg.get(dg), venueGreen);
      if (Number.isFinite(rel.relative) && rel.n >= 8) {
        greenGrassStroke = -wGreenGrass * clamp(rel.relative, -1.2, 1.2);
        greenGrassDetail = {
          surface: venueGreen,
          sg: Math.round(rel.sg * 1000) / 1000,
          relative: Math.round(rel.relative * 1000) / 1000,
          n: rel.n,
        };
      }
    }

    // Ball-speed / length proxy: longer hitters slightly helped on long venues via distance rating.
    const distRating = num(row?.driving_distance_rating, NaN);
    let distStroke = 0;
    if (Number.isFinite(distRating)) {
      distStroke = -wDistProxy * clamp(distRating / 10, -1, 1);
    }

    const vol = playerVolatility(dg, residualBundle, corKeys);
    const round_sd = Number.isFinite(vol)
      ? clamp(baseSd + 0.55 * (vol - 0.9), 2.2, 3.6)
      : baseSd;

    const totalStroke = puttStroke + argStroke + regionStroke + distStroke + greenGrassStroke;
    return {
      putt_surface_stroke: Math.round(puttStroke * 1000) / 1000,
      putt_n: putt.n,
      arg_surface_stroke: Math.round(argStroke * 1000) / 1000,
      arg_n: arg.n,
      region_stroke: Math.round(regionStroke * 1000) / 1000,
      region_n: region.n,
      green_grass_stroke: Math.round(greenGrassStroke * 1000) / 1000,
      green_grass: greenGrassDetail || undefined,
      dist_proxy_stroke: Math.round(distStroke * 1000) / 1000,
      totalStroke: Math.round(totalStroke * 1000) / 1000,
      round_sd: Math.round(round_sd * 100) / 100,
      volatility: Number.isFinite(vol) ? Math.round(vol * 1000) / 1000 : null,
      surface_putt_courses: puttKeys.slice(0, 6),
      surface_arg_courses: argKeys.slice(0, 6),
      region_courses: regionKeys.slice(0, 6),
    };
  };
}
