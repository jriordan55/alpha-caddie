#!/usr/bin/env node
/**
 * Pre-push checks: pin banner copy, market rating computable for GIR, benchmarks present.
 *   npm run verify:web-deploy
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { buildFieldMarketStats, fieldMarketRating100ForPlayer } from "./market-rating-player.mjs";
import { roundAdjustmentsFromPinSheet } from "./pin-sheet-difficulty.mjs";
import { ensureProjectionCourseBasisComplete } from "./course-round-adjustments.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");

function fail(msg) {
  console.error(`[verify:web-deploy] FAIL: ${msg}`);
  process.exit(1);
}

function loadJson(rel) {
  const p = join(WEB, rel);
  if (!existsSync(p)) fail(`missing ${rel}`);
  try {
    return JSON.parse(readFileSync(p, "utf8"));
  } catch (e) {
    fail(`invalid JSON ${rel}: ${e.message || e}`);
  }
}

const indexHtml = readFileSync(join(WEB, "index.html"), "utf8");
if (!/app\.js\?v=\d+/.test(indexHtml)) {
  fail("index.html must reference app.js?v=N for cache busting");
}

const activePath = join(WEB, "data", "pin_sheets", "pin_sheet_active.json");
if (existsSync(activePath)) {
  const active = JSON.parse(readFileSync(activePath, "utf8"));
  if (Array.isArray(active.holes) && active.holes.length >= 9) {
    const adj = roundAdjustmentsFromPinSheet(active.holes);
    const summary = String(adj.summary || "");
    if (/\.{3}|…/.test(summary)) fail(`pin_sheet summary must not truncate with ellipsis: ${summary}`);
    if (!summary.trim()) fail("pin_sheet summary empty");
  }
}

const proj = loadJson("projections.json");
const bench = proj.pga_tour_market_benchmarks || proj.meta?.pga_tour_market_benchmarks;
if (!bench?.GIR?.mean) fail("projections.json missing pga_tour_market_benchmarks.GIR");

const courseBench = proj.pga_tour_course_benchmarks || proj.meta?.pga_tour_course_benchmarks;
if (!courseBench?.["Total score"]?.mean) {
  fail("projections.json missing pga_tour_course_benchmarks['Total score']");
}

if (!proj.projection_course_basis || typeof proj.projection_course_basis !== "object") {
  fail("projections.json missing projection_course_basis");
}
ensureProjectionCourseBasisComplete(proj.projection_course_basis, proj);
const venueRoundScore = Number(proj.projection_course_basis.venue_avg_round_score);
if (!Number.isFinite(venueRoundScore)) {
  fail("projection_course_basis.venue_avg_round_score missing — run node scripts/repair-projection-course-basis.mjs");
}
const par18 = Math.round(Number(proj.course_par_18)) || 72;
if (venueRoundScore < par18 - 14 || venueRoundScore > par18 + 22) {
  fail(`venue_avg_round_score ${venueRoundScore} out of range for par ${par18}`);
}

const players = Array.isArray(proj.players) ? proj.players : [];
const dr = Math.round(Number(proj.display_round ?? proj.meta?.display_round ?? 1));
const r1Players = players.filter(
  (p) => Math.round(Number(p.round)) === dr && Number.isFinite(Number(p.dg_gir_pct)) && Number(p.dg_gir_pct) > 0,
);
if (r1Players.length < 20) {
  fail(`expected ≥20 display_round R${dr} players with dg_gir_pct, got ${r1Players.length}`);
}

const fieldStats = buildFieldMarketStats(r1Players, ["Total score", "GIR"]);
const scoreFs = fieldStats.get("Total score");
if (!scoreFs || scoreFs.n < 20) fail("could not build field Total score stats for market rating");

let bestScore = Infinity;
let worstScore = -Infinity;
let bestPlayer = null;
let worstPlayer = null;
for (const p of r1Players) {
  const ts = Number(p.total_score);
  if (!Number.isFinite(ts)) continue;
  if (ts < bestScore) {
    bestScore = ts;
    bestPlayer = p;
  }
  if (ts > worstScore) {
    worstScore = ts;
    worstPlayer = p;
  }
}
if (bestPlayer && worstPlayer) {
  const bestR = fieldMarketRating100ForPlayer("Total score", bestPlayer, fieldStats);
  const worstR = fieldMarketRating100ForPlayer("Total score", worstPlayer, fieldStats);
  if (!Number.isFinite(bestR) || bestR < 55) {
    fail(`lowest projected score (${bestPlayer.player_name}, ${bestScore}) should rate ≥55, got ${bestR}`);
  }
  if (!Number.isFinite(worstR) || worstR > 45) {
    fail(`highest projected score (${worstPlayer.player_name}, ${worstScore}) should rate ≤45, got ${worstR}`);
  }
  if (bestR <= worstR) {
    fail(`field market rating order wrong: best ${bestR} vs worst ${worstR}`);
  }
}

let bad = 0;
for (const p of r1Players.slice(0, 12)) {
  const score = fieldMarketRating100ForPlayer("GIR", p, fieldStats);
  if (!Number.isFinite(score) || score < 1 || score > 100) {
    console.error(`  invalid GIR field market rating for ${p.player_name}: ${score}`);
    bad++;
  }
}
if (bad > 0) fail(`${bad} player(s) with invalid GIR field market rating`);

const pinMeta = proj.pin_sheet || proj.meta?.pin_sheet;
if (pinMeta?.summary && /\.{3}|…/.test(pinMeta.summary)) {
  fail(`projections pin_sheet.summary must not truncate with ellipsis: ${pinMeta.summary}`);
}

// When DataGolf live-hole-stats has AM/PM wave splits, push:live must have applied them.
const livePath = join(WEB, "live-in-play.json");
if (existsSync(livePath)) {
  try {
    const live = JSON.parse(readFileSync(livePath, "utf8"));
    const courses = live?.live_hole_stats?.courses;
    let hasWave = false;
    if (Array.isArray(courses)) {
      for (const c of courses) {
        for (const rnd of c?.rounds || []) {
          const holes = rnd?.holes || [];
          const amOk = holes.some((h) => Number(h?.morning_wave?.players_thru) >= 8);
          const pmOk = holes.some((h) => Number(h?.afternoon_wave?.players_thru) >= 8);
          if (amOk && pmOk && holes.length >= 9) {
            hasWave = true;
            break;
          }
        }
        if (hasWave) break;
      }
    }
    if (hasWave) {
      const wave = proj.projection_course_basis?.live_hole_stats_wave || proj.meta?.projection_course_basis?.live_hole_stats_wave;
      const uf = proj.projection_unified_factors || proj.meta?.projection_unified_factors || {};
      const sourceOk =
        (wave && Number.isFinite(Number(wave.delta_stp ?? wave.deltaAfternoonMinusMorning))) ||
        uf?.tee_wave_bias?.source === "live_hole_stats" ||
        uf?.tee_wave_source === "live_hole_stats";
      if (!sourceOk) {
        fail(
          "live-in-play has DataGolf AM/PM hole-stats wave split but projections missing live_hole_stats_wave — re-run apply:unified-factors after fetch:in-play",
        );
      }
      const bogMean = (() => {
        const vals = r1Players.map((p) => Number(p.bogeys)).filter(Number.isFinite);
        return vals.length ? vals.reduce((a, b) => a + b, 0) / vals.length : NaN;
      })();
      const dgBog = Number(wave?.total?.bogeys);
      if (Number.isFinite(bogMean) && Number.isFinite(dgBog) && Math.abs(bogMean - dgBog) > 1.25) {
        fail(
          `field avg bogeys ${bogMean.toFixed(2)} far from DG live-hole-stats ${dgBog.toFixed(2)} — wave recenter missing after weather bake`,
        );
      }
    }
  } catch (e) {
    console.warn(`[verify:web-deploy] WARN: could not check live_hole_stats wave — ${e.message || e}`);
  }
}

console.log(
  `[verify:web-deploy] OK — pin copy clean, field GIR market rating for ${Math.min(12, r1Players.length)} sample players, Total score spread best/worst validated, course round score ${venueRoundScore}, app.js cache bust present`,
);
