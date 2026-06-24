#!/usr/bin/env node
/**
 * Gate publish (push:live) — projections must have correct par, counting stats, and DK O/U for core markets.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { readCoursePar18 } from "./projection-course-par.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");

const DK_CORE_MARKETS = ["Total Score", "Birdies", "Pars", "Bogeys"];
const DK_MIN_LINES_PER_MARKET = 20;
/** Match course-round-adjustments event-week vs historical venue guard. */
const EVENT_WEEK_VENUE_MAX_GAP_STROKES = 2.25;

function historicalVenueScoreForRound(basis, rnd) {
  const key = String(rnd);
  return num(
    basis?.historical_venue_avg_score_by_round?.[key] ??
      basis?.field_avg_score_by_round?.[key] ??
      basis?.venue_avg_round_score,
    NaN,
  );
}

/** In-week live scoring anchor when trustworthy; else historical venue round average. */
function totalScoreTargetForValidation(basis, displayRound, coursePar) {
  const hist = historicalVenueScoreForRound(basis, displayRound);
  const ew = num(basis?.event_week_field_avg_score_by_round?.[String(displayRound)], NaN);
  if (Number.isFinite(ew)) {
    if (!Number.isFinite(hist) || Math.abs(ew - hist) <= EVENT_WEEK_VENUE_MAX_GAP_STROKES) {
      return { target: ew, eventWeekTrusted: true };
    }
  }
  if (Number.isFinite(hist)) return { target: hist, eventWeekTrusted: false };
  const venue = num(basis?.venue_avg_round_score, NaN);
  if (Number.isFinite(venue)) return { target: venue, eventWeekTrusted: false };
  return { target: coursePar + 1.0, eventWeekTrusted: false };
}

function envTruthy(name, defaultVal = false) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function fail(msg) {
  console.error(`[validate:projections] FAIL: ${msg}`);
  process.exit(1);
}

if (!existsSync(projPath)) {
  fail("missing projections.json");
}

let proj;
try {
  proj = JSON.parse(readFileSync(projPath, "utf8"));
} catch (e) {
  fail(`could not parse projections.json — ${e.message || e}`);
}

const players = Array.isArray(proj.players) ? proj.players : [];
if (!players.length) fail("projections.players is empty");

const holePars = proj.hole_pars;
const parFromHoles =
  Array.isArray(holePars) && holePars.length === 18
    ? holePars.reduce((s, p) => s + Math.round(num(p, 4)), 0)
    : NaN;
const coursePar = Math.round(num(readCoursePar18(proj), parFromHoles));
const parSource = String(proj.hole_pars_source || "").trim().toLowerCase();
const courseKey = normCourseNameKey(String(proj.course_used || "").trim());

if (!Number.isFinite(coursePar) || coursePar < 68 || coursePar > 73) {
  fail(`invalid course_par_18=${proj.course_par_18}`);
}

if (parSource === "generic") {
  fail(
    `hole_pars_source is generic (par ${coursePar}) — add course_holes.json / live_hole_stats before publish`,
  );
}

if (Number.isFinite(parFromHoles) && parFromHoles !== coursePar) {
  fail(`hole_pars sum ${parFromHoles} != course_par_18 ${coursePar}`);
}

if (courseKey.includes("shinnecock") && coursePar !== 70) {
  fail(`Shinnecock Hills must be par 70 (got ${coursePar}, source=${parSource || "?"})`);
}

const displayRound = Math.round(num(proj.display_round ?? proj.datagolf_field_current_round, NaN)) || 1;
const roundRows = players.filter((p) => Math.round(num(p.round, NaN)) === displayRound);
if (!roundRows.length) fail(`no player rows for display round R${displayRound}`);

const parBad = roundRows.filter((p) => {
  const stp = num(p.score_to_par, NaN);
  const ts = num(p.total_score, NaN);
  if (!Number.isFinite(stp) || !Number.isFinite(ts)) return true;
  return Math.abs(ts - (coursePar + stp)) > 0.05;
});
if (parBad.length) {
  fail(
    `${parBad.length}/${roundRows.length} R${displayRound} rows have total_score ≠ par+score_to_par (e.g. ${parBad[0]?.player_name || "?"} ts=${parBad[0]?.total_score} stp=${parBad[0]?.score_to_par}) — run npm run ensure:projection-course-par`,
  );
}

const basis = proj.projection_course_basis || proj.meta?.projection_course_basis || {};
const venueBird = num(basis.venue_avg_birdies, 3.2);
const venueBog = num(basis.venue_avg_bogeys, 2.9);

const missingCounts = roundRows.filter(
  (p) => !Number.isFinite(num(p.birdies, NaN)) || !Number.isFinite(num(p.pars, NaN)),
);
if (missingCounts.length) {
  fail(
    `${missingCounts.length}/${roundRows.length} R${displayRound} rows missing birdies or pars (e.g. ${missingCounts[0]?.player_name || "?"})`,
  );
}

const avg = (key) => {
  const vals = roundRows.map((p) => num(p[key], NaN)).filter((v) => Number.isFinite(v));
  return vals.length ? vals.reduce((a, b) => a + b, 0) / vals.length : NaN;
};
const avgPars = avg("pars");
const avgBogeys = avg("bogeys");
const avgBirdies = avg("birdies");
if (Number.isFinite(avgPars) && avgPars > 12.2) {
  fail(
    `R${displayRound} field avg pars ${avgPars.toFixed(2)} too high (par-heavy profile) — run reconcile-projection-counts or check inferHoleCountsFromScoreSplit`,
  );
}
if (Number.isFinite(avgBirdies) && avgBirdies < Math.max(1.5, venueBird * 0.45)) {
  fail(
    `R${displayRound} field avg birdies ${avgBirdies.toFixed(2)} too low — event-week counting profile missing or stale`,
  );
}
if (Number.isFinite(avgBogeys) && avgBogeys > Math.max(5.15, venueBog + 2.0)) {
  fail(
    `R${displayRound} field avg bogeys ${avgBogeys.toFixed(2)} too high — event-week counting profile missing or stale`,
  );
}
if (Number.isFinite(avgBogeys) && avgBogeys < Math.max(2.0, venueBog * 0.55)) {
  fail(
    `R${displayRound} field avg bogeys ${avgBogeys.toFixed(2)} too low — counting markets miscalibrated`,
  );
}
if (Number.isFinite(avgBirdies) && Number.isFinite(avgBogeys) && avgBirdies + avgBogeys + avgPars < 16.5) {
  fail(
    `R${displayRound} bird+pars+bog ${(avgBirdies + avgBogeys + avgPars).toFixed(2)} < 16.5 — hole counts do not sum to ~18`,
  );
}

const props = Array.isArray(proj.props) ? proj.props : [];
const dkSlug = String(proj.dk_league_slug || "").trim();
const skipDkGate = envTruthy("GOLF_SKIP_DK_OU_VALIDATE", false);
const anyDkProps = props.some((r) => String(r.source || "").trim().toLowerCase() === "draftkings");

if (skipDkGate || !anyDkProps) {
  const reason = skipDkGate
    ? "GOLF_SKIP_DK_OU_VALIDATE=1"
    : "DraftKings has not posted round O/U yet (pre-tournament)";
  console.warn(
    `[validate:projections] WARN: skipping DK O/U coverage gate (${reason}). Re-run fetch:book-odds after DK posts lines.`,
  );
} else {
  for (const mkt of DK_CORE_MARKETS) {
    const dkLines = props.filter(
      (r) =>
        String(r.source || "").trim().toLowerCase() === "draftkings" &&
        String(r.market || "").trim() === mkt,
    );
    const fake = props.filter(
      (r) =>
        String(r.source || "").trim().toLowerCase() !== "draftkings" &&
        String(r.market || "").trim() === mkt,
    );
    if (fake.length) {
      fail(`${fake.length} non-DK ${mkt} props in projections.json — run fetch:book-odds (DK only for this market)`);
    }
    if (dkLines.length < DK_MIN_LINES_PER_MARKET) {
      fail(
        `DraftKings ${mkt}: only ${dkLines.length} lines (need ≥${DK_MIN_LINES_PER_MARKET}) — check DK league slug${dkSlug ? ` (${dkSlug})` : ""} / Playwright scrape`,
      );
    }
  }
}

const outrightBake = proj.outright_sim_probs;
const outrightByDg = outrightBake?.by_dg;
const outrightN =
  outrightByDg && typeof outrightByDg === "object" ? Object.keys(outrightByDg).length : 0;
if (outrightN < 20) {
  fail(
    `outright_sim_probs missing or too small (${outrightN} players) — run npm run bake:outright-sim before publish`,
  );
}

const avgTotal = avg("total_score");
const { target: scoreTarget, eventWeekTrusted } = totalScoreTargetForValidation(
  basis,
  displayRound,
  coursePar,
);
const minTotal = scoreTarget - (eventWeekTrusted ? 1.0 : 1.75);
const maxTotal = scoreTarget + (eventWeekTrusted ? 0.55 : 2.75);
if (Number.isFinite(avgTotal) && avgTotal < minTotal) {
  fail(
    `R${displayRound} field avg total ${avgTotal.toFixed(2)} too low (min ${minTotal.toFixed(2)}, target ${scoreTarget.toFixed(2)}${eventWeekTrusted ? ", event-week" : ", venue hist"}) — check repair:projection-course-basis`,
  );
}
if (Number.isFinite(avgTotal) && avgTotal > maxTotal) {
  fail(
    `R${displayRound} field avg total ${avgTotal.toFixed(2)} too high (max ${maxTotal.toFixed(2)}, target ${scoreTarget.toFixed(2)}${eventWeekTrusted ? ", event-week" : ", venue hist"}) — venue target overshot`,
  );
}

const targetLabel = eventWeekTrusted ? "event-week" : "venue-hist";
console.log(
  `[validate:projections] OK — par ${coursePar} (${parSource}), R${displayRound}: ${roundRows.length} golfers; avg total ${Number.isFinite(avgTotal) ? avgTotal.toFixed(2) : "?"} (target ${scoreTarget.toFixed(2)} ${targetLabel}); avg bird/pars/bog ${avgBirdies.toFixed(2)}/${avgPars.toFixed(2)}/${avgBogeys.toFixed(2)}; outright MC baked: ${outrightN} players; DK lines: ${DK_CORE_MARKETS.map((m) => `${m}=${props.filter((r) => r.source === "draftkings" && r.market === m).length}`).join(", ")}`,
);
