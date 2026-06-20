#!/usr/bin/env node
/**
 * Gate publish (push:live) — projections must have correct par, counting stats, and DK O/U for core markets.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");

const DK_CORE_MARKETS = ["Total Score", "Birdies", "Pars", "Bogeys"];
const DK_MIN_LINES_PER_MARKET = 20;

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
const coursePar = Math.round(num(proj.course_par_18, parFromHoles));
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
if (Number.isFinite(avgBogeys) && avgBogeys < 3.2) {
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
      `DraftKings ${mkt}: only ${dkLines.length} lines (need ≥${DK_MIN_LINES_PER_MARKET}) — check us-open slug / Playwright scrape`,
    );
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
const basis = proj.projection_course_basis || proj.meta?.projection_course_basis || {};
const ewMean = num(basis.event_week_field_avg_score, NaN);
const minTotal = Number.isFinite(ewMean) ? ewMean - 1.0 : coursePar + 1.5;
if (Number.isFinite(avgTotal) && avgTotal < minTotal) {
  fail(
    `R${displayRound} field avg total ${avgTotal.toFixed(2)} too low (min ${minTotal.toFixed(2)}) — upcoming-round venue calibration missing or stale`,
  );
}

console.log(
  `[validate:projections] OK — par ${coursePar} (${parSource}), R${displayRound}: ${roundRows.length} golfers; avg total ${Number.isFinite(avgTotal) ? avgTotal.toFixed(2) : "?"}; avg bird/pars/bog ${avgBirdies.toFixed(2)}/${avgPars.toFixed(2)}/${avgBogeys.toFixed(2)}; outright MC baked: ${outrightN} players; DK lines: ${DK_CORE_MARKETS.map((m) => `${m}=${props.filter((r) => r.source === "draftkings" && r.market === m).length}`).join(", ")}`,
);
