#!/usr/bin/env node
/**
 * Guards Round Projections μ from in-play collapse (birdies ~0 on completed rounds, etc.).
 * DK/PrizePicks O/U lines are full-round — the table must use useFullRoundBookMean.
 *
 *   npm run verify:ou-round-projection-means
 */
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const APP = join(WEB, "app.js");

function fail(msg) {
  console.error(`[verify:ou-round-projection-means] FAIL: ${msg}`);
  process.exit(1);
}

function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

function clamp(v, lo, hi) {
  return Math.min(hi, Math.max(lo, v));
}

// --- Minimal replicas of app.js live-adjust helpers (keep in sync with app.js) ---

function ouUsesFullRoundBookMean(opts = {}) {
  return opts.useFullRoundBookMean === true || opts.skipLivePartial === true;
}

function inferBirdiesSoFarFromTodayVsPar(today, thru) {
  if (!Number.isFinite(today) || !Number.isFinite(thru) || thru < 1) return NaN;
  if (today >= 0) return 0;
  const under = -today;
  return Math.min(thru, Math.max(0, Math.round(under * 0.52)));
}

function livePartialRoundCountPropAdjust(market, row, inPlayAffects = true) {
  const out = { muDelta: 0, sigmaScale: 1 };
  if (!inPlayAffects) return out;
  if (market !== "Birdies" && market !== "Pars" && market !== "Bogeys") return out;
  const thru = Math.round(num(row.dg_live_thru, NaN));
  const today = num(row.dg_live_today, NaN);
  if (!Number.isFinite(thru) || thru < 1) return out;
  if (thru >= 18) return out;
  const rem = 18 - thru;
  const field = market === "Birdies" ? "birdies" : market === "Pars" ? "pars" : "bogeys";
  const muFull = num(row[field], NaN);
  if (!Number.isFinite(muFull) || muFull < 0) return out;
  let b = num(row.dg_live_birdies_so_far, NaN);
  if (!Number.isFinite(b)) b = inferBirdiesSoFarFromTodayVsPar(today, thru);
  if (!Number.isFinite(b)) b = 0;
  const rate = muFull / 18;
  const soFar = market === "Birdies" ? b : 0;
  let muLive = soFar + rate * rem;
  muLive = clamp(muLive, 0, 18);
  out.muDelta = muLive - muFull;
  out.sigmaScale = clamp(Math.sqrt(rem / 18), 0.17, 1);
  return out;
}

function liveCurrentRoundTotalScoreMuDelta(row, par18 = 72) {
  const baseMu = num(row.total_score, NaN);
  const thru = Math.round(num(row.dg_live_thru, NaN));
  const today = num(row.dg_live_today, NaN);
  if (!Number.isFinite(baseMu) || !Number.isFinite(par18)) return 0;
  if (!Number.isFinite(today)) return 0;
  if (Number.isFinite(thru) && thru >= 18) return 0;
  if (!Number.isFinite(thru) || thru < 1) return 0;
  const parThru = (par18 / 18) * thru;
  const parRem = par18 - parThru;
  const rem = 18 - thru;
  if (rem <= 0) return 0;
  const expExcessRem = ((baseMu - par18) * rem) / 18;
  const actualStrokes = parThru + today;
  const muLive = actualStrokes + parRem + expExcessRem;
  return clamp(muLive - baseMu, -12, 12);
}

function ouInPlayCountingAdjust(market, row, opts = {}) {
  if (ouUsesFullRoundBookMean(opts)) return { muDelta: 0, sigmaScale: 1 };
  return livePartialRoundCountPropAdjust(market, row);
}

function ouInPlayMuDeltaForTotalScore(row, opts = {}) {
  if (ouUsesFullRoundBookMean(opts)) return 0;
  return liveCurrentRoundTotalScoreMuDelta(row);
}

// --- Static source checks ---

const src = readFileSync(APP, "utf8");

if (!/const OU_PROJ_TABLE_MEAN_OPTS = Object\.freeze\(\{\s*useFullRoundBookMean:\s*true\s*\}\)/.test(src)) {
  fail("OU_PROJ_TABLE_MEAN_OPTS must freeze { useFullRoundBookMean: true }");
}

const requiredPatterns = [
  /ouProjectedMean\(col\.market, player, OU_PROJ_TABLE_MEAN_OPTS\)/,
  /ouProjectedMean\(market, player, OU_PROJ_TABLE_MEAN_OPTS\)/,
  /ouProjectedMean\("Total score", p, OU_PROJ_TABLE_MEAN_OPTS\)/,
  /ouEdgeForCell\([\s\S]*?OU_PROJ_TABLE_MEAN_OPTS/,
  /ouInPlayCountingAdjust\(mKey, player, OU_PROJ_TABLE_MEAN_OPTS\)/,
  /(?:ouInPlaySigmaScaleForMarket\(mKey, player[\s\S]*?OU_PROJ_TABLE_MEAN_OPTS|propPricingSigmaForOu\([\s\S]*?OU_PROJ_TABLE_MEAN_OPTS)/,
];

for (let i = 0; i < requiredPatterns.length; i++) {
  if (!requiredPatterns[i].test(src)) {
    fail(`app.js missing Round Projections guard (pattern ${i + 1})`);
  }
}

if (/const OU_PROJ_TABLE_MEAN_OPTS = Object\.freeze\(\{\s*skipLivePartial:/.test(src)) {
  fail("remove legacy skipLivePartial OU_PROJ_TABLE_MEAN_OPTS duplicate");
}

if (!/if \(Number\.isFinite\(thru\) && thru >= 18\) \{\s*return 0;\s*\}/.test(src)) {
  fail("liveCurrentRoundTotalScoreMuDelta must return 0 when thru >= 18 (not snap to final score)");
}

if (!/function ouInPlayCountingAdjust\(market, row, opts = \{\}\)/.test(src)) {
  fail("ouInPlayCountingAdjust helper missing — Round Projections must route through it");
}

// --- Behavioral regression (Eric Cole–style completed round) ---

const completedLiveRow = {
  birdies: 4.1,
  total_score: 71.5,
  dg_live_thru: 18,
  dg_live_today: 5,
  round: 2,
};

const liveOpts = {};
const tableOpts = { useFullRoundBookMean: true };

const birdiesLive = livePartialRoundCountPropAdjust("Birdies", completedLiveRow);
const birdiesTable = ouInPlayCountingAdjust("Birdies", completedLiveRow, tableOpts);
const birdiesMuTable = completedLiveRow.birdies + birdiesTable.muDelta;
if (birdiesMuTable < completedLiveRow.birdies * 0.5) {
  fail(`completed-round birdies μ collapsed to ${birdiesMuTable} (expected ~${completedLiveRow.birdies})`);
}

const scoreLiveDelta = liveCurrentRoundTotalScoreMuDelta(completedLiveRow);
const scoreTableDelta = ouInPlayMuDeltaForTotalScore(completedLiveRow, tableOpts);
if (Math.abs(scoreTableDelta) > 0.01) {
  fail(`Total score table mu delta should be 0 on completed round, got ${scoreTableDelta}`);
}
// In-play betting path may still adjust mid-round; completed round partial adjust is no-op.
if (birdiesLive.muDelta !== 0) {
  fail(`livePartialRoundCountPropAdjust should no-op at thru=18, got muDelta=${birdiesLive.muDelta}`);
}

const inProgressRow = {
  birdies: 4.1,
  dg_live_thru: 9,
  dg_live_today: -2,
  dg_live_birdies_so_far: 2,
  round: 2,
};
const inPlayBirdies = livePartialRoundCountPropAdjust("Birdies", inProgressRow);
if (inPlayBirdies.muDelta === 0) {
  fail("in-progress round should still get live birdies μ adjust when not using useFullRoundBookMean");
}
const inPlayTable = ouInPlayCountingAdjust("Birdies", inProgressRow, tableOpts);
if (inPlayTable.muDelta !== 0) {
  fail("useFullRoundBookMean must zero in-play birdies μDelta even mid-round");
}

const markets = ["Birdies", "Pars", "Bogeys", "GIR", "Fairways hit", "Total score"];
for (const m of markets) {
  const row =
    m === "Total score"
      ? { ...completedLiveRow }
      : { ...completedLiveRow, pars: 10.2, bogeys: 2.8, gir: 11.5, fairways: 9.2 };
  const adj = m === "Total score" ? { muDelta: ouInPlayMuDeltaForTotalScore(row, tableOpts) } : ouInPlayCountingAdjust(m, row, tableOpts);
  if (Math.abs(adj.muDelta) > 0.001) {
    fail(`${m}: table opts must not apply in-play μDelta on completed round`);
  }
}

console.log(
  "[verify:ou-round-projection-means] OK — useFullRoundBookMean wired, completed-round μ stable for all markets",
);
