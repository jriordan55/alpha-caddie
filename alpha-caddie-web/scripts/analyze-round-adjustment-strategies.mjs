#!/usr/bin/env node
/**
 * Walk-forward analysis: which round-by-round adjustments best predict actual scores?
 *   node scripts/analyze-round-adjustment-strategies.mjs
 */
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import { courseDifficultyStrokeShift } from "./course-round-adjustments.mjs";
import { bounceBackStrokeShift } from "./projection-unified-factors.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const CSV = join(__dirname, "..", "..", "data", "historical_rounds_all.csv");

function num(x, f = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : f;
}
function clamp(x, lo, hi) {
  return Math.max(lo, Math.min(hi, x));
}
function rmse(errs) {
  if (!errs.length) return NaN;
  return Math.sqrt(errs.reduce((s, e) => s + e * e, 0) / errs.length);
}
function mae(errs) {
  if (!errs.length) return NaN;
  return errs.reduce((s, e) => s + Math.abs(e), 0) / errs.length;
}
function bias(errs) {
  if (!errs.length) return NaN;
  return errs.reduce((s, e) => s + e, 0) / errs.length;
}

function rowTimeMs(row) {
  const s = String(row.event_completed || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const yr = Math.round(num(row.year, NaN));
  return Number.isFinite(yr) ? Date.parse(`${yr}-06-15T12:00:00Z`) : NaN;
}

function recencyMuSg(rows, decay = 0.86) {
  let sum = 0;
  let wsum = 0;
  for (let i = 0; i < rows.length; i++) {
    const v = num(rows[i].sg ?? rows[i].sg_total, NaN);
    if (!Number.isFinite(v)) continue;
    const w = decay ** i;
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

/** @typedef {{ dg: number, event: string, year: number, courseKey: string, par: number, rounds: Map<number, { score: number, stp: number, sg: number }> }} EventPlayer */

async function loadRows() {
  const rows = [];
  await new Promise((resolve, reject) => {
    createReadStream(CSV)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => {
        const tour = String(r.tour || "").toLowerCase();
        if (tour && tour !== "pga") return;
        const dg = Math.round(num(r.dg_id, NaN));
        const rnd = Math.round(num(r.round_num, NaN));
        const rs = num(r.round_score, NaN);
        const cp = num(r.course_par, NaN);
        const sg = num(r.sg_total, NaN);
        const yr = Math.round(num(r.year, NaN));
        const evt = String(r.event_name || "").trim();
        if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) return;
        if (!Number.isFinite(rs) || rs < 55 || rs > 95) return;
        if (!Number.isFinite(cp) || cp < 63 || cp > 76) return;
        if (!evt || !Number.isFinite(yr)) return;
        rows.push({
          dg,
          rnd,
          rs,
          cp,
          stp: rs - cp,
          sg: Number.isFinite(sg) ? sg : - (rs - cp),
          yr,
          evt,
          course: String(r.course_name || ""),
          courseKey: normCourseNameKey(r.course_name || ""),
          t: rowTimeMs(r),
        });
      })
      .on("end", resolve)
      .on("error", reject);
  });
  rows.sort((a, b) => a.t - b.t || a.dg - b.dg || a.rnd - b.rnd);
  return rows;
}

function buildEvents(rows) {
  /** @type {Map<string, { event: string, year: number, courseKey: string, par: number, t: number, players: Map<number, Map<number, { score: number, stp: number, sg: number }>> }>} */
  const events = new Map();
  for (const r of rows) {
    const ek = `${r.yr}|${foldComparableTitle(r.evt)}|${r.courseKey}`;
    if (!events.has(ek)) {
      events.set(ek, {
        event: r.evt,
        year: r.yr,
        courseKey: r.courseKey,
        par: r.cp,
        t: r.t,
        players: new Map(),
      });
    }
    const ev = events.get(ek);
    if (!ev.players.has(r.dg)) ev.players.set(r.dg, new Map());
    ev.players.get(r.dg).set(r.rnd, { score: r.rs, stp: r.stp, sg: r.sg });
  }
  return [...events.values()].sort((a, b) => a.t - b.t);
}

function venuePlayerMaps(histBefore, courseKey, beforeT) {
  const roundScores = new Map();
  const roundCounts = new Map();
  const allScores = new Map();
  const roundRecent2 = new Map();
  const fieldByRound = new Map();

  for (const r of histBefore) {
    if (r.courseKey !== courseKey) continue;
    if (Number.isFinite(beforeT) && r.t >= beforeT) continue;
    const dg = r.dg;
    const rnd = r.rnd;
    const pk = `${dg}|${rnd}`;
    if (!roundScores.has(pk)) roundScores.set(pk, []);
    roundScores.get(pk).push(r.rs);
    roundCounts.set(pk, (roundCounts.get(pk) || 0) + 1);
    if (!allScores.has(dg)) allScores.set(dg, []);
    allScores.get(dg).push(r.rs);
    if (!fieldByRound.has(rnd)) fieldByRound.set(rnd, []);
    fieldByRound.get(rnd).push(r.stp);

    const age = beforeT ? Math.max(0, Math.floor((beforeT - r.t) / (365.25 * 86400000))) : 0;
    if (age <= 2) {
      const rk = `${dg}|${rnd}`;
      if (!roundRecent2.has(rk)) roundRecent2.set(rk, []);
      roundRecent2.get(rk).push(r.rs);
    }
  }

  const mean = (a) => (a.length ? a.reduce((x, y) => x + y, 0) / a.length : NaN);
  const finRound = new Map();
  for (const [k, a] of roundScores) finRound.set(k, mean(a));
  const finAll = new Map();
  for (const [k, a] of allScores) finAll.set(k, mean(a));
  const finRecent2 = new Map();
  for (const [k, a] of roundRecent2) finRecent2.set(k, mean(a));
  const fieldStp = new Map();
  for (const [rnd, a] of fieldByRound) fieldStp.set(rnd, mean(a));

  return { finRound, finAll, finRecent2, fieldStp, roundCounts };
}

function shrinkRoundToVenue(roundPred, venueAll, n, capN = 3) {
  if (!Number.isFinite(roundPred)) return venueAll;
  if (!Number.isFinite(venueAll)) return roundPred;
  const w = clamp(n / capN, 0, 1);
  return w * roundPred + (1 - w) * venueAll;
}

function priorTourRound(histBefore, dg, beforeT) {
  let best = null;
  for (const r of histBefore) {
    if (r.dg !== dg) continue;
    if (Number.isFinite(beforeT) && r.t >= beforeT) continue;
    if (!best || r.t > best.t) best = r;
  }
  return best;
}

function venueExpandingPlayerPred(dg, tr, venue) {
  const all = venue.finAll.get(dg);
  const bucket = (r) => venue.finRound.get(`${dg}|${r}`);
  if (tr === 1) return Number.isFinite(all) ? all : NaN;
  if (tr === 2) {
    const r1 = bucket(1);
    return Number.isFinite(r1) ? r1 : all;
  }
  const parts = [];
  for (let r = 1; r < tr; r++) {
    const b = bucket(r);
    if (Number.isFinite(b)) parts.push(b);
  }
  if (parts.length) return parts.reduce((a, b) => a + b, 0) / parts.length;
  return all;
}

/** R1–R4 each use that round's venue bucket only (no expanding window). */
function venueRoundBucketOnly(dg, tr, venue, fallbackAll = true) {
  const b = venue.finRound.get(`${dg}|${tr}`);
  if (Number.isFinite(b)) return b;
  return fallbackAll ? venue.finAll.get(dg) : NaN;
}

function predictR1(dg, par, mu, venue) {
  const preds = {};
  preds.skill = par - mu;
  preds.venue_all = venue.finAll.get(dg);
  preds.venue_round = venue.finRound.get(`${dg}|1`);
  preds.venue_recent2 = venue.finRecent2.get(`${dg}|1`);
  const blend = (a, b, w) => (Number.isFinite(a) && Number.isFinite(b) ? w * a + (1 - w) * b : a ?? b);
  preds.venue_blend = blend(venue.finRound.get(`${dg}|1`), venue.finAll.get(dg), 0.55);
  const n1 = venue.roundCounts.get(`${dg}|1`) || 0;
  preds.venue_round_shrink = shrinkRoundToVenue(preds.venue_round, preds.venue_all, n1);
  preds.venue_recent2_blend = blend(preds.venue_recent2, preds.venue_all, 0.45);
  return preds;
}

function withinEventFormShift(priorSurpluses, k, cap, fieldShare = 0.35) {
  let sh = 0;
  for (const { player, field } of priorSurpluses) {
    let target = player;
    if (Number.isFinite(field) && Number.isFinite(player)) target = fieldShare * field + (1 - fieldShare) * player;
    else if (Number.isFinite(field)) target = field;
    else if (!Number.isFinite(player)) continue;
    sh += k * target;
  }
  return clamp(sh, -cap, cap);
}

function fieldPriorExcess(event, targetRound, fieldMeanMu) {
  let sum = 0;
  let n = 0;
  for (const [, rounds] of event.players) {
    for (let rn = 1; rn < targetRound; rn++) {
      const pr = rounds.get(rn);
      if (!pr) continue;
      sum += pr.stp - (-fieldMeanMu);
      n++;
    }
  }
  return n >= 12 ? sum / n : NaN;
}

function evalStrategy(name, pred, actual, bucket, store) {
  if (!Number.isFinite(pred)) return;
  const err = pred - actual;
  if (!store[name]) store[name] = { errs: [], byRound: { 1: [], 2: [], 3: [], 4: [] }, byPrior: { good: [], bad: [], neutral: [] } };
  store[name].errs.push(err);
  if (bucket.round) store[name].byRound[bucket.round].push(err);
  if (bucket.priorBucket) store[name].byPrior[bucket.priorBucket].push(err);
}

async function main() {
  console.log("Loading historical_rounds_all.csv …");
  const rows = await loadRows();
  const events = buildEvents(rows);
  console.log(`Events: ${events.length}, rows: ${rows.length}`);

  const histByDg = new Map();
  for (const r of rows) {
    if (!histByDg.has(r.dg)) histByDg.set(r.dg, []);
    histByDg.get(r.dg).push(r);
  }
  for (const arr of histByDg.values()) arr.sort((a, b) => b.t - a.t);

  const store = {};
  const formKs = [0, 0.04, 0.08, 0.12, 0.16, 0.2];
  const bounceKs = [0, 0.08, 0.15, 0.22, 0.3];
  /** @type {Map<string, Map<number, number>>} */
  const policyPredsByPlayerEvent = new Map();

  let nObs = 0;

  for (const ev of events) {
    const beforeT = ev.t;
    const histBefore = rows.filter((r) => r.t < beforeT);
    const venue = venuePlayerMaps(histBefore, ev.courseKey, beforeT);

    const fieldPlayers = [];
    for (const [dg] of ev.players) {
      const past = (histByDg.get(dg) || []).filter((r) => r.t < beforeT);
      const mu = recencyMuSg(past);
      if (Number.isFinite(mu)) fieldPlayers.push({ dg, mu });
    }
    if (fieldPlayers.length < 20) continue;
    const fieldMeanMu = fieldPlayers.reduce((s, p) => s + p.mu, 0) / fieldPlayers.length;

    for (const [dg, rounds] of ev.players) {
      const past = (histByDg.get(dg) || []).filter((r) => r.t < beforeT);
      const mu = recencyMuSg(past);
      if (!Number.isFinite(mu)) continue;

      const r1 = rounds.get(1);
      if (!r1) continue;

      const r1preds = predictR1(dg, ev.par, mu, venue);
      const r1base =
        Number.isFinite(r1preds.venue_round_shrink) ? r1preds.venue_round_shrink
        : Number.isFinite(r1preds.venue_all) ? r1preds.venue_all
        : r1preds.skill;
      const peKey = `${ev.year}|${ev.courseKey}|${dg}`;
      if (!policyPredsByPlayerEvent.has(peKey)) policyPredsByPlayerEvent.set(peKey, new Map());
      policyPredsByPlayerEvent.get(peKey).set(1, r1base);

      for (const [name, pred] of Object.entries(r1preds)) {
        evalStrategy(`R1_${name}`, pred, r1.score, { round: 1, priorBucket: null }, store);
      }

      for (let tr = 1; tr <= 4; tr++) {
        const act = rounds.get(tr);
        if (!act) continue;
        const expanding = venueExpandingPlayerPred(dg, tr, venue);
        const bucketOnly = venueRoundBucketOnly(dg, tr, venue);
        const allFlat = venue.finAll.get(dg);
        evalStrategy("venue_expanding", expanding, act.score, { round: tr, priorBucket: null }, store);
        evalStrategy("venue_round_bucket", bucketOnly, act.score, { round: tr, priorBucket: null }, store);
        evalStrategy("venue_alltime_flat", allFlat, act.score, { round: tr, priorBucket: null }, store);
        let expandingShrink = expanding;
        if (tr >= 2) {
          const parts = [];
          for (let r = 1; r < tr; r++) {
            const raw = venue.finRound.get(`${dg}|${r}`);
            const n = venue.roundCounts.get(`${dg}|${r}`) || 0;
            const shrunk = shrinkRoundToVenue(raw, allFlat, n);
            if (Number.isFinite(shrunk)) parts.push(shrunk);
          }
          if (tr === 2 && parts.length) expandingShrink = parts[0];
          else if (parts.length) expandingShrink = parts.reduce((a, b) => a + b, 0) / parts.length;
          else if (Number.isFinite(allFlat)) expandingShrink = allFlat;
        } else if (Number.isFinite(allFlat)) {
          expandingShrink = allFlat;
        }
        evalStrategy("venue_expanding_shrink", expandingShrink, act.score, { round: tr, priorBucket: null }, store);
      }

      const lastTour = priorTourRound(histBefore, dg, beforeT);
      if (lastTour) {
        const surprise = lastTour.stp - (-mu);
        let priorBucket = "neutral";
        if (surprise <= -1) priorBucket = "good";
        else if (surprise >= 1) priorBucket = "bad";
        for (const k of bounceKs) {
          const adj = -k * surprise;
          evalStrategy(`R1_prior_tour_bounce_k${k}`, r1base + adj, r1.score, { round: 1, priorBucket }, store);
        }
        for (const k of formKs) {
          const adj = -k * surprise * 1.05;
          evalStrategy(`R1_prior_tour_momo_k${k}`, r1base - adj, r1.score, { round: 1, priorBucket }, store);
        }
      }

      let baseScore = r1base;
      const priorSurpluses = [];

      for (let tr = 2; tr <= 4; tr++) {
        const act = rounds.get(tr);
        if (!act) continue;
        nObs++;

        const expectedStp = -mu;
        const prior = rounds.get(tr - 1);
        const priorPrior = tr >= 3 ? rounds.get(tr - 2) : null;
        const priorSurprise = prior ? prior.stp - expectedStp : NaN;
        const priorPriorSurprise = priorPrior ? priorPrior.stp - expectedStp : NaN;

        let priorBucket = "neutral";
        if (Number.isFinite(priorSurprise)) {
          if (priorSurprise <= -1) priorBucket = "good";
          else if (priorSurprise >= 1) priorBucket = "bad";
        }

        const venueRoundPred = venue.finRound.get(`${dg}|${tr}`);
        const predVenueRound = Number.isFinite(venueRoundPred) ? venueRoundPred : baseScore;

        evalStrategy("R2plus_naive_prior_score", prior?.score, act.score, { round: tr, priorBucket }, store);
        evalStrategy("R2plus_skill_static", ev.par - mu, act.score, { round: tr, priorBucket }, store);
        evalStrategy("R2plus_venue_round", predVenueRound, act.score, { round: tr, priorBucket }, store);
        evalStrategy("R2plus_carry_base", baseScore, act.score, { round: tr, priorBucket }, store);

        const fieldSurPrior = (() => {
          let s = 0;
          let n = 0;
          for (const [, prs] of ev.players) {
            const p = prs.get(tr - 1);
            if (!p) continue;
            s += p.stp - expectedStp;
            n++;
          }
          return n >= 12 ? s / n : NaN;
        })();

        const playerSurPrior = prior ? prior.stp - expectedStp : NaN;

        for (const k of formKs) {
          const formSg = withinEventFormShift(
            priorSurpluses.map((x) => ({ player: x.player, field: x.field })),
            k,
            0.45,
          );
          const pred = baseScore - formSg;
          evalStrategy(`R2plus_within_form_k${k}`, pred, act.score, { round: tr, priorBucket }, store);
        }

        if (Number.isFinite(priorSurprise)) {
          for (const k of bounceKs) {
            const bb = -k * priorSurprise;
            evalStrategy(`R2plus_bounce_k${k}`, baseScore + bb, act.score, { round: tr, priorBucket }, store);
          }
          const bbModel = bounceBackStrokeShift(
            prior.stp,
            expectedStp,
            priorPrior?.stp,
            expectedStp,
          );
          evalStrategy("R2plus_bounce_model", baseScore + bbModel, act.score, { round: tr, priorBucket }, store);

          for (const k of bounceKs) {
            const momo = -k * priorSurprise;
            evalStrategy(`R2plus_momentum_k${k}`, baseScore - momo, act.score, { round: tr, priorBucket }, store);
          }
        }

        const fldEx = fieldPriorExcess(ev, tr, fieldMeanMu);
        if (Number.isFinite(fldEx)) {
          const shift = courseDifficultyStrokeShift(fldEx);
          evalStrategy("R2plus_course_difficulty", baseScore + shift, act.score, { round: tr, priorBucket }, store);
        }

        if (Number.isFinite(fieldSurPrior)) {
          evalStrategy("R2plus_field_prior_stp", baseScore + fieldSurPrior * 0.45, act.score, { round: tr, priorBucket }, store);
        }
        if (Number.isFinite(playerSurPrior) && Number.isFinite(fieldSurPrior)) {
          const blendSur = 0.35 * fieldSurPrior + 0.65 * playerSurPrior;
          evalStrategy("R2plus_player_field_surprise", baseScore + blendSur * 0.4, act.score, { round: tr, priorBucket }, store);
        }

        const fieldRndStp = venue.fieldStp.get(tr);
        if (Number.isFinite(fieldRndStp)) {
          evalStrategy("R2plus_venue_field_round", ev.par + fieldRndStp, act.score, { round: tr, priorBucket }, store);
        }

        let policyPred = baseScore;
        if (tr === 2 && Number.isFinite(fieldSurPrior)) {
          policyPred = baseScore + fieldSurPrior * 0.45;
        } else if (tr >= 3) {
          const formSg = withinEventFormShift(
            priorSurpluses.map((x) => ({ player: x.player, field: x.field })),
            0.08,
            0.35,
          );
          policyPred = baseScore - formSg;
        }
        if (priorBucket === "bad" && Number.isFinite(priorSurprise)) {
          policyPred += bounceBackStrokeShift(prior.stp, expectedStp, priorPrior?.stp, expectedStp);
        } else if (priorBucket === "good" && Number.isFinite(priorSurprise)) {
          policyPred += clamp(-0.1 * priorSurprise, -0.35, 0.35);
        }
        evalStrategy("policy_combo_recommended", policyPred, act.score, { round: tr, priorBucket }, store);
        policyPredsByPlayerEvent.get(peKey)?.set(tr, policyPred);

        if (priorBucket === "bad" && Number.isFinite(priorSurprise)) {
          evalStrategy("R2plus_bad_only_bounce", baseScore + bounceBackStrokeShift(prior.stp, expectedStp, priorPrior?.stp, expectedStp), act.score, { round: tr, priorBucket }, store);
        }
        if (priorBucket === "good" && Number.isFinite(priorSurprise)) {
          evalStrategy("R2plus_good_only_fade", baseScore + clamp(-0.12 * priorSurprise, -0.4, 0.4), act.score, { round: tr, priorBucket }, store);
        }

        priorSurpluses.push({
          player: playerSurPrior,
          field: fieldSurPrior,
        });
      }
    }
  }

  console.log(`\nObservations R2–R4: ${nObs}\n`);

  const r1Strats = Object.keys(store).filter((k) => k.startsWith("R1_")).sort((a, b) => rmse(store[a].errs) - rmse(store[b].errs));
  console.log("=== R1 strategies (lowest RMSE first) ===");
  for (const name of r1Strats.slice(0, 12)) {
    const s = store[name];
    console.log(
      name.padEnd(28),
      "n=" + String(s.errs.length).padStart(5),
      "RMSE",
      rmse(s.errs).toFixed(2),
      "MAE",
      mae(s.errs).toFixed(2),
      "bias",
      bias(s.errs).toFixed(2),
    );
  }

  const r2Strats = Object.keys(store).filter((k) => k.startsWith("R2plus_") && !k.includes("_k")).sort((a, b) => rmse(store[a].errs) - rmse(store[b].errs));
  console.log("\n=== R2–R4 strategies (no k-sweep, lowest RMSE) ===");
  for (const name of r2Strats.slice(0, 14)) {
    const s = store[name];
    console.log(name.padEnd(32), "RMSE", rmse(s.errs).toFixed(2), "MAE", mae(s.errs).toFixed(2), "bias", bias(s.errs).toFixed(2));
  }

  function bestK(prefix) {
    const keys = Object.keys(store).filter((k) => k.startsWith(prefix + "_k"));
    keys.sort((a, b) => rmse(store[a].errs) - rmse(store[b].errs));
    if (!keys.length) return null;
    const best = keys[0];
    return { name: best, rmse: rmse(store[best].errs), mae: mae(store[best].errs) };
  }

  console.log("\n=== Best k parameters ===");
  for (const p of ["R2plus_within_form", "R2plus_bounce", "R2plus_momentum", "R1_prior_tour_bounce", "R1_prior_tour_momo"]) {
    const b = bestK(p);
    if (b) console.log(b.name, "RMSE", b.rmse.toFixed(2), "MAE", b.mae.toFixed(2));
  }

  console.log("\n=== R2–R4 by round: best few strategies ===");
  const top = ["R2plus_venue_round", "R2plus_bounce_k0.15", "R2plus_within_form_k0.12", "R2plus_carry_base", "R2plus_naive_prior_score"];
  for (const rnd of [2, 3, 4]) {
    console.log("Round", rnd);
    for (const name of top) {
      const s = store[name];
      if (!s) continue;
      const errs = s.byRound[rnd];
      if (!errs.length) continue;
      console.log(" ", name.padEnd(32), "n=" + errs.length, "RMSE", rmse(errs).toFixed(2));
    }
  }

  console.log("\n=== After good / bad / neutral prior round (R2–R4, selected) ===");
  for (const name of ["R2plus_bounce_k0.15", "R2plus_within_form_k0.12", "R2plus_momentum_k0.12", "R2plus_carry_base", "policy_combo_recommended"]) {
    const s = store[name];
    if (!s) continue;
    console.log(name);
    for (const bucket of ["good", "neutral", "bad"]) {
      const errs = s.byPrior[bucket];
      if (!errs.length) continue;
      console.log("  prior", bucket.padEnd(8), "n=" + String(errs.length).padStart(5), "RMSE", rmse(errs).toFixed(2), "bias", bias(errs).toFixed(2));
    }
  }

  const spreads = { r12: [], r23: [], r34: [] };
  for (const roundMap of policyPredsByPlayerEvent.values()) {
    const p1 = roundMap.get(1);
    const p2 = roundMap.get(2);
    const p3 = roundMap.get(3);
    const p4 = roundMap.get(4);
    if (Number.isFinite(p1) && Number.isFinite(p2)) spreads.r12.push(Math.abs(p2 - p1));
    if (Number.isFinite(p2) && Number.isFinite(p3)) spreads.r23.push(Math.abs(p3 - p2));
    if (Number.isFinite(p3) && Number.isFinite(p4)) spreads.r34.push(Math.abs(p4 - p3));
  }
  console.log("\n=== Recommended policy round separation (strokes) ===");
  for (const [k, arr] of Object.entries(spreads)) {
    if (!arr.length) continue;
    const mean = arr.reduce((a, b) => a + b, 0) / arr.length;
    console.log(k, "mean |delta|", mean.toFixed(2), "n", arr.length);
  }

  if (store.policy_combo_recommended) {
    const s = store.policy_combo_recommended;
    console.log("\n=== policy_combo_recommended overall ===");
    console.log("RMSE", rmse(s.errs).toFixed(2), "MAE", mae(s.errs).toFixed(2), "bias", bias(s.errs).toFixed(2));
    for (const rnd of [2, 3, 4]) {
      const errs = s.byRound[rnd];
      if (!errs.length) continue;
      console.log(" R" + rnd, "RMSE", rmse(errs).toFixed(2), "n", errs.length);
    }
  }

  console.log("\n=== Venue expanding window (your proposal) vs alternatives ===");
  for (const name of ["venue_expanding", "venue_expanding_shrink", "venue_round_bucket", "venue_alltime_flat"]) {
    const s = store[name];
    if (!s) continue;
    console.log(name, "overall RMSE", rmse(s.errs).toFixed(2), "MAE", mae(s.errs).toFixed(2));
    for (const rnd of [1, 2, 3, 4]) {
      const errs = s.byRound[rnd];
      if (!errs.length) continue;
      console.log("  R" + rnd, "n=" + errs.length, "RMSE", rmse(errs).toFixed(2), "bias", bias(errs).toFixed(2));
    }
  }

  const venueSpread = { r12: [], r23: [], r34: [] };
  for (const ev of events) {
    const beforeT = ev.t;
    const histBefore = rows.filter((r) => r.t < beforeT);
    const venue = venuePlayerMaps(histBefore, ev.courseKey, beforeT);
    for (const [dg, rounds] of ev.players) {
      if (!rounds.get(1)) continue;
      const p = [];
      for (let tr = 1; tr <= 4; tr++) {
        const pred = venueExpandingPlayerPred(dg, tr, venue);
        if (Number.isFinite(pred)) p[tr] = pred;
      }
      if (Number.isFinite(p[1]) && Number.isFinite(p[2])) venueSpread.r12.push(Math.abs(p[2] - p[1]));
      if (Number.isFinite(p[2]) && Number.isFinite(p[3])) venueSpread.r23.push(Math.abs(p[3] - p[2]));
      if (Number.isFinite(p[3]) && Number.isFinite(p[4])) venueSpread.r34.push(Math.abs(p[4] - p[3]));
    }
  }
  console.log("\n=== venue_expanding projection spread ===");
  for (const [k, arr] of Object.entries(venueSpread)) {
    if (!arr.length) continue;
    console.log(k, "mean |delta|", (arr.reduce((a, b) => a + b, 0) / arr.length).toFixed(2), "n", arr.length);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
