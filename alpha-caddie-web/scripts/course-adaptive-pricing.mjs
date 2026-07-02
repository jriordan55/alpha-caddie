/**
 * Single adaptive pricing path: recent form + venue history + SG weighted by what the course demands.
 * Course-table regression coeffs (putt_sg, app_sg, ott_sg, arg_sg) define category importance —
 * e.g. TPC River Highlands (6800 yds) emphasizes approach and putting over driving distance.
 */
import { loadCourseTablePayload, resolveCourseTableRow } from "./projection-unified-factors.mjs";

const SG_KEYS = ["sg_ott", "sg_app", "sg_arg", "sg_putt"];
export const RECENT_FORM_MIN = 8;
export const RECENT_FORM_MAX = 12;
const CT_COEFF = { sg_ott: "ott_sg", sg_app: "app_sg", sg_arg: "arg_sg", sg_putt: "putt_sg" };

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function meanNumFromRounds(rounds, key) {
  const vals = [];
  for (const r of rounds) {
    const v = num(r[key], NaN);
    if (Number.isFinite(v)) vals.push(v);
  }
  return vals.length ? vals.reduce((a, b) => a + b, 0) / vals.length : NaN;
}

function meanNumFromRoundsRecencyWeighted(rounds, key, decay = 0.86) {
  if (!rounds?.length) return NaN;
  let sum = 0;
  let wsum = 0;
  for (let i = 0; i < rounds.length; i++) {
    const v = num(rounds[i][key], NaN);
    if (!Number.isFinite(v)) continue;
    const w = decay ** i;
    sum += v * w;
    wsum += w;
  }
  return wsum > 0 ? sum / wsum : NaN;
}

/** Positive course-table coeffs → normalized SG category weights for this venue. */
export function courseRequirementSgWeights(ctRow) {
  const raw = {};
  let sum = 0;
  for (const sk of SG_KEYS) {
    const coeff = num(ctRow?.[CT_COEFF[sk]], 0);
    const w = Math.max(0, coeff);
    raw[sk] = w;
    sum += w;
  }
  if (sum < 1e-6) {
    return { sg_ott: 0.25, sg_app: 0.25, sg_arg: 0.25, sg_putt: 0.25 };
  }
  const out = {};
  for (const sk of SG_KEYS) out[sk] = raw[sk] / sum;
  return out;
}

/** Static course-fit: player SG × venue coeffs (strokes gained bonus, not form delta). */
export function courseTablePlayerMuNudge(playerRow, ctRow, weight = 0.14) {
  if (!ctRow || !playerRow) return 0;
  let fit =
    num(playerRow.sg_putt, 0) * num(ctRow.putt_sg, 0) +
    num(playerRow.sg_arg, 0) * num(ctRow.arg_sg, 0) +
    num(playerRow.sg_app, 0) * num(ctRow.app_sg, 0) +
    num(playerRow.sg_ott, 0) * num(ctRow.ott_sg, 0);
  return clamp(-fit * weight, -0.32, 0.32);
}

/** Recent-vs-older form delta on SG categories weighted by course requirements. */
export function courseWeightedSkillFormBonus(rounds, ctRow, playerRow, players, modelRound) {
  const weights = courseRequirementSgWeights(ctRow);
  const nRec = Math.min(RECENT_FORM_MAX, Math.max(RECENT_FORM_MIN, rounds?.length >= RECENT_FORM_MIN ? 10 : 6));
  const recent = rounds.slice(0, nRec);
  const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 28));

  let formSum = 0;
  let formW = 0;
  for (const sk of SG_KEYS) {
    const w = weights[sk];
    if (w < 0.02) continue;
    const rMean = meanNumFromRounds(recent, sk);
    const oMean = meanNumFromRounds(older, sk);
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      formSum += w * (rMean - oMean);
      formW += w;
    }
  }
  let out = formW > 0 ? clamp(formSum / formW * 0.82, -0.38, 0.38) : 0;

  // Field z-score on highest-weight category when history is thin.
  if (Math.abs(out) < 0.04 && playerRow && players?.length >= 8) {
    const topSk = SG_KEYS.reduce((best, sk) => (weights[sk] > weights[best] ? sk : best), "sg_app");
    const id = Math.round(num(playerRow.dg_id, NaN));
    const row = players.find(
      (p) => Math.round(num(p.dg_id)) === id && Math.round(num(p.round)) === modelRound,
    );
    if (row) {
      const v = num(row[topSk], NaN);
      if (Number.isFinite(v)) {
        const vals = [];
        for (const p of players) {
          if (Math.round(num(p.round)) !== modelRound) continue;
          const x = num(p[topSk], NaN);
          if (Number.isFinite(x)) vals.push(x);
        }
        if (vals.length >= 8) {
          vals.sort((a, b) => a - b);
          const mid = Math.floor(vals.length / 2);
          const median = vals.length % 2 === 1 ? vals[mid] : (vals[mid - 1] + vals[mid]) / 2;
          out = clamp((v - median) * 0.14 * (weights[topSk] + 0.35), -0.28, 0.28);
        }
      }
    }
  }

  const staticFit = courseTablePlayerMuNudge(playerRow, ctRow, 0.1);
  return clamp(out + staticFit, -0.42, 0.42);
}

export function recentFormMuBonus(rounds) {
  const nRec = Math.min(RECENT_FORM_MAX, Math.max(RECENT_FORM_MIN, rounds?.length >= RECENT_FORM_MIN ? 10 : 6));
  const recent = rounds.slice(0, nRec);
  const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 24));
  let rMean = meanNumFromRounds(recent, "sg_total");
  let oMean = meanNumFromRounds(older, "sg_total");
  if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
    return clamp((rMean - oMean) * 0.9, -0.35, 0.35);
  }
  rMean = meanNumFromRounds(recent, "round_score");
  oMean = meanNumFromRounds(older, "round_score");
  if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
    return clamp(((oMean - rMean) / 6) * 0.85, -0.35, 0.35);
  }
  return 0;
}

export function courseHistoryMuBonus(rounds, venueName, courseNameMatchesVenue) {
  if (!venueName || !rounds?.length) return { bonus: 0, venueRounds: 0 };
  const here = rounds.filter((r) => courseNameMatchesVenue(r.course_name, venueName));
  if (here.length < 2) return { bonus: 0, venueRounds: here.length };
  const other = rounds.filter((r) => !courseNameMatchesVenue(r.course_name, venueName));
  const hMean = meanNumFromRoundsRecencyWeighted(here, "sg_total", 0.84);
  const oMean = meanNumFromRoundsRecencyWeighted(other.length ? other : rounds, "sg_total", 0.9);
  if (Number.isFinite(hMean) && Number.isFinite(oMean)) {
    return {
      bonus: clamp((hMean - oMean) * 1.05, -0.42, 0.42),
      venueRounds: here.length,
    };
  }
  return { bonus: 0, venueRounds: here.length };
}

/** Blend recent / course / course-weighted-skill into one μ-SG bonus. */
export function blendAdaptiveMuSgBonus(recent, course, skill, venueRounds = 0) {
  let wRecent = 0.36;
  let wCourse = 0.28;
  let wSkill = 0.36;
  if (venueRounds >= 4) {
    wCourse = 0.34;
    wRecent = 0.3;
    wSkill = 0.36;
  } else if (venueRounds < 2) {
    wCourse = 0.1;
    wRecent = 0.34;
    wSkill = 0.56;
  }
  const blend = wRecent * recent + wCourse * course + wSkill * skill;
  return clamp(blend, -0.32, 0.32);
}

export function isAdaptivePricingMode(modeRaw) {
  const m = String(modeRaw || "default").toLowerCase();
  return m === "default" || m === "adaptive";
}

/** Market nudge from venue counting history (scaled for adaptive — not course-only). */
export function adaptiveVenueStatMuNudge(market, venueAvg, broadAvg, modeRaw) {
  if (!Number.isFinite(venueAvg) || !Number.isFinite(broadAvg)) return 0;
  const strength = isAdaptivePricingMode(modeRaw) ? 0.62 : String(modeRaw).toLowerCase() === "course" ? 1 : 0;
  if (strength <= 0) return 0;
  const delta = venueAvg - broadAvg;
  if (market === "Total score") return clamp(-delta * 0.55 * strength, -2.8, 2.8);
  if (market === "Bogeys") return clamp(delta * 0.42 * strength, -1.8, 1.8);
  if (market === "Birdies") return clamp(delta * 0.48 * strength, -1.8, 1.8);
  if (market === "Pars") return clamp(delta * 0.12 * strength, -1.2, 1.2);
  if (market === "GIR") return clamp(delta * 3.2 * strength, -2.2, 2.2);
  if (market === "Fairways hit") return clamp(delta * 2.4 * strength, -2.2, 2.2);
  if (market === "Putts") return clamp(-delta * 0.38 * strength, -2.2, 2.2);
  return 0;
}

/** Extra market nudge from course-weighted current SG vs field median. */
export function courseWeightedMarketMuNudge(market, playerRow, ctRow, players, modelRound) {
  if (!ctRow || !playerRow || !players?.length) return 0;
  const weights = courseRequirementSgWeights(ctRow);
  const id = Math.round(num(playerRow.dg_id, NaN));
  const row = players.find((p) => Math.round(num(p.dg_id)) === id && Math.round(num(p.round)) === modelRound);
  if (!row) return 0;

  const fieldMedian = (sk) => {
    const vals = [];
    for (const p of players) {
      if (Math.round(num(p.round)) !== modelRound) continue;
      const x = num(p[sk], NaN);
      if (Number.isFinite(x)) vals.push(x);
    }
    if (vals.length < 8) return NaN;
    vals.sort((a, b) => a - b);
    const mid = Math.floor(vals.length / 2);
    return vals.length % 2 === 1 ? vals[mid] : (vals[mid - 1] + vals[mid]) / 2;
  };

  let sk = null;
  let scale = 0;
  if (market === "GIR") {
    sk = "sg_app";
    scale = 0.42 * (weights.sg_app + 0.25);
  } else if (market === "Fairways hit") {
    sk = "sg_ott";
    scale = 0.35 * (weights.sg_ott + 0.2);
  } else if (market === "Birdies") {
    sk = weights.sg_app >= weights.sg_putt ? "sg_app" : "sg_putt";
    scale = 0.38 * (weights.sg_app + weights.sg_putt);
  } else if (market === "Putts") {
    sk = "sg_putt";
    scale = 0.4 * (weights.sg_putt + 0.2);
  } else if (market === "Total score") {
    const fit = courseTablePlayerMuNudge(row, ctRow, 0.22);
    return clamp(-fit * 1.05, -1.4, 1.4);
  } else {
    return 0;
  }

  const med = fieldMedian(sk);
  const v = num(row[sk], NaN);
  if (!Number.isFinite(med) || !Number.isFinite(v)) return 0;
  return clamp((v - med) * scale, -1.6, 1.6);
}

export function resolveCourseTableForVenue(venueName) {
  const ctPayload = loadCourseTablePayload();
  return resolveCourseTableRow(ctPayload, venueName);
}
