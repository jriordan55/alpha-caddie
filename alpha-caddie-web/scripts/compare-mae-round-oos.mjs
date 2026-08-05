#!/usr/bin/env node
/**
 * MAE-first round projections — DataGolf / Connolly–Rendleman start-over.
 *
 * Walk-forward, no future leakage:
 *   score μ = course/round/year/wave STP + weather − player SG
 *   SG      = exp-decay (sequence ⊕ calendar) + SG-category reweight + shrunk course history/fit
 *   counts  = same additive stack on the raw stat
 *   MAE*    = book + α·(model − book)  with event-level LOO α  (plus consensus books)
 *
 *   npm run compare:mae-round-oos
 *   → data/mae_round_oos.json
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";
import { MARKET_BOOK_CALIBRATION_MARKETS } from "./market-book-calibration.mjs";
import { EXPORT_MARKETS } from "./round-projection-mu.mjs";
import { resolveWalkforwardWeather } from "./historical-walkforward-projections.mjs";
import { statWeatherMuAdjustment } from "./weather-mu-adjustments.mjs";
import { teeWaveFromTeetimeAndLabel } from "./open-meteo-forecast.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const HIST = join(REPO, "data", "historical_rounds_all.csv");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const PROJ = join(WEB, "projections.json");
const OUT_JSON = join(WEB, "data", "mae_round_oos.json");

const MARKETS = MARKET_BOOK_CALIBRATION_MARKETS;
const N_FW = 14;
const LAMBDA_SEQ = -Math.log(0.3) / 50;
const LAMBDA_TIME = Math.log(2) / 150;
const SEQ_SHARE = 0.55;
const SAME_EVENT_BOOST = 2.4;
const CAT_MIX = 0.28;
const CAT_BETA = { ott: 1.15, app: 1.0, arg: 0.88, putt: 0.55 };
const FIT_K = 90;
const K = {
  "Total score": { course: 8, year: 5, round: 8, wave: 12, player: 14, pc: 22, hole: 18 },
  Birdies: { course: 12, year: 6, round: 10, wave: 14, player: 26, pc: 24, hole: 20 },
  GIR: { course: 10, year: 5, round: 8, wave: 12, player: 16, pc: 16, hole: 18 },
  "Fairways hit": { course: 8, year: 4, round: 8, wave: 12, player: 10, pc: 12, hole: 16 },
};
const BOOK_STEM = {
  "Total score": "round_score",
  Birdies: "birdies",
  GIR: "gir",
  "Fairways hit": "fairways",
};
const ALT_BOOKS = ["pp", "sl", "ud", "fd", "czr", "kl"];

function nnum(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}
function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}
function shrink(n, k) {
  return Math.max(0, n) / (Math.max(0, n) + k);
}
function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
}
function completedMs(row) {
  const s = String(row?.event_completed || "").trim();
  const iso = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) return Date.parse(`${iso[1]}-${iso[2]}-${iso[3]}T12:00:00Z`);
  const mdy = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})/);
  if (mdy) return Date.parse(`${mdy[3]}-${mdy[1].padStart(2, "0")}-${mdy[2].padStart(2, "0")}T12:00:00Z`);
  return parseMs(s);
}
function girFwCount(raw, nHoles) {
  const v = nnum(raw, NaN);
  if (!Number.isFinite(v)) return NaN;
  if (v > 0 && v <= 1.0001) return v * nHoles;
  if (v > 1 && v <= nHoles + 0.51) return v;
  if (v > nHoles && v <= 100) return (v / 100) * nHoles;
  return NaN;
}
function placeholderCounts(row) {
  const b = nnum(row.birdies, NaN);
  const p = nnum(row.pars, NaN);
  const bg = nnum(row.bogeys ?? row.bogies, NaN);
  return b === 0 && bg === 0 && (!Number.isFinite(p) || p === 0 || p >= 10);
}
function marketVal(market, row) {
  if (market === "Total score") {
    const rs = nnum(row.round_score, NaN);
    return Number.isFinite(rs) && rs >= 55 && rs <= 95 ? rs : NaN;
  }
  if (placeholderCounts(row)) return NaN;
  if (market === "Birdies") {
    const b = nnum(row.birdies, NaN);
    if (!Number.isFinite(b) || b < 0 || b > 18) return NaN;
    const e = nnum(row.eagles_or_better ?? row.eagles, 0);
    return b + Math.max(0, Number.isFinite(e) ? e : 0);
  }
  if (market === "GIR") return girFwCount(row.gir, 18);
  if (market === "Fairways hit") {
    const raw = Number.isFinite(nnum(row.driving_acc, NaN)) ? row.driving_acc : row.fairways;
    return girFwCount(raw, N_FW);
  }
  return NaN;
}
function clampMu(market, mu, par = 72) {
  if (!Number.isFinite(mu)) return NaN;
  const p = Number.isFinite(par) && par >= 68 && par <= 73 ? par : 72;
  if (market === "Total score") return clamp(mu, p - 8, p + 14);
  if (market === "Birdies") return clamp(mu, 0.4, 10);
  if (market === "GIR") return clamp(mu, 5, 16.5);
  if (market === "Fairways hit") return clamp(mu, 2, N_FW + 0.5);
  return mu;
}
function wmean(items) {
  let sw = 0;
  let sx = 0;
  for (const it of items) {
    if (!Number.isFinite(it.v) || !(it.w > 0)) continue;
    sw += it.w;
    sx += it.w * it.v;
  }
  return sw > 0 ? { mean: sx / sw, n: sw } : { mean: NaN, n: 0 };
}
function neff(weights) {
  let s = 0;
  let s2 = 0;
  for (const w of weights) {
    if (!(w > 0)) continue;
    s += w;
    s2 += w * w;
  }
  return s2 > 0 ? (s * s) / s2 : 0;
}

const MARKET_COLS = Object.fromEntries(
  EXPORT_MARKETS.map((m) => [
    m.market,
    { model: m.lineCol, book: m.bookLineCol, actual: m.actualCol },
  ]),
);

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? nnum(s, NaN) : NaN;
}

function consensusBook(row, market) {
  const stem = BOOK_STEM[market];
  if (!stem) return NaN;
  const vals = [parseLine(row[`${stem}_book_line`])];
  for (const b of ALT_BOOKS) vals.push(parseLine(row[`${stem}_${b}_line`]));
  const ok = vals.filter((x) => Number.isFinite(x));
  if (!ok.length) return NaN;
  return ok.reduce((a, b) => a + b, 0) / ok.length;
}

async function loadHistTyped() {
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);
  /** @type {object[]} */
  const rows = [];
  await new Promise((resolvePromise, reject) => {
    createReadStream(HIST)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => {
        const dg = Math.round(nnum(r.dg_id, NaN));
        const t = completedMs(r);
        const ck = normCourseNameKey(r.course_name || "");
        const par = nnum(r.course_par, NaN);
        if (!Number.isFinite(dg) || !ck) return;
        const vals = {};
        let any = false;
        for (const m of MARKETS) {
          const v = marketVal(m, r);
          vals[m] = v;
          if (Number.isFinite(v)) any = true;
        }
        if (!any) return;
        const wave = teeWaveFromTeetimeAndLabel(r.teetime ?? r.tee_time, r.dg_tee_wave);
        const startHole = Math.round(nnum(r.start_hole, NaN));
        rows.push({
          dg,
          t: Number.isFinite(t) ? t : 0,
          ck,
          par: Number.isFinite(par) && par >= 68 && par <= 73 ? par : 72,
          event: String(r.event_name || "").trim(),
          year: Math.round(nnum(r.year, NaN)),
          round: Math.round(nnum(r.round_num, NaN)),
          vals,
          sg: {
            total: nnum(r.sg_total, NaN),
            ott: nnum(r.sg_ott, NaN),
            app: nnum(r.sg_app, NaN),
            arg: nnum(r.sg_arg, NaN),
            putt: nnum(r.sg_putt, NaN),
          },
          dist: nnum(r.driving_dist, NaN),
          acc: (() => {
            const a = nnum(r.driving_acc, NaN);
            if (!Number.isFinite(a)) return NaN;
            if (a > 0 && a <= 1.0001) return a * 100;
            if (a > 1 && a <= 100) return a;
            return NaN;
          })(),
          wave,
          startHole: startHole === 1 || startHole === 10 ? startHole : NaN,
        });
      })
      .on("end", resolvePromise)
      .on("error", reject);
  });
  rows.sort((a, b) => a.t - b.t || a.year - b.year || a.round - b.round);
  return rows;
}

function courseParMap(hist) {
  /** @type {Map<string, Map<number, number>>} */
  const tallies = new Map();
  for (const r of hist) {
    const m = tallies.get(r.ck) || new Map();
    m.set(r.par, (m.get(r.par) || 0) + 1);
    tallies.set(r.ck, m);
  }
  /** @type {Map<string, number>} */
  const out = new Map();
  for (const [ck, m] of tallies) {
    let bestP = 72;
    let bestN = -1;
    for (const [p, n] of m) {
      if (n > bestN) {
        bestN = n;
        bestP = p;
      }
    }
    out.set(ck, bestP);
  }
  return out;
}

async function loadEvalRows(parByCourse) {
  const raw = readFileSync(VS, "utf8");
  const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
  const aligned = alignDetailCsvContent(raw, headerLine);
  const rows = [];
  await new Promise((resolvePromise, reject) => {
    Readable.from([aligned])
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        if (String(row.pricing_mode || "") !== "default") return;
        if (String(row.book_odds_source || "") !== "pre_round_audit") return;
        const event = String(row.event_name || "").trim();
        const dg = Math.round(nnum(row.dg_id, NaN));
        const rnd = Math.round(nnum(row.round, NaN));
        const ck = normCourseNameKey(row.course_name || row.course_used || "");
        if (!event || !Number.isFinite(dg) || !Number.isFinite(rnd)) return;
        const t = parseMs(row.projections_updated_at) || parseMs(row.exported_at);
        const waveRaw = String(row.tee_wave || "").trim().toLowerCase();
        const wave = waveRaw.includes("morn") || waveRaw === "am" || waveRaw === "early"
          ? "morning"
          : waveRaw.includes("after") || waveRaw === "pm" || waveRaw === "late"
            ? "afternoon"
            : "";
        for (const market of MARKETS) {
          const cols = MARKET_COLS[market];
          if (!cols) continue;
          const actual = parseLine(row[cols.actual]);
          const book = parseLine(row[cols.book]);
          const wf = parseLine(row[cols.model]);
          const cons = consensusBook(row, market);
          if (!Number.isFinite(actual)) continue;
          if ((market === "Birdies" || market === "Bogeys") && actual === 0) {
            const sc = parseLine(row.actual_round_score);
            if (Number.isFinite(sc) && sc > 0) continue;
          }
          rows.push({
            event,
            eventMs: t,
            market,
            dg,
            round: rnd,
            courseKey: ck,
            actual,
            book,
            consensus: cons,
            wf,
            par: parByCourse.get(ck) || 72,
            wave,
            sgOtt: nnum(row.sg_ott, NaN),
            sgApp: nnum(row.sg_app, NaN),
            weatherRow: {
              weather_temp_f: nnum(row.weather_temp_f, NaN),
              weather_wind_mph: nnum(row.weather_wind_mph, NaN),
              weather_condition: row.weather_condition || "",
            },
          });
        }
      })
      .on("end", resolvePromise)
      .on("error", reject);
  });
  return rows;
}

function loadLiveEvent() {
  if (!existsSync(PROJ)) return "";
  try {
    const j = JSON.parse(readFileSync(PROJ, "utf8"));
    return String(j?.event_name || j?.meta?.event_name || "").trim();
  } catch {
    return "";
  }
}

function prefixBefore(hist, cutoffMs, eventName, targetRound, eventYear) {
  const out = [];
  for (const r of hist) {
    if (r.t >= cutoffMs && !(r.t === 0 && cutoffMs > 1e11)) {
      const sameEv = eventsLikelySame(r.event, eventName);
      const sameYr = !Number.isFinite(eventYear) || !Number.isFinite(r.year) || r.year === eventYear;
      if (!(sameEv && sameYr && Number.isFinite(r.round) && r.round < targetRound)) continue;
    } else if (eventsLikelySame(r.event, eventName)) {
      const sameYr = !Number.isFinite(eventYear) || !Number.isFinite(r.year) || r.year === eventYear;
      if (sameYr && Number.isFinite(r.round) && r.round >= targetRound) continue;
    }
    out.push(r);
  }
  return out;
}

function decayWeight(roundsAgo, daysAgo, sameEventEarlier) {
  const seq = Math.exp(-LAMBDA_SEQ * Math.max(0, roundsAgo));
  const tim = Math.exp(-LAMBDA_TIME * Math.max(0, daysAgo));
  let w = SEQ_SHARE * seq + (1 - SEQ_SHARE) * tim;
  if (sameEventEarlier) w *= SAME_EVENT_BOOST;
  return w;
}

function effectsAtCutoff(histPrefix, market, cutoffMs, eventName, eventYear) {
  const kk = K[market];
  let tourSum = 0;
  let tourN = 0;
  /** @type {Map<string, { sum: number, n: number }>} */
  const course = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseYear = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseRound = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseWave = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const courseHole = new Map();

  for (const r of histPrefix) {
    const v = r.vals[market];
    if (!Number.isFinite(v)) continue;
    tourSum += v;
    tourN++;
    const c = course.get(r.ck) || { sum: 0, n: 0 };
    c.sum += v;
    c.n++;
    course.set(r.ck, c);
  }
  const tour = tourN ? tourSum / tourN : NaN;
  /** @type {Map<string, number>} */
  const courseEff = new Map();
  for (const [ck, c] of course) courseEff.set(ck, shrink(c.n, kk.course) * (c.sum / c.n - tour));

  for (const r of histPrefix) {
    const v = r.vals[market];
    if (!Number.isFinite(v)) continue;
    const ce = courseEff.get(r.ck) || 0;
    const resid = v - tour - ce;
    const yk = `${r.ck}|${r.year}`;
    const y = courseYear.get(yk) || { sum: 0, n: 0 };
    y.sum += resid;
    y.n++;
    courseYear.set(yk, y);
    if (Number.isFinite(r.round) && r.round >= 1 && r.round <= 4) {
      const rk = `${r.ck}|${r.round}`;
      const rr = courseRound.get(rk) || { sum: 0, n: 0 };
      rr.sum += resid;
      rr.n++;
      courseRound.set(rk, rr);
    }
    if (r.wave === "morning" || r.wave === "afternoon") {
      const wk = `${r.ck}|${r.wave}`;
      const w = courseWave.get(wk) || { sum: 0, n: 0 };
      w.sum += resid;
      w.n++;
      courseWave.set(wk, w);
    }
    if (r.startHole === 1 || r.startHole === 10) {
      const hk = `${r.ck}|${r.startHole}`;
      const h = courseHole.get(hk) || { sum: 0, n: 0 };
      h.sum += resid;
      h.n++;
      courseHole.set(hk, h);
    }
  }
  /** @type {Map<string, number>} */
  const yearEff = new Map();
  for (const [k, x] of courseYear) yearEff.set(k, shrink(x.n, kk.year) * (x.sum / x.n));
  /** @type {Map<string, number>} */
  const roundEff = new Map();
  for (const [k, x] of courseRound) roundEff.set(k, shrink(x.n, kk.round) * (x.sum / x.n));
  /** @type {Map<string, number>} */
  const waveEff = new Map();
  for (const [k, x] of courseWave) waveEff.set(k, shrink(x.n, kk.wave) * (x.sum / x.n));
  /** @type {Map<string, number>} */
  const holeEff = new Map();
  for (const [k, x] of courseHole) holeEff.set(k, shrink(x.n, kk.hole) * (x.sum / x.n));

  /** @type {Map<number, object[]>} */
  const byPlayer = new Map();
  for (const r of histPrefix) {
    const v = r.vals[market];
    if (!Number.isFinite(v)) continue;
    const arr = byPlayer.get(r.dg) || [];
    arr.push(r);
    byPlayer.set(r.dg, arr);
  }

  /** @type {Map<number, { pe: number, sg: number, dist: number, acc: number, ott: number, app: number, arg: number, putt: number, n: number }>} */
  const playerSkill = new Map();
  /** @type {Map<string, { sum: number, n: number }>} */
  const pc = new Map();

  for (const [dg, rounds] of byPlayer) {
    rounds.sort((a, b) => a.t - b.t || a.round - b.round);
    const items = [];
    const sgItems = [];
    const distItems = [];
    const accItems = [];
    const ottItems = [];
    const appItems = [];
    const argItems = [];
    const puttItems = [];
    const ws = [];
    for (let i = 0; i < rounds.length; i++) {
      const r = rounds[i];
      const v = r.vals[market];
      const ce = courseEff.get(r.ck) || 0;
      const ye = yearEff.get(`${r.ck}|${r.year}`) || 0;
      const re = roundEff.get(`${r.ck}|${r.round}`) || 0;
      const we = r.wave ? waveEff.get(`${r.ck}|${r.wave}`) || 0 : 0;
      const he = Number.isFinite(r.startHole) ? holeEff.get(`${r.ck}|${r.startHole}`) || 0 : 0;
      const resid = v - tour - ce - ye - re - we - he;
      const roundsAgo = rounds.length - 1 - i;
      const daysAgo = Number.isFinite(cutoffMs) && r.t > 0 ? (cutoffMs - r.t) / 86400000 : roundsAgo * 7;
      const sameEv =
        eventsLikelySame(r.event, eventName) &&
        (!Number.isFinite(eventYear) || r.year === eventYear) &&
        Number.isFinite(r.round);
      const w = decayWeight(roundsAgo, daysAgo, sameEv);
      items.push({ v: resid, w });
      ws.push(w);
      if (Number.isFinite(r.sg.total)) sgItems.push({ v: r.sg.total, w });
      if (Number.isFinite(r.dist) && r.dist > 230 && r.dist < 380) distItems.push({ v: r.dist, w });
      if (Number.isFinite(r.acc)) accItems.push({ v: r.acc, w });
      if (Number.isFinite(r.sg.ott)) ottItems.push({ v: r.sg.ott, w });
      if (Number.isFinite(r.sg.app)) appItems.push({ v: r.sg.app, w });
      if (Number.isFinite(r.sg.arg)) argItems.push({ v: r.sg.arg, w });
      if (Number.isFinite(r.sg.putt)) puttItems.push({ v: r.sg.putt, w });
      const pk = `${dg}|${r.ck}`;
      const x = pc.get(pk) || { sum: 0, n: 0 };
      x.sum += resid * w;
      x.n += w;
      pc.set(pk, x);
    }
    const wm = wmean(items);
    const nE = neff(ws);
    const pe = shrink(nE, kk.player) * (Number.isFinite(wm.mean) ? wm.mean : 0);
    const sgW = wmean(sgItems);
    let sg = shrink(sgW.n, kk.player) * (Number.isFinite(sgW.mean) ? sgW.mean : 0);
    const ott = wmean(ottItems).mean;
    const app = wmean(appItems).mean;
    const arg = wmean(argItems).mean;
    const putt = wmean(puttItems).mean;
    if (
      market === "Total score" &&
      [ott, app, arg, putt].every((x) => Number.isFinite(x))
    ) {
      const cat =
        CAT_BETA.ott * ott + CAT_BETA.app * app + CAT_BETA.arg * arg + CAT_BETA.putt * putt;
      const catNorm = cat / ((CAT_BETA.ott + CAT_BETA.app + CAT_BETA.arg + CAT_BETA.putt) / 4);
      if (Number.isFinite(sgW.mean)) sg = shrink(sgW.n, kk.player) * ((1 - CAT_MIX) * sgW.mean + CAT_MIX * catNorm);
      else sg = shrink(Math.min(sgW.n || 8, 20), kk.player) * catNorm;
    }
    playerSkill.set(dg, {
      pe,
      sg,
      dist: wmean(distItems).mean,
      acc: wmean(accItems).mean,
      ott: Number.isFinite(ott) ? ott : 0,
      app: Number.isFinite(app) ? app : 0,
      arg: Number.isFinite(arg) ? arg : 0,
      putt: Number.isFinite(putt) ? putt : 0,
      n: nE,
    });
  }

  let distMu = 0;
  let distN = 0;
  let accMu = 0;
  let accN = 0;
  for (const s of playerSkill.values()) {
    if (Number.isFinite(s.dist)) {
      distMu += s.dist;
      distN++;
    }
    if (Number.isFinite(s.acc)) {
      accMu += s.acc;
      accN++;
    }
  }
  distMu = distN ? distMu / distN : 295;
  accMu = accN ? accMu / accN : 60;
  let distSd = 0;
  let accSd = 0;
  for (const s of playerSkill.values()) {
    if (Number.isFinite(s.dist)) distSd += (s.dist - distMu) ** 2;
    if (Number.isFinite(s.acc)) accSd += (s.acc - accMu) ** 2;
  }
  distSd = distN > 2 ? Math.sqrt(distSd / distN) : 12;
  accSd = accN > 2 ? Math.sqrt(accSd / accN) : 8;

  /** @type {Map<string, { xx: number[][], xy: number[], n: number }>} */
  const fitAcc = new Map();
  for (const r of histPrefix) {
    const v = r.vals[market];
    if (!Number.isFinite(v)) continue;
    const sk = playerSkill.get(r.dg);
    if (!sk || sk.n < 8) continue;
    const ce = courseEff.get(r.ck) || 0;
    const ye = yearEff.get(`${r.ck}|${r.year}`) || 0;
    const re = roundEff.get(`${r.ck}|${r.round}`) || 0;
    const y = v - tour - ce - ye - re - sk.pe;
    const distZ = Number.isFinite(sk.dist) ? (sk.dist - distMu) / distSd : 0;
    const accZ = Number.isFinite(sk.acc) ? (sk.acc - accMu) / accSd : 0;
    const x = [distZ, accZ, sk.app, sk.arg, sk.putt];
    const bucket = fitAcc.get(r.ck) || { xx: [0, 0, 0, 0, 0].map(() => [0, 0, 0, 0, 0]), xy: [0, 0, 0, 0, 0], n: 0 };
    for (let i = 0; i < 5; i++) {
      bucket.xy[i] += x[i] * y;
      for (let j = 0; j < 5; j++) bucket.xx[i][j] += x[i] * x[j];
    }
    bucket.n++;
    fitAcc.set(r.ck, bucket);
  }
  /** @type {Map<string, number[]>} */
  const fitSlope = new Map();
  for (const [ck, b] of fitAcc) {
    const lam = FIT_K / Math.max(1, b.n);
    const slopes = [];
    for (let i = 0; i < 5; i++) {
      const denom = b.xx[i][i] + lam;
      slopes.push(denom > 1e-8 ? (b.xy[i] / denom) * shrink(b.n, FIT_K) : 0);
    }
    fitSlope.set(ck, slopes);
  }

  /** @type {Map<string, number>} */
  const pcEff = new Map();
  for (const [pk, x] of pc) {
    const dg = Number(pk.split("|")[0]);
    const pe = playerSkill.get(dg)?.pe || 0;
    const mean = x.n > 0 ? x.sum / x.n - pe : 0;
    pcEff.set(pk, shrink(x.n, kk.pc) * mean);
  }

  return {
    tour,
    courseEff,
    yearEff,
    roundEff,
    waveEff,
    holeEff,
    playerSkill,
    pcEff,
    fitSlope,
    distMu,
    distSd,
    accMu,
    accSd,
  };
}

function predict(eff, row, weatherSnap) {
  const market = row.market;
  const tour = eff?.tour;
  if (!Number.isFinite(tour)) return NaN;
  const ck = row.courseKey;
  const sk = eff.playerSkill.get(row.dg);
  const ce = eff.courseEff.get(ck) || 0;
  const ye = Number.isFinite(row.eventYear) ? eff.yearEff.get(`${ck}|${row.eventYear}`) || 0 : 0;
  const re = eff.roundEff.get(`${ck}|${row.round}`) || 0;
  const wave = row.wave || "";
  const we = wave ? eff.waveEff.get(`${ck}|${wave}`) || 0 : 0;
  const he = Number.isFinite(row.startHole) ? eff.holeEff.get(`${ck}|${row.startHole}`) || 0 : 0;
  const pe = sk?.pe || 0;
  const pce = eff.pcEff.get(`${row.dg}|${ck}`) || 0;
  let mu = tour + ce + ye + re + we + he + pe + pce;
  const slopes = eff.fitSlope.get(ck);
  if (slopes && sk) {
    const distZ = Number.isFinite(sk.dist) ? (sk.dist - eff.distMu) / (eff.distSd || 12) : 0;
    const accZ = Number.isFinite(sk.acc) ? (sk.acc - eff.accMu) / (eff.accSd || 8) : 0;
    const x = [distZ, accZ, sk.app, sk.arg, sk.putt];
    for (let i = 0; i < 5; i++) mu += (slopes[i] || 0) * x[i];
  }
  if (market === "Total score" && sk && Number.isFinite(sk.sg) && Math.abs(sk.sg) > 0) {
    const sgBridge = tour + ce + ye + re - sk.sg;
    mu = 0.62 * mu + 0.38 * sgBridge;
    if (Number.isFinite(row.sgOtt) && Number.isFinite(row.sgApp)) {
      const ball = row.sgOtt + row.sgApp;
      mu -= 0.12 * ball;
    }
  }
  const wxRow = Number.isFinite(row.weatherRow?.weather_wind_mph)
    ? row.weatherRow
    : weatherSnap
      ? {
          weather_temp_f: weatherSnap.tempF,
          weather_wind_mph: weatherSnap.windMph,
          weather_humidity: weatherSnap.humidityPct,
          weather_condition: weatherSnap.condition,
        }
      : null;
  if (wxRow) mu += statWeatherMuAdjustment(market, wxRow);
  return clampMu(market, mu, row.par);
}

function maeOf(pairs) {
  let n = 0;
  let abs = 0;
  let err = 0;
  for (const p of pairs) {
    if (!Number.isFinite(p.pred) || !Number.isFinite(p.actual)) continue;
    n++;
    abs += Math.abs(p.pred - p.actual);
    err += p.pred - p.actual;
  }
  return n
    ? { n, mae: Math.round((abs / n) * 1000) / 1000, bias: Math.round((err / n) * 100) / 100 }
    : { n: 0, mae: null, bias: null };
}

function fitAlpha(pairs) {
  let sxx = 0;
  let sxy = 0;
  for (const p of pairs) {
    if (!Number.isFinite(p.model) || !Number.isFinite(p.book) || !Number.isFinite(p.actual)) continue;
    const x = p.model - p.book;
    const y = p.actual - p.book;
    sxx += x * x;
    sxy += x * y;
  }
  if (sxx < 1e-8) return 0;
  return clamp(sxy / sxx, -0.15, 1.15);
}

function bestBlend2(pairsA, pairsB) {
  let bestW = 0.5;
  let bestMae = Infinity;
  for (let w = 0; w <= 10; w++) {
    const ww = w / 10;
    const pairs = pairsA.map((p, i) => ({
      actual: p.actual,
      pred: ww * p.pred + (1 - ww) * pairsB[i].pred,
    }));
    const m = maeOf(pairs);
    if (m.n && m.mae < bestMae) {
      bestMae = m.mae;
      bestW = ww;
    }
  }
  return bestW;
}

async function main() {
  const t0 = Date.now();
  console.log("[mae-oos] loading history + eval rows…");
  const hist = await loadHistTyped();
  const parByCourse = courseParMap(hist);
  let evalRows = await loadEvalRows(parByCourse);
  const live = loadLiveEvent();
  evalRows = evalRows.filter((r) => !live || !eventsLikelySame(r.event, live));

  const waveByKey = new Map();
  const holeByKey = new Map();
  for (const h of hist) {
    const k = `${foldComparableTitle(h.event)}|${h.year}|${h.round}|${h.dg}`;
    if (h.wave) waveByKey.set(k, h.wave);
    if (Number.isFinite(h.startHole)) holeByKey.set(k, h.startHole);
  }

  const events = [...new Set(evalRows.map((r) => r.event))];
  console.log(`[mae-oos] ${hist.length} hist rounds · ${evalRows.length} eval rows · ${events.length} events · exclude ${live || "none"}`);

  const bundles = new Map();
  for (const r of evalRows) {
    const key = `${foldComparableTitle(r.event)}|${r.round}`;
    if (!bundles.has(key)) {
      bundles.set(key, { event: r.event, round: r.round, eventMs: r.eventMs, courseKey: r.courseKey });
    } else if (!Number.isFinite(bundles.get(key).eventMs) && Number.isFinite(r.eventMs)) {
      bundles.get(key).eventMs = r.eventMs;
    }
    r.bundleKey = key;
  }

  /** @type {Map<string, object>} */
  const bundleCache = new Map();
  let i = 0;
  const bundleList = [...bundles.entries()];
  for (const [key, b] of bundleList) {
    const evYear = hist.find((h) => eventsLikelySame(h.event, b.event) && Number.isFinite(h.year) && h.t <= (b.eventMs || Infinity))?.year
      ?? hist.find((h) => eventsLikelySame(h.event, b.event))?.year;
    b.eventYear = evYear;
    let cutoff = b.eventMs;
    if (!Number.isFinite(cutoff) || cutoff < 1e11) {
      const ts = hist
        .filter((h) => eventsLikelySame(h.event, b.event) && (!Number.isFinite(evYear) || h.year === evYear))
        .map((h) => h.t)
        .filter((t) => Number.isFinite(t) && t > 1e11);
      cutoff = ts.length ? Math.min(...ts) : Date.parse("2024-01-01T00:00:00Z");
    }
    b.eventMs = cutoff;
    const prefix = prefixBefore(hist, cutoff, b.event, b.round, evYear);
    const byM = {};
    for (const m of MARKETS) byM[m] = effectsAtCutoff(prefix, m, cutoff, b.event, evYear);
    const weatherSnap = resolveWalkforwardWeather({
      webRoot: WEB,
      histRows: prefix.map((h) => ({
        event_name: h.event,
        year: h.year,
        round_num: h.round,
        event_id: NaN,
        event_completed: Number.isFinite(h.t) && h.t > 1e11 ? new Date(h.t).toISOString().slice(0, 10) : "",
      })),
      eventName: b.event,
      eventYear: evYear,
      targetRound: b.round,
    });
    bundleCache.set(key, { byM, weatherSnap, eventYear: evYear });
    i++;
    if (i % 5 === 0 || i === bundleList.length) process.stdout.write(`\r  effects ${i}/${bundleList.length}`);
  }
  process.stdout.write("\n");

  const scored = [];
  for (const r of evalRows) {
    const pack = bundleCache.get(r.bundleKey);
    r.eventYear = pack.eventYear;
    if (!r.wave) {
      const wk = `${foldComparableTitle(r.event)}|${r.eventYear}|${r.round}|${r.dg}`;
      r.wave = waveByKey.get(wk) || "";
      r.startHole = holeByKey.get(wk);
    } else {
      r.startHole = holeByKey.get(`${foldComparableTitle(r.event)}|${r.eventYear}|${r.round}|${r.dg}`);
    }
    const pred = predict(pack.byM[r.market], r, pack.weatherSnap);
    scored.push({ ...r, additive: pred });
  }

  /** @type {Record<string, any>} */
  const report = {};
  /** @type {Record<string, string>} */
  const recipe = {};
  for (const market of MARKETS) {
    const rows = scored.filter((r) => r.market === market && Number.isFinite(r.actual));
    const byEvent = new Map();
    for (const r of rows) {
      if (!byEvent.has(r.event)) byEvent.set(r.event, []);
      byEvent.get(r.event).push(r);
    }

    const bookPairs = rows.map((r) => ({ actual: r.actual, pred: r.book }));
    const consPairs = rows.map((r) => ({ actual: r.actual, pred: r.consensus }));
    const wfPairs = rows.map((r) => ({ actual: r.actual, pred: r.wf }));
    const addPairs = rows.map((r) => ({ actual: r.actual, pred: r.additive }));

    const looBlend = [];
    const looDkAlpha = [];
    const looConsAlpha = [];
    const looWfAlpha = [];
    const ws = [];
    const alphas = [];
    const consAlphas = [];
    for (const [ev, evRows] of byEvent) {
      const train = rows.filter((x) => x.event !== ev);
      const w = bestBlend2(
        train.map((x) => ({ actual: x.actual, pred: x.additive })),
        train.map((x) => ({ actual: x.actual, pred: x.wf })),
      );
      const aDk = fitAlpha(train.map((x) => ({ actual: x.actual, model: x.additive, book: x.book })));
      const aCons = fitAlpha(train.map((x) => ({ actual: x.actual, model: x.additive, book: x.consensus })));
      const aWf = fitAlpha(train.map((x) => ({ actual: x.actual, model: x.wf, book: x.consensus })));
      ws.push(w);
      alphas.push(aDk);
      consAlphas.push(aCons);
      for (const r of evRows) {
        looBlend.push({ actual: r.actual, pred: w * r.additive + (1 - w) * r.wf });
        looDkAlpha.push({
          actual: r.actual,
          pred: Number.isFinite(r.book) ? r.book + aDk * (r.additive - r.book) : r.additive,
        });
        looConsAlpha.push({
          actual: r.actual,
          pred: Number.isFinite(r.consensus) ? r.consensus + aCons * (r.additive - r.consensus) : r.additive,
        });
        looWfAlpha.push({
          actual: r.actual,
          pred: Number.isFinite(r.consensus) ? r.consensus + aWf * (r.wf - r.consensus) : r.wf,
        });
      }
    }

    const candidates = {
      book_dk: maeOf(bookPairs),
      book_consensus: maeOf(consPairs),
      current_wf: maeOf(wfPairs),
      additive: maeOf(addPairs),
      blend_add_wf_loo: {
        ...maeOf(looBlend),
        mean_w_additive: Math.round((ws.reduce((s, x) => s + x, 0) / Math.max(1, ws.length)) * 100) / 100,
      },
      book_plus_additive_loo: {
        ...maeOf(looDkAlpha),
        mean_alpha: Math.round((alphas.reduce((s, x) => s + x, 0) / Math.max(1, alphas.length)) * 100) / 100,
      },
      consensus_plus_additive_loo: {
        ...maeOf(looConsAlpha),
        mean_alpha: Math.round((consAlphas.reduce((s, x) => s + x, 0) / Math.max(1, consAlphas.length)) * 100) / 100,
      },
      consensus_plus_wf_loo: maeOf(looWfAlpha),
    };
    report[market] = candidates;

    let bestName = "book_dk";
    let bestMae = Infinity;
    for (const [name, m] of Object.entries(candidates)) {
      if (Number.isFinite(m.mae) && m.mae < bestMae) {
        bestMae = m.mae;
        bestName = name;
      }
    }
    recipe[market] = bestName;

    console.log(`\n${market}  → use ${bestName}`);
    for (const [name, m] of Object.entries(candidates)) {
      const extra =
        name === "blend_add_wf_loo"
          ? `  w_add=${m.mean_w_additive}`
          : name === "book_plus_additive_loo" || name === "consensus_plus_additive_loo"
            ? `  α=${m.mean_alpha}`
            : "";
      console.log(`  ${name.padEnd(28)} mae ${m.mae}  bias ${m.bias}  n=${m.n}${extra}`);
    }
  }

  const payload = {
    generated_at: new Date().toISOString(),
    goal: "minimize MAE vs actual for all round markets",
    method:
      "Walk-forward Connolly–Rendleman / DataGolf stack: exp-decay SG (sequence+calendar), SG-category reweight, shrunk course/year/round/wave/start-hole + course-fit slopes + weather. MAE blends via event LOO residual-on-book (α) and consensus books.",
    citations: [
      "Connolly & Rendleman JASA 2008 — player skill + round-course difficulty",
      "DataGolf predictive model (2018–2021) — exp decay, SG cats OTT>APP>ARG>PUTT, random-effects course fit, weather",
      "Efron–Morris / James–Stein shrinkage; arxiv 2506.21822 putting-like stats shrink harder",
      "Book residual correction: μ* = book + α(model−book) is the MAE-optimal linear blend",
    ],
    constants: { K, LAMBDA_SEQ, LAMBDA_TIME, SEQ_SHARE, SAME_EVENT_BOOST, CAT_MIX, CAT_BETA, FIT_K },
    recommended_mu: recipe,
    excluded_live_event: live || null,
    events,
    elapsed_sec: Math.round((Date.now() - t0) / 1000),
    by_market: report,
  };
  writeFileSync(OUT_JSON, `${JSON.stringify(payload, null, 2)}\n`);
  console.log(`\n[mae-oos] wrote ${OUT_JSON} (${payload.elapsed_sec}s)`);
  console.log("[mae-oos] recommended", recipe);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
