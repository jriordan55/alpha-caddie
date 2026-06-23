#!/usr/bin/env node
/**
 * Hunt for edge signals: course traits, weather, cross-stat mismatches, round phase.
 * Joins backtest bets + dk audit + course_table.csv (+ optional player history for weather).
 *
 *   node scripts/analyze-edge-signals.mjs
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import { eventsLikelySame } from "./dg-events-align.mjs";
import {
  modelEdgePctAtLine,
  modelProbOver,
  num,
  pickBetSide,
  pnlForResult,
} from "../projection-tracker/ev-math.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const AUDIT = join(WEB, "data", "dk_round_projection_audit.csv");
const COURSE_TABLE = join(WEB, "data", "course_table.csv");
const HIST_JSON = join(WEB, "player_round_history.json");
const OUT = join(WEB, "data", "edge_signal_scan.json");

const SPECS = [
  { market: "Fairways hit", bookCol: "fairways_book_line", modelCol: "fairways_line", overOdds: "fairways_over_odds", underOdds: "fairways_under_odds", overRes: "fairways_over", underRes: "fairways_under", auditKey: "model_fairways" },
  { market: "GIR", bookCol: "gir_book_line", modelCol: "gir_line", overOdds: "gir_over_odds", underOdds: "gir_under_odds", overRes: "gir_over", underRes: "gir_under", auditKey: "model_gir" },
  { market: "Birdies", bookCol: "birdies_book_line", modelCol: "birdies_line", overOdds: "birdies_over_odds", underOdds: "birdies_under_odds", overRes: "birdies_over", underRes: "birdies_under", auditKey: "model_birdies" },
  { market: "Total score", bookCol: "round_score_book_line", modelCol: "round_score_line", overOdds: "round_score_over_odds", underOdds: "round_score_under_odds", overRes: "round_score_over", underRes: "round_score_under", auditKey: "model_total_score" },
  { market: "Bogeys", bookCol: "bogeys_book_line", modelCol: "bogeys_line", overOdds: "bogeys_over_odds", underOdds: "bogeys_under_odds", overRes: "bogeys_over", underRes: "bogeys_under", auditKey: "model_bogeys" },
];

function parseLine(v) {
  const s = String(v ?? "").trim();
  return s ? num(s, NaN) : NaN;
}

function parseCsvSimple(path) {
  const text = readFileSync(path, "utf8");
  const lines = text.split(/\r?\n/).filter(Boolean);
  const header = lines[0].split(",");
  const rows = [];
  for (let i = 1; i < lines.length; i++) {
    const cells = [];
    let cur = "";
    let q = false;
    for (const ch of lines[i]) {
      if (ch === '"') {
        q = !q;
        continue;
      }
      if (ch === "," && !q) {
        cells.push(cur);
        cur = "";
        continue;
      }
      cur += ch;
    }
    cells.push(cur);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
    rows.push(row);
  }
  return rows;
}

async function loadAuditMap() {
  const rows = [];
  await new Promise((resolve, reject) => {
    createReadStream(AUDIT)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("error", reject)
      .on("end", resolve);
  });
  const m = new Map();
  for (const r of rows) {
    const dg = Math.round(num(r.dg_id, NaN));
    const rnd = Math.round(num(r.display_round, NaN));
    const mkt = String(r.market || "").trim();
    const ev = String(r.event_name || "").trim();
    const cap = String(r.captured_at || "");
    const key = `${dg}|${ev}|${rnd}|${mkt}`;
    const prev = m.get(key);
    if (!prev || cap > prev.captured_at) m.set(key, r);
  }
  return m;
}

function loadCourseTable() {
  if (!existsSync(COURSE_TABLE)) return new Map();
  const rows = parseCsvSimple(COURSE_TABLE);
  const byKey = new Map();
  for (const r of rows) {
    const k = normCourseNameKey(r.course);
    if (k) byKey.set(k, r);
  }
  return byKey;
}

function matchCourse(courseTable, courseUsed) {
  const k = normCourseNameKey(courseUsed);
  if (courseTable.has(k)) return courseTable.get(k);
  for (const [ck, row] of courseTable) {
    if (k.includes(ck) || ck.includes(k)) return row;
  }
  return null;
}

function buildWindIndex(hist) {
  const byKey = new Map();
  for (const b of Object.values(hist.byDgId || {})) {
    for (const r of b.rounds || []) {
      const w = num(r.weather_wind_mph, NaN);
      const rain = String(r.weather_condition || r.weather_rain || "").toLowerCase();
      if (!Number.isFinite(w)) continue;
      const dg = Math.round(num(r.dg_id ?? b.dg_id, NaN));
      const rnd = Math.round(num(r.round_num ?? r.round, NaN));
      const yr = num(r.year, NaN) || parseInt(String(r.event_completed || "").split("/")[2], 10);
      const ev = String(r.event_name || "").toLowerCase().trim();
      if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;
      byKey.set(`${dg}|${yr}|${rnd}|${ev}`, {
        wind: w,
        rain: rain.includes("rain") || num(r.weather_precip_mm, 0) > 0.5,
        event_name: r.event_name,
      });
    }
  }
  return byKey;
}

function lookupWeather(byKey, dg, rnd, eventName) {
  for (const yr of [2026, 2025, 2024, 2023, 2022]) {
    const rec = byKey.get(`${dg}|${yr}|${rnd}|${String(eventName || "").toLowerCase().trim()}`);
    if (rec) return rec;
  }
  for (const [k, rec] of byKey) {
    const [dgS, , rndS] = k.split("|");
    if (Number(dgS) === dg && Number(rndS) === rnd && eventsLikelySame(eventName, rec.event_name)) return rec;
  }
  return null;
}

function roiGroup(bets, labelFn, minN = 12) {
  const g = new Map();
  for (const b of bets) {
    const lab = labelFn(b);
    if (!lab) continue;
    let a = g.get(lab);
    if (!a) a = { label: lab, n: 0, units: 0, wins: 0, losses: 0 };
    a.n++;
    a.units += b.pnl;
    if (b.result === "W") a.wins++;
    else if (b.result === "L") a.losses++;
    g.set(lab, a);
  }
  return [...g.values()]
    .filter((a) => a.n >= minN)
    .map((a) => ({
      ...a,
      roi: a.n ? (a.units / a.n) * 100 : NaN,
      hit: a.wins + a.losses ? (a.wins / (a.wins + a.losses)) * 100 : NaN,
      units: Math.round(a.units * 100) / 100,
      roi: Math.round((a.units / a.n) * 1000) / 10,
    }))
    .sort((a, b) => b.roi - a.roi);
}

function csvSignal(row, key, fb = NaN) {
  const v = row[key];
  if (v == null || String(v).trim() === "") return fb;
  if (key === "pin_sheet_active") {
    const s = String(v).trim();
    if (s === "1") return 1;
    if (s === "0") return 0;
    return fb;
  }
  if (key === "weather_condition" || key === "tee_wave") return String(v).trim().toLowerCase();
  return num(v, fb);
}

function buildBets(detail, audit, courseTable, windByKey, minEv) {
  const bets = [];
  for (const row of detail) {
    if (row.pricing_mode !== "default" || row.book_odds_source !== "pre_round_audit") continue;
    const dg = Math.round(num(row.dg_id, NaN));
    const rnd = Math.round(num(row.round, NaN));
    const ev = String(row.event_name || "").trim();
    const courseUsed = String(row.course_used || "").trim();
    const ct = matchCourse(courseTable, courseUsed);

    for (const spec of SPECS) {
      const book = parseLine(row[spec.bookCol]);
      if (!Number.isFinite(book)) continue;
      const model = parseLine(row[spec.modelCol]);
      const mu = Number.isFinite(model) ? model : NaN;
      const oO = num(row[spec.overOdds], NaN);
      const uO = num(row[spec.underOdds], NaN);
      const { edgeOver, edgeUnder } = modelEdgePctAtLine(spec.market, mu, book, oO, uO);
      const pick = pickBetSide(edgeOver, edgeUnder, minEv);
      if (!pick) continue;
      const side = pick.side;
      const res = String(side === "over" ? row[spec.overRes] : row[spec.underRes]).toUpperCase();
      if (res !== "W" && res !== "L" && res !== "P") continue;
      const auditRow = audit.get(`${dg}|${ev}|${rnd}|${spec.market}`);
      const auditAll = audit.get(`${dg}|${ev}|${rnd}|Fairways hit`) || audit.get(`${dg}|${ev}|${rnd}|GIR`);
      const mFw = audit.get(`${dg}|${ev}|${rnd}|Fairways hit`);
      const mGir = audit.get(`${dg}|${ev}|${rnd}|GIR`);
      const mBird = audit.get(`${dg}|${ev}|${rnd}|Birdies`);
      const mScore = audit.get(`${dg}|${ev}|${rnd}|Total score`) || audit.get(`${dg}|${ev}|${rnd}|Total Score`);

      const modelFw = mFw ? num(mFw.model_fairways, NaN) : NaN;
      const modelGir = mGir ? num(mGir.model_gir, NaN) : NaN;
      const modelBird = mBird ? num(mBird.model_birdies, NaN) : NaN;
      const modelScore = mScore ? num(mScore.model_total_score, NaN) : NaN;
      const dkFw = mFw ? num(mFw.dk_line, NaN) : parseLine(row.fairways_book_line);
      const dkGir = mGir ? num(mGir.dk_line, NaN) : parseLine(row.gir_book_line);

      const csvWind = csvSignal(row, "weather_wind_mph");
      const csvCond = csvSignal(row, "weather_condition", "");
      const wx = Number.isFinite(csvWind)
        ? {
            wind: csvWind,
            rain:
              String(csvCond).includes("rain") ||
              String(csvCond).includes("drizzle") ||
              String(csvCond).includes("storm"),
          }
        : lookupWeather(windByKey, dg, rnd, ev);
      const csvGirFw = csvSignal(row, "gir_minus_fw");
      const csvFwWidth = csvSignal(row, "course_fw_width");
      const csvPin = csvSignal(row, "pin_sheet_active");
      const csvTee = csvSignal(row, "tee_wave", "");
      const csvSgOtt = csvSignal(row, "sg_ott");
      const csvSgApp = csvSignal(row, "sg_app");
      const csvWxDiff = csvSignal(row, "weather_difficulty");

      bets.push({
        market: spec.market,
        event: ev,
        course: courseUsed,
        round: rnd,
        side,
        edge: pick.edge,
        result: res,
        pnl: pnlForResult(res, side === "over" ? oO : uO),
        modelBookDelta: Number.isFinite(mu) ? mu - book : NaN,
        modelFw,
        modelGir,
        modelBird,
        modelScore,
        girMinusFw:
          Number.isFinite(csvGirFw)
            ? csvGirFw
            : Number.isFinite(modelGir) && Number.isFinite(modelFw)
              ? modelGir - modelFw
              : NaN,
        girFwSkillGap: Number.isFinite(modelGir) && Number.isFinite(dkGir) && Number.isFinite(modelFw) && Number.isFinite(dkFw)
          ? (modelGir - dkGir) - (modelFw - dkFw)
          : NaN,
        wind: wx?.wind ?? NaN,
        rainy: wx?.rain ?? null,
        weatherDifficulty: csvWxDiff,
        sgOtt: csvSgOtt,
        sgApp: csvSgApp,
        teeWave: csvTee,
        pinSheetActive: csvPin,
        fwWidth: Number.isFinite(csvFwWidth) ? csvFwWidth : ct ? num(ct.fw_width, NaN) : NaN,
        courseYardage: ct ? num(ct.yardage, NaN) : NaN,
        courseAdjAcc: ct ? num(ct.adj_driving_accuracy, NaN) : NaN,
        courseAdjGir: ct ? num(ct.adj_gir, NaN) : NaN,
        courseOttSg: ct ? num(ct.ott_sg, NaN) : NaN,
        courseAppSg: ct ? num(ct.app_sg, NaN) : NaN,
        courseScoreAdj: ct ? num(ct.adj_score_to_par, NaN) : NaN,
        courseMissFwPen: ct ? num(ct.miss_fw_pen_frac, NaN) : NaN,
      });
    }
  }
  return bets;
}

function scanSignals(bets, marketFilter) {
  const b = marketFilter ? bets.filter((x) => x.market === marketFilter) : bets;
  return {
    market: marketFilter || "all",
    n: b.length,
    overallRoi: b.length ? Math.round((b.reduce((s, x) => s + x.pnl, 0) / b.length) * 1000) / 10 : NaN,
    byWind: roiGroup(b, (x) => {
      if (!Number.isFinite(x.wind)) return null;
      if (x.wind <= 8) return "Calm wind (≤8 mph)";
      if (x.wind <= 12) return "Moderate (8–12 mph)";
      if (x.wind <= 18) return "Windy (12–18 mph)";
      return "Very windy (>18 mph)";
    }),
    byRain: roiGroup(b, (x) => (x.rainy === true ? "Rain / soft" : x.rainy === false ? "Dry" : null), 8),
    byTeeWave: roiGroup(b, (x) => {
      if (x.teeWave === "morning") return "Morning wave";
      if (x.teeWave === "afternoon") return "Afternoon wave";
      return null;
    }, 8),
    byPinSheet: roiGroup(b, (x) => {
      if (x.pinSheetActive === 1) return "Pin sheet active";
      if (x.pinSheetActive === 0) return "No pin sheet";
      return null;
    }, 8),
    bySgOtt: roiGroup(b, (x) => {
      if (!Number.isFinite(x.sgOtt)) return null;
      if (x.sgOtt >= 0.35) return "Strong OTT (≥0.35)";
      if (x.sgOtt <= -0.15) return "Weak OTT (≤−0.15)";
      return "Average OTT";
    }, 12),
    byRound: roiGroup(b, (x) => (x.round === 1 ? "R1" : x.round === 4 ? "R4" : `R${x.round}`)),
    byCourseFwWidth: roiGroup(b, (x) => {
      if (!Number.isFinite(x.fwWidth)) return null;
      if (x.fwWidth < 30) return "Narrow FW (<30 yd avg)";
      if (x.fwWidth < 34) return "Medium FW (30–34 yd)";
      return "Wide FW (≥34 yd)";
    }),
    byCourseDrivingDifficulty: roiGroup(b, (x) => {
      if (!Number.isFinite(x.courseAdjAcc)) return null;
      if (x.courseAdjAcc < 0.55) return "Hard driving course (adj acc <55%)";
      if (x.courseAdjAcc < 0.62) return "Average driving course";
      return "Easy driving course (adj acc ≥62%)";
    }),
    byCourseScoring: roiGroup(b, (x) => {
      if (!Number.isFinite(x.courseScoreAdj)) return null;
      if (x.courseScoreAdj > 1.2) return "Hard vs par (adj >+1.2)";
      if (x.courseScoreAdj < 0.5) return "Easy vs par (adj <+0.5)";
      return "Neutral scoring course";
    }),
    byGirFwSkillGap: roiGroup(
      b.filter((x) => x.market === "Fairways hit" || x.market === "GIR"),
      (x) => {
        if (!Number.isFinite(x.girFwSkillGap)) return null;
        if (x.girFwSkillGap >= 1.5) return "GIR edge >> FW edge (+1.5+)";
        if (x.girFwSkillGap <= -1.5) return "FW edge >> GIR edge";
        return "GIR/FW edges aligned";
      },
      10,
    ),
    byGirMinusFw: roiGroup(
      b.filter((x) => x.market === "Fairways hit"),
      (x) => {
        if (!Number.isFinite(x.girMinusFw)) return null;
        if (x.girMinusFw >= 3.5) return "High GIR−FW (approach > driving)";
        if (x.girMinusFw <= 2.5) return "Low GIR−FW (driving profile)";
        return "Balanced GIR−FW";
      },
      10,
    ),
    byEdgeTier: roiGroup(b, (x) => {
      if (x.edge >= 15) return "Edge ≥15%";
      if (x.edge >= 10) return "Edge 10–15%";
      return "Edge 5–10%";
    }),
    bySide: roiGroup(b, (x) => `${x.market} ${x.side}`),
    topCourses: roiGroup(b, (x) => x.course, 15),
    worstCourses: roiGroup(b, (x) => x.course, 15).slice(-8).reverse(),
  };
}

function actionableSignals(scans) {
  const tips = [];
  for (const [name, scan] of Object.entries(scans)) {
    const pools = [
      ["wind", scan.byWind],
      ["rain", scan.byRain],
      ["tee wave", scan.byTeeWave],
      ["pin sheet", scan.byPinSheet],
      ["SG OTT", scan.bySgOtt],
      ["fairway width", scan.byCourseFwWidth],
      ["driving difficulty", scan.byCourseDrivingDifficulty],
      ["scoring difficulty", scan.byCourseScoring],
      ["GIR/FW skill gap", scan.byGirFwSkillGap],
      ["edge tier", scan.byEdgeTier],
    ];
    for (const [kind, rows] of pools) {
      const best = rows?.[0];
      const worst = rows?.[rows.length - 1];
      if (best && best.roi >= 12 && best.n >= 15) {
        tips.push({ market: name, signal: `${kind}: ${best.label}`, roi: best.roi, n: best.n, action: "lean in" });
      }
      if (worst && worst.roi <= -5 && worst.n >= 15) {
        tips.push({ market: name, signal: `${kind}: ${worst.label}`, roi: worst.roi, n: worst.n, action: "fade / skip" });
      }
    }
  }
  return tips.sort((a, b) => Math.abs(b.roi) - Math.abs(a.roi));
}

async function main() {
  const detail = parseCsvSimple(VS);
  const audit = await loadAuditMap();
  const courseTable = loadCourseTable();
  let windByKey = new Map();
  let weatherNote = "No weather — run build:history to populate player_round_history.json";
  const csvWeatherRows = detail.filter((r) => Number.isFinite(num(r.weather_wind_mph, NaN))).length;
  if (csvWeatherRows > 0) {
    weatherNote = `${csvWeatherRows} detail rows with weather_wind_mph in CSV export`;
  } else if (existsSync(HIST_JSON)) {
    const hist = JSON.parse(readFileSync(HIST_JSON, "utf8"));
    if (Object.keys(hist.byDgId || {}).length) {
      windByKey = buildWindIndex(hist);
      weatherNote = `${windByKey.size} rounds with wind in history`;
    }
  }

  const bets5 = buildBets(detail, audit, courseTable, windByKey, 5);
  const bets12 = buildBets(detail, audit, courseTable, windByKey, 12.5);

  const markets = ["Fairways hit", "GIR", "Birdies", "Total score", "Bogeys"];
  const scans5 = {};
  const scans12 = {};
  for (const m of markets) {
    scans5[m] = scanSignals(bets5, m);
    scans12[m] = scanSignals(bets12, m);
  }
  scans5.all = scanSignals(bets5, null);
  scans12.all = scanSignals(bets12, null);

  const payload = {
    generated_at: new Date().toISOString(),
    weatherNote,
    courseTableMatches: bets5.filter((b) => Number.isFinite(b.fwWidth)).length,
    courseTableTotal: bets5.length,
    actionable: actionableSignals(scans12),
    minEv5: scans5,
    minEv12_5: scans12,
    hypotheses: [
      "Narrow fairway courses (low fw_width): model uses course-table adj_driving_accuracy — DK may not move FW lines enough vs course setup.",
      "Wind 12–18 mph: model applies −0.14× difficulty to FW, −0.22× to GIR — if DK is flat, windy overs on accurate drivers can be wrong side; test under FW in wind.",
      "Rain/soft: model gives negative difficulty (easier scoring, more birdies) — birdie overs may outperform in rain if books slow to adjust.",
      "GIR edge >> FW edge (girFwSkillGap): approach-strong player at FW market — book may anchor FW to name; your model splits OTT vs APP.",
      "Hard vs par courses: total score unders and bogey overs historically track; birdie overs harder.",
      "R4 vs R1: within-event form + Sunday pressure in model — R2–R4 may differ from R1 calibration.",
      "Edge tier: FW/GIR need 12.5%+ EV; birdies profitable at lower edge.",
    ],
  };

  writeFileSync(OUT, JSON.stringify(payload, null, 2));

  console.log("\n=== EDGE SIGNAL SCAN ===");
  console.log(weatherNote);
  console.log(`Course table joined: ${payload.courseTableMatches}/${payload.courseTableTotal} bets\n`);

  console.log("--- Actionable (≥12.5% EV, |ROI|≥12% or ≤-5%, n≥15) ---");
  for (const t of payload.actionable.slice(0, 20)) {
    console.log(`  [${t.action}] ${t.market}: ${t.signal} → ROI ${t.roi >= 0 ? "+" : ""}${t.roi}% (n=${t.n})`);
  }

  for (const m of ["Fairways hit", "GIR", "Birdies"]) {
    const s = scans12[m];
    console.log(`\n--- ${m} @ ≥12.5% EV (ROI ${s.overallRoi}%, n=${s.n}) ---`);
    for (const [title, rows] of [
      ["Wind", s.byWind],
      ["Fairway width", s.byCourseFwWidth],
      ["Driving course", s.byCourseDrivingDifficulty],
      ["Edge tier", s.byEdgeTier],
    ]) {
      if (!rows?.length) continue;
      console.log(`  ${title}:`);
      for (const r of rows.slice(0, 4)) {
        console.log(`    ${r.label.padEnd(32)} n=${String(r.n).padStart(3)} ROI ${r.roi >= 0 ? "+" : ""}${r.roi}%`);
      }
    }
  }
  console.log(`\nFull output: ${OUT}\n`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
