#!/usr/bin/env node
/**
 * One-off: which pin locations / days had the biggest modeled + actual scoring impact.
 * Joins pin_locations DB ↔ hole_data.csv (hole scores) ↔ historical_rounds_all.csv (round totals).
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import path from "path";
import readline from "readline";
import { fileURLToPath } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { holePinDifficulty, roundAdjustmentsFromPinSheet } from "./pin-sheet-difficulty.mjs";
import { defaultPinLocationsRoot } from "./pin-locations-db.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const MODEL_ROOT = path.resolve(__dirname, "..", "..");
const PIN_ROOT = defaultPinLocationsRoot();
const HOLES_CSV = path.join(MODEL_ROOT, "data", "hole_data.csv");
const ROUNDS_CSV = path.join(MODEL_ROOT, "data", "historical_rounds_all.csv");

function normEvent(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (c === '"') {
      q = !q;
      continue;
    }
    if (c === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += c;
  }
  out.push(cur);
  return out;
}

function mdyToIso(mdy) {
  const s = String(mdy || "").trim();
  const m = s.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  if (!m) return "";
  return `${m[3]}-${String(m[2]).padStart(2, "0")}-${String(m[1]).padStart(2, "0")}`;
}

function loadPinSheets() {
  const idxPath = path.join(PIN_ROOT, "index.json");
  const idx = JSON.parse(readFileSync(idxPath, "utf8"));
  const sheets = [];
  for (const ent of idx.entries || []) {
    const p = path.join(PIN_ROOT, ent.path);
    if (!existsSync(p)) continue;
    let raw;
    try {
      raw = JSON.parse(readFileSync(p, "utf8"));
    } catch {
      continue;
    }
    const holes = Array.isArray(raw.holes) ? raw.holes : [];
    if (holes.length < 9) continue;
    const adj = roundAdjustmentsFromPinSheet(holes);
    const perHole = holes.map((h) => {
      const d = holePinDifficulty(h);
      return {
        hole: d.hole,
        pinScore: d.score,
        front: h.pin_from_front_yds,
        side: h.pin_from_side_yds,
        depth: h.green_depth_yds,
        hazard: Boolean(h.near_hazard),
      };
    });
    sheets.push({
      key: ent.key,
      course: ent.course_name || raw.course_name,
      courseKey: ent.course_key || normCourseNameKey(ent.course_name),
      playDate: ent.play_date,
      round: ent.round_num,
      event: ent.event_name_ref || raw.event_name_ref || "",
      adj,
      perHole,
      eventNorm: normEvent(ent.event_name_ref || raw.event_name_ref || ""),
    });
  }
  return sheets;
}

/** event|round -> { hole -> { sum, n } } */
async function loadHoleScoringByEventRound() {
  const map = new Map();
  if (!existsSync(HOLES_CSV)) return map;
  const rl = readline.createInterface({ input: createReadStream(HOLES_CSV), crlfDelay: Infinity });
  let headers = null;
  let iEv = -1;
  let iRd = -1;
  let iHole = -1;
  let iPar = -1;
  let iScore = -1;
  for await (const line of rl) {
    if (!headers) {
      headers = parseCsvLine(line);
      iEv = headers.indexOf("tournament_name");
      iRd = headers.indexOf("round");
      iHole = headers.indexOf("hole");
      iPar = headers.indexOf("par");
      iScore = headers.indexOf("score");
      continue;
    }
    const cols = parseCsvLine(line);
    const ev = cols[iEv];
    const rd = Math.round(Number(cols[iRd]));
    const hole = Math.round(Number(cols[iHole]));
    const par = Number(cols[iPar]);
    const score = Number(cols[iScore]);
    if (!ev || !Number.isFinite(rd) || !Number.isFinite(hole) || !Number.isFinite(par) || !Number.isFinite(score)) continue;
    const k = `${normEvent(ev)}|${rd}`;
    if (!map.has(k)) map.set(k, new Map());
    const hm = map.get(k);
    if (!hm.has(hole)) hm.set(hole, { sum: 0, n: 0 });
    const rec = hm.get(hole);
    rec.sum += score - par;
    rec.n++;
  }
  return map;
}

/** normEvent|round -> { sumScore, n, event, course } */
async function loadRoundScoringByEventRound() {
  const map = new Map();
  if (!existsSync(ROUNDS_CSV)) return map;
  const rl = readline.createInterface({ input: createReadStream(ROUNDS_CSV), crlfDelay: Infinity });
  let headers = null;
  for await (const line of rl) {
    if (!headers) {
      headers = parseCsvLine(line);
      continue;
    }
    const cols = parseCsvLine(line);
    const row = Object.fromEntries(headers.map((h, i) => [h, cols[i]]));
    const evNorm = normEvent(row.event_name);
    const rd = Math.round(Number(row.round_num));
    const rs = Number(row.round_score);
    if (!evNorm || !Number.isFinite(rd) || !Number.isFinite(rs) || rs < 50) continue;
    const k = `${evNorm}|${rd}`;
    if (!map.has(k)) map.set(k, { sumScore: 0, n: 0, event: row.event_name, course: row.course_name });
    const rec = map.get(k);
    rec.sumScore += rs;
    rec.n++;
  }
  return map;
}

function findRoundScoring(roundMap, sheet) {
  const rd = sheet.round;
  const evNorm = sheet.eventNorm;
  if (!evNorm) return null;
  const direct = roundMap.get(`${evNorm}|${rd}`);
  if (direct) return direct;
  for (const [k, v] of roundMap) {
    const [en, r] = k.split("|");
    if (Number(r) !== rd) continue;
    if (eventsLikelySame(en, evNorm)) return v;
  }
  return null;
}

function findHoleScoring(holeMap, sheet) {
  const rd = sheet.round;
  const evNorm = sheet.eventNorm;
  if (!evNorm) return null;
  const direct = holeMap.get(`${evNorm}|${rd}`);
  if (direct) return direct;
  for (const [k, v] of holeMap) {
    const [en, r] = k.split("|");
    if (Number(r) !== rd) continue;
    if (eventsLikelySame(en, evNorm) || en.includes(evNorm.slice(0, 12)) || evNorm.includes(en.slice(0, 12))) {
      return v;
    }
  }
  return null;
}

async function main() {
  const sheets = loadPinSheets();
  console.log(`Loaded ${sheets.length} pin sheets from ${PIN_ROOT}\n`);

  const [holeMap, roundMap] = await Promise.all([loadHoleScoringByEventRound(), loadRoundScoringByEventRound()]);

  const holeRows = [];
  const roundRows = [];

  for (const s of sheets) {
    const roundActual = findRoundScoring(roundMap, s);
    const fieldAvg = roundActual && roundActual.n >= 20 ? roundActual.sumScore / roundActual.n : null;

    roundRows.push({
      course: s.course,
      event: s.event,
      date: s.playDate,
      round: s.round,
      pinTotalDelta: s.adj.totalScoreDelta,
      pinExcess: s.adj.excess,
      summary: s.adj.summary,
      fieldAvg,
      fieldN: roundActual?.n ?? 0,
    });

    const holeScoring = findHoleScoring(holeMap, s);
    for (const h of s.perHole) {
      let actualVsPar = null;
      let n = 0;
      if (holeScoring) {
        const rec = holeScoring.get(h.hole);
        if (rec && rec.n >= 30) {
          actualVsPar = rec.sum / rec.n;
          n = rec.n;
        }
      }
      holeRows.push({
        course: s.course,
        event: s.event,
        date: s.playDate,
        round: s.round,
        hole: h.hole,
        pinScore: h.pinScore,
        front: h.front,
        side: h.side,
        depth: h.depth,
        hazard: h.hazard,
        actualVsPar,
        n,
        impact: Number.isFinite(actualVsPar) ? h.pinScore * actualVsPar : null,
      });
    }
  }

  const roundsWithField = roundRows.filter((r) => Number.isFinite(r.fieldAvg));
  roundsWithField.sort((a, b) => b.pinTotalDelta - a.pinTotalDelta);
  console.log("=== Hardest pin SETUPS by model (round-level total_score delta) ===\n");
  for (const r of roundsWithField.slice(0, 12)) {
    console.log(
      `${r.date} R${r.round} · ${r.event || r.course} · pin +${r.pinTotalDelta.toFixed(2)} · field avg ${r.fieldAvg.toFixed(2)} (n=${r.fieldN})`,
    );
  }

  const roundsHardestActual = [...roundsWithField].sort((a, b) => b.fieldAvg - a.fieldAvg);
  console.log("\n=== Highest actual FIELD scoring (rounds with pin sheets) ===\n");
  for (const r of roundsHardestActual.slice(0, 12)) {
    console.log(
      `${r.date} R${r.round} · ${r.event || r.course} · field avg ${r.fieldAvg.toFixed(2)} · pin model +${r.pinTotalDelta.toFixed(2)}`,
    );
  }

  const withBoth = roundRows.filter((r) => Number.isFinite(r.fieldAvg));
  const meanField = withBoth.reduce((a, r) => a + r.fieldAvg, 0) / withBoth.length;
  withBoth.forEach((r) => {
    r.fieldExcess = r.fieldAvg - meanField;
    r.pinActualAlign = r.pinTotalDelta * r.fieldExcess;
  });
  withBoth.sort((a, b) => b.pinActualAlign - a.pinActualAlign);
  console.log("\n=== Pin model × actual hardness (rounds where tough pins met high scoring) ===\n");
  for (const r of withBoth.slice(0, 10)) {
    console.log(
      `${r.date} R${r.round} · ${r.event || r.course} · pin +${r.pinTotalDelta.toFixed(2)} · field ${r.fieldAvg.toFixed(2)} (${r.fieldExcess >= 0 ? "+" : ""}${r.fieldExcess.toFixed(2)} vs pin-sheet avg)`,
    );
  }

  const holesRated = holeRows.filter((h) => Number.isFinite(h.actualVsPar) && h.pinScore >= 0.35);
  holesRated.sort((a, b) => b.actualVsPar - a.actualVsPar);
  console.log("\n=== Toughest individual HOLES in play (high pin score + high actual vs par) ===\n");
  for (const h of holesRated.slice(0, 15)) {
    console.log(
      `${h.date} R${h.round} H${h.hole} · ${h.event || h.course} · pin ${h.pinScore.toFixed(2)} · field ${h.actualVsPar >= 0 ? "+" : ""}${h.actualVsPar.toFixed(3)} vs par (n=${h.n}) · front ${h.front}y side ${h.side}y${h.hazard ? " hazard" : ""}`,
    );
  }

  const byPinScore = [...holeRows].filter((h) => h.pinScore >= 0.5).sort((a, b) => b.pinScore - a.pinScore);
  console.log("\n=== Most aggressive PIN POSITIONS by model score (any day) ===\n");
  for (const h of byPinScore.slice(0, 15)) {
    const act = Number.isFinite(h.actualVsPar)
      ? ` · actual ${h.actualVsPar >= 0 ? "+" : ""}${h.actualVsPar.toFixed(3)} vs par`
      : " · no hole_data match";
    console.log(
      `${h.date} R${h.round} H${h.hole} · ${h.event || h.course} · pin ${h.pinScore.toFixed(2)}${act} · front ${h.front}y / depth ${h.depth}y`,
    );
  }

  const withImpact = holeRows.filter((h) => Number.isFinite(h.impact));
  withImpact.sort((a, b) => b.impact - a.impact);
  console.log("\n=== Biggest pin × scoring product (model difficulty × actual vs par) ===\n");
  for (const h of withImpact.slice(0, 12)) {
    console.log(
      `${h.date} R${h.round} H${h.hole} · ${h.event || h.course} · pin ${h.pinScore.toFixed(2)} × actual ${h.actualVsPar >= 0 ? "+" : ""}${h.actualVsPar.toFixed(3)} = ${h.impact.toFixed(3)}`,
    );
  }

  console.log(`\nAnalyzed ${sheets.length} pin sheets, ${holeRows.length} hole setups, ${withBoth.length} rounds with field scoring.`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
