#!/usr/bin/env node
/**
 * Bake-off: legacy course-fit/tailor vs Plus4 COR on tracked O/U rows.
 *
 * Uses data/round_projection_vs_actual.csv player×round rows with book + actual.
 * Skill seed = rolling SG:Total before the event; score = venueBookMean − (μ − fieldMeanμ).
 *
 *   node scripts/compare-plus4-cor-oos.mjs
 * Writes data/plus4_cor_oos.json
 */
import { createReadStream, existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { normCourseNameKey } from "./course-name-key.mjs";
import { applyVenueCourseFitToMu } from "./course-round-adjustments.mjs";
import {
  applyCourseTailoringShiftsToRow,
  computeCourseTailoringShifts,
  fieldSgMedians,
  loadPlayerHistoryRoundsByDg,
  mergeSgImportance,
} from "./course-skill-tailoring.mjs";
import { resolveCourseTableForVenue } from "./course-adaptive-pricing.mjs";
import {
  applyPlus4CorToPlayers,
  buildPlayerCourseResiduals,
  ensureCourseCorMatrix,
  num,
} from "./plus4-cor-score.mjs";
import { makePlus4ExtrasApplicator } from "./plus4-extra-signals.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const HIST = join(WEB, "player_round_history.json");
const OUT = join(WEB, "data", "plus4_cor_oos.json");

function numStrict(x) {
  if (x === undefined || x === null) return NaN;
  const s = String(x).trim();
  if (!s) return NaN;
  const n = Number(s);
  return Number.isFinite(n) ? n : NaN;
}

function parseEventMs(row) {
  const raw = String(row.projections_updated_at || row.exported_at || "").trim();
  const t = Date.parse(raw);
  return Number.isFinite(t) ? t : Date.now();
}

function skillSeedBefore(rounds, asofMs) {
  const prior = (rounds || []).filter((r) => {
    const t = Date.parse(String(r.play_date || r.event_completed || r.date || ""));
    return Number.isFinite(t) && t < asofMs && Number.isFinite(num(r.sg_total, NaN));
  });
  prior.sort((a, b) => {
    const ta = Date.parse(String(a.play_date || a.event_completed || "")) || 0;
    const tb = Date.parse(String(b.play_date || b.event_completed || "")) || 0;
    return tb - ta;
  });
  const take = prior.slice(0, 24);
  if (!take.length) return 0;
  let w = 0;
  let s = 0;
  take.forEach((r, i) => {
    const ww = 0.92 ** i;
    s += ww * num(r.sg_total, 0);
    w += ww;
  });
  return w > 0 ? s / w : 0;
}

function mae(pairs) {
  if (!pairs.length) return null;
  return pairs.reduce((a, b) => a + b, 0) / pairs.length;
}

async function loadRows() {
  if (!existsSync(VS)) throw new Error(`missing ${VS}`);
  const rows = [];
  const parser = createReadStream(VS).pipe(
    parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
  );
  for await (const row of parser) {
    const actual = numStrict(row.actual_round_score);
    const book = numStrict(row.round_score_book_line);
    const model = numStrict(row.round_score_line);
    if (!Number.isFinite(actual) || !Number.isFinite(book)) continue;
    const dg = Math.round(numStrict(row.dg_id));
    if (!Number.isFinite(dg)) continue;
    rows.push({
      event: String(row.event_name || "").trim(),
      course: String(row.course_used || "").trim(),
      courseKey: normCourseNameKey(row.course_used || ""),
      round: Math.round(numStrict(row.round) || numStrict(row.display_round) || 1),
      dg_id: dg,
      player_name: row.player_name,
      actual,
      book,
      model: Number.isFinite(model) ? model : NaN,
      asofMs: parseEventMs(row),
    });
  }
  return rows;
}

function scoreFromMu(mu, fieldMeanMu, venueMeanScore) {
  // STP ≈ venueStp − 1*(μ − fieldMean) ; total = venueMeanScore − (μ − fieldMean)
  return venueMeanScore - (mu - fieldMeanMu);
}

async function main() {
  console.log("[compare:plus4-cor] loading vs-actual + history…");
  const detail = await loadRows();
  console.log(`[compare:plus4-cor] ${detail.length} graded book+actual rows`);

  ensureCourseCorMatrix({ historyPath: HIST, force: false });
  const histJson = JSON.parse(readFileSync(HIST, "utf8"));
  const dgIds = [...new Set(detail.map((r) => r.dg_id))];
  const historyByDg = loadPlayerHistoryRoundsByDg(HIST, dgIds);

  // Group by event|course
  const groups = new Map();
  for (const r of detail) {
    const k = `${r.event}\0${r.courseKey}`;
    if (!groups.has(k)) groups.set(k, []);
    groups.get(k).push(r);
  }

  const legacyBook = [];
  const legacyAct = [];
  const plus4Book = [];
  const plus4Act = [];
  const publishedBook = [];
  const publishedAct = [];
  const byEvent = [];

  for (const [, rows] of groups) {
    const venueKey = rows[0].courseKey;
    const event = rows[0].event;
    const asofMs = Math.min(...rows.map((r) => r.asofMs));
    const venueMeanBook =
      rows.reduce((s, r) => s + r.book, 0) / rows.length;

    // Unique players in this event/course (any round) — use first row per dg
    const byDg = new Map();
    for (const r of rows) {
      if (!byDg.has(r.dg_id)) byDg.set(r.dg_id, r);
    }

    const seeds = [];
    for (const r of byDg.values()) {
      const rounds = historyByDg.get(r.dg_id) || [];
      const sg = skillSeedBefore(rounds, asofMs);
      seeds.push({
        dg_id: r.dg_id,
        player_name: r.player_name,
        mu_sg: sg,
        sg_total: sg,
        sg_ott: num(rounds[0]?.sg_ott, 0),
        sg_app: num(rounds[0]?.sg_app, 0),
        sg_arg: num(rounds[0]?.sg_arg, 0),
        sg_putt: num(rounds[0]?.sg_putt, 0),
        driving_distance_rating: NaN,
      });
    }

    // Legacy copy
    const legacy = seeds.map((s) => ({ ...s }));
    const fieldMean0 = legacy.reduce((s, r) => s + r.mu_sg, 0) / Math.max(1, legacy.length);
    // Venue course-fit hist from player rounds at this course before asof
    const courseFitByDg = new Map();
    for (const r of byDg.values()) {
      const rounds = historyByDg.get(r.dg_id) || [];
      const sea = rounds.filter((x) => {
        const t = Date.parse(String(x.play_date || x.event_completed || ""));
        if (!Number.isFinite(t) || t >= asofMs) return false;
        return normCourseNameKey(x.course_name || x.course || "") === venueKey;
      });
      const sgs = sea.map((x) => num(x.sg_total, NaN)).filter(Number.isFinite);
      if (sgs.length >= 5) {
        courseFitByDg.set(r.dg_id, {
          avgSg: sgs.reduce((a, b) => a + b, 0) / sgs.length,
          n: sgs.length,
        });
      }
    }
    const venueScoring = { courseFitByDg, venueAvgBirdies: 3.5 };
    for (const row of legacy) {
      row.mu_sg = applyVenueCourseFitToMu(row.mu_sg, row.dg_id, venueScoring, fieldMean0);
      row.implied_mu_sg = row.mu_sg;
    }
    const ctRow = resolveCourseTableForVenue(rows[0].course);
    const sgImportance = mergeSgImportance(null, ctRow);
    const medians = fieldSgMedians(legacy);
    for (const row of legacy) {
      const shifts = computeCourseTailoringShifts({
        row,
        rounds: historyByDg.get(row.dg_id) || [],
        sgImportance,
        fieldMedians: medians,
        venueScoring,
        ctRow,
        teeWaveShift: 0,
      });
      applyCourseTailoringShiftsToRow(row, shifts, 72);
    }
    const legacyMu = new Map(legacy.map((r) => [r.dg_id, r.mu_sg]));
    const legacyField = legacy.reduce((s, r) => s + r.mu_sg, 0) / Math.max(1, legacy.length);

    // Plus4 copy
    const plus = seeds.map((s) => ({ ...s }));
    applyPlus4CorToPlayers(plus, {
      venueKey,
      courseUsed: rows[0].course,
      historyPath: HIST,
      asofMs,
      residualBundle: buildPlayerCourseResiduals(histJson, { asofMs }),
      applyExtras: makePlus4ExtrasApplicator(venueKey),
    });
    const plusMu = new Map(plus.map((r) => [r.dg_id, r.mu_sg]));
    const plusField = plus.reduce((s, r) => s + r.mu_sg, 0) / Math.max(1, plus.length);

    let eLegBook = 0;
    let eLegAct = 0;
    let eP4Book = 0;
    let eP4Act = 0;
    let ePubBook = 0;
    let ePubAct = 0;
    let n = 0;
    let nPub = 0;

    for (const r of rows) {
      const lMu = legacyMu.get(r.dg_id);
      const pMu = plusMu.get(r.dg_id);
      if (!Number.isFinite(lMu) || !Number.isFinite(pMu)) continue;
      const lScore = scoreFromMu(lMu, legacyField, venueMeanBook);
      const pScore = scoreFromMu(pMu, plusField, venueMeanBook);
      const lb = Math.abs(lScore - r.book);
      const la = Math.abs(lScore - r.actual);
      const pb = Math.abs(pScore - r.book);
      const pa = Math.abs(pScore - r.actual);
      legacyBook.push(lb);
      legacyAct.push(la);
      plus4Book.push(pb);
      plus4Act.push(pa);
      eLegBook += lb;
      eLegAct += la;
      eP4Book += pb;
      eP4Act += pa;
      n++;
      if (Number.isFinite(r.model)) {
        publishedBook.push(Math.abs(r.model - r.book));
        publishedAct.push(Math.abs(r.model - r.actual));
        ePubBook += Math.abs(r.model - r.book);
        ePubAct += Math.abs(r.model - r.actual);
        nPub++;
      }
    }
    if (n > 0) {
      byEvent.push({
        event,
        course: venueKey,
        n,
        legacy_mae_book: +(eLegBook / n).toFixed(3),
        legacy_mae_actual: +(eLegAct / n).toFixed(3),
        plus4_mae_book: +(eP4Book / n).toFixed(3),
        plus4_mae_actual: +(eP4Act / n).toFixed(3),
        published_mae_book: nPub ? +(ePubBook / nPub).toFixed(3) : null,
        published_mae_actual: nPub ? +(ePubAct / nPub).toFixed(3) : null,
      });
    }
  }

  const out = {
    generated_at: new Date().toISOString(),
    source: "data/round_projection_vs_actual.csv",
    method:
      "skill seed → legacy(venue fit+tailor) vs plus4_cor(+extras); score = mean(book) − (μ − fieldMeanμ)",
    n_rows: detail.length,
    n_events: byEvent.length,
    overall: {
      legacy: {
        mae_vs_book: mae(legacyBook) != null ? +mae(legacyBook).toFixed(3) : null,
        mae_vs_actual: mae(legacyAct) != null ? +mae(legacyAct).toFixed(3) : null,
        n: legacyBook.length,
      },
      plus4_cor: {
        mae_vs_book: mae(plus4Book) != null ? +mae(plus4Book).toFixed(3) : null,
        mae_vs_actual: mae(plus4Act) != null ? +mae(plus4Act).toFixed(3) : null,
        n: plus4Book.length,
      },
      published_model_line: {
        mae_vs_book: mae(publishedBook) != null ? +mae(publishedBook).toFixed(3) : null,
        mae_vs_actual: mae(publishedAct) != null ? +mae(publishedAct).toFixed(3) : null,
        n: publishedBook.length,
      },
    },
    by_event: byEvent.sort((a, b) => a.event.localeCompare(b.event)),
    default_recipe: "plus4_cor",
    note: "GOLF_PROJECTION_COURSE_FIT=plus4_cor|legacy",
  };

  writeFileSync(OUT, JSON.stringify(out, null, 2));
  console.log("[compare:plus4-cor] overall", JSON.stringify(out.overall, null, 2));
  console.log(`[compare:plus4-cor] wrote ${OUT}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
