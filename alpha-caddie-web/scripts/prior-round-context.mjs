/**
 * Prior in-event round lookup from historical_rounds_all.csv.
 */
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { extractPrevRoundSgFromHist, num } from "./projection-context-signals.mjs";

export function yearFromEventCompleted(s) {
  const m = String(s || "").match(/(\d{4})/);
  return m ? Number(m[1]) : NaN;
}

/** Match export-round-projection-vs-actual-csv actualsKey. */
export function actualsKey(eventName, eventYear, dg, rnd) {
  const yr = Math.round(num(eventYear, NaN));
  const yPart = Number.isFinite(yr) ? `${yr}\x1f` : "";
  return `${String(eventName).trim()}\x1f${yPart}${Math.round(num(dg, NaN))}|${Math.round(num(rnd, NaN))}`;
}

/** @returns {Promise<Map<string, object>>} */
export async function loadHistByKey(histPath) {
  /** @type {Map<string, object>} */
  const histByKey = new Map();
  await new Promise((resolve, reject) => {
    createReadStream(histPath)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => {
        const ev = String(row.event_name || "").trim();
        const dg = Math.round(num(row.dg_id, NaN));
        const rnd = Math.round(num(row.round_num, NaN));
        const score = num(row.round_score, NaN);
        if (!ev || !Number.isFinite(dg) || !Number.isFinite(rnd) || !Number.isFinite(score)) return;
        const yr = Math.round(num(row.year, NaN)) || yearFromEventCompleted(row.event_completed);
        if (!Number.isFinite(yr)) return;
        histByKey.set(actualsKey(ev, yr, dg, rnd), row);
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return histByKey;
}

function pickPriorCell(csvVal, histVal) {
  const raw = csvVal == null ? "" : String(csvVal).trim();
  if (raw !== "") {
    const c = num(raw, NaN);
    if (Number.isFinite(c)) return c;
  }
  return num(histVal, NaN);
}

export function priorContextForBetRow(histByKey, row) {
  const ev = String(row.event_name || row.event || "").trim();
  const dg = Math.round(num(row.dg_id, NaN));
  const rnd = Math.round(num(row.round, NaN));
  const yr =
    Math.round(num(row.year, NaN)) ||
    yearFromEventCompleted(row.event_completed) ||
    yearFromEventCompleted(row.projections_updated_at);
  const fromHist = extractPrevRoundSgFromHist(histByKey, actualsKey, ev, yr, dg, rnd);
  return {
    prev_sg_ott: pickPriorCell(row.prev_sg_ott, fromHist.prev_sg_ott),
    prev_sg_app: pickPriorCell(row.prev_sg_app, fromHist.prev_sg_app),
    prev_sg_putt: pickPriorCell(row.prev_sg_putt, fromHist.prev_sg_putt),
    prev_gir_pct: pickPriorCell(row.prev_gir_pct, fromHist.prev_gir_pct),
    prev_bob_pct: pickPriorCell(row.prev_bob_pct, fromHist.prev_bob_pct),
    prev_fairway_pct: pickPriorCell(row.prev_fairway_pct, fromHist.prev_fairway_pct),
  };
}
