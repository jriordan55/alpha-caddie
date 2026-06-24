/**
 * Walk-forward MAE for counting markets vs historical actuals (OOS events).
 */
import { createReadStream, existsSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { buildFullModelMuMapForEvent } from "./historical-walkforward-projections.mjs";
import { foldComparableTitle } from "./dg-events-align.mjs";
import { birdiesPlusEaglesFromRow, num } from "./round-projection-mu.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = join(WEB, "..");
const HIST = join(WEB, "data", "historical_rounds_all.csv");

const OOS_EVENTS = [
  "THE CJ CUP Byron Nelson",
  "Charles Schwab Challenge",
  "the Memorial Tournament presented by Workday",
  "RBC Canadian Open",
  "U.S. Open",
];

async function loadHistRows() {
  const rows = [];
  if (!existsSync(HIST)) return rows;
  await new Promise((resolve, reject) => {
    createReadStream(HIST)
      .pipe(parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (row) => rows.push(row))
      .on("end", resolve)
      .on("error", reject);
  });
  return rows;
}

function eventRows(histRows, eventName, year) {
  const fold = foldComparableTitle(eventName);
  return histRows.filter((r) => {
    if (foldComparableTitle(r.event_name) !== fold) return false;
    const yr = Math.round(num(r.year, NaN));
    return !Number.isFinite(year) || yr === year;
  });
}

function actualBogeys(row) {
  return num(row.bogeys ?? row.bogies, NaN);
}

async function main() {
  const histRows = await loadHistRows();
  const err = { birdies: [], bogeys: [], pars: [], gir: [] };

  for (const ev of OOS_EVENTS) {
    const evRows = histRows.filter((r) => foldComparableTitle(r.event_name) === foldComparableTitle(ev));
    const years = [...new Set(evRows.map((r) => Math.round(num(r.year, NaN))).filter(Number.isFinite))].sort();
    const year = years[years.length - 1];
    if (!year) continue;

    const fieldDgIds = [
      ...new Set(evRows.filter((r) => Math.round(num(r.year, NaN)) === year).map((r) => Math.round(num(r.dg_id, NaN)))),
    ].filter(Number.isFinite);

    for (let rnd = 1; rnd <= 4; rnd++) {
      const wfMap = await buildFullModelMuMapForEvent({
        repoRoot: REPO,
        histRows,
        eventName: ev,
        eventYear: year,
        targetRound: rnd,
        fieldDgIds,
      });

      for (const row of evRows) {
        if (Math.round(num(row.round_num, NaN)) !== rnd) continue;
        if (Math.round(num(row.year, NaN)) !== year) continue;
        const dg = Math.round(num(row.dg_id, NaN));
        const mus = wfMap.get(dg);
        if (!mus) continue;
        const actBird = birdiesPlusEaglesFromRow(row);
        const actBog = actualBogeys(row);
        const actPar = num(row.pars, NaN);
        const actGir = num(row.gir, NaN);
        const girAct = Number.isFinite(actGir) && actGir <= 1 ? actGir * 18 : actGir;

        const pairs = [
          ["birdies", mus.get("Birdies"), actBird],
          ["bogeys", mus.get("Bogeys"), actBog],
          ["pars", mus.get("Pars"), actPar],
          ["gir", mus.get("GIR"), girAct],
        ];
        for (const [k, pred, act] of pairs) {
          if (Number.isFinite(pred) && Number.isFinite(act)) err[k].push(Math.abs(pred - act));
        }
      }
    }
    console.log(`Graded ${ev} (${year})`);
  }

  const mae = (a) => (a.length ? a.reduce((s, x) => s + x, 0) / a.length : NaN);
  console.log("\nWalk-forward MAE vs actual (OOS events):");
  for (const k of Object.keys(err)) {
    console.log(`  ${k.padEnd(8)} ${mae(err[k]).toFixed(3)}  n=${err[k].length}`);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
