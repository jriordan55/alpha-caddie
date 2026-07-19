import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { join } from "path";
import { defaultDkAuditPath } from "./dk-pre-round-props.mjs";

const WEB = join(import.meta.dirname, "..");
const REPO = join(WEB, "..");
const histPath = join(REPO, "data", "historical_rounds_all.csv");
const auditPath = defaultDkAuditPath(WEB);

const targets = [
  { dg: 17365, ev: "Charles Schwab Challenge", name: "Mitchell" },
  { dg: 15470, ev: "THE CJ CUP Byron Nelson", name: "Hossler" },
  { dg: 9771, ev: "U.S. Open", name: "Day" },
  { dg: 17576, ev: "RBC Canadian Open", name: "Conners" },
];

for (const t of targets) {
  const byYear = {};
  for await (const row of createReadStream(histPath).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  )) {
    if (Math.round(Number(row.dg_id)) !== t.dg) continue;
    if (String(row.event_name || "").trim() !== t.ev) continue;
    const yr = String(row.year);
    const rnd = Math.round(Number(row.round_num));
    byYear[yr] = byYear[yr] || {};
    byYear[yr][rnd] = {
      score: row.round_score,
      bird: row.birdies,
      bog: row.bogies || row.bogeys,
      completed: row.event_completed,
    };
  }
  console.log(t.name, t.ev, byYear);
}

// What round field does audit use?
let headerPrinted = false;
const schwabRounds = new Set();
for await (const row of createReadStream(auditPath).pipe(
  parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
)) {
  if (!headerPrinted) {
    console.log(
      "audit cols with round",
      Object.keys(row).filter((k) => /round/i.test(k)),
    );
    headerPrinted = true;
  }
  if (String(row.event_name || "").trim() !== "Charles Schwab Challenge") continue;
  if (Math.round(Number(row.dg_id)) !== 17365) continue;
  schwabRounds.add(
    `display=${row.display_round}|round_num=${row.round_num}|round=${row.round}`,
  );
}
console.log("mitchell schwab round fields", [...schwabRounds]);
