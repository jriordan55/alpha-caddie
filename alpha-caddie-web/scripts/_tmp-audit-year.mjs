import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { join } from "path";
import { defaultDkAuditPath } from "./dk-pre-round-props.mjs";

const WEB = join(import.meta.dirname, "..");
const auditPath = defaultDkAuditPath(WEB);
console.log("audit", auditPath);

const hits = [];
for await (const row of createReadStream(auditPath).pipe(
  parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
)) {
  if (String(row.event_name || "").trim() !== "Charles Schwab Challenge") continue;
  if (Math.round(Number(row.dg_id)) !== 17365) continue;
  hits.push({
    rnd: row.round_num,
    captured_at: row.captured_at,
    projections_updated_at: row.projections_updated_at,
    player: row.player_name,
    market: row.market,
  });
  if (hits.length >= 12) break;
}
console.log("mitchell hits", hits.length, hits);

// Also count missing-score players' audit years
const missing = [
  { dg: 17365, ev: "Charles Schwab Challenge", rnd: 4 },
  { dg: 15470, ev: "THE CJ CUP Byron Nelson", rnd: 4 },
  { dg: 9771, ev: "U.S. Open", rnd: 1 },
];
for (const m of missing) {
  const years = new Set();
  const caps = [];
  for await (const row of createReadStream(auditPath).pipe(
    parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
  )) {
    if (String(row.event_name || "").trim() !== m.ev) continue;
    if (Math.round(Number(row.dg_id)) !== m.dg) continue;
    if (Math.round(Number(row.round_num)) !== m.rnd) continue;
    const c = String(row.captured_at || "");
    caps.push(c);
    const y = c.slice(0, 4);
    if (/^\d{4}$/.test(y)) years.add(y);
  }
  console.log(m, { years: [...years], caps: caps.slice(0, 3) });
}
