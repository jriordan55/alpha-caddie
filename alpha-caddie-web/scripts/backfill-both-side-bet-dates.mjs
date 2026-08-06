#!/usr/bin/env node
/**
 * One-shot: stamp date/ts onto data/both_side_bets.json from the vs-actual CSV
 * (event + round → earliest projections_updated_at / exported_at).
 */
import { readFileSync, writeFileSync } from "fs";
import { Readable } from "stream";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { alignDetailCsvContent } from "./projection-context-signals.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const BETS = join(WEB, "data", "both_side_bets.json");

const raw = readFileSync(VS, "utf8");
const headerLine = `${raw.split(/\r?\n/).filter(Boolean)[0]}\n`;
const aligned = alignDetailCsvContent(raw, headerLine);
/** @type {Map<string, number>} */
const keyTs = new Map();

await new Promise((resolve, reject) => {
  Readable.from([aligned])
    .pipe(
      parse({
        columns: true,
        relax_quotes: true,
        relax_column_count: true,
        skip_records_with_error: true,
      }),
    )
    .on("data", (row) => {
      if (String(row.pricing_mode || "") !== "default") return;
      if (String(row.pricing_skill || "") !== "default") return;
      const event = String(row.event_name || "").trim();
      const round = Math.round(Number(row.round));
      if (!event || !Number.isFinite(round)) return;
      const t =
        Date.parse(String(row.projections_updated_at || "")) ||
        Date.parse(String(row.exported_at || ""));
      if (!Number.isFinite(t)) return;
      const key = `${event}|${round}`;
      const prev = keyTs.get(key);
      if (prev == null || t < prev) keyTs.set(key, t);
    })
    .on("end", resolve)
    .on("error", reject);
});

const j = JSON.parse(readFileSync(BETS, "utf8"));
let hit = 0;
let miss = 0;
for (const b of j.bets) {
  const t = keyTs.get(`${b.event}|${Math.round(Number(b.round))}`);
  if (t) {
    b.ts = t;
    b.date = new Date(t).toISOString().slice(0, 10);
    hit++;
  } else {
    b.ts = null;
    b.date = "";
    miss++;
  }
}
j.bets.sort(
  (a, b) =>
    (a.ts || 0) - (b.ts || 0) ||
    (a.round || 0) - (b.round || 0) ||
    String(a.market || "").localeCompare(String(b.market || "")),
);
writeFileSync(BETS, `${JSON.stringify(j, null, 2)}\n`);
console.log(
  JSON.stringify(
    {
      keys: keyTs.size,
      hit,
      miss,
      sample: j.bets.slice(0, 3).map((b) => ({
        event: b.event,
        round: b.round,
        date: b.date,
      })),
    },
    null,
    2,
  ),
);
