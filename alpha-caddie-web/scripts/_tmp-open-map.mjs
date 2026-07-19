import { readFileSync, createReadStream } from "fs";
import { parse } from "csv-parse";
import { parse as parseSync } from "csv-parse/sync";
import { join } from "path";

const WEB = join(import.meta.dirname, "..");
const detail = [];
for await (const r of createReadStream(join(WEB, "data/round_projection_vs_actual.csv")).pipe(
  parse({ columns: true, relax_column_count: true, skip_empty_lines: true }),
)) {
  detail.push(r);
}
const parseNum = (x) => {
  const s = String(x ?? "").trim();
  if (s === "") return null;
  const n = Number(s);
  return Number.isFinite(n) ? n : null;
};
const openMiss = detail.filter(
  (r) => r.event_name === "The Open Championship" && parseNum(r.actual_birdies) === null,
);
const rows = parseSync(readFileSync(join(WEB, "..", "data", "pga_datagolf_player_map.csv")), {
  columns: true,
  skip_empty_lines: true,
});
const byDg = new Map();
for (const r of rows) byDg.set(Math.round(Number(r.dg_id)), r);
let inMap = 0;
let outMap = 0;
const missingNames = [];
for (const r of openMiss) {
  const dg = Math.round(Number(r.dg_id));
  if (byDg.has(dg)) inMap++;
  else {
    outMap++;
    missingNames.push({ p: r.player_name, dg });
  }
}
const uniqueMissing = missingNames.filter((x, i, a) => a.findIndex((y) => y.dg === x.dg) === i);
console.log({
  openMissRows: openMiss.length,
  uniquePlayers: new Set(openMiss.map((r) => r.dg_id)).size,
  inMapRows: inMap,
  outMapRows: outMap,
  mapSize: byDg.size,
});
console.log("unmapped", uniqueMissing);
console.log(
  "mapped but still missing birdies",
  openMiss
    .filter((r) => byDg.has(Math.round(Number(r.dg_id))))
    .slice(0, 20)
    .map((r) => ({ p: r.player_name, dg: r.dg_id, map: byDg.get(Math.round(Number(r.dg_id))) })),
);
