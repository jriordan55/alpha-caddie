import { readFileSync } from "fs";

function parseCsvRow(line) {
  const cells = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
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
  return cells;
}

const csv = readFileSync("data/round_projection_vs_actual.csv", "utf8").split(/\r?\n/);
const hdr = parseCsvRow(csv[0]);
const idx = Object.fromEntries(hdr.map((h, i) => [h, i]));
const by = {};
for (const line of csv.slice(1)) {
  if (!line) continue;
  const c = parseCsvRow(line);
  if ((c[idx.event_name] || "") !== "Wyndham Championship") continue;
  const rnd = String(c[idx.round]);
  if (!by[rnd]) by[rnd] = { rows: 0, withScore: 0 };
  by[rnd].rows++;
  if (String(c[idx.actual_round_score] ?? "").trim() !== "") by[rnd].withScore++;
}
console.log("Wyndham after push export", by);
