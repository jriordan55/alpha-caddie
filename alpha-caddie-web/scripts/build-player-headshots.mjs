/**
 * Build alpha-caddie-web/data/player_headshots.json from
 * data/pga_datagolf_player_map.csv (+ optional Betsperts profile images).
 *
 *   npm run build:headshots
 */
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");
const MAP = join(ROOT, "data", "pga_datagolf_player_map.csv");
const BETSPERTS = join(ROOT, "data", "betsperts", "players", "profiles.json");
const OUT = join(WEB, "data", "player_headshots.json");

function normName(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/,/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

const rows = parse(readFileSync(MAP, "utf8"), {
  columns: true,
  skip_empty_lines: true,
  relax_column_count: true,
  bom: true,
});

const dgToPga = {};
const nameToPga = {};
for (const r of rows) {
  const dg = String(r.dg_id || "").trim();
  const pga = String(r.pga_player_id || "").trim();
  if (!/^\d+$/.test(dg) || !/^\d+$/.test(pga)) continue;
  dgToPga[dg] = Number(pga);
  for (const nm of [r.dg_player_name, r.pga_display_name]) {
    const k = normName(nm);
    if (k) nameToPga[k] = Number(pga);
  }
}

const betspertsByName = {};
if (existsSync(BETSPERTS)) {
  try {
    const j = JSON.parse(readFileSync(BETSPERTS, "utf8"));
    for (const p of j.data || []) {
      const url = p?.ProfileImage;
      if (typeof url !== "string" || !url.startsWith("http")) continue;
      const k = normName(p.Name);
      if (k) betspertsByName[k] = url;
    }
  } catch {
    /* optional */
  }
}

mkdirSync(dirname(OUT), { recursive: true });
writeFileSync(
  OUT,
  JSON.stringify(
    {
      generatedAt: new Date().toISOString(),
      source: "data/pga_datagolf_player_map.csv + data/betsperts/players/profiles.json",
      dgToPga,
      nameToPga,
      betspertsByName,
    },
    null,
    0,
  ) + "\n",
  "utf8",
);
console.log(
  `player_headshots.json dg=${Object.keys(dgToPga).length} names=${Object.keys(nameToPga).length} betsperts=${Object.keys(betspertsByName).length}`,
);
