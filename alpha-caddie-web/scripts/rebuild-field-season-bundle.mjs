#!/usr/bin/env node
/**
 * Rebuild player-history/field-{year}.json from per-player shards (fast push:live path).
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const PROJ_JSON = path.join(WEB, "projections.json");

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function writeJsonAtomic(filePath, obj) {
  const dir = path.dirname(filePath);
  fs.mkdirSync(dir, { recursive: true });
  const tmp = `${filePath}.tmp`;
  fs.writeFileSync(tmp, JSON.stringify(obj));
  try {
    fs.renameSync(tmp, filePath);
  } catch {
    fs.writeFileSync(filePath, JSON.stringify(obj));
    try {
      fs.unlinkSync(tmp);
    } catch {
      /* ignore */
    }
  }
}

function resolveFieldSeasonYearFromProjections() {
  if (!fs.existsSync(PROJ_JSON)) return new Date().getFullYear();
  try {
    const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
    const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : {};
    const ds = String(meta.datagolf_field_date_start || "").match(/^(\d{4})-/);
    if (ds) {
      const y = parseInt(ds[1], 10);
      if (Number.isFinite(y) && y >= 1990 && y <= 2100) return y;
    }
  } catch {
    /* ignore */
  }
  return new Date().getFullYear();
}

function fieldDgIds(proj) {
  const ids = new Set();
  for (const p of proj?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(dg)) ids.add(dg);
  }
  return ids;
}

const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
const seasonYear = resolveFieldSeasonYearFromProjections();
const ids = fieldDgIds(proj);
const byDgId = {};
let roundCount = 0;

for (const dg of ids) {
  const shardPath = path.join(SHARD_DIR, `${dg}.json`);
  if (!fs.existsSync(shardPath)) continue;
  let shard;
  try {
    shard = JSON.parse(fs.readFileSync(shardPath, "utf8"));
  } catch {
    continue;
  }
  const rounds = (Array.isArray(shard?.rounds) ? shard.rounds : []).filter(
    (r) => parseInt(String(r?.year || ""), 10) === seasonYear,
  );
  if (!rounds.length) continue;
  roundCount += rounds.length;
  byDgId[String(dg)] = {
    dg_id: dg,
    player_name: String(shard?.player_name || "").trim(),
    rounds,
    _propsSeasonSlice: seasonYear,
    _propsSeasonEmpty: false,
  };
}

const outPath = path.join(WEB, "player-history", `field-${seasonYear}.json`);
writeJsonAtomic(outPath, {
  meta: {
    season_year: seasonYear,
    players: Object.keys(byDgId).length,
    rounds: roundCount,
    updated_at: new Date().toISOString(),
  },
  byDgId,
  holesByPlayerKey: {},
});

console.log(
  `[rebuild-field-season] Wrote field-${seasonYear}.json (${Object.keys(byDgId).length} players, ${roundCount} rounds)`,
);
