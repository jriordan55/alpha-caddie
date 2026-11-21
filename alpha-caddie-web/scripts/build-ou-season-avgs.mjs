#!/usr/bin/env node
/**
 * Slim season averages for Round projections Average column.
 * Avoids fetching/parsing field-{year}.json (~16MB) just to show "This season".
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const projFile = String(process.env.GOLF_PROJECTIONS_FILE || "projections.json").trim() || "projections.json";

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function writeJsonAtomic(filePath, obj) {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
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

function resolveSeasonYear() {
  const p = path.join(WEB, projFile);
  if (fs.existsSync(p)) {
    try {
      const meta = JSON.parse(fs.readFileSync(p, "utf8"))?.meta || {};
      const ds = String(meta.datagolf_field_date_start || "").match(/^(\d{4})-/);
      if (ds) return parseInt(ds[1], 10);
    } catch {
      /* ignore */
    }
  }
  return new Date().getFullYear();
}

function fieldDgIds() {
  const ids = new Set();
  for (const name of ["projections.json", "projections-pga.json", "projections-euro.json"]) {
    const fp = path.join(WEB, name);
    if (!fs.existsSync(fp)) continue;
    try {
      const j = JSON.parse(fs.readFileSync(fp, "utf8"));
      for (const pl of j?.players || []) {
        const dg = Math.round(num(pl.dg_id));
        if (Number.isFinite(dg)) ids.add(dg);
      }
    } catch {
      /* ignore */
    }
  }
  return ids;
}

function roundStat(r, key) {
  if (key === "total") return num(r.round_score ?? r.score);
  if (key === "birdies") return num(r.birdies);
  if (key === "pars") return num(r.pars);
  if (key === "bogeys") return num(r.bogeys ?? r.bogies);
  if (key === "gir") return num(r.gir ?? r.greens_hit);
  if (key === "fairways") return num(r.fairways ?? r.fairways_hit ?? r.fw_hit);
  return NaN;
}

function avgOf(rounds, key) {
  const vals = [];
  for (const r of rounds) {
    const v = roundStat(r, key);
    if (Number.isFinite(v)) vals.push(v);
  }
  if (!vals.length) return null;
  return {
    avg: vals.reduce((a, b) => a + b, 0) / vals.length,
    n: vals.length,
  };
}

const MARKETS = [
  ["Total score", "total"],
  ["Birdies", "birdies"],
  ["Pars", "pars"],
  ["Bogeys", "bogeys"],
  ["GIR", "gir"],
  ["Fairways hit", "fairways"],
];

const seasonYear = resolveSeasonYear();
const ids = fieldDgIds();
const byDgId = {};

// Prefer field bundle if present (one read), else per-player shards.
const fieldPath = path.join(WEB, "player-history", `field-${seasonYear}.json`);
let source = null;
if (fs.existsSync(fieldPath)) {
  try {
    source = JSON.parse(fs.readFileSync(fieldPath, "utf8"))?.byDgId || null;
  } catch {
    source = null;
  }
}

for (const dg of ids) {
  let rounds = [];
  if (source?.[String(dg)]?.rounds) {
    rounds = source[String(dg)].rounds;
  } else {
    const shardPath = path.join(SHARD_DIR, `${dg}.json`);
    if (!fs.existsSync(shardPath)) continue;
    try {
      const shard = JSON.parse(fs.readFileSync(shardPath, "utf8"));
      rounds = (Array.isArray(shard?.rounds) ? shard.rounds : []).filter(
        (r) => parseInt(String(r?.year || ""), 10) === seasonYear,
      );
    } catch {
      continue;
    }
  }
  rounds = rounds.filter((r) => parseInt(String(r?.year || ""), 10) === seasonYear || !r?.year);
  const markets = {};
  for (const [label, key] of MARKETS) {
    const hit = avgOf(rounds, key);
    if (hit) markets[label] = hit;
  }
  if (Object.keys(markets).length) byDgId[String(dg)] = markets;
}

const outPath = path.join(WEB, "player-history", `ou-season-avgs-${seasonYear}.json`);
writeJsonAtomic(outPath, {
  meta: {
    season_year: seasonYear,
    players: Object.keys(byDgId).length,
    updated_at: new Date().toISOString(),
  },
  byDgId,
});

console.log(
  `[build-ou-season-avgs] Wrote ou-season-avgs-${seasonYear}.json (${Object.keys(byDgId).length} players)`,
);
