#!/usr/bin/env node
/**
 * Build Betsperts strokes-gained sidecar and overlay onto projections*.json.
 *
 *   node scripts/build-betsperts-sg.mjs
 *   npm run build:betsperts-sg
 */
import { existsSync, mkdirSync, readFileSync, renameSync, unlinkSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import {
  WEB,
  ROOT,
  loadBetspertsSgByDg,
  applyBetspertsSgToPlayers,
  SG_KEYS,
} from "./betsperts-sg.mjs";

const OUT_WEB = join(WEB, "data", "betsperts_sg_by_dg.json");
const OUT_ROOT = join(ROOT, "data", "betsperts_sg_by_dg.json");
const PROJ_FILES = ["projections.json", "projections-pga.json", "projections-euro.json"];

function writeJsonAtomic(filePath, text) {
  mkdirSync(dirname(filePath), { recursive: true });
  const tmp = `${filePath}.${process.pid}.tmp`;
  writeFileSync(tmp, text, "utf8");
  try {
    renameSync(tmp, filePath);
  } catch {
    writeFileSync(filePath, text, "utf8");
    try {
      unlinkSync(tmp);
    } catch {
      /* ignore */
    }
  }
}

function writeJson(path, obj) {
  writeJsonAtomic(path, JSON.stringify(obj) + "\n");
}

const byDg = loadBetspertsSgByDg();
const players = {};
for (const [dg, row] of byDg) {
  players[String(dg)] = row;
}

const payload = {
  generatedAt: new Date().toISOString(),
  source: "data/betsperts/players/by-id + data/pga_datagolf_player_map.csv",
  n: byDg.size,
  keys: SG_KEYS,
  players,
};

writeJson(OUT_WEB, payload);
writeJson(OUT_ROOT, payload);
console.log(`[betsperts-sg] wrote ${byDg.size} players → data/betsperts_sg_by_dg.json`);

let totalPatched = 0;
for (const name of PROJ_FILES) {
  const fp = join(WEB, name);
  if (!existsSync(fp)) continue;
  let j;
  try {
    j = JSON.parse(readFileSync(fp, "utf8"));
  } catch (e) {
    console.warn(`[betsperts-sg] skip ${name}:`, e.message || e);
    continue;
  }
  if (!Array.isArray(j.players)) continue;
  const { patched } = applyBetspertsSgToPlayers(j.players, { betspertsByDg: byDg, updateMu: true });
  if (!j.meta || typeof j.meta !== "object") j.meta = {};
  j.meta.sg_source = "betsperts";
  j.meta.betsperts_sg_n = patched;
  j.meta.betsperts_sg_at = payload.generatedAt;
  writeJsonAtomic(fp, JSON.stringify(j));
  totalPatched += patched;
  console.log(`[betsperts-sg] ${name}: patched sg_* on ${patched}/${j.players.length} player-rows`);
}

console.log(`[betsperts-sg] done (projection rows touched: ${totalPatched})`);
