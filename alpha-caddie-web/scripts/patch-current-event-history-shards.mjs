#!/usr/bin/env node
/**
 * Fast path: merge pgatour_event_rounds.json + live_round_actuals into player-history shards
 * without re-scanning historical_rounds_all.csv.
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { resolveLiveRoundActualsByDg } from "./dg-live-tournament-stats.mjs";
import { foldComparableTitle } from "./dg-events-align.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const PGA_JSON = path.join(WEB, "data", "pgatour_event_rounds.json");
const LIVE_JSON = path.join(WEB, "live-in-play.json");
const PROJ_JSON = path.join(WEB, "projections.json");

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function normEvt(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function mergeRow(existing, liveRec) {
  const out = { ...existing, ...liveRec, _from_pgatour: true, _from_live_tournament_stats: true };
  for (const k of [
    "gir",
    "fairways",
    "putts",
    "sg_putt",
    "sg_app",
    "sg_arg",
    "sg_ott",
    "sg_t2g",
    "sg_total",
  ]) {
    if (Number.isFinite(num(liveRec[k], NaN))) out[k] = liveRec[k];
    else if (Number.isFinite(num(existing[k], NaN))) out[k] = existing[k];
  }
  return out;
}

function enrichFromActuals(row, act) {
  if (!act || typeof act !== "object") return row;
  const out = { ...row };
  const girRaw = num(act.gir, NaN);
  if (Number.isFinite(girRaw)) out.gir = Math.round(girRaw <= 1 ? girRaw * 18 : girRaw);
  if (Number.isFinite(num(act.fairways, NaN))) out.fairways = Math.round(num(act.fairways, NaN));
  if (Number.isFinite(num(act.putts, NaN))) out.putts = Math.round(num(act.putts, NaN));
  for (const k of ["sg_putt", "sg_app", "sg_arg", "sg_ott", "sg_t2g", "sg_total"]) {
    if (Number.isFinite(num(act[k], NaN))) out[k] = act[k];
  }
  return out;
}

const pga = JSON.parse(fs.readFileSync(PGA_JSON, "utf8"));
const live = JSON.parse(fs.readFileSync(LIVE_JSON, "utf8"));
const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
const fwHoles = Math.round(num(proj?.projection_course_basis?.fairway_holes_modeled, 14)) || 14;
const roundPar = num(proj?.course_par_18, 70) || 70;
const actualsByDg = resolveLiveRoundActualsByDg(live, { roundPar, fairwayHoles: fwHoles });
const metaEvent = foldComparableTitle(pga?.meta?.event_name || proj?.event_name || "");

const byDg = new Map();
for (const r of pga.rounds || []) {
  const dg = Math.round(num(r.dg_id, NaN));
  if (!Number.isFinite(dg)) continue;
  if (!byDg.has(dg)) byDg.set(dg, []);
  const rnd = Math.round(num(r.round_num, NaN));
  let row = { ...r };
  const act = actualsByDg[String(dg)]?.[String(rnd)];
  row = enrichFromActuals(row, act);
  byDg.get(dg).push(row);
}

let patched = 0;
let roundsAdded = 0;
for (const [dg, pgaRows] of byDg) {
  const shardPath = path.join(SHARD_DIR, `${dg}.json`);
  if (!fs.existsSync(shardPath)) continue;
  const shard = JSON.parse(fs.readFileSync(shardPath, "utf8"));
  if (!Array.isArray(shard.rounds)) shard.rounds = [];
  for (const liveRec of pgaRows) {
    const wantEvt = normEvt(liveRec.event_name);
    const wantYr = liveRec.year;
    const wantRnd = liveRec.round_num;
    let hit = -1;
    for (let i = shard.rounds.length - 1; i >= 0; i--) {
      const rr = shard.rounds[i];
      if (rr.round_num !== wantRnd) continue;
      if (parseInt(String(rr.year || ""), 10) !== wantYr) continue;
      if (normEvt(rr.event_name) !== wantEvt) continue;
      hit = i;
      break;
    }
    if (hit >= 0) shard.rounds[hit] = mergeRow(shard.rounds[hit], liveRec);
    else {
      shard.rounds.push(liveRec);
      roundsAdded += 1;
    }
  }
  shard.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  fs.writeFileSync(shardPath, JSON.stringify(shard));
  patched += 1;
}

console.log(
  `[patch-current-event] Updated ${patched} shard(s), +${roundsAdded} round row(s); event="${metaEvent}"`,
);
