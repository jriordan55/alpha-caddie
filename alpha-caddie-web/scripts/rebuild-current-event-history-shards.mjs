#!/usr/bin/env node
/**
 * Replace (not patch) all player-history shard rows for the current projections event
 * from pgatouR scorecards + live tournament stats + optional historical CSV rows.
 *
 *   npm run rebuild:current-event-history
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";
import { parse } from "csv-parse";
import { resolveLiveRoundActualsByDg } from "./dg-live-tournament-stats.mjs";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const PGA_JSON = path.join(WEB, "data", "pgatour_event_rounds.json");
const LIVE_JSON = path.join(WEB, "live-in-play.json");
const PROJ_JSON = path.join(WEB, "projections.json");
const ROUNDS_CSV = path.join(REPO, "data", "historical_rounds_all.csv");

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

function parseUsSortKey(mdy, rnd) {
  const m = String(mdy || "").match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  if (!m) return 0;
  const y = +m[3];
  const mo = +m[1];
  const d = +m[2];
  return (y * 10000 + mo * 100 + d) * 10 + (rnd || 1);
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

function csvRowToHistoryRec(row) {
  const dg = Math.round(num(row.dg_id, NaN));
  const rnd = parseInt(String(row.round_num || "1"), 10) || 1;
  const eventDate = String(row.event_completed || "").trim();
  const yr = parseInt(String(row.year || ""), 10);
  return {
    dg_id: dg,
    player_name: String(row.player_name || "").trim(),
    sortKey: parseUsSortKey(eventDate, rnd),
    event_completed: eventDate,
    year: Number.isFinite(yr) ? yr : new Date().getFullYear(),
    event_name: String(row.event_name || "").trim(),
    event_id: String(row.event_id || ""),
    course_name: String(row.course_name || row.event_name || "").trim(),
    round_num: rnd,
    fin_text: String(row.fin_text || ""),
    round_score: num(row.round_score),
    birdies: num(row.birdies),
    pars: num(row.pars),
    bogies: num(row.bogeys ?? row.bogies),
    gir: num(row.gir),
    fairways: num(row.fairways),
    putts: num(row.putts),
    eagles_or_better: num(row.eagles_or_better),
    doubles_or_worse: num(row.doubles_or_worse),
    sg_putt: num(row.sg_putt),
    sg_app: num(row.sg_app),
    sg_arg: num(row.sg_arg),
    sg_ott: num(row.sg_ott),
    sg_t2g: num(row.sg_t2g),
    sg_total: num(row.sg_total),
    _from_dg_historical_rounds: true,
  };
}

async function loadCsvRowsForEvent(eventName, allowedDg) {
  /** @type {Map<string, object[]>} */
  const byDg = new Map();
  if (!fs.existsSync(ROUNDS_CSV)) return byDg;

  const cy = new Date().getFullYear();
  await new Promise((resolve, reject) => {
    const parser = createReadStream(ROUNDS_CSV).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    parser.on("data", (row) => {
      if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) return;
      const yr = parseInt(row.year, 10);
      if (Number.isFinite(yr) && (yr < cy - 1 || yr > cy + 1)) return;
      const dg = Math.round(num(row.dg_id, NaN));
      if (!Number.isFinite(dg) || !allowedDg.has(dg)) return;
      const rs = num(row.round_score);
      if (!Number.isFinite(rs) || rs <= 0) return;
      const rec = csvRowToHistoryRec(row);
      if (!byDg.has(dg)) byDg.set(dg, []);
      byDg.get(dg).push(rec);
    });
    parser.on("end", resolve);
    parser.on("error", reject);
  });
  return byDg;
}

function fieldDgIds(proj) {
  const ids = new Set();
  for (const p of proj?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(dg)) ids.add(dg);
  }
  return ids;
}

function roundKey(r) {
  return `${r.year}|${r.round_num}|${normEvt(r.event_name)}`;
}

const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
const eventName = String(proj?.event_name || "").trim();
if (!eventName) {
  console.error("[rebuild-current-event] No event_name in projections.json");
  process.exit(1);
}

const pga = fs.existsSync(PGA_JSON) ? JSON.parse(fs.readFileSync(PGA_JSON, "utf8")) : { rounds: [] };
const live = fs.existsSync(LIVE_JSON) ? JSON.parse(fs.readFileSync(LIVE_JSON, "utf8")) : null;
const fwHoles = Math.round(num(proj?.projection_course_basis?.fairway_holes_modeled, 14)) || 14;
const roundPar = num(proj?.course_par_18, 70) || 70;
const actualsByDg = live
  ? resolveLiveRoundActualsByDg(live, { roundPar, fairwayHoles: fwHoles })
  : {};

const allowedDg = fieldDgIds(proj);
const wantEvt = normEvt(eventName);

/** @type {Map<number, Map<string, object>>} */
const freshByDg = new Map();

for (const r of pga.rounds || []) {
  const dg = Math.round(num(r.dg_id, NaN));
  if (!Number.isFinite(dg) || !allowedDg.has(dg)) continue;
  if (normEvt(r.event_name) !== wantEvt && !eventsLikelySame(r.event_name, eventName)) continue;
  const rnd = Math.round(num(r.round_num, NaN));
  let row = {
    ...r,
    _from_pgatour: true,
    _from_live_tournament_stats: true,
  };
  row = enrichFromActuals(row, actualsByDg[String(dg)]?.[String(rnd)]);
  if (!freshByDg.has(dg)) freshByDg.set(dg, new Map());
  freshByDg.get(dg).set(roundKey(row), row);
}

const csvByDg = await loadCsvRowsForEvent(eventName, allowedDg);
for (const [dg, list] of csvByDg) {
  if (!freshByDg.has(dg)) freshByDg.set(dg, new Map());
  const m = freshByDg.get(dg);
  for (const rec of list) {
    const k = roundKey(rec);
    const prev = m.get(k);
    if (!prev) {
      m.set(k, rec);
      continue;
    }
    m.set(k, {
      ...prev,
      ...rec,
      birdies: Number.isFinite(rec.birdies) ? rec.birdies : prev.birdies,
      pars: Number.isFinite(rec.pars) ? rec.pars : prev.pars,
      bogies: Number.isFinite(rec.bogies) ? rec.bogies : prev.bogies,
      gir: Number.isFinite(rec.gir) ? rec.gir : prev.gir,
      fairways: Number.isFinite(rec.fairways) ? rec.fairways : prev.fairways,
      putts: Number.isFinite(rec.putts) ? rec.putts : prev.putts,
      sg_putt: Number.isFinite(rec.sg_putt) ? rec.sg_putt : prev.sg_putt,
      sg_app: Number.isFinite(rec.sg_app) ? rec.sg_app : prev.sg_app,
      sg_arg: Number.isFinite(rec.sg_arg) ? rec.sg_arg : prev.sg_arg,
      sg_ott: Number.isFinite(rec.sg_ott) ? rec.sg_ott : prev.sg_ott,
      sg_t2g: Number.isFinite(rec.sg_t2g) ? rec.sg_t2g : prev.sg_t2g,
      sg_total: Number.isFinite(rec.sg_total) ? rec.sg_total : prev.sg_total,
      _from_dg_historical_rounds: true,
    });
  }
}

let rebuilt = 0;
let removed = 0;
let inserted = 0;

for (const dg of allowedDg) {
  const shardPath = path.join(SHARD_DIR, `${dg}.json`);
  if (!fs.existsSync(shardPath)) continue;
  const shard = JSON.parse(fs.readFileSync(shardPath, "utf8"));
  if (!Array.isArray(shard.rounds)) shard.rounds = [];

  const before = shard.rounds.length;
  shard.rounds = shard.rounds.filter((rr) => {
    const same =
      normEvt(rr.event_name) === wantEvt || eventsLikelySame(rr.event_name, eventName);
    if (same) {
      removed += 1;
      return false;
    }
    return true;
  });

  const freshMap = freshByDg.get(dg);
  if (freshMap?.size) {
    for (const row of freshMap.values()) {
      shard.rounds.push(row);
      inserted += 1;
    }
  }

  shard.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  fs.writeFileSync(shardPath, JSON.stringify(shard));
  rebuilt += 1;
  if (before !== shard.rounds.length) {
    /* logged aggregate below */
  }
}

console.log(
  `[rebuild-current-event] Event "${foldComparableTitle(eventName)}": rebuilt ${rebuilt} shard(s); removed ${removed} old row(s); inserted ${inserted} fresh row(s) (pgatouR + live stats + CSV).`,
);
