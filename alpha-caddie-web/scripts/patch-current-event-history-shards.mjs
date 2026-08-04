#!/usr/bin/env node
/**
 * Fast path: merge current-event rounds into player-history shards without full CSV rescan.
 * Sources (in order):
 *   1. live-in-play.json live_round_actuals (works on CI without R / pgatouR)
 *   2. pgatour_event_rounds.json when it matches projections.event_name
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { spawnSync } from "child_process";
import {
  resolveLiveRoundActualsByDg,
  sanitizeLiveCountingFields,
  countingFromInPlayRow,
} from "./dg-live-tournament-stats.mjs";
import { foldComparableTitle, eventsLikelySame } from "./dg-events-align.mjs";
import { normCourseNameKey, courseShardFileName, formatCourseLabelForDisplay } from "./course-name-key.mjs";
import { historyRoundChartUtcIsoDay } from "./history-round-dates.mjs";
import { dateStartIsFuture } from "./dg-display-round-from-bundle.mjs";
import { reconcileHoleCountsFromScore } from "./course-round-adjustments.mjs";

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

function eventsCompatible(a, b) {
  const fa = foldComparableTitle(a);
  const fb = foldComparableTitle(b);
  if (!fa || !fb) return false;
  if (fa === fb || fa.includes(fb) || fb.includes(fa)) return true;
  return eventsLikelySame(a, b);
}

function parseUsDateSortKey(s) {
  if (!s) return 0;
  const t = String(s).trim();
  const iso = t.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) {
    const y = parseInt(iso[1], 10);
    const mo = parseInt(iso[2], 10);
    const d = parseInt(iso[3], 10);
    if (Number.isFinite(y) && Number.isFinite(mo) && Number.isFinite(d)) return y * 10000 + mo * 100 + d;
  }
  const p = t.split("/");
  if (p.length !== 3) return 0;
  const mo = parseInt(p[0], 10);
  const d = parseInt(p[1], 10);
  const y = parseInt(p[2], 10);
  if (!Number.isFinite(y)) return 0;
  return y * 10000 + (mo || 0) * 100 + (d || 0);
}

function eventCompletedMdYForRound(dateStartIso, roundNum) {
  if (!dateStartIso || roundNum < 1) return "";
  const m = String(dateStartIso).match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return "";
  const t = Date.UTC(parseInt(m[1], 10), parseInt(m[2], 10) - 1, parseInt(m[3], 10)) + (roundNum - 1) * 86400000;
  const d = new Date(t);
  return `${d.getUTCMonth() + 1}/${d.getUTCDate()}/${d.getUTCFullYear()}`;
}

function liveInPlayGrossForRound(inPlayRow, rnd) {
  if (!inPlayRow) return NaN;
  const r = Math.round(num(rnd, NaN));
  if (!Number.isFinite(r) || r < 1 || r > 4) return NaN;
  return num(inPlayRow[`R${r}`] ?? inPlayRow[`r${r}`], NaN);
}

function mergeRow(existing, liveRec) {
  const out = {
    ...existing,
    ...liveRec,
    _from_live_tournament_stats: true,
  };
  if (liveRec?._from_pgatour || existing?._from_pgatour) out._from_pgatour = true;
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
    "sg_50_100_fw",
    "sg_100_150_fw",
    "sg_150_200_fw",
    "sg_over_200_fw",
    "sg_under_150_rgh",
    "sg_over_150_rgh",
    "sg_app_dist_total",
    "n_50_100_fw",
    "n_100_150_fw",
    "n_150_200_fw",
    "n_over_200_fw",
    "n_under_150_rgh",
    "n_over_150_rgh",
    "n_app_dist",
    "sg_putt_0_5ft",
    "sg_putt_5_10ft",
    "sg_putt_10_15ft",
    "sg_putt_15_25ft",
    "sg_putt_25plus_ft",
    "sg_putt_dist_total",
    "n_putt_0_5ft",
    "n_putt_5_10ft",
    "n_putt_10_15ft",
    "n_putt_15_25ft",
    "n_putt_25plus_ft",
    "n_putt_dist",
    "birdies",
    "pars",
    "bogies",
    "bogeys",
    "round_score",
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
  if (Number.isFinite(num(act.birdies, NaN))) out.birdies = Math.round(num(act.birdies, NaN));
  if (Number.isFinite(num(act.pars, NaN))) out.pars = Math.round(num(act.pars, NaN));
  if (Number.isFinite(num(act.bogeys, NaN))) {
    out.bogeys = Math.round(num(act.bogeys, NaN));
    out.bogies = out.bogeys;
  }
  return out;
}

function fieldPlayerNames(proj) {
  /** @type {Map<number, string>} */
  const names = new Map();
  for (const p of proj?.players || []) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    names.set(dg, String(p.player_name || "").trim());
  }
  return names;
}

function upsertRoundIntoShard(shard, liveRec) {
  const wantEvt = normEvt(liveRec.event_name);
  const wantYr = parseInt(String(liveRec.year || ""), 10);
  const wantRnd = Math.round(num(liveRec.round_num, NaN));
  let hit = -1;
  for (let i = shard.rounds.length - 1; i >= 0; i--) {
    const rr = shard.rounds[i];
    if (Math.round(num(rr.round_num, NaN)) !== wantRnd) continue;
    if (Number.isFinite(wantYr) && parseInt(String(rr.year || ""), 10) !== wantYr) continue;
    if (normEvt(rr.event_name) !== wantEvt) continue;
    hit = i;
    break;
  }
  if (hit >= 0) {
    shard.rounds[hit] = mergeRow(shard.rounds[hit], liveRec);
    return false;
  }
  shard.rounds.push(liveRec);
  return true;
}

function buildLiveHistoryRows(proj, live, actualsByDg, fieldNames) {
  const meta = proj?.meta && typeof proj.meta === "object" ? proj.meta : {};
  const fu = live?.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const projEvent = String(proj?.event_name || "").trim();
  const fieldEvent = String(fu.event_name || "").trim();
  const inPlayEvent = String(live?.info?.event_name || live?.event_name || "").trim();
  const eventName = String(projEvent || fieldEvent || inPlayEvent).trim();
  if (!eventName) return [];

  if (projEvent && fieldEvent && !eventsCompatible(projEvent, fieldEvent)) return [];
  if (projEvent && inPlayEvent && !eventsCompatible(projEvent, inPlayEvent)) return [];

  const dateStartIso = String(fu.date_start || live?.info?.date_start || "").trim();
  if (dateStartIsFuture(dateStartIso)) return [];

  let courseName =
    String(proj?.course_used || meta.course_used || fu.course_name || "").trim() || eventName;
  courseName = formatCourseLabelForDisplay(courseName) || courseName;
  const roundPar = num(
    proj?.course_par_18 ?? meta.course_par_18 ?? fu.course_par ?? live?.info?.course_par ?? live?.course_par,
    72,
  );
  const eventIdStr = fu.event_id != null && fu.event_id !== "" ? String(fu.event_id) : "";
  const rows = Array.isArray(live?.data) ? live.data : [];
  const nameByDg = new Map(fieldNames);
  for (const r of rows) {
    const dg = Math.round(num(r?.dg_id ?? r?.dgId, NaN));
    if (!Number.isFinite(dg)) continue;
    const nm = String(r?.player_name ?? r?.playerName ?? "").trim();
    if (nm) nameByDg.set(dg, nm);
  }

  /** @type {object[]} */
  const out = [];
  for (const [dgKey, perRound] of Object.entries(actualsByDg || {})) {
    const dg = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    const displayName = nameByDg.get(dg) || "";
    const ipRow = rows.find((r) => Math.round(num(r?.dg_id ?? r?.dgId, NaN)) === dg);
    const playerR = Math.round(num(ipRow?.round ?? ipRow?.Round, NaN));

    for (const [rndKey, act] of Object.entries(perRound)) {
      if (!act || typeof act !== "object") continue;
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      let roundScore = num(act.round_score, NaN);
      if (ipRow) {
        const g = liveInPlayGrossForRound(ipRow, rnd);
        if (Number.isFinite(g)) roundScore = g;
      }
      if (!Number.isFinite(roundScore) || roundScore <= 0) continue;

      const eventDate = dateStartIso ? eventCompletedMdYForRound(dateStartIso, rnd) : "";
      if (!eventDate) continue;

      const eventYear = parseInt(String(eventDate).split("/")[2] || "", 10);
      let birdies = Number.isFinite(num(act.birdies, NaN)) ? Math.round(num(act.birdies, NaN)) : null;
      let pars = Number.isFinite(num(act.pars, NaN)) ? Math.round(num(act.pars, NaN)) : null;
      let bogeys = Number.isFinite(num(act.bogeys, NaN)) ? Math.round(num(act.bogeys, NaN)) : null;
      if (ipRow && playerR === rnd) {
        const thru = Math.round(num(act.thru ?? ipRow.thru, NaN));
        const ip = countingFromInPlayRow(ipRow, thru);
        if (Number.isFinite(ip.birdies)) birdies = Math.round(ip.birdies);
        if (Number.isFinite(ip.pars)) pars = Math.round(ip.pars);
        if (Number.isFinite(ip.bogeys)) bogeys = Math.round(ip.bogeys);
      }
      const girRaw = num(act.gir, NaN);
      let girVal = null;
      if (Number.isFinite(girRaw)) girVal = Math.round(girRaw > 0 && girRaw <= 1.0001 ? girRaw * 18 : girRaw);
      const fwVal = Number.isFinite(num(act.fairways, NaN)) ? Math.round(num(act.fairways, NaN)) : null;
      const puttsVal = Number.isFinite(num(act.putts, NaN)) ? Math.round(num(act.putts, NaN)) : null;

      let row = sanitizeLiveCountingFields({
        dg_id: dg,
        player_name: displayName,
        sortKey: parseUsDateSortKey(eventDate) * 10 + rnd,
        event_completed: eventDate,
        year: Number.isFinite(eventYear) ? eventYear : new Date().getFullYear(),
        event_name: eventName,
        event_id: eventIdStr,
        course_name: courseName,
        round_num: rnd,
        fin_text: "",
        round_score: Math.round(roundScore * 10) / 10,
        birdies,
        pars,
        bogies: bogeys,
        gir: girVal,
        fairways: fwVal,
        putts: puttsVal,
        eagles_or_better: null,
        doubles_or_worse: null,
        sg_putt: Number.isFinite(num(act.sg_putt, NaN)) ? num(act.sg_putt, NaN) : null,
        sg_app: Number.isFinite(num(act.sg_app, NaN)) ? num(act.sg_app, NaN) : null,
        sg_arg: Number.isFinite(num(act.sg_arg, NaN)) ? num(act.sg_arg, NaN) : null,
        sg_ott: Number.isFinite(num(act.sg_ott, NaN)) ? num(act.sg_ott, NaN) : null,
        sg_t2g: Number.isFinite(num(act.sg_t2g, NaN)) ? num(act.sg_t2g, NaN) : null,
        sg_total: Number.isFinite(num(act.sg_total, NaN)) ? num(act.sg_total, NaN) : null,
        _from_live_tournament_stats: true,
      });
      const fixed = reconcileHoleCountsFromScore(row, Number.isFinite(roundPar) ? roundPar : 72);
      if (Number.isFinite(fixed.birdies)) row.birdies = Math.round(fixed.birdies);
      if (Number.isFinite(fixed.bogeys)) {
        row.bogeys = Math.round(fixed.bogeys);
        row.bogies = row.bogeys;
      }
      if (Number.isFinite(fixed.pars)) row.pars = Math.round(fixed.pars);
      out.push(row);
    }
  }
  return out;
}

const proj = JSON.parse(fs.readFileSync(PROJ_JSON, "utf8"));
const live = fs.existsSync(LIVE_JSON) ? JSON.parse(fs.readFileSync(LIVE_JSON, "utf8")) : {};
const pga = fs.existsSync(PGA_JSON) ? JSON.parse(fs.readFileSync(PGA_JSON, "utf8")) : { rounds: [] };
const fieldNames = fieldPlayerNames(proj);
const fwHoles = Math.round(num(proj?.projection_course_basis?.fairway_holes_modeled, 14)) || 14;
const roundPar = num(proj?.course_par_18, 70) || 70;
const actualsByDg = resolveLiveRoundActualsByDg(live, { roundPar, fairwayHoles: fwHoles });
const projEvent = String(proj?.event_name || "").trim();
const pgaEvent = String(pga?.meta?.event_name || "").trim();
const pgaMatches = Boolean(pgaEvent && projEvent && eventsCompatible(pgaEvent, projEvent));

/** @type {Map<number, object[]>} */
const byDg = new Map();

const liveRows = buildLiveHistoryRows(proj, live, actualsByDg, fieldNames);
for (const row of liveRows) {
  const dg = Math.round(num(row.dg_id, NaN));
  if (!Number.isFinite(dg)) continue;
  if (!byDg.has(dg)) byDg.set(dg, []);
  byDg.get(dg).push(row);
}

if (pgaMatches) {
  for (const r of pga.rounds || []) {
    const dg = Math.round(num(r.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    if (!byDg.has(dg)) byDg.set(dg, []);
    const rnd = Math.round(num(r.round_num, NaN));
    let row = { ...r, _from_pgatour: true };
    const act = actualsByDg[String(dg)]?.[String(rnd)];
    row = enrichFromActuals(row, act);
    byDg.get(dg).push(row);
  }
} else if (pgaEvent) {
  console.log(
    `[patch-current-event] Skipping stale pgatouR "${pgaEvent}" (projections="${projEvent || "?"}") — using live-in-play only.`,
  );
}

let patched = 0;
let roundsAdded = 0;
for (const [dg, rows] of byDg) {
  const shardPath = path.join(SHARD_DIR, `${dg}.json`);
  if (!fs.existsSync(shardPath)) {
    if (!rows.length) continue;
    fs.mkdirSync(SHARD_DIR, { recursive: true });
  }
  const shard = fs.existsSync(shardPath)
    ? JSON.parse(fs.readFileSync(shardPath, "utf8"))
    : {
        dg_id: dg,
        player_name: fieldNames.get(dg) || String(rows[0]?.player_name || "").trim(),
        rounds: [],
      };
  if (!Array.isArray(shard.rounds)) shard.rounds = [];
  let addedHere = 0;
  for (const liveRec of rows) {
    if (upsertRoundIntoShard(shard, liveRec)) addedHere += 1;
  }
  shard.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  if (!shard.player_name) {
    shard.player_name = fieldNames.get(dg) || String(rows[0]?.player_name || "").trim();
  }
  fs.writeFileSync(shardPath, JSON.stringify(shard));
  patched += 1;
  roundsAdded += addedHere;
}

let courseEntriesPatched = 0;
const courseKey = normCourseNameKey(proj.course_used || (pgaMatches ? pga?.meta?.course_name : "") || "");
if (courseKey) {
  const byCoursePath = path.join(WEB, "player-history", "by-course", courseShardFileName(courseKey));
  /** @type {{ course_key?: string, days?: string[], entries?: object[] }} */
  let courseShard = { course_key: courseKey, days: [], entries: [] };
  if (fs.existsSync(byCoursePath)) {
    try {
      courseShard = JSON.parse(fs.readFileSync(byCoursePath, "utf8"));
    } catch {
      /* rebuild */
    }
  }
  if (!Array.isArray(courseShard.entries)) courseShard.entries = [];
  const daysSet = new Set(Array.isArray(courseShard.days) ? courseShard.days : []);

  const courseRows = [];
  for (const rows of byDg.values()) courseRows.push(...rows);
  for (const row of courseRows) {
    if (normCourseNameKey(row.course_name) && normCourseNameKey(row.course_name) !== courseKey) continue;
    const dg = Math.round(num(row.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const wantEvt = normEvt(row.event_name);
    const wantYr = row.year;
    const wantRnd = row.round_num;
    let hit = -1;
    for (let i = courseShard.entries.length - 1; i >= 0; i--) {
      const e = courseShard.entries[i];
      const rr = e?.row && typeof e.row === "object" ? e.row : e;
      const eDg = Math.round(num(e?.dg_id ?? rr?.dg_id, NaN));
      if (eDg !== dg) continue;
      if (rr.round_num !== wantRnd) continue;
      if (parseInt(String(rr.year || ""), 10) !== wantYr) continue;
      if (normEvt(rr.event_name) !== wantEvt) continue;
      hit = i;
      break;
    }
    const merged = mergeRow(hit >= 0 ? courseShard.entries[hit]?.row || {} : {}, row);
    const entry = {
      dg_id: dg,
      player_name: String(row.player_name || "").trim(),
      row: merged,
    };
    if (hit >= 0) courseShard.entries[hit] = entry;
    else courseShard.entries.push(entry);
    courseEntriesPatched += 1;
    const iso = historyRoundChartUtcIsoDay(merged);
    if (iso) daysSet.add(iso);
  }

  courseShard.course_key = courseKey;
  courseShard.days = [...daysSet].sort((a, b) => b.localeCompare(a));
  let skipCourseWrite = false;
  if (courseEntriesPatched === 0 && courseShard.entries.length === 0 && fs.existsSync(byCoursePath)) {
    try {
      const prev = JSON.parse(fs.readFileSync(byCoursePath, "utf8"));
      if (Array.isArray(prev?.entries) && prev.entries.length > 0) {
        skipCourseWrite = true;
        console.log(
          `[patch-current-event] Keeping prior by-course ${courseShardFileName(courseKey)} (${prev.entries.length} entries; no current-event rows yet).`,
        );
      }
    } catch {
      /* write empty */
    }
  }
  if (!skipCourseWrite) {
    fs.mkdirSync(path.dirname(byCoursePath), { recursive: true });
    fs.writeFileSync(byCoursePath, JSON.stringify(courseShard));
  }
}

console.log(
  `[patch-current-event] Updated ${patched} shard(s), +${roundsAdded} round row(s)` +
    ` (live=${liveRows.length}${pgaMatches ? `, pgatour=${(pga.rounds || []).length}` : ", pgatour=skipped"})` +
    (courseKey ? `; by-course ${courseShardFileName(courseKey)} (${courseEntriesPatched} row(s))` : "") +
    `; event="${foldComparableTitle(projEvent || pgaEvent)}"`,
);

const sync = spawnSync(process.execPath, ["scripts/sync-missing-field-history-from-csv.mjs"], {
  cwd: WEB,
  stdio: "inherit",
  env: process.env,
});
if (sync.status !== 0 && sync.status != null) process.exit(sync.status);
