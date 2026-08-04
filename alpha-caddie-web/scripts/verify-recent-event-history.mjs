#!/usr/bin/env node
/**
 * Verify Historical Trends shards include all recent completed rounds from CSV.
 * Finds the most recently completed PGA event in historical_rounds_all.csv and
 * ensures every player-round for that event that exists in CSV also exists on
 * the matching by-dg shard (when the shard exists).
 *
 * Exit 1 if gaps remain (so push:live softOpt can warn, or hard-fail when required).
 * Set GOLF_HISTORY_VERIFY_SOFT=1 to warn without failing.
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";
import { parse } from "csv-parse";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");
const SHARD_DIR = path.join(WEB, "player-history", "by-dg");
const ROUNDS_CSV = path.join(REPO, "data", "historical_rounds_all.csv");
const soft = String(process.env.GOLF_HISTORY_VERIFY_SOFT || "").trim() === "1";

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

function parseCompleted(mdy) {
  const m = String(mdy || "").match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  if (!m) return null;
  return new Date(Date.UTC(+m[3], +m[1] - 1, +m[2]));
}

function roundKey(yr, evt, rnd) {
  return `${yr}|${normEvt(evt)}|${rnd}`;
}

if (!fs.existsSync(ROUNDS_CSV)) {
  console.warn("[verify-history-recent] No CSV — skip");
  process.exit(0);
}

/** @type {Map<string, {event:string, year:number, completed:string, date:Date, rounds:object[]}>} */
const byEvent = new Map();
await new Promise((res, rej) => {
  createReadStream(ROUNDS_CSV)
    .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
    .on("data", (r) => {
      if (String(r.tour || "").toLowerCase() !== "pga") return;
      const completed = String(r.event_completed || "").trim();
      const d = parseCompleted(completed);
      if (!d) return;
      const yr = parseInt(String(r.year || ""), 10);
      const evt = String(r.event_name || "").trim();
      if (!evt || !Number.isFinite(yr)) return;
      const rnd = Math.round(num(r.round_num, NaN));
      const dg = Math.round(num(r.dg_id, NaN));
      if (!Number.isFinite(rnd) || !Number.isFinite(dg)) return;
      const ek = `${yr}|${normEvt(evt)}`;
      if (!byEvent.has(ek)) {
        byEvent.set(ek, { event: evt, year: yr, completed, date: d, rounds: [] });
      }
      const pack = byEvent.get(ek);
      if (d > pack.date) {
        pack.date = d;
        pack.completed = completed;
      }
      pack.rounds.push({
        dg,
        rnd,
        score: num(r.round_score),
        player: String(r.player_name || ""),
      });
    })
    .on("end", res)
    .on("error", rej);
});

const events = [...byEvent.values()].sort((a, b) => b.date - a.date);
if (!events.length) {
  console.warn("[verify-history-recent] No dated PGA events in CSV");
  process.exit(0);
}

const latest = events[0];
console.log(
  `[verify-history-recent] Latest completed event: ${latest.event} ${latest.year} (${latest.completed}) — ${latest.rounds.length} CSV rounds`,
);

const missing = [];
const byRnd = {};
for (const r of latest.rounds) {
  byRnd[r.rnd] = (byRnd[r.rnd] || 0) + 1;
  const shardPath = path.join(SHARD_DIR, `${r.dg}.json`);
  if (!fs.existsSync(shardPath)) continue; // no Trends shard yet — sync creates for field only
  let shard;
  try {
    shard = JSON.parse(fs.readFileSync(shardPath, "utf8"));
  } catch {
    missing.push({ ...r, reason: "bad_shard" });
    continue;
  }
  const want = roundKey(latest.year, latest.event, r.rnd);
  const hit = (shard.rounds || []).some(
    (rr) => roundKey(rr.year, rr.event_name, rr.round_num) === want,
  );
  if (!hit) missing.push({ ...r, reason: "missing_round" });
}

console.log(`[verify-history-recent] CSV rounds by round_num:`, byRnd);
if (!missing.length) {
  console.log("[verify-history-recent] OK — all CSV rounds for latest event present on existing shards.");
  process.exit(0);
}

console.warn(
  `[verify-history-recent] ${missing.length} gap(s) on existing shards for ${latest.event}:`,
);
for (const m of missing.slice(0, 20)) {
  console.warn(`  dg=${m.dg} ${m.player} R${m.rnd} score=${m.score} (${m.reason})`);
}
if (missing.length > 20) console.warn(`  … +${missing.length - 20} more`);

if (soft) {
  console.warn("[verify-history-recent] GOLF_HISTORY_VERIFY_SOFT=1 — continuing.");
  process.exit(0);
}
process.exit(1);
