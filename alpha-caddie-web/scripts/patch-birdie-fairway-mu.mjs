#!/usr/bin/env node
/**
 * Patch counting-market lines in round_projection_vs_actual.csv from DG methodology
 * (Birdies BoB, Fairways acc, GIR/Bogeys/Pars level+course paths).
 *
 *   node scripts/patch-birdie-fairway-mu.mjs
 *   npm run patch:birdie-fairway-mu
 */
import { createReadStream, existsSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { buildDgMethodologyMuMapForEvent } from "./dg-methodology-mu.mjs";
import { num } from "./round-projection-mu.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const REPO = join(WEB, "..");
const VS = join(WEB, "data", "round_projection_vs_actual.csv");
const HIST = existsSync(join(REPO, "data", "historical_rounds_all.csv"))
  ? join(REPO, "data", "historical_rounds_all.csv")
  : join(WEB, "data", "historical_rounds_all.csv");

const LINE_COLS = {
  Birdies: "birdies_line",
  "Fairways hit": "fairways_line",
  GIR: "gir_line",
  Bogeys: "bogeys_line",
  Pars: "pars_line",
  "Total score": "round_score_line",
};

async function loadCsv(path) {
  const rows = [];
  for await (const r of createReadStream(path).pipe(
    parse({ columns: true, relax_column_count: true, skip_records_with_error: true }),
  )) {
    rows.push(r);
  }
  return rows;
}

function parseMs(v) {
  const t = Date.parse(String(v || ""));
  return Number.isFinite(t) ? t : NaN;
}

function groupKey(r) {
  const event = String(r.event_name || "").trim();
  const year = Math.round(num(r.event_year ?? r.year, NaN));
  const round = Math.round(num(r.round, NaN));
  const bet =
    parseMs(r.bet_time) ||
    parseMs(r.projections_updated_at) ||
    parseMs(r.exported_at) ||
    0;
  const course = String(r.course_used || r.course_name || "").trim();
  return `${event}|${year}|${round}|${bet}|${course}`;
}

function csvEscape(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

function rowsToCsv(rows) {
  if (!rows.length) return "";
  const cols = Object.keys(rows[0]);
  const lines = [cols.map(csvEscape).join(",")];
  for (const r of rows) {
    lines.push(cols.map((c) => csvEscape(r[c])).join(","));
  }
  return `${lines.join("\n")}\n`;
}

async function main() {
  if (!existsSync(VS)) throw new Error(`Missing ${VS}`);
  if (!existsSync(HIST)) throw new Error(`Missing ${HIST}`);

  console.log("Loading CSV + hist…");
  const rows = await loadCsv(VS);
  const histRows = await loadCsv(HIST);
  console.log(`rows=${rows.length} hist=${histRows.length}`);

  /** @type {Map<string, object[]>} */
  const groups = new Map();
  for (const r of rows) {
    if (String(r.pricing_mode || "") !== "default") continue;
    if (String(r.pricing_skill || "") !== "default") continue;
    const k = groupKey(r);
    if (!groups.has(k)) groups.set(k, []);
    groups.get(k).push(r);
  }
  console.log(`cutoffs=${groups.size}`);

  /** @type {Map<object, Record<string, number>>} */
  const patch = new Map();
  let nOk = 0;
  let nSkip = 0;
  let i = 0;

  for (const [, group] of groups) {
    i++;
    const sample = group[0];
    const eventName = String(sample.event_name || "").trim();
    const eventYear = Math.round(num(sample.event_year ?? sample.year, NaN));
    const targetRound = Math.round(num(sample.round, NaN));
    const betTimeMs =
      parseMs(sample.bet_time) ||
      parseMs(sample.projections_updated_at) ||
      parseMs(sample.exported_at);
    const courseName = String(sample.course_used || sample.course_name || "").trim();
    const fieldDgIds = [
      ...new Set(
        group
          .map((r) => Math.round(num(r.dg_id, NaN)))
          .filter((d) => Number.isFinite(d)),
      ),
    ];
    if (!eventName || !Number.isFinite(targetRound) || !Number.isFinite(betTimeMs) || !fieldDgIds.length) {
      nSkip++;
      continue;
    }

    process.stdout.write(`\r  cutoff ${i}/${groups.size} ${eventName} R${targetRound}…`);
    const byDg = await buildDgMethodologyMuMapForEvent({
      repoRoot: REPO,
      histRows,
      eventName,
      eventYear,
      targetRound,
      betTimeMs,
      fieldDgIds,
      courseName,
    });

    for (const r of group) {
      const dg = Math.round(num(r.dg_id, NaN));
      const mus = byDg.get(dg);
      if (!mus) continue;
      /** @type {Record<string, number>} */
      const vals = {};
      let any = false;
      for (const market of Object.keys(LINE_COLS)) {
        const v = mus.get(market);
        if (Number.isFinite(v)) {
          vals[market] = v;
          any = true;
        }
      }
      if (!any) continue;
      patch.set(r, vals);
      nOk++;
    }
  }
  process.stdout.write("\n");

  console.log(`patched players=${nOk} skipped_groups=${nSkip}`);

  /** @type {Record<string, number>} */
  const changed = Object.fromEntries(Object.keys(LINE_COLS).map((m) => [m, 0]));
  for (const r of rows) {
    const p = patch.get(r);
    if (!p) continue;
    for (const [market, col] of Object.entries(LINE_COLS)) {
      const v = p[market];
      if (!Number.isFinite(v)) continue;
      const prev = num(r[col], NaN);
      r[col] = String(v);
      if (prev !== v) changed[market]++;
    }
  }
  console.log(
    Object.entries(changed)
      .map(([m, n]) => `${LINE_COLS[m]}=${n}`)
      .join(" "),
  );

  writeFileSync(VS, rowsToCsv(rows));
  console.log(`Wrote ${VS}`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
