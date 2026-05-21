#!/usr/bin/env node
/**
 * Write one CSV row per projections.json player×round with model stats + actual results when known.
 *
 *   npm run export:round-projection-vs-actual
 *
 * Actuals: `data/historical_rounds_all.csv` (same event, dg_id, round_num), then live-in-play R1–R4
 * gross scores for the active event when the round is complete but CSV lags.
 *
 * Output: alpha-caddie-web/data/round_projection_vs_actual.csv
 * `npm run push:all` runs this after update:rounds.
 */
import { createReadStream, existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { formatCourseLabelForDisplay } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const DEFAULT_OUT = join(WEB_ROOT, "data", "round_projection_vs_actual.csv");

const HEADER =
  "exported_at,projections_updated_at,event_name,course_used,display_round,round,dg_id,player_name," +
  "model_total_score,model_birdies,model_pars,model_bogeys,model_gir,model_fairways,model_putts,model_mu_sg," +
  "actual_total_score,actual_birdies,actual_pars,actual_bogeys,actual_gir,actual_fairways,actual_putts,actual_source\n";

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

function fmt(v) {
  return Number.isFinite(v) ? v : "";
}

/** Same scaling as build-player-history.mjs (rate 0–1 vs raw counts). */
function countFromRateOrRaw(raw, holes) {
  const n = num(raw, NaN);
  if (!Number.isFinite(n)) return NaN;
  if (n > 0 && n <= 1.0001) return Math.min(holes, Math.max(0, Math.round(n * holes)));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

function resolveHistCsv() {
  if (process.env.HISTORICAL_ROUNDS_CSV?.trim()) {
    return resolve(process.env.HISTORICAL_ROUNDS_CSV.trim());
  }
  const candidates = [
    join(REPO_ROOT, "data", "historical_rounds_all.csv"),
    join(WEB_ROOT, "data", "historical_rounds_all.csv"),
  ];
  return candidates.find((p) => existsSync(p)) || candidates[0];
}

async function loadActualsFromHistorical(eventName, csvPath) {
  const map = new Map();
  if (!eventName || !existsSync(csvPath)) return map;

  const parser = createReadStream(csvPath).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  );

  for await (const row of parser) {
    const ev = String(row.event_name || "").trim();
    if (!eventsLikelySame(eventName, ev)) continue;
    const dg = Math.round(num(row.dg_id));
    const rnd = Math.round(num(row.round_num));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
    const score = num(row.round_score);
    if (!Number.isFinite(score)) continue;

    const gir = countFromRateOrRaw(row.gir, 18);
    const fairways = countFromRateOrRaw(row.driving_acc, 14);
    const puttsRaw = num(row.putts);
    const putts = Number.isFinite(puttsRaw) && puttsRaw > 1.5 && puttsRaw < 80 ? Math.round(puttsRaw) : NaN;
    map.set(`${dg}|${rnd}`, {
      total_score: Math.round(score * 10) / 10,
      birdies: num(row.birdies),
      pars: num(row.pars),
      bogeys: num(row.bogies),
      gir,
      fairways,
      putts,
      source: "historical_rounds",
    });
  }
  return map;
}

function overlayLiveInPlayActuals(eventName, actuals, livePath, projections) {
  if (!existsSync(livePath)) return;
  let live;
  try {
    live = JSON.parse(readFileSync(livePath, "utf8"));
  } catch {
    return;
  }
  const rows = Array.isArray(live?.data) ? live.data : [];
  if (!rows.length) return;

  const fu = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const liveEvent = String(fu.event_name || live?.info?.event_name || "").trim();
  if (liveEvent && eventName && !eventsLikelySame(eventName, liveEvent)) return;

  const meta = projections?.meta && typeof projections.meta === "object" ? projections.meta : {};
  const dateStartIso = String(fu.date_start || live?.info?.date_start || "").trim();
  let currentRound = Math.round(
    num(
      meta.datagolf_live_current_round ??
        meta.display_round ??
        fu.current_round ??
        live?.info?.current_round,
      NaN,
    ),
  );
  if (!Number.isFinite(currentRound) || currentRound < 1) currentRound = 4;

  for (const r of rows) {
    const dg = Math.round(num(r?.dg_id ?? r?.dgId));
    if (!Number.isFinite(dg)) continue;
    for (let rnd = 1; rnd <= 4; rnd++) {
      const gross = num(r[`R${rnd}`] ?? r[`r${rnd}`], NaN);
      if (!Number.isFinite(gross)) continue;
      if (rnd > currentRound && dateStartIso) continue;

      const key = `${dg}|${rnd}`;
      const prev = actuals.get(key) || {};
      const score = Math.round(gross * 10) / 10;
      actuals.set(key, {
        ...prev,
        total_score: score,
        source: prev.source === "historical_rounds" && Number.isFinite(prev.birdies) ? "historical_rounds" : "live_in_play",
      });
    }
  }
}

/**
 * @param {{ projectionsPath?: string, outPath?: string, livePath?: string }} [opts]
 */
export async function writeRoundProjectionVsActualCsv(opts = {}) {
  const projPath = opts.projectionsPath || join(WEB_ROOT, "projections.json");
  const outPath = opts.outPath || DEFAULT_OUT;
  const livePath = opts.livePath || join(WEB_ROOT, "live-in-play.json");

  if (!existsSync(projPath)) {
    throw new Error(`Missing projections: ${projPath}`);
  }

  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  const eventName = String(payload.event_name || "").trim();
  const course = formatCourseLabelForDisplay(String(payload.course_used || "").trim());
  const displayRound = Math.round(num(payload.display_round, 1)) || 1;
  const projAt = String(payload.updated_at || "").trim();
  const exported = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const players = Array.isArray(payload.players) ? payload.players : [];

  const histPath = resolveHistCsv();
  const actuals = await loadActualsFromHistorical(eventName, histPath);
  overlayLiveInPlayActuals(eventName, actuals, livePath, payload);

  const lines = [HEADER];
  let withActual = 0;

  for (const p of players) {
    const dg = Math.round(num(p?.dg_id));
    const rnd = Math.round(num(p?.round));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;

    const act = actuals.get(`${dg}|${rnd}`) || {};
    if (Number.isFinite(act.total_score)) withActual++;

    lines.push(
      [
        exported,
        projAt,
        eventName,
        course,
        displayRound,
        rnd,
        dg,
        String(p?.player_name || "").trim(),
        fmt(num(p?.total_score)),
        fmt(num(p?.birdies)),
        fmt(num(p?.pars)),
        fmt(num(p?.bogeys)),
        fmt(num(p?.gir)),
        fmt(num(p?.fairways)),
        fmt(num(p?.putts)),
        fmt(num(p?.mu_sg)),
        fmt(act.total_score),
        fmt(act.birdies),
        fmt(act.pars),
        fmt(act.bogeys),
        fmt(act.gir),
        fmt(act.fairways),
        fmt(act.putts),
        act.source || "",
      ]
        .map(csvCell)
        .join(",") + "\n",
    );
  }

  mkdirSync(dirname(outPath), { recursive: true });
  writeFileSync(outPath, lines.join(""), "utf8");
  return { path: outPath, rows: lines.length - 1, withActual, eventName };
}

async function main() {
  const { path, rows, withActual, eventName } = await writeRoundProjectionVsActualCsv();
  console.log(
    `[round-projection-vs-actual] Wrote ${rows} row(s) (${withActual} with actual_total_score) -> ${path}` +
      (eventName ? ` (${eventName})` : ""),
  );
}

const isMain =
  Boolean(process.argv[1]) &&
  resolve(fileURLToPath(import.meta.url)) === resolve(process.argv[1]);
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
