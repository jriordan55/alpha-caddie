#!/usr/bin/env node
/**
 * Write one CSV row per projections.json player×round×pricing_mode with model lines,
 * over/under results vs actuals, and best edge (matches Round projections / Historical Trends).
 *
 * Walk-forward backtest projections use the same flat venue player score + field market reconcile as live
 * (historical-walkforward-projections.mjs → reconcileAllProjectionPlayerRows). Model lines in the detail CSV
 * are raw walk-forward μ at bet time — not book-calibrated snapshots from the audit log. DK lines/odds only
 * from pre-round audit (or live_snapshot for in-progress weeks with real DK props).
 *
 *   npm run export:round-projection-vs-actual
 *   npm run export:round-projection-vs-actual -- --full-backtest-rebuild  (regrade all prior events)
 *
 * Actuals (same priority as build-player-history): pgatour_event_rounds.json for the current event,
 * then preds/live-tournament-stats + in-play R1–R4 via live-in-play.json, then historical_rounds_all.csv
 * only for the matching event title + calendar year (never prior Byron Nelsons).
 * Birdies actuals include eagles (and eagles_or_better).
 * Book lines/odds: closing pre-round lines from dk_round_projection_audit.csv (last capture before
 * that round's first tee). Upcoming / in-progress rounds use current projections.props DraftKings rows
 * (book_odds_source=live_snapshot) when real DK props exist. No synthetic −110 model-as-book lines.
 * Completed rounds never use live props for book lines (pre vs actual comparison stays honest).
 * Rows with no book odds and no completed round score are omitted.
 *
 * Output:
 *   alpha-caddie-web/data/round_projection_vs_actual.csv (detail rows)
 *   alpha-caddie-web/data/round_projection_vs_actual_summary.csv (RMSE/MAE + EV-threshold units)
 *   alpha-caddie-web/data/round_projection_vs_actual.xlsx (detail + summary tabs)
 * `npm run push:live` (refresh:live) runs this after live merges + post-live CSV merge.
 */
import {
  copyFileSync,
  createReadStream,
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  unlinkSync,
  writeFileSync,
} from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame, foldComparableTitle } from "./dg-events-align.mjs";
import { resolveLiveRoundActualsByDg } from "./dg-live-tournament-stats.mjs";
import { formatCourseLabelForDisplay } from "./course-name-key.mjs";
import {
  EXPORT_ACTUAL_COLS,
  EXPORT_BOOK_LINE_COLS,
  EXPORT_MARKETS,
  EXPORT_MODEL_LINE_COLS,
  EXPORT_OVER_ODDS_COLS,
  EXPORT_OVER_RESULT_COLS,
  EXPORT_PRICING_MODES,
  EXPORT_UNDER_ODDS_COLS,
  EXPORT_UNDER_RESULT_COLS,
  birdiesPlusEaglesFromRow,
  createProjectionContext,
  enforceHalfLine,
  modelEdgePctAtLine,
  num,
  ouProjectedMeanForMode,
  ouSideResults,
} from "./round-projection-mu.mjs";
import {
  buildRoundStartUtcMs,
  buildRoundStartUtcMsForAuditEvent,
  buildRoundStartUtcMsFromDateStart,
  defaultDkAuditPath,
  inferDateStartFromAuditCaptures,
  loadPreRoundDkPropsFromAudit,
} from "./dk-pre-round-props.mjs";
import {
  buildRoundProjectionVsActualSummary,
  writeRoundProjectionVsActualWorkbook,
} from "./round-projection-vs-actual-summary.mjs";
import {
  EXPORT_SIGNAL_COLS,
  alignDetailCsvContent,
  extractSignalsFromHistRow,
  extractSignalsFromPlayerRow,
  loadCourseTableByKey,
  pinSheetActiveFor,
  signalCellsFromObject,
} from "./projection-context-signals.mjs";
import { normCourseNameKey } from "./course-name-key.mjs";
import {
  fitOutcomeMuBiasCorrections,
  fitOutcomeSigmaScales,
  setOutcomeMuBiasCorrections,
  setOutcomeSigmaScales,
} from "./projection-stat-model.mjs";
import { flatVenueProjectionPipelineEnv } from "./projection-pipeline-env.mjs";
import { buildBookPropsIndex } from "./projection-book-props.mjs";

if (process.argv.includes("--full-backtest-rebuild")) {
  process.env.GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS = "1";
}
Object.assign(process.env, flatVenueProjectionPipelineEnv());

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const DEFAULT_OUT = join(WEB_ROOT, "data", "round_projection_vs_actual.csv");
const DEFAULT_SUMMARY_OUT = join(WEB_ROOT, "data", "round_projection_vs_actual_summary.csv");
const DEFAULT_XLSX_OUT = join(WEB_ROOT, "data", "round_projection_vs_actual.xlsx");

const DETAIL_MID_COLS = [
  ...EXPORT_ACTUAL_COLS,
  "book_odds_source",
  ...EXPORT_MODEL_LINE_COLS,
  ...EXPORT_BOOK_LINE_COLS,
  ...EXPORT_OVER_ODDS_COLS,
  ...EXPORT_UNDER_ODDS_COLS,
  ...EXPORT_OVER_RESULT_COLS,
  ...EXPORT_UNDER_RESULT_COLS,
];
const HEADER =
  "exported_at,projections_updated_at,event_name,course_used,display_round,round,pricing_mode,pricing_skill,dg_id,player_name," +
  [...DETAIL_MID_COLS, ...EXPORT_SIGNAL_COLS, "edge"].join(",") +
  "\n";

function formatAmericanOdds(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return "";
  return v > 0 ? `+${v}` : String(v);
}

function fmtActual(marketKey, v) {
  if (!Number.isFinite(v)) return "";
  if (marketKey === "total") return (Math.round(v * 10) / 10).toFixed(1);
  return String(Math.round(v * 10) / 10 === Math.round(v) ? Math.round(v) : v);
}

function dkPropForPlayer(dkIndex, dg, rnd, propsMarket) {
  return dkIndex.get(`${dg}|${rnd}|${propsMarket}`) || null;
}


function roundHasCompletedScore(actuals, dg, rnd) {
  const act = actuals.get(`${dg}|${rnd}`);
  const score = num(act?.total_score, NaN);
  return Number.isFinite(score) && score > 0;
}

/**
 * Pre-round audit line when available; otherwise live DK props for rounds not yet completed.
 */
function dkPropForExport(preRoundIndex, liveIndex, dg, rnd, propsMarket, actuals) {
  const key = `${dg}|${rnd}|${propsMarket}`;
  const pre = preRoundIndex.get(key);
  if (pre) return { ...pre, oddsSource: "pre_round_audit" };
  if (roundHasCompletedScore(actuals, dg, rnd)) return null;
  const live = liveIndex.get(key);
  if (live) {
    const src = String(live.source || "live_snapshot").trim().toLowerCase();
    const oddsSource = src === "draftkings" ? "live_snapshot" : src;
    return { line: live.line, over: live.over, under: live.under, oddsSource };
  }
  return null;
}

/** Keep row only when at least one market has posted over or under American odds. */
function rowHasAnyBookOdds(rowCells) {
  for (const col of EXPORT_OVER_ODDS_COLS) {
    if (String(rowCells[col] ?? "").trim()) return true;
  }
  for (const col of EXPORT_UNDER_ODDS_COLS) {
    if (String(rowCells[col] ?? "").trim()) return true;
  }
  return false;
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

/**
 * Read prior-event detail rows from the existing CSV so the detail tab accumulates across events.
 * If none exist, backfill from the DK audit + historical actuals for all past events.
 * Returns { priorLines: string[], priorSummarySamples: object[] }.
 */
async function loadPriorEventRows(csvPath, summaryPath, currentEventName, opts = {}) {
  const priorLines = [];
  const priorSummaryRows = [];
  if (existsSync(csvPath)) {
    const raw = readFileSync(csvPath, "utf8");
    const aligned = alignDetailCsvContent(raw, HEADER);
    const rows = aligned.split(/\r?\n/).filter(Boolean);
    if (rows.length >= 2) {
      const cols = rows[0].split(",");
      const iEvent = cols.indexOf("event_name");
      if (iEvent >= 0) {
        for (let i = 1; i < rows.length; i++) {
          const cells = parseCsvRow(rows[i]);
          const ev = (cells[iEvent] || "").trim();
          if (!ev || eventsLikelySame(currentEventName, ev)) continue;
          priorLines.push(rows[i] + "\n");
        }
      }
    }
  }

  let summaryHasPrior = false;
  if (existsSync(summaryPath)) {
    const sumRaw = readFileSync(summaryPath, "utf8");
    const sumLines = sumRaw.split(/\r?\n/).filter(Boolean);
    if (sumLines.length > 1) {
      const hdrCols = sumLines[0].split(",");
      const evIdx = hdrCols.indexOf("event_name");
      if (evIdx >= 0) {
        for (let i = 1; i < sumLines.length; i++) {
          const cells = parseCsvRow(sumLines[i]);
          const ev = (cells[evIdx] || "").trim();
          if (ev && !eventsLikelySame(currentEventName, ev)) {
            summaryHasPrior = true;
            break;
          }
        }
      }
    }
  }

  const auditPath = opts.auditPath || defaultDkAuditPath(WEB_ROOT);
  const histPath = opts.histPath || resolveHistCsv();
  if (!existsSync(auditPath)) return { priorLines, priorSummaryRows };

  const backfill = await backfillFromAudit(auditPath, histPath, currentEventName, {
    fairwayHoles: opts.fairwayHoles || 14,
    livePath: opts.livePath || join(WEB_ROOT, "live-in-play.json"),
  });
  if (backfill.lines.length > 0) {
    priorLines.length = 0;
    priorLines.push(...backfill.lines);
    console.log(`[round-projection-vs-actual] Backfilled ${backfill.lines.length} prior-event detail row(s) from DK audit`);
  } else if (summaryHasPrior && priorLines.length) {
    /* keep cached prior detail when audit backfill unavailable */
  }
  priorSummaryRows.push(...backfill.summaryRows);
  return { priorLines, priorSummaryRows };
}

function actualsKey(eventName, eventYear, dg, rnd) {
  const yr = Math.round(num(eventYear, NaN));
  const yPart = Number.isFinite(yr) ? `${yr}\x1f` : "";
  return `${eventName}\x1f${yPart}${dg}|${rnd}`;
}

function readLiveBundleEvents(live) {
  const fu = live?.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const infoEvent = String(live?.info?.event_name || "").trim();
  const fuEvent = String(fu.event_name || "").trim();
  const ltsEvent = String(live?.live_tournament_stats?.event_name || "").trim();
  return {
    fu,
    infoEvent,
    fuEvent,
    inPlayEvent: infoEvent || ltsEvent,
    dateStartIso: String(fu.date_start || live?.info?.date_start || "").trim(),
  };
}

/** preds/in-play `data` belongs to `info` when field_updates already advanced to next week. */
function liveInPlayDataEventName(live) {
  const { infoEvent, fuEvent, inPlayEvent } = readLiveBundleEvents(live);
  if (infoEvent && fuEvent && !eventsLikelySame(infoEvent, fuEvent)) return inPlayEvent;
  return inPlayEvent || fuEvent || infoEvent;
}

function liveBundleMatchesTargetEvent(live, targetEventName) {
  if (!targetEventName || !live) return false;
  const dataEvent = liveInPlayDataEventName(live);
  if (!dataEvent) return false;
  return eventsLikelySame(targetEventName, dataEvent);
}

function inPlayGrossForRound(row, rnd) {
  const r = Math.round(num(rnd, NaN));
  if (!Number.isFinite(r) || r < 1 || r > 4) return NaN;
  return num(row?.[`R${r}`] ?? row?.[`r${r}`], NaN);
}

function enrichLiveCountingPatch(patch, act, coursePar = 70) {
  if (!patch || typeof patch !== "object") return patch;
  const par = Math.round(num(coursePar, NaN)) || 70;
  const e = Math.max(0, num(act?.eagles ?? act?.eagles_or_better, 0));
  const d = Math.max(0, num(act?.doubles ?? act?.doubles_or_worse, 0));
  let b = num(patch.birdies, NaN);
  const p = num(patch.pars, NaN);
  const bg = num(patch.bogeys, NaN);
  if (Number.isFinite(patch.total_score) && Number.isFinite(p) && Number.isFinite(bg) && (!Number.isFinite(b) || b === 0)) {
    const implied = 18 - e - d - p - bg;
    if (implied > 0 && implied < 12) patch.birdies = implied;
  }
  if (Number.isFinite(patch.total_score) && !Number.isFinite(p) && Number.isFinite(b) && Number.isFinite(bg)) {
    patch.pars = Math.max(0, 18 - e - d - b - bg);
  }
  return patch;
}

function liveCountingPatchWeak(patch) {
  if (!patch || !Number.isFinite(num(patch.total_score, NaN))) return false;
  const b = num(patch.birdies, NaN);
  const p = num(patch.pars, NaN);
  const bg = num(patch.bogeys, NaN);
  return (!Number.isFinite(b) || b === 0) && (!Number.isFinite(p) || p === 0) && (!Number.isFinite(bg) || bg === 0);
}

function patchFromPlayerHistoryShard(dg, eventName, eventYear, rnd, webRoot, fairwayHoles) {
  const shardPath = join(webRoot, "player-history", "by-dg", `${Math.round(dg)}.json`);
  if (!existsSync(shardPath)) return null;
  let shard;
  try {
    shard = JSON.parse(readFileSync(shardPath, "utf8"));
  } catch {
    return null;
  }
  const row = (Array.isArray(shard?.rounds) ? shard.rounds : []).find((r) => {
    if (Math.round(num(r.round_num, NaN)) !== rnd) return false;
    if (Math.round(num(r.year, NaN)) !== Math.round(num(eventYear, NaN))) return false;
    return eventsLikelySame(String(r.event_name || "").trim(), eventName);
  });
  if (!row) return null;
  const score = num(row.round_score, NaN);
  if (!Number.isFinite(score) || score <= 0) return null;
  return {
    total_score: Math.round(score * 10) / 10,
    birdies: birdiesPlusEaglesFromRow(row),
    pars: num(row.pars, NaN),
    bogeys: num(row.bogeys ?? row.bogies, NaN),
    gir: countFromRateOrRaw(row.gir, 18),
    fairways: countFromRateOrRaw(row.fairways, fairwayHoles),
    putts: Number.isFinite(num(row.putts, NaN)) && num(row.putts, NaN) > 1.5 ? Math.round(num(row.putts, NaN)) : NaN,
    source: "player_history_shard",
  };
}

function mergeShardPatchIntoActuals(allActuals, key, prev, shardPatch) {
  const weak = liveCountingPatchWeak(prev);
  const out = { ...(prev || {}) };
  if (weak || !Number.isFinite(num(prev?.total_score, NaN))) {
    Object.assign(out, shardPatch);
  } else {
    for (const k of ["birdies", "pars", "bogeys", "gir", "fairways", "putts"]) {
      if (!Number.isFinite(num(prev[k], NaN)) && Number.isFinite(num(shardPatch[k], NaN))) out[k] = shardPatch[k];
    }
    if (liveCountingPatchWeak(prev)) {
      out.birdies = shardPatch.birdies;
      out.pars = shardPatch.pars;
      out.bogeys = shardPatch.bogeys;
    }
    out.source = prev.source ? `${prev.source}+${shardPatch.source}` : shardPatch.source;
  }
  allActuals.set(key, out);
}

/** Fill gaps from player-history shards — including rounds with no live/historical row yet (e.g. R4). */
function overlayPlayerHistoryActualsForBackfill(
  allActuals,
  auditEventName,
  eventYear,
  webRoot,
  fairwayHoles,
  playerRounds = [],
) {
  if (!auditEventName || !Number.isFinite(num(eventYear, NaN))) return 0;
  const yr = Math.round(eventYear);
  const targets = new Map();
  for (const pr of playerRounds) {
    const dg = Math.round(num(pr?.dg, NaN));
    const rnd = Math.round(num(pr?.rnd, NaN));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;
    targets.set(`${dg}|${rnd}`, { dg, rnd });
  }
  const prefix = `${auditEventName}\x1f${yr}\x1f`;
  for (const [key] of allActuals) {
    if (!key.startsWith(prefix)) continue;
    const tail = key.slice(prefix.length);
    const [dgStr, rndStr] = tail.split("|");
    const dg = Math.round(num(dgStr, NaN));
    const rnd = Math.round(num(rndStr, NaN));
    if (Number.isFinite(dg) && Number.isFinite(rnd)) targets.set(`${dg}|${rnd}`, { dg, rnd });
  }

  let n = 0;
  for (const { dg, rnd } of targets.values()) {
    const shardPatch = patchFromPlayerHistoryShard(dg, auditEventName, yr, rnd, webRoot, fairwayHoles);
    if (!shardPatch) continue;
    const key = actualsKey(auditEventName, yr, dg, rnd);
    const prev = allActuals.get(key) || {};
    mergeShardPatchIntoActuals(allActuals, key, prev, shardPatch);
    n++;
  }
  return n;
}

/** Audit capture year for recurring events (U.S. Open, Memorial, …). */
function resolveAuditEventYearFromSnaps(snaps) {
  const years = [];
  for (const snap of snaps || []) {
    if (Number.isFinite(snap?.capturedMs)) years.push(new Date(snap.capturedMs).getUTCFullYear());
    const pat = String(snap?.projAt || "").match(/^(\d{4})/);
    if (pat) years.push(parseInt(pat[1], 10));
  }
  return years.length ? Math.max(...years) : NaN;
}

/** @deprecated use resolveAuditEventYearFromSnaps */
function resolveAuditEventYear(evMap) {
  return resolveAuditEventYearFromSnaps([...evMap.values()]);
}

/** When field_updates rolled to next week, preds/in-play still holds the prior event's results. */
function overlayStaleLiveActualsForBackfill(allActuals, auditEventName, eventYear, livePath, fairwayHoles) {
  if (!auditEventName || !existsSync(livePath)) return 0;
  let live;
  try {
    live = JSON.parse(readFileSync(livePath, "utf8"));
  } catch {
    return 0;
  }
  const fu = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const { infoEvent, fuEvent } = readLiveBundleEvents(live);
  const staleWeek = infoEvent && fuEvent && !eventsLikelySame(infoEvent, fuEvent);
  const liveEvent = staleWeek
    ? eventsLikelySame(auditEventName, infoEvent)
      ? infoEvent
      : ""
    : eventsLikelySame(auditEventName, infoEvent)
      ? infoEvent
      : eventsLikelySame(auditEventName, fuEvent)
        ? fuEvent
        : "";
  if (!liveEvent) return 0;

  const roundPar = num(fu.course_par ?? live?.info?.course_par, 72) || 72;
  const actualsByDg = resolveLiveRoundActualsByDg(live, { roundPar, fairwayHoles });
  const inPlayByDg = new Map();
  for (const row of Array.isArray(live.data) ? live.data : []) {
    const id = Math.round(num(row?.dg_id ?? row?.dgId, NaN));
    if (Number.isFinite(id)) inPlayByDg.set(id, row);
  }

  let n = 0;
  for (const [dgKey, perRound] of Object.entries(actualsByDg || {})) {
    const dg = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    const ipRow = inPlayByDg.get(dg);
    for (const [rndKey, act] of Object.entries(perRound)) {
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      let score = num(act?.round_score, NaN);
      if (ipRow) {
        const g = inPlayGrossForRound(ipRow, rnd);
        if (Number.isFinite(g) && g > 0) score = g;
      }
      const patch = enrichLiveCountingPatch(
        patchFromLiveRoundAct({ ...act, round_score: Number.isFinite(score) ? score : act?.round_score }, fairwayHoles),
        act,
        roundPar,
      );
      if (!patch) continue;
      const key = actualsKey(auditEventName, eventYear, dg, rnd);
      const prev = allActuals.get(key) || {};
      const out = { ...prev, ...patch };
      out.source = prev.source && patch.source ? `${prev.source}+${patch.source}` : patch.source || prev.source || "live_stale";
      allActuals.set(key, out);
      n++;
    }
  }
  return n;
}

/**
 * Rebuild detail rows for prior events from DK audit + historical actuals.
 * Uses the last pre-round capture per player×round×market as the snapshot.
 */
async function backfillFromAudit(auditPath, histPath, currentEventName, opts = {}) {
  if (existsSync(DEFAULT_OUT)) {
    const [scales, muBias] = await Promise.all([
      fitOutcomeSigmaScales(DEFAULT_OUT),
      fitOutcomeMuBiasCorrections(DEFAULT_OUT),
    ]);
    setOutcomeSigmaScales(scales);
    setOutcomeMuBiasCorrections(muBias);
  }
  const fairwayHoles = opts.fairwayHoles ?? 14;
  const livePath = opts.livePath || join(WEB_ROOT, "live-in-play.json");
  const MARKET_MODEL_COL = {
    "Total Score": "model_total_score",
    Birdies: "model_birdies",
    Pars: "model_pars",
    Bogeys: "model_bogeys",
    GIR: "model_gir",
    "Fairways hit": "model_fairways",
  };
  const MARKET_KEY = {
    "Total Score": "total",
    Birdies: "birdies",
    Pars: "pars",
    Bogeys: "bogeys",
    GIR: "gir",
    "Fairways hit": "fairways",
  };

  const auditEvents = new Set();
  const auditParserScan = createReadStream(auditPath).pipe(
    parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
  );
  for await (const row of auditParserScan) {
    const ev = String(row.event_name || "").trim();
    if (ev && !eventsLikelySame(currentEventName, ev)) auditEvents.add(ev);
  }

  const allActuals = new Map();
  const histByKey = new Map();
  /** @type {object[]} */
  const histRows = [];
  if (existsSync(histPath)) {
    const histParser = createReadStream(histPath).pipe(
      parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
    );
    for await (const row of histParser) {
      histRows.push(row);
      const ev = String(row.event_name || "").trim();
      if (!ev || eventsLikelySame(currentEventName, ev)) continue;
      if (!auditEvents.has(ev)) continue;
      const dg = Math.round(num(row.dg_id));
      const rnd = Math.round(num(row.round_num));
      if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      const score = num(row.round_score);
      if (!Number.isFinite(score)) continue;
      const ry = Math.round(num(row.year, NaN)) || yearFromEventCompleted(row.event_completed);
      const key = actualsKey(ev, ry, dg, rnd);
      histByKey.set(key, row);
      allActuals.set(key, {
        total_score: Math.round(score * 10) / 10,
        birdies: birdiesPlusEaglesFromRow(row),
        pars: num(row.pars, NaN),
        bogeys: num(row.bogies, NaN),
        gir: countFromRateOrRaw(row.gir, 18),
        fairways: countFromRateOrRaw(row.driving_acc, fairwayHoles),
        source: "historical_rounds",
      });
    }
  }

  /** @type {Map<string, Map<string, object>>} event → pre-round DK index */
  const auditByEvent = new Map();
  for (const ev of auditEvents) {
    const dateStart = await inferDateStartFromAuditCaptures(ev, auditPath);
    let roundStart = buildRoundStartUtcMsForAuditEvent(ev, {
      histRows,
      livePath,
      dateStart,
    });
    if (!roundStart.size && dateStart) {
      roundStart = buildRoundStartUtcMsFromDateStart(dateStart, "America/New_York");
    }
    const preIndex = await loadPreRoundDkPropsFromAudit(ev, auditPath, roundStart);
    if (preIndex.size) auditByEvent.set(ev, preIndex);
  }

  for (const [ev, preIndex] of auditByEvent) {
    const eventYear = resolveAuditEventYearFromSnaps([...preIndex.values()]);
    const playerRounds = [];
    const seen = new Set();
    for (const pk of preIndex.keys()) {
      const [dgStr, rndStr] = pk.split("|");
      const dg = Math.round(num(dgStr, NaN));
      const rnd = Math.round(num(rndStr, NaN));
      const sk = `${dg}|${rnd}`;
      if (!Number.isFinite(dg) || !Number.isFinite(rnd) || seen.has(sk)) continue;
      seen.add(sk);
      playerRounds.push({ dg, rnd });
    }
    overlayStaleLiveActualsForBackfill(allActuals, ev, eventYear, livePath, fairwayHoles);
    overlayPlayerHistoryActualsForBackfill(allActuals, ev, eventYear, WEB_ROOT, fairwayHoles, playerRounds);
  }

  const wfCache = histRows.length
    ? new (await import("./historical-walkforward-projections.mjs")).FullModelProjectionCache(
        REPO_ROOT,
        histRows,
      )
    : null;

  const resultLines = [];
  const samples = [];
  /** @type {object[]} */
  const wfTrainingRows = [];
  /** @type {object[]} */
  const pendingRows = [];

  for (const [ev, preIndex] of auditByEvent) {
    const snaps = [...preIndex.values()];
    const eventYear = resolveAuditEventYearFromSnaps(snaps);
    const playerRounds = new Map();
    for (const [pk, snap] of preIndex) {
      const [dgStr, rndStr, ...marketParts] = pk.split("|");
      const dg = Math.round(num(dgStr, NaN));
      const propRound = Math.round(num(rndStr, NaN));
      const market = marketParts.join("|") || snap.market;
      if (!Number.isFinite(dg) || !Number.isFinite(propRound)) continue;
      const prKey = `${dg}|${propRound}`;
      if (!playerRounds.has(prKey)) {
        playerRounds.set(prKey, {
          dg,
          rnd: propRound,
          playerName: snap.playerName,
          course: snap.course,
          projAt: snap.projAt,
          markets: {},
        });
      }
      const pr = playerRounds.get(prKey);
      pr.markets[market] = snap;
    }

    /** @type {Map<number, Map<number, Map<string, number>>>} */
    const wfByRound = new Map();
    if (wfCache) {
      const year = eventYear;
      const fieldDgIds = [...new Set([...playerRounds.values()].map((p) => p.dg))];
      let betTimeMs = Infinity;
      for (const snap of snaps) {
        if (Number.isFinite(snap.capturedMs) && snap.capturedMs < betTimeMs) betTimeMs = snap.capturedMs;
      }
      for (let rnd = 1; rnd <= 4; rnd++) {
        if (![...playerRounds.values()].some((p) => p.rnd === rnd)) continue;
        const wfMap = await wfCache.ensureEvent({
          event: ev,
          year,
          round: rnd,
          bet_time_ms: Number.isFinite(betTimeMs) ? betTimeMs : undefined,
          _field_dg_ids: fieldDgIds,
        });
        wfByRound.set(rnd, wfMap);
      }
    }

    let eventMs = Infinity;
    for (const snap of snaps) {
      if (Number.isFinite(snap.capturedMs) && snap.capturedMs < eventMs) eventMs = snap.capturedMs;
    }

    for (const [, pr] of playerRounds) {
      const act = allActuals.get(actualsKey(ev, eventYear, pr.dg, pr.rnd)) || {};
      const hasCompleted = Number.isFinite(act.total_score);
      let hasBookOdds = false;
      /** @type {object[]} */
      const marketDrafts = [];

      for (const spec of EXPORT_MARKETS) {
        const snap = pr.markets[spec.propsMarket];
        const wfMu = wfByRound.get(pr.rnd)?.get(pr.dg)?.get(spec.market);
        const auditModelLine = snap
          ? spec.propsMarket === "Total Score"
            ? snap.modelTotal
            : spec.key === "birdies"
              ? snap.modelBirdies
              : spec.key === "pars"
                ? snap.modelPars
                : spec.key === "bogeys"
                  ? snap.modelBogeys
                  : spec.key === "gir"
                    ? snap.modelGir
                    : spec.key === "fairways"
                      ? snap.modelFairways
                      : NaN
          : NaN;
        const wfModelLine = Number.isFinite(wfMu)
          ? spec.propsMarket === "Total Score"
            ? Math.round(wfMu * 10) / 10
            : wfMu
          : NaN;
        // Walk-forward full model (pre book-alignment) — audit snap is live-week calibrated toward DK.
        const rawModelLine = Number.isFinite(wfModelLine) ? wfModelLine : auditModelLine;
        const bookLine = snap ? enforceHalfLine(snap.dkLine) : NaN;
        const overOdds = snap?.overOdds;
        const underOdds = snap?.underOdds;
        if (Number.isFinite(overOdds) || Number.isFinite(underOdds)) hasBookOdds = true;

        if (Number.isFinite(rawModelLine) && Number.isFinite(bookLine)) {
          wfTrainingRows.push({
            event: ev,
            eventMs: Number.isFinite(eventMs) ? eventMs : Date.parse(pr.projAt) || NaN,
            market: spec.market,
            modelBookDelta: rawModelLine - bookLine,
          });
        }

        marketDrafts.push({
          spec,
          rawModelLine,
          bookLine,
          overOdds,
          underOdds,
          act,
        });
      }

      if (!hasBookOdds && !hasCompleted) continue;

      pendingRows.push({
        ev,
        eventYear,
        pr,
        act,
        hasCompleted,
        marketDrafts,
      });
    }
  }

  for (const pending of pendingRows) {
    const { ev, eventYear, pr, act, hasCompleted, marketDrafts } = pending;
    const exported = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
    const rowCells = {};

    for (const md of marketDrafts) {
      const { spec, rawModelLine, bookLine, overOdds, underOdds } = md;
      // Backfill grades raw walk-forward μ vs DK; do not bake book μ-shift (that tightens toward DK).
      const modelLine = Number.isFinite(rawModelLine) ? rawModelLine : NaN;

      rowCells[spec.lineCol] = fmtModelLine(spec.market, modelLine);
      rowCells[spec.bookLineCol] = Number.isFinite(bookLine) ? fmtLine(spec.market, bookLine) : "";
      rowCells[spec.overOddsCol] = formatAmericanOdds(overOdds);
      rowCells[spec.underOddsCol] = formatAmericanOdds(underOdds);

      const actual = actualForMarket(act, spec.key);
      const gradeLine = Number.isFinite(bookLine) ? bookLine : modelLine;
      const sides = Number.isFinite(actual)
        ? ouSideResults(spec.market, actual, gradeLine)
        : { over: "", under: "" };
      rowCells[spec.overCol] = sides.over;
      rowCells[spec.underCol] = sides.under;
      rowCells[spec.actualCol] = fmtActual(spec.key, actual);

      if (Number.isFinite(modelLine) && Number.isFinite(bookLine)) {
        const stubRow =
          spec.market === "Total score"
            ? { total_score: modelLine, round_sd: 2.75 }
            : spec.key === "birdies"
              ? { birdies: modelLine }
              : spec.key === "bogeys"
                ? { bogeys: modelLine }
                : spec.key === "gir"
                  ? { gir: modelLine }
                  : spec.key === "fairways"
                    ? { fairways: modelLine }
                    : { pars: modelLine };
        const meta = { projection_course_basis: { fairway_holes_modeled: fairwayHoles } };
        const edge = modelEdgePctAtLine(
          spec.market,
          modelLine,
          bookLine,
          stubRow,
          meta,
          overOdds,
          underOdds,
        );
        samples.push({
          _eventName: ev,
          _course: formatCourseLabelForDisplay(pr.course),
          pricingMode: "default",
          pricingSkill: "default",
          marketKey: spec.key,
          modelLine,
          bookLine,
          edgeOver: edge.edgeOver,
          edgeUnder: edge.edgeUnder,
          overOdds,
          underOdds,
          overResult: sides.over,
          underResult: sides.under,
          hasActual: hasCompleted,
          bookOddsSource: "pre_round_audit",
        });
      }
    }

    rowCells.actual_source = act.source || (hasCompleted ? "historical_rounds" : "");

    const histRow = histByKey.get(actualsKey(ev, eventYear, pr.dg, pr.rnd));
    let sigObj = histRow
      ? extractSignalsFromHistRow(histRow, WEB_ROOT, {
          courseUsed: pr.course,
          eventName: ev,
          projUpdatedAt: pr.projAt,
          fairwayHoles,
        })
      : {};
    if (!histRow) {
      const mGir = pr.markets.GIR;
      const mFw = pr.markets["Fairways hit"];
      const modelGir = mGir ? num(mGir.modelGir, NaN) : NaN;
      const modelFw = mFw ? num(mFw.modelFairways, NaN) : NaN;
      sigObj = {
        ...sigObj,
        gir_minus_fw: Number.isFinite(modelGir) && Number.isFinite(modelFw) ? modelGir - modelFw : NaN,
        course_fw_width: num(loadCourseTableByKey(WEB_ROOT).get(normCourseNameKey(pr.course))?.fw_width, NaN),
        pin_sheet_active: pinSheetActiveFor(WEB_ROOT, {
          courseUsed: pr.course,
          eventName: ev,
          round: pr.rnd,
          projUpdatedAt: pr.projAt,
        }),
      };
    }
    const sigCells = signalCellsFromObject(sigObj);

    const rowOrder = [
      exported,
      pr.projAt,
      ev,
      formatCourseLabelForDisplay(pr.course),
      pr.rnd,
      pr.rnd,
      "default",
      "default",
      pr.dg,
      pr.playerName,
      ...EXPORT_ACTUAL_COLS.map((c) => rowCells[c] || ""),
      "pre_round_audit",
      ...EXPORT_MODEL_LINE_COLS.map((c) => rowCells[c] || ""),
      ...EXPORT_BOOK_LINE_COLS.map((c) => rowCells[c] || ""),
      ...EXPORT_OVER_ODDS_COLS.map((c) => rowCells[c] || ""),
      ...EXPORT_UNDER_ODDS_COLS.map((c) => rowCells[c] || ""),
      ...EXPORT_OVER_RESULT_COLS.map((c) => rowCells[c] || ""),
      ...EXPORT_UNDER_RESULT_COLS.map((c) => rowCells[c] || ""),
      ...EXPORT_SIGNAL_COLS.map((c) => sigCells[c] || ""),
      "",
    ];
    resultLines.push(rowOrder.map(csvCell).join(",") + "\n");
  }

  const summaryRows = [];
  const samplesByEvent = new Map();
  for (const s of samples) {
    const sev = s._eventName || "unknown";
    if (!samplesByEvent.has(sev)) samplesByEvent.set(sev, { samples: [], course: "", projAt: "" });
    const bucket = samplesByEvent.get(sev);
    bucket.samples.push(s);
    if (!bucket.course && s._course) bucket.course = s._course;
  }
  for (const [sev, bucket] of samplesByEvent) {
    const evMeta = {
      exported: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
      projectionsUpdatedAt: bucket.projAt || "",
      eventName: sev,
      course: bucket.course || "",
      displayRound: "",
    };
    const content = buildRoundProjectionVsActualSummary(bucket.samples, evMeta);
    const rows = content.split(/\r?\n/).filter(Boolean).slice(1);
    summaryRows.push(...rows);
  }

  return { lines: resultLines, summaryRows };
}

function parseCsvRow(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"') {
      if (q && line[i + 1] === '"') { cur += '"'; i++; }
      else q = !q;
      continue;
    }
    if (ch === "," && !q) { out.push(cur); cur = ""; continue; }
    cur += ch;
  }
  out.push(cur);
  return out;
}

/**
 * Build cumulative summary: preserve prior-event rows from existing summary CSV,
 * add backfill rows for newly discovered prior events, and regenerate current-event rows.
 */
function buildCumulativeSummaryWithBackfill(
  summaryPath,
  currentEventName,
  currentSamples,
  currentMeta,
  backfillSummaryRows,
  opts = {},
) {
  const currentContent = buildRoundProjectionVsActualSummary(currentSamples, currentMeta);
  const currentRows = currentContent.split(/\r?\n/).filter(Boolean).slice(1);

  const forceFullRebuild = opts.forceFullRebuild === true;
  let priorRows = [];
  if (!forceFullRebuild && existsSync(summaryPath)) {
    const existing = readFileSync(summaryPath, "utf8");
    const existingLines = existing.split(/\r?\n/).filter(Boolean);
    if (existingLines.length > 1) {
      const hdrCols = existingLines[0].split(",");
      const evIdx = hdrCols.indexOf("event_name");
      if (evIdx >= 0) {
        for (let i = 1; i < existingLines.length; i++) {
          const cells = parseCsvRow(existingLines[i]);
          const ev = (cells[evIdx] || "").trim();
          if (ev && !eventsLikelySame(currentEventName, ev)) {
            priorRows.push(existingLines[i]);
          }
        }
      }
    }
  }

  if (forceFullRebuild && backfillSummaryRows?.length) {
    priorRows = [...backfillSummaryRows];
  } else if (priorRows.length === 0 && backfillSummaryRows && backfillSummaryRows.length > 0) {
    priorRows = backfillSummaryRows;
  }

  const SUMMARY_HEADER =
    "section,exported_at,projections_updated_at,event_name,course_used,display_round,pricing_mode,pricing_skill,market,rmse,mae,n_line_pairs,ev_threshold_pct,bet_side,bets,wins,losses,pushes,units_net,roi_pct\n";
  const perEventRows = [...priorRows, ...currentRows];

  const crossEventRows = buildCrossEventAggregation(perEventRows);

  const allRows = [...perEventRows, ...crossEventRows];
  if (allRows.length === 0) return SUMMARY_HEADER;
  return SUMMARY_HEADER + allRows.map((r) => `${r}\n`).join("");
}

/**
 * Aggregate ev_backtest rows across all events by market × EV threshold × side.
 * Also aggregates model_vs_book by market across events.
 */
function buildCrossEventAggregation(perEventRows) {
  const lineAgg = new Map();
  const evAgg = new Map();
  const exported = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");

  for (const row of perEventRows) {
    const cells = parseCsvRow(row);
    const section = (cells[0] || "").trim();

    if (section === "model_vs_book") {
      const market = (cells[8] || "").trim();
      const rmse = Number(cells[9]);
      const mae = Number(cells[10]);
      const n = Number(cells[11]);
      if (!market || !Number.isFinite(n) || n <= 0) continue;
      let m = lineAgg.get(market);
      if (!m) { m = { market, sq: 0, abs: 0, n: 0 }; lineAgg.set(market, m); }
      m.sq += rmse * rmse * n;
      m.abs += mae * n;
      m.n += n;
    } else if (section === "ev_backtest") {
      const market = (cells[8] || "").trim();
      const threshold = (cells[12] || "").trim();
      const side = (cells[13] || "").trim();
      const bets = Number(cells[14]);
      const wins = Number(cells[15]);
      const losses = Number(cells[16]);
      const pushes = Number(cells[17]);
      const units = Number(cells[18]);
      if (!market || !Number.isFinite(bets) || bets <= 0) continue;
      const ek = `${market}\x1f${threshold}\x1f${side}`;
      let m = evAgg.get(ek);
      if (!m) { m = { market, threshold, side, bets: 0, wins: 0, losses: 0, pushes: 0, units: 0 }; evAgg.set(ek, m); }
      m.bets += bets;
      m.wins += wins;
      m.losses += losses;
      m.pushes += pushes;
      m.units += Number.isFinite(units) ? units : 0;
    }
  }

  const out = [];
  for (const m of lineAgg.values()) {
    const rmse = Math.sqrt(m.sq / m.n);
    const mae = m.abs / m.n;
    out.push(
      [
        "model_vs_book_all",
        exported, "", "(all events)", "", "",
        "(all)", "(all)", m.market,
        fmtN(rmse, 3), fmtN(mae, 3), m.n,
        "", "", "", "", "", "", "", "",
      ].map(csvCell).join(","),
    );
  }
  for (const m of evAgg.values()) {
    const roi = m.bets > 0 ? (m.units / m.bets) * 100 : NaN;
    out.push(
      [
        "ev_backtest_all",
        exported, "", "(all events)", "", "",
        "(all)", "(all)", m.market,
        "", "", "",
        m.threshold, m.side, m.bets, m.wins, m.losses, m.pushes,
        fmtN(m.units, 2), fmtN(roi, 1),
      ].map(csvCell).join(","),
    );
  }
  return out;
}

function fmtN(v, digits) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10 ** digits) / 10 ** digits).toFixed(digits);
}

function fmt(v) {
  return Number.isFinite(v) ? v : "";
}

/** DK book lines (half-point buckets). */
function fmtLine(market, mu) {
  if (!Number.isFinite(mu)) return "";
  if (market === "Total score") return (Math.round(mu * 10) / 10).toFixed(1);
  return String(enforceHalfLine(mu));
}

/** Model μ for export/grading — keep precision; do not snap to DK half-lines. */
function fmtModelLine(market, mu) {
  if (!Number.isFinite(mu)) return "";
  if (market === "Total score") return (Math.round(mu * 10) / 10).toFixed(1);
  if (market === "Birdies" || market === "GIR") return (Math.round(mu * 10) / 10).toFixed(1);
  return String(Math.round(mu));
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

/** Same scaling as build-player-history.mjs (rate 0–1 vs raw counts). */
function countFromRateOrRaw(raw, holes) {
  if (raw == null || raw === "") return NaN;
  const n = num(raw, NaN);
  if (!Number.isFinite(n) || n === 0) return NaN;
  if (n > 0 && n <= 1.0001) {
    const c = Math.round(n * holes);
    if (c <= 1) return NaN;
    return Math.min(holes, Math.max(0, c));
  }
  if (n > 1.0001 && n <= holes + 1e-6) return Math.min(holes, Math.max(0, n));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

function eventYearFromPayload(payload) {
  const ds = String(
    payload?.datagolf_field_date_start || payload?.meta?.datagolf_field_date_start || "",
  ).trim();
  const y = parseInt(ds.slice(0, 4), 10);
  return Number.isFinite(y) ? y : NaN;
}

function yearFromEventCompleted(ec) {
  const m = String(ec || "").trim().match(/(\d{4})\s*$/);
  return m ? parseInt(m[1], 10) : NaN;
}

/** Same event week only — not every prior "Byron Nelson" in the archive. */
function historicalRowMatchesCurrentWeek(row, eventName, eventYear) {
  const ev = String(row.event_name || "").trim();
  if (!ev || !eventName) return false;
  if (foldComparableTitle(ev) !== foldComparableTitle(eventName)) return false;
  const ry = Math.round(num(row.year, NaN)) || yearFromEventCompleted(row.event_completed);
  if (Number.isFinite(eventYear) && Number.isFinite(ry) && ry !== eventYear) return false;
  return true;
}

function mergeActualEntry(map, dg, rnd, patch, opts = {}) {
  const key = `${dg}|${rnd}`;
  const prev = map.get(key) || {};
  const out = { ...prev };
  const onlyIfMissing = Boolean(opts.onlyIfMissing);
  const fields =
    opts.fields ||
    ["total_score", "birdies", "pars", "bogeys", "gir", "fairways", "putts"];
  for (const k of fields) {
    if (!Number.isFinite(patch[k])) continue;
    if (onlyIfMissing && Number.isFinite(prev[k])) continue;
    out[k] = patch[k];
  }
  if (patch.source) {
    out.source =
      prev.source && prev.source !== patch.source ? `${prev.source}+${patch.source}` : patch.source;
  }
  map.set(key, out);
}

function patchFromLiveRoundAct(act, fairwayHoles) {
  if (!act || typeof act !== "object") return null;
  const score = num(act.round_score, NaN);
  const birdies = birdiesPlusEaglesFromRow(act);
  const pars = num(act.pars, NaN);
  const bogeys = num(act.bogeys ?? act.bogies, NaN);
  const gir = countFromRateOrRaw(act.gir, 18);
  const fairways = countFromRateOrRaw(act.fairways, fairwayHoles);
  const puttsRaw = num(act.putts, NaN);
  const putts = Number.isFinite(puttsRaw) && puttsRaw > 1.5 && puttsRaw < 80 ? Math.round(puttsRaw) : NaN;
  if (
    !Number.isFinite(score) &&
    !Number.isFinite(birdies) &&
    !Number.isFinite(pars) &&
    !Number.isFinite(bogeys)
  ) {
    return null;
  }
  return {
    total_score: Number.isFinite(score) ? Math.round(score * 10) / 10 : NaN,
    birdies,
    pars,
    bogeys,
    gir,
    fairways,
    putts,
    source: String(act.source || "live").trim() || "live",
  };
}

function overlayPgatourEventActuals(map, eventName, webRoot, fairwayHoles = 14) {
  const pgPath = join(webRoot, "data", "pgatour_event_rounds.json");
  if (!eventName || !existsSync(pgPath)) return 0;
  let raw;
  try {
    raw = JSON.parse(readFileSync(pgPath, "utf8"));
  } catch {
    return 0;
  }
  const metaEvent = String(raw?.meta?.event_name || "").trim();
  if (metaEvent && foldComparableTitle(metaEvent) !== foldComparableTitle(eventName)) {
    if (!eventsLikelySame(eventName, metaEvent)) return 0;
  }
  const list = (Array.isArray(raw?.rounds) ? raw.rounds : []).filter((r) => r?._from_pgatour);
  let n = 0;
  for (const r of list) {
    const dg = Math.round(num(r.dg_id, NaN));
    const rnd = Math.round(num(r.round_num, NaN));
    const score = num(r.round_score, NaN);
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4 || !Number.isFinite(score) || score <= 0)
      continue;
    const pgPatch = {
      total_score: Math.round(score * 10) / 10,
      birdies: birdiesPlusEaglesFromRow(r),
      pars: num(r.pars, NaN),
      bogeys: num(r.bogies ?? r.bogeys, NaN),
      source: "pgatour",
    };
    const gir = countFromRateOrRaw(r.gir, 18);
    const fairways = countFromRateOrRaw(r.fairways, fairwayHoles);
    const puttsRaw = num(r.putts, NaN);
    const putts =
      Number.isFinite(puttsRaw) && puttsRaw > 1.5 && puttsRaw < 80 ? Math.round(puttsRaw) : NaN;
    if (Number.isFinite(gir)) pgPatch.gir = gir;
    if (Number.isFinite(fairways)) pgPatch.fairways = fairways;
    if (Number.isFinite(putts)) pgPatch.putts = putts;
    mergeActualEntry(map, dg, rnd, pgPatch);
    n++;
  }
  return n;
}

function overlayLiveRoundActuals(map, eventName, livePath, payload, fairwayHoles) {
  if (!existsSync(livePath)) return 0;
  let live;
  try {
    live = JSON.parse(readFileSync(livePath, "utf8"));
  } catch {
    return 0;
  }
  if (!liveBundleMatchesTargetEvent(live, eventName)) return 0;

  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : {};
  const { fu } = readLiveBundleEvents(live);
  const roundPar =
    num(payload?.course_par_18 ?? meta.course_par_18 ?? fu.course_par ?? live?.info?.course_par, NaN) || 72;
  const actualsByDg = resolveLiveRoundActualsByDg(live, { roundPar, fairwayHoles });
  const inPlayByDg = new Map();
  for (const row of Array.isArray(live.data) ? live.data : []) {
    const id = Math.round(num(row?.dg_id ?? row?.dgId, NaN));
    if (Number.isFinite(id)) inPlayByDg.set(id, row);
  }
  let n = 0;
  for (const [dgKey, perRound] of Object.entries(actualsByDg || {})) {
    const dg = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    const ipRow = inPlayByDg.get(dg);
    for (const [rndKey, act] of Object.entries(perRound)) {
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      let score = num(act?.round_score, NaN);
      if (ipRow) {
        const g = inPlayGrossForRound(ipRow, rnd);
        if (Number.isFinite(g) && g > 0) score = g;
      }
      let patch = enrichLiveCountingPatch(
        patchFromLiveRoundAct({ ...act, round_score: Number.isFinite(score) ? score : act?.round_score }, fairwayHoles),
        act,
        roundPar,
      );
      if (!patch) continue;
      mergeActualEntry(map, dg, rnd, patch, {
        onlyIfMissing: true,
        fields: ["total_score", "birdies", "pars", "bogeys"],
      });
      mergeActualEntry(map, dg, rnd, patch, {
        fields: ["gir", "fairways", "putts"],
      });
      n++;
    }
  }
  return n;
}

async function fillHistoricalActualGaps(map, eventName, eventYear, csvPath, fairwayHoles) {
  if (!eventName || !existsSync(csvPath)) return 0;
  let n = 0;
  const parser = createReadStream(csvPath).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  );

  for await (const row of parser) {
    if (!historicalRowMatchesCurrentWeek(row, eventName, eventYear)) continue;
    const dg = Math.round(num(row.dg_id));
    const rnd = Math.round(num(row.round_num));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
    const key = `${dg}|${rnd}`;
    const prev = map.get(key) || {};
    const score = num(row.round_score);
    if (!Number.isFinite(score)) continue;

    const patch = {
      total_score: Math.round(score * 10) / 10,
      birdies: birdiesPlusEaglesFromRow(row),
      pars: num(row.pars, NaN),
      bogeys: num(row.bogies, NaN),
      gir: countFromRateOrRaw(row.gir, 18),
      fairways: countFromRateOrRaw(row.driving_acc, fairwayHoles),
      putts: NaN,
      source: "historical_rounds",
    };
    const puttsRaw = num(row.putts);
    if (Number.isFinite(puttsRaw) && puttsRaw > 1.5 && puttsRaw < 80) patch.putts = Math.round(puttsRaw);

    let merged = false;
    for (const k of ["total_score", "birdies", "pars", "bogeys", "gir", "fairways", "putts"]) {
      if (!Number.isFinite(prev[k]) && Number.isFinite(patch[k])) merged = true;
    }
    if (!merged && Number.isFinite(prev.total_score)) continue;

    mergeActualEntry(map, dg, rnd, patch);
    n++;
  }
  return n;
}

export async function buildActualsMapForEvent(payload, opts = {}) {
  const eventName = String(payload.event_name || "").trim();
  const eventYear = eventYearFromPayload(payload);
  const fairwayHoles = opts.fairwayHoles ?? 14;
  const livePath = opts.livePath || join(WEB_ROOT, "live-in-play.json");
  const histPath = opts.histPath || resolveHistCsv();
  const map = new Map();

  const liveN = overlayLiveRoundActuals(map, eventName, livePath, payload, fairwayHoles);
  const pgN = overlayPgatourEventActuals(map, eventName, WEB_ROOT, fairwayHoles);
  const histN = await fillHistoricalActualGaps(map, eventName, eventYear, histPath, fairwayHoles);

  return { map, pgN, liveN, histN, eventYear };
}

function actualForMarket(act, marketKey) {
  if (!act || typeof act !== "object") return NaN;
  if (marketKey === "total") return num(act.total_score, NaN);
  if (marketKey === "birdies") return num(act.birdies, NaN);
  if (marketKey === "pars") return num(act.pars, NaN);
  if (marketKey === "bogeys") return num(act.bogeys, NaN);
  if (marketKey === "gir") return num(act.gir, NaN);
  if (marketKey === "fairways") return num(act.fairways, NaN);
  return NaN;
}

/**
 * @param {{ projectionsPath?: string, outPath?: string, livePath?: string }} [opts]
 */
export async function writeRoundProjectionVsActualCsv(opts = {}) {
  const projPath = opts.projectionsPath || join(WEB_ROOT, "projections.json");
  const outPath = opts.outPath || DEFAULT_OUT;
  if (existsSync(outPath)) {
    const [scales, muBias] = await Promise.all([
      fitOutcomeSigmaScales(outPath),
      fitOutcomeMuBiasCorrections(outPath),
    ]);
    setOutcomeSigmaScales(scales);
    setOutcomeMuBiasCorrections(muBias);
  }
  const livePath = opts.livePath || join(WEB_ROOT, "live-in-play.json");

  if (!existsSync(projPath)) {
    throw new Error(`Missing projections: ${projPath}`);
  }

  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  const meta = { ...payload, course_used: payload.course_used, meta: payload };
  meta._webRoot = WEB_ROOT;
  const eventName = String(payload.event_name || "").trim();
  const course = formatCourseLabelForDisplay(String(payload.course_used || "").trim());
  const displayRound = Math.round(num(payload.display_round, 1)) || 1;
  const projAt = String(payload.updated_at || "").trim();
  const exported = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const players = Array.isArray(payload.players) ? payload.players : [];
  const fairwayHoles = Math.round(num(payload.projection_course_basis?.fairway_holes_modeled, 14)) || 14;

  const histPath = resolveHistCsv();
  const { map: actuals, pgN, liveN, histN } = await buildActualsMapForEvent(payload, {
    fairwayHoles,
    livePath,
    histPath,
  });

  const roundStartUtcMs = buildRoundStartUtcMs(players, payload);
  const auditPath = opts.dkAuditPath || defaultDkAuditPath(WEB_ROOT);
  const preRoundDkIndex = await loadPreRoundDkPropsFromAudit(eventName, auditPath, roundStartUtcMs);
  const liveDkIndex = buildBookPropsIndex(payload);
  const ctx = createProjectionContext({ ...payload, _webRoot: WEB_ROOT });
  const lines = [HEADER];
  const summarySamples = [];
  let withActual = 0;
  let skippedEmpty = 0;
  let preRoundOddsRows = 0;
  let liveSnapshotOddsRows = 0;

  for (const p of players) {
    const dg = Math.round(num(p?.dg_id));
    const rnd = Math.round(num(p?.round));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;

    const act = actuals.get(`${dg}|${rnd}`) || {};
    if (Number.isFinite(act.total_score)) withActual++;

    const playerSignals = signalCellsFromObject(extractSignalsFromPlayerRow(p, payload, WEB_ROOT));

    for (const pm of EXPORT_PRICING_MODES) {
      const rowCells = Object.fromEntries(
        [
          ...EXPORT_ACTUAL_COLS,
          "book_odds_source",
          ...EXPORT_MODEL_LINE_COLS,
          ...EXPORT_BOOK_LINE_COLS,
          ...EXPORT_OVER_ODDS_COLS,
          ...EXPORT_UNDER_ODDS_COLS,
          ...EXPORT_OVER_RESULT_COLS,
          ...EXPORT_UNDER_RESULT_COLS,
          "edge",
        ].map((c) => [c, ""]),
      );

      for (const spec of EXPORT_MARKETS) {
        rowCells[spec.actualCol] = fmtActual(spec.key, actualForMarket(act, spec.key));
      }
      rowCells.actual_source = act.source || "";
      rowCells.book_odds_source = "";

      let bestEdge = NaN;
      let rowOddsSource = "";
      const rowMarketSamples = [];

      for (const spec of EXPORT_MARKETS) {
        const mu = ouProjectedMeanForMode(spec.market, p, payload, pm.mode, pm.skill, ctx);
        const modelLine = mu;
        rowCells[spec.lineCol] = fmtModelLine(spec.market, mu);

        const dk = dkPropForExport(preRoundDkIndex, liveDkIndex, dg, rnd, spec.propsMarket, actuals);
        const actual = actualForMarket(act, spec.key);
        const roundComplete = Number.isFinite(actual);
        const bookLine = dk ? enforceHalfLine(dk.line) : NaN;
        if (dk) {
          if (!rowOddsSource) rowOddsSource = dk.oddsSource;
          rowCells[spec.bookLineCol] = fmtLine(spec.market, bookLine);
          rowCells[spec.overOddsCol] = formatAmericanOdds(dk.over);
          rowCells[spec.underOddsCol] = formatAmericanOdds(dk.under);
        }
        const gradeLine = Number.isFinite(bookLine) ? bookLine : modelLine;

        const sides = roundComplete
          ? ouSideResults(spec.market, actual, gradeLine)
          : { over: "", under: "" };
        rowCells[spec.overCol] = sides.over;
        rowCells[spec.underCol] = sides.under;

        const edgeLine = Number.isFinite(bookLine) ? bookLine : modelLine;
        const edge = modelEdgePctAtLine(
          spec.market,
          mu,
          edgeLine,
          p,
          payload,
          dk?.over,
          dk?.under,
        );
        if (Number.isFinite(edge.best) && (!Number.isFinite(bestEdge) || edge.best > bestEdge)) {
          bestEdge = edge.best;
        }

        rowMarketSamples.push({
          pricingMode: pm.mode,
          pricingSkill: pm.skill,
          marketKey: spec.key,
          modelLine: Number.isFinite(modelLine) ? modelLine : NaN,
          bookLine: Number.isFinite(bookLine) ? bookLine : NaN,
          edgeOver: edge.edgeOver,
          edgeUnder: edge.edgeUnder,
          overOdds: dk?.over,
          underOdds: dk?.under,
          overResult: sides.over,
          underResult: sides.under,
          hasActual: Number.isFinite(actual),
        });
      }

      rowCells.book_odds_source = rowOddsSource;
      rowCells.edge = Number.isFinite(bestEdge) ? (Math.round(bestEdge * 10) / 10).toFixed(1) : "";

      const hasBook = rowHasAnyBookOdds(rowCells);
      const hasCompleted = roundHasCompletedScore(actuals, dg, rnd);
      if (!hasBook && !hasCompleted) {
        skippedEmpty++;
        continue;
      }
      for (const sm of rowMarketSamples) {
        summarySamples.push({ ...sm, bookOddsSource: rowOddsSource });
      }
      if (rowOddsSource === "pre_round_audit") preRoundOddsRows++;
      else if (rowOddsSource === "live_snapshot" || rowOddsSource === "model_fallback") liveSnapshotOddsRows++;

      const rowOrder = [
        exported,
        projAt,
        eventName,
        course,
        displayRound,
        rnd,
        pm.mode,
        pm.skill,
        dg,
        String(p?.player_name || "").trim(),
        ...EXPORT_ACTUAL_COLS.map((c) => rowCells[c]),
        rowCells.book_odds_source,
        ...EXPORT_MODEL_LINE_COLS.map((c) => rowCells[c]),
        ...EXPORT_BOOK_LINE_COLS.map((c) => rowCells[c]),
        ...EXPORT_OVER_ODDS_COLS.map((c) => rowCells[c]),
        ...EXPORT_UNDER_ODDS_COLS.map((c) => rowCells[c]),
        ...EXPORT_OVER_RESULT_COLS.map((c) => rowCells[c]),
        ...EXPORT_UNDER_RESULT_COLS.map((c) => rowCells[c]),
        ...EXPORT_SIGNAL_COLS.map((c) => playerSignals[c] || ""),
        rowCells.edge,
      ];

      lines.push(rowOrder.map(csvCell).join(",") + "\n");
    }
  }

  const summaryPath = opts.summaryPath || DEFAULT_SUMMARY_OUT;
  const { priorLines, priorSummaryRows } = await loadPriorEventRows(outPath, summaryPath, eventName, {
    auditPath: opts.dkAuditPath || defaultDkAuditPath(WEB_ROOT),
    histPath: resolveHistCsv(),
    fairwayHoles,
  });
  const detailContent = [HEADER, ...priorLines, ...lines.slice(1)].join("");
  persistCsv(outPath, detailContent);
  const xlsxPath = opts.xlsxPath || DEFAULT_XLSX_OUT;
  const summaryMeta = {
    exported,
    projectionsUpdatedAt: projAt,
    eventName,
    course,
    displayRound,
  };
  const forceRebuildBacktest =
    String(process.env.GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS || "").trim() === "1";
  const summaryContent = buildCumulativeSummaryWithBackfill(
    summaryPath,
    eventName,
    summarySamples,
    summaryMeta,
    priorSummaryRows,
    { forceFullRebuild: forceRebuildBacktest },
  );
  persistCsv(summaryPath, summaryContent);
  let xlsxWritten = false;
  const skipXlsx = String(process.env.GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX || "").trim() === "1";
  if (skipXlsx) {
    console.log("[round-projection-vs-actual] Skipping xlsx (GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL_XLSX=1).");
  } else {
    try {
      xlsxWritten = await writeRoundProjectionVsActualWorkbook(outPath, summaryContent, xlsxPath);
    } catch (e) {
      console.warn(`[round-projection-vs-actual] xlsx write failed: ${e?.message || e}`);
    }
  }
  let finalPath = outPath;
  let finalSummaryPath = summaryPath;
  const pub = ensureRoundProjectionArtifactsPublished(outPath, summaryPath, xlsxPath);
  finalPath = pub.detail.path;
  finalSummaryPath = pub.summary.path;
  if (pub.lockedCount > 0) {
    console.warn(
      `[round-projection-vs-actual] ${pub.lockedCount} file(s) still locked — close Excel/editor, then: npm run promote:round-projection-vs-actual`,
    );
  }
  const rowCount = lines.length - 1 + priorLines.length;
  const summaryRowCount = Math.max(0, summaryContent.split("\n").filter(Boolean).length - 1);
  return {
    path: finalPath,
    summaryPath: finalSummaryPath,
    xlsxPath: xlsxWritten ? xlsxPath : "",
    rows: rowCount,
    summaryRows: summaryRowCount,
    withActual,
    skippedNoOdds: skippedEmpty,
    preRoundOddsRows,
    liveSnapshotOddsRows,
    eventName,
    pricingModes: EXPORT_PRICING_MODES.length,
    actualSources: { pgatour: pgN, live: liveN, historical: histN },
  };
}

/** Write via temp file; on lock, stage `.new` and warn (push:live can still finish). */
function persistCsv(outPath, content) {
  mkdirSync(dirname(outPath), { recursive: true });
  const tmp = `${outPath}.tmp`;
  writeFileSync(tmp, content, "utf8");
  try {
    try {
      unlinkSync(outPath);
    } catch (e) {
      if (e?.code !== "ENOENT") throw e;
    }
    renameSync(tmp, outPath);
    return outPath;
  } catch (e) {
    if (e?.code !== "EBUSY" && e?.code !== "EPERM" && e?.code !== "EACCES") throw e;
    const alt = `${outPath}.new`;
    try {
      unlinkSync(alt);
    } catch (err) {
      if (err?.code !== "ENOENT") throw err;
    }
    renameSync(tmp, alt);
    console.warn(
      `[round-projection-vs-actual] ${outPath} is locked (close Excel/editor). Fresh export is at ${alt}.`,
    );
    return alt;
  }
}

/** Promote `.new` → main artifact when Excel had the target locked. Never throws on EBUSY. */
export function ensureRoundProjectionCsvPublished(outPath = DEFAULT_OUT) {
  const alt = `${outPath}.new`;
  if (!existsSync(alt)) {
    return { path: outPath, promoted: false, locked: false };
  }
  try {
    copyFileSync(alt, outPath);
    unlinkSync(alt);
    console.log(`[round-projection-vs-actual] Promoted ${alt} -> ${outPath}`);
    return { path: outPath, promoted: true, locked: false };
  } catch (e) {
    if (e?.code !== "EBUSY" && e?.code !== "EPERM" && e?.code !== "EACCES") throw e;
    console.warn(
      `[round-projection-vs-actual] ${outPath} is locked (close Excel/editor). Fresh export remains at ${alt}.`,
    );
    return { path: alt, promoted: false, locked: true };
  }
}

/** Promote detail + summary + xlsx `.new` files when possible. */
export function ensureRoundProjectionArtifactsPublished(
  outPath = DEFAULT_OUT,
  summaryPath = DEFAULT_SUMMARY_OUT,
  xlsxPath = DEFAULT_XLSX_OUT,
) {
  const detail = ensureRoundProjectionCsvPublished(outPath);
  const summary = ensureRoundProjectionCsvPublished(summaryPath);
  const xlsx = ensureRoundProjectionCsvPublished(xlsxPath);
  const lockedCount = [detail, summary, xlsx].filter((r) => r.locked).length;
  return { detail, summary, xlsx, lockedCount };
}

async function main() {
  const {
    path,
    summaryPath,
    xlsxPath,
    rows,
    summaryRows,
    withActual,
    skippedNoOdds,
    preRoundOddsRows,
    liveSnapshotOddsRows,
    eventName,
    pricingModes,
    actualSources,
  } = await writeRoundProjectionVsActualCsv();
  const src = actualSources || {};
  console.log(
    `[round-projection-vs-actual] Wrote ${rows} detail row(s)` +
      (summaryRows ? ` + ${summaryRows} summary row(s)` : "") +
      (skippedNoOdds ? ` (skipped ${skippedNoOdds} with no odds or completed score)` : "") +
      `; book_odds pre_round_audit=${preRoundOddsRows ?? 0} live_snapshot=${liveSnapshotOddsRows ?? 0}` +
      `; ${withActual} player-rounds with actual score; actuals pgatour=${src.pgatour ?? 0} live=${src.live ?? 0} historical=${src.historical ?? 0}; ${pricingModes} pricing modes -> ${path}` +
      (summaryPath ? ` | summary ${summaryPath}` : "") +
      (xlsxPath ? ` | xlsx ${xlsxPath}` : "") +
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
