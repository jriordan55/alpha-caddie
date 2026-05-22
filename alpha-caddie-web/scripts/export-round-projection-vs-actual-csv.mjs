#!/usr/bin/env node
/**
 * Write one CSV row per projections.json player×round×pricing_mode with model lines,
 * over/under results vs actuals, and best edge (matches Round projections / Historical Trends).
 *
 *   npm run export:round-projection-vs-actual
 *
 * Actuals: `data/historical_rounds_all.csv` (same event, dg_id, round_num), then live-in-play R1–R4
 * gross scores for the active event when the round is complete but CSV lags.
 * Birdies actuals include eagles (and eagles_or_better).
 * Book lines/odds: last DK capture in dk_round_projection_audit.csv strictly before that round's
 * first tee time (not live projections.props refreshes mid-round).
 *
 * Output: alpha-caddie-web/data/round_projection_vs_actual.csv (overwrite each run)
 * `npm run push:live` (refresh:live) runs this after live merges + post-live CSV merge.
 */
import { createReadStream, existsSync, mkdirSync, readFileSync, renameSync, unlinkSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
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
  defaultDkAuditPath,
  loadPreRoundDkPropsFromAudit,
} from "./dk-pre-round-props.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const DEFAULT_OUT = join(WEB_ROOT, "data", "round_projection_vs_actual.csv");

const HEADER =
  "exported_at,projections_updated_at,event_name,course_used,display_round,round,pricing_mode,pricing_skill,dg_id,player_name," +
  [
    ...EXPORT_ACTUAL_COLS,
    ...EXPORT_MODEL_LINE_COLS,
    ...EXPORT_BOOK_LINE_COLS,
    ...EXPORT_OVER_ODDS_COLS,
    ...EXPORT_UNDER_ODDS_COLS,
    ...EXPORT_OVER_RESULT_COLS,
    ...EXPORT_UNDER_RESULT_COLS,
    "edge",
  ].join(",") + "\n";

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

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

function fmt(v) {
  return Number.isFinite(v) ? v : "";
}

function fmtLine(market, mu) {
  if (!Number.isFinite(mu)) return "";
  if (market === "Total score") return (Math.round(mu * 10) / 10).toFixed(1);
  return String(enforceHalfLine(mu));
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
  const n = num(raw, NaN);
  if (!Number.isFinite(n)) return NaN;
  if (n > 0 && n <= 1.0001) return Math.min(holes, Math.max(0, Math.round(n * holes)));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

async function loadActualsFromHistorical(eventName, csvPath, fairwayHoles) {
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
    const fairways = countFromRateOrRaw(row.driving_acc, fairwayHoles);
    const puttsRaw = num(row.putts);
    const putts = Number.isFinite(puttsRaw) && puttsRaw > 1.5 && puttsRaw < 80 ? Math.round(puttsRaw) : NaN;
    const birdies = birdiesPlusEaglesFromRow(row);

    map.set(`${dg}|${rnd}`, {
      total_score: Math.round(score * 10) / 10,
      birdies,
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

  const meta = projections?.meta && typeof projections.meta === "object" ? projections.meta : projections;
  let currentRound = Math.round(
    num(meta.datagolf_live_current_round ?? meta.display_round ?? fu.current_round ?? live?.info?.current_round, NaN),
  );
  if (!Number.isFinite(currentRound) || currentRound < 1) currentRound = 4;

  for (const r of rows) {
    const dg = Math.round(num(r?.dg_id ?? r?.dgId));
    if (!Number.isFinite(dg)) continue;
    for (let rnd = 1; rnd <= 4; rnd++) {
      const gross = num(r[`R${rnd}`] ?? r[`r${rnd}`], NaN);
      if (!Number.isFinite(gross)) continue;
      if (rnd > currentRound && String(meta?.date_start || fu.date_start || "").trim()) continue;

      const key = `${dg}|${rnd}`;
      const prev = actuals.get(key) || {};
      const score = Math.round(gross * 10) / 10;
      actuals.set(key, {
        ...prev,
        total_score: score,
        source:
          prev.source === "historical_rounds" && Number.isFinite(prev.birdies) ? "historical_rounds" : "live_in_play",
      });
    }
  }
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
  const actuals = await loadActualsFromHistorical(eventName, histPath, fairwayHoles);
  overlayLiveInPlayActuals(eventName, actuals, livePath, payload);

  const roundStartUtcMs = buildRoundStartUtcMs(players, payload);
  const auditPath = opts.dkAuditPath || defaultDkAuditPath(WEB_ROOT);
  const dkIndex = await loadPreRoundDkPropsFromAudit(eventName, auditPath, roundStartUtcMs);
  const ctx = createProjectionContext({ ...payload, _webRoot: WEB_ROOT });
  const lines = [HEADER];
  let withActual = 0;

  for (const p of players) {
    const dg = Math.round(num(p?.dg_id));
    const rnd = Math.round(num(p?.round));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;

    const act = actuals.get(`${dg}|${rnd}`) || {};
    if (Number.isFinite(act.total_score)) withActual++;

    for (const pm of EXPORT_PRICING_MODES) {
      const rowCells = Object.fromEntries(
        [
          ...EXPORT_ACTUAL_COLS,
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

      let bestEdge = NaN;

      for (const spec of EXPORT_MARKETS) {
        const mu = ouProjectedMeanForMode(spec.market, p, payload, pm.mode, pm.skill, ctx);
        const modelLine = spec.market === "Total score" ? mu : enforceHalfLine(mu);
        rowCells[spec.lineCol] = fmtLine(spec.market, mu);

        const dk = dkPropForPlayer(dkIndex, dg, rnd, spec.propsMarket);
        const bookLine = dk ? enforceHalfLine(dk.line) : NaN;
        const gradeLine = Number.isFinite(bookLine) ? bookLine : modelLine;
        if (dk) {
          rowCells[spec.bookLineCol] = fmtLine(spec.market, bookLine);
          rowCells[spec.overOddsCol] = formatAmericanOdds(dk.over);
          rowCells[spec.underOddsCol] = formatAmericanOdds(dk.under);
        }

        const actual = actualForMarket(act, spec.key);
        const sides = ouSideResults(spec.market, actual, gradeLine);
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
        ).best;
        if (Number.isFinite(edge) && (!Number.isFinite(bestEdge) || edge > bestEdge)) {
          bestEdge = edge;
        }
      }

      rowCells.edge = Number.isFinite(bestEdge) ? (Math.round(bestEdge * 10) / 10).toFixed(1) : "";

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
        ...EXPORT_MODEL_LINE_COLS.map((c) => rowCells[c]),
        ...EXPORT_BOOK_LINE_COLS.map((c) => rowCells[c]),
        ...EXPORT_OVER_ODDS_COLS.map((c) => rowCells[c]),
        ...EXPORT_UNDER_ODDS_COLS.map((c) => rowCells[c]),
        ...EXPORT_OVER_RESULT_COLS.map((c) => rowCells[c]),
        ...EXPORT_UNDER_RESULT_COLS.map((c) => rowCells[c]),
        rowCells.edge,
      ];

      lines.push(rowOrder.map(csvCell).join(",") + "\n");
    }
  }

  const writtenPath = persistCsv(outPath, lines.join(""));
  const rowCount = lines.length - 1;
  return { path: writtenPath, rows: rowCount, withActual, eventName, pricingModes: EXPORT_PRICING_MODES.length };
}

/** Write via temp rename; if the target is open (Excel), fall back to `.new` beside it. */
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
      `[round-projection-vs-actual] ${outPath} is locked (close Excel/editor). Wrote ${alt} — re-run export after closing.`,
    );
    return alt;
  }
}

async function main() {
  const { path, rows, withActual, eventName, pricingModes } = await writeRoundProjectionVsActualCsv();
  console.log(
    `[round-projection-vs-actual] Wrote ${rows} row(s) (${withActual} player-rounds with actual score; ${pricingModes} pricing modes each) -> ${path}` +
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
