#!/usr/bin/env node
/**
 * Write one CSV row per projections.json player×round×pricing_mode with model lines,
 * over/under results vs actuals, and best edge (matches Round projections / Historical Trends).
 *
 *   npm run export:round-projection-vs-actual
 *
 * Actuals (same priority as build-player-history): pgatour_event_rounds.json for the current event,
 * then preds/live-tournament-stats + in-play R1–R4 via live-in-play.json, then historical_rounds_all.csv
 * only for the matching event title + calendar year (never prior Byron Nelsons).
 * Birdies actuals include eagles (and eagles_or_better).
 * Book lines/odds: closing pre-round lines from dk_round_projection_audit.csv (last capture before
 * that round's first tee). Upcoming / in-progress rounds without a pre-round line use current
 * projections.props DraftKings rows (book_odds_source=live_snapshot) so R3/R4 lines are still exported.
 * Completed rounds never use live props for book lines (pre vs actual comparison stays honest).
 * Rows with no book odds and no completed round score are omitted.
 *
 * Output: alpha-caddie-web/data/round_projection_vs_actual.csv (overwrite each run)
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
    "book_odds_source",
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

/** Current DK scrape in projections.json keyed like the audit index. */
function buildLiveDkPropsFromProjections(payload) {
  const map = new Map();
  for (const r of Array.isArray(payload?.props) ? payload.props : []) {
    if (String(r.source || "").trim().toLowerCase() !== "draftkings") continue;
    const dg = Math.round(num(r.dg_id, NaN));
    const rnd = Math.round(num(r.round_num, NaN));
    const market = String(r.market || "").trim();
    const line = num(r.line, NaN);
    const over = num(r.over_odds, NaN);
    const under = num(r.under_odds, NaN);
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || rnd < 1 || rnd > 4 || !market) continue;
    if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;
    map.set(`${dg}|${rnd}|${market}`, { line, over, under });
  }
  return map;
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
  if (live) return { ...live, oddsSource: "live_snapshot" };
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
  const fu = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
  const liveEvent = String(fu.event_name || live?.info?.event_name || live?.live_tournament_stats?.event_name || "").trim();
  if (liveEvent && eventName && !eventsLikelySame(eventName, liveEvent)) return 0;

  const meta = payload?.meta && typeof payload.meta === "object" ? payload.meta : {};
  const roundPar =
    num(payload?.course_par_18 ?? meta.course_par_18 ?? fu.course_par ?? live?.info?.course_par, NaN) || 72;
  const actualsByDg = resolveLiveRoundActualsByDg(live, { roundPar, fairwayHoles });
  let n = 0;
  for (const [dgKey, perRound] of Object.entries(actualsByDg || {})) {
    const dg = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    for (const [rndKey, act] of Object.entries(perRound)) {
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      const patch = patchFromLiveRoundAct(act, fairwayHoles);
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
  const liveDkIndex = buildLiveDkPropsFromProjections(payload);
  const ctx = createProjectionContext({ ...payload, _webRoot: WEB_ROOT });
  const lines = [HEADER];
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

      for (const spec of EXPORT_MARKETS) {
        const mu = ouProjectedMeanForMode(spec.market, p, payload, pm.mode, pm.skill, ctx);
        const modelLine = spec.market === "Total score" ? mu : enforceHalfLine(mu);
        rowCells[spec.lineCol] = fmtLine(spec.market, mu);

        const dk = dkPropForExport(preRoundDkIndex, liveDkIndex, dg, rnd, spec.propsMarket, actuals);
        const bookLine = dk ? enforceHalfLine(dk.line) : NaN;
        const gradeLine = Number.isFinite(bookLine) ? bookLine : modelLine;
        if (dk) {
          if (!rowOddsSource) rowOddsSource = dk.oddsSource;
          rowCells[spec.bookLineCol] = fmtLine(spec.market, bookLine);
          rowCells[spec.overOddsCol] = formatAmericanOdds(dk.over);
          rowCells[spec.underOddsCol] = formatAmericanOdds(dk.under);
        }

        const actual = actualForMarket(act, spec.key);
        const sides = Number.isFinite(actual)
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
        ).best;
        if (Number.isFinite(edge) && (!Number.isFinite(bestEdge) || edge > bestEdge)) {
          bestEdge = edge;
        }
      }

      rowCells.book_odds_source = rowOddsSource;
      rowCells.edge = Number.isFinite(bestEdge) ? (Math.round(bestEdge * 10) / 10).toFixed(1) : "";

      const hasBook = rowHasAnyBookOdds(rowCells);
      const hasCompleted = roundHasCompletedScore(actuals, dg, rnd);
      if (!hasBook && !hasCompleted) {
        skippedEmpty++;
        continue;
      }
      if (rowOddsSource === "pre_round_audit") preRoundOddsRows++;
      else if (rowOddsSource === "live_snapshot") liveSnapshotOddsRows++;

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
        rowCells.edge,
      ];

      lines.push(rowOrder.map(csvCell).join(",") + "\n");
    }
  }

  persistCsv(outPath, lines.join(""));
  let finalPath = outPath;
  try {
    finalPath = ensureRoundProjectionCsvPublished(outPath);
  } catch (e) {
    const alt = `${outPath}.new`;
    if (existsSync(alt)) {
      console.warn(String(e?.message || e));
      finalPath = alt;
    } else {
      throw e;
    }
  }
  const rowCount = lines.length - 1;
  return {
    path: finalPath,
    rows: rowCount,
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

/** Promote `.new` → main CSV after a locked write; required before push:live can succeed. */
export function ensureRoundProjectionCsvPublished(outPath = DEFAULT_OUT) {
  const alt = `${outPath}.new`;
  if (!existsSync(alt)) return outPath;
  try {
    copyFileSync(alt, outPath);
    unlinkSync(alt);
    console.log(`[round-projection-vs-actual] Promoted ${alt} -> ${outPath}`);
    return outPath;
  } catch (e) {
    throw new Error(
      `[round-projection-vs-actual] ${outPath} is still locked (close Excel/editor). Fresh export remains at ${alt}.`,
      { cause: e },
    );
  }
}

async function main() {
  const {
    path,
    rows,
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
    `[round-projection-vs-actual] Wrote ${rows} row(s)` +
      (skippedNoOdds ? ` (skipped ${skippedNoOdds} with no odds or completed score)` : "") +
      `; book_odds pre_round_audit=${preRoundOddsRows ?? 0} live_snapshot=${liveSnapshotOddsRows ?? 0}` +
      `; ${withActual} player-rounds with actual score; actuals pgatour=${src.pgatour ?? 0} live=${src.live ?? 0} historical=${src.historical ?? 0}; ${pricingModes} pricing modes -> ${path}` +
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
