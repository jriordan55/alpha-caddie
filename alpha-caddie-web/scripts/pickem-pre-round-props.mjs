/**
 * Pre-round props from pick'em / multi-book audit CSVs (PP / SL / UD / FD / CZR / KL).
 * Same tee-window rules as dk-pre-round-props.mjs.
 */
import { createReadStream, existsSync } from "fs";
import { Readable } from "stream";
import { join } from "path";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import {
  auditPropRoundFromCapture,
  normalizeAuditRow,
  num,
} from "./dk-pre-round-props.mjs";

function snapFromAuditRow(row, capturedMs) {
  return {
    capturedMs,
    projAt: String(row.projections_updated_at || "").trim(),
    course: String(row.course_used || "").trim(),
    dg: Math.round(num(row.dg_id)),
    playerName: String(row.player_name || "").trim(),
    market: String(row.market || "").trim(),
    overOdds: num(row.over_odds, NaN),
    underOdds: num(row.under_odds, NaN),
    modelTotal: num(row.model_total_score, NaN),
    modelBirdies: num(row.model_birdies, NaN),
    modelPars: num(row.model_pars, NaN),
    modelBogeys: num(row.model_bogeys, NaN),
    modelGir: num(row.model_gir, NaN),
    modelFairways: num(row.model_fairways, NaN),
  };
}

/**
 * @param {string} lineCol e.g. pp_line / sl_line / ud_line
 */
function normalizePickemAuditRow(row, lineCol) {
  const rawLine = row[lineCol] ?? row.pp_line ?? row.dk_line;
  const aliased = { ...row, dk_line: rawLine };
  const norm = normalizeAuditRow(aliased);
  if (norm !== aliased && norm[lineCol] == null && norm.dk_line != null) {
    return { ...norm, [lineCol]: norm.dk_line };
  }
  return norm;
}

function ingestPickemAuditRow(best, row, roundStartUtcMs, lineCol) {
  const norm = normalizePickemAuditRow(row, lineCol);
  const dg = Math.round(num(norm.dg_id));
  const market = String(norm.market || "").trim();
  if (!Number.isFinite(dg) || !market) return;

  const capturedMs = Date.parse(String(norm.captured_at || "").trim());
  const propRound = auditPropRoundFromCapture(norm, roundStartUtcMs, capturedMs);
  if (!Number.isFinite(propRound) || propRound < 1 || propRound > 4) return;

  const line = num(norm[lineCol] ?? norm.pp_line ?? norm.dk_line, NaN);
  const over = num(norm.over_odds, NaN);
  const under = num(norm.under_odds, NaN);
  if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) return;

  const key = `${dg}|${propRound}|${market}`;
  const prev = best.get(key);
  const snap = snapFromAuditRow(norm, capturedMs);
  if (!prev) {
    best.set(key, {
      line,
      over,
      under,
      capturedMs,
      ...snap,
      openLine: line,
      openOver: over,
      openUnder: under,
      openCapturedMs: capturedMs,
      displayRound: propRound,
    });
    return;
  }
  const next = { ...prev };
  if (!Number.isFinite(prev.openCapturedMs) || capturedMs < prev.openCapturedMs) {
    next.openLine = line;
    next.openOver = over;
    next.openUnder = under;
    next.openCapturedMs = capturedMs;
  }
  if (!Number.isFinite(prev.capturedMs) || capturedMs > prev.capturedMs) {
    Object.assign(next, {
      line,
      over,
      under,
      capturedMs,
      ...snap,
      displayRound: propRound,
      openLine: next.openLine,
      openOver: next.openOver,
      openUnder: next.openUnder,
      openCapturedMs: next.openCapturedMs,
    });
  }
  best.set(key, next);
}

/**
 * @param {string} eventName
 * @param {string} csvText
 * @param {Map<number, number>} roundStartUtcMs
 * @param {{ lineCol?: string }} [opts]
 */
export async function loadPreRoundPickemPropsFromAuditText(eventName, csvText, roundStartUtcMs, opts = {}) {
  const best = new Map();
  const lineCol = String(opts.lineCol || "pp_line").trim() || "pp_line";
  if (!eventName || !String(csvText || "").trim()) return best;
  await new Promise((resolve, reject) => {
    Readable.from([csvText])
      .pipe(
        parse({
          columns: true,
          relax_quotes: true,
          relax_column_count: true,
          skip_records_with_error: true,
        }),
      )
      .on("data", (row) => {
        const ev = String(row.event_name || "").trim();
        if (!eventsLikelySame(eventName, ev)) return;
        ingestPickemAuditRow(best, row, roundStartUtcMs, lineCol);
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return best;
}

/**
 * @param {string} eventName
 * @param {string} auditPath
 * @param {Map<number, number>} roundStartUtcMs
 * @param {{ lineCol?: string }} [opts]
 */
export async function loadPreRoundPickemPropsFromAudit(eventName, auditPath, roundStartUtcMs, opts = {}) {
  const best = new Map();
  const lineCol = String(opts.lineCol || "pp_line").trim() || "pp_line";
  if (!eventName || !existsSync(auditPath)) return best;

  const parser = createReadStream(auditPath).pipe(
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
    ingestPickemAuditRow(best, row, roundStartUtcMs, lineCol);
  }
  return best;
}

export function defaultPickemAuditPath(webRoot, short) {
  return join(webRoot, "data", `${short}_round_projection_audit.csv`);
}

export function defaultPpAuditPath(webRoot) {
  return defaultPickemAuditPath(webRoot, "pp");
}

export function defaultSlAuditPath(webRoot) {
  return defaultPickemAuditPath(webRoot, "sl");
}

export function defaultUdAuditPath(webRoot) {
  return defaultPickemAuditPath(webRoot, "ud");
}

export function defaultFdAuditPath(webRoot) {
  return defaultPickemAuditPath(webRoot, "fd");
}

export function defaultCzrAuditPath(webRoot) {
  return defaultPickemAuditPath(webRoot, "czr");
}

export function defaultKlAuditPath(webRoot) {
  return defaultPickemAuditPath(webRoot, "kl");
}

export async function loadPreRoundPpPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  return loadPreRoundPickemPropsFromAudit(eventName, auditPath, roundStartUtcMs, { lineCol: "pp_line" });
}

export async function loadPreRoundPpPropsFromAuditText(eventName, csvText, roundStartUtcMs) {
  return loadPreRoundPickemPropsFromAuditText(eventName, csvText, roundStartUtcMs, { lineCol: "pp_line" });
}

export async function loadPreRoundSlPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  return loadPreRoundPickemPropsFromAudit(eventName, auditPath, roundStartUtcMs, { lineCol: "sl_line" });
}

export async function loadPreRoundUdPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  return loadPreRoundPickemPropsFromAudit(eventName, auditPath, roundStartUtcMs, { lineCol: "ud_line" });
}

export async function loadPreRoundFdPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  return loadPreRoundPickemPropsFromAudit(eventName, auditPath, roundStartUtcMs, { lineCol: "fd_line" });
}

export async function loadPreRoundCzrPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  return loadPreRoundPickemPropsFromAudit(eventName, auditPath, roundStartUtcMs, { lineCol: "czr_line" });
}

export async function loadPreRoundKlPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  return loadPreRoundPickemPropsFromAudit(eventName, auditPath, roundStartUtcMs, { lineCol: "kl_line" });
}
