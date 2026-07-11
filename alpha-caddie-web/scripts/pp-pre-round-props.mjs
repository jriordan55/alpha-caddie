/**
 * PrizePicks round props from pp_round_projection_audit.csv — same pre-round tee-window
 * rules as dk-pre-round-props.mjs.
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

function snapFromPpAuditRow(row, capturedMs) {
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

/** PP audit rows use pp_line; reuse DK normalize by aliasing for shifted legacy rows. */
function normalizePpAuditRow(row) {
  const aliased = { ...row, dk_line: row.pp_line ?? row.dk_line };
  const norm = normalizeAuditRow(aliased);
  if (norm !== aliased && norm.pp_line == null && norm.dk_line != null) {
    return { ...norm, pp_line: norm.dk_line };
  }
  return norm;
}

function ingestPpAuditRow(best, row, roundStartUtcMs) {
  const norm = normalizePpAuditRow(row);
  const dg = Math.round(num(norm.dg_id));
  const market = String(norm.market || "").trim();
  if (!Number.isFinite(dg) || !market) return;

  const capturedMs = Date.parse(String(norm.captured_at || "").trim());
  const propRound = auditPropRoundFromCapture(norm, roundStartUtcMs, capturedMs);
  if (!Number.isFinite(propRound) || propRound < 1 || propRound > 4) return;

  const line = num(norm.pp_line, NaN);
  const over = num(norm.over_odds, NaN);
  const under = num(norm.under_odds, NaN);
  if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) return;

  const key = `${dg}|${propRound}|${market}`;
  const prev = best.get(key);
  const snap = snapFromPpAuditRow(norm, capturedMs);
  if (!prev || capturedMs > prev.capturedMs) {
    best.set(key, {
      line,
      over,
      under,
      capturedMs,
      ...snap,
      displayRound: propRound,
    });
  }
}

export async function loadPreRoundPpPropsFromAuditText(eventName, csvText, roundStartUtcMs) {
  const best = new Map();
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
        ingestPpAuditRow(best, row, roundStartUtcMs);
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return best;
}

export async function loadPreRoundPpPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  const best = new Map();
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
    ingestPpAuditRow(best, row, roundStartUtcMs);
  }
  return best;
}

export function defaultPpAuditPath(webRoot) {
  return join(webRoot, "data", "pp_round_projection_audit.csv");
}
