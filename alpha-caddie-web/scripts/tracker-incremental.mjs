/**
 * Shared incremental watermark for projection-tracker + matchup-tracker.
 * Reads the last recorded date from a detail CSV so push:live only refreshes
 * from that point forward (plus a small overlap), keeping older rows cached.
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
      continue;
    }
    if (ch === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  out.push(cur);
  return out;
}

function isoDateFromRaw(raw) {
  const s = String(raw || "").trim();
  const m = s.match(/^(\d{4}-\d{2}-\d{2})/);
  return m ? m[1] : "";
}

function shiftIsoDays(iso, days) {
  const t = Date.parse(`${iso}T00:00:00Z`);
  if (!Number.isFinite(t)) return iso;
  const d = new Date(t);
  d.setUTCDate(d.getUTCDate() + days);
  return d.toISOString().slice(0, 10);
}

/**
 * @param {{
 *   detailPath: string,
 *   envVar?: string,
 *   dateColumns?: string[],
 *   overlapDays?: number,
 *   fallbackDays?: number,
 * }} opts
 * @returns {{ sinceIso: string, lastRecordedIso: string|null, source: string }}
 */
export function resolveTrackerIncrementalSinceIso(opts) {
  const envVar = String(opts.envVar || "").trim();
  if (envVar) {
    const envSince = String(process.env[envVar] || "").trim();
    if (envSince) {
      return { sinceIso: envSince, lastRecordedIso: null, source: `env:${envVar}` };
    }
  }

  const detailPath = opts.detailPath;
  const dateColumns = Array.isArray(opts.dateColumns) && opts.dateColumns.length
    ? opts.dateColumns
    : ["close_time", "exported_at"];
  const overlapDays = Number.isFinite(Number(opts.overlapDays)) ? Number(opts.overlapDays) : 2;
  const fallbackDays = Number.isFinite(Number(opts.fallbackDays)) ? Number(opts.fallbackDays) : 14;

  if (!detailPath || !existsSync(detailPath)) {
    const d = new Date();
    d.setUTCDate(d.getUTCDate() - fallbackDays);
    return {
      sinceIso: d.toISOString().slice(0, 10),
      lastRecordedIso: null,
      source: "fallback:no_detail_csv",
    };
  }

  const text = readFileSync(detailPath, "utf8");
  const lines = text.split(/\r?\n/).filter(Boolean);
  if (lines.length < 2) {
    const d = new Date();
    d.setUTCDate(d.getUTCDate() - fallbackDays);
    return {
      sinceIso: d.toISOString().slice(0, 10),
      lastRecordedIso: null,
      source: "fallback:empty_detail",
    };
  }

  const header = parseCsvLine(lines[0]);
  /** @type {Record<string, string>} */
  const maxByCol = {};
  for (const col of dateColumns) maxByCol[col] = "";

  for (let i = 1; i < lines.length; i++) {
    const cols = parseCsvLine(lines[i]);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cols[j] ?? "";
    for (const col of dateColumns) {
      const iso = isoDateFromRaw(row[col] || "");
      if (iso && iso > (maxByCol[col] || "")) maxByCol[col] = iso;
    }
  }

  let lastRecordedIso = null;
  let sourceCol = "";
  for (const col of dateColumns) {
    if (maxByCol[col]) {
      lastRecordedIso = maxByCol[col];
      sourceCol = col;
      break;
    }
  }

  if (!lastRecordedIso) {
    const d = new Date();
    d.setUTCDate(d.getUTCDate() - fallbackDays);
    return {
      sinceIso: d.toISOString().slice(0, 10),
      lastRecordedIso: null,
      source: "fallback:empty_detail",
    };
  }

  return {
    sinceIso: shiftIsoDays(lastRecordedIso, -Math.abs(overlapDays)),
    lastRecordedIso,
    source: `detail:${sourceCol}`,
  };
}

export function resolveMatchupIncrementalSinceIso(opts = {}) {
  return resolveTrackerIncrementalSinceIso({
    detailPath: opts.detailPath || join(WEB_ROOT, "data", "matchup_backtest_detail.csv"),
    envVar: "GOLF_MATCHUP_BACKTEST_SINCE",
    dateColumns: ["close_time", "exported_at"],
    overlapDays: opts.overlapDays,
    fallbackDays: opts.fallbackDays,
  });
}

export function resolveOuIncrementalSinceIso(opts = {}) {
  return resolveTrackerIncrementalSinceIso({
    detailPath: opts.detailPath || join(WEB_ROOT, "data", "round_projection_vs_actual.csv"),
    envVar: "GOLF_OU_BACKTEST_SINCE",
    dateColumns: ["exported_at", "projections_updated_at"],
    overlapDays: opts.overlapDays,
    fallbackDays: opts.fallbackDays,
  });
}
