/**
 * Incremental watermark for matchup-tracker on push:live.
 * Reads the last recorded close/export date from matchup_backtest_detail.csv
 * so we only refresh DG odds + reprice from that point (plus a small overlap).
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const DEFAULT_DETAIL = join(WEB_ROOT, "data", "matchup_backtest_detail.csv");

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
 * @param {{ detailPath?: string, overlapDays?: number, fallbackDays?: number }} [opts]
 * @returns {{ sinceIso: string, lastRecordedIso: string|null, source: string }}
 */
export function resolveMatchupIncrementalSinceIso(opts = {}) {
  const envSince = String(process.env.GOLF_MATCHUP_BACKTEST_SINCE || "").trim();
  if (envSince) {
    return { sinceIso: envSince, lastRecordedIso: null, source: "env:GOLF_MATCHUP_BACKTEST_SINCE" };
  }

  const detailPath = opts.detailPath || DEFAULT_DETAIL;
  const overlapDays = Number.isFinite(Number(opts.overlapDays)) ? Number(opts.overlapDays) : 2;
  const fallbackDays = Number.isFinite(Number(opts.fallbackDays)) ? Number(opts.fallbackDays) : 14;

  if (!existsSync(detailPath)) {
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
  let maxClose = "";
  let maxExported = "";
  for (let i = 1; i < lines.length; i++) {
    const cols = parseCsvLine(lines[i]);
    const row = {};
    for (let j = 0; j < header.length; j++) row[header[j]] = cols[j] ?? "";
    const close = isoDateFromRaw(row.close_time || row.close_date || "");
    if (close && close > maxClose) maxClose = close;
    const exp = isoDateFromRaw(row.exported_at || "");
    if (exp && exp > maxExported) maxExported = exp;
  }

  const lastRecordedIso = maxClose || maxExported || null;
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
    source: maxClose ? "detail:close_time" : "detail:exported_at",
  };
}
