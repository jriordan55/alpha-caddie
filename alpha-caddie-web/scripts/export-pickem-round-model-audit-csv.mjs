#!/usr/bin/env node
/**
 * Append audit CSV rows for a pick'em book source (sleeper / underdog).
 *   Used by fetch-book-odds after merge.
 */
import { appendFileSync, existsSync, mkdirSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { formatCourseLabelForDisplay } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function num(v, d = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : d;
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

function displayRoundFromPayload(payload) {
  const r = Math.round(num(payload?.display_round, NaN));
  if (Number.isFinite(r) && r >= 1 && r <= 4) return r;
  return 1;
}

function playerRowForRound(players, dgId, rnd) {
  const id = Math.round(num(dgId, NaN));
  const r = Math.round(num(rnd, NaN));
  if (!Number.isFinite(id) || !Number.isFinite(r) || r < 1 || r > 4) return null;
  for (const p of players || []) {
    if (Math.round(num(p?.dg_id, NaN)) !== id) continue;
    if (Math.round(num(p?.round, NaN)) !== r) continue;
    return p;
  }
  return null;
}

/**
 * @param {object} payload
 * @param {{ source: string, outPath?: string, skipEnv?: string, refreshedAtKey?: string, lineCol?: string }} opts
 */
export function appendPickemRoundProjectionAuditCsv(payload, opts) {
  const source = String(opts.source || "").trim().toLowerCase();
  const skipEnv = opts.skipEnv || "";
  if (skipEnv && String(process.env[skipEnv] || "").trim() === "1") {
    return { appended: 0, path: opts.outPath || "", skipped: true };
  }
  const short = source === "underdog" ? "ud" : source === "sleeper" ? "sl" : source.slice(0, 2);
  const outPath = opts.outPath || join(WEB_ROOT, "data", `${short}_round_projection_audit.csv`);
  const props = Array.isArray(payload?.props) ? payload.props : [];
  const rows = props.filter((r) => String(r?.source || "").trim().toLowerCase() === source);
  if (!rows.length) return { appended: 0, path: outPath };

  const lineCol = opts.lineCol || `${short}_line`;
  const refreshedKey = opts.refreshedAtKey || `${short}_round_props_refreshed_at`;
  const header =
    `captured_at,${refreshedKey},projections_updated_at,event_name,course_used,display_round,round_num,dg_id,player_name,market,${lineCol},over_odds,under_odds,p_over_implied,p_under_implied,odds_method,model_total_score,model_birdies,model_pars,model_bogeys,model_gir,model_fairways,model_putts\n`;

  const displayRnd = displayRoundFromPayload(payload);
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const captured = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const bookAt =
    String(payload?.[refreshedKey] || payload?.meta?.[refreshedKey] || "").trim() ||
    String(payload?.book_odds_refreshed_at || "").trim() ||
    captured;
  const projAt = String(payload?.updated_at || "").trim() || captured;
  const event = String(payload?.event_name || "").trim();
  const course = formatCourseLabelForDisplay(String(payload?.course_used || "").trim());

  mkdirSync(dirname(outPath), { recursive: true });
  if (!existsSync(outPath) || readFileSync(outPath, "utf8").trim() === "") {
    appendFileSync(outPath, header, "utf8");
  }

  let appended = 0;
  const lines = [];
  for (const pr of rows) {
    const dgId = Math.round(num(pr?.dg_id, NaN));
    const propRound = Math.round(num(pr?.round_num, NaN));
    const rnd =
      Number.isFinite(propRound) && propRound >= 1 && propRound <= 4 ? propRound : displayRnd;
    const pl = Number.isFinite(dgId) ? playerRowForRound(players, dgId, rnd) : null;
    const oddsMethod =
      String(pr?.ud_odds_method || pr?.sl_odds_method || pr?.pp_odds_method || "").trim();
    const row = [
      captured,
      bookAt,
      projAt,
      event,
      course,
      displayRnd,
      rnd,
      Number.isFinite(dgId) ? dgId : "",
      String(pr?.player_name || "").trim(),
      String(pr?.market || "").trim(),
      num(pr?.line, ""),
      num(pr?.over_odds, ""),
      num(pr?.under_odds, ""),
      num(pr?.p_over_implied, ""),
      num(pr?.p_under_implied, ""),
      oddsMethod,
      pl ? num(pl.total_score, "") : "",
      pl ? num(pl.birdies, "") : "",
      pl ? num(pl.pars, "") : "",
      pl ? num(pl.bogeys, "") : "",
      pl ? num(pl.gir, "") : "",
      pl ? num(pl.fairways, "") : "",
      pl ? num(pl.putts, "") : "",
    ];
    lines.push(row.map(csvCell).join(",") + "\n");
    appended++;
  }
  if (lines.length) appendFileSync(outPath, lines.join(""), "utf8");
  return { appended, path: outPath };
}

export function appendUdRoundProjectionAuditCsv(payload, opts = {}) {
  return appendPickemRoundProjectionAuditCsv(payload, {
    source: "underdog",
    skipEnv: "GOLF_SKIP_UD_ROUND_AUDIT_CSV",
    refreshedAtKey: "ud_round_props_refreshed_at",
    lineCol: "ud_line",
    ...opts,
  });
}

export function appendSlRoundProjectionAuditCsv(payload, opts = {}) {
  return appendPickemRoundProjectionAuditCsv(payload, {
    source: "sleeper",
    skipEnv: "GOLF_SKIP_SL_ROUND_AUDIT_CSV",
    refreshedAtKey: "sl_round_props_refreshed_at",
    lineCol: "sl_line",
    ...opts,
  });
}

function main() {
  const source = String(process.argv[2] || "underdog").trim().toLowerCase();
  const projPath = process.env.GOLF_PROJECTIONS_JSON?.trim()
    ? resolve(process.env.GOLF_PROJECTIONS_JSON.trim())
    : join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("Missing projections file:", projPath);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  const hit =
    source === "sleeper"
      ? appendSlRoundProjectionAuditCsv(payload)
      : appendUdRoundProjectionAuditCsv(payload);
  console.log(`[${source}-round-audit] appended ${hit.appended} row(s) -> ${hit.path}`);
}

const isMain =
  Boolean(process.argv[1]) &&
  resolve(fileURLToPath(import.meta.url)) === resolve(process.argv[1]);
if (isMain) main();
