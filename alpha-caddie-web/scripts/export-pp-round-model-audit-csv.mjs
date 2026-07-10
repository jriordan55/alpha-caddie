#!/usr/bin/env node
/**
 * After PrizePicks round O/U lines are merged into projections.json, append one CSV row per PP prop.
 *
 * Called from fetch-book-odds-into-projections.mjs when PP props exist. Standalone:
 *   npm run export:pp-round-audit-csv
 *
 * Output: alpha-caddie-web/data/pp_round_projection_audit.csv (append-only log).
 * Skip: GOLF_SKIP_PP_ROUND_AUDIT_CSV=1
 */
import { appendFileSync, existsSync, mkdirSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { formatCourseLabelForDisplay } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const DEFAULT_OUT = join(WEB_ROOT, "data", "pp_round_projection_audit.csv");

const HEADER =
  "captured_at,pp_round_props_refreshed_at,projections_updated_at,event_name,course_used,display_round,round_num,dg_id,player_name,market,pp_line,over_odds,under_odds,p_over_implied,p_under_implied,pp_odds_method,model_total_score,model_birdies,model_pars,model_bogeys,model_gir,model_fairways,model_putts\n";

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
 * @param {object} payload — full projections after PP merge
 * @param {{ outPath?: string }} [opts]
 */
export function appendPpRoundProjectionAuditCsv(payload, opts = {}) {
  if (String(process.env.GOLF_SKIP_PP_ROUND_AUDIT_CSV || "").trim() === "1") {
    return { appended: 0, path: opts.outPath || DEFAULT_OUT, skipped: true };
  }
  const outPath = opts.outPath || DEFAULT_OUT;
  const props = Array.isArray(payload?.props) ? payload.props : [];
  const ppRows = props.filter((r) => String(r?.source || "").trim().toLowerCase() === "prizepicks");
  if (!ppRows.length) {
    return { appended: 0, path: outPath };
  }

  const displayRnd = displayRoundFromPayload(payload);
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const captured = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const ppAt =
    String(payload?.pp_round_props_refreshed_at || payload?.meta?.pp_round_props_refreshed_at || "").trim() ||
    String(payload?.book_odds_refreshed_at || "").trim() ||
    captured;
  const projAt = String(payload?.updated_at || "").trim() || captured;
  const event = String(payload?.event_name || "").trim();
  const course = formatCourseLabelForDisplay(String(payload?.course_used || "").trim());

  mkdirSync(dirname(outPath), { recursive: true });
  if (!existsSync(outPath) || readFileSync(outPath, "utf8").trim() === "") {
    appendFileSync(outPath, HEADER, "utf8");
  }

  let appended = 0;
  const lines = [];
  for (const pr of ppRows) {
    const dgId = Math.round(num(pr?.dg_id, NaN));
    const propRound = Math.round(num(pr?.round_num, NaN));
    const rnd =
      Number.isFinite(propRound) && propRound >= 1 && propRound <= 4 ? propRound : displayRnd;
    const pl = Number.isFinite(dgId) ? playerRowForRound(players, dgId, rnd) : null;
    const row = [
      captured,
      ppAt,
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
      String(pr?.pp_odds_method || "").trim(),
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

function main() {
  const projPath = process.env.GOLF_PROJECTIONS_JSON?.trim()
    ? resolve(process.env.GOLF_PROJECTIONS_JSON.trim())
    : join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("Missing projections file:", projPath);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  const { appended, path } = appendPpRoundProjectionAuditCsv(payload);
  console.log(`[pp-round-audit] appended ${appended} row(s) -> ${path}`);
}

const isMain =
  Boolean(process.argv[1]) &&
  resolve(fileURLToPath(import.meta.url)) === resolve(process.argv[1]);
if (isMain) main();
