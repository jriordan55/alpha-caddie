/**
 * Load strokes-gained pillars from the local Betsperts Rabbit Hole DB
 * (data/betsperts/players/by-id) keyed by DataGolf dg_id via pga_datagolf_player_map.
 */
import { existsSync, readFileSync, readdirSync } from "fs";
import { join, resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse/sync";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");

const SG_KEYS = ["sg_total", "sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_t2g"];

function num(v, fallback = NaN) {
  if (v == null || v === "") return fallback;
  const n = Number(String(v).replace(/,/g, "").trim());
  return Number.isFinite(n) ? n : fallback;
}

function round3(x) {
  return Math.round(x * 1000) / 1000;
}

function sgFromSummaryRow(row) {
  if (!row || typeof row !== "object") return null;
  const sg_total = num(row["SG:TOT"] ?? row.sg_total, NaN);
  const sg_ott = num(row["SG:OTT"] ?? row.sg_ott, NaN);
  const sg_app = num(row["SG:APP"] ?? row.sg_app, NaN);
  const sg_arg = num(row["SG:ARG"] ?? row.sg_arg, NaN);
  const sg_putt = num(row["SG:P"] ?? row.sg_putt, NaN);
  let sg_t2g = num(row["SG:T2G"] ?? row.sg_t2g, NaN);
  if (!Number.isFinite(sg_t2g) && [sg_ott, sg_app, sg_arg].every(Number.isFinite)) {
    sg_t2g = sg_ott + sg_app + sg_arg;
  }
  if (![sg_total, sg_ott, sg_app, sg_arg, sg_putt, sg_t2g].some(Number.isFinite)) return null;
  const out = {};
  if (Number.isFinite(sg_total)) out.sg_total = round3(sg_total);
  if (Number.isFinite(sg_ott)) out.sg_ott = round3(sg_ott);
  if (Number.isFinite(sg_app)) out.sg_app = round3(sg_app);
  if (Number.isFinite(sg_arg)) out.sg_arg = round3(sg_arg);
  if (Number.isFinite(sg_putt)) out.sg_putt = round3(sg_putt);
  if (Number.isFinite(sg_t2g)) out.sg_t2g = round3(sg_t2g);
  return out;
}

/** Prefer current/latest season year with SG:TOT; require at least one pillar when possible. */
function pickBestSgSummary(sgSummary) {
  if (!Array.isArray(sgSummary) || !sgSummary.length) return null;
  const scored = [];
  for (const row of sgSummary) {
    const pillars = sgFromSummaryRow(row);
    if (!pillars || !Number.isFinite(pillars.sg_total)) continue;
    const year = num(row.Year ?? row.year, 0);
    const rounds = num(row.Rounds ?? row.rounds, 0);
    const hasPillars = ["sg_ott", "sg_app", "sg_arg", "sg_putt"].every((k) => Number.isFinite(pillars[k]));
    scored.push({ year, rounds, hasPillars, pillars, row });
  }
  if (!scored.length) return null;
  scored.sort((a, b) => {
    if (a.hasPillars !== b.hasPillars) return a.hasPillars ? -1 : 1;
    if (a.year !== b.year) return b.year - a.year;
    return b.rounds - a.rounds;
  });
  return scored[0];
}

function loadPgaDgMap(mapPath = join(ROOT, "data", "pga_datagolf_player_map.csv")) {
  const pgaToDg = new Map();
  const dgToPga = new Map();
  if (!existsSync(mapPath)) return { pgaToDg, dgToPga };
  const rows = parse(readFileSync(mapPath, "utf8"), {
    columns: true,
    skip_empty_lines: true,
    relax_column_count: true,
    bom: true,
  });
  for (const r of rows) {
    const pga = String(r.pga_player_id || "").replace(/\D/g, "");
    const dg = Math.round(num(r.dg_id, NaN));
    if (!pga || !Number.isFinite(dg)) continue;
    pgaToDg.set(pga, dg);
    if (!dgToPga.has(dg)) dgToPga.set(dg, pga);
  }
  return { pgaToDg, dgToPga };
}

/**
 * @returns {Map<number, { sg_total?: number, sg_ott?: number, sg_app?: number, sg_arg?: number, sg_putt?: number, sg_t2g?: number, pga_id: string, year: number, rounds: number, source: string }>}
 */
export function loadBetspertsSgByDg(opts = {}) {
  const byIdDir = opts.byIdDir || join(ROOT, "data", "betsperts", "players", "by-id");
  const mapPath = opts.mapPath || join(ROOT, "data", "pga_datagolf_player_map.csv");
  const out = new Map();
  if (!existsSync(byIdDir)) return out;

  const { pgaToDg } = loadPgaDgMap(mapPath);
  let files;
  try {
    files = readdirSync(byIdDir).filter((f) => f.endsWith(".json"));
  } catch {
    return out;
  }

  for (const f of files) {
    const pgaId = f.replace(/\.json$/i, "").replace(/\D/g, "");
    if (!pgaId) continue;
    const dg = pgaToDg.get(pgaId);
    if (!Number.isFinite(dg)) continue;
    let raw;
    try {
      raw = JSON.parse(readFileSync(join(byIdDir, f), "utf8"));
    } catch {
      continue;
    }
    const best = pickBestSgSummary(raw?.stats?.sgSummary);
    if (!best) continue;
    out.set(dg, {
      ...best.pillars,
      pga_id: pgaId,
      year: best.year,
      rounds: best.rounds,
      source: "betsperts_sgSummary",
      player_name: raw.playerName || best.row?.playerName || undefined,
    });
  }
  return out;
}

/** Overlay Betsperts SG pillars onto a skillByDg Map (mutates values in place). */
export function applyBetspertsSgToSkillByDg(skillByDg, betspertsByDg = null) {
  const src = betspertsByDg || loadBetspertsSgByDg();
  let n = 0;
  for (const [dg, bp] of src) {
    const prev = skillByDg.get(dg) || { dg_id: dg };
    const next = { ...prev };
    for (const k of SG_KEYS) {
      if (Number.isFinite(bp[k])) next[k] = bp[k];
    }
    next.sg_source = "betsperts";
    skillByDg.set(dg, next);
    n++;
  }
  return { applied: n, available: src.size };
}

/** Patch projection player rows' sg_* (and optionally mu_sg) from Betsperts. */
export function applyBetspertsSgToPlayers(players, opts = {}) {
  const src = opts.betspertsByDg || loadBetspertsSgByDg();
  const updateMu = opts.updateMu !== false;
  let n = 0;
  for (const pl of players || []) {
    const dg = Math.round(num(pl.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const bp = src.get(dg);
    if (!bp) continue;
    for (const k of SG_KEYS) {
      if (Number.isFinite(bp[k])) pl[k] = bp[k];
    }
    pl.sg_source = "betsperts";
    if (updateMu && Number.isFinite(bp.sg_total)) {
      pl.mu_sg = bp.sg_total;
      pl.implied_mu_sg = bp.sg_total;
    }
    n++;
  }
  return { patched: n, available: src.size };
}

export { SG_KEYS, pickBestSgSummary, sgFromSummaryRow, loadPgaDgMap, WEB, ROOT };
