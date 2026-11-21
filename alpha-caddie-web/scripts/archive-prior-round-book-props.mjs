/**
 * Keep each round's sportsbook O/U lines stored with that round's projection rows.
 *
 * - `props` holds ALL round-tagged book lines (R1/R2/R3…) — the UI filters by exact round.
 * - `props_by_round` is a durable snapshot `{ "2": [...], "3": [...] }` so a later scrape
 *   cannot orphan yesterday's lines from yesterday's μ.
 * - Completed rounds (round < display_round) get `projection_round_locked` on player rows
 *   so hierarchical μ / weather never rewrite them.
 *
 *   node scripts/archive-prior-round-book-props.mjs
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath, pathToFileURL } from "url";
import { projectionExportMeta } from "./projection-export-meta.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

const SPORTSBOOK_SOURCES = new Set([
  "draftkings",
  "prizepicks",
  "sleeper",
  "underdog",
  "fanduel",
  "kalshi",
  "caesars",
  "onyx",
  "courtside",
  "betmgm",
  "bet365",
]);

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function propRound(row) {
  return Math.round(num(row?.round_num ?? row?.round, NaN));
}

function propKey(row) {
  const src = String(row?.source || "")
    .trim()
    .toLowerCase();
  const rnd = propRound(row);
  const dg = Math.round(num(row?.dg_id, NaN));
  const name = String(row?.player_name || "")
    .trim()
    .toLowerCase();
  const mkt = String(row?.market || "")
    .trim()
    .toLowerCase();
  const line = num(row?.line, NaN);
  return `${src}|${rnd}|${Number.isFinite(dg) ? dg : name}|${mkt}|${Number.isFinite(line) ? line : ""}`;
}

function mergePropLists(...lists) {
  const out = [];
  const seen = new Set();
  for (const list of lists) {
    if (!Array.isArray(list)) continue;
    for (const row of list) {
      if (!row || typeof row !== "object") continue;
      const k = propKey(row);
      if (seen.has(k)) continue;
      seen.add(k);
      out.push(row);
    }
  }
  return out;
}

function buildPropsByRound(props) {
  /** @type {Record<string, object[]>} */
  const by = {};
  for (const row of props || []) {
    const rnd = propRound(row);
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
    const key = String(rnd);
    if (!by[key]) by[key] = [];
    by[key].push(row);
  }
  return by;
}

/**
 * @param {object} proj
 * @returns {{ displayRound: number; props: number; byRound: Record<string, number>; lockedPlayers: number }}
 */
export function archivePriorRoundBookProps(proj) {
  if (!proj || typeof proj !== "object") {
    return { displayRound: NaN, props: 0, byRound: {}, lockedPlayers: 0 };
  }
  const meta = projectionExportMeta(proj);
  const displayRound = Math.round(
    num(meta.display_round ?? proj.display_round ?? meta.datagolf_live_current_round, NaN),
  );

  // Restore anything previously stripped into props_prior_rounds, then keep everything in props.
  const existingByRound = proj.props_by_round && typeof proj.props_by_round === "object" ? proj.props_by_round : {};
  const fromByRound = Object.values(existingByRound).flatMap((x) => (Array.isArray(x) ? x : []));
  const merged = mergePropLists(proj.props, proj.props_prior_rounds, fromByRound);

  // Prefer the durable snapshot for completed rounds when a newer scrape dropped them.
  if (Number.isFinite(displayRound) && displayRound >= 2) {
    for (let r = 1; r < displayRound; r++) {
      const snap = existingByRound[String(r)];
      if (Array.isArray(snap) && snap.length) {
        // already merged via fromByRound
      }
    }
  }

  proj.props = merged;
  const byRound = buildPropsByRound(merged);
  // Never shrink a completed round's snapshot — only grow / refresh.
  for (const [k, rows] of Object.entries(byRound)) {
    const rnd = Math.round(num(k, NaN));
    const prev = Array.isArray(existingByRound[k]) ? existingByRound[k] : [];
    if (Number.isFinite(displayRound) && Number.isFinite(rnd) && rnd < displayRound && prev.length > rows.length) {
      byRound[k] = mergePropLists(prev, rows);
    }
  }
  proj.props_by_round = byRound;
  // Keep a flat prior list for older consumers, but do not remove from props.
  if (Number.isFinite(displayRound) && displayRound >= 2) {
    proj.props_prior_rounds = merged.filter((row) => {
      const src = String(row.source || "")
        .trim()
        .toLowerCase();
      const pr = propRound(row);
      return SPORTSBOOK_SOURCES.has(src) && Number.isFinite(pr) && pr < displayRound;
    });
  }

  let lockedPlayers = 0;
  const lockedAt = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  if (Number.isFinite(displayRound) && displayRound >= 2 && Array.isArray(proj.players)) {
    for (const p of proj.players) {
      if (!p || typeof p !== "object") continue;
      const rnd = Math.round(num(p.round, NaN));
      if (!Number.isFinite(rnd) || rnd < 1) continue;
      if (rnd < displayRound) {
        if (!p.projection_round_locked) lockedPlayers++;
        p.projection_round_locked = true;
        p.projection_round_locked_at = p.projection_round_locked_at || lockedAt;
      }
    }
  }

  meta.props_by_round_updated_at = lockedAt;
  if (Number.isFinite(displayRound)) meta.props_by_round_display_round = displayRound;
  if (proj.meta && typeof proj.meta === "object") {
    proj.meta.props_by_round_updated_at = lockedAt;
    if (Number.isFinite(displayRound)) proj.meta.props_by_round_display_round = displayRound;
  }

  const counts = Object.fromEntries(Object.entries(byRound).map(([k, v]) => [k, v.length]));
  return {
    displayRound,
    props: merged.length,
    byRound: counts,
    lockedPlayers,
  };
}

function main() {
  const file = String(process.env.GOLF_PROJECTIONS_FILE || "projections.json").trim() || "projections.json";
  const path = join(WEB_ROOT, file);
  if (!existsSync(path)) {
    console.warn(`[archive-prior-props] missing ${file} — skip`);
    process.exit(0);
  }
  const proj = JSON.parse(readFileSync(path, "utf8"));
  const r = archivePriorRoundBookProps(proj);
  writeFileSync(path, `${JSON.stringify(proj, null, 2)}\n`, "utf8");
  console.log(
    `[archive-prior-props] display_round=${r.displayRound} props=${r.props} by_round=${JSON.stringify(r.byRound)} locked_players=${r.lockedPlayers} → ${file}`,
  );
}

const isDirect = process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href;
if (isDirect) main();
