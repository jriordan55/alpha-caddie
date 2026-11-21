/**
 * Keep sportsbook O/U props for prior rounds out of the active `props` array once
 * display_round has moved on — prevents Round projections from pairing yesterday's
 * R(n-1) lines with today's R(n) model μ. Prior rows are retained in
 * `props_prior_rounds` for audit / tracker rebuilds.
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

/**
 * @param {object} proj
 * @returns {{ archived: number; kept: number; displayRound: number }}
 */
export function archivePriorRoundBookProps(proj) {
  if (!proj || typeof proj !== "object") {
    return { archived: 0, kept: 0, displayRound: NaN };
  }
  const meta = projectionExportMeta(proj);
  const displayRound = Math.round(
    num(meta.display_round ?? proj.display_round ?? meta.datagolf_live_current_round, NaN),
  );
  if (!Number.isFinite(displayRound) || displayRound < 2) {
    return { archived: 0, kept: Array.isArray(proj.props) ? proj.props.length : 0, displayRound };
  }

  const props = Array.isArray(proj.props) ? proj.props : [];
  const prior = Array.isArray(proj.props_prior_rounds) ? [...proj.props_prior_rounds] : [];
  const keep = [];
  let archived = 0;

  for (const row of props) {
    if (!row || typeof row !== "object") continue;
    const src = String(row.source || "")
      .trim()
      .toLowerCase();
    const pr = Math.round(num(row.round_num ?? row.round, NaN));
    if (SPORTSBOOK_SOURCES.has(src) && Number.isFinite(pr) && pr >= 1 && pr < displayRound) {
      prior.push(row);
      archived++;
      continue;
    }
    keep.push(row);
  }

  proj.props = keep;
  proj.props_prior_rounds = prior;
  meta.props_prior_rounds_archived_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  meta.props_prior_rounds_for_display_round = displayRound;
  if (proj.meta && typeof proj.meta === "object") {
    proj.meta.props_prior_rounds_archived_at = meta.props_prior_rounds_archived_at;
    proj.meta.props_prior_rounds_for_display_round = displayRound;
  }
  return { archived, kept: keep.length, displayRound };
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
    `[archive-prior-props] display_round=${r.displayRound} archived=${r.archived} kept=${r.kept} → ${file}`,
  );
}

const isDirect = process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href;
if (isDirect) main();
