#!/usr/bin/env node
/**
 * Alternate fairway projections — reverse-engineered from external sheet vs alpha-caddie model.
 *
 * The sheet's "Proj Fairways" sit ~0.5–1.2 below our DG-rate pipeline at Travelers.
 * Best empirical fit on 15 hand-matched players: model − 0.85 (RMSE ≈ 0.37).
 * Principled equivalent: anchor to course-table driving accuracy, keep ~35% of player DG spread.
 *
 *   node scripts/fairway-projection-alt.mjs
 *   node scripts/fairway-projection-alt.mjs --method course
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const PROJ_PATH = join(WEB_ROOT, "projections.json");

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function loadCourseAdjRate(courseUsed) {
  const p = join(WEB_ROOT, "course-table.json");
  if (!existsSync(p)) return NaN;
  try {
    const ct = JSON.parse(readFileSync(p, "utf8"));
    const key = String(courseUsed || "")
      .trim()
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, " ")
      .trim();
    const row = ct?.byNormKey?.[key] || ct?.rows?.find((r) => String(r._normKey || "").includes("river"));
    return num(row?.adj_driving_accuracy, NaN);
  } catch {
    return NaN;
  }
}

/**
 * Course-layout anchor: shrink DG season-long FW% toward course difficulty.
 * @param {object} opts
 * @param {number} opts.dgFairwayPct — 0–1 fairway rate (dg_fairway_pct)
 * @param {number} [opts.courseAdjRate] — course_table adj_driving_accuracy
 * @param {number} [opts.nFairwayHoles] — driving holes modeled (default 14)
 * @param {number} [opts.spreadKeep] — fraction of (player − course) skill kept (default 0.35)
 */
export function fairwayProjectionCourseAnchored(opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const courseRate = num(opts.courseAdjRate, 0.645);
  const dgRate = num(opts.dgFairwayPct, NaN);
  if (!Number.isFinite(dgRate)) return NaN;
  const spreadKeep = num(opts.spreadKeep, 0.35);
  const course14 = courseRate * nFw;
  const dg14 = dgRate * nFw;
  const fw = course14 + spreadKeep * (dg14 - course14);
  return Math.round(Math.max(2, Math.min(nFw + 0.5, fw)) * 100) / 100;
}

/**
 * Venue + field-relative shrink (fits sheet moderately; RMSE ~0.54 at Travelers).
 */
export function fairwayProjectionVenueRelative(opts = {}) {
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const venueFw = num(opts.venueAvgFairways, 9.28);
  const fieldMeanDg14 = num(opts.fieldMeanDg14, 10);
  const dg14 = num(opts.dgFairwayPct, NaN) * nFw;
  if (!Number.isFinite(dg14)) return NaN;
  const fw = venueFw + (dg14 - fieldMeanDg14) * 0.4;
  return Math.round(Math.max(2, Math.min(nFw + 0.5, fw)) * 100) / 100;
}

/** Empirical sheet fit: alpha-caddie model minus constant (~0.85 at Travelers). */
export function fairwayProjectionModelShrink(opts = {}) {
  const model = num(opts.modelFairways, NaN);
  const shift = num(opts.shift, 0.85);
  if (!Number.isFinite(model)) return NaN;
  const nFw = Math.round(num(opts.nFairwayHoles, 14)) || 14;
  const fw = model - shift;
  return Math.round(Math.max(2, Math.min(nFw + 0.5, fw)) * 100) / 100;
}

function main() {
  const method = process.argv.includes("--method")
    ? process.argv[process.argv.indexOf("--method") + 1] || "shrink"
    : "shrink";

  if (!existsSync(PROJ_PATH)) {
    console.error(`Missing ${PROJ_PATH}`);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(PROJ_PATH, "utf8"));
  const basis = payload.projection_course_basis || {};
  const nFw = Math.round(num(basis.fairway_holes_modeled, 14)) || 14;
  const courseAdj = num(basis.course_adj_fairway_rate, NaN) || loadCourseAdjRate(payload.course_used);
  const venueFw = num(basis.venue_avg_fairways, 9.28);
  const round = Math.round(num(payload.display_round, 1)) || 1;

  const r1 = (payload.players || []).filter((p) => Math.round(num(p.round, NaN)) === round);
  const dgRates = r1.map((p) => num(p.dg_fairway_pct, NaN)).filter(Number.isFinite);
  const fieldMeanDg14 = dgRates.length ? (dgRates.reduce((s, r) => s + r, 0) / dgRates.length) * nFw : 10;

  const propsByDg = new Map();
  for (const pr of payload.props || []) {
    if (pr.market !== "Fairways hit") continue;
    if (Math.round(num(pr.round_num, NaN)) !== round) continue;
    propsByDg.set(Math.round(num(pr.dg_id, NaN)), pr);
  }

  const rows = [];
  for (const pl of r1) {
    const dg = num(pl.dg_fairway_pct, NaN);
    const model = num(pl.fairways, NaN);
    const common = { dgFairwayPct: dg, courseAdjRate: courseAdj, nFairwayHoles: nFw, venueAvgFairways: venueFw, fieldMeanDg14, modelFairways: model };
    let alt = NaN;
    if (method === "course") alt = fairwayProjectionCourseAnchored(common);
    else if (method === "venue") alt = fairwayProjectionVenueRelative(common);
    else alt = fairwayProjectionModelShrink(common);

    const pr = propsByDg.get(Math.round(num(pl.dg_id, NaN)));
    rows.push({
      player: pl.player_name,
      tee: pl.tee_time_local || pl.tee_time || "",
      model_fw: model,
      alt_fw: alt,
      dg_pct: dg,
      dk_line: pr ? num(pr.line, NaN) : "",
      over_odds: pr?.over_odds ?? "",
      under_odds: pr?.under_odds ?? "",
    });
  }
  rows.sort((a, b) => String(a.player).localeCompare(String(b.player)));

  const header =
    "player,tee_time,model_fairways,alt_fairways,dg_fairway_pct,dk_line,over_odds,under_odds\n";
  const body = rows
    .map((r) =>
      [
        r.player,
        r.tee,
        r.model_fw,
        r.alt_fw,
        r.dg_pct,
        r.dk_line,
        r.over_odds,
        r.under_odds,
      ].join(","),
    )
    .join("\n");
  const outPath = join(WEB_ROOT, "data", "fairway_projection_alt.csv");
  writeFileSync(outPath, header + body + "\n", "utf8");

  const meanAlt = rows.reduce((s, r) => s + num(r.alt_fw, 0), 0) / Math.max(1, rows.length);
  const meanModel = rows.reduce((s, r) => s + num(r.model_fw, 0), 0) / Math.max(1, rows.length);
  console.log(`[fairway-alt] method=${method} event=${payload.event_name} R${round}`);
  console.log(`  course_adj_rate=${Number.isFinite(courseAdj) ? courseAdj.toFixed(3) : "—"} n_fw=${nFw}`);
  console.log(`  field mean: model ${meanModel.toFixed(2)} → alt ${meanAlt.toFixed(2)}`);
  console.log(`  wrote ${outPath} (${rows.length} players)`);
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === join(process.argv[1]);
if (isMain) main();
