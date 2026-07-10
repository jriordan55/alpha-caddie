#!/usr/bin/env node
/**
 * Regression guard: PrizePicks round props merge, field alignment, and projection-tracker PP columns.
 * Uses only committed projections.json + audit artifacts — no mock API payloads.
 *
 *   npm run verify:pp-round-props
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { ppPropMatchesProjectionField } from "./pp-field-align.mjs";
import { EXPORT_PP_LINE_COLS } from "./round-projection-mu.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");

function fail(msg) {
  console.error(`[verify:pp-round-props] FAIL: ${msg}`);
  process.exit(1);
}

function loadJson(rel) {
  const p = join(WEB, rel);
  if (!existsSync(p)) fail(`missing ${rel}`);
  try {
    return JSON.parse(readFileSync(p, "utf8"));
  } catch (e) {
    fail(`invalid JSON ${rel}: ${e.message || e}`);
  }
}

function num(x) {
  const n = Number(x);
  return Number.isFinite(n) ? n : NaN;
}

const ppIngestSrc = readFileSync(join(__dirname, "prizepicks-ou-props.mjs"), "utf8");
for (const statName of ["Greens In Regulation", "Greens in Regulation", "Fairways Hit"]) {
  if (!ppIngestSrc.includes(statName)) {
    fail(`prizepicks-ou-props.mjs missing stat mapping for ${statName}`);
  }
}

const proj = loadJson("projections.json");
const players = Array.isArray(proj.players) ? proj.players : [];
const displayRound = Math.round(num(proj.display_round ?? proj.meta?.display_round, 1)) || 1;
const ppRows = (Array.isArray(proj.props) ? proj.props : []).filter(
  (r) => String(r?.source || "").trim().toLowerCase() === "prizepicks",
);

if (!EXPORT_PP_LINE_COLS.includes("gir_pp_line")) {
  fail("round-projection-mu.mjs missing gir_pp_line export column");
}
if (!EXPORT_PP_LINE_COLS.includes("fairways_pp_line")) {
  fail("round-projection-mu.mjs missing fairways_pp_line export column");
}
const exportSrc = readFileSync(join(__dirname, "export-round-projection-vs-actual-csv.mjs"), "utf8");
if (!exportSrc.includes("EXPORT_PP_LINE_COLS") || !exportSrc.includes("pp_book_odds_source")) {
  fail("export-round-projection-vs-actual-csv.mjs missing PrizePicks CSV wiring");
}

for (const r of ppRows) {
  const mk = String(r.market || "").trim();
  if (!mk) fail("PP prop missing market");
  if (!Number.isFinite(num(r.line, NaN))) fail(`PP ${mk} missing line for ${r.player_name || "?"}`);
  if (!Number.isFinite(num(r.over_odds, NaN)) || !Number.isFinite(num(r.under_odds, NaN))) {
    fail(`PP ${mk} missing odds for ${r.player_name || "?"}`);
  }
  if (!ppPropMatchesProjectionField(r, players)) {
    fail(
      `PP ${mk} for ${r.player_name || "?"} is not in R${displayRound} field — stale cross-event row in projections.json`,
    );
  }
  const id = Math.round(num(r.dg_id, NaN));
  if (!Number.isFinite(id) || id <= 0) {
    fail(`PP ${mk} for ${r.player_name || "?"} missing dg_id after field merge`);
  }
}

const ppByMarket = {};
for (const r of ppRows) {
  const mk = String(r.market || "").trim();
  ppByMarket[mk] = (ppByMarket[mk] || 0) + 1;
}

const ppAudit = join(WEB, "data", "pp_round_projection_audit.csv");
if (ppRows.length && !existsSync(ppAudit)) {
  console.warn(
    `[verify:pp-round-props] warn: ${ppRows.length} PP props in projections but no pp_round_projection_audit.csv yet (run fetch:book-odds or refresh:live)`,
  );
}

console.log(
  `[verify:pp-round-props] OK — ${ppRows.length} PP props in field (${Object.entries(ppByMarket)
    .map(([m, n]) => `${m}=${n}`)
    .join(", ")}); export PP columns present`,
);
