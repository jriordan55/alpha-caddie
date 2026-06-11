#!/usr/bin/env node
/**
 * Pre-push checks: pin banner copy, market rating computable for GIR, benchmarks present.
 *   npm run verify:web-deploy
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { marketRating100ForPlayer } from "./market-rating-player.mjs";
import { roundAdjustmentsFromPinSheet } from "./pin-sheet-difficulty.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");

function fail(msg) {
  console.error(`[verify:web-deploy] FAIL: ${msg}`);
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

const indexHtml = readFileSync(join(WEB, "index.html"), "utf8");
if (!/app\.js\?v=\d+/.test(indexHtml)) {
  fail("index.html must reference app.js?v=N for cache busting");
}

const activePath = join(WEB, "data", "pin_sheets", "pin_sheet_active.json");
if (existsSync(activePath)) {
  const active = JSON.parse(readFileSync(activePath, "utf8"));
  if (Array.isArray(active.holes) && active.holes.length >= 9) {
    const adj = roundAdjustmentsFromPinSheet(active.holes);
    const summary = String(adj.summary || "");
    if (/\.{3}|…/.test(summary)) fail(`pin_sheet summary must not truncate with ellipsis: ${summary}`);
    if (!summary.trim()) fail("pin_sheet summary empty");
  }
}

const proj = loadJson("projections.json");
const bench = proj.pga_tour_market_benchmarks || proj.meta?.pga_tour_market_benchmarks;
if (!bench?.GIR?.mean) fail("projections.json missing pga_tour_market_benchmarks.GIR");

const players = Array.isArray(proj.players) ? proj.players : [];
const dr = Math.round(Number(proj.display_round ?? proj.meta?.display_round ?? 1));
const r1Players = players.filter(
  (p) => Math.round(Number(p.round)) === dr && Number.isFinite(Number(p.dg_gir_pct)) && Number(p.dg_gir_pct) > 0,
);
if (r1Players.length < 20) {
  fail(`expected ≥20 display_round R${dr} players with dg_gir_pct, got ${r1Players.length}`);
}

let bad = 0;
for (const p of r1Players.slice(0, 12)) {
  const score = marketRating100ForPlayer("GIR", p, bench);
  if (!Number.isFinite(score) || score < 1 || score > 100) {
    console.error(`  invalid GIR market rating for ${p.player_name}: ${score}`);
    bad++;
  }
}
if (bad > 0) fail(`${bad} player(s) with invalid GIR market rating`);

const pinMeta = proj.pin_sheet || proj.meta?.pin_sheet;
if (pinMeta?.summary && /\.{3}|…/.test(pinMeta.summary)) {
  fail(`projections pin_sheet.summary must not truncate with ellipsis: ${pinMeta.summary}`);
}

console.log(
  `[verify:web-deploy] OK — pin copy clean, GIR market rating for ${Math.min(12, r1Players.length)} sample players, app.js cache bust present`,
);
