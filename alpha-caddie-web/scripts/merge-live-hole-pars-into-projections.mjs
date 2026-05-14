#!/usr/bin/env node
/**
 * After fetch:in-play, align projections.json hole_pars with live-in-play.json live_hole_stats
 * (same per-hole table as preds/live-hole-stats). npm run push:all runs this after fetch:book-odds so an inline
 * fetch:dg inside book-odds does not leave projections without live_hole_stats pars before publish.
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { holeParsFromLiveHoleStatsPayload } from "./dg-live-hole-pars.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");
const livePath = join(WEB_ROOT, "live-in-play.json");

if (!existsSync(projPath)) {
  console.warn("merge-live-hole-pars: missing projections.json");
  process.exit(0);
}
if (!existsSync(livePath)) {
  console.log("merge-live-hole-pars: no live-in-play.json — skip");
  process.exit(0);
}

let proj;
let live;
try {
  proj = JSON.parse(readFileSync(projPath, "utf8"));
  live = JSON.parse(readFileSync(livePath, "utf8"));
} catch (e) {
  console.warn("merge-live-hole-pars: parse error —", e.message || e);
  process.exit(0);
}

const lh = live.live_hole_stats;
const fieldRaw = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : null;

const pars = holeParsFromLiveHoleStatsPayload(
  lh,
  String(proj.course_used || "").trim(),
  fieldRaw,
  String(proj.event_name || "").trim(),
);

if (!pars || pars.length !== 18) {
  console.log("merge-live-hole-pars: no pars from live_hole_stats — skip");
  process.exit(0);
}

const prev = JSON.stringify(proj.hole_pars);
const next = JSON.stringify(pars);
const src = String(proj.hole_pars_source || "").toLowerCase();
if (prev === next && src === "live_hole_stats") {
  console.log("merge-live-hole-pars: projections already match live_hole_stats");
  process.exit(0);
}

proj.hole_pars = pars;
proj.course_par_18 = pars.reduce((sum, p) => sum + Math.round(Number(p) || 4), 0);
proj.hole_pars_source = "live_hole_stats";
writeFileSync(projPath, JSON.stringify(proj, null, 2), "utf8");
console.log(
  prev === next
    ? "merge-live-hole-pars: set hole_pars_source to live_hole_stats"
    : "merge-live-hole-pars: updated projections.json hole_pars from live-in-play live_hole_stats",
);
