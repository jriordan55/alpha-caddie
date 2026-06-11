#!/usr/bin/env node
/**
 * Merge DataGolf field-updates tee times into projections.json (`dg_teetime_local`, `dg_tee_wave`).
 * Eastern/local times from field-updates — displayed in the Round O/U table Tee time column.
 *
 * Runs on push:live after fetch:in-play + merge:live-round-meta (independent of Open-Meteo bake).
 *
 *   npm run merge:field-teetimes-into-projections
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";
import {
  fieldUpdatesAlignWithProjections,
  mergeFieldTeeTimesIntoProjections,
} from "./open-meteo-forecast.mjs";
import { flattenProjectionExportMeta, projectionExportMeta } from "./projection-export-meta.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");
const livePath = join(WEB_ROOT, "live-in-play.json");

function fieldUpdatesSafeToMerge(proj, fu) {
  if (!fu || typeof fu !== "object") return false;
  if (fieldUpdatesAlignWithProjections(proj, fu)) return true;
  const modelEvent = String(proj?.event_name || "").trim();
  const fuEvent = String(fu.event_name ?? fu.eventName ?? "").trim();
  return !!(modelEvent && fuEvent && eventsLikelySame(modelEvent, fuEvent));
}

function main() {
  if (!existsSync(projPath)) {
    console.warn("[merge:field-teetimes] missing projections.json — skip");
    process.exit(0);
  }
  if (!existsSync(livePath)) {
    console.log("[merge:field-teetimes] no live-in-play.json — skip");
    process.exit(0);
  }

  let proj;
  let live;
  try {
    proj = JSON.parse(readFileSync(projPath, "utf8"));
    live = JSON.parse(readFileSync(livePath, "utf8"));
  } catch (e) {
    console.warn("[merge:field-teetimes] parse error —", e.message || e);
    process.exit(0);
  }

  const fu = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : null;
  if (!fu) {
    console.log("[merge:field-teetimes] live-in-play has no field_updates — skip");
    process.exit(0);
  }

  if (!fieldUpdatesSafeToMerge(proj, fu)) {
    const modelEvent = String(proj?.event_name || "").trim();
    const fuEvent = String(fu.event_name ?? fu.eventName ?? "").trim();
    console.warn(
      `[merge:field-teetimes] field_updates event mismatch ("${fuEvent || "?"}" vs "${modelEvent || "?"}") — skip`,
    );
    process.exit(0);
  }

  const n = mergeFieldTeeTimesIntoProjections(proj, fu);
  const meta = projectionExportMeta(proj);
  meta.dg_teetimes_merged_at = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  meta.forecast_weather_tee_times_merged = n;
  flattenProjectionExportMeta(proj);
  writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`, "utf8");
  console.log(`[merge:field-teetimes] merged ${n} tee time row(s) → ${projPath}`);
}

main();
