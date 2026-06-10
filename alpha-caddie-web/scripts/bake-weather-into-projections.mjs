#!/usr/bin/env node
/**
 * Bake Open-Meteo tee-time weather into projections.json (for static deploy / push:live).
 * Requires live-in-play.json field_updates for per-player tee slices when available.
 *
 *   npm run bake:weather
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import path from "path";
import { fileURLToPath } from "url";
import {
  bakeOpenMeteoWeatherIntoProjections,
  fieldUpdatesAlignWithProjections,
} from "./open-meteo-forecast.mjs";
import { flattenProjectionExportMeta, projectionExportMeta } from "./projection-export-meta.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const projPath = path.join(WEB_ROOT, "projections.json");
const livePath = path.join(WEB_ROOT, "live-in-play.json");

function readJson(p) {
  return JSON.parse(readFileSync(p, "utf8"));
}

async function main() {
  if (!existsSync(projPath)) {
    console.warn("[bake-weather] missing projections.json — skip");
    process.exit(0);
  }
  const proj = readJson(projPath);
  let fieldUpdates = null;
  if (existsSync(livePath)) {
    try {
      const live = readJson(livePath);
      const fu = live?.field_updates;
      if (fu && typeof fu === "object") {
        if (fieldUpdatesAlignWithProjections(proj, fu)) fieldUpdates = fu;
        else {
          console.warn(
            "[bake-weather] field_updates week mismatch — tee times not merged (venue forecast still runs)",
          );
        }
      }
    } catch (e) {
      console.warn("[bake-weather] could not read live-in-play.json:", e.message || e);
    }
  } else {
    console.warn("[bake-weather] no live-in-play.json — median/venue-only weather (no tee slices)");
  }

  const result = await bakeOpenMeteoWeatherIntoProjections(proj, { fieldUpdates });
  flattenProjectionExportMeta(proj);
  writeFileSync(projPath, `${JSON.stringify(proj, null, 2)}\n`, "utf8");
  const meta = projectionExportMeta(proj);
  console.log(
    `[bake-weather] status=${result.status} display_round=${meta.forecast_weather_display_round ?? "?"} players=${result.playersWithWeather}/${result.playerCount} tee_slices=${result.teeMatches} counts_baked=${result.countsWeatherBaked ?? 0}`,
  );
  /* Weather is best-effort — do not fail push:live on API/network issues. */
  process.exit(0);
}

main().catch((e) => {
  console.error("[bake-weather] fatal:", e.message || e);
  process.exit(0);
});
