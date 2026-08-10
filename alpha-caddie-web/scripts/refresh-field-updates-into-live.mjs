#!/usr/bin/env node
/**
 * Fresh DataGolf field-updates → live-in-play.json (tee times + week metadata).
 * Always runs on push:live so tee times update even when preds/in-play bundle token is unchanged.
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { fieldUpdatesAlignWithProjections } from "./open-meteo-forecast.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");
const livePath = join(WEB_ROOT, "live-in-play.json");

function loadApiKey() {
  const env = String(process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = join(WEB_ROOT, "datagolf.local.json");
  if (!existsSync(p)) return "";
  try {
    const j = JSON.parse(readFileSync(p, "utf8"));
    return String(j.apiKey || j.key || "").trim();
  } catch {
    return "";
  }
}

function countTeeSlots(fieldRaw) {
  const flist = fieldRaw?.field ?? fieldRaw?.field_updates ?? fieldRaw?.players ?? fieldRaw?.data;
  if (!Array.isArray(flist)) return 0;
  let n = 0;
  for (const p of flist) {
    const tt = p?.teetimes;
    if (Array.isArray(tt)) n += tt.filter((s) => s?.teetime != null && String(s.teetime).trim()).length;
  }
  return n;
}

async function fetchFieldUpdates(tour, key) {
  const u = new URL("https://feeds.datagolf.com/field-updates");
  u.searchParams.set("tour", tour);
  u.searchParams.set("file_format", "json");
  u.searchParams.set("key", key);
  const res = await fetch(u.href, { headers: { Accept: "application/json" } });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res.json();
}

async function main() {
  if (!existsSync(projPath)) {
    console.log("[refresh-field-updates] no projections.json — skip");
    return;
  }
  const key = loadApiKey();
  if (!key) {
    console.warn("[refresh-field-updates] no DATAGOLF_API_KEY — skip");
    return;
  }

  const proj = JSON.parse(readFileSync(projPath, "utf8"));
  const tour = String(proj.datagolf_feed_tour || process.env.GOLF_DATAGOLF_TOUR || "pga").trim().toLowerCase() || "pga";
  let fu;
  try {
    fu = await fetchFieldUpdates(tour, key);
  } catch (e) {
    console.warn("[refresh-field-updates] fetch failed:", e.message || e);
    return;
  }

  const projEvent = String(proj.event_name || "").trim();
  const fuEvent = String(fu?.event_name ?? fu?.eventName ?? "").trim();
  if (projEvent && fuEvent && !eventsLikelySame(projEvent, fuEvent) && !fieldUpdatesAlignWithProjections(proj, fu)) {
    console.warn(
      `[refresh-field-updates] week mismatch ("${fuEvent}" vs "${projEvent}") — skip patch`,
    );
    return;
  }

  const freshTees = countTeeSlots(fu);
  let live = {};
  if (existsSync(livePath)) {
    try {
      live = JSON.parse(readFileSync(livePath, "utf8"));
    } catch {
      live = {};
    }
  }
  const prevTees = countTeeSlots(live.field_updates);
  const liveInfoEv = String(live?.info?.event_name || "").trim();
  const liveLtsEv = String(
    live?.live_tournament_stats?.event_name || live?.info?.event_name || "",
  ).trim();
  const hybridPriorWeek =
    !!(fuEvent && liveInfoEv && !eventsLikelySame(fuEvent, liveInfoEv)) ||
    !!(
      fuEvent &&
      liveLtsEv &&
      !eventsLikelySame(fuEvent, liveLtsEv) &&
      Object.keys(live?.live_tournament_stats_by_round || {}).length
    );

  if (hybridPriorWeek) {
    // Never leave last week's LTS/leaderboard under this week's field_updates — Live Stats reads those.
    live = {
      data: [],
      info: {
        event_name: fuEvent || projEvent,
        current_round: Math.round(Number(fu?.current_round) || 0) || 0,
        last_update: new Date().toISOString(),
        pre_event: true,
        note: `Cleared prior-week live stats (${liveInfoEv || liveLtsEv || "unknown"}) after field-updates rolled to ${fuEvent || projEvent}`,
      },
      field_updates: fu,
      live_tournament_stats: null,
      live_tournament_stats_by_round: {},
      live_round_actuals_by_dg: {},
      live_hole_stats: null,
      live_stats_pre_event: true,
      field_updates_refreshed_at: new Date().toISOString(),
    };
    writeFileSync(livePath, `${JSON.stringify(live, null, 2)}\n`, "utf8");
    console.warn(
      `[refresh-field-updates] reset live-in-play.json to pre-event skeleton for "${fuEvent || projEvent}" (was hybrid with "${liveInfoEv || liveLtsEv}")`,
    );
    return;
  }

  live.field_updates = fu;
  live.field_updates_refreshed_at = new Date().toISOString();
  writeFileSync(livePath, `${JSON.stringify(live, null, 2)}\n`, "utf8");
  console.log(
    `[refresh-field-updates] patched live-in-play.json field_updates (tee slots ${prevTees} → ${freshTees})`,
  );
}

main().catch((e) => {
  console.warn("[refresh-field-updates] fatal:", e.message || e);
  process.exit(0);
});
