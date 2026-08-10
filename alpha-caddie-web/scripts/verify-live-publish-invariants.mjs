#!/usr/bin/env node
/**
 * Hard gates for `npm run push:live` so stale tracker / odds / Live Stats cannot publish.
 *
 * Checks:
 *  1. live-in-play.json is not a hybrid of last week's LTS/data + this week's field_updates
 *  2. Odds Screen matchups (when present) match projections.event_name
 *  3. Projection-tracker O/U CSV + walkforward_oos_roi.json exist and are fresh vs this run
 *
 * Env:
 *   GOLF_REQUIRE_LIVE_PUBLISH_INVARIANTS=1 — fail (exit 1) on violation (push:live default)
 *   GOLF_LIVE_PUBLISH_MAX_STALE_MIN=180 — tracker files older than this vs projections fail
 */
import { existsSync, readFileSync, statSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");

function envTruthy(name, fallback = false) {
  const v = String(process.env[name] ?? "").trim().toLowerCase();
  if (!v) return fallback;
  return !["0", "false", "no", "off"].includes(v);
}

function loadJson(p) {
  return JSON.parse(readFileSync(p, "utf8"));
}

function mtimeMs(p) {
  try {
    return statSync(p).mtimeMs;
  } catch {
    return NaN;
  }
}

function eventFromLive(live) {
  return String(
    live?.field_updates?.event_name ||
      live?.info?.event_name ||
      live?.live_tournament_stats?.event_name ||
      "",
  ).trim();
}

function ltsEvent(live) {
  const by = live?.live_tournament_stats_by_round;
  if (by && typeof by === "object") {
    for (const pack of Object.values(by)) {
      const ev = String(pack?.event_name || "").trim();
      if (ev) return ev;
    }
  }
  return String(live?.live_tournament_stats?.event_name || live?.info?.event_name || "").trim();
}

function main() {
  const requireHard = envTruthy("GOLF_REQUIRE_LIVE_PUBLISH_INVARIANTS", true);
  const maxStaleMin = Math.max(
    15,
    Math.min(24 * 60, Number(process.env.GOLF_LIVE_PUBLISH_MAX_STALE_MIN || 180) || 180),
  );
  /** @type {string[]} */
  const errors = [];
  /** @type {string[]} */
  const warns = [];

  const projPath = join(WEB_ROOT, "projections.json");
  const livePath = join(WEB_ROOT, "live-in-play.json");
  if (!existsSync(projPath)) {
    errors.push("missing projections.json");
  }
  if (!existsSync(livePath)) {
    errors.push("missing live-in-play.json");
  }
  if (errors.length) {
    for (const e of errors) console.error(`[verify:live-publish] FAIL: ${e}`);
    process.exit(requireHard ? 1 : 0);
  }

  const proj = loadJson(projPath);
  const live = loadJson(livePath);
  const projEv = String(proj.event_name || "").trim();
  const liveEv = eventFromLive(live);
  const infoEv = String(live?.info?.event_name || "").trim();
  const fuEv = String(live?.field_updates?.event_name || "").trim();
  const ltsEv = ltsEvent(live);
  const preEvent = live?.info?.pre_event === true || live?.live_stats_pre_event === true;

  // Hybrid: field_updates on new week, info/LTS still on old week.
  if (fuEv && infoEv && !eventsLikelySame(fuEv, infoEv) && !preEvent) {
    errors.push(
      `live-in-play hybrid week: info="${infoEv}" vs field_updates="${fuEv}" (refusing stale Live Stats)`,
    );
  }
  if (fuEv && ltsEv && !eventsLikelySame(fuEv, ltsEv) && !preEvent) {
    const by = live?.live_tournament_stats_by_round || {};
    const hasRows = Object.values(by).some((p) => Array.isArray(p?.live_stats) && p.live_stats.length > 0);
    if (hasRows) {
      errors.push(
        `live-in-play LTS still on "${ltsEv}" while field_updates is "${fuEv}" — clear or rewrite live bundle`,
      );
    }
  }
  if (projEv && liveEv && !eventsLikelySame(projEv, liveEv) && !preEvent) {
    errors.push(`live-in-play event "${liveEv}" ≠ projections "${projEv}"`);
  }
  if (projEv && preEvent && liveEv && !eventsLikelySame(projEv, liveEv)) {
    errors.push(`pre-event live skeleton event "${liveEv}" ≠ projections "${projEv}"`);
  }

  // Odds Screen: matchups must not advertise last week's event.
  const matchups = proj.matchups && typeof proj.matchups === "object" ? proj.matchups : {};
  for (const market of ["tournament_matchups", "round_matchups", "3_balls"]) {
    const pack = matchups[market];
    if (!pack || typeof pack !== "object") continue;
    const mev = String(pack.event_name || "").trim();
    if (!projEv || !mev) continue;
    if (eventsLikelySame(projEv, mev)) continue;
    const list = pack.match_list;
    const n = Array.isArray(list) ? list.length : typeof list === "string" && list && !/no .+ being offered/i.test(list) ? 1 : 0;
    if (n > 0) {
      errors.push(`Odds Screen ${market} still on "${mev}" (projections="${projEv}") — clear stale matchups`);
    } else {
      warns.push(`${market} event_name "${mev}" ≠ "${projEv}" but match_list empty`);
    }
  }

  // Projection tracker artifacts must exist and have been refreshed this session (wall-clock).
  const trackerFiles = [
    join(WEB_ROOT, "data", "round_projection_vs_actual.csv"),
    join(WEB_ROOT, "data", "round_projection_vs_actual_summary.csv"),
    join(WEB_ROOT, "data", "walkforward_oos_roi.json"),
  ];
  const skipTracker = envTruthy("GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL", false);
  if (!skipTracker) {
    const now = Date.now();
    for (const f of trackerFiles) {
      if (!existsSync(f)) {
        errors.push(`missing tracker file ${f.replace(/\\/g, "/").split("/alpha-caddie-web/").pop()}`);
        continue;
      }
      const tm = mtimeMs(f);
      if (!Number.isFinite(tm)) continue;
      const ageMin = (now - tm) / 60000;
      if (ageMin > maxStaleMin) {
        errors.push(
          `stale tracker ${f.split(/[/\\]/).pop()} last written ${ageMin.toFixed(0)}m ago (max ${maxStaleMin}m) — push:live must refresh projection-tracker`,
        );
      }
    }
  } else {
    warns.push("GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL=1 — tracker freshness not enforced");
  }

  for (const w of warns) console.warn(`[verify:live-publish] WARN: ${w}`);
  if (errors.length) {
    for (const e of errors) console.error(`[verify:live-publish] FAIL: ${e}`);
    if (requireHard) {
      console.error(
        "[verify:live-publish] Fix: re-run refresh:live after event-roll hygiene; do not publish hybrid live-in-play or last-week matchups.",
      );
      process.exit(1);
    }
    console.warn("[verify:live-publish] continuing (GOLF_REQUIRE_LIVE_PUBLISH_INVARIANTS soft/off)");
    return;
  }
  console.log(
    `[verify:live-publish] OK — event="${projEv}" live=${preEvent ? "pre-event" : "aligned"} trackers fresh`,
  );
}

main();
