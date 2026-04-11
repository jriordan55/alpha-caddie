#!/usr/bin/env node
/**
 * DataGolf live bundle for the static app:
 * - preds/in-play (placement probs, ~5 min)
 * - preds/live-tournament-stats (field SG / traditional stats)
 * - preds/live-hole-stats (hole scoring vs par — drives live “course difficulty” pricing)
 *
 * https://feeds.datagolf.com/preds/in-play?tour=[tour]&dead_heat=[no|yes]&odds_format=[percent|...]&file_format=json
 * https://feeds.datagolf.com/preds/live-tournament-stats?stats=...&round=event_avg&display=value&file_format=json
 * https://feeds.datagolf.com/preds/live-hole-stats?tour=[tour]&file_format=json
 *
 * Writes alpha-caddie-web/live-in-play.json: API shape for in-play plus optional top-level
 * `live_tournament_stats` and `live_hole_stats` objects from DataGolf.
 *
 * Env:
 *   DATAGOLF_API_KEY or datagolf.local.json { apiKey }
 *   GOLF_MODEL_DIR — repo root (parent of alpha-caddie-web)
 *   GOLF_DATAGOLF_TOUR — primary tour (default: pga). Options: pga, euro, opp, kft, alt
 *   GOLF_IN_PLAY_FALLBACK_TOUR — if primary returns empty data[], try this (default: opp)
 *   GOLF_IN_PLAY_DEAD_HEAT — no (default) | yes
 *   GOLF_IN_PLAY_ODDS_FORMAT — percent (default), american, decimal, fraction
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = process.env.GOLF_MODEL_DIR ? path.resolve(process.env.GOLF_MODEL_DIR) : path.resolve(WEB_ROOT, "..");

function loadApiKey() {
  const env = (process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = path.join(WEB_ROOT, "datagolf.local.json");
  if (fs.existsSync(p)) {
    try {
      const j = JSON.parse(fs.readFileSync(p, "utf8"));
      return String(j.apiKey || j.key || "").trim();
    } catch {
      return "";
    }
  }
  return "";
}

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

async function dgGetJson(url) {
  const maxAttempts = Math.max(3, Math.min(15, Number(process.env.GOLF_DG_MAX_ATTEMPTS || 8)));
  let lastErr;
  let lastStatus;
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const res = await fetch(url, { redirect: "follow" }).catch((e) => {
      lastErr = e;
      return null;
    });
    if (!res) {
      await sleep(Math.min(5000 + attempt * 2000, 30000));
      continue;
    }
    if (res.status === 200) {
      try {
        return await res.json();
      } catch (e) {
        lastErr = e;
        await sleep(2000);
        continue;
      }
    }
    lastStatus = res.status;
    if ([429, 500, 502, 503, 504].includes(res.status)) {
      let waitMs = Math.min(15000 + attempt * 5000, 90000);
      const ra = res.headers.get("retry-after");
      if (ra) {
        const sec = parseInt(ra, 10);
        if (Number.isFinite(sec) && sec > 0) waitMs = Math.max(waitMs, sec * 1000);
      }
      console.warn(`[fetch-live-in-play] HTTP ${res.status} retry ${attempt}/${maxAttempts}; waiting ${Math.round(waitMs / 1000)}s…`);
      await sleep(waitMs);
      continue;
    }
    const text = await res.text().catch(() => "");
    throw new Error(`HTTP ${res.status} ${text.slice(0, 200)}`);
  }
  throw lastErr || new Error(`DataGolf HTTP ${lastStatus ?? "?"} after ${maxAttempts} attempts`);
}

function inPlayUrl(key, tour, deadHeat, oddsFormat) {
  const u = new URL("https://feeds.datagolf.com/preds/in-play");
  u.searchParams.set("tour", tour);
  u.searchParams.set("dead_heat", deadHeat);
  u.searchParams.set("odds_format", oddsFormat);
  u.searchParams.set("file_format", "json");
  u.searchParams.set("key", key);
  return u.href;
}

function dataLength(parsed) {
  if (!parsed || typeof parsed !== "object") return 0;
  const d = parsed.data;
  return Array.isArray(d) ? d.length : 0;
}

async function main() {
  const key = loadApiKey();
  if (!key) {
    console.error("[fetch-live-in-play] Set DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json with apiKey.");
    process.exit(1);
  }

  const primary = (process.env.GOLF_DATAGOLF_TOUR || "pga").trim().toLowerCase() || "pga";
  const fallback = (process.env.GOLF_IN_PLAY_FALLBACK_TOUR || "opp").trim().toLowerCase();
  const deadHeat = (process.env.GOLF_IN_PLAY_DEAD_HEAT || "no").trim().toLowerCase();
  const oddsFormat = (process.env.GOLF_IN_PLAY_ODDS_FORMAT || "percent").trim().toLowerCase();

  let tourUsed = primary;
  let parsed = await dgGetJson(inPlayUrl(key, primary, deadHeat, oddsFormat));

  if (dataLength(parsed) === 0 && fallback && fallback !== primary) {
    console.log(`[fetch-live-in-play] Primary tour '${primary}' returned 0 players; trying '${fallback}'…`);
    tourUsed = fallback;
    parsed = await dgGetJson(inPlayUrl(key, fallback, deadHeat, oddsFormat));
  }

  if (!parsed || typeof parsed !== "object" || !Array.isArray(parsed.data)) {
    console.error("[fetch-live-in-play] Unexpected JSON (no data array).");
    process.exit(1);
  }

  let liveTournamentStats = null;
  let liveHoleStats = null;
  try {
    liveTournamentStats = await dgGetJson(liveTournamentStatsUrl(key));
  } catch (e) {
    console.warn("[fetch-live-in-play] live-tournament-stats:", e.message || e);
  }
  try {
    liveHoleStats = await dgGetJson(liveHoleStatsUrl(key, tourUsed));
  } catch (e) {
    console.warn("[fetch-live-in-play] live-hole-stats:", e.message || e);
  }

  const bundle = {
    ...parsed,
    ...(liveTournamentStats && typeof liveTournamentStats === "object"
      ? { live_tournament_stats: liveTournamentStats }
      : {}),
    ...(liveHoleStats && typeof liveHoleStats === "object" ? { live_hole_stats: liveHoleStats } : {}),
  };

  const out = path.join(WEB_ROOT, "live-in-play.json");
  fs.mkdirSync(path.dirname(out), { recursive: true });
  const token = compositeLiveBundleToken(parsed, liveTournamentStats, liveHoleStats);
  if (token && fs.existsSync(out)) {
    try {
      const prev = JSON.parse(fs.readFileSync(out, "utf8"));
      const prevTok = compositeLiveBundleToken(
        prev,
        prev.live_tournament_stats,
        prev.live_hole_stats
      );
      if (prevTok && prevTok === token) {
        console.log(`[fetch-live-in-play] unchanged bundle token (${token}); skip write`);
        return;
      }
    } catch {
      /* rewrite if parse fails */
    }
  }
  fs.writeFileSync(out, JSON.stringify(bundle, null, 2), "utf8");
  console.log(
    `[fetch-live-in-play] wrote ${out} (${parsed.data.length} players, tour=${tourUsed}, odds_format=${oddsFormat}; live feeds=${liveTournamentStats ? "t" : "-"}${liveHoleStats ? "h" : "-"})`
  );
}

main().catch((e) => {
  console.error("[fetch-live-in-play]", e.message || e);
  process.exit(1);
});
