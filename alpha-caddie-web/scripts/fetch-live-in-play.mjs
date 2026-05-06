#!/usr/bin/env node
/**
 * DataGolf live bundle for the static app:
 * - preds/in-play (placement probs)
 * - field-updates (authoritative live leaderboard `current_score` / to-par when DG exposes it)
 * - preds/live-tournament-stats (field SG / traditional stats)
 * - preds/live-hole-stats (hole scoring vs par — drives live “course difficulty” pricing)
 *
 * https://feeds.datagolf.com/preds/in-play?tour=[tour]&dead_heat=[no|yes]&odds_format=[percent|...]&file_format=json
 * https://feeds.datagolf.com/field-updates?tour=[tour]&file_format=json
 * https://feeds.datagolf.com/preds/live-tournament-stats?stats=...&round=event_avg&display=value&file_format=json
 * https://feeds.datagolf.com/preds/live-hole-stats?tour=[tour]&file_format=json
 *
 * Writes alpha-caddie-web/live-in-play.json. Scores from field-updates are merged onto each
 * preds/in-play `data` row by `dg_id` so the browser model (outrights, +EV) tracks the live board.
 *
 * Env:
 *   DATAGOLF_API_KEY or datagolf.local.json { apiKey }
 *   GOLF_MODEL_DIR — repo root (parent of alpha-caddie-web)
 *   GOLF_DATAGOLF_TOUR or GOLF_TOUR — default primary tour when projections.json has no datagolf_feed_tour
 *   projections.datagolf_feed_tour — set by fetch:dg when the chosen field is opp/kft/etc. (overrides env)
 *   GOLF_IN_PLAY_FALLBACK_TOUR — if primary returns empty data[], try this (default: opp)
 *   GOLF_IN_PLAY_DEAD_HEAT — no (default) | yes
 *   GOLF_IN_PLAY_ODDS_FORMAT — percent (default), american, decimal, fraction
 *   GOLF_SKIP_LIVE_IN_PLAY_PGA_ALIGN_FETCH_DG=1 — skip PGA-vs-projections week check (runs fetch:dg when snapshot is wrong tour).
 */
import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame, fieldWeekKey, fieldWeekKeysRoughMatch } from "./dg-events-align.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? path.resolve(process.env.GOLF_MODEL_DIR.trim())
  : path.resolve(WEB_ROOT, "..");

/** Prefer tour chosen during fetch:dg (pga vs opp dual-field weeks). */
function datagolfFeedTourFromProjections() {
  try {
    const p = path.join(WEB_ROOT, "projections.json");
    if (!fs.existsSync(p)) return "";
    const j = JSON.parse(fs.readFileSync(p, "utf8"));
    return String(j.datagolf_feed_tour || "").trim().toLowerCase();
  } catch {
    return "";
  }
}

function readProjectionsRoot() {
  const p = path.join(WEB_ROOT, "projections.json");
  if (!fs.existsSync(p)) return null;
  try {
    const j = JSON.parse(fs.readFileSync(p, "utf8"));
    return j && typeof j === "object" ? j : null;
  } catch {
    return null;
  }
}

async function fetchJsonGet(href) {
  const res = await fetch(href, { headers: { Accept: "application/json" } });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res.json();
}

/**
 * Git/deploy snapshots can pin `datagolf_feed_tour=liv` while PGA has rolled — preds/in-play then stays on LIV forever.
 * Align disk projections with `/field-updates?tour=pga` before pulling preds/in-play.
 */
async function maybeRebuildProjectionsIfPgaWeekMismatch(key) {
  if (String(process.env.GOLF_SKIP_LIVE_IN_PLAY_PGA_ALIGN_FETCH_DG || "").trim() === "1") return;
  const proj = readProjectionsRoot();
  if (!proj) return;
  const projEvent = String(proj.event_name || "").trim();
  const projCourse = String(proj.course_used || "").trim();
  if (!projEvent) return;
  let fu;
  try {
    const u = new URL("https://feeds.datagolf.com/field-updates");
    u.searchParams.set("tour", "pga");
    u.searchParams.set("file_format", "json");
    u.searchParams.set("key", key);
    fu = await fetchJsonGet(u.href);
  } catch (e) {
    console.warn("[fetch-live-in-play] PGA field-updates (week alignment):", e.message || e);
    return;
  }
  const pgaEv = String(fu.event_name || "").trim();
  const pgaCourse = String(fu.course_name || fu.course || "").trim();
  if (!pgaEv) return;
  const pk =
    String(proj.datagolf_field_week_key || "").trim() || fieldWeekKey(projEvent, projCourse);
  const fk = fieldWeekKey(pgaEv, pgaCourse);
  if (fieldWeekKeysRoughMatch(pk, fk) && eventsLikelySame(projEvent, pgaEv)) return;
  console.warn(
    `[fetch-live-in-play] projections snapshot (${projEvent}) does not match PGA field-updates (${pgaEv}) — running fetch:dg …`
  );
  const dgScript = path.join(WEB_ROOT, "scripts", "fetch-datagolf.mjs");
  const r = spawnSync(process.execPath, [dgScript], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env: { ...process.env, GOLF_MODEL_DIR: GOLF_MODEL_ROOT, DATAGOLF_API_KEY: key },
  });
  if (r.status !== 0) console.warn("[fetch-live-in-play] fetch:dg exited", r.status);
}

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
  return new Promise((r) => setTimeout(r));
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

function fieldUpdatesUrl(key, tour) {
  const u = new URL("https://feeds.datagolf.com/field-updates");
  u.searchParams.set("tour", tour);
  u.searchParams.set("file_format", "json");
  u.searchParams.set("key", key);
  return u.href;
}

function liveTournamentStatsUrl(key) {
  const u = new URL("https://feeds.datagolf.com/preds/live-tournament-stats");
  u.searchParams.set(
    "stats",
    String(process.env.GOLF_LIVE_TOURNAMENT_STATS_STATS || "").trim() ||
      "sg_ott,distance,accuracy,sg_app,gir,prox_fw,sg_putt,scrambling"
  );
  u.searchParams.set("round", String(process.env.GOLF_LIVE_TOURNAMENT_STATS_ROUND || "event_avg").trim() || "event_avg");
  u.searchParams.set("display", "value");
  u.searchParams.set("file_format", "json");
  u.searchParams.set("key", key);
  return u.href;
}

function liveHoleStatsUrl(key, tour) {
  const u = new URL("https://feeds.datagolf.com/preds/live-hole-stats");
  u.searchParams.set("tour", tour);
  u.searchParams.set("file_format", "json");
  u.searchParams.set("key", key);
  return u.href;
}

function dataLength(parsed) {
  if (!parsed || typeof parsed !== "object") return 0;
  const d = parsed.data;
  return Array.isArray(d) ? d.length : 0;
}

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function fieldListFromJson(fieldRaw) {
  if (!fieldRaw || typeof fieldRaw !== "object") return [];
  const f = fieldRaw.field ?? fieldRaw.field_updates ?? fieldRaw.players ?? fieldRaw.data;
  if (Array.isArray(f)) return f;
  return [];
}

/** First numeric score-like column on a field-updates player row (to-par preferred). */
function scoreFromFieldPlayerRow(p) {
  if (!p || typeof p !== "object") return NaN;
  const keys = [
    "current_score",
    "currentScore",
    "score",
    "tot",
    "total",
    "strokes_vs_par",
    "to_par",
    "round_score",
    "today",
  ];
  for (const k of keys) {
    const v = num(p[k]);
    if (Number.isFinite(v)) return v;
  }
  return NaN;
}

/**
 * Merge DataGolf field-updates scores onto preds/in-play `data` rows (matched by dg_id).
 * Returns count of rows whose `current_score` was set or changed.
 */
function mergeFieldScoresIntoInPlayRows(dataRows, fieldRaw) {
  if (!Array.isArray(dataRows) || !dataRows.length || !fieldRaw) return 0;
  const flist = fieldListFromJson(fieldRaw);
  if (!flist.length) return 0;
  const byDg = new Map();
  for (const p of flist) {
    const id = Math.round(num(p?.dg_id ?? p?.dgId));
    if (!Number.isFinite(id)) continue;
    const sc = scoreFromFieldPlayerRow(p);
    if (!Number.isFinite(sc)) continue;
    byDg.set(id, sc);
  }
  if (!byDg.size) return 0;
  let n = 0;
  for (const row of dataRows) {
    const id = Math.round(num(row?.dg_id ?? row?.dgId));
    if (!Number.isFinite(id)) continue;
    const sc = byDg.get(id);
    if (!Number.isFinite(sc)) continue;
    const prev = num(row.current_score ?? row.currentScore);
    if (!Number.isFinite(prev) || Math.abs(prev - sc) > 1e-6) n++;
    row.current_score = sc;
  }
  return n;
}

function hashDjb2(str) {
  let h = 5381;
  for (let i = 0; i < str.length; i++) {
    h = Math.imul(h, 33) + str.charCodeAt(i);
  }
  return (h >>> 0).toString(36);
}

function scoreDigestFromInPlayData(data) {
  if (!Array.isArray(data) || !data.length) return "0";
  const chunks = [];
  for (const r of data) {
    const id = r?.dg_id ?? r?.dgId ?? "";
    const cs = r?.current_score ?? r?.currentScore ?? "";
    const td = r?.today ?? r?.Today ?? "";
    chunks.push(`${id}:${cs}:${td}`);
  }
  chunks.sort();
  return `${data.length}:${hashDjb2(chunks.join("|"))}`;
}

function compositeLiveBundleToken(parsed, liveTournamentStats, liveHoleStats, fieldRaw) {
  const parts = [];
  const lu = parsed?.info?.last_update ?? parsed?.last_update;
  if (lu != null) parts.push(`lu:${String(lu).trim()}`);
  if (Array.isArray(parsed?.data)) parts.push(`sc:${scoreDigestFromInPlayData(parsed.data)}`);
  if (liveTournamentStats?.last_updated != null) parts.push(`lts:${String(liveTournamentStats.last_updated).trim()}`);
  const lhu = liveHoleStats?.last_update ?? liveHoleStats?.last_updated;
  if (lhu != null) parts.push(`lhs:${String(lhu).trim()}`);
  const flu = fieldRaw?.last_updated ?? fieldRaw?.last_update ?? fieldRaw?.updated_at;
  if (flu != null) parts.push(`fu:${String(flu).trim()}`);
  return parts.join("|");
}

async function main() {
  const key = loadApiKey();
  if (!key) {
    console.error("[fetch-live-in-play] Set DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json with apiKey.");
    process.exit(1);
  }

  await maybeRebuildProjectionsIfPgaWeekMismatch(key);

  const envPrimary = (process.env.GOLF_DATAGOLF_TOUR || process.env.GOLF_TOUR || "pga").trim().toLowerCase() || "pga";
  const fromProj = datagolfFeedTourFromProjections();
  const primary = fromProj || envPrimary;
  if (fromProj) {
    console.log(`[fetch-live-in-play] tour=${primary} (from projections.datagolf_feed_tour)`);
  } else {
    console.log(`[fetch-live-in-play] tour=${primary} (env; projections has no datagolf_feed_tour yet)`);
  }
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

  let fieldUpdates = null;
  try {
    fieldUpdates = await dgGetJson(fieldUpdatesUrl(key, tourUsed));
  } catch (e) {
    console.warn("[fetch-live-in-play] field-updates:", e.message || e);
  }
  let scoreMergeCount = mergeFieldScoresIntoInPlayRows(parsed.data, fieldUpdates);
  if (scoreMergeCount === 0 && fallback && fallback !== tourUsed) {
    try {
      const fu2 = await dgGetJson(fieldUpdatesUrl(key, fallback));
      const n2 = mergeFieldScoresIntoInPlayRows(parsed.data, fu2);
      if (n2 > 0) {
        scoreMergeCount = n2;
        fieldUpdates = fu2;
      }
    } catch (e) {
      console.warn("[fetch-live-in-play] field-updates fallback:", e.message || e);
    }
  }
  if (scoreMergeCount > 0) {
    console.log(`[fetch-live-in-play] merged field-updates scores onto ${scoreMergeCount} in-play row(s)`);
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
    ...(fieldUpdates && typeof fieldUpdates === "object" ? { field_updates: fieldUpdates } : {}),
    ...(liveTournamentStats && typeof liveTournamentStats === "object"
      ? { live_tournament_stats: liveTournamentStats }
      : {}),
    ...(liveHoleStats && typeof liveHoleStats === "object" ? { live_hole_stats: liveHoleStats } : {}),
  };

  const out = path.join(WEB_ROOT, "live-in-play.json");
  fs.mkdirSync(path.dirname(out), { recursive: true });
  const token = compositeLiveBundleToken(parsed, liveTournamentStats, liveHoleStats, fieldUpdates);
  const pm = readProjectionsRoot();
  const infoEv = String(parsed?.info?.event_name || "").trim();
  const projEv = pm ? String(pm.event_name || "").trim() : "";
  const projInPlayEventMismatch = !!(projEv && infoEv && !eventsLikelySame(projEv, infoEv));
  if (projInPlayEventMismatch) {
    console.warn(
      `[fetch-live-in-play] preds/in-play info.event_name "${infoEv}" vs projections "${projEv}" — forcing disk write (token skip disabled)`
    );
  }
  if (token && fs.existsSync(out) && !projInPlayEventMismatch) {
    try {
      const prev = JSON.parse(fs.readFileSync(out, "utf8"));
      const prevTok = compositeLiveBundleToken(
        { ...prev, data: Array.isArray(prev.data) ? prev.data : [] },
        prev.live_tournament_stats,
        prev.live_hole_stats,
        prev.field_updates
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
    `[fetch-live-in-play] wrote ${out} (${parsed.data.length} players, tour=${tourUsed}, odds_format=${oddsFormat}; field_scores=${scoreMergeCount > 0 ? "yes" : "no"}; live feeds=${liveTournamentStats ? "t" : "-"}${liveHoleStats ? "h" : "-"})`
  );
}

main().catch((e) => {
  console.error("[fetch-live-in-play]", e.message || e);
  process.exit(1);
});
