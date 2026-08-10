#!/usr/bin/env node
/**
 * DataGolf live bundle for the static app:
 * - preds/in-play (placement probs)
 * - field-updates (authoritative live leaderboard `current_score` / to-par when DG exposes it)
 * - preds/live-tournament-stats (per-round Live Tournament Stats — round score / birdies / pars / bogeys for Historical Trends)
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
 * Completed-round gross columns (`R1`–`R3`) are merged forward from the previous on-disk snapshot when the fresh
 * preds/in-play payload omits them after a round rollover (field-updates only refreshes `current_score`).
 * That keeps `build-player-history` / Historical Trends whole during `npm run push:all` until historical-raw-data catches up.
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
import {
  liveRoundActualsRoundCounts,
  resolveLiveRoundActualsByDg,
  fetchLiveTournamentStatsByRound,
  liveTournamentStatsUrl,
} from "./dg-live-tournament-stats.mjs";
import { archivePriorEventLiveBundle } from "./prior-event-live-archive.mjs";

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

/** Gross strokes for completed round `rnd` on preds/in-play row (`R1` / `r1`, …). */
function grossForRoundColumn(row, rnd) {
  const r = Math.round(Number(rnd));
  if (!Number.isFinite(r) || r < 1 || r > 4) return NaN;
  return num(row[`R${r}`] ?? row[`r${r}`], NaN);
}

/**
 * When the tournament advances, preds/in-play sometimes drops completed `R*` gross columns while still
 * supplying `current_score` / today — `npm run push:all` would then overwrite live-in-play.json and
 * `build-player-history` would lose prior-round rows until historical-raw-data catches up.
 * Merge missing prior-round gross from the last on-disk snapshot for the same event.
 */
function mergeCarryForwardPriorRoundGross(dataRows, prevDataRows, tournamentRoundFallback) {
  if (!Array.isArray(dataRows) || !dataRows.length || !Array.isArray(prevDataRows) || !prevDataRows.length)
    return 0;
  const prevByDg = new Map();
  for (const pr of prevDataRows) {
    const id = Math.round(num(pr?.dg_id ?? pr?.dgId));
    if (Number.isFinite(id)) prevByDg.set(id, pr);
  }
  let carried = 0;
  const fb = Math.round(num(tournamentRoundFallback, NaN));
  for (const row of dataRows) {
    const id = Math.round(num(row?.dg_id ?? row?.dgId));
    if (!Number.isFinite(id)) continue;
    const prevRow = prevByDg.get(id);
    if (!prevRow) continue;
    const playerR = Math.round(num(row?.round ?? row?.Round, NaN));
    const prEff =
      Number.isFinite(playerR) && playerR >= 1 && playerR <= 4 ? playerR : Number.isFinite(fb) ? fb : NaN;
    if (!Number.isFinite(prEff) || prEff < 2) continue;
    for (let rnd = 1; rnd < prEff; rnd++) {
      if (Number.isFinite(grossForRoundColumn(row, rnd))) continue;
      const pg = grossForRoundColumn(prevRow, rnd);
      if (!Number.isFinite(pg)) continue;
      row[`R${rnd}`] = pg;
      carried++;
    }
  }
  return carried;
}

function tournamentRoundFallbackFromBundle(parsed, fieldRaw, projectionsRoot) {
  const meta =
    projectionsRoot?.meta && typeof projectionsRoot.meta === "object" ? projectionsRoot.meta : {};
  const fu = fieldRaw && typeof fieldRaw === "object" ? fieldRaw : {};
  /** Max — any single stale source must not suppress a higher verified round */
  let best = NaN;
  const push = (cand) => {
    const rn = Math.round(num(cand));
    if (!Number.isFinite(rn) || rn < 1 || rn > 4) return;
    best = Number.isFinite(best) ? Math.max(best, rn) : rn;
  };
  push(meta.datagolf_live_current_round);
  push(meta.display_round);
  push(fu.current_round);
  push(parsed?.info?.current_round);
  push(parsed?.current_round);
  const rows = Array.isArray(parsed?.data) ? parsed.data : [];
  for (const r of rows) push(r?.round);
  return best;
}

function liveBundlesSameEvent(prevBundle, parsed, fieldRaw) {
  const pfu =
    prevBundle?.field_updates && typeof prevBundle.field_updates === "object"
      ? prevBundle.field_updates
      : null;
  const nfu = fieldRaw && typeof fieldRaw === "object" ? fieldRaw : null;
  const pid =
    pfu && pfu.event_id != null && String(pfu.event_id).trim() !== ""
      ? String(pfu.event_id).trim()
      : "";
  const nid =
    nfu && nfu.event_id != null && String(nfu.event_id).trim() !== ""
      ? String(nfu.event_id).trim()
      : "";
  if (pid && nid && pid === nid) return true;
  const pName = String(pfu?.event_name || prevBundle?.info?.event_name || "").trim();
  const nName = String(nfu?.event_name || parsed?.info?.event_name || "").trim();
  if (!pName || !nName) return false;
  return eventsLikelySame(pName, nName);
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
    const rSlots = [1, 2, 3, 4].map((k) => String(r[`R${k}`] ?? r[`r${k}`] ?? "")).join(",");
    chunks.push(`${id}:${cs}:${td}:${rSlots}`);
  }
  chunks.sort();
  return `${data.length}:${hashDjb2(chunks.join("|"))}`;
}

function compositeLiveBundleToken(parsed, liveTournamentStats, liveHoleStats, fieldRaw, liveRoundActualsByDg) {
  const parts = [];
  const lu = parsed?.info?.last_update ?? parsed?.last_update;
  if (lu != null) parts.push(`lu:${String(lu).trim()}`);
  if (Array.isArray(parsed?.data)) parts.push(`sc:${scoreDigestFromInPlayData(parsed.data)}`);
  if (liveTournamentStats?.last_updated != null) parts.push(`lts:${String(liveTournamentStats.last_updated).trim()}`);
  const lhu = liveHoleStats?.last_update ?? liveHoleStats?.last_updated;
  if (lhu != null) parts.push(`lhs:${String(lhu).trim()}`);
  const flu = fieldRaw?.last_updated ?? fieldRaw?.last_update ?? fieldRaw?.updated_at;
  if (flu != null) parts.push(`fu:${String(flu).trim()}`);
  if (liveRoundActualsByDg && typeof liveRoundActualsByDg === "object") {
    const rc = liveRoundActualsRoundCounts(liveRoundActualsByDg);
    parts.push(`lra:${rc["1"] || 0},${rc["2"] || 0},${rc["3"] || 0},${rc["4"] || 0}`);
  }
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

  const liveOutPath = path.join(WEB_ROOT, "live-in-play.json");
  let prevDiskBundle = null;
  try {
    if (fs.existsSync(liveOutPath)) {
      prevDiskBundle = JSON.parse(fs.readFileSync(liveOutPath, "utf8"));
    }
  } catch {
    prevDiskBundle = null;
  }
  const projectionsSnapshot = readProjectionsRoot();
  const fbRound = tournamentRoundFallbackFromBundle(parsed, fieldUpdates, projectionsSnapshot);
  if (
    prevDiskBundle &&
    Array.isArray(prevDiskBundle.data) &&
    prevDiskBundle.data.length &&
    liveBundlesSameEvent(prevDiskBundle, parsed, fieldUpdates)
  ) {
    const nCarry = mergeCarryForwardPriorRoundGross(parsed.data, prevDiskBundle.data, fbRound);
    if (nCarry > 0) {
      console.log(
        `[fetch-live-in-play] carried forward ${nCarry} prior-round gross score slot(s) from previous live-in-play.json`,
      );
    }
  }

  let liveTournamentStats = null;
  let liveTournamentStatsByRound = null;
  let liveRoundActualsByDg = null;
  let liveHoleStats = null;
  try {
    liveTournamentStats = await dgGetJson(
      liveTournamentStatsUrl(
        key,
        String(process.env.GOLF_LIVE_TOURNAMENT_STATS_ROUND || "event_avg").trim() || "event_avg",
      ),
    );
  } catch (e) {
    console.warn("[fetch-live-in-play] live-tournament-stats (event_avg):", e.message || e);
  }
  try {
    liveTournamentStatsByRound = await fetchLiveTournamentStatsByRound(key, dgGetJson);
    const inPlayByDg = new Map();
    for (const row of parsed.data) {
      const id = Math.round(num(row?.dg_id ?? row?.dgId, NaN));
      if (Number.isFinite(id)) inPlayByDg.set(id, row);
    }
    const fu = fieldUpdates && typeof fieldUpdates === "object" ? fieldUpdates : {};
    const roundPar = num(fu.course_par ?? fu.coursePar ?? projectionsSnapshot?.course_par_18, NaN);
    const draftBundle = {
      data: parsed.data,
      field_updates: fu,
      live_tournament_stats_by_round: liveTournamentStatsByRound,
    };
    liveRoundActualsByDg = resolveLiveRoundActualsByDg(draftBundle, {
      roundPar: Number.isFinite(roundPar) ? roundPar : 72,
    });
    const nPlayers = Object.keys(liveRoundActualsByDg).length;
    const rc = liveRoundActualsRoundCounts(liveRoundActualsByDg);
    if (nPlayers > 0) {
      console.log(
        `[fetch-live-in-play] live round actuals: ${nPlayers} player(s); R1=${rc["1"] || 0} R2=${rc["2"] || 0} R3=${rc["3"] || 0} R4=${rc["4"] || 0} with gross`,
      );
    }
  } catch (e) {
    console.warn("[fetch-live-in-play] live-tournament-stats by round:", e.message || e);
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
    ...(liveTournamentStatsByRound && typeof liveTournamentStatsByRound === "object"
      ? { live_tournament_stats_by_round: liveTournamentStatsByRound }
      : {}),
    ...(liveRoundActualsByDg && typeof liveRoundActualsByDg === "object"
      ? { live_round_actuals_by_dg: liveRoundActualsByDg }
      : {}),
    ...(liveHoleStats && typeof liveHoleStats === "object" ? { live_hole_stats: liveHoleStats } : {}),
  };

  fs.mkdirSync(path.dirname(liveOutPath), { recursive: true });
  const token = compositeLiveBundleToken(
    parsed,
    liveTournamentStats,
    liveHoleStats,
    fieldUpdates,
    liveRoundActualsByDg,
  );
  const pm = readProjectionsRoot();
  const infoEv = String(parsed?.info?.event_name || "").trim();
  const projEv = pm ? String(pm.event_name || "").trim() : "";
  const fuEv = String(fieldUpdates?.event_name ?? fieldUpdates?.eventName ?? "").trim();
  const projInPlayEventMismatch = !!(projEv && infoEv && !eventsLikelySame(projEv, infoEv));
  if (projInPlayEventMismatch) {
    console.warn(
      `[fetch-live-in-play] preds/in-play info.event_name "${infoEv}" vs projections "${projEv}" — stale in-play event metadata detected`,
    );
    // When field-updates already rolled to the projections week, write a pre-event skeleton
    // so Live Stats / Trends never keep last week's LTS+leaderboard under a new header.
    const fuAlignsProj = !!(projEv && fuEv && eventsLikelySame(projEv, fuEv));
    if (fuAlignsProj && fieldUpdates && typeof fieldUpdates === "object") {
      // Keep completed prior-week R1–R4 actuals for projection-tracker backfill after we clear live.
      try {
        const archived = archivePriorEventLiveBundle(bundle, { eventName: infoEv });
        if (archived?.ok) {
          console.warn(
            `[fetch-live-in-play] archived prior-event live bundle "${archived.eventName}" ` +
              `(R1=${archived.posted?.["1"] || 0} R2=${archived.posted?.["2"] || 0} ` +
              `R3=${archived.posted?.["3"] || 0} R4=${archived.posted?.["4"] || 0}) -> ${archived.path}`,
          );
        } else if (archived?.skipped) {
          console.warn(
            `[fetch-live-in-play] prior-event archive skip "${archived.eventName}" (${archived.reason})`,
          );
        }
      } catch (e) {
        console.warn("[fetch-live-in-play] prior-event archive failed:", e?.message || e);
      }
      const skeleton = {
        data: [],
        info: {
          event_name: projEv,
          current_round: Math.round(num(fieldUpdates.current_round, 0)) || 0,
          last_update: new Date().toISOString(),
          pre_event: true,
          note: `Awaiting preds/in-play for ${projEv} (DataGolf still serving "${infoEv}")`,
        },
        field_updates: fieldUpdates,
        live_tournament_stats: null,
        live_tournament_stats_by_round: {},
        live_round_actuals_by_dg: {},
        live_hole_stats:
          liveHoleStats &&
          typeof liveHoleStats === "object" &&
          eventsLikelySame(String(liveHoleStats.event_name || ""), projEv)
            ? liveHoleStats
            : null,
        live_stats_pre_event: true,
        field_updates_refreshed_at: new Date().toISOString(),
      };
      fs.writeFileSync(liveOutPath, JSON.stringify(skeleton, null, 2), "utf8");
      console.warn(
        `[fetch-live-in-play] wrote pre-event live-in-play.json for "${projEv}" (cleared stale "${infoEv}" LTS/data)`,
      );
      return;
    }
    if (prevDiskBundle && pm) {
      const prevEv = String(
        prevDiskBundle?.field_updates?.event_name || prevDiskBundle?.info?.event_name || "",
      ).trim();
      if (prevEv && eventsLikelySame(prevEv, projEv)) {
        console.warn(
          `[fetch-live-in-play] keeping existing live-in-play.json for "${prevEv}" — skip write (DataGolf in-play still on "${infoEv}")`,
        );
        return;
      }
    }
    console.warn(
      `[fetch-live-in-play] skip write — would overwrite projections week with stale in-play from "${infoEv}"`,
    );
    return;
  }
  if (token && fs.existsSync(liveOutPath)) {
    try {
      const prev = JSON.parse(fs.readFileSync(liveOutPath, "utf8"));
      const prevTok = compositeLiveBundleToken(
        { ...prev, data: Array.isArray(prev.data) ? prev.data : [] },
        prev.live_tournament_stats,
        prev.live_hole_stats,
        prev.field_updates,
        prev.live_round_actuals_by_dg,
      );
      if (prevTok && prevTok === token) {
        if (projInPlayEventMismatch) {
          const prevInfoEv = String(prev?.info?.event_name || "").trim();
          if (prevInfoEv && !eventsLikelySame(prevInfoEv, projEv)) {
            console.warn(
              `[fetch-live-in-play] unchanged stale in-play bundle token (${token}); skip write to avoid refresh loop`
            );
          } else {
            console.warn(
              `[fetch-live-in-play] unchanged bundle token (${token}) with in-play/projections event mismatch; skip write`
            );
          }
        }
        console.log(`[fetch-live-in-play] unchanged bundle token (${token}); skip write`);
        return;
      }
    } catch {
      /* rewrite if parse fails */
    }
  }
  fs.writeFileSync(liveOutPath, JSON.stringify(bundle, null, 2), "utf8");
  console.log(
    `[fetch-live-in-play] wrote ${liveOutPath} (${parsed.data.length} players, tour=${tourUsed}, odds_format=${oddsFormat}; field_scores=${scoreMergeCount > 0 ? "yes" : "no"}; live feeds=${liveTournamentStats ? "t" : "-"}${liveHoleStats ? "h" : "-"})`
  );
}

main().catch((e) => {
  console.error("[fetch-live-in-play]", e.message || e);
  process.exit(1);
});
