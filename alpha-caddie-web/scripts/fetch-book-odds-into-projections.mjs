#!/usr/bin/env node
/**
 * Refresh sportsbook columns: fetches DataGolf finish-position outrights API + matchup odds,
 * then replaces parseable outright book columns with scraped sportsbook pages.
 *
 * Use between full `npm run fetch:dg` / R exports so the static app sees current lines without rebuilding players.
 * Preflight: compares `/field-updates` to projections (week key + fuzzy title); on mismatch runs `fetch-datagolf.mjs`
 * then re-reads JSON. Compares `GOLF_DATAGOLF_TOUR`/`pga` field-updates when feed tour differs so a stuck `liv`/snapshot
 * cannot self-match forever.
 * Skip with GOLF_SKIP_INLINE_FETCH_DG_ON_EVENT_MISMATCH=1.
 *
 *   npm run fetch:book-odds
 *
 * Round projections tab (DK only, no outrights/matchups): npm run update:dk-round-projections
 *
 * Env: DATAGOLF_API_KEY or datagolf.local.json; GOLF_MODEL_DIR (repo root); GOLF_DATAGOLF_TOUR / GOLF_TOUR fallback when
 *      projections.json lacks datagolf_feed_tour (written by fetch-datagolf when multiple tours are compared).
 *      Outright EV rows: DataGolf betting-tools/outrights API, which backs the Finish Position Betting Tool.
 *      GOLF_SKIP_PROPS_CSV=1 — do not merge data/player_props_*.csv into projections.props (Model O/U DK lines).
 *      GOLF_SKIP_DK_OU=1 — do not pull DK round props (see draftkings-ou-props.mjs).
 *      GOLF_SKIP_PP_OU=1 — do not pull PrizePicks round props (see prizepicks-ou-props.mjs).
 *      GOLF_SKIP_SL_OU=1 — do not pull Sleeper round props (see sleeper-ou-props.mjs).
 *      GOLF_SKIP_UD_OU=1 — do not pull Underdog round props (see underdog-ou-props.mjs).
 *      GOLF_SKIP_FD_OU=1 — do not pull FanDuel round props (see fanduel-ou-props.mjs).
 *      GOLF_SKIP_KL_OU=1 — do not pull Kalshi round-score props (see kalshi-ou-props.mjs).
 *      GOLF_SKIP_CZR_OU=1 — do not pull Caesars round props (see caesars-ou-props.mjs).
 *      GOLF_REQUIRE_DK_OU=1 — abort when DK scrape returns 0 fresh props (default on push:live / refresh:live).
 *      GOLF_REQUIRE_PP_OU=1 — abort when PrizePicks fetch returns 0 fresh props (optional).
 *      GOLF_REQUIRE_SL_OU=1 / GOLF_REQUIRE_UD_OU=1 — abort when Sleeper/Underdog return 0 rows (optional).
 *      GOLF_REQUIRE_FD_OU=1 / GOLF_REQUIRE_KL_OU=1 / GOLF_REQUIRE_CZR_OU=1 — abort when those books return 0 (optional).
 *      DK_HEADLESS=0 — headed Chromium on Windows/macOS (DK blocks headless Nash API).
 *      GOLF_SKIP_DK_ROUND_AUDIT_CSV=1 — do not append alpha-caddie-web/data/dk_round_projection_audit.csv after merge.
 *      GOLF_SKIP_MODEL_FALLBACK_OU=1 — do not synthesize GIR / fairways / putts from projections.players for players DK omits.
 *      GIR/FW/Putts: each run drops stale csv+model_fallback rows for those markets, then re-adds model lines from current
 *      payload.players for any player DK does not supply (DK rows stay authoritative when present).
 *
 * DraftKings round props (Birdies, Pars, Bogeys, Round Score → Total Score) use Playwright + Chromium.
 * Production (Render): `playwright` is a runtime dependency; build should run `npx playwright install chromium`.
 * Point DK at the active event: DK_LEAGUE_URL (e.g. …/leagues/golf/{event}?category=round) and DK_LEAGUE_ID (from DK URL).
 * If DK_LEAGUE_URL is unset, uses projections.event_name → slug (override with dk_league_slug on JSON or set DK_LEAGUE_URL when DK’s slug differs).
 * CSV files still override or fill gaps when DK omits a player or market.
 */
import { spawnSync } from "child_process";
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  coursesClearlyDistinct,
  eventsLikelySame,
  fieldWeekKey,
  fieldWeekKeysRoughMatch,
  titleTokenOverlapRatio,
  tokenizeEventTitle,
} from "./dg-events-align.mjs";
import { fetchDataGolfOutrightsApi } from "./datagolf-outrights-api.mjs";
import { fetchSportsbookOutrightsFromUrls } from "./sportsbook-outrights-scraper.mjs";
import { appendDkRoundProjectionAuditCsv } from "./export-dk-round-model-audit-csv.mjs";
import { appendPpRoundProjectionAuditCsv } from "./export-pp-round-model-audit-csv.mjs";
import {
  appendCzrRoundProjectionAuditCsv,
  appendFdRoundProjectionAuditCsv,
  appendKlRoundProjectionAuditCsv,
  appendSlRoundProjectionAuditCsv,
  appendUdRoundProjectionAuditCsv,
} from "./export-pickem-round-model-audit-csv.mjs";
import { refreshCaesarsRoundProps } from "./merge-caesars-round-props.mjs";
import { refreshRoundProjectionProps } from "./merge-dk-round-props.mjs";
import { refreshFanduelRoundProps } from "./merge-fanduel-round-props.mjs";
import { refreshKalshiRoundProps } from "./merge-kalshi-round-props.mjs";
import { refreshPrizePicksRoundProps } from "./merge-pp-round-props.mjs";
import { refreshSleeperRoundProps } from "./merge-sleeper-round-props.mjs";
import { refreshUnderdogRoundProps } from "./merge-underdog-round-props.mjs";
import { bakePaperBookLines } from "./bake-paper-book-lines.mjs";
import { projectionExportMeta } from "./projection-export-meta.mjs";
const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");
const ENV_DEFAULT_TOUR = ((process.env.GOLF_DATAGOLF_TOUR || process.env.GOLF_TOUR || "pga").trim() || "pga").toLowerCase();

function envTruthy(name, defaultVal = false) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function loadApiKey() {
  const env = (process.env.DATAGOLF_API_KEY || "").trim();
  if (env) return env;
  const p = join(WEB_ROOT, "datagolf.local.json");
  if (existsSync(p)) {
    try {
      const j = JSON.parse(readFileSync(p, "utf8"));
      return String(j.apiKey || j.key || "").trim();
    } catch {
      return "";
    }
  }
  return "";
}

/** Rough player-list shape from field-updates (matches fetch-datagolf parsing intent). */
function fieldRowsFromUpdates(raw) {
  if (!raw || typeof raw !== "object") return [];
  if (Array.isArray(raw.field) && raw.field.length) return raw.field;
  for (const k of ["data", "players", "baseline_history_fit", "baseline"]) {
    const v = raw[k];
    if (Array.isArray(v) && v.length) return v;
  }
  return [];
}

async function fetchDg(path, params, key) {
  const u = new URL(`https://feeds.datagolf.com${path}`);
  for (const [k, v] of Object.entries(params)) u.searchParams.set(k, String(v));
  u.searchParams.set("key", key);
  const res = await fetch(u.toString(), { headers: { Accept: "application/json" } });
  if (!res.ok) throw new Error(`${path} HTTP ${res.status}: ${await res.text().catch(() => "")}`);
  return res.json();
}

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function mergeScrapedOutrightsIntoMarket(existingPack, scrapedPack) {
  if (!scrapedPack || !Array.isArray(scrapedPack.rows) || !scrapedPack.rows.length) return existingPack;
  const useScrapedDraftKings = String(process.env.GOLF_SKIP_SCRAPED_DK_OUTRIGHTS || "").trim() !== "1";
  const baseById = new Map();
  for (const r of Array.isArray(existingPack?.rows) ? existingPack.rows : []) {
    const id = Math.round(num(r.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    baseById.set(id, {
      ...r,
      dg_id: id,
      player_name: String(r.player_name || "").trim(),
    });
  }
  for (const r of scrapedPack.rows) {
    const id = Math.round(num(r.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const out = baseById.get(id) || { dg_id: id, player_name: String(r.player_name || "").trim() };
    const dgModel = num(r.dg_model, NaN);
    if (Number.isFinite(dgModel) && dgModel > 0) out.dg_model = dgModel;
    for (const bk of scrapedPack.bookKeys || []) {
      if (String(bk).toLowerCase() === "draftkings" && !useScrapedDraftKings) continue;
      const pct = num(r[bk], NaN);
      if (Number.isFinite(pct) && pct !== 0) out[bk] = pct;
    }
    baseById.set(id, out);
  }
  const rows = [...baseById.values()];
  const bookKeys = new Set(Array.isArray(existingPack?.bookKeys) ? existingPack.bookKeys.map((bk) => String(bk).toLowerCase()) : []);
  for (const bk of scrapedPack.bookKeys || []) {
    const k = String(bk).toLowerCase();
    if (k === "draftkings" && !useScrapedDraftKings) continue;
    bookKeys.add(k);
  }
  return { rows, bookKeys: [...bookKeys].sort() };
}

function mergeScrapedOutrights(outrights, scrapedOutrights) {
  const next = { ...(outrights && typeof outrights === "object" ? outrights : {}) };
  for (const [market, scrapedPack] of Object.entries(scrapedOutrights || {})) {
    next[market] = mergeScrapedOutrightsIntoMarket(next[market], scrapedPack);
  }
  return next;
}

function scrapedOutrightsHasBook(scrapedOutrights, bookKey) {
  const want = String(bookKey || "").toLowerCase();
  for (const pack of Object.values(scrapedOutrights || {})) {
    const keys = Array.isArray(pack?.bookKeys) ? pack.bookKeys : [];
    if (keys.some((k) => String(k).toLowerCase() === want)) return true;
  }
  return false;
}

function removeBookFromOutrights(outrights, bookKey) {
  const want = String(bookKey || "").toLowerCase();
  const next = { ...(outrights && typeof outrights === "object" ? outrights : {}) };
  for (const [market, pack] of Object.entries(next)) {
    if (!pack || typeof pack !== "object") continue;
    const rows = Array.isArray(pack.rows)
      ? pack.rows.map((row) => {
          const out = { ...row };
          for (const k of Object.keys(out)) {
            if (String(k).toLowerCase() === want) delete out[k];
          }
          return out;
        })
      : pack.rows;
    const bookKeys = Array.isArray(pack.bookKeys)
      ? pack.bookKeys.filter((k) => String(k).toLowerCase() !== want)
      : pack.bookKeys;
    next[market] = { ...pack, rows, bookKeys };
  }
  return next;
}

async function main() {
  const key = loadApiKey();
  if (!key) {
    console.error("Missing API key. Set DATAGOLF_API_KEY or datagolf.local.json.");
    process.exit(1);
  }

  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("Missing", projPath);
    process.exit(1);
  }

  let payload;
  try {
    payload = JSON.parse(readFileSync(projPath, "utf8"));
  } catch (e) {
    console.error("Could not parse projections.json:", e.message);
    process.exit(1);
  }
  if (!payload || typeof payload !== "object") {
    console.error("Invalid projections.json root");
    process.exit(1);
  }

  const tourForFeeds = String(payload.datagolf_feed_tour || "").trim().toLowerCase() || ENV_DEFAULT_TOUR;

  /** DataGolf week rotated but git/deploy snapshot still has last event → merge odds only locks stale field. */
  const skipInlineDg = String(process.env.GOLF_SKIP_INLINE_FETCH_DG_ON_EVENT_MISMATCH || "").trim() === "1";
  if (!skipInlineDg) {
    try {
      const fu = await fetchDg("/field-updates", { tour: tourForFeeds, file_format: "json" }, key);
      /** Calendar week for staleness: always compare to ENV_DEFAULT_TOUR (usually `pga`), not only `datagolf_feed_tour`.
       * Otherwise a stuck `liv` / `opp` snapshot matches itself forever and never runs inline fetch:dg. */
      let fuCal = fu;
      if (tourForFeeds !== ENV_DEFAULT_TOUR) {
        try {
          fuCal = await fetchDg("/field-updates", { tour: ENV_DEFAULT_TOUR, file_format: "json" }, key);
        } catch (e) {
          console.warn(`[fetch-book-odds] field-updates ${ENV_DEFAULT_TOUR}:`, e.message || e);
        }
      }
      const fuEventFeed = String(fu.event_name || fu.eventName || "").trim();
      const fuCourseFeed = String(fu.course_name || fu.courseName || fu.course || "").trim();
      const calEvent = String(fuCal.event_name || fuCal.eventName || "").trim() || fuEventFeed;
      const calCourse = String(fuCal.course_name || fuCal.courseName || fuCal.course || "").trim() || fuCourseFeed;
      const fuRows = fieldRowsFromUpdates(fuCal).filter((p) => {
        if (!p || typeof p !== "object") return false;
        const id = num(p.dg_id ?? p.dgId, NaN);
        const pn = String(p.player_name || p.name || p.playerName || "").trim();
        return Number.isFinite(id) && pn.length > 0;
      });
      const projEvent = String(payload.event_name || "").trim();
      const fuKey = fieldWeekKey(calEvent, calCourse);
      const projKey = String(payload.datagolf_field_week_key || "").trim() || fieldWeekKey(projEvent, String(payload.course_used || ""));
      const hasFu = calEvent && fuRows.length >= 8;
      const keysOk = fieldWeekKeysRoughMatch(projKey, fuKey);
      const nameFuzzyOk = !!(projEvent && calEvent && eventsLikelySame(projEvent, calEvent));
      const ta = tokenizeEventTitle(projEvent);
      const tb = tokenizeEventTitle(calEvent);
      const tokenStale =
        projEvent &&
        calEvent &&
        ta.length >= 3 &&
        tb.length >= 3 &&
        titleTokenOverlapRatio(projEvent, calEvent) < 0.38;
      const courseStale = !!(projEvent && calEvent && nameFuzzyOk && coursesClearlyDistinct(payload.course_used, calCourse));

      const staleWeek =
        hasFu &&
        (!projEvent ||
          !keysOk ||
          !nameFuzzyOk ||
          courseStale ||
          tokenStale);

      console.log(
        `[fetch-book-odds] field-updates sync: feedTour=${tourForFeeds} vs calendarTour=${ENV_DEFAULT_TOUR} feed="${fuEventFeed}" calendar="${calEvent}" projKey=${JSON.stringify(
          projKey,
        )} fuKey=${JSON.stringify(fuKey)} keysOk=${keysOk} fuzzy=${nameFuzzyOk} stale=${staleWeek}`
      );

      if (staleWeek) {
        console.warn(
          `[fetch-book-odds] projections look stale vs ${ENV_DEFAULT_TOUR} field-updates ("${projEvent || "(none)"}" vs "${calEvent}") — running fetch:dg …`
        );
        const dgScript = join(WEB_ROOT, "scripts", "fetch-datagolf.mjs");
        const r = spawnSync(process.execPath, [dgScript], {
          cwd: WEB_ROOT,
          stdio: "inherit",
          env: {
            ...process.env,
            GOLF_MODEL_DIR: GOLF_MODEL_ROOT,
            DATAGOLF_API_KEY: key,
            GOLF_SKIP_HISTORY_ON_FETCH_DG: "1",
          },
        });
        if (r.status !== 0) {
          console.warn("[fetch-book-odds] fetch:dg exited", r.status, "— merging book odds into existing projections.");
        } else {
          try {
            payload = JSON.parse(readFileSync(projPath, "utf8"));
          } catch (e) {
            console.warn("[fetch-book-odds] could not re-read projections after fetch:dg:", e.message);
          }
        }
      }
    } catch (e) {
      console.warn("[fetch-book-odds] field-updates preflight:", e.message || e);
    }
  }

  let outrights = { ...(payload.outrights && typeof payload.outrights === "object" ? payload.outrights : {}) };
  try {
    console.log("Fetching DataGolf betting-tools/outrights for finish-position EV data…");
    const dgOutrights = await fetchDataGolfOutrightsApi({ apiKey: key, tour: tourForFeeds, oddsFormat: "percent" });
    for (const msg of dgOutrights.logs || []) console.log("[datagolf-outrights]", msg);
    if (dgOutrights.outrights && Object.keys(dgOutrights.outrights).length) {
      outrights = mergeScrapedOutrights(outrights, dgOutrights.outrights);
    }
  } catch (e) {
    console.warn("[datagolf-outrights] skipped; keeping existing outright rows:", e.message || e);
  }

  let sportsbookOutrights = {};
  try {
    console.log("Scraping sportsbook outright/finish pages…");
    const scraped = await fetchSportsbookOutrightsFromUrls({ players: payload.players });
    sportsbookOutrights = scraped.outrights || {};
    for (const msg of scraped.logs || []) console.log("[sportsbook-outrights]", msg);
    const n = Object.values(sportsbookOutrights).reduce((sum, pack) => sum + (Array.isArray(pack?.rows) ? pack.rows.length : 0), 0);
    if (n > 0) {
      if (scrapedOutrightsHasBook(sportsbookOutrights, "draftkings")) {
        outrights = removeBookFromOutrights(outrights, "draftkings");
      }
      Object.assign(outrights, mergeScrapedOutrights(outrights, sportsbookOutrights));
      console.log(`[sportsbook-outrights] Replaced DataGolf API sportsbook columns on ${Object.keys(sportsbookOutrights).join(", ")} with ${n} scraped rows.`);
    } else {
      console.warn("[sportsbook-outrights] No parseable sportsbook rows found; keeping existing DataGolf API book columns.");
    }
  } catch (e) {
    console.warn("[sportsbook-outrights] skipped:", e.message || e);
  }
  outrights = removeBookFromOutrights(outrights, "datagolf");

  const matchupMarkets = ["tournament_matchups", "round_matchups", "3_balls"];
  const matchups = { ...(payload.matchups && typeof payload.matchups === "object" ? payload.matchups : {}) };
  const projEventForMatchups = String(payload.event_name || "").trim();
  for (const m of matchupMarkets) {
    try {
      console.log(`Fetching betting-tools/matchups (${m})…`);
      const raw = await fetchDg(
        "/betting-tools/matchups",
        { tour: tourForFeeds, market: m, odds_format: "decimal", file_format: "json" },
        key
      );
      if (raw && typeof raw === "object") matchups[m] = raw;
    } catch (e) {
      console.warn(`Matchups ${m} skipped:`, e.message);
    }
  }
  // Odds Screen must never keep last week's matchup board under a new projections event.
  for (const m of matchupMarkets) {
    const pack = matchups[m];
    if (!pack || typeof pack !== "object") continue;
    const mev = String(pack.event_name || "").trim();
    if (!projEventForMatchups || !mev || eventsLikelySame(projEventForMatchups, mev)) continue;
    console.warn(
      `[fetch-book-odds] clearing stale ${m} ("${mev}" ≠ projections "${projEventForMatchups}")`,
    );
    matchups[m] = {
      event_name: projEventForMatchups,
      market: m,
      match_list: [],
      last_updated: new Date().toISOString(),
      stale_cleared: true,
      note: `DataGolf still returned ${mev}; cleared so Odds Screen does not show last week`,
    };
  }

  const next = {
    ...payload,
    outrights,
    matchups,
    outrights_odds_format: "percent",
    matchups_odds_format: "decimal",
    updated_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    book_odds_refreshed_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
  };
  if (next.outrights_model_blend_weight == null) next.outrights_model_blend_weight = 1;
  if (next.outright_win_score_blend == null) next.outright_win_score_blend = 0;
  if (next.outright_live_score_placement_nudge == null) next.outright_live_score_placement_nudge = false;

  if (process.env.GOLF_SKIP_PROPS_CSV !== "1" || process.env.GOLF_SKIP_DK_OU !== "1") {
    let props = [];
    let nCsv = 0;
    let nDk = 0;
    let nDkFresh = 0;
    let dkLeagueSlug = "";
    let dkLeagueUrl = "";
    let dkError = "";
    let dkRoundOuNotPosted = false;
    try {
      ({
        props,
        nCsv,
        nDk,
        nDkFresh,
        dkLeagueSlug,
        dkLeagueUrl,
        dkError,
        dkRoundOuNotPosted,
      } = await refreshRoundProjectionProps(payload, GOLF_MODEL_ROOT));
    } catch (e) {
      console.warn(
        `[fetch-book-odds] DraftKings / round props scrape crashed: ${e?.message || e} — keeping prior props`,
      );
      props = Array.isArray(payload.props) ? payload.props : [];
      dkError = String(e?.message || e);
    }
    if (props.length) {
      next.props = props;
      if (dkLeagueSlug) next.dk_league_slug = dkLeagueSlug;
      const exportMeta = projectionExportMeta(next);
      if (nDkFresh > 0) {
        next.dk_round_props_refreshed_at = next.book_odds_refreshed_at;
        delete exportMeta.dk_round_props_note;
        delete exportMeta.dk_round_ou_not_posted;
      } else if (String(process.env.GOLF_SKIP_DK_OU || "").trim() !== "1") {
        const errNote = String(dkError || "").trim();
        exportMeta.dk_round_props_note = errNote
          ? errNote
          : "DraftKings returned 0 round O/U props — lines may not be posted yet, or the Nash API scrape failed (see push:live logs).";
        if (dkRoundOuNotPosted) exportMeta.dk_round_ou_not_posted = true;
        console.warn(
          `[fetch-book-odds] DraftKings scrape returned 0 fresh props (${dkLeagueUrl || "no league URL"})${dkRoundOuNotPosted ? " — DK has not posted round O/U yet; using model O/U fallback" : " — Round projections tab will be empty until DK lines load"}`,
        );
      }
      console.log(
        "Merged",
        props.length,
        "Model O/U prop rows (CSV:",
        nCsv,
        "rows; DK auto:",
        nDkFresh,
        "fresh /",
        nDk,
        "total DK rows; model O/U for all counting markets where DK omits)",
      );
    }
    if (
      envTruthy("GOLF_REQUIRE_DK_OU") &&
      String(process.env.GOLF_SKIP_DK_OU || "").trim() !== "1" &&
      nDkFresh === 0 &&
      !dkRoundOuNotPosted
    ) {
      const softLive =
        String(process.env.GOLF_LIVE_WEEK_SOFT || "").trim() === "1" ||
        String(process.env.GOLF_SKIP_DK_OU_REQUIRE || "").trim() === "1";
      const priorDk = (Array.isArray(next.props) ? next.props : []).filter(
        (r) => String(r?.source || "").trim().toLowerCase() === "draftkings",
      ).length;
      const err =
        String(dkError || "").trim() ||
        "DraftKings scrape returned 0 fresh round O/U props — Round projections requires real DK lines";
      if (softLive || priorDk > 0) {
        console.warn(
          `[fetch-book-odds] WARN (DK soft): ${err}${priorDk ? ` — keeping ${priorDk} existing draftkings props` : " — continuing with model O/U"}`,
        );
        const exportMetaSoft = projectionExportMeta(next);
        exportMetaSoft.dk_round_props_note = err;
      } else {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_DK_OU=1): ${err}`);
        console.error(
          "[fetch-book-odds] Fix: install Playwright Chromium (npx playwright install chromium); on Windows/macOS use DK_HEADLESS=0 (set automatically by push:live).",
        );
        process.exit(1);
      }
    }
  }

  if (String(process.env.GOLF_SKIP_PP_OU || "").trim() !== "1") {
    try {
      const { props: ppMerged, nPp, ppError } = await refreshPrizePicksRoundProps(next);
      next.props = ppMerged;
      if (nPp > 0) {
        next.pp_round_props_refreshed_at = next.book_odds_refreshed_at;
        console.log(
          `[fetch-book-odds] PrizePicks round props: ${nPp} fresh rows (${ppMerged.length} total props)`,
        );
        try {
          const ppAudit = appendPpRoundProjectionAuditCsv(next);
          if (ppAudit.appended > 0) {
            console.log(`[fetch-book-odds] PP round audit CSV +${ppAudit.appended} rows -> ${ppAudit.path}`);
          }
        } catch (e) {
          console.warn("[fetch-book-odds] PP round audit CSV:", e.message || e);
        }
      } else if (ppError && !String(ppError).startsWith("skipped")) {
        console.warn(`[fetch-book-odds] PrizePicks: ${ppError}`);
      }
      if (envTruthy("GOLF_REQUIRE_PP_OU") && nPp === 0) {
        const err =
          String(ppError || "").trim() ||
          "PrizePicks fetch returned 0 fresh round O/U props — check PP_LEAGUE_ID / API rate limits";
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_PP_OU=1): ${err}`);
        process.exit(1);
      }
    } catch (e) {
      console.warn("[fetch-book-odds] PrizePicks O/U skipped:", e.message || e);
      if (envTruthy("GOLF_REQUIRE_PP_OU")) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_PP_OU=1): ${e.message || e}`);
        process.exit(1);
      }
    }
  }

  if (String(process.env.GOLF_SKIP_SL_OU || "").trim() !== "1") {
    try {
      const { props: slMerged, nSl, slError } = await refreshSleeperRoundProps(next);
      next.props = slMerged;
      if (nSl > 0) {
        next.sl_round_props_refreshed_at = next.book_odds_refreshed_at;
        console.log(
          `[fetch-book-odds] Sleeper round props: ${nSl} fresh rows (${slMerged.length} total props)`,
        );
        try {
          const slAudit = appendSlRoundProjectionAuditCsv(next);
          if (slAudit.appended > 0) {
            console.log(`[fetch-book-odds] SL round audit CSV +${slAudit.appended} rows -> ${slAudit.path}`);
          }
        } catch (e) {
          console.warn("[fetch-book-odds] SL round audit CSV:", e.message || e);
        }
      } else if (slError && !String(slError).startsWith("skipped")) {
        console.warn(`[fetch-book-odds] Sleeper: ${slError}`);
      }
      if (envTruthy("GOLF_REQUIRE_SL_OU") && nSl === 0) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_SL_OU=1): ${slError || "0 Sleeper rows"}`);
        process.exit(1);
      }
    } catch (e) {
      console.warn("[fetch-book-odds] Sleeper O/U skipped:", e.message || e);
      if (envTruthy("GOLF_REQUIRE_SL_OU")) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_SL_OU=1): ${e.message || e}`);
        process.exit(1);
      }
    }
  }

  if (String(process.env.GOLF_SKIP_UD_OU || "").trim() !== "1") {
    try {
      const { props: udMerged, nUd, udError } = await refreshUnderdogRoundProps(next);
      next.props = udMerged;
      if (nUd > 0) {
        next.ud_round_props_refreshed_at = next.book_odds_refreshed_at;
        console.log(
          `[fetch-book-odds] Underdog round props: ${nUd} fresh rows (${udMerged.length} total props)`,
        );
        try {
          const udAudit = appendUdRoundProjectionAuditCsv(next);
          if (udAudit.appended > 0) {
            console.log(`[fetch-book-odds] UD round audit CSV +${udAudit.appended} rows -> ${udAudit.path}`);
          }
        } catch (e) {
          console.warn("[fetch-book-odds] UD round audit CSV:", e.message || e);
        }
      } else if (udError && !String(udError).startsWith("skipped")) {
        console.warn(`[fetch-book-odds] Underdog: ${udError}`);
      }
      if (envTruthy("GOLF_REQUIRE_UD_OU") && nUd === 0) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_UD_OU=1): ${udError || "0 Underdog rows"}`);
        process.exit(1);
      }
    } catch (e) {
      console.warn("[fetch-book-odds] Underdog O/U skipped:", e.message || e);
      if (envTruthy("GOLF_REQUIRE_UD_OU")) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_UD_OU=1): ${e.message || e}`);
        process.exit(1);
      }
    }
  }

  if (String(process.env.GOLF_SKIP_FD_OU || "").trim() !== "1") {
    try {
      const { props: fdMerged, nFd, fdError } = await refreshFanduelRoundProps(next);
      next.props = fdMerged;
      if (nFd > 0) {
        next.fd_round_props_refreshed_at = next.book_odds_refreshed_at;
        console.log(
          `[fetch-book-odds] FanDuel round props: ${nFd} fresh rows (${fdMerged.length} total props)`,
        );
        try {
          const fdAudit = appendFdRoundProjectionAuditCsv(next);
          if (fdAudit.appended > 0) {
            console.log(`[fetch-book-odds] FD round audit CSV +${fdAudit.appended} rows -> ${fdAudit.path}`);
          }
        } catch (e) {
          console.warn("[fetch-book-odds] FD round audit CSV:", e.message || e);
        }
      } else if (fdError && !String(fdError).startsWith("skipped")) {
        console.warn(`[fetch-book-odds] FanDuel: ${fdError}`);
      }
      if (envTruthy("GOLF_REQUIRE_FD_OU") && nFd === 0) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_FD_OU=1): ${fdError || "0 FanDuel rows"}`);
        process.exit(1);
      }
    } catch (e) {
      console.warn("[fetch-book-odds] FanDuel O/U skipped:", e.message || e);
      if (envTruthy("GOLF_REQUIRE_FD_OU")) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_FD_OU=1): ${e.message || e}`);
        process.exit(1);
      }
    }
  }

  if (String(process.env.GOLF_SKIP_KL_OU || "").trim() !== "1") {
    try {
      const { props: klMerged, nKl, klError } = await refreshKalshiRoundProps(next);
      next.props = klMerged;
      if (nKl > 0) {
        next.kl_round_props_refreshed_at = next.book_odds_refreshed_at;
        console.log(
          `[fetch-book-odds] Kalshi round props: ${nKl} fresh rows (${klMerged.length} total props)`,
        );
        try {
          const klAudit = appendKlRoundProjectionAuditCsv(next);
          if (klAudit.appended > 0) {
            console.log(`[fetch-book-odds] KL round audit CSV +${klAudit.appended} rows -> ${klAudit.path}`);
          }
        } catch (e) {
          console.warn("[fetch-book-odds] KL round audit CSV:", e.message || e);
        }
      } else if (klError && !String(klError).startsWith("skipped")) {
        console.warn(`[fetch-book-odds] Kalshi: ${klError}`);
      }
      if (envTruthy("GOLF_REQUIRE_KL_OU") && nKl === 0) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_KL_OU=1): ${klError || "0 Kalshi rows"}`);
        process.exit(1);
      }
    } catch (e) {
      console.warn("[fetch-book-odds] Kalshi O/U skipped:", e.message || e);
      if (envTruthy("GOLF_REQUIRE_KL_OU")) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_KL_OU=1): ${e.message || e}`);
        process.exit(1);
      }
    }
  }

  if (String(process.env.GOLF_SKIP_CZR_OU || "").trim() !== "1") {
    try {
      const { props: czrMerged, nCzr, czrError } = await refreshCaesarsRoundProps(next);
      next.props = czrMerged;
      if (nCzr > 0) {
        next.czr_round_props_refreshed_at = next.book_odds_refreshed_at;
        console.log(
          `[fetch-book-odds] Caesars round props: ${nCzr} fresh rows (${czrMerged.length} total props)`,
        );
        try {
          const czrAudit = appendCzrRoundProjectionAuditCsv(next);
          if (czrAudit.appended > 0) {
            console.log(`[fetch-book-odds] CZR round audit CSV +${czrAudit.appended} rows -> ${czrAudit.path}`);
          }
        } catch (e) {
          console.warn("[fetch-book-odds] CZR round audit CSV:", e.message || e);
        }
      } else if (czrError && !String(czrError).startsWith("skipped")) {
        console.warn(`[fetch-book-odds] Caesars: ${czrError}`);
      }
      if (envTruthy("GOLF_REQUIRE_CZR_OU") && nCzr === 0) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_CZR_OU=1): ${czrError || "0 Caesars rows"}`);
        process.exit(1);
      }
    } catch (e) {
      console.warn("[fetch-book-odds] Caesars O/U skipped:", e.message || e);
      if (envTruthy("GOLF_REQUIRE_CZR_OU")) {
        console.error(`[fetch-book-odds] FAIL (GOLF_REQUIRE_CZR_OU=1): ${e.message || e}`);
        process.exit(1);
      }
    }
  }

  const outJson = JSON.stringify(next, null, 2);
  writeFileSync(projPath, outJson, "utf8");
  console.log("Wrote", projPath, "(outrights + matchups only; players unchanged)");

  const websiteDataDir = join(GOLF_MODEL_ROOT, "website", "public", "data");
  const websiteProj = join(websiteDataDir, "projections.json");
  if (existsSync(websiteDataDir)) {
    writeFileSync(websiteProj, outJson, "utf8");
    console.log("Wrote", websiteProj);
  }

  if (String(process.env.GOLF_SKIP_DK_OU || "").trim() !== "1") {
    const deferAudit = String(process.env.GOLF_DEFER_DK_ROUND_AUDIT_UNTIL_REPAIR || "").trim() === "1";
    if (deferAudit) {
      console.log("[fetch-book-odds] Deferring DK round audit CSV until post-venue repair (refresh:live).");
    } else {
      try {
        const audit = appendDkRoundProjectionAuditCsv(next);
        if (audit.appended > 0) {
          console.log(`[fetch-book-odds] DK round audit CSV +${audit.appended} rows -> ${audit.path}`);
        }
      } catch (e) {
        console.warn("[fetch-book-odds] DK round audit CSV:", e.message || e);
      }
    }
  }

  if (String(process.env.GOLF_SKIP_PAPER_BOOK_BAKE || "").trim() !== "1") {
    try {
      await bakePaperBookLines(next);
    } catch (e) {
      console.warn("[fetch-book-odds] paper book bake:", e.message || e);
    }
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
