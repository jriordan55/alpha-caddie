/**
 * AlphaCaddie — demo grid is bundled; over HTTP the app loads projections.json (same schema as
 * scripts/export_projections_for_website.R after round_projections.R → simulated_round_static.rds).
 * Export writes both website/public/data/projections.json and alpha-caddie-web/projections.json.
 * Background projections reload: off by default. Enable with ?poll=30 (15–3600s) or meta.projections_poll_interval_sec.
 * Disable explicitly: ?poll=0. Value 0 in meta turns polling off.
 * Override URL: ?projections=/path.json
 * or window.__ALPHA_CADDIE_PROJECTIONS_URL__. Round history: embedded-player-round-history.js;
 * player_round_history.json + player_shots_web.json when served over HTTP.
 *
 * Live bundle (DataGolf): live-in-play.json next to projections.json — preds/in-play plus optional
 * `field_updates` (DataGolf field-updates API scores merged into `data[]`), live_tournament_stats / live_hole_stats (npm run fetch:in-play). Hole-level avg vs par sets
 * meta.live_course_round_excess_strokes for O/U and props mu (even when pricing mode is default).
 * preds/in-play `thru` / `today` merge onto players for display / outrights; **round-level model odds**
 * (Model O/U, round matchups, 3-balls) ignore those hooks unless `meta.in_play_affects_round_odds` is true.
 * **+EV outright rows:** When live tournament context exists (`meta.datagolf_live_*`), model probs default to
 * **leaderboard `current_score`** (softmax win; Monte Carlo noisy ranks for top 5/10/20), not DG preds/outrights fair.
 * Opt out: `meta.outright_ev_live_leaderboard_model: false`. Optional `outright_ev_live_leaderboard_sigma` (stroke RMSE, default 2.25),
 * `outright_ev_live_leaderboard_mc_sims` (default 420, min 100, max 2500).
 * Tournament matchups can still use live win-share blend when that flag is on.
 * projections.json: player win/top_* are implied probs (0–1) from preds/pre-tournament (default API decimal odds).
 * Outrights book columns: implied % (0–100) from DataGolf `betting-tools/outrights` (same markets as
 * https://datagolf.com/betting-tool-finish; fetch scripts default odds_format=percent to match IMPLIED %). Over HTTP
 * the app can refetch live-in-play.json on an interval when meta.poll_datagolf_live_predictions is true
 * or ?liveOverlay=1 / ?liveInPlay=1. Opt out: ?liveInPlay=0 or ?liveInPlayPoll=0. Poll interval: ?liveInPlayPoll=90
 * (seconds, 15–600). Never embed a DG API key in the browser.
 */

const OU_HOLD = 0.048;
const OU_DEFAULT_ODDS_AM = -110;

/**
 * When true in projections.json `meta`, preds/in-play mid-round fields shift **round** model odds
 * (O/U live thru/today, round_matchups / 3-ball SG delta, matchup win-share blend). Default false.
 */
function inPlayAffectsRoundOdds() {
  return DATA?.meta?.in_play_affects_round_odds === true;
}

/**
 * Max model-implied / book-implied ratio before we hide outright EV (stale projections.json model vs
 * updated book cells — otherwise EV shows thousands of %). Real edges this wide are extremely rare.
 */
const OUTRIGHT_EV_MAX_MODEL_TO_BOOK_RATIO = Object.freeze({
  win: 28,
  top_5: 18,
  top_10: 12,
  top_20: 8,
  make_cut: 10,
  mc: 10,
  frl: 28,
});

/** @returns {number} EV or NaN if ratio implausible or inputs invalid */
function outrightEvFromModelAndBook(modelP, pBook, marketKey) {
  if (!Number.isFinite(modelP) || !Number.isFinite(pBook) || modelP <= 0 || pBook <= 0 || pBook >= 1) return NaN;
  const cap = OUTRIGHT_EV_MAX_MODEL_TO_BOOK_RATIO[marketKey];
  if (!Number.isFinite(cap) || cap <= 0) return NaN;
  if (modelP / pBook > cap) return NaN;
  return modelP / pBook - 1;
}
const PROPS_HISTORY_ROUND_MIN = 1;
/** Upper bound for the “Rounds” window; raise if bundles store more per player. */
const PROPS_HISTORY_ROUND_MAX = 2000;
const PROPS_HISTORY_ROUND_DEFAULT = 50;
/** Min qualifying rounds with stat data to appear in Historical Trends top-10 table (all courses). */
const PROPS_TOP_HIT_MIN_ROUNDS = 20;

const OU_LINE_RANGES = {
  "Total score": [67.5, 68.5, 69.5, 70.5, 71.5, 72.5, 73.5],
  Birdies: [0.5, 1.5, 2.5, 3.5, 4.5, 5.5],
  Pars: [8.5, 9.5, 10.5, 11.5, 12.5, 13.5],
  Bogeys: [0.5, 1.5, 2.5, 3.5, 4.5, 5.5],
  GIR: [8.5, 9.5, 10.5, 11.5, 12.5, 13.5],
  "Fairways hit": [5.5, 6.5, 7.5, 8.5, 9.5, 10.5],
};

/** DataGolf / OWGR-style codes → flagcdn.com slug (lowercase). */
const GOLF_COUNTRY_TO_FLAG = {
  usa: "us",
  us: "us",
  eng: "gb-eng",
  england: "gb-eng",
  sco: "gb-sct",
  scotland: "gb-sct",
  wal: "gb-wls",
  wales: "gb-wls",
  nir: "gb-nir",
  ireland: "ie",
  irl: "ie",
  deu: "de",
  ger: "de",
  fra: "fr",
  esp: "es",
  ita: "it",
  swe: "se",
  nor: "no",
  den: "dk",
  fin: "fi",
  bel: "be",
  ned: "nl",
  nld: "nl",
  aut: "at",
  che: "ch",
  sui: "ch",
  pol: "pl",
  cze: "cz",
  zaf: "za",
  rsa: "za",
  aus: "au",
  nzl: "nz",
  jpn: "jp",
  kor: "kr",
  tpe: "tw",
  can: "ca",
  mex: "mx",
  arg: "ar",
  chi: "cl",
  col: "co",
  ven: "ve",
  per: "pe",
  bra: "br",
  pry: "py",
  uru: "uy",
  ind: "in",
  tha: "th",
  mas: "my",
  sgp: "sg",
  phl: "ph",
  phi: "ph",
  idn: "id",
  chn: "cn",
  rus: "ru",
  ukr: "ua",
  sle: "si",
  svn: "si",
  srb: "rs",
  hrv: "hr",
  isl: "is",
  prt: "pt",
  tur: "tr",
  egy: "eg",
  mar: "ma",
  jam: "jm",
  fij: "fj",
  pan: "pa",
  par: "py",
};

function golfCountryToFlagSlug(countryRaw) {
  const k = String(countryRaw || "")
    .trim()
    .toLowerCase()
    .replace(/\./g, "");
  if (!k) return "";
  if (GOLF_COUNTRY_TO_FLAG[k]) return GOLF_COUNTRY_TO_FLAG[k];
  if (/^[a-z]{2}$/.test(k)) return k;
  return "";
}

function propsFlagPlaceholderDataUri() {
  return `data:image/svg+xml,${encodeURIComponent(
    '<svg xmlns="http://www.w3.org/2000/svg" width="72" height="54" viewBox="0 0 72 54"><rect fill="#2a2d33" width="72" height="54" rx="8"/><text x="36" y="32" text-anchor="middle" fill="#8b8f9c" font-size="11" font-family="system-ui,sans-serif">—</text></svg>'
  )}`;
}

function flagImageUrlFromCountry(countryRaw) {
  const slug = golfCountryToFlagSlug(countryRaw);
  if (!slug) return propsFlagPlaceholderDataUri();
  return `https://flagcdn.com/w80/${slug}.png`;
}

function setPropsCountryFlag(p) {
  const img = document.getElementById("props-flag");
  if (!img) return;
  const country = String(p?.country || "").trim();
  img.alt = country || "Country";
  img.title = country || "";
  const url = flagImageUrlFromCountry(country);
  img.onerror = function onFlagErr() {
    this.onerror = null;
    this.src = propsFlagPlaceholderDataUri();
  };
  img.src = url;
}

/** ~12 names × 4 rounds + sample props — replace or add projections.json */
function buildDefaultProjectionsPayload() {
  const names = [
    "Scheffler, Scottie",
    "McIlroy, Rory",
    "Morikawa, Collin",
    "Schauffele, Xander",
    "Homa, Max",
    "Hatton, Tyrrell",
    "Finau, Tony",
    "Clark, Wyndham",
    "Thomas, Justin",
    "Spieth, Jordan",
    "Rahm, Jon",
    "Fleetwood, Tommy",
  ];
  const countries = ["USA", "NIR", "SWE", "USA", "USA", "ENG", "USA", "USA", "USA", "USA", "ESP", "ENG"];
  const players = [];
  names.forEach((player_name, i) => {
    const dg_id = i + 1;
    const country = countries[i] || "USA";
    for (let r = 1; r <= 4; r++) {
      const bump = i * 0.28 + (r - 1) * 0.12;
      const total_score = Math.round((69.4 + bump) * 10) / 10;
      const stp = Math.round((total_score - 72) * 10) / 10;
      players.push({
        dg_id,
        player_name,
        country,
        round: r,
        total_score,
        round_sd: Math.round((2.62 + (i % 7) * 0.04) * 100) / 100,
        score_to_par: stp,
        birdies: Math.round((Math.max(2.5, 4.3 - i * 0.12 - r * 0.05)) * 10) / 10,
        pars: Math.round((Math.min(13, 10.5 + i * 0.08)) * 10) / 10,
        bogeys: Math.round((Math.min(4.5, 2.4 + i * 0.14 + r * 0.04)) * 10) / 10,
        gir: Math.round((Math.max(9, 13 - i * 0.35)) * 10) / 10,
        fairways: Math.round((Math.max(7, 10 - i * 0.15)) * 10) / 10,
        eagles: Math.round((i < 3 ? 0.25 : 0.12) * 100) / 100,
        doubles: Math.round((0.35 + (i % 4) * 0.08) * 100) / 100,
        win: Math.round(Math.max(0.005, 0.14 - i * 0.009) * 1000) / 1000,
        top_5: Math.round(Math.max(0.02, 0.42 - i * 0.025) * 1000) / 1000,
        top_10: Math.round(Math.max(0.05, 0.58 - i * 0.022) * 1000) / 1000,
        top_20: Math.round(Math.max(0.1, 0.75 - i * 0.018) * 1000) / 1000,
        make_cut: Math.round(Math.max(0.15, 0.92 - i * 0.02) * 1000) / 1000,
        position: i + 1,
        mu_sg: Math.round((72 - total_score) * 0.2 * 10) / 10,
        implied_mu_sg: Math.round((72 - total_score) * 0.2 * 10) / 10,
      });
    }
  });
  const props = [
    { dg_id: 1, player_name: "Scheffler, Scottie", line: 69.5, over_odds: -108, under_odds: -112, market: "Total Score" },
    { dg_id: 2, player_name: "McIlroy, Rory", line: 70.5, over_odds: -110, under_odds: -110, market: "Total Score" },
    { dg_id: 3, player_name: "Morikawa, Collin", line: 4.5, over_odds: -115, under_odds: -105, market: "Birdies" },
    { dg_id: 5, player_name: "Homa, Max", line: 10.5, over_odds: -110, under_odds: -118, market: "Pars" },
    { dg_id: 4, player_name: "Schauffele, Xander", line: 2.5, over_odds: -120, under_odds: -102, market: "Bogeys" },
  ];
  return {
    event_name: "Bundled demo field — edit buildDefaultProjectionsPayload() in app.js",
    course_used: "Demo venue",
    display_round_label: "",
    updated_at: "",
    source: "bundled-demo",
    outrights_model_blend_weight: 1,
    outright_win_score_blend: 0,
    outright_live_score_placement_nudge: false,
    outrights_odds_format: "percent",
    matchups_odds_format: "decimal",
    players,
    props,
    outrights: buildDemoOutrightsFromPlayers(players),
    matchups: buildDemoMatchupsFromPlayers(players),
  };
}

const OUTRIGHT_MARKET_KEYS = ["win", "top_5", "top_10", "top_20", "make_cut", "mc", "frl"];

function outrightPayloadHasRows(outrights) {
  if (!outrights || typeof outrights !== "object") return false;
  for (const mk of OUTRIGHT_MARKET_KEYS) {
    const pack = outrights[mk];
    if (pack && Array.isArray(pack.rows) && pack.rows.length > 0) return true;
  }
  return false;
}

/** Synthetic sportsbook columns so Outrights works offline (mirrors Shiny layout without API). */
function buildDemoOutrightsFromPlayers(players) {
  const bookKeys = [
    "bet365",
    "betmgm",
    "borgata",
    "betway",
    "caesars",
    "draftkings",
    "fanduel",
    "pointsbet",
    "unibet",
    "wynnbet",
  ];
  const markets = [
    { key: "win", col: "win" },
    { key: "top_5", col: "top_5" },
    { key: "top_10", col: "top_10" },
    { key: "top_20", col: "top_20" },
    { key: "make_cut", col: "make_cut" },
    { key: "mc", col: "make_cut", invert: true },
  ];
  const outrights = {};
  for (const { key, col, invert } of markets) {
    const rows = [];
    const seen = new Set();
    for (const p of players) {
      if (!samePlayerRound(p, 1)) continue;
      const id = Math.round(num(p.dg_id, NaN));
      if (!Number.isFinite(id) || seen.has(id)) continue;
      seen.add(id);
      let v = num(p[col], NaN);
      if (invert && Number.isFinite(v)) v = 1 - v;
      if (!Number.isFinite(v)) continue;
      const basePct = v > 1.5 ? v : v * 100;
      const row = { dg_id: id, player_name: String(p.player_name || "") };
      bookKeys.forEach((bk, bi) => {
        const jitter = 1 + (bi - 3) * 0.035;
        row[bk] = Math.min(99, Math.max(0.1, basePct * jitter));
      });
      rows.push(row);
    }
    outrights[key] = { rows, bookKeys: [...bookKeys] };
  }
  return outrights;
}

const DEFAULT_PROJECTIONS_PAYLOAD = buildDefaultProjectionsPayload();

let DATA = {
  players: [],
  props: [],
  meta: {},
  outrights: {},
  matchups: {},
};

// Round history: loaded on demand from player_round_history.json. The embedded script is only a fallback
// for file:// demos or unusual static hosts where JSON fetch is unavailable.
// Assign: window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__ = <object> (see embed script)

let HISTORY = { meta: {}, byDgId: {}, holesByPlayerKey: {}, _ok: false };
let playerHistoryLoadPromise = null;
let embeddedRoundHistoryScriptPromise = null;
const playerHistoryBucketLoadPromises = new Map();

/** Same-origin cache from `approach_skill_ytd.json` (written by `npm run fetch:dg`). Falls back to legacy `approach_skill_l12.json`. Cleared when projections reload. */
let approachSkillYtdCache = null;
let approachSkillYtdLoadPromise = null;
/** Built by build:shots-web from all_shots_*.csv — unrelated to Historical Trends (round history JSON). */
let SHOTS = { meta: {}, byDgId: {}, _ok: false };
/** Built from data/course_table.json (see scripts/build-course-table-json.mjs, npm run build:course-table). */
let COURSE_TABLE_PAYLOAD = null;
let courseTableJsonLoadPromise = null;
let RESULTS = { loaded: false, loading: false, error: "", payload: null };
/** Chronological bet tuples for Kelly ROI — from `data/results_kelly_bets.json` (see build-results-backtest.mjs). */
let KELLY = { loaded: false, loading: false, error: "", payload: null };
/** Results tab: `1w` | `1m` | `1y` | `ytd` | `all` */
let resultsTimeRange = "all";
/** Hit regions for Results chart win-marker tooltips (canvas). */
let resultsChartHitRegions = [];
let matchupAnalysisSelectedKey = "";
/** Full matchup list for the active market (search / suggest); `<select>` may list fewer. */
let matchupAnalysisRowsCache = [];
let propsTrendsLineContextKey = "";
/** Perf caches for pricing-mode recomputes (cleared when history/context changes). */
const HISTORY_ROUNDS_CHRONO_CACHE = new Map();
const PRICING_MU_BONUS_CACHE = new Map();
/** Last valid line used when the input is mid-edit or empty. */
let propsTrendLastGoodLine = NaN;
/** Field-by-course: one live-history merge per mode session (avoids re-render loop). */
let propsCourseWindowLiveMergeAttempted = false;
/** Increment when round-history payloads change so course/session caches invalidate cheaply. */
let historyMutationEpoch = 0;
function bumpHistoryMutationEpoch() {
  historyMutationEpoch++;
  propsFieldVenueRoundsCacheSig = "";
  propsFieldVenueRoundsCache = { season: [], all: [] };
  propsCourseRoundIndexSig = "";
  propsCourseRoundIndex.clear();
  propsSingleCourseIndexSig = "";
  propsSingleCourseIndexCache = null;
  propsSingleCourseIndexPromise = null;
  propsSingleCourseIndexCourseKey = "";
  propsCourseWindowLastEntries = null;
  courseWindowRoundEntriesCacheSig = "";
  courseWindowRoundEntriesCache = null;
  propsDgIdNameManifestUiRefreshDone = false;
}
/** Cached sorted course dropdown rows from full history scan. */
let propsAllPlayersCourseOptsCacheKey = "";
/** @type {[string, string][] | null} */
let propsAllPlayersCourseOptsEntries = null;
/** courseKey → cached distinct UTC calendar days with completed rounds at that venue (`historyMutationEpoch` invalidates). */
const distinctCourseSessionDatesCache = new Map();
/** Last course key applied by `ensurePropsCourseWindowDateDefaults` (dropdown-equivalent resets on change). */
let propsCourseWindowDateDefaultsCourseTracked = "";
/** Last collectCourseWindowRoundEntriesFixed signature within one UI tick (heavy scan). */
let courseWindowRoundEntriesCacheSig = "";
/** @type {Array<{ row: object, dgId: number, playerName: string }> | null} */
let courseWindowRoundEntriesCache = null;
let filteredHistoryRoundsMemoSigStored = "";
const filteredHistoryRoundsMemoByDgId = new Map();

/** Top-10 table sort: `dir` 1 = ascending, -1 = descending (higher values first). */
let propsTopTableSort = /** @type {{ key: "overRate" | "underRate" | "over" | "under" | "name", dir: -1 | 1 }} */ ({
  key: "overRate",
  dir: -1,
});
let propsTopTableSortStatKey = "";
/** Bottom table: only 🔥 side or only 🧊 side (toggle on emoji). Default fire. */
let propsTopHitsFitMode = /** @type {"fire" | "ice"} */ ("fire");
/** Last chart payload for resize redraw: `{ actual, completed, year }[]` */
let propsChartCache = { series: null, lineY: NaN, statKey: "" };
/** Bar hit targets in canvas pixel space (full column band for easier clicks). */
let propsChartHitRegions = [];
let propsChartTooltipPinned = false;
/** Debounced Historical Trends refresh (full-field scans are heavy with full history loaded). */
let propsTrendsRenderDebounceT = 0;
/** Cached field-wide rounds at current venue (`historyMutationEpoch` + venue invalidates). */
let propsFieldVenueRoundsCacheSig = "";
/** @type {{ season: object[], all: object[] }} */
let propsFieldVenueRoundsCache = { season: [], all: [] };
/** One-pass index: courseKey → sorted session ISO days + all round entries at that venue. */
let propsCourseRoundIndexSig = "";
/** @type {Map<string, { days: string[], entries: { row: object, dgId: number, playerName: string }[] }>} */
const propsCourseRoundIndex = new Map();
let propsCourseWindowRenderGen = 0;
/** @type {{ courses?: { course_key: string, file: string }[] } | null} */
let propsCoursesManifestCache = null;
let propsCoursesManifestPromise = null;
/** dg_id → display name from player-history/manifest.json (full archive field). */
let propsDgIdNameById = null;
let propsDgIdNameManifestPromise = null;
let propsDgIdNameManifestUiRefreshDone = false;
/** Single-venue index for field-by-course (not all courses). */
let propsSingleCourseIndexSig = "";
/** @type {{ days: string[], entries: { row: object, dgId: number, playerName: string }[] } | null} */
let propsSingleCourseIndexCache = null;
/** @type {Promise<{ days: string[], entries: { row: object, dgId: number, playerName: string }[] }> | null} */
let propsSingleCourseIndexPromise = null;
let propsSingleCourseIndexCourseKey = "";
/** Last field-by-course entries used for chart + table (one collect per render). */
let propsCourseWindowLastEntries = null;
/** Field-by-course chart: no winN cap — canvas scrolls/fits all bars in the date window (perf guard only). */
const PROPS_COURSE_WINDOW_MAX_CHART_BARS = 640;
const PROPS_COURSE_INDEX_PLAYER_CHUNK = 8;

function num(v, d) {
  const n = Number(v);
  return Number.isFinite(n) ? n : d;
}

/**
 * “Model” round from projections meta: default min(display, live overlay, field API) so calendar export
 * does not sit on R2 before the field is live — but when preds/in-play overlay lags after a rollover and
 * export+field already agree on the higher round, trust that max (push:all merge + multi-source round).
 */
function effectiveUiModelRoundFromMeta() {
  const m = DATA?.meta || {};
  const ex = Math.round(num(m.display_round, NaN));
  const live = Math.round(num(m.datagolf_live_current_round, NaN));
  const field = Math.round(num(m.datagolf_field_current_round, NaN));
  const ok = (x) => Number.isFinite(x) && x >= 1 && x <= 4;
  const parts = [ex, live, field].filter(ok);
  if (!parts.length) return NaN;
  const mn = Math.min(...parts);
  const mx = Math.max(...parts);
  if (mx > mn && ok(live) && live === mn && ok(ex) && ok(field) && ex === field && field === mx) return mx;
  return mn;
}

/** Results tab removed from `index.html`; keep guards so leftover JS does not touch missing DOM. */
function resultsFeatureEnabled() {
  try {
    return Boolean(document.getElementById("panel-results"));
  } catch {
    return false;
  }
}

function samePlayerRound(p, round) {
  const pr = Math.round(num(p?.round, NaN));
  const rr = Math.round(num(round, NaN));
  return Number.isFinite(pr) && Number.isFinite(rr) && pr === rr;
}

function isAfterSunday8pmEt(now = new Date()) {
  try {
    const fmt = new Intl.DateTimeFormat("en-US", {
      timeZone: "America/New_York",
      weekday: "short",
      hour: "numeric",
      hour12: false,
      minute: "numeric",
    });
    const parts = fmt.formatToParts(now);
    const wd = parts.find((p) => p.type === "weekday")?.value ?? "Sun";
    const hour = parseInt(parts.find((p) => p.type === "hour")?.value ?? "0", 10);
    const minute = parseInt(parts.find((p) => p.type === "minute")?.value ?? "0", 10);
    const h = hour + minute / 60;
    const isSun = wd.slice(0, 3) === "Sun";
    return isSun && h >= 20;
  } catch (_) {
    return false;
  }
}

/**
 * Player row for model / +EV: prefers live DG round, then export `display_round`, then preferred round, then any row.
 * Avoids stale placement probs when the leaderboard round picker lags the real tournament.
 */
function projectionPlayerRowForModel(dgId, preferredRound) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return null;
  const pr = Math.round(num(preferredRound, NaN));
  const liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  const metaDr = effectiveUiModelRoundFromMeta();
  const candidates = (DATA.players || []).filter((p) => Math.round(num(p.dg_id, NaN)) === id);
  if (!candidates.length) return null;
  // After 8pm ET on Sunday, never use prior-round fallbacks.
  if (isAfterSunday8pmEt()) {
    const target = Number.isFinite(pr) && pr >= 1 && pr <= 4 ? pr : 1;
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === target);
    return hit || null;
  }
  if (Number.isFinite(liveR) && liveR >= 1 && liveR <= 4) {
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === liveR);
    if (hit) return hit;
  }
  if (Number.isFinite(metaDr) && metaDr >= 1 && metaDr <= 4) {
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === metaDr);
    if (hit) return hit;
  }
  if (Number.isFinite(pr) && pr >= 1 && pr <= 4) {
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === pr);
    if (hit) return hit;
  }
  return candidates[0];
}

function projectionPlayerRowForModelByIdOrName(dgId, playerName, preferredRound) {
  const byId = projectionPlayerRowForModel(dgId, preferredRound);
  if (byId) return byId;
  const pKey = playerKeyFromName(playerName);
  if (!pKey) return null;
  const pr = Math.round(num(preferredRound, NaN));
  const liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  const metaDr = effectiveUiModelRoundFromMeta();
  const candidates = (DATA.players || []).filter((p) => playerKeyFromName(p?.player_name) === pKey);
  if (!candidates.length) return null;
  // After 8pm ET on Sunday, never use prior-round fallbacks.
  if (isAfterSunday8pmEt()) {
    const target = Number.isFinite(pr) && pr >= 1 && pr <= 4 ? pr : 1;
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === target);
    return hit || null;
  }
  if (Number.isFinite(liveR) && liveR >= 1 && liveR <= 4) {
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === liveR);
    if (hit) return hit;
  }
  if (Number.isFinite(metaDr) && metaDr >= 1 && metaDr <= 4) {
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === metaDr);
    if (hit) return hit;
  }
  if (Number.isFinite(pr) && pr >= 1 && pr <= 4) {
    const hit = candidates.find((p) => Math.round(num(p.round, NaN)) === pr);
    if (hit) return hit;
  }
  return candidates[0];
}

function modeledMuSgFromRow(row) {
  const mu = num(row?.mu_sg, NaN);
  const implied = num(row?.implied_mu_sg, NaN);
  // Some exports zero-out mu_sg for completed rounds (especially R4) while implied_mu_sg
  // still carries skill signal; using mu_sg blindly collapses matchups to 50/50.
  if (Number.isFinite(implied) && Number.isFinite(mu) && Math.abs(mu) < 1e-9 && Math.abs(implied) > 1e-9) {
    return implied;
  }
  if (Number.isFinite(mu)) return mu;
  if (Number.isFinite(implied)) return implied;
  const stp = num(row?.score_to_par, NaN);
  if (Number.isFinite(stp)) return -stp;
  const ts = num(row?.total_score, NaN);
  if (Number.isFinite(ts)) {
    const par18 = num(DATA?.meta?.course_par_18, 72);
    if (Number.isFinite(par18)) return par18 - ts;
  }
  return NaN;
}

const PLACEMENT_PROB_COLS = ["win", "top_5", "top_10", "top_20", "make_cut"];

/**
 * Win / top_N / make_cut are tournament-wide; export rows are per-round and placement may only be
 * populated on one round per dg_id. Merge from any round so +EV / Outrights model price is not blank.
 */
function projectionRowWithPlacementMerged(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return null;
  const rows = (DATA.players || []).filter((p) => Math.round(num(p.dg_id, NaN)) === id);
  if (!rows.length) return null;
  const base = projectionPlayerRowForModel(id, getModelRoundForEv()) || { ...rows[0] };
  const out = { ...base };
  for (const col of PLACEMENT_PROB_COLS) {
    const cur = datagolfModelProb01(out[col]);
    if (Number.isFinite(cur) && cur > 0) continue;
    for (const p of rows) {
      const pp = datagolfModelProb01(p[col]);
      if (Number.isFinite(pp) && pp > 0) {
        out[col] = p[col];
        break;
      }
    }
  }
  for (const p of rows) {
    const cs = num(p.current_score, NaN);
    if (Number.isFinite(cs)) {
      out.current_score = cs;
      break;
    }
  }
  if (rows.some((p) => p.dg_live_placement_from_api)) out.dg_live_placement_from_api = true;
  return out;
}

/** Round for outright / placement +EV: live DG → export display_round → O/U picker (not leaderboard-only R1). */
function getModelRoundForEv() {
  if (isAfterSunday8pmEt()) return 1;
  const m = DATA?.meta || {};
  const liveR = Math.round(num(m.datagolf_live_current_round, NaN));
  if (Number.isFinite(liveR) && liveR >= 1 && liveR <= 4) return liveR;
  return ouDisplayRoundAuto();
}

/** Regulation fairway opportunities from projections meta (pars-based in fetch-datagolf); 14 when absent. */
function fairwayHolesModeledFromData() {
  const n = num(DATA?.meta?.projection_course_basis?.fairway_holes_modeled, NaN);
  if (Number.isFinite(n) && n > 0) return Math.round(n);
  return 14;
}

/**
 * When the event advances (DataGolf live round or export display_round), align the shared Round
 * selector so O/U, +EV, and matchups read projection rows for the active round. Live current_round
 * can move the picker up or down to correct stale export display_round. Without live, only advance
 * forward from export (user can still pick an earlier round manually).
 */
function syncLbRoundToTournamentModelRound() {
  const sel = document.getElementById("lb-round");
  if (!sel || isAfterSunday8pmEt()) return false;
  const mismatch = String(DATA?.meta?.datagolf_live_event_mismatch || "").trim();
  const liveR = mismatch
    ? NaN
    : Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  const drEff = effectiveUiModelRoundFromMeta();
  const drExport = Math.round(num(DATA?.meta?.display_round, NaN));
  /* Live file from a different tournament can leave the picker on R4 while projections are R1 — snap back. */
  if (mismatch && Number.isFinite(drExport) && drExport >= 1 && drExport <= 4) {
    const curSnap = Math.round(num(sel.value, NaN));
    if (!Number.isFinite(curSnap) || curSnap !== drExport) {
      sel.value = String(drExport);
      return true;
    }
    return false;
  }
  const fromLive = Number.isFinite(liveR) && liveR >= 1 && liveR <= 4;
  let target = 0;
  if (fromLive) target = liveR;
  else if (Number.isFinite(drEff) && drEff >= 1 && drEff <= 4) target = drEff;
  if (target < 1) return false;
  const cur = Math.round(num(sel.value, NaN));
  if (!Number.isFinite(cur) || cur < 1 || cur > 4) {
    sel.value = String(target);
    return true;
  }
  if (fromLive) {
    if (target !== cur) {
      sel.value = String(target);
      return true;
    }
    return false;
  }
  if (target > cur) {
    sel.value = String(target);
    return true;
  }
  return false;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

/** "Last, First" → "First Last" for display; leaves other formats unchanged. */
function displayGolferName(name) {
  const s = String(name || "").trim();
  const m = s.match(/^([^,]+),\s*(.+)$/);
  if (m) return `${m[2].trim()} ${m[1].trim()}`.trim();
  return s;
}

/** Never show synthetic `DG 12345` labels in the UI. */
function isDgPlaceholderDisplayName(name) {
  return /^DG\s*\d+\s*$/i.test(String(name || "").trim());
}

function normalizeGolferDisplayName(raw) {
  const s = displayGolferName(String(raw || "").trim());
  if (!s || isDgPlaceholderDisplayName(s)) return "";
  return s;
}

function clampProb01(p) {
  if (!Number.isFinite(p)) return NaN;
  return Math.max(0, Math.min(1, p));
}

/** Bar hit targets for O/U histogram tooltips (CSS px, matches canvas drawing). */
let ouChartHitRegions = [];

function erf(x) {
  const ax = Math.abs(x);
  const t = 1 / (1 + 0.3275911 * ax);
  const p = 1 - ((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-ax * ax);
  return x >= 0 ? p : -p;
}

function normalCdf(z) {
  return 0.5 * (1 + erf(z / Math.SQRT2));
}

function americanFromDecimal(d) {
  if (!Number.isFinite(d) || d <= 1) return NaN;
  if (d >= 2) return Math.round((d - 1) * 100);
  return Math.round(-100 / (d - 1));
}

function decimalFromAmerican(a) {
  if (!Number.isFinite(a) || a === 0) return NaN;
  if (a > 0) return 1 + a / 100;
  return 1 + 100 / Math.abs(a);
}

function impliedProbFromAmerican(a) {
  const d = decimalFromAmerican(a);
  if (!Number.isFinite(d) || d <= 0) return NaN;
  return 1 / d;
}

/** Decimal odds for one matchup line (handles `matchups_odds_format` + mis-tagged American in decimal feeds). */
function matchupOddsDecodeScalar(raw) {
  const v = num(raw, NaN);
  if (!Number.isFinite(v) || v === 0) return NaN;
  const fmt = String(DATA?.meta?.matchups_odds_format || "").toLowerCase();
  if (fmt === "american" || fmt === "us") return decimalFromAmerican(Math.round(v));
  if (v > 1 && v <= 80) return v;
  if (v >= 100 || v <= -1) return decimalFromAmerican(Math.round(v));
  if (v > 80 && v < 100 && Number.isInteger(v)) return decimalFromAmerican(Math.round(v));
  return NaN;
}

function matchupOddsTwoWayFromPack(pack) {
  if (!pack || typeof pack !== "object") return { d1: NaN, d2: NaN };
  const d1 = matchupOddsDecodeScalar(pack.p1 ?? pack.P1 ?? pack.player_1 ?? pack.line_1 ?? pack.home);
  const d2 = matchupOddsDecodeScalar(pack.p2 ?? pack.P2 ?? pack.player_2 ?? pack.line_2 ?? pack.away);
  return { d1, d2 };
}

function matchupOddsThreeWayFromPack(pack) {
  if (!pack || typeof pack !== "object") return { d1: NaN, d2: NaN, d3: NaN };
  const d1 = matchupOddsDecodeScalar(pack.p1 ?? pack.P1 ?? pack.player_1 ?? pack.line_1 ?? pack.home);
  const d2 = matchupOddsDecodeScalar(pack.p2 ?? pack.P2 ?? pack.player_2 ?? pack.line_2 ?? pack.away);
  const d3 = matchupOddsDecodeScalar(pack.p3 ?? pack.P3 ?? pack.player_3 ?? pack.line_3);
  return { d1, d2, d3 };
}

function americanFromImpliedProb(p) {
  const pp = clamp(p, 1e-6, 1 - 1e-6);
  return americanFromDecimal(1 / pp);
}

function viggedDecimalsForOverUnder(pOver) {
  const p = clamp(pOver, 0.02, 0.98);
  const fairDo = 1 / p;
  const fairDu = 1 / (1 - p);
  const k = 1 / (1 + OU_HOLD);
  const minD = 1.02;
  return {
    do: Math.max(minD, fairDo * k),
    du: Math.max(minD, fairDu * k),
  };
}

function formatAmerican(a) {
  if (!Number.isFinite(a)) return "—";
  const r = Math.round(a);
  if (Math.abs(r) > 250000) return "—";
  return (r > 0 ? "+" : "") + r;
}

/**
 * DataGolf outright book cells mix formats in the same JSON:
 * - probability in (0, 1] (common on Win market)
 * - implied percent in (1, 100) e.g. 58.82 => 58.82%
 * - small percents as 1.0989 => 1.0989% (NOT 109%)
 * - American odds when |x| is large (e.g. +1500)
 */
function bookImpliedProb01(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x)) return NaN;
  if (Math.abs(x) >= 1e6) return NaN;
  if (x === 0) return NaN;
  if (x > 0 && x <= 1) return x;
  if (x > 1 && x < 100) return x / 100;
  if (x === 100) return NaN;
  if (Math.abs(x) >= 101 && Math.abs(x) <= 500000) {
    const dec = decimalFromAmerican(Math.round(x));
    if (Number.isFinite(dec) && dec > 1) return 1 / dec;
  }
  return NaN;
}

/**
 * preds/in-play `data` with odds_format=percent: values in (1, 100) are percents (e.g. 1.2 => 1.2%).
 * Same convention as bookImpliedProb01 — NOT the old (0,1.5] bug that turned 1.2% into ~100% model prob.
 * Unit probabilities in (0, 1] are accepted as-is.
 */
function datagolfModelProb01(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x) || x < 0) return NaN;
  if (x === 0) return 0;
  if (x > 0 && x <= 1) return Math.min(1, Math.max(0, x));
  if (x > 1 && x < 100) return Math.min(1, x / 100);
  if (x === 100) return NaN;
  if (Math.abs(x) >= 101 && Math.abs(x) <= 500000) {
    const dec = decimalFromAmerican(Math.round(x));
    if (Number.isFinite(dec) && dec > 1) return 1 / dec;
  }
  return NaN;
}

/** Hoisted for DEFAULT_PROJECTIONS_PAYLOAD — demo head-to-heads from R1 field. */
function buildDemoMatchupsFromPlayers(players) {
  const r1 = players.filter((p) => samePlayerRound(p, 1));
  const byId = new Map();
  for (const p of r1) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    if (!byId.has(id)) byId.set(id, p);
  }
  const list = [...byId.values()].sort((a, b) => num(a.position, 999) - num(b.position, 999));
  const match_list = [];
  for (let i = 0; i + 1 < list.length; i += 2) {
    const p1 = list[i];
    const p2 = list[i + 1];
    const m1 = modeledMuSgFromRow(p1);
    const m2 = modeledMuSgFromRow(p2);
    // Keep fallback books on the same probability model used by +EV matchup pricing.
    // Previously this used a much tighter sigma (0.35), producing aggressive book prices
    // while +EV valued with matchupWinProb() (sigmaDiff ~2.85 for round matchups),
    // which inflated apparent edges and often pinned model near +100.
    const p1w = matchupWinProb(m1, m2, "round_matchups");
    const d1 = +(1.03 / p1w).toFixed(2);
    const d2 = +(1.03 / (1 - p1w)).toFixed(2);
    match_list.push({
      p1_dg_id: Math.round(num(p1.dg_id, 0)),
      p1_player_name: String(p1.player_name || ""),
      p2_dg_id: Math.round(num(p2.dg_id, 0)),
      p2_player_name: String(p2.player_name || ""),
      ties: "void",
      odds: {
        draftkings: { p1: d1, p2: d2 },
        datagolf: { p1: +(1 / p1w).toFixed(3), p2: +(1 / (1 - p1w)).toFixed(3) },
      },
    });
  }
  const en = "Demo matchups";
  return {
    tournament_matchups: {
      event_name: en,
      market: "tournament_matchups",
      match_list: "No tournament_matchups being offered right now.",
    },
    round_matchups: { event_name: en, market: "round_matchups", match_list },
    "3_balls": { event_name: en, market: "3_balls", match_list: "No 3_balls being offered right now." },
  };
}

let dataSource = "bundled";
/** Non-zero while background polling of projections is enabled (ms). */
let projectionsPollMs = 0;
let projectionsPollTimerId = 0;
let projectionsLoadInFlight = false;
/** If a silent poll hit while a load was in flight, run one more right after (avoid missing odds/EV updates). */
let projectionsSilentReloadQueued = false;
let lastDocVisibleProjectionsRefetchAt = 0;
/** Set when projections.json (or bundled demo) finishes applying; used to refetch market odds on +EV. */
let lastProjectionsLoadedAtMs = 0;
/** Min time since last successful projections load before +EV tab triggers another silent fetch. */

let datagolfLivePollTimerId = 0;
/** Fingerprint of last merged preds/in-play (info.last_update); skip merge until DataGolf publishes a new one. */
let lastDatagolfInPlayToken = "";
/** Every N polls, merge anyway so make_cut / current_pos refresh if the file changes without last_update bumping. */
let datagolfLivePeriodicForceTick = 0;
/** Last preds/in-play bundle for Historical Trends (independent of live odds polling). */
let lastLiveInPlayBundleForHistory = null;
let liveTournamentHistoryMergeInFlight = null;
let propsTrendsLiveHistoryFetchQueued = false;

function projectionsJsonUrl() {
  if (typeof window !== "undefined" && window.__ALPHA_CADDIE_PROJECTIONS_URL__) {
    const u = String(window.__ALPHA_CADDIE_PROJECTIONS_URL__).trim();
    if (u) return u;
  }
  try {
    const q = new URLSearchParams(window.location.search).get("projections");
    if (q != null && String(q).trim()) return String(q).trim();
  } catch (_) {}
  return "projections.json";
}

/** Same-origin fetches can still reuse a cached body; bust query on polls so book odds / +EV stay current. */
function cacheBustFetchUrl(baseUrl) {
  const raw = String(baseUrl || "").trim();
  if (!raw) return raw;
  try {
    const u = new URL(raw, typeof location !== "undefined" ? location.href : undefined);
    u.searchParams.set("_cb", String(Date.now()));
    return u.toString();
  } catch (_) {
    const sep = raw.includes("?") ? "&" : "?";
    return `${raw}${sep}_cb=${Date.now()}`;
  }
}

/** Sibling of projections.json, or window.__ALPHA_CADDIE_LIVE_IN_PLAY_URL__. */
function liveInPlayJsonUrl() {
  if (typeof window !== "undefined" && window.__ALPHA_CADDIE_LIVE_IN_PLAY_URL__) {
    const u = String(window.__ALPHA_CADDIE_LIVE_IN_PLAY_URL__).trim();
    if (u) return u;
  }
  const base = projectionsJsonUrl().trim();
  if (!base) return "live-in-play.json";
  try {
    const u = new URL(base, typeof location !== "undefined" ? location.href : undefined);
    u.pathname = u.pathname.replace(/[^/]+$/, "live-in-play.json");
    u.search = "";
    u.hash = "";
    return u.toString();
  } catch (_) {
    return "live-in-play.json";
  }
}

function courseTableJsonUrl() {
  const base = projectionsJsonUrl().trim();
  if (!base) return "course-table.json";
  try {
    const u = new URL(base, typeof location !== "undefined" ? location.href : undefined);
    u.pathname = u.pathname.replace(/[^/]+$/, "course-table.json");
    u.search = "";
    u.hash = "";
    return u.toString();
  } catch (_) {
    return "course-table.json";
  }
}

/** Loads course-table.json once; used by Course Fit, static course difficulty prior for live props, etc. */
async function loadCourseTableJson() {
  if (COURSE_TABLE_PAYLOAD) return COURSE_TABLE_PAYLOAD;
  if (courseTableJsonLoadPromise) return courseTableJsonLoadPromise;
  courseTableJsonLoadPromise = (async () => {
    try {
      const res = await fetch(cacheBustFetchUrl(courseTableJsonUrl()), { cache: "no-store" });
      if (!res.ok) throw new Error(String(res.status));
      COURSE_TABLE_PAYLOAD = await res.json();
    } catch {
      COURSE_TABLE_PAYLOAD = null;
    } finally {
      courseTableJsonLoadPromise = null;
    }
    return COURSE_TABLE_PAYLOAD;
  })();
  return courseTableJsonLoadPromise;
}

function datagolfLivePollingDisabledExplicitly() {
  try {
    const q = new URLSearchParams(window.location.search);
    const poll = (q.get("liveInPlayPoll") || "").trim().toLowerCase();
    if (poll === "0" || poll === "off" || poll === "false" || poll === "no") return true;
    if (q.get("liveInPlay") === "0" || q.get("liveOverlay") === "0") return true;
  } catch (_) {}
  if (DATA?.meta && DATA.meta.poll_datagolf_live_predictions === false) return true;
  return false;
}

/** Refetch live-in-play.json on an interval when meta opts in or URL forces live overlay (default off). */
function datagolfLiveOverlayEnabled() {
  try {
    const q = new URLSearchParams(window.location.search);
    if (q.get("liveOverlay") === "1" || q.get("liveInPlay") === "1") return true;
  } catch (_) {}
  if (datagolfLivePollingDisabledExplicitly()) return false;
  return DATA?.meta?.poll_datagolf_live_predictions === true;
}

/**
 * How often to check live-in-play.json. Merges only when dgInPlayUpdateToken() changes (DataGolf last_update).
 * Default off (0); meta.datagolf_live_poll_interval_sec 15–600 when live polling is enabled.
 */
function datagolfLivePollIntervalMs() {
  try {
    const q = new URLSearchParams(window.location.search).get("liveInPlayPoll");
    if (q != null && String(q).trim() !== "") {
      const sec = Number(q);
      if (!Number.isFinite(sec) || sec <= 0) return 0;
      return Math.min(600, Math.max(15, sec)) * 1000;
    }
  } catch (_) {}
  const sec = num(DATA?.meta?.datagolf_live_poll_interval_sec, 0);
  if (!Number.isFinite(sec) || sec <= 0) return 0;
  if (sec < 15) return 15 * 1000;
  return Math.min(600, sec) * 1000;
}

/** First non-null field from row (JSON API may use snake_case or camelCase). */
function dgInPlayField(row, names) {
  for (const k of names) {
    if (row[k] == null || row[k] === "") continue;
    return row[k];
  }
  return undefined;
}

/** Cheap string hash so live row edits (thru/today/scores) bump the merge token even if `last_update` is unchanged. */
function hashDjb2(str) {
  let h = 5381;
  for (let i = 0; i < str.length; i++) {
    h = Math.imul(h, 33) + str.charCodeAt(i);
  }
  return (h >>> 0).toString(36);
}

/** Fingerprint in-play `data` rows: placement + hole progress + card vs par (not only info.last_update). */
function dgInPlayLiveScorebookHash(j) {
  if (!Array.isArray(j.data) || !j.data.length) return "0";
  const chunks = [];
  for (const r of j.data) {
    if (!r || typeof r !== "object") continue;
    const id = dgInPlayField(r, ["dg_id", "dgId"]) ?? "";
    const thru = dgInPlayField(r, ["thru", "Thru"]);
    const today = dgInPlayField(r, ["today", "Today"]);
    const cs = dgInPlayField(r, ["current_score", "currentScore"]);
    const w = dgInPlayField(r, ["win", "win_prob"]);
    const wn = num(w, NaN);
    chunks.push(`${id}:${thru}:${today}:${cs}:${Number.isFinite(wn) ? wn.toFixed(4) : ""}`);
  }
  return `${j.data.length}:${hashDjb2(chunks.join("|"))}`;
}

/** Stable token from live bundle JSON so we only re-merge after DataGolf updates any included feed. */
function dgInPlayUpdateToken(j) {
  if (!j || typeof j !== "object") return "";
  const info = j.info && typeof j.info === "object" ? j.info : {};
  const lu = info.last_update != null ? String(info.last_update).trim() : "";
  const tLu =
    j.live_tournament_stats && j.live_tournament_stats.last_updated != null
      ? String(j.live_tournament_stats.last_updated).trim()
      : "";
  const hLu =
    j.live_hole_stats && j.live_hole_stats.last_update != null ? String(j.live_hole_stats.last_update).trim() : "";
  const scH = dgInPlayLiveScorebookHash(j);
  if (lu || tLu || hLu) return `lu:${lu}|ts:${tLu}|hs:${hLu}|sc:${scH}`;
  const n = Array.isArray(j.data) ? j.data.length : 0;
  const parts = [];
  for (let i = 0; i < Math.min(8, n); i++) {
    const r = j.data[i];
    if (!r || typeof r !== "object") continue;
    const w = num(dgInPlayField(r, ["win", "win_prob"]), NaN);
    parts.push(`${dgInPlayField(r, ["dg_id", "dgId"]) ?? ""}:${Number.isFinite(w) ? w.toFixed(5) : ""}`);
  }
  return `fb:${n}:${parts.join("|")}|sc:${scH}`;
}

function playerDgFingerprint(players) {
  if (!Array.isArray(players) || !players.length) return "";
  const ids = [];
  const seen = new Set();
  for (const p of players) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || seen.has(id)) continue;
    seen.add(id);
    ids.push(id);
  }
  ids.sort((a, b) => a - b);
  return `${ids.length}:${ids.slice(0, 400).join(",")}`;
}

/** Target round for live-hole-stats (DataGolf current_round, else latest round_num in payload). */
function dgLiveHoleStatsTargetRoundNum(payload) {
  if (!payload || typeof payload !== "object") return NaN;
  const cr = num(payload.current_round, NaN);
  if (Number.isFinite(cr) && cr >= 1) return Math.floor(cr);
  let maxR = NaN;
  const courses = payload.courses;
  if (!Array.isArray(courses)) return NaN;
  for (const c of courses) {
    const rounds = c.rounds;
    if (!Array.isArray(rounds)) continue;
    for (const rr of rounds) {
      const rn = num(rr.round_num, NaN);
      if (Number.isFinite(rn)) maxR = Number.isFinite(maxR) ? Math.max(maxR, rn) : rn;
    }
  }
  return maxR;
}

/**
 * Per course: sum over holes of (avg_score − par) for one round; return mean across courses.
 * `minThru` drops thin holes (early wave).
 */
function liveCourseRoundExcessForRoundNum(payload, roundNum, minThru = 4) {
  if (!payload || typeof payload !== "object") return NaN;
  const courses = payload.courses;
  if (!Array.isArray(courses) || !courses.length) return NaN;
  const rn = Math.round(num(roundNum, NaN));
  if (!Number.isFinite(rn) || rn < 1) return NaN;

  const perCourse = [];
  for (const c of courses) {
    const rounds = c.rounds;
    if (!Array.isArray(rounds)) continue;
    let sum = 0;
    let nh = 0;
    for (const rr of rounds) {
      if (Math.round(num(rr.round_num, NaN)) !== rn) continue;
      const holes = rr.holes;
      if (!Array.isArray(holes)) continue;
      for (const h of holes) {
        const par = num(h.par, NaN);
        const total = h.total && typeof h.total === "object" ? h.total : {};
        const avg = num(total.avg_score, NaN);
        const th = num(total.players_thru, NaN);
        if (!Number.isFinite(par) || !Number.isFinite(avg)) continue;
        if (Number.isFinite(th) && th < minThru) continue;
        sum += avg - par;
        nh++;
      }
    }
    if (nh > 0) perCourse.push(sum);
  }
  if (!perCourse.length) return NaN;
  if (perCourse.length === 1) return perCourse[0];
  const mean = perCourse.reduce((a, b) => a + b, 0) / perCourse.length;
  const mx = Math.max(...perCourse);
  return mean + 0.5 * (mx - mean);
}

/** Mean field excess vs par for completed rounds 1..targetRound−1 (live hole stats). */
function priorRoundsMeanExcessFromLiveHoleStats(payload, targetRound, minThru = 4) {
  const tr = Math.round(num(targetRound, NaN));
  if (!Number.isFinite(tr) || tr < 2) return NaN;
  const exs = [];
  for (let rn = 1; rn < tr; rn++) {
    const ex = liveCourseRoundExcessForRoundNum(payload, rn, minThru);
    if (Number.isFinite(ex)) exs.push(ex);
  }
  if (!exs.length) return NaN;
  return exs.reduce((a, b) => a + b, 0) / exs.length;
}

function liveCourseRoundExcessFromHoleStats(payload, minThru = 4) {
  if (!payload || typeof payload !== "object") return NaN;
  const courses = payload.courses;
  if (!Array.isArray(courses) || !courses.length) return NaN;

  let targetRn = dgLiveHoleStatsTargetRoundNum(payload);
  let ex = Number.isFinite(targetRn) ? liveCourseRoundExcessForRoundNum(payload, targetRn, minThru) : NaN;
  if (!Number.isFinite(ex)) {
    let maxR = NaN;
    for (const c of courses) {
      for (const rr of c.rounds || []) {
        const rn = num(rr.round_num, NaN);
        if (Number.isFinite(rn)) maxR = Number.isFinite(maxR) ? Math.max(maxR, rn) : rn;
      }
    }
    if (Number.isFinite(maxR)) ex = liveCourseRoundExcessForRoundNum(payload, maxR, minThru);
  }
  return ex;
}

/** Field-updates teetimes `course_num` ↔ live_hole_stats `course_key` (same as fetch-datagolf.mjs). */
function courseNumsFromFieldUpdates(fieldRaw) {
  const nums = new Set();
  const fieldList = Array.isArray(fieldRaw?.field) ? fieldRaw.field : [];
  for (const p of fieldList) {
    const tt = p?.teetimes;
    if (!Array.isArray(tt)) continue;
    for (const t of tt) {
      const n = t?.course_num ?? t?.courseNum;
      if (n == null || n === "") continue;
      nums.add(String(n).trim());
    }
  }
  return nums;
}

function pickLiveHoleStatsCourseClient(lh, courseUsed, fieldRaw) {
  const courses = lh?.courses;
  if (!Array.isArray(courses) || !courses.length) return null;
  if (courses.length === 1) return courses[0];
  const nums = courseNumsFromFieldUpdates(fieldRaw);
  if (nums.size) {
    for (const c of courses) {
      const ck = String(c.course_key ?? c.courseKey ?? "").trim();
      if (ck && nums.has(ck)) return c;
    }
  }
  const cu = String(courseUsed || "").trim().toLowerCase();
  for (const c of courses) {
    const cn = String(c.course_name ?? c.courseName ?? "").trim();
    if (!cn || !cu) continue;
    const cl = cn.toLowerCase();
    if (cl.includes(cu) || cu.includes(cl)) return c;
  }
  return null;
}

/** Per-hole par array from preds/live-hole-stats payload (live-in-play.json `live_hole_stats`). */
function holeParsArrayFromLiveHoleStats(lh, courseUsed, fieldRaw) {
  const courseEntry = pickLiveHoleStatsCourseClient(lh, courseUsed, fieldRaw);
  if (!courseEntry) return null;
  const rounds = courseEntry.rounds;
  if (!Array.isArray(rounds) || !rounds.length) return null;
  const cr = num(lh.current_round, NaN);
  let roundPick = rounds;
  if (Number.isFinite(cr)) {
    const matched = rounds.filter((r) => num(r.round_num ?? r.roundNum, NaN) === cr);
    if (matched.length) roundPick = matched;
  } else {
    let maxRn = -Infinity;
    for (const r of rounds) {
      const rn = num(r.round_num ?? r.roundNum, NaN);
      if (Number.isFinite(rn)) maxRn = Math.max(maxRn, rn);
    }
    if (Number.isFinite(maxRn)) {
      const matched = rounds.filter((r) => num(r.round_num ?? r.roundNum, NaN) === maxRn);
      if (matched.length) roundPick = matched;
    }
  }
  const holes = roundPick[0]?.holes;
  if (!Array.isArray(holes) || holes.length < 18) return null;
  const byHole = new Map();
  for (const h of holes) {
    if (!h || typeof h !== "object") continue;
    const hn = Math.round(num(h.hole, NaN));
    const p = num(h.par, NaN);
    if (!Number.isFinite(hn) || hn < 1 || hn > 18) continue;
    if (!Number.isFinite(p) || p < 3 || p > 5) continue;
    byHole.set(hn, Math.round(p));
  }
  if (byHole.size < 18) return null;
  const arr = [];
  for (let i = 1; i <= 18; i++) {
    if (!byHole.has(i)) return null;
    arr.push(byHole.get(i));
  }
  return arr;
}

/** Pull live course difficulty (+ optional DG labels) from fetch-live-in-play bundle. */
/**
 * Overlay preds/live-tournament-stats `distance` / `accuracy` onto every round row for each dg_id.
 * Same feed as DataGolf Live Stats (event_avg); works even when `j.data` is missing.
 */
function mergeLiveTournamentDrivingIntoPlayers(j) {
  if (!j || typeof j !== "object" || !DATA.players || !DATA.players.length) return false;
  const lt = j.live_tournament_stats;
  if (!lt || typeof lt !== "object") return false;
  const lst = Array.isArray(lt.live_stats) ? lt.live_stats : [];
  if (!lst.length) return false;
  const modelEvent = String(DATA.meta?.event_name || DATA.event_name || "").trim();
  const ltEv = String(lt.event_name || "").trim();
  if (
    modelEvent &&
    ltEv &&
    !eventNameMatchesCurrentSchedule(ltEv, modelEvent) &&
    !eventNameMatchesCurrentSchedule(modelEvent, ltEv)
  ) {
    return false;
  }
  const byDg = new Map();
  for (const row of lst) {
    if (!row || typeof row !== "object") continue;
    const id = Math.round(num(row.dg_id ?? row.dgId, NaN));
    if (!Number.isFinite(id)) continue;
    const dist = num(row.distance, NaN);
    let acc = num(row.accuracy, NaN);
    if (Number.isFinite(acc) && acc > 0 && acc <= 1) acc *= 100;
    byDg.set(id, { dist, acc });
  }
  if (!byDg.size) return false;
  let touched = 0;
  for (const p of DATA.players) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const x = byDg.get(id);
    if (!x) continue;
    if (Number.isFinite(x.dist)) {
      const dy = Math.round(x.dist);
      p.driving_distance = dy;
      p.avg_driving_distance = dy;
      touched++;
    }
    if (Number.isFinite(x.acc)) {
      p.driving_accuracy = Math.round(x.acc * 10) / 10;
      touched++;
    }
  }
  return touched > 0;
}

function mergeDatagolfLiveCourseMeta(j) {
  if (!DATA.meta) DATA.meta = {};
  let touched = false;
  const clearKeys = (keys) => {
    for (const k of keys) {
      if (Object.prototype.hasOwnProperty.call(DATA.meta, k)) {
        delete DATA.meta[k];
        touched = true;
      }
    }
  };

  if (Object.prototype.hasOwnProperty.call(j, "live_hole_stats")) {
    const lh = j.live_hole_stats;
    if (lh && typeof lh === "object") {
      const hlu = lh.last_update != null ? String(lh.last_update).trim() : "";
      const cr = num(lh.current_round, NaN);
      const ex = liveCourseRoundExcessFromHoleStats(lh);
      if (DATA.meta.live_course_hole_stats_last_update !== hlu) touched = true;
      if (hlu) DATA.meta.live_course_hole_stats_last_update = hlu;
      else delete DATA.meta.live_course_hole_stats_last_update;
      if (Number.isFinite(cr)) {
        if (DATA.meta.live_course_hole_stats_round !== cr) touched = true;
        DATA.meta.live_course_hole_stats_round = cr;
      } else {
        clearKeys(["live_course_hole_stats_round"]);
      }
      const prevEx = DATA.meta.live_course_round_excess_strokes;
      if (Number.isFinite(ex)) {
        if (prevEx !== ex) touched = true;
        DATA.meta.live_course_round_excess_strokes = ex;
      } else {
        clearKeys(["live_course_round_excess_strokes"]);
      }
      if (refreshPriorRoundCourseMetaFromLiveHoleStats(lh)) touched = true;

      const lhEv = String(lh.event_name ?? "").trim();
      const modelEv = String(DATA.meta?.event_name ?? "").trim();
      const evOk =
        !lhEv ||
        !modelEv ||
        lhEv.toLowerCase() === modelEv.toLowerCase() ||
        eventNameMatchesCurrentSchedule(lhEv, modelEv) ||
        eventNameMatchesCurrentSchedule(modelEv, lhEv);
      if (evOk) {
        const courseUsed = String(DATA.meta?.course_used ?? "").trim();
        const fu = j.field_updates && typeof j.field_updates === "object" ? j.field_updates : null;
        const parsArr = holeParsArrayFromLiveHoleStats(lh, courseUsed, fu);
        if (parsArr && parsArr.length === 18) {
          const prevJson = JSON.stringify(DATA.meta.hole_pars);
          const nextJson = JSON.stringify(parsArr);
          if (prevJson !== nextJson) {
            DATA.meta.hole_pars = parsArr;
            DATA.meta.course_par_18 = parsArr.reduce((sum, p) => sum + Math.round(num(p, 4)), 0);
            DATA.meta.hole_pars_source = "live_hole_stats";
            touched = true;
          }
        }
      }
    } else {
      clearKeys([
        "live_course_hole_stats_last_update",
        "live_course_hole_stats_round",
        "live_course_round_excess_strokes",
      ]);
    }
  }

  if (Object.prototype.hasOwnProperty.call(j, "live_tournament_stats")) {
    const lt = j.live_tournament_stats;
    if (lt && typeof lt === "object") {
      const lud = lt.last_updated != null ? String(lt.last_updated).trim() : "";
      if (DATA.meta.live_dg_tournament_stats_last !== lud) touched = true;
      if (lud) DATA.meta.live_dg_tournament_stats_last = lud;
      else delete DATA.meta.live_dg_tournament_stats_last;
      const ev = String(lt.event_name || "").trim();
      if (DATA.meta.live_dg_tournament_stats_event !== ev) touched = true;
      if (ev) DATA.meta.live_dg_tournament_stats_event = ev;
      else delete DATA.meta.live_dg_tournament_stats_event;
    } else {
      clearKeys(["live_dg_tournament_stats_last", "live_dg_tournament_stats_event"]);
    }
  }

  return touched;
}

function clearDgLiveRoundScratch(p) {
  delete p.dg_live_thru;
  delete p.dg_live_today;
  delete p.dg_live_birdies_so_far;
  delete p.dg_live_bogeys_so_far;
  delete p.dg_live_pars_so_far;
  delete p.dg_live_eagles_so_far;
}

/** Optional hole counts from preds/in-play (field names vary by DG version). */
function mergeDgLiveScorecardCounts(p, inPlayRow, thruRounded) {
  const th = Math.round(num(thruRounded, NaN));
  const cap = Number.isFinite(th) && th > 0 ? th + 3 : 22;
  const q = (aliases) => {
    const v = num(dgInPlayField(inPlayRow, aliases), NaN);
    if (!Number.isFinite(v) || v < 0) return NaN;
    const r = Math.round(v);
    return r <= cap ? r : NaN;
  };
  const setCt = (val, key) => {
    if (Number.isFinite(val) && val >= 0 && val <= 22) p[key] = val;
    else delete p[key];
  };
  setCt(q(["today_birdies", "round_birdies", "birdies_today", "birdies_thru", "n_birdies"]), "dg_live_birdies_so_far");
  setCt(
    q(["today_bogeys", "round_bogeys", "bogeys_today", "bogies_today", "today_bogies", "bogeys_thru"]),
    "dg_live_bogeys_so_far"
  );
  setCt(q(["today_pars", "round_pars", "pars_today", "pars_thru"]), "dg_live_pars_so_far");
  setCt(q(["today_eagles", "eagles_today", "eagles_or_better_today", "eagles_thru"]), "dg_live_eagles_so_far");
  const genB = num(dgInPlayField(inPlayRow, ["birdies"]), NaN);
  if (
    !Object.prototype.hasOwnProperty.call(p, "dg_live_birdies_so_far") &&
    Number.isFinite(genB) &&
    Number.isFinite(th) &&
    th >= 1 &&
    genB >= 0 &&
    genB <= th
  ) {
    p.dg_live_birdies_so_far = Math.round(genB);
  }
}

/** Tee times per round from field_updates → projection rows (`dg_teetime_local`) for forecast slicing. */
function mergeDgFieldTeeTimesIntoPlayers(fieldUpdatesRaw) {
  if (!fieldUpdatesRaw || typeof fieldUpdatesRaw !== "object" || !DATA.players?.length) return 0;
  if (!DATA.meta) DATA.meta = {};
  const ds = fieldUpdatesRaw.date_start != null ? String(fieldUpdatesRaw.date_start).trim() : "";
  if (ds) DATA.meta.datagolf_field_date_start = ds;
  const flist =
    fieldUpdatesRaw.field ??
    fieldUpdatesRaw.field_updates ??
    fieldUpdatesRaw.players ??
    fieldUpdatesRaw.data;
  if (!Array.isArray(flist) || !flist.length) return 0;
  const byDg = new Map();
  for (const fp of flist) {
    const id = Math.round(num(fp?.dg_id ?? fp?.dgId, NaN));
    if (!Number.isFinite(id)) continue;
    byDg.set(id, Array.isArray(fp.teetimes) ? fp.teetimes : []);
  }
  let n = 0;
  for (const p of DATA.players) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const tt = byDg.get(id);
    if (!Array.isArray(tt)) {
      delete p.dg_teetime_local;
      delete p.dg_tee_wave;
      continue;
    }
    const rnd = Math.round(num(p.round, NaN));
    const slot = tt.find((t) => Math.round(num(t.round_num, NaN)) === rnd);
    if (slot && slot.teetime != null && String(slot.teetime).trim() !== "") {
      p.dg_teetime_local = String(slot.teetime).trim();
      p.dg_tee_wave = String(slot.wave || "").trim();
      n++;
    } else {
      delete p.dg_teetime_local;
      delete p.dg_tee_wave;
    }
  }
  return n;
}

/**
 * Overlay DataGolf `field-updates` leaderboard numbers onto preds/in-play `data[]` (by dg_id)
 * when the live bundle includes `field_updates` (see scripts/fetch-live-in-play.mjs).
 */
function mergeDgFieldScoresFromBundleIntoData(dataRows, fieldUpdatesRaw) {
  if (!Array.isArray(dataRows) || !dataRows.length || !fieldUpdatesRaw || typeof fieldUpdatesRaw !== "object") return 0;
  const flist =
    fieldUpdatesRaw.field ?? fieldUpdatesRaw.field_updates ?? fieldUpdatesRaw.players ?? fieldUpdatesRaw.data;
  if (!Array.isArray(flist) || !flist.length) return 0;
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
  const byDg = new Map();
  for (const p of flist) {
    if (!p || typeof p !== "object") continue;
    const id = Math.round(num(p.dg_id ?? p.dgId, NaN));
    if (!Number.isFinite(id)) continue;
    let sc = NaN;
    for (const k of keys) {
      const v = num(p[k], NaN);
      if (Number.isFinite(v)) {
        sc = v;
        break;
      }
    }
    if (Number.isFinite(sc)) byDg.set(id, sc);
  }
  if (!byDg.size) return 0;
  let n = 0;
  for (const row of dataRows) {
    if (!row || typeof row !== "object") continue;
    const id = Math.round(num(dgInPlayField(row, ["dg_id", "dgId"]) ?? row.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const sc = byDg.get(id);
    if (!Number.isFinite(sc)) continue;
    const prev = num(row.current_score ?? row.currentScore, NaN);
    if (!Number.isFinite(prev) || Math.abs(prev - sc) > 1e-6) n++;
    row.current_score = sc;
  }
  return n;
}

/**
 * Highest plausible tournament round (1–4) from the full live bundle.
 * preds/in-play `info.current_round` alone often lags after a rollover while field_updates,
 * live_hole_stats, or player `round` rows already show the next round.
 */
function dgLiveBundleConsensusCurrentRound(j) {
  if (!j || typeof j !== "object") return NaN;
  const info = j.info && typeof j.info === "object" ? j.info : {};
  /** @type {number[]} */
  const cands = [];
  const push = (v) => {
    const r = Math.round(num(v, NaN));
    if (Number.isFinite(r) && r >= 1 && r <= 4) cands.push(r);
  };
  push(info.current_round);
  push(j.current_round);
  const lh = j.live_hole_stats;
  if (lh && typeof lh === "object") {
    push(lh.current_round);
    const lhi = lh.info && typeof lh.info === "object" ? lh.info : {};
    push(lhi.current_round);
  }
  const fu = j.field_updates;
  if (fu && typeof fu === "object") push(fu.current_round ?? fu.currentRound);
  for (const row of Array.isArray(j.data) ? j.data : []) {
    if (row && typeof row === "object") push(row.round ?? row.Round);
  }
  push(DATA?.meta?.display_round);
  push(DATA?.meta?.datagolf_field_current_round);
  push(DATA?.meta?.datagolf_live_current_round);
  if (!cands.length) return NaN;
  return Math.max(...cands);
}

/**
 * Merge DataGolf preds/in-play `data` rows into DATA.players:
 * win, top_5, top_10, top_20, make_cut (and mc → make_cut when make_cut absent).
 * Placement probs are tournament-wide — update every round row for that dg_id.
 */
function mergeDatagolfInPlayPayload(j) {
  if (!j || typeof j !== "object" || !DATA.players || !DATA.players.length) return false;
  const metaTouched = mergeDatagolfLiveCourseMeta(j);
  let drivingTouched = false;
  if (!Array.isArray(j.data)) return metaTouched;
  const info = j.info && typeof j.info === "object" ? j.info : {};
  const currentRound = dgLiveBundleConsensusCurrentRound(j);
  const lastUpdate = info.last_update != null ? String(info.last_update) : "";
  const inPlayEvent = String(
    info.event_name ||
      j.event_name ||
      j.live_tournament_stats?.event_name ||
      ""
  ).trim();
  const modelEvent = String(DATA?.meta?.event_name || "").trim();
  const eventAligned =
    !inPlayEvent ||
    !modelEvent ||
    eventNameMatchesCurrentSchedule(inPlayEvent, modelEvent) ||
    eventNameMatchesCurrentSchedule(modelEvent, inPlayEvent);
  // Only overlay placement probabilities when in-play bundle and projections refer to the same tournament.
  // A cross-event merge can produce near-certain model prices against unrelated outright books.
  if (!eventAligned) {
    delete DATA.live_in_play_snapshot;
    /* preds/in-play `info.event_name` can lag a week while field-updates already match this event.
       Still merge tee times + date_start from field_updates so Open-Meteo / per-tee weather works. */
    const fu = j.field_updates && typeof j.field_updates === "object" ? j.field_updates : null;
    const fuEvent = String(fu?.event_name ?? fu?.eventName ?? "").trim();
    const fuAligns =
      fu &&
      (!fuEvent ||
        !modelEvent ||
        eventNameMatchesCurrentSchedule(fuEvent, modelEvent) ||
        eventNameMatchesCurrentSchedule(modelEvent, fuEvent));
    if (fuAligns) {
      mergeDgFieldTeeTimesIntoPlayers(fu);
    } else {
      for (const p of DATA.players || []) {
        delete p.dg_teetime_local;
        delete p.dg_tee_wave;
        delete p.dg_auto_weather;
      }
    }
    if (DATA.meta) {
      DATA.meta.datagolf_live_event_mismatch = `${inPlayEvent} vs ${modelEvent}`;
      DATA.meta.datagolf_live_placement_rows_merged = 0;
      /* Stale live bundle (wrong week) must not leave R4 etc. on meta — that forces post-cut UI off pre-tournament rows. */
      delete DATA.meta.datagolf_live_current_round;
    }
    return metaTouched;
  }
  if (DATA.meta) delete DATA.meta.datagolf_live_event_mismatch;
  DATA.live_in_play_snapshot = j.data;
  drivingTouched = mergeLiveTournamentDrivingIntoPlayers(j);
  if (j.field_updates && typeof j.field_updates === "object") {
    mergeDgFieldScoresFromBundleIntoData(j.data, j.field_updates);
    mergeDgFieldTeeTimesIntoPlayers(j.field_updates);
  }
  let touched = 0;
  const countingTouched = mergeLiveTournamentCountingIntoProjections(j);
  if (countingTouched) touched++;
  for (const row of j.data) {
    if (!row || typeof row !== "object") continue;
    const id = Math.round(num(dgInPlayField(row, ["dg_id", "dgId"]) ?? row.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const win = datagolfModelProb01(dgInPlayField(row, ["win", "win_prob", "p_win"]));
    const top5 = datagolfModelProb01(dgInPlayField(row, ["top_5", "top5"]));
    const top10 = datagolfModelProb01(dgInPlayField(row, ["top_10", "top10"]));
    const top20 = datagolfModelProb01(dgInPlayField(row, ["top_20", "top20"]));
    let makeCut = datagolfModelProb01(dgInPlayField(row, ["make_cut", "makeCut"]));
    if (!Number.isFinite(makeCut)) {
      const mcRaw = dgInPlayField(row, ["mc", "miss_cut", "missCut"]);
      const mcP = datagolfModelProb01(mcRaw);
      if (Number.isFinite(mcP)) makeCut = 1 - mcP;
    }
    const byId = DATA.players.filter((p) => Math.round(num(p.dg_id, NaN)) === id);
    if (!byId.length) continue;
    const placementFromApi =
      Number.isFinite(win) ||
      Number.isFinite(top5) ||
      Number.isFinite(top10) ||
      Number.isFinite(top20) ||
      Number.isFinite(makeCut);
    const curPos = dgInPlayField(row, ["current_pos", "currentPos"]);
    const curScore = dgInPlayField(row, ["current_score", "currentScore"]);
    const dgRound = Math.round(num(dgInPlayField(row, ["round", "Round"]), NaN));
    const thruLive = num(dgInPlayField(row, ["thru", "Thru"]), NaN);
    const todayLive = num(dgInPlayField(row, ["today", "Today"]), NaN);
    for (const p of byId) {
      if (Number.isFinite(win)) p.win = win;
      if (Number.isFinite(top5)) p.top_5 = top5;
      if (Number.isFinite(top10)) p.top_10 = top10;
      if (Number.isFinite(top20)) p.top_20 = top20;
      if (Number.isFinite(makeCut)) p.make_cut = makeCut;
      if (placementFromApi) p.dg_live_placement_from_api = true;
      if (curPos != null && String(curPos).trim() !== "") p.current_pos = String(curPos).trim();
      if (Number.isFinite(num(curScore, NaN))) p.current_score = num(curScore, NaN);
      const pr = Math.round(num(p.round, NaN));
      if (!Number.isFinite(dgRound) || dgRound < 1 || dgRound > 4) {
        clearDgLiveRoundScratch(p);
      } else if (pr === dgRound) {
        if (Number.isFinite(thruLive)) p.dg_live_thru = thruLive;
        else delete p.dg_live_thru;
        if (Number.isFinite(todayLive)) p.dg_live_today = todayLive;
        else delete p.dg_live_today;
        if (Number.isFinite(thruLive) && Math.round(thruLive) >= 1) {
          mergeDgLiveScorecardCounts(p, row, thruLive);
        } else {
          delete p.dg_live_birdies_so_far;
          delete p.dg_live_bogeys_so_far;
          delete p.dg_live_pars_so_far;
          delete p.dg_live_eagles_so_far;
        }
      } else {
        clearDgLiveRoundScratch(p);
      }
      touched++;
    }
  }
  if (DATA.meta) {
    if (lastUpdate) DATA.meta.datagolf_live_last_update = lastUpdate;
    if (Number.isFinite(currentRound)) DATA.meta.datagolf_live_current_round = currentRound;
    DATA.meta.datagolf_live_placement_rows_merged = touched;
    if (
      touched > 0 &&
      inPlayAffectsRoundOdds() &&
      !Object.prototype.hasOwnProperty.call(DATA.meta, "live_matchup_model_blend")
    ) {
      DATA.meta.live_matchup_model_blend = 0.35;
    }
  }
  return touched > 0 || metaTouched || drivingTouched || countingTouched;
}

/** Match build-player-history normEvt() for upserting live rows onto CSV-backed rounds. */
function liveHistNormEvtKey(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/&/g, " and ")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

/** First tournament round date (ISO YYYY-MM-DD); round n → that calendar day + (n−1). */
function eventCompletedMdYForRoundLiveHist(dateStartIso, roundNum) {
  if (!dateStartIso || roundNum < 1) return "";
  const m = String(dateStartIso).match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return "";
  const t = Date.UTC(+m[1], +m[2] - 1, +m[3]) + (roundNum - 1) * 86400000;
  const d = new Date(t);
  return `${d.getUTCMonth() + 1}/${d.getUTCDate()}/${d.getUTCFullYear()}`;
}

function liveHistDateStartIsFuture(dateStartIso) {
  const m = String(dateStartIso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return false;
  const start = Date.UTC(+m[1], +m[2] - 1, +m[3]);
  const now = new Date();
  const today = Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate());
  return Number.isFinite(start) && start > today;
}

function projectionRowForDgRound(dgId, rnd) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id) || !DATA.players || !DATA.players.length) return null;
  const rWant = Math.round(num(rnd, NaN));
  if (!Number.isFinite(rWant) || rWant < 1) return null;
  for (const p of DATA.players) {
    if (Math.round(num(p.dg_id, NaN)) !== id) continue;
    const pr = Math.round(num(p.round, NaN));
    if (pr === rWant) return p;
  }
  return null;
}

const MAX_HISTORY_ROUNDS_PER_PLAYER = 2000;

function dgInPlayRowForId(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return null;
  const snap = Array.isArray(DATA?.live_in_play_snapshot)
    ? DATA.live_in_play_snapshot
    : Array.isArray(DATA?.data)
      ? DATA.data
      : [];
  return snap.find((x) => Math.round(num(x?.dg_id ?? x?.dgId, NaN)) === id) || null;
}

function liveInPlayGrossForRound(inPlayRow, rnd) {
  if (!inPlayRow) return NaN;
  const r = Math.round(num(rnd, NaN));
  if (!Number.isFinite(r) || r < 1 || r > 4) return NaN;
  return num(inPlayRow[`R${r}`] ?? inPlayRow[`r${r}`], NaN);
}

function currentEventLiveRoundNum() {
  const mismatch = String(DATA?.meta?.datagolf_live_event_mismatch || "").trim();
  if (mismatch) return NaN;
  let liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  if (!Number.isFinite(liveR) && lastLiveInPlayBundleForHistory) {
    liveR = Math.round(num(dgLiveBundleConsensusCurrentRound(lastLiveInPlayBundleForHistory), NaN));
  }
  return Number.isFinite(liveR) && liveR >= 1 && liveR <= 4 ? liveR : NaN;
}

function currentTournamentProgressRoundCap() {
  const liveR = currentEventLiveRoundNum();
  if (Number.isFinite(liveR)) return liveR;
  const dr = Math.round(num(DATA?.meta?.display_round, NaN));
  return Number.isFinite(dr) && dr >= 1 && dr <= 4 ? dr : NaN;
}

function historyRoundMatchesCurrentEvent(row) {
  if (!row || typeof row !== "object") return false;
  const metaEvent = String(DATA?.meta?.event_name || "").trim();
  if (!metaEvent) return false;
  return (
    eventNameMatchesCurrentSchedule(row.event_name, metaEvent) ||
    scheduleNameMatchesMeta(row.event_name, metaEvent)
  );
}

/** Counting stats on live-merge rows must not come from projections.json (μ_SG formulas). */
const LIVE_HISTORY_COUNTING_KEYS = ["birdies", "pars", "bogies", "bogeys", "gir", "fairways", "putts"];

function historyRowHasStoredCountingStat(row, key) {
  if (!row || typeof row !== "object") return false;
  const v = row[key];
  if (v == null || v === "") return false;
  const n = Number(v);
  if (!Number.isFinite(n)) return false;
  if ((key === "gir" || key === "fairways" || key === "putts") && (n === 0 || n === 1)) return false;
  if (
    (key === "birdies" || key === "pars" || key === "bogies" || key === "bogeys") &&
    n === 0 &&
    row._from_live_tournament_stats &&
    !row._from_pgatour
  ) {
    return false;
  }
  return true;
}

/** DG live-stats often sends pars≈18 with no birdies/bogeys — not valid for Trends counting markets. */
function scrubLivePlaceholderCountingOnRow(row) {
  if (!row || typeof row !== "object") return row;
  if (row._from_pgatour) return row;
  if (!row._from_live_tournament_stats && !row._from_live_in_play) return row;
  const out = { ...row };
  const b = num(out.birdies, NaN);
  let p = num(out.pars, NaN);
  let bg = num(out.bogies ?? out.bogeys, NaN);
  const gir = num(out.gir, NaN);
  if (Number.isFinite(p) && p >= 10 && (!Number.isFinite(b) || b === 0) && (!Number.isFinite(bg) || bg === 0)) {
    out.birdies = null;
    out.pars = null;
    out.bogies = null;
    out.bogeys = null;
  }
  if (Number.isFinite(gir) && Number.isFinite(p) && Math.round(gir) === Math.round(p)) out.pars = null;
  return out;
}

function historyLiveCountingTrusted(row) {
  if (!row || typeof row !== "object") return false;
  if (row._from_pgatour || row._from_dg_historical_rounds) return true;
  if (!row._from_live_tournament_stats && !row._from_live_in_play) return true;
  const b = num(row.birdies, NaN);
  const p = num(row.pars, NaN);
  const bg = num(row.bogies ?? row.bogeys, NaN);
  if (Number.isFinite(b) || Number.isFinite(bg)) return true;
  if (Number.isFinite(p) && p > 0 && p < 14) return true;
  if (Number.isFinite(p) && p >= 10 && (!Number.isFinite(b) || b === 0) && (!Number.isFinite(bg) || bg === 0)) return false;
  return false;
}

/** preds/in-play today_* counting for the active tournament round only. */
function liveInPlayCountingFromRow(ipRow, thruRounded) {
  if (!ipRow || typeof ipRow !== "object") return {};
  const th = Math.round(num(thruRounded, NaN));
  const cap = Number.isFinite(th) && th > 0 ? th + 3 : 22;
  const capCt = (v) => {
    if (!Number.isFinite(v) || v < 0) return NaN;
    const r = Math.round(v);
    return r <= cap ? r : NaN;
  };
  const birdies = capCt(
    num(ipRow.today_birdies ?? ipRow.round_birdies ?? ipRow.birdies_today ?? ipRow.birdies, NaN),
  );
  const bogeys = capCt(
    num(
      ipRow.today_bogeys ??
        ipRow.round_bogeys ??
        ipRow.bogeys_today ??
        ipRow.bogies_today ??
        ipRow.bogeys,
      NaN,
    ),
  );
  let pars = capCt(num(ipRow.today_pars ?? ipRow.round_pars ?? ipRow.pars_today ?? ipRow.pars, NaN));
  const eagles = capCt(num(ipRow.today_eagles ?? ipRow.eagles_today, NaN));
  if (!Number.isFinite(pars) && Number.isFinite(th) && th >= 1) {
    const b = Number.isFinite(birdies) ? birdies : 0;
    const bg = Number.isFinite(bogeys) ? bogeys : 0;
    const e = Number.isFinite(eagles) ? eagles : 0;
    pars = Math.max(0, Math.min(th, th - b - bg - e));
  }
  return { birdies, pars, bogeys, eagles };
}

const LIVE_HISTORY_SG_KEYS = ["sg_putt", "sg_app", "sg_arg", "sg_ott", "sg_t2g", "sg_total"];

/** Prefer CSV / pgatouR birdies/pars; keep live GIR/FW/putts/SG when pgatouR only has scorecard counting. */
function mergeLiveTournamentStatsOntoHistoryRound(existing, liveRec) {
  if (liveRec?._from_pgatour) {
    const out = {
      ...existing,
      ...liveRec,
      _from_pgatour: true,
      _from_live_tournament_stats: true,
    };
    for (const k of [...LIVE_HISTORY_COUNTING_KEYS, ...LIVE_HISTORY_SG_KEYS]) {
      if (Number.isFinite(num(liveRec[k], NaN))) out[k] = liveRec[k];
      else if (Number.isFinite(num(existing[k], NaN))) out[k] = existing[k];
    }
    return scrubLivePlaceholderCountingOnRow(out);
  }
  const cleaned = scrubLivePlaceholderCountingOnRow(liveRec);
  const prev = scrubLivePlaceholderCountingOnRow(existing);
  const out = { ...existing, ...cleaned };
  for (const k of LIVE_HISTORY_COUNTING_KEYS) {
    if (historyRowHasStoredCountingStat(prev, k) && historyLiveCountingTrusted(prev)) out[k] = prev[k];
    else if (Number.isFinite(num(cleaned[k], NaN))) out[k] = cleaned[k];
    else if (historyLiveCountingTrusted(prev)) out[k] = prev[k];
    else out[k] = cleaned[k] ?? null;
  }
  out._from_live_tournament_stats = true;
  delete out._from_live_in_play;
  return out;
}

function mergeLiveInPlayOntoHistoryRound(existing, liveRec) {
  if (liveRec?._from_live_tournament_stats) return mergeLiveTournamentStatsOntoHistoryRound(existing, liveRec);
  const out = { ...existing, ...liveRec };
  if (!liveRec || !liveRec._from_live_in_play) return out;
  for (const k of LIVE_HISTORY_COUNTING_KEYS) {
    if (historyRowHasStoredCountingStat(existing, k)) out[k] = existing[k];
    else out[k] = existing[k] ?? null;
  }
  return out;
}

function upsertHistoryBucketLiveRound(dgId, liveRec) {
  if (historyDateMdYIsFuture(liveRec?.event_completed)) return false;
  if (historyRoundChartDateIsFuture(liveRec)) return false;
  const key = String(dgId);
  let bucket = HISTORY.byDgId[key];
  if (!bucket || !Array.isArray(bucket.rounds)) {
    HISTORY.byDgId[key] = {
      dg_id: dgId,
      player_name: String(liveRec?.player_name || "").trim(),
      rounds: [],
    };
    bucket = HISTORY.byDgId[key];
  } else if (!bucket.player_name && liveRec?.player_name) {
    bucket.player_name = String(liveRec.player_name).trim();
  }
  const wantEvt = liveHistNormEvtKey(liveRec.event_name);
  const wantYr = parseInt(String(liveRec.year || ""), 10);
  const wantRnd = parseInt(String(liveRec.round_num || ""), 10);
  let hitIdx = -1;
  for (let i = bucket.rounds.length - 1; i >= 0; i--) {
    const rr = bucket.rounds[i];
    if (parseInt(String(rr.round_num || ""), 10) !== wantRnd) continue;
    if (Number.isFinite(wantYr) && parseInt(String(rr.year || ""), 10) !== wantYr) continue;
    if (liveHistNormEvtKey(rr.event_name) !== wantEvt) continue;
    hitIdx = i;
    break;
  }
  if (hitIdx >= 0) {
    const prev = bucket.rounds[hitIdx];
    const next = mergeLiveInPlayOntoHistoryRound(prev, liveRec);
    const changed = liveHistoryRoundMateriallyChanged(prev, next);
    bucket.rounds[hitIdx] = next;
    bucket.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
    if (bucket.rounds.length > MAX_HISTORY_ROUNDS_PER_PLAYER) {
      bucket.rounds = bucket.rounds.slice(-MAX_HISTORY_ROUNDS_PER_PLAYER);
    }
    return changed;
  }
  bucket.rounds.push(liveRec);
  bucket.rounds.sort((a, b) => num(a.sortKey, 0) - num(b.sortKey, 0));
  if (bucket.rounds.length > MAX_HISTORY_ROUNDS_PER_PLAYER) bucket.rounds = bucket.rounds.slice(-MAX_HISTORY_ROUNDS_PER_PLAYER);
  return true;
}

function liveHistoryRoundMateriallyChanged(prev, next) {
  if (!prev || !next) return true;
  const keys = ["round_score", "birdies", "pars", "bogies", "bogeys", "gir", "fairways", "putts"];
  for (const k of keys) {
    const a = num(prev[k], NaN);
    const b = num(next[k], NaN);
    if (Number.isFinite(a) || Number.isFinite(b)) {
      if (!Number.isFinite(a) || !Number.isFinite(b) || Math.abs(a - b) > 1e-6) return true;
    }
  }
  return false;
}

/** preds/in-play `R1`…`R4` gross (+ active-round counting) when precomputed actuals are missing. */
function syncFallbackLiveRoundActualsFromInPlay(j) {
  /** @type {Record<string, Record<string, object>>} */
  const byDg = {};
  if (!j || !Array.isArray(j.data)) return byDg;
  for (const row of j.data) {
    const dg = Math.round(num(row?.dg_id ?? row?.dgId, NaN));
    if (!Number.isFinite(dg)) continue;
    const playerR = Math.round(num(row?.round ?? row?.Round, NaN));
    for (let rnd = 1; rnd <= 4; rnd++) {
      const g = num(row[`R${rnd}`] ?? row[`r${rnd}`], NaN);
      if (!Number.isFinite(g) || g <= 0) continue;
      const dk = String(dg);
      const rk = String(rnd);
      if (!byDg[dk]) byDg[dk] = {};
      /** @type {Record<string, unknown>} */
      const rec = {
        round_score: Math.round(g * 10) / 10,
        source: "in_play_gross",
        birdies: null,
        pars: null,
        bogeys: null,
        gir: null,
      };
      if (playerR === rnd) {
        const thru = Math.round(num(row.thru ?? row.Thru, NaN));
        const ip = liveInPlayCountingFromRow(row, thru);
        if (Number.isFinite(ip.birdies)) rec.birdies = ip.birdies;
        if (Number.isFinite(ip.pars)) rec.pars = ip.pars;
        if (Number.isFinite(ip.bogeys)) rec.bogeys = ip.bogeys;
        rec.thru = Number.isFinite(thru) ? thru : null;
      }
      byDg[dk][rk] = scrubLivePlaceholderCountingOnRow(rec);
    }
  }
  return byDg;
}

function mergeLiveRoundActualsMaps(pre, built) {
  if (!built || typeof built !== "object") return pre && typeof pre === "object" ? pre : {};
  if (!pre || typeof pre !== "object") return built;
  const out = { ...pre };
  for (const [dgKey, per] of Object.entries(built)) {
    if (!per || typeof per !== "object") continue;
    if (!out[dgKey]) out[dgKey] = {};
    for (const [rk, rec] of Object.entries(per)) {
      const prev = out[dgKey][rk];
      if (prev && typeof prev === "object") {
        const merged = { ...prev, ...rec };
        if (Number.isFinite(num(prev.round_score, NaN)) && !Number.isFinite(num(rec.round_score, NaN)))
          merged.round_score = prev.round_score;
        out[dgKey][rk] = merged;
      } else {
        out[dgKey][rk] = rec;
      }
    }
  }
  return out;
}

function resolveLiveRoundActualsForHistory(j) {
  const pre = j?.live_round_actuals_by_dg;
  const fallback = syncFallbackLiveRoundActualsFromInPlay(j);
  return mergeLiveRoundActualsMaps(
    pre && typeof pre === "object" ? pre : null,
    fallback,
  );
}

/**
 * During live weeks (Thu–Sun): merge preds/live-tournament-stats round actuals into HISTORY for Historical Trends.
 * `fetch:in-play` builds `live_round_actuals_by_dg` from per-round Live Tournament Stats API pulls.
 */
function mergeLiveInPlayIntoRoundHistory(j) {
  if (!j || typeof j !== "object" || !HISTORY._ok || !HISTORY.byDgId) return 0;
  const actualsByDg = resolveLiveRoundActualsForHistory(j);
  if (!actualsByDg || typeof actualsByDg !== "object" || !Object.keys(actualsByDg).length) return 0;

  const info = j.info && typeof j.info === "object" ? j.info : {};
  const fu = j.field_updates && typeof j.field_updates === "object" ? j.field_updates : {};
  const inPlayEvent = String(info.event_name || fu.event_name || j.event_name || "").trim();
  const modelEvent = String(DATA?.meta?.event_name || "").trim();
  const eventAligned =
    !inPlayEvent ||
    !modelEvent ||
    eventNameMatchesCurrentSchedule(inPlayEvent, modelEvent) ||
    eventNameMatchesCurrentSchedule(modelEvent, inPlayEvent);
  if (!eventAligned) return 0;

  let dateStartIso = String(fu.date_start || info.date_start || DATA?.meta?.datagolf_field_date_start || "").trim();
  if (!/^\d{4}-\d{2}-\d{2}/.test(dateStartIso)) {
    const lastUp = String(info.last_update || "").trim();
    const isoM = lastUp.match(/^(\d{4})-(\d{2})-(\d{2})/);
    const cr = Math.round(num(dgLiveBundleConsensusCurrentRound(j), NaN));
    if (isoM && Number.isFinite(cr) && cr >= 1 && cr <= 4) {
      const t = Date.UTC(+isoM[1], +isoM[2] - 1, +isoM[3]) - (cr - 1) * 86400000;
      const d = new Date(t);
      dateStartIso = `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
    }
  }
  if (liveHistDateStartIsFuture(dateStartIso)) return 0;

  const eventName = String(modelEvent || fu.event_name || info.event_name || "").trim();
  if (!eventName) return 0;
  const courseName = String(DATA?.meta?.course_used || fu.course_name || "").trim() || eventName;
  const eventIdStr = fu.event_id != null && fu.event_id !== "" ? String(fu.event_id) : "";

  const inPlayRowByDg = new Map();
  if (Array.isArray(j.data)) {
    for (const r of j.data) {
      const dg = Math.round(num(r?.dg_id ?? r?.dgId, NaN));
      if (!Number.isFinite(dg)) continue;
      inPlayRowByDg.set(dg, r);
    }
  }

  let merged = 0;
  for (const [dgKey, perRound] of Object.entries(actualsByDg)) {
    const dg = Math.round(num(dgKey, NaN));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    const ipRow = inPlayRowByDg.get(dg);
    const plyName = String(ipRow?.player_name || ipRow?.playerName || "").trim();
    const playerR = Math.round(num(ipRow?.round ?? ipRow?.Round, NaN));

    for (const [rndKey, act] of Object.entries(perRound)) {
      if (!act || typeof act !== "object") continue;
      const rnd = Math.round(num(rndKey, NaN));
      if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
      let actScore = num(act.round_score, NaN);
      if (ipRow) {
        const g = liveInPlayGrossForRound(ipRow, rnd);
        if (Number.isFinite(g)) actScore = g;
      }
      if (!Number.isFinite(actScore) || actScore <= 0) continue;

      let eventDate = "";
      if (dateStartIso) eventDate = eventCompletedMdYForRoundLiveHist(dateStartIso, rnd);
      if (!eventDate) continue;

      let birdies = Number.isFinite(num(act.birdies, NaN)) ? num(act.birdies, NaN) : null;
      let pars = Number.isFinite(num(act.pars, NaN)) ? num(act.pars, NaN) : null;
      let bogeys = Number.isFinite(num(act.bogeys, NaN)) ? num(act.bogeys, NaN) : null;
      if (ipRow && playerR === rnd) {
        const thru = Math.round(num(act.thru ?? ipRow.thru, NaN));
        const ip = liveInPlayCountingFromRow(ipRow, thru);
        if (Number.isFinite(ip.birdies)) birdies = ip.birdies;
        if (Number.isFinite(ip.pars)) pars = ip.pars;
        if (Number.isFinite(ip.bogeys)) bogeys = ip.bogeys;
      }

      const eventYear = parseInt(String(eventDate).split("/")[2] || "", 10);
      const chronoBase = parseEventCompletedChronoBase(eventDate);
      const liveRec = scrubLivePlaceholderCountingOnRow({
        dg_id: dg,
        player_name: plyName,
        sortKey: chronoBase * 10 + rnd,
        event_completed: eventDate,
        year: Number.isFinite(eventYear) ? eventYear : new Date().getFullYear(),
        event_name: eventName,
        event_id: eventIdStr,
        course_name: courseName,
        round_num: rnd,
        fin_text: "",
        round_score: actScore,
        birdies,
        pars,
        bogies: bogeys,
        gir: Number.isFinite(num(act.gir, NaN)) ? num(act.gir, NaN) : null,
        fairways: Number.isFinite(num(act.fairways, NaN)) ? num(act.fairways, NaN) : null,
        putts: Number.isFinite(num(act.putts, NaN)) ? num(act.putts, NaN) : null,
        eagles_or_better: null,
        doubles_or_worse: null,
        weather_temp_f: null,
        weather_wind_mph: null,
        weather_humidity: null,
        weather_condition: "",
        sg_putt: Number.isFinite(num(act.sg_putt, NaN)) ? num(act.sg_putt, NaN) : null,
        sg_app: Number.isFinite(num(act.sg_app, NaN)) ? num(act.sg_app, NaN) : null,
        sg_arg: Number.isFinite(num(act.sg_arg, NaN)) ? num(act.sg_arg, NaN) : null,
        sg_ott: Number.isFinite(num(act.sg_ott, NaN)) ? num(act.sg_ott, NaN) : null,
        sg_t2g: Number.isFinite(num(act.sg_t2g, NaN)) ? num(act.sg_t2g, NaN) : null,
        sg_total: Number.isFinite(num(act.sg_total, NaN)) ? num(act.sg_total, NaN) : null,
        _from_live_tournament_stats: true,
      });
      const chartProbe = { ...liveRec, event_name: eventName };
      if (historyDateMdYIsFuture(eventDate) || historyRoundChartDateIsFuture(chartProbe)) continue;

      if (upsertHistoryBucketLiveRound(dg, liveRec)) merged++;
    }
  }

  if (merged > 0) {
    for (const dg of inPlayRowByDg.keys()) {
      const bucket = HISTORY.byDgId?.[String(dg)];
      if (!bucket?.rounds) continue;
      bucket.rounds = bucket.rounds.map((r) => scrubLivePlaceholderCountingOnRow(r));
    }
    scrubNonActualRoundsFromHistoryBuckets();
    HISTORY_ROUNDS_CHRONO_CACHE.clear();
    PRICING_MU_BONUS_CACHE.clear();
    bumpHistoryMutationEpoch();
  }
  return merged;
}

function liveInPlayHistoryBundleWithActuals(j) {
  if (!j || typeof j !== "object") return j;
  return { ...j, live_round_actuals_by_dg: resolveLiveRoundActualsForHistory(j) };
}

function reapplyLiveInPlayHistoryMerge() {
  if (!HISTORY._ok || !lastLiveInPlayBundleForHistory) return 0;
  return mergeLiveInPlayIntoRoundHistory(liveInPlayHistoryBundleWithActuals(lastLiveInPlayBundleForHistory));
}

/**
 * Fetch preds/in-play and merge completed live-week rounds into HISTORY for Historical Trends.
 * Runs even when meta.poll_datagolf_live_predictions is false (odds overlay off).
 */
async function ensureLiveTournamentHistoryMerged(opts = {}) {
  if (isFileProtocol()) return 0;
  if (!DATA.players?.length) return 0;
  if (opts.useCache !== false && HISTORY._ok && lastLiveInPlayBundleForHistory) {
    const n = reapplyLiveInPlayHistoryMerge();
    if (n > 0) {
      refreshPricingAffectedViews();
      updateStatusBar();
    }
    return n;
  }
  if (liveTournamentHistoryMergeInFlight) return liveTournamentHistoryMergeInFlight;
  liveTournamentHistoryMergeInFlight = (async () => {
    try {
      const res = await fetch(cacheBustFetchUrl(liveInPlayJsonUrl()), { cache: "no-store" });
      if (!res.ok) return 0;
      const j = await res.json();
      lastLiveInPlayBundleForHistory = j;
      const histBundle = liveInPlayHistoryBundleWithActuals(j);
      lastLiveInPlayBundleForHistory = histBundle;
      if (!HISTORY._ok) return 0;
      const histMerged = mergeLiveInPlayIntoRoundHistory(histBundle);
      if (histMerged > 0) {
        refreshPricingAffectedViews();
        updateStatusBar();
      }
      return histMerged;
    } catch (_) {
      return 0;
    } finally {
      liveTournamentHistoryMergeInFlight = null;
    }
  })();
  return liveTournamentHistoryMergeInFlight;
}

/** Overlay live-tournament-stats counting actuals onto projection rows for the live tournament week. */
function mergeLiveTournamentCountingIntoProjections(j) {
  if (!j || typeof j !== "object" || !DATA.players?.length) return false;
  if (!inPlayAffectsRoundOdds()) return false;
  const actualsByDg = j.live_round_actuals_by_dg;
  if (!actualsByDg || typeof actualsByDg !== "object") return false;

  const info = j.info && typeof j.info === "object" ? j.info : {};
  const fu = j.field_updates && typeof j.field_updates === "object" ? j.field_updates : {};
  const inPlayEvent = String(info.event_name || fu.event_name || j.event_name || "").trim();
  const modelEvent = String(DATA?.meta?.event_name || "").trim();
  if (
    inPlayEvent &&
    modelEvent &&
    !eventNameMatchesCurrentSchedule(inPlayEvent, modelEvent) &&
    !eventNameMatchesCurrentSchedule(modelEvent, inPlayEvent)
  ) {
    return false;
  }

  let touched = 0;
  for (const p of DATA.players) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const perRound = actualsByDg[String(id)] ?? actualsByDg[id];
    if (!perRound || typeof perRound !== "object") continue;
    const pr = Math.round(num(p.round, NaN));
    if (!Number.isFinite(pr) || pr < 1 || pr > 4) continue;
    const act = perRound[String(pr)];
    if (!act || typeof act !== "object") continue;

    const thru = Math.round(num(act.thru, NaN));
    const rs = num(act.round_score, NaN);
    const b = num(act.birdies, NaN);
    const pa = num(act.pars, NaN);
    const bg = num(act.bogeys, NaN);

    if (Number.isFinite(rs)) {
      p.total_score = Math.round(rs * 10) / 10;
      touched++;
    }
    if (Number.isFinite(b)) {
      p.birdies = Math.round(b * 10) / 10;
      touched++;
    }
    if (Number.isFinite(pa)) {
      p.pars = Math.round(pa * 10) / 10;
      touched++;
    }
    if (Number.isFinite(bg)) {
      p.bogeys = Math.round(bg * 10) / 10;
      touched++;
    }
    if (Number.isFinite(thru) && thru >= 1) {
      p.dg_live_thru = thru;
      if (Number.isFinite(num(act.today, NaN))) p.dg_live_today = num(act.today, NaN);
      if (Number.isFinite(b)) p.dg_live_birdies_so_far = Math.round(b);
      if (Number.isFinite(bg)) p.dg_live_bogeys_so_far = Math.round(bg);
      if (Number.isFinite(pa)) p.dg_live_pars_so_far = Math.round(pa);
    }
  }
  return touched > 0;
}

async function fetchAndMergeDatagolfLiveInPlay(opts = {}) {
  let force = Boolean(opts.force);
  if (!force) {
    datagolfLivePeriodicForceTick += 1;
    if (datagolfLivePeriodicForceTick >= 8) {
      force = true;
      datagolfLivePeriodicForceTick = 0;
    }
  } else {
    datagolfLivePeriodicForceTick = 0;
  }
  if (isFileProtocol()) return;
  if (!force && datagolfLivePollingDisabledExplicitly()) return;
  if (!force && !datagolfLiveOverlayEnabled()) return;
  if (!DATA.players || !DATA.players.length) return;
  const url = cacheBustFetchUrl(liveInPlayJsonUrl());
  try {
    const res = await fetch(url, { cache: "no-store" });
    if (!res.ok) return;
    const j = await res.json();
    lastLiveInPlayBundleForHistory = j;
    const token = dgInPlayUpdateToken(j);
    if (!force && token && lastDatagolfInPlayToken && token === lastDatagolfInPlayToken) {
      if (HISTORY._ok) reapplyLiveInPlayHistoryMerge();
      return;
    }
    const histBundle = liveInPlayHistoryBundleWithActuals(j);
    lastLiveInPlayBundleForHistory = histBundle;
    const merged = datagolfLiveOverlayEnabled() ? mergeDatagolfInPlayPayload(j) : false;
    const histMerged = HISTORY._ok ? mergeLiveInPlayIntoRoundHistory(histBundle) : 0;
    const roundBumped = syncLbRoundToTournamentModelRound();
    if (token) lastDatagolfInPlayToken = token;
    if (merged || roundBumped || histMerged > 0) {
      refreshPricingAffectedViews();
      updateStatusBar();
    }
    void refreshForecastWeatherFromOpenMeteo().then((fwOk) => {
      if (fwOk) {
        refreshPricingAffectedViews();
        updateStatusBar();
      }
    });
  } catch (_) {
    /* ignore missing live file or CORS */
  }
}

function stopDatagolfLivePolling() {
  if (datagolfLivePollTimerId) {
    window.clearInterval(datagolfLivePollTimerId);
    datagolfLivePollTimerId = 0;
  }
}

function startDatagolfLivePolling() {
  stopDatagolfLivePolling();
  if (!datagolfLiveOverlayEnabled() || isFileProtocol()) return;
  const ms = datagolfLivePollIntervalMs();
  if (!ms) return;
  datagolfLivePollTimerId = window.setInterval(() => {
    void fetchAndMergeDatagolfLiveInPlay();
  }, ms);
}

/**
 * Interval for silent projections reload; 0 = disabled (default when meta omits or sets 0).
 * URL ?poll= overrides; else meta.projections_poll_interval_sec (0 or 15–3600).
 */
function projectionsPollIntervalMs() {
  if (isFileProtocol()) return 0;
  try {
    const raw = new URLSearchParams(window.location.search).get("poll");
    if (raw === null || raw === "") {
      const msec = num(DATA?.meta?.projections_poll_interval_sec, NaN);
      if (Number.isFinite(msec) && msec === 0) return 0;
      if (Number.isFinite(msec) && msec >= 15 && msec <= 3600) return msec * 1000;
      return 0;
    }
    const t = String(raw).trim().toLowerCase();
    if (t === "0" || t === "false" || t === "off" || t === "no") return 0;
    const sec = Number(raw);
    if (!Number.isFinite(sec) || sec <= 0) return 0;
    return Math.min(3600, Math.max(15, sec)) * 1000;
  } catch (_) {}
  return 0;
}

/** Chain timeouts so the next poll only starts after the previous load finishes (setInterval was skipping while inFlight). */
function scheduleProjectionsPollTimeout() {
  window.clearTimeout(projectionsPollTimerId);
  projectionsPollTimerId = 0;
  const ms = projectionsPollIntervalMs();
  projectionsPollMs = ms;
  if (!ms) return;
  projectionsPollTimerId = window.setTimeout(() => {
    void (async () => {
      try {
        await loadProjections({ silent: true, reloadSidecar: false });
      } catch (_) {
        /* errors handled inside loadProjections */
      } finally {
        scheduleProjectionsPollTimeout();
      }
    })();
  }, ms);
}

function startProjectionsPolling() {
  scheduleProjectionsPollTimeout();
}

function isFileProtocol() {
  return typeof location !== "undefined" && location.protocol === "file:";
}

function showFileProtocolBanner(on) {
  const el = document.getElementById("file-protocol-banner");
  if (el) el.hidden = !on;
}

function setBootError(msg) {
  const el = document.getElementById("boot-error");
  if (!el) return;
  if (msg) {
    el.textContent = msg;
    el.hidden = false;
  } else {
    el.textContent = "";
    el.hidden = true;
  }
}

function applyPayload(raw) {
  const prevFieldFp = playerDgFingerprint(DATA.players);
  const players = Array.isArray(raw.players) ? raw.players : [];
  const props = Array.isArray(raw.props) ? raw.props : [];
  let outrights = raw.outrights && typeof raw.outrights === "object" ? raw.outrights : {};
  let matchups = raw.matchups && typeof raw.matchups === "object" ? raw.matchups : {};
  const meta = { ...raw };
  delete meta.players;
  delete meta.props;
  delete meta.outrights;
  delete meta.matchups;

  if (!outrightPayloadHasRows(outrights) && players.length) {
    outrights = buildDemoOutrightsFromPlayers(players);
  }
  if (!matchups || !Object.keys(matchups).length) {
    matchups = buildDemoMatchupsFromPlayers(players);
  }

  DATA = {
    players,
    props,
    meta,
    outrights,
    matchups,
  };
  approachSkillYtdCache = null;
  approachSkillYtdLoadPromise = null;
  COURSE_TABLE_PAYLOAD = null;
  const nextFieldFp = playerDgFingerprint(players);
  if (prevFieldFp !== nextFieldFp) lastDatagolfInPlayToken = "";
  hydrateBakedWeatherFromPlayerFields();
}

/** O/U + model default round from meta (see effectiveUiModelRoundFromMeta); **1** if unknown. */
function ouDisplayRoundAuto() {
  const eff = effectiveUiModelRoundFromMeta();
  if (Number.isFinite(eff) && eff >= 1 && eff <= 4) return eff;
  return 1;
}

function getOuRound() {
  const sel = document.getElementById("lb-round");
  const v = sel ? String(sel.value) : "1";
  const n = num(v, NaN);
  if (Number.isFinite(n) && n >= 1 && n <= 4) return Math.round(n);
  return ouDisplayRoundAuto();
}

/** Max of live DG round, export display_round, and O/U picker — drives post-cut list filtering. */
function tournamentMaxEffectiveRound() {
  const mm = String(DATA?.meta?.datagolf_live_event_mismatch || "").trim();
  const liveR = mm ? NaN : Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  const drEff = effectiveUiModelRoundFromMeta();
  const ou = Math.round(getOuRound());
  const liveOk = Number.isFinite(liveR) && liveR >= 1 ? liveR : 0;
  const drOk = Number.isFinite(drEff) && drEff >= 1 ? drEff : 0;
  const ouOk = Number.isFinite(ou) && ou >= 1 ? ou : 0;
  /* When live reports a round, do not treat stale export display_round as “ahead” of live (calendar R2 bump). */
  if (liveOk) return Math.max(liveOk, ouOk);
  return Math.max(drOk, ouOk);
}

/**
 * True when missed-cut / WD-style players should be hidden from O/U, +EV, matchups, etc.
 * R3+ always; during R2 also once any player row is definitively eliminated (covers Friday cut before meta bumps to 3).
 */
function tournamentPostCutListPhase() {
  const mx = tournamentMaxEffectiveRound();
  if (mx >= 3) return true;
  if (mx < 2) return false;
  if (!Array.isArray(DATA.players)) return false;
  return DATA.players.some((p) => isPlayerEliminatedFromEvent(p));
}

function isPlayerEliminatedFromEvent(playerRow) {
  if (!playerRow || typeof playerRow !== "object") return false;
  const mc = playerRow.make_cut;
  if (mc === false) return true;
  if (mc === true) return false;
  if (typeof mc === "boolean") return !mc;
  /* Number(null)===0 — null/undefined make_cut must mean “unknown”, not missed cut (clears every tab in post-cut phase). */
  if (mc == null || mc === "") return false;
  const n = num(mc, NaN);
  if (Number.isFinite(n) && n <= 0) return true;
  const pos = String(playerRow.current_pos || "");
  return /\b(CUT|WD|DQ|MDF|DNS|W\/D|RET)\b/i.test(pos);
}

/** dg_ids to omit from post-cut actionable markets (not used for make_cut / mc outright tabs). */
function dgIdsEliminatedFromEventPostCut() {
  const out = new Set();
  if (!tournamentPostCutListPhase() || !Array.isArray(DATA.players)) return out;
  for (const p of DATA.players) {
    if (!isPlayerEliminatedFromEvent(p)) continue;
    const id = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(id)) out.add(id);
  }
  return out;
}

function updateRoundLabels() {
  const ar = document.getElementById("auto-round");
  const meta = DATA.meta || {};
  const dr = ouDisplayRoundAuto();
  const exportedR = Math.round(num(meta.display_round, NaN));
  if (ar) {
    ar.hidden = true;
    const rawLabel = meta.display_round_label && String(meta.display_round_label).trim();
    if (rawLabel && Number.isFinite(exportedR) && exportedR === dr) {
      ar.textContent = rawLabel.replace(/\s*\([^)]*America\/New_York[^)]*\)\s*/i, "").trim();
    } else {
      ar.textContent = `R${dr}`;
    }
  }
}

function formatDataSizeBytes(n) {
  const x = num(n, NaN);
  if (!Number.isFinite(x) || x < 0) return "";
  if (x >= 1e9) return `${(x / 1e9).toFixed(2)} GB`;
  if (x >= 1e6) return `${(x / 1e6).toFixed(0)} MB`;
  if (x >= 1e3) return `${(x / 1e3).toFixed(0)} KB`;
  return `${Math.round(x)} B`;
}

/** Event first, then venue — status bar used to show only `course_used`, which reads like the wrong “tournament”. */
function metaEventVenueLabel() {
  const m = DATA.meta || {};
  const ev = m.event_name ? String(m.event_name).trim() : "";
  const course = m.course_used ? String(m.course_used).trim() : "";
  if (ev && course) return `${ev} · ${course}`;
  return ev || course || "";
}

function metaEventVenueHtmlNote() {
  const s = metaEventVenueLabel();
  if (!s) return "";
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

function updateStatusBar() {
  const el = document.getElementById("data-status-primary");
  if (!el) return;
  const m = DATA.meta || {};
  const ev = m.event_name ? String(m.event_name).trim() : "";
  const course = m.course_used ? String(m.course_used).trim() : "";
  const line = metaEventVenueLabel() || "—";
  el.textContent = line;
  el.title = ev && course ? `${ev}\n${course}` : line;
}

function configureRoundPickerUi() {
  const sel = document.getElementById("lb-round");
  if (!sel) return;
  if (syncLbRoundToTournamentModelRound()) return;
  const dr = ouDisplayRoundAuto();
  sel.value = String(dr);
}

const OU_STAT_MAP = {
  "Total score": { field: "total_score", sdKey: "round_sd" },
  Birdies: { field: "birdies", sdKey: null },
  Pars: { field: "pars", sdKey: null },
  Bogeys: { field: "bogeys", sdKey: null },
  GIR: { field: "gir", sdKey: null },
  "Fairways hit": { field: "fairways", sdKey: null },
};

/** Model O/U tab market order; displayed columns are dynamic from live DK props. */
const OU_PROJECTION_MARKETS = Object.freeze([
  { market: "Total score", label: "Round score" },
  { market: "Birdies", label: "Birdies" },
  { market: "Pars", label: "Pars" },
  { market: "Bogeys", label: "Bogeys" },
  { market: "GIR", label: "GIR" },
  { market: "Fairways hit", label: "Fairways" },
]);

/**
 * Props used to resolve lines and odds (DK scrape + CSV + model fallback rows in JSON).
 * {@link ouProjectionColumnsActive} lists every standard market once `players` exist; these rows supply lines when present.
 */
function ouRoundOuPropsForLines() {
  const props = Array.isArray(DATA.props) ? DATA.props : [];
  const merged = props.filter((r) => {
    const s = String(r.source || "").trim().toLowerCase();
    return !s || s === "draftkings" || s === "csv" || s === "model_fallback";
  });
  return merged.length ? merged : props;
}

function draftKingsRoundPropOddsAvailable() {
  const props = Array.isArray(DATA.props) ? DATA.props : [];
  return props.some((r) => {
    const source = String(r.source || "").trim().toLowerCase();
    if (source !== "draftkings") return false;
    return (
      Number.isFinite(enforceHalfLine(num(r.line, NaN))) &&
      Number.isFinite(num(r.over_odds, NaN)) &&
      Number.isFinite(num(r.under_odds, NaN))
    );
  });
}

function updateOuSyntheticOddsNoteVisibility() {
  const el = document.getElementById("ou-synthetic-odds-note");
  if (!el) return;
  el.hidden = draftKingsRoundPropOddsAvailable();
}

/**
 * Columns for the Round projections grid + Market filter.
 * With a loaded field, always list every standard market so Birdies/GIR/etc. stay visible even when DraftKings
 * only posts lines for a subset (parse probes vary by event). Book columns still fill only when props exist.
 * Without players, infer columns from props only (demo / props-only payloads).
 */
function ouProjectionColumnsActive() {
  const players = Array.isArray(DATA.players) ? DATA.players : [];
  if (players.length) return [...OU_PROJECTION_MARKETS];

  const props = Array.isArray(DATA.props) ? DATA.props : [];
  const dk = props.filter((r) => String(r.source || "").trim().toLowerCase() === "draftkings");
  const nonDk = props.filter((r) => {
    const s = String(r.source || "").trim().toLowerCase();
    return s === "csv" || s === "model_fallback" || !s;
  });
  const dkSet = new Set(dk.map((r) => String(r.market || "").trim()));
  const nonDkSet = new Set(nonDk.map((r) => String(r.market || "").trim()));
  const out = [];
  for (const col of OU_PROJECTION_MARKETS) {
    const canon = ouPropsCanonicalMarket(col.market);
    if (dkSet.has(canon) || nonDkSet.has(canon)) out.push(col);
  }
  return out;
}

/**
 * Canonical `OU_STAT_MAP` key for O/U pricing (handles "Total Score", lowercase, minor aliases).
 * Unknown labels still fall back in `ouStatRec` to round score — avoid passing non-O/U markets here.
 */
function ouModelMarketKey(market) {
  const raw = String(market || "").trim();
  if (OU_STAT_MAP[raw]) return raw;
  const sl = raw.toLowerCase().replace(/\s+/g, " ");
  if (sl === "total score") return "Total score";
  if (sl === "birdies") return "Birdies";
  if (sl === "pars") return "Pars";
  if (sl === "bogeys" || sl.includes("bogey")) return "Bogeys";
  if (sl === "gir" || sl.includes("green")) return "GIR";
  if (sl.includes("fairway")) return "Fairways hit";
  if (sl.includes("putt")) return "Putts";
  return null;
}

/**
 * Damp in-round live-hole nudges on non-total props when `in_play_affects_round_odds` is false.
 * Prior-round course difficulty uses `meta.prior_round_course_stroke_shift` via combinedCourseDifficultyOUMuAdjustment.
 */
function liveCourseOuNonTotalScale() {
  return inPlayAffectsRoundOdds() ? 1 : 0.28;
}

const WEATHER_DEFAULTS = Object.freeze({
  tempF: 72,
  windMph: 8,
  humidityPct: 55,
  condition: "default",
});

const WEATHER_CONDITION_MEAN_DELTA = Object.freeze({
  default: 0,
  clear: 0,
  cloudy: 0.1,
  windy: 0.22,
  rain: 0.45,
  storm: 0.8,
});

const WEATHER_CONDITION_SIGMA_DELTA = Object.freeze({
  default: 0,
  clear: 0,
  cloudy: 0.02,
  windy: 0.05,
  rain: 0.09,
  storm: 0.14,
});

let WEATHER_STATE = { ...WEATHER_DEFAULTS };

/** Manual weather sliders removed — forecasts come from tee-time slices (see `refreshForecastWeatherFromOpenMeteo`). */
const WEATHER_UI_IDS = [];

/** Normalized course_used → lat/lon for hourly forecast at venue (extend as needed). */
const COURSE_COORDINATES_BY_NAME = {
  /** 2026 PGA Championship host (Newtown Square, PA) — venue forecast + tee-time weather. */
  "aronimink golf club": { lat: 39.991, lon: -75.308 },
  "quail hollow club": { lat: 35.1158, lon: -80.8529 },
  "augusta national golf club": { lat: 33.503, lon: -82.0199 },
  "the stadium course at tpc sawgrass": { lat: 30.198, lon: -81.394 },
  "tpc sawgrass": { lat: 30.198, lon: -81.394 },
  "tpc craig ranch": { lat: 33.1972, lon: -96.7314 },
  "oak hill country club": { lat: 43.1227, lon: -77.5229 },
  "torrey pines golf course": { lat: 32.8955, lon: -117.246 },
  "the oceans course at half moon bay golf links": { lat: 37.4636, lon: -122.449 },
  "pebble beach golf links": { lat: 36.5698, lon: -121.9506 },
  "harbour town golf links": { lat: 32.1392, lon: -80.8107 },
  "east lake golf club": { lat: 33.7437, lon: -84.349 },
  "wilmington country club": { lat: 39.7878, lon: -84.2108 },
  "castle pines golf club": { lat: 39.4189, lon: -104.894 },
  "detroit golf club": { lat: 42.4369, lon: -83.161 },
  "royal liverpool golf club": { lat: 53.3728, lon: -3.184 },
  "the riviera country club": { lat: 34.0497, lon: -118.501 },
  "colonial country club": { lat: 32.7248, lon: -97.434 },
  "muirfield village golf club": { lat: 40.1416, lon: -82.791 },
  "congressional country club": { lat: 39.0299, lon: -77.164 },
};

function normCourseKeyForForecast(name) {
  return String(name || "")
    .trim()
    .toLowerCase()
    .replace(/\s+/g, " ");
}

/** Cached Open-Meteo hourly payload; recomputed player snapshots when tee times / round change. */
let OPEN_METEO_FORECAST_CACHE = /** @type {{ key: string; atMs: number; hourly: object | null }} */ ({
  key: "",
  atMs: 0,
  hourly: null,
});
const OPEN_METEO_TTL_MS = 30 * 60 * 1000;

function courseCoordinatesFromMeta() {
  const key = normCourseKeyForForecast(DATA?.meta?.course_used || DATA?.course_used || "");
  return COURSE_COORDINATES_BY_NAME[key] || null;
}

function forecastTimezoneFromMeta() {
  const lab = String(DATA?.meta?.display_round_label || "");
  const m = lab.match(/America\/[A-Za-z_/]+/);
  if (m) return m[0];
  return "America/New_York";
}

function openMeteoForecastUrl(lat, lon, timezone) {
  const u = new URL("https://api.open-meteo.com/v1/forecast");
  u.searchParams.set("latitude", String(lat));
  u.searchParams.set("longitude", String(lon));
  u.searchParams.set(
    "hourly",
    "temperature_2m,relativehumidity_2m,precipitation_probability,windspeed_10m,weathercode",
  );
  u.searchParams.set("windspeed_unit", "mph");
  u.searchParams.set("temperature_unit", "fahrenheit");
  u.searchParams.set("forecast_days", "8");
  u.searchParams.set("timezone", timezone || "America/New_York");
  return u.href;
}

/** Date + clock from DataGolf tee strings (`YYYY-MM-DD HH:MM` or ISO `YYYY-MM-DDTHH:MM`). */
function parseDgTeetimeParts(teetimeStr) {
  const m = String(teetimeStr || "")
    .trim()
    .match(/^(\d{4}-\d{2}-\d{2})[ T](\d{1,2}):(\d{2})/);
  if (!m) return null;
  return { ymd: m[1], hh: parseInt(m[2], 10), mm: parseInt(m[3], 10) };
}

function teeHourFloorIsoFromDg(teetimeStr) {
  const p = parseDgTeetimeParts(teetimeStr);
  if (!p) return "";
  const hh = String(p.hh).padStart(2, "0");
  return `${p.ymd}T${hh}:00`;
}

function hourlyIndexForDgTeetime(timesArr, teetimeStr) {
  const floorIso = teeHourFloorIsoFromDg(teetimeStr);
  const p = parseDgTeetimeParts(teetimeStr);
  if (!floorIso || !p || !Array.isArray(timesArr) || !timesArr.length) return -1;
  for (let i = 0; i < timesArr.length; i++) {
    const t = String(timesArr[i] || "");
    if (t.length >= 16 && t.slice(0, 16) >= floorIso.slice(0, 16)) return i;
  }
  /* Tee after last model hour: use last hour ON THAT CALENDAR DAY — never snap all players to final array slot. */
  let lastSameDay = -1;
  for (let i = 0; i < timesArr.length; i++) {
    const t = String(timesArr[i] || "");
    if (t.slice(0, 10) !== p.ymd) continue;
    lastSameDay = i;
  }
  return lastSameDay;
}

function openMeteoConditionFromHourSlice(codeWorst, maxPrecipProb) {
  const p = num(maxPrecipProb, 0);
  const c = Math.round(num(codeWorst, NaN));
  const rainyCodes = [51, 53, 55, 56, 57, 61, 63, 65, 66, 67, 80, 81, 82];
  const stormCodes = [95, 96, 99];
  if (stormCodes.includes(c)) return "storm";
  if (p >= 55 || rainyCodes.includes(c)) return "rain";
  if (p >= 30 && (rainyCodes.includes(c) || c >= 51)) return "rain";
  if ([45, 48].includes(c)) return "cloudy";
  if (c <= 3 && p < 18) return "clear";
  if (c === 3) return "cloudy";
  return "cloudy";
}

function hourlySliceWeatherSnapshot(hourly, startIdx, spanHours) {
  const times = hourly?.time;
  const T = hourly?.temperature_2m;
  const W = hourly?.windspeed_10m;
  const H = hourly?.relativehumidity_2m;
  const P = hourly?.precipitation_probability;
  const C = hourly?.weathercode;
  if (!Array.isArray(times) || startIdx < 0 || startIdx >= times.length) return null;
  const end = Math.min(times.length, startIdx + spanHours);
  let nt = 0,
    sT = 0,
    sW = 0,
    sH = 0,
    sP = 0,
    worstCode = -999,
    maxPP = 0;
  for (let i = startIdx; i < end; i++) {
    const ti = num(T?.[i], NaN);
    if (!Number.isFinite(ti)) continue;
    sT += ti;
    sW += num(W?.[i], 0);
    sH += num(H?.[i], 0);
    sP += num(P?.[i], 0);
    const cc = num(C?.[i], NaN);
    if (Number.isFinite(cc) && cc > worstCode) worstCode = cc;
    const pp = num(P?.[i], 0);
    if (pp > maxPP) maxPP = pp;
    nt++;
  }
  if (!nt) return null;
  const cond =
    worstCode > -999 ? openMeteoConditionFromHourSlice(worstCode, maxPP) : openMeteoConditionFromHourSlice(NaN, maxPP);
  return {
    tempF: sT / nt,
    windMph: sW / nt,
    humidityPct: sH / nt,
    condition: cond,
  };
}

function medianFinite(vals) {
  const a = vals.filter((x) => Number.isFinite(x)).sort((x, y) => x - y);
  if (!a.length) return NaN;
  const mid = Math.floor(a.length / 2);
  return a.length % 2 ? a[mid] : (a[mid - 1] + a[mid]) / 2;
}

function medianWeatherSnapshotFromSamples(samples) {
  if (!samples.length) return null;
  const mt = medianFinite(samples.map((s) => s.tempF));
  const mw = medianFinite(samples.map((s) => s.windMph));
  const mh = medianFinite(samples.map((s) => s.humidityPct));
  if (!Number.isFinite(mt) || !Number.isFinite(mw) || !Number.isFinite(mh)) return null;
  const rank = { storm: 5, rain: 4, windy: 3, cloudy: 2, clear: 1, default: 0 };
  let bestC = "default";
  let br = -1;
  for (const s of samples) {
    const c = String(s.condition || "default").toLowerCase();
    const r = rank[c] ?? 0;
    if (r > br) {
      br = r;
      bestC = c;
    }
  }
  return {
    tempF: mt,
    windMph: mw,
    humidityPct: mh,
    condition: bestC,
  };
}

function weatherConditionDisplayLabel(condRaw) {
  const c = String(condRaw || "").trim().toLowerCase();
  if (!c || c === "default") return "neutral";
  return c;
}

/** Emoji for Open-Meteo-derived condition buckets (wave banner + accessible companion text stays in the stats line). */
function weatherConditionEmoji(condRaw) {
  const c = String(condRaw || "").trim().toLowerCase();
  if (c === "storm") return "⛈️";
  if (c === "rain") return "🌧️";
  if (c === "windy") return "💨";
  if (c === "cloudy") return "☁️";
  if (c === "clear") return "☀️";
  return "🌤️";
}

function escapeHtmlText(s) {
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

/** Stacked morning / afternoon rows for WEATHER toolbars (theme classes in CSS). */
function weatherWaveForecastBannerInnerHtml(morningSnap, afternoonSnap) {
  const bits = [];
  if (morningSnap) {
    const line = formatWeatherSnapshotCompact(morningSnap);
    if (line) {
      const em = weatherConditionEmoji(morningSnap.condition);
      bits.push(
        `<div class="weather-wave-line"><span class="weather-wave-emoji" aria-hidden="true">${em}</span><span class="weather-wave-copy"><strong class="weather-wave-kicker">Morning tees</strong><span class="weather-wave-sep"> · </span><span class="weather-wave-stats">${escapeHtmlText(line)}</span></span></div>`,
      );
    }
  }
  if (afternoonSnap) {
    const line = formatWeatherSnapshotCompact(afternoonSnap);
    if (line) {
      const em = weatherConditionEmoji(afternoonSnap.condition);
      bits.push(
        `<div class="weather-wave-line"><span class="weather-wave-emoji" aria-hidden="true">${em}</span><span class="weather-wave-copy"><strong class="weather-wave-kicker">Afternoon tees</strong><span class="weather-wave-sep"> · </span><span class="weather-wave-stats">${escapeHtmlText(line)}</span></span></div>`,
      );
    }
  }
  if (!bits.length) return "";
  return `<div class="weather-wave-banner-inner">${bits.join("")}</div>`;
}

/** One-line weather for banners and model-input panels (matches projection row snapshot). */
function formatWeatherSnapshotCompact(w) {
  if (!w || typeof w !== "object" || !Number.isFinite(w.tempF)) return "";
  if (!Number.isFinite(w.windMph) || !Number.isFinite(w.humidityPct)) return "";
  const lab = weatherConditionDisplayLabel(w.condition);
  return `${w.tempF.toFixed(1)}°F · ${w.windMph.toFixed(1)} mph · ${w.humidityPct.toFixed(0)}% · ${lab}`;
}

/** Pick a calendar day that exists in Open-Meteo hourly times (field date or majority tee date). */
function forecastAnchorDateYmd(hourly) {
  const times = hourly?.time;
  if (!Array.isArray(times) || !times.length) return "";
  const hasDay = (ymd) => times.some((t) => String(t || "").slice(0, 10) === ymd);

  const ds = String(DATA?.meta?.datagolf_field_date_start || "").match(/^(\d{4}-\d{2}-\d{2})/);
  if (ds && hasDay(ds[1])) return ds[1];

  const counts = new Map();
  for (const pl of DATA.players || []) {
    const tt = parseDgTeetimeParts(pl?.dg_teetime_local);
    if (!tt || !hasDay(tt.ymd)) continue;
    counts.set(tt.ymd, (counts.get(tt.ymd) || 0) + 1);
  }
  let best = "";
  let bestN = -1;
  for (const [ymd, n] of counts) {
    if (n > bestN) {
      bestN = n;
      best = ymd;
    }
  }
  if (best) return best;
  return String(times[0]).slice(0, 10);
}

function firstHourIndexOnDate(hourly, dateYmd) {
  const times = hourly?.time;
  if (!Array.isArray(times) || !dateYmd) return -1;
  for (let i = 0; i < times.length; i++) {
    if (String(times[i] || "").slice(0, 10) === dateYmd) return i;
  }
  return -1;
}

function hourlyIndexNearLocalHour(hourly, dateYmd, hour) {
  const times = hourly?.time;
  if (!Array.isArray(times) || !dateYmd) return -1;
  const want = `${dateYmd}T${String(Math.min(23, Math.max(0, hour))).padStart(2, "0")}`;
  let lastSameDay = -1;
  for (let i = 0; i < times.length; i++) {
    const t = String(times[i] || "");
    if (t.slice(0, 10) !== dateYmd) continue;
    lastSameDay = i;
    if (t.length >= 13 && t.slice(0, 13) >= want) return i;
  }
  return lastSameDay;
}

/**
 * Banner summary: fixed local morning (~8:00) vs afternoon (~15:00) on a day present in the hourly API —
 * avoids collapsing both waves to one slot when tee strings disagree with the timeline or index clamping.
 */
function computeMorningAfternoonForecastSnapshots(hourly, players) {
  void players;
  if (!hourly) return { morning: null, afternoon: null };
  const timesArr = hourly.time;
  if (!Array.isArray(timesArr) || !timesArr.length) return { morning: null, afternoon: null };

  const dateYmd = forecastAnchorDateYmd(hourly);
  if (!dateYmd) return { morning: null, afternoon: null };

  const dayStart = firstHourIndexOnDate(hourly, dateYmd);
  if (dayStart < 0) return { morning: null, afternoon: null };

  let ixM = hourlyIndexNearLocalHour(hourly, dateYmd, 8);
  let ixA = hourlyIndexNearLocalHour(hourly, dateYmd, 15);
  if (ixM < 0) ixM = dayStart;
  if (ixA < 0) ixA = hourlyIndexNearLocalHour(hourly, dateYmd, 14);
  if (ixA < 0) ixA = Math.min(timesArr.length - 5, dayStart + 7);

  const minGap = 5;
  if (ixA - ixM < minGap) ixA = Math.min(timesArr.length - 5, ixM + minGap);
  if (ixA <= ixM) ixA = Math.min(timesArr.length - 5, ixM + minGap);

  return {
    morning: hourlySliceWeatherSnapshot(hourly, ixM, 5),
    afternoon: hourlySliceWeatherSnapshot(hourly, ixA, 5),
  };
}

function buildForecastWaveSummaryString(morningSnap, afternoonSnap) {
  const m = morningSnap ? formatWeatherSnapshotCompact(morningSnap) : "";
  const a = afternoonSnap ? formatWeatherSnapshotCompact(afternoonSnap) : "";
  if (!m && !a) return "";
  const parts = [];
  if (m) parts.push(`Morning tees: ${m}.`);
  if (a) parts.push(`Afternoon tees: ${a}.`);
  return parts.join("\n");
}

function finalizeForecastWaveSummary(hourlyOrNull) {
  if (!DATA.meta) DATA.meta = {};
  if (hourlyOrNull && DATA.players?.length) {
    const { morning, afternoon } = computeMorningAfternoonForecastSnapshots(hourlyOrNull, DATA.players);
    DATA.meta.forecast_wave_slots = { morning, afternoon };
    DATA.meta.forecast_wave_summary = buildForecastWaveSummaryString(morning, afternoon);
  } else if (DATA.meta.forecast_wave_slots && typeof DATA.meta.forecast_wave_slots === "object") {
    const slots = DATA.meta.forecast_wave_slots;
    const morning = slots.morning ?? null;
    const afternoon = slots.afternoon ?? null;
    if (!String(DATA.meta.forecast_wave_summary || "").trim()) {
      DATA.meta.forecast_wave_summary = buildForecastWaveSummaryString(morning, afternoon);
    }
  } else {
    DATA.meta.forecast_wave_slots = { morning: null, afternoon: null };
    DATA.meta.forecast_wave_summary = "";
  }
  syncForecastWaveBannerTexts();
}

/** Trust server-baked Open-Meteo on projections.json (push:live / bake:weather) for several hours. */
const BAKED_FORECAST_WEATHER_MAX_AGE_MS = 6 * 60 * 60 * 1000;

function projectionsWeatherUsableFromBaked() {
  if (!DATA.meta || !DATA.players?.length) return false;
  const st = String(DATA.meta.forecast_weather_status || "");
  if (["open_meteo_fetch_failed", "empty_hourly", "no_course_coords", "no_players"].includes(st)) return false;
  const at = DATA.meta.forecast_weather_updated_at;
  if (!at) return false;
  const age = Date.now() - Date.parse(at);
  if (!Number.isFinite(age) || age < 0 || age > BAKED_FORECAST_WEATHER_MAX_AGE_MS) return false;
  return DATA.players.some((p) => {
    if (Number.isFinite(num(p.weather_temp_f, NaN))) return true;
    const auto = p.dg_auto_weather;
    return auto && typeof auto === "object" && Number.isFinite(num(auto.tempF, NaN));
  });
}

/** Sync baked weather_* columns ↔ dg_auto_weather after loading projections.json. */
function hydrateBakedWeatherFromPlayerFields() {
  if (!DATA.players?.length) return;
  for (const p of DATA.players) {
    const t = num(p.weather_temp_f, NaN);
    const w = num(p.weather_wind_mph, NaN);
    const h = num(p.weather_humidity, NaN);
    if (Number.isFinite(t) && Number.isFinite(w) && Number.isFinite(h)) {
      p.dg_auto_weather = {
        tempF: t,
        windMph: w,
        humidityPct: h,
        condition: String(p.weather_condition || p.dg_auto_weather?.condition || "default").toLowerCase(),
      };
      continue;
    }
    const auto = p.dg_auto_weather;
    if (auto && typeof auto === "object" && Number.isFinite(num(auto.tempF, NaN))) {
      p.weather_temp_f = Math.round(auto.tempF * 10) / 10;
      p.weather_wind_mph = Math.round(num(auto.windMph, 0) * 10) / 10;
      p.weather_humidity = Math.round(num(auto.humidityPct, 0));
      p.weather_condition = String(auto.condition || "default").toLowerCase();
    }
  }
}

function syncForecastWaveBannerTexts() {
  const fallback =
    "Morning and afternoon snapshots appear once the venue forecast loads.";
  const slots = DATA.meta?.forecast_wave_slots;
  const morning = slots && typeof slots === "object" ? slots.morning : null;
  const afternoon = slots && typeof slots === "object" ? slots.afternoon : null;
  const html = weatherWaveForecastBannerInnerHtml(morning, afternoon);
  const status = String(DATA.meta?.forecast_weather_status || "");
  const forecastLoaded =
    Boolean(DATA.meta?.forecast_weather_updated_at) &&
    !["open_meteo_fetch_failed", "empty_hourly", "no_course_coords", "no_players"].includes(status);
  for (const id of ["ou-weather-wave-summary", "ev-weather-wave-summary", "hh-weather-wave-summary"]) {
    const el = document.getElementById(id);
    if (!el) continue;
    el.hidden = false;
    if (html) {
      el.innerHTML = html;
      el.classList.add("weather-wave-banner");
    } else {
      el.classList.remove("weather-wave-banner");
      const raw = DATA.meta?.forecast_wave_summary;
      const text = typeof raw === "string" ? raw.trim() : "";
      if (text) {
        el.textContent = text;
      } else if (forecastLoaded) {
        el.textContent = "";
        el.hidden = true;
      } else {
        el.textContent = fallback;
      }
    }
  }
}

/**
 * DataGolf documents tee times in field-updates (bundled as live-in-play `field_updates`).
 * Hourly conditions come from Open-Meteo at the venue — DG website hourly tables are not in the public API feed.
 */
async function refreshForecastWeatherFromOpenMeteo() {
  if (typeof fetch !== "function") return false;
  if (projectionsWeatherUsableFromBaked()) {
    hydrateBakedWeatherFromPlayerFields();
    finalizeForecastWaveSummary(null);
    PRICING_MU_BONUS_CACHE.clear();
    return true;
  }
  const coords = courseCoordinatesFromMeta();
  const tz = forecastTimezoneFromMeta();
  if (!DATA.meta) DATA.meta = {};
  if (!coords || !DATA.players?.length) {
    for (const p of DATA.players || []) delete p.dg_auto_weather;
    DATA.meta.forecast_weather_status = coords ? "no_players" : "no_course_coords";
    finalizeForecastWaveSummary(null);
    return false;
  }
  const cacheKey = `${coords.lat}|${coords.lon}|${tz}|${DATA.meta.datagolf_field_date_start || ""}`;
  const now = Date.now();
  let hourly = OPEN_METEO_FORECAST_CACHE.hourly;
  if (
    OPEN_METEO_FORECAST_CACHE.key !== cacheKey ||
    now - OPEN_METEO_FORECAST_CACHE.atMs > OPEN_METEO_TTL_MS ||
    !hourly
  ) {
    try {
      const res = await fetch(openMeteoForecastUrl(coords.lat, coords.lon, tz));
      if (!res.ok) throw new Error(String(res.status));
      const j = await res.json();
      hourly = j.hourly;
      OPEN_METEO_FORECAST_CACHE = { key: cacheKey, atMs: now, hourly };
    } catch {
      DATA.meta.forecast_weather_status = "open_meteo_fetch_failed";
      finalizeForecastWaveSummary(null);
      return false;
    }
  }
  const timesArr = hourly?.time;
  if (!Array.isArray(timesArr) || !timesArr.length) {
    DATA.meta.forecast_weather_status = "empty_hourly";
    finalizeForecastWaveSummary(null);
    return false;
  }

  const perTeeSamples = [];
  for (const p of DATA.players) {
    const tt = p?.dg_teetime_local;
    if (!tt) continue;
    const ix = hourlyIndexForDgTeetime(timesArr, tt);
    if (ix < 0) continue;
    const snap = hourlySliceWeatherSnapshot(hourly, ix, 5);
    if (snap) perTeeSamples.push(snap);
  }
  const medianSnap = medianWeatherSnapshotFromSamples(perTeeSamples);

  for (const p of DATA.players) {
    const tt = p?.dg_teetime_local;
    let snap = null;
    if (tt) {
      const ix = hourlyIndexForDgTeetime(timesArr, tt);
      if (ix >= 0) snap = hourlySliceWeatherSnapshot(hourly, ix, 5);
    }
    if (!snap && medianSnap && Number.isFinite(medianSnap.tempF)) {
      snap = { ...medianSnap };
    }
    if (snap && Number.isFinite(snap.tempF) && Number.isFinite(snap.windMph) && Number.isFinite(snap.humidityPct)) {
      p.dg_auto_weather = snap;
    } else delete p.dg_auto_weather;
  }

  DATA.meta.forecast_weather_status = perTeeSamples.length ? "ok_tee_time" : medianSnap ? "ok_median" : "no_tee_match";
  DATA.meta.forecast_weather_updated_at = new Date().toISOString();
  finalizeForecastWaveSummary(hourly);
  PRICING_MU_BONUS_CACHE.clear();
  return true;
}

const PRICING_DEFAULTS = Object.freeze({ mode: "default", skill: "default" });
const PRICING_SKILL_COLUMNS = Object.freeze(["sg_total", "sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_t2g"]);
let PRICING_STATE = { ...PRICING_DEFAULTS };

const PRICING_UI_IDS = [
  { mode: "ou-pricing-mode", skill: "ou-pricing-skill", skillLabel: "ou-pricing-skill-label" },
  { mode: "ev-pricing-mode", skill: "ev-pricing-skill", skillLabel: "ev-pricing-skill-label" },
  { mode: "matchups-pricing-mode", skill: "matchups-pricing-skill", skillLabel: "matchups-pricing-skill-label" },
  { mode: "outrights-pricing-mode", skill: "outrights-pricing-skill", skillLabel: "outrights-pricing-skill-label" },
  { mode: "hh-pricing-mode", skill: "hh-pricing-skill", skillLabel: "hh-pricing-skill-label" },
];

function pricingFromUiIds(ids) {
  const modeEl = document.getElementById(ids.mode);
  const skillEl = document.getElementById(ids.skill);
  const rawM = String(modeEl?.value || PRICING_DEFAULTS.mode).toLowerCase();
  const mode = ["default", "recent", "course", "skill"].includes(rawM) ? rawM : "default";
  let skill = PRICING_DEFAULTS.skill;
  if (mode === "skill") {
    const rawS = String(skillEl?.value || "sg_total").toLowerCase();
    if (rawS === "default") skill = "sg_total";
    else skill = PRICING_SKILL_COLUMNS.includes(rawS) ? rawS : "sg_total";
  }
  return { mode, skill };
}

/** History column for skill-focus pricing; never "default". */
function pricingSkillHistoryKey() {
  const s = PRICING_STATE.skill;
  if (s && s !== "default" && PRICING_SKILL_COLUMNS.includes(s)) return s;
  return "sg_total";
}

function syncPricingUiFromState() {
  for (const ids of PRICING_UI_IDS) {
    const modeEl = document.getElementById(ids.mode);
    const skillEl = document.getElementById(ids.skill);
    if (modeEl) modeEl.value = PRICING_STATE.mode;
    if (skillEl) {
      skillEl.value = PRICING_STATE.skill;
      // Skill pillar only affects μ_SG nudge in "Skill focus"; recent/course use fixed SG history shapes.
      skillEl.disabled = PRICING_STATE.mode !== "skill";
    }
  }
  updatePricingSkillLabelsVisibility();
}

function updatePricingSkillLabelsVisibility() {
  const show = PRICING_STATE.mode === "skill";
  for (const ids of PRICING_UI_IDS) {
    const lab = document.getElementById(ids.skillLabel);
    if (lab) lab.hidden = !show;
  }
}

function refreshPricingAffectedViews() {
  const tab = activeAppTabId();
  if (tab === "ou") return void buildOuTable();
  if (tab === "ev") return void buildEvTable();
  if (tab === "matchups") return void buildMatchupsTable();
  if (tab === "matchup-analysis") return void buildMatchupAnalysisTool();
  if (tab === "outrights") return void buildOutrightsTable();
  if (tab === "props") {
    scheduleRenderPropsTrends(0);
    updatePropsFooterEv();
    return;
  }
  if (tab === "live-prop") return void renderLivePropPredictor();
  if (tab === "hangout") return void scheduleHangoutSimulateDebounced();
  if (tab === "course-fit") return void buildCourseFitTab();
  /* Unknown / very early tab state — refresh round projections only (avoid rebuilding every hidden panel). */
  buildOuTable();
}

function weatherScalarFromInput(raw, cur, lo, hi) {
  const s = String(raw ?? "").trim();
  if (s === "") return cur;
  const n = Number(s);
  if (!Number.isFinite(n)) return cur;
  return clamp(n, lo, hi);
}

function weatherFromUiIds(ids) {
  const tempEl = document.getElementById(ids.temp);
  const windEl = document.getElementById(ids.wind);
  const humEl = document.getElementById(ids.humidity);
  const condEl = document.getElementById(ids.condition);
  return {
    tempF: weatherScalarFromInput(tempEl?.value, WEATHER_STATE.tempF, 20, 120),
    windMph: weatherScalarFromInput(windEl?.value, WEATHER_STATE.windMph, 0, 60),
    humidityPct: weatherScalarFromInput(humEl?.value, WEATHER_STATE.humidityPct, 0, 100),
    condition: String(condEl?.value || WEATHER_DEFAULTS.condition).toLowerCase(),
  };
}

function syncWeatherUiFromState() {
  const ae = document.activeElement;
  for (const ids of WEATHER_UI_IDS) {
    const tempEl = document.getElementById(ids.temp);
    const windEl = document.getElementById(ids.wind);
    const humEl = document.getElementById(ids.humidity);
    const condEl = document.getElementById(ids.condition);
    if (tempEl && tempEl !== ae) tempEl.value = String(Math.round(WEATHER_STATE.tempF));
    if (windEl && windEl !== ae) windEl.value = String(Math.round(WEATHER_STATE.windMph));
    if (humEl && humEl !== ae) humEl.value = String(Math.round(WEATHER_STATE.humidityPct));
    if (condEl && condEl !== ae) condEl.value = WEATHER_STATE.condition;
  }
}

function weatherDifficultyDeltaFromSnapshot(w) {
  if (!w || typeof w !== "object") return 0;
  const tempAdj = w.tempF >= 72 ? 0.03 * (w.tempF - 72) : 0.02 * (w.tempF - 72);
  const windAdj = 0.045 * Math.max(0, w.windMph - 8);
  const humAdj = 0.012 * Math.max(0, w.humidityPct - 55);
  const sliderPart = tempAdj + windAdj + humAdj;
  if (w.condition === "default") return sliderPart;
  const condAdj = WEATHER_CONDITION_MEAN_DELTA[w.condition] ?? 0;
  return sliderPart + condAdj;
}

function weatherDifficultyDelta() {
  return weatherDifficultyDeltaFromSnapshot(WEATHER_STATE);
}

function weatherSigmaMultiplierFromSnapshot(w) {
  if (!w || typeof w !== "object") return 1;
  const windVar = 0.01 * Math.max(0, w.windMph - 8);
  const humVar = 0.0015 * Math.max(0, w.humidityPct - 55);
  if (w.condition === "default") {
    return clamp(1 + windVar + humVar, 0.9, 1.5);
  }
  const condVar = WEATHER_CONDITION_SIGMA_DELTA[w.condition] ?? 0;
  return clamp(1 + windVar + humVar + condVar, 0.9, 1.5);
}

function weatherSigmaMultiplier() {
  return weatherSigmaMultiplierFromSnapshot(WEATHER_STATE);
}

/** Tee-time forecast (bundled live field_updates + Open-Meteo hourly) when present; else global fallback. */
function effectiveWeatherForProjectionRow(row) {
  const auto = row?.dg_auto_weather;
  if (
    auto &&
    typeof auto === "object" &&
    Number.isFinite(auto.tempF) &&
    Number.isFinite(auto.windMph) &&
    Number.isFinite(auto.humidityPct)
  ) {
    return {
      tempF: auto.tempF,
      windMph: auto.windMph,
      humidityPct: auto.humidityPct,
      condition: String(auto.condition || "default").toLowerCase(),
    };
  }
  return { ...WEATHER_STATE };
}

/** Weather line for expanded projection “Model inputs” (every player gets a readable snapshot). */
function formatEffectiveWeatherLine(row) {
  return formatWeatherSnapshotCompact(effectiveWeatherForProjectionRow(row)) || "—";
}

function statWeatherMuAdjustment(market, row) {
  const d = weatherDifficultyDeltaFromSnapshot(effectiveWeatherForProjectionRow(row));
  if (!Number.isFinite(d)) return 0;
  if (market === "Total score") return d;
  if (market === "Bogeys") return 0.45 * d;
  if (market === "Birdies") return -0.5 * d;
  if (market === "Putts") return 0.35 * d;
  if (market === "GIR") return -0.22 * d;
  if (market === "Fairways hit") return -0.14 * d;
  return 0;
}

/**
 * Maps DataGolf live-hole-stats round excess (strokes vs par for the field) into a stroke-unit shift `d`
 * for O/U / +EV.
 * Keep "course playing hard" responsive, but damp "course playing easy" so totals do not get unrealistically low.
 */
const LIVE_COURSE_EXCESS_TO_STROKE_K_HARD = 1.5;
const LIVE_COURSE_EXCESS_TO_STROKE_K_EASY = 0.8;
const LIVE_COURSE_D_CLAMP_NEG = -1.6;
const LIVE_COURSE_D_CLAMP_POS = 3.4;
/** Prior completed rounds at this event (fetch-datagolf / course-round-adjustments.mjs defaults). */
const PRIOR_ROUND_EXCESS_TO_STROKE_K_HARD = 1.5;
const PRIOR_ROUND_EXCESS_TO_STROKE_K_EASY = 0.8;
const PRIOR_ROUND_STROKE_SHIFT_CLAMP_NEG = -1.2;
const PRIOR_ROUND_STROKE_SHIFT_CLAMP_POS = 2.15;

/** True only after tournament play has actually started (at least one player has begun the live round). */
function hasStartedLiveRoundData() {
  if (!inPlayAffectsRoundOdds()) return false;
  const liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  if (!Number.isFinite(liveR) || liveR < 1 || liveR > 4) return false;
  const rows = Array.isArray(DATA?.live_in_play_snapshot)
    ? DATA.live_in_play_snapshot
    : Array.isArray(DATA?.data)
      ? DATA.data
      : [];
  for (const r of rows) {
    const rr = Math.round(num(r?.round, NaN));
    if (rr !== liveR) continue;
    const thru = Math.round(num(r?.thru ?? r?.Thru, NaN));
    if (Number.isFinite(thru) && thru >= 1) return true;
  }
  return false;
}

function liveCourseDifficultyDForMu() {
  if (hasStartedLiveRoundData()) {
    const exR = num(DATA?.meta?.live_course_round_excess_strokes, NaN);
    if (Number.isFinite(exR)) {
      const k = exR < 0 ? LIVE_COURSE_EXCESS_TO_STROKE_K_EASY : LIVE_COURSE_EXCESS_TO_STROKE_K_HARD;
      return clamp(exR * k, LIVE_COURSE_D_CLAMP_NEG, LIVE_COURSE_D_CLAMP_POS);
    }
  }
  const d0 = courseTableStaticDifficultyD();
  if (!Number.isFinite(d0) || d0 === 0) return 0;
  return clamp(d0 * 0.42, -0.85, 1.45);
}

/** Stroke shift from prior-round field excess (+ = harder vs par). Mirrors course-round-adjustments.mjs. */
function courseDifficultyStrokeShiftFromExcess(excessStrokes) {
  const exR = num(excessStrokes, NaN);
  if (!Number.isFinite(exR) || exR === 0) return 0;
  const k = exR < 0 ? PRIOR_ROUND_EXCESS_TO_STROKE_K_EASY : PRIOR_ROUND_EXCESS_TO_STROKE_K_HARD;
  return clamp(exR * k, PRIOR_ROUND_STROKE_SHIFT_CLAMP_NEG, PRIOR_ROUND_STROKE_SHIFT_CLAMP_POS);
}

/** Refresh meta prior-round course difficulty from live hole stats (blend with fetch/historical when present). */
function refreshPriorRoundCourseMetaFromLiveHoleStats(lh) {
  if (!lh || typeof lh !== "object") return false;
  if (!DATA.meta) DATA.meta = {};
  const prevEx = DATA.meta.prior_round_course_excess_strokes;
  const prevSh = DATA.meta.prior_round_course_stroke_shift;
  const nextEx =
    prevEx && typeof prevEx === "object" ? { ...prevEx } : { 1: 0, 2: 0, 3: 0, 4: 0 };
  const nextSh =
    prevSh && typeof prevSh === "object" ? { ...prevSh } : { 1: 0, 2: 0, 3: 0, 4: 0 };
  let touched = false;
  for (let tr = 2; tr <= 4; tr++) {
    const liveEx = priorRoundsMeanExcessFromLiveHoleStats(lh, tr);
    if (!Number.isFinite(liveEx)) continue;
    const histEx = num(nextEx[String(tr)] ?? nextEx[tr], NaN);
    const ex = Number.isFinite(histEx) && histEx !== 0 ? 0.55 * liveEx + 0.45 * histEx : liveEx;
    const sh = courseDifficultyStrokeShiftFromExcess(ex);
    const exR = Math.round(ex * 1000) / 1000;
    const shR = Math.round(sh * 1000) / 1000;
    if (nextEx[tr] !== exR) touched = true;
    if (nextSh[tr] !== shR) touched = true;
    nextEx[tr] = exR;
    nextSh[tr] = shR;
  }
  if (touched) {
    DATA.meta.prior_round_course_excess_strokes = nextEx;
    DATA.meta.prior_round_course_stroke_shift = nextSh;
  }
  return touched;
}

function priorRoundCourseStrokeShiftBakedOnRow(row) {
  return Object.prototype.hasOwnProperty.call(row || {}, "prior_round_course_stroke_shift");
}

/** Prior-round stroke shift for this projection row (0 when fetch:dg already baked into μ / totals). */
function priorRoundCourseStrokeShiftForProjectionRow(row) {
  if (priorRoundCourseStrokeShiftBakedOnRow(row)) return 0;
  const rnd = Math.round(num(row?.round, NaN));
  if (!Number.isFinite(rnd) || rnd < 2) return 0;
  const pack = DATA?.meta?.prior_round_course_stroke_shift;
  if (!pack || typeof pack !== "object") return 0;
  const v = num(pack[String(rnd)] ?? pack[rnd], NaN);
  return Number.isFinite(v) ? v : 0;
}

/** Map stroke-unit difficulty `d` into O/U counting-stat means (+d ⇒ harder ⇒ higher totals / bogeys). */
function ouMuAdjustmentFromCourseDifficultyD(market, d) {
  if (!Number.isFinite(d) || d === 0) return 0;
  const mKey = ouModelMarketKey(market) || "Total score";
  const scale = mKey === "Total score" ? 1 : liveCourseOuNonTotalScale();
  if (mKey === "Total score") return d;
  if (mKey === "Bogeys") return 0.48 * d * scale;
  if (mKey === "Birdies") return -0.55 * d * scale;
  if (mKey === "Pars") return -0.11 * d * scale;
  if (mKey === "Putts") return 0.42 * d * scale;
  if (mKey === "GIR") return -0.28 * d * scale;
  if (mKey === "Fairways hit") return -0.2 * d * scale;
  return 0;
}

function priorRoundCourseOUMuAdjustment(market, row) {
  return ouMuAdjustmentFromCourseDifficultyD(market, priorRoundCourseStrokeShiftForProjectionRow(row));
}

/** In-round live hole-stats difficulty for the active live round only. */
function liveCourseOUMuAdjustmentForRound(market, targetRound) {
  const tr = Math.round(num(targetRound, NaN));
  const liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  if (!Number.isFinite(tr) || !Number.isFinite(liveR) || tr !== liveR) return 0;
  return ouMuAdjustmentFromCourseDifficultyD(market, liveCourseDifficultyDForMu());
}

/**
 * Prior rounds at this venue + in-round live difficulty for `row.round`.
 * Used by Round projections, +EV props, Historical Trends model line, Live Prop, etc.
 */
function combinedCourseDifficultyOUMuAdjustment(market, row) {
  if (!row || typeof row !== "object") return 0;
  const tr = Math.round(num(row.round, NaN));
  return (
    priorRoundCourseOUMuAdjustment(market, row) +
    (Number.isFinite(tr) ? liveCourseOUMuAdjustmentForRound(market, tr) : 0)
  );
}

/** @deprecated Use combinedCourseDifficultyOUMuAdjustment with a projection row when possible. */
function liveCourseOUMuAdjustment(market) {
  return ouMuAdjustmentFromCourseDifficultyD(market, liveCourseDifficultyDForMu());
}

function liveCoursePropHistoryNudge(statKey, projectionRowOpt) {
  const market = ouMarketKeyFromStatKey(statKey);
  if (projectionRowOpt && typeof projectionRowOpt === "object") {
    return combinedCourseDifficultyOUMuAdjustment(market, projectionRowOpt);
  }
  return liveCourseOUMuAdjustment(market);
}

/** μ_SG delta from prior-round course hardness when projections were not rebuilt by fetch:dg. */
function priorRoundCourseMuSgDelta(row) {
  if (!row || priorRoundCourseStrokeShiftBakedOnRow(row)) return 0;
  const shift = priorRoundCourseStrokeShiftForProjectionRow(row);
  const form = num(row?.within_event_form_shift, NaN);
  const formAdj = Number.isFinite(form) ? form : 0;
  return -shift + formAdj;
}

/** Sum par for holes 1..n (n = holes completed, e.g. thru=14 → first 14 holes). */
function courseParSumFirstNHoles(holePars, nHolesCompleted) {
  const n = Math.min(18, Math.max(0, Math.floor(num(nHolesCompleted, NaN))));
  if (!n) return 0;
  if (!Array.isArray(holePars) || holePars.length < n) return NaN;
  let s = 0;
  for (let i = 0; i < n; i++) {
    const p = num(holePars[i], NaN);
    if (!Number.isFinite(p)) return NaN;
    s += p;
  }
  return s;
}

/**
 * Mid-round: blend pre-round `total_score` with realized strokes + prorated remainder.
 * Uses preds/in-play `today` (vs par through `thru`) on the row whose `round` matches live DG round.
 */
function liveCurrentRoundTotalScoreMuDelta(row) {
  if (!inPlayAffectsRoundOdds()) return 0;
  const liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  const pr = Math.round(num(row?.round, NaN));
  if (!Number.isFinite(liveR) || liveR < 1 || liveR > 4 || pr !== liveR) return 0;
  const thru = Math.round(num(row.dg_live_thru, NaN));
  const today = num(row.dg_live_today, NaN);
  const baseMu = num(row.total_score, NaN);
  const par18 = num(DATA?.meta?.course_par_18, NaN);
  const holePars = DATA.meta?.hole_pars;
  if (!Number.isFinite(baseMu) || !Number.isFinite(par18)) return 0;
  if (!Number.isFinite(today)) return 0;

  if (Number.isFinite(thru) && thru >= 18) {
    const finalStrokes = par18 + today;
    return clamp(finalStrokes - baseMu, -14, 14);
  }
  if (!Number.isFinite(thru) || thru < 1) return 0;

  let parThru = courseParSumFirstNHoles(holePars, thru);
  if (!Number.isFinite(parThru)) parThru = (par18 / 18) * thru;
  const parRem = par18 - parThru;
  const rem = 18 - thru;
  if (rem <= 0) return 0;
  const expExcessRem = ((baseMu - par18) * rem) / 18;
  const actualStrokes = parThru + today;
  const muLive = actualStrokes + parRem + expExcessRem;
  return clamp(muLive - baseMu, -12, 12);
}

function liveRowMatchesDgLiveRound(row) {
  const liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  const pr = Math.round(num(row?.round, NaN));
  return Number.isFinite(liveR) && liveR >= 1 && liveR <= 4 && pr === liveR;
}

/** When DG omits hole counts, infer minimum plausible birdies / bogeys from strokes vs par through `thru`. */
function inferBirdiesSoFarFromTodayVsPar(today, thru) {
  if (!Number.isFinite(today) || !Number.isFinite(thru) || thru < 1) return NaN;
  if (today >= 0) return 0;
  const under = -today;
  return Math.min(thru, Math.max(0, Math.round(under * 0.52)));
}

function inferBogeysSoFarFromTodayVsPar(today, thru) {
  if (!Number.isFinite(today) || !Number.isFinite(thru) || thru < 1) return NaN;
  if (today <= 0) return 0;
  return Math.min(thru, Math.max(0, Math.round(today * 0.52)));
}

/**
 * Birdies / pars / bogeys O/U: expected full-round mean becomes (count so far) + (proj rate × holes left).
 * Uses merged dg_live_*_so_far when present; else infers birdies/bogeys from `today` vs par and pars as residual holes.
 * Tightens sigma by √(holes_left / 18) (and near lock when round complete).
 */
function livePartialRoundCountPropAdjust(market, row) {
  const out = { muDelta: 0, sigmaScale: 1 };
  if (!inPlayAffectsRoundOdds()) return out;
  if (market !== "Birdies" && market !== "Pars" && market !== "Bogeys") return out;
  if (!liveRowMatchesDgLiveRound(row)) return out;
  const thru = Math.round(num(row.dg_live_thru, NaN));
  const today = num(row.dg_live_today, NaN);
  if (!Number.isFinite(thru) || thru < 1) return out;
  const rem = 18 - thru;
  if (rem < 0) return out;

  const field = market === "Birdies" ? "birdies" : market === "Pars" ? "pars" : "bogeys";
  const muFull = num(row[field], NaN);
  if (!Number.isFinite(muFull) || muFull < 0) return out;

  let b = num(row.dg_live_birdies_so_far, NaN);
  let bg = num(row.dg_live_bogeys_so_far, NaN);
  if (!Number.isFinite(b)) b = inferBirdiesSoFarFromTodayVsPar(today, thru);
  if (!Number.isFinite(bg)) bg = inferBogeysSoFarFromTodayVsPar(today, thru);
  if (!Number.isFinite(b)) b = 0;
  if (!Number.isFinite(bg)) bg = 0;

  const eg = num(row.dg_live_eagles_so_far, NaN);
  const eagles = Number.isFinite(eg) && eg >= 0 ? Math.min(thru, Math.round(eg)) : 0;

  let pSo = num(row.dg_live_pars_so_far, NaN);
  if (!Number.isFinite(pSo)) {
    pSo = Math.max(0, thru - b - bg - eagles);
    pSo = Math.min(thru, pSo);
  }

  const rate = muFull / 18;
  let soFar;
  if (market === "Birdies") soFar = b;
  else if (market === "Bogeys") soFar = bg;
  else soFar = pSo;

  let muLive = soFar + rate * rem;
  muLive = clamp(muLive, 0, 18);

  out.muDelta = muLive - muFull;
  if (thru >= 18) out.sigmaScale = 0.26;
  else out.sigmaScale = clamp(Math.sqrt(rem / 18), 0.17, 1);
  return out;
}

/**
 * Matchups / 3-ball +EV use `mu_sg`; map in-round total-score revision to the same stroke↔SG scale
 * as bundled demo rows (mu_sg ≈ (par − total_score) × 0.2 ⇒ Δsg ≈ −0.2 × Δstrokes).
 */
function liveCurrentRoundMuSgDelta(row) {
  const d = liveCurrentRoundTotalScoreMuDelta(row);
  if (!Number.isFinite(d) || d === 0) return 0;
  return clamp(-0.2 * d, -1.45, 1.45);
}

/** Tighten Total score O/U sigma as the live round progresses (less variance left to play). */
function sigmaLiveRoundShrinkForTotalScore(row, rec) {
  if (!inPlayAffectsRoundOdds()) return 1;
  if (!rec || rec.field !== "total_score") return 1;
  const liveR = Math.round(num(DATA?.meta?.datagolf_live_current_round, NaN));
  const pr = Math.round(num(row?.round, NaN));
  if (!Number.isFinite(liveR) || pr !== liveR) return 1;
  const thru = Math.round(num(row.dg_live_thru, NaN));
  if (!Number.isFinite(thru) || thru < 1) return 1;
  if (thru >= 18) return 0.32;
  return clamp(Math.sqrt((18 - thru) / 18), 0.2, 1);
}

function playerSkillWeatherEdge(row) {
  const baseSg = modeledMuSgFromRow(row);
  const roundSd = num(row?.round_sd, NaN);
  const sgEdge = Number.isFinite(baseSg) ? baseSg * 0.12 : 0;
  const consistencyEdge = Number.isFinite(roundSd) ? clamp((2.8 - roundSd) * 0.03, -0.06, 0.06) : 0;
  return weatherDifficultyDeltaFromSnapshot(effectiveWeatherForProjectionRow(row)) * (sgEdge + consistencyEdge);
}

function weatherAdjustedMuSg(row) {
  const base = modeledMuSgFromRow(row);
  if (!Number.isFinite(base)) return NaN;
  return base + playerSkillWeatherEdge(row);
}

function ouStatRec(market) {
  const key = ouModelMarketKey(market);
  if (key && OU_STAT_MAP[key]) return OU_STAT_MAP[key];
  return OU_STAT_MAP["Total score"];
}

/** Maps Historical Trends statKey / prop-stat value → `OU_STAT_MAP` / `getOuMarket()` label. */
function ouMarketKeyFromStatKey(statKey) {
  if (statKey === "total") return "Total score";
  if (statKey === "birdies") return "Birdies";
  if (statKey === "pars") return "Pars";
  if (statKey === "bogeys") return "Bogeys";
  if (statKey === "gir") return "GIR";
  if (statKey === "fairways") return "Fairways hit";
  if (statKey === "putts") return "Putts";
  return "Total score";
}

/** Per-round SD for counting O/U: GIR/FW use binomial-ish spread; others use sqrt-mean with floors. */
function sigmaOuDiscreteCounting(market, muAbs) {
  const m = num(muAbs, NaN);
  if (!Number.isFinite(m) || m <= 0) return 2.4;
  if (market === "GIR") {
    const n = 18;
    const p = clamp(m / n, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(n * p * (1 - p)));
  }
  if (market === "Fairways hit") {
    const n = fairwayHolesModeledFromData();
    if (!Number.isFinite(n) || n <= 0) return 2.4;
    const p0 = m / n;
    const p = Number.isFinite(p0) ? Math.min(1 - 1e-12, Math.max(1e-12, p0)) : 0.5;
    return Math.sqrt(n * p * (1 - p));
  }
  if (market === "Putts") {
    return clamp(Math.sqrt(m * 1.15), 2.35, 5.85);
  }
  if (market === "Birdies" || market === "Bogeys") {
    return clamp(Math.sqrt(m * 1.08), 1.05, 3.15);
  }
  if (market === "Pars") {
    return clamp(Math.sqrt(m * 1.06), 1.15, 3.35);
  }
  return Math.max(0.55, Math.sqrt(Math.max(m, 0.2)) * 0.9);
}

function sigmaForOu(market, row) {
  const mKey = ouModelMarketKey(market) || "Total score";
  const rec = ouStatRec(mKey);
  const weatherMult = weatherSigmaMultiplierFromSnapshot(effectiveWeatherForProjectionRow(row));
  const liveShrink = sigmaLiveRoundShrinkForTotalScore(row, rec);
  if (rec.sdKey) {
    const s = num(row[rec.sdKey], NaN);
    if (Number.isFinite(s) && s > 0.05) return s * weatherMult * liveShrink;
    return 2.75 * weatherMult * liveShrink;
  }
  const muFull = ouMeanCountingStat(mKey, row);
  const muFallback = ouFallbackScalarForProjectedMean(mKey, row, rec);
  const muAbs = Number.isFinite(muFull) && muFull > 0 ? Math.abs(muFull) : Math.abs(num(muFallback, NaN));
  if (!Number.isFinite(muAbs) || muAbs <= 0) return 2.75 * weatherMult * liveShrink;
  return sigmaOuDiscreteCounting(mKey, muAbs) * weatherMult * liveShrink;
}

/** Model-projected mean μ for one market / player / round (weather, live course, partial live round, pricing). */
function ouProjectedMean(market, row) {
  const mKey = ouModelMarketKey(market) || "Total score";
  const rec = ouStatRec(mKey);
  const dgId = Math.round(num(row?.dg_id, NaN));
  const liveRoundAdj = mKey === "Total score" ? liveCurrentRoundTotalScoreMuDelta(row) : 0;
  const countLive = livePartialRoundCountPropAdjust(mKey, row);
  const baseMean = ouMeanCountingStat(mKey, row);
  const baseScalar = Number.isFinite(baseMean) ? baseMean : ouFallbackScalarForProjectedMean(mKey, row, rec);
  return (
    baseScalar +
    statWeatherMuAdjustment(mKey, row) +
    combinedCourseDifficultyOUMuAdjustment(mKey, row) +
    liveRoundAdj +
    countLive.muDelta +
    pricingStatMuAdjustment(mKey, dgId)
  );
}

function modelProbOverMarket(market, row, line) {
  const mKey = ouModelMarketKey(market) || "Total score";
  const mu = ouProjectedMean(market, row);
  if (!Number.isFinite(mu)) return NaN;
  const countLive = livePartialRoundCountPropAdjust(mKey, row);
  let sig = sigmaForOu(mKey, row) * countLive.sigmaScale;
  if (!Number.isFinite(sig) || sig < 0.06) sig = sigmaForOu(mKey, row);
  const z = (line - mu) / sig;
  return 1 - normalCdf(z);
}

function getOuViewMode() {
  const priceOn = document.getElementById("ou-mode-price")?.classList.contains("active");
  return priceOn ? "price" : "prob";
}

function setOuViewMode(mode) {
  const prob = document.getElementById("ou-mode-prob");
  const price = document.getElementById("ou-mode-price");
  if (!prob || !price) return;
  const isPrice = mode === "price";
  prob.classList.toggle("active", !isPrice);
  price.classList.toggle("active", isPrice);
  prob.setAttribute("aria-selected", (!isPrice).toString());
  price.setAttribute("aria-selected", isPrice.toString());
}

function getOuMarket() {
  const el = document.getElementById("ou-market-filter");
  const v = el && el.value ? el.value : "Total score";
  return v === "Putts" ? "Total score" : v;
}

/** Round score & bogeys: chart shows P(over) (falls as line rises). Birdies/pars: P(under) (rises as line rises). */
function ouMarketLowerIsBetter(market) {
  return market === "Total score" || market === "Bogeys" || market === "Putts";
}

function enforceHalfLine(v) {
  if (!Number.isFinite(v)) return NaN;
  return Math.round(v - 0.5) + 0.5;
}

/** R export / CSV uses "Total Score"; O/U UI uses "Total score". */
function ouPropsCanonicalMarket(market) {
  const m = String(market || "");
  if (m === "Total score") return "Total Score";
  return m;
}

function ouPropPlayerKeyRaw(name) {
  return String(name || "").trim().toLowerCase();
}

function ouPropPlayerKeyDisplay(name) {
  return String(displayGolferName(name || ""))
    .trim()
    .toLowerCase();
}

/** Map id:${dgId}:${line} / nm:${name}:${line} → { over, under } American odds. */
function ouBuildPropsOddsIndex(market) {
  const canon = ouPropsCanonicalMarket(market);
  const map = new Map();
  const props = ouRoundOuPropsForLines();
  for (const r of props) {
    if (String(r.market || "").trim() !== canon) continue;
    const L = enforceHalfLine(num(r.line, NaN));
    if (!Number.isFinite(L)) continue;
    const o = num(r.over_odds, NaN);
    const u = num(r.under_odds, NaN);
    if (!Number.isFinite(o) || !Number.isFinite(u)) continue;
    const id = Math.round(num(r.dg_id, NaN));
    if (Number.isFinite(id) && id > 0) map.set(`id:${id}:${L}`, { over: o, under: u });
    const raw = String(r.player_name || "").trim();
    if (raw) {
      map.set(`nm:${ouPropPlayerKeyRaw(raw)}:${L}`, { over: o, under: u });
      map.set(`nm:${ouPropPlayerKeyDisplay(raw)}:${L}`, { over: o, under: u });
    }
  }
  return map;
}

function ouPropsBookOddsFromIndex(idx, playerRow, line) {
  const L = enforceHalfLine(line);
  if (!Number.isFinite(L) || !idx) return null;
  const id = Math.round(num(playerRow?.dg_id, NaN));
  if (Number.isFinite(id) && id > 0) {
    const byId = idx.get(`id:${id}:${L}`);
    if (byId) return byId;
  }
  const nm = String(playerRow?.player_name || "").trim();
  if (!nm) return null;
  let hit = idx.get(`nm:${ouPropPlayerKeyRaw(nm)}:${L}`);
  if (hit) return hit;
  hit = idx.get(`nm:${ouPropPlayerKeyDisplay(nm)}:${L}`);
  return hit || null;
}

function ouPropsRowsForMarketPlayer(market, playerRow, opts = {}) {
  const canon = ouPropsCanonicalMarket(market);
  const props = ouRoundOuPropsForLines();
  const out = [];
  const wantId = Math.round(num(playerRow?.dg_id, NaN));
  const wantRaw = ouPropPlayerKeyRaw(playerRow?.player_name || "");
  const wantDisp = ouPropPlayerKeyDisplay(playerRow?.player_name || "");
  for (const r of props) {
    if (String(r.market || "").trim() !== canon) continue;
    const L = enforceHalfLine(num(r.line, NaN));
    const o = num(r.over_odds, NaN);
    const u = num(r.under_odds, NaN);
    if (!Number.isFinite(L) || !Number.isFinite(o) || !Number.isFinite(u)) continue;
    const rid = Math.round(num(r.dg_id, NaN));
    const rRaw = ouPropPlayerKeyRaw(r.player_name || "");
    const rDisp = ouPropPlayerKeyDisplay(r.player_name || "");
    const sameById = Number.isFinite(wantId) && wantId > 0 && rid === wantId;
    const sameByName = (wantRaw && rRaw && wantRaw === rRaw) || (wantDisp && rDisp && wantDisp === rDisp);
    if (!sameById && !sameByName) continue;
    out.push({ line: L, over: o, under: u, source: String(r.source || "").trim().toLowerCase() });
  }
  const dkOnly = out.filter((r) => r.source === "draftkings");
  if (opts.dkOnly) return dkOnly;
  if (dkOnly.length) return dkOnly;
  return out;
}

function chooseOuPropLineForProjection(market, playerRow, mu, opts = {}) {
  const rows = ouPropsRowsForMarketPlayer(market, playerRow, opts);
  if (!rows.length) return null;
  let best = rows[0];
  let bestDist = Number.POSITIVE_INFINITY;
  for (const r of rows) {
    const d = Number.isFinite(mu) ? Math.abs(r.line - mu) : 0;
    if (d < bestDist) {
      best = r;
      bestDist = d;
    }
  }
  return best;
}

function ouBookImpliedForSortColumn(playerRow, market, L, lineSel, pImpOverSel, pImpUnderSel, propIdx, side) {
  const useCustom = lineMatchesOuHighlight(lineSel, L, market);
  if (useCustom) return side === "over" ? pImpOverSel : pImpUnderSel;
  const pk = ouPropsBookOddsFromIndex(propIdx, playerRow, L);
  if (pk && Number.isFinite(pk.over) && Number.isFinite(pk.under)) {
    return side === "over" ? impliedProbFromAmerican(pk.over) : impliedProbFromAmerican(pk.under);
  }
  return impliedProbFromAmerican(OU_DEFAULT_ODDS_AM);
}

function formatAmericanOddsInput(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return "";
  return v > 0 ? `+${v}` : String(v);
}

/** When one golfer is selected and props include their line, mirror DK over/under into the toolbar. */
function syncOuToolbarOddsFromProps(market, lineSel, round) {
  const oEl = document.getElementById("ou-odds-over-filter");
  const uEl = document.getElementById("ou-odds-under-filter");
  const pf = document.getElementById("ou-player-filter");
  if (!oEl || !uEl || !pf || !(pf instanceof HTMLInputElement)) return;
  if (document.activeElement === oEl || document.activeElement === uEl) return;
  const r = Math.round(num(round, NaN));
  const allRows = ouSortedPlayerRowsProjection(r);
  const want = ouResolveSinglePlayerNameForToolbar(allRows, String(pf.value || "").trim());
  if (!want) return;
  const row = DATA.players.find((p) => String(p.player_name || "").trim() === want && samePlayerRound(p, r));
  if (!row) return;
  const idx = ouBuildPropsOddsIndex(market);
  const L = enforceHalfLine(lineSel);
  if (!Number.isFinite(L)) return;
  const pk = ouPropsBookOddsFromIndex(idx, row, L);
  if (!pk || !Number.isFinite(pk.over) || !Number.isFinite(pk.under)) return;
  oEl.value = formatAmericanOddsInput(pk.over);
  uEl.value = formatAmericanOddsInput(pk.under);
}

function parseOuLineFilterInput() {
  const el = document.getElementById("ou-line-filter");
  if (!el) return NaN;
  const s = String(el.value ?? "").trim();
  if (!s || s === "-" || s === "+" || s === "." || s === "-." || s === "+.") return NaN;
  const raw = num(s, NaN);
  if (!Number.isFinite(raw)) return NaN;
  return enforceHalfLine(raw);
}

/** Highlight / odds column: parsed line if valid, else last committed (typing must not snap every keystroke). */
function lineSelForOuTable() {
  const v = parseOuLineFilterInput();
  return Number.isFinite(v) ? v : ouLineCommitted;
}

function commitOuLineFilterValue() {
  const el = document.getElementById("ou-line-filter");
  if (!el) return;
  let v = parseOuLineFilterInput();
  if (!Number.isFinite(v)) v = ouLineCommitted;
  else ouLineCommitted = v;
  el.value = ouLineCommitted.toFixed(1);
}

function selectedOuOddsById(inputId, normalizeInput = false) {
  const el = document.getElementById(inputId);
  const raw = String(el?.value || "").trim().replace(/\s+/g, "");
  const m = raw.match(/^([+-]?)(\d+)$/);
  let v = m ? parseInt(`${m[1] || ""}${m[2]}`, 10) : OU_DEFAULT_ODDS_AM;
  if (!Number.isFinite(v) || v === 0) v = OU_DEFAULT_ODDS_AM;
  if (el && normalizeInput) el.value = v > 0 ? `+${v}` : String(v);
  return v;
}

function formatEdgePct(edge) {
  if (!Number.isFinite(edge)) return "—";
  const p = edge * 100;
  return `${p >= 0 ? "+" : ""}${p.toFixed(1)}%`;
}

function ouEdgeForCell(market, p, L, pImpOver, pImpUnder) {
  const pOver = clampProb01(modelProbOverMarket(market, p, L));
  if (!Number.isFinite(pOver)) return { pOver: NaN, pUnder: NaN, edgeO: NaN, edgeU: NaN };
  const pUnder = clampProb01(1 - pOver);
  const edgeO = Number.isFinite(pImpOver) ? pOver - pImpOver : NaN;
  const edgeU = Number.isFinite(pImpUnder) ? pUnder - pImpUnder : NaN;
  return { pOver, pUnder, edgeO, edgeU };
}

function ouCellEdgeStackHtml(market, p, L, pImpOver, pImpUnder, viewMode, oddsOverAm, oddsUnderAm) {
  const { pOver, pUnder, edgeO, edgeU } = ouEdgeForCell(market, p, L, pImpOver, pImpUnder);
  const lineStr = String(L);
  const oddsTxtOver = formatAmerican(oddsOverAm);
  const oddsTxtUnder = formatAmerican(oddsUnderAm);
  const clsO = edgeO > 0 ? "pos" : edgeO < 0 ? "neg" : "";
  const clsU = edgeU > 0 ? "pos" : edgeU < 0 ? "neg" : "";
  let modelLine = "";
  if (Number.isFinite(pOver)) {
    if (viewMode === "prob") {
      modelLine = `<div class="ou-model-line">O ${(pOver * 100).toFixed(1)}% · U ${(pUnder * 100).toFixed(1)}%</div>`;
    } else {
      const { do: dO, du: dU } = viggedDecimalsForOverUnder(pOver);
      modelLine = `<div class="ou-model-line">O ${formatAmerican(americanFromDecimal(dO))} · U ${formatAmerican(
        americanFromDecimal(dU)
      )}</div>`;
    }
  }
  return `<div class="ou-edge-stack">${modelLine}
    <div class="ou-edge-row ou-edge-row-over"><span class="ou-edge ${clsO}">${formatEdgePct(edgeO)}</span><span class="ou-edge-meta"><span class="ou-side-pill">O</span><span class="ou-line-pill">${lineStr}</span><span class="ou-edge-odds">${oddsTxtOver}</span></span></div>
    <div class="ou-edge-row ou-edge-row-under"><span class="ou-edge ${clsU}">${formatEdgePct(edgeU)}</span><span class="ou-edge-meta"><span class="ou-side-pill">U</span><span class="ou-line-pill">${lineStr}</span><span class="ou-edge-odds">${oddsTxtUnder}</span></span></div>
  </div>`;
}

/** Default: same order as projections (expected total ↑, or stat ↓ for props-style markets). */
let ouTableSort = { key: "pr-edge", dir: -1 };
let ouTableSortInited = false;
/** Last snapped O/U line; used while the line input is empty or mid-edit (avoid rewriting on every keystroke). */
let ouLineCommitted = 70.5;
/** Expanded round-projection row key (`rawName\\x1e${col.label}\\x1e${side}`); empty = none. */
let ouProjExpandedKey = "";
/** Set while building the open detail drawer so the chart can redraw after layout. */
let ouProjExpandedDetail = null;

function ouTableSortValue(playerRow, market, lineSel, pImpOverSel, pImpUnderSel, sortKey, propIdx) {
  if (sortKey === "golfer") return displayGolferName(playerRow.player_name || "").toLowerCase();
  if (sortKey && sortKey.startsWith("line-")) {
    const L = parseFloat(sortKey.slice(5));
    const pImpOver = ouBookImpliedForSortColumn(playerRow, market, L, lineSel, pImpOverSel, pImpUnderSel, propIdx, "over");
    const pImpUnder = ouBookImpliedForSortColumn(playerRow, market, L, lineSel, pImpOverSel, pImpUnderSel, propIdx, "under");
    const { edgeO } = ouEdgeForCell(market, playerRow, L, pImpOver, pImpUnder);
    return Number.isFinite(edgeO) ? edgeO : -Infinity;
  }
  return 0;
}

function updateOuSortIndicators() {
  const table = document.getElementById("table-ou");
  if (!table) return;
  table.querySelectorAll("thead th.sortable").forEach((th) => {
    const key = th.dataset.sortKey;
    const up = th.querySelector(".sort-up");
    const dn = th.querySelector(".sort-down");
    if (up) up.classList.toggle("active", key === ouTableSort.key && ouTableSort.dir > 0);
    if (dn) dn.classList.toggle("active", key === ouTableSort.key && ouTableSort.dir < 0);
  });
}

function initOuTableSortOnce() {
  if (ouTableSortInited) return;
  const table = document.getElementById("table-ou");
  if (!table) return;
  ouTableSortInited = true;
  table.querySelector("thead")?.addEventListener("click", (ev) => {
    const th = ev.target.closest("th.sortable");
    if (!th || !table.contains(th)) return;
    const key = th.dataset.sortKey;
    if (!key) return;
    if (ouTableSort.key === key) ouTableSort.dir *= -1;
    else {
      ouTableSort.key = key;
      if (key === "golfer" || key === "pr-golfer") ouTableSort.dir = 1;
      else if (key === "pr-market" || key === "pr-side") ouTableSort.dir = 1;
      else if (key === "pr-mu") ouTableSort.dir = -1;
      else if (key === "pr-line") ouTableSort.dir = 1;
      else if (key === "pr-odds" || key === "pr-pmod" || key === "pr-edge") ouTableSort.dir = -1;
      else ouTableSort.dir = -1;
    }
    buildOuTable();
  });
}

function lineMatchesOuHighlight(lineSel, L, market) {
  if (!Number.isFinite(lineSel)) return false;
  return Math.abs(lineSel - L) < 1e-6;
}

/** All players for `round`, sorted the same way as the O/U table for `market`. */
function ouSortedPlayerRows(market, round) {
  let rows = DATA.players.filter((p) => samePlayerRound(p, round));
  if (tournamentPostCutListPhase()) {
    rows = rows.filter((p) => !isPlayerEliminatedFromEvent(p));
  }
  rows.sort((a, b) => {
    const rec = ouStatRec(market);
    const va = num(a[rec.field], 1e9);
    const vb = num(b[rec.field], 1e9);
    if (market === "Total score" || market === "Putts") return va - vb;
    return vb - va;
  });
  return rows;
}

/** Field for Model O/U projection grid: best (lowest) projected round score first. */
function ouSortedPlayerRowsProjection(round) {
  let rows = DATA.players.filter((p) => samePlayerRound(p, round));
  if (tournamentPostCutListPhase()) {
    rows = rows.filter((p) => !isPlayerEliminatedFromEvent(p));
  }
  rows.sort((a, b) => {
    const ma = ouProjectedMean("Total score", a);
    const mb = ouProjectedMean("Total score", b);
    if (!Number.isFinite(ma) && !Number.isFinite(mb)) return 0;
    if (!Number.isFinite(ma)) return 1;
    if (!Number.isFinite(mb)) return -1;
    return ma - mb;
  });
  return rows;
}

/** Main grid Proj column (one decimal). */
function formatOuProjectedMean(mu) {
  return Number.isFinite(mu) ? mu.toFixed(1) : "—";
}

function projectionSortComparable(val, dir) {
  if (typeof val === "string") return val;
  if (Number.isFinite(val)) return val;
  return dir > 0 ? Number.POSITIVE_INFINITY : Number.NEGATIVE_INFINITY;
}

/** One display row: golfer × DK market × Over|Under (Round projections: DK lines only). */
function ouProjectionFlatRowsForPlayers(players, cols) {
  const out = [];
  for (const player of players) {
    for (let colIdx = 0; colIdx < cols.length; colIdx++) {
      const col = cols[colIdx];
      const mu = ouProjectedMean(col.market, player);
      const pick = chooseOuPropLineForProjection(col.market, player, mu, { dkOnly: true });
      if (!pick) continue;
      for (const side of ["over", "under"]) {
        out.push({ player, col, colIdx, side, mu, pick });
      }
    }
  }
  return out;
}

function ouProjectionRowStatOrder(a, b) {
  const na = displayGolferName(a.player.player_name || "").toLowerCase();
  const nb = displayGolferName(b.player.player_name || "").toLowerCase();
  const g = na.localeCompare(nb);
  if (g !== 0) return g;
  if (a.colIdx !== b.colIdx) return a.colIdx - b.colIdx;
  if (a.side === b.side) return 0;
  return a.side === "over" ? -1 : 1;
}

function ouTableSortValueProjRow(row, sortKey) {
  const { player, col, colIdx, side, mu, pick } = row;
  if (sortKey === "pr-golfer" || sortKey === "golfer") {
    return displayGolferName(player.player_name || "").toLowerCase();
  }
  if (sortKey === "pr-market") return colIdx;
  if (sortKey === "pr-side") return side === "over" ? 0 : 1;
  if (sortKey === "pr-mu") return mu;
  if (sortKey === "pr-line") return pick && Number.isFinite(pick.line) ? pick.line : NaN;
  if (sortKey === "pr-odds") {
    if (!pick) return NaN;
    const am = side === "over" ? pick.over : pick.under;
    return Number.isFinite(am) ? am : NaN;
  }
  if (sortKey === "pr-pmod") {
    if (!pick) return NaN;
    const pImpOver = impliedProbFromAmerican(pick.over);
    const pImpUnder = impliedProbFromAmerican(pick.under);
    const { pOver, pUnder } = ouEdgeForCell(col.market, player, pick.line, pImpOver, pImpUnder);
    return side === "over" ? pOver : pUnder;
  }
  if (sortKey === "pr-edge") {
    if (!pick) return NaN;
    const pImpOver = impliedProbFromAmerican(pick.over);
    const pImpUnder = impliedProbFromAmerican(pick.under);
    const { edgeO, edgeU } = ouEdgeForCell(col.market, player, pick.line, pImpOver, pImpUnder);
    const edge = side === "over" ? edgeO : edgeU;
    return Number.isFinite(edge) ? edge : NaN;
  }
  return NaN;
}

function compareOuProjectionRows(a, b, sortKey, dir) {
  const va = ouTableSortValueProjRow(a, sortKey);
  const vb = ouTableSortValueProjRow(b, sortKey);
  let c = 0;
  if (typeof va === "string" && typeof vb === "string") c = va.localeCompare(vb);
  else {
    const na = projectionSortComparable(va, dir);
    const nb = projectionSortComparable(vb, dir);
    c = Number(na) - Number(nb);
  }
  if (c !== 0) return c * dir;
  return ouProjectionRowStatOrder(a, b);
}

function ouProjMakeExpandKey(rawName, colLabel, side) {
  return `${String(rawName || "")}\x1e${String(colLabel || "")}\x1e${String(side || "")}`;
}

function ouProjLineBucketInt(L) {
  if (!Number.isFinite(L)) return NaN;
  return Math.floor(L - 0.5 + 1e-9);
}

function ouProjDiscreteBinProb(mu, sig, k) {
  if (!Number.isFinite(mu) || !Number.isFinite(sig) || sig <= 0) return NaN;
  const zHi = (k + 0.5 - mu) / sig;
  const zLo = (k - 0.5 - mu) / sig;
  return normalCdf(zHi) - normalCdf(zLo);
}

function drawOuProjDetailDistribution(canvas, market, player, line) {
  const ctx = canvas.getContext("2d");
  if (!ctx) return;
  const mKey = ouModelMarketKey(market) || "Total score";
  const mu = ouProjectedMean(market, player);
  const countLive = livePartialRoundCountPropAdjust(mKey, player);
  let sig = sigmaForOu(mKey, player) * countLive.sigmaScale;
  if (!Number.isFinite(sig) || sig < 0.06) sig = sigmaForOu(mKey, player);
  const dpr = Math.min(2, window.devicePixelRatio || 1);
  const cssW = 340;
  const cssH = 140;
  canvas.width = Math.round(cssW * dpr);
  canvas.height = Math.round(cssH * dpr);
  canvas.style.width = `${cssW}px`;
  canvas.style.height = `${cssH}px`;
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, cssW, cssH);
  ctx.fillStyle = "#0a0e14";
  ctx.fillRect(0, 0, cssW, cssH);
  const pad = { l: 28, r: 8, t: 14, b: 22 };
  const innerW = cssW - pad.l - pad.r;
  const innerH = cssH - pad.t - pad.b;
  if (!Number.isFinite(mu) || !Number.isFinite(sig)) {
    ctx.fillStyle = "#8b93a5";
    ctx.font = "11px DM Sans, system-ui, sans-serif";
    ctx.fillText("Not enough data for a distribution.", pad.l, pad.t + 24);
    return;
  }
  let lo = Math.floor(mu - 3.25 * sig);
  let hi = Math.ceil(mu + 3.25 * sig);
  if (mKey === "Total score") {
    lo = Math.max(58, lo);
    hi = Math.min(92, Math.max(hi, lo + 6));
  } else {
    lo = Math.max(0, lo);
    hi = Math.min(24, Math.max(hi, lo + 4));
  }
  if (Number.isFinite(line)) {
    const b = ouProjLineBucketInt(line);
    if (Number.isFinite(b)) {
      lo = Math.min(lo, b - 2);
      hi = Math.max(hi, b + 6);
    }
  }
  if (hi - lo > 16) hi = lo + 16;
  const keys = [];
  for (let k = lo; k <= hi; k++) keys.push(k);
  if (!keys.length) return;
  const probs = keys.map((kk) => ouProjDiscreteBinProb(mu, sig, kk));
  const maxP = Math.max(1e-6, ...probs.map((p) => (Number.isFinite(p) ? p : 0)));
  const lineBucket = Number.isFinite(line) ? ouProjLineBucketInt(line) : NaN;
  const slotW = innerW / keys.length;
  const barW = Math.max(4, slotW * 0.62);
  ctx.font = "9px DM Sans, system-ui, sans-serif";
  ctx.textAlign = "center";
  for (let i = 0; i < keys.length; i++) {
    const k = keys[i];
    const p = Number.isFinite(probs[i]) ? probs[i] : 0;
    const barH = (p / maxP) * innerH * 0.92;
    const cx = pad.l + (i + 0.5) * slotW;
    const x0 = cx - barW / 2;
    const y0 = pad.t + innerH - barH;
    const yBase = pad.t + innerH;
    let fill = "rgba(120,128,148,0.78)";
    if (Number.isFinite(lineBucket)) {
      if (k === lineBucket) fill = "#e4b022";
      else if (k > lineBucket) fill = "rgba(0, 196, 107, 0.88)";
    }
    ctx.fillStyle = fill;
    ctx.fillRect(x0, y0, barW, yBase - y0);
    ctx.strokeStyle = "rgba(255,255,255,0.1)";
    ctx.strokeRect(x0, y0, barW, yBase - y0);
    ctx.fillStyle = "#8b93a5";
    ctx.fillText(String(k), cx, cssH - 6);
  }
  ctx.textAlign = "left";
  ctx.fillStyle = "#6d7382";
  ctx.font = "8px DM Sans, system-ui, sans-serif";
  ctx.fillText(
    `Normal approx · μ ${mu.toFixed(2)} · σ ${sig.toFixed(2)}${Number.isFinite(line) ? ` · line ${line}` : ""}`,
    pad.l,
    10,
  );
}

function buildOuProjDetailPanel(player, col, side, mu, pick, rawName) {
  const wrap = document.createElement("div");
  wrap.className = "ou-proj-detail-wrap";
  const mKey = ouModelMarketKey(col.market) || "Total score";
  const rec = ouStatRec(mKey);
  const countLive = livePartialRoundCountPropAdjust(mKey, player);
  let sig = sigmaForOu(mKey, player) * countLive.sigmaScale;
  if (!Number.isFinite(sig) || sig < 0.06) sig = sigmaForOu(mKey, player);
  const baseField = num(player[rec.field], NaN);

  const chartCol = document.createElement("div");
  chartCol.className = "ou-proj-detail-col ou-proj-detail-chart";
  const h1 = document.createElement("h4");
  h1.className = "ou-proj-detail-heading";
  h1.textContent = "Distribution";
  const sub1 = document.createElement("p");
  sub1.className = "ou-proj-detail-sub";
  sub1.textContent = `Model mass by outcome (line bucket highlighted). Side: ${side === "over" ? "Over" : "Under"}.`;
  const canvas = document.createElement("canvas");
  canvas.className = "ou-proj-detail-canvas";
  canvas.setAttribute("role", "img");
  canvas.setAttribute("aria-label", `${col.label} outcome distribution`);
  const leg = document.createElement("div");
  leg.className = "ou-proj-detail-legend";
  leg.innerHTML =
    '<span class="ou-proj-leg ou-proj-leg-line"><span class="ou-proj-leg-dot"></span>Line</span>' +
    '<span class="ou-proj-leg ou-proj-leg-over"><span class="ou-proj-leg-dot"></span>Over</span>' +
    '<span class="ou-proj-leg ou-proj-leg-under"><span class="ou-proj-leg-dot"></span>Under</span>';
  chartCol.appendChild(h1);
  chartCol.appendChild(sub1);
  chartCol.appendChild(canvas);
  chartCol.appendChild(leg);

  const histCol = document.createElement("div");
  histCol.className = "ou-proj-detail-col ou-proj-detail-hist";
  const h2 = document.createElement("h4");
  h2.className = "ou-proj-detail-heading";
  h2.textContent = "Projection snapshot";
  const dl1 = document.createElement("dl");
  dl1.className = "ou-proj-detail-dl";
  const addRow = (dt, dd) => {
    const dtt = document.createElement("dt");
    dtt.textContent = dt;
    const ddd = document.createElement("dd");
    ddd.textContent = dd;
    dl1.appendChild(dtt);
    dl1.appendChild(ddd);
  };
  addRow("Field μ (" + col.label + ")", Number.isFinite(mu) ? mu.toFixed(2) : "—");
  addRow("σ (model)", Number.isFinite(sig) ? sig.toFixed(2) : "—");
  addRow("Raw field", Number.isFinite(baseField) ? baseField.toFixed(2) : "—");
  if (mKey === "Total score" && Number.isFinite(num(player.round_sd, NaN))) {
    addRow("Round SD (feed)", num(player.round_sd).toFixed(2));
  }
  histCol.appendChild(h2);
  histCol.appendChild(dl1);

  const modelCol = document.createElement("div");
  modelCol.className = "ou-proj-detail-col ou-proj-detail-model";
  const h3 = document.createElement("h4");
  h3.className = "ou-proj-detail-heading";
  h3.textContent = "Model inputs";
  const dl2 = document.createElement("dl");
  dl2.className = "ou-proj-detail-dl";
  const addM = (dt, dd) => {
    const dtt = document.createElement("dt");
    dtt.textContent = dt;
    const ddd = document.createElement("dd");
    ddd.textContent = dd;
    dl2.appendChild(dtt);
    dl2.appendChild(ddd);
  };
  addM("μ SG", Number.isFinite(num(player.mu_sg, NaN)) ? num(player.mu_sg).toFixed(3) : "—");
  addM("Implied μ SG", Number.isFinite(num(player.implied_mu_sg, NaN)) ? num(player.implied_mu_sg).toFixed(3) : "—");
  addM("Score to par", Number.isFinite(num(player.score_to_par, NaN)) ? num(player.score_to_par).toFixed(2) : "—");
  addM("Weather", formatEffectiveWeatherLine(player));
  addM("Pricing", PRICING_STATE.mode + (PRICING_STATE.mode === "skill" ? ` · ${PRICING_STATE.skill}` : ""));
  if (pick && Number.isFinite(pick.line)) {
    addM("Book line", String(pick.line));
    const nv = propsNoVigOverProb(pick.over, pick.under);
    addM("P(over) no-vig", Number.isFinite(nv) ? `${(nv * 100).toFixed(1)}%` : "—");
  }
  modelCol.appendChild(h3);
  modelCol.appendChild(dl2);

  wrap.appendChild(chartCol);
  wrap.appendChild(histCol);
  wrap.appendChild(modelCol);

  ouProjExpandedDetail = { market: col.market, player, line: pick && Number.isFinite(pick.line) ? pick.line : NaN };
  return wrap;
}

function buildOuTable() {
  ensureOuMarketFilterValid();
  const table = document.getElementById("table-ou");
  if (!table) return;
  updateOuSyntheticOddsNoteVisibility();
  initOuTableSortOnce();
  ouProjExpandedDetail = null;
  const round = getOuRound();
  const thead = table.querySelector("thead");
  const tbody = table.querySelector("tbody");
  if (!thead || !tbody) return;

  const sortInd = `<span class="sort-ind"><span class="sort-up">▲</span><span class="sort-down">▼</span></span>`;
  const cols = ouProjectionColumnsActive();
  const pmf = document.getElementById("ou-proj-market-filter");
  const prevProjMarket = pmf ? String(pmf.value || "") : "";
  if (pmf) {
    pmf.innerHTML = "";
    const allM = document.createElement("option");
    allM.value = "";
    allM.textContent = "All markets";
    pmf.appendChild(allM);
    for (const col of cols) {
      const opt = document.createElement("option");
      opt.value = col.label;
      opt.textContent = col.label;
      pmf.appendChild(opt);
    }
    const labels = new Set(cols.map((c) => c.label));
    if (prevProjMarket && labels.has(prevProjMarket)) pmf.value = prevProjMarket;
    else pmf.value = "";
  }
  if (ouTableSort.key.startsWith("proj-") || ouTableSort.key.startsWith("mkt-")) {
    ouTableSort = { key: "pr-edge", dir: -1 };
  }
  if (ouTableSort.key === "golfer") ouTableSort.key = "pr-golfer";
  const hr = document.createElement("tr");
  const projHeadSpecs = [
    ["pr-golfer", "Golfer", "sortable ou-proj-long-th ou-proj-th-golfer"],
    ["pr-market", "Market", "sortable ou-proj-long-th ou-proj-th-market"],
    ["pr-side", "Side", "sortable ou-proj-long-th num ou-proj-th-side"],
    ["pr-mu", "Proj", "sortable ou-proj-long-th num ou-proj-th-mu"],
    ["pr-line", "Line", "sortable ou-proj-long-th num ou-proj-th-line"],
    ["", "Book", "ou-proj-long-th num ou-proj-th-book"],
    ["pr-odds", "Odds", "sortable ou-proj-long-th num ou-proj-th-odds"],
    ["pr-pmod", "P(model)", "sortable ou-proj-long-th num ou-proj-th-pmod"],
    ["pr-edge", "Edge", "sortable ou-proj-long-th num ou-proj-th-edge"],
  ];
  for (const [key, label, cls] of projHeadSpecs) {
    const th = document.createElement("th");
    th.className = cls;
    if (key) {
      th.dataset.sortKey = key;
      th.innerHTML = `${label}${sortInd}`;
    } else {
      th.textContent = label;
      th.title = "DraftKings";
    }
    hr.appendChild(th);
  }
  thead.innerHTML = "";
  thead.appendChild(hr);

  const allRows = ouSortedPlayerRowsProjection(round);
  const pf = document.getElementById("ou-player-filter");
  const suggPanel = document.getElementById("ou-player-filter-suggest");
  const prevQ = pf && pf instanceof HTMLInputElement ? String(pf.value || "").trim().toLowerCase() : "";
  if (pf && pf instanceof HTMLInputElement && suggPanel) {
    const labels = [...new Set(allRows.map((p) => displayGolferName(String(p.player_name || ""))))]
      .filter(Boolean)
      .sort((a, b) => a.localeCompare(b));
    golferSuggestWriteLabels(suggPanel, labels);
    reopenGolferSuggestIfSearchFocused(pf, suggPanel, () => {
      ouProjExpandedKey = "";
      buildOuTable();
    });
    if (prevQ && !allRows.some((p) => golferNameMatchesQuery(String(p.player_name || ""), prevQ))) pf.value = "";
  }
  const q = pf && pf instanceof HTMLInputElement ? String(pf.value || "").trim().toLowerCase() : "";
  const playersFiltered = !q ? allRows.slice() : allRows.filter((p) => golferNameMatchesQuery(String(p.player_name || ""), q));

  let flatRows = ouProjectionFlatRowsForPlayers(playersFiltered, cols);
  const projMarketSel = String(document.getElementById("ou-proj-market-filter")?.value || "").trim();
  if (projMarketSel) flatRows = flatRows.filter((r) => String(r.col.label) === projMarketSel);
  const k = ouTableSort.key;
  const d = ouTableSort.dir;
  if (k !== "stat-order") {
    flatRows = flatRows.slice().sort((a, b) => compareOuProjectionRows(a, b, k, d));
  } else {
    flatRows.sort(ouProjectionRowStatOrder);
  }

  const projColCount = projHeadSpecs.length;
  const flatRowKeys = new Set(
    flatRows.map((rr) => ouProjMakeExpandKey(String(rr.player.player_name || ""), rr.col.label, rr.side)),
  );
  if (ouProjExpandedKey && !flatRowKeys.has(ouProjExpandedKey)) ouProjExpandedKey = "";

  tbody.innerHTML = "";
  if (!cols.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = projColCount;
    td.className = "ou-cell ou-proj-long-td ou-proj-empty-td";
    td.textContent =
      "No round O/U props in projections.json yet.\n\n" +
      "• Set DATAGOLF_API_KEY (Render dashboard or datagolf.local.json locally).\n" +
      "• Keep DraftKings enabled: do not set GOLF_SKIP_DK_OU=1 (omit it or use 0).\n" +
      "• Run one of: npm run fetch:book-odds · npm run refresh · npm run perfect\n" +
      "• DraftKings needs Chromium: npx playwright install chromium (see Render build).\n" +
      "• If DK opens the wrong event, set DK_LEAGUE_URL to your league URL with ?category=round.";
    tr.appendChild(td);
    tbody.appendChild(tr);
  } else if (!flatRows.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = projColCount;
    td.className = "ou-cell ou-proj-long-td ou-proj-empty-td";
    td.textContent = draftKingsRoundPropOddsAvailable()
      ? "No DraftKings lines for this golfer, market, or filter. Rows with only model lines are hidden."
      : "No DraftKings round O/U in projections.json yet — run fetch:book-odds with DK enabled.";
    tr.appendChild(td);
    tbody.appendChild(tr);
  } else {
  const ouTbodyFrag = document.createDocumentFragment();
  for (const r of flatRows) {
    const tr = document.createElement("tr");
    tr.className = "ou-proj-long-tr ou-proj-data-row";
    const { player, col, side, mu, pick } = r;
    const rawName = String(player.player_name || "");
    const expandKey = ouProjMakeExpandKey(rawName, col.label, side);
    tr.dataset.expandKey = expandKey;
    if (ouProjExpandedKey === expandKey) tr.classList.add("ou-proj-row-expanded");
    const nameTd = document.createElement("td");
    nameTd.className = "ou-cell ou-proj-long-td ou-proj-td-golfer";
    const countryRaw = String(player.country || "").trim();
    const countrySlug = golfCountryToFlagSlug(countryRaw);
    if (countrySlug) {
      const flagEl = document.createElement("img");
      flagEl.className = "ou-player-flag";
      flagEl.alt = countryRaw || "Country";
      flagEl.title = countryRaw || "";
      flagEl.loading = "lazy";
      flagEl.decoding = "async";
      flagEl.src = flagImageUrlFromCountry(countryRaw);
      flagEl.onerror = function onFlagErr() {
        this.onerror = null;
        this.style.display = "none";
      };
      nameTd.appendChild(flagEl);
    }
    const nameText = document.createElement("span");
    nameText.textContent = displayGolferName(rawName);
    nameTd.appendChild(nameText);
    nameTd.dataset.playerValue = rawName;
    tr.appendChild(nameTd);

    const mktTd = document.createElement("td");
    mktTd.className = "ou-cell ou-proj-long-td ou-proj-td-market";
    mktTd.textContent = col.label;
    tr.appendChild(mktTd);

    const sideTd = document.createElement("td");
    sideTd.className = "ou-cell ou-proj-long-td num ou-proj-td-side";
    sideTd.textContent = side === "over" ? "Over" : "Under";
    tr.appendChild(sideTd);

    const muTd = document.createElement("td");
    muTd.className = "ou-cell ou-proj-long-td num ou-proj-td-mu";
    muTd.textContent = formatOuProjectedMean(mu);
    tr.appendChild(muTd);

    const lineTd = document.createElement("td");
    lineTd.className = "ou-cell ou-proj-long-td num ou-proj-td-line";
    lineTd.textContent = pick && Number.isFinite(pick.line) ? String(pick.line) : "—";
    tr.appendChild(lineTd);

    const bookTd = document.createElement("td");
    bookTd.className = "ou-cell ou-proj-long-td num ou-proj-td-book";
    const bookWrap = document.createElement("span");
    bookWrap.className = "ou-proj-book-logo-wrap";
    const bookImg = document.createElement("img");
    bookImg.className = "ou-proj-book-logo-img";
    bookImg.alt = "DraftKings";
    bookImg.loading = "lazy";
    const bookFb = document.createElement("span");
    bookFb.className = "ou-proj-book-logo-fallback";
    bookFb.textContent = "DK";
    bookFb.style.display = "none";
    bookWrap.appendChild(bookImg);
    bookWrap.appendChild(bookFb);
    bookTd.appendChild(bookWrap);
    attachBookLogoWithFallback(bookImg, bookFb, SPORTSBOOK_META.draftkings.domain);
    tr.appendChild(bookTd);

    const oddsTd = document.createElement("td");
    oddsTd.className = "ou-cell ou-proj-long-td num ou-proj-td-odds";
    if (pick) {
      const am = side === "over" ? pick.over : pick.under;
      oddsTd.textContent = Number.isFinite(am) ? formatAmerican(am) : "—";
    } else oddsTd.textContent = "—";
    tr.appendChild(oddsTd);

    const pTd = document.createElement("td");
    pTd.className = "ou-cell ou-proj-long-td num ou-proj-td-pmod";
    const edgeTd = document.createElement("td");
    edgeTd.className = "ou-cell ou-proj-long-td num ou-proj-td-edge";
    if (pick) {
      const pImpOver = impliedProbFromAmerican(pick.over);
      const pImpUnder = impliedProbFromAmerican(pick.under);
      const { pOver, pUnder, edgeO, edgeU } = ouEdgeForCell(col.market, player, pick.line, pImpOver, pImpUnder);
      const pMod = side === "over" ? pOver : pUnder;
      const edge = side === "over" ? edgeO : edgeU;
      pTd.textContent = Number.isFinite(pMod) ? `${(pMod * 100).toFixed(1)}%` : "—";
      edgeTd.textContent = formatEdgePct(edge);
      edgeTd.classList.add(edge > 0 ? "pos" : edge < 0 ? "neg" : "");
    } else {
      pTd.textContent = "—";
      edgeTd.textContent = "—";
    }
    tr.appendChild(pTd);
    tr.appendChild(edgeTd);

    if (Number.isFinite(mu)) {
      const mKey = ouModelMarketKey(col.market) || "Total score";
      const countLive = livePartialRoundCountPropAdjust(mKey, player);
      let sig = sigmaForOu(mKey, player) * countLive.sigmaScale;
      if (!Number.isFinite(sig) || sig < 0.06) sig = sigmaForOu(mKey, player);
      if (Number.isFinite(sig)) {
        tr.title = `${col.label} · μ ${mu.toFixed(2)} · σ ${sig.toFixed(2)} · ${side === "over" ? "Over" : "Under"}`;
      }
    }
    ouTbodyFrag.appendChild(tr);
    if (ouProjExpandedKey === expandKey) {
      const dtr = document.createElement("tr");
      dtr.className = "ou-proj-detail-tr";
      const dtd = document.createElement("td");
      dtd.colSpan = projColCount;
      dtd.className = "ou-proj-detail-td";
      dtd.appendChild(buildOuProjDetailPanel(player, col, side, mu, pick, rawName));
      dtr.appendChild(dtd);
      ouTbodyFrag.appendChild(dtr);
    }
  }
  tbody.appendChild(ouTbodyFrag);
  }

  updateOuSortIndicators();
  syncOuChartCard();
  syncForecastWaveBannerTexts();
  if (ouProjExpandedDetail) {
    requestAnimationFrame(() => {
      const c = document.querySelector("#table-ou tbody .ou-proj-detail-canvas");
      if (!c || !ouProjExpandedDetail) return;
      drawOuProjDetailDistribution(
        c,
        ouProjExpandedDetail.market,
        ouProjExpandedDetail.player,
        ouProjExpandedDetail.line,
      );
    });
  }
}

function isOuGolferSelected() {
  return Boolean(String(document.getElementById("ou-player-filter")?.value || "").trim());
}

/** Line-distribution chart when a golfer is chosen (table row or Golfer filter). */
function syncOuChartCard() {
  const card = document.getElementById("ou-chart-card");
  if (!card) return;
  if (document.getElementById("panel-ou")?.dataset?.ouView === "projections") {
    card.hidden = true;
    hideOuChartTooltip();
    ouChartHitRegions = [];
    return;
  }
  const show = isOuGolferSelected();
  card.hidden = !show;
  if (!show) {
    hideOuChartTooltip();
    ouChartHitRegions = [];
    return;
  }
  drawOuLineDistributionChart();
}

function hideOuChartTooltip() {
  const tip = document.getElementById("ou-chart-tooltip");
  if (tip) tip.hidden = true;
}

function showOuChartTooltip(ev, hit) {
  const tip = document.getElementById("ou-chart-tooltip");
  const wrap = document.querySelector(".ou-chart-wrap");
  if (!tip || !wrap) return;
  const pO = clampProb01(hit.pOver);
  const pU = clampProb01(1 - hit.pOver);
  const { do: dO, du: dU } = viggedDecimalsForOverUnder(pO);
  const oAm = americanFromDecimal(dO);
  const uAm = americanFromDecimal(dU);
  tip.innerHTML = `<div class="ou-tip-row"><strong>Line</strong><span class="ou-tip-value">${hit.line}</span></div><div class="ou-tip-row"><strong>P(over)</strong><span class="ou-tip-value">${(pO * 100).toFixed(1)}%</span></div><div class="ou-tip-row"><strong>P(under)</strong><span class="ou-tip-value">${(pU * 100).toFixed(1)}%</span></div><div class="ou-tip-row"><strong>Over</strong><span class="ou-tip-value">${formatAmerican(oAm)}</span></div><div class="ou-tip-row"><strong>Under</strong><span class="ou-tip-value">${formatAmerican(uAm)}</span></div>`;
  tip.hidden = false;
  const rect = wrap.getBoundingClientRect();
  const padWrap = 8;
  let left = ev.clientX - rect.left + 12;
  let top = ev.clientY - rect.top + 10;
  tip.style.left = `${left}px`;
  tip.style.top = `${top}px`;
  const tw = tip.offsetWidth;
  const th = tip.offsetHeight;
  const maxL = wrap.clientWidth - tw - padWrap;
  const maxT = wrap.clientHeight - th - padWrap;
  if (left > maxL) left = Math.max(padWrap, maxL);
  if (top > maxT) top = Math.max(padWrap, maxT);
  tip.style.left = `${left}px`;
  tip.style.top = `${top}px`;
}

function hideResultsChartTooltip() {
  const tip = document.getElementById("results-chart-tooltip");
  if (tip) tip.hidden = true;
}

function showResultsChartTooltip(ev, hit) {
  const tip = document.getElementById("results-chart-tooltip");
  const wrap = tip?.closest(".results-chart-wrap");
  if (!tip || !wrap || !hit?.tipHtml) return;
  tip.innerHTML = hit.tipHtml;
  tip.hidden = false;
  const rect = wrap.getBoundingClientRect();
  const padWrap = 8;
  let left = ev.clientX - rect.left + 12;
  let top = ev.clientY - rect.top + 10;
  tip.style.left = `${left}px`;
  tip.style.top = `${top}px`;
  const tw = tip.offsetWidth;
  const th = tip.offsetHeight;
  const maxL = wrap.clientWidth - tw - padWrap;
  const maxT = wrap.clientHeight - th - padWrap;
  if (left > maxL) left = Math.max(padWrap, maxL);
  if (top > maxT) top = Math.max(padWrap, maxT);
  tip.style.left = `${left}px`;
  tip.style.top = `${top}px`;
}

/** Bar chart: P(over) for score/bogeys (declining vs line); P(under) for birdies/pars (rising vs line). */
function drawOuLineDistributionChart() {
  const canvas = document.getElementById("ou-chart-canvas");
  if (!canvas || !canvas.getContext) return;
  hideOuChartTooltip();
  ouChartHitRegions = [];
  const market = getOuMarket();
  const lowerBetter = ouMarketLowerIsBetter(market);
  const titleEl = document.getElementById("ou-chart-title");
  if (titleEl) {
    titleEl.textContent = lowerBetter ? "P(over) by line" : "P(under) by line";
  }
  canvas.setAttribute(
    "aria-label",
    lowerBetter ? "P(over) by line" : "P(under) by line"
  );
  const lines = OU_LINE_RANGES[market] || OU_LINE_RANGES["Total score"];
  const round = getOuRound();
  const allRows = ouSortedPlayerRows(market, round);
  const fp = String(document.getElementById("ou-player-filter")?.value || "");
  if (!fp.trim()) return;
  const row = ouResolveSinglePlayerRowFromFilter(allRows, fp);
  if (!row) return;

  const dpr = Math.min(2, window.devicePixelRatio || 1);
  let cssW = canvas.clientWidth;
  if (!Number.isFinite(cssW) || cssW < 48) cssW = 800;
  const cssH = Math.round((cssW * 240) / 800) || 240;
  canvas.width = Math.round(cssW * dpr);
  canvas.height = Math.round(cssH * dpr);
  const ctx = canvas.getContext("2d");
  if (!ctx) return;
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  const w = cssW;
  const h = cssH;
  ctx.clearRect(0, 0, w, h);
  ctx.fillStyle = "#0a0c0f";
  ctx.fillRect(0, 0, w, h);

  const pad = { l: 42, r: 12, t: 10, b: 34 };
  const innerW = w - pad.l - pad.r;
  const innerH = h - pad.t - pad.b;
  const n = lines.length;
  if (!row || !n) {
    ctx.fillStyle = "#8b8f9c";
    ctx.font = "13px DM Sans, system-ui, sans-serif";
    const msg = !n
      ? "No lines for this market."
      : !allRows.length
        ? "Load projections to see the chart."
        : "No player matches the filter.";
    ctx.fillText(msg, pad.l, pad.t + 28);
    return;
  }

  function yPct(p) {
    return pad.t + innerH * (1 - p / 100);
  }

  ctx.strokeStyle = "rgba(255,255,255,0.08)";
  ctx.lineWidth = 1;
  for (const pct of [0, 25, 50, 75, 100]) {
    const y = yPct(pct);
    ctx.beginPath();
    ctx.moveTo(pad.l, y);
    ctx.lineTo(pad.l + innerW, y);
    ctx.stroke();
  }
  ctx.strokeStyle = "rgba(255,255,255,0.22)";
  ctx.setLineDash([4, 4]);
  ctx.beginPath();
  ctx.moveTo(pad.l, yPct(50));
  ctx.lineTo(pad.l + innerW, yPct(50));
  ctx.stroke();
  ctx.setLineDash([]);

  ctx.fillStyle = "#8b8f9c";
  ctx.font = "10px DM Sans, system-ui, sans-serif";
  ctx.textAlign = "right";
  for (const pct of [0, 25, 50, 75, 100]) {
    ctx.fillText(`${pct}%`, pad.l - 6, yPct(pct) + 3);
  }

  const slotW = innerW / n;
  const barW = Math.max(4, slotW * 0.62);
  for (let i = 0; i < n; i++) {
    const L = lines[i];
    const pOverRaw = modelProbOverMarket(market, row, L);
    const pOver = clampProb01(pOverRaw);
    const pUnder = Number.isFinite(pOver) ? 1 - pOver : NaN;
    const pChart = lowerBetter ? pOver : 1 - pOver;
    const pct = Number.isFinite(pChart) ? pChart * 100 : NaN;
    const cx = pad.l + (i + 0.5) * slotW;
    const x0 = cx - barW / 2;
    if (!Number.isFinite(pct)) continue;
    const y0 = yPct(pct);
    const yBase = yPct(0);
    // Colors: for round score/bogeys show Under=green, Over=red.
    // For other markets, keep "high plotted probability" = green.
    const isUnderFav = Number.isFinite(pUnder) && Number.isFinite(pOver) ? pUnder >= pOver : false;
    const isGreen = lowerBetter ? isUnderFav : pct >= 50;
    ctx.fillStyle = isGreen ? "rgba(0, 196, 107, 0.82)" : "rgba(255, 138, 138, 0.88)";
    ctx.fillRect(x0, y0, barW, yBase - y0);
    ctx.strokeStyle = "rgba(255,255,255,0.12)";
    ctx.strokeRect(x0, y0, barW, yBase - y0);
    const hitPad = 3;
    ouChartHitRegions.push({
      x0: Math.max(pad.l, x0 - hitPad),
      y0: pad.t,
      w: Math.min(pad.l + innerW, x0 + barW + hitPad) - Math.max(pad.l, x0 - hitPad),
      h: pad.t + innerH - pad.t,
      line: L,
      pOver,
    });
  }

  ctx.fillStyle = "#8b8f9c";
  ctx.font = "10px DM Sans, system-ui, sans-serif";
  ctx.textAlign = "center";
  for (let i = 0; i < n; i++) {
    const cx = pad.l + (i + 0.5) * slotW;
    ctx.fillText(String(lines[i]), cx, h - 10);
  }

  ctx.textAlign = "left";
  ctx.fillStyle = "#6b6f7a";
  ctx.font = "9px DM Sans, system-ui, sans-serif";
  ctx.fillText("Line", pad.l, h - 2);
}

function propMarketToStatKey(market) {
  const m = String(market || "").toLowerCase();
  if (m.includes("total")) return "total";
  if (m.includes("bird")) return "birdies";
  if (m.includes("par")) return "pars";
  if (m.includes("bogey")) return "bogeys";
  if (m.includes("gir")) return "gir";
  if (m.includes("fairway")) return "fairways";
  if (m.includes("putt")) return "putts";
  return "total";
}

function projectionRowForPlayerRound(playerName, round) {
  const want = String(playerName || "").trim().toLowerCase();
  const r = num(round, 1);
  return DATA.players.find(
    (p) => String(p.player_name || "").trim().toLowerCase() === want && samePlayerRound(p, r)
  );
}

/** Props / DK use "First Last"; projections use "Last, First". Resolve a row for `getModelRoundForEv` context. */
function projectionRowForPropPlayerSource(propRow, preferredRound) {
  const id = Math.round(num(propRow?.dg_id, NaN));
  if (Number.isFinite(id) && id > 0) {
    const hit = projectionPlayerRowForModel(id, preferredRound);
    if (hit) return hit;
  }
  const raw = String(propRow?.player_name || "").trim().toLowerCase();
  if (!raw) return null;
  const cand = (DATA.players || []).filter((p) => samePlayerRound(p, preferredRound));
  for (const p of cand) {
    if (String(p.player_name || "").trim().toLowerCase() === raw) return p;
    if (displayGolferName(p.player_name).trim().toLowerCase() === raw) return p;
  }
  return null;
}

/** De-vig two-way O/U into an implied P(over) from posted American prices. */
function propsNoVigOverProb(overAm, underAm) {
  const o = impliedProbFromAmerican(overAm);
  const u = impliedProbFromAmerican(underAm);
  if (!Number.isFinite(o) || !Number.isFinite(u) || o + u <= 1e-9) return NaN;
  return o / (o + u);
}

function appendModelOuPropsEvRows(rows, elim) {
  const props = Array.isArray(DATA.props) ? DATA.props : [];
  if (!props.length) return;
  const rMod = getModelRoundForEv();
  const allow = new Set(["Total Score", "Birdies", "Pars", "Bogeys", "GIR", "Fairways hit", "Putts"]);
  for (const pr of props) {
    const marketCanon = ouPropsCanonicalMarket(pr.market);
    if (!allow.has(marketCanon)) continue;
    const mKey = marketCanon === "Total Score" ? "Total score" : marketCanon;
    const L = enforceHalfLine(num(pr.line, NaN));
    if (!Number.isFinite(L)) continue;
    const oAm = Math.round(num(pr.over_odds, NaN));
    const uAm = Math.round(num(pr.under_odds, NaN));
    if (!Number.isFinite(oAm) || !Number.isFinite(uAm) || oAm === 0 || uAm === 0) continue;
    const prow = projectionRowForPropPlayerSource(pr, rMod);
    if (!prow) continue;
    const dgId = Math.round(num(prow.dg_id, NaN));
    if (elim.size && elim.has(dgId)) continue;
    const pOver = clampProb01(modelProbOverMarket(mKey, prow, L));
    if (!Number.isFinite(pOver)) continue;
    const pUnder = clampProb01(1 - pOver);
    const dO = decimalFromAmerican(oAm);
    const dU = decimalFromAmerican(uAm);
    const qOver = propsNoVigOverProb(oAm, uAm);
    const golfer = displayGolferName(String(prow.player_name || ""));
    rows.push({
      golfer,
      market: marketCanon,
      bet: `Over ${L}`,
      modelPct: pOver,
      modelEv: Number.isFinite(dO) ? pOver * dO - 1 : NaN,
      bestBook: "draftkings",
      bestBookOdds: formatAmerican(oAm),
      bestDec: dO,
      consensusP: qOver,
    });
    rows.push({
      golfer,
      market: marketCanon,
      bet: `Under ${L}`,
      modelPct: pUnder,
      modelEv: Number.isFinite(dU) ? pUnder * dU - 1 : NaN,
      bestBook: "draftkings",
      bestBookOdds: formatAmerican(uAm),
      bestDec: dU,
      consensusP: Number.isFinite(qOver) ? 1 - qOver : NaN,
    });
  }
}

function modelProbForProp(prop) {
  const stat = propMarketToStatKey(prop.market);
  const row = projectionRowForPlayerRound(prop.player_name, getModelRoundForEv());
  if (!row) return { pOver: NaN, pUnder: NaN };
  const line = num(prop.line, NaN);
  if (!Number.isFinite(line)) return { pOver: NaN, pUnder: NaN };
  const marketLabel = ouMarketKeyFromStatKey(stat);
  const pOver = modelProbOverMarket(marketLabel, row, line);
  return { pOver, pUnder: 1 - pOver };
}

const SPORTSBOOK_META = {
  draftkings: { label: "DraftKings", short: "DK", domain: "draftkings.com" },
  fanduel: { label: "FanDuel", short: "FD", domain: "fanduel.com" },
  betmgm: { label: "BetMGM", short: "MGM", domain: "betmgm.com" },
  caesars: { label: "Caesars", short: "CZR", domain: "caesars.com" },
  bet365: { label: "Bet365", short: "365", domain: "bet365.com" },
  bovada: { label: "Bovada", short: "BOV", domain: "bovada.lv" },
  pointsbet: { label: "PointsBet", short: "PB", domain: "pointsbet.com" },
  williamhill: { label: "William Hill", short: "WH", domain: "williamhill.com" },
  betway: { label: "Betway", short: "BW", domain: "betway.com" },
  unibet: { label: "Unibet", short: "UB", domain: "unibet.com" },
  skybet: { label: "SkyBet", short: "SKY", domain: "skybet.com" },
  pinnacle: { label: "Pinnacle", short: "PIN", domain: "pinnacle.com" },
  betonline: { label: "BetOnline", short: "BOL", domain: "betonline.ag" },
  circa: { label: "Circa", short: "CIRC", domain: "circasports.com" },
  betcris: { label: "BetCRIS", short: "BC", domain: "betcris.com" },
  wynnbet: { label: "WynnBET", short: "WYN", domain: "wynnbet.com" },
  datagolf: { label: "DataGolf", short: "DG", domain: "datagolf.com" },
};

const EV_ALLOWED_SPORTSBOOKS = new Set([
  "pinnacle",
  "draftkings",
  "fanduel",
  "bet365",
  "betmgm",
  "betonline",
  "betcris",
]);

/** Books often present on DataGolf outright feeds but omitted from tight EV list — needed for finish ladder / Course Fit when only e.g. PointsBet posts top 20. */
const OUTRIGHT_LADDER_EXTRA_BOOKS = new Set([
  "pointsbet",
  "williamhill",
  "betway",
  "skybet",
  "wynnbet",
  "circa",
  "betcris",
  "unibet",
]);

function outrightLadderSportsbookAllowed(bookRaw) {
  const k = normalizeEvSportsbookKey(bookRaw);
  if (!k || k === "datagolf") return false;
  if (EV_ALLOWED_SPORTSBOOKS.has(k)) return true;
  return OUTRIGHT_LADDER_EXTRA_BOOKS.has(k);
}

function normalizeEvSportsbookKey(bookRaw) {
  const k = String(bookRaw || "").trim().toLowerCase();
  if (!k) return "";
  if (k === "betonlineag" || k === "betonlineas") return "betonline";
  return k;
}

function evSportsbookAllowed(bookRaw, opts = {}) {
  if (opts.allowDatagolf && normalizeEvSportsbookKey(bookRaw) === "datagolf") return true;
  return EV_ALLOWED_SPORTSBOOKS.has(normalizeEvSportsbookKey(bookRaw));
}

function evDevigSportsbookAllowed(bookRaw) {
  const k = normalizeEvSportsbookKey(bookRaw);
  return k === "datagolf" || EV_ALLOWED_SPORTSBOOKS.has(k);
}

function filterOddsObjectForEvSportsbooks(oddsObj, opts = {}) {
  const out = {};
  if (!oddsObj || typeof oddsObj !== "object") return out;
  for (const [bk, pack] of Object.entries(oddsObj)) {
    const norm = normalizeEvSportsbookKey(bk);
    if (!evSportsbookAllowed(norm, opts)) continue;
    out[norm] = pack;
  }
  return out;
}

function bookMeta(book) {
  const k = String(book || "").trim().toLowerCase();
  if (SPORTSBOOK_META[k]) return { ...SPORTSBOOK_META[k], key: k };
  return { label: k || "Book", short: (k || "BK").slice(0, 3).toUpperCase(), key: k || "book", domain: "" };
}

function bookLogoSlugFromDomain(domain) {
  const d = String(domain || "").trim().toLowerCase();
  if (!d) return "";
  return d.replace(/[^a-z0-9]+/g, "_");
}

function bookFaviconUrlFromDomain(domain) {
  const d = String(domain || "").trim().toLowerCase();
  if (!d) return "";
  return `https://www.google.com/s2/favicons?domain=${encodeURIComponent(d)}&sz=64`;
}

/** Remote icon URLs first — `logos/*.png` is optional and usually absent in this repo. */
function bookLogoCandidateUrls(domain) {
  const d = String(domain || "").trim().toLowerCase();
  const slug = bookLogoSlugFromDomain(d);
  const urls = [];
  if (d) urls.push(`https://icons.duckduckgo.com/ip3/${encodeURIComponent(d)}.ico`);
  if (d) urls.push(`https://www.google.com/s2/favicons?domain=${encodeURIComponent(d)}&sz=64`);
  if (slug) urls.push(`logos/${slug}.png`);
  return urls;
}

/** Try candidate URLs in order; hide img and show fallbackEl when all fail. */
function attachBookLogoWithFallback(imgEl, fallbackEl, domain) {
  const urls = bookLogoCandidateUrls(domain);
  if (!urls.length) {
    imgEl.style.display = "none";
    if (fallbackEl) fallbackEl.style.display = "flex";
    return;
  }
  let idx = 0;
  imgEl.onerror = () => {
    idx += 1;
    if (idx >= urls.length) {
      imgEl.style.display = "none";
      if (fallbackEl) fallbackEl.style.display = "flex";
      return;
    }
    imgEl.src = urls[idx];
  };
  imgEl.src = urls[0];
}

function bookBadgeHtml(book) {
  const m = bookMeta(book);
  const slug = bookLogoSlugFromDomain(m.domain);
  const localLogo = slug ? `logos/${slug}.png` : "";
  const favicon = bookFaviconUrlFromDomain(m.domain);
  const imgSrc = favicon || localLogo;
  const onErr = favicon && localLogo
    ? `if(this.getAttribute('data-tried')!=='1'){this.setAttribute('data-tried','1');this.src='${localLogo}';}else{this.style.display='none';this.nextElementSibling.style.display='inline-flex';}`
    : "this.style.display='none';this.nextElementSibling.style.display='inline-flex';";
  const mark = imgSrc
    ? `<img class="book-logo-img-inline" src="${imgSrc}" alt="${m.label}" loading="lazy" data-tried="0" onerror="${onErr}" /><span class="book-logo-mark" style="display:none">${m.short}</span>`
    : `<span class="book-logo-mark">${m.short}</span>`;
  return `<span class="book-logo-pill" title="${m.label}">${mark}<span class="book-logo-name">${m.label}</span></span>`;
}

function modelAmericanFromProb(p) {
  if (!Number.isFinite(p) || p <= 0 || p >= 1) return "—";
  // formatAmerican hides |am|>250k; extreme model p (longshots / huge favorites) would read as a blank cell.
  const lo = 1 / 2501;
  const hi = 2500 / 2501;
  const pShow = clamp(p, lo, hi);
  return formatAmerican(americanFromImpliedProb(pShow));
}

/** Apply profit boost: decimal becomes 1 + (d−1)×(1 + boost%/100). */
function decimalWithProfitBoost(dec, boostPct) {
  if (!Number.isFinite(dec) || dec <= 1) return dec;
  if (!Number.isFinite(boostPct) || boostPct <= 0) return dec;
  const k = 1 + boostPct / 100;
  return 1 + (dec - 1) * k;
}

/** Boost % from +EV toolbar (0 if None). */
function evProfitBoostPctFromUi() {
  const sel = document.getElementById("ev-boost");
  const v = String(sel?.value || "none");
  if (v === "none") return 0;
  if (v === "custom") return Math.max(0, num(document.getElementById("ev-boost-pct")?.value, 0));
  if (v === "nosweat") return 25;
  const n = num(v, 0);
  return Number.isFinite(n) && n > 0 ? n : 0;
}

function syncEvBoostPctInputDisabled() {
  const sel = document.getElementById("ev-boost");
  const inp = document.getElementById("ev-boost-pct");
  if (!sel || !inp) return;
  inp.disabled = sel.value !== "custom";
}

/** Kelly stake in $: quarter Kelly, still capped at 1 unit (bankroll / 100). */
function evKellyDollarsFromDecimal(modelPct, dec, bankroll) {
  if (!Number.isFinite(modelPct) || modelPct <= 0 || !Number.isFinite(dec) || dec <= 1) return NaN;
  if (!Number.isFinite(bankroll) || bankroll <= 0) return NaN;
  const edge = modelPct * dec - 1;
  if (edge <= 0) return 0;
  const den = dec - 1;
  if (den <= 0) return NaN;
  const f = (edge / den) * 0.25;
  if (!Number.isFinite(f) || f <= 0) return NaN;
  const oneUnitDollars = bankroll / 100;
  return Math.min(Math.min(f, 0.25) * bankroll, oneUnitDollars);
}

/** Implied win prob from the arithmetic mean of posted decimal odds (raw consensus line). */
function consensusProbFromMeanDecimals(decimals) {
  const ds = (decimals || []).filter((d) => Number.isFinite(d) && d > 1);
  if (!ds.length) return NaN;
  const avgDec = ds.reduce((a, c) => a + c, 0) / ds.length;
  return avgDec > 1 ? 1 / avgDec : NaN;
}

const EV_DEVIG_STORAGE_KEY = "alphaCaddie_ev_devig_v1";

function loadEvDevigPrefs() {
  try {
    const raw = localStorage.getItem(EV_DEVIG_STORAGE_KEY);
    if (!raw) return defaultEvDevigPrefs();
    const j = JSON.parse(raw);
    const method = evDevigMethodValid(j.method) ? j.method : "none";
    let consensusMode = j.consensusMode;
    if (!["market", "single", "split"].includes(consensusMode)) {
      if (!j.books || !j.books.length) consensusMode = "market";
      else if (j.books.length === 1) consensusMode = "single";
      else consensusMode = "split";
    }
    const singleBook = sanitizeEvDevigBookKey(
      j.singleBook || (consensusMode === "single" && j.books?.[0]) || ""
    );
    const splitBooks = Array.isArray(j.splitBooks)
      ? sanitizeEvDevigBookList(j.splitBooks)
      : consensusMode === "split" && Array.isArray(j.books)
        ? sanitizeEvDevigBookList(j.books)
        : [];
    const weights =
      j.weights && typeof j.weights === "object"
        ? Object.fromEntries(
            Object.entries(j.weights)
              .map(([k, v]) => [sanitizeEvDevigBookKey(k), num(v, NaN)])
              .filter(([k]) => k)
          )
        : null;
    let books = null;
    let cm = consensusMode;
    if (cm === "single" && !singleBook) cm = "market";
    if (cm === "market") books = null;
    else if (cm === "single") books = singleBook ? [singleBook] : null;
    else {
      books = splitBooks.slice();
      /* Empty split list used to yield books=[] and filtered out every book (Market/Odds blank everywhere). */
      if (!books.length) {
        cm = "market";
        books = null;
      }
    }
    const bookWeights =
      weights && Object.keys(weights).some((k) => Number.isFinite(weights[k]) && weights[k] > 0) ? weights : null;
    return {
      method,
      books,
      bookWeights,
      consensusMode: cm,
      singleBook: cm === "single" ? singleBook : "",
      splitBooks: cm === "split" ? splitBooks : [],
    };
  } catch {
    return defaultEvDevigPrefs();
  }
}

function defaultEvDevigPrefs() {
  return {
    method: "none",
    books: null,
    bookWeights: null,
    consensusMode: "market",
    singleBook: "",
    splitBooks: [],
  };
}

function saveEvDevigPrefs(prefs) {
  try {
    localStorage.setItem(
      EV_DEVIG_STORAGE_KEY,
      JSON.stringify({
        method: prefs.method,
        consensusMode: prefs.consensusMode || "market",
        singleBook: prefs.singleBook || "",
        splitBooks: Array.isArray(prefs.splitBooks) ? prefs.splitBooks : [],
        weights: prefs.weights && typeof prefs.weights === "object" ? prefs.weights : null,
      })
    );
  } catch {
    /* ignore */
  }
}

function evBookAllowedInConsensus(bk, prefs, opts = {}) {
  const k = normalizeEvSportsbookKey(bk);
  if (k === "datagolf" && !opts.allowDatagolf) return false;
  if (!evSportsbookAllowed(k, opts)) return false;
  if (!prefs || prefs.books == null) return true;
  const selected = prefs.books.map(sanitizeEvDevigBookKey).filter(Boolean);
  if (selected.length === 0) return true;
  return selected.includes(k);
}

function evConsensusWeightForBook(bk, prefs) {
  if (!prefs || !prefs.bookWeights) return 1;
  const k = normalizeEvSportsbookKey(bk);
  const w = num(prefs.bookWeights[k], NaN);
  if (Number.isFinite(w) && w > 0) return w;
  return 0;
}

function evDevigAffectsEvAndKelly(prefs) {
  if (!prefs) return false;
  const method = evDevigMethodValid(prefs.method) ? prefs.method : "none";
  return method !== "none" || prefs.consensusMode === "single" || prefs.consensusMode === "split";
}

function sanitizeEvDevigBookKey(bookRaw) {
  const k = normalizeEvSportsbookKey(bookRaw);
  return evDevigSportsbookAllowed(k) ? k : "";
}

function sanitizeEvDevigBookList(list) {
  const out = [];
  const seen = new Set();
  for (const raw of Array.isArray(list) ? list : []) {
    const k = sanitizeEvDevigBookKey(raw);
    if (!k || seen.has(k)) continue;
    seen.add(k);
    out.push(k);
  }
  return out;
}

const EV_DEVIG_METHODS = [
  "none",
  "multiplicative",
  "additive",
  "power",
  "probit",
  "shin",
  "worst",
  "average",
];

function evDevigMethodValid(m) {
  return EV_DEVIG_METHODS.includes(String(m || ""));
}

/** Inverse normal CDF (bisection on normalCdf). */
function normalQuantile(p) {
  const pp = clamp(p, 1e-12, 1 - 1e-12);
  let lo = -10;
  let hi = 10;
  for (let i = 0; i < 80; i++) {
    const mid = (lo + hi) / 2;
    if (normalCdf(mid) < pp) lo = mid;
    else hi = mid;
  }
  return (lo + hi) / 2;
}

/**
 * Fair win probability for side 1 only (two-way, implied q1,q2 from decimals).
 * `shin`: quadratic-mean (√q normalization), common two-way Shin-family form.
 */
function devigFairP1TwoWay(q1, q2, method) {
  const p1 = clamp(q1, 1e-9, 1 - 1e-9);
  const p2 = clamp(q2, 1e-9, 1 - 1e-9);
  const s = p1 + p2;
  if (method === "multiplicative") {
    if (s <= 0) return NaN;
    return p1 / s;
  }
  if (method === "additive") {
    const R = s - 1;
    let a1 = p1 - R / 2;
    let a2 = p2 - R / 2;
    if (a1 >= 0 && a2 >= 0) return a1;
    a1 = Math.max(0, a1);
    a2 = Math.max(0, a2);
    const n = a1 + a2;
    return n > 0 ? a1 / n : NaN;
  }
  if (method === "power") {
    if (s <= 1 + 1e-12) return p1 / s;
    const f = (g) => p1 ** g + p2 ** g - 1;
    let lo = 1;
    let hi = 2;
    while (f(hi) > 0 && hi < 512) hi *= 1.25;
    if (f(hi) > 0) return p1 / s;
    for (let i = 0; i < 70; i++) {
      const m = (lo + hi) / 2;
      if (f(m) > 0) lo = m;
      else hi = m;
    }
    const g = (lo + hi) / 2;
    return p1 ** g;
  }
  if (method === "probit") {
    const z1 = normalQuantile(p1);
    const z2 = normalQuantile(p2);
    if (!Number.isFinite(z1) || !Number.isFinite(z2)) return NaN;
    const sumAt = (d) => normalCdf(z1 - d) + normalCdf(z2 - d);
    if (sumAt(0) <= 1 + 1e-9) return p1 / s;
    let hiD = 0.5;
    while (sumAt(hiD) > 1 && hiD < 40) hiD *= 2;
    if (sumAt(hiD) > 1) return p1 / s;
    let loD = 0;
    for (let i = 0; i < 70; i++) {
      const m = (loD + hiD) / 2;
      if (sumAt(m) > 1) loD = m;
      else hiD = m;
    }
    const d = (loD + hiD) / 2;
    return normalCdf(z1 - d);
  }
  if (method === "shin") {
    const r1 = Math.sqrt(p1);
    const r2 = Math.sqrt(p2);
    const rs = r1 + r2;
    return rs > 0 ? r1 / rs : NaN;
  }
  return NaN;
}

const EV_DEVIG_CORE_METHODS = ["multiplicative", "additive", "power", "probit", "shin"];

/** Fair prob for `sideKey` after devig; supports average & worst over core methods. */
function devigFairForSide(q1, q2, method, sideKey) {
  const wantP1 = sideKey === "p1";
  if (method === "average") {
    const fp1s = EV_DEVIG_CORE_METHODS.map((m) => devigFairP1TwoWay(q1, q2, m)).filter((x) =>
      Number.isFinite(x)
    );
    if (!fp1s.length) return NaN;
    const m1 = fp1s.reduce((a, c) => a + c, 0) / fp1s.length;
    return wantP1 ? m1 : 1 - m1;
  }
  if (method === "worst") {
    const vals = EV_DEVIG_CORE_METHODS.map((m) => {
      const fp1 = devigFairP1TwoWay(q1, q2, m);
      if (!Number.isFinite(fp1)) return NaN;
      return wantP1 ? fp1 : 1 - fp1;
    }).filter((x) => Number.isFinite(x));
    if (!vals.length) return NaN;
    return Math.min(...vals);
  }
  const fp1 = devigFairP1TwoWay(q1, q2, method);
  if (!Number.isFinite(fp1)) return NaN;
  return wantP1 ? fp1 : 1 - fp1;
}

/**
 * Consensus implied prob for matchup side `p1` or `p2`.
 * `none`: weighted mean of posted decimals → implied from mean decimal.
 * Other methods: two-way devig per book, then weighted mean of fair win probs.
 */
/**
 * Multiplicative “devig” fair prob for one side of a 3-way (3-ball) market per book, then weighted mean.
 */
function matchupConsensusThreeWaySide(oddsObj, sideKey, prefs) {
  const want = String(sideKey || "").toLowerCase();
  if (!["p1", "p2", "p3"].includes(want)) return NaN;
  const items = [];
  for (const bk of Object.keys(oddsObj || {})) {
    if (!evBookAllowedInConsensus(bk, prefs, { allowDatagolf: true })) continue;
    const wB = evConsensusWeightForBook(bk, prefs);
    if (prefs.bookWeights && wB <= 0) continue;
    const pack = oddsObj[bk];
    const { d1, d2, d3 } = matchupOddsThreeWayFromPack(pack);
    if (!Number.isFinite(d1) || d1 <= 1 || !Number.isFinite(d2) || d2 <= 1 || !Number.isFinite(d3) || d3 <= 1) continue;
    const q1 = 1 / d1;
    const q2 = 1 / d2;
    const q3 = 1 / d3;
    const s = q1 + q2 + q3;
    if (s <= 0) continue;
    const pFair = want === "p1" ? q1 / s : want === "p2" ? q2 / s : q3 / s;
    items.push({ bk, v: pFair, w: wB });
  }
  if (!items.length) return NaN;
  const tw = items.reduce((sum, it) => sum + it.w, 0);
  if (tw <= 0) return NaN;
  return items.reduce((sum, it) => sum + it.w * it.v, 0) / tw;
}

/**
 * Implied win probability for a matchup side. Uses +EV devig / book prefs when possible; if those
 * exclude every book that posted the line (e.g. split set to a book with no price), falls back to
 * all allowed books, then to any non–DataGolf book with valid decimals.
 */
function matchupMarketImpliedProbSide(rawOdds, filteredEvOdds, sideKey, prefs, isThree = false) {
  if (isThree) {
    let p = matchupConsensusThreeWaySide(filteredEvOdds, sideKey, prefs);
    if (Number.isFinite(p)) return p;
    const widePrefs = {
      ...prefs,
      books: null,
      bookWeights: null,
      consensusMode: "market",
    };
    p = matchupConsensusThreeWaySide(filteredEvOdds, sideKey, widePrefs);
    if (Number.isFinite(p)) return p;
    return matchupAnalysisMarketProbSide(matchupAnalysisOddsWithoutDataGolf(rawOdds || {}), sideKey, true);
  }
  let p = matchupConsensusSide(filteredEvOdds, sideKey, prefs);
  if (Number.isFinite(p)) return p;
  const widePrefs = {
    ...prefs,
    books: null,
    bookWeights: null,
    consensusMode: "market",
  };
  p = matchupConsensusSide(filteredEvOdds, sideKey, widePrefs);
  if (Number.isFinite(p)) return p;
  return matchupAnalysisMarketProbSide(matchupAnalysisOddsWithoutDataGolf(rawOdds || {}), sideKey, false);
}

function matchupConsensusSide(oddsObj, sideKey, prefs) {
  const items = [];
  const method = evDevigMethodValid(prefs.method) ? prefs.method : "none";
  const want = String(sideKey || "").toLowerCase();
  for (const bk of Object.keys(oddsObj || {})) {
    if (!evBookAllowedInConsensus(bk, prefs, { allowDatagolf: true })) continue;
    const wB = evConsensusWeightForBook(bk, prefs);
    if (prefs.bookWeights && wB <= 0) continue;
    const pack = oddsObj[bk];
    const { d1, d2 } = matchupOddsTwoWayFromPack(pack);
    if (!Number.isFinite(d1) || d1 <= 1 || !Number.isFinite(d2) || d2 <= 1) continue;
    if (method !== "none") {
      const q1 = 1 / d1;
      const q2 = 1 / d2;
      const pFair = devigFairForSide(q1, q2, method, sideKey);
      if (!Number.isFinite(pFair)) continue;
      items.push({ bk, v: pFair, w: wB });
    } else {
      const q1 = 1 / d1;
      const q2 = 1 / d2;
      const s = q1 + q2;
      if (s <= 0) continue;
      const imp = want === "p1" ? q1 / s : q2 / s;
      items.push({ bk, v: imp, w: wB });
    }
  }
  if (!items.length) return NaN;
  const tw = items.reduce((s, it) => s + it.w, 0);
  if (tw <= 0) return NaN;
  return items.reduce((s, it) => s + it.w * it.v, 0) / tw;
}

function matchupMarketProbWithFallback(filteredOddsObj, sideKey, prefs, isThree = false) {
  const p = isThree
    ? matchupConsensusThreeWaySide(filteredOddsObj, sideKey, prefs)
    : matchupConsensusSide(filteredOddsObj, sideKey, prefs);
  if (Number.isFinite(p)) return p;
  return NaN;
}

function matchupAnalysisOddsWithoutDataGolf(oddsObj) {
  const out = {};
  if (!oddsObj || typeof oddsObj !== "object") return out;
  for (const [bk, pack] of Object.entries(oddsObj)) {
    const k = normalizeEvSportsbookKey(bk);
    if (!k || k === "datagolf") continue;
    if (!pack || typeof pack !== "object") continue;
    out[k] = pack;
  }
  return out;
}

function matchupAnalysisMarketProbSide(oddsObj, sideKey, isThree = false) {
  const want = String(sideKey || "").toLowerCase();
  const vals = [];
  for (const pack of Object.values(oddsObj || {})) {
    if (!pack || typeof pack !== "object") continue;
    if (isThree) {
      if (!["p1", "p2", "p3"].includes(want)) continue;
      const { d1, d2, d3 } = matchupOddsThreeWayFromPack(pack);
      if (!Number.isFinite(d1) || d1 <= 1 || !Number.isFinite(d2) || d2 <= 1 || !Number.isFinite(d3) || d3 <= 1) continue;
      const q1 = 1 / d1;
      const q2 = 1 / d2;
      const q3 = 1 / d3;
      const s = q1 + q2 + q3;
      if (s > 0) vals.push(want === "p1" ? q1 / s : want === "p2" ? q2 / s : q3 / s);
      continue;
    }
    const { d1, d2 } = matchupOddsTwoWayFromPack(pack);
    if (!Number.isFinite(d1) || d1 <= 1 || !Number.isFinite(d2) || d2 <= 1) continue;
    const q1 = 1 / d1;
    const q2 = 1 / d2;
    const s = q1 + q2;
    if (s > 0) vals.push(want === "p1" ? q1 / s : q2 / s);
  }
  return vals.length ? vals.reduce((a, c) => a + c, 0) / vals.length : NaN;
}

function bestBookDecimalForSideWithFallback(oddsObj, sideKey, prefs, opts = {}) {
  const filtered = filterOddsObjectForEvSportsbooks(oddsObj || {}, opts);
  const best = bestBookDecimalForSide(filtered, sideKey, opts);
  if (Number.isFinite(best.dec) && best.dec > 1) return best;
  return best;
}

/** Weighted mean of posted decimals → consensus implied prob (outrights). */
function outrightConsensusProbFromBooks(bookDecItems, prefs) {
  let tw = 0;
  let s = 0;
  for (const { bk, dec } of bookDecItems) {
    if (!evBookAllowedInConsensus(bk, prefs)) continue;
    if (!Number.isFinite(dec) || dec <= 1) continue;
    const w = prefs.bookWeights ? evConsensusWeightForBook(bk, prefs) : 1;
    if (prefs.bookWeights && w <= 0) continue;
    s += w * dec;
    tw += w;
  }
  if (tw <= 0) return NaN;
  const avgDec = s / tw;
  return avgDec > 1 ? 1 / avgDec : NaN;
}

function draftKingsFinishOddsByDgIndex() {
  const markets = ["win", "top_5", "top_10", "top_20"];
  const byId = new Map();
  for (const mk of markets) {
    const pack = DATA.outrights?.[mk];
    for (const row of Array.isArray(pack?.rows) ? pack.rows : []) {
      const id = Math.round(num(row.dg_id, NaN));
      if (!Number.isFinite(id)) continue;
      const pct = impliedPctFromOutrightBookField(row.draftkings);
      if (!Number.isFinite(pct) || pct <= 0) continue;
      const p = outrightFeedPlaceholderProbNaN(pct / 100, mk, "draftkings");
      if (!Number.isFinite(p) || p <= 0 || p >= 1) continue;
      const am = americanFromImpliedProb(p);
      if (!Number.isFinite(am)) continue;
      const cur = byId.get(id) || {};
      cur[mk] = { p, am: Math.round(am) };
      byId.set(id, cur);
    }
  }
  return byId;
}

function evDevigSortedBookKeys() {
  return Object.keys(SPORTSBOOK_META)
    .filter((k) => evDevigSportsbookAllowed(k))
    .sort((a, b) => bookMeta(a).label.localeCompare(bookMeta(b).label));
}

function clearEvDevigTileSelection() {
  document.getElementById("ev-devig-single-key").value = "";
  document.querySelectorAll("#ev-devig-tiles .ev-devig-tile").forEach((el) => {
    el.classList.remove("selected");
    el.setAttribute("aria-pressed", "false");
  });
}

function selectEvDevigTile(bookKey) {
  const k = String(bookKey || "").toLowerCase();
  clearEvDevigTileSelection();
  if (!k) return;
  const t = document.querySelector(`#ev-devig-tiles .ev-devig-tile[data-book="${k}"]`);
  if (t) {
    t.classList.add("selected");
    t.setAttribute("aria-pressed", "true");
  }
  document.getElementById("ev-devig-single-key").value = k;
}

function filterEvDevigBySearch(q) {
  const needle = String(q || "").trim().toLowerCase();
  document.querySelectorAll("#ev-devig-tiles .ev-devig-tile").forEach((el) => {
    const hay = String(el.dataset.search || "").toLowerCase();
    el.hidden = Boolean(needle) && !hay.includes(needle);
  });
  document.querySelectorAll("#ev-devig-split-list .ev-devig-split-row").forEach((el) => {
    const hay = String(el.dataset.search || "").toLowerCase();
    el.hidden = Boolean(needle) && !hay.includes(needle);
  });
}

function readEvDevigSplitForm() {
  const picked = [];
  for (const row of document.querySelectorAll("#ev-devig-split-list .ev-devig-split-row")) {
    const cb = row.querySelector(".ev-devig-split-cb");
    if (!cb?.checked) continue;
    const bk = sanitizeEvDevigBookKey(cb.value);
    if (!bk) continue;
    picked.push({
      bk,
      pct: num(row.querySelector(".ev-devig-split-pct")?.value, NaN),
    });
  }
  if (!picked.length) return { books: [], weights: null };
  const allPct = picked.every((x) => Number.isFinite(x.pct) && x.pct > 0);
  if (allPct) {
    return {
      books: picked.map((x) => x.bk),
      weights: Object.fromEntries(picked.map((x) => [x.bk, x.pct])),
    };
  }
  return { books: picked.map((x) => x.bk), weights: null };
}

function initEvDevigUiOnce() {
  const tiles = document.getElementById("ev-devig-tiles");
  const list = document.getElementById("ev-devig-split-list");
  if (!tiles || !list || tiles.dataset.inited === "1") return;
  tiles.dataset.inited = "1";
  for (const k of evDevigSortedBookKeys()) {
    const m = bookMeta(k);
    const search = `${m.label} ${m.short} ${k}`.toLowerCase();
    const btn = document.createElement("button");
    btn.type = "button";
    btn.className = "ev-devig-tile";
    btn.dataset.book = k;
    btn.dataset.search = search;
    btn.setAttribute("aria-pressed", "false");
    btn.title = m.label;
    const wrap = document.createElement("span");
    wrap.className = "ev-devig-tile-logo-wrap";
    const imgEl = document.createElement("img");
    imgEl.className = "ev-devig-tile-logo";
    imgEl.alt = "";
    imgEl.loading = "lazy";
    const fb = document.createElement("span");
    fb.className = "ev-devig-tile-fallback";
    fb.textContent = m.short;
    fb.style.display = "none";
    wrap.appendChild(imgEl);
    wrap.appendChild(fb);
    attachBookLogoWithFallback(imgEl, fb, m.domain);
    const short = document.createElement("span");
    short.className = "ev-devig-tile-short";
    short.textContent = m.short;
    btn.appendChild(wrap);
    btn.appendChild(short);
    tiles.appendChild(btn);
    const row = document.createElement("div");
    row.className = "ev-devig-split-row";
    row.dataset.book = k;
    row.dataset.search = search;
    const lab = document.createElement("label");
    lab.className = "ev-devig-split-check";
    const cb = document.createElement("input");
    cb.type = "checkbox";
    cb.className = "ev-devig-split-cb";
    cb.value = k;
    const logoSpan = document.createElement("span");
    logoSpan.className = "ev-devig-split-logo";
    const sImg = document.createElement("img");
    sImg.alt = "";
    sImg.loading = "lazy";
    const sFb = document.createElement("span");
    sFb.className = "ev-devig-tile-fallback";
    sFb.textContent = m.short;
    sFb.style.display = "none";
    logoSpan.appendChild(sImg);
    logoSpan.appendChild(sFb);
    attachBookLogoWithFallback(sImg, sFb, m.domain);
    const name = document.createElement("span");
    name.className = "ev-devig-split-name";
    name.textContent = m.label;
    lab.appendChild(cb);
    lab.appendChild(logoSpan);
    lab.appendChild(name);
    const pct = document.createElement("input");
    pct.type = "text";
    pct.className = "ev-devig-split-pct";
    pct.inputMode = "decimal";
    pct.placeholder = "%";
    pct.dataset.book = k;
    row.appendChild(lab);
    row.appendChild(pct);
    list.appendChild(row);
  }
  tiles.addEventListener("click", (ev) => {
    const t = ev.target.closest(".ev-devig-tile");
    if (!t || !tiles.contains(t)) return;
    const bk = t.dataset.book;
    document.querySelectorAll("#ev-devig-tiles .ev-devig-tile").forEach((el) => {
      el.classList.remove("selected");
      el.setAttribute("aria-pressed", "false");
    });
    t.classList.add("selected");
    t.setAttribute("aria-pressed", "true");
    document.getElementById("ev-devig-single-key").value = bk || "";
    const rs = document.getElementById("ev-cm-single");
    if (rs) rs.checked = true;
  });
}

function syncEvDevigFormFromPrefs() {
  const p = loadEvDevigPrefs();
  const methodEl = document.getElementById("ev-devig-method");
  if (methodEl) methodEl.value = p.method;
  if (p.consensusMode === "market") {
    document.getElementById("ev-cm-market").checked = true;
    clearEvDevigTileSelection();
  } else if (p.consensusMode === "single") {
    document.getElementById("ev-cm-single").checked = true;
    const sk = p.singleBook || (p.books && p.books[0]) || "";
    selectEvDevigTile(sk);
  } else {
    document.getElementById("ev-cm-split").checked = true;
    clearEvDevigTileSelection();
  }
  const splitSet = new Set(p.consensusMode === "split" ? p.splitBooks || [] : []);
  document.querySelectorAll(".ev-devig-split-cb").forEach((cb) => {
    cb.checked = splitSet.has(String(cb.value).toLowerCase());
  });
  document.querySelectorAll(".ev-devig-split-pct").forEach((inp) => {
    const bk = String(inp.dataset.book || "").toLowerCase();
    if (!splitSet.has(bk)) {
      inp.value = "";
      return;
    }
    const w = p.bookWeights && Number.isFinite(p.bookWeights[bk]) ? p.bookWeights[bk] : NaN;
    inp.value = Number.isFinite(w) && w > 0 ? String(w) : "";
  });
}

function readEvDevigFormToPrefs() {
  const method = String(document.getElementById("ev-devig-method")?.value || "none");
  const m = evDevigMethodValid(method) ? method : "none";
  const mode =
    (document.querySelector('input[name="ev-consensus-mode"]:checked') || {}).value || "market";
  if (mode === "market") {
    return { method: m, consensusMode: "market", singleBook: "", splitBooks: [], weights: null };
  }
  if (mode === "single") {
    const sk = sanitizeEvDevigBookKey(document.getElementById("ev-devig-single-key")?.value || "");
    if (!sk) return { method: m, consensusMode: "market", singleBook: "", splitBooks: [], weights: null };
    return { method: m, consensusMode: "single", singleBook: sk, splitBooks: [], weights: null };
  }
  const sp = readEvDevigSplitForm();
  if (!sp.books.length) return { method: m, consensusMode: "market", singleBook: "", splitBooks: [], weights: null };
  return { method: m, consensusMode: "split", singleBook: "", splitBooks: sp.books, weights: sp.weights };
}

function openEvDevigDialog() {
  initEvDevigUiOnce();
  syncEvDevigFormFromPrefs();
  const s = document.getElementById("ev-devig-search");
  if (s) s.value = "";
  filterEvDevigBySearch("");
  document.getElementById("ev-devig-dialog")?.showModal();
}

function closeEvDevigDialog() {
  document.getElementById("ev-devig-dialog")?.close();
}

function openEvHelpDialog() {
  document.getElementById("ev-help-dialog")?.showModal();
}

function closeEvHelpDialog() {
  document.getElementById("ev-help-dialog")?.close();
}

function collectUnifiedEvRows() {
  const rows = [];
  const devigPrefs = loadEvDevigPrefs();
  const r = getModelRoundForEv();
  const elim = dgIdsEliminatedFromEventPostCut();
  const mpack = DATA.matchups || {};
  for (const mk of ["tournament_matchups", "round_matchups", "3_balls"]) {
    const list = mpack[mk] && mpack[mk].match_list;
    if (!Array.isArray(list)) continue;
    const marketLabel =
      mk === "tournament_matchups" ? "Tournament Matchups" : mk === "round_matchups" ? "Round Matchups" : "3 Balls";
    for (const m of list) {
      const id1 = Math.round(num(m.p1_dg_id, NaN));
      const id2 = Math.round(num(m.p2_dg_id, NaN));
      const id3 = Math.round(num(m.p3_dg_id, NaN));
      const row1 = projectionPlayerRowForModelByIdOrName(id1, m.p1_player_name, r);
      const row2 = projectionPlayerRowForModelByIdOrName(id2, m.p2_player_name, r);
      const row3 = projectionPlayerRowForModelByIdOrName(id3, m.p3_player_name, r);
      const mu1 = effectiveMuSg(row1, id1, mk);
      const mu2 = effectiveMuSg(row2, id2, mk);
      const mu3 = effectiveMuSg(row3, id3, mk);
      const isThreeBall = mk === "3_balls" && Number.isFinite(id3) && id3 > 0;
      if (elim.size && (elim.has(id1) || elim.has(id2) || (isThreeBall && elim.has(id3)))) continue;
      const oddsEv = filterOddsObjectForEvSportsbooks(m.odds || {}, { allowDatagolf: true });
      if (isThreeBall) {
        const [tp1, tp2, tp3] = threeBallModelProbsLiveBlended(mu1, mu2, mu3, row1, row2, row3);
        const b1 = bestBookDecimalForSideWithFallback(oddsEv, "p1", devigPrefs);
        const b2 = bestBookDecimalForSideWithFallback(oddsEv, "p2", devigPrefs);
        const b3 = bestBookDecimalForSideWithFallback(oddsEv, "p3", devigPrefs);
        const n1 = displayGolferName(String(m.p1_player_name || ""));
        const n2 = displayGolferName(String(m.p2_player_name || ""));
        const n3 = displayGolferName(String(m.p3_player_name || ""));
        const mp1 = matchupConsensusThreeWaySide(oddsEv, "p1", devigPrefs);
        const mp2 = matchupConsensusThreeWaySide(oddsEv, "p2", devigPrefs);
        const mp3 = matchupConsensusThreeWaySide(oddsEv, "p3", devigPrefs);
        rows.push({
          golfer: n1,
          market: marketLabel,
          bet: `3-ball vs ${n2} & ${n3}`,
          modelPct: tp1,
          modelEv: Number.isFinite(b1.dec) ? tp1 * b1.dec - 1 : NaN,
          bestBook: b1.book,
          bestBookOdds: Number.isFinite(b1.dec) ? formatAmerican(americanFromDecimal(b1.dec)) : "—",
          bestDec: b1.dec,
          consensusP: mp1,
        });
        rows.push({
          golfer: n2,
          market: marketLabel,
          bet: `3-ball vs ${n1} & ${n3}`,
          modelPct: tp2,
          modelEv: Number.isFinite(b2.dec) ? tp2 * b2.dec - 1 : NaN,
          bestBook: b2.book,
          bestBookOdds: Number.isFinite(b2.dec) ? formatAmerican(americanFromDecimal(b2.dec)) : "—",
          bestDec: b2.dec,
          consensusP: mp2,
        });
        rows.push({
          golfer: n3,
          market: marketLabel,
          bet: `3-ball vs ${n1} & ${n2}`,
          modelPct: tp3,
          modelEv: Number.isFinite(b3.dec) ? tp3 * b3.dec - 1 : NaN,
          bestBook: b3.book,
          bestBookOdds: Number.isFinite(b3.dec) ? formatAmerican(americanFromDecimal(b3.dec)) : "—",
          bestDec: b3.dec,
          consensusP: mp3,
        });
        continue;
      }
      const p1 = matchupWinProbLiveBlended(mu1, mu2, mk, row1, row2);
      const b1 = bestBookDecimalForSideWithFallback(oddsEv, "p1", devigPrefs);
      const b2 = bestBookDecimalForSideWithFallback(oddsEv, "p2", devigPrefs);
      const modelEv1 = Number.isFinite(b1.dec) ? p1 * b1.dec - 1 : NaN;
      const modelEv2 = Number.isFinite(b2.dec) ? (1 - p1) * b2.dec - 1 : NaN;
      const marketP1 = matchupConsensusSide(oddsEv, "p1", devigPrefs);
      const marketP2 = matchupConsensusSide(oddsEv, "p2", devigPrefs);
      rows.push({
        golfer: displayGolferName(String(m.p1_player_name || "")),
        market: marketLabel,
        bet: `vs ${displayGolferName(String(m.p2_player_name || ""))}`,
        modelPct: p1,
        modelEv: modelEv1,
        bestBook: b1.book,
        bestBookOdds: Number.isFinite(b1.dec) ? formatAmerican(americanFromDecimal(b1.dec)) : "—",
        bestDec: b1.dec,
        consensusP: marketP1,
      });
      rows.push({
        golfer: displayGolferName(String(m.p2_player_name || "")),
        market: marketLabel,
        bet: `vs ${displayGolferName(String(m.p1_player_name || ""))}`,
        modelPct: 1 - p1,
        modelEv: modelEv2,
        bestBook: b2.book,
        bestBookOdds: Number.isFinite(b2.dec) ? formatAmerican(americanFromDecimal(b2.dec)) : "—",
        bestDec: b2.dec,
        consensusP: marketP2,
      });
    }
  }
  const opack = DATA.outrights || {};
  const rOut = getModelRoundForEv();
  const evLbOpts = outrightEvLiveLeaderboardModelEnabled() ? { evLiveLeaderboard: true } : {};
  if (evLbOpts.evLiveLeaderboard) ensureOutrightEvLiveLeaderboardProbCache();
  for (const mk of ["win", "top_5", "top_10", "top_20", "make_cut", "mc", "frl"]) {
    const pack = opack[mk];
    if (!pack || !Array.isArray(pack.rows)) continue;
    const books = Array.isArray(pack.bookKeys)
      ? pack.bookKeys.filter((k) => k && k !== "datagolf" && k !== "dg_model" && evSportsbookAllowed(k))
      : [];
    for (const row of pack.rows) {
      const id = Math.round(num(row.dg_id, NaN));
      if (elim.size && elim.has(id) && mk !== "make_cut" && mk !== "mc") continue;
      let modelP = modelProbOutrightFromRowOrProjections(row, mk, evLbOpts);
      const modelOk = Number.isFinite(modelP) && modelP > 0;
      const decItems = [];
      for (const bk of books) {
        const bkNorm = normalizeEvSportsbookKey(bk);
        if (!evBookAllowedInConsensus(bkNorm, devigPrefs)) continue;
        const pct = impliedPctFromOutrightBookField(row[bk] ?? row[bkNorm]);
        if (!Number.isFinite(pct) || pct <= 0) continue;
        const pp = pct / 100;
        if (pp <= 0 || pp >= 1) continue;
        decItems.push({ bk: bkNorm, dec: 1 / pp });
      }
      const marketP = outrightConsensusProbFromBooks(decItems, devigPrefs);
      const marketLabel =
        mk === "win"
          ? "Outright Win"
          : mk === "top_5"
            ? "Outright Top 5"
            : mk === "top_10"
              ? "Outright Top 10"
              : mk === "top_20"
                ? "Outright Top 20"
                : mk === "make_cut"
                  ? "Outright Make Cut"
                  : mk === "mc"
                    ? "Outright Miss Cut"
                    : "First Round Leader";
      const betLabel =
        mk === "mc" ? "Miss Cut" : mk === "make_cut" ? "Make Cut" : mk === "frl" ? "FRL" : mk.replace("_", " ").toUpperCase();
      /** One row per golfer × market: keep only the best posted price (highest decimal = best for the bettor). */
      const byBook = [];
      for (const bk of books) {
        const bkNorm = normalizeEvSportsbookKey(bk);
        const pct = impliedPctFromOutrightBookField(row[bk] ?? row[bkNorm]);
        if (!Number.isFinite(pct) || pct <= 0 || !modelOk) continue;
        const pBook = pct / 100;
        if (!Number.isFinite(pBook) || pBook <= 0 || pBook >= 1) continue;
        const modelEv = outrightEvFromModelAndBook(modelP, pBook, mk);
        if (!Number.isFinite(modelEv)) continue;
        const am = Math.round(americanFromImpliedProb(pBook));
        const dec = 1 / pBook;
        byBook.push({ bkNorm, dec, pBook, modelEv, am });
      }
      if (!byBook.length) continue;
      byBook.sort((a, b) => b.dec - a.dec || String(a.bkNorm).localeCompare(String(b.bkNorm)));
      const best = byBook[0];
      rows.push({
        golfer: displayGolferName(String(row.player_name || "")),
        market: marketLabel,
        bet: betLabel,
        modelPct: modelP,
        modelEv: best.modelEv,
        bestBook: best.bkNorm,
        bestBookOdds: Number.isFinite(best.am) ? formatAmerican(best.am) : "—",
        bestDec: best.dec,
        consensusP: marketP,
      });
    }
  }
  return rows;
}

function fillEvFilters(rows) {
  const g = document.getElementById("ev-filter-golfer");
  const m = document.getElementById("ev-filter-market");
  const booksSel = document.getElementById("ev-filter-books-select");
  if (!g || !m || !booksSel) return;
  const gPrev = g.value;
  const mPrev = m.value;
  const booksPrevVal = String(booksSel.value || "").trim();
  const booksPrevKey = booksPrevVal ? normalizeEvSportsbookKey(booksPrevVal) : "";
  const gSet = new Set(rows.map((r) => r.golfer).filter(Boolean));
  const mSet = new Set(rows.map((r) => r.market).filter(Boolean));
  for (const label of ["Tournament Matchups", "Round Matchups", "3 Balls"]) mSet.add(label);
  const bSet = new Set([...EV_ALLOWED_SPORTSBOOKS]);
  for (const r of rows) {
    const bk = normalizeEvSportsbookKey(r.bestBook);
    if (evSportsbookAllowed(bk)) bSet.add(bk);
  }
  const refill = (sel, vals) => {
    sel.innerHTML = '<option value="">All</option>';
    [...vals].sort((a, c) => String(a).localeCompare(String(c))).forEach((v) => {
      const op = document.createElement("option");
      op.value = String(v);
      op.textContent = sel === g ? String(v) : String(v);
      sel.appendChild(op);
    });
  };
  refill(g, gSet);
  refill(m, mSet);
  booksSel.innerHTML = "";
  const allBooksOp = document.createElement("option");
  allBooksOp.value = "";
  allBooksOp.textContent = "All books";
  booksSel.appendChild(allBooksOp);
  for (const v of [...bSet].sort((a, c) => String(a).localeCompare(String(c)))) {
    const op = document.createElement("option");
    op.value = String(v);
    op.textContent = bookMeta(v).label;
    booksSel.appendChild(op);
  }
  if (booksPrevKey) {
    const match = [...booksSel.options].find(
      (o) => o.value && normalizeEvSportsbookKey(o.value) === booksPrevKey,
    );
    booksSel.value = match ? match.value : "";
  } else {
    booksSel.value = "";
  }
  if ([...g.options].some((o) => o.value === gPrev)) g.value = gPrev;
  if ([...m.options].some((o) => o.value === mPrev)) m.value = mPrev;
  refreshGolferComboboxFromSelect("ev-filter-golfer");
}

/** +EV “Best books” filter: `null` = all books; else row must match that book (single-select). */
function selectedEvFilterBookSet() {
  const sel = document.getElementById("ev-filter-books-select");
  if (!sel) return null;
  const raw = String(sel.value || "").trim();
  if (!raw) return null;
  const k = normalizeEvSportsbookKey(raw);
  if (!k) return null;
  return new Set([k]);
}

let evSort = { key: "model_ev", dir: -1 };
let evSortInited = false;

function updateEvSortIndicators() {
  const table = document.getElementById("table-ev");
  if (!table) return;
  table.querySelectorAll("thead th.sortable").forEach((th) => {
    const key = String(th.dataset.sortKey || "");
    const up = th.querySelector(".sort-up");
    const dn = th.querySelector(".sort-down");
    if (up) up.classList.toggle("active", key === evSort.key && evSort.dir > 0);
    if (dn) dn.classList.toggle("active", key === evSort.key && evSort.dir < 0);
  });
}

function initEvTableSortOnce() {
  if (evSortInited) return;
  const table = document.getElementById("table-ev");
  if (!table) return;
  const keyOrder = [
    "model_ev",
    "kelly",
    "best_book",
    "golfer",
    "model",
    "consensus",
    "implied",
    "delta",
    "market",
    "bet",
  ];
  const ths = table.querySelectorAll("thead th");
  ths.forEach((th, idx) => {
    const key = keyOrder[idx] || "";
    if (!key) return;
    th.classList.add("sortable");
    th.dataset.sortKey = key;
    if (!th.querySelector(".sort-ind")) {
      const s = document.createElement("span");
      s.className = "sort-ind";
      s.innerHTML = '<span class="sort-up">▲</span><span class="sort-down">▼</span>';
      th.appendChild(s);
    }
  });
  table.querySelector("thead")?.addEventListener("click", (ev) => {
    const th = ev.target.closest("th.sortable");
    if (!th || !table.contains(th)) return;
    const key = String(th.dataset.sortKey || "");
    if (!key) return;
    if (evSort.key === key) evSort.dir *= -1;
    else {
      evSort.key = key;
      evSort.dir = key === "golfer" || key === "market" || key === "bet" || key === "best_book" ? 1 : -1;
    }
    buildEvTable();
  });
  evSortInited = true;
  updateEvSortIndicators();
}

function evSortValue(row, key) {
  if (key === "model_ev") return row._modelEv;
  if (key === "kelly") return row._kelly;
  if (key === "best_book") return String(row.bestBook || "");
  if (key === "golfer") return String(row.golfer || "");
  if (key === "model") return row.modelPct;
  if (key === "consensus") return row.consensusP;
  if (key === "implied") return row._bookImp;
  if (key === "delta") return row._deltaPct;
  if (key === "market") return String(row.market || "");
  if (key === "bet") return String(row.bet || "");
  return row._modelEv;
}

function compareEvRows(a, b, key, dir) {
  const va = evSortValue(a, key);
  const vb = evSortValue(b, key);
  if (typeof va === "string" || typeof vb === "string") {
    return dir * String(va || "").localeCompare(String(vb || ""));
  }
  const na = Number.isFinite(va) ? va : dir > 0 ? Number.POSITIVE_INFINITY : Number.NEGATIVE_INFINITY;
  const nb = Number.isFinite(vb) ? vb : dir > 0 ? Number.POSITIVE_INFINITY : Number.NEGATIVE_INFINITY;
  if (na !== nb) return dir * (na - nb);
  return String(a.golfer || "").localeCompare(String(b.golfer || ""));
}

function buildEvTable() {
  const tbody = document.querySelector("#table-ev tbody");
  if (!tbody) return;
  syncEvBoostPctInputDisabled();
  const rows = collectUnifiedEvRows();
  fillEvFilters(rows);
  const gSearchEl = document.getElementById("ev-filter-golfer-search");
  const gSelEl = document.getElementById("ev-filter-golfer");
  const gTyping = String(gSearchEl?.value || "").trim();
  const gCommitted = String(gSelEl?.value || "").trim();
  const g =
    document.activeElement === gSearchEl
      ? gTyping || gCommitted
      : gCommitted || gTyping;
  const m = String(document.getElementById("ev-filter-market")?.value || "");
  const bookFilter = selectedEvFilterBookSet();
  const bankroll = num(document.getElementById("ev-bankroll")?.value, 1000);
  const boostPct = evProfitBoostPctFromUi();
  const devigPrefs = loadEvDevigPrefs();
  const useDevigProbForEv = evDevigAffectsEvAndKelly(devigPrefs);
  const evProbForRow = (r) => {
    const pDevig = num(r.consensusP, NaN);
    if (useDevigProbForEv && Number.isFinite(pDevig) && pDevig > 0 && pDevig < 1) return pDevig;
    return r.modelPct;
  };
  const modelEvWithBoost = (r) => {
    const d0 = num(r.bestDec, NaN);
    const d = decimalWithProfitBoost(d0, boostPct);
    const p = evProbForRow(r);
    return Number.isFinite(d) && d > 1 && Number.isFinite(p) ? p * d - 1 : NaN;
  };
  const gLow = g.toLowerCase();
  const maxOddsRaw = String(document.getElementById("ev-filter-max-odds")?.value || "").trim();
  const maxAmericanOdds = maxOddsRaw ? num(maxOddsRaw, NaN) : NaN;
  let out = rows
    .filter((r) => {
      const rowBook = normalizeEvSportsbookKey(r.bestBook);
      if (!evSportsbookAllowed(rowBook)) return false;
      const okG = !g || String(r.golfer || "").toLowerCase().includes(gLow);
      return okG && (!m || r.market === m) && (!bookFilter || bookFilter.has(rowBook));
    })
    .map((r) => {
      const dec0 = num(r.bestDec, NaN);
      const dec = decimalWithProfitBoost(dec0, boostPct);
      const bookImp = Number.isFinite(dec) && dec > 1 ? 1 / dec : NaN;
      const am = Number.isFinite(dec) && dec > 1 ? americanFromDecimal(dec) : NaN;
      const evProb = evProbForRow(r);
      const mEv = modelEvWithBoost(r);
      const kelly = evKellyDollarsFromDecimal(evProb, dec, bankroll);
      const deltaPct = Number.isFinite(evProb) && Number.isFinite(bookImp) ? (evProb - bookImp) * 100 : NaN;
      return { ...r, _dec: dec, _am: am, _bookImp: bookImp, _evProb: evProb, _modelEv: mEv, _kelly: kelly, _deltaPct: deltaPct };
    })
    .filter((r) => {
      const hasSportsbookOdds =
        String(r.bestBook || "").trim() !== "" && Number.isFinite(r._dec) && r._dec > 1;
      if (!hasSportsbookOdds) return false;
      if (Number.isFinite(maxAmericanOdds) && Number.isFinite(r._am) && r._am >= maxAmericanOdds) return false;
      return true;
    });
  /** Drop pathological model-EV rows, then cap count: pool = top 500 by model EV (desc), then apply table sort. */
  const EV_TABLE_MODEL_EV_ABS_MAX = 0.5;
  const EV_TABLE_MAX_ROWS = 500;
  out = out.filter((r) => {
    const me = num(r._modelEv, NaN);
    if (!Number.isFinite(me)) return true;
    return me <= EV_TABLE_MODEL_EV_ABS_MAX && me >= -EV_TABLE_MODEL_EV_ABS_MAX;
  });
  out.sort((a, b) => {
    const ma = num(a._modelEv, NaN);
    const mb = num(b._modelEv, NaN);
    if (!Number.isFinite(ma) && !Number.isFinite(mb)) return 0;
    if (!Number.isFinite(ma)) return 1;
    if (!Number.isFinite(mb)) return -1;
    return mb - ma;
  });
  if (out.length > EV_TABLE_MAX_ROWS) out = out.slice(0, EV_TABLE_MAX_ROWS);
  out = out.slice().sort((a, c) => compareEvRows(a, c, evSort.key, evSort.dir));
  updateEvSortIndicators();
  tbody.innerHTML = "";
  if (!out.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 10;
    td.className = "text-muted";
    td.textContent = "No bets for current filters.";
    tr.appendChild(td);
    tbody.appendChild(tr);
    return;
  }
  const evDash = (s) => {
    const t = String(s ?? "").trim();
    if (t === "—" || t === "-" || t === "–") return "";
    return String(s ?? "");
  };
  const evCell = (s) => {
    const t = String(s ?? "").trim();
    if (t === "—" || t === "-" || t === "–") return "";
    return String(s ?? "");
  };
  const evFrag = document.createDocumentFragment();
  for (const r of out) {
    const tr = document.createElement("tr");
    const mkTd = (txt, cls = "") => {
      const td = document.createElement("td");
      if (cls) td.className = cls;
      td.textContent = txt;
      return td;
    };
    const dec = r._dec;
    const bookImp = r._bookImp;
    const impliedStr = Number.isFinite(bookImp) ? `${(bookImp * 100).toFixed(1)}%` : "";
    let deltaStr = "";
    if (Number.isFinite(r._deltaPct)) {
      const dPct = r._deltaPct;
      deltaStr = `${dPct >= 0 ? "+" : ""}${dPct.toFixed(1)}%`;
    }
    const kelly$ = r._kelly;
    const kellyStr = Number.isFinite(kelly$) ? `$${kelly$.toFixed(2)}` : "";

    const mEv = r._modelEv;
    const modelEvTd = mkTd(Number.isFinite(mEv) ? `${(mEv * 100).toFixed(1)}%` : "", "num");
    if (Number.isFinite(mEv)) modelEvTd.classList.add(mEv >= 0 ? "ev-pos" : "ev-neg");
    tr.appendChild(modelEvTd);
    tr.appendChild(mkTd(evCell(kellyStr), "num"));
    const bb = document.createElement("td");
    bb.className = "num best-book-td";
    if (r.bestBook) {
      const oddsDisp =
        boostPct > 0 && Number.isFinite(dec) && dec > 1
          ? formatAmerican(americanFromDecimal(dec))
          : r.bestBookOdds;
      bb.innerHTML = `${bookBadgeHtml(r.bestBook)} <span class="best-book-odds">${evDash(oddsDisp)}</span>`;
    }
    tr.appendChild(bb);
    tr.appendChild(mkTd(evCell(r.golfer), ""));
    const modelTd = document.createElement("td");
    modelTd.className = "num";
    modelTd.textContent = evCell(modelAmericanFromProb(r.modelPct));
    tr.appendChild(modelTd);
    tr.appendChild(mkTd(evCell(modelAmericanFromProb(r.consensusP)), "num"));
    tr.appendChild(mkTd(evCell(impliedStr), "num"));
    tr.appendChild(mkTd(evCell(deltaStr), "num"));
    tr.appendChild(mkTd(evCell(r.market), ""));
    tr.appendChild(mkTd(evCell(r.bet), ""));
    evFrag.appendChild(tr);
  }
  tbody.appendChild(evFrag);
}

/**
 * P(player 1 beats player 2) for one round / multi-day context.
 * Uses a Gaussian difference with stroke-scale noise; old 0.35·√2 SD (~0.5) was far too tight vs
 * single-round variance, which produced bogus ~−2000 “fair” prices.
 */
function matchupWinProb(mu1, mu2, marketKind) {
  if (!Number.isFinite(mu1) || !Number.isFinite(mu2)) return NaN;
  const sigmaDiff =
    marketKind === "tournament_matchups"
      ? 1.55
      : 2.85;
  const d = (mu1 - mu2) / sigmaDiff;
  return clamp(normalCdf(d), 0.12, 0.88);
}

/** Win shares for a 3-ball (lowest score wins); softmax on mu_sg so similar skills stay ~⅓ each. */
function threeBallModelProbs(mu1, mu2, mu3) {
  const m = [mu1, mu2, mu3].map((x) => num(x, NaN));
  if (m.some((x) => !Number.isFinite(x))) return [1 / 3, 1 / 3, 1 / 3];
  const T = 2.05;
  const ex = m.map((v) => Math.exp(v / T));
  const s = ex[0] + ex[1] + ex[2];
  if (s <= 0) return [1 / 3, 1 / 3, 1 / 3];
  return [ex[0] / s, ex[1] / s, ex[2] / s];
}

/** When >0, blend matchup model probs with live DataGolf win shares (round / 3-ball never blend). */
function liveMatchupModelBlendAlpha(marketKind) {
  if (!inPlayAffectsRoundOdds()) return 0;
  if (marketKind === "round_matchups" || marketKind === "3_balls") return 0;
  const a = num(DATA.meta?.live_matchup_model_blend, NaN);
  if (!Number.isFinite(a) || a <= 0) return 0;
  return clamp(a, 0, 0.85);
}

function matchupWinProbLiveBlended(mu1, mu2, mk, row1, row2) {
  const pBase = matchupWinProb(mu1, mu2, mk);
  const alpha = liveMatchupModelBlendAlpha(mk);
  if (!alpha || !row1 || !row2) return pBase;
  const w1 = num(row1.win, NaN);
  const w2 = num(row2.win, NaN);
  if (!Number.isFinite(w1) || !Number.isFinite(w2) || w1 + w2 <= 0) return pBase;
  const pLive = w1 / (w1 + w2);
  return clamp(alpha * pLive + (1 - alpha) * pBase, 0.08, 0.92);
}

function threeBallModelProbsLiveBlended(mu1, mu2, mu3, row1, row2, row3) {
  const base = threeBallModelProbs(mu1, mu2, mu3);
  const alpha = liveMatchupModelBlendAlpha("3_balls");
  if (!alpha || !row1 || !row2 || !row3) return base;
  const w1 = num(row1.win, NaN);
  const w2 = num(row2.win, NaN);
  const w3 = num(row3.win, NaN);
  if (![w1, w2, w3].every(Number.isFinite) || w1 + w2 + w3 <= 0) return base;
  const t = w1 + w2 + w3;
  const live = [w1 / t, w2 / t, w3 / t];
  const out = [0, 1, 2].map((i) => alpha * live[i] + (1 - alpha) * base[i]);
  const s = out[0] + out[1] + out[2];
  if (s <= 0) return base;
  return [out[0] / s, out[1] / s, out[2] / s];
}

function bestBookDecimalForSide(oddsObj, side /* 'p1'|'p2'|'p3' */, opts = {}) {
  if (!oddsObj || typeof oddsObj !== "object") return { book: "", dec: NaN };
  let bestD = NaN;
  let bestB = "";
  for (const bk of Object.keys(oddsObj)) {
    if (normalizeEvSportsbookKey(bk) === "datagolf" && !opts.allowDatagolf) continue;
    const pack = oddsObj[bk];
    if (!pack || typeof pack !== "object") continue;
    let d = NaN;
    if (side === "p3") {
      const t = matchupOddsThreeWayFromPack(pack);
      d = t.d3;
    } else {
      const t = matchupOddsTwoWayFromPack(pack);
      d = side === "p1" ? t.d1 : t.d2;
    }
    if (!Number.isFinite(d) || d <= 1) continue;
    if (!Number.isFinite(bestD) || d > bestD) {
      bestD = d;
      bestB = bk;
    }
  }
  return { book: bestB, dec: bestD };
}

/** Like `bestBookDecimalForSide` but only keys allowed by +EV whitelist *and* Devig consensus settings. */
function bestBookDecimalForSideEvPrefs(oddsObj, side /* 'p1'|'p2'|'p3' */, prefs, opts = {}) {
  const filtered = filterOddsObjectForEvSportsbooks(oddsObj || {}, opts);
  if (!filtered || typeof filtered !== "object") return { book: "", dec: NaN };
  let bestD = NaN;
  let bestB = "";
  for (const bk of Object.keys(filtered)) {
    if (!evBookAllowedInConsensus(bk, prefs, opts)) continue;
    const pack = filtered[bk];
    if (!pack || typeof pack !== "object") continue;
    let d = NaN;
    if (side === "p3") {
      const t = matchupOddsThreeWayFromPack(pack);
      d = t.d3;
    } else {
      const t = matchupOddsTwoWayFromPack(pack);
      d = side === "p1" ? t.d1 : t.d2;
    }
    if (!Number.isFinite(d) || d <= 1) continue;
    if (!Number.isFinite(bestD) || d > bestD) {
      bestD = d;
      bestB = bk;
    }
  }
  return { book: bestB, dec: bestD };
}

function drivingDistanceSkillRating(raw) {
  if (!Number.isFinite(raw)) return NaN;
  return raw;
}

function drivingAccuracySkillRating(raw) {
  if (!Number.isFinite(raw)) return NaN;
  return raw > -1 && raw < 1 ? raw * 100 : raw;
}

/** Fill missing driving_* fields from another projection row with the same dg_id. */
function mergedPlayerRowForDrivingFields(row) {
  if (!row || typeof row !== "object") return row;
  const id = Math.round(num(row.dg_id, NaN));
  if (!Number.isFinite(id)) return row;
  const keys = [
    "driving_dist",
    "avg_driving_distance",
    "driving_distance",
    "average_driving_distance",
    "avg_drive_distance",
    "predicted_driving_distance",
    "predicted_avg_driving_distance",
    "adj_driving_distance",
    "driving_distance_rating",
    "driving_acc",
    "driving_accuracy",
  ];
  const out = { ...row };
  for (const p of DATA.players || []) {
    if (Math.round(num(p.dg_id, NaN)) !== id) continue;
    for (const k of keys) {
      if (!Number.isFinite(num(out[k], NaN)) && Number.isFinite(num(p[k], NaN))) out[k] = p[k];
    }
  }
  return out;
}

function matchupAnalysisMetricValue(row, key) {
  row = mergedPlayerRowForDrivingFields(row);
  if (!row) return NaN;
  if (key === "sg_total") {
    const base = num(row.sg_total, NaN);
    if (Number.isFinite(base)) return base;
    return num(row.mu_sg, NaN);
  }
  if (key === "distance") {
    const yds = playerDrivingDistanceYds(row);
    return Number.isFinite(yds) ? yds : NaN;
  }
  if (key === "accuracy") {
    const cands = [
      num(row.driving_acc, NaN),
      num(row.driving_accuracy, NaN),
    ];
    for (const v of cands) {
      if (Number.isFinite(v)) return drivingAccuracySkillRating(v);
    }
    return NaN;
  }
  return num(row[key], NaN);
}

function renderMatchupAnalysisPricing(host, entry) {
  if (!host) return;
  host.innerHTML = "";
  if (!entry?.pricingSides?.length) {
    const p = document.createElement("p");
    p.className = "text-muted";
    p.textContent = "No pricing for this matchup.";
    host.appendChild(p);
    return;
  }
  const tbl = document.createElement("table");
  tbl.className = "data-table data-table-outrights matchup-analysis-price-table";
  const thead = document.createElement("thead");
  const hr = document.createElement("tr");
  hr.appendChild(document.createElement("th"));
  let maxEdge = NaN;
  for (const s of entry.pricingSides) {
    const e = num(s.edge, NaN);
    if (!Number.isFinite(e)) continue;
    maxEdge = Number.isFinite(maxEdge) ? Math.max(maxEdge, e) : e;
  }
  const multi = entry.pricingSides.length > 1;
  for (const s of entry.pricingSides) {
    const th = document.createElement("th");
    th.className = "num";
    th.textContent = displayGolferName(String(s.label || ""));
    const se = num(s.edge, NaN);
    if (multi && Number.isFinite(se) && Number.isFinite(maxEdge) && Math.abs(se - maxEdge) < 1e-9) {
      th.classList.add("matchup-analysis-best-pick");
    }
    hr.appendChild(th);
  }
  thead.appendChild(hr);
  tbl.appendChild(thead);
  const tb = document.createElement("tbody");
  const addRow = (label, fill) => {
    const tr = document.createElement("tr");
    const td0 = document.createElement("td");
    td0.textContent = label;
    tr.appendChild(td0);
    for (const s of entry.pricingSides) {
      const td = document.createElement("td");
      td.className = "num";
      fill(td, s);
      tr.appendChild(td);
    }
    tb.appendChild(tr);
  };
  addRow("Model", (td, s) => {
    td.textContent = Number.isFinite(s.modelPct) ? `${(s.modelPct * 100).toFixed(1)}%` : "—";
  });
  addRow("Market", (td, s) => {
    td.textContent = Number.isFinite(s.marketPct) ? `${(s.marketPct * 100).toFixed(1)}%` : "—";
  });
  addRow("Edge", (td, s) => {
    if (Number.isFinite(s.edge)) {
      td.textContent = `${(s.edge * 100).toFixed(1)}%`;
      if (s.edge > 0) td.classList.add("ev-pos");
      else if (s.edge < 0) td.classList.add("ev-neg");
    } else td.textContent = "—";
  });
  addRow("Odds", (td, s) => {
    td.textContent =
      Number.isFinite(s.book?.dec) && s.book.dec > 1 ? formatAmerican(americanFromDecimal(s.book.dec)) : "—";
  });
  addRow("Best Book", (td, s) => {
    if (s.book?.book) td.innerHTML = bookBadgeHtml(s.book.book);
    else td.textContent = "—";
  });
  tbl.appendChild(tb);
  host.appendChild(tbl);
}

/** One projection row per `dg_id` for the active model round (same resolution as matchup rows). */
function matchupAnalysisFieldRowsForRound(r) {
  const ids = new Set(
    (DATA.players || []).map((p) => Math.round(num(p.dg_id, NaN))).filter((id) => Number.isFinite(id)),
  );
  const rows = [];
  for (const id of ids) {
    const row = projectionPlayerRowForModel(id, r);
    if (row) rows.push(row);
  }
  return rows;
}

/** Share of the field strictly worse on this SG metric (higher SG is better), in 0–100. */
function matchupAnalysisFieldPctHigherBetter(samples, v) {
  if (!Array.isArray(samples) || !samples.length || !Number.isFinite(v)) return NaN;
  let below = 0;
  for (const x of samples) {
    if (Number.isFinite(x) && x < v) below++;
  }
  return (below / samples.length) * 100;
}

/** Narrow screens: short header to avoid broken mid-word wraps (full name still in row context). */
function matchupAnalysisShortPlayerHead(fullName) {
  const s = displayGolferName(String(fullName || "")).trim();
  if (!s) return "Player";
  const parts = s.split(/\s+/).filter(Boolean);
  if (parts.length === 1) return parts[0].length > 14 ? `${parts[0].slice(0, 13)}…` : parts[0];
  return parts[parts.length - 1];
}

function buildMatchupAnalysisTool() {
  const pricingHost = document.getElementById("matchup-analysis-pricing");
  const matchupPickEl = document.getElementById("analysis-matchup-select");
  const setMatchupPickUiHidden = (hidden) => {
    const wrap = matchupPickEl?.closest(".golfer-combobox-wrap");
    if (wrap) wrap.hidden = hidden;
    // Native `<select>` stays hidden: only the search field + suggest panel are visible (never show two pickers).
    if (matchupPickEl) matchupPickEl.hidden = true;
    if (hidden) {
      const s = document.getElementById("analysis-matchup-select-search");
      if (s) s.value = "";
    }
  };
  const sgBody = document.querySelector("#table-matchup-analysis-sg tbody");
  const marketEl = document.getElementById("analysis-market");
  const note = document.getElementById("analysis-market-note");
  if (!sgBody) return;
  sgBody.innerHTML = "";
  if (pricingHost) pricingHost.innerHTML = "";
  const key = String(marketEl?.value || "round_matchups");
  const pack = DATA.matchups && DATA.matchups[key];
  const list = pack && pack.match_list;
  const titleA = document.getElementById("analysis-sg-player-a");
  const titleB = document.getElementById("analysis-sg-player-b");
  const sgHeading = document.getElementById("matchup-analysis-sg-heading");
  const sgWrap = document.getElementById("matchup-analysis-sg-wrap");
  const hideSgTableMarket = key === "3_balls";
  if (sgHeading) sgHeading.hidden = hideSgTableMarket;
  if (sgWrap) sgWrap.hidden = hideSgTableMarket;
  if (titleA) titleA.textContent = "Player A";
  if (titleB) titleB.textContent = "Player B";
  if (typeof list === "string") {
    if (note) {
      note.hidden = true;
      note.textContent = "";
    }
    if (matchupPickEl) {
      matchupPickEl.innerHTML = "";
      setMatchupPickUiHidden(true);
    }
    matchupAnalysisRowsCache = [];
    return;
  }
  if (note) note.hidden = true;
  if (!Array.isArray(list) || !list.length) {
    if (matchupPickEl) {
      matchupPickEl.innerHTML = "";
      setMatchupPickUiHidden(true);
    }
    matchupAnalysisRowsCache = [];
    return;
  }

  const r = getModelRoundForEv();
  const devigPrefs = loadEvDevigPrefs();
  const fieldRows = matchupAnalysisFieldRowsForRound(r);
  const sgMetricKeys = ["sg_total", "sg_t2g", "sg_ott", "sg_app", "sg_arg", "sg_putt", "distance", "accuracy"];
  /** @type {Record<string, number[]>} */
  const fieldSamplesByMetric = {};
  for (const mk of sgMetricKeys) {
    fieldSamplesByMetric[mk] = fieldRows
      .map((row) => matchupAnalysisMetricValue(row, mk))
      .filter((x) => Number.isFinite(x));
  }

  const rows = [];
  for (const m of list) {
    const id1 = Math.round(num(m.p1_dg_id, NaN));
    const id2 = Math.round(num(m.p2_dg_id, NaN));
    const id3 = Math.round(num(m.p3_dg_id, NaN));
    const row1 = projectionPlayerRowForModelByIdOrName(id1, m.p1_player_name, r);
    const row2 = projectionPlayerRowForModelByIdOrName(id2, m.p2_player_name, r);
    const row3 = projectionPlayerRowForModelByIdOrName(id3, m.p3_player_name, r);
    const rawOdds = m.odds || {};
    const oddsEv = filterOddsObjectForEvSportsbooks(rawOdds, {});
    const b1 = bestBookDecimalForSideWithFallback(oddsEv, "p1", devigPrefs);
    const b2 = bestBookDecimalForSideWithFallback(oddsEv, "p2", devigPrefs);
    const b3 = bestBookDecimalForSideWithFallback(oddsEv, "p3", devigPrefs);
    const isThree = key === "3_balls" && Number.isFinite(id3) && id3 > 0;
    const mu1 = effectiveMuSg(row1, id1, key);
    const mu2 = effectiveMuSg(row2, id2, key);
    const mu3 = effectiveMuSg(row3, id3, key);
    if (isThree) {
      const [p1, p2, p3] = threeBallModelProbsLiveBlended(mu1, mu2, mu3, row1, row2, row3);
      const mp1 = matchupMarketImpliedProbSide(rawOdds, oddsEv, "p1", devigPrefs, true);
      const mp2 = matchupMarketImpliedProbSide(rawOdds, oddsEv, "p2", devigPrefs, true);
      const mp3 = matchupMarketImpliedProbSide(rawOdds, oddsEv, "p3", devigPrefs, true);
      const ev1 = Number.isFinite(b1.dec) ? p1 * b1.dec - 1 : NaN;
      const ev2 = Number.isFinite(b2.dec) ? p2 * b2.dec - 1 : NaN;
      const ev3 = Number.isFinite(b3.dec) ? p3 * b3.dec - 1 : NaN;
      const pricingSides = [
        {
          label: String(m.p1_player_name || ""),
          modelPct: p1,
          marketPct: mp1,
          edge: ev1,
          book: b1,
          row: row1,
        },
        {
          label: String(m.p2_player_name || ""),
          modelPct: p2,
          marketPct: mp2,
          edge: ev2,
          book: b2,
          row: row2,
        },
        {
          label: String(m.p3_player_name || ""),
          modelPct: p3,
          marketPct: mp3,
          edge: ev3,
          book: b3,
          row: row3,
        },
      ];
      const sides = [
        { side: "p1", name: pricingSides[0].label, modelPct: p1, marketPct: mp1, edge: ev1, book: b1, row: row1 },
        { side: "p2", name: pricingSides[1].label, modelPct: p2, marketPct: mp2, edge: ev2, book: b2, row: row2 },
        { side: "p3", name: pricingSides[2].label, modelPct: p3, marketPct: mp3, edge: ev3, book: b3, row: row3 },
      ].sort((a, b) => num(b.edge, -99) - num(a.edge, -99));
      rows.push({
        key: `3b:${id1}:${id2}:${id3}`,
        matchup: `${displayGolferName(String(m.p1_player_name || ""))} / ${displayGolferName(String(m.p2_player_name || ""))} / ${displayGolferName(String(m.p3_player_name || ""))}`,
        best: sides[0],
        pricingSides,
        isThree: true,
      });
      continue;
    }
    const p1 = matchupWinProbLiveBlended(mu1, mu2, key, row1, row2);
    const p2 = 1 - p1;
    const marketP1 = matchupMarketImpliedProbSide(rawOdds, oddsEv, "p1", devigPrefs, false);
    const marketP2 = matchupMarketImpliedProbSide(rawOdds, oddsEv, "p2", devigPrefs, false);
    const ev1 = Number.isFinite(b1.dec) ? p1 * b1.dec - 1 : NaN;
    const ev2 = Number.isFinite(b2.dec) ? p2 * b2.dec - 1 : NaN;
    const best =
      ev1 >= ev2
        ? {
            side: "p1",
            name: String(m.p1_player_name || ""),
            modelPct: p1,
            marketPct: marketP1,
            edge: ev1,
            book: b1,
            row: row1,
          }
        : {
            side: "p2",
            name: String(m.p2_player_name || ""),
            modelPct: p2,
            marketPct: marketP2,
            edge: ev2,
            book: b2,
            row: row2,
          };
    const pricingSides = [
      {
        label: String(m.p1_player_name || ""),
        modelPct: p1,
        marketPct: marketP1,
        edge: ev1,
        book: b1,
        row: row1,
      },
      {
        label: String(m.p2_player_name || ""),
        modelPct: p2,
        marketPct: marketP2,
        edge: ev2,
        book: b2,
        row: row2,
      },
    ];
    rows.push({
      key: `h2h:${id1}:${id2}`,
      matchup: `${displayGolferName(String(m.p1_player_name || ""))} vs ${displayGolferName(String(m.p2_player_name || ""))}`,
      best,
      pricingSides,
      isThree: false,
      left: { name: String(m.p1_player_name || ""), row: row1 },
      right: { name: String(m.p2_player_name || ""), row: row2 },
    });
  }

  rows.sort((a, b) => num(b.best?.edge, -99) - num(a.best?.edge, -99));
  matchupAnalysisRowsCache = rows;
  if (!rows.length) {
    matchupAnalysisRowsCache = [];
    if (matchupPickEl) {
      matchupPickEl.innerHTML = "";
      setMatchupPickUiHidden(true);
    }
    return;
  }

  if (!rows.some((x) => x.key === matchupAnalysisSelectedKey)) matchupAnalysisSelectedKey = rows[0].key;
  const selected = rows.find((x) => x.key === matchupAnalysisSelectedKey) || rows[0];

  const MATCHUP_SELECT_MAX = 20;
  let uiRows = rows.slice(0, MATCHUP_SELECT_MAX);
  const sk = matchupAnalysisSelectedKey;
  if (sk && !uiRows.some((x) => x.key === sk)) {
    const keep = rows.find((x) => x.key === sk);
    if (keep) uiRows = [keep, ...rows.filter((x) => x.key !== sk).slice(0, MATCHUP_SELECT_MAX - 1)];
  }

  if (matchupPickEl) {
    setMatchupPickUiHidden(false);
    matchupPickEl.innerHTML = "";
    for (const item of uiRows) {
      const opt = document.createElement("option");
      opt.value = item.key;
      opt.textContent = item.matchup;
      matchupPickEl.appendChild(opt);
    }
    matchupPickEl.value = selected.key;
    refreshGolferComboboxFromSelect("analysis-matchup-select");
  }
  renderMatchupAnalysisPricing(pricingHost, selected);

  const hideSgTable = hideSgTableMarket;

  const renderSgBreakdown = (entry) => {
    sgBody.innerHTML = "";
    if (!entry || !entry.left || !entry.right) {
      const tr = document.createElement("tr");
      const td = document.createElement("td");
      td.colSpan = 6;
      td.className = "text-muted";
      td.textContent = "Select a head-to-head matchup.";
      tr.appendChild(td);
      sgBody.appendChild(tr);
      return;
    }
    const narrowSgHead =
      typeof window !== "undefined" &&
      window.matchMedia &&
      window.matchMedia("(max-width: 720px)").matches;
    const fullA = displayGolferName(String(entry.left.name || "")) || "Player A";
    const fullB = displayGolferName(String(entry.right.name || "")) || "Player B";
    if (titleA) {
      titleA.textContent = narrowSgHead ? matchupAnalysisShortPlayerHead(entry.left.name) : fullA;
      titleA.title = fullA;
    }
    if (titleB) {
      titleB.textContent = narrowSgHead ? matchupAnalysisShortPlayerHead(entry.right.name) : fullB;
      titleB.title = fullB;
    }
    const metrics = [
      ["SG: Total", "sg_total"],
      ["SG: Tee-to-Green", "sg_t2g"],
      ["Driving Accuracy Rating", "accuracy"],
      ["Off Tee", "sg_ott"],
      ["Approach", "sg_app"],
      ["Around Green", "sg_arg"],
      ["Putting", "sg_putt"],
      ["Driving Distance Rating", "distance"],
    ];
    const barPctMatchup = (kind, v, samples) => {
      const finite = (samples || []).filter((x) => Number.isFinite(x));
      if (kind === "sg") {
        let maxAbs = 0;
        for (const s of finite) maxAbs = Math.max(maxAbs, Math.abs(s));
        maxAbs = Math.max(maxAbs, Math.abs(v), 0.6);
        return Math.max(0, Math.min(100, (Math.abs(v) / maxAbs) * 100));
      }
      let lo = Infinity;
      let hi = -Infinity;
      for (const s of finite) {
        lo = Math.min(lo, s);
        hi = Math.max(hi, s);
      }
      lo = Math.min(lo, v);
      hi = Math.max(hi, v);
      const span = hi - lo;
      if (!Number.isFinite(span) || span < 1e-9) return 0;
      return Math.max(0, Math.min(100, ((v - lo) / span) * 100));
    };
    const formatDistanceYards = (v) => {
      const y = Math.round(v);
      const unit = Math.abs(y) === 1 ? "yd" : "yds";
      return `${y > 0 ? "+" : ""}${y} ${unit}`;
    };
    const buildMetricCell = (td, v, samples, kind = "sg") => {
      td.className = "num matchup-analysis-bar-cell";
      if (!Number.isFinite(v)) {
        td.textContent = "—";
        return;
      }
      const pct = barPctMatchup(kind, v, samples);
      const wrap = document.createElement("span");
      wrap.className = "matchup-analysis-bar-wrap";
      const val = document.createElement("span");
      if (kind === "sg") {
        val.className = `matchup-analysis-bar-val${v >= 0 ? " pos" : " neg"}`;
        val.textContent = `${v >= 0 ? "+" : ""}${v.toFixed(2)}`;
      } else {
        val.className = `matchup-analysis-bar-val neutral ${kind}`;
        if (kind === "distance") val.textContent = formatDistanceYards(v);
        else if (kind === "accuracy") val.textContent = `${v >= 0 ? "+" : ""}${v.toFixed(1)} pts`;
        else val.textContent = `${v.toFixed(1)}%`;
      }
      const track = document.createElement("span");
      track.className = "matchup-analysis-bar-track";
      const fill = document.createElement("span");
      fill.className = `matchup-analysis-bar-fill ${kind}`;
      fill.style.width = `${pct.toFixed(1)}%`;
      track.appendChild(fill);
      wrap.appendChild(val);
      wrap.appendChild(track);
      td.appendChild(wrap);
    };
    const edgeForAdvantagedSide = (leftSide) => {
      const side = leftSide ? entry.pricingSides?.[0] : entry.pricingSides?.[1];
      return num(side?.edge, NaN);
    };
    for (const [label, keyMetric] of metrics) {
      const samples = fieldSamplesByMetric[keyMetric] || [];
      const a = matchupAnalysisMetricValue(entry.left.row, keyMetric);
      const b = matchupAnalysisMetricValue(entry.right.row, keyMetric);
      const diff = Number.isFinite(a) && Number.isFinite(b) ? a - b : NaN;
      const pctA = matchupAnalysisFieldPctHigherBetter(samples, a);
      const pctB = matchupAnalysisFieldPctHigherBetter(samples, b);
      const tr = document.createElement("tr");
      const tdMetric = document.createElement("td");
      tdMetric.textContent = label;
      const tdA = document.createElement("td");
      buildMetricCell(tdA, a, samples, keyMetric === "sg_total" || keyMetric.startsWith("sg_") ? "sg" : keyMetric);
      const tdPctA = document.createElement("td");
      tdPctA.className = "num";
      tdPctA.textContent = Number.isFinite(pctA) ? `${pctA.toFixed(0)}%` : "—";
      const tdB = document.createElement("td");
      buildMetricCell(tdB, b, samples, keyMetric === "sg_total" || keyMetric.startsWith("sg_") ? "sg" : keyMetric);
      const tdPctB = document.createElement("td");
      tdPctB.className = "num";
      tdPctB.textContent = Number.isFinite(pctB) ? `${pctB.toFixed(0)}%` : "—";
      const tdAdv = document.createElement("td");
      const eps = keyMetric.startsWith("sg_") ? 0.005 : keyMetric === "distance" ? 0.2 : 0.2;
      if (!Number.isFinite(diff) || Math.abs(diff) < eps) {
        tdAdv.textContent = "Even";
      } else {
        const leftAdvantage = diff > 0;
        const rawWho = leftAdvantage ? entry.left.name : entry.right.name;
        const who = displayGolferName(String(rawWho || ""));
        const adv = Math.abs(diff);
        const edge = edgeForAdvantagedSide(leftAdvantage);
        if (keyMetric.startsWith("sg_")) {
          tdAdv.textContent = `${who} +${adv.toFixed(2)} strokes`;
        } else if (keyMetric === "distance") {
          tdAdv.textContent = `${who} +${Math.round(adv)} yds`;
        } else {
          tdAdv.textContent = `${who} +${adv.toFixed(1)} pts`;
        }
        if (edge > 0) tdAdv.className = "ev-pos";
        else if (edge < 0) tdAdv.className = "ev-neg";
      }
      tr.appendChild(tdMetric);
      tr.appendChild(tdA);
      tr.appendChild(tdPctA);
      tr.appendChild(tdB);
      tr.appendChild(tdPctB);
      tr.appendChild(tdAdv);
      sgBody.appendChild(tr);
    }
  };

  if (!hideSgTable) {
    renderSgBreakdown(selected);
  } else {
    sgBody.innerHTML = "";
    if (titleA) titleA.textContent = "Player A";
    if (titleB) titleB.textContent = "Player B";
  }
}

/** --- Course fit tab (skill-shape radar, venue similarity, fit leaderboard) --- */
const COURSE_FIT_AXIS_KEYS = ["dist", "acc", "app", "arg", "putt"];

/** Legacy 5-axis helper: yards when measured, else implied carry from DG yards-vs-tour rating. */
function courseFitRawProfile(row) {
  if (!row || typeof row !== "object") return COURSE_FIT_AXIS_KEYS.map(() => NaN);
  const m = mergedPlayerRowForDrivingFields(row);
  const d = drivingDistanceSkillRating(num(playerDrivingDistanceYds(m), NaN));
  const acc = drivingAccuracySkillRating(num(m.driving_acc ?? m.driving_accuracy, NaN));
  return [d, acc, num(row.sg_app, NaN), num(row.sg_arg, NaN), num(row.sg_putt, NaN)];
}

function courseFitMinMaxFromRows(rows) {
  const vals = COURSE_FIT_AXIS_KEYS.map(() => []);
  for (const r of rows) {
    const raw = courseFitRawProfile(r);
    for (let i = 0; i < 5; i++) vals[i].push(raw[i]);
  }
  return vals.map((arr) => {
    const finite = arr.filter((x) => Number.isFinite(x));
    if (!finite.length) return { lo: 0, hi: 1 };
    let lo = Math.min(...finite);
    let hi = Math.max(...finite);
    if (hi - lo < 1e-9) {
      lo -= 1;
      hi += 1;
    }
    return { lo, hi };
  });
}

function courseFitNormalizeRaw(raw, ranges) {
  return raw.map((v, i) => {
    if (!Number.isFinite(v)) return 0.5;
    const { lo, hi } = ranges[i];
    return clamp((v - lo) / (hi - lo), 0, 1);
  });
}

function courseFitMeanNormalized(rows, ranges) {
  if (!rows.length) return COURSE_FIT_AXIS_KEYS.map(() => 0.5);
  const sum = COURSE_FIT_AXIS_KEYS.map(() => 0);
  let n = 0;
  for (const r of rows) {
    const nv = courseFitNormalizeRaw(courseFitRawProfile(r), ranges);
    if (nv.every((x) => Number.isFinite(x))) {
      for (let i = 0; i < 5; i++) sum[i] += nv[i];
      n++;
    }
  }
  if (!n) return COURSE_FIT_AXIS_KEYS.map(() => 0.5);
  return sum.map((s) => s / n);
}

let courseFitCourseStatsHistoryRef = null;
let courseFitCourseStatsCache = null;
/** Bump when `normCourseNameKey` changes so per-course SG buckets re-merge. */
const COURSE_FIT_STATS_NORM_VERSION = 1;
let courseFitCourseStatsNormVersionRef = 0;

function courseFitCourseStatsByCourse() {
  if (!HISTORY._ok || !HISTORY.byDgId) return new Map();
  if (
    courseFitCourseStatsCache &&
    courseFitCourseStatsHistoryRef === HISTORY.byDgId &&
    courseFitCourseStatsNormVersionRef === COURSE_FIT_STATS_NORM_VERSION
  ) {
    return courseFitCourseStatsCache;
  }
  const keys = ["sg_ott", "sg_app", "sg_arg", "sg_putt"];
  /** @type {Map<string, { sum: number[]; ct: number[] }>} */
  const acc = new Map();
  for (const rec of Object.values(HISTORY.byDgId)) {
    if (!rec?.rounds) continue;
    for (const r of rec.rounds) {
      if (historyRoundIsPlaceholderAllMarketsZero(r)) continue;
      const ck = normCourseNameKey(historyRoundCourseName(r));
      if (!ck) continue;
      if (!acc.has(ck)) acc.set(ck, { sum: [0, 0, 0, 0], ct: [0, 0, 0, 0] });
      const b = acc.get(ck);
      for (let j = 0; j < 4; j++) {
        const v = num(r[keys[j]], NaN);
        if (Number.isFinite(v)) {
          b.sum[j] += v;
          b.ct[j]++;
        }
      }
    }
  }
  courseFitCourseStatsHistoryRef = HISTORY.byDgId;
  courseFitCourseStatsNormVersionRef = COURSE_FIT_STATS_NORM_VERSION;
  courseFitCourseStatsCache = acc;
  return acc;
}

/** Mean SG from embedded history at venue (ott/app/arg/putt); returns null if thin data. */
function courseFitVenueHistoricalSgMeans(venueKeyNorm) {
  if (!venueKeyNorm) return null;
  const b = courseFitCourseStatsByCourse().get(venueKeyNorm);
  if (!b) return null;
  const mins = 8;
  const totalSamples = b.ct.reduce((a, c) => a + c, 0);
  if (totalSamples < mins) return null;
  const means = b.sum.map((s, j) => (b.ct[j] ? s / b.ct[j] : NaN));
  return { ott: means[0], app: means[1], arg: means[2], putt: means[3], samples: totalSamples };
}

function historyRoundCourseName(r) {
  return String(r?.course_name ?? "").trim();
}

function courseFitNormScalar(v, rangeObj) {
  if (!Number.isFinite(v)) return 0.5;
  const lo = rangeObj.lo;
  const hi = rangeObj.hi;
  const span = hi - lo < 1e-9 ? 1 : hi - lo;
  return clamp((v - lo) / span, 0, 1);
}

/**
 * Match embedded-history venue SG to the selected course key (projections spelling may differ).
 * Returns resolvedKey whose rounds supply histSg; otherwise null.
 */
function courseFitResolveHistSgForVenue(vk) {
  if (!vk) return null;
  const direct = courseFitVenueHistoricalSgMeans(vk);
  if (direct) return { histSg: direct, resolvedKey: vk };
  const map = courseFitMeanSgVectorByCourse();
  const scored = [];
  for (const ck of map.keys()) {
    const hh = courseFitVenueHistoricalSgMeans(ck);
    if (!hh) continue;
    let score = 0;
    if (ck === vk) score = 10000;
    else if (vk.length >= 6 && (ck.includes(vk) || vk.includes(ck))) {
      score = 500 + Math.min(ck.length, vk.length);
    } else {
      const vt = vk.split(" ").filter((t) => t.length > 1);
      const ct = ck.split(" ").filter((t) => t.length > 1);
      for (const t of vt) {
        if (ct.some((u) => u === t || (t.length >= 4 && (u.startsWith(t) || t.startsWith(u))))) score += 25;
      }
    }
    if (score > 0) scored.push({ ck, score, hh });
  }
  scored.sort((a, b) => b.score - a.score);
  const top = scored[0];
  if (top && top.score >= 25) return { histSg: top.hh, resolvedKey: top.ck };
  return null;
}

/** When venue SG is unavailable, derive axis weights from skill spread in the field (fallback emphasis). */
function courseFitFieldVarianceEmphasis(rows, ranges) {
  const vals = COURSE_FIT_AXIS_KEYS.map(() => []);
  for (const r of rows) {
    const nv = courseFitNormalizeRaw(courseFitRawProfile(r), ranges);
    for (let i = 0; i < 5; i++) vals[i].push(nv[i]);
  }
  const spread = vals.map((xs) => {
    const f = xs.filter(Number.isFinite);
    if (f.length < 2) return 0.2;
    const m = f.reduce((a, b) => a + b, 0) / f.length;
    return Math.sqrt(f.reduce((s, x) => s + (x - m) ** 2, 0) / f.length);
  });
  const mx = Math.max(...spread, 1e-6);
  return spread.map((s) => (s / mx) * 0.35 + 0.05);
}

/** Blend field-average radar with historical SG means at this venue (embedded history). */
function courseFitVenueProfileVector(rows, ranges, histSg) {
  const tour = courseFitMeanNormalized(rows, ranges);
  if (!histSg) return tour.slice();
  const v = tour.slice();
  v[2] = courseFitNormScalar(histSg.app, ranges[2]);
  v[3] = courseFitNormScalar(histSg.arg, ranges[3]);
  v[4] = courseFitNormScalar(histSg.putt, ranges[4]);
  const ovs = rows.map((r) => num(r.sg_ott, NaN)).filter(Number.isFinite);
  if (ovs.length && Number.isFinite(histSg.ott)) {
    const lo = Math.min(...ovs);
    const hi = Math.max(...ovs);
    const span = hi - lo < 1e-9 ? 1 : hi - lo;
    v[0] = clamp((histSg.ott - lo) / span, 0, 1);
  }
  return v;
}

/** Radar spokes + Who-fits category names (Course Fit). */
const COURSE_FIT_RADAR_SPOKE_LABELS = [
  "Driving Accuracy",
  "Off Tee",
  "Approach",
  "Around Green",
  "Putting",
  "Driving Distance",
];

/** Per-course mean SG vector [ott,app,arg,putt] from embedded history (for similarity). */
function courseFitMeanSgVectorByCourse() {
  const acc = courseFitCourseStatsByCourse();
  /** @type {Map<string, number[]>} */
  const out = new Map();
  for (const [ck, b] of acc) {
    const totalCt = b.ct.reduce((a, c) => a + c, 0);
    if (totalCt < 24) continue;
    const vec = b.sum.map((s, j) => (b.ct[j] ? s / b.ct[j] : NaN));
    if (vec.every(Number.isFinite)) out.set(ck, vec);
  }
  return out;
}

function courseFitSimilarCourses(venueKeyNorm, histSg) {
  const map = courseFitMeanSgVectorByCourse();
  const base = histSg ? [histSg.ott, histSg.app, histSg.arg, histSg.putt] : null;
  if (!base || !base.every(Number.isFinite)) return [];
  const rows = [];
  for (const [ck, vec] of map) {
    if (ck === venueKeyNorm) continue;
    const d = Math.hypot(vec[0] - base[0], vec[1] - base[1], vec[2] - base[2], vec[3] - base[3]);
    rows.push({ ck, dist: d, sim: 1 / (1 + d) });
  }
  rows.sort((a, b) => b.sim - a.sim);
  return rows.slice(0, 12);
}

function courseFitPlayerPool() {
  const rnd = getOuRound();
  let rows = (DATA.players || []).filter((p) => samePlayerRound(p, rnd));
  if (tournamentPostCutListPhase()) rows = rows.filter((p) => !isPlayerEliminatedFromEvent(p));
  return rows;
}

function courseFitBestCategoryAndFit(playerN, emphasis) {
  let bestI = 0;
  let best = -Infinity;
  const n = Math.min(playerN.length, emphasis.length, COURSE_FIT_RADAR_SPOKE_LABELS.length);
  for (let i = 0; i < n; i++) {
    const c = Math.max(0, emphasis[i]) * (playerN[i] - 0.5);
    if (c > best) {
      best = c;
      bestI = i;
    }
  }
  const strokeLike =
    emphasis.reduce((s, e, i) => s + Math.max(0, e) * (playerN[i] - 0.5), 0) * 2.4;
  return { cat: COURSE_FIT_RADAR_SPOKE_LABELS[bestI] || "—", fit: strokeLike };
}

/** Legacy: best +EV book for one outright market (make_cut / mc or fallback). */
function courseFitOutrightBestBookOddsSingle(marketKey, dgId) {
  const elim = dgIdsEliminatedFromEventPostCut();
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id))
    return { html: "—" };
  if (elim.size && elim.has(id) && marketKey !== "make_cut" && marketKey !== "mc") {
    return { html: "—" };
  }
  const pack = DATA.outrights?.[marketKey];
  if (!pack?.rows) return { html: "—" };
  const row = pack.rows.find((r) => Math.round(num(r.dg_id, NaN)) === id);
  if (!row) return { html: "—" };
  const bookKeys = Array.isArray(pack.bookKeys)
    ? pack.bookKeys.filter((k) => k && k !== "datagolf" && outrightLadderSportsbookAllowed(k))
    : [];
  const modelP = modelProbOutrightFromRowOrProjections(row, marketKey);
  let bestBook = "";
  let bestAm = NaN;
  let bestEv = NaN;
  const modelOk = Number.isFinite(modelP) && modelP > 0;
  for (const bk of bookKeys) {
    const bkNorm = normalizeEvSportsbookKey(bk);
    const pct = impliedPctFromOutrightBookField(row[bk] ?? row[bkNorm]);
    if (!Number.isFinite(pct) || pct <= 0 || !modelOk) continue;
    let pBook = pct / 100;
    pBook = outrightFeedPlaceholderProbNaN(pBook, marketKey, bk);
    if (!Number.isFinite(pBook) || pBook <= 0 || pBook >= 1) continue;
    const ev = outrightEvFromModelAndBook(modelP, pBook, marketKey);
    if (!Number.isFinite(ev)) continue;
    const am = americanFromImpliedProb(pBook);
    if (!Number.isFinite(bestEv) || ev > bestEv) {
      bestEv = ev;
      bestBook = bkNorm || bk;
      bestAm = am;
    }
  }
  if (!bestBook || !Number.isFinite(bestAm)) return { html: "—" };
  return {
    html: `${bookBadgeHtml(bestBook)} <span class="course-fit-out-odds">${formatAmerican(Math.round(bestAm))}</span>`,
  };
}

const COURSE_FIT_ALLOWED_OUTRIGHT_BOOKS = new Set([
  "draftkings",
  "fanduel",
  "bet365",
  "betmgm",
  "pinnacle",
  "betcris",
  "betonline",
]);

function courseFitOutrightBestPriceOdds(marketKey, dgId) {
  const mk = String(marketKey || "");
  const elim = dgIdsEliminatedFromEventPostCut();
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return { html: "—", book: "", am: NaN, dec: NaN };
  if (elim.size && elim.has(id) && mk !== "make_cut" && mk !== "mc") {
    return { html: "—", book: "", am: NaN, dec: NaN };
  }
  const pack = DATA.outrights?.[mk];
  const row = Array.isArray(pack?.rows) ? pack.rows.find((r) => Math.round(num(r.dg_id, NaN)) === id) : null;
  if (!row) return { html: "—", book: "", am: NaN, dec: NaN };
  const rawBookKeys = Array.isArray(pack?.bookKeys) && pack.bookKeys.length ? pack.bookKeys : Object.keys(row);
  let bestBook = "";
  let bestAm = NaN;
  let bestDec = NaN;
  const seen = new Set();
  for (const bkRaw of rawBookKeys) {
    const bkNorm = normalizeEvSportsbookKey(bkRaw);
    if (!bkNorm || seen.has(bkNorm) || !COURSE_FIT_ALLOWED_OUTRIGHT_BOOKS.has(bkNorm)) continue;
    seen.add(bkNorm);
    const pct = impliedPctFromOutrightBookField(row[bkRaw] ?? row[bkNorm]);
    if (!Number.isFinite(pct) || pct <= 0) continue;
    const pBook = outrightFeedPlaceholderProbNaN(pct / 100, mk, bkNorm);
    if (!Number.isFinite(pBook) || pBook <= 0 || pBook >= 1) continue;
    const dec = 1 / pBook;
    const am = americanFromImpliedProb(pBook);
    if (!Number.isFinite(dec) || !Number.isFinite(am)) continue;
    if (!Number.isFinite(bestDec) || dec > bestDec) {
      bestDec = dec;
      bestAm = am;
      bestBook = bkNorm;
    }
  }
  if (!bestBook || !Number.isFinite(bestAm) || !Number.isFinite(bestDec)) {
    return { html: "—", book: "", am: NaN, dec: NaN };
  }
  return {
    html: `${bookBadgeHtml(bestBook)} <span class="course-fit-out-odds">${formatAmerican(Math.round(bestAm))}</span>`,
    book: bestBook,
    am: Math.round(bestAm),
    dec: bestDec,
  };
}

function courseFitDraftKingsOutrightOdds(marketKey, dgId) {
  const mk = String(marketKey || "");
  if (!["win", "top_5", "top_10", "top_20"].includes(mk)) return { html: "—" };
  const elim = dgIdsEliminatedFromEventPostCut();
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return { html: "—" };
  if (elim.size && elim.has(id)) return { html: "—" };
  const pack = DATA.outrights?.[mk];
  const row = Array.isArray(pack?.rows) ? pack.rows.find((r) => Math.round(num(r.dg_id, NaN)) === id) : null;
  if (!row) return { html: "—" };
  const pct = impliedPctFromOutrightBookField(row.draftkings);
  if (!Number.isFinite(pct) || pct <= 0) return { html: "—" };
  const pBook = outrightFeedPlaceholderProbNaN(pct / 100, mk, "draftkings");
  if (!Number.isFinite(pBook) || pBook <= 0 || pBook >= 1) return { html: "—" };
  const am = americanFromImpliedProb(pBook);
  if (!Number.isFinite(am)) return { html: "—" };
  return {
    html: `${bookBadgeHtml("draftkings")} <span class="course-fit-out-odds">${formatAmerican(Math.round(am))}</span>`,
  };
}

function courseFitDraftKingsFinishOddsIndex() {
  const out = new Map();
  for (const [id, markets] of draftKingsFinishOddsByDgIndex()) {
    const row = {};
    for (const mk of ["win", "top_5", "top_10", "top_20"]) {
      if (Number.isFinite(markets[mk]?.am)) row[mk] = markets[mk].am;
    }
    out.set(id, row);
  }
  return out;
}

function courseFitDraftKingsOutrightOddsFromIndex(index, marketKey, dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return { html: "—" };
  const am = index?.get(id)?.[String(marketKey || "")];
  if (!Number.isFinite(am)) return { html: "—" };
  return {
    html: `${bookBadgeHtml("draftkings")} <span class="course-fit-out-odds">${formatAmerican(am)}</span>`,
  };
}

/**
 * Course Fit finish columns show the best available posted sportsbook price from the API feed.
 * DataGolf model prices are excluded; those are fair lines, not book offers.
 */
function courseFitOutrightBestBookOdds(marketKey, dgId) {
  return courseFitOutrightBestPriceOdds(marketKey, dgId);
}

function drawCourseFitRadar(canvas, tour5, venue5, player5, similar5) {
  if (!canvas || typeof canvas.getContext !== "function") return;
  const ctx = canvas.getContext("2d");
  if (!ctx) return;
  const dpr = typeof window !== "undefined" ? window.devicePixelRatio || 1 : 1;
  const rect = canvas.getBoundingClientRect();
  const W = Math.max(280, rect.width || 520);
  const H = Math.max(260, Math.round(rect.width * 0.82) || 400);
  canvas.width = W * dpr;
  canvas.height = H * dpr;
  canvas.style.width = `${W}px`;
  canvas.style.height = `${H}px`;
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, W, H);
  const cx = W / 2;
  const cy = H / 2 + 8;
  const R = Math.min(W, H) * 0.36;
  const n = 6;
  const padRadar = (arr) => {
    const a = Array.isArray(arr) ? arr.map((x) => num(x, 0.5)) : [];
    while (a.length < n) a.push(0.5);
    return a.slice(0, n);
  };
  const tV = padRadar(tour5);
  const vV = padRadar(venue5);
  const pV = padRadar(player5);
  const sV = similar5 && similar5.length >= n ? padRadar(similar5) : null;
  const tau = (Math.PI * 2) / n;
  const angleAt = (i) => -Math.PI / 2 + i * tau;

  ctx.strokeStyle = "rgba(255,255,255,0.12)";
  ctx.lineWidth = 1;
  for (let ring = 1; ring <= 4; ring++) {
    const rr = (R * ring) / 4;
    ctx.beginPath();
    for (let i = 0; i <= n; i++) {
      const a = angleAt(i % n);
      const x = cx + rr * Math.cos(a);
      const y = cy + rr * Math.sin(a);
      if (i === 0) ctx.moveTo(x, y);
      else ctx.lineTo(x, y);
    }
    ctx.closePath();
    ctx.stroke();
  }
  for (let i = 0; i < n; i++) {
    const a = angleAt(i);
    ctx.beginPath();
    ctx.moveTo(cx, cy);
    ctx.lineTo(cx + R * Math.cos(a), cy + R * Math.sin(a));
    ctx.stroke();
  }

  const poly = (pts, stroke, fill, dash) => {
    ctx.beginPath();
    for (let i = 0; i < n; i++) {
      const t = pts[i] ?? 0.5;
      const rr = R * (0.25 + 0.75 * clamp(t, 0, 1));
      const a = angleAt(i);
      const x = cx + rr * Math.cos(a);
      const y = cy + rr * Math.sin(a);
      if (i === 0) ctx.moveTo(x, y);
      else ctx.lineTo(x, y);
    }
    ctx.closePath();
    if (fill) {
      ctx.fillStyle = fill;
      ctx.fill();
    }
    ctx.strokeStyle = stroke;
    ctx.lineWidth = 2;
    ctx.setLineDash(dash || []);
    ctx.stroke();
    ctx.setLineDash([]);
  };

  poly(tV, "rgba(140,148,168,0.95)", null, [6, 4]);
  poly(vV, "rgba(0,196,107,0.95)", "rgba(0,196,107,0.14)", []);
  if (sV) {
    poly(sV, "rgba(156, 162, 180, 0.95)", "rgba(156, 162, 180, 0.12)", []);
  }
  poly(pV, "rgba(245,166,35,0.98)", "rgba(245,166,35,0.12)", []);

  ctx.fillStyle = "rgba(180,186,198,0.95)";
  ctx.font = "600 10px DM Sans, system-ui, sans-serif";
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";
  for (let i = 0; i < n; i++) {
    const a = angleAt(i);
    const lab = COURSE_FIT_RADAR_SPOKE_LABELS[i] || "";
    const xr = cx + (R + 30) * Math.cos(a);
    const yr = cy + (R + 30) * Math.sin(a);
    const words = lab.split(/\s+/).filter(Boolean);
    words.forEach((w, j) => {
      ctx.fillText(w, xr, yr + j * 11 - ((words.length - 1) * 5.5));
    });
  }
}

let courseFitRadarResizeBound = false;
/** Normalized course key from "Course similarity" list; shown as an extra radar overlay. */
let courseFitSimilarSelectedKey = null;
let courseFitSimilarListClickBound = false;
/** User-chosen venue for radar / similarity (normalized key); null = use this event's course from projections. */
let courseFitVenueFilterKey = null;
/** When projections switch events, reset venue filter and similarity selection. */
let courseFitVenueEventKeyTracked = "";
let courseFitGolferDefaultApplied = false;
let courseFitTableSortBound = false;
let courseFitTableSort = { key: "fit", dir: -1 };

function courseFitDefaultSortDir(key) {
  return key === "golfer" || key === "category" ? 1 : -1;
}

function updateCourseFitTableSortIndicators() {
  const table = document.getElementById("table-course-fit");
  if (!table) return;
  table.querySelectorAll("thead th.sortable[data-course-fit-sort]").forEach((th) => {
    const key = String(th.getAttribute("data-course-fit-sort") || "");
    const active = key && key === courseFitTableSort.key;
    th.setAttribute("aria-sort", active ? (courseFitTableSort.dir > 0 ? "ascending" : "descending") : "none");
    const up = th.querySelector(".sort-up");
    const dn = th.querySelector(".sort-down");
    if (up) up.classList.toggle("active", active && courseFitTableSort.dir > 0);
    if (dn) dn.classList.toggle("active", active && courseFitTableSort.dir < 0);
  });
}

function initCourseFitTableSortOnce() {
  const table = document.getElementById("table-course-fit");
  if (!table || courseFitTableSortBound) return;
  courseFitTableSortBound = true;
  table.querySelector("thead")?.addEventListener("click", (ev) => {
    const th = ev.target.closest("th.sortable[data-course-fit-sort]");
    if (!th || !table.contains(th)) return;
    const key = String(th.getAttribute("data-course-fit-sort") || "");
    if (!key) return;
    if (courseFitTableSort.key === key) {
      courseFitTableSort = { key, dir: -courseFitTableSort.dir };
    } else {
      courseFitTableSort = { key, dir: courseFitDefaultSortDir(key) };
    }
    buildCourseFitTab();
  });
  updateCourseFitTableSortIndicators();
}

function courseFitPrettyCourseKey(ck) {
  return String(ck || "")
    .trim()
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

function initCourseFitSimilarListClick() {
  const panel = document.getElementById("panel-course-fit");
  if (!panel || courseFitSimilarListClickBound) return;
  courseFitSimilarListClickBound = true;
  function activateSimilarLi(li) {
    const ck = String(li.getAttribute("data-course-fit-ck") || "").trim();
    if (!ck) return;
    courseFitSimilarSelectedKey = courseFitSimilarSelectedKey === ck ? null : ck;
    buildCourseFitTab();
  }
  panel.addEventListener("click", (ev) => {
    const li = ev.target.closest("li.course-fit-similar-li[data-course-fit-ck]");
    if (!li) return;
    ev.preventDefault();
    activateSimilarLi(li);
  });
  panel.addEventListener("keydown", (ev) => {
    if (ev.key !== "Enter" && ev.key !== " ") return;
    const li = ev.target.closest("li.course-fit-similar-li[data-course-fit-ck]");
    if (!li) return;
    ev.preventDefault();
    activateSimilarLi(li);
  });
}

async function loadApproachSkillYtdJson() {
  if (approachSkillYtdCache) return approachSkillYtdCache;
  if (approachSkillYtdLoadPromise) return approachSkillYtdLoadPromise;
  approachSkillYtdLoadPromise = (async () => {
    for (const name of ["approach_skill_ytd.json", "approach_skill_l12.json"]) {
      try {
        const res = await fetch(cacheBustFetchUrl(name), { cache: "no-store" });
        if (!res.ok) continue;
        const j = await res.json();
        if (j && typeof j === "object" && Array.isArray(j.players)) {
          approachSkillYtdCache = j;
          return approachSkillYtdCache;
        }
      } catch {
        /* try next */
      }
    }
    approachSkillYtdCache = null;
    return null;
  })().finally(() => {
    approachSkillYtdLoadPromise = null;
  });
  return approachSkillYtdLoadPromise;
}

function initCourseFitSubtabs() {
  const root = document.querySelector(".course-fit-subtabs");
  if (!root || root.dataset.bound === "1") return;
  root.dataset.bound = "1";
  root.querySelectorAll("[data-course-fit-subtab]").forEach((btn) => {
    btn.addEventListener("click", () => setCourseFitSubtab(String(btn.getAttribute("data-course-fit-subtab") || "")));
  });
}

function setCourseFitSubtab(id) {
  const adj = document.getElementById("course-fit-subpanel-adjustments");
  const shot = document.getElementById("course-fit-subpanel-shots");
  if (!adj || !shot) return;
  const active = id === "shots" ? "shots" : "adjustments";
  adj.hidden = active !== "adjustments";
  shot.hidden = active !== "shots";
  document.querySelectorAll("[data-course-fit-subtab]").forEach((btn) => {
    const on = String(btn.getAttribute("data-course-fit-subtab") || "") === active;
    btn.classList.toggle("active", on);
    btn.setAttribute("aria-selected", on ? "true" : "false");
  });
  if (active === "shots") buildCourseFitTab();
}

function formatAmericanOddsShort(n) {
  const x = num(n, NaN);
  if (!Number.isFinite(x)) return "—";
  if (x === 0) return "EV";
  return x > 0 ? `+${Math.round(x)}` : `${Math.round(x)}`;
}

/** Prefer DraftKings rows from merged props for O/U display. */
function courseFitFindDraftKingsOuProp(dgId, playerName, market) {
  const props = ouRoundOuPropsForLines();
  const dk = props.filter((r) => String(r.source || "").trim().toLowerCase() === "draftkings");
  const pool = dk.length ? dk : props;
  const pid = Math.round(num(dgId, NaN));
  const want = String(market || "").trim();
  const pk = playerKeyFromName(String(playerName || ""));
  for (const r of pool) {
    if (String(r.market || "").trim() !== want) continue;
    const id = Math.round(num(r.dg_id, NaN));
    if (Number.isFinite(pid) && pid > 0 && id === pid) return r;
  }
  for (const r of pool) {
    if (String(r.market || "").trim() !== want) continue;
    if (playerKeyFromName(String(r.player_name || "")) === pk) return r;
  }
  return null;
}

const COURSE_FIT_BIN_LABELS = {
  put: ["putts from 2-5 feet", "putts from 5-30 feet", "putts from 30+ feet"],
  rough: ["rough approach under 150 yards", "rough approach 150+ yards"],
  fw: [
    "fairway approach 50-100 yards",
    "fairway approach 100-150 yards",
    "fairway approach 150-200 yards",
    "fairway approach 200+ yards",
  ],
};

function courseFitBinTooltipHide() {
  const tip = document.getElementById("course-fit-bin-tooltip");
  if (tip) tip.hidden = true;
}

function ensureCourseFitBinTooltipHandlers() {
  const panel = document.getElementById("panel-course-fit");
  if (!panel || panel.dataset.cfTipBound === "1") return;
  panel.dataset.cfTipBound = "1";
  panel.addEventListener("pointermove", (ev) => {
    const td = ev.target.closest(".course-fit-shot-bin-td");
    const tip = document.getElementById("course-fit-bin-tooltip");
    if (!td || !tip || td.closest("#course-fit-shot-tbody") === null) {
      courseFitBinTooltipHide();
      return;
    }
    courseFitBinTooltipShow(td, ev.clientX, ev.clientY);
  });
  panel.addEventListener("pointerleave", () => courseFitBinTooltipHide());
}

function courseFitBinTooltipShow(td, clientX, clientY) {
  const tip = document.getElementById("course-fit-bin-tooltip");
  const tr = td.closest("tr");
  if (!tip || !tr) return;
  const dg = Math.round(num(tr.dataset.dgId, NaN));
  const zone = String(td.dataset.cfZone || "");
  const idx = Math.round(num(td.dataset.cfIdx, NaN));
  const pred = num(td.dataset.cfPred, NaN);
  const field = num(td.dataset.cfField, NaN);
  const rows = courseFitPlayerPool();
  const prow = rows.find((r) => Math.round(num(r.dg_id, NaN)) === dg);
  const nm = displayGolferName(String(prow?.player_name || tr.querySelector(".course-fit-shot-player")?.textContent || ""));
  const girs = rows.map((r) => num(r.gir, NaN)).filter(Number.isFinite);
  const fws = rows.map((r) => num(r.fairways, NaN)).filter(Number.isFinite);
  const puttsAll = rows.map((r) => num(r.putts, NaN)).filter(Number.isFinite);
  const meanGir = girs.length ? girs.reduce((s, x) => s + x, 0) / girs.length : NaN;
  const meanFw = fws.length ? fws.reduce((s, x) => s + x, 0) / fws.length : NaN;
  const meanPutts = puttsAll.length ? puttsAll.reduce((s, x) => s + x, 0) / puttsAll.length : NaN;

  let binPhrase = "shots";
  if (zone === "put") binPhrase = COURSE_FIT_BIN_LABELS.put[idx] || "putting bin";
  else if (zone === "rough") binPhrase = COURSE_FIT_BIN_LABELS.rough[idx] || "rough bin";
  else if (zone === "fw") binPhrase = COURSE_FIT_BIN_LABELS.fw[idx] || "fairway approach bin";

  const mainEl = tip.querySelector(".course-fit-tip-main");
  const statEl = tip.querySelector(".course-fit-tip-stat");
  const dkEl = tip.querySelector(".course-fit-tip-dk");
  const predNumEl = tip.querySelector(".course-fit-tip-pred-num");
  const hl = tip.querySelector(".course-fit-tip-highlight");
  const dotF = tip.querySelector(".course-fit-tip-dot-field");
  const dotP = tip.querySelector(".course-fit-tip-dot-player");
  const lineEl = tip.querySelector(".course-fit-tip-line");
  if (mainEl) {
    const pStr = Number.isFinite(pred) ? pred.toFixed(1) : "—";
    const fStr = Number.isFinite(field) ? field.toFixed(1) : "—";
    mainEl.innerHTML = `We predict <strong>${escapeHtml(nm)}</strong> will hit <strong>${pStr}</strong> ${escapeHtml(binPhrase)} per round (scaled). At this field's average in-bin profile we use <strong>${fStr}</strong> per round.`;
  }

  const span = Math.max(Math.abs(pred - field) * 2.5, 0.35, Math.abs(pred - field) + 0.25);
  const lo = Math.min(pred, field) - span * 0.25;
  const hi = Math.max(pred, field) + span * 0.25;
  const range = Math.max(hi - lo, 1e-6);
  const fp = ((field - lo) / range) * 100;
  const pp = ((pred - lo) / range) * 100;
  if (dotF) dotF.style.left = `${clamp(fp, 5, 95)}%`;
  if (dotP) dotP.style.left = `${clamp(pp, 5, 95)}%`;
  if (hl) {
    const left = Math.min(fp, pp);
    const w = Math.abs(pp - fp);
    hl.style.left = `${clamp(left, 0, 100)}%`;
    hl.style.width = `${clamp(w, 2, 90)}%`;
  }
  if (lineEl) {
    lineEl.style.left = `${clamp(Math.min(fp, pp), 5, 95)}%`;
    lineEl.style.width = `${clamp(Math.abs(pp - fp), 0, 90)}%`;
  }
  if (predNumEl) predNumEl.textContent = Number.isFinite(pred) ? pred.toFixed(1) : "—";

  if (statEl) {
    let line = "";
    if (zone === "put" && prow) {
      const mp = num(prow.putts, NaN);
      line = `Putts (model round): <strong>${Number.isFinite(mp) ? mp.toFixed(1) : "—"}</strong> vs field avg <strong>${Number.isFinite(meanPutts) ? meanPutts.toFixed(1) : "—"}</strong>.`;
    } else if (zone === "rough" && prow) {
      const mf = num(prow.fairways, NaN);
      line = `Fairways hit (model round): <strong>${Number.isFinite(mf) ? mf.toFixed(1) : "—"}</strong> vs field avg <strong>${Number.isFinite(meanFw) ? meanFw.toFixed(1) : "—"}</strong>.`;
    } else if (zone === "fw" && prow) {
      const mg = num(prow.gir, NaN);
      line = `GIR (model round): <strong>${Number.isFinite(mg) ? mg.toFixed(1) : "—"}</strong> vs field avg <strong>${Number.isFinite(meanGir) ? meanGir.toFixed(1) : "—"}</strong>.`;
    }
    statEl.innerHTML = line || "";
  }

  if (dkEl) {
    dkEl.hidden = true;
    dkEl.textContent = "";
    const mk =
      zone === "put" ? "Putts" : zone === "rough" ? "Fairways hit" : zone === "fw" ? "GIR" : "";
    const dkRow = mk ? courseFitFindDraftKingsOuProp(dg, nm, mk) : null;
    const rLab = num(DATA?.meta?.display_round, NaN);
    const rStr = Number.isFinite(rLab) && rLab >= 1 && rLab <= 4 ? `R${Math.round(rLab)}` : "";
    if (dkRow && Number.isFinite(num(dkRow.line, NaN))) {
      const L = num(dkRow.line, NaN);
      dkEl.hidden = false;
      dkEl.innerHTML =
        `${rStr ? `${rStr} · ` : ""}Line <strong>${L.toFixed(L % 1 === 0 ? 0 : 1)}</strong> · O ${formatAmericanOddsShort(dkRow.over_odds)} / U ${formatAmericanOddsShort(dkRow.under_odds)}`;
    }
  }

  tip.hidden = false;
  const pad = 14;
  let x = clientX + pad;
  let y = clientY + pad;
  const rect = tip.getBoundingClientRect();
  const vw = typeof window !== "undefined" ? window.innerWidth : 1200;
  const vh = typeof window !== "undefined" ? window.innerHeight : 800;
  if (x + rect.width > vw - 8) x = clientX - rect.width - pad;
  if (y + rect.height > vh - 8) y = clientY - rect.height - pad;
  tip.style.left = `${clamp(x, 8, vw - rect.width - 8)}px`;
  tip.style.top = `${clamp(y, 8, vh - rect.height - 8)}px`;
}

/** Putting buckets: not in DataGolf JSON — split projected putts using SG putting percentile vs the field. */
function courseFitPuttingThreeBins(putts, sgPutt, sortedSgPuttField) {
  if (!Number.isFinite(putts) || putts <= 0 || !sortedSgPuttField.length || !Number.isFinite(sgPutt)) {
    return [NaN, NaN, NaN];
  }
  let below = 0;
  for (let i = 0; i < sortedSgPuttField.length; i++) {
    if (sortedSgPuttField[i] < sgPutt) below++;
    else break;
  }
  const rank = sortedSgPuttField.length ? below / sortedSgPuttField.length : 0.5;
  const wShort = 0.09 + 0.06 * rank;
  const wLong = 0.47 - 0.1 * rank;
  const wMid = 1 - wShort - wLong;
  return [putts * wShort, putts * wMid, putts * wLong];
}

function courseFitPuttingFieldBins(meanPutts) {
  if (!Number.isFinite(meanPutts) || meanPutts <= 0) return [NaN, NaN, NaN];
  const rank = 0.5;
  const wShort = 0.09 + 0.06 * rank;
  const wLong = 0.47 - 0.1 * rank;
  const wMid = 1 - wShort - wLong;
  return [meanPutts * wShort, meanPutts * wMid, meanPutts * wLong];
}

function courseFitShotBinStripHtml(playerVal, fieldVal, lo, hi, skillPositive) {
  const span = Math.max(hi - lo, 1e-6);
  const fp = ((num(fieldVal, NaN) - lo) / span) * 100;
  const pp = ((num(playerVal, NaN) - lo) / span) * 100;
  const fpC = clamp(Number.isFinite(fp) ? fp : 50, 0, 100);
  const ppC = clamp(Number.isFinite(pp) ? pp : 50, 0, 100);
  const cls = skillPositive ? "course-fit-bin-player course-fit-bin-good" : "course-fit-bin-player course-fit-bin-bad";
  const disp = Number.isFinite(num(playerVal, NaN)) ? num(playerVal, 0).toFixed(1) : "—";
  return `<div class="course-fit-bin-cell-inner">
    <span class="course-fit-bin-val">${disp}</span>
    <div class="course-fit-bin-track" role="presentation">
      <span class="course-fit-bin-field-dot" style="left:${fpC}%"></span>
      <span class="${cls}" style="left:${ppC}%"></span>
    </div>
  </div>`;
}

function buildCourseFitShotBinsTable(rows, approachPayload, venueName, searchShots) {
  const tbody = document.getElementById("course-fit-shot-tbody");
  const headEl = document.getElementById("course-fit-shot-heading");
  if (!tbody) return;

  /** Approach-skill `*_shot_count` values are L12 totals; scale to ~per-round for display (DataGolf-style). */
  const AP_SKILL_COUNT_PER_ROUND_DIV = 45;

  if (headEl) headEl.textContent = `Where are players hitting shots from at ${venueName}?`;

  const q = String(searchShots || "")
    .trim()
    .toLowerCase();
  const asMap = new Map();
  const plist = Array.isArray(approachPayload?.players) ? approachPayload.players : [];
  for (const p of plist) {
    const id = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(id)) asMap.set(id, p);
  }

  const sgPuttSorted = rows
    .map((r) => num(r.sg_putt, NaN))
    .filter(Number.isFinite)
    .sort((a, b) => a - b);
  const puttsVals = rows.map((r) => num(r.putts, NaN)).filter(Number.isFinite);
  const meanPutts = puttsVals.length ? puttsVals.reduce((s, x) => s + x, 0) / puttsVals.length : NaN;
  const meanSgPutt = sgPuttSorted.length ? sgPuttSorted.reduce((s, x) => s + x, 0) / sgPuttSorted.length : NaN;

  const fieldPutBins = courseFitPuttingFieldBins(meanPutts);

  const ROUGH_KEYS = ["under_150_rgh_shot_count", "over_150_rgh_shot_count"];
  const ROUGH_SG_KEYS = ["under_150_rgh_sg_per_shot", "over_150_rgh_sg_per_shot"];
  const FW_KEYS = ["50_100_fw_shot_count", "100_150_fw_shot_count", "150_200_fw_shot_count", "over_200_fw_shot_count"];
  const FW_SG_KEYS = ["50_100_fw_sg_per_shot", "100_150_fw_sg_per_shot", "150_200_fw_sg_per_shot", "over_200_fw_sg_per_shot"];

  const meanShot = {};
  const meanSg = {};
  for (const k of [...ROUGH_KEYS, ...FW_KEYS]) {
    const xs = [];
    for (const r of rows) {
      const a = asMap.get(Math.round(num(r.dg_id, NaN)));
      if (!a) continue;
      const v = num(a[k], NaN);
      if (Number.isFinite(v)) xs.push(v / AP_SKILL_COUNT_PER_ROUND_DIV);
    }
    meanShot[k] = xs.length ? xs.reduce((s, x) => s + x, 0) / xs.length : NaN;
  }
  for (const k of [...ROUGH_SG_KEYS, ...FW_SG_KEYS]) {
    const xs = [];
    for (const r of rows) {
      const a = asMap.get(Math.round(num(r.dg_id, NaN)));
      if (!a) continue;
      const v = num(a[k], NaN);
      if (Number.isFinite(v)) xs.push(v);
    }
    meanSg[k] = xs.length ? xs.reduce((s, x) => s + x, 0) / xs.length : NaN;
  }

  /** @type {Array<{ nm: string; dg: number; put: number[]; rough: number[]; fw: number[]; sgPutt: number; ap: object }>} */
  const built = [];
  for (const r of rows) {
    const nm = displayGolferName(String(r.player_name || ""));
    if (q && !nm.toLowerCase().includes(q)) continue;
    const dg = Math.round(num(r.dg_id, NaN));
    const ap = asMap.get(dg);
    if (!ap) continue;
    const put = courseFitPuttingThreeBins(num(r.putts, NaN), num(r.sg_putt, NaN), sgPuttSorted);
    const rough = [
      num(ap[ROUGH_KEYS[0]], NaN) / AP_SKILL_COUNT_PER_ROUND_DIV,
      num(ap[ROUGH_KEYS[1]], NaN) / AP_SKILL_COUNT_PER_ROUND_DIV,
    ];
    const fw = FW_KEYS.map((k) => num(ap[k], NaN) / AP_SKILL_COUNT_PER_ROUND_DIV);
    built.push({ nm, dg, put, rough, fw, sgPutt: num(r.sg_putt, NaN), ap });
  }

  tbody.innerHTML = "";

  if (!plist.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 10;
    td.className = "text-muted";
    td.innerHTML =
      "No <code>approach_skill_ytd.json</code> data. Run <code>npm run fetch:dg</code> with <code>DATAGOLF_API_KEY</code> (or <code>datagolf.local.json</code>) to embed DataGolf <code>preds/approach-skill</code> (year-to-date).";
    tr.appendChild(td);
    tbody.appendChild(tr);
    return;
  }

  if (!built.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 10;
    td.className = "text-muted";
    td.textContent = q
      ? "No players match this search with approach-skill rows."
      : "No overlapping players between the projection field and approach-skill export.";
    tr.appendChild(td);
    tbody.appendChild(tr);
    return;
  }

  const colValsPut = [[], [], []];
  const colValsRough = [[], []];
  const colValsFw = [[], [], [], []];
  for (const b of built) {
    for (let i = 0; i < 3; i++) colValsPut[i].push(b.put[i]);
    for (let j = 0; j < 2; j++) colValsRough[j].push(b.rough[j]);
    for (let j = 0; j < 4; j++) colValsFw[j].push(b.fw[j]);
  }

  const rangePut = fieldPutBins.map((fv, i) => {
    const xs = colValsPut[i].filter(Number.isFinite);
    const lo0 = xs.length ? Math.min(...xs, fv) : fv;
    const hi0 = xs.length ? Math.max(...xs, fv) : fv;
    const lo = lo0 - 0.05;
    const hi = hi0 + 0.05;
    return { lo: hi - lo < 0.08 ? lo - 0.15 : lo, hi: hi - lo < 0.08 ? hi + 0.15 : hi };
  });

  const rangeRough = ROUGH_KEYS.map((key, j) => {
    const fv = meanShot[key];
    const xs = colValsRough[j].filter(Number.isFinite);
    const lo0 = xs.length ? Math.min(...xs, fv) : fv;
    const hi0 = xs.length ? Math.max(...xs, fv) : fv;
    const lo = lo0 - 0.05;
    const hi = hi0 + 0.05;
    return { lo: hi - lo < 0.08 ? lo - 0.2 : lo, hi: hi - lo < 0.08 ? hi + 0.2 : hi };
  });

  const rangeFw = FW_KEYS.map((key, j) => {
    const fv = meanShot[key];
    const xs = colValsFw[j].filter(Number.isFinite);
    const lo0 = xs.length ? Math.min(...xs, fv) : fv;
    const hi0 = xs.length ? Math.max(...xs, fv) : fv;
    const lo = lo0 - 0.05;
    const hi = hi0 + 0.05;
    return { lo: hi - lo < 0.08 ? lo - 0.2 : lo, hi: hi - lo < 0.08 ? hi + 0.2 : hi };
  });

  for (const b of built) {
    const tr = document.createElement("tr");
    tr.dataset.dgId = String(b.dg);

    const tdP = document.createElement("td");
    tdP.className = "course-fit-shot-player";
    tdP.textContent = b.nm;
    tr.appendChild(tdP);

    for (let i = 0; i < 3; i++) {
      const td = document.createElement("td");
      td.className = "num course-fit-shot-bin-td";
      td.dataset.cfZone = "put";
      td.dataset.cfIdx = String(i);
      td.dataset.cfPred = String(b.put[i]);
      td.dataset.cfField = String(fieldPutBins[i]);
      const good = Number.isFinite(b.sgPutt) && Number.isFinite(meanSgPutt) && b.sgPutt >= meanSgPutt;
      td.innerHTML = courseFitShotBinStripHtml(b.put[i], fieldPutBins[i], rangePut[i].lo, rangePut[i].hi, good);
      tr.appendChild(td);
    }

    for (let j = 0; j < 2; j++) {
      const td = document.createElement("td");
      td.className = "num course-fit-shot-bin-td";
      td.dataset.cfZone = "rough";
      td.dataset.cfIdx = String(j);
      td.dataset.cfPred = String(b.rough[j]);
      td.dataset.cfField = String(meanShot[ROUGH_KEYS[j]]);
      const sgKey = ROUGH_SG_KEYS[j];
      const fv = meanShot[ROUGH_KEYS[j]];
      const meanSgV = meanSg[sgKey];
      const good =
        Number.isFinite(num(b.ap[sgKey], NaN)) && Number.isFinite(meanSgV)
          ? num(b.ap[sgKey], NaN) >= meanSgV
          : Number.isFinite(b.sgPutt) && Number.isFinite(meanSgPutt) && b.sgPutt >= meanSgPutt;
      td.innerHTML = courseFitShotBinStripHtml(b.rough[j], fv, rangeRough[j].lo, rangeRough[j].hi, good);
      tr.appendChild(td);
    }

    for (let j = 0; j < 4; j++) {
      const td = document.createElement("td");
      td.className = "num course-fit-shot-bin-td";
      td.dataset.cfZone = "fw";
      td.dataset.cfIdx = String(j);
      td.dataset.cfPred = String(b.fw[j]);
      td.dataset.cfField = String(meanShot[FW_KEYS[j]]);
      const sgKey = FW_SG_KEYS[j];
      const fv = meanShot[FW_KEYS[j]];
      const meanSgV = meanSg[sgKey];
      const good =
        Number.isFinite(num(b.ap[sgKey], NaN)) && Number.isFinite(meanSgV)
          ? num(b.ap[sgKey], NaN) >= meanSgV
          : false;
      td.innerHTML = courseFitShotBinStripHtml(b.fw[j], fv, rangeFw[j].lo, rangeFw[j].hi, good);
      tr.appendChild(td);
    }

    tbody.appendChild(tr);
  }

  ensureCourseFitBinTooltipHandlers();
}

const COURSE_FIT_TABLE_RADAR_KEYS_DEFAULT = Object.freeze([
  "adj_driving_accuracy",
  "ott_sg",
  "app_sg",
  "arg_sg",
  "putt_sg",
  "adj_driving_distance",
]);

function playerDrivingAccuracyFrac(mrow) {
  const da = num(mrow?.driving_accuracy ?? mrow?.driving_acc, NaN);
  if (!Number.isFinite(da)) return NaN;
  return da > 1 ? da / 100 : da;
}

function playerDrivingDistanceYds(mrow) {
  if (!mrow || typeof mrow !== "object") return NaN;
  const yardCandidates = [
    mrow.driving_distance,
    mrow.avg_driving_distance,
    mrow.adj_driving_distance,
    mrow.average_driving_distance,
    mrow.predicted_driving_distance,
    mrow.predicted_avg_driving_distance,
  ];
  for (const c of yardCandidates) {
    const y = num(c, NaN);
    if (Number.isFinite(y) && y >= 235 && y <= 380) return y;
  }
  const dDist = num(mrow.driving_dist, NaN);
  if (Number.isFinite(dDist) && dDist >= 235 && dDist <= 380) return dDist;
  let rt = num(mrow.driving_distance_rating, NaN);
  if (!Number.isFinite(rt) && Number.isFinite(dDist) && dDist >= -120 && dDist <= 120) rt = dDist;
  if (Number.isFinite(rt) && rt >= -120 && rt <= 120) return 302 + rt;
  return NaN;
}

function courseFitPlayerRadarAxisRaw(mrow, axisIndex) {
  if (axisIndex === 0) return playerDrivingAccuracyFrac(mrow);
  if (axisIndex === 1) return num(mrow?.sg_ott, NaN);
  if (axisIndex === 2) return num(mrow?.sg_app, NaN);
  if (axisIndex === 3) return num(mrow?.sg_arg, NaN);
  if (axisIndex === 4) return num(mrow?.sg_putt, NaN);
  if (axisIndex === 5) return playerDrivingDistanceYds(mrow);
  return NaN;
}

function courseFitRadarKeysFromTable() {
  const k = COURSE_TABLE_PAYLOAD?.radarKeys;
  if (Array.isArray(k) && k.length === 6) return k;
  return [...COURSE_FIT_TABLE_RADAR_KEYS_DEFAULT];
}

function resolveCourseTableRowForNormKey(activeVk) {
  const p = COURSE_TABLE_PAYLOAD;
  if (!p?.byNormKey || !activeVk) return null;
  if (p.byNormKey[activeVk]) return p.byNormKey[activeVk];
  let best = null;
  let bestScore = 0;
  for (const key of Object.keys(p.byNormKey)) {
    let score = 0;
    if (key === activeVk) score = 10000;
    else if (activeVk.length >= 5 && (key.includes(activeVk) || activeVk.includes(key)))
      score = 500 + Math.min(key.length, activeVk.length);
    else {
      const vt = activeVk.split(" ").filter((t) => t.length > 2);
      const kt = key.split(" ").filter((t) => t.length > 2);
      for (const t of vt) {
        if (kt.some((u) => u === t || (t.length >= 4 && (u.startsWith(t) || t.startsWith(u))))) score += 22;
      }
    }
    if (score > bestScore) {
      bestScore = score;
      best = p.byNormKey[key];
    }
  }
  return bestScore >= 22 ? best : null;
}

function courseTableScalarNormTo01(v, col) {
  const r = COURSE_TABLE_PAYLOAD?.ranges?.[col];
  if (!Number.isFinite(v) || !r || !Number.isFinite(r.lo) || !Number.isFinite(r.hi)) return 0.5;
  const span = r.hi - r.lo < 1e-9 ? 1 : r.hi - r.lo;
  return clamp((v - r.lo) / span, 0, 1);
}

function courseFitTour5FromCourseTable() {
  const keys = courseFitRadarKeysFromTable();
  const means = COURSE_TABLE_PAYLOAD?.means || {};
  return keys.map((col) => courseTableScalarNormTo01(num(means[col], NaN), col));
}

function courseFitVenue5FromCourseTableRow(row) {
  const keys = courseFitRadarKeysFromTable();
  if (!row) return courseFitTour5FromCourseTable();
  return keys.map((col) => courseTableScalarNormTo01(num(row[col], NaN), col));
}

function courseFitMergedLoHiForCol(col, fieldSamples) {
  const tr = COURSE_TABLE_PAYLOAD?.ranges?.[col];
  const ff = fieldSamples.filter(Number.isFinite);
  const flo = ff.length ? Math.min(...ff) : NaN;
  const fhi = ff.length ? Math.max(...ff) : NaN;
  if (!tr) {
    if (!Number.isFinite(flo) || !Number.isFinite(fhi)) return { lo: 0, hi: 1 };
    return { lo: flo, hi: fhi <= flo ? flo + 1e-3 : fhi };
  }
  if (!ff.length) return { lo: tr.lo, hi: tr.hi };
  return { lo: Math.min(tr.lo, flo), hi: Math.max(tr.hi, fhi) };
}

/** Player profile on the same axes as `courseFitRadarKeysFromTable()` (merged field + course_table ranges). */
function courseFitPlayerRadarVectorMerged(rows, prow) {
  const keys = courseFitRadarKeysFromTable();
  if (!prow) return keys.map(() => 0.5);
  const prm = mergedPlayerRowForDrivingFields(prow);
  const out = [];
  for (let i = 0; i < keys.length; i++) {
    const col = keys[i];
    const samples = rows
      .map((r) => courseFitPlayerRadarAxisRaw(mergedPlayerRowForDrivingFields(r), i))
      .filter(Number.isFinite);
    const { lo, hi } = courseFitMergedLoHiForCol(col, samples);
    const pv = courseFitPlayerRadarAxisRaw(prm, i);
    if (!Number.isFinite(pv)) {
      out.push(0.5);
      continue;
    }
    const span = hi - lo < 1e-9 ? 1 : hi - lo;
    out.push(clamp((pv - lo) / span, 0, 1));
  }
  return out;
}

function courseFitSimilarCoursesFromCourseTable(activeVk, venueRow) {
  const p = COURSE_TABLE_PAYLOAD;
  const keys = courseFitRadarKeysFromTable();
  if (!p?.rows?.length || !venueRow) return [];
  const ref = keys.map((k) => num(venueRow[k], NaN));
  if (!ref.every(Number.isFinite)) return [];
  const out = [];
  for (const row of p.rows) {
    const nk = row._normKey || normCourseNameKey(String(row.course || ""));
    if (!nk || nk === activeVk) continue;
    const vec = keys.map((k) => num(row[k], NaN));
    if (!vec.every(Number.isFinite)) continue;
    const d = Math.hypot(...vec.map((x, j) => x - ref[j]));
    out.push({ ck: nk, dist: d, sim: 1 / (1 + d) });
  }
  out.sort((a, b) => b.sim - a.sim);
  return out.slice(0, 12);
}

/**
 * One or two axis indices where this venue differs most from tour / field average
 * (|venue − field|); the Who-fits table scores only these so categories stay focused.
 */
function courseFitVenueEmphasisAxisIndices(tour5, venue5) {
  const n = Math.min(tour5?.length || 0, venue5?.length || 0, COURSE_FIT_RADAR_SPOKE_LABELS.length);
  if (n < 1) return [];
  const scored = [];
  for (let i = 0; i < n; i++) {
    const stress = venue5[i] - tour5[i];
    scored.push({ i, abs: Math.abs(stress) });
  }
  scored.sort((a, b) => b.abs - a.abs);
  if (!scored[0]?.abs || scored[0].abs < 1e-6) return [scored[0].i];
  const out = [scored[0].i];
  if (
    scored.length > 1 &&
    scored[1].abs >= scored[0].abs * 0.35
  ) {
    out.push(scored[1].i);
  }
  return out.slice(0, 2);
}

/** Fit sums venue-emphasis axes; category is exactly one of those axes — where this player’s edge is largest (at most two distinct labels in the column). */
function courseFitPlayerCatAndFitOnAxes(tour5, venue5, player5, axisIdxs) {
  if (!player5?.length || !venue5?.length || !tour5?.length || !axisIdxs?.length) {
    return { cat: "—", fit: 0 };
  }
  const contribs = [];
  let fit = 0;
  for (const i of axisIdxs) {
    if (i < 0 || i >= player5.length) continue;
    const stress = venue5[i] - tour5[i];
    const skill = player5[i] - 0.5;
    const c = stress * skill;
    fit += c;
    contribs.push({ i, c });
  }
  if (!contribs.length) return { cat: "—", fit: 0 };
  contribs.sort((a, b) => (b.c !== a.c ? b.c - a.c : a.i - b.i));
  const top = contribs[0];
  if (!top || top.c <= 0) return { cat: "—", fit };
  return { cat: COURSE_FIT_RADAR_SPOKE_LABELS[top.i] || "—", fit };
}

function courseTableStaticDifficultyD() {
  const ev = String(DATA?.meta?.course_used || DATA?.course_used || "").trim();
  if (!ev) return 0;
  const row = resolveCourseTableRowForNormKey(normCourseNameKey(ev));
  const v = num(row?.adj_score_to_par, NaN);
  const m = num(COURSE_TABLE_PAYLOAD?.means?.adj_score_to_par, NaN);
  if (!Number.isFinite(v) || !Number.isFinite(m)) return 0;
  return clamp(v - m, -3, 5);
}

function buildCourseFitTab() {
  const capEl = document.getElementById("course-fit-radar-caption");
  const legEl = document.getElementById("course-fit-radar-legend");
  const simList = document.getElementById("course-fit-similar-list");
  const simEmpty = document.getElementById("course-fit-similar-empty");
  const sel = document.getElementById("course-fit-player");
  const tbody = document.querySelector("#table-course-fit tbody");
  const theadHeading = document.getElementById("course-fit-table-heading");
  const canvas = document.getElementById("course-fit-radar-canvas");
  if (!tbody || !canvas) return;

  if (!COURSE_TABLE_PAYLOAD?.rows?.length) {
    void loadCourseTableJson().then(() => {
      if (activeAppTabId() === "course-fit") buildCourseFitTab();
    });
    if (capEl) capEl.textContent = "Loading course_table map…";
    if (legEl) legEl.innerHTML = "";
    return;
  }

  const eventVenueName = String(DATA?.meta?.course_used || DATA?.course_used || "this venue").trim() || "this venue";
  const eventVk = normCourseNameKey(eventVenueName);
  if (courseFitVenueEventKeyTracked !== eventVk) {
    courseFitVenueEventKeyTracked = eventVk;
    courseFitVenueFilterKey = null;
    courseFitSimilarSelectedKey = null;
    courseFitGolferDefaultApplied = false;
  }

  let courseKeys = Object.keys(COURSE_TABLE_PAYLOAD.byNormKey || {}).sort((a, b) => a.localeCompare(b));
  if (eventVk && !courseKeys.includes(eventVk)) {
    courseKeys.push(eventVk);
    courseKeys.sort((a, b) => a.localeCompare(b));
  }

  const venueSel = document.getElementById("course-fit-venue");
  if (venueSel) {
    venueSel.innerHTML = "";
    for (const ck of courseKeys) {
      const o = document.createElement("option");
      o.value = ck;
      o.textContent = courseFitPrettyCourseKey(ck);
      venueSel.appendChild(o);
    }
  }

  let activeVk = eventVk;
  if (courseFitVenueFilterKey && courseKeys.includes(courseFitVenueFilterKey)) {
    activeVk = courseFitVenueFilterKey;
  }
  if (venueSel && courseKeys.length) {
    if (courseKeys.includes(activeVk)) venueSel.value = activeVk;
    else {
      activeVk = courseKeys[0];
      venueSel.value = activeVk;
    }
  }

  const venueName = courseFitPrettyCourseKey(activeVk);
  const vk = activeVk;

  const rows = courseFitPlayerPool();
  const ctRow = resolveCourseTableRowForNormKey(vk);
  const tour5 = courseFitTour5FromCourseTable();
  const venue5 = courseFitVenue5FromCourseTableRow(ctRow);
  const similarRanked = courseFitSimilarCoursesFromCourseTable(vk, ctRow);
  const similarKeys = new Set(similarRanked.map((x) => x.ck));
  if (courseFitSimilarSelectedKey && !similarKeys.has(courseFitSimilarSelectedKey)) {
    courseFitSimilarSelectedKey = null;
  }
  const similarRow =
    courseFitSimilarSelectedKey && COURSE_TABLE_PAYLOAD?.byNormKey?.[courseFitSimilarSelectedKey]
      ? COURSE_TABLE_PAYLOAD.byNormKey[courseFitSimilarSelectedKey]
      : null;
  const similar5 = similarRow ? courseFitVenue5FromCourseTableRow(similarRow) : null;

  const similarDisplayName = courseFitSimilarSelectedKey
    ? courseFitSimilarSelectedKey.replace(/\b\w/g, (c) => c.toUpperCase())
    : "";

  if (capEl) capEl.textContent = venueName;

  if (sel) {
    const prev = sel.value;
    sel.innerHTML = "";
    const opt0 = document.createElement("option");
    opt0.value = "";
    opt0.textContent = "— None —";
    sel.appendChild(opt0);
    for (const r of rows) {
      const id = Math.round(num(r.dg_id, NaN));
      if (!Number.isFinite(id)) continue;
      const o = document.createElement("option");
      o.value = String(id);
      o.textContent = displayGolferName(String(r.player_name || ""));
      sel.appendChild(o);
    }
    if (prev && [...sel.options].some((x) => x.value === prev)) {
      sel.value = prev;
    } else if (!courseFitGolferDefaultApplied) {
      courseFitGolferDefaultApplied = true;
      const wantTf = (o) =>
        String(o.textContent || "")
          .trim()
          .toLowerCase()
          .includes("tommy fleetwood");
      const tfOpt = [...sel.options].find(wantTf);
      if (tfOpt) sel.value = tfOpt.value;
    }
    refreshGolferComboboxFromSelect("course-fit-player");
  }

  const dgSel = Math.round(num(sel?.value, NaN));
  const prow = rows.find((r) => Math.round(num(r.dg_id, NaN)) === dgSel);
  const player5 = courseFitPlayerRadarVectorMerged(rows, prow);

  if (legEl) {
    const selectedGolferName =
      (prow && displayGolferName(String(prow.player_name || ""))) || "Selected golfer";
    let html =
      '<span class="course-fit-leg-item"><span class="course-fit-leg-dash"></span> Field Avg</span>' +
      `<span class="course-fit-leg-item"><span class="course-fit-leg-green"></span> ${escapeHtml(venueName)}</span>`;
    if (similar5 && similarDisplayName) {
      html +=
        `<span class="course-fit-leg-item"><span class="course-fit-leg-blue" aria-hidden="true"></span> ${escapeHtml(similarDisplayName)}</span>`;
    }
    html += `<span class="course-fit-leg-item"><span class="course-fit-leg-gold"></span> ${escapeHtml(selectedGolferName)}</span>`;
    legEl.innerHTML = html;
  }

  drawCourseFitRadar(canvas, tour5, venue5, player5, similar5);

  if (!courseFitRadarResizeBound && typeof window !== "undefined") {
    courseFitRadarResizeBound = true;
    window.addEventListener("resize", () => {
      if (activeAppTabId() === "course-fit") buildCourseFitTab();
    });
  }

  if (simList && simEmpty) {
    simList.innerHTML = "";
    if (!similarRanked.length) {
      simEmpty.hidden = false;
      courseFitSimilarSelectedKey = null;
    } else {
      simEmpty.hidden = true;
      let rank = 1;
      for (const s of similarRanked) {
        const li = document.createElement("li");
        li.className = "course-fit-similar-li";
        li.setAttribute("data-course-fit-ck", s.ck);
        li.setAttribute("role", "button");
        li.setAttribute("tabindex", "0");
        li.setAttribute(
          "aria-pressed",
          courseFitSimilarSelectedKey === s.ck ? "true" : "false",
        );
        if (courseFitSimilarSelectedKey === s.ck) li.classList.add("course-fit-similar-li-selected");
        li.innerHTML = `<span class="course-fit-sim-rank">${rank++}.</span><span class="course-fit-sim-name">${escapeHtml(
          s.ck.replace(/\b\w/g, (c) => c.toUpperCase()),
        )}</span><span class="course-fit-sim-score">${(s.sim * 100).toFixed(0)}</span>`;
        simList.appendChild(li);
      }
    }
  }

  const venueEmphasisAxes = courseFitVenueEmphasisAxisIndices(tour5, venue5);

  const search = String(document.getElementById("course-fit-search")?.value || "")
    .trim()
    .toLowerCase();
  const ranked = [];
  for (const r of rows) {
    const playerN = courseFitPlayerRadarVectorMerged(rows, r);
    const { cat, fit } = courseFitPlayerCatAndFitOnAxes(tour5, venue5, playerN, venueEmphasisAxes);
    ranked.push({ r, cat, fit });
  }
  ranked.sort((a, b) => b.fit - a.fit);

  tbody.innerHTML = "";
  if (theadHeading) {
    theadHeading.textContent = `Who fits ${venueName}?`;
  }

  const marketKeys = ["win", "top_5", "top_10", "top_20"];
  const displayRows = [];
  for (const row of ranked) {
    const nm = displayGolferName(String(row.r.player_name || ""));
    if (search && !nm.toLowerCase().includes(search)) continue;
    const dgId = Math.round(num(row.r.dg_id, NaN));
    const odds = {};
    for (const mk of marketKeys) odds[mk] = courseFitOutrightBestPriceOdds(mk, dgId);
    displayRows.push({ ...row, nm, dgId, odds });
  }

  const sortKey = String(courseFitTableSort.key || "fit");
  const sortDir = courseFitTableSort.dir > 0 ? 1 : -1;
  displayRows.sort((a, b) => {
    if (sortKey === "golfer") {
      const c = a.nm.localeCompare(b.nm);
      return c * sortDir || b.fit - a.fit;
    }
    if (sortKey === "category") {
      const c = String(a.cat || "").localeCompare(String(b.cat || ""));
      return c * sortDir || b.fit - a.fit;
    }
    if (marketKeys.includes(sortKey)) {
      const av = a.odds?.[sortKey]?.am;
      const bv = b.odds?.[sortKey]?.am;
      const af = Number.isFinite(av);
      const bf = Number.isFinite(bv);
      if (af !== bf) return af ? -1 : 1;
      if (af && av !== bv) return (av - bv) * sortDir;
      return b.fit - a.fit || a.nm.localeCompare(b.nm);
    }
    const fitCmp = (a.fit - b.fit) * sortDir;
    return fitCmp || a.nm.localeCompare(b.nm);
  });

  if (!displayRows.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 7;
    td.className = "text-muted";
    td.textContent = "No players match this search.";
    tr.appendChild(td);
    tbody.appendChild(tr);
  } else {
    for (const row of displayRows.slice(0, 20)) {
      const tr = document.createElement("tr");
      const tdN = document.createElement("td");
      tdN.textContent = row.nm;
      const tdC = document.createElement("td");
      tdC.className = row.cat && row.cat !== "—" ? "num ev-pos" : "num";
      tdC.textContent = row.cat;
      const tdF = document.createElement("td");
      tdF.className = `num ${row.fit >= 0 ? "ev-pos" : "ev-neg"}`;
      tdF.textContent = `${row.fit >= 0 ? "+" : ""}${row.fit.toFixed(2)}`;
      tr.appendChild(tdN);
      tr.appendChild(tdC);
      tr.appendChild(tdF);
      for (const mk of marketKeys) {
        const tdO = document.createElement("td");
        tdO.className = "num course-fit-out-td";
        const ob = row.odds?.[mk] || { html: "—" };
        tdO.innerHTML = ob.html;
        tr.appendChild(tdO);
      }
      tbody.appendChild(tr);
    }
  }
  updateCourseFitTableSortIndicators();

  const shotPanel = document.getElementById("course-fit-subpanel-shots");
  if (shotPanel && !shotPanel.hidden) {
    const shotSearch = String(document.getElementById("course-fit-shots-search")?.value || "")
      .trim()
      .toLowerCase();
    void loadApproachSkillYtdJson().then((ap) => {
      const shotVenueTitle = eventVk ? courseFitPrettyCourseKey(eventVk) : eventVenueName;
      buildCourseFitShotBinsTable(rows, ap, shotVenueTitle, shotSearch);
    });
  }
}

function escapeHtml(s) {
  return String(s)
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

function buildMatchupsTable() {
  const tbody = document.querySelector("#table-matchups tbody");
  const note = document.getElementById("matchups-source-note");
  if (!tbody) return;
  tbody.innerHTML = "";
  const msel = document.getElementById("matchups-market");
  const key = msel ? msel.value : "round_matchups";
  const pack = DATA.matchups && DATA.matchups[key];
  const list = pack && pack.match_list;
  if (typeof list === "string") {
    if (note) {
      note.hidden = true;
      note.textContent = "";
    }
    return;
  }
  if (note) note.hidden = true;
  if (!Array.isArray(list) || !list.length) {
    return;
  }
  const r = getModelRoundForEv();
  const elim = dgIdsEliminatedFromEventPostCut();
  for (const m of list) {
    const id1 = Math.round(num(m.p1_dg_id, NaN));
    const id2 = Math.round(num(m.p2_dg_id, NaN));
    const id3 = Math.round(num(m.p3_dg_id, NaN));
    const row1 = projectionPlayerRowForModelByIdOrName(id1, m.p1_player_name, r);
    const row2 = projectionPlayerRowForModelByIdOrName(id2, m.p2_player_name, r);
    const row3 = projectionPlayerRowForModelByIdOrName(id3, m.p3_player_name, r);
    const mu1 = effectiveMuSg(row1, id1, key);
    const mu2 = effectiveMuSg(row2, id2, key);
    const mu3 = effectiveMuSg(row3, id3, key);
    const odds = m.odds || {};
    const b1 = bestBookDecimalForSide(odds, "p1", { allowDatagolf: true });
    const b2 = bestBookDecimalForSide(odds, "p2", { allowDatagolf: true });
    const b3 = bestBookDecimalForSide(odds, "p3", { allowDatagolf: true });
    const isThree = key === "3_balls" && Number.isFinite(id3) && id3 > 0;
    if (elim.size && (elim.has(id1) || elim.has(id2) || (isThree && elim.has(id3)))) continue;
    const label = isThree
      ? `${m.p1_player_name || ""} / ${m.p2_player_name || ""} / ${m.p3_player_name || ""}`
      : `${m.p1_player_name || ""} vs ${m.p2_player_name || ""}`;
    const span = isThree ? 3 : 2;
    function row(side, name, modelPct, ev, bb) {
      const tr = document.createElement("tr");
      const td0 = document.createElement("td");
      if (side === 1) {
        td0.rowSpan = span;
        td0.textContent = label;
      }
      const td1 = document.createElement("td");
      td1.textContent = String(name || "");
      const td2 = document.createElement("td");
      td2.className = "num";
      td2.textContent = Number.isFinite(modelPct) ? `${(modelPct * 100).toFixed(1)}%` : "—";
      const td3 = document.createElement("td");
      td3.className = "num";
      if (Number.isFinite(ev)) {
        td3.textContent = `${(ev * 100).toFixed(1)}%`;
        td3.classList.add(ev >= 0 ? "ev-pos" : "ev-neg");
      } else td3.textContent = "—";
      const td4 = document.createElement("td");
      td4.className = "num best-book-td";
      if (bb.book && Number.isFinite(bb.dec)) {
        td4.innerHTML = `${bookBadgeHtml(bb.book)} <span class="best-book-odds">${bb.dec.toFixed(2)}</span>`;
      } else td4.textContent = "—";
      if (side === 1) tr.appendChild(td0);
      tr.appendChild(td1);
      tr.appendChild(td2);
      tr.appendChild(td3);
      tr.appendChild(td4);
      tbody.appendChild(tr);
    }
    if (isThree) {
      const [tp1, tp2, tp3] = threeBallModelProbsLiveBlended(mu1, mu2, mu3, row1, row2, row3);
      const ev1 = Number.isFinite(b1.dec) ? tp1 * b1.dec - 1 : NaN;
      const ev2 = Number.isFinite(b2.dec) ? tp2 * b2.dec - 1 : NaN;
      const ev3 = Number.isFinite(b3.dec) ? tp3 * b3.dec - 1 : NaN;
      row(1, m.p1_player_name, tp1, ev1, b1);
      row(2, m.p2_player_name, tp2, ev2, b2);
      row(3, m.p3_player_name, tp3, ev3, b3);
    } else {
      const p1m = matchupWinProbLiveBlended(mu1, mu2, key, row1, row2);
      const ev1 = Number.isFinite(b1.dec) ? p1m * b1.dec - 1 : NaN;
      const ev2 = Number.isFinite(b2.dec) ? (1 - p1m) * b2.dec - 1 : NaN;
      row(1, m.p1_player_name, p1m, ev1, b1);
      row(2, m.p2_player_name, 1 - p1m, ev2, b2);
    }
  }
}

function impliedPctFromBookField(v) {
  const p = bookImpliedProb01(v);
  if (!Number.isFinite(p) || p <= 0 || p >= 1) return NaN;
  return p * 100;
}

function impliedPctFromOutrightBookField(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x) || x <= 0) return NaN;
  if (x > 0 && x < 100) return x;
  if (x === 100) return NaN;
  return impliedPctFromBookField(x);
}

/**
 * DataGolf feeds sometimes substitute ~10% implied (~+900) on FanDuel/Betway/SkyBet win markets when a book has no real price (same float across much of the field). Skip those for ladder / best-book picks so Caesars-only junk does not dominate.
 */
function outrightFeedPlaceholderProbNaN(p01, marketKey, bookRaw) {
  if (!Number.isFinite(p01) || p01 <= 0 || p01 >= 1) return NaN;
  const mk = String(marketKey || "");
  const bk = normalizeEvSportsbookKey(bookRaw);
  if (mk === "win" && (bk === "fanduel" || bk === "betway" || bk === "skybet")) {
    if (p01 >= 0.098 && p01 <= 0.102) return NaN;
  }
  return p01;
}

function impliedProbFromOutrightRowBook(row, bk, marketKey) {
  if (!row || !bk) return NaN;
  const bkNorm = normalizeEvSportsbookKey(bk);
  const pct = impliedPctFromOutrightBookField(row[bk] ?? row[bkNorm]);
  if (!Number.isFinite(pct)) return NaN;
  let p01 = pct / 100;
  p01 = outrightFeedPlaceholderProbNaN(p01, marketKey, bk);
  return Number.isFinite(p01) ? p01 : NaN;
}

function outrightFinishRowsByMarketDg(dgId) {
  const markets = ["win", "top_5", "top_10", "top_20"];
  const id = Math.round(num(dgId, NaN));
  /** @type {Record<string, object | null>} */
  const rowsByM = Object.create(null);
  if (!Number.isFinite(id)) return rowsByM;
  for (const mk of markets) {
    const pack = DATA.outrights?.[mk];
    const row = Array.isArray(pack?.rows) ? pack.rows.find((r) => Math.round(num(r.dg_id, NaN)) === id) : null;
    rowsByM[mk] = row;
  }
  return rowsByM;
}

/**
 * Pick one book per player so finish-market cells share the same sportsbook.
 * Maximizes sum of per-market EV across the four ladders under the existing EV ratio caps.
 */
function outrightFinishLadderBestBookBundle(dgId, opts) {
  const markets = ["win", "top_5", "top_10", "top_20"];
  const rowsByM = outrightFinishRowsByMarketDg(dgId);
  /** @type {Record<string, number>} */
  const modelPs = Object.create(null);
  for (const mk of markets) {
    modelPs[mk] = modelProbOutrightFromRowOrProjections(rowsByM[mk] || {}, mk, opts);
  }
  const bookSet = new Set();
  for (const mk of markets) {
    const pack = DATA.outrights?.[mk];
    const bks = Array.isArray(pack?.bookKeys) ? pack.bookKeys : [];
    for (const k of bks) {
      if (k && k !== "datagolf" && outrightLadderSportsbookAllowed(k)) bookSet.add(normalizeEvSportsbookKey(k));
    }
  }
  const books = [...bookSet].sort((a, b) => a.localeCompare(b));
  let best = null;
  let bestScore = -Infinity;
  for (const bk of books) {
    const raw = markets.map((mk) => impliedProbFromOutrightRowBook(rowsByM[mk], bk, mk));
    let sumEv = 0;
    let nOk = 0;
    for (let i = 0; i < 4; i++) {
      const mk = markets[i];
      const pBook = raw[i];
      const modelP = modelPs[mk];
      if (!Number.isFinite(pBook) || !Number.isFinite(modelP) || modelP <= 0) continue;
      const ev = outrightEvFromModelAndBook(modelP, pBook, mk);
      if (!Number.isFinite(ev)) continue;
      sumEv += ev;
      nOk++;
    }
    if (nOk === 0) continue;
    const better =
      !best ||
      sumEv > bestScore + 1e-9 ||
      (Math.abs(sumEv - bestScore) <= 1e-9 && best && bk.localeCompare(best.book) < 0);
    if (better) {
      bestScore = sumEv;
      best = { book: bk, raw, modelPs, rowsByM };
    }
  }
  return best;
}

function outrightLogit(p) {
  const x = clamp(p, 1e-9, 1 - 1e-9);
  return Math.log(x / (1 - x));
}

function outrightInvlogit(t) {
  return 1 / (1 + Math.exp(-t));
}

/** Mean/std of tournament `current_score` (lower = better vs par) for outright nudging. */
function outrightFieldCurrentScoreStats() {
  const seen = new Set();
  const vals = [];
  for (const p of DATA.players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || seen.has(id)) continue;
    const s = num(p.current_score, NaN);
    if (!Number.isFinite(s)) continue;
    seen.add(id);
    vals.push(s);
  }
  if (vals.length < 5) return null;
  const mean = vals.reduce((a, b) => a + b, 0) / vals.length;
  let v = 0;
  for (const s of vals) v += (s - mean) ** 2;
  const std = Math.sqrt(v / vals.length);
  if (!Number.isFinite(std) || std < 0.25) return null;
  return { mean, std, n: vals.length };
}

function outrightLiveTournamentContext() {
  const m = DATA?.meta || {};
  if (Number.isFinite(num(m.datagolf_live_current_round, NaN))) return true;
  return Boolean(String(m.datagolf_live_last_update || "").trim());
}

/**
 * Softmax win shares from field `current_score` (lower = better vs par). Used to blend with DG win
 * so model prices move with the live board even when placement API is slow.
 */
function outrightFieldScoreSoftmaxWinMap() {
  const byId = new Map();
  for (const p of DATA.players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || byId.has(id)) continue;
    const s = num(p.current_score, NaN);
    if (!Number.isFinite(s)) continue;
    byId.set(id, s);
  }
  if (byId.size < 5) return new Map();
  const arr = [...byId.entries()];
  const scores = arr.map(([, s]) => s);
  const minS = Math.min(...scores);
  const maxS = Math.max(...scores);
  const spread = Math.max(0.5, maxS - minS);
  const T = spread * 0.42 + 0.85;
  let sum = 0;
  const wts = [];
  for (const [id, s] of arr) {
    const w = Math.exp(-(s - minS) / T);
    wts.push([id, w]);
    sum += w;
  }
  const out = new Map();
  for (const [id, w] of wts) out.set(id, w / sum);
  return out;
}

/** +EV only: use score-driven probs when live bundle present; set `meta.outright_ev_live_leaderboard_model` false to use DG placement for +EV. */
function outrightEvLiveLeaderboardModelEnabled() {
  if (!outrightLiveTournamentContext()) return false;
  return DATA?.meta?.outright_ev_live_leaderboard_model !== false;
}

let outrightEvLbProbCache = {
  sig: "",
  /** @type {Map<number, number>} */
  win: new Map(),
  /** @type {Map<number, number>} */
  top5: new Map(),
  /** @type {Map<number, number>} */
  top10: new Map(),
  /** @type {Map<number, number>} */
  top20: new Map(),
};

function randStdNormal() {
  let u = 0;
  let v = 0;
  while (u === 0) u = Math.random();
  while (v === 0) v = Math.random();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}

function outrightEvLiveLeaderboardCacheSig() {
  const sigma = num(DATA?.meta?.outright_ev_live_leaderboard_sigma, NaN);
  const strokeNoise = Number.isFinite(sigma) && sigma > 0 ? sigma : 2.25;
  const nSimsRaw = Math.round(num(DATA?.meta?.outright_ev_live_leaderboard_mc_sims, NaN));
  const nSims = Number.isFinite(nSimsRaw) && nSimsRaw >= 100 ? Math.min(2500, nSimsRaw) : 420;
  const parts = [];
  for (const p of DATA.players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const s = num(p.current_score, NaN);
    if (!Number.isFinite(s)) continue;
    parts.push(`${id}:${s}`);
  }
  parts.sort();
  return `${DATA.meta?.datagolf_live_last_update}|${strokeNoise}|${nSims}|${parts.join(";")}`;
}

/** Builds Maps dg_id -> P(market) for win (softmax on scores) and top_K (noisy rank MC). Safe to call repeatedly; keyed by score fingerprint. */
function ensureOutrightEvLiveLeaderboardProbCache() {
  const sig = outrightEvLiveLeaderboardCacheSig();
  if (outrightEvLbProbCache.sig === sig && outrightEvLbProbCache.win.size >= 5) return;

  const scoresById = new Map();
  for (const p of DATA.players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || scoresById.has(id)) continue;
    const s = num(p.current_score, NaN);
    if (!Number.isFinite(s)) continue;
    scoresById.set(id, s);
  }
  if (scoresById.size < 5) {
    outrightEvLbProbCache = {
      sig: "",
      win: new Map(),
      top5: new Map(),
      top10: new Map(),
      top20: new Map(),
    };
    return;
  }

  outrightEvLbProbCache = {
    sig,
    win: new Map(),
    top5: new Map(),
    top10: new Map(),
    top20: new Map(),
  };

  const winSm = outrightFieldScoreSoftmaxWinMap();
  for (const [id, pw] of winSm) outrightEvLbProbCache.win.set(id, pw);

  const sigma = num(DATA?.meta?.outright_ev_live_leaderboard_sigma, NaN);
  const strokeNoise = Number.isFinite(sigma) && sigma > 0 ? sigma : 2.25;
  const nSimsRaw = Math.round(num(DATA?.meta?.outright_ev_live_leaderboard_mc_sims, NaN));
  const nSims = Number.isFinite(nSimsRaw) && nSimsRaw >= 100 ? Math.min(2500, nSimsRaw) : 420;

  const ids = [...scoresById.keys()];
  /** @type {Map<number, number>} */
  const mkCounts = () => {
    const m = new Map();
    for (const id of ids) m.set(id, 0);
    return m;
  };
  const c5 = mkCounts();
  const c10 = mkCounts();
  const c20 = mkCounts();

  for (let rep = 0; rep < nSims; rep++) {
    const perf = ids.map((id) => ({
      id,
      key: scoresById.get(id) + randStdNormal() * strokeNoise,
    }));
    perf.sort((a, b) => a.key - b.key);
    const n = perf.length;
    for (let i = 0; i < Math.min(5, n); i++) {
      const id = perf[i].id;
      c5.set(id, c5.get(id) + 1);
    }
    for (let i = 0; i < Math.min(10, n); i++) {
      const id = perf[i].id;
      c10.set(id, c10.get(id) + 1);
    }
    for (let i = 0; i < Math.min(20, n); i++) {
      const id = perf[i].id;
      c20.set(id, c20.get(id) + 1);
    }
  }

  for (const id of ids) {
    outrightEvLbProbCache.top5.set(id, c5.get(id) / nSims);
    outrightEvLbProbCache.top10.set(id, c10.get(id) / nSims);
    outrightEvLbProbCache.top20.set(id, c20.get(id) / nSims);
  }
}

/** Lookup +EV live leaderboard model prob for one outright row + market (win | top_5 | top_10 | top_20). */
function modelProbOutrightLiveLeaderboardEvLookup(outrightRow, marketKey) {
  const id = Math.round(num(outrightRow?.dg_id, NaN));
  if (!Number.isFinite(id)) return NaN;
  ensureOutrightEvLiveLeaderboardProbCache();
  if (!outrightEvLbProbCache.sig || outrightEvLbProbCache.win.size < 5) return NaN;
  const mk = String(marketKey || "");
  let m = null;
  if (mk === "win") m = outrightEvLbProbCache.win;
  else if (mk === "top_5") m = outrightEvLbProbCache.top5;
  else if (mk === "top_10") m = outrightEvLbProbCache.top10;
  else if (mk === "top_20") m = outrightEvLbProbCache.top20;
  else return NaN;
  const p = m.get(id);
  if (!Number.isFinite(p) || p <= 0 || p >= 1) return NaN;
  return clamp(p, 1e-6, 1 - 1e-6);
}

/**
 * Nudge placement probs using leaderboard `current_score` vs the field (DataGolf: lower = better).
 * Scaled down when preds/in-play already supplied placement so we do not double-count DG's model.
 * Off by default — +EV uses raw export placement unless meta.outright_live_score_placement_nudge is true.
 */
function outrightProbWithLiveScoreNudge(rowPlayer, marketKey, baseP) {
  if (!Number.isFinite(baseP) || baseP <= 0 || baseP >= 1) return baseP;
  if (DATA?.meta?.outright_live_score_placement_nudge !== true) return baseP;
  if (!outrightLiveTournamentContext()) return baseP;
  const fs = outrightFieldCurrentScoreStats();
  if (!fs) return baseP;
  const s = num(rowPlayer?.current_score, NaN);
  if (!Number.isFinite(s)) return baseP;
  const zAdv = (fs.mean - s) / fs.std;
  if (!Number.isFinite(zAdv)) return baseP;
  const fromApi = Boolean(rowPlayer?.dg_live_placement_from_api);
  const apiScale = fromApi ? 0.82 : 1;
  const k =
    marketKey === "win"
      ? 0.42
      : marketKey === "top_5"
        ? 0.3
        : marketKey === "top_10"
          ? 0.22
            : marketKey === "top_20"
            ? 0.15
            : marketKey === "make_cut"
              ? 0.24
              : 0;
  const t = outrightLogit(baseP) + apiScale * k * clamp(zAdv, -2.8, 2.8);
  return clamp(outrightInvlogit(t), 1e-6, 1 - 1e-6);
}

/** +EV / model American: tilt placement probs using the same μ_SG bonus as O/U (pricing mode + skill). */
function outrightProbWithPricingModeNudge(rowPlayer, marketKey, baseP) {
  if (!Number.isFinite(baseP) || baseP <= 0 || baseP >= 1) return baseP;
  const id = Math.round(num(rowPlayer?.dg_id, NaN));
  if (!Number.isFinite(id)) return baseP;
  const b = pricingModeMuSgBonus(id);
  if (!Number.isFinite(b) || Math.abs(b) < 1e-10) return baseP;
  const mk = String(marketKey || "");
  const k =
    mk === "win"
      ? 0.52
      : mk === "top_5"
        ? 0.36
        : mk === "top_10"
          ? 0.26
          : mk === "top_20"
            ? 0.18
            : mk === "make_cut" || mk === "mc"
              ? 0.22
              : mk === "frl"
                ? 0.42
                : 0.3;
  const t = outrightLogit(baseP) + k * b;
  return clamp(outrightInvlogit(t), 1e-6, 1 - 1e-6);
}

function modelProbOutrightMarket(rowPlayer, marketKey) {
  const col =
    marketKey === "win"
      ? "win"
      : marketKey === "top_5"
        ? "top_5"
        : marketKey === "top_10"
          ? "top_10"
          : marketKey === "top_20"
            ? "top_20"
            : marketKey === "make_cut" || marketKey === "mc"
              ? "make_cut"
              : "win";
  const rawVal = rowPlayer ? rowPlayer[col] : undefined;
  /* Number(null)===0: null/blank placement fields must be treated as missing, not near-zero event odds. */
  if (rawVal == null || rawVal === "") return NaN;
  let baseP = datagolfModelProb01(rawVal);
  if (!Number.isFinite(baseP) || baseP <= 0) return NaN;
  if (marketKey === "win" && outrightLiveTournamentContext()) {
    const id = Math.round(num(rowPlayer?.dg_id, NaN));
    const sm = outrightFieldScoreSoftmaxWinMap();
    const pScore = sm.get(id);
    if (Number.isFinite(pScore) && sm.size >= 5) {
      const metaBlend = num(DATA?.meta?.outright_win_score_blend, NaN);
      const blend = Number.isFinite(metaBlend) ? clamp(metaBlend, 0, 1) : 0;
      if (blend > 0) baseP = clamp(blend * pScore + (1 - blend) * baseP, 1e-6, 1 - 1e-6);
    }
  }
  if (marketKey === "mc") {
    let pCut = outrightProbWithLiveScoreNudge(rowPlayer, "make_cut", baseP);
    pCut = outrightProbWithPricingModeNudge(rowPlayer, "make_cut", pCut);
    return clamp(1 - pCut, 1e-6, 1 - 1e-6);
  }
  baseP = outrightProbWithLiveScoreNudge(rowPlayer, marketKey, baseP);
  baseP = outrightProbWithPricingModeNudge(rowPlayer, marketKey, baseP);
  return clamp(baseP, 1e-6, 1 - 1e-6);
}

/**
 * Outrights "Model" fair price: scraped DataGolf finish-tool model, then projection rows, then book mean.
 * When `opts.evLiveLeaderboard` (+EV during live events only), tries leaderboard `current_score` model first — see file header.
 */
function modelProbOutrightFromRowOrProjections(outrightRow, marketKey, opts) {
  const id = Math.round(num(outrightRow?.dg_id, NaN));
  const evLb = opts && opts.evLiveLeaderboard === true && outrightEvLiveLeaderboardModelEnabled();
  if (evLb) {
    const pLb = modelProbOutrightLiveLeaderboardEvLookup(outrightRow, marketKey);
    if (Number.isFinite(pLb) && pLb > 0) {
      let prowLb = Number.isFinite(id) ? projectionRowWithPlacementMerged(id) : null;
      if (!prowLb && outrightRow?.player_name) {
        prowLb = projectionPlayerRowForModelByIdOrName(NaN, outrightRow.player_name, getModelRoundForEv());
      }
      return outrightProbWithPricingModeNudge(prowLb || {}, marketKey, pLb);
    }
  }
  const scrapedModelPct = num(outrightRow?.dg_model, NaN);
  if (Number.isFinite(scrapedModelPct) && scrapedModelPct > 0 && scrapedModelPct < 100) {
    let prowSc = Number.isFinite(id) ? projectionRowWithPlacementMerged(id) : null;
    if (!prowSc && outrightRow?.player_name) {
      prowSc = projectionPlayerRowForModelByIdOrName(NaN, outrightRow.player_name, getModelRoundForEv());
    }
    const p0 = clamp(scrapedModelPct / 100, 1e-6, 1 - 1e-6);
    return outrightProbWithPricingModeNudge(prowSc || {}, marketKey, p0);
  }
  let prow = Number.isFinite(id) ? projectionRowWithPlacementMerged(id) : null;
  if (!prow && outrightRow?.player_name) {
    prow = projectionPlayerRowForModelByIdOrName(NaN, outrightRow.player_name, getModelRoundForEv());
  }
  const fromPret = modelProbOutrightMarket(prow || {}, marketKey);
  if (Number.isFinite(fromPret) && fromPret > 0) return fromPret;

  // Last-resort: when projections placement fields are null,
  // use market consensus from posted books so model price does not go blank.
  let s = 0;
  let nBooks = 0;
  for (const [k, v] of Object.entries(outrightRow || {})) {
    const kk = String(k || "").toLowerCase();
    if (!kk || kk === "datagolf" || kk === "dg_model" || kk === "dg_id" || kk === "id" || kk === "player_name" || kk === "name") continue;
    const pct = impliedPctFromBookField(v);
    if (!Number.isFinite(pct) || pct <= 0 || pct >= 100) continue;
    s += pct / 100;
    nBooks += 1;
  }
  if (nBooks > 0) {
    let p = s / nBooks;
    if (marketKey === "mc") p = 1 - p;
    if (Number.isFinite(p) && p > 0 && p < 1) return clamp(p, 1e-6, 1 - 1e-6);
  }
  return NaN;
}

let outrightSort = { key: "player", dir: 1 };

function buildOutrightsTableBodyOnly() {
  const table = document.getElementById("table-outrights");
  if (!table) return;
  const msel = document.getElementById("outright-market");
  const mk = msel ? msel.value : "win";
  const pack = DATA.outrights && DATA.outrights[mk];
  const tbody = table.querySelector("tbody");
  if (!tbody || !pack || !Array.isArray(pack.rows)) return;
  const bookKeys = Array.isArray(pack.bookKeys) ? pack.bookKeys.filter((k) => k && k !== "datagolf") : [];
  const elim = dgIdsEliminatedFromEventPostCut();
  const outrightRowOk =
    mk === "make_cut" || mk === "mc"
      ? () => true
      : (row) => !elim.has(Math.round(num(row.dg_id, NaN)));
  const finishMk = mk === "win" || mk === "top_5" || mk === "top_10" || mk === "top_20";
  const rows = pack.rows.filter(outrightRowOk).map((row) => {
    const id = Math.round(num(row.dg_id, NaN));
    if (finishMk) {
      const bundle = outrightFinishLadderBestBookBundle(id);
      if (bundle) {
        const idx = ["win", "top_5", "top_10", "top_20"].indexOf(mk);
        const pBook = bundle.raw[idx];
        const mp = bundle.modelPs[mk];
        if (Number.isFinite(pBook) && Number.isFinite(mp) && mp > 0) {
          const ev = outrightEvFromModelAndBook(mp, pBook, mk);
          if (Number.isFinite(ev)) {
            return {
              row,
              modelP: mp,
              bestBook: bundle.book,
              bestAm: americanFromImpliedProb(pBook),
              bestEv: ev,
            };
          }
        }
      }
    }
    let modelP = modelProbOutrightFromRowOrProjections(row, mk);
    let bestBook = "";
    let bestAm = NaN;
    let bestEv = NaN;
    for (const bk of bookKeys) {
      const pct = impliedPctFromOutrightBookField(row[bk]);
      if (!Number.isFinite(pct) || pct <= 0) continue;
      let pBook = pct / 100;
      pBook = outrightFeedPlaceholderProbNaN(pBook, mk, bk);
      if (!Number.isFinite(pBook) || pBook <= 0 || pBook >= 1) continue;
      if (!Number.isFinite(modelP) || modelP <= 0) continue;
      const ev = outrightEvFromModelAndBook(modelP, pBook, mk);
      if (!Number.isFinite(ev)) continue;
      const am = americanFromImpliedProb(pBook);
      if (!Number.isFinite(bestEv) || ev > bestEv) {
        bestEv = ev;
        bestBook = bk;
        bestAm = am;
      }
    }
    return {
      row,
      modelP,
      bestBook,
      bestAm: Number.isFinite(bestAm) ? Math.round(bestAm) : bestAm,
      bestEv,
    };
  });

  function sortVal(item, key) {
    if (key === "player") return String(item.row.player_name || "").toLowerCase();
    if (key === "model") return num(item.modelP, -1);
    if (key === "ev") return num(item.bestEv, -1e9);
    if (key === "best") return num(item.bestAm, 0);
    const pct = impliedPctFromBookField(item.row[key]);
    return num(pct, -1);
  }

  const sk = outrightSort.key;
  const sd = outrightSort.dir;
  rows.sort((a, b) => {
    const va = sortVal(a, sk);
    const vb = sortVal(b, sk);
    if (va < vb) return -sd;
    if (va > vb) return sd;
    return 0;
  });

  tbody.innerHTML = "";
  let i = 0;
  for (const it of rows) {
    const tr = document.createElement("tr");
    tr.className = "outrights-data-row" + (i % 2 ? " outrights-row-alt" : "");
    const nameTd = document.createElement("td");
    nameTd.textContent = String(it.row.player_name || "");
    tr.appendChild(nameTd);
    for (const bk of bookKeys) {
      const td = document.createElement("td");
      td.className = "num";
      const pct = impliedPctFromBookField(it.row[bk]);
      td.textContent = Number.isFinite(pct) ? `${pct.toFixed(1)}%` : "—";
      tr.appendChild(td);
    }
    const mTd = document.createElement("td");
    mTd.className = "num";
    mTd.textContent = Number.isFinite(it.modelP) ? `${(it.modelP * 100).toFixed(1)}%` : "—";
    tr.appendChild(mTd);
    const evTd = document.createElement("td");
    evTd.className = "num";
    if (Number.isFinite(it.bestEv)) {
      evTd.textContent = `${(it.bestEv * 100).toFixed(1)}%`;
      evTd.classList.add(it.bestEv >= 0 ? "ev-pos" : "ev-neg");
    } else evTd.textContent = "—";
    tr.appendChild(evTd);
    const bbTd = document.createElement("td");
    bbTd.className = "num best-book-td";
    if (it.bestBook && Number.isFinite(it.bestAm)) {
      bbTd.innerHTML = `${bookBadgeHtml(it.bestBook)} <span class="best-book-odds">${formatAmerican(it.bestAm)}</span>`;
    } else bbTd.textContent = "—";
    tr.appendChild(bbTd);
    tbody.appendChild(tr);
    i++;
  }
}

function buildOutrightsTable() {
  const table = document.getElementById("table-outrights");
  if (!table) return;
  const msel = document.getElementById("outright-market");
  const mk = msel ? msel.value : "win";
  const pack = DATA.outrights && DATA.outrights[mk];
  const thead = table.querySelector("thead");
  const tbody = table.querySelector("tbody");
  if (!thead || !tbody) return;
  const bookKeys = pack && Array.isArray(pack.bookKeys) ? pack.bookKeys.filter((k) => k && k !== "datagolf") : [];
  const trh = document.createElement("tr");
  const thPlayer = document.createElement("th");
  thPlayer.className = "sortable";
  thPlayer.dataset.sortKey = "player";
  thPlayer.innerHTML = `Player<span class="sort-ind"><span class="sort-up">▲</span><span class="sort-down">▼</span></span>`;
  trh.appendChild(thPlayer);
  for (const bk of bookKeys) {
    const th = document.createElement("th");
    th.className = "num book-col-th sortable";
    th.dataset.sortKey = bk;
    th.innerHTML = `<span class="book-th-inner"><span class="book-badge-inline" title="${bk}">${bk.slice(0, 3).toUpperCase()}</span></span><span class="sort-ind"><span class="sort-up">▲</span><span class="sort-down">▼</span></span>`;
    trh.appendChild(th);
  }
  const thM = document.createElement("th");
  thM.className = "num sortable";
  thM.dataset.sortKey = "model";
  thM.innerHTML = `Model<span class="sort-ind"><span class="sort-up">▲</span><span class="sort-down">▼</span></span>`;
  trh.appendChild(thM);
  const thE = document.createElement("th");
  thE.className = "num sortable";
  thE.dataset.sortKey = "ev";
  thE.innerHTML = `EV<span class="sort-ind"><span class="sort-up">▲</span><span class="sort-down">▼</span></span>`;
  trh.appendChild(thE);
  const thB = document.createElement("th");
  thB.className = "num sortable";
  thB.dataset.sortKey = "best";
  thB.innerHTML = `Best Book<span class="sort-ind"><span class="sort-up">▲</span><span class="sort-down">▼</span></span>`;
  trh.appendChild(thB);
  thead.innerHTML = "";
  thead.appendChild(trh);
  buildOutrightsTableBodyOnly();
  updateOutrightsSortIndicators();
}

function updateOutrightsSortIndicators() {
  const table = document.getElementById("table-outrights");
  if (!table) return;
  const ths = table.querySelectorAll("thead th.sortable");
  ths.forEach((th) => {
    const key = th.dataset.sortKey;
    const up = th.querySelector(".sort-up");
    const dn = th.querySelector(".sort-down");
    if (up) up.classList.toggle("active", key === outrightSort.key && outrightSort.dir > 0);
    if (dn) dn.classList.toggle("active", key === outrightSort.key && outrightSort.dir < 0);
  });
}

let outrightsSortInited = false;

function initOutrightsTableSortOnce() {
  if (outrightsSortInited) return;
  const table = document.getElementById("table-outrights");
  if (!table) return;
  outrightsSortInited = true;
  table.querySelector("thead")?.addEventListener("click", (ev) => {
    const th = ev.target.closest("th.sortable");
    if (!th || !table.contains(th)) return;
    const key = th.dataset.sortKey;
    if (!key) return;
    if (outrightSort.key === key) outrightSort.dir *= -1;
    else {
      outrightSort.key = key;
      outrightSort.dir = key === "player" ? 1 : -1;
    }
    buildOutrightsTableBodyOnly();
    updateOutrightsSortIndicators();
  });
}

function playerKeyFromName(full) {
  const s = String(full || "").trim();
  const i = s.indexOf(",");
  if (i > 0) {
    const last = s.slice(0, i).trim().toLowerCase();
    const first = (s.slice(i + 1).trim().split(/\s+/)[0] || "").toLowerCase();
    return `${last}|${first}`;
  }
  const tok = s.toLowerCase().split(/\s+/).filter(Boolean);
  if (tok.length >= 2) return `${tok[tok.length - 1]}|${tok[0]}`;
  return s.toLowerCase().replace(/\s+/g, "");
}

function normEvtNameKey(s) {
  return String(s || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function parseRoundNumFromRLabel(rlab) {
  const m = String(rlab || "").match(/(\d+)/);
  return m ? parseInt(m[1], 10) : NaN;
}

/** Match hole-card round key to player_shots_web.json round uid (tournament_name\tR{n}). */
function findShotsRoundUid(byRound, wantUid) {
  if (!byRound || !wantUid) return null;
  if (byRound[wantUid]) return wantUid;
  const parts = String(wantUid).split("\t");
  const evRaw = (parts[0] || "").trim();
  const rlab = (parts[1] || "").trim();
  const rn = parseRoundNumFromRLabel(rlab);
  const wantEv = normEvtNameKey(evRaw);
  for (const k of Object.keys(byRound)) {
    const kp = String(k).split("\t");
    const e2 = (kp[0] || "").trim();
    const r2 = (kp[1] || "").trim();
    const rn2 = parseRoundNumFromRLabel(r2);
    if (Number.isFinite(rn) && Number.isFinite(rn2) && rn !== rn2) continue;
    if (normEvtNameKey(e2) === wantEv) return k;
  }
  return null;
}

function renderPropsShotsForSelection(dg, fullKey) {
  const wrap = document.getElementById("props-shot-wrap");
  const metaEl = document.getElementById("props-shot-meta");
  const tbody = document.querySelector("#props-shots-table tbody");
  if (!wrap || !tbody) return;
  if (!SHOTS._ok || !fullKey) {
    wrap.hidden = true;
    tbody.innerHTML = "";
    if (metaEl) metaEl.textContent = "";
    return;
  }
  const byRound = SHOTS.byDgId && SHOTS.byDgId[String(dg)];
  const uid = findShotsRoundUid(byRound, fullKey);
  if (!uid || !byRound || !byRound[uid]) {
    wrap.hidden = true;
    tbody.innerHTML = "";
    if (metaEl) metaEl.textContent = "";
    return;
  }
  const holesObj = byRound[uid];
  const holeNums = Object.keys(holesObj)
    .map((h) => parseInt(h, 10))
    .filter((n) => Number.isFinite(n) && n >= 1 && n <= 18)
    .sort((a, b) => a - b);
  const rows = [];
  for (const hn of holeNums) {
    const arr = holesObj[String(hn)] || [];
    for (const s of arr) {
      rows.push({ hole: hn, ...s });
    }
  }
  if (!rows.length) {
    wrap.hidden = true;
    tbody.innerHTML = "";
    if (metaEl) metaEl.textContent = "";
    return;
  }
  wrap.hidden = false;
  if (metaEl) {
    const m = SHOTS.meta || {};
    metaEl.textContent = `Source: ${m.source_csv || "shots"} · ${m.min_shot_season_year || "2022"}+ · last ${m.shot_round_tail ?? "—"} rounds/player · ${(m.rows_used || 0).toLocaleString()} shot rows in bundle`;
  }
  tbody.innerHTML = "";
  for (const s of rows) {
    const tr = document.createElement("tr");
    const t0 = document.createElement("td");
    t0.textContent = String(s.hole);
    const t1 = document.createElement("td");
    t1.className = "num";
    t1.textContent = String(s.sn);
    const t2 = document.createElement("td");
    t2.textContent = s.f || "—";
    const t3 = document.createElement("td");
    t3.textContent = s.t || "—";
    const t4 = document.createElement("td");
    t4.className = "num";
    t4.textContent = Number.isFinite(s.d) ? String(s.d) : "—";
    const t5 = document.createElement("td");
    let pbp = String(s.p || "").trim();
    if (s.fin) pbp = pbp ? `${pbp} · final` : "final";
    t5.textContent = pbp;
    t5.className = "props-shot-pbp";
    tr.appendChild(t0);
    tr.appendChild(t1);
    tr.appendChild(t2);
    tr.appendChild(t3);
    tr.appendChild(t4);
    tr.appendChild(t5);
    tbody.appendChild(tr);
  }
}

function embeddedRoundHistoryPayload() {
  if (
    typeof window === "undefined" ||
    !window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__ ||
    typeof window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__ !== "object" ||
    !window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__.byDgId
  ) {
    return null;
  }
  return window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__;
}

function loadEmbeddedRoundHistoryScript() {
  if (embeddedRoundHistoryPayload()) return Promise.resolve();
  if (typeof document === "undefined") return Promise.resolve();
  if (embeddedRoundHistoryScriptPromise) return embeddedRoundHistoryScriptPromise;
  embeddedRoundHistoryScriptPromise = new Promise((resolve) => {
    const s = document.createElement("script");
    s.src = isFileProtocol() ? "embedded-player-round-history.js" : cacheBustFetchUrl("embedded-player-round-history.js");
    s.async = true;
    s.onload = () => resolve();
    s.onerror = () => resolve();
    document.body.appendChild(s);
  });
  return embeddedRoundHistoryScriptPromise;
}

function historyDateMdYIsFuture(s) {
  const raw = String(s || "").trim();
  let y = NaN;
  let mo = NaN;
  let d = NaN;
  const mdy = raw.match(/^(\d{1,2})\/(\d{1,2})\/(\d{4})$/);
  const iso = raw.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (mdy) {
    mo = Number(mdy[1]);
    d = Number(mdy[2]);
    y = Number(mdy[3]);
  } else if (iso) {
    y = Number(iso[1]);
    mo = Number(iso[2]);
    d = Number(iso[3]);
  } else {
    return false;
  }
  const t = Date.UTC(y, mo - 1, d);
  const now = new Date();
  const today = Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate());
  return Number.isFinite(t) && t > today;
}

/** PGA/LIV stroke events: DG CSV uses event_completed = last day; spread rounds backward (R4 on that day). */
function historyEventStrokeRoundCap(row) {
  const t = String(row?.tour || "").toLowerCase();
  return t === "liv" ? 3 : 4;
}

function historyMdyFromChronoBase(chronoBase, dayOffset) {
  if (!Number.isFinite(chronoBase) || chronoBase <= 0) return "";
  const y = Math.floor(chronoBase / 10000);
  const mo = Math.floor((chronoBase % 10000) / 100);
  const d = chronoBase % 100;
  const ms = Date.UTC(y, mo - 1, d) + Math.round(Number(dayOffset) || 0) * 86400000;
  const dt = new Date(ms);
  return `${dt.getUTCMonth() + 1}/${dt.getUTCDate()}/${dt.getUTCFullYear()}`;
}

function historyRowUsesEventEndAnchor(row) {
  if (!row || typeof row !== "object") return false;
  if (row._from_live_tournament_stats || row._from_pgatour || row._from_live_in_play) return false;
  if (row._from_dg_historical_rounds) return true;
  const ec = String(row.event_completed || "").trim();
  if (!ec) return false;
  const sk = Math.round(num(row.sortKey, NaN));
  if (Number.isFinite(sk) && sk > 9_999_999) {
    const skBase = Math.floor(sk / 10);
    const ecBase = parseEventCompletedChronoBase(ec);
    return ecBase > 0 && skBase === ecBase;
  }
  return true;
}

function historyRoundDayOffsetFromEventAnchor(row) {
  const rnd = Math.round(num(row?.round_num, NaN));
  if (!Number.isFinite(rnd) || rnd < 1) return 0;
  if (!historyRowUsesEventEndAnchor(row)) return 0;
  const cap = historyEventStrokeRoundCap(row);
  return -(Math.max(0, cap - rnd));
}

/** Play date M/D/YYYY for Historical Trends chart + date filters. */
function historyRoundPlayMdY(row) {
  if (!row || typeof row !== "object") return "";
  const ec = String(row.event_completed || "").trim();
  const offset = historyRoundDayOffsetFromEventAnchor(row);
  if (offset !== 0 && ec) {
    const base = parseEventCompletedChronoBase(ec);
    if (base > 0) return historyMdyFromChronoBase(base, offset);
  }
  return ec;
}

/** Calendar day shown on the trends chart, as UTC midnight ms. */
function historyRoundChartDateUtcMs(row) {
  const mdy = historyRoundPlayMdY(row);
  const base = parseEventCompletedChronoBase(mdy);
  if (!base) return NaN;
  const y = Math.floor(base / 10000);
  const mo = Math.floor((base % 10000) / 100);
  const d = base % 100;
  return Date.UTC(y, mo - 1, d);
}

function historyRoundChartDateIsFuture(row) {
  if (row && typeof row === "object") {
    const rnd = Math.round(num(row.round_num, NaN));
    const cap = currentTournamentProgressRoundCap();
    if (historyRoundMatchesCurrentEvent(row) && Number.isFinite(rnd) && Number.isFinite(cap) && rnd <= cap) {
      const ms = historyRoundChartDateUtcMs(row);
      if (Number.isFinite(ms)) {
        const now = new Date();
        const today = Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate());
        if (ms <= today) return false;
      }
    }
  }
  const ms = historyRoundChartDateUtcMs(row);
  if (!Number.isFinite(ms)) return historyDateMdYIsFuture(row?.event_completed);
  const now = new Date();
  const today = Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate());
  return ms > today;
}

function sanitizePlayerHistoryPayload(payload) {
  if (!payload || typeof payload !== "object" || !payload.byDgId || typeof payload.byDgId !== "object") return payload;
  let removed = 0;
  let liveRowsRemoved = 0;
  for (const bucket of Object.values(payload.byDgId)) {
    if (!bucket || !Array.isArray(bucket.rounds)) continue;
    const before = bucket.rounds.length;
    bucket.rounds = bucket.rounds.filter((r) => {
      if (r?._from_live_in_play && !r?._from_live_tournament_stats) {
        liveRowsRemoved += 1;
        return false;
      }
      return !historyDateMdYIsFuture(r?.event_completed) && !historyRoundChartDateIsFuture(r);
    });
    removed += before - bucket.rounds.length;
  }
  if (removed > 0 || liveRowsRemoved > 0) {
    payload.meta = {
      ...(payload.meta || {}),
      ...(removed > 0 ? { future_rounds_filtered: removed } : {}),
      ...(liveRowsRemoved > 0 ? { live_in_play_rows_stripped: liveRowsRemoved } : {}),
    };
  }
  return payload;
}

/**
 * Historical Trends actuals: historical-raw-data/rounds (CSV) or preds/live-tournament-stats during the live week.
 */
function historyRowFromDgHistoricalRoundsApi(row) {
  if (!row || typeof row !== "object") return false;
  if (row._from_live_tournament_stats || row._from_pgatour) return true;
  return !row._from_live_in_play;
}

function applyPlayerHistoryPayload(payload, opts = {}) {
  const clean = sanitizePlayerHistoryPayload(payload);
  if (clean?.byDgId && typeof clean.byDgId === "object") {
    for (const bucket of Object.values(clean.byDgId)) {
      if (!bucket?.rounds) continue;
      bucket.rounds = bucket.rounds.map((r) => scrubLivePlaceholderCountingOnRow(r));
    }
  }
  HISTORY = { ...clean, _ok: true, _loading: false, _partial: Boolean(opts.partial) };
  HISTORY_ROUNDS_CHRONO_CACHE.clear();
  PRICING_MU_BONUS_CACHE.clear();
  if (DATA?.meta?.event_name) scrubNonActualRoundsFromHistoryBuckets();
  bumpHistoryMutationEpoch();
  void ensurePropsDgIdNameManifestLoaded();
}

function normalizeHistoryShardPayload(payload) {
  if (!payload || typeof payload !== "object") return null;
  if (payload.byDgId && typeof payload.byDgId === "object") return payload;
  const dg = Math.round(num(payload.dg_id, NaN));
  if (!Number.isFinite(dg) || !Array.isArray(payload.rounds)) return null;
  return {
    meta: payload.meta && typeof payload.meta === "object" ? payload.meta : {},
    byDgId: {
      [String(dg)]: {
        dg_id: dg,
        player_name: String(payload.player_name || "").trim(),
        rounds: payload.rounds,
      },
    },
    holesByPlayerKey: {},
  };
}

function mergePlayerHistoryPartialPayload(payload) {
  const normalized = normalizeHistoryShardPayload(payload);
  const clean = sanitizePlayerHistoryPayload(normalized || payload);
  if (!clean || !clean.byDgId || typeof clean.byDgId !== "object") return false;
  const mergedByDgId = { ...(HISTORY.byDgId || {}), ...clean.byDgId };
  HISTORY = {
    meta: { ...(HISTORY.meta || {}), ...(clean.meta || {}) },
    byDgId: mergedByDgId,
    holesByPlayerKey: { ...(HISTORY.holesByPlayerKey || {}), ...(clean.holesByPlayerKey || {}) },
    _ok: true,
    _loading: false,
    _partial: Object.keys(mergedByDgId).length < 20,
  };
  for (const k of Object.keys(clean.byDgId)) {
    const id = Math.round(num(k, NaN));
    if (Number.isFinite(id)) HISTORY_ROUNDS_CHRONO_CACHE.delete(id);
  }
  PRICING_MU_BONUS_CACHE.clear();
  bumpHistoryMutationEpoch();
  return true;
}

function historyBucketLoaded(dgId) {
  const id = Math.round(num(dgId, NaN));
  const bucket = HISTORY.byDgId?.[String(id)];
  return (
    Number.isFinite(id) &&
    Boolean(bucket && Array.isArray(bucket.rounds) && bucket.rounds.length > 0)
  );
}

async function extractHistoryBucketFromEmbedded(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return false;
  try {
    await loadEmbeddedRoundHistoryScript();
    const emb = embeddedRoundHistoryPayload();
    const bucket = emb?.byDgId?.[String(id)];
    if (!bucket || !Array.isArray(bucket.rounds) || !bucket.rounds.length) return false;
    return mergePlayerHistoryPartialPayload({
      meta: emb.meta || {},
      byDgId: { [String(id)]: bucket },
      holesByPlayerKey: {},
    });
  } catch (_) {
    return false;
  }
}

function historyBucketLoading(dgId) {
  const id = Math.round(num(dgId, NaN));
  return Number.isFinite(id) && playerHistoryBucketLoadPromises.has(id);
}

async function loadPlayerHistoryBucket(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return false;
  if (historyBucketLoaded(id)) return true;
  if (isFileProtocol()) {
    await loadPlayerHistory();
    return historyBucketLoaded(id);
  }
  if (playerHistoryBucketLoadPromises.has(id)) return playerHistoryBucketLoadPromises.get(id);
  const p = (async () => {
    const url = cacheBustFetchUrl(`player-history/by-dg/${id}.json`);
    const fetchOpts = { cache: "no-store" };
    if (typeof AbortSignal !== "undefined" && typeof AbortSignal.timeout === "function") {
      fetchOpts.signal = AbortSignal.timeout(20000);
    }
    try {
      const res = await fetch(url, fetchOpts);
      if (res.ok) {
        const ok = mergePlayerHistoryPartialPayload(await res.json());
        if (ok && historyBucketLoaded(id)) return true;
      }
    } catch (_) {
      /* shard missing, timeout, or parse error */
    }
    if (await extractHistoryBucketFromEmbedded(id)) return true;
    return false;
  })();
  playerHistoryBucketLoadPromises.set(id, p);
  try {
    return await p;
  } finally {
    playerHistoryBucketLoadPromises.delete(id);
  }
}

/**
 * Prefer player_round_history.json (npm run build:history from historical_rounds_all.csv) when served
 * over HTTP; use embedded script as fallback or for file:// demos.
 */
async function loadPlayerHistory() {
  if (HISTORY._ok && !HISTORY._partial) return;
  const selDg = selectedDgId();
  if (Number.isFinite(selDg) && historyBucketLoaded(selDg)) return;
  if (playerHistoryLoadPromise) return playerHistoryLoadPromise;
  HISTORY = { ...HISTORY, _loading: true };
  playerHistoryLoadPromise = (async () => {
    if (isFileProtocol()) {
      await loadEmbeddedRoundHistoryScript();
      const emb = embeddedRoundHistoryPayload();
      if (emb) {
        applyPlayerHistoryPayload(emb, { partial: false });
        return;
      }
      HISTORY = { meta: {}, byDgId: {}, holesByPlayerKey: {}, _ok: false, _loading: false };
      HISTORY_ROUNDS_CHRONO_CACHE.clear();
      PRICING_MU_BONUS_CACHE.clear();
      return;
    }
    try {
      const res = await fetch(cacheBustFetchUrl("player_round_history.json"), { cache: "no-store" });
      if (res.ok) {
        applyPlayerHistoryPayload(await res.json(), { partial: false });
        return;
      }
    } catch (_) {}
    await loadEmbeddedRoundHistoryScript();
    const emb = embeddedRoundHistoryPayload();
    if (emb) {
      applyPlayerHistoryPayload(emb, { partial: false });
      return;
    }
    HISTORY = { meta: {}, byDgId: {}, holesByPlayerKey: {}, _ok: false, _loading: false };
    HISTORY_ROUNDS_CHRONO_CACHE.clear();
    PRICING_MU_BONUS_CACHE.clear();
  })();
  try {
    await playerHistoryLoadPromise;
  } finally {
    playerHistoryLoadPromise = null;
    if (HISTORY && typeof HISTORY === "object") HISTORY._loading = false;
  }
}

async function loadPlayerShots() {
  if (isFileProtocol()) {
    SHOTS = { meta: {}, byDgId: {}, _ok: false };
    return;
  }
  try {
    const res = await fetch("player_shots_web.json", { cache: "no-store" });
    if (!res.ok) throw new Error(String(res.status));
    SHOTS = { ...(await res.json()), _ok: true };
  } catch (_) {
    SHOTS = { meta: {}, byDgId: {}, _ok: false };
  }
}

function defaultPropGolferDgId() {
  const nm = (s) => String(s || "").toLowerCase();
  for (const p of DATA.players) {
    if (!samePlayerRound(p, 1)) continue;
    if (tournamentPostCutListPhase() && isPlayerEliminatedFromEvent(p)) continue;
    const n = nm(p.player_name);
    if (n.includes("scheffler") && n.includes("scottie")) return Math.round(num(p.dg_id, NaN));
  }
  return NaN;
}

/** Case-insensitive match on DataGolf name and display name; all query tokens must match somewhere. */
function golferNameMatchesQuery(nameRaw, qLower) {
  const q = String(qLower || "").trim().toLowerCase();
  if (!q) return true;
  const raw = String(nameRaw || "").toLowerCase();
  const disp = displayGolferName(String(nameRaw || "")).toLowerCase();
  if (disp.includes(q) || raw.includes(q)) return true;
  const parts = q.split(/\s+/).filter(Boolean);
  return parts.every((t) => disp.includes(t) || raw.includes(t));
}

/** Match full matchup label (`A vs B` / 3-ball) or either side for combobox search. */
function matchupRowLabelMatchesQuery(labelRaw, qLower) {
  const q = String(qLower || "").trim().toLowerCase();
  if (!q) return false;
  const label = String(labelRaw || "").trim();
  if (!label) return false;
  if (golferNameMatchesQuery(label, q)) return true;
  const disp = displayGolferName(label).trim().toLowerCase();
  if (disp === q || disp.includes(q)) return true;
  const segs = disp.split(/\s+vs\s+|\s*\/\s*/).map((s) => s.trim()).filter(Boolean);
  const toks = q.split(/\s+/).filter((t) => t.length >= 2);
  if (segs.length >= 2 && toks.length) {
    if (segs.some((seg) => toks.every((t) => seg.includes(t)))) return true;
  }
  return false;
}

/** Themed custom list (native `<datalist>` popups cannot match dark UI). */
const GOLFER_SUGGEST_PANEL_MAX = 80;

function golferSuggestReadLabels(panel) {
  if (!panel) return [];
  const raw = /** @type {{ _golferSuggestLabels?: string[] }} */ (/** @type {unknown} */ (panel))._golferSuggestLabels;
  return Array.isArray(raw) ? raw : [];
}

function golferSuggestWriteLabels(panel, labels) {
  if (!panel) return;
  /** @type {{ _golferSuggestLabels?: string[] }} */ (/** @type {unknown} */ (panel))._golferSuggestLabels = labels;
}

function filterGolferSuggestLabels(names, qRaw) {
  const q = String(qRaw || "").trim().toLowerCase();
  if (!q) return names.slice(0, GOLFER_SUGGEST_PANEL_MAX);
  const useMatchup = names.some((n) => /\s+vs\s+|\s+\/\s+/.test(String(n)));
  const out = [];
  for (const name of names) {
    const ok = useMatchup ? matchupRowLabelMatchesQuery(name, q) : golferNameMatchesQuery(name, q);
    if (ok) out.push(name);
    if (out.length >= GOLFER_SUGGEST_PANEL_MAX) break;
  }
  return out;
}

function hideOpenGolferSuggestPanels() {
  document.querySelectorAll(".golfer-suggest-panel").forEach((p) => {
    p.hidden = true;
    p.innerHTML = "";
  });
}

function renderOpenGolferSuggestPanel(panel, labels, onPick) {
  if (!panel) return;
  panel.innerHTML = "";
  if (!labels.length) {
    panel.hidden = true;
    return;
  }
  for (const lab of labels) {
    const b = document.createElement("button");
    b.type = "button";
    b.className = "golfer-suggest-item";
    b.setAttribute("role", "option");
    b.textContent = lab;
    b.addEventListener("pointerdown", (e) => {
      e.preventDefault();
      onPick(lab);
    });
    panel.appendChild(b);
  }
  panel.hidden = false;
}

let golferSuggestGlobalPointerWired = false;
function wireGolferSuggestGlobalDismissOnce() {
  if (golferSuggestGlobalPointerWired) return;
  golferSuggestGlobalPointerWired = true;
  document.addEventListener(
    "pointerdown",
    (ev) => {
      const t = ev.target;
      if (!(t instanceof Element)) return;
      if (t.closest(".golfer-suggest-panel") || t.closest(".golfer-suggest-anchor")) return;
      hideOpenGolferSuggestPanels();
    },
    true,
  );
}

function openGolferSuggestForSearchInput(search, panel, onPickFromList) {
  const labels = golferSuggestReadLabels(panel);
  const q = String(search.value || "");
  const picked = filterGolferSuggestLabels(labels, q);
  renderOpenGolferSuggestPanel(panel, picked, (lab) => {
    search.value = lab;
    hideOpenGolferSuggestPanels();
    onPickFromList(lab);
  });
}

/** After label cache updates: keep the panel open if the user is still typing (table/select refresh must not wipe it). */
function reopenGolferSuggestIfSearchFocused(search, panel, onPickFromList) {
  if (!panel) return;
  if (!search || document.activeElement !== search) {
    panel.innerHTML = "";
    panel.hidden = true;
    return;
  }
  openGolferSuggestForSearchInput(search, panel, onPickFromList);
}

function wireOuPlayerFilterSuggestOnce() {
  const search = document.getElementById("ou-player-filter");
  const panel = document.getElementById("ou-player-filter-suggest");
  if (!search || !panel || !(search instanceof HTMLInputElement) || search.dataset.golferSuggestWired === "1") return;
  search.dataset.golferSuggestWired = "1";
  wireGolferSuggestGlobalDismissOnce();
  const refreshPanel = () =>
    openGolferSuggestForSearchInput(search, panel, () => {
      ouProjExpandedKey = "";
      buildOuTable();
    });
  search.addEventListener("focus", refreshPanel);
  search.addEventListener("input", refreshPanel);
  search.addEventListener("blur", () => {
    setTimeout(() => {
      if (panel.contains(document.activeElement)) return;
      panel.hidden = true;
      panel.innerHTML = "";
    }, 160);
  });
}

function wireGolferSuggestComboOnce(selectId) {
  const search = document.getElementById(`${selectId}-search`);
  const panel = document.getElementById(`${selectId}-suggest`);
  const sel = document.getElementById(selectId);
  if (!search || !panel || !sel || search.dataset.golferSuggestWired === "1") return;
  search.dataset.golferSuggestWired = "1";
  wireGolferSuggestGlobalDismissOnce();
  const refreshPanel = () =>
    openGolferSuggestForSearchInput(search, panel, () => {
      commitGolferComboSearchToSelect(selectId);
      sel.dispatchEvent(new Event("change", { bubbles: true }));
    });
  // Matchup Analysis: do not open the suggestion list on focus alone (avoids a second “dropdown” feel); click or typing opens it.
  const openSuggestOnFocusOnly = selectId !== "analysis-matchup-select";
  if (openSuggestOnFocusOnly) search.addEventListener("focus", refreshPanel);
  else search.addEventListener("click", refreshPanel);
  search.addEventListener("input", refreshPanel);
  search.addEventListener("blur", () => {
    setTimeout(() => {
      if (panel.contains(document.activeElement)) return;
      panel.hidden = true;
      panel.innerHTML = "";
    }, 160);
  });
}

/** `select` + `#${id}-search` + `#${id}-suggest` panel — wire once after DOM. */
const GOLFER_COMBO_SELECT_IDS = [
  "prop-golfer",
  "live-prop-golfer",
  "course-fit-player",
  "hh-player",
  "ev-filter-golfer",
  "analysis-matchup-select",
];

function syncGolferComboSearchFromSelect(selectId) {
  const sel = document.getElementById(selectId);
  const search = document.getElementById(`${selectId}-search`);
  if (!sel || !search) return;
  const opt = sel.selectedOptions[0];
  if (!opt || String(opt.value || "") === "") {
    search.value = "";
    return;
  }
  search.value = opt ? String(opt.textContent || "").trim() : "";
}

function commitGolferComboSearchToSelect(selectId) {
  const sel = /** @type {HTMLSelectElement | null} */ (document.getElementById(selectId));
  const search = document.getElementById(`${selectId}-search`);
  if (!sel || !search) return;
  const q = String(search.value || "").trim();
  const qLow = q.toLowerCase();
  if (!q) {
    const empty = [...sel.options].find((o) => String(o.value || "") === "");
    if (empty) sel.value = "";
    else if (sel.options.length) sel.selectedIndex = 0;
    return;
  }
  let hit = [...sel.options].find((o) => String(o.textContent || "").trim().toLowerCase() === qLow);
  if (!hit) hit = [...sel.options].find((o) => String(o.textContent || "").toLowerCase().includes(qLow));
  if (!hit) hit = [...sel.options].find((o) => String(o.value || "") === q);
  if (!hit && selectId === "analysis-matchup-select" && matchupAnalysisRowsCache.length) {
    const row = matchupAnalysisRowsCache.find((r) => matchupRowLabelMatchesQuery(r.matchup, qLow));
    if (row) {
      matchupAnalysisSelectedKey = row.key;
      buildMatchupAnalysisTool();
      return;
    }
  }
  if (hit) {
    sel.value = String(hit.value);
    search.value = String(hit.value || "") === "" ? "" : String(hit.textContent || "").trim();
  } else {
    syncGolferComboSearchFromSelect(selectId);
  }
}

function refreshGolferComboboxFromSelect(selectId) {
  const sel = document.getElementById(selectId);
  const panel = document.getElementById(`${selectId}-suggest`);
  const search = document.getElementById(`${selectId}-search`);
  if (!sel || !panel) return;
  let labels;
  if (selectId === "analysis-matchup-select" && matchupAnalysisRowsCache.length) {
    labels = matchupAnalysisRowsCache.map((r) => String(r.matchup || "").trim()).filter(Boolean);
  } else {
    labels = [...sel.querySelectorAll("option")]
      .filter((o) => String(o.value || "") !== "")
      .map((o) => String(o.textContent || "").trim())
      .filter(Boolean);
  }
  golferSuggestWriteLabels(panel, labels);
  reopenGolferSuggestIfSearchFocused(search, panel, () => {
    commitGolferComboSearchToSelect(selectId);
    sel.dispatchEvent(new Event("change", { bubbles: true }));
  });
  if (!search || document.activeElement !== search) syncGolferComboSearchFromSelect(selectId);
}

function wireGolferSearchCombo(selectId) {
  const search = document.getElementById(`${selectId}-search`);
  const sel = document.getElementById(selectId);
  if (!search || !sel || search.dataset.golferComboWired === "1") return;
  search.dataset.golferComboWired = "1";
  const commit = () => {
    commitGolferComboSearchToSelect(selectId);
    sel.dispatchEvent(new Event("change", { bubbles: true }));
  };
  search.addEventListener("change", commit);
  search.addEventListener("blur", commit);
  sel.addEventListener("change", () => syncGolferComboSearchFromSelect(selectId));
}

function wireAllGolferSearchCombosOnce() {
  wireGolferSuggestGlobalDismissOnce();
  for (const id of GOLFER_COMBO_SELECT_IDS) {
    wireGolferSearchCombo(id);
    wireGolferSuggestComboOnce(id);
  }
}

function ouResolveSinglePlayerRowFromFilter(allRows, qTrim) {
  const q = String(qTrim || "").trim().toLowerCase();
  if (!q) return null;
  const m = allRows.filter((p) => golferNameMatchesQuery(String(p.player_name || ""), q));
  if (m.length === 1) return m[0];
  const exact = allRows.find((p) => String(p.player_name || "").trim().toLowerCase() === q);
  if (exact) return exact;
  return allRows.find((p) => displayGolferName(String(p.player_name || "")).trim().toLowerCase() === q) || null;
}

function ouResolveSinglePlayerNameForToolbar(allRows, qTrim) {
  const row = ouResolveSinglePlayerRowFromFilter(allRows, qTrim);
  return row ? String(row.player_name || "").trim() : "";
}

function fillFieldGolferSelect(selId, pickDefaultIdFn) {
  const sel = document.getElementById(selId);
  if (!sel) return;
  const prevVal = String(sel.value || "");
  const seen = new Set();
  const opts = [];
  for (const p of DATA.players) {
    if (!samePlayerRound(p, 1)) continue;
    if (tournamentPostCutListPhase() && isPlayerEliminatedFromEvent(p)) continue;
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || seen.has(id)) continue;
    seen.add(id);
    opts.push({ id, name: String(p.player_name || "") });
  }
  opts.sort((a, b) => displayGolferName(a.name).localeCompare(displayGolferName(b.name)));
  sel.innerHTML = "";
  for (const o of opts) {
    const op = document.createElement("option");
    op.value = String(o.id);
    op.textContent = displayGolferName(o.name);
    sel.appendChild(op);
  }
  const defaultId = pickDefaultIdFn ? pickDefaultIdFn() : NaN;
  let nextVal = "";
  if (prevVal && [...sel.options].some((o) => o.value === prevVal)) nextVal = prevVal;
  else if (Number.isFinite(defaultId) && [...sel.options].some((o) => o.value === String(defaultId))) nextVal = String(defaultId);
  else if (sel.options.length) nextVal = sel.options[0].value;

  sel.value = nextVal;
  const changed = prevVal !== String(sel.value || "");
  if (GOLFER_COMBO_SELECT_IDS.includes(selId)) refreshGolferComboboxFromSelect(selId);
  if (changed && (selId === "live-prop-golfer" || selId === "prop-golfer")) {
    sel.dispatchEvent(new Event("change", { bubbles: true }));
  }
}

function fillPropGolferSelect() {
  fillFieldGolferSelect("prop-golfer", defaultPropGolferDgId);
}

function fillLivePropGolferSelect() {
  fillFieldGolferSelect("live-prop-golfer", defaultLivePropGolferDgId);
}

function defaultLivePropGolferDgId() {
  const nm = (s) => String(s || "").toLowerCase();
  for (const p of DATA.players) {
    if (!samePlayerRound(p, 1)) continue;
    if (tournamentPostCutListPhase() && isPlayerEliminatedFromEvent(p)) continue;
    const n = nm(p.player_name);
    if (n.includes("cameron") && n.includes("young")) return Math.round(num(p.dg_id, NaN));
  }
  return defaultPropGolferDgId();
}

/** Strip DG live partial-round fields so remainder math uses full-round priors + user-entered “so far”. */
function rowWithoutLivePartialFields(row) {
  if (!row || typeof row !== "object") return row;
  const out = { ...row };
  delete out.dg_live_thru;
  delete out.dg_live_today;
  delete out.dg_live_birdies_so_far;
  delete out.dg_live_bogeys_so_far;
  delete out.dg_live_pars_so_far;
  delete out.dg_live_eagles_so_far;
  delete out.dg_live_placement_from_api;
  return out;
}

function livePropSampleMeanStd(xs) {
  const n = xs.length;
  if (!n) return { mean: NaN, std: NaN, n: 0 };
  const mean = xs.reduce((s, x) => s + x, 0) / n;
  if (n < 2) return { mean, std: NaN, n };
  let v = 0;
  for (const x of xs) v += (x - mean) ** 2;
  return { mean, std: Math.sqrt(v / (n - 1)), n };
}

/** Shotgun / split tees: playing order from `startHole` (1–18), wrapping (e.g. 10 → … → 18 → 1 → … → 9). */
function livePropPlayOrder(startHole) {
  const s = Math.round(num(startHole, NaN));
  const start = Number.isFinite(s) && s >= 1 && s <= 18 ? s : 1;
  const out = [];
  for (let i = 0; i < 18; i++) {
    let h = start + i;
    if (h > 18) h -= 18;
    out.push(h);
  }
  return out;
}

function livePropHoleParFromCard(holePars, holeNum1) {
  const hn = Math.round(num(holeNum1, NaN));
  if (!Array.isArray(holePars) || holePars.length < 18 || hn < 1 || hn > 18) return NaN;
  return num(holePars[hn - 1], NaN);
}

/** Sum par for the first N holes completed in `playOrder` using `meta.hole_pars` (hole numbers 1–18 on the card). */
function courseParSumPlayOrderThru(holePars, playOrder, nCompleted) {
  const n = Math.min(18, Math.max(0, Math.floor(num(nCompleted, NaN))));
  if (!n) return 0;
  if (!Array.isArray(holePars) || holePars.length < 18 || !Array.isArray(playOrder) || playOrder.length < 18) return NaN;
  let s = 0;
  for (let i = 0; i < n; i++) {
    const p = livePropHoleParFromCard(holePars, playOrder[i]);
    if (!Number.isFinite(p)) return NaN;
    s += p;
  }
  return s;
}

/** Fairway-eligible holes on the card (excludes par 3s) using current course `hole_pars`. */
function livePropFairwayOppFullFromPars(holePars) {
  if (!Array.isArray(holePars) || holePars.length < 18) return NaN;
  let c = 0;
  for (let i = 0; i < 18; i++) {
    const p = num(holePars[i], NaN);
    if (!Number.isFinite(p)) return NaN;
    if (p !== 3) c += 1;
  }
  return c;
}

/** Fairway opportunities in the first `nCompleted` holes of `playOrder` (par 3s excluded). */
function livePropFairwayOppPlayedThru(holePars, playOrder, nCompleted) {
  const n = Math.min(18, Math.max(0, Math.floor(num(nCompleted, NaN))));
  if (!n) return 0;
  if (!Array.isArray(holePars) || holePars.length < 18 || !Array.isArray(playOrder) || playOrder.length < 18) return NaN;
  let c = 0;
  for (let i = 0; i < n; i++) {
    const p = livePropHoleParFromCard(holePars, playOrder[i]);
    if (!Number.isFinite(p)) return NaN;
    if (p !== 3) c += 1;
  }
  return c;
}

function livePropDedupeHolesSorted(holes) {
  const byHole = new Map();
  if (!Array.isArray(holes)) return [];
  for (const h of holes) {
    const hn = Math.round(num(h.hole, NaN));
    if (!Number.isFinite(hn) || hn < 1 || hn > 18) continue;
    byHole.set(hn, h);
  }
  return [...byHole.entries()].sort((a, b) => a[0] - b[0]).map(([, h]) => h);
}

function livePropHolesForRound(pkey, r) {
  const hm = HISTORY.holesByPlayerKey[pkey];
  if (!hm || !r) return null;
  const rn = Math.round(num(r.round_num, NaN));
  if (!Number.isFinite(rn) || rn < 1) return null;
  const suf = `\tR${rn}`;
  const evN = normEvtNameKey(String(r.event_name || ""));
  let fuzzy = null;
  for (const k of Object.keys(hm)) {
    if (!k.endsWith(suf)) continue;
    const pref = k.slice(0, k.length - suf.length).trim();
    if (normEvtNameKey(pref) === evN) return hm[k];
    fuzzy ??= hm[k];
  }
  return fuzzy;
}

function livePropHolesCoverCompleted(holes, playOrder, completedHoles) {
  const deduped = livePropDedupeHolesSorted(holes);
  if (!deduped.length || completedHoles < 1) return false;
  if (!Array.isArray(playOrder) || playOrder.length < 18) return false;
  const have = new Set();
  for (const h of deduped) {
    const hn = Math.round(num(h.hole, NaN));
    if (Number.isFinite(hn)) have.add(hn);
  }
  const n = Math.min(18, Math.max(0, Math.round(completedHoles)));
  for (let i = 0; i < n; i++) {
    if (!have.has(playOrder[i])) return false;
  }
  return true;
}

function livePropCumulativeFromHoles(holes, playOrder, completedHoles, statKey) {
  if (!Array.isArray(holes) || !Array.isArray(playOrder) || playOrder.length < 18 || completedHoles < 1) return NaN;
  const byHole = new Map();
  for (const h of holes) {
    const hn = Math.round(num(h.hole, NaN));
    if (!Number.isFinite(hn) || hn < 1 || hn > 18) continue;
    byHole.set(hn, h);
  }
  const n = Math.min(18, Math.max(0, Math.round(completedHoles)));
  let strokes = 0;
  let birdies = 0;
  let pars = 0;
  let bogeys = 0;
  for (let i = 0; i < n; i++) {
    const hn = playOrder[i];
    const h = byHole.get(hn);
    if (!h) return NaN;
    const par = num(h.par, NaN);
    const sc = num(h.score, NaN);
    if (!Number.isFinite(sc) || !Number.isFinite(par)) continue;
    strokes += sc;
    const rel = sc - par;
    if (rel === -1) birdies += 1;
    if (rel === 0) pars += 1;
    if (rel >= 1) bogeys += 1;
  }
  if (statKey === "total") return strokes;
  if (statKey === "birdies") return birdies;
  if (statKey === "pars") return pars;
  if (statKey === "bogeys") return bogeys;
  return NaN;
}

/** Full-round counting stats from 18 scored holes (overrides projection placeholders on merged live rows when hole JSON exists). */
function historyFullRoundCountingStatFromHoles(statKey, holes) {
  if (!Array.isArray(holes) || holes.length < 1) return NaN;
  const deduped = livePropDedupeHolesSorted(holes);
  if (deduped.length !== 18) return NaN;
  let strokes = 0;
  let birdies = 0;
  let pars = 0;
  let bogeys = 0;
  for (const h of deduped) {
    const par = num(h.par, NaN);
    const sc = num(h.score, NaN);
    if (!Number.isFinite(sc) || !Number.isFinite(par)) return NaN;
    strokes += sc;
    const rel = sc - par;
    if (rel === -1) birdies += 1;
    if (rel === 0) pars += 1;
    if (rel >= 1) bogeys += 1;
  }
  if (statKey === "total") return strokes;
  if (statKey === "birdies") return birdies;
  if (statKey === "pars") return pars;
  if (statKey === "bogeys") return bogeys;
  return NaN;
}

function historyTrendRowHoleArray(row) {
  if (!row || typeof row !== "object") return null;
  const dg = Math.round(num(row.dg_id, NaN));
  if (!Number.isFinite(dg)) return null;
  const rec = HISTORY.byDgId && HISTORY.byDgId[String(dg)];
  const pname = String(rec?.player_name || "").trim();
  if (!pname) return null;
  const pkey = playerKeyFromName(pname);
  return livePropHolesForRound(pkey, row);
}

function livePropHistoricalRemainders(dgId, statKey, completedHoles, maxRounds, playOrder) {
  const samples = [];
  let holeBacked = 0;
  const rec = HISTORY.byDgId && HISTORY.byDgId[String(dgId)];
  if (!rec || !Array.isArray(rec.rounds)) return { samples, holeBacked, n: 0 };
  const pname = String(rec.player_name || "").trim();
  const pkey = playerKeyFromName(pname);
  const rounds = rec.rounds.slice().sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a));
  const cap =
    Number.isFinite(maxRounds) && maxRounds > 0 ? Math.min(maxRounds, rounds.length) : rounds.length;
  const ord = Array.isArray(playOrder) && playOrder.length >= 18 ? playOrder : livePropPlayOrder(1);
  for (let i = 0; i < cap; i++) {
    const r = rounds[i];
    const fin = actualForRoundRow(statKey, r);
    if (!Number.isFinite(fin)) continue;
    let cum = NaN;
    let usedHoleBack = false;
    const holes = livePropHolesForRound(pkey, r);
    if (
      completedHoles >= 1 &&
      holes &&
      livePropHolesCoverCompleted(holes, ord, completedHoles) &&
      (statKey === "total" || statKey === "birdies" || statKey === "pars" || statKey === "bogeys")
    ) {
      cum = livePropCumulativeFromHoles(holes, ord, completedHoles, statKey);
      if (Number.isFinite(cum)) usedHoleBack = true;
    }
    if (!Number.isFinite(cum)) {
      const c = completedHoles;
      cum = c > 0 ? fin * (c / 18) : 0;
    }
    if (usedHoleBack) holeBacked++;
    samples.push(fin - cum);
  }
  return { samples, holeBacked, n: samples.length };
}

function livePropModelRemainderSigma(marketLabel, rowClean, statKey, completedHoles, opts = {}) {
  const playOrder = Array.isArray(opts.playOrder) && opts.playOrder.length >= 18 ? opts.playOrder : livePropPlayOrder(1);
  const holePars = opts.holePars;
  const remHoles = clamp(18 - completedHoles, 0, 18);
  const muFull = ouProjectedMean(marketLabel, rowClean);
  const sigFull = sigmaForOu(marketLabel, rowClean);
  if (!Number.isFinite(muFull) || !Number.isFinite(sigFull)) return { muRem: NaN, sigRem: NaN };

  if (statKey === "fairways") {
    let fullFair = NaN;
    let playedFair = 0;
    if (Array.isArray(holePars) && holePars.length >= 18) {
      fullFair = livePropFairwayOppFullFromPars(holePars);
      playedFair = livePropFairwayOppPlayedThru(holePars, playOrder, completedHoles);
    }
    if (!Number.isFinite(fullFair) || fullFair < 1) {
      if (remHoles <= 0) return { muRem: 0, sigRem: Math.max(0.08, sigFull * 0.06) };
      const denom = fairwayHolesModeledFromData();
      const remU = Math.max(1, Math.round((denom / 18) * remHoles));
      return {
        muRem: (muFull / denom) * remU,
        sigRem: Math.max(0.2, sigFull * Math.sqrt(remU / denom)),
      };
    }
    const remFair = Math.max(0, fullFair - playedFair);
    if (remFair <= 0) return { muRem: 0, sigRem: Math.max(0.08, sigFull * 0.06) };
    return {
      muRem: (muFull / fullFair) * remFair,
      sigRem: Math.max(0.2, sigFull * Math.sqrt(remFair / fullFair)),
    };
  }

  if (remHoles <= 0) return { muRem: 0, sigRem: Math.max(0.08, sigFull * 0.06) };
  if (statKey === "gir" || statKey === "putts") {
    const denom = 18;
    return {
      muRem: (muFull / denom) * remHoles,
      sigRem: Math.max(0.25, sigFull * Math.sqrt(remHoles / denom)),
    };
  }
  return {
    muRem: muFull * (remHoles / 18),
    sigRem: Math.max(0.15, sigFull * Math.sqrt(remHoles / 18)),
  };
}

function renderLivePropPredictor() {
  const root = document.getElementById("live-prop-results");
  if (!root) return;

  const dg = Math.round(num(document.getElementById("live-prop-golfer")?.value, NaN));
  const statKey = String(document.getElementById("live-prop-market")?.value || "total");
  const startHoleRaw = Math.round(num(document.getElementById("live-prop-start-hole")?.value, NaN));
  const startHole = Number.isFinite(startHoleRaw) && startHoleRaw >= 1 && startHoleRaw <= 18 ? startHoleRaw : 1;
  const playOrder = livePropPlayOrder(startHole);
  const throughHoles = Math.round(num(document.getElementById("live-prop-through-holes")?.value, NaN));
  const curRaw = num(document.getElementById("live-prop-current")?.value, NaN);
  const lineRaw = num(document.getElementById("live-prop-line")?.value, NaN);
  const line = clampPropLineForMarket(statKey, enforceHalfLine(lineRaw));
  const oAm = num(document.getElementById("live-prop-over-am")?.value, NaN);
  const uAm = num(document.getElementById("live-prop-under-am")?.value, NaN);

  const completed = Number.isFinite(throughHoles) ? clamp(throughHoles, 0, 18) : NaN;
  const isRoundScoreMarket = statKey === "total";
  const par18 = num(DATA?.meta?.course_par_18, NaN);
  const holePars = DATA?.meta?.hole_pars;
  let parThru = courseParSumPlayOrderThru(holePars, playOrder, completed);
  if (!Number.isFinite(parThru)) {
    parThru = Number.isFinite(par18) && Number.isFinite(completed) ? (par18 / 18) * completed : NaN;
  }
  const currentForProjection = isRoundScoreMarket && Number.isFinite(parThru) ? parThru + curRaw : curRaw;
  const marketLabel = ouMarketKeyFromStatKey(statKey);
  const rEv = getModelRoundForEv();
  const rowRaw =
    projectionPlayerRowForModel(dg, rEv) ||
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg && samePlayerRound(p, rEv)) ||
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg);
  const rowClean = rowRaw ? rowWithoutLivePartialFields(rowRaw) : null;

  const histOk = Boolean(HISTORY._ok);

  if (
    !Number.isFinite(dg) ||
    !Number.isFinite(throughHoles) ||
    throughHoles < 0 ||
    throughHoles > 18 ||
    !Number.isFinite(curRaw) ||
    (!isRoundScoreMarket && curRaw < 0) ||
    (isRoundScoreMarket && !Number.isFinite(parThru)) ||
    !Number.isFinite(line)
  ) {
    root.innerHTML = `<p class="live-prop-placeholder">Complete the form to see projection and edge.</p>`;
    return;
  }

  if (!rowClean) {
    root.innerHTML = `<p class="text-warn">No projection row for this golfer — refresh projections.</p>`;
    return;
  }

  let muRem;
  let sigRem;

  if (statKey === "gir" || statKey === "fairways" || statKey === "putts") {
    const m = livePropModelRemainderSigma(marketLabel, rowClean, statKey, completed, { playOrder, holePars });
    muRem = m.muRem;
    sigRem = m.sigRem;
  } else {
    const { samples } = livePropHistoricalRemainders(dg, statKey, completed, undefined, playOrder);
    const mMod = livePropModelRemainderSigma(marketLabel, rowClean, statKey, completed, { playOrder, holePars });
    const { mean: muHist, std: sigHist, n: nEff } = livePropSampleMeanStd(samples);
    const w = Math.min(1, nEff / 26);
    if (histOk && nEff >= 6 && Number.isFinite(muHist) && Number.isFinite(sigHist)) {
      muRem = w * muHist + (1 - w) * mMod.muRem;
      sigRem = Math.max(0.25, w * sigHist + (1 - w) * mMod.sigRem);
    } else {
      muRem = mMod.muRem;
      sigRem = mMod.sigRem;
    }
  }

  if (!Number.isFinite(muRem) || !Number.isFinite(sigRem)) {
    root.innerHTML = `<p class="text-warn">Could not build remainder distribution for this market.</p>`;
    return;
  }

  const predFinal = currentForProjection + muRem;
  const sigF = Math.max(0.22, sigRem);
  const z = (line - predFinal) / sigF;
  const pOver = clampProb01(1 - normalCdf(z));
  const pUnder = clampProb01(1 - pOver);

  const dO = decimalFromAmerican(oAm);
  const dU = decimalFromAmerican(uAm);
  const evO = Number.isFinite(dO) && dO > 1 ? pOver * dO - 1 : NaN;
  const evU = Number.isFinite(dU) && dU > 1 ? pUnder * dU - 1 : NaN;

  const LIVE_PROP_MODEL_VIG = 0.075;
  const vigOverProb = clampProb01(pOver * (1 + LIVE_PROP_MODEL_VIG));
  const vigUnderProb = clampProb01(pUnder * (1 + LIVE_PROP_MODEL_VIG));
  const fairOverAm = americanFromImpliedProb(vigOverProb);
  const fairUnderAm = americanFromImpliedProb(vigUnderProb);
  const fmtFair = (am) => (Number.isFinite(am) ? formatAmerican(am) : "—");
  const fmtEv = (x) =>
    Number.isFinite(x)
      ? `<span class="${x >= 0 ? "ev-pos" : "ev-neg"}">${(x * 100).toFixed(1)}%</span>`
      : "—";

  const prec = statKey === "total" ? 2 : 1;
  root.innerHTML = `
    <div class="live-prop-result-grid">
      <div class="live-prop-metric">
        <span class="live-prop-metric-label">Projected</span>
        <span class="live-prop-metric-val">${predFinal.toFixed(prec)}</span>
      </div>
      <div class="live-prop-metric">
        <span class="live-prop-metric-label">Standard deviation</span>
        <span class="live-prop-metric-val">${sigF.toFixed(2)}</span>
      </div>
      <div class="live-prop-metric">
        <span class="live-prop-metric-label">Over fair price</span>
        <span class="live-prop-metric-val">${fmtFair(fairOverAm)}</span>
      </div>
      <div class="live-prop-metric">
        <span class="live-prop-metric-label">Under fair price</span>
        <span class="live-prop-metric-val">${fmtFair(fairUnderAm)}</span>
      </div>
      <div class="live-prop-metric">
        <span class="live-prop-metric-label">EV Over</span>
        <span class="live-prop-metric-val">${fmtEv(evO)}</span>
      </div>
      <div class="live-prop-metric">
        <span class="live-prop-metric-label">EV Under</span>
        <span class="live-prop-metric-val">${fmtEv(evU)}</span>
      </div>
    </div>`;
}

/** DraftKings round props in DATA.props (from fetch:dk-ou / fetch:book-odds): fill line + American odds when available for golfer + market. */
function syncLivePropBookLineAndOddsFromDk() {
  const statKey = String(document.getElementById("live-prop-market")?.value || "total");
  const dg = Math.round(num(document.getElementById("live-prop-golfer")?.value, NaN));
  const lineEl = document.getElementById("live-prop-line");
  const oEl = document.getElementById("live-prop-over-am");
  const uEl = document.getElementById("live-prop-under-am");
  if (!lineEl || !oEl || !uEl) return;

  const fallbackLineAndOdds = () => {
    const d = defaultPropLineForStat(statKey);
    lineEl.value = formatPropLineValueForInput(clampPropLineForMarket(statKey, d));
    oEl.value = formatAmericanOddsInput(OU_DEFAULT_ODDS_AM);
    uEl.value = formatAmericanOddsInput(OU_DEFAULT_ODDS_AM);
  };

  if (!Number.isFinite(dg) || dg <= 0) {
    fallbackLineAndOdds();
    return;
  }

  const marketLabel = ouMarketKeyFromStatKey(statKey);
  const rEv = getModelRoundForEv();
  const rowRaw =
    projectionPlayerRowForModel(dg, rEv) ||
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg && samePlayerRound(p, rEv)) ||
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg);
  const rowClean = rowRaw ? rowWithoutLivePartialFields(rowRaw) : null;

  if (!rowClean) {
    fallbackLineAndOdds();
    return;
  }

  const mu = ouProjectedMean(marketLabel, rowClean);
  const pick = chooseOuPropLineForProjection(marketLabel, rowClean, mu);
  if (pick && Number.isFinite(pick.line)) {
    lineEl.value = formatPropLineValueForInput(clampPropLineForMarket(statKey, pick.line));
    oEl.value = formatAmericanOddsInput(pick.over);
    uEl.value = formatAmericanOddsInput(pick.under);
    return;
  }

  fallbackLineAndOdds();
}

function syncLivePropCurrentInputLabel() {
  const statKey = String(document.getElementById("live-prop-market")?.value || "total");
  const through = Math.round(num(document.getElementById("live-prop-through-holes")?.value, NaN));
  const startHoleRaw = Math.round(num(document.getElementById("live-prop-start-hole")?.value, NaN));
  const startHole = Number.isFinite(startHoleRaw) && startHoleRaw >= 1 && startHoleRaw <= 18 ? startHoleRaw : 1;
  const playOrder = livePropPlayOrder(startHole);
  const labelEl = document.getElementById("live-prop-current-label");
  const inputEl = document.getElementById("live-prop-current");
  if (!labelEl || !inputEl) return;
  if (statKey === "total") {
    const par18 = num(DATA?.meta?.course_par_18, NaN);
    const holePars = DATA?.meta?.hole_pars;
    let parThru = courseParSumPlayOrderThru(holePars, playOrder, through);
    if (!Number.isFinite(parThru) && Number.isFinite(par18) && Number.isFinite(through)) {
      parThru = (par18 / 18) * clamp(through, 0, 18);
    }
    labelEl.textContent = "Current to par";
    inputEl.min = "-18";
    inputEl.step = "1";
    inputEl.placeholder = "0";
  } else {
    labelEl.textContent = "Current total";
    inputEl.min = "0";
    inputEl.step = "0.1";
    inputEl.placeholder = "0";
  }
}

function initLivePropPredictorUi() {
  const marketEl = document.getElementById("live-prop-market");
  if (marketEl && [...marketEl.options].some((o) => o.value === "birdies")) marketEl.value = "birdies";
  const throughEl = document.getElementById("live-prop-through-holes");
  if (throughEl) throughEl.value = "6";
  const startHoleEl = document.getElementById("live-prop-start-hole");
  if (startHoleEl) startHoleEl.value = "1";
  const currentEl = document.getElementById("live-prop-current");
  if (currentEl) currentEl.value = "2";
  const lineEl = document.getElementById("live-prop-line");
  if (lineEl) lineEl.value = "4.5";
  syncLivePropCurrentInputLabel();
  const ids = [
    "live-prop-golfer",
    "live-prop-market",
    "live-prop-start-hole",
    "live-prop-through-holes",
    "live-prop-current",
    "live-prop-line",
    "live-prop-over-am",
    "live-prop-under-am",
  ];
  for (const id of ids) {
    document.getElementById(id)?.addEventListener("change", () => {
      if (id === "live-prop-market" || id === "live-prop-golfer") syncLivePropBookLineAndOddsFromDk();
      if (id === "live-prop-market" || id === "live-prop-through-holes" || id === "live-prop-start-hole") {
        syncLivePropCurrentInputLabel();
      }
      renderLivePropPredictor();
    });
    document.getElementById(id)?.addEventListener("input", () => {
      if (id === "live-prop-market" || id === "live-prop-through-holes" || id === "live-prop-start-hole") {
        syncLivePropCurrentInputLabel();
      }
      renderLivePropPredictor();
    });
  }
  fillLivePropGolferSelect();
  syncLivePropBookLineAndOddsFromDk();
  renderLivePropPredictor();
}

function selectedDgId() {
  const sel = document.getElementById("prop-golfer");
  return sel ? Math.round(num(sel.value, NaN)) : NaN;
}

function statKeyFromPropSelect() {
  const sel = document.getElementById("prop-stat");
  const v = sel ? sel.value : "total";
  return v === "putts" ? "total" : v;
}

/** Putting market removed from Historical Trends — reset stale saved selection. */
function ensurePropsStatSelectValid() {
  const sel = document.getElementById("prop-stat");
  if (!sel || sel.value !== "putts") return;
  sel.value = "total";
}

function ensureOuMarketFilterValid() {
  const el = document.getElementById("ou-market-filter");
  if (!el || el.value !== "Putts") return;
  el.value = "Total score";
  const lineInp = document.getElementById("ou-line-filter");
  if (lineInp) lineInp.value = "70.5";
}

function historyRoundsForDg(dgId) {
  const rec = HISTORY.byDgId && HISTORY.byDgId[String(dgId)];
  if (!rec || !Array.isArray(rec.rounds)) return [];
  return rec.rounds.filter((r) => historyRoundCountsAsActual(r));
}

/** YYYYMMDD * 10 + round_num; matches build-player-history sortKey when present, else parses event_completed. */
function historyRoundChronoKey(r) {
  if (!r || typeof r !== "object") return 0;
  const sk = num(r.sortKey, NaN);
  if (Number.isFinite(sk) && sk > 0) return sk;
  const base = parseEventCompletedChronoBase(r.event_completed);
  const rn = num(r.round_num, NaN);
  const rnd = Number.isFinite(rn) && rn > 0 ? rn : 1;
  return base * 10 + rnd;
}

function parseEventCompletedChronoBase(s) {
  if (!s) return 0;
  const t = String(s).trim();
  const iso = t.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) {
    const y = parseInt(iso[1], 10);
    const mo = parseInt(iso[2], 10);
    const d = parseInt(iso[3], 10);
    if (!Number.isFinite(y)) return 0;
    return y * 10000 + (mo || 0) * 100 + (d || 0);
  }
  const p = t.split("/").map((x) => x.trim());
  if (p.length !== 3) return 0;
  const mo = parseInt(p[0], 10);
  const d = parseInt(p[1], 10);
  let y = parseInt(p[2], 10);
  if (!Number.isFinite(y)) return 0;
  if (y < 100) y += y >= 70 ? 1900 : 2000;
  return y * 10000 + (mo || 0) * 100 + (d || 0);
}

function venueCourseName() {
  return String(DATA.meta.course_used || DATA.course_used || "").trim().toLowerCase();
}

function courseFilterOn() {
  const cb = document.getElementById("props-filter-current-course");
  return Boolean(cb && cb.checked);
}

/** Map a typed °F value to the same bucket keys used by weatherRangeMatch ("temp"). Empty input → "" */
function propsWeatherBucketFromTempF(t) {
  if (!Number.isFinite(t)) return "";
  if (t < 60) return "lt60";
  if (t < 70) return "60_69";
  if (t < 80) return "70_79";
  if (t < 90) return "80_89";
  return "gte90";
}

/** Map typed wind mph to weatherRangeMatch wind buckets. */
function propsWeatherBucketFromWindMph(w) {
  if (!Number.isFinite(w)) return "";
  if (w < 6) return "0_5";
  if (w < 11) return "6_10";
  if (w < 16) return "11_15";
  if (w < 21) return "16_20";
  return "gte21";
}

/** Map typed humidity % to weatherRangeMatch humidity buckets. */
function propsWeatherBucketFromHumidityPct(h) {
  if (!Number.isFinite(h)) return "";
  if (h < 40) return "lt40";
  if (h < 60) return "40_59";
  if (h < 80) return "60_79";
  return "gte80";
}

/** °F min/max from Historical Trends filters; blank = unbounded. If min &gt; max, values are swapped. */
function propsTempBoundsFromDom() {
  const rsMin = String(document.getElementById("props-filter-temp-min")?.value ?? "").trim();
  const rsMax = String(document.getElementById("props-filter-temp-max")?.value ?? "").trim();
  let minF = rsMin ? parseWeatherNumber(rsMin) : NaN;
  let maxF = rsMax ? parseWeatherNumber(rsMax) : NaN;
  if (!Number.isFinite(minF)) minF = NaN;
  if (!Number.isFinite(maxF)) maxF = NaN;
  if (Number.isFinite(minF) && Number.isFinite(maxF) && minF > maxF) {
    const t = minF;
    minF = maxF;
    maxF = t;
  }
  return { minF, maxF };
}

function propsTempFilterActive() {
  const { minF, maxF } = propsTempBoundsFromDom();
  return Number.isFinite(minF) || Number.isFinite(maxF);
}

function historyRoundTempFInBand(tempF, minF, maxF) {
  if (!Number.isFinite(tempF)) return false;
  if (Number.isFinite(minF) && tempF < minF) return false;
  if (Number.isFinite(maxF) && tempF > maxF) return false;
  return true;
}

function filterHistoryRoundsByTempRange(list) {
  const { minF, maxF } = propsTempBoundsFromDom();
  if (!Number.isFinite(minF) && !Number.isFinite(maxF)) return list;
  return list.filter((r) =>
    historyRoundTempFInBand(parseWeatherNumber(r?.pga_meta_weather_temp_f ?? r?.weather_temp_f), minF, maxF),
  );
}

function propsTrendTempContextKey() {
  const { minF, maxF } = propsTempBoundsFromDom();
  if (!Number.isFinite(minF) && !Number.isFinite(maxF)) return "all";
  const a = Number.isFinite(minF) ? String(minF) : "";
  const b = Number.isFinite(maxF) ? String(maxF) : "";
  return `${a}:${b}`;
}

function selectedPropsWindRangeFilter() {
  const raw = String(document.getElementById("props-filter-wind-range")?.value ?? "").trim();
  if (!raw) return "";
  const w = parseWeatherNumber(raw);
  return propsWeatherBucketFromWindMph(w);
}

function selectedPropsHumidityRangeFilter() {
  const raw = String(document.getElementById("props-filter-humidity-range")?.value ?? "").trim();
  if (!raw) return "";
  const h = parseWeatherNumber(raw);
  return propsWeatherBucketFromHumidityPct(h);
}

function selectedPropsCourseFilter() {
  const raw = String(document.getElementById("props-filter-course")?.value || "").trim();
  if (!raw) return "";
  return normCourseNameKey(raw);
}

/** Event venue key for Historical Trends course window (projections meta). */
function propsEventVenueCourseKey() {
  const metaVenue = String(DATA?.meta?.course_used || DATA?.course_used || "").trim();
  return metaVenue ? normCourseNameKey(metaVenue) : "";
}

/** Field-by-course mode always uses this week's venue from projections, not the golfer "All" filter. */
function propsEffectiveCourseKey() {
  if (!propsCourseWindowModeOn()) return selectedPropsCourseFilter();
  return propsEventVenueCourseKey() || selectedPropsCourseFilter();
}

function ensurePropsCourseSelectedForWindow() {
  if (!propsCourseWindowModeOn()) return false;
  const courseSel = document.getElementById("props-filter-course");
  if (!courseSel) return false;
  const prefer = propsEventVenueCourseKey();
  if (!prefer) return false;
  if ([...courseSel.options].some((o) => o.value === prefer)) {
    if (courseSel.value !== prefer) {
      courseSel.value = prefer;
      return true;
    }
    return false;
  }
  return false;
}

/** UTC calendar day of the trends chart timestamp for one round row → `YYYY-MM-DD`. */
function historyRoundChartUtcIsoDay(row) {
  const ms = historyRoundChartDateUtcMs(row);
  if (!Number.isFinite(ms)) return "";
  const d = new Date(ms);
  return `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
}

/** Safe filename for `player-history/by-course/*.json` (must match build-player-history.mjs). */
function propsCourseShardFileName(courseKey) {
  const safe = String(courseKey || "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 96);
  return `${safe || "unknown"}.json`;
}

function propsStoreSingleCourseBucket(courseKey, bucket) {
  propsSingleCourseIndexSig = `${historyMutationEpoch}|${courseKey}`;
  propsSingleCourseIndexCache = bucket;
  propsCourseRoundIndex.set(courseKey, bucket);
  const ep = historyMutationEpoch;
  distinctCourseSessionDatesCache.set(courseKey, { epoch: ep, days: bucket.days });
}

function propsGetSingleCourseBucketSync(courseKey) {
  if (!courseKey) return null;
  const sig = `${historyMutationEpoch}|${courseKey}`;
  if (propsSingleCourseIndexSig === sig && propsSingleCourseIndexCache) return propsSingleCourseIndexCache;
  const fromMap = propsCourseRoundIndex.get(courseKey);
  if (fromMap) return fromMap;
  return null;
}

/** ISO YYYY-MM-DD for tournament round N from field week start (R1 = date_start). */
function propsIsoFromFieldDateStartAndRound(dateStartIso, roundNum) {
  const m = String(dateStartIso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return "";
  const rn = Math.round(num(roundNum, NaN));
  if (!Number.isFinite(rn) || rn < 1 || rn > 4) return m[0];
  const t = Date.UTC(+m[1], +m[2] - 1, +m[3]) + (rn - 1) * 86400000;
  const d = new Date(t);
  return `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
}

/** Default session date from projections (current round when in play). */
function propsDefaultSessionIsoFromMeta() {
  const ds = String(DATA?.meta?.datagolf_field_date_start || "").match(/^(\d{4}-\d{2}-\d{2})/);
  const cr = Math.round(num(DATA?.meta?.datagolf_field_current_round ?? DATA?.meta?.display_round, NaN));
  if (ds && Number.isFinite(cr) && cr >= 1 && cr <= 4) {
    return propsIsoFromFieldDateStartAndRound(ds[0], cr);
  }
  if (ds) return ds[1];
  const upd = String(DATA?.meta?.updated_at || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (upd && Number.isFinite(cr) && cr >= 1 && cr <= 4) {
    const t = Date.UTC(+upd[1], +upd[2] - 1, +upd[3]) - (cr - 1) * 86400000;
    const d = new Date(t);
    return `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
  }
  return "";
}

function ensurePropsCourseWindowDateDefaultsFromMeta() {
  if (!propsCourseWindowModeOn()) return;
  const fromEl = document.getElementById("props-filter-date-from");
  const toEl = document.getElementById("props-filter-date-to");
  if (!fromEl || !toEl) return;
  if (String(fromEl.value || "").trim() || String(toEl.value || "").trim()) return;
  const iso = propsDefaultSessionIsoFromMeta();
  if (iso) {
    fromEl.value = iso;
    toEl.value = iso;
  }
}

async function loadPropsCoursesManifest() {
  if (propsCoursesManifestCache) return propsCoursesManifestCache;
  if (propsCoursesManifestPromise) return propsCoursesManifestPromise;
  propsCoursesManifestPromise = (async () => {
    if (isFileProtocol()) return { courses: [] };
    try {
      const res = await fetch(cacheBustFetchUrl("player-history/courses-manifest.json"), { cache: "no-store" });
      if (res.ok) {
        propsCoursesManifestCache = await res.json();
        return propsCoursesManifestCache;
      }
    } catch (_) {
      /* manifest optional */
    }
    propsCoursesManifestCache = { courses: [] };
    return propsCoursesManifestCache;
  })().finally(() => {
    propsCoursesManifestPromise = null;
  });
  return propsCoursesManifestPromise;
}

function propsCourseShardFilesToTry(courseKey) {
  const files = new Set();
  if (courseKey) files.add(propsCourseShardFileName(courseKey));
  const venueRaw = String(DATA?.meta?.course_used || DATA?.course_used || "").trim();
  for (const c of propsCoursesManifestCache?.courses || []) {
    const ck = String(c.course_key || "").trim();
    const file = String(c.file || "").trim();
    if (!file) continue;
    if (ck && normCourseNameKey(ck) === courseKey) files.add(file);
    if (venueRaw && ck && courseNameMatchesVenueLoose(ck, venueRaw)) files.add(file);
  }
  return [...files];
}

function parsePropsCourseShardPayload(j) {
  const entries = [];
  for (const e of j.entries || []) {
    const row = e.row && typeof e.row === "object" ? e.row : e;
    const dgId = Math.round(num(e.dg_id ?? row?.dg_id, NaN));
    if (!Number.isFinite(dgId)) continue;
    entries.push({
      row,
      dgId,
      playerName: resolveGolferDisplayNameForDg(dgId, e.player_name || row?.player_name),
    });
  }
  const days = Array.isArray(j.days) ? j.days.map(String) : [];
  return { days, entries };
}

function propsCourseWindowEntryDedupeKey(e) {
  const r = e?.row;
  const sk = Math.round(num(r?.sortKey, NaN));
  if (Number.isFinite(sk) && sk > 0) return `${e.dgId}|${sk}`;
  const yr = Math.round(num(r?.year, NaN));
  const rn = Math.round(num(r?.round_num, NaN));
  const ev = String(r?.event_name || "").trim().toLowerCase();
  return `${e.dgId}|${yr}|${rn}|${ev}`;
}

/** Supplement prebuilt course shard with live-week rows from in-memory HISTORY (not yet in shard file). */
function mergeMemoryCourseEntriesIntoBucket(bucket, courseKey) {
  if (!bucket || !courseKey || !HISTORY?.byDgId) return bucket;
  const ep = historyMutationEpoch;
  if (bucket._memoryMergeEpoch === ep) return bucket;
  const seen = new Set((bucket.entries || []).map(propsCourseWindowEntryDedupeKey));
  const dateSet = new Set(Array.isArray(bucket.days) ? bucket.days : []);
  const nameByDg = buildPropsGolferDisplayNameMap();
  const dgIds = new Set();
  if (Array.isArray(DATA?.players)) {
    for (const p of DATA.players) {
      const id = Math.round(num(p.dg_id, NaN));
      if (Number.isFinite(id)) dgIds.add(id);
    }
  }
  const scanAll = propsHistorySmallEnoughForMemoryCourseIndex();
  const sources = scanAll
    ? Object.entries(HISTORY.byDgId)
    : [...dgIds].map((id) => [String(id), HISTORY.byDgId[String(id)]]);
  for (const [dgStr, rec] of sources) {
    const dgId = Math.round(num(dgStr, NaN));
    if (!Number.isFinite(dgId) || !rec || !Array.isArray(rec.rounds)) continue;
    const playerName = resolveGolferDisplayNameForDg(dgId, nameByDg.get(dgId), nameByDg);
    for (const r of rec.rounds) {
      if (!historyRoundCountsAsActual(r)) continue;
      if (normCourseNameKey(r.course_name) !== courseKey) continue;
      const entry = { row: r, dgId, playerName };
      const key = propsCourseWindowEntryDedupeKey(entry);
      if (seen.has(key)) continue;
      seen.add(key);
      bucket.entries.push(entry);
      const iso = historyRoundChartUtcIsoDay(r);
      if (iso) dateSet.add(iso);
    }
  }
  bucket.days = [...dateSet].sort((a, b) => b.localeCompare(a));
  bucket._memoryMergeEpoch = ep;
  return bucket;
}

async function fetchPropsCourseHistoryShard(courseKey) {
  if (!courseKey || isFileProtocol()) return null;
  await loadPropsCoursesManifest();
  const files = propsCourseShardFilesToTry(courseKey);
  for (const file of files) {
    try {
      const res = await fetch(cacheBustFetchUrl(`player-history/by-course/${file}`), { cache: "no-store" });
      if (!res.ok) continue;
      const parsed = parsePropsCourseShardPayload(await res.json());
      if (parsed.entries.length) return parsed;
    } catch (_) {
      /* try next alias file */
    }
  }
  return null;
}

/** True when in-memory scan is small enough not to freeze the tab. */
function propsHistorySmallEnoughForMemoryCourseIndex() {
  const by = HISTORY?.byDgId;
  if (!by) return true;
  const keys = Object.keys(by);
  if (keys.length > 24) return false;
  let rounds = 0;
  for (const k of keys) {
    rounds += by[k]?.rounds?.length || 0;
    if (rounds > 4000) return false;
  }
  return true;
}

/** Scan in-memory history for one venue only, yielding so the tab stays responsive. */
async function buildPropsSingleCourseIndexFromMemory(courseKey) {
  const dateSet = new Set();
  const entries = [];
  if (!courseKey || !HISTORY?.byDgId) return { days: [], entries };
  const nameByDg = buildPropsGolferDisplayNameMap();
  const dgEntries = Object.entries(HISTORY.byDgId);
  for (let i = 0; i < dgEntries.length; i += PROPS_COURSE_INDEX_PLAYER_CHUNK) {
    for (const [dgStr, rec] of dgEntries.slice(i, i + PROPS_COURSE_INDEX_PLAYER_CHUNK)) {
      const dgId = Math.round(num(dgStr, NaN));
      if (!Number.isFinite(dgId) || !rec || !Array.isArray(rec.rounds)) continue;
      const playerName = resolveGolferDisplayNameForDg(dgId, nameByDg.get(dgId), nameByDg);
      for (const r of rec.rounds) {
        if (!historyRoundCountsAsActual(r)) continue;
        if (normCourseNameKey(r.course_name) !== courseKey) continue;
        entries.push({ row: r, dgId, playerName });
        const iso = historyRoundChartUtcIsoDay(r);
        if (iso) dateSet.add(iso);
      }
    }
    if (i + PROPS_COURSE_INDEX_PLAYER_CHUNK < dgEntries.length) await yieldToMain();
  }
  return { days: [...dateSet].sort((a, b) => b.localeCompare(a)), entries };
}

/**
 * Field-by-course: prefer prebuilt `player-history/by-course/*.json`, else chunked in-memory scan.
 * Never builds the all-courses index (that was freezing the tab).
 */
async function ensurePropsCourseIndexForKeyAsync(courseKey) {
  if (!courseKey) return { days: [], entries: [] };
  const cached = propsGetSingleCourseBucketSync(courseKey);
  if (cached) return cached;
  if (propsSingleCourseIndexPromise && propsSingleCourseIndexCourseKey === courseKey) {
    return propsSingleCourseIndexPromise;
  }
  propsSingleCourseIndexCourseKey = courseKey;
  propsSingleCourseIndexPromise = (async () => {
    const shard = await fetchPropsCourseHistoryShard(courseKey);
    if (shard?.entries?.length) {
      mergeMemoryCourseEntriesIntoBucket(shard, courseKey);
      propsStoreSingleCourseBucket(courseKey, shard);
      return shard;
    }
    if (propsHistorySmallEnoughForMemoryCourseIndex()) {
      const built = await buildPropsSingleCourseIndexFromMemory(courseKey);
      propsStoreSingleCourseBucket(courseKey, built);
      return built;
    }
    const iso = propsDefaultSessionIsoFromMeta();
    return {
      days: iso ? [iso] : [],
      entries: [],
      shardMissing: true,
    };
  })().finally(() => {
    propsSingleCourseIndexPromise = null;
  });
  return propsSingleCourseIndexPromise;
}

/**
 * Build full course dropdown list only (single-player mode). Field-by-course skips this scan.
 */
function rebuildPropsCourseRoundIndex() {
  const sig = String(historyMutationEpoch);
  if (propsCourseRoundIndexSig === sig && propsCourseRoundIndex.size > 0) return;
  propsCourseRoundIndexSig = sig;
  propsCourseRoundIndex.clear();
  if (!HISTORY?.byDgId) return;
  const nameByDg = buildPropsGolferDisplayNameMap();
  const tmp = new Map();
  for (const [dgStr, rec] of Object.entries(HISTORY.byDgId)) {
    const dgId = Math.round(num(dgStr, NaN));
    if (!Number.isFinite(dgId) || !rec || !Array.isArray(rec.rounds)) continue;
    const playerName = resolveGolferDisplayNameForDg(dgId, nameByDg.get(dgId), nameByDg);
    for (const r of rec.rounds) {
      if (!historyRoundCountsAsActual(r)) continue;
      const ck = normCourseNameKey(r.course_name);
      if (!ck) continue;
      let bucket = tmp.get(ck);
      if (!bucket) {
        bucket = { dateSet: new Set(), entries: [] };
        tmp.set(ck, bucket);
      }
      bucket.entries.push({ row: r, dgId, playerName });
      const iso = historyRoundChartUtcIsoDay(r);
      if (iso) bucket.dateSet.add(iso);
    }
  }
  for (const [ck, bucket] of tmp) {
    propsCourseRoundIndex.set(ck, {
      days: [...bucket.dateSet].sort((a, b) => b.localeCompare(a)),
      entries: bucket.entries,
    });
  }
}

function distinctCompletedRoundDatesAtCourse(courseKey) {
  if (!courseKey || !HISTORY?.byDgId) return [];
  const bucket = propsGetSingleCourseBucketSync(courseKey);
  if (bucket) return bucket.days;
  const ep = historyMutationEpoch;
  const hit = distinctCourseSessionDatesCache.get(courseKey);
  if (hit && hit.epoch === ep) return hit.days;
  return [];
}

function formatPropsCourseSessionDateLabel(isoRaw) {
  const m = String(isoRaw || "").match(/^(\d{4})-(\d{2})-(\d{2})$/);
  if (!m) return String(isoRaw || "").trim() || "—";
  const mo = Number(m[2]);
  const d = Number(m[3]);
  const y = Number(m[1]);
  const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  return `${months[mo - 1]} ${d}, ${y}`;
}

/** Prefer this week's round day from meta when it appears in course history. */
function propsEventPreferredSessionDateIso(sortedDescDays) {
  const ds = String(DATA?.meta?.datagolf_field_date_start || "").match(/^(\d{4}-\d{2}-\d{2})/);
  const cr = Math.round(num(DATA?.meta?.datagolf_field_current_round ?? DATA?.meta?.display_round, NaN));
  if (ds && Number.isFinite(cr) && cr >= 1 && cr <= 4) {
    const s = propsIsoFromFieldDateStartAndRound(ds[0], cr);
    if (s && sortedDescDays.includes(s)) return s;
  }
  if (ds) {
    const s = ds[1];
    if (sortedDescDays.includes(s)) return s;
  }
  const upd = String(DATA?.meta?.updated_at || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (upd && Number.isFinite(cr) && cr >= 1 && cr <= 4) {
    const t = Date.UTC(+upd[1], +upd[2] - 1, +upd[3]) - (cr - 1) * 86400000;
    const d = new Date(t);
    const s = `${d.getUTCFullYear()}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${String(d.getUTCDate()).padStart(2, "0")}`;
    if (sortedDescDays.includes(s)) return s;
  }
  return "";
}

/** Match old session dropdown: pick defaults when entering field-by-course or changing course while dates are unset. */
function ensurePropsCourseWindowDateDefaults() {
  if (!propsCourseWindowModeOn()) {
    propsCourseWindowDateDefaultsCourseTracked = "";
    return;
  }
  const courseKey = propsEffectiveCourseKey();
  const fromEl = document.getElementById("props-filter-date-from");
  const toEl = document.getElementById("props-filter-date-to");
  if (!fromEl || !toEl || !courseKey || !HISTORY?._ok) return;

  const applyDefaultIso = (isoFallback) => {
    if (isoFallback) {
      fromEl.value = isoFallback;
      toEl.value = isoFallback;
    } else {
      fromEl.value = "";
      toEl.value = "";
    }
  };

  let days = distinctCompletedRoundDatesAtCourse(courseKey);
  if (!days.length) {
    const iso = propsDefaultSessionIsoFromMeta();
    if (iso) days = [iso];
  }

  const bumpedCourse = courseKey !== propsCourseWindowDateDefaultsCourseTracked;
  propsCourseWindowDateDefaultsCourseTracked = courseKey;

  const fromRaw = String(fromEl.value || "").trim();
  const toRaw = String(toEl.value || "").trim();

  const pickPreferred = () =>
    propsEventPreferredSessionDateIso(days) || (days.length ? days[0] : "");

  if (!bumpedCourse) {
    if (!fromRaw && !toRaw) applyDefaultIso(pickPreferred());
    return;
  }

  const prevIso = fromRaw && toRaw && fromRaw === toRaw ? fromRaw : "";
  if (prevIso && days.includes(prevIso)) applyDefaultIso(prevIso);
  else applyDefaultIso(pickPreferred());
}

function propsCourseWindowModeOn() {
  return Boolean(document.getElementById("props-filter-course-window")?.checked);
}

/** Field-by-course mode: current-week venue + inclusive date range (`from` and/or `to`). */
function propsCourseWindowModeActive() {
  if (!propsCourseWindowModeOn()) return false;
  if (!propsEffectiveCourseKey()) return false;
  const fromRaw = String(document.getElementById("props-filter-date-from")?.value || "").trim();
  const toRaw = String(document.getElementById("props-filter-date-to")?.value || "").trim();
  return Boolean(fromRaw || toRaw);
}

function propsCourseWindowDateInputToUtcMs(raw, endOfDay) {
  const s = String(raw || "").trim();
  const m = s.match(/^(\d{4})-(\d{2})-(\d{2})$/);
  if (!m) return NaN;
  const y = Number(m[1]);
  const mo = Number(m[2]);
  const d = Number(m[3]);
  if (!Number.isFinite(y) || !Number.isFinite(mo) || !Number.isFinite(d)) return NaN;
  return endOfDay ? Date.UTC(y, mo - 1, d, 23, 59, 59, 999) : Date.UTC(y, mo - 1, d);
}

function propsCourseWindowDateRangeLabel() {
  const fromRaw = String(document.getElementById("props-filter-date-from")?.value || "").trim();
  const toRaw = String(document.getElementById("props-filter-date-to")?.value || "").trim();
  if (!fromRaw && !toRaw) return "";
  const f = formatPropsCourseSessionDateLabel;
  if (fromRaw && toRaw && fromRaw === toRaw) return f(fromRaw);
  if (fromRaw && !toRaw) return `${f(fromRaw)} → …`;
  if (!fromRaw && toRaw) return `… → ${f(toRaw)}`;
  return `${f(fromRaw)} – ${f(toRaw)}`;
}

function historyRoundMatchesCourseKey(row, courseKey) {
  if (!row || !courseKey) return false;
  return normCourseNameKey(row.course_name) === courseKey;
}

function historyRoundInCourseDateWindow(row, fromMs, toMs) {
  const ms = historyRoundChartDateUtcMs(row);
  if (!Number.isFinite(ms)) return true;
  if (Number.isFinite(fromMs) && ms < fromMs) return false;
  if (Number.isFinite(toMs) && ms > toMs) return false;
  return true;
}

function applyPropsSidebarWeatherFiltersToRounds(list) {
  let out = filterHistoryRoundsByTempRange(list);
  const windBucket = selectedPropsWindRangeFilter();
  if (windBucket) {
    out = out.filter((r) =>
      weatherRangeMatch("wind", windBucket, parseWeatherNumber(r?.pga_meta_weather_wind_mph ?? r?.weather_wind_mph)),
    );
  }
  const humidityBucket = selectedPropsHumidityRangeFilter();
  if (humidityBucket) {
    out = out.filter((r) =>
      weatherRangeMatch(
        "humidity",
        humidityBucket,
        parseWeatherNumber(r?.pga_meta_weather_humidity ?? r?.weather_humidity),
      ),
    );
  }
  return out;
}

/** All player-round rows at one course inside the inclusive date window (Historical Trends field view). */
function collectCourseWindowRoundEntriesFixed(bucketOpt) {
  const courseKey = propsEffectiveCourseKey();
  if (!courseKey || !HISTORY?.byDgId) return [];
  const fromRaw = String(document.getElementById("props-filter-date-from")?.value || "").trim();
  const toRaw = String(document.getElementById("props-filter-date-to")?.value || "").trim();
  if (!fromRaw && !toRaw) return [];
  const sig = [
    historyMutationEpoch,
    courseKey,
    fromRaw,
    toRaw,
    propsTrendTempContextKey(),
    selectedPropsWindRangeFilter() || "",
    selectedPropsHumidityRangeFilter() || "",
  ].join("|");
  if (sig === courseWindowRoundEntriesCacheSig && courseWindowRoundEntriesCache) {
    return courseWindowRoundEntriesCache;
  }
  const bucket = bucketOpt || propsGetSingleCourseBucketSync(courseKey);
  if (!bucket) return [];
  let fromMs = propsCourseWindowDateInputToUtcMs(fromRaw, false);
  let toMs = propsCourseWindowDateInputToUtcMs(toRaw, true);
  if (Number.isFinite(fromMs) && Number.isFinite(toMs) && fromMs > toMs) {
    const swap = fromMs;
    fromMs = toMs;
    toMs = swap;
  }
  const raw = [];
  for (const e of bucket.entries) {
    if (!historyRoundInCourseDateWindow(e.row, fromMs, toMs)) continue;
    raw.push(e);
  }
  raw.sort((a, b) => historyRoundChronoKey(a.row) - historyRoundChronoKey(b.row));
  const rowsOnly = applyPropsSidebarWeatherFiltersToRounds(raw.map((e) => e.row));
  const rowSet = new Set(rowsOnly);
  const out = raw.filter((e) => rowSet.has(e.row));
  courseWindowRoundEntriesCacheSig = sig;
  courseWindowRoundEntriesCache = out;
  return out;
}

function propsCourseWindowEntriesForChart(winN) {
  const list = collectCourseWindowRoundEntriesFixed();
  if (propsCourseWindowModeActive()) return list;
  const wn = clamp(
    Math.round(num(winN, PROPS_HISTORY_ROUND_DEFAULT)),
    PROPS_HISTORY_ROUND_MIN,
    PROPS_HISTORY_ROUND_MAX,
  );
  if (list.length > wn) return list.slice(-wn);
  return list;
}

/** When the date window has more bars than we paint, sample evenly (not slice(-N) which drops early days). */
function sampleCourseWindowChartEntriesEvenly(entries, cap) {
  if (!entries?.length || entries.length <= cap) return entries || [];
  const out = [];
  const n = entries.length;
  for (let i = 0; i < cap; i++) {
    const idx = cap <= 1 ? 0 : Math.round((i * (n - 1)) / (cap - 1));
    out.push(entries[idx]);
  }
  return out;
}

function propsFullHitStatsFromRoundList(statKey, line, rounds) {
  if (!Number.isFinite(line)) return { valid: 0, over: 0, under: 0, overRate: NaN, underRate: NaN };
  let valid = 0;
  let over = 0;
  let under = 0;
  for (const r of rounds) {
    const a = actualForRoundRow(statKey, r);
    if (!Number.isFinite(a)) continue;
    valid++;
    if (a > line) over++;
    else if (a < line) under++;
  }
  return {
    valid,
    over,
    under,
    overRate: valid > 0 ? over / valid : NaN,
    underRate: valid > 0 ? under / valid : NaN,
  };
}

function propsCourseWindowFieldHitStats(statKey, line, winN) {
  const rounds = propsCourseWindowEntriesForChart(winN).map((e) => e.row);
  return propsFullHitStatsFromRoundList(statKey, line, rounds);
}

function refreshPropsCourseFilterOptionsAllPlayers() {
  const courseSel = document.getElementById("props-filter-course");
  if (!courseSel) return;
  const prev = courseSel.value;
  const windowOn = propsCourseWindowModeOn();
  const metaVenue = String(DATA?.meta?.course_used || DATA?.course_used || "").trim();
  const mkVenue = metaVenue ? normCourseNameKey(metaVenue) : "";
  const optsCacheKey = `${historyMutationEpoch}|${windowOn ? 1 : 0}|${mkVenue}`;
  let sortedEntries = propsAllPlayersCourseOptsEntries;
  if (propsAllPlayersCourseOptsCacheKey !== optsCacheKey || !sortedEntries) {
    if (windowOn) {
      sortedEntries = mkVenue ? [[mkVenue, courseFitPrettyCourseKey(mkVenue)]] : [];
    } else {
      rebuildPropsCourseRoundIndex();
      const byKey = new Map();
      for (const k of propsCourseRoundIndex.keys()) {
        byKey.set(k, courseFitPrettyCourseKey(k));
      }
      if (metaVenue) {
        if (mkVenue && !byKey.has(mkVenue)) byKey.set(mkVenue, courseFitPrettyCourseKey(mkVenue));
      }
      sortedEntries = [...byKey.entries()].sort((a, b) => a[0].localeCompare(b[0]));
    }
    propsAllPlayersCourseOptsCacheKey = optsCacheKey;
    propsAllPlayersCourseOptsEntries = sortedEntries;
  }
  courseSel.disabled = windowOn;
  if (windowOn) {
    const venueKey = propsEventVenueCourseKey();
    courseSel.innerHTML = venueKey
      ? `<option value="${venueKey}">${courseFitPrettyCourseKey(venueKey)}</option>`
      : '<option value="">Current course unknown (check projections)</option>';
    if (venueKey) courseSel.value = venueKey;
  } else {
    courseSel.innerHTML = '<option value="">All</option>';
    for (const [k, label] of sortedEntries) {
      const op = document.createElement("option");
      op.value = k;
      op.textContent = label;
      courseSel.appendChild(op);
    }
    const prevK = prev ? normCourseNameKey(prev) : "";
    if (prevK && [...courseSel.options].some((o) => o.value === prevK)) courseSel.value = prevK;
  }
}

function defaultLineForCourseWindow(statKey, entries) {
  const vals = entries
    .map((e) => actualForRoundRow(statKey, e.row))
    .filter((x) => Number.isFinite(x));
  if (vals.length) {
    const mean = vals.reduce((a, b) => a + b, 0) / vals.length;
    return clampPropLineForMarket(statKey, snapPropLineToDotFive(mean));
  }
  return clampPropLineForMarket(statKey, defaultPropLineForStat(statKey));
}

function paintPropsTrendKpiRowCourseWindow(statKey, hitSt, graphSeries, entries) {
  const el = document.getElementById("props-trends-kpis");
  if (!el) return;
  el.replaceChildren();
  const rounds = entries.map((e) => e.row);
  const playerIds = new Set(
    entries
      .filter((e) => e && Number.isFinite(num(e.dgId, NaN)) && String(e.playerName || "").trim())
      .map((e) => e.dgId),
  );
  const vals = (graphSeries || []).map((s) => s.actual).filter((x) => Number.isFinite(x));
  const graphMean = vals.length ? vals.reduce((a, b) => a + b, 0) / vals.length : NaN;
  const fieldMean = propsTrendMeanActual(statKey, rounds);

  const addKpi = (label, val, cls) => {
    const wrap = document.createElement("div");
    wrap.className = "props-trends-kpi";
    const lab = document.createElement("span");
    lab.className = "props-trends-kpi-lab";
    lab.textContent = label;
    const v = document.createElement("span");
    v.className = "props-trends-kpi-val" + (cls ? ` ${cls}` : "");
    v.textContent = formatPropsTrendKpiValue(statKey, val);
    wrap.appendChild(lab);
    wrap.appendChild(v);
    el.appendChild(wrap);
  };

  addKpi("Field avg (window)", fieldMean);
  addKpi("Graph avg", graphMean);
  addKpi("Rounds", rounds.length, "");
  addKpi("Players", playerIds.size, "");

  if (hitSt && hitSt.valid > 0) {
    const lowerBetter = propsStatLowerIsBetter(statKey);
    const addRateKpi = (label, rate, wins, total, side) => {
      const wrap = document.createElement("div");
      wrap.className = "props-trends-kpi";
      const lab = document.createElement("span");
      lab.className = "props-trends-kpi-lab";
      lab.textContent = label;
      const val = document.createElement("span");
      val.className = "props-trends-kpi-val";
      val.textContent = Number.isFinite(rate) ? `${(rate * 100).toFixed(1)}% (${wins}/${total})` : "—";
      const isUnderSide = side === "under";
      const greenSide = lowerBetter ? isUnderSide : !isUnderSide;
      val.classList.add(greenSide ? "ev-pos" : "ev-neg");
      wrap.appendChild(lab);
      wrap.appendChild(val);
      el.appendChild(wrap);
    };
    addRateKpi("Over hit rate", hitSt.overRate, hitSt.over, hitSt.valid, "over");
    addRateKpi("Under hit rate", hitSt.underRate, hitSt.under, hitSt.valid, "under");
  }
}

function parseWeatherNumber(v) {
  const n = num(v, NaN);
  if (Number.isFinite(n)) return n;
  const cleaned = String(v ?? "").replace(/[^0-9.-]+/g, "");
  const m = parseFloat(cleaned);
  return Number.isFinite(m) ? m : NaN;
}

function weatherRangeMatch(kind, bucket, value) {
  if (!bucket) return true;
  if (!Number.isFinite(value)) return false;
  if (kind === "temp") {
    if (bucket === "lt60") return value < 60;
    if (bucket === "60_69") return value >= 60 && value <= 69.999;
    if (bucket === "70_79") return value >= 70 && value <= 79.999;
    if (bucket === "80_89") return value >= 80 && value <= 89.999;
    if (bucket === "gte90") return value >= 90;
  }
  if (kind === "wind") {
    if (bucket === "0_5") return value >= 0 && value <= 5.999;
    if (bucket === "6_10") return value >= 6 && value <= 10.999;
    if (bucket === "11_15") return value >= 11 && value <= 15.999;
    if (bucket === "16_20") return value >= 16 && value <= 20.999;
    if (bucket === "gte21") return value >= 21;
  }
  if (kind === "humidity") {
    if (bucket === "lt40") return value < 40;
    if (bucket === "40_59") return value >= 40 && value <= 59.999;
    if (bucket === "60_79") return value >= 60 && value <= 79.999;
    if (bucket === "gte80") return value >= 80;
  }
  return true;
}

function propsConditionKeyFromRow(r) {
  const raw = String(
    r?.pga_meta_weather_condition ?? r?.weather_condition ?? r?.condition ?? ""
  )
    .trim()
    .toLowerCase();
  if (!raw) return "";
  if (raw.includes("storm") || raw.includes("thunder")) return "storm";
  if (raw.includes("rain") || raw.includes("shower") || raw.includes("drizzle")) return "rain";
  if (raw.includes("wind")) return "windy";
  if (raw.includes("cloud") || raw.includes("overcast")) return "cloudy";
  if (raw.includes("sun") || raw.includes("clear")) return "clear";
  return raw;
}

function propsConditionLabel(key) {
  if (!key) return "";
  if (key === "clear") return "Clear";
  if (key === "cloudy") return "Cloudy";
  if (key === "windy") return "Windy";
  if (key === "rain") return "Rain";
  if (key === "storm") return "Storm";
  return key
    .split(/[\s_]+/)
    .filter(Boolean)
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(" ");
}

function refreshPropsFilterOptionsForGolfer(dgId) {
  if (propsCourseWindowModeOn()) {
    refreshPropsCourseFilterOptionsAllPlayers();
    return;
  }
  const courseSel = document.getElementById("props-filter-course");
  if (!courseSel) return;
  const rounds = historyRoundsForDg(dgId).filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r));
  if (courseSel) {
    const prev = courseSel.value;
    /** @type {Map<string, string>} canonical key → pretty label */
    const byKey = new Map();
    for (const r of rounds) {
      const cn = String(r?.course_name || "").trim();
      if (!cn) continue;
      const k = normCourseNameKey(cn);
      if (!k) continue;
      if (!byKey.has(k)) byKey.set(k, courseFitPrettyCourseKey(k));
    }
    const metaVenue = String(DATA?.meta?.course_used || DATA?.course_used || "").trim();
    if (metaVenue) {
      const mk = normCourseNameKey(metaVenue);
      if (mk && !byKey.has(mk)) byKey.set(mk, courseFitPrettyCourseKey(mk));
    }
    courseSel.innerHTML = '<option value="">All</option>';
    [...byKey.entries()]
      .sort((a, b) => a[0].localeCompare(b[0]))
      .forEach(([k, label]) => {
        const op = document.createElement("option");
        op.value = k;
        op.textContent = label;
        courseSel.appendChild(op);
      });
    const prevK = prev ? normCourseNameKey(prev) : "";
    if (prevK && [...courseSel.options].some((o) => o.value === prevK)) courseSel.value = prevK;
  }
}

/** Birdies / pars / GIR / fairways: higher is better. Round score / bogeys / putts: higher is worse. */
function propsMarketHigherIsBetter(statKey) {
  return statKey === "birdies" || statKey === "pars" || statKey === "gir" || statKey === "fairways";
}

/** Round score / bogeys / putts: lower actual is better for O/U coloring (over line = red). */
function propsStatLowerIsBetter(statKey) {
  return !propsMarketHigherIsBetter(statKey);
}

/**
 * Field-by-course & date: order bars left-to-right — worst/highest first for score-like markets,
 * best/lowest first for counting markets where more is better (birdies, pars, fairways, GIR).
 */
function sortPropsFieldByCourseSeriesChart(statKey, series) {
  if (!series || series.length < 2) return;
  const hi = propsMarketHigherIsBetter(statKey);
  series.sort((a, b) => {
    const va = num(a.actual, NaN);
    const vb = num(b.actual, NaN);
    const fa = Number.isFinite(va);
    const fb = Number.isFinite(vb);
    if (fa && fb && va !== vb) return hi ? va - vb : vb - va;
    if (fa !== fb) return fa ? -1 : 1;
    const pa = String(a.playerName || "").localeCompare(String(b.playerName || ""));
    if (pa !== 0) return pa;
    return String(a.date || "").localeCompare(String(b.date || ""));
  });
}

/**
 * Placeholder / bad rows: all counting markets are 0 and no real total — drop from chart & hit stats.
 */
function historyRoundIsPlaceholderAllMarketsZero(row) {
  if (!row || typeof row !== "object") return true;
  const b = num(row.birdies, 0);
  const p = num(row.pars, 0);
  const bg = num(row.bogies ?? row.bogeys, 0);
  const g = num(row.gir, 0);
  const f = num(row.fairways, 0);
  const pt = num(row.putts, 0);
  const t = num(row.round_score, NaN);
  const countsAllZero = b === 0 && p === 0 && bg === 0 && g === 0 && f === 0 && pt === 0;
  const noRealTotal = !Number.isFinite(t) || t <= 0;
  return countsAllZero && noRealTotal;
}

/**
 * Historical Trends / hit-rate: only rounds that have actually been completed (not scheduled future days,
 * not in-progress live projections, not round_num ahead of the live tournament).
 */
function historyRoundCountsAsActual(row) {
  if (!row || typeof row !== "object") return false;
  if (!historyRowFromDgHistoricalRoundsApi(row)) return false;
  if (historyRoundIsPlaceholderAllMarketsZero(row)) return false;
  if (historyDateMdYIsFuture(row.event_completed)) return false;
  if (historyRoundChartDateIsFuture(row)) return false;

  if (row._from_live_tournament_stats) {
    const rs = num(row.round_score, NaN);
    if (!Number.isFinite(rs) || rs <= 0) return false;
  }

  if (historyRoundMatchesCurrentEvent(row)) {
    const rnd = Math.round(num(row.round_num, NaN));
    const rs = num(row.round_score, NaN);
    const liveGrossLocked =
      (row._from_live_tournament_stats || row._from_pgatour) && Number.isFinite(rs) && rs > 0;
    if (!liveGrossLocked) {
      const cap = currentTournamentProgressRoundCap();
      if (Number.isFinite(rnd) && Number.isFinite(cap) && rnd > cap) return false;
    }
  }
  return true;
}

function scrubNonActualRoundsFromHistoryBuckets() {
  if (!HISTORY?.byDgId || typeof HISTORY.byDgId !== "object") return 0;
  let removed = 0;
  for (const bucket of Object.values(HISTORY.byDgId)) {
    if (!bucket || !Array.isArray(bucket.rounds)) continue;
    const before = bucket.rounds.length;
    bucket.rounds = bucket.rounds.filter((r) => historyRoundCountsAsActual(r));
    removed += before - bucket.rounds.length;
  }
  if (removed > 0) {
    HISTORY_ROUNDS_CHRONO_CACHE.clear();
    PRICING_MU_BONUS_CACHE.clear();
    bumpHistoryMutationEpoch();
  }
  return removed;
}

/** Loose match for schedule titles (e.g. “THE MASTERS” vs “Masters Tournament”). */
function scheduleNameMatchesMeta(histNameRaw, metaNameRaw) {
  const meta = String(metaNameRaw || "").trim();
  const hist = String(histNameRaw || "").trim();
  if (!meta || !hist) return false;
  if (courseNameMatchesVenue(hist, meta)) return true;
  const strip = (s) =>
    s
      .toLowerCase()
      .replace(/\b(the|pga|liv\s*golf|dp\s*world)\b/g, " ")
      .replace(/\b(championship|tournament|invitational|classic|open)\b/g, " ")
      .replace(/[^a-z0-9]+/g, " ")
      .replace(/\s+/g, " ")
      .trim();
  const h = strip(hist);
  const m = strip(meta);
  if (!h || !m) return false;
  if (h.includes(m) || m.includes(h)) return true;
  const tokens = (s) => s.split(" ").filter((t) => t.length >= 4);
  const ht = tokens(h);
  const mt = tokens(m);
  for (const t of mt) {
    if (ht.some((x) => x.includes(t) || t.includes(x))) return true;
  }
  return false;
}

/**
 * Extra course matching for API vs CSV naming (same idea as hangoutHistoryPriorThree):
 * short venue strings from DataGolf vs longer course_name in history.
 */
function courseNameMatchesVenueLoose(courseNameRaw, venueRaw) {
  if (courseNameMatchesVenue(courseNameRaw, venueRaw)) return true;
  const c = String(courseNameRaw || "").trim().toLowerCase();
  const needle = String(venueRaw || "").trim().toLowerCase();
  if (!c || !needle) return false;
  const headC = Math.min(10, c.length);
  const headN = Math.min(10, needle.length);
  if (headC >= 3 && needle.includes(c.slice(0, headC))) return true;
  if (headN >= 3 && c.includes(needle.slice(0, headN))) return true;
  return false;
}

/**
 * Known duplicate venue labels in feeds/history (after internal normalization) → one canonical key.
 * Keys must match the post-normalization lowercase string from `normCourseNameKey` pipeline.
 */
const COURSE_NAME_CANONICAL_KEYS = Object.freeze({
  albany: "albany golf club",
  "albany bahamas": "albany golf club",
  "sea island resort": "sea island golf club",
});

/** Course-name canonical key for filters/matching (e.g. "Trump National Doral" vs "(Blue Monster)"). */
function normCourseNameKey(raw) {
  let s = String(raw || "").trim().toLowerCase();
  s = s.replace(/\([^)]*\)/g, " ");
  s = s.replace(/\b(blue monster|stadium course|championship course|club de golf)\b/g, " ");
  s = s.replace(/&/g, " and ");
  /* Venue / feed variants (TPC Sawgrass "The Players", Harbour Town Gl vs Golf Links, etc.). */
  s = s.replace(/\bthe players\b/gi, " ");
  /* Unify "… Gc" / "… Cc" / "… Gl" / dotted forms (DataGolf vs CSV vs history). */
  s = s.replace(/\bc\.?\s*c\.?\b/gi, "country club");
  s = s.replace(/\bg\.?\s*c\.?\b/gi, "golf club");
  s = s.replace(/\bg\.?\s*l\.?\b/gi, "golf links");
  s = s.replace(/\bgolf club(\s+golf club)+\b/gi, "golf club");
  s = s.replace(/\bcountry club(\s+country club)+\b/gi, "country club");
  s = s.replace(/\bgolf links(\s+golf links)+\b/gi, "golf links");
  s = s.replace(/[^a-z0-9]+/g, " ");
  s = s.replace(/\s+/g, " ").trim();
  const alias = COURSE_NAME_CANONICAL_KEYS[s];
  return alias || s;
}

/** Schedule title match + prefix / normalized fallbacks (sponsor-heavy titles vs short CSV names). */
function eventNameMatchesCurrentSchedule(histNameRaw, metaNameRaw) {
  if (scheduleNameMatchesMeta(histNameRaw, metaNameRaw)) return true;
  const en = String(histNameRaw || "").trim().toLowerCase();
  const evN = String(metaNameRaw || "").trim().toLowerCase();
  if (!en || !evN) return false;
  if (en.includes(evN.slice(0, 14)) || evN.includes(en.slice(0, 10))) return true;
  const a = normEvtNameKey(histNameRaw);
  const b = normEvtNameKey(metaNameRaw);
  if (a && b && (a === b || a.includes(b) || b.includes(a))) return true;
  return false;
}

/** Current tournament context match for history rows.
 * If venue is known, require course match for "Current course only".
 * Event-name fallback is used only when venue is unavailable.
 */
function currentTournamentContextMatchesRound(r) {
  const vn = venueCourseName();
  const metaEvent = String(DATA.meta.event_name || "").trim();
  if (vn) return courseNameMatchesVenueLoose(r.course_name, vn);
  if (metaEvent) return eventNameMatchesCurrentSchedule(r.event_name, metaEvent);
  return false;
}

function filteredHistoryRoundsMemoSig() {
  return [
    historyMutationEpoch,
    courseFilterOn() ? 1 : 0,
    venueCourseName(),
    String(DATA?.meta?.event_name || ""),
    propsCourseWindowModeActive() ? propsEffectiveCourseKey() || "" : "",
    propsTrendTempContextKey(),
    selectedPropsWindRangeFilter() || "",
    selectedPropsHumidityRangeFilter() || "",
  ].join("\x1f");
}

function filteredHistoryRounds(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return [];
  const ms = filteredHistoryRoundsMemoSig();
  if (ms !== filteredHistoryRoundsMemoSigStored) {
    filteredHistoryRoundsMemoSigStored = ms;
    filteredHistoryRoundsMemoByDgId.clear();
  }
  const memoHit = filteredHistoryRoundsMemoByDgId.get(id);
  if (memoHit) return memoHit;

  let list = historyRoundsForDg(id);
  if (courseFilterOn()) {
    const vn = venueCourseName();
    const metaEvent = String(DATA.meta.event_name || "").trim();
    if (vn || metaEvent) {
      list = list.filter((r) => currentTournamentContextMatchesRound(r));
    }
  }
  const courseFilter = propsCourseWindowModeActive() ? propsEffectiveCourseKey() : "";
  if (courseFilter) {
    list = list.filter((r) => normCourseNameKey(r.course_name) === normCourseNameKey(courseFilter));
  }
  list = filterHistoryRoundsByTempRange(list);
  const windBucket = selectedPropsWindRangeFilter();
  if (windBucket) {
    list = list.filter((r) => weatherRangeMatch("wind", windBucket, parseWeatherNumber(r?.pga_meta_weather_wind_mph ?? r?.weather_wind_mph)));
  }
  const humidityBucket = selectedPropsHumidityRangeFilter();
  if (humidityBucket) {
    list = list.filter((r) =>
      weatherRangeMatch("humidity", humidityBucket, parseWeatherNumber(r?.pga_meta_weather_humidity ?? r?.weather_humidity))
    );
  }
  list = list.filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r));
  /* Current-course filter can yield zero rows (rookies, first-time venue) — fall back only when venue is unknown. */
  if (courseFilterOn() && !list.length) {
    const vn = venueCourseName();
    if (!vn) {
      list = historyRoundsForDg(dgId).filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r));
      if (courseFilter) {
        list = list.filter((r) => normCourseNameKey(r.course_name) === normCourseNameKey(courseFilter));
      }
      list = filterHistoryRoundsByTempRange(list);
      if (windBucket) {
        list = list.filter((r) => weatherRangeMatch("wind", windBucket, parseWeatherNumber(r?.pga_meta_weather_wind_mph ?? r?.weather_wind_mph)));
      }
      if (humidityBucket) {
        list = list.filter((r) =>
          weatherRangeMatch("humidity", humidityBucket, parseWeatherNumber(r?.pga_meta_weather_humidity ?? r?.weather_humidity))
        );
      }
      list.sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a));
      list = list.slice(0, 60);
    }
  }
  list.sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a));
  filteredHistoryRoundsMemoByDgId.set(id, list);
  return list;
}

/** Newest-first rounds for chart + hit stats. “Current course only” uses every matching round (no Rounds cap). */
function propsFilteredRoundsNewestFirst(dgId, winN) {
  const list = filteredHistoryRounds(dgId);
  if (courseFilterOn()) return list;
  const wn = clamp(
    Math.round(num(winN, PROPS_HISTORY_ROUND_DEFAULT)),
    PROPS_HISTORY_ROUND_MIN,
    PROPS_HISTORY_ROUND_MAX
  );
  return list.slice(0, wn);
}

function historyRoundsChronoNewestFirst(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (Number.isFinite(id)) {
    const cached = HISTORY_ROUNDS_CHRONO_CACHE.get(id);
    if (cached) return cached;
  }
  const list = historyRoundsForDg(dgId).filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r));
  const sorted = list.sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a));
  if (Number.isFinite(id)) HISTORY_ROUNDS_CHRONO_CACHE.set(id, sorted);
  return sorted;
}

function meanNumFromRounds(rounds, key) {
  const vals = [];
  for (const r of rounds) {
    const v = num(r[key], NaN);
    if (Number.isFinite(v)) vals.push(v);
  }
  if (!vals.length) return NaN;
  return vals.reduce((a, b) => a + b, 0) / vals.length;
}

/** Map pricing UI skill value to projection / history SG column (e.g. `sg_putt`). */
function pricingSkillColumnKeyFromRaw(skillRaw) {
  const skRaw = String(skillRaw || "sg_total").toLowerCase();
  return skRaw === "default" ? "sg_total" : PRICING_SKILL_COLUMNS.includes(skRaw) ? skRaw : "sg_total";
}

/**
 * Skill-focus fallback when round history is short or lacks the chosen pillar: player SG vs field median
 * on the active model round so +EV Model odds respond to the Skill dropdown (Outright Win, matchups, O/U).
 */
function projectionSkillFocusNudgeFromField(dgId, skillKey) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id) || !PRICING_SKILL_COLUMNS.includes(skillKey)) return 0;
  const prRound = getModelRoundForEv();
  const row = projectionPlayerRowForModel(id, prRound);
  if (!row) return 0;
  const v = num(row[skillKey], NaN);
  if (!Number.isFinite(v)) return 0;
  const vals = [];
  for (const p of DATA.players || []) {
    const pid = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(pid)) continue;
    const pr = projectionPlayerRowForModel(pid, prRound);
    if (!pr) continue;
    const x = num(pr[skillKey], NaN);
    if (Number.isFinite(x)) vals.push(x);
  }
  if (vals.length < 8) return 0;
  vals.sort((a, b) => a - b);
  const mid = Math.floor(vals.length / 2);
  const median = vals.length % 2 === 1 ? vals[mid] : (vals[mid - 1] + vals[mid]) / 2;
  const delta = v - median;
  return clamp(delta * 0.32, -0.35, 0.35);
}

function courseNameMatchesVenue(courseNameRaw, venueRaw) {
  const c = String(courseNameRaw || "").trim().toLowerCase();
  const v = String(venueRaw || "").trim().toLowerCase();
  if (!c || !v) return false;
  if (c.includes(v) || v.includes(c)) return true;
  const ck = normCourseNameKey(c);
  const vk = normCourseNameKey(v);
  if (!ck || !vk) return false;
  return ck.includes(vk) || vk.includes(ck);
}

function pricingModeMuSgBonus(dgId) {
  return pricingModeMuSgBonusForMode(dgId, PRICING_STATE.mode, PRICING_STATE.skill);
}

function pricingModeMuSgBonusForMode(dgId, modeRaw, skillRaw = PRICING_STATE.skill) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return 0;
  const mode = ["default", "recent", "course", "skill"].includes(String(modeRaw || "").toLowerCase())
    ? String(modeRaw || "").toLowerCase()
    : "default";
  const skillKey = String(skillRaw || "default").toLowerCase();
  const venueKey = mode === "course" ? normCourseNameKey(venueCourseName()) : "";
  const cacheKey = `${id}|${mode}|${skillKey}|${venueKey}`;
  if (PRICING_MU_BONUS_CACHE.has(cacheKey)) return PRICING_MU_BONUS_CACHE.get(cacheKey) || 0;
  if (mode === "default") {
    const recent = pricingModeMuSgBonusForMode(id, "recent", skillRaw);
    const course = pricingModeMuSgBonusForMode(id, "course", skillRaw);
    const skill = pricingModeMuSgBonusForMode(id, "skill", skillRaw);
    // Blended default: combine all modes, but keep magnitude below specialized modes.
    const out = clamp(0.4 * recent + 0.25 * course + 0.35 * skill, -0.28, 0.28);
    PRICING_MU_BONUS_CACHE.set(cacheKey, out);
    return out;
  }

  const rounds = historyRoundsChronoNewestFirst(id);
  if (rounds.length < 4) {
    if (mode === "skill") {
      const sk0 = pricingSkillColumnKeyFromRaw(skillRaw);
      const fb0 = projectionSkillFocusNudgeFromField(id, sk0);
      PRICING_MU_BONUS_CACHE.set(cacheKey, fb0);
      return fb0;
    }
    PRICING_MU_BONUS_CACHE.set(cacheKey, 0);
    return 0;
  }

  if (mode === "recent") {
    const nRec = Math.min(6, Math.max(3, Math.floor(rounds.length / 2)));
    const recent = rounds.slice(0, nRec);
    const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 18));
    let rMean = meanNumFromRounds(recent, "sg_total");
    let oMean = meanNumFromRounds(older, "sg_total");
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      const out = clamp((rMean - oMean) * 0.9, -0.35, 0.35);
      PRICING_MU_BONUS_CACHE.set(cacheKey, out);
      return out;
    }
    rMean = meanNumFromRounds(recent, "round_score");
    oMean = meanNumFromRounds(older, "round_score");
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      const out = clamp(((oMean - rMean) / 6) * 0.85, -0.35, 0.35);
      PRICING_MU_BONUS_CACHE.set(cacheKey, out);
      return out;
    }
    PRICING_MU_BONUS_CACHE.set(cacheKey, 0);
    return 0;
  }

  if (mode === "course") {
    const vn = venueCourseName();
    if (!vn) {
      PRICING_MU_BONUS_CACHE.set(cacheKey, 0);
      return 0;
    }
    const here = rounds.filter((r) => courseNameMatchesVenue(r.course_name, vn));
    if (here.length < 2) {
      PRICING_MU_BONUS_CACHE.set(cacheKey, 0);
      return 0;
    }
    const other = rounds.filter((r) => !courseNameMatchesVenue(r.course_name, vn));
    const hMean = meanNumFromRounds(here, "sg_total");
    const oMean = meanNumFromRounds(other.length ? other : rounds, "sg_total");
    if (Number.isFinite(hMean) && Number.isFinite(oMean)) {
      const out = clamp((hMean - oMean) * 0.75, -0.35, 0.35);
      PRICING_MU_BONUS_CACHE.set(cacheKey, out);
      return out;
    }
    const hSc = meanNumFromRounds(here, "round_score");
    const oSc = meanNumFromRounds(other.length ? other : rounds, "round_score");
    if (Number.isFinite(hSc) && Number.isFinite(oSc)) {
      const out = clamp(((oSc - hSc) / 6) * 0.7, -0.35, 0.35);
      PRICING_MU_BONUS_CACHE.set(cacheKey, out);
      return out;
    }
    PRICING_MU_BONUS_CACHE.set(cacheKey, 0);
    return 0;
  }

  if (mode === "skill") {
    const sk = pricingSkillColumnKeyFromRaw(skillRaw);
    const nRec = Math.min(8, Math.max(3, Math.floor(rounds.length / 2)));
    const recent = rounds.slice(0, nRec);
    const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 24));
    const rMean = meanNumFromRounds(recent, sk);
    const oMean = meanNumFromRounds(older, sk);
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      const out = clamp((rMean - oMean) * 0.75, -0.35, 0.35);
      PRICING_MU_BONUS_CACHE.set(cacheKey, out);
      return out;
    }
    const fb = projectionSkillFocusNudgeFromField(id, sk);
    PRICING_MU_BONUS_CACHE.set(cacheKey, fb);
    return fb;
  }

  PRICING_MU_BONUS_CACHE.set(cacheKey, 0);
  return 0;
}

/** Live thru/today → SG delta for matchups; skipped for round_matchups / 3_balls unless `in_play_affects_round_odds`. */
function liveMuSgDeltaForMatchupRow(row, matchupMarketKind) {
  if (!inPlayAffectsRoundOdds()) return 0;
  if (matchupMarketKind === "round_matchups" || matchupMarketKind === "3_balls") return 0;
  return liveCurrentRoundMuSgDelta(row);
}

function effectiveMuSg(row, dgIdOpt, matchupMarketKind) {
  const base = weatherAdjustedMuSg(row);
  const id = Number.isFinite(dgIdOpt) ? Math.round(dgIdOpt) : Math.round(num(row?.dg_id, NaN));
  if (!Number.isFinite(base) || !Number.isFinite(id)) return base;
  return (
    base +
    pricingModeMuSgBonus(id) +
    priorRoundCourseMuSgDelta(row) +
    liveMuSgDeltaForMatchupRow(row, matchupMarketKind)
  );
}

function pricingStatMuAdjustment(market, dgId) {
  const b = pricingModeMuSgBonus(dgId);
  if (!Number.isFinite(b) || b === 0) return 0;
  if (market === "Total score") return -1.05 * b;
  if (market === "Bogeys") return -0.45 * b;
  if (market === "Birdies") return 0.5 * b;
  if (market === "Pars") return 0.08 * b;
  if (market === "GIR") return 0.35 * b;
  if (market === "Fairways hit") return 0.22 * b;
  if (market === "Putts") return -0.32 * b;
  return 0;
}

function pricingModelHistoryNudge(statKey, dgId) {
  const b = pricingModeMuSgBonus(dgId);
  if (!Number.isFinite(b) || b === 0) return 0;
  if (statKey === "total") return -1.02 * b;
  if (statKey === "bogeys") return -0.42 * b;
  if (statKey === "birdies") return 0.48 * b;
  if (statKey === "gir") return 0.38 * b;
  if (statKey === "fairways") return 0.24 * b;
  if (statKey === "pars") return 0.06 * b;
  if (statKey === "putts") return -0.3 * b;
  return 0;
}

function propsTrendLineContextKeyFromDom() {
  const dg = selectedDgId();
  const sk = statKeyFromPropSelect();
  const winN = clamp(
    Math.round(num(document.getElementById("props-window-n")?.value, PROPS_HISTORY_ROUND_DEFAULT)),
    PROPS_HISTORY_ROUND_MIN,
    PROPS_HISTORY_ROUND_MAX
  );
  const winNKey = courseFilterOn() ? "all" : String(winN);
  const temp = propsTrendTempContextKey();
  const wind = selectedPropsWindRangeFilter() || "all";
  const hum = selectedPropsHumidityRangeFilter() || "all";
  const course = propsCourseWindowModeActive() ? propsEffectiveCourseKey() || "all" : "all";
  const pm = PRICING_STATE.mode || "default";
  const ps = PRICING_STATE.skill === "default" ? "default" : pricingSkillHistoryKey();
  const cw = propsCourseWindowModeActive() ? 1 : 0;
  const winDates = cw
    ? `${String(document.getElementById("props-filter-date-from")?.value || "")}|${String(document.getElementById("props-filter-date-to")?.value || "")}`
    : "";
  return `${dg}|${sk}|${courseFilterOn() ? 1 : 0}|${winNKey}|${temp}|${wind}|${hum}|${course}|${pm}|${ps}|${cw}|${winDates}`;
}

/** After user changes line or steppers so projection logic does not overwrite the input. */
function lockPropsTrendLineContextToCurrentFilter() {
  propsTrendsLineContextKey = propsTrendLineContextKeyFromDom();
}

/**
 * Min rounds to list a player in the trends table. Full-field default is high for stability;
 * any narrow filter (this event’s course, field-by-course window + venue, or weather buckets)
 * uses 1 so you still see the whole field when sample sizes are small per player.
 */
function propsTopHitMinRoundsForFilter() {
  if (propsCourseWindowModeActive()) return 1;
  if (courseFilterOn()) return 1;
  if (propsTempFilterActive()) return 1;
  if (selectedPropsWindRangeFilter()) return 1;
  if (selectedPropsHumidityRangeFilter()) return 1;
  return PROPS_TOP_HIT_MIN_ROUNDS;
}

function propsPlayerMeetsFireSide(statKey, row) {
  if (!row || !row.valid) return false;
  const hi = propsMarketHigherIsBetter(statKey);
  if (hi) return row.overRate >= 0.5;
  return row.underRate >= 0.5;
}

function propsPlayerMeetsIceSide(statKey, row) {
  if (!row || !row.valid) return false;
  const hi = propsMarketHigherIsBetter(statKey);
  if (hi) return row.underRate >= 0.5;
  return row.overRate >= 0.5;
}

/**
 * History JSON uses `null` for missing stats; `Number(null) === 0` would fake zeros on charts / hit rates.
 * GIR / fairways from CSV are sometimes fractions in (0,1] (share of holes) — convert to counts like build-player-history.
 */
function historyScalarOrNaN(v) {
  if (v == null || v === "") return NaN;
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

/**
 * GIR / fairways: values in (0, 1] are share-of-holes (e.g. 0.72 → integer hole counts); values in (1, holes]
 * are expected counts from history or projections — keep fractional scale (do not `Math.round` to whole holes;
 * that turned 9.91 fairways / 11.88 GIR into 10 / 12 for O-U means).
 */
function girFairwaysCountFromRawForOu(v, holes) {
  const n = historyScalarOrNaN(v);
  if (!Number.isFinite(n)) return NaN;
  if (n > 0 && n <= 1.0001) return Math.min(holes, Math.max(0, Math.round(n * holes)));
  if (n > 1.0001 && n <= holes + 1e-6) return Math.min(holes, Math.max(0, n));
  return Math.min(holes, Math.max(0, Math.round(n)));
}

function historyGirOrFairwaysCount(v, holes) {
  return girFairwaysCountFromRawForOu(v, holes);
}

/** Fallback μ when `ouMeanCountingStat` is NaN — must not use `num(null)` (Number(null) is 0). */
function ouFallbackScalarForProjectedMean(mKey, row, rec) {
  const v = historyScalarOrNaN(row?.[rec.field]);
  if (mKey === "GIR") return girFairwaysCountFromRawForOu(v, 18);
  if (mKey === "Fairways hit") return girFairwaysCountFromRawForOu(v, fairwayHolesModeledFromData());
  return v;
}

/** Mean for O/U +EV: GIR / fairways as hole counts when projections store rates. */
function ouMeanCountingStat(market, row) {
  const mKey = ouModelMarketKey(market) || "Total score";
  const rec = ouStatRec(mKey);
  const raw = historyScalarOrNaN(row?.[rec.field]);
  if (!Number.isFinite(raw)) return NaN;
  if (mKey === "GIR") return girFairwaysCountFromRawForOu(raw, 18);
  if (mKey === "Fairways hit") return girFairwaysCountFromRawForOu(raw, fairwayHolesModeledFromData());
  return raw;
}

function actualForRoundRow(statKey, row) {
  if (!row || typeof row !== "object") return NaN;
  if (statKey === "total" || statKey === "birdies" || statKey === "pars" || statKey === "bogeys") {
    if (!historyRowFromDgHistoricalRoundsApi(row)) return NaN;
    if (statKey === "total") return historyScalarOrNaN(row.round_score);
    if (!historyLiveCountingTrusted(row)) return NaN;
    if (statKey === "birdies") return historyScalarOrNaN(row.birdies);
    if (statKey === "pars") return historyScalarOrNaN(row.pars);
    return historyScalarOrNaN(row.bogies ?? row.bogeys);
  }
  if (statKey === "gir") {
    const scrubbed = scrubLivePlaceholderCountingOnRow(row);
    const v = historyGirOrFairwaysCount(scrubbed.gir, 18);
    if (v === 0 || v === 1) return NaN;
    return v;
  }
  if (statKey === "fairways") {
    const v = historyGirOrFairwaysCount(row.fairways, fairwayHolesModeledFromData());
    return v === 0 || v === 1 ? NaN : v;
  }
  if (statKey === "putts") {
    const v = historyScalarOrNaN(row.putts);
    return v === 0 || v === 1 ? NaN : v;
  }
  return NaN;
}

/** Lines are only *.5 (never whole numbers): … 69.5, 70.5, 71.5 … */
function snapPropLineToDotFive(x) {
  const v = num(x, NaN);
  if (!Number.isFinite(v)) return NaN;
  return Math.round(v - 0.5) + 0.5;
}

function formatPropLineValueForInput(line) {
  const s = snapPropLineToDotFive(line);
  if (!Number.isFinite(s)) return "";
  return s.toFixed(1);
}

function formatPropLineChartLabel(statKey, line) {
  const s = clampPropLineForMarket(statKey, line);
  if (!Number.isFinite(s)) return "";
  return s.toFixed(1);
}

function clampPropLineForMarket(statKey, line) {
  const s = snapPropLineToDotFive(line);
  if (!Number.isFinite(s)) return NaN;
  if (statKey === "total") return clamp(s, 50.5, 99.5);
  if (statKey === "gir") return clamp(s, 4.5, 16.5);
  if (statKey === "fairways") {
    const nFw = fairwayHolesModeledFromData();
    const hi = Number.isFinite(nFw) && nFw >= 1 ? nFw - 0.5 : 13.5;
    return clamp(s, 0.5, hi);
  }
  if (statKey === "putts") return clamp(s, 22.5, 36.5);
  return clamp(s, 0.5, 29.5);
}

/** Default O/U line when projections or inputs do not supply one (prefer venue historical means). */
function defaultPropLineForStat(statKey) {
  const b = DATA?.meta?.projection_course_basis;
  if (b && typeof b === "object") {
    if (statKey === "total" && Number.isFinite(num(b.venue_avg_round_score, NaN))) {
      return enforceHalfLine(num(b.venue_avg_round_score, NaN));
    }
    if (statKey === "birdies" && Number.isFinite(num(b.venue_avg_birdies, NaN))) {
      return enforceHalfLine(num(b.venue_avg_birdies, NaN));
    }
    if (statKey === "pars" && Number.isFinite(num(b.venue_avg_pars, NaN))) {
      return enforceHalfLine(num(b.venue_avg_pars, NaN));
    }
    if (statKey === "bogeys" && Number.isFinite(num(b.venue_avg_bogeys, NaN))) {
      return enforceHalfLine(num(b.venue_avg_bogeys, NaN));
    }
    if (statKey === "gir" && Number.isFinite(num(b.venue_avg_gir, NaN))) {
      return enforceHalfLine(num(b.venue_avg_gir, NaN));
    }
    if (statKey === "fairways" && Number.isFinite(num(b.venue_avg_fairways, NaN))) {
      return enforceHalfLine(num(b.venue_avg_fairways, NaN));
    }
    if (statKey === "putts" && Number.isFinite(num(b.venue_avg_putts, NaN))) {
      return enforceHalfLine(num(b.venue_avg_putts, NaN));
    }
  }
  if (statKey === "total") return 70.5;
  if (statKey === "gir") return 11.5;
  if (statKey === "fairways") return 8.5;
  if (statKey === "putts") return 29.5;
  return 3.5;
}

/** Label for prop-stat / history market (toolbar + hit-rate copy). */
function propMarketLabelFromKey(statKey) {
  if (statKey === "total") return "Round score";
  if (statKey === "birdies") return "Birdies";
  if (statKey === "pars") return "Pars";
  if (statKey === "bogeys") return "Bogeys";
  if (statKey === "gir") return "GIR";
  if (statKey === "fairways") return "Fairways hit";
  if (statKey === "putts") return "Putts";
  return String(statKey || "");
}

/**
 * Over / under vs line (strict; pushes excluded from both counts).
 * Selected golfer: same window as chart (`propsFilteredRoundsNewestFirst`).
 * Leaderboard table: all rounds matching filters (ignore Rounds stepper).
 */
function propsFullHitStatsForDg(dgId, statKey, line, winN, forLeaderboardTable = false) {
  if (!Number.isFinite(line)) return { valid: 0, over: 0, under: 0, overRate: NaN, underRate: NaN };
  const newestFirst = forLeaderboardTable
    ? filteredHistoryRounds(dgId)
    : propsFilteredRoundsNewestFirst(dgId, winN);
  let valid = 0;
  let over = 0;
  let under = 0;
  for (const r of newestFirst) {
    const a = actualForRoundRow(statKey, r);
    if (!Number.isFinite(a)) continue;
    valid++;
    if (a > line) over++;
    else if (a < line) under++;
  }
  return {
    valid,
    over,
    under,
    overRate: valid > 0 ? over / valid : NaN,
    underRate: valid > 0 ? under / valid : NaN,
  };
}

function ensurePropsDgIdNameManifestLoaded() {
  if (propsDgIdNameById) return Promise.resolve(propsDgIdNameById);
  if (propsDgIdNameManifestPromise) return propsDgIdNameManifestPromise;
  propsDgIdNameManifestPromise = (async () => {
    if (isFileProtocol()) {
      propsDgIdNameById = new Map();
      return propsDgIdNameById;
    }
    try {
      const res = await fetch(cacheBustFetchUrl("player-history/manifest.json"), { cache: "no-store" });
      if (res.ok) {
        const j = await res.json();
        const m = new Map();
        for (const p of j.players || []) {
          const id = Math.round(num(p.dg_id, NaN));
          const nm = normalizeGolferDisplayName(p.player_name);
          if (Number.isFinite(id) && nm) m.set(id, nm);
        }
        propsDgIdNameById = m;
        return m;
      }
    } catch (_) {
      /* manifest optional */
    }
    propsDgIdNameById = new Map();
    return propsDgIdNameById;
  })().finally(() => {
    propsDgIdNameManifestPromise = null;
  });
  return propsDgIdNameManifestPromise;
}

function golferDisplayNameFromHistoryBucket(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id) || !HISTORY?.byDgId) return "";
  const rec = HISTORY.byDgId[String(id)];
  if (!rec) return "";
  const bucketNm = normalizeGolferDisplayName(rec.player_name);
  if (bucketNm) return bucketNm;
  const rounds = Array.isArray(rec.rounds) ? rec.rounds : [];
  for (let i = rounds.length - 1; i >= 0; i--) {
    const rn = normalizeGolferDisplayName(rounds[i]?.player_name);
    if (rn) return rn;
  }
  return "";
}

/**
 * Resolve a golfer label for Historical Trends / field-by-course (never `DG {id}`).
 * @param {number} dgId
 * @param {string} [hintName] — shard row / entry hint
 * @param {Map<number, string>} [nameByDgOpt]
 */
function resolveGolferDisplayNameForDg(dgId, hintName = "", nameByDgOpt = null) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return "";
  const hint = normalizeGolferDisplayName(hintName);
  if (hint) return hint;
  const map = nameByDgOpt || buildPropsGolferDisplayNameMap();
  const fromMap = map.get(id);
  if (fromMap) return fromMap;
  const fromHist = golferDisplayNameFromHistoryBucket(id);
  if (fromHist) return fromHist;
  if (propsDgIdNameById?.has(id)) return propsDgIdNameById.get(id) || "";
  return "";
}

function propsPlayerDisplayNameForDg(dgId, hintName = "") {
  return resolveGolferDisplayNameForDg(dgId, hintName) || "—";
}

/** O(players + history + manifest) lookup for leaderboard rows. */
function buildPropsGolferDisplayNameMap() {
  const m = new Map();
  const players = DATA.players || [];
  for (const p of players) {
    const pid = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(pid)) continue;
    const nm = normalizeGolferDisplayName(p.player_name);
    if (nm) m.set(pid, nm);
  }
  if (HISTORY?.byDgId) {
    for (const [dgStr, rec] of Object.entries(HISTORY.byDgId)) {
      const pid = Math.round(num(dgStr, NaN));
      if (!Number.isFinite(pid) || m.has(pid)) continue;
      const nm = golferDisplayNameFromHistoryBucket(pid);
      if (nm) m.set(pid, nm);
    }
  }
  if (propsDgIdNameById?.size) {
    for (const [pid, nm] of propsDgIdNameById) {
      if (!m.has(pid) && nm) m.set(pid, nm);
    }
  }
  return m;
}

/** Max rows that could count toward O/U (finite actual on this market); filters only remove rounds. Used to prune leaderboard work. */
function propsMaxFiniteMarketRoundsUpperBound(dgId, statKey, minRoundsNeeded = 1) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return 0;
  const need = Math.max(1, Math.round(num(minRoundsNeeded, 1)));
  let n = 0;
  for (const r of historyRoundsForDg(id)) {
    if (historyRoundIsPlaceholderAllMarketsZero(r)) continue;
    if (!Number.isFinite(actualForRoundRow(statKey, r))) continue;
    n++;
    if (n >= need) return n;
  }
  return n;
}

const FIRE3 = String.fromCodePoint(0x1f525).repeat(3);
const ICE3 = String.fromCodePoint(0x1f9ca).repeat(3);

function paintPropsTopHitsHeadUi(statKey, line) {
  const em = document.getElementById("props-top-hits-emoji");
  if (em) em.textContent = propsTopHitsFitMode === "fire" ? FIRE3 : ICE3;
  const mk = document.getElementById("props-top-hits-market");
  if (mk) mk.textContent = propMarketLabelFromKey(statKey);
  const ln = document.getElementById("props-top-hits-line-text");
  if (ln) {
    ln.textContent = Number.isFinite(line) ? `Line ${formatPropLineValueForInput(line)}` : "Line —";
  }
  const toggle = document.getElementById("props-top-hits-emoji-toggle");
  if (toggle) {
    toggle.setAttribute(
      "aria-label",
      propsTopHitsFitMode === "fire"
        ? "Prioritize fire-side fits — click to prioritize ice-side fits"
        : "Prioritize ice-side fits — click to prioritize fire-side fits"
    );
  }
}

function propsTopTableSortInPlace(rows, statKey) {
  const fitFirst =
    propsTopHitsFitMode === "fire"
      ? (r) => propsPlayerMeetsFireSide(statKey, r)
      : (r) => propsPlayerMeetsIceSide(statKey, r);
  const { key, dir } = propsTopTableSort;
  rows.sort((a, b) => {
    const fa = fitFirst(a) ? 0 : 1;
    const fb = fitFirst(b) ? 0 : 1;
    if (fa !== fb) return fa - fb;
    let cmp = 0;
    if (key === "name") cmp = String(a.name).localeCompare(String(b.name));
    else if (key === "overRate") cmp = a.overRate - b.overRate;
    else if (key === "underRate") cmp = a.underRate - b.underRate;
    else if (key === "over") cmp = a.over - b.over;
    else if (key === "under") cmp = a.under - b.under;
    if (cmp !== 0) return dir === 1 ? cmp : -cmp;
    if (b.valid !== a.valid) return b.valid - a.valid;
    return String(a.name).localeCompare(String(b.name));
  });
}

/** Calendar year on a history row (`year` field or parsed `event_completed`). */
function historyRoundSeasonYear(r) {
  if (!r || typeof r !== "object") return NaN;
  const y = num(r.year, NaN);
  if (Number.isFinite(y) && y >= 1990 && y <= 2100) return Math.round(y);
  const base = parseEventCompletedChronoBase(r.event_completed);
  const yy = Math.floor(base / 10000);
  return yy >= 1990 && yy <= 2100 ? yy : NaN;
}

const PROPS_TREND_DISPLAY_SEASON_YEAR = 2026;

function propsTrendHistoryBaselineRounds(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return [];
  return historyRoundsForDg(id).filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r));
}

function propsTrendMeanActual(statKey, rounds) {
  let sum = 0;
  let n = 0;
  for (const r of rounds) {
    const a = actualForRoundRow(statKey, r);
    if (!Number.isFinite(a)) continue;
    sum += a;
    n++;
  }
  return n > 0 ? sum / n : NaN;
}

function propsTrendCourseFilterActive() {
  return Boolean(courseFilterOn() || (propsCourseWindowModeActive() && propsEffectiveCourseKey()));
}

/** Course-only slice of career history (“Current course only” and/or Course dropdown while field-by-course window is on); ignores weather & graph window. */
function roundsMatchingCourseSelectionOnly(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return [];
  if (!propsTrendCourseFilterActive()) return [];
  let list = propsTrendHistoryBaselineRounds(id);
  if (courseFilterOn()) {
    const vn = venueCourseName();
    const metaEvent = String(DATA.meta?.event_name || "").trim();
    if (vn || metaEvent) list = list.filter((r) => currentTournamentContextMatchesRound(r));
  }
  const courseSel = propsCourseWindowModeActive() ? propsEffectiveCourseKey() : "";
  if (courseSel) list = list.filter((r) => normCourseNameKey(r.course_name) === normCourseNameKey(courseSel));
  return list;
}

/** Current tournament venue-only slice (independent of filters), used for KPI context. */
function roundsMatchingCurrentCourseOnly(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return [];
  const vn = venueCourseName();
  const metaEvent = String(DATA.meta.event_name || "").trim();
  if (!vn && !metaEvent) return [];
  let list = propsTrendHistoryBaselineRounds(id);
  list = list.filter((r) => currentTournamentContextMatchesRound(r));
  // "Current course avg" should reflect this season only, not prior-year starts at the same venue.
  list = list.filter((r) => historyRoundSeasonYear(r) === PROPS_TREND_DISPLAY_SEASON_YEAR);
  return list;
}

/** True when enough player buckets are loaded to run full-field KPI / top-10 scans safely. */
function propsFieldLeaderboardEnabled() {
  if (!HISTORY._ok || HISTORY._partial) return false;
  return Object.keys(HISTORY.byDgId || {}).length >= 20;
}

function rebuildPropsFieldVenueRoundsCache() {
  const venueRaw = String(DATA?.meta?.course_used || DATA?.course_used || "").trim();
  const sig = `${historyMutationEpoch}|${normCourseNameKey(venueRaw)}`;
  if (!venueRaw || !propsFieldLeaderboardEnabled()) {
    propsFieldVenueRoundsCacheSig = sig;
    propsFieldVenueRoundsCache = { season: [], all: [] };
    return;
  }
  if (propsFieldVenueRoundsCacheSig === sig) return;
  const season = [];
  const all = [];
  for (const rec of Object.values(HISTORY.byDgId || {})) {
    if (!rec || !Array.isArray(rec.rounds)) continue;
    for (const r of rec.rounds) {
      if (historyRoundIsPlaceholderAllMarketsZero(r)) continue;
      if (!courseNameMatchesVenueLoose(r.course_name, venueRaw)) continue;
      all.push(r);
      if (historyRoundSeasonYear(r) === PROPS_TREND_DISPLAY_SEASON_YEAR) season.push(r);
    }
  }
  propsFieldVenueRoundsCacheSig = sig;
  propsFieldVenueRoundsCache = { season, all };
}

/** Field-wide current-course rounds (all players) for this season, for the Current course avg KPI. */
function roundsMatchingCurrentCourseOnlyFieldSeason() {
  rebuildPropsFieldVenueRoundsCache();
  return propsFieldVenueRoundsCache.season;
}

/** Field-wide current-course rounds (all players), all seasons. */
function roundsMatchingCurrentCourseOnlyFieldAllTime() {
  rebuildPropsFieldVenueRoundsCache();
  return propsFieldVenueRoundsCache.all;
}

function formatPropsTrendKpiValue(statKey, v) {
  if (!Number.isFinite(v)) return "—";
  void statKey;
  if (Math.abs(v - Math.round(v)) < 1e-9) return String(Math.round(v));
  return v.toFixed(1);
}

function propMarketTrendBookLabel(statKey) {
  if (statKey === "total") return "Score";
  return propMarketLabelFromKey(statKey);
}

/**
 * Posted O/U for a sportsbook (`fanduel` | `draftkings`) nearest the chart line.
 */
function propsTrendPickQuoteForBook(playerRow, marketLabel, hintLine, bookSlug) {
  const canon = ouPropsCanonicalMarket(marketLabel);
  const want = String(bookSlug || "").trim().toLowerCase();
  if (!want || !playerRow) return null;
  const props = Array.isArray(DATA.props) ? DATA.props : [];
  /** @type {{ line: number, over: number, under: number }[]} */
  const cand = [];
  for (const r of props) {
    const src = String(r.source || "").trim().toLowerCase();
    if (src !== want) continue;
    if (String(r.market || "").trim() !== canon) continue;
    const L = enforceHalfLine(num(r.line, NaN));
    const o = Math.round(num(r.over_odds, NaN));
    const u = Math.round(num(r.under_odds, NaN));
    if (!Number.isFinite(L) || !Number.isFinite(o) || !Number.isFinite(u) || o === 0 || u === 0) continue;
    const rid = Math.round(num(r.dg_id, NaN));
    const wantId = Math.round(num(playerRow.dg_id, NaN));
    const rRaw = ouPropPlayerKeyRaw(r.player_name || "");
    const rDisp = ouPropPlayerKeyDisplay(r.player_name || "");
    const wantRaw = ouPropPlayerKeyRaw(playerRow.player_name || "");
    const wantDisp = ouPropPlayerKeyDisplay(playerRow.player_name || "");
    const sameById = Number.isFinite(wantId) && wantId > 0 && rid === wantId;
    const sameByName =
      (wantRaw && rRaw && wantRaw === rRaw) || (wantDisp && rDisp && wantDisp === rDisp);
    if (!sameById && !sameByName) continue;
    cand.push({ line: L, over: o, under: u });
  }
  if (!cand.length) return null;
  const h = enforceHalfLine(hintLine);
  if (!Number.isFinite(h)) return cand.slice().sort((a, b) => a.line - b.line)[0];
  let best = cand[0];
  let bd = Math.abs(best.line - h);
  for (let i = 1; i < cand.length; i++) {
    const d = Math.abs(cand[i].line - h);
    if (d < bd) {
      best = cand[i];
      bd = d;
    }
  }
  return best;
}

function paintPropsTrendBookRows(playerRow, statKey, lineHint, hitSt) {
  const wrap = document.getElementById("props-trends-book-lines");
  if (!wrap) return;
  wrap.replaceChildren();
  if (!playerRow) return;

  const marketLbl = ouMarketKeyFromStatKey(statKey);
  const mShort = propMarketTrendBookLabel(statKey);
  const fd = propsTrendPickQuoteForBook(playerRow, marketLbl, lineHint, "fanduel");
  const dk = propsTrendPickQuoteForBook(playerRow, marketLbl, lineHint, "draftkings");

  const mkRow = (bookKey, pick) => {
    if (!pick) return;
    const meta = SPORTSBOOK_META[bookKey];
    if (!meta) return;
    const row = document.createElement("div");
    row.className = `props-trends-book-row${propsStatLowerIsBetter(statKey) ? " props-ou-lower-is-better" : ""}`;
    const logoWrap = document.createElement("span");
    logoWrap.className = "props-trends-book-logo-wrap";
    const img = document.createElement("img");
    img.className = "props-trends-book-logo-img";
    img.alt = meta.label;
    img.loading = "lazy";
    const fb = document.createElement("span");
    fb.className = "props-trends-book-logo-fallback";
    fb.textContent = meta.short || bookKey.slice(0, 2).toUpperCase();
    fb.style.display = "none";
    logoWrap.appendChild(img);
    logoWrap.appendChild(fb);
    row.appendChild(logoWrap);
    attachBookLogoWithFallback(img, fb, meta.domain);

    const main = document.createElement("span");
    main.className = "props-trends-book-main";
    main.appendChild(document.createTextNode(`${formatPropLineValueForInput(pick.line)} `));
    const mkt = document.createElement("span");
    mkt.className = "props-trends-book-mkt";
    mkt.textContent = mShort;
    main.appendChild(mkt);
    row.appendChild(main);

    const ou = document.createElement("span");
    ou.className = "props-trends-book-ou";
    const oSp = document.createElement("span");
    oSp.className = "props-trends-book-ou-over";
    oSp.textContent = `O ${formatAmerican(pick.over)}`;
    const uSp = document.createElement("span");
    uSp.className = "props-trends-book-ou-under";
    uSp.textContent = `U ${formatAmerican(pick.under)}`;
    ou.appendChild(oSp);
    ou.appendChild(uSp);
    row.appendChild(ou);
    wrap.appendChild(row);
  };

  mkRow("fanduel", fd);
  mkRow("draftkings", dk);

  if (!fd && !dk && Number.isFinite(lineHint)) {
    const lowerBetter = propsStatLowerIsBetter(statKey);
    const lineTxt = formatPropLineValueForInput(lineHint);
    const market = propMarketLabelFromKey(statKey);
    const underPct =
      hitSt && hitSt.valid > 0 && Number.isFinite(hitSt.underRate) ? Math.round(hitSt.underRate * 100) : NaN;
    const overPct =
      hitSt && hitSt.valid > 0 && Number.isFinite(hitSt.overRate) ? Math.round(hitSt.overRate * 100) : NaN;
    const underTxt =
      hitSt && hitSt.valid > 0 && Number.isFinite(underPct) ? `${hitSt.under}/${hitSt.valid} (${underPct}%)` : "—";
    const overTxt =
      hitSt && hitSt.valid > 0 && Number.isFinite(overPct) ? `${hitSt.over}/${hitSt.valid} (${overPct}%)` : "—";
    const fbRow = document.createElement("div");
    fbRow.className = `props-trends-inline-meta${lowerBetter ? " props-ou-lower-is-better" : ""}`;
    fbRow.innerHTML = `<span class="props-trends-inline-line">Line ${lineTxt}</span><span class="props-trends-inline-market">${market}</span><span class="props-trends-inline-under">U ${underTxt}</span><span class="props-trends-inline-over">O ${overTxt}</span>`;
    wrap.appendChild(fbRow);
  }
}

function paintPropsTrendKpiRow(statKey, hitSt, graphSeries, dgId) {
  const el = document.getElementById("props-trends-kpis");
  if (!el) return;
  el.replaceChildren();
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return;

  const pool = propsTrendHistoryBaselineRounds(id);
  const allMean = propsTrendMeanActual(statKey, pool);
  const seasonRounds = pool.filter((r) => historyRoundSeasonYear(r) === PROPS_TREND_DISPLAY_SEASON_YEAR);
  const seasonMean = propsTrendMeanActual(statKey, seasonRounds);

  const vals = (graphSeries || []).map((s) => s.actual).filter((x) => Number.isFinite(x));
  const graphMean = vals.length ? vals.reduce((a, b) => a + b, 0) / vals.length : NaN;

  const addKpi = (label, val) => {
    const wrap = document.createElement("div");
    wrap.className = "props-trends-kpi";
    const lab = document.createElement("span");
    lab.className = "props-trends-kpi-lab";
    lab.textContent = label;
    const v = document.createElement("span");
    v.className = "props-trends-kpi-val";
    v.textContent = formatPropsTrendKpiValue(statKey, val);
    wrap.appendChild(lab);
    wrap.appendChild(v);
    el.appendChild(wrap);
  };

  addKpi("All-time avg", allMean);
  addKpi("Season avg", seasonMean);
  addKpi("Graph avg", graphMean);
  const atVenueSeason = roundsMatchingCurrentCourseOnlyFieldSeason();
  const atVenueAll = roundsMatchingCurrentCourseOnlyFieldAllTime();
  addKpi(
    `${PROPS_TREND_DISPLAY_SEASON_YEAR} course avg`,
    propsTrendMeanActual(statKey, atVenueSeason),
  );
  const venueFallbackLabel =
    atVenueSeason.length ? "All-time course avg" : atVenueAll.length ? "All Time Course Avg" : "All-time course avg";
  addKpi(venueFallbackLabel, propsTrendMeanActual(statKey, atVenueAll));

  if (hitSt && hitSt.valid > 0) {
    const lowerBetter = propsStatLowerIsBetter(statKey);
    const addRateKpi = (label, rate, wins, total, side) => {
      const wrap = document.createElement("div");
      wrap.className = "props-trends-kpi";
      const lab = document.createElement("span");
      lab.className = "props-trends-kpi-lab";
      lab.textContent = label;
      const val = document.createElement("span");
      val.className = "props-trends-kpi-val";
      val.textContent = Number.isFinite(rate) ? `${(rate * 100).toFixed(1)}% (${wins}/${total})` : "—";
      const isUnderSide = side === "under";
      const greenSide = lowerBetter ? isUnderSide : !isUnderSide;
      val.classList.add(greenSide ? "ev-pos" : "ev-neg");
      wrap.appendChild(lab);
      wrap.appendChild(val);
      el.appendChild(wrap);
    };
    addRateKpi("Over hit rate", hitSt.overRate, hitSt.over, hitSt.valid, "over");
    addRateKpi("Under hit rate", hitSt.underRate, hitSt.under, hitSt.valid, "under");
  }

}

function paintPropsTrendsInsightHeader(playerRow, statKey, line, hitSt, graphSeries, dgId) {
  paintPropsTrendBookRows(playerRow, statKey, line, hitSt);
  paintPropsTrendKpiRow(statKey, hitSt, graphSeries, dgId);
}

function paintPropsTopTableSortHeaders() {
  document.querySelectorAll("#table-props-top-hits thead th[data-props-sort]").forEach((th) => {
    const k = th.getAttribute("data-props-sort");
    const active = k === propsTopTableSort.key;
    th.setAttribute("aria-sort", active ? (propsTopTableSort.dir === -1 ? "descending" : "ascending") : "none");
    const up = th.querySelector(".sort-up");
    const dn = th.querySelector(".sort-down");
    if (up && dn) {
      up.classList.toggle("active", active && propsTopTableSort.dir === -1);
      dn.classList.toggle("active", active && propsTopTableSort.dir === 1);
    }
  });
}

let propsTopTableSortListenerBound = false;
function initPropsTopTableSortOnce() {
  if (propsTopTableSortListenerBound) return;
  const thead = document.querySelector("#table-props-top-hits thead");
  if (!thead) return;
  propsTopTableSortListenerBound = true;
  thead.addEventListener("click", (ev) => {
    const th = ev.target.closest("th[data-props-sort]");
    if (!th) return;
    const key = th.getAttribute("data-props-sort");
    if (!key || !["name", "overRate", "over", "underRate", "under"].includes(key)) return;
    if (propsTopTableSort.key === key) {
      propsTopTableSort = { key, dir: /** @type {-1 | 1} */ (-propsTopTableSort.dir) };
    } else {
      const defaultDir = key === "name" ? 1 : -1;
      propsTopTableSort = { key, dir: defaultDir };
    }
    renderPropsTrendsNow();
  });
}

function renderPropsHitRateAndTopTable(statKey, line, winN, courseWindowEntriesOpt) {
  initPropsTopTableSortOnce();
  paintPropsTopHitsHeadUi(statKey, line);
  const block = document.getElementById("props-hit-rate-block");
  const underEl = document.getElementById("props-hit-under-val");
  const overEl = document.getElementById("props-hit-over-val");
  const tbody = document.querySelector("#table-props-top-hits tbody");
  if (!HISTORY._ok) {
    if (block) {
      block.hidden = true;
      block.classList.remove("props-ou-lower-is-better");
    }
    if (underEl) underEl.textContent = "—";
    if (overEl) overEl.textContent = "—";
    if (tbody) tbody.innerHTML = "";
    paintPropsTopTableSortHeaders();
    return;
  }
  const wn = clamp(
    Math.round(num(winN, PROPS_HISTORY_ROUND_DEFAULT)),
    PROPS_HISTORY_ROUND_MIN,
    PROPS_HISTORY_ROUND_MAX
  );
  const dg = selectedDgId();
  const st = propsCourseWindowModeActive()
    ? propsCourseWindowFieldHitStats(statKey, line, wn)
    : propsFullHitStatsForDg(dg, statKey, line, wn);
  if (block) {
    block.hidden = false;
    block.classList.toggle("props-ou-lower-is-better", propsStatLowerIsBetter(statKey));
  }
  if (underEl) {
    if (st.valid > 0) {
      const pct = Math.round(st.underRate * 100);
      underEl.textContent = `${st.under}/${st.valid} (${pct}%)`;
    } else {
      underEl.textContent = "—";
    }
  }
  if (overEl) {
    if (st.valid > 0) {
      const pct = Math.round(st.overRate * 100);
      overEl.textContent = `${st.over}/${st.valid} (${pct}%)`;
    } else {
      overEl.textContent = "—";
    }
  }
  if (!tbody) return;
  tbody.innerHTML = "";
  if (!propsCourseWindowModeActive() && !propsFieldLeaderboardEnabled()) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 6;
    td.className = "text-muted";
    td.textContent = HISTORY._partial
      ? window.matchMedia("(max-width: 699px)").matches
        ? "Selected-player history loaded. Full-field rankings load after history finishes (or open Course fit once)."
        : "Loading full-field rankings…"
      : "Full-field rankings need more player history loaded.";
    tr.appendChild(td);
    tbody.appendChild(tr);
    paintPropsTopTableSortHeaders();
    return;
  }
  const minR = propsTopHitMinRoundsForFilter();
  /** Pre-aggregate course-window rounds once (avoid O(field × history) rescans per golfer). */
  let courseWindowRoundsByDg = /** @type {Map<number, object[]> | null} */ (null);
  if (propsCourseWindowModeActive()) {
    courseWindowRoundsByDg = new Map();
    const list = courseWindowEntriesOpt || collectCourseWindowRoundEntriesFixed();
    for (const e of list) {
      const id = e.dgId;
      if (!courseWindowRoundsByDg.has(id)) courseWindowRoundsByDg.set(id, []);
      courseWindowRoundsByDg.get(id).push(e.row);
    }
  }
  const ids = propsCourseWindowModeActive()
    ? [...(courseWindowRoundsByDg?.keys() || [])].map((id) => Math.round(num(id, NaN))).filter((x) => Number.isFinite(x))
    : Object.keys(HISTORY.byDgId || {})
        .map((k) => num(k, NaN))
        .filter((x) => Number.isFinite(x));
  const nameByDg = buildPropsGolferDisplayNameMap();
  const courseWindowNameByDg = new Map();
  if (propsCourseWindowModeActive() && courseWindowEntriesOpt?.length) {
    for (const e of courseWindowEntriesOpt) {
      const id = Math.round(num(e.dgId, NaN));
      if (!Number.isFinite(id) || courseWindowNameByDg.has(id)) continue;
      const nm = resolveGolferDisplayNameForDg(id, e.playerName, nameByDg);
      if (nm) courseWindowNameByDg.set(id, nm);
    }
  }
  const rows = [];
  for (const id of ids) {
    if (!propsCourseWindowModeActive() && propsMaxFiniteMarketRoundsUpperBound(id, statKey, minR) < minR) {
      continue;
    }
    const s = propsCourseWindowModeActive()
      ? propsFullHitStatsFromRoundList(statKey, line, courseWindowRoundsByDg?.get(id) || [])
      : propsFullHitStatsForDg(id, statKey, line, wn, true);
    if (s.valid < minR) continue;
    const name = propsCourseWindowModeActive()
      ? courseWindowNameByDg.get(id) || resolveGolferDisplayNameForDg(id, "", nameByDg)
      : resolveGolferDisplayNameForDg(id, nameByDg.get(id), nameByDg);
    if (!name) continue;
    rows.push({
      dgId: id,
      name,
      valid: s.valid,
      over: s.over,
      under: s.under,
      overRate: s.overRate,
      underRate: s.underRate,
    });
  }
  propsTopTableSortInPlace(rows, statKey);
  const top = rows.slice(0, 10);
  if (!top.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 6;
    td.className = "text-muted";
    td.textContent = `No golfers with at least ${minR} qualifying rounds for this filter.`;
    tr.appendChild(td);
    tbody.appendChild(tr);
    paintPropsTopTableSortHeaders();
    return;
  }
  top.forEach((r, i) => {
    const tr = document.createElement("tr");
    if (r.dgId === dg) tr.classList.add("props-top-hit-current");
    const mk = (txt, cls) => {
      const td = document.createElement("td");
      td.textContent = txt;
      if (cls) td.className = cls;
      return td;
    };
    tr.appendChild(mk(String(i + 1), "num"));
    tr.appendChild(mk(r.name));
    tr.appendChild(mk(`${(r.overRate * 100).toFixed(1)}%`, "num"));
    tr.appendChild(mk(`${r.over} / ${r.valid}`, "num"));
    tr.appendChild(mk(`${(r.underRate * 100).toFixed(1)}%`, "num"));
    tr.appendChild(mk(`${r.under} / ${r.valid}`, "num"));
    tbody.appendChild(tr);
  });
  paintPropsTopTableSortHeaders();
}

function modelForHistoryRow(statKey, row) {
  const r = projectionRowForPlayerRound(row._playerName || "", getOuRound());
  if (!r) return NaN;
  const dgId = Math.round(num(r.dg_id, NaN));
  let base = NaN;
  if (statKey === "total") base = num(r.total_score, NaN);
  else if (statKey === "birdies") base = num(r.birdies, NaN);
  else if (statKey === "pars") base = num(r.pars, NaN);
  else if (statKey === "bogeys") base = num(r.bogeys, NaN);
  else if (statKey === "gir") base = girFairwaysCountFromRawForOu(num(r.gir, NaN), 18);
  else if (statKey === "fairways") base = girFairwaysCountFromRawForOu(num(r.fairways, NaN), fairwayHolesModeledFromData());
  else if (statKey === "putts") base = num(r.putts, NaN);
  if (!Number.isFinite(base)) return NaN;
  const liveRound =
    statKey === "total" && inPlayAffectsRoundOdds() ? liveCurrentRoundTotalScoreMuDelta(r) : 0;
  return (
    base +
    pricingModelHistoryNudge(statKey, dgId) +
    combinedCourseDifficultyOUMuAdjustment(ouMarketKeyFromStatKey(statKey), r) +
    liveRound
  );
}

/** Chart date string for a history round (see `historyRoundPlayMdY`). */
function propsTrendChartDateFromRow(r) {
  return historyRoundPlayMdY(r);
}

function shortPropsDateLabel(completed) {
  const t = String(completed || "").trim();
  const p = t.split("/");
  if (p.length >= 2) {
    const day = String(p[1]).replace(/-\d+$/, "").trim();
    return `${p[0]}/${day}`;
  }
  return t.length > 6 ? t.slice(0, 6) : t;
}

/** Chart x-axis: month/day only (no year). Strips trailing `-R` round suffix from day if present. */
function propsChartAxisLabel(completed) {
  const t = String(completed || "").trim();
  const parts = t.split("/").map((s) => s.trim());
  if (parts.length >= 2) {
    const day = String(parts[1]).replace(/-\d+$/, "").trim();
    return `${parts[0]}/${day}`;
  }
  const iso = t.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (iso) {
    return `${parseInt(iso[2], 10)}/${parseInt(iso[3], 10)}`;
  }
  return shortPropsDateLabel(t);
}

/**
 * One string per bar for the x-axis (M/D; adds 'YY when the same calendar label spans multiple years).
 */
function buildPropsTrendXAxisLabels(series) {
  if (!series.length) return [];
  const bases = series.map((s) => propsChartAxisLabel(s.date || ""));
  const countByBase = new Map();
  for (const b of bases) countByBase.set(b, (countByBase.get(b) || 0) + 1);
  return series.map((s, i) => {
    const b = bases[i];
    if ((countByBase.get(b) || 0) <= 1) return b;
    const r = s._hist;
    const yr = num(r?.year, NaN);
    const sameBaseIdx = [];
    for (let j = 0; j < bases.length; j++) if (bases[j] === b) sameBaseIdx.push(j);
    const years = new Set(
      sameBaseIdx.map((j) => num(series[j]._hist?.year, NaN)).filter((y) => Number.isFinite(y) && y >= 1990)
    );
    if (years.size > 1 && Number.isFinite(yr)) return `${b} '${String(yr).slice(-2)}`;
    return b;
  });
}

/**
 * FanDuel-style x-axis: a few labels when many bars; duplicate display strings blanked.
 * `perBarLabels` = output of buildPropsTrendXAxisLabels (same length as series).
 * Returns Map barIndex → label string.
 */
function propsChartSparseTickLabels(perBarLabels, innerWidthPx) {
  const n = perBarLabels.length;
  const map = new Map();
  if (!n) return map;
  const minPx = 62;
  const maxTicks = Math.max(4, Math.min(10, Math.floor(innerWidthPx / minPx)));
  let indices;
  if (n <= maxTicks) {
    indices = Array.from({ length: n }, (_, i) => i);
  } else if (n === 1) {
    indices = [0];
  } else {
    const k = maxTicks;
    indices = [];
    for (let j = 0; j < k; j++) {
      indices.push(Math.round((j / (k - 1)) * (n - 1)));
    }
  }
  const uniq = [...new Set(indices)].sort((a, b) => a - b);
  for (const i of uniq) {
    map.set(i, String(perBarLabels[i] || "").trim());
  }
  const shown = new Set();
  for (const i of uniq) {
    const t = map.get(i);
    if (!t) continue;
    if (shown.has(t)) map.set(i, "");
    else shown.add(t);
  }
  return map;
}

/** One label per bar for small samples; larger windows use sparse ticks to avoid clutter. */
function propsChartXAxisDateLabels(perBarLabels, innerW) {
  const n = perBarLabels.length;
  const map = new Map();
  if (!n) return map;
  if (n <= 15) {
    for (let i = 0; i < n; i++) {
      map.set(i, String(perBarLabels[i] || "").trim());
    }
    return map;
  }
  const minPx = 54;
  const labelEveryBar = n * minPx <= innerW;
  if (labelEveryBar) {
    const seen = new Map();
    for (let i = 0; i < n; i++) {
      let lab = String(perBarLabels[i] || "").trim();
      const prev = seen.get(lab) || 0;
      seen.set(lab, prev + 1);
      if (prev > 0) lab = "";
      map.set(i, lab);
    }
    return map;
  }
  return propsChartSparseTickLabels(perBarLabels, innerW);
}

/**
 * Equal spacing per round (rolling window). Time-based x was misleading with multi-year history
 * (years squashed left, recent rounds sparse right).
 */
function propsChartBarLayout(series, padL, innerW) {
  const n = series.length;
  const xCenter = new Array(n);
  const barW = new Array(n);
  if (n === 0) return { xCenter, barW };
  const slotEq = innerW / n;
  const bwFrac = n > 48 ? 0.44 : n > 24 ? 0.56 : 0.72;
  const minBarPx = n > 48 ? 4 : n > 24 ? 6 : 10;
  for (let i = 0; i < n; i++) {
    xCenter[i] = padL + (i + 0.5) * slotEq;
    if (n === 1) {
      barW[i] = Math.max(18, Math.min(innerW * 0.22, 72));
    } else {
      const maxW = Math.max(1, slotEq - 1);
      barW[i] = Math.max(1, Math.min(maxW, Math.max(minBarPx, slotEq * bwFrac)));
    }
  }
  return { xCenter, barW };
}

/** `course_name` from historical_rounds_all → JSON (only field used for “Course” in UI). */
function propsCourseNameFromRow(r) {
  if (!r || typeof r !== "object") return "";
  return String(r.course_name ?? "").trim();
}

function propsCourseDisplay(s) {
  const r = s && s._hist;
  if (r && typeof r === "object") {
    const c = propsCourseNameFromRow(r);
    if (c) return courseFitPrettyCourseKey(normCourseNameKey(c));
  }
  const raw = String(s?.course ?? "").trim();
  return raw ? courseFitPrettyCourseKey(normCourseNameKey(raw)) : "—";
}

function pointInPropsChartHitRegion(canvasX, canvasY) {
  return propsChartHitRegions.some((r) => canvasX >= r.x0 && canvasX < r.x0 + r.w && canvasY >= r.y0 && canvasY < r.y0 + r.h);
}

/** Hit regions are non-overlapping column slots; first match is unambiguous. */
function pickPropsChartHit(canvasX, canvasY) {
  for (const r of propsChartHitRegions) {
    if (canvasX >= r.x0 && canvasX < r.x0 + r.w && canvasY >= r.y0 && canvasY < r.y0 + r.h) return r;
  }
  return null;
}

function hidePropsChartTooltip() {
  propsChartTooltipPinned = false;
  const tip = document.getElementById("props-chart-tooltip");
  if (tip) tip.hidden = true;
}

function propsChartFormatValue(statKey, v) {
  if (!Number.isFinite(v)) return "—";
  return String(Math.round(v));
}

/**
 * Map pointer to chart coordinates. Drawing + hit regions use logical CSS pixels (0..clientWidth)
 * after setTransform(dpr); bitmap width/height are dpr × logical — do not use canvas.width here.
 */
function canvasCoordsFromEvent(canvas, ev) {
  const rect = canvas.getBoundingClientRect();
  const lw = canvas.clientWidth || rect.width;
  const lh = canvas.clientHeight || rect.height;
  if (rect.width <= 0 || rect.height <= 0) return { x: 0, y: 0 };
  return {
    x: ((ev.clientX - rect.left) / rect.width) * lw,
    y: ((ev.clientY - rect.top) / rect.height) * lh,
  };
}

function showPropsChartTooltip(canvas, ev, hit) {
  const wrap = canvas.closest(".props-trends-chart-wrap");
  const tip = document.getElementById("props-chart-tooltip");
  if (!wrap || !tip) return;
  tip.replaceChildren();
  const row = (label, value) => {
    const div = document.createElement("div");
    div.className = "props-tip-row";
    const lb = document.createElement("strong");
    lb.textContent = label;
    const val = document.createElement("span");
    val.className = "props-tip-value";
    val.textContent = value;
    div.appendChild(lb);
    div.appendChild(val);
    tip.appendChild(div);
  };
  row("Date", hit.date || "—");
  if (hit.playerName) row("Golfer", hit.playerName);
  row("Value", propsChartFormatValue(hit.statKey, hit.actual));
  row("Course", propsCourseDisplay(hit));
  tip.hidden = false;
  const padWrap = 8;
  const wRect = wrap.getBoundingClientRect();
  let left = ev.clientX - wRect.left + 10;
  let top = ev.clientY - wRect.top + 10;
  tip.style.left = `${left}px`;
  tip.style.top = `${top}px`;
  const tw = tip.offsetWidth;
  const th = tip.offsetHeight;
  const maxL = wrap.clientWidth - tw - padWrap;
  const maxT = wrap.clientHeight - th - padWrap;
  if (left > maxL) left = Math.max(padWrap, maxL);
  if (top > maxT) top = Math.max(padWrap, maxT);
  tip.style.left = `${left}px`;
  tip.style.top = `${top}px`;
}

/** Y-axis ticks: integers for round score / counting stats so grid lines match numeric labels. */
function propsChartYTickValues(minV, maxV, statKey) {
  const intLike =
    statKey === "total" ||
    statKey === "birdies" ||
    statKey === "pars" ||
    statKey === "bogeys" ||
    statKey === "gir" ||
    statKey === "fairways" ||
    statKey === "putts";
  if (!intLike) {
    const n = 5;
    const out = [];
    for (let g = 0; g <= n; g++) out.push(minV + ((maxV - minV) * g) / n);
    return out;
  }
  const lo = Math.floor(minV);
  const hi = Math.ceil(maxV);
  if (hi <= lo) return [lo, hi + 1];
  const span = hi - lo;
  let step = Math.max(1, Math.round(span / 5));
  if (step === 3 && span >= 18) step = 4;
  const ticks = [];
  for (let t = Math.ceil(lo / step) * step; t <= hi; t += step) ticks.push(t);
  if (!ticks.length) return [lo, hi];
  if (ticks[0] > lo) ticks.unshift(lo);
  if (ticks[ticks.length - 1] < hi) ticks.push(hi);
  return [...new Set(ticks)].sort((a, b) => a - b);
}

function propsChartTickLabel(statKey, v) {
  const intLike =
    statKey === "total" ||
    statKey === "birdies" ||
    statKey === "pars" ||
    statKey === "bogeys" ||
    statKey === "gir" ||
    statKey === "fairways" ||
    statKey === "putts";
  if (intLike) return String(Math.round(v));
  return Math.abs(v - Math.round(v)) < 0.05 ? String(Math.round(v)) : String(Number(v.toFixed(1)));
}

/** Match CSS layout box to bitmap aspect ratio (avoids non-uniform scaling from height:auto + max-height). */
function syncPropsTrendCanvasCssBox(canvas, cssW, cssH) {
  canvas.style.width = `${cssW}px`;
  canvas.style.height = `${cssH}px`;
  canvas.style.maxWidth = "100%";
  canvas.style.boxSizing = "border-box";
}

/** `series` items: `{ actual, date?, _hist? }` — `_hist` is raw round row (course_name, …). */
function drawPropsTrendCanvas(series, lineY, statKey) {
  propsChartHitRegions = [];
  hidePropsChartTooltip();
  const canvas = document.getElementById("props-trend-canvas");
  const wrap = canvas?.closest(".props-trends-chart-wrap");
  if (!canvas || !canvas.getContext) return;
  const dpr = Math.min(2, window.devicePixelRatio || 1);
  const vhCap = typeof window !== "undefined" ? Math.min(480, Math.round(window.innerHeight * 0.42)) : 480;

  function paintEmptyBackground(cssW0, cssH0) {
    canvas.width = Math.round(cssW0 * dpr);
    canvas.height = Math.round(cssH0 * dpr);
    syncPropsTrendCanvasCssBox(canvas, cssW0, cssH0);
    const c0 = canvas.getContext("2d");
    if (!c0) return;
    c0.setTransform(dpr, 0, 0, dpr, 0, 0);
    c0.clearRect(0, 0, cssW0, cssH0);
    c0.fillStyle = "#0a0c0f";
    c0.fillRect(0, 0, cssW0, cssH0);
  }

  if (!series.length) {
    const vis = wrap && wrap.clientWidth > 80 ? wrap.clientWidth - 28 : 800;
    const cssH0 = Math.round(clamp(vis * 0.48, 240, Math.min(420, vhCap)));
    paintEmptyBackground(vis, cssH0);
    return;
  }
  const vals = series.map((s) => s.actual).filter((x) => Number.isFinite(x));
  if (!vals.length) {
    const vis = wrap && wrap.clientWidth > 80 ? wrap.clientWidth - 28 : 800;
    paintEmptyBackground(vis, Math.round(clamp(vis * 0.48, 240, Math.min(420, vhCap))));
    return;
  }

  const n = series.length;
  const visibleW = wrap && wrap.clientWidth > 80 ? wrap.clientWidth - 28 : 400;
  const pad = { l: 42, r: 14, t: 12, b: n > 12 ? 54 : 46 };
  const innerW = Math.max(80, visibleW - pad.l - pad.r);
  const cssW = Math.round(visibleW);
  const cssH = Math.round(clamp(visibleW * 0.5, 260, Math.min(480, vhCap)));
  syncPropsTrendCanvasCssBox(canvas, cssW, cssH);
  canvas.width = Math.round(cssW * dpr);
  canvas.height = Math.round(cssH * dpr);
  const ctx = canvas.getContext("2d");
  if (!ctx) return;
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  const w = cssW;
  const h = cssH;
  ctx.clearRect(0, 0, w, h);
  ctx.fillStyle = "#0a0c0f";
  ctx.fillRect(0, 0, w, h);

  let minV = Math.min(...vals);
  let maxV = Math.max(...vals);
  if (Number.isFinite(lineY)) {
    minV = Math.min(minV, lineY);
    maxV = Math.max(maxV, lineY);
  }
  // Keep a wider y-domain so bars are easier to read.
  const baseRange = Math.max(0, maxV - minV);
  const minSpan = statKey === "total" ? 12 : 8;
  const padAbs = statKey === "total" ? 2 : 1;
  const yPad = Math.max(padAbs, baseRange * 0.35);
  minV -= yPad;
  maxV += yPad;
  if (maxV - minV < minSpan) {
    const mid = (minV + maxV) / 2;
    minV = mid - minSpan / 2;
    maxV = mid + minSpan / 2;
  }
  if (statKey !== "total") minV = Math.max(0, minV);
  if (minV === maxV) {
    minV -= 1;
    maxV += 1;
  }
  const innerH = h - pad.t - pad.b;
  /** Numeric axis: smallest value at bottom of chart, largest at top (standard scale). */
  function yScale(v) {
    const t = (v - minV) / (maxV - minV);
    return pad.t + innerH * (1 - t);
  }
  const yBase = yScale(minV);
  const yTicks = propsChartYTickValues(minV, maxV, statKey);
  ctx.strokeStyle = "rgba(255, 255, 255, 0.07)";
  ctx.lineWidth = 1;
  for (const tv of yTicks) {
    if (tv < minV - 1e-9 || tv > maxV + 1e-9) continue;
    const y = yScale(tv);
    ctx.beginPath();
    ctx.moveTo(pad.l, y);
    ctx.lineTo(w - pad.r, y);
    ctx.stroke();
  }
  ctx.strokeStyle = "#2b2e36";
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(pad.l, pad.t);
  ctx.lineTo(pad.l, h - pad.b);
  ctx.lineTo(w - pad.r, h - pad.b);
  ctx.stroke();
  const slotW = innerW / n;
  const xAxisPerBar = buildPropsTrendXAxisLabels(series);
  const { xCenter, barW } = propsChartBarLayout(series, pad.l, innerW);
  const lowerIsBetter = propsStatLowerIsBetter(statKey);
  for (let i = 0; i < n; i++) {
    const v = series[i].actual;
    if (!Number.isFinite(v)) continue;
    const bw = barW[i];
    const xc = xCenter[i];
    const x0 = Math.max(pad.l, Math.min(xc - bw / 2, pad.l + innerW - bw));
    const yTop = yScale(v);
    const hBar = Math.max(1, yBase - yTop);
    const slotLeft = pad.l + i * slotW;
    propsChartHitRegions.push({
      x0: slotLeft,
      y0: pad.t,
      w: Math.max(1, slotW),
      h: yBase - pad.t,
      _hist: series[i]._hist,
      date: String(series[i].date || "").trim() || "—",
      playerName: String(series[i].playerName || "").trim(),
      actual: v,
      statKey,
    });
    let fill = "#00c46b";
    if (Number.isFinite(lineY)) {
      if (lowerIsBetter) {
        if (v < lineY) fill = "#00c46b";
        else if (v > lineY) fill = "#ff4d4f";
        else fill = "#8b8f9c";
      } else {
        if (v > lineY) fill = "#00c46b";
        else if (v < lineY) fill = "#ff4d4f";
        else fill = "#8b8f9c";
      }
    }
    ctx.fillStyle = fill;
    ctx.fillRect(Math.round(x0), yTop, Math.round(bw), hBar);
    ctx.strokeStyle = "rgba(0,0,0,0.35)";
    ctx.lineWidth = 1;
    ctx.strokeRect(Math.round(x0) + 0.5, yTop + 0.5, Math.round(bw) - 1, hBar - 1);
  }
  /* Draw reference line + pill on top of bars so the label is not occluded. */
  if (Number.isFinite(lineY)) {
    const yL = yScale(lineY);
    ctx.strokeStyle = "#f5a623";
    ctx.lineWidth = 2;
    ctx.setLineDash([]);
    ctx.beginPath();
    ctx.moveTo(pad.l, yL);
    ctx.lineTo(w - pad.r, yL);
    ctx.stroke();
    const lineLbl = formatPropLineChartLabel(statKey, lineY);
    ctx.font = "bold 10px DM Sans, sans-serif";
    ctx.textAlign = "left";
    ctx.textBaseline = "middle";
    const padX = 6;
    const tw = ctx.measureText(lineLbl).width + padX * 2;
    const bh = 16;
    const bx = pad.l + 4;
    const by = yL - bh / 2;
    ctx.fillStyle = "#f5a623";
    ctx.beginPath();
    if (typeof ctx.roundRect === "function") ctx.roundRect(bx, by, tw, bh, 4);
    else ctx.rect(bx, by, tw, bh);
    ctx.fill();
    ctx.fillStyle = "#0a0c0f";
    ctx.fillText(lineLbl, bx + padX, yL);
    ctx.textAlign = "left";
    ctx.textBaseline = "alphabetic";
  }
  ctx.fillStyle = "#8b8f9c";
  ctx.font = "9px DM Sans, sans-serif";
  ctx.textAlign = "left";
  ctx.textBaseline = "middle";
  for (const tv of yTicks) {
    if (tv < minV - 1e-9 || tv > maxV + 1e-9) continue;
    const y = yScale(tv);
    ctx.fillText(propsChartTickLabel(statKey, tv), 5, y);
  }
  ctx.textBaseline = "alphabetic";
  const tickMap = propsChartXAxisDateLabels(xAxisPerBar, innerW);
  const xLabFont = n > 36 ? 10 : 12;
  ctx.font = `${xLabFont}px DM Sans, sans-serif`;
  ctx.textAlign = "center";
  ctx.fillStyle = "#9ca0ac";
  for (const [i, lab] of tickMap.entries()) {
    if (!lab) continue;
    const cx = xCenter[i] != null ? xCenter[i] : pad.l + innerW / 2;
    ctx.fillText(lab, cx, h - 10);
  }
  ctx.textAlign = "left";
}

function syncPropsCourseWindowUiState() {
  const modeOn = propsCourseWindowModeOn();
  const on = propsCourseWindowModeActive();
  const hdr = document.getElementById("props-trends-header");
  if (hdr) hdr.classList.toggle("props-course-window-active", on);
  const sessExtra = document.getElementById("props-course-session-extra");
  if (sessExtra) sessExtra.hidden = !modeOn;
  const curCourse = document.getElementById("props-filter-current-course");
  if (curCourse && modeOn) curCourse.checked = false;
  const roundsStepper = document.querySelector(".props-chart-steppers .props-stepper-block");
  if (roundsStepper) roundsStepper.hidden = modeOn;
  if (!modeOn) propsCourseWindowLiveMergeAttempted = false;
}

/** Run Historical Trends immediately (cancels any pending debounced pass). */
function renderPropsTrendsNow() {
  window.clearTimeout(propsTrendsRenderDebounceT);
  propsTrendsRenderDebounceT = 0;
  renderPropsTrends();
}

/** Debounce Historical Trends rebuilds so typing line/filters does not rescan the full field every keystroke. */
function scheduleRenderPropsTrends(ms = 200) {
  window.clearTimeout(propsTrendsRenderDebounceT);
  propsTrendsRenderDebounceT = window.setTimeout(() => {
    propsTrendsRenderDebounceT = 0;
    renderPropsTrends();
  }, ms);
}

function paintPropsCourseWindowBuilding(message) {
  const empty = document.getElementById("props-chart-empty");
  if (empty) {
    empty.hidden = false;
    empty.textContent = message;
  }
  drawPropsTrendCanvas([], NaN, statKeyFromPropSelect());
}

function renderPropsTrendsCourseWindow() {
  ensurePropsStatSelectValid();
  const gen = ++propsCourseWindowRenderGen;
  propsCourseWindowLastEntries = null;
  syncPropsCourseWindowUiState();
  refreshPropsCourseFilterOptionsAllPlayers();
  ensurePropsCourseWindowDateDefaultsFromMeta();
  void loadPropsCoursesManifest();

  const courseKey = propsEffectiveCourseKey();
  if (!courseKey) {
    renderPropsTrendsCourseWindowBody(gen, null);
    return;
  }

  const cached = propsGetSingleCourseBucketSync(courseKey);
  if (cached) {
    renderPropsTrendsCourseWindowBody(gen, cached);
    return;
  }

  paintPropsCourseWindowBuilding("Loading all players at this course…");
  void ensurePropsCourseIndexForKeyAsync(courseKey).then((bucket) => {
    if (gen !== propsCourseWindowRenderGen || activeAppTabId() !== "props") return;
    ensurePropsCourseWindowDateDefaults();
    renderPropsTrendsCourseWindowBody(gen, bucket);
  });
}

function renderPropsTrendsCourseWindowBody(gen, courseBucket) {
  if (gen !== propsCourseWindowRenderGen) return;
  syncPropsCourseWindowUiState();
  const empty = document.getElementById("props-chart-empty");
  const titleEl = document.getElementById("props-trends-title");
  const subEl = document.getElementById("props-trends-sub");
  const flagEl = document.getElementById("props-flag");
  const statKey = statKeyFromPropSelect();
  if (propsTopTableSortStatKey !== statKey) {
    propsTopTableSort = { key: "overRate", dir: -1 };
    propsTopTableSortStatKey = statKey;
  }
  const courseKey = propsEffectiveCourseKey();
  const courseLabel = courseKey ? courseFitPrettyCourseKey(courseKey) : "—";
  if (flagEl) flagEl.hidden = true;
  if (titleEl) titleEl.textContent = courseLabel;
  if (subEl) {
    const dr = propsCourseWindowDateRangeLabel();
    const mkt = propMarketLabelFromKey(statKey);
    const sortHint = propsMarketHigherIsBetter(statKey) ? "bars least→greatest" : "bars greatest→least";
    const histNote = courseBucket?.shardMissing
      ? " · course data file missing on server (rebuild history)"
      : "";
    subEl.textContent = dr
      ? `${mkt} · ${dr} · all players at ${courseLabel}${histNote} · ${sortHint}`
      : propsEventVenueCourseKey()
        ? `${mkt} · set From/To dates for all players at ${courseLabel}${histNote}`
        : `${mkt} · projections missing course_used — cannot scope field view`;
  }
  const nWinEl = document.getElementById("props-window-n");
  const winN = clamp(
    Math.round(num(nWinEl?.value, PROPS_HISTORY_ROUND_DEFAULT)),
    PROPS_HISTORY_ROUND_MIN,
    PROPS_HISTORY_ROUND_MAX,
  );
  if (nWinEl) nWinEl.value = String(winN);

  if (!propsCourseWindowModeActive()) {
    if (empty) {
      empty.hidden = false;
      const needVenue = !propsEventVenueCourseKey();
      const fromBlank = !String(document.getElementById("props-filter-date-from")?.value || "").trim();
      const toBlank = !String(document.getElementById("props-filter-date-to")?.value || "").trim();
      const needDates = fromBlank && toBlank;
      const ck = propsEffectiveCourseKey();
      if (needVenue) {
        empty.textContent =
          "Field-by-course uses this week’s venue from projections (course_used). Reload projections or turn off field mode.";
      } else if (needDates) {
        empty.textContent = `Set at least one of From / To dates for all players at ${courseFitPrettyCourseKey(ck)}.`;
      } else if (HISTORY._ok && distinctCompletedRoundDatesAtCourse(ck).length === 0) {
        empty.textContent = `No completed rounds in history for ${courseFitPrettyCourseKey(ck)} yet (refresh data).`;
      } else {
        empty.textContent =
          "Set at least one of From / To dates (leave the other blank for an open-ended range), or widen the dates you chose.";
      }
    }
    drawPropsTrendCanvas([], NaN, statKey);
    paintPropsTrendsInsightHeader(null, statKey, NaN, { valid: 0, over: 0, under: 0, overRate: NaN, underRate: NaN }, [], NaN);
    const bookWrap = document.getElementById("props-trends-book-lines");
    if (bookWrap) bookWrap.replaceChildren();
    renderPropsHitRateAndTopTable(statKey, NaN, winN);
    return;
  }

  if (!HISTORY._ok) {
    if (empty) {
      empty.hidden = false;
      empty.textContent = HISTORY._loading ? "Loading player history..." : "No history file.";
    }
    drawPropsTrendCanvas([], NaN, statKey);
    renderPropsHitRateAndTopTable(statKey, NaN, winN);
    return;
  }
  if (!isFileProtocol() && !propsCourseWindowLiveMergeAttempted) {
    propsCourseWindowLiveMergeAttempted = true;
    void ensureLiveTournamentHistoryMerged({ useCache: true }).then((n) => {
      if (n > 0 && activeAppTabId() === "props" && propsCourseWindowModeOn()) {
        propsSingleCourseIndexSig = "";
        propsSingleCourseIndexCache = null;
        const ck = propsEffectiveCourseKey();
        if (ck) propsCourseRoundIndex.delete(ck);
        courseWindowRoundEntriesCache = null;
        courseWindowRoundEntriesCacheSig = "";
        scheduleRenderPropsTrends(0);
      }
    });
  }
  if (empty) empty.hidden = true;

  const lineInp = document.getElementById("prop-line");
  const ctxKey = propsTrendLineContextKeyFromDom();
  const lineEditing = Boolean(lineInp && document.activeElement === lineInp);
  const entriesAll = collectCourseWindowRoundEntriesFixed(courseBucket);
  propsCourseWindowLastEntries = entriesAll;
  let line = clampPropLineForMarket(statKey, snapPropLineToDotFive(lineInp?.value));
  if (lineEditing && !Number.isFinite(line) && Number.isFinite(propsTrendLastGoodLine)) {
    line = propsTrendLastGoodLine;
  }
  if (!lineEditing && (!Number.isFinite(line) || propsTrendsLineContextKey !== ctxKey)) {
    line = defaultLineForCourseWindow(statKey, entriesAll);
    if (lineInp) lineInp.value = formatPropLineValueForInput(line);
    propsTrendsLineContextKey = ctxKey;
  } else if (!lineEditing && lineInp) {
    lineInp.value = formatPropLineValueForInput(line);
  }
  if (Number.isFinite(line)) propsTrendLastGoodLine = line;

  let chartEntries = entriesAll;
  if (chartEntries.length > PROPS_COURSE_WINDOW_MAX_CHART_BARS) {
    chartEntries = sampleCourseWindowChartEntriesEvenly(chartEntries, PROPS_COURSE_WINDOW_MAX_CHART_BARS);
  }

  const nameByDgChart = buildPropsGolferDisplayNameMap();
  const seriesChart = [];
  for (const e of chartEntries) {
    const actual = actualForRoundRow(statKey, e.row);
    if (!Number.isFinite(actual)) continue;
    const playerName = resolveGolferDisplayNameForDg(e.dgId, e.playerName, nameByDgChart);
    if (!playerName) continue;
    seriesChart.push({
      actual,
      date: propsTrendChartDateFromRow(e.row),
      playerName,
      _hist: e.row,
    });
  }

  sortPropsFieldByCourseSeriesChart(statKey, seriesChart);

  if (!seriesChart.length) {
    if (empty) {
      empty.hidden = false;
      const nAll = entriesAll.length;
      if (courseBucket?.shardMissing) {
        empty.textContent =
          "Field-by-course needs player-history/by-course data on the server. Run npm run build:history and redeploy (includes this venue).";
      } else {
        empty.textContent = nAll
          ? "No chartable stat values for these rounds (try another market)."
          : "No rounds at this course on that date after filters. Pick another date or relax weather filters.";
      }
    }
  }

  drawPropsTrendCanvas(seriesChart, line, statKey);
  const hitSt = propsFullHitStatsFromRoundList(
    statKey,
    line,
    entriesAll.map((e) => e.row),
  );
  const bookWrap = document.getElementById("props-trends-book-lines");
  if (bookWrap) bookWrap.replaceChildren();
  paintPropsTrendKpiRowCourseWindow(statKey, hitSt, seriesChart, entriesAll);
  const chartLeg = document.getElementById("props-chart-line-legend");
  if (chartLeg) chartLeg.hidden = !Number.isFinite(line);
  window.requestAnimationFrame(() => {
    if (gen !== propsCourseWindowRenderGen) return;
    renderPropsHitRateAndTopTable(statKey, line, winN, propsCourseWindowLastEntries);
  });
}

function renderPropsTrends() {
  ensurePropsStatSelectValid();
  void ensurePropsDgIdNameManifestLoaded().then((m) => {
    if (!m?.size || activeAppTabId() !== "props" || propsDgIdNameManifestUiRefreshDone) return;
    propsDgIdNameManifestUiRefreshDone = true;
    scheduleRenderPropsTrends(0);
  });
  if (propsCourseWindowModeOn()) {
    renderPropsTrendsCourseWindow();
    return;
  }
  syncPropsCourseWindowUiState();
  const dg = selectedDgId();
  const empty = document.getElementById("props-chart-empty");
  const titleEl = document.getElementById("props-trends-title");
  const subEl = document.getElementById("props-trends-sub");
  refreshPropsFilterOptionsForGolfer(dg);
  const statKey = statKeyFromPropSelect();
  const selectedHistoryMissing = Number.isFinite(dg) && !historyBucketLoaded(dg);
  if (selectedHistoryMissing && historyBucketLoading(dg)) {
    if (empty) {
      empty.hidden = false;
      empty.textContent = "Loading player history...";
    }
    drawPropsTrendCanvas([], NaN, statKey);
    renderPropsHitRateAndTopTable(statKey, NaN, PROPS_HISTORY_ROUND_DEFAULT);
    return;
  }
  if (selectedHistoryMissing && activeAppTabId() === "props") {
    void ensurePlayerHistoryLoadedForTab("props");
  }
  if (propsTopTableSortStatKey !== statKey) {
    propsTopTableSort = { key: "overRate", dir: -1 };
    propsTopTableSortStatKey = statKey;
  }
  const playerRow =
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg && samePlayerRound(p, 1)) || DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg);
  const flagEl = document.getElementById("props-flag");
  if (flagEl) flagEl.hidden = false;
  if (playerRow) {
    setPropsCountryFlag(playerRow);
    if (titleEl) titleEl.textContent = displayGolferName(playerRow.player_name) || "—";
    if (subEl) subEl.textContent = "";
  } else {
    if (titleEl) titleEl.textContent = "—";
    if (subEl) subEl.textContent = "";
  }
  const historyHasSelectedBucket = Number.isFinite(dg) && historyBucketLoaded(dg);
  if (!HISTORY._ok || !historyHasSelectedBucket) {
    if (empty) {
      empty.hidden = false;
      const metaHint = String(HISTORY.meta?.note || "").trim();
      const waiting =
        historyBucketLoading(dg) || (HISTORY._loading && !historyHasSelectedBucket);
      empty.textContent = waiting
        ? "Loading player history..."
        : !HISTORY._ok
        ? "No history file."
        : metaHint ||
          "History export has no rounds yet. On Render: set GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1 (full PGA+LIV merge; slow), or widen GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS if you use a capped merge — check deploy logs for update-historical-rounds-node / build-player-history. One-shot uncapped repair: GOLF_RENDER_FULL_HISTORICAL_MERGE_IF_EMPTY=1.";
    }
    const wnEarly = clamp(
      Math.round(num(document.getElementById("props-window-n")?.value, PROPS_HISTORY_ROUND_DEFAULT)),
      PROPS_HISTORY_ROUND_MIN,
      PROPS_HISTORY_ROUND_MAX
    );
    const lineInpEarly = document.getElementById("prop-line");
    const ctxKeyEarly = propsTrendLineContextKeyFromDom();
    const lineEditingEarly = Boolean(lineInpEarly && document.activeElement === lineInpEarly);
    let lineEarly = clampPropLineForMarket(statKey, snapPropLineToDotFive(lineInpEarly?.value));
    if (lineEditingEarly && !Number.isFinite(lineEarly) && Number.isFinite(propsTrendLastGoodLine)) {
      lineEarly = propsTrendLastGoodLine;
    }
    if (!lineEditingEarly && (!Number.isFinite(lineEarly) || propsTrendsLineContextKey !== ctxKeyEarly)) {
      const rproj = projectionRowForPlayerRound(playerRow?.player_name, getOuRound());
      const fallbackRaw =
        statKey === "total"
          ? num(rproj?.total_score, defaultPropLineForStat(statKey))
          : statKey === "gir" || statKey === "fairways"
            ? (() => {
                const c =
                  statKey === "gir"
                    ? girFairwaysCountFromRawForOu(num(rproj?.gir, NaN), 18)
                    : girFairwaysCountFromRawForOu(num(rproj?.fairways, NaN), fairwayHolesModeledFromData());
                return Number.isFinite(c) ? c : defaultPropLineForStat(statKey);
              })()
            : num(rproj?.[statKey === "fairways" ? "fairways" : statKey], defaultPropLineForStat(statKey));
      lineEarly = clampPropLineForMarket(statKey, snapPropLineToDotFive(fallbackRaw));
      if (!Number.isFinite(lineEarly)) lineEarly = clampPropLineForMarket(statKey, defaultPropLineForStat(statKey));
      if (lineInpEarly) lineInpEarly.value = formatPropLineValueForInput(lineEarly);
      propsTrendsLineContextKey = ctxKeyEarly;
    } else if (!lineEditingEarly && lineInpEarly) {
      lineInpEarly.value = formatPropLineValueForInput(lineEarly);
    }
    if (Number.isFinite(lineEarly)) propsTrendLastGoodLine = lineEarly;
    const stEarly = propsFullHitStatsForDg(dg, statKey, lineEarly, wnEarly);
    paintPropsTrendsInsightHeader(playerRow, statKey, lineEarly, stEarly, [], dg);
    const chartLegEarly = document.getElementById("props-chart-line-legend");
    if (chartLegEarly) chartLegEarly.hidden = !Number.isFinite(lineEarly);
    drawPropsTrendCanvas([], lineEarly, statKey);
    renderPropsHitRateAndTopTable(statKey, lineEarly, wnEarly);
    return;
  }
  if (empty) empty.hidden = true;
  if (!isFileProtocol() && !propsTrendsLiveHistoryFetchQueued) {
    propsTrendsLiveHistoryFetchQueued = true;
    window.setTimeout(() => {
      void ensureLiveTournamentHistoryMerged({ useCache: true }).then((n) => {
        propsTrendsLiveHistoryFetchQueued = false;
        if (n > 0 && activeAppTabId() === "props") renderPropsTrends();
      });
    }, 0);
  }
  const nWinEl = document.getElementById("props-window-n");
  const winN = clamp(
    Math.round(num(nWinEl?.value, PROPS_HISTORY_ROUND_DEFAULT)),
    PROPS_HISTORY_ROUND_MIN,
    PROPS_HISTORY_ROUND_MAX
  );
  if (nWinEl) nWinEl.value = String(winN);
  const lineInp = document.getElementById("prop-line");
  const ctxKey = propsTrendLineContextKeyFromDom();
  const lineEditing = Boolean(lineInp && document.activeElement === lineInp);
  let line = clampPropLineForMarket(statKey, snapPropLineToDotFive(lineInp?.value));
  if (lineEditing && !Number.isFinite(line) && Number.isFinite(propsTrendLastGoodLine)) {
    line = propsTrendLastGoodLine;
  }
  if (!lineEditing && (!Number.isFinite(line) || propsTrendsLineContextKey !== ctxKey)) {
    const rproj = projectionRowForPlayerRound(playerRow?.player_name, getOuRound());
    const fallbackRaw =
      statKey === "total"
        ? num(rproj?.total_score, defaultPropLineForStat(statKey))
        : statKey === "gir" || statKey === "fairways"
          ? (() => {
              const c =
                statKey === "gir"
                  ? girFairwaysCountFromRawForOu(num(rproj?.gir, NaN), 18)
                  : girFairwaysCountFromRawForOu(num(rproj?.fairways, NaN), fairwayHolesModeledFromData());
              return Number.isFinite(c) ? c : defaultPropLineForStat(statKey);
            })()
          : num(rproj?.[statKey === "fairways" ? "fairways" : statKey], defaultPropLineForStat(statKey));
    line = clampPropLineForMarket(statKey, snapPropLineToDotFive(fallbackRaw));
    if (!Number.isFinite(line)) line = clampPropLineForMarket(statKey, defaultPropLineForStat(statKey));
    if (lineInp) lineInp.value = formatPropLineValueForInput(line);
    propsTrendsLineContextKey = ctxKey;
  } else if (!lineEditing && lineInp) {
    lineInp.value = formatPropLineValueForInput(line);
  }
  if (Number.isFinite(line)) propsTrendLastGoodLine = line;
  const newestFirst = propsFilteredRoundsNewestFirst(dg, winN);
  const rawList = newestFirst.slice().sort((a, b) => historyRoundChronoKey(a) - historyRoundChronoKey(b));
  const seriesFull = [];
  for (const r of rawList) {
    const actual = actualForRoundRow(statKey, r);
    if (!Number.isFinite(actual)) continue;
    const m = modelForHistoryRow(statKey, { ...r, _playerName: playerRow?.player_name });
    seriesFull.push({
      _hist: r,
      date: propsTrendChartDateFromRow(r),
      course: propsCourseNameFromRow(r),
      actual,
      model: m,
      dif: Number.isFinite(m) ? actual - m : NaN,
    });
  }
  const seriesChart = seriesFull.map((s) => ({ actual: s.actual, date: s.date, _hist: s._hist }));
  drawPropsTrendCanvas(seriesChart, line, statKey);
  const stNow = propsFullHitStatsForDg(dg, statKey, line, winN);
  paintPropsTrendsInsightHeader(playerRow, statKey, line, stNow, seriesChart, dg);
  const chartLeg = document.getElementById("props-chart-line-legend");
  if (chartLeg) chartLeg.hidden = !Number.isFinite(line);
  renderPropsHitRateAndTopTable(statKey, line, winN);
}

function updatePropsFooterEv() {
  const box = document.getElementById("ev-props");
  if (!box) return;
  const line = num(document.getElementById("prop-line")?.value, NaN);
  const oAm = num(document.getElementById("prop-over")?.value, NaN);
  const uAm = num(document.getElementById("prop-under")?.value, NaN);
  const dg = selectedDgId();
  const statKey = statKeyFromPropSelect();
  const rEv = getModelRoundForEv();
  const rproj =
    projectionPlayerRowForModel(dg, rEv) ||
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg && samePlayerRound(p, rEv)) ||
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg);
  const marketLabel = ouMarketKeyFromStatKey(statKey);
  const pOver =
    rproj && Number.isFinite(line) ? modelProbOverMarket(marketLabel, rproj, line) : NaN;
  const pUnder = Number.isFinite(pOver) ? 1 - pOver : NaN;
  const dO = decimalFromAmerican(oAm);
  const dU = decimalFromAmerican(uAm);
  const evO = Number.isFinite(pOver) && Number.isFinite(dO) ? pOver * dO - 1 : NaN;
  const evU = Number.isFinite(pUnder) && Number.isFinite(dU) ? pUnder * dU - 1 : NaN;
  let html = "";
  if (Number.isFinite(evO)) html += `<p class="${evO >= 0 ? "ev-pos" : "ev-neg"}">Over EV: ${(evO * 100).toFixed(1)}%</p>`;
  if (Number.isFinite(evU)) html += `<p class="${evU >= 0 ? "ev-pos" : "ev-neg"}">Under EV: ${(evU * 100).toFixed(1)}%</p>`;
  if (!html) html = "<p class=\"text-muted\">Enter odds to see EV.</p>";
  box.innerHTML = html;
}

function updatePropsHoleCard() {
  const courseSel = document.getElementById("props-hole-course");
  const roundSel = document.getElementById("props-hole-round");
  const holeTbody = document.querySelector("#props-hole-table tbody");
  if (!courseSel || !roundSel || !holeTbody) return;
  const dg = selectedDgId();
  const rec = HISTORY.byDgId && HISTORY.byDgId[String(dg)];
  const prow =
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg && samePlayerRound(p, 1)) ||
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg);
  const pname = String(rec?.player_name || prow?.player_name || "").trim();
  const pkey = playerKeyFromName(pname);
  const holesMap = (HISTORY.holesByPlayerKey && HISTORY.holesByPlayerKey[pkey]) || {};
  const byCourse = {};
  for (const fullKey of Object.keys(holesMap)) {
    const parts = fullKey.split("\t");
    const cname = (parts[0] || fullKey).trim() || fullKey;
    const rlabel = (parts[1] || "Round").trim();
    if (!byCourse[cname]) byCourse[cname] = [];
    byCourse[cname].push({ fullKey, rlabel });
  }
  const courseNames = Object.keys(byCourse).sort();
  const prevCourse = courseSel.dataset.lastCourse || "";
  const prevRound = roundSel.dataset.lastRound || "";
  courseSel.innerHTML = '<option value="">—</option>';
  for (const c of courseNames) {
    const o = document.createElement("option");
    o.value = c;
    o.textContent = c;
    courseSel.appendChild(o);
  }
  if (prevCourse && courseNames.includes(prevCourse)) courseSel.value = prevCourse;
  else if (courseNames.length) courseSel.value = courseNames[0];
  courseSel.dataset.lastCourse = courseSel.value || "";
  const cPick = courseSel.value;
  roundSel.innerHTML = '<option value="">—</option>';
  const roundsForCourse = cPick ? byCourse[cPick] || [] : [];
  roundsForCourse.sort((a, b) => String(a.rlabel).localeCompare(String(b.rlabel)));
  for (const { fullKey, rlabel } of roundsForCourse) {
    const o = document.createElement("option");
    o.value = fullKey;
    o.textContent = rlabel || fullKey;
    roundSel.appendChild(o);
  }
  if (prevRound && [...roundSel.options].some((o) => o.value === prevRound)) roundSel.value = prevRound;
  else if (roundSel.options.length > 1) roundSel.selectedIndex = 1;
  roundSel.dataset.lastRound = roundSel.value || "";
  holeTbody.innerHTML = "";
  const full = roundSel.value;
  const arr = full ? (holesMap[full] || []).slice().sort((a, b) => num(a.hole, 0) - num(b.hole, 0)) : [];
  for (const h of arr) {
    const tr = document.createElement("tr");
    const t0 = document.createElement("td");
    t0.textContent = String(h.hole);
    const t1 = document.createElement("td");
    t1.textContent = String(h.par);
    const t2 = document.createElement("td");
    t2.textContent = String(h.score);
    const rel = num(h.score, NaN) - num(h.par, NaN);
    if (rel < 0) t2.classList.add("good");
    else if (rel > 0) t2.classList.add("bad");
    const t3 = document.createElement("td");
    t3.textContent = String(h.score_type || "");
    tr.appendChild(t0);
    tr.appendChild(t1);
    tr.appendChild(t2);
    tr.appendChild(t3);
    holeTbody.appendChild(tr);
  }
  renderPropsShotsForSelection(dg, full);
}

const hangoutPalette = ["#00c46b", "#5ac8fa", "#ff8a8a", "#ffd166", "#c77dff", "#ff9f1c"];
/** Last simulated path length for resize redraw of `#hh-hole-canvas`. */
let hangoutCanvasShotCount = 0;
let hangoutSimDebounceT = 0;

function hangoutResultsVisible() {
  const vz = document.getElementById("hh-hole-viz");
  return Boolean(vz && !vz.hidden && hangoutCanvasShotCount > 0);
}

/** Debounced full hole sim so filters and live fields update results without hammering the UI. */
function scheduleHangoutSimulateDebounced(ms = 240) {
  window.clearTimeout(hangoutSimDebounceT);
  hangoutSimDebounceT = window.setTimeout(() => runHangoutSimulate(), ms);
}

function scheduleHangoutLiveRecompute() {
  scheduleHangoutSimulateDebounced(280);
}

function onHangoutLiveFieldChanged() {
  hangoutZeroYdsIfGreenLie();
  scheduleHangoutSimulateDebounced();
}

/** Stack so nested simulate calls (shouldn't happen) restore prior RNG. */
const _hangoutRngStack = [];
let _hangoutRngImpl = null;

function hangoutRngU01() {
  if (typeof _hangoutRngImpl === "function") return _hangoutRngImpl();
  return Math.random();
}

function hangoutPushRngState(rngFn) {
  _hangoutRngStack.push(_hangoutRngImpl);
  _hangoutRngImpl = rngFn;
}

function hangoutPopRngState() {
  _hangoutRngImpl = _hangoutRngStack.pop() ?? null;
}

function makeMulberry32(a) {
  return function mulberry32() {
    let t = (a += 0x6d2b79f5);
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function hangoutFnv1aHash(str) {
  let h = 2166136261 >>> 0;
  for (let i = 0; i < str.length; i++) {
    h ^= str.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
}

function hangoutSelectedPlayerWeatherSeedFragment() {
  const pid = document.getElementById("hh-player")?.value || "";
  const id = Math.round(num(pid, NaN));
  const row = Number.isFinite(id) ? projectionPlayerRowForModel(id, getOuRound()) : null;
  const w = effectiveWeatherForProjectionRow(row || {});
  return `${w.tempF}|${w.windMph}|${w.humidityPct}|${w.condition}`;
}

function buildHangoutSimSeedKey(hpars, holeIdx) {
  const r = getOuRound();
  const hole = String(holeIdx + 1);
  const par = hpars[holeIdx] ?? 4;
  const pid = document.getElementById("hh-player")?.value || "";
  const w = hangoutSelectedPlayerWeatherSeedFragment();
  const pr = `${PRICING_STATE.mode}|${PRICING_STATE.skill}`;
  const live = hangoutLiveOn()
    ? `L|${document.getElementById("hh-shot-num")?.value}|${document.getElementById("hh-dist-yds")?.value}|${document.getElementById("hh-lie")?.value}|${document.getElementById("hh-putt-ft")?.value}`
    : "N";
  const meta = `${DATA.meta?.event_name || ""}|${DATA.meta?.course_used || ""}`;
  return `${meta}|R${r}|H${hole}|P${par}|DG${pid}|${w}|${pr}|${live}`;
}

/** Last 3-way hole result model for Prob/Price toggle without re-rolling. */
let hangoutLastThreeProbs = null;

/** PGA Tour putting table (approx. tour-wide); distances in feet. */
const PGA_PUTT_DISTANCE_FT = [2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 40];
const PGA_PUTT_ONE_PUTT = [0.99, 0.96, 0.88, 0.77, 0.66, 0.58, 0.5, 0.45, 0.4, 0.23, 0.15, 0.07, 0.04];
const PGA_PUTT_THREE_PUTT = [0, 0.001, 0.003, 0.004, 0.004, 0.005, 0.006, 0.007, 0.007, 0.013, 0.022, 0.05, 0.1];
const PGA_PUTT_TOUR_AVG = [1.01, 1.04, 1.13, 1.23, 1.34, 1.42, 1.5, 1.56, 1.61, 1.78, 1.87, 1.98, 2.06];

function interpPgaPuttingSeries(ft, distArr, valArr) {
  const x = clamp(ft, 2, 40);
  if (x <= distArr[0]) return valArr[0];
  const n = distArr.length;
  if (x >= distArr[n - 1]) return valArr[n - 1];
  for (let i = 0; i < n - 1; i++) {
    if (x <= distArr[i + 1]) {
      const lo = distArr[i];
      const hi = distArr[i + 1];
      const t = (x - lo) / (hi - lo);
      return valArr[i] + t * (valArr[i + 1] - valArr[i]);
    }
  }
  return valArr[n - 1];
}

function hangoutNormThree(o) {
  const s = num(o.birdie, 0) + num(o.par, 0) + num(o.bogeyPlus, 0);
  if (s < 1e-12) return { birdie: 1 / 3, par: 1 / 3, bogeyPlus: 1 / 3 };
  return { birdie: o.birdie / s, par: o.par / s, bogeyPlus: o.bogeyPlus / s };
}

function hangoutCollapseFiveToThree(p5) {
  return {
    birdie: num(p5.eagle, 0) + num(p5.birdie, 0),
    par: num(p5.par, 0),
    bogeyPlus: num(p5.bogey, 0) + num(p5.double, 0),
  };
}

function hangoutRemoveHoleOutMass(probsFive, holePar) {
  if (Math.round(num(holePar, 4)) >= 5) return probsFive;
  const eagle = Math.max(0, num(probsFive.eagle, 0));
  if (eagle <= 0) return probsFive;
  return {
    ...probsFive,
    eagle: 0,
    par: num(probsFive.par, 0) + eagle,
  };
}

function hangoutBlendThree(a, b, wHist) {
  const w = clamp(num(wHist, 0), 0, 0.85);
  return hangoutNormThree({
    birdie: (1 - w) * a.birdie + w * b.birdie,
    par: (1 - w) * a.par + w * b.par,
    bogeyPlus: (1 - w) * a.bogeyPlus + w * b.bogeyPlus,
  });
}

function hangoutHistoryPriorThree(dgId, courseUsed, eventName) {
  const rec = HISTORY.byDgId && HISTORY.byDgId[String(dgId)];
  if (!rec || !Array.isArray(rec.rounds) || !rec.rounds.length) return null;
  const needle = String(courseUsed || "").trim().toLowerCase();
  const evN = String(eventName || "").trim().toLowerCase();
  const byVenue = (arr) =>
    needle
      ? arr.filter((r) => {
          const c = String(r.course_name || "").trim().toLowerCase();
          return c && (c.includes(needle) || needle.includes(c.slice(0, Math.min(10, c.length))));
        })
      : arr.slice(0, 120);
  const byEvent = (arr) => {
    if (!evN || !arr.length) return arr;
    const evF = arr.filter((r) => {
      const en = String(r.event_name || "").trim().toLowerCase();
      return en && (en.includes(evN.slice(0, 14)) || evN.includes(en.slice(0, 10)));
    });
    return evF.length ? evF : arr;
  };
  const sortedCareer = () =>
    rec.rounds
      .filter((r) => r && !historyRoundIsPlaceholderAllMarketsZero(r))
      .sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a))
      .slice(0, 100);
  let rounds = byEvent(byVenue(rec.rounds));
  /* New venue / rookies: no prior rounds on this course — use same-event anywhere, then recent career. */
  if (!rounds.length && needle && evN) {
    rounds = byEvent(rec.rounds.filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r)));
  }
  if (!rounds.length && needle) {
    rounds = sortedCareer();
  }
  if (!rounds.length) return null;
  let b = 0;
  let p = 0;
  let w = 0;
  for (const r of rounds) {
    b += num(r.birdies, 0) + num(r.eagles_or_better, 0);
    p += num(r.pars, 0);
    w += num(r.bogies, 0) + num(r.doubles_or_worse, 0);
  }
  const t = b + p + w;
  if (t < 1) return null;
  return hangoutNormThree({ birdie: b / t, par: p / t, bogeyPlus: w / t });
}

function hangoutHoleHistoryPriorThree(dgId, courseUsed, eventName, holeNum, holePar) {
  const rec = HISTORY.byDgId && HISTORY.byDgId[String(dgId)];
  if (!rec || !Array.isArray(rec.rounds) || !rec.rounds.length) return null;
  const pname = String(rec.player_name || "").trim();
  const pkey = playerKeyFromName(pname);
  const holesMap = pkey && HISTORY.holesByPlayerKey ? HISTORY.holesByPlayerKey[pkey] : null;
  if (!holesMap || typeof holesMap !== "object") return null;
  const hWant = Math.round(num(holeNum, NaN));
  const pWant = Math.round(num(holePar, NaN));
  if (!Number.isFinite(hWant) || hWant < 1 || hWant > 18) return null;
  const courseNeedle = String(courseUsed || "").trim();
  const eventNeedle = String(eventName || "").trim();
  const byRound = new Map();
  for (const r of rec.rounds) {
    const rn = Math.round(num(r?.round_num, NaN));
    if (!Number.isFinite(rn)) continue;
    const k = `${normEvtNameKey(r?.event_name)}\tR${rn}`;
    if (!byRound.has(k)) byRound.set(k, []);
    byRound.get(k).push(r);
  }
  const tiers = [
    { birdie: 0, par: 0, bogeyPlus: 0, n: 0 },
    { birdie: 0, par: 0, bogeyPlus: 0, n: 0 },
    { birdie: 0, par: 0, bogeyPlus: 0, n: 0 },
    { birdie: 0, par: 0, bogeyPlus: 0, n: 0 },
  ];
  for (const [fullKey, holes] of Object.entries(holesMap)) {
    if (!Array.isArray(holes)) continue;
    const hit = holes.find((h) => Math.round(num(h?.hole, NaN)) === hWant);
    if (!hit) continue;
    const hPar = Math.round(num(hit.par, NaN));
    const sc = Math.round(num(hit.score, NaN));
    if (!Number.isFinite(hPar) || !Number.isFinite(sc)) continue;
    if (Number.isFinite(pWant) && pWant >= 3 && pWant <= 5 && hPar !== pWant) continue;
    const parts = String(fullKey).split("\t");
    const evRaw = (parts[0] || "").trim();
    const rn = parseRoundNumFromRLabel(parts[1] || "");
    const metaRows = byRound.get(`${normEvtNameKey(evRaw)}\tR${rn}`) || [];
    const courseHit = courseNeedle && metaRows.some((r) => courseNameMatchesVenueLoose(r?.course_name, courseNeedle));
    const eventHit =
      eventNeedle &&
      (eventNameMatchesCurrentSchedule(evRaw, eventNeedle) ||
        metaRows.some((r) => eventNameMatchesCurrentSchedule(r?.event_name, eventNeedle)));
    const tier = courseHit && eventHit ? 0 : courseHit ? 1 : eventHit ? 2 : 3;
    const rel = sc - hPar;
    if (rel <= -2 && hPar < 5) tiers[tier].par += 1;
    else if (rel <= -1) tiers[tier].birdie += 1;
    else if (rel === 0) tiers[tier].par += 1;
    else tiers[tier].bogeyPlus += 1;
    tiers[tier].n += 1;
  }
  const idx = tiers.findIndex((t) => t.n > 0);
  if (idx < 0) return null;
  const picked = tiers[idx];
  const cap = [0.72, 0.66, 0.56, 0.46][idx] || 0.46;
  const weight = clamp(picked.n / (picked.n + 5), 0.14, cap);
  return {
    probs: hangoutNormThree(picked),
    n: picked.n,
    tier: idx,
    weight,
  };
}

/**
 * Birdie / Par / Bogey+ from PGA make and 3-putt rates at `puttFt`, given hole par and live shot #.
 */
function hangoutPuttingThreeWay(holePar, shotNum, puttFt) {
  const ft = clamp(num(puttFt, 10), 2, 40);
  const o1 = interpPgaPuttingSeries(ft, PGA_PUTT_DISTANCE_FT, PGA_PUTT_ONE_PUTT);
  const t3 = interpPgaPuttingSeries(ft, PGA_PUTT_DISTANCE_FT, PGA_PUTT_THREE_PUTT);
  const s = Math.round(num(shotNum, 1));
  if (s === holePar - 1) {
    const birdie = o1;
    const bogeyPlus = t3;
    const par = Math.max(0, 1 - birdie - bogeyPlus);
    return hangoutNormThree({ birdie, par, bogeyPlus });
  }
  if (s === holePar) {
    return hangoutNormThree({ birdie: 0, par: Math.max(0, 1 - t3), bogeyPlus: t3 });
  }
  if (s >= holePar + 1) {
    return hangoutNormThree({ birdie: 0, par: o1 * 0.82, bogeyPlus: Math.max(0, 1 - o1 * 0.82) });
  }
  if (s <= holePar - 2) {
    const birdie = o1 + Math.max(0, 1 - o1 - t3) * 0.55;
    const bogeyPlus = t3;
    const par = Math.max(0, 1 - birdie - bogeyPlus);
    return hangoutNormThree({ birdie, par, bogeyPlus });
  }
  return hangoutNormThree({ birdie: o1 * 0.45, par: Math.max(0, 1 - o1 * 0.45 - t3), bogeyPlus: t3 });
}

function liveRoughFiveMults() {
  if (!hangoutLiveOn()) return { eagle: 1, birdie: 1, par: 1, bogey: 1, double: 1 };
  const lie = String(document.getElementById("hh-lie")?.value || "Fairway");
  if (lie === "Green") return { eagle: 1, birdie: 1, par: 1, bogey: 1, double: 1 };
  const dist = num(document.getElementById("hh-dist-yds")?.value, 150);
  const putt = num(document.getElementById("hh-putt-ft")?.value, 15);
  const shotN = clamp(Math.round(num(document.getElementById("hh-shot-num")?.value, 1)), 1, 18);
  const phase = clamp((shotN - 1) / 7, 0, 1);
  let lieT = 0;
  if (lie === "Sand") lieT = 0.62;
  else if (lie === "Rough") lieT = 0.45;
  let distT = clamp((dist - 132) / 340, -0.1, 0.2) * (1 - 0.3 * phase);
  /* Short-sided rough/sand: do not treat “few yards left” as easy — keep difficulty positive. */
  if (lie === "Rough" || lie === "Sand") {
    const short = Number.isFinite(dist) && dist >= 0 && dist < 95;
    if (short) {
      const bump = clamp((95 - dist) / 95, 0, 1) * (lie === "Sand" ? 0.12 : 0.08);
      distT = Math.max(distT, 0.05 + bump);
    } else if (distT < 0.04) distT = 0.04;
  }
  const puttT = clamp((putt - 9) / 48, -0.08, 0.16) * (0.35 + 0.65 * phase);
  const T = clamp(lieT + distT + puttT, -0.08, 1.28);
  return {
    eagle: clamp(1 - 0.54 * T, 0.25, 1.08),
    birdie: clamp(1 - 0.4 * T, 0.36, 1.1),
    par: clamp(1 + 0.045 * T, 0.93, 1.16),
    bogey: clamp(1 + 0.44 * T, 0.88, 1.88),
    double: clamp(1 + 0.66 * T, 0.68, 2.15),
  };
}

/** When live inputs are on and not putting, tilt birdie/par/bogey+ toward trouble in rough/sand vs fairway. */
function hangoutLiveLieThreeWayTilt(three, lieRaw, shotNum, distYds) {
  const lie = String(lieRaw || "Fairway");
  if (lie === "Green" || lie === "Fairway") return three;
  const sn = clamp(Math.round(num(shotNum, 1)), 1, 18);
  const late = clamp((sn - 1) / 6.5, 0, 1);
  let u = lie === "Sand" ? 0.22 : 0.14;
  u += late * (lie === "Sand" ? 0.09 : 0.06);
  const d = num(distYds, NaN);
  if (Number.isFinite(d) && d >= 0 && d < 90) u += 0.045 * (1 - d / 90);
  u = clamp(u, 0, 0.38);
  return hangoutNormThree({
    birdie: three.birdie * Math.exp(-u),
    par: three.par,
    bogeyPlus: three.bogeyPlus * Math.exp(u),
  });
}

function hangoutAmericanForThreeWayProb(p) {
  const fair = 1 / clamp(p, 0.055, 0.965);
  const d = Math.max(1.02, fair * (1 + OU_HOLD * 0.45));
  return americanFromDecimal(d);
}

/** Blend toward a mild hole prior so birdie / bogey+ rarely sit at extreme longshot prices together. */
function hangoutWidenThreeWayForPrices(three, liveGreen) {
  const w = liveGreen ? 0.09 : 0.15;
  const a = liveGreen
    ? { birdie: 0.2, par: 0.6, bogeyPlus: 0.2 }
    : { birdie: 0.24, par: 0.46, bogeyPlus: 0.3 };
  return hangoutNormThree({
    birdie: (1 - w) * three.birdie + w * a.birdie,
    par: (1 - w) * three.par + w * a.par,
    bogeyPlus: (1 - w) * three.bogeyPlus + w * a.bogeyPlus,
  });
}

function hangoutRenderThreeOutcomes(p3) {
  const rows = document.getElementById("hh-outcomes-rows");
  const tb = document.getElementById("hh-out-toolbar");
  if (tb) tb.hidden = false;
  if (!rows) return;
  const priceOn = document.getElementById("hh-odds-mode-price")?.classList.contains("active");
  const order = [
    ["birdie", "Birdie"],
    ["par", "Par"],
    ["bogeyPlus", "Bogey or worse"],
  ];
  rows.innerHTML = "";
  let mi = 0;
  for (const [key, lab] of order) {
    const pv = p3[key] || 0;
    const rowEl = document.createElement("div");
    rowEl.className = "hole-sim-outcome-row";
    const pct = (pv * 100).toFixed(1);
    const am = priceOn ? hangoutAmericanForThreeWayProb(pv) : NaN;
    const right = priceOn && Number.isFinite(am) ? formatAmerican(am) : `${pct}%`;
    rowEl.innerHTML = `<span style="min-width:132px">${lab}</span><div class="hole-sim-outcome-bar"><div class="hole-sim-outcome-fill" style="width:${pct}%;background:${hangoutPalette[mi % hangoutPalette.length]}"></div></div><span class="num">${right}</span>`;
    rows.appendChild(rowEl);
    mi++;
  }
}

function setHangoutOddsViewMode(price) {
  const prob = document.getElementById("hh-odds-mode-prob");
  const prc = document.getElementById("hh-odds-mode-price");
  if (prob) {
    prob.classList.toggle("active", !price);
    prob.setAttribute("aria-selected", (!price).toString());
  }
  if (prc) {
    prc.classList.toggle("active", price);
    prc.setAttribute("aria-selected", price.toString());
  }
  if (hangoutLastThreeProbs) hangoutRenderThreeOutcomes(hangoutLastThreeProbs);
}

function hangoutBuildShotsFromBundleOrSynth(holePar, sc, dgId, holeNum1) {
  void dgId;
  void holeNum1;
  /* Hole Hangout always uses scripted shot lines. Real shot bundles (player_shots_web.json) can replay
   * eagles as “approach + hole” and bypass the no–hole-out wording; pricing already avoids that mass. */
  return hangoutBuildShots(holePar, sc);
}

function hangoutZeroYdsIfGreenLie() {
  const lie = String(document.getElementById("hh-lie")?.value || "");
  const di = document.getElementById("hh-dist-yds");
  if (lie === "Green" && di) di.value = "0";
}

/** Official Augusta National 18-hole par layout (par 72). Used when meta is still generic. */
const AUGUSTA_NATIONAL_HOLE_PARS = Object.freeze([4, 5, 4, 3, 4, 3, 4, 5, 4, 4, 4, 3, 5, 4, 5, 3, 4, 4]);
/** TPC Louisiana (Zurich Classic host) — PGA Tour scorecard; used when hole_pars_source is still generic. */
const TPC_LOUISIANA_HOLE_PARS = Object.freeze([4, 5, 3, 4, 4, 4, 5, 4, 3, 4, 5, 4, 4, 3, 4, 4, 3, 5]);

function normalizeHoleParsClient(hp) {
  if (!Array.isArray(hp) || hp.length < 18) return null;
  const first = hp[0];
  if (first && typeof first === "object" && !Array.isArray(first)) {
    const h0 = num(first.hole ?? first.hole_number ?? first.hole_num ?? first.num, NaN);
    if (Number.isFinite(h0)) {
      const byHole = new Map();
      for (const x of hp) {
        if (!x || typeof x !== "object") continue;
        const h = Math.round(num(x.hole ?? x.hole_number ?? x.hole_num ?? x.num, NaN));
        const p = Math.round(num(x.par ?? x.par_hole ?? x.hole_par, NaN));
        if (h >= 1 && h <= 18 && p >= 3 && p <= 5) byHole.set(h, p);
      }
      if (byHole.size >= 18) {
        const arr = [];
        for (let h = 1; h <= 18; h++) {
          if (!byHole.has(h)) return null;
          arr.push(byHole.get(h));
        }
        return arr;
      }
    }
  }
  const arr = hp.slice(0, 18).map((x) => Math.round(num(x, 4)));
  if (!arr.every((n) => n >= 3 && n <= 5)) return null;
  return arr;
}

function parseHoleParsMeta() {
  const vn = String(DATA.meta?.course_used || "").trim().toLowerCase();
  const ev = String(DATA.meta?.event_name || "").trim().toLowerCase();
  const augustaContext = vn.includes("augusta") || ev.includes("masters");
  const zurichTpcContext =
    vn.includes("tpc louisiana") || ev.includes("zurich classic") || ev.includes("new orleans");
  const src = String(DATA.meta?.hole_pars_source || "").toLowerCase();
  if (augustaContext && src === "generic") return [...AUGUSTA_NATIONAL_HOLE_PARS];
  if (zurichTpcContext && src === "generic") return [...TPC_LOUISIANA_HOLE_PARS];

  const hp = DATA.meta.hole_pars;
  const normalized = normalizeHoleParsClient(hp);
  if (normalized) return normalized;
  if (augustaContext) return [...AUGUSTA_NATIONAL_HOLE_PARS];
  return Array.from({ length: 18 }, () => 4);
}

/** Wipe prior simulate output so round/player/hole changes cannot show stale odds or paths. */
function clearHangoutSimulationResults() {
  const top = document.getElementById("hh-top-bar");
  const pOutRows = document.getElementById("hh-outcomes-rows");
  const outToolbar = document.getElementById("hh-out-toolbar");
  const pSeq = document.getElementById("hh-panel-sequence");
  const hadResults = top && !top.hidden;
  hangoutLastThreeProbs = null;
  hangoutCanvasShotCount = 0;
  if (outToolbar) outToolbar.hidden = true;
  if (top) {
    top.hidden = true;
    top.innerHTML = "";
  }
  if (pOutRows) {
    pOutRows.innerHTML = hadResults
      ? '<p class="text-muted hangout-cleared-msg">Results cleared — change a control or use <strong>Run simulation</strong>.</p>'
      : "";
  }
  const viz = document.getElementById("hh-hole-viz");
  const cv = document.getElementById("hh-hole-canvas");
  const holeCard = document.getElementById("hh-hole-card-body");
  if (viz) viz.hidden = true;
  if (cv && cv.getContext) {
    const ctx = cv.getContext("2d");
    if (ctx) ctx.clearRect(0, 0, cv.width, cv.height);
  }
  if (holeCard) holeCard.innerHTML = "";
  if (pSeq) pSeq.innerHTML = "";
}

function initHangoutSelectors(resetHole) {
  const hp = parseHoleParsMeta();
  const holeSel = document.getElementById("hh-hole");
  const plSel = document.getElementById("hh-player");
  if (holeSel) {
    const prev = holeSel.value;
    holeSel.innerHTML = "";
    for (let i = 0; i < 18; i++) {
      const o = document.createElement("option");
      o.value = String(i + 1);
      o.textContent = `Hole ${i + 1} (par ${hp[i]})`;
      holeSel.appendChild(o);
    }
    if (!resetHole && prev && num(prev, 0) >= 1 && num(prev, 0) <= 18) holeSel.value = prev;
    else holeSel.value = "1";
  }
  if (plSel) {
    const pr = plSel.value;
    plSel.innerHTML = "";
    const seen = new Set();
    for (const p of DATA.players) {
      if (!samePlayerRound(p, getOuRound())) continue;
      if (tournamentPostCutListPhase() && isPlayerEliminatedFromEvent(p)) continue;
      const id = Math.round(num(p.dg_id, NaN));
      if (!Number.isFinite(id) || seen.has(id)) continue;
      seen.add(id);
      const o = document.createElement("option");
      o.value = String(id);
      o.textContent = displayGolferName(String(p.player_name || ""));
      plSel.appendChild(o);
    }
    if (pr && [...plSel.options].some((o) => o.value === pr)) plSel.value = pr;
    else if (plSel.options.length) plSel.selectedIndex = 0;
    refreshGolferComboboxFromSelect("hh-player");
  }
}

function getHangoutPlayerRow() {
  const sel = document.getElementById("hh-player");
  const id = sel ? Math.round(num(sel.value, NaN)) : NaN;
  const r = getOuRound();
  return DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id && samePlayerRound(p, r));
}

function selectedHangoutDgId() {
  const sel = document.getElementById("hh-player");
  const id = sel ? Math.round(num(sel.value, NaN)) : NaN;
  return Number.isFinite(id) ? id : NaN;
}

function scoreMixFromProjection(row) {
  const e = num(row?.eagles, 0);
  const b = num(row?.birdies, 0);
  const p = num(row?.pars, 0);
  const g = num(row?.bogeys, 0);
  const d = num(row?.doubles, 0);
  let s = e + b + p + g + d;
  if (s < 0.01) s = 1;
  return {
    eagle: e / s,
    birdie: b / s,
    par: p / s,
    bogey: g / s,
    double: d / s,
  };
}

function parTilt(holePar, holeParsArr) {
  const counts = { 3: 0, 4: 0, 5: 0 };
  for (const x of holeParsArr) {
    const v = Math.round(num(x, 4));
    if (counts[v] !== undefined) counts[v]++;
  }
  const tot = counts[3] + counts[4] + counts[5] || 1;
  const ref = { 3: counts[3] / tot, 4: counts[4] / tot, 5: counts[5] / tot };
  const hp = Math.round(num(holePar, 4));
  const bump = { 3: 1.15, 4: 1, 5: 0.92 };
  const t = bump[hp] || 1;
  const base = ref[hp] || 0.33;
  return t / (base * 3 + 1e-6);
}

const HANGOUT_FIVE_ORDER = ["eagle", "birdie", "par", "bogey", "double"];

/**
 * Log-odds tilt for hangout hole outcomes from pricing mode + weather (field difficulty
 * and skill×weather), consistent with O/U pricing and weather adjustments.
 */
function hangoutOutcomeDistributionT(row, dgId) {
  const id = Math.round(num(dgId, NaN));
  const pBonus = Number.isFinite(id) ? pricingModeMuSgBonus(id) : 0;
  const tPrice =
    Number.isFinite(pBonus) && Math.abs(pBonus) > 1e-12 ? clamp(pBonus * 0.22, -0.09, 0.09) : 0;
  const d = weatherDifficultyDeltaFromSnapshot(effectiveWeatherForProjectionRow(row));
  const tWeatherBase = clamp(-num(d, 0) * 0.055, -0.09, 0.09);
  const wEdge = playerSkillWeatherEdge(row);
  const tWeatherSkill =
    Number.isFinite(wEdge) && Math.abs(wEdge) > 1e-12 ? clamp(wEdge * 0.2, -0.09, 0.09) : 0;
  return clamp(tPrice + tWeatherBase + tWeatherSkill, -0.2, 0.2);
}

/** Renormalize eagle..double mix after tilt so Prob bars, expected score, and sampled score align. */
function hangoutTiltProbsFive(probsFive, t) {
  if (!Number.isFinite(t) || Math.abs(t) < 1e-12) return probsFive;
  const mult = {
    eagle: Math.exp(t * 1.22),
    birdie: Math.exp(t * 1.02),
    par: Math.exp(t * 0.12),
    bogey: Math.exp(-t * 0.88),
    double: Math.exp(-t * 1.12),
  };
  let s = 0;
  const out = {};
  for (const k of HANGOUT_FIVE_ORDER) {
    const v = (probsFive[k] || 0) * mult[k];
    out[k] = v;
    s += v;
  }
  if (s < 1e-15) return probsFive;
  for (const k of HANGOUT_FIVE_ORDER) out[k] /= s;
  return out;
}

function hangoutLiveOn() {
  return Boolean(document.getElementById("hh-use-live")?.checked);
}

function hangoutRi(lo, hi) {
  return Math.round(lo + hangoutRngU01() * (hi - lo));
}

function hangoutScoreLabel(holePar, sc) {
  const d = sc - holePar;
  if (d <= -3) return "Double eagle+";
  if (d === -2) return "Eagle";
  if (d === -1) return "Birdie";
  if (d === 0) return "Par";
  return "Bogey+";
}

function hangoutApplyLiveToShot(index1, shot) {
  if (!hangoutLiveOn()) return shot;
  const n = clamp(Math.round(num(document.getElementById("hh-shot-num")?.value, 1)), 1, 18);
  if (index1 !== n) return shot;
  const distEl = document.getElementById("hh-dist-yds");
  const puttEl = document.getElementById("hh-putt-ft");
  const distRaw = distEl && String(distEl.value).trim() !== "" ? num(distEl.value, NaN) : NaN;
  const puttRaw = puttEl && String(puttEl.value).trim() !== "" ? num(puttEl.value, NaN) : NaN;
  const lie = String(document.getElementById("hh-lie")?.value || "Fairway");
  if (/putt/i.test(shot.title)) {
    const feet = Number.isFinite(puttRaw) ? Math.round(puttRaw) : shot.feet;
    return { ...shot, feet, yards: null, lie: "Green" };
  }
  if (lie === "Green") {
    return { ...shot, yards: 0, feet: null, lie: "Green" };
  }
  const yards = Number.isFinite(distRaw) ? Math.round(distRaw) : shot.yards;
  return { ...shot, yards, lie };
}

function hangoutShotsPar3(sc) {
  const tee = hangoutRi(165, 232);
  if (sc <= 1) {
    return [{ title: "Tee shot", yards: tee, lie: "Green" }];
  }
  if (sc === 2) {
    const onG = hangoutRngU01() < 0.48;
    return [
      { title: "Tee shot", yards: tee, lie: onG ? "Green" : "Fairway" },
      { title: "Putt", feet: hangoutRi(7, 22), tag: "Birdie" },
    ];
  }
  if (sc === 3) {
    if (hangoutRngU01() < 0.42) {
      return [
        { title: "Tee shot", yards: tee, lie: "Green" },
        { title: "Putt", feet: hangoutRi(22, 38), lie: "" },
        { title: "Putt", feet: hangoutRi(2, 5), tag: "Par" },
      ];
    }
    return [
      { title: "Tee shot", yards: tee, lie: "Rough" },
      { title: "Chip", yards: hangoutRi(22, 42), lie: "Green" },
      { title: "Putt", feet: hangoutRi(6, 16), tag: "Par" },
    ];
  }
  if (sc === 4) {
    return [
      { title: "Tee shot", yards: tee, lie: "Rough" },
      { title: "Approach", yards: hangoutRi(95, 135), lie: "Green" },
      { title: "Putt", feet: hangoutRi(24, 40), lie: "" },
      { title: "Putt", feet: hangoutRi(2, 5), tag: "Bogey" },
    ];
  }
  return [
    { title: "Tee shot", yards: tee, lie: "Rough" },
    { title: "Layup", yards: hangoutRi(88, 125), lie: "Fairway" },
    { title: "Pitch", yards: hangoutRi(38, 58), lie: "Green" },
    { title: "Putt", feet: hangoutRi(18, 32), lie: "" },
    { title: "Putt", feet: hangoutRi(2, 5), tag: "Double+" },
  ].slice(0, sc);
}

function hangoutShotsPar4(sc) {
  const tee = hangoutRi(285, 348);
  if (sc <= 2) {
    return [
      { title: "Tee shot", yards: tee, lie: hangoutRngU01() < 0.24 ? "Rough" : "Fairway" },
      { title: "Approach", yards: hangoutRi(86, 154), lie: "Green" },
    ];
  }
  if (sc === 3) {
    return [
      { title: "Tee shot", yards: tee, lie: "Fairway" },
      { title: "Approach", yards: hangoutRi(95, 155), lie: "Green" },
      { title: "Putt", feet: hangoutRi(8, 20), tag: "Birdie" },
    ];
  }
  if (sc === 4) {
    return [
      { title: "Tee shot", yards: tee, lie: hangoutRngU01() < 0.22 ? "Rough" : "Fairway" },
      { title: "Approach", yards: hangoutRi(118, 168), lie: "Green" },
      { title: "Putt", feet: hangoutRi(22, 38), lie: "" },
      { title: "Putt", feet: hangoutRi(2, 5), tag: "Par" },
    ];
  }
  if (sc === 5) {
    return [
      { title: "Tee shot", yards: tee, lie: "Rough" },
      { title: "Approach", yards: hangoutRi(135, 175), lie: "Fairway" },
      { title: "Chip", yards: hangoutRi(28, 48), lie: "Green" },
      { title: "Putt", feet: hangoutRi(16, 28), lie: "" },
      { title: "Putt", feet: hangoutRi(2, 5), tag: "Bogey" },
    ];
  }
  return [
    { title: "Tee shot", yards: tee, lie: "Rough" },
    { title: "Layup", yards: hangoutRi(165, 205), lie: "Fairway" },
    { title: "Pitch", yards: hangoutRi(48, 72), lie: "Green" },
    { title: "Putt", feet: hangoutRi(22, 36), lie: "" },
    { title: "Putt", feet: hangoutRi(4, 9), lie: "" },
    { title: "Putt", feet: hangoutRi(2, 4), tag: "Double+" },
  ].slice(0, sc);
}

function hangoutShotsPar5(sc) {
  const tee = hangoutRi(286, 345);
  if (sc <= 3) {
    return [
      { title: "Tee shot", yards: tee, lie: "Fairway" },
      { title: "Second", yards: hangoutRi(205, 275), lie: "Green" },
      { title: "Putt", feet: hangoutRi(6, 22), tag: "Eagle" },
    ];
  }
  if (sc === 4) {
    return [
      { title: "Tee shot", yards: tee, lie: "Fairway" },
      { title: "Second", yards: hangoutRi(210, 270), lie: "Green" },
      { title: "Putt", feet: hangoutRi(20, 42), lie: "" },
      { title: "Putt", feet: hangoutRi(10, 22), tag: "Birdie" },
    ];
  }
  if (sc === 5) {
    return [
      { title: "Tee shot", yards: tee, lie: hangoutRngU01() < 0.18 ? "Rough" : "Fairway" },
      { title: "Layup", yards: hangoutRi(170, 235), lie: "Fairway" },
      { title: "Approach", yards: hangoutRi(105, 148), lie: "Green" },
      { title: "Putt", feet: hangoutRi(22, 36), lie: "" },
      { title: "Putt", feet: hangoutRi(2, 5), tag: "Par" },
    ];
  }
  if (sc === 6) {
    return [
      { title: "Tee shot", yards: tee, lie: "Fairway" },
      { title: "Second", yards: hangoutRi(185, 245), lie: "Rough" },
      { title: "Approach", yards: hangoutRi(128, 168), lie: "Green" },
      { title: "Putt", feet: hangoutRi(26, 40), lie: "" },
      { title: "Putt", feet: hangoutRi(4, 9), lie: "" },
      { title: "Putt", feet: hangoutRi(2, 4), tag: "Bogey" },
    ];
  }
  return [
    { title: "Tee shot", yards: tee, lie: "Rough" },
    { title: "Layup", yards: hangoutRi(160, 225), lie: "Fairway" },
    { title: "Approach", yards: hangoutRi(145, 185), lie: "Rough" },
    { title: "Chip", yards: hangoutRi(32, 52), lie: "Green" },
    { title: "Putt", feet: hangoutRi(20, 34), lie: "" },
    { title: "Putt", feet: hangoutRi(4, 10), lie: "" },
    { title: "Putt", feet: hangoutRi(2, 4), tag: "Double+" },
  ].slice(0, sc);
}

function hangoutFallbackShots(holePar, sc) {
  const out = [];
  const tee = hangoutRi(270, 345);
  out.push({ title: "Tee shot", yards: tee, lie: "Fairway" });
  let rem = sc - 1;
  let d = hangoutRi(130, 175);
  while (rem > 2) {
    out.push({
      title: "Approach",
      yards: d,
      lie: hangoutRngU01() < 0.2 ? "Rough" : "Green",
    });
    d = Math.max(35, d - hangoutRi(25, 55));
    rem--;
  }
  while (rem > 1) {
    out.push({ title: "Putt", feet: hangoutRi(16, 36), lie: "" });
    rem--;
  }
  if (rem === 1) {
    out.push({ title: "Putt", feet: hangoutRi(2, 6), tag: hangoutScoreLabel(holePar, sc) });
  }
  return out.slice(0, sc);
}

function hangoutBuildShots(holePar, sc) {
  let p = Math.round(num(holePar, 4));
  if (p < 3) p = 3;
  if (p > 5) p = 5;
  let shots;
  if (p === 3) shots = hangoutShotsPar3(sc);
  else if (p === 4) shots = hangoutShotsPar4(sc);
  else shots = hangoutShotsPar5(sc);
  if (!Array.isArray(shots) || shots.length !== sc) shots = hangoutFallbackShots(holePar, sc);
  return shots.map((s, i) => hangoutApplyLiveToShot(i + 1, { ...s }));
}

function hangoutBezierPoint(t, w, h) {
  const teeX = w * 0.48;
  const teeY = h * 0.86;
  const pinX = w * 0.52;
  const pinY = h * 0.12;
  const cx = w * 0.78;
  const cy = h * 0.38;
  const omt = 1 - t;
  const x = omt * omt * teeX + 2 * omt * t * cx + t * t * pinX;
  const y = omt * omt * teeY + 2 * omt * t * cy + t * t * pinY;
  return { x, y };
}

function drawHangoutHoleCanvas(canvas, numShots) {
  if (!canvas || !canvas.getContext || numShots < 1) return;
  const dpr = Math.min(2, window.devicePixelRatio || 1);
  const wrap = canvas.parentElement;
  let cssW = wrap ? wrap.clientWidth : canvas.clientWidth;
  if (!Number.isFinite(cssW) || cssW < 120) cssW = 400;
  const cssH = Math.round((cssW * 320) / 560) || 200;
  canvas.width = Math.round(cssW * dpr);
  canvas.height = Math.round(cssH * dpr);
  const ctx = canvas.getContext("2d");
  if (!ctx) return;
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  const w = cssW;
  const h = cssH;
  ctx.fillStyle = "#0a0c0f";
  ctx.fillRect(0, 0, w, h);
  ctx.lineCap = "round";
  ctx.lineJoin = "round";
  ctx.strokeStyle = "rgba(38, 95, 58, 0.4)";
  ctx.lineWidth = Math.min(w, h) * 0.34;
  ctx.beginPath();
  for (let t = 0; t <= 1.001; t += 0.03) {
    const { x, y } = hangoutBezierPoint(Math.min(1, t), w, h);
    if (t === 0) ctx.moveTo(x, y);
    else ctx.lineTo(x, y);
  }
  ctx.stroke();
  ctx.strokeStyle = "rgba(52, 130, 78, 0.55)";
  ctx.lineWidth = Math.min(w, h) * 0.12;
  ctx.stroke();
  const pin = hangoutBezierPoint(1, w, h);
  ctx.fillStyle = "rgba(42, 115, 72, 0.6)";
  ctx.beginPath();
  ctx.arc(pin.x, pin.y, Math.min(w, h) * 0.1, 0, Math.PI * 2);
  ctx.fill();
  const pts = [];
  for (let i = 0; i < numShots; i++) {
    const t = (i + 1) / (numShots + 0.35);
    pts.push(hangoutBezierPoint(Math.min(0.985, t), w, h));
  }
  ctx.strokeStyle = "rgba(255,255,255,0.88)";
  ctx.lineWidth = 2.25;
  ctx.beginPath();
  ctx.moveTo(pts[0].x, pts[0].y);
  for (let i = 1; i < pts.length; i++) ctx.lineTo(pts[i].x, pts[i].y);
  ctx.stroke();
  pts.forEach((pt, i) => {
    const col = hangoutPalette[i % hangoutPalette.length];
    ctx.fillStyle = col;
    ctx.beginPath();
    ctx.arc(pt.x, pt.y, 9, 0, Math.PI * 2);
    ctx.fill();
    ctx.fillStyle = "#0a0c0f";
    ctx.font = "bold 11px DM Sans, system-ui, sans-serif";
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.fillText(String(i + 1), pt.x, pt.y);
  });
}

function hangoutShotRowHtml(n, shot) {
  const main =
    shot.feet != null && shot.feet !== ""
      ? `${shot.feet} ft`
      : shot.yards != null && shot.yards !== ""
        ? `${shot.yards} yds`
        : "—";
  const sub = [shot.lie, shot.tag].filter((x) => x != null && String(x).trim() !== "").join(" · ");
  return `<div class="hole-sim-shot"><div class="hangout-shot-line"><span class="hangout-shot-title">${n}. ${shot.title}</span><span class="hangout-shot-main">${main}</span>${sub ? `<span class="hangout-shot-sub">${sub}</span>` : ""}</div></div>`;
}

function runHangoutSimulate() {
  hangoutZeroYdsIfGreenLie();
  const top = document.getElementById("hh-top-bar");
  const pOutRows = document.getElementById("hh-outcomes-rows");
  const holePanel = document.getElementById("hh-panel-hole");
  const holeCard = document.getElementById("hh-hole-card-body");
  const pSeq = document.getElementById("hh-panel-sequence");
  const viz = document.getElementById("hh-hole-viz");
  const canvas = document.getElementById("hh-hole-canvas");
  if (!pOutRows || !holePanel || !holeCard || !pSeq) return;
  const hpars = parseHoleParsMeta();
  const holeIdx = num(document.getElementById("hh-hole")?.value, 1) - 1;
  const holePar = hpars[holeIdx] || 4;
  const holeNum1 = holeIdx + 1;
  const seedKey = buildHangoutSimSeedKey(hpars, holeIdx);
  const seed32 = hangoutFnv1aHash(seedKey) || 0x9e3779b9;
  hangoutPushRngState(makeMulberry32(seed32));
  try {
    const row = getHangoutPlayerRow();
    const dname = displayGolferName(String(row?.player_name || "Player"));
    if (!row) {
      hangoutCanvasShotCount = 0;
      hangoutLastThreeProbs = null;
      if (viz) viz.hidden = true;
      if (canvas && canvas.getContext) {
        const c0 = canvas.getContext("2d");
        if (c0) c0.clearRect(0, 0, canvas.width, canvas.height);
      }
      const otb = document.getElementById("hh-out-toolbar");
      if (otb) otb.hidden = true;
      if (top) {
        top.hidden = false;
        top.innerHTML = `<span class="hh-top-title">Hole ${holeIdx + 1} · Par ${holePar}</span><span class="hh-top-note">${metaEventVenueHtmlNote()}</span>`;
      }
      pOutRows.innerHTML =
        '<p class="text-muted" style="margin:0;font-size:0.9rem;">No row for this player/round.</p>';
      holeCard.innerHTML = "";
      pSeq.innerHTML = "";
      return;
    }
    const dgId = Math.round(num(row.dg_id, NaN));
    const mix = scoreMixFromProjection(row);
    const tilt = parTilt(holePar, hpars);
    const liveFive = liveRoughFiveMults();
    const labels = [
      { k: "eagle" },
      { k: "birdie" },
      { k: "par" },
      { k: "bogey" },
      { k: "double" },
    ];
    const order = ["eagle", "birdie", "par", "bogey", "double"];
    const raw = {};
    let ssum = 0;
    for (const { k } of labels) {
      let w = (mix[k] || 0) / 18;
      if (k === "birdie" && holePar <= 3) w *= 1.12 * tilt;
      else if (k === "eagle" && holePar >= 5) w *= 1.1 * tilt;
      else if (k === "par") w *= tilt;
      else if (k === "bogey") w *= 2 - 0.5 * (tilt - 1);
      const lm = num(liveFive[k], 1);
      raw[k] = w * (Number.isFinite(lm) && lm > 0 ? lm : 1);
      ssum += raw[k];
    }
    let probsFive = {};
    if (ssum < 1e-15) {
      const u = 0.2;
      for (const k of order) probsFive[k] = u;
    } else {
      for (const { k } of labels) probsFive[k] = raw[k] / ssum;
    }
    probsFive = hangoutRemoveHoleOutMass(
      hangoutTiltProbsFive(probsFive, hangoutOutcomeDistributionT(row, dgId)),
      holePar,
    );
    let three = hangoutCollapseFiveToThree(probsFive);
    three = hangoutNormThree(three);
    const holeHist3 = hangoutHoleHistoryPriorThree(dgId, DATA.meta.course_used, DATA.meta.event_name, holeNum1, holePar);
    if (holeHist3) {
      three = hangoutBlendThree(three, holeHist3.probs, holeHist3.weight);
    } else {
      const hist3 = hangoutHistoryPriorThree(dgId, DATA.meta.course_used, DATA.meta.event_name);
      if (hist3) three = hangoutBlendThree(three, hist3, 0.38);
    }
    const shotN = clamp(Math.round(num(document.getElementById("hh-shot-num")?.value, 1)), 1, 18);
    const puttFt = clamp(num(document.getElementById("hh-putt-ft")?.value, 10), 2, 120);
    const liveGreen = hangoutLiveOn() && String(document.getElementById("hh-lie")?.value || "") === "Green";
    if (liveGreen) {
      three = hangoutPuttingThreeWay(holePar, shotN, puttFt);
      three = hangoutNormThree(three);
      const tLG = hangoutOutcomeDistributionT(row, dgId);
      if (Number.isFinite(tLG) && Math.abs(tLG) > 1e-9) {
        const u = clamp(tLG * 0.38, -0.07, 0.07);
        three = hangoutNormThree({
          birdie: three.birdie * Math.exp(u),
          par: three.par,
          bogeyPlus: three.bogeyPlus * Math.exp(-u),
        });
      }
      const bg = three.bogeyPlus;
      probsFive = {
        eagle: 0,
        birdie: three.birdie,
        par: three.par,
        bogey: bg * 0.78,
        double: bg * 0.22,
      };
    } else {
      three = hangoutNormThree(three);
      if (hangoutLiveOn()) {
        const lieRaw = String(document.getElementById("hh-lie")?.value || "Fairway");
        const distLive = num(document.getElementById("hh-dist-yds")?.value, NaN);
        three = hangoutLiveLieThreeWayTilt(three, lieRaw, shotN, distLive);
      }
    }
    three = hangoutWidenThreeWayForPrices(three, liveGreen);
    hangoutLastThreeProbs = three;
    const bogM =
      probsFive.bogey + probsFive.double > 1e-9
        ? (probsFive.bogey + 2 * probsFive.double) / (probsFive.bogey + probsFive.double)
        : 1.32;
    const exp = liveGreen
      ? shotN - 1 + interpPgaPuttingSeries(puttFt, PGA_PUTT_DISTANCE_FT, PGA_PUTT_TOUR_AVG)
      : three.birdie * (holePar - 1) + three.par * holePar + three.bogeyPlus * (holePar + bogM);
    if (top) {
      top.hidden = false;
      top.innerHTML = `<span class="hh-top-title">Hole ${holeIdx + 1} · Par ${holePar}</span><span class="hh-top-note">${dname} · ${metaEventVenueHtmlNote()}</span>`;
    }
    hangoutRenderThreeOutcomes(three);
    holeCard.innerHTML = `<h4>Expected Score</h4><p class="hangout-pred-score">${exp.toFixed(2)}</p>`;
    const rPick = hangoutRngU01();
    let cat = "par";
    if (rPick < three.birdie) cat = "birdie";
    else if (rPick < three.birdie + three.par) cat = "par";
    else cat = "bogeyPlus";
    let sc;
    if (cat === "birdie") {
      if (holePar >= 5) {
        const pe = probsFive.eagle;
        const pb = probsFive.birdie;
        const den = pe + pb + 1e-12;
        sc = hangoutRngU01() < pe / den ? holePar - 2 : holePar - 1;
      } else {
        sc = holePar - 1;
      }
    } else if (cat === "par") {
      sc = holePar;
    } else {
      const pg = probsFive.bogey;
      const pd = probsFive.double;
      const den = pg + pd + 1e-12;
      sc = hangoutRngU01() < pg / den ? holePar + 1 : holePar + 2;
    }
    const shots = hangoutBuildShotsFromBundleOrSynth(holePar, sc, dgId, holeNum1).map((s, i) => hangoutApplyLiveToShot(i + 1, { ...s }));
    hangoutCanvasShotCount = shots.length;
    if (viz) viz.hidden = false;
    if (canvas) drawHangoutHoleCanvas(canvas, shots.length);
    const rel = hangoutScoreLabel(holePar, sc);
    const delta = sc - holePar;
    const scoreCls = delta < 0 ? "good" : delta > 0 ? "bad" : "";
    const rows = shots.map((s, i) => hangoutShotRowHtml(i + 1, s)).join("");
    pSeq.innerHTML = `<div class="hangout-seq-head"><h4>Shots</h4><span class="hangout-seq-score ${scoreCls}">${sc} · ${rel}</span></div>${rows}`;
  } finally {
    hangoutPopRngState();
  }
}

function updateHangout() {
  initHangoutSelectors(false);
  scheduleHangoutSimulateDebounced();
}

/** Yield one frame so the browser can paint / handle input before more main-thread work. */
function yieldToMain() {
  return new Promise((resolve) => {
    if (typeof requestAnimationFrame === "function") requestAnimationFrame(() => resolve());
    else setTimeout(resolve, 0);
  });
}

/** Live merge + course table: not needed for first paint; load after projections UI, then refresh affected tabs. */
function prefetchPostProjectionsSidecarsAfterPaint() {
  const jobs = [];
  if (!isFileProtocol()) {
    jobs.push(ensureLiveTournamentHistoryMerged({ useCache: false }));
    if (datagolfLiveOverlayEnabled()) jobs.push(fetchAndMergeDatagolfLiveInPlay({ force: true }));
  }
  jobs.push(loadCourseTableJson());
  return Promise.all(jobs).finally(() => {
    refreshPricingAffectedViews();
    updateStatusBar();
  });
}

/** Rebuild projection-driven UI for the visible tab only (avoids rebuilding +EV / matchups / O/U grids on every poll). */
async function refreshAll() {
  syncLbRoundToTournamentModelRound();
  updateRoundLabels();
  fillPropGolferSelect();
  fillLivePropGolferSelect();

  const tab = activeAppTabId() || "ou";
  if (tab === "ou") buildOuTable();
  else if (tab === "ev") buildEvTable();
  else if (tab === "matchup-analysis") buildMatchupAnalysisTool();

  const pm = document.getElementById("panel-matchups");
  if (pm && !pm.hidden) buildMatchupsTable();
  const po = document.getElementById("panel-outrights");
  if (po && !po.hidden) buildOutrightsTable();

  await yieldToMain();

  if (tab === "props") {
    await ensurePlayerHistoryLoadedForTab("props");
    renderPropsTrendsNow();
  }
  if (tab === "live-prop") renderLivePropPredictor();
  if (tab === "hangout") {
    initHangoutSelectors(false);
    scheduleHangoutSimulateDebounced();
  }
  if (tab === "course-fit") {
    void ensurePlayerHistoryLoadedForTab("course-fit");
    await yieldToMain();
    buildCourseFitTab();
  }
}

/**
 * @param {{ silent?: boolean, reloadSidecar?: boolean }} [opts]
 *   silent: on fetch failure, keep current DATA (for background poll).
 *   reloadSidecar: fetch player_round_history.json / player_shots_web.json (initial load only).
 */
async function loadProjections(opts = {}) {
  const silent = Boolean(opts.silent);
  const reloadSidecar = opts.reloadSidecar !== false;
  if (projectionsLoadInFlight) {
    if (silent) projectionsSilentReloadQueued = true;
    return;
  }
  projectionsLoadInFlight = true;
  if (!silent) setBootError("");

  const finishOk = async () => {
    lastProjectionsLoadedAtMs = Date.now();
    const blocking = [];
    if (reloadSidecar) blocking.push(loadPlayerShots());
    await Promise.all(blocking);
    if (HISTORY._ok) scrubNonActualRoundsFromHistoryBuckets();
    await yieldToMain();
    hydrateBakedWeatherFromPlayerFields();
    if (!projectionsWeatherUsableFromBaked()) {
      await refreshForecastWeatherFromOpenMeteo();
    } else {
      finalizeForecastWaveSummary(null);
      PRICING_MU_BONUS_CACHE.clear();
    }
    await refreshAll();
    updateStatusBar();
    stopDatagolfLivePolling();
    if (datagolfLiveOverlayEnabled() && !isFileProtocol()) {
      startDatagolfLivePolling();
    }
    requestAnimationFrame(() => {
      void prefetchPostProjectionsSidecarsAfterPaint();
    });
  };

  try {
    if (isFileProtocol()) {
      showFileProtocolBanner(true);
      dataSource = "bundled-file";
      applyPayload(DEFAULT_PROJECTIONS_PAYLOAD);
      await finishOk();
      return;
    }
    showFileProtocolBanner(false);
    const url = cacheBustFetchUrl(projectionsJsonUrl());
    const fetchOpts = { cache: "no-store" };
    if (typeof AbortSignal !== "undefined" && typeof AbortSignal.timeout === "function") {
      fetchOpts.signal = AbortSignal.timeout(45000);
    }
    const res = await fetch(url, fetchOpts);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const j = await res.json();
    const base = url.split("/").pop() || url;
    dataSource = base === "projections.json" ? "projections.json" : url;
    applyPayload(j);
    if (!silent) setBootError("");
    await finishOk();
  } catch (e) {
    if (silent) {
      updateStatusBar();
    } else {
      dataSource = "bundled-demo";
      applyPayload(DEFAULT_PROJECTIONS_PAYLOAD);
      setBootError(`Could not load ${projectionsJsonUrl()} (${e.message || e}). Using bundled demo.`);
      await finishOk();
    }
  } finally {
    projectionsLoadInFlight = false;
    if (projectionsSilentReloadQueued) {
      projectionsSilentReloadQueued = false;
      queueMicrotask(() => {
        void loadProjections({ silent: true, reloadSidecar: false });
      });
    }
  }
}

function activeAppTabId() {
  const active = document.querySelector(".tabs .tab.active");
  return active ? String(active.getAttribute("data-tab") || "") : "";
}

function ensurePlayerHistoryLoadedForTab(tab) {
  if (!["props", "course-fit", "hangout"].includes(String(tab || ""))) return Promise.resolve();
  const p =
    tab === "props"
      ? (async () => {
          const dg = selectedDgId();
          if (!Number.isFinite(dg)) return false;
          if (historyBucketLoaded(dg)) return true;
          const ok = await loadPlayerHistoryBucket(dg);
          if (!ok && !historyBucketLoaded(dg)) await extractHistoryBucketFromEmbedded(dg);
          return historyBucketLoaded(dg);
        })()
      : tab === "hangout"
        ? loadPlayerHistoryBucket(selectedHangoutDgId())
        : HISTORY._ok && !HISTORY._partial
          ? Promise.resolve(true)
          : loadPlayerHistory();
  return p.then(() => {
    if (activeAppTabId() !== tab) return;
    if (tab === "props") renderPropsTrendsNow();
    if (tab === "course-fit") buildCourseFitTab();
    if (tab === "hangout") scheduleHangoutSimulateDebounced(0);
  }).catch(() => {
    if (activeAppTabId() === "props") renderPropsTrendsNow();
  });
}

/** Rebuild +EV table from already-loaded DATA (book odds come from projections.json; optional background poll updates DATA). */
function syncEvTabOddsAfterShow() {
  buildEvTable();
}

function resultsJsonUrl() {
  return "data/results_backtest.json";
}

function resultsStatus(text) {
  const el = document.getElementById("results-status");
  if (el) el.textContent = String(text || "");
}

function resultsSelectValue(id, fallback = "__all__") {
  const el = document.getElementById(id);
  if (!el) return fallback;
  return String(el.value || fallback);
}

function loadResultsPayload() {
  if (!resultsFeatureEnabled()) return;
  if (RESULTS.loaded || RESULTS.loading) return;
  RESULTS.loading = true;
  resultsStatus("Loading results...");
  fetch(cacheBustFetchUrl(resultsJsonUrl()), { cache: "no-store" })
    .then((r) => {
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      return r.json();
    })
    .then((j) => {
      RESULTS.payload = j && typeof j === "object" ? j : null;
      RESULTS.loaded = true;
      RESULTS.error = "";
      initResultsFilters();
      renderResultsTab();
    })
    .catch((e) => {
      RESULTS.error = e?.message || String(e);
      resultsStatus(`Results file missing: ${resultsJsonUrl()} (${RESULTS.error})`);
    })
    .finally(() => {
      RESULTS.loading = false;
    });
}

function resultsAllMarkets() {
  const p = RESULTS.payload || {};
  const a = Array.isArray(p?.markets?.matchups) ? p.markets.matchups : [];
  const b = Array.isArray(p?.markets?.outrights) ? p.markets.outrights : [];
  return [...new Set([...a, ...b])]
    .filter((m) => m && m !== "__all__" && m !== "__combined__")
    .sort();
}

function resultsAllBooks() {
  const p = RESULTS.payload || {};
  const a = Array.isArray(p?.books?.matchups) ? p.books.matchups : [];
  const b = Array.isArray(p?.books?.outrights) ? p.books.outrights : [];
  return [...new Set([...a, ...b])].filter((bk) => bk && bk !== "__all__").sort();
}

function refillResultsMarketSelect(items, prev) {
  const sel = document.getElementById("results-filter-market");
  if (!sel) return;
  sel.innerHTML = "";
  for (const it of items) {
    if (!it || it === "__all__" || it === "__combined__") continue;
    const o = document.createElement("option");
    o.value = it;
    o.textContent = resultsMarketDisplayName(it);
    sel.appendChild(o);
  }
  if (!sel.options.length) return;
  const prevOk =
    prev &&
    prev !== "__combined__" &&
    prev !== "__all__" &&
    [...sel.options].some((o) => o.value === prev);
  sel.value = prevOk ? prev : sel.options[0].value;
}

function resultsDefaultMarketValue() {
  const sel = document.getElementById("results-filter-market");
  if (!sel || !sel.options.length) return "";
  return String(sel.options[0].value || "");
}

function refillResultsBookSelect(items, prev = "__all__") {
  const sel = document.getElementById("results-filter-book");
  if (!sel) return;
  sel.innerHTML = "";
  const all = document.createElement("option");
  all.value = "__all__";
  all.textContent = "All books";
  sel.appendChild(all);
  for (const it of items) {
    const o = document.createElement("option");
    o.value = it;
    const m = bookMeta(it);
    o.textContent = m.short;
    o.title = m.label;
    sel.appendChild(o);
  }
  if ([...sel.options].some((o) => o.value === prev)) sel.value = prev;
  else sel.value = "__all__";
}

function refillResultsCourseSelect(prev = "__all__") {
  const sel = document.getElementById("results-filter-course");
  if (!sel) return;
  const tuples = kellyBetTuples();
  const names = new Set();
  for (const row of tuples) {
    if (String(row[2] || "") !== "outrights") continue;
    const en = kellyTupleEventName(row);
    if (en) names.add(en);
  }
  const items = [...names].sort((a, b) => a.localeCompare(b));
  sel.innerHTML = "";
  const all = document.createElement("option");
  all.value = "__all__";
  all.textContent = "All tournaments";
  sel.appendChild(all);
  for (const it of items) {
    const o = document.createElement("option");
    o.value = it;
    o.textContent = it;
    sel.appendChild(o);
  }
  if (prev && prev !== "__all__" && [...sel.options].some((o) => o.value === prev)) sel.value = prev;
  else sel.value = "__all__";
}

function refillResultsPlayerSelect(prev = "__all__") {
  const sel = document.getElementById("results-filter-player");
  if (!sel) return;
  const tuples = kellyBetTuples();
  const byKey = new Map();
  for (const row of tuples) {
    const key = resultsPlayerFilterKey(row);
    const nm = String(row[10] ?? "").trim();
    if (!key || !nm) continue;
    if (!byKey.has(key)) byKey.set(key, nm);
  }
  const sorted = [...byKey.entries()].sort((a, b) =>
    displayGolferName(a[1]).localeCompare(displayGolferName(b[1]), undefined, { sensitivity: "base" }),
  );
  sel.innerHTML = "";
  const all = document.createElement("option");
  all.value = "__all__";
  all.textContent = "All players";
  sel.appendChild(all);
  for (const [val, rawName] of sorted) {
    const o = document.createElement("option");
    o.value = val;
    o.textContent = displayGolferName(rawName);
    sel.appendChild(o);
  }
  if (prev && prev !== "__all__" && [...sel.options].some((o) => o.value === prev)) sel.value = prev;
  else sel.value = "__all__";
}

function syncResultsKellyAuxFilters() {
  const prevC = resultsSelectValue("results-filter-course", "__all__");
  const prevP = resultsSelectValue("results-filter-player", "__all__");
  refillResultsCourseSelect(prevC);
  refillResultsPlayerSelect(prevP);
}

function initResultsFilters() {
  if (!RESULTS.payload) return;
  const prevM = resultsSelectValue("results-filter-market", "");
  const prevB = resultsSelectValue("results-filter-book", "__all__");
  refillResultsMarketSelect(resultsAllMarkets(), prevM);
  refillResultsBookSelect(resultsAllBooks(), prevB);
  syncResultsBookLogoUi();
}

function resultsMaxDateIso(rows) {
  let max = "";
  for (const r of rows) {
    const d = String(r.date || "").trim().slice(0, 10);
    if (/^\d{4}-\d{2}-\d{2}$/.test(d) && d > max) max = d;
  }
  return max;
}

function resultsTimeRangeStartIso(maxD, rangeKey) {
  if (!maxD || rangeKey === "all") return "";
  const m = maxD.match(/^(\d{4})-(\d{2})-(\d{2})$/);
  if (!m) return "";
  const y = Number(m[1]);
  const mo = Number(m[2]);
  const day = Number(m[3]);
  const maxT = Date.UTC(y, mo - 1, day);
  let startT = maxT;
  if (rangeKey === "1w") startT = maxT - 6 * 86400000;
  else if (rangeKey === "1m") startT = maxT - 29 * 86400000;
  else if (rangeKey === "1y") startT = maxT - 364 * 86400000;
  else if (rangeKey === "ytd") startT = Date.UTC(y, 0, 1);
  else return "";
  return new Date(startT).toISOString().slice(0, 10);
}

function resultsRowsInTimeRange(rows, rangeKey) {
  if (!rows.length || rangeKey === "all") return rows;
  const maxD = resultsMaxDateIso(rows);
  if (!maxD) return rows;
  const start = resultsTimeRangeStartIso(maxD, rangeKey);
  if (!start) return rows;
  return rows.filter((r) => String(r.date || "").slice(0, 10) >= start);
}

function resultsRangeCaption(rangeKey) {
  if (rangeKey === "1w") return "Last 7 days";
  if (rangeKey === "1m") return "Last 30 days";
  if (rangeKey === "1y") return "Last 365 days";
  if (rangeKey === "ytd") return "Year to date (sample year)";
  return "All history";
}

function syncResultsRangePillsUi() {
  if (!resultsFeatureEnabled()) return;
  document.querySelectorAll(".results-range-pill[data-results-range]").forEach((b) => {
    b.classList.toggle("active", (b.getAttribute("data-results-range") || "") === resultsTimeRange);
  });
}

function kellyBetsJsonUrl() {
  return "data/results_kelly_bets.json";
}

function loadKellyBetsPayload() {
  if (KELLY.loaded || KELLY.loading) return Promise.resolve();
  KELLY.loading = true;
  KELLY.error = "";
  return fetch(cacheBustFetchUrl(kellyBetsJsonUrl()), { cache: "no-store" })
    .then((r) => {
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      return r.json();
    })
    .then((j) => {
      KELLY.payload = j && typeof j === "object" ? j : null;
      KELLY.loaded = true;
    })
    .catch((e) => {
      KELLY.error = e?.message || String(e);
      KELLY.payload = null;
      KELLY.loaded = true;
    })
    .finally(() => {
      KELLY.loading = false;
    });
}

/** @returns {number[][]} */
function kellyBetTuples() {
  const b = KELLY.payload?.bets;
  return Array.isArray(b) ? b : [];
}

function kellyTupleEventName(row) {
  return Array.isArray(row) && row.length > 11 ? String(row[11] ?? "").trim() : "";
}

function kellyTupleDgId(row) {
  return Array.isArray(row) && row.length > 12 ? String(row[12] ?? "").trim() : "";
}

function resultsPlayerNameNormKey(name) {
  return String(name || "")
    .toLowerCase()
    .replace(/,/g, " ")
    .replace(/\s+/g, " ")
    .trim();
}

/** Stable value for results-filter-player (dg:id or nm:key). */
function resultsPlayerFilterKey(row) {
  const id = kellyTupleDgId(row);
  if (id) return `dg:${id}`;
  const nm = String(row[10] ?? "").trim();
  if (!nm) return "";
  return `nm:${resultsPlayerNameNormKey(nm)}`;
}

function filterKellyBetTuples(tuples) {
  const market = resultsSelectValue("results-filter-market", resultsDefaultMarketValue());
  const book = resultsSelectValue("results-filter-book", "__all__");
  const minEv = num(document.getElementById("results-filter-min-ev")?.value, 0);
  return tuples.filter((row) => {
    const mkt = String(row[3] || "");
    const bk = String(row[4] || "");
    if (market && mkt !== market) return false;
    if (book !== "__all__" && bk !== book) return false;
    const evPct = num(row[5], NaN);
    if (!Number.isFinite(evPct) || evPct < minEv) return false;
    return true;
  });
}

function resultsPricingModeValue() {
  const raw = String(resultsSelectValue("results-filter-pricing-mode", "default") || "default").toLowerCase();
  return ["default", "recent", "course", "skill"].includes(raw) ? raw : "default";
}

function resultsPricingSkillKeyForRow(row) {
  const src = String(row?.[2] || "").toLowerCase();
  const mkt = String(row?.[3] || "").toLowerCase();
  if (src === "outrights") {
    if (mkt === "win") return "sg_total";
    if (mkt === "top_5" || mkt === "top_10" || mkt === "top_20") return "sg_t2g";
    if (mkt === "make_cut" || mkt === "mc") return "sg_ott";
    return "sg_total";
  }
  if (mkt === "3_balls") return "sg_app";
  if (mkt === "round_matchups") return "sg_t2g";
  return "sg_total";
}

function buildResultsDgIdLookupFromTuples(tuples) {
  const byName = new Map();
  for (const row of tuples || []) {
    const id = Math.round(num(row?.[12], NaN));
    const nameKey = resultsPlayerNameNormKey(String(row?.[10] || ""));
    if (!nameKey || !Number.isFinite(id) || id <= 0) continue;
    const prev = byName.get(nameKey);
    if (!prev) byName.set(nameKey, { id, n: 1 });
    else if (prev.id === id) prev.n += 1;
    else if (prev.n <= 1) byName.set(nameKey, { id, n: 1 });
  }
  const out = new Map();
  for (const [k, v] of byName.entries()) out.set(k, v.id);
  return out;
}

function resultsTupleDgIdResolved(row, dgIdLookup) {
  const id = Math.round(num(row?.[12], NaN));
  if (Number.isFinite(id) && id > 0) return id;
  const nm = resultsPlayerNameNormKey(String(row?.[10] || ""));
  if (!nm) return NaN;
  const lk = Math.round(num(dgIdLookup?.get(nm), NaN));
  return Number.isFinite(lk) && lk > 0 ? lk : NaN;
}

function applyPricingModeProbAdjust(baseP, row, pricingMode, dgIdLookup) {
  if (!Number.isFinite(baseP)) return NaN;
  const p = clamp(baseP, 1e-6, 1 - 1e-6);
  if (pricingMode === "default") return p;
  const dgId = resultsTupleDgIdResolved(row, dgIdLookup);
  if (!Number.isFinite(dgId)) return p;
  const skillKey = pricingMode === "skill" ? resultsPricingSkillKeyForRow(row) : PRICING_STATE.skill;
  const b = pricingModeMuSgBonusForMode(dgId, pricingMode, skillKey);
  if (!Number.isFinite(b) || b === 0) return p;
  const logit = Math.log(p / (1 - p));
  const z = logit + b * 0.85;
  const ez = Math.exp(clamp(z, -12, 12));
  return ez / (1 + ez);
}

/**
 * Keep rows where calibrated model has strictly positive edge vs book decimal (same rule as Kelly stake sizing).
 * Ensures Results uses **all +EV opportunities**, not binned EV approximations.
 */
function filterKellyTuplesPositiveModelEdge(tuples, pricingMode = "default", dgIdLookup = null) {
  return tuples.filter((row) => {
    const pBase = num(row[6], NaN);
    const dec = num(row[7], NaN);
    if (!Number.isFinite(pBase) || !Number.isFinite(dec) || dec <= 1) return false;
    const p = applyPricingModeProbAdjust(pBase, row, pricingMode, dgIdLookup);
    return p * dec - 1 > 1e-12;
  });
}

/**
 * Outrights only: keep top-N +EV runners by model EV% per (event × book × market).
 * Balances sample size and avoids betting the full field for one market snapshot.
 */
function filterOutrightTopNPerEventBookMarket(tuples, topN = 5) {
  const k = Math.max(1, Math.min(20, Math.round(num(topN, 5))));
  const keepNonOutrights = [];
  const buckets = new Map();
  for (const row of tuples) {
    const src = String(row[2] || "");
    if (src !== "outrights") {
      keepNonOutrights.push(row);
      continue;
    }
    const eid = String(row[9] || "").trim();
    const bk = String(row[4] || "").trim().toLowerCase();
    const mkt = String(row[3] || "").trim().toLowerCase();
    const date = String(row[1] || "").slice(0, 10);
    const key = eid ? `${eid}|${bk}|${mkt}` : `${date}|${bk}|${mkt}`;
    let arr = buckets.get(key);
    if (!arr) {
      arr = [];
      buckets.set(key, arr);
    }
    arr.push(row);
  }
  const keepOutrights = [];
  for (const arr of buckets.values()) {
    arr.sort((a, b) => num(b[5], -Infinity) - num(a[5], -Infinity) || num(a[0], 0) - num(b[0], 0));
    for (let i = 0; i < arr.length && i < k; i++) keepOutrights.push(arr[i]);
  }
  return [...keepNonOutrights, ...keepOutrights];
}

/**
 * Outright win rows are mutually exclusive outcomes, but keeping only one runner/event is too sparse.
 * Keep top-N model EV% win runners per event × book so sample size is usable.
 */
function filterOutrightWinTopNPerEventBook(tuples, topN = 3) {
  const k = Math.max(1, Math.min(20, Math.round(num(topN, 3))));
  const nonWin = [];
  const buckets = new Map();
  for (const row of tuples) {
    const src = String(row[2] || "");
    const mkt = String(row[3] || "");
    if (src !== "outrights" || mkt !== "win") {
      nonWin.push(row);
      continue;
    }
    const eid = String(row[9] || "").trim();
    const bk = String(row[4] || "").trim().toLowerCase();
    const date = String(row[1] || "").slice(0, 10);
    const key = eid ? `${eid}|${bk}` : `${date}|${bk}`;
    let arr = buckets.get(key);
    if (!arr) {
      arr = [];
      buckets.set(key, arr);
    }
    arr.push(row);
  }
  const keepWin = [];
  for (const arr of buckets.values()) {
    arr.sort((a, b) => num(b[5], -Infinity) - num(a[5], -Infinity) || num(a[0], 0) - num(b[0], 0));
    for (let i = 0; i < arr.length && i < k; i++) keepWin.push(arr[i]);
  }
  return [...nonWin, ...keepWin];
}

function kellyMaxDateIsoFromTuples(tuples) {
  let max = "";
  for (const row of tuples) {
    const d = String(row[1] || "").slice(0, 10);
    if (/^\d{4}-\d{2}-\d{2}$/.test(d) && d > max) max = d;
  }
  return max;
}

function kellyTuplesInTimeRange(tuples, rangeKey) {
  if (!tuples.length || rangeKey === "all") return tuples;
  const maxD = kellyMaxDateIsoFromTuples(tuples);
  if (!maxD) return tuples;
  const start = resultsTimeRangeStartIso(maxD, rangeKey);
  if (!start) return tuples;
  return tuples.filter((row) => String(row[1] || "").slice(0, 10) >= start);
}

/**
 * Kelly sim; tuple indices through dg_id when schema ≥ 3 (see build-results-backtest.mjs).
 * Stake fractions use **starting** bankroll B0 only (flat sizing). Equity is tracked without flooring at 0
 * so the series does not flatline after ruin when thousands of +EV bets share settlement days.
 * Same-day gross stake is capped to **one B0** (proportional scale), not to trailing equity.
 * @returns {{ series: object[], topWins: object[], nDays: number }}
 */
function simulateKellyDailySeriesDetailed(tuples, meta) {
  const B0 = num(meta?.bankroll0, 100);
  const kFrac = num(meta?.kelly_fraction, 0.25);
  const capF = num(meta?.max_kelly_stake_frac, 0.15);
  const pricingMode = String(meta?.pricing_mode || "default").toLowerCase();
  const dgIdLookup = meta?.dg_id_lookup || null;
  let equity = B0;
  let cumStake = 0;
  let nB = 0;
  const series = [];
  let lastTs = 0;
  /** @type {{ date: string, cumPnlUnits: number, profitUnits: number, stakeUnits: number, bankrollBeforeUnits: number, source: string, market: string, book: string, dec: number, player: string }[]} */
  const wins = [];
  const sorted = [...tuples].sort((a, b) => num(a[0], 0) - num(b[0], 0));
  const groups = new Map();
  for (const row of sorted) {
    const date = String(row[1] || "").slice(0, 10);
    if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) continue;
    let arr = groups.get(date);
    if (!arr) {
      arr = [];
      groups.set(date, arr);
    }
    arr.push(row);
  }
  const dates = [...groups.keys()].sort();
  const nDays = dates.length;
  for (const date of dates) {
    const rows = groups.get(date) || [];
    const sized = [];
    for (const row of rows) {
      const pBase = num(row[6], NaN);
      const decN = num(row[7], NaN);
      const w = num(row[8], 0) === 1 ? 1 : 0;
      const player = String(row[10] ?? "");
      const src = String(row[2] || "");
      const mkt = String(row[3] || "");
      const bk = String(row[4] || "");
      if (!Number.isFinite(decN) || decN <= 1 || !Number.isFinite(pBase)) continue;
      const pN = applyPricingModeProbAdjust(pBase, row, pricingMode, dgIdLookup);
      const edge = pN * decN - 1;
      let f = edge > 0 ? edge / (decN - 1) : 0;
      if (!Number.isFinite(f) || f <= 0) continue;
      f *= kFrac;
      f = Math.min(f, capF);
      if (!Number.isFinite(f) || f <= 0) continue;
      sized.push({ t: num(row[0], 0), f, decN, w, player, src, mkt, bk });
    }
    sized.sort(
      (a, b) =>
        a.t - b.t ||
        String(a.mkt).localeCompare(String(b.mkt)) ||
        String(a.bk).localeCompare(String(b.bk)) ||
        String(a.player).localeCompare(String(b.player)),
    );
    const nominalStakes = sized.map((s) => Math.min(Math.max(0, B0 * s.f), B0));
    const nominalTotal = nominalStakes.reduce((a, b) => a + b, 0);
    // Softer compression than linear scaling so high-volume days still show meaningful volatility.
    const dayScale = nominalTotal > 0 ? Math.min(1, Math.sqrt(B0 / nominalTotal)) : 0;
    for (let i = 0; i < sized.length; i++) {
      const s = sized[i];
      const stake = nominalStakes[i] * dayScale;
      if (!Number.isFinite(stake) || stake <= 0) continue;
      const bankrollBeforeUnits = equity;
      cumStake += stake;
      nB += 1;
      if (s.w) {
        const profitU = stake * (s.decN - 1);
        equity += profitU;
        wins.push({
          date,
          cumPnlUnits: equity - B0,
          profitUnits: profitU,
          stakeUnits: stake,
          bankrollBeforeUnits,
          source: s.src,
          market: s.mkt,
          book: s.bk,
          dec: s.decN,
          player: s.player,
        });
      } else {
        equity -= stake;
      }
      const ts = Math.max(lastTs + 1, num(s.t, 0));
      lastTs = ts;
      series.push({
        date,
        ts,
        cumBets: nB,
        cumStake,
        cumPnl: equity - B0,
        cumRoiPct: B0 > 0 ? (equity / B0 - 1) * 100 : 0,
        _kellyBr: equity,
      });
    }
  }
  wins.sort((a, b) => b.profitUnits - a.profitUnits);
  const topWins = wins.slice(0, 8);
  return { series, topWins, nDays };
}

function resultsBankrollDollarsFromUi() {
  const raw = num(document.getElementById("results-bankroll-dollars")?.value, 10000);
  return Math.max(100, raw);
}

function formatUsdCompact(n) {
  if (!Number.isFinite(n)) return "—";
  const sign = n < 0 ? "-" : "";
  const v = Math.abs(n);
  if (v >= 1e6) return `${sign}$${(v / 1e6).toFixed(2)}M`;
  if (v >= 10000) return `${sign}$${Math.round(v).toLocaleString("en-US")}`;
  if (v >= 1000) return `${sign}$${(v / 1000).toFixed(1)}k`;
  return `${sign}$${v.toFixed(0)}`;
}

function resultsMarketDisplayName(key) {
  const k = String(key || "").trim().toLowerCase();
  const map = {
    win: "Win",
    top_5: "Top 5",
    top_10: "Top 10",
    top_20: "Top 20",
    make_cut: "Make cut",
    mc: "Miss cut",
    tournament_matchups: "Tournament matchups",
    round_matchups: "Round matchups",
    "3_balls": "3-Balls",
  };
  if (map[k]) return map[k];
  return String(key || "")
    .replace(/_/g, " ")
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

function syncResultsBookLogoUi() {
  const wrap = document.getElementById("results-book-logo-wrap");
  if (!wrap) return;
  wrap.innerHTML = "";
  const bk = resultsSelectValue("results-filter-book", "__all__");
  if (bk === "__all__") return;
  const m = bookMeta(bk);
  const img = document.createElement("img");
  img.className = "results-book-logo-img";
  img.alt = m.label;
  const fb = document.createElement("span");
  fb.className = "results-book-logo-fallback";
  fb.textContent = m.short;
  fb.style.display = "none";
  wrap.appendChild(img);
  wrap.appendChild(fb);
  attachBookLogoWithFallback(img, fb, m.domain);
}

async function renderResultsKellyPnL() {
  if (!resultsFeatureEnabled()) return;
  await loadKellyBetsPayload();
  const bankrollDollars = resultsBankrollDollarsFromUi();
  const B0Fallback = num(KELLY.payload?.bankroll0, 100);
  const metaFallback = {
    bankroll0: B0Fallback,
    kelly_fraction: num(KELLY.payload?.kelly_fraction, 0.25),
    max_kelly_stake_frac: num(KELLY.payload?.max_kelly_stake_frac, 0.15),
    bankrollDollars,
  };
  const tuples0 = kellyBetTuples();
  if (!KELLY.payload || !tuples0.length) {
    resultsStatus(
      KELLY.error ? `Unable to load bet history (${KELLY.error})` : "Bet history unavailable. Run npm run build:results.",
    );
    drawResultsChart([], { bankrollDollars, bankroll0: B0Fallback, totalBets: 0, winMarkers: [] });
    renderResultsSummaryKellyUsd([], metaFallback, bankrollDollars);
    syncResultsBookLogoUi();
    return;
  }
  const pricingMode = resultsPricingModeValue();
  const dgIdLookup = buildResultsDgIdLookupFromTuples(tuples0);
  let t = filterKellyBetTuples(tuples0);
  t = filterKellyTuplesPositiveModelEdge(t, pricingMode, dgIdLookup);
  t = filterOutrightTopNPerEventBookMarket(t, 5);
  const selectedMarket = resultsSelectValue("results-filter-market", resultsDefaultMarketValue());
  t = kellyTuplesInTimeRange(t, resultsTimeRange);
  const meta = {
    bankroll0: num(KELLY.payload.bankroll0, 100),
    kelly_fraction: num(KELLY.payload.kelly_fraction, 0.25),
    max_kelly_stake_frac: num(KELLY.payload.max_kelly_stake_frac, 0.15),
    pricing_mode: pricingMode,
    dg_id_lookup: dgIdLookup,
  };
  const B0 = meta.bankroll0;
  const { series, nDays } = simulateKellyDailySeriesDetailed(t, meta);
  const points = series.map((p) => ({
    ...p,
    cumPnlUsd: (p.cumPnl / B0) * bankrollDollars,
    cumStakeUsd: (p.cumStake / B0) * bankrollDollars,
  }));
  const winMarkers = [];
  const kfPct = (num(meta.kelly_fraction, 0.25) * 100).toFixed(0);
  const capPct = (num(meta.max_kelly_stake_frac, 0.15) * 100).toFixed(0);
  resultsStatus("");
  drawResultsChart(points, {
    bankrollDollars,
    bankroll0: B0,
    totalBets: t.length,
    winMarkers,
  });
  renderResultsSummaryKellyUsd(points, { ...meta, bankrollDollars }, bankrollDollars);
  syncResultsBookLogoUi();
}

function syncResultsChartCanvasCssBox(canvas, cssW, cssH) {
  canvas.style.width = `${cssW}px`;
  canvas.style.height = `${cssH}px`;
  canvas.style.maxWidth = "100%";
  canvas.style.boxSizing = "border-box";
}

const RESULTS_MONTH_SHORT = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

/** Axis / table: include year for clarity (e.g. Jan 20, 2026). */
function resultsChartFormatDateLong(iso) {
  const m = String(iso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return String(iso || "");
  const y = Number(m[1]);
  const mo = Number(m[2]);
  const d = Number(m[3]);
  const mon = RESULTS_MONTH_SHORT[mo - 1] || String(mo);
  return `${mon} ${d}, ${y}`;
}

/** Compact axis label (e.g. Jan '19) — even spacing uses index ticks; short text avoids overlap. */
function resultsChartFormatDateAxis(iso) {
  const m = String(iso || "").match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (!m) return String(iso || "").slice(0, 9);
  const y = Number(m[1]);
  const mo = Number(m[2]);
  const mon = RESULTS_MONTH_SHORT[mo - 1] || String(mo);
  const yy = y % 100;
  return `${mon} '${yy < 10 ? `0${yy}` : String(yy)}`;
}

/** `count` tick positions from 0 … n-1 at equal index gaps (symmetric on chart width). */
function resultsChartEvenXTickIndices(n, count) {
  if (n <= 0) return [];
  const k = Math.max(2, Math.min(count, n));
  if (n === 1) return [0];
  const out = [];
  for (let j = 0; j < k; j++) {
    out.push(Math.round((j / (k - 1)) * (n - 1)));
  }
  return [...new Set(out)].sort((a, b) => a - b);
}

function resultsChartNiceYTicks(lo, hi, wantInt = false) {
  const span = Math.max(1e-9, hi - lo);
  const raw = span / 4;
  const pow10 = 10 ** Math.floor(Math.log10(raw));
  const fr = raw / pow10;
  let step = pow10;
  if (fr < 1.5) step = pow10;
  else if (fr < 3.5) step = 2 * pow10;
  else if (fr < 8) step = 5 * pow10;
  else step = 10 * pow10;
  const start = Math.floor(lo / step) * step;
  const out = [];
  for (let v = start; v <= hi + step * 0.001; v += step) {
    if (v >= lo - 1e-9 && v <= hi + 1e-9) out.push(v);
    if (out.length > 8) break;
  }
  if (wantInt) return out.map((x) => Math.round(x));
  return out;
}

function drawResultsChart(points, opts = {}) {
  resultsChartHitRegions = [];
  hideResultsChartTooltip();
  const bankrollDollars = num(opts.bankrollDollars, 10000);
  const bankroll0 = num(opts.bankroll0, 100);
  const totalBets = Math.round(
    num(opts.totalBets, points.length ? points[points.length - 1].cumBets : 0),
  );
  const winMarkers = Array.isArray(opts.winMarkers) ? opts.winMarkers : [];

  function rowUsd(p) {
    if (Number.isFinite(p.cumPnlUsd)) return p.cumPnlUsd;
    return (num(p.cumPnl, 0) / bankroll0) * bankrollDollars;
  }

  const canvas = document.getElementById("results-chart-canvas");
  const wrap = canvas?.closest(".results-chart-wrap");
  if (!canvas || !canvas.getContext) return;
  const ctx = canvas.getContext("2d");
  if (!ctx) return;

  const kpiMain = document.getElementById("results-kpi-main");
  const kpiSub = document.getElementById("results-kpi-sub");
  const kpiLab = document.getElementById("results-kpi-label");
  const dpr = Math.min(2, window.devicePixelRatio || 1);
  const vhCap = typeof window !== "undefined" ? Math.min(520, Math.round(window.innerHeight * 0.42)) : 520;

  function paintEmpty(msg, cssW0, cssH0) {
    canvas.width = Math.round(cssW0 * dpr);
    canvas.height = Math.round(cssH0 * dpr);
    syncResultsChartCanvasCssBox(canvas, cssW0, cssH0);
    const c0 = canvas.getContext("2d");
    if (!c0) return;
    c0.setTransform(dpr, 0, 0, dpr, 0, 0);
    c0.clearRect(0, 0, cssW0, cssH0);
    c0.fillStyle = "#111216";
    c0.fillRect(0, 0, cssW0, cssH0);
    c0.fillStyle = "#8b8f9c";
    c0.font = "13px DM Sans, system-ui, sans-serif";
    c0.textAlign = "left";
    c0.textBaseline = "top";
    c0.fillText(msg, 14, 14);
  }

  const visibleW = wrap && wrap.clientWidth > 80 ? wrap.clientWidth - 4 : 800;
  const cssW = Math.round(visibleW);
  const cssH = Math.round(clamp(visibleW * 0.42, 240, vhCap));

  if (!points.length) {
    if (kpiLab) kpiLab.textContent = "Cumulative Kelly PnL";
    if (kpiMain) {
      kpiMain.textContent = "—";
      kpiMain.classList.remove("results-kpi-pos", "results-kpi-neg");
    }
    if (kpiSub) kpiSub.textContent = "";
    paintEmpty("No bets for current filters.", cssW, cssH);
    return;
  }

  syncResultsChartCanvasCssBox(canvas, cssW, cssH);
  canvas.width = Math.round(cssW * dpr);
  canvas.height = Math.round(cssH * dpr);
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, cssW, cssH);
  ctx.fillStyle = "#111216";
  ctx.fillRect(0, 0, cssW, cssH);

  const pad = { l: 62, r: 16, t: 14, b: 44 };
  const innerW = Math.max(60, cssW - pad.l - pad.r);
  const innerH = Math.max(60, cssH - pad.t - pad.b);

  const vals = points.map((p) => rowUsd(p));
  let minV = Math.min(...vals);
  let maxV = Math.max(...vals);
  const span0 = Math.max(1e-9, maxV - minV);
  const padAbs = Math.max(250, span0 * 0.15);
  minV -= padAbs;
  maxV += padAbs;
  if (maxV - minV < 800) {
    const mid = (minV + maxV) / 2;
    minV = mid - 400;
    maxV = mid + 400;
  }

  const yScale = (v) => pad.t + innerH * (1 - (v - minV) / (maxV - minV));
  const yTicks = resultsChartNiceYTicks(minV, maxV, false);

  ctx.strokeStyle = "rgba(255, 255, 255, 0.07)";
  ctx.lineWidth = 1;
  for (const tv of yTicks) {
    if (tv < minV - 1e-9 || tv > maxV + 1e-9) continue;
    const y = yScale(tv);
    ctx.beginPath();
    ctx.moveTo(pad.l, y);
    ctx.lineTo(pad.l + innerW, y);
    ctx.stroke();
  }

  ctx.strokeStyle = "#2b2e36";
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(pad.l, pad.t);
  ctx.lineTo(pad.l, pad.t + innerH);
  ctx.lineTo(pad.l + innerW, pad.t + innerH);
  ctx.stroke();

  ctx.fillStyle = "#8b8f9c";
  ctx.font = "10px DM Sans, system-ui, sans-serif";
  ctx.textAlign = "right";
  ctx.textBaseline = "middle";
  for (const tv of yTicks) {
    if (tv < minV - 1e-9 || tv > maxV + 1e-9) continue;
    const y = yScale(tv);
    ctx.fillText(formatUsdCompact(tv), pad.l - 8, y);
  }

  ctx.save();
  ctx.translate(12, pad.t + innerH / 2);
  ctx.rotate(-Math.PI / 2);
  ctx.fillStyle = "#6b7280";
  ctx.font = "11px DM Sans, system-ui, sans-serif";
  ctx.textAlign = "center";
  ctx.textBaseline = "middle";
  ctx.fillText("Cumulative Kelly PnL ($)", 0, 0);
  ctx.restore();

  const xByIndex = (i) => pad.l + (i / Math.max(1, points.length - 1)) * innerW;

  const zeroY = minV <= 0 && maxV >= 0 ? yScale(0) : NaN;
  if (Number.isFinite(zeroY)) {
    ctx.strokeStyle = "rgba(255,255,255,0.22)";
    ctx.setLineDash([4, 4]);
    ctx.beginPath();
    ctx.moveTo(pad.l, zeroY);
    ctx.lineTo(pad.l + innerW, zeroY);
    ctx.stroke();
    ctx.setLineDash([]);
  }

  const grad = ctx.createLinearGradient(pad.l, 0, pad.l + innerW, 0);
  grad.addColorStop(0, "#00c46b");
  grad.addColorStop(1, "rgba(0, 196, 107, 0.45)");
  ctx.strokeStyle = grad;
  ctx.lineWidth = 2.25;
  ctx.lineJoin = "round";
  ctx.lineCap = "round";
  ctx.beginPath();
  for (let i = 0; i < points.length; i++) {
    const p = points[i];
    const px = xByIndex(i);
    const py = yScale(rowUsd(p));
    if (!Number.isFinite(px) || !Number.isFinite(py)) continue;
    if (i === 0) ctx.moveTo(px, py);
    else ctx.lineTo(px, py);
  }
  ctx.stroke();

  const nPts = points.length;
  const maxTicks = innerW < 480 ? 5 : innerW < 680 ? 6 : innerW < 900 ? 7 : 8;
  const xTickIdx = resultsChartEvenXTickIndices(nPts, maxTicks);

  const xFontPx = nPts > 400 || innerW < 560 ? 9 : 10;
  ctx.fillStyle = "#8b8f9c";
  ctx.font = `${xFontPx}px DM Sans, system-ui, sans-serif`;
  ctx.textAlign = "center";
  ctx.textBaseline = "top";
  for (const idx of xTickIdx) {
    const p = points[idx];
    const px = xByIndex(idx);
    if (!Number.isFinite(px)) continue;
    const lab = resultsChartFormatDateAxis(p.date);
    ctx.fillText(lab, px, pad.t + innerH + 8);
    ctx.strokeStyle = "rgba(255,255,255,0.08)";
    ctx.beginPath();
    ctx.moveTo(px, pad.t + innerH);
    ctx.lineTo(px, pad.t + innerH + 5);
    ctx.stroke();
  }

  ctx.fillStyle = "#8b8f9c";
  ctx.font = "10px DM Sans, system-ui, sans-serif";
  ctx.textAlign = "center";
  ctx.textBaseline = "top";
  ctx.fillText("Date", pad.l + innerW / 2, cssH - 12);

  for (const wm of winMarkers) {
    const i = Math.max(0, Math.min(points.length - 1, Math.round(num(wm.pointIndex, 0))));
    const px = xByIndex(i) + num(wm.xShift, 0);
    const py = yScale(rowUsd(points[i]));
    if (!Number.isFinite(px) || !Number.isFinite(py)) continue;
    ctx.beginPath();
    ctx.arc(px, py, 6, 0, Math.PI * 2);
    ctx.fillStyle = "#facc15";
    ctx.fill();
    ctx.strokeStyle = "rgba(255,255,255,0.92)";
    ctx.lineWidth = 2;
    ctx.stroke();
    resultsChartHitRegions.push({
      kind: "win",
      cx: px,
      cy: py,
      r: 14,
      tipHtml: wm.tipHtml || "",
    });
  }

  const last = points[points.length - 1];
  const first = points[0];
  if (kpiLab) kpiLab.textContent = "Cumulative Kelly PnL";
  if (kpiMain) {
    const rawEnd = rowUsd(last);
    const endStr = `${rawEnd > 0 ? "+" : ""}${formatUsdCompact(rawEnd)}`;
    kpiMain.textContent = endStr;
    kpiMain.classList.remove("results-kpi-pos", "results-kpi-neg");
    if (rawEnd > 25) kpiMain.classList.add("results-kpi-pos");
    else if (rawEnd < -25) kpiMain.classList.add("results-kpi-neg");
  }
  if (kpiSub) kpiSub.textContent = "";
}

function renderResultsSummaryKellyUsd(points, meta, bankrollDollars) {
  const trh = document.querySelector("#table-results-summary thead tr");
  if (trh) {
    trh.innerHTML =
      "<th class=\"num\">Bets</th><th class=\"num\">Total staked</th><th class=\"num\">Net P&amp;L</th><th class=\"num\">Return</th><th>Period</th>";
  }
  const tb = document.querySelector("#table-results-summary tbody");
  if (!tb) return;
  tb.innerHTML = "";
  const tr = document.createElement("tr");
  const B0 = num(meta?.bankroll0, 100);
  const last = points.length ? points[points.length - 1] : null;
  const bets = last ? Math.round(last.cumBets) : 0;
  let stakeUsd = 0;
  let netUsd = 0;
  let roiPct = 0;
  if (last) {
    stakeUsd = Number.isFinite(last.cumStakeUsd)
      ? last.cumStakeUsd
      : (num(last.cumStake, 0) / B0) * bankrollDollars;
    netUsd = Number.isFinite(last.cumPnlUsd)
      ? last.cumPnlUsd
      : (num(last.cumPnl, 0) / B0) * bankrollDollars;
    roiPct = Number.isFinite(last.cumRoiPct) ? last.cumRoiPct : 0;
  }
  const firstDate = points.length ? resultsChartFormatDateLong(points[0].date) : "—";
  const lastDate = points.length ? resultsChartFormatDateLong(points[points.length - 1].date) : "—";
  const cells = [
    bets.toLocaleString(),
    formatUsdCompact(stakeUsd),
    `${netUsd >= 0 ? "+" : ""}${formatUsdCompact(netUsd)}`,
    `${roiPct >= 0 ? "+" : ""}${roiPct.toFixed(2)}%`,
    `${firstDate} – ${lastDate}`,
  ];
  for (let i = 0; i < cells.length; i++) {
    const td = document.createElement("td");
    td.textContent = cells[i];
    if (i < 4) td.className = "num";
    tr.appendChild(td);
  }
  tb.appendChild(tr);
  const kf = num(meta?.kelly_fraction, 0.25);
  const cap = num(meta?.max_kelly_stake_frac, 0.15);
  tr.title = `Model bankroll ${B0} u · scaled to $${Math.round(bankrollDollars).toLocaleString("en-US")} · ${(kf * 100).toFixed(0)}% Kelly · max ${(cap * 100).toFixed(0)}% per bet`;
}

function renderResultsTab() {
  if (!resultsFeatureEnabled()) return;
  if (resultsTimeRange === "1d") resultsTimeRange = "all";
  syncResultsRangePillsUi();
  void renderResultsKellyPnL();
}

function initTabs() {
  document.querySelectorAll(".tabs .tab").forEach((btn) => {
    btn.addEventListener("click", () => {
      const tab = btn.getAttribute("data-tab");
      document.querySelectorAll(".tabs .tab").forEach((b) => {
        b.classList.toggle("active", b === btn);
        b.setAttribute("aria-selected", b === btn ? "true" : "false");
      });
      document.querySelectorAll(".panel").forEach((p) => {
        p.classList.toggle("active", p.id === `panel-${tab}`);
        p.hidden = p.id !== `panel-${tab}`;
      });
      if (tab === "ou")
        requestAnimationFrame(() => {
          buildOuTable();
          syncOuChartCard();
          const ouProjCanvas = document.querySelector("#table-ou tbody .ou-proj-detail-canvas");
          if (ouProjCanvas && ouProjExpandedDetail) {
            drawOuProjDetailDistribution(
              ouProjCanvas,
              ouProjExpandedDetail.market,
              ouProjExpandedDetail.player,
              ouProjExpandedDetail.line,
            );
          }
        });
      if (tab === "hangout") {
        requestAnimationFrame(() => {
          void ensurePlayerHistoryLoadedForTab("hangout");
          const vz = document.getElementById("hh-hole-viz");
          const cv = document.getElementById("hh-hole-canvas");
          if (vz && cv && !vz.hidden && hangoutCanvasShotCount > 0) {
            drawHangoutHoleCanvas(cv, hangoutCanvasShotCount);
          } else {
            scheduleHangoutSimulateDebounced(0);
          }
        });
      }
      if (tab === "props") {
        requestAnimationFrame(() => {
          void ensurePlayerHistoryLoadedForTab("props");
        });
      }
      if (tab === "matchup-analysis") {
        requestAnimationFrame(() => buildMatchupAnalysisTool());
      }
      if (tab === "live-prop") {
        requestAnimationFrame(() => renderLivePropPredictor());
      }
      if (tab === "course-fit") {
        requestAnimationFrame(() => {
          void ensurePlayerHistoryLoadedForTab("course-fit");
          buildCourseFitTab();
        });
      }
      if (tab === "ev") {
        requestAnimationFrame(() => {
          syncEvTabOddsAfterShow();
          if (!isFileProtocol()) {
            void loadProjections({ silent: true, reloadSidecar: false });
          }
        });
      }
      if (tab === "results") {
        requestAnimationFrame(() => {
          loadResultsPayload();
          renderResultsTab();
        });
      }
    });
  });
}

document.addEventListener("DOMContentLoaded", () => {
  syncWeatherUiFromState();
  syncPricingUiFromState();
  for (const ids of PRICING_UI_IDS) {
    for (const id of [ids.mode, ids.skill]) {
      const el = document.getElementById(id);
      if (!el) continue;
      el.addEventListener("change", () => {
        PRICING_STATE = pricingFromUiIds(ids);
        PRICING_MU_BONUS_CACHE.clear();
        syncPricingUiFromState();
        refreshPricingAffectedViews();
      });
    }
  }
  initPropsTopTableSortOnce();
  configureRoundPickerUi();
  initTabs();
  initCourseFitSubtabs();
  initCourseFitSimilarListClick();
  initCourseFitTableSortOnce();
  ensureCourseFitBinTooltipHandlers();
  initLivePropPredictorUi();
  initOutrightsTableSortOnce();
  initEvTableSortOnce();
  wireAllGolferSearchCombosOnce();
  wireOuPlayerFilterSuggestOnce();
  document.getElementById("course-fit-venue")?.addEventListener("change", (e) => {
    const v = String(/** @type {HTMLSelectElement} */ (e.target).value || "").trim();
    courseFitVenueFilterKey = v || null;
    courseFitSimilarSelectedKey = null;
    buildCourseFitTab();
  });
  document.getElementById("course-fit-player")?.addEventListener("change", () => buildCourseFitTab());
  document.getElementById("course-fit-search")?.addEventListener("input", () => buildCourseFitTab());
  document.getElementById("course-fit-shots-search")?.addEventListener("input", () => buildCourseFitTab());
  document.getElementById("btn-refresh-outrights")?.addEventListener("click", () => loadProjections());
  document.getElementById("lb-round")?.addEventListener("change", () => {
    ouProjExpandedKey = "";
    updateRoundLabels();
    const t = activeAppTabId() || "ou";
    if (t === "ou") buildOuTable();
    if (t === "ev") buildEvTable();
    const pm = document.getElementById("panel-matchups");
    if (pm && !pm.hidden) buildMatchupsTable();
    const po = document.getElementById("panel-outrights");
    if (po && !po.hidden) buildOutrightsTable();
    if (activeAppTabId() === "matchup-analysis") buildMatchupAnalysisTool();
    if (activeAppTabId() === "props") renderPropsTrendsNow();
    syncLivePropBookLineAndOddsFromDk();
    if (activeAppTabId() === "live-prop") renderLivePropPredictor();
    if (activeAppTabId() === "hangout") {
      initHangoutSelectors(false);
      scheduleHangoutSimulateDebounced();
    }
    if (activeAppTabId() === "course-fit") buildCourseFitTab();
    void refreshForecastWeatherFromOpenMeteo().then((fwOk) => {
      if (fwOk) refreshPricingAffectedViews();
    });
  });
  document.getElementById("analysis-market")?.addEventListener("change", () => {
    matchupAnalysisSelectedKey = "";
    buildMatchupAnalysisTool();
  });
  document.getElementById("analysis-matchup-select")?.addEventListener("change", (e) => {
    matchupAnalysisSelectedKey = String(/** @type {HTMLSelectElement} */ (e.target).value || "");
    buildMatchupAnalysisTool();
  });
  document.getElementById("ou-market-filter")?.addEventListener("change", () => {
    ouTableSort = { key: "pr-edge", dir: -1 };
    const m = getOuMarket();
    const rng = OU_LINE_RANGES[m] || OU_LINE_RANGES["Total score"];
    const inp = document.getElementById("ou-line-filter");
    if (inp && rng.length) {
      const mid = m === "Total score" ? 70.5 : enforceHalfLine(rng[Math.floor(rng.length / 2)]);
      ouLineCommitted = Number.isFinite(mid) ? mid : 70.5;
      inp.value = ouLineCommitted.toFixed(1);
    }
    buildOuTable();
  });
  {
    let ouPlayerFilterDebounce = null;
    document.getElementById("ou-player-filter")?.addEventListener("input", () => {
      ouProjExpandedKey = "";
      clearTimeout(ouPlayerFilterDebounce);
      ouPlayerFilterDebounce = setTimeout(() => buildOuTable(), 140);
    });
    document.getElementById("ou-player-filter")?.addEventListener("change", () => {
      ouProjExpandedKey = "";
      buildOuTable();
    });
  }
  document.getElementById("ou-proj-market-filter")?.addEventListener("change", () => {
    ouProjExpandedKey = "";
    ouTableSort = { key: "pr-edge", dir: -1 };
    buildOuTable();
  });
  document.getElementById("ou-line-filter")?.addEventListener("change", () => {
    commitOuLineFilterValue();
    buildOuTable();
  });
  document.getElementById("ou-line-filter")?.addEventListener("input", () => buildOuTable());
  document.getElementById("ou-line-filter")?.addEventListener("blur", () => {
    commitOuLineFilterValue();
    buildOuTable();
  });
  {
    const v0 = parseOuLineFilterInput();
    if (Number.isFinite(v0)) ouLineCommitted = v0;
  }
  document.getElementById("ou-odds-over-filter")?.addEventListener("change", () => {
    selectedOuOddsById("ou-odds-over-filter", true);
    buildOuTable();
  });
  document.getElementById("ou-odds-over-filter")?.addEventListener("blur", () => {
    selectedOuOddsById("ou-odds-over-filter", true);
    buildOuTable();
  });
  document.getElementById("ou-odds-over-filter")?.addEventListener("input", () => buildOuTable());
  document.getElementById("ou-odds-under-filter")?.addEventListener("change", () => {
    selectedOuOddsById("ou-odds-under-filter", true);
    buildOuTable();
  });
  document.getElementById("ou-odds-under-filter")?.addEventListener("blur", () => {
    selectedOuOddsById("ou-odds-under-filter", true);
    buildOuTable();
  });
  document.getElementById("ou-odds-under-filter")?.addEventListener("input", () => buildOuTable());
  document.getElementById("ou-mode-prob")?.addEventListener("click", () => {
    setOuViewMode("prob");
    buildOuTable();
  });
  document.getElementById("ou-mode-price")?.addEventListener("click", () => {
    setOuViewMode("price");
    buildOuTable();
  });
  document.getElementById("table-ou")?.addEventListener("click", (ev) => {
    const tr = ev.target.closest("tr.ou-proj-data-row");
    if (!tr || !tr.dataset.expandKey) return;
    if (document.getElementById("panel-ou")?.dataset?.ouView !== "projections") return;
    const key = tr.dataset.expandKey;
    ouProjExpandedKey = ouProjExpandedKey === key ? "" : key;
    buildOuTable();
  });
  const ouCv = document.getElementById("ou-chart-canvas");
  ouCv?.addEventListener("mousemove", (ev) => {
    if (!ouChartHitRegions.length) {
      hideOuChartTooltip();
      return;
    }
    const rect = ouCv.getBoundingClientRect();
    const x = ev.clientX - rect.left;
    const y = ev.clientY - rect.top;
    const hit = ouChartHitRegions.find((r) => x >= r.x0 && x < r.x0 + r.w && y >= r.y0 && y < r.y0 + r.h);
    if (hit) showOuChartTooltip(ev, hit);
    else hideOuChartTooltip();
  });
  ouCv?.addEventListener("mouseleave", () => hideOuChartTooltip());
  const resCv = document.getElementById("results-chart-canvas");
  resCv?.addEventListener("mousemove", (ev) => {
    if (!resultsChartHitRegions.length) {
      hideResultsChartTooltip();
      return;
    }
    const rect = resCv.getBoundingClientRect();
    const x = ev.clientX - rect.left;
    const y = ev.clientY - rect.top;
    const hit = resultsChartHitRegions.find((r) => {
      if (r.kind !== "win") return false;
      const dx = x - r.cx;
      const dy = y - r.cy;
      return dx * dx + dy * dy <= r.r * r.r;
    });
    if (hit) showResultsChartTooltip(ev, hit);
    else hideResultsChartTooltip();
  });
  resCv?.addEventListener("mouseleave", () => hideResultsChartTooltip());
  let ouChartResizeT = 0;
  window.addEventListener("resize", () => {
    window.clearTimeout(ouChartResizeT);
    ouChartResizeT = window.setTimeout(() => {
      if (isOuGolferSelected()) drawOuLineDistributionChart();
      const ouProjCanvas = document.querySelector("#table-ou tbody .ou-proj-detail-canvas");
      if (ouProjCanvas && ouProjExpandedDetail) {
        drawOuProjDetailDistribution(
          ouProjCanvas,
          ouProjExpandedDetail.market,
          ouProjExpandedDetail.player,
          ouProjExpandedDetail.line,
        );
      }
      const propsPanel = document.getElementById("panel-props");
      if (propsPanel && propsPanel.classList.contains("active") && !propsPanel.hidden) {
        scheduleRenderPropsTrends(120);
      }
      const resPanel = document.getElementById("panel-results");
      if (resPanel && resPanel.classList.contains("active") && !resPanel.hidden) {
        renderResultsTab();
      }
      const vz = document.getElementById("hh-hole-viz");
      const cv = document.getElementById("hh-hole-canvas");
      if (vz && cv && !vz.hidden && hangoutCanvasShotCount > 0) {
        drawHangoutHoleCanvas(cv, hangoutCanvasShotCount);
      }
    }, 120);
  });
  document.getElementById("outright-market")?.addEventListener("change", () => buildOutrightsTable());
  document.getElementById("matchups-market")?.addEventListener("change", () => buildMatchupsTable());
  ["ev-filter-golfer", "ev-filter-market", "ev-filter-books-select"].forEach((id) =>
    document.getElementById(id)?.addEventListener("change", () => buildEvTable()),
  );
  {
    let evGolferSearchDebounce = null;
    document.getElementById("ev-filter-golfer-search")?.addEventListener("input", () => {
      clearTimeout(evGolferSearchDebounce);
      evGolferSearchDebounce = setTimeout(() => buildEvTable(), 140);
    });
  }
  ["results-filter-market", "results-filter-book", "results-filter-pricing-mode"].forEach((id) =>
    document.getElementById(id)?.addEventListener("change", () => {
      if (id === "results-filter-market" && resultsTimeRange !== "all") {
        resultsTimeRange = "all";
        syncResultsRangePillsUi();
      }
      if (id === "results-filter-book") syncResultsBookLogoUi();
      renderResultsTab();
    }),
  );
  document.getElementById("results-bankroll-dollars")?.addEventListener("input", () => renderResultsTab());
  document.getElementById("results-bankroll-dollars")?.addEventListener("change", () => renderResultsTab());
  document.getElementById("panel-results")?.addEventListener("click", (e) => {
    const btn = e.target && /** @type {HTMLElement} */ (e.target).closest?.(".results-range-pill[data-results-range]");
    if (!btn) return;
    const r = btn.getAttribute("data-results-range");
    if (!r) return;
    resultsTimeRange = r;
    syncResultsRangePillsUi();
    renderResultsTab();
  });
  document.getElementById("results-filter-min-ev")?.addEventListener("input", () => renderResultsTab());
  document.getElementById("results-filter-min-ev")?.addEventListener("change", () => renderResultsTab());
  document.getElementById("ev-bankroll")?.addEventListener("input", () => buildEvTable());
  document.getElementById("ev-bankroll")?.addEventListener("change", () => buildEvTable());
  document.getElementById("ev-filter-max-odds")?.addEventListener("input", () => buildEvTable());
  document.getElementById("ev-filter-max-odds")?.addEventListener("change", () => buildEvTable());
  document.getElementById("ev-boost")?.addEventListener("change", () => buildEvTable());
  document.getElementById("ev-boost-pct")?.addEventListener("input", () => buildEvTable());
  document.getElementById("ev-boost-pct")?.addEventListener("change", () => buildEvTable());
  document.getElementById("btn-ev-devig")?.addEventListener("click", () => openEvDevigDialog());
  document.getElementById("btn-ev-help")?.addEventListener("click", () => openEvHelpDialog());
  document.getElementById("ev-help-close-x")?.addEventListener("click", () => closeEvHelpDialog());
  document.getElementById("ev-help-dismiss")?.addEventListener("click", () => closeEvHelpDialog());
  document.getElementById("ev-help-dialog")?.addEventListener("click", (e) => {
    if (e.target && /** @type {HTMLElement} */ (e.target).id === "ev-help-dialog") closeEvHelpDialog();
  });
  document.querySelectorAll(".tab-help-btn[data-tab-help]").forEach((btn) => {
    btn.addEventListener("click", () => {
      const k = btn.getAttribute("data-tab-help");
      if (k) document.getElementById(`tab-help-dialog-${k}`)?.showModal();
    });
  });
  document.querySelectorAll("dialog.tab-help-dialog").forEach((dlg) => {
    dlg.querySelector(".tab-help-close-x")?.addEventListener("click", () => dlg.close());
    dlg.querySelector(".tab-help-dismiss")?.addEventListener("click", () => dlg.close());
    dlg.addEventListener("click", (e) => {
      if (e.target === dlg) dlg.close();
    });
  });
  document.getElementById("ev-devig-close-x")?.addEventListener("click", () => closeEvDevigDialog());
  document.getElementById("ev-devig-dismiss")?.addEventListener("click", () => closeEvDevigDialog());
  document.getElementById("ev-devig-search")?.addEventListener("input", (e) => {
    filterEvDevigBySearch(/** @type {HTMLInputElement} */ (e.target).value);
  });
  document.getElementById("ev-cm-market")?.addEventListener("change", () => {
    if (document.getElementById("ev-cm-market")?.checked) clearEvDevigTileSelection();
  });
  document.getElementById("ev-cm-split")?.addEventListener("change", () => {
    if (document.getElementById("ev-cm-split")?.checked) clearEvDevigTileSelection();
  });
  document.getElementById("ev-devig-quick-market")?.addEventListener("click", () => {
    const m = document.getElementById("ev-cm-market");
    if (m) m.checked = true;
    clearEvDevigTileSelection();
    document.querySelectorAll(".ev-devig-split-cb").forEach((cb) => {
      cb.checked = false;
    });
    document.querySelectorAll(".ev-devig-split-pct").forEach((inp) => {
      inp.value = "";
    });
  });
  document.getElementById("ev-devig-quick-fddk")?.addEventListener("click", () => {
    const sp = document.getElementById("ev-cm-split");
    if (sp) sp.checked = true;
    clearEvDevigTileSelection();
    document.querySelectorAll(".ev-devig-split-cb").forEach((cb) => {
      cb.checked = false;
    });
    document.querySelectorAll(".ev-devig-split-pct").forEach((inp) => {
      inp.value = "";
    });
    for (const k of ["fanduel", "draftkings"]) {
      const cb = document.querySelector(`#ev-devig-split-list .ev-devig-split-cb[value="${k}"]`);
      if (cb) cb.checked = true;
      const inp = document.querySelector(`#ev-devig-split-list .ev-devig-split-pct[data-book="${k}"]`);
      if (inp) inp.value = "50";
    }
  });
  document.getElementById("ev-devig-apply")?.addEventListener("click", () => {
    saveEvDevigPrefs(readEvDevigFormToPrefs());
    closeEvDevigDialog();
    buildEvTable();
    buildMatchupAnalysisTool();
  });
  document.getElementById("ev-devig-clear")?.addEventListener("click", () => {
    saveEvDevigPrefs({ method: "none", consensusMode: "market", singleBook: "", splitBooks: [], weights: null });
    syncEvDevigFormFromPrefs();
    buildEvTable();
    buildMatchupAnalysisTool();
  });
  document.getElementById("ev-devig-dialog")?.addEventListener("click", (e) => {
    if (e.target && /** @type {HTMLElement} */ (e.target).id === "ev-devig-dialog") closeEvDevigDialog();
  });
  const propsIds = [
    "prop-golfer",
    "prop-stat",
    "props-filter-current-course",
    "props-filter-temp-min",
    "props-filter-temp-max",
    "props-filter-wind-range",
    "props-filter-humidity-range",
    "props-filter-course",
    "props-filter-course-window",
    "props-filter-date-from",
    "props-filter-date-to",
    "props-window-n",
  ];
  propsIds.forEach((id) => {
    const el = document.getElementById(id);
    if (!el) return;
    el.addEventListener("change", () => scheduleRenderPropsTrends());
    if (
      id === "props-filter-current-course" ||
      id === "props-filter-temp-min" ||
      id === "props-filter-temp-max" ||
      id === "props-filter-wind-range" ||
      id === "props-filter-humidity-range" ||
      id === "props-filter-date-from" ||
      id === "props-filter-date-to"
    ) {
      el.addEventListener("input", () => scheduleRenderPropsTrends());
    }
  });
  document.getElementById("props-filter-course-window")?.addEventListener("change", () => {
    if (!propsCourseWindowModeOn()) propsCourseWindowDateDefaultsCourseTracked = "";
    renderPropsTrendsNow();
  });
  document.getElementById("props-top-hits-emoji-toggle")?.addEventListener("click", () => {
    propsTopHitsFitMode = propsTopHitsFitMode === "fire" ? "ice" : "fire";
    renderPropsTrendsNow();
  });
  function syncPropLineInputFromValue(el) {
    if (!el) return;
    const sk = statKeyFromPropSelect();
    const v = clampPropLineForMarket(sk, snapPropLineToDotFive(el.value));
    if (Number.isFinite(v)) el.value = formatPropLineValueForInput(v);
  }
  document.getElementById("prop-line")?.addEventListener("change", (e) => {
    const el = /** @type {HTMLInputElement} */ (e.target);
    syncPropLineInputFromValue(el);
    lockPropsTrendLineContextToCurrentFilter();
    renderPropsTrendsNow();
  });
  document.getElementById("prop-line")?.addEventListener("input", () => {
    lockPropsTrendLineContextToCurrentFilter();
    scheduleRenderPropsTrends();
  });
  document.getElementById("prop-line")?.addEventListener("blur", (e) => {
    syncPropLineInputFromValue(/** @type {HTMLInputElement} */ (e.target));
  });
  function bumpNumberInput(inputEl, direction) {
    if (!inputEl) return;
    if (direction > 0 && typeof inputEl.stepUp === "function") inputEl.stepUp(1);
    else if (direction < 0 && typeof inputEl.stepDown === "function") inputEl.stepDown(1);
    else inputEl.value = String(num(inputEl.value, 0) + direction);
    const min = num(inputEl.min, NaN);
    const max = num(inputEl.max, NaN);
    let v = num(inputEl.value, NaN);
    if (Number.isFinite(min)) v = Math.max(min, v);
    if (Number.isFinite(max)) v = Math.min(max, v);
    if (Number.isFinite(v)) inputEl.value = String(v);
  }
  function syncPropsLineStep() {
    const lineEl = document.getElementById("prop-line");
    if (!lineEl) return;
    lineEl.step = "0.5";
  }
  function bumpPropsWindowN(delta) {
    const el = document.getElementById("props-window-n");
    if (!el) return;
    let v = Math.round(num(el.value, PROPS_HISTORY_ROUND_DEFAULT));
    if (!Number.isFinite(v)) v = PROPS_HISTORY_ROUND_DEFAULT;
    v = clamp(v + delta, PROPS_HISTORY_ROUND_MIN, PROPS_HISTORY_ROUND_MAX);
    el.value = String(v);
  }
  /** Capture phase + high z-index on sidebar: chart/canvas stacking was winning hit-testing over the steppers. */
  document.body.addEventListener(
    "click",
    (ev) => {
      const raw = ev.target;
      if (!(raw instanceof Node)) return;
      const el =
        raw.nodeType === Node.TEXT_NODE ? /** @type {Text} */ (raw).parentElement : /** @type {Element | null} */ (raw);
      if (!el || !(el instanceof Element)) return;
      const btn = el.closest("button");
      if (!btn || !btn.id) return;
      if (
        btn.id !== "props-win-minus" &&
        btn.id !== "props-win-plus" &&
        btn.id !== "props-line-minus" &&
        btn.id !== "props-line-plus"
      ) {
        return;
      }
      ev.preventDefault();
      hidePropsChartTooltip();
      if (btn.id === "props-win-minus") {
        bumpPropsWindowN(-1);
        lockPropsTrendLineContextToCurrentFilter();
        renderPropsTrends();
        return;
      }
      if (btn.id === "props-win-plus") {
        bumpPropsWindowN(1);
        lockPropsTrendLineContextToCurrentFilter();
        renderPropsTrends();
        return;
      }
      syncPropsLineStep();
      const lineInp = document.getElementById("prop-line");
      const sk = statKeyFromPropSelect();
      const cur = clampPropLineForMarket(sk, snapPropLineToDotFive(lineInp?.value));
      const base = Number.isFinite(cur) ? cur : defaultPropLineForStat(sk);
      const v = clampPropLineForMarket(sk, btn.id === "props-line-minus" ? base - 1 : base + 1);
      if (lineInp) lineInp.value = formatPropLineValueForInput(v);
      lockPropsTrendLineContextToCurrentFilter();
      renderPropsTrendsNow();
    },
    true
  );
  document.getElementById("hh-hole")?.addEventListener("change", () => updateHangout());
  document.getElementById("hh-player")?.addEventListener("change", () => {
    void ensurePlayerHistoryLoadedForTab("hangout");
    scheduleHangoutSimulateDebounced();
  });
  document.getElementById("hh-sim-run")?.addEventListener("click", () => runHangoutSimulate());
  document.getElementById("hh-odds-mode-prob")?.addEventListener("click", () => setHangoutOddsViewMode(false));
  document.getElementById("hh-odds-mode-price")?.addEventListener("click", () => setHangoutOddsViewMode(true));
  document.getElementById("hh-use-live")?.addEventListener("change", () => {
    scheduleHangoutSimulateDebounced();
  });
  const hhLiveDebounceIds = ["hh-shot-num", "hh-dist-yds", "hh-lie", "hh-putt-ft"];
  hhLiveDebounceIds.forEach((id) => {
    const el = document.getElementById(id);
    if (!el) return;
    el.addEventListener("input", () => onHangoutLiveFieldChanged());
    el.addEventListener("change", () => onHangoutLiveFieldChanged());
  });
  const trendCanvas = document.getElementById("props-trend-canvas");
  function updatePropsTrendChartHover(canvas, ev) {
    if (!canvas || !propsChartHitRegions.length) {
      if (canvas) canvas.style.cursor = "";
      if (!propsChartTooltipPinned) hidePropsChartTooltip();
      return;
    }
    const { x, y } = canvasCoordsFromEvent(canvas, ev);
    const hit = pickPropsChartHit(x, y);
    canvas.style.cursor = hit ? "pointer" : "default";
    if (propsChartTooltipPinned) return;
    if (!hit) {
      hidePropsChartTooltip();
      return;
    }
    showPropsChartTooltip(canvas, ev, hit);
  }
  function pinPropsTrendChartTooltip(canvas, ev) {
    if (!canvas || !propsChartHitRegions.length) return;
    const { x, y } = canvasCoordsFromEvent(canvas, ev);
    const hit = pickPropsChartHit(x, y);
    if (!hit) {
      hidePropsChartTooltip();
      return;
    }
    propsChartTooltipPinned = true;
    showPropsChartTooltip(canvas, ev, hit);
  }
  function leavePropsTrendChart(canvas) {
    if (canvas) canvas.style.cursor = "";
    if (!propsChartTooltipPinned) hidePropsChartTooltip();
  }
  if (trendCanvas) {
    if (window.PointerEvent) {
      trendCanvas.addEventListener("pointermove", (ev) => updatePropsTrendChartHover(trendCanvas, ev));
      trendCanvas.addEventListener("pointerdown", (ev) => pinPropsTrendChartTooltip(trendCanvas, ev));
      trendCanvas.addEventListener("pointerleave", () => leavePropsTrendChart(trendCanvas));
    } else {
      trendCanvas.addEventListener("mousemove", (ev) => updatePropsTrendChartHover(trendCanvas, ev));
      trendCanvas.addEventListener("click", (ev) => pinPropsTrendChartTooltip(trendCanvas, ev));
      trendCanvas.addEventListener("mouseleave", () => leavePropsTrendChart(trendCanvas));
      trendCanvas.addEventListener(
        "touchstart",
        (ev) => {
          if (ev.touches.length !== 1) return;
          const t = ev.touches[0];
          pinPropsTrendChartTooltip(trendCanvas, t);
        },
        { passive: true }
      );
    }
  }
  document.getElementById("prop-stat")?.addEventListener("change", () => syncPropsLineStep());
  syncPropsLineStep();
  document.addEventListener("click", (e) => {
    if (e.target instanceof Element && e.target.closest("#props-trend-canvas")) return;
    hidePropsChartTooltip();
  });
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState !== "visible") return;
    const pollMs = projectionsPollIntervalMs();
    const now = Date.now();
    if (pollMs > 0 && !isFileProtocol() && now - lastDocVisibleProjectionsRefetchAt > 8000) {
      lastDocVisibleProjectionsRefetchAt = now;
      void loadProjections({ silent: true, reloadSidecar: false });
      return;
    }
    if (pollMs > 0 && activeAppTabId() === "ev" && !isFileProtocol()) {
      void loadProjections({ silent: true, reloadSidecar: false });
      return;
    }
    if (datagolfLiveOverlayEnabled() && !isFileProtocol()) void fetchAndMergeDatagolfLiveInPlay({ force: true });
  });

  window.addEventListener("online", () => {
    if (isFileProtocol()) return;
    if (projectionsPollIntervalMs() > 0) void loadProjections({ silent: true, reloadSidecar: false });
  });

  window.addEventListener("pageshow", (ev) => {
    if (!ev.persisted || isFileProtocol()) return;
    if (projectionsPollIntervalMs() <= 0) return;
    lastDocVisibleProjectionsRefetchAt = Date.now();
    void loadProjections({ silent: true, reloadSidecar: false });
  });

  void (async () => {
    await loadProjections();
    startProjectionsPolling();
  })();
});
