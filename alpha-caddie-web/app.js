/**
 * AlphaCaddie — demo grid is bundled; over HTTP the app loads projections.json (same schema as
 * scripts/export_projections_for_website.R after round_projections.R → simulated_round_static.rds).
 * Export writes both website/public/data/projections.json and alpha-caddie-web/projections.json.
 * Auto-refresh: default every 120s (?poll=0 off, ?poll=30 for 30s). Override URL: ?projections=/path.json
 * or window.__ALPHA_CADDIE_PROJECTIONS_URL__. Round history: embedded-player-round-history.js;
 * player_round_history.json + player_shots_web.json when served over HTTP.
 */

const OU_HOLD = 0.048;
const OU_DEFAULT_ODDS_AM = -110;
const PROPS_HISTORY_ROUND_MIN = 1;
const PROPS_HISTORY_ROUND_MAX = 100;
const PROPS_HISTORY_ROUND_DEFAULT = 20;
/** Min qualifying rounds with stat data to appear in Historical Trends top-10 table (all courses). */
const PROPS_TOP_HIT_MIN_ROUNDS = 20;

const OU_LINE_RANGES = {
  "Total score": [67.5, 68.5, 69.5, 70.5, 71.5, 72.5, 73.5],
  Birdies: [0.5, 1.5, 2.5, 3.5, 4.5, 5.5],
  Pars: [8.5, 9.5, 10.5, 11.5, 12.5, 13.5],
  Bogeys: [0.5, 1.5, 2.5, 3.5, 4.5, 5.5],
  GIR: [4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5],
  "Fairways hit": [4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5, 14.5, 15.5],
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
    { player_name: "Scheffler, Scottie", line: 69.5, over_odds: -108, under_odds: -112, market: "Total Score" },
    { player_name: "McIlroy, Rory", line: 70.5, over_odds: -110, under_odds: -110, market: "Total Score" },
    { player_name: "Morikawa, Collin", line: 4.5, over_odds: -115, under_odds: -105, market: "Birdies" },
    { player_name: "Homa, Max", line: 10.5, over_odds: -110, under_odds: -118, market: "Pars" },
    { player_name: "Schauffele, Xander", line: 2.5, over_odds: -120, under_odds: -102, market: "Bogeys" },
  ];
  return {
    event_name: "Bundled demo field — edit buildDefaultProjectionsPayload() in app.js",
    course_used: "Demo venue",
    display_round_label: "",
    updated_at: "",
    source: "bundled-demo",
    outrights_odds_format: "percent",
    matchups_odds_format: "decimal",
    players,
    props,
    outrights: buildDemoOutrightsFromPlayers(players),
    matchups: buildDemoMatchupsFromPlayers(players),
  };
}

const OUTRIGHT_MARKET_KEYS = ["win", "top_5", "top_10", "top_20", "make_cut", "mc"];

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
    "datagolf",
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

// Round history: optional global from embedded-player-round-history.js (script tag before app.js in index.html)
// Assign: window.__ALPHA_CADDIE_EMBEDDED_ROUND_HISTORY__ = <object> (see embed script)

let HISTORY = { meta: {}, byDgId: {}, holesByPlayerKey: {}, _ok: false };
/** Built by npm run build:shots-web from data/all_shots_2021_2026.csv (2022+ rows). */
let SHOTS = { meta: {}, byDgId: {}, _ok: false };
let propsTrendsLineContextKey = "";
/** Last valid line used when the input is mid-edit or empty. */
let propsTrendLastGoodLine = NaN;
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

function num(v, d) {
  const n = Number(v);
  return Number.isFinite(n) ? n : d;
}

function samePlayerRound(p, round) {
  const pr = Math.round(num(p?.round, NaN));
  const rr = Math.round(num(round, NaN));
  return Number.isFinite(pr) && Number.isFinite(rr) && pr === rr;
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
    const m1 = num(p1.mu_sg ?? p1.implied_mu_sg, NaN);
    const m2 = num(p2.mu_sg ?? p2.implied_mu_sg, NaN);
    const sig = 0.35;
    let p1w = 0.5;
    if (Number.isFinite(m1) && Number.isFinite(m2)) {
      const d = (m1 - m2) / (sig * Math.SQRT2);
      p1w = normalCdf(d);
    }
    p1w = clamp(p1w, 0.06, 0.94);
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

/** Interval for silent projections reload; 0 = disabled. Default 120s when ?poll omitted. */
function projectionsPollIntervalMs() {
  if (isFileProtocol()) return 0;
  try {
    const raw = new URLSearchParams(window.location.search).get("poll");
    if (raw === null || raw === "") return 120 * 1000;
    const t = String(raw).trim().toLowerCase();
    if (t === "0" || t === "false" || t === "off" || t === "no") return 0;
    const sec = Number(raw);
    if (!Number.isFinite(sec) || sec <= 0) return 0;
    return Math.min(3600, Math.max(15, sec)) * 1000;
  } catch (_) {}
  return 120 * 1000;
}

function startProjectionsPolling() {
  window.clearInterval(projectionsPollTimerId);
  projectionsPollTimerId = 0;
  projectionsPollMs = projectionsPollIntervalMs();
  if (!projectionsPollMs) return;
  projectionsPollTimerId = window.setInterval(() => {
    loadProjections({ silent: true, reloadSidecar: false });
  }, projectionsPollMs);
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
}

function ouDisplayRoundAuto() {
  const dr = num(DATA.meta.display_round, NaN);
  if (Number.isFinite(dr) && dr >= 1 && dr <= 4) return Math.round(dr);
  try {
    const wd = (
      new Intl.DateTimeFormat("en-US", { timeZone: "America/New_York", weekday: "short" }).formatToParts(new Date()).find((p) => p.type === "weekday") || {}
    ).value;
    const map = { Wed: 1, Thu: 1, Fri: 2, Sat: 3, Sun: 4, Mon: 4, Tue: 3 };
    if (wd && map[wd]) return map[wd];
  } catch (_) {}
  return 1;
}

function getOuRound() {
  const sel = document.getElementById("lb-round");
  const v = sel ? String(sel.value) : "1";
  const n = num(v, NaN);
  if (Number.isFinite(n) && n >= 1 && n <= 4) return Math.round(n);
  return ouDisplayRoundAuto();
}

function updateRoundLabels() {
  const ar = document.getElementById("auto-round");
  const meta = DATA.meta || {};
  const dr = ouDisplayRoundAuto();
  if (ar) {
    ar.hidden = true;
    ar.textContent =
      meta.display_round_label && String(meta.display_round_label).trim()
        ? String(meta.display_round_label).replace(/\s*\([^)]*America\/New_York[^)]*\)\s*/i, "").trim()
        : `R${dr}`;
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

function updateStatusBar() {
  const el = document.getElementById("data-status-primary");
  if (!el) return;
  const m = DATA.meta || {};
  const parts = [];
  if (m.event_name) parts.push(String(m.event_name));
  if (m.course_used) parts.push(String(m.course_used));
  el.textContent = parts.join(" · ");
}

function configureRoundPickerUi() {
  const sel = document.getElementById("lb-round");
  if (!sel) return;
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

const WEATHER_DEFAULTS = Object.freeze({
  tempF: 72,
  windMph: 8,
  humidityPct: 55,
  condition: "default",
});

const WEATHER_CONDITION_AVERAGES = Object.freeze({
  default: { tempF: 72, windMph: 8, humidityPct: 55 },
  clear: { tempF: 78, windMph: 7, humidityPct: 45 },
  cloudy: { tempF: 70, windMph: 10, humidityPct: 62 },
  windy: { tempF: 66, windMph: 18, humidityPct: 58 },
  rain: { tempF: 64, windMph: 14, humidityPct: 85 },
  storm: { tempF: 60, windMph: 22, humidityPct: 92 },
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

const WEATHER_UI_IDS = [
  { temp: "ou-weather-temp", wind: "ou-weather-wind", humidity: "ou-weather-humidity", condition: "ou-weather-condition" },
  { temp: "props-weather-temp", wind: "props-weather-wind", humidity: "props-weather-humidity", condition: "props-weather-condition" },
  { temp: "ev-weather-temp", wind: "ev-weather-wind", humidity: "ev-weather-humidity", condition: "ev-weather-condition" },
  { temp: "hh-weather-temp", wind: "hh-weather-wind", humidity: "hh-weather-humidity", condition: "hh-weather-condition" },
];

const PRICING_DEFAULTS = Object.freeze({ mode: "default", skill: "sg_total" });
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
  const rawS = String(skillEl?.value || PRICING_DEFAULTS.skill).toLowerCase();
  const mode = ["default", "recent", "course", "skill"].includes(rawM) ? rawM : "default";
  const skill = ["sg_total", "sg_ott", "sg_app", "sg_arg", "sg_putt", "sg_t2g"].includes(rawS) ? rawS : "sg_total";
  return { mode, skill };
}

function syncPricingUiFromState() {
  for (const ids of PRICING_UI_IDS) {
    const modeEl = document.getElementById(ids.mode);
    const skillEl = document.getElementById(ids.skill);
    if (modeEl) modeEl.value = PRICING_STATE.mode;
    if (skillEl) skillEl.value = PRICING_STATE.skill;
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
  buildOuTable();
  buildEvTable();
  buildMatchupsTable();
  buildOutrightsTable();
  renderPropsTrends();
  updatePropsFooterEv();
  if (hangoutResultsVisible()) scheduleHangoutLiveRecompute();
}

function weatherFromUiIds(ids) {
  return {
    tempF: clamp(num(document.getElementById(ids.temp)?.value, WEATHER_DEFAULTS.tempF), 20, 120),
    windMph: clamp(num(document.getElementById(ids.wind)?.value, WEATHER_DEFAULTS.windMph), 0, 60),
    humidityPct: clamp(num(document.getElementById(ids.humidity)?.value, WEATHER_DEFAULTS.humidityPct), 0, 100),
    condition: String(document.getElementById(ids.condition)?.value || WEATHER_DEFAULTS.condition).toLowerCase(),
  };
}

function syncWeatherUiFromState() {
  for (const ids of WEATHER_UI_IDS) {
    const tempEl = document.getElementById(ids.temp);
    const windEl = document.getElementById(ids.wind);
    const humEl = document.getElementById(ids.humidity);
    const condEl = document.getElementById(ids.condition);
    if (tempEl) tempEl.value = String(Math.round(WEATHER_STATE.tempF));
    if (windEl) windEl.value = String(Math.round(WEATHER_STATE.windMph));
    if (humEl) humEl.value = String(Math.round(WEATHER_STATE.humidityPct));
    if (condEl) condEl.value = WEATHER_STATE.condition;
  }
}

function weatherDifficultyDelta() {
  const w = WEATHER_STATE;
  const tempAdj = w.tempF >= 72 ? 0.03 * (w.tempF - 72) : 0.02 * (w.tempF - 72);
  const windAdj = 0.045 * Math.max(0, w.windMph - 8);
  const humAdj = 0.012 * Math.max(0, w.humidityPct - 55);
  const sliderPart = tempAdj + windAdj + humAdj;
  if (w.condition === "default") return sliderPart;
  const condAdj = WEATHER_CONDITION_MEAN_DELTA[w.condition] ?? 0;
  return sliderPart + condAdj;
}

function weatherSigmaMultiplier() {
  const w = WEATHER_STATE;
  const windVar = 0.01 * Math.max(0, w.windMph - 8);
  const humVar = 0.0015 * Math.max(0, w.humidityPct - 55);
  if (w.condition === "default") {
    return clamp(1 + windVar + humVar, 0.9, 1.5);
  }
  const condVar = WEATHER_CONDITION_SIGMA_DELTA[w.condition] ?? 0;
  return clamp(1 + windVar + humVar + condVar, 0.9, 1.5);
}

function statWeatherMuAdjustment(market) {
  const d = weatherDifficultyDelta();
  if (!Number.isFinite(d)) return 0;
  if (market === "Total score") return d;
  if (market === "Bogeys") return 0.45 * d;
  if (market === "Birdies") return -0.5 * d;
  if (market === "GIR") return -0.4 * d;
  if (market === "Fairways hit") return -0.25 * d;
  return 0;
}

function playerSkillWeatherEdge(row) {
  const baseSg = num(row?.mu_sg ?? row?.implied_mu_sg, NaN);
  const roundSd = num(row?.round_sd, NaN);
  const sgEdge = Number.isFinite(baseSg) ? baseSg * 0.12 : 0;
  const consistencyEdge = Number.isFinite(roundSd) ? clamp((2.8 - roundSd) * 0.03, -0.06, 0.06) : 0;
  return weatherDifficultyDelta() * (sgEdge + consistencyEdge);
}

function weatherAdjustedMuSg(row) {
  const base = num(row?.mu_sg ?? row?.implied_mu_sg, NaN);
  if (!Number.isFinite(base)) return NaN;
  return base + playerSkillWeatherEdge(row);
}

function logistic(x) {
  if (!Number.isFinite(x)) return NaN;
  if (x > 40) return 1;
  if (x < -40) return 0;
  return 1 / (1 + Math.exp(-x));
}

function logit(p) {
  const pp = clamp(num(p, NaN), 1e-6, 1 - 1e-6);
  if (!Number.isFinite(pp)) return NaN;
  return Math.log(pp / (1 - pp));
}

function weatherAdjustedOutrightProb(baseP, rowPlayer, marketKey) {
  if (!Number.isFinite(baseP)) return NaN;
  const k =
    marketKey === "win"
      ? 0.35
      : marketKey === "top_5"
        ? 0.28
        : marketKey === "top_10"
          ? 0.22
          : marketKey === "top_20"
            ? 0.16
            : 0.1;
  const wShift = playerSkillWeatherEdge(rowPlayer) * k;
  const id = Math.round(num(rowPlayer?.dg_id, NaN));
  const pB = Number.isFinite(id) ? pricingModeMuSgBonus(id) : 0;
  const pShift = Number.isFinite(pB) ? pB * k * 2.35 : 0;
  return clamp(logistic(logit(baseP) + wShift + pShift), 1e-4, 1 - 1e-4);
}

function ouStatRec(market) {
  return OU_STAT_MAP[market] || OU_STAT_MAP["Total score"];
}

function sigmaForOu(market, row, rec) {
  const weatherMult = weatherSigmaMultiplier();
  if (rec.sdKey) {
    const s = num(row[rec.sdKey], NaN);
    if (Number.isFinite(s) && s > 0.05) return s * weatherMult;
    return 2.75 * weatherMult;
  }
  const mu = Math.abs(num(row[rec.field], 1));
  return Math.max(0.55, Math.sqrt(Math.max(mu, 0.2)) * 0.9 * weatherMult);
}

function modelProbOverMarket(market, row, line) {
  const rec = ouStatRec(market);
  const dgId = Math.round(num(row?.dg_id, NaN));
  const mu =
    num(row[rec.field], NaN) + statWeatherMuAdjustment(market) + pricingStatMuAdjustment(market, dgId);
  if (!Number.isFinite(mu)) return NaN;
  const sig = sigmaForOu(market, row, rec);
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
  return el && el.value ? el.value : "Total score";
}

function enforceHalfLine(v) {
  if (!Number.isFinite(v)) return NaN;
  return Math.round(v - 0.5) + 0.5;
}

function selectedOuLine() {
  const el = document.getElementById("ou-line-filter");
  const raw = num(el?.value, NaN);
  const v = enforceHalfLine(raw);
  if (el && Number.isFinite(v)) el.value = v.toFixed(1);
  return v;
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
    <div class="ou-edge-row"><span class="ou-edge ${clsO}">${formatEdgePct(edgeO)}</span><span class="ou-edge-meta"><span class="ou-o-u">o</span>${lineStr} <span class="ou-edge-odds">${oddsTxtOver}</span></span></div>
    <div class="ou-edge-row"><span class="ou-edge ${clsU}">${formatEdgePct(edgeU)}</span><span class="ou-edge-meta"><span class="ou-o-u">u</span>${lineStr} <span class="ou-edge-odds">${oddsTxtUnder}</span></span></div>
  </div>`;
}

/** Default: same order as projections (expected total ↑, or stat ↓ for props-style markets). */
let ouTableSort = { key: "stat-order", dir: 1 };
let ouTableSortInited = false;

function ouTableSortValue(playerRow, market, lineSel, pImpOverSel, pImpUnderSel, sortKey) {
  if (sortKey === "golfer") return displayGolferName(playerRow.player_name || "").toLowerCase();
  if (sortKey && sortKey.startsWith("line-")) {
    const L = parseFloat(sortKey.slice(5));
    const useCustom = lineMatchesOuHighlight(lineSel, L, market);
    const pImpOver = useCustom ? pImpOverSel : impliedProbFromAmerican(OU_DEFAULT_ODDS_AM);
    const pImpUnder = useCustom ? pImpUnderSel : impliedProbFromAmerican(OU_DEFAULT_ODDS_AM);
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
      ouTableSort.dir = key === "golfer" ? 1 : -1;
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
  const rows = DATA.players.filter((p) => samePlayerRound(p, round));
  rows.sort((a, b) => {
    const rec = ouStatRec(market);
    const va = num(a[rec.field], 1e9);
    const vb = num(b[rec.field], 1e9);
    if (market === "Total score") return va - vb;
    return vb - va;
  });
  return rows;
}

function buildOuTable() {
  const table = document.getElementById("table-ou");
  if (!table) return;
  initOuTableSortOnce();
  const viewMode = getOuViewMode();
  const market = getOuMarket();
  const lines = OU_LINE_RANGES[market] || OU_LINE_RANGES["Total score"];
  const round = getOuRound();
  const thead = table.querySelector("thead");
  const tbody = table.querySelector("tbody");
  if (!thead || !tbody) return;
  const lineInp = document.getElementById("ou-line-filter");
  if (lineInp) lineInp.step = "0.5";
  const lineSel = selectedOuLine();
  const oddsOver = selectedOuOddsById("ou-odds-over-filter");
  const oddsUnder = selectedOuOddsById("ou-odds-under-filter");
  const pImpOver = impliedProbFromAmerican(oddsOver);
  const pImpUnder = impliedProbFromAmerican(oddsUnder);

  const sortInd = `<span class="sort-ind"><span class="sort-up">▲</span><span class="sort-down">▼</span></span>`;
  const hr = document.createElement("tr");
  const th0 = document.createElement("th");
  th0.className = "sortable";
  th0.dataset.sortKey = "golfer";
  th0.innerHTML = `Golfer${sortInd}`;
  hr.appendChild(th0);
  for (const L of lines) {
    const th = document.createElement("th");
    th.className = "num sortable";
    th.dataset.sortKey = `line-${L}`;
    th.innerHTML = `${L}${sortInd}`;
    hr.appendChild(th);
  }
  thead.innerHTML = "";
  thead.appendChild(hr);

  const allRows = ouSortedPlayerRows(market, round);
  const pf = document.getElementById("ou-player-filter");
  const prevPlayerFilter = pf ? String(pf.value || "") : "";
  if (pf) {
    pf.innerHTML = "";
    const allOpt = document.createElement("option");
    allOpt.value = "";
    allOpt.textContent = "All";
    pf.appendChild(allOpt);
    for (const p of allRows) {
      const nm = String(p.player_name || "");
      const opt = document.createElement("option");
      opt.value = nm;
      opt.textContent = displayGolferName(nm);
      pf.appendChild(opt);
    }
    const names = new Set(allRows.map((p) => String(p.player_name || "")));
    if (prevPlayerFilter && names.has(prevPlayerFilter)) pf.value = prevPlayerFilter;
    else pf.value = "";
  }
  const playerFilter = pf ? String(pf.value || "") : "";
  let rows = playerFilter ? allRows.filter((p) => String(p.player_name || "") === playerFilter) : allRows.slice();

  const k = ouTableSort.key;
  const d = ouTableSort.dir;
  if (k !== "stat-order") {
    rows.sort((a, b) => {
      const va = ouTableSortValue(a, market, lineSel, pImpOver, pImpUnder, k);
      const vb = ouTableSortValue(b, market, lineSel, pImpOver, pImpUnder, k);
      if (typeof va === "string" && typeof vb === "string") return va.localeCompare(vb) * d;
      return (Number(va) - Number(vb)) * d;
    });
  }

  tbody.innerHTML = "";
  for (const p of rows) {
    const tr = document.createElement("tr");
    const nameTd = document.createElement("td");
    const rawName = String(p.player_name || "");
    nameTd.textContent = displayGolferName(rawName);
    nameTd.dataset.playerValue = rawName;
    tr.appendChild(nameTd);
    for (const L of lines) {
      const td = document.createElement("td");
      td.className = "ou-cell ou-cell-edge num";
      const useCustomOdds = lineMatchesOuHighlight(lineSel, L, market);
      if (useCustomOdds) td.classList.add("ou-line-col-highlight");
      const pOver = clampProb01(modelProbOverMarket(market, p, L));
      if (!Number.isFinite(pOver)) {
        td.textContent = "—";
      } else {
        const colOddsOver = useCustomOdds ? oddsOver : OU_DEFAULT_ODDS_AM;
        const colOddsUnder = useCustomOdds ? oddsUnder : OU_DEFAULT_ODDS_AM;
        const colPImpOver = impliedProbFromAmerican(colOddsOver);
        const colPImpUnder = impliedProbFromAmerican(colOddsUnder);
        td.innerHTML = ouCellEdgeStackHtml(market, p, L, colPImpOver, colPImpUnder, viewMode, colOddsOver, colOddsUnder);
      }
      tr.appendChild(td);
    }
    tbody.appendChild(tr);
  }

  updateOuSortIndicators();
  syncOuChartCard();
}

function isOuGolferSelected() {
  return Boolean(String(document.getElementById("ou-player-filter")?.value || "").trim());
}

/** P(over) chart only when a golfer is chosen (table row or Golfer filter). */
function syncOuChartCard() {
  const card = document.getElementById("ou-chart-card");
  if (!card) return;
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

/** Bar chart: P(over) at each line — same Market + Golfer filters as the table. */
function drawOuLineDistributionChart() {
  const canvas = document.getElementById("ou-chart-canvas");
  if (!canvas || !canvas.getContext) return;
  hideOuChartTooltip();
  ouChartHitRegions = [];
  const market = getOuMarket();
  const lines = OU_LINE_RANGES[market] || OU_LINE_RANGES["Total score"];
  const round = getOuRound();
  const allRows = ouSortedPlayerRows(market, round);
  const fp = String(document.getElementById("ou-player-filter")?.value || "");
  if (!fp.trim()) return;
  const row = allRows.find((p) => String(p.player_name || "") === fp);

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
    const pct = Number.isFinite(pOver) ? pOver * 100 : NaN;
    const cx = pad.l + (i + 0.5) * slotW;
    const x0 = cx - barW / 2;
    if (!Number.isFinite(pct)) continue;
    const y0 = yPct(pct);
    const yBase = yPct(0);
    ctx.fillStyle = pct >= 50 ? "rgba(255, 138, 138, 0.88)" : "rgba(0, 196, 107, 0.82)";
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
  return "total";
}

function projectionRowForPlayerRound(playerName, round) {
  const want = String(playerName || "").trim().toLowerCase();
  const r = num(round, 1);
  return DATA.players.find(
    (p) => String(p.player_name || "").trim().toLowerCase() === want && samePlayerRound(p, r)
  );
}

function modelProbForProp(prop) {
  const stat = propMarketToStatKey(prop.market);
  const row = projectionRowForPlayerRound(prop.player_name, getOuRound());
  if (!row) return { pOver: NaN, pUnder: NaN };
  const line = num(prop.line, NaN);
  if (!Number.isFinite(line)) return { pOver: NaN, pUnder: NaN };
  const marketLabel =
    stat === "total"
      ? "Total score"
      : stat === "birdies"
        ? "Birdies"
        : stat === "pars"
          ? "Pars"
          : stat === "bogeys"
            ? "Bogeys"
            : stat === "gir"
              ? "GIR"
              : "Fairways hit";
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
  return formatAmerican(americanFromImpliedProb(p));
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

/** Kelly stake in $: full Kelly capped at 25% of bankroll and at 1 unit (bankroll / 100). */
function evKellyDollarsFromDecimal(modelPct, dec, bankroll) {
  if (!Number.isFinite(modelPct) || modelPct <= 0 || !Number.isFinite(dec) || dec <= 1) return NaN;
  if (!Number.isFinite(bankroll) || bankroll <= 0) return NaN;
  const edge = modelPct * dec - 1;
  if (edge <= 0) return 0;
  const den = dec - 1;
  if (den <= 0) return NaN;
  const f = edge / den;
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
    const singleBook = String(j.singleBook || (consensusMode === "single" && j.books?.[0]) || "").toLowerCase();
    const splitBooks = Array.isArray(j.splitBooks)
      ? j.splitBooks.map((x) => String(x).toLowerCase())
      : consensusMode === "split" && Array.isArray(j.books)
        ? j.books.map((x) => String(x).toLowerCase())
        : [];
    const weights =
      j.weights && typeof j.weights === "object"
        ? Object.fromEntries(
            Object.entries(j.weights).map(([k, v]) => [String(k).toLowerCase(), num(v, NaN)])
          )
        : null;
    let books = null;
    let cm = consensusMode;
    if (cm === "single" && !singleBook) cm = "market";
    if (cm === "market") books = null;
    else if (cm === "single") books = singleBook ? [singleBook] : null;
    else books = splitBooks.slice();
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

function evBookAllowedInConsensus(bk, prefs) {
  const k = String(bk || "").toLowerCase();
  if (k === "datagolf") return false;
  if (!prefs || prefs.books == null) return true;
  if (prefs.books.length === 0) return false;
  return prefs.books.includes(k);
}

function evConsensusWeightForBook(bk, prefs) {
  if (!prefs || !prefs.bookWeights) return 1;
  const k = String(bk || "").toLowerCase();
  const w = num(prefs.bookWeights[k], NaN);
  if (Number.isFinite(w) && w > 0) return w;
  return 0;
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
function matchupConsensusSide(oddsObj, sideKey, prefs) {
  const items = [];
  const method = evDevigMethodValid(prefs.method) ? prefs.method : "none";
  for (const bk of Object.keys(oddsObj || {})) {
    if (!evBookAllowedInConsensus(bk, prefs)) continue;
    const wB = evConsensusWeightForBook(bk, prefs);
    if (prefs.bookWeights && wB <= 0) continue;
    const pack = oddsObj[bk];
    const d1 = num(pack?.p1, NaN);
    const d2 = num(pack?.p2, NaN);
    if (method !== "none") {
      if (!Number.isFinite(d1) || d1 <= 1 || !Number.isFinite(d2) || d2 <= 1) continue;
      const q1 = 1 / d1;
      const q2 = 1 / d2;
      const pFair = devigFairForSide(q1, q2, method, sideKey);
      if (!Number.isFinite(pFair)) continue;
      items.push({ bk, v: pFair, w: wB });
    } else {
      const d = num(pack?.[sideKey], NaN);
      if (Number.isFinite(d) && d > 1) items.push({ bk, v: d, w: wB });
    }
  }
  if (!items.length) return NaN;
  const tw = items.reduce((s, it) => s + it.w, 0);
  if (tw <= 0) return NaN;
  const blend = items.reduce((s, it) => s + it.w * it.v, 0) / tw;
  if (method === "none") {
    return blend > 1 ? 1 / blend : NaN;
  }
  return blend;
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

function evDevigSortedBookKeys() {
  return Object.keys(SPORTSBOOK_META)
    .filter((k) => k !== "datagolf")
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
    picked.push({
      bk: String(cb.value).toLowerCase(),
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
    const sk = String(document.getElementById("ev-devig-single-key")?.value || "").toLowerCase();
    return { method: m, consensusMode: "single", singleBook: sk, splitBooks: [], weights: null };
  }
  const sp = readEvDevigSplitForm();
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
  const r = getOuRound();
  const mpack = DATA.matchups || {};
  for (const mk of ["tournament_matchups", "round_matchups", "3_balls"]) {
    const list = mpack[mk] && mpack[mk].match_list;
    if (!Array.isArray(list)) continue;
    for (const m of list) {
      const id1 = Math.round(num(m.p1_dg_id, NaN));
      const id2 = Math.round(num(m.p2_dg_id, NaN));
      const row1 = DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id1 && samePlayerRound(p, r));
      const row2 = DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id2 && samePlayerRound(p, r));
      const mu1 = effectiveMuSg(row1, id1);
      const mu2 = effectiveMuSg(row2, id2);
      const p1 = matchupWinProb(mu1, mu2);
      const b1 = bestBookDecimalForSide(m.odds || {}, "p1");
      const b2 = bestBookDecimalForSide(m.odds || {}, "p2");
      const modelEv1 = Number.isFinite(b1.dec) ? p1 * b1.dec - 1 : NaN;
      const modelEv2 = Number.isFinite(b2.dec) ? (1 - p1) * b2.dec - 1 : NaN;
      const marketP1 = matchupConsensusSide(m.odds || {}, "p1", devigPrefs);
      const marketP2 = matchupConsensusSide(m.odds || {}, "p2", devigPrefs);
      rows.push({
        golfer: displayGolferName(String(m.p1_player_name || "")),
        market: mk === "tournament_matchups" ? "Tournament Matchups" : mk === "round_matchups" ? "Round Matchups" : "3 Balls",
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
        market: mk === "tournament_matchups" ? "Tournament Matchups" : mk === "round_matchups" ? "Round Matchups" : "3 Balls",
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
  for (const mk of ["win", "top_5", "top_10", "top_20", "make_cut", "mc"]) {
    const pack = opack[mk];
    if (!pack || !Array.isArray(pack.rows)) continue;
    const books = Array.isArray(pack.bookKeys) ? pack.bookKeys.filter((k) => k && k !== "datagolf") : [];
    for (const row of pack.rows) {
      const id = Math.round(num(row.dg_id, NaN));
      const prow =
        DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id && samePlayerRound(p, r)) ||
        DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id);
      const modelP = modelProbOutrightMarket(prow || {}, mk);
      let bestBook = "";
      let bestAm = NaN;
      let bestEv = NaN;
      const modelOk = Number.isFinite(modelP) && modelP > 0;
      for (const bk of books) {
        const pct = impliedPctFromBookField(row[bk]);
        if (!Number.isFinite(pct) || pct <= 0 || !modelOk) continue;
        const pBook = pct / 100;
        if (pBook <= 0 || pBook >= 1) continue;
        const ev = modelP / pBook - 1;
        if (!Number.isFinite(bestEv) || ev > bestEv) {
          bestEv = ev;
          bestBook = bk;
          bestAm = americanFromImpliedProb(pBook);
        }
      }
      const decItems = [];
      for (const bk of books) {
        if (!evBookAllowedInConsensus(bk, devigPrefs)) continue;
        const pct = impliedPctFromBookField(row[bk]);
        if (!Number.isFinite(pct) || pct <= 0) continue;
        const pp = pct / 100;
        if (pp <= 0 || pp >= 1) continue;
        decItems.push({ bk, dec: 1 / pp });
      }
      const marketP = outrightConsensusProbFromBooks(decItems, devigPrefs);
      const bestDec = Number.isFinite(bestAm) ? decimalFromAmerican(bestAm) : NaN;
      rows.push({
        golfer: displayGolferName(String(row.player_name || "")),
        market:
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
                    : "Outright Miss Cut",
        bet: mk === "mc" ? "Miss Cut" : mk === "make_cut" ? "Make Cut" : mk.replace("_", " ").toUpperCase(),
        modelPct: modelP,
        modelEv: bestEv,
        bestBook,
        bestBookOdds: Number.isFinite(bestAm) ? formatAmerican(bestAm) : "—",
        bestDec,
        consensusP: marketP,
      });
    }
  }
  return rows;
}

function fillEvFilters(rows) {
  const g = document.getElementById("ev-filter-golfer");
  const m = document.getElementById("ev-filter-market");
  const b = document.getElementById("ev-filter-book");
  if (!g || !m || !b) return;
  const gPrev = g.value;
  const mPrev = m.value;
  const bPrev = b.value;
  const gSet = new Set(rows.map((r) => r.golfer).filter(Boolean));
  const mSet = new Set(rows.map((r) => r.market).filter(Boolean));
  const bSet = new Set(rows.map((r) => r.bestBook).filter(Boolean));
  const refill = (sel, vals) => {
    sel.innerHTML = '<option value="">All</option>';
    [...vals].sort((a, c) => String(a).localeCompare(String(c))).forEach((v) => {
      const op = document.createElement("option");
      op.value = String(v);
      op.textContent = sel === b ? bookMeta(v).label : String(v);
      sel.appendChild(op);
    });
  };
  refill(g, gSet);
  refill(m, mSet);
  refill(b, bSet);
  if ([...g.options].some((o) => o.value === gPrev)) g.value = gPrev;
  if ([...m.options].some((o) => o.value === mPrev)) m.value = mPrev;
  if ([...b.options].some((o) => o.value === bPrev)) b.value = bPrev;
}

function buildEvTable() {
  const tbody = document.querySelector("#table-ev tbody");
  if (!tbody) return;
  syncEvBoostPctInputDisabled();
  const rows = collectUnifiedEvRows();
  fillEvFilters(rows);
  const g = String(document.getElementById("ev-filter-golfer")?.value || "");
  const m = String(document.getElementById("ev-filter-market")?.value || "");
  const b = String(document.getElementById("ev-filter-book")?.value || "");
  const bankroll = num(document.getElementById("ev-bankroll")?.value, 1000);
  const boostPct = evProfitBoostPctFromUi();
  const modelEvWithBoost = (r) => {
    const d0 = num(r.bestDec, NaN);
    const d = decimalWithProfitBoost(d0, boostPct);
    return Number.isFinite(d) && d > 1 && Number.isFinite(r.modelPct) ? r.modelPct * d - 1 : NaN;
  };
  const out = rows
    .filter((r) => (!g || r.golfer === g) && (!m || r.market === m) && (!b || r.bestBook === b))
    .sort((a, c) => num(modelEvWithBoost(c), -1e9) - num(modelEvWithBoost(a), -1e9));
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
  const evDash = (s) => (s === "—" ? "" : s);
  for (const r of out) {
    const tr = document.createElement("tr");
    const mkTd = (txt, cls = "") => {
      const td = document.createElement("td");
      if (cls) td.className = cls;
      td.textContent = txt;
      return td;
    };
    const dec0 = num(r.bestDec, NaN);
    const dec = decimalWithProfitBoost(dec0, boostPct);
    const bookImp = Number.isFinite(dec) && dec > 1 ? 1 / dec : NaN;
    const impliedStr = Number.isFinite(bookImp) ? `${(bookImp * 100).toFixed(1)}%` : "";
    let deltaStr = "";
    if (Number.isFinite(r.modelPct) && Number.isFinite(bookImp)) {
      const dPct = (r.modelPct - bookImp) * 100;
      deltaStr = `${dPct >= 0 ? "+" : ""}${dPct.toFixed(1)}%`;
    }
    const kelly$ = evKellyDollarsFromDecimal(r.modelPct, dec, bankroll);
    const kellyStr = Number.isFinite(kelly$) ? `$${kelly$.toFixed(2)}` : "";

    const mEv = modelEvWithBoost(r);
    const modelEvTd = mkTd(Number.isFinite(mEv) ? `${(mEv * 100).toFixed(1)}%` : "", "num");
    if (Number.isFinite(mEv)) modelEvTd.classList.add(mEv >= 0 ? "ev-pos" : "ev-neg");
    tr.appendChild(modelEvTd);
    tr.appendChild(mkTd(kellyStr, "num"));
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
    tr.appendChild(mkTd(r.golfer));
    tr.appendChild(mkTd(evDash(modelAmericanFromProb(r.modelPct)), "num"));
    tr.appendChild(mkTd(evDash(modelAmericanFromProb(r.consensusP)), "num"));
    tr.appendChild(mkTd(impliedStr, "num"));
    tr.appendChild(mkTd(deltaStr, "num"));
    tr.appendChild(mkTd(r.market));
    tr.appendChild(mkTd(r.bet));
    tbody.appendChild(tr);
  }
}

function matchupWinProb(mu1, mu2) {
  if (!Number.isFinite(mu1) || !Number.isFinite(mu2)) return 0.5;
  const sig = 0.35;
  const d = (mu1 - mu2) / (sig * Math.SQRT2);
  return clamp(normalCdf(d), 0.04, 0.96);
}

function bestBookDecimalForSide(oddsObj, side /* 'p1'|'p2' */) {
  if (!oddsObj || typeof oddsObj !== "object") return { book: "", dec: NaN };
  let bestD = NaN;
  let bestB = "";
  for (const bk of Object.keys(oddsObj)) {
    if (bk === "datagolf") continue;
    const pack = oddsObj[bk];
    if (!pack || typeof pack !== "object") continue;
    const d = num(pack[side], NaN);
    if (!Number.isFinite(d) || d <= 1) continue;
    if (!Number.isFinite(bestD) || d > bestD) {
      bestD = d;
      bestB = bk;
    }
  }
  return { book: bestB, dec: bestD };
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
      note.hidden = false;
      note.textContent = String(list);
    }
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 5;
    td.className = "text-muted";
    td.textContent = String(list);
    tr.appendChild(td);
    tbody.appendChild(tr);
    return;
  }
  if (note) note.hidden = true;
  if (!Array.isArray(list) || !list.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 5;
    td.textContent = "No matchups.";
    tr.appendChild(td);
    tbody.appendChild(tr);
    return;
  }
  const r = getOuRound();
  for (const m of list) {
    const id1 = Math.round(num(m.p1_dg_id, NaN));
    const id2 = Math.round(num(m.p2_dg_id, NaN));
    const row1 = DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id1 && samePlayerRound(p, r));
    const row2 = DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id2 && samePlayerRound(p, r));
    const mu1 = effectiveMuSg(row1, id1);
    const mu2 = effectiveMuSg(row2, id2);
    const p1m = matchupWinProb(mu1, mu2);
    const odds = m.odds || {};
    const b1 = bestBookDecimalForSide(odds, "p1");
    const b2 = bestBookDecimalForSide(odds, "p2");
    const ev1 = Number.isFinite(b1.dec) ? p1m * b1.dec - 1 : NaN;
    const ev2 = Number.isFinite(b2.dec) ? (1 - p1m) * b2.dec - 1 : NaN;
    const label = `${m.p1_player_name || ""} vs ${m.p2_player_name || ""}`;
    function row(side, name, modelPct, ev, bb) {
      const tr = document.createElement("tr");
      const td0 = document.createElement("td");
      if (side === 1) {
        td0.rowSpan = 2;
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
    row(1, m.p1_player_name, p1m, ev1, b1);
    row(2, m.p2_player_name, 1 - p1m, ev2, b2);
  }
}

function impliedPctFromBookField(v) {
  const p = bookImpliedProb01(v);
  if (!Number.isFinite(p) || p <= 0 || p >= 1) return NaN;
  return p * 100;
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
  let v = num(rowPlayer[col], NaN);
  if (marketKey === "mc" && Number.isFinite(v)) v = 1 - v;
  if (!Number.isFinite(v)) return NaN;
  const p = v > 1.5 ? v / 100 : v;
  return weatherAdjustedOutrightProb(p, rowPlayer, marketKey);
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
  const r1 = getOuRound();
  const rows = pack.rows.map((row) => {
    const id = Math.round(num(row.dg_id, NaN));
    const prow = DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id && samePlayerRound(p, r1)) ||
      DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id);
    const modelP = modelProbOutrightMarket(prow || {}, mk);
    let bestBook = "";
    let bestAm = NaN;
    let bestEv = NaN;
    for (const bk of bookKeys) {
      const pct = impliedPctFromBookField(row[bk]);
      if (!Number.isFinite(pct) || pct <= 0) continue;
      const pBook = pct / 100;
      if (pBook <= 0 || pBook >= 1) continue;
      if (!Number.isFinite(modelP) || modelP <= 0) continue;
      const ev = modelP / pBook - 1;
      const am = americanFromImpliedProb(pBook);
      if (!Number.isFinite(bestEv) || ev > bestEv) {
        bestEv = ev;
        bestBook = bk;
        bestAm = am;
      }
    }
    return { row, modelP, bestBook, bestAm, bestEv };
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

/**
 * Prefer player_round_history.json (npm run build:history from historical_rounds_all.csv) when served
 * over HTTP; use embedded script as fallback or for file:// demos.
 */
async function loadPlayerHistory() {
  if (isFileProtocol()) {
    const emb = embeddedRoundHistoryPayload();
    if (emb) {
      HISTORY = { ...emb, _ok: true };
      return;
    }
    HISTORY = { meta: {}, byDgId: {}, holesByPlayerKey: {}, _ok: false };
    return;
  }
  try {
    const res = await fetch("player_round_history.json", { cache: "no-store" });
    if (res.ok) {
      HISTORY = { ...(await res.json()), _ok: true };
      return;
    }
  } catch (_) {}
  const emb = embeddedRoundHistoryPayload();
  if (emb) {
    HISTORY = { ...emb, _ok: true };
    return;
  }
  HISTORY = { meta: {}, byDgId: {}, holesByPlayerKey: {}, _ok: false };
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
    const n = nm(p.player_name);
    if (n.includes("scheffler") && n.includes("scottie")) return Math.round(num(p.dg_id, NaN));
  }
  return NaN;
}

function fillPropGolferSelect() {
  const sel = document.getElementById("prop-golfer");
  if (!sel) return;
  const seen = new Set();
  const opts = [];
  for (const p of DATA.players) {
    if (!samePlayerRound(p, 1)) continue;
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id) || seen.has(id)) continue;
    seen.add(id);
    opts.push({ id, name: String(p.player_name || "") });
  }
  opts.sort((a, b) => displayGolferName(a.name).localeCompare(displayGolferName(b.name)));
  const cur = sel.value;
  sel.innerHTML = "";
  for (const o of opts) {
    const op = document.createElement("option");
    op.value = String(o.id);
    op.textContent = displayGolferName(o.name);
    sel.appendChild(op);
  }
  const defaultId = defaultPropGolferDgId();
  if (cur && [...sel.options].some((o) => o.value === cur)) sel.value = cur;
  else if (Number.isFinite(defaultId) && [...sel.options].some((o) => o.value === String(defaultId))) {
    sel.value = String(defaultId);
  } else if (sel.options.length) sel.selectedIndex = 0;
}

function selectedDgId() {
  const sel = document.getElementById("prop-golfer");
  return sel ? Math.round(num(sel.value, NaN)) : NaN;
}

function statKeyFromPropSelect() {
  const sel = document.getElementById("prop-stat");
  return sel ? sel.value : "total";
}

function historyRoundsForDg(dgId) {
  const rec = HISTORY.byDgId && HISTORY.byDgId[String(dgId)];
  if (!rec || !Array.isArray(rec.rounds)) return [];
  return rec.rounds.slice();
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

function courseFilterOn() {
  const cb = document.getElementById("props-filter-current-course");
  return cb && cb.checked;
}

function venueCourseName() {
  return String(DATA.meta.course_used || "").trim().toLowerCase();
}

function selectedPropsTempRangeFilter() {
  return String(document.getElementById("props-filter-temp-range")?.value || "").trim().toLowerCase();
}

function selectedPropsWindRangeFilter() {
  return String(document.getElementById("props-filter-wind-range")?.value || "").trim().toLowerCase();
}

function selectedPropsHumidityRangeFilter() {
  return String(document.getElementById("props-filter-humidity-range")?.value || "").trim().toLowerCase();
}

function selectedPropsCourseFilter() {
  return String(document.getElementById("props-filter-course")?.value || "").trim().toLowerCase();
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
  const courseSel = document.getElementById("props-filter-course");
  if (!courseSel) return;
  const rounds = historyRoundsForDg(dgId).filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r));
  if (courseSel) {
    const prev = courseSel.value;
    const set = new Set();
    for (const r of rounds) {
      const cn = String(r?.course_name || "").trim();
      if (cn) set.add(cn);
    }
    courseSel.innerHTML = '<option value="">All</option>';
    [...set].sort((a, b) => a.localeCompare(b)).forEach((cn) => {
      const op = document.createElement("option");
      op.value = cn;
      op.textContent = cn;
      courseSel.appendChild(op);
    });
    if ([...courseSel.options].some((o) => o.value === prev)) courseSel.value = prev;
  }
}

/** Birdies / pars / GIR / fairways: higher is better. Round score / bogeys: higher is worse. */
function propsMarketHigherIsBetter(statKey) {
  return statKey === "birdies" || statKey === "pars" || statKey === "gir" || statKey === "fairways";
}

function propsChartInvertYAxis(statKey) {
  return statKey === "total" || statKey === "bogeys";
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
  const t = num(row.round_score, NaN);
  const countsAllZero = b === 0 && p === 0 && bg === 0 && g === 0 && f === 0;
  const noRealTotal = !Number.isFinite(t) || t <= 0;
  return countsAllZero && noRealTotal;
}

function filteredHistoryRounds(dgId) {
  let list = historyRoundsForDg(dgId);
  if (courseFilterOn() && venueCourseName()) {
    const vn = venueCourseName();
    list = list.filter((r) => String(r.course_name || "").trim().toLowerCase().includes(vn) || vn.includes(String(r.course_name || "").trim().toLowerCase()));
  }
  const courseFilter = selectedPropsCourseFilter();
  if (courseFilter) {
    list = list.filter((r) => String(r.course_name || "").trim().toLowerCase() === courseFilter);
  }
  const tempBucket = selectedPropsTempRangeFilter();
  if (tempBucket) {
    list = list.filter((r) => weatherRangeMatch("temp", tempBucket, parseWeatherNumber(r?.pga_meta_weather_temp_f ?? r?.weather_temp_f)));
  }
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
  list.sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a));
  return list;
}

function historyRoundsChronoNewestFirst(dgId) {
  const list = historyRoundsForDg(dgId).filter((r) => !historyRoundIsPlaceholderAllMarketsZero(r));
  return list.sort((a, b) => historyRoundChronoKey(b) - historyRoundChronoKey(a));
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

function courseNameMatchesVenue(courseNameRaw, venueRaw) {
  const c = String(courseNameRaw || "").trim().toLowerCase();
  const v = String(venueRaw || "").trim().toLowerCase();
  if (!c || !v) return false;
  return c.includes(v) || v.includes(c);
}

function pricingModeMuSgBonus(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return 0;
  const mode = PRICING_STATE.mode;
  if (mode === "default") return 0;

  const rounds = historyRoundsChronoNewestFirst(id);
  if (rounds.length < 4) return 0;

  const skillKey = PRICING_STATE.skill || "sg_total";

  if (mode === "recent") {
    const nRec = Math.min(6, Math.max(3, Math.floor(rounds.length / 2)));
    const recent = rounds.slice(0, nRec);
    const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 18));
    let rMean = meanNumFromRounds(recent, "sg_total");
    let oMean = meanNumFromRounds(older, "sg_total");
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      return clamp((rMean - oMean) * 0.9, -0.35, 0.35);
    }
    rMean = meanNumFromRounds(recent, "round_score");
    oMean = meanNumFromRounds(older, "round_score");
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      return clamp(((oMean - rMean) / 6) * 0.85, -0.35, 0.35);
    }
    return 0;
  }

  if (mode === "course") {
    const vn = venueCourseName();
    if (!vn) return 0;
    const here = rounds.filter((r) => courseNameMatchesVenue(r.course_name, vn));
    if (here.length < 2) return 0;
    const other = rounds.filter((r) => !courseNameMatchesVenue(r.course_name, vn));
    const hMean = meanNumFromRounds(here, "sg_total");
    const oMean = meanNumFromRounds(other.length ? other : rounds, "sg_total");
    if (Number.isFinite(hMean) && Number.isFinite(oMean)) {
      return clamp((hMean - oMean) * 0.75, -0.35, 0.35);
    }
    const hSc = meanNumFromRounds(here, "round_score");
    const oSc = meanNumFromRounds(other.length ? other : rounds, "round_score");
    if (Number.isFinite(hSc) && Number.isFinite(oSc)) {
      return clamp(((oSc - hSc) / 6) * 0.7, -0.35, 0.35);
    }
    return 0;
  }

  if (mode === "skill") {
    const sk = skillKey;
    const nRec = Math.min(8, Math.max(3, Math.floor(rounds.length / 2)));
    const recent = rounds.slice(0, nRec);
    const older = rounds.slice(nRec, Math.min(rounds.length, nRec + 24));
    const rMean = meanNumFromRounds(recent, sk);
    const oMean = meanNumFromRounds(older, sk);
    if (Number.isFinite(rMean) && Number.isFinite(oMean)) {
      return clamp((rMean - oMean) * 0.75, -0.35, 0.35);
    }
    return 0;
  }

  return 0;
}

function effectiveMuSg(row, dgIdOpt) {
  const base = weatherAdjustedMuSg(row);
  const id = Number.isFinite(dgIdOpt) ? Math.round(dgIdOpt) : Math.round(num(row?.dg_id, NaN));
  if (!Number.isFinite(base) || !Number.isFinite(id)) return base;
  return base + pricingModeMuSgBonus(id);
}

function pricingStatMuAdjustment(market, dgId) {
  const b = pricingModeMuSgBonus(dgId);
  if (!Number.isFinite(b) || b === 0) return 0;
  if (market === "Total score") return -1.05 * b;
  if (market === "Bogeys") return -0.45 * b;
  if (market === "Birdies") return 0.5 * b;
  if (market === "GIR") return 0.4 * b;
  if (market === "Fairways hit") return 0.25 * b;
  if (market === "Pars") return 0.08 * b;
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
  const temp = selectedPropsTempRangeFilter() || "all";
  const wind = selectedPropsWindRangeFilter() || "all";
  const hum = selectedPropsHumidityRangeFilter() || "all";
  const course = selectedPropsCourseFilter() || "all";
  const pm = PRICING_STATE.mode || "default";
  const ps = PRICING_STATE.skill || "sg_total";
  return `${dg}|${sk}|${courseFilterOn() ? 1 : 0}|${winN}|${temp}|${wind}|${hum}|${course}|${pm}|${ps}`;
}

/** After user changes line or steppers so projection logic does not overwrite the input. */
function lockPropsTrendLineContextToCurrentFilter() {
  propsTrendsLineContextKey = propsTrendLineContextKeyFromDom();
}

/** With “current course only”, any player with ≥1 qualifying round in the window is eligible. */
function propsTopHitMinRoundsForFilter() {
  if (courseFilterOn()) return 1;
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

function actualForRoundRow(statKey, row) {
  if (statKey === "total") return num(row.round_score, NaN);
  if (statKey === "birdies") return num(row.birdies, NaN);
  if (statKey === "pars") return num(row.pars, NaN);
  if (statKey === "bogeys") return num(row.bogies ?? row.bogeys, NaN);
  if (statKey === "gir") return num(row.gir, NaN);
  if (statKey === "fairways") return num(row.fairways, NaN);
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
  return clamp(s, 0.5, 29.5);
}

/** Label for prop-stat / history market (toolbar + hit-rate copy). */
function propMarketLabelFromKey(statKey) {
  if (statKey === "total") return "Round score";
  if (statKey === "birdies") return "Birdies";
  if (statKey === "pars") return "Pars";
  if (statKey === "bogeys") return "Bogeys";
  if (statKey === "gir") return "GIR";
  if (statKey === "fairways") return "Fairways hit";
  return String(statKey || "");
}

/**
 * Over / under vs line (strict; pushes excluded from both counts). Same window as chart.
 */
function propsFullHitStatsForDg(dgId, statKey, line, winN) {
  if (!Number.isFinite(line)) return { valid: 0, over: 0, under: 0, overRate: NaN, underRate: NaN };
  const wn = clamp(
    Math.round(num(winN, PROPS_HISTORY_ROUND_DEFAULT)),
    PROPS_HISTORY_ROUND_MIN,
    PROPS_HISTORY_ROUND_MAX
  );
  const newestFirst = filteredHistoryRounds(dgId).slice(0, wn);
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

function propsPlayerDisplayNameForDg(dgId) {
  const id = Math.round(num(dgId, NaN));
  if (!Number.isFinite(id)) return "—";
  const p =
    DATA.players.find((x) => Math.round(num(x.dg_id, NaN)) === id && samePlayerRound(x, 1)) ||
    DATA.players.find((x) => Math.round(num(x.dg_id, NaN)) === id);
  return p ? displayGolferName(p.player_name) : `DG ${id}`;
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
        ? "Showing fire side — click to show ice side"
        : "Showing ice side — click to show fire side"
    );
  }
}

function propsTopTableSortInPlace(rows) {
  const { key, dir } = propsTopTableSort;
  rows.sort((a, b) => {
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
    renderPropsTrends();
  });
}

function renderPropsHitRateAndTopTable(statKey, line, winN) {
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
  const st = propsFullHitStatsForDg(dg, statKey, line, wn);
  if (block) {
    block.hidden = false;
    block.classList.toggle("props-ou-lower-is-better", statKey === "total" || statKey === "bogeys");
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
  const minR = propsTopHitMinRoundsForFilter();
  const ids = Object.keys(HISTORY.byDgId || {})
    .map((k) => num(k, NaN))
    .filter((x) => Number.isFinite(x));
  const rows = [];
  for (const id of ids) {
    const s = propsFullHitStatsForDg(id, statKey, line, wn);
    if (s.valid < minR) continue;
    rows.push({
      dgId: id,
      name: propsPlayerDisplayNameForDg(id),
      valid: s.valid,
      over: s.over,
      under: s.under,
      overRate: s.overRate,
      underRate: s.underRate,
    });
  }
  const poolSize = rows.length;
  const filtered =
    propsTopHitsFitMode === "fire"
      ? rows.filter((r) => propsPlayerMeetsFireSide(statKey, r))
      : rows.filter((r) => propsPlayerMeetsIceSide(statKey, r));
  propsTopTableSortInPlace(filtered);
  const top = filtered.slice(0, 10);
  if (!top.length) {
    const tr = document.createElement("tr");
    const td = document.createElement("td");
    td.colSpan = 6;
    td.className = "text-muted";
    td.textContent =
      poolSize === 0
        ? `No golfers with at least ${minR} qualifying rounds for this filter.`
        : `No golfers match the selected fit (🔥 or 🧊 side) with this filter.`;
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
  else if (statKey === "gir") base = num(r.gir, NaN);
  else if (statKey === "fairways") base = num(r.fairways, NaN);
  if (!Number.isFinite(base)) return NaN;
  return base + pricingModelHistoryNudge(statKey, dgId);
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
 * FanDuel-style x-axis: a few labels when many bars; text always propsChartAxisLabel; duplicates blanked.
 * Returns Map barIndex → label string.
 */
function propsChartSparseTickLabels(dates, innerWidthPx) {
  const n = dates.length;
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
    map.set(i, propsChartAxisLabel(dates[i]));
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

/** One label per bar when there is room; duplicate M/D → suffix (no year on axis). */
function propsChartXAxisDateLabels(dates, innerW) {
  const n = dates.length;
  const map = new Map();
  if (!n) return map;
  const minPx = 38;
  const labelEveryBar = n * minPx <= innerW || n <= 20;
  if (labelEveryBar) {
    const seen = new Map();
    for (let i = 0; i < n; i++) {
      let lab = propsChartAxisLabel(dates[i]);
      const prev = seen.get(lab) || 0;
      seen.set(lab, prev + 1);
      if (prev > 0) lab = "";
      map.set(i, lab);
    }
    return map;
  }
  return propsChartSparseTickLabels(dates, innerW);
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
    if (c) return c;
  }
  return String(s?.course ?? "").trim() || "—";
}

function pointInPropsChartHitRegion(canvasX, canvasY) {
  return propsChartHitRegions.some((r) => canvasX >= r.x0 && canvasX < r.x0 + r.w && canvasY >= r.y0 && canvasY < r.y0 + r.h);
}

/** When regions overlap, pick the bar whose center is closest to x. */
function pickPropsChartHit(canvasX, canvasY) {
  let best = null;
  let bestDist = Infinity;
  for (const r of propsChartHitRegions) {
    if (canvasX < r.x0 || canvasX >= r.x0 + r.w || canvasY < r.y0 || canvasY >= r.y0 + r.h) continue;
    const mid = r.x0 + r.w / 2;
    const d = Math.abs(canvasX - mid);
    if (d < bestDist) {
      bestDist = d;
      best = r;
    }
  }
  return best;
}

function hidePropsChartTooltip() {
  const tip = document.getElementById("props-chart-tooltip");
  if (tip) tip.hidden = true;
}

function propsChartFormatValue(statKey, v) {
  if (!Number.isFinite(v)) return "—";
  return String(Math.round(v));
}

function canvasCoordsFromEvent(canvas, ev) {
  const rect = canvas.getBoundingClientRect();
  const sx = rect.width > 0 ? canvas.width / rect.width : 1;
  const sy = rect.height > 0 ? canvas.height / rect.height : 1;
  return {
    x: (ev.clientX - rect.left) * sx,
    y: (ev.clientY - rect.top) * sy,
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

/** `series` items: `{ actual, date?, _hist? }` — `_hist` is raw round row (course_name, …). */
function drawPropsTrendCanvas(series, lineY, statKey) {
  propsChartHitRegions = [];
  hidePropsChartTooltip();
  const canvas = document.getElementById("props-trend-canvas");
  const wrap = canvas?.closest(".props-trends-chart-wrap");
  if (!canvas || !canvas.getContext) return;
  const dpr = Math.min(2, window.devicePixelRatio || 1);

  function paintEmptyBackground(cssW0, cssH0) {
    canvas.width = Math.round(cssW0 * dpr);
    canvas.height = Math.round(cssH0 * dpr);
    const c0 = canvas.getContext("2d");
    if (!c0) return;
    c0.setTransform(dpr, 0, 0, dpr, 0, 0);
    c0.clearRect(0, 0, cssW0, cssH0);
    c0.fillStyle = "#0a0c0f";
    c0.fillRect(0, 0, cssW0, cssH0);
  }

  if (!series.length) {
    canvas.style.width = "100%";
    canvas.style.maxWidth = "100%";
    const vis = wrap && wrap.clientWidth > 80 ? wrap.clientWidth - 28 : 800;
    const cssH0 = Math.round(clamp(vis * 0.48, 280, 420));
    paintEmptyBackground(vis, cssH0);
    return;
  }
  const vals = series.map((s) => s.actual).filter((x) => Number.isFinite(x));
  if (!vals.length) {
    canvas.style.width = "100%";
    canvas.style.maxWidth = "100%";
    const vis = wrap && wrap.clientWidth > 80 ? wrap.clientWidth - 28 : 800;
    paintEmptyBackground(vis, Math.round(clamp(vis * 0.48, 280, 420)));
    return;
  }

  const n = series.length;
  const visibleW = wrap && wrap.clientWidth > 80 ? wrap.clientWidth - 28 : 400;
  const pad = { l: 42, r: 14, t: 12, b: n > 12 ? 54 : 46 };
  const innerW = Math.max(80, visibleW - pad.l - pad.r);
  const cssW = Math.round(visibleW);
  const cssH = Math.round(clamp(visibleW * 0.5, 300, 480));
  canvas.style.width = "100%";
  canvas.style.maxWidth = "100%";
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
  const invertY = propsChartInvertYAxis(statKey);
  function yScale(v) {
    const t = (v - minV) / (maxV - minV);
    if (invertY) return pad.t + innerH * t;
    return pad.t + innerH * (1 - t);
  }
  const yBase = invertY ? yScale(maxV) : yScale(minV);
  const yTickCount = 5;
  ctx.strokeStyle = "rgba(255, 255, 255, 0.07)";
  ctx.lineWidth = 1;
  for (let g = 0; g <= yTickCount; g++) {
    const v = minV + ((maxV - minV) * g) / yTickCount;
    const y = yScale(v);
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
  if (Number.isFinite(lineY)) {
    ctx.strokeStyle = "rgba(255, 255, 255, 0.55)";
    ctx.lineWidth = 1;
    ctx.setLineDash([4, 3]);
    ctx.beginPath();
    const yL = yScale(lineY);
    ctx.moveTo(pad.l, yL);
    ctx.lineTo(w - pad.r, yL);
    ctx.stroke();
    ctx.setLineDash([]);
    ctx.fillStyle = "rgba(180, 184, 196, 0.95)";
    ctx.font = "9px DM Sans, sans-serif";
    ctx.textAlign = "right";
    ctx.textBaseline = "bottom";
    const lineLbl = formatPropLineChartLabel(statKey, lineY);
    ctx.fillText(lineLbl, w - pad.r, yL - 3);
    ctx.textAlign = "left";
    ctx.textBaseline = "alphabetic";
  }
  const dates = series.map((s) => s.date || "");
  const { xCenter, barW } = propsChartBarLayout(series, pad.l, innerW);
  const hitPad = 5;
  const lowerIsBetter = invertY;
  for (let i = 0; i < n; i++) {
    const v = series[i].actual;
    if (!Number.isFinite(v)) continue;
    const bw = barW[i];
    const xc = xCenter[i];
    const x0 = Math.max(pad.l, Math.min(xc - bw / 2, pad.l + innerW - bw));
    const yTop = yScale(v);
    const hBar = Math.max(1, yBase - yTop);
    const xHit0 = Math.max(pad.l, x0 - hitPad);
    const xHit1 = Math.min(pad.l + innerW, x0 + bw + hitPad);
    propsChartHitRegions.push({
      x0: xHit0,
      y0: pad.t,
      w: Math.max(1, xHit1 - xHit0),
      h: yBase - pad.t,
      _hist: series[i]._hist,
      date: String(series[i].date || "").trim() || "—",
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
  ctx.fillStyle = "#8b8f9c";
  ctx.font = "9px DM Sans, sans-serif";
  ctx.textAlign = "left";
  ctx.textBaseline = "middle";
  for (let g = 0; g <= yTickCount; g++) {
    const v = minV + ((maxV - minV) * g) / yTickCount;
    const y = yScale(v);
    ctx.fillText(String(Math.round(v)), 5, y);
  }
  ctx.textBaseline = "alphabetic";
  const tickMap = propsChartXAxisDateLabels(dates, innerW);
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

function renderPropsTrends() {
  const empty = document.getElementById("props-chart-empty");
  const titleEl = document.getElementById("props-trends-title");
  const subEl = document.getElementById("props-trends-sub");
  const dg = selectedDgId();
  refreshPropsFilterOptionsForGolfer(dg);
  const statKey = statKeyFromPropSelect();
  if (propsTopTableSortStatKey !== statKey) {
    propsTopTableSort = { key: "overRate", dir: -1 };
    propsTopTableSortStatKey = statKey;
  }
  const playerRow =
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg && samePlayerRound(p, 1)) || DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg);
  if (playerRow) {
    setPropsCountryFlag(playerRow);
    if (titleEl) titleEl.textContent = displayGolferName(playerRow.player_name) || "—";
    if (subEl) subEl.textContent = DATA.meta.event_name ? String(DATA.meta.event_name) : "";
  } else {
    if (titleEl) titleEl.textContent = "—";
    if (subEl) subEl.textContent = "";
  }
  if (!HISTORY._ok) {
    if (empty) empty.hidden = false;
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
        statKey === "total" ? num(rproj?.total_score, 70.5) : num(rproj?.[statKey === "fairways" ? "fairways" : statKey], 3.5);
      lineEarly = clampPropLineForMarket(statKey, snapPropLineToDotFive(fallbackRaw));
      if (!Number.isFinite(lineEarly)) lineEarly = clampPropLineForMarket(statKey, statKey === "total" ? 70.5 : 3.5);
      if (lineInpEarly) lineInpEarly.value = formatPropLineValueForInput(lineEarly);
      propsTrendsLineContextKey = ctxKeyEarly;
    } else if (!lineEditingEarly && lineInpEarly) {
      lineInpEarly.value = formatPropLineValueForInput(lineEarly);
    }
    if (Number.isFinite(lineEarly)) propsTrendLastGoodLine = lineEarly;
    drawPropsTrendCanvas([], lineEarly, statKey);
    const wnEarly = clamp(
      Math.round(num(document.getElementById("props-window-n")?.value, PROPS_HISTORY_ROUND_DEFAULT)),
      PROPS_HISTORY_ROUND_MIN,
      PROPS_HISTORY_ROUND_MAX
    );
    renderPropsHitRateAndTopTable(statKey, lineEarly, wnEarly);
    return;
  }
  if (empty) empty.hidden = true;
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
      statKey === "total" ? num(rproj?.total_score, 70.5) : num(rproj?.[statKey === "fairways" ? "fairways" : statKey], 3.5);
    line = clampPropLineForMarket(statKey, snapPropLineToDotFive(fallbackRaw));
    if (!Number.isFinite(line)) line = clampPropLineForMarket(statKey, statKey === "total" ? 70.5 : 3.5);
    if (lineInp) lineInp.value = formatPropLineValueForInput(line);
    propsTrendsLineContextKey = ctxKey;
  } else if (!lineEditing && lineInp) {
    lineInp.value = formatPropLineValueForInput(line);
  }
  if (Number.isFinite(line)) propsTrendLastGoodLine = line;
  const newestFirst = filteredHistoryRounds(dg).slice(0, winN);
  const rawList = newestFirst.slice().sort((a, b) => historyRoundChronoKey(a) - historyRoundChronoKey(b));
  const series = [];
  for (const r of rawList) {
    const actual = actualForRoundRow(statKey, r);
    if (!Number.isFinite(actual)) continue;
    const m = modelForHistoryRow(statKey, { ...r, _playerName: playerRow?.player_name });
    series.push({
      _hist: r,
      date: r.event_completed || "",
      course: propsCourseNameFromRow(r),
      actual,
      model: m,
      dif: Number.isFinite(m) ? actual - m : NaN,
    });
  }
  drawPropsTrendCanvas(
    series.map((s) => ({ actual: s.actual, date: s.date, _hist: s._hist })),
    line,
    statKey
  );
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
  const playerRow =
    DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg && samePlayerRound(p, 1)) || DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === dg);
  const marketLabel =
    statKey === "total"
      ? "Total score"
      : statKey === "birdies"
        ? "Birdies"
        : statKey === "pars"
          ? "Pars"
          : statKey === "bogeys"
            ? "Bogeys"
            : statKey === "gir"
              ? "GIR"
              : "Fairways hit";
  const rproj = projectionRowForPlayerRound(playerRow?.player_name, getOuRound());
  const pOver = rproj && Number.isFinite(line) ? modelProbOverMarket(marketLabel, rproj, line) : NaN;
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
let hangoutLiveRefreshT = 0;

function hangoutResultsVisible() {
  const vz = document.getElementById("hh-hole-viz");
  return Boolean(vz && !vz.hidden && hangoutCanvasShotCount > 0);
}

function scheduleHangoutLiveRecompute() {
  window.clearTimeout(hangoutLiveRefreshT);
  hangoutLiveRefreshT = window.setTimeout(() => runHangoutSimulate(), 320);
}

function onHangoutLiveFieldChanged() {
  hangoutZeroYdsIfGreenLie();
  if (hangoutResultsVisible()) scheduleHangoutLiveRecompute();
  else clearHangoutSimulationResults();
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
  let rounds = needle
    ? rec.rounds.filter((r) => {
        const c = String(r.course_name || "").trim().toLowerCase();
        return c && (c.includes(needle) || needle.includes(c.slice(0, Math.min(10, c.length))));
      })
    : rec.rounds.slice(0, 120);
  const evN = String(eventName || "").trim().toLowerCase();
  if (evN && rounds.length) {
    const evF = rounds.filter((r) => {
      const en = String(r.event_name || "").trim().toLowerCase();
      return en && (en.includes(evN.slice(0, 14)) || evN.includes(en.slice(0, 10)));
    });
    if (evF.length) rounds = evF;
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
  if (lie === "Sand") lieT = 0.52;
  else if (lie === "Rough") lieT = 0.34;
  const distT = clamp((dist - 132) / 340, -0.1, 0.2) * (1 - 0.3 * phase);
  const puttT = clamp((putt - 9) / 48, -0.08, 0.16) * (0.35 + 0.65 * phase);
  const T = clamp(lieT + distT + puttT, -0.15, 1.25);
  return {
    eagle: clamp(1 - 0.55 * T, 0.3, 1.15),
    birdie: clamp(1 - 0.32 * T, 0.45, 1.12),
    par: 1,
    bogey: clamp(1 + 0.36 * T, 0.72, 1.75),
    double: clamp(1 + 0.58 * T, 0.55, 2.1),
  };
}

function hangoutAmericanForThreeWayProb(p) {
  const fair = 1 / clamp(p, 0.025, 0.975);
  const d = Math.max(1.02, fair * (1 + OU_HOLD * 0.45));
  return americanFromDecimal(d);
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

function getHangoutShotsBundleRows(dgId, holeNum1) {
  if (!SHOTS._ok) return null;
  const byRound = SHOTS.byDgId && SHOTS.byDgId[String(dgId)];
  if (!byRound) return null;
  const ev = String(DATA.meta.event_name || "").trim();
  const r = getOuRound();
  const uid = findShotsRoundUid(byRound, `${ev}\tR${r}`);
  if (!uid || !byRound[uid]) return null;
  const arr = byRound[uid][String(holeNum1)];
  if (!Array.isArray(arr) || !arr.length) return null;
  return arr;
}

function hangoutLieFromCodes(f, t) {
  const a = `${String(f || "")}${String(t || "")}`.toUpperCase();
  if (a.includes("GR")) return "Green";
  if (a.includes("FW")) return "Fairway";
  if (a.includes("SF") || a.includes("SD") || a.includes("SB")) return "Sand";
  if (String(f || "").toUpperCase() === "OTB" || a.includes("TB")) return "Tee";
  return "Rough";
}

function webShotToHangoutShot(s, idx) {
  const p = String(s.p || "");
  const isPutt = /putt/i.test(p) || (s.d == null && /\d+\s*ft/i.test(p));
  let feet = null;
  let yards = null;
  if (isPutt) {
    const mi = p.match(/(\d+)\s*ft\s*(\d+)\s*in/i);
    if (mi) feet = Math.round(parseInt(mi[1], 10) + parseInt(mi[2], 10) / 12);
    else {
      const mf = p.match(/(\d+)\s*ft/i);
      if (mf) feet = parseInt(mf[1], 10);
    }
  }
  if (!isPutt && Number.isFinite(s.d)) yards = Math.round(s.d);
  if (!isPutt && yards == null) {
    const my = p.match(/(\d+)\s*yds?\s*to/i) || p.match(/(\d+)\s*yds?/i);
    if (my) yards = parseInt(my[1], 10);
  }
  let title = "Shot";
  if (s.sn === 1) title = "Tee shot";
  else if (isPutt) title = "Putt";
  else if (idx === 1) title = "Approach";
  else title = /layup|lay up/i.test(p) ? "Layup" : "Approach";
  const lie = isPutt ? "Green" : hangoutLieFromCodes(s.f, s.t);
  const tag = s.fin ? "Hole out" : "";
  return { title, yards, feet, lie, tag };
}

function hangoutBuildShotsFromBundleOrSynth(holePar, sc, dgId, holeNum1) {
  const bundle = getHangoutShotsBundleRows(dgId, holeNum1);
  if (bundle && bundle.length === sc) {
    return bundle.map((row, i) => webShotToHangoutShot(row, i));
  }
  return hangoutBuildShots(holePar, sc);
}

function hangoutZeroYdsIfGreenLie() {
  const lie = String(document.getElementById("hh-lie")?.value || "");
  const di = document.getElementById("hh-dist-yds");
  if (lie === "Green" && di) di.value = "0";
}

function parseHoleParsMeta() {
  const hp = DATA.meta.hole_pars;
  if (Array.isArray(hp) && hp.length >= 18) return hp.slice(0, 18).map((x) => Math.round(num(x, 4)));
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
      ? '<p class="text-muted" style="margin:0;font-size:0.9rem;">Cleared — run <strong>Simulate hole</strong>.</p>'
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
  const courseLine = document.getElementById("hh-course-line");
  const parHint = document.getElementById("hh-hole-par-hint");
  if (courseLine) courseLine.textContent = DATA.meta.course_used || "—";
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
    const hi = num(holeSel.value, 1) - 1;
    if (parHint) parHint.textContent = `Selected: par ${hp[hi]}`;
  }
  if (plSel) {
    const pr = plSel.value;
    plSel.innerHTML = "";
    const seen = new Set();
    for (const p of DATA.players) {
      if (!samePlayerRound(p, getOuRound())) continue;
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
  }
}

function getHangoutPlayerRow() {
  const sel = document.getElementById("hh-player");
  const id = sel ? Math.round(num(sel.value, NaN)) : NaN;
  const r = getOuRound();
  return DATA.players.find((p) => Math.round(num(p.dg_id, NaN)) === id && samePlayerRound(p, r));
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

function hangoutLiveOn() {
  return Boolean(document.getElementById("hh-use-live")?.checked);
}

function hangoutRi(lo, hi) {
  return Math.round(lo + Math.random() * (hi - lo));
}

function hangoutScoreLabel(holePar, sc) {
  const d = sc - holePar;
  if (d <= -2) return "Eagle+";
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
    return [{ title: "Tee shot", yards: tee, lie: "Green", tag: "Hole-in-one" }];
  }
  if (sc === 2) {
    const onG = Math.random() < 0.48;
    return [
      { title: "Tee shot", yards: tee, lie: onG ? "Green" : "Fairway" },
      { title: "Putt", feet: hangoutRi(7, 22), tag: "Birdie" },
    ];
  }
  if (sc === 3) {
    if (Math.random() < 0.42) {
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
      { title: "Tee shot", yards: tee, lie: Math.random() < 0.4 ? "Green" : "Fairway" },
      { title: "Putt", feet: hangoutRi(4, 28), tag: "Eagle" },
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
      { title: "Tee shot", yards: tee, lie: Math.random() < 0.22 ? "Rough" : "Fairway" },
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
  const tee = hangoutRi(525, 595);
  if (sc <= 3) {
    return [
      { title: "Tee shot", yards: tee, lie: "Fairway" },
      { title: "Second", yards: hangoutRi(115, 185), lie: "Green" },
      { title: "Putt", feet: hangoutRi(6, 22), tag: "Eagle" },
    ];
  }
  if (sc === 4) {
    return [
      { title: "Tee shot", yards: tee, lie: "Fairway" },
      { title: "Layup", yards: hangoutRi(238, 288), lie: "Fairway" },
      { title: "Approach", yards: hangoutRi(95, 135), lie: "Green" },
      { title: "Putt", feet: hangoutRi(10, 22), tag: "Birdie" },
    ];
  }
  if (sc === 5) {
    return [
      { title: "Tee shot", yards: tee, lie: Math.random() < 0.18 ? "Rough" : "Fairway" },
      { title: "Layup", yards: hangoutRi(220, 275), lie: "Fairway" },
      { title: "Approach", yards: hangoutRi(105, 148), lie: "Green" },
      { title: "Putt", feet: hangoutRi(22, 36), lie: "" },
      { title: "Putt", feet: hangoutRi(2, 5), tag: "Par" },
    ];
  }
  if (sc === 6) {
    return [
      { title: "Tee shot", yards: tee, lie: "Fairway" },
      { title: "Second", yards: hangoutRi(255, 295), lie: "Rough" },
      { title: "Approach", yards: hangoutRi(128, 168), lie: "Green" },
      { title: "Putt", feet: hangoutRi(26, 40), lie: "" },
      { title: "Putt", feet: hangoutRi(4, 9), lie: "" },
      { title: "Putt", feet: hangoutRi(2, 4), tag: "Bogey" },
    ];
  }
  return [
    { title: "Tee shot", yards: tee, lie: "Rough" },
    { title: "Layup", yards: hangoutRi(210, 250), lie: "Fairway" },
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
      lie: Math.random() < 0.2 ? "Rough" : "Green",
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
      top.innerHTML = `<span class="hh-top-title">Hole ${holeIdx + 1} · Par ${holePar}</span><span class="hh-top-note">${DATA.meta.event_name || ""}</span>`;
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
  const probsFive = {};
  if (ssum < 1e-15) {
    const u = 0.2;
    for (const k of order) probsFive[k] = u;
  } else {
    for (const { k } of labels) probsFive[k] = raw[k] / ssum;
  }
  let three = hangoutCollapseFiveToThree(probsFive);
  three = hangoutNormThree(three);
  const hist3 = hangoutHistoryPriorThree(dgId, DATA.meta.course_used, DATA.meta.event_name);
  if (hist3) three = hangoutBlendThree(three, hist3, 0.38);
  const pBonus = pricingModeMuSgBonus(dgId);
  if (Number.isFinite(pBonus) && Math.abs(pBonus) > 1e-9) {
    const t = clamp(pBonus * 0.22, -0.09, 0.09);
    three = hangoutNormThree({
      birdie: three.birdie * Math.exp(t),
      par: three.par,
      bogeyPlus: three.bogeyPlus * Math.exp(-t),
    });
  }
  const shotN = clamp(Math.round(num(document.getElementById("hh-shot-num")?.value, 1)), 1, 18);
  const puttFt = clamp(num(document.getElementById("hh-putt-ft")?.value, 10), 2, 120);
  const liveGreen = hangoutLiveOn() && String(document.getElementById("hh-lie")?.value || "") === "Green";
  if (liveGreen) three = hangoutPuttingThreeWay(holePar, shotN, puttFt);
  three = hangoutNormThree(three);
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
    top.innerHTML = `<span class="hh-top-title">Hole ${holeIdx + 1} · Par ${holePar}</span><span class="hh-top-note">${dname} · ${DATA.meta.event_name || ""}</span>`;
  }
  hangoutRenderThreeOutcomes(three);
  const traced = getHangoutShotsBundleRows(dgId, holeNum1);
  holeCard.innerHTML = `<h4>Hole card</h4><p class="hangout-pred-score">${exp.toFixed(2)}</p><p class="text-muted">Par ${holePar}${traced ? " · traced" : " · model"}</p>`;
  const rPick = Math.random();
  let cat = "par";
  if (rPick < three.birdie) cat = "birdie";
  else if (rPick < three.birdie + three.par) cat = "par";
  else cat = "bogeyPlus";
  let sc;
  if (cat === "birdie") {
    const pe = probsFive.eagle;
    const pb = probsFive.birdie;
    const den = pe + pb + 1e-12;
    sc = Math.random() < pe / den ? holePar - 2 : holePar - 1;
  } else if (cat === "par") {
    sc = holePar;
  } else {
    const pg = probsFive.bogey;
    const pd = probsFive.double;
    const den = pg + pd + 1e-12;
    sc = Math.random() < pg / den ? holePar + 1 : holePar + 2;
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
}

function updateHangout() {
  initHangoutSelectors(false);
  const parHint = document.getElementById("hh-hole-par-hint");
  const hp = parseHoleParsMeta();
  const hi = num(document.getElementById("hh-hole")?.value, 1) - 1;
  if (parHint && hi >= 0 && hi < 18) parHint.textContent = `Selected: par ${hp[hi]}`;
  clearHangoutSimulationResults();
}

function refreshAll() {
  updateRoundLabels();
  const ot = document.getElementById("outrights-tournament");
  if (ot) ot.textContent = `Tournament: ${DATA.meta.event_name || "—"}`;
  buildOuTable();
  buildEvTable();
  buildMatchupsTable();
  buildOutrightsTable();
  fillPropGolferSelect();
  renderPropsTrends();
  initHangoutSelectors(false);
  clearHangoutSimulationResults();
}

/**
 * @param {{ silent?: boolean, reloadSidecar?: boolean }} [opts]
 *   silent: on fetch failure, keep current DATA (for background poll).
 *   reloadSidecar: fetch player_round_history.json / player_shots_web.json (initial load only).
 */
async function loadProjections(opts = {}) {
  const silent = Boolean(opts.silent);
  const reloadSidecar = opts.reloadSidecar !== false;
  if (projectionsLoadInFlight) return;
  projectionsLoadInFlight = true;
  if (!silent) setBootError("");

  const finishOk = async () => {
    if (reloadSidecar) {
      await loadPlayerHistory();
      await loadPlayerShots();
    }
    refreshAll();
    updateStatusBar();
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
    const url = projectionsJsonUrl();
    const res = await fetch(url, { cache: "no-store" });
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
  }
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
      if (tab === "ou") requestAnimationFrame(() => syncOuChartCard());
      if (tab === "hangout") {
        requestAnimationFrame(() => {
          const vz = document.getElementById("hh-hole-viz");
          const cv = document.getElementById("hh-hole-canvas");
          if (vz && cv && !vz.hidden && hangoutCanvasShotCount > 0) {
            drawHangoutHoleCanvas(cv, hangoutCanvasShotCount);
          }
        });
      }
      if (tab === "props") requestAnimationFrame(() => renderPropsTrends());
    });
  });
}

document.addEventListener("DOMContentLoaded", () => {
  function refreshAllWeatherAffectedViews() {
    buildOuTable();
    buildEvTable();
    buildMatchupsTable();
    buildOutrightsTable();
    renderPropsTrends();
    updatePropsFooterEv();
    if (hangoutResultsVisible()) scheduleHangoutLiveRecompute();
  }
  function applyWeatherFrom(ids, syncUi = true) {
    const next = weatherFromUiIds(ids);
    const avg = WEATHER_CONDITION_AVERAGES[next.condition] || WEATHER_CONDITION_AVERAGES.default;
    if (avg) {
      next.tempF = avg.tempF;
      next.windMph = avg.windMph;
      next.humidityPct = avg.humidityPct;
    }
    WEATHER_STATE = next;
    if (syncUi) syncWeatherUiFromState();
    refreshAllWeatherAffectedViews();
  }
  syncWeatherUiFromState();
  syncPricingUiFromState();
  for (const ids of PRICING_UI_IDS) {
    for (const id of [ids.mode, ids.skill]) {
      const el = document.getElementById(id);
      if (!el) continue;
      el.addEventListener("change", () => {
        PRICING_STATE = pricingFromUiIds(ids);
        syncPricingUiFromState();
        refreshPricingAffectedViews();
      });
    }
  }
  for (const ids of WEATHER_UI_IDS) {
    [ids.temp, ids.wind, ids.humidity, ids.condition].forEach((id) => {
      const el = document.getElementById(id);
      if (!el) return;
      el.addEventListener("change", () => {
        if (id === ids.condition) {
          applyWeatherFrom(ids, true);
          return;
        }
        WEATHER_STATE = weatherFromUiIds(ids);
        syncWeatherUiFromState();
        refreshAllWeatherAffectedViews();
      });
      el.addEventListener("input", () => {
        if (id === ids.condition) return;
        WEATHER_STATE = weatherFromUiIds(ids);
        syncWeatherUiFromState();
        refreshAllWeatherAffectedViews();
      });
    });
  }
  initPropsTopTableSortOnce();
  configureRoundPickerUi();
  initTabs();
  initOutrightsTableSortOnce();
  document.getElementById("btn-refresh-outrights")?.addEventListener("click", () => loadProjections());
  document.getElementById("lb-round")?.addEventListener("change", () => {
    updateRoundLabels();
    buildOuTable();
    buildEvTable();
    buildMatchupsTable();
    renderPropsTrends();
    initHangoutSelectors(false);
    clearHangoutSimulationResults();
  });
  document.getElementById("ou-market-filter")?.addEventListener("change", () => {
    ouTableSort = { key: "stat-order", dir: 1 };
    const m = getOuMarket();
    const rng = OU_LINE_RANGES[m] || OU_LINE_RANGES["Total score"];
    const inp = document.getElementById("ou-line-filter");
    if (inp && rng.length) {
      const mid = m === "Total score" ? 70.5 : enforceHalfLine(rng[Math.floor(rng.length / 2)]);
      inp.step = "0.5";
      inp.value = Number.isFinite(mid) ? Number(mid).toFixed(1) : "70.5";
    }
    buildOuTable();
  });
  document.getElementById("ou-player-filter")?.addEventListener("change", () => buildOuTable());
  document.getElementById("ou-line-filter")?.addEventListener("change", () => buildOuTable());
  document.getElementById("ou-line-filter")?.addEventListener("input", () => buildOuTable());
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
    const tr = ev.target.closest("tbody tr");
    if (!tr) return;
    const nameTd = tr.querySelector("td");
    const name = (nameTd?.dataset?.playerValue || nameTd?.textContent || "").trim();
    const s = document.getElementById("ou-player-filter");
    if (!s || !name) return;
    if (![...s.options].some((o) => o.value === name)) return;
    s.value = name;
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
  let ouChartResizeT = 0;
  window.addEventListener("resize", () => {
    window.clearTimeout(ouChartResizeT);
    ouChartResizeT = window.setTimeout(() => {
      if (isOuGolferSelected()) drawOuLineDistributionChart();
      const propsPanel = document.getElementById("panel-props");
      if (propsPanel && propsPanel.classList.contains("active") && !propsPanel.hidden) {
        renderPropsTrends();
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
  ["ev-filter-golfer", "ev-filter-market", "ev-filter-book"].forEach((id) =>
    document.getElementById(id)?.addEventListener("change", () => buildEvTable())
  );
  document.getElementById("ev-bankroll")?.addEventListener("input", () => buildEvTable());
  document.getElementById("ev-bankroll")?.addEventListener("change", () => buildEvTable());
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
  });
  document.getElementById("ev-devig-clear")?.addEventListener("click", () => {
    saveEvDevigPrefs({ method: "none", consensusMode: "market", singleBook: "", splitBooks: [], weights: null });
    syncEvDevigFormFromPrefs();
    buildEvTable();
  });
  document.getElementById("ev-devig-dialog")?.addEventListener("click", (e) => {
    if (e.target && /** @type {HTMLElement} */ (e.target).id === "ev-devig-dialog") closeEvDevigDialog();
  });
  const propsIds = [
    "prop-golfer",
    "prop-stat",
    "props-filter-current-course",
    "props-filter-temp-range",
    "props-filter-wind-range",
    "props-filter-humidity-range",
    "props-filter-course",
    "props-window-n",
  ];
  propsIds.forEach((id) => {
    const el = document.getElementById(id);
    if (!el) return;
    el.addEventListener("change", () => renderPropsTrends());
    if (id === "props-filter-current-course") el.addEventListener("input", () => renderPropsTrends());
  });
  document.getElementById("props-top-hits-emoji-toggle")?.addEventListener("click", () => {
    propsTopHitsFitMode = propsTopHitsFitMode === "fire" ? "ice" : "fire";
    renderPropsTrends();
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
    renderPropsTrends();
  });
  document.getElementById("prop-line")?.addEventListener("input", () => {
    lockPropsTrendLineContextToCurrentFilter();
    renderPropsTrends();
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
      const base = Number.isFinite(cur) ? cur : sk === "total" ? 70.5 : 3.5;
      const v = clampPropLineForMarket(sk, btn.id === "props-line-minus" ? base - 1 : base + 1);
      if (lineInp) lineInp.value = formatPropLineValueForInput(v);
      lockPropsTrendLineContextToCurrentFilter();
      renderPropsTrends();
    },
    true
  );
  document.getElementById("hh-hole")?.addEventListener("change", () => updateHangout());
  document.getElementById("hh-player")?.addEventListener("change", () => clearHangoutSimulationResults());
  document.getElementById("hh-sim-run")?.addEventListener("click", () => runHangoutSimulate());
  document.getElementById("hh-odds-mode-prob")?.addEventListener("click", () => setHangoutOddsViewMode(false));
  document.getElementById("hh-odds-mode-price")?.addEventListener("click", () => setHangoutOddsViewMode(true));
  document.getElementById("hh-use-live")?.addEventListener("change", () => {
    if (hangoutResultsVisible()) scheduleHangoutLiveRecompute();
    else clearHangoutSimulationResults();
  });
  const hhLiveDebounceIds = ["hh-shot-num", "hh-dist-yds", "hh-lie", "hh-putt-ft"];
  hhLiveDebounceIds.forEach((id) => {
    const el = document.getElementById(id);
    if (!el) return;
    el.addEventListener("input", () => onHangoutLiveFieldChanged());
    el.addEventListener("change", () => onHangoutLiveFieldChanged());
  });
  const trendCanvas = document.getElementById("props-trend-canvas");
  trendCanvas?.addEventListener("mousemove", (ev) => {
    if (!propsChartHitRegions.length) {
      trendCanvas.style.cursor = "";
      hidePropsChartTooltip();
      return;
    }
    const { x, y } = canvasCoordsFromEvent(trendCanvas, ev);
    const hit = pickPropsChartHit(x, y);
    trendCanvas.style.cursor = hit ? "pointer" : "default";
    if (!hit) {
      hidePropsChartTooltip();
      return;
    }
    showPropsChartTooltip(trendCanvas, ev, hit);
  });
  trendCanvas?.addEventListener("mouseleave", () => hidePropsChartTooltip());
  document.getElementById("prop-stat")?.addEventListener("change", () => syncPropsLineStep());
  syncPropsLineStep();
  document.addEventListener("click", () => hidePropsChartTooltip());
  void (async () => {
    await loadProjections();
    startProjectionsPolling();
  })();
});
