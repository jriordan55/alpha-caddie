/**
 * Pull Betsperts Golf player + course database (Rabbit Hole backends).
 *
 * Public guest endpoints on api.betspertsgolf.com — no login required for
 * player profiles/stats and course details. Rabbit Hole query endpoints
 * (/user/rabbit_hole/*, courseMetrics, hole breakdown, …) need a live
 * session_key from a subscribed account (BETSPERTS_SESSION_KEY).
 *
 *   node scripts/fetch-betsperts-db.mjs
 *   node scripts/fetch-betsperts-db.mjs --players-only
 *   node scripts/fetch-betsperts-db.mjs --courses-only
 *   node scripts/fetch-betsperts-db.mjs --limit-players=50
 *   node scripts/fetch-betsperts-db.mjs --force
 *   BETSPERTS_SESSION_KEY=… node scripts/fetch-betsperts-db.mjs --with-auth
 *
 * Session (first match wins):
 *   1) BETSPERTS_SESSION_KEY env
 *   2) alpha-caddie-web/betsperts.local.json → { "sessionKey": "…" }
 *      (gitignored; copy from betsperts.local.example.json)
 *
 * Env:
 *   GOLF_BETSPERTS_FORCE=1 — same as --force (used by nightly push:live)
 *   GOLF_SKIP_BETSPERTS=1 — skip when invoked from refresh:live
 *   GOLF_BETSPERTS_WITH_AUTH=0 — disable gated pulls even if session exists
 *   BETSPERTS_API_BASE / BETSPERTS_CONCURRENCY
 *
 * Writes: ../data/betsperts/
 */
import { mkdirSync, writeFileSync, readFileSync, existsSync, readdirSync, renameSync, unlinkSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { normalizeHoleStatsRows } from "./betsperts-course-context.mjs";
import { pullRhMatrix } from "./betsperts-rh-matrix.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = resolve(__dirname, "..");
const ROOT = resolve(WEB_ROOT, "..");
const OUT = join(ROOT, "data", "betsperts");
const API = String(process.env.BETSPERTS_API_BASE || "https://api.betspertsgolf.com").replace(/\/$/, "");
const CONCURRENCY = Math.max(1, Math.min(16, Number(process.env.BETSPERTS_CONCURRENCY || 8) || 8));
const RETRIES = 3;

function envTruthy(name, defaultVal = false) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function loadSessionKey() {
  const fromEnv = String(process.env.BETSPERTS_SESSION_KEY || "").trim();
  if (fromEnv) return fromEnv;
  const localPath = join(WEB_ROOT, "betsperts.local.json");
  if (!existsSync(localPath)) return "";
  try {
    const j = JSON.parse(readFileSync(localPath, "utf8"));
    return String(j.sessionKey || j.session_key || j.BETSPERTS_SESSION_KEY || "").trim();
  } catch {
    return "";
  }
}

const SESSION = loadSessionKey();
const args = process.argv.slice(2);
const playersOnly = args.includes("--players-only");
const coursesOnly = args.includes("--courses-only");
const authOnly = args.includes("--auth-only");
const force = args.includes("--force") || envTruthy("GOLF_BETSPERTS_FORCE", false);
/** Gated Rabbit Hole / hole-breakdown pulls when a session exists (disable: --no-auth or GOLF_BETSPERTS_WITH_AUTH=0). */
const withAuth =
  Boolean(SESSION) && !args.includes("--no-auth") && envTruthy("GOLF_BETSPERTS_WITH_AUTH", true);
const limitPlayersArg = args.find((a) => a.startsWith("--limit-players="));
const LIMIT_PLAYERS = limitPlayersArg ? Math.max(0, Number(limitPlayersArg.split("=")[1]) || 0) : 0;

/**
 * Default Rabbit Hole load body (All Players).
 * Rolling windows must use `round` (e.g. "50"); year filters use `season: ["2026"]`.
 * `display` is "Average" or "Total".
 */
function rabbitHoleLoadBody(overrides = {}) {
  return {
    playerID: "All Players",
    displayTourId: "All Players",
    tyear: null,
    tourIndex: 0,
    date: "All",
    region: [],
    ctype: [],
    view: "Strokes Gained",
    course: [],
    courseName: [],
    season: [],
    condition: "",
    clength: "",
    fieldStrength: "",
    par: "",
    cut: "",
    tournament: "",
    eventSeason: "",
    fieldSize: "",
    bunkerDanger: "",
    waterDanger: "",
    green: "",
    greenSize: "",
    speed: "",
    fairwaySurface: "",
    roughSurface: "",
    rough: "",
    elevation: "",
    architect: "",
    gainOTT: "",
    OTTClub: "",
    fwyAccuracy: "",
    missedFwyPenalty: "",
    roughPenalty: "",
    gainAPP: "",
    GIRAccuracy: "",
    par3Scoring: "",
    par4Scoring: "",
    par5Scoring: "",
    gainARG: "",
    scramblingRough: "",
    scramblingShortGrass: "",
    sandSavesCond: "",
    gainPutting: "",
    gainingPuttingInside15: "",
    gainingPuttingOutside15: "",
    PuttAVD: "",
    display: "Average",
    TempAVG: "",
    GrassCondition: "",
    PreferredLies: "",
    play: "",
    teeTime: "",
    startDate: "",
    endDate: "",
    round: "",
    wind: "",
    min_round: "",
    ...overrides,
  };
}

const PLAYER_ENDPOINTS = [
  { key: "sgSummary", path: "/user/player/AvgSGsummary", body: (id) => ({ playerID: id, display_by: "year" }) },
  { key: "sgGuest", path: "/user/player/getStrokesGainGuestView", body: (id) => ({ playerID: id }) },
  { key: "sgByEvent", path: "/user/player/AvgSGsummaryEventWise", body: (id) => ({ playerID: id }) },
  { key: "floorCeiling", path: "/user/player/FloorCeilingData", body: (id) => ({ playerID: id }) },
  { key: "greenGrass", path: "/user/player/greenGrassData", body: (id) => ({ playerID: id }) },
  { key: "byRound", path: "/user/player/AvgSGbyRound", body: (id) => ({ playerID: id }) },
  { key: "byCourseLength", path: "/user/player/AvgSGbyCourseLength", body: (id) => ({ playerID: id }) },
  { key: "byScoringCondition", path: "/user/player/AvgSGbyScoringCondition", body: (id) => ({ playerID: id }) },
  { key: "byFieldStrength", path: "/user/player/AvgSGbyFieldStrength", body: (id) => ({ playerID: id }) },
  { key: "amPmSplit", path: "/user/player/AvgSGbyAmPmSplit", body: (id) => ({ playerID: id }) },
  { key: "approachProximity", path: "/user/player/OverallApproachProximityData", body: (id) => ({ playerID: id }) },
  { key: "sgBySeasonHeld", path: "/user/player/SGbyEventSeasonData", body: (id) => ({ playerID: id }) },
];

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

function ensureDirs() {
  for (const p of [
    OUT,
    join(OUT, "players"),
    join(OUT, "players", "by-id"),
    join(OUT, "courses"),
    join(OUT, "courses", "by-id"),
    join(OUT, "courses", "hole-stats"),
    join(OUT, "tournaments"),
    join(OUT, "rabbit-hole"),
  ]) {
    mkdirSync(p, { recursive: true });
  }
}

function sleepMs(ms) {
  const end = Date.now() + ms;
  while (Date.now() < end) {
    /* spin — short lock retries on Windows file locks */
  }
}

function writeJson(path, data) {
  const text = JSON.stringify(data, null, 2) + "\n";
  const tmp = `${path}.${process.pid}.${Date.now()}.tmp`;
  writeFileSync(tmp, text, "utf8");
  let lastErr = null;
  for (let i = 0; i < 6; i++) {
    try {
      renameSync(tmp, path);
      return;
    } catch (e) {
      lastErr = e;
      try {
        writeFileSync(path, text, "utf8");
        try {
          unlinkSync(tmp);
        } catch {
          /* ignore */
        }
        return;
      } catch (e2) {
        lastErr = e2;
        sleepMs(40 * (i + 1));
      }
    }
  }
  try {
    unlinkSync(tmp);
  } catch {
    /* ignore */
  }
  throw lastErr || new Error(`writeJson failed for ${path}`);
}

function headers(extra = {}) {
  const h = {
    "User-Agent": "Mozilla/5.0 (compatible; AlphaCaddie/1.0; +local-db-pull)",
    Accept: "application/json",
    "Content-Type": "application/json",
    Origin: "https://betspertsgolf.com",
    Referer: "https://betspertsgolf.com/the-rabbit-hole",
    ...extra,
  };
  if (SESSION) h.session_key = SESSION;
  return h;
}

async function api(method, path, body = null, { auth = false } = {}) {
  const url = `${API}${path}`;
  let lastErr;
  for (let attempt = 1; attempt <= RETRIES; attempt++) {
    try {
      const opts = {
        method,
        headers: headers(auth && SESSION ? { session_key: SESSION } : {}),
      };
      if (body != null && method !== "GET") opts.body = JSON.stringify(body);
      const res = await fetch(url, opts);
      const text = await res.text();
      let json = null;
      try {
        json = text ? JSON.parse(text) : null;
      } catch {
        json = { _raw: text.slice(0, 500) };
      }
      if (res.status === 429 || res.status >= 500) {
        lastErr = new Error(`HTTP ${res.status} ${path}`);
        await sleep(400 * attempt);
        continue;
      }
      return { ok: res.ok, status: res.status, json, textLen: text.length };
    } catch (e) {
      lastErr = e;
      await sleep(400 * attempt);
    }
  }
  return { ok: false, status: 0, json: { error: String(lastErr?.message || lastErr) }, textLen: 0 };
}

async function mapPool(items, concurrency, fn) {
  const results = new Array(items.length);
  let i = 0;
  async function worker() {
    while (true) {
      const idx = i++;
      if (idx >= items.length) break;
      results[idx] = await fn(items[idx], idx);
    }
  }
  await Promise.all(Array.from({ length: Math.min(concurrency, items.length) }, () => worker()));
  return results;
}

async function pullTournaments() {
  console.log("tournaments…");
  const list = await api("GET", "/user/simulator/get_list_of_tournament");
  const active = await api("GET", "/user/tournament/active");
  const activeSim = await api("GET", "/user/simulator/get_active_tournament");
  const courseActive = await api("GET", "/user/course/active");
  writeJson(join(OUT, "tournaments", "list.json"), list.json);
  writeJson(join(OUT, "tournaments", "active.json"), {
    tournamentActive: active.json,
    simulatorActive: activeSim.json,
    courseActive: courseActive.json,
  });
  return {
    tournaments: Array.isArray(list.json?.data) ? list.json.data.length : 0,
  };
}

async function pullCourses() {
  console.log("courses index…");
  const indexRes = await api("POST", "/user/course/list", {});
  const courses = Array.isArray(indexRes.json?.data) ? indexRes.json.data : [];
  writeJson(join(OUT, "courses", "index.json"), {
    pulledAt: new Date().toISOString(),
    count: courses.length,
    data: courses,
  });

  console.log(`course details × ${courses.length} (concurrency ${CONCURRENCY}; auth=${withAuth ? "yes" : "no"})…`);
  let ok = 0;
  let fail = 0;
  let authOk = 0;
  await mapPool(courses, CONCURRENCY, async (c) => {
    const num = String(c.CourseNum ?? c.courseNum ?? "");
    if (!num) return;
    const outPath = join(OUT, "courses", "by-id", `${num}.json`);
    let payload = null;
    const needBase = force || !existsSync(outPath);
    if (!needBase && existsSync(outPath)) {
      try {
        payload = JSON.parse(readFileSync(outPath, "utf8"));
      } catch {
        payload = null;
      }
    }
    if (needBase || !payload) {
      const [detail, byCourse] = await Promise.all([
        api("POST", "/user/course/get_details", { tcourse: Number(num) || num }),
        api("POST", "/user/tournament/filter_by_course", { tcourse: Number(num) || num }),
      ]);
      payload = {
        courseNum: num,
        courseName: c.CourseName ?? detail.json?.data?.CourseName ?? null,
        list: c,
        details: detail.ok ? detail.json?.data ?? detail.json : null,
        tournaments: byCourse.ok ? byCourse.json?.data ?? byCourse.json : null,
        _meta: {
          pulledAt: new Date().toISOString(),
          detailStatus: detail.status,
          tournamentsStatus: byCourse.status,
        },
      };
      if (detail.ok) ok++;
      else fail++;
    } else {
      ok++;
    }
    /** Course Metrics views matching the Betsperts Course Metrics UI (year × skill category). */
    const METRIC_VIEWS = [
      "Off the Tee",
      "Approach",
      "Around the Green",
      "Putting",
      "Scoring",
      "Strokes Gained",
    ];
    const holeRowsExisting = Array.isArray(payload.auth?.holeBreakdown) ? payload.auth.holeBreakdown : [];
    const holesMissing = holeRowsExisting.length === 0;
    const needAuth =
      withAuth &&
      (force ||
        holesMissing ||
        !payload.auth?.correlated ||
        !payload.auth?.metrics ||
        !payload.auth?.metrics?.["Off the Tee"] ||
        !Array.isArray(payload.holeStats) ||
        payload.holeStats.length < 18);
    if (needAuth) {
      const id = Number(num) || num;
      /** Some list rows share multiple CourseNums — try each until hole stats return. */
      const altNums = [
        id,
        ...((Array.isArray(c.CourseNums) ? c.CourseNums : payload.list?.CourseNums) || [])
          .map((x) => Number(x) || x)
          .filter((x) => x != null && String(x) !== String(id)),
      ];
      let holes = { ok: false, status: 0, json: null };
      let holeTried = [];
      for (const cid of altNums) {
        const res = await api("POST", "/user/hole_stats/get_breakdown", { tcourse: cid }, { auth: true });
        holeTried.push({ courseNum: cid, status: res.status, n: Array.isArray(res.json?.data) ? res.json.data.length : 0 });
        if (res.ok && Array.isArray(res.json?.data) && res.json.data.length) {
          holes = res;
          break;
        }
        holes = res;
      }
      const [correlated, ...metricRes] = await Promise.all([
        api("POST", "/user/course/getCorrelatedCourse", { tcourse: id }, { auth: true }),
        ...METRIC_VIEWS.map((view) =>
          api("POST", "/user/course/courseMetrics", { CourseNums: [id], view }, { auth: true }),
        ),
      ]);
      const metrics = {};
      let metricsOk = 0;
      METRIC_VIEWS.forEach((view, i) => {
        const r = metricRes[i];
        metrics[view] = r?.ok ? r.json?.data ?? r.json : null;
        if (r?.ok && Array.isArray(metrics[view]) && metrics[view].length) metricsOk++;
      });
      const holeBreakdown = holes.ok ? holes.json?.data ?? holes.json : holes.json;
      const holeStats = normalizeHoleStatsRows(Array.isArray(holeBreakdown) ? holeBreakdown : []);
      payload.auth = {
        ...(payload.auth && typeof payload.auth === "object" ? payload.auth : {}),
        holeBreakdown,
        correlated: correlated.ok ? correlated.json?.data ?? correlated.json : correlated.json,
        metrics,
        _meta: {
          pulledAt: new Date().toISOString(),
          holeStatus: holes.status,
          holeTried,
          holeStatsN: holeStats.length,
          correlatedStatus: correlated.status,
          metricsOk,
          metricViews: METRIC_VIEWS,
        },
      };
      payload.holeStats = holeStats;
      payload.holeStatsAvailable = holeStats.length >= 18;
      if (holes.ok || metricsOk > 0) authOk++;
    } else if (!Array.isArray(payload.holeStats) || !payload.holeStats.length) {
      // Backfill normalized Hole Stats schema from existing raw breakdown without a network pull.
      payload.holeStats = normalizeHoleStatsRows(holeRowsExisting);
      payload.holeStatsAvailable = payload.holeStats.length >= 18;
    }
    writeJson(outPath, payload);
    // Durable per-course Hole Stats sidecar (screenshot columns).
    writeJson(join(OUT, "courses", "hole-stats", `${num}.json`), {
      courseNum: num,
      courseName: payload.courseName,
      pulledAt: payload.auth?._meta?.pulledAt || payload._meta?.pulledAt || null,
      available: Boolean(payload.holeStatsAvailable),
      holes: Array.isArray(payload.holeStats) ? payload.holeStats : [],
    });
  });
  console.log(`courses done ok=${ok} fail=${fail} authHoleOk=${authOk}`);
  return { courses: courses.length, courseOk: ok, courseFail: fail, courseAuthOk: authOk };
}

async function pullPlayers() {
  console.log("players index + profiles…");
  const [listRes, profilesRes] = await Promise.all([
    api("POST", "/user/query/getPlayerList", {}),
    api("POST", "/user/player/playerData", {}),
  ]);
  const players = Array.isArray(listRes.json?.data) ? listRes.json.data : [];
  const profiles = Array.isArray(profilesRes.json?.data) ? profilesRes.json.data : [];
  writeJson(join(OUT, "players", "index.json"), {
    pulledAt: new Date().toISOString(),
    count: players.length,
    data: players,
  });
  writeJson(join(OUT, "players", "profiles.json"), {
    pulledAt: new Date().toISOString(),
    count: profiles.length,
    data: profiles,
  });

  const profileByName = new Map();
  for (const p of profiles) {
    if (p?.Name) profileByName.set(String(p.Name).trim().toLowerCase(), p);
  }

  let work = players.map((p) => ({
    id: String(p.PlayerID),
    name: p.PlayerName,
    flag: p.flag,
  }));
  if (LIMIT_PLAYERS > 0) work = work.slice(0, LIMIT_PLAYERS);

  console.log(`player stats × ${work.length} × ${PLAYER_ENDPOINTS.length} endpoints (concurrency ${CONCURRENCY})…`);
  let ok = 0;
  let skip = 0;
  let fail = 0;
  const t0 = Date.now();

  await mapPool(work, CONCURRENCY, async (p, idx) => {
    const outPath = join(OUT, "players", "by-id", `${p.id}.json`);
    if (!force && existsSync(outPath)) {
      skip++;
      ok++;
      return;
    }
    const stats = {};
    let anyOk = false;
    for (const ep of PLAYER_ENDPOINTS) {
      const res = await api("POST", ep.path, ep.body(p.id));
      stats[ep.key] = res.ok ? res.json?.data ?? res.json : { _error: true, status: res.status, body: res.json };
      if (res.ok) anyOk = true;
      else if (res.status === 401 || res.status === 403) {
        // stop hammering if auth wall appears mid-run
        break;
      }
    }
    const payload = {
      playerId: p.id,
      playerName: p.name,
      flag: p.flag,
      profile: profileByName.get(String(p.name || "").trim().toLowerCase()) || null,
      stats,
      _meta: {
        pulledAt: new Date().toISOString(),
        ok: anyOk,
      },
    };
    writeJson(outPath, payload);
    if (anyOk) ok++;
    else fail++;
    if ((idx + 1) % 50 === 0 || idx + 1 === work.length) {
      const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
      const rate = ((idx + 1) / Math.max(0.001, (Date.now() - t0) / 1000)).toFixed(2);
      console.log(`  players ${idx + 1}/${work.length} ok=${ok} skip=${skip} fail=${fail} ${rate}/s ${elapsed}s`);
    }
  });

  console.log(`players done ok=${ok} skip=${skip} fail=${fail}`);
  return { players: work.length, playerOk: ok, playerSkip: skip, playerFail: fail };
}

async function pullRabbitHoleAuth() {
  if (!withAuth) {
    writeJson(join(OUT, "rabbit-hole", "auth-status.json"), {
      pulledAt: new Date().toISOString(),
      sessionProvided: Boolean(SESSION),
      withAuthFlag: withAuth,
      note: "Put sessionKey in alpha-caddie-web/betsperts.local.json (or BETSPERTS_SESSION_KEY) to pull gated Rabbit Hole data.",
    });
    return { rabbitAuth: false };
  }
  console.log("rabbit-hole auth endpoints…");
  const probes = [
    {
      name: "load-last50",
      method: "POST",
      path: "/user/rabbit_hole/load",
      body: rabbitHoleLoadBody({ round: "50", display: "Average" }),
    },
    {
      name: "load-last100",
      method: "POST",
      path: "/user/rabbit_hole/load",
      body: rabbitHoleLoadBody({ round: "100", display: "Average" }),
    },
    {
      name: "load-last50-total",
      method: "POST",
      path: "/user/rabbit_hole/load",
      body: rabbitHoleLoadBody({ round: "50", display: "Total" }),
    },
    {
      name: "load-current-golfers-last50",
      method: "POST",
      path: "/user/rabbit_hole/load",
      body: rabbitHoleLoadBody({
        playerID: "Current Golfers",
        displayTourId: "Current Golfers",
        round: "50",
        display: "Average",
      }),
    },
    { name: "presetSplits", method: "POST", path: "/user/rabbit_hole/preset_splits", body: {} },
    { name: "recentForm", method: "POST", path: "/user/query/recentForm", body: { playerID: "46046" } },
    { name: "regions", method: "POST", path: "/user/course/getRegion", body: {} },
    { name: "types", method: "POST", path: "/user/course/getType", body: {} },
    { name: "expertRankings", method: "GET", path: "/user/custom_report/expertRankings", body: null },
  ];
  const out = {};
  for (const p of probes) {
    const res = await api(p.method, p.path, p.body, { auth: true });
    out[p.name] = res.status;
    const rows = Array.isArray(res.json?.data)
      ? res.json.data.length
      : Array.isArray(res.json?.data?.players)
        ? res.json.data.players.length
        : null;
    writeJson(join(OUT, "rabbit-hole", `${p.name}.json`), {
      pulledAt: new Date().toISOString(),
      status: res.status,
      rowCount: rows,
      json: res.json,
    });
    console.log(`  ${p.name} status=${res.status} rows=${rows ?? "—"} bytes≈${res.textLen}`);
  }

  // Full matrix: all views × Average/Total × all seasons + rolling windows (all players).
  let matrix = null;
  if (!args.includes("--skip-rh-matrix") && !envTruthy("GOLF_SKIP_BETSPERTS_RH_MATRIX", false)) {
    console.log("rabbit-hole full matrix (all views × Average/Total × years + rounds)…");
    matrix = await pullRhMatrix({ sessionKey: SESSION });
    out.rhMatrix = {
      nSlices: matrix?.nSlices ?? 0,
      nOk: matrix?.nOk ?? 0,
      nEmpty: matrix?.nEmpty ?? 0,
      nFail: matrix?.nFail ?? 0,
    };
  } else {
    console.log("  skipping RH matrix (GOLF_SKIP_BETSPERTS_RH_MATRIX / --skip-rh-matrix)");
  }

  writeJson(join(OUT, "rabbit-hole", "auth-status.json"), {
    pulledAt: new Date().toISOString(),
    sessionProvided: true,
    results: out,
    matrix: out.rhMatrix || null,
  });
  return { rabbitAuth: true, rabbitEndpoints: out, rhMatrix: matrix };
}

async function main() {
  if (envTruthy("GOLF_SKIP_BETSPERTS", false)) {
    console.log("GOLF_SKIP_BETSPERTS=1 — skipping Betsperts pull.");
    return;
  }
  ensureDirs();
  const started = new Date().toISOString();
  console.log(`Betsperts pull → ${OUT}`);
  console.log(
    `API ${API} concurrency=${CONCURRENCY} force=${force ? "yes" : "no"} session=${SESSION ? "yes" : "no"} withAuth=${withAuth ? "yes" : "no"}`,
  );

  const summary = { started, api: API, session: Boolean(SESSION), withAuth };

  if (authOnly) {
    Object.assign(summary, await pullCourses());
    Object.assign(summary, await pullRabbitHoleAuth());
  } else {
    if (!playersOnly) {
      Object.assign(summary, await pullTournaments());
      Object.assign(summary, await pullCourses());
    }
    if (!coursesOnly) {
      Object.assign(summary, await pullPlayers());
    }
    Object.assign(summary, await pullRabbitHoleAuth());
  }

  summary.finished = new Date().toISOString();
  summary.playerFiles = existsSync(join(OUT, "players", "by-id"))
    ? readdirSync(join(OUT, "players", "by-id")).filter((f) => f.endsWith(".json")).length
    : 0;
  summary.courseFiles = existsSync(join(OUT, "courses", "by-id"))
    ? readdirSync(join(OUT, "courses", "by-id")).filter((f) => f.endsWith(".json")).length
    : 0;
  writeJson(join(OUT, "manifest.json"), summary);
  console.log("manifest", JSON.stringify(summary, null, 2));
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
