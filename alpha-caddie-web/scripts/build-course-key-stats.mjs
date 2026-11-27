/**
 * Precompute Course Key Stats (finish correlations + field fit) for the active venue.
 *
 * Reads course-history shards, Rabbit Hole betting viz, course facts, approach skill,
 * and projections field — writes data/course_key_stats.json for instant UI paint.
 *
 *   node scripts/build-course-key-stats.mjs
 *   node scripts/build-course-key-stats.mjs --all-courses
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { normCourseNameKey } from "./course-name-key.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const ROOT = join(WEB, "..");
const LOOKBACK_YEARS = 3;
const APPROACH_BINS = [
  { sgKey: "50_100_fw_sg_per_shot", label: "Proximity 50–100" },
  { sgKey: "100_150_fw_sg_per_shot", label: "Proximity 100–150", boost: 0.01 },
  { sgKey: "150_200_fw_sg_per_shot", label: "Proximity 150–200", boost: 0.01 },
  { sgKey: "over_200_fw_sg_per_shot", label: "Proximity 200+" },
  { sgKey: "under_150_rgh_sg_per_shot", label: "Proximity Rough <150" },
  { sgKey: "over_150_rgh_sg_per_shot", label: "Proximity Rough 150+" },
];

const ALL_COURSES = process.argv.includes("--all-courses");

function num(v, d = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : d;
}

function readJson(path) {
  if (!existsSync(path)) return null;
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return null;
  }
}

function pearson(xs, ys) {
  const n = xs.length;
  if (n < 8) return NaN;
  let sx = 0;
  let sy = 0;
  let sxx = 0;
  let syy = 0;
  let sxy = 0;
  for (let i = 0; i < n; i++) {
    sx += xs[i];
    sy += ys[i];
    sxx += xs[i] * xs[i];
    syy += ys[i] * ys[i];
    sxy += xs[i] * ys[i];
  }
  const nume = n * sxy - sx * sy;
  const den = Math.sqrt((n * sxx - sx * sx) * (n * syy - sy * sy));
  return den > 0 ? nume / den : NaN;
}

function meanSd(vals) {
  const xs = vals.filter((v) => Number.isFinite(v));
  if (!xs.length) return { mean: NaN, sd: NaN };
  const mean = xs.reduce((a, b) => a + b, 0) / xs.length;
  const sd = Math.sqrt(xs.reduce((a, b) => a + (b - mean) ** 2, 0) / Math.max(1, xs.length)) || 1;
  return { mean, sd };
}

function parseFinishRank(finText) {
  const s = String(finText || "").trim().toUpperCase();
  if (!s || s === "CUT" || s === "WD" || s === "DQ" || s === "MDF") return NaN;
  const m = s.match(/T?(\d+)/);
  return m ? Math.round(num(m[1], NaN)) : NaN;
}

function yardsToLengthLabel(yards) {
  const y = num(yards, NaN);
  if (!Number.isFinite(y) || y <= 0) return null;
  if (y < 6800) return "Very Short";
  if (y < 7000) return "Short";
  if (y < 7200) return "Average";
  if (y < 7400) return "Long";
  return "Very Long";
}

function shardFileName(courseKey) {
  const safe = String(courseKey || "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 96);
  return `${safe || "unknown"}.json`;
}

function displayName(raw) {
  const s = String(raw || "").trim();
  if (!s) return "";
  if (s.includes(",")) {
    const [last, first] = s.split(",").map((x) => x.trim());
    if (first && last) return `${first} ${last}`;
  }
  return s;
}

function courseKeysForVenue(venueRaw, manifestCourses) {
  const raw = String(venueRaw || "").trim();
  const keys = new Set();
  if (!raw) return [];
  const full = normCourseNameKey(raw);
  if (full) keys.add(full);
  const parts = raw.includes(" · ") || raw.includes(";") ? raw.split(/\s*[·;]\s*/) : [raw];
  for (const part of parts) {
    const k = normCourseNameKey(part);
    if (!k) continue;
    keys.add(k);
    const tokens = k.split(/\s+/).filter(Boolean);
    for (let i = tokens.length - 1; i >= 2; i--) keys.add(tokens.slice(0, i).join(" "));
  }
  for (const c of manifestCourses || []) {
    const ck = normCourseNameKey(c.course_key || "");
    if (!ck) continue;
    if (full.includes(ck) || [...keys].some((k) => k.includes(ck) || ck.includes(k))) keys.add(ck);
  }
  return [...keys];
}

function loadEntriesForKeys(keys, manifestCourses) {
  const dir = join(WEB, "player-history", "by-course");
  const files = new Set();
  for (const k of keys) {
    files.add(shardFileName(k));
    for (const c of manifestCourses || []) {
      const ck = normCourseNameKey(c.course_key || "");
      if (ck === k && c.file) files.add(String(c.file));
    }
  }
  const seen = new Set();
  const entries = [];
  for (const file of files) {
    const j = readJson(join(dir, file));
    if (!j?.entries?.length) continue;
    for (const e of j.entries) {
      const row = e.row && typeof e.row === "object" ? e.row : e;
      const dgId = Math.round(num(e.dg_id ?? row?.dg_id, NaN));
      if (!Number.isFinite(dgId)) continue;
      const sk = Math.round(num(row?.sortKey, NaN));
      const dk = Number.isFinite(sk) && sk > 0
        ? `${dgId}|${sk}`
        : `${dgId}|${row?.year}|${row?.round_num}|${String(row?.event_name || "").toLowerCase()}`;
      if (seen.has(dk)) continue;
      seen.add(dk);
      entries.push({ dgId, row });
    }
  }
  return entries;
}

function venueContext(factsByKey, venueRaw) {
  const parts = String(venueRaw || "").includes(" · ") || String(venueRaw || "").includes(";")
    ? String(venueRaw).split(/\s*[·;]\s*/)
    : [venueRaw];
  const facts = [];
  const seen = new Set();
  for (const part of parts) {
    const k = normCourseNameKey(part);
    const f = factsByKey?.[k];
    if (f?.key && !seen.has(f.key)) {
      seen.add(f.key);
      facts.push(f);
    }
  }
  const f = facts[0] || null;
  const yardsList = facts.map((x) => num(x?.yards, NaN)).filter((y) => y > 0);
  const yards = yardsList.length ? yardsList.reduce((a, b) => a + b, 0) / yardsList.length : num(f?.yards, NaN);
  const lengthLabel = yardsToLengthLabel(yards);
  const greens = f?.grass?.greens?.[0] || null;
  const difficulty = f?.difficulty || null;
  return {
    facts,
    greens,
    fairways: f?.grass?.fairways?.[0] || null,
    difficulty,
    lengthLabel,
    preferredRhKeys: new Set(
      [
        greens ? `green:${String(greens).toLowerCase()}` : "",
        difficulty ? `condition:${String(difficulty).toLowerCase()}` : "",
        lengthLabel ? `length:${String(lengthLabel).toLowerCase()}` : "",
      ].filter(Boolean),
    ),
  };
}

function rhSplit(vizByDg, dgId, kind, label) {
  const viz = vizByDg?.[String(dgId)];
  if (!viz || !label) return NaN;
  if (kind === "green") {
    const hit = (viz.greenGrass || []).find((r) => String(r.surface || "").toLowerCase() === String(label).toLowerCase());
    return num(hit?.sg, NaN);
  }
  if (kind === "length") {
    const hit = (viz.courseLength || []).find((r) => String(r.label || "").toLowerCase() === String(label).toLowerCase());
    return num(hit?.sg, NaN);
  }
  if (kind === "condition") {
    const hit = (viz.scoringCondition || []).find((r) => String(r.label || "").toLowerCase() === String(label).toLowerCase());
    return num(hit?.sg, NaN);
  }
  return NaN;
}

function playerFormMetric(dgId, key, ctx, deps) {
  const id = Math.round(num(dgId, NaN));
  const row = deps.playersByDg.get(id);
  const rh = deps.rhByDg.get(id);
  const sg = deps.sgByDg.get(id);
  const win = row?.betsperts_form_windows?.average || deps.vizByDg?.[String(id)]?.windows?.average;
  const l50 = win?.["round-50"] || win?.["round-24"] || win?.["round-12"];
  if (key === "sg_app") return num(rh?.sg_app ?? sg?.sg_app ?? l50?.sg_app ?? row?.sg_app, NaN);
  if (key === "sg_ott") return num(rh?.sg_ott ?? sg?.sg_ott ?? l50?.sg_ott ?? row?.sg_ott, NaN);
  if (key === "sg_arg") return num(rh?.sg_arg ?? sg?.sg_arg ?? l50?.sg_arg ?? row?.sg_arg, NaN);
  if (key === "sg_putt") {
    const greenSplit = ctx?.greens ? rhSplit(deps.vizByDg, id, "green", ctx.greens) : NaN;
    if (Number.isFinite(greenSplit)) return greenSplit;
    return num(rh?.sg_putt ?? sg?.sg_putt ?? l50?.sg_putt ?? row?.sg_putt, NaN);
  }
  if (key === "sg_t2g") return num(rh?.sg_t2g ?? sg?.sg_t2g ?? row?.sg_t2g, NaN);
  if (key === "sg_total") return num(rh?.sg_total ?? sg?.sg_total ?? l50?.sg_tot ?? row?.sg_total, NaN);
  if (key === "driving_dist") {
    return num(row?.driving_distance ?? row?.avg_driving_distance ?? row?.adj_driving_distance, NaN);
  }
  if (key === "driving_acc") return num(rh?.dr_accuracy ?? row?.driving_accuracy, NaN);
  if (key === "gir") return num(rh?.gir_pct ?? row?.gir, NaN);
  if (key.startsWith("appbin:")) return num(deps.approachByDg.get(id)?.[key.slice(7)], NaN);
  if (key.startsWith("rh_green:")) return rhSplit(deps.vizByDg, id, "green", key.slice(9));
  if (key.startsWith("rh_length:")) return rhSplit(deps.vizByDg, id, "length", key.slice(10));
  if (key.startsWith("rh_condition:")) return rhSplit(deps.vizByDg, id, "condition", key.slice(13));
  return NaN;
}

function computeKeyStats(entries, ctx, deps, minYear) {
  const eventMap = new Map();
  let nRounds = 0;
  for (const e of entries || []) {
    const r = e.row || e;
    const dgId = Math.round(num(e.dgId ?? e.dg_id ?? r.dg_id, NaN));
    if (!Number.isFinite(dgId)) continue;
    const y = Math.round(num(r.year, NaN));
    if (Number.isFinite(y) && y < minYear) continue;
    nRounds += 1;
    const finish = parseFinishRank(r.fin_text);
    const evKey = `${dgId}|${y}|${String(r.event_id || r.event_name || "").trim().toLowerCase()}`;
    let ev = eventMap.get(evKey);
    if (!ev) {
      ev = {
        dgId,
        finish: NaN,
        n: 0,
        sg_app: 0,
        sg_ott: 0,
        sg_arg: 0,
        sg_putt: 0,
        sg_t2g: 0,
        sg_total: 0,
        gir: 0,
        fairways: 0,
      };
      eventMap.set(evKey, ev);
    }
    if (Number.isFinite(finish)) ev.finish = finish;
    const bump = (key, v) => {
      if (Number.isFinite(v)) ev[key] += v;
    };
    ev.n += 1;
    bump("sg_app", num(r.sg_app, NaN));
    bump("sg_ott", num(r.sg_ott, NaN));
    bump("sg_arg", num(r.sg_arg, NaN));
    bump("sg_putt", num(r.sg_putt, NaN));
    bump("sg_t2g", num(r.sg_t2g, NaN));
    bump("sg_total", num(r.sg_total, NaN));
    bump("gir", num(r.gir, NaN));
    bump("fairways", num(r.fairways, NaN));
  }

  const events = [];
  for (const ev of eventMap.values()) {
    if (ev.n < 1) continue;
    const avg = (sum) => sum / ev.n;
    const hasFinish = Number.isFinite(ev.finish);
    const perf = hasFinish ? -ev.finish : avg(ev.sg_total);
    if (!Number.isFinite(perf)) continue;
    events.push({
      dgId: ev.dgId,
      perf,
      hasFinish,
      sg_app: avg(ev.sg_app),
      sg_ott: avg(ev.sg_ott),
      sg_arg: avg(ev.sg_arg),
      sg_putt: avg(ev.sg_putt),
      sg_t2g: avg(ev.sg_t2g),
      gir: avg(ev.gir),
      fairways: avg(ev.fairways),
    });
  }

  const finishEvents = events.filter((e) => e.hasFinish);
  const sample = finishEvents.length >= 12 ? finishEvents : events;
  const out = [];
  const puttLabel = ctx.greens ? `SG: Putting (${ctx.greens})` : "SG: Putting";
  const eventCandidates = [
    { key: "sg_app", label: "SG: Approach" },
    { key: "sg_ott", label: "SG: Off-the-Tee" },
    { key: "sg_arg", label: "SG: Around-the-Green" },
    { key: "sg_putt", label: puttLabel },
    { key: "sg_t2g", label: "SG: Tee-to-Green" },
    { key: "gir", label: "GIR" },
    { key: "fairways", label: "Fairways Hit" },
  ];
  for (const c of eventCandidates) {
    const xs = [];
    const ys = [];
    for (const ev of sample) {
      const x = ev[c.key];
      if (!Number.isFinite(x) || !Number.isFinite(ev.perf)) continue;
      xs.push(x);
      ys.push(ev.perf);
    }
    const r = pearson(xs, ys);
    if (!Number.isFinite(r) || r <= 0) continue;
    out.push({ key: c.key, label: c.label, r, n: xs.length, source: "course-history", venueBoost: 0 });
  }

  const byPlayer = new Map();
  for (const ev of sample) {
    const prev = byPlayer.get(ev.dgId) || { n: 0, sum: 0 };
    prev.n += 1;
    prev.sum += ev.perf;
    byPlayer.set(ev.dgId, prev);
  }

  const playerCandidates = [];
  const greenSeen = new Set();
  const lengthSeen = new Set();
  const conditionSeen = new Set();
  for (const dgId of byPlayer.keys()) {
    const viz = deps.vizByDg?.[String(dgId)];
    if (!viz) continue;
    for (const g of viz.greenGrass || []) {
      const lab = String(g.surface || "").trim();
      if (!lab || greenSeen.has(lab.toLowerCase())) continue;
      greenSeen.add(lab.toLowerCase());
      playerCandidates.push({
        key: `rh_green:${lab}`,
        label: `SG: Putting (${lab})`,
        venueBoost: ctx.preferredRhKeys?.has(`green:${lab.toLowerCase()}`) ? 0.015 : 0,
      });
    }
    for (const g of viz.courseLength || []) {
      const lab = String(g.label || "").trim();
      if (!lab || lengthSeen.has(lab.toLowerCase())) continue;
      lengthSeen.add(lab.toLowerCase());
      playerCandidates.push({
        key: `rh_length:${lab}`,
        label: `Course Length (${lab})`,
        venueBoost: ctx.preferredRhKeys?.has(`length:${lab.toLowerCase()}`) ? 0.015 : 0,
      });
    }
    for (const g of viz.scoringCondition || []) {
      const lab = String(g.label || "").trim();
      if (!lab || conditionSeen.has(lab.toLowerCase())) continue;
      conditionSeen.add(lab.toLowerCase());
      playerCandidates.push({
        key: `rh_condition:${lab}`,
        label: `Scoring (${lab})`,
        venueBoost: ctx.preferredRhKeys?.has(`condition:${lab.toLowerCase()}`) ? 0.015 : 0,
      });
    }
  }
  for (const def of APPROACH_BINS) {
    playerCandidates.push({
      key: `appbin:${def.sgKey}`,
      label: def.label,
      venueBoost: def.boost || 0,
    });
  }
  playerCandidates.push(
    { key: "driving_dist", label: "Driving Distance", venueBoost: 0 },
    { key: "driving_acc", label: "Driving Accuracy", venueBoost: 0 },
  );

  for (const c of playerCandidates) {
    const xs = [];
    const ys = [];
    for (const [dgId, agg] of byPlayer) {
      const x = playerFormMetric(dgId, c.key, ctx, deps);
      if (!Number.isFinite(x) || agg.n < 1) continue;
      xs.push(x);
      ys.push(agg.sum / agg.n);
    }
    const rRaw = pearson(xs, ys);
    if (!Number.isFinite(rRaw) || rRaw <= 0) continue;
    out.push({
      key: c.key,
      label: c.label,
      r: Math.min(0.99, rRaw + (c.venueBoost || 0)),
      n: xs.length,
      source: c.key.startsWith("rh_") ? "rabbit-hole" : c.key.startsWith("appbin:") ? "approach-skill" : "player-form",
      venueBoost: c.venueBoost || 0,
    });
  }

  out.sort((a, b) => b.r - a.r || (b.venueBoost || 0) - (a.venueBoost || 0));
  const seen = new Set();
  const top = [];
  for (const row of out) {
    const lk = row.label.toLowerCase();
    if (seen.has(lk)) continue;
    seen.add(lk);
    top.push({
      key: row.key,
      label: row.label,
      r: Math.round(row.r * 1000) / 1000,
      n: row.n,
      source: row.source,
    });
    if (top.length >= 5) break;
  }
  return { metrics: top, nRounds, nPlayers: byPlayer.size };
}

function percentileRanks(vals) {
  const indexed = vals.map((v, i) => ({ v, i })).filter((x) => Number.isFinite(x.v));
  indexed.sort((a, b) => a.v - b.v);
  const out = vals.map(() => null);
  const n = indexed.length;
  for (let r = 0; r < n; r++) {
    out[indexed[r].i] = Math.round((r / Math.max(1, n - 1)) * 99);
  }
  return out;
}

function scoreFieldFits(metrics, ctx, deps, fieldPlayers) {
  if (!metrics.length || !fieldPlayers.length) return [];
  const metricVals = metrics.map((m) => fieldPlayers.map((f) => playerFormMetric(f.dgId, m.key, ctx, deps)));
  const metricPcts = metricVals.map((col) => percentileRanks(col));
  const metricZs = metricVals.map((col) => {
    const { mean, sd } = meanSd(col);
    return col.map((v) => (Number.isFinite(v) && Number.isFinite(mean) ? (v - mean) / (sd || 1) : 0));
  });
  const wSum = metrics.reduce((a, m) => a + Math.max(0, m.r), 0) || 1;
  const scored = fieldPlayers.map((f, i) => {
    let score = 0;
    const pctByKey = {};
    metrics.forEach((m, mi) => {
      const z = metricZs[mi][i];
      score += (Math.max(0, m.r) / wSum) * z;
      const pct = metricPcts[mi][i];
      pctByKey[m.key] = Number.isFinite(pct) ? pct : null;
    });
    return { dgId: f.dgId, name: f.name, raw: score, pctByKey };
  });
  const sorted = [...scored].sort((a, b) => a.raw - b.raw);
  const n = sorted.length;
  for (let i = 0; i < n; i++) sorted[i].fit = Math.round((i / Math.max(1, n - 1)) * 99);
  scored.sort((a, b) => b.fit - a.fit || b.raw - a.raw);
  return scored.map(({ dgId, name, fit, pctByKey }) => ({ dgId, name, fit, pctByKey }));
}

function loadProjections() {
  return (
    readJson(join(WEB, "projections-pga.json")) ||
    readJson(join(WEB, "projections.json")) ||
    readJson(join(ROOT, "website", "public", "data", "projections.json")) ||
    {}
  );
}

function main() {
  const t0 = Date.now();
  const minYear = new Date().getFullYear() - (LOOKBACK_YEARS - 1);
  const proj = loadProjections();
  const venueRaw = String(proj.course_used || proj.meta?.course_used || "").trim();
  const eventName = String(proj.event_name || proj.meta?.event_name || "").trim();

  const manifest = readJson(join(WEB, "player-history", "courses-manifest.json")) || { courses: [] };
  const facts = readJson(join(WEB, "data", "betsperts_course_facts.json")) || {};
  const viz = readJson(join(WEB, "data", "betsperts_betting_viz.json")) || {};
  const rhOu = readJson(join(WEB, "data", "betsperts_rh_ou_stats.json")) || {};
  const sg = readJson(join(WEB, "data", "betsperts_sg_by_dg.json")) || {};
  const approach =
    readJson(join(WEB, "approach_skill_ytd.json")) ||
    readJson(join(WEB, "approach_skill_l12.json")) ||
    { players: [] };

  const playersByDg = new Map();
  for (const p of proj.players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(id)) continue;
    const round = Math.round(num(p.round, NaN));
    const prev = playersByDg.get(id);
    if (!prev || round === 1) playersByDg.set(id, p);
  }
  const rhByDg = new Map();
  for (const [k, v] of Object.entries(rhOu.players || {})) {
    const id = Math.round(num(v?.dg_id ?? k, NaN));
    if (Number.isFinite(id)) rhByDg.set(id, v);
  }
  const sgByDg = new Map();
  for (const [k, v] of Object.entries(sg.players || {})) {
    const id = Math.round(num(v?.dg_id ?? k, NaN));
    if (Number.isFinite(id)) sgByDg.set(id, v);
  }
  const approachByDg = new Map();
  for (const p of approach.players || []) {
    const id = Math.round(num(p.dg_id, NaN));
    if (Number.isFinite(id)) approachByDg.set(id, p);
  }

  const deps = {
    playersByDg,
    rhByDg,
    sgByDg,
    approachByDg,
    vizByDg: viz.playersByDg || {},
  };

  const fieldPlayers = [];
  const seenField = new Set();
  for (const p of proj.players || []) {
    const dgId = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dgId) || seenField.has(dgId)) continue;
    const hasR1 = (proj.players || []).some(
      (x) => Math.round(num(x.dg_id, NaN)) === dgId && Math.round(num(x.round, NaN)) === 1,
    );
    if (hasR1 && Math.round(num(p.round, NaN)) !== 1) continue;
    seenField.add(dgId);
    fieldPlayers.push({
      dgId,
      name: displayName(p.player_name) || String(p.player_name || ""),
    });
  }

  const venuesToBuild = [];
  if (venueRaw) venuesToBuild.push({ venueRaw, primary: true });
  if (ALL_COURSES) {
    for (const c of manifest.courses || []) {
      const label = String(c.course_key || "").trim();
      if (!label) continue;
      if (venuesToBuild.some((v) => normCourseNameKey(v.venueRaw) === normCourseNameKey(label))) continue;
      venuesToBuild.push({ venueRaw: label, primary: false });
    }
  }

  const byCourseKey = {};
  let current = null;

  for (const item of venuesToBuild) {
    const keys = courseKeysForVenue(item.venueRaw, manifest.courses);
    const entries = loadEntriesForKeys(keys, manifest.courses);
    const ctx = venueContext(facts.byKey || {}, item.venueRaw);
    const { metrics, nRounds, nPlayers } = computeKeyStats(entries, ctx, deps, minYear);
    const primaryKey = keys.find((k) => byCourseKey[k] == null) || normCourseNameKey(item.venueRaw) || keys[0];
    const block = {
      venue: item.venueRaw,
      courseKeys: keys,
      greens: ctx.greens,
      difficulty: ctx.difficulty,
      lengthLabel: ctx.lengthLabel,
      metrics,
      nRounds,
      nPlayers,
    };
    for (const k of keys) {
      if (!byCourseKey[k] || (metrics.length && (byCourseKey[k].metrics?.length || 0) < metrics.length)) {
        byCourseKey[k] = block;
      }
    }
    if (item.primary) {
      const fits = scoreFieldFits(metrics, ctx, deps, fieldPlayers);
      current = {
        ...block,
        eventName,
        venueLabel: item.venueRaw,
        fits: fits.slice(0, 80),
        bestFit: fits[0] || null,
      };
    }
  }

  const payload = {
    generatedAt: new Date().toISOString(),
    lookbackYears: LOOKBACK_YEARS,
    minSeasonYear: minYear,
    eventName,
    venue: venueRaw,
    current,
    byCourseKey,
  };

  const outWeb = join(WEB, "data", "course_key_stats.json");
  const outRoot = join(ROOT, "data", "course_key_stats.json");
  mkdirSync(dirname(outWeb), { recursive: true });
  const json = `${JSON.stringify(payload)}\n`;
  writeFileSync(outWeb, json, "utf8");
  try {
    mkdirSync(dirname(outRoot), { recursive: true });
    writeFileSync(outRoot, json, "utf8");
  } catch {
    /* optional root mirror */
  }

  const m = current?.metrics || [];
  console.log(
    `[course-key-stats] venue="${venueRaw || "—"}" metrics=${m.length} fits=${current?.fits?.length || 0} courses=${Object.keys(byCourseKey).length} in ${Date.now() - t0}ms → data/course_key_stats.json`,
  );
  if (m.length) console.log(`[course-key-stats] top: ${m.map((x) => `${x.label} ${x.r}`).join(" · ")}`);
  if (current?.bestFit) console.log(`[course-key-stats] best fit: ${current.bestFit.name} (${current.bestFit.fit})`);
}

main();
