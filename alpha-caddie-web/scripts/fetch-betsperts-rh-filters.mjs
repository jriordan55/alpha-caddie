#!/usr/bin/env node
/**
 * Persist Rabbit Hole Filter-by-Conditions catalog + pull All-Players slices
 * for every filter value × all views × Average/Total × (all + last 50 rounds).
 *
 *   node scripts/fetch-betsperts-rh-filters.mjs
 *   node scripts/fetch-betsperts-rh-filters.mjs --catalog-only
 *   node scripts/fetch-betsperts-rh-filters.mjs --limit-slices=20
 *
 * Writes:
 *   data/betsperts_rh_filters.json
 *   data/betsperts/rabbit-hole/filters/catalog.json
 *   data/betsperts/rabbit-hole/filters/by-slice/*.json
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync, renameSync, unlinkSync, readdirSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import {
  buildFilterCatalogPayload,
  expandFilterPullValues,
  filterValueToOverrides,
} from "./betsperts-rh-filters-catalog.mjs";
import {
  RH_MATRIX_VIEWS,
  RH_MATRIX_DISPLAYS,
  rabbitHoleLoadBody,
  resolveBetspertsSessionKey,
  sliceKey,
} from "./betsperts-rh-matrix.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");
const OUT_ROOT = join(ROOT, "data", "betsperts", "rabbit-hole", "filters");
const OUT_SLICES = join(OUT_ROOT, "by-slice");
const OUT_CATALOG_ROOT = join(ROOT, "data", "betsperts_rh_filters.json");
const OUT_CATALOG_WEB = join(WEB, "data", "betsperts_rh_filters.json");
const OUT_CATALOG_RH = join(OUT_ROOT, "catalog.json");
const API = String(process.env.BETSPERTS_API_BASE || "https://api.betspertsgolf.com").replace(/\/$/, "");

function envTruthy(name, defaultVal = false) {
  const raw = process.env[name];
  if (raw === undefined || String(raw).trim() === "") return defaultVal;
  const s = String(raw).trim().toLowerCase();
  return s === "1" || s === "true" || s === "yes";
}

function writeJsonAtomic(filePath, obj, pretty = false) {
  mkdirSync(dirname(filePath), { recursive: true });
  const text = (pretty ? JSON.stringify(obj, null, 2) : JSON.stringify(obj)) + "\n";
  const tmp = `${filePath}.${process.pid}.tmp`;
  writeFileSync(tmp, text, "utf8");
  try {
    renameSync(tmp, filePath);
  } catch {
    writeFileSync(filePath, text, "utf8");
    try {
      unlinkSync(tmp);
    } catch {
      /* ignore */
    }
  }
}

function loadJson(path) {
  if (!existsSync(path)) return null;
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return null;
  }
}

function loadRegionsTypesCourses() {
  const regionsFile = loadJson(join(ROOT, "data", "betsperts", "rabbit-hole", "regions.json"));
  const typesFile = loadJson(join(ROOT, "data", "betsperts", "rabbit-hole", "types.json"));
  const coursesIndex = loadJson(join(ROOT, "data", "betsperts", "courses", "index.json"));
  const regions = Array.isArray(regionsFile?.json?.data)
    ? regionsFile.json.data.map((x) => (typeof x === "string" ? x : x.Region || x.name || x)).filter(Boolean)
    : Array.isArray(regionsFile?.data)
      ? regionsFile.data
      : [];
  const types = Array.isArray(typesFile?.json?.data)
    ? typesFile.json.data.map((x) => (typeof x === "string" ? x : x.Type || x.name || x)).filter(Boolean)
    : Array.isArray(typesFile?.data)
      ? typesFile.data
      : [];
  const courses = Array.isArray(coursesIndex?.data)
    ? coursesIndex.data.map((c) => ({ id: String(c.CourseNum ?? ""), name: String(c.CourseName ?? "") }))
    : [];
  // Fallback to existing filters file
  const prev = loadJson(OUT_CATALOG_ROOT) || loadJson(OUT_CATALOG_WEB);
  return {
    regions: regions.length ? regions : prev?.regions || [],
    types: types.length ? types : prev?.types || [],
    courses: courses.length ? courses : prev?.courses || [],
  };
}

async function apiPost(path, body, sessionKey) {
  const res = await fetch(`${API}${path}`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Accept: "application/json",
      "User-Agent": "Mozilla/5.0 (compatible; AlphaCaddie/1.0; +rh-filters)",
      Origin: "https://betspertsgolf.com",
      Referer: "https://betspertsgolf.com/the-rabbit-hole",
      session_key: sessionKey,
    },
    body: JSON.stringify(body),
  });
  const text = await res.text();
  let json = null;
  try {
    json = JSON.parse(text);
  } catch {
    json = null;
  }
  return { ok: res.ok, status: res.status, json, textLen: text.length };
}

function extractRows(json) {
  if (Array.isArray(json?.data)) return json.data;
  if (Array.isArray(json?.data?.players)) return json.data.players;
  return [];
}

function slug(s) {
  return String(s || "")
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-|-$/g, "");
}

function buildFilterSlicePlan(catalog, opts = {}) {
  const views = opts.views || RH_MATRIX_VIEWS;
  const displays = opts.displays || RH_MATRIX_DISPLAYS;
  const times = opts.times || [
    { timeKey: "all", overrides: { date: "All", season: [], round: "" } },
    { timeKey: "round-50", overrides: { date: "All", season: [], round: "50" } },
  ];
  const filterVals = expandFilterPullValues(catalog);
  const slices = [];
  for (const fv of filterVals) {
    for (const view of views) {
      for (const display of displays) {
        for (const t of times) {
          const timeKey = `${fv.dimKey}__${slug(fv.apiValue)}__${t.timeKey}`;
          slices.push({
            view,
            display,
            timeKey,
            dimKey: fv.dimKey,
            filterValue: fv.value,
            apiField: fv.apiField,
            apiValue: fv.apiValue,
            label: `${view} · ${display} · ${fv.dimLabel}=${fv.value} · ${t.timeKey}`,
            overrides: {
              ...t.overrides,
              view,
              display,
              ...filterValueToOverrides(fv),
            },
          });
        }
      }
    }
  }
  return slices;
}

async function pullFilterMatrix(catalog, opts = {}) {
  const sessionKey = opts.sessionKey || resolveBetspertsSessionKey();
  if (!sessionKey) return { ok: false, error: "no_session" };
  mkdirSync(OUT_SLICES, { recursive: true });
  let slices = buildFilterSlicePlan(catalog, opts);
  if (opts.limitSlices > 0) slices = slices.slice(0, opts.limitSlices);
  const concurrency = Math.max(
    1,
    Math.min(6, Number(opts.concurrency || process.env.BETSPERTS_RH_CONCURRENCY || 3) || 3),
  );
  const results = [];
  let idx = 0;

  async function worker() {
    while (idx < slices.length) {
      const i = idx++;
      const slice = slices[i];
      const key = sliceKey({ view: slice.view, display: slice.display, timeKey: slice.timeKey });
      const body = rabbitHoleLoadBody(slice.overrides);
      let res = await apiPost("/user/rabbit_hole/load", body, sessionKey);
      let rows = extractRows(res.json);
      if (!res.ok || (rows.length === 0 && res.status >= 500)) {
        await new Promise((r) => setTimeout(r, 350));
        res = await apiPost("/user/rabbit_hole/load", body, sessionKey);
        rows = extractRows(res.json);
      }
      const payload = {
        key,
        view: slice.view,
        display: slice.display,
        timeKey: slice.timeKey,
        filter: {
          dimKey: slice.dimKey,
          apiField: slice.apiField,
          value: slice.filterValue,
          apiValue: slice.apiValue,
        },
        label: slice.label,
        pulledAt: new Date().toISOString(),
        status: res.status,
        rowCount: rows.length,
        columns: rows[0] ? Object.keys(rows[0]) : [],
        rows,
      };
      writeJsonAtomic(join(OUT_SLICES, `${key}.json`), payload);
      results.push({
        key,
        status: res.status,
        rowCount: rows.length,
        dimKey: slice.dimKey,
        value: slice.filterValue,
        view: slice.view,
        display: slice.display,
        timeKey: slice.timeKey,
      });
      if ((i + 1) % 15 === 0 || i + 1 === slices.length) {
        console.log(`  rh-filters ${i + 1}/${slices.length} last=${key} n=${rows.length} status=${res.status}`);
      }
    }
  }

  console.log(`[rh-filters] pulling ${slices.length} filter slices concurrency=${concurrency}`);
  await Promise.all(Array.from({ length: concurrency }, () => worker()));
  const manifest = {
    generatedAt: new Date().toISOString(),
    nSlices: results.length,
    nOk: results.filter((r) => r.status === 200 && r.rowCount > 0).length,
    nEmpty: results.filter((r) => r.status === 200 && r.rowCount === 0).length,
    nFail: results.filter((r) => r.status !== 200).length,
    views: [...RH_MATRIX_VIEWS],
    displays: [...RH_MATRIX_DISPLAYS],
    times: ["all", "round-50"],
    slices: results,
  };
  writeJsonAtomic(join(OUT_ROOT, "manifest.json"), manifest, true);
  console.log(
    `[rh-filters] done ok=${manifest.nOk} empty=${manifest.nEmpty} fail=${manifest.nFail} / ${manifest.nSlices}`,
  );
  return { ok: true, ...manifest };
}

async function main() {
  if (envTruthy("GOLF_SKIP_BETSPERTS", false)) {
    console.log("GOLF_SKIP_BETSPERTS=1 — skipping RH filters pull.");
    return;
  }
  const args = process.argv.slice(2);
  const catalogOnly = args.includes("--catalog-only");
  const limitArg = args.find((a) => a.startsWith("--limit-slices="));
  const LIMIT = limitArg ? Math.max(0, Number(limitArg.split("=")[1]) || 0) : 0;

  const { regions, types, courses } = loadRegionsTypesCourses();
  const catalog = buildFilterCatalogPayload({ regions, types, courses });
  // Keep legacy top-level keys used by older readers
  const legacyCompat = {
    ...catalog,
    timeframes: [
      { value: "all", label: "All" },
      { value: "round:12", label: "Last 12 rounds" },
      { value: "round:24", label: "Last 24 rounds" },
      { value: "round:36", label: "Last 36 rounds" },
      { value: "round:50", label: "Last 50 rounds" },
      { value: "round:100", label: "Last 100 rounds" },
      ...Array.from({ length: 12 }, (_, i) => {
        const y = new Date().getUTCFullYear() - 11 + i;
        return { value: `season:${y}`, label: `${y} season` };
      }),
    ],
    regions,
    types,
    courses,
  };
  writeJsonAtomic(OUT_CATALOG_ROOT, legacyCompat, true);
  writeJsonAtomic(OUT_CATALOG_WEB, legacyCompat, true);
  writeJsonAtomic(OUT_CATALOG_RH, legacyCompat, true);
  const nVals = expandFilterPullValues(catalog).length;
  console.log(
    `[rh-filters] catalog written (${catalog.dimensions.length} dimensions, ${nVals} pullable values, ${courses.length} courses)`,
  );

  if (catalogOnly) return;

  if (!resolveBetspertsSessionKey()) {
    console.error("No Betsperts sessionKey — catalog saved; skip data pull.");
    process.exitCode = 1;
    return;
  }
  const res = await pullFilterMatrix(catalog, { limitSlices: LIMIT });
  if (!res.ok) {
    console.error("[rh-filters] pull failed", res.error || res);
    process.exitCode = 1;
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
