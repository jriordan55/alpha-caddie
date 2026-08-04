/**
 * Supplement data/pga_datagolf_player_map.csv for PGA player_ids that appear in
 * round SG-by-distance CSVs (or shots) but lack a dg_id mapping.
 *
 * Resolves names via pgatour.com player pages, then matches DataGolf player-list
 * with accent-insensitive "Last, First" keys.
 *
 * Usage: node scripts/supplement-pga-dg-map.mjs
 */
import fs from "fs";
import path from "path";
import { createReadStream, createWriteStream } from "fs";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";
import { finished } from "stream/promises";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");
const MAP = path.join(REPO, "data", "pga_datagolf_player_map.csv");
const PUTT = path.join(WEB, "data", "round_sg_putt_by_distance.csv");
const APP = path.join(WEB, "data", "round_sg_by_distance.csv");
const DG_LOCAL = path.join(WEB, "datagolf.local.json");

function normPgaId(pid) {
  const s = String(pid ?? "").trim();
  if (!s) return "";
  if (/^\d+$/.test(s) && s.length < 5) return s.padStart(5, "0");
  return s;
}

function foldName(s) {
  return String(s || "")
    .replace(/ß/g, "ss")
    .replace(/[æÆ]/g, "ae")
    .replace(/[øØ]/g, "o")
    .replace(/[åÅ]/g, "a")
    .replace(/[ðÐ]/g, "d")
    .replace(/[þÞ]/g, "th")
    .normalize("NFD")
    .replace(/\p{M}/gu, "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .replace(/\s+/g, " ");
}

function nameKey(s) {
  return foldName(s).split(" ").filter(Boolean).sort().join(" ");
}

function displayToLastFirst(ogTitle) {
  const raw = String(ogTitle || "")
    .replace(/\s*PGA TOUR Player Profile.*$/i, "")
    .replace(/\s*PGA TOUR.*$/i, "")
    .trim();
  if (!raw) return "";
  const parts = raw.split(/\s+/).filter(Boolean);
  if (parts.length < 2) return raw;
  const last = parts[parts.length - 1];
  const first = parts.slice(0, -1).join(" ");
  return `${last}, ${first}`;
}

async function loadCsv(file) {
  const rows = [];
  if (!fs.existsSync(file)) return rows;
  await new Promise((res, rej) => {
    createReadStream(file)
      .pipe(parse({ columns: true, relax_column_count: true, skip_records_with_error: true }))
      .on("data", (r) => rows.push(r))
      .on("end", res)
      .on("error", rej);
  });
  return rows;
}

function dgApiKey() {
  try {
    const j = JSON.parse(fs.readFileSync(DG_LOCAL, "utf8"));
    return String(j.apiKey || j.key || "").trim();
  } catch {
    return String(process.env.DATAGOLF_API_KEY || "").trim();
  }
}

async function loadDgPlayers(key) {
  const url =
    "https://feeds.datagolf.com/get-player-list?file_format=json&key=" + encodeURIComponent(key);
  const res = await fetch(url);
  if (!res.ok) throw new Error(`DG player-list HTTP ${res.status}`);
  const arr = await res.json();
  /** @type {Map<string, {dg_id:number, player_name:string, country:string}>} */
  const byNorm = new Map();
  /** @type {Map<string, {dg_id:number, player_name:string, country:string}>} */
  const byKey = new Map();
  for (const p of arr || []) {
    const dg = Math.round(Number(p.dg_id));
    const name = String(p.player_name || "").trim();
    if (!Number.isFinite(dg) || !name) continue;
    const rec = { dg_id: dg, player_name: name, country: String(p.country || "") };
    const n = foldName(name);
    const k = nameKey(name);
    if (!byNorm.has(n)) byNorm.set(n, rec);
    if (!byKey.has(k)) byKey.set(k, rec);
  }
  return { byNorm, byKey, n: arr?.length || 0 };
}

async function fetchPgaDisplayName(pgaId) {
  const id = String(pgaId).replace(/^0+/, "") || "0";
  const url = `https://www.pgatour.com/player/${id}`;
  const res = await fetch(url, {
    headers: { "user-agent": "Mozilla/5.0", accept: "text/html" },
    redirect: "follow",
  });
  if (!res.ok) return null;
  const t = await res.text();
  const og = (t.match(/property="og:title" content="([^"]+)"/i) || [])[1];
  return og || null;
}

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

const mapRows = await loadCsv(MAP);
const mappedPids = new Set();
const mappedDgs = new Set();
for (const r of mapRows) {
  const pid = normPgaId(r.pga_player_id);
  if (pid) mappedPids.add(pid);
  const dg = Math.round(Number(r.dg_id));
  if (Number.isFinite(dg)) mappedDgs.add(dg);
}

const needCounts = new Map();
for (const file of [PUTT, APP]) {
  for (const r of await loadCsv(file)) {
    const pid = normPgaId(r.pga_player_id);
    if (!pid) continue;
    const hasDg = r.dg_id && String(r.dg_id).trim() && Number.isFinite(Number(r.dg_id));
    if (hasDg || mappedPids.has(pid)) continue;
    needCounts.set(pid, (needCounts.get(pid) || 0) + 1);
  }
}

const need = [...needCounts.entries()].sort((a, b) => b[1] - a[1]);
console.log(`[supplement-map] Unmapped PGA ids in SG CSVs: ${need.length}`);
if (!need.length) {
  console.log("Nothing to do.");
  process.exit(0);
}

const key = dgApiKey();
if (!key) {
  console.error("Missing DataGolf API key (datagolf.local.json or DATAGOLF_API_KEY)");
  process.exit(1);
}
const dg = await loadDgPlayers(key);
console.log(`[supplement-map] DG players: ${dg.n}`);

const added = [];
let failed = 0;
for (let i = 0; i < need.length; i++) {
  const [pid, nRows] = need[i];
  if (i && i % 25 === 0) console.log(`  … ${i}/${need.length} (added ${added.length})`);
  let og;
  try {
    og = await fetchPgaDisplayName(pid);
  } catch {
    og = null;
  }
  await sleep(80);
  if (!og) {
    failed++;
    continue;
  }
  const lastFirst = displayToLastFirst(og);
  const n = foldName(lastFirst);
  const k = nameKey(lastFirst);
  let hit = dg.byNorm.get(n) || dg.byKey.get(k);
  if (!hit) {
    failed++;
    continue;
  }
  if (mappedDgs.has(hit.dg_id) || mappedPids.has(pid)) {
    // Allow same dg only if not already mapped from another pga id — skip duplicate dg
    if (mappedPids.has(pid)) continue;
    // If dg already mapped to another pga, still add if this pid is new (1:1 preferred)
    // Prefer not to create duplicate dg_ids in map — skip
    const existing = mapRows.find((r) => Math.round(Number(r.dg_id)) === hit.dg_id);
    if (existing) {
      failed++;
      continue;
    }
  }
  const parts = lastFirst.split(",").map((s) => s.trim());
  const last = parts[0] || "";
  const first = parts.slice(1).join(", ").trim();
  const row = {
    pga_player_id: pid,
    pga_display_name: `${first} ${last}`.trim(),
    pga_first_name: first,
    pga_last_name: last,
    pga_country: "",
    dg_id: hit.dg_id,
    dg_player_name: hit.player_name,
    dg_country: hit.country || "",
    n_rounds_hist: nRows,
    match_method: "supplement_pgatour_og_accentfold",
    confidence: 0.9,
  };
  mapRows.push(row);
  mappedPids.add(pid);
  mappedDgs.add(hit.dg_id);
  added.push(row);
  console.log(`  + ${pid} → ${hit.dg_id} (${hit.player_name}) [${nRows} rows]`);
}

const header = [
  "pga_player_id",
  "pga_display_name",
  "pga_first_name",
  "pga_last_name",
  "pga_country",
  "dg_id",
  "dg_player_name",
  "dg_country",
  "n_rounds_hist",
  "match_method",
  "confidence",
];

function csvEscape(v) {
  const s = String(v ?? "");
  if (/[",\n]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

const out = createWriteStream(MAP, { encoding: "utf8" });
out.write(header.map(csvEscape).join(",") + "\n");
for (const r of mapRows) {
  out.write(header.map((h) => csvEscape(r[h])).join(",") + "\n");
}
out.end();
await finished(out);

console.log(
  `[supplement-map] Added ${added.length} mappings (failed/ambiguous ${failed}). Map now ${mapRows.length} rows → ${MAP}`,
);
