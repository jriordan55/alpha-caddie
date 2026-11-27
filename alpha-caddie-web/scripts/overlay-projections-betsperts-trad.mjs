#!/usr/bin/env node
/**
 * Overlay Betsperts Rabbit Hole traditional stats onto projections*.json as primary
 * driving accuracy / GIR / BoB / bogey rates (DataGolf kept only when unmapped).
 *
 * Reads: data/betsperts_rh_ou_stats.json
 * Writes: projections.json, projections-pga.json, projections-euro.json (in place)
 */
import { existsSync, mkdirSync, readFileSync, renameSync, unlinkSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const ROOT = resolve(WEB, "..");
const PROJ_FILES = ["projections.json", "projections-pga.json", "projections-euro.json"];

function num(v, fallback = NaN) {
  const n = Number(String(v ?? "").replace(/,/g, "").trim());
  return Number.isFinite(n) ? n : fallback;
}

function writeJsonAtomic(filePath, text) {
  mkdirSync(dirname(filePath), { recursive: true });
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

function loadRhOu() {
  for (const p of [
    join(WEB, "data", "betsperts_rh_ou_stats.json"),
    join(ROOT, "data", "betsperts_rh_ou_stats.json"),
  ]) {
    if (!existsSync(p)) continue;
    try {
      return JSON.parse(readFileSync(p, "utf8"));
    } catch {
      /* next */
    }
  }
  return null;
}

function loadWindows() {
  for (const p of [
    join(WEB, "data", "betsperts_betting_viz.json"),
    join(ROOT, "data", "betsperts_betting_viz.json"),
  ]) {
    if (!existsSync(p)) continue;
    try {
      const j = JSON.parse(readFileSync(p, "utf8"));
      return j?.playersByDg && typeof j.playersByDg === "object" ? j.playersByDg : null;
    } catch {
      /* next */
    }
  }
  return null;
}

function pct01(v) {
  const n = num(v, NaN);
  if (!Number.isFinite(n)) return NaN;
  return n > 1.5 ? n / 100 : n;
}

const rh = loadRhOu();
const windowsByDg = loadWindows();
if (!rh?.players && !windowsByDg) {
  console.warn("[overlay-projections-betsperts-trad] no RH OU / betting-viz — skip");
  process.exit(0);
}

let total = 0;
for (const name of PROJ_FILES) {
  const fp = join(WEB, name);
  if (!existsSync(fp)) continue;
  let j;
  try {
    j = JSON.parse(readFileSync(fp, "utf8"));
  } catch (e) {
    console.warn(`[overlay-projections-betsperts-trad] skip ${name}:`, e.message || e);
    continue;
  }
  if (!Array.isArray(j.players)) continue;
  let patched = 0;
  for (const p of j.players) {
    const dg = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    const rhRow = rh?.players?.[String(dg)];
    if (rhRow) {
      const acc = pct01(rhRow.dr_accuracy);
      if (Number.isFinite(acc)) {
        p.driving_accuracy = acc;
        p.driving_acc = acc;
        p.betsperts_dr_accuracy = rhRow.dr_accuracy;
      }
      const gir = pct01(rhRow.gir_pct);
      if (Number.isFinite(gir)) {
        p.gir_pct = gir > 1 ? gir : gir * 100;
        p.betsperts_gir_pct = rhRow.gir_pct;
      }
      if (Number.isFinite(num(rhRow.bob_pct, NaN))) p.betsperts_bob_pct = num(rhRow.bob_pct);
      if (Number.isFinite(num(rhRow.bogey_pct, NaN))) p.betsperts_bogey_pct = num(rhRow.bogey_pct);
      if (Number.isFinite(num(rhRow.sg_total, NaN)) && !Number.isFinite(num(p.sg_total, NaN))) {
        p.sg_total = num(rhRow.sg_total);
      }
      patched++;
    }
    const win = windowsByDg?.[String(dg)]?.windows;
    if (win && typeof win === "object") {
      p.betsperts_form_windows = win;
      const avg = win.average && typeof win.average === "object" ? win.average : win;
      const sgOf = (key) => num(avg?.[key]?.sg_tot ?? avg?.[key]?.sg ?? avg?.[key], NaN);
      const l12 = sgOf("round-12");
      const l24 = sgOf("round-24");
      const l50 = sgOf("round-50");
      const l100 = sgOf("round-100");
      if (Number.isFinite(l12)) p.betsperts_sg_l12 = l12;
      if (Number.isFinite(l24)) p.betsperts_sg_l24 = l24;
      if (Number.isFinite(l50)) p.betsperts_sg_l50 = l50;
      if (Number.isFinite(l100)) p.betsperts_sg_l100 = l100;
      patched++;
    }
  }
  if (!j.meta || typeof j.meta !== "object") j.meta = {};
  j.meta.trad_stats_source = rh?.players ? "betsperts" : j.meta.trad_stats_source || "datagolf";
  j.meta.betsperts_trad_n = patched;
  j.meta.betsperts_trad_at = new Date().toISOString();
  j.meta.prefer_betsperts = true;
  writeJsonAtomic(fp, JSON.stringify(j));
  total += patched;
  console.log(`[overlay-projections-betsperts-trad] ${name}: patched ${patched}/${j.players.length}`);
}

console.log(`[overlay-projections-betsperts-trad] done (rows touched: ${total})`);
